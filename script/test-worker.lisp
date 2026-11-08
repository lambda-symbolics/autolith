(in-package #:cl-user)

(require :sb-posix)
(require :asdf)

(defvar *check-script-library-mode* nil
  "Bind true when loading CLI helpers without running the command.")

(unless (fboundp 'check--load-tests)
  (let ((*check-script-library-mode* t))
    (load (merge-pathnames "check.lisp"
                           (uiop:pathname-directory-pathname *load-truename*)))))

(defparameter *test-worker-probe-argument*
  "--autolith-internal-test-worker-probe"
  "The private probe shared by the test image builder and its installer.")

(defun test-worker-source-paths (source-root)
  "Return every application, test and builder input embedded in a test image."
  (sort
   (remove-duplicates
    (append (uiop:symbol-call '#:autolith '#:active-image-source-paths source-root)
            '("script/check.lisp" "script/test-worker.lisp" "script/roots.lisp")
            (mapcar (lambda (pathname) (enough-namestring pathname source-root))
                    (uiop:symbol-call '#:autolith '#:source-lisp-pathnames
                                      (merge-pathnames "tests/" source-root))))
    :test #'string=)
   #'string<))

(defun test-worker--toplevel (arguments)
  "Run one shard from its source-matched, already loaded test image."
  (handler-case
      (progn
        (unless (= (length arguments) 3)
          (check--fail "The test image needs SOURCE-ROOT REQUEST RESULT."))
        (let ((source-root (uiop:ensure-directory-pathname (pathname (first arguments)))))
          (unless (uiop:symbol-call
                   '#:sbcl-generations '#:image-build-record-compatible-p
                   (uiop:symbol-call '#:sbcl-generations '#:image-running-build-record)
                   source-root :inputs #'test-worker-source-paths)
            (check--fail "The loaded test image does not match its source and runtime."))
          (uiop:quit (test-worker-main (rest arguments) source-root :preloaded-p t))))
    (error (condition)
      (format *error-output* "~&Test image failed: ~A~%" condition)
      (uiop:quit 2))))

(defun test-worker--save-core (source-root pathname)
  "Load the test system in this fresh builder, save its image and exit."
  (check--load-tests source-root)
  (uiop:symbol-call
   '#:sbcl-generations '#:image-save pathname
   (uiop:symbol-call '#:sbcl-generations '#:image-build-record
                     source-root :inputs #'test-worker-source-paths)
   :inputs         #'test-worker-source-paths
   :toplevel       #'test-worker--toplevel
   :prepare        (lambda ()
                     (uiop:symbol-call '#:autolith '#:active-image--prepare-saver))
   :probe-argument *test-worker-probe-argument*))

(defun test-worker--report-live-threads ()
  "Name every Lisp thread still alive after the cases, so leaks are visible."
  (let ((others (remove sb-thread:*current-thread* (sb-thread:list-all-threads))))
    (when others
      (format *error-output* "~&Live threads after cases: ~{~A~^, ~}~%"
              (mapcar #'sb-thread:thread-name others)))))

(defun test-worker--start-stack-watchdog ()
  "Print live Lisp thread stacks after AUTOLITH_TEST_STACK_AFTER seconds."
  (let ((value (uiop:getenv "AUTOLITH_TEST_STACK_AFTER")))
    (when value
      (let ((seconds (handler-case (parse-integer value)
                       (parse-error ()
                         (check--fail "AUTOLITH_TEST_STACK_AFTER must be a positive integer."))))
            (output  *error-output*))
        (unless (plusp seconds)
          (check--fail "AUTOLITH_TEST_STACK_AFTER must be a positive integer."))
        (sb-thread:make-thread
         (lambda ()
           (sleep seconds)
           (dolist (thread (sb-thread:list-all-threads))
             (unless (eq thread sb-thread:*current-thread*)
               (handler-case
                   (sb-thread:interrupt-thread
                    thread
                    (lambda ()
                      (let ((dump
                              (with-output-to-string (stream)
                                (format stream "~&Worker thread ~S after ~D seconds:~%"
                                        (sb-thread:thread-name sb-thread:*current-thread*) seconds)
                                (sb-debug:print-backtrace :count 60 :stream stream))))
                        (write-string dump output)
                        (finish-output output))))
                 (error () nil)))))
         :name "test worker stack watchdog")))))

(defun test-worker-main (arguments source-root &key preloaded-p)
  "Run one assigned shard and atomically publish its portable result."
  (handler-case
      (progn
        (unless (= (length arguments) 2)
          (check--fail "Usage: test-worker.lisp REQUEST RESULT"))
        (let* ((request      (check--read-single-form (first arguments)))
               (result-path  (pathname (second arguments)))
               (staging-path (make-pathname :type "pending" :defaults result-path)))
          (unless (and (check--plist-p request '(:version :cases :temporary-root))
                       (eql (getf request :version) 1)
                       (pathnamep (getf request :temporary-root))
                       (uiop:absolute-pathname-p (getf request :temporary-root))
                       (uiop:directory-exists-p (getf request :temporary-root))
                       (check--proper-list-p (getf request :cases))
                       (getf request :cases)
                       (every #'stringp (getf request :cases))
                       (= (length (getf request :cases))
                          (length (remove-duplicates (getf request :cases)
                                                     :test #'string-equal))))
            (check--fail "Invalid test worker request."))
          (unless preloaded-p
            (check--load-tests source-root))
          (let* ((names (getf request :cases))
                 (cases (if preloaded-p
                            (uiop:symbol-call '#:autolith '#:tests-select :tests names)
                            (check--select-tests nil names))))
            (unless (equal (mapcar #'string-downcase cases) names)
              (check--fail "Worker selection does not match its assigned cases."))
            (test-worker--start-stack-watchdog)
            (let ((result
                    (handler-case
                        (uiop:symbol-call '#:autolith '#:tests-run-cases cases
                                          :temporary-root (getf request :temporary-root))
                      (error (condition)
                        (handler-case (test-worker--report-live-threads)
                          (error () nil))
                        (error condition)))))
              (check--validate-result result names)
              (test-worker--report-live-threads)
              (unwind-protect
                   (progn
                     (check--write-form staging-path result)
                     (uiop:rename-file-overwriting-target staging-path result-path))
                (when (probe-file staging-path)
                  (delete-file staging-path)))
              (if (getf result :failures) 1 0)))))
    (error (condition)
      (format *error-output* "~&Test worker failed: ~A~%" condition)
      2)))

(unless *check-script-library-mode*
  (let ((arguments   (uiop:command-line-arguments))
        (source-root (uiop:pathname-parent-directory-pathname
                      (uiop:pathname-directory-pathname (truename *load-truename*)))))
    (if (and (= (length arguments) 2) (string= (first arguments) "--save-core"))
        (test-worker--save-core source-root (pathname (second arguments)))
        (uiop:quit (test-worker-main arguments source-root)))))
