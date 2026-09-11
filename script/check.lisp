(in-package #:cl-user)

(require :sb-posix)
(require :asdf)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defvar *check-script-library-mode* nil
  "Bind true when loading CLI helpers without running the command.")

(define-condition check-error (error)
  ((message :initarg :message :reader check-error-message
            :documentation "The actionable CLI or worker failure."))
  (:documentation "A test command or worker protocol failure.")
  (:report (lambda (condition stream)
             (write-string (check-error-message condition) stream))))

(defun check--fail (control &rest arguments)
  "Signal a structured check failure formatted from CONTROL and ARGUMENTS."
  (error 'check-error :message (apply #'format nil control arguments)))

(defun check--processor-count ()
  "Return the available logical CPU count, falling back to one worker.

Prefer the Windows NUMBER_OF_PROCESSORS variable, then affinity-aware nproc,
then the online CPU count from getconf or sysctl."
  (let ((environment-count (ignore-errors
                            (parse-integer
                             (or (uiop:getenv "NUMBER_OF_PROCESSORS") "")))))
    (when (typep environment-count '(integer 1 *))
      (return-from check--processor-count environment-count)))
  (loop for command in '(("nproc") ("getconf" "_NPROCESSORS_ONLN")
                        ("sysctl" "-n" "hw.ncpu"))
        for count = (handler-case
                        (multiple-value-bind (output diagnostics status)
                            (uiop:run-program command :output ':string
                                                      :error-output nil
                                                      :ignore-error-status t)
                          (declare (ignore diagnostics))
                          (and (eql status 0) (parse-integer output)))
                      (error () nil))
        when (typep count '(integer 1 *))
          return count
        finally (return 1)))

(defun check--parse-arguments (arguments)
  "Parse selectors and execution limits, rejecting invalid command arguments."
  (let ((suites nil) (tests nil) (jobs nil) (timeout 600) (list-p nil) (help-p nil))
    (labels ((argument (option)
               (or (pop arguments) (check--fail "~A requires a value." option)))

             (positive-integer (option)
               (let ((value (argument option)))
                 (unless (and (plusp (length value))
                              (every #'digit-char-p value)
                              (plusp (parse-integer value)))
                   (check--fail "~A requires a positive integer, received ~S."
                                option value))
                 (parse-integer value))))
      (loop while arguments
            for option = (pop arguments)
            do (cond
                 ((string= option "--suite")
                  (push (argument option) suites))
                 ((string= option "--test")
                  (push (argument option) tests))
                 ((string= option "--jobs")
                  (setf jobs (positive-integer option)))
                 ((string= option "--timeout")
                  (setf timeout (positive-integer option)))
                 ((string= option "--list")
                  (setf list-p t))
                 ((string= option "--help")
                  (setf help-p t))
                 (t
                  (check--fail "Unknown argument ~S. Use --help." option)))))
    (list :suites (nreverse suites) :tests (nreverse tests)
          :jobs (or jobs (check--processor-count))
          :timeout timeout :list list-p :help help-p)))

(defun check--usage (&optional (stream *standard-output*))
  "Print the supported test command options."
  (write-string "Usage: ./script/check [options]

  --suite NAME   Select a suite (repeatable).
  --test NAME    Select a case (repeatable).
  --list         List selected suites and cases without running them.
  --jobs N       Maximum concurrent test processes (default: available logical CPUs).
  --timeout N    Deadline in seconds per process (default: 600).
  --help         Show this help.

With no selectors, run every case and the recovery checks.
Selectors and --list bypass recovery checks.
" stream))

(defun check--read-single-form (pathname)
  "Read PATHNAME's only form with reader evaluation disabled."
  (let ((*read-eval* nil))
    (with-open-file (stream pathname :external-format ':utf-8)
      (let ((form (read stream t nil))
            (end-marker (gensym "CHECK-END-")))
        (unless (eq (read stream nil end-marker) end-marker)
          (check--fail "Check data at ~A contains trailing forms." pathname))
        form))))

(defun check--write-form (pathname form)
  "Write portable FORM to a fresh PATHNAME."
  (with-open-file (stream pathname :direction ':output :if-exists ':error
                                  :external-format ':utf-8)
    (let ((*print-readably* t) (*print-pretty* nil))
      (write form :stream stream)
      (terpri stream))))

(defun check--proper-list-p (value)
  "Return true for finite proper lists, including NIL."
  (and (listp value)
       (handler-case (integerp (list-length value))
         (type-error ()
           nil))))

(defun check--plist-p (value keys)
  "Return true when VALUE contains each of KEYS exactly once."
  (and (check--proper-list-p value)
       (= (length value) (* 2 (length keys)))
       (let ((actual (loop for tail on value by #'cddr collect (first tail))))
         (and (= (length actual) (length (remove-duplicates actual)))
              (every (lambda (key) (member key keys)) actual)
              t))))

(defun check--validate-result (result names)
  "Validate the result schema and exact assigned case coverage; return RESULT."
  (unless (and (check--plist-p result '(:version :cases :checks :failures :timings))
               (eql (getf result :version) 1)
               (eql (getf result :cases) (length names))
               (typep (getf result :checks) '(integer 0 *))
               (check--proper-list-p (getf result :failures))
               (check--proper-list-p (getf result :timings))
               (= (length (getf result :timings)) (length names))
               (every (lambda (timing)
                        (and (check--proper-list-p timing)
                             (= (length timing) 2)
                             (stringp (first timing))
                             (typep (second timing) '(real 0 *))))
                      (getf result :timings))
               (equal (mapcar #'first (getf result :timings)) names)
               (every (lambda (failure)
                        (and (check--plist-p failure '(:test :detail))
                             (stringp (getf failure :test))
                             (member (getf failure :test) names :test #'string=)
                             (stringp (getf failure :detail))))
                      (getf result :failures))
               (= (length (getf result :failures))
                  (length (remove-duplicates (getf result :failures)
                                             :key (lambda (failure) (getf failure :test))
                                             :test #'string=))))
    (check--fail "Worker result has an invalid schema or case coverage."))
  result)

(defun check--partition-cases (cases jobs)
  "Partition CASES into at most JOBS nonempty deterministic round-robin shards."
  (unless (and cases (typep jobs '(integer 1 *)))
    (check--fail "Test execution needs at least one case and a positive job count."))
  (let ((partitions (make-array (min jobs (length cases)) :initial-element nil)))
    (loop for case in cases for index from 0
          do (push case (aref partitions (mod index (length partitions)))))
    (map 'list #'nreverse partitions)))

(defstruct check-process
  "One isolated subprocess, its captured output, and its terminal status."
  label command output names result-path process deadline status problem
  environment)

(defun check--runtime-command (source-root)
  "Return the command prefix that runs a Lisp script under the checked runtime.

POSIX hosts go through the stable runtime launcher; Windows runs the SBCL
that is running this check, since the launcher is a Bash script."
  #-win32
  (list (namestring (merge-pathnames "bin/autolith-runtime" source-root)) "--script")
  #+win32
  (progn
    source-root
    (list (or (uiop:getenv "AUTOLITH_SBCL")
              (namestring sb-ext:*runtime-pathname*))
          "--noinform" "--script")))

(defun check--kill-process-group (process)
  "Kill PROCESS's process group where the host has process groups."
  #-win32
  (let ((pid (uiop:process-info-pid process)))
    ;; SBCL gives noninteractive children a separate process group on both
    ;; Linux and macOS. UIOP's terminate-process only signals the leader.
    (handler-case (sb-posix:kill (- pid) sb-posix:sigkill)
      (sb-posix:syscall-error (condition)
        (unless (= (sb-posix:syscall-errno condition) sb-posix:esrch)
          (error condition)))))
  #+win32
  process
  nil)

(defun check--stop-process (entry)
  "Kill ENTRY's process group and reap its leader, including on unwind."
  (let ((process (check-process-process entry)))
    (check--kill-process-group process)
    (when (uiop:process-alive-p process)
      (uiop:terminate-process process :urgent t))
    (setf (check-process-status entry) (uiop:wait-process process))))

(defun check--run-processes (entries &key (jobs (check--processor-count)) (timeout 600))
  "Run ENTRIES with bounded concurrency and deadlines; always reap children."
  (unless (and (typep jobs '(integer 1 *)) (typep timeout '(integer 1 *)))
    (check--fail "Jobs and timeout must be positive integers."))
  (let ((pending (copy-list entries)) (running nil))
    (unwind-protect
         (loop while (or pending running)
               do (loop while (and pending (< (length running) jobs))
                        for entry = (pop pending)
                        do (handler-case
                               (progn
                                 (setf (check-process-process entry)
                                       (apply #'uiop:launch-program
                                              (check-process-command entry)
                                              :input nil :output (check-process-output entry)
                                              :error-output ':output
                                              (when (check-process-environment entry)
                                                (list :environment
                                                      (check-process-environment entry))))
                                       (check-process-deadline entry)
                                       (+ (get-internal-real-time)
                                          (* timeout internal-time-units-per-second)))
                                 (push entry running))
                             (error (condition)
                               (setf (check-process-problem entry) (princ-to-string condition)
                                     (check-process-status entry) -1))))
                  (dolist (entry running)
                    (cond
                      ((not (uiop:process-alive-p (check-process-process entry)))
                       (check--stop-process entry))
                      ((>= (get-internal-real-time) (check-process-deadline entry))
                       (setf (check-process-problem entry)
                             (format nil "Timed out after ~D seconds." timeout))
                       (check--stop-process entry))))
                  (setf running (remove-if #'check-process-status running))
                  (when running
                    (sleep 0.05)))
      (dolist (entry entries)
        (when (and (check-process-process entry)
                   (null (check-process-status entry)))
          (check--stop-process entry)))))
  entries)

(defun check--print-process-log (entry)
  "Print ENTRY's failure and captured output without hiding worker diagnostics."
  (format *error-output* "~&~A (exit ~A)~@[ : ~A~]~%~A~%"
          (check-process-label entry) (check-process-status entry)
          (check-process-problem entry)
          (if (probe-file (check-process-output entry))
              (uiop:read-file-string (check-process-output entry))
              "Worker log is missing.")))

(defun check--run-workers (cases &key source-root temporary-root
                                    (jobs (check--processor-count)) (timeout 600))
  "Run CASES in fresh SBCL processes and remove their owned fixture directories.
Parent cleanup follows process-group termination, including crashes and timeouts."
  (let ((entries nil) (results nil) (fixture-roots nil) (successful-p t))
    (unwind-protect
         (progn
           (loop for partition in (check--partition-cases cases jobs) for index from 1
                 for directory = (merge-pathnames (format nil "worker-~D/" index) temporary-root)
                 for request = (merge-pathnames "request.sexp" directory)
                 for result = (merge-pathnames "result.sexp" directory)
                 for names = (mapcar #'string-downcase partition)
                 for fixture-root = (uiop:symbol-call '#:autolith '#:test-make-temporary-root)
                 do (push fixture-root fixture-roots)
                    (ensure-directories-exist request)
                    (check--write-form request (list :version 1 :cases names
                                                    :temporary-root fixture-root))
                    (push (make-check-process
                           :label (format nil "Test worker ~D" index)
                           :names names :result-path result
                           :output (merge-pathnames "output.log" directory)
                           ;; Leave TMPDIR alone: native Unix-domain sockets need short paths.
                           :command (append (check--runtime-command source-root)
                                            (list (namestring (merge-pathnames "script/test-worker.lisp" source-root))
                                                  (namestring request) (namestring result))))
                          entries))
           (setf entries (nreverse entries))
           (format t "~&Running ~D cases in ~D isolated workers.~%" (length cases) (length entries))
           (check--run-processes entries :jobs jobs :timeout timeout)
           (dolist (entry entries)
             (handler-case
                 (let ((result (check--validate-result
                                (check--read-single-form (check-process-result-path entry))
                                (check-process-names entry))))
                   (push result results)
                   (unless (or (check-process-problem entry)
                               (= (check-process-status entry)
                                  (if (getf result :failures) 1 0)))
                     (setf (check-process-problem entry) "Unexpected worker exit status."))
                   (when (getf result :failures)
                     (setf successful-p nil)))
               (error (condition)
                 (unless (check-process-problem entry)
                   (setf (check-process-problem entry) (princ-to-string condition)))))
             (when (or (check-process-problem entry) (not (zerop (check-process-status entry))))
               (setf successful-p nil)
               (check--print-process-log entry)))
           (let* ((timings (loop for result in results append (getf result :timings)))
                  (aggregate (list :version 1
                                   :cases (loop for result in results sum (getf result :cases))
                                   :checks (loop for result in results sum (getf result :checks))
                                   :failures (loop for result in (reverse results)
                                                   append (getf result :failures))
                                   :timings (loop for case in cases
                                                  for timing = (assoc (string-downcase case) timings
                                                                      :test #'string=)
                                                  when timing collect timing))))
             (and (uiop:symbol-call '#:autolith '#:tests-report aggregate) successful-p)))
      (dolist (root fixture-roots)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))))

(defun check--committed-version (source-root)
  "Return the version of HEAD, which pristine recovery uses for source fallback."
  (let ((source (uiop:run-program
                 (list "git" "-C" (namestring source-root)
                       "show" "HEAD:autolith.asd")
                 :output ':string)))
    (with-input-from-string (stream source)
      (let* ((*read-eval* nil)
             (definition (read stream))
             (version (and (consp definition)
                           (eq (first definition) 'asdf:defsystem)
                           (getf (cddr definition) :version))))
        (unless (and (stringp version) (plusp (length version)))
          (check--fail "Committed Autolith source has no readable version."))
        version))))

(defun check--run-recovery (&key source-root temporary-root quicklisp-setup jobs timeout)
  "Run the pristine probe, listing, and fallback checks with bounded processes."
  (let* ((data-root (autolith-application-root :data))
         (core (merge-pathnames "recovery/autolith-recovery.core" data-root))
         (manifest-path (merge-pathnames "recovery/manifest.sexp" data-root))
         (temporary-home (merge-pathnames "home/" temporary-root))
         (command (list (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")
                        "--noinform" "--core" (namestring core)
                        "--end-runtime-options" (namestring source-root))))
    (unless (and (probe-file core) (probe-file manifest-path))
      (check--fail "Autolith's pristine recovery image is missing; run ./script/bootstrap."))
    (let ((manifest (check--read-single-form manifest-path)))
      (unless (and (listp manifest) (eq (first manifest) :recovery-image)
                   (eql (getf (rest manifest) :version) 2)
                   (equal (truename (getf (rest manifest) :core)) (truename core)))
        (check--fail "Autolith's pristine recovery manifest is invalid.")))
    (ensure-directories-exist temporary-home)
    (let ((entries
            (loop for label in '("Recovery probe" "Recovery listing" "Recovery fallback")
                  for filename in '("probe.log" "list.log" "fallback.log")
                  for arguments in
                  (list (append command '("--probe"))
                        (append command '("--list"))
                        (append command '("--" "--version")))
                  for environment in
                  (list nil
                        nil
                        (append (list (format nil "HOME=~A" temporary-home)
                                      (format nil "XDG_DATA_HOME=~Adata/" temporary-root)
                                      (format nil "XDG_STATE_HOME=~Astate/" temporary-root)
                                      (format nil "XDG_CACHE_HOME=~Acache/" temporary-root)
                                      (format nil "AUTOLITH_PROJECT_SETUP=~A" quicklisp-setup))
                                (sb-ext:posix-environ)))
                  collect (make-check-process
                           :label label :command arguments
                           :environment environment
                           :output (merge-pathnames filename temporary-root)))))
      (check--run-processes entries :jobs jobs :timeout timeout)
      (let ((failed-p nil))
        (dolist (entry entries)
          (when (or (check-process-problem entry) (not (zerop (check-process-status entry))))
            (setf failed-p t)
            (check--print-process-log entry)))
        (when failed-p
          (check--fail "Recovery subprocess checks failed.")))
      (let ((probe (check--read-single-form (check-process-output (first entries))))
            (fallback (uiop:read-file-string (check-process-output (third entries)))))
        (unless (and (listp probe) (eq (first probe) :recovery-probe)
                     (eql (getf (rest probe) :version) 2))
          (check--print-process-log (first entries))
          (check--fail "Autolith's pristine recovery probe is invalid."))
        (unless (and (search "No compatible retained generation is available." fallback)
                     (search (format nil "autolith version ~A"
                                     (check--committed-version source-root))
                             fallback))
          (check--print-process-log (third entries))
          (check--fail "Recovery without a retained generation failed."))))
    (format t "~&Recovery checks passed.~%")
    t))

(defun check--load-tests (source-root &key build-sandbox)
  "Load the locked test system and runtime libraries; return the setup pathname."
  (let ((setup (merge-pathnames ".qlot/setup.lisp" source-root)))
    (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
    (autolith-require-minimum-runtime (merge-pathnames "sbcl.version" source-root))
    (unless (probe-file setup)
      (check--fail "Locked dependencies are missing; run ./script/bootstrap."))
    (load setup)
    (when build-sandbox
      (load (merge-pathnames "script/build-sandbox.lisp" source-root)))
    (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
    (let ((directory (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
          (directories (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
      (when (probe-file directory)
        (pushnew directory (symbol-value directories) :test #'equal)))
    (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
    (asdf:load-system :autolith/tests)
    setup))

(defun check--delete-temporary-root (pathname)
  "Delete PATHNAME through Autolith's host-specific directory-tree adapter."
  (let ((platform-symbol (find-symbol "*PLATFORM*" "AUTOLITH"))
        (delete-symbol (find-symbol "PLATFORM-DELETE-DIRECTORY-TREE" "AUTOLITH")))
    (funcall (symbol-function delete-symbol)
             (symbol-value platform-symbol)
             pathname
             :validate t
             :if-does-not-exist ':ignore)))

(defun check-main (arguments source-root)
  "Execute the test CLI and return its exit status."
  (handler-case
      (let ((options (check--parse-arguments arguments)))
        (when (getf options :help)
          (check--usage)
          (return-from check-main 0))
        ;; Compile once before concurrent processes load the same FASL cache.
        (let ((quicklisp-setup
                (check--load-tests source-root :build-sandbox (not (getf options :list)))))
          (let* ((suites (getf options :suites))
                 (tests (getf options :tests))
                 (cases (uiop:symbol-call '#:autolith '#:tests-select :suites suites :tests tests)))
            (unless cases
              (check--fail "No test cases matched the selection."))
            (when (getf options :list)
              (uiop:symbol-call '#:autolith '#:tests-list :suites suites :tests tests)
              (return-from check-main 0))
            (let* ((temporary-root
                     (merge-pathnames
                      (format nil "autolith-check-~D-~D/" (sb-posix:getpid)
                              (random most-positive-fixnum))
                      (uiop:temporary-directory)))
                   (jobs (getf options :jobs))
                   (timeout (getf options :timeout)))
              (unwind-protect
                   (let ((passed-p (check--run-workers
                                    cases :source-root source-root :temporary-root temporary-root
                                    :jobs jobs :timeout timeout)))
                     (unless (or suites tests)
                       (check--run-recovery :source-root source-root :temporary-root temporary-root
                                            :quicklisp-setup quicklisp-setup :jobs jobs :timeout timeout))
                     (if passed-p 0 1))
                 (check--delete-temporary-root temporary-root))))))
    (error (condition)
      (format *error-output* "~&Test command failed: ~A~%" condition)
      1)))

(unless *check-script-library-mode*
  (uiop:quit
   (check-main (uiop:command-line-arguments)
               (uiop:pathname-parent-directory-pathname
                (uiop:pathname-directory-pathname (truename *load-truename*))))))
