;;;; -- Clean Compile Warnings --

;;; Compile and load every supported Autolith system from scratch into a private
;;; FASL directory and record each warning the compiler or loader would print.
;;; Warnings SBCL already muffles by policy, such as uninteresting
;;; redefinitions, are not recorded, nor are those of dependencies or ASDF's
;;; per-file summaries of them. The parent check reports what this records.
;;;
;;; Usage: compile-warnings.lisp SOURCE-ROOT FASL-DIRECTORY RESULT-FILE

(require :asdf)

(defparameter *compile-warnings-systems*
  '("autolith-agent" "autolith-supervisor" "autolith-protocol"
    "autolith-supervisor/protocol" "autolith-supervisor/protocol/tests"
    "autolith-supervisor/session" "autolith-supervisor/session/tests"
    "autolith-supervisor/model" "autolith-supervisor/model/tests"
    "autolith-supervisor/accounts" "autolith-supervisor/accounts/tests"
    "autolith-supervisor/private-process" "autolith-supervisor/private-process/tests"
    "autolith-supervisor/terminal" "autolith-supervisor/terminal/tests"
    "autolith-supervisor/provider-http"
    "autolith-supervisor/provider-executor" "autolith-supervisor/provider-executor/tests"
    "autolith-protocol-client" "autolith-protocol-client/tests"
    "autolith-protocol-client/records" "autolith-protocol-client/records/tests"
    "autolith" "autolith/checks" "autolith/tests"
    #-win32 "autolith/release-server"
    "autolith/structural" "autolith/structural/tests" "autolith/debug"
    "autolith/debug/tests")
  "Every supported agent, supervisor, and extension system for this host.")

(defun compile-warnings--owned-paths (source-root)
  "Map supported systems' exact source and translated output paths to source names."
  (let ((paths (make-hash-table :test #+win32 'equalp #-win32 'equal)))
    (labels ((record (path relative)
               ;; SBCL 2.6.6 EQUALP tables can compare distinct pathname keys by EQ.
               ;; Namestrings also fold Windows case before the output files exist.
               (setf (gethash (uiop:native-namestring path) paths) relative)
               (let ((existing (probe-file path)))
                 (when existing
                   (setf (gethash (uiop:native-namestring existing) paths) relative))))

             (visit (component)
               (when (typep component 'asdf:cl-source-file)
                 (let* ((source   (asdf:component-pathname component))
                        (relative (uiop:unix-namestring
                                   (uiop:enough-pathname source source-root))))
                   (record source relative)
                   (dolist (output (asdf:output-files 'asdf:compile-op component))
                     (record output relative))))
               (when (typep component 'asdf:parent-component)
                 (mapc #'visit (asdf:component-children component)))))
      (dolist (name *compile-warnings-systems*)
        (let ((system (asdf:find-system name nil)))
          (when system
            (visit system)))))
    paths))

(defun compile-warnings--source-path (owned-paths)
  "Return the owned source name for the current compilation or load, or NIL."
  ;; A dependency compilation nested in a project load belongs to the compiled
  ;; file, not to the enclosing load. Output paths come from ASDF, not suffixes.
  (let ((path (or *compile-file-truename* *load-truename*)))
    (when path
      (or (gethash (uiop:native-namestring path) owned-paths)
          ;; Output files did not exist when the map was built. Resolve them now
          ;; so a case-insensitive volume's stored directory spelling agrees.
          (let ((current (probe-file path)))
            (when current
              (loop for recorded being the hash-keys of owned-paths
                      using (hash-value source)
                    for existing = (probe-file recorded)
                    when (and existing (uiop:pathname-equal current existing))
                      return source)))))))

(defun compile-warnings--line (source-root path)
  "Return the source line of the form the compiler is processing in PATH, or NIL."
  (let ((position (ignore-errors
                   (let ((context (sb-c::find-error-context nil)))
                     (and context (sb-c::compiler-error-context-file-position context))))))
    (and position
         path
         (with-open-file (stream (merge-pathnames path source-root)
                                 :external-format ':utf-8)
           (let ((text (make-string position)))
             (read-sequence text stream)
             (1+ (count #\Newline text)))))))

(defun compile-warnings--interesting-p (condition)
  "Return true when CONDITION would be printed and is not ASDF's summary of others."
  (not (or (typep condition sb-ext:*muffled-warnings*)
           (typep condition 'uiop:compile-warned-warning)
           (typep condition 'uiop:compile-failed-warning))))

(let* ((arguments      (uiop:command-line-arguments))
       (source-root    (uiop:ensure-directory-pathname (first arguments)))
       (fasl-directory (uiop:ensure-directory-pathname (second arguments)))
       (result-file    (pathname (third arguments)))
       (warnings       nil))
  (load (merge-pathnames ".qlot/setup.lisp" source-root))
  ;; Select registered ASDF before loading dependencies so a later upgrade
  ;; cannot reset their load state and redefine installed structures.
  (asdf:load-system "asdf")
  (asdf:initialize-output-translations
   `(:output-translations (,(uiop:native-namestring source-root)
                           ,(uiop:native-namestring fasl-directory))
                          :inherit-configuration))
  (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
  (let ((owned-paths (compile-warnings--owned-paths source-root)))
    (handler-bind ((warning
                     (lambda (condition)
                       (when (compile-warnings--interesting-p condition)
                         (let ((path (compile-warnings--source-path owned-paths)))
                           ;; A warning without a file of ours belongs to a
                           ;; dependency, unless no file is being processed at
                           ;; all, as for undefined functions reported at the end.
                           (unless (and (null path) (or *compile-file-truename* *load-truename*))
                             (push (list :path    path
                                         :line    (compile-warnings--line source-root path)
                                         :type    (string-downcase (type-of condition))
                                         :message (substitute #\Space #\Newline
                                                              (princ-to-string condition)))
                                   warnings)))))))
      (let ((asdf:*compile-file-warnings-behaviour* ':warn)
            (asdf:*compile-file-failure-behaviour*  ':warn))
        (dolist (system *compile-warnings-systems*)
          (when (asdf:find-system system nil)
            (asdf:load-system system))))))
  (with-open-file (stream result-file :direction ':output :if-exists ':supersede
                                      :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (prin1 (reverse warnings) stream))))
  (uiop:quit 0))
