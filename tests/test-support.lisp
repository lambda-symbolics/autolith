(in-package #:autolith)

;;;; -- Shared Test Support --

(defparameter *tests-running-p* nil
  "Whether TEST-ASSERT should record its result in the current FiveAM test.")

(defparameter *test-conversation-tiny-png*
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGP4z8DwHwAFAAH/iZk9HQAAAABJRU5ErkJggg=="
  "A one-pixel PNG used to exercise durable image input.")

(-> test-assert (t string) null)
(defun test-assert (value description)
  "Record a FiveAM assertion, or signal directly outside a registered test."
  (if *tests-running-p*
      (fiveam:is-true value "~A" description)
      (unless value
        (error "Test failed: ~A" description)))
  nil)

(-> test-terminal-row-text (list) string)
(defun test-terminal-row-text (row)
  "Return ROW's concatenated terminal span text."
  (apply #'concatenate 'string (mapcar #'terminal-span-text row)))

(-> test-call-with-function-replacements (list function) t)
(defun test-call-with-function-replacements (replacements function)
  "Call FUNCTION while REPLACEMENTS temporarily replace global functions."
  (let ((originals
          (mapcar
           (lambda (replacement)
             (cons (first replacement)
                   (symbol-function (first replacement))))
           replacements)))
    (unwind-protect
         (progn
           (dolist (replacement replacements)
             (setf (symbol-function (first replacement))
                   (second replacement)))
           (funcall function))
      (dolist (original originals)
        (setf (symbol-function (first original))
              (rest original))))))

(-> test-object-contains-string-p (t string) boolean)
(defun test-object-contains-string-p (root needle)
  "Return true when an ordinary object reachable from ROOT contains NEEDLE."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (value)
               "Search VALUE without invoking application accessors."
               (cond
                 ((stringp value)
                  (not (null (search needle value))))
                 ((or (null value)
                      (numberp value)
                      (characterp value)
                      (symbolp value)
                      (pathnamep value)
                      (functionp value))
                  nil)
                 ((gethash value seen)
                  nil)
                 ((consp value)
                  (setf (gethash value seen) t)
                  (or (visit (first value))
                      (visit (rest value))))
                 ((hash-table-p value)
                  (setf (gethash value seen) t)
                  (loop for key being the hash-keys of value
                          using (hash-value child)
                        thereis
                        (or (visit key) (visit child))))
                 ((vectorp value)
                  (setf (gethash value seen) t)
                  (loop for child across value
                        thereis (visit child)))
                 ((or (typep value 'condition)
                      (typep value 'standard-object))
                  (setf (gethash value seen) t)
                  (handler-case
                      (loop for slot in (class-slots (class-of value))
                            for name = (slot-definition-name slot)
                            thereis
                            (and
                             (slot-boundp value name)
                             (visit (slot-value value name))))
                    (error ()
                      nil)))
                 (t
                  nil))))
      (and (visit root) t))))

(defvar *test-temporary-root* nil
  "The process-global fixture parent owned by the current test run.
Use SYMBOL-GLOBAL-VALUE so configuration fixtures in child threads share it.")

(-> test-make-temporary-root (&optional (or null pathname)) pathname)
(defun test-make-temporary-root (&optional parent)
  "Atomically create a short, private test directory below PARENT or TMPDIR."
  (uiop:ensure-directory-pathname
   (truename
    (sb-posix:mkdtemp
     (namestring
      (merge-pathnames "autolith-tests-XXXXXX"
                       (or parent (uiop:temporary-directory))))))))

(-> test-call-with-temporary-root (function &key (:temporary-root (or null pathname))) t)
(defun test-call-with-temporary-root (function &key temporary-root)
  "Call FUNCTION with an owned fixture root and delete it on every exit.
TEMPORARY-ROOT transfers ownership of an existing directory to this scope.
Otherwise create a fresh root, nested below any enclosing run. Restore the
process-global fixture parent on exit; parallel runs need separate processes."
  (let* ((previous (sb-ext:symbol-global-value '*test-temporary-root*))
         (root (or temporary-root (test-make-temporary-root previous))))
    (unwind-protect
         (progn
           (setf (sb-ext:symbol-global-value '*test-temporary-root*) root)
           (funcall function root))
      (setf (sb-ext:symbol-global-value '*test-temporary-root*) previous)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))

(-> test-configuration () configuration)
(defun test-configuration ()
  "Return an isolated configuration rooted in a fresh temporary directory."
  (let* ((parent (sb-ext:symbol-global-value '*test-temporary-root*))
         (root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "~A~A/" (if parent "" "autolith-tests-")
                         (make-identifier))
                 (or parent (uiop:temporary-directory)))))
         (source-root (asdf:system-source-directory :autolith)))
    (uiop:ensure-all-directories-exist (list root))
    ;; Canonicalize ROOT so tests compare paths on the same terms as the
    ;; truename-resolved working directory, even when the platform temporary
    ;; directory is a symlink (macOS maps /var to /private/var).
    (setf root (uiop:ensure-directory-pathname (truename root)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :config-root (merge-pathnames "config/" root)
                   :data-root (merge-pathnames "data/" root)
                   :state-root (merge-pathnames "state/" root)
                   :cache-root (merge-pathnames "cache/" root)
                   :config-root (merge-pathnames "config/" root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" root)
                   :grok-bootstrap-auth-path
                   (merge-pathnames "missing-grok-auth.json" root)
                   :model *default-model*
                   :context-window (configuration--context-window-for
                                    *default-model*)
                   :reasoning-effort *default-reasoning-effort*
                   :provider-endpoint *codex-responses-endpoint*)))
(-> test-configuration-root (configuration) pathname)
(defun test-configuration-root (configuration)
  "Return the common temporary root containing CONFIGURATION's data directory."
  (uiop:pathname-parent-directory-pathname
   (configuration-data-root configuration)))


;;;; -- Reusable Test Fixtures --

(-> test-call-with-configuration (function) t)
(defun test-call-with-configuration (function)
  "Call FUNCTION with a fresh configuration and its canonical temporary root.
Delete that root on every exit and return all values produced by FUNCTION."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (funcall function configuration root)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))

(defmacro with-test-configuration ((configuration &optional root) &body body)
  "Bind CONFIGURATION and optional ROOT around BODY with automatic cleanup.
ROOT is a canonical temporary directory. BODY's bindings and setup run inside
the cleanup boundary, and all of BODY's values are returned."
  (let ((root-name (or root (gensym "ROOT"))))
    `(test-call-with-configuration
      (lambda (,configuration ,root-name)
        ,@(unless root `((declare (ignore ,root-name))))
        ,@body))))

(-> test-call-with-environment (list function) t)
(defun test-call-with-environment (bindings function)
  "Call FUNCTION with process environment BINDINGS, restoring them on every exit.
Each binding is (NAME VALUE); NIL removes NAME and a string sets it, including
an empty string. Nested calls restore their enclosing values. Environment
changes are process-global, so concurrent tests require separate processes."
  (let ((originals
          (mapcar (lambda (binding)
                    (destructuring-bind (name value) binding
                      (check-type name string)
                      (check-type value (or null string))
                      (list name (uiop:getenv name))))
                  bindings)))
    (labels ((install (bindings)
               "Install BINDINGS in this process's environment."
               (dolist (binding bindings)
                 (destructuring-bind (name value) binding
                   (if value
                       (sb-posix:setenv name value 1)
                       (sb-posix:unsetenv name))))))
      (unwind-protect
           (progn
             (install bindings)
             (funcall function))
        (install originals)))))

(defmacro with-test-environment (bindings &body body)
  "Evaluate each (NAME VALUE) in BINDINGS once, then run BODY in that environment.
NIL removes a variable. Restore prior values, including absence, on every exit
and return all of BODY's values. Use process isolation for parallel execution."
  `(test-call-with-environment
    (list ,@(mapcar (lambda (binding) `(list ,@binding)) bindings))
    (lambda () ,@body)))

(-> test-configuration-for-source-root (pathname) configuration)
(defun test-configuration-for-source-root (source-root)
  "Return an isolated configuration whose tracked source is SOURCE-ROOT."
  (let ((state-root (merge-pathnames ".autolith-test-state/" source-root)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :config-root (merge-pathnames "config/" state-root)
                   :data-root (merge-pathnames "data/" state-root)
                   :state-root (merge-pathnames "state/" state-root)
                   :cache-root (merge-pathnames "cache/" state-root)
                   :config-root (merge-pathnames "config/" state-root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" state-root)
                   :grok-bootstrap-auth-path
                   (merge-pathnames "missing-grok-auth.json" state-root)
                   :model *default-model*
                   :reasoning-effort *default-reasoning-effort*
                   :provider-endpoint *codex-responses-endpoint*)))
