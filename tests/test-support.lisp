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
    (platform-make-temporary-directory
     *platform*
     (or parent (uiop:temporary-directory))
     "autolith-tests-"))))

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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))

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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))

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
                       (platform-setenv name value)
                       (platform-unsetenv name))))))
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

(-> test-run-program-with-environment (list list) (values integer string))
(defun test-run-program-with-environment (command environment)
  "Run COMMAND with ENVIRONMENT entries replacing this process's variables.

ENVIRONMENT holds NAME=VALUE strings. Return the exit status and the combined
output."
  (flet ((variable-name (entry)
           "Return the variable ENTRY assigns."
           (subseq entry 0 (position #\= entry))))
    (let* ((names (mapcar #'variable-name environment))
           (inherited (remove-if (lambda (entry)
                                   (member (variable-name entry) names
                                           :test #'string=))
                                 (sb-ext:posix-environ)))
           (output (make-string-output-stream))
           (process (sb-ext:run-program (first command) (rest command)
                                        :search t
                                        :environment (append environment inherited)
                                        :input nil
                                        :output output
                                        :error ':output
                                        :wait t)))
      (values (sb-ext:process-exit-code process)
              (get-output-stream-string output)))))


;;;; -- Host Fixtures --

;;; Some checks need host facilities Autolith itself never uses, such as
;;; symbolic links, FIFOs, forked children, and POSIX file modes. The
;;; protocol below dispatches on *PLATFORM*; tests/posix-fixtures.lisp and
;;; tests/win32-fixtures.lisp implement it for their hosts. A host without a
;;; fixture says so through TEST-FIXTURE-AVAILABLE-P, and the checks that need
;;; it are recorded as skipped through WITH-TEST-FIXTURE.

(deftype test-fixture-kind ()
  "A host facility the tests may depend on.

:EMPTY-ENVIRONMENT-VALUES distinguishes an empty variable from an absent one,
:WILDCARD-FILE-NAMES allows * ? and [ in file names, and :POSIX-ADAPTER means
the POSIX platform adapter and the SB-POSIX symbols it reads exist."
  '(member :symbolic-links :fifos :device-nodes :file-modes :fork
           :pseudo-terminals :posix-shell :empty-environment-values
           :wildcard-file-names :posix-adapter))

(deftype test-file-permissions ()
  "A permission expectation TEST-FIXTURE-PERMISSIONS-P verifies."
  '(member :private-file :private-directory :read-only))

(defgeneric test-fixture-available-p (platform fixture)
  (:documentation
   "Return true when PLATFORM provides FIXTURE, a TEST-FIXTURE-KIND."))

(-> test-withheld (keyword string) null)
(defun test-withheld (facility description)
  "Record the checks DESCRIPTION names as skipped for want of FACILITY.

FACILITY is a TEST-FIXTURE-KIND or a platform capability."
  (when *tests-running-p*
    (fiveam:skip "~A: this host has no ~(~A~)" description facility))
  nil)

(defmacro with-test-fixture ((fixture description) &body body)
  "Run BODY when this host provides FIXTURE, else record DESCRIPTION as skipped."
  `(if (test-fixture-available-p *platform* ,fixture)
       (progn ,@body)
       (test-withheld ,fixture ,description)))

(defmacro with-platform-capability ((capability description) &body body)
  "Run BODY when *PLATFORM* supports CAPABILITY, else record DESCRIPTION as skipped."
  `(if (platform-supports-p *platform* ,capability)
       (progn ,@body)
       (test-withheld ,capability ,description)))

(defgeneric test-fixture-make-symbolic-link (platform target link)
  (:documentation
   "Create symbolic LINK pointing at TARGET, both native namestrings."))

(defgeneric test-fixture-remove-link (platform link)
  (:documentation
   "Remove symbolic LINK, a native namestring, leaving its target alone."))

(defgeneric test-fixture-make-fifo (platform pathname)
  (:documentation
   "Create a FIFO at PATHNAME that only the current user may open."))

(defgeneric test-fixture-device-node (platform)
  (:documentation
   "Return the pathname of a character device such as /dev/null."))

(defgeneric test-fixture-permissions-p (platform pathname permissions)
  (:documentation
   "Return true when PATHNAME carries PERMISSIONS, a TEST-FILE-PERMISSIONS.

:PRIVATE-FILE and :PRIVATE-DIRECTORY mean POSIX modes #o600 and #o700, or an
owner-only access control list on Windows. :READ-ONLY means POSIX mode #o444,
or the read-only attribute on Windows."))

(defgeneric test-fixture-file-mode (platform pathname)
  (:documentation "Return PATHNAME's POSIX permission bits."))

(defgeneric test-fixture-set-file-mode (platform pathname mode)
  (:documentation "Set PATHNAME's POSIX permission bits to MODE."))

(defgeneric test-fixture-call-with-descriptor-input (platform content function)
  (:documentation
   "Call FUNCTION with a descriptor-backed character input stream holding CONTENT."))

(defgeneric test-fixture-run-forked (platform function)
  (:documentation
   "Run FUNCTION in a forked child and return the child's exit status.

FUNCTION returns the status to exit with; a child whose FUNCTION signals exits
with status 1. Return NIL when the child did not exit normally."))

(defgeneric test-fixture-call-with-forked-holder (platform holder-function
                                                  function)
  (:documentation
   "Call FUNCTION while a forked child that ran HOLDER-FUNCTION stays alive.

FUNCTION receives true when the child reported that HOLDER-FUNCTION completed.
The child then exits without unwinding, so FUNCTION's caller observes kernel
cleanup after a dead holder. Return whether the child was reaped and whether
it exited cleanly."))

(defgeneric test-fixture-call-with-pseudo-terminal (platform function)
  (:documentation
   "Call FUNCTION with the descriptor of a pseudo-terminal that echoes its input.

The terminal's original mode is restored on every exit."))

(defgeneric test-fixture-terminal-input-mode (platform descriptor)
  (:documentation
   "Return DESCRIPTOR's terminal input mode as a comparable integer."))

(defgeneric test-fixture-terminal-echo-p (platform descriptor)
  (:documentation "Return true when DESCRIPTOR's terminal echoes its input."))
