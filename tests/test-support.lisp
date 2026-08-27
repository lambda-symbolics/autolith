(in-package #:autolith)

;;;; -- Minimal Test Harness --

(defvar *test-count* 0
  "The number of assertions attempted by the current test run.")

(defparameter *test-conversation-tiny-png*
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGP4z8DwHwAFAAH/iZk9HQAAAABJRU5ErkJggg=="
  "A one-pixel PNG used to exercise durable image input.")

(-> test-assert (t string) null)
(defun test-assert (value description)
  "Record one assertion and signal an error when VALUE is false."
  (incf *test-count*)
  (unless value
    (error "Test failed: ~A" description))
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

(-> test-configuration (&key (:site-config-root (option pathname))) configuration)
(defun test-configuration (&key site-config-root)
  "Return an isolated configuration with an optional SITE-CONFIG-ROOT."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "autolith-tests-~A/" (make-identifier))
                 (uiop:temporary-directory))))
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
                   :site-config-root site-config-root
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
