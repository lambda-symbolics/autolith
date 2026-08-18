(in-package #:autolith)

;;;; -- Command Permission Tests --

(-> test-command-permission-persistence () null)
(defun test-command-permission-persistence ()
  "Test exact command approvals persist, remain directory-scoped, and clear."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (other (merge-pathnames "other/" root))
         (state (permissions-load configuration)))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-permissions-path configuration)
                   (merge-pathnames "permissions.sexp"
                                    (configuration-state-root configuration)))
            "persistent command approvals live under the state root")
           (ensure-directories-exist other)
           (test-assert
            (not (permissions-allowed-p state "git status" root))
            "unknown commands are denied by persistent permission lookup")
           (permissions-allow :configuration configuration
                              :state         state
                              :command       "git status"
                              :directory     root)
           (let ((mode
                   (sb-posix:stat-mode
                    (sb-posix:stat
                     (namestring
                      (configuration-permissions-path configuration))))))
             (test-assert (= (logand mode #o777) #o600)
                          "command permissions are private on disk"))
           (let ((loaded (permissions-load configuration)))
             (test-assert (permissions-allowed-p loaded "git status" root)
                          "an exact command approval survives reload")
             (test-assert (not (permissions-allowed-p loaded "git status " root))
                          "command approvals match exact shell text")
             (test-assert (not (permissions-allowed-p loaded "git status" other))
                          "command approvals are scoped to their working directory")
             (permissions-clear configuration loaded)
             (test-assert
              (not (permissions-allowed-p
                    (permissions-load configuration) "git status" root))
              "clearing approvals persists an empty permission state")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-command-permission-corruption () null)
(defun test-command-permission-corruption ()
  "Test malformed permission files warn and fail closed without reader evaluation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-permissions-path configuration)))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create)
             (write-string "#.(error \"must not evaluate\")" stream))
           (let ((warned-p nil)
                 (state nil))
             (handler-bind
                 ((permissions-load-warning
                    (lambda (condition)
                      (declare (ignore condition))
                      (setf warned-p t)
                      (muffle-warning))))
               (setf state (permissions-load configuration)))
             (test-assert warned-p
                          "malformed command permissions emit a warning")
             (test-assert
              (not (permissions-allowed-p state "anything" root))
              "malformed command permissions fail closed")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
    nil)

