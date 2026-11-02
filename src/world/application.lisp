(in-package #:autolith)

;;;; -- World Hooks of the Application --

;;; The application keeps the world in step with the conversations it opens.
;;; The reconciliation hook is release machinery that leaves with
;;; src/world/sessions.lisp.

(-> application-world-sync (application &key (:all-p boolean)) null)
(defun application-world-sync (application &key all-p)
  "Reconcile the active conversation, or every conversation with ALL-P, and select its channel.

Failures are reported as warnings: the world is a projection of the
conversations and never blocks a session."
  (let* ((configuration (application-configuration application))
         (conversation  (and (slot-boundp application 'conversation)
                             (application-conversation application))))
    (handler-case
        (progn
          (if all-p
              (world-reconcile-sessions configuration)
              (when (and conversation (conversation-persisted-p conversation))
                (world-reconcile-sessions configuration
                                          :identities (list (conversation-pathname conversation)))))
          (when conversation
            (world-session-select configuration (conversation-identifier conversation))))
      (error (cause)
        (warn 'world-session-warning
              :pathname (configuration-world-sessions-path configuration)
              :cause    cause))))
  nil)
