(in-package #:autolith)

;;;; -- Channel Deletion --

(defparameter *application-channel-deletion-handoff-seconds* 10
  "Maximum time to wait for the invoking terminal and command to leave a channel.")

(-> application--delete-channel (application string) null)
(defun application--delete-channel (application identifier)
  "Remove IDENTIFIER through the durable removal boundary and present its outcome."
  (let ((removed (channel-remove (application-configuration application)
                                 (list identifier) :tool-name "(delete)")))
    (application-present
     application
     (application-notice-line
      (format nil "deleted channel ~A and ~D nested channel~:P"
              (conversation-identifier-display identifier) (1- removed)))))
  nil)

(-> application--channel-deletion-ready-p (list) boolean)
(defun application--channel-deletion-ready-p (request)
  "Return whether REQUEST's invoking command and terminal attachment have left."
  (let* ((source     (getf request :source))
         (controller (application-input-controller source))
         (terminal   (terminal-ui-terminal (application-ui source))))
    (and (or (null controller)
             (null (getf request :work))
             (with-lock-held ((application-input-controller-lock controller))
               (not (eq (application-input-controller-active-work controller)
                        (getf request :work)))))
         (with-lock-held ((image-daemon:relay-lock terminal))
           (not (eq (image-daemon:relay-controller terminal)
                    (getf request :attachment)))))))

(-> application--finish-channel-deletion (application list) null)
(defun application--finish-channel-deletion (application request)
  "Finish a confirmed REQUEST on the destination channel, outside its former host.

Wait only for the initiating work and attachment. Fresh removal validation refuses
other attached clients, new work, descendants in use, and competing leases."
  (handler-case
      (let ((deadline (+ (get-internal-real-time)
                         (* *application-channel-deletion-handoff-seconds*
                            internal-time-units-per-second))))
        (loop until (application--channel-deletion-ready-p request)
              do (when (>= (get-internal-real-time) deadline)
                   (error 'configuration-error
                          :message "The terminal or command has not left the channel; deletion was not started."))
                 (sleep 0.01))
        (application--delete-channel application (getf request :target)))
    (autolith-error (condition)
      (application-present application
                           (application-failure-line (autolith-error-message condition)))))
  nil)

(-> application--handoff-channel-deletion (application string string) null)
(defun application--handoff-channel-deletion (application identifier destination)
  "Move only APPLICATION's controlling terminal to DESTINATION and queue deletion.

The destination controller removes IDENTIFIER after this command and the invoking
attachment have left. It never waits for or interrupts unrelated work."
  (let* ((server     (application-world-server application))
         (terminal   (terminal-ui-terminal (application-ui application)))
         (controller (application-input-controller application))
         (attachment (with-lock-held ((image-daemon:relay-lock terminal))
                       (image-daemon:relay-controller terminal))))
    (unless (and server controller attachment)
      (error 'configuration-error :message "This channel has no controlling terminal to move."))
    (world-server-open server (list :channel destination :switch-p t))
    (let* ((host    (world-server--find-host server destination))
           (target  (and host (application-input-controller (world-host-application host))))
           (work    (with-lock-held ((application-input-controller-lock controller))
                      (application-input-controller-active-work controller)))
           (request (list :source application :target identifier :work work :attachment attachment)))
      (unless target
        (error 'configuration-error :message "The destination channel is not ready; nothing was deleted."))
      (unless (localgroup-terminal-switch-controller terminal destination :controller attachment)
        (error 'configuration-error :message "The controlling terminal disconnected; nothing was deleted."))
      (unless (application-input-controller-enqueue-work target (list :channel-deletion request))
        (error 'configuration-error :message "The destination channel stopped; nothing was deleted."))))
  nil)

(-> application--standalone-deletion-ready-p (application) boolean)
(defun application--standalone-deletion-ready-p (application)
  "Return whether replacing APPLICATION's runtime would leave no unrelated work.

Permit the invoking local command on its own main thread, but refuse a provider
turn, external work on another thread, pending input, and live asynchronous jobs."
  (let ((controller   (application-input-controller application))
        (orchestrator (application--task-orchestrator application)))
    (and (or (null orchestrator)
             (zerop (task-orchestrator-session-live-count orchestrator)))
         (or (null controller)
             (with-lock-held ((application-input-controller-lock controller))
               (and (every #'deque-empty-p (application-input-controller--queues controller))
                    (null (application-input-controller-follow-up-edit-work controller))
                    (or (not (application-input-controller-active-p controller))
                        (and (eq (current-thread) (application-input-controller-main-thread controller))
                             (member (application-input-controller-active-work-kind controller)
                                     '(:command :lisp :native-lisp :client-work))
                             t))))))))

(-> application--resume-deletion-inbox (application string) application)
(defun application--resume-deletion-inbox (application identifier)
  "Claim and install the roleless inbox before releasing the channel being deleted."
  (multiple-value-bind (conversation lease acquired-p)
      (application--conversation-load-owned
       application (application-configuration application) identifier)
    (let ((completed-p nil))
      (unwind-protect
           (prog1
               (application--install-owned-conversation
                application conversation :conversation-lease lease :deletion-inbox-p t)
             (setf completed-p t))
        (unless completed-p
          (when acquired-p
            (conversation-lease-release lease)))))))

(-> application--delete-current-channel (application string) null)
(defun application--delete-current-channel (application identifier)
  "Move the invoking client outside IDENTIFIER's subtree, then remove it safely."
  (let* ((world       (application-world application))
         (inbox       (world-inbox-channel world))
         (destination (and inbox (channel-identifier inbox)))
         (terminal    (terminal-ui-terminal (application-ui application))))
    (unless destination
      (error 'configuration-error :message "There is no inbox to move to; nothing was deleted."))
    (cond
      ((and (application-hosted-p application) (typep terminal 'localgroup-terminal))
       (application--handoff-channel-deletion application identifier destination))
      ((application-clients-attached-p application)
       (channel--busy identifier "(delete)" :state ':attached))
      ((not (application--standalone-deletion-ready-p application))
       (channel--busy identifier "(delete)" :state ':running))
      (t
       ;; Claim the inbox before retiring the previous runtime and its child role.
       (application--resume-deletion-inbox application destination)
       (application-render-records application)
       (application--delete-channel application identifier))))
  nil)

(define-application-command application--builtin-delete-command
    (:name              "/delete"
     :argument          "[ID]"
     :description       "delete a channel and its nested channels after confirmation"
     :tip               "deletes this channel by default; a unique channel ID prefix selects another."
     :busy-behavior     :inspect
     :terminal-behavior :exclusive
     :callable          t)
    (application &optional identifier)
  (let* ((world   (application-world application))
         (current (conversation-identifier (application-conversation application)))
         (target  (if identifier
                      (application-world-channel-identifier application (princ-to-string identifier))
                      current))
         (channel (world--require-channel world target ':delete-channel)))
    (when (channel-inbox-p channel)
      (error 'configuration-error :message "The inbox cannot be deleted."))
    (when (equal "remove"
                 (application-select-approval
                  application
                  :kind  ':channel-deletion
                  :title "Delete channel?"
                  :entry (format nil "Delete channel ~A and every channel nested in it."
                                 (conversation-identifier-display target))
                  :items (reverse
                          (application--channel-remove-confirm-items
                           (list :name (or (channel-title channel)
                                           (conversation-identifier-display target)))))))
      ;; Nothing, including client movement, precedes explicit confirmation.
      (if (member current (channel--subtree world target) :test #'string=)
          (application--delete-current-channel application target)
          (application--delete-channel application target))))
  ':continue)
