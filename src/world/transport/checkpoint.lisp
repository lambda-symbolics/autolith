(in-package #:autolith)

;;;; -- World-Terminal Checkpoint Quiescence --

(-> world-terminal--finish-checkpoint-clients (application string) null)
(defun world-terminal--finish-checkpoint-clients (application reconnect-id)
  "Drain structured clients through one cursor boundary before a planned restart."
  (let ((hub     (application-client-hub application))
        (clients nil)
        (event   nil))
    (with-lock-held ((client-hub-lock hub))
      (setf (client-hub-admitting-p hub) nil
            clients (copy-list (client-hub-clients hub))
            event (list :checkpoint-reconnect :id reconnect-id
                        :sequence             (client-hub-event-sequence hub)
                        :pending-requests     ':interrupted))
      (dolist (client clients)
        (image-daemon:attachment-seal (channel-client-attachment client) event)))
    (dolist (client clients)
      (channel-client-finish client event)))
  nil)

(-> world-terminal--finish-checkpoint-attachments (channel-endpoint string) null)
(defun world-terminal--finish-checkpoint-attachments (session reconnect-id)
  "Send SESSION's clients to the endpoint that follows the checkpoint named RECONNECT-ID.

The session is stopping from here on, so a client that leaves is not a lost terminal."
  (with-lock-held ((image-daemon:daemon-runtime-lock session))
    (setf (image-daemon:daemon-runtime-stopping-p session) t))
  (world-terminal--finish-checkpoint-clients (channel-endpoint-application session) reconnect-id)
  nil)

(-> application-call-with-world-terminal-quiesced (application function) t)
(defun application-call-with-world-terminal-quiesced (application function)
  "Call FUNCTION without world-terminal threads and reconnect clients to the parent."
  (let ((session (application-channel-endpoint application)))
    (unless session
      (return-from application-call-with-world-terminal-quiesced (funcall function)))
    (let ((token        (image-daemon:daemon-runtime-token session))
          (created-at   (image-daemon:daemon-runtime-created-at session))
          (reconnect-id (make-identifier)))
      (unwind-protect
           (progn
             (world-terminal--finish-checkpoint-attachments session reconnect-id)
             (world-terminal-stop application)
             (funcall function))
        (when (eq (application-channel-endpoint application) session)
          (world-terminal-stop application))
        (unless (application-channel-endpoint application)
          (world-terminal-start application :token token :created-at created-at
                                       :checkpoint-reconnect-id reconnect-id))))))
