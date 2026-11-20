(in-package #:autolith)

;;;; -- Localgroup Checkpoint Quiescence --

(-> localgroup--finish-checkpoint-clients (application string) null)
(defun localgroup--finish-checkpoint-clients (application reconnect-id)
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

(-> localgroup--finish-checkpoint-attachments (localgroup-session string) null)
(defun localgroup--finish-checkpoint-attachments (session reconnect-id)
  "Send SESSION's clients to the endpoint that follows the checkpoint named RECONNECT-ID.

The session is stopping from here on, so a client that leaves is not a lost terminal."
  (with-lock-held ((image-daemon:daemon-runtime-lock session))
    (setf (image-daemon:daemon-runtime-stopping-p session) t))
  (localgroup--finish-checkpoint-clients (localgroup-session-application session) reconnect-id)
  nil)

(-> application-call-with-localgroup-quiesced (application function) t)
(defun application-call-with-localgroup-quiesced (application function)
  "Call FUNCTION without localgroup threads and reconnect clients to the parent."
  (let ((session (application-localgroup-session application)))
    (unless session
      (return-from application-call-with-localgroup-quiesced (funcall function)))
    (let ((token        (image-daemon:daemon-runtime-token session))
          (created-at   (image-daemon:daemon-runtime-created-at session))
          (reconnect-id (make-identifier)))
      (unwind-protect
           (progn
             (localgroup--finish-checkpoint-attachments session reconnect-id)
             (localgroup-stop application)
             (funcall function))
        (when (eq (application-localgroup-session application) session)
          (localgroup-stop application))
        (unless (application-localgroup-session application)
          (localgroup-start application :token token :created-at created-at
                                       :checkpoint-reconnect-id reconnect-id))))))
