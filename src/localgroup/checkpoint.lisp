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
  "Deliver a checkpoint transition after queued output to every attached client."
  (let ((terminal         (terminal-ui-terminal (application-ui (localgroup-session-application session))))
        (attachments      nil)
        (history-position 0))
    (localgroup--finish-checkpoint-clients (localgroup-session-application session) reconnect-id)
    (unless (typep terminal 'localgroup-terminal)
      (return-from localgroup--finish-checkpoint-attachments nil))
    (with-lock-held ((image-daemon:daemon-runtime-lock session))
      (setf (image-daemon:daemon-runtime-stopping-p session) t))
    (with-lock-held ((image-daemon:relay-lock terminal))
      (setf attachments
            (remove-duplicates
             (append (when (image-daemon:relay-controller terminal)
                       (list (image-daemon:relay-controller terminal)))
                     (image-daemon:relay-observers terminal))
             :test #'eq)
            history-position (image-daemon:relay-history-position terminal)
            (image-daemon:relay-controller terminal) nil
            (image-daemon:relay-observers terminal) nil))
    (unwind-protect
         (dolist (attachment attachments)
           (image-daemon:attachment-finish
            attachment (list :checkpoint-reconnect :id reconnect-id
                             :history-position     history-position)))
      (dolist (attachment attachments)
        (image-daemon:attachment-close attachment))))
  nil)

(-> application-call-with-localgroup-quiesced (application function) t)
(defun application-call-with-localgroup-quiesced (application function)
  "Call FUNCTION without localgroup threads and reconnect clients to the parent."
  (let ((session (application-localgroup-session application)))
    (unless session
      (return-from application-call-with-localgroup-quiesced (funcall function)))
    (let ((token        (image-daemon:daemon-runtime-token session))
          (created-at   (image-daemon:daemon-runtime-created-at session))
          (reconnect-id (make-identifier))
          (detached-explicitly-p
            (with-lock-held ((image-daemon:daemon-runtime-lock session))
              (localgroup-session-detached-explicitly-p session)))
          (linger-p     (localgroup-session-linger-p session)))
      (unwind-protect
           (progn
             (localgroup--finish-checkpoint-attachments session reconnect-id)
             (localgroup-stop application)
             (funcall function))
        (when (eq (application-localgroup-session application) session)
          (localgroup-stop application))
        (unless (application-localgroup-session application)
          (localgroup-start application :token token :created-at created-at
                                       :detached-explicitly-p   detached-explicitly-p
                                       :checkpoint-reconnect-id reconnect-id
                                       :linger-p                linger-p))))))
