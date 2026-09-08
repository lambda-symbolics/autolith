(in-package #:autolith)

;;;; -- Localgroup Checkpoint Quiescence --

(-> application-call-with-localgroup-quiesced (application function) t)

(defun application-call-with-localgroup-quiesced (application function)
  "Call FUNCTION without localgroup threads, restoring the active conversation endpoint."
  (let ((session (application-localgroup-session application)))
    (unless session
      (return-from application-call-with-localgroup-quiesced (funcall function)))
    (let ((token (image-daemon:daemon-runtime-token session))
          (created-at (image-daemon:daemon-runtime-created-at session))
          (detached-explicitly-p
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (localgroup-session-detached-explicitly-p session))))
      (localgroup-stop application)
      (unwind-protect (funcall function)
        (unless (application-localgroup-session application)
          (localgroup-start application :token token :created-at created-at
                            :detached-explicitly-p detached-explicitly-p))))))
