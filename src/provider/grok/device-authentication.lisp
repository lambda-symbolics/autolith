(in-package #:autolith)

;;;; -- Grok Device Authentication --

;;; Autolith implements the RFC 8628 device authorization grant against the
;;; xAI OAuth issuer, matching grok-build reference commit 47348d13.

(defclass grok-device-authentication-client
    (rfc8628-device-authentication-client)
  ()
  (:documentation "The RFC 8628 device authorization client for the xAI issuer."))

(defclass grok-device-authorization (rfc8628-device-authorization)
  ()
  (:documentation "One pending xAI device authorization and its lifetime."))

(defmethod rfc8628-device-authentication-authorization-class
    ((client grok-device-authentication-client))
  "Return Grok's RFC 8628 authorization class."
  (declare (ignore client))
  'grok-device-authorization)

(-> grok-device-authentication-client-create
    (&key
     (:issuer string)
     (:client-id string)
     (:request-function (option function))
     (:poll-function (option function))
     (:sleep-function function)
     (:clock-function function)
     (:browser-function function)
     (:poll-timeout integer))
    grok-device-authentication-client)
(defun grok-device-authentication-client-create
    (&key
       (issuer *grok-oauth-issuer*)
       (client-id *grok-oauth-client-id*)
       request-function
       poll-function
       (sleep-function nil sleep-supplied-p)
       (clock-function nil clock-supplied-p)
       (browser-function nil browser-supplied-p)
       (poll-timeout nil timeout-supplied-p))
  "Create a Grok device client with optional I/O and timing overrides."
  (apply #'make-instance 'grok-device-authentication-client
         :issuer issuer :client-id client-id
         :device-code-path "/oauth2/device/code" :token-path "/oauth2/token"
         :scope                   (format nil "~{~A~^ ~}" *grok-oauth-scopes*)
         :request-code-parameters (list (cons "referrer" "autolith"))
         (append (when request-function (list :request-function request-function))
                 (when poll-function (list :poll-function poll-function))
                 (when sleep-supplied-p (list :sleep-function sleep-function))
                 (when clock-supplied-p (list :clock-function clock-function))
                 (when browser-supplied-p (list :browser-function browser-function))
                 (when timeout-supplied-p (list :poll-timeout poll-timeout)))))

(defmethod device-authentication-display-code
    ((client grok-device-authentication-client)
     (authorization device-authorization)
     (stream stream))
  "Display the Grok verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with Grok:~%  Open: ~A~%  Code: ~A~%~%Continue only if you started this login in Autolith and the browser shows the same code.~%"
          (device-authorization-verification-url authorization)
          (device-authorization-user-code authorization))
  (finish-output stream)
  nil)
