(in-package #:autolith)

;;;; -- Nous Device Authentication --

;;; Protocol details were verified against NousResearch/hermes-agent reference
;;; commit f293e7206b4ddd66042329442c6afebc19a8808d.

(defclass nous-device-authentication-client
    (rfc8628-device-authentication-client)
  ()
  (:documentation "The RFC 8628 device authorization client for Nous Portal."))

(defclass nous-device-authorization (rfc8628-device-authorization)
  ()
  (:documentation "One pending Nous Portal device authorization."))

(defmethod rfc8628-device-authentication-authorization-class
    ((client nous-device-authentication-client))
  "Return Nous's RFC 8628 authorization class."
  (declare (ignore client))
  'nous-device-authorization)

(defmethod rfc8628-device-authentication-validate-token-response
    ((client nous-device-authentication-client) (document hash-table))
  "Require a Nous access JWT with the inference:invoke scope."
  (declare (ignore client))
  (let ((access-token (json-get document "access_token")))
    (unless (and (non-empty-string-p access-token)
                 (jwt-payload access-token))
      (device-authentication-fail
       :stage   ':credentials
       :message "The Nous device flow did not return an access JWT."))
    (unless (nous-authentication--access-token-scope-p
             access-token
             *nous-oauth-scope*)
      (device-authentication-fail
       :stage ':credentials
       :message
       "The Nous access token lacks the inference:invoke scope.")))
  nil)

(defmethod rfc8628-device-authentication-account-id
    ((client nous-device-authentication-client) (document hash-table))
  "Return the stable subject carried by the Nous access JWT."
  (declare (ignore client))
  (let ((access-token (json-get document "access_token")))
    (and (stringp access-token)
         (jwt-subject access-token))))

(defmethod rfc8628-device-authentication-publish-credentials
    ((client nous-device-authentication-client)
     (manager nous-credential-manager)
     (credentials oauth-credentials))
  "Save Nous CREDENTIALS under the process-shared rotation lock."
  (declare (ignore client))
  (credential-manager-call-with-refresh-lock
   manager
   (lambda ()
     (credential-manager-accept-account manager credentials :allow-change t)
     (credential-source-save (credential-manager-primary-source manager) credentials))))

(-> nous-device-authentication-client-create
    (&key
     (:portal-url string)
     (:client-id string)
     (:request-function (option function))
     (:poll-function (option function))
     (:sleep-function function)
     (:clock-function function)
     (:browser-function function)
     (:poll-timeout integer))
    nous-device-authentication-client)
(defun nous-device-authentication-client-create
    (&key
       (portal-url (nous-portal-url))
       (client-id *nous-oauth-client-id*)
       request-function
       poll-function
       (sleep-function nil sleep-supplied-p)
       (clock-function nil clock-supplied-p)
       (browser-function nil browser-supplied-p)
       (poll-timeout nil timeout-supplied-p))
  "Create a Nous device client with optional I/O and timing overrides."
  (apply #'make-instance 'nous-device-authentication-client
         :issuer portal-url :client-id client-id
         :device-code-path "/api/oauth/device/code" :token-path "/api/oauth/token"
         :scope *nous-oauth-scope*
         (append (when request-function (list :request-function request-function))
                 (when poll-function (list :poll-function poll-function))
                 (when sleep-supplied-p (list :sleep-function sleep-function))
                 (when clock-supplied-p (list :clock-function clock-function))
                 (when browser-supplied-p (list :browser-function browser-function))
                 (when timeout-supplied-p (list :poll-timeout poll-timeout)))))

(defmethod device-authentication-display-code
    ((client nous-device-authentication-client)
     (authorization device-authorization)
     (stream stream))
  "Display the Nous verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with Nous Research:~%  Open: ~A~%  Code: ~A~%~%Continue only if you started this login in Autolith and the browser shows the same code.~%"
          (device-authorization-verification-url authorization)
          (device-authorization-user-code authorization))
  (finish-output stream)
  nil)
