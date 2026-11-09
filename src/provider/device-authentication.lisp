(in-package #:autolith)

;;;; -- RFC 8628 Conditions --

(define-condition device-authentication-error
    (cl-rfc8628:device-authentication-error authentication-error)
  ()
  (:documentation
   "A device authentication failure in Autolith's condition hierarchy."))


;;;; -- ChatGPT Device Authentication State --

(defclass device-authorization-code ()
  ((authorization-code
    :initarg       :authorization-code
    :reader        device-authorization-code-value
    :type          non-empty-string
    :documentation "The short-lived OAuth authorization code.")
   (code-verifier
    :initarg       :code-verifier
    :reader        device-authorization-code-verifier
    :type          non-empty-string
    :documentation "The PKCE verifier returned by the device service."))
  (:documentation "The short-lived result of an approved device authorization."))


;;;; -- ChatGPT Device Authentication Methods --

(defclass openai-device-authentication-client (device-authentication-client)
  ()
  (:documentation
   "Client for OpenAI's proprietary ChatGPT device authorization."))

(defmethod device-authentication-request-code
    ((client openai-device-authentication-client))
  "Request a fresh user code from CLIENT's configured OpenAI issuer."
  (call-with-secret-use
   (lambda ()
     (let* ((document
              (device-authentication-json-request
               :client       client
               :url          (device-authentication-issuer-url
                              client
                              "/api/accounts/deviceauth/usercode")
               :content-type "application/json"
               :content      (json-encode
                              (json-object
                               "client_id"
                               (device-authentication-client-id client)))
               :stage        ':request-code))
            (device-authorization-id
              (json-get document "device_auth_id"))
            (user-code
              (or (json-get document "user_code")
                  (json-get document "usercode")))
            (poll-interval
              (device-authentication-poll-interval
               (json-get document "interval"))))
       (unless (and (non-empty-string-p device-authorization-id)
                    (non-empty-string-p user-code))
         (device-authentication-fail
          :stage   ':request-code
          :message "The device authorization response omitted required fields."))
       (make-instance 'device-authorization
                      :verification-url
                      (device-authentication-issuer-url client "/codex/device")
                      :user-code               user-code
                      :device-authorization-id device-authorization-id
                      :poll-interval           poll-interval)))))

(defmethod device-authentication-complete
    ((client openai-device-authentication-client)
     (authorization device-authorization)
     (manager credential-manager))
  "Poll AUTHORIZATION, exchange the code, and save the credentials."
  (call-with-secret-use
   (lambda ()
     (let* ((authorization-code
              (funcall
               (device-authentication-client-poll-function client)
               client
               authorization))
            (primary-source (credential-manager-primary-source manager)))
       (unless (typep authorization-code 'device-authorization-code)
         (device-authentication-fail
          :stage   ':poll
          :message "The device authorization poll returned an invalid result."))
       (let ((credentials
               (device-authentication--exchange-code
                :client             client
                :authorization-code authorization-code
                :source-path        (credential-source-pathname primary-source))))
         (credential-manager-accept-account
          manager credentials :allow-change t)
         (credential-source-save primary-source credentials))
       t))))


;;;; -- ChatGPT Device Authentication Construction and Presentation --

(-> device-authentication-client-create
    (&key
     (:issuer string)
     (:client-id string)
     (:request-function (option function))
     (:poll-function (option function))
     (:sleep-function function)
     (:clock-function function)
     (:browser-function function)
     (:poll-timeout integer))
    device-authentication-client)
(defun device-authentication-client-create
    (&key
       (issuer *openai-oauth-issuer*)
       (client-id *openai-oauth-client-id*)
       request-function
       poll-function
       (sleep-function #'sleep)
       (clock-function #'device-authentication-monotonic-seconds)
       (browser-function #'device-authentication-open-browser)
       (poll-timeout *device-authentication-timeout*))
  "Create a ChatGPT device client with optional I/O and timing overrides."
  (unless (and (non-empty-string-p issuer)
               (non-empty-string-p client-id)
               (plusp poll-timeout))
    (device-authentication-fail
     :stage   ':configuration
     :message "Device authentication configuration is invalid."))
  (make-instance 'openai-device-authentication-client
                 :issuer           (string-right-trim '(#\/) issuer)
                 :client-id        client-id
                 :request-function
                 (or request-function #'device-authentication-request)
                 :poll-function
                 (or poll-function #'device-authentication--poll-for-code)
                 :sleep-function   sleep-function
                 :clock-function   clock-function
                 :browser-function browser-function
                 :poll-timeout     poll-timeout))

(defmethod device-authentication-display-code
    ((client openai-device-authentication-client)
     (authorization device-authorization)
     (stream stream))
  "Display the ChatGPT verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with ChatGPT:~%  Open: ~A~%  Code: ~A~%~%The code expires in 15 minutes. Continue only if you started this login in Autolith.~%"
          (device-authorization-verification-url authorization)
          (device-authorization-user-code authorization))
  (finish-output stream)
  nil)


;;;; -- Private ChatGPT Device Flow --

(-> device-authentication--poll-for-code
    (device-authentication-client device-authorization)
    device-authorization-code)
(defun device-authentication--poll-for-code (client authorization)
  "Poll CLIENT until AUTHORIZATION succeeds, fails, or reaches its deadline."
  (let* ((clock      (device-authentication-client-clock-function client))
         (started-at (funcall clock))
         (deadline   (+ started-at
                        (device-authentication-client-poll-timeout client)))
         (url        (device-authentication-issuer-url
                      client
                      "/api/accounts/deviceauth/token"))
         (content
           (json-encode
            (json-object
             "device_auth_id" (device-authorization-id authorization)
             "user_code" (device-authorization-user-code authorization)))))
    (loop
      (multiple-value-bind (body status response-headers)
          (device-authentication-invoke-request
           :client  client
           :url     url
           :headers (list (cons "Content-Type" "application/json")
                          (cons "Accept" "application/json")
                          (cons "User-Agent"
                                (provider-user-agent)))
           :content content
           :stage   ':poll)
        (declare (ignore response-headers))
        (cond
          ((device-authentication-success-status-p status)
           (let* ((document
                    (handler-case
                        (json-decode body)
                      (error ()
                        (device-authentication-fail
                         :stage   ':poll
                         :message "The approved device response contained invalid JSON."))))
                  (authorization-code
                    (and (json-object-p document)
                         (json-get document "authorization_code")))
                  (code-verifier
                    (and (json-object-p document)
                         (json-get document "code_verifier"))))
             (unless (and (non-empty-string-p authorization-code)
                          (non-empty-string-p code-verifier))
               (device-authentication-fail
                :stage   ':poll
                :message "The approved device response omitted required fields."))
             (return
               (make-instance 'device-authorization-code
                              :authorization-code authorization-code
                              :code-verifier      code-verifier))))
          ((member status '(403 404))
           (let ((now (funcall clock)))
             (when (>= now deadline)
               (device-authentication-fail
                :stage   ':poll
                :message "Device authentication timed out after 15 minutes."))
             (funcall (device-authentication-client-sleep-function client)
                      (min (device-authorization-poll-interval authorization)
                           (max 0 (- deadline now))))))
          (t
           (let ((code
                   (device-authentication-error-code-of-body
                    body
                    (list
                     (device-authorization-id authorization)
                     (device-authorization-user-code authorization)
                     content))))
             (device-authentication-fail
              :stage   ':poll
              :message (format nil "Device authorization was not completed~@[ (~A)~]."
                               code)
              :status  status
              :code    code))))))))

(-> device-authentication--exchange-code
    (&key
     (:client device-authentication-client)
     (:authorization-code device-authorization-code)
     (:source-path pathname))
    oauth-credentials)
(defun device-authentication--exchange-code
    (&key client authorization-code source-path)
  "Exchange AUTHORIZATION-CODE and record SOURCE-PATH in the credentials."
  (let* ((redirect-url
           (device-authentication-issuer-url client "/deviceauth/callback"))
         (content
           (url-encode-params
            (list
             (cons "grant_type" "authorization_code")
             (cons "code"
                   (device-authorization-code-value authorization-code))
             (cons "redirect_uri" redirect-url)
             (cons "client_id" (device-authentication-client-id client))
             (cons "code_verifier"
                   (device-authorization-code-verifier authorization-code)))))
         (document
           (device-authentication-json-request
            :client       client
            :url          (device-authentication-issuer-url client "/oauth/token")
            :content-type "application/x-www-form-urlencoded"
            :content      content
            :stage        ':exchange
            :secret-values
            (list
             (device-authorization-code-value authorization-code)
             (device-authorization-code-verifier authorization-code)
             content)))
         (id-token      (json-get document "id_token"))
         (access-token  (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (account-id
           (or (and (stringp id-token)
                    (device-authentication--jwt-account-id id-token))
               (and (stringp access-token)
                    (device-authentication--jwt-account-id access-token)))))
    (unless (and (non-empty-string-p id-token)
                 (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (non-empty-string-p account-id))
      (device-authentication-fail
       :stage   ':credentials
       :message "The OAuth exchange omitted required credential fields."))
    (make-instance 'oauth-credentials
                   :access-token  access-token
                   :refresh-token refresh-token
                   :id-token      id-token
                   :account-id    account-id
                   :expires-at    (or (jwt-expiration access-token)
                                      (jwt-expiration id-token))
                   :source-path   source-path)))

(-> device-authentication--jwt-account-id (string) (option string))
(defun device-authentication--jwt-account-id (token)
  "Return the account identifier carried by TOKEN's unverified JWT payload."
  (jwt-account-id token))


;;;; -- cl-rfc8628 Host Wiring --

(setf cl-rfc8628:*user-agent-function* #'provider-user-agent
      cl-rfc8628:*device-authentication-error-class* 'device-authentication-error)
