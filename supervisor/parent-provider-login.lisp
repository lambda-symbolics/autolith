;;;; -- Shared Provider Login --

(in-package #:autolith-supervisor)

(defparameter *openai-oauth-issuer*
  "https://auth.openai.com"
  "The OpenAI issuer serving ChatGPT browser OAuth.")

(defparameter *openai-oauth-scopes*
  '("openid" "profile" "email" "offline_access" "api.connectors.read" "api.connectors.invoke")
  "The scopes requested by ChatGPT browser OAuth.")

(defparameter *openai-oauth-originator*
  "autolith"
  "The honest client originator sent during ChatGPT browser OAuth.")

(defparameter *chatgpt-oauth-callback-ports*
  '(1455 1457)
  "The localhost callback ports allowed by the ChatGPT OAuth client.")

(defparameter *chatgpt-oauth-callback-timeout*
  900
  "The maximum seconds to wait for the ChatGPT browser callback.")

(defparameter *gemini-oauth-authorization-endpoint*
  "https://accounts.google.com/o/oauth2/v2/auth"
  "The Google OAuth authorization endpoint used for Gemini subscription login.")

(defparameter *gemini-oauth-scopes*
  '("https://www.googleapis.com/auth/cloud-platform"
    "https://www.googleapis.com/auth/userinfo.email"
    "https://www.googleapis.com/auth/userinfo.profile")
  "The Google scopes required by Gemini Code Assist subscription access.")

(defparameter *gemini-oauth-callback-timeout*
  300
  "The maximum seconds to wait for the installed-app loopback callback.")

(defparameter *grok-oauth-scopes*
  '("openid" "profile" "email" "offline_access" "grok-cli:access" "api:access" "conversations:read"
    "conversations:write" "workspaces:read" "workspaces:write")
  "The OAuth scopes requested for Grok subscription access.")

(define-condition device-authentication-error
    (cl-rfc8628:device-authentication-error authentication-error)
    nil
  (:documentation "A device authentication failure joined to Autolith's condition hierarchy."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass device-authorization-code ()
    ((authorization-code :initarg :authorization-code :reader device-authorization-code-value
                         :type          family-account-non-empty-string
                         :documentation "The short-lived OAuth authorization code.")
     (code-verifier :initarg :code-verifier :reader device-authorization-code-verifier
                    :type          family-account-non-empty-string
                    :documentation "The PKCE verifier returned by the device service."))
    (:documentation "The short-lived result of an approved device authorization.")))

(defclass openai-device-authentication-client (cl-rfc8628:device-authentication-client) nil
          (:documentation
           "The proprietary OpenAI device authorization client behind ChatGPT logins."))

(defmethod cl-rfc8628:device-authentication-request-code
           ((client openai-device-authentication-client))
  "Request a fresh user code from CLIENT's configured OpenAI issuer."
  (call-with-secret-use
   (lambda ()
     (let* ((document
             (cl-rfc8628:device-authentication-json-request :client client :url
                                                            (cl-rfc8628:device-authentication-issuer-url
                                                             client
                                                             "/api/accounts/deviceauth/usercode")
                                                            :content-type "application/json"
                                                            :content
                                                            (cl-rfc8628:json-encode
                                                             (cl-rfc8628:json-object "client_id"
                                                                                     (cl-rfc8628:device-authentication-client-id
                                                                                      client)))
                                                            :stage        ':request-code))
            (cl-rfc8628:device-authorization-id (cl-rfc8628:json-get document "device_auth_id"))
            (user-code
             (or (cl-rfc8628:json-get document "user_code")
                 (cl-rfc8628:json-get document "usercode")))
            (poll-interval
             (cl-rfc8628:device-authentication-poll-interval
              (cl-rfc8628:json-get document "interval"))))
       (unless
           (and (family-account-non-empty-string-p cl-rfc8628:device-authorization-id)
                (family-account-non-empty-string-p user-code))
         (cl-rfc8628:device-authentication-fail :stage ':request-code :message
                                                "The device authorization response omitted required fields."))
       (make-instance 'cl-rfc8628:device-authorization :verification-url
                      (cl-rfc8628:device-authentication-issuer-url client "/codex/device")
                      :user-code user-code :device-authorization-id
                      cl-rfc8628:device-authorization-id :poll-interval poll-interval)))))

(defmethod cl-rfc8628:device-authentication-complete
           ((client openai-device-authentication-client)
            (authorization cl-rfc8628:device-authorization) (manager credential-manager))
  "Poll AUTHORIZATION, exchange its code, and securely publish the result."
  (call-with-secret-use
   (lambda ()
     (let* ((authorization-code
             (funcall (cl-rfc8628:device-authentication-client-poll-function client) client
                      authorization))
            (primary-source (cl-rfc8628:credential-manager-primary-source manager)))
       (unless (typep authorization-code 'device-authorization-code)
         (cl-rfc8628:device-authentication-fail :stage ':poll :message
                                                "The device authorization poll returned an invalid result."))
       (let ((credentials
              (device-authentication--exchange-code :client client :authorization-code
               authorization-code :source-path
               (cl-rfc8628:credential-source-pathname primary-source))))
         (cl-rfc8628:credential-manager-accept-account manager credentials :allow-change t)
         (cl-rfc8628:credential-source-save primary-source credentials))
       t))))

(serapeum/types:-> device-authentication-client-create
                   (&key (:issuer string) (:client-id string) (:request-function (option function))
                    (:poll-function (option function)) (:sleep-function function)
                    (:clock-function function) (:browser-function function)
                    (:poll-timeout integer))
                   cl-rfc8628:device-authentication-client)
(defun device-authentication-client-create
       (
        &key (issuer *openai-oauth-issuer*) (client-id *openai-oauth-client-id*) request-function
        poll-function (sleep-function #'sleep)
        (clock-function #'cl-rfc8628:device-authentication-monotonic-seconds)
        (browser-function #'cl-rfc8628:device-authentication-open-browser)
        (poll-timeout cl-rfc8628:*device-authentication-timeout*))
  "Create a ChatGPT device client, optionally replacing every external effect."
  (unless
      (and (family-account-non-empty-string-p issuer) (family-account-non-empty-string-p client-id)
           (plusp poll-timeout))
    (cl-rfc8628:device-authentication-fail :stage ':configuration :message
                                           "Device authentication configuration is invalid."))
  (make-instance 'openai-device-authentication-client :issuer (string-right-trim '(#\/) issuer)
                 :client-id client-id :request-function
                 (or request-function #'cl-rfc8628:device-authentication-request) :poll-function
                 (or poll-function #'device-authentication--poll-for-code) :sleep-function
                 sleep-function :clock-function clock-function :browser-function browser-function
                 :poll-timeout                                poll-timeout))

(defmethod cl-rfc8628:device-authentication-display-code
           ((client openai-device-authentication-client)
            (authorization cl-rfc8628:device-authorization) (stream stream))
  "Display the ChatGPT verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with ChatGPT:~%  Open: ~A~%  Code: ~A~%~%The code expires in 15 minutes. Continue only if you started this login in Autolith.~%"
          (cl-rfc8628:device-authorization-verification-url authorization)
          (cl-rfc8628:device-authorization-user-code authorization))
  (finish-output stream)
  nil)

(serapeum/types:-> device-authentication--poll-for-code
                   (cl-rfc8628:device-authentication-client cl-rfc8628:device-authorization)
                   device-authorization-code)
(defun device-authentication--poll-for-code (client authorization)
  "Poll CLIENT until AUTHORIZATION succeeds, fails, or reaches its deadline."
  (let* ((clock      (cl-rfc8628:device-authentication-client-clock-function client))
         (started-at (funcall clock))
         (deadline   (+ started-at (cl-rfc8628:device-authentication-client-poll-timeout client)))
         (url
          (cl-rfc8628:device-authentication-issuer-url client "/api/accounts/deviceauth/token"))
         (content
          (cl-rfc8628:json-encode
           (cl-rfc8628:json-object "device_auth_id"
                                   (cl-rfc8628:device-authorization-id authorization) "user_code"
                                   (cl-rfc8628:device-authorization-user-code authorization)))))
    (loop
     (multiple-value-bind (body status dexador.error:response-headers)
         (cl-rfc8628:device-authentication-invoke-request :client client :url url :headers
                                                          (list
                                                           (cons "Content-Type" "application/json")
                                                           (cons "Accept" "application/json")
                                                           (cons "User-Agent"
                                                                 (provider-user-agent)))
                                                          :content content :stage ':poll)
       (declare (ignore dexador.error:response-headers))
       (cond
        ((cl-rfc8628:device-authentication-success-status-p status)
         (let* ((document
                 (handler-case (cl-rfc8628:json-decode body)
                               (error nil
                                      (cl-rfc8628:device-authentication-fail :stage ':poll :message
                                                                             "The approved device response contained invalid JSON."))))
                (authorization-code
                 (and (cl-rfc8628:json-object-p document)
                      (cl-rfc8628:json-get document "authorization_code")))
                (code-verifier
                 (and (cl-rfc8628:json-object-p document)
                      (cl-rfc8628:json-get document "code_verifier"))))
           (unless
               (and (family-account-non-empty-string-p authorization-code)
                    (family-account-non-empty-string-p code-verifier))
             (cl-rfc8628:device-authentication-fail :stage ':poll :message
                                                    "The approved device response omitted required fields."))
           (return
            (make-instance 'device-authorization-code :authorization-code authorization-code
                           :code-verifier                                 code-verifier))))
        ((member status '(403 404))
         (let ((now (funcall clock)))
           (when (>= now deadline)
             (cl-rfc8628:device-authentication-fail :stage ':poll :message
                                                    "Device authentication timed out after 15 minutes."))
           (funcall (cl-rfc8628:device-authentication-client-sleep-function client)
                    (min (cl-rfc8628:device-authorization-poll-interval authorization)
                         (max 0 (- deadline now))))))
        (t
         (let ((code
                (cl-rfc8628:device-authentication-error-code-of-body body
                                                                     (list
                                                                      (cl-rfc8628:device-authorization-id
                                                                       authorization)
                                                                      (cl-rfc8628:device-authorization-user-code
                                                                       authorization)
                                                                      content))))
           (cl-rfc8628:device-authentication-fail :stage ':poll :message
                                                  (format nil
                                                          "Device authorization was not completed~@[ (~A)~]."
                                                          code)
                                                  :status status :code code))))))))

(serapeum/types:-> device-authentication--exchange-code
                   (&key (:client cl-rfc8628:device-authentication-client)
                    (:authorization-code device-authorization-code) (:source-path pathname))
                   cl-rfc8628:oauth-credentials)
(defun device-authentication--exchange-code (&key client authorization-code source-path)
  "Exchange AUTHORIZATION-CODE and return credentials attributed to SOURCE-PATH."
  (let* ((redirect-url  (cl-rfc8628:device-authentication-issuer-url client "/deviceauth/callback"))
         (content
          (quri.encode:url-encode-params
           (list (cons "grant_type" "authorization_code")
                 (cons "code" (device-authorization-code-value authorization-code))
                 (cons "redirect_uri" redirect-url)
                 (cons "client_id" (cl-rfc8628:device-authentication-client-id client))
                 (cons "code_verifier" (device-authorization-code-verifier authorization-code)))))
         (document
          (cl-rfc8628:device-authentication-json-request :client client :url
                                                         (cl-rfc8628:device-authentication-issuer-url
                                                          client "/oauth/token")
                                                         :content-type
                                                         "application/x-www-form-urlencoded"
                                                         :content content :stage ':exchange
                                                         :secret-values
                                                         (list
                                                          (device-authorization-code-value
                                                           authorization-code)
                                                          (device-authorization-code-verifier
                                                           authorization-code)
                                                          content)))
         (id-token      (cl-rfc8628:json-get document "id_token"))
         (access-token  (cl-rfc8628:json-get document "access_token"))
         (refresh-token (cl-rfc8628:json-get document "refresh_token"))
         (account-id
          (or (and (stringp id-token) (device-authentication--jwt-account-id id-token))
              (and (stringp access-token) (device-authentication--jwt-account-id access-token)))))
    (unless
        (and (family-account-non-empty-string-p id-token)
             (family-account-non-empty-string-p access-token)
             (family-account-non-empty-string-p refresh-token)
             (family-account-non-empty-string-p account-id))
      (cl-rfc8628:device-authentication-fail :stage ':credentials :message
                                             "The OAuth exchange omitted required credential fields."))
    (make-instance 'cl-rfc8628:oauth-credentials :access-token access-token :refresh-token
                   refresh-token :id-token id-token :account-id account-id :expires-at
                   (or (cl-rfc8628:jwt-expiration access-token)
                       (cl-rfc8628:jwt-expiration id-token))
                   :source-path source-path)))

(serapeum/types:-> device-authentication--jwt-account-id (string) (option string))
(defun device-authentication--jwt-account-id (token)
  "Return the account identifier carried by TOKEN's unverified JWT payload."
  (jwt-account-id token))

(setf cl-rfc8628:*device-authentication-error-class* 'device-authentication-error)

(define-condition chatgpt-oauth-error
    (authentication-error)
    ((stage :initarg :stage :reader chatgpt-oauth-error-stage :type keyword :documentation
      "The browser OAuth stage that failed.")
     (status :initarg :status :initform nil :reader chatgpt-oauth-error-status :type
      (option integer) :documentation "The HTTP status returned by OpenAI, if known.")
     (code :initarg :code :initform nil :reader chatgpt-oauth-error-code :type (option string)
      :documentation "A bounded non-secret OAuth error code, if supplied.")
     (response :initarg :response :initform nil :reader chatgpt-oauth-error-response :type
      (option string) :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in ChatGPT browser OAuth."))

(serapeum/types:-> chatgpt-oauth--fail
                   (&key (:stage keyword) (:message string) (:status (option integer))
                    (:code (option string)) (:response (option string)))
                   nil)
(defun chatgpt-oauth--fail (&key stage message status code response)
  "Signal a structured ChatGPT OAuth failure containing only safe metadata."
  (error 'chatgpt-oauth-error :message message :stage stage :status status :code code :response
         response))

(serapeum/types:-> chatgpt-oauth--state-matches-p (t string) boolean)
(defun chatgpt-oauth--state-matches-p (actual expected)
  "Return true when ACTUAL is EXPECTED or its supported onboarding variant."
  (and (stringp actual)
       (or (string= actual expected)
           (string= actual (format nil "~A.onboarding_entrypoint=life_sciences" expected)))))

(serapeum/types:-> chatgpt-oauth--rfc8252-client
                   (&key (:request-function function) (:issuer string) (:client-id string)
                    (:token-endpoint string) (:originator string))
                   cl-rfc8252:browser-authentication-client)
(defun chatgpt-oauth--rfc8252-client
       (
        &key (request-function #'chatgpt-oauth--request) (issuer *openai-oauth-issuer*)
        (client-id *openai-oauth-client-id*) (token-endpoint *openai-oauth-token-endpoint*)
        (originator *openai-oauth-originator*))
  "Create the shared browser OAuth client with ChatGPT's provider policy."
  (make-instance 'cl-rfc8252:browser-authentication-client :authorization-endpoint
                 (format nil "~A/oauth/authorize" (string-right-trim '(#\/) issuer))
                 :token-endpoint token-endpoint :client-id client-id :scope
                 (format nil "~{~A~^ ~}" *openai-oauth-scopes*) :authorization-parameters
                 (list (cons "id_token_add_organizations" "true")
                       (cons "codex_cli_simplified_flow" "true") (cons "originator" originator))
                 :ports *chatgpt-oauth-callback-ports* :redirect-host "localhost" :callback-path
                 "/auth/callback" :verifier-octets 64 :state-test #'chatgpt-oauth--state-matches-p
                 :timeout *chatgpt-oauth-callback-timeout* :headers
                 (list (cons "originator" originator)) :request-function request-function
                 :credential-function #'chatgpt-oauth--credentials-from-document :error-function
                 #'chatgpt-oauth--fail :error-type 'authentication-error :bounded-string-function
                 #'parent-provider-login--bounded-string :display-function
                 #'chatgpt-oauth--display-login :secret-function #'call-with-secret-use :label
                 "ChatGPT" :success-response
                 "ChatGPT authorization was received. Return to Autolith." :failure-response
                 "ChatGPT authorization failed. Return to Autolith." :mismatch-response
                 "ChatGPT authorization did not match this login."))

(serapeum/types:-> chatgpt-oauth-create-pkce nil (values string string))
(defun chatgpt-oauth-create-pkce ()
  "Return a fresh ChatGPT PKCE verifier and S256 challenge."
  (cl-rfc8252:browser-authentication-create-pkce :verifier-octets 64))

(serapeum/types:-> chatgpt-oauth--state nil string)
(defun chatgpt-oauth--state ()
  "Return a fresh browser OAuth state value."
  (cl-rfc8252:browser-authentication-state))

(serapeum/types:-> chatgpt-oauth-authorization-url
                   (&key (:redirect-uri string) (:state string) (:code-challenge string)
                    (:issuer string) (:client-id string) (:originator string))
                   string)
(defun chatgpt-oauth-authorization-url
       (
        &key redirect-uri state code-challenge (issuer *openai-oauth-issuer*)
        (client-id *openai-oauth-client-id*) (originator *openai-oauth-originator*))
  "Build the ChatGPT authorization URL through cl-rfc8252."
  (cl-rfc8252:browser-authentication-authorization-url
   (chatgpt-oauth--rfc8252-client :issuer issuer :client-id client-id :originator originator)
   :redirect-uri redirect-uri :state state :code-challenge code-challenge))

(serapeum/types:-> chatgpt-oauth-loopback-open nil (values t string))
(defun chatgpt-oauth-loopback-open ()
  "Open the ChatGPT RFC 8252 loopback listener."
  (cl-rfc8252:browser-authentication-loopback-open (chatgpt-oauth--rfc8252-client)))

(serapeum/types:-> chatgpt-oauth-await-loopback (t string &key (:timeout real)) string)
(defun chatgpt-oauth-await-loopback
       (listener expected-state &key (timeout *chatgpt-oauth-callback-timeout*))
  "Await a bounded ChatGPT callback."
  (cl-rfc8252:browser-authentication-await-loopback (chatgpt-oauth--rfc8252-client) listener
                                                    expected-state :timeout timeout))

(serapeum/types:-> chatgpt-oauth--request-wrapper (function) t)
(defun chatgpt-oauth--request-wrapper (thunk)
  "Run an OpenAI token request under the provider response deadline."
  (handler-case (cl-llm-provider-api:provider-call-with-response-deadline 60 thunk)
                (sb-sys:deadline-timeout nil
                 (error 'authentication-error :message
                        "OpenAI OAuth exceeded its response deadline."))))

(serapeum/types:-> chatgpt-oauth--request (&key (:url string) (:content string)) t)
(defun chatgpt-oauth--request (&key url content)
  "POST an OAuth form with Autolith's identity and OpenAI's originator header."
  (cl-rfc8252:browser-authentication-request :url url :content content :headers
                                             (list (cons "User-Agent" (provider-user-agent))
                                                   (cons "originator" *openai-oauth-originator*))
                                             :request-wrapper #'chatgpt-oauth--request-wrapper))

(serapeum/types:-> chatgpt-oauth--token-document
                   (function string &key (:parameters list) (:stage keyword))
                   cl-rfc8628:json-object)
(defun chatgpt-oauth--token-document (request-function endpoint &key parameters stage)
  "Request a token document through the shared redaction and validation policy."
  (cl-rfc8252:browser-authentication-token-document
   (chatgpt-oauth--rfc8252-client :request-function request-function :token-endpoint endpoint)
   :parameters parameters :stage stage))

(serapeum/types:-> chatgpt-oauth--display-login
                   (string &key (:redirect-uri string) (:timeout real)
                           (:stream (or stream boolean)))
                   (option string))
(defun chatgpt-oauth--display-login (url &key redirect-uri timeout stream)
  "Print ChatGPT browser login instructions."
  (format stream
          "~&Sign in with ChatGPT in your browser:~%  ~A~%~%Callback: ~A~%Waiting up to ~A seconds for the local callback.~%"
          url redirect-uri timeout))

(serapeum/types:-> chatgpt-oauth--credentials-from-document
                   (credential-manager cl-rfc8628:json-object) cl-rfc8628:oauth-credentials)
(defun chatgpt-oauth--credentials-from-document (manager document)
  "Validate DOCUMENT and return persisted ChatGPT OAuth credentials."
  (let* ((id-token      (cl-rfc8628:json-get document "id_token"))
         (access-token  (cl-rfc8628:json-get document "access_token"))
         (refresh-token (cl-rfc8628:json-get document "refresh_token"))
         (account-id
          (or (and (family-account-non-empty-string-p id-token) (jwt-account-id id-token))
              (and (family-account-non-empty-string-p access-token)
                   (jwt-account-id access-token)))))
    (unless
        (and (family-account-non-empty-string-p id-token)
             (family-account-non-empty-string-p access-token)
             (family-account-non-empty-string-p refresh-token)
             (family-account-non-empty-string-p account-id))
      (chatgpt-oauth--fail :stage ':token-response :message
       "The ChatGPT OAuth token response omitted required fields."))
    (make-instance 'cl-rfc8628:oauth-credentials :access-token access-token :refresh-token
                   refresh-token :id-token id-token :account-id account-id :expires-at
                   (or (cl-rfc8628:jwt-expiration access-token)
                       (cl-rfc8628:jwt-expiration id-token))
                   :source-path
                   (cl-rfc8628:credential-source-pathname
                    (cl-rfc8628:credential-manager-primary-source manager)))))

(serapeum/types:-> chatgpt-oauth-exchange-code
                   (credential-manager &key (:code string) (:verifier string)
                    (:redirect-uri string) (:request-function function) (:client-id string)
                    (:token-endpoint string))
                   cl-rfc8628:oauth-credentials)
(defun chatgpt-oauth-exchange-code
       (manager
        &key code verifier redirect-uri (request-function #'chatgpt-oauth--request)
        (client-id *openai-oauth-client-id*) (token-endpoint *openai-oauth-token-endpoint*))
  "Exchange a ChatGPT authorization code and validate its credentials."
  (let ((client
         (chatgpt-oauth--rfc8252-client :request-function request-function :client-id client-id
          :token-endpoint token-endpoint)))
    (chatgpt-oauth--credentials-from-document manager
     (cl-rfc8252:browser-authentication-exchange-code client :code code :verifier verifier
                                                      :redirect-uri redirect-uri))))

(serapeum/types:-> chatgpt-oauth-login
                   (credential-manager &key (:stream stream) (:open-browser-p boolean)
                    (:browser-function function) (:callback-function function)
                    (:request-function function) (:timeout real))
                   cl-rfc8628:oauth-credentials)
(defun chatgpt-oauth-login
       (manager
        &key (stream *standard-output*) (open-browser-p t)
        (browser-function #'cl-rfc8628:device-authentication-open-browser)
        (callback-function #'chatgpt-oauth-await-loopback)
        (request-function #'chatgpt-oauth--request) (timeout *chatgpt-oauth-callback-timeout*))
  "Authenticate ChatGPT through the shared RFC 8252 browser flow."
  (cl-rfc8252:browser-authentication-login
   (chatgpt-oauth--rfc8252-client :request-function request-function) manager :stream stream
   :open-browser-p open-browser-p :browser-function browser-function :callback-function
   callback-function :timeout timeout))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass grok-device-authentication-client (cl-rfc8628:rfc8628-device-authentication-client) ()
    (:documentation "The RFC 8628 device authorization client for the xAI issuer."))

  (defclass grok-device-authorization (cl-rfc8628:rfc8628-device-authorization) ()
    (:documentation "One pending xAI device authorization and its lifetime.")))

(defmethod cl-rfc8628:rfc8628-device-authentication-authorization-class
           ((client grok-device-authentication-client))
  "Use Grok's named RFC 8628 authorization state."
  (declare (ignore client))
  'grok-device-authorization)

(serapeum/types:-> grok-device-authentication-client-create
                   (&key (:issuer string) (:client-id string) (:request-function (option function))
                    (:poll-function (option function)) (:sleep-function function)
                    (:clock-function function) (:browser-function function)
                    (:poll-timeout integer))
                   grok-device-authentication-client)
(defun grok-device-authentication-client-create
       (
        &key (issuer *grok-oauth-issuer*) (client-id *grok-oauth-client-id*) request-function
        poll-function (sleep-function #'sleep)
        (clock-function #'cl-rfc8628:device-authentication-monotonic-seconds)
        (browser-function #'cl-rfc8628:device-authentication-open-browser)
        (poll-timeout cl-rfc8628:*device-authentication-timeout*))
  "Create a Grok device client, optionally replacing every external effect."
  (unless
      (and (family-account-non-empty-string-p issuer) (family-account-non-empty-string-p client-id)
           (plusp poll-timeout))
    (cl-rfc8628:device-authentication-fail :stage ':configuration :message
                                           "Grok device authentication configuration is invalid."))
  (make-instance 'grok-device-authentication-client :issuer (string-right-trim '(#\/) issuer)
                 :client-id client-id :device-code-path "/oauth2/device/code" :token-path
                 "/oauth2/token" :scope (format nil "~{~A~^ ~}" *grok-oauth-scopes*)
                 :request-code-parameters (list (cons "referrer" "autolith")) :request-function
                 (or request-function #'cl-rfc8628:device-authentication-request) :poll-function
                 (or poll-function #'cl-rfc8628:rfc8628-device-authentication-poll-for-tokens)
                 :sleep-function sleep-function :clock-function clock-function :browser-function
                 browser-function :poll-timeout poll-timeout))

(defmethod cl-rfc8628:device-authentication-display-code
           ((client grok-device-authentication-client)
            (authorization cl-rfc8628:device-authorization) (stream stream))
  "Display the Grok verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with Grok:~%  Open: ~A~%  Code: ~A~%~%Continue only if you started this login in Autolith and the browser shows the same code.~%"
          (cl-rfc8628:device-authorization-verification-url authorization)
          (cl-rfc8628:device-authorization-user-code authorization))
  (finish-output stream)
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass nous-device-authentication-client (cl-rfc8628:rfc8628-device-authentication-client) ()
    (:documentation "The RFC 8628 device authorization client for Nous Portal."))

  (defclass nous-device-authorization (cl-rfc8628:rfc8628-device-authorization) ()
    (:documentation "One pending Nous Portal device authorization.")))

(defmethod cl-rfc8628:rfc8628-device-authentication-authorization-class
           ((client nous-device-authentication-client))
  "Use Nous's named RFC 8628 authorization state."
  (declare (ignore client))
  'nous-device-authorization)

(defmethod cl-rfc8628:rfc8628-device-authentication-validate-token-response
           ((client nous-device-authentication-client) (document hash-table))
  "Require a Nous access JWT authorized for inference invocation."
  (declare (ignore client))
  (let ((access-token (cl-rfc8628:json-get document "access_token")))
    (unless
        (and (family-account-non-empty-string-p access-token)
             (cl-rfc8628:jwt-payload access-token))
      (cl-rfc8628:device-authentication-fail :stage ':credentials :message
                                             "The Nous device flow did not return an access JWT."))
    (unless (nous-authentication--access-token-scope-p access-token *nous-oauth-scope*)
      (cl-rfc8628:device-authentication-fail :stage ':credentials :message
                                             "The Nous access token lacks the inference:invoke scope.")))
  nil)

(defmethod cl-rfc8628:rfc8628-device-authentication-account-id
           ((client nous-device-authentication-client) (document hash-table))
  "Return the stable subject carried by the Nous access JWT."
  (declare (ignore client))
  (let ((access-token (cl-rfc8628:json-get document "access_token")))
    (and (stringp access-token) (nous-authentication--access-token-account-id access-token))))

(defmethod cl-rfc8628:rfc8628-device-authentication-publish-credentials
           ((client nous-device-authentication-client) (manager nous-credential-manager)
            (credentials cl-rfc8628:oauth-credentials))
  "Publish Nous CREDENTIALS while holding the process-shared rotation lock."
  (declare (ignore client))
  (cl-rfc8628:credential-manager-call-with-refresh-lock manager
                                                        (lambda ()
                                                          (cl-rfc8628:credential-manager-accept-account
                                                           manager credentials :allow-change t)
                                                          (cl-rfc8628:credential-source-save
                                                           (cl-rfc8628:credential-manager-primary-source
                                                            manager)
                                                           credentials))))

(serapeum/types:-> nous-device-authentication-client-create
                   (&key (:portal-url string) (:client-id string)
                    (:request-function (option function)) (:poll-function (option function))
                    (:sleep-function function) (:clock-function function)
                    (:browser-function function) (:poll-timeout integer))
                   nous-device-authentication-client)
(defun nous-device-authentication-client-create
       (
        &key (portal-url (nous-portal-url)) (client-id *nous-oauth-client-id*) request-function
        poll-function (sleep-function #'sleep)
        (clock-function #'cl-rfc8628:device-authentication-monotonic-seconds)
        (browser-function #'cl-rfc8628:device-authentication-open-browser)
        (poll-timeout cl-rfc8628:*device-authentication-timeout*))
  "Create a Nous device client, optionally replacing every external effect."
  (unless
      (and (family-account-non-empty-string-p portal-url)
           (family-account-non-empty-string-p client-id) (plusp poll-timeout))
    (cl-rfc8628:device-authentication-fail :stage ':configuration :message
                                           "Nous device authentication configuration is invalid."))
  (make-instance 'nous-device-authentication-client :issuer (string-right-trim '(#\/) portal-url)
                 :client-id client-id :device-code-path "/api/oauth/device/code" :token-path
                 "/api/oauth/token" :scope *nous-oauth-scope* :request-function
                 (or request-function #'cl-rfc8628:device-authentication-request) :poll-function
                 (or poll-function #'cl-rfc8628:rfc8628-device-authentication-poll-for-tokens)
                 :sleep-function sleep-function :clock-function clock-function :browser-function
                 browser-function :poll-timeout poll-timeout))

(defmethod cl-rfc8628:device-authentication-display-code
           ((client nous-device-authentication-client)
            (authorization cl-rfc8628:device-authorization) (stream stream))
  "Display the Nous verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with Nous Research:~%  Open: ~A~%  Code: ~A~%~%Continue only if you started this login in Autolith and the browser shows the same code.~%"
          (cl-rfc8628:device-authorization-verification-url authorization)
          (cl-rfc8628:device-authorization-user-code authorization))
  (finish-output stream)
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass copilot-device-authentication-client (cl-rfc8628:rfc8628-device-authentication-client) ()
    (:documentation "GitHub's RFC 8628 device login, followed by a Copilot token exchange.")))

(serapeum/types:-> copilot--device-request
                   (&key (:method keyword) (:url string) (:headers list) (:content string))
                   (values string integer t))
(defun copilot--device-request (&key method url headers content)
  "Normalize GitHub's HTTP 200 OAuth errors for the shared RFC 8628 polling loop."
  (multiple-value-bind (body status dexador.error:response-headers)
      (cl-rfc8628:device-authentication-request :method method :url url :headers headers :content
                                                content)
    (values body
            (if (and (= status 200) (cl-rfc8628:oauth-error-code body))
                400
                status)
            dexador.error:response-headers)))

(serapeum/types:-> copilot-device-authentication-client-create nil
                   copilot-device-authentication-client)
(defun copilot-device-authentication-client-create ()
  "Create the GitHub device client using the shared polling, timeout, and browser effects."
  (make-instance 'copilot-device-authentication-client :issuer
                 (format nil "https://~A" (copilot--domain)) :client-id *copilot-oauth-client-id*
                 :device-code-path "/login/device/code" :token-path "/login/oauth/access_token"
                 :scope "read:user" :request-function #'copilot--device-request :poll-function
                 #'cl-rfc8628:rfc8628-device-authentication-poll-for-tokens))

(defmethod cl-rfc8628:device-authentication-complete
           ((client copilot-device-authentication-client)
            (authorization cl-rfc8628:rfc8628-device-authorization)
            (manager copilot-credential-manager))
  "Poll for the GitHub token and publish only after the Copilot exchange succeeds."
  (call-with-secret-use
   (lambda ()
     (let* ((document
             (funcall (cl-rfc8628:device-authentication-client-poll-function client) client
                      authorization))
            (github-token
             (and (cl-rfc8628:json-object-p document)
                  (cl-rfc8628:json-get document "access_token"))))
       (unless (family-account-non-empty-string-p github-token)
         (cl-rfc8628:device-authentication-fail :stage ':credentials :message
                                                "GitHub device login omitted the access token."))
       (let* ((domain (copilot-credential-manager-domain manager))
              (user   (copilot--get-json (format nil "https://api.~A/user" domain) github-token))
              (id     (cl-rfc8628:json-get user "id")))
         (unless (typep id '(integer 1))
           (error 'authentication-error :message "GitHub did not return an account identifier."))
         (let ((credentials
                (copilot--exchange manager github-token (format nil "~A:~D" domain id))))
           (cl-rfc8628:credential-manager-accept-account manager credentials :allow-change t)
           (cl-rfc8628:credential-source-save
            (cl-rfc8628:credential-manager-primary-source manager) credentials)
           t))))))

(define-condition gemini-oauth-error
    (authentication-error)
    ((stage :initarg :stage :reader gemini-oauth-error-stage :type keyword :documentation
      "The OAuth stage that failed.")
     (status :initarg :status :initform nil :reader gemini-oauth-error-status :type
      (option integer) :documentation "The HTTP status returned by Google, if known.")
     (code :initarg :code :initform nil :reader gemini-oauth-error-code :type (option string)
      :documentation "A bounded non-secret OAuth error code, if supplied.")
     (response :initarg :response :initform nil :reader gemini-oauth-error-response :type
      (option string) :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in Google installed-application OAuth for Gemini."))

(serapeum/types:-> gemini-oauth--fail
                   (&key (:stage keyword) (:message string) (:status (option integer))
                    (:code (option string)) (:response (option string)))
                   nil)
(defun gemini-oauth--fail (&key stage message status code response)
  "Signal a structured Gemini OAuth failure containing only safe metadata."
  (error 'gemini-oauth-error :message message :stage stage :status status :code code :response
         response))

(serapeum/types:-> gemini-oauth--request-wrapper (function) t)
(defun gemini-oauth--request-wrapper (thunk)
  "Run a Gemini token request under the provider response deadline."
  (handler-case (cl-llm-provider-api:provider-call-with-response-deadline 60 thunk)
                (sb-sys:deadline-timeout nil
                 (gemini-oauth--fail :stage ':token-request :message
                  "Google OAuth exceeded its response deadline."))))

(serapeum/types:-> gemini-oauth--client
                   (&key (:request-function function) (:client-id string)
                    (:authorization-endpoint string) (:token-endpoint string)
                    (:token-parameters list))
                   cl-rfc8252:browser-authentication-client)
(defun gemini-oauth--client
       (
        &key (request-function #'gemini-oauth--request) (client-id (gemini-oauth-client-id))
        (authorization-endpoint *gemini-oauth-authorization-endpoint*)
        (token-endpoint *gemini-oauth-token-endpoint*)
        (token-parameters
         (let ((secret (gemini-oauth-client-secret)))
           (when secret (list (cons "client_secret" secret))))))
  "Build the shared installed-app client with Gemini's endpoint and host policy."
  (make-instance 'cl-rfc8252:browser-authentication-client :authorization-endpoint
                 authorization-endpoint :token-endpoint token-endpoint :client-id client-id :scope
                 (format nil "~{~A~^ ~}" *gemini-oauth-scopes*) :authorization-parameters
                 '(("access_type" . "offline") ("prompt" . "consent")) :token-parameters
                 token-parameters :ports '(0) :redirect-host "127.0.0.1" :callback-path
                 "/oauth2callback" :timeout *gemini-oauth-callback-timeout* :request-function
                 request-function :credential-function #'gemini-oauth--credentials-from-document
                 :error-function #'gemini-oauth--fail :error-type 'gemini-oauth-error
                 :secret-function #'call-with-secret-use :bounded-string-function
                 #'parent-provider-login--bounded-string :display-function
                 #'gemini-oauth--display-login :label "Gemini" :success-response
                 "Gemini authentication succeeded. You may close this tab." :failure-response
                 "Gemini authentication failed. Return to Autolith."))

(serapeum/types:-> gemini-oauth-create-pkce nil (values string string))
(defun gemini-oauth-create-pkce ()
  "Return a fresh 256-bit PKCE verifier and its S256 challenge."
  (cl-rfc8252:browser-authentication-create-pkce :verifier-octets 32))

(serapeum/types:-> gemini-oauth-authorization-url
                   (&key (:redirect-uri string) (:state string) (:code-challenge string)
                    (:client-id string) (:authorization-endpoint string))
                   string)
(defun gemini-oauth-authorization-url
       (
        &key redirect-uri state code-challenge (client-id (gemini-oauth-client-id))
        (authorization-endpoint *gemini-oauth-authorization-endpoint*))
  "Build the Google installed-app authorization URL for one PKCE flow."
  (cl-rfc8252:browser-authentication-authorization-url
   (gemini-oauth--client :authorization-endpoint authorization-endpoint :client-id client-id
    :token-parameters nil)
   :redirect-uri redirect-uri :state state :code-challenge code-challenge))

(serapeum/types:-> gemini-oauth-loopback-open nil (values t string))
(defun gemini-oauth-loopback-open ()
  "Open an ephemeral IPv4 loopback listener and return it with its redirect URI."
  (cl-rfc8252:browser-authentication-loopback-open (gemini-oauth--client :token-parameters nil)))

(serapeum/types:-> gemini-oauth-loopback-close (t) (values))
(defun gemini-oauth-loopback-close (listener)
  "Close a Gemini OAuth loopback listener."
  (cl-rfc8252:browser-authentication-loopback-close listener))

(serapeum/types:-> gemini-oauth-await-loopback (t string &key (:timeout real)) string)
(defun gemini-oauth-await-loopback
       (listener expected-state &key (timeout *gemini-oauth-callback-timeout*))
  "Wait at most TIMEOUT seconds for a valid Gemini loopback callback."
  (cl-rfc8252:browser-authentication-await-loopback (gemini-oauth--client :token-parameters nil)
                                                    listener expected-state :timeout timeout))

(serapeum/types:-> gemini-oauth--request (&key (:url string) (:content string)) t)
(defun gemini-oauth--request (&key url content)
  "POST one form-encoded request to Google's OAuth token endpoint."
  (cl-rfc8252:browser-authentication-request :url url :content content :headers
                                             (list (cons "User-Agent" (provider-user-agent)))
                                             :request-wrapper #'gemini-oauth--request-wrapper))

(serapeum/types:-> gemini-oauth--token-document
                   (function string &key (:parameters list) (:stage keyword))
                   cl-rfc8628:json-object)
(defun gemini-oauth--token-document (request-function endpoint &key parameters stage)
  "POST PARAMETERS and validate the JSON token response for STAGE."
  (cl-rfc8252:browser-authentication-token-document
   (gemini-oauth--client :request-function request-function :token-parameters nil) :endpoint
   endpoint :parameters parameters :stage stage))

(serapeum/types:-> gemini-oauth--credentials-from-document
                   (credential-manager cl-rfc8628:json-object) cl-rfc8628:oauth-credentials)
(defun gemini-oauth--credentials-from-document (manager document)
  "Validate login token DOCUMENT and return persisted Gemini credentials."
  (let* ((access-token  (cl-rfc8628:json-get document "access_token"))
         (refresh-token (cl-rfc8628:json-get document "refresh_token"))
         (id-token      (cl-rfc8628:json-get document "id_token"))
         (expires-in    (cl-rfc8628:json-get document "expires_in"))
         (account-id
          (or (and (family-account-non-empty-string-p id-token) (cl-rfc8628:jwt-subject id-token))
              "google-main-account")))
    (unless
        (and (family-account-non-empty-string-p access-token)
             (family-account-non-empty-string-p refresh-token)
             (or (null id-token) (family-account-non-empty-string-p id-token))
             (integerp expires-in) (plusp expires-in))
      (gemini-oauth--fail :stage ':token-response :message
       "The Gemini OAuth token response omitted required fields."))
    (make-instance 'cl-rfc8628:oauth-credentials :access-token access-token :refresh-token
                   refresh-token :id-token id-token :account-id account-id :expires-at
                   (+ (get-universal-time) expires-in) :source-path
                   (cl-rfc8628:credential-source-pathname
                    (cl-rfc8628:credential-manager-primary-source manager)))))

(serapeum/types:-> gemini-oauth-exchange-code
                   (credential-manager &key (:code string) (:verifier string)
                    (:redirect-uri string) (:request-function function) (:client-id string)
                    (:client-secret (option string)) (:token-endpoint string))
                   cl-rfc8628:oauth-credentials)
(defun gemini-oauth-exchange-code
       (manager
        &key code verifier redirect-uri (request-function #'gemini-oauth--request)
        (client-id (gemini-oauth-client-id)) (client-secret (gemini-oauth-client-secret))
        (token-endpoint *gemini-oauth-token-endpoint*))
  "Exchange one authorization CODE using VERIFIER and persist no state."
  (let ((client
         (gemini-oauth--client :request-function request-function :client-id client-id
          :token-endpoint token-endpoint :token-parameters
          (when client-secret (list (cons "client_secret" client-secret))))))
    (gemini-oauth--credentials-from-document manager
     (cl-rfc8252:browser-authentication-exchange-code client :code code :verifier verifier
                                                      :redirect-uri redirect-uri))))

(serapeum/types:-> gemini-oauth--display-login
                   (string &key (:redirect-uri string) (:timeout real)
                           (:stream (or stream boolean)))
                   (option string))
(defun gemini-oauth--display-login (url &key redirect-uri timeout stream)
  "Print Gemini browser login instructions."
  (declare (ignore redirect-uri))
  (format stream
          "~&Sign in with Gemini in your browser:~%  ~A~%~%Waiting up to ~A seconds for the local callback.~%"
          url timeout))

(serapeum/types:-> gemini-oauth-login
                   (credential-manager &key (:stream stream) (:open-browser-p boolean)
                    (:browser-function function) (:callback-function function)
                    (:request-function function) (:timeout real))
                   cl-rfc8628:oauth-credentials)
(defun gemini-oauth-login
       (manager
        &key (stream *standard-output*) (open-browser-p t)
        (browser-function #'cl-rfc8628:device-authentication-open-browser)
        (callback-function #'gemini-oauth-await-loopback)
        (request-function #'gemini-oauth--request) (timeout *gemini-oauth-callback-timeout*))
  "Authenticate Gemini through the shared RFC 8252 browser flow."
  (call-with-secret-use
   (lambda ()
     (let* ((secret (rest (assoc "client_secret"
                                 (cl-rfc8628:credential-manager-refresh-parameters manager "")
                                 :test #'equal)))
            (*provider-account-active-configuration*
              (make-provider-account-configuration
               (append (list :state-root
                             (uiop:pathname-directory-pathname
                              (cl-rfc8628:credential-source-pathname
                               (cl-rfc8628:credential-manager-primary-source manager)))
                             :gemini-client-id (cl-rfc8628:credential-manager-client-id manager))
                       (when secret (list :gemini-client-secret secret))))))
       (cl-rfc8252:browser-authentication-login
        (gemini-oauth--client :request-function request-function) manager :stream stream
        :open-browser-p open-browser-p :browser-function browser-function :callback-function
        callback-function :timeout timeout)))))


;;;; -- Trusted Parent Login Dispatch --

(define-condition parent-provider-login-error (authentication-error)
  ((reason :initarg :reason :reader parent-provider-login-reason
           :documentation "The fixed public login failure category."))
  (:documentation "A parent login failed without exposing private credentials."))

(define-condition parent-provider-login-cancelled (parent-provider-login-error)
  ()
  (:default-initargs :reason ':cancelled :message "Provider login was cancelled.")
  (:documentation "The caller cancelled a provider login before completion."))

(defparameter *parent-provider-login-scope* nil
  "The dynamic cancellation scope of one parent login.")

(serapeum/types:-> parent-provider-login--bounded-string (t &key (:limit integer)) string)
(defun parent-provider-login--bounded-string (value &key (limit 8000))
  "Bound redacted OAuth metadata before constructing a public condition."
  (let ((text (princ-to-string value)))
    (subseq text 0 (min limit (length text)))))

(serapeum/types:-> parent-provider-login--family (t) keyword)
(defun parent-provider-login--family (provider)
  "Resolve a built-in provider name without interning caller text."
  (let ((name (and (or (stringp provider) (symbolp provider))
                   (string-downcase (string provider)))))
    (or (loop for (family . names) in
                '((:codex "codex" "chatgpt" "openai" "chatgpt-codex")
                  (:grok "grok" "xai") (:nous "nous" "nous-research")
                  (:gemini "gemini" "google") (:copilot "copilot" "github-copilot")
                  (:openai-api "openai-api") (:anthropic "anthropic")
                  (:mistral "mistral") (:fireworks "fireworks")
                  (:openrouter "openrouter") (:opencode "opencode")
                  (:openai-compatible "openai-compatible"))
              when (member name names :test #'equal) return family)
        (error 'parent-provider-login-error :reason ':unknown-provider
               :message                             "The provider has no built-in login."))))

(serapeum/types:-> parent-provider-login-methods (t) list)
(defun parent-provider-login-methods (provider)
  "Return supported login methods in provider-specific default order."
  (case (parent-provider-login--family provider)
    (:codex
     '(:browser :device))
    ((:grok :nous :copilot)
     '(:device))
    (:gemini
     '(:browser))
    (otherwise
     '(:api-key))))

(serapeum/types:-> parent-provider-login--method (t list) keyword)
(defun parent-provider-login--method (method methods)
  "Normalize a requested method and reject unsupported combinations."
  (let* ((name     (and method (or (stringp method) (symbolp method))
                        (string-downcase (string method))))
         (resolved (cond
                     ((null method)
                      (first methods))
                     ((member name '("device" "device-code") :test #'equal)
                      ':device)
                     ((member name '("browser" "oauth" "installed-oauth") :test #'equal)
                      ':browser)
                     ((member name '("api-key" "key") :test #'equal)
                      ':api-key))))
    (unless (member resolved methods)
      (error 'parent-provider-login-error :reason ':unsupported-method
             :message                             "The provider does not support that login method."))
    resolved))

(serapeum/types:-> parent-provider-login--configuration (t) provider-account-configuration)
(defun parent-provider-login--configuration (configuration)
  "Use existing private account settings or derive them from parent configuration."
  (if (typep configuration 'provider-account-configuration)
      configuration
      (make-provider-account-configuration
       (funcall (symbol-function 'parent-configuration-get) configuration ':account-configuration))))

(serapeum/types:-> parent-provider-login--call (function (or null function)) t)
(defun parent-provider-login--call (function cancelled)
  "Run FUNCTION with prompt cancellation and unwind all OAuth resources.
CANCELLED is a thread-safe, zero-argument predicate. A late interrupt cannot
leave this dynamic scope or affect a later login."
  (if (null cancelled)
      (funcall function)
      (let* ((scope                         (gensym "LOGIN"))
             (*parent-provider-login-scope* scope)
             (owner                         (bordeaux-threads:current-thread))
             (active                        t)
             (marker                        (list ':cancelled))
             (watcher                       nil)
             (result
               (catch scope
                 (when (funcall cancelled)
                   (error 'parent-provider-login-cancelled))
                 (unwind-protect
                      (progn
                        (setf watcher
                              (bordeaux-threads:make-thread
                               (lambda ()
                                 (loop while active
                                       do (when (funcall cancelled)
                                            (bordeaux-threads:interrupt-thread
                                             owner
                                             (lambda ()
                                               (when (and active
                                                          (eq *parent-provider-login-scope* scope))
                                                 (setf active nil)
                                                 (throw scope marker))))
                                            (return))
                                          (sleep 0.05)))
                               :name "parent provider login cancellation"))
                        (funcall function))
                   (setf active nil)
                   (when watcher
                     (bordeaux-threads:join-thread watcher))))))
        (if (eq result marker)
            (error 'parent-provider-login-cancelled)
            result))))

(serapeum/types:-> parent-provider-login--device
                   (provider-account-configuration keyword cl-rfc8628:credential-manager
                    &key (:output stream) (:open-browser-p boolean)) t)
(defun parent-provider-login--device (configuration family manager &key output open-browser-p)
  "Complete the provider's device protocol and publish with its existing manager."
  (let ((client
          (case family
            (:codex
             (device-authentication-client-create))
            (:grok
             (grok-device-authentication-client-create
              :issuer    (or (provider-account-config ':grok-issuer configuration) *grok-oauth-issuer*)
              :client-id (or (provider-account-config ':grok-client-id configuration)
                             *grok-oauth-client-id*)))
            (:nous
             (nous-device-authentication-client-create))
            (:copilot
             (copilot-device-authentication-client-create)))))
    (cl-rfc8628:device-authentication-login client manager
                                           :stream output :open-browser-p open-browser-p)))

(serapeum/types:-> parent-provider-login--browser
                   (keyword cl-rfc8628:credential-manager
                    &key (:output stream) (:open-browser-p boolean)) t)
(defun parent-provider-login--browser (family manager &key output open-browser-p)
  "Run the shared PKCE loopback flow for one provider."
  (ecase family
    (:codex
     (chatgpt-oauth-login manager :stream output :open-browser-p open-browser-p))
    (:gemini
     (gemini-oauth-login manager :stream output :open-browser-p open-browser-p))))

(serapeum/types:-> parent-provider-login--read-key
                   (stream stream (or null function)) string)
(defun parent-provider-login--read-key (input output read-secret)
  "Read a secret through the caller's modal reader or from a non-terminal stream."
  (let ((value
          (if read-secret
              (funcall read-secret "API key: ")
              (progn
                (when (interactive-stream-p input)
                  (error 'parent-provider-login-error :reason ':secret-reader-required
                         :message                             "Interactive API-key login requires a hidden input reader."))
                (write-string "API key: " output)
                (finish-output output)
                (read-line input nil nil)))))
    (unless value
      (error 'parent-provider-login-cancelled))
    (let ((key (string-trim '(#\Space #\Tab #\Return #\Newline) value)))
      (unless (family-account-non-empty-string-p key)
        (error 'parent-provider-login-error :reason ':empty-api-key
               :message                             "No API key was entered."))
      key)))

(serapeum/types:-> parent-provider-login--validate-key (keyword string) null)
(defun parent-provider-login--validate-key (family key)
  "Probe authenticated endpoints before publishing a static key."
  (when (member family '(:anthropic :mistral :openrouter :fireworks))
    (handler-case
        (cl-llm-provider-api:provider-call-with-response-deadline
         60
         (lambda ()
           (let ((headers (list (cons "Authorization" (concatenate 'string "Bearer " key))
                                (cons "User-Agent" (provider-user-agent)))))
             (if (eq family ':fireworks)
                 (dexador:post
                  (or (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT")
                      "https://api.fireworks.ai/inference/v1/responses")
                  :headers (append headers '(("Content-Type" . "application/json")
                                             ("Accept" . "application/json")))
                  :content "{\"model\":\"accounts/fireworks/models/kimi-k3\",\"input\":\"Reply with the single word: ok\",\"store\":false,\"stream\":false}"
                  :force-string t :keep-alive nil :connect-timeout 30 :read-timeout 60)
                 (dexador:get
                  (ecase family
                    (:anthropic
                     "https://api.anthropic.com/v1/models")
                    (:mistral
                     (or (uiop:getenv "AUTOLITH_MISTRAL_MODELS_ENDPOINT")
                         "https://api.mistral.ai/v1/models"))
                    (:openrouter
                     (or (uiop:getenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT")
                         "https://openrouter.ai/api/v1/models")))
                  :headers (if (eq family ':anthropic)
                               (list (cons "x-api-key" key)
                                     (cons "anthropic-version" "2023-06-01")
                                     (cons "User-Agent" (provider-user-agent)))
                               headers)
                  :force-string t :keep-alive nil :connect-timeout 30 :read-timeout 60)))))
      (error ()
        (error 'parent-provider-login-error :reason ':key-validation-failed
               :message                             "API-key validation failed; no credentials were saved."))))
  nil)

(serapeum/types:-> parent-provider-login--save-key
                   (cl-rfc8628:credential-manager string keyword) t)
(defun parent-provider-login--save-key (manager key family)
  "Persist a static key through the existing private source protocol."
  (let* ((source (cl-rfc8628:credential-manager-primary-source manager))
         (credentials
           (make-instance 'cl-rfc8628:oauth-credentials :access-token key
                          :refresh-token nil :id-token nil :expires-at nil
                          :account-id                                 (if (typep source 'api-key-credential-source)
                                                                          (api-key-credential-source-provider-name source)
                                                                          (string-downcase family))
                          :source-path                                (cl-rfc8628:credential-source-pathname source))))
    (cl-rfc8628:credential-manager-accept-account manager credentials :allow-change t)
    (cl-rfc8628:credential-source-save source credentials)))

(serapeum/types:-> parent-provider-login
                   (t t &key (:method t) (:input stream) (:output stream)
                    (:open-browser-p boolean) (:cancelled (or null function))
                    (:read-secret (or null function))) list)
(defun parent-provider-login
    (configuration provider &key method (input *standard-input*) (output *standard-output*)
                                (open-browser-p t) cancelled read-secret)
  "Authenticate a built-in provider in the trusted parent and return public data.
METHOD defaults to the provider's first supported method. CANCELLED is a
thread-safe predicate. READ-SECRET takes a prompt and returns a string or NIL.
The caller holds terminal ownership across this whole operation. OAuth managers
retain their existing refresh, rotation, account validation, and private stores."
  (let* ((family                                  (parent-provider-login--family provider))
         (selected                                (parent-provider-login--method method (parent-provider-login-methods provider)))
         (settings                                (parent-provider-login--configuration configuration))
         (*provider-account-active-configuration* settings)
         (manager                                 (make-provider-account-credential-manager settings family)))
    (parent-provider-login--call
     (lambda ()
       (call-with-secret-use
        (lambda ()
          (ecase selected
            (:device
             (parent-provider-login--device settings family manager
                                            :output output :open-browser-p open-browser-p))
            (:browser
             (parent-provider-login--browser family manager
                                             :output output :open-browser-p open-browser-p))
            (:api-key
              (let ((key (parent-provider-login--read-key input output read-secret)))
                (parent-provider-login--validate-key family key)
                (when (and cancelled (funcall cancelled))
                  (error 'parent-provider-login-cancelled))
                (parent-provider-login--save-key manager key family)))))))
     cancelled)
    (list :status ':authenticated :provider (string-downcase family) :method selected)))
