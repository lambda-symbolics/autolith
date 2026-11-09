(in-package #:autolith)

;;;; -- Gemini Installed-App OAuth Conditions --

(define-condition gemini-oauth-error (authentication-error)
  ((stage :initarg :stage :reader gemini-oauth-error-stage :type keyword
          :documentation "The OAuth stage that failed.")
   (status :initarg :status :initform nil :reader gemini-oauth-error-status
           :type          (option integer)
           :documentation "The HTTP status returned by Google, if known.")
   (code :initarg :code :initform nil :reader gemini-oauth-error-code
         :type          (option string)
         :documentation "A bounded non-secret OAuth error code, if supplied.")
   (response :initarg :response :initform nil :reader gemini-oauth-error-response
             :type          (option string)
             :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in Google installed-application OAuth for Gemini."))


;;;; -- Gemini Credential Management --

(defclass gemini-credential-source (autolith-credential-source)
  ()
  (:documentation "Autolith's private Gemini OAuth credential source."))

(defclass gemini-credential-manager (oauth-credential-manager)
  ()
  (:documentation "The Google OAuth credential manager for Gemini subscriptions."))

(defmethod credential-manager-provider-label ((manager gemini-credential-manager))
  "Name Gemini in user-visible credential failures."
  (declare (ignore manager))
  "Gemini")

(defmethod credential-manager-login-hint ((manager gemini-credential-manager))
  "Point Gemini credential failures at its installed-app login function."
  (declare (ignore manager))
  "authenticate with gemini-oauth-login")

(-> configuration-gemini-auth-path (configuration) pathname)
(defun configuration-gemini-auth-path (configuration)
  "Return Autolith's private Gemini OAuth credential pathname."
  (merge-pathnames "gemini-auth.sexp" (config :state-root configuration)))

(-> gemini-credential-manager-create (configuration) gemini-credential-manager)
(defun gemini-credential-manager-create (configuration)
  "Create a Gemini credential manager for CONFIGURATION's private store."
  (make-instance 'gemini-credential-manager
                 :primary-source
                 (make-instance 'gemini-credential-source
                                :pathname
                                (configuration-gemini-auth-path configuration))))


;;;; -- Shared Browser OAuth Adapter --

(-> gemini-oauth--fail
    (&key (:stage keyword) (:message string) (:status (option integer))
          (:code (option string)) (:response (option string)))
    nil)
(defun gemini-oauth--fail (&key stage message status code response)
  "Signal a structured Gemini OAuth failure containing only safe metadata."
  (error 'gemini-oauth-error
         :message message :stage stage :status status :code code
         :response response))

(-> gemini-oauth--request-wrapper (function) t)
(defun gemini-oauth--request-wrapper (thunk)
  "Run a Gemini token request under the provider response deadline."
  (handler-case
      (provider-call-with-response-deadline 60 thunk)
    (sb-sys:deadline-timeout ()
      (gemini-oauth--fail
       :stage   ':token-request
       :message "Google OAuth exceeded its response deadline."))))

(-> gemini-oauth--client
    (&key (:request-function function) (:client-id string) (:authorization-endpoint string)
          (:token-endpoint string) (:token-parameters list))
    browser-authentication-client)
(defun gemini-oauth--client
    (&key (request-function #'gemini-oauth--request)
          (client-id (gemini-oauth-client-id))
          (authorization-endpoint *gemini-oauth-authorization-endpoint*)
          (token-endpoint *gemini-oauth-token-endpoint*)
          (token-parameters (let ((secret (gemini-oauth-client-secret)))
                              (when secret (list (cons "client_secret" secret))))))
  "Build the shared installed-app client with Gemini's endpoint and host policy."
  (make-instance 'browser-authentication-client
                 :authorization-endpoint  authorization-endpoint
                 :token-endpoint token-endpoint :client-id client-id
                 :scope                   (format nil "~{~A~^ ~}" *gemini-oauth-scopes*)
                 :authorization-parameters
                 '(("access_type" . "offline") ("prompt" . "consent"))
                 :token-parameters        token-parameters
                 :ports '(0) :redirect-host "127.0.0.1" :callback-path "/oauth2callback"
                 :timeout                 *gemini-oauth-callback-timeout*
                 :request-function        request-function
                 :credential-function     #'gemini-oauth--credentials-from-document
                 :error-function #'gemini-oauth--fail :error-type 'gemini-oauth-error
                 :secret-function         #'call-with-secret-use
                 :bounded-string-function #'bounded-string
                 :display-function #'gemini-oauth--display-login :label "Gemini"
                 :success-response        "Gemini authentication succeeded. You may close this tab."
                 :failure-response        "Gemini authentication failed. Return to Autolith."))

(-> gemini-oauth-create-pkce () (values string string))
(defun gemini-oauth-create-pkce ()
  "Return a fresh 256-bit PKCE verifier and its S256 challenge."
  (browser-authentication-create-pkce :verifier-octets 32))

(-> gemini-oauth-authorization-url
    (&key (:redirect-uri string) (:state string) (:code-challenge string)
          (:client-id string) (:authorization-endpoint string))
    string)
(defun gemini-oauth-authorization-url
    (&key redirect-uri state code-challenge
          (client-id (gemini-oauth-client-id))
          (authorization-endpoint *gemini-oauth-authorization-endpoint*))
  "Build the Google installed-app authorization URL for one PKCE flow."
  (browser-authentication-authorization-url
   (gemini-oauth--client :authorization-endpoint authorization-endpoint
                         :client-id client-id :token-parameters nil)
   :redirect-uri redirect-uri :state state :code-challenge code-challenge))

(-> gemini-oauth-loopback-open () (values t string))
(defun gemini-oauth-loopback-open ()
  "Open an ephemeral IPv4 loopback listener and return it with its redirect URI."
  (browser-authentication-loopback-open
   (gemini-oauth--client :token-parameters nil)))

(-> gemini-oauth-loopback-close (t) (values))
(defun gemini-oauth-loopback-close (listener)
  "Close a Gemini OAuth loopback listener."
  (browser-authentication-loopback-close listener))

(-> gemini-oauth-await-loopback (t string &key (:timeout real)) string)
(defun gemini-oauth-await-loopback (listener expected-state &key
                                                   (timeout *gemini-oauth-callback-timeout*))
  "Wait at most TIMEOUT seconds for a valid Gemini loopback callback."
  (browser-authentication-await-loopback
   (gemini-oauth--client :token-parameters nil) listener expected-state :timeout timeout))


;;;; -- Token Exchange and Refresh --

(-> gemini-oauth--request (&key (:url string) (:content string)) t)
(defun gemini-oauth--request (&key url content)
  "POST one form-encoded request to Google's OAuth token endpoint."
  (browser-authentication-request
   :url url :content content
   :headers         (list (cons "User-Agent" (provider-user-agent)))
   :request-wrapper #'gemini-oauth--request-wrapper))

(-> gemini-oauth--token-document
    (function string &key (:parameters list) (:stage keyword))
    json-object)
(defun gemini-oauth--token-document (request-function endpoint &key parameters stage)
  "POST PARAMETERS and validate the JSON token response for STAGE."
  (browser-authentication-token-document
   (gemini-oauth--client :request-function request-function :token-parameters nil)
   :endpoint endpoint :parameters parameters :stage stage))

(-> gemini-oauth--credentials-from-document (credential-manager json-object) oauth-credentials)
(defun gemini-oauth--credentials-from-document (manager document)
  "Validate token response DOCUMENT and return Gemini credentials."
  (let* ((access-token  (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (id-token      (json-get document "id_token"))
         (expires-in    (json-get document "expires_in"))
         (account-id
           (or (and (non-empty-string-p id-token) (jwt-subject id-token))
               "google-main-account")))
    (unless (and (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (or (null id-token) (non-empty-string-p id-token))
                 (integerp expires-in) (plusp expires-in))
      (gemini-oauth--fail
       :stage   ':token-response
       :message "The Gemini OAuth token response omitted required fields."))
    (make-instance 'oauth-credentials
                   :access-token access-token :refresh-token refresh-token
                   :id-token id-token :account-id account-id
                   :expires-at (+ (get-universal-time) expires-in)
                   :source-path
                   (credential-source-pathname
                    (credential-manager-primary-source manager)))))

(-> gemini-oauth-exchange-code
    (credential-manager
     &key (:code string) (:verifier string) (:redirect-uri string)
          (:request-function function) (:client-id string)
          (:client-secret (option string)) (:token-endpoint string))
    oauth-credentials)
(defun gemini-oauth-exchange-code
    (manager &key code verifier redirect-uri
             (request-function #'gemini-oauth--request)
             (client-id (gemini-oauth-client-id))
             (client-secret (gemini-oauth-client-secret))
             (token-endpoint *gemini-oauth-token-endpoint*))
  "Exchange one authorization CODE using VERIFIER and persist no state."
  (let ((client (gemini-oauth--client :request-function request-function
                                     :client-id client-id :token-endpoint token-endpoint
                                     :token-parameters
                                     (when client-secret
                                       (list (cons "client_secret" client-secret))))))
    (gemini-oauth--credentials-from-document
     manager
     (browser-authentication-exchange-code
      client :code code :verifier verifier :redirect-uri redirect-uri))))

(defmethod credential-manager-token-endpoint ((manager gemini-credential-manager))
  "Return Google's installed-app OAuth token endpoint for credential refresh."
  (declare (ignore manager))
  *gemini-oauth-token-endpoint*)

(defmethod credential-manager-client-id ((manager gemini-credential-manager))
  "Return the configured Gemini installed-app client ID."
  (declare (ignore manager))
  (gemini-oauth-client-id))

(defmethod credential-manager-refresh-parameters
    ((manager gemini-credential-manager) (refresh-token string))
  "Add the installed-app client secret when one is configured."
  (let ((client-secret (gemini-oauth-client-secret)))
    (append (call-next-method)
            (when client-secret
              (list (cons "client_secret" client-secret))))))


;;;; -- Public Login Flow --

(-> gemini-oauth--display-login
    (string &key (:redirect-uri string) (:timeout real) (:stream (or stream boolean)))
    (option string))
(defun gemini-oauth--display-login (url &key redirect-uri timeout stream)
  "Print Gemini browser login instructions."
  (declare (ignore redirect-uri))
  (format stream "~&Sign in with Gemini in your browser:~%  ~A~%~%Waiting up to ~A seconds for the local callback.~%"
          url timeout))

(-> gemini-oauth-login
    (credential-manager
     &key (:stream stream) (:open-browser-p boolean) (:browser-function function)
          (:callback-function function) (:request-function function) (:timeout real))
    oauth-credentials)
(defun gemini-oauth-login
    (manager &key (stream *standard-output*) (open-browser-p t)
             (browser-function #'device-authentication-open-browser)
             (callback-function #'gemini-oauth-await-loopback)
             (request-function #'gemini-oauth--request)
             (timeout *gemini-oauth-callback-timeout*))
  "Authenticate Gemini through the shared RFC 8252 browser flow."
  (call-with-secret-use
   (lambda ()
     (browser-authentication-login
      (gemini-oauth--client :request-function request-function) manager
      :stream stream :open-browser-p open-browser-p :browser-function browser-function
      :callback-function callback-function :timeout timeout))))
