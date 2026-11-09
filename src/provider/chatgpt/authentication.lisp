(in-package #:autolith)

;;;; -- ChatGPT Browser OAuth Conditions --

(define-condition chatgpt-oauth-error (authentication-error)
  ((stage
    :initarg       :stage
    :reader        chatgpt-oauth-error-stage
    :type          keyword
    :documentation "The browser OAuth stage that failed.")
   (status
    :initarg       :status
    :initform      nil
    :reader        chatgpt-oauth-error-status
    :type          (option integer)
    :documentation "The HTTP status returned by OpenAI, if known.")
   (code
    :initarg       :code
    :initform      nil
    :reader        chatgpt-oauth-error-code
    :type          (option string)
    :documentation "A bounded non-secret OAuth error code, if supplied.")
   (response
    :initarg       :response
    :initform      nil
    :reader        chatgpt-oauth-error-response
    :type          (option string)
    :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in ChatGPT browser OAuth."))


;;;; -- RFC 8252 Provider Adapter --


(-> chatgpt-oauth--fail
    (&key (:stage keyword) (:message string) (:status (option integer))
          (:code (option string)) (:response (option string)))
    nil)
(defun chatgpt-oauth--fail (&key stage message status code response)
  "Signal a structured ChatGPT OAuth failure containing only safe metadata."
  (error 'chatgpt-oauth-error :message message :stage stage :status status
         :code code :response response))

(-> chatgpt-oauth--state-matches-p (t string) boolean)
(defun chatgpt-oauth--state-matches-p (actual expected)
  "Return true when ACTUAL matches EXPECTED, with or without the onboarding suffix."
  (and (stringp actual)
       (or (string= actual expected)
           (string= actual
                    (format nil "~A.onboarding_entrypoint=life_sciences" expected)))))

(-> chatgpt-oauth--rfc8252-client
    (&key (:request-function function) (:issuer string) (:client-id string)
          (:token-endpoint string) (:originator string))
    browser-authentication-client)
(defun chatgpt-oauth--rfc8252-client
    (&key (request-function #'chatgpt-oauth--request)
       (issuer *openai-oauth-issuer*) (client-id *openai-oauth-client-id*)
       (token-endpoint *openai-oauth-token-endpoint*)
       (originator *openai-oauth-originator*))
  "Create a browser OAuth client with ChatGPT's provider policy."
  (make-instance 'cl-rfc8252:browser-authentication-client
                 :authorization-endpoint
                 (format nil "~A/oauth/authorize"
                         (string-right-trim '(#\/) issuer))
                 :token-endpoint token-endpoint :client-id client-id
                 :scope                   (format nil "~{~A~^ ~}" *openai-oauth-scopes*)
                 :authorization-parameters
                 (list (cons "id_token_add_organizations" "true")
                       (cons "codex_cli_simplified_flow" "true")
                       (cons "originator" originator))
                 :ports                   *chatgpt-oauth-callback-ports*
                 :redirect-host "localhost" :callback-path "/auth/callback"
                 :verifier-octets 64 :state-test #'chatgpt-oauth--state-matches-p
                 :timeout                 *chatgpt-oauth-callback-timeout*
                 :headers                 (list (cons "originator" originator))
                 :request-function        request-function
                 :credential-function     #'chatgpt-oauth--credentials-from-document
                 :error-function          #'chatgpt-oauth--fail
                 :error-type              'authentication-error
                 :bounded-string-function #'bounded-string
                 :display-function        #'chatgpt-oauth--display-login
                 :secret-function #'call-with-secret-use :label "ChatGPT"
                 :success-response        "ChatGPT authorization was received. Return to Autolith."
                 :failure-response        "ChatGPT authorization failed. Return to Autolith."
                 :mismatch-response       "ChatGPT authorization did not match this login."))

(-> chatgpt-oauth-create-pkce () (values string string))
(defun chatgpt-oauth-create-pkce ()
  "Return a fresh ChatGPT PKCE verifier and S256 challenge."
  (cl-rfc8252:browser-authentication-create-pkce :verifier-octets 64))

(-> chatgpt-oauth--state () string)
(defun chatgpt-oauth--state ()
  "Return a fresh browser OAuth state value."
  (cl-rfc8252:browser-authentication-state))

(-> chatgpt-oauth-authorization-url
    (&key (:redirect-uri string) (:state string) (:code-challenge string)
          (:issuer string) (:client-id string) (:originator string))
    string)
(defun chatgpt-oauth-authorization-url
    (&key redirect-uri state code-challenge (issuer *openai-oauth-issuer*)
       (client-id *openai-oauth-client-id*) (originator *openai-oauth-originator*))
  "Build the ChatGPT authorization URL through cl-rfc8252."
  (cl-rfc8252:browser-authentication-authorization-url
   (chatgpt-oauth--rfc8252-client :issuer issuer :client-id client-id
                                  :originator originator)
   :redirect-uri redirect-uri :state state :code-challenge code-challenge))

(-> chatgpt-oauth-loopback-open () (values t string))
(defun chatgpt-oauth-loopback-open ()
  "Open the ChatGPT RFC 8252 loopback listener."
  (cl-rfc8252:browser-authentication-loopback-open
   (chatgpt-oauth--rfc8252-client)))

(-> chatgpt-oauth-await-loopback (t string &key (:timeout real)) string)
(defun chatgpt-oauth-await-loopback
    (listener expected-state &key (timeout *chatgpt-oauth-callback-timeout*))
  "Await a bounded ChatGPT callback."
  (cl-rfc8252:browser-authentication-await-loopback
   (chatgpt-oauth--rfc8252-client) listener expected-state :timeout timeout))

(-> chatgpt-oauth--request-wrapper (function) t)
(defun chatgpt-oauth--request-wrapper (thunk)
  "Run an OpenAI token request under the provider response deadline."
  (handler-case
      (provider-call-with-response-deadline 60 thunk)
    (sb-sys:deadline-timeout ()
      (error 'authentication-error :message "OpenAI OAuth exceeded its response deadline."))))

(-> chatgpt-oauth--request (&key (:url string) (:content string)) t)
(defun chatgpt-oauth--request (&key url content)
  "POST an OAuth form with Autolith's User-Agent and OpenAI's originator header."
  (cl-rfc8252:browser-authentication-request
   :url url :content content
   :headers         (list (cons "User-Agent" (provider-user-agent))
                          (cons "originator" *openai-oauth-originator*))
   :request-wrapper #'chatgpt-oauth--request-wrapper))

(-> chatgpt-oauth--token-document
    (function string &key (:parameters list) (:stage keyword))
    json-object)
(defun chatgpt-oauth--token-document (request-function endpoint &key parameters stage)
  "Request a token document through the shared redaction and validation policy."
  (cl-rfc8252:browser-authentication-token-document
   (chatgpt-oauth--rfc8252-client :request-function request-function :token-endpoint endpoint)
   :parameters parameters :stage stage))

(-> chatgpt-oauth--display-login
    (string &key (:redirect-uri string) (:timeout real) (:stream (or stream boolean)))
    (option string))
(defun chatgpt-oauth--display-login (url &key redirect-uri timeout stream)
  "Print ChatGPT browser login instructions."
  (format stream "~&Sign in with ChatGPT in your browser:~%  ~A~%~%Callback: ~A~%Waiting up to ~A seconds for the local callback.~%"
          url redirect-uri timeout))

(-> chatgpt-oauth--credentials-from-document (credential-manager json-object) oauth-credentials)
(defun chatgpt-oauth--credentials-from-document (manager document)
  "Validate DOCUMENT and return ChatGPT OAuth credentials."
  (let* ((id-token      (json-get document "id_token"))
         (access-token  (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (account-id    (or (and (non-empty-string-p id-token) (jwt-account-id id-token))
                            (and (non-empty-string-p access-token) (jwt-account-id access-token)))))
    (unless (and (non-empty-string-p id-token) (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token) (non-empty-string-p account-id))
      (chatgpt-oauth--fail :stage   ':token-response
                           :message "The ChatGPT OAuth token response omitted required fields."))
    (make-instance 'oauth-credentials :access-token access-token :refresh-token refresh-token
                   :id-token id-token :account-id account-id
                   :expires-at  (or (jwt-expiration access-token) (jwt-expiration id-token))
                   :source-path (credential-source-pathname
                                 (credential-manager-primary-source manager)))))

(-> chatgpt-oauth-exchange-code
    (credential-manager
     &key (:code string) (:verifier string) (:redirect-uri string)
          (:request-function function) (:client-id string) (:token-endpoint string))
    oauth-credentials)
(defun chatgpt-oauth-exchange-code
    (manager &key code verifier redirect-uri (request-function #'chatgpt-oauth--request)
       (client-id *openai-oauth-client-id*) (token-endpoint *openai-oauth-token-endpoint*))
  "Exchange a ChatGPT authorization code and validate its credentials."
  (let ((client (chatgpt-oauth--rfc8252-client :request-function request-function
                                                :client-id       client-id
                                                :token-endpoint  token-endpoint)))
    (chatgpt-oauth--credentials-from-document
     manager
     (cl-rfc8252:browser-authentication-exchange-code
      client :code code :verifier verifier :redirect-uri redirect-uri))))

(-> chatgpt-oauth-login
    (credential-manager
     &key (:stream stream) (:open-browser-p boolean) (:browser-function function)
          (:callback-function function) (:request-function function) (:timeout real))
    oauth-credentials)
(defun chatgpt-oauth-login
    (manager &key (stream *standard-output*) (open-browser-p t)
       (browser-function #'device-authentication-open-browser)
       (callback-function #'chatgpt-oauth-await-loopback)
       (request-function #'chatgpt-oauth--request)
       (timeout *chatgpt-oauth-callback-timeout*))
  "Authenticate ChatGPT through the shared RFC 8252 browser flow."
  (cl-rfc8252:browser-authentication-login
   (chatgpt-oauth--rfc8252-client :request-function request-function)
   manager :stream stream :open-browser-p open-browser-p
   :browser-function browser-function :callback-function callback-function
   :timeout timeout))
