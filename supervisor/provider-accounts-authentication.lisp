;;;; -- Trusted Provider Accounts --

(in-package #:autolith-supervisor)

(defclass family-account-manager ()
  ((settings :initarg :settings :initform nil :reader family-account-manager-settings
             :documentation "Trusted settings or a zero-argument host settings callback."))
  (:documentation "Account policy independent of a host's configuration representation."))

(declaim (ftype (function (family-account-manager keyword t) t) family-account-manager-setting))

(defun family-account-manager-setting (manager key fallback)
  "Read current trusted settings without retaining host configuration or request state."
  (let* ((settings      (or *provider-account-active-configuration*
                            (family-account-manager-settings manager)))
         (configuration (if (functionp settings) (funcall settings) settings)))
    (or (and configuration (provider-account-config key configuration)) fallback)))

(defparameter *api-key-store-version*
  1
  "The portable version of Autolith's OpenAI-compatible API-key store.")

(defclass api-key-credential-source (autolith-credential-source)
          ((provider-name :initarg :provider-name :reader api-key-credential-source-provider-name
            :type family-account-non-empty-string :documentation
            "The provider name associated with this key source."))
          (:documentation "A private persistent credential source for one API-key provider."))

(declaim (ftype (function (string) string) api-key--canonical-provider-name))

(defun api-key--canonical-provider-name (provider-name)
  "Return the case-insensitive store key for PROVIDER-NAME."
  (string-downcase provider-name))

(declaim (ftype (function (pathname function) t) api-key--call-with-store-lock))

(defun api-key--call-with-store-lock (pathname function)
  "Call FUNCTION while holding the process and filesystem API-key locks."
  (let ((lock-pathname
         (merge-pathnames "api-keys.lock" (uiop/pathname:pathname-directory-pathname pathname))))
    (handler-case (ls-flock:call-with-file-lock lock-pathname function)
                  (authentication-error (condition) (error condition))
                  (error (cause)
                         (error 'authentication-error :message
                                (format nil "Could not lock the API-key store at ~A: ~A"
                                        lock-pathname cause))))))

(declaim (ftype (function (pathname) null) api-key--invalid-store))

(defun api-key--invalid-store (pathname)
  "Signal that PATHNAME is not a valid private API-key store."
  (error 'authentication-error :message
         (format nil "Invalid Autolith API-key store at ~A." pathname)))

(declaim (ftype (function (list) (or null string)) api-key--entry-provider-name))

(defun api-key--entry-provider-name (entry)
  "Return the provider name from one validated or unvalidated ENTRY."
  (and (listp entry) (getf (rest entry) :provider-name)))

(declaim (ftype (function (list) (or null string)) api-key--entry-key))

(defun api-key--entry-key (entry)
  "Return the secret key from one validated or unvalidated ENTRY."
  (and (listp entry) (getf (rest entry) :api-key)))

(declaim (ftype (function (list pathname) list) api-key--validate-entries))

(defun api-key--validate-entries (entries pathname)
  "Validate API-key ENTRIES read from PATHNAME and return them unchanged."
  (unless (listp entries) (api-key--invalid-store pathname))
  (let ((seen nil))
    (dolist (entry entries)
      (let ((provider-name (api-key--entry-provider-name entry))
            (api-key       (api-key--entry-key entry)))
        (unless
            (and (listp entry) (eq (first entry) :provider) (family-account-non-empty-string-p provider-name)
                 (family-account-non-empty-string-p api-key))
          (api-key--invalid-store pathname))
        (let ((canonical-name (api-key--canonical-provider-name provider-name)))
          (when (member canonical-name seen :test #'string=) (api-key--invalid-store pathname))
          (push canonical-name seen))))
    entries))

(declaim (ftype (function (pathname) list) api-key--read-entries))

(defun api-key--read-entries (pathname)
  "Read and validate the provider entries in PATHNAME, or return NIL when absent."
  (if (not (probe-file pathname))
      nil
      (handler-case
       (let* ((record  (read-portable-form pathname))
              (version (and (listp record) (getf (rest record) :version)))
              (entries
               (and (listp record) (eq (first record) :api-keys) (getf (rest record) :providers))))
         (unless
             (and (listp record) (eq (first record) :api-keys) (= version *api-key-store-version*))
           (api-key--invalid-store pathname))
         (api-key--validate-entries entries pathname))
       (authentication-error (condition) (error condition))
       (error nil (api-key--invalid-store pathname)))))

(declaim (ftype (function (string list) (or null list)) api-key--entry-for))

(defun api-key--entry-for (provider-name entries)
  "Find the API-key entry for PROVIDER-NAME in ENTRIES."
  (find (api-key--canonical-provider-name provider-name) entries :key
        (lambda (entry) (api-key--canonical-provider-name (api-key--entry-provider-name entry)))
        :test #'string=))

(defmethod cl-rfc8628:credential-source-label ((source api-key-credential-source))
  "Name the API-key provider in credential failures."
  (api-key-credential-source-provider-name source))

(defmethod cl-rfc8628:credential-source-load ((source api-key-credential-source))
  "Load SOURCE's API key into request scope, without retaining it in the image."
  (let ((pathname (cl-rfc8628:credential-source-pathname source)))
    (api-key--call-with-store-lock pathname
     (lambda ()
       (let* ((entry
               (api-key--entry-for (api-key-credential-source-provider-name source)
                (api-key--read-entries pathname)))
              (access-token (and entry (api-key--entry-key entry))))
         (when (family-account-non-empty-string-p access-token)
           (make-instance 'cl-rfc8628:oauth-credentials :access-token access-token :refresh-token
                          nil :id-token nil :account-id
                          (format nil "api-key/~A"
                                  (api-key--canonical-provider-name
                                   (api-key-credential-source-provider-name source)))
                          :expires-at nil :source-path pathname)))))))

(defmethod cl-rfc8628:credential-source-save
           ((source api-key-credential-source)
            (credentials cl-rfc8628:oauth-credentials))
  "Atomically save CREDENTIALS' API key in SOURCE's private store."
  (call-with-secret-use
   (lambda ()
     (let ((api-key (cl-rfc8628:oauth-credentials-access-token credentials)))
       (unless (family-account-non-empty-string-p api-key)
         (error 'authentication-error :message
                (format nil "The ~A API key is empty."
                        (cl-rfc8628:credential-source-label source))))
       (let* ((pathname (cl-rfc8628:credential-source-pathname source))
              (provider-name
               (api-key--canonical-provider-name
                (api-key-credential-source-provider-name source))))
         (api-key--call-with-store-lock pathname
          (lambda ()
            (let* ((entries (api-key--read-entries pathname))
                   (updated-entry
                    (list :provider :provider-name provider-name :api-key
                          api-key)))
              (ensure-directories-exist pathname)
              (sexp-store:snapshot-write pathname
                                         (list :api-keys :version
                                               *api-key-store-version*
                                               :providers
                                               (append
                                                (remove-if
                                                 (lambda (entry)
                                                   (string= provider-name
                                                            (api-key--canonical-provider-name
                                                             (api-key--entry-provider-name
                                                              entry))))
                                                 entries)
                                                (list updated-entry)))
                                         :mode 384)))))
       credentials))))

(defclass api-key-credential-manager (credential-manager) nil
          (:documentation "A credential manager for one persistent API-key source."))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager api-key-credential-manager))
  "Name the API-key provider in credential failures."
  (cl-rfc8628:credential-source-label (cl-rfc8628:credential-manager-primary-source manager)))

(defmethod cl-rfc8628:credential-manager-credential-description
           ((manager api-key-credential-manager))
  "Describe static API-key credentials precisely."
  (declare (ignore manager))
  "API key")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager api-key-credential-manager))
  "Describe the command that stores this provider's API key."
  (format nil "run autolith auth ~A to enter it"
          (cl-rfc8628:credential-manager-provider-label manager)))

(defmethod cl-rfc8628:credential-manager-refreshable-p ((manager api-key-credential-manager))
  "Static provider API keys cannot refresh."
  (declare (ignore manager))
  nil)

(declaim
 (ftype
  (function (&key (:provider-name family-account-non-empty-string) (:pathname pathname))
   api-key-credential-manager)
  api-key-credential-manager-create))

(defun api-key-credential-manager-create (&key provider-name pathname)
  "Create an API-key manager backed by PROVIDER-NAME in PATHNAME."
  (let ((source
         (make-instance 'api-key-credential-source :pathname pathname :provider-name
                        provider-name)))
    (make-instance 'api-key-credential-manager :primary-source source :bootstrap-source source)))

(defclass environment-api-key-credential-source (cl-rfc8628:environment-credential-source) nil
          (:documentation
           "A read-only adapter loading one static provider API key from the environment."))

(declaim (ftype (function (string) pathname) environment-api-key-credential-source--pathname))

(defun environment-api-key-credential-source--pathname (account-id)
  "Return the conventional reporting path for ACCOUNT-ID's environment source."
  (merge-pathnames (format nil "~A-auth.sexp" account-id)
                   (merge-pathnames "autolith/"
                                    (environment-directory "XDG_STATE_HOME"
                                     (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defmethod cl-rfc8628:credential-source-pathname ((source environment-api-key-credential-source))
  "Report a conventional key path because the environment has no pathname."
  (environment-api-key-credential-source--pathname
   (cl-rfc8628:environment-credential-source-account-id source)))

(defclass static-api-key-credential-manager
          (api-key-credential-manager cl-rfc8628:static-credential-manager) nil
          (:documentation "A static API-key manager that prefers the current environment key."))

(declaim (ftype (function (string) string) grok-oauth-url))

(defun grok-oauth-url (path)
  "Return the xAI OAuth issuer joined to absolute PATH."
  (concatenate 'string *grok-oauth-issuer* path))

(declaim (ftype (function nil string) grok-oauth-token-endpoint))

(defun grok-oauth-token-endpoint ()
  "Return the xAI OAuth token endpoint."
  (grok-oauth-url "/oauth2/token"))

(declaim (ftype (function nil string) grok-auth-json-scope))

(defun grok-auth-json-scope ()
  "Return the Grok Build auth.json scope key of the first-party xAI client."
  (format nil "~A::~A" *grok-oauth-issuer* *grok-oauth-client-id*))

(defclass grok-bootstrap-credential-source (credential-source) nil
          (:documentation "A read-only adapter for an existing Grok Build auth.json file."))

(defmethod cl-rfc8628:credential-source-label ((source grok-bootstrap-credential-source))
  "Name the Grok Build bootstrap source in user-visible failures."
  (declare (ignore source))
  "Grok Build")

(defmethod cl-rfc8628:credential-source-load ((source grok-bootstrap-credential-source))
  "Load one non-renewable Grok bootstrap credential without modifying Grok Build."
  (let ((pathname (cl-rfc8628:credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
       (let* ((document (read-json-file-with-retry pathname))
              (record   (cl-rfc8628:json-get document (grok-auth-json-scope)))
              (auth-mode
               (and (cl-rfc8628:json-object-p record) (cl-rfc8628:json-get record "auth_mode")))
              (access-token
               (and (cl-rfc8628:json-object-p record) (cl-rfc8628:json-get record "key")))
              (user-id
               (and (cl-rfc8628:json-object-p record) (cl-rfc8628:json-get record "user_id")))
              (account-id
               (or (and (family-account-non-empty-string-p user-id) user-id)
                   (and (stringp access-token) (cl-rfc8628:jwt-subject access-token))))
              (expires-at
               (and (cl-rfc8628:json-object-p record) (cl-rfc8628:json-get record "expires_at"))))
         (when
             (and (stringp auth-mode) (string-equal auth-mode "oidc")
                  (family-account-non-empty-string-p access-token) (family-account-non-empty-string-p account-id))
           (make-instance 'cl-rfc8628:oauth-credentials :access-token access-token :refresh-token
                          nil :id-token nil :account-id account-id :expires-at
                          (or (and (stringp expires-at) (rfc3339->universal-time expires-at))
                              (cl-rfc8628:jwt-expiration access-token))
                          :source-path pathname)))
       (error nil nil)))))

(defmethod cl-rfc8628:credential-source-save
           ((source grok-bootstrap-credential-source) (credentials cl-rfc8628:oauth-credentials))
  "Reject writes to the Grok Build bootstrap source."
  (declare (ignore credentials))
  (error 'authentication-error :message
         (format nil "The Grok Build bootstrap store ~A is read-only."
                 (cl-rfc8628:credential-source-pathname source))))

(defclass grok-credential-manager
          (family-account-manager oauth-credential-manager) nil
          (:documentation
           "The xAI OAuth credential manager behind the Grok provider."))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager grok-credential-manager))
  "Name the Grok account service in user-visible failures."
  (declare (ignore manager))
  "Grok")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager grok-credential-manager))
  "Point Grok credential failures at the Grok login command."
  (declare (ignore manager))
  "run autolith auth grok")

(defmethod cl-rfc8628:credential-manager-token-endpoint ((manager grok-credential-manager))
  "Refresh Grok credentials at the xAI OAuth token endpoint."
  (declare (ignore manager))
  (grok-oauth-token-endpoint))

(defmethod cl-rfc8628:credential-manager-client-id ((manager grok-credential-manager))
  "Refresh as the first-party xAI OAuth client."
  (declare (ignore manager))
  *grok-oauth-client-id*)

(defclass nous-credential-manager
          (family-account-manager oauth-credential-manager)
          ((refresh-request-function :initarg :refresh-request-function
            :initform nil :reader
            nous-credential-manager-refresh-request-function :type
            (or null function) :documentation
            "An injected HTTP request function replacing the refresh transport, if any."))
          (:documentation
           "The OAuth credential manager for Nous Research inference."))

(declaim (ftype (function (pathname) pathname) nous-authentication--lock-pathname))

(defun nous-authentication--lock-pathname (credential-pathname)
  "Return the process-shared lock pathname for CREDENTIAL-PATHNAME."
  (merge-pathnames "nous-auth.lock"
                   (uiop/pathname:pathname-directory-pathname credential-pathname)))

(declaim (ftype (function (pathname function) t) nous-authentication--call-with-store-lock))

(defun nous-authentication--call-with-store-lock (credential-pathname function)
  "Call FUNCTION while holding the process and filesystem Nous OAuth locks."
  (let ((lock-pathname (nous-authentication--lock-pathname credential-pathname)))
    (handler-case (ls-flock:call-with-file-lock lock-pathname function)
                  (authentication-error (condition) (error condition))
                  (error (cause)
                         (error 'authentication-error :message
                                (format nil "Could not lock the Nous OAuth store at ~A: ~A"
                                        lock-pathname cause))))))

(declaim (ftype (function (t) list) nous-authentication--scope-values))

(defun nous-authentication--scope-values (value)
  "Return normalized OAuth scope strings represented by VALUE."
  (labels ((collect-scopes (candidate)
             (cond
              ((stringp candidate)
               (remove-if-not #'family-account-non-empty-string-p
                              (uiop/utility:split-string candidate :separator
                                                         '(#\  #\Tab #\Newline #\Return #\,))))
              ((vectorp candidate)
               (loop for item across candidate
                     append (collect-scopes item)))
              ((listp candidate)
               (loop for item in candidate
                     append (collect-scopes item)))
              (t
               nil))))
    (remove-duplicates (collect-scopes value) :test #'string=)))

(declaim (ftype (function (string string) boolean) nous-authentication--access-token-scope-p))

(defun nous-authentication--access-token-scope-p (access-token required-scope)
  "Return true when ACCESS-TOKEN is a JWT carrying REQUIRED-SCOPE."
  (let ((payload (cl-rfc8628:jwt-payload access-token)))
    (if (and payload
             (member required-scope
                     (append
                      (nous-authentication--scope-values (cl-rfc8628:json-get payload "scope"))
                      (nous-authentication--scope-values (cl-rfc8628:json-get payload "scp")))
                     :test #'string=))
        t
        nil)))

(declaim (ftype (function (string) (or null string)) nous-authentication--access-token-account-id))

(defun nous-authentication--access-token-account-id (access-token)
  "Return the stable subject carried by a valid Nous access JWT."
  (cl-rfc8628:jwt-subject access-token))

(declaim
 (ftype
  (function (nous-credential-manager cl-rfc8628:oauth-credentials) cl-rfc8628:oauth-credentials)
  nous-authentication--validate-stored-credentials))

(defun nous-authentication--validate-stored-credentials (manager credentials)
  "Validate stored Nous CREDENTIALS before returning them to request scope."
  (let* ((access-token  (cl-rfc8628:oauth-credentials-access-token credentials))
         (token-account (nous-authentication--access-token-account-id access-token)))
    (unless
        (and (cl-rfc8628:jwt-payload access-token)
             (nous-authentication--access-token-scope-p access-token *nous-oauth-scope*)
             (family-account-non-empty-string-p token-account)
             (string= token-account (cl-rfc8628:oauth-credentials-account-id credentials)))
      (error 'credentials-unavailable :message
             (format nil "The stored Nous OAuth credentials cannot invoke inference; ~A."
                     (cl-rfc8628:credential-manager-login-hint manager))
             :searched-paths (list (cl-rfc8628:oauth-credentials-source-path credentials))))
    credentials))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager nous-credential-manager))
  "Name the Nous Research account service in user-visible failures."
  (declare (ignore manager))
  "Nous Research")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager nous-credential-manager))
  "Point Nous credential failures at the browser login command."
  (declare (ignore manager))
  "run autolith auth nous")

(declaim
 (ftype
  (function (provider-account-configuration &key (:refresh-request-function (or null function)))
   nous-credential-manager)
  nous-credential-manager-create))

(defun nous-credential-manager-create
       (provider-account-configuration &key refresh-request-function)
  "Create a Nous credential manager for CONFIGURATION's private state root."
  (make-instance 'nous-credential-manager :settings provider-account-configuration :primary-source
                 (make-instance 'autolith-credential-source :pathname
                                (configuration-nous-auth-path provider-account-configuration))
                 :refresh-request-function refresh-request-function))

(defmethod cl-rfc8628:credential-manager-call-with-refresh-lock
           ((manager nous-credential-manager) (function function))
  "Serialize Nous rotation and publication across processes sharing the state root."
  (nous-authentication--call-with-store-lock
   (cl-rfc8628:credential-source-pathname (cl-rfc8628:credential-manager-primary-source manager))
   function))

(defmethod cl-rfc8628:credential-manager-validate-credentials
           ((manager nous-credential-manager) (credentials cl-rfc8628:oauth-credentials))
  "Require stored Nous credentials to carry the inference scope for their account."
  (nous-authentication--validate-stored-credentials manager credentials))

(defmethod cl-rfc8628:credential-manager-load ((manager nous-credential-manager))
  "Load only Autolith-owned Nous credentials under the shared store lock."
  (cl-rfc8628:credential-manager-call-with-refresh-lock manager (lambda () (call-next-method))))

(defmethod cl-rfc8628:credential-manager-token-endpoint ((manager nous-credential-manager))
  "Refresh at the trusted portal selected by the host."
  (concatenate 'string
               (string-right-trim "/" (family-account-manager-setting manager ':nous-portal-url
                                                                       (nous-portal-url)))
               "/api/oauth/token"))

(defmethod cl-rfc8628:credential-manager-client-id ((manager nous-credential-manager))
  "Refresh as the Hermes CLI OAuth client."
  (declare (ignore manager))
  *nous-oauth-client-id*)

(defmethod cl-rfc8628:credential-manager-refresh-parameters
           ((manager nous-credential-manager) (refresh-token string))
  "Keep the refresh token out of the form, since Nous reads it from a header."
  (remove "refresh_token" (call-next-method) :key #'first :test #'string=))

(defmethod cl-rfc8628:credential-manager-refresh-headers
           ((manager nous-credential-manager) (refresh-token string))
  "Send the single-use refresh token in the header Nous reads it from."
  (declare (ignore manager))
  (list (cons "x-nous-refresh-token" refresh-token)))

(defmethod cl-rfc8628:credential-manager-refresh-request
           ((manager nous-credential-manager) &key url headers content)
  "POST through the injected request function when one replaces the transport."
  (let ((function (nous-credential-manager-refresh-request-function manager)))
    (if function
        (funcall function :method ':post :url url :headers headers :content content)
        (call-next-method))))

(defmethod cl-rfc8628:credential-manager-validate-refresh-response
           ((manager nous-credential-manager) (document hash-table)
            (credentials cl-rfc8628:oauth-credentials))
  "Require a rotated single-use refresh token and a scoped access JWT naming its subject."
  (declare (ignore manager))
  (let ((access-token  (cl-rfc8628:json-get document "access_token"))
        (refresh-token (cl-rfc8628:json-get document "refresh_token")))
    (unless
        (and (family-account-non-empty-string-p refresh-token)
             (not
              (string= refresh-token (cl-rfc8628:oauth-credentials-refresh-token credentials))))
      (error 'token-refresh-failed :message
             "The Nous OAuth refresh response omitted rotated credentials." :status nil :response
             nil))
    (unless (nous-authentication--access-token-scope-p access-token *nous-oauth-scope*)
      (error 'token-refresh-failed :message
             "The refreshed Nous access token lacks the inference:invoke scope." :status nil
             :response nil))
    (unless (family-account-non-empty-string-p (nous-authentication--access-token-account-id access-token))
      (error 'token-refresh-failed :message "The refreshed Nous access token omitted its subject."
             :status nil :response nil))))

(defclass gemini-credential-source (autolith-credential-source) nil
          (:documentation "Autolith's private Gemini OAuth credential source."))

(defclass gemini-credential-manager
          (family-account-manager oauth-credential-manager) nil
          (:documentation
           "The Google OAuth credential manager for Gemini subscriptions."))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager gemini-credential-manager))
  "Name Gemini in user-visible credential failures."
  (declare (ignore manager))
  "Gemini")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager gemini-credential-manager))
  "Point Gemini credential failures at its installed-app login function."
  (declare (ignore manager))
  "authenticate with gemini-oauth-login")

(declaim
 (ftype (function (provider-account-configuration) pathname) configuration-gemini-auth-path))

(defun configuration-gemini-auth-path (provider-account-configuration)
  "Return Autolith's private Gemini OAuth credential pathname."
  (merge-pathnames "gemini-auth.sexp"
                   (provider-account-config :state-root provider-account-configuration)))

(declaim
 (ftype (function (provider-account-configuration) gemini-credential-manager)
  gemini-credential-manager-create))

(defun gemini-credential-manager-create (provider-account-configuration)
  "Create a Gemini credential manager for CONFIGURATION's private store."
  (make-instance 'gemini-credential-manager :settings provider-account-configuration :primary-source
                 (make-instance 'gemini-credential-source :pathname
                                (configuration-gemini-auth-path provider-account-configuration))))

(defmethod cl-rfc8628:credential-manager-token-endpoint ((manager gemini-credential-manager))
  "Refresh Gemini credentials at Google's installed-app OAuth token endpoint."
  (declare (ignore manager))
  *gemini-oauth-token-endpoint*)

(defmethod cl-rfc8628:credential-manager-client-id ((manager gemini-credential-manager))
  "Refresh as the currently configured installed-app client."
  (family-account-manager-setting manager ':gemini-client-id (gemini-oauth-client-id)))

(defmethod cl-rfc8628:credential-manager-refresh-parameters
    ((manager gemini-credential-manager) (refresh-token string))
  "Add the current trusted client secret when configured."
  (let ((client-secret (family-account-manager-setting manager ':gemini-client-secret
                                                      (gemini-oauth-client-secret))))
    (append (call-next-method)
            (when client-secret (list (cons "client_secret" client-secret))))))

(defparameter *copilot-oauth-client-id*
  "Iv1.b507a08c87ecfe98"
  "The public GitHub device-flow client used by Copilot Chat.")

(declaim (ftype (function nil string) copilot--domain))

(defun copilot--domain ()
  "Return the GitHub.com or Enterprise Cloud issuer configured for Copilot."
  (let* ((domain (string-downcase (or (uiop/os:getenvp "AUTOLITH_COPILOT_DOMAIN") "github.com")))
         (labels (uiop/utility:split-string domain :separator '(#\.)))
         (tenant (first labels)))
    (unless
        (or (string= domain "github.com")
            (and (= (length labels) 3) (equal (rest labels) '("ghe" "com"))
                 (<= 1 (length tenant) 63) (not (char= (char tenant 0) #\-))
                 (not (char= (char tenant (1- (length tenant))) #\-))
                 (every
                  (lambda (character)
                    (or (find character "abcdefghijklmnopqrstuvwxyz0123456789-") nil))
                  tenant)))
      (error 'family-account-settings-error :reason ':invalid-copilot-domain :message
             "AUTOLITH_COPILOT_DOMAIN must be github.com or a TENANT.ghe.com Enterprise Cloud hostname."))
    domain))

(declaim (ftype (function (string) list) copilot--headers))

(defun copilot--headers (token)
  "Use the shared Copilot integration headers with Autolith's user agent."
  (cl-llm-provider-api:copilot-http-headers token :user-agent (provider-user-agent)))

(declaim
 (ftype (function (string string &key (:headers list)) cl-rfc8628:json-object) copilot--get-json))

(defun copilot--get-json (url token &key headers)
  "GET one authenticated JSON object without retaining secret-bearing HTTP errors."
  (if *provider-account-copilot-json-function*
      (funcall *provider-account-copilot-json-function* url token :headers headers)
      (handler-case
       (multiple-value-bind (body status)
           (cl-llm-provider-api:provider-call-with-response-deadline 30
                                                                     (lambda ()
                                                                       (dexador:get url :headers
                                                                                    (append
                                                                                     (copilot--headers
                                                                                      token)
                                                                                     headers)
                                                                                    :force-string t
                                                                                    :keep-alive   nil
                                                                                    :max-redirects
                                                                                    0
                                                                                    :connect-timeout
                                                                                    10
                                                                                    :read-timeout
                                                                                    30)))
         (unless (and (integerp status) (<= 200 status 299))
           (error 'authentication-error :message
                  (format nil "GitHub Copilot returned HTTP ~A; run autolith auth copilot."
                          status)))
         (let ((document (cl-rfc8628:json-decode body)))
           (unless (cl-rfc8628:json-object-p document)
             (error 'authentication-error :message "GitHub Copilot returned invalid JSON."))
           document))
       (authentication-error (condition) (error condition))
       (dexador.error:http-request-failed (condition)
                                          (error 'authentication-error :message
                                                 (format nil
                                                         "GitHub Copilot returned HTTP ~A; run autolith auth copilot."
                                                         (dexador.error:response-status
                                                          condition))))
       (error nil
              (error 'authentication-error :message
                     "GitHub Copilot could not be reached or returned invalid JSON.")))))

(defclass copilot-credential-manager (credential-manager)
          ((domain :initarg :domain :reader copilot-credential-manager-domain :type string
            :documentation "The GitHub host authorizing this manager."))
          (:documentation "A manager renewing Copilot tokens with a persistent GitHub token."))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager copilot-credential-manager))
  "Name the Copilot account service."
  (declare (ignore manager))
  "GitHub Copilot")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager copilot-credential-manager))
  "Point Copilot credential failures at device login."
  (declare (ignore manager))
  "run autolith auth copilot")

(defmethod cl-rfc8628:credential-manager-validate-credentials
           ((manager copilot-credential-manager) (credentials cl-rfc8628:oauth-credentials))
  "Prevent a stored GitHub token from being sent to a different issuer."
  (unless
      (uiop/utility:string-prefix-p
       (concatenate 'string (copilot-credential-manager-domain manager) ":")
       (cl-rfc8628:oauth-credentials-account-id credentials))
    (error 'authentication-error :message
           "Copilot credentials belong to a different GitHub host; run autolith auth copilot."))
  credentials)

(declaim
 (ftype (function (copilot-credential-manager string string) cl-rfc8628:oauth-credentials)
  copilot--exchange))

(defun copilot--exchange (manager github-token account-id)
  "Exchange GITHUB-TOKEN for a validated Copilot token with an early expiry."
  (let* ((document
          (copilot--get-json
           (format nil "https://api.~A/copilot_internal/v2/token"
                   (copilot-credential-manager-domain manager))
           github-token))
         (token      (cl-rfc8628:json-get document "token"))
         (expires-at (cl-rfc8628:json-get document "expires_at")))
    (unless
        (and (family-account-non-empty-string-p token) (integerp expires-at)
             (> expires-at (- (get-universal-time) 2208988800)))
      (error 'authentication-error :message
             "GitHub did not return a valid Copilot token; check your Copilot subscription."))
    (make-instance 'cl-rfc8628:oauth-credentials :access-token token :refresh-token github-token
                   :account-id account-id :expires-at (+ expires-at 2208988800) :source-path
                   (cl-rfc8628:credential-source-pathname
                    (cl-rfc8628:credential-manager-primary-source manager)))))

(defmethod cl-rfc8628:credential-manager-refresh-exchange
           ((manager copilot-credential-manager) (credentials cl-rfc8628:oauth-credentials)
            refresh-token)
  "Renew the short-lived token without rotating the persistent GitHub credential."
  (cl-rfc8628:credential-manager-validate-credentials manager credentials)
  (values
   (copilot--exchange manager refresh-token (cl-rfc8628:oauth-credentials-account-id credentials))
   t))

(declaim (ftype (function (cl-rfc8628:oauth-credentials) string) copilot--base-url))

(defun copilot--base-url (credentials)
  "Resolve the trusted API host belonging to CREDENTIALS' pinned issuer."
  (let* ((account (cl-rfc8628:oauth-credentials-account-id credentials))
         (domain  (subseq account 0 (position #\: account))))
    (handler-case
     (cl-llm-provider-api:copilot-base-url (cl-rfc8628:oauth-credentials-access-token credentials)
                                           :domain domain)
     (cl-llm-provider-api:provider-error (condition)
      (error 'authentication-error :message (autolith-error-message condition))))))

(declaim
 (ftype
  (function (api-key-credential-manager family-account-non-empty-string) cl-rfc8628:OAUTH-CREDENTIALS)
  api-key-credential-manager-save-key))

(defun api-key-credential-manager-save-key (manager api-key)
  "Save API-KEY for MANAGER and return request-scoped credential metadata."
  (call-with-secret-use
   (lambda ()
     (let* ((source (cl-rfc8628:CREDENTIAL-MANAGER-PRIMARY-SOURCE manager))
            (credentials
              (make-instance
               'cl-rfc8628:OAUTH-CREDENTIALS
               :access-token  api-key
               :refresh-token nil
               :id-token      nil
               :account-id
               (format nil "api-key/~A"
                       (api-key--canonical-provider-name
                        (api-key-credential-source-provider-name source)))
               :expires-at    nil
               :source-path   (cl-rfc8628:CREDENTIAL-SOURCE-PATHNAME source))))
       (cl-rfc8628:CREDENTIAL-SOURCE-SAVE source credentials)))))

(declaim
 (ftype (function (api-key-credential-manager) boolean)
  api-key-credential-available-p))

(defun api-key-credential-available-p (manager)
  "Return true when MANAGER has a stored API key.

The probe uses the same request-scope secret accounting as a provider request and
never retains the resulting credential after this call."
  (handler-case
      (progn
        (call-with-credentials
         manager
         (lambda (credentials)
           (declare (ignore credentials))
           t))
        t)
    (credentials-unavailable () nil)))
