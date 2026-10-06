(in-package #:autolith)

;;;; -- GitHub Copilot Authentication --

;;; Reference: https://github.com/earendil-works/pi at
;;; 1cedd32724abfcb0915f76cc61b6827e2c16dbad, inspected after fetching origin.
;;; Device flow: packages/ai/src/auth/oauth/github-copilot.ts.

(defparameter *copilot-oauth-client-id* "Iv1.b507a08c87ecfe98"
  "The public GitHub device-flow client used by Copilot Chat.")

(-> copilot--domain () string)
(defun copilot--domain ()
  "Return the GitHub.com or Enterprise Cloud issuer configured for Copilot."
  (let* ((domain (string-downcase
                  (or (uiop:getenvp "AUTOLITH_COPILOT_DOMAIN") "github.com")))
         (labels (uiop:split-string domain :separator '(#\.)))
         (tenant (first labels)))
    (unless (or (string= domain "github.com")
                (and (= (length labels) 3)
                     (equal (rest labels) '("ghe" "com"))
                     (<= 1 (length tenant) 63)
                     (not (char= (char tenant 0) #\-))
                     (not (char= (char tenant (1- (length tenant))) #\-))
                     (every (lambda (character)
                              (or (find character "abcdefghijklmnopqrstuvwxyz0123456789-")
                                  nil))
                            tenant)))
      (error 'configuration-error
             :message "AUTOLITH_COPILOT_DOMAIN must be github.com or a TENANT.ghe.com Enterprise Cloud hostname."))
    domain))

(-> copilot--headers (string) list)
(defun copilot--headers (token)
  "Use the shared Copilot integration headers with Autolith's user agent."
  (copilot-http-headers token :user-agent (provider-user-agent)))

(-> copilot--get-json (string string &key (:headers list)) json-object)
(defun copilot--get-json (url token &key headers)
  "GET one authenticated JSON object without retaining secret-bearing HTTP errors."
  (handler-case
      (multiple-value-bind (body status)
          (provider-call-with-response-deadline
           30 (lambda ()
                (dexador:get url :headers (append (copilot--headers token) headers)
                                 :force-string t :keep-alive nil
                                 :max-redirects 0 :connect-timeout 10 :read-timeout 30)))
        (unless (and (integerp status) (<= 200 status 299))
          (error 'authentication-error
                 :message (format nil "GitHub Copilot returned HTTP ~A; run autolith auth copilot."
                                  status)))
        (let ((document (json-decode body)))
          (unless (json-object-p document)
            (error 'authentication-error :message "GitHub Copilot returned invalid JSON."))
          document))
    (authentication-error (condition) (error condition))
    (http-request-failed (condition)
      (error 'authentication-error
             :message (format nil "GitHub Copilot returned HTTP ~A; run autolith auth copilot."
                              (response-status condition))))
    (error ()
      (error 'authentication-error
             :message "GitHub Copilot could not be reached or returned invalid JSON."))))

(defclass copilot-credential-manager (credential-manager)
  ((domain :initarg :domain :reader copilot-credential-manager-domain
           :type string :documentation "The GitHub host authorizing this manager."))
  (:documentation "A manager renewing Copilot tokens with a persistent GitHub token."))

(defmethod credential-manager-provider-label ((manager copilot-credential-manager))
  "Name the Copilot account service."
  (declare (ignore manager))
  "GitHub Copilot")

(defmethod credential-manager-login-hint ((manager copilot-credential-manager))
  "Point Copilot credential failures at device login."
  (declare (ignore manager))
  "run autolith auth copilot")

(-> copilot-credential-manager-create (configuration) copilot-credential-manager)
(defun copilot-credential-manager-create (configuration)
  "Create a Copilot manager without retaining any tokens in the image."
  (make-instance 'copilot-credential-manager
                 :domain (copilot--domain)
                 :primary-source
                 (make-instance 'autolith-credential-source
                                :pathname (merge-pathnames "copilot-auth.sexp"
                                                           (config :state-root configuration)))))

(defmethod credential-manager-validate-credentials
    ((manager copilot-credential-manager) (credentials oauth-credentials))
  "Prevent a stored GitHub token from being sent to a different issuer."
  (unless (uiop:string-prefix-p
           (concatenate 'string (copilot-credential-manager-domain manager) ":")
           (oauth-credentials-account-id credentials))
    (error 'authentication-error
           :message "Copilot credentials belong to a different GitHub host; run autolith auth copilot."))
  credentials)

(-> copilot--exchange (copilot-credential-manager string string) oauth-credentials)
(defun copilot--exchange (manager github-token account-id)
  "Exchange GITHUB-TOKEN for a validated Copilot token with an early expiry."
  (let* ((document (copilot--get-json
                    (format nil "https://api.~A/copilot_internal/v2/token"
                            (copilot-credential-manager-domain manager))
                    github-token))
         (token (json-get document "token"))
         (expires-at (json-get document "expires_at")))
    (unless (and (non-empty-string-p token)
                 (integerp expires-at)
                 (> expires-at (- (get-universal-time) 2208988800)))
      (error 'authentication-error
             :message "GitHub did not return a valid Copilot token; check your Copilot subscription."))
    (make-instance 'oauth-credentials
                   :access-token token :refresh-token github-token
                   :account-id account-id
                   ;; The shared manager renews within 300 seconds of expiry.
                   :expires-at (+ expires-at 2208988800)
                   :source-path (credential-source-pathname
                                 (credential-manager-primary-source manager)))))

(defmethod credential-manager-refresh-exchange
    ((manager copilot-credential-manager) (credentials oauth-credentials) refresh-token)
  "Renew the short-lived token without rotating the persistent GitHub credential."
  (credential-manager-validate-credentials manager credentials)
  (values (copilot--exchange manager refresh-token (oauth-credentials-account-id credentials)) t))

(-> copilot--base-url (oauth-credentials) string)
(defun copilot--base-url (credentials)
  "Resolve the trusted API host belonging to CREDENTIALS' pinned issuer."
  (let* ((account (oauth-credentials-account-id credentials))
         (domain (subseq account 0 (position #\: account))))
    (handler-case
        (copilot-base-url (oauth-credentials-access-token credentials) :domain domain)
      (provider-error (condition)
        (error 'authentication-error :message (autolith-error-message condition))))))

;;;; -- Device Login --

(defclass copilot-device-authentication-client (rfc8628-device-authentication-client)
  ()
  (:documentation "GitHub's RFC 8628 device login, followed by a Copilot token exchange."))

(-> copilot--device-request
    (&key (:method keyword) (:url string) (:headers list) (:content string))
    (values string integer t))
(defun copilot--device-request (&key method url headers content)
  "Normalize GitHub's HTTP 200 OAuth errors for the shared RFC 8628 polling loop."
  (multiple-value-bind (body status response-headers)
      (device-authentication-request :method method :url url :headers headers :content content)
    (values body
            (if (and (= status 200) (oauth-error-code body)) 400 status)
            response-headers)))

(-> copilot-device-authentication-client-create () copilot-device-authentication-client)
(defun copilot-device-authentication-client-create ()
  "Create the GitHub device client using the shared polling, timeout, and browser effects."
  (make-instance 'copilot-device-authentication-client
                 :issuer (format nil "https://~A" (copilot--domain))
                 :client-id *copilot-oauth-client-id*
                 :device-code-path "/login/device/code"
                 :token-path "/login/oauth/access_token"
                 :scope "read:user"
                 :request-function #'copilot--device-request
                 :poll-function #'rfc8628-device-authentication-poll-for-tokens))

(defmethod device-authentication-complete
    ((client copilot-device-authentication-client)
     (authorization rfc8628-device-authorization)
     (manager copilot-credential-manager))
  "Poll for the GitHub token and publish only after the Copilot exchange succeeds."
  (call-with-secret-use
   (lambda ()
     (let* ((document (funcall (device-authentication-client-poll-function client)
                               client authorization))
            (github-token (and (json-object-p document) (json-get document "access_token"))))
       (unless (non-empty-string-p github-token)
         (device-authentication-fail :stage ':credentials
                                     :message "GitHub device login omitted the access token."))
       (let* ((domain (copilot-credential-manager-domain manager))
              (user (copilot--get-json (format nil "https://api.~A/user" domain) github-token))
              (id (json-get user "id")))
         (unless (typep id '(integer 1))
           (error 'authentication-error :message "GitHub did not return an account identifier."))
         (let ((credentials (copilot--exchange manager github-token (format nil "~A:~D" domain id))))
           (credential-manager-accept-account manager credentials :allow-change t)
           (credential-source-save (credential-manager-primary-source manager) credentials)
           t))))))
