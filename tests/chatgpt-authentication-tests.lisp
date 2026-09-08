(in-package #:autolith)

;;;; -- ChatGPT OAuth Test Support --

(defvar *chatgpt-test-saved-credentials* nil
  "Credentials observed by the ChatGPT recording source.")

(defclass chatgpt-test-credential-source (autolith-credential-source)
  ()
  (:documentation "A ChatGPT credential source that records test writes."))

(defmethod credential-source-save
    ((source chatgpt-test-credential-source) (credentials oauth-credentials))
  "Record CREDENTIALS without writing SOURCE."
  (declare (ignore source))
  (setf *chatgpt-test-saved-credentials* credentials))

(-> chatgpt-test--manager () chatgpt-credential-manager)
(defun chatgpt-test--manager ()
  "Return an isolated recording ChatGPT credential manager."
  (make-instance 'chatgpt-credential-manager
                 :primary-source
                 (make-instance 'chatgpt-test-credential-source
                                :pathname #P"/tmp/chatgpt-auth.sexp")))

(-> chatgpt-test--refresh-response-deadline () null)
(defun chatgpt-test--refresh-response-deadline ()
  "Test ChatGPT refresh uses its deadline and normalizes deadline failure."
  (let* ((manager (chatgpt-test--manager))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "old-access"
                          :refresh-token "old-refresh"
                          :id-token nil
                          :account-id "account-refresh-deadline"
                          :expires-at nil
                          :source-path #P"/tmp/chatgpt-auth.sexp"))
         (deadline nil))
    (test-call-with-function-replacements
     (list
      (list
       'provider-call-with-response-deadline
       (lambda (seconds function)
         (setf deadline seconds)
         (funcall function)))
      (list
       'dexador:post
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (json-encode
          (json-object "access_token" "new-access"
                       "refresh_token" "new-refresh")))))
     (lambda ()
       (multiple-value-bind (refreshed publish-p)
           (credential-manager-refresh-exchange
            manager credentials "old-refresh")
         (test-assert
          (and publish-p
               (string= (oauth-credentials-access-token refreshed) "new-access")
               (= deadline 60))
          "ChatGPT token refresh wraps the complete response in a 60-second deadline"))))
    (test-assert
     (handler-case
         (test-call-with-function-replacements
          (list
           (list
            'provider-call-with-response-deadline
            (lambda (seconds function)
              (declare (ignore seconds function))
              (error 'sb-sys:deadline-timeout))))
          (lambda ()
            (credential-manager-refresh-exchange
             manager credentials "old-refresh")))
       (token-refresh-failed ()
         t))
     "ChatGPT refresh deadlines become typed token refresh failures"))
  nil)

(-> chatgpt-test--parameter (string string) (option string))
(defun chatgpt-test--parameter (target name)
  "Return NAME from TARGET's decoded query parameters."
  (rest (assoc name (oauth--query-parameters target) :test #'string=)))


;;;; -- ChatGPT OAuth Tests --

(-> run-chatgpt-authentication-tests () null)
(defun run-chatgpt-authentication-tests ()
  "Test PKCE, authorization, callback validation, exchange, and command routing."
  (chatgpt-test--refresh-response-deadline)
  (multiple-value-bind (verifier challenge)
      (chatgpt-oauth-create-pkce)
    (test-assert (= (length verifier) 86)
                 "ChatGPT PKCE emits the current 512-bit verifier")
    (test-assert (= (length challenge) 43)
                 "ChatGPT PKCE emits an S256 challenge")
    (test-assert (and (not (find #\= verifier))
                      (not (find #\= challenge)))
                 "ChatGPT PKCE values are unpadded Base64url"))
  (let* ((redirect-uri "http://localhost:1455/auth/callback")
         (url
           (chatgpt-oauth-authorization-url
            :redirect-uri redirect-uri
            :state "state-test"
            :code-challenge "challenge-test"
            :issuer "https://issuer.test/"
            :client-id "client-test"
            :originator "autolith-test")))
    (test-assert (string= (subseq url 0 (position #\? url))
                          "https://issuer.test/oauth/authorize")
                 "ChatGPT authorization uses the configured issuer")
    (dolist (case
             `(("response_type" . "code")
               ("client_id" . "client-test")
               ("redirect_uri" . ,redirect-uri)
               ("scope" . "openid profile email offline_access api.connectors.read api.connectors.invoke")
               ("code_challenge" . "challenge-test")
               ("code_challenge_method" . "S256")
               ("id_token_add_organizations" . "true")
               ("codex_cli_simplified_flow" . "true")
               ("state" . "state-test")
               ("originator" . "autolith-test")))
      (test-assert
       (string= (chatgpt-test--parameter url (first case)) (rest case))
       (format nil "ChatGPT authorization includes ~A" (first case)))))
  (test-assert
   (string=
    (chatgpt-oauth--callback-code
     "/auth/callback?code=code-test&state=state-test"
     "state-test")
    "code-test")
   "ChatGPT callback validation returns the authorization code")
  (test-assert
   (string=
    (chatgpt-oauth--callback-code
     "/auth/callback?code=code-test&state=state-test.onboarding_entrypoint%3Dlife_sciences"
     "state-test")
    "code-test")
   "ChatGPT callback validation accepts the supported onboarding state suffix")
  (let ((condition nil)
        (state "state-secret")
        (code "code-secret"))
    (handler-case
        (chatgpt-oauth--callback-code
         (format nil "/auth/callback?code=~A&state=wrong" code)
         state)
      (chatgpt-oauth-error (caught)
        (setf condition caught)))
    (test-assert
     (and condition
          (eq (chatgpt-oauth-error-stage condition) ':callback)
          (not (test-object-contains-string-p condition state))
          (not (test-object-contains-string-p condition code)))
     "ChatGPT callback failures reject mismatched state without retaining secrets")
    (test-assert (typep condition 'chatgpt-oauth-state-mismatch)
                 "ChatGPT state mismatches use their dedicated condition"))
  (let* ((secret "secret-that-crosses-the-original-boundary")
         (value
           (concatenate 'string
                        (make-string 230 :initial-element #\x)
                        secret
                        " trailing text"))
         (safe-value (chatgpt-oauth--redacted-value value (list secret))))
    (test-assert
     (and (search "[OAUTH VALUE REDACTED]" safe-value)
          (not (search (subseq secret 0 16) safe-value)))
     "ChatGPT OAuth errors redact complete secrets before bounding output"))
  (test-assert
   (null
    (chatgpt-oauth--callback-code-or-continue
     "/auth/callback?code=ignored&state=wrong"
     "state-test"))
   "ChatGPT listener handling ignores unrelated local callbacks")
  (let ((wait-count 0))
    (test-assert
     (null
      (chatgpt-oauth--read-request-line
       (make-string-input-stream "")
       -1
       201/2
       :clock-function (lambda () 1/2)
       :wait-function
       (lambda (file-descriptor direction timeout)
         (declare (ignore file-descriptor direction timeout))
         (incf wait-count)
         nil)))
     "ChatGPT callback request reading stops when its local read wait expires")
    (test-assert (= wait-count 1)
                 "ChatGPT callback request reading performs one bounded wait"))
  (let ((headers (make-hash-table :test #'equal)))
    (multiple-value-bind (body status returned-headers)
        (test-call-with-function-replacements
         (list
          (list
           'dexador:post
           (lambda (&rest arguments)
             (declare (ignore arguments))
             (values "{}" 200 headers nil nil))))
         (lambda ()
           (chatgpt-oauth--request
            :url "https://issuer.test/oauth/token"
            :content "grant_type=authorization_code")))
      (test-assert
       (and (string= body "{}")
            (= status 200)
            (eq returned-headers headers))
       "ChatGPT token transport accepts Dexador hash-table response headers")))
  (let* ((manager (chatgpt-test--manager))
         (id-token (test-account-jwt "account-test"))
         (request-url nil)
         (request-content nil)
         (credentials
           (chatgpt-oauth-exchange-code
            manager
            "code-secret"
            "verifier-secret"
            "http://localhost:1455/auth/callback"
            :client-id "client-test"
            :token-endpoint "https://issuer.test/oauth/token"
            :request-function
            (lambda (&key url content)
              (setf request-url url
                    request-content content)
              (values
               (json-encode
                (json-object "id_token" id-token
                             "access_token" "access-test"
                             "refresh_token" "refresh-test"))
               200
               nil)))))
    (test-assert (string= request-url "https://issuer.test/oauth/token")
                 "ChatGPT exchange uses the configured token endpoint")
    (dolist (case
             '(("grant_type" . "authorization_code")
               ("code" . "code-secret")
               ("redirect_uri" . "http://localhost:1455/auth/callback")
               ("client_id" . "client-test")
               ("code_verifier" . "verifier-secret")))
      (test-assert
       (string= (chatgpt-test--parameter
                 (format nil "?~A" request-content)
                 (first case))
                (rest case))
       (format nil "ChatGPT exchange includes ~A" (first case))))
    (test-assert
     (and (string= (oauth-credentials-access-token credentials) "access-test")
          (string= (oauth-credentials-refresh-token credentials) "refresh-test")
          (string= (oauth-credentials-account-id credentials) "account-test"))
     "ChatGPT exchange returns renewable account credentials"))
  (let ((condition nil)
        (secret "verifier-do-not-leak"))
    (handler-case
        (chatgpt-oauth--token-document
         (lambda (&key url content)
           (declare (ignore url content))
           (values
            (json-encode
             (json-object
              "error"
              (json-object "code" "invalid_grant"
                           "message" (format nil "bad ~A" secret))))
            400
            nil))
         "https://issuer.test/oauth/token"
         (list (cons "code_verifier" secret))
         ':exchange)
      (chatgpt-oauth-error (caught)
        (setf condition caught)))
    (test-assert
     (and condition
          (eq (chatgpt-oauth-error-stage condition) ':exchange)
          (= (chatgpt-oauth-error-status condition) 400)
          (string= (chatgpt-oauth-error-code condition) "invalid_grant")
          (not (test-object-contains-string-p condition secret)))
     "ChatGPT token failures use typed redacted diagnostics"))
  (let* ((manager (chatgpt-test--manager))
         (id-token (test-account-jwt "account-login"))
         (*chatgpt-test-saved-credentials* nil)
         (output (make-string-output-stream))
         (browser-url nil)
         (secret-guard-observed-p nil))
    (test-call-with-function-replacements
     (list
      (list 'chatgpt-oauth-loopback-open
            (lambda ()
              (values ':listener "http://localhost:1455/auth/callback")))
      (list 'chatgpt-oauth-create-pkce
            (lambda () (values "verifier-test" "challenge-test")))
      (list 'chatgpt-oauth--state
            (lambda () "state-test")))
     (lambda ()
       (chatgpt-oauth-login
        manager
        :stream output
        :browser-function (lambda (url) (setf browser-url url) nil)
        :callback-function
        (lambda (listener state &key timeout)
          (test-assert (eq listener ':listener)
                       "ChatGPT login waits on its loopback listener")
          (test-assert (and (string= state "state-test") (= timeout 900))
                       "ChatGPT login passes state and timeout to the callback")
          (setf secret-guard-observed-p (secret-use-active-p))
          "code-test")
        :request-function
        (lambda (&key url content)
          (declare (ignore url content))
          (values
           (json-encode
            (json-object "id_token" id-token
                         "access_token" "access-login"
                         "refresh_token" "refresh-login"))
           200
           nil)))))
    (let ((text (get-output-stream-string output)))
      (test-assert (and browser-url
                        (search "http%3A%2F%2Flocalhost%3A1455%2Fauth%2Fcallback"
                                browser-url)
                        (search "Could not open a browser" text))
                   "ChatGPT login exposes the browser URL and manual fallback"))
    (test-assert secret-guard-observed-p
                 "ChatGPT login keeps transient OAuth data in secret scope")
    (test-assert
     (and *chatgpt-test-saved-credentials*
          (string= (oauth-credentials-access-token
                    *chatgpt-test-saved-credentials*)
                   "access-login"))
     "ChatGPT login publishes credentials through the credential manager"))
  (let* ((provider
           (provider-authentication-provider (test-configuration) "chatgpt"))
         (output (make-string-output-stream))
         (browser-setting nil)
         (device-setting nil)
         (browser-login-count 0)
         (device-login-count 0)
         (browser-message nil)
         (device-message nil))
    (test-call-with-function-replacements
     (list
      (list 'chatgpt-oauth-login
            (lambda (manager &key stream open-browser-p)
              (declare (ignore manager stream))
              (incf browser-login-count)
              (setf browser-setting open-browser-p)
              nil))
      (list 'device-authentication-login
            (lambda (client manager &key stream open-browser-p)
              (declare (ignore client manager stream))
              (incf device-login-count)
              (setf device-setting open-browser-p)
              t)))
     (lambda ()
       (setf browser-message
             (provider-authenticate-with-method
              provider nil :stream output :open-browser-p nil)
             device-message
             (provider-authenticate-with-method
              provider "device" :stream output :open-browser-p nil))))
    (test-assert
     (and (= browser-login-count 1)
          (= device-login-count 1)
          (null browser-setting)
          (null device-setting)
          (string= browser-message
                   "ChatGPT authentication was saved by Autolith.")
          (string= device-message browser-message))
     "The ChatGPT auth command offers browser and device OAuth")
    (test-assert
     (handler-case
         (progn
           (provider-authenticate-with-method
            provider "invalid" :stream output :open-browser-p nil)
           nil)
       (authentication-error ()
         t))
     "The ChatGPT auth command rejects unknown authentication methods"))
  nil)