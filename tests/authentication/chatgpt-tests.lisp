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
  (let* ((manager  (chatgpt-test--manager))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token  "old-access"
                          :refresh-token "old-refresh"
                          :id-token      nil
                          :account-id    "account-refresh-deadline"
                          :expires-at    nil
                          :source-path   #P"/tmp/chatgpt-auth.sexp"))
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
         (values (json-encode
                  (json-object "access_token" "new-access"
                               "refresh_token" "new-refresh"))
                 200
                 nil))))
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
  (rest (assoc name (quri:url-decode-params (quri:uri-query (quri:uri target)))
               :test #'string=)))


;;;; -- ChatGPT OAuth Tests --

(-> run-chatgpt-authentication-tests () null)
(defun run-chatgpt-authentication-tests ()
  "Test PKCE, authorization, callback validation, exchange, and command routing."
  (chatgpt-test--refresh-response-deadline)
  (let* ((redirect-uri "http://localhost:1455/auth/callback")
         (url
           (chatgpt-oauth-authorization-url
            :redirect-uri   redirect-uri
            :state          "state-test"
            :code-challenge "challenge-test"
            :issuer         "https://issuer.test/"
            :client-id      "client-test"
            :originator     "autolith-test")))
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
  (let* ((manager     (chatgpt-test--manager))
         (id-token    (test-account-jwt "account-test"))
         (request-url nil)
         (credentials
           (chatgpt-oauth-exchange-code
            manager
            :code           "code-secret"
            :verifier       "verifier-secret"
            :redirect-uri   "http://localhost:1455/auth/callback"
            :client-id      "client-test"
            :token-endpoint "https://issuer.test/oauth/token"
            :request-function
            (lambda (&key url content)
              (declare (ignore content))
              (setf request-url url)
              (values
               (json-encode
                (json-object "id_token" id-token
                             "access_token" "access-test"
                             "refresh_token" "refresh-test"))
               200
               nil)))))
    (test-assert (string= request-url "https://issuer.test/oauth/token")
                 "ChatGPT exchange uses the configured token endpoint")
    (test-assert
     (and (string= (oauth-credentials-access-token credentials) "access-test")
          (string= (oauth-credentials-refresh-token credentials) "refresh-test")
          (string= (oauth-credentials-account-id credentials) "account-test"))
     "ChatGPT exchange returns renewable account credentials"))
  (let ((condition nil)
        (secret    "verifier-do-not-leak"))
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
         :parameters (list (cons "code_verifier" secret))
         :stage      ':exchange)
      (chatgpt-oauth-error (caught)
        (setf condition caught)))
    (test-assert
     (and (typep condition 'authentication-error)
          (eq (browser-authentication-error-stage condition) ':exchange)
          (= (browser-authentication-error-status condition) 400)
          (string= (browser-authentication-error-code condition) "invalid_grant")
          (not (test-object-contains-string-p condition secret)))
     "ChatGPT token failures use typed redacted diagnostics"))
  (let* ((manager                          (chatgpt-test--manager))
         (id-token                         (test-account-jwt "account-login"))
         (*chatgpt-test-saved-credentials* nil)
         (output                           (make-string-output-stream))
         (browser-url                      nil)
         (secret-guard-observed-p          nil))
    (chatgpt-oauth-login
     manager
     :stream           output
     :browser-function (lambda (url) (setf browser-url url) nil)
     :callback-function
      (lambda (listener expected-state &key timeout)
        (let* ((uri    (quri:uri (chatgpt-test--parameter browser-url "redirect_uri")))
               (port   (quri:uri-port uri))
               (socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
               (stream (usocket:socket-stream socket)))
          (unwind-protect
               (progn
                 (write-sequence
                  (babel:string-to-octets
                   (format nil "GET ~A?code=code-test&state=~A.onboarding_entrypoint%3Dlife_sciences HTTP/1.1~C~C"
                           (quri:uri-path uri) expected-state #\Return #\Newline)
                   :encoding ':utf-8)
                  stream)
                 (finish-output stream))
            (usocket:socket-close socket)))
        (setf secret-guard-observed-p (secret-use-active-p))
        (chatgpt-oauth-await-loopback listener expected-state :timeout (min timeout 5)))
     :request-function
     (lambda (&key url content)
       (declare (ignore url content))
       (values
        (json-encode
         (json-object "id_token" id-token
                      "access_token" "access-login"
                      "refresh_token" "refresh-login"))
        200
        nil)))
    (let ((text (get-output-stream-string output)))
      (test-assert
       (and browser-url
            (let ((redirect (quri:uri (chatgpt-test--parameter browser-url "redirect_uri"))))
              (and (string= (quri:uri-host redirect) "localhost")
                   (member (quri:uri-port redirect) '(1455 1457))
                   (string= (quri:uri-path redirect) "/auth/callback")))
            (search "code_challenge_method=S256" browser-url)
            (search browser-url text))
       "ChatGPT login uses the fixed localhost redirect and PKCE"))
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
         (output              (make-string-output-stream))
         (browser-setting     nil)
         (device-setting      nil)
         (browser-login-count 0)
         (device-login-count  0)
         (browser-message     nil)
         (device-message      nil))
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
              provider ':device-code :stream output :open-browser-p nil))))
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
            provider ':invalid :stream output :open-browser-p nil)
           nil)
       (authentication-error ()
         t))
     "The ChatGPT auth command rejects unknown authentication methods"))
  nil)
