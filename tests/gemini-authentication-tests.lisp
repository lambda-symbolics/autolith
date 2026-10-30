(in-package #:autolith)

;;;; -- Gemini OAuth Tests --

(defvar *gemini-test-saved-credentials* nil
  "Credentials observed by the Gemini recording source.")

(defclass gemini-test-credential-source (gemini-credential-source)
  ()
  (:documentation "A Gemini credential source that records test writes."))

(defmethod credential-source-save
    ((source gemini-test-credential-source) (credentials oauth-credentials))
  "Record CREDENTIALS without writing SOURCE."
  (declare (ignore source))
  (setf *gemini-test-saved-credentials* credentials))

(-> gemini-test--manager () gemini-credential-manager)
(defun gemini-test--manager ()
  "Return an isolated recording Gemini credential manager."
  (make-instance 'gemini-credential-manager
                 :primary-source
                 (make-instance 'gemini-test-credential-source
                                :pathname #P"/tmp/gemini-auth.sexp")))

(-> run-gemini-authentication-tests () null)
(defun run-gemini-authentication-tests ()
  "Test PKCE, authorization, exchange, redaction, expiry, and login publication."
  (let ((url (gemini-oauth-authorization-url
              :redirect-uri           "http://127.0.0.1:3456/oauth2callback"
              :state                  "state-test"
              :code-challenge         "challenge-test"
              :client-id              "client-test"
              :authorization-endpoint "https://accounts.test/auth")))
    (dolist (fragment '("client_id=client-test"
                        "code_challenge=challenge-test"
                        "code_challenge_method=S256"
                        "access_type=offline"
                        "state=state-test"))
      (test-assert (search fragment url)
                   "Gemini authorization URL contains required installed-app fields")))
  (let* ((manager         (gemini-test--manager))
         (started-at      (get-universal-time))
         (request-content nil)
         (credentials
           (gemini-oauth-exchange-code
            manager
            :code           "code-secret"
            :verifier       "verifier-secret"
            :redirect-uri   "http://127.0.0.1:1234/oauth2callback"
            :client-id      "public-client"
            :client-secret  nil
            :token-endpoint "https://oauth.test/token"
            :request-function
            (lambda (&key url content)
              (test-assert (string= url "https://oauth.test/token")
                           "Gemini exchange uses the configured endpoint")
              (setf request-content content)
              (values (json-encode
                       (json-object "access_token" "access-test"
                                    "refresh_token" "refresh-test"
                                    "expires_in" 3600))
                      200 nil)))))
    (test-assert (and (search "code_verifier=verifier-secret" request-content)
                      (not (search "client_secret" request-content)))
                 "Gemini exchange uses PKCE public-client fields without a secret")
    (test-assert (string= (oauth-credentials-access-token credentials)
                          "access-test")
                 "Gemini exchange returns the access token")
    (test-assert (>= (oauth-credentials-expires-at credentials)
                     (+ started-at 3599))
                 "Gemini exchange records expires_in as universal time"))
  (let ((request-content nil))
    (gemini-oauth-exchange-code
     (gemini-test--manager)
     :code           "code-secret"
     :verifier       "verifier-secret"
     :redirect-uri   "http://127.0.0.1:1234/oauth2callback"
     :client-id      "confidential-client"
     :client-secret  "client-secret"
     :token-endpoint "https://oauth.test/token"
     :request-function
     (lambda (&key url content)
       (declare (ignore url))
       (setf request-content content)
       (values (json-encode
                (json-object "access_token" "access-secret-client"
                             "refresh_token" "refresh-secret-client"
                             "expires_in" 3600))
               200
               nil)))
    (test-assert
     (and (search "client_id=confidential-client" request-content)
          (search "client_secret=client-secret" request-content))
     "Gemini exchange includes the configured client credentials"))
  (let ((condition nil)
        (secret    "refresh-do-not-leak"))
    (handler-case
        (gemini-oauth--token-document
         (lambda (&key url content)
           (declare (ignore url content))
           (values (json-encode
                    (json-object "error" "invalid_grant"
                                 "error_description"
                                 (format nil "bad ~A" secret)))
                   400 nil))
         "https://oauth.test/token"
         :parameters (list (cons "refresh_token" secret))
         :stage      ':refresh)
      (gemini-oauth-error (caught)
        (setf condition caught)))
    (test-assert (and condition
                      (eq (gemini-oauth-error-stage condition) ':refresh)
                      (= (gemini-oauth-error-status condition) 400))
                 "Gemini token failures use the typed condition")
    (test-assert (not (test-object-contains-string-p condition secret))
                 "Gemini token failure conditions redact credential values"))
  (let* ((manager                         (gemini-test--manager))
         (*gemini-test-saved-credentials* nil)
         (output                          (make-string-output-stream))
         (browser-url                     nil))
    (gemini-oauth-login
     manager
     :stream           output
     :browser-function (lambda (url) (setf browser-url url) nil)
     :callback-function
     (lambda (listener expected-state &key timeout)
        (let* ((query  (quri:url-decode-params (quri:uri-query (quri:uri browser-url))))
               (uri    (quri:uri (rest (assoc "redirect_uri" query :test #'string=))))
               (port   (quri:uri-port uri))
               (socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
               (stream (usocket:socket-stream socket)))
          (test-assert (and (plusp port) (string= (quri:uri-host uri) "127.0.0.1")
                            (string= (quri:uri-path uri) "/oauth2callback"))
                       "Gemini binds an ephemeral IPv4 loopback callback")
          (unwind-protect
               (progn
                 (write-sequence
                  (babel:string-to-octets
                   (format nil "GET ~A?code=code-test&state=~A HTTP/1.1~C~C"
                           (quri:uri-path uri) expected-state #\Return #\Newline)
                   :encoding ':utf-8)
                  stream)
                 (finish-output stream))
            (usocket:socket-close socket)))
        (gemini-oauth-await-loopback listener expected-state :timeout (min timeout 5)))
     :request-function
     (lambda (&key url content)
       (declare (ignore url content))
       (values (json-encode
                (json-object "access_token" "access-login"
                             "refresh_token" "refresh-login"
                             "expires_in" 3600))
               200
               nil)))
    (test-assert
     (and browser-url
          (search "127.0.0.1" browser-url)
          (search "code_challenge_method=S256" browser-url)
          (search "access_type=offline" browser-url))
     "Gemini login uses an ephemeral loopback redirect and PKCE")
    (test-assert (search browser-url (get-output-stream-string output))
                 "Gemini displays the authorization URL for manual browser launch")
    (test-assert
     (and *gemini-test-saved-credentials*
          (string= (oauth-credentials-access-token
                    *gemini-test-saved-credentials*)
                   "access-login"))
     "Gemini login publishes credentials through the credential manager"))
  (with-test-configuration (configuration)
    (with-test-environment (("AUTOLITH_GEMINI_OAUTH_CLIENT_ID" "environment-client")
                            ("AUTOLITH_GEMINI_OAUTH_CLIENT_SECRET" "environment-secret"))
      (let ((manager (gemini-credential-manager-create configuration)))
        (test-call-with-function-replacements
         (list
          (list 'cl-rfc8252:browser-authentication-login
                (lambda (client received &rest options)
                  (declare (ignore client options))
                  (test-assert (eq manager received) "Gemini login uses the agent manager")
                  (test-assert
                   (equal "environment-client" (autolith-supervisor::gemini-oauth-client-id))
                   "Gemini login uses the manager's environment client ID")
                  (test-assert
                   (equal "environment-secret" (autolith-supervisor::gemini-oauth-client-secret))
                   "Gemini login uses the manager's environment client secret")
                  (make-instance 'oauth-credentials
                                 :access-token "fixture-token" :refresh-token nil :id-token nil
                                 :expires-at nil :account-id nil
                                 :source-path (configuration-gemini-auth-path configuration)))))
         (lambda () (gemini-oauth-login manager :open-browser-p nil))))))
  nil)
