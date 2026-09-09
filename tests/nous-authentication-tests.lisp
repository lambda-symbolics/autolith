(in-package #:autolith)

;;;; -- Nous Authentication Test Support --

(-> nous-test--base64url (string) string)
(defun nous-test--base64url (source)
  "Return SOURCE encoded as unpadded RFC 4648 Base64url text."
  (string-right-trim
   '(#\=)
   (substitute #\_
               #\/
               (substitute #\-
                           #\+
                           (cl-base64:string-to-base64-string source)))))

(-> nous-test--jwt
    (&key (:subject string) (:scope t) (:expiration (option integer)))
    string)
(defun nous-test--jwt
    (&key (subject "nous-user-1")
          (scope *nous-oauth-scope*)
          expiration)
  "Return an unsigned Nous test JWT carrying SUBJECT, SCOPE, and EXPIRATION."
  (let ((payload (json-object "sub" subject "scope" scope)))
    (when expiration
      (setf (gethash "exp" payload) expiration))
    (format nil "~A.~A.signature"
            (nous-test--base64url "{\"alg\":\"none\"}")
            (nous-test--base64url (json-encode payload)))))

(-> nous-authentication-test--credentials
    (pathname &key
              (:subject string)
              (:scope t)
              (:access-token (option string))
              (:refresh-token string)
              (:account-id (option string)))
    oauth-credentials)
(defun nous-authentication-test--credentials
    (pathname &key
                (subject "nous-user-1")
                (scope *nous-oauth-scope*)
                access-token
                (refresh-token "nous-refresh-1")
                account-id)
  "Return renewable Nous credentials rooted at PATHNAME."
  (make-instance
   'oauth-credentials
   :access-token (or access-token
                     (nous-test--jwt :subject subject :scope scope))
   :refresh-token refresh-token
   :id-token nil
   :account-id (or account-id subject)
   :expires-at nil
   :source-path pathname))

(-> nous-authentication-test--header-value (string list) (option string))
(defun nous-authentication-test--header-value (name headers)
  "Return NAME's case-insensitive value from HEADERS."
  (rest (assoc name headers :test #'string-equal)))


;;;; -- Nous Authentication Tests --

(-> nous-authentication-test--manager-loading () null)
(defun nous-authentication-test--manager-loading ()
  "Test the dedicated Nous store, scope validation, and account continuity."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (manager (nous-credential-manager-create configuration))
         (source (credential-manager-primary-source manager))
         (pathname (credential-source-pathname source)))
    (unwind-protect
         (progn
           (test-assert
            (null (credential-manager-bootstrap-source manager))
            "the Nous manager has no bootstrap credential source")
           (test-assert
            (handler-case
                (progn (credential-manager-load manager) nil)
              (credentials-unavailable (condition)
                (and (test-object-contains-string-p condition "auth nous")
                     (not (test-object-contains-string-p
                           condition
                           "nous-refresh-secret")))))
            "an empty Nous store points at browser authentication")
           (credential-source-save
            source
            (nous-authentication-test--credentials
             pathname
             :refresh-token "nous-refresh-secret"))
           (let ((loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "nous-user-1")
              "the Nous manager loads a scoped access JWT"))
           (credential-source-save
            source
            (nous-authentication-test--credentials
             pathname
             :scope "profile"
             :refresh-token "missing-scope-secret"))
           (test-assert
            (handler-case
                (progn (credential-manager-load manager) nil)
              (credentials-unavailable (condition)
                (and (test-object-contains-string-p
                      condition
                      "cannot invoke inference")
                     (not (test-object-contains-string-p
                           condition
                           "missing-scope-secret")))))
            "stored Nous credentials require the inference scope without echoing secrets")
           (credential-source-save
            source
            (nous-authentication-test--credentials
             pathname
             :account-id "different-account"
             :refresh-token "account-mismatch-secret"))
           (test-assert
            (handler-case
                (progn (credential-manager-load
                        (nous-credential-manager-create configuration))
                       nil)
              (credentials-unavailable (condition)
                (not (test-object-contains-string-p
                      condition
                      "account-mismatch-secret"))))
            "the stored account identity must match the access JWT subject"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> nous-authentication-test--refresh-validation () null)
(defun nous-authentication-test--refresh-validation ()
  "Test rotated refresh response fields, scopes, and account continuity."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (manager (nous-credential-manager-create configuration))
         (pathname
           (credential-source-pathname
            (credential-manager-primary-source manager)))
         (old
           (nous-authentication-test--credentials
            pathname
            :refresh-token "old-refresh")))
    (unwind-protect
         (progn
           (let ((refreshed
                   (nous-refresh-response-credentials
                    manager
                    old
                    (json-encode
                     (json-object
                      "access_token" (nous-test--jwt)
                      "refresh_token" "new-refresh"
                      "expires_in" 900)))))
             (test-assert
              (string= (oauth-credentials-refresh-token refreshed)
                       "new-refresh")
              "a Nous refresh response rotates the single-use refresh token")
             (test-assert
              (let ((expires-at (oauth-credentials-expires-at refreshed)))
                (and expires-at
                     (<= 890 (- expires-at (get-universal-time)) 910)))
              "a Nous refresh response maps expires_in to an expiration"))
           (dolist (body
                    (list
                     (json-encode
                      (json-object
                       "access_token" (nous-test--jwt :scope "profile")
                       "refresh_token" "new-refresh"))
                     (json-encode
                      (json-object
                       "access_token" (nous-test--jwt)
                       "refresh_token" "old-refresh"))
                     (json-encode
                      (json-object
                       "access_token" (nous-test--jwt :subject "nous-user-2")
                       "refresh_token" "new-refresh"))))
             (test-assert
              (handler-case
                  (progn
                    (nous-refresh-response-credentials manager old body)
                    nil)
                (token-refresh-failed ()
                  t))
              "a Nous refresh response rejects invalid scope, rotation, or account data")))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> nous-authentication-test--serialized-refresh () null)
(defun nous-authentication-test--serialized-refresh ()
  "Test one network rotation across concurrent managers sharing a credential store."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-nous-auth-path configuration))
         (request-count 0)
         (request-lock (make-lock "Nous refresh request test"))
         (captured-request nil)
         (new-access (nous-test--jwt))
         (old
           (nous-authentication-test--credentials
            pathname
            :access-token (nous-test--jwt)
            :refresh-token "single-use-refresh")))
    (labels ((request (&key method url headers content)
               (with-lock-held (request-lock)
                 (incf request-count)
                 (setf captured-request
                       (list :method method
                             :url url
                             :headers headers
                             :content content)))
               (sleep 0.05)
               (values
                (json-encode
                 (json-object
                  "access_token" new-access
                  "refresh_token" "rotated-refresh"
                  "expires_in" 900))
                200
                nil)))
      (let* ((first-manager
               (nous-credential-manager-create
                configuration
                :refresh-request-function #'request))
             (second-manager
               (nous-credential-manager-create
                configuration
                :refresh-request-function #'request))
             (source (credential-manager-primary-source first-manager))
             (first-result nil)
             (second-result nil)
             (first-condition nil)
             (second-condition nil))
        (unwind-protect
             (progn
               (credential-source-save source old)
               (let ((first-thread
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf first-result
                                    (credential-manager-refresh
                                     first-manager
                                     old))
                            (error (condition)
                              (setf first-condition condition))))))
                     (second-thread
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf second-result
                                    (credential-manager-refresh
                                     second-manager
                                     old))
                            (error (condition)
                              (setf second-condition condition)))))))
                 (join-thread first-thread)
                 (join-thread second-thread))
               (test-assert
                (and (null first-condition) (null second-condition))
                "concurrent Nous refreshes both complete successfully")
               (test-assert
                (= request-count 1)
                "the shared Nous lock spends a single-use refresh token once")
               (test-assert
                (and first-result second-result
                     (string= (oauth-credentials-access-token first-result)
                              new-access)
                     (string= (oauth-credentials-access-token second-result)
                              new-access))
                "concurrent managers converge on the latest access token")
               (let ((saved (credential-source-load source)))
                 (test-assert
                  (and saved
                       (string= (oauth-credentials-refresh-token saved)
                                "rotated-refresh"))
                  "the rotated Nous refresh token is published before releasing the lock"))
               (test-assert
                (and (eq (getf captured-request :method) ':post)
                      (uiop:string-suffix-p
                       (getf captured-request :url)
                       "/api/oauth/token")
                     (string=
                      (nous-authentication-test--header-value
                       "x-nous-refresh-token"
                       (getf captured-request :headers))
                      "single-use-refresh")
                     (search "grant_type=refresh_token"
                             (getf captured-request :content))
                     (search "client_id=hermes-cli"
                             (getf captured-request :content))
                     (not (search "single-use-refresh"
                                  (getf captured-request :content))))
                "Nous refresh uses the header token and form-encoded public fields"))
          (platform-delete-directory-tree *platform* root
                                          :validate t
                                          :if-does-not-exist ':ignore)))))
  nil)

(-> nous-authentication-test--refresh-redaction () null)
(defun nous-authentication-test--refresh-redaction ()
  "Test that refresh failures cannot retain echoed Nous credential material."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-nous-auth-path configuration))
         (old
           (nous-authentication-test--credentials
            pathname
            :access-token (nous-test--jwt)
            :refresh-token "echoed-refresh-secret"))
         (manager
           (nous-credential-manager-create
            configuration
            :refresh-request-function
            (lambda (&key method url headers content)
              (declare (ignore method url headers content))
              (values
               (json-encode
                (json-object
                 "error"
                 "refresh_token_reused:echoed-refresh-secret"))
               400
               nil)))))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source manager)
            old)
           (test-assert
            (handler-case
                (progn (credential-manager-refresh manager old) nil)
              (token-refresh-failed (condition)
                (and (not (test-object-contains-string-p
                           condition
                           "echoed-refresh-secret"))
                     (test-object-contains-string-p condition "auth nous"))))
            "Nous refresh failures redact echoed tokens and point at reauthentication"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> run-nous-authentication-tests () boolean)
(defun run-nous-authentication-tests ()
  "Run the offline Nous OAuth credential-manager tests."
  (nous-authentication-test--manager-loading)
  (nous-authentication-test--refresh-validation)
  (nous-authentication-test--serialized-refresh)
  (nous-authentication-test--refresh-redaction)
  t)
