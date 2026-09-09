(in-package #:autolith)

;;;; -- Grok Authentication Tests --

(-> test-write-grok-auth
    (pathname &key (:auth-mode string)
                   (:user-id string)
                   (:access-token string)
                   (:scope (option string)))
    null)
(defun test-write-grok-auth (pathname &key auth-mode user-id access-token scope)
  "Write a synthetic Grok Build credential document to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string
     (json-encode
      (json-object
       (or scope (grok-auth-json-scope))
       (json-object
        "key" access-token
        "auth_mode" auth-mode
        "user_id" user-id
        "refresh_token" "must-not-be-imported"
        "expires_at" "2199-01-01T00:00:00Z")))
     stream))
  nil)

(-> test-subject-jwt (string) string)
(defun test-subject-jwt (subject)
  "Return a synthetic unsigned JWT carrying SUBJECT."
  (format nil
          "e30.~A.signature"
          (cl-base64:string-to-base64-string
           (json-encode (json-object "sub" subject))
           :uri t)))

(-> grok-authentication-tests--test-rfc3339 () null)
(defun grok-authentication-tests--test-rfc3339 ()
  "Test RFC 3339 parsing over valid and malformed timestamps."
  (loop for (text . expected)
          in (list
              (cons "1900-01-01T00:00:00Z" 0)
              (cons "1900-01-01t00:00:00z" 0)
              (cons "1900-01-01 00:00:01Z" 1)
              (cons "1900-01-01T00:00:30.999Z" 30)
              (cons "1900-01-01T02:00:00+02:00" 0)
              (cons "2000-01-01T00:00:00-02:00"
                    (encode-universal-time 0 0 2 1 1 2000 0)))
        do (test-assert
            (eql (rfc3339->universal-time text) expected)
            "RFC 3339 parsing yields the expected universal time"))
  (dolist (text (list "not-a-timestamp"
                      "1900-01-01T00:00:00"
                      "1900-01-01T00:00:00+0200"
                      "1900-01-01T00:00:00.Z"
                      "1900-01-01T00:00:00Zjunk"
                      ""))
    (test-assert
     (null (rfc3339->universal-time text))
     "RFC 3339 parsing rejects a malformed timestamp"))
  nil)

(-> grok-authentication-tests--test-bootstrap (configuration) null)
(defun grok-authentication-tests--test-bootstrap (configuration)
  "Test one-way Grok Build bootstrap import and its rejection rules."
  (let* ((bootstrap-pathname
           (configuration-grok-bootstrap-auth-path configuration))
         (manager (grok-credential-manager-create configuration)))
    (test-write-grok-auth bootstrap-pathname
                          :auth-mode "api_key"
                          :user-id "user-a"
                          :access-token "bootstrap-a")
    (test-assert
     (null (credential-source-load
            (credential-manager-bootstrap-source manager)))
     "the Grok bootstrap rejects non-OIDC authentication modes")
    (test-write-grok-auth bootstrap-pathname
                          :auth-mode "oidc"
                          :user-id "user-a"
                          :access-token "bootstrap-a"
                          :scope "xai::api_key")
    (test-assert
     (null (credential-source-load
            (credential-manager-bootstrap-source manager)))
     "the Grok bootstrap ignores entries outside the first-party scope")
    (test-write-grok-auth bootstrap-pathname
                          :auth-mode "oidc"
                          :user-id "user-a"
                          :access-token "bootstrap-a")
    (let ((imported (credential-manager-load manager)))
      (test-assert
       (string= (oauth-credentials-account-id imported) "user-a")
       "the initial Grok bootstrap account is imported")
      (test-assert
       (null (oauth-credentials-refresh-token imported))
       "the Grok Build refresh token is never imported")
      (test-assert
       (equal (oauth-credentials-source-path imported)
              (configuration-grok-auth-path configuration))
       "bootstrap access is copied into Autolith's private Grok store"))
    (test-assert
     (handler-case
         (progn
           (credential-manager-refresh manager
                                       (credential-manager-load manager))
           nil)
       (token-refresh-failed (condition)
         (test-object-contains-string-p condition
                                        "run autolith auth grok")))
     "non-renewable Grok credentials point at the Grok device flow")
    (test-assert
     (handler-case
         (progn
           (credential-source-save
            (credential-manager-bootstrap-source manager)
            (credential-manager-load manager))
           nil)
       (authentication-error ()
         t))
     "the Grok Build bootstrap store rejects writes"))
  nil)

(-> grok-authentication-tests--test-refresh (configuration) null)
(defun grok-authentication-tests--test-refresh (configuration)
  "Test Grok refresh response validation and account continuity."
  (let* ((manager (grok-credential-manager-create configuration))
         (primary-source (credential-manager-primary-source manager))
         (renewable
           (make-instance 'oauth-credentials
                          :access-token "old-access"
                          :refresh-token "old-refresh"
                          :id-token nil
                          :account-id "user-a"
                          :expires-at nil
                          :source-path
                          (credential-source-pathname primary-source))))
    (let ((valid
            (grok-refresh-response-credentials
             manager
             renewable
             (json-encode
              (json-object "access_token" "new-access"
                           "refresh_token" "new-refresh"
                           "expires_in" 900)))))
      (test-assert
       (string= (oauth-credentials-access-token valid) "new-access")
       "a validated Grok refresh response yields new access credentials")
      (test-assert
       (string= (oauth-credentials-refresh-token valid) "new-refresh")
       "a validated Grok refresh response rotates the refresh token")
      (test-assert
       (string= (oauth-credentials-account-id valid) "user-a")
       "Grok refresh without a subject claim preserves the pinned account")
      (test-assert
       (let ((expires-at (oauth-credentials-expires-at valid)))
         (and expires-at
              (<= 890 (- expires-at (get-universal-time)) 910)))
       "Grok refresh maps expires_in onto an absolute expiration"))
    (let ((rotationless
            (grok-refresh-response-credentials
             manager
             renewable
             (json-encode (json-object "access_token" "new-access")))))
      (test-assert
       (string= (oauth-credentials-refresh-token rotationless) "old-refresh")
       "Grok refresh keeps the previous refresh token when none rotates"))
    (dolist (body '("not-json" "{}"))
      (test-assert
       (handler-case
           (progn
             (grok-refresh-response-credentials manager renewable body)
             nil)
         (token-refresh-failed ()
           t))
       "malformed Grok refresh bodies become typed failures"))
    (test-assert
     (handler-case
         (progn
           (grok-refresh-response-credentials
            manager
            renewable
            (json-encode
             (json-object
              "access_token" (test-subject-jwt "user-b")
              "refresh_token" "new-refresh")))
           nil)
       (token-refresh-failed ()
         t))
     "Grok refresh rejects a token that switches accounts")
    (credential-source-save primary-source renewable)
    (let ((condition
            (handler-case
                (test-call-with-function-replacements
                 (list
                  (list
                   'dexador:post
                   (lambda (url &rest arguments)
                     (declare (ignore url arguments))
                     (error
                      (make-condition
                       'http-request-failed
                       :body (json-encode
                              (json-object "error" "invalid_grant"))
                       :status 400
                       :headers nil
                       :uri nil
                       :method ':post)))))
                 (lambda ()
                   (credential-manager-refresh manager renewable)))
              (token-refresh-failed (failure)
                failure))))
      (test-assert
       (and condition
            (test-object-contains-string-p condition "invalid_grant")
            (test-object-contains-string-p condition "Grok"))
       "a rejected Grok refresh reports its OAuth code without secrets")))
  nil)

(-> test-grok-authentication () null)
(defun test-grok-authentication ()
  "Test Grok credential storage, bootstrap import, and refresh handling."
  (grok-authentication-tests--test-rfc3339)
  (test-assert
   (string= (jwt-subject (test-subject-jwt "user-a")) "user-a")
   "JWT subject extraction reads the sub claim")
  (test-assert
   (null (jwt-subject "not-a-jwt"))
   "JWT subject extraction rejects malformed tokens")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-grok-auth-path configuration)
                   (merge-pathnames "grok-auth.sexp"
                                    (configuration-state-root configuration)))
            "private Grok credentials live under the state root")
           (grok-authentication-tests--test-bootstrap configuration)
           (grok-authentication-tests--test-refresh configuration))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
