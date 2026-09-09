(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> authentication-tests--test-secret-use-quiescence () null)
(defun authentication-tests--test-secret-use-quiescence ()
  "Test checkpoint quiescence rejects new work without blocking its owner."
  (let ((child-result nil))
    (call-with-secret-use-quiescence
     (lambda ()
       (call-with-secret-use
        (lambda ()
          (test-assert
           (secret-use-active-p)
           "the quiescence owner may perform nested secret-bearing cleanup")))
       (let ((thread
               (make-thread
                (lambda ()
                  (setf
                   child-result
                   (handler-case
                       (progn
                         (call-with-secret-use (lambda () nil))
                         ':unexpected-success)
                     (authentication-error ()
                       ':rejected))))
                :name "Autolith secret-use quiescence test")))
         (join-thread thread))))
    (test-assert
     (eq child-result ':rejected)
     "checkpoint quiescence rejects a new secret user on another thread")
    (test-assert
     (not (secret-use-active-p))
     "checkpoint quiescence leaves no active secret-use count")
    (test-assert
     (null *secret-use-quiescence-owner*)
     "checkpoint quiescence releases its owner after success"))
  (let* ((lock (make-lock "Autolith existing secret-use test"))
         (condition
           (make-condition-variable
            :name "Autolith existing secret-use test"))
         (ready-p nil)
         (continue-p nil)
         (nested-use-observed-p nil)
         (thread
           (make-thread
            (lambda ()
              (call-with-secret-use
               (lambda ()
                 (with-lock-held (lock)
                   (setf ready-p t)
                   (condition-notify condition)
                   (loop until continue-p
                         do (condition-wait condition lock)))
                 (call-with-secret-use
                  (lambda ()
                    (setf nested-use-observed-p
                          (secret-use-active-p)))))))
            :name "Autolith existing secret-use test")))
    (with-lock-held (lock)
      (loop until ready-p
            do (condition-wait condition lock)))
    (call-with-secret-use-quiescence
     (lambda ()
       (with-lock-held (lock)
         (setf continue-p t)
         (condition-notify condition))
       (join-thread thread)))
    (test-assert
     nested-use-observed-p
     "a pre-existing secret operation may enter nested cleanup during quiescence"))
  (handler-case
      (call-with-secret-use-quiescence
       (lambda ()
         (error "synthetic quiescence failure")))
    (simple-error ()
      nil))
  (test-assert
   (null *secret-use-quiescence-owner*)
   "checkpoint quiescence releases its owner during unwinding")
  (test-assert
   (string=
    (redact-exact-string-values
     "unchanged"
    (list "" nil)
    "[REDACTED]")
   "unchanged")
   "exact redaction ignores empty and absent secret values")
  (let* ((secrets '("]x" "Z"))
         (marker (safe-redaction-marker "[REDACTED]" secrets))
         (redacted
           (redact-exact-string-values "Zx" secrets marker)))
    (test-assert
     (notany (lambda (secret)
               (search secret redacted))
             secrets)
     "redaction markers cannot form an earlier secret across a boundary"))
  nil)

(-> authentication-tests--test-oauth-error-code () null)
(defun authentication-tests--test-oauth-error-code ()
  "Test OAuth error extraction rejects malformed values and bounds strings."
  (dolist (body
            (list
             (json-encode (json-object "error" 42))
             (json-encode
              (json-object "error" (json-object "code" 42)))
             (json-encode
              (json-object "error"
                           (json-object "type" (json-object "nested" t))))
             (json-encode (json-object "error" "   "))
             "not-json"))
    (test-assert
     (null (oauth-error-code body))
     "OAuth error extraction rejects a malformed or empty code"))
  (let* ((unbounded (make-string 300 :initial-element #\x))
         (code
           (oauth-error-code
            (json-encode (json-object "error" unbounded)))))
    (test-assert
     (and code (= (length code) 256))
     "OAuth error extraction bounds an untrusted string code"))
  nil)

(-> authentication-tests--test-api-key-prompt () null)
(defun authentication-tests--test-api-key-prompt ()
  "Test API-key entry gives explicit hidden-input instructions without echoing."
  (let* ((escape (code-char 27))
         (key "secret-test-key")
         (input
           (make-string-input-stream
            (format nil "~C[200~~~A~C[201~~~%" escape key escape)))
         (output (make-string-output-stream))
         (value
            (api-key-read-hidden
             "Example"
             :input input
             :input-file-descriptor -1
             :stream output
             :note "EXAMPLE_API_KEY overrides the stored key when set."))
         (text (get-output-stream-string output)))
    (test-assert
     (string= value key)
     "API-key entry removes bracketed-paste markers")
    (test-assert
     (and (search "Example authentication" text)
          (search "Paste the Example API key below, then press Enter." text)
          (search "Input is hidden. Nothing will appear" text)
          (search "EXAMPLE_API_KEY overrides the stored key" text)
          (search "API key" text)
          (not (search key text)))
     "API-key entry clearly labels the hidden field without echoing its value"))
  (let ((input (make-string-input-stream (format nil "styled-key~%")))
        (output (make-string-output-stream))
        (*api-key-output-styled-p* t)
        (*terminal-style-reset* "<reset>"))
    (test-call-with-function-replacements
     (list
      (list 'terminal-style-sequence
            (lambda (style &optional indexed-color-p)
              (declare (ignore indexed-color-p))
              (format nil "<~A>" style))))
     (lambda ()
       (api-key-read-hidden
        "Styled"
        :input input
        :input-file-descriptor -1
        :stream output
        :note "Credential note.")))
    (let ((text (get-output-stream-string output)))
      (test-assert
       (and (search "<BRAND>" text)
            (search "<DIM>" text)
            (search "<HINT>" text)
            (search "<USER>" text)
            (search "<reset>" text))
       "interactive API-key prompts use semantic terminal styles")))
  nil)

(defclass authentication-test-error-input-stream
    (sb-gray:fundamental-character-input-stream)
  ()
  (:documentation "A test stream that fails as soon as API-key input is read."))

(defmethod sb-gray:stream-read-char
    ((stream authentication-test-error-input-stream))
  "Signal a synthetic input failure for STREAM."
  (declare (ignore stream))
  (error "synthetic API-key input failure"))

(-> authentication-tests--test-api-key-terminal-mode () null)
(defun authentication-tests--test-api-key-terminal-mode ()
  "Test hidden entry detects descriptors, fails closed, and always restores modes."
  (let ((restored nil))
    (test-call-with-function-replacements
     (list
      (list 'api-key--hidden-input-mode
            (lambda (input configured-descriptor)
              (declare (ignore input configured-descriptor))
              (cons 7 ':saved-mode)))
      (list 'api-key--restore-input-mode
            (lambda (saved-mode)
              (setf restored saved-mode)
              nil)))
     (lambda ()
       (test-assert
        (string=
         (api-key-read-hidden
          "Example"
          :input (make-string-input-stream (format nil "restored-key~%"))
          :stream (make-string-output-stream))
         "restored-key")
        "API-key entry returns the entered key when concealment succeeds")))
    (test-assert
     (equal restored (cons 7 ':saved-mode))
     "API-key entry restores the saved terminal mode after reading"))
  (let ((restore-count 0)
        (restored nil))
    (test-call-with-function-replacements
     (list
      (list 'api-key--hidden-input-mode
            (lambda (input configured-descriptor)
              (declare (ignore input configured-descriptor))
              (cons 8 ':saved-mode)))
      (list 'api-key--restore-input-mode
            (lambda (saved-mode)
              (incf restore-count)
              (setf restored saved-mode)
              nil)))
     (lambda ()
       (test-assert
        (handler-case
            (progn
              (api-key-read-hidden
               "Example"
               :input (make-instance 'authentication-test-error-input-stream)
               :stream (make-string-output-stream))
              nil)
          (simple-error ()
            t))
        "API-key entry propagates an input failure after concealment")))
    (test-assert
     (and (= restore-count 1)
          (equal restored (cons 8 ':saved-mode)))
     "API-key entry restores the saved terminal mode exactly once after read failure"))
  (let ((input (make-string-input-stream (format nil "must-not-be-read~%"))))
    (test-call-with-function-replacements
     (list
      (list 'api-key--hidden-input-mode
            (lambda (ignored configured-descriptor)
              (declare (ignore ignored configured-descriptor))
              (error 'authentication-error
                     :message "synthetic concealment failure"))))
     (lambda ()
       (test-assert
        (handler-case
            (progn
              (api-key-read-hidden
               "Example"
               :input input
               :stream (make-string-output-stream))
              nil)
          (authentication-error ()
            t))
        "API-key entry fails instead of reading when concealment fails")))
    (test-assert
     (string= (read-line input) "must-not-be-read")
     "failed concealment leaves the API key unread"))
  (let ((input (make-string-input-stream (format nil "descriptorless-key~%"))))
    (test-assert
     (handler-case
         (progn
           (api-key-read-hidden
            "Example"
            :input input
            :stream (make-string-output-stream))
           nil)
       (authentication-error ()
         t))
     "API-key entry rejects an input wrapper without a known descriptor")
    (test-assert
     (string= (read-line input) "descriptorless-key")
     "descriptorless failure leaves the API key unread"))
  (let ((input (make-string-input-stream (format nil "bound-wrapper-key~%"))))
    (let ((*standard-input* input))
      (test-assert
       (handler-case
           (progn
             (api-key-read-hidden
              "Example"
              :input input
              :stream (make-string-output-stream))
             nil)
         (authentication-error ()
           t))
       "a descriptorless standard-input wrapper still fails closed"))
    (test-assert
     (string= (read-line input) "bound-wrapper-key")
     "a descriptorless standard-input wrapper remains unread"))
  (let* ((input (make-string-input-stream (format nil "known-wrapper-key~%")))
         (*standard-input* input)
         (*api-key-input-file-descriptor* -1))
    (test-assert
     (string= (api-key-read-hidden
               "Example"
               :input input
               :stream (make-string-output-stream))
              "known-wrapper-key")
     "a transport-provided descriptor permits hidden input through a wrapper"))
  (with-test-fixture (':pseudo-terminals
                      "API-key entry through a wrapped pseudo-terminal")
    (test-fixture-call-with-pseudo-terminal
     *platform*
     (lambda (descriptor)
       (let ((input (make-string-input-stream (format nil "wrapped-key~%"))))
         (test-assert
          (not (interactive-stream-p input))
          "the descriptor test uses a noninteractive input wrapper")
         (test-assert
          (api-key--interactive-file-descriptor-p descriptor)
          "API-key entry recognizes the wrapped TTY descriptor")
         (let* ((before (test-fixture-terminal-input-mode *platform* descriptor))
                (before-echo-p (test-fixture-terminal-echo-p *platform* descriptor))
                (saved-mode (api-key--hidden-input-mode input descriptor))
                (during-echo-p (test-fixture-terminal-echo-p *platform* descriptor)))
           (unwind-protect
                (test-assert
                 (and before-echo-p (not during-echo-p))
                 "API-key concealment clears ECHO on the actual TTY")
             (api-key--restore-input-mode saved-mode))
           (test-assert
            (= (test-fixture-terminal-input-mode *platform* descriptor) before)
            "API-key concealment restores the actual TTY mode"))
         (test-assert
          (string=
           (api-key-read-hidden
            "Example"
            :input input
            :input-file-descriptor descriptor
            :stream (make-string-output-stream))
           "wrapped-key")
          "API-key entry conceals a TTY reached through a noninteractive wrapper")))))
  nil)

(-> authentication-tests--test-provider-api-key-prompts () null)
(defun authentication-tests--test-provider-api-key-prompts ()
  "Test built-in API-key providers use the shared hidden prompt and secret scope."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (calls nil)
         (fireworks-secret-use-active-p nil)
         (fireworks-input-descriptor nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'api-key-read-hidden
                 (lambda (provider-name
                          &key input input-file-descriptor stream note)
                   (declare (ignore input stream))
                   (when (string= provider-name "Fireworks")
                     (setf fireworks-input-descriptor input-file-descriptor))
                   (push (list provider-name note) calls)
                   (format nil "~A-test-key" (string-downcase provider-name))))
           (list 'anthropic-validate-api-key
                 (lambda (key)
                   (declare (ignore key))
                   nil))
           (list 'fireworks-validate-api-key
                 (lambda (key)
                   (declare (ignore key))
                   (setf fireworks-secret-use-active-p
                         (secret-use-active-p))
                   nil)))
          (lambda ()
            (anthropic-api-key-login
             (anthropic-credential-manager-create configuration)
             :stream (make-string-output-stream))
            (fireworks-api-key-login
             (fireworks-credential-manager-create configuration)
             :stream (make-string-output-stream)
             :input (make-string-input-stream "")
             :input-file-descriptor 17)))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    (test-assert
     (and
      (find "Anthropic" calls :key #'first :test #'string=)
      (find "Fireworks" calls :key #'first :test #'string=)
      (every (lambda (call)
               (search "overrides the stored key" (second call)))
             calls))
     "Anthropic and Fireworks use the shared labeled prompt with override guidance")
    (test-assert
     (and fireworks-secret-use-active-p
          (= fireworks-input-descriptor 17)
          (not (secret-use-active-p)))
     "Fireworks forwards its descriptor, validates in secret scope, and releases it"))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (fireworks-secret-use-active-p nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'api-key-read-hidden
                 (lambda (provider-name
                          &key input input-file-descriptor stream note)
                   (declare
                    (ignore provider-name input input-file-descriptor stream note))
                   "fireworks-test-key"))
           (list 'fireworks-validate-api-key
                 (lambda (key)
                   (declare (ignore key))
                   (setf fireworks-secret-use-active-p
                         (secret-use-active-p))
                   (error 'authentication-error
                          :message "synthetic Fireworks validation failure"))))
          (lambda ()
            (test-assert
             (handler-case
                 (progn
                   (fireworks-api-key-login
                    (fireworks-credential-manager-create configuration)
                    :stream (make-string-output-stream)
                    :input (make-string-input-stream ""))
                   nil)
               (authentication-error ()
                 t))
             "Fireworks login propagates validation failure")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    (test-assert
     (and fireworks-secret-use-active-p
          (not (secret-use-active-p)))
     "Fireworks validation failure releases its secret scope"))
  nil)

(-> authentication-tests--test-main-input-descriptor () null)
(defun authentication-tests--test-main-input-descriptor ()
  "Test command-line authentication supplies terminal input and styling state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (observed-descriptor nil)
         (observed-styled-p ':unset)
          (observed-method nil)
         (provider-function
           (lambda (candidate selection)
             (declare (ignore candidate selection))
             ':test-provider))
         (authenticator
            (lambda (provider method &key stream open-browser-p)
              (declare (ignore stream))
              (test-assert (and (eq provider ':test-provider)
                                open-browser-p)
                           "command-line auth invokes the selected provider")
              (setf observed-descriptor *api-key-input-file-descriptor*
                    observed-styled-p *api-key-output-styled-p*
                    observed-method method)
              "Provider authentication was saved.")))
    (unwind-protect
         (progn
           (test-call-with-function-replacements
            (list (list 'main--authentication-provider provider-function)
                  (list 'provider-authenticate-with-method authenticator))
            (lambda ()
              (let ((*standard-output* (make-string-output-stream)))
                (main-authenticate configuration "example"))))
           (test-assert
            (and (= observed-descriptor 0)
                 (null observed-styled-p))
            "noninteractive command-line auth supplies stdin without terminal styling")
            (test-assert (null observed-method)
                         "command-line auth defaults its method selection")
           (setf observed-styled-p ':unset)
           (test-call-with-function-replacements
            (list (list 'main--authentication-provider provider-function)
                  (list 'main--authentication-output-styled-p
                        (lambda (stream)
                          (declare (ignore stream))
                          t))
                  (list 'provider-authenticate-with-method authenticator))
            (lambda ()
              (let ((*standard-output* (make-string-output-stream)))
                (main-authenticate configuration "example" "device"))))
            (test-assert (and (eq observed-styled-p t)
                              (string= observed-method "device"))
                         "command-line auth passes styling and method selection"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-authentication-store () null)
(defun test-authentication-store ()
  "Test private credential storage without exposing real authentication data."
  (authentication-tests--test-secret-use-quiescence)
  (authentication-tests--test-oauth-error-code)
  (authentication-tests--test-api-key-prompt)
  (authentication-tests--test-api-key-terminal-mode)
  (authentication-tests--test-provider-api-key-prompts)
  (authentication-tests--test-main-input-descriptor)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (make-instance 'autolith-credential-source
                                :pathname (configuration-auth-path configuration)))
         (credentials (make-instance 'oauth-credentials
                                     :access-token "test-access-token"
                                     :refresh-token "test-refresh-token"
                                     :id-token nil
                                     :account-id "test-account"
                                     :expires-at nil
                                     :source-path (configuration-auth-path configuration))))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-auth-path configuration)
                   (merge-pathnames "auth.sexp"
                                    (configuration-state-root configuration)))
            "private credentials live under the state root")
           (credential-source-save source credentials)
           (let ((loaded (credential-source-load source)))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "test-account")
              "the private credential store round-trips its account")
             (test-assert (test-fixture-permissions-p
                           *platform*
                           (configuration-auth-path configuration)
                           ':private-file)
                          "the private credential store is private to the user")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-write-codex-auth
    (pathname &key (:auth-mode string) (:account-id string) (:access-token string))
    null)
(defun test-write-codex-auth (pathname &key auth-mode account-id access-token)
  "Write a synthetic Codex credential document to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string
     (json-encode
      (json-object
       "auth_mode" auth-mode
       "tokens" (json-object
                  "access_token" access-token
                  "refresh_token" "must-not-be-imported"
                  "account_id" account-id)))
     stream))
  nil)

(-> test-account-jwt (string) string)
(defun test-account-jwt (account-id)
  "Return a synthetic unsigned JWT carrying ACCOUNT-ID."
  (format nil
          "e30.~A.signature"
          (cl-base64:string-to-base64-string
           (json-encode (json-object "chatgpt_account_id" account-id))
           :uri t)))

(-> test-authentication-bootstrap-and-refresh () null)
(defun test-authentication-bootstrap-and-refresh ()
  "Test one-way Codex bootstrap import, account continuity, and refresh parsing."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (bootstrap-pathname (configuration-codex-auth-path configuration))
         (manager (credential-manager-create configuration)))
    (unwind-protect
         (progn
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "apikey"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (test-assert
            (null (credential-source-load
                   (credential-manager-bootstrap-source manager)))
            "Codex bootstrap rejects non-ChatGPT authentication modes")
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (let ((imported (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id imported) "account-a")
              "the initial ChatGPT bootstrap account is imported")
             (test-assert (null (oauth-credentials-refresh-token imported))
                          "the Codex refresh token is never imported")
             (test-assert
              (equal (oauth-credentials-source-path imported)
                     (configuration-auth-path configuration))
              "bootstrap access is copied into Autolith's private store"))
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-b"
                                  :access-token "bootstrap-b")
           (let ((loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "account-a")
              "subsequent loads ignore changes to the Codex bootstrap store")
             (test-assert
              (string= (oauth-credentials-access-token loaded) "bootstrap-a")
              "Autolith requests depend only on the imported private credential"))
           (test-assert
            (handler-case
                (progn
                  (credential-manager-refresh manager
                                              (credential-manager-load manager))
                  nil)
              (token-refresh-failed ()
                t))
             "non-renewable bootstrap credentials require Autolith's browser login")
           (let* ((primary-source (credential-manager-primary-source manager))
                  (renewable
                    (make-instance 'oauth-credentials
                                   :access-token "old-access"
                                   :refresh-token "old-refresh"
                                   :id-token (test-account-jwt "account-a")
                                   :account-id "account-a"
                                   :expires-at nil
                                   :source-path
                                   (credential-source-pathname primary-source)))
                  (valid
                    (oauth-refresh-response-credentials
                     manager
                     renewable
                     (json-encode
                      (json-object "access_token" "new-access"
                                   "refresh_token" "new-refresh")))))
             (test-assert
              (string= (oauth-credentials-access-token valid) "new-access")
              "a validated refresh response yields new access credentials")
             (test-assert
              (string= (oauth-credentials-account-id valid) "account-a")
              "refresh without an account claim preserves the pinned account")
             (dolist (body '("not-json" "{}"))
               (test-assert
                (handler-case
                    (progn
                      (oauth-refresh-response-credentials manager renewable body)
                      nil)
                  (token-refresh-failed ()
                    t))
                "malformed refresh success bodies become typed failures"))
             (dolist (accounts '(("account-b" nil)
                                 ("account-b" "account-a")
                                 ("account-a" "account-b")))
               (let ((response (json-object
                                "access_token" (test-account-jwt (first accounts))
                                "refresh_token" "new-refresh")))
                 (when (second accounts)
                   (setf (gethash "id_token" response)
                         (test-account-jwt (second accounts))))
                 (test-assert
                  (handler-case
                      (progn
                        (oauth-refresh-response-credentials
                         manager renewable (json-encode response))
                        nil)
                    (token-refresh-failed ()
                      t))
                  "each returned account claim must match the credential account")))
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
                                :body
                                (json-encode
                                 (json-object "error" "old-refresh"))
                                :status 400
                                :headers nil
                                :uri nil
                                :method ':post)))))
                          (lambda ()
                            (credential-manager-refresh manager renewable)))
                       (token-refresh-failed (failure)
                         failure))))
               (test-assert
                (and
                 condition
                 (not
                  (test-object-contains-string-p
                   condition
                   "old-refresh"))
                 (test-object-contains-string-p
                  condition
                  "[OAUTH CREDENTIAL REDACTED]"))
                "OAuth failure diagnostics redact an echoed refresh token"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
