(in-package #:autolith)

;;;; -- OAuth Credentials --

(defvar *credentials-in-request-scope* nil
  "True only while provider credentials are dynamically available to a request.")

(defvar *active-secret-use-lock*
  (make-lock "Autolith active secret use")
  "The lock protecting the process-wide active secret-use counter.")

(defvar *active-secret-use-count* 0
  "The number of live provider or MCP operations that may retain secrets.")

(defvar *secret-use-depth* 0
  "The dynamic nesting depth of secret-bearing operations on this thread.")

(defvar *secret-use-quiescence-owner* nil
  "The thread temporarily preventing new transient secret use.")

(-> call-with-secret-use (function) t)
(defun call-with-secret-use (function)
  "Call FUNCTION while checkpointing can observe active transient secrets."
  (with-lock-held (*active-secret-use-lock*)
    (when (and *secret-use-quiescence-owner*
               (zerop *secret-use-depth*)
               (not (eq *secret-use-quiescence-owner*
                        sb-thread:*current-thread*)))
      (error 'authentication-error
             :message
             "A checkpoint is temporarily preventing new credential-bearing operations."))
    (incf *active-secret-use-count*))
  (let ((*secret-use-depth* (1+ *secret-use-depth*)))
    (unwind-protect
         (funcall function)
      (with-lock-held (*active-secret-use-lock*)
        (decf *active-secret-use-count*)
        (when (minusp *active-secret-use-count*)
          (setf *active-secret-use-count* 0)
          (error 'authentication-error
                 :message
                 "The active secret-use counter became inconsistent."))))))

(-> call-with-secret-use-quiescence (function) t)
(defun call-with-secret-use-quiescence (function)
  "Call FUNCTION while preventing other threads from beginning secret use.

Existing operations remain counted and may finish nested cleanup. Nested
secret use by the owner is also allowed so runtime shutdown can authenticate
its protocol-level close operation."
  (let ((owner sb-thread:*current-thread*))
    (with-lock-held (*active-secret-use-lock*)
      (when *secret-use-quiescence-owner*
        (error 'authentication-error
               :message "Another secret-use quiescence operation is active."))
      (setf *secret-use-quiescence-owner* owner))
    (unwind-protect
         (funcall function)
      (with-lock-held (*active-secret-use-lock*)
        (when (eq *secret-use-quiescence-owner* owner)
          (setf *secret-use-quiescence-owner* nil))))))

(-> secret-use-active-p () boolean)
(defun secret-use-active-p ()
  "Return true while any thread may retain transient secret values."
  (with-lock-held (*active-secret-use-lock*)
    (plusp *active-secret-use-count*)))

(-> jwt-account-id (string) (option string))
(defun jwt-account-id (token)
  "Return the ChatGPT account identifier carried by TOKEN, if any."
  (let ((payload (jwt-payload token)))
    (when payload
      (or (json-get payload "chatgpt_account_id")
          (let ((auth (json-get payload "https://api.openai.com/auth")))
            (and (json-object-p auth)
                 (json-get auth "chatgpt_account_id")))
          (let ((organizations (json-get payload "organizations")))
            (when (and (vectorp organizations)
                       (plusp (length organizations))
                       (json-object-p (aref organizations 0)))
              (json-get (aref organizations 0) "id")))))))


;;;; -- OAuth Wire Helpers --

(-> authentication-user-agent () string)
(defun authentication-user-agent ()
  "Return the honest Autolith user agent sent to authentication services."
  (format nil "autolith/~A (~A ~A; ~A)"
          *autolith-version*
          (software-type)
          (software-version)
          (machine-type)))

(-> oauth--base64url ((simple-array (unsigned-byte 8) (*))) string)
(defun oauth--base64url (octets)
  "Return OCTETS as unpadded RFC 4648 Base64url text."
  (string-right-trim
   '(#\=)
   (substitute #\_
               #\/
               (substitute #\-
                           #\+
                           (usb8-array-to-base64-string octets)))))

(-> oauth--create-pkce (&key (:verifier-octets integer)) (values string string))
(defun oauth--create-pkce (&key (verifier-octets 32))
  "Return a fresh PKCE verifier and S256 challenge from VERIFIER-OCTETS."
  (unless (plusp verifier-octets)
    (error 'authentication-error
           :message "The OAuth PKCE verifier size must be positive."))
  (let* ((verifier (oauth--base64url (random-data verifier-octets)))
         (octets (map '(simple-array (unsigned-byte 8) (*)) #'char-code verifier))
         (challenge
           (oauth--base64url
            (ironclad:digest-sequence ':sha256 octets))))
    (values verifier challenge)))

(-> oauth--query-parameters (string) list)
(defun oauth--query-parameters (target)
  "Decode TARGET's query string into an association list."
  (let ((question (position #\? target)))
    (when question
      (loop with query = (subseq target (1+ question))
            with start = 0
            for end = (position #\& query :start start)
            for field = (subseq query start end)
            for equals = (position #\= field)
            collect (cons (url-decode (subseq field 0 equals))
                          (url-decode (if equals
                                          (subseq field (1+ equals))
                                          "")))
            while end
            do (setf start (1+ end))))))

;;;; -- Credential Sources --

(defclass credential-source (cl-rfc8628:credential-source)
  ((pathname
    :initarg :pathname
    :reader credential-source-pathname
    :type pathname
    :documentation "The credential file read by this source."))
  (:documentation "A replaceable source of OAuth credentials."))

(defclass autolith-credential-source (credential-source)
  ()
  (:documentation "Autolith's private, writable S-expression credential store."))

(defclass codex-bootstrap-credential-source (credential-source)
  ()
  (:documentation "A read-only adapter for an existing Codex auth.json file."))

(defmethod credential-source-label ((source codex-bootstrap-credential-source))
  "Name the Codex bootstrap source in user-visible failures."
  (declare (ignore source))
  "Codex")

(-> read-portable-form (pathname) t)
(defun read-portable-form (pathname)
  "Read one portable form from PATHNAME with reader evaluation disabled."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read pathname)
    (declare (ignore sole-form-p))
    form))

(-> read-json-file-with-retry (pathname &key (:attempts integer)) json-object)
(defun read-json-file-with-retry (pathname &key (attempts 3))
  "Read PATHNAME as JSON, retrying transient partial rewrites up to ATTEMPTS."
  (loop for attempt from 1 to attempts
        do (handler-case
               (with-open-file (stream pathname
                                       :direction ':input
                                       :external-format ':utf-8)
                 (let ((value (yason:parse stream)))
                   (unless (json-object-p value)
                     (error "Credential root is not a JSON object."))
                   (return value)))
             (error (condition)
               (when (= attempt attempts)
                 (error condition))
               (sleep 0.02)))))

(defmethod credential-source-load ((source autolith-credential-source))
  "Load Autolith's private OAuth record from SOURCE."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (let ((record (read-portable-form pathname)))
        (unless (and (listp record) (eq (first record) :oauth))
          (error 'authentication-error
                 :message (format nil "Invalid Autolith credential record at ~A." pathname)))
        (let ((access-token (getf (rest record) :access-token))
              (account-id (getf (rest record) :account-id)))
          (when (and (non-empty-string-p access-token)
                     (non-empty-string-p account-id))
            (make-instance 'oauth-credentials
                           :access-token access-token
                           :refresh-token (getf (rest record) :refresh-token)
                           :id-token (getf (rest record) :id-token)
                           :account-id account-id
                           :expires-at (or (getf (rest record) :expires-at)
                                           (jwt-expiration access-token))
                           :source-path pathname)))))))

(defmethod credential-source-load ((source codex-bootstrap-credential-source))
  "Load one non-renewable ChatGPT bootstrap credential without modifying Codex."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
          (let* ((document (read-json-file-with-retry pathname))
                 (auth-mode (json-get document "auth_mode"))
                 (tokens (json-get document "tokens"))
                 (access-token (and (json-object-p tokens)
                                    (json-get tokens "access_token")))
                 (id-token (and (json-object-p tokens)
                                (json-get tokens "id_token")))
                 (account-id (and (json-object-p tokens)
                                  (or (json-get tokens "account_id")
                                      (and id-token (jwt-account-id id-token))
                                      (and access-token (jwt-account-id access-token))))))
            (when (and (stringp auth-mode)
                       (string-equal auth-mode "chatgpt")
                       (non-empty-string-p access-token)
                       (non-empty-string-p account-id))
              (make-instance 'oauth-credentials
                             :access-token access-token
                             :refresh-token nil
                             :id-token nil
                             :account-id account-id
                             :expires-at (jwt-expiration access-token)
                             :source-path pathname)))
        (error ()
          nil)))))

(defmethod credential-source-save ((source autolith-credential-source)
                                   (credentials oauth-credentials))
  "Atomically save CREDENTIALS to Autolith's private store with mode 0600."
  (let* ((pathname (credential-source-pathname source))
         (record (list :oauth
                       :version 1
                       :access-token (oauth-credentials-access-token credentials)
                       :refresh-token (oauth-credentials-refresh-token credentials)
                       :id-token (oauth-credentials-id-token credentials)
                       :account-id (oauth-credentials-account-id credentials)
                       :expires-at (oauth-credentials-expires-at credentials))))
    (snapshot-write pathname record)
    credentials))

(defmethod credential-source-save ((source codex-bootstrap-credential-source)
                                   (credentials oauth-credentials))
  "Reject writes to the Codex bootstrap source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message (format nil "The Codex bootstrap store ~A is read-only."
                          (credential-source-pathname source))))


(defclass credential-manager (cl-rfc8628:managed-credential-manager)
  ()
  (:documentation "Autolith's product-specific credential manager base."))

(defclass chatgpt-credential-manager (credential-manager)
  ()
  (:documentation "The ChatGPT OAuth credential manager behind the Codex provider."))

(defmethod credential-manager-provider-label
    ((manager chatgpt-credential-manager))
  "Name the ChatGPT account service in user-visible failures."
  (declare (ignore manager))
  "ChatGPT")

(defmethod credential-manager-login-hint ((manager chatgpt-credential-manager))
  "Point ChatGPT credential failures at the default login command."
  (declare (ignore manager))
  "run autolith auth")

(-> credential-manager-create (configuration) chatgpt-credential-manager)
(defun credential-manager-create (configuration)
  "Create the ChatGPT credential manager for CONFIGURATION's private paths."
  (make-instance 'chatgpt-credential-manager
                 :primary-source (make-instance
                                  'autolith-credential-source
                                  :pathname (configuration-auth-path configuration))
                 :bootstrap-source (make-instance
                                    'codex-bootstrap-credential-source
                                    :pathname (configuration-codex-auth-path configuration))))

(-> oauth-refresh-response-credentials
    (credential-manager oauth-credentials string)
    oauth-credentials)
(defun oauth-refresh-response-credentials (manager credentials body)
  "Validate refresh BODY and return account-continuous Autolith credentials."
  (handler-case
      (let ((response (json-decode body)))
        (unless (json-object-p response)
          (error "The OAuth refresh root is not an object."))
        (let* ((access-token (json-get response "access_token"))
               (response-id-token (json-get response "id_token"))
               (id-token (or response-id-token
                             (oauth-credentials-id-token credentials)))
               (rotated-refresh-token
                 (or (json-get response "refresh_token")
                     (oauth-credentials-refresh-token credentials))))
          (unless (and (non-empty-string-p access-token)
                       (or (null response-id-token)
                           (non-empty-string-p response-id-token))
                       (non-empty-string-p rotated-refresh-token))
            (error "The OAuth refresh response omitted required fields."))
          (let* ((previous-account
                   (oauth-credentials-account-id credentials))
                 (token-account
                   (or (and id-token (jwt-account-id id-token))
                       (jwt-account-id access-token)))
                 (account-id (or token-account previous-account)))
            (when (and token-account
                       (not (string= token-account previous-account)))
              (error 'token-refresh-failed
                     :message "The OAuth refresh response changed ChatGPT accounts."
                     :status nil
                     :response nil))
            (make-instance
             'oauth-credentials
             :access-token access-token
             :refresh-token rotated-refresh-token
             :id-token id-token
             :account-id account-id
             :expires-at (jwt-expiration access-token)
             :source-path
             (credential-source-pathname
              (credential-manager-primary-source manager))))))
    (token-refresh-failed (condition)
      (error condition))
    (error ()
      (error 'token-refresh-failed
             :message "The OAuth refresh response was malformed."
             :status nil
             :response nil))))

(-> credential-manager--refresh-token-reuse-recovery
    (credential-manager string (option string))
    (option oauth-credentials))

(defun credential-manager--refresh-token-reuse-recovery
    (manager attempted-refresh-token code)
  "Recover an already-published rotation after OpenAI rejects a reused token."
  (when (and code (string= code "refresh_token_reused"))
    (cl-rfc8628:credential-manager-newer-rotation manager attempted-refresh-token)))

(defmethod credential-manager-refresh-exchange
    ((manager chatgpt-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Rotate REFRESH-TOKEN at OpenAI, recovering when a sibling already rotated it."
  (handler-case
      (let* ((request (json-object
                       "client_id" *openai-oauth-client-id*
                       "grant_type" "refresh_token"
                       "refresh_token" refresh-token))
             (body
               (provider-call-with-response-deadline
                60
                (lambda ()
                  (dexador:post
                   *openai-oauth-token-endpoint*
                   :headers '(("Content-Type" . "application/json")
                              ("Accept" . "application/json"))
                   :content (json-encode request)
                   :force-string t
                   :connect-timeout 30
                   :read-timeout 60)))))
        (values (oauth-refresh-response-credentials manager credentials body)
                t))
    (sb-sys:deadline-timeout ()
      (error 'token-refresh-failed
             :message "OAuth token refresh exceeded its response deadline."
             :status nil
             :response nil))
    (http-request-failed (condition)
      (let* ((body (provider--error-body-text (response-body condition)))
             (raw-code (oauth-error-code body))
             (code
               (and
                raw-code
                (let ((secrets
                        (oauth-credentials-secret-values credentials)))
                  (redact-exact-string-values
                   raw-code
                   secrets
                   (safe-redaction-marker
                    "[OAUTH CREDENTIAL REDACTED]"
                    secrets)))))
             (newer-primary
               (credential-manager--refresh-token-reuse-recovery
                manager refresh-token code)))
          (if newer-primary
              (values newer-primary nil)
            (error 'token-refresh-failed
                   :message (format nil "OAuth token refresh failed~@[ (~A)~]." code)
                   :status (response-status condition)
                   :response code))))
    (authentication-error (condition)
      (error condition))
    (error ()
      (error 'token-refresh-failed
             :message "OAuth token refresh could not be completed."
             :status nil
             :response nil))))

(-> call-with-credentials
    (credential-manager function &key (:force-refresh boolean))
    t)

(defun call-with-credentials (manager function &key force-refresh)
  "Call FUNCTION inside Autolith's credential request scope."
  (let ((*credentials-in-request-scope* t))
    (cl-rfc8628:call-with-credentials manager function :force-refresh force-refresh)))

(defmacro with-credentials ((variable manager &key force-refresh) &body body)
  "Bind VARIABLE to request-scoped credentials from MANAGER while evaluating BODY."
  `(call-with-credentials ,manager
                          (lambda (,variable)
                            ,@body)
                          :force-refresh ,force-refresh))


;;;; -- cl-rfc8628 Host Wiring --

(setf cl-rfc8628:*secret-region-function* #'call-with-secret-use)

(setf cl-rfc8628:*credential-error-class* 'authentication-error
      cl-rfc8628:*credentials-unavailable-class* 'credentials-unavailable
      cl-rfc8628:*token-refresh-failed-class* 'token-refresh-failed)
