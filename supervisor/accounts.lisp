;;;; -- Shared OAuth Accounts --

(in-package #:autolith-supervisor)

(define-condition autolith-error (error)
  ((message
    :initarg       :message
    :reader        autolith-error-message
    :type          string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected Autolith failures.")
  (:report (lambda (condition stream)
             (write-string (autolith-error-message condition) stream))))

(define-condition authentication-error (autolith-error)
  ()
  (:documentation "The base condition for authentication failures."))

(define-condition credentials-unavailable (authentication-error)
  ((searched-paths
    :initarg       :searched-paths
    :reader        credentials-unavailable-searched-paths
    :type          list
    :documentation "Credential pathnames inspected before the failure."))
  (:documentation "No usable model-provider credentials were found."))

(define-condition token-refresh-failed (authentication-error)
  ((status
    :initarg       :status
    :reader        token-refresh-failed-status
    :type          (or null integer)
    :documentation "The HTTP status returned by the OAuth server, if known.")
   (response
    :initarg       :response
    :reader        token-refresh-failed-response
    :type          (or null string)
    :documentation "A bounded, non-secret OAuth error response."))
  (:documentation "Refreshing an OAuth access token failed."))

(defparameter *openai-oauth-token-endpoint* "https://auth.openai.com/oauth/token"
  "The OpenAI OAuth token endpoint.")

(defparameter *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "The public OAuth client identifier used by Codex-compatible clients.")

(defvar *credentials-in-request-scope* nil
  "True only while provider credentials are dynamically available to a request.")

(defvar *active-secret-use-lock*
  (bordeaux-threads:make-lock "Autolith active secret use")
  "The lock protecting the process-wide active secret-use counter.")

(defvar *active-secret-use-count* 0
  "The number of live provider or MCP operations that may retain secrets.")

(defvar *secret-use-depth* 0
  "The dynamic nesting depth of secret-bearing operations on this thread.")

(defvar *secret-use-quiescence-owner* nil
  "The thread temporarily preventing new transient secret use.")

(declaim (ftype (function (function) t) call-with-secret-use))

(defun call-with-secret-use (function)
  "Call FUNCTION while checkpointing can observe active transient secrets."
  (bordeaux-threads:with-lock-held (*active-secret-use-lock*)
    (when (and *secret-use-quiescence-owner*
               (zerop *secret-use-depth*)
               (not (eq *secret-use-quiescence-owner* sb-thread:*current-thread*)))
      (error 'authentication-error
             :message "A checkpoint is temporarily preventing new credential-bearing operations."))
    (incf *active-secret-use-count*))
  (let ((*secret-use-depth* (1+ *secret-use-depth*)))
    (unwind-protect
         (funcall function)
      (bordeaux-threads:with-lock-held (*active-secret-use-lock*)
        (decf *active-secret-use-count*)
        (when (minusp *active-secret-use-count*)
          (setf *active-secret-use-count* 0)
          (error 'authentication-error
                 :message "The active secret-use counter became inconsistent."))))))

(declaim (ftype (function (function) t) call-with-secret-use-quiescence))

(defun call-with-secret-use-quiescence (function)
  "Call FUNCTION while preventing other threads from beginning secret use.

Existing operations remain counted and may finish nested cleanup. Nested
secret use by the owner is allowed so shutdown can authenticate its close."
  (let ((owner sb-thread:*current-thread*))
    (bordeaux-threads:with-lock-held (*active-secret-use-lock*)
      (when *secret-use-quiescence-owner*
        (error 'authentication-error
               :message "Another secret-use quiescence operation is active."))
      (setf *secret-use-quiescence-owner* owner))
    (unwind-protect
         (funcall function)
      (bordeaux-threads:with-lock-held (*active-secret-use-lock*)
        (when (eq *secret-use-quiescence-owner* owner)
          (setf *secret-use-quiescence-owner* nil))))))

(declaim (ftype (function () boolean) secret-use-active-p))

(defun secret-use-active-p ()
  "Return true while any thread may retain transient secret values."
  (bordeaux-threads:with-lock-held (*active-secret-use-lock*)
    (plusp *active-secret-use-count*)))

(declaim (ftype (function () string) accounts-user-agent))

(defun accounts-user-agent ()
  "Return the honest Autolith user agent for authentication services."
  (format nil "autolith/~A (~A ~A; ~A)"
          (asdf:component-version (asdf:find-system "autolith-supervisor"))
          (software-type) (software-version) (machine-type)))

(declaim (ftype (function (string) (or null string)) jwt-account-id))

(defun jwt-account-id (token)
  "Return the ChatGPT account identifier carried by TOKEN, if any."
  (let ((payload (cl-rfc8628:jwt-payload token)))
    (when payload
      (or (cl-rfc8628:json-get payload "chatgpt_account_id")
          (let ((auth (cl-rfc8628:json-get payload "https://api.openai.com/auth")))
            (and (cl-rfc8628:json-object-p auth)
                 (cl-rfc8628:json-get auth "chatgpt_account_id")))
          (let ((organizations (cl-rfc8628:json-get payload "organizations")))
            (when (and (vectorp organizations)
                       (plusp (length organizations))
                       (cl-rfc8628:json-object-p (aref organizations 0)))
              (cl-rfc8628:json-get (aref organizations 0) "id")))))))


;;;; -- Credential Storage --

(defclass credential-source (cl-rfc8628:credential-source)
  ((pathname
    :initarg       :pathname
    :reader        cl-rfc8628:credential-source-pathname
    :type          pathname
    :documentation "The trusted credential file read by this source."))
  (:documentation "A replaceable source of OAuth credentials."))

(defclass autolith-credential-source (credential-source)
  ()
  (:documentation "Autolith's private, writable S-expression credential store."))

(defclass codex-bootstrap-credential-source (credential-source)
  ()
  (:documentation "A read-only adapter for an existing Codex auth.json file."))

(defmethod cl-rfc8628:credential-source-label ((source codex-bootstrap-credential-source))
  "Name the Codex bootstrap source in user-visible failures."
  (declare (ignore source))
  "Codex")

(declaim (ftype (function (pathname) t) read-portable-form))

(defun read-portable-form (pathname)
  "Read one portable form from PATHNAME with reader evaluation disabled."
  (multiple-value-bind (form sole-form-p)
      (sexp-store:snapshot-read pathname)
    (declare (ignore sole-form-p))
    form))

(defparameter *credential-json-maximum-octets* (* 1024 1024)
  "The largest external credential JSON file read.")

(declaim (ftype (function (pathname &key (:attempts integer)) hash-table)
                read-json-file-with-retry))

(defun read-json-file-with-retry (pathname &key (attempts 3))
  "Read PATHNAME as JSON, retrying transient partial rewrites up to ATTEMPTS.

The file belongs to another tool, so a symbolic link to it is followed."
  (loop for attempt from 1 to attempts
        do (handler-case
               (let ((value (cl-rfc8628:json-decode
                             (ls-compat.files:read-file-text
                              pathname :maximum-octets *credential-json-maximum-octets*
                              :follow-links-p          t))))
                 (unless (cl-rfc8628:json-object-p value)
                   (error 'authentication-error
                          :message "Credential root is not a JSON object."))
                 (return value))
             (error (condition)
               (when (= attempt attempts)
                 (error condition))
               (sleep 0.02)))))

(defmethod cl-rfc8628:credential-source-load ((source autolith-credential-source))
  "Load Autolith's private OAuth record from SOURCE."
  (let ((pathname (cl-rfc8628:credential-source-pathname source)))
    (when (probe-file pathname)
      (let ((record (read-portable-form pathname)))
        (unless (and (listp record) (eq (first record) :oauth))
          (error 'authentication-error
                 :message (format nil "Invalid Autolith credential record at ~A." pathname)))
        (let ((access-token (getf (rest record) :access-token))
              (account-id   (getf (rest record) :account-id)))
          (when (and (cl-rfc8628:non-empty-string-p access-token)
                     (cl-rfc8628:non-empty-string-p account-id))
            (make-instance 'cl-rfc8628:oauth-credentials
                           :access-token  access-token
                           :refresh-token (getf (rest record) :refresh-token)
                           :id-token      (getf (rest record) :id-token)
                           :account-id    account-id
                           :expires-at    (or (getf (rest record) :expires-at)
                                              (cl-rfc8628:jwt-expiration access-token))
                           :source-path   pathname)))))))

(defmethod cl-rfc8628:credential-source-load ((source codex-bootstrap-credential-source))
  "Load one non-renewable ChatGPT bootstrap credential without modifying Codex."
  (let ((pathname (cl-rfc8628:credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
          (let* ((document     (read-json-file-with-retry pathname))
                 (auth-mode    (cl-rfc8628:json-get document "auth_mode"))
                 (tokens       (cl-rfc8628:json-get document "tokens"))
                 (access-token (and (cl-rfc8628:json-object-p tokens)
                                    (cl-rfc8628:json-get tokens "access_token")))
                 (id-token     (and (cl-rfc8628:json-object-p tokens)
                                    (cl-rfc8628:json-get tokens "id_token")))
                 (account-id   (and (cl-rfc8628:json-object-p tokens)
                                    (or (cl-rfc8628:json-get tokens "account_id")
                                        (and id-token (jwt-account-id id-token))
                                        (and access-token (jwt-account-id access-token))))))
            (when (and (stringp auth-mode)
                       (string-equal auth-mode "chatgpt")
                       (cl-rfc8628:non-empty-string-p access-token)
                       (cl-rfc8628:non-empty-string-p account-id))
              (make-instance 'cl-rfc8628:oauth-credentials
                             :access-token  access-token
                             :refresh-token nil
                             :id-token      nil
                             :account-id    account-id
                             :expires-at    (cl-rfc8628:jwt-expiration access-token)
                             :source-path   pathname)))
        (error ()
          nil)))))

(defmethod cl-rfc8628:credential-source-save ((source autolith-credential-source)
                                              (credentials cl-rfc8628:oauth-credentials))
  "Atomically save CREDENTIALS to Autolith's private store with mode 0600."
  (let* ((pathname (cl-rfc8628:credential-source-pathname source))
         (record   (list :oauth
                         :version 1
                         :access-token  (cl-rfc8628:oauth-credentials-access-token credentials)
                         :refresh-token (cl-rfc8628:oauth-credentials-refresh-token credentials)
                         :id-token      (cl-rfc8628:oauth-credentials-id-token credentials)
                         :account-id    (cl-rfc8628:oauth-credentials-account-id credentials)
                         :expires-at    (cl-rfc8628:oauth-credentials-expires-at credentials))))
    (sexp-store:snapshot-write pathname record)
    credentials))

(defmethod cl-rfc8628:credential-source-save ((source codex-bootstrap-credential-source)
                                              (credentials cl-rfc8628:oauth-credentials))
  "Reject writes to the Codex bootstrap source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message (format nil "The Codex bootstrap store ~A is read-only."
                          (cl-rfc8628:credential-source-pathname source))))


;;;; -- ChatGPT Refresh and Account Scope --

(defclass credential-manager (cl-rfc8628:managed-credential-manager)
  ()
  (:documentation "Autolith's product-specific credential manager base."))

(defclass oauth-credential-manager
    (credential-manager cl-rfc8628:refresh-grant-credential-manager)
  ()
  (:documentation "An Autolith manager rotating OAuth tokens with the refresh grant."))

(defmethod cl-rfc8628:credential-manager-refresh-request :around
    ((manager oauth-credential-manager) &key url headers content)
  "Bound one refresh exchange by its response deadline and read streamed error bodies."
  (declare (ignore url headers content))
  (handler-case
      (multiple-value-bind (body status response-headers)
          (cl-llm-provider-api:provider-call-with-response-deadline
           60 (lambda () (call-next-method)))
        (values (if (stringp body)
                    body
                    (or (cl-llm-provider-api:provider--error-body-text body) ""))
                status response-headers))
    (sb-sys:deadline-timeout ()
      (error 'token-refresh-failed
             :message (format nil "~A OAuth token refresh exceeded its response deadline."
                              (cl-rfc8628:credential-manager-provider-label manager))
             :status nil :response nil))))

(defclass chatgpt-credential-manager (oauth-credential-manager)
  ()
  (:documentation "The ChatGPT OAuth credential manager behind the Codex provider."))

(defmethod cl-rfc8628:credential-manager-provider-label ((manager chatgpt-credential-manager))
  "Name the ChatGPT account service in user-visible failures."
  (declare (ignore manager))
  "ChatGPT")

(defmethod cl-rfc8628:credential-manager-login-hint ((manager chatgpt-credential-manager))
  "Point ChatGPT credential failures at the default login command."
  (declare (ignore manager))
  "run autolith auth")

(declaim (ftype (function (&key (:auth-path pathname) (:codex-auth-path (or null pathname)))
                         chatgpt-credential-manager)
                credential-manager-create))

(defun credential-manager-create (&key auth-path codex-auth-path)
  "Create a ChatGPT manager from explicit trusted private and optional bootstrap paths."
  (check-type auth-path pathname)
  (check-type codex-auth-path (or null pathname))
  (make-instance 'chatgpt-credential-manager
                 :primary-source   (make-instance 'autolith-credential-source :pathname auth-path)
                 :bootstrap-source (when codex-auth-path
                                     (make-instance 'codex-bootstrap-credential-source
                                                    :pathname codex-auth-path))))

(defmethod cl-rfc8628:credential-manager-token-endpoint ((manager chatgpt-credential-manager))
  "Refresh ChatGPT credentials at OpenAI's OAuth token endpoint."
  (declare (ignore manager))
  *openai-oauth-token-endpoint*)

(defmethod cl-rfc8628:credential-manager-client-id ((manager chatgpt-credential-manager))
  "Refresh as the first-party Codex OAuth client."
  (declare (ignore manager))
  *openai-oauth-client-id*)

(defmethod cl-rfc8628:credential-manager-refresh-content-type ((manager chatgpt-credential-manager))
  "Send OpenAI's token endpoint a JSON refresh request."
  (declare (ignore manager))
  cl-rfc8628:*refresh-json-content-type*)

(defmethod cl-rfc8628:credential-manager-refreshed-account-ids
    ((manager chatgpt-credential-manager) (document hash-table))
  "Return the ChatGPT accounts claimed by DOCUMENT's OpenID and access tokens."
  (declare (ignore manager))
  (loop for key in '("id_token" "access_token")
        for token = (cl-rfc8628:json-get document key)
        for account-id = (and (cl-rfc8628:non-empty-string-p token) (jwt-account-id token))
        when account-id
          collect account-id))

(declaim (ftype (function (credential-manager function &key (:force-refresh boolean)) t)
                call-with-credentials))

(defun call-with-credentials (manager function &key force-refresh)
  "Call FUNCTION inside the shared credential request scope."
  (let ((*credentials-in-request-scope* t))
    (cl-rfc8628:call-with-credentials manager function :force-refresh force-refresh)))

(defmacro with-credentials ((variable manager &key force-refresh) &body body)
  "Bind VARIABLE to request-scoped credentials from MANAGER while evaluating BODY."
  `(call-with-credentials ,manager
                          (lambda (,variable)
                            ,@body)
                          :force-refresh ,force-refresh))


;;;; -- Process-wide OAuth Host Wiring --

;; Own these library hooks here so loading legacy agent code cannot replace them.
(setf cl-rfc8628:*secret-region-function* #'call-with-secret-use
      cl-rfc8628:*user-agent-function* #'accounts-user-agent
      cl-rfc8628:*credential-error-class* 'authentication-error
      cl-rfc8628:*credentials-unavailable-class* 'credentials-unavailable
      cl-rfc8628:*token-refresh-failed-class* 'token-refresh-failed)
