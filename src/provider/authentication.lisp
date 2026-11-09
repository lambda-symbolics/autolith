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
  "Call FUNCTION while counting its use of transient secrets."
  (with-lock-held (*active-secret-use-lock*)
    (when (and *secret-use-quiescence-owner*
               (zerop *secret-use-depth*)
               (not (eq *secret-use-quiescence-owner*
                        sb-thread:*current-thread*)))
      (error 'authentication-error
             :message
             "New credential operations are temporarily blocked for a checkpoint."))
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

Existing operations may finish nested cleanup. The owner may use secrets to
authenticate runtime shutdown."
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

(-> provider-user-agent () string)
(defun provider-user-agent ()
  "Return the Autolith user agent used for provider and authentication requests."
  (format nil "autolith/~A (~A ~A; ~A)"
          *autolith-version*
          (software-type)
          (software-version)
          (machine-type)))


;;;; -- Credential Sources --

(defclass credential-source (cl-rfc8628:credential-source)
  ((pathname
    :initarg       :pathname
    :reader        credential-source-pathname
    :type          pathname
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

(defparameter *credential-json-maximum-octets* (* 1024 1024)
  "The largest external credential JSON file read.")

(-> read-json-file-with-retry (pathname &key (:attempts integer)) json-object)
(defun read-json-file-with-retry (pathname &key (attempts 3))
  "Read PATHNAME as JSON, retrying transient partial rewrites up to ATTEMPTS.

The file belongs to another tool, so a symbolic link to it is followed."
  (loop for attempt from 1 to attempts
        do (handler-case
               (let ((value (json-decode
                             (read-file-text pathname
                                             :maximum-octets *credential-json-maximum-octets*
                                             :follow-links-p t))))
                 (unless (json-object-p value)
                   (error 'authentication-error
                          :message "Credential root is not a JSON object."))
                 (return value))
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
              (account-id   (getf (rest record) :account-id)))
          (when (and (non-empty-string-p access-token)
                     (non-empty-string-p account-id))
            (make-instance 'oauth-credentials
                           :access-token  access-token
                           :refresh-token (getf (rest record) :refresh-token)
                           :id-token      (getf (rest record) :id-token)
                           :account-id    account-id
                           :expires-at    (or (getf (rest record) :expires-at)
                                              (jwt-expiration access-token))
                           :source-path   pathname)))))))

(defmethod credential-source-load ((source codex-bootstrap-credential-source))
  "Load one non-renewable ChatGPT bootstrap credential without modifying Codex."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
          (let* ((document     (read-json-file-with-retry pathname))
                 (auth-mode    (json-get document "auth_mode"))
                 (tokens       (json-get document "tokens"))
                 (access-token (and (json-object-p tokens)
                                    (json-get tokens "access_token")))
                 (id-token     (and (json-object-p tokens)
                                    (json-get tokens "id_token")))
                 (account-id   (and (json-object-p tokens)
                                    (or (json-get tokens "account_id")
                                        (and id-token (jwt-account-id id-token))
                                        (and access-token (jwt-account-id access-token))))))
            (when (and (stringp auth-mode)
                       (string-equal auth-mode "chatgpt")
                       (non-empty-string-p access-token)
                       (non-empty-string-p account-id))
              (make-instance 'oauth-credentials
                             :access-token  access-token
                             :refresh-token nil
                             :id-token      nil
                             :account-id    account-id
                             :expires-at    (jwt-expiration access-token)
                             :source-path   pathname)))
        (error ()
          nil)))))

(defmethod credential-source-save ((source autolith-credential-source)
                                   (credentials oauth-credentials))
  "Atomically save CREDENTIALS to Autolith's private store with mode 0600."
  (let* ((pathname (credential-source-pathname source))
         (record   (list :oauth
                         :version 1
                         :access-token  (oauth-credentials-access-token credentials)
                         :refresh-token (oauth-credentials-refresh-token credentials)
                         :id-token      (oauth-credentials-id-token credentials)
                         :account-id    (oauth-credentials-account-id credentials)
                         :expires-at    (oauth-credentials-expires-at credentials))))
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
  (:documentation "Base credential manager for Autolith."))

(defclass oauth-credential-manager (credential-manager refresh-grant-credential-manager)
  ()
  (:documentation "Credential manager with OAuth refresh grants."))

(defmethod credential-manager-refresh-request :around
    ((manager oauth-credential-manager) &key url headers content)
  "Limit refresh requests to the response deadline and read streamed error bodies."
  (declare (ignore url headers content))
  (handler-case
      (multiple-value-bind (body status response-headers)
          (provider-call-with-response-deadline 60 (lambda () (call-next-method)))
        (values (if (stringp body)
                    body
                    (or (provider--error-body-text body) ""))
                status
                response-headers))
    (sb-sys:deadline-timeout ()
      (error 'token-refresh-failed
             :message  (format nil "~A OAuth token refresh exceeded its response deadline."
                               (credential-manager-provider-label manager))
             :status   nil
             :response nil))))

(defclass chatgpt-credential-manager (oauth-credential-manager)
  ()
  (:documentation "ChatGPT OAuth credential manager for the Codex provider."))

(defmethod credential-manager-provider-label
    ((manager chatgpt-credential-manager))
  "Name the ChatGPT account service in user-visible failures."
  (declare (ignore manager))
  "ChatGPT")

(defmethod credential-manager-login-hint ((manager chatgpt-credential-manager))
  "Return the default ChatGPT login command."
  (declare (ignore manager))
  "run autolith auth")

(-> credential-manager-create (configuration) chatgpt-credential-manager)
(defun credential-manager-create (configuration)
  "Create the ChatGPT credential manager for CONFIGURATION's private paths."
  (make-instance 'chatgpt-credential-manager
                 :primary-source   (make-instance
                                    'autolith-credential-source
                                    :pathname (configuration-auth-path configuration))
                 :bootstrap-source (make-instance
                                    'codex-bootstrap-credential-source
                                    :pathname (config :codex-auth-path configuration))))

(defmethod credential-manager-token-endpoint ((manager chatgpt-credential-manager))
  "Return OpenAI's OAuth token endpoint."
  (declare (ignore manager))
  *openai-oauth-token-endpoint*)

(defmethod credential-manager-client-id ((manager chatgpt-credential-manager))
  "Return the first-party Codex OAuth client identifier."
  (declare (ignore manager))
  *openai-oauth-client-id*)

(defmethod credential-manager-refresh-content-type ((manager chatgpt-credential-manager))
  "Return the content type for OpenAI's JSON refresh requests."
  (declare (ignore manager))
  *refresh-json-content-type*)

(defmethod credential-manager-refreshed-account-ids
    ((manager chatgpt-credential-manager) (document hash-table))
  "Return the ChatGPT accounts claimed by DOCUMENT's OpenID and access tokens."
  (declare (ignore manager))
  (loop for key in '("id_token" "access_token")
        for token = (json-get document key)
        for account-id = (and (non-empty-string-p token) (jwt-account-id token))
        when account-id
          collect account-id))

(defgeneric call-with-credentials (manager function &key force-refresh)
  (:documentation "Call FUNCTION inside the manager's credential request and secret scopes."))

(defmethod call-with-credentials :around
    ((manager credential-manager) function &key force-refresh)
  "Mark credential ownership for the complete load and request."
  (declare (ignore function force-refresh))
  (let ((*credentials-in-request-scope* t))
    (call-next-method)))

(defmethod call-with-credentials
    ((manager credential-manager) function &key force-refresh)
  "Use the OAuth manager's normal loading and refresh policy."
  (cl-rfc8628:call-with-credentials manager function :force-refresh force-refresh))

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
