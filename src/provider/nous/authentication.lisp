(in-package #:autolith)

;;;; -- Nous OAuth Credential Management --

(defclass nous-credential-manager (oauth-credential-manager)
  ((refresh-request-function
    :initarg       :refresh-request-function
    :initform      nil
    :reader        nous-credential-manager-refresh-request-function
    :type          (option function)
    :documentation "An injected HTTP request function replacing the refresh transport, if any."))
  (:documentation "The OAuth credential manager for Nous Research inference."))


;;;; -- Store Lock --

(-> nous-authentication--lock-pathname (pathname) pathname)
(defun nous-authentication--lock-pathname (credential-pathname)
  "Return the process-shared lock pathname for CREDENTIAL-PATHNAME."
  (merge-pathnames
   "nous-auth.lock"
   (uiop:pathname-directory-pathname credential-pathname)))

(-> nous-authentication--call-with-store-lock (pathname function) t)
(defun nous-authentication--call-with-store-lock (credential-pathname function)
  "Call FUNCTION while holding the process and filesystem Nous OAuth locks."
  (let ((lock-pathname
          (nous-authentication--lock-pathname credential-pathname)))
    (handler-case
        (call-with-file-lock lock-pathname function)
      (authentication-error (condition)
        (error condition))
      (error (cause)
        (error 'authentication-error
               :message
               (format nil "Could not lock the Nous OAuth store at ~A: ~A"
                       lock-pathname cause))))))


;;;; -- Access Token Validation --

(-> nous-authentication--scope-values (t) list)
(defun nous-authentication--scope-values (value)
  "Return normalized OAuth scope strings represented by VALUE."
  (labels ((collect-scopes (candidate)
             (cond
               ((stringp candidate)
                (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string
                  candidate
                  :separator '(#\Space #\Tab #\Newline #\Return #\,))))
               ((vectorp candidate)
                (loop for item across candidate
                      append (collect-scopes item)))
               ((listp candidate)
                (loop for item in candidate
                      append (collect-scopes item)))
               (t
                nil))))
    (remove-duplicates (collect-scopes value) :test #'string=)))

(-> nous-authentication--access-token-scope-p (string string) boolean)
(defun nous-authentication--access-token-scope-p (access-token required-scope)
  "Return true when ACCESS-TOKEN is a JWT carrying REQUIRED-SCOPE."
  (let ((payload (jwt-payload access-token)))
    (if (and payload
             (member required-scope
                     (append
                      (nous-authentication--scope-values
                       (json-get payload "scope"))
                      (nous-authentication--scope-values
                       (json-get payload "scp")))
                     :test #'string=))
        t
        nil)))

(-> nous-authentication--access-token-account-id (string) (option string))
(defun nous-authentication--access-token-account-id (access-token)
  "Return the stable subject carried by a valid Nous access JWT."
  (jwt-subject access-token))

(-> nous-authentication--validate-stored-credentials
    (nous-credential-manager oauth-credentials)
    oauth-credentials)
(defun nous-authentication--validate-stored-credentials (manager credentials)
  "Validate stored Nous CREDENTIALS before returning them to request scope."
  (let* ((access-token (oauth-credentials-access-token credentials))
         (token-account
           (nous-authentication--access-token-account-id access-token)))
    (unless (and (jwt-payload access-token)
                 (nous-authentication--access-token-scope-p
                  access-token
                  *nous-oauth-scope*)
                 (non-empty-string-p token-account)
                 (string= token-account
                          (oauth-credentials-account-id credentials)))
      (error 'credentials-unavailable
             :message
             (format nil
                     "The stored Nous OAuth credentials cannot invoke inference; ~A."
                     (credential-manager-login-hint manager))
             :searched-paths
             (list (oauth-credentials-source-path credentials))))
    credentials))


;;;; -- Credential Manager Protocol --

(defmethod credential-manager-provider-label ((manager nous-credential-manager))
  "Name the Nous Research account service in user-visible failures."
  (declare (ignore manager))
  "Nous Research")

(defmethod credential-manager-login-hint ((manager nous-credential-manager))
  "Point Nous credential failures at the browser login command."
  (declare (ignore manager))
  "run autolith auth nous")

(-> nous-credential-manager-create
    (configuration &key (:refresh-request-function (option function)))
    nous-credential-manager)
(defun nous-credential-manager-create (configuration &key refresh-request-function)
  "Create a Nous credential manager for CONFIGURATION's private state root."
  (make-instance
   'nous-credential-manager
   :primary-source
   (make-instance
    'autolith-credential-source
    :pathname (configuration-nous-auth-path configuration))
   :refresh-request-function refresh-request-function))

(defmethod credential-manager-call-with-refresh-lock
    ((manager nous-credential-manager) (function function))
  "Serialize Nous token rotation and login publication across processes.

Refresh tokens are single-use. Reload the credentials while holding the
filesystem lock before each exchange."
  (nous-authentication--call-with-store-lock
   (credential-source-pathname (credential-manager-primary-source manager))
   function))

(defmethod credential-manager-validate-credentials
    ((manager nous-credential-manager) (credentials oauth-credentials))
  "Require stored Nous credentials to carry the inference scope for their account."
  (nous-authentication--validate-stored-credentials manager credentials))

(defmethod credential-manager-load ((manager nous-credential-manager))
  "Load only Autolith-owned Nous credentials under the shared store lock."
  (credential-manager-call-with-refresh-lock manager (lambda () (call-next-method))))


;;;; -- Refresh Exchange --

(defmethod credential-manager-token-endpoint ((manager nous-credential-manager))
  "Return the Nous Portal OAuth token endpoint."
  (declare (ignore manager))
  (concatenate 'string (nous-portal-url) "/api/oauth/token"))

(defmethod credential-manager-client-id ((manager nous-credential-manager))
  "Return the Hermes CLI OAuth client ID."
  (declare (ignore manager))
  *nous-oauth-client-id*)

(defmethod credential-manager-refresh-parameters
    ((manager nous-credential-manager) (refresh-token string))
  "Keep the refresh token out of the form, since Nous reads it from a header."
  (remove "refresh_token" (call-next-method) :key #'first :test #'string=))

(defmethod credential-manager-refresh-headers
    ((manager nous-credential-manager) (refresh-token string))
  "Send the single-use refresh token in the header Nous reads it from."
  (declare (ignore manager))
  (list (cons "x-nous-refresh-token" refresh-token)))

(defmethod credential-manager-refresh-request
    ((manager nous-credential-manager) &key url headers content)
  "POST through the injected request function when one replaces the transport."
  (let ((function (nous-credential-manager-refresh-request-function manager)))
    (if function
        (funcall function :method ':post :url url :headers headers :content content)
        (call-next-method))))

(defmethod credential-manager-validate-refresh-response
    ((manager nous-credential-manager) (document hash-table) (credentials oauth-credentials))
  "Require a rotated single-use refresh token and a scoped access JWT naming its subject."
  (declare (ignore manager))
  (let ((access-token  (json-get document "access_token"))
        (refresh-token (json-get document "refresh_token")))
    (unless (and (non-empty-string-p refresh-token)
                 (not (string= refresh-token (oauth-credentials-refresh-token credentials))))
      (error 'token-refresh-failed
             :message  "The Nous OAuth refresh response omitted rotated credentials."
             :status   nil
             :response nil))
    (unless (nous-authentication--access-token-scope-p access-token *nous-oauth-scope*)
      (error 'token-refresh-failed
             :message  "The refreshed Nous access token lacks the inference:invoke scope."
             :status   nil
             :response nil))
    (unless (non-empty-string-p (nous-authentication--access-token-account-id access-token))
      (error 'token-refresh-failed
             :message  "The refreshed Nous access token omitted its subject."
             :status   nil
             :response nil))))
