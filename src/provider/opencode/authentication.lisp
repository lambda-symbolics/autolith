(in-package #:autolith)

;;;; -- OpenCode API Key Authentication --

;;; OpenCode authenticates with a static account API key sent as a bearer
;;; token. The key rides in the generic oauth-credentials access-token slot so
;;; transport and redaction treat it exactly like any other provider credential.
;;; Keys carry no expiry, so Autolith never attempts a refresh; a rejected key
;;; fails as unauthorized and the user re-runs `autolith auth opencode`.

(defparameter *opencode-account-label* "opencode"
  "The synthetic account identifier pinned for static OpenCode API keys.")

(defparameter *opencode-environment-variable* "OPENCODE_API_KEY"
  "The environment variable holding the OpenCode account API key.")

(defclass opencode-environment-credential-source
    (environment-api-key-credential-source)
  ()
  (:default-initargs
   :environment-variable *opencode-environment-variable*
   :account-id *opencode-account-label*)
  (:documentation
   "A read-only adapter loading the OpenCode API key from the environment."))


;;;; -- OpenCode Credential Manager --

(defclass opencode-credential-manager (static-api-key-credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the OpenCode provider."))

(defmethod credential-manager-provider-label
    ((manager opencode-credential-manager))
  "Name the OpenCode account service in user-visible failures."
  (declare (ignore manager))
  "OpenCode")

(defmethod credential-manager-login-hint
    ((manager opencode-credential-manager))
  "Point OpenCode credential failures at the OpenCode login command."
  (declare (ignore manager))
  "run autolith auth opencode")

(-> opencode-credential-manager-create (configuration) opencode-credential-manager)
(defun opencode-credential-manager-create (configuration)
  "Create the OpenCode credential manager for CONFIGURATION's private paths."
  (make-instance 'opencode-credential-manager
                 :primary-source
                 (make-instance
                  'autolith-credential-source
                  :pathname (configuration-opencode-auth-path configuration))
                 :bootstrap-source
                 (make-instance 'opencode-environment-credential-source)))


;;;; -- OpenCode API Key Login --

(-> opencode-api-key-login
    (opencode-credential-manager &key
                                 (:stream stream)
                                 (:input stream)
                                 (:input-file-descriptor (option integer)))
    string)
(defun opencode-api-key-login
    (manager
     &key
       (stream *standard-output*)
       (input *standard-input*)
       (input-file-descriptor
         (and (eq input *standard-input*)
              *api-key-input-file-descriptor*)))
  "Prompt for an OpenCode API key and save it to MANAGER's private store.

OpenCode's model-list endpoint is public, so login cannot validate a key without
issuing a real chat request. A rejected key therefore fails on its first provider
request with the normal actionable static-key authentication error."
  (api-key-login manager
                 :stream stream
                 :input input
                 :input-file-descriptor input-file-descriptor))
