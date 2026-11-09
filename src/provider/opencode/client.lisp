(in-package #:autolith)

;;;; -- OpenCode Chat Completions Provider --

(defparameter *opencode-model-prefix* "opencode/"
  "Prefix for OpenCode Go model names.")

(-> opencode--model-name (string &optional keyword) non-empty-string)
(defun opencode--model-name (wire-name &optional (family ':opencode))
  "Prefix WIRE-NAME with FAMILY's model namespace."
  (concatenate 'string (ecase family
                         (:opencode
                          *opencode-model-prefix*)
                         (:opencode-zen
                          "opencode-zen/"))
               wire-name))

(-> opencode--wire-model-name (non-empty-string &optional keyword) non-empty-string)
(defun opencode--wire-model-name (model &optional (family ':opencode))
  "Return MODEL without FAMILY's namespace prefix."
  (let ((prefix (opencode--model-name "" family)))
    (unless (and (uiop:string-prefix-p prefix model)
                 (< (length prefix) (length model)))
      (error 'configuration-error
             :message (format nil "OpenCode model identifiers must begin with ~A, not ~S."
                              prefix model)))
    (subseq model (length prefix))))

(-> opencode--discover-models (configuration keyword) list)
(defun opencode--discover-models (configuration family)
  "Discover models using FAMILY's endpoint, credentials and namespace."
  (let ((zen-p (eq family ':opencode-zen)))
    (mapcar (lambda (spec)
              (model-spec-rename spec (opencode--model-name (model-spec-name spec) family)))
            (openai-compatible--fetch-models
             configuration
             :provider-name      (if zen-p "opencode-zen" "opencode-go")
             :endpoint           (if zen-p (opencode-zen-models-endpoint) (opencode-models-endpoint))
             :credential-manager (if zen-p
                                     (opencode-zen-credential-manager-create configuration)
                                     (opencode-credential-manager-create configuration))))))

(-> opencode--fetch-models (configuration) list)
(defun opencode--fetch-models (configuration)
  "Discover OpenCode Go models in the opencode/ namespace."
  (opencode--discover-models configuration ':opencode))

(-> opencode-zen--fetch-models (configuration) list)
(defun opencode-zen--fetch-models (configuration)
  "Discover OpenCode Zen models in the opencode-zen/ namespace."
  (opencode--discover-models configuration ':opencode-zen))

(defclass opencode-chat-completions-provider (openai-compatible-provider)
  ()
  (:documentation
   "A static API key client for the OpenCode Chat Completions API."))

(defmethod provider-family-create
    ((family (eql ':opencode))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the OpenCode Chat Completions provider."
  (declare (ignore reasoning-summaries-p))
  (opencode-provider-create configuration))

(defmethod provider-family-create
    ((family (eql ':opencode-zen))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the OpenCode Zen Chat Completions provider."
  (declare (ignore reasoning-summaries-p))
  (opencode-provider-create configuration :family family))

(-> opencode-provider-create
    (configuration &key (:family keyword)) opencode-chat-completions-provider)
(defun opencode-provider-create (configuration &key (family ':opencode))
  "Create an OpenCode client with a stable session header for caching."
  (let ((session-id (make-identifier))
        (zen-p      (eq family ':opencode-zen)))
    (make-instance 'opencode-chat-completions-provider
                   :configuration      configuration
                   :credential-manager (if zen-p
                                           (opencode-zen-credential-manager-create configuration)
                                           (opencode-credential-manager-create configuration))
                   :session-id         session-id
                   :headers            (list (cons "x-opencode-session" session-id))
                   :display-name       (if zen-p "OpenCode Zen" "OpenCode Go")
                   :family             family)))

(defmethod provider-request-object :around
    ((provider opencode-chat-completions-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Translate the namespaced OpenCode model to its raw wire identifier."
  (multiple-value-bind (request delivery)
      (call-next-method provider conversation tool-namespaces
                        :goal-context goal-context
                        :compaction-p compaction-p)
    (setf (gethash "model" request)
          (opencode--wire-model-name (json-get request "model") (provider-family provider)))
    (values request delivery)))

(defmethod provider-authenticate ((provider opencode-chat-completions-provider)
                                  &key stream open-browser-p)
  "Prompt for and save the OpenCode API key."
  (declare (ignore open-browser-p))
  (funcall (ecase (provider-family provider)
             (:opencode
              #'opencode-api-key-login)
             (:opencode-zen
              #'opencode-zen-api-key-login))
           (provider-credential-manager provider)
           :stream (or stream *standard-output*)))
