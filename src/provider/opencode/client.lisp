(in-package #:autolith)

;;;; -- OpenCode Chat Completions Provider --

;;; The OpenCode provider serves the standard streaming OpenAI Chat
;;; Completions dialect via the opencode.ai/zen/go/v1 endpoint. It
;;; participates in dynamic model discovery and persists credentials in
;;; Autolith's private store with an optional OPENCODE_API_KEY environment
;;; bootstrap. Tool names ride in the Chat Completions Base64 encoding
;;; inherited from OPENAI-COMPATIBLE-PROVIDER, so conversations persist in
;;; the same namespaced shape regardless of the serving provider. Every
;;; request also carries the provider's stable session identifier in the
;;; x-opencode-session header so the backend can pin caching.

(defparameter *opencode-model-prefix* "opencode/"
  "The namespace distinguishing OpenCode models from other provider models.")

(-> opencode--model-name (non-empty-string) non-empty-string)
(defun opencode--model-name (wire-name)
  "Return the user-visible OpenCode model name for WIRE-NAME."
  (format nil "~A~A" *opencode-model-prefix* wire-name))

(-> opencode--wire-model-name (non-empty-string) non-empty-string)
(defun opencode--wire-model-name (model)
  "Return the OpenCode wire identifier encoded by namespaced MODEL."
  (unless (and (uiop:string-prefix-p *opencode-model-prefix* model)
               (< (length *opencode-model-prefix*) (length model)))
    (error 'configuration-error
           :message
           (format nil "OpenCode model identifiers must begin with ~A, not ~S."
                   *opencode-model-prefix*
                   model)))
  (subseq model (length *opencode-model-prefix*)))

(-> opencode--fetch-models (configuration) list)
(defun opencode--fetch-models (configuration)
  "Discover OpenCode models and namespace their user-visible identifiers."
  (mapcar #'opencode--model-name
          (openai-compatible--fetch-models
           configuration
           :provider-name "opencode"
           :endpoint (opencode-models-endpoint)
           :credential-manager
           (opencode-credential-manager-create configuration))))

(defclass opencode-chat-completions-provider (openai-compatible-provider)
  ()
  (:documentation
   "A static API key client for the OpenCode Chat Completions API."))

(defmethod provider-family-create
    ((family (eql ':opencode))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the OpenCode Chat Completions provider; summaries always stream when served."
  (declare (ignore reasoning-summaries-p))
  (opencode-provider-create configuration))

(-> opencode-provider-create (configuration) opencode-chat-completions-provider)
(defun opencode-provider-create (configuration)
  "Create the OpenCode API key provider for CONFIGURATION.

The provider's stable session identifier also rides as the x-opencode-session
request header so the backend can pin caching to one session."
  (let ((session-id (make-identifier)))
    (make-instance 'opencode-chat-completions-provider
                   :configuration configuration
                   :credential-manager (opencode-credential-manager-create
                                        configuration)
                   :session-id session-id
                   :headers (list (cons "x-opencode-session" session-id))
                   :display-name "OpenCode"
                   :family ':opencode)))

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
          (opencode--wire-model-name (json-get request "model")))
    (values request delivery)))

(defmethod provider-authenticate ((provider opencode-chat-completions-provider)
                                  &key stream open-browser-p)
  "Prompt for and save the OpenCode API key."
  (declare (ignore open-browser-p))
  (opencode-api-key-login (provider-credential-manager provider)
                          :stream (or stream *standard-output*)))

