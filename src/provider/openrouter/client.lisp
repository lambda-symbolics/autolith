(in-package #:autolith)

;;;; -- OpenRouter Chat Completions Provider --

(defparameter *openrouter-model-prefix* "openrouter/"
  "The namespace distinguishing OpenRouter models from other providers.")

(defparameter *openrouter-request-headers*
  '(("HTTP-Referer" . "https://github.com/lambda-symbolics/autolith")
    ("X-OpenRouter-Title" . "Autolith"))
  "The non-secret OpenRouter attribution headers sent with provider requests.")

(-> openrouter--model-name (non-empty-string) non-empty-string)
(defun openrouter--model-name (wire-name)
  "Return the user-visible OpenRouter model name for WIRE-NAME."
  (format nil "~A~A" *openrouter-model-prefix* wire-name))

(-> openrouter--wire-model-name (non-empty-string) non-empty-string)
(defun openrouter--wire-model-name (model)
  "Return the OpenRouter wire identifier encoded by namespaced MODEL."
  (unless (and (uiop:string-prefix-p *openrouter-model-prefix* model)
               (< (length *openrouter-model-prefix*) (length model)))
    (error 'configuration-error
           :message
           (format nil "OpenRouter model identifiers must begin with ~A, not ~S."
                   *openrouter-model-prefix*
                   model)))
  (subseq model (length *openrouter-model-prefix*)))

(-> openrouter--string-member-p (string t) boolean)
(defun openrouter--string-member-p (needle values)
  "Return true when sequence VALUES contains string NEEDLE."
  (if (and (typep values 'sequence)
           (find needle values
                 :test (lambda (left right)
                         (and (stringp left)
                              (stringp right)
                              (string= left right)))))
      t
      nil))

(-> openrouter--chat-tool-model-p (json-object) boolean)
(defun openrouter--chat-tool-model-p (entry)
  "Return true when OpenRouter model ENTRY accepts Autolith's request shape."
  (let* ((architecture (json-get entry "architecture"))
         (output-modalities
           (and (json-object-p architecture)
                (json-get architecture "output_modalities")))
         (parameters (json-get entry "supported_parameters"))
         (supports-reasoning-p
           (openrouter--string-member-p "reasoning" parameters))
         (model-id (json-get entry "id")))
    (unless supports-reasoning-p
      (setf (gethash (openrouter--model-name model-id) *openrouter-models-without-reasoning*) t))
    (if (and (vectorp output-modalities)
             (vectorp parameters)
             (openrouter--string-member-p "text" output-modalities)
             (openrouter--string-member-p "tools" parameters)
             (openrouter--string-member-p "tool_choice" parameters))
        t
        nil)))

(-> openrouter--fetch-models (configuration) list)
(defun openrouter--fetch-models (configuration)
  "Discover and namespace OpenRouter text models with tools and reasoning."
  (mapcar #'openrouter--model-name
          (openai-compatible--fetch-models
           configuration
           :provider-name "OpenRouter"
           :endpoint (openrouter-models-endpoint)
           :headers *openrouter-request-headers*
           :credential-manager
           (openrouter-credential-manager-create configuration)
           :entry-predicate #'openrouter--chat-tool-model-p)))


;;;; -- Provider Lifecycle --

(defclass openrouter-chat-completions-provider (openai-compatible-provider)
  ()
  (:documentation
   "A static API key client for OpenRouter's Chat Completions API."))

(defmethod openai-compatible-provider-output-ceiling-field
    ((provider openrouter-chat-completions-provider))
  "Use the output limit field accepted across OpenRouter upstream providers."
  (declare (ignore provider))
  "max_tokens")

(defmethod provider-family-create
    ((family (eql ':openrouter))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the OpenRouter Chat Completions provider."
  (declare (ignore family reasoning-summaries-p))
  (openrouter-provider-create configuration))

(-> openrouter-provider-create (configuration) openrouter-chat-completions-provider)
(defun openrouter-provider-create (configuration)
  "Create the OpenRouter API key provider for CONFIGURATION."
  (make-instance
   'openrouter-chat-completions-provider
   :configuration configuration
   :credential-manager (openrouter-credential-manager-create configuration)
   :session-id (make-identifier)
   :display-name "OpenRouter"
   :family ':openrouter
   :headers *openrouter-request-headers*))

(-> openrouter--reasoning-effort (string) (option string))
(defun openrouter--reasoning-effort (effort)
  "Translate Autolith reasoning EFFORT to OpenRouter's normalized values."
  (cond
    ((string= effort "none")
     nil)
    ((string= effort "ultra")
     "max")
    (t
     effort)))

(defparameter *openrouter-models-without-reasoning* (make-hash-table :test #'equal)
  "Cached set of OpenRouter models that lack reasoning support.")

(-> openrouter--model-supports-reasoning-p (non-empty-string) boolean)
(defun openrouter--model-supports-reasoning-p (model-id)
  "Check if MODEL-ID supports reasoning; assumes yes unless known otherwise."
  (not (gethash model-id *openrouter-models-without-reasoning*)))

(defmethod provider-request-object :around
    ((provider openrouter-chat-completions-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Translate the namespaced model and normalized reasoning controls for OpenRouter."
  (multiple-value-bind (request delivery)
      (call-next-method provider conversation tool-namespaces
                        :goal-context goal-context
                        :compaction-p compaction-p)
    (let* ((model (json-get request "model"))
           (effort
            (openrouter--reasoning-effort
             (configuration-reasoning-effort
              (provider-configuration provider))))
           (model-supports-reasoning-p
            (openrouter--model-supports-reasoning-p model)))
      (setf (gethash "model" request)
            (openrouter--wire-model-name model))
      (when (and effort model-supports-reasoning-p)
        (setf (gethash "reasoning" request)
              (json-object "effort" effort))))
    (values request delivery)))

(defmethod provider-authenticate
    ((provider openrouter-chat-completions-provider)
     &key stream open-browser-p)
  "Prompt for, validate, and save the OpenRouter API key."
  (declare (ignore open-browser-p))
  (openrouter-api-key-login (provider-credential-manager provider)
                            :stream (or stream *standard-output*)))
