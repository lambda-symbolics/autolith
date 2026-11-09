(in-package #:autolith)

;;;; -- Mistral API Provider --

(defclass mistral-api-key-provider (openai-compatible-provider)
  ()
  (:documentation
   "A static API key client for the Mistral Chat Completions API."))

(defmethod provider-account-label ((provider mistral-api-key-provider))
  "Name the Mistral account service in user-visible failures."
  (declare (ignore provider))
  "Mistral")

(defmethod provider-family ((provider mistral-api-key-provider))
  "Return the Mistral model family."
  (declare (ignore provider))
  ':mistral)

(defmethod openai-compatible-provider-output-ceiling-field
    ((provider mistral-api-key-provider))
  "Use Mistral's documented output limit field."
  (declare (ignore provider))
  "max_tokens")

(defmethod provider-family-create
    ((family (eql ':mistral))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Mistral API key provider."
  (declare (ignore family reasoning-summaries-p))
  (mistral-provider-create configuration))

(-> mistral-provider-create (configuration) mistral-api-key-provider)
(defun mistral-provider-create (configuration)
  "Create the Mistral API key provider for CONFIGURATION."
  (make-instance
   'mistral-api-key-provider
   :configuration      configuration
   :credential-manager (mistral-credential-manager-create configuration)
   :session-id         (make-identifier)
   :display-name       "Mistral"
   :family             ':mistral))

(defmethod provider-authenticate ((provider mistral-api-key-provider)
                                  &key stream open-browser-p)
  "Prompt for, validate, and save the Mistral API key."
  (declare (ignore open-browser-p))
  (mistral-api-key-login (provider-credential-manager provider)
                         :stream (or stream *standard-output*)))


;;;; -- Model Discovery --

(-> mistral--chat-model-p (json-object) boolean)
(defun mistral--chat-model-p (entry)
  "Return true when Mistral model ENTRY supports Chat Completions."
  (let ((capabilities (json-get entry "capabilities")))
    (unless (json-object-p capabilities)
      (error 'configuration-error
             :message
             "The Mistral model response omitted a capabilities object."))
    (multiple-value-bind (completion-chat present-p)
        (json-get-present capabilities "completion_chat")
      (unless (and present-p
                   (or (eq completion-chat t) (null completion-chat)))
        (error 'configuration-error
               :message
               "The Mistral model response contained an invalid chat capability."))
      (if completion-chat t nil))))

(-> mistral--fetch-models (configuration) list)
(defun mistral--fetch-models (configuration)
  "Discover Mistral models that support Chat Completions."
  (openai-compatible--fetch-models
   configuration
   :provider-name      "Mistral"
   :endpoint           (mistral-models-endpoint)
   :credential-manager (mistral-credential-manager-create configuration)
   :entry-predicate    #'mistral--chat-model-p))
