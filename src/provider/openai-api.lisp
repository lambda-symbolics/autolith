(in-package #:autolith)

;;;; -- Official OpenAI API --

(defclass openai-api-provider
    (session-preserving-provider-mixin responses-api-provider)
  ()
  (:documentation "The public OpenAI Responses API with private API-key authentication."))

(defmethod provider-account-label ((provider openai-api-provider))
  "Identify the private API-key service, independently of ChatGPT subscriptions."
  (declare (ignore provider))
  "openai-api")

(defmethod provider-family ((provider openai-api-provider))
  "Project public API conversations as standard Responses items."
  (declare (ignore provider))
  ':openai-api)

(-> openai-api-provider-create (configuration) openai-api-provider)
(defun openai-api-provider-create (configuration)
  "Create the public API provider without loading secrets into provider state."
  (make-instance 'openai-api-provider
                 :configuration configuration
                 :credential-manager
                 (api-key-credential-manager-create
                  :provider-name "openai-api"
                  :pathname      (configuration-api-keys-path configuration))
                 :session-id    (make-identifier)))

(defmethod provider-authenticate ((provider openai-api-provider)
                                  &key stream open-browser-p)
  "Enter or replace the public API key in the private credential store."
  (openai-compatible--authenticate provider :stream        stream
                                           :open-browser-p open-browser-p))

(defmethod provider-output-ceiling-p ((provider openai-api-provider))
  "Accept the standard Responses maximum output token field."
  (declare (ignore provider))
  t)

(defmethod provider-responses-wire-effort
    ((provider openai-api-provider) (configuration configuration))
  "Use the model default when the API catalog advertises no reasoning controls."
  (declare (ignore provider configuration))
  nil)

(defmethod provider-responses-request-fields
    ((provider openai-api-provider) (conversation conversation) &key compaction-p)
  "Disable server-side response storage and allow parallel calls for public API requests.

Parallel calls let one response carry independent calls, which the agent runs
in waves, instead of spending a model request on each. Compaction calls none.
CONVERSATION's prompt cache key routes its requests to the same cache."
  (declare (ignore provider))
  (list "store" yason:false
        "parallel_tool_calls" (if compaction-p yason:false t)
        "prompt_cache_key" (conversation-prompt-cache-key conversation)))

(defmethod provider-wire-tool-name
    ((provider openai-api-provider) (namespace string) (name string))
  "Use the shared reversible codec for OpenAI's restricted function names."
  (declare (ignore provider))
  (openai-compatible--wire-tool-name namespace name))

(defmethod provider-wire-tool
    ((provider openai-api-provider) (namespace string) (tool hash-table))
  "Project only function parameter roots, retaining the Responses envelope."
  (let ((wire (call-next-method)))
    (setf (gethash "parameters" wire)
          (cl-llm-provider-api:openai-compatible--wire-parameters
           (json-get tool "parameters")))
    wire))

(defmethod provider-normalize-output-item
    ((provider openai-api-provider) (item hash-table))
  "Decode completed function names for local dispatch and portable history."
  (call-next-method)
  (when (clinker-transcript:function-call-item-p item)
    (multiple-value-bind (namespace name)
        (openai-compatible--decode-wire-tool-name (json-get item "name"))
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name))))
  item)

(defmethod provider-open-response-stream
    ((provider openai-api-provider) (request hash-table)
     &key credentials conversation)
  "Send a public API request with bearer authentication and no subscription headers."
  (declare (ignore conversation))
  (provider-post-event-stream
   (config :provider-endpoint (provider-configuration provider))
   (json-encode-utf8 request)
   :headers (openai-compatible--authenticated-headers
             credentials :accept       "text/event-stream"
                         :content-type "application/json")))

(-> openai-api--registration-factory
    (configuration &key (:reasoning-summaries-p boolean)) openai-api-provider)
(defun openai-api--registration-factory (configuration &key reasoning-summaries-p)
  "Construct a public API client from registration metadata."
  (declare (ignore reasoning-summaries-p))
  (openai-api-provider-create configuration))

(-> openai-api--fetch-models (configuration) list)
(defun openai-api--fetch-models (configuration)
  "Discover models using the public API key and standard models endpoint."
  (openai-compatible--fetch-models
   configuration :provider-name "openai-api"
                 :endpoint      "https://api.openai.com/v1/models"))
