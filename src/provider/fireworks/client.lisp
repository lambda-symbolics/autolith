(in-package #:autolith)

;;;; -- Fireworks API Key Provider --

(defclass fireworks-api-key-provider
    (session-preserving-provider-mixin responses-api-provider)
  ()
  (:documentation
   "A static API key client for the Fireworks Responses API."))

(defmethod provider-account-label ((provider fireworks-api-key-provider))
  "Name the Fireworks account service in user-visible failures."
  (declare (ignore provider))
  "Fireworks")

(defmethod provider-family ((provider fireworks-api-key-provider))
  "Return the Fireworks model family."
  (declare (ignore provider))
  ':fireworks)

(defmethod provider-family-create
    ((family (eql ':fireworks))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Fireworks API key provider."
  (declare (ignore reasoning-summaries-p))
  (fireworks-provider-create configuration))

(-> fireworks-provider-create (configuration) fireworks-api-key-provider)
(defun fireworks-provider-create (configuration)
  "Create the Fireworks API key provider for CONFIGURATION."
  (make-instance 'fireworks-api-key-provider
                 :configuration      configuration
                 :credential-manager (fireworks-credential-manager-create
                                      configuration)
                 :session-id         (make-identifier)))


;;;; -- Fireworks Protocol Specializations --

(defparameter *fireworks-reasoning-effort-models-blacklist*
  '("accounts/fireworks/models/qwen3p7-plus")
  "Fireworks models that reject reasoning parameters. Omit the reasoning object
for these models; see PROVIDER-RESPONSES-WIRE-EFFORT.")

(-> fireworks-model-reasoning-effort-p (string) boolean)
(defun fireworks-model-reasoning-effort-p (model)
  "Return true when MODEL accepts a reasoning effort parameter."
  (not (member model *fireworks-reasoning-effort-models-blacklist*
               :test #'string=)))

(defmethod provider-responses-wire-effort
    ((provider fireworks-api-key-provider) (configuration configuration))
  "Return CONFIGURATION's Fireworks reasoning effort, or NIL to omit it."
  (declare (ignore provider))
  (when (fireworks-model-reasoning-effort-p
         (config :model configuration))
    (configuration-fireworks-wire-effort configuration)))

(defmethod provider-responses-request-fields
    ((provider fireworks-api-key-provider)
     (conversation conversation)
     &key compaction-p)
  "Add CONVERSATION's stable Fireworks prompt cache key."
  (declare (ignore provider compaction-p))
  (list "prompt_cache_key" (conversation-prompt-cache-key conversation)))


;;;; -- Fireworks Transport --

(defmethod provider-open-response-stream
    ((provider fireworks-api-key-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the Fireworks Responses API."
  (declare (type oauth-credentials credentials)
           (type conversation conversation)
           (ignorable conversation))
  (provider-post-event-stream
   (config :provider-endpoint (provider-configuration provider))
   (json-encode-utf8 request)
   :headers (list
             (cons "Authorization"
                   (format nil "Bearer ~A"
                           (oauth-credentials-access-token credentials)))
             (cons "Content-Type" "application/json")
             (cons "Accept" "text/event-stream")
             (cons "User-Agent" (provider-user-agent)))))
