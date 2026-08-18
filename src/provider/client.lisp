(in-package #:autolith)

;;;; -- Provider Events --

(defclass provider-event ()
  ()
  (:documentation "A semantic event emitted while consuming a provider stream."))

(defclass provider-progress-event (provider-event)
  ()
  (:documentation
   "Provider activity with no assistant text or completed item to present."))

(defclass assistant-delta-event (provider-event)
  ((text
    :initarg :text
    :reader assistant-delta-event-text
    :type string
    :documentation "The newly received assistant text."))
  (:documentation "An incremental assistant text update."))

(defclass reasoning-delta-event (provider-event)
  ((text
    :initarg :text
    :reader reasoning-delta-event-text
    :type string
    :documentation "The newly received visible reasoning summary text."))
  (:documentation "An incremental visible reasoning summary update."))

(defclass provider-item-event (provider-event)
  ((item
    :initarg :item
    :reader provider-item-event-item
    :type json-object
    :documentation "The authoritative completed Responses item."))
  (:documentation "A completed provider output item ready for persistence."))

(defclass provider-completed-event (provider-event)
  ((response-id
    :initarg :response-id
    :reader provider-completed-event-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (usage
    :initarg :usage
    :reader provider-completed-event-usage
    :type t
    :documentation "Portable provider usage metadata, if supplied.")
   (turn-completion
    :initarg :turn-completion
    :initform ':unspecified
    :reader provider-completed-event-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The successful terminal event for one provider request."))

(defclass provider-retry-event (provider-event)
  ((attempt
    :initarg :attempt
    :reader provider-retry-event-attempt
    :type (integer 1)
    :documentation "The one-based reconnect attempt about to begin.")
   (maximum-attempts
    :initarg :maximum-attempts
    :reader provider-retry-event-maximum-attempts
    :type (integer 1)
    :documentation "The maximum number of reconnect attempts allowed.")
   (delay
    :initarg :delay
    :reader provider-retry-event-delay
    :type real
    :documentation "Seconds to wait before reconnecting."))
  (:documentation "A transient provider stream is about to be retried."))

(defclass provider-result ()
  ((response-id
    :initarg :response-id
    :reader provider-result-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (output-items
    :initarg :output-items
    :reader provider-result-output-items
    :type list
    :documentation "Authoritative completed response items in wire order.")
   (tool-calls
    :initarg :tool-calls
    :reader provider-result-tool-calls
    :type list
    :documentation "The function-call subset of OUTPUT-ITEMS.")
   (usage
    :initarg :usage
    :reader provider-result-usage
    :type t
    :documentation "Provider usage metadata, if supplied.")
   (turn-state
    :initarg :turn-state
    :reader provider-result-turn-state
    :type (option string)
    :documentation "The routing token to replay within the current user turn.")
   (turn-completion
    :initarg :turn-completion
    :initform ':unspecified
    :reader provider-result-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The complete semantic result of one streamed provider request."))


;;;; -- Provider Protocol --

(defclass model-provider ()
  ((registration
    :initarg :registration
    :initform nil
    :accessor model-provider-registration
    :type (option provider-registration)
    :documentation "The registry metadata that created this provider."))
  (:documentation "The abstract interface between an agent and a model service."))

(defclass subscription-provider (model-provider)
  ((configuration
    :initarg :configuration
    :reader provider-configuration
    :type configuration
    :documentation "Immutable model and path configuration.")
   (credential-manager
    :initarg :credential-manager
    :reader provider-credential-manager
    :type credential-manager
    :documentation "Credential paths and refresh policy without retained tokens.")
   (session-id
    :initarg :session-id
    :reader provider-session-id
    :type non-empty-string
    :documentation "The stable provider session identifier."))
  (:documentation
   "A direct OAuth subscription client streaming Responses service turns."))

(defclass codex-subscription-provider (subscription-provider)
  ((reasoning-summaries-p
    :initarg :reasoning-summaries-p
    :initform nil
    :accessor provider-reasoning-summaries-p
    :type boolean
    :documentation "Whether requests opt in to provider-visible reasoning summaries.")
   (rate-limits
    :initform nil
    :accessor provider-rate-limits
    :type list
    :documentation "The most recent portable rate limit snapshot from response headers."))
  (:documentation "A direct ChatGPT subscription client for the Codex Responses service."))

(-> provider-account-label (model-provider) string)
(defgeneric provider-account-label (provider)
  (:documentation "Return the short user-visible name of PROVIDER's account service."))

(defmethod provider-account-label ((provider model-provider))
  "Return the registered provider name for a provider without a custom label."
  (or (and (model-provider-registration provider)
           (provider-registration-name (model-provider-registration provider)))
      "provider"))

(defmethod provider-account-label ((provider codex-subscription-provider))
  "Name the ChatGPT account service in user-visible failures."
  (declare (ignore provider))
  "ChatGPT")

(-> provider-authenticate
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defgeneric provider-authenticate (provider &key stream open-browser-p)
  (:documentation
   "Authenticate PROVIDER and return a safe user-visible completion message."))

(defmethod provider-authenticate :around
    ((provider model-provider) &key stream open-browser-p)
  "Give a registered provider authenticator precedence over protocol defaults."
  (let ((authenticator
          (and (model-provider-registration provider)
               (provider-registration-authenticator
                (model-provider-registration provider)))))
    (if authenticator
        (funcall authenticator provider
                 :stream stream
                 :open-browser-p open-browser-p)
        (call-next-method))))

(defmethod provider-authenticate ((provider model-provider)
                                  &key stream open-browser-p)
  "Reject authentication for a provider without an authentication protocol."
  (declare (ignore stream open-browser-p))
  (error 'authentication-error
         :message
         (format nil
                 "The ~A provider does not expose an authentication operation."
                 (provider-account-label provider))))

(defmethod provider-authenticate ((provider subscription-provider)
                                  &key stream open-browser-p)
  "Run the device login protocol for a subscription provider."
  (device-authentication-login
   (provider-device-authentication-client provider)
   (provider-credential-manager provider)
   :stream (or stream *standard-output*)
   :open-browser-p open-browser-p)
  (format nil "~A authentication was saved by Autolith."
          (provider-account-label provider)))

(-> provider-note-response-headers (subscription-provider t) t)
(defgeneric provider-note-response-headers (provider headers)
  (:documentation
   "Record portable metadata carried by sanitized response HEADERS."))

(defmethod provider-note-response-headers
    ((provider subscription-provider) (headers t))
  "Ignore response headers for providers without portable metadata."
  (declare (ignore provider headers))
  nil)

(defmethod provider-note-response-headers
    ((provider codex-subscription-provider) (headers t))
  "Record the subscription rate limit snapshot from Codex HEADERS."
  (provider-record-rate-limits provider headers))

(defparameter *provider-stream-retry-delays*
    '(1 2 4 8 16)
  "Backoff seconds for bounded provider request reconnects.")

(defparameter *provider-rate-limit-event-error-codes*
    '("rate_limit_exceeded")
  "Structured SSE error codes reporting exhausted provider allowance.")

(defparameter *provider-retryable-event-error-codes*
    '("server_error"
      "internal_server_error"
      "server_is_overloaded"
      "slow_down"
      "rate_limit_exceeded")
  "Structured SSE error codes eligible for bounded retry.")

(defparameter *provider-error-detail-limit* 1000
  "The characters of a provider failure explanation shown in its message.")

(defparameter *provider-retryable-http-statuses*
    '(500 502 503 504 507)
  "HTTP statuses eligible for bounded provider retry.")

(defparameter *provider-stream-retry-sleep-function* #'sleep
  "Function used to wait between provider stream reconnect attempts.")

(defparameter *provider-credential-redaction-marker*
  "[PROVIDER CREDENTIAL REDACTED]"
  "The preferred replacement for a credential echoed by a provider response.")

(defvar *provider-active-credential-values* nil
  "Exact credential strings available only inside one provider attempt.")

(defvar *provider-active-credential-redaction-marker* nil
  "A request-local marker containing none of the active credential values.")

(-> provider--sanitize-wire-string (string) string)
(defun provider--sanitize-wire-string (source)
  "Redact active exact provider credentials from untrusted wire SOURCE."
  (redact-exact-string-values
   source
   *provider-active-credential-values*
   (or *provider-active-credential-redaction-marker*
       *provider-credential-redaction-marker*)))

(-> provider--sanitize-wire-value (t) t)
(defun provider--sanitize-wire-value (value)
  "Return a detached provider wire VALUE with active credentials redacted."
  (cond
    ((stringp value)
     (provider--sanitize-wire-string value))
    ((hash-table-p value)
     (let ((copy
             (make-hash-table
              :test (hash-table-test value)
              :size (hash-table-count value))))
       (maphash
        (lambda (key child)
          (setf
           (gethash (provider--sanitize-wire-value key) copy)
           (provider--sanitize-wire-value child)))
        value)
       copy))
    ((vectorp value)
     (map 'vector #'provider--sanitize-wire-value value))
    ((consp value)
     (cons
      (provider--sanitize-wire-value (first value))
      (provider--sanitize-wire-value (rest value))))
    (t
     value)))

(defmethod provider-rate-limits ((provider model-provider))
  "Return no rate limit snapshot for providers that do not report one."
  (declare (ignore provider))
  nil)

(-> provider-family (model-provider) keyword)
(defgeneric provider-family (provider)
  (:documentation "Return the model family keyword PROVIDER serves."))

(defmethod provider-family ((provider model-provider))
  "Return the registered family for a provider."
  (or (and (model-provider-registration provider)
           (provider-registration-family (model-provider-registration provider)))
      ':custom))

(defmethod provider-family ((provider codex-subscription-provider))
  "The Codex provider serves the ChatGPT model family."
  (declare (ignore provider))
  ':codex)

(-> provider-child-reference-history-p (model-provider) boolean)
(defgeneric provider-child-reference-history-p (provider)
  (:documentation
   "Return true when child agents should inherit filtered parent reference history."))

(defmethod provider-child-reference-history-p ((provider model-provider))
  "Leave parent reference-history inheritance disabled by default."
  (declare (ignore provider))
  nil)

(defmethod provider-child-reference-history-p
    ((provider codex-subscription-provider))
  "Enable Codex multi-agent v2 style inherited reference history.

This follows the filtered fork-history behavior in Codex
=ba42e6866cef4baed7ad92c73e6be8cd42e49d8b= under
=codex-rs/core/src/agent/control/spawn.rs=."
  (declare (ignore provider))
  t)

(-> provider-device-authentication-client
    (subscription-provider)
    device-authentication-client)
(defgeneric provider-device-authentication-client (provider)
  (:documentation
   "Return a fresh device authentication client for PROVIDER's account service."))

(defmethod provider-device-authentication-client
    ((provider codex-subscription-provider))
  "Return the ChatGPT device authentication client."
  (declare (ignore provider))
  (device-authentication-client-create))

(-> provider-family-create
    (keyword configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defgeneric provider-family-create (family configuration &key reasoning-summaries-p)
  (:documentation
   "Create the subscription provider serving FAMILY for CONFIGURATION."))

(defmethod provider-family-create
    ((family (eql ':codex))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the direct ChatGPT subscription provider."
  (make-instance 'codex-subscription-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)
                 :reasoning-summaries-p reasoning-summaries-p))

(-> provider-create
    (configuration &key
                   (:reasoning-summaries-p boolean)
                   (:registration (option provider-registration)))
    model-provider)
(defun provider-create
    (configuration &key reasoning-summaries-p registration)
  "Create the provider serving CONFIGURATION's model.

REGISTRATION selects an explicit provider layer for callers such as /auth. When
it is NIL, the effective registration for CONFIGURATION's model is used."
  (let* ((model (configuration-model configuration))
         (effective-registration
           (or registration (provider-registration-for-model model)))
         (provider
           (if effective-registration
               (progn
                 (unless (some (lambda (candidate)
                                 (string= (provider-model-name candidate) model))
                               (provider-registration-models effective-registration))
                   (error 'configuration-error
                          :message
                          (format nil
                                  "Provider ~A does not serve model ~A."
                                  (provider-registration-name effective-registration)
                                  model)))
                 (funcall (provider-registration-factory effective-registration)
                          configuration
                          :reasoning-summaries-p reasoning-summaries-p))
               (provider-family-create
                (model-family model)
                configuration
                :reasoning-summaries-p reasoning-summaries-p))))
    (unless (typep provider 'model-provider)
      (error 'configuration-error
             :message
             (format nil
                     "Provider factory for model ~A returned ~S instead of a model-provider."
                     model provider)))
    (setf (model-provider-registration provider) effective-registration)
    provider))

(-> provider-authentication-provider
    (configuration string &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider-authentication-provider
    (configuration name &key reasoning-summaries-p)
  "Create NAME's registered provider for authentication.

When an authenticator exists without model metadata, construct the provider directly
so authentication can bootstrap credentials before model discovery."
  (let* ((canonical (provider--canonical-name name))
         (registration (provider-registration-find canonical)))
    (unless registration
      (error 'configuration-error
             :message
             (format nil "Unknown provider ~A. Registered providers: ~{~A~^, ~}."
                     name
                     (mapcar #'provider-registration-name (provider-registrations)))))
    (if (and (null (provider-registration-models registration))
             (provider-registration-authenticator registration))
        (let ((provider
                (funcall (provider-registration-factory registration)
                         configuration
                         :reasoning-summaries-p reasoning-summaries-p)))
          (unless (typep provider 'model-provider)
            (error 'configuration-error
                   :message
                   (format nil
                           "Provider factory for ~A returned ~S instead of a model-provider."
                           (provider-registration-name registration)
                           provider)))
          (setf (model-provider-registration provider) registration)
          provider)
        (progn
          (when (and (provider-registration-model-discovery registration)
                     (null (provider-registration-models registration)))
            (let ((failures
                    (provider-refresh-models configuration :provider-name canonical)))
              (when failures
                (error (first failures)))))
          (let ((model (first (provider-registration-models registration))))
            (unless model
              (error 'configuration-error
                     :message (format nil "Provider ~A has no available models."
                                      (provider-registration-name registration))))
            (provider-create
             (configuration-with-model configuration (provider-model-name model))
             :reasoning-summaries-p reasoning-summaries-p
             :registration registration))))))

(-> provider-with-configuration (model-provider configuration) model-provider)
(defgeneric provider-with-configuration (provider configuration)
  (:documentation
   "Return PROVIDER reconfigured for CONFIGURATION while preserving session state."))

(defmethod provider-with-configuration ((provider model-provider)
                                        (configuration configuration))
  "Create a fresh registered provider for a generic provider implementation."
  (declare (ignore provider))
  (provider-create configuration))

(defmethod provider-with-configuration :around
    ((provider subscription-provider) (configuration configuration))
  "Create a fresh provider when CONFIGURATION selects another registration."
  (let ((selected-registration
          (provider-registration-for-model (configuration-model configuration)))
        (current-registration (model-provider-registration provider)))
    (if (and (eq (provider-family provider)
                 (model-family (configuration-model configuration)))
             (or (null current-registration)
                 (eq current-registration selected-registration)))
        (call-next-method)
        (provider-create configuration))))

(defmethod provider-with-configuration
    ((provider codex-subscription-provider) (configuration configuration))
  "Copy PROVIDER with CONFIGURATION, retaining credentials, session, and limits."
  (let ((copy
          (make-instance 'codex-subscription-provider
                         :configuration configuration
                         :registration (model-provider-registration provider)
                         :credential-manager
                         (provider-credential-manager provider)
                         :session-id (provider-session-id provider)
                         :reasoning-summaries-p
                         (provider-reasoning-summaries-p provider))))
    (setf (provider-rate-limits copy) (copy-tree (provider-rate-limits provider)))
    copy))

(-> provider-set-reasoning-summaries (model-provider boolean) model-provider)
(defgeneric provider-set-reasoning-summaries (provider enabled-p)
  (:documentation
   "Set whether PROVIDER requests visible reasoning summaries when supported."))

(defmethod provider-set-reasoning-summaries
    ((provider model-provider) (enabled-p t))
  "Leave providers without reasoning-summary support unchanged."
  (declare (ignore enabled-p))
  provider)

(defmethod provider-set-reasoning-summaries
    ((provider codex-subscription-provider) (enabled-p t))
  "Set whether the Codex subscription provider requests reasoning summaries."
  (check-type enabled-p boolean)
  (setf (provider-reasoning-summaries-p provider) enabled-p)
  provider)

(-> provider-stream-turn
    (model-provider conversation
     &key (:tool-namespaces vector)
          (:event-callback function)
          (:goal-context (option string))
          (:compaction-p boolean))
    provider-result)
(defgeneric provider-stream-turn
    (provider conversation
     &key tool-namespaces event-callback goal-context compaction-p)
  (:documentation
   "Stream one model response for CONVERSATION using TOOL-NAMESPACES and EVENT-CALLBACK."))

(-> provider-native-compact-conversation
    (model-provider conversation
     &key (:tool-namespaces vector) (:event-callback function))
    (option json-object))
(defgeneric provider-native-compact-conversation
    (provider conversation &key tool-namespaces event-callback)
  (:documentation
   "Return PROVIDER's opaque native compaction item, or NIL when unsupported."))

(defmethod provider-native-compact-conversation
    ((provider model-provider)
     (conversation conversation)
     &key tool-namespaces event-callback)
  "Leave native compaction unavailable for providers without an endpoint."
  (declare (ignore provider conversation tool-namespaces event-callback))
  nil)

(-> provider-open-response-stream
    (model-provider json-object
     &key (:credentials oauth-credentials) (:conversation conversation))
    (values stream integer t))
(defgeneric provider-open-response-stream
    (provider request &key credentials conversation)
  (:documentation "Open an authenticated provider stream and return body, status, and headers."))

(-> provider-open-native-compaction
    (codex-subscription-provider json-object
     &key (:credentials oauth-credentials) (:conversation conversation))
    (values string integer t))
(defgeneric provider-open-native-compaction
    (provider request &key credentials conversation)
  (:documentation
   "POST REQUEST to PROVIDER's native compaction endpoint and return its body."))


;;;; -- Responses Lite Encoding --

(-> responses-lite-developer-message (string) json-object)
(defun responses-lite-developer-message (instructions)
  "Return the Responses Lite developer message containing INSTRUCTIONS."
  (json-object
   "type" "message"
   "role" "developer"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" instructions))))

(-> responses-lite-additional-tools (vector) json-object)
(defun responses-lite-additional-tools (tool-namespaces)
  "Return the Responses Lite developer item exposing TOOL-NAMESPACES."
  (json-object
   "type" "additional_tools"
   "role" "developer"
   "tools" tool-namespaces))

;; Modeled on the Codex context checkpoint compaction instructions at
;; reference commit 6219b7c40f, restated for Autolith.
(defparameter *compaction-instructions*
  "You are performing a context checkpoint compaction. Write a handoff summary for another model that will resume this conversation. Include the current progress and key decisions, important context, constraints, and user preferences, what remains to be done as clear next steps, and any critical data or references needed to continue. Reference completed rlm.infer and rlm.map frames by their trace identifiers as inference:<trace-id> resources instead of restating frame content; the traces stay readable through resource.read. Be concise, structured, and complete enough that no earlier context is required."
  "The developer instructions driving one compaction request.")

(-> response-item-assistant-text (json-object) (option string))
(defun response-item-assistant-text (item)
  "Return the joined visible text of assistant message ITEM, when applicable."
  (when (and (json-string= (json-get item "type") "message")
             (json-string= (json-get item "role") "assistant"))
    (let ((content (json-get item "content")))
      (when (vectorp content)
        (let ((parts
                (loop for part across content
                      when (and (json-object-p part)
                                (json-string-member-p
                                 (json-get part "type")
                                 '("output_text" "text"))
                                (stringp (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~%~}" parts)))))))

(-> provider-result-assistant-text (provider-result) (option string))
(defun provider-result-assistant-text (result)
  "Return the joined assistant text across RESULT's output items."
  (let ((parts (loop for item in (provider-result-output-items result)
                     for text = (and (json-object-p item)
                                     (response-item-assistant-text item))
                     when text
                       collect text)))
    (when parts
      (format nil "~{~A~^~%~}" parts))))

(-> response-item-reasoning-summary (json-object) (option string))
(defun response-item-reasoning-summary (item)
  "Return ITEM's provider-visible reasoning summary, never raw reasoning text."
  (when (json-string= (json-get item "type") "reasoning")
    (let ((summary (json-get item "summary")))
      (when (vectorp summary)
        (let ((parts
                (loop for part across summary
                      when (and (json-object-p part)
                                (json-string= (json-get part "type")
                                              "summary_text")
                                (non-empty-string-p (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~2%~}" parts)))))))

(defparameter *provider-hosted-tools-enabled-p* t
  "Whether the current provider request may advertise hosted provider tools.")

(-> provider--web-search-content-types (configuration) (option vector))
(defun provider--web-search-content-types (configuration)
  "Return CONFIGURATION's provider-required web-search content types.

All supported Codex GPT-5.6 models require text-and-image web search, as
recorded in the pinned Codex model catalog at ba42e6866cef4baed7ad92c73e6be8cd42e49d8b.
Grok receives no Codex-specific search-content declaration."
  (when (and (slot-boundp configuration 'model)
             (eq (model-family (configuration-model configuration)) ':codex))
    (json-array "text" "image")))

(-> provider-web-search-tool (configuration) (option json-object))
(defun provider-web-search-tool (configuration)
  "Return NIL because the subscription Responses endpoint does not execute web_search.

Autolith exposes web.run instead. It calls the provider's authenticated
standalone search endpoint and returns the cited result through the ordinary
local tool protocol."
  (declare (ignore configuration))
  nil)

(-> provider-request-object
    (subscription-provider conversation vector
     &key (:goal-context (option string))
          (:compaction-p boolean))
    (values json-object (option context-delivery)))
(defgeneric provider-request-object
    (provider conversation tool-namespaces &key goal-context compaction-p)
  (:documentation
   "Build PROVIDER's complete stateless streaming request for CONVERSATION.

The second value is the context delivery that the transport consumes only
after a completed response, when one participates in the request."))

(-> provider--codex-prompt-cache-key
    (codex-subscription-provider conversation)
    non-empty-string)
(defun provider--codex-prompt-cache-key (provider conversation)
  "Return CONVERSATION's root-and-child shared prompt-cache routing key.

The provider session remains broader than one resumable conversation, so the
cache key follows the conversation lineage instead. This preserves isolation
between roots while allowing a root and its task children to share a prefix."
  (declare (ignore provider))
  (conversation-prompt-cache-key conversation))

;; No service_tier field is ever sent. Omitting it selects the provider's
;; standard processing path; sending "priority" selects the fast path that
;; drains subscription rate limits much faster (Codex reference commit
;; 5c19155c filters its explicit "default" sentinel out of requests too).
(defmethod provider-request-object
    ((provider codex-subscription-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build the complete stateless Sol Responses Lite request for CONVERSATION.

The request never carries a service_tier, keeping Autolith on the standard path.
GOAL-CONTEXT and resolved context contributions ride as transient developer
messages that are never persisted in the durable conversation. Skill catalogs
and explicitly selected bodies participate through that same context delivery.
COMPACTION-P builds a tool-free summarization request whose trailing developer
message asks for a context checkpoint handoff. The second value is the context
delivery that the transport consumes only after a completed response."
  (let* ((configuration (provider-configuration provider))
         (web-search-tool (and (not compaction-p)
                               (provider-web-search-tool configuration)))
         (effective-tools
           (cond
             (compaction-p
              #())
             (web-search-tool
              (concatenate 'vector
                           tool-namespaces
                           (vector web-search-tool)))
             (t
              tool-namespaces)))
         (reasoning
           (json-object
            "effort" (configuration-wire-effort configuration)
            "context" "all_turns"))
         (prefix (append
                  (list (responses-lite-additional-tools effective-tools)
                        (responses-lite-developer-message
                         (let ((*system-prompt-hosted-web-search-p*
                                 (not (null web-search-tool))))
                           (system-prompt configuration))))
                  (when (and goal-context
                             (not compaction-p))
                    (list (responses-lite-developer-message goal-context)))))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-tools
              :goal-context goal-context)))
         (context-message
           (and delivery
                (context-delivery-rendered delivery)
                (responses-lite-developer-message
                 (context-delivery-rendered delivery))))
         (input (coerce (append prefix
                                (conversation-input-items-for-family
                                 conversation
                                 (provider-family provider)
                                 :include-ephemeral-p (not compaction-p))
                                (when context-message (list context-message))
                                (when compaction-p
                                  (list (responses-lite-developer-message
                                        *compaction-instructions*))))
                        'vector)))
    (when (and (provider-reasoning-summaries-p provider)
               (not compaction-p))
      (setf (gethash "summary" reasoning) "auto"))
    (values
     (json-object
      "model" (configuration-model configuration)
      "input" input
      "tool_choice" "auto"
      "parallel_tool_calls" false
      "reasoning" reasoning
      "store" false
      "stream" t
      "include" (json-array "reasoning.encrypted_content")
      "prompt_cache_key" (provider--codex-prompt-cache-key provider conversation)
      "text" (json-object "verbosity" "low"))
     delivery)))

(-> provider-native-compaction-request-object
    (codex-subscription-provider conversation vector)
    json-object)
(defun provider-native-compaction-request-object
    (provider conversation tool-namespaces)
  "Build Codex's non-streaming Responses compaction request for CONVERSATION.

This mirrors the Responses Lite compaction payload at Codex reference commit
ba42e6866cef4baed7ad92c73e6be8cd42e49d8b: durable family-compatible history
receives the ordinary developer prefix without an extra compaction trigger.
Request-local contributions and pending one-response items stay outside the
checkpoint."
  (let* ((configuration (provider-configuration provider))
         (web-search-tool (provider-web-search-tool configuration))
         (effective-tools
           (if web-search-tool
               (concatenate 'vector tool-namespaces (vector web-search-tool))
               tool-namespaces))
         (reasoning
           (json-object
            "effort" (configuration-wire-effort configuration)
            "context" "all_turns"))
         (input
           (coerce
            (append
             (list (responses-lite-additional-tools effective-tools)
                   (responses-lite-developer-message
                    (let ((*system-prompt-hosted-web-search-p* nil))
                      (system-prompt configuration))))
             (conversation-input-items-for-family
              conversation
              (provider-family provider)
              :include-ephemeral-p nil))
            'vector)))
    (when (provider-reasoning-summaries-p provider)
      (setf (gethash "summary" reasoning) "auto"))
    (json-object
     "model" (configuration-model configuration)
     "input" input
     "parallel_tool_calls" false
     "reasoning" reasoning
     "prompt_cache_key" (provider--codex-prompt-cache-key provider conversation)
     "text" (json-object "verbosity" "low"))))

(-> provider-user-agent () string)
(defun provider-user-agent ()
  "Return an honest, stable user agent for direct Autolith provider requests."
  (format nil "autolith/~A (~A ~A; ~A)"
          *autolith-version*
          (software-type)
          (software-version)
          (machine-type)))

(-> provider--codex-request-headers
    (codex-subscription-provider oauth-credentials conversation
     &key (:accept string))
    list)
(defun provider--codex-request-headers
    (provider credentials conversation &key accept)
  "Return authenticated Codex headers for one request to CONVERSATION."
  (append
   (list
    (cons "Authorization"
          (format nil "Bearer ~A" (oauth-credentials-access-token credentials)))
    (cons "ChatGPT-Account-ID" (oauth-credentials-account-id credentials))
    (cons "Content-Type" "application/json")
    (cons "Accept" accept)
    (cons "x-openai-internal-codex-responses-lite" "true")
    (cons "originator" "autolith")
    (cons "User-Agent" (provider-user-agent))
    (cons "session-id" (provider-session-id provider))
    (cons "thread-id" (conversation-identifier conversation))
    (cons "x-client-request-id" (make-identifier)))
   (when (conversation-turn-state conversation)
     (list (cons "x-codex-turn-state" (conversation-turn-state conversation))))))

(-> provider--native-compaction-endpoint (codex-subscription-provider) string)
(defun provider--native-compaction-endpoint (provider)
  "Return the native compaction endpoint corresponding to PROVIDER's endpoint."
  (let ((endpoint
          (string-right-trim
           '(#\/)
           (configuration-provider-endpoint (provider-configuration provider)))))
    (if (uiop:string-suffix-p "/responses/compact" endpoint)
        endpoint
        (format nil "~A/compact" endpoint))))

(defmethod provider-open-response-stream
    ((provider codex-subscription-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the ChatGPT Codex endpoint."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (let ((configuration (provider-configuration provider)))
    (dexador:post
     (configuration-provider-endpoint configuration)
     :headers (provider--codex-request-headers
               provider credentials conversation :accept "text/event-stream")
     :content (json-encode-utf8 request)
     :want-stream t
     :force-string t
     :keep-alive nil
     :connect-timeout 30
     :read-timeout 300)))

(defmethod provider-open-native-compaction
    ((provider codex-subscription-provider)
     (request hash-table)
     &key credentials conversation)
  "POST a JSON native compaction REQUEST to the ChatGPT Codex endpoint."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (dexador:post
   (provider--native-compaction-endpoint provider)
   :headers (provider--codex-request-headers
             provider credentials conversation :accept "application/json")
   :content (json-encode-utf8 request)
   :force-string t
   :keep-alive nil
   :connect-timeout 30
   :read-timeout 300))


;;;; -- SSE Decoding --

(defvar *sse-end-of-stream* (gensym "SSE-END-")
  "A private marker returned after a clean SSE end of stream.")

(defparameter *sse-maximum-line-characters* (* 1024 1024)
  "Maximum accepted character count for one SSE wire line.")

(defparameter *sse-maximum-event-characters* (* 4 1024 1024)
  "Maximum accepted joined data character count for one SSE event.")

(-> sse--signal-size-error (string) null)
(defun sse--signal-size-error (message)
  "Signal a bounded provider stream failure described by MESSAGE."
  (error 'response-stream-error
         :message message
         :status nil
         :request-id nil
         :response nil))

(-> sse-data-line (string) (option string))
(defun sse-data-line (line)
  "Return the payload of an SSE data LINE, or NIL for another field."
  (when (and (>= (length line) 5)
             (string= line "data:" :end1 5 :end2 5))
    (let ((start (if (and (> (length line) 5)
                          (char= (char line 5) #\Space))
                     6
                     5)))
      (subseq line start))))

(-> sse-read-line (stream) t)
(defun sse-read-line (stream)
  "Read one bounded line using only the portable character-stream protocol."
  (let ((characters
          (make-array 256
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)))
    (loop for character = (read-char stream nil *sse-end-of-stream*)
          do (cond
               ((eq character *sse-end-of-stream*)
                (return (if (plusp (length characters))
                            (coerce characters 'string)
                            *sse-end-of-stream*)))
               ((char= character #\Newline)
                (return (coerce characters 'string)))
               ((>= (length characters) *sse-maximum-line-characters*)
                (sse--signal-size-error
                 "The provider returned an SSE line above the configured limit."))
               (t
                (vector-push-extend character characters))))))

(-> read-sse-data (stream) t)
(defun read-sse-data (stream)
  "Read one bounded SSE event's joined data field from STREAM."
  (let ((data-stream (make-string-output-stream))
        (data-character-count 0)
        (data-line-count 0))
    (labels ((event-data ()
               (if (plusp data-line-count)
                   (get-output-stream-string data-stream)
                   *sse-end-of-stream*)))
      (loop
        (let ((raw-line (sse-read-line stream)))
          (when (eq raw-line *sse-end-of-stream*)
            (return (event-data)))
          (let ((line (string-right-trim '(#\Return) raw-line)))
            (when (zerop (length line))
              (when (plusp data-line-count)
                (return (event-data))))
            (let ((data (sse-data-line line)))
              (when data
                (let ((next-count (+ data-character-count
                                     (if (plusp data-line-count) 1 0)
                                     (length data))))
                  (when (> next-count *sse-maximum-event-characters*)
                    (sse--signal-size-error
                     "The provider returned an SSE event above the configured limit."))
                  (when (plusp data-line-count)
                    (write-char #\Newline data-stream))
                  (write-string data data-stream)
                  (setf data-character-count next-count)
                  (incf data-line-count))))))))))

(-> response-header (t string) (option string))
(defun response-header (headers name)
  "Return case-insensitive header NAME from Dexador HEADERS."
  (labels ((matching-name-p (candidate)
             (string-equal (string candidate) name)))
    (cond
      ((hash-table-p headers)
       (loop for key being the hash-keys of headers
               using (hash-value value)
             when (matching-name-p key)
               return value))
      ((listp headers)
       (let ((pair (find name headers :key #'first :test #'string-equal)))
         (when pair
           (if (consp (rest pair))
               (second pair)
               (rest pair)))))
      (t
       nil))))

(-> provider--response-request-id (t) (option string))
(defun provider--response-request-id (headers)
  "Return a provider request identifier from common response HEADERS."
  (let ((request-id
          (or (response-header headers "x-request-id")
              (response-header headers "request-id"))))
    (and (non-empty-string-p request-id) request-id)))

;;;; -- Rate Limit Snapshots --

(-> provider--parse-decimal (string) (option real))
(defun provider--parse-decimal (text)
  "Parse non-negative decimal TEXT such as 28 or 28.5 without the Lisp reader."
  (handler-case
      (let* ((trimmed (string-trim " " text))
             (dot (position #\. trimmed)))
        (if dot
            (let ((whole (parse-integer trimmed :end dot))
                  (fraction (subseq trimmed (1+ dot))))
              (if (zerop (length fraction))
                  whole
                  (float (+ whole
                            (/ (parse-integer fraction)
                               (expt 10 (length fraction)))))))
            (parse-integer trimmed)))
    (error ()
      nil)))

(-> provider--rate-limit-window (t string) (option list))
(defun provider--rate-limit-window (headers prefix)
  "Return one portable rate limit window parsed from HEADERS under PREFIX."
  (let ((used (response-header headers
                               (format nil "~A-used-percent" prefix))))
    (when (non-empty-string-p used)
      (let ((used-percent (provider--parse-decimal used))
            (minutes (response-header headers
                                      (format nil "~A-window-minutes" prefix)))
            (resets (response-header headers
                                     (format nil "~A-reset-at" prefix))))
        (when used-percent
          (list :used-percent used-percent
                :window-minutes (and (non-empty-string-p minutes)
                                     (parse-integer minutes :junk-allowed t))
                :resets-at (let ((seconds
                                   (and (non-empty-string-p resets)
                                        (parse-integer resets
                                                       :junk-allowed t))))
                             (and seconds
                                  (unix-time->universal-time seconds)))))))))

(-> provider-rate-limit-snapshot (t) (option list))
(defun provider-rate-limit-snapshot (headers)
  "Return the portable subscription rate limit snapshot carried by HEADERS."
  (let ((primary (provider--rate-limit-window headers "x-codex-primary"))
        (secondary (provider--rate-limit-window headers "x-codex-secondary")))
    (when (or primary secondary)
      (list :captured-at (get-universal-time)
            :primary primary
            :secondary secondary))))

(-> provider-record-rate-limits (codex-subscription-provider t) (option list))
(defun provider-record-rate-limits (provider headers)
  "Record and return rate limit data from HEADERS when the provider sent it."
  (let ((snapshot (provider-rate-limit-snapshot headers)))
    (when snapshot
      (setf (provider-rate-limits provider) snapshot))
    snapshot))

(-> provider--close-response-stream (stream) null)
(defun provider--close-response-stream (stream)
  "Abortively close STREAM without allowing cleanup failure to escape."
  (handler-case
      (when (open-stream-p stream)
        (close stream :abort t))
    (error ()
      nil))
  nil)

(-> provider--drain-error-body (stream) (option string))
(defun provider--drain-error-body (stream)
  "Read and return a bounded error body from STREAM, closing it afterwards.

Both decoded character streams and undecoded byte streams are accepted,
because the dependency chooses the element type from response headers."
  (unwind-protect
       (handler-case
           (if (subtypep (stream-element-type stream) 'character)
               (let* ((buffer (make-string 4000))
                      (end (read-character-sequence buffer stream)))
                 (and (plusp end) (subseq buffer 0 end)))
               (let* ((buffer (make-array 4000 :element-type '(unsigned-byte 8)))
                      (end (read-sequence buffer stream)))
                 (and (plusp end)
                      (sb-ext:octets-to-string (subseq buffer 0 end)
                                               :external-format ':utf-8))))
         (error ()
           nil))
    (provider--close-response-stream stream)))

(-> provider--error-body-text (t) (option string))
(defun provider--error-body-text (content)
  "Return dependency error CONTENT as displayable text, when it carries any.

A streaming request leaves the dependency's failure body as an undrained
character stream rather than a string, so every shape is normalized here:
strings pass through, streams are drained and closed, and octet vectors
are decoded as UTF-8."
  (handler-case
      (cond
        ((stringp content)
         (and (plusp (length content)) content))
        ((streamp content)
         (provider--drain-error-body content))
        ((and (vectorp content) (plusp (length content)))
         (sb-ext:octets-to-string (coerce content '(vector (unsigned-byte 8)))
                                  :external-format ':utf-8))
        (t
         nil))
    (error ()
      nil)))

(-> provider--error-body-detail ((option string)) (option string))
(defun provider--error-body-detail (body)
  "Return the human-readable explanation carried by an error BODY, if any."
  (when (non-empty-string-p body)
    (let ((message
            (handler-case
                (let ((decoded (json-decode body)))
                  (when (json-object-p decoded)
                    (let ((error-value (json-get decoded "error")))
                      (or (and (json-object-p error-value)
                               (let ((text (json-get error-value "message")))
                                 (and (non-empty-string-p text) text)))
                          ;; The Grok proxy reports a plain string here.
                          (and (non-empty-string-p error-value) error-value)
                          (let ((detail (json-get decoded "detail")))
                            (and (non-empty-string-p detail) detail))))))
              (error ()
                nil))))
      (bounded-string (or message body)
                      :limit *provider-error-detail-limit*))))

(-> provider--http-error-message (integer (option string)) string)
(defun provider--http-error-message (status body)
  "Return a display message for HTTP STATUS including BODY's explanation."
  (let ((detail (provider--error-body-detail body))
        (hint (case status
                (404 "The requested resource or model is not being served.")
                (429 "The subscription rate limit was reached; see /status.")
                ((500 502 503 504 507) "The provider service is having trouble.")
                (t nil))))
    (format nil "The provider returned HTTP ~D.~@[ ~A~]~@[~%~A~]"
            status
            hint
            detail)))

(-> provider--retryable-http-status-p (integer) boolean)
(defun provider--retryable-http-status-p (status)
  "Return true when provider HTTP STATUS describes a transient failure."
  (not (null (member status *provider-retryable-http-statuses* :test #'=))))

(-> provider--signal-http-status-failure
    (subscription-provider integer &key (:headers t) (:raw-body t))
    null)
(defun provider--signal-http-status-failure
    (provider status &key headers raw-body)
  "Signal the typed failure represented by HTTP STATUS, HEADERS, and RAW-BODY."
  (let* ((raw-text (provider--error-body-text raw-body))
         (body (and raw-text (provider--sanitize-wire-string raw-text))))
    (if (= status 401)
        (error 'provider-unauthorized
               :message
               (format nil "The provider rejected the current ~A credentials."
                       (provider-account-label provider))
               :status status
               :request-id (provider--response-request-id headers)
               :response nil)
        (error (if (provider--retryable-http-status-p status)
                   'provider-retryable-error
                   'provider-error)
               :message (provider--http-error-message status body)
               :status status
               :request-id (provider--response-request-id headers)
               :response (and body (bounded-string body :limit 2000)))))
  nil)

(-> provider-signal-http-failure
    (subscription-provider http-request-failed)
    null)
(defun provider-signal-http-failure (provider condition)
  "Record CONDITION headers and signal a typed provider or authentication error."
  (let ((status (response-status condition))
        (headers
          (provider--sanitize-wire-value
           (response-headers condition)))
        (body (let ((text (provider--error-body-text
                           (handler-case
                               (response-body condition)
                             (error ()
                               nil)))))
                (and text (provider--sanitize-wire-string text)))))
    (provider-note-response-headers provider headers)
    (if (= status 401)
        (error 'provider-unauthorized
               :message (format nil "The provider rejected the current ~A credentials."
                                (provider-account-label provider))
               :status status
               :request-id (provider--response-request-id headers)
               :response nil)
        (error (if (provider--retryable-http-status-p status)
                   'provider-retryable-error
                   'provider-error)
               :message (provider--http-error-message status body)
               :status status
               :request-id (provider--response-request-id headers)
               :response (and body (bounded-string body :limit 2000))))))


(-> normalize-response-item (json-object) json-object)
(defun normalize-response-item (item)
  "Remove transient server item identifiers from replayable provider ITEM."
  (remhash "id" item)
  item)

(-> provider-normalize-output-item (model-provider json-object) json-object)
(defgeneric provider-normalize-output-item (provider item)
  (:documentation
   "Return completed ITEM normalized for persistence, replay, and dispatch."))

(defmethod provider-normalize-output-item
    ((provider model-provider) (item hash-table))
  "Remove transient server item identifiers from replayable ITEM."
  (declare (ignore provider))
  (normalize-response-item item))

(-> function-call-item-p (json-object) boolean)
(defun function-call-item-p (item)
  "Return true when ITEM is a Responses function call."
  (json-string= (json-get item "type") "function_call"))

(-> provider-note-doom-loop-event (model-provider json-object) null)
(defgeneric provider-note-doom-loop-event (provider event)
  (:documentation
   "React to one server-reported doom-loop detection EVENT.

A provider with resample support may signal PROVIDER-RESAMPLE-REQUESTED to
abandon the looping stream; the default reaction ignores the report."))

(defmethod provider-note-doom-loop-event
    ((provider model-provider) (event hash-table))
  "Ignore doom-loop reports for providers without resample support."
  (declare (ignore provider event))
  nil)

(-> provider--reasoning-summary-key (json-object) (option list))
(defun provider--reasoning-summary-key (event)
  "Return EVENT's stable reasoning summary part identity, when available."
  (let ((item-id (json-get event "item_id"))
        (output-index (json-get event "output_index"))
        (summary-index (json-get event "summary_index")))
    (cond
      ((integerp output-index)
       (list :output output-index :summary summary-index))
      ((non-empty-string-p item-id)
       (list :item item-id :summary summary-index))
      (t
       nil))))

(-> provider--signal-stream-interruption (t string) null)
(defun provider--signal-stream-interruption (headers message)
  "Signal a retryable provider stream interruption described by MESSAGE."
  (error 'response-stream-error
         :message message
         :status nil
         :request-id (provider--response-request-id headers)
         :response nil))

(-> provider--signal-transport-failure (string &key (:retryable-p boolean)) null)
(defun provider--signal-transport-failure (message &key retryable-p)
  "Signal a credential-free provider transport failure described by MESSAGE."
  (error (if retryable-p
             'provider-transport-error
             'provider-error)
         :message message
         :status nil
         :request-id nil
         :response nil))

(-> provider--open-response-stream
    (model-provider hash-table
     &key (:credentials oauth-credentials)
          (:conversation conversation))
    *)
(defun provider--open-response-stream
    (provider request &key credentials conversation)
  "Open one provider response and normalize dependency transport conditions."
  (handler-case
      (provider-open-response-stream
       provider
       request
       :credentials credentials
       :conversation conversation)
    (ssl-error-syscall ()
      (provider--signal-transport-failure
       "The provider connection failed before a response was received."
       :retryable-p t))
    (socket-error ()
      (provider--signal-transport-failure
       "The provider connection failed before a response was received."
       :retryable-p t))
    (sb-bsd-sockets:name-service-error ()
      (provider--signal-transport-failure
       "The provider address could not be resolved."
       :retryable-p t))
    (ns-error ()
      (provider--signal-transport-failure
       "The provider address could not be resolved."
       :retryable-p t))
    (cl+ssl-error ()
      (provider--signal-transport-failure
       "The provider TLS connection could not be established."
       :retryable-p nil))
    (simple-error ()
      (provider--signal-transport-failure
       "The provider transport failed before a response was received."
       :retryable-p nil))))

(-> provider--open-native-compaction
    (codex-subscription-provider json-object
     &key (:credentials oauth-credentials) (:conversation conversation))
    (values string integer t))
(defun provider--open-native-compaction
    (provider request &key credentials conversation)
  "Open one native compaction request and normalize dependency transport failures."
  (handler-case
      (provider-open-native-compaction
       provider request :credentials credentials :conversation conversation)
    (ssl-error-syscall ()
      (provider--signal-transport-failure
       "The provider connection failed before compaction began."
       :retryable-p t))
    (socket-error ()
      (provider--signal-transport-failure
       "The provider connection failed before compaction began."
       :retryable-p t))
    (sb-bsd-sockets:name-service-error ()
      (provider--signal-transport-failure
       "The provider address could not be resolved."
       :retryable-p t))
    (ns-error ()
      (provider--signal-transport-failure
       "The provider address could not be resolved."
       :retryable-p t))
    (cl+ssl-error ()
      (provider--signal-transport-failure
       "The provider TLS connection could not be established."
       :retryable-p nil))
    (simple-error ()
      (provider--signal-transport-failure
       "The provider transport failed before compaction began."
       :retryable-p nil))))

(-> provider--read-sse-data (stream t) t)
(defun provider--read-sse-data (stream headers)
  "Read one SSE payload and normalize transport EOF into a provider condition."
  (handler-case
      (read-sse-data stream)
    (provider-error (condition)
      (error condition))
    (end-of-file ()
      (provider--signal-stream-interruption
       headers
       "The provider connection closed during an SSE event."))
    (error ()
      (provider--signal-stream-interruption
       headers
       "The provider stream could not be read."))))

(-> provider--decode-sse-data (string t) t)
(defun provider--decode-sse-data (data headers)
  "Decode one SSE DATA payload, normalizing truncation into a provider condition."
  (handler-case
      (provider--sanitize-wire-value (json-decode data))
    (end-of-file ()
      (provider--signal-stream-interruption
       headers
       "The provider connection closed during an SSE event."))
    (error ()
      (provider--signal-stream-interruption
       headers
       "The provider returned a malformed SSE event."))))

(-> provider--event-response (json-object) (option json-object))
(defun provider--event-response (event)
  "Return EVENT's nested response object, when present."
  (let ((response (json-get event "response")))
    (and (json-object-p response) response)))

(-> provider--event-error-object (json-object) (option json-object))
(defun provider--event-error-object (event)
  "Return the structured error object nested in EVENT, when present."
  (let* ((response (provider--event-response event))
         (response-error (and response (json-get response "error")))
         (event-error (json-get event "error")))
    (cond
      ((json-object-p response-error)
       response-error)
      ((json-object-p event-error)
       event-error)
      ((equal (json-get event "type") "error")
       event)
      (t
       nil))))

(-> provider--event-error-code ((option json-object)) (option string))
(defun provider--event-error-code (error-object)
  "Return ERROR-OBJECT's code or type when either is a non-empty string."
  (when error-object
    (let ((code (json-get error-object "code"))
          (type (json-get error-object "type")))
      (cond
        ((non-empty-string-p code)
         code)
        ((non-empty-string-p type)
         type)
        (t
         nil)))))

(-> provider--event-response-id
    (json-object (option string))
    (option string))
(defun provider--event-response-id (event current-response-id)
  "Return EVENT's response identifier or CURRENT-RESPONSE-ID."
  (let* ((response (provider--event-response event))
         (nested-id (and response (json-get response "id")))
         (event-id (json-get event "response_id")))
    (cond
      ((non-empty-string-p nested-id)
       nested-id)
      ((non-empty-string-p event-id)
       event-id)
      (t
       current-response-id))))

(-> provider--event-request-id
    (json-object (option json-object) t)
    (option string))
(defun provider--event-request-id (event error-object headers)
  "Return the structured or header request identifier for EVENT."
  (let ((error-request-id (and error-object
                               (json-get error-object "request_id")))
        (event-request-id (json-get event "request_id"))
        (header-request-id
          (provider--response-request-id headers)))
    (cond
      ((non-empty-string-p error-request-id)
       error-request-id)
      ((non-empty-string-p event-request-id)
       event-request-id)
      ((non-empty-string-p header-request-id)
       header-request-id)
      (t
       nil))))

(-> provider--retryable-event-error-code-p ((option string)) boolean)
(defun provider--retryable-event-error-code-p (code)
  "Return true when structured provider CODE describes a transient failure."
  (not (null (and code
                  (member code
                          *provider-retryable-event-error-codes*
                          :test #'string-equal)))))

(-> provider-rate-limit-error-p (t) boolean)
(defun provider-rate-limit-error-p (condition)
  "Return true when CONDITION reports exhausted provider allowance."
  (and (typep condition 'provider-error)
       (let ((code (provider-error-code condition)))
         (or (eql (provider-error-status condition) 429)
             (and code
                  (not
                   (null
                    (member code
                            *provider-rate-limit-event-error-codes*
                            :test #'string-equal))))))
       t))

(-> provider--event-incomplete-reason (json-object) non-empty-string)
(defun provider--event-incomplete-reason (event)
  "Return EVENT's response.incomplete reason, defaulting to unknown."
  (let* ((response (json-get event "response"))
         (details (and (json-object-p response)
                       (json-get response "incomplete_details")))
         (reason (and (json-object-p details) (json-get details "reason"))))
    (if (non-empty-string-p reason) reason "unknown")))

(-> provider--signal-incomplete-response
    (json-object &key (:data string) (:headers t)
                 (:response-id (option string)))
    null)
(defun provider--signal-incomplete-response
    (event &key data headers response-id)
  "Signal retryable EVENT with its structured incomplete reason."
  (let ((reason (provider--event-incomplete-reason event)))
    (error 'provider-incomplete-response
           :message (format nil "The provider returned an incomplete response (~A)."
                            reason)
           :reason reason
           :status nil
           :code "response_incomplete"
           :request-id (provider--event-request-id event nil headers)
           :response-id (provider--event-response-id event response-id)
           :response
           (bounded-string
            (provider--sanitize-wire-string data)
            :limit 2000))))

(-> provider--signal-event-failure
    (json-object
     &key (:type string)
          (:data string)
          (:headers t)
          (:response-id (option string)))
    null)
(defun provider--signal-event-failure
    (event &key type data headers response-id)
  "Signal EVENT as a structured terminal or retryable provider failure."
  (let* ((error-object (provider--event-error-object event))
         (code (provider--event-error-code error-object))
         (detail (and error-object (json-get error-object "message")))
         (message
           (if (non-empty-string-p detail)
               (format nil "The provider returned ~A.~%~A"
                       (or code type)
                       (bounded-string detail :limit 1000))
               (format nil "The provider ended with ~A." type)))
         (condition-type
           (if (provider--retryable-event-error-code-p code)
               'provider-retryable-error
               'provider-error)))
    (error condition-type
           :message message
           :status nil
           :code code
           :request-id (provider--event-request-id event error-object headers)
           :response-id (provider--event-response-id event response-id)
           :response
           (bounded-string
            (provider--sanitize-wire-string data)
            :limit 2000))))

(-> provider-consume-stream (model-provider stream t function) provider-result)
(defgeneric provider-consume-stream (provider stream headers event-callback)
  (:documentation
   "Consume PROVIDER's protocol stream while invoking EVENT-CALLBACK."))

(defmethod provider-consume-stream ((provider model-provider) stream headers event-callback)
  "Consume a Responses protocol STREAM into a provider result."
  (let ((output-items nil)
        (response-id nil)
        (usage nil)
        (turn-completion :unspecified)
        (reasoning-summary-key nil)
        (completed-p nil))
    (loop until completed-p
          for data = (provider--read-sse-data stream headers)
          do (when (eq data *sse-end-of-stream*)
               (provider--signal-stream-interruption
                headers
                "The provider stream closed before a terminal event."))
             (unless (string= data "[DONE]")
               (let* ((event (provider--decode-sse-data data headers))
                      (type (and (json-object-p event)
                                 (json-get event "type"))))
                 (cond
                  ((json-string= type "response.created")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (json-get response "id"))))
                    (funcall event-callback
                             (make-instance 'provider-progress-event)))
                  ((json-string= type "response.output_text.delta")
                    (funcall event-callback
                             (make-instance 'assistant-delta-event
                                            :text (or (json-get event "delta") ""))))
                  ((json-string=
                    type "response.reasoning_summary_text.delta")
                    (let* ((delta (or (json-get event "delta") ""))
                           (next-key
                             (and (plusp (length delta))
                                  (provider--reasoning-summary-key event)))
                           (new-part-p
                             (and next-key
                                  reasoning-summary-key
                                  (not (equal next-key reasoning-summary-key)))))
                      (when next-key
                        (setf reasoning-summary-key next-key))
                      (funcall event-callback
                               (make-instance
                                'reasoning-delta-event
                                :text (if new-part-p
                                          (format nil "~2%~A" delta)
                                          delta)))))
                  ((json-string= type "response.output_item.done")
                    (let ((item (json-get event "item")))
                      (when (json-object-p item)
                        (provider-normalize-output-item provider item)
                        (push item output-items)
                        (funcall event-callback
                                 (make-instance 'provider-item-event :item item)))))
                  ((json-string= type "response.completed")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (or (json-get response "id") response-id)
                              usage (json-get response "usage"))
                        (multiple-value-bind (end-turn present-p)
                            (gethash "end_turn" response)
                          (when present-p
                            (setf turn-completion
                                  (if end-turn :end :continue)))))
                      (setf completed-p t)
                      (funcall event-callback
                               (make-instance 'provider-completed-event
                                              :response-id response-id
                                              :usage usage
                                              :turn-completion turn-completion))))
                  ((json-string= type "response.doom_loop_check")
                    ;; A non-standard xAI event reporting a detected
                    ;; generation loop; it must never reach item handling.
                    (provider-note-doom-loop-event provider event)
                    (funcall event-callback
                             (make-instance 'provider-progress-event)))
                  ((json-string= type "response.incomplete")
                    (provider--signal-incomplete-response
                     event
                     :data data
                     :headers headers
                     :response-id response-id))
                  ((json-string-member-p
                    type '("response.failed" "error"))
                    (provider--signal-event-failure
                     event
                     :type type
                     :data data
                     :headers headers
                     :response-id response-id))
                   (t
                    (funcall event-callback
                             (make-instance 'provider-progress-event)))))))
    (let* ((ordered-items (nreverse output-items))
           (tool-calls (remove-if-not #'function-call-item-p ordered-items)))
      (make-instance 'provider-result
                     :response-id response-id
                     :output-items ordered-items
                     :tool-calls tool-calls
                     :usage usage
                     :turn-state (response-header headers "x-codex-turn-state")
                     :turn-completion turn-completion))))

(-> provider-attempt-turn
    (model-provider conversation
     &key (:tool-namespaces vector)
          (:event-callback function)
          (:force-refresh boolean)
          (:goal-context (option string))
          (:compaction-p boolean))
    provider-result)
(defgeneric provider-attempt-turn
    (provider conversation
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  (:documentation
   "Perform one normalized provider attempt, optionally forcing credential refresh."))

(defmethod provider-attempt-turn
    ((provider subscription-provider)
     (conversation conversation)
     &key
       tool-namespaces
       event-callback
       force-refresh
       goal-context
       compaction-p)
  "Perform one direct request and normalize every HTTP boundary condition."
  (declare (type vector tool-namespaces)
           (type function event-callback)
           (type boolean force-refresh))
  (with-credentials (credentials (provider-credential-manager provider)
                                 :force-refresh force-refresh)
    (let* ((*provider-active-credential-values*
             (oauth-credentials-secret-values credentials))
           (*provider-active-credential-redaction-marker*
             (safe-redaction-marker
              *provider-credential-redaction-marker*
              *provider-active-credential-values*)))
      (handler-case
        (multiple-value-bind (request delivery)
            (provider-request-object
             provider
             conversation
             tool-namespaces
             :goal-context goal-context
             :compaction-p compaction-p)
          (multiple-value-bind (stream status raw-headers)
              (provider--open-response-stream
               provider
               request
               :credentials credentials
               :conversation conversation)
            (let* ((headers
                     (provider--sanitize-wire-value raw-headers))
                   (result
                     (unwind-protect
                          (progn
                            (provider-note-response-headers provider headers)
                            (unless (= status 200)
                              (provider--signal-http-status-failure
                               provider status
                               :headers headers
                               :raw-body stream))
                            (provider-consume-stream
                             provider stream headers event-callback))
                       (provider--close-response-stream stream))))
              (context-delivery-complete delivery)
              result)))
        (dexador.error:http-request-unauthorized (condition)
          (provider-signal-http-failure provider condition))
        (http-request-failed (condition)
          (provider-signal-http-failure provider condition))))))

(-> provider--call-with-bounded-retries
    (subscription-provider function function)
    t)
(defun provider--call-with-bounded-retries
    (provider attempt-function event-callback)
  "Call ATTEMPT-FUNCTION with bounded authentication and transport recovery.

ATTEMPT-FUNCTION receives one boolean indicating whether it must force a
credential refresh. EVENT-CALLBACK receives the retry notifications shared by
streaming turns and native compaction requests."
  (labels ((attempt-with-authentication ()
             "Run one logical request with bounded credential recovery."
             (let* ((manager (provider-credential-manager provider))
                    (refreshable-p
                      (credential-manager-refreshable-p manager))
                    (maximum-attempts (if refreshable-p 3 1)))
               (loop for attempt-number from 1 to maximum-attempts
                     for force-refresh = (and refreshable-p
                                              (= attempt-number 3))
                     do (handler-case
                            (return-from attempt-with-authentication
                              (funcall attempt-function force-refresh))
                          (provider-unauthorized ()
                            (when (= attempt-number maximum-attempts)
                              (error 'authentication-error
                                     :message
                                     (if refreshable-p
                                         (format nil
                                                 "~A rejected Autolith's credentials after a bounded refresh."
                                                 (provider-account-label provider))
                                         (format nil
                                                 "~A rejected Autolith's API key; ~A."
                                                 (provider-account-label provider)
                                                 (credential-manager-login-hint manager))))))))
               (error 'authentication-error
                      :message
                      (format nil "~A authentication retry ended unexpectedly."
                              (provider-account-label provider))))))
    (let ((retry-number 0))
      (loop
        (handler-case
            (return-from provider--call-with-bounded-retries
              (attempt-with-authentication))
          (provider-resample-requested (condition)
            ;; Loops are stochastic at sampling temperature, so a fresh
            ;; sample is the remedy and waiting buys nothing. The signaling
            ;; provider enforces its own per-turn resample budget.
            (funcall event-callback
                     (make-instance
                      'provider-retry-event
                      :attempt (provider-resample-requested-attempt condition)
                      :maximum-attempts
                      (provider-resample-requested-maximum-attempts condition)
                      :delay 0)))
          (provider-retryable-error (condition)
            (when (= retry-number
                     (length *provider-stream-retry-delays*))
              (error condition))
            (let ((delay
                    (nth retry-number *provider-stream-retry-delays*)))
              (funcall event-callback
                       (make-instance
                        'provider-retry-event
                        :attempt (1+ retry-number)
                        :maximum-attempts
                        (length *provider-stream-retry-delays*)
                        :delay delay))
              (funcall *provider-stream-retry-sleep-function* delay)
              (incf retry-number))))))))

(defmethod provider-stream-turn
    ((provider subscription-provider)
     (conversation conversation)
     &key
       tool-namespaces
       event-callback
       goal-context
       compaction-p)
  "Stream one subscription turn with bounded authentication and transport retries."
  (declare (type vector tool-namespaces)
           (type function event-callback))
  (provider--call-with-bounded-retries
   provider
   (lambda (force-refresh)
     (provider-attempt-turn
      provider
      conversation
      :tool-namespaces tool-namespaces
      :event-callback event-callback
      :force-refresh force-refresh
      :goal-context goal-context
      :compaction-p compaction-p))
   event-callback))


;;;; -- Native Responses Compaction --

(-> provider--signal-invalid-native-compaction
    (codex-subscription-provider integer t)
    null)
(defun provider--signal-invalid-native-compaction (provider status headers)
  "Signal that Codex returned an unusable successful compaction response."
  (declare (ignore provider))
  (error 'provider-error
         :message "The provider returned an invalid native compaction response."
         :status status
         :request-id (provider--response-request-id headers)
         :response nil))

(-> provider--decode-native-compaction-response
    (codex-subscription-provider t &key (:status integer) (:headers t))
    (option json-object))
(defun provider--decode-native-compaction-response
    (provider body &key status headers)
  "Decode BODY and return its newest normalized opaque compaction output item.

The endpoint can return a compacted transcript containing ordinary output,
multiple checkpoint encodings, or no opaque checkpoint. The newest usable
checkpoint carries native state; an opaque-free transcript uses the portable
summary fallback."
  (let ((source (provider--error-body-text body)))
    (unless (non-empty-string-p source)
      (provider--signal-invalid-native-compaction provider status headers))
    (let ((response
            (handler-case
                (json-decode source)
              (error ()
                (provider--signal-invalid-native-compaction
                 provider status headers)))))
      (let ((output (and (json-object-p response)
                         (json-get response "output"))))
        (unless (and (vectorp output)
                     (every #'json-object-p output))
          (provider--signal-invalid-native-compaction provider status headers))
        (let ((items
                (remove-if-not
                 #'native-compaction-item-p
                 (map 'list
                      (lambda (item)
                        (native-compaction-item-canonicalize
                         (provider-normalize-output-item provider item)))
                      output))))
          (first (last items)))))))

(-> provider-attempt-native-compaction
    (codex-subscription-provider conversation
     &key (:tool-namespaces vector) (:force-refresh boolean))
    (option json-object))
(defgeneric provider-attempt-native-compaction
    (provider conversation &key tool-namespaces force-refresh)
  (:documentation
   "Perform one authenticated Codex native compaction request."))

(defmethod provider-attempt-native-compaction
    ((provider codex-subscription-provider)
     (conversation conversation)
     &key tool-namespaces force-refresh)
  "Perform one native compaction attempt with optional credential refresh."
  (declare (type vector tool-namespaces)
           (type boolean force-refresh))
  (with-credentials (credentials (provider-credential-manager provider)
                                 :force-refresh force-refresh)
    (let* ((*provider-active-credential-values*
             (oauth-credentials-secret-values credentials))
           (*provider-active-credential-redaction-marker*
             (safe-redaction-marker
              *provider-credential-redaction-marker*
              *provider-active-credential-values*)))
      (handler-case
          (let ((request
                  (provider-native-compaction-request-object
                   provider conversation tool-namespaces)))
            (multiple-value-bind (body status raw-headers)
                (provider--open-native-compaction
                 provider request :credentials credentials :conversation conversation)
              (let ((headers (provider--sanitize-wire-value raw-headers)))
                (provider-note-response-headers provider headers)
                (unless (= status 200)
                  (provider--signal-http-status-failure
                   provider status :headers headers :raw-body body))
                (provider--decode-native-compaction-response
                 provider body :status status :headers headers))))
        (dexador.error:http-request-unauthorized (condition)
          (provider-signal-http-failure provider condition))
        (http-request-failed (condition)
          (provider-signal-http-failure provider condition))))))

(-> provider--native-compaction-unavailable-p (provider-error) boolean)
(defun provider--native-compaction-unavailable-p (condition)
  "Return true when CONDITION means this Codex endpoint is not available."
  (let ((status (provider-error-status condition)))
    (and (integerp status)
         (not (null (member status '(404 405 501) :test #'=))))))

(defmethod provider-native-compact-conversation
    ((provider codex-subscription-provider)
     (conversation conversation)
     &key tool-namespaces event-callback)
  "Compact CONVERSATION remotely, returning NIL only when the endpoint is absent."
  (declare (type vector tool-namespaces)
           (type function event-callback))
  (handler-case
      (provider--call-with-bounded-retries
       provider
       (lambda (force-refresh)
         (provider-attempt-native-compaction
          provider conversation
          :tool-namespaces tool-namespaces
          :force-refresh force-refresh))
       event-callback)
    (provider-error (condition)
      (if (provider--native-compaction-unavailable-p condition)
          nil
          (error condition)))))
