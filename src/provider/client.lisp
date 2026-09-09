(in-package #:autolith)

;;;; -- Provider Protocol --

(defclass session-preserving-provider-mixin ()
  ()
  (:documentation
   "Mixin for providers whose reconfiguration preserves session state."))

(defclass codex-subscription-provider
    (session-preserving-provider-mixin responses-api-provider)
  ((reasoning-summaries-p
    :initarg :reasoning-summaries-p
    :initform nil
    :accessor provider-reasoning-summaries-p
    :type boolean
    :documentation "Whether requests opt in to provider-visible reasoning summaries.")
   (rate-limits
    :initarg :rate-limits
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

(defparameter *chatgpt-authentication-method* ':browser
  "The dynamically selected ChatGPT authentication method.")

(-> provider--authentication-method
    (model-provider (or null string symbol))
    keyword)
(defun provider--authentication-method (provider method)
  "Validate and normalize METHOD for PROVIDER authentication."
  (when (and method
             (not (typep provider 'codex-subscription-provider)))
    (error 'authentication-error
           :message
           "Only the ChatGPT provider accepts an authentication method."))
  (let ((name (and method (string-downcase (string method)))))
    (cond
      ((or (null name) (string= name "browser"))
       ':browser)
      ((member name '("device" "device-code") :test #'string=)
       ':device-code)
      (t
       (error 'authentication-error
              :message
              "ChatGPT authentication method must be browser or device.")))))

(-> provider-authenticate-with-method
    (model-provider (or null string symbol)
     &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider-authenticate-with-method
    (provider method &key stream open-browser-p)
  "Authenticate PROVIDER using its selected METHOD."
  (let ((*chatgpt-authentication-method*
          (provider--authentication-method provider method)))
    (provider-authenticate provider
                           :stream stream
                           :open-browser-p open-browser-p)))

(-> provider--authentication-completion-message
    (model-provider string)
    string)
(defun provider--authentication-completion-message (provider message)
  "Refresh PROVIDER's dynamic model catalog and append any warning to MESSAGE."
  (let ((registration (model-provider-registration provider)))
    (if (and registration
             (provider-registration-model-discovery registration))
        (let ((failures
                (provider-refresh-models
                 (provider-configuration provider)
                 :provider-name (provider-registration-name registration))))
          (if failures
              (format nil
                      "~A~%Model discovery warnings:~%~{~A~%~}"
                      message
                      (mapcar #'autolith-error-message failures))
              message))
        message)))

(defmethod provider-authenticate :around
    ((provider model-provider) &key stream open-browser-p)
  "Give a registered provider authenticator precedence over protocol defaults."
  (let* ((registration (model-provider-registration provider))
         (authenticator
           (and registration
                (provider-registration-authenticator registration)))
         (message
           (if authenticator
               (funcall authenticator provider
                        :stream stream
                        :open-browser-p open-browser-p)
               (call-next-method))))
    (provider--authentication-completion-message provider message)))

(defmethod provider-authenticate ((provider model-provider)
                                  &key stream open-browser-p)
  "Reject authentication for a provider without an authentication protocol."
  (declare (ignore stream open-browser-p))
  (error 'authentication-error
         :message
         (format nil
                 "The ~A provider does not expose an authentication operation."
                 (provider-account-label provider))))

(defmethod provider-authenticate
    ((provider codex-subscription-provider) &key stream open-browser-p)
  "Run the selected OAuth flow for the ChatGPT subscription provider."
  (let ((stream (or stream *standard-output*)))
    (ecase *chatgpt-authentication-method*
      (:browser
       (chatgpt-oauth-login
        (provider-credential-manager provider)
        :stream stream
        :open-browser-p open-browser-p))
      (:device-code
       (device-authentication-login
        (provider-device-authentication-client provider)
        (provider-credential-manager provider)
        :stream stream
        :open-browser-p open-browser-p))))
  "ChatGPT authentication was saved by Autolith.")

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

(defparameter *provider-stream-inactivity-seconds* 300
  "Seconds one provider stream line may stall before reconnecting.

Dexador's :READ-TIMEOUT governs the response header exchange but not the
blocking reads that follow on a TLS stream, so a connection lost mid-stream
otherwise parks the turn forever. NIL disables the bound.")

(defmethod provider-rate-limits ((provider model-provider))
  "Return no rate limit snapshot for providers that do not report one."
  (declare (ignore provider))
  nil)

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

(-> provider-reconfiguration-initargs
    (session-preserving-provider-mixin)
    list)
(defgeneric provider-reconfiguration-initargs (provider)
  (:method-combination append)
  (:documentation
   "Return additional MAKE-INSTANCE initargs preserved while reconfiguring PROVIDER."))

(defmethod provider-reconfiguration-initargs append
    ((provider session-preserving-provider-mixin))
  "Preserve PROVIDER's registration, credentials, and session identity."
  (list :registration (model-provider-registration provider)
        :credential-manager (provider-credential-manager provider)
        :session-id (provider-session-id provider)))

(defmethod provider-reconfiguration-initargs append
    ((provider codex-subscription-provider))
  "Preserve Codex reasoning-summary and portable rate-limit state."
  (list :reasoning-summaries-p (provider-reasoning-summaries-p provider)
        :rate-limits (copy-tree (provider-rate-limits provider))))

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
    ((provider session-preserving-provider-mixin)
     (configuration configuration))
  "Copy PROVIDER with CONFIGURATION while preserving its session state."
  (apply #'make-instance
         (class-of provider)
         :configuration configuration
         (provider-reconfiguration-initargs provider)))

(defmethod provider-set-reasoning-summaries
    ((provider codex-subscription-provider) (enabled-p t))
  "Set whether the Codex subscription provider requests reasoning summaries."
  (check-type enabled-p boolean)
  (setf (provider-reasoning-summaries-p provider) enabled-p)
  provider)

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

;; Modeled on the Codex context checkpoint compaction instructions at
;; reference commit 6219b7c40f, restated for Autolith.
(defparameter *compaction-instructions*
  "You are performing a context checkpoint compaction. Write a handoff summary for another model that will resume this conversation. Include the current progress and key decisions, important context, constraints, and user preferences, what remains to be done as clear next steps, and any critical data or references needed to continue. Reference completed rlm.infer and rlm.map frames by their trace identifiers as inference:<trace-id> resources instead of restating frame content; the traces stay readable through resource.read. Be concise, structured, and complete enough that no earlier context is required."
  "The developer instructions driving one compaction request.")

(defparameter *provider-hosted-tools-enabled-p* t
  "Whether the current provider request may advertise hosted provider tools.")

(defparameter *provider-maximum-output-tokens* nil
  "An optional output token ceiling for the current provider request.

Inference frames bind this to their reserved output tranche so one
response cannot dramatically overrun the shared subtree budget.")

(-> provider-web-search-tool (configuration) (option json-object))
(defun provider-web-search-tool (configuration)
  "Return NIL because the subscription Responses endpoint does not execute web_search.

Autolith exposes web.run instead. It calls the provider's authenticated
standalone search endpoint and returns the cited result through the ordinary
local tool protocol."
  (declare (ignore configuration))
  nil)

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

;; Codex Fast mode uses service_tier="priority", the canonical request value
;; for Fast mode, only when the current model advertises support. This follows
;; Codex reference commit 287587c32c9cbc1e78edbf2aaae6a6d84f5b0c56.

(-> provider--codex-responses-request-fields
    (codex-subscription-provider conversation &key (:compaction-p boolean))
    list)
(defun provider--codex-responses-request-fields
    (provider conversation &key compaction-p)
  "Return Codex fields shared by the concrete and generic Responses views."
  (let ((configuration (provider-configuration provider)))
    (append
     (list "parallel_tool_calls" (if compaction-p false t)
           "include" (json-array "reasoning.encrypted_content")
           "prompt_cache_key" (provider--codex-prompt-cache-key
                               provider conversation)
           "text" (json-object "verbosity" "low"))
     (when (configuration-codex-fast-mode-active-p configuration)
       (list "service_tier" "priority")))))

(-> provider-native-compaction-request-object
    (codex-subscription-provider conversation vector)
    json-object)
(defun provider-native-compaction-request-object
    (provider conversation tool-namespaces)
  "Build a standard Responses compaction request for CONVERSATION.

Durable family-compatible history and top-level instructions participate in the
native checkpoint. Request-local contributions and pending one-response items
stay outside it."
  (declare (ignore tool-namespaces))
  (let* ((configuration (provider-configuration provider))
         (instructions
           (responses-standard-instructions
            (list
             (let ((*system-prompt-hosted-web-search-p* nil))
               (system-prompt configuration)))))
          (input
            (map 'vector
                 (lambda (item)
                   (provider-wire-input-item provider item))
                 (conversation-input-items-for-family
                  conversation
                  (provider-family provider)
                  :include-ephemeral-p nil))))
    (apply
     #'json-object
     (append
      (list
       "model" (configuration-model configuration)
       "instructions" instructions
       "input" input
       "prompt_cache_key" (provider--codex-prompt-cache-key provider conversation))
      (when (configuration-codex-fast-mode-active-p configuration)
        (list "service_tier" "priority"))))))

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
    (provider-call-with-response-deadline
     300
     (lambda ()
       (dexador:post
        (configuration-provider-endpoint configuration)
        :headers (provider--codex-request-headers
                  provider credentials conversation :accept "text/event-stream")
        :content (json-encode-utf8 request)
        :want-stream t
        :force-string t
        :keep-alive nil
        :connect-timeout 30
        :read-timeout 300)))))

(defmethod provider-open-native-compaction
    ((provider codex-subscription-provider)
     (request hash-table)
     &key credentials conversation)
  "POST a JSON native compaction REQUEST to the ChatGPT Codex endpoint."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (provider-call-with-response-deadline
   300
   (lambda ()
     (dexador:post
      (provider--native-compaction-endpoint provider)
      :headers (provider--codex-request-headers
                provider credentials conversation :accept "application/json")
      :content (json-encode-utf8 request)
      :force-string t
      :keep-alive nil
      :connect-timeout 30
      :read-timeout 300))))


;;;; -- SSE Decoding --

;;; Bounded SSE decoding lives in cl-llm-provider-api. Autolith supplies the
;;; runtime-specific pieces: an inactivity deadline around each line read and
;;; a provider condition class for stream size violations.

(-> sse-read-line (stream) t)
(defun sse-read-line (stream)
  "Read one bounded line, reconnecting when the stream stalls.

The deadline covers one line, so every delivered line renews it. A stream
that stops mid-turn signals a transport failure the bounded retry ladder can
act on instead of blocking on a dead connection indefinitely."
  (if (and *provider-stream-inactivity-seconds*
           (plusp *provider-stream-inactivity-seconds*))
      (handler-case
          (provider-call-with-response-deadline
           *provider-stream-inactivity-seconds*
           (lambda ()
             (sse-read-line-characters stream)))
        (sb-sys:deadline-timeout ()
          (error 'response-stream-error
                 :message
                 (format nil
                         "The provider stream delivered nothing for ~D seconds."
                         *provider-stream-inactivity-seconds*)
                 :status nil
                 :request-id nil
                 :response nil)))
      (sse-read-line-characters stream)))

(setf *sse-read-line-function* #'sse-read-line)
(setf *stream-limit-error-class* 'response-stream-limit-error)

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

(-> provider--open-response-stream
    (model-provider hash-table
     &key (:credentials oauth-credentials)
          (:conversation conversation))
    *)


(defun provider--open-response-stream (provider request &key credentials conversation)
  "Open an authenticated product request through portable transport normalization."
  (provider--call-with-transport-normalization
   (lambda ()
     (provider-open-response-stream provider request :credentials credentials
                                    :conversation conversation))
   :terminal-errors-p t))

(-> provider--open-native-compaction
    (codex-subscription-provider json-object
     &key (:credentials oauth-credentials) (:conversation conversation))
    (values string integer t))


(defun provider--open-native-compaction
       (provider request &key credentials conversation)
  "Open an authenticated product request through portable transport normalization."
  (provider--call-with-transport-normalization
   (lambda ()
     (provider-open-native-compaction provider request :credentials credentials
                                      :conversation conversation))
   :terminal-errors-p t))

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
           ((provider subscription-provider) (conversation conversation)
            &key tool-namespaces event-callback force-refresh goal-context
            compaction-p)
  "Execute one projected request inside product credential and delivery ownership."
  (with-credentials (credentials (provider-credential-manager provider) :force-refresh
                     force-refresh)
    (let* ((*provider-active-credential-values*
            (oauth-credentials-secret-values credentials))
           (*provider-active-credential-redaction-marker*
            (safe-redaction-marker *provider-credential-redaction-marker*
                                   *provider-active-credential-values*)))
      (handler-case
       (multiple-value-bind (request delivery)
           (provider-request-object provider conversation tool-namespaces :goal-context
                                    goal-context :compaction-p compaction-p)
         (cl-llm-provider-api::provider-execute-request provider request :secrets
          *provider-active-credential-values* :event-callback event-callback :transport
          (lambda (request)
            (provider--open-response-stream provider request :credentials credentials
                                            :conversation conversation))
          :completion (lambda () (context-delivery-complete delivery))))
       (http-request-failed (condition)
                            (provider-signal-http-failure provider condition))))))

(defparameter *provider-maximum-transient-retries* 6
  "Maximum retryable provider failures allowed after the initial attempt.")

(-> provider--call-with-transient-retries
    (function function &key (:sleep-function function) (:random-state random-state))
    t)


(defun provider--call-with-transient-retries
       (attempt-function event-callback
        &key (sleep-function *bounded-retry-sleep-function*)
        (random-state *random-state*))
  "Apply the product reconnect limit and jitter policy to the shared retry engine."
  (call-with-bounded-retries attempt-function event-callback :maximum-retries
                             *provider-maximum-transient-retries* :sleep-function
                             sleep-function :delay-function
                             (lambda (retry-number condition)
                               (declare (ignore condition))
                               (let ((base-delay
                                      (min 50 (ash 1 (min 6 (1- retry-number))))))
                                 (max 1
                                      (min 60
                                           (round
                                            (* base-delay
                                               (+ 0.8d0
                                                  (random 0.4d0 random-state))))))))))

(-> provider--call-with-bounded-retries
    (subscription-provider function function)
    t)
(defun provider--call-with-bounded-retries
    (provider attempt-function event-callback)
  "Call ATTEMPT-FUNCTION with bounded authentication and persistent transport recovery."
  (labels ((attempt-with-authentication ()
             "Run one logical request with bounded credential recovery."
             (let* ((manager (provider-credential-manager provider))
                    (refreshable-p
                      (credential-manager-refreshable-p manager))
                    (maximum-attempts (if refreshable-p 2 1)))
               (loop for attempt-number from 1 to maximum-attempts
                     for force-refresh = (and refreshable-p
                                              (= attempt-number 2))
                     do (handler-case
                            (return-from attempt-with-authentication
                              (provider--call-with-transport-normalization
                               (lambda ()
                                 (funcall attempt-function force-refresh))))
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
    (provider--call-with-transient-retries
     #'attempt-with-authentication event-callback)))

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
  "Compact CONVERSATION through the standard Responses compact endpoint."
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
