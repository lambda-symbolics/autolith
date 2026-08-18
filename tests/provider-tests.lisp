(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-provider-rate-limits () null)
(defun test-provider-rate-limits ()
  "Test rate limit header parsing into portable snapshots."
  (let ((snapshot (provider-rate-limit-snapshot
                   '(("x-codex-primary-used-percent" . "28.5")
                     ("x-codex-primary-window-minutes" . "300")
                     ("x-codex-primary-reset-at" . "1783000000")
                     ("x-codex-secondary-used-percent" . "45")
                     ("x-codex-secondary-window-minutes" . "10080")))))
    (test-assert (= (getf (getf snapshot :primary) :window-minutes) 300)
                 "primary rate limit windows parse their minutes")
    (test-assert (= (round (* 10 (getf (getf snapshot :primary)
                                       :used-percent)))
                    285)
                 "decimal used percents parse without the Lisp reader")
    (test-assert (= (getf (getf snapshot :primary) :resets-at)
                    (unix-time->universal-time 1783000000))
                 "reset times convert from the POSIX epoch")
    (test-assert (= (getf (getf snapshot :secondary) :used-percent) 45)
                 "secondary rate limit windows parse")
    (test-assert (null (getf (getf snapshot :secondary) :resets-at))
                 "missing reset headers stay absent"))
  (test-assert (null (provider-rate-limit-snapshot
                      '(("content-type" . "text/event-stream"))))
               "absent rate limit headers produce no snapshot")
  (test-assert
   (search "model_not_found means this model is unavailable"
           (provider--http-error-message
            404
            "{\"error\":{\"message\":\"model_not_found means this model is unavailable\"}}"))
   "HTTP errors surface the provider's own explanation")
  (test-assert
   (search "not being served"
           (provider--http-error-message 404 nil))
   "HTTP 404 carries a human hint even without a body")
  (test-assert
   (search "rate limit"
           (provider--http-error-message 429 "plain text overload"))
   "HTTP 429 explains itself and keeps the raw body")
  ;; A streaming request leaves the dependency's failure body undrained, so
  ;; the stream and octet shapes must surface exactly like a string does.
  (test-assert
   (string= (provider--error-body-text
             (make-string-input-stream "streamed explanation"))
            "streamed explanation")
   "an undrained failure body stream becomes displayable text")
  (test-assert
   (string= (provider--error-body-text
             (sb-ext:string-to-octets "encoded explanation"
                                      :external-format ':utf-8))
            "encoded explanation")
   "an undecoded failure body vector becomes displayable text")
  (test-assert
   (null (provider--error-body-text nil))
   "an absent failure body stays absent")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration))
         (condition
           (make-condition
            'http-request-failed
            :body "rate limited"
            :status 429
            :headers '(("x-request-id" . "request-429")
                       ("x-codex-primary-used-percent" . "100")
                       ("x-codex-primary-window-minutes" . "300"))
            :uri nil
            :method ':post)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-error (error)
                (and (= (provider-error-status error) 429)
                     (string= (provider-error-request-id error)
                              "request-429"))))
            "HTTP 429 remains a typed provider failure")
           (test-assert
            (= (getf (getf (provider-rate-limits provider) :primary)
                       :used-percent)
               100)
            "HTTP error headers refresh the visible rate limit snapshot")
           (let ((stream-condition
                   (make-condition
                    'http-request-failed
                    :body (make-string-input-stream
                           "{\"error\":{\"message\":\"input item is not supported\"}}")
                    :status 400
                    :headers '(("request-id" . "request-400"))
                    :uri nil
                    :method ':post)))
             (test-assert
              (handler-case
                  (progn
                    (provider-signal-http-failure provider stream-condition)
                    nil)
                 (provider-error (error)
                   (and (= (provider-error-status error) 400)
                        (string= (provider-error-request-id error)
                                 "request-400")
                        (search "input item is not supported"
                                (format nil "~A" error))
                        (search "input item is not supported"
                                (or (provider-error-response error) ""))
                        t)))
              "a streamed failure body reaches both the message and the response"))
            (test-assert
             (handler-case
                 (progn
                   (provider--signal-http-status-failure
                    provider 400
                    :headers '(("request-id" . "request-direct"))
                    :raw-body "direct failure")
                   nil)
               (provider-error (error)
                 (and (= (provider-error-status error) 400)
                      (string= (provider-error-request-id error)
                               "request-direct"))))
             "direct HTTP status failures retain the common request-id header")
           (dolist (status '(500 502 503 504 507))
             (let ((transient-condition
                     (make-condition
                      'http-request-failed
                      :body "temporary provider failure"
                      :status status
                      :headers nil
                      :uri nil
                      :method ':post)))
               (test-assert
                (handler-case
                    (progn
                      (provider-signal-http-failure
                       provider transient-condition)
                      nil)
                  (provider-retryable-error (error)
                    (and (= (provider-error-status error) status)
                         (search "temporary provider failure"
                                 (or (provider-error-response error) ""))
                         (search "provider service is having trouble"
                                 (format nil "~A" error)
                                 :test #'char-equal)
                         t)))
                (format nil "HTTP ~D is eligible for bounded retry" status)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--request-tools (json-object) vector)
(defun provider-tests--request-tools (request)
  "Return the tools from REQUEST's leading additional-tools item."
  (json-get (aref (json-get request "input") 0) "tools"))

(-> provider-tests--request-tool-of-type
    (json-object string) (option json-object))
(defun provider-tests--request-tool-of-type (request type)
  "Return REQUEST's first additional tool whose type equals TYPE."
  (find type
        (provider-tests--request-tools request)
        :key (lambda (tool)
               (and (json-object-p tool)
                    (json-get tool "type")))
        :test #'equal))

(-> test-provider-request () null)
(defun test-provider-request ()
  "Test the Sol Responses Lite request shape without network access."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration
                                 :working-directory root)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "request-shape"))
                (provider (provider-create configuration))
                (schemas (json-array
                          (json-object
                           "type" "namespace"
                           "name" "test"
                           "description" "Test tools."
                           "tools" (json-array))))
                (request nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert (provider-child-reference-history-p provider)
                        "Codex enables inherited child reference history")
           (test-assert
            (not (provider-child-reference-history-p
                  (make-instance 'model-provider)))
            "providers opt into inherited child reference history explicitly")
           (test-assert (null (json-get request "service_tier"))
                        "requests never select a provider service tier")
           (test-assert (null (json-get request "max_output_tokens"))
                        "requests omit the output ceiling when none is bound")
           (let ((bounded (let ((*provider-maximum-output-tokens* 4242))
                            (provider-request-object provider conversation
                                                     schemas))))
             (test-assert (= (json-get bounded "max_output_tokens") 4242)
                          "a bound output ceiling reaches the wire request"))
           (let ((input (json-get request "input")))
             (test-assert (= (length input) 3)
                          "the provider request prefixes two developer items")
             (test-assert
              (string= (json-get (aref input 0) "type") "additional_tools")
              "additional tools are the first input item")
             (test-assert
              (null (provider-tests--request-tool-of-type request "web_search"))
              "requests omit the nonfunctional native web search tool")
             (test-assert
              (string= (json-get (aref input 1) "role") "developer")
              "the Autolith system prompt is the second input item")
             (test-assert (string= (json-get (aref input 2) "role") "user")
                          "conversation history follows the developer prefix"))
           (let* ((goal-request
                    (provider-request-object
                     provider conversation schemas
                     :goal-context "<goal_context>persist</goal_context>"))
                  (goal-input (json-get goal-request "input"))
                  (goal-message (aref goal-input 2)))
             (test-assert (= (length goal-input) 4)
                          "an active goal adds one transient developer message")
             (test-assert (string= (json-get goal-message "role") "developer")
                          "the goal context is a developer message")
             (test-assert
              (search "<goal_context>"
                      (json-get (aref (json-get goal-message "content") 0)
                                "text"))
              "the goal context rides as developer text"))
           (test-assert
            (null (provider-web-search-tool configuration))
            "the nonfunctional native web search tool stays disabled")
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "max")
            "the provider request maps Ultra reasoning to Max")
           (dolist (model (remove-if-not (lambda (candidate)
                                           (eq (model-family candidate) ':codex))
                                         *supported-models*))
             (dolist (effort *supported-reasoning-efforts*)
               (let* ((selected
                        (configuration-with-reasoning-effort
                         (configuration-with-model configuration model)
                         effort))
                      (selected-provider (provider-create selected))
                      (selected-request
                        (provider-request-object
                         selected-provider conversation schemas))
                      (wire-effort (if (string= effort "ultra")
                                       "max"
                                       effort)))
                 (test-assert
                  (and (string= (json-get selected-request "model") model)
                       (string= (json-get
                                 (json-get selected-request "reasoning")
                                 "effort")
                                wire-effort))
                  (format nil "~A at ~A has the correct request identity"
                          model effort)))))
           (multiple-value-bind (value present-p)
               (gethash "summary" (json-get request "reasoning"))
             (declare (ignore value))
             (test-assert (not present-p)
                          "hidden traces do not request reasoning summaries"))
           (let* ((trace-provider
                    (provider-create configuration :reasoning-summaries-p t))
                  (trace-request
                    (provider-request-object
                     trace-provider conversation schemas))
                  (trace-reasoning (json-get trace-request "reasoning")))
             (test-assert
              (string= (json-get trace-reasoning "summary") "auto")
              "visible traces request the best supported reasoning summary")
             (let ((compaction-reasoning
                     (json-get
                      (provider-request-object
                       trace-provider conversation schemas :compaction-p t)
                      "reasoning")))
               (multiple-value-bind (value present-p)
                   (gethash "summary" compaction-reasoning)
                 (declare (ignore value))
                 (test-assert
                  (not present-p)
                  "side-channel compaction does not request unused summaries")))
             (let ((reconfigured
                     (provider-with-configuration trace-provider configuration)))
               (test-assert
                (provider-reasoning-summaries-p reconfigured)
                "provider reconfiguration preserves the trace preference")))
           (test-assert
            (string= (json-get (json-get request "reasoning") "context")
                     "all_turns")
            "the provider request retains reasoning across the current context")
           (test-assert (string= (json-get request "tool_choice") "auto")
                        "the provider request permits automatic tool selection")
           (test-assert (eq (json-get request "parallel_tool_calls") false)
                        "the provider request disables parallel tool calls")
           (test-assert (eq (json-get request "store") false)
                        "the provider request disables server-side storage")
           (test-assert (eq (json-get request "stream") t)
                        "the provider request enables event streaming")
           (test-assert
            (equalp (json-get request "include")
                    (json-array "reasoning.encrypted_content"))
            "the provider request retains encrypted reasoning for replay")
           (test-assert
            (string= (json-get request "prompt_cache_key")
                     (conversation-identifier conversation))
            "the root conversation is the stable prompt cache key")
           (let* ((child-conversation
                    (conversation-create
                     configuration
                     :identifier "child-request-shape"
                     :prompt-cache-key
                     (conversation-prompt-cache-key conversation)))
                  (child-provider
                    (provider-with-configuration provider configuration)))
             (conversation-append-user-message child-conversation "inspect")
             (let ((child-request
                     (provider-request-object
                      child-provider child-conversation schemas)))
               (test-assert
                (not (string= (conversation-identifier child-conversation)
                              (conversation-identifier conversation)))
                "parent and child conversations keep distinct thread identities")
               (test-assert
                (string= (json-get child-request "prompt_cache_key")
                         (json-get request "prompt_cache_key"))
                "reconfigured child providers share the parent prompt cache key")))
           (let* ((other-conversation
                    (conversation-create configuration
                                         :identifier "other-request-shape"))
                  (other-request
                    (provider-request-object provider other-conversation schemas)))
             (test-assert
              (not (string= (json-get other-request "prompt_cache_key")
                            (json-get request "prompt_cache_key")))
              "independent root conversations keep isolated prompt cache keys"))
           (test-assert
            (string= (json-get (json-get request "text") "verbosity") "low")
            "the provider request asks for restrained text verbosity")
           (multiple-value-bind (value present-p)
               (gethash "instructions" request)
             (declare (ignore value))
             (test-assert (not present-p)
                          "Responses Lite omits top-level instructions"))
           (multiple-value-bind (value present-p)
               (gethash "tools" request)
             (declare (ignore value))
             (test-assert (not present-p)
                          "Responses Lite omits top-level tools")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-native-compaction () null)
(defun test-provider-native-compaction ()
  "Test the Codex native checkpoint request, transport, and fallback boundary."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "native-request"))
         (provider (provider-create configuration))
         (credentials (provider-tests--credentials configuration))
         (schemas
           (json-array
            (json-object "type" "namespace"
                         "name" "test"
                         "description" "Test tools."
                         "tools" (json-array)))))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "retain this work")
           (conversation-append-provider-item
            conversation
            (json-object "type" "reasoning"
                         "encrypted_content" "retained-reasoning"
                         "summary" (json-array)))
           (conversation-append-provider-item
            conversation
            (json-object "type" "function_call"
                         "call_id" "ephemeral-call"
                         "name" "test")
            :persistence ':next-response)
           (let* ((request
                    (provider-native-compaction-request-object
                     provider conversation schemas))
                  (input (json-get request "input")))
             (test-assert (= (length input) 4)
                          "native compaction carries prefix and durable history")
             (test-assert
              (string= (json-get (aref input 0) "type") "additional_tools")
              "native compaction retains the Responses Lite tool prefix")
             (test-assert
              (null (provider-tests--request-tool-of-type request "web_search"))
              "native compaction omits the nonfunctional web search tool")
             (test-assert
              (find "test"
                    (provider-tests--request-tools request)
                    :key (lambda (tool) (json-get tool "name"))
                    :test #'equal)
              "ordinary native compaction retains namespace schemas")

             (test-assert
              (string= (json-get (aref input 1) "role") "developer")
              "native compaction retains the system developer message")
             (test-assert
              (and (reasoning-item-p (aref input 3))
                   (string= (json-get (aref input 3) "encrypted_content")
                            "retained-reasoning"))
              "native compaction carries retained encrypted reasoning")
             (test-assert
              (not (find-if
                    (lambda (item)
                      (string= (or (json-get item "type") "")
                               "compaction_trigger"))
                    (coerce input 'list)))
              "native compaction leaves the endpoint to select its own trigger")
             (test-assert
              (string= (json-get (json-get request "reasoning") "context")
                       "all_turns")
              "native compaction retains reasoning across its input")
             (test-assert
              (string= (json-get request "prompt_cache_key")
                       (conversation-prompt-cache-key conversation))
              "native compaction shares the root conversation cache key")
             (dolist (name '("stream" "store" "include" "tool_choice"))
               (multiple-value-bind (value present-p) (gethash name request)
                 (declare (ignore value))
                 (test-assert (not present-p)
                              (format nil "native compaction omits ~A" name)))))
           (setf (provider-reasoning-summaries-p provider) t)
           (test-assert
            (string= (json-get
                      (json-get
                       (provider-native-compaction-request-object
                        provider conversation schemas)
                       "reasoning")
                      "summary")
                     "auto")
            "native compaction preserves enabled reasoning summaries")
           (setf (provider-reasoning-summaries-p provider) nil)
           (let ((captured-url nil)
                 (captured-headers nil)
                 (captured-content nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (url &key headers content &allow-other-keys)
                  (setf captured-url url
                        captured-headers headers
                        captured-content content)
                  (values "{\"output\":[]}" 200 nil))))
              (lambda ()
                (provider-open-native-compaction
                 provider
                 (json-object "model" *default-model*)
                 :credentials credentials
                 :conversation conversation)))
             (flet ((header (name)
                      (rest (assoc name captured-headers :test #'string-equal))))
               (test-assert
                (string= captured-url
                         "https://chatgpt.com/backend-api/codex/responses/compact")
                "native compaction derives the Responses compact endpoint")
               (test-assert (string= (header "Accept") "application/json")
                            "native compaction requests one JSON response")
               (test-assert
                (typep captured-content '(array (unsigned-byte 8) (*)))
                "native compaction sends JSON as direct UTF-8 octets")
               (test-assert
                (string=
                 (json-get
                  (json-decode
                   (sb-ext:octets-to-string
                    captured-content :external-format ':utf-8))
                  "model")
                 *default-model*)
                "direct UTF-8 request content retains its JSON payload")
               (test-assert
                (string= (header "Authorization")
                         "Bearer provider-test-access-7f386d")
                "native compaction uses the ordinary subscription credentials")))
           (credential-source-save
            (credential-manager-primary-source
             (provider-credential-manager provider))
            credentials)
           (let ((result nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values
                   (json-encode
                   (json-object
                     "output"
                     (json-array
                      (json-object
                       "type" "message"
                       "role" "assistant"
                       "content"
                       (json-array
                        (json-object "type" "output_text"
                                     "text" "Ignored compact reply.")))
                      (json-object "id" "cmp_123"
                                   "type" "compaction"
                                   "encrypted_content" "opaque-checkpoint"))))
                   200
                   '(("x-request-id" . "native-success"))))))
              (lambda ()
                (setf result
                      (provider-native-compact-conversation
                       provider conversation
                       :tool-namespaces schemas
                       :event-callback #'identity))))
             (test-assert (native-compaction-item-p result)
                          "native compaction finds its opaque checkpoint among output")
             (multiple-value-bind (value present-p) (gethash "id" result)
               (declare (ignore value))
               (test-assert (not present-p)
                            "native compaction drops transient server item identifiers")))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")
                       (json-object "type" "context_compaction"
                                    "encrypted_content" "current-checkpoint")))))))
             (test-assert
              (and (native-compaction-item-p result)
                   (string= (json-get result "type") "context_compaction")
                   (string= (json-get result "encrypted_content")
                            "current-checkpoint"))
              "native compaction keeps the newest current checkpoint encoding"))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")))))))
             (test-assert
              (string= (json-get result "type") "compaction")
              "native compaction canonicalizes Codex's legacy checkpoint alias"))
           (test-assert
            (null
             (provider--decode-native-compaction-response
              provider (json-encode (json-object "output" (json-array)))))
            "an opaque-free compact transcript falls back to the portable summary")
           (test-call-with-function-replacements
            (list
             (list
              'dexador:post
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (values "not found" 404 nil))))
            (lambda ()
              (test-assert
               (null
                (provider-native-compact-conversation
                 provider conversation
                 :tool-namespaces schemas
                 :event-callback #'identity))
               "an unavailable native endpoint falls back without failing compaction"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-sse-event-string (json-object) string)
(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

(defclass test-character-input-stream
    (sb-gray:fundamental-character-input-stream)
  ((source
    :initarg :source
    :reader test-character-input-stream-source
    :type string
    :documentation "The deterministic character source.")
   (position
    :initform 0
    :accessor test-character-input-stream-position
    :type integer
    :documentation "The next source character offset."))
  (:documentation "A test stream implementing character reads but not line reads."))

(defmethod sb-gray:stream-read-char ((stream test-character-input-stream))
  "Read one character from STREAM, returning the Gray-stream EOF marker at its end."
  (let ((position (test-character-input-stream-position stream))
        (source (test-character-input-stream-source stream)))
    (if (< position (length source))
        (prog1 (char source position)
          (incf (test-character-input-stream-position stream)))
        :eof)))

(defclass test-failing-close-stream (test-character-input-stream)
  ((close-abort-p
    :initform nil
    :accessor test-failing-close-stream-close-abort-p
    :type boolean
    :documentation "Whether the attempted close was abortive."))
  (:documentation "A deterministic provider stream whose close operation fails."))

(defmethod close ((stream test-failing-close-stream) &key abort)
  "Record ABORT and inject the low-level TLS cleanup failure under test."
  (setf (test-failing-close-stream-close-abort-p stream)
        (not (null abort)))
  (error 'ssl-error-syscall
         :queue nil
         :printed-queue nil
         :ret -1
         :handle nil
         :syscall 'close))

(-> test-provider-stream-decoding () null)
(defun test-provider-stream-decoding ()
  "Test semantic stream decoding from a deterministic SSE fixture."
  (let* ((message-item
           (json-object
            "id" "ephemeral-item-id"
            "type" "message"
            "role" "assistant"
            "content" (json-array
                       (json-object "type" "output_text" "text" "hello"))))
         (reasoning-item
           (json-object
            "id" "ephemeral-reasoning-id"
            "type" "reasoning"
            "summary" (json-array
                       (json-object "type" "summary_text"
                                    "text" "I inspected the request.")
                       (json-object "type" "summary_text"
                                    "text" "I chose a safe response."))
            "content" (json-array
                       (json-object "type" "reasoning_text"
                                    "text" "raw private reasoning"))
            "encrypted_content" "opaque-test-ciphertext"))
         (source
           (concatenate
            'string
            (test-sse-event-string
             (json-object
              "type" "response.created"
              "response" (json-object "id" "response-1")))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 0
              "delta" "I inspected "))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 0
              "delta" "the request."))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 1
              "delta" "I chose a safe response."))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_text.delta"
              "delta" "raw private reasoning"))
            (test-sse-event-string
             (json-object
              "type" "response.function_call_arguments.delta"
              "item_id" "call-progress"
              "delta" "{\"path\":"))
            (test-sse-event-string
             (json-object "type" "response.output_text.delta" "delta" "hello"))
            (test-sse-event-string
             (json-object
              "type" "response.output_item.done"
              "item" message-item))
            (test-sse-event-string
             (json-object
              "type" "response.output_item.done"
              "item" reasoning-item))
            (test-sse-event-string
             (json-object
              "type" "response.completed"
              "response" (json-object
                           "id" "response-1"
                           "end_turn" false
                           "usage" (json-object "input_tokens" 5))))))
         (events nil)
         (result
           (provider-consume-stream
            (make-instance 'model-provider)
            (make-instance 'test-character-input-stream :source source)
            '(("x-codex-turn-state" . "turn-state-1"))
            (lambda (event)
              (push event events)))))
    (test-assert (= (length (provider-result-output-items result)) 2)
                 "the stream retains authoritative completed items in wire order")
    (test-assert (string= (provider-result-response-id result) "response-1")
                 "the stream retains its response identifier")
    (test-assert (string= (provider-result-turn-state result) "turn-state-1")
                 "the stream retains request-local turn state")
    (test-assert (eq (provider-result-turn-completion result) :continue)
                 "the stream retains an explicit provider continuation")
    (test-assert (not (gethash "id"
                               (first (provider-result-output-items result))))
                 "completed response items discard transient server identifiers")
    (test-assert
     (string= (json-get (second (provider-result-output-items result))
                        "encrypted_content")
              "opaque-test-ciphertext")
     "completed encrypted reasoning remains available for replay")
    (let* ((reasoning-output (second (provider-result-output-items result)))
           (summary (response-item-reasoning-summary reasoning-output)))
      (test-assert
       (string= summary
                (format nil "I inspected the request.~2%I chose a safe response."))
       "completed reasoning exposes only its dedicated visible summary")
      (test-assert (not (search "raw private reasoning" summary))
                   "raw reasoning content is never folded into the summary"))
    (let* ((reasoning-events
             (reverse
              (remove-if-not (lambda (event)
                               (typep event 'reasoning-delta-event))
                             events)))
           (streamed-summary
             (format nil
                     "~{~A~}"
                     (mapcar #'reasoning-delta-event-text reasoning-events))))
      (test-assert (= (length reasoning-events) 3)
                   "only summary deltas become visible reasoning events")
      (test-assert
       (string= streamed-summary
                (format nil
                        "I inspected the request.~2%I chose a safe response."))
       "summary part boundaries match the authoritative completed text"))
    (test-assert (= (count-if (lambda (event)
                                (typep event 'provider-progress-event))
                              events)
                    3)
                 "non-presentational stream events still report provider progress")
    (test-assert (= (length events) 10)
                 "the stream emits safe deltas, items, and completion events"))
  nil)

(-> test-provider-stream-failures () null)
(defun test-provider-stream-failures ()
  "Test failed and truncated streams become typed provider conditions."
  (dolist (source
           (list
            (test-sse-event-string
             (json-object "type" "response.failed"
                          "response" (json-object "id" "failed-response")))
            (test-sse-event-string
             (json-object "type" "response.output_text.delta" "delta" "partial"))
            "data: {\"type\":\"response.output_text.delta\",\"delta\":\"partial"))
    (test-assert
     (handler-case
         (progn
           (provider-consume-stream
            (make-instance 'model-provider)
            (make-instance 'test-character-input-stream :source source)
            nil
            (lambda (event)
              (declare (ignore event))))
           nil)
       (provider-error ()
         t))
     "failed and unterminated SSE streams signal typed provider errors"))
  nil)

(-> test-provider-stream-error-classification () null)
(defun test-provider-stream-error-classification ()
  "Test structured SSE failures retain details and only transient codes retry."
  (let* ((source
           (test-sse-event-string
            (json-object
             "type" "response.failed"
             "response"
             (json-object
              "id" "failed-response"
              "error"
              (json-object
               "code" "server_error"
               "message" "Temporary provider failure."
               "request_id" "request-from-event")))))
         (condition
           (handler-case
               (progn
                 (provider-consume-stream
                  (make-instance 'model-provider)
                  (make-instance 'test-character-input-stream :source source)
                  nil
                  #'identity)
                 nil)
             (provider-error (error)
               error))))
    (test-assert (typep condition 'provider-retryable-error)
                 "response.failed server errors are retryable")
    (test-assert (string= (provider-error-code condition) "server_error")
                 "response.failed retains its structured error code")
    (test-assert
     (string= (provider-error-request-id condition) "request-from-event")
     "response.failed retains its structured request identifier")
    (test-assert
     (string= (provider-error-response-id condition) "failed-response")
     "response.failed keeps its response identifier distinct")
    (test-assert (search "Temporary provider failure." (format nil "~A" condition))
                 "response.failed surfaces the provider's explanation"))
  (let* ((source
           (test-sse-event-string
            (json-object
             "type" "response.incomplete"
             "response"
             (json-object
              "id" "incomplete-response"
              "incomplete_details"
              (json-object "reason" "max_output_tokens")))))
         (condition
           (handler-case
               (progn
                 (provider-consume-stream
                  (make-instance 'model-provider)
                  (make-instance 'test-character-input-stream :source source)
                  nil
                  #'identity)
                 nil)
             (provider-incomplete-response (error)
               error))))
    (test-assert
     (and condition
          (typep condition 'provider-retryable-error)
          (string= (provider-error-code condition) "response_incomplete")
          (string= (provider-incomplete-response-reason condition)
                   "max_output_tokens")
          (string= (provider-error-response-id condition)
                   "incomplete-response")
          (search "max_output_tokens" (format nil "~A" condition)))
     "response.incomplete retains its reason and retries as a typed failure"))
  (dolist (code '("server_is_overloaded" "slow_down" "rate_limit_exceeded"))
    (let* ((source
             (test-sse-event-string
              (json-object
               "type" "response.failed"
               "response"
               (json-object
                "id" "overloaded-response"
                "error"
                (json-object
                 "code" code
                 "message" "The service is temporarily overloaded.")))))
           (condition
             (handler-case
                 (progn
                   (provider-consume-stream
                    (make-instance 'model-provider)
                    (make-instance 'test-character-input-stream :source source)
                    nil
                    #'identity)
                   nil)
               (provider-error (error)
                 error))))
      (test-assert
       (typep condition 'provider-retryable-error)
       (format nil "~A response failures are retryable" code))
      (test-assert
       (string= (provider-error-code condition) code)
       (format nil "~A response failures retain their code" code))
      (when (string= code "rate_limit_exceeded")
        (test-assert
         (provider-rate-limit-error-p condition)
         "streamed rate-limit failures retain their exhausted-allowance class"))))
  (let* ((source
           (test-sse-event-string
            (json-object
             "type" "error"
             "code" "server_error"
             "message" "Please retry the request.")))
         (condition
           (handler-case
               (progn
                 (provider-consume-stream
                  (make-instance 'model-provider)
                  (make-instance 'test-character-input-stream :source source)
                  '(("x-request-id" . "request-from-header"))
                  #'identity)
                 nil)
             (provider-error (error)
               error))))
    (test-assert (typep condition 'provider-retryable-error)
                 "top-level server error events are retryable")
    (test-assert
     (string= (provider-error-request-id condition) "request-from-header")
     "top-level errors fall back to the response request header")
    (test-assert (search "Please retry the request." (format nil "~A" condition))
                 "top-level errors surface the provider's explanation"))
  (let* ((source
           (test-sse-event-string
            (json-object
             "type" "response.failed"
             "response"
             (json-object
              "id" "invalid-response"
              "error"
              (json-object
               "code" "invalid_prompt"
               "message" "The prompt is invalid.")))))
         (condition
           (handler-case
               (progn
                 (provider-consume-stream
                  (make-instance 'model-provider)
                  (make-instance 'test-character-input-stream :source source)
                  nil
                  #'identity)
                 nil)
             (provider-error (error)
               error))))
    (test-assert
     (and (typep condition 'provider-error)
          (not (typep condition 'provider-retryable-error)))
     "invalid prompt failures remain terminal")
    (test-assert (string= (provider-error-code condition) "invalid_prompt")
                 "terminal failures retain their structured error code"))
  nil)

(defclass test-transport-provider (codex-subscription-provider)
  ((outcomes
    :initarg :outcomes
    :accessor test-transport-provider-outcomes
    :type list
    :documentation "The connection outcomes returned or signaled in order.")
   (attempt-count
    :initform 0
    :accessor test-transport-provider-attempt-count
    :type integer
    :documentation "The number of response streams requested."))
  (:documentation "A subscription provider injecting transport boundary outcomes."))

(defmethod provider-open-response-stream
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Return or signal the next scripted transport outcome for PROVIDER."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (cond
      ((eq outcome ':syscall)
       (error 'ssl-error-syscall
              :queue nil
              :printed-queue nil
              :ret -1
              :handle nil
              :syscall 'connect))
      ((eq outcome ':tls)
       (error 'cl+ssl-error))
      ((eq outcome ':name-service)
       (error 'sb-bsd-sockets:name-service-error
              :errno 1
              :symbol 'getaddrinfo
              :syscall 'getaddrinfo))
      ((eq outcome ':simple-error)
       (error "Synthetic provider transport failure."))
      ((consp outcome)
       (values-list outcome))
      (t
       (values outcome 200 nil)))))

(defmethod provider-open-native-compaction
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Signal the next scripted transport outcome for native compaction."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (if (eq outcome ':name-service)
        (error 'sb-bsd-sockets:name-service-error
               :errno 1
               :symbol 'getaddrinfo
               :syscall 'getaddrinfo)
        (values outcome 200 nil))))

(-> provider-tests--completed-sse-source (string) string)
(defun provider-tests--completed-sse-source (response-id)
  "Return a minimal successful SSE response carrying RESPONSE-ID."
  (concatenate
   'string
   (test-sse-event-string
    (json-object
     "type" "response.created"
     "response" (json-object "id" response-id)))
   (test-sse-event-string
    (json-object
     "type" "response.completed"
     "response" (json-object
                  "id" response-id
                  "usage" (json-object "input_tokens" 1))))))

(-> provider-tests--transport-provider
    (configuration list)
    test-transport-provider)
(defun provider-tests--transport-provider (configuration outcomes)
  "Return a test provider yielding transport OUTCOMES."
  (make-instance
   'test-transport-provider
   :configuration configuration
   :credential-manager (credential-manager-create configuration)
   :session-id (make-identifier)
   :outcomes outcomes))

(-> test-provider-transport-boundary () null)
(defun test-provider-transport-boundary ()
  "Test connection normalization and failure-proof provider stream cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-transport"))
         (credentials (provider-tests--credentials configuration)))
    (unwind-protect
         (progn
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "transport-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list :syscall success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*provider-stream-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "transport-retry-success")
                  "an open-time TLS syscall failure reconnects successfully")
                 (test-assert
                 (= (test-transport-provider-attempt-count provider) 2)
                  "a transient open failure consumes one bounded retry"))))
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "name-service-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list ':name-service success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*provider-stream-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "name-service-retry-success")
                  "an SBCL name-service error reconnects successfully")
                 (test-assert
                  (= (test-transport-provider-attempt-count provider) 2)
                  "an SBCL name-service error remains inside the retry boundary"))))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list ':name-service))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-native-compaction
                     provider
                     (json-object)
                     :credentials credentials
                     :conversation conversation)
                    nil)
                (provider-transport-error ()
                  t))
              "native compaction normalizes an SBCL name-service error"))
            (let ((provider
                    (provider-tests--transport-provider
                     configuration
                     (list ':simple-error))))
              (test-assert
               (handler-case
                   (progn
                     (provider--open-response-stream
                      provider
                      (json-object)
                      :credentials credentials
                      :conversation conversation)
                     nil)
                 (provider-error (condition)
                   (and (not (typep condition 'provider-retryable-error))
                        (string=
                         (autolith-error-message condition)
                         "The provider transport failed before a response was received."))))
               "a raw transport SIMPLE-ERROR becomes a terminal provider failure"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source
                     (provider-tests--completed-sse-source
                      "close-failure-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((result
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback #'identity
                      :force-refresh nil
                      :goal-context nil
                      :compaction-p nil)))
               (test-assert
                (string= (provider-result-response-id result)
                         "close-failure-success")
                "cleanup failure cannot replace a completed provider result")
               (test-assert
                (test-failing-close-stream-close-abort-p stream)
                "provider response streams close abortively")))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "data: {"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity
                     :force-refresh nil
                     :goal-context nil
                     :compaction-p nil)
                    nil)
                (response-stream-error ()
                  t))
              "cleanup failure preserves the original stream interruption")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "interrupted provider streams also close abortively"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "retry buffer failure"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list (list stream 507 nil)))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (test-call-with-function-replacements
                   (list
                    (list
                     'provider--signal-http-status-failure
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "synthetic status normalization failure"))))
                   (lambda ()
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback #'identity
                      :force-refresh nil
                      :goal-context nil
                      :compaction-p nil)))
                (simple-error ()
                  t))
              "status normalization failures retain their original condition")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "non-success provider responses remain under stream cleanup"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list :tls))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity
                     :force-refresh nil
                     :goal-context nil
                     :compaction-p nil)
                    nil)
                (provider-error (condition)
                  (not (typep condition 'provider-retryable-error))))
              "non-transient TLS setup failures remain typed and terminal")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--credentials (configuration) oauth-credentials)
(defun provider-tests--credentials (configuration)
  "Return four distinct synthetic credentials for provider containment tests."
  (make-instance
   'oauth-credentials
   :access-token "provider-test-access-7f386d"
   :refresh-token "provider-test-refresh-a280c4"
   :id-token "provider-test-identity-f969b1"
   :account-id "provider-test-account-a0542e"
   :expires-at nil
   :source-path (configuration-auth-path configuration)))

(-> provider-tests--assert-credential-free (t list string) null)
(defun provider-tests--assert-credential-free (root secrets description)
  "Assert ROOT contains none of SECRETS, reporting DESCRIPTION."
  (dolist (secret secrets)
    (test-assert
     (not (test-object-contains-string-p root secret))
     description))
  nil)

(-> test-provider-credential-echo-containment () null)
(defun test-provider-credential-echo-containment ()
  "Test provider wire data cannot echo request credentials into retained state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-secret-echo"))
         (provider (provider-create configuration))
         (credentials (provider-tests--credentials configuration))
         (secrets (oauth-credentials-secret-values credentials)))
    (labels
        ((attempt (response-function event-callback)
           "Run one real provider attempt against RESPONSE-FUNCTION."
           (test-call-with-function-replacements
            (list
             (list
              'provider-open-response-stream
              (lambda (active-provider request
                       &key active-credentials active-conversation
                         &allow-other-keys)
                (declare
                 (ignore active-provider request active-credentials
                         active-conversation))
                (funcall response-function))))
            (lambda ()
              (provider-attempt-turn
               provider
               conversation
               :tool-namespaces #()
               :event-callback event-callback
               :force-refresh nil
               :goal-context nil
               :compaction-p nil)))))
      (unwind-protect
           (progn
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let* ((events nil)
                    (source
                      (concatenate
                       'string
                       (test-sse-event-string
                        (json-object
                         "type" "response.created"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_text.delta"
                         "delta"
                         (oauth-credentials-refresh-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.reasoning_summary_text.delta"
                         "output_index" 0
                         "summary_index" 0
                         "delta"
                         (oauth-credentials-id-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_item.done"
                         "item"
                         (json-object
                          "type" "message"
                          "role" "assistant"
                          "content"
                          (json-array
                           (json-object
                            "type" "output_text"
                            "text"
                            (oauth-credentials-account-id credentials))))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.completed"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials)
                          "usage"
                          (json-object
                           "echo"
                           (coerce secrets 'vector)))))))
                    (headers
                      (list
                       (cons "x-request-id"
                             (oauth-credentials-account-id credentials))
                       (cons "x-codex-turn-state"
                             (format nil "~{~A~^/~}" secrets))))
                    (result
                      (attempt
                       (lambda ()
                         (values
                          (make-instance
                           'test-character-input-stream
                           :source source)
                          200
                          headers))
                       (lambda (event)
                         (push event events)))))
               (provider-tests--assert-credential-free
                (list result events)
                secrets
                "successful provider results and callbacks contain no credential")
               (test-assert
                (test-object-contains-string-p
                 (list result events)
                 *provider-credential-redaction-marker*)
                "successful credential echoes carry an explicit redaction marker")
               (test-assert
                (and (provider-result-response-id result)
                     (provider-result-usage result)
                     (provider-result-turn-state result)
                     (find-if
                      (lambda (event)
                        (typep event 'assistant-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'reasoning-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'provider-item-event))
                      events))
                "successful containment retains each semantic provider channel"))
             (let* ((source
                      (test-sse-event-string
                       (json-object
                        "type" "response.failed"
                        "response"
                        (json-object
                         "id" (oauth-credentials-access-token credentials)
                         "error"
                         (json-object
                          "code" "invalid_prompt"
                          "message"
                          (oauth-credentials-refresh-token credentials)
                          "request_id"
                          (oauth-credentials-account-id credentials))))))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                nil))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "structured provider failures contain no credential")
               (test-assert
                (test-object-contains-string-p
                 condition
                 *provider-credential-redaction-marker*)
                "structured provider failures retain a redaction marker"))
             (let* ((source
                      (format
                       nil
                       "data: {\"type\":\"~A~%~%"
                       (oauth-credentials-access-token credentials)))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                (list
                                 (cons
                                  "x-request-id"
                                  (oauth-credentials-id-token credentials)))))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "malformed provider events contain no credential"))
             (dolist (signaled-p '(nil t))
               (let ((condition
                       (handler-case
                           (progn
                             (attempt
                              (lambda ()
                                (let ((headers
                                        (list
                                         (cons
                                          "x-request-id"
                                          (oauth-credentials-id-token
                                           credentials))))
                                      (body
                                        (json-encode
                                         (json-object
                                          "error"
                                          (json-object
                                           "message"
                                           (oauth-credentials-refresh-token
                                            credentials))))))
                                  (if signaled-p
                                      (error
                                       (make-condition
                                        'http-request-failed
                                        :body body
                                        :status 400
                                        :headers headers
                                        :uri nil
                                        :method ':post))
                                      (values
                                       (make-string-input-stream body)
                                       400
                                       headers))))
                              #'identity)
                             nil)
                         (provider-error (failure)
                           failure))))
                 (provider-tests--assert-credential-free
                  condition
                  secrets
                  "HTTP provider failures contain no credential")
                 (test-assert
                  (test-object-contains-string-p
                   condition
                   *provider-credential-redaction-marker*)
                  "HTTP credential echoes carry a redaction marker")))
             (let* ((collision "PROVIDER")
                    (marker
                      (safe-redaction-marker
                       *provider-credential-redaction-marker*
                       (list collision))))
               (test-assert
                (not (search collision marker))
                "credential collisions select a marker without the credential")))
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist ':ignore)))
    (test-assert
     (and (null *provider-active-credential-values*)
          (null *provider-active-credential-redaction-marker*))
     "provider attempts retain no dynamic credential redaction state"))
  nil)

(defclass test-codex-provider (codex-subscription-provider)
  ((outcomes
    :initarg :outcomes
    :accessor test-codex-provider-outcomes
    :type list
    :documentation "The attempt outcomes returned in order.")
   (refresh-flags
    :initform nil
    :accessor test-codex-provider-refresh-flags
    :type list
    :documentation "The force-refresh values observed by attempts."))
  (:documentation "A direct-provider test double for bounded authentication retries."))

(defmethod provider-attempt-turn
    ((provider test-codex-provider)
     (conversation conversation)
     &key
       tool-namespaces
       event-callback
       force-refresh
       goal-context
       compaction-p)
  "Return the next scripted PROVIDER outcome and record FORCE-REFRESH."
  (declare (ignore conversation tool-namespaces event-callback goal-context
                   compaction-p))
  (push force-refresh (test-codex-provider-refresh-flags provider))
  (let ((outcome (pop (test-codex-provider-outcomes provider))))
    (cond
      ((typep outcome 'provider-result)
       outcome)
      ((eq outcome :unauthorized)
       (error 'provider-unauthorized
              :message "Injected unauthorized response."
              :status 401
              :request-id nil
              :response nil))
      ((eq outcome :stream-error)
       (error 'response-stream-error
              :message "Injected stream interruption."
              :status nil
              :request-id nil
              :response nil))
      ((eq outcome :server-error)
       (error 'provider-retryable-error
              :message "Injected transient server failure."
              :status nil
              :code "server_error"
              :request-id "request-server-error"
              :response-id "response-server-error"
              :response nil))
      ((eq outcome :overloaded)
       (error 'provider-retryable-error
              :message "Injected provider overload."
              :status nil
              :code "server_is_overloaded"
              :request-id "request-overloaded"
              :response-id "response-overloaded"
              :response nil))
      ((eq outcome :slow-down)
       (error 'provider-retryable-error
              :message "Injected provider slowdown."
              :status nil
              :code "slow_down"
              :request-id "request-slow-down"
              :response-id "response-slow-down"
              :response nil))
      ((eq outcome :resample)
       (error 'provider-resample-requested
              :message "Injected provider loop report."
              :status nil
              :request-id nil
              :response nil
              :triggers (list "tail_repetition:4@thinking")
              :attempt 1
              :maximum-attempts 2))
      (t
       (error "Invalid scripted provider outcome ~S." outcome)))))

(-> test-codex-provider-create (configuration list) test-codex-provider)
(defun test-codex-provider-create (configuration outcomes)
  "Return a test direct provider yielding OUTCOMES."
  (make-instance 'test-codex-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)
                 :outcomes outcomes))

(-> test-provider-authentication-retries () null)
(defun test-provider-authentication-retries ()
  "Test bounded credential reload, refresh, and final unauthorized normalization."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "provider-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "retry-success"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (progn
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized result))))
             (test-assert
              (eq (provider-stream-turn provider conversation
                                        :tool-namespaces #()
                                        :event-callback #'identity)
                  result)
              "a credential reload may satisfy the first unauthorized response")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil nil))
              "the reload retry does not rotate credentials"))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized :unauthorized result))))
             (test-assert
              (eq (provider-stream-turn provider conversation
                                        :tool-namespaces #()
                                        :event-callback #'identity)
                  result)
              "one forced refresh may satisfy two unauthorized responses")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil nil t))
              "the third and final attempt forces credential refresh"))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    '(:unauthorized :unauthorized :unauthorized))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (authentication-error ()
                  t))
              "a third unauthorized response becomes a typed authentication failure"))
           (let* ((manager
                    (api-key-credential-manager-create
                     :provider-name "retry-api-key"
                     :pathname (configuration-api-keys-path configuration)))
                  (provider
                    (make-instance 'test-codex-provider
                                   :configuration configuration
                                   :credential-manager manager
                                   :session-id (make-identifier)
                                   :outcomes '(:unauthorized :unexpected))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (authentication-error (condition)
                  (search "run autolith auth retry-api-key"
                          (princ-to-string condition))))
              "a rejected static API key gives its direct authentication hint")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil))
              "a rejected static API key is not retried or refreshed")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-stream-retries () null)
(defun test-provider-stream-retries ()
  "Test bounded stream reconnection, observer events, and final failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-stream-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "stream-retry-success"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (let ((*provider-stream-retry-sleep-function*
                 (lambda (seconds)
                   (declare (ignore seconds)))))
           (dolist (failure
                    '(:stream-error :server-error :overloaded :slow-down))
             (let ((events nil)
                   (provider
                     (test-codex-provider-create
                      configuration
                      (list failure result))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback (lambda (event) (push event events)))
                    result)
                "transient stream and server failures retry the provider turn")
               (let ((retry-event
                       (find-if (lambda (event)
                                  (typep event 'provider-retry-event))
                                events)))
                 (test-assert
                  (and retry-event
                       (= (provider-retry-event-attempt retry-event) 1)
                       (= (provider-retry-event-maximum-attempts retry-event)
                          (length *provider-stream-retry-delays*))
                       (= (provider-retry-event-delay retry-event) 1))
                  "provider retries expose their attempt and delay to the observer"))
               (test-assert
                (equal (nreverse
                        (test-codex-provider-refresh-flags provider))
                       '(nil nil))
                "provider retries do not force an authentication refresh")))
           (let ((delays nil)
                 (provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded :overloaded result))))
             (let ((*provider-stream-retry-sleep-function*
                     (lambda (seconds)
                       (push seconds delays))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity)
                    result)
                "provider overload retries may recover")
               (test-assert
                (equal (nreverse delays) '(1 2))
                "provider overload retries use the bounded backoff schedule")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded result))))
             (let ((*provider-stream-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds))
                       (error
                        (make-condition 'application-turn-cancelled)))))
               (test-assert
                (handler-case
                    (progn
                      (provider-stream-turn
                       provider
                       conversation
                       :tool-namespaces #()
                       :event-callback #'identity)
                      nil)
                  (application-turn-cancelled ()
                    t))
                "turn cancellation interrupts provider overload backoff")
               (test-assert
                (= (length
                    (test-codex-provider-refresh-flags provider))
                   1)
                "turn cancellation prevents another provider attempt")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (make-list
                     (1+ (length *provider-stream-retry-delays*))
                     :initial-element :server-error))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider
                                          conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (provider-retryable-error (condition)
                  (string= (provider-error-code condition) "server_error")))
              "retry exhaustion re-signals the final structured failure")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
