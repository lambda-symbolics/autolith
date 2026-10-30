;;;; -- Optional Parent Model Provider --

(in-package #:autolith)

(defparameter *agent-protocol-provider-cancel-predicate* nil
  "Optional zero-argument cancellation predicate for the current parent model call.")

(defclass agent-protocol-provider (model-provider)
  ((configuration :initarg :configuration :reader agent-protocol-provider-configuration
                  :documentation "Agent context configuration, without transport credentials.")
   (driver :initarg :driver :reader agent-protocol-provider-driver
           :documentation "The connection's sole-reader driver.")
   (profile-id :initarg :profile-id :reader agent-protocol-provider-profile-id
               :documentation "The trusted parent profile identifier.")
   (reasoning-capabilities :initarg       :reasoning-capabilities
                           :reader        agent-protocol-provider-reasoning-capabilities
                           :documentation "Public reasoning support from the selected parent profile.")
   (parameters :initarg :parameters :reader agent-protocol-provider-parameters
               :documentation "Explicit approved generation options.")
   (states :initform (make-hash-table :test #'eq) :reader agent-protocol-provider-states
           :documentation "Conversation-local accepted state, scoped to a parent turn.")
   (lock :initform      (bordeaux-threads:make-lock "parent model provider")
         :reader        agent-protocol-provider-lock
         :documentation "Serialize calls and accepted state updates."))
  (:documentation "A credential-free optional adapter. It never retries a request."))

(-> make-agent-protocol-provider
    (configuration t string
     &key (:max-output-tokens (option integer)) (:temperature (option double-float))
           (:reasoning-effort (option string)) (:reasoning-summary (option string))
           (:reasoning-capabilities (option hash-table)))
    agent-protocol-provider)
(defun make-agent-protocol-provider
    (configuration driver profile-id
     &key max-output-tokens temperature reasoning-capabilities
       (reasoning-effort nil effort-p) (reasoning-summary nil summary-p))
  "Build a parent adapter with optional overrides of live generation settings."
  (unless (non-empty-string-p profile-id)
    (error 'agent-protocol-error :reason ':invalid-provider-profile))
  (make-instance 'agent-protocol-provider :configuration configuration :driver driver
                 :profile-id             (copy-seq profile-id)
                 :reasoning-capabilities (agent-protocol-provider--capabilities reasoning-capabilities)
                 :parameters             (append (list :max-output-tokens max-output-tokens :temperature temperature)
                                                 (when effort-p (list :reasoning-effort reasoning-effort))
                                                 (when summary-p (list :reasoning-summary reasoning-summary)))))

(-> agent-protocol-provider--capabilities ((option hash-table)) (option hash-table))
(defun agent-protocol-provider--capabilities (capabilities)
  "Validate and copy the bounded public capability projection, without inferring support."
  (when capabilities
    (let ((copy (make-hash-table :test #'equal)))
      (unless (<= (hash-table-count capabilities) 2)
        (error 'agent-protocol-error :reason ':invalid-reasoning-capabilities))
      (maphash
       (lambda (key values)
         (unless (and (member key '("efforts" "summaries") :test #'equal)
                      (vectorp values) (not (stringp values)) (<= (length values) 64)
                      (every (lambda (value) (and (stringp value) (<= 1 (length value) 128))) values))
           (error 'agent-protocol-error :reason ':invalid-reasoning-capabilities))
         (setf (gethash key copy) (map 'vector #'copy-seq values)))
       capabilities)
      copy)))

(defmethod provider-family ((provider agent-protocol-provider))
  "Keep normalized parent items in their own logical conversation family."
  (declare (ignore provider))
  ':parent)

(defmethod provider-with-configuration ((provider agent-protocol-provider) configuration)
  "Use new context configuration without changing connection or accepted state."
  (let ((copy (apply #'make-agent-protocol-provider configuration
                     (agent-protocol-provider-driver provider)
                     (agent-protocol-provider-profile-id provider)
                     :reasoning-capabilities (agent-protocol-provider-reasoning-capabilities provider)
                     (agent-protocol-provider-parameters provider))))
    (setf (slot-value copy 'states) (agent-protocol-provider-states provider)
          (slot-value copy 'lock) (agent-protocol-provider-lock provider))
    copy))

(-> agent-protocol-provider--failure () nil)
(defun agent-protocol-provider--failure ()
  "Signal a fixed local failure without copying parent diagnostic text."
  (error 'provider-error :message "The parent model request failed."
                         :status nil :request-id nil :response nil))

(-> agent-protocol-provider--metadata ((option agent-wire:response-metadata))
    (option cl-llm-provider-api:provider-response-metadata))
(defun agent-protocol-provider--metadata (metadata)
  "Convert observed response metadata without request-side model guesses."
  (when metadata
    (make-instance 'cl-llm-provider-api:provider-response-metadata
                   :model              (when (agent-wire:response-metadata.has-model metadata)
                                         (agent-wire:response-metadata.model metadata))
                   :finish-reason      (when (agent-wire:response-metadata.has-finish-reason metadata)
                                         (agent-wire:response-metadata.finish-reason metadata))
                   :cache-write-tokens (when (agent-wire:response-metadata.has-cache-write-tokens metadata)
                                         (agent-wire:response-metadata.cache-write-tokens metadata))
                   :usage-observed-p   (agent-wire:response-metadata.usage-observed metadata))))

(-> agent-protocol-provider--usage ((option agent-wire:usage)) t)
(defun agent-protocol-provider--usage (usage)
  "Return the exact public provider usage object when one was supplied."
  (when usage
    (if (agent-wire:usage.has-provider-usage usage)
        (agent-context--json
         (autolith-supervisor::protocol-value->data
          (cl-protobufs.autolith.common.v1:make-value :map-value (agent-wire:usage.provider-usage usage))))
        (let ((result (make-hash-table :test #'equal)))
          (when (agent-wire:usage.has-input-tokens usage)
            (setf (gethash "input_tokens" result) (agent-wire:usage.input-tokens usage)))
          (when (agent-wire:usage.has-output-tokens usage)
            (setf (gethash "output_tokens" result) (agent-wire:usage.output-tokens usage)))
          (when (agent-wire:usage.has-cached-input-tokens usage)
            (setf (gethash "cached_input_tokens" result) (agent-wire:usage.cached-input-tokens usage)))
          (when (agent-wire:usage.has-cache-write-tokens usage)
            (setf (gethash "cache_write_tokens" result) (agent-wire:usage.cache-write-tokens usage)))
          result))))

(-> agent-protocol-provider-item-from-data (hash-table) hash-table)
(defun agent-protocol-provider-item-from-data (data)
  "Normalize one bounded OutputItem map from a validated parent completion or resume.
Keep the detached parent replay wrapper beside the public logical view. This helper
does not establish completion acceptance or authorize an agent-supplied handle."
  (let* ((item   (autolith-supervisor::protocol-value->data
                  (autolith-supervisor::protocol-data->value data)))
         (handle (gethash "replay_handle" item))
         (family (gethash "family" item))
         (views  (remove-if-not (lambda (key) (nth-value 1 (gethash key item)))
                                '("message" "tool_call" "tool_discovery_call" "reasoning"
                                  "hosted_tool" "compaction"))))
    (unless (and (non-empty-string-p handle) (non-empty-string-p family)
                 (typep (gethash "item_index" item) '(integer 0)) (= 1 (length views)))
      (agent-context--fail ':invalid-parent-output-data))
    (let* ((marker (json-object "type" "parent_output" "replay_handle" (copy-seq handle)
                                "family" (copy-seq family) "view" (agent-context--json item)))
           (view   (gethash (first views) item))
           (result
             (cond
               ((equal (first views) "message")
                (unless (and (hash-table-p view) (equal (gethash "role" view) "assistant")
                             (typep (gethash "content" view) 'simple-vector))
                  (agent-context--fail ':invalid-parent-message))
                (json-object "type" "message" "role" "assistant" "content"
                             (map 'vector
                                  (lambda (block)
                                    (unless (hash-table-p block)
                                      (agent-context--fail ':invalid-parent-output-block))
                                    (cond
                                      ((nth-value 1 (gethash "text" block))
                                       (json-object "type" "output_text" "text" (gethash "text" block)))
                                      ((nth-value 1 (gethash "reasoning_text" block))
                                       (json-object "type" "reasoning_text" "text" (gethash "reasoning_text" block)))
                                      (t
                                       (agent-context--fail ':unsupported-parent-output-block))))
                                  (gethash "content" view))))
               ((member (first views) '("tool_call" "tool_discovery_call") :test #'equal)
                (unless (and (hash-table-p view) (non-empty-string-p (gethash "call_id" view))
                             (non-empty-string-p (gethash "name" view))
                             (or (stringp (gethash "arguments_json" view))
                                 (hash-table-p (gethash "arguments" view))))
                  (agent-context--fail ':invalid-parent-tool-call))
                (let ((discovery (equal (first views) "tool_discovery_call")))
                  (json-object "type" (if discovery "tool_search_call" "function_call")
                               "execution" (if discovery "client" nil)
                               "call_id" (gethash "call_id" view) "name" (gethash "name" view)
                               "namespace" (gethash "namespace" view "")
                               "arguments" (if (nth-value 1 (gethash "arguments_json" view))
                                               (gethash "arguments_json" view)
                                               (agent-context--json (gethash "arguments" view))))))
               ((equal (first views) "reasoning")
                (json-object "type" "reasoning" "summary"
                             (vector (json-object "type" "summary_text" "text" (gethash "text" view)))))
               ((equal (first views) "compaction")
                (json-object "type" "parent_compaction"))
               (t
                (json-object "type" "parent_output_view" "view" (agent-context--json item))))))
      (setf (gethash "parent_replay" result) marker)
      result)))

(-> agent-protocol-provider--item (agent-wire:output-item) hash-table)
(defun agent-protocol-provider--item (item)
  "Use the shared portable-output projection for live and resumed results."
  (agent-protocol-provider-item-from-data (autolith-supervisor::protocol-message->data item)))

(-> agent-protocol-provider--result (agent-context-projection agent-wire:model-complete) provider-result)
(defun agent-protocol-provider--result (projection complete)
  "Normalize the terminal result before consuming context delivery receipts."
  (let* ((wire-items (agent-wire:model-complete.output-items complete))
         (items      (mapcar #'agent-protocol-provider--item wire-items))
         (result     (make-instance
                      'provider-result :output-items items
                      :tool-calls                    (remove-if-not #'function-call-item-p items)
                      :response-id                   (agent-wire:model-complete.response-id complete)
                      :usage                         (agent-protocol-provider--usage (agent-wire:model-complete.usage complete))
                      :metadata                      (agent-protocol-provider--metadata (agent-wire:model-complete.metadata complete))
                      :turn-state                    (agent-wire:model-complete.turn-state-handle complete)
                      :turn-completion               (case (agent-wire:model-complete.turn-completion complete)
                                                       (:ended
                                                        ':end)
                                                       (:continued
                                                        ':continue)
                                                       (otherwise
                                                        ':unspecified))))
         (markers    (agent-context-accept projection complete)))
    (loop for item in items for marker in markers
          do (setf (gethash "parent_replay" item) marker))
    result))

(-> agent-protocol-provider--event (agent-wire:model-event function) null)
(defun agent-protocol-provider--event (event callback)
  "Publish display events only. Provisional items cannot authorize persistence or tools."
  (cond
    ((agent-wire:model-event.has-text-delta event)
     (let ((delta (agent-wire:model-event.text-delta event)))
       (funcall callback (make-instance (if (agent-wire:text-delta.reasoning delta)
                                           'reasoning-delta-event 'assistant-delta-event)
                                        :text (agent-wire:text-delta.text delta)))))
    ((agent-wire:model-event.has-metadata event)
     (funcall callback (make-instance 'provider-response-metadata-event
                                      :metadata (agent-protocol-provider--metadata (agent-wire:model-event.metadata event)))))
    ((agent-wire:model-event.has-attempt-started event)
     (let ((started (agent-wire:model-event.attempt-started event)))
       (when (> (agent-wire:model-event.attempt event) 1)
         (funcall callback (make-instance 'provider-retry-event
                                          :attempt          (agent-wire:model-event.attempt event)
                                          :maximum-attempts (agent-wire:attempt-started.maximum-attempts started)
                                          :delay            (/ (agent-wire:attempt-started.delay-milliseconds started) 1000))))))
    ((agent-wire:model-event.has-attempt-failed event)
     (let ((failed (agent-wire:model-event.attempt-failed event)))
       (funcall callback
                (make-instance 'provider-attempt-failed-event
                               :attempt           (agent-wire:model-event.attempt event)
                               :elapsed-seconds   (agent-wire:attempt-failed.elapsed-seconds failed)
                               :output-received-p (agent-wire:attempt-failed.output-received failed)
                               :retryable-p       (agent-wire:attempt-failed.retryable failed)
                               :condition         (make-condition 'provider-error
                                                                  :message "The parent model attempt failed.")))))
    ((or (agent-wire:model-event.has-tool-call-delta event)
         (agent-wire:model-event.has-item-completed event))
     (funcall callback (make-instance 'provider-progress-event)))
    (t
     (funcall callback (make-instance 'provider-progress-event))))
  nil)

(-> agent-protocol-provider--turn-id (agent-protocol-client) (option string))
(defun agent-protocol-provider--turn-id (client)
  "Copy the active turn identity under the client state lock."
  (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
    (let ((turn (agent-protocol-client-turn client)))
      (when turn
        (copy-seq (agent-wire:turn-start.turn-id (agent-wire:envelope.turn-start turn)))))))

(defmethod provider-output-ceiling-p ((provider agent-protocol-provider))
  "Allow the agent to bound each parent generation request."
  (declare (ignore provider))
  t)

(defmethod provider-configuration ((provider agent-protocol-provider))
  "Expose the agent context configuration to existing provider observers."
  (agent-protocol-provider-configuration provider))

(define-condition agent-protocol-native-unavailable (error) ()
  (:documentation "An explicit unsupported native operation permits ordinary summary fallback."))

(defmethod provider-stream-turn
    ((provider agent-protocol-provider) (conversation conversation)
     &key (tool-namespaces #()) (event-callback (constantly nil)) goal-context compaction-p)
  "Generate ordinary output or a portable summary through the parent."
  (agent-protocol-provider--stream-turn provider conversation :tool-namespaces tool-namespaces
    :event-callback event-callback :goal-context goal-context :compaction-p compaction-p))

(defmethod provider-native-compact-conversation
    ((provider agent-protocol-provider) (conversation conversation)
     &key (tool-namespaces #()) (event-callback (constantly nil)))
  "Return an accepted opaque parent checkpoint, or NIL only after explicit unsupported."
  (handler-case
      (let* ((result (agent-protocol-provider--stream-turn provider conversation
                       :tool-namespaces tool-namespaces :event-callback event-callback
                       :compaction-p t :kind ':native-compaction))
             (item   (find-if #'conversation-parent-compaction-item-p (provider-result-output-items result))))
        (unless item (agent-protocol-provider--failure))
        (values item (provider-result-usage result)))
    (agent-protocol-native-unavailable () (values nil nil))))

(-> agent-protocol-provider--stream-turn
    (agent-protocol-provider conversation
     &key (:tool-namespaces vector) (:event-callback function) (:goal-context (option string))
          (:compaction-p boolean) (:kind keyword)) provider-result)
(defun agent-protocol-provider--stream-turn
    (provider conversation
     &key (tool-namespaces #()) (event-callback (constantly nil)) goal-context compaction-p (kind ':generate))
  "Run one parent request through the sole reader. Never retry effects after failure.
Cooperative cancellation drains its ACK and the target terminal. Accepted completion
still consumes context receipts, but cancellation prevents tool-result publication."
  (bordeaux-threads:with-lock-held ((agent-protocol-provider-lock provider))
    (let* ((driver         (agent-protocol-provider-driver provider))
           (client         (agent-protocol-driver-client driver))
           (turn-id        (agent-protocol-provider--turn-id client))
           (state          (gethash conversation (agent-protocol-provider-states provider)))
           (parameters     (copy-list (agent-protocol-provider-parameters provider)))
           (request        nil)
           (cancel-request nil)
           (cancelled      nil)
           (terminal       nil))
      (unless turn-id (agent-protocol-provider--failure))
      (unless (equal (getf state :turn-id) turn-id) (setf state nil))
      (loop for (key name) in '((:reasoning-effort "efforts") (:reasoning-summary "summaries"))
            for capabilities = (agent-protocol-provider-reasoning-capabilities provider)
            for value = (config key (agent-protocol-provider-configuration provider))
            when (and capabilities
                      (find value (gethash name capabilities #()) :test #'equal)
                      (or (eq key ':reasoning-effort) (getf parameters :reasoning-effort))
                      (not (loop for tail on parameters by (lambda (tail) (rest (rest tail)))
                                 thereis (eq (first tail) key))))
              do (setf (getf parameters key) value))
      (when *provider-maximum-output-tokens*
        (setf (getf parameters :max-output-tokens) *provider-maximum-output-tokens*))
      (when compaction-p (remf parameters :reasoning-summary))
      ;; Cleanup shares the acceptance path but never publishes effect events.
      (labels ((observe-cancellation ()
                 "Latch cancellation and send its request once."
                 (when (and *agent-protocol-provider-cancel-predicate*
                            (funcall *agent-protocol-provider-cancel-predicate*))
                   (setf cancelled t))
                 (when (and cancelled request (null cancel-request))
                   (setf cancel-request (agent-protocol-driver-cancel driver request))))

               (finish-cancellation ()
                 "Drain the separate ACK after the target terminal, then unwind."
                 (when cancelled
                   (when cancel-request
                     (multiple-value-bind (envelope terminal-p)
                         (agent-protocol-driver-next cancel-request)
                       (unless (and terminal-p envelope (agent-wire:envelope.has-cancel-ack envelope))
                         (agent-protocol-provider--failure))))
                   (error 'application-turn-cancelled)))

               (emit (event)
                 "Convert callback cancellation into the same cooperative path."
                 (handler-case (funcall event-callback event)
                   (application-turn-cancelled ()
                     (setf cancelled t)))
                 (observe-cancellation)))
        (observe-cancellation)
        (finish-cancellation)
        (let ((projection
                (apply #'agent-context-project (agent-protocol-provider-configuration provider)
                       conversation tool-namespaces :profile-id (agent-protocol-provider-profile-id provider)
                       :turn-id turn-id :goal-context goal-context :summary-p (and compaction-p (eq kind ':generate)) :kind kind
                       :continuation-handle                     (unless compaction-p (getf state :continuation))
                       :turn-state-handle                       (unless compaction-p (getf state :turn-state))
                       parameters)))
          (labels ((accept-completion (complete)
                     "Consume the receipt and retain accepted continuation state."
                     (let ((result (agent-protocol-provider--result projection complete)))
                       (unless compaction-p
                         (setf (gethash conversation (agent-protocol-provider-states provider))
                               (list :turn-id      turn-id
                                     :continuation (when (agent-wire:model-complete.has-continuation-handle complete)
                                                     (agent-wire:model-complete.continuation-handle complete))
                                     :turn-state   (when (agent-wire:model-complete.has-turn-state-handle complete)
                                                     (agent-wire:model-complete.turn-state-handle complete)))))
                       result))

                   (drain-unwind ()
                     "Drain cancellation and accepted completion within one cleanup deadline."
                     (handler-case
                        (sb-sys:with-deadline (:seconds 5)
                          (let ((end (+ (get-internal-real-time)
                                        (* 5 internal-time-units-per-second))))
                            (unless cancel-request
                              (handler-case
                                  (setf cancel-request (agent-protocol-driver-cancel driver request))
                                ;; A failed send must not discard an already queued receipt.
                                (error () nil)))
                            (loop until (or terminal (>= (get-internal-real-time) end))
                                 do (multiple-value-bind (envelope terminal-p)
                                        (agent-protocol-driver-next request :timeout 0.1)
                                      (when envelope
                                        (setf terminal terminal-p)
                                        (when (and (agent-wire:envelope.has-model-event envelope)
                                                   (agent-wire:model-event.has-complete
                                                    (agent-wire:envelope.model-event envelope)))
                                          (accept-completion
                                           (agent-wire:model-event.complete
                                            (agent-wire:envelope.model-event envelope)))))))
                            (when (and terminal cancel-request)
                              (agent-protocol-driver-next
                               cancel-request :timeout (max 0 (/ (- end (get-internal-real-time))
                                                                internal-time-units-per-second))))))
                       ;; Cleanup errors must not replace the callback's original exit.
                       (error () nil))))
            (unwind-protect
               (handler-case
                   (progn
                     (setf request (agent-protocol-driver-request
                                    driver (lambda (client)
                                             (agent-protocol-model-start client (agent-context-projection-start projection)))))
                     (when (agent-context-projection-delivery projection)
                       (context-delivery-notify (agent-context-projection-delivery projection) #'emit))
                     (loop
                       (observe-cancellation)
                       (multiple-value-bind (envelope terminal-p)
                           (agent-protocol-driver-next request :timeout 0.1)
                         (when envelope
                           (setf terminal terminal-p)
                           (observe-cancellation)
                          (unless (agent-wire:envelope.has-model-event envelope)
                            (when terminal (finish-cancellation))
                            (when (and (eq kind ':native-compaction)
                                       (agent-wire:envelope.has-failure envelope)
                                       (eq (agent-wire:failure.code (agent-wire:envelope.failure envelope)) ':unsupported))
                              (error 'agent-protocol-native-unavailable))
                            (agent-protocol-provider--failure))
                          (let ((event (agent-wire:envelope.model-event envelope)))
                            (cond
                              ((agent-wire:model-event.has-complete event)
                               (let* ((complete (agent-wire:model-event.complete event))
                                      (result   (accept-completion complete)))
                                 ;; Acceptance is durable even when cancellation wins locally.
                                 ;; Consume its receipt, but return no result for tool execution.
                                 (observe-cancellation)
                                 (finish-cancellation)
                                 (dolist (item (provider-result-output-items result))
                                   (emit (make-instance 'provider-item-event :item item))
                                   (finish-cancellation))
                                 (emit (make-instance 'provider-completed-event
                                                      :response-id     (provider-result-response-id result)
                                                      :usage           (provider-result-usage result)
                                                      :metadata        (provider-result-metadata result)
                                                      :turn-completion (provider-result-turn-completion result)))
                                 (finish-cancellation)
                                 (return result)))
                              ((or terminal-p (agent-wire:model-event.has-failed event))
                               (finish-cancellation)
                               (when (and (eq kind ':native-compaction)
                                          (agent-wire:model-event.has-failed event)
                                          (eq (agent-wire:failure.code (agent-wire:model-event.failed event)) ':unsupported))
                                 (error 'agent-protocol-native-unavailable))
                               (agent-protocol-provider--failure))
                              ((not cancelled)
                               (agent-protocol-provider--event event #'emit))))))))
                 (agent-protocol-error () (agent-protocol-provider--failure))
                 (autolith-supervisor::parent-protocol-error () (agent-protocol-provider--failure)))
             (when (and request (not terminal))
               ;; An arbitrary callback exit cannot leave a parent invocation orphaned.
               (unwind-protect
                    (drain-unwind)
                 (handler-case (agent-protocol-driver-close driver)
                   (error () nil)))))))))))
