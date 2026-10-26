;;;; -- Parent Model Event Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (&rest t) hash-table) model-event-test--object))

(defun model-event-test--object (&rest pairs)
  "Build a library JSON object without altering its values."
  (apply #'model-wire::json-object pairs))

(declaim (ftype (function (&optional string) hash-table) model-event-test--text))

(defun model-event-test--text (&optional (text "answer"))
  "Build a standard completed assistant item."
  (model-event-test--object "type" "message" "role" "assistant" "status" "completed"
    "content" (vector (model-event-test--object "type" "output_text" "text" text))))

(declaim (ftype (function (&key (:family keyword) (:capabilities list) (:hosted-tools hash-table)
                               (:hosted-types hash-table) (:kind keyword)
                               (:accept t) (:emit-failure boolean) (:log-failure boolean)
                               (:mutate-hooks boolean))
                          (values model-event-collector hash-table)) model-event-test--collector))

(defun model-event-test--collector (&key (family ':responses) capabilities
                                        (hosted-tools (make-hash-table :test 'equal))
                                        (hosted-types (make-hash-table :test 'equal))
                                        (kind ':generate) (accept t) emit-failure log-failure mutate-hooks)
  "Return a collector and its independent in-memory durable/public hook evidence."
  (let ((state (make-hash-table :test 'eq)))
    (setf (gethash ':accepted-count state) 0)
    (values
      (make-model-event-collector
        :profile       (make-model-profile :id "profile" :model "model" :family family
                         :capabilities capabilities :hosted-tools hosted-tools)
        :kind kind :hosted-types hosted-types
        :record-item   (lambda (&key attempt item-index original view)
                         (assert (zerop (length (parent-proto:output-item.replay-handle view))))
                         (assert (= item-index (parent-proto:output-item.item-index view)))
                         (push (list ':item attempt item-index) (gethash ':order state))
                         (push (model-events--copy original) (gethash ':originals state))
                         (when mutate-hooks (clrhash original))
                         (format nil "handle-~d-~d" attempt item-index))
        :record-event  (lambda (event)
                         (when (and log-failure (parent-proto:model-event.has-complete event))
                           (model-events--reject ':test-log-failure))
                         (push (list ':log (parent-proto:model-event.index event)) (gethash ':order state))
                         (push (model-events--message-copy event) (gethash ':events state))
                         (when mutate-hooks (setf (parent-proto:model-event.index event) 0)))
        :emit-event    (lambda (event)
                         (when (and emit-failure (parent-proto:model-event.has-complete event))
                           (model-events--reject ':test-send-failure))
                         (push (list ':emit (parent-proto:model-event.index event)) (gethash ':order state))
                         (push event (gethash ':sent state)))
        :retain-state  (lambda (&key kind value)
                         (push (list kind value) (gethash ':retained state))
                         (format nil "private-~(~a~)" kind))
        :accept-result (lambda (complete)
                         (incf (gethash ':accepted-count state))
                         (when (eq accept ':error) (model-events--reject ':test-accept-failure))
                         (when accept
                           (setf (gethash ':durable-complete state) (model-events--message-copy complete)))
                         (when (eq accept ':after-commit-error)
                           (model-events--reject ':test-accept-failure))
                         (when mutate-hooks
                           (setf (parent-proto:model-complete.output-items complete) nil))
                         accept))
      state)))

(declaim (ftype (function (function keyword) null) model-event-test--rejects))

(defun model-event-test--rejects (function code)
  "Require the expected typed redacted failure."
  (assert (handler-case (progn (funcall function) nil)
            (model-event-error (condition)
              (and (eq (model-event-error-code condition) code)
                   (not (search "private-secret" (princ-to-string condition)))))))
  nil)

(declaim (ftype (function (model-event-collector hash-table) t) model-event-test--item))

(defun model-event-test--item (collector item)
  "Feed an actual pinned-library item event."
  (model-event-collector-receive collector (make-instance 'model-wire:provider-item-event :item item)))

(declaim (ftype (function (model-event-collector &key (:attempt (integer 1)) (:retryable boolean)) t)
                model-event-test--attempt-failed))

(defun model-event-test--attempt-failed (collector &key (attempt 1) (retryable t))
  "Feed a failure with a private condition that must never enter the public log."
  (model-event-collector-receive collector
    (make-instance 'model-wire:provider-attempt-failed-event :attempt attempt :elapsed-seconds 4
      :output-received-p t :retryable-p retryable :condition
      (make-condition 'simple-error :format-control "private-secret"))))

(declaim (ftype (function () null) model-event-test--items))

(defun model-event-test--items ()
  "Check exact raw arguments, ordered views, persistence order and defensive copies."
  (dolist (arguments '("" "{" "null" "[1]" "{\"x\":false}"))
    (multiple-value-bind (collector state) (model-event-test--collector :mutate-hooks t)
      (let ((item (model-event-test--object "type" "function_call" "call_id" "call"
                    "name" "run" "namespace" "shell" "arguments" arguments
                    "provider-secret" "private-secret")))
        (model-event-collector-begin collector)
        (model-event-test--item collector item)
        (setf (gethash "provider-secret" item) "changed")
        (let* ((original (first (gethash ':originals state)))
               (event    (first (gethash ':sent state)))
               (output   (parent-proto:model-event.item-completed event))
               (call     (parent-proto:output-item.tool-call output)))
          (assert (equal (gethash "arguments" original) arguments))
          (assert (equal (gethash "provider-secret" original) "private-secret"))
          (assert (parent-proto:tool-call.has-arguments-json call))
          (assert (not (parent-proto:tool-call.has-arguments call)))
          (assert (equal (parent-proto:tool-call.arguments-json call) arguments))
          (assert (equal (parent-proto:tool-call.namespace call) "shell"))
          (assert (= (parent-proto:model-event.index event) 2))
          (assert (equal (reverse (gethash ':order state))
                         '((:log 1) (:emit 1) (:item 1 0) (:log 2) (:emit 2))))
          (setf (parent-proto:tool-call.arguments-json call) "mutated public view")
          (let* ((result        (make-instance 'model-wire:provider-result :output-items (list original)
                                 :tool-calls                                             (list original)))
                 (complete      (model-event-collector-finish collector result))
                 (accepted-call (parent-proto:output-item.tool-call
                                 (first (parent-proto:model-complete.output-items complete)))))
            (assert (not (parent-proto:model-complete.has-message complete)))
            (assert (eq (parent-proto:model-complete.reason complete) ':tool-calls))
            (assert (equal (parent-proto:tool-call.arguments-json accepted-call) arguments))
            (assert (= (gethash ':accepted-count state) 1))
            (setf (parent-proto:model-complete.output-items complete) nil)
            (assert (= (length (parent-proto:model-complete.output-items
                                (model-event-collector-finish collector result))) 1))
            (assert (= (gethash ':accepted-count state) 1)))))))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (model-event-collector-begin collector)
    (let* ((thinking  (model-event-test--object "type" "message" "role" "assistant" "content"
                        (vector (model-event-test--object "type" "thinking" "thinking" "visible"
                                  "signature" "private-secret")
                                (model-event-test--object "type" "redacted_thinking" "data" "private-secret")
                                (model-event-test--object "type" "text" "text" "shown"))))
           (reasoning (model-event-test--object "type" "reasoning" "encrypted_content" "private-secret"
                        "summary" (vector (model-event-test--object "type" "summary_text" "text" "summary"))))
           (discovery (model-event-test--object "type" "tool_search_call" "execution" "client"
                        "call_id" "search" "arguments" "{malformed"))
           (portable  (model-event-test--object "type" "reasoning_content" "content" "portable"))
           (items     (list thinking reasoning discovery portable)))
      (dolist (item items) (model-event-test--item collector item))
      (let* ((complete (model-event-collector-finish collector
                         (make-instance 'model-wire:provider-result :output-items items)))
             (views    (parent-proto:model-complete.output-items complete))
             (content  (parent-proto:conversation-message.content (parent-proto:output-item.message (first views)))))
        (assert (eq (parent-proto:model-complete.reason complete) ':tool-calls))
        (assert (equal (mapcar #'parent-proto:output-item.item-index views) '(0 1 2 3)))
        (assert (equal (mapcar #'parent-proto:content-block.content-case content)
                       '(parent-proto:reasoning-text parent-proto:reasoning-text parent-proto:text)))
        (assert (equal (parent-proto:content-block.reasoning-text (first content)) "visible"))
        (assert (equal (parent-proto:content-block.reasoning-text (second content)) ""))
        (assert (equal (parent-proto:reasoning-view.text (parent-proto:output-item.reasoning (second views))) "summary"))
        (assert (equal (parent-proto:tool-call.name
                        (parent-proto:output-item.tool-discovery-call (third views))) "tool_search"))
        (assert (not (nth-value 1 (gethash "name" (second (gethash ':originals state)))))))))
  nil)

(declaim (ftype (function () null) model-event-test--attempts))

(defun model-event-test--attempts ()
  "Check aggregate display indexes, attempt reset and request-wide event order."
  (multiple-value-bind (collector state) (model-event-test--collector)
    (model-event-collector-begin collector :maximum-attempts 2)
    (model-event-collector-receive collector (make-instance 'model-wire:assistant-delta-event :text "a"))
    (model-event-collector-receive collector (make-instance 'model-wire:reasoning-delta-event :text "r"))
    (model-event-test--item collector (model-event-test--text "failed attempt"))
    (model-event-test--attempt-failed collector)
    (model-event-collector-receive collector
      (make-instance 'model-wire:provider-retry-event :attempt 2 :maximum-attempts 2 :delay 1/4))
    (model-event-collector-receive collector (make-instance 'model-wire:assistant-delta-event :text "b"))
    (let* ((item     (model-event-test--text "accepted attempt"))
           (complete (model-event-collector-finish collector
                       (make-instance 'model-wire:provider-result :output-items (list item)))))
      (assert (= (length (parent-proto:model-complete.output-items complete)) 1))
      (assert (= (parent-proto:output-item.item-index (first (parent-proto:model-complete.output-items complete))) 0))
      (assert (equal (parent-proto:output-item.replay-handle
                      (first (parent-proto:model-complete.output-items complete))) "handle-2-0")))
    (let* ((events       (reverse (gethash ':events state)))
           (deltas       (remove-if-not #'parent-proto:model-event.has-text-delta events))
           (second-start (sixth events)))
      (assert (equal (mapcar #'parent-proto:model-event.index events) '(1 2 3 4 5 6 7 8 9)))
      (assert (equal (mapcar (lambda (event) (parent-proto:text-delta.block-index
                                              (parent-proto:model-event.text-delta event))) deltas) '(0 1 0)))
      (assert (= (parent-proto:attempt-started.delay-milliseconds
                  (parent-proto:model-event.attempt-started second-start)) 250))
      (assert (equal (mapcar #'parent-proto:model-event.attempt events) '(1 1 1 1 1 2 2 2 2))))
    (model-event-test--rejects (lambda () (model-event-collector-receive collector
                                           (make-instance 'model-wire:provider-progress-event))) ':event-order))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (declare (ignore state))
    (model-event-collector-begin collector :maximum-attempts 2)
    (model-event-test--attempt-failed collector :retryable nil)
    (model-event-test--rejects (lambda () (model-event-collector-begin collector :maximum-attempts 2)) ':attempt-order)
    (model-event-collector-fail collector))
  (model-event-test--rejects
    (lambda () (make-model-event-collector :require-item-delta-identity t)) ':delta-identity-unavailable)
  nil)

(declaim (ftype (function () null) model-event-test--metadata-state))

(defun model-event-test--metadata-state ()
  "Check metadata snapshots, exact usage and private continuation retention."
  (multiple-value-bind (collector state)
      (model-event-test--collector :capabilities '(:continuation :turn-state))
    (model-event-collector-begin collector)
    (let* ((model    (copy-seq "observed-model"))
           (metadata (make-instance 'model-wire:provider-response-metadata :model model
                       :finish-reason "completed" :cache-write-tokens 0 :usage-observed-p t))
           (usage    (model-event-test--object "input_tokens" 0 "output_tokens" 3
                       "input_tokens_details" (model-event-test--object "cached_tokens" 0)
                       "cache_write_tokens" 2 "nullable" nil "flag" ':json-false)))
      (model-event-collector-receive collector
        (make-instance 'model-wire:provider-response-metadata-event :metadata metadata))
      (setf (char model 0) #\X)
      (assert (equal (parent-proto:response-metadata.model
                      (parent-proto:model-event.metadata (first (gethash ':sent state)))) "observed-model"))
      (model-event-collector-receive collector
        (make-instance 'model-wire:provider-completed-event :response-id "response" :usage usage
          :turn-completion ':continue))
      (assert (zerop (gethash ':accepted-count state)))
      (let* ((result    (make-instance 'model-wire:provider-result :response-id "response" :usage usage
                          :turn-completion ':continue :turn-state "private-secret"))
             (complete  (model-event-collector-finish collector result))
             (projected (parent-proto:model-complete.usage complete))
             (raw       (protocol-map->data (parent-proto:usage.provider-usage projected))))
        (assert (parent-proto:usage.has-input-tokens projected))
        (assert (zerop (parent-proto:usage.input-tokens projected)))
        (assert (= (parent-proto:usage.output-tokens projected) 3))
        (assert (parent-proto:usage.has-cached-input-tokens projected))
        (assert (zerop (parent-proto:usage.cached-input-tokens projected)))
        (assert (= (parent-proto:usage.cache-write-tokens projected) 2))
        (assert (eq (gethash "nullable" raw) ':protocol-null))
        (assert (null (gethash "flag" raw)))
        (assert (equal (parent-proto:model-complete.continuation-handle complete) "private-continuation"))
        (assert (equal (parent-proto:model-complete.turn-state-handle complete) "private-turn-state"))
        (assert (eq (parent-proto:model-complete.turn-completion complete) ':continued))
        (assert (equal (gethash ':retained state) '((:turn-state "private-secret") (:continuation "response")))))))
  (let ((hosted (model-event-test--object "search" (model-event-test--object "type" "web_search")))
        (types  (model-event-test--object "web_search_call" "search")))
    (multiple-value-bind (collector state) (model-event-test--collector :hosted-tools hosted :hosted-types types)
      (model-event-collector-begin collector)
      (let ((item (model-event-test--object "type" "web_search_call" "status" "completed"
                    "action" (model-event-test--object "query" "private-secret"))))
        (model-event-test--item collector item)
        (let* ((complete (model-event-collector-finish collector
                           (make-instance 'model-wire:provider-result :output-items (list item))))
               (view     (parent-proto:output-item.hosted-tool (first (parent-proto:model-complete.output-items complete)))))
          (assert (equal (parent-proto:hosted-tool-view.capability-id view) "search"))
          (assert (equal (parent-proto:hosted-tool-view.status view) "completed"))
          (assert (null (parent-proto:hosted-tool-view.content view)))
          (assert (equal (gethash "query" (gethash "action" (first (gethash ':originals state)))) "private-secret"))))))
  (multiple-value-bind (collector state) (model-event-test--collector :kind ':native-compaction)
    (declare (ignore state))
    (model-event-collector-begin collector)
    (let* ((item     (model-event-test--object "type" "compaction" "encrypted_content" "private-secret"))
           (complete (model-event-collector-finish collector
                       (make-instance 'model-wire:provider-result :output-items (list item)))))
      (assert (eq (parent-proto:model-complete.compaction-outcome complete) ':compacted))
      (assert (not (parent-proto:compaction-view.has-summary
                    (parent-proto:output-item.compaction (first (parent-proto:model-complete.output-items complete))))))))
  nil)

(declaim (ftype (function () null) model-event-test--failure-boundaries))

(defun model-event-test--failure-boundaries ()
  "Check acceptance ACK, post-acceptance send/log failure, and one-shot finish."
  (dolist (accept '(nil :error :after-commit-error))
    (multiple-value-bind (collector state) (model-event-test--collector :accept accept)
      (model-event-collector-begin collector)
      (model-event-test--rejects
        (lambda () (model-event-collector-finish collector (make-instance 'model-wire:provider-result)))
        (if accept ':test-accept-failure ':acceptance-not-acknowledged))
      (assert (notany #'parent-proto:model-event.has-complete (gethash ':sent state)))
      (assert (= (gethash ':accepted-count state) 1))
      (model-event-test--rejects
        (lambda () (model-event-collector-finish collector (make-instance 'model-wire:provider-result))) ':event-order)
      (assert (eq (model-event-collector-phase collector) ':uncertain))
      (model-event-test--rejects (lambda () (model-event-collector-fail collector)) ':event-order)
      (when (eq accept ':after-commit-error) (assert (gethash ':durable-complete state)))
      (assert (notany #'parent-proto:model-event.has-failed (gethash ':sent state)))))
  (dolist (boundary '(:send :log))
    (multiple-value-bind (collector state)
        (model-event-test--collector :emit-failure (eq boundary ':send) :log-failure (eq boundary ':log))
      (model-event-collector-begin collector)
      (let* ((result   (make-instance 'model-wire:provider-result :output-items (list (model-event-test--text))))
             (expected (if (eq boundary ':send) ':test-send-failure ':test-log-failure)))
        (model-event-test--rejects (lambda () (model-event-collector-finish collector result)) expected)
        (assert (gethash ':durable-complete state))
        (assert (eq (model-event-collector-phase collector) ':accepted))
        (assert (= (length (parent-proto:model-complete.output-items
                            (model-event-collector-finish collector result))) 1))
        (assert (= (gethash ':accepted-count state) 1))
        (model-event-test--rejects (lambda () (model-event-collector-fail collector)) ':event-order))))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (declare (ignore state))
    (model-event-collector-begin collector)
    (model-event-test--item collector (model-event-test--text "first"))
    (model-event-test--rejects
      (lambda () (model-event-collector-finish collector
                   (make-instance 'model-wire:provider-result :output-items (list (model-event-test--text "different")))))
      ':item-mismatch))
  nil)

(declaim (ftype (function () null) model-event-test--unsupported))

(defun model-event-test--unsupported ()
  "Reject unsupported content and cyclic/oversized values with fixed typed errors."
  (dolist (item (list (model-event-test--object "type" "new_private_kind" "data" "private-secret")
                     (model-event-test--object "type" "message" "role" "assistant" "content"
                       (vector (model-event-test--object "type" "output_image" "data" "private-secret")))
                     (model-event-test--object "type" "message" "role" "assistant" "content"
                       (vector (model-event-test--object "type" "refusal" "refusal" "private-secret")))))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (model-event-collector-begin collector)
      (model-event-test--rejects (lambda () (model-event-test--item collector item))
        (if (equal (gethash "type" item) "message") ':unsupported-content ':unsupported-item))
      (assert (null (gethash ':originals state)))
      (assert (zerop (gethash ':accepted-count state)))))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (declare (ignore state))
    (model-event-collector-begin collector)
    (model-event-test--rejects
      (lambda () (model-event-collector-receive collector (make-instance 'model-wire:provider-event))) ':unsupported-event))
  (let ((cyclic (model-event-test--object "type" "message")))
    (setf (gethash "self" cyclic) cyclic)
    (model-event-test--rejects (lambda () (model-events--copy cyclic)) ':invalid-value))
  (model-event-test--rejects (lambda () (model-events--copy (make-string 2097153 :initial-element #\x))) ':resource-limit)
  (let ((list (loop for index below 100 collect index)))
    (assert (equal list (model-events--copy list)))
    (setf (rest (last list)) (rest list))
    (model-event-test--rejects (lambda () (model-events--copy list)) ':invalid-value))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (model-event-collector-begin collector)
    (model-event-test--rejects
      (lambda () (model-event-test--item collector
                   (model-event-test--object "type" "function_call" "call_id" "call"
                     "name" "run" "arguments" t))) ':invalid-arguments)
    (assert (zerop (gethash ':accepted-count state))))
  (multiple-value-bind (collector state) (model-event-test--collector)
    (model-event-collector-begin collector)
    (let* ((item     (model-event-test--object "type" "reasoning" "encrypted_content" "private-secret"))
           (complete (model-event-collector-finish collector
                       (make-instance 'model-wire:provider-result :output-items (list item)))))
      (assert (equal (parent-proto:reasoning-view.text
                      (parent-proto:output-item.reasoning
                        (first (parent-proto:model-complete.output-items complete)))) ""))
      (assert (= (gethash ':accepted-count state) 1))))
  nil)

(declaim (ftype (function (model-event-collector string function &key (:late-failure boolean)) t)
                model-event-test--execute))

(defun model-event-test--execute (collector data cleanup &key late-failure)
  "Use the real pinned Responses consumer and executor with an in-memory transport."
  (model-wire:provider-execute-request (make-instance 'model-wire:responses-api-provider) nil
    :transport          (lambda (request)
                          (declare (ignore request))
                          (values (make-string-input-stream data) 200 nil))
    :event-callback     (lambda (event) (model-event-collector-receive collector event))
    :call-with-deadline (lambda (function)
                          (let ((result (funcall function)))
                            (when late-failure
                              (error 'model-wire:provider-protocol-error :message "Late terminal validation failed."))
                            result))
    :cleanup            cleanup))

(declaim (ftype (function () null) model-event-test--executor))

(defun model-event-test--executor ()
  "Check real executor completion, late failure, cleanup order and normalized originals."
  (let* ((item-json "{\"type\":\"message\",\"id\":\"transient\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"answer\"}]}")
         (valid     (format nil "data: {\"type\":\"response.output_item.done\",\"item\":~a}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"response\",\"usage\":{\"input_tokens\":1}}}~%~%" item-json)))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (model-event-collector-begin collector)
      (let* ((cleaned nil)
             (result  (model-event-test--execute collector valid
                        (lambda (stream)
                          (assert (zerop (gethash ':accepted-count state)))
                          (setf cleaned t) (close stream)))))
        (assert cleaned)
        (assert (zerop (gethash ':accepted-count state)))
        (assert (not (nth-value 1 (gethash "id" (first (gethash ':originals state))))))
        (model-event-collector-finish collector result)
        (assert (= (gethash ':accepted-count state) 1))))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (model-event-collector-begin collector)
      (let ((cleaned nil)
            (failed  nil))
        ;; A backend may validate the result after the completed callback.
        (handler-case
            (model-event-test--execute collector valid
              (lambda (stream) (setf cleaned t) (close stream)) :late-failure t)
          (model-wire:provider-error () (setf failed t)))
        (assert failed)
        (assert cleaned)
        (assert (model-event-collector-completed collector))
        (assert (zerop (gethash ':accepted-count state)))
        (model-event-collector-fail collector)
        (assert (notany #'parent-proto:model-event.has-complete (gethash ':sent state)))
        (assert (parent-proto:model-event.has-failed (first (gethash ':sent state)))))))
  nil)

(declaim (ftype (function () null) model-event-test--budget-admission))

(defun model-event-test--budget-admission ()
  "Check admission, terminal reserves and uncertain durable callback outcomes."
  ;; Completion must pass admission before either private retention callback.
  (dolist (capabilities '((:continuation) (:turn-state) (:continuation :turn-state)))
    (multiple-value-bind (collector state) (model-event-test--collector :capabilities capabilities)
      (model-event-collector-begin collector)
      (setf (model-event-collector-public-size collector)
            (- (* 32 1024 1024) (model-events--failure-reserve) 4000))
      (let ((index (model-event-collector-index collector))
            (bytes (model-event-collector-public-size collector)))
        (model-event-test--rejects
          (lambda ()
            (model-event-collector-finish collector
              (make-instance 'model-wire:provider-result :response-id "response"
                :turn-state                                           (when (member ':turn-state capabilities) "state"))))
          ':resource-limit)
        (assert (null (gethash ':retained state)))
        (assert (zerop (gethash ':accepted-count state)))
        (assert (= index (model-event-collector-index collector)))
        (assert (= bytes (model-event-collector-public-size collector)))
        (model-event-collector-fail collector :code ':resource-limit)
        (assert (= (1+ index) (parent-proto:model-event.index (first (gethash ':events state)))))
        (assert (parent-proto:model-event.has-failed (first (gethash ':events state)))))))
  ;; Validate the second state and configuration before retaining the first.
  (dolist (problem '(:invalid-state :unsupported-state :missing-hook :missing-compaction :invalid-response))
    (multiple-value-bind (collector state)
        (model-event-test--collector
          :capabilities (if (eq problem ':unsupported-state) '(:continuation) '(:continuation :turn-state))
          :kind         (if (eq problem ':missing-compaction) ':native-compaction ':generate))
      (when (eq problem ':missing-hook)
        (setf (slot-value collector 'retain-state) nil))
      (model-event-collector-begin collector)
      (model-event-test--rejects
        (lambda ()
          (model-event-collector-finish collector
            (make-instance 'model-wire:provider-result
              :response-id (if (eq problem ':invalid-response) 7 "response")
              :turn-state  (if (eq problem ':invalid-state) #'identity "state"))))
        (ecase problem
          (:invalid-state
           ':invalid-value)
          (:unsupported-state
           ':unsupported-turn-state)
          (:missing-hook
           ':state-retention-required)
          (:missing-compaction
           ':missing-compaction)
          (:invalid-response
           ':invalid-response-id)))
      (assert (null (gethash ':retained state)))
      (assert (zerop (gethash ':accepted-count state)))
      (model-event-collector-fail collector)))
  ;; An item hook may persist its indexed event before raising or returning bad data.
  (dolist (outcome '(:before-write :after-write :invalid-handle))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (model-event-collector-begin collector)
      (setf (slot-value collector 'record-item)
            (lambda (&key attempt item-index original view)
              (declare (ignore attempt item-index original view))
              (unless (eq outcome ':before-write)
                (setf (gethash ':item-write state) t))
              (if (eq outcome ':invalid-handle)
                  ""
                  (model-events--reject ':test-item-failure))))
      (model-event-test--rejects
        (lambda () (model-event-test--item collector (model-event-test--text)))
        (if (eq outcome ':invalid-handle) ':invalid-handle ':test-item-failure))
      (assert (eq (model-event-collector-phase collector) ':uncertain))
      (assert (eq (not (null (gethash ':item-write state))) (not (eq outcome ':before-write))))
      (assert (= 1 (model-event-collector-index collector)))
      (assert (zerop (gethash ':accepted-count state)))
      (model-event-test--rejects (lambda () (model-event-collector-fail collector)) ':event-order)))
  (dolist (limit '(:events :bytes :originals))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (model-event-collector-begin collector)
      (ecase limit
        (:events
         (setf (model-event-collector-index collector) 65535))
        (:bytes
         (setf (model-event-collector-public-size collector)
               (- (* 32 1024 1024) (model-events--failure-reserve) 4096)))
        (:originals
         (setf (model-event-collector-original-size collector) (* 16 1024 1024))))
      (let ((index     (model-event-collector-index collector))
            (bytes     (model-event-collector-public-size collector))
            (originals (model-event-collector-original-size collector)))
        (model-event-test--rejects
          (lambda () (model-event-test--item collector (model-event-test--text))) ':resource-limit)
        (assert (null (gethash ':originals state)))
        (assert (= index (model-event-collector-index collector)))
        (assert (= bytes (model-event-collector-public-size collector)))
        (assert (= originals (model-event-collector-original-size collector)))
        (model-event-collector-fail collector :code ':resource-limit)
        (assert (= (1+ index) (parent-proto:model-event.index (first (gethash ':events state)))))
        (assert (parent-proto:model-event.has-failed (first (gethash ':events state)))))))
  ;; Admit the last nonterminal event with exactly the fixed failure reserve left.
  (multiple-value-bind (collector state) (model-event-test--collector)
    (model-event-collector-begin collector)
    (setf (model-event-collector-index collector) 65534)
    (let* ((event (parent-proto:make-model-event :progress
                    (parent-proto:make-model-progress :phase "provider" :text "")))
           (size  (model-events--event-size collector event)))
      (setf (model-event-collector-public-size collector)
            (- (* 32 1024 1024) size (model-events--failure-reserve)))
      (model-events--publish collector event)
      (assert (= 65535 (model-event-collector-index collector)))
      (model-event-test--rejects
        (lambda () (model-events--publish collector event)) ':resource-limit)
      (assert (= 65535 (model-event-collector-index collector)))
      (model-event-collector-fail collector)
      (assert (= 65536 (parent-proto:model-event.index (first (gethash ':events state)))))
      (assert (<= (model-event-collector-public-size collector) (* 32 1024 1024)))))
  ;; A successful record consumes its index even when the sender fails.
  (let ((recorded  nil)
        (collector nil))
    (setf collector
          (make-model-event-collector
            :profile       (make-model-profile :id "profile" :model "model" :family ':responses)
            :record-item   (lambda (&rest arguments) (declare (ignore arguments)) "handle")
            :record-event  (lambda (event) (push event recorded))
            :emit-event    (lambda (event) (declare (ignore event)) (model-events--reject ':test-send-failure))
            :accept-result (lambda (complete) (declare (ignore complete)) t)))
    (model-event-test--rejects (lambda () (model-event-collector-begin collector)) ':test-send-failure)
    (assert (= 1 (model-event-collector-index collector)))
    (assert (plusp (model-event-collector-public-size collector)))
    (assert (= 1 (parent-proto:model-event.index (first recorded)))))
  ;; Both a pre-write error and a post-write error have unknown durable outcome.
  (dolist (write-first '(nil t))
    (let* ((recorded nil)
           (collector
             (make-model-event-collector
               :profile       (make-model-profile :id "profile" :model "model" :family ':responses)
               :record-item   (lambda (&rest arguments) (declare (ignore arguments)) "handle")
               :record-event  (lambda (event)
                                (when write-first (push event recorded))
                                (model-events--reject ':test-log-failure))
               :emit-event    (lambda (event) (declare (ignore event)) (assert nil))
               :accept-result (lambda (complete) (declare (ignore complete)) t))))
      (model-event-test--rejects (lambda () (model-event-collector-begin collector)) ':test-log-failure)
      (assert (eq (model-event-collector-phase collector) ':uncertain))
      (assert (eq (not (null recorded)) write-first))
      (model-event-test--rejects (lambda () (model-event-collector-fail collector)) ':event-order)))
  (dolist (stage '(:receive :finish))
    (multiple-value-bind (collector state) (model-event-test--collector)
      (declare (ignore state))
      (model-event-collector-begin collector)
      (setf (slot-value collector 'record-event)
            (lambda (event) (declare (ignore event)) (model-events--reject ':test-log-failure)))
      (model-event-test--rejects
        (lambda ()
          (ecase stage
            (:receive
             (model-event-test--item collector (model-event-test--text)))
            (:finish
             (model-event-collector-finish collector
               (make-instance 'model-wire:provider-result
                 :output-items (list (model-event-test--text)))))))
        ':test-log-failure)
      (assert (eq (model-event-collector-phase collector) ':uncertain))
      (model-event-test--rejects (lambda () (model-event-collector-fail collector)) ':event-order)))
  nil)

(declaim (ftype (function () null) run-model-event-tests))

(defun run-model-event-tests ()
  "Run the focused parent-only event contract checks against the pinned wire API."
  (assert (not (find-package '#:autolith)))
  (model-event-test--items)
  (model-event-test--attempts)
  (model-event-test--metadata-state)
  (model-event-test--failure-boundaries)
  (model-event-test--unsupported)
  (model-event-test--executor)
  (model-event-test--budget-admission)
  (assert (not (find-package '#:autolith)))
  nil)
