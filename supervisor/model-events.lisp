;;;; -- Parent Model Event Projection --

(in-package #:autolith-supervisor)

(define-condition model-event-error (error)
  ((code :initarg :code :reader model-event-error-code
         :documentation "A fixed category, with no provider data."))
  (:documentation "A provider event cannot be projected without loss or ambiguity.")
  (:report (lambda (condition stream)
             (format stream "Model event projection failed: ~a." (model-event-error-code condition)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defclass model-event-collector ()
   ((profile :initarg :profile :reader model-event-collector-profile
             :documentation "Trusted parent profile.")
    (record-item :initarg :record-item :reader model-event-collector-record-item
                 :documentation "Durable provisional item writer.")
    (record-event :initarg :record-event :reader model-event-collector-record-event
                  :documentation "Durable public event writer.")
    (emit-event :initarg :emit-event :reader model-event-collector-emit-event
                :documentation "Public event sender, called after persistence.")
    (retain-state :initarg :retain-state :reader model-event-collector-retain-state
                  :documentation "Private continuation and turn-state writer.")
    (accept-result :initarg :accept-result :reader model-event-collector-accept-result
                   :documentation "Durable acceptance and session projection acknowledgment.")
    (hosted-types :initarg :hosted-types :reader model-event-collector-hosted-types
                  :documentation "Trusted output type to enabled capability ID map.")
    (kind :initarg :kind :reader model-event-collector-kind
          :documentation "Generate or native compaction.")
    (phase :initform ':new :accessor model-event-collector-phase
           :documentation "Collection and acceptance state, including uncertain publication.")
    (attempt :initform 0 :accessor model-event-collector-attempt
             :documentation "One-based attempt identity.")
    (maximum-attempts :initform 0 :accessor model-event-collector-maximum-attempts
                      :documentation "Fixed retry ceiling.")
    (retry-allowed :initform nil :accessor model-event-collector-retry-allowed
                   :documentation "Whether the last failed attempt permits a retry.")
    (original-size :initform 0 :accessor model-event-collector-original-size
                   :documentation "Cumulative retained original size estimate, across attempts.")
    (public-size :initform 0 :accessor model-event-collector-public-size
                 :documentation "Cumulative encoded public event bytes, across attempts.")
    (index :initform 0 :accessor model-event-collector-index
           :documentation "Last public event index across all attempts.")
    (items :initform nil :accessor model-event-collector-items
           :documentation "Reverse ordered public completed items for this attempt.")
    (originals :initform nil :accessor model-event-collector-originals
               :documentation "Reverse ordered library-normalized originals.")
    (metadata :initform nil :accessor model-event-collector-metadata
              :documentation "Last copied accumulated metadata snapshot.")
    (completed :initform nil :accessor model-event-collector-completed
               :documentation "Provisional completed event evidence, not acceptance.")
    (completion :initform nil :accessor model-event-collector-completion
                :documentation "Copied accepted completion, retained if public emission fails."))
   (:documentation "Single-owner, bounded event adapter for the pinned wire API.

Originals are exact library-normalized callback values, not raw wire objects.
The library may remove item IDs or redact credentials before this boundary.
No transport, retry scheduler, or locks are provided. Call from one owner.
Assistant/reasoning deltas have no item identity in the library. Block 0 is the
attempt-wide assistant display stream; block 1 is the reasoning display stream.
They never identify output items. Set REQUIRE-ITEM-DELTA-IDENTITY to reject this
API when a caller needs item-relative deltas. There are no argument delta events
in this library; completed calls preserve their exact argument strings. Discovery
calls without a name use the canonical logical display name tool_search; their
private original has no synthetic field.

Supported views: assistant text, signed/redacted Anthropic thinking, Responses
reasoning summaries, portable reasoning_content, function/discovery calls,
compaction, and explicitly enabled web/file search status views. Hosted action,
query, result, citation and provider metadata remain private. Output media and
unknown content kinds fail explicitly; the pinned consumers expose no portable
output-media contract. Full bounded usage objects are public as required by the
protocol. Continuation IDs and turn-state tokens use private retention hooks.")))

(declaim (ftype (function (keyword) nil) model-events--reject))

(defun model-events--reject (code)
  "Signal a fixed redacted error."
  (error 'model-event-error :code code))

(declaim (ftype (function (error) nil) model-events--resignal))

(defun model-events--resignal (condition)
  "Keep fixed adapter errors and replace unexpected reports with a redacted code."
  (if (typep condition 'model-event-error)
      (error condition)
      (model-events--reject ':projection-failed)))

(declaim (ftype (function (t) (values t (integer 0) &optional)) model-events--copy))

(defun model-events--copy (value)
  "Return a detached portable value and its bounded storage size estimate."
  (let ((nodes 65536) (characters 0) (active (make-hash-table :test 'eq)))
    (labels ((count-node ()
               (when (minusp (decf nodes)) (model-events--reject ':resource-limit)))

             (count-characters (count)
               (incf characters count)
               (when (> characters 2097152) (model-events--reject ':resource-limit)))

             (walk-list (item depth)
               (let ((tail item) (seen nil) (result nil))
                 (unwind-protect
                      (progn
                        (loop while (consp tail) do
                          (unless (eq tail item)
                            (count-node)
                            (when (gethash tail active) (model-events--reject ':invalid-value))
                            (setf (gethash tail active) t)
                            (push tail seen))
                          (push (walk (first tail) (1+ depth)) result)
                          (setf tail (rest tail))
                          (when (eq tail item) (model-events--reject ':invalid-value)))
                        (unless (null tail) (model-events--reject ':invalid-value))
                        (nreverse result))
                   (dolist (cell seen) (remhash cell active)))))

             (walk (item depth)
               (count-node)
               (when (> depth 32) (model-events--reject ':resource-limit))
               (when (or (consp item) (hash-table-p item) (vectorp item))
                 (when (gethash item active) (model-events--reject ':invalid-value))
                 (setf (gethash item active) t))
               (unwind-protect
                    (typecase item
                      (string
                       (count-characters (length item))
                       (copy-seq item))
                      (hash-table
                       (unless (eq (hash-table-test item) 'equal) (model-events--reject ':invalid-value))
                       (let ((result (make-hash-table :test 'equal)))
                         (maphash (lambda (key child)
                                    (unless (stringp key) (model-events--reject ':invalid-value))
                                    (setf (gethash (walk key (1+ depth)) result)
                                          (walk child (1+ depth)))) item)
                         result))
                      (cons
                       (walk-list item depth))
                      ((array (unsigned-byte 8) (*))
                       (count-characters (length item))
                       (copy-seq item))
                      (vector
                       (map 'vector (lambda (child) (walk child (1+ depth))) item))
                      (integer
                       item)
                      (float
                       (when (or (sb-ext:float-nan-p item) (sb-ext:float-infinity-p item))
                         (model-events--reject ':invalid-value))
                       item)
                      (symbol
                       (unless (member item '(nil t :json-false :protocol-null yason:false yason:true))
                         (model-events--reject ':invalid-value))
                       item)
                      (t
                       (model-events--reject ':invalid-value)))
                 (remhash item active))))
      (values (walk value 0) (+ characters (* 32 (- 65536 nodes)))))))

(declaim (ftype (function (t) t) model-events--message-copy))

(defun model-events--message-copy (message)
  "Detach a generated protocol message."
  (cl-protobufs:deserialize-from-bytes (type-of message)
                                     (cl-protobufs:serialize-to-bytes message)))

(declaim (ftype (function (t) t) model-events--public-data))

(defun model-events--public-data (value)
  "Convert already bounded library JSON null and false to protocol values."
  (cond
    ((null value)
     ':protocol-null)
    ((member value '(:json-false yason:false))
     nil)
    ((eq value 'yason:true)
     t)
    ((hash-table-p value)
     (let ((result (make-hash-table :test 'equal)))
       (maphash (lambda (key child)
                  (setf (gethash key result) (model-events--public-data child))) value)
       result))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'model-events--public-data value))
    ((consp value)
     (map 'vector #'model-events--public-data value))
    (t
     value)))

(declaim (ftype (function (&key (:profile model-profile) (:record-item function)
                               (:record-event function) (:emit-event function)
                               (:retain-state t) (:accept-result function)
                               (:hosted-types hash-table) (:kind keyword)
                               (:require-item-delta-identity boolean)) model-event-collector)
                make-model-event-collector))

(defun make-model-event-collector (&key profile record-item record-event emit-event retain-state
                                      accept-result (hosted-types (make-hash-table :test 'equal))
                                      (kind ':generate) require-item-delta-identity)
  "Create a single-owner collector with mandatory durable hooks.
RECORD-ITEM receives &KEY ATTEMPT ITEM-INDEX ORIGINAL VIEW and returns a
provisional handle. VIEW is a detached OutputItem without a handle. Persist both
the original and public view. RECORD-EVENT receives a detached ModelEvent before
EMIT-EVENT does. If RECORD-ITEM or ACCEPT-RESULT already wrote that event, verify
its exact identity instead of appending it twice.
RETAIN-STATE receives &KEY KIND VALUE, where KIND is :CONTINUATION or :TURN-STATE,
and returns a provisional scoped handle. All hooks capture request/session scope.
ACCEPT-RESULT receives a detached ModelComplete. It must durably accept the
attempt AND publish/acknowledge the session recovery projection before returning
exactly T. No handle may resolve before that acknowledgment. A failed acceptance
cannot emit complete. The hook must reconcile its own write-ahead journal after
partial failure. FINISH is called only after executor terminal validation and
cleanup return; PROVIDER-COMPLETED-EVENT alone never calls acceptance.
The library cleanup is best effort, so this adapter cannot assert close success.
Projection and pre-acceptance hook errors break the collector. Call FAIL or discard
it; do not retry a partially performed hook. If ACCEPT-RESULT fails or does not
acknowledge, the state is UNCERTAIN: reconcile the private journal and session
projection instead of sending a failure that could revoke a committed result.
After acceptance, repeated FINISH returns a copy without sending again, even if
event logging or public emission failed. Portable copies permit at most 65,536
nodes, depth 32, and 2 MiB of characters/bytes per value. Each event is at most
7 MiB; the request event budget is 32 MiB and 65,536 events. Retained original
size estimates total at most 16 MiB. Lower transport limits are the sender's
responsibility."
  (when require-item-delta-identity (model-events--reject ':delta-identity-unavailable))
  (unless (and (typep profile 'model-profile) (functionp record-item)
               (functionp record-event) (functionp emit-event) (functionp accept-result)
               (or (null retain-state) (functionp retain-state))
               (member kind '(:generate :native-compaction)))
    (model-events--reject ':invalid-configuration))
  (maphash (lambda (type capability)
             (unless (and (member type '("web_search_call" "file_search_call") :test #'equal)
                          (stringp capability)
                          (gethash capability (model-profile-hosted-tools profile)))
               (model-events--reject ':unsupported-hosted-tool))) hosted-types)
  (make-instance 'model-event-collector :profile profile :record-item record-item
                 :record-event record-event :emit-event emit-event :retain-state retain-state
                 :accept-result accept-result :hosted-types (model-events--copy hosted-types) :kind kind))

(declaim (ftype (function (model-event-collector parent-proto:model-event) (integer 0)) model-events--event-size))

(defun model-events--event-size (collector event)
  "Check per-event and request-wide bounds before external side effects."
  (setf (parent-proto:model-event.index event) (1+ (model-event-collector-index collector))
        (parent-proto:model-event.attempt event) (model-event-collector-attempt collector))
  (let ((size (length (cl-protobufs:serialize-to-bytes event))))
    (when (or (>= (model-event-collector-index collector) 65536)
              (> size (* 7 1024 1024))
              (> (+ size (model-event-collector-public-size collector)) (* 32 1024 1024)))
      (model-events--reject ':resource-limit))
    size))

(declaim (ftype (function (model-event-collector parent-proto:model-event) t) model-events--publish))

(defun model-events--publish (collector event)
  "Record before emission, with independent message copies for both callbacks."
  (let ((size (model-events--event-size collector event)))
    (incf (model-event-collector-public-size collector) size)
    (incf (model-event-collector-index collector)))
  (funcall (model-event-collector-record-event collector) (model-events--message-copy event))
  (funcall (model-event-collector-emit-event collector) (model-events--message-copy event)))

(declaim (ftype (function (model-event-collector &key (:maximum-attempts (integer 1))
                                                    (:delay-milliseconds (integer 0))) t)
                model-event-collector-begin))

(defun model-event-collector-begin (collector &key (maximum-attempts 1) (delay-milliseconds 0))
  "Start the first attempt or the next attempt after ATTEMPT-FAILED."
  (unless (and (member (model-event-collector-phase collector) '(:new :waiting))
               (or (eq (model-event-collector-phase collector) ':new)
                   (model-event-collector-retry-allowed collector))
               (<= 1 maximum-attempts 1024)
               (typep delay-milliseconds '(unsigned-byte 64))
               (< (model-event-collector-attempt collector) maximum-attempts)
               (or (zerop (model-event-collector-attempt collector))
                   (= maximum-attempts (model-event-collector-maximum-attempts collector)))
               (or (plusp (model-event-collector-attempt collector)) (zerop delay-milliseconds)))
    (model-events--reject ':attempt-order))
  (setf (model-event-collector-phase collector) ':active
        (model-event-collector-maximum-attempts collector) maximum-attempts
        (model-event-collector-items collector) nil
        (model-event-collector-originals collector) nil
        (model-event-collector-completed collector) nil
        (model-event-collector-metadata collector) nil)
  (incf (model-event-collector-attempt collector))
  (handler-case
      (model-events--publish collector
        (parent-proto:make-model-event :attempt-started
          (parent-proto:make-attempt-started :maximum-attempts maximum-attempts
                                           :delay-milliseconds delay-milliseconds)))
    (error (condition)
      (setf (model-event-collector-phase collector) ':broken)
      (model-events--resignal condition))))

(declaim (ftype (function (hash-table string &key (:empty boolean)) string) model-events--string))

(defun model-events--string (object field &key empty)
  "Require a string field without inserting a default."
  (let ((value (gethash field object)))
    (unless (and (stringp value) (or empty (plusp (length value))))
      (model-events--reject ':invalid-item))
    (copy-seq value)))

(declaim (ftype (function (hash-table &key (:discovery boolean)) parent-proto:tool-call) model-events--call))

(defun model-events--call (item &key discovery)
  "Preserve raw argument strings, including empty, scalar and malformed JSON."
  (let ((call      (parent-proto:make-tool-call
                    :call-id   (model-events--string item "call_id")
                    :name      (if (and discovery (not (nth-value 1 (gethash "name" item))))
                                   "tool_search" (model-events--string item "name"))
                    :namespace (if (gethash "namespace" item)
                                   (model-events--string item "namespace" :empty t) "")))
        (arguments (gethash "arguments" item)))
    (cond
      ((stringp arguments)
       (setf (parent-proto:tool-call.arguments-json call) (copy-seq arguments)))
      ((hash-table-p arguments)
       (setf (parent-proto:tool-call.arguments call)
             (common-proto:value.map-value
              (protocol-data->value (model-events--public-data arguments)))))
      (t
       (model-events--reject ':invalid-arguments)))
    call))

(declaim (ftype (function (hash-table) list) model-events--content))

(defun model-events--content (item)
  "Project all known content in order; keep signatures and opaque blobs private."
  (let ((content (gethash "content" item)))
    (unless (and (vectorp content) (not (stringp content)))
      (model-events--reject ':unsupported-content))
    (loop for part across content collect
      (progn
        (unless (hash-table-p part) (model-events--reject ':unsupported-content))
        (let ((type (gethash "type" part)))
          (cond
            ((member type '("output_text" "text") :test #'equal)
             (parent-proto:make-content-block :text (model-events--string part "text" :empty t)))
            ((equal type "thinking")
             (model-events--string part "signature")
             (parent-proto:make-content-block :reasoning-text
               (model-events--string part "thinking" :empty t)))
            ((equal type "redacted_thinking")
             (model-events--string part "data")
             (parent-proto:make-content-block :reasoning-text ""))
            (t
             (model-events--reject ':unsupported-content))))))))

(declaim (ftype (function (model-event-collector hash-table (integer 0)) parent-proto:output-item)
                model-events--item-view))

(defun model-events--item-view (collector item index)
  "Build one explicit public view without copying provider-only fields."
  (let* ((type   (gethash "type" item))
         (output (parent-proto:make-output-item :item-index index
                   :family                                  (string-downcase (symbol-name
                                                                              (model-profile-family (model-event-collector-profile collector)))))))
    (cond
      ((equal type "message")
       (unless (equal (gethash "role" item) "assistant") (model-events--reject ':unsupported-role))
       (setf (parent-proto:output-item.message output)
             (parent-proto:make-conversation-message :role ':assistant :content (model-events--content item))))
      ((member type '("function_call" "tool_search_call") :test #'equal)
       (if (equal type "function_call")
           (setf (parent-proto:output-item.tool-call output) (model-events--call item))
           (progn
             (unless (equal (gethash "execution" item) "client")
               (model-events--reject ':unsupported-discovery))
             (setf (parent-proto:output-item.tool-discovery-call output)
                   (model-events--call item :discovery t)))))
      ((equal type "reasoning_content")
       (setf (parent-proto:output-item.reasoning output)
             (parent-proto:make-reasoning-view :text (model-events--string item "content" :empty t))))
      ((equal type "reasoning")
       (let ((summary (gethash "summary" item #())))
         (unless (and (vectorp summary) (not (stringp summary)))
           (model-events--reject ':unsupported-reasoning))
         (setf (parent-proto:output-item.reasoning output)
               (parent-proto:make-reasoning-view :text
                 (format nil "~{~a~^~2%~}"
                   (loop for part across summary collect
                     (progn
                       (unless (and (hash-table-p part) (equal (gethash "type" part) "summary_text"))
                         (model-events--reject ':unsupported-reasoning))
                       (model-events--string part "text" :empty t))))))))
      ((equal type "compaction")
       (unless (eq (model-event-collector-kind collector) ':native-compaction)
         (model-events--reject ':unexpected-compaction))
       (let ((view (parent-proto:make-compaction-view)))
         (when (nth-value 1 (gethash "summary" item))
           (setf (parent-proto:compaction-view.summary view) (model-events--string item "summary" :empty t)))
         (setf (parent-proto:output-item.compaction output) view)))
      ((gethash type (model-event-collector-hosted-types collector))
       (setf (parent-proto:output-item.hosted-tool output)
             (parent-proto:make-hosted-tool-view
               :capability-id (gethash type (model-event-collector-hosted-types collector))
               :status        (model-events--string item "status"))))
      (t
       (model-events--reject ':unsupported-item)))
    output))

(declaim (ftype (function (model-event-collector t) t) model-events--record-item))

(defun model-events--record-item (collector value)
  "Persist an original and its scope before the corresponding public event."
  (multiple-value-bind (original size) (model-events--copy value)
    (incf (model-event-collector-original-size collector) size)
    (when (> (model-event-collector-original-size collector) (* 16 1024 1024))
      (model-events--reject ':resource-limit))
    (let ((index (length (model-event-collector-items collector))))
     (unless (hash-table-p original) (model-events--reject ':unsupported-item))
     (when (>= index 4096) (model-events--reject ':resource-limit))
     (let* ((output (model-events--item-view collector original index))
            (handle (funcall (model-event-collector-record-item collector)
                            :attempt (model-event-collector-attempt collector)
                            :item-index index :original (model-events--copy original)
                            :view    (model-events--message-copy output))))
       (unless (and (stringp handle) (< 0 (length handle) 1025))
         (model-events--reject ':invalid-handle))
       (when (find handle (model-event-collector-items collector)
                   :key #'parent-proto:output-item.replay-handle :test #'equal)
         (model-events--reject ':duplicate-handle))
       (setf (parent-proto:output-item.replay-handle output) (copy-seq handle))
       (push original (model-event-collector-originals collector))
       (push output (model-event-collector-items collector))
       (model-events--publish collector (parent-proto:make-model-event :item-completed output))))))

(declaim (ftype (function (t) parent-proto:response-metadata) model-events--metadata))

(defun model-events--metadata (metadata)
  "Copy observed metadata into a typed snapshot."
  (unless (typep metadata 'model-wire:provider-response-metadata)
    (model-events--reject ':invalid-metadata))
  (let ((result (parent-proto:make-response-metadata
                 :usage-observed (model-wire:provider-response-metadata-usage-observed-p metadata))))
    (let ((model  (model-wire:provider-response-metadata-model metadata))
          (finish (model-wire:provider-response-metadata-finish-reason metadata))
          (cache  (model-wire:provider-response-metadata-cache-write-tokens metadata)))
      (when model
        (unless (stringp model) (model-events--reject ':invalid-metadata))
        (setf (parent-proto:response-metadata.model result) (model-events--copy model)))
      (when finish
        (unless (stringp finish) (model-events--reject ':invalid-metadata))
        (setf (parent-proto:response-metadata.finish-reason result) (model-events--copy finish)))
      (when cache
        (unless (typep cache '(unsigned-byte 64)) (model-events--reject ':invalid-metadata))
        (setf (parent-proto:response-metadata.cache-write-tokens result) cache)))
    result))

(defgeneric model-events--receive (collector event)
  (:documentation "Project one exact supported provider event class."))

(defmethod model-events--receive (collector (event model-wire:assistant-delta-event))
  "Project the attempt-wide assistant display stream, not an output item index."
  (model-events--publish collector (parent-proto:make-model-event :text-delta
                                    (parent-proto:make-text-delta :block-index 0
                                      :text                                    (model-events--copy (model-wire:assistant-delta-event-text event))))))

(defmethod model-events--receive (collector (event model-wire:reasoning-delta-event))
  "Project the attempt-wide visible reasoning display stream."
  (model-events--publish collector (parent-proto:make-model-event :text-delta
                                    (parent-proto:make-text-delta :block-index 1 :reasoning t
                                      :text (model-events--copy (model-wire:reasoning-delta-event-text event))))))

(defmethod model-events--receive (collector (event model-wire:provider-progress-event))
  "Emit fixed progress text without provider data."
  (model-events--publish collector (parent-proto:make-model-event :progress
                                    (parent-proto:make-model-progress :phase "provider" :text ""))))

(defmethod model-events--receive (collector (event model-wire:provider-item-event))
  "Persist the exact library-normalized completed item before forwarding its view."
  (model-events--record-item collector (model-wire:provider-item-event-item event)))

(defmethod model-events--receive (collector (event model-wire:provider-response-metadata-event))
  "Persist and forward one detached accumulated metadata snapshot."
  (let ((metadata (model-events--metadata (model-wire:provider-response-metadata-event-metadata event))))
    (setf (model-event-collector-metadata collector) metadata)
    (model-events--publish collector (parent-proto:make-model-event :metadata metadata))))

(defmethod model-events--receive (collector (event model-wire:provider-completed-event))
  "Retain provisional completion evidence without accepting the attempt."
  (when (model-event-collector-completed collector) (model-events--reject ':duplicate-completed))
  (setf (model-event-collector-completed collector)
        (list (model-events--copy (model-wire:provider-completed-event-response-id event))
              (model-events--copy (model-wire:provider-completed-event-usage event))
              (model-wire:provider-completed-event-turn-completion event)))
  (when (model-wire:provider-completed-event-metadata event)
    (setf (model-event-collector-metadata collector)
          (model-events--metadata (model-wire:provider-completed-event-metadata event))))
  nil)

(defmethod model-events--receive (collector (event model-wire:provider-attempt-failed-event))
  "Emit fixed redacted failure details and retain the retry decision."
  (unless (= (model-wire:provider-attempt-failed-event-attempt event)
             (model-event-collector-attempt collector))
    (model-events--reject ':attempt-order))
  (let ((retryable (model-wire:provider-attempt-failed-event-retryable-p event)))
    (model-events--publish collector
      (parent-proto:make-model-event :attempt-failed
        (parent-proto:make-attempt-failed
          :elapsed-seconds (model-wire:provider-attempt-failed-event-elapsed-seconds event)
          :output-received (model-wire:provider-attempt-failed-event-output-received-p event)
          :retryable retryable :failure
          (parent-proto:make-failure :code ':unavailable :message "Provider attempt failed."
                                    :retryable retryable)))))
  (setf (model-event-collector-phase collector) ':waiting
        (model-event-collector-retry-allowed collector)
        (model-wire:provider-attempt-failed-event-retryable-p event))
  nil)

(defmethod model-events--receive (collector event)
  "Reject event classes with no supported public projection."
  (declare (ignore collector event))
  (model-events--reject ':unsupported-event))

(declaim (ftype (function (model-event-collector t) t) model-event-collector-receive))

(defun model-event-collector-receive (collector event)
  "Consume a library callback; never infer acceptance from a completed event."
  (handler-case
      (if (typep event 'model-wire:provider-retry-event)
          (progn
            (unless (= (model-wire:provider-retry-event-attempt event)
                       (1+ (model-event-collector-attempt collector)))
              (model-events--reject ':attempt-order))
            (let ((delay (model-wire:provider-retry-event-delay event)))
              (unless (and (realp delay) (<= 0 delay 86400))
                (model-events--reject ':invalid-delay))
              (model-event-collector-begin collector
                :maximum-attempts   (model-wire:provider-retry-event-maximum-attempts event)
                :delay-milliseconds (ceiling (* delay 1000)))))
          (progn
            (unless (eq (model-event-collector-phase collector) ':active)
              (model-events--reject ':event-order))
            (model-events--receive collector event)))
    (error (condition)
      (unless (member (model-event-collector-phase collector) '(:accepted :failed :uncertain))
        (setf (model-event-collector-phase collector) ':broken))
      (model-events--resignal condition))))

(declaim (ftype (function (model-event-collector &key (:code keyword)) t) model-event-collector-fail))

(defun model-event-collector-fail (collector &key (code ':unavailable))
  "Emit one terminal fixed failure; never revoke an accepted result."
  (unless (and (member (model-event-collector-phase collector) '(:active :waiting :broken))
               (member code '(:unavailable :unsupported :resource-limit :cancelled :internal)))
    (model-events--reject ':event-order))
  (setf (model-event-collector-phase collector) ':failed)
  (model-events--publish collector (parent-proto:make-model-event :failed
                                    (parent-proto:make-failure :code code :message "Model request failed."))))

(declaim (ftype (function (t t) boolean) model-events--equal-p))

(defun model-events--equal-p (left right)
  "Compare copied portable library values without JSON re-encoding."
  (cond
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (loop for key being the hash-keys of left using (hash-value child)
                always (multiple-value-bind (other present) (gethash key right)
                         (and present (model-events--equal-p child other))))))
    ((and (stringp left) (stringp right))
     (string= left right))
    ((and (consp left) (consp right))
     (and (model-events--equal-p (first left) (first right))
          (model-events--equal-p (rest left) (rest right))))
    ((and (vectorp left) (vectorp right) (not (stringp left)) (not (stringp right)))
     (and (= (length left) (length right)) (every #'model-events--equal-p left right)))
    (t
     (eql left right))))

(declaim (ftype (function (t) parent-proto:usage) model-events--usage))

(defun model-events--usage (usage)
  "Preserve the exact bounded provider usage map and normalized optional counters."
  (unless (or (null usage) (hash-table-p usage)) (model-events--reject ':invalid-usage))
  (let* ((copy       (model-events--copy usage))
         (normalized (model-wire:provider-usage-normalize copy))
         (result     (parent-proto:make-usage)))
    (when copy
      (setf (parent-proto:usage.provider-usage result)
            (common-proto:value.map-value (protocol-data->value (model-events--public-data copy))))
      (loop for key in '("input_tokens" "output_tokens" "cached_input_tokens" "cache_creation_input_tokens")
            for writer in (list #'(setf parent-proto:usage.input-tokens)
                                #'(setf parent-proto:usage.output-tokens)
                                #'(setf parent-proto:usage.cached-input-tokens)
                                #'(setf parent-proto:usage.cache-write-tokens))
            for value = (gethash key normalized)
            when value do
              (unless (typep value '(unsigned-byte 64)) (model-events--reject ':invalid-usage))
              (funcall writer value result)))
    result))

(declaim (ftype (function (model-event-collector keyword t) string) model-events--retain))

(defun model-events--retain (collector kind value)
  "Retain an opaque token privately and return only the scoped handle."
  (let ((hook (model-event-collector-retain-state collector)))
    (unless hook (model-events--reject ':state-retention-required))
    (let ((handle (funcall hook :kind kind :value (model-events--copy value))))
      (unless (and (stringp handle) (< 0 (length handle) 1025))
        (model-events--reject ':invalid-handle))
      (copy-seq handle))))

(declaim (ftype (function (model-event-collector model-wire:provider-result) parent-proto:model-complete)
                model-event-collector-finish))

(defun model-event-collector-finish (collector result)
  "Accept only after the caller's executor returns a validated, cleaned-up result.
Validate callback/result consistency, append remaining items, retain opaque state,
then call durable acceptance with its session acknowledgment barrier. Repeated
calls after acceptance return a detached completion and do not emit again."
  (when (eq (model-event-collector-phase collector) ':accepted)
    (return-from model-event-collector-finish
      (model-events--message-copy (model-event-collector-completion collector))))
  (unless (eq (model-event-collector-phase collector) ':active)
    (model-events--reject ':event-order))
  (setf (model-event-collector-phase collector) ':finishing)
  (handler-case
      (let* ((originals   (reverse (model-event-collector-originals collector)))
             (final       (model-events--copy (model-wire:provider-result-output-items result)))
             (evidence    (model-event-collector-completed collector))
             (response-id (model-wire:provider-result-response-id result))
             (usage       (model-wire:provider-result-usage result))
             (turn        (model-wire:provider-result-turn-completion result)))
        (unless (and (listp final) (<= (length originals) (length final))
                     (loop for original in originals for item in final
                           always (model-events--equal-p original item)))
          (model-events--reject ':item-mismatch))
        (when (and evidence
                   (not (and (model-events--equal-p (first evidence) response-id)
                             (model-events--equal-p (second evidence) usage)
                             (eq (third evidence) turn))))
          (model-events--reject ':completion-mismatch))
        (dolist (item (nthcdr (length originals) final)) (model-events--record-item collector item))
        (when (model-wire:provider-result-metadata result)
          (setf (model-event-collector-metadata collector)
                (model-events--metadata (model-wire:provider-result-metadata result))))
        (let* ((metadata (model-event-collector-metadata collector))
               (finish   (and metadata (parent-proto:response-metadata.finish-reason metadata)))
               (complete (parent-proto:make-model-complete
                          :output-items    (reverse (model-event-collector-items collector))
                          :usage           (model-events--usage usage)
                          :reason          (cond
                                             ((member finish '("max_tokens" "length" "MAX_TOKENS") :test #'equal)
                                              ':output-limit)
                                             ((member finish '("content_filter" "SAFETY") :test #'equal)
                                              ':filtered)
                                            ((some (lambda (item)
                                                     (or (parent-proto:output-item.has-tool-call item)
                                                         (parent-proto:output-item.has-tool-discovery-call item)))
                                                   (model-event-collector-items collector))
                                             ':tool-calls)
                                            (t
                                             ':stop))
                          :turn-completion (case turn
                                             (:end
                                              ':ended) (:continue
                                                        ':continued)
                                             (:unspecified
                                              ':turn-completion-unspecified)
                                             (otherwise
                                              (model-events--reject ':invalid-turn-completion))))))
          (when metadata (setf (parent-proto:model-complete.metadata complete) metadata))
          (when response-id
            (unless (stringp response-id) (model-events--reject ':invalid-response-id))
            (setf (parent-proto:model-complete.response-id complete) (copy-seq response-id))
            (when (member ':continuation (model-profile-capabilities (model-event-collector-profile collector)))
              (setf (parent-proto:model-complete.continuation-handle complete)
                    (model-events--retain collector ':continuation response-id))))
          (when (model-wire:provider-result-turn-state result)
            (unless (member ':turn-state (model-profile-capabilities (model-event-collector-profile collector)))
              (model-events--reject ':unsupported-turn-state))
            (setf (parent-proto:model-complete.turn-state-handle complete)
                  (model-events--retain collector ':turn-state (model-wire:provider-result-turn-state result))))
          (when (eq (model-event-collector-kind collector) ':native-compaction)
            (unless (and final (some (lambda (item) (equal (gethash "type" item) "compaction")) final))
              (model-events--reject ':missing-compaction))
            (setf (parent-proto:model-complete.compaction-outcome complete) ':compacted))
          (let ((saved (model-events--message-copy complete)))
            (model-events--event-size collector (parent-proto:make-model-event :complete saved))
            (setf (model-event-collector-phase collector) ':accepting)
            (unless (eq t (funcall (model-event-collector-accept-result collector)
                                  (model-events--message-copy saved)))
              (model-events--reject ':acceptance-not-acknowledged))
            (setf (model-event-collector-completion collector) saved
                  (model-event-collector-phase collector) ':accepted)
            (model-events--publish collector (parent-proto:make-model-event :complete saved))
            (model-events--message-copy saved))))
    (error (condition)
      (unless (eq (model-event-collector-phase collector) ':accepted)
        (setf (model-event-collector-phase collector)
              (if (eq (model-event-collector-phase collector) ':accepting) ':uncertain ':broken)))
      (model-events--resignal condition))))
