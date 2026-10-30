;;;; -- Agent Model Streams --

(in-package #:autolith)

(defparameter *agent-protocol-model-history-limit* 4096
  "Maximum model request identities per connection, including active requests.")

(defclass agent-model-request ()
  ((index :initform 0 :accessor agent-model-request-index
          :documentation "Last validated request-wide event index.")
   (kind :initarg :kind :initform ':generate :reader agent-model-request-kind
         :documentation "Admitted operation, used to validate its terminal compaction outcome.")
   (attempt :initform 0 :accessor agent-model-request-attempt
            :documentation "Last started attempt.")
   (maximum :initform nil :accessor agent-model-request-maximum
            :documentation "Fixed retry ceiling after the first attempt starts.")
   (phase :initform ':new :accessor agent-model-request-phase
          :documentation "NEW, ACTIVE, or WAITING before a terminal response.")
   (retry :initform nil :accessor agent-model-request-retry
          :documentation "Whether another attempt may start.")
   (items :initform nil :accessor agent-model-request-items
          :documentation "Reverse ordered detached item encodings for this attempt.")
   (handles :initform (make-hash-table :test 'equal) :reader agent-model-request-handles
            :documentation "Provisional handles seen across all attempts.")
   (bytes :initform 0 :accessor agent-model-request-bytes
          :documentation "Cumulative encoded event bytes across attempts."))
  (:documentation "Bounded client evidence for one ordered model response stream."))

(defclass agent-model-cancellation ()
  ()
  (:documentation "One cancellation acknowledgement, separate from the target result."))

(-> agent-protocol-model-start (agent-protocol-client agent-wire:model-start) integer)
(defun agent-protocol-model-start (client start)
  "Send generation or native compaction for the active turn and return its request ID.
Receive every correlated event through AGENT-PROTOCOL-RECEIVE. Items and handles
are provisional until the terminal COMPLETE. Do not execute tools from deltas or
ITEM-COMPLETED events. A cancellation acknowledgement is not a terminal result."
  (agent-protocol--send
   client
   (lambda ()
     (let ((features (agent-wire:welcome.features (agent-protocol-client-welcome client)))
           (turn     (agent-protocol-client-turn client)))
       (unless (and (member "model.v1" features :test #'equal)
                    (member "model-items-v1" features :test #'equal))
         (error 'agent-protocol-error :reason ':model-not-negotiated))
       (unless (and turn (null (agent-protocol-client-stop client))
                    (string= (agent-wire:model-start.turn-id start)
                             (agent-wire:turn-start.turn-id (agent-wire:envelope.turn-start turn)))
                    (member (agent-wire:model-start.kind start) '(:generate :native-compaction)))
         (error 'agent-protocol-error :reason ':invalid-model-start))
       (when (or (>= (hash-table-count (agent-protocol-client-pending client))
                     (agent-wire:limits.max-in-flight-requests
                      (autolith-supervisor::parent-connection-limits client)))
                 (>= (+ (hash-table-count (agent-protocol-client-model-terminals client))
                        (loop for expected being the hash-values of (agent-protocol-client-pending client)
                              count (typep expected 'agent-model-request)))
                     *agent-protocol-model-history-limit*))
         (error 'agent-protocol-error :reason ':request-limit))
      (values (agent-wire:make-envelope :model-start start)
              (make-instance 'agent-model-request :kind (agent-wire:model-start.kind start)))))))

(-> agent-protocol-cancel-request (agent-protocol-client (integer 1)) integer)
(defun agent-protocol-cancel-request (client request-id)
  "Cancel a known model request. Receive its CancelAck separately from its result."
  (agent-protocol--send
   client
   (lambda ()
     (unless (or (typep (gethash request-id (agent-protocol-client-pending client)) 'agent-model-request)
                 (gethash request-id (agent-protocol-client-model-terminals client)))
       (error 'agent-protocol-error :reason ':unknown-model-request))
     (when (>= (hash-table-count (agent-protocol-client-pending client))
               (agent-wire:limits.max-in-flight-requests
                (autolith-supervisor::parent-connection-limits client)))
       (error 'agent-protocol-error :reason ':request-limit))
     (values (agent-wire:make-envelope :cancel-request (agent-wire:make-cancel-request :request-id request-id))
             (make-instance 'agent-model-cancellation)))))

(defmethod agent-protocol--response-data (client envelope (expected agent-model-cancellation))
  "Retire only the cancellation request, never its target model stream."
  (declare (ignore expected))
  (case (agent-wire:envelope.body-case envelope)
    (agent-wire:cancel-ack
     t)
    (agent-wire:failure
     (agent-protocol--failure client (agent-wire:envelope.failure envelope))
     t)
    (otherwise
     (agent-protocol--fatal client ':unexpected-response))))

(-> agent-model--item (agent-protocol-client agent-model-request agent-wire:output-item) null)
(defun agent-model--item (client request item)
  "Retain detached identity evidence without exposing mutable validation state."
  (let ((handle (agent-wire:output-item.replay-handle item)))
    (unless (and (= (length (agent-model-request-items request)) (agent-wire:output-item.item-index item))
                 (< (length (agent-model-request-items request)) 4096)
                 (< 0 (length handle) 1025)
                 (plusp (length (agent-wire:output-item.family item)))
                 (member (agent-wire:output-item.view-case item)
                         '(agent-wire:message agent-wire:tool-call agent-wire:tool-discovery-call
                           agent-wire:reasoning agent-wire:hosted-tool agent-wire:compaction))
                 (not (gethash handle (agent-model-request-handles request))))
      (agent-protocol--fatal client ':invalid-model-item))
    (setf (gethash (copy-seq handle) (agent-model-request-handles request)) t)
    (push (cl-protobufs:serialize-to-bytes item) (agent-model-request-items request)))
  nil)

(-> agent-model--complete (agent-protocol-client agent-model-request agent-wire:model-complete) null)
(defun agent-model--complete (client request complete)
  "Require the accepted ordered items to match the provisional item stream."
  (let ((items (agent-wire:model-complete.output-items complete))
        (saved (reverse (agent-model-request-items request))))
    (unless (and (null (agent-wire:model-complete.message complete))
                 (member (agent-wire:model-complete.reason complete) '(:stop :tool-calls :output-limit :filtered))
                 (member (agent-wire:model-complete.turn-completion complete)
                         '(:turn-completion-unspecified :ended :continued))
                 (if (eq (agent-model-request-kind request) ':native-compaction)
                     (and (eq (agent-wire:model-complete.compaction-outcome complete) ':compacted)
                          (some #'agent-wire:output-item.has-compaction items))
                     (eq (agent-wire:model-complete.compaction-outcome complete) ':compaction-outcome-unspecified))
                 (= (length items) (length saved))
                 (every (lambda (item bytes) (equalp (cl-protobufs:serialize-to-bytes item) bytes)) items saved)
                 (every (lambda (handle) (< 0 (length handle) 1025))
                        (append (when (agent-wire:model-complete.has-continuation-handle complete)
                                  (list (agent-wire:model-complete.continuation-handle complete)))
                                (when (agent-wire:model-complete.has-turn-state-handle complete)
                                  (list (agent-wire:model-complete.turn-state-handle complete))))))
      (agent-protocol--fatal client ':invalid-model-complete)))
  nil)

(-> agent-model--event (agent-protocol-client agent-model-request agent-wire:model-event) boolean)
(defun agent-model--event (client request event)
  "Validate event order and limits; return true for a terminal event."
  (let* ((kind     (agent-wire:model-event.event-case event))
         (terminal (member kind '(agent-wire:complete agent-wire:failed)))
         (bytes    (length (cl-protobufs:serialize-to-bytes event))))
    (unless (and (= (1+ (agent-model-request-index request)) (agent-wire:model-event.index event))
                 (<= (agent-wire:model-event.index event) (if terminal 65536 65535))
                 (<= (+ bytes (agent-model-request-bytes request)) (* 32 1024 1024)))
      (agent-protocol--fatal client ':invalid-model-event))
    (cond
      ((eq kind 'agent-wire:attempt-started)
       (let* ((start   (agent-wire:model-event.attempt-started event))
              (maximum (agent-wire:attempt-started.maximum-attempts start)))
         (unless (and (or (eq (agent-model-request-phase request) ':new)
                          (and (eq (agent-model-request-phase request) ':waiting)
                               (agent-model-request-retry request)))
                      (= (1+ (agent-model-request-attempt request)) (agent-wire:model-event.attempt event))
                      (<= 1 (agent-wire:model-event.attempt event) maximum 1024)
                      (or (null (agent-model-request-maximum request))
                          (= maximum (agent-model-request-maximum request)))
                      (or (plusp (agent-model-request-attempt request))
                          (zerop (agent-wire:attempt-started.delay-milliseconds start))))
           (agent-protocol--fatal client ':invalid-model-attempt))
         (setf (agent-model-request-attempt request) (agent-wire:model-event.attempt event)
               (agent-model-request-maximum request) maximum
               (agent-model-request-phase request) ':active
               (agent-model-request-items request) nil)))
      (t
       (unless (and (plusp (agent-model-request-attempt request))
                    (= (agent-model-request-attempt request) (agent-wire:model-event.attempt event))
                    (or (eq (agent-model-request-phase request) ':active)
                        (and (eq kind 'agent-wire:failed) (eq (agent-model-request-phase request) ':waiting))))
         (agent-protocol--fatal client ':invalid-model-attempt))
       (case kind
         (agent-wire:item-completed
          (agent-model--item client request (agent-wire:model-event.item-completed event)))
         (agent-wire:attempt-failed
          (let ((failed (agent-wire:model-event.attempt-failed event)))
            (agent-protocol--failure client (agent-wire:attempt-failed.failure failed))
            (setf (agent-model-request-phase request) ':waiting
                  (agent-model-request-retry request)
                  (and (agent-wire:attempt-failed.retryable failed)
                       (< (agent-model-request-attempt request) (agent-model-request-maximum request))))))
         (agent-wire:complete
          (agent-model--complete client request (agent-wire:model-event.complete event)))
         (agent-wire:failed
          (agent-protocol--failure client (agent-wire:model-event.failed event)))
         ((agent-wire:text-delta agent-wire:tool-call-delta agent-wire:metadata agent-wire:progress)
          nil)
         (otherwise
          (agent-protocol--fatal client ':invalid-model-event)))))
    (incf (agent-model-request-bytes request) bytes)
    (incf (agent-model-request-index request))
    (not (null terminal))))

(defmethod agent-protocol--response-data (client envelope (expected agent-model-request))
  "Keep a model request pending through retries and retire it at one terminal reply."
  (let ((terminal
          (case (agent-wire:envelope.body-case envelope)
            (agent-wire:failure
             (unless (zerop (agent-model-request-index expected))
               (agent-protocol--fatal client ':unexpected-response))
             (agent-protocol--failure client (agent-wire:envelope.failure envelope))
             t)
            (agent-wire:model-event
             (agent-model--event client expected (agent-wire:envelope.model-event envelope)))
            (otherwise
             (agent-protocol--fatal client ':unexpected-response)))))
    (when terminal
      (setf (gethash (agent-wire:envelope.reply-to envelope) (agent-protocol-client-model-terminals client)) t))
    terminal))
