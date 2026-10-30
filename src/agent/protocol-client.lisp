;;;; -- Agent Parent-Protocol Client --

(in-package #:autolith)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:agent-wire '#:cl-protobufs.autolith.parent.v1
                                     '#:autolith))

(defparameter *agent-protocol-write-timeout* 5
  "Maximum seconds for a client write, including writer admission.")

(defparameter *agent-protocol-assignment-limit* 65536
  "Maximum distinct durable assignments retained per connection. Never evict old identities.")

(defparameter *agent-protocol-assignment-character-limit* 1048576
  "Maximum total retained event-ID characters per connection.")

(defvar *agent-protocol-reader-owner* nil
  "Dynamic receive authority for a client permanently assigned to a driver.")

(defclass agent-protocol-client (autolith-supervisor::parent-connection)
  ((state-lock :initform      (bordeaux-threads:make-lock "agent protocol state")
               :reader        agent-protocol-client-state-lock
               :documentation "Serialize requests and lifecycle state transitions.")
   (assignments :initform      (make-hash-table :test 'equal)
                :reader        agent-protocol-client-assignments
                :documentation "Exact event IDs mapped to observed durable sequences.")
   (sequences :initform      (make-hash-table)
              :reader        agent-protocol-client-sequences
              :documentation "Observed durable sequences mapped to exact event IDs.")
   (assignment-characters :initform 0 :accessor agent-protocol-client-assignment-characters
                          :documentation "Total characters copied into the durable assignment index.")
   (reading :initform nil :accessor agent-protocol-client-reading
            :documentation "Single reader claim, protected by the state lock.")
   (reader-owner :initform nil :accessor agent-protocol-client-reader-owner
                 :documentation "Permanent receive owner, or NIL for explicit per-call reads.")
   (pending :initform      (make-hash-table)
            :reader        agent-protocol-client-pending
            :documentation "Outstanding local request IDs and their expected response data.")
   (model-terminals :initform (make-hash-table) :reader agent-protocol-client-model-terminals
                    :documentation "Terminal model request IDs retained for repeated cancellation.")
   (welcome :initform nil :accessor agent-protocol-client-welcome
             :documentation "A private copy of the accepted Welcome, available before initialization.")
   (turn :initform nil :accessor agent-protocol-client-turn
         :documentation "The active parent TurnStart envelope, if any.")
   (stop :initform nil :accessor agent-protocol-client-stop
         :documentation "The outstanding parent PrepareStop envelope, if any.")
   (last-sequence :initform 0 :accessor agent-protocol-client-last-sequence
                  :documentation "The largest durable session sequence observed."))
  (:documentation "An optional explicit-stream client. No startup or service adapter is installed.
Use AGENT-PROTOCOL-HANDSHAKE with an initialization callback, then call
AGENT-PROTOCOL-RECEIVE from exactly one reader. Session and model calls return
request IDs; receive returns correlated replies, model events and lifecycle messages. Finish each accepted
turn and stop obligation explicitly. A disconnect never authorizes an effect retry."))

(define-condition agent-protocol-error (error)
  ((reason :initarg :reason :reader agent-protocol-error-reason
           :documentation "A fixed local failure category."))
  (:documentation "A local agent client state or peer semantic failure.")
  (:report (lambda (condition stream)
             (format stream "Agent protocol failed: ~a." (agent-protocol-error-reason condition)))))

(-> agent-protocol--message-copy (t) t)
(defun agent-protocol--message-copy (message)
  "Detach bounded protocol data before exposing another copy to the caller."
  (cl-protobufs:deserialize-from-bytes (type-of message) (cl-protobufs:serialize-to-bytes message)))

(-> agent-protocol-open (stream stream string) agent-protocol-client)
(defun agent-protocol-open (input output instance-id)
  "Create an unstarted client with parent-assigned INSTANCE-ID and binary streams."
  (unless (plusp (length instance-id))
    (error 'agent-protocol-error :reason ':empty-instance))
  (make-instance 'agent-protocol-client :input input :output output :instance-id instance-id))

(-> agent-protocol-close (agent-protocol-client) null)
(defun agent-protocol-close (client)
  "Mark the client closed, clear pending requests, and bound stream cleanup."
  (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
    (setf (autolith-supervisor::parent-connection-phase client) ':closed)
    (clrhash (agent-protocol-client-pending client)))
  (dolist (stream (list (autolith-supervisor::parent-connection-input client)
                       (autolith-supervisor::parent-connection-output client)))
    (handler-case (sb-ext:with-timeout 0.1 (close stream :abort t))
      (error () nil)))
  nil)

(-> agent-protocol--fatal (agent-protocol-client keyword) nil)
(defun agent-protocol--fatal (client reason)
  "Signal REASON. The outer operation closes CLIENT after releasing its state lock."
  (declare (ignore client))
  (error 'agent-protocol-error :reason reason))

(-> agent-protocol--send (agent-protocol-client function) integer)
(defun agent-protocol--send (client prepare)
  "Order registration and writes without holding the state lock during I/O.
PREPARE returns an envelope and optionally expected reply data under the state lock."
  (let ((prepared nil))
    (handler-case
        (sb-ext:with-timeout *agent-protocol-write-timeout*
          (bordeaux-threads:with-lock-held ((autolith-supervisor::parent-connection-write-lock client))
            (let ((envelope
                    (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
                      (agent-protocol--ready client)
                      (multiple-value-bind (envelope expected) (funcall prepare)
                        (when expected
                          (setf (gethash (1+ (autolith-supervisor::parent-connection-sent client))
                                         (agent-protocol-client-pending client)) expected))
                        (setf prepared t)
                        envelope))))
              (agent-protocol--write client envelope)
              (agent-wire:envelope.message-id envelope))))
      (sb-ext:timeout ()
        (agent-protocol-close client)
        (error 'agent-protocol-error :reason ':write-timeout))
      (error (condition)
        (when prepared (agent-protocol-close client))
        (error condition)))))

(-> agent-protocol--write (agent-protocol-client agent-wire:envelope) null)
(defun agent-protocol--write (client envelope)
  "Write one frame using the shared codec without unbounded parent cleanup.
The caller owns sender admission or the exclusive handshake reader claim."
  (when (eq ':closed (autolith-supervisor::parent-connection-phase client))
    (error 'agent-protocol-error :reason ':closed))
  (let ((limits (autolith-supervisor::parent-connection-limits client)))
    (setf (agent-wire:envelope.protocol-major envelope) 1
          (agent-wire:envelope.instance-id envelope) (autolith-supervisor::parent-connection-instance-id client)
          (agent-wire:envelope.message-id envelope) (1+ (autolith-supervisor::parent-connection-sent client)))
    (let ((bytes (cl-protobufs:serialize-to-bytes envelope)))
      (autolith-supervisor::protocol-validate-protobuf
       bytes 'agent-wire:envelope
       :max-bytes   (agent-wire:limits.max-frame-bytes limits)
       :max-depth   (agent-wire:limits.max-value-depth limits)
       :max-entries (agent-wire:limits.max-collection-entries limits))
      (autolith-supervisor::protocol-write-frame
       (autolith-supervisor::parent-connection-output client) bytes
       :limit (agent-wire:limits.max-frame-bytes limits)))
    (incf (autolith-supervisor::parent-connection-sent client)))
  nil)

(-> agent-protocol--assignment (agent-protocol-client string integer) null)
(defun agent-protocol--assignment (client event sequence)
  "Retain an exact bidirectional durable assignment. Fail closed at the memory bound.
Keep old entries so retries and later pages cannot erase evidence of a conflict."
  (let* ((events         (agent-protocol-client-assignments client))
         (sequences      (agent-protocol-client-sequences client))
         (known-sequence (gethash event events))
         (known-event    (gethash sequence sequences)))
    (when (or (and known-sequence (/= sequence known-sequence))
              (and known-event (not (string= event known-event))))
      (agent-protocol--fatal client ':conflicting-session-assignment))
    (unless known-sequence
      (when (or (>= (hash-table-count events) *agent-protocol-assignment-limit*)
                (> (+ (agent-protocol-client-assignment-characters client) (length event))
                   *agent-protocol-assignment-character-limit*))
        (agent-protocol--fatal client ':assignment-limit))
      (let ((copy (copy-seq event)))
        (setf (gethash copy events) sequence
              (gethash sequence sequences) copy)
        (incf (agent-protocol-client-assignment-characters client) (length copy)))))
  nil)

(-> agent-protocol--ready (agent-protocol-client) null)
(defun agent-protocol--ready (client)
  "Require an initialized, open channel."
  (unless (eq ':ready (autolith-supervisor::parent-connection-phase client))
    (error 'agent-protocol-error :reason ':not-ready))
  nil)

(-> agent-protocol--claim-reader (agent-protocol-client) null)
(defun agent-protocol--claim-reader (client)
  "Claim the byte reader without blocking or allowing recursive reads."
  (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
    (when (and (agent-protocol-client-reader-owner client)
               (not (eq *agent-protocol-reader-owner* (agent-protocol-client-reader-owner client))))
      (error 'agent-protocol-error :reason ':reader-owned))
    (when (agent-protocol-client-reading client)
      (error 'agent-protocol-error :reason ':concurrent-reader))
    (setf (agent-protocol-client-reading client) t))
  nil)

(-> agent-protocol--release-reader (agent-protocol-client) null)
(defun agent-protocol--release-reader (client)
  "Release the byte reader after normal completion or an error."
  (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
    (setf (agent-protocol-client-reading client) nil))
  nil)

(-> agent-protocol--page (agent-protocol-client agent-wire:session-page integer) null)
(defun agent-protocol--page (client page after)
  "Check record order, parent metadata, payload values, and page progress."
  (let ((sequence after) (high (agent-wire:session-page.high-water-sequence page)))
    (dolist (record (agent-wire:session-page.records page))
      (unless
          (and (< sequence (agent-wire:session-record.sequence record))
               (<= (agent-wire:session-record.sequence record) high)
               (plusp (length (agent-wire:session-record.event-id record)))
               (plusp (length (agent-wire:session-record.turn-id record)))
               (member (agent-wire:session-record.origin record) '(:parent :agent))
               (member (agent-wire:session-record.kind record)
                       '(:user-input :assistant-message :tool-started :tool-finished :note
                         :turn-outcome))
               (case (agent-wire:session-record.kind record)
                 ((:user-input :turn-outcome)
                  (eq ':parent (agent-wire:session-record.origin record)))
                 (:note
                  t)
                 (otherwise
                  (eq ':agent (agent-wire:session-record.origin record)))))
        (agent-protocol--fatal client ':invalid-session-page))
      (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload record))
      (progn
       (agent-protocol--assignment client (agent-wire:session-record.event-id record)
        (agent-wire:session-record.sequence record))
       (setf sequence (agent-wire:session-record.sequence record))))
    (when (and (agent-wire:session-page.has-more page) (or (= sequence after) (>= sequence high)))
      (agent-protocol--fatal client ':invalid-session-page))
    (when (agent-wire:session-page.records page)
      (setf (agent-protocol-client-last-sequence client)
            (max (agent-protocol-client-last-sequence client) sequence))))
  nil)

(-> agent-protocol-handshake
    (agent-protocol-client &key (:initialize function) (:timeout (real (0))) (:features list)) agent-wire:welcome)
(defun agent-protocol-handshake (client &key (initialize #'identity) (timeout 30)
                                           (features '("session.v1")))
  "Offer implemented FEATURES, validate Welcome, initialize, then send Ready.
Call INITIALIZE with Welcome before Ready. Do not read the channel in that callback.
The deadline covers I/O and initialization. Return the accepted Welcome."
  (unless (and (member "session.v1" features :test #'equal)
               (= (length features) (length (remove-duplicates features :test #'equal)))
               (subsetp features '("session.v1" "model.v1" "model-items-v1" "records.v1") :test #'equal)
               (eq (not (null (member "model.v1" features :test #'equal)))
                   (not (null (member "model-items-v1" features :test #'equal)))))
    (error 'agent-protocol-error :reason ':unsupported-features))
  (agent-protocol--claim-reader client)
  (unwind-protect
      (handler-case
       (sb-ext:with-timeout timeout
          (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
            (unless (eq ':new (autolith-supervisor::parent-connection-phase client))
              (error 'agent-protocol-error :reason ':handshake-already-started))
            (setf (autolith-supervisor::parent-connection-phase client) ':awaiting-welcome))
         (agent-protocol--write client
                                  (agent-wire:make-envelope :hello
                                                           (agent-wire:make-hello :features features)))
         (multiple-value-bind (envelope bytes)
             (autolith-supervisor::parent-connection-receive client)
           (let ((welcome (agent-wire:envelope.welcome envelope)))
             (unless
                 (and welcome (agent-wire:envelope.has-reply-to envelope)
                      (= 1 (agent-wire:envelope.reply-to envelope)))
               (agent-protocol--fatal client ':invalid-welcome))
             (autolith-supervisor::parent-connection--validate-welcome welcome features)
             (let ((selected (agent-wire:welcome.features welcome)))
               (unless (eq (not (null (member "model.v1" selected :test #'equal)))
                           (not (null (member "model-items-v1" selected :test #'equal))))
                 (agent-protocol--fatal client ':invalid-welcome)))
             (setf (autolith-supervisor::parent-connection-limits client)
                   (agent-protocol--message-copy (agent-wire:welcome.limits welcome)))
             (let ((limits (agent-wire:welcome.limits welcome)))
               (autolith-supervisor::protocol-validate-protobuf bytes 'agent-wire:envelope
                                                                :max-bytes
                                                                (agent-wire:limits.max-frame-bytes
                                                                 limits)
                                                                :max-depth
                                                                (agent-wire:limits.max-value-depth
                                                                 limits)
                                                                :max-entries
                                                                (agent-wire:limits.max-collection-entries
                                                                 limits)))
             (when (agent-wire:welcome.configuration welcome)
               (autolith-supervisor::protocol-map->data
                (agent-wire:welcome.configuration welcome)))
             (dolist (grant (agent-wire:welcome.active-grants welcome))
               (unless
                   (and (member (agent-wire:grant.resource grant) '(:filesystem :network :service))
                        (member (agent-wire:grant.duration grant) '(:instance :session))
                        (plusp (length (agent-wire:grant.target grant)))
                        (agent-wire:grant.operations grant)
                        (every (lambda (operation) (plusp (length operation)))
                               (agent-wire:grant.operations grant)))
                 (agent-protocol--fatal client ':invalid-grant)))
             (agent-protocol--page client (agent-wire:welcome.resume welcome) 0)
             (setf (agent-protocol-client-welcome client) (agent-protocol--message-copy welcome))
             (funcall initialize welcome)
             (agent-protocol--write client
                                    (agent-wire:make-envelope :reply-to
                                                              (agent-wire:envelope.message-id
                                                               envelope)
                                                              :ready (agent-wire:make-ready)))
             (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
               (unless (eq ':awaiting-welcome (autolith-supervisor::parent-connection-phase client))
                 (agent-protocol--fatal client ':closed))
               (setf (autolith-supervisor::parent-connection-phase client) ':ready))
             welcome)))
       (sb-ext:timeout nil (agent-protocol-close client)
        (agent-protocol--fatal client ':handshake-timeout))
       (error (condition) (agent-protocol-close client) (error condition)))
    (agent-protocol--release-reader client)))

(-> agent-protocol--request (agent-protocol-client agent-wire:envelope list) integer)
(defun agent-protocol--request (client envelope expected)
  "Register before sending, without holding the state lock during I/O."
  (agent-protocol--send
   client (lambda ()
            (when (>= (hash-table-count (agent-protocol-client-pending client))
                      (agent-wire:limits.max-in-flight-requests
                       (autolith-supervisor::parent-connection-limits client)))
              (error 'agent-protocol-error :reason ':request-limit))
            (values envelope expected))))

(-> agent-protocol-session-read
    (agent-protocol-client &key (:after-sequence (integer 0)) (:limit (integer 1 256))) integer)
(defun agent-protocol-session-read (client &key (after-sequence 0) (limit 256))
  "Request one bounded session page. Receive its correlated response with RECEIVE."
  (check-type after-sequence (unsigned-byte 64))
  (check-type limit (integer 1 256))
  (agent-protocol--request
   client (agent-wire:make-envelope :session-read (agent-wire:make-session-read
                                                   :after-sequence after-sequence :limit limit))
   (list 'agent-wire:session-page after-sequence limit)))

(-> agent-protocol-session-append (agent-protocol-client agent-wire:session-record) integer)
(defun agent-protocol-session-append (client record)
  "Submit RECORD without parent metadata. The parent validates authority and deduplication.
Wait for its matching SessionAck before executing effects or reporting durability."
  (unless
      (and (zerop (agent-wire:session-record.sequence record))
           (eq ':origin-unspecified (agent-wire:session-record.origin record))
           (plusp (length (agent-wire:session-record.event-id record)))
           (plusp (length (agent-wire:session-record.turn-id record)))
           (member (agent-wire:session-record.kind record)
                   '(:assistant-message :tool-started :tool-finished :note)))
    (error 'agent-protocol-error :reason ':invalid-append))
  (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload record))
  (agent-protocol--request client
                           (agent-wire:make-envelope :session-append
                                                     (agent-wire:make-session-append :record
                                                                                     record))
                           (list 'agent-wire:session-ack
                                 (copy-seq (agent-wire:session-record.event-id record)))))

(-> agent-protocol--reject (agent-protocol-client agent-wire:envelope keyword) null)
(defun agent-protocol--reject (client envelope code)
  "Send a bounded fixed Failure outside the state lock."
  (agent-protocol--send
   client (lambda ()
            (agent-wire:make-envelope
             :reply-to (agent-wire:envelope.message-id envelope)
             :failure  (agent-wire:make-failure :code code :message "Unsupported or invalid parent request."))))
  nil)

(-> agent-protocol-receive (agent-protocol-client) (or null agent-wire:envelope))
(defun agent-protocol-receive (client)
  "Read one envelope from the sole reader. Return replies, model events or lifecycle messages.
A returned Failure terminates its correlated request. Return NIL after
rejecting an invalid request, or ignoring an inactive cancellation. Check the
active turn ID before acting on cancellation. Concurrent reads fail before I/O.
Do not retry effects after disconnect."
  (agent-protocol--claim-reader client)
  (unwind-protect
      (handler-case
       (progn
        (agent-protocol--ready client)
        (let ((envelope  (autolith-supervisor::parent-connection-receive client))
              (accepted  t)
              (rejection nil))
          (with-lock-held ((agent-protocol-client-state-lock client))
            (let ((body (agent-wire:envelope.body-case envelope)))
              (if (agent-wire:envelope.has-reply-to envelope)
                  (agent-protocol--response client envelope body)
                  (case body
                    (agent-wire:turn-start
                     (let ((turn (agent-wire:envelope.turn-start envelope)))
                       (if (and (null (agent-protocol-client-turn client))
                                (null (agent-protocol-client-stop client))
                                (plusp (length (agent-wire:turn-start.turn-id turn)))
                                (plusp (agent-wire:turn-start.input-sequence turn)))
                           (setf (agent-protocol-client-turn client) (agent-protocol--message-copy envelope))
                           (progn (setf accepted nil) (setf rejection ':invalid-request)))))
                    (agent-wire:turn-cancel
                     (let ((id
                            (agent-wire:turn-cancel.turn-id
                             (agent-wire:envelope.turn-cancel envelope)))
                           (turn (agent-protocol-client-turn client)))
                       (unless
                           (and turn
                                (string= id
                                         (agent-wire:turn-start.turn-id
                                          (agent-wire:envelope.turn-start turn))))
                         (setf accepted nil))
                       (when (zerop (length id)) (setf rejection ':invalid-request))))
                    (agent-wire:prepare-stop
                     (if (agent-protocol-client-stop client)
                         (progn (setf accepted nil) (setf rejection ':invalid-request))
                         (setf (agent-protocol-client-stop client) (agent-protocol--message-copy envelope))))
                    (otherwise
                     (setf accepted nil) (setf rejection ':unsupported))))))
          (progn
           (when rejection (agent-protocol--reject client envelope rejection))
           (when accepted envelope))))
       (error (condition) (agent-protocol-close client) (error condition)))
    (agent-protocol--release-reader client)))

(defgeneric agent-protocol--response-data (client envelope expected)
  (:documentation "Validate a correlated response under the state lock; return true only at its terminal reply."))

(-> agent-protocol--failure (agent-protocol-client agent-wire:failure) null)
(defun agent-protocol--failure (client failure)
  "Require a defined failure category without using peer report text."
  (unless (and failure
               (member (agent-wire:failure.code failure)
                       '(:invalid-request :permission-denied :revision-conflict :unsupported
                         :resource-limit :cancelled :unavailable :internal)))
    (agent-protocol--fatal client ':invalid-failure))
  nil)

(defmethod agent-protocol--response-data (client envelope (expected cons))
  "Validate one terminal session response. Caller holds the client state lock."
  (let ((body (agent-wire:envelope.body-case envelope)))
    (unless (or (eq body (first expected)) (eq body 'agent-wire:failure))
      (agent-protocol--fatal client ':unexpected-response))
    (case body
      (agent-wire:session-page
       (let ((page (agent-wire:envelope.session-page envelope)))
         (when (> (length (agent-wire:session-page.records page)) (third expected))
           (agent-protocol--fatal client ':invalid-session-page))
         (agent-protocol--page client page (second expected))))
      (agent-wire:session-ack
       (let ((ack (agent-wire:envelope.session-ack envelope)))
         (unless
             (and (string= (second expected) (agent-wire:session-ack.event-id ack))
                  (plusp (agent-wire:session-ack.sequence ack)))
           (agent-protocol--fatal client ':invalid-session-ack))
         (progn
          (agent-protocol--assignment client (agent-wire:session-ack.event-id ack)
           (agent-wire:session-ack.sequence ack))
          (setf (agent-protocol-client-last-sequence client)
                (max (agent-protocol-client-last-sequence client)
                     (agent-wire:session-ack.sequence ack))))))
      (agent-wire:failure
       (agent-protocol--failure client (agent-wire:envelope.failure envelope)))))
  t)

(-> agent-protocol--response (agent-protocol-client agent-wire:envelope symbol) null)
(defun agent-protocol--response (client envelope body)
  "Route one correlated response. Caller holds the state lock."
  (declare (ignore body))
  (let* ((id       (agent-wire:envelope.reply-to envelope))
         (expected (gethash id (agent-protocol-client-pending client))))
    (unless expected (agent-protocol--fatal client ':unexpected-response))
    (when (agent-protocol--response-data client envelope expected)
      (remhash id (agent-protocol-client-pending client))))
  nil)

(-> agent-protocol-finish-turn (agent-protocol-client keyword) null)
(defun agent-protocol-finish-turn (client outcome)
  "Send the terminal reply after all session replies arrive, with bounded I/O."
  (agent-protocol--send
   client
   (lambda ()
     (unless (and (agent-protocol-client-turn client)
                  (member outcome '(:completed :cancelled :failed))
                  (zerop (hash-table-count (agent-protocol-client-pending client))))
       (error 'agent-protocol-error :reason ':invalid-turn-finish))
     (let ((turn (agent-protocol-client-turn client)))
       (prog1
           (agent-wire:make-envelope
            :reply-to    (agent-wire:envelope.message-id turn)
            :turn-finish (agent-wire:make-turn-finish
                          :turn-id (agent-wire:turn-start.turn-id (agent-wire:envelope.turn-start turn))
                          :outcome outcome :last-session-sequence (agent-protocol-client-last-sequence client)))
         (setf (agent-protocol-client-turn client) nil)))))
  nil)

(-> agent-protocol-ready-to-stop (agent-protocol-client &key (:wrap-up-event-id (or null string))) null)
(defun agent-protocol-ready-to-stop (client &key wrap-up-event-id)
  "Reply to PrepareStop after the turn and requests end. Close and exit afterward."
  (agent-protocol--send
   client
   (lambda ()
     (unless (and (agent-protocol-client-stop client)
                  (null (agent-protocol-client-turn client))
                  (zerop (hash-table-count (agent-protocol-client-pending client)))
                  (or (null wrap-up-event-id) (plusp (length wrap-up-event-id))))
       (error 'agent-protocol-error :reason ':invalid-ready-to-stop))
     (let ((ready (agent-wire:make-ready-to-stop
                   :last-session-sequence (agent-protocol-client-last-sequence client))))
       (when wrap-up-event-id (setf (agent-wire:ready-to-stop.wrap-up-event-id ready) wrap-up-event-id))
       (prog1
           (agent-wire:make-envelope :reply-to      (agent-wire:envelope.message-id (agent-protocol-client-stop client))
                                     :ready-to-stop ready)
         (setf (autolith-supervisor::parent-connection-phase client) ':stopped)))))
  nil)

(-> agent-protocol-output (agent-protocol-client keyword string) null)
(defun agent-protocol-output (client channel text)
  "Send bounded transient display text without persisting an assistant record."
  (agent-protocol--send
   client (lambda ()
            (unless (member channel '(:progress :diagnostic))
              (error 'agent-protocol-error :reason ':invalid-output-channel))
            (agent-wire:make-envelope :output (agent-wire:make-output :channel channel :text text))))
  nil)
