;;;; -- Agent Protocol Driver --

(in-package #:autolith)

(defclass agent-protocol-delivery ()
  ((driver :initarg :driver :reader agent-protocol-delivery-driver
           :documentation "The driver which owns this request.")
   (id :initarg :id :reader agent-protocol-delivery-id
       :documentation "The local wire request ID.")
   (queue :initform nil :accessor agent-protocol-delivery-queue
          :documentation "Ordered validated envelopes and their byte charges.")
   (ready :initform (bordeaux-threads:make-condition-variable)
          :reader agent-protocol-delivery-ready :documentation "Wake the sole consumer.")
   (finished :initform nil :accessor agent-protocol-delivery-finished
             :documentation "Whether the consumer has received the terminal reply.")
   (waiting :initform nil :accessor agent-protocol-delivery-waiting
            :documentation "Reject concurrent consumers of one delivery."))
  (:documentation "One bounded response stream. Consume it through DRIVER-NEXT."))

(defclass agent-protocol-driver ()
  ((client :initarg :client :reader agent-protocol-driver-client
           :documentation "The existing validated protocol client.")
   (lock :initform (bordeaux-threads:make-lock "agent driver")
         :reader agent-protocol-driver-lock :documentation "Protect delivery and request registration.")
   (ready :initform (bordeaux-threads:make-condition-variable)
          :reader agent-protocol-driver-ready :documentation "Wake the lifecycle dispatcher.")
   (requests :initform (make-hash-table) :reader agent-protocol-driver-requests
             :documentation "Active and unconsumed terminal deliveries.")
   (queue :initform nil :accessor agent-protocol-driver-queue
          :documentation "Ordered lifecycle envelopes with byte charges.")
   (events :initform 0 :accessor agent-protocol-driver-events
           :documentation "Total queued or callback-owned envelope count.")
   (bytes :initform 0 :accessor agent-protocol-driver-bytes
          :documentation "Total queued or callback-owned serialized bytes.")
   (max-requests :initarg :max-requests :reader agent-protocol-driver-max-requests
                 :documentation "Maximum retained delivery handles.")
   (max-events :initarg :max-events :reader agent-protocol-driver-max-events
               :documentation "Maximum retained envelope count.")
   (max-bytes :initarg :max-bytes :reader agent-protocol-driver-max-bytes
              :documentation "Maximum retained encoded bytes.")
   (lifecycle :initarg :lifecycle :reader agent-protocol-driver-lifecycle
              :documentation "Callback receiving driver and validated lifecycle envelope.")
   (disconnect :initarg :disconnect :reader agent-protocol-driver-disconnect
               :documentation "Callback receiving driver and first local failure.")
   (failure :initform nil :accessor agent-protocol-driver-failure
            :documentation "First failure, including explicit closure.")
   (close-complete :initform nil :accessor agent-protocol-driver-close-complete
                   :documentation "Transport cleanup returned before disconnect notification.")
   (reader :initform nil :accessor agent-protocol-driver-reader
           :documentation "The sole reader thread.")
   (dispatcher :initform nil :accessor agent-protocol-driver-dispatcher
               :documentation "The lifecycle callback thread."))
  (:documentation "Optional explicit driver; no launch or provider defaults change.
After handshake, transfer exclusive client receive ownership to this driver.
Use DRIVER-REQUEST for every correlated send. Direct client lifecycle replies
and output are allowed. Call CLOSE then JOIN during cleanup. No operation is
retried. Callback code must return for JOIN to succeed."))

(-> agent-protocol-driver-start
    (agent-protocol-client &key (:lifecycle function) (:disconnect function)
     (:max-requests (integer 1)) (:max-events (integer 1)) (:max-bytes (integer 1)))
    agent-protocol-driver)
(defun agent-protocol-driver-start (client &key (lifecycle (lambda (driver envelope)
                                                            (declare (ignore driver envelope))))
                                              (disconnect (lambda (driver condition)
                                                            (declare (ignore driver condition))))
                                              (max-requests 64) (max-events 256)
                                              (max-bytes 16777216))
  "Start one reader and one lifecycle dispatcher after the client handshake.
Callbacks run outside driver locks, in lifecycle wire order. Schedule turn work
on a separate worker and return promptly so TurnCancel and PrepareStop can be
handled while a turn runs. Use existing client lifecycle reply APIs. Queue
overflow closes the connection instead of retrying. DISCONNECT runs once on the
dispatcher after closure, including explicit CLOSE."
  (check-type max-requests (integer 1))
  (check-type max-events (integer 1))
  (check-type max-bytes (integer 1))
  (let ((driver (make-instance 'agent-protocol-driver :client client :lifecycle lifecycle
                               :disconnect disconnect :max-requests max-requests
                               :max-events max-events :max-bytes max-bytes)))
    (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
      (agent-protocol--ready client)
      (when (or (agent-protocol-client-reader-owner client)
                (agent-protocol-client-reading client))
        (error 'agent-protocol-error :reason ':reader-owned))
      (when (plusp (hash-table-count (agent-protocol-client-pending client)))
        (error 'agent-protocol-error :reason ':driver-pending-requests))
      (setf (agent-protocol-client-reader-owner client) driver))
    (handler-case
        (progn
          (setf (agent-protocol-driver-dispatcher driver)
                (bordeaux-threads:make-thread (lambda () (agent-protocol-driver--dispatch driver))
                                              :name "agent lifecycle dispatcher"))
          (setf (agent-protocol-driver-reader driver)
                (bordeaux-threads:make-thread
                 (lambda ()
                   (let ((*agent-protocol-reader-owner* driver))
                     (agent-protocol-driver--read driver)))
                 :name "agent protocol reader"))
          driver)
      (error (condition)
        (agent-protocol-driver-close driver :condition condition)
        (agent-protocol-driver-join driver)
        (error condition)))))

(-> agent-protocol-driver-request (agent-protocol-driver function) agent-protocol-delivery)
(defun agent-protocol-driver-request (driver send)
  "Call SEND once with the client; return its registered delivery handle.
SEND must call exactly one existing correlated client sender and return its ID.
Registration excludes response routing, including a reply received before SEND
returns. A send failure closes the driver: its effect outcome can be unknown."
  (handler-case
      (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
        (when (agent-protocol-driver-failure driver)
          (error (agent-protocol-driver-failure driver)))
        (when (>= (hash-table-count (agent-protocol-driver-requests driver))
                  (agent-protocol-driver-max-requests driver))
          (error 'agent-protocol-error :reason ':driver-request-limit))
        (let* ((id       (funcall send (agent-protocol-driver-client driver)))
               (delivery (make-instance 'agent-protocol-delivery :driver driver :id id)))
          (unless (and (typep id '(integer 1))
                       (not (gethash id (agent-protocol-driver-requests driver))))
            (error 'agent-protocol-error :reason ':driver-invalid-send))
          (setf (gethash id (agent-protocol-driver-requests driver)) delivery)))
    (error (condition)
      (agent-protocol-driver-close driver :condition condition)
      (error condition))))

(-> agent-protocol-driver-next
    (agent-protocol-delivery &key (:timeout (or null (real 0))))
    (values (or null agent-wire:envelope) boolean))
(defun agent-protocol-driver-next (delivery &key timeout)
  "Return the next validated envelope and whether it is terminal.
Return NIL, NIL on timeout; the request continues and no retry is sent.
Consume a terminal reply exactly once. Fail after queued replies are consumed
if the channel closed. Only one consumer may wait on each delivery."
  (let* ((driver   (agent-protocol-delivery-driver delivery))
         (deadline (when timeout (+ (get-internal-real-time)
                                    (* timeout internal-time-units-per-second)))))
    (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
      (when (or (agent-protocol-delivery-finished delivery)
                (agent-protocol-delivery-waiting delivery))
        (error 'agent-protocol-error :reason ':driver-invalid-consumer))
      (setf (agent-protocol-delivery-waiting delivery) t)
      (unwind-protect
          (loop
            (let ((entry (pop (agent-protocol-delivery-queue delivery))))
              (when entry
                (agent-protocol-driver--release driver entry)
                (let ((terminal (third entry)))
                  (when terminal
                    (setf (agent-protocol-delivery-finished delivery) t)
                    (remhash (agent-protocol-delivery-id delivery)
                             (agent-protocol-driver-requests driver)))
                  (return (values (first entry) terminal)))))
            (when (agent-protocol-driver-failure driver)
              (error (agent-protocol-driver-failure driver)))
            (let ((remaining (when deadline
                               (/ (- deadline (get-internal-real-time))
                                  internal-time-units-per-second))))
              (when (and remaining (<= remaining 0))
                (return (values nil nil)))
              (bordeaux-threads:condition-wait (agent-protocol-delivery-ready delivery)
                                               (agent-protocol-driver-lock driver)
                                               :timeout remaining)))
        (setf (agent-protocol-delivery-waiting delivery) nil)))))

(-> agent-protocol-driver-cancel (agent-protocol-driver agent-protocol-delivery) agent-protocol-delivery)
(defun agent-protocol-driver-cancel (driver delivery)
  "Send cancellation once. Consume the ACK separately from the target terminal."
  (unless (eq driver (agent-protocol-delivery-driver delivery))
    (error 'agent-protocol-error :reason ':driver-foreign-delivery))
  (agent-protocol-driver-request
   driver (lambda (client) (agent-protocol-cancel-request client (agent-protocol-delivery-id delivery)))))

(-> agent-protocol-driver-close (agent-protocol-driver &key (:condition condition)) null)
(defun agent-protocol-driver-close (driver &key (condition (make-condition 'agent-protocol-error
                                                                         :reason ':driver-closed)))
  "Close the transport, wake all waiters, and retain the first failure. Idempotent."
  (let ((close-owner
          (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
            (unless (agent-protocol-driver-failure driver)
              (setf (agent-protocol-driver-failure driver) condition)
              (dolist (entry (agent-protocol-driver-queue driver))
                (agent-protocol-driver--release driver entry))
              (setf (agent-protocol-driver-queue driver) nil)
              (maphash (lambda (id delivery)
                         (declare (ignore id))
                         (bordeaux-threads:condition-notify (agent-protocol-delivery-ready delivery)))
                       (agent-protocol-driver-requests driver))
              t))))
    (when close-owner
      (agent-protocol-close (agent-protocol-driver-client driver))
      (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
        (setf (agent-protocol-driver-close-complete driver) t)
        (bordeaux-threads:condition-notify (agent-protocol-driver-ready driver)))))
  nil)

(-> agent-protocol-driver-join (agent-protocol-driver &key (:timeout (real (0)))) null)
(defun agent-protocol-driver-join (driver &key (timeout 10))
  "Join both threads within TIMEOUT seconds. Call CLOSE first.
Signal DRIVER-JOIN-TIMEOUT if callbacks or stream cleanup do not return.
Do not join from a driver callback; close there and join from its owner."
  (when (member (bordeaux-threads:current-thread)
                (list (agent-protocol-driver-reader driver) (agent-protocol-driver-dispatcher driver)))
    (error 'agent-protocol-error :reason ':driver-self-join))
  (handler-case
      (sb-ext:with-timeout timeout
        (dolist (thread (list (agent-protocol-driver-reader driver)
                             (agent-protocol-driver-dispatcher driver)))
          (when thread (bordeaux-threads:join-thread thread))))
    (sb-ext:timeout ()
      (error 'agent-protocol-error :reason ':driver-join-timeout)))
  nil)

(-> agent-protocol-driver--release (agent-protocol-driver list) null)
(defun agent-protocol-driver--release (driver entry)
  "Release the count and bytes of ENTRY under the driver lock."
  (decf (agent-protocol-driver-events driver))
  (decf (agent-protocol-driver-bytes driver) (second entry))
  nil)

(-> agent-protocol-driver--terminal-p (agent-wire:envelope) boolean)
(defun agent-protocol-driver--terminal-p (envelope)
  "Classify already validated replies without consulting mutable client state."
  (if (eq 'agent-wire:model-event (agent-wire:envelope.body-case envelope))
      (not (null (member (agent-wire:model-event.event-case (agent-wire:envelope.model-event envelope))
                         '(agent-wire:complete agent-wire:failed))))
      t))

(-> agent-protocol-driver--read (agent-protocol-driver) null)
(defun agent-protocol-driver--read (driver)
  "Route only envelopes accepted by the existing sole-reader validator."
  (handler-case
      (loop
        (let ((envelope (agent-protocol-receive (agent-protocol-driver-client driver))))
          (when envelope
            (let ((bytes (length (cl-protobufs:serialize-to-bytes envelope))))
              (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
                (when (agent-protocol-driver-failure driver) (return))
                (when (or (>= (agent-protocol-driver-events driver)
                              (agent-protocol-driver-max-events driver))
                          (> (+ bytes (agent-protocol-driver-bytes driver))
                             (agent-protocol-driver-max-bytes driver)))
                  (error 'agent-protocol-error :reason ':driver-delivery-limit))
                (let* ((id       (when (agent-wire:envelope.has-reply-to envelope)
                                   (agent-wire:envelope.reply-to envelope)))
                       (delivery (when id (gethash id (agent-protocol-driver-requests driver))))
                       (terminal (and id (agent-protocol-driver--terminal-p envelope)))
                       (entry    (list envelope bytes terminal)))
                  (when (and id (null delivery))
                    (error 'agent-protocol-error :reason ':driver-unregistered-reply))
                  (incf (agent-protocol-driver-events driver))
                  (incf (agent-protocol-driver-bytes driver) bytes)
                  (if delivery
                      (progn
                        (setf (agent-protocol-delivery-queue delivery)
                              (nconc (agent-protocol-delivery-queue delivery) (list entry)))
                        (bordeaux-threads:condition-notify (agent-protocol-delivery-ready delivery)))
                      (progn
                        (setf (agent-protocol-driver-queue driver)
                              (nconc (agent-protocol-driver-queue driver) (list entry)))
                        (bordeaux-threads:condition-notify (agent-protocol-driver-ready driver))))))))))
    (error (condition) (agent-protocol-driver-close driver :condition condition)))
  nil)

(-> agent-protocol-driver--dispatch (agent-protocol-driver) null)
(defun agent-protocol-driver--dispatch (driver)
  "Deliver lifecycle calls outside locks; account for a callback-owned envelope."
  (handler-case
      (loop
        (let ((entry
                (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
                  (loop until (or (agent-protocol-driver-queue driver)
                                  (agent-protocol-driver-failure driver))
                        do (bordeaux-threads:condition-wait (agent-protocol-driver-ready driver)
                                                            (agent-protocol-driver-lock driver)))
                  (pop (agent-protocol-driver-queue driver)))))
          (unless entry (return))
          (unwind-protect
              (funcall (agent-protocol-driver-lifecycle driver) driver (first entry))
            (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
              (agent-protocol-driver--release driver entry)))))
    (error (condition) (agent-protocol-driver-close driver :condition condition)))
  (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
    (loop until (agent-protocol-driver-close-complete driver)
          do (bordeaux-threads:condition-wait (agent-protocol-driver-ready driver)
                                              (agent-protocol-driver-lock driver))))
  ;; Notification errors cannot restart dispatch or replace the first failure.
  (handler-case
      (funcall (agent-protocol-driver-disconnect driver) driver (agent-protocol-driver-failure driver))
    (error () nil))
  nil)
