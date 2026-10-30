;;;; -- Optional Parent Model Service --

(in-package #:autolith-supervisor)

(define-condition model-service-cancelled (error) ()
  (:documentation "Internal cancellation fence. No provider or peer text is retained."))

(defclass model-invocation ()
  ((inputs :initarg :inputs :reader model-invocation-inputs
           :documentation "Seven validated projection values, owned by the trusted executor.")
   (wire-id :initarg :wire-id :reader model-invocation-wire-id
            :documentation "Original connection-local ModelStart message ID.")
   (request-id :initarg :request-id :reader model-invocation-request-id
               :documentation "Durable instance-scoped replay key.")
   (turn-id :initarg :turn-id :reader model-invocation-turn-id
            :documentation "Admitted parent turn, copied at admission.")
   (deadline :initarg :deadline :reader model-invocation-deadline
             :documentation "Absolute parent monotonic deadline, including retry delays.")
   (collector :initform nil :accessor model-invocation-collector
              :documentation "Collector used only by the storage owner.")
   (cancel-code :initform nil :accessor model-invocation-cancel-code
                :documentation "First cancellation cause, under the mailbox lock.")
   (cancel-request-id :initform nil :accessor model-invocation-cancel-request-id
                      :documentation "First explicit cancellation ID, under the mailbox lock.")
   (cancelled-by :initform nil :accessor model-invocation-cancelled-by
                 :documentation "Explicit cancellation that caused the terminal failure. Owner-only.")
   (abort-called :initform nil :accessor model-invocation-abort-called
                 :documentation "The control worker has claimed abort delivery.")
   (abort-returned :initform nil :accessor model-invocation-abort-returned
                   :documentation "The trusted abort callback has returned.")
   (thread :initform nil :accessor model-invocation-thread
           :documentation "Tracked executor thread.")
   (returned :initform nil :accessor model-invocation-returned
             :documentation "Executor return is available in the independent result slot.")
   (result :initform nil :accessor model-invocation-result
           :documentation "Validated library result or NIL after an executor error.")
   (first-retry :initform nil :accessor model-invocation-first-retry
                :documentation "Initial pinned-library attempt announcement was consumed.")
   (accepting :initform nil :accessor model-invocation-accepting
              :documentation "Acceptance operation in progress, not a durable terminal outcome. Protected by the mailbox lock.")
   (terminal :initform nil :accessor model-invocation-terminal
             :documentation "Durably accepted, failed or interrupted. Owner-only."))
  (:documentation "One admitted model call. Callback completion is not executor return."))

(defclass model-service ()
  ((profiles :initarg :profiles :reader model-service-profiles
             :documentation "Trusted parent profile registry.")
   (replay-store :initarg :replay-store :reader model-service-replay-store
                 :documentation "Durable replay journal for the attached session.")
   (executor :initarg :executor :reader model-service-executor
             :documentation "Trusted FUNCTION of invocation and synchronous ordered event callback.")
   (abort :initarg :abort :reader model-service-abort
          :documentation "Idempotent FUNCTION of invocation which unblocks its real provider transport.")
   (clock :initarg :clock :reader model-service-clock
          :documentation "Trusted monotonic seconds function.")
   (timeout :initarg :timeout :reader model-service-timeout
            :documentation "Maximum request duration in seconds, including retries.")
   (maximum-attempts :initarg :maximum-attempts :reader model-service-maximum-attempts
                     :documentation "Executor retry policy, announced in the first durable event.")
   (nonblocking-output :initarg :nonblocking-output :reader model-service-nonblocking-output
                       :documentation "Trusted assertion that a configured transient output callback cannot block.")
   (mailbox :initform (make-instance 'model-runtime-mailbox) :reader model-service-mailbox
            :documentation "Bounded owner/writer queues and short cancellation lock.")
   (runtime :initform nil :accessor model-service-runtime
            :documentation "Exactly one attached session runtime.")
   (active :initform nil :accessor model-service-active
           :documentation "One call, including a cancelled executor that has not returned.")
   (pending-starts :initform (make-hash-table) :reader model-service-pending-starts
                   :documentation "Bounded reader admission latches for starts not yet dispatched.")
   (abort-ready :initform      (bordeaux-threads:make-condition-variable)
                :reader        model-service-abort-ready
                :documentation "Independent control-worker notification.")
   (abort-thread :initform nil :accessor model-service-abort-thread
                 :documentation "Tracked abort worker, separate from storage and peer I/O.")
   (deadline-ready :initform      (bordeaux-threads:make-condition-variable)
                   :reader        model-service-deadline-ready
                   :documentation "Independent deadline monitor wakeup.")
   (deadline-thread :initform nil :accessor model-service-deadline-thread
                    :documentation "Tracked clock monitor, independent of transport abort.")
   (quiesced :initform nil :accessor model-service-quiesced
             :documentation "All service workers joined before session-store interruption."))
  (:documentation "Optional ordinary-generation service. Never installs a provider or changes launch."))

(declaim (ftype (function (&key (:profiles hash-table) (:replay-store model-replay-store)
                                (:executor function) (:abort function) (:clock function)
                                (:timeout real) (:maximum-attempts integer)
                                (:nonblocking-output boolean)) model-service)
                make-model-service))

(defun make-model-service (&key profiles replay-store executor abort (clock #'session-runtime--clock)
                               (timeout 120) (maximum-attempts 1) nonblocking-output)
  "Construct a parent-installed service. No accounts, HTTP or agent systems are loaded.
EXECUTOR receives (INVOCATION CALLBACK). INPUTS contains the seven projection values.
It must emit bounded ordered callbacks synchronously, stop callbacks before return,
obey MAXIMUM-ATTEMPTS, and return a provider-result. Keep each callback object
unchanged until CALLBACK returns. ABORT must be safe before executor start and
after return. It must unblock live transport and confirm actual transport closure
before returning. It runs once on cancellation or normal return. The pinned library
suppresses cleanup errors, so executor return alone is insufficient. Cleanup waits
for both workers. No asynchronous thread termination is used.
If session OUTPUT-CALLBACK is set, NONBLOCKING-OUTPUT must assert that this trusted
callback returns promptly without I/O or waiting. The owner calls it directly."
  (unless (and (hash-table-p profiles) (typep replay-store 'model-replay-store)
               (functionp executor) (functionp abort) (functionp clock)
               (realp timeout) (< 0 timeout 86400) (<= 1 maximum-attempts 1024))
    (error 'parent-protocol-error :reason ':invalid-model-service))
  (make-instance 'model-service :profiles profiles :replay-store replay-store
                 :executor executor :abort abort :clock clock :timeout timeout
                 :maximum-attempts maximum-attempts :nonblocking-output nonblocking-output))

(declaim (ftype (function (model-service model-invocation) boolean) model-invocation-cancelled-p))

(defun model-invocation-cancelled-p (service invocation)
  "Return the cancellation latch under its short mailbox lock."
  (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock (model-service-mailbox service)))
    (not (null (model-invocation-cancel-code invocation)))))

(declaim (ftype (function (model-service keyword &key (:turn-id (or null string))) boolean)
                model-service--latch))

(defun model-service--latch (service code &key turn-id)
  "Latch cancellation independently of data queues, output and storage."
  (let ((mailbox (model-service-mailbox service)))
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (let ((invocation (model-service-active service)))
        (when (and invocation (or (null turn-id) (equal turn-id (model-invocation-turn-id invocation)))
                   (null (model-invocation-cancel-code invocation)))
          (setf (model-invocation-cancel-code invocation) code)
          (bordeaux-threads:condition-notify (model-service-abort-ready service))
          (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))
          (return-from model-service--latch t)))))
  nil)

(declaim (ftype (function (model-service) null) model-service--abort-loop))

(defun model-service--abort-loop (service)
  "Check deadlines and confirm transport closure outside storage and I/O locks."
  (let ((mailbox (model-service-mailbox service)))
    (loop
      (session-runtime--model-expire service (model-service-runtime service))
      (let ((invocation
              (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
                (let ((active (model-service-active service)))
                  (cond
                    ((and active (or (model-invocation-cancel-code active)
                                     (model-invocation-returned active))
                          (not (model-invocation-abort-called active)))
                     (setf (model-invocation-abort-called active) t)
                     active)
                    ((model-runtime-mailbox-closed mailbox)
                     (return-from model-service--abort-loop nil))
                    (t
                     (bordeaux-threads:condition-wait (model-service-abort-ready service)
                                                    (model-runtime-mailbox-lock mailbox) :timeout 0.05)
                     nil))))))
        (when invocation
          (handler-case (funcall (model-service-abort service) invocation)
            (error ()
              (model-service--latch service ':unavailable)
              (model-runtime--close-mailbox mailbox ':provider-abort-failed)
              (session-runtime-disconnect (model-service-runtime service))))
          (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
            (setf (model-invocation-abort-returned invocation) t)
            (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))))))))

(defmethod session-runtime--operation ((service model-service) runtime function)
  "Use the one storage owner. Cleanup runs directly only after all workers joined."
  (declare (ignore runtime))
  (if (model-service-quiesced service)
      (funcall function)
      (model-runtime--call (model-service-mailbox service) function)))

(defmethod session-runtime--send ((service model-service) runtime envelope)
  "Reserve a bounded detached frame. The owner never writes to the peer."
  (model-runtime--enqueue-write (model-service-mailbox service) (session-runtime-connection runtime) envelope))

(defmethod session-runtime--after-send ((service model-service) runtime function)
  "Attach a history update to the last reserved write, not to queue admission."
  (declare (ignore runtime))
  (let ((write (model-runtime-mailbox-last-write (model-service-mailbox service))))
    (unless write (error 'parent-protocol-error :reason ':missing-delivery-receipt))
    (push function (model-runtime-write-actions write))))

(defmethod session-runtime--fence ((service model-service) runtime reason &key turn-id)
  "Stop provider work when the turn or connection loses authority."
  (declare (ignore runtime reason))
  (model-service--latch service ':cancelled :turn-id turn-id)
  nil)

(defmethod session-runtime--model-expire ((service model-service) runtime)
  "Latch the trusted absolute model deadline, even during blocked storage or output."
  (declare (ignore runtime))
  (let ((mailbox (model-service-mailbox service))
        (now     (funcall (model-service-clock service))))
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (let ((active (model-service-active service)))
        (when (and active (null (model-invocation-cancel-code active))
                   (>= now (model-invocation-deadline active)))
          (setf (model-invocation-cancel-code active) ':unavailable)
          (bordeaux-threads:condition-notify (model-service-abort-ready service))
          (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))
          (return-from session-runtime--model-expire t)))))
  nil)

(declaim (ftype (function (model-service model-invocation) null) model-service--reconcile))

(defun model-service--reconcile (service invocation)
  "Reconcile a possible commit. Never replace an unknown outcome with a failure."
  (let ((store (model-service-replay-store service)))
    (multiple-value-bind (status attempt)
        (model-replay-store-status store (model-invocation-request-id invocation))
      (case status
        (:accepted
         (setf (model-invocation-terminal invocation) ':accepted)
         (model-replay-store-project-session store (session-runtime-store (model-service-runtime service))))
        (:terminal-failed
         (setf (model-invocation-terminal invocation) ':failed))
        ((:failed :interrupted)
         (setf (model-invocation-terminal invocation) status))
        (otherwise
         (model-replay-store-interrupt-attempt store :request-id (model-invocation-request-id invocation)
                                                    :attempt     attempt)
         (setf (model-invocation-terminal invocation) ':interrupted)))))
  nil)

(declaim (ftype (function (model-service model-invocation) null) model-service--cancel-owned))

(defun model-service--cancel-owned (service invocation)
  "Commit one fixed terminal failure if cancellation won before acceptance."
  (when (and (model-invocation-cancel-code invocation) (null (model-invocation-terminal invocation)))
    (let ((collector (model-invocation-collector invocation)))
      (handler-case
          (progn
            (model-event-collector-fail collector :code (model-invocation-cancel-code invocation))
            (setf (model-invocation-terminal invocation) ':failed
                  (model-invocation-cancelled-by invocation) (model-invocation-cancel-request-id invocation))
            (setf (gethash (model-invocation-wire-id invocation)
                           (session-runtime-terminal-requests (model-service-runtime service)))
                  (or (model-invocation-cancelled-by invocation) ':failed)))
        (error ()
          (model-service--reconcile service invocation)
          (session-runtime-disconnect (model-service-runtime service))))))
  nil)

(declaim (ftype (function (model-service model-invocation t) t) model-service--event))

(defun model-service--event (service invocation event)
  "Synchronously submit one trusted bounded callback to the owner."
  (when (model-invocation-cancelled-p service invocation)
    (error 'model-service-cancelled))
  (model-runtime--call
   (model-service-mailbox service)
   (lambda ()
     (when (or (model-invocation-terminal invocation) (model-invocation-cancelled-p service invocation))
       (error 'model-service-cancelled))
     (let ((collector (model-invocation-collector invocation)))
       ;; The pinned library announces attempt 1 as a retry event too.
       (if (and (typep event 'model-wire:provider-retry-event)
                (= 1 (model-wire:provider-retry-event-attempt event)))
           (progn
             (unless (and (not (model-invocation-first-retry invocation))
                          (= (model-service-maximum-attempts service)
                             (model-wire:provider-retry-event-maximum-attempts event))
                          (zerop (model-wire:provider-retry-event-delay event)))
               (error 'parent-protocol-error :reason ':invalid-initial-attempt))
             (setf (model-invocation-first-retry invocation) t))
           (model-event-collector-receive collector event))))
   :bytes (* 8 1024 1024)))

(declaim (ftype (function (model-service model-invocation) null) model-service--execute))

(defun model-service--execute (service invocation)
  "Run the injected executor. Publish return through an independent one-entry slot."
  (let ((result nil))
    (unwind-protect
         (handler-case
             (unless (model-invocation-cancelled-p service invocation)
               (setf result (funcall (model-service-executor service) invocation
                                     (lambda (event) (model-service--event service invocation event)))))
           (error () nil))
      (let ((mailbox (model-service-mailbox service)))
        (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
          (setf (model-invocation-result invocation) result
                (model-invocation-returned invocation) t)
          (bordeaux-threads:condition-notify (model-service-abort-ready service))
          (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))))))
  nil)

(declaim (ftype (function (model-service model-invocation) boolean) model-service--begin-acceptance))

(defun model-service--begin-acceptance (service invocation)
  "Order acceptance against cancellation under the short mailbox lock.
Cancellation before this operation prevents acceptance. Cancellation received during
acceptance remains pending until its durable outcome is known. No storage runs here."
  (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock (model-service-mailbox service)))
    (unless (model-invocation-cancel-code invocation)
      (setf (model-invocation-accepting invocation) t)
      (return-from model-service--begin-acceptance t)))
  nil)

(declaim (ftype (function (model-service model-invocation) null) model-service--finish))

(defun model-service--finish (service invocation)
  "Accept only after executor return; a committed acceptance wins later cancellation."
  (let ((collector (model-invocation-collector invocation)))
    (unless (model-invocation-terminal invocation)
      (handler-case
          (cond
            ((typep (model-invocation-result invocation) 'model-wire:provider-result)
             (if (model-service--begin-acceptance service invocation)
                 (progn
                   (model-event-collector-finish collector (model-invocation-result invocation))
                   (setf (model-invocation-terminal invocation) ':accepted))
                 (model-service--cancel-owned service invocation)))
            ((model-invocation-cancelled-p service invocation)
             (model-service--cancel-owned service invocation))
            (t
             (model-event-collector-fail collector)
             (setf (model-invocation-terminal invocation) ':failed)))
        (error ()
          (if (and (model-invocation-cancelled-p service invocation)
                   (member (model-event-collector-phase collector) '(:active :waiting :broken)))
              (model-service--cancel-owned service invocation)
              (progn
                (model-service--reconcile service invocation)
                (session-runtime-disconnect (model-service-runtime service)))))))
    (when (model-invocation-terminal invocation)
      (setf (gethash (model-invocation-wire-id invocation)
                     (session-runtime-terminal-requests (model-service-runtime service)))
            (or (model-invocation-cancelled-by invocation) (model-invocation-terminal invocation)))))
  nil)

(declaim (ftype (function (model-service parent-proto:envelope) null) model-service--start))

(defun model-service--start (service request)
  "Validate one ordinary request and reserve the single provider slot before execution."
  (let* ((runtime (model-service-runtime service))
         (start   (parent-proto:envelope.model-start request))
         (wire-id (parent-proto:envelope.message-id request)))
    (labels ((reject (code)
               (session-runtime--failure runtime request code)
               (setf (gethash wire-id (session-runtime-terminal-requests runtime)) t)))
      (when (session-runtime--history-full-p runtime request :table (session-runtime-terminal-requests runtime)
                                                           :key     wire-id)
        (return-from model-service--start nil))
      (unless (and (eq (session-runtime-phase runtime) ':running)
                   (equal (parent-proto:model-start.turn-id start) (session-runtime-turn-id runtime)))
        (reject ':invalid-request)
        (return-from model-service--start nil))
      (when (model-service-active service)
        (reject ':resource-limit)
        (return-from model-service--start nil))
      (unless (eq (parent-proto:model-start.kind start) ':generate)
        (reject ':unsupported)
        (return-from model-service--start nil))
      (let ((inputs
              (handler-case
                  (multiple-value-list
                   (model-start->wire-request start (model-service-profiles service)
                                             :replay-resolver
                                             (make-model-replay-resolver (model-service-replay-store service)
                                                                        :turn-id (session-runtime-turn-id runtime))))
                (error () nil))))
        (unless inputs
          (reject ':invalid-request)
          (return-from model-service--start nil))
        (let* ((identity   (format nil "~A/~D" (parent-connection-instance-id (session-runtime-connection runtime)) wire-id))
               (key        (concatenate 'string "model/" (ironclad:byte-array-to-hex-string
                                                          (ironclad:digest-sequence ':sha256
                                                            (sb-ext:string-to-octets identity :external-format ':utf-8)))))
               (invocation (make-instance 'model-invocation :inputs inputs :wire-id wire-id :request-id key
                                           :turn-id  (copy-seq (session-runtime-turn-id runtime))
                                           :deadline (+ (funcall (model-service-clock service))
                                                        (model-service-timeout service))))
               (collector
                 (make-model-replay-collector
                  :replay-store (model-service-replay-store service) :session-store (session-runtime-store runtime)
                  :request-id key :turn-id (model-invocation-turn-id invocation) :profile (third inputs)
                   :kind      (parent-proto:model-start.kind start)
                   :purpose   (if (eq (parent-proto:model-start.purpose start) ':ordinary-summary) ':summary ':ordinary)
                  :emit-event (lambda (event)
                                (unless (eq ':closed (session-runtime-phase runtime))
                                  (session-runtime--send service runtime
                                    (parent-proto:make-envelope :reply-to wire-id :model-event event)))))))
          (setf (model-invocation-collector invocation) collector)
          (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock (model-service-mailbox service)))
            (setf (model-service-active service) invocation)
            (when (first (gethash wire-id (model-service-pending-starts service)))
              (setf (model-invocation-cancel-code invocation) ':cancelled
                    (model-invocation-cancel-request-id invocation)
                    (first (gethash wire-id (model-service-pending-starts service))))
              (bordeaux-threads:condition-notify (model-service-abort-ready service))))
          (model-event-collector-begin collector :maximum-attempts (model-service-maximum-attempts service))
          (setf (model-invocation-thread invocation)
                (bordeaux-threads:make-thread (lambda () (model-service--execute service invocation))
                                             :name "parent model executor"))))))
  nil)

(defmethod session-runtime--model-dispatch ((service model-service) runtime request)
  "Handle model starts and cancellation on the one owner."
  (case (parent-proto:envelope.body-case request)
    (parent-proto:model-start
     (model-service--start service request)
     t)
    (parent-proto:cancel-request
     (let* ((target   (parent-proto:cancel-request.request-id (parent-proto:envelope.cancel-request request)))
            (active   (model-service-active service))
            (terminal (gethash target (session-runtime-terminal-requests runtime))))
       (cond
         (terminal
          (session-runtime--send service runtime
            (parent-proto:make-envelope :reply-to  (parent-proto:envelope.message-id request)
                                       :cancel-ack (parent-proto:make-cancel-ack
                                                    :already-terminal
                                                    (not (eql terminal (parent-proto:envelope.message-id request)))))))
         ((and active (= target (model-invocation-wire-id active)))
          (model-service--latch service ':cancelled)
          (model-service--cancel-owned service active)
          (session-runtime--send service runtime
            (parent-proto:make-envelope :reply-to  (parent-proto:envelope.message-id request)
                                       :cancel-ack (parent-proto:make-cancel-ack
                                                    :already-terminal
                                                    (not (eql (model-invocation-cancelled-by active)
                                                              (parent-proto:envelope.message-id request)))))))
         (t
          (session-runtime--failure runtime request ':invalid-request))))
     t)
    (otherwise
     nil)))

(defmethod session-runtime--incoming ((service model-service) runtime request &key bytes)
  "Latch control before bounded admission. Preserve delivery to an active target."
  (let ((mailbox (model-service-mailbox service)))
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (unless (parent-proto:envelope.has-reply-to request)
        (cond
          ((parent-proto:envelope.has-model-start request)
           (when (>= (hash-table-count (model-service-pending-starts service)) 64)
             (error 'parent-protocol-error :reason ':runtime-input-limit))
           (setf (gethash (parent-proto:envelope.message-id request)
                          (model-service-pending-starts service)) (list nil)))
          ((parent-proto:envelope.has-cancel-request request)
           (let* ((target  (parent-proto:cancel-request.request-id
                            (parent-proto:envelope.cancel-request request)))
                  (pending (gethash target (model-service-pending-starts service)))
                  (active  (model-service-active service)))
             (when (and pending (not (first pending)))
                (setf (first pending) (parent-proto:envelope.message-id request)))
             (when (and active (= target (model-invocation-wire-id active))
                        (null (model-invocation-cancel-code active)))
              (setf (model-invocation-cancel-code active) ':cancelled
                    (model-invocation-cancel-request-id active) (parent-proto:envelope.message-id request))
              (bordeaux-threads:condition-notify (model-service-abort-ready service))
              (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))))))))
    (when (or (and (parent-proto:envelope.has-turn-finish request)
                   (parent-proto:envelope.has-reply-to request)
                   (eql (parent-proto:envelope.reply-to request) (session-runtime-turn-request runtime))
                   (equal (parent-proto:turn-finish.turn-id (parent-proto:envelope.turn-finish request))
                          (session-runtime-turn-id runtime)))
              (and (parent-proto:envelope.has-ready-to-stop request)
                   (parent-proto:envelope.has-reply-to request)
                   (eql (parent-proto:envelope.reply-to request) (session-runtime-stop-request runtime))))
      (model-service--latch service ':cancelled))
    (model-runtime--call
     mailbox
     (lambda ()
        (unwind-protect (session-runtime--dispatch runtime request)
          (when (parent-proto:envelope.has-model-start request)
            (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
              (remhash (parent-proto:envelope.message-id request) (model-service-pending-starts service))))))
     :bytes bytes :wait nil :delivery t))
  nil)

(declaim (ftype (function (model-service) null) model-service--owner))

(defun model-service--owner (service)
  "Serialize storage, collector operations and terminal decisions. Never write peer bytes."
  (let* ((mailbox (model-service-mailbox service))
         (runtime (model-service-runtime service)))
    (handler-case
        (loop
          (dolist (action (model-runtime--take-receipts mailbox)) (funcall action))
          (session-runtime--model-expire service runtime)
          (let ((active (model-service-active service)))
            (when active
              (model-service--cancel-owned service active)
              (when (and (model-invocation-returned active)
                         (model-invocation-abort-returned active))
                (model-service--finish service active)
                ;; RETURNED is published only after all executor callbacks have unwound.
                (bordeaux-threads:join-thread (model-invocation-thread active))
                (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
                  (setf (model-service-active service) nil)))))
          (let ((call
                  (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
                    (when (model-runtime-mailbox-closed mailbox)
                      (return-from model-service--owner nil))
                    (let ((first (first (model-runtime-mailbox-calls mailbox))))
                      (if (and first
                               (not (and (model-runtime-call-delivery first)
                                         (model-runtime-mailbox-writes mailbox))))
                          (pop (model-runtime-mailbox-calls mailbox))
                          (progn
                            (bordeaux-threads:condition-wait (model-runtime-mailbox-ready mailbox)
                                                            (model-runtime-mailbox-lock mailbox) :timeout 0.1)
                            nil))))))
            (when call
              (handler-case
                  (setf (model-runtime-call-values call)
                        (multiple-value-list (funcall (model-runtime-call-function call))))
                (error (condition)
                  (setf (model-runtime-call-error call) condition)
                  (unless (model-runtime-call-wait call)
                    (session-runtime-disconnect runtime))))
              (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
                (decf (model-runtime-mailbox-input-bytes mailbox) (model-runtime-call-bytes call))
                (setf (model-runtime-call-done call) t)
                (bordeaux-threads:condition-notify (model-runtime-call-finished call))))))
      (error () (session-runtime-disconnect runtime))))
  nil)

(defmethod session-runtime--attach ((service model-service) runtime)
  "Install the optional service after explicit three-feature selection."
  (unless (and (null (model-service-runtime service))
               (string= (session-store-id (session-runtime-store runtime))
                        (model-replay-store-session (model-service-replay-store service)))
               (or (null (session-runtime-output-callback runtime))
                   (model-service-nonblocking-output service)))
    (error 'parent-protocol-error :reason ':invalid-model-service))
  (model-replay-store-project-session (model-service-replay-store service) (session-runtime-store runtime))
  (setf (model-service-runtime service) runtime)
  (setf (parent-connection-receive-failure (session-runtime-connection runtime))
        (lambda () (session-runtime-disconnect runtime)))
  (let ((mailbox (model-service-mailbox service)))
    (setf (model-service-abort-thread service)
          (bordeaux-threads:make-thread (lambda () (model-service--abort-loop service)) :name "parent model abort")
          (model-service-deadline-thread service)
          (bordeaux-threads:make-thread (lambda () (model-service--deadline-loop service))
                                       :name "parent model deadlines")
          (model-runtime-mailbox-owner mailbox)
          (bordeaux-threads:make-thread (lambda () (model-service--owner service)) :name "parent model owner")
          (model-runtime-mailbox-writer mailbox)
          (bordeaux-threads:make-thread
           (lambda () (model-runtime--writer mailbox (session-runtime-connection runtime)
                                             (lambda () (session-runtime-disconnect runtime))))
           :name "parent model writer")))
  nil)

(declaim (ftype (function (model-service) null) model-service--deadline-loop))

(defun model-service--deadline-loop (service)
  "Enforce deadlines even when abort, storage or the writer cannot return."
  (let ((mailbox (model-service-mailbox service)))
    (handler-case
        (loop
          (session-runtime-expire (model-service-runtime service))
          (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
            (when (model-runtime-mailbox-closed mailbox)
              (return nil))
            (bordeaux-threads:condition-wait (model-service-deadline-ready service)
                                            (model-runtime-mailbox-lock mailbox) :timeout 0.05)))
      (error ()
        (session-runtime-disconnect (model-service-runtime service)))))
  nil)

(defmethod session-runtime--quiesce ((service model-service) runtime)
  "Join executor, abort, owner and writer before session-store interruption.
The public AWAIT-CLOSE timeout reports failure if transport abort or stream close
cannot unblock a worker. A timeout never reports successful cleanup."
  (declare (ignore runtime))
  (model-service--latch service ':cancelled)
  (let* ((mailbox (model-service-mailbox service))
         (active  (model-service-active service)))
    (when (and active (model-invocation-thread active))
      (bordeaux-threads:join-thread (model-invocation-thread active)))
    (model-runtime--close-mailbox mailbox ':closed)
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (bordeaux-threads:condition-notify (model-service-abort-ready service)))
    (dolist (thread (list (model-service-abort-thread service) (model-runtime-mailbox-owner mailbox)
                          (model-service-deadline-thread service)
                          (model-runtime-mailbox-writer mailbox)))
      (when thread (bordeaux-threads:join-thread thread)))
    (when active (model-service--reconcile service active))
    (setf (model-service-quiesced service) t)
    (unless (member (model-runtime-mailbox-error mailbox) '(nil :closed))
      (error 'parent-protocol-error :reason (model-runtime-mailbox-error mailbox))))
  nil)
