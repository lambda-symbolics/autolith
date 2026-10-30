;;;; -- Asynchronous Parent Access Session Adapter --

(in-package #:autolith-supervisor)

(defclass access-session-request ()
  ((id :initarg :id :reader access-session-request-id :documentation "Connection request ID.")
   (turn-id :initarg :turn-id :reader access-session-request-turn-id :documentation "Trusted admission turn.")
   (control :initform (make-access-request-control) :reader access-session-request-control
            :documentation "Cancellation serialized against policy publication.")
   (cancel-replies :initform nil :accessor access-session-request-cancel-replies
                   :documentation "At most 64 pending cancellation acknowledgement IDs.")
   (cancel-worker :initform nil :accessor access-session-request-cancel-worker
                  :documentation "A worker or a short reservation marker."))
  (:documentation "One bounded access approval owned by a runtime."))

(defclass access-session-service ()
  ((policy :initarg :policy :reader access-session-policy :documentation "Trusted durable policy service.")
   (runtime :initform nil :accessor access-session-runtime :documentation "One attached runtime, never reused.")
   (lock :initform (bordeaux-threads:make-lock "access session") :reader access-session-lock
         :documentation "Protect short worker and cancellation bookkeeping only.")
   (requests :initform (make-hash-table :test 'eql) :reader access-session-requests
             :documentation "At most one admitted approval worker.")
   (terminal :initform (make-hash-table :test 'eql) :reader access-session-terminal
             :documentation "Owned terminal request IDs, bounded by runtime history.")
   (workers :initform nil :accessor access-session-workers :documentation "Live approval and cancellation threads.")
   (fenced :initform nil :accessor access-session-fenced-p :documentation "Reject new approvals after lifecycle fencing.")
   (failure :initform nil :accessor access-session-failure :documentation "A fixed asynchronous cleanup failure code.")
   (restart-revision :initform nil :accessor access-session-restart-revision
                     :documentation "Approved revision for the parent's restart controller."))
  (:documentation "Nonblocking runtime hooks around the parent policy service.
Only PENDING_APPROVAL is nonterminal on this connection. An approved pending
restart decision is terminal, with no effective grants. The trusted launcher
must stop the old instance, apply external enforcement, project notes, and put
the complete ACTIVE set in the replacement Welcome. Pause normal terminal input
before permitting approval prompts. No agent Lisp or callback enters the parent."))

(declaim (ftype (function (access-service) (values access-session-service &optional)) make-access-session-service))

(defun make-access-session-service (policy)
  "Make a fresh runtime adapter for POLICY. Use a new adapter after restart."
  (make-instance 'access-session-service :policy policy))

(declaim (ftype (function (access-service session-store) integer) access-service-project-notes))

(defun access-service-project-notes (service store)
  "Project durable parent outcomes idempotently, including after a parent crash.
Call after policy transitions and before constructing a replacement Welcome.
This is parent-only storage work, not an agent append or a new input turn."
  (unless (string= (access-context-session-id (access-service-context service)) (session-store-id store))
    (access-policy--reject ':session-mismatch))
  (let ((notes (access-service-notes service)))
    (dolist (note notes)
      (let* ((kind    (string-downcase (symbol-name (getf note :kind))))
             (outcome (make-hash-table :test 'equal))
             (payload (make-hash-table :test 'equal)))
        (setf (gethash "sequence" outcome) (getf note :sequence)
              (gethash "kind" outcome) kind
              (gethash "operation_id" outcome) (getf note :operation-id)
              (gethash "policy_revision" outcome) (format nil "~D" (getf note :revision))
              (gethash "access_outcome" payload) outcome
              (gethash "text" payload) (format nil "Access policy ~A at revision ~D."
                                               kind (getf note :revision)))
        (session-store-parent-note store :event-id (format nil "access/~D" (getf note :sequence))
                                        :payload   payload)))
    (length notes)))

(defmethod session-access-attach ((service access-session-service) runtime)
  "Validate the binding only. The runtime owner and writer are not running yet."
  (unless (and (null (access-session-runtime service))
               (string= (access-context-session-id (access-service-context (access-session-policy service)))
                        (session-store-id (session-runtime-store runtime))))
    (access-policy--reject ':session-mismatch))
  (let ((owner (access-service-owner (access-session-policy service))))
    (when (and owner (not (string= owner (parent-connection-instance-id (session-runtime-connection runtime)))))
      (access-policy--reject ':instance-mismatch)))
  (setf (access-session-runtime service) runtime)
  nil)

(declaim (ftype (function (access-session-service function) t) access-session--spawn))

(defun access-session--spawn (service function)
  "Start one bounded worker without holding a runtime owner or doing approval I/O."
  (bordeaux-threads:with-lock-held ((access-session-lock service))
    (setf (access-session-workers service)
          (remove-if-not #'sb-thread:thread-alive-p (access-session-workers service)))
    (let ((thread (bordeaux-threads:make-thread function :name "parent access")))
      (push thread (access-session-workers service))
      thread)))

(declaim (ftype (function (keyword) keyword) access-session--failure-code))

(defun access-session--failure-code (code)
  "Map private fixed failures to redacted protocol codes."
  (case code
    ((:cancelled :session-closed :restart-in-progress)
     ':cancelled)
    ((:private-authority :unknown-service)
     ':permission-denied)
    (:unsupported-backend
     ':unsupported)
    ((:operation-conflict :revision-conflict)
     ':revision-conflict)
    ((:stale-binding :storage-failure :corrupt-store :session-mismatch)
     ':unavailable)
    (otherwise
     ':invalid-request)))

(declaim (ftype (function (access-session-service session-runtime integer &key (:reply parent-proto:envelope)) null)
                access-session--finish))

(defun access-session--finish (service runtime id &key reply)
  "Reserve one terminal reply on the owner and retain cancellation history."
  (setf (gethash id (access-session-terminal service)) t
        (gethash id (session-runtime-terminal-requests runtime)) t)
  (bordeaux-threads:with-lock-held ((access-session-lock service))
    (remhash id (access-session-requests service)))
  (when (member (session-runtime-phase runtime) '(:running :stopping))
    (session-runtime--send (session-runtime-service runtime) runtime reply))
  nil)

(declaim (ftype (function (integer keyword) parent-proto:envelope) access-session--failure))

(defun access-session--failure (id code)
  "Build a redacted terminal failure."
  (parent-proto:make-envelope :reply-to id :failure
                              (parent-proto:make-failure :code code :message "Parent rejected access.")))

(declaim (ftype (function (access-session-service session-runtime function) null) access-session--post))

(defun access-session--post (service runtime function)
  "Run a short completion on the owner. Preserve a fixed failure if delivery fails."
  (handler-case
      (with-session-runtime-owner (runtime) (funcall function))
    (error ()
      (setf (access-session-failure service) ':runtime-failure)
      (session-runtime-disconnect runtime)))
  nil)

(declaim (ftype (function (access-session-service session-runtime access-session-request &key (:request t)) null)
                access-session--approve))

(defun access-session--approve (service runtime operation &key request)
  "Approve off-owner, project durable notes, then reserve the terminal reply."
  (let ((decision nil) (failure nil))
    (handler-case
        (setf decision (access-service-handle (access-session-policy service) request
                                             :control (access-session-request-control operation)))
      (access-policy-error (condition)
        (setf failure (access-session--failure-code (access-policy-error-code condition))))
      (error () (setf failure ':unavailable)))
    (handler-case
        (access-service-project-notes (access-session-policy service) (session-runtime-store runtime))
      (error ()
        (setf failure ':unavailable (access-session-failure service) ':note-projection-failed)))
    (access-session--post
     service runtime
     (lambda ()
       (let ((id (access-session-request-id operation)))
         (access-session--finish
           service runtime id :reply
          (if failure (access-session--failure id failure)
              (parent-proto:make-envelope :reply-to id :access-decision decision)))
         (when (and decision (eq (parent-proto:access-decision.state decision) ':approved-pending-restart))
           (setf (access-session-restart-revision service) (parent-proto:access-decision.policy-revision decision))
           (session-runtime--notify runtime ':access-restart-required
                                    :turn-id (access-session-request-turn-id operation) :outcome ':approved-pending-restart))))))
  nil)

(defmethod session-access-dispatch ((service access-session-service) runtime envelope)
  "Validate and admit at most one asynchronous approval. Do not wait on the owner."
  (let* ((id      (parent-proto:envelope.message-id envelope))
         (request (parent-proto:envelope.access-request envelope)))
    (when (session-runtime--history-full-p runtime envelope
                                          :table (session-runtime-terminal-requests runtime) :key id)
      (return-from session-access-dispatch t))
    (let ((failure
            (cond
              ((or (access-session-fenced-p service) (not (eq (session-runtime-phase runtime) ':running)))
               ':cancelled)
              ((or (plusp (hash-table-count (access-session-requests service)))
                   (> (length (cl-protobufs:serialize-to-bytes request)) 1048576))
               ':resource-limit)
              (t
               (handler-case
                   (progn (access-policy--request-data
                           (access-service-context (access-session-policy service)) request) nil)
                 (access-policy-error (condition)
                   (access-session--failure-code (access-policy-error-code condition))))))))
      (when failure
        (access-session--finish service runtime id :reply (access-session--failure id failure))
        (return-from session-access-dispatch t)))
    (let ((operation (make-instance 'access-session-request :id id :turn-id (session-runtime-turn-id runtime))))
      (bordeaux-threads:with-lock-held ((access-session-lock service))
        (setf (gethash id (access-session-requests service)) operation))
      (session-runtime--send
       (session-runtime-service runtime) runtime
       (parent-proto:make-envelope :reply-to id :access-decision
                                   (parent-proto:make-access-decision :state ':pending-approval)))
      (access-session--spawn service (lambda () (access-session--approve service runtime operation :request request)))))
  t)

(declaim (ftype (function (access-session-service session-runtime access-session-request) null)
                access-session--cancel))

(defun access-session--cancel (service runtime operation)
  "Coalesce bounded cancellation work without waiting for a policy commit on-owner."
  (let ((start nil))
    (bordeaux-threads:with-lock-held ((access-session-lock service))
      (unless (access-session-request-cancel-worker operation)
        (setf (access-session-request-cancel-worker operation) t start t)))
    (when start
      (access-session--spawn
       service
       (lambda ()
         (let ((status (access-request-control-cancel (access-session-request-control operation))))
           (access-session--post
            service runtime
            (lambda ()
              (let ((replies nil))
                (bordeaux-threads:with-lock-held ((access-session-lock service))
                  (setf replies (access-session-request-cancel-replies operation)
                        (access-session-request-cancel-replies operation) nil
                        (access-session-request-cancel-worker operation) nil))
                (when (member (session-runtime-phase runtime) '(:running :stopping))
                  (dolist (id (reverse replies))
                    (session-runtime--send
                     (session-runtime-service runtime) runtime
                     (parent-proto:make-envelope :reply-to id :cancel-ack
                      (parent-proto:make-cancel-ack :already-terminal (not (eq status ':cancelled))))))))))))))
   nil))

(defmethod session-access-cancel ((service access-session-service) runtime envelope)
  "Acknowledge only owned access targets. Completion and cancellation share the commit fence."
  (let* ((target    (parent-proto:cancel-request.request-id (parent-proto:envelope.cancel-request envelope)))
         (id        (parent-proto:envelope.message-id envelope))
         (operation (gethash target (access-session-requests service))))
    (cond
      ((gethash target (access-session-terminal service))
       (session-runtime--send (session-runtime-service runtime) runtime
        (parent-proto:make-envelope :reply-to id :cancel-ack (parent-proto:make-cancel-ack :already-terminal t)))
       t)
      (operation
       (let ((full nil))
         (bordeaux-threads:with-lock-held ((access-session-lock service))
           (setf full (>= (length (access-session-request-cancel-replies operation)) 64))
           (unless full (push id (access-session-request-cancel-replies operation))))
         (when full
           (session-runtime--failure runtime envelope ':resource-limit)
           (return-from session-access-cancel t)))
       (access-session--cancel service runtime operation)
       t)
      (t
       nil))))

(defmethod session-access-fence ((service access-session-service) runtime reason &key turn-id)
  "Latch admission fencing and schedule cancellation without I/O or approval waits."
  (declare (ignore reason))
  (let ((operations nil))
    (bordeaux-threads:with-lock-held ((access-session-lock service))
      (unless turn-id (setf (access-session-fenced-p service) t))
      (setf operations (loop for operation being the hash-values of (access-session-requests service)
                             when (or (null turn-id) (equal turn-id (access-session-request-turn-id operation)))
                               collect operation)))
    (dolist (operation operations) (access-session--cancel service runtime operation)))
  nil)

(defmethod session-access-quiesce ((service access-session-service) runtime)
  "Cancel and join bounded work while the runtime owner and writer are available."
  (session-access-fence service runtime ':quiesce)
  (dolist (thread (bordeaux-threads:with-lock-held ((access-session-lock service))
                    (copy-list (access-session-workers service))))
    (when (eq (sb-thread:join-thread thread :timeout 65 :default ':timeout) ':timeout)
      (access-policy--reject ':quiesce-timeout)))
  (when (access-session-failure service) (access-policy--reject (access-session-failure service)))
  nil)
