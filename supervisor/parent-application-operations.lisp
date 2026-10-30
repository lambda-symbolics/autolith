;;;; -- Typed Parent Application Operations --

(in-package #:autolith-supervisor)

(defclass parent-application-operation-service ()
  ((handlers :initarg :handlers :reader parent-application-operation-service-handlers
             :documentation "Trusted fixed-kind handlers; never agent-selected functions.")
   (lock :initform      (bordeaux-threads:make-lock "parent application operations")
         :reader        parent-application-operation-service-lock
         :documentation "Protect pending workers and cancellation latches.")
   (pending :initform      (make-hash-table :test #'eq)
            :reader        parent-application-operation-service-pending
            :documentation "At most one pending private operation per runtime."))
  (:documentation "Execute private operations outside the storage owner and sole reader."))

(declaim (ftype (function (list) parent-application-operation-service)
                make-parent-application-operation-service))

(defun make-parent-application-operation-service (handlers)
  "Capture explicit trusted KIND/FUNCTION pairs. Each handler gets data and a cancel predicate.
Return a fresh string-keyed hash table with approved public fields only. Never
return credentials, provider tokens, transport objects, or private file contents."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry handlers)
      (unless (and (consp entry) (member (first entry)
                                        '(:auth :model :preferences :permissions :session :restart :mcp :web))
                   (functionp (second entry)) (null (rest (rest entry)))
                   (not (gethash (first entry) table)))
        (error 'parent-application-operation-error :code ':invalid-request))
      (setf (gethash (first entry) table) (second entry)))
    (make-instance 'parent-application-operation-service :handlers table)))

(declaim (ftype (function (parent-application-operation-service session-runtime) boolean)
                parent-application-operation-cancelled-p))

(defun parent-application-operation-cancelled-p (service runtime)
  "Read the pending operation's cancellation latch without waiting for its callback."
  (bordeaux-threads:with-lock-held ((parent-application-operation-service-lock service))
    (let ((pending (gethash runtime (parent-application-operation-service-pending service))))
      (or (null pending) (not (null (second pending)))))))

(declaim (ftype (function (parent-application-operation-service session-runtime) null)
                parent-application-operation-cancel))

(defun parent-application-operation-cancel (service runtime)
  "Latch cancellation before lifecycle or transport waits."
  (bordeaux-threads:with-lock-held ((parent-application-operation-service-lock service))
    (let ((pending (gethash runtime (parent-application-operation-service-pending service))))
      (when pending
        (setf (second pending) t))))
  nil)

(declaim (ftype (function (parent-application-operation-service session-runtime
                           parent-proto:envelope) null)
                parent-application-operation-dispatch))

(defun parent-application-operation-dispatch (service runtime envelope)
  "Admit one bounded fixed operation in an active parent-created durable scope.
Run trusted callbacks on a separate worker so cancellation and stop stay live.
Do not retry a callback after a disconnect or an uncertain result."
  (let* ((request   (parent-proto:envelope.parent-operation envelope))
         (kind      (parent-proto:parent-operation.kind request))
         (handler   (gethash kind (parent-application-operation-service-handlers service)))
         (arguments nil))
    (labels ((reject (code)
               (session-runtime--send
                (session-runtime-service runtime) runtime
                (parent-proto:make-envelope
                 :reply-to (parent-proto:envelope.message-id envelope)
                 :failure  (parent-proto:make-failure :code   code
                                                     :message "Parent application operation refused.")))))
      (handler-case
          (progn
            (unless (and handler
                         (not (parent-proto:envelope.has-reply-to envelope))
                         (equal (parent-proto:parent-operation.turn-id request)
                                (session-runtime-turn-id runtime))
                         (session-runtime-turn-request runtime)
                         (session-application-scope-open-p runtime (parent-proto:parent-operation.operation-id request))
                         (eq (session-runtime-phase runtime) ':running)
                         (<= (length (cl-protobufs:serialize-to-bytes request)) 65536))
              (error 'parent-application-operation-error :code ':invalid-request))
            (setf arguments (protocol-map->data (parent-proto:parent-operation.arguments request)
                                               :max-depth 8 :max-entries 4096))
            (let ((pending (list nil nil nil)))
              (bordeaux-threads:with-lock-held ((parent-application-operation-service-lock service))
                (when (let ((old (gethash runtime (parent-application-operation-service-pending service))))
                        (and old (not (third old))))
                  (error 'parent-application-operation-error :code ':resource-limit))
                (setf (gethash runtime (parent-application-operation-service-pending service)) pending)
                (setf (first pending)
                      (bordeaux-threads:make-thread
                       (lambda ()
                         (unwind-protect
                              (handler-case
                                  (let* ((fields
                                           (funcall handler arguments
                                                    (lambda ()
                                                      (parent-application-operation-cancelled-p
                                                       service runtime))))
                                         (result
                                           (parent-proto:make-parent-operation-result
                                            :fields (protocol-data->map fields :max-depth    8
                                                                                :max-entries 4096))))
                                    (unless (<= (length (cl-protobufs:serialize-to-bytes result))
                                                (if (eq (parent-proto:parent-operation.kind request) ':web)
                                                    (* 4 1024 1024)
                                                    65536))
                                      (error 'parent-application-operation-error :code ':resource-limit))
                                    (with-session-runtime-owner (runtime)
                                      (bordeaux-threads:with-lock-held ((parent-application-operation-service-lock service))
                                        (setf (third pending) t))
                                      (if (parent-application-operation-cancelled-p service runtime)
                                          (reject ':cancelled)
                                          (session-runtime--send
                                           (session-runtime-service runtime) runtime
                                           (parent-proto:make-envelope
                                            :reply-to                (parent-proto:envelope.message-id envelope)
                                            :parent-operation-result result)))))
                                (parent-application-operation-error (condition)
                                  (with-session-runtime-owner (runtime)
                                    (reject (parent-application-operation-error-code condition))))
                                (error ()
                                  (unless (eq (session-runtime-phase runtime) ':closed)
                                    (with-session-runtime-owner (runtime)
                                      (reject ':internal)))))
                           (bordeaux-threads:with-lock-held
                               ((parent-application-operation-service-lock service))
                             (when (eq pending (gethash runtime (parent-application-operation-service-pending service)))
                               (remhash runtime (parent-application-operation-service-pending service))))))
                       :name "parent private application operation")))))
        (parent-application-operation-error (condition)
          (reject (parent-application-operation-error-code condition)))
        (protocol-value-error ()
          (reject ':invalid-request)))))
  nil)

(declaim (ftype (function (parent-application-operation-service session-runtime) null)
                parent-application-operation-join))

(defun parent-application-operation-join (service runtime)
  "Join the cancelled worker before private storage ownership can be reused."
  (parent-application-operation-cancel service runtime)
  (let ((thread
          (bordeaux-threads:with-lock-held ((parent-application-operation-service-lock service))
            (first (gethash runtime (parent-application-operation-service-pending service))))))
    (when thread
      (multiple-value-bind (value failure)
          (sb-thread:join-thread thread :timeout 5 :default ':join-failed)
        (declare (ignore value))
        (when failure
          (error 'parent-application-operation-error :code ':unavailable)))))
  nil)


;;;; -- Session Runtime Boundary --

(defmethod session-runtime--incoming :around ((owner t) runtime envelope &key bytes)
  "Route application scopes and private requests outside the sole reader."
  (declare (ignore bytes))
  (cond
    ((eq (parent-proto:envelope.body-case envelope) 'parent-proto:parent-operation)
     (with-session-runtime-owner (runtime)
       (if (and (session-runtime-application-service runtime)
                (member "application.v1" (session-runtime-features runtime) :test #'equal))
           (parent-application-operation-dispatch (session-runtime-application-service runtime) runtime envelope)
           (session-runtime--failure runtime envelope ':unsupported))))
    ((session-application-handle-incoming runtime envelope))
    (t
     (call-next-method))))(defmethod session-runtime--fence :around ((owner t) runtime reason &key turn-id)
                           "Cancel private callbacks before waiting for any storage or model work."
                           (declare (ignore reason turn-id))
                           (when (session-runtime-application-service runtime)
                             (parent-application-operation-cancel (session-runtime-application-service runtime) runtime))
                           (call-next-method))

(defmethod session-runtime--quiesce :around ((owner t) runtime)
  "Join private callbacks while the storage owner can still send their terminal result."
  (when (session-runtime-application-service runtime)
    (parent-application-operation-join (session-runtime-application-service runtime) runtime))
  (call-next-method))
