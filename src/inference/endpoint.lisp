(in-package #:autolith)

;;;; -- Recursive Inference Host Endpoint --

(defparameter *rlm-endpoint-map-maximum-tasks* 64
  "The most tasks one proxied environment rlm-map call may fan out.")

(defclass rlm-endpoint ()
  ((listener
    :initarg :listener
    :reader rlm-endpoint--listener
    :type sb-bsd-sockets:socket
    :documentation "The loopback listener accepting environment requests.")
   (port
    :initarg :port
    :reader rlm-endpoint-port
    :type (integer 1)
    :documentation "The ephemeral loopback port the environment connects to.")
   (token
    :initarg :token
    :reader rlm-endpoint-token
    :type non-empty-string
    :documentation "The capability token required on every request.")
   (provider
    :initarg :provider
    :reader rlm-endpoint--provider
    :type model-provider
    :documentation "The provider serving proxied sub-inferences.")
   (configuration
    :initarg :configuration
    :reader rlm-endpoint--configuration
    :type configuration
    :documentation "The configuration serving proxied sub-inferences.")
   (budget
    :initarg :budget
    :reader rlm-endpoint--budget
    :type rlm-budget
    :documentation "The root budget subtree every proxied call descends.")
   (ledger
    :initarg :ledger
    :initform nil
    :reader rlm-endpoint--ledger
    :type (option function)
    :documentation "An optional function recording one plist per served operation.")
   (lock
    :initform (make-lock "Autolith inference endpoint")
    :reader rlm-endpoint--lock
    :documentation "The lock guarding lifecycle and final-value state.")
   (stopping-p
    :initform nil
    :accessor rlm-endpoint--stopping-p
    :type boolean
    :documentation "True once the endpoint is shutting down.")
   (accept-thread
    :initform nil
    :accessor rlm-endpoint--accept-thread
    :type t
    :documentation "The thread accepting environment connections.")
   (client-threads
    :initform nil
    :accessor rlm-endpoint--client-threads
    :type list
    :documentation "The request handler threads joined at shutdown.")
   (final-value
    :initform nil
    :accessor rlm-endpoint--final-value
    :type t
    :documentation "The value the environment recorded through finish.")
   (final-p
    :initform nil
    :accessor rlm-endpoint--final-p
    :type boolean
    :documentation "True once the environment recorded a final value.")
   (active-operations
    :initform 0
    :accessor rlm-endpoint--active-operations
    :type (integer 0)
    :documentation "The admitted inference operations still running."))
  (:documentation
   "A loopback endpoint proxying environment inference calls to the host."))

(-> rlm-endpoint-final (rlm-endpoint) (values t boolean))
(defun rlm-endpoint-final (endpoint)
  "Return the environment's recorded final value and whether one exists."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (values (rlm-endpoint--final-value endpoint)
            (rlm-endpoint--final-p endpoint))))

(-> rlm-endpoint--admit (rlm-endpoint) null)
(defun rlm-endpoint--admit (endpoint)
  "Atomically admit one inference operation unless the run finished.

Admission and the finished check share one lock, so no operation can
start after a finish commits; operations admitted earlier complete
normally, bounded by the shared budget."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (when (rlm-endpoint--final-p endpoint)
      (error 'rlm-inference-error
             :message "The run already recorded its final value."))
    (incf (rlm-endpoint--active-operations endpoint)))
  nil)

(-> rlm-endpoint--release (rlm-endpoint) null)
(defun rlm-endpoint--release (endpoint)
  "Release one admitted inference operation."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (decf (rlm-endpoint--active-operations endpoint)))
  nil)

(-> rlm-endpoint--call-admitted (rlm-endpoint t function) list)
(defun rlm-endpoint--call-admitted (endpoint admit-p function)
  "Call FUNCTION, holding an admission lease while ADMIT-P."
  (if admit-p
      (progn
        (rlm-endpoint--admit endpoint)
        (unwind-protect
             (funcall function)
          (rlm-endpoint--release endpoint)))
      (funcall function)))

(-> rlm-endpoint--record (rlm-endpoint list) null)
(defun rlm-endpoint--record (endpoint record)
  "Append one served-operation RECORD to ENDPOINT's ledger, when any.

The ledger links every child trace to the root run even when the
environment's Lisp discards the returned trace identifiers, so a run
leaves a machine-readable invocation tree instead of orphaned frames."
  (let ((ledger (rlm-endpoint--ledger endpoint)))
    (when ledger
      (handler-case
          (funcall ledger
                   (append record
                           (list :calls-remaining
                                 (rlm-budget-remaining-calls
                                  (rlm-endpoint--budget endpoint))
                                 :tokens-remaining
                                 (rlm-budget-remaining-tokens
                                  (rlm-endpoint--budget endpoint)))))
        (error (condition)
          (format *error-output*
                  "~&The inference run ledger failed: ~A~%" condition)))))
  nil)

(-> rlm-endpoint--dispatch (rlm-endpoint keyword list) list)
(defun rlm-endpoint--dispatch (endpoint operation arguments)
  "Serve one authenticated environment OPERATION and return its response."
  (let ((provider (rlm-endpoint--provider endpoint))
        (configuration (rlm-endpoint--configuration endpoint))
        (budget (rlm-endpoint--budget endpoint)))
    (rlm-endpoint--call-admitted
     endpoint
     (member operation '(:infer :map))
     (lambda ()
       (ecase operation
         (:infer
          (let ((task (getf arguments ':task)))
            (unless (and (stringp task) (non-empty-string-p task))
           (error 'rlm-inference-error
                  :message "An environment infer call requires task text."))
         (multiple-value-bind (value trace-identifier)
             (infer task
                    :context (getf arguments ':context)
                    :contract (or (getf arguments ':contract) ':text)
                    :budget (rlm-budget-descend budget :task task)
                    :provider provider
                    :configuration configuration)
           (rlm-endpoint--record endpoint
                                 (list :operation :infer
                                       :task task
                                       :child-trace trace-identifier))
           (list :rlm-response :status :ok
                 :value value :trace trace-identifier))))
      (:map
       (let ((tasks (getf arguments ':tasks)))
         (unless (and (listp tasks)
                      (plusp (length tasks))
                      (<= (length tasks) *rlm-endpoint-map-maximum-tasks*))
           (error 'rlm-inference-error
                  :message
                  (format nil "An environment map call fans out 1 to ~D tasks."
                          *rlm-endpoint-map-maximum-tasks*)))
         (let ((results
                 (rlm-map tasks
                          :contract (or (getf arguments ':contract) ':text)
                          :budget (rlm-budget-descend budget :task "rlm-map")
                          :provider provider
                          :configuration configuration
                          :concurrency
                          (let ((requested (getf arguments ':concurrency)))
                            (if (and (integerp requested) (plusp requested))
                                requested
                                *rlm-map-default-concurrency*)))))
           (rlm-endpoint--record
            endpoint
            (list :operation :map
                  :children
                  (loop for result in results
                        collect (append
                                 (list :task (getf result ':task))
                                 (if (getf result ':error)
                                     (list :error (getf result ':error))
                                     (list :child-trace
                                           (getf result ':trace)))))))
           (list :rlm-response :status :ok :value results))))
      (:finish
       (with-lock-held ((rlm-endpoint--lock endpoint))
         ;; The first finish wins, atomically with the admission check, so
         ;; no operation can start after the terminal state commits.
         (when (rlm-endpoint--final-p endpoint)
           (error 'rlm-inference-error
                  :message "The run already recorded its final value."))
         (setf (rlm-endpoint--final-value endpoint) (getf arguments ':value)
               (rlm-endpoint--final-p endpoint) t))
       (rlm-endpoint--record endpoint (list :operation :finish))
       (list :rlm-response :status :ok :value ':finished)))))))

(-> rlm-endpoint--handle-client (rlm-endpoint sb-bsd-sockets:socket) null)
(defun rlm-endpoint--handle-client (endpoint socket)
  "Read, answer, and close one environment client SOCKET."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf stream (localgroup--socket-stream socket))
               (let ((request (localgroup-read-packet stream)))
                 (unless (and (listp request)
                              (eq (first request) ':rlm-request))
                   (error 'rlm-inference-error
                          :message "The environment request is malformed."))
                 (let ((fields (rest request)))
                   (unless (equal (getf fields ':token)
                                  (rlm-endpoint-token endpoint))
                     (error 'rlm-inference-error
                            :message "The environment token is invalid."))
                   (localgroup-write-packet
                    stream
                    (rlm-endpoint--dispatch
                     endpoint
                     (getf fields ':operation)
                     (getf fields ':arguments))))))
           (error (condition)
             (when stream
               (ignore-errors
                 (localgroup-write-packet
                  stream
                  (list :rlm-response :status :error
                        :message (princ-to-string condition)))))))
      (if stream
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> rlm-endpoint--serve (rlm-endpoint) null)
(defun rlm-endpoint--serve (endpoint)
  "Accept environment requests until ENDPOINT stops."
  (loop
    (when (with-lock-held ((rlm-endpoint--lock endpoint))
            (rlm-endpoint--stopping-p endpoint))
      (return))
    (handler-case
        (let ((socket (sb-bsd-sockets:socket-accept
                       (rlm-endpoint--listener endpoint))))
          (if (with-lock-held ((rlm-endpoint--lock endpoint))
                (rlm-endpoint--stopping-p endpoint))
              (ignore-errors (sb-bsd-sockets:socket-close socket))
              (let ((thread
                      (make-thread
                       (lambda ()
                         (rlm-endpoint--handle-client endpoint socket))
                       :name "Autolith inference request")))
                (with-lock-held ((rlm-endpoint--lock endpoint))
                  (push thread (rlm-endpoint--client-threads endpoint))))))
      (error ()
        (return))))
  nil)

(-> rlm-endpoint-start
    (&key (:provider model-provider)
          (:configuration configuration)
          (:budget rlm-budget)
          (:ledger (option function)))
    rlm-endpoint)
(defun rlm-endpoint-start (&key provider configuration budget ledger)
  "Start a loopback endpoint proxying environment calls into BUDGET."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type ':stream
                                 :protocol ':tcp))
        (completed-p nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind
            listener (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
           (sb-bsd-sockets:socket-listen listener 16)
           (multiple-value-bind (address port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore address))
             (let ((endpoint (make-instance 'rlm-endpoint
                                            :listener listener
                                            :port port
                                            :token (localgroup-random-token)
                                            :provider provider
                                            :configuration configuration
                                            :budget budget
                                            :ledger ledger)))
               (setf (rlm-endpoint--accept-thread endpoint)
                     (make-thread (lambda ()
                                    (rlm-endpoint--serve endpoint))
                                  :name "Autolith inference endpoint")
                     completed-p t)
               endpoint)))
      (unless completed-p
        (ignore-errors (sb-bsd-sockets:socket-close listener))))))

(-> rlm-endpoint--wake (rlm-endpoint) null)
(defun rlm-endpoint--wake (endpoint)
  "Nudge ENDPOINT's blocking accept loop with a throwaway connection."
  (handler-case
      (multiple-value-bind (socket stream)
          (localgroup-connect (rlm-endpoint-port endpoint))
        (declare (ignore socket))
        (close stream))
    (error () nil))
  nil)

(-> rlm-endpoint-stop (rlm-endpoint) null)
(defun rlm-endpoint-stop (endpoint)
  "Stop ENDPOINT's listener and wait for its accept loop to end.

The wake connection can miss its connect deadline on a heavily loaded
host, so it retries with a bounded join each time; a loop that still
never wakes is abandoned rather than deadlocking the caller, holding
nothing but a dead listener until process exit."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (setf (rlm-endpoint--stopping-p endpoint) t))
  (let ((thread (rlm-endpoint--accept-thread endpoint)))
    (when thread
      (loop repeat 3
            while (thread-alive-p thread)
            do (rlm-endpoint--wake endpoint)
               (handler-case
                   (sb-ext:with-timeout 10
                     (join-thread thread))
                 (error () nil)))
      (when (thread-alive-p thread)
        (format *error-output*
                "~&The inference endpoint accept loop on port ~D was abandoned.~%"
                (rlm-endpoint-port endpoint)))))
  (ignore-errors
    (sb-bsd-sockets:socket-close (rlm-endpoint--listener endpoint)))
  ;; Handlers finish once their proxied inference returns; the timeout
  ;; abandons only a handler still holding a long provider call, which
  ;; stays budget-bounded and whose response write fails harmlessly.
  (dolist (thread (with-lock-held ((rlm-endpoint--lock endpoint))
                    (copy-list (rlm-endpoint--client-threads endpoint))))
    (when (thread-alive-p thread)
      (handler-case
          (sb-ext:with-timeout 30
            (join-thread thread))
        (error () nil))))
  nil)
