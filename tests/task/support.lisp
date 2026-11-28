(in-package #:autolith)

;;;; -- In-Process Task Orchestration Tests --

(defclass task-test-abort-tool (tool)
  ()
  (:documentation "Signal the task cancellation control condition on execution."))

(defclass task-test-blocking-tool (tool)
  ((lock
    :initform      (make-lock "Autolith blocking tool test")
    :reader        task-test-blocking-tool-lock
    :documentation "The lock protecting the test barrier.")
   (condition-variable
    :initform      (make-condition-variable)
    :reader        task-test-blocking-tool-condition-variable
    :documentation "The condition coordinating tool entry and release.")
   (started-p
    :initform      nil
    :accessor      task-test-blocking-tool-started-p
    :type          boolean
    :documentation "True after a child enters the ordinary tool call.")
   (released-p
    :initform      nil
    :accessor      task-test-blocking-tool-released-p
    :type          boolean
    :documentation "True when the test permits normal tool completion."))
  (:documentation "Block an ordinary tool call at a test barrier."))

(defmethod tool-execute
    ((tool task-test-blocking-tool)
     (context tool-context)
     (arguments hash-table))
  "Wait at a test barrier until cancellation unwinds or the test releases it."
  (declare (ignore context arguments))
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-started-p tool) t)
    (sb-thread:condition-broadcast
     (task-test-blocking-tool-condition-variable tool))
    (loop until (task-test-blocking-tool-released-p tool)
          do (condition-wait
              (task-test-blocking-tool-condition-variable tool)
              (task-test-blocking-tool-lock tool))))
  (tool-success "blocking tool released"))

(defclass task-test-provider (model-provider) ()
  (:documentation "Return one deterministic completion for test agents."))

(defmethod provider-stream-turn
    ((provider task-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return a deterministic final response without external I/O."
  (declare (ignore provider conversation tool-namespaces event-callback goal-context compaction-p))
  (agent-test-result "task-test" (list (agent-test-message "done"))))

(defmethod tool-execute
    ((tool task-test-abort-tool)
     (context tool-context)
     (arguments hash-table))
  "Signal a deliberate task abort through ordinary registry dispatch."
  (declare (ignore tool context arguments))
  (error 'job-aborted
         :message "Task test was cancelled."
         :reason  ':test-cancel))

(-> task-tests--wait-until (function (real 0)) boolean)
(defun task-tests--wait-until (predicate timeout-seconds)
  "Wait up to TIMEOUT-SECONDS for PREDICATE to become true."
  (let ((deadline
          (+ (get-internal-real-time)
             (* timeout-seconds internal-time-units-per-second))))
    (loop
      when (funcall predicate)
        return t
      when (>= (get-internal-real-time) deadline)
        return nil
      do (sleep 0.001))))

(-> task-tests--primary-agent
    (configuration string &optional tool-registry)
    agent)
(defun task-tests--primary-agent
    (configuration identifier &optional (registry (make-instance 'tool-registry)))
  "Return a primary test agent with conversation IDENTIFIER."
  (agent-create
   :configuration configuration
   :provider      (make-instance 'model-provider)
   :conversation  (conversation-create configuration :identifier identifier)
   :tool-registry registry
   :worker        nil))

(defvar *task-tests--orchestrators* nil
                                                                   "Orchestrators created by tests, closed when the task suite finishes.")

(defvar *task-tests--cleanup-failures* nil
  "Process-global (orchestrator . outcome) records after incomplete test shutdown.
Access the global value so dynamic fixture bindings cannot conceal unsafe cleanup.
A later successful close does not prove that earlier timed-out threads stopped.")

(define-condition test-cleanup-error (error)
  ((failures
    :initarg       :failures
    :reader        test-cleanup-error-failures
    :documentation "Orchestrators paired with NIL close results or original errors."))
  (:report (lambda (condition stream)
             (format stream "Incomplete test shutdown; restart the test process. Failures: ~S"
                     (test-cleanup-error-failures condition))))
  (:documentation "Incomplete asynchronous cleanup quarantines this test process."))

(-> task-tests--require-cleanup () null)
(defun task-tests--require-cleanup ()
  "Refuse another case after any incomplete shutdown in this process."
  (let ((failures (sb-ext:symbol-global-value '*task-tests--cleanup-failures*)))
    (when failures
      (error 'test-cleanup-error :failures failures))))

(-> task-tests--orchestrator () task-orchestrator)
(defun task-tests--orchestrator ()
  "Return a fresh orchestrator the task suite will close afterwards.

A pool starts its workers on first admission, so a test that admits a job and then
walks away leaks worker threads into every later test. Nothing else notices, but
the tests that require a single-threaded image do."
  (let ((orchestrator (task-orchestrator-create)))
    (push orchestrator *task-tests--orchestrators*)
    orchestrator))

(-> task-tests--close-orchestrators () boolean)
(defun task-tests--close-orchestrators ()
  "Close new handles without signaling; return true only for safe process cleanup.
Retain failed handles and their NIL result or original error globally. Never retry
failed handles: later close success cannot establish that old threads stopped."
  (let ((retained nil))
    (dolist (orchestrator (remove-duplicates *task-tests--orchestrators* :test #'eq))
      (cond
        ((assoc orchestrator
                (sb-ext:symbol-global-value '*task-tests--cleanup-failures*))
         (push orchestrator retained))
        (t
         (let ((outcome (handler-case (task-orchestrator-close orchestrator)
                          (error (condition) condition))))
           (when (or (null outcome) (typep outcome 'error))
             (push orchestrator retained)
             (push (cons orchestrator outcome)
                   (sb-ext:symbol-global-value '*task-tests--cleanup-failures*)))))))
    (setf *task-tests--orchestrators* (nreverse retained))
    (null (sb-ext:symbol-global-value '*task-tests--cleanup-failures*))))
