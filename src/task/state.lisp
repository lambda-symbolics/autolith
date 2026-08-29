(in-package #:autolith)

;;;; -- In-Process Task Orchestration --

(defparameter *task-default-maximum-concurrency* 8
  "The default number of child agents that may run concurrently.")

(defparameter *task-maximum-concurrency* 32
  "The largest supported child-agent worker pool.")

(defparameter *task-default-maximum-runtime-milliseconds* 0
  "The default unlimited child runtime; positive overrides enable a deadline.")

(defparameter *task-maximum-batch-size* 16
  "The largest task batch accepted atomically.")

(defparameter *task-maximum-live-jobs* 64
  "The maximum combined queued and running task jobs.")

(defparameter *task-hurry-up-maximum-agents* 2
  "The total child-agent admissions allowed during one hurry-up interval.")

(defparameter *task-terminal-retention-limit* 64
  "The maximum terminal task summaries retained in one session.")

(defparameter *tool-execution-default-maximum-concurrency* 4
  "The default number of asynchronous tool executions that may run concurrently.")

(defparameter *tool-execution-maximum-concurrency* 16
  "The largest supported asynchronous tool-execution worker pool.")

(defparameter *tool-execution-maximum-live-jobs* 64
  "The maximum combined queued and running asynchronous tool executions.")

(defparameter *tool-execution-terminal-retention-limit* 64
  "The maximum terminal tool-execution summaries retained in one session.")

(defparameter *tool-execution-summary-limit* 500
  "The operation-summary characters retained for one tool execution.")

(defparameter *tool-execution-result-limit* 8000
  "The result characters retained for one terminal tool execution.")

(defparameter *tool-execution-blocking-grace-seconds* 10
  "Seconds a default execution may block before handing off its existing job.")

(defparameter *task-shutdown-timeout-seconds* 10
  "The maximum time allowed for task worker shutdown.")

(defparameter *task-default-maximum-depth* 2
  "The default maximum child-agent depth below the primary agent.")

(defparameter *task-inherited-reference-maximum-bytes* 32768
  "The maximum UTF-8 wire bytes inherited by one child as parent reference.")

(defparameter *task-inherited-reference-context-divisor* 4
  "The minimum child-context fraction reserved outside inherited reference.")

(defparameter *task-default-maximum-output-bytes* 500000
  "The default maximum UTF-8 bytes retained from one child result.")

(defparameter *task-default-maximum-output-lines* 5000
  "The default maximum lines retained from one child result.")

(defparameter *task-progress-output-limit* 8000
  "The assistant-text tail retained in a live child progress snapshot.")

(defparameter *task-progress-recent-tool-limit* 8
  "The completed tool names retained in one live child activity trace.")

(defparameter *task-result-preview-limit* 6000
  "The result characters shown inline before referring to an artifact.")

(defparameter *task-identifier-maximum-characters* 64
  "The maximum friendly task identifier fragment retained by the scheduler.")

(defparameter *task-retained-assignment-limit* 1000
  "The assignment characters retained after a task becomes terminal.")

(defparameter *task-retained-output-limit* 2000
  "The result output characters retained after artifact publication.")

(defparameter *task-retained-progress-output-limit* 1000
  "The streamed output characters retained for a terminal job.")

(defparameter *task-retained-structured-output-limit* 2000
  "The readable structured-result characters retained outside its artifact.")

(defparameter *task-retained-usage-limit* 1000
  "The provider-usage characters retained after a task becomes terminal.")

(defparameter *task-tool-content-limit* 16000
  "The maximum provider-visible characters returned by task and job tools.")

(defparameter *task-agent-page-default* 16
  "The default number of task-agent discovery records returned at once.")

(defparameter *task-agent-page-maximum* 32
  "The largest task-agent discovery page accepted by the provider tool.")

(defparameter *task-job-wait-maximum-seconds* 3600
  "The longest blocking wait accepted by job.wait.")

(defparameter *task-job-page-default* 32
  "The default number of job.list records returned at once.")

(defparameter *task-job-page-maximum* 64
  "The largest job.list page accepted by the provider tool.")

(defparameter *task-result-label-maximum-characters* 256
  "The maximum child yield label length accepted and retained.")

(defparameter *task-steering-maximum-items* 32
  "The most accepted child steering messages awaiting durable append.")

(defparameter *task-steering-maximum-characters* 131072
  "The largest individual steering message accepted for a running child.")

(defparameter *task-steering-maximum-total-characters* 262144
  "The largest combined queued and in-flight child steering text.")

(defparameter *task-response-promotion-maximum-items* 32
  "The most steered child responses awaiting one verbal result each.")


(defclass task-completion nil
  ((called-p :initform nil :accessor task-completion-called-p :type
             boolean :documentation
             "True after the child accepted one terminal yield.")
   (status :initform nil :accessor task-completion-status :type
           (option keyword) :documentation
           "The success, failed, or aborted yield status.")
   (text :initform nil :accessor task-completion-text :type
         (option string) :documentation
         "The optional human-readable yield result.")
   (data :initform nil :accessor task-completion-data :type t
         :documentation "The raw validated provider JSON yield value.")
   (data-present-p :initform nil :accessor task-completion-data-present-p
                   :type boolean :documentation
                   "True when the child explicitly supplied yield data, including null.")
   (error :initform nil :accessor task-completion-error :type
          (option string) :documentation
          "The optional child-reported failure text.")
   (label :initform nil :accessor task-completion-label :type
          (option string) :documentation
          "The optional concise result label."))
  (:documentation
   "The explicit terminal protocol state of one child agent."))

(defclass task-progress nil
  ((lock :initform (make-lock "Autolith task progress") :reader
         task-progress-lock :documentation
         "The lock protecting snapshots read by job tools.")
   (status :initform ':queued :accessor task-progress-status :type
           keyword :documentation
           "The queued, running, completed, failed, or aborted state.")
   (current-tool
    :initform nil
    :accessor task-progress-current-tool
    :type (option string)
    :documentation "The tool currently executing in the child.")
    (current-tool-started-at
     :initform nil
     :accessor task-progress-current-tool-started-at
     :type (option integer)
     :documentation "The internal real time at which the current tool began.")
   (recent-tools
    :initform (make-deque :maximum-count *task-progress-recent-tool-limit*)
    :reader task-progress-recent-tools
    :type deque
    :documentation "The completed child tools retained in chronological order.")
   (output-tail
    :initform ""
    :accessor task-progress-output-tail
    :type string
    :documentation "The bounded tail of streamed assistant text.")
   (request-count
    :initform 0
    :accessor task-progress-request-count
    :type (integer 0)
    :documentation "The provider requests started by the child.")
   (usage :initform nil :accessor task-progress-usage :type t
          :documentation "The newest portable provider usage snapshot.")
   (started-at :initform nil :accessor task-progress-started-at :type t
               :documentation "The internal real time at which execution began.")
   (updated-at :initform (get-internal-real-time) :accessor
               task-progress-updated-at :type integer :documentation
               "The internal real time of the newest progress event."))
  (:documentation "A normalized, thread-safe child progress snapshot."))

(defclass task-orchestrator nil
  ((pool
    :initarg :pool
    :reader task-orchestrator-pool
    :type job-pool
    :documentation "The supervised worker pool running this session's children.")
   (execution-pool
    :initarg :execution-pool
    :reader task-orchestrator-execution-pool
    :type job-pool
    :documentation "The supervised worker pool running asynchronous tool calls.")
   (lock
    :initform (make-lock "Autolith task orchestrator")
    :accessor task-orchestrator-lock
    :documentation "The lock protecting naming, ordering, hurry-up, and listeners.")
   (hurry-up-p
    :initarg :hurry-up-p
    :initform nil
    :accessor task-orchestrator-hurry-up-p
    :type boolean
    :documentation "Whether urgent session limits govern new child work.")
   (hurry-up-admission-count
    :initform 0
    :accessor task-orchestrator-hurry-up-admission-count
    :type (integer 0)
    :documentation "The children admitted or reserved during the current hurry-up interval.")
   (maximum-depth
    :initarg :maximum-depth
    :accessor task-orchestrator-maximum-depth
    :type (integer 1)
    :documentation "The maximum child depth below the primary agent.")
   (next-name-index
    :initform 0
    :accessor task-orchestrator-next-name-index
    :type (integer 0)
    :documentation "The source of readable names for children given none.")
   (next-session-order
    :initform 0
    :accessor task-orchestrator-next-session-order
    :type (integer 0)
    :documentation "The source of ordering shared by child and tool jobs.")
   (listeners
    :initform nil
    :accessor task-orchestrator-listeners
    :type list
    :documentation "Callbacks receiving portable task and execution events."))
  (:documentation
   "Session-scoped child and asynchronous tool execution state.

The two CL-JOBPOND pools own queueing, workers, deadlines, shutdown, and job
tables. The orchestrator owns cross-pool ordering, child naming, hurry-up policy,
nesting depth, and lifecycle listeners."))

(defclass session-job (job)
  ((orchestrator
    :initarg :orchestrator
    :reader session-job-orchestrator
    :type task-orchestrator
    :documentation "The session orchestrator owning this job.")
   (execution-identifier
    :initarg :execution-identifier
    :reader session-job-execution-identifier
    :type non-empty-string
    :documentation "The process-independent execution identity.")
   (session-order
    :initarg :session-order
    :initform nil
    :reader session-job-explicit-order
    :type (option (integer 1))
    :documentation "The optional cross-pool admission order.")
   (public-identifier
    :initarg :public-identifier
    :initform nil
    :reader session-job-public-identifier
    :type (option non-empty-string)
    :documentation "The optional identifier exposed instead of the pool identifier.")
   (parent-call-id
    :initarg :parent-call-id
    :initform nil
    :reader session-job-parent-call-id
    :type (option string)
    :documentation "The provider tool call that created this job.")
   (detached-p
    :initarg :detached-p
    :initform t
    :accessor session-job-detached-p
    :type boolean
    :documentation "True when the caller is no longer waiting for this job."))
  (:documentation
   "Common session ownership, identity, ordering, and waiting state for a job."))

(-> task-job--steering-entry-characters (agent-steering-input) (integer 0))
(defun task-job--steering-entry-characters (entry)
  "Return the text character count retained by steering ENTRY."
  (length
   (user-message-input-text
    (agent-steering-input-content entry))))

(defclass task-job (session-job)
  ((definition :initarg :definition :accessor task-job-definition :type
               (option task-agent-definition) :documentation
               "The full child role while this job remains live.")
   (definition-summary
    :initform nil
    :accessor task-job-definition-summary
    :type (option list)
    :documentation "Compact non-instruction role metadata retained at terminal state.")
   (item :initarg :item :accessor task-job-item :type list :documentation
         "The normalized assignment plist.")
   (parent-agent
    :initarg :parent-agent
    :accessor task-job-parent-agent
    :type (option agent)
    :documentation "The parent session while this job remains live.")
   (observability-context
    :initarg :observability-context
    :initform nil
    :accessor task-job-observability-context
    :type (option t)
    :documentation
    "The opaque parent observability context captured when this job was admitted.")
   (inherited-reference-p
    :initarg :inherited-reference-p
    :initform nil
    :accessor task-job-inherited-reference-p
    :type boolean
    :documentation "Whether this job may receive its captured parent reference.")
   (inherited-reference-items
    :initarg :inherited-reference-items
    :initform nil
    :accessor task-job-inherited-reference-items
    :type list
    :documentation "The filtered parent reference messages captured at admission.")
   (command-authorization-function
    :initarg :command-authorization-function
    :initform nil
    :accessor task-job-command-authorization-function
    :type (option function)
    :documentation "The parent capability used to authorize child shell commands.")
   (tool-authorization-function
    :initarg :tool-authorization-function
    :initform nil
    :accessor task-job-tool-authorization-function
    :type (option function)
    :documentation
    "The parent capability used to authorize child external tool calls.")
    (steering-lock
     :initform (make-lock "Autolith task steering")
     :reader task-job-steering-lock
     :documentation
     "The lock serializing steering, response promotion, and terminal claims.")
    (steering-items
     :initform (make-deque :weight-function #'task-job--steering-entry-characters)
     :reader task-job-steering-items
     :type deque
     :documentation "Accepted steering entries waiting for the next safe boundary.")
    (steering-in-flight-items
     :initform (make-deque :weight-function #'task-job--steering-entry-characters)
     :reader task-job-steering-in-flight-items
     :type deque
     :documentation "Steering entries drained but not yet durably acknowledged.")
    (response-promotion-identifiers
     :initform (make-deque)
     :reader task-job-response-promotion-identifiers
     :type deque
     :documentation "FIFO steering identifiers awaiting one durable verbal response.")
    (steering-closed-p
     :initform nil
     :accessor task-job-steering-closed-p
     :type boolean
     :documentation "Whether this child has atomically stopped accepting steering.")
   (progress :initform (make-instance 'task-progress) :reader
             task-job-progress :type task-progress :documentation
             "The normalized progress visible to job inspection."))
  (:documentation
   "One synchronous or detached child-agent execution.

The lifecycle lock, state, publication claim, worker thread, run token, result,
deadline, and timings are inherited from CL-JOBPOND:JOB. Child role, assignment,
parent, and borrowed capabilities are released at terminal state."))

(defclass tool-execution-job (session-job)
  ((tool-name
    :initarg :tool-name
    :reader tool-execution-job-tool-name
    :type non-empty-string
    :documentation "The canonical shell or Lisp tool name being executed.")
    (description
     :initarg :description
     :initform nil
     :reader tool-execution-job-description
     :type (option string)
     :documentation "The optional model-authored purpose shown during execution.")
   (summary
    :initarg :summary
    :accessor tool-execution-job-summary
    :type string
    :documentation "The bounded operation summary retained for inspection.")
   (observability-context
    :initarg :observability-context
    :initform nil
    :accessor tool-execution-job-observability-context
    :type (option t)
    :documentation
    "The opaque observability context captured when this execution was admitted.")
   (operation-function
    :initarg :operation-function
    :accessor tool-execution-job-operation-function
    :type (option function)
    :documentation "The live operation called once and cleared at terminal state."))
  (:documentation "One inspectable asynchronous shell or Lisp tool execution."))

(-> task-job-orchestrator (task-job) task-orchestrator)
(defun task-job-orchestrator (job)
  "Return the session orchestrator owning task JOB."
  (session-job-orchestrator job))

(-> task-job-execution-identifier (task-job) non-empty-string)
(defun task-job-execution-identifier (job)
  "Return task JOB's process-independent execution identity."
  (session-job-execution-identifier job))

(-> task-job-parent-call-id (task-job) (option string))
(defun task-job-parent-call-id (job)
  "Return the task.run call that created task JOB."
  (session-job-parent-call-id job))

(-> task-job-detached-p (task-job) boolean)
(defun task-job-detached-p (job)
  "Return true when the parent is not waiting for task JOB."
  (session-job-detached-p job))

(-> task-job--steering-pending-p (task-job) boolean)
(defun task-job--steering-pending-p (job)
  "Return true when locked JOB has queued or in-flight steering."
  (or (not (deque-empty-p (task-job-steering-items job)))
      (not (deque-empty-p (task-job-steering-in-flight-items job)))))

(-> task-job--terminal-admission-reason-locked
    (task-job)
    (option keyword))
(defun task-job--terminal-admission-reason-locked (job)
  "Return lifecycle reason locked JOB cannot accept a terminal claim, or NIL."
  (cond
    ((not (eq (job-state job) ':running))
     ':not-running)
    ((or (job-cancellation-reason job)
         (cl-jobpond::job--publication-claimed-p job))
     ':closing)
    (t
     nil)))

(-> task-job--claim-normal-completion (task-job) boolean)
(defun task-job--claim-normal-completion (job)
  "Atomically close running JOB for normal completion unless steering is pending."
  (with-lock-held ((cl-jobpond::job--lock job))
    (unless (task-job--terminal-admission-reason-locked job)
      (with-lock-held ((task-job-steering-lock job))
        (cond
          ((task-job-steering-closed-p job)
           t)
          ((task-job--steering-pending-p job)
           nil)
          (t
           (setf (task-job-steering-closed-p job) t)
           t))))))

(-> task-job--call-with-yield-claim
    (task-job function)
    (values boolean keyword))
(defun task-job--call-with-yield-claim (job function)
  "Call FUNCTION under JOB's terminal-yield claim, or return its rejection reason."
  (with-lock-held ((cl-jobpond::job--lock job))
    (let ((reason (task-job--terminal-admission-reason-locked job)))
      (if reason
          (values nil reason)
          (with-lock-held ((task-job-steering-lock job))
            (cond
              ((task-job-steering-closed-p job)
               (values nil ':closed))
              ((task-job--steering-pending-p job)
               (values nil ':steering-pending))
              (t
               (setf (task-job-steering-closed-p job) t)
               (funcall function)
               (values t ':accepted))))))))

(defclass task-child-agent (agent)
  ((definition :initarg :definition :reader task-child-agent-definition
               :type task-agent-definition :documentation
               "The role and policy configuring this child.")
   (identity :initarg :identity :reader task-child-agent-identity :type
             list :documentation "The stable identity of this child.")
   (depth :initarg :depth :reader task-child-agent-depth :type
          (integer 1) :documentation
          "The explicit child depth below the primary agent.")
   (completion :initarg :completion :reader task-child-agent-completion
               :type task-completion :documentation
               "The required terminal yield state.")
   (orchestrator
    :initarg :orchestrator
    :reader task-child-agent-orchestrator
    :type task-orchestrator

    :documentation "The shared session task orchestrator.")
   (job :initarg :job :reader task-child-agent-job :type task-job
        :documentation
        "The lifecycle and progress record for this child."))
  (:documentation
   "A real in-process agent session that must finish through yield.submit."))


(defmethod agent-hurry-up-p ((agent task-child-agent))
  "Return the live hurry-up policy shared by AGENT's task orchestrator."
  (task-orchestrator-hurry-up-p (task-child-agent-orchestrator agent)))

(defvar *task-admission-parent-locked-p* nil
  "True while nested task admission has already checked its parent job.")

(-> task--condition-broadcast (t) null)
(defun task--condition-broadcast (condition-variable)
  "Wake every waiter on CONDITION-VARIABLE through the narrow SBCL adapter."
  #+sbcl
  (sb-thread:condition-broadcast condition-variable)
  #-sbcl
  (condition-notify condition-variable)
  nil)

(defmethod agent-turn-complete-p
    ((agent task-child-agent) (result provider-result))
  "Return true after AGENT atomically yields or claims an unsteered stop."
  (or (task-completion-called-p (task-child-agent-completion agent))
      (and (call-next-method)
           (task-job--claim-normal-completion
            (task-child-agent-job agent)))))

(defmethod agent-turn-completion-details ((agent task-child-agent))
  "Identify whether AGENT completed through its explicit yield protocol."
  (list :yielded-p
        (task-completion-called-p (task-child-agent-completion agent))))

(defclass task-tool-result (tool-result)
  ((details :initarg :details :reader task-tool-result-details
            :reader tool-result-details :type t
            :documentation
            "Portable machine-readable task or job orchestration details."))
  (:documentation
   "A normal tool result carrying structured orchestration metadata."))

(defclass task-orchestrator-tool (tool)
  ((orchestrator
    :initarg :orchestrator
    :reader task-orchestrator-tool-orchestrator
    :type task-orchestrator
    :documentation "The session-scoped scheduler shared by task and job tools."))
  (:documentation "A provider tool backed by one shared task orchestrator."))

(defclass task-run-tool (task-orchestrator-tool) nil
  (:documentation
   "Spawn one child agent or a concurrency-limited batch."))

(defclass task-agents-tool (task-orchestrator-tool) nil
  (:documentation "Discover effective child roles and rejected role files."))

(defclass task-job-tool (task-orchestrator-tool) nil
  (:documentation "Inspect, wait for, or cancel child and tool execution jobs."))

(-> task-run-tool-orchestrator (task-run-tool) task-orchestrator)
(defun task-run-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-agents-tool-orchestrator (task-agents-tool) task-orchestrator)
(defun task-agents-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-job-tool-orchestrator (task-job-tool) task-orchestrator)
(defun task-job-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(defclass task-yield-tool (tool) nil
  (:documentation
   "Submit the required terminal result from a child agent."))

(defmethod tool-execution-policy ((tool task-yield-tool))
  "Execute the terminal child yield without concurrent sibling calls."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-decode-arguments ((tool task-run-tool) source)
  "Decode task.run booleans without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.run"))

(defmethod tool-decode-arguments ((tool task-yield-tool) source)
  "Decode yield values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "yield.submit"))

(defmethod tool-decode-arguments ((tool task-agents-tool) source)
  "Decode task.agents values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.agents"))

(defmethod tool-decode-arguments ((tool task-job-tool) source)
  "Decode job tool values without conflating JSON false and null."
  (task-json-decode source :tool-name (tool-canonical-name tool)))
