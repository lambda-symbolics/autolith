(in-package #:autolith)

;;;; -- In-Process Task Orchestration --

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

(defparameter *tool-execution-current-job* nil
  "The supervised tool job executing on this worker, or NIL outside execution.")

(defparameter *tool-execution-blocking-grace-seconds* 10
  "Seconds a default execution may block before handing off its existing job.")

(defparameter *task-shutdown-timeout-seconds* 10
  "The maximum time allowed for task worker shutdown.")

(defparameter *channel-default-maximum-depth* 3
  "The default maximum channel depth below a top-level channel.")

(defparameter *task-inherited-reference-maximum-bytes* 32768
  "The maximum UTF-8 wire bytes inherited by one child as parent reference.")

(defparameter *task-inherited-reference-context-divisor* 4
  "The minimum child-context fraction reserved outside inherited reference.")

(defparameter *task-default-maximum-output-bytes* 500000
  "The default maximum UTF-8 bytes retained from one child result.")

(defparameter *task-default-maximum-output-lines* 5000
  "The default maximum lines retained from one child result.")

(defparameter *task-result-preview-limit* 6000
  "The result characters shown inline before referring to an artifact.")

(defparameter *task-identifier-maximum-characters* 64
  "The maximum friendly task identifier fragment retained by the scheduler.")

(defparameter *task-tool-content-limit* 16000
  "The maximum provider-visible characters returned by task and job tools.")

(defparameter *task-job-page-default* 32
  "The default number of job.list records returned at once.")

(defparameter *task-job-page-maximum* 64
  "The largest job.list page accepted by the provider tool.")

(defclass task-orchestrator nil
  ((execution-pool
    :initarg       :execution-pool
    :reader        task-orchestrator-execution-pool
    :type          job-pool
    :documentation "The supervised worker pool running asynchronous tool calls.")
   (lock
    :initform      (make-lock "Autolith task orchestrator")
    :accessor      task-orchestrator-lock
    :documentation "The lock protecting naming, ordering, hurry-up, and listeners.")
   (artifact-lock
    :initform      (make-lock "Autolith task artifacts")
    :reader        task-orchestrator-artifact-lock
    :documentation "The lock serializing artifact publication and retention cleanup.")
   (closed-p
    :initform      nil
    :accessor      task-orchestrator-closed-p
    :type          boolean
    :documentation "Whether runtime shutdown has begun and ordinary refresh is forbidden.")
   (next-session-order
    :initform      0
    :accessor      task-orchestrator-next-session-order
    :type          (integer 0)
    :documentation "The source of ordering shared by child and tool jobs.")
   (completion-lock
    :initform      (make-lock "Autolith completion services")
    :reader        task-orchestrator-completion-lock
    :documentation "Serializes creation and closure of owner completion services.")
   (completion-services
    :initform      (make-hash-table :test #'equal)
    :reader        task-orchestrator-completion-services
    :documentation "Durable notification subscriptions keyed by owning conversation.")
   (listeners
    :initform      nil
    :accessor      task-orchestrator-listeners
    :type          list
    :documentation "Callbacks receiving portable task and execution events."))
  (:documentation "Channel-local asynchronous tool execution and completion state."))

(defclass session-job (job)
  ((cancellation-observation
    :initform      nil
    :accessor      session-job-cancellation-observation
    :documentation "Autolith cancellation timing and once-only warning, under the orchestrator lock.")
   (orchestrator
    :initarg       :orchestrator
    :reader        session-job-orchestrator
    :type          task-orchestrator
    :documentation "The session orchestrator owning this job.")
   (execution-identifier
    :initarg       :execution-identifier
    :reader        session-job-execution-identifier
    :type          non-empty-string
    :documentation "The process-independent execution identity.")
   (session-order
    :initarg       :session-order
    :initform      nil
    :reader        session-job-explicit-order
    :type          (option (integer 1))
    :documentation "The optional cross-pool admission order.")
   (public-identifier
    :initarg       :public-identifier
    :initform      nil
    :reader        session-job-public-identifier
    :type          (option non-empty-string)
    :documentation "The optional identifier exposed instead of the pool identifier.")
   (parent-call-id
    :initarg       :parent-call-id
    :initform      nil
    :reader        session-job-parent-call-id
    :type          (option string)
    :documentation "The provider tool call that created this job.")
   (detached-p
    :initarg       :detached-p
    :initform      t
    :accessor      session-job-detached-p
    :type          boolean
    :documentation "True when the caller is no longer waiting for this job.")
   (completion-owner-conversation
    :initarg       :completion-owner-conversation
    :initform      nil
    :reader        session-job-completion-owner-conversation
    :type          (option non-empty-string)
    :documentation "The exact parent conversation authorized to receive this outcome.")
   (completion-owner-participant
    :initarg       :completion-owner-participant
    :initform      nil
    :reader        session-job-completion-owner-participant
    :type          (option string)
    :documentation "The agent that started the job in that conversation, as `conversation-adopted-participant' named it then; only that agent continues from its outcome.")
   (completion-policy
    :initarg       :completion-policy
    :initform      ':continue
    :reader        session-job-completion-policy
    :type          (member :notify :continue)
    :documentation "Notify at a safe boundary, or also request an automatic continuation.")
   (completion-mission-id
    :initarg       :completion-mission-id
    :initform      nil
    :reader        session-job-completion-mission-id
    :type          (option non-empty-string)
    :documentation "The admitted mission identity, preventing a completion from reviving another goal."))
  (:documentation
   "Common session ownership, identity, ordering, and waiting state for a job."))

(defclass tool-execution-job (session-job)
  ((tool-name
    :initarg       :tool-name
    :reader        tool-execution-job-tool-name
    :type          non-empty-string
    :documentation "The canonical shell or Lisp tool name being executed.")
   (description
    :initarg       :description
    :initform      nil
    :reader        tool-execution-job-description
    :type          (option string)
    :documentation "The optional model-authored purpose shown during execution.")
   (summary
    :initarg       :summary
    :accessor      tool-execution-job-summary
    :type          string
    :documentation "The bounded operation summary retained for inspection.")
   (activity
    :initform      nil
    :accessor      tool-execution-job-activity
    :type          (option string)
    :documentation "The latest bounded content-free execution phase, under the job lock.")
   (activity-at
    :initform      nil
    :accessor      tool-execution-job-activity-at
    :type          (option integer)
    :documentation "The monotonic time of the latest real phase update, under the job lock.")
   (cleanup-condition
    :initform      nil
    :accessor      tool-execution-job-cleanup-condition
    :documentation "Native cleanup ownership retained until process and readers finish, under the job lock.")
   (operation-function
    :initarg       :operation-function
    :accessor      tool-execution-job-operation-function
    :type          (option function)
    :documentation "The live operation called once and cleared at terminal state."))
  (:documentation "One inspectable asynchronous shell or Lisp tool execution."))

(defclass task-tool-result (tool-result)
  ((details :initarg :details :reader task-tool-result-details
            :reader tool-result-details :type t
            :documentation
            "Portable machine-readable task or job orchestration details."))
  (:documentation
   "A normal tool result carrying structured orchestration metadata."))

(defclass task-orchestrator-tool (tool)
  ((orchestrator
    :initarg       :orchestrator
    :reader        task-orchestrator-tool-orchestrator
    :type          task-orchestrator
    :documentation "The session-scoped scheduler shared by task and job tools."))
  (:documentation "A provider tool backed by one shared task orchestrator."))

(defclass task-run-tool (task-orchestrator-tool) nil
  (:documentation
   "Spawn one child agent or a concurrency-limited batch."))

(defclass task-job-tool (task-orchestrator-tool) nil
  (:documentation "Inspect, wait for, or cancel child and tool execution jobs."))

(-> task-run-tool-orchestrator (task-run-tool) task-orchestrator)
(defun task-run-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-job-tool-orchestrator (task-job-tool) task-orchestrator)
(defun task-job-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(defmethod tool-decode-arguments ((tool task-job-tool) source)
  "Decode job tool values without conflating JSON false and null."
  (task-json-decode source :tool-name (tool-canonical-name tool)))

(defmethod tool-decode-native-arguments ((tool task-job-tool) arguments)
  "Detach job values with their textual decoder's marker policy."
  (task-json-normalize arguments :tool-name (tool-canonical-name tool)))
