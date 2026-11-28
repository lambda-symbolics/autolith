(in-package #:autolith)

;;;; -- Task Scheduling and Publication --

(-> task--retained-prefix (string integer) string)
(defun task--retained-prefix (text limit)
  "Return at most LIMIT leading characters from TEXT."
  (check-type limit (integer 0 *))
  (take limit text))

(-> task--compact-native-value (t integer) t)
(defun task--compact-native-value (value limit)
  "Return native VALUE or a descriptor when its readable form exceeds LIMIT."
  (let ((characters (length (task--write-readable-sexp value))))
    (if (<= characters limit)
        value
        (list :omitted :characters characters))))

(-> session-job--cancelled-ancestor-reason
    (session-job)
    (option keyword))
(defun session-job--cancelled-ancestor-reason (job)
  "Return JOB's inherited cancellation reason, or NIL.

A job admitted just as its parent was cancelled can miss that cancellation's
cross-pool walk. Checking retained ancestry before any work closes that window.
An evicted ancestor counts as live because a normally completed parent is no
reason to stop a detached descendant. An ancestor's :moved cancellation,
caused by its agent moving or parking, is also ignored here."
  (let ((jobs
          (task-orchestrator-list-jobs
           (session-job-orchestrator job))))
    (dolist (identifier (job-owner-identifiers job))
      (when-let ((ancestor
                   (find identifier jobs :key #'job-identifier :test #'string=)))
        (let ((reason (job-cancellation-reason ancestor)))
          (when (and reason (not (eq reason ':moved)))
            (return reason)))))))

(-> task-parent-root-conversation-identifier (agent) non-empty-string)
(defun task-parent-root-conversation-identifier (parent)
  "Return the exact channel owning PARENT's tool jobs."
  (conversation-identifier (agent-conversation parent)))


;;;; -- Asynchronous Tool Execution --

(defmethod job-interrupt-on-cancellation-p ((job tool-execution-job))
  "Cancel shell jobs through the executor's poll so capture cleanup can finish."
  (if (string= (tool-execution-job-tool-name job) "shell.run")
      nil
      (call-next-method)))

(-> tool-execution-job-await-native-cleanup
    (tool-execution-job cl-exec-sandbox:sandbox-process-cleanup-error) null)
(defun tool-execution-job-await-native-cleanup (job condition)
  "Retain JOB as a visible cleanup owner until native process and readers stop.
The parent waits only its own cleanup budget; this execution remains running."
  (with-lock-held ((cl-jobpond::job--lock job))
    (setf (tool-execution-job-cleanup-condition job) condition
          (tool-execution-job-activity job) "native cleanup"
          (tool-execution-job-activity-at job) (get-internal-real-time)))
  (cl-exec-sandbox:sandbox-cleanup-wait
   (cl-exec-sandbox:sandbox-process-cleanup-error-owner condition))
  (with-lock-held ((cl-jobpond::job--lock job))
    (setf (tool-execution-job-cleanup-condition job) nil))
  nil)

(-> tool-execution-job--run (tool-execution-job) tool-result)
(defun tool-execution-job--run (job)
  "Call JOB's operation exactly once after checking task ancestry."
  (job-check-cancellation job)
  (when-let ((reason (session-job--cancelled-ancestor-reason job)))
    (error 'job-aborted
           :message
           (format nil "Execution ~A was ~(~A~) with its parent."
                   (session-job-identifier job) reason)
           :identifier (job-identifier job)
           :reason     reason))
  (let ((operation                    (tool-execution-job-operation-function job))
        (*tool-execution-current-job* job))
    (unless operation
      (error 'tool-error
             :message   "The asynchronous tool operation is no longer available."
             :tool-name (tool-execution-job-tool-name job)))
    (funcall operation)))

(-> tool-execution-job--terminal-record
    (tool-execution-job &key (:state keyword) (:result t) (:report (option string)))
    (values list (option string) keyword))
(defun tool-execution-job--terminal-record (job &key state result report)
  "Return JOB's bounded terminal tool record and release its operation closure."
  (setf (tool-execution-job-operation-function job) nil)
  (let* ((tool-result-p (typep result 'tool-result))
         (successful-p  (and tool-result-p (tool-result-success-p result)))
         (final-state
           (cond
             ((eq state :aborted)
              :aborted)
             ((and (eq state :completed) successful-p)
              :completed)
             (t
              :failed)))
         (status        (case final-state
                          (:completed
                           :success)
                          (:aborted
                           :aborted)
                          (otherwise
                           :failed)))
         (content
           (bounded-string
            (cond
              (tool-result-p
               (tool-result-content result))
              (report
               report)
              ((eq final-state :aborted)
               "The tool execution was aborted.")
              (t
               "The tool execution produced no valid result."))
            :limit *tool-execution-result-limit*))
         (final-report
           (or report
               (and (eq final-state :failed)
                    (not tool-result-p)
                    content)))
         (duration
           (and (job-started-at job)
                (task--milliseconds-between
                 (job-started-at job)
                 (get-internal-real-time)))))
    (values (list :status      status
                  :content     content
                  :error-code  (when tool-result-p (tool-result-error-code result))
                  :denied-p    (and tool-result-p (tool-result-denied-p result))
                  :details     (when tool-result-p
                                 (conversation--bounded-tool-details (tool-result-details result)))
                  :duration-ms duration)
            (and final-report
                 (bounded-string final-report
                                 :limit *tool-execution-result-limit*))
            final-state)))

(-> tool-execution-job-result->tool-result
    (tool-execution-job)
    tool-result)
(defun tool-execution-job-result->tool-result (job)
  "Rebuild JOB's retained outcome using its ordinary result rendering policy."
  (let* ((record     (job-result job))
         (content
           (or (and (listp record) (getf record :content))
               (job-condition-report job)
               "The tool execution has no retained result."))
         (details    (and (listp record) (getf record :details)))
         (error-code (and (listp record) (getf record :error-code)))
         (denied-p   (and (listp record) (getf record :denied-p)))
         (success-p  (and (listp record) (eq (getf record :status) ':success))))
    (if (string= (tool-execution-job-tool-name job) "shell.run")
        (shell-command-result content details success-p :code error-code :denied-p denied-p)
        (if success-p
            (tool-success content :details details)
            (tool-failure content :code error-code :denied-p denied-p :details details)))))

(-> task-orchestrator-start-execution-job
    (task-orchestrator agent
     &key (:tool-name non-empty-string)
       (:description (option string))
       (:summary string)
       (:operation-function function)
       (:detached-p boolean)
       (:completion-policy (member :notify :continue))
       (:parent-call-id (option string))
       (:terminal-result-function function))
    tool-execution-job)
(defun task-orchestrator-start-execution-job
    (orchestrator parent-agent
     &key tool-name description summary operation-function detached-p
       parent-call-id
      (completion-policy ':continue)
      (terminal-result-function
       (lambda (job state result report)
         (tool-execution-job--terminal-record job :state  state
                                                  :result result
                                                  :report report))))
  "Admit one operation for exactly-once supervised execution.

TERMINAL-RESULT-FUNCTION receives the job, terminal state, result, and condition
report, including cancellation before execution. It returns the retained result,
condition report, and terminal state, as TOOL-EXECUTION-JOB--TERMINAL-RECORD does."
  (check-type tool-name non-empty-string)
  (check-type description (option string))
  (check-type summary string)
  (check-type completion-policy (member :notify :continue))
  (check-type operation-function function)
  (let* ((publications *shell-log-publications*)
         (publication  nil)
         (session-order
           (first (task-orchestrator-reserve-session-orders orchestrator 1)))
         (identifier   (format nil "exec:~D" session-order))
         (entry
          (list :function         (lambda (job)
                                    (let ((*shell-log-publications*
                                            (cons (setf publication
                                                        (make-instance 'shell-log-publication
                                                                       :configuration (agent-configuration parent-agent)))
                                                  publications)))
                                      (task-continuity-record-job job parent-agent)
                                      (tool-execution-job--run job)))
                 :terminal-result-function
                 (lambda (job state result report)
                   (let ((completed-p nil))
                     (unwind-protect
                          (multiple-value-bind (record final-report final-state)
                              (funcall terminal-result-function job state result report)
                            (when (and (typep result 'tool-result)
                                       (null (getf record :details)))
                              (setf (getf record :details) (tool-result-details result)))
                            (setf (getf record :details)
                                  (conversation--bounded-tool-details (getf record :details)))
                            (let* ((metadata (shell-log-job-metadata
                                              (agent-configuration parent-agent) job))
                                   (details  (getf record :details))
                                   (properties-p
                                     (and (proper-list-p details)
                                          (evenp (length details))
                                          (every #'keywordp (plist-keys details))))
                                   (logs     (or (and properties-p (getf details :shell-logs))
                                                 (getf metadata :shell-logs))))
                              ;; A final result can be stronger evidence than an unwritten
                              ;; manifest after disk failure. Use storage on abort/no result.
                              (when logs
                                (setf (getf record :shell-logs) logs
                                      (getf record :shell-logs-omitted)
                                      (getf metadata :shell-logs-omitted 0)
                                      (getf record :details)
                                      (if properties-p
                                          (let ((updated (copy-list details)))
                                            (setf (getf updated :shell-logs) logs)
                                            updated)
                                          (list :shell-logs logs :tool-details details)))))
                            (setf (getf record :details)
                                  (conversation--bounded-tool-details (getf record :details)))
                            (task-continuity-record-terminal job parent-agent record
                                                             :state final-state)
                            (setf completed-p t)
                            (values record final-report final-state))
                       (if completed-p
                           (shell-log--finish-publication publication)
                           ; mallet:suppress no-ignore-errors -- preserve failed terminal publication
                           (ignore-errors (shell-log--finish-publication publication))))))
                 :parent-job      cl-jobpond::*current-job*
                 :name            tool-name
                 :root-identifier (task-parent-root-conversation-identifier parent-agent)
                 :initargs
                 (list :orchestrator         orchestrator
                        :mission-context     (mission--agent-context parent-agent)
                       :execution-identifier (task-execution-identifier-generate
                                              (agent-configuration parent-agent)
                                              (task-parent-root-conversation-identifier parent-agent))
                       :session-order        session-order
                       :public-identifier    identifier
                       :completion-owner-conversation
                       (conversation-identifier (agent-conversation parent-agent))
                       :completion-owner-participant
                       (conversation-adopted-participant (agent-conversation parent-agent))
                       :completion-policy    completion-policy
                       :completion-mission-id
                       (task-completion-mission-identifier parent-agent)
                       :parent-call-id       parent-call-id
                       :detached-p           detached-p
                       :tool-name            tool-name
                       :description
                       (and (non-empty-string-p description)
                            (bounded-string
                             description
                             :limit *tool-execution-summary-limit*))
                       :summary
                       (bounded-string summary
                                       :limit *tool-execution-summary-limit*)
                       :operation-function   operation-function))))
    (handler-case
        (let ((job (first (job-pool-submit-batch
                           (task-orchestrator-execution-pool orchestrator)
                           (list entry)))))
          (task-orchestrator-emit
           orchestrator ':tool-execution-lifecycle
           (list :id (session-job-identifier job) :tool tool-name
                 :status ':admitted :parent-tool-call-id parent-call-id))
          job)
      (job-pool-error (condition)
        (error 'task-error
               :message
               (typecase condition
                 (job-pool-capacity-exceeded
                  (format nil
                          "The tool execution runtime admits at most ~D live jobs."
                          *tool-execution-maximum-live-jobs*))
                 (t
                  "The session job runtime is shutting down."))
               :tool-name tool-name)))))
