(in-package #:autolith)

;;;; -- Task Child Execution --

(-> task-conversation-depth (configuration conversation) (integer 0))
(defun task-conversation-depth (configuration conversation)
  "Count registered channel ancestors and explicit unregistered parent links."
  (if (conversation-inbox-p conversation)
      0
      (let* ((world   (world-load configuration))
             (channel (world-channel-find world (conversation-identifier conversation)))
             (seen    (make-hash-table :test #'equal)))
        (loop for parent = (if channel
                               (channel-parent channel)
                               (conversation-parent-identifier conversation)) then (and
                                                                                    channel
                                                                                    (channel-parent
                                                                                     channel))
              while (and parent (not (gethash parent seen)))
              count t into depth
              do (setf (gethash parent seen) t
                       channel (world-channel-find world parent))
              finally (return (max depth 0))))))

(-> task-parent-depth ((option agent)) (integer 0))
(defun task-parent-depth (agent)
  "Return the current task or channel depth of AGENT."
  (typecase agent
    (task-child-agent
     (task-child-agent-depth agent))
    (agent
     (task-conversation-depth (agent-configuration agent) (agent-conversation agent)))
    (t
     0)))

(-> task-child-tool-registry
    (tool-registry &key (:orchestrator (option task-orchestrator)))
    tool-registry)
(defun task-child-tool-registry (parent-registry &key orchestrator)
  "Copy the configured tools and borrow the shared runtimes of PARENT-REGISTRY."
  (let* ((registry
          (make-instance 'tool-registry :resource-registry
                         (tool-registry-resource-registry parent-registry)))
         (manager (mcp-tool-registry-manager parent-registry)))
   (maphash
    (lambda (key binding) (unless (eq key :mcp) (tool-registry-bind-runtime registry key binding)))
    (tool-registry-runtime-bindings parent-registry))
   (tool-registry-bind-runtime registry 'task-orchestrator orchestrator)
   (maphash
    (lambda (namespace description)
      (tool-registry-describe-namespace registry namespace description))
    (tool-registry-namespace-descriptions parent-registry))
   (dolist (tool (tool-registry-tools parent-registry))
     (unless (typep tool 'mcp-provider-tool) (tool-registry-register registry tool)))
   (when manager
     (mcp-tool-registry-bind-manager registry manager)
     (mcp-tool-registry-refresh registry :only-dirty-p t)
     (mcp-tool-registry-activate-discovered-tools registry))
   registry))

(defparameter *task-default-reasoning-effort*
  "medium"
  "Default reasoning effort for delegated child assignments.")

(-> task-child-configuration (configuration list) configuration)
(defun task-child-configuration (parent-configuration item)
  "Copy PARENT-CONFIGURATION with the model and effort selected by ITEM."
  (let ((model  (or (find-if (lambda (candidate)
                               (member candidate *supported-models* :test #'string=))
                             (getf item :models))
                    (config :model parent-configuration)))
        (effort (or (getf item :reasoning-effort) *task-default-reasoning-effort*)))

    (configuration-copy parent-configuration
                        :model model :reasoning-effort effort :codex-fast-mode-p nil)))

(-> task-child-reference-history-p (agent configuration) boolean)
(defun task-child-reference-history-p (parent child-configuration)
  "Return true when PARENT and CHILD-CONFIGURATION's provider both opt in."
  (and (provider-child-reference-history-p (agent-provider parent))
       (provider-child-reference-history-p
        (provider-with-configuration
         (agent-provider parent) child-configuration))))

(-> task-child-reference-history-byte-limit
    (configuration)
    (option integer))
(defun task-child-reference-history-byte-limit (configuration)
  "Return a conservative inherited-reference wire budget, or NIL when too small.

UTF-8 bytes are an upper bound on tokenizer tokens, so this leaves at least
three quarters of the child context for instructions, tools, and new work while
also applying a fixed retention cap. Inheritance is disabled when its mandatory
boundary cannot fit within that budget."
  (let ((limit
          (min *task-inherited-reference-maximum-bytes*
               (max 1
                    (floor (config :context-window configuration)
                           *task-inherited-reference-context-divisor*)))))
    (and (>= limit
             (conversation--inherited-reference-wire-byte-length nil))
         limit)))

(-> task-child-goal-context (task-job configuration) string)
(defun task-child-goal-context (job child-configuration)
  "Build child instructions without a terminal output contract."
  (let* ((item (task-job-item job))
         (context
          (if (getf item :worktree)
              (format nil
                      "~@[~A~2%~]This is an isolated modifying task at baseline ~A. Edit and test in ~A. Return your findings through the ordinary child result; the parent will explicitly inspect and integrate your artifact. Do not modify or integrate into source workspace ~A. Stage new files before finishing so the complete patch can be extracted."
                      (getf item :context) (getf (getf item :worktree) :baseline)
                      (getf (getf item :worktree) :path)
                      (getf (getf item :worktree) :source))
              (getf item :context))))
    (format nil
            "You are child agent ~A, depth ~D.~@[~2%Shared parent context:~%~A~]~2%Your initial workspace is ~A. Work within the assignment's scope. Paths outside the workspace use ordinary tool path authorization. Complete the assignment in the user message and finish the turn normally. Messages on your channel refine your current assignment unless they explicitly replace or cancel it. Between assignments, answer your parent through channel.send to @parent."
            (getf (task-job-identity job) :id)
            (1+ (task-parent-depth (task-job-parent-agent job))) context
            (namestring (config :working-directory child-configuration)))))

(-> task--bounded-output ((option string)) string)
(defun task--bounded-output (text)
  "Bound TEXT by configured UTF-8 bytes and lines, marking truncation."
  (let* ((maximum-bytes
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_BYTES"
                                     *task-default-maximum-output-bytes*
                                     :minimum 1))
         (maximum-lines
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_LINES"
                                     *task-default-maximum-output-lines*
                                     :minimum 1))
         (lines          (task--split-lines (or text "")))
         (line-bounded-p (> (length lines) maximum-lines))
         (line-text
          (format nil "~{~A~^~%~}"
                  (subseq lines 0 (min maximum-lines (length lines)))))
         (prefix-end     (utf8-string-prefix-end line-text maximum-bytes))
         (byte-bounded-p (< prefix-end (length line-text)))
         (bounded        (if byte-bounded-p
                             (subseq line-text 0 prefix-end)
                             line-text)))
    (if (or line-bounded-p byte-bounded-p)
        (format nil "~A~%... [task output truncated]" bounded)
        bounded)))

(-> task--child-prompt-cache-key (non-empty-string) non-empty-string)
(defun task--child-prompt-cache-key (root-conversation-identifier)
  "Return the prompt-cache key shared by every task child below one root.

A child never extends the root's prompt, so it must not route its traffic onto
the root's cache key. Siblings with matching prefixes still share one key."
  (format nil "task:~A" root-conversation-identifier))

(-> task--artifact-root (configuration task-job) pathname)
(defun task--artifact-root (configuration job)
  "Return JOB's private transcript and artifact directory."
  (merge-pathnames
   (format nil "tasks/~A/~A/"
           (or (conversation-identifier-path-fragment
                (task-job-root-conversation-identifier job))
               (task--identifier-fragment
                (task-job-root-conversation-identifier job))
               "conversation")
           (task-job-execution-identifier job))
   (config :data-root configuration)))

(defparameter *task-result-maximum-octets* (* 16 1024 1024)
  "The largest durable child result accepted by task inspection.")

(-> task--result-grammar () source-grammar)
(defun task--result-grammar ()
  "Return the bounded portable data dialect used by durable child results."
  (make-source-grammar
   :label                                     "Child result artifact"
   :maximum-depth 128 :maximum-nodes 262144
   :maximum-string-characters                 *task-result-maximum-octets*
   :qualified-common-lisp-symbols-permitted-p t
   :readable-strings-permitted-p              t
   :read-default-float-format                 'single-float
   :allowed-atom-predicate
   (lambda (value)
     (or (null value) (eq value t) (keywordp value) (stringp value)
         (task-output--json-number-p value)))))

(-> task--result-fields () list)
(defun task--result-fields ()
  "Describe the terminal child result fields consumed by job inspection."
  (append
   (list (list :indicator ':status :required t
               :validate (lambda (value)
                           (not (null (member value '(:success :failed :aborted))))))
         (list :indicator ':request-count
               :validate  (lambda (value) (typep value '(integer 0))))
         (list :indicator ':duration-ms
               :validate  (lambda (value) (typep value '(integer 0))))
         (list :indicator ':usage
               :validate  (lambda (value)
                            (or (null value) (task-progress--usage-object-p value)))))
   (mapcar (lambda (key)
             (list :indicator key
                   :validate  (lambda (value) (or (null value) (stringp value)))))
           '(:id :name :assignment :error :label :model :conversation-file))
   (mapcar (lambda (key)
             (list :indicator key :validate (lambda (value) (typep value 'boolean))))

           '(:detached))))

(-> task--read-result-artifact
    ((or pathname string) &key (:maximum-octets (integer 1))) list)
(defun task--read-result-artifact
    (pathname &key (maximum-octets *task-result-maximum-octets*))
  "Read exactly one bounded, schema-checked durable child result."
  (sexp-store:snapshot-read-record
   pathname :grammar (task--result-grammar) :maximum-octets maximum-octets
   :properties-p t :keyword-keys-p t :maximum-length 128
   :fields (task--result-fields)))

(-> task--write-result-artifact (task-job list) pathname)
(defun task--write-result-artifact (job result)
  "Publish portable RESULT once without running artifact retention."
  (let* ((configuration (agent-configuration (task-job-parent-agent job)))
         (root          (task--artifact-root configuration job))
         (target        (merge-pathnames "result.sexp" root))
         (temporary
           (merge-pathnames
            (make-pathname :name
                           (format nil ".result.~A" (make-identifier))
                           :type "tmp")
            root))
         (orchestrator  (session-job-orchestrator job)))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (ensure-directories-exist target)
      (when (probe-file target)
        (error 'task-error
               :message
               (format nil "Task artifact pathname is already occupied: ~A"
                       target)
               :tool-name "child.spawn"
               :task-id   (job-identifier job)))
      (unwind-protect
           (progn
             (with-open-file
                 (stream temporary
                         :direction         ':output
                         :if-exists         ':supersede
                         :if-does-not-exist ':create
                         :external-format   ':utf-8)
               (with-standard-io-syntax
                 (let ((*print-readably* t)
                       (*print-pretty*   t)
                       (*print-circle*   t))
                   (prin1 (task--character-strings result) stream)
                   (terpri stream)
                   (finish-output stream))))
             (task--read-result-artifact temporary)
             (when (probe-file target)
               (error 'task-error
                      :message
                      (format nil "Task artifact pathname became occupied: ~A"
                              target)
                      :tool-name "child.spawn"
                      :task-id   (job-identifier job)))
             (rename-file temporary target)
             target)
        (when (probe-file temporary)
          (delete-file temporary))))))

(-> task--result-output (task-progress provider-result) string)
(defun task--result-output (progress result)
  "Select bounded text from the final provider response or streamed output."
  (let* ((answer (provider-result-assistant-text result))
         (text   (if (non-empty-string-p answer)
                     answer
                     (with-lock-held ((task-progress-lock progress))
                       (copy-seq (task-progress-output-tail progress))))))
    (cond ((non-empty-string-p text)
           (task--bounded-output text))
          ((plusp (task-progress-request-count progress))
           (format nil "(no output) after ~D req"
                   (task-progress-request-count progress)))
          (t
           "(no output)"))))

(-> task--execution-metadata (task-job &key (:configuration (option configuration))) list)
(defun task--execution-metadata (job &key configuration)
  "Capture portable assignment identity and attempted model before execution."
  (or (task-job-execution-metadata job)
      (let* ((parent        (task-job-parent-agent job))
             (configuration (or configuration
                                (and parent (task-child-configuration
                                             (agent-configuration parent) (task-job-item job))))))
        (setf (task-job-execution-metadata job)
              (list :id (job-identifier job) :name (task-job-display-name job)
                    :assignment (getf (task-job-item job) :task)
                    :model      (and configuration (config :model configuration))
                    :detached   (task-job-detached-p job))))))

(-> task--terminal-result (task-job list) list)
(defun task--terminal-result (job outcome)
  "Combine captured execution metadata, final accounting, and OUTCOME fields."
  (let ((progress (task-job-progress job)))
    (append (copy-list (task--execution-metadata job))
            (list :request-count (task-progress-request-count progress)
                  :usage         (task-progress--usage-summary (task-progress-usage progress))
                  :duration-ms
                  (task--milliseconds-between
                   (or (job-started-at job) (job-created-at job))
                   (get-internal-real-time)))
            outcome)))

(-> task--assemble-child-result
    (task-job &key (:provider-result provider-result)
                   (:child agent)
                   (:conversation conversation))
    list)
(defun task--assemble-child-result
    (job &key provider-result child conversation)
  "Assemble the ordinary child outcome with shared execution metadata."
  (task--execution-metadata job :configuration (agent-configuration child))
  (setf (getf (task-job-execution-metadata job) :conversation-file)
        (namestring (conversation-pathname conversation)))
  (task--terminal-result
   job
   (list :status (if (agent-turn-complete-p child provider-result)
                     ':success
                     ':failed)
         :output (task--result-output (task-job-progress job) provider-result))))

(-> task--failed-result (task-job keyword (option string)) list)
(defun task--failed-result (job status message)
  "Build JOB's failure or cancellation outcome using the attempted child model."
  (let* ((progress (task-job-progress job))
         (tail     (with-lock-held ((task-progress-lock progress))
                     (copy-seq (task-progress-output-tail progress)))))
    (task--terminal-result
     job
     (list :status status
           :output (if (non-empty-string-p tail)
                       (task--bounded-output tail)
                       "(no output)")
           :error  message
           :label  nil))))

(-> task-child-inherit-reference-history
    (task-job conversation model-provider)
    (option list))
(defun task-child-inherit-reference-history (job conversation provider)
  "Append JOB's spawn-time parent reference history when PROVIDER supports it."
  (let ((parent (task-job-parent-agent job)))
    (when (and (task-job-inherited-reference-p job)
               (provider-child-reference-history-p (agent-provider parent))
               (provider-child-reference-history-p provider))
      (conversation-append-inherited-reference
       conversation
       (session-job-completion-owner-conversation job)
       (task-job-inherited-reference-items job)))))

(-> task-job-note-agent-status (task-job keyword list) null)
(defun task-job-note-agent-status (job status details)
  "Project one child observer STATUS into progress or durable response delivery."
  (if (eq status ':assistant-response-persisted)
      (let ((text      (getf details :text))
            (timestamp (getf details :time)))
        (when (and (stringp text) (typep timestamp 'timestamp))
          (task-job-note-verbal-response job text timestamp)))
      (task-progress-note-status job status details))
  nil)

(defparameter *task-descendant-cleanup-timeout-seconds* 2
  "Overall wait for tool descendants before retaining them as live cleanup owners.")

(-> task-job-cancel-execution-descendants (task-job keyword) list)
(defun task-job-cancel-execution-descendants (job reason)
  "Cancel tool descendants and wait one bounded interval for their cleanup.
Live executions retain their pool ownership and remain inspectable after return."
  (let* ((executions
           (job-pool-descendant-jobs
            (task-orchestrator-execution-pool (task-job-orchestrator job))
            (job-identifier job)))
         (deadline (+ (get-internal-real-time)
                      (* *task-descendant-cleanup-timeout-seconds*
                         internal-time-units-per-second))))
    (dolist (execution executions)
      (job-cancel execution :reason reason :cascade-p t))
    (dolist (execution executions)
      (job-await execution
                 :timeout-seconds
                 (max 0 (/ (- deadline (get-internal-real-time))
                           internal-time-units-per-second))))
    executions))

(-> task-assignment-origin (agent) list)
(defun task-assignment-origin (parent)
  "Snapshot PARENT's sender identity, name and source channel before an assignment can wait."
  (let* ((source        (agent-conversation parent))
         (configuration (agent-configuration parent))
         (sender        (or (world-register-conversation configuration source)
                            (participant-state-identity (agent-participant-state parent)))))
    (list :sender      (and sender (participant-identifier sender))
          :sender-name (if sender
                           (participant-name sender)
                           (or (conversation-agent-name source) "autolith"))
          :source      (conversation-identifier source))))

(-> task-child-assignment-request (task-job conversation) list)
(defun task-child-assignment-request (job conversation)
  "Return JOB's admitted assignment provenance, independent of its parent's current residence."
  (list :operation "child.spawn"
        :summary   (getf (task-job-item job) :task)
        :message   (append (or (task-job-assignment-origin job)
                               (task-assignment-origin (task-job-parent-agent job)))
                           (list :target (conversation-identifier conversation)
                                 :kind   ':post))))

(-> task-run-child (task-job) list)
(defun task-run-child (job)
  "Create and run JOB's real in-process child session through a terminal child turn."
  (let* ((parent       (task-job-parent-agent job))
         (item         (task-job-item job))
         (orchestrator (task-job-orchestrator job))
         (depth        (1+ (task-parent-depth parent)))
         (configuration
          (task-worktree-configuration job
                                       (task-child-configuration
                                        (agent-configuration parent) item)))
         (conversation
          (progn
           (task--execution-metadata job :configuration configuration)
           (let ((conversation
                  (conversation-create configuration :identifier
                                       (task-job-execution-identifier job)
                                       :prompt-cache-key
                                       (task--child-prompt-cache-key
                                        (task-job-root-conversation-identifier job))
                                       :storage-root
                                       (task--artifact-root configuration job)
                                       :parent-identifier
                                       (session-job-completion-owner-conversation job)
                                       :agent-name (job-name job))))
             (setf (getf (task-job-execution-metadata job) :conversation-file)
                   (namestring (conversation-pathname conversation)))
             conversation)))
         (worker       (lisp-worker-pool-create configuration))
         (taken        (make-hash-table :test #'equal))
         (registry
          (task-child-tool-registry (agent-tool-registry parent) :orchestrator
                                    orchestrator))
         (provider     (provider-with-configuration (agent-provider parent) configuration))
         (child
          (agent-create :class 'task-child-agent :configuration configuration :provider
                        provider :conversation conversation :tool-registry registry
                        :worker worker :initargs
                        (list :identity (task-job-identity job) :depth depth
                              :orchestrator orchestrator :job job)))
         (progress     (task-job-progress job))
         (observer
          (callback-agent-observer-create :text-callback
                                          (lambda (text)
                                            (task-progress-append-output progress text))
                                          :reasoning-callback
                                          (lambda (text) (declare (ignore text)) nil)
                                          :status-callback
                                          (lambda (status details)
                                            (when (eq status ':user-message-persisted)
                                              (setf (participant-state-identity
                                                     (agent-participant-state child))
                                                    (world-register-conversation
                                                     configuration conversation :subscribe-parent-p t)))
                                            (task-job-note-agent-status job status
                                                                        details))
                                          :steering-callback
                                          (lambda ()
                                            (append (task-job-take-steering job)
                                                    (world-inbox-steering-take
                                                     configuration :channel
                                                     (conversation-identifier
                                                      conversation)
                                                     :conversation conversation :taken
                                                     taken)))
                                          :steering-persisted-callback
                                          (lambda (identifier)
                                            (or
                                             (world-inbox-steering-ack configuration
                                                                       :channel
                                                                       (conversation-identifier
                                                                        conversation)
                                                                       :identifier
                                                                       identifier :taken
                                                                       taken)
                                             (task-job-acknowledge-steering job
                                                                            identifier)))
                                          :command-authorization-callback
                                          (task-job-command-authorization-function job)
                                          :tool-authorization-callback
                                          (task-job-tool-authorization-function job))))
    (setf (task-job-participant-state job) (agent-participant-state child))
    (mission-context-bind (task-job-mission-context job) configuration)
    (mission-context-bind (task-job-mission-context job) conversation)
    (mission-review-bind-checkpoint (task-job-review-checkpoint job) conversation)
    ;; The pooled job keeps execution authority; its hosted channel owns any
    ;; resources that must outlive the assignment's terminal result.
    (let ((lease
           (conversation-lease-acquire configuration
                                       (conversation-identifier conversation)))
          (host nil))
      (unwind-protect
          (progn
           (sb-sys:without-interrupts
             (setf host (world-server-prepare-assignment job child lease))
             (when host (world-server-start-assignment host)))
           (when host
             (world-server--wait-ready
              (application-world-server (world-host-application host)) host)
             (setf observer
                     (application-assignment-observer (world-host-application host)
                                                      observer job)))
           (task-child-inherit-reference-history job conversation provider)
           (let ((result
                  (agent-run-user-turn child (getf (task-job-item job) :task) :observer
                                       observer :automatic-p t :operation-request
                                       (task-child-assignment-request job conversation)
                                       :goal-context
                                       (task-child-goal-context job configuration))))
             (task--assemble-child-result job :provider-result result :child child
                                          :conversation conversation)))
        (unwind-protect
            (progn
             (task-job-cancel-execution-descendants job ':parent-finished)
             (when (participant-state-goal (agent-participant-state child))
               (conversation-append-record conversation
                                           (goal->record
                                            (participant-state-goal
                                             (agent-participant-state child))))))
          ; mallet:suppress no-ignore-errors -- best-effort release must not mask the child result
          (ignore-errors (task-completion-release child))
          ; mallet:suppress no-ignore-errors -- best-effort release must not mask the child result
          (ignore-errors
           (context-forget-conversation (conversation-identifier conversation)))
          (unless host
            ; mallet:suppress no-ignore-errors -- best-effort worker shutdown must not mask the child result
            (ignore-errors (lisp-worker-pool-stop-all worker))
            (conversation-lease-release lease)))))))
