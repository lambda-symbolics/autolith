(in-package #:autolith)

;;;; -- Task Child Execution --

(defun task-parent-depth (agent)
  "Return AGENT's explicit task depth, treating the primary agent as zero."
  (if (typep agent 'task-child-agent)
      (task-child-agent-depth agent)
      0))

(defun task-parent-spawn-policy (agent)
  "Return AGENT's effective child-agent spawn policy."
  (if (typep agent 'task-child-agent)
      (task-agent-definition-spawns (task-child-agent-definition agent))
      :all))

(defun task-parent-can-spawn-p (agent child-name orchestrator)
  "Return true when AGENT may create CHILD-NAME at the configured depth."
  (let ((policy (task-parent-spawn-policy agent)))
    (and
     (< (task-parent-depth agent)
        (task-orchestrator-maximum-depth orchestrator))
     (or (eq policy :all)
         (and (listp policy) (member child-name policy :test #'string-equal)))
     t)))

(defun task--tool-spec-matches-p (spec tool)
  "Return true when agent tool SPEC permits TOOL."
  (let* ((normalized (string-downcase spec))
         (canonical (string-downcase (tool-canonical-name tool)))
         (namespace (string-downcase (tool-namespace tool))))
    (or (string= normalized canonical)
        (string= normalized (format nil "~A.*" namespace)))))

(defun task--definition-allows-tool-p (definition tool)
  "Return true when DEFINITION permits ordinary TOOL."
  (let ((specs (task-agent-definition-tools definition)))
    (and (tool-child-safe-p tool)
         (or (eq specs :all)
             (and (listp specs)
                  (some (lambda (spec)
                          (task--tool-spec-matches-p spec tool))
                        specs))))))

(-> task-agent-definition-validate-tools-available
    (task-agent-definition tool-registry)
    null)
(defun task-agent-definition-validate-tools-available (definition registry)
  "Reject explicit grants in DEFINITION that REGISTRY cannot provide."
  (let ((specifications (task-agent-definition-tools definition)))
    (when (listp specifications)
      (dolist (specification specifications)
        (unless
            (or (string= specification "web_search")
                (some (lambda (tool)
                        (and (tool-child-safe-p tool)
                             (task--tool-spec-matches-p specification tool)))
                      (tool-registry-tools registry)))
          (task-agent-definition--error
           :pathname (task-agent-definition-pathname definition)
           :source (task-agent-definition-source definition)
           :field ':tools
           :cause (format nil "Tool grant ~S is unavailable in this session."
                          specification)
           :definition-name (task-agent-definition-name definition)))))
  nil))

(defun task-child-tool-registry (parent-registry definition orchestrator depth)
  "Build a restricted child registry with yield and structurally bounded spawning."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools parent-registry))
      (when (task--definition-allows-tool-p definition tool)
        (tool-registry-register registry tool)))
    (when
        (and (task-agent-definition-spawns definition)
             (< depth (task-orchestrator-maximum-depth orchestrator)))
      (dolist (name '("run" "agents"))
        (let ((task-tool (tool-registry-find parent-registry "task" name)))
          (when task-tool
            (tool-registry-register registry task-tool)))))
    (dolist (tool (tool-registry-tools parent-registry))
      (when (string= (tool-namespace tool) "job")
        (tool-registry-register registry tool)))
    (let ((output (task-agent-definition-output definition)))
      (tool-registry-describe-namespace
       registry "yield"
       "Required terminal result submission for child agents.")
      (tool-registry-register
       registry
       (make-instance
        'task-yield-tool
        :namespace "yield"
        :name "submit"
        :description
        "Submit the required terminal child result. Call exactly once when the assignment is complete or cannot continue."
        :parameters
        (tool-object-schema
         (json-object
          "status"
          (json-object "type" "string"
                       "description" "Terminal result status."
                       "enum" (json-array "success" "failed" "aborted"))
          "text"
          (tool-string-property
           "Human-readable result for the parent; include concrete findings or changes.")
          "data"
          (if output
              (task-output-schema->json output)
              (json-object
               "description"
               "Optional structured result when the agent has no output contract."))
          "error"
          (tool-string-property
           "Failure or abort explanation when status is not success.")
          "label"
          (tool-string-property "Optional short result label."))
         '("status")))))
    (let ((manager (mcp-tool-registry-manager parent-registry)))
      (when
          (and manager
               (or
                (eq (task-agent-definition-tools definition) :all)
                (some
                 (lambda (tool)
                   (and
                    (typep tool 'mcp-provider-tool)
                    (task--definition-allows-tool-p definition tool)))
                 (tool-registry-tools parent-registry))))
        (mcp-tool-registry-bind-manager
         registry
         manager
         (lambda (tool)
           (task--definition-allows-tool-p definition tool)))
        (mcp-tool-registry-refresh registry :only-dirty-p t)))
    registry))

(defun task--model-alias (alias parent-model)
  "Resolve one child model ALIAS relative to PARENT-MODEL."
  (let ((value (string-downcase alias)))
    (cond
      ((member value '("@task" "@parent" "@auto") :test #'string=) parent-model)
      ((string= value "@smol") "gpt-5.6-luna")
      ((member value '("@slow" "@designer") :test #'string=) "gpt-5.6-terra")
      ((member alias *supported-models* :test #'string=) alias) (t nil))))

(defun task--thinking-effort (level parent-effort)
  "Resolve child reasoning LEVEL to a supported provider effort."
  (let ((value (and level (string-downcase (symbol-name level)))))
    (cond
      ((or (null value) (string= value "auto"))
       parent-effort)
      ((member value *supported-reasoning-efforts* :test #'string=) value)
      (t parent-effort))))

(defun task-configuration-for-definition (parent-configuration definition)
  "Copy PARENT-CONFIGURATION with DEFINITION's model, effort, and web policy."
  (let* ((parent-model (configuration-model parent-configuration))
         (model
          (or
           (loop for candidate in (task-agent-definition-models definition)
                 for resolved = (task--model-alias candidate parent-model)
                 when resolved return resolved)
           parent-model))
         (effort
          (task--thinking-effort
           (task-agent-definition-reasoning-effort definition)
           (configuration-reasoning-effort parent-configuration)))
         (web-enabled-p
          (or (eq (task-agent-definition-tools definition) :all)
              (member "web_search" (task-agent-definition-tools definition)
                      :test #'string-equal))))
    (configuration--clone
     parent-configuration
     :model model
     :reasoning-effort effort
     :web-search-mode
     (if web-enabled-p
         (configuration-web-search-mode parent-configuration)
         "disabled"))))

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
                    (floor (configuration-context-window configuration)
                           *task-inherited-reference-context-divisor*)))))
    (and (>= limit
             (conversation--inherited-reference-wire-byte-length nil))
         limit)))

(defun task-output-definition-text (definition)
  "Return DEFINITION's output contract as prompt text, or NIL."
  (let ((output (task-agent-definition-output definition)))
    (and output
         (task--write-readable-sexp output :pretty-p t))))

(defun task-child-goal-context (job child-configuration)
  "Build the transient developer instructions for JOB's child session."
  (let* ((definition (task-job-definition job))
         (identity (task-job-identity job))
         (item (task-job-item job))
         (context (getf item :context))
         (output (task-output-definition-text definition)))
    (format nil
            "You are child agent ~A of type ~A, depth ~D. Your specialized role follows.~2%~A~@[~2%Shared parent context:~%~A~]~@[~2%Your yield data must satisfy this native output contract:~%~A~]~2%You are not the primary Autolith session. self.* tools are deliberately unavailable. Work only in ~A. Complete the assignment in the user message. You MUST end by calling yield.submit exactly once. A normal assistant stop without yield is a failed child run. Put the useful parent-facing answer in yield.text and structured data in yield.data when requested."
            (getf identity :id) (task-agent-definition-name definition)
            (1+ (task-parent-depth (task-job-parent-agent job)))
            (task-agent-definition-instructions definition) context output
            (namestring
             (configuration-working-directory child-configuration)))))


(defmethod tool-execute
    ((tool task-yield-tool) (context tool-context) arguments)
  "Validate and record one exact terminal child yield."
  (declare (ignore tool))
  (let ((agent (tool-context-agent context)))
    (unless (typep agent 'task-child-agent)
      (error 'task-yield-error
             :message "yield.submit is available only inside a child agent."
             :tool-name "yield.submit"))
    (let* ((completion (task-child-agent-completion agent))
           (identifier (getf (task-child-agent-identity agent) :id))
           (output
             (task-agent-definition-output
              (task-child-agent-definition agent))))
      (labels ((yield-error (message)
                 (error 'task-yield-error
                        :message message
                        :tool-name "yield.submit"
                        :task-id identifier))

               (optional-string (name)
                 (multiple-value-bind (value present-p)
                     (gethash name arguments)
                   (when (and present-p (not (stringp value)))
                     (yield-error
                      (format nil "Yield field ~S must be a string when supplied."
                              name)))
                   (values value present-p))))
        (when (task-completion-called-p completion)
          (yield-error "This child already submitted its terminal yield."))
        (loop for field being the hash-keys of arguments
              unless (member field
                             '("status" "text" "data" "error" "label")
                             :test #'string=)
                do (yield-error
                    (format nil "Unknown yield field ~S." field)))
        (multiple-value-bind (status-text status-present-p)
            (gethash "status" arguments)
          (unless (and status-present-p (stringp status-text))
            (yield-error "Yield status must be a string."))
          (let ((status
                  (cond
                    ((string= status-text "success") :success)
                    ((string= status-text "failed") :failed)
                    ((string= status-text "aborted") :aborted)
                    (t
                     (yield-error
                      "Yield status must be exactly success, failed, or aborted.")))))
            (multiple-value-bind (text text-present-p)
                (optional-string "text")
              (declare (ignore text-present-p))
              (multiple-value-bind (failure failure-present-p)
                  (optional-string "error")
                (multiple-value-bind (label label-present-p)
                    (optional-string "label")
                  (declare (ignore label-present-p))
                  (when (and label
                             (> (length label)
                                *task-result-label-maximum-characters*))
                    (yield-error
                     (format nil
                             "Yield label may contain at most ~D characters."
                             *task-result-label-maximum-characters*)))
                  (multiple-value-bind (data data-present-p)
                      (gethash "data" arguments)
                    (case status
                      (:success
                       (when failure-present-p
                         (yield-error
                          "A successful yield must not contain an error field."))
                       (unless (or data-present-p
                                   (non-empty-string-p (task--trim (or text ""))))
                         (yield-error
                          "A successful yield requires non-empty text or explicit data."))
                       (when (and output (not data-present-p))
                         (yield-error
                          "This role requires an explicit structured yield value."))
                       (when (and output
                                  (not (task-output-schema-valid-p data output)))
                         (yield-error
                          "The supplied yield data does not satisfy the role output contract.")))
                      ((:failed :aborted)
                       (unless (and failure-present-p
                                    (non-empty-string-p
                                     (task--trim (or failure ""))))
                         (yield-error
                          "A failed or aborted yield requires a non-empty error string."))
                       (when data-present-p
                         (yield-error
                          "A failed or aborted yield must not contain structured data."))))
                    (when data-present-p
                      (task-json->sexp data))
                    (multiple-value-bind (accepted-p reason)
                        (task-job--call-with-yield-claim
                         (task-child-agent-job agent)
                         (lambda ()
                           (setf (task-completion-called-p completion) t
                                 (task-completion-status completion) status
                                 (task-completion-text completion) text
                                 (task-completion-data completion) data
                                 (task-completion-data-present-p completion)
                                 data-present-p
                                 (task-completion-error completion) failure
                                 (task-completion-label completion) label)))
                      (unless accepted-p
                        (yield-error
                         (ecase reason
                           (:closed
                            "This child can no longer accept a terminal yield.")
                           (:closing
                            "This child is closing and cannot accept a terminal yield.")
                           (:not-running
                            "This child is not running and cannot accept a terminal yield.")
                           (:steering-pending
                            "Additional user context is waiting. Continue the child turn before yielding."))))
                      (tool-success
                       (task--write-readable-sexp
                        '(:yield-submit :accepted-p t))))))))))))))

(defun task--utf8-length (text)
  "Return the UTF-8 byte length of TEXT on the supported SBCL runtime."
  (length (sb-ext:string-to-octets text :external-format ':utf-8)))

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
         (lines (task--split-lines (or text "")))
         (line-bounded-p (> (length lines) maximum-lines))
         (line-text
          (format nil "~{~A~^~%~}"
                  (subseq lines 0 (min maximum-lines (length lines)))))
         (byte-bounded-p (> (task--utf8-length line-text) maximum-bytes))
         (bounded
          (if byte-bounded-p
              (let ((low 0) (high (length line-text)))
                (loop while (< low high)
                      for middle = (ceiling (+ low high) 2)
                      if (<= (task--utf8-length (subseq line-text 0 middle))
                             maximum-bytes)
                      do (setf low middle) else
                      do (setf high (1- middle)))
                (subseq line-text 0 low))
              line-text)))
    (if (or line-bounded-p byte-bounded-p)
        (format nil "~A~%... [task output truncated]" bounded)
        bounded)))

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
   (configuration-data-root configuration)))

(-> task--write-result-artifact (task-job list) pathname)
(defun task--write-result-artifact (job result)
  "Publish portable RESULT once at JOB's unique artifact pathname."
  (let* ((configuration (agent-configuration (task-job-parent-agent job)))
         (root (task--artifact-root configuration job))
         (target (merge-pathnames "result.sexp" root))
         (temporary
          (merge-pathnames
           (make-pathname :name
                          (format nil ".result.~A" (make-identifier))
                          :type "tmp")
           root)))
    (ensure-directories-exist target)
    (when (probe-file target)
      (error 'task-error
             :message
             (format nil "Task artifact pathname is already occupied: ~A"
                     target)
             :tool-name "task.run"
             :task-id (job-identifier job)))
    (unwind-protect
         (progn
           (with-open-file
               (stream temporary
                       :direction ':output
                       :if-exists ':supersede
                       :if-does-not-exist ':create
                       :external-format ':utf-8)
             (with-standard-io-syntax
               (let ((*print-readably* t)
                     (*print-pretty* t)
                     (*print-circle* t))
                 (prin1 result stream)
                 (terpri stream)
                 (finish-output stream))))
           (when (probe-file target)
             (error 'task-error
                    :message
                    (format nil "Task artifact pathname became occupied: ~A"
                            target)
                    :tool-name "task.run"
                    :task-id (job-identifier job)))
           (rename-file temporary target)
           target)
      (when (probe-file temporary) (delete-file temporary)))))

(defun task--result-output (completion progress result)
  "Select bounded child output from COMPLETION, PROGRESS, and provider RESULT."
  (let ((text
         (or (task-completion-text completion)
             (provider-result-assistant-text result)
             (let ((tail (with-lock-held ((task-progress-lock progress))
                           (copy-seq (task-progress-output-tail progress)))))
               (and (non-empty-string-p tail) tail)))))
    (cond ((non-empty-string-p text) (task--bounded-output text))
          ((task-completion-data-present-p completion)
           (task--bounded-output
            (task--write-readable-sexp
             (task-json->sexp (task-completion-data completion)))))
          ((plusp (task-progress-request-count progress))
           (format nil "(no output) after ~D req"
                   (task-progress-request-count progress)))
          (t "(no output)"))))

(defun task--assemble-child-result
    (job provider-result child conversation completion)
  "Assemble one portable SingleResult-style plist for a completed child."
  (let* ((progress (task-job-progress job))
         (status
          (if (task-completion-called-p completion)
              (task-completion-status completion)
              :failed))
         (output (task--result-output completion progress provider-result))
         (duration
          (task--milliseconds-between
           (or (job-started-at job) (job-created-at job))
           (get-internal-real-time)))
         (base
          (list :id (job-identifier job) :name
                (task-job-display-name job) :agent
                (task-agent-definition-name (task-job-definition job))
                :agent-source
                (task-agent-definition-source (task-job-definition job))
                :assignment (getf (task-job-item job) :task) :status status
                :output output :error
                (or (task-completion-error completion)
                    (and (not (task-completion-called-p completion))
                         "Child stopped without calling yield.submit."))
                :yielded-p (task-completion-called-p completion)
                :structured-output-present-p
                (task-completion-data-present-p completion)
                :structured-output
                (and (task-completion-data-present-p completion)
                     (task-json->sexp (task-completion-data completion)))
                :label
                (task-completion-label completion) :request-count
                (task-progress-request-count progress) :usage
                (task-progress-usage progress) :duration-ms duration :model
                (configuration-model (agent-configuration child))
                :conversation-file
                (namestring (conversation-pathname conversation)) :detached
                (task-job-detached-p job))))
    base))

(defun task--failed-result (job status message)
  "Return a portable terminal failure result for JOB."
  (let* ((parent (task-job-parent-agent job))
         (progress (task-job-progress job))
         (tail (with-lock-held ((task-progress-lock progress))
                 (copy-seq (task-progress-output-tail progress))))
         (model (and parent
                     (configuration-model (agent-configuration parent)))))
    (list :id (job-identifier job)
          :name (task-job-display-name job)
          :agent (task-agent-definition-name (task-job-definition job))
          :agent-source (task-agent-definition-source
                         (task-job-definition job))
          :assignment (getf (task-job-item job) :task)
          :status status
          :output (if (non-empty-string-p tail)
                      (task--bounded-output tail)
                      "(no output)")
          :error message
          :yielded-p nil
          :request-count (task-progress-request-count progress)
          :usage (task-progress-usage progress)
          :duration-ms
          (task--milliseconds-between
           (or (job-started-at job) (job-created-at job))
           (get-internal-real-time))
          :model model
          :detached (task-job-detached-p job))))

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
       (conversation-identifier (agent-conversation parent))
       (task-job-inherited-reference-items job)))))

(-> task-job-note-agent-status (task-job keyword list) null)
(defun task-job-note-agent-status (job status details)
  "Project one child observer STATUS into progress or durable response delivery."
  (if (eq status ':assistant-response-persisted)
      (let ((text (getf details :text))
            (timestamp (getf details :time)))
        (when (and (stringp text) (typep timestamp 'timestamp))
          (task-job-note-verbal-response job text timestamp)))
      (task-progress-note-status job status details))
  nil)

(defun task-run-child (job)
  "Create and run JOB's real in-process child session through terminal yield."
  (let* ((parent (task-job-parent-agent job))
         (definition (task-job-definition job))
         (orchestrator (task-job-orchestrator job))
         (depth (1+ (task-parent-depth parent)))
         (configuration
          (task-configuration-for-definition (agent-configuration parent)
                                             definition))
         (conversation
          (conversation-create
           configuration
           :identifier (task-job-execution-identifier job)
           :prompt-cache-key (task-job-root-conversation-identifier job)
           :storage-root (task--artifact-root configuration job)))
         (worker (lisp-worker-pool-create configuration))
         (completion (make-instance 'task-completion))
         (registry
          (task-child-tool-registry (agent-tool-registry parent) definition
                                    orchestrator depth))
         (provider
          (provider-with-configuration (agent-provider parent) configuration))
         (child
          (make-instance 'task-child-agent :configuration configuration
                         :provider provider :conversation conversation
                         :tool-registry registry
                         :worker worker
                         :session-id (agent-session-id parent)
                         :definition definition
                         :identity (task-job-identity job) :depth depth
                         :completion completion :orchestrator orchestrator :job
                         job))
         (progress (task-job-progress job))
         (observer
          (callback-agent-observer-create
           :text-callback
           (lambda (text)
             (task-progress-append-output progress text))
           :reasoning-callback
           (lambda (text)
             (declare (ignore text))
             nil)
           :status-callback
           (lambda (status details)
             (task-job-note-agent-status job status details))
           :steering-callback
           (lambda ()
             (task-job-take-steering job))
           :steering-persisted-callback
           (lambda (identifier)
             (task-job-acknowledge-steering job identifier))
           :command-authorization-callback
           (task-job-command-authorization-function job)
           :tool-authorization-callback
           (task-job-tool-authorization-function job))))
    (unwind-protect
         (progn
           (task-child-inherit-reference-history job conversation provider)
           (with-observability-context
               (task-job-observability-context job)
             (let ((result
                     (agent-run-user-turn
                      child
                      (getf (task-job-item job) :task)
                      :observer observer
                      :goal-context
                      (task-child-goal-context job configuration))))
               (task--assemble-child-result
                job result child conversation completion))))
      (ignore-errors (lisp-worker-pool-stop-all worker)))))
