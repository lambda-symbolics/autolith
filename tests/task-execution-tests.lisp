(in-package #:autolith)

;;;; -- Task Child Execution Tests --

(-> task-tests--child-registry
    (task-agent-definition task-orchestrator)
    tool-registry)
(defun task-tests--child-registry (definition orchestrator)
  "Return a child registry with authorization, yield, and trailing-effect tools."
  (let ((registry
          (task-child-tool-registry
           (make-instance 'tool-registry)
           definition
           orchestrator
           1)))
    (tool-registry-register
     registry
     (make-instance 'task-test-authorization-tool
                    :namespace "test"
                    :name "authorize"
                    :description "Authorize a harmless command."
                    :parameters (tool-object-schema (json-object) nil)))
    (tool-registry-register
     registry
     (make-instance 'task-test-effect-tool
                    :namespace "test"
                    :name "effect"
                    :description "Record an observable test effect."
                    :parameters (tool-object-schema (json-object) nil)))
    registry))

(-> test-task-abort-control-condition () null)
(defun test-task-abort-control-condition ()
  "Test that registry dispatch preserves the internal cancellation unwind."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration))
         (registry      (make-instance 'tool-registry))
         (tool
           (make-instance
            'task-test-abort-tool
            :namespace "test"
            :name "abort"
            :description "Signal a task cancellation."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry)))
    (unwind-protect
         (progn
           (tool-registry-register registry tool)
           (test-assert
            (handler-case
                (progn
                  (tool-registry-execute-call
                   registry
                   (json-object "namespace" "test"
                                "name" "abort"
                                "arguments" "{}")
                   context)
                  nil)
              (job-aborted (condition)
                (and (eq (job-aborted-reason condition) :test-cancel)
                     (string= (job-aborted-message condition)
                              "Task test was cancelled.")))
              (condition ()
                nil))
            "tool registry dispatch propagates job-aborted as control flow"))
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist ':ignore)))
  nil)

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, agent discovery, and yields."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((registry (make-default-tool-registry))
                  (initial-count (length (tool-registry-tools registry))))
             (task-augment-tool-registry registry)
             (test-assert
              (= (length (tool-registry-tools registry))
                 (+ initial-count 7))
              "task augmentation adds two task and five job tools")
             (dolist (name '("run" "agents"))
               (test-assert (tool-registry-find registry "task" name)
                            (format nil
                                    "task augmentation registers task.~A"
                                    name)))
             (dolist (name '("list" "get" "wait" "cancel"))
               (test-assert (tool-registry-find registry "job" name)
                            (format nil "task augmentation registers job.~A" name)))
             (test-assert (eq registry (task-augment-tool-registry registry))
                          "task augmentation is idempotent")
              (let* ((orchestrator
                       (task-run-tool-orchestrator
                        (tool-registry-find registry "task" "run")))
                     (definition
                       (task-find-agent-definition
                        (task-bundled-agent-definitions)
                        "task"))
                     (child-registry
                       (task-child-tool-registry
                        registry definition orchestrator 1))
                     (non-spawning-definition
                       (task-agent-definition-create
                        :name "no-spawn-jobs"
                        :description "Inspect no asynchronous work."
                        :instructions "Do not delegate."
                        :spawns nil
                        :source ':test))
                     (non-spawning-registry
                       (task-child-tool-registry
                        registry non-spawning-definition orchestrator 1)))
                (test-assert
                 (tool-registry-find child-registry "search" "content")
                 "general task children inherit native repository search")
                (test-assert
                 (null (tool-registry-find child-registry "self" "status"))
                 "task children never inherit active-image tools")
                (test-assert
                 (and
                  (tool-registry-find child-registry "task" "run")
                  (tool-registry-find child-registry "task" "agents")
                  (every
                   (lambda (name)
                     (null (tool-registry-find child-registry "job" name)))
                   '("list" "get" "wait" "cancel")))
                 "child spawning excludes the primary session job surface")
                (test-assert
                 (and
                  (null (tool-registry-find non-spawning-registry "task" "run"))
                  (null (tool-registry-find non-spawning-registry "task" "agents"))
                  (every
                   (lambda (name)
                     (null
                      (tool-registry-find non-spawning-registry "job" name)))
                   '("list" "get" "wait" "cancel")))
                 "children without spawn authority receive no task controls")))
           (let* ((registry (make-default-tool-registry))
                  (local-definition
                    (task-agent-definition-create
                     :name "local-grant"
                     :description "Use one available local tool."
                     :instructions "Read one file."
                     :tools '("resource.read")
                     :source ':test))
                  (hosted-definition
                    (task-agent-definition-create
                     :name "hosted-grant"
                     :description "Use hosted provider search."
                     :instructions "Search one authoritative source."
                     :tools '("web_search")
                     :source ':test))
                  (missing-definition
                    (task-agent-definition-create
                     :name "missing-grant"
                     :description "Request one unavailable local tool."
                     :instructions "Exercise fail-closed grant validation."
                     :tools '("missing.operation")
                     :source ':test)))
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     local-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "available child-safe local grants validate against the registry")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     hosted-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "web_search remains a recognized hosted provider grant")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     missing-definition registry)
                    nil)
                (task-agent-definition-error (condition)
                  (eq (task-agent-definition-error-field condition) :tools)))
              "unavailable local tool grants fail closed with typed metadata"))
           (let* ((parent-registry (make-instance 'tool-registry))
                  (definition
                    (task-agent-definition-create
                     :name "extension-boundary"
                     :description "Exercise extension capability defaults."
                     :instructions "Use only explicitly child-safe extensions."
                     :tools ':all
                     :source ':test))
                  (orchestrator (task-tests--orchestrator)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-default-deny-tool
                             :namespace "extension"
                             :name "denied"
                             :description "Remain unavailable to children."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-child-safe-tool
                             :namespace "extension"
                             :name "allowed"
                             :description "Opt into child availability."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (let ((child-registry
                     (task-child-tool-registry
                      parent-registry definition orchestrator 1)))
               (test-assert
                (null
                 (tool-registry-find child-registry "extension" "denied"))
                "ordinary extension tools default closed for child agents")
               (test-assert
                (tool-registry-find child-registry "extension" "allowed")
                "a class-specific child-safe method opts an extension in")))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Inspect the parser."
                                            "agent" "SCOUT")))))
             (test-assert (string= (getf item :agent) "scout")
                          "task normalization canonicalizes agent names")
             (test-assert (and (getf item :async)
                               (null (getf item :blocking)))
                          "task normalization detaches children by default"))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Stay synchronous."
                                            "blocking" t)))))
             (test-assert (and (getf item :blocking)
                               (null (getf item :async)))
                          "task blocking is an explicit opt-in"))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Use legacy blocking."
                                            "async" false)))))
             (test-assert (and (getf item :blocking)
                               (null (getf item :async)))
                          "legacy JSON false remains a blocking override"))
           (let* ((registry
                    (task-augment-tool-registry
                     (make-default-tool-registry)))
                  (conversation
                    (conversation-create
                     configuration :identifier "task-null-dispatch"))
                  (parent
                    (agent-create
                     :configuration configuration
                     :provider (make-instance 'model-provider)
                     :conversation conversation
                     :tool-registry registry
                     :worker nil))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :registry registry
                                   :agent parent))
                  (tool
                    (tool-registry-find registry "task" "run"))
                  (orchestrator (task-run-tool-orchestrator tool)))
             (unwind-protect
                  (let ((result
                          (tool-registry-execute-call
                           registry
                           (json-object
                            "namespace" "task"
                            "name" "run"
                            "arguments"
                            "{\"task\":\"Reject null async.\",\"async\":null}")
                           context)))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (null
                           (task-orchestrator-list-jobs orchestrator)))
                     "registry task.run decoding rejects JSON null before job admission"))
               (tool-registry-close-runtime-state registry)))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject removed fields."
                                "isolated" false))
                  nil)
              (task-error () t))
            "task normalization rejects the removed isolated field")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject bad booleans."
                                "async" "false"))
                  nil)
              (task-error () t))
            "task normalization rejects non-boolean async values")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject conflicting policy."
                                "blocking" t
                                "async" t))
                  nil)
              (task-error () t))
            "task normalization rejects combined blocking and async fields")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "tasks"
                                (json-array
                                 (json-object "task" "First")
                                 (json-object "task" "Second"))))
                  nil)
              (task-error ()
                t))
            "batch task normalization requires shared context")
           (dolist
               (case
                (list
                 (list
                  (json-object
                   "name" "forbidden-top-level-name"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level name")
                 (list
                  (json-object
                   "agent" "scout"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level agent")
                 (list
                 (json-object
                   "context" "Shared batch context."
                   "tasks"
                   (json-array
                    (json-object "task" "First" "legacy" t)))
                  "batch items reject unknown fields")
                 (list
                  (json-object
                   "context" "Shared batch context."
                   "tasks" "this string is not a task array")
                  "batch tasks reject strings despite their vector representation")))
             (test-assert
              (handler-case
                  (progn
                    (task-normalize-arguments (first case))
                    nil)
                (task-error ()
                  t))
              (second case)))
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.sexp" agent-directory))
                  (project-configuration
                    (configuration--clone configuration :working-directory root)))
             (task-tests--write-native-form
              agent-path
              (task-tests--role-form
               "scout" "Project scout" "Project instructions."))
             (let ((definition
                     (task-find-agent-definition
                      (task-discover-agents project-configuration)
                      "scout")))
               (test-assert (eq (task-agent-definition-source definition) :project)
                            "project agents override bundled definitions")
               (test-assert (string= (task-agent-definition-instructions definition)
                                     "Project instructions.")
                            "agent discovery retains native role instructions")))
           (let* ((immutable
                    (configuration--clone configuration :immutable-p t))
                  (definition
                    (task-agent-definition-create
                     :name "inheritance"
                     :description "Exercise configuration inheritance."
                     :instructions "Preserve inherited runtime configuration."
                     :tools ':all
                     :models '("@parent")
                     :source ':test))
                  (child-configuration
                    (task-configuration-for-definition immutable definition)))
             (test-assert
              (and (configuration-immutable-p child-configuration)
                   (equal (configuration-config-root child-configuration)
                          (configuration-config-root immutable))
                   (equal (configuration-data-root child-configuration)
                          (configuration-data-root immutable))
                   (equal (configuration-state-root child-configuration)
                          (configuration-state-root immutable))
                   (equal (configuration-cache-root child-configuration)
                          (configuration-cache-root immutable))
                   (equal (configuration-provider-endpoint child-configuration)
                          (configuration-provider-endpoint immutable)))
              "task model selection preserves every parent runtime boundary"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "structured"
                     :description "Yield structured data."
                     :instructions "Yield data matching the native output contract."
                     :output '(:type :object
                               :properties (("answer" (:type :string)))
                               :required ("answer"))
                     :source ':test))
                  (completion (make-instance 'task-completion))
                  (orchestrator (task-tests--orchestrator))
                  (parent (agent-create
                           :configuration configuration
                           :provider (make-instance 'model-provider)
                           :conversation (conversation-create configuration)
                           :tool-registry (make-instance 'tool-registry)
                           :worker nil))
                  (job (task-tests--make-job orchestrator
                                            :identifier "yield-test"
                                            :definition definition
                                            :item (list :task "Yield")
                                            :parent-agent parent))
                  (child (make-instance 'task-child-agent
                                        :configuration configuration
                                        :provider (make-instance 'model-provider)
                                        :conversation (conversation-create configuration)
                                        :tool-registry (make-instance 'tool-registry)
                                        :worker nil
                                        :definition definition
                                        :identity (task-job-identity job)
                                        :depth 1
                                        :completion completion
                                        :orchestrator orchestrator
                                        :job job))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker nil
                                          :conversation nil
                                          :agent child))
                  (tool (make-instance 'task-yield-tool
                                       :namespace "yield"
                                       :name "submit"
                                       :description ""
                                       :parameters (json-object))))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute tool context
                                  (json-object "status" "success"
                                               "data" (json-object "wrong" "shape")))
                    nil)
                (task-yield-error ()
                  t))
              "yield validation rejects data outside the output contract")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-task-child-cancels-lisp-executions () null)
(defun test-task-child-cancels-lisp-executions ()
  "Test child cleanup aborts asynchronous Lisp work before stopping its REPL pool."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (marker        (merge-pathnames "child-lisp-started" root))
         (worker-pool   (lisp-worker-pool-create configuration))
         (registry      (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool      (tool-registry-find registry "task" "run"))
         (orchestrator  (task-run-tool-orchestrator run-tool))
         (definition
           (task-agent-definition-create
            :name "lisp-cleanup"
            :description "Exercise asynchronous Lisp cleanup."
            :instructions "Start asynchronous Lisp work, then finish."
            :tools '("lisp.eval")
            :spawns ':all
            :source ':test))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation (conversation-create configuration)
            :tool-registry registry
            :worker nil))
         (job
           (task-tests--make-job
            orchestrator
            :identifier "lisp-cleanup-child"
            :definition definition
            :item (list :task "Exercise asynchronous Lisp cleanup.")
            :parent-agent parent))
         (child-registry
           (task-child-tool-registry registry definition orchestrator 1))
         (child
           (make-instance
            'task-child-agent
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation (conversation-create configuration)
            :tool-registry child-registry
            :worker worker-pool
            :definition definition
            :identity (task-job-identity job)
            :depth 1
            :completion (make-instance 'task-completion)
            :orchestrator orchestrator
            :job job))
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker worker-pool
            :conversation (agent-conversation child)
            :registry child-registry
            :agent child
            :call-id "child-lisp-cleanup"))
         (execution nil)
         (worker nil))
    (with-lock-held ((cl-jobpond::job--lock job))
      (setf (job-state job) ':running))
    (task-job--set-progress-state job ':running)
    (unwind-protect
         (let* ((form
                  (format nil
                          "(progn (with-open-file (stream ~A :direction :output :if-exists :supersede :if-does-not-exist :create) (write-string \"started\" stream)) (loop (sleep 1)))"
                          (prin1-to-string marker)))
                (result
                  (tool-execute
                   (tool-registry-find child-registry "lisp" "eval")
                   context
                   (json-object "form" form
                                "repl" "child-cleanup"
                                "async" t)))
                (details (tool-result-details result))
                (record (and (listp details) (getf (rest details) :job)))
                (identifier (and record (getf record :id))))
           (setf execution
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier child "lisp.eval")))
           ;; The evaluation starts a fresh SBCL worker, which takes tens of
           ;; seconds on Windows while the parallel check loads the host.
           (test-assert
            (and execution
                 (task-tests--wait-until (lambda () (probe-file marker)) 90))
            "a child asynchronous Lisp evaluation starts before cleanup")
           (setf worker (lisp-worker-pool-worker worker-pool "child-cleanup"))
           (test-assert
            (member execution
                    (task-job-cancel-execution-descendants job ':parent-finished)
                    :test #'eq)
            "child cleanup finds and cancels its asynchronous execution")
           (multiple-value-bind (snapshot terminal-p)
               (session-job-await execution 5)
             (test-assert
              (and terminal-p
                   (eq (getf snapshot :state) ':aborted)
                   (not (lisp-worker-running-p worker)))
              "child cleanup joins the aborted execution and detaches its REPL"))
           (lisp-worker-pool-stop-all worker-pool))
      (ignore-errors
        (task-job-cancel-execution-descendants job ':test-cleanup))
      (when (and execution (not (job-terminal-p execution)))
        (job-cancel execution :reason ':test-cleanup))
      (ignore-errors (lisp-worker-pool-stop-all worker-pool))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-task-child-shared-agent-loop () null)
(defun test-task-child-shared-agent-loop ()
  "Test child yield uses the ordinary provider and tool execution path."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "runtime"
            :description "Exercise the shared child runtime."
            :instructions "Yield after checking authorization."
            :source ':test))
         (orchestrator (task-tests--orchestrator))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation (conversation-create configuration)
            :tool-registry (make-instance 'tool-registry)
            :worker nil))
         (job
           (task-tests--make-job orchestrator
                                 :identifier "runtime-child"
                                 :definition definition
                                 :item (list :task "Exercise the shared loop.")
                                 :parent-agent parent))
         (completion (make-instance 'task-completion))
         (conversation
           (conversation-create configuration :identifier "task-shared-loop"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "child-yield"
              (list
               (agent-test-call :call-id "authorize"
                                :namespace "test"
                                :name "authorize")
               (agent-test-call
                :call-id "yield"
                :namespace "yield"
                :name "submit"
                :arguments
                "{\"status\":\"success\",\"text\":\"done\"}")
               (agent-test-call :call-id "effect"
                                :namespace "test"
                                :name "effect"))))))
         (child
           (make-instance 'task-child-agent
                          :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry
                          (task-tests--child-registry definition orchestrator)
                          :worker nil
                          :definition definition
                          :identity (task-job-identity job)
                          :depth 1
                          :completion completion
                          :orchestrator orchestrator
                          :job job))
         (observer
           (callback-agent-observer-create
            :command-authorization-callback
            (lambda (command directory)
              (declare (ignore command directory))
              ':sandboxed))))
    (with-lock-held ((cl-jobpond::job--lock job))
      (setf (job-state job) ':running))
    (task-job--set-progress-state job ':running)
    (unwind-protect
         (let ((*task-test-command-decision* nil)
               (*task-test-effect-count* 0))
           (agent-run-user-turn child "Run the shared loop." :observer observer)
           (test-assert (task-completion-called-p completion)
                        "yield.submit completes a child through the shared loop")
           (test-assert (eq *task-test-command-decision* ':sandboxed)
                        "child tools receive the ordinary command authorization path")
           (test-assert (zerop *task-test-effect-count*)
                        "calls after terminal yield are not executed")
           (let* ((records (conversation--read-records
                            (conversation-pathname conversation)))
                  (results (remove-if-not
                            (lambda (record)
                              (eq (first record) :tool-result))
                            records)))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :status))
                             results)
                     '(:ok :ok :error))
              "yield retains its result and rejects every trailing call")
             (test-assert
              (and (every (lambda (record)
                            (typep (getf (rest record) :cpu-microseconds)
                                   '(integer 0)))
                          (subseq results 0 2))
                   (null (getf (rest (third results)) :cpu-microseconds)))
              "executed child calls retain timings while rejected calls omit them")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
