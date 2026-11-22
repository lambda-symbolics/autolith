(in-package #:autolith)

;;;; -- Task Child Execution Tests --

(-> task-tests--child-registry
    (task-agent-definition task-orchestrator)
    tool-registry)
(defun task-tests--child-registry (definition orchestrator)
  "Return a child registry with the ordinary command-authorization fixture."
  (let ((registry
          (task-child-tool-registry
           (make-instance 'tool-registry)
           definition
           :orchestrator orchestrator
           :depth        1)))
    (tool-registry-register
     registry
     (make-instance 'task-test-authorization-tool
                    :namespace   "test"
                    :name        "authorize"
                    :description "Authorize a harmless command."
                    :parameters  (tool-object-schema (json-object) nil)))
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
            :namespace   "test"
            :name        "abort"
            :description "Signal a task cancellation."
            :parameters  (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker        nil
                          :conversation  conversation
                          :registry      registry)))
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
                                           :if-does-not-exist   ':ignore)))
  nil)

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, and agent discovery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((registry (make-default-tool-registry)))
             (task-augment-tool-registry registry)
             (dolist (name '("spawn" "roles"))
               (test-assert (tool-registry-find registry "child" name)
                            (format nil
                                    "task augmentation registers child.~A"
                                    name)))
             (dolist (name '("list" "get" "wait" "cancel"))
               (test-assert (tool-registry-find registry "job" name)
                            (format nil "task augmentation registers job.~A" name)))
             (test-assert (eq registry (task-augment-tool-registry registry))
                          "task augmentation is idempotent")
             (let* ((orchestrator
                      (task-run-tool-orchestrator
                       (tool-registry-find registry "child" "spawn")))
                    (definition
                      (task-find-agent-definition
                       (task-bundled-agent-definitions)
                       "task"))
                    (child-registry
                      (task-child-tool-registry
                       registry definition :orchestrator orchestrator
                                           :depth        1))
                    (non-spawning-definition
                      (task-agent-definition-create
                       :name         "no-spawn-jobs"
                       :description  "Inspect no asynchronous work."
                       :instructions "Do not delegate."
                       :spawns       nil
                       :source       ':test))
                    (non-spawning-registry
                      (task-child-tool-registry
                       registry non-spawning-definition :orchestrator orchestrator
                                                        :depth        1)))
               (test-assert
                (tool-registry-find child-registry "search" "content")
                "general task children inherit native repository search")
               (test-assert
                (and (null (tool-registry-find child-registry "self" "status"))
                     (null (tool-registry-find child-registry "lisp" "apropos")))
                "task children never inherit active-image tools")
               (test-assert
                (and
                 (tool-registry-find child-registry "child" "spawn")
                 (tool-registry-find child-registry "child" "roles")
                 (every
                  (lambda (name)
                    (tool-registry-find child-registry "job" name))
                  '("list" "get" "wait" "cancel")))
                "task children can manage jobs within their existing visibility scope")
               (test-assert
                (and
                 (null (tool-registry-find non-spawning-registry "child" "spawn"))
                 (null (tool-registry-find non-spawning-registry "child" "roles"))
                 (every
                  (lambda (name)
                    (null
                     (tool-registry-find non-spawning-registry "job" name)))
                  '("list" "get" "wait" "cancel")))
                "children without spawn authority receive no task controls")))
           (let* ((registry (make-default-tool-registry))
                  (local-definition
                    (task-agent-definition-create
                     :name         "local-grant"
                     :description  "Use one available local tool."
                     :instructions "Read one file."
                     :tools        '("resource.read")
                     :source       ':test))
                  (hosted-definition
                    (task-agent-definition-create
                     :name         "hosted-grant"
                     :description  "Use hosted provider search."
                     :instructions "Search one authoritative source."
                     :tools        '("web_search")
                     :source       ':test))
                  (missing-definition
                    (task-agent-definition-create
                     :name         "missing-grant"
                     :description  "Request one unavailable local tool."
                     :instructions "Exercise fail-closed grant validation."
                     :tools        '("missing.operation")
                     :source       ':test)))
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
                     :name         "extension-boundary"
                     :description  "Exercise extension capability defaults."
                     :instructions "Use only explicitly child-safe extensions."
                     :tools        ':all
                     :source       ':test))
                  (orchestrator    (task-tests--orchestrator)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-default-deny-tool
                             :namespace   "extension"
                             :name        "denied"
                             :description "Remain unavailable to children."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-child-safe-tool
                             :namespace   "extension"
                             :name        "allowed"
                             :description "Opt into child availability."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (let ((child-registry
                     (task-child-tool-registry
                      parent-registry definition :orchestrator orchestrator
                                                 :depth        1)))
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
                          "task normalization canonicalizes agent names"))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject removed fields."
                                "isolated" (json-false)))
                  nil)
              (task-error () t))
            "task normalization rejects the removed isolated field")
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.sexp" agent-directory))
                  (project-configuration
                    (configuration-copy configuration :working-directory root)))
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
                    (configuration-copy configuration
                                        :immutable-p       t
                                        :reasoning-effort  "xhigh"
                                        :codex-fast-mode-p t))
                  (definition
                    (task-agent-definition-create
                     :name         "inheritance"
                     :description  "Exercise configuration inheritance."
                     :instructions "Preserve inherited runtime configuration."
                     :tools        ':all
                     :models       '("@parent")
                     :source       ':test))
                  (child-configuration
                    (task-configuration-for-definition immutable definition))
                  (explicit-definition
                    (task-agent-definition-create
                     :name             "explicit-effort"
                     :description      "Exercise an explicit child effort."
                     :instructions     "Reason at the declared effort."
                     :tools            ':all
                     :models           '("@parent")
                     :reasoning-effort ':high
                     :source           ':test))
                  (explicit-configuration
                    (task-configuration-for-definition
                     immutable explicit-definition)))
             (test-assert
              (string= (config :reasoning-effort child-configuration)
                       *task-default-reasoning-effort*)
              "an unspecified child effort resolves to the routine default")
             (test-assert
              (string= (config :reasoning-effort explicit-configuration) "high")
              "an explicit child effort is honored")
             (test-assert
              (and (not (config :codex-fast-mode-p child-configuration))
                   (not (config :codex-fast-mode-p explicit-configuration)))
              "children never inherit Codex Fast mode")
             (test-assert
              (and (config :immutable-p child-configuration)
                   (equal (config :config-root child-configuration)
                          (config :config-root immutable))
                   (equal (config :data-root child-configuration)
                          (config :data-root immutable))
                   (equal (config :state-root child-configuration)
                          (config :state-root immutable))
                   (equal (config :cache-root child-configuration)
                          (config :cache-root immutable))
                   (equal (config :provider-endpoint child-configuration)
                          (config :provider-endpoint immutable)))
              "task model selection preserves every parent runtime boundary")))
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
         (run-tool      (tool-registry-find registry "child" "spawn"))
         (orchestrator  (task-run-tool-orchestrator run-tool))
         (definition
           (task-agent-definition-create
            :name         "lisp-cleanup"
            :description  "Exercise asynchronous Lisp cleanup."
            :instructions "Start asynchronous Lisp work, then finish."
            :tools        '("lisp.eval")
            :spawns       ':all
            :source       ':test))
         (parent
           (agent-create
            :configuration configuration
            :provider      (make-instance 'model-provider)
            :conversation  (conversation-create configuration)
            :tool-registry registry
            :worker        nil))
         (job
           (task-tests--make-job
            orchestrator
            :identifier   "lisp-cleanup-child"
            :definition   definition
            :item         (list :task "Exercise asynchronous Lisp cleanup.")
            :parent-agent parent))
         (child-registry
           (task-child-tool-registry registry definition :orchestrator orchestrator
                                                         :depth        1))
         (child
           (make-instance
            'task-child-agent
            :configuration configuration
            :provider      (make-instance 'model-provider)
            :conversation  (conversation-create configuration)
            :tool-registry child-registry
            :worker        worker-pool
            :definition    definition
            :identity      (task-job-identity job)
            :depth         1
            :orchestrator  orchestrator
            :job           job))
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        worker-pool
            :conversation  (agent-conversation child)
            :registry      child-registry
            :agent         child
            :call-id       "child-lisp-cleanup"))
         (execution     nil)
         (worker        nil))
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
                   (json-object "forms" (vector form)
                                "repl" "child-cleanup"
                                "async" t)))
                (details    (tool-result-details result))
                (record     (and (listp details) (getf (rest details) :job)))
                (identifier (and record (getf record :id))))
           (setf execution
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier :viewer child :tool-name "lisp.eval")))
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
                   (not (sbcl-worker-running-p worker)))
              "child cleanup joins the aborted execution and detaches its REPL"))
           (lisp-worker-pool-stop-all worker-pool))
      (ignore-errors
        (task-job-cancel-execution-descendants job ':test-cleanup))
      (when (and execution (not (job-terminal-p execution)))
        (job-cancel execution :reason ':test-cleanup))
      (test-assert (task-orchestrator-close orchestrator)
                   "The fixture joins execution-pool writers before deleting their state")
      (ignore-errors (lisp-worker-pool-stop-all worker-pool))
      (unless (task-orchestrator-close orchestrator)
        (error 'task-error
               :message   "Task job pools did not stop before test cleanup."
               :tool-name "child.spawn"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-task-child-shared-agent-loop () null)
(defun test-task-child-shared-agent-loop ()
  "Test ordinary child responses share the provider and tool execution path."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name         "runtime"
            :description  "Exercise the shared child runtime."
            :instructions "Check authorization, then finish."
            :source       ':test))
         (orchestrator  (task-tests--orchestrator))
         (parent
           (agent-create
            :configuration configuration
            :provider      (make-instance 'model-provider)
            :conversation  (conversation-create configuration)
            :tool-registry (make-instance 'tool-registry)
            :worker        nil))
         (job
           (task-tests--make-job orchestrator
                                 :identifier   "runtime-child"
                                 :definition   definition
                                 :item         (list :task "Exercise the shared loop.")
                                 :parent-agent parent))
         (conversation
           (conversation-create configuration :identifier "task-shared-loop"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "child-authorize"
              (list (agent-test-call :call-id   "authorize"
                                     :namespace "test"
                                     :name      "authorize")))
             (agent-test-result "child-final"
                                (list (agent-test-message "done"))))))
         (child
           (make-instance 'task-child-agent
                          :configuration configuration
                          :provider      provider
                          :conversation  conversation
                          :tool-registry
                          (task-tests--child-registry definition orchestrator)
                          :worker        nil
                          :definition    definition
                          :identity      (task-job-identity job)
                          :depth         1
                          :orchestrator  orchestrator
                          :job           job))
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
         (let* ((result  (agent-run-user-turn child "Run the shared loop."
                                            :observer observer))
                (records (conversation--read-records
                          (conversation-pathname conversation)))
                (authorization
                  (find-if (lambda (record)
                             (and (eq (first record) :tool-result)
                                  (equal (getf (rest record) :call-id) "authorize")))
                           records)))
           (test-assert (agent-turn-complete-p child result)
                        "an ordinary final response completes the child turn")
           (test-assert
            (and authorization
                 (eq (getf (rest authorization) :category) ':success)
                 (equal (getf (rest authorization) :output) "sandboxed"))
            "child tools receive the ordinary command authorization path"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-task-child-execution-job-controls () null)
(defun test-task-child-execution-job-controls ()
  "Collect child-owned asynchronous results without exposing parent jobs."
  (with-test-configuration (configuration)
    (let* ((registry       (task-augment-tool-registry (make-default-tool-registry)))
           (orchestrator
             (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
           (definition
             (task-agent-definition-create
              :name "job-reader" :description "Inspect owned asynchronous work."
              :instructions "Await the result." :tools ':all :spawns nil
              :source ':test))
           (primary        (task-tests--primary-agent configuration "child-jobs" registry))
           (child-job      (task-tests--register-job orchestrator primary definition
                                                      :name "child-owner"))
           (child-registry (task-child-tool-registry registry definition
                                                     :orchestrator orchestrator
                                                     :depth        1))
           (child          (task-tests--child-viewer configuration child-job
                                                      :registry child-registry))
           (context        (make-instance 'tool-context
                                          :configuration configuration
                                          :conversation  (agent-conversation child)
                                          :registry child-registry :agent child :worker nil)))
      (unwind-protect
           (progn
             (dolist (role '("reviewer" "librarian"))
               (let* ((role-definition
                        (task-find-agent-definition (task-bundled-agent-definitions) role))
                      (role-registry
                        (task-child-tool-registry registry role-definition
                                                  :orchestrator orchestrator
                                                  :depth        1)))
                 (test-assert (tool-registry-find role-registry "job" "wait")
                              (format nil "~A can await its execution jobs" role))))
             (let* ((handoff
                      (tool-execution-invoke
                       orchestrator child :tool-name "test.async" :summary "child result"
                       :operation-function (lambda () (tool-success "owned-result"))
                       :async-p            t))
                    (identifier (getf (getf (rest (tool-result-details handoff)) :job) :id)))
               (test-assert (and identifier
                                 (null (tool-registry-find child-registry "child" "spawn")))
                            "a non-spawning child can still own an execution job")
               (dolist (operation '("wait" "get"))
                 (let ((result
                         (tool-execute
                          (tool-registry-find child-registry "job" operation) context
                          (if (string= operation "wait")
                              (json-object "id" identifier "timeout-seconds" 10)
                              (json-object "id" identifier)))))
                   (test-assert (and (tool-result-success-p result)
                                     (search "owned-result" (tool-result-content result)))
                                (format nil "child job.~A returns its asynchronous result"
                                        operation))))
               (let* ((execution (getf (getf (rest (tool-result-details handoff)) :job) :execution-id))
                      (result    (tool-execute (tool-registry-find child-registry "job" "get") context
                                               (json-object "id" execution))))
                 (test-assert (and (stringp execution)
                                   (tool-result-success-p result)
                                   (search "owned-result" (tool-result-content result)))
                              "job.get also finds a job by the execution ID child.spawn reports")))
             (dolist (operation '("get" "wait" "cancel"))
               (test-assert
                (handler-case
                    (progn
                      (tool-execute
                       (tool-registry-find child-registry "job" operation) context
                       (json-object "id" (job-identifier child-job)))
                      nil)
                  (task-error () t))
                (format nil "child job.~A cannot operate on its parent-owned job" operation))))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-task-child-prompt-cache-key () null)
(defun test-task-child-prompt-cache-key ()
  "Test task children share one cache key that differs from their root's key."
  (let ((cases '(("root-1" "task:root-1")
                 ("g4P5Hsc" "task:g4P5Hsc"))))
    (loop for (root expected) in cases
          for key = (task--child-prompt-cache-key root)
          do (test-assert (string= key expected)
                          "children below one root derive one shared cache key")
             (test-assert (not (string= key root))
                          "the child cache key never equals the root's own key")))
  nil)

(-> test-task-participant-state () null)
(defun test-task-participant-state ()
  "Exercise real task setup through the shared execution factory and goal persistence."
  (with-test-configuration (configuration)
    (let* ((definition        (task-agent-definition-create
                               :name "state" :description "Check participant setup."
                               :instructions "Complete this assignment."
                               :tools '("resource.read" "resource.edit") :source ':test))
           (orchestrator      (task-tests--orchestrator))
           (provider          (make-instance 'task-test-provider :mode ':concurrent))
           (parent            (agent-create :configuration configuration :provider provider
                                            :tool-registry (make-default-tool-registry)))
           (job               (task-tests--make-job orchestrator :identifier "participant-task"
                                                   :definition definition :parent-agent parent
                                                   :item                     (list :task "Do the assignment.")))
           (factory           (fdefinition 'agent-create))
           (stream-turn       (fdefinition 'provider-stream-turn))
           (invoke-context    (fdefinition 'context--invoke-contributors))
           (principal         (fdefinition 'memory-resource--principal))
           (execute-call      (fdefinition 'tool-registry-execute-call))
           (observations-lock (make-lock "First child memory observations"))
           (state             nil)
           (request-count     0)
           (context-owners    nil)
           (tool-owners       nil)
           (tool-results      nil)
           (result            nil))
      (unwind-protect
           (progn
             (conversation-append-user-message (agent-conversation parent) "Parent assignment.")
             (setf (participant-state-identity (agent-participant-state parent))
                   (world-register-conversation configuration (agent-conversation parent)))
             (setf (slot-value job 'completion-owner-conversation)
                   (conversation-identifier (agent-conversation parent)))
             (with-lock-held ((cl-jobpond::job--lock job))
               (setf (job-state job) ':running))
             (test-call-with-function-replacements
              (list (list 'agent-create
                          (lambda (&rest arguments)
                            (let ((child (apply factory arguments)))
                              (when (typep child 'task-child-agent)
                                (setf state (agent-participant-state child)
                                      (participant-state-goal state)
                                      (mission--validate-specification
                                       '(:objective "ordinary participant mission") :final-marker-p t)))
                              child)))
                    (list 'context--invoke-contributors
                          (lambda (request registrations)
                            (push (request-context-participant request) context-owners)
                            (funcall invoke-context request registrations)))
                    (list 'memory-resource--principal
                          (lambda (context)
                            (multiple-value-bind (owner shared-write-p) (funcall principal context)
                              (with-lock-held (observations-lock)
                                (push (list owner shared-write-p) tool-owners))
                              (values owner shared-write-p))))
                    (list 'tool-registry-execute-call
                          (lambda (&rest arguments)
                            (let ((result (apply execute-call arguments)))
                              (with-lock-held (observations-lock)
                                (push result tool-results))
                              result)))
                    (list 'provider-stream-turn
                          (lambda (provider conversation &rest arguments)
                            (incf request-count)
                            (context-resolve-request configuration conversation #())
                            (labels ((call (uri name arguments)
                                       "Return one scripted memory tool call."
                                       (agent-test-call :call-id   (format nil "~A-~D" uri request-count)
                                                        :namespace "resource" :name name
                                                        :arguments (json-encode arguments)))

                                     (revision (uri)
                                       "Read the revision from the preceding actual tool result."
                                       (let* ((content (loop for result in tool-results
                                                             for text = (tool-result-content result)
                                                             when (search (format nil "URI: ~A~%" uri) text)
                                                               return text))
                                              (start   (+ (search "Revision: " content) 10)))
                                         (subseq content start (position #\Newline content :start start)))))
                              (case request-count
                                (1
                                 (agent-test-result
                                  "memory-first-read"
                                  (loop for uri in '("memory:personal" "memory:global")
                                        collect (call uri "read" (json-object "uri" uri)))))
                                (2
                                 (agent-test-result
                                  "memory-first-write"
                                  (loop for uri in '("memory:personal" "memory:global")
                                        collect
                                        (call uri "edit"
                                              (json-object
                                               "uri" uri "base-revision" (revision uri)
                                               "operations"
                                               (json-array
                                                (json-object "op" "memory-remember"
                                                             "title" "First-turn personal memory"
                                                             "content" "Personal state from the first child turn.")))))))
                                (otherwise
                                 (apply stream-turn provider conversation arguments)))))))
              (lambda () (setf result (task-run-child job))))
             (test-assert (and (eq (getf result :status) ':success)
                               (eq state (task-job-participant-state job))
                               (typep (participant-state-conversation state) 'conversation)
                               (eq definition (task-job-definition job)))
                          "pooled task setup shares the factory's actual state, keeping assignment metadata on the job")
             (test-assert
              (not (find (task-job-execution-identifier job) (conversation-list configuration)
                         :key #'pathname-name :test #'equal))
              "private child registration keeps the child out of the top-level conversation list")
             (let* ((identity       (participant-state-identity state))
                    (owner          (participant-identifier identity))
                    (*memory-owner* owner)
                    (personal       (first (memory-list configuration :visibility ':personal))))
               (test-assert
                (and (typep identity 'participant)
                     (equal owner (participant-identifier
                                   (world-register-conversation
                                    configuration (participant-state-conversation state))))
                     (equal owner (participant-identifier (first (last context-owners)))))
                "the first spawned provider context has the child's durable participant identity")
               (test-assert
                (and personal (equal owner (memory-owner personal))
                     (every (lambda (entry) (and (equal owner (first entry)) (second entry)))
                            tool-owners)
                     (= 4 (count-if #'tool-result-success-p tool-results)))
                (format nil "First child owner ~S, personal ~S, tool principals ~S, results ~S"
                        owner (and personal (memory-owner personal)) tool-owners
                        (mapcar (lambda (result) (list (tool-result-error-code result)
                                                      (tool-result-content result))) tool-results)))
               (let* ((world    (world-load configuration))
                      (reloaded (world-participant-find world owner))
                      (context  (make-instance
                                 'tool-context :configuration configuration :worker nil
                                 :agent (make-instance
                                         'agent :configuration configuration
                                         :participant-state
                                         (make-instance 'participant-state :identity reloaded)))))
                 (test-assert
                  (and (world-inbox-channel world)
                       (= 2 (length (conversation-list configuration)))
                       (equal (participant-parent reloaded)
                              (participant-identifier
                               (participant-state-identity (agent-participant-state parent))))
                       (equal owner (memory-owner (first (memory-list configuration :visibility ':personal)))))
                  "normal world bootstrap creates its inbox without changing child identity or personal ownership")
                 (multiple-value-bind (reloaded-owner shared-write-p) (memory-resource--principal context)
                  (test-assert (and (equal owner reloaded-owner) shared-write-p)
                               "child shared-write authority survives world bootstrap and runtime reconstruction"))))
             (test-assert
              (equal (goal-record->goal
                      (conversation-latest-goal-record
                       (conversation-load (conversation-pathname
                                           (participant-state-conversation state)))))
                     (participant-state-goal state))
              "task settlement persists the ordinary participant goal"))
        (lisp-worker-pool-stop-all (agent-worker parent)))))
  nil)

(-> test-task-child-assignment-provenance () null)
(defun test-task-child-assignment-provenance ()
  "Exercise initial assignment attribution through child execution, reload and replay."
  (with-test-configuration (configuration)
    (let* ((source       (conversation-create configuration :parent-identifier "ancestor"
                                                            :agent-name        "Original parent"))
           (provider     (make-instance 'task-test-provider :mode ':concurrent))
           (parent       (agent-create :configuration configuration :provider provider
                                       :conversation  source
                                       :tool-registry (make-default-tool-registry)))
           (orchestrator (task-tests--orchestrator))
           (definition   (task-find-agent-definition (task-bundled-agent-definitions) "task"))
           (assignment   "Complete the assigned review.")
           (job          (task-tests--make-job orchestrator :identifier "atlas-release-planner"
                                              :definition definition :parent-agent parent
                                              :root-identifier          "ancestor"
                                              :item                     (list :task assignment)))
           (application  (application-tests--ui-application :columns 100)))
      (unwind-protect
           (progn
             (conversation-append-user-message source "Delegate the review.")
             (let ((sender (world-register-conversation configuration source)))
               (world-participant-rename configuration (participant-identifier sender) "Dennis")
               (setf (participant-state-identity (agent-participant-state parent)) sender)
               (setf (slot-value job 'completion-owner-conversation) (conversation-identifier source))
               (with-lock-held ((cl-jobpond::job--lock job))
                 (setf (job-state job) ':running))
               (test-assert (eq (getf (task-run-child job) :status) ':success)
                            "the real child completes its initial assignment through an ordinary response")
               (let* ((conversation (participant-state-conversation (task-job-participant-state job)))
                      (identity     (conversation-pathname conversation)))
                 ;; Identical text from the person must retain its own provenance.
                 (conversation-append-user-message conversation assignment)
                 (labels ((check-records (records)
                            "Check agent and person attribution in one durable transcript view."
                            (let* ((messages (remove-if-not
                                              (lambda (record)
                                                (and (eq (first record) ':message)
                                                     (eq (getf (rest record) :role) ':user)))
                                              records))
                                   (initial  (find-if (lambda (record)
                                                        (getf (rest record) :operation-request)) messages))
                                   (personal (find-if (lambda (record)
                                                        (null (getf (rest record) :operation-request))) messages))
                                   (request  (getf (rest initial) :operation-request))
                                   (message  (getf request :message))
                                   (entry    (conversation-record-entry application initial))
                                   (name     (find ':agent-name (rest entry) :key #'terminal-span-style)))
                              (test-assert
                               (and (= (length messages) 2)
                                    (getf (rest initial) :automatic-p)
                                    (equal (getf (rest initial) :content) assignment)
                                    (equal (getf message :sender) (participant-identifier sender))
                                    (equal (getf message :sender-name) "Dennis")
                                    (equal (getf message :source) (conversation-identifier source))
                                    (equal (getf message :target) (conversation-identifier conversation)))
                               "assignment provenance identifies the immediate spawning agent, not the person or ancestor")
                              (test-assert (and name (string= (terminal-span-text name) "Dennis"))
                                           "the child transcript presents its assignment under the spawning agent")
                              (test-assert
                               (and personal
                                    (null (getf (rest personal) :automatic-p))
                                    (equal (conversation-record-entry application personal)
                                           (application--user-message-entry
                                            application assignment (getf (rest personal) :time))))
                               "identical genuine user input uses the person's own transcript presentation"))))
                   (check-records (conversation-records-newest conversation 100))
                   (check-records (conversation-records-newest (conversation-load identity) 100))
                   (check-records
                    (map 'list #'conversation-replay-record-record
                         (conversation-replay-session-records
                          (conversation-replay-create (conversation-replay-load-identity identity)))))))))
        (lisp-worker-pool-stop-all (agent-worker parent))
        (test-assert (task-orchestrator-close orchestrator)
                     "the fixture joins task writers before removing its state"))))
  nil)

(-> test-task-child-sends-to-parent () null)
(defun test-task-child-sends-to-parent ()
  "Send to @parent from a spawned child's first provider turn."
  (with-test-configuration (configuration)
    (let* ((provider     (make-instance 'task-test-provider :mode ':concurrent))
           (parent       (agent-create :configuration configuration :provider provider
                                       :tool-registry (make-default-tool-registry)))
           (conversation (agent-conversation parent))
           (orchestrator (task-tests--orchestrator))
           (definition   (task-find-agent-definition (task-bundled-agent-definitions) "task"))
           (job          (task-tests--make-job orchestrator
                                              :identifier   "send-to-parent"
                                              :definition   definition
                                              :parent-agent parent
                                              :item         (list :task "Send a message to your parent.")))
           (stream-turn  (fdefinition 'provider-stream-turn))
           (text         "Child delivery to its parent.")
           (sent-p       nil))
      (unwind-protect
           (progn
             (conversation-append-user-message conversation "Delegate a message.")
             (setf (participant-state-identity (agent-participant-state parent))
                   (world-register-conversation configuration conversation)
                   (slot-value job 'completion-owner-conversation)
                   (conversation-identifier conversation))
             (with-lock-held ((cl-jobpond::job--lock job))
               (setf (job-state job) ':running))
             (test-call-with-function-replacements
              (list (list 'provider-stream-turn
                          (lambda (provider conversation &rest arguments)
                            (if sent-p
                                (apply stream-turn provider conversation arguments)
                                (progn
                                  (setf sent-p t)
                                  (agent-test-result
                                   "send-to-parent"
                                   (list (agent-test-call
                                          :call-id "send-to-parent" :namespace "channel" :name "send"
                                          :arguments (json-encode
                                                      (json-object "channel" "@parent" "text" text))))))))))
              (lambda ()
                (test-assert (eq (getf (task-run-child job) :status) ':success)
                             "the spawned child completes the task")))
             (let* ((state    (task-job-participant-state job))
                    (child    (participant-state-identity state))
                    (target   (conversation-identifier conversation))
                    (delivery (find text (world-inbox-deliveries configuration target)
                                    :key #'world-delivery-text :test #'string=)))
               (test-assert (and delivery
                                 (equal (world-delivery-sender delivery) (participant-identifier child)))
                            "explicit @parent reaches the parent channel under the child's identity")))
        (lisp-worker-pool-stop-all (agent-worker parent))
        (test-assert (task-orchestrator-close orchestrator)
                     "the fixture joins task writers before removing its state"))))
  nil)
