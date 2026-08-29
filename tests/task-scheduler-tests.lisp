(in-package #:autolith)

;;;; -- Scheduler Boundary Tests --

(-> task-tests--release-blocking-tool (task-test-blocking-tool) null)
(defun task-tests--release-blocking-tool (tool)
  "Permit TOOL to return normally when a cancellation test is unwinding."
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-released-p tool) t)
    (task--condition-broadcast
     (task-test-blocking-tool-condition-variable tool)))
  nil)

(-> test-task-default-detachment () null)
(defun test-task-default-detachment ()
  "Test default detachment, explicit blocking, and forced blocking roles."
  (labels ((run-case (label arguments expected-detached-p &key blocking-role-p)
             (let* ((base-configuration (test-configuration))
                    (root (test-configuration-root base-configuration))
                    (configuration
                      (configuration--clone
                       base-configuration :working-directory root))
                    (registry (make-default-tool-registry))
                    (blocking-tool
                      (make-instance
                       'task-test-blocking-tool
                       :namespace "test"
                       :name "block"
                       :description "Wait until the detachment test releases this call."
                       :parameters (tool-object-schema (json-object) nil)))
                    (provider
                      (make-instance 'task-test-provider :mode ':blocking-tool))
                    (conversation nil)
                    (primary nil)
                    (run-tool nil)
                    (orchestrator nil)
                    (context nil)
                    (runner nil)
                    (run-result nil)
                    (run-condition nil))
               (when blocking-role-p
                 (task-tests--write-native-form
                  (merge-pathnames
                   ".autolith/agents/forced-blocking.sexp" root)
                  (task-tests--role-form
                   "forced-blocking"
                   "Always wait for this child."
                   "Enter the blocking test tool, then yield."
                   :tools ':all
                   :blocking-p t)))
               (tool-registry-register registry blocking-tool)
               (task-augment-tool-registry registry)
               (setf conversation (conversation-create configuration)
                     primary
                     (agent-create :configuration configuration
                                   :provider provider
                                   :conversation conversation
                                   :tool-registry registry
                                   :worker nil)
                     run-tool (tool-registry-find registry "task" "run")
                     orchestrator (task-run-tool-orchestrator run-tool)
                     context
                     (make-instance 'tool-context
                                    :configuration configuration
                                    :worker nil
                                    :conversation conversation
                                    :registry registry
                                    :agent primary
                                    :call-id label))
               (unwind-protect
                    (progn
                      (if expected-detached-p
                          (setf run-result
                                (tool-execute run-tool context arguments))
                          (setf runner
                                (make-thread
                                 (lambda ()
                                   (handler-case
                                       (setf run-result
                                             (tool-execute
                                              run-tool context arguments))
                                     (serious-condition (condition)
                                       (setf run-condition condition))))
                                 :name
                                 (format nil "Autolith ~A task wait test" label))))
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-blocking-tool-lock blocking-tool))
                            (task-test-blocking-tool-started-p blocking-tool)))
                        2)
                       (format nil "~A reaches the blocking child tool" label))
                      (let* ((jobs (task-orchestrator-list-jobs orchestrator))
                             (job (and (= (length jobs) 1) (first jobs))))
                        (test-assert
                         (and job
                              (eq (and (task-job-detached-p job) t)
                                  expected-detached-p)
                              (not (job-terminal-p job))
                              (if expected-detached-p
                                  (and (null runner)
                                       (typep run-result 'tool-result)
                                       (tool-result-success-p run-result))
                                  (and runner (thread-alive-p runner)
                                       (null run-result)
                                       (null run-condition))))
                         (format nil "~A obeys its effective waiting policy" label))
                        (task-tests--release-blocking-tool blocking-tool)
                        (when runner
                          (join-thread runner)
                          (setf runner nil))
                        (multiple-value-bind (snapshot terminal-p)
                            (task-job-await job 2)
                          (test-assert
                           (and terminal-p
                                (eq (getf snapshot :state) :completed)
                                (eq (getf (getf snapshot :result) :status)
                                    :success)
                                (null run-condition)
                                (typep run-result 'tool-result)
                                (tool-result-success-p run-result))
                           (format nil "~A completes after release" label)))))
                 (task-tests--release-blocking-tool blocking-tool)
                 (when runner
                   (join-thread runner))
                 (dolist (job (task-orchestrator-list-jobs orchestrator))
                   (unless (job-terminal-p job)
                     (task-job-cancel job :test-cleanup)
                     (task-job-await job 2)))
                 (ignore-errors (tool-registry-close-runtime-state registry))
                 (uiop:delete-directory-tree root :validate t
                                                  :if-does-not-exist ':ignore)))))
    (run-case
     "default-detached"
     (json-object "name" "default-detached"
                  "agent" "task"
                  "task" "Enter the blocking ordinary tool.")
     t)
    (run-case
     "explicit-blocking"
     (json-object "name" "explicit-blocking"
                  "agent" "task"
                  "task" "Enter the blocking ordinary tool."
                  "blocking" t)
     nil)
    (run-case
     "forced-blocking-role"
     (json-object "name" "forced-blocking-role"
                  "agent" "forced-blocking"
                  "task" "Enter the blocking ordinary tool.")
     nil
     :blocking-role-p t))
  nil)

(-> test-task-running-cancellation () null)
(defun test-task-running-cancellation ()
  "Test prompt cancellation while a child executes an ordinary tool call."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait until the cancellation test releases this call."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode ':blocking-tool))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "running-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "blocked-running-child"
                           "agent" "task"
                           "task" "Enter the blocking ordinary tool."
                           "async" t))
             (let ((job (first (task-orchestrator-list-jobs orchestrator))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held
                       ((task-test-blocking-tool-lock blocking-tool))
                     (task-test-blocking-tool-started-p blocking-tool)))
                 2)
                "the child reaches its ordinary tool call before cancellation")
               (let ((started-at (get-internal-real-time)))
                 (multiple-value-bind (accepted-p descendants)
                     (task-job-cancel job :user)
                   (declare (ignore descendants))
                   (multiple-value-bind (snapshot terminal-p)
                       (task-job-await job 2)
                     (test-assert
                      (and accepted-p
                           terminal-p
                           (eq (getf snapshot :state) :aborted)
                           (eq (getf (getf snapshot :result) :status)
                               :aborted)
                           (< (task--milliseconds-between
                               started-at (get-internal-real-time))
                              1000))
                      "running cancellation unwinds an ordinary tool call promptly"))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator))))
                 2)
                "cancelled ordinary tool execution releases scheduler accounting")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)

(-> test-task-runtime-deadline () null)
(defun test-task-runtime-deadline ()
  "Test the unlimited default and opt-in timeout of a stalled child."
  (let ((previous-runtime
          (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS")
           (let ((orchestrator (task-tests--orchestrator)))
             (test-assert
              (and
               (zerop
                (task-orchestrator-maximum-runtime-milliseconds orchestrator))
               (zerop *task-default-maximum-runtime-milliseconds*))
              "task children have no default runtime deadline"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000" 1)
           (let* ((configuration (test-configuration))
                  (root          (test-configuration-root configuration))
                  (registry      (make-default-tool-registry))
                  (blocking-tool
                    (make-instance
                     'task-test-blocking-tool
                     :namespace "test"
                     :name "block"
                     :description
                     "Wait until the deadline test releases this call."
                     :parameters (tool-object-schema (json-object) nil)))
                  (runner nil)
                  (run-result nil)
                  (run-condition nil))
             (tool-registry-register registry blocking-tool)
             (task-augment-tool-registry registry)
             (let* ((provider
                      (make-instance 'task-test-provider
                                     :mode ':blocking-tool))
                    (conversation (conversation-create configuration))
                    (primary
                      (agent-create :configuration configuration
                                    :provider provider
                                    :conversation conversation
                                    :tool-registry registry
                                    :worker nil))
                    (run-tool (tool-registry-find registry "task" "run"))
                    (orchestrator (task-run-tool-orchestrator run-tool))
                    (context
                      (make-instance
                       'tool-context
                       :configuration configuration
                       :worker nil
                       :conversation conversation
                       :registry registry
                       :agent primary
                       :call-id "runtime-deadline")))
               (unwind-protect
                    (progn
                      (setf runner
                            (make-thread
                             (lambda ()
                               (handler-case
                                   (setf run-result
                                         (tool-execute
                                          run-tool
                                          context
                                          (json-object
                                           "name" "stalled-child"
                                           "agent" "task"
                                           "task"
                                           "Enter the blocking ordinary tool."
                                           "blocking" t)))
                                 (serious-condition (condition)
                                   (setf run-condition condition))))
                             :name "Autolith task deadline test"))
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-blocking-tool-lock blocking-tool))
                            (task-test-blocking-tool-started-p blocking-tool)))
                        2)
                       "the child stalls inside a real ordinary tool call")
                      (test-assert
                       (task-tests--wait-until
                        (lambda () (not (thread-alive-p runner)))
                        3)
                       "the runtime deadline releases synchronous task.run")
                      (join-thread runner)
                      (setf runner nil)
                      (let* ((jobs
                               (task-orchestrator-list-jobs orchestrator))
                             (snapshot
                               (and (= (length jobs) 1)
                                    (task-job-snapshot (first jobs)))))
                        (test-assert
                         (and (null run-condition)
                              (typep run-result 'tool-result)
                              (not (tool-result-success-p run-result))
                              snapshot
                              (eq (getf snapshot :state) :aborted)
                              (eq (getf snapshot :cancellation-reason)
                                  :timeout)
                              (eq (getf (getf snapshot :result) :status)
                                  :aborted)
                              (task-tests--wait-until
                               (lambda ()
                                 (and
                                    (zerop
                                     (task-orchestrator-active-count
                                      orchestrator))
                                    (zerop
                                     (task-orchestrator-live-count
                                      orchestrator))))
                               2))
                         "a deadline publishes one timeout result and releases scheduler accounting")))
                 (task-tests--release-blocking-tool blocking-tool)
                 (when runner
                   (join-thread runner))
                 (ignore-errors
                   (tool-registry-close-runtime-state registry))
                 (uiop:delete-directory-tree root :validate t
                                                  :if-does-not-exist
                                                  :ignore)))))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS"
                          previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-nested-parent-cancellation () null)
(defun test-task-nested-parent-cancellation ()
  "Test parent cancellation while its synchronous descendant is running."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait inside a nested synchronous child."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode ':nested-cancel))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "nested-parent-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "nested-cancel-parent"
                           "agent" "task"
                           "task" "Delegate synchronously, then wait."
                           "async" t))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held
                     ((task-test-blocking-tool-lock blocking-tool))
                   (task-test-blocking-tool-started-p blocking-tool)))
               2)
              "the synchronous descendant reaches its blocking tool")
             (let* ((jobs (task-orchestrator-list-jobs orchestrator))
                    (parent (first jobs))
                    (descendant (second jobs))
                    (descendant-id
                      (job-identifier descendant)))
               (test-assert (= (length jobs) 2)
                            "nested cancellation observes parent and descendant")
               (multiple-value-bind (accepted-p descendants)
                   (task-job-cancel parent :user)
                 (test-assert
                  (and accepted-p
                       (equal descendants (list descendant-id)))
                  "parent cancellation reaches the live synchronous descendant"))
               (dolist (job jobs)
                 (multiple-value-bind (snapshot terminal-p)
                     (task-job-await job 2)
                   (test-assert
                    (and terminal-p
                         (eq (getf snapshot :state) :aborted)
                         (eq (getf (getf snapshot :result) :status)
                             :aborted))
                    "parent and synchronous descendant both terminalize as aborted")))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator))
                          (null (task-tests--queued-jobs orchestrator))))
                 2)
                "nested parent cancellation leaves no orphan or live-count leak")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)

(-> test-task-admission-cancellation-barrier () null)
(defun test-task-admission-cancellation-barrier ()
  "Test that a child admitted after a cascade scanned is still stopped.

Admission no longer holds the parent's lifecycle lock, so a child can be admitted
after a cascading cancellation has already walked the subtree. What closes that
window is the ancestry check a child makes before it runs, and this exercises
exactly that race."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "admission-race"
            :description "Exercise admission and cancellation ordering."
            :instructions "Remain queued for the scheduler race."
            :spawns ':all
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "admission-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-tests--orchestrator))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition :name "race-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entries
                    (list
                     (list
                      :definition definition
                      :item
                      (list :name "racing-descendant"
                            :agent "admission-race"
                            :task "Be admitted before cancellation scans."
                            :context nil
                            :async t)
                      :detached t)))
                  (pause-lock (make-lock "Autolith admission race pause"))
                  (pause-condition (make-condition-variable))
                  (admission-paused-p nil)
                  (release-admission-p nil)
                  (admitted nil)
                  (admission-failure nil)
                  (cancel-accepted-p nil)
                  (cancelled-descendants nil)
                  (admission-thread nil)
                  (original-owner-identifiers
                    (symbol-function 'task-parent-owner-identifiers)))
             (unwind-protect
                  (test-call-with-function-replacements
                   (list
                    (list
                     'task-parent-owner-identifiers
                     (lambda (candidate)
                       (let ((identifiers
                               (funcall original-owner-identifiers candidate)))
                         (when (eq candidate viewer)
                           (with-lock-held (pause-lock)
                             (setf admission-paused-p t)
                             (task--condition-broadcast pause-condition)
                             (loop until release-admission-p
                                   do (condition-wait
                                       pause-condition pause-lock))))
                         identifiers))))
                   (lambda ()
                     (setf admission-thread
                           (make-thread
                            (lambda ()
                              (handler-case
                                  (setf admitted
                                        (first
                                         (multiple-value-list
                                          (task-orchestrator-start-jobs
                                           orchestrator viewer entries))))
                                (condition (condition)
                                  (setf admission-failure condition))))
                            :name "Autolith admission race"))
                     (with-lock-held (pause-lock)
                       (loop until admission-paused-p
                             unless
                             (condition-wait
                              pause-condition pause-lock :timeout 2)
                               do (error
                                   "Timed out waiting for admission pause.")))
                     (test-assert
                      (null admitted)
                      "nested admission pauses after its parent check")
                     (multiple-value-setq
                         (cancel-accepted-p cancelled-descendants)
                       (task-job-cancel parent :user))
                     (test-assert
                      (and cancel-accepted-p
                           (null cancelled-descendants)
                           (eq (job-state parent) :aborted))
                      "cancellation completes its subtree scan before admission")
                     (with-lock-held (pause-lock)
                       (setf release-admission-p t)
                       (task--condition-broadcast pause-condition))
                     (join-thread admission-thread)
                     (setf admission-thread nil)
                     (let ((child (first admitted)))
                       (test-assert
                        (and (null admission-failure)
                             (= (length admitted) 1)
                             (member
                              (job-identifier parent)
                              (job-owner-identifiers child)
                              :test #'string=))
                        "a child can be admitted after the cascade has scanned")
                       (test-assert
                        (task-tests--wait-until
                         (lambda () (job-terminal-p child))
                         5)
                        "a child admitted after the cascade still terminalizes")
                       (test-assert
                        (and (eq (job-state child) :aborted)
                             (search "with its parent"
                                     (or (getf (job-result child) :error) "")))
                        "its cancelled ancestor stops it before it does any work")
                       (test-assert
                        (task-tests--wait-until
                         (lambda ()
                           (zerop (task-orchestrator-live-count orchestrator)))
                         5)
                        "the raced admission leaks no live capacity"))))
               (with-lock-held (pause-lock)
                 (setf release-admission-p t)
                 (task--condition-broadcast pause-condition))
               (when admission-thread
                 (join-thread admission-thread))))
           (let* ((orchestrator (task-tests--orchestrator))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "cancel-first-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entry
                    (list
                     :definition definition
                     :item
                     (list :name "too-late"
                           :agent "admission-race"
                           :task "This child must not be admitted."
                           :context nil
                           :async t)
                     :detached t)))
             (task-job-cancel parent :user)
             (test-assert
              (handler-case
                  (progn
                    (task-orchestrator-start-jobs
                     orchestrator viewer (list entry))
                    nil)
                (job-aborted ()
                  t))
              "admission loses atomically when parent cancellation wins")
             (test-assert
              (and (= (hash-table-count
                       (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                      1)
                   (zerop (task-orchestrator-live-count orchestrator))
                   (= (cl-jobpond::job-pool--next-index (task-orchestrator-pool orchestrator)) 1))
              "cancel-first admission consumes no identity or live capacity")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-hurry-up-admission-races () null)
(defun test-task-hurry-up-admission-races ()
  "Test hurry-up reservations and pool-limit reconfiguration ordering."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "hurry-race"
            :description "Exercise hurry-up admission serialization."
            :instructions "Remain inline for the admission test."
            :spawns ':all
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "hurry-race-primary")))
    (labels ((make-viewer (orchestrator identifier)
               (task-tests--child-viewer
                configuration
                (task-tests--make-job
                 orchestrator
                 :definition definition
                 :item
                 (list :name identifier
                       :agent "hurry-race"
                       :task "Own inline admission test children."
                       :context nil
                       :async nil)
                 :parent-agent primary
                 :identifier identifier)))

             (make-entries (count prefix)
               (loop for index from 1 to count
                     collect
                     (list
                      :definition definition
                      :item
                      (list :name (format nil "~A-~D" prefix index)
                            :agent "hurry-race"
                            :task "Remain inline for admission accounting."
                            :context nil
                            :async nil)
                      :detached nil))))
      (unwind-protect
           (progn
             (let* ((orchestrator (task-tests--orchestrator))
                    (pool (task-orchestrator-pool orchestrator)))
               (task-orchestrator-set-hurry-up orchestrator nil)
               (test-assert
                (and (null (cl-jobpond::job-pool--worker-threads pool))
                     (null (cl-jobpond::job-pool--monitor-thread pool)))
                "reapplying inactive hurry-up keeps an unused task pool lazy"))
             (let* ((orchestrator (task-tests--orchestrator))
                    (viewer (make-viewer orchestrator "rollback-parent"))
                    (pool (task-orchestrator-pool orchestrator))
                    (refused-p nil))
               (task-orchestrator-set-hurry-up orchestrator t)
               (with-lock-held ((task-orchestrator-lock orchestrator))
                 (setf (job-pool-maximum-live-jobs pool) 1))
               (handler-case
                   (task-orchestrator-start-jobs
                    orchestrator viewer (make-entries 2 "refused"))
                 (task-error ()
                   (setf refused-p t)))
               (test-assert
                (and refused-p
                     (zerop
                      (task-orchestrator-hurry-up-admission-count orchestrator))
                     (zerop (task-orchestrator-live-count orchestrator)))
                "failed pool admission rolls back its hurry-up reservation")
               (with-lock-held ((task-orchestrator-lock orchestrator))
                 (setf (job-pool-maximum-live-jobs pool)
                       *task-hurry-up-maximum-agents*))
               (let ((jobs
                       (first
                        (multiple-value-list
                         (task-orchestrator-start-jobs
                          orchestrator viewer (make-entries 2 "accepted"))))))
                 (test-assert
                  (and (= (length jobs) 2)
                       (= (task-orchestrator-hurry-up-admission-count
                           orchestrator)
                          2)
                       (= (task-orchestrator-live-count orchestrator) 2))
                  "a rolled-back reservation leaves the full allowance usable")))
             (let* ((orchestrator (task-tests--orchestrator))
                    (viewer (make-viewer orchestrator "concurrent-parent"))
                    (pool (task-orchestrator-pool orchestrator))
                    (gate-lock (make-lock "Autolith hurry race gate"))
                    (gate-condition (make-condition-variable))
                    (gate-count 0)
                    (release-gate-p nil)
                    (result-lock (make-lock "Autolith hurry race results"))
                    (success-count 0)
                    (conditions nil)
                    (threads nil)
                    (original-configuration
                      (symbol-function 'task-configuration-for-definition)))
               (task-orchestrator-set-hurry-up orchestrator t)
               ;; Isolate the cumulative session allowance from the pool's live
               ;; bound. Inline jobs remain live until test cleanup.
               (with-lock-held ((task-orchestrator-lock orchestrator))
                 (setf (job-pool-maximum-live-jobs pool)
                       *task-maximum-live-jobs*))
               (test-call-with-function-replacements
                (list
                 (list
                  'task-configuration-for-definition
                  (lambda (parent-configuration child-definition)
                    (with-lock-held (gate-lock)
                      (incf gate-count)
                      (when (= gate-count 3)
                        (setf release-gate-p t)
                        (task--condition-broadcast gate-condition))
                      (loop until release-gate-p
                            unless
                            (condition-wait gate-condition gate-lock :timeout 2)
                              do (error
                                  "Timed out synchronizing hurry-up admissions.")))
                    (funcall original-configuration
                             parent-configuration child-definition))))
                (lambda ()
                  (setf threads
                        (loop for index from 1 to 3
                              collect
                              (make-thread
                               (lambda ()
                                 (handler-case
                                     (progn
                                       (task-orchestrator-start-jobs
                                        orchestrator viewer
                                        (make-entries
                                         1 (format nil "concurrent-~D" index)))
                                       (with-lock-held (result-lock)
                                         (incf success-count)))
                                   (task-error (condition)
                                     (with-lock-held (result-lock)
                                       (push condition conditions)))))
                               :name (format nil
                                             "Autolith hurry admission ~D"
                                             index))))
                  (dolist (thread threads)
                    (join-thread thread))))
               (test-assert
                (and (= success-count 2)
                     (= (length conditions) 1)
                     (= (task-orchestrator-hurry-up-admission-count orchestrator)
                        2)
                     (= (task-orchestrator-live-count orchestrator) 2))
                "concurrent task.run calls atomically share the two-child allowance"))
             (let* ((orchestrator (task-tests--orchestrator))
                    (viewer (make-viewer orchestrator "limit-parent"))
                    (submit-lock (make-lock "Autolith limit race submit"))
                    (submit-condition (make-condition-variable))
                    (submission-entered-p nil)
                    (release-submission-p nil)
                    (state-lock (make-lock "Autolith limit race state"))
                    (reconfiguration-started-p nil)
                    (reconfiguration-finished-p nil)
                    (jobs nil)
                    (submission-condition nil)
                    (submission-thread nil)
                    (reconfiguration-thread nil)
                    (original-submit (symbol-function 'job-pool-submit-batch)))
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list
                       'job-pool-submit-batch
                       (lambda (pool entries)
                         (with-lock-held (submit-lock)
                           (setf submission-entered-p t)
                           (task--condition-broadcast submit-condition)
                           (loop until release-submission-p
                                 do (condition-wait
                                     submit-condition submit-lock)))
                         (funcall original-submit pool entries))))
                     (lambda ()
                       (setf submission-thread
                             (make-thread
                              (lambda ()
                                (handler-case
                                    (setf jobs
                                          (first
                                           (multiple-value-list
                                            (task-orchestrator-start-jobs
                                             orchestrator viewer
                                             (make-entries 3 "limit")))))
                                  (condition (condition)
                                    (setf submission-condition condition))))
                              :name "Autolith limit race admission"))
                       (with-lock-held (submit-lock)
                         (loop until submission-entered-p
                               unless
                               (condition-wait
                                submit-condition submit-lock :timeout 2)
                                 do (error
                                     "Timed out waiting for pool submission.")))
                       (setf reconfiguration-thread
                             (make-thread
                              (lambda ()
                                (with-lock-held (state-lock)
                                  (setf reconfiguration-started-p t))
                                (task-orchestrator-set-hurry-up orchestrator t)
                                (with-lock-held (state-lock)
                                  (setf reconfiguration-finished-p t)))
                              :name "Autolith limit race reconfiguration"))
                       (test-assert
                        (task-tests--wait-until
                         (lambda ()
                           (with-lock-held (state-lock)
                             reconfiguration-started-p))
                         2)
                        "limit reconfiguration starts during pool submission")
                       (sleep 0.1)
                       (test-assert
                        (with-lock-held (state-lock)
                          (not reconfiguration-finished-p))
                        "limit reconfiguration waits for in-flight admission")
                       (with-lock-held (submit-lock)
                         (setf release-submission-p t)
                         (task--condition-broadcast submit-condition))
                       (join-thread submission-thread)
                       (setf submission-thread nil)
                       (join-thread reconfiguration-thread)
                       (setf reconfiguration-thread nil)))
                 (with-lock-held (submit-lock)
                   (setf release-submission-p t)
                   (task--condition-broadcast submit-condition))
                 (when submission-thread
                   (join-thread submission-thread))
                 (when reconfiguration-thread
                   (join-thread reconfiguration-thread)))
               (test-assert
                (and (null submission-condition)
                     (= (length jobs) 3)
                     (task-orchestrator-hurry-up-p orchestrator)
                     (= (task-orchestrator-hurry-up-admission-count orchestrator)
                        3))
                "a limit change takes effect after the atomic submission boundary")))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)

(-> task-tests--release-publication-barrier
    (task-test-publication-barrier)
    null)
(defun task-tests--release-publication-barrier (barrier)
  "Permit BARRIER's artifact printer to finish or signal its test failure."
  (with-lock-held ((task-test-publication-barrier-lock barrier))
    (setf (task-test-publication-barrier-released-p barrier) t)
    (task--condition-broadcast
     (task-test-publication-barrier-condition-variable barrier)))
  nil)

(-> test-task-publication-coherence () null)
(defun test-task-publication-coherence ()
  "Test coherent snapshots, forced failure, and terminal role compaction."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (secret
           "AUTOLITH-TERMINAL-ROLE-INSTRUCTION-SENTINEL-71D21A")
         (definition
           (task-agent-definition-create
            :name "publication"
            :description "Exercise terminal publication."
            :instructions secret
            :tools ':all
            :spawns ':all
            :models '("@task")
            :reasoning-effort ':high
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "publication-primary")))
    (unwind-protect
         (progn
           (let* ((job
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "coherent-publication"))
                  (barrier (make-instance 'task-test-publication-barrier))
                  (result
                    (task-tests--terminal-result
                     job :status ':success :output "published"))
                  (publication-result nil)
                  (publication-condition nil))
             (setf (getf result :publication-barrier) barrier
                   (getf result :portable-integer) 42)
             (let ((thread
                     (make-thread
                      (lambda ()
                        (let ((*print-base* 2)
                              (*print-radix* nil)
                              (*print-case* :downcase))
                          (handler-case
                              (setf publication-result
                                    (task-tests--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition)))))
                      :name "Autolith coherent publication")))
               (unwind-protect
                    (progn
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "terminal publication reaches its post-claim artifact phase")
                      (dotimes (index 32)
                        (declare (ignore index))
                        (let ((snapshot (task-job-snapshot job)))
                          (test-assert
                           (and (eq (getf snapshot :state) :queued)
                                (null (getf snapshot :result)))
                           "a concurrent snapshot never exposes a partial terminal result")))
                      (test-assert
                       (with-lock-held ((cl-jobpond::job--lock job))
                         (cl-jobpond::job--publication-claimed-p job))
                       "snapshot sampling occurs while terminal publication is claimed"))
                 (task-tests--release-publication-barrier barrier))
               (join-thread thread))
             (let* ((snapshot (task-job-snapshot job))
                    (summary (task-job-definition-summary job))
                    (result-summary
                      (getf (getf snapshot :result) :agent-definition))
                    (artifact
                      (task-tests--read-exact-native-value
                       (uiop:read-file-string
                        (getf (getf snapshot :result) :output-path)))))
               (test-assert
                (and publication-result
                     (null publication-condition)
                     (eq (getf snapshot :state) :completed)
                     (eq (getf (getf snapshot :result) :status) :success)
                     (null (cl-jobpond::job--publication-claimed-p job))
                     (cl-jobpond::job--retained-p job)
                     (zerop (task-orchestrator-live-count orchestrator))
                     (listp artifact)
                     (= (getf artifact :portable-integer) 42))
                "publication moves atomically from a public live snapshot to terminal state")
               (test-assert
                (and (null (task-job-definition job))
                     summary
                     (equal summary result-summary)
                     (null (member :instructions summary :test #'eq))
                     (null
                      (search
                       secret
                       (task--write-readable-sexp
                        (list summary result-summary)))))
                "terminal jobs discard full role definitions and instruction text")))
           (dolist (failure '(:error :abort))
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "publication-~A" failure)))
                    (barrier
                      (make-instance
                       'task-test-publication-barrier
                       :failure failure))
                    (result
                      (task-tests--terminal-result
                       job
                       :status ':success
                       :output (make-string 3000 :initial-element #\O)))
                    (publication-result nil)
                    (publication-condition nil))
               (setf (getf result :publication-barrier) barrier
                     (getf result :structured-output-present-p) t
                     (getf result :structured-output)
                     (make-string 3000 :initial-element #\S))
               (let ((thread
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf publication-result
                                    (task-tests--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition))))
                        :name
                        (format nil "Autolith publication ~A" failure))))
                 (unwind-protect
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "failing publication reaches the post-claim barrier")
                   (task-tests--release-publication-barrier barrier))
                 (join-thread thread))
               (let* ((snapshot (task-job-snapshot job))
                      (terminal-result (getf snapshot :result))
                      (output-path (getf terminal-result :output-path)))
                 (test-assert
                  (and publication-result
                       (null publication-condition)
                       (eq (getf snapshot :state) :failed)
                       (eq (getf terminal-result :status) :failed)
                       (cl-jobpond::job--retained-p job)
                       (null (cl-jobpond::job--publication-claimed-p job))
                       (null (task-job-definition job))
                       (not
                        (and (null output-path)
                             (or
                              (eq (getf terminal-result :output-storage)
                                  :artifact)
                              (eq
                               (getf terminal-result
                                     :structured-output-storage)
                               :artifact)
                              (eq (getf terminal-result :error-storage)
                                  :artifact))))
                       (zerop
                        (task-orchestrator-live-count orchestrator)))
                  (format nil
                          "post-claim ~A forces one coherent terminal failure"
                          failure))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-terminal-wakeup-ordering () null)
(defun test-task-terminal-wakeup-ordering ()
  "Test that terminal waiters wake before arbitrary lifecycle listeners run."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition
           (task-agent-definition-create
            :name "wakeup-order"
            :description "Exercise terminal wakeup ordering."
            :instructions "Publish one result."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "wakeup-primary"))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "wakeup-job"))
         (result
           (task-tests--terminal-result
            job :status ':success :output "wake the waiter"))
         (ready-lock (make-lock "Autolith waiter readiness"))
         (ready-condition (make-condition-variable))
         (waiter-ready-p nil)
         (waiter-returned-p nil)
         (listener-lock (make-lock "Autolith blocking lifecycle listener"))
         (listener-condition (make-condition-variable))
         (listener-reached-p nil)
         (listener-released-p nil)
         (waiter nil)
         (publisher nil))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (declare (ignore payload))
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (listener-lock)
           (setf listener-reached-p t)
           (task--condition-broadcast listener-condition)
           (loop until listener-released-p
                 do (condition-wait listener-condition listener-lock))))))
    (unwind-protect
         (progn
           (setf waiter
                 (make-thread
                  (lambda ()
                    (with-lock-held ((cl-jobpond::job--lock job))
                      (with-lock-held (ready-lock)
                        (setf waiter-ready-p t)
                        (task--condition-broadcast ready-condition))
                      (loop until
                            (task--terminal-state-p
                             (job-state job))
                            do (condition-wait
                                (cl-jobpond::job--condition-variable job)
                                (cl-jobpond::job--lock job))))
                    (setf waiter-returned-p t))
                  :name "Autolith terminal waiter"))
           (test-assert
            (task-tests--wait-until (lambda () waiter-ready-p) 2)
            "the terminal waiter is parked before publication")
           (setf publisher
                 (make-thread
                  (lambda ()
                    (task-tests--publish-terminal job :completed result))
                  :name "Autolith listener-blocked publisher"))
           (test-assert
            (task-tests--wait-until (lambda () listener-reached-p) 2)
            "terminal publication reaches the blocking lifecycle listener")
           (let ((woke-before-listener-release-p
                   (task-tests--wait-until
                    (lambda () waiter-returned-p)
                    0.5)))
             (with-lock-held (listener-lock)
               (setf listener-released-p t)
               (task--condition-broadcast listener-condition))
             (join-thread publisher)
             (setf publisher nil)
             (join-thread waiter)
             (setf waiter nil)
             (test-assert
              woke-before-listener-release-p
              "terminal publication wakes waiters before invoking listeners")))
      (with-lock-held (listener-lock)
        (setf listener-released-p t)
        (task--condition-broadcast listener-condition))
      (when publisher
        (join-thread publisher))
      (when waiter
        (with-lock-held ((cl-jobpond::job--lock job))
          (task--condition-broadcast (cl-jobpond::job--condition-variable job)))
        (join-thread waiter))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-job-visibility () null)
(defun test-task-job-visibility ()
  "Test conversation ownership, child ancestry, and opaque job lookup errors."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (missing-orchestrator (task-tests--orchestrator))
         (definition
           (task-agent-definition-create
            :name "visibility"
            :description "Exercise task-tree visibility."
            :instructions "Inspect only descendant task jobs."
            :source ':test))
         (primary-a
           (task-tests--primary-agent configuration "visibility-primary-a"))
         (primary-b
           (task-tests--primary-agent configuration "visibility-primary-b")))
    (unwind-protect
         (let* ((root-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "root-a"))
                (viewer-a
                  (task-tests--child-viewer configuration root-a))
                (descendant-a
                  (task-tests--register-job
                   orchestrator viewer-a definition :name "descendant-a"))
                (sibling-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "sibling-a"))
                (sibling-viewer
                  (task-tests--child-viewer configuration sibling-a))
                (sibling-descendant
                  (task-tests--register-job
                   orchestrator sibling-viewer definition
                   :name "sibling-descendant"))
                (foreign
                  (task-tests--register-job
                   orchestrator primary-b definition :name "foreign"))
                (root-a-id (job-identifier root-a))
                (descendant-a-id
                  (job-identifier descendant-a))
                (sibling-a-id (job-identifier sibling-a))
                (sibling-descendant-id
                  (job-identifier sibling-descendant))
                (foreign-id (job-identifier foreign)))
           (test-assert
            (equal
             (mapcar
              (lambda (job) (job-identifier job))
              (task-orchestrator-list-visible-jobs orchestrator primary-a))
             (list root-a-id descendant-a-id
                   sibling-a-id sibling-descendant-id))
            "a primary sees only jobs owned by its conversation")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (job-identifier job))
              (task-orchestrator-list-visible-jobs orchestrator primary-b))
             (list foreign-id))
            "another primary cannot see the first conversation's task tree")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (job-identifier job))
              (task-orchestrator-list-visible-jobs orchestrator viewer-a))
             (list descendant-a-id))
            "a child sees descendants but not itself, its parent, or siblings")
           (dolist (operation '("wait" "cancel"))
             (let ((invisible-report
                     (task-tests--job-tool-error-report
                      orchestrator primary-a
                      :operation operation
                      :identifier foreign-id))
                   (missing-report
                     (task-tests--job-tool-error-report
                      missing-orchestrator primary-a
                      :operation operation
                      :identifier foreign-id)))
               (test-assert
                (and (string= invisible-report missing-report)
                     (search "No visible job" invisible-report))
                (format nil
                        "job.~A does not disclose whether an invisible identifier exists"
                        operation))))
           (test-assert
            (and (eq (job-state foreign) :queued)
                 (null (job-cancellation-reason foreign)))
            "invisible wait and cancel attempts cannot mutate the job"))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)


(-> test-task-durable-job-lookup () null)
(defun test-task-durable-job-lookup ()
  "Test live inspection and conversation-scoped durable result lookup."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (task-augment-tool-registry
                         (make-default-tool-registry)))
         (run-tool      (tool-registry-find registry "task" "run"))
         (get-tool      (tool-registry-find registry "job" "get"))
         (orchestrator  (task-run-tool-orchestrator run-tool))
         (definition
           (task-agent-definition-create
            :name "durable-lookup"
            :description "Exercise durable result lookup."
            :instructions "Remain queued for live inspection."
            :source ':test))
         (primary-a
           (task-tests--primary-agent
            configuration "durable-lookup-primary-a" registry))
         (primary-b
           (task-tests--primary-agent
            configuration "durable-lookup-primary-b" registry))
         (live-job
           (task-tests--register-job
            orchestrator primary-a definition :name "live-lookup"))
         (child
           (task-tests--child-viewer
            configuration live-job :registry registry))
         (execution-identifier (make-identifier))
         (artifact-path
           (merge-pathnames
            (format nil "~A/result.sexp" execution-identifier)
            (task--artifact-group-root
             configuration
             (conversation-identifier (agent-conversation primary-a)))))
         (durable-result
           (list :id "resumed-child-1"
                 :name "resumed-child"
                 :agent "task"
                 :status ':success
                 :output "Recovered durable child output."
                 :error nil
                 :yielded-p t
                 :structured-output-present-p nil
                 :duration-ms 17
                 :model "test-model"
                 :detached t)))
    (labels ((execute-get (viewer identifier)
               (tool-execute
                get-tool
                (make-instance
                 'tool-context
                 :configuration configuration
                 :worker nil
                 :conversation (agent-conversation viewer)
                 :registry registry
                 :agent viewer
                 :call-id "durable-job-lookup")
                (json-object "id" identifier))))
      (unwind-protect
           (progn
             (snapshot-write artifact-path durable-result)
             (let* ((result
                      (execute-get primary-a (session-job-identifier live-job)))
                    (details (rest (tool-result-details result)))
                    (record (getf details :job)))
               (test-assert
                (and (tool-result-success-p result)
                     (null (getf details :durable-p))
                     (string= (getf record :id)
                              (session-job-identifier live-job))
                     (eq (getf record :state) :queued))
                "job.get inspects a retained live session job"))
             (let* ((result (execute-get primary-a execution-identifier))
                    (details (rest (tool-result-details result)))
                    (record (getf details :job))
                    (result-record (getf record :result))
                    (artifact (getf result-record :artifact)))
               (test-assert
                (and (tool-result-success-p result)
                     (getf details :durable-p)
                     (string= (getf record :id) "resumed-child-1")
                     (string= (getf record :execution-id)
                              execution-identifier)
                     (eq (getf record :state) :completed)
                     (eq (getf result-record :status) :success)
                     (string= (getf result-record :output)
                              "Recovered durable child output.")
                     (string= (getf artifact :path)
                              (namestring artifact-path))
                     (getf artifact :available-p))
                "job.get reconstructs a bounded terminal record from its artifact"))
             (dolist (viewer (list primary-b child))
               (let ((report
                       (task-tests--job-tool-error-report
                        orchestrator viewer
                        :operation "get"
                        :identifier execution-identifier)))
                 (test-assert
                  (search "No visible job" report)
                  "durable lookup does not expose another conversation or task branch"))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)


(-> test-session-tool-execution-jobs () null)
(defun test-session-tool-execution-jobs ()
  "Test shared ordering, visibility, inspection, waiting, and cancellation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (definition
           (task-agent-definition-create
            :name "execution-owner"
            :description "Own one asynchronous tool execution."
            :instructions "Remain available while the tool is inspected."
            :source ':test))
         (primary-a
           (task-tests--primary-agent
            configuration "execution-primary-a" registry))
         (primary-b
           (task-tests--primary-agent
            configuration "execution-primary-b" registry))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation (agent-conversation primary-a)
                          :registry registry
                          :agent primary-a
                          :call-id "execution-job-test"))
         (barrier
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "execution-block"
            :description "Hold one execution job for inspection."
            :parameters (tool-object-schema (json-object) nil)))
         (root-job nil)
         (execution-job nil)
         (operation-count 0))
    (labels ((execute-job-tool (name arguments)
               (tool-execute
                (tool-registry-find registry "job" name)
                context
                arguments)))
      (unwind-protect
           (progn
             (setf root-job
                   (task-tests--register-job
                    orchestrator primary-a definition :name "execution-root"))
             (let ((child
                     (task-tests--child-viewer
                      configuration root-job :registry registry)))
               (setf execution-job
                     (task-orchestrator-start-execution-job
                      orchestrator child
                      :tool-name "lisp.eval"
                      :summary "Wait inside one inspectable execution job."
                      :operation-function
                      (lambda ()
                        (incf operation-count)
                        (tool-execute barrier context (json-object)))
                      :detached-p t
                      :parent-call-id "execution-job-test"))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held ((task-test-blocking-tool-lock barrier))
                     (task-test-blocking-tool-started-p barrier)))
                 2)
                "the asynchronous operation starts on the execution pool")
               (let ((root-id (session-job-identifier root-job))
                     (execution-id (session-job-identifier execution-job)))
                 (test-assert
                  (and (string= execution-id
                                (format nil "exec:~D"
                                        (session-job-order execution-job)))
                       (not (string= root-id execution-id))
                       (equal
                        (mapcar #'session-job-identifier
                                (task-orchestrator-list-jobs orchestrator))
                        (list root-id execution-id)))
                  "child and execution jobs have distinct globally ordered identities")
                 (test-assert
                  (equal
                   (mapcar #'session-job-identifier
                           (task-orchestrator-list-visible-jobs
                            orchestrator primary-a))
                   (list root-id execution-id))
                  "the owning primary sees task and execution jobs")
                 (test-assert
                  (equal
                   (mapcar #'session-job-identifier
                           (task-orchestrator-list-visible-jobs
                            orchestrator child))
                   (list execution-id))
                  "a child sees its execution job but not its own task job")
                 (test-assert
                  (null
                   (task-orchestrator-list-visible-jobs
                    orchestrator primary-b))
                  "another primary cannot inspect the execution job")
                 (let* ((list-result
                          (execute-job-tool
                           "list" (json-object "offset" 0 "limit" 32)))
                        (records
                          (getf (rest (tool-result-details list-result)) :jobs)))
                   (test-assert
                    (and (tool-result-success-p list-result)
                         (equal (mapcar (lambda (record) (getf record :id))
                                        records)
                                (list root-id execution-id))
                         (equal (mapcar (lambda (record) (getf record :type))
                                        records)
                                '(:task :tool))
                         (string= (getf (second records) :tool) "lisp.eval"))
                    "job.list presents child and tool execution jobs"))
                  (let* ((inspect-result
                           (execute-job-tool
                            "wait"
                            (json-object "id" execution-id
                                         "timeout-seconds" 0)))
                         (record (getf (rest (tool-result-details inspect-result))
                                       :job)))
                    (test-assert
                     (and (tool-result-success-p inspect-result)
                          (eq (getf record :type) :tool)
                          (eq (getf record :state) :running)
                          (string= (getf record :tool) "lisp.eval"))
                     "job.wait with zero timeout renders a live execution job"))
                 (let* ((wait-result
                          (execute-job-tool
                           "wait"
                           (json-object "id" execution-id
                                        "timeout-seconds" 0)))
                        (details (rest (tool-result-details wait-result))))
                   (test-assert
                    (and (tool-result-success-p wait-result)
                         (not (getf details :terminal-p))
                         (eq (getf (getf details :job) :state) :running))
                    "job.wait returns a nonterminal execution snapshot on timeout"))
                 (let* ((cancel-result
                          (execute-job-tool
                           "cancel" (json-object "id" root-id)))
                        (details (rest (tool-result-details cancel-result))))
                   (test-assert
                    (and (tool-result-success-p cancel-result)
                         (getf details :accepted-p)
                         (member execution-id
                                 (getf details :cancelled-descendants)
                                 :test #'string=))
                    "cancelling a task job also cancels owned execution jobs"))
                 (multiple-value-bind (snapshot terminal-p)
                     (session-job-await execution-job 2)
                   (test-assert
                    (and terminal-p
                         (= operation-count 1)
                         (eq (getf snapshot :state) :aborted)
                         (eq (getf (getf snapshot :result) :status) :aborted)
                         (null
                          (tool-execution-job-operation-function execution-job)))
                    "the execution runs once, aborts, and releases its closure")))))
        (task-tests--release-blocking-tool barrier)
        (when (and execution-job (not (job-terminal-p execution-job)))
          (session-job-cancel execution-job :test-cleanup)
          (session-job-await execution-job 2))
        (when (and root-job (not (job-terminal-p root-job)))
          (task-job-cancel root-job :test-cleanup)
          (task-job-await root-job 2))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)


(-> test-shell-execution-jobs () null)
(defun test-shell-execution-jobs ()
  "Test authorized shell jobs, automatic handoff, inspection, and cancellation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (shell-tool (tool-registry-find registry "shell" "run"))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "shell-execution-primary" registry))
         (authorization-count 0)
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker nil
            :conversation (agent-conversation primary)
            :registry registry
            :agent primary
            :call-id "shell-execution-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              (incf authorization-count)
              ':full-access)))
         (denied-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker nil
            :conversation (agent-conversation primary)
            :registry registry
            :agent primary
            :call-id "shell-execution-denied-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              ':deny)))
          (unavailable-context
            (make-instance
             'tool-context
             :configuration configuration
             :worker nil
             :conversation (agent-conversation primary)
             :registry registry
             :agent primary
             :call-id "shell-execution-unavailable-test"
             :command-authorization-function
             (lambda (command directory)
               (error 'command-authorization-unavailable
                      :message "Command approval requires an interactive terminal."
                      :command command
                      :directory directory))))
         (slow-path (merge-pathnames "shell-execution-once.txt" root))
         (started-path (merge-pathnames "shell-execution-started.txt" root))
         (finished-path (merge-pathnames "shell-execution-finished.txt" root)))
    (labels ((run-shell (execution-context &rest arguments)
               "Execute shell.run with decoded test ARGUMENTS."
               (tool-execute
                shell-tool execution-context
                (apply #'json-object arguments)))

             (execution-count ()
               "Return the retained asynchronous execution count."
               (length
                (job-pool-list-jobs
                 (task-orchestrator-execution-pool orchestrator))))

             (handoff-job (result)
               "Return RESULT's visible handed-off execution job."
               (let* ((details (tool-result-details result))
                      (record (and (listp details)
                                   (getf (rest details) :job)))
                      (identifier (and record (getf record :id))))
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier primary "shell.run")))))
      (unwind-protect
           (progn
             (let* ((parameters (tool-parameters shell-tool))
                    (properties (json-get parameters "properties"))
                    (required (json-get parameters "required"))
                    (async-schema (and properties
                                       (gethash "async" properties)))
                    (description-schema
                      (and properties
                           (gethash "description" properties))))
               (test-assert
                (and (json-object-p async-schema)
                     (string= (json-get async-schema "type") "boolean")
                     (json-object-p description-schema)
                     (string= (json-get description-schema "type") "string")
                     (not (find "description" required :test #'string=)))
                "shell.run advertises an optional description"))
             (let ((before (execution-count))
                   (result
                     (run-shell denied-context
                                "command"
                                (format nil "printf denied > ~A"
                                        (uiop:escape-shell-token
                                         (namestring slow-path)))
                                "async" t)))
               (test-assert
                (and (not (tool-result-success-p result))
                     (= (execution-count) before)
                     (not (probe-file slow-path)))
                "shell authorization denial occurs before execution-job admission"))
              (let ((before (execution-count))
                    (result
                      (run-shell unavailable-context
                                 "command"
                                 (format nil "printf unavailable > ~A"
                                         (uiop:escape-shell-token
                                          (namestring slow-path))))))
                (test-assert
                 (and (not (tool-result-success-p result))
                      (search "interactive terminal"
                              (tool-result-content result))
                      (= (execution-count) before)
                      (not (probe-file slow-path)))
                 "unavailable interactive authorization becomes a failed tool result before subprocess admission"))
             (let* ((*tool-execution-blocking-grace-seconds* 2)
                    (before (execution-count))
                    (result
                      (run-shell context
                                 "command" "printf fast-shell")))
               (test-assert
                (and (tool-result-success-p result)
                     (not (typep result 'task-tool-result))
                     (search "exit 0" (tool-result-content result))
                     (search "fast-shell" (tool-result-content result))
                     (= (execution-count) (1+ before))
                     (= authorization-count 1))
                "a fast default shell job returns its ordinary result"))
             (let* ((result (run-shell context "command" "exit 7"))
                    (details (tool-result-details result)))
               (test-assert
                (and (not (tool-result-success-p result))
                     (eq (tool-result-error-code result) ':process-exit)
                     (json-object-p details)
                     (= (json-get details "process.exit.code") 7))
                 "a nonzero shell exit is preserved as a failed result through the execution scheduler"))
             (let* ((*tool-execution-blocking-grace-seconds* 0.01)
                    (before (execution-count))
                    (result
                      (run-shell
                       context
                       "command"
                       (format nil "sleep 1; printf once >> ~A"
                               (uiop:escape-shell-token
                                (namestring slow-path)))))
                    (details (tool-result-details result))
                    (job (handoff-job result))
                    (identifier (and job (session-job-identifier job))))
               (test-assert
                (and (typep result 'task-tool-result)
                     (tool-result-success-p result)
                     (eq (getf (rest details) :handoff-reason)
                         :grace-expired)
                     job
                     (session-job-detached-p job)
                     (= (execution-count) (1+ before)))
                "a slow default shell call hands off its existing execution job")
               (let* ((activity (session-job-live-activity job))
                      (summary (tool-execution-job-summary job)))
                 (test-assert
                  (and (null (tool-execution-job-description job))
                       (string= (getf activity :description) summary)
                       (<= (length summary) *tool-execution-summary-limit*)
                       (search "sleep 1" summary))
                  "an omitted description uses the retained bounded job summary"))
               (let* ((wait-result
                        (tool-execute
                         (tool-registry-find registry "job" "wait")
                         context
                         (json-object "id" identifier
                                      "timeout-seconds" 2)))
                      (wait-details (rest (tool-result-details wait-result)))
                      (record (getf wait-details :job)))
                 (test-assert
                  (and (tool-result-success-p wait-result)
                       (getf wait-details :terminal-p)
                       (string= (getf record :id) identifier)
                       (null (getf record :description))
                       (eq (getf record :state) :completed)
                       (string= (uiop:read-file-string slow-path) "once")
                       (= (execution-count) (1+ before)))
                  "job.wait retains one shell execution without a description")))
             (let* ((result
                      (run-shell
                       context
                       "command"
                       (format nil
                               "printf started > ~A; sleep 1; printf finished > ~A"
                               (uiop:escape-shell-token
                                (namestring started-path))
                               (uiop:escape-shell-token
                                (namestring finished-path)))
                       "async" t))
                    (details (tool-result-details result))
                    (job (handoff-job result))
                    (identifier (and job (session-job-identifier job))))
               (test-assert
                (and (typep result 'task-tool-result)
                     (eq (getf (rest details) :handoff-reason) :requested)
                     job
                     (task-tests--wait-until
                      (lambda () (probe-file started-path)) 2))
                "explicit async returns an inspectable shell job while it runs")
                (let* ((wait-result
                         (tool-execute
                          (tool-registry-find registry "job" "wait")
                          context
                          (json-object "id" identifier
                                       "timeout-seconds" 0)))
                       (record (getf (rest (tool-result-details wait-result))
                                     :job)))
                  (test-assert
                   (and (tool-result-success-p wait-result)
                        (string= (getf record :id) identifier)
                        (eq (getf record :state) :running))
                   "job.wait with zero timeout inspects the running shell execution"))
               (let* ((cancel-result
                        (tool-execute
                         (tool-registry-find registry "job" "cancel")
                         context
                         (json-object "id" identifier)))
                      (cancel-details
                        (rest (tool-result-details cancel-result))))
                 (test-assert
                  (and (tool-result-success-p cancel-result)
                       (getf cancel-details :accepted-p))
                  "job.cancel accepts cancellation of a running shell execution"))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :aborted)
                       (eq (getf (getf snapshot :result) :status) :aborted))
                  "shell cancellation publishes an aborted terminal job"))
               (sleep 1.2)
               (test-assert
                (not (probe-file finished-path))
                "shell cancellation prevents the external command from continuing"))
             (let* ((result
                      (let ((*shell-maximum-output-characters* 5))
                        (run-shell context
                                   "command" "printf 123456789"
                                   "async" t)))
                    (job (handoff-job result)))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (let ((content (getf (getf snapshot :result) :content)))
                   (test-assert
                    (and terminal-p
                         (search "12345" content)
                         (not (search "6789" content))
                         (search
                          "combined output truncated after 5 characters"
                          content))
                    "an asynchronous shell job retains its admission-time output bound")))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)


(-> test-tool-execution-retention () null)
(defun test-tool-execution-retention ()
  "Test bounded execution retention and complete two-pool detachment."
  (let* ((*tool-execution-terminal-retention-limit* 2)
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "execution-retention-primary" registry))
         (identifiers nil)
         (closed-p nil))
    (unwind-protect
         (progn
           (dotimes (index 3)
             (let ((job
                     (task-orchestrator-start-execution-job
                      orchestrator primary
                      :tool-name "lisp.eval"
                      :summary (format nil "retained execution ~D" index)
                      :operation-function
                      (lambda ()
                        (tool-success (format nil "result ~D" index)))
                      :detached-p t)))
               (push (session-job-identifier job) identifiers)
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :completed)
                       (null (tool-execution-job-operation-function job)))
                  "each retained execution completes and releases its closure"))))
           (setf identifiers (nreverse identifiers))
           (let ((retained
                   (job-pool-list-jobs
                    (task-orchestrator-execution-pool orchestrator))))
             (test-assert
              (and (= (length retained) 2)
                   (equal (mapcar #'session-job-identifier retained)
                          (rest identifiers))
                   (null
                    (task-orchestrator--find-job
                     orchestrator (first identifiers)))
                   (zerop (task-orchestrator-session-live-count orchestrator)))
              "the execution pool retains exactly its newest terminal records"))
           (setf closed-p (task-orchestrator-close orchestrator))
           (test-assert closed-p "both session job pools close completely")
           (task-orchestrator-detach orchestrator)
           (test-assert
            (and
             (null
              (job-pool-list-jobs (task-orchestrator-pool orchestrator)))
             (null
              (job-pool-list-jobs
               (task-orchestrator-execution-pool orchestrator))))
            "detachment removes both closed job graphs"))
      (unless closed-p
        (ignore-errors (task-orchestrator-close orchestrator)))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-job-list-pagination () null)
(defun test-task-job-list-pagination ()
  "Test content-aware native pagination at the maximum job.list page size."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (agent-name
           (concatenate 'string "a" (make-string 63 :initial-element #\z)))
         (requested-name (make-string 64 :initial-element #\n))
         (definition
           (task-agent-definition-create
            :name agent-name
            :description "Exercise the largest job listing page."
            :instructions "Remain queued for pagination."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "job-list-primary"))
         (tool
           (make-instance
            'task-job-tool
            :orchestrator orchestrator
            :namespace "job"
            :name "list"
            :description "List test jobs."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation (agent-conversation primary)
                          :registry (agent-tool-registry primary)
                          :agent primary)))
    (unwind-protect
         (progn
           (dotimes (index *task-job-page-maximum*)
             (declare (ignore index))
             (task-tests--register-job
              orchestrator primary definition :name requested-name))
           (let* ((result
                    (tool-execute
                     tool context
                     (json-object
                      "offset" 0
                      "limit" *task-job-page-maximum*)))
                  (content (tool-result-content result))
                  (form (task-tests--read-exact-native-value content))
                  (properties (rest form)))
             (test-assert
              (and (eq (first form) :job-list)
                   (= (getf properties :count)
                      *task-job-page-maximum*)
                   (= (getf properties :total)
                      *task-job-page-maximum*)
                   (null (getf properties :next-offset))
                   (<= (length content) *task-tool-content-limit*))
              "a maximum-size job.list request returns a bounded native page"))
           (let ((offset 0)
                 (identifiers nil)
                 (page-count 0))
             (loop
               (let* ((result
                        (tool-execute
                         tool context
                         (json-object
                          "offset" offset
                          "limit" 17)))
                      (content (tool-result-content result))
                      (form
                        (task-tests--read-exact-native-value content))
                      (properties (rest form))
                      (jobs (getf properties :jobs))
                      (next-offset (getf properties :next-offset)))
                 (incf page-count)
                 (test-assert
                  (and (eq (first form) :job-list)
                       (= (getf properties :offset) offset)
                       (= (getf properties :count) (length jobs))
                       (= (getf properties :total)
                          *task-job-page-maximum*)
                       (plusp (length jobs))
                       (<= (length content) *task-tool-content-limit*))
                  "each maximum-size job.list request returns one bounded native page")
                 (setf identifiers
                       (nconc identifiers
                              (mapcar (lambda (job) (getf job :id)) jobs)))
                 (if next-offset
                     (setf offset next-offset)
                     (return))))
             (test-assert
              (and (> page-count 1)
                   (= (length identifiers) *task-job-page-maximum*)
                   (= (length
                       (remove-duplicates identifiers :test #'string=))
                      *task-job-page-maximum*))
              "job.list pagination returns every oversized summary exactly once")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-refresh-after-delayed-close () null)
(defun test-task-refresh-after-delayed-close ()
  "Test reopening after a timed-out close's final worker exits later."
  (let* ((orchestrator (task-tests--orchestrator))
         (barrier-lock (make-lock "Autolith delayed close test"))
         (barrier (make-condition-variable))
         (started-p nil)
         (released-p nil)
         (thread
           (make-thread
            (lambda ()
              (with-lock-held (barrier-lock)
                (setf started-p t)
                (task--condition-broadcast barrier)
                (loop until released-p
                      do (condition-wait barrier barrier-lock))))
            :name "Autolith delayed closing worker")))
    (unwind-protect
         (progn
           (test-assert
            (task-tests--wait-until (lambda () started-p) 2)
            "the delayed closing worker reaches its barrier")
           ;; A real close first, so the pool's own workers and deadline monitor
           ;; are genuinely gone and the only thread left alive is the one this
           ;; test controls.
           (task-orchestrator-close orchestrator)
           (with-lock-held ((cl-jobpond::job-pool--lock (task-orchestrator-pool orchestrator)))
             (setf (cl-jobpond::job-pool--monitor-thread (task-orchestrator-pool orchestrator))
                   nil
                   (cl-jobpond::job-pool--worker-threads (task-orchestrator-pool orchestrator))
                   (list thread)
                   (job-pool-lifecycle-state (task-orchestrator-pool orchestrator)) :closing
                   (cl-jobpond::job-pool--close-owner (task-orchestrator-pool orchestrator)) nil
                   (cl-jobpond::job-pool--shutdown-p (task-orchestrator-pool orchestrator)) t
                   (cl-jobpond::job-pool--active-count (task-orchestrator-pool orchestrator)) 1))
           (test-assert
            (handler-case
                (progn
                  (task-orchestrator-refresh orchestrator)
                  nil)
              (task-error ()
                t))
            "refresh refuses a timed-out close while its delayed worker is live")
           (with-lock-held (barrier-lock)
             (setf released-p t)
             (task--condition-broadcast barrier))
           (join-thread thread)
           (task-orchestrator-refresh orchestrator)
           (test-assert
            (and (eq (job-pool-lifecycle-state (task-orchestrator-pool orchestrator))
                       :open)
                   (null (cl-jobpond::job-pool--close-owner (task-orchestrator-pool orchestrator)))
                   (not (cl-jobpond::job-pool--shutdown-p (task-orchestrator-pool orchestrator)))
                   (zerop (task-orchestrator-active-count orchestrator))
                   (plusp
                    (length
                     (cl-jobpond::job-pool--worker-threads (task-orchestrator-pool orchestrator))))
                   (every
                    #'thread-alive-p
                    (cl-jobpond::job-pool--worker-threads (task-orchestrator-pool orchestrator))))
            "refresh reaps the delayed death and reopens a fresh worker pool"))
      (with-lock-held (barrier-lock)
        (setf released-p t)
        (task--condition-broadcast barrier))
      (when (thread-alive-p thread)
        (join-thread thread))
      (ignore-errors (task-orchestrator-close orchestrator))))
  nil)

(-> test-task-terminal-cancellation-and-publication () null)
(defun test-task-terminal-cancellation-and-publication ()
  "Test terminal-parent cascades and exactly-once terminal publication."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition
           (task-agent-definition-create
            :name "lifecycle"
            :description "Exercise terminal lifecycle invariants."
            :instructions "Publish one terminal result."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "lifecycle-primary"))
         (events nil)
         (event-lock (make-lock "Autolith task lifecycle event test")))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (event-lock)
           (push (copy-tree payload) events)))))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name "terminal-parent"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status ':success :output "parent complete"))
                (parent-id (job-identifier parent))
                (viewer
                  (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (descendant-id
                  (job-identifier descendant))
                (unrelated
                  (task-tests--register-job
                   orchestrator primary definition :name "unrelated")))
           (test-assert
            (task-tests--publish-terminal parent :completed parent-result)
            "the parent publishes its first terminal result")
           (multiple-value-bind (parent-accepted-p descendants)
               (task-job-cancel parent :terminate)
             (test-assert
              (and (null parent-accepted-p)
                   (equal descendants (list descendant-id)))
              "cancelling a terminal parent still cancels its live descendants"))
           (test-assert
            (and (eq (job-state parent) :completed)
                 (eq (job-state descendant) :aborted)
                 (eq (job-state unrelated) :queued)
                 (null (job-cancellation-reason unrelated)))
            "terminal-parent cancellation preserves parent and unrelated states")
           (let ((event-count (length events))
                 (terminal-count
                   (length (cl-jobpond::job-pool--terminal-identifiers
                            (task-orchestrator-pool orchestrator)))))
             (multiple-value-bind (parent-accepted-p descendants)
                 (task-job-cancel parent :terminate)
               (test-assert
                (and (null parent-accepted-p)
                     (null descendants)
                     (= (length events) event-count)
                     (= (length
                         (cl-jobpond::job-pool--terminal-identifiers (task-orchestrator-pool orchestrator)))
                        terminal-count))
                "duplicate cancellation publishes no second result or event")))
           (dolist (job (list parent descendant))
             (let* ((result (job-result job))
                    (pathname (getf result :output-path))
                    (artifact
                      (and pathname
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname)))))
               (test-assert
                (and pathname
                     (probe-file pathname)
                     (listp artifact)
                     (eq (getf artifact :status)
                         (getf result :status)))
                "each cancellation result artifact is exactly one readable s-expression")))
           (let* ((race-job
                    (task-tests--register-job
                     orchestrator primary definition :name "publication-race"))
                  (result
                    (task-tests--terminal-result
                     race-job :status ':success :output "race winner"))
                  (barrier-lock
                    (make-lock "Autolith terminal publication barrier"))
                  (barrier (make-condition-variable))
                  (ready 0)
                  (released-p nil)
                  (claims nil)
                  (claim-lock
                    (make-lock "Autolith terminal publication claims")))
             (labels ((publish ()
                        (with-lock-held (barrier-lock)
                          (incf ready)
                          (task--condition-broadcast barrier)
                          (loop until released-p
                                do (condition-wait barrier barrier-lock)))
                        (let ((claimed-p
                                (task-tests--publish-terminal
                                 race-job :completed result)))
                          (with-lock-held (claim-lock)
                            (push claimed-p claims)))))
               (let ((first-thread
                       (make-thread #'publish
                                    :name "Autolith publication race one"))
                     (second-thread
                       (make-thread #'publish
                                    :name "Autolith publication race two")))
                 (with-lock-held (barrier-lock)
                   (loop until (= ready 2)
                         do (condition-wait barrier barrier-lock))
                   (setf released-p t)
                   (task--condition-broadcast barrier))
                 (join-thread first-thread)
                 (join-thread second-thread)))
             (let* ((race-id (job-identifier race-job))
                    (terminal-occurrences
                      (count race-id
                             (cl-jobpond::job-pool--terminal-identifiers
                              (task-orchestrator-pool orchestrator))
                             :test #'string=))
                    (event-occurrences
                      (count race-id events
                             :test #'string=
                             :key (lambda (event) (getf event :id)))))
               (test-assert
                (and (= (count t claims) 1)
                     (= (count nil claims) 1)
                     (= terminal-occurrences 1)
                     (= event-occurrences 1)
                     (probe-file
                      (getf (job-result race-job) :output-path)))
                "concurrent duplicate publication claims one artifact and event"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-retention-and-admission () null)
(defun test-task-retention-and-admission ()
  "Test exact terminal retention and atomic live-job admission limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "capacity"
            :description "Exercise scheduler capacity."
            :instructions "Remain inert until the test changes state."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "capacity-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-tests--orchestrator))
                  (live
                    (task-tests--register-job
                     orchestrator primary definition :name "live-sentinel"))
                  (identifiers nil))
             (dotimes (index (1+ *task-terminal-retention-limit*))
               (let* ((job
                        (task-tests--register-job
                         orchestrator primary definition
                         :name (format nil "retained-~2,'0D" index)))
                      (identifier (job-identifier job))
                      (result
                        (task-tests--terminal-result
                         job
                         :status ':success
                         :output (format nil "terminal result ~D" index))))
                 (push identifier identifiers)
                 (test-assert
                  (task-tests--publish-terminal job :completed result)
                  "each retained job publishes exactly once")))
             (setf identifiers (nreverse identifiers))
             (let ((first-id (first identifiers))
                   (last-id (car (last identifiers)))
                   (live-id (job-identifier live)))
               (test-assert
                (and (= (length
                         (cl-jobpond::job-pool--terminal-identifiers (task-orchestrator-pool orchestrator)))
                        *task-terminal-retention-limit*)
                     (= (hash-table-count
                         (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                        (1+ *task-terminal-retention-limit*))
                     (= (task-orchestrator-live-count orchestrator) 1)
                     (null
                      (gethash first-id
                               (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))))
                     (gethash last-id
                              (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                     (eq (gethash live-id
                                  (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                         live))
                "retention keeps exactly 64 newest terminals without evicting live jobs")
               (test-assert
                (every
                 (lambda (identifier)
                   (let* ((job
                            (gethash identifier
                                     (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))))
                          (pathname
                            (and job
                                 (getf (job-result job) :output-path))))
                     (and job
                          (job-terminal-p job)
                          pathname
                          (listp
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname))))))
                 (cl-jobpond::job-pool--terminal-identifiers (task-orchestrator-pool orchestrator)))
                "every retained identifier maps to one terminal job and readable artifact")))
           (let* ((orchestrator (task-tests--orchestrator))
                  (entries
                    (lambda (offset count)
                      (loop for index from offset below (+ offset count)
                            collect
                            (list
                             :definition definition
                             :item
                             (list :name (format nil "live-~2,'0D" index)
                                   :agent "capacity"
                                   :task "Remain queued."
                                   :context nil
                                   :async t)
                             :detached t)))))
             ;; Capacity is filled with children no worker will ever pick up, so
             ;; the bound is measured rather than raced against jobs finishing.
             (dotimes (index *task-maximum-live-jobs*)
               (task-tests--register-job
                orchestrator primary definition
                :name (format nil "live-~2,'0D" index)))
             (let ((next-index (cl-jobpond::job-pool--next-index (task-orchestrator-pool orchestrator)))
                   (job-count
                     (hash-table-count
                      (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))))
                   (queue-count (length (cl-jobpond::job-pool--queue (task-orchestrator-pool orchestrator))))
                   (live-count (task-orchestrator-live-count orchestrator)))
               (test-assert
                (= live-count *task-maximum-live-jobs*)
                "the scheduler admits exactly 64 simultaneous live jobs")
               (test-assert
                (handler-case
                    (progn
                      (task-orchestrator-start-jobs
                       orchestrator primary
                       (funcall entries 64 1))
                      nil)
                  (task-error ()
                    t))
                "admission beyond 64 live jobs fails")
               (test-assert
                (and (= (cl-jobpond::job-pool--next-index (task-orchestrator-pool orchestrator))
                        next-index)
                     (= (hash-table-count
                         (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                        job-count)
                     (= (length (cl-jobpond::job-pool--queue (task-orchestrator-pool orchestrator)))
                        queue-count)
                     (= (task-orchestrator-live-count orchestrator)
                        live-count))
                "failed live admission consumes no identity or scheduler state"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-evicted-identity-retention () null)
(defun test-task-evicted-identity-retention ()
  "Test that evicted generated identities remain unique across live ancestry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition
           (task-agent-definition-create
            :name "identity-retention"
            :description "Exercise retained task ancestry."
            :instructions "Keep descendant ownership unambiguous."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "identity-primary")))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name nil))
                (parent-id (job-identifier parent))
                (viewer (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status ':success :output "parent terminal")))
           (task-tests--publish-terminal parent :completed parent-result)
           (dotimes (index *task-terminal-retention-limit*)
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "identity-filler-~D" index)))
                    (result
                      (task-tests--terminal-result
                       job :status ':success :output "filler terminal")))
               (task-tests--publish-terminal job :completed result)))
           (let* ((replacement
                    (task-tests--register-job
                     orchestrator primary definition :name nil))
                  (replacement-id
                    (job-identifier replacement)))
             (test-assert
              (and (null
                    (gethash parent-id
                             (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))))
                   (member parent-id
                           (job-owner-identifiers descendant)
                           :test #'string=)
                   (not (string= replacement-id parent-id))
                   (> (job-index replacement)
                      (job-index parent))
                   (= (task-orchestrator-live-count orchestrator) 2))
              "eviction never permits a generated ancestor identity to be reused")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-task-live-activity-snapshots () null)
(defun test-task-live-activity-snapshots ()
  "Test stable, bounded progress projection for queued and running tasks."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (orchestrator (task-tests--orchestrator))
         (definition
           (task-agent-definition-create
            :name "activity"
            :description "Exercise live task presentation."
            :instructions "Remain observable while the test inspects you."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "activity-primary"))
         (queued nil)
         (running nil))
    (unwind-protect
         (progn
           (setf queued
                 (task-tests--register-job
                  orchestrator primary definition :name "queued-child")
                 running
                 (task-tests--register-job
                  orchestrator primary definition :name "running-child"))
           (with-lock-held ((cl-jobpond::job--lock running))
             (setf (job-state running) ':running)
             (task-job--set-progress-state running ':running))
           (task-progress-note-status
            running ':provider-request-started
            (list :request-number 2))
           (loop for index from 1 to 10
                 for tool = (format nil "tool.~D" index)
                 do (task-progress-note-status
                     running ':tool-call-started
                     (list :tool tool))
                    (task-progress-note-status
                     running ':tool-call-completed
                     (list :tool tool)))
           (task-progress-note-status
            running ':tool-call-started
            (list :tool "lisp.eval"))
           (let* ((activities
                    (task-orchestrator-live-activities orchestrator))
                  (running-activity (second activities))
                  (progress-snapshot (task-progress-snapshot running)))
             (test-assert
              (and (= (length activities) 2)
                   (equal
                    (mapcar (lambda (activity)
                              (getf activity :id))
                            activities)
                    (mapcar (lambda (job)
                              (job-identifier job))
                            (list queued running)))
                   (eq (getf (first activities) :state) ':queued)
                   (eq (getf running-activity :state) ':running)
                   (string= (getf running-activity :current-tool)
                            "lisp.eval")
                   (equal (getf running-activity :recent-tools)
                          (loop for index from 3 to 10
                                collect (format nil "tool.~D" index)))
                   (= (getf running-activity :request-count) 2)
                   (typep (getf running-activity :duration-ms)
                          '(integer 0))
                   (typep (getf running-activity :current-tool-duration-ms)
                          '(integer 0))
                   (<= (getf running-activity :current-tool-duration-ms)
                       (getf running-activity :duration-ms))
                   (string= (getf running-activity :agent) "activity")
                   (getf running-activity :detached)
                   (equal (getf progress-snapshot :recent-tools)
                          (getf running-activity :recent-tools))
                   (= (getf progress-snapshot :request-count) 2)
                   (typep (getf progress-snapshot :current-tool-duration-ms)
                          '(integer 0)))
              "live activity snapshots retain bounded chronological progress"))
           (task-progress-note-status
            running ':tool-call-completed
            (list :tool "lisp.eval"))
           (let* ((activities
                    (task-orchestrator-live-activities orchestrator))
                  (running-activity (second activities))
                  (progress-snapshot (task-progress-snapshot running)))
             (test-assert
              (and (null (getf running-activity :current-tool))
                   (null (getf running-activity :current-tool-duration-ms))
                   (null (getf progress-snapshot :current-tool-duration-ms))
                   (equal (getf running-activity :recent-tools)
                          (append
                           (loop for index from 4 to 10
                                 collect (format nil "tool.~D" index))
                           (list "lisp.eval"))))
              "tool completion clears timing and preserves the newest eight tools"))
           (task-tests--publish-terminal
            queued
            ':completed
            (task-tests--terminal-result
             queued :status ':success :output "queued complete"))
           (test-assert
            (equal (mapcar (lambda (activity)
                             (getf activity :id))
                           (task-orchestrator-live-activities orchestrator))
                   (list (job-identifier running)))
            "terminal publication immediately removes a child snapshot")
           (task-tests--publish-terminal
            running
            ':completed
            (task-tests--terminal-result
             running :status ':success :output "running complete"))
           (test-assert
            (null (task-orchestrator-live-activities orchestrator))
            "no task presentation remains after every child is terminal"))
      (dolist (job (remove nil (list queued running)))
        (unless (job-terminal-p job)
          (task-tests--publish-terminal
           job
           ':aborted
           (task-tests--terminal-result
            job :status ':aborted :output "test cleanup"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> task-tests--run-scheduler-case
    (task-test-provider json-object
     &key (:parent-reference-p boolean)
          (:oversized-parent-reference-p boolean))
    list)
(defun task-tests--run-scheduler-case
    (provider arguments &key parent-reference-p oversized-parent-reference-p)
  "Execute one real task tool case and return observations after clean shutdown."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (task-augment-tool-registry
                    (make-default-tool-registry)))
         (conversation (conversation-create configuration))
         (parent
          (agent-create :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry registry
                        :worker nil))
         (tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator tool))
         (context
          (make-instance 'tool-context
                         :configuration configuration
                         :worker nil
                         :conversation conversation
                         :registry registry
                         :agent parent
                         :call-id "task-scheduler-test"))
         (started-at (get-internal-real-time)))
    (unwind-protect
         (progn
           (when parent-reference-p
             (conversation-append-user-message
              conversation
              (if oversized-parent-reference-p
                  (make-string 40000 :initial-element #\P)
                  "Use the parent design constraints."))
             (conversation-append-provider-item
              conversation
              (json-object
               "type" "message"
               "role" "assistant"
               "content" (json-array
                          (json-object "type" "output_text"
                                       "text" "Parent design is ready.")))))
           (let* ((result (tool-execute tool context arguments))
                  (idle-p
                    (task-tests--wait-until
                     (lambda ()
                       (and
                          (zerop
                           (task-orchestrator-active-count orchestrator))
                          (loop for job being the hash-values of
                                (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))
                                always (job-terminal-p job))))
                     1))
                  (jobs (task-orchestrator-list-jobs orchestrator))
                  (details (tool-result-details result))
                  (content (tool-result-content result))
                  (native-content
                    (task-tests--read-exact-native-value content))
                  (artifact-forms
                    (mapcar
                     (lambda (job)
                       (task-tests--read-exact-native-value
                        (uiop:read-file-string
                         (getf (job-result job) :output-path))))
                     jobs))
                  (workers
                    (copy-list
                       (cl-jobpond::job-pool--worker-threads (task-orchestrator-pool orchestrator)))))
             (list :success-p (tool-result-success-p result)
                 :content content
                 :native-content native-content
                 :details details
                 :artifact-forms artifact-forms
                 :job-count (length jobs)
                 :all-terminal-p (every #'job-terminal-p jobs)
                 :heavy-references-cleared-p
                 (every (lambda (job)
                          (and (null (task-job-parent-agent job))
                               (not (task-job-inherited-reference-p job))
                               (null (task-job-inherited-reference-items job))
                               (null
                                (task-job-command-authorization-function job))
                               (null
                                (task-job-tool-authorization-function job))
                               (null (cl-jobpond::job--thread job))))
                        jobs)
                 :worker-count (length workers)
                 :workers-alive-p (every #'thread-alive-p workers)
                 :provider-worker-count
                 (length (task-test-provider-threads provider))
                 :provider-maximum-active
                 (task-test-provider-maximum-active-count provider)
                 :provider-conversation-identifiers
                 (with-lock-held ((task-test-provider-lock provider))
                   (copy-list
                    (task-test-provider-conversation-identifiers provider)))
                 :provider-request-inputs
                 (with-lock-held ((task-test-provider-lock provider))
                   (reverse
                    (copy-tree (task-test-provider-request-inputs provider))))
                 :execution-identifiers
                 (mapcar #'task-job-execution-identifier jobs)
                 :provider-request-count
                 (task-test-provider-request-count provider)
                 :scheduler-idle-p idle-p
                 :active-count
                 (task-orchestrator-active-count orchestrator)
                 :live-count
                 (task-orchestrator-live-count orchestrator)
                 :artifacts-exist-p
                 (every (lambda (job)
                          (let ((path (getf (job-result job)
                                           :output-path)))
                            (and path (probe-file path))))
                        jobs)
                 :private-transcripts-p
                 (every (lambda (job)
                          (let ((path (getf (job-result job)
                                           :conversation-file)))
                            (and path (search "/tasks/" path))))
                        jobs)
                 :public-conversation-count
                 (length (conversation-list configuration))
                 :duration-ms
                 (task--milliseconds-between started-at
                                             (get-internal-real-time)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore))))

(-> test-task-run-native-manifest () null)
(defun test-task-run-native-manifest ()
  "Test fair bounded native manifests for the largest synchronous batch."
  (let ((previous-concurrency
          (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "5000" 1)
           (let* ((provider
                    (make-instance 'task-test-provider :mode ':manifest))
                  (tasks
                    (coerce
                     (loop for index from 1 to *task-maximum-batch-size*
                           collect
                           (json-object
                            "name" (format nil "manifest-~2,'0D" index)
                            "agent" "task"
                            "task"
                            (format nil
                                    "Return manifest result ~D."
                                    index)))
                     'vector))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "context" "Exercise fair native aggregation."
                      "blocking" t
                      "tasks" tasks)))
                  (content (getf observation :content))
                  (form (getf observation :native-content))
                  (results (getf (rest form) :results))
                  (artifacts (getf observation :artifact-forms))
                  (first-result (first results))
                  (last-result (car (last results)))
                  (last-result-value (getf last-result :result)))
             (test-assert
              (and (not (getf observation :success-p))
                   (equal form (getf observation :details))
                   (eq (first form) :task-run)
                   (null (getf (rest form) :succeeded-p))
                   (<= (length content) *task-tool-content-limit*))
              "task.run returns one exact bounded native manifest on partial failure")
             (test-assert
              (and (= (length results) *task-maximum-batch-size*)
                   (= (length artifacts) *task-maximum-batch-size*)
                   (= (length
                       (remove-duplicates
                        (mapcar (lambda (result) (getf result :id)) results)
                        :test #'string=))
                      *task-maximum-batch-size*)
                   (every
                    (lambda (result)
                      (let ((artifact (getf (getf result :result) :artifact)))
                        (and (non-empty-string-p (getf result :id))
                             (getf result :execution-id)
                             (member (getf result :state)
                                     '(:completed :failed :aborted)
                                     :test #'eq)
                             (stringp (getf artifact :path))
                             (eq (getf artifact :format) :sexp)
                             (getf artifact :available-p))))
                    results))
              "the manifest retains every child identity, state, and artifact descriptor")
             (test-assert
              (and
               (equal (getf first-result :id) "manifest-01-1")
               (equal (getf last-result :id) "manifest-16-16")
               (eq (getf first-result :state) :completed)
               (eq (getf last-result :state) :failed)
               (eq (getf last-result-value :status) :failed)
               (string=
                (getf last-result-value :error)
                "AUTOLITH-LAST-MANIFEST-CHILD-FAILED"))
              "a huge first result cannot hide the final failed child")
             (test-assert
              (and (> (length (getf (first artifacts) :output)) 90000)
                   (string=
                    (getf (car (last artifacts)) :error)
                    "AUTOLITH-LAST-MANIFEST-CHILD-FAILED")
                   (every #'listp artifacts))
              "every child artifact remains exactly one readable native result")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS"
                          previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-scheduler () null)
(defun test-task-scheduler ()
  "Test bounded reusable workers, private artifacts, and nested help-join."
  (let ((previous-concurrency (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "2" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000" 1)
           (let* ((provider (make-instance 'task-test-provider
                                           :mode ':concurrent))
                  (tasks
                   (coerce
                    (loop for index from 1 to 4
                          collect (json-object
                                   "agent" "task"
                                   "task" (format nil "Return result ~D." index)))
                    'vector))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "context" "Independent scheduler checks."
                                 "blocking" t
                                 "tasks" tasks))))
             (test-assert (getf observation :success-p)
                          "a concurrent task batch succeeds")
             (test-assert (= (getf observation :job-count) 4)
                          "the scheduler retains every admitted job")
             (test-assert (getf observation :all-terminal-p)
                          "synchronous scheduler jobs are terminal on return")
             (test-assert (getf observation :heavy-references-cleared-p)
                          "terminal jobs release live agent capabilities")
             (test-assert (= (getf observation :worker-count) 2)
                          "the configured pool contains only two workers")
             (test-assert (getf observation :workers-alive-p)
                          "reusable workers remain live until registry shutdown")
             (test-assert (= (getf observation :provider-worker-count) 2)
                          "four children reuse the bounded worker pair")
             (test-assert (= (getf observation :provider-maximum-active) 2)
                          "the pool executes up to its configured concurrency")
             (let ((identifiers
                     (getf observation :provider-conversation-identifiers))
                   (execution-identifiers
                     (getf observation :execution-identifiers)))
               (test-assert
                (and (= (length identifiers) 4)
                     (= (length (remove-duplicates identifiers :test #'string=))
                        4)
                     (equal (sort (copy-list identifiers) #'string<)
                            (sort (copy-list execution-identifiers) #'string<)))
                "parallel children use their unique execution identifiers as provider threads"))
             (test-assert (getf observation :artifacts-exist-p)
                          "every terminal child publishes one unique artifact")
             (test-assert (getf observation :private-transcripts-p)
                          "child transcripts live in the private task tree")
             (test-assert (zerop (getf observation
                                       :public-conversation-count))
                          "private child transcripts stay out of conversation lists"))
           (let* ((provider (make-instance 'task-test-provider
                                           :mode ':concurrent))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object "agent" "task"
                                  "task" "Use the inherited design context."
                                  "blocking" t)
                     :parent-reference-p t))
                  (inputs (first (getf observation :provider-request-inputs))))
             (test-assert (getf observation :success-p)
                          "a child with inherited reference context succeeds")
             (test-assert
              (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                     '("user" "assistant" "developer" "user"))
              "the real task child receives parent turns, a boundary, and its task")
             (test-assert
              (search "inherited reference context"
                      (json-get (aref (json-get (third inputs) "content") 0)
                                "text"))
              "the inherited parent turns end at an explicit developer boundary")
             (test-assert (= (getf observation :public-conversation-count) 1)
                          "only the seeded parent appears in public conversations"))
           (let* ((provider
                    (make-instance 'task-test-provider :mode ':concurrent))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object "agent" "task"
                                  "task" "Use bounded inherited context."
                                  "async" t)
                     :parent-reference-p t
                     :oversized-parent-reference-p t))
                  (inputs (first (getf observation :provider-request-inputs))))
             (test-assert (getf observation :success-p)
                          "a detached child with oversized parent history succeeds")
             (test-assert
              (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                     '("assistant" "developer" "user"))
              "oversized parent history keeps only the newest fitting message")
             (test-assert
              (<= (conversation--inherited-reference-wire-byte-length
                   (list (first inputs)))
                  *task-inherited-reference-maximum-bytes*)
              "detached child inheritance stays within the fixed wire budget"))
           (dolist (capabilities '((nil t "parent") (t nil "child")))
             (destructuring-bind
                 (parent-reference-p child-reference-p label)
                 capabilities
               (let* ((provider
                        (make-instance
                         'task-test-provider
                         :mode ':concurrent
                         :reference-history-p parent-reference-p
                         :child-reference-history-p child-reference-p))
                      (observation
                        (task-tests--run-scheduler-case
                         provider
                         (json-object "agent" "task"
                                      "task" "Do not inherit parent history."
                                      "blocking" t)
                         :parent-reference-p t))
                      (inputs
                        (first (getf observation :provider-request-inputs))))
                 (test-assert (getf observation :success-p)
                              "a provider-isolation task succeeds")
                 (test-assert
                  (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                         '("user"))
                  (format nil
                          "unsupported ~A provider retains no inherited history"
                          label)))))
           (let* ((*task-inherited-reference-context-divisor* 1000000)
                  (provider
                    (make-instance 'task-test-provider :mode ':concurrent))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object "agent" "task"
                                  "task" "Use a tiny child context budget."
                                  "blocking" t)
                     :parent-reference-p t))
                  (inputs (first (getf observation :provider-request-inputs))))
             (test-assert (getf observation :success-p)
                          "a child with a tiny reference budget succeeds")
             (test-assert
              (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                     '("user"))
              "inheritance is disabled when its boundary exceeds the budget"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (let* ((provider (make-instance 'task-test-provider :mode ':nested))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "agent" "task"
                                 "task" "Delegate once, then return."
                                 "blocking" t))))
             (test-assert (getf observation :success-p)
                          "a nested synchronous task succeeds at concurrency one")
             (test-assert (= (getf observation :job-count) 2)
                          "nested execution retains parent and leaf jobs")
             (test-assert (= (getf observation :provider-request-count) 3)
                          "nested help-join resumes the parent after the leaf")
             (test-assert (= (getf observation :provider-worker-count) 1)
                          "nested synchronous work reuses its parent's worker")
             (test-assert
              (find-if
               (lambda (items)
                 (equal (mapcar (lambda (item) (json-get item "role")) items)
                        '("user" "developer" "user")))
               (getf observation :provider-request-inputs))
              "a nested child inherits its immediate parent's assignment")
             (test-assert (< (getf observation :duration-ms) 1000)
                          "nested help-join avoids a concurrency-one deadlock"))
           (let* ((provider
                    (make-instance 'task-test-provider :mode ':async-wait))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "agent" "task"
                      "task"
                      "Spawn one detached task, wait for it, then return."
                      "blocking" t)))
                  (artifacts (getf observation :artifact-forms)))
             (test-assert
              (and (getf observation :success-p)
                   (= (getf observation :job-count) 2)
                   (getf observation :all-terminal-p)
                   (= (getf observation :provider-request-count) 4)
                   (= (getf observation :provider-worker-count) 1)
                   (getf observation :scheduler-idle-p)
                   (zerop (getf observation :active-count))
                   (zerop (getf observation :live-count))
                   (every (lambda (artifact)
                            (eq (getf artifact :status) :success))
                          artifacts)
                   (< (getf observation :duration-ms) 1000))
              "a child can await detached work at concurrency one without starvation"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "999" 1)
           (let ((orchestrator (task-tests--orchestrator)))
             (test-assert
              (= (task-orchestrator-maximum-concurrency orchestrator)
                 *task-maximum-concurrency*)
              "environment concurrency cannot exceed the hard pool cap")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)
