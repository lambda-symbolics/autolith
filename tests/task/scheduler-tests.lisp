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
  "Test detached admission and completion through a real child tool call."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (configuration      (configuration-copy base-configuration :working-directory root))
         (registry           (task-augment-tool-registry (make-default-tool-registry)))
         (blocking-tool
          (make-instance 'task-test-blocking-tool :namespace "test" :name "block"
                         :description
                         "Wait until the detachment test releases this call." :parameters
                         (tool-object-schema (json-object) nil)))
         (provider           (make-instance 'task-test-provider :mode ':blocking-tool))
         (conversation       (conversation-create configuration))
         (primary
          (agent-create :configuration configuration :provider provider :conversation
                        conversation :tool-registry registry :worker nil))
         (run-tool           (tool-registry-find registry "child" "spawn"))
         (orchestrator       (task-run-tool-orchestrator run-tool))
         (context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation conversation :registry registry :agent primary
                         :call-id "default-detached")))
    (unwind-protect
        (progn
         (tool-registry-register registry blocking-tool)
         (let* ((result
                 (tool-execute run-tool context
                               (json-object "name" "default-detached" "task"
                                            "Enter the blocking ordinary tool.")))
                (jobs (task-orchestrator-list-jobs orchestrator))
                (job  (and (= (length jobs) 1) (first jobs))))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held ((task-test-blocking-tool-lock blocking-tool))
                 (task-test-blocking-tool-started-p blocking-tool)))
             2)
            "the child reaches the blocking tool after admission returns")
           (test-assert
            (and job (task-job-detached-p job) (not (job-terminal-p job))
                 (tool-result-success-p result))
            "child.spawn returns an admitted detached job")
           (task-tests--release-blocking-tool blocking-tool)
           (multiple-value-bind (snapshot terminal-p)
               (task-job-await job 2)
             (test-assert
              (and terminal-p (eq (getf snapshot :state) ':completed)
                   (eq (getf (getf snapshot :result) :status) ':success))
              "the detached child publishes its result after release"))))
      (task-tests--release-blocking-tool blocking-tool)
      (dolist (job (task-orchestrator-list-jobs orchestrator))
        (unless (job-terminal-p job)
          (task-job-cancel job ':test-cleanup)
          (task-job-await job 2)))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-running-cancellation () null)
(defun test-task-running-cancellation ()
  "Test prompt cancellation while a child executes an ordinary tool call."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
          (make-instance 'task-test-blocking-tool :namespace "test" :name "block"
                         :description
                         "Wait until the cancellation test releases this call."
                         :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider     (make-instance 'task-test-provider :mode ':blocking-tool))
           (conversation (conversation-create configuration))
           (primary
            (agent-create :configuration configuration :provider provider :conversation
                          conversation :tool-registry registry :worker nil))
           (run-tool     (tool-registry-find registry "child" "spawn"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
            (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation conversation :registry registry :agent primary
                           :call-id "running-cancellation")))
      (unwind-protect
          (progn
           (tool-execute run-tool context
                         (json-object "name" "blocked-running-child" "task"
                                      "Enter the blocking ordinary tool."))
           (let ((job (first (task-orchestrator-list-jobs orchestrator))))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held ((task-test-blocking-tool-lock blocking-tool))
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
                    (and accepted-p terminal-p (eq (getf snapshot :state) :aborted)
                         (eq (getf (getf snapshot :result) :status) :aborted)
                         (<
                          (task--milliseconds-between started-at
                                                      (get-internal-real-time))
                          1000))
                    "running cancellation unwinds an ordinary tool call promptly"))))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (and (zerop (task-orchestrator-active-count orchestrator))
                      (zerop (task-orchestrator-live-count orchestrator))))
               2)
              "cancelled ordinary tool execution releases scheduler accounting")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-task-runtime-deadline () null)
(defun test-task-runtime-deadline ()
  "Test the bounded default and environment override of a stalled child."
  (let ((previous-runtime (uiop/os:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
        (progn
         (platform-unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS")
         (let ((orchestrator (task-tests--orchestrator)))
           (test-assert
            (and (plusp *task-default-maximum-runtime-milliseconds*)
                 (= (task-orchestrator-maximum-runtime-milliseconds orchestrator)
                    *task-default-maximum-runtime-milliseconds*))
            "task children have a bounded default runtime deadline"))
         (platform-setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000")
         (let* ((configuration (test-configuration))
                (root          (test-configuration-root configuration))
                (registry      (make-default-tool-registry))
                (blocking-tool
                 (make-instance 'task-test-blocking-tool :namespace "test" :name "block"
                                :description
                                "Wait until the deadline test releases this call."
                                :parameters (tool-object-schema (json-object) nil)))
                (runner        nil)
                (run-result    nil)
                (run-condition nil))
           (tool-registry-register registry blocking-tool)
           (task-augment-tool-registry registry)
           (let* ((provider     (make-instance 'task-test-provider :mode ':blocking-tool))
                  (conversation (conversation-create configuration))
                  (primary
                   (agent-create :configuration configuration :provider provider
                                 :conversation conversation :tool-registry registry
                                 :worker nil))
                  (run-tool     (tool-registry-find registry "child" "spawn"))
                  (orchestrator (task-run-tool-orchestrator run-tool))
                  (context
                   (make-instance 'tool-context :configuration configuration :worker nil
                                  :conversation conversation :registry registry :agent
                                  primary :call-id "runtime-deadline")))
             (unwind-protect
                 (progn
                  (setf runner
                          (make-thread
                           (lambda ()
                             (handler-case
                              (setf run-result
                                      (tool-execute run-tool context
                                                    (json-object "name" "stalled-child"
                                                                 "task"
                                                                 "Enter the blocking ordinary tool.")))
                              (serious-condition (condition)
                               (setf run-condition condition))))
                           :name "Autolith task deadline test"))
                  (test-assert
                   (task-tests--wait-until
                    (lambda ()
                      (with-lock-held ((task-test-blocking-tool-lock blocking-tool))
                        (task-test-blocking-tool-started-p blocking-tool)))
                    2)
                   "the child stalls inside a real ordinary tool call")
                  (test-assert
                   (task-tests--wait-until
                    (lambda ()
                      (let ((job (first (task-orchestrator-list-jobs orchestrator))))
                        (and job (job-terminal-p job))))
                    3)
                   "the runtime deadline terminates the stalled child")
                  (join-thread runner)
                  (setf runner nil)
                  (let* ((jobs (task-orchestrator-list-jobs orchestrator))
                         (snapshot
                          (and (= (length jobs) 1) (task-job-snapshot (first jobs)))))
                    (test-assert
                     (and (null run-condition) (typep run-result 'tool-result)
                          (tool-result-success-p run-result) snapshot
                          (eq (getf snapshot :state) :aborted)
                          (eq (getf snapshot :cancellation-reason) :timeout)
                          (eq (getf (getf snapshot :result) :status) :aborted)
                          (task-tests--wait-until
                           (lambda ()
                             (and (zerop (task-orchestrator-active-count orchestrator))
                                  (zerop (task-orchestrator-live-count orchestrator))))
                           2))
                     "a deadline publishes one timeout result and releases scheduler accounting")))
               (task-tests--release-blocking-tool blocking-tool)
               (when runner (join-thread runner))
               (ignore-errors (tool-registry-close-runtime-state registry))
               (platform-delete-directory-tree *platform* root :validate t
                                               :if-does-not-exist        :ignore)))))
      (if previous-runtime
          (platform-setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" previous-runtime)
          (platform-unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-nested-parent-cancellation () null)
(defun test-task-nested-parent-cancellation ()
  "Test parent cancellation while its synchronous descendant is running."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
          (make-instance 'task-test-blocking-tool :namespace "test" :name "block"
                         :description "Wait inside a nested synchronous child."
                         :parameters  (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider     (make-instance 'task-test-provider :mode ':nested-cancel))
           (conversation (conversation-create configuration))
           (primary
            (agent-create :configuration configuration :provider provider :conversation
                          conversation :tool-registry registry :worker nil))
           (run-tool     (tool-registry-find registry "child" "spawn"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
            (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation conversation :registry registry :agent primary
                           :call-id "nested-parent-cancellation")))
      (unwind-protect
          (progn
           (tool-execute run-tool context
                         (json-object "name" "nested-cancel-parent" "task"
                                      "Delegate, then await the descendant."))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held ((task-test-blocking-tool-lock blocking-tool))
                 (task-test-blocking-tool-started-p blocking-tool)))
             2)
            "the descendant reaches its blocking tool")
           (let* ((jobs          (task-orchestrator-list-jobs orchestrator))
                  (parent        (first jobs))
                  (descendant    (second jobs))
                  (descendant-id (job-identifier descendant)))
             (test-assert (= (length jobs) 2)
                          "nested cancellation observes parent and descendant")
             (multiple-value-bind (accepted-p descendants)
                 (task-job-cancel parent :user)
               (test-assert (and accepted-p (equal descendants (list descendant-id)))
                            "parent cancellation reaches the live synchronous descendant"))
             (dolist (job jobs)
               (multiple-value-bind (snapshot terminal-p)
                   (task-job-await job 2)
                 (test-assert
                  (and terminal-p (eq (getf snapshot :state) :aborted)
                       (eq (getf (getf snapshot :result) :status) :aborted))
                  "parent and synchronous descendant both terminalize as aborted")))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (and (zerop (task-orchestrator-active-count orchestrator))
                      (zerop (task-orchestrator-live-count orchestrator))
                      (null (task-tests--queued-jobs orchestrator))))
               2)
              "nested parent cancellation leaves no orphan or live-count leak")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-task-admission-cancellation-barrier () null)
(defun test-task-admission-cancellation-barrier ()
  "Refuse admission racing cancellation at the shared ownership boundary.

Pause after Autolith's courtesy check, then cancel before pool admission. The
pool must reject the cancelled parent without creating a descendant."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "admission-primary")))
    (unwind-protect
        (progn
         (let* ((orchestrator          (task-tests--orchestrator))
                (parent
                 (task-tests--register-job orchestrator primary definition :name
                                           "race-parent"))
                (viewer                (task-tests--child-viewer configuration parent))
                (entries
                 (list
                  (list :item
                        (append
                         (list :name "racing-descendant" :task
                               "Be admitted before cancellation scans." :context nil
                               :async t)
                         definition)
                        :detached t)))
                (pause-lock            (make-lock "Autolith admission race pause"))
                (pause-condition       (make-condition-variable))
                (admission-paused-p    nil)
                (release-admission-p   nil)
                (admitted              nil)
                (admission-failure     nil)
                (cancel-accepted-p     nil)
                (cancelled-descendants nil)
                (admission-thread      nil)
                (original-owner-identifiers
                 (symbol-function 'task-parent-owner-identifiers)))
           (unwind-protect
               (test-call-with-function-replacements
                (list
                 (list 'task-parent-owner-identifiers
                       (lambda (candidate)
                         (let ((identifiers
                                (funcall original-owner-identifiers candidate)))
                           (when (eq candidate viewer)
                             (with-lock-held (pause-lock)
                               (setf admission-paused-p t)
                               (task--condition-broadcast pause-condition)
                               (loop until release-admission-p
                                     do (condition-wait pause-condition pause-lock))))
                           identifiers))))
                (lambda ()
                  (setf admission-thread
                          (make-thread
                           (lambda ()
                             (handler-case
                              (setf admitted
                                      (first
                                       (multiple-value-list
                                        (task-orchestrator-start-jobs orchestrator viewer
                                                                      entries))))
                              (condition (condition)
                               (setf admission-failure condition))))
                           :name "Autolith admission race"))
                  (with-lock-held (pause-lock)
                    (loop until admission-paused-p
                          unless (condition-wait pause-condition pause-lock :timeout 2)
                          do (error "Timed out waiting for admission pause.")))
                  (test-assert (null admitted)
                               "nested admission pauses after its parent check")
                  (multiple-value-setq (cancel-accepted-p cancelled-descendants)
                    (task-job-cancel parent :user))
                  (test-assert
                   (and cancel-accepted-p (null cancelled-descendants)
                        (job-cancellation-requested-p parent))
                   "cancellation marks the parent before admission resumes")
                  (with-lock-held (pause-lock)
                    (setf release-admission-p t)
                    (task--condition-broadcast pause-condition))
                  (join-thread admission-thread)
                  (setf admission-thread nil)
                  (test-assert
                   (and (typep admission-failure 'job-aborted) (null admitted))
                   "pool admission refuses a parent cancelled after the courtesy check")
                  (test-assert
                   (and (nth-value 1 (job-await parent :timeout-seconds 2))
                        (eq (job-state parent) ':aborted)
                        (= (length (task-orchestrator-list-jobs orchestrator)) 1)
                        (zerop (task-orchestrator-live-count orchestrator)))
                   "the refused admission creates no child or live capacity")))
             (with-lock-held (pause-lock)
               (setf release-admission-p t)
               (task--condition-broadcast pause-condition))
             (when admission-thread (join-thread admission-thread))))
         (let* ((orchestrator (task-tests--orchestrator))
                (parent
                 (task-tests--register-job orchestrator primary definition :name
                                           "cancel-first-parent"))
                (viewer       (task-tests--child-viewer configuration parent))
                (entry
                 (list :item
                       (append
                        (list :name "too-late" :task
                              "This child must not be admitted." :context nil :async t)
                        definition)
                       :detached t)))
           (task-job-cancel parent :user)
           (test-assert
            (handler-case
             (progn (task-orchestrator-start-jobs orchestrator viewer (list entry)) nil)
             (job-aborted nil t))
            "admission loses atomically when parent cancellation wins")
           (test-assert
            (and (nth-value 1 (job-await parent :timeout-seconds 2))
                 (=
                  (hash-table-count
                   (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                  1)
                 (zerop (task-orchestrator-live-count orchestrator)))
            "cancel-first admission consumes no live capacity")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-hurry-up-admission-races () null)
(defun test-task-hurry-up-admission-races ()
  "Test hurry-up reservations and pool-limit reconfiguration ordering."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "hurry-race-primary")))
    (labels ((make-viewer (orchestrator identifier)
               (task-tests--child-viewer configuration
                                         (task-tests--make-job orchestrator :item
                                                               (append
                                                                (list :name  identifier
                                                                      :task
                                                                      "Own inline admission test children."
                                                                      :context nil :async
                                                                      nil)
                                                                definition)
                                                               :parent-agent primary
                                                               :identifier   identifier)))

             (make-entry (name)
               (list
                (list :item
                      (append
                       (list :name name :task
                             "Remain inline for admission accounting." :context nil
                             :async nil)
                       definition)
                      :detached nil))))
      (unwind-protect
          (progn
           (let* ((orchestrator (task-tests--orchestrator))
                  (pool         (task-orchestrator-pool orchestrator)))
             (task-orchestrator-set-hurry-up orchestrator nil)
             (test-assert
              (and (null (cl-jobpond::job-pool--worker-threads pool))
                   (null (cl-jobpond::job-pool--monitor-thread pool)))
              "reapplying inactive hurry-up keeps an unused task pool lazy"))
           (let* ((orchestrator (task-tests--orchestrator))
                  (viewer       (make-viewer orchestrator "rollback-parent"))
                  (pool         (task-orchestrator-pool orchestrator))
                  (refused-p    nil))
             (task-orchestrator-set-hurry-up orchestrator t)
             (with-lock-held ((task-orchestrator-lock orchestrator))
               (setf (job-pool-maximum-live-jobs pool) 1))
             (task-orchestrator-start-jobs orchestrator viewer (make-entry "occupy"))
             (handler-case
              (task-orchestrator-start-jobs orchestrator viewer (make-entry "refused"))
              (task-error nil (setf refused-p t)))
             (test-assert
              (and refused-p
                   (= (task-orchestrator-hurry-up-admission-count orchestrator) 1)
                   (= (task-orchestrator-live-count orchestrator) 1))
              "failed pool admission rolls back its hurry-up reservation")
             (with-lock-held ((task-orchestrator-lock orchestrator))
               (setf (job-pool-maximum-live-jobs pool) *task-hurry-up-maximum-agents*))
             (task-orchestrator-start-jobs orchestrator viewer (make-entry "accepted"))
             (test-assert
              (and (= (task-orchestrator-hurry-up-admission-count orchestrator) 2)
                   (= (task-orchestrator-live-count orchestrator) 2))
              "a rolled-back reservation leaves room for the next child"))
           (let* ((orchestrator           (task-tests--orchestrator))
                  (viewer                 (make-viewer orchestrator "concurrent-parent"))
                  (pool                   (task-orchestrator-pool orchestrator))
                  (gate-lock              (make-lock "Autolith hurry race gate"))
                  (gate-condition         (make-condition-variable))
                  (gate-count             0)
                  (release-gate-p         nil)
                  (result-lock            (make-lock "Autolith hurry race results"))
                  (success-count          0)
                  (conditions             nil)
                  (threads                nil)
                  (original-configuration (symbol-function 'task-child-configuration)))
             (task-orchestrator-set-hurry-up orchestrator t)
             (with-lock-held ((task-orchestrator-lock orchestrator))
               (setf (job-pool-maximum-live-jobs pool) *task-maximum-live-jobs*))
             (test-call-with-function-replacements
              (list
               (list 'task-child-configuration
                     (lambda (parent-configuration child-definition)
                       (with-lock-held (gate-lock)
                         (incf gate-count)
                         (when (= gate-count 3)
                           (setf release-gate-p t)
                           (task--condition-broadcast gate-condition))
                         (loop until release-gate-p
                               unless (condition-wait gate-condition gate-lock :timeout
                                                      2)
                               do (error
                                   "Timed out synchronizing hurry-up admissions.")))
                       (funcall original-configuration parent-configuration
                                child-definition))))
              (lambda ()
                (setf threads
                        (loop for index from 1 to 3
                              collect (make-thread
                                       (lambda ()
                                         (handler-case
                                          (progn
                                           (task-orchestrator-start-jobs orchestrator
                                                                         viewer
                                                                         (make-entry
                                                                          (format nil
                                                                                  "concurrent-~D"
                                                                                  index)))
                                           (with-lock-held (result-lock)
                                             (incf success-count)))
                                          (task-error (condition)
                                           (with-lock-held (result-lock)
                                             (push condition conditions)))))
                                       :name
                                       (format nil "Autolith hurry admission ~D"
                                               index))))
                (dolist (thread threads) (join-thread thread))))
             (test-assert
              (and (= success-count 2) (= (length conditions) 1)
                   (= (task-orchestrator-hurry-up-admission-count orchestrator) 2)
                   (= (task-orchestrator-live-count orchestrator) 2))
              "concurrent child.spawn calls atomically share the two-child allowance"))
           (let* ((orchestrator               (task-tests--orchestrator))
                  (viewer                     (make-viewer orchestrator "limit-parent"))
                  (submit-lock                (make-lock "Autolith limit race submit"))
                  (submit-condition           (make-condition-variable))
                  (submission-entered-p       nil)
                  (release-submission-p       nil)
                  (state-lock                 (make-lock "Autolith limit race state"))
                  (reconfiguration-started-p  nil)
                  (reconfiguration-finished-p nil)
                  (jobs                       nil)
                  (submission-condition       nil)
                  (submission-thread          nil)
                  (reconfiguration-thread     nil)
                  (original-submit            (symbol-function 'job-pool-submit-batch)))
             (unwind-protect
                 (test-call-with-function-replacements
                  (list
                   (list 'job-pool-submit-batch
                         (lambda (pool entries)
                           (with-lock-held (submit-lock)
                             (setf submission-entered-p t)
                             (task--condition-broadcast submit-condition)
                             (loop until release-submission-p
                                   do (condition-wait submit-condition submit-lock)))
                           (funcall original-submit pool entries))))
                  (lambda ()
                    (setf submission-thread
                            (make-thread
                             (lambda ()
                               (handler-case
                                (setf jobs
                                        (first
                                         (multiple-value-list
                                          (task-orchestrator-start-jobs orchestrator
                                                                        viewer
                                                                        (make-entry
                                                                         "limit")))))
                                (condition (condition)
                                 (setf submission-condition condition))))
                             :name "Autolith limit race admission"))
                    (with-lock-held (submit-lock)
                      (loop until submission-entered-p
                            unless (condition-wait submit-condition submit-lock :timeout
                                                   2)
                            do (error "Timed out waiting for pool submission.")))
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
               (when submission-thread (join-thread submission-thread))
               (when reconfiguration-thread (join-thread reconfiguration-thread)))
             (test-assert
              (and (null submission-condition) (= (length jobs) 1)
                   (task-orchestrator-hurry-up-p orchestrator)
                   (= (task-orchestrator-hurry-up-admission-count orchestrator) 1))
              "a limit change takes effect after the atomic submission boundary")))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
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
  "Test coherent snapshots and forced terminal publication failures."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition    (list :reasoning-effort "high"))
         (primary       (task-tests--primary-agent configuration "publication-primary")))
    (unwind-protect
        (progn
         (let* ((job
                 (task-tests--register-job orchestrator primary definition :name
                                           "coherent-publication"))
                (barrier               (make-instance 'task-test-publication-barrier))
                (result
                 (task-tests--terminal-result job :status ':success :output "published"))
                (publication-result    nil)
                (publication-condition nil))
           (setf (getf result :publication-barrier) barrier
                 (getf result :portable-integer) 42)
           (let ((thread
                  (make-thread
                   (lambda ()
                     (let ((*print-base* 2) (*print-radix* nil) (*print-case* :downcase))
                       (handler-case
                        (setf publication-result
                                (task-tests--publish-terminal job :completed result))
                        (condition (condition) (setf publication-condition condition)))))
                   :name "Autolith coherent publication")))
             (unwind-protect
                 (progn
                  (test-assert
                   (task-tests--wait-until
                    (lambda ()
                      (with-lock-held ((task-test-publication-barrier-lock barrier))
                        (task-test-publication-barrier-reached-p barrier)))
                    2)
                   "terminal publication reaches its post-claim artifact phase")
                  (dotimes (index 32)
                    (declare (ignorable index))
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
                  (artifact
                   (task-tests--read-exact-native-value
                    (uiop/stream:read-file-string
                     (getf (getf snapshot :result) :output-path)))))
             (test-assert
              (and publication-result (null publication-condition)
                   (eq (getf snapshot :state) :completed)
                   (eq (getf (getf snapshot :result) :status) :success)
                   (null (cl-jobpond::job--publication-claimed-p job))
                   (cl-jobpond::job--retained-p job)
                   (zerop (task-orchestrator-live-count orchestrator)) (listp artifact)
                   (= (getf artifact :portable-integer) 42))
              "publication moves atomically from a public live snapshot to terminal state")))
         (dolist (failure '(:error :abort))
           (let* ((job
                   (task-tests--register-job orchestrator primary definition :name
                                             (format nil "publication-~A" failure)))
                  (barrier
                   (make-instance 'task-test-publication-barrier :failure failure))
                  (result
                   (task-tests--terminal-result job :status ':success :output
                                                (make-string 3000 :initial-element #\O)))
                  (publication-result    nil)
                  (publication-condition nil))
             (setf (getf result :publication-barrier) barrier)
             (let ((thread
                    (make-thread
                     (lambda ()
                       (handler-case
                        (setf publication-result
                                (task-tests--publish-terminal job :completed result))
                        (condition (condition) (setf publication-condition condition))))
                     :name (format nil "Autolith publication ~A" failure))))
               (unwind-protect
                   (test-assert
                    (task-tests--wait-until
                     (lambda ()
                       (with-lock-held ((task-test-publication-barrier-lock barrier))
                         (task-test-publication-barrier-reached-p barrier)))
                     2)
                    "failing publication reaches the post-claim barrier")
                 (task-tests--release-publication-barrier barrier))
               (join-thread thread))
             (let* ((snapshot        (task-job-snapshot job))
                    (terminal-result (getf snapshot :result))
                    (output-path     (getf terminal-result :output-path)))
               (test-assert
                (and publication-result (null publication-condition)
                     (eq (getf snapshot :state) :failed)
                     (eq (getf terminal-result :status) :failed)
                     (cl-jobpond::job--retained-p job)
                     (null (cl-jobpond::job--publication-claimed-p job))
                     (not
                      (and (null output-path)
                           (or (eq (getf terminal-result :output-storage) :artifact)
                               (eq (getf terminal-result :error-storage) :artifact))))
                     (zerop (task-orchestrator-live-count orchestrator)))
                (format nil "post-claim ~A forces one coherent terminal failure"
                        failure))))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-terminal-wakeup-ordering () null)
(defun test-task-terminal-wakeup-ordering ()
  "Test that terminal waiters wake before arbitrary lifecycle listeners run."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (orchestrator        (task-tests--orchestrator))
         (definition          (list))
         (primary             (task-tests--primary-agent configuration "wakeup-primary"))
         (job
          (task-tests--register-job orchestrator primary definition :name "wakeup-job"))
         (result
          (task-tests--terminal-result job :status ':success :output "wake the waiter"))
         (ready-lock          (make-lock "Autolith waiter readiness"))
         (ready-condition     (make-condition-variable))
         (waiter-ready-p      nil)
         (waiter-returned-p   nil)
         (listener-lock       (make-lock "Autolith blocking lifecycle listener"))
         (listener-condition  (make-condition-variable))
         (listener-reached-p  nil)
         (listener-released-p nil)
         (waiter              nil)
         (publisher           nil))
    (task-orchestrator-add-listener orchestrator
                                    (lambda (channel payload)
                                      (declare (ignore payload))
                                      (when (eq channel :task-subagent-lifecycle)
                                        (with-lock-held (listener-lock)
                                          (setf listener-reached-p t)
                                          (task--condition-broadcast listener-condition)
                                          (loop until listener-released-p
                                                do (condition-wait listener-condition
                                                                   listener-lock))))))
    (unwind-protect
        (progn
         (setf waiter
                 (make-thread
                  (lambda ()
                    (with-lock-held ((cl-jobpond::job--lock job))
                      (with-lock-held (ready-lock)
                        (setf waiter-ready-p t)
                        (task--condition-broadcast ready-condition))
                      (loop until (task--terminal-state-p (job-state job))
                            do (condition-wait (cl-jobpond::job--condition-variable job)
                                               (cl-jobpond::job--lock job))))
                    (setf waiter-returned-p t))
                  :name "Autolith terminal waiter"))
         (test-assert (task-tests--wait-until (lambda () waiter-ready-p) 2)
                      "the terminal waiter is parked before publication")
         (setf publisher
                 (make-thread
                  (lambda () (task-tests--publish-terminal job :completed result)) :name
                  "Autolith listener-blocked publisher"))
         (test-assert (task-tests--wait-until (lambda () listener-reached-p) 2)
                      "terminal publication reaches the blocking lifecycle listener")
         (let ((woke-before-listener-release-p
                (task-tests--wait-until (lambda () waiter-returned-p) 0.5)))
           (with-lock-held (listener-lock)
             (setf listener-released-p t)
             (task--condition-broadcast listener-condition))
           (join-thread publisher)
           (setf publisher nil)
           (join-thread waiter)
           (setf waiter nil)
           (test-assert woke-before-listener-release-p
                        "terminal publication wakes waiters before invoking listeners")))
      (with-lock-held (listener-lock)
        (setf listener-released-p t)
        (task--condition-broadcast listener-condition))
      (when publisher (join-thread publisher))
      (when waiter
        (with-lock-held ((cl-jobpond::job--lock job))
          (task--condition-broadcast (cl-jobpond::job--condition-variable job)))
        (join-thread waiter))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-job-visibility () null)
(defun test-task-job-visibility ()
  "Test conversation ownership, child ancestry, and opaque job lookup errors."
  (let* ((configuration        (test-configuration))
         (root                 (test-configuration-root configuration))
         (orchestrator         (task-tests--orchestrator))
         (missing-orchestrator (task-tests--orchestrator))
         (definition           (list))
         (primary-a            (task-tests--primary-agent configuration "visibility-primary-a"))
         (primary-b            (task-tests--primary-agent configuration "visibility-primary-b")))
    (unwind-protect
        (let* ((root-a
                (task-tests--register-job orchestrator primary-a definition :name
                                          "root-a"))
               (viewer-a              (task-tests--child-viewer configuration root-a))
               (descendant-a
                (task-tests--register-job orchestrator viewer-a definition :name
                                          "descendant-a"))
               (sibling-a
                (task-tests--register-job orchestrator primary-a definition :name
                                          "sibling-a"))
               (sibling-viewer        (task-tests--child-viewer configuration sibling-a))
               (sibling-descendant
                (task-tests--register-job orchestrator sibling-viewer definition :name
                                          "sibling-descendant"))
               (foreign
                (task-tests--register-job orchestrator primary-b definition :name
                                          "foreign"))
               (root-a-id             (job-identifier root-a))
               (descendant-a-id       (job-identifier descendant-a))
               (sibling-a-id          (job-identifier sibling-a))
               (sibling-descendant-id (job-identifier sibling-descendant))
               (foreign-id            (job-identifier foreign)))
          (test-assert
           (equal
            (mapcar (lambda (job) (job-identifier job))
                    (task-orchestrator-list-visible-jobs orchestrator primary-a))
            (list root-a-id descendant-a-id sibling-a-id sibling-descendant-id))
           "a primary sees only jobs owned by its conversation")
          (test-assert
           (equal
            (mapcar (lambda (job) (job-identifier job))
                    (task-orchestrator-list-visible-jobs orchestrator primary-b))
            (list foreign-id))
           "another primary cannot see the first conversation's task tree")
          (test-assert
           (equal
            (mapcar (lambda (job) (job-identifier job))
                    (task-orchestrator-list-visible-jobs orchestrator viewer-a))
            (list descendant-a-id))
           "a child sees descendants but not itself, its parent, or siblings")
          (dolist (operation '("wait" "cancel"))
            (let ((invisible-report
                   (task-tests--job-tool-error-report orchestrator primary-a :operation
                                                      operation :identifier foreign-id))
                  (missing-report
                   (task-tests--job-tool-error-report missing-orchestrator primary-a
                                                      :operation operation :identifier
                                                      foreign-id)))
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-durable-job-lookup () null)
(defun test-task-durable-job-lookup ()
  "Test live inspection and conversation-scoped durable result lookup."
  (let* ((configuration        (test-configuration))
         (root                 (test-configuration-root configuration))
         (registry             (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool             (tool-registry-find registry "child" "spawn"))
         (get-tool             (tool-registry-find registry "job" "get"))
         (orchestrator         (task-run-tool-orchestrator run-tool))
         (definition           (list))
         (primary-a
          (task-tests--primary-agent configuration "durable-lookup-primary-a" registry))
         (primary-b
          (task-tests--primary-agent configuration "durable-lookup-primary-b" registry))
         (live-job
          (task-tests--register-job orchestrator primary-a definition :name
                                    "live-lookup"))
         (child                (task-tests--child-viewer configuration live-job :registry registry))
         (execution-identifier (make-identifier))
         (artifact-path
          (merge-pathnames (format nil "~A/result.sexp" execution-identifier)
                           (task--artifact-group-root configuration
                                                      (conversation-identifier
                                                       (agent-conversation primary-a)))))
         (durable-result
          (list :id "resumed-child-1" :name "resumed-child" :status
                ':success :output "Recovered durable child output." :error nil
                :duration-ms 17 :model "test-model" :detached t)))
    (labels ((execute-get (viewer identifier)
               (tool-execute get-tool
                             (make-instance 'tool-context :configuration configuration
                                            :worker nil :conversation
                                            (agent-conversation viewer) :registry
                                            registry :agent viewer :call-id
                                            "durable-job-lookup")
                             (json-object "id" identifier))))
      (unwind-protect
          (progn
           (snapshot-write artifact-path durable-result)
           (let* ((result  (execute-get primary-a (session-job-identifier live-job)))
                  (details (rest (tool-result-details result)))
                  (record  (getf details :job)))
             (test-assert
              (and (tool-result-success-p result) (null (getf details :durable-p))
                   (string= (getf record :id) (session-job-identifier live-job))
                   (eq (getf record :state) :queued))
              "job.get inspects a retained live session job"))
           (let* ((result        (execute-get primary-a execution-identifier))
                  (details       (rest (tool-result-details result)))
                  (record        (getf details :job))
                  (result-record (getf record :result))
                  (artifact      (getf result-record :artifact)))
             (test-assert
              (and (tool-result-success-p result) (getf details :durable-p)
                   (string= (getf record :id) "resumed-child-1")
                   (string= (getf record :execution-id) execution-identifier)
                   (eq (getf record :state) :completed)
                   (eq (getf result-record :status) :success)
                   (string= (getf result-record :output)
                            "Recovered durable child output.")
                   (string= (getf artifact :path) (namestring artifact-path))
                   (getf artifact :available-p))
              "job.get reconstructs a bounded terminal record from its artifact"))
           (snapshot-write artifact-path '(:status :success :status :failed))
           (test-assert
            (handler-case (progn (execute-get primary-a execution-identifier) nil)
                          (task-error (condition)
                           (string= (tool-error-tool-name condition) "job.get")))
            "job.get reports malformed durable result records at the task boundary")
           (snapshot-write artifact-path durable-result)
           (let ((*task-result-maximum-octets* 16))
             (test-assert
              (handler-case (progn (execute-get primary-a execution-identifier) nil)
                            (task-error nil t))
              "job.get bounds durable artifacts before reconstructing results"))
           (dolist (viewer (list primary-b child))
             (let ((report
                    (task-tests--job-tool-error-report orchestrator viewer :operation
                                                       "get" :identifier
                                                       execution-identifier)))
               (test-assert (search "No visible job" report)
                            "durable lookup does not expose another conversation or task branch"))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-session-tool-execution-jobs () null)
(defun test-session-tool-execution-jobs ()
  "Test shared ordering, visibility, inspection, waiting, and cancellation."
  (let* ((configuration   (test-configuration))
         (root            (test-configuration-root configuration))
         (registry        (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool        (tool-registry-find registry "child" "spawn"))
         (orchestrator    (task-run-tool-orchestrator run-tool))
         (definition      (list))
         (primary-a
          (task-tests--primary-agent configuration "execution-primary-a" registry))
         (primary-b
          (task-tests--primary-agent configuration "execution-primary-b" registry))
         (context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation (agent-conversation primary-a) :registry registry
                         :agent primary-a :call-id "execution-job-test"))
         (barrier
          (make-instance 'task-test-blocking-tool :namespace "test" :name
                         "execution-block" :description
                         "Hold one execution job for inspection." :parameters
                         (tool-object-schema (json-object) nil)))
         (root-job        nil)
         (execution-job   nil)
         (operation-count 0))
    (labels ((execute-job-tool (name arguments)
               (tool-execute (tool-registry-find registry "job" name) context arguments)))
      (unwind-protect
          (progn
           (setf root-job
                   (task-tests--register-job orchestrator primary-a definition :name
                                             "execution-root"))
           (let ((child
                  (task-tests--child-viewer configuration root-job :registry registry)))
             (setf execution-job
                     (task-orchestrator-start-execution-job orchestrator child :tool-name
                                                            "lisp.eval" :summary
                                                            "Wait inside one inspectable execution job."
                                                            :operation-function
                                                            (lambda ()
                                                              (incf operation-count)
                                                              (tool-execute barrier
                                                                            context
                                                                            (json-object)))
                                                            :detached-p t :parent-call-id
                                                            "execution-job-test"))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held ((task-test-blocking-tool-lock barrier))
                   (task-test-blocking-tool-started-p barrier)))
               2)
              "the asynchronous operation starts on the execution pool")
             (let ((root-id      (session-job-identifier root-job))
                   (execution-id (session-job-identifier execution-job)))
               (test-assert
                (and
                 (string= execution-id
                          (format nil "exec:~D" (session-job-order execution-job)))
                 (not (string= root-id execution-id))
                 (equal
                  (mapcar #'session-job-identifier
                          (task-orchestrator-list-jobs orchestrator))
                  (list root-id execution-id)))
                "child and execution jobs have distinct globally ordered identities")
               (test-assert
                (equal
                 (mapcar #'session-job-identifier
                         (task-orchestrator-list-visible-jobs orchestrator primary-a))
                 (list root-id execution-id))
                "the owning primary sees task and execution jobs")
               (test-assert
                (equal
                 (mapcar #'session-job-identifier
                         (task-orchestrator-list-visible-jobs orchestrator child))
                 (list execution-id))
                "a child sees its execution job but not its own task job")
               (test-assert
                (null (task-orchestrator-list-visible-jobs orchestrator primary-b))
                "another primary cannot inspect the execution job")
               (let* ((list-result
                       (execute-job-tool "list" (json-object "offset" 0 "limit" 32)))
                      (records (getf (rest (tool-result-details list-result)) :jobs)))
                 (test-assert
                  (and (tool-result-success-p list-result)
                       (equal (mapcar (lambda (record) (getf record :id)) records)
                              (list root-id execution-id))
                       (equal (mapcar (lambda (record) (getf record :type)) records)
                              '(:task :tool))
                       (string= (getf (second records) :tool) "lisp.eval"))
                  "job.list presents child and tool execution jobs"))
               (let* ((inspect-result
                       (execute-job-tool "wait"
                        (json-object "id" execution-id "timeout-seconds" 0)))
                      (record (getf (rest (tool-result-details inspect-result)) :job)))
                 (test-assert
                  (and (tool-result-success-p inspect-result)
                       (eq (getf record :type) :tool) (eq (getf record :state) :running)
                       (string= (getf record :tool) "lisp.eval"))
                  "job.wait with zero timeout renders a live execution job"))
               (let* ((wait-result
                       (execute-job-tool "wait"
                        (json-object "id" execution-id "timeout-seconds" 0)))
                      (details (rest (tool-result-details wait-result))))
                 (test-assert
                  (and (tool-result-success-p wait-result)
                       (not (getf details :terminal-p))
                       (eq (getf (getf details :job) :state) :running))
                  "job.wait returns a nonterminal execution snapshot on timeout"))
               (let* ((cancel-result
                       (execute-job-tool "cancel" (json-object "id" root-id)))
                      (details (rest (tool-result-details cancel-result))))
                 (test-assert
                  (and (tool-result-success-p cancel-result) (getf details :accepted-p)
                       (member execution-id (getf details :cancelled-descendants) :test
                               #'string=))
                  "cancelling a task job also cancels owned execution jobs"))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await execution-job 2)
                 (test-assert
                  (and terminal-p (= operation-count 1)
                       (eq (getf snapshot :state) :aborted)
                       (eq (getf (getf snapshot :result) :status) :aborted)
                       (null (tool-execution-job-operation-function execution-job)))
                  "the execution runs once, aborts, and releases its closure")))))
        (task-tests--release-blocking-tool barrier)
        (when (and execution-job (not (job-terminal-p execution-job)))
          (session-job-cancel execution-job :test-cleanup)
          (session-job-await execution-job 2))
        (when (and root-job (not (job-terminal-p root-job)))
          (task-job-cancel root-job :test-cleanup)
          (task-job-await root-job 2))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-execution-admission-presentation () null)
(defun test-execution-admission-presentation ()
  "Present admitted shell work even while every execution worker is occupied."
  (with-test-configuration (configuration)
    (let* ((registry     (task-augment-tool-registry (make-instance 'tool-registry)))
           (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
           (parent       (task-tests--primary-agent configuration "queued-shell" registry))
           (terminal     (make-instance 'recording-terminal :columns 100 :rows 24))
           (ui           (terminal-ui-create :terminal terminal))
           (application  (make-instance 'application :configuration configuration
                                        :conversation               (agent-conversation parent)
                                        :tool-registry registry :ui ui))
           (lock         (make-lock "Queued shell presentation"))
           (condition    (make-condition-variable))
           (ready-p      nil)
           (release-p    nil))
      (unwind-protect
           (with-terminal-ui (ui ui)
             (let ((pool (task-orchestrator-execution-pool orchestrator)))
               (job-pool-update-limits
                pool :maximum-concurrency     1
                :maximum-batch-size           (job-pool-maximum-batch-size pool)
                :maximum-live-jobs            (job-pool-maximum-live-jobs pool)
                :maximum-runtime-milliseconds (job-pool-maximum-runtime-milliseconds pool)))
             (application-connect-task-presentation application)
             (task-orchestrator-start-execution-job
              orchestrator parent :tool-name "shell.run" :summary "held command"
              :operation-function
              (lambda ()
                (with-lock-held (lock)
                  (setf ready-p t)
                  (task--condition-broadcast condition)
                  (loop until release-p do (condition-wait condition lock :timeout 2)))
                (make-instance 'tool-result :content "done")))
             (test-assert (task-tests--wait-until (lambda () ready-p) 5)
                          "the first execution occupies the only worker")
             (let* ((job        (task-orchestrator-start-execution-job
                                 orchestrator parent :tool-name "shell.run" :summary "queued command"
                                 :operation-function (lambda () (make-instance 'tool-result :content "done"))))
                    (identifier (session-job-identifier job))
                    (row        (find identifier (terminal-ui-command-activities ui)
                                      :key (lambda (activity) (getf activity :id)) :test #'equal)))
               (test-assert (and (eq (job-state job) ':queued) row
                                 (eq (getf row :state) ':queued)
                                 (not (getf row :detached)))
                            "synchronous queued work reaches the prompt area before a worker starts it")
               (recording-terminal-reset terminal)
               (terminal-ui-refresh-status ui)
               (test-assert (search identifier (recording-terminal-output terminal))
                            "the reader can paint the queued execution's identity")))
        (with-lock-held (lock)
          (setf release-p t)
          (task--condition-broadcast condition))
        (application-disconnect-task-presentation application)
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-shell-execution-jobs () null)
(defun test-shell-execution-jobs ()
  "Test authorized shell jobs, automatic handoff, inspection, and cancellation."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (shell-tool          (tool-registry-find registry "shell" "run"))
         (run-tool            (tool-registry-find registry "child" "spawn"))
         (orchestrator        (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "shell-execution-primary" registry))
         (authorization-count 0)
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              (incf authorization-count)
              ':full-access)))
         (denied-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-denied-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              ':deny)))
         (sandboxed-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-sandboxed-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              ':sandboxed)))
         (unavailable-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-unavailable-test"
            :command-authorization-function
            (lambda (command directory)
              (error 'command-authorization-unavailable
                     :message   "Command approval requires an interactive terminal."
                     :command   command
                     :directory directory))))
         (slow-path           (merge-pathnames "shell-execution-once.txt" root))
         (started-path        (merge-pathnames "shell-execution-started.txt" root))
         (finished-path       (merge-pathnames "shell-execution-finished.txt" root)))
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
               (let* ((details    (tool-result-details result))
                      (record     (and (listp details)
                                       (getf (rest details) :job)))
                      (identifier (and record (getf record :id))))
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier :viewer primary :tool-name "shell.run")))))
      (unwind-protect
           (progn
             (let* ((parameters   (tool-parameters shell-tool))
                    (properties   (json-get parameters "properties"))
                    (required     (json-get parameters "required"))
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
             (with-test-fixture (':posix-shell "shell execution below the POSIX root")
               (let ((result
                       (run-shell context
                                  "command" "pwd -P"
                                  "directory" "/")))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search (format nil "~%/~%")
                               (tool-result-content result)))
                  "full-access shell execution accepts a directory outside workspace roots")))
             (test-assert
              (handler-case
                  (progn
                    (run-shell sandboxed-context
                               "command" "pwd -P"
                               "directory" "/")
                    nil)
                (tool-error (condition)
                  (search "outside the allowed workspace roots"
                          (princ-to-string condition))))
              "sandboxed shell execution still rejects a directory outside workspace roots")
             (let* ((*tool-execution-blocking-grace-seconds* 2)
                    (before                                  (execution-count))
                    (authorizations-before                   authorization-count)
                    (result
                      (run-shell context
                                 "command" (test-fixture-shell-command
                                            *platform* "printf fast-shell"
                                            "[Console]::Write('fast-shell')"))))
               (test-assert
                (and (tool-result-success-p result)
                     (eq (tool-result-category result) ':success)
                     (= 0 (getf (tool-result-details result) :exit-code))
                     (search "exit 0" (tool-result-content result))
                     (search "fast-shell" (tool-result-content result))
                     (= (execution-count) (1+ before))
                     (= authorization-count (1+ authorizations-before)))
                "a fast default shell job returns its ordinary result"))
             (let* ((*tool-execution-blocking-grace-seconds* 0.01)
                    (before                                  (execution-count))
                    (result
                      (run-shell
                       context
                       "command"
                       (format nil
                               (test-fixture-shell-command
                                *platform* "sleep 1; printf once >> ~A"
                                "Start-Sleep 1; [IO.File]::AppendAllText(~A,'once')")
                               (test-fixture-shell-quote *platform* (namestring slow-path)))))
                    (details                                 (tool-result-details result))
                    (job                                     (handoff-job result))
                    (identifier                              (and job (session-job-identifier job))))
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
                      (summary  (tool-execution-job-summary job)))
                 (test-assert
                  (and (null (tool-execution-job-description job))
                       (string= (getf activity :description) summary)
                       (<= (length summary) *tool-execution-summary-limit*)
                       (search "sleep 1" (string-downcase summary)))
                  "an omitted description uses the retained bounded job summary"))
               (let* ((wait-result
                        (tool-execute
                         (tool-registry-find registry "job" "wait")
                         context
                         (json-object "id" identifier
                                      "timeout-seconds" 2)))
                      (wait-details (rest (tool-result-details wait-result)))
                      (record       (getf wait-details :job)))
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
                               (test-fixture-shell-command
                                *platform* "printf started > ~A; sleep 1; printf finished > ~A"
                                "[IO.File]::WriteAllText(~A,'started'); Start-Sleep 1; [IO.File]::WriteAllText(~A,'finished')")
                               (test-fixture-shell-quote *platform* (namestring started-path))
                               (test-fixture-shell-quote *platform* (namestring finished-path)))
                       "async" t))
                    (details    (tool-result-details result))
                    (job        (handoff-job result))
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
                                   "command" (test-fixture-shell-command
                                              *platform* "printf 123456789"
                                              "[Console]::Write('123456789')")
                                   "async" t)))
                    (job (handoff-job result)))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (let* ((record  (getf snapshot :result))
                        (content (getf record :content))
                        (capture (first (getf (first (getf record :shell-logs)) :captures))))
                   (test-assert
                    (and terminal-p
                         (search "6789" content)
                         (not (search "123456789" content))
                         (= 9 (getf capture :byte-count))
                         (getf capture :complete-p))
                    "An asynchronous shell job bounds its preview and retains complete raw output")))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t
                                             :if-does-not-exist   ':ignore))))
  nil)

(-> test-tool-execution-overflow-binding () null)
(defun test-tool-execution-overflow-binding ()
  "Test lossless result spilling inside execution-pool worker threads."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool      (tool-registry-find registry "child" "spawn"))
         (orchestrator  (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "execution-overflow-primary" registry)))
    (unwind-protect
         (let* ((*tool-result-overflow-function*
                  (lambda (text)
                    (declare (ignore text))
                    "context:worker-overflow"))
                (result
                  (tool-execution-invoke
                   orchestrator primary
                   :tool-name "test.overflow"
                   :summary   "Return one oversized worker result."
                   :operation-function
                   (lambda ()
                     (tool-success (make-string 9000 :initial-element #\x)))
                   :async-p   nil)))
           (test-assert
            (and (tool-result-success-p result)
                 (search "context:worker-overflow"
                         (tool-result-content result)))
            "execution workers inherit the complete-result spill function"))
      (ignore-errors (task-orchestrator-close orchestrator))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist   ':ignore)))
  nil)

(-> test-tool-execution-retention () null)
(defun test-tool-execution-retention ()
  "Test bounded execution retention and complete two-pool detachment."
  (let* ((*tool-execution-terminal-retention-limit* 2)
         (configuration                             (test-configuration))
         (root                                      (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool                                  (tool-registry-find registry "child" "spawn"))
         (orchestrator                              (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "execution-retention-primary" registry))
         (identifiers                               nil)
         (closed-p                                  nil))
    (unwind-protect
         (progn
           (dotimes (index 3)
             (let ((job
                     (task-orchestrator-start-execution-job
                      orchestrator primary
                      :tool-name  "lisp.eval"
                      :summary    (format nil "retained execution ~D" index)
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
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist   ':ignore)))
  nil)

(-> test-tool-execution-job-snapshot-bound () null)
(defun test-tool-execution-job-snapshot-bound ()
  "Test that job.get bounds a finished tool job only when its output does not fit."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (task-augment-tool-registry (make-default-tool-registry)))
         (orchestrator  (task-run-tool-orchestrator
                         (tool-registry-find registry "child" "spawn")))
         (primary       (task-tests--primary-agent
                         configuration "execution-snapshot-bound-primary" registry)))
    (flet ((finished-job (text &key failure-p)
             (let ((job (task-orchestrator-start-execution-job
                         orchestrator primary
                         :tool-name  "lisp.eval"
                         :summary    "snapshot bound"
                         :operation-function
                         (lambda ()
                           (let ((details (list :evaluation (list :output text :values nil))))
                             (if failure-p
                                 (tool-failure text :details details)
                                 (tool-success text :details details))))
                         :detached-p t)))
               (test-assert (nth-value 1 (session-job-await job 2))
                            "the tool job finishes")
               job))
           (job-get-record (job &optional (snapshot (session-job-snapshot job)))
             (let ((failure nil) (form nil) (content nil))
               (handler-case
                   (multiple-value-setq (form content)
                     (task--job-native-form
                      job snapshot primary
                      :preview-limit 6000
                      :wrapper       (lambda (record)
                                       (list :job-get :durable-p nil :job record))))
                 (task-error (condition)
                   (setf failure (princ-to-string condition))))
               (values (getf (rest form) :job) content failure)))
           (terminal-path (job)
             (merge-pathnames
              (format nil "~A/terminal.sexp" (session-job-execution-identifier job))
              (task--artifact-group-root (agent-configuration primary)
                                         (session-job-root-conversation-identifier job)))))
      (unwind-protect
           (let ((large (finished-job (make-string 20000 :initial-element #\x)))
                 (small (finished-job "small result")))
             (multiple-value-bind (record content failure) (job-get-record large)
               (test-assert (null failure)
                            (format nil "job.get renders a large tool job~@[: ~A~]" failure))
               (let* ((result        (getf record :result))
                      (content-field (getf result :content)))
                 (test-assert (and content (<= (length content) *task-tool-content-limit*))
                              "the rendered snapshot fits the tool content limit")
                 (test-assert (and (listp content-field)
                                   (stringp (getf content-field :preview))
                                   (or (getf content-field :omitted)
                                       (getf content-field :in-artifact)))
                              "large content keeps a preview and an omission marker")
                 (test-assert (and (string= (getf record :id) (session-job-identifier large))
                                   (string= (getf record :tool) "lisp.eval")
                                   (eq (getf record :state) :completed)
                                   (eq (getf result :status) :success))
                              "the bounded snapshot identifies the job and its outcome")))
             (let ((result (getf (job-get-record small) :result)))
               (test-assert (and (equal (getf result :content) "small result")
                                 (equal (getf result :details)
                                        '(:evaluation (:output "small result" :values nil))))
                            "small tool results are shown inline unchanged"))
             ;; Above the preview limit and within the content limit, job.get
             ;; shows exactly the record it showed before bounding existed.
             (let* ((medium   (finished-job (make-string 7000 :initial-element #\m)))
                    (snapshot (session-job-snapshot medium))
                    (result   (getf snapshot :result))
                    (expected
                      (list :job-get :durable-p nil :job
                            (list :id                  (getf snapshot :job-id)
                                  :execution-id        (getf snapshot :execution-id)
                                  :type                :tool
                                  :tool                "lisp.eval"
                                  :description         (getf snapshot :description)
                                  :summary             (getf snapshot :summary)
                                  :state               (getf snapshot :state)
                                  :detached            (and (getf snapshot :detached) t)
                                  :owner               (getf snapshot :owner)
                                  :completion-policy   (getf snapshot :completion-policy)
                                  :shell-logs          (getf (shell-log-job-metadata
                                                              configuration medium)
                                                             :shell-logs)
                                  :result              (list :status      (getf result :status)
                                                             :content     (getf result :content)
                                                             :details     (getf result :details)
                                                             :shell-logs  (getf result :shell-logs)
                                                             :duration-ms (getf result :duration-ms))
                                  :cleanup             (getf snapshot :cleanup)
                                  :cancellation-reason (getf snapshot :cancellation-reason)
                                  :condition-report    (getf snapshot :condition-report)
                                  :progress            (list :duration-ms
                                                             (getf (getf snapshot :progress)
                                                                   :duration-ms))))))
               (test-assert (equal (nth-value 1 (job-get-record medium snapshot))
                                   (task--write-readable-sexp expected :pretty-p t))
                            "a result that fits is rendered byte-equal to the unbounded record"))
             ;; Without a retained artifact the marker says the rest was omitted.
             (let ((orphan (finished-job (make-string 20000 :initial-element #\o))))
               (when (probe-file (terminal-path orphan))
                 (delete-file (terminal-path orphan)))
               (multiple-value-bind (record content failure) (job-get-record orphan)
                 (let ((content-field (getf (getf record :result) :content)))
                   (test-assert (and (null failure) content
                                     (<= (length content) *task-tool-content-limit*)
                                     (stringp (getf content-field :preview))
                                     (getf content-field :omitted)
                                     (not (getf content-field :in-artifact))
                                     (not (member :path (getf content-field :omitted))))
                                "a large result without an artifact is omitted without a path"))))
             ;; Failed and aborted results keep their error text and reason.
             (let ((failed (finished-job
                            (concatenate 'string "ERROR-MARK boom "
                                         (make-string 20000 :initial-element #\e))
                            :failure-p t)))
               (multiple-value-bind (record content failure) (job-get-record failed)
                 (declare (ignore content))
                 (let* ((result  (getf record :result))
                        (preview (getf (getf result :content) :preview)))
                   (test-assert (and (null failure)
                                     (eq (getf record :state) :failed)
                                     (eq (getf result :status) :failed)
                                     (stringp preview)
                                     (eql 0 (search "ERROR-MARK boom" preview)))
                                "a large failure keeps its status and leading error text")))
               (let ((aborted (copy-list (session-job-snapshot failed))))
                 (setf (getf aborted :state)               :aborted
                       (getf aborted :cancellation-reason) :user
                       (getf aborted :condition-report)    "Cancelled by the user."
                       (getf aborted :result)
                       (let ((result (copy-list (getf aborted :result))))
                         (setf (getf result :status) :aborted)
                         result))
                 (multiple-value-bind (record content failure) (job-get-record failed aborted)
                   (test-assert (and (null failure) content
                                     (<= (length content) *task-tool-content-limit*)
                                     (eq (getf record :state) :aborted)
                                     (eq (getf (getf record :result) :status) :aborted)
                                     (eq (getf record :cancellation-reason) :user)
                                     (equal (getf record :condition-report)
                                            "Cancelled by the user."))
                                "a large aborted result keeps its state and cancellation reason")))))
        (ignore-errors (task-orchestrator-close orchestrator))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-task-job-list-pagination () null)
(defun test-task-job-list-pagination ()
  "Test content-aware native pagination at the maximum job.list page size."
  (let* ((configuration  (test-configuration))
         (root           (test-configuration-root configuration))
         (orchestrator   (task-tests--orchestrator))
         (requested-name (make-string 64 :initial-element #\n))
         (definition     (list))
         (primary        (task-tests--primary-agent configuration "job-list-primary"))
         (tool
          (make-instance 'task-job-tool :orchestrator orchestrator :namespace "job" :name
                         "list" :description "List test jobs." :parameters
                         (tool-object-schema (json-object) nil)))
         (context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation (agent-conversation primary) :registry
                         (agent-tool-registry primary) :agent primary)))
    (unwind-protect
        (progn
         (dotimes (index *task-job-page-maximum*)
           (declare (ignorable index))
           (task-tests--register-job orchestrator primary definition :name
                                     requested-name))
         (let* ((result
                 (tool-execute tool context
                               (json-object "offset" 0 "limit" *task-job-page-maximum*)))
                (content    (tool-result-content result))
                (form       (task-tests--read-exact-native-value content))
                (properties (rest form)))
           (test-assert
            (and (eq (first form) :job-list)
                 (= (getf properties :count) *task-job-page-maximum*)
                 (= (getf properties :total) *task-job-page-maximum*)
                 (null (getf properties :next-offset))
                 (<= (length content) *task-tool-content-limit*))
            "a maximum-size job.list request returns a bounded native page"))
         (let ((offset 0) (identifiers nil) (page-count 0))
           (loop
            (let* ((result
                    (tool-execute tool context (json-object "offset" offset "limit" 17)))
                   (content     (tool-result-content result))
                   (form        (task-tests--read-exact-native-value content))
                   (properties  (rest form))
                   (jobs        (getf properties :jobs))
                   (next-offset (getf properties :next-offset)))
              (incf page-count)
              (test-assert
               (and (eq (first form) :job-list) (= (getf properties :offset) offset)
                    (= (getf properties :count) (length jobs))
                    (= (getf properties :total) *task-job-page-maximum*)
                    (plusp (length jobs))
                    (<= (length content) *task-tool-content-limit*))
               "each maximum-size job.list request returns one bounded native page")
              (setf identifiers
                      (nconc identifiers (mapcar (lambda (job) (getf job :id)) jobs)))
              (if next-offset
                  (setf offset next-offset)
                  (return))))
           (test-assert
            (and (> page-count 1) (= (length identifiers) *task-job-page-maximum*)
                 (= (length (remove-duplicates identifiers :test #'string=))
                    *task-job-page-maximum*))
            "job.list pagination returns every oversized summary exactly once")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-refresh-after-delayed-close () null)
(defun test-task-refresh-after-delayed-close ()
  "Test reopening after a timed-out close's final worker exits later."
  (let* ((orchestrator (task-tests--orchestrator))
         (barrier-lock (make-lock "Autolith delayed close test"))
         (barrier      (make-condition-variable))
         (started-p    nil)
         (released-p   nil)
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
           (task-orchestrator-refresh orchestrator :reopen-p t)
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
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "lifecycle-primary"))
         (events        nil)
         (event-lock    (make-lock "Autolith task lifecycle event test")))
    (task-orchestrator-add-listener orchestrator
                                    (lambda (channel payload)
                                      (when (eq channel :task-subagent-lifecycle)
                                        (with-lock-held (event-lock)
                                          (push (copy-tree payload) events)))))
    (unwind-protect
        (let* ((parent
                (task-tests--register-job orchestrator primary definition :name
                                          "terminal-parent"))
               (parent-result
                (task-tests--terminal-result parent :status ':success :output
                                             "parent complete"))
               (viewer        (task-tests--child-viewer configuration parent))
               (descendant
                (task-tests--register-job orchestrator viewer definition :name
                                          "live-descendant"))
               (descendant-id (job-identifier descendant))
               (unrelated
                (task-tests--register-job orchestrator primary definition :name
                                          "unrelated")))
          (test-assert (task-tests--publish-terminal parent :completed parent-result)
                       "the parent publishes its first terminal result")
          (multiple-value-bind (parent-accepted-p descendants)
              (task-job-cancel parent :terminate)
            (test-assert
             (and (null parent-accepted-p) (equal descendants (list descendant-id)))
             "cancelling a terminal parent still cancels its live descendants"))
          (test-assert (nth-value 1 (task-job-await descendant 5))
                       "the cancelled descendant finishes terminal publication")
          (test-assert
           (and (eq (job-state parent) :completed) (eq (job-state descendant) :aborted)
                (eq (job-state unrelated) :queued)
                (null (job-cancellation-reason unrelated)))
           "terminal-parent cancellation preserves parent and unrelated states")
          (let ((event-count
                 (with-lock-held (event-lock)
                   (length events)))
                (terminal-count
                 (length
                  (cl-jobpond::job-pool--terminal-identifiers
                   (task-orchestrator-pool orchestrator)))))
            (multiple-value-bind (parent-accepted-p descendants)
                (task-job-cancel parent :terminate)
              (test-assert
               (and (null parent-accepted-p) (null descendants)
                    (=
                     (with-lock-held (event-lock)
                       (length events))
                     event-count)
                    (=
                     (length
                      (cl-jobpond::job-pool--terminal-identifiers
                       (task-orchestrator-pool orchestrator)))
                     terminal-count))
               "duplicate cancellation publishes no second result or event")))
          (dolist (job (list parent descendant))
            (let* ((result   (job-result job))
                   (pathname (getf result :output-path))
                   (artifact
                    (and pathname
                         (task-tests--read-exact-native-value
                          (uiop/stream:read-file-string pathname)))))
              (test-assert
               (and pathname (probe-file pathname) (listp artifact)
                    (eq (getf artifact :status) (getf result :status)))
               "each cancellation result artifact is exactly one readable s-expression")))
          (let* ((race-job
                  (task-tests--register-job orchestrator primary definition :name
                                            "publication-race"))
                 (result
                  (task-tests--terminal-result race-job :status ':success :output
                                               "race winner"))
                 (barrier-lock (make-lock "Autolith terminal publication barrier"))
                 (barrier      (make-condition-variable))
                 (ready        0)
                 (released-p   nil)
                 (claims       nil)
                 (claim-lock   (make-lock "Autolith terminal publication claims")))
            (labels ((publish ()
                       (with-lock-held (barrier-lock)
                         (incf ready)
                         (task--condition-broadcast barrier)
                         (loop until released-p
                               do (condition-wait barrier barrier-lock)))
                       (let ((claimed-p
                              (task-tests--publish-terminal race-job :completed result)))
                         (with-lock-held (claim-lock)
                           (push claimed-p claims)))))
              (let ((first-thread
                     (make-thread #'publish :name "Autolith publication race one"))
                    (second-thread
                     (make-thread #'publish :name "Autolith publication race two")))
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
                    (count race-id events :test #'string= :key
                           (lambda (event) (getf event :id)))))
              (test-assert
               (and (= (count t claims) 1) (= (count nil claims) 1)
                    (= terminal-occurrences 1) (= event-occurrences 1)
                    (probe-file (getf (job-result race-job) :output-path)))
               "concurrent duplicate publication claims one artifact and event"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-retention-and-admission () null)
(defun test-task-retention-and-admission ()
  "Test exact terminal retention and atomic live-job admission limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "capacity-primary")))
    (unwind-protect
        (progn
         (let* ((orchestrator (task-tests--orchestrator))
                (live
                 (task-tests--register-job orchestrator primary definition :name
                                           "live-sentinel"))
                (identifiers  nil))
           (dotimes (index (1+ *task-terminal-retention-limit*))
             (let* ((job
                     (task-tests--register-job orchestrator primary definition :name
                                               (format nil "retained-~2,'0D" index)))
                    (identifier (job-identifier job))
                    (result
                     (task-tests--terminal-result job :status ':success :output
                                                  (format nil "terminal result ~D"
                                                          index))))
               (push identifier identifiers)
               (test-assert (task-tests--publish-terminal job :completed result)
                            "each retained job publishes exactly once")))
           (setf identifiers (nreverse identifiers))
           (let ((first-id (first identifiers))
                 (last-id  (first (last identifiers)))
                 (live-id  (job-identifier live)))
             (test-assert
              (and
               (=
                (length
                 (cl-jobpond::job-pool--terminal-identifiers
                  (task-orchestrator-pool orchestrator)))
                *task-terminal-retention-limit*)
               (=
                (hash-table-count
                 (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                (1+ *task-terminal-retention-limit*))
               (= (task-orchestrator-live-count orchestrator) 1)
               (null
                (gethash first-id
                         (cl-jobpond::job-pool--jobs
                          (task-orchestrator-pool orchestrator))))
               (gethash last-id
                        (cl-jobpond::job-pool--jobs
                         (task-orchestrator-pool orchestrator)))
               (eq
                (gethash live-id
                         (cl-jobpond::job-pool--jobs
                          (task-orchestrator-pool orchestrator)))
                live))
              "retention keeps exactly 64 newest terminals without evicting live jobs")
             (test-assert
              (every
               (lambda (identifier)
                 (let* ((job
                         (gethash identifier
                                  (cl-jobpond::job-pool--jobs
                                   (task-orchestrator-pool orchestrator))))
                        (pathname (and job (getf (job-result job) :output-path))))
                   (and job (job-terminal-p job) pathname
                        (listp
                         (task-tests--read-exact-native-value
                          (uiop/stream:read-file-string pathname))))))
               (cl-jobpond::job-pool--terminal-identifiers
                (task-orchestrator-pool orchestrator)))
              "every retained identifier maps to one terminal job and readable artifact")))
         (let* ((orchestrator (task-tests--orchestrator))
                (entries
                 (lambda (offset count)
                   (loop for index from offset below (+ offset count)
                         collect (list :item
                                       (append
                                        (list :name (format nil "live-~2,'0D" index)
                                              :task "Remain queued."
                                              :context nil :async t)
                                        definition)
                                       :detached t)))))
           (dotimes (index *task-maximum-live-jobs*)
             (task-tests--register-job orchestrator primary definition :name
                                       (format nil "live-~2,'0D" index)))
           (let ((job-count
                  (hash-table-count
                   (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))))
                 (queue-count
                  (length
                   (cl-jobpond::job-pool--queue (task-orchestrator-pool orchestrator))))
                 (live-count (task-orchestrator-live-count orchestrator)))
             (test-assert (= live-count *task-maximum-live-jobs*)
                          "the scheduler admits exactly 64 simultaneous live jobs")
             (test-assert
              (handler-case
               (progn
                (task-orchestrator-start-jobs orchestrator primary
                                              (funcall entries 64 1))
                nil)
               (task-error nil t))
              "admission beyond 64 live jobs fails")
             (test-assert
              (and
               (=
                (hash-table-count
                 (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator)))
                job-count)
               (=
                (length
                 (cl-jobpond::job-pool--queue (task-orchestrator-pool orchestrator)))
                queue-count)
               (= (task-orchestrator-live-count orchestrator) live-count))
              "failed live admission consumes no identity or scheduler state"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-evicted-identity-retention () null)
(defun test-task-evicted-identity-retention ()
  "Test that evicted generated identities remain unique across live ancestry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "identity-primary")))
    (unwind-protect
        (let* ((parent
                (task-tests--register-job orchestrator primary definition :name nil))
               (parent-id (job-identifier parent))
               (viewer    (task-tests--child-viewer configuration parent))
               (descendant
                (task-tests--register-job orchestrator viewer definition :name
                                          "live-descendant"))
               (parent-result
                (task-tests--terminal-result parent :status ':success :output
                                             "parent terminal")))
          (task-tests--publish-terminal parent :completed parent-result)
          (dotimes (index *task-terminal-retention-limit*)
            (let* ((job
                    (task-tests--register-job orchestrator primary definition :name
                                              (format nil "identity-filler-~D" index)))
                   (result
                    (task-tests--terminal-result job :status ':success :output
                                                 "filler terminal")))
              (task-tests--publish-terminal job :completed result)))
          (let* ((replacement
                  (task-tests--register-job orchestrator primary definition :name nil))
                 (replacement-id (job-identifier replacement)))
            (test-assert
             (and
              (null
               (gethash parent-id
                        (cl-jobpond::job-pool--jobs
                         (task-orchestrator-pool orchestrator))))
              (member parent-id (job-owner-identifiers descendant) :test #'string=)
              (not (string= replacement-id parent-id))
              (> (job-index replacement) (job-index parent))
              (= (task-orchestrator-live-count orchestrator) 2))
             "eviction never permits a generated ancestor identity to be reused")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-live-activity-snapshots () null)
(defun test-task-live-activity-snapshots ()
  "Test stable, bounded progress projection for queued and running tasks."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition    (list))
         (primary       (task-tests--primary-agent configuration "activity-primary"))
         (queued        nil)
         (running       nil))
    (unwind-protect
        (progn
         (setf queued
                 (task-tests--register-job orchestrator primary definition :name
                                           "queued-child")
               running
                 (task-tests--register-job orchestrator primary definition :name
                                           "running-child"))
         (with-lock-held ((cl-jobpond::job--lock running))
           (setf (job-state running) ':running)
           (task-job--set-progress-state running ':running))
         (task-progress-note-status running ':provider-request-started
                                    (list :request-number 2))
         (loop for index from 1 to 10
               for tool = (format nil "tool.~D" index)
               do (task-progress-note-status running ':tool-call-started
                                             (list :tool
                                                   tool)) (task-progress-note-status
                                                           running ':tool-call-completed
                                                           (list :tool tool)))
         (task-progress-note-status running ':tool-call-started (list :tool "lisp.eval"))
         (let* ((activities        (task-orchestrator-live-activities orchestrator))
                (running-activity  (second activities))
                (progress-snapshot (task-progress-snapshot running)))
           (test-assert
            (and (= (length activities) 2)
                 (equal (mapcar (lambda (activity) (getf activity :id)) activities)
                        (mapcar (lambda (job) (job-identifier job))
                                (list queued running)))
                 (eq (getf (first activities) :state) ':queued)
                 (eq (getf running-activity :state) ':running)
                 (string= (getf running-activity :current-tool) "lisp.eval")
                 (equal (getf running-activity :recent-tools)
                        (loop for index from 3 to 10
                              collect (format nil "tool.~D" index)))
                 (= (getf running-activity :request-count) 2)
                 (typep (getf running-activity :duration-ms) '(integer 0))
                 (typep (getf running-activity :current-tool-duration-ms) '(integer 0))
                 (<= (getf running-activity :current-tool-duration-ms)
                     (getf running-activity :duration-ms))
                 (string= (getf running-activity :agent) "running-child")
                 (getf running-activity :detached)
                 (equal (getf progress-snapshot :recent-tools)
                        (getf running-activity :recent-tools))
                 (= (getf progress-snapshot :request-count) 2)
                 (typep (getf progress-snapshot :current-tool-duration-ms) '(integer 0)))
            "live activity snapshots retain bounded chronological progress"))
         (task-progress-note-status running ':tool-call-completed
                                    (list :tool "lisp.eval"))
         (let* ((activities        (task-orchestrator-live-activities orchestrator))
                (running-activity  (second activities))
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
         (task-tests--publish-terminal queued ':completed
                                       (task-tests--terminal-result queued :status
                                                                    ':success :output
                                                                    "queued complete"))
         (test-assert
          (equal
           (mapcar (lambda (activity) (getf activity :id))
                   (task-orchestrator-live-activities orchestrator))
           (list (job-identifier running)))
          "terminal publication immediately removes a child snapshot")
         (task-tests--publish-terminal running ':completed
                                       (task-tests--terminal-result running :status
                                                                    ':success :output
                                                                    "running complete"))
         (test-assert (null (task-orchestrator-live-activities orchestrator))
                      "no task presentation remains after every child is terminal")
         (test-assert
          (= (getf (task-progress-snapshot running) :duration-ms)
             (task--milliseconds-between
              (task-progress-started-at (task-job-progress running))
              (job-ended-at running)))
          "Terminal inspection measures duration through the recorded end time"))
      (dolist (job (remove nil (list queued running)))
        (unless (job-terminal-p job)
          (task-tests--publish-terminal job ':aborted
                                        (task-tests--terminal-result job :status
                                                                     ':aborted :output
                                                                     "test cleanup"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-cumulative-child-usage () null)
(defun test-task-cumulative-child-usage ()
  "Test cumulative usage across direct and nested child result envelopes."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-tests--orchestrator))
         (definition
          (list :models
                (list (find-if (lambda (model)
                                 (not (string= model (config :model configuration))))
                               *supported-models*))))
         (primary       (task-tests--primary-agent configuration "usage-primary"))
         (direct        nil)
         (nested        nil))
    (labels ((usage (&key input output cached created)
               (list (list "input_tokens" input) (list "output_tokens" output)
                     (list "total_tokens" (+ input output))
                     (list "cached_input_tokens" cached)
                     (list "cache_creation_input_tokens" created)))

             (note-request (job number request-usage)
               (task-progress-note-status job ':provider-request-started
                                          (list :request-number number))
               (task-progress-note-status job ':provider-request-completed
                                          (list :request-number number :usage
                                                request-usage))))
      (unwind-protect
          (progn
           (setf direct
                   (task-tests--register-job orchestrator primary definition :name
                                             "direct-usage"))
           (setf (conversation-last-total-tokens (agent-conversation primary)) 91)
           (let ((first-usage (usage :input 3 :output 2 :cached 1 :created 0)))
             (note-request direct 1 first-usage)
             (test-assert
              (equal (getf (task-progress-snapshot direct) :usage) first-usage)
              "one child provider request preserves its normalized usage"))
           (note-request direct 2 (usage :input 7 :output 4 :cached 2 :created 1))
           (let* ((expected (usage :input 10 :output 6 :cached 3 :created 1))
                  (failed   (task--failed-result direct ':failed "provider failed")))
             (let ((model
                    (config :model (task-child-configuration configuration definition))))
               (test-assert
                (and (string/= model (config :model configuration))
                     (string= model (getf failed :model))
                     (string= (getf failed :id) (job-identifier direct))
                     (string= (getf failed :assignment)
                              (getf (task-job-item direct) :task))
                     (not (minusp (getf failed :duration-ms)))
                     (= 2 (getf failed :request-count)))
                "failed results retain the attempted child's model, identity and accounting")
               (dolist (state '(:failed :aborted :cancelled))
                 (let ((result (task--failed-result direct state "Stopped")))
                   (test-assert
                    (and (eq state (getf result :status))
                         (string= model (getf result :model))
                         (equal expected (getf result :usage)))
                    "failure and cancellation share execution metadata and accounting")))
               (let* ((empty
                       (task-tests--register-job orchestrator primary definition :name
                                                 "no-result"))
                      (identifier (job-identifier empty))
                      (assignment (getf (task-job-item empty) :task)))
                 (task-tests--publish-terminal empty ':failed nil :report "No result")
                 (let ((result (getf (task-job-snapshot empty) :result)))
                   (test-assert
                    (and (string= model (getf result :model))
                         (string= identifier (getf result :id))
                         (string= assignment (getf result :assignment))
                         (eq ':failed (getf result :status)))
                    "terminal publication attributes an absent result to the attempted child"))))
             (test-assert
              (and (equal (getf (task-progress-snapshot direct) :usage) expected)
                   (equal (getf failed :usage) expected)
                   (= (conversation-last-total-tokens (agent-conversation primary)) 91))
              "direct child usage accumulates without changing parent usage")
             (let* ((viewer              (task-tests--child-viewer configuration direct))
                    (viewer-conversation (agent-conversation viewer)))
               (setf (conversation-last-total-tokens viewer-conversation) 37
                     nested
                       (task-tests--register-job orchestrator viewer definition :name
                                                 "nested-usage"))
               (note-request nested 1 (usage :input 2 :output 1 :cached 0 :created 1))
               (note-request nested 2 (usage :input 5 :output 3 :cached 2 :created 0))
               (let* ((nested-usage (usage :input 7 :output 4 :cached 2 :created 1))
                      (completed
                       (task-tests--terminal-result nested :status ':success :output
                                                    "nested complete")))
                 (setf (getf completed :request-count) 2
                       (getf completed :usage) nested-usage)
                 (task-tests--publish-terminal nested ':completed completed)
                 (let* ((snapshot (task-job-snapshot nested))
                        (native
                         (session-job-native-record nested snapshot primary
                                                    :preview-limit
                                                    *task-result-preview-limit*
                                                    :include-progress-p t)))
                   (test-assert
                    (and (getf native :detached)
                         (equal (getf (getf native :result) :usage) nested-usage)
                         (equal (getf (getf native :progress) :usage) nested-usage)
                         (= (conversation-last-total-tokens viewer-conversation) 37))
                    "nested detached results expose cumulative usage beside their parent")))
               (task-tests--publish-terminal direct ':failed failed)
               (let* ((snapshot (task-job-snapshot direct))
                      (native
                       (session-job-native-record direct snapshot primary :preview-limit
                                                  *task-result-preview-limit*
                                                  :include-progress-p t)))
                 (test-assert
                  (and (equal (getf (getf native :result) :usage) expected)
                       (equal (run-job--usage (getf snapshot :result))
                              '(:input-tokens 10 :output-tokens 6 :provider-requests 2)))
                  "failed child and durable job projections retain cumulative usage"))))
           (dolist (job (remove nil (list nested direct)))
             (unless (job-terminal-p job)
               (task-tests--publish-terminal job ':aborted
                                             (task-tests--terminal-result job :status
                                                                          ':aborted
                                                                          :output
                                                                          "test cleanup"))))
           (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                           ':ignore))))
    nil))

(-> task-tests--run-scheduler-case
    (task-test-provider json-object
     &key (:parent-reference-p boolean)
          (:oversized-parent-reference-p boolean))
    list)
(defun task-tests--run-scheduler-case
    (provider arguments &key parent-reference-p oversized-parent-reference-p)
  "Execute one real task tool case and return observations after clean shutdown."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (task-augment-tool-registry
                         (make-default-tool-registry)))
         (conversation  (conversation-create configuration))
         (parent
          (agent-create :configuration configuration
                        :provider      provider
                        :conversation  conversation
                        :tool-registry registry
                        :worker        nil))
         (tool          (tool-registry-find registry "child" "spawn"))
         (orchestrator  (task-run-tool-orchestrator tool))
         (context
          (make-instance 'tool-context
                         :configuration configuration
                         :worker        nil
                         :conversation  conversation
                         :registry      registry
                         :agent         parent
                         :call-id       "task-scheduler-test"))
         (started-at    (get-internal-real-time)))
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
           (let* ((result        (tool-execute tool context arguments))
                  (admitted-jobs (task-orchestrator-list-jobs orchestrator))
                  (idle-p
                    (progn
                      (dolist (job admitted-jobs)
                        (unless (nth-value 1 (task-job-await job 30))
                          (error "Scheduler fixture did not observe terminal publication for ~A."
                                 (job-identifier job))))
                      ;; A terminal result may precede the worker releasing its slot.
                      (task-tests--wait-until
                       (lambda ()
                         (and (zerop (task-orchestrator-live-count orchestrator))
                              (zerop (task-orchestrator-active-count orchestrator))))
                       30)))
                  (jobs          (task-orchestrator-list-jobs orchestrator))
                  (details       (tool-result-details result))
                  (content       (tool-result-content result))
                  (native-content
                    (task-tests--read-exact-native-value content))
                  (artifact-forms
                    (mapcar
                     (lambda (job)
                       (let ((path (getf (job-result job) :output-path)))
                         (unless path
                           (error "Terminal fixture job ~A in state ~A did not publish an artifact."
                                  (job-identifier job) (job-state job)))
                         (task-tests--read-exact-native-value (uiop:read-file-string path))))
                     jobs))
                  (workers
                    (copy-list
                       (cl-jobpond::job-pool--worker-threads (task-orchestrator-pool orchestrator)))))
             (list :success-p      (and (tool-result-success-p result)
                                        (every (lambda (artifact)
                                                 (eq (getf artifact :status) ':success))
                                               artifact-forms))
                 :content          content
                 :native-content   native-content
                 :details          details
                 :artifact-forms   artifact-forms
                 :job-count        (length jobs)
                 :all-terminal-p   (every #'job-terminal-p jobs)
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
                 :worker-count     (length workers)
                 :workers-alive-p  (every #'thread-alive-p workers)
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
                 :duration-ms
                 (task--milliseconds-between started-at
                                             (get-internal-real-time)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist   ':ignore))))

(-> test-task-closed-runtime-refresh () null)
(defun test-task-closed-runtime-refresh ()
  "Test ordinary refresh cannot reopen a deliberately closed task runtime."
  (let ((orchestrator (task-orchestrator-create)))
    (unwind-protect
         (progn
           (test-assert
            (task-orchestrator-close orchestrator)
            "the lazy task runtime closes completely")
           (test-assert
            (handler-case
                (progn
                  (task-orchestrator-refresh orchestrator)
                  nil)
              (task-error ()
                t))
            "ordinary refresh rejects a closed task runtime")
           (test-assert
            (and (task-orchestrator-closed-p orchestrator)
                 (eq (task-orchestrator-lifecycle-state orchestrator) ':closed))
            "rejected refresh leaves the runtime closed")
           (task-orchestrator-refresh orchestrator :reopen-p t)
           (test-assert
            (and (not (task-orchestrator-closed-p orchestrator))
                 (eq (task-orchestrator-lifecycle-state orchestrator) ':open))
            "explicit runtime resume reopens both task pools"))
      (task-orchestrator-close orchestrator)))
  nil)

(-> test-task-scheduler () null)
(defun test-task-scheduler ()
  "Test bounded reusable workers, private artifacts, and nested help-join."
  (let ((previous-concurrency (uiop/os:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime     (uiop/os:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
        (progn
         (platform-setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "2")
         (platform-setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "10000")
         (let* ((provider (make-instance 'task-test-provider :mode ':concurrent))
                (observation
                 (task-tests--run-scheduler-case provider
                                                 (json-object "task"
                                                              "Use the inherited design context.")
                                                 :parent-reference-p t))
                (inputs   (first (getf observation :provider-request-inputs))))
           (test-assert (getf observation :success-p)
                        "a child with inherited reference context succeeds")
           (test-assert
            (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                   '("user" "assistant" "developer" "user"))
            "the real task child receives parent turns, a boundary, and its task")
           (test-assert
            (search "inherited reference context"
                    (json-get (aref (json-get (third inputs) "content") 0) "text"))
            "the inherited parent turns end at an explicit developer boundary"))
         (let* ((provider (make-instance 'task-test-provider :mode ':concurrent))
                (observation
                 (task-tests--run-scheduler-case provider
                                                 (json-object "task"
                                                              "Use bounded inherited context.")
                                                 :parent-reference-p           t
                                                 :oversized-parent-reference-p t))
                (inputs   (first (getf observation :provider-request-inputs))))
           (test-assert
            (and (getf observation :success-p) (= (getf observation :job-count) 1))
            "the scheduler accepts one detached child with oversized parent history")
           (test-assert (getf observation :scheduler-idle-p)
                        (format nil "Detached scheduler drained (active ~D, live ~D)."
                                (getf observation :active-count)
                                (getf observation :live-count)))
           (let* ((artifact (first (getf observation :artifact-forms)))
                  (status   (getf artifact :status)))
             (test-assert (eq status ':success)
                          (format nil "Detached child artifact status ~S: ~A" status
                                  (getf artifact :error))))
           (test-assert
            (equal (mapcar (lambda (item) (json-get item "role")) inputs)
                   '("assistant" "developer" "user"))
            "oversized parent history keeps only the newest fitting message")
           (test-assert
            (<=
             (conversation--inherited-reference-wire-byte-length (list (first inputs)))
             *task-inherited-reference-maximum-bytes*)
            "detached child inheritance stays within the fixed wire budget"))
         (dolist (capabilities '((nil t "parent") (t nil "child")))
           (destructuring-bind
               (parent-reference-p child-reference-p label)
               capabilities
             (let* ((provider
                     (make-instance 'task-test-provider :mode  ':concurrent
                                    :reference-history-p       parent-reference-p
                                    :child-reference-history-p child-reference-p))
                    (observation
                     (task-tests--run-scheduler-case provider
                                                     (json-object "task"
                                                                  "Do not inherit parent history.")
                                                     :parent-reference-p t))
                    (inputs (first (getf observation :provider-request-inputs))))
               (test-assert (getf observation :success-p)
                            "a provider-isolation task succeeds")
               (test-assert
                (equal (mapcar (lambda (item) (json-get item "role")) inputs) '("user"))
                (format nil "unsupported ~A provider retains no inherited history"
                        label)))))
         (let* ((*task-inherited-reference-context-divisor* 1000000)
                (provider                                   (make-instance 'task-test-provider :mode ':concurrent))
                (observation
                 (task-tests--run-scheduler-case provider
                                                 (json-object "task"
                                                              "Use a tiny child context budget.")
                                                 :parent-reference-p t))
                (inputs                                     (first (getf observation :provider-request-inputs))))
           (test-assert (getf observation :success-p)
                        "a child with a tiny reference budget succeeds")
           (test-assert
            (equal (mapcar (lambda (item) (json-get item "role")) inputs) '("user"))
            "inheritance is disabled when its boundary exceeds the budget"))
         (platform-setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1")
         (let* ((provider (make-instance 'task-test-provider :mode ':nested))
                (observation
                 (task-tests--run-scheduler-case provider
                                                 (json-object "task"
                                                              "Delegate once, then await its job."))))
           (test-assert (getf observation :success-p)
                        (format nil "a nested task succeeds at concurrency one: ~S"
                                (getf observation :details)))
           (test-assert (= (getf observation :job-count) 2)
                        "nested execution retains parent and leaf jobs")
           (test-assert (= (getf observation :provider-request-count) 4)
                        "nested waiting resumes the parent after the leaf")
           (test-assert
            (find-if
             (lambda (items)
               (equal (mapcar (lambda (item) (json-get item "role")) items)
                      '("user" "developer" "user")))
             (getf observation :provider-request-inputs))
            "a nested child inherits its immediate parent's assignment")
           (test-assert
            (and (getf observation :scheduler-idle-p) (getf observation :all-terminal-p)
                 (zerop (getf observation :active-count))
                 (zerop (getf observation :live-count)))
            "nested help-join drains the concurrency-one scheduler"))
         (platform-setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "999")
         (let ((orchestrator (task-tests--orchestrator)))
           (test-assert
            (= (task-orchestrator-maximum-concurrency orchestrator)
               *task-maximum-concurrency*)
            "environment concurrency cannot exceed the hard pool cap")))
      (if previous-concurrency
          (platform-setenv "AUTOLITH_TASK_MAX_CONCURRENCY" previous-concurrency)
          (platform-unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (platform-setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" previous-runtime)
          (platform-unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-cancellation-cleanup-ownership () null)
(defun test-task-cancellation-cleanup-ownership ()
  "Notify execution descendants before queued publication and retain slow cleanup."
  (with-test-configuration (configuration)
    (let* ((orchestrator             (task-tests--orchestrator))
           (primary                  (task-tests--primary-agent configuration "cleanup-primary"))
           (definition               (list))
           (parent
            (task-tests--register-job orchestrator primary definition :name
                                      "cleanup-parent"))
           (viewer                   (task-tests--child-viewer configuration parent))
           (queued
            (task-tests--register-job orchestrator viewer definition :name
                                      "cleanup-queued"))
           (lock                     (make-lock "native cleanup test"))
           (release-p                nil)
           (release-readers-p        nil)
           (native-owner
            (make-thread
             (lambda ()
               (loop until (with-lock-held (lock)
                             release-p)
                     do (sleep 0.01))
               t)
             :name "test native cleanup owner"))
           (reader
            (make-thread
             (lambda ()
               (loop until (with-lock-held (lock)
                             release-readers-p)
                     do (sleep 0.01))
               t)
             :name "test capture cleanup owner"))
           (execution                nil)
           (publication-observations nil)
           (publication-condition    (make-condition-variable))
           (release-publication-p    nil)
           (retry-count              0)
           (terminal-function        (symbol-function 'task-job--terminal-record)))
      (unwind-protect
          (progn
           (setf execution
                   (task-orchestrator-start-execution-job orchestrator viewer :tool-name
                                                          "shell.run" :summary
                                                          "cleanup test"
                                                          :operation-function
                                                          (lambda ()
                                                            (tool-execution-job-await-native-cleanup
                                                             *tool-execution-current-job*
                                                             (make-condition
                                                              'cl-exec-sandbox:sandbox-process-cleanup-error
                                                              :message
                                                              "Controlled cleanup deadline."
                                                              :command '("test") :process
                                                              nil :alive-p t
                                                              :termination-timeout 0.01
                                                              :cleanup-thread      nil
                                                              :cleanup-readers
                                                              (list reader)
                                                              :cleanup-retry
                                                              (lambda ()
                                                                (incf retry-count)
                                                                (if (= retry-count 1)
                                                                    (error
                                                                     "Controlled thread creation failure.")
                                                                    native-owner))))
                                                            (tool-failure
                                                             "cleanup completed"))))
           (test-assert
            (task-tests--wait-until
             (lambda () (getf (session-job-snapshot execution) :cleanup)) 2)
            "native cleanup transfers to the visible execution")
           (test-call-with-function-replacements
            (list
             (list 'task-job--terminal-record
                   (lambda (job &rest arguments)
                     (with-lock-held (lock)
                       (push (job-cancellation-requested-p execution)
                             publication-observations)
                       (task--condition-broadcast publication-condition)
                       (loop until release-publication-p
                             do (condition-wait publication-condition lock)))
                     (apply terminal-function job arguments))))
            (lambda ()
              (unwind-protect
                  (progn
                   (task-job-cancel parent ':test-cancel)
                   (with-lock-held (lock)
                     (loop until publication-observations
                           unless (condition-wait publication-condition lock :timeout 2)
                           do (error "Timed out waiting for queued publication."))
                     (test-assert
                      (and (not (job-terminal-p parent)) (not (job-terminal-p queued))
                           (job-cancellation-requested-p parent)
                           (job-cancellation-requested-p queued)
                           (every #'identity publication-observations))
                      "cancellation returns with both pools marked while publication is blocked")))
                (with-lock-held (lock)
                  (setf release-publication-p t)
                  (task--condition-broadcast publication-condition)))
              (test-assert
               (and (nth-value 1 (job-await parent :timeout-seconds 2))
                    (nth-value 1 (job-await queued :timeout-seconds 2))
                    (= (length publication-observations) 2)
                    (every #'identity publication-observations))
               "both pools receive cancellation before every queued task publication")))
           (let* ((*task-descendant-cleanup-timeout-seconds* 0.03)
                  (started                                   (get-internal-real-time)))
             (task-job-cancel-execution-descendants parent ':test-cancel)
             (test-assert
              (< (/ (- (get-internal-real-time) started) internal-time-units-per-second)
                 1)
              "one descendant cleanup budget bounds the parent wait"))
           (test-assert
            (and (eq (job-state execution) ':running)
                 (equal
                  (getf
                   (session-job-native-record parent (session-job-snapshot parent)
                                              primary)
                   :cleanup-descendants)
                  (list (session-job-identifier execution)))
                 (equal
                  (getf
                   (getf
                    (session-job-native-record execution (session-job-snapshot execution)
                                               primary)
                    :cleanup)
                   :owner)
                  (session-job-identifier execution)))
            "the terminal parent exposes its live execution cleanup owner")
           (with-lock-held (lock)
             (setf release-p t))
           (join-thread native-owner)
           (test-assert (not (nth-value 1 (job-await execution :timeout-seconds 0.03)))
                        "finishing process reaping does not abandon unfinished capture readers")
           (with-lock-held (lock)
             (setf release-readers-p t))
           (multiple-value-bind (snapshot terminal-p)
               (job-await execution :timeout-seconds 2)
             (test-assert
              (and terminal-p (eq (getf snapshot :state) ':aborted)
                   (null (getf (session-job-snapshot execution) :cleanup))
                   (>= retry-count 2))
              "cleanup retries keep ownership until process and readers finish")))
        (with-lock-held (lock)
          (setf release-p t
                release-readers-p t))
        (join-thread native-owner)
        (join-thread reader)
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-task-child-identity () null)
(defun test-task-child-identity ()
  "Test short child identifiers avoid existing histories and artifact claims."
  (with-test-configuration (configuration)
    (let* ((timestamp               3994000000)
           (generate                #'identifier-generate)
           (root-identifier         "identity-fixture")
           (root                    (task--artifact-group-root configuration root-identifier))
           (*random-index-function* (lambda (limit) (declare (ignore limit)) 0)))
      (test-call-with-function-replacements
       (list
        (list 'identifier-generate
              (lambda (&rest arguments)
                (apply generate :timestamp timestamp arguments))))
       (lambda ()
         (let* ((conversation-id         (identifier-from-seed timestamp 0))
                (stored-id               (identifier-from-seed timestamp 1))
                (claim-id                (identifier-from-seed timestamp 2))
                (partial-id              (identifier-from-seed timestamp 3))
                (partial-conversation-id (identifier-from-seed timestamp 4))
                (stored-path
                 (merge-pathnames (format nil "~A/continuity.sexp" stored-id) root)))
           (conversation-append-user-message
            (conversation-create configuration :identifier conversation-id)
            "Stored identity.")
           (task-continuity--write stored-path
                                   (list :version 1 :kind ':tool :job-id
                                         "stored-execution" :execution-id stored-id
                                         :root-conversation root-identifier :owners nil))
           (test-assert
            (task-execution--claim-directory
             (merge-pathnames (format nil "~A/" claim-id) root))
            "a reserved but unpublished execution claims its durable directory")
           (platform-ensure-directories-exist *platform*
                                              (merge-pathnames
                                               (format nil "~A/unpublished" partial-id)
                                               root))
           (platform-ensure-directories-exist *platform*
                                              (merge-pathnames "unpublished"
                                                               (conversation-storage-directory-pathname
                                                                (conversation-pathname-for-id
                                                                 configuration
                                                                 partial-conversation-id))))
           (let ((stored (uiop/stream:read-file-string stored-path)))
             (identifier-clear-reservations)
             (let ((identifier
                    (task-execution-identifier-generate configuration root-identifier)))
               (test-assert
                (and (identifier-p identifier)
                     (string= identifier (identifier-from-seed timestamp 5)))
                "allocation skips stored and partial conversations, executions, claims and directories"))
             (identifier-clear-reservations)
             (test-assert
              (string= (task-execution-identifier-generate configuration root-identifier)
                       (identifier-from-seed timestamp 6))
              "restart allocation skips an execution reserved before the restart")
             (test-assert (string= stored (uiop/stream:read-file-string stored-path))
                          "new admissions never overwrite prior durable execution identity"))
           (dolist (reason '(:refused :failed))
             (let* ((directory
                     (merge-pathnames (format nil "failure-~(~A~)/" reason) root))
                    (claim (merge-pathnames "identity.reservation" directory))
                    (open  #'platform-open-new-file)
                    (failure
                     (make-condition 'platform-error :operation ':open :pathname claim
                                     :reason reason :message "Injected claim failure.")))
               (test-call-with-function-replacements
                (list
                 (list 'platform-open-new-file
                       (lambda (platform path)
                         (when (eq reason ':failed)
                           (with-open-stream (stream (funcall open platform path))))
                         (error failure))))
                (lambda ()
                  (test-assert
                   (handler-case (progn (task-execution--claim-directory directory) nil)
                                 (platform-error (condition) (eq failure condition)))
                   "genuine filesystem failure propagates even when a claim marker exists")))))
           (let ((lock        (make-lock "Concurrent identity admission"))
                 (identifiers nil)
                 (failures    nil))
             (let ((threads
                    (loop repeat 2
                          collect (make-thread
                                   (lambda ()
                                     (handler-case
                                      (let* ((*random-index-function*
                                              (lambda (limit)
                                                (declare (ignore limit))
                                                0))
                                             (identifier
                                              (task-execution-identifier-generate
                                               configuration root-identifier)))
                                        (with-lock-held (lock)
                                          (push identifier identifiers)))
                                      (error (condition)
                                             (with-lock-held (lock)
                                               (push condition failures)))))))))
               (mapc #'join-thread threads))
             (test-assert
              (and (null failures) (= 2 (length identifiers))
                   (not (equal (first identifiers) (second identifiers))))
              "concurrent admissions with identical candidates reserve distinct storage"))
           (let ((directory  (merge-pathnames "exclusive-claim/" root))
                 (ensure     #'platform-ensure-directories-exist)
                 (lock       (make-lock "Exclusive execution claim"))
                 (condition  (make-condition-variable))
                 (arrivals   0)
                 (released-p nil)
                 (results    nil)
                 (failures   nil))
             (test-call-with-function-replacements
              (list
               (list 'platform-ensure-directories-exist
                     (lambda (platform path)
                       (with-lock-held (lock)
                         (incf arrivals)
                         (loop until released-p
                               do (condition-wait condition lock)))
                       (funcall ensure platform path))))
              (lambda ()
                (let ((threads
                       (loop repeat 2
                             collect (make-thread
                                      (lambda ()
                                        (handler-case
                                         (let ((result
                                                (task-execution--claim-directory
                                                 directory)))
                                           (with-lock-held (lock)
                                             (push result results)))
                                         (error (failure)
                                                (with-lock-held (lock)
                                                  (push failure failures)))))))))
                  (unwind-protect
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held (lock)
                            (= arrivals 2)))
                        2)
                       "both claimants reach the exclusive creation boundary")
                    (with-lock-held (lock)
                      (setf released-p t)
                      (dotimes (index 2) (condition-notify condition)))
                    (mapc #'join-thread threads)))))
             (test-assert
              (and (null failures) (= 1 (count t results)) (= 1 (count nil results)))
              "exclusive durable creation admits exactly one cross-process-style claimant")))))))
  nil)

(-> test-task-children-limit () null)
(defun test-task-children-limit ()
  "Test child.spawn refuses a child beyond the most an agent may have, and says what to do instead."
  (let ((*task-children-maximum* 0))
    (test-assert
     (handler-case
      (progn
       (task-tests--run-scheduler-case
        (make-instance 'task-test-provider :mode ':concurrent)
        (json-object "task" "Do not fit."))
       nil)
      (task-error (condition) (search "child.remove" (princ-to-string condition))))
     "a spawn past the limit is refused at admission, pointing at reuse and removal"))
  nil)

(-> test-task-child-channel-lease () null)
(defun test-task-child-channel-lease ()
  "Test a running child leases its conversation, so nothing opens its channel twice, and frees it when done."
  (let ((run      (symbol-function 'agent-run-user-turn))
        (release  (symbol-function 'conversation-lease-release))
        (running  nil)
        (released nil))
    (test-call-with-function-replacements
     (list
      (list 'agent-run-user-turn
            (lambda (agent &rest arguments)
              (when (typep agent 'task-child-agent)
                (let ((identifier (conversation-identifier (agent-conversation agent))))
                  (push
                   (cons identifier
                         (conversation-lease-busy-p (agent-configuration agent)
                                                    identifier))
                   running)))
              (apply run agent arguments)))
      (list 'conversation-lease-release
            (lambda (lease)
              (push (conversation-lease-identifier lease) released)
              (funcall release lease))))
     (lambda ()
       (let ((observation
              (task-tests--run-scheduler-case
               (make-instance 'task-test-provider :mode ':concurrent)
               (json-object "task" "Hold the channel."))))
         (test-assert (getf observation :success-p) "the leased child run succeeds")
         (test-assert (and running (every #'rest running))
                      "the child's conversation is leased while it runs")
         (test-assert (member (first (first running)) released :test #'string=)
                      "the child's lease is released when its run ends")))))
  nil)
