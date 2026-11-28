(in-package #:autolith)

;;;; -- Task Child Execution Tests --

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
  "Test task tool registration and inherited configuration."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((registry (make-default-tool-registry)))
             (task-augment-tool-registry registry)
             (dolist (name '("spawn"))
               (test-assert (tool-registry-find registry "child" name)
                            (format nil "task augmentation registers child.~A" name)))
             (dolist (name '("list" "get" "cancel"))
              (test-assert (tool-registry-find registry "job" name)
                           (format nil "task augmentation registers job.~A" name))))
           (let* ((immutable
                    (configuration-copy configuration :immutable-p t :reasoning-effort
                                        "xhigh" :codex-fast-mode-p t))
                  (child-configuration (task-child-configuration immutable nil))
                  (explicit-definition (list :reasoning-effort "high"))
                  (explicit-configuration
                    (task-child-configuration immutable explicit-definition)))
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-child-spawn-channel-delivery () null)
(defun test-child-spawn-channel-delivery ()
  "Deliver the first and subsequent child messages through the same durable inbox."
  (with-test-configuration (initial root)
    (declare (ignore root))
    (let* ((configuration (configuration-copy initial :reasoning-effort "xhigh" :codex-fast-mode-p t))
           (registry      (task-augment-tool-registry (make-default-tool-registry)))
           (parent        (task-tests--primary-agent configuration "spawnparent" registry))
           (origin        (agent-conversation parent))
           (context       (make-instance 'tool-context :configuration configuration :worker nil
                                        :conversation origin :registry registry :agent parent)))
      (unwind-protect
           (progn
             (conversation-append-user-message origin "Parent turn")
             (permissions-save-channel-mode configuration (conversation-pathname origin)
                                            (conversation-created-at origin) :mode ':sandboxed)
             (let* ((result  (tool-execute (tool-registry-find registry "child" "spawn") context
                                           (json-object "name" "helper" "task" "First turn")))
                    (details (tool-result-details result))
                    (child   (getf (rest details) :channel))
                    (world   (world-load configuration))
                    (sender  (world-participant-find world "spawnparent"))
                    (channel (world-channel-find world child)))
               (test-assert (and (tool-result-success-p result) (eq (first details) ':child-spawn)
                                 (eq (getf (rest details) :delivery) ':delivered)
                                 (equal (channel-parent channel) "spawnparent")
                                 (member child (participant-subscriptions sender) :test #'equal))
                            "spawn registers the child and parent subscription")
               (let ((loaded (conversation-load-by-id configuration child)))
                 (test-assert (eq (permissions-channel-mode configuration (conversation-pathname loaded)
                                                           (conversation-created-at loaded)) :sandboxed)
                              "the ordinary child channel has the parent's saved permission mode")
                 (test-assert (equal *task-default-reasoning-effort* (conversation-reasoning-effort loaded))
                              "the child's recorded effort defaults independently of the parent"))
               (let* ((later      (world-message-send configuration :sender (participant-identifier sender)
                                                      :channel child :text "Second turn"))
                      (deliveries (world-inbox-deliveries configuration child)))
                 (test-assert (and (equal (mapcar #'world-delivery-text deliveries)
                                          '("First turn" "Second turn"))
                                   (equal (mapcar #'world-delivery-identifier deliveries)
                                          (list (getf (rest details) :message-id) (getf later :id))))
                              "first and later messages are ordered durable inbox deliveries"))))
        (tool-registry-close-runtime-state registry))))
  nil)
