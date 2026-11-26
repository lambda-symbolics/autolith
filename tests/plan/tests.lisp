(in-package #:autolith)

(-> test-workspace-plan () null)
(defun test-workspace-plan ()
  "Test workspace-isolated plan updates, migration, rendering, and tool-level clearing."
  (with-test-configuration (base-configuration root)
    (let* ((workspace-a (merge-pathnames "workspace-a/" root))
           (workspace-b (merge-pathnames "workspace-b/" root)))
      (ensure-directories-exist workspace-a)
      (ensure-directories-exist workspace-b)
      (configuration-ensure-directories base-configuration)
      (let ((configuration-a
              (configuration-copy
               base-configuration :working-directory workspace-a))
            (configuration-b
              (configuration-copy
               base-configuration :working-directory workspace-b)))
        (test-assert (null (plan-load configuration-a))
                     "a workspace begins without a plan")
        (let ((plan
                (plan-update
                 configuration-a
                 (list (list :text "inspect the queue" :status "pending")
                       (list :text "fix the 429 path" :status "doing")
                       (list :text "ship the release" :status "done"))
                 :explanation "Finish the 0.20.3 release work.")))
          (test-assert (= (length (workspace-plan-steps plan)) 3)
                       "plan update retains ordered steps")
          (test-assert
           (eq (plan-step-status (second (workspace-plan-steps plan)))
               ':doing)
           "plan statuses normalize doing aliases")
          (let ((loaded   (plan-load configuration-a))
                (rendered (plan-render plan)))
            (test-assert
             (and loaded
                  (string= (workspace-plan-explanation loaded)
                           "Finish the 0.20.3 release work."))
             "plan load restores the explanation")
            (test-assert (search "[doing] fix the 429 path" rendered)
                         "plan rendering shows status and text")))
        (plan-update
         configuration-b
         (list (list :text "keep workspace B" :status "pending")))
        (test-assert
         (not (equal (plan--pathname configuration-a)
                     (plan--pathname configuration-b)))
         "different workspaces use different plan snapshots")
        (test-assert
         (string= (plan-step-text
                   (first (workspace-plan-steps
                           (plan-load configuration-a))))
                  "inspect the queue")
         "updating workspace B preserves workspace A's plan")
        (plan-clear configuration-a)
        (test-assert
         (string= (plan-step-text
                   (first (workspace-plan-steps
                           (plan-load configuration-b))))
                  "keep workspace B")
         "clearing workspace A preserves workspace B's plan")
        (plan-clear configuration-b)
        (plan-update
         configuration-a
         (list (list :text "migrate the legacy plan" :status "doing")))
        (rename-file (plan--pathname configuration-a)
                     (configuration-legacy-plan-path configuration-a))
        (test-assert
         (and (null (plan-load configuration-b))
              (probe-file
               (configuration-legacy-plan-path configuration-b)))
         "another workspace cannot claim the legacy global plan")
        (let ((migrated (plan-load configuration-a)))
          (test-assert
           (and migrated
                (string= (plan-step-text
                          (first (workspace-plan-steps migrated)))
                         "migrate the legacy plan")
                (probe-file (plan--pathname configuration-a))
                (not (probe-file
                      (configuration-legacy-plan-path configuration-a))))
           "the owning workspace migrates legacy plan state atomically"))
        (let* ((registry (make-default-tool-registry))
               (conversation
                 (conversation-create configuration-a
                                      :identifier "plan-tool-clear"))
               (context
                 (make-instance 'tool-context
                                :configuration configuration-a
                                :worker        nil
                                :conversation  conversation))
               (result
                 (tool-registry-execute-call
                  registry
                  (json-object
                   "namespace" "plan"
                   "name" "update"
                   "arguments"
                   (json-encode (json-object "steps" (json-array))))
                  context)))
          (test-assert
           (and (null (tool-registry-find registry "plan" "clear"))
                (typep (tool-registry-find registry "plan" "update")
                       'plan-update-tool))
           "plan.update subsumes the removed plan.clear tool")
          (test-assert
           (and (tool-result-success-p result)
                (string= (tool-result-content result) "No active plan.")
                (null (plan-load configuration-a)))
           "an empty plan.update clears the durable workspace plan")))))
  nil)
