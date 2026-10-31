(in-package #:autolith)

;;;; -- Authoritative Plan Context --

(-> test-plan-state-context-projects-current-plan () null)
(defun test-plan-state-context-projects-current-plan ()
  "Project the durable plan with its current revision and complete evidence."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "plan-context"))
           (request (make-instance 'request-context
                                   :configuration configuration
                                   :conversation conversation
                                   :tool-namespaces #())))
      (plan-update configuration
                    (list (list :text "inspect the queue" :status "pending"
                                :requires '((queue :open))))
                    :explanation "Release evidence.")
      (let* ((plan (plan-load configuration))
             (contribution (plan-state-context request))
             (instruction (context-contribution-instruction contribution)))
        (test-assert contribution "the active plan contributor is present")
        (test-assert (= (context-contribution-priority contribution) 1000)
                     "the active plan is mandatory high-priority context")
        (test-assert (search (namestring (plan--pathname configuration)) instruction)
                     "the contribution identifies its authoritative resource")
        (test-assert (search (princ-to-string (workspace-plan-updated-at plan)) instruction)
                     "the contribution identifies the exact plan revision")
        (test-assert (search "inspect the queue" instruction)
                     "the contribution includes the current plan steps")
        (test-assert (search "queue" instruction)
                       "the contribution includes durable step evidence"))))
  nil)

(-> test-plan-state-context-rebuilds-after-plan-update () null)
(defun test-plan-state-context-rebuilds-after-plan-update ()
  "Rebuild the authoritative contribution after a plan update."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "plan-reload"))
           (request (make-instance 'request-context
                                   :configuration configuration
                                   :conversation conversation
                                   :tool-namespaces #())))
      (plan-update configuration
                    (list (list :text "first revision" :status "doing")))
      (let ((first (plan-state-context request)))
        (plan-update configuration
                      (list (list :text "replacement revision" :status "done"))
                      :explanation "Updated evidence.")
        (let ((second (plan-state-context request)))
          (test-assert (not (equal (context-contribution-instruction first)
                                   (context-contribution-instruction second)))
                       "a later plan update replaces the prior projection")
          (test-assert (search "replacement revision"
                               (context-contribution-instruction second))
                       "the rebuilt projection contains the latest plan")
          (test-assert (search "Updated evidence"
                               (context-contribution-instruction second))
                               "the rebuilt projection contains the latest explanation")))))
  nil)
