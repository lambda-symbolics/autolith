(in-package #:autolith)

;;;; -- Detached Completion Policy Tests --

(-> test-job-completion-policy-normalization () null)
(defun test-job-completion-policy-normalization ()
  "Test policy defaults, explicit selections, batch inheritance, and rejection."
  (dolist (case '((nil :continue) ("notify" :notify) ("continue" :continue)))
    (destructuring-bind (value expected) case
      (let ((arguments (json-object "task" "Return one result.")))
        (when value
          (setf (gethash "completion-policy" arguments) value))
        (test-assert
         (eq expected (getf (first (task-normalize-arguments arguments))
                            :completion-policy))
         "flat task policy is normalized to a keyword"))))
  (let* ((arguments
           (json-object
            "context" "Shared test context."
            "completion-policy" "notify"
            "tasks" (vector
                     (json-object "task" "Inherited policy.")
                     (json-object "task" "Explicit continuation."
                                  "completion-policy" "continue")
                     (json-object "task" "Explicit notification."
                                  "completion-policy" "notify"))))
         (items (task-normalize-arguments arguments)))
    (test-assert
     (equal '(:notify :continue :notify)
            (mapcar (lambda (item) (getf item :completion-policy)) items))
     "batch items inherit the top policy and may override it"))
  (test-assert
   (eq ':continue
       (getf (first
              (task-normalize-arguments
               (json-object "context" "Shared context."
                            "tasks" (vector (json-object "task" "Default policy.")))))
             :completion-policy))
   "batch policy defaults to continue")
  (dolist (value '("poll" "CONTINUE" "" 1 t nil :notify))
    (dolist (location '(:flat :batch :item))
      (let* ((item (json-object "task" "Invalid policy."))
             (arguments
               (if (eq location ':flat)
                   item
                   (json-object "context" "Shared context." "tasks" (vector item)))))
        (setf (gethash "completion-policy"
                       (if (eq location ':item) item arguments))
              value)
        (test-assert
         (handler-case
             (progn (task-normalize-arguments arguments) nil)
           (tool-error () t))
         "invalid policy is rejected at every task input boundary"))))
  nil)

(-> test-job-completion-policy-schema-invocation () null)
(defun test-job-completion-policy-schema-invocation ()
  "Test that async tool schemas advertise the shared executable policy."
  (let* ((registry (task-augment-tool-registry (make-default-tool-registry)))
         (count 0))
    (unwind-protect
         (progn
           (dolist (tool (tool-registry-tools registry))
             (let ((properties (json-get (tool-parameters tool) "properties")))
               (when (and properties (gethash "async" properties))
                 (incf count)
                 (let ((schema (gethash "completion-policy" properties)))
                   (test-assert (json-object-p schema)
                                 (format nil "~A exposes completion policy" (tool-canonical-name tool)))
                   (when schema
                     (loop for value across (json-get schema "enum")
                           do (test-assert
                               (member
                                (tool-completion-policy-argument
                                 (json-object "completion-policy" value)
                                 :tool-name (tool-canonical-name tool))
                                '(:notify :continue))
                              "advertised completion selections are executable")))))))
           (test-assert (plusp count) "async schemas are exercised")
           (let* ((properties (json-get (task-run-parameters-schema) "properties"))
                  (item-properties
                    (json-get (json-get (json-get properties "tasks") "items")
                              "properties")))
             (test-assert
              (and (gethash "completion-policy" properties)
                   (gethash "completion-policy" item-properties))
              "task.run exposes policy at flat and batch-item boundaries")))
      (tool-registry-close-runtime-state registry)))
  nil)

(-> test-job-completion-policy-transport () null)
(defun test-job-completion-policy-transport ()
  "Test detached and grace-expired policy transport through execution wrappers."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (task-augment-tool-registry (make-default-tool-registry)))
         (orchestrator
           (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
         (primary (task-tests--primary-agent configuration "policy-primary" registry))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :conversation (agent-conversation primary)
                          :registry registry
                          :agent primary
                          :worker nil
                         :command-authorization-function
                          (lambda (command directory)
                            (declare (ignore command directory))
                            ':full-access))))
    (labels ((handoff-job (result)
               "Resolve the native handoff RESULT to its execution job."
               (let* ((record (getf (rest (tool-result-details result)) :job))
                      (job (task-orchestrator-find-visible-job
                            orchestrator (getf record :id) primary "job.get")))
                 (test-assert (session-job-detached-p job)
                              "handoff marks the job detached")
                 (test-assert
                  (eq (session-job-completion-policy job)
                      (getf record :completion-policy))
                  "native handoff exposes the job completion policy")
                 job))

           (check-wrapper (function arguments &key expected)
               "Check policy transport through FUNCTION without external dependencies."
               (let* ((result
                        (funcall function context arguments
                                :tool-name "test.policy" :summary "Policy transport"
                                 :operation-function (lambda () (tool-success "retained"))))
                      (job (handoff-job result)))
                 (test-assert (eq expected (session-job-completion-policy job))
                              "execution wrappers retain the selected policy")
                 (multiple-value-bind (snapshot terminal-p) (session-job-await job 5)
                   (declare (ignore snapshot))
                   (test-assert terminal-p "detached policy jobs finish normally"))
                 (test-assert
                  (string= "retained"
                           (tool-result-content (tool-execution-job-result->tool-result job)))
                  "both policies retain the original tool result"))))
      (unwind-protect
           (progn
             (dolist (function '(lisp-tool-invoke-managed-execution rlm--tool-invoke))
             (check-wrapper function (json-object "async" t) :expected ':continue)
             (check-wrapper function
                            (json-object "async" t "completion-policy" "notify")
                            :expected ':notify)
               (test-assert
                (handler-case
                    (progn
                      (funcall function context
                               (json-object "async" t "completion-policy" "invalid")
                               :tool-name "test.policy" :summary "Invalid policy"
                               :operation-function (lambda () (tool-success "unexpected")))
                      nil)
                  (tool-error () t))
                "execution wrappers reject invalid policy before admission"))
             (let* ((shell-tool (tool-registry-find registry "shell" "run"))
                    (result
                      (tool-execute
                       shell-tool context
                       (json-object
                        "command" (test-fixture-shell-command
                                   *platform* "printf policy" "[Console]::Write('policy')")
                        "async" t "completion-policy" "notify")))
                    (job (handoff-job result)))
               (test-assert (eq ':notify (session-job-completion-policy job))
                            "shell execution transports completion policy")
               (test-assert (search "use job.wait" (tool-result-content result))
                            "without a controller wakeup the handoff says to wait for the result")
               (session-job-await job 5))
             (task-completion-connect primary (lambda ()))
             (unwind-protect
                  (dolist (case '(("continue" "wakes you as new input; do not poll")
                                  ("notify" "arrives with your next turn; do not poll")))
                    (destructuring-bind (policy expected) case
                      (let ((result
                              (tool-execute
                               (tool-registry-find registry "shell" "run") context
                               (json-object
                                "command" (test-fixture-shell-command
                                           *platform* "printf policy" "[Console]::Write('policy')")
                                "async" t "completion-policy" policy))))
                        (test-assert (search expected (tool-result-content result))
                                     (format nil "a connected ~A handoff tells the model not to poll"
                                             policy))
                        (session-job-await (handoff-job result) 5))))
               (task-completion-disconnect primary))
             (let* ((*tool-execution-blocking-grace-seconds* 0)
                    (barrier
                      (make-instance 'task-test-blocking-tool
                                     :namespace "test" :name "barrier"
                                     :description "Wait for test release."
                                     :parameters (tool-object-schema (json-object) nil))))
               (unwind-protect
                    (let* ((result
                             (tool-execution-invoke
                              orchestrator primary :tool-name "test.grace"
                              :summary "Grace-expired policy"
                              :operation-function
                              (lambda () (tool-execute barrier context (json-object)))))
                           (job (handoff-job result)))
                      (test-assert
                       (eq ':continue (session-job-completion-policy job))
                       "grace-expired jobs default to automatic continuation"))
                 (with-lock-held ((task-test-blocking-tool-lock barrier))
                   (setf (task-test-blocking-tool-released-p barrier) t)
                   (task--condition-broadcast
                    (task-test-blocking-tool-condition-variable barrier))))))
        (ignore-errors (task-orchestrator-close orchestrator))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t
                                             :if-does-not-exist ':ignore))))
  nil)
