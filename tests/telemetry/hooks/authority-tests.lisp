(in-package #:autolith)

;;;; -- Task and Mission Authority Failures --

(-> test-telemetry-mission-authority-hooks () null)
(defun test-telemetry-mission-authority-hooks ()
  "Mission, review and schedule emitters preserve authority without relabeling validation."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((application  (mission-test--application configuration))
            (conversation (conversation-create configuration)))
       (let ((spans (test-telemetry-hooks--tool-attempts
                     configuration
                     (loop for namespace in '("mission-review" "mission-schedule")
                           collect (agent-test-call :call-id namespace :namespace namespace :name "inspect" :arguments "{}"))
                     :agent        (application-agent application)
                     :conversation (application-conversation application)
                     :registry     (application-tool-registry application))))
         (test-assert (= 2 (length spans)) "Both tool families run as the legitimate primary without a mission.")
         (dolist (span spans)
           (test-telemetry-hooks--assert-failure span "error" "tool-failure")))
       (application-mission-start application (mission-test--specification))
       (mission-context-inherit (application-conversation application) conversation)
       (let ((spans (test-telemetry-hooks--tool-attempts
                     configuration
                     (list (agent-test-call :call-id "mission" :namespace "mission" :name "verify" :arguments "{}")
                           (agent-test-call :call-id "review" :namespace "mission-review" :name "inspect" :arguments "{}")
                           (agent-test-call :call-id "schedule" :namespace "mission-schedule" :name "inspect" :arguments "{}"))
                     :conversation conversation)))
         (test-assert (= 3 (length spans)) "All three real mission authority emitters run.")
         (dolist (span spans)
           (test-telemetry-hooks--assert-failure span "permission_denied" "authority")))
       (let ((spans (test-telemetry-hooks--tool-attempts
                     configuration
                     (loop for decision in '("invalid" "resolve")
                           for reason in '("evidence" "")
                           collect (agent-test-call
                                    :call-id decision :namespace "mission-review" :name "decide"
                                    :arguments (json-encode (json-object "id" "missing" "finding" "missing"
                                                                         "decision" decision "reason" reason))))
                     :agent        (application-agent application)
                     :conversation (application-conversation application)
                     :registry     (application-tool-registry application))))
         (test-assert (= 2 (length spans)) "Both primary review validation branches run.")
         (loop for span in spans for code in '("type-error" "tool-failure")
                do (test-telemetry-hooks--assert-failure span "error" code))))))
  nil)

(-> test-telemetry-continuity-authority-hooks () null)
(defun test-telemetry-continuity-authority-hooks ()
  "Only explicit continuity refusals are denials; malformed actions remain errors."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((orchestrator (task-tests--orchestrator))
            (registry     (make-instance 'tool-registry)))
       (unwind-protect
            (progn
              (tool-registry-register registry
                                      (make-instance 'task-continuity-tool :namespace "job" :name "continuity"
                                                     :description "Continuity decisions."
                                                     :parameters (task-continuity-parameters) :orchestrator orchestrator))
              (let ((spans (test-telemetry-hooks--tool-attempts
                            configuration
                             (loop for action in '("checkpoint" "abandon" "invalid")
                                  collect (agent-test-call :call-id action :namespace "job" :name "continuity"
                                                           :arguments (json-encode (json-object "action" action "id" "absent"))))
                            :registry registry)))
                 (test-assert (= 3 (length spans)) "All continuity refusal and validation branches run.")
                 (dolist (span (subseq spans 0 2))
                   (test-telemetry-hooks--assert-failure span "permission_denied" "authority"))
                 (test-telemetry-hooks--assert-failure (third spans) "error" "invalid-arguments")))
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-telemetry-worker-host-admission-hooks () null)
(defun test-telemetry-worker-host-admission-hooks ()
  "Real Lisp host-tool admission distinguishes unavailable names from authority refusals."
  (test-telemetry--call
   (lambda (configuration)
     (let ((cases '((#("typo.nonexistent") "error" "tool-unavailable")
                    (#("self.status") "permission_denied" "capability-denied")
                    (#(12) "error" "invalid-arguments"))))
       (let ((spans (test-telemetry-hooks--tool-attempts
                     configuration
                     (loop for (requested) in cases for index from 1
                           collect (agent-test-call
                                    :call-id   (format nil "host-admission-~D" index)
                                    :namespace "lisp" :name "eval"
                                    :arguments (json-encode
                                                (json-object "forms" #("42") "host-tools" requested)))))))
         (test-assert (= (length cases) (length spans)) "Every real host-tools admission failure runs.")
         (loop for span in spans for case in cases
               do (test-telemetry-hooks--assert-failure span (second case) (third case)))))))
  nil)

(-> test-telemetry-worktree-authority-hook () null)
(defun test-telemetry-worktree-authority-hook ()
  "Distinguish denied Git execution from malformed worktree arguments."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((root          (test-configuration-root configuration))
            (configuration (configuration-copy configuration :working-directory root))
            (registry      (make-instance 'tool-registry))
            (parent        (task-tests--primary-agent configuration "worktree-telemetry" registry))
            (context       (make-instance 'tool-context :configuration configuration :agent parent
                                          :conversation (agent-conversation parent) :registry registry
                                          :command-authorization-function
                                          (lambda (command directory)
                                            (declare (ignore command directory))
                                            ':deny))))
       (task-worktree-tests--git configuration root '("init"))
       (task-worktree-register-tool registry)
       (let* ((bodies
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (telemetry-call-with-run
                    configuration
                    (lambda ()
                      (loop for action in '("discover" "inspect")
                            for result = (tool-registry-execute-call
                                          registry
                                          (agent-test-call :call-id action :namespace "child" :name "worktree"
                                                           :arguments (json-encode (json-object "action" action)))
                                          context)
                            do (telemetry-note-tool :tool "child.worktree" :outcome "error"
                                                    :error-code (tool-result-error-code result))))))))
              (spans (test-telemetry-hooks--kind (first bodies) "tool")))
         (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "authority")
         (test-telemetry-hooks--assert-failure (second spans) "error" "invalid-arguments")))))
  nil)
