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
               do (test-telemetry-hooks--assert-failure span "error" code)))
       (test-assert
        (handler-case
            (progn (mission-review--run-task
                    (mission-context-find (application-conversation application)) nil
                    :tool-context (mission-test--context application))
                   nil)
          (mission-error (condition)
            (and (eq ':unavailable (mission-error-reason condition))
                 (eq ':tool-failure (tool-failure-code condition)))))
        "A missing review runtime remains a lifecycle error.")
       (let ((spans (test-telemetry-hooks--tool-attempts
                     configuration (list (agent-test-call :call-id "missing" :namespace "mission" :name "verify" :arguments "{}")))))
         (test-telemetry-hooks--assert-failure (first spans) "error" "tool-failure")))))
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
                            (loop for action in '("allow-restart" "checkpoint" "abandon" "invalid")
                                  collect (agent-test-call :call-id action :namespace "job" :name "continuity"
                                                           :arguments (json-encode (json-object "action" action "id" "absent"))))
                            :registry registry)))
                (test-assert (= 4 (length spans)) "All continuity refusal and validation branches run.")
                (dolist (span (subseq spans 0 3))
                  (test-telemetry-hooks--assert-failure span "permission_denied" "authority"))
                (test-telemetry-hooks--assert-failure (fourth spans) "error" "invalid-arguments")))
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-telemetry-peer-authority-hooks () null)
(defun test-telemetry-peer-authority-hooks ()
  "Peer grants and message ownership are denials; invalid or missing identities remain errors."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((*peer-message-services* (make-hash-table :test #'equal))
            (orchestrator            (task-tests--orchestrator))
            (registry                (peer-message-register-tools (make-instance 'tool-registry) :orchestrator orchestrator)))
       (unwind-protect
            (let ((spans (test-telemetry-hooks--tool-attempts
                          configuration
                          (loop for text in '("message" "") for index from 1
                                collect (agent-test-call :call-id (format nil "peer-~D" index) :namespace "peer" :name "send"
                                                         :arguments (json-encode (json-object "id" "message" "receiver" "ungranted" "text" text))))
                          :registry registry)))
              (test-assert (= 2 (length spans)) "Both real peer send failures run.")
              (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "authority")
              (test-telemetry-hooks--assert-failure (second spans) "error" "invalid-arguments"))
         (task-orchestrator-close orchestrator)))))
  (unwind-protect
       (peer-message-tests--fixture
        (lambda (&key configuration owner sender receiver service path registry)
          (declare (ignore path))
          (let ((job (task-child-agent-job sender)))
            (with-lock-held ((cl-jobpond::job--lock job))
              (setf (job-state job) ':running))
            (task-job--set-progress-state job ':running))
          (peer-message-tests--grant service owner :sender sender :receiver receiver)
          (peer-message-send service sender :id "owned" :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver))
                                           :text "Private peer payload")
          (let* ((delivery (peer-message-receive service receiver :id "owned"))
                 (token    (getf delivery :token)))
            (multiple-value-bind (ack-spans outputs)
                (test-telemetry-hooks--tool-attempts
                 configuration
                 (loop for id in '("owned" "missing" "")
                       collect (agent-test-call :call-id (concatenate 'string "ack-" id) :namespace "peer" :name "ack"
                                                :arguments (json-encode (json-object "id" id "token" token))))
                 :agent sender :conversation (agent-conversation sender) :registry registry)
              (test-assert (= 3 (length ack-spans)) "The sender attempts another receiver's acknowledgment and invalid IDs.")
              (test-telemetry-hooks--assert-failure (first ack-spans) "permission_denied" "invalid-arguments")
              (test-assert (equal (first outputs) (second outputs))
                           "Acknowledgment replies do not disclose whether an inaccessible ID exists.")
              (dolist (span (rest ack-spans))
                (test-telemetry-hooks--assert-failure span "error" "invalid-arguments"))))
          ;; A new message from the primary to the receiver is outside the first child's scope.
          (peer-message-grant service owner :id "primary-to-receiver"
                              :sender (peer-message--actor-endpoint owner) :receiver (peer-message--actor-endpoint receiver))
          (peer-message-send service owner :id "unrelated" :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver))
                                          :text "Unrelated payload")
          (multiple-value-bind (spans outputs body)
              (test-telemetry-hooks--tool-attempts
               configuration
               (loop for id in '("unrelated" "missing" "")
                     collect (agent-test-call :call-id (concatenate 'string "inspect-" id) :namespace "peer" :name "inspect"
                                              :arguments (json-encode (json-object "id" id))))
               :agent sender :conversation (agent-conversation sender) :registry registry)
            (test-assert (= 3 (length spans)) "Inspection checks ownership, absence and malformed IDs through real tools.")
            (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "invalid-arguments")
            (test-assert (equal (first (last outputs 3)) (second (last outputs 3)))
                         "Inspection replies do not disclose whether an inaccessible ID exists.")
            (dolist (span (rest spans))
              (test-telemetry-hooks--assert-failure span "error" "invalid-arguments"))
            (test-assert (not (search "Unrelated payload" body)) "Unrelated message content stays off the wire."))))
    (telemetry-shutdown))
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
  "Exercise the worktree emitter directly because child registries normally withhold it."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((orchestrator (task-tests--orchestrator))
            (job          (task-tests--make-job
                           orchestrator :identifier "worktree-child" :root-identifier "worktree-owner"
                           :definition (task-agent-definition-create :name "worker" :description "Worktree fixture."
                                                                     :instructions "Return a result.")))
            (child        (task-tests--child-viewer configuration job))
            (registry     (make-instance 'tool-registry))
            (context      (make-instance 'tool-context :configuration configuration :agent child
                                         :conversation (agent-conversation child) :registry registry :worker nil)))
       (unwind-protect
            (progn
              (task-worktree-register-tool registry orchestrator)
              (let* ((bodies (test-telemetry-hooks--capture
                              configuration
                              (lambda ()
                                (telemetry-call-with-run
                                 configuration
                                 (lambda ()
                                   (let ((result (tool-registry-execute-call
                                                  registry (agent-test-call :call-id "child" :namespace "child" :name "worktree"
                                                                            :arguments "{\"action\":\"inspect\"}")
                                                  context)))
                                     (test-assert (search "belong to the primary parent" (tool-result-content result))
                                                  "The child worktree refusal emitter runs.")
                                     (telemetry-note-tool :tool "child.worktree" :outcome "error"
                                                          :error-code (tool-result-error-code result)))
                                   (let* ((primary-context (make-instance 'tool-context :configuration configuration
                                                                         :registry registry :worker nil))
                                          (result          (tool-registry-execute-call
                                                            registry (agent-test-call :call-id "invalid" :namespace "child" :name "worktree"
                                                                                      :arguments "{\"action\":\"inspect\"}")
                                                            primary-context)))
                                     (telemetry-note-tool :tool "child.worktree" :outcome "error"
                                                          :error-code (tool-result-error-code result))))))))
                     (span   (first (test-telemetry-hooks--kind (first bodies) "tool"))))
                (test-telemetry-hooks--assert-failure span "permission_denied" "authority")
                (test-telemetry-hooks--assert-failure
                 (second (test-telemetry-hooks--kind (first bodies) "tool")) "error" "invalid-arguments")))
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-telemetry-child-authority-hooks () null)
(defun test-telemetry-child-authority-hooks ()
  "Export child image and spawn refusals while retaining ordinary argument errors."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry     (task-augment-tool-registry (make-default-tool-registry)))
            (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
            (job          (task-tests--make-job
                           orchestrator :identifier "authority-child" :root-identifier "authority-owner"
                           :definition (task-agent-definition-create
                                        :name "worker" :description "Authority fixture."
                                        :instructions "Return a result." :spawns '("allowed"))))
            ;; Use the parent registry to reach emitters normally withheld from children.
            (child        (task-tests--child-viewer configuration job :registry registry))
            (cases        '(("lisp" "describe" "{\"designator\":\"CL:CAR\",\"target\":\"self\"}" "permission_denied" "authority")
                            ("lisp" "source" "{\"name\":\"CL:CAR\",\"target\":\"self\"}" "permission_denied" "authority")
                            ("lisp" "apropos" "{\"query\":\"CAR\"}" "permission_denied" "authority")
                            ("child" "spawn" "{\"agent\":\"forbidden\",\"task\":\"Return a result.\"}" "permission_denied" "authority")
                            ("lisp" "describe" "{\"designator\":\"CL:CAR\",\"target\":\"invalid\"}" "error" "invalid-arguments")
                            ("child" "spawn" "{\"agent\":\"allowed\",\"task\":\"Return a result.\"}" "error" "invalid-arguments"))))
       (with-lock-held ((cl-jobpond::job--lock job))
         (setf (job-state job) ':running))
       (task-job--set-progress-state job ':running)
       (unwind-protect
            (let ((spans (test-telemetry-hooks--tool-attempts
                          configuration
                          (loop for (namespace name arguments) in cases for index from 1
                                collect (agent-test-call :call-id (format nil "authority-~D" index)
                                                         :namespace namespace :name name :arguments arguments))
                          :agent child :conversation (agent-conversation child) :registry registry)))
              (test-assert (= (length cases) (length spans)) "Every child authority and validation emitter runs.")
              (loop for span in spans for case in cases
                    do (test-telemetry-hooks--assert-failure span (fourth case) (fifth case))))
         (task-orchestrator-close orchestrator)))))
  nil)
