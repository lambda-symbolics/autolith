(in-package #:autolith)

;;;; -- Independent Reviewer Boundary Tests --

(defclass mission-review-test-provider (scripted-provider) ()
  (:documentation "Scripted provider whose ordinary children inherit reference history."))

(defmethod provider-with-configuration
    ((provider mission-review-test-provider) (configuration configuration))
  "Preserve scripted responses when the task configures its independent provider."
  (declare (ignore configuration))
  provider)

(defmethod provider-child-reference-history-p ((provider mission-review-test-provider))
  "Enable inheritance so checkpoint isolation is tested rather than assumed."
  (declare (ignore provider))
  t)

(-> mission-review-test--data (&key (:findings boolean)) json-object)
(defun mission-review-test--data (&key (findings t))
  "Return one native reviewer response with identified evidence."
  (json-object "summary" "Observed advisory risk"
               "findings" (if findings
                              (vector (json-object "id" "risk" "severity" "critical"
                                                   "confidence" "high" "summary" "Missing validation"
                                                   "evidence" #("artifact.lisp:1")))
                              #())))

(-> mission-review-test--result (json-object) provider-result)
(defun mission-review-test--result (data)
  "Return an actual yield invocation with exact reported usage."
  (agent-test-result
   (make-identifier)
   (list (agent-test-call :call-id "review-yield" :namespace "yield" :name "submit"
                          :arguments (json-encode (json-object "status" "success" "data" data))))
   :usage (json-object "input_tokens" 2 "output_tokens" 3)))

(-> mission-review-test--policy (&key (:turns integer) (:tokens integer) (:runs integer) (:phase keyword)) list)
(defun mission-review-test--policy (&key (turns 4) (tokens 100) (runs 4) (phase ':final-acceptance))
  "Return explicitly configured independent review policy."
  (list :id "checkpoint" :phase phase :inputs '("artifact.lisp")
        :context "Inspect the supplied artifact independently."
        :turn-limit turns :token-limit tokens :run-limit runs))

(-> mission-review-test--prepare (configuration list) application)
(defun mission-review-test--prepare (configuration results)
  "Create a real task-capable primary application with a scripted reviewer."
  (setf configuration (configuration-copy configuration
                                          :working-directory (merge-pathnames "review-workspace/" (config :data-root configuration))))
  (ensure-directories-exist (merge-pathnames "artifact.lisp" (config :working-directory configuration)))
  (let* ((application (mission-test--application configuration))
         (provider (make-instance 'mission-review-test-provider :results results)))
    (setf (application-provider application) provider
          (agent-provider (application-agent application)) provider)
    (task-augment-tool-registry (application-tool-registry application))
    (mission-test--write-file (merge-pathnames "artifact.lisp" (config :working-directory configuration)) "(print 1)")
    (conversation-append-user-message (application-conversation application) "primary-hidden-reference")
    (application-mission-start application (mission-test--specification))
    application))

(-> mission-review-test--close (application) null)
(defun mission-review-test--close (application)
  "Close the fixture's actual existing task runtime."
  (task-orchestrator-close
   (tool-registry-runtime-binding (application-tool-registry application) 'task-orchestrator))
  nil)

(-> test-mission-review-checkpoint-lifecycle () null)
(defun test-mission-review-checkpoint-lifecycle ()
  "Invoke an independent real task, retain decisions and suppress unchanged work."
  (with-test-configuration (configuration)
    (let* ((application (mission-review-test--prepare
                         configuration (list (mission-review-test--result (mission-review-test--data)))))
           (provider (application-provider application)))
      (unwind-protect
           (progn
             (application-mission-review-configure application (mission-review-test--policy))
             (application-mission-verify application)
             (let* ((policy (first (getf (application-goal application) :reviews)))
                    (run (first (getf policy :runs)))
                    (finding (first (getf run :findings))))
               (test-assert (eq (getf run :status) ':completed) "a real contracted reviewer completes")
               (test-assert (and (equal "critical" (getf finding :severity))
                                 (equal "high" (getf finding :confidence))
                                 (equal '("artifact.lisp:1") (getf finding :evidence)))
                            "identified severity/confidence/evidence survive the task artifact")
               (test-assert (eq ':active (getf (application-goal application) :status))
                            "pending advice prevents final acceptance without claiming failed gates")
               (test-assert (= 1 (getf policy :turns-used)) "reviewer has a separately charged call allowance")
               (test-assert (= 5 (getf policy :tokens-used)) "reviewer has separately charged billable usage")
               (test-assert (and (= 1 (getf (application-goal application) :turns-used))
                                 (= 5 (getf (application-goal application) :tokens-used)))
                            "reviewer calls are also charged exactly once to the mission")
               (test-assert
                (not (search "primary-hidden-reference"
                             (prin1-to-string (scripted-provider-input-snapshots provider))))
                "checkpoint context excludes parent reference history even on supporting providers")
               (application-mission-review-trigger application "checkpoint")
               (test-assert (= 1 (length (scripted-provider-input-counts provider)))
                            "unchanged checkpoint is not billed or reviewed again")
               (test-assert
                (handler-case
                    (progn (application-mission-review-decide application "checkpoint" "risk"
                                                             :agent (make-instance 'agent :configuration configuration)
                                                             :decision ':reject :reason "untrusted") nil)
                  (mission-error (condition) (eq ':review-authority (mission-error-reason condition))))
                "another agent cannot adjudicate primary findings")
               (application-mission-review-decide application "checkpoint" "risk"
                                                  :decision ':resolve :reason "Validated by the primary")
               (test-assert (application-mission-review-checkpoint application ':final-acceptance)
                            "adjudicated findings allow checkpoint passage")
               (application-mission-review-decide application "checkpoint" "risk"
                                                  :decision ':reject :reason "Primary retained counter-evidence")
               (let* ((conversation (application-conversation application))
                      (identifier (conversation-identifier conversation)))
                 (conversation-append-summary conversation "Compacted unrelated discussion")
                 (setf (application-conversation application) (conversation-load-by-id configuration identifier))
                 (application--load-goal application)
                 (let* ((restored (first (getf (application-goal application) :reviews)))
                        (decision (first (getf (first (getf restored :runs)) :findings))))
                   (test-assert (and (eq ':reject (getf decision :decision))
                                     (equal "Primary retained counter-evidence" (getf decision :reason))
                                     (= 5 (getf restored :tokens-used)))
                                "decision reasons and reviewer usage survive compaction and disk restart")))
               (test-assert (eq ':completed (getf (application-mission-review-trigger application "checkpoint") :status))
                            "restarted unchanged checkpoint reuses durable successful result")
               (test-assert (= 1 (length (scripted-provider-input-counts provider)))
                            "restart does not reset the review budget or cause duplicate inference")))
        (mission-review-test--close application))))
  nil)

(-> test-mission-review-native-contract () null)
(defun test-mission-review-native-contract ()
  "Reject malformed advice through the actual native yield schema boundary."
  (with-test-configuration (configuration)
    (let ((definition (mission-review--definition (mission-review-test--policy))))
      (dolist (field '("id" "severity" "confidence" "evidence"))
        (let* ((data (mission-review-test--data))
               (fixture (task-tests--yield-fixture configuration definition (format nil "review-missing-~A" field))))
          (remhash field (aref (gethash "findings" data) 0))
          (test-assert (not (tool-result-success-p
                            (task-tests--execute-yield fixture
                                                      (json-encode (json-object "status" "success" "data" data)))))
                       "missing reviewer fields cannot complete the native output contract")))
      (let ((data (mission-review-test--data)))
        (setf (gethash "severity" (aref (gethash "findings" data) 0)) "invented")
        (test-assert (not (task-output-schema-valid-p data (task-agent-definition-output definition)))
                     "unidentified severity categories are rejected"))
      (let* ((data (mission-review-test--data))
             (finding (aref (gethash "findings" data) 0)))
        (setf (gethash "findings" data) (vector finding finding))
        (test-assert
         (handler-case (progn (mission-review--findings
                               (list :status ':success :structured-output-present-p t
                                     :structured-output (task-json->sexp data))) nil)
           (mission-error () t))
         "duplicate finding IDs cannot become ambiguous primary decisions"))))
  nil)

(-> mission-review-test--acceptance-race () null)
(defun mission-review-test--acceptance-race ()
  "Exercise complete-current-run validation and refused admission at final acceptance."
  (with-test-configuration (configuration)
    (let* ((application (mission-review-test--prepare
                         configuration
                         (list (mission-review-test--result (mission-review-test--data :findings nil)))))
           (original-checkpoint (symbol-function 'application-mission-review-checkpoint)))
      (unwind-protect
           (progn
             (application-mission-review-configure application (mission-review-test--policy :runs 1))
             (test-assert (application-mission-review-checkpoint application ':final-acceptance)
                          "the original completed independent review permits acceptance")
             (let* ((policy (mission-review--policy (mission-review--context application) "checkpoint"))
                    (run (first (getf policy :runs)))
                    (original-fingerprint (copy-tree (getf policy :current-fingerprint))))
               (dolist (status '(:running :failed :interrupted :cancelled))
                 (setf (getf run :status) status)
                 (test-assert (not (application-mission-review-current-p application ':final-acceptance))
                              "matching inputs alone cannot make an unfinished review acceptable"))
               (setf (getf run :status) ':completed
                     (getf run :findings) (list (list :id "late-finding" :decision ':pending)))
               (test-assert (not (application-mission-review-current-p application ':final-acceptance))
                            "current completed review still requires adjudicated findings")
               (setf (getf (first (getf run :findings)) :decision) ':resolve)
               (test-assert (application-mission-review-current-p application ':final-acceptance)
                            "current completed adjudicated review is acceptable")
               (setf (getf run :findings) nil)
               ;; Inject the concurrent actor's effects immediately after the first
               ;; checkpoint passes, before verification takes its request lock.
               (setf (symbol-function 'application-mission-review-checkpoint)
                     (lambda (target phase)
                       (let ((accepted-p (funcall original-checkpoint target phase)))
                         (when accepted-p
                           (mission-test--write-file
                            (merge-pathnames "artifact.lisp"
                                             (config :working-directory (application-configuration application)))
                            "(print 2)")
                           (test-assert
                            (handler-case
                                (progn (application-mission-review-trigger target "checkpoint") nil)
                              (mission-error (condition)
                                (eq ':review-budget (mission-error-reason condition))))
                            "the concurrent changed-input review is refused at its run allowance")
                           (test-assert (equal original-fingerprint (getf policy :current-fingerprint))
                                        "refused review admission cannot publish new applicability"))
                         accepted-p)))
               (application-mission-verify application)
               (test-assert (eq ':active (getf (application-goal application) :status))
                            "final acceptance stops when review inputs changed after its checkpoint")
               (test-assert (not (application-mission-review-current-p application ':final-acceptance))
                            "the changed snapshot has no completed adjudicated run")))
        (setf (symbol-function 'application-mission-review-checkpoint) original-checkpoint)
        (mission-review-test--close application))))
  nil)

(-> test-mission-review-budget-and-failure () null)
(defun test-mission-review-budget-and-failure ()
  "Keep checkpoint budgets separate and reuse failed/changed durable outcomes."
  (with-test-configuration (configuration)
    (let* ((application (mission-review-test--prepare
                         configuration (list (mission-test--result "No yield yet")
                                             (mission-review-test--result (mission-review-test--data :findings nil)))))
           (provider (application-provider application)))
      (unwind-protect
           (progn
             (application-mission-review-configure application (mission-review-test--policy :turns 1))
             (test-assert (eq ':failed (getf (application-mission-review-trigger application "checkpoint") :status))
                          "reviewer call budget failure is explicit")
             (test-assert (= 1 (length (scripted-provider-input-counts provider)))
                          "second reviewer call is refused before provider invocation")
             (application-mission-review-trigger application "checkpoint")
             (test-assert (= 1 (length (scripted-provider-input-counts provider)))
                          "unchanged reviewer failure is not retried")
             (mission-test--write-file (merge-pathnames "artifact.lisp" (config :working-directory (application-configuration application))) "(print 2)")
             (test-assert (not (application-mission-review-current-p application ':final-acceptance))
                          "changed relevant inputs invalidate completed checkpoint applicability")
             (test-assert (eq ':failed (getf (application-mission-review-trigger application "checkpoint") :status))
                          "changed checkpoint cannot reset cumulative reviewer allowance")
             (test-assert (= 1 (length (scripted-provider-input-counts provider)))
                          "changed snapshot cannot bypass the separate call allowance"))
        (mission-review-test--close application))))
  (with-test-configuration (configuration)
    (let ((application (mission-review-test--prepare configuration nil)))
      (unwind-protect
           (progn
             (test-assert (application-mission-review-checkpoint application ':final-acceptance)
                          "unconfigured missions never invoke another model")
             (application-mission-review-configure application (mission-review-test--policy))
             (let* ((context (mission-review--context application))
                    (policy (mission-review--policy context "checkpoint"))
                    (fingerprint (first (multiple-value-list
                                         (mission-review--snapshot policy (mission-test--context application))))))
               (setf (getf policy :runs) (list (list :fingerprint fingerprint :status ':running)))
               (mission--record context)
               (test-assert (eq ':interrupted (getf (application-mission-review-trigger application "checkpoint") :status))
                            "lost in-flight review is durably interrupted rather than silently relaunched")))
        (mission-review-test--close application))))
  (mission-review-test--acceptance-race)
  nil)

(-> test-mission-review-cancellation-and-authority () null)
(defun test-mission-review-cancellation-and-authority ()
  "Retain reviewer aborts and enforce the primary tool/decision boundary."
  (with-test-configuration (configuration)
    (let* ((response (agent-test-result
                      (make-identifier)
                      (list (agent-test-call :call-id "review-abort" :namespace "yield" :name "submit"
                                             :arguments "{\"status\":\"aborted\",\"error\":\"review cancelled\"}"))
                      :usage (json-object "input_tokens" 2 "output_tokens" 3)))
           (application (mission-review-test--prepare configuration (list response))))
      (unwind-protect
           (progn
             (application-mission-review-configure application (mission-review-test--policy))
             (let ((run (application-mission-review-trigger application "checkpoint")))
               (test-assert (eq ':cancelled (getf run :status)) "reviewer abort is explicit and durable")
               (test-assert (not (application-mission-review-checkpoint application ':final-acceptance))
                            "cancelled reviewer cannot prove final acceptance"))
             (mission-review-augment-tool-registry (application-tool-registry application))
             (let* ((context (mission-test--context application))
                    (tool (tool-registry-find (application-tool-registry application) "mission-review" "inspect")))
               (test-assert (tool-result-success-p (tool-execute tool context (json-object)))
                            "primary can inspect durable review state")
               (setf (slot-value context 'agent) (make-instance 'agent :configuration configuration))
               (test-assert (handler-case (progn (tool-execute tool context (json-object)) nil)
                              (mission-error (condition) (eq ':review-authority (mission-error-reason condition))))
                            "unrelated agent cannot use mission reviewer tools")))
        (mission-review-test--close application))))
  nil)
