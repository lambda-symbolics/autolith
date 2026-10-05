(in-package #:autolith)

;;;; -- Mission Boundary Fixtures --

(-> mission-test--write-file (pathname string) null)
(defun mission-test--write-file (path text)
  "Write a complete relevant-state fixture file."
  (with-open-file (stream path :direction :output :if-exists ':supersede
                              :if-does-not-exist ':create)
    (write-string text stream))
  nil)

(-> mission-test--application (configuration &key (:results list) (:worker t)) application)
(defun mission-test--application (configuration &key results worker)
  "Create a recorded application with an explicitly scripted provider."
  (let* ((conversation (conversation-create configuration))
         (provider (make-instance 'scripted-provider :results results))
         (registry (make-default-tool-registry :configuration configuration))
         (agent (make-instance 'agent :configuration configuration :conversation conversation
                              :provider provider :tool-registry registry :worker worker))
         (ui (terminal-ui-create :terminal (make-instance 'recording-terminal :columns 80))))
    (terminal-ui-start ui)
    (make-instance 'application :configuration configuration :conversation conversation
                   :provider provider :tool-registry registry :worker worker :agent agent :ui ui)))

(-> mission-test--specification (&key (:gates list) (:turns integer) (:tokens integer)) list)
(defun mission-test--specification (&key gates (turns 20) (tokens 1000))
  "Return one explicit acceptance policy with an optional executable criterion."
  (list :objective "Produce an accepted artifact" :turn-limit turns :token-limit tokens
        :wall-seconds 120 :gates gates
        :criteria (list (list :id "delivery" :description "The requested artifact is accepted"
                              :gates (mapcar (lambda (gate) (getf gate :id)) gates)))))

(-> mission-test--context (application &key (:authorization keyword)) tool-context)
(defun mission-test--context (application &key (authorization ':full-access))
  "Create the ordinary gate tool boundary with a deterministic authorization decision."
  (make-instance 'tool-context :configuration (application-configuration application)
                 :conversation (application-conversation application)
                 :registry (application-tool-registry application)
                 :worker (application-worker application) :agent (application-agent application)
                 :command-authorization-function
                 (lambda (command directory) (declare (ignore command directory)) authorization)))

(-> mission-test--result (string &key (:usage json-object)) provider-result)
(defun mission-test--result (text &key (usage (json-object "input_tokens" 2 "output_tokens" 3)))
  "Return one final provider response with exact, configurable usage."
  (agent-test-result (make-identifier) (list (agent-test-message text)) :usage usage))

(-> test-mission-accounting () null)
(defun test-mission-accounting ()
  "Exercise exact shared accounting across primary, inherited and nested inference."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application
                         configuration :results (loop repeat 4 collect (mission-test--result "done"))))
           (conversation (application-conversation application))
           (provider (application-provider application)))
      (application-mission-start application (mission-test--specification :turns 4))
      (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
      (let ((child (conversation-create configuration)))
        (mission-context-inherit conversation child)
        (provider-stream-turn provider child :tool-namespaces #() :event-callback #'identity))
      (test-assert (string= "done" (infer "one bounded inference" :provider provider :configuration configuration))
                   "direct recursive inference returns its value")
      (test-assert (getf (first (rlm-map '("nested frame") :provider provider :configuration configuration
                                       :concurrency 1)) :value)
                   "mapped frames share the mission despite different worker threads")
      (let ((goal (application-goal application)))
        (test-assert (= 4 (getf goal :turns-used)) "every provider inference uses the shared turn allowance")
        (test-assert (= 20 (getf goal :tokens-used)) "every reported billable token is charged exactly once")
        (test-assert (handler-case
                         (progn (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity) nil)
                       (mission-error (condition) (eq (mission-error-reason condition) ':exhausted)))
                     "exhaustion rejects the next inference before invoking the provider")
        (test-assert (eq ':exhausted (getf goal :status)) "exhaustion is a durable terminal transition")))))

(-> test-mission-unknown-usage-and-recovery () null)
(defun test-mission-unknown-usage-and-recovery ()
  "Keep unavailable usage unknown and recover interruption conservatively."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application
                        configuration :results (list (mission-test--result "done" :usage (json-object))))))
      (application-mission-start application (mission-test--specification))
      (provider-stream-turn (application-provider application) (application-conversation application)
                            :tool-namespaces #() :event-callback #'identity)
      (test-assert (eq ':blocked (getf (application-goal application) :status)) "unknown usage blocks admission")
      (test-assert (= 1 (getf (application-goal application) :unknown-usage)) "unknown requests have a separate durable counter")
      (test-assert (zerop (getf (application-goal application) :tokens-used)) "unknown usage is not guessed into known spend")
      (test-assert (handler-case (progn (application-mission-resume application) nil)
                     (mission-error () t)) "resume cannot pretend unknown spend is available")
      (application-mission-start application (mission-test--specification))
      (setf (getf (application-goal application) :requests-outstanding) 1)
      (application--record-goal application)
      (application--load-goal application)
      (test-assert (eq ':blocked (getf (application-goal application) :status)) "restore blocks interrupted inference")
      (test-assert (= 1 (getf (application-goal application) :unknown-usage)) "restore retains the missing usage evidence")
      (application-mission-start application (mission-test--specification))
      (application-mission-stop application ':cancelled "Operator stopped the mission")
      (application--load-goal application)
      (test-assert (eq ':cancelled (getf (application-goal application) :status)) "terminal cancellation restores")
      (test-assert (getf (application-goal application) :evidence) "terminal evidence is persisted independently of messages"))))

(-> test-mission-gate-retries () null)
(defun test-mission-gate-retries ()
  "Exercise gate order, deterministic failure reuse, invalidation and separate attempts."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((input (merge-pathnames "gate-input" root))
           (application (mission-test--application configuration))
           (calls nil)
           (gates '((:id "first" :kind :artifact :path "first" :inputs ("gate-input") :deterministic-p t)
                    (:id "second" :kind :artifact :path "second" :inputs ("gate-input")
                     :deterministic-p t :attempt-limit 2))))
      (mission-test--write-file input "first state")
      (application-mission-start application (mission-test--specification :gates gates))
      (test-call-with-function-replacements
       (list (list 'mission--tool-context (lambda (app) (mission-test--context app)))
             (list 'mission-gate-execute
                   (lambda (gate context)
                     (declare (ignore context))
                     (push (getf gate :id) calls)
                     (values (equal (getf gate :id) "first") "bounded test evidence"))))
       (lambda ()
         (application-mission-verify application)
         (test-assert (equal '("second" "first") calls) "gates execute in configured order")
         (test-assert (eq ':active (getf (application-goal application) :status)) "a failed gate permits another work turn")
         (application-mission-verify application)
         (test-assert (= 2 (length calls)) "unchanged deterministic passes and failures do not rerun")
         (application-mission-invalidate application "second" "Remote state changed")
         (application-mission-verify application)
         (test-assert (= 3 (length calls)) "gate-specific invalidation reruns only the invalidated gate")
         (test-assert (eq ':exhausted (getf (application-goal application) :status)) "separate gate attempts terminate exhausted")
         (test-assert (zerop (getf (application-goal application) :turns-used)) "gate retries do not consume model turns")))
      (application-mission-start application (mission-test--specification :gates (list (first gates))))
      (test-call-with-function-replacements
       (list (list 'mission--tool-context (lambda (app) (mission-test--context app)))
             (list 'mission-gate-execute (lambda (gate context) (declare (ignore gate context)) (values nil "failed"))))
       (lambda ()
         (application-mission-verify application)
         (mission-test--write-file input "changed state")
         (application-mission-verify application)
         (test-assert (= 2 (getf (first (getf (application-goal application) :gates)) :attempts))
                      "changed relevant file content invalidates a deterministic failure"))))))

(-> test-mission-acceptance-proof () null)
(defun test-mission-acceptance-proof ()
  "Separate model completion, gate success and explicit user acceptance."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (application-mission-start application (mission-test--specification))
      (test-call-with-function-replacements
       (list (list 'mission--tool-context (lambda (app) (mission-test--context app))))
       (lambda ()
         (application--note-goal-turn application (mission-test--result "[GOAL-COMPLETE]"))
         (test-assert (getf (application-goal application) :model-complete-p) "completion requests verification")
         (test-assert (eq ':blocked (getf (application-goal application) :status)) "unproven user criteria block verification")
         (application-mission-accept application "delivery" "Operator inspected the final artifact")
         (application-mission-resume application)
         (application-mission-verify application)
         (test-assert (eq ':verified (getf (application-goal application) :status)) "explicit acceptance proves the criterion")
         (application--load-goal application)
         (test-assert (eq ':verified (getf (application-goal application) :status)) "verified proof restores")
         (test-assert (getf (first (getf (application-goal application) :criteria)) :evidence)
                      "criterion evidence restores with mission state"))))))

(-> test-mission-execution-gates () null)
(defun test-mission-execution-gates ()
  "Run command, artifact and Lisp gates through existing sandbox and worker boundaries."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((worker (lisp-worker-pool-create configuration))
           (application (mission-test--application configuration :worker worker))
           (context (mission-test--context application)))
      (unwind-protect
           (progn
             (mission-test--write-file (merge-pathnames "accepted" root) "accepted artifact")
             (dolist (specification '((:id "command" :kind :command :command "exit 0")
                                      (:id "artifact" :kind :artifact :path "accepted")
                                      (:id "lisp" :kind :lisp :predicate "(= (+ 2 2) 4)")))
               (multiple-value-bind (passed-p evidence)
                   (mission-gate-execute (mission-gate-normalize specification) context)
                 (test-assert passed-p "the configured execution gate passes")
                 (test-assert (non-empty-string-p evidence) "each gate returns concrete evidence")))
             (test-assert (not (mission-gate-execute
                                (mission-gate-normalize '(:id "failure" :kind :command :command "exit 7")) context))
                          "a nonzero exit is not acceptance")
             (test-assert (not (mission-gate-execute
                                (mission-gate-normalize '(:id "denied" :kind :command :command "exit 0"))
                                (mission-test--context application :authorization ':deny)))
                          "command gates honor denied authorization"))
        (lisp-worker-pool-stop-all worker)))))

(-> test-mission-wall-budget () null)
(defun test-mission-wall-budget ()
  "Enforce the wall budget through cl-jobpond and preserve it on restore."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (application-mission-start application (mission-test--specification))
      (setf (getf (application-goal application) :deadline) (1+ (get-universal-time)))
      (let ((context (mission-context-find (application-conversation application))))
        (test-assert (handler-case
                         (progn (mission--supervise context "Wall budget test" (lambda () (sleep 3))) nil)
                       (mission-error (condition) (eq (mission-error-reason condition) ':exhausted)))
                     "cl-jobpond stops work at the mission deadline"))
      (test-assert (eq ':exhausted (getf (application-goal application) :status)) "deadline exhaustion is terminal")
      (application--load-goal application)
      (test-assert (eq ':exhausted (getf (application-goal application) :status)) "exhaustion survives restoration"))))


(-> test-mission-command-and-compaction () null)
(defun test-mission-command-and-compaction ()
  "Invoke user/model operations and restore terminal proof after durable compaction."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (registry (application-tool-registry application)))
      (test-assert (application-command-find "/mission") "mission command participates in operation lookup")
      (test-assert
       (eq ':continue (application-command application
                                           (format nil "/mission start ~S" (mission-test--specification))))
       "the complete mission policy is one free-form command argument")
      (test-call-with-function-replacements
       (list (list 'mission--tool-context (lambda (app) (mission-test--context app))))
       (lambda ()
         (test-assert
          (tool-result-success-p
           (tool-registry-execute-call registry
                                       (agent-test-call :namespace "mission" :name "verify")
                                       (mission-test--context application)))
          "model verification uses the registered tool boundary")
         (test-assert (eq ':blocked (getf (application-goal application) :status))
                      "the model cannot prove a human acceptance criterion")
         (test-assert (null (tool-registry-find registry "mission" "accept"))
                      "user acceptance is not model authority")
         (application-command application "/mission accept (\"delivery\" \"Inspected by the operator\")")
         (application-command application "/mission resume")
         (application-command application "/mission verify")))
      (test-assert (eq ':verified (getf (application-goal application) :status)) "user commands complete verification")
      (let* ((conversation (application-conversation application))
             (identifier (conversation-identifier conversation)))
        (conversation-append-summary conversation "Unrelated compacted transcript")
        (setf (application-conversation application) (conversation-load-by-id configuration identifier))
        (application--load-goal application)
        (test-assert (eq ':verified (getf (application-goal application) :status)) "verification survives compaction and disk replay")
        (test-assert (getf (first (getf (application-goal application) :criteria)) :evidence)
                     "terminal proof is not reconstructed from the model summary")))))

(-> test-mission-queued-child-authority () null)
(defun test-mission-queued-child-authority ()
  "Capture child authority at admission and cancel queued work without moving budgets."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (orchestrator (task-orchestrator-create))
           (definition (task-agent-definition-create
                        :name "mission-child" :description "Check immutable mission admission"
                        :instructions "Work under the admitted mission" :source ':test))
           (entry (list :definition definition :detached t
                        :item (list :name "child" :agent "mission-child" :task "Check admission")))
           (submit (symbol-function 'job-pool-submit-batch)))
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'job-pool-submit-batch
                        (lambda (pool entries)
                          (funcall submit pool
                                   (mapcar (lambda (entry) (list* :inline-only-p t entry)) entries)))))
            (lambda ()
              (application-mission-start application (mission-test--specification))
              (let* ((context (mission-context-find (application-conversation application)))
                     (job (first (task-orchestrator-start-jobs orchestrator (application-agent application) (list entry)))))
                (test-assert (eq context (task-job-mission-context job)) "queued children retain admitted mission identity")
                (mission-context-bind context (application-agent application))
                (application-mission-start application (mission-test--specification))
                (test-assert (handler-case (progn (mission-task-job-run job) nil)
                               (mission-error (condition) (eq ':replaced (mission-error-reason condition))))
                             "replaced mission authority is refused before child startup")
                (test-assert (mission-agent-terminal-p (application-agent application))
                             "the replaced primary turn stops under its admitted mission")
                (test-assert (eq context (mission--agent-context (application-agent application)))
                             "new delegated work cannot acquire replacement authority from an old turn")
                (test-assert
                 (handler-case
                     (progn
                       (provider-stream-turn (application-provider application)
                                             (application-conversation application)
                                             :tool-namespaces #() :event-callback #'identity)
                       nil)
                   (mission-error (condition) (eq ':replaced (mission-error-reason condition))))
                 "an old primary request cannot charge the replacement mission")
                (let ((inherited (configuration-copy configuration)))
                  (mission-context-inherit configuration inherited)
                  (test-assert (eq context (mission-context-find inherited))
                               "recursive work retains the engaged turn's exact mission"))
                (test-assert (zerop (getf (application-goal application) :turns-used))
                             "old primary and child work cannot charge a new mission")
                (with-lock-held (*mission-contexts-lock*)
                  (remhash (application-agent application) *mission-contexts*)))
              (tool-registry-bind-runtime (application-tool-registry application) 'task-orchestrator orchestrator)
              (let ((job (first (task-orchestrator-start-jobs orchestrator (application-agent application) (list entry)))))
                (application-mission-stop application ':cancelled "Operator cancelled queued work")
                (test-assert (job-terminal-p job) "explicit mission cancellation publishes queued child termination"))))
        (task-orchestrator-close orchestrator)))))

(defclass mission-test-native-provider (scripted-provider) ()
  (:documentation "A scripted provider reporting separate native-compaction usage."))

(defmethod provider-native-compact-conversation
    ((provider mission-test-native-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Return one compacted item and exact native inference usage."
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values (json-object "type" "compaction") (json-object "input_tokens" 2 "output_tokens" 3)))

(-> test-mission-native-compaction-budget () null)
(defun test-mission-native-compaction-budget ()
  "Include native compaction usage and retain exact reported token exhaustion."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (application-mission-start application (mission-test--specification :tokens 4))
      (multiple-value-bind (item usage)
          (provider-native-compact-conversation
           (make-instance 'mission-test-native-provider :results nil)
           (application-conversation application) :tool-namespaces #() :event-callback #'identity)
        (test-assert (and item usage) "native compaction preserves both protocol values"))
      (test-assert (= 1 (getf (application-goal application) :turns-used)) "native compaction charges an inference admission")
      (test-assert (= 5 (getf (application-goal application) :tokens-used)) "reported usage is recorded without clipping it to the limit")
      (test-assert (eq ':exhausted (getf (application-goal application) :status)) "reported token exhaustion is a durable terminal state"))))
