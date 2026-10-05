(in-package #:autolith)

;;;; -- Verified Session Goals --

(defparameter *mission-evidence-limit* 4096
  "Maximum retained characters in one mission evidence entry.")

(defparameter *mission-evidence-count-limit* 64
  "Maximum evidence entries retained in each durable mission snapshot.")

(defvar *mission-contexts* (make-hash-table :test #'eq :weakness ':key)
  "Configuration and conversation identities associated with mission contexts.")

(defvar *mission-contexts-lock* (make-lock "Mission identities")
  "Lock protecting mission context identity associations.")

(defclass mission-context ()
  ((application :initarg :application :reader mission-context-application
                :documentation "Application owning the durable goal.")
   (goal :initarg :goal :accessor mission-context-goal
         :documentation "Exact goal identity, including after replacement.")
   (lock :initform (make-recursive-lock "Mission state") :reader mission-context-lock
         :documentation "Serializes counters, transitions and persistence.")
   (request-lock :initform (make-recursive-lock "Mission requests")
                 :reader mission-context-request-lock
                 :documentation "Serializes provider admission and settlement.")
   (threads :initform (make-hash-table :test #'eq) :reader mission-context-threads
            :documentation "Thread identities for safe sibling cancellation.")
   (jobs :initform nil :accessor mission-context-jobs
         :documentation "Supervised operations currently owned by the mission."))
  (:documentation "Ephemeral synchronization for one durable session goal."))

(define-condition mission-error (autolith-error)
  ((reason :initarg :reason :reader mission-error-reason
           :documentation "Machine-readable reason for a rejected mission operation."))
  (:documentation "A mission cannot admit work or validate its acceptance policy."))

(-> mission--reject (keyword string) null)
(defun mission--reject (reason message)
  "Signal a typed mission policy failure with REASON and MESSAGE."
  (error 'mission-error :reason reason :message message))

(-> mission-goal-p (t) boolean)
(defun mission-goal-p (goal)
  "Return true when GOAL has explicit mission acceptance policy."
  (and (listp goal) (eq (getf goal :kind) ':mission)))

(-> mission--bounded-text (t) string)
(defun mission--bounded-text (value)
  "Render VALUE without retaining unbounded evidence."
  (let* ((*print-length* 64) (*print-level* 8)
         (text (if (stringp value) value (prin1-to-string value))))
    (subseq text 0 (min (length text) *mission-evidence-limit*))))

(-> mission-context-find (t) (option mission-context))
(defun mission-context-find (identity)
  "Return the mission context registered for IDENTITY."
  (with-lock-held (*mission-contexts-lock*)
    (gethash identity *mission-contexts*)))

(-> mission-context-inherit (t t) null)
(defun mission-context-inherit (parent child)
  "Associate CHILD with PARENT's exact mission, including terminal missions."
  (let* ((registered (mission-context-find parent))
         (application (and registered (mission-context-application registered)))
         (context (if (and application (eq parent (application-configuration application)))
                      (mission--conversation-context (application-conversation application))
                      registered)))
    (when (and context
               (or (not (eq parent (application-configuration (mission-context-application context))))
                   (mission--request-owned-p context (application-conversation (mission-context-application context)))))
      (mission-context-bind context child)))
  nil)

(-> mission--attach (application) (option mission-context))
(defun mission--attach (application)
  "Register synchronization for APPLICATION's current durable mission."
  (let ((goal (application-goal application)))
    (when (mission-goal-p goal)
      (let ((context (make-instance 'mission-context :application application :goal goal)))
        (with-lock-held (*mission-contexts-lock*)
          (setf (gethash (application-configuration application) *mission-contexts*) context
                (gethash (application-conversation application) *mission-contexts*) context))
        context))))

(-> mission--record (mission-context) null)
(defun mission--record (context)
  "Persist CONTEXT's exact mission without changing a replacement goal."
  (let ((application (mission-context-application context))
        (goal (mission-context-goal context)))
    (when (eq goal (application-goal application))
      (conversation-append-record (application-conversation application)
                                  (cons :goal (copy-tree goal)))))
  nil)

(-> mission--evidence (mission-context keyword t) null)
(defun mission--evidence (context kind text)
  "Append bounded durable KIND evidence TEXT while holding the mission lock."
  (let* ((goal (mission-context-goal context))
         (entries (append (getf goal :evidence)
                          (list (list :kind kind :time (get-universal-time)
                                      :text (mission--bounded-text text))))))
    (setf (getf goal :evidence)
          (last entries (min (length entries) *mission-evidence-count-limit*))))
  nil)

(-> mission--transition (mission-context keyword t) null)
(defun mission--transition (context status evidence)
  "Persist a mission STATUS with EVIDENCE and stop its admitted operations."
  (with-recursive-lock-held ((mission-context-lock context))
    (let ((goal (mission-context-goal context)))
      (setf (getf goal :status) status)
      (mission--evidence context status evidence)
      (mission--record context)))
  (when (member status '(:blocked :exhausted :cancelled :failed))
    (let ((jobs
            (with-recursive-lock-held ((mission-context-lock context))
              (remove-if (lambda (job)
                           (eq (gethash job (mission-context-threads context)) (current-thread)))
                         (copy-list (mission-context-jobs context))))))
      (dolist (job jobs) (job-cancel job :reason status))))
  nil)

(-> mission--remaining-milliseconds (mission-context) integer)
(defun mission--remaining-milliseconds (context)
  "Return the remaining durable wall-clock budget, including time away."
  (* 1000 (- (getf (mission-context-goal context) :deadline)
             (get-universal-time))))

(-> mission--admit (mission-context &key (:inference-p boolean)) null)
(defun mission--admit (context &key (inference-p t))
  "Reject terminal, replaced or exhausted missions before admitting work.
Verification may use the final model turn's evidence without another inference."
  (let* ((goal (mission-context-goal context))
         (application (mission-context-application context)))
    (unless (eq goal (application-goal application))
      (mission--reject ':replaced "The mission was replaced."))
    (unless (eq (getf goal :status) ':active)
      (mission--reject (getf goal :status) "The mission is not active."))
    (when (or (not (plusp (mission--remaining-milliseconds context)))
              (and inference-p
                   (or (>= (getf goal :turns-used) (getf goal :turn-limit))
                       (>= (getf goal :tokens-used) (getf goal :token-limit)))))
      (mission--transition context ':exhausted "Mission-wide work budget exhausted.")
      (mission--reject ':exhausted "Mission-wide work budget exhausted.")))
  nil)

(-> mission--supervise (mission-context string function &key (:timeout integer)) t)
(defun mission--supervise (context name function &key timeout)
  "Run FUNCTION inline through cl-jobpond under the mission's remaining deadline.
The calling thread preserves provider bindings and authorization callbacks."
  (let* ((milliseconds (mission--remaining-milliseconds context))
         (pool (make-job-pool :name name :maximum-concurrency 1
                              :maximum-batch-size 1 :maximum-live-jobs 1
                              :maximum-runtime-milliseconds
                              (max 1 (if timeout (min milliseconds timeout) milliseconds))
                              :terminal-retention-limit 1 :start-threads-p nil))
         (job (first (job-pool-submit-batch
                      pool (list (list :name name :inline-only-p t
                                       :function (lambda (job)
                                                   (job-check-cancellation job)
                                                   (funcall function))))))))
    (unwind-protect
         (progn
           (with-recursive-lock-held ((mission-context-lock context))
             (push job (mission-context-jobs context))
             (setf (gethash job (mission-context-threads context)) (current-thread)))
           (job-run-inline job)
           (let ((snapshot (job-await job)))
            (cond
              ((eq (getf snapshot :state) ':completed) (getf snapshot :result))
              ((eq (getf snapshot :cancellation-reason) ':timeout)
               (if (and timeout (< timeout milliseconds))
                   (mission--reject ':gate-timeout "Gate execution reached its deadline.")
                   (progn
                     (mission--transition context ':exhausted "Mission operation reached its wall-clock deadline.")
                     (mission--reject ':exhausted "Mission operation reached its deadline."))))
              (t
               (mission--reject (getf snapshot :state)
                                (or (getf snapshot :condition-report)
                                    "Mission operation did not complete."))))))
      (with-recursive-lock-held ((mission-context-lock context))
        (setf (mission-context-jobs context)
              (remove job (mission-context-jobs context)))
        (remhash job (mission-context-threads context)))
      (job-pool-close pool))))

(-> mission--account-inference (mission-context function function) t)
(defun mission--account-inference (context function usage-function)
  "Admit one inference, settle reported billable usage, and preserve multiple values."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (with-recursive-lock-held ((mission-context-lock context))
      (mission--admit context)
      (incf (getf (mission-context-goal context) :turns-used))
      (incf (getf (mission-context-goal context) :requests-outstanding))
      (mission--record context))
    (let* ((goal (mission-context-goal context))
           (*provider-maximum-output-tokens*
             (min (or *provider-maximum-output-tokens* (getf goal :token-limit))
                  (- (getf goal :token-limit) (getf goal :tokens-used))))
           (results
             (handler-case
                 (mission--supervise context "Mission inference"
                                     (lambda () (multiple-value-list (funcall function))))
               (error (condition)
                 (with-recursive-lock-held ((mission-context-lock context))
                   (decf (getf goal :requests-outstanding))
                   (incf (getf goal :unknown-usage))
                   (when (eq (getf goal :status) ':active)
                     (mission--transition context ':failed condition))
                   (mission--record context))
                 (error condition))))
           (tokens (rlm-usage-billable-tokens
                    (provider-usage-normalize (funcall usage-function results)))))
      (with-recursive-lock-held ((mission-context-lock context))
        (decf (getf goal :requests-outstanding))
        (if (integerp tokens)
            (incf (getf goal :tokens-used) tokens)
            (progn
              (incf (getf goal :unknown-usage))
              (when (eq (getf goal :status) ':active)
                (mission--transition context ':blocked
                                     "Provider omitted billable usage; the remaining token budget is unknown."))))
        (when (and (eq (getf goal :status) ':active)
                   (> (getf goal :tokens-used) (getf goal :token-limit)))
          (mission--transition context ':exhausted "Mission token budget exhausted."))
        (mission--record context))
      (values-list results))))

(-> mission--conversation-context (conversation) (option mission-context))
(defun mission--conversation-context (conversation)
  "Prefer primary work's admitted identity over a replacement conversation mission."
  (let* ((context (mission-context-find conversation))
         (application (and context (mission-context-application context)))
         (agent (and application (application-agent application))))
    (if (and agent (eq conversation (application-conversation application)))
        (or (mission-context-find agent) context)
        context)))

(defmethod provider-stream-turn :around
    ((provider model-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Charge primary, delegated and recursive streaming inference to its mission."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (let ((context (mission--conversation-context conversation)))
    (if (and context (mission--request-owned-p context conversation))
        (mission--account-inference context #'call-next-method
                                    (lambda (results) (provider-result-usage (first results))))
        (call-next-method))))

(defmethod provider-native-compact-conversation :around
    ((provider model-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Include native compaction inference and its separately reported usage."
  (declare (ignore tool-namespaces event-callback))
  (let ((context (mission--conversation-context conversation)))
    (if (and context (mission--request-owned-p context conversation))
        (mission--account-inference context #'call-next-method #'second)
        (call-next-method))))

(-> mission--validate-specification (list) list)
(defun mission--validate-specification (specification)
  "Validate user acceptance policy and return a new durable goal."
  (unless (and (non-empty-string-p (getf specification :objective))
               (consp (getf specification :criteria))
               (every (lambda (key) (typep (getf specification key) '(integer 1)))
                      '(:turn-limit :token-limit :wall-seconds)))
    (mission--reject ':invalid "A mission requires objective, criteria and positive turn-limit, token-limit and wall-seconds."))
  (let* ((gates (mapcar #'mission-gate-normalize (getf specification :gates)))
         (gate-identifiers (mapcar (lambda (gate) (getf gate :id)) gates))
         (criteria (loop for criterion in (getf specification :criteria)
                         collect (list :id (getf criterion :id)
                                       :description (getf criterion :description)
                                       :gates (copy-list (getf criterion :gates))
                                       :evidence nil)))
         (criterion-identifiers nil))
    (dolist (criterion criteria)
      (unless (and (non-empty-string-p (getf criterion :id))
                   (non-empty-string-p (getf criterion :description))
                   (listp (getf criterion :gates))
                   (every (lambda (id) (member id gate-identifiers :test #'equal))
                          (getf criterion :gates))
                   (not (member (getf criterion :id) criterion-identifiers :test #'equal)))
        (mission--reject ':invalid "Criteria require unique IDs, descriptions and existing gate IDs."))
      (push (getf criterion :id) criterion-identifiers))
    (unless (= (length gate-identifiers) (length (remove-duplicates gate-identifiers :test #'equal)))
      (mission--reject ':invalid "Gate IDs must be unique."))
    (list :kind ':mission :version 1 :objective (getf specification :objective)
          :status ':active :continuations 0 :created-at (get-universal-time)
          :deadline (+ (get-universal-time) (getf specification :wall-seconds))
          :turn-limit (getf specification :turn-limit)
          :token-limit (getf specification :token-limit)
          :turns-used 0 :tokens-used 0 :unknown-usage 0 :requests-outstanding 0
          :model-complete-p nil :criteria criteria :gates gates :evidence nil)))

(-> application-mission-start (application list) null)
(defun application-mission-start (application specification)
  "Replace the goal with an explicitly budgeted mission, without starting a turn."
  (let ((goal (mission--validate-specification specification)))
    (when (and (mission-goal-p (application-goal application))
               (member (getf (application-goal application) :status) '(:active :paused :blocked)))
      (application-mission-stop application ':cancelled "Replaced by a new mission."))
    (setf (application-goal application) goal)
    (let ((context (mission--attach application)))
      (mission--evidence context ':started "Explicit mission acceptance policy installed.")
      (mission--record context)))
  nil)

(-> application-mission-stop (application keyword string) null)
(defun application-mission-stop (application status evidence)
  "Stop a mission with explicit STATUS and EVIDENCE, cancelling admitted jobs."
  (unless (member status '(:blocked :cancelled :failed))
    (mission--reject ':invalid "Explicit stop status must be blocked, cancelled or failed."))
  (let ((context (mission-context-find (application-conversation application))))
    (unless (and context (mission-goal-p (application-goal application)))
      (mission--reject ':missing "No mission is set."))
    (unless (member (getf (application-goal application) :status) '(:active :paused :blocked))
      (mission--reject ':terminal "The mission is already terminal."))
    (mission--transition context status evidence)
    (dolist (job (copy-list (mission-context-jobs context)))
      (job-cancel job :reason status)))
  (let ((context (mission-context-find (application-conversation application)))
        (runtime (tool-registry-runtime-binding (application-tool-registry application) 'task-orchestrator)))
    (when (and (typep runtime 'task-orchestrator) (application-agent application))
      (dolist (job (task-orchestrator-list-visible-jobs runtime (application-agent application)))
        (when (and (eq context (mission-context-find job)) (not (job-terminal-p job)))
          (session-job-cancel job status)))))
  nil)

(-> application-mission-resume (application) null)
(defun application-mission-resume (application)
  "Resume a paused or blocked mission without resetting its durable budgets."
  (let* ((context (mission-context-find (application-conversation application)))
         (goal (application-goal application)))
    (unless (and context (mission-goal-p goal)
                 (member (getf goal :status) '(:paused :blocked)))
      (mission--reject ':invalid "Only paused or blocked missions can resume."))
    (when (plusp (getf goal :unknown-usage))
      (mission--reject ':unknown-usage "Unknown token usage requires a new mission budget, not resume."))
    (mission--transition context ':active "User resumed the mission without resetting budgets.")
    (mission--admit context :inference-p nil))
  nil)

(-> application-mission-context (application) (option string))
(defun application-mission-context (application)
  "Render durable mission policy and bounded evidence for each model request."
  (let ((goal (application-goal application)))
    (when (mission-goal-p goal)
      (format nil "<mission_context>~%~S~%Model completion is a verification request, not proof. Use mission.verify after work is done. Use mission.stop to report a blocker. Acceptance criteria without gates require explicit user acceptance. Do not claim verified before the harness verifies.~%</mission_context>"
              goal))))


(-> mission-restore (application) null)
(defun mission-restore (application)
  "Restore mission synchronization and conservatively recover interrupted operations."
  (let ((context (mission--attach application)))
    (when context
      (let ((goal (mission-context-goal context)))
        (dolist (gate (getf goal :gates))
          (when (eq (getf gate :status) ':running)
            (setf (getf gate :status) ':failed
                  (getf gate :fingerprint) nil
                  (getf gate :evidence) "Interrupted gate execution; no passing evidence recorded.")))
        (cond
          ((and (member (getf goal :status) '(:active :paused :blocked))
                (plusp (getf goal :requests-outstanding 0)))
           (incf (getf goal :unknown-usage) (getf goal :requests-outstanding))
           (setf (getf goal :requests-outstanding) 0)
           (mission--transition context ':blocked
                                "Interrupted inference has unknown token usage; resume requires a new budget."))
          ((and (member (getf goal :status) '(:active :paused :blocked))
                (not (plusp (mission--remaining-milliseconds context))))
           (mission--transition context ':exhausted "The durable wall-clock deadline elapsed while away."))
          (t (mission--record context))))))
  nil)

(-> mission-agent-terminal-p (agent) boolean)
(defun mission-agent-terminal-p (agent)
  "Return true when AGENT's exact mission no longer permits a model continuation."
  (let ((context (mission-context-find agent)))
    (and context
         (or (not (eq (mission-context-goal context)
                      (application-goal (mission-context-application context))))
             (not (eq (getf (mission-context-goal context) :status) ':active))))))

(defmethod tool-execution-invoke :around
    ((runtime task-orchestrator) parent
     &key tool-name description summary operation-function async-p parent-call-id)
  "Apply mission deadlines to delegated execution using the existing job lifecycle."
  (let ((context (and (typep parent 'agent) (mission--agent-context parent))))
    (if (or (null context)
            (not (mission--request-owned-p context (agent-conversation parent))))
        (call-next-method)
        (call-next-method
         runtime parent :tool-name tool-name :description description :summary summary
         :async-p async-p :parent-call-id parent-call-id
         :operation-function
         (lambda ()
           (mission--admit context :inference-p nil)
           (mission--supervise context (format nil "Mission ~A" tool-name) operation-function))))))


(-> mission--request-owned-p (mission-context conversation) boolean)
(defun mission--request-owned-p (context conversation)
  "Distinguish mission work from new interactive turns after a terminal mission."
  (or (not (eq conversation (application-conversation (mission-context-application context))))
      (eq (getf (mission-context-goal context) :status) ':active)
      (let ((agent (application-agent (mission-context-application context))))
        (and agent (eq (mission-context-find agent) context)))))

(defmethod agent-run-user-turn :around ((agent agent) content &key &allow-other-keys)
  "Retain mission engagement throughout one turn, including its tool workers."
  (declare (ignore content))
  (let ((context (mission-context-find (agent-conversation agent)))
        (previous (mission-context-find agent)))
    (when (and context (eq (getf (mission-context-goal context) :status) ':active))
      (with-lock-held (*mission-contexts-lock*)
        (setf (gethash agent *mission-contexts*) context)))
    (unwind-protect (call-next-method)
      (with-lock-held (*mission-contexts-lock*)
        (if previous
            (setf (gethash agent *mission-contexts*) previous)
            (remhash agent *mission-contexts*))))))

(-> mission-detach (application) null)
(defun mission-detach (application)
  "Cancel and detach a mission before replacing it with a lightweight goal."
  (when (mission-goal-p (application-goal application))
    (when (member (getf (application-goal application) :status) '(:active :paused :blocked))
      (application-mission-stop application ':cancelled "User cleared or replaced the mission goal."))
    (with-lock-held (*mission-contexts-lock*)
      (remhash (application-configuration application) *mission-contexts*)
      (remhash (application-conversation application) *mission-contexts*)))
  nil)


(-> mission-context-bind ((option mission-context) t) null)
(defun mission-context-bind (context identity)
  "Associate IDENTITY with exact admitted CONTEXT rather than mutable parent state."
  (when context
    (with-lock-held (*mission-contexts-lock*)
      (setf (gethash identity *mission-contexts*) context)))
  nil)

(-> mission--agent-context (agent) (option mission-context))
(defun mission--agent-context (agent)
  "Return the mission authority of AGENT's current work at admission."
  (or (mission-context-find agent)
      (let ((context (mission-context-find (agent-conversation agent))))
        (and context (mission--request-owned-p context (agent-conversation agent)) context))))

(-> mission-task-job-run (task-job) t)
(defun mission-task-job-run (job)
  "Run a child using the exact mission and absolute deadline captured at admission."
  (let ((context (task-job-mission-context job)))
    (if context
        (progn
          (mission--admit context :inference-p nil)
          (mission--supervise context "Mission child" (lambda () (task-job--run job))))
        (task-job--run job))))


(defmethod initialize-instance :after ((job session-job) &key mission-context &allow-other-keys)
  "Capture exact mission identity before a session job can enter its worker queue."
  (mission-context-bind mission-context job))
