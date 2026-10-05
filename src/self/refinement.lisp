(in-package #:autolith)

;;;; -- Refinement Lifecycle --

(defparameter *refinement-text-limit* 16384
  "Maximum characters in one portable refinement evidence or operation field.")

(define-condition refinement-error (error)
  ((code :initarg :code :reader refinement-error-code
         :documentation "Machine-readable reason for rejecting a transition.")
   (message :initarg :message :reader refinement-error-message
            :documentation "The corrective action or rejected invariant."))
  (:report (lambda (condition stream)
             (write-string (refinement-error-message condition) stream)))
  (:documentation "An invalid, stale, unauthorized or interrupted refinement operation."))

(-> refinement--reject (keyword string) nil)
(defun refinement--reject (code message)
  "Signal a structured refinement failure with corrective MESSAGE."
  (error 'refinement-error :code code :message message))

(-> refinement--text (t) string)
(defun refinement--text (value)
  "Validate a bounded, nonblank portable text field without truncation."
  (unless (and (stringp value) (<= (length value) *refinement-text-limit*)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (refinement--reject ':invalid "Expected nonblank text within the refinement field limit."))
  value)

(-> refinement--workspace (configuration) string)
(defun refinement--workspace (configuration)
  "Return the workspace identity used to confine durable refinement records."
  (namestring (config ':working-directory configuration)))

(-> refinement-record-p (t) boolean)
(defun refinement-record-p (record)
  "Recognize complete portable refinement snapshots from the append-only journal."
  (and (listp record) (eq (first record) :refinement)
       (handler-case
           (let ((properties (rest record)))
             (and (every (lambda (key) (non-empty-string-p (getf properties key)))
                         '(:id :workspace :owner :reason))
                  (typep (getf properties :revision) '(integer 1 *))
                  (member (getf properties :target)
                          '(:none :memory :repository-fact :context :skill :role
                            :rlm-policy :live-change))
                  (member (getf properties :scope) '(:session :project :global))
                  (member (getf properties :state)
                          '(:no-op :proposed :applying :experiment :evaluating :observed
                            :passed :failed :promoting :promoted :blocked))
                  (consp (getf properties :evidence))
                  (every #'non-empty-string-p (getf properties :evidence))
                  (or (eq (getf properties :target) :none)
                      (and (non-empty-string-p (getf properties :benefit))
                           (non-empty-string-p (getf properties :criterion))))
                  t))
         (error () nil))))

(-> refinement--transition-p ((option list) list) boolean)
(defun refinement--transition-p (previous current)
  "Validate immutable evidence, lifecycle progression and scoped journal replay."
  (let ((state (getf current :state)) (scope (getf current :scope)))
    (and
     (if previous
         (and
          (every (lambda (key) (equal (getf previous key) (getf current key)))
                 '(:id :workspace :owner :target :evidence :reason :benefit :criterion))
          (member state
                  (case (getf previous :state)
                    (:proposed '(:applying))
                    (:applying '(:experiment :failed :blocked))
                    (:experiment '(:applying :evaluating))
                    (:evaluating '(:observed :failed :blocked))
                    (:observed '(:passed :failed :applying :evaluating))
                    (:passed '(:applying :evaluating :promoting))
                    (:failed '(:applying :evaluating))
                    (:promoting '(:promoted :failed :blocked))
                    (:promoted '(:applying :evaluating))
                    (:blocked '(:failed))))
          (or (eq scope (getf previous :scope))
              (and (eq (getf previous :state) :promoting) (eq state :promoted)
                   (eq scope (getf previous :destination))
                   (eq scope (case (getf previous :scope) (:session ':project) (:project ':global))))))
         (and (eq scope :session)
              (eq state (if (eq (getf current :target) :none) ':no-op ':proposed))))
     (or (not (eq state :passed))
         (let ((evaluation (getf current :task-evaluation)))
           (and (eq (getf evaluation :verdict) :passed)
                (eq (getf evaluation :basis) :trusted-reviewer)
                (eq (getf evaluation :scope) scope)
                (equal (getf evaluation :criterion) (getf current :criterion))
                (eql (getf evaluation :observation-revision) (1- (getf current :revision))))))
     t)))

(-> refinement-list (configuration) list)
(defun refinement-list (configuration)
  "Replay workspace refinement snapshots, rejecting corrupt or discontinuous records.
The existing journal reader tolerates an interrupted final form. Returned snapshots
are fresh portable data; they are not active definitions or verified model claims."
  (let ((records (make-hash-table :test #'equal))
        (order nil)
        (workspace (refinement--workspace configuration)))
    (dolist (record (mutation-journal-read-records configuration))
      (when (and (consp record) (eq (first record) :refinement))
        (unless (refinement-record-p record)
          (refinement--reject ':corrupt "Invalid refinement journal record; inspect the journal before continuing."))
        (let* ((properties (rest record))
               (identifier (getf properties :id))
               (previous (gethash identifier records)))
          (when (equal workspace (getf properties :workspace))
            (unless (= (getf properties :revision)
                       (if previous (1+ (getf previous :revision)) 1))
              (refinement--reject ':corrupt "Discontinuous refinement revision; inspect the journal before continuing."))
          (unless (refinement--transition-p previous properties)
            (refinement--reject ':corrupt "Invalid refinement lifecycle, immutable evidence or scope transition."))
            (unless previous (push identifier order))
            (setf (gethash identifier records) (copy-tree properties))))))
    (mapcar (lambda (identifier) (gethash identifier records)) (nreverse order))))

(-> refinement-find (configuration string) list)
(defun refinement-find (configuration identifier)
  "Return the current workspace proposal IDENTIFIER or signal MISSING."
  (or (find identifier (refinement-list configuration)
            :key (lambda (record) (getf record :id)) :test #'equal)
      (refinement--reject ':missing "No refinement with this identifier exists in this workspace.")))

(-> refinement--append (configuration list) list)
(defun refinement--append (configuration proposal)
  "Append a snapshot and return the same canonical data as journal replay."
  (let ((properties (copy-tree proposal)))
    (remf properties :time)
    (copy-tree (rest (mutation-journal-append configuration (cons :refinement properties))))))

(-> refinement-propose
    (configuration &key (:owner string) (:target keyword) (:evidence list)
     (:reason string) (:benefit (option string)) (:criterion (option string))) list)
(defun refinement-propose (configuration &key owner target evidence reason benefit criterion)
  "Record a trajectory-derived proposal or an ordinary NONE no-op.
REASON explains selection of the smallest useful target, including why a less
invasive adaptation is insufficient. EVIDENCE contains trajectory references or
observations, not instructions. New proposals always begin at session scope."
  (with-live-mutation
    (refinement--text owner)
    (refinement--text reason)
    (unless (and (consp evidence) (<= (length evidence) 16))
      (refinement--reject ':invalid "Supply one to sixteen trajectory evidence fields."))
    (mapc #'refinement--text evidence)
    (unless (member target '(:none :memory :repository-fact :context :skill :role
                            :rlm-policy :live-change))
      (refinement--reject ':invalid "Unknown refinement adaptation target."))
    (unless (eq target :none)
      (refinement--text benefit)
      (refinement--text criterion))
    (refinement--append
     configuration
     (list :id (make-identifier) :revision 1
           :workspace (refinement--workspace configuration) :owner owner
           :target target :scope ':session
           :state (if (eq target :none) ':no-op ':proposed)
           :evidence (copy-list evidence) :reason reason :benefit benefit
           :criterion criterion :task-evaluation nil :mechanical-checks nil))))

(-> refinement--current (tool-context string integer) list)
(defun refinement--current (context identifier revision)
  "Check revision and child authority before a lifecycle transition."
  (when (resource-context-child-agent-p context)
    (refinement--reject ':authority "Refinement transitions belong to the primary agent."))
  (let ((proposal (refinement-find (tool-context-configuration context) identifier)))
    (unless (and (integerp revision) (= revision (getf proposal :revision)))
      (refinement--reject ':stale "Reread the refinement and use its current revision."))
    (when (and (eq (getf proposal :scope) :session)
               (not (equal (getf proposal :owner)
                           (conversation-identifier (tool-context-conversation context)))))
      (refinement--reject ':authority "A session experiment belongs to its originating conversation."))
    proposal))

(-> refinement--save (tool-context list keyword) list)
(defun refinement--save (context proposal state)
  "Append a validated next revision without changing the caller on write failure."
  (let ((previous (refinement--current context (getf proposal :id) (getf proposal :revision)))
        (next (copy-tree proposal)))
    (setf (getf next :revision) (1+ (getf next :revision))
          (getf next :state) state)
    (unless (refinement--transition-p previous next)
      (refinement--reject ':invalid "Invalid refinement state transition."))
    (refinement--append (tool-context-configuration context) next)))

(-> refinement--authorize (tool-context list string t) null)
(defun refinement--authorize (context proposal action detail)
  "Require the existing user command authority for an explicit scoped decision.
Include the criterion and complete decision detail in the approval request.
Sandbox permission does not authorize evaluation acceptance or promotion."
  (unless (eq (tool-context-authorize-command
               context
               (format nil "refinement ~A ~A revision ~D scope ~A criterion ~S detail ~S"
                       action (getf proposal :id) (getf proposal :revision)
                       (getf proposal :scope) (getf proposal :criterion) detail)
               (config ':working-directory (tool-context-configuration context)))
              :full-access)
    (refinement--reject ':authority "Explicit user authority is required for this refinement decision."))
  (refinement--current context (getf proposal :id) (getf proposal :revision))
  nil)

(-> refinement--operation (string) json-object)
(defun refinement--operation (source)
  "Validate one existing owner-tool call, without executing or evaluating source."
  (refinement--text source)
  (let ((operation (handler-case (json-decode source)
                     (error () (refinement--reject ':invalid "The owner operation must be JSON.")))))
    (unless (and (json-object-p operation)
                 (non-empty-string-p (json-get operation "namespace"))
                 (non-empty-string-p (json-get operation "name"))
                 (stringp (json-get operation "arguments")))
      (refinement--reject ':invalid "Expected namespace, name and JSON-encoded arguments in the owner operation."))
    operation))

(-> refinement--owner-p (keyword json-object boolean) boolean)
(defun refinement--owner-p (target operation evaluation-p)
  "Limit delegation to existing owners; refinement never gains a new capability."
  (let ((namespace (json-get operation "namespace"))
        (name (json-get operation "name")))
    (and
     (if evaluation-p
         (member (cons namespace name)
                 '(("shell" . "run") ("lisp" . "eval") ("lisp" . "run-tests")
                   ("self" . "exercise") ("resource" . "read")) :test #'equal)
         (case target
           ((:memory :repository-fact) (and (equal namespace "resource") (equal name "edit")))
           ((:context :role :live-change)
            (or (and (equal namespace "resource") (equal name "edit"))
                (and (equal namespace "self")
                     (member name '("define" "redefine" "set" "eval" "commit"
                                    "persist-definition" "discard" "experiment-start"
                                    "experiment-settle") :test #'equal))))
           (:skill (or (and (equal namespace "skill")
                           (member name '("load" "edit") :test #'equal))
                       (and (equal namespace "resource") (equal name "edit"))))
           (:rlm-policy (or (and (equal namespace "rlm") (equal name "distill"))
                            (and (equal namespace "self")
                                 (member name '("define" "redefine" "commit" "discard") :test #'equal))))))
     t)))

(-> refinement--scope-check (json-object keyword) null)
(defun refinement--scope-check (operation scope)
  "Reserve known globally durable owners for the global promotion stage."
  (let* ((namespace (json-get operation "namespace"))
         (name (json-get operation "name"))
         (arguments (handler-case (json-decode (json-get operation "arguments"))
                      (error () (refinement--reject ':invalid "Owner arguments must be a JSON object."))))
         (uri (and (json-object-p arguments) (json-get arguments "uri"))))
    (unless (json-object-p arguments)
      (refinement--reject ':invalid "Owner arguments must be a JSON object."))
    (when (equal (json-encode (json-get arguments "async")) "true")
      (refinement--reject ':invalid "Refinement requires completed owner operations, not explicit asynchronous handoffs."))
    (when (and (not (eq scope :global))
               (or (and (equal namespace "self")
                        (member name '("commit" "persist-definition") :test #'equal))
                   (and (equal namespace "skill") (equal name "edit"))
                   (and (equal namespace "resource") (equal name "edit")
                        (stringp uri) (uiop:string-prefix-p "memory:global" uri))))
      (refinement--reject ':scope "Global persistence requires the global promotion stage."))
    (when (and (eq scope :session) (equal namespace "resource") (equal name "edit")
               (stringp uri) (uiop:string-prefix-p "memory:workspace" uri))
      (refinement--reject ':scope "Use a session draft before promoting into workspace memory.")))
  nil)

(-> refinement--delegate (tool-context list string boolean) tool-result)
(defun refinement--delegate (context proposal source evaluation-p)
  "Invoke the existing registry with the original context and owner authority."
  (let ((operation (refinement--operation source))
        (registry (tool-context-registry context)))
    (unless (refinement--owner-p (getf proposal :target) operation evaluation-p)
      (refinement--reject ':invalid "This operation is not an existing owner for the selected adaptation target."))
    (unless registry
      (refinement--reject ':missing "Supply the current tool registry to delegate an owner operation."))
    (tool-registry-execute-call registry operation context)))

(-> refinement--guard-read (tool-context string) string)
(defun refinement--guard-read (context source)
  "Capture actual read-only owner state, never a model-supplied fingerprint."
  (let* ((operation (refinement--operation source))
         (registry (tool-context-registry context)))
    (unless (and registry
                 (member (cons (json-get operation "namespace") (json-get operation "name"))
                         '(("resource" . "read") ("self" . "diff") ("self" . "status")
                           ("task" . "agents")) :test #'equal))
      (refinement--reject ':invalid "The adaptation guard must inspect read-only owner state."))
    (let ((result (tool-registry-execute-call registry operation context)))
      (unless (tool-result-success-p result)
        (refinement--reject ':guard "Owner-state inspection failed; inspect the adaptation before continuing."))
      (refinement--text (tool-result-content result)))))

(-> refinement--guard-check (tool-context list) null)
(defun refinement--guard-check (context proposal)
  "Reject task assessment or promotion when the inspected adaptation changed."
  (unless (and (getf proposal :binding)
               (equal (getf proposal :binding)
                      (refinement--guard-read context (getf proposal :guard))))
    (refinement--reject ':stale "The adaptation changed after observation; start a fresh experiment and evaluation."))
  nil)

(-> refinement-run
    (tool-context &key (:identifier string) (:revision integer) (:operation string)
     (:phase keyword) (:scope (option keyword)) (:guard (option string))) list)
(defun refinement-run (context &key identifier revision operation phase scope guard)
  "Execute an authorized experiment, observation, or next-scope promotion.
An owner-tool success records execution only. Task efficacy requires separate
user-reviewed assessment. Pending/interrupted owner effects require inspection
and recovery rather than automatic retries or promotion. Mechanical checks remain
with the owner, especially self.commit's clean-process replay checks."
  (with-live-mutation
    (let* ((proposal (refinement--current context identifier revision))
           (current-scope (getf proposal :scope))
           (state (getf proposal :state))
           (evaluation-p (eq phase :evaluate))
           (next-scope (case current-scope (:session ':project) (:project ':global))))
      (case phase
        (:experiment
         (unless (member state '(:proposed :failed :experiment :observed :passed :promoted))
           (refinement--reject ':invalid "Start an experiment from a settled proposal, not an interrupted operation.")))
        (:evaluate
         (unless (and (getf proposal :binding)
                      (member state '(:experiment :observed :passed :failed :promoted)))
           (refinement--reject ':invalid "Run task evaluation only after an applied experiment or promotion.")))
        (:promote
         (unless (and (eq state :passed) next-scope (eq scope next-scope)
                      (eq (getf (getf proposal :task-evaluation) :verdict) :passed)
                      (eq (getf (getf proposal :task-evaluation) :scope) current-scope)
                      (= (getf (getf proposal :task-evaluation) :observation-revision -1)
                         (1- revision)))
           (refinement--reject ':invalid "Promotion requires current passed task evaluation and exactly the next scope.")))
        (otherwise (refinement--reject ':invalid "Unknown refinement execution phase.")))
      ;; Validate before recording intent; no-op and unsupported owners cannot run.
      (unless (refinement--owner-p (getf proposal :target)
                                  (refinement--operation operation) evaluation-p)
        (refinement--reject ':invalid "Invalid owner operation for this refinement."))
      (refinement--scope-check (refinement--operation operation)
                               (if (eq phase :promote) scope current-scope))
      (when (eq phase :experiment)
        (refinement--guard-read context (or guard "")))
      (unless (eq phase :experiment) (refinement--guard-check context proposal))
      (refinement--authorize context proposal (string-downcase (symbol-name phase))
                             (list :destination scope :operation operation :guard guard))
      (unless (eq phase :experiment) (refinement--guard-check context proposal))
      (setf (getf proposal :task-evaluation) nil
            (getf proposal :pending-job) nil
            (getf proposal :operation) operation
            (getf proposal :destination) scope)
      (setf proposal (refinement--save context proposal
                                      (case phase (:experiment ':applying)
                                            (:evaluate ':evaluating) (:promote ':promoting))))
      (handler-case
          (let* ((result (refinement--delegate context proposal operation evaluation-p))
                 (success-p (tool-result-success-p result))
                 (details (tool-result-details result)))
            (setf (getf proposal :observation)
                  (subseq (tool-result-content result) 0
                          (min *refinement-text-limit* (length (tool-result-content result))))
                  (getf proposal :owner-success-p) success-p)
            ;; A job handoff is not successful completion of the adaptation.
            (when (and (listp details) (member (getf details :kind) '(:job :execution-job)))
              (setf success-p nil (getf proposal :owner-success-p) nil
                    (getf proposal :pending-job) (getf details :id)))
            (when (and success-p evaluation-p)
              (refinement--guard-check context proposal))
            (when (and success-p (not evaluation-p))
              (setf (getf proposal :guard) (or guard (getf proposal :guard))
                    (getf proposal :binding)
                    (refinement--guard-read context (getf proposal :guard))))
            (when (and success-p (eq phase :promote))
              (setf (getf proposal :scope) scope))
            (when (and success-p (not evaluation-p))
              (push (list :operation operation :observation (getf proposal :observation)
                          :claim ':owner-execution-only)
                    (getf proposal :mechanical-checks)))
            (refinement--save context proposal
                              (cond ((getf proposal :pending-job) ':blocked)
                                    ((not success-p) ':failed)
                                    (evaluation-p ':observed)
                                    ((eq phase :promote) ':promoted)
                                    (t ':experiment))))
        (error (condition)
          (setf (getf proposal :observation) (princ-to-string condition))
          (refinement--save context proposal ':blocked)
          (error condition))))))

(-> refinement-assess
    (tool-context &key (:identifier string) (:revision integer) (:reviewer function)) list)
(defun refinement-assess (context &key identifier revision reviewer)
  "Apply a trusted local REVIEWER to actual task observations.
REVIEWER receives a detached proposal and returns verdict PASSED or FAILED and
concrete evidence. It is supplied by a local user command or trusted harness,
never by model-facing JSON. Owner execution success and mutation replay alone
cannot establish task efficacy. No refinement tool exposes this acceptance API."
  (with-live-mutation
    (let ((proposal (refinement--current context identifier revision)))
      (unless (eq (getf proposal :state) :observed)
        (refinement--reject ':invalid "Assess only recorded task observations."))
      (unless (functionp reviewer)
        (refinement--reject ':authority "A trusted local reviewer is required; model verdicts are not evaluations."))
      (refinement--guard-check context proposal)
      (multiple-value-bind (verdict evidence) (funcall reviewer (copy-tree proposal))
        (unless (member verdict '(:passed :failed))
          (refinement--reject ':invalid "A trusted reviewer must return passed or failed with task evidence."))
        (refinement--text evidence)
        ;; The reviewer may perform work. Revalidate before recording its verdict.
        (refinement--current context identifier revision)
        (refinement--guard-check context proposal)
        (setf (getf proposal :task-evaluation)
              (list :verdict verdict :evidence evidence :basis ':trusted-reviewer
                    :criterion (getf proposal :criterion) :scope (getf proposal :scope)
                    :observation-revision revision))
        (refinement--save context proposal verdict)))))

(-> refinement-recover
    (tool-context &key (:identifier string) (:revision integer) (:evidence string)) list)
(defun refinement-recover (context &key identifier revision evidence)
  "Record inspection of an interrupted effect, leaving it failed and unevaluated.
Use the original owner's inspect/discard/recovery operation first. Recovery does
not retry effects, undo them, or preserve a previous efficacy verdict."
  (with-live-mutation
    (let ((proposal (refinement--current context identifier revision)))
      (unless (member (getf proposal :state) '(:applying :evaluating :promoting :blocked))
        (refinement--reject ':invalid "Only interrupted or blocked refinements need recovery."))
      (refinement--text evidence)
      (refinement--authorize context proposal "recover" evidence)
      (setf (getf proposal :task-evaluation) nil
            (getf proposal :recovery-evidence) evidence)
      (refinement--save context proposal ':failed))))
