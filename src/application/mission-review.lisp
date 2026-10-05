(in-package #:autolith)

;;;; -- Optional Independent Mission Checkpoints --

(defparameter *mission-review-output-schema*
  '(:type :object
    :properties
    (("findings" (:type :array :max-items 32
                   :items (:type :object
                           :properties
                           (("id" (:type :string))
                            ("severity" (:type :string :enum ("info" "warning" "critical")))
                            ("confidence" (:type :string :enum ("low" "medium" "high")))
                            ("summary" (:type :string))
                            ("evidence" (:type :array :items (:type :string) :max-items 16)))
                           :required ("id" "severity" "confidence" "summary" "evidence")
                           :additional-properties nil)))
     ("summary" (:type :string)))
    :required ("findings" "summary") :additional-properties nil)
  "Native reviewer result contract. Findings are advice, not acceptance evidence.")

(defvar *mission-review-conversations* (make-hash-table :test #'eq :weakness ':key)
  "Reviewer conversation identities mapped to their durable checkpoint state.")

(defvar *mission-review-lock* (make-lock "Reviewer identities")
  "Protects reviewer conversation identity associations.")

(-> mission-review-bind-checkpoint (t conversation) null)
(defun mission-review-bind-checkpoint (checkpoint conversation)
  "Bind fresh reviewer CONVERSATION to CHECKPOINT before its first inference."
  (when checkpoint
    (with-lock-held (*mission-review-lock*)
      (setf (gethash conversation *mission-review-conversations*) checkpoint)))
  nil)

(-> mission-review--account (mission-context conversation &key (:function function) (:usage-function function)) t)
(defun mission-review--account (context conversation &key function usage-function)
  "Charge reviewer inference to its separate allowance and the ordinary mission."
  (let ((checkpoint (with-lock-held (*mission-review-lock*)
                      (gethash conversation *mission-review-conversations*))))
    (if (null checkpoint)
        (mission--account-inference context function usage-function)
        (with-recursive-lock-held ((mission-context-request-lock context))
          (let* ((policy (getf checkpoint :policy))
                 (remaining (- (getf policy :token-limit) (getf policy :tokens-used))))
            (with-recursive-lock-held ((mission-context-lock context))
              (mission--admit context)
              (unless (and (plusp remaining)
                           (< (getf policy :turns-used) (getf policy :turn-limit)))
                (mission--reject ':review-budget "Independent reviewer allowance exhausted."))
              (incf (getf policy :turns-used))
              (mission--record context))
            (let* ((*provider-maximum-output-tokens*
                     (min remaining (or *provider-maximum-output-tokens* remaining)))
                   (results (multiple-value-list
                             (mission--account-inference context function usage-function)))
                   (tokens (rlm-usage-billable-tokens
                            (provider-usage-normalize (funcall usage-function results)))))
              (with-recursive-lock-held ((mission-context-lock context))
                (when tokens (incf (getf policy :tokens-used) tokens))
                (mission--record context))
              (when (or (null tokens) (> (getf policy :tokens-used) (getf policy :token-limit)))
                (mission--reject ':review-budget "Reviewer token allowance exhausted or usage unknown."))
              (values-list results)))))))

(-> mission-review--context (application) mission-context)
(defun mission-review--context (application)
  "Return APPLICATION's exact mission context or reject missing policy."
  (or (and (mission-goal-p (application-goal application))
           (mission-context-find (application-conversation application)))
      (mission--reject ':missing "No mission is set.")))

(-> application-mission-review-configure (application list) list)
(defun application-mission-review-configure (application specification)
  "Attach explicit bounded reviewer policy, retaining previous decisions.
Each ID may be installed once per mission. INPUTS name the complete relevant
file set; CONTEXT contains the independent assignment's background. Budgets are
cumulative across changed snapshots, not reset by retries or restart."
  (let* ((context (mission-review--context application))
         (id (getf specification :id))
         (phase (getf specification :phase))
         (inputs (getf specification :inputs))
         (background (getf specification :context))
         (turns (getf specification :turn-limit))
         (tokens (getf specification :token-limit))
         (runs (getf specification :run-limit)))
    (unless (and (non-empty-string-p id) (<= (length id) 128)
                 (member phase '(:before-integration :final-acceptance :after-edit-set))
                 (proper-list-p inputs) inputs (<= (length inputs) 64)
                 (every #'non-empty-string-p inputs)
                 (non-empty-string-p background) (<= (length background) 16384)
                 (typep turns '(integer 1 128)) (typep tokens '(integer 1))
                 (typep runs '(integer 1 32))
                 (proper-list-p (getf specification :models))
                 (every #'non-empty-string-p (getf specification :models)))
      (mission--reject ':invalid "Review policy requires ID, phase, explicit inputs/context and positive bounded turn, token and run allowances."))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (with-recursive-lock-held ((mission-context-lock context))
        (mission--admit context :inference-p nil)
        (let ((goal (mission-context-goal context)))
          (when (or (>= (length (getf goal :reviews)) 16)
                    (find id (getf goal :reviews) :key (lambda (policy) (getf policy :id)) :test #'equal))
            (mission--reject ':invalid "Review IDs must be unique; at most sixteen policies are allowed."))
          (let ((policy (list :id id :phase phase :inputs (copy-list inputs)
                              :context background :models (copy-list (getf specification :models))
                              :turn-limit turns :token-limit tokens :run-limit runs
                              :turns-used 0 :tokens-used 0 :runs nil :current-fingerprint nil)))
            ;; Preserve goal identity when attaching policy to a pre-review snapshot.
            (unless (member :reviews goal)
              (nconc goal (list :reviews nil)))
            (setf (getf goal :reviews) (append (getf goal :reviews) (list policy)))
            (mission--evidence context ':review-configured id)
            (mission--record context)
            (copy-tree policy)))))))

(-> mission-review--policy (mission-context string) list)
(defun mission-review--policy (context id)
  "Find one attached review policy by ID."
  (or (find id (getf (mission-context-goal context) :reviews)
            :key (lambda (policy) (getf policy :id)) :test #'equal)
      (mission--reject ':missing "Unknown review checkpoint ID.")))

(-> mission-review--snapshot (list tool-context) (values list string))
(defun mission-review--snapshot (policy context)
  "Read bounded independent context with stable digests of declared inputs."
  (let ((fingerprint nil) (characters 0))
    (let ((background
            (with-output-to-string (stream)
              (write-string (getf policy :context) stream)
              (dolist (input (getf policy :inputs))
                (let* ((path (workspace-tool-path context input))
                       (before (mission--file-digest path)))
                  (format stream "~2%Input ~A:~%" input)
                  (if (probe-file path)
                      (with-open-file (file path :external-format ':utf-8)
                        (loop for character = (read-char file nil nil)
                              while character
                              do (when (> (incf characters) 32768)
                                   (mission--reject ':review-context "Reviewer inputs exceed the context bound."))
                                 (write-char character stream)))
                      (write-string "[missing]" stream))
                  (unless (equal before (mission--file-digest path))
                    (mission--reject ':review-context "Reviewer input changed while reading its snapshot."))
                  (push (list input before) fingerprint))))))
      (values (nreverse fingerprint) background))))

(-> mission-review--definition (list) task-agent-definition)
(defun mission-review--definition (policy)
  "Return a read-only programmatic reviewer using the native task contract."
  (make-instance 'task-agent-definition
                 :name "reviewer" :source ':programmatic
                 :description "Independent mission checkpoint reviewer"
                 :instructions "Review only the supplied independent context. Identify concrete findings with stable unique IDs, severity, confidence and evidence. Findings are advice. Do not claim harness acceptance. Submit your native contracted data through yield.submit."
                 :tools nil :spawns nil :models (getf policy :models)
                 :blocking-p t
                 :output (task-output-schema-normalize *mission-review-output-schema*)))

(-> mission-review--run-task (mission-context list &key (:run list) (:background string) (:tool-context tool-context)) list)
(defun mission-review--run-task (context policy &key run background tool-context)
  "Invoke one actual contracted task under existing delegation and permissions."
  (let* ((parent (tool-context-agent tool-context))
         (runtime (tool-context-execution-runtime tool-context))
         (definition (mission-review--definition policy))
         (item (list :name (format nil "review-~A" (getf policy :id))
                     :agent "reviewer" :blocking t :context background
                     :task "Perform the configured independent review. Return findings and summary through the required native output contract.")))
    (unless (and (typep parent 'agent) (not (typep parent 'task-child-agent))
                 (typep runtime 'task-orchestrator))
      (mission--reject ':review-authority "Only the primary agent may request mission review."))
    (let* ((entries (task--resolve-items parent runtime (list definition)
                                       :items (list item) :registry (tool-context-registry tool-context)))
           (entry (first entries)) (jobs nil) (finished-p nil))
      (setf (first entries)
            (list* :independent-context-p t
                   :review-checkpoint (list :policy policy :run run) entry))
      (unwind-protect
           (progn
             (multiple-value-bind (admitted inline)
                 (task-orchestrator-start-jobs
                  runtime parent entries :parent-call-id (tool-context-call-id tool-context)
                  :command-authorization-function (tool-context-command-authorization-function tool-context)
                  :tool-authorization-function (tool-context-tool-authorization-function tool-context))
              (setf jobs admitted)
              (with-recursive-lock-held ((mission-context-lock context))
                (setf (getf run :job-id) (job-identifier (first jobs)))
                (mission--record context))
               (dolist (job inline) (job-run-inline job)))
             (task-job-await (first jobs) nil)
             (setf finished-p t)
             ;; Use the complete durable artifact, not its bounded preview.
             (let* ((result (getf (task-job-snapshot (first jobs)) :result))
                    (path (getf result :output-path)))
               (if path
                   (with-open-file (stream path :external-format ':utf-8)
                     (let ((*read-eval* nil)) (read stream)))
                   result)))
        (unless finished-p (task-run--cancel-synchronous-jobs jobs))))))

(-> mission-review--findings (list) list)
(defun mission-review--findings (result)
  "Validate complete native task output and retain identified advisory findings."
  (when (member (getf result :status) '(:aborted :cancelled))
    (mission--reject ':review-cancelled "Reviewer was cancelled before completing its contract."))
  (unless (and (eq (getf result :status) ':success)
               (getf result :structured-output-present-p))
    (mission--reject ':review-failed "Reviewer did not submit a successful structured result."))
  (let* ((data (task-sexp->json (getf result :structured-output)))
         (ids nil))
    (unless (task-output-schema-valid-p data
                                      (task-output-schema-normalize *mission-review-output-schema*))
      (mission--reject ':review-failed "Reviewer output violates its native contract."))
    (loop for finding across (gethash "findings" data)
          for id = (gethash "id" finding)
          collect
          (progn
            (unless (and (non-empty-string-p id) (<= (length id) 128)
                         (not (member id ids :test #'equal)))
              (mission--reject ':review-failed "Reviewer finding IDs must be unique nonempty bounded strings."))
            (push id ids)
            (list :id id :severity (gethash "severity" finding)
                  :confidence (gethash "confidence" finding)
                  :summary (mission--bounded-text (gethash "summary" finding))
                  :evidence (map 'list #'mission--bounded-text (gethash "evidence" finding))
                  :decision ':pending :reason nil :decisions nil :decided-at nil)))))

(defvar *mission-review-live-runs* (make-hash-table :test #'eq :weakness ':key)
  "Ephemeral run identities distinguishing live work from interrupted snapshots.")

(-> application-mission-review-trigger (application string &key (:tool-context (option tool-context))) list)
(defun application-mission-review-trigger (application id &key tool-context)
  "Run changed ID once. Unchanged completed, failed and interrupted runs are reused.
Every finding needs an explicit primary resolve/reject decision before acceptance."
  (let* ((context (mission-review--context application))
         (boundary (or tool-context (mission--tool-context application)))
         (policy (mission-review--policy context id)))
    (unless (eq (tool-context-agent boundary) (application-agent application))
      (mission--reject ':review-authority "Only the primary agent may trigger a checkpoint."))
    (multiple-value-bind (fingerprint background) (mission-review--snapshot policy boundary)
      (let ((run nil))
        ;; Final acceptance holds this request lock through its state transition.
        ;; Publish review applicability under the same lock, then release it before delegation.
        (with-recursive-lock-held ((mission-context-request-lock context))
          (with-recursive-lock-held ((mission-context-lock context))
            (mission--admit context :inference-p nil)
            (let ((existing (find fingerprint (getf policy :runs)
                                  :key (lambda (entry) (getf entry :fingerprint)) :test #'equal)))
              (when existing
                (setf (getf policy :current-fingerprint) fingerprint)
                (when (and (eq (getf existing :status) ':running)
                           (not (with-lock-held (*mission-review-lock*)
                                  (gethash existing *mission-review-live-runs*))))
                  (setf (getf existing :status) ':interrupted))
                (mission--record context)
                (return-from application-mission-review-trigger (copy-tree existing))))
            (when (>= (length (getf policy :runs)) (getf policy :run-limit))
              (mission--reject ':review-budget "Reviewer checkpoint run allowance exhausted."))
            (setf run (list :fingerprint fingerprint :status ':running :created-at (get-universal-time)
                            :findings nil :job-id nil :error nil :summary nil)
                  (getf policy :runs) (append (getf policy :runs) (list run))
                  (getf policy :current-fingerprint) fingerprint)
            (with-lock-held (*mission-review-lock*)
              (setf (gethash run *mission-review-live-runs*) t))
            (mission--record context)))
        ;; Never hold the request/state lock while awaiting the delegated worker.
        (unwind-protect
             (handler-case
               (let* ((result (mission-review--run-task context policy
                                                      :run run :background background :tool-context boundary))
                      (findings (mission-review--findings result)))
                 (with-recursive-lock-held ((mission-context-lock context))
                   (setf (getf run :findings) findings
                         (getf run :status) ':completed
                         (getf run :summary)
                         (mission--bounded-text (gethash "summary" (task-sexp->json (getf result :structured-output)))))
                   (mission--evidence context ':review (list id findings))))
               (error (condition)
                 (with-recursive-lock-held ((mission-context-lock context))
                   (setf (getf run :status)
                         (if (or (eq (getf (mission-context-goal context) :status) ':cancelled)
                                 (and (typep condition 'mission-error)
                                      (eq (mission-error-reason condition) ':review-cancelled)))
                             ':cancelled ':failed)
                         (getf run :error) (mission--bounded-text (princ-to-string condition)))
                   (mission--evidence context ':review-failed (list id condition)))))
          (with-recursive-lock-held ((mission-context-lock context))
            (with-lock-held (*mission-review-lock*)
              (remhash run *mission-review-live-runs*))
            (when (eq (getf run :status) ':running) (setf (getf run :status) ':cancelled))
            (mission--record context)))
        (copy-tree run)))))

(-> application-mission-review-decide (application string string &key (:decision keyword) (:reason string) (:agent t)) list)
(defun application-mission-review-decide (application checkpoint-id finding-id &key decision reason agent)
  "Retain a primary resolve/reject decision for the latest checkpoint's finding."
  (unless (and (member decision '(:resolve :reject)) (non-empty-string-p reason)
               (or (null agent) (eq agent (application-agent application))))
    (mission--reject ':review-authority "Review decisions require the primary agent or local user and an explicit resolve/reject reason."))
  (let* ((context (mission-review--context application))
         (policy (mission-review--policy context checkpoint-id)))
    (with-recursive-lock-held ((mission-context-lock context))
      (let* ((run (find (getf policy :current-fingerprint) (getf policy :runs)
                        :key (lambda (entry) (getf entry :fingerprint)) :test #'equal))
             (finding (find finding-id (getf run :findings)
                            :key (lambda (entry) (getf entry :id)) :test #'equal)))
        (unless (and (eq (getf run :status) ':completed) finding)
          (mission--reject ':missing "No completed review finding has this ID."))
        (when (>= (length (getf finding :decisions)) 64)
          (mission--reject ':review-budget "Finding decision history is full."))
        (setf (getf finding :decisions)
              (append (getf finding :decisions)
                      (list (list :decision decision :reason (mission--bounded-text reason)
                                  :time (get-universal-time)))))
        (setf (getf finding :decision) decision
              (getf finding :reason) (mission--bounded-text reason)
              (getf finding :decided-at) (get-universal-time))
        (mission--evidence context ':review-decision (list checkpoint-id finding-id decision reason))
        (mission--record context)
        (copy-tree finding)))))

(-> application-mission-review-checkpoint (application keyword) boolean)
(defun application-mission-review-checkpoint (application phase)
  "Run only configured PHASE reviews. Return true when all advice is adjudicated.
Pending findings, failures and interrupted/cancelled reviewers prevent acceptance
without claiming failed harness checks. No configured review means no model call."
  (let ((context (mission-review--context application)))
    (dolist (policy (getf (mission-context-goal context) :reviews)
                    (application-mission-review-current-p application phase))
      (when (eq (getf policy :phase) phase)
        (let ((run (application-mission-review-trigger application (getf policy :id))))
          (unless (and (eq (getf run :status) ':completed)
                       (every (lambda (finding) (member (getf finding :decision) '(:resolve :reject)))
                              (getf run :findings)))
            (mission--evidence context ':review-pending (list (getf policy :id) (getf run :status)))
            (mission--record context)
            (return nil)))))))


(-> application-mission-review-current-p (application keyword) boolean)
(defun application-mission-review-current-p (application phase)
  "Require completed adjudicated reviews for current inputs without another model call.
Final acceptance retains the request lock from this check through its transition;
policy admission and fingerprint selection use the same request-before-state order."
  (let* ((context (mission-review--context application))
         (boundary (mission--tool-context application)))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (with-recursive-lock-held ((mission-context-lock context))
        (every
         (lambda (policy)
           (or (not (eq (getf policy :phase) phase))
               (let* ((fingerprint
                        (loop for input in (getf policy :inputs)
                              collect (list input (mission--file-digest
                                                   (workspace-tool-path boundary input)))))
                      (run (find fingerprint (getf policy :runs)
                                 :key (lambda (entry) (getf entry :fingerprint))
                                 :test #'equal)))
                 (and (equal fingerprint (getf policy :current-fingerprint))
                      run
                      (eq (getf run :status) ':completed)
                      (every (lambda (finding)
                               (not (null (member (getf finding :decision) '(:resolve :reject)))))
                             (getf run :findings))))))
         (getf (mission-context-goal context) :reviews))))))
