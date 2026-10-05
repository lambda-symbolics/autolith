(in-package #:autolith)

;;;; -- Mission Acceptance Policy --

(-> mission-gate-normalize (list) list)
(defun mission-gate-normalize (specification)
  "Validate one gate and initialize its durable evidence and retry state."
  (let* ((kind (getf specification :kind))
         (inputs (getf specification :inputs))
         (deterministic-p (getf specification :deterministic-p))
         (attempt-limit (or (getf specification :attempt-limit) 3))
         (timeout (or (getf specification :timeout-seconds) 60)))
    (unless (and (non-empty-string-p (getf specification :id))
                 (member kind '(:command :artifact :lisp))
                 (typep attempt-limit '(integer 1 64))
                 (typep timeout '(integer 1))
                 (listp inputs) (every #'non-empty-string-p inputs)
                 (or (null deterministic-p) (and (eq deterministic-p t) inputs))
                 (case kind
                   (:command (non-empty-string-p (getf specification :command)))
                   (:artifact (non-empty-string-p (getf specification :path)))
                   (:lisp (non-empty-string-p (getf specification :predicate)))))
      (mission--reject ':invalid "Gates require ID, command/artifact/Lisp policy, positive retry and timeout limits; deterministic gates require explicit file inputs."))
    (when (and (getf specification :sha256)
               (not (and (stringp (getf specification :sha256))
                         (= (length (getf specification :sha256)) 64)
                         (every (lambda (character) (digit-char-p character 16))
                                (getf specification :sha256)))))
      (mission--reject ':invalid "Artifact SHA-256 must contain 64 hexadecimal digits."))
    (list :id (getf specification :id) :kind kind
          :command (getf specification :command)
          :directory (getf specification :directory)
          :path (getf specification :path) :sha256 (getf specification :sha256)
          :predicate (getf specification :predicate)
          :inputs (copy-list inputs) :deterministic-p (and deterministic-p t)
          :attempt-limit attempt-limit :timeout-seconds timeout
          :attempts 0 :status ':pending :fingerprint nil :invalidation 0
          :evidence nil)))

(-> mission--tool-context (application) tool-context)
(defun mission--tool-context (application)
  "Create a gate context with the ordinary application's permission boundary."
  (let ((observer (application-agent-observer application)))
    (make-instance 'tool-context
                   :configuration (application-configuration application)
                   :conversation (application-conversation application)
                   :registry (application-tool-registry application)
                   :worker (application-worker application)
                   :agent (application-agent application)
                   :observer observer
                   :command-authorization-function
                   (lambda (command directory)
                     (agent-observer-authorize-command observer command directory))
                   :tool-authorization-function
                   (lambda (tool arguments)
                     (agent-observer-authorize-tool observer tool arguments)))))

(-> mission--file-digest (pathname) string)
(defun mission--file-digest (pathname)
  "Return the SHA-256 of one file, or a stable missing-state marker."
  (cond
    ((not (probe-file pathname)) "missing")
    ((uiop:directory-exists-p pathname)
     (mission--reject ':invalid "Deterministic gate inputs must name files, not directories."))
    (t
     (ironclad:byte-array-to-hex-string (ironclad:digest-file ':sha256 pathname)))))

(-> mission--gate-fingerprint-unlocked (list tool-context) list)
(defun mission--gate-fingerprint-unlocked (gate context)
  "Fingerprint GATE while the workspace mutation lock is held."
  (list (getf gate :invalidation)
        (loop for input in (getf gate :inputs)
              for path = (workspace-tool-path context input)
              collect (list input (mission--file-digest path)))))

(-> mission-gate-fingerprint (list tool-context) list)
(defun mission-gate-fingerprint (gate context)
  "Fingerprint GATE's relevant files and invalidation epoch atomically."
  (with-recursive-lock-held (*workspace-file-mutation-lock*)
    (mission--gate-fingerprint-unlocked gate context)))

(-> mission--gate-tool-result (tool-context string string json-object) tool-result)
(defun mission--gate-tool-result (context namespace name arguments)
  "Invoke an existing execution tool and await its single admitted job."
  (let ((tool (tool-registry-find (tool-context-registry context) namespace name)))
    (unless tool
      (mission--reject ':unavailable (format nil "Gate tool ~A.~A is unavailable." namespace name)))
    ;; The gate's outer cl-jobpond operation supplies the hard deadline.
    ;; Its tool operation must not turn a handoff into passing evidence.
    (let ((*tool-execution-blocking-grace-seconds*
            (+ 1 (gethash "timeout-seconds" arguments 60))))
      (tool-execute tool context arguments))))

(-> mission-gate-execute (list tool-context) (values boolean string))
(defun mission-gate-execute (gate context)
  "Execute one first-class gate through existing execution and permission policy."
  (let ((timeout (getf gate :timeout-seconds)))
    (ecase (getf gate :kind)
      (:command
       (let* ((arguments (json-object "command" (getf gate :command)
                                      "timeout-seconds" timeout))
              (directory (getf gate :directory)))
         (when directory (setf (gethash "directory" arguments) directory))
         (let* ((result (mission--gate-tool-result context "shell" "run" arguments))
                (text (tool-result-content result)))
           (values (and (tool-result-success-p result)
                        (uiop:string-prefix-p (format nil "exit 0~%") text))
                   (mission--bounded-text text)))))
      (:artifact
       (let* ((path (workspace-tool-path context (getf gate :path)))
              (digest (mission--file-digest path))
              (expected (getf gate :sha256))
              (passed-p (and (not (string= digest "missing"))
                             (or (null expected) (string-equal expected digest)))))
         (values (and passed-p t)
                 (format nil "Artifact ~A SHA-256 ~A~@[; expected ~A~]"
                         (getf gate :path) digest expected))))
      (:lisp
       (let* ((code (format nil "(assert (progn ~A))" (getf gate :predicate)))
              (result (mission--gate-tool-result
                       context "lisp" "eval"
                       (json-object "forms" (vector code) "timeout-seconds" timeout))))
         (values (tool-result-success-p result)
                 (mission--bounded-text (tool-result-content result))))))))

(-> application-mission-invalidate (application string string) null)
(defun application-mission-invalidate (application identifier evidence)
  "Invalidate one gate's relevant state without resetting its attempt budget."
  (let ((context (mission-context-find (application-conversation application))))
    (unless context
      (mission--reject ':missing "No such mission gate."))
    ;; Request admission and terminal validation share this lock with final
    ;; acceptance, so invalidation cannot race the verified transition.
    (with-recursive-lock-held ((mission-context-request-lock context))
      (with-recursive-lock-held ((mission-context-lock context))
        (let* ((goal (mission-context-goal context))
               (gate (find identifier (getf goal :gates)
                           :key (lambda (candidate) (getf candidate :id))
                           :test #'equal)))
          (unless gate
            (mission--reject ':missing "No such mission gate."))
          (unless (member (getf goal :status) '(:active :paused :blocked))
            (mission--reject ':terminal "A terminal mission requires a new acceptance policy."))
          (incf (getf gate :invalidation))
          (setf (getf gate :status) ':pending (getf gate :fingerprint) nil)
          (mission--evidence context ':invalidation (list identifier evidence))
          (mission--record context))))
  nil))

(-> application-mission-accept (application string string) null)
(defun application-mission-accept (application identifier evidence)
  "Record explicit user acceptance of a criterion without executable gates."
  (let* ((context (mission-context-find (application-conversation application)))
         (criterion (find identifier (getf (application-goal application) :criteria)
                          :key (lambda (criterion) (getf criterion :id)) :test #'equal)))
    (unless (and context criterion (null (getf criterion :gates))
                 (non-empty-string-p evidence))
      (mission--reject ':invalid "User acceptance requires a criterion without gates and nonempty evidence."))
    (with-recursive-lock-held ((mission-context-lock context))
      (setf (getf criterion :evidence) (mission--bounded-text evidence))
      (mission--evidence context ':user-acceptance (list identifier evidence))
      (mission--record context)))
  nil)

(-> mission--criteria-proven-p (list) boolean)
(defun mission--criteria-proven-p (goal)
  "Return true when every criterion has its configured evidence."
  (and
   (every (lambda (criterion)
            (let ((identifiers (getf criterion :gates)))
              (if identifiers
                  (every (lambda (identifier)
                           (eq (getf (find identifier (getf goal :gates)
                                          :key (lambda (gate) (getf gate :id)) :test #'equal)
                                     :status)
                               ':passed))
                         identifiers)
                  (non-empty-string-p (getf criterion :evidence)))))
          (getf goal :criteria))
   t))

(-> mission--check-gate (mission-context list tool-context) boolean)
(defun mission--check-gate (context gate tool-context)
  "Check one applicable gate, retaining bounded evidence and independent attempt limits."
  (handler-case
      (let ((fingerprint (mission--supervise context "Gate relevant state"
                                              (lambda () (mission-gate-fingerprint gate tool-context)))))
        (when (and (getf gate :deterministic-p)
                   (equal fingerprint (getf gate :fingerprint)))
          (case (getf gate :status)
            (:passed (return-from mission--check-gate t))
            (:failed
             (mission--evidence context ':unchanged-failure (getf gate :id))
             (mission--record context)
             (return-from mission--check-gate nil))))
        (when (>= (getf gate :attempts) (getf gate :attempt-limit))
          (mission--transition context ':exhausted (format nil "Gate ~A exhausted its attempt budget." (getf gate :id)))
          (return-from mission--check-gate nil))
        (incf (getf gate :attempts))
        (setf (getf gate :status) ':running (getf gate :fingerprint) fingerprint)
        (mission--record context)
        (let* ((result (mission--supervise
                        context (format nil "Mission gate ~A" (getf gate :id))
                        (lambda () (multiple-value-list (mission-gate-execute gate tool-context)))
                        :timeout (* 1000 (getf gate :timeout-seconds))))
               (passed-p (first result)) (evidence (second result)))
          (setf (getf gate :status) (if passed-p ':passed ':failed)
                (getf gate :evidence) (mission--bounded-text evidence))
          (mission--evidence context ':gate (list (getf gate :id) (getf gate :status) evidence))
          (mission--record context)
          (when (and (not passed-p) (>= (getf gate :attempts) (getf gate :attempt-limit)))
            (mission--transition context ':exhausted (format nil "Gate ~A exhausted its attempt budget." (getf gate :id))))
          (and passed-p t)))
    (error (condition)
      (setf (getf gate :status) ':failed
            (getf gate :evidence) (mission--bounded-text condition))
      (mission--evidence context ':gate (list (getf gate :id) condition))
      (when (and (eq (getf (mission-context-goal context) :status) ':active)
                 (>= (getf gate :attempts) (getf gate :attempt-limit)))
        (mission--transition context ':exhausted "Gate attempt budget exhausted after an execution failure."))
      (mission--record context)
      nil)))

(-> mission--revalidate-gates (mission-context list tool-context) boolean)
(defun mission--revalidate-gates (context goal tool-context)
  "Revalidate every passed gate while workspace and mission state are locked."
  (let ((stale nil))
    (dolist (gate (getf goal :gates))
      (let ((fingerprint (mission--gate-fingerprint-unlocked gate tool-context)))
        (unless (and (eq (getf gate :status) ':passed)
                     (equal fingerprint (getf gate :fingerprint)))
          (setf (getf gate :status) ':pending
                (getf gate :fingerprint) nil)
          (push (getf gate :id) stale))))
    (when stale
      (mission--evidence context ':stale-gate (nreverse stale))
      (mission--record context))
    (null stale)))

(-> application-mission-verify (application) null)
(defun application-mission-verify (application)
  "Run ordered gates and prove every configured criterion, separately from completion."
  (let* ((context (mission-context-find (application-conversation application)))
         (goal (application-goal application)))
    (unless (and context (mission-goal-p goal))
      (mission--reject ':missing "No mission is set."))
    ;; A reviewer uses a delegated worker; do not await it under the request lock.
    (unless (application-mission-review-checkpoint application ':final-acceptance)
      (return-from application-mission-verify nil))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (with-recursive-lock-held ((mission-context-lock context))
        (mission--admit context :inference-p nil)
        (when (plusp (getf goal :requests-outstanding 0))
          (mission--evidence context ':pending-work "Inference is outstanding; collect its usage before verification.")
          (mission--record context)
          (return-from application-mission-verify nil))
        (setf (getf goal :model-complete-p) t)
        (mission--evidence context ':model-complete "Harness verification requested.")
        (mission--record context))
      (let* ((tool-context (mission--tool-context application))
             (runtime (tool-context-execution-runtime tool-context))
             (live-jobs (and (typep runtime 'task-orchestrator)
                             (remove-if-not
                              (lambda (job)
                                (and (eq context (mission-context-find job)) (not (job-terminal-p job))))
                              (task-orchestrator-list-visible-jobs runtime (application-agent application))))))
        (when (or live-jobs (mission-context-jobs context))
          (mission--evidence context ':pending-work "Delegated work is still live; collect its terminal result before verification.")
          (mission--record context)
          (return-from application-mission-verify nil))
        (dolist (gate (getf goal :gates))
          (unless (mission--check-gate context gate tool-context)
            (return-from application-mission-verify nil)))
        ;; Hold the workspace mutation lock through the proof and transition.
        ;; Authorized edits therefore cannot land between the last digest and
        ;; the terminal status, while invalidation remains serialized by the
        ;; request lock held above.
        (with-recursive-lock-held (*workspace-file-mutation-lock*)
          (with-recursive-lock-held ((mission-context-lock context))
            (when (or (plusp (getf goal :requests-outstanding 0))
                      (mission-context-jobs context))
              (mission--evidence context ':pending-work "Mission work appeared during verification; collect it before acceptance.")
              (mission--record context)
              (return-from application-mission-verify nil))
            (unless (mission--revalidate-gates context goal tool-context)
              (return-from application-mission-verify nil))
            (unless (application-mission-review-current-p application ':final-acceptance)
              (mission--evidence context ':review-pending "Review inputs changed during acceptance checks.")
              (mission--record context)
              (return-from application-mission-verify nil))
            (mission--admit context :inference-p nil)
            (if (mission--criteria-proven-p goal)
                (mission--transition context ':verified "Every explicit acceptance criterion has configured evidence.")
                (mission--transition context ':blocked "Gates passed; acceptance criteria still require user evidence.")))))
  nil)))
