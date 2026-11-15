(in-package #:autolith)

;;;; -- Checkpoint References --

;;; Before a compaction request the harness lists what a checkpoint note may
;;; cite: durable records of the conversation, plan steps, mission gates, facts
;;; the conversation was given and unfinished jobs. A plan step, a gate or a
;;; fact can change after the checkpoint, so its reference keeps the state seen
;;; at capture, and later requests report the cited ones whose state differs.

(defparameter *continuation-label-characters* 120
  "Maximum characters of the text a reference's label quotes.")

(defparameter *continuation-maximum-claims* 16
  "Maximum facts given to the conversation that one catalog offers.")

(defvar *continuation-context-turn-changes* (make-hash-table :test #'equal :synchronized t)
  "Each conversation's changed-reference advice for its current turn, computed once.

An entry lists the turn, the advice, the checkpoint it was computed for and
the state of each reference the model was last told.")

(context-register-conversation-table *continuation-context-turn-changes*)

(defgeneric continuation-reference-state (kind reference authorities)
  (:documentation
   "Return the current state of the thing REFERENCE of KIND names, as a short string.

Return NIL for a thing that is gone and for a kind whose things never change.
AUTHORITIES is the result of CONTINUATION--AUTHORITIES.")
  (:method ((kind t) reference authorities)
    (declare (ignore reference authorities))
    nil))

(defmethod continuation-reference-state ((kind (eql :plan-step)) reference authorities)
  "Return a plan step's status and, when its requirements are checked, how they stand."
  (let* ((steps (and (equal (getf reference :workspace) (getf authorities :workspace))
                     (getf authorities :plan)
                     (workspace-plan-steps (getf authorities :plan))))
         (index (position (getf reference :step) steps
                          :key #'continuation--plan-step-key :test #'equal)))
    (when index
      (format nil "~(~A~)~@[, ~A~]"
              (plan-step-status (nth index steps))
              (getf (find (1+ index) (getf authorities :step-notes)
                          :key (lambda (entry) (getf entry :index)))
                    :note)))))

(defmethod continuation-reference-state ((kind (eql :gate)) reference authorities)
  "Return the status of a gate of the conversation's own mission.

A passed gate says so when its inputs no longer match the fingerprint it
passed on, and when they cannot be read to tell. The inputs are fingerprinted
as the mission does it; the gate is neither run nor changed."
  (let* ((mission (getf authorities :mission))
         (gate    (and mission
                       (equal (getf reference :mission) (mission-knowledge--identity mission))
                       (find (getf reference :gate) (getf (mission-context-goal mission) :gates)
                             :key (lambda (gate) (getf gate :id)) :test #'equal))))
    (when gate
      (format nil "~(~A~)~@[, ~A~]"
              (getf gate :status)
              (and (eq (getf gate :status) ':passed)
                   (handler-case
                       (and (not (equal (getf gate :fingerprint)
                                        (mission-gate-fingerprint
                                         gate (getf authorities :tool-context))))
                            "inputs changed since it was checked")
                     (error ()
                       "inputs cannot be read to check that it still holds")))))))

(defmethod continuation-reference-state ((kind (eql :claim)) reference authorities)
  "Return a fact's logical status, whether it applies now, and the bases it rests on.

The current knowledge view evaluates the fact from its evidence and rules, so
a retracted premise changes the state of what was derived from it. Without an
active view the fact is taken to stand as it did at capture."
  (let ((view (getf authorities :view)))
    (if view
        (destructuring-bind (&key text status applicability bases &allow-other-keys)
            (first (knowledge-explain view (getf reference :claim) :limit 1))
          (and text
               (format nil "~(~A, ~A~@[, on ~{~{~A~^ and ~}~^ or ~}~]~)"
                       status applicability bases)))
        (getf reference :observed))))


;;;; -- Public Functions --

(-> continuation-references
    (agent list &key (:prior (option list)) (:unfinished-work list)) list)
(defun continuation-references (agent evidence &key prior unfinished-work)
  "Return the references a checkpoint of AGENT's conversation may cite.

Plan steps, mission gates, facts the conversation was given and the jobs of
the UNFINISHED-WORK snapshot come first, then the references the PRIOR
checkpoint kept, then EVIDENCE, the user messages and tool results the model
is shown, up to *CONTINUATION-MAXIMUM-REFERENCES*. A thing named twice keeps
its first reference, so a plan step, gate or fact the prior checkpoint cited
shows its state now."
  (let ((configuration (agent-configuration agent))
        (conversation  (agent-conversation agent)))
    (take *continuation-maximum-references*
          (remove-duplicates
           (append (continuation--authority-references
                    configuration conversation
                    (continuation--authorities configuration conversation))
                   (mapcar #'continuation--job-reference (getf unfinished-work :jobs))
                   (getf prior :references)
                   (mapcar #'continuation--evidence-reference evidence))
           :key (lambda (reference) (getf reference :handle)) :test #'string= :from-end t))))

(define-context-contributor continuation-changes (request)
  "Report once each reference the current checkpoint cites whose state changed.

The states are read once per turn. A change the model was told is not
reported again until the state changes once more."
  (let* ((conversation (request-context-conversation request))
         (continuation (and (not (request-context-compaction-p request))
                            (conversation-continuation conversation)))
         (observed     (remove nil (getf continuation :references)
                               :key (lambda (reference) (getf reference :observed)))))
    (when observed
      (let ((identifier (conversation-identifier conversation))
            (turn       (memory-context--turn-key conversation)))
        (destructuring-bind (&optional cached-turn advice checkpoint told)
            (gethash identifier *continuation-context-turn-changes*)
          (if (and (eql cached-turn turn) (eq checkpoint continuation))
              advice
              (multiple-value-bind (advice states)
                  (continuation--changes
                   (continuation--authorities (request-context-configuration request)
                                              conversation)
                   observed
                   :told (and (eq checkpoint continuation) told)
                   :turn turn)
                (setf (gethash identifier *continuation-context-turn-changes*)
                      (list turn advice continuation states))
                advice)))))))


;;;; -- Private Functions --

(-> continuation--reference (keyword string list) list)
(defun continuation--reference (kind label identity)
  "Return a reference of KIND, described by LABEL, to the thing IDENTITY names.

IDENTITY is a property list. The handle is a digest of KIND and IDENTITY, so
one thing keeps one handle from checkpoint to checkpoint."
  (list* :handle (let ((*knowledge-identifier-digits* 8))
                   (knowledge-digest-identifier (char-downcase (char (symbol-name kind) 0))
                                                (cons kind identity)))
         :kind   kind
         :label  label
         identity))

(-> continuation--excerpt ((option string)) string)
(defun continuation--excerpt (text)
  "Return the single-line start of TEXT that a reference's label quotes."
  (single-line-excerpt (or text "") *continuation-label-characters*))

(-> continuation--plan-step-key (plan-step) string)
(defun continuation--plan-step-key (step)
  "Return what names STEP in a reference: its identifier, or else its whole text."
  (or (plan-step-identifier step) (plan-step-text step)))

(-> continuation--authorities (configuration conversation) list)
(defun continuation--authorities (configuration conversation)
  "Return what the references of one request are resolved against, each read once.

That is the workspace plan and how its steps read, the current knowledge view,
and CONVERSATION's mission. The view exists only while knowledge is active,
and only then are plan steps read against current facts. The tool context
fingerprints gate inputs. It asks nobody for authorization, so it reads no
input outside the workspace."
  (let ((plan (plan-load configuration))
        (view (and (knowledge-active-p configuration)
                   (knowledge-view-current configuration))))
    (list :workspace    (workspace-directory-identifier (config :working-directory configuration))
          :plan         plan
          :step-notes   (and plan view (plan-step-snapshot plan view))
          :view         view
          :mission      (mission-context-find conversation)
          :tool-context (make-instance 'tool-context :configuration configuration
                                                     :conversation  conversation
                                                     :worker        nil))))

(-> continuation--changes
    (list list &key (:told list) (:turn integer)) (values (option context-contribution) list))
(defun continuation--changes (authorities references &key told turn)
  "Return advice on REFERENCES whose state differs from what the model knows, and their states now.

The states are resolved against AUTHORITIES. TOLD maps a handle to the state
the model was last told; a reference it leaves out is known by the state
observed at capture. The second value maps each handle to its state now. The
advice is delivered once in turn number TURN."
  (let* ((states  (mapcar (lambda (reference)
                            (cons (getf reference :handle)
                                  (continuation-reference-state
                                   (getf reference :kind) reference authorities)))
                          references))
         (changes (loop for reference in references
                        for (handle . state) in states
                        for entry = (assoc handle told :test #'string=)
                        for known = (if entry
                                        (rest entry)
                                        (getf reference :observed))
                        unless (equal state known)
                          collect (format nil "~A, ~A: was ~:[missing~;~:*~A~], ~:[can no longer be found~;now ~:*~A~]"
                                          handle (getf reference :label) known state))))
    (values (and changes
                 (make-context-contribution
                  :identifier        "continuation-changes"
                  :instruction
                  (format nil "These references of the checkpoint changed after it was written. Revisit the notes that cite them.~%~{- ~A~%~}"
                          changes)
                  :priority          55
                  :lifetime          ':next-request
                  :deduplication-key (format nil "continuation-changes:~D" turn)))
            states)))

(-> continuation--authority-references (configuration conversation list) list)
(defun continuation--authority-references (configuration conversation authorities)
  "Return a reference to each plan step, mission gate and given fact, with its state now.

The facts are those CONVERSATION was given, at most
*CONTINUATION-MAXIMUM-CLAIMS*, and only while AUTHORITIES holds a knowledge
view. A fact's label gives the address the memory resource reads it at."
  (destructuring-bind (&key workspace plan view mission &allow-other-keys) authorities
    (flet ((observe (kind label identity)
             "Return a reference that also holds the state of its thing now."
             (let ((reference (continuation--reference kind label identity)))
               (append reference
                       (list :observed
                             (continuation-reference-state kind reference authorities))))))
      (append
       (loop for step in (and plan (workspace-plan-steps plan))
             collect (observe ':plan-step
                              (format nil "plan step ~S"
                                      (continuation--excerpt (plan-step-text step)))
                              (list :workspace workspace
                                    :step      (continuation--plan-step-key step))))
       (loop for gate in (and mission (getf (mission-context-goal mission) :gates))
             collect (observe ':gate
                              (format nil "mission gate ~A" (getf gate :id))
                              (list :mission (mission-knowledge--identity mission)
                                    :gate    (getf gate :id))))
       (loop for entry in (and view
                               (take *continuation-maximum-claims*
                                     (knowledge-viewer-snapshot
                                      configuration
                                      (memory-context-recall-viewer conversation)
                                      (knowledge-view-workspace view))))
             collect (observe ':claim
                              (format nil "fact memory:claim/~A: ~A"
                                      (getf entry :claim)
                                      (continuation--excerpt (getf entry :text)))
                              (list :claim (getf entry :claim))))))))

(-> continuation--job-reference (list) list)
(defun continuation--job-reference (job)
  "Return a reference to JOB, one row of an unfinished-work snapshot."
  (continuation--reference
   ':job
   (format nil "job ~A, ~(~A~) at the checkpoint" (getf job :job-id) (getf job :status))
   (list :execution-id (getf job :execution-id))))

(-> continuation--evidence-reference (list) list)
(defun continuation--evidence-reference (evidence)
  "Return a reference to the user message or tool result EVIDENCE describes.

Its label gives the address the conversation resource reads the record at, so
the record stays reachable from a conversation that adopts the checkpoint."
  (destructuring-bind (&key conversation sequence text call-id tool status) evidence
    (let ((identity (list :conversation conversation :sequence sequence))
          (address  (format nil "conversation:id/~A sequence ~D" conversation sequence)))
      (if call-id
          (continuation--reference
           ':tool-result
           (format nil "~A result at ~A, call ~A~@[, ~(~A~)~]: ~A"
                   (or tool "tool") address call-id status (continuation--excerpt text))
           (append identity (list :call-id call-id)))
          (continuation--reference
           ':message
           (format nil "user message at ~A: ~A" address (continuation--excerpt text))
           identity)))))
