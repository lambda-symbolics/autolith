(in-package #:autolith)

;;;; -- Checkpoint References --

;;; Before a compaction request the harness lists what a checkpoint note may
;;; cite: durable records of the conversation, plan steps, mission gates, facts
;;; the conversation was given and unfinished jobs. A plan step, a gate or a
;;; fact can change after the checkpoint, so its reference keeps the state seen
;;; at capture, and later requests report the cited ones whose state differs.

(defparameter *continuation-label-characters* 120
  "Maximum characters of the text a reference's label quotes.")

(defparameter *continuation-context-maximum-changes* 16
  "Maximum changed references one request reports.")

(defparameter *continuation-maximum-claims* 16
  "Maximum facts given to the conversation that one catalog offers.")

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

A passed gate whose inputs no longer match the fingerprint it passed on says
so. The inputs are fingerprinted as the mission does it; the gate is neither
run nor changed. Inputs that cannot be read leave the status as it is."
  (let* ((mission (getf authorities :mission))
         (gate    (and mission
                       (equal (getf reference :mission) (mission-knowledge--identity mission))
                       (find (getf reference :gate) (getf (mission-context-goal mission) :gates)
                             :key (lambda (gate) (getf gate :id)) :test #'equal))))
    (when gate
      (format nil "~(~A~)~:[~;, inputs changed since it was checked~]"
              (getf gate :status)
              (and (eq (getf gate :status) ':passed)
                   (handler-case
                       (not (equal (getf gate :fingerprint)
                                   (mission-gate-fingerprint gate (getf authorities :tool-context))))
                     (error ()
                       nil)))))))

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
(defun continuation-references (agent records &key prior unfinished-work)
  "Return the references a checkpoint of AGENT's conversation may cite.

Plan steps, mission gates, facts the conversation was given and the jobs of
the UNFINISHED-WORK snapshot come first, then the references the PRIOR
checkpoint kept, then RECORDS, the covered user messages and tool results, up
to *CONTINUATION-MAXIMUM-REFERENCES*. A thing named twice keeps its first
reference, so a plan step, gate or fact the prior checkpoint cited shows its
state now."
  (let ((configuration (agent-configuration agent))
        (conversation  (agent-conversation agent)))
    (take *continuation-maximum-references*
          (remove-duplicates
           (append (continuation--authority-references
                    configuration conversation
                    (continuation--authorities configuration conversation))
                   (mapcar #'continuation--job-reference (getf unfinished-work :jobs))
                   (getf prior :references)
                   (mapcar (lambda (record)
                             (continuation--record-reference conversation record))
                           records))
           :key (lambda (reference) (getf reference :handle)) :test #'string= :from-end t))))

(define-context-contributor continuation-changes (request)
  "Report the references the current checkpoint cites whose state changed since."
  (let* ((conversation (request-context-conversation request))
         (observed     (and (not (request-context-compaction-p request))
                            (remove nil
                                    (getf (conversation-continuation conversation) :references)
                                    :key (lambda (reference) (getf reference :observed)))))
         (authorities  (and observed
                            (continuation--authorities
                             (request-context-configuration request) conversation)))
         (changes
           (loop for reference in observed
                 for state = (continuation-reference-state
                              (getf reference :kind) reference authorities)
                 unless (equal state (getf reference :observed))
                   collect (format nil "~A, ~A: was ~A, ~:[can no longer be found~;now ~:*~A~]"
                                   (getf reference :handle) (getf reference :label)
                                   (getf reference :observed) state))))
    (when changes
      (make-context-contribution
       :identifier        "continuation-changes"
       :instruction
       (format nil "These references of the checkpoint changed after it was written. Revisit the notes that cite them.~%~{- ~A~%~}"
               (take *continuation-context-maximum-changes* changes))
       :priority          55
       :deduplication-key "continuation-changes"))))


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

(-> continuation--record-reference (conversation list) list)
(defun continuation--record-reference (conversation record)
  "Return a reference to RECORD, a user message or a tool result of CONVERSATION.

Its label gives the address the conversation resource reads the record at, so
the record stays reachable from a conversation that adopts the checkpoint."
  (destructuring-bind (kind &key seq tool call-id status content output &allow-other-keys)
      record
    (let* ((identifier (conversation-identifier conversation))
           (identity   (list :conversation identifier :sequence seq))
           (address    (format nil "conversation:id/~A sequence ~D" identifier seq)))
      (if (eq kind ':message)
          (continuation--reference
           ':message
           (format nil "user message at ~A: ~A" address (continuation--excerpt content))
           identity)
          (continuation--reference
           ':tool-result
           (format nil "~A result at ~A, call ~A, ~(~A~): ~A"
                   tool address call-id status (continuation--excerpt output))
           (append identity (list :call-id call-id)))))))
