(in-package #:autolith)

;;;; -- Checkpoint References --

;;; Before a compaction request the harness lists what a checkpoint note may
;;; cite: durable records of the conversation, plan steps, mission gates and
;;; unfinished jobs. A plan step or a gate can change after the checkpoint, so
;;; its reference keeps the state seen at capture, and later requests report
;;; the cited ones whose state differs.

(defparameter *continuation-label-characters* 120
  "Maximum characters of a reference's label.")

(defparameter *continuation-context-maximum-changes* 16
  "Maximum changed references one request reports.")

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
  "Return a mission gate's status."
  (let* ((goal (getf authorities :goal))
         (gate (and (equal (getf reference :mission) (getf goal :created-at))
                    (find (getf reference :gate) (getf goal :gates)
                          :key (lambda (gate) (getf gate :id)) :test #'equal))))
    (when gate
      (string-downcase (getf gate :status)))))


;;;; -- Public Functions --

(-> continuation-references
    (agent list &key (:prior (option list)) (:unfinished-work list)) list)
(defun continuation-references (agent records &key prior unfinished-work)
  "Return the references a checkpoint of AGENT's conversation may cite.

Plan steps, mission gates and the jobs of the UNFINISHED-WORK snapshot come
first, then the references the PRIOR checkpoint kept, then RECORDS, the
covered user messages and tool results, up to
*CONTINUATION-MAXIMUM-REFERENCES*. A thing named twice keeps its first
reference, so a plan step or gate the prior checkpoint cited shows its state
now."
  (let ((conversation (agent-conversation agent)))
    (take *continuation-maximum-references*
          (remove-duplicates
           (append (continuation--authority-references
                    (continuation--authorities (agent-configuration agent) conversation))
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
                   collect (format nil "~A, ~A: was ~A, ~:[is gone~;now ~:*~A~]"
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
         :label  (single-line-excerpt label *continuation-label-characters*)
         identity))

(-> continuation--plan-step-key (plan-step) string)
(defun continuation--plan-step-key (step)
  "Return what names STEP in a reference: its identifier, or else its text."
  (single-line-excerpt (or (plan-step-identifier step) (plan-step-text step))
                       *continuation-label-characters*))

(-> continuation--authorities (configuration conversation) list)
(defun continuation--authorities (configuration conversation)
  "Return the workspace plan, how its steps read, and CONVERSATION's mission goal.

Each is read once for all references of a request. The steps are read against
current facts only while knowledge is active."
  (let ((plan    (plan-load configuration))
        (mission (mission-context-find conversation)))
    (list :workspace  (workspace-directory-identifier (config :working-directory configuration))
          :plan       plan
          :step-notes (and plan
                           (knowledge-active-p configuration)
                           (plan-step-snapshot plan (knowledge-view-current configuration)))
          :goal       (and mission (mission-context-goal mission)))))

(-> continuation--authority-references (list) list)
(defun continuation--authority-references (authorities)
  "Return a reference to each plan step and mission gate of AUTHORITIES, with its state now."
  (flet ((observe (kind label identity)
           "Return a reference that also holds the state of its thing now."
           (let ((reference (continuation--reference kind label identity)))
             (append reference
                     (list :observed
                           (continuation-reference-state kind reference authorities))))))
    (let ((plan (getf authorities :plan))
          (goal (getf authorities :goal)))
      (append
       (loop for step in (and plan (workspace-plan-steps plan))
             collect (observe ':plan-step
                              (format nil "plan step ~S" (plan-step-text step))
                              (list :workspace (getf authorities :workspace)
                                    :step      (continuation--plan-step-key step))))
       (loop for gate in (getf goal :gates)
             collect (observe ':gate
                              (format nil "mission gate ~A" (getf gate :id))
                              (list :mission (getf goal :created-at)
                                    :gate    (getf gate :id))))))))

(-> continuation--job-reference (list) list)
(defun continuation--job-reference (job)
  "Return a reference to JOB, one row of an unfinished-work snapshot."
  (continuation--reference
   ':job
   (format nil "job ~A, ~(~A~) at the checkpoint" (getf job :job-id) (getf job :status))
   (list :execution-id (getf job :execution-id))))

(-> continuation--record-reference (conversation list) list)
(defun continuation--record-reference (conversation record)
  "Return a reference to RECORD, a user message or a tool result of CONVERSATION."
  (destructuring-bind (kind &key seq tool call-id status content output &allow-other-keys)
      record
    (let ((identity (list :conversation (conversation-identifier conversation)
                          :sequence     seq)))
      (if (eq kind ':message)
          (continuation--reference
           ':message
           (format nil "user message, sequence ~D: ~A" seq (or content ""))
           identity)
          (continuation--reference
           ':tool-result
           (format nil "~A result, sequence ~D, call ~A, ~(~A~): ~A"
                   tool seq call-id status (or output ""))
           (append identity (list :call-id call-id)))))))
