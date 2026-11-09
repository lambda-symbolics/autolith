(in-package #:autolith)

;;;; -- Completion-Driven Primary Turns --

(-> application-job-completions--notice-admissible-p
    (application list &key (:ignore-reservations-p boolean)) boolean)
(defun application-job-completions--notice-admissible-p
    (application notice &key ignore-reservations-p)
  "Require exact ownership and unspent authority, optionally ignoring temporary reservations."
  (let* ((goal       (application-goal application))
         (mission-id (getf notice :mission-id)))
    (not
     (null
      (and (equal (getf notice :owner-conversation)
                  (conversation-identifier (application-conversation application)))
           (eq (getf notice :completion-policy) ':continue)
           (cond
             (mission-id
              (and (mission-goal-p goal)
                   (equal mission-id (getf goal :completion-id))
                   (eq (getf goal :status) ':active)
                   (not (mission--unknown-usage-blocks-p goal))
                   (< (getf goal :turns-used) (getf goal :turn-limit))
                   (< (getf goal :tokens-used) (getf goal :token-limit))
                   (or ignore-reservations-p
                       (< (+ (getf goal :tokens-used) (getf goal :tokens-reserved 0))
                          (getf goal :token-limit)))
                   (> (getf goal :deadline) (get-universal-time))))
             ((mission-goal-p goal)
              (not (eq (getf goal :status) ':active)))
             (goal
              (case (getf goal :status)
                (:active
                 (not (application--goal-continuations-exhausted-p application goal)))
                (:complete
                 t)
                (t
                 nil)))
             (t
              t)))))))

(-> application-job-completions-awaited-p (application) boolean)
(defun application-job-completions-awaited-p (application)
  "Return true while a pending result or unfinished job will wake APPLICATION.

Goal continuations defer while a pending result or unfinished job is expected to
trigger a completion turn. That turn resumes the goal. Without a connected
wakeup, no completion turn is expected."
  (let ((agent (and (slot-boundp application 'agent) (application-agent application))))
    (flet ((admissible-p (notice)
             (application-job-completions--notice-admissible-p application notice)))
      (and (typep agent 'agent)
           (task-completion-wakeup-connected-p agent)
           (or (some #'admissible-p (task-completion-pending agent :continuation-only-p t))
               (some (lambda (job)
                       (admissible-p
                        (list :owner-conversation (session-job-completion-owner-conversation job)
                              :completion-policy  (session-job-completion-policy job)
                              :mission-id         (session-job-completion-mission-id job))))
                     (task-completion-awaited-jobs agent)))
           t))))

(-> application-job-completions--presentation-admissible-p
    (application-input-controller &key (:session-locked-p boolean)) boolean)
(defun application-job-completions--presentation-admissible-p (controller &key session-locked-p)
  "Allow terminal notices only while this controller owns an unpaused session.

Persistence and follow-up editing restrict inference, not terminal presentation."
  (let ((application (application-input-controller-application controller)))
    (and (slot-boundp application 'agent)
         (typep (application-agent application) 'agent)
         (not (application-input-controller-stopping-p controller))
         (not (application-input-controller-failure controller))
         (not (application-input-controller-localgroup-handoff-p controller))
         (not (application-input-controller-queued-work-paused-p controller))
         (not (if session-locked-p
                  (let ((session (application-localgroup-session application)))
                    (and session (localgroup-session-paused-p session)))
                  (application-localgroup-paused-p application))))))

(-> application-job-completions--controller-admissible-p
    (application-input-controller) boolean)
(defun application-job-completions--controller-admissible-p (controller)
  "Check primary input authority without clearing a cancellation or pause."
  (let ((application (application-input-controller-application controller)))
    (and (slot-boundp application 'agent)
         (typep (application-agent application) 'agent)
         (not (application-input-controller-stopping-p controller))
         (not (application-input-controller-failure controller))
         (not (application-input-controller-localgroup-handoff-p controller))
         (not (application-input-controller-queued-work-paused-p controller))
         (null (application-input-controller-follow-up-edit-index controller))
         (application-input-controller-pending-persistence-enabled-p controller)
         (not (application-localgroup-paused-p application)))))

(-> application-job-completions-awaited-count (application) (integer 0))
(defun application-job-completions-awaited-count (application)
  "Snapshot unfinished jobs with authority to wake this primary conversation.

Pending outcomes are deliberately excluded: they no longer represent running
work. Admission uses the same owner, policy, mission and controller authority as
completion dispatch. Both tool executions and child tasks use this job registry."
  (let ((agent      (and (slot-boundp application 'agent) (application-agent application)))
        (controller (application-input-controller application)))
    (if (and (typep agent 'agent)
             controller
             (application-job-completions--controller-admissible-p controller)
             (task-completion-wakeup-connected-p agent))
        (count-if
         (lambda (job)
           (application-job-completions--notice-admissible-p
            application
            (list :owner-conversation (session-job-completion-owner-conversation job)
                  :completion-policy  (session-job-completion-policy job)
                  :mission-id         (session-job-completion-mission-id job))))
         (task-completion-awaited-jobs agent))
        0)))

(-> application-job-completions-present-yield (application list) null)
(defun application-job-completions-present-yield (application details)
  "Record and present an empty terminal response's historical wakeup snapshot.

This is presentation metadata, not an assistant message or provider item. Replay
uses the recorded count rather than today's jobs. A job completing after this
snapshot cannot change the meaning of an already finalized transcript entry."
  (let ((count (application-job-completions-awaited-count application)))
    (when (plusp count)
      (conversation-append-record
       (application-conversation application)
       (list :assistant-yield :job-count count :response-id (getf details :response-id)))
      (application-render-records application)))
  nil)

(-> application-job-completions--next-time
    (application-input-controller) (option integer))
(defun application-job-completions--next-time (controller)
  "Return the next inference or display batch deadline while the controller is safe."
  (when (application-job-completions--controller-admissible-p controller)
    (let* ((application (application-input-controller-application controller))
           (agent       (application-agent application))
           (notices     (remove-if-not
                         (lambda (notice)
                            (or (application-job-completions--notice-admissible-p application notice)
                                (application-job-completions--terminal-notice-p application notice :ready-p nil)))
                         (task-completion-pending agent :continuation-only-p t))))
      (when notices
        (reduce #'min notices :key (lambda (notice) (getf notice :ready-at)))))))

(-> application-job-completions--terminal-notice-p (application list &key (:ready-p boolean)) boolean)
(defun application-job-completions--terminal-notice-p (application notice &key (ready-p t))
  "Select owned terminal outcomes that cannot authorize automatic inference."
  (not
   (null
    (and (equal (getf notice :owner-conversation)
                (conversation-identifier (application-conversation application)))
         (eq (getf notice :completion-policy) ':continue)
         (member (getf notice :state) '(:completed :failed :aborted))
         (or (not ready-p) (<= (getf notice :ready-at) (get-universal-time)))
         (not (task-completion--delivered-p (application-conversation application) (getf notice :id)))
         (not (application-job-completions--notice-admissible-p
               application notice :ignore-reservations-p t))))))

(-> application-job-completions--no-resume-reason (application list) string)
(defun application-job-completions--no-resume-reason (application notice)
  "Explain the current restriction and a valid next action without changing authority."
  (let* ((goal       (application-goal application))
         (status     (getf goal :status))
         (mission-id (getf notice :mission-id))
         (new-budget "Start a new mission with (mission :start SPECIFICATION).")
         (review     "Review the result and decide separate work explicitly."))
    (multiple-value-bind (reason action)
        (cond
          ((and mission-id
                (or (not (mission-goal-p goal))
                    (not (equal mission-id (getf goal :completion-id)))))
           (values "the result belongs to a previous mission" review))
          ((and (mission-goal-p goal) (not mission-id) (eq status ':active))
           (values "the result is not part of the active mission" review))
          ((and (mission-goal-p goal) (mission--unknown-usage-blocks-p goal))
           (values (format nil "the mission is ~A with unknown token usage" status) new-budget))
          ((and (mission-goal-p goal) (<= (getf goal :deadline) (get-universal-time)))
           (values (format nil "the mission is ~A and its deadline has passed" status) new-budget))
          ((and (mission-goal-p goal) (>= (getf goal :turns-used) (getf goal :turn-limit)))
           (values (format nil "the mission is ~A and used ~D of ~D turns"
                           status (getf goal :turns-used) (getf goal :turn-limit))
                   new-budget))
          ((and (mission-goal-p goal)
                (>= (+ (getf goal :tokens-used) (getf goal :tokens-reserved 0))
                    (getf goal :token-limit)))
           (values (format nil "the mission is ~A and used or reserved ~D of ~D tokens"
                           status (+ (getf goal :tokens-used) (getf goal :tokens-reserved 0))
                           (getf goal :token-limit))
                   new-budget))
          ((and (mission-goal-p goal) (eq status ':blocked))
           (values "the mission is blocked"
                   "Review the result and resolve the stated blocker, then use (mission :resume)."))
          ((and (mission-goal-p goal) (eq status ':paused))
           (values "the mission is paused" "Use (mission :resume) to continue the mission."))
          ((and (mission-goal-p goal) (not (eq status ':active)))
           (values (format nil "the mission is ~A" status) new-budget))
          ((mission-goal-p goal)
           (values (format nil "the mission is ~A" status) review))
          ((eq status ':complete)
           (values "the session goal is complete" "Set new work with (goal \"OBJECTIVE\")."))
          ((eq status ':paused)
           (values "the session goal is paused" "Use (goal :resume) to continue the goal."))
          ((and goal (not (eq status ':active)))
           (values (format nil "the session goal is ~A" status) review))
          ((and goal (application--goal-continuations-exhausted-p application goal))
           (let ((limit (application-goal-continuation-limit application)))
             (values (format nil "the session goal used ~D of ~D automatic continuations (effective limit ~D)"
                             (getf goal :continuations 0) limit limit)
                     (format nil "Send a message to reset the count, or raise the limit with ~
                                  (settings :goal-continuation-limit ~D)."
                             (* 2 limit)))))
          (t
           (values "the result is not eligible for automatic continuation" review)))
      (format nil "No automatic continuation: ~A. ~A" reason action))))

(-> application-job-completions--present-terminal (application list) null)
(defun application-job-completions--present-terminal (application notices)
  "Present only newly delivered records, using their existing replay identities."
  (let* ((conversation (application-conversation application))
         (records      (conversation-records-newest conversation *task-completion-batch-limit*)))
    (dolist (notice notices)
      (let* ((receipt (task-completion--receipt (getf notice :id)))
             (record  (find receipt records :test #'equal
                            :key                  (lambda (entry) (getf (rest entry) :pending-input-identifier))))
             (reason  (application-job-completions--no-resume-reason application notice)))
        (when record
          (application-present
           application
           (append
            (if (application-ui application)
                (application--job-completion-entry application (rest record))
                (list (terminal-span ':notice
                                     (format nil "Job ~A: ~A.~%" (getf notice :job-id) (getf notice :outcome)))))
            (list (terminal-span ':plain (string #\Newline))
                  (terminal-span ':notice reason)))
           :identifier (application--record-presentation-identifier application record))))))
  nil)

(-> application-job-completions--deliver-terminal (application-input-controller list) null)
(defun application-job-completions--deliver-terminal (controller ticket)
  "Recheck terminal consumption and presentation under controller/session authority."
  (let* ((application (application-input-controller-application controller))
         (agent       (application-agent application))
         (session     (application-localgroup-session application)))
    (flet ((guard (deliver)
             (flet ((consume ()
                      (when (and (application-job-completions--presentation-admissible-p
                                  controller :session-locked-p t)
                                 (null (application-input-controller-follow-up-edit-index controller))
                                 (application-input-controller-pending-persistence-enabled-p controller)
                                 (equal (getf ticket :conversation)
                                        (conversation-identifier (application-conversation application)))
                                 (getf ticket :events)
                                 (every (lambda (identifier)
                                          (let ((notice (find identifier (task-completion-pending agent)
                                                              :key  (lambda (entry) (getf entry :id))
                                                              :test #'equal)))
                                            (and notice
                                                 (application-job-completions--terminal-notice-p application notice))))
                                        (getf ticket :events)))
                        (application-job-completions--present-terminal application (funcall deliver)))))
               (with-lock-held ((application-input-controller-lock controller))
                 (if session
                     (with-lock-held ((image-daemon:daemon-runtime-lock session))
                       (consume))
                     (consume))))))
      (task-completion-deliver agent :identifiers (getf ticket :events) :call-with-delivery #'guard)))
  nil)

(-> application-job-completions--take-maintenance
    (application-input-controller) (option list))
(defun application-job-completions--take-maintenance (controller)
  "Drain notification pressure without granting an inference or consuming admitted results."
  (let* ((application (application-input-controller-application controller))
         (agent       (and (slot-boundp application 'agent) (application-agent application)))
         (scan-wait   (and agent (task-cancellation-scan-wait agent))))
    (when (and (application-job-completions--presentation-admissible-p controller)
               scan-wait (zerop scan-wait))
      (return-from application-job-completions--take-maintenance
        (list ':job-completion-maintenance
              (list :conversation        (conversation-identifier (application-conversation application))
                    :cancellation-scan-p t)))))
  (when (application-job-completions--controller-admissible-p controller)
    (let* ((application (application-input-controller-application controller))
           (agent       (application-agent application)))
      (let ((notices (remove-if-not
                      (lambda (notice) (application-job-completions--terminal-notice-p application notice))
                      (task-completion-pending agent :continuation-only-p t))))
        (when notices
          (return-from application-job-completions--take-maintenance
            (list ':job-completion-maintenance
                  (list :conversation      (conversation-identifier (application-conversation application))
                        :terminal-notice-p t
                        :events            (mapcar (lambda (notice) (getf notice :id))
                                                   (subseq notices 0 (min (length notices) *task-completion-batch-limit*))))))))
      (when (task-completion-maintenance-needed-p agent)
        (let ((notices (remove-if
                        (lambda (notice)
                          (application-job-completions--notice-admissible-p
                           application notice :ignore-reservations-p t))
                        (task-completion-pending agent))))
          (when notices
            (list ':job-completion-maintenance
                  (list :conversation (conversation-identifier (application-conversation application))
                        :events       (mapcar (lambda (notice) (getf notice :id))
                                              (subseq notices 0 (min (length notices) *task-completion-batch-limit*)))))))))))

(-> application-job-completions--present-cancellations
    (application-input-controller list) null)
(defun application-job-completions--present-cancellations (controller ticket)
  "Drain terminal warnings under controller, session, delivery, then UI locks.
Scanning and wake callbacks run outside these locks. Pause/resume writers release
session before acquiring controller; keep the same controller-to-session order
as ordinary controller admission, without recursively locking the session."
  (let* ((application (application-input-controller-application controller))
         (session     (application-localgroup-session application)))
    (flet ((present ()
             (when (and (application-job-completions--presentation-admissible-p
                         controller :session-locked-p t)
                        (equal (getf ticket :conversation)
                               (conversation-identifier (application-conversation application))))
               (task-cancellation-present-pending
                (application-agent application)
                (lambda (notice)
                  (application-present
                   application
                   (application--job-completion-entry
                    application (list :job-completion notice :time (get-universal-time)))
                   :identifier
                   (application--record-presentation-identifier
                    application (list :message :job-completion notice)))
                  ;; Durable replay may already have emitted this identity.
                  t)))))
      (with-lock-held ((application-input-controller-lock controller))
        (if session
            (with-lock-held ((image-daemon:daemon-runtime-lock session))
              (present))
            (present)))))
  nil)

(-> application-job-completions-maintain (application-input-controller list) null)
(defun application-job-completions-maintain (controller ticket)
  "Recheck a bounded maintenance ticket and free receipt-backed capacity without a provider turn."
  (when (getf ticket :terminal-notice-p)
    (return-from application-job-completions-maintain
      (application-job-completions--deliver-terminal controller ticket)))
  (let* ((application (application-input-controller-application controller))
         (agent       (application-agent application))
         (admitted-p
           (with-lock-held ((application-input-controller-lock controller))
             (and (if (getf ticket :cancellation-scan-p)
                      (application-job-completions--presentation-admissible-p controller)
                      (application-job-completions--controller-admissible-p controller))
                  (equal (getf ticket :conversation)
                         (conversation-identifier (application-conversation application)))))))
    (when admitted-p
      (task-cancellation-maintain agent)
      (application-job-completions--present-cancellations controller ticket)
      (let ((identifiers
              (loop for notice in (task-completion-pending agent)
                    when (and (not (eq (getf notice :kind) ':cancellation-warning))
                              (member (getf notice :id) (getf ticket :events) :test #'equal)
                              (not (application-job-completions--notice-admissible-p
                                    application notice :ignore-reservations-p t)))
                      collect (getf notice :id))))
        (when identifiers
          (task-completion-deliver agent :identifiers identifiers)))
      ;; Selected delivery can itself publish a newly due warning.
      (application-job-completions--present-cancellations controller ticket)))
  nil)

(-> application-job-completions--take-work
    (application-input-controller) (option list))
(defun application-job-completions--take-work (controller)
  "Return one coalesced primary work ticket when the oldest batch is ready."
  (let ((deadline (application-job-completions--next-time controller)))
    (when (and deadline (<= deadline (get-universal-time)))
      (let* ((application (application-input-controller-application controller))
             (notices     (remove-if-not
                           (lambda (notice)
                             (application-job-completions--notice-admissible-p application notice))
                           (task-completion-pending (application-agent application)
                                                    :continuation-only-p t))))
        (when notices
          (list ':job-completion
                (list :conversation (conversation-identifier (application-conversation application))
                      :events       (mapcar (lambda (notice) (getf notice :id)) notices))))))))

(-> application-job-completions-run (application-input-controller list) null)
(defun application-job-completions-run (controller ticket)
  "Recheck TICKET, deliver its batch durably, then run one ordinary automatic turn."
  (let* ((application          (application-input-controller-application controller))
         (agent                (application-agent application))
         (admitted-p
           (with-lock-held ((application-input-controller-lock controller))
             (and (application-job-completions--controller-admissible-p controller)
                  (equal (getf ticket :conversation)
                         (conversation-identifier (application-conversation application))))))
         (notices
           (when admitted-p
             (remove-if-not
              (lambda (notice)
                (and (member (getf notice :id) (getf ticket :events) :test #'equal)
                     (application-job-completions--notice-admissible-p application notice)))
              (task-completion-pending agent :continuation-only-p t))))
         (mission-authorized-p (some (lambda (notice) (getf notice :mission-id)) notices)))
    (when notices
      (when mission-authorized-p
        (let ((context (mission-context-find (application-conversation application))))
          (unless context
            (mission--reject ':completion-mission "The completion's mission authority is unavailable."))
          (mission--admit context)))
      ;; Delivery may have won a busy request boundary after ticket construction.
      (when (some (lambda (notice)
                    (application-job-completions--notice-admissible-p application notice))
                  (setf notices
                      (task-completion-deliver
                       agent :identifiers (mapcar (lambda (notice) (getf notice :id)) notices))))
        (let ((goal (application-goal application)))
          (when (and goal (if (mission-goal-p goal)
                             mission-authorized-p
                             (eq (getf goal :status) ':active)))
            (incf (getf goal :continuations 0))
            (application--record-goal application)))
        (application-input-controller-call-with-primary-steering
         controller
         (lambda (take acknowledge)
           (flet ((pending-operations (owner)
                    (application-input-controller--apply-pending-commands controller :agent owner)))
             (application--run-turn
              application
              (if (some (lambda (notice) (eq (getf notice :kind) ':cancellation-warning)) notices)
                  *application-job-cancellation-continuation-prompt*
                  *application-job-completion-continuation-prompt*)
              :continuation-p              t
              :steering-function           take
              :steering-persisted-function acknowledge
              :pending-operations-function #'pending-operations)
             (application--run-goal-continuations
              application
              :steering-function           take
              :steering-persisted-function acknowledge
              :pending-operations-function #'pending-operations)))))))
  nil)

(-> application-job-completions-connect (application) null)
(defun application-job-completions-connect (application)
  "Restore completion continuity and connect only this primary controller's wakeup."
  (let ((agent      (and (slot-boundp application 'agent) (application-agent application)))
        (controller (application-input-controller application)))
    (when (and agent controller)
      (task-completion-restore agent)
      (task-completion-connect
       agent
       (lambda ()
         (with-lock-held ((application-input-controller-lock controller))
           (sb-thread:condition-broadcast
            (application-input-controller-condition-variable controller)))))))
  nil)

(-> application-job-completions-disconnect (application) null)
(defun application-job-completions-disconnect (application)
  "Remove the current agent's completion wakeup before retiring its controller."
  (when (and (slot-boundp application 'agent) (application-agent application))
    (task-completion-disconnect (application-agent application)))
  nil)
