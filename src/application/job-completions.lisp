(in-package #:autolith)

;;;; -- Completion-Driven Primary Turns --

(-> application-job-completions--notice-admissible-p (application list) boolean)
(defun application-job-completions--notice-admissible-p (application notice)
  "Require the notice's exact conversation and current, unspent goal authority."
  (let* ((goal (application-goal application))
         (mission-id (getf notice :mission-id)))
    (not
     (null
      (and (equal (getf notice :owner-conversation)
                  (conversation-identifier (application-conversation application)))
           (eq (getf notice :completion-policy) ':continue)
           (cond
             ((mission-goal-p goal)
              (and mission-id
                   (equal mission-id (getf goal :completion-id))
                   (eq (getf goal :status) ':active)
                   (zerop (getf goal :unknown-usage 0))
                   (< (getf goal :turns-used) (getf goal :turn-limit))
                   (< (+ (getf goal :tokens-used) (getf goal :tokens-reserved 0))
                      (getf goal :token-limit))
                   (> (getf goal :deadline) (get-universal-time))))
             (mission-id
              nil)
             (goal
              (and (eq (getf goal :status) ':active)
                   (< (getf goal :continuations 0) *application-goal-continuation-limit*)))
             (t
              t)))))))

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

(-> application-job-completions--next-time
    (application-input-controller) (option integer))
(defun application-job-completions--next-time (controller)
  "Return an admitted batch deadline, or NIL when automatic work must wait."
  (when (application-job-completions--controller-admissible-p controller)
    (let* ((application (application-input-controller-application controller))
           (agent (application-agent application))
           (notices (remove-if-not
                     (lambda (notice)
                       (application-job-completions--notice-admissible-p application notice))
                     (task-completion-pending agent :continuation-only-p t))))
      (when notices
        (reduce #'min notices :key (lambda (notice) (getf notice :ready-at)))))))

(-> application-job-completions--take-maintenance
    (application-input-controller) (option list))
(defun application-job-completions--take-maintenance (controller)
  "Drain notification pressure without granting an inference or consuming admitted results."
  (when (application-job-completions--controller-admissible-p controller)
    (let* ((application (application-input-controller-application controller))
           (agent (application-agent application)))
      (when (task-completion-maintenance-needed-p agent)
        (let ((notices (remove-if
                        (lambda (notice)
                          (application-job-completions--notice-admissible-p application notice))
                        (task-completion-pending agent))))
          (when notices
            (list ':job-completion-maintenance
                  (list :conversation (conversation-identifier (application-conversation application))
                        :events (mapcar (lambda (notice) (getf notice :id))
                                        (subseq notices 0 (min (length notices) *task-completion-batch-limit*)))))))))))

(-> application-job-completions-maintain (application-input-controller list) null)
(defun application-job-completions-maintain (controller ticket)
  "Recheck a bounded maintenance ticket and free receipt-backed capacity without a provider turn."
  (let* ((application (application-input-controller-application controller))
         (agent (application-agent application))
         (admitted-p
           (with-lock-held ((application-input-controller-lock controller))
             (and (application-job-completions--controller-admissible-p controller)
                  (equal (getf ticket :conversation)
                         (conversation-identifier (application-conversation application)))))))
    (when admitted-p
      (let ((identifiers
              (loop for notice in (task-completion-pending agent)
                    when (and (member (getf notice :id) (getf ticket :events) :test #'equal)
                              (not (application-job-completions--notice-admissible-p application notice)))
                      collect (getf notice :id))))
        (when identifiers
          (task-completion-deliver agent :identifiers identifiers)))))
  nil)

(-> application-job-completions--take-work
    (application-input-controller) (option list))
(defun application-job-completions--take-work (controller)
  "Return one coalesced primary work ticket when the oldest batch is ready."
  (let ((deadline (application-job-completions--next-time controller)))
    (when (and deadline (<= deadline (get-universal-time)))
      (let* ((application (application-input-controller-application controller))
             (notices (remove-if-not
                       (lambda (notice)
                         (application-job-completions--notice-admissible-p application notice))
                       (task-completion-pending (application-agent application)
                                                :continuation-only-p t))))
        (when notices
          (list ':job-completion
                (list :conversation (conversation-identifier (application-conversation application))
                      :events (mapcar (lambda (notice) (getf notice :id)) notices))))))))

(-> application-job-completions-run (application-input-controller list) null)
(defun application-job-completions-run (controller ticket)
  "Recheck TICKET, deliver its batch durably, then run one ordinary automatic turn."
  (let* ((application (application-input-controller-application controller))
         (agent (application-agent application))
         (admitted-p
           (with-lock-held ((application-input-controller-lock controller))
             (and (application-job-completions--controller-admissible-p controller)
                  (equal (getf ticket :conversation)
                         (conversation-identifier (application-conversation application)))))))
    (when (and admitted-p
               (some (lambda (notice)
                       (and (member (getf notice :id) (getf ticket :events) :test #'equal)
                            (application-job-completions--notice-admissible-p application notice)))
                     (task-completion-pending agent :continuation-only-p t)))
      (let ((context (mission-context-find (application-conversation application))))
        (when (mission-goal-p (application-goal application))
          (unless context
            (mission--reject ':completion-mission "The completion's mission authority is unavailable."))
          (mission--admit context)))
      ;; Delivery may have won a busy request boundary after ticket construction.
      (when (some (lambda (notice)
                    (application-job-completions--notice-admissible-p application notice))
                  (task-completion-deliver agent))
        (let ((goal (application-goal application)))
          (when goal
            (incf (getf goal :continuations 0))
            (application--record-goal application)))
        (application-input-controller-call-with-primary-steering
         controller
         (lambda (take acknowledge)
           (application--run-turn
            application
            *application-job-completion-continuation-prompt*
            :continuation-p t
            :steering-function take
            :steering-persisted-function acknowledge
            :pending-operations-function
            (lambda (owner)
              (application-input-controller--apply-pending-commands controller :agent owner))))))))
  nil)

(-> application-job-completions-connect (application) null)
(defun application-job-completions-connect (application)
  "Restore completion continuity and connect only this primary controller's wakeup."
  (let ((agent (and (slot-boundp application 'agent) (application-agent application)))
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
