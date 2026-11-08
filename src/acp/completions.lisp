(in-package #:autolith)

;;;; -- Headless Completion Controller --

(-> acp-completion-wake (acp-session) null)
(defun acp-completion-wake (session)
  "Signal SESSION's controller without doing wire or durable work in a job callback."
  (with-lock-held ((acp-session-lock session))
    (setf (acp-session-completion-wakeup-p session) t)
    (condition-notify (acp-session-completion-condition session)))
  nil)

(-> acp-completion--admitted-notices (acp-session) list)
(defun acp-completion--admitted-notices (session)
  "Select undelivered continuation notices with current conversation and goal authority."
  (let ((application (acp-session-application session)))
    (remove-if-not
     (lambda (notice)
       (application-job-completions--notice-admissible-p application notice))
     (task-completion-pending (application-agent application) :continuation-only-p t))))

(-> acp-completion--prepare-turn (acp-session) boolean)
(defun acp-completion--prepare-turn (session)
  "Recheck goal authority and durably deliver a batch after primary admission."
  (let* ((application (acp-session-application session))
         (agent       (application-agent application))
         (notices     (acp-completion--admitted-notices session)))
    (when notices
      (when (mission-goal-p (application-goal application))
        (let ((context (mission-context-find (application-conversation application))))
          (unless context
            (mission--reject ':completion-mission "The completion's mission authority is unavailable."))
          (mission--admit context)))
      (when (some (lambda (notice)
                    (application-job-completions--notice-admissible-p application notice))
                  (task-completion-deliver agent))
        (when (application-goal application)
          (incf (getf (application-goal application) :continuations 0))
          (application--record-goal application))
        t))))

(-> acp-completion--idle-delay (acp-session) (option real))
(defun acp-completion--idle-delay (session)
  "Return the idle compaction delay when ACP has no higher-priority work."
  (application-idle-compaction-delay
   (acp-session-application session)))

(-> acp-completion--idle-ready-locked-p (application-input-controller) boolean)
(defun acp-completion--idle-ready-locked-p (controller)
  "Check native idle admission while holding CONTROLLER's lock."
  (and (not (application-input-controller-active-p controller))
       (not (application-input-controller-stopping-p controller))
       (not (application-input-controller-failure controller))
       (not (application-input-controller-queued-work-paused-p controller))
       (null (application-input-controller-follow-up-edit-index controller))
       (every #'deque-empty-p
              (list (application-input-controller-work-items controller)
                    (application-input-controller-initial-work-items controller)
                    (application-input-controller-pending-apply-items controller)
                    (application-input-controller-steering-items controller)
                    (application-input-controller-steering-in-flight-items controller)))
       t))

(-> acp-completion--run-idle-compaction (acp-session) boolean)
(defun acp-completion--run-idle-compaction (session)
  "Consume one idle opportunity under the ACP primary lease and input admission lock."
  (let* ((application (acp-session-application session))
         (agent       (application-agent application))
         (controller  (acp-input-controller session))
         (episode     (application-idle-compaction-episode application))
         (claimed-p   nil))
    (when (and episode controller)
      (acp-service--call-with-operation
       (acp-session-service session) session
       (lambda ()
         (unwind-protect
              (progn
                (with-lock-held ((acp-session-lock session))
                  (with-lock-held ((application-input-controller-lock controller))
                    (when (and (not (acp-session-closing-p session))
                               (not (acp-session-closed-p session))
                               (not (acp-session-cancelled-p session))
                               (null (acp-session-prompt-thread session))
                               (acp-completion--idle-ready-locked-p controller)
                               (application-idle-compaction-consume application episode))
                      (incf (acp-session-epoch session))
                      (setf (acp-session-prompt-thread session) (current-thread)
                            (acp-session-prompt-interruptible-p session) t
                            (slot-value controller 'main-thread) (current-thread)
                            (application-input-controller-active-p controller) t
                            (application-input-controller-active-work-kind controller) ':idle-compaction
                            (application-input-controller-primary-steering-p controller) nil
                            claimed-p t))))
                (when claimed-p
                  (let ((*active-application* application)
                        (*configuration*      (application-configuration application))
                        (*default-pathname-defaults*
                          (config :working-directory (application-configuration application))))
                    (handler-case
                        (agent-compact-conversation agent (acp-observer-create session))
                      (provider-context-budget-error (condition)
                        (acp-session--call-with-finalization
                         session
                         (lambda ()
                           (acp-session-check-cancelled session)
                           (application-present-context-refusal application condition))))))))
           (when claimed-p
             (acp-session--call-with-finalization
              session
              (lambda ()
                (unwind-protect
                     (application-input-controller-finish-external-work controller)
                  (with-lock-held ((acp-session-lock session))
                    (setf (acp-session-prompt-thread session) nil)))))
             (acp-extension-notify session "state")
             (acp-completion-wake session))))))
    claimed-p))

(-> acp-completion--cycle (acp-session) (option real))
(defun acp-completion--cycle (session)
  "Run ready user or completion work before idle compaction; return the next bounded delay."
  (when (with-lock-held ((acp-session-lock session))
          (or (acp-session-cancelled-p session)
              (acp-session-completion-report session)
              (acp-session-closing-p session)
              (acp-session-closed-p session)))
    (return-from acp-completion--cycle nil))
  (let* ((application  (acp-session-application session))
         (agent        (application-agent application))
         (notices      (acp-completion--admitted-notices session))
         (notice-delay (and notices
                            (max 0 (- (reduce #'min notices :key (lambda (notice) (getf notice :ready-at)))
                                      (get-universal-time)))))
         (idle-delay   (acp-completion--idle-delay session)))
    (cond
      ((acp-input--pending-commands-p session)
       (acp-input--run-pending-commands session)
       0)
      ((acp-input--pending-p session)
       (acp-session-run-turn session "" :queued-p t
                             :prepare             (lambda () (acp-input--prepare-queued-turn session)))
       0)
      ((and notice-delay (zerop notice-delay))
       (acp-session-run-turn
        session *application-job-completion-continuation-prompt*
        :automatic-p t :prepare (lambda () (acp-completion--prepare-turn session)))
       0)
      ((and idle-delay (zerop idle-delay))
       (if (acp-completion--run-idle-compaction session) 0 1))
      ((task-completion-maintenance-needed-p agent)
       (acp-service--call-with-operation
        (acp-session-service session) session
        (lambda ()
          (let ((identifiers
                  (loop for notice in (task-completion-pending agent)
                        unless (application-job-completions--notice-admissible-p application notice)
                          collect (getf notice :id))))
            (when identifiers
              (task-completion-deliver agent :identifiers identifiers)))))
       0)
      (t
       (let ((delays (remove nil (list notice-delay idle-delay))))
         (when delays (apply #'min delays)))))))

(-> acp-completion--run (acp-session) null)
(defun acp-completion--run (session)
  "Own the session's wakeup loop, pausing on failure rather than retrying inference."
  (let* ((*standard-output* *error-output*)
         (*trace-output*    *error-output*)
         (*standard-input*  (make-string-input-stream ""))
         (*terminal-io*     (make-two-way-stream *standard-input* *error-output*))
         (*query-io*        *terminal-io*)
         (*debug-io*        *terminal-io*)
         (delay             nil))
    (loop
      (with-lock-held ((acp-session-lock session))
        (unless (or (acp-session-completion-wakeup-p session)
                    (acp-session-closing-p session)
                    (acp-service-closed-p (acp-session-service session)))
          (condition-wait (acp-session-completion-condition session)
                          (acp-session-lock session) :timeout delay))
        (when (or (acp-session-closing-p session)
                  (acp-session-closed-p session)
                  (acp-service-closed-p (acp-session-service session)))
          (return))
        (setf (acp-session-completion-wakeup-p session) nil))
      (setf delay
            (handler-case
                (progn
                  (acp-extension-notify session "jobs")
                  (acp-completion--cycle session))
              (acp-operation-busy ()
                0.1)
              (application-turn-cancelled ()
                nil)
              (serious-condition (condition)
                (with-lock-held ((acp-session-lock session))
                  (setf (acp-session-completion-report session)
                        (let ((report (princ-to-string condition)))
                          (subseq report 0 (min 2000 (length report))))))
                (acp-extension-notify session "state")
                nil)))))
  nil)

(-> acp-completion-start (acp-session) null)
(defun acp-completion-start (session)
  "Attach one owned completion controller after session construction or replay succeeds."
  (with-lock-held ((acp-session-cleanup-lock session))
    (unless (or (acp-session-completion-thread session)
                (acp-session-closing-p session)
                (acp-session-closed-p session))
      (let ((agent (application-agent (acp-session-application session))))
        (handler-case
            (progn
              (task-completion-connect agent (lambda () (acp-completion-wake session)))
              (with-lock-held ((acp-session-lock session))
                (setf (acp-session-completion-wakeup-p session) t)
                (setf (acp-session-completion-thread session)
                      (make-thread (lambda () (acp-completion--run session))
                                   :name "Autolith ACP completion controller"))))
          (serious-condition (condition)
            (task-completion-disconnect agent)
            (error condition))))))
  nil)

(-> acp-completion-stop (acp-session) null)
(defun acp-completion-stop (session)
  "Detach wakeups and join the controller after closing and cancellation are published."
  (task-completion-disconnect (application-agent (acp-session-application session)))
  (acp-completion-wake session)
  (let ((thread   (acp-session-completion-thread session))
        (deadline (+ (get-internal-real-time)
                     (* *acp-session-close-seconds* internal-time-units-per-second))))
    (when thread
      (unless (eq thread (current-thread))
        (loop while (thread-alive-p thread)
              do (when (>= (get-internal-real-time) deadline)
                   (error 'agentcomms:acp-state-error
                          :message "The completion controller has not finished cancelling."))
                 (sleep 0.01))
        (join-thread thread))
      (setf (acp-session-completion-thread session) nil)))
  nil)
