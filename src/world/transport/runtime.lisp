(in-package #:autolith)

(defclass recoverable-world-endpoint (image-daemon:daemon-runtime)
  ((failure-thread
    :initform      nil
    :accessor      world-endpoint-failure-thread
    :documentation "The failed listener generation pending owner recovery.")
   (failure
    :initform      nil
    :accessor      world-endpoint-failure
    :type          (option serious-condition)
    :documentation "The pending listener failure, protected by the server lock.")
   (failure-backtrace
    :initform      nil
    :accessor      world-endpoint-failure-backtrace
    :type          list
    :documentation "The backtrace captured by the failed listener."))
  (:documentation "A world endpoint whose listener can be replaced by the owner."))

(defclass channel-endpoint (recoverable-world-endpoint)
          ((application :initarg :application :reader channel-endpoint-application :type
            application :documentation "The primary application exposed by this endpoint.")
           (paused-p :initform nil :accessor channel-endpoint-paused-p :type boolean
            :documentation "Whether queued primary work must wait for explicit input.")
           (closing-p :initform nil :accessor channel-endpoint-closing-p :type boolean
            :documentation
            "Whether a close was committed while no terminal controlled the session, so no terminal may attach now.")
           (checkpoint-reconnect-id :initarg :checkpoint-reconnect-id :initform nil :accessor
            channel-endpoint-checkpoint-reconnect-id :type (option string) :documentation
            "The checkpoint transition that created this endpoint.")
           (owner-thread :initform (current-thread) :reader channel-endpoint-owner-thread
            :documentation "The serving thread that published this endpoint."))
          (:documentation "One authenticated local control endpoint for a primary application."))

(-> world-endpoint-directory (configuration) pathname)
(defun world-endpoint-directory (configuration)
  "Return the ephemeral endpoint directory for this image selection.

Durable catalogues and channel writer leases are shared across selections."
  (world--endpoint-directory (config :state-root configuration)))

(-> world--endpoint-directory (pathname) pathname)
(defun world--endpoint-directory (state-root)
  "Return this image selection's ephemeral endpoint directory below STATE-ROOT."
  (let ((root (merge-pathnames "world/endpoints/" state-root)))
    (if *world-image-selection*
        (progn
          (conversation-identifier-validate-path-component *world-image-selection*)
          (merge-pathnames (format nil "~A/" *world-image-selection*) root))
        root)))

(-> world-channel-endpoint-directory (configuration) pathname)
(defun world-channel-endpoint-directory (configuration)
  "Return this image selection's ephemeral channel endpoint directory."
  (merge-pathnames "channels/" (world-endpoint-directory configuration)))

(-> world-channel-endpoint-pathname (configuration string) pathname)
(defun world-channel-endpoint-pathname (configuration session-id)
  "Return the discovery pathname for SESSION-ID under CONFIGURATION."
  (image-daemon:daemon-registry-pathname
   (world-channel-endpoint-directory configuration) session-id))

(defmethod image-daemon:daemon-runtime-record ((session channel-endpoint))
  "Return the discovery record for SESSION, including the checkpoint reconnect identifier if set."
  (append (call-next-method)
          (when (channel-endpoint-checkpoint-reconnect-id session)
            (list :checkpoint-reconnect-id
                  (channel-endpoint-checkpoint-reconnect-id session)))))

(-> world-terminal--registry-record (channel-endpoint) list)
(defun world-terminal--registry-record (session)
  "Return the authenticated discovery record for SESSION."
  (image-daemon:daemon-runtime-record session))

(-> world-terminal--publish-registry (channel-endpoint) null)
(defun world-terminal--publish-registry (session)
  "Publish SESSION's authenticated discovery record."
  (image-daemon:daemon-registry-publish
   (image-daemon:daemon-runtime-registry-pathname session)
   (world-terminal--registry-record session))
  nil)

(-> world-terminal--delete-owned-registry (channel-endpoint) null)
(defun world-terminal--delete-owned-registry (session)
  "Delete SESSION's registry record only while it still names this endpoint."
  (image-daemon:daemon-registry-delete-matching
   (image-daemon:daemon-runtime-registry-pathname session)
   (world-terminal--registry-record session))
  nil)

(-> application-world-terminal-sync-conversation (application) null)
(defun application-world-terminal-sync-conversation (application)
  "Rekey APPLICATION's endpoint to its active conversation without reconnecting clients.

Publication failure restores the old endpoint identity so the surrounding
conversation transaction can roll back before releasing either lease."
  (let ((session    (application-channel-endpoint application))
        (identifier (conversation-identifier (application-conversation application))))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (unless (string= identifier (image-daemon:daemon-runtime-identifier session))
          (let ((old-identifier (image-daemon:daemon-runtime-identifier session))
                (old-pathname   (image-daemon:daemon-runtime-registry-pathname session))
                (old-record     (world-terminal--registry-record session))
                (completed-p    nil))
            (unwind-protect
                (progn
                 (setf (image-daemon:daemon-runtime-identifier session) identifier
                       (image-daemon:daemon-runtime-registry-pathname session)
                       (world-channel-endpoint-pathname
                        (application-configuration application) identifier))
                 (world-terminal--publish-registry session)
                 (image-daemon:daemon-registry-delete-matching
                  old-pathname old-record :errorp t)
                 (setf completed-p t))
              (unless completed-p
                (world-terminal--delete-owned-registry session)
                (setf (image-daemon:daemon-runtime-identifier session) old-identifier
                      (image-daemon:daemon-runtime-registry-pathname session)
                      old-pathname))))))))
  (application-mission-schedules-notify application)
  nil)

(-> world-terminal-endpoint-records (configuration) list)
(defun world-terminal-endpoint-records (configuration)
  "Return CONFIGURATION's discovery records ordered by newest publication."
  (image-daemon:daemon-registry-discover
   (world-channel-endpoint-directory configuration)))

(-> world-terminal-reconcile-endpoint-records (configuration) (integer 0))
(defun world-terminal-reconcile-endpoint-records (configuration)
  "Remove definitely dead endpoint records beneath CONFIGURATION and return their count."
  (image-daemon:daemon-registry-reconcile
   (world-channel-endpoint-directory configuration)))

(-> application-world-terminal-paused-p (application) boolean)
(defun application-world-terminal-paused-p (application)
  "Return true when APPLICATION is deliberately holding queued work."
  (let ((session (application-channel-endpoint application)))
    (and session
         (not
          (null
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (channel-endpoint-paused-p session)))))))

(-> application-world-terminal-resume (application) boolean)
(defun application-world-terminal-resume (application)
  "Resume APPLICATION's queued work and report whether it had been paused."
  (let ((session (application-channel-endpoint application)) (resumed-p nil))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf resumed-p (channel-endpoint-paused-p session)
              (channel-endpoint-paused-p session) nil))
      (when resumed-p
        (when-let ((controller (application-input-controller application)))
          (with-lock-held ((application-input-controller-lock controller))
            (sb-thread:condition-broadcast
             (application-input-controller-condition-variable controller))))))
    (not (null resumed-p))))

(-> application-world-terminal-pause (application &key (:reason keyword)) boolean)
(defun application-world-terminal-pause (application &key (reason ':cancelled))
  "Hold queued work before requesting active-turn cancellation with REASON."
    (let ((session    (application-channel-endpoint application))
          (controller (application-input-controller application)))
      (unless (and session controller) (return-from application-world-terminal-pause nil))
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf (channel-endpoint-paused-p session) t))
      (unless (application-assignment-cancel application :reason reason)
        (application-input-controller--request-active-turn-cancellation controller))
      (with-lock-held ((application-input-controller-lock controller))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))
      t))

(-> world-terminal--task-counts (application) (values (integer 0) (integer 0) (integer 0)))
(defun world-terminal--task-counts (application)
  "Return live children, active children, and all live jobs for APPLICATION."
  (if-let ((orchestrator (application-task-presentation-orchestrator application)))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (values (task-orchestrator-live-count orchestrator)
                (task-orchestrator-active-count orchestrator)
                (task-orchestrator-session-live-count orchestrator)))
      (values 0 0 0)))

(-> application-terminal-attached-p (application) boolean)
(defun application-terminal-attached-p (application)
  "Return true while a terminal shows APPLICATION: its own, or that of a client that controls its presentation."
  (let ((terminal (and (slot-boundp application 'ui)
                       (application-ui application)
                       (terminal-ui-terminal (application-ui application)))))
    (and terminal (terminal-interactive-p terminal) t)))

(-> world-terminal-status-snapshot (channel-endpoint) list)
(defun world-terminal-status-snapshot (session)
  "Return a portable, internally consistent status snapshot for SESSION."
  (let* ((application       (channel-endpoint-application session))
         (configuration     (application-configuration application))
         (conversation      (application-conversation application))
         (terminal          (terminal-ui-terminal (application-ui application)))
         (client-pids       (multiple-value-list
                             (if (application-presentation application)
                                 (application-presenting-client-pids application)
                                 (and (terminal-interactive-p terminal) (current-process-id)))))
         (controller        (application-input-controller application))
         (paused-p          (application-world-terminal-paused-p application))
         (active-p          nil)
         (queued-count      0)
         (steering-count    0)
         (recalled-p        nil)
         (stopping-p        t)
         (cancelling-p      nil)
         (reader-paused-p   nil)
         (failed-p          nil)
         (task-live-count   0)
         (task-active-count 0)
         (job-live-count    0))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (setf active-p (application-input-controller-active-p controller)
              queued-count
                (deque-count (application-input-controller-work-items controller))
              steering-count
                (deque-count (application-input-controller-steering-items controller))
              recalled-p
                (not
                 (null (application-input-controller-follow-up-edit-work controller)))
              stopping-p (application-input-controller-stopping-p controller)
              cancelling-p (application-input-controller-turn-cancellation-p controller)
              reader-paused-p (application-input-controller-reader-paused-p controller)
              failed-p (not (null (application-input-controller-failure controller))))))
    (multiple-value-setq (task-live-count task-active-count job-live-count)
      (world-terminal--task-counts application))
    (let* ((waiting-for-input-p
             (and controller (not active-p) (zerop queued-count) (zerop steering-count)
                  (not recalled-p) (not stopping-p) (not cancelling-p)
                  (not reader-paused-p) (not failed-p)))
           (idle-p (and waiting-for-input-p (not paused-p) (zerop job-live-count)))
           (state
            (cond (stopping-p
                   ':stopping)
                  (failed-p
                   ':failed)
                  (paused-p
                   ':paused)
                  (cancelling-p
                   ':cancelling)
                  (active-p
                   ':active)
                  ((or (plusp queued-count) (plusp steering-count) recalled-p
                        (plusp job-live-count))
                   ':working)
                  (idle-p
                   ':idle)
                  (t
                   ':starting))))
      (list :world-terminal-status :version *daemon-protocol-version* :session-id
            (conversation-identifier conversation) :pid (current-process-id)
            :terminal-client-pid (first client-pids) :observer-client-pids (second client-pids)
            :autolith-version *autolith-version* :state state :idle-p (not (null idle-p))
            :waiting-for-input-p (not (null waiting-for-input-p)) :paused-p
              (not (null paused-p)) :cwd
            (namestring (config :working-directory configuration)) :conversation-id
            (conversation-identifier conversation) :conversation-display-id
            (conversation-identifier-display (conversation-identifier conversation))
            :conversation-title (conversation-title conversation)
            :conversation-persisted-p
            (not (null (conversation-persisted-p conversation))) :model
            (config :model configuration) :reasoning-effort
            (config :reasoning-effort configuration) :permission-mode
            (application-permission-mode application) :active-turn-p
            (not (null active-p)) :queued-input-count queued-count :steering-input-count
            steering-count :recalled-input-p (not (null recalled-p)) :turn-cancelling-p
            (not (null cancelling-p)) :reader-paused-p (not (null reader-paused-p))
            :task-live-count task-live-count :task-active-count task-active-count
           :job-live-count      job-live-count
            :message-count      (conversation-message-count conversation)
            :mention-count
            (if-let ((person (application-world-human-identifier application)))
                (gethash person (conversation-channel-mention-counts conversation) 0)
                0)
            :terminal-attached-p
            (application-terminal-attached-p application)
            :terminal-attachment
            (cond
              ((not (terminal-interactive-p terminal))
               ':detached)
              ((application-presentation application)
               ':remote)
              (t
               ':foreground))
            :observer-count     (length (second client-pids))
            :created-at         (image-daemon:daemon-runtime-created-at session)))))

;;;; -- World-Terminal Terminal Ownership --


(-> channel-endpoint-endpoint-live-p (channel-endpoint) boolean)
(defun channel-endpoint-endpoint-live-p (session)
  "Return whether SESSION owns a healthy listener and has not committed to close.

Startup and checkpoint pause are separate lifecycle states; use this predicate
for an endpoint already published as ready."
  (and (image-daemon:daemon-runtime-live-p session)
       (with-lock-held ((image-daemon:daemon-runtime-lock session))
         (and (not (channel-endpoint-closing-p session))
              (channel-endpoint-serving-p session)))))

(-> channel-endpoint-serving-p (channel-endpoint) boolean)
(defun channel-endpoint-serving-p (session)
  "Return whether SESSION's owner and input controller can serve it.

Call with the runtime lock held; do not acquire the controller or world server
lock. Controller callbacks take the runtime lock while holding their own lock.
Sample its monotonic stopping flag alongside its failure state instead."
  (let* ((application (channel-endpoint-application session))
         (controller  (application-input-controller application))
         (owner       (channel-endpoint-owner-thread session)))
    (and owner (thread-alive-p owner)
         (null (image-daemon:daemon-runtime-failure session))
         controller (null (application-input-controller-failure controller))
         (not (application-input-controller-stopping-p controller)) t)))

(-> channel-endpoint-admits-attachment-p (channel-endpoint) boolean)
(defun channel-endpoint-admits-attachment-p (session)
  "Return true while SESSION admits a new terminal: it is neither stopping nor committed to close.

Call it with SESSION's runtime lock held, the lock a committed close takes
before it checks for a controlling terminal, so no terminal is admitted
between that check and the close."
  (and (not (image-daemon:daemon-runtime-stopping-p session))
       (not (channel-endpoint-closing-p session))
       (channel-endpoint-serving-p session)))

(-> world-terminal--terminal-event-p (t) boolean)
(defun world-terminal--terminal-event-p (event)
  "Accept a client's keys, bounded pointer events and the side panel's channel commands."
  (not (null (or (image-daemon:daemon-terminal-event-p event)
                 (channel-command-text event)
                 (typep event '(cons (eql :scroll) (cons (member -1 1) null)))
                 (typep event '(cons (member :click :drag :release)
                                (cons (integer 1 10000)
                                 (cons (integer 1 10000) null))))))))

(-> world-terminal--detach-terminal (channel-endpoint) list)
(defun world-terminal--detach-terminal (session)
  "Tell the controlling terminal client to leave SESSION's channel running."
  (let ((application (channel-endpoint-application session))
        (identifier  (image-daemon:daemon-runtime-identifier session)))
    (unless (application-client-tell application ':detach)
      (error 'world-terminal-error
             :message    "No terminal shows this channel."
             :operation  ':detach
             :session-id identifier))
    (list :ok :operation :detach :scheduled-p nil :session-id identifier)))

(-> world-terminal-finish-attachments
    ((option application)
     &key (:status (integer 0 255)) (:message (option string)) (:details list))
    boolean)
(defun world-terminal-finish-attachments (application &key status message details)
  "Tell the terminals that show APPLICATION's channel that the session exits with STATUS and MESSAGE.

A detached session's own launcher only logs its exit status, so the terminal
that shows it must exit with it instead for the outer launcher to update,
roll back, or recover. DETAILS carries what that launcher needs besides the
status, such as the session's crash capsule. Each terminal is let go once it
has the word, since the endpoint closes next. Return true when APPLICATION's
clients present it; one that already left costs nothing."
  (when (and application (application-presentation application))
    (application-clients-finish-presenting
     application ':exit :status status :message message :details details)
    t))


;;;; -- World-Terminal Request Handling --

(-> world-terminal--request-field (list keyword) t)
(defun world-terminal--request-field (request key)
  "Return KEY from REQUEST's property list."
  (getf (rest request) key))

(-> world-terminal--valid-request-p (t channel-endpoint) boolean)
(defun world-terminal--valid-request-p (request session)
  "Validate REQUEST against SESSION's image-daemon capability."
  (and (image-daemon:daemon-request-valid-p
        request (image-daemon:daemon-runtime-token session)
        :request-tag ':world-terminal-request)
       t))

(defmethod image-daemon:daemon-runtime-request-valid-p ((session channel-endpoint) request)
  "Accept this channel's authenticated terminal protocol at the transport boundary."
  (world-terminal--valid-request-p request session))

(-> world-terminal--dispatch-request (channel-endpoint list) list)
(defun world-terminal--dispatch-request (session request)
  "Return SESSION's response to one authenticated REQUEST."
  (unless (world-terminal--valid-request-p request session)
    (return-from world-terminal--dispatch-request
      (list :error :message "The world-terminal request was rejected.")))
  (let ((operation (world-terminal--request-field request :operation)))
    (case operation
      (:status
       (list :ok :status (world-terminal-status-snapshot session)))
      (:pause
       (if (application-world-terminal-pause (channel-endpoint-application session))
           (list :ok :operation :pause :session-id
                 (image-daemon:daemon-runtime-identifier session))
           (list :error :message
                 "The world-terminal session cannot pause while detaching.")))
      (:detach
       (world-terminal--detach-terminal session))
      (:kill
       (list :ok :operation :kill :session-id
             (image-daemon:daemon-runtime-identifier session)))
      (otherwise
       (list :error :message
             (format nil "Unknown world-terminal operation ~S." operation))))))

(-> world-terminal-start
    (application
     &key (:token (option string)) (:created-at (option integer))
          (:checkpoint-reconnect-id (option string)))
    channel-endpoint)
(defun world-terminal-start
    (application &key token created-at checkpoint-reconnect-id)
  "Publish APPLICATION's hosted channel endpoint independently of terminal lifetime."
  (let ((configuration (application-configuration application))
        (session       nil)
        (completed-p   nil))
    (unwind-protect
         (progn
           (setf session
                 (image-daemon:daemon-runtime-create
                  :directory        (world-channel-endpoint-directory configuration)
                  :identifier       (conversation-identifier (application-conversation application))
                  :token            token
                  :created-at       created-at
                  :class            'channel-endpoint
                  :initargs         (list :application             application
                                          :checkpoint-reconnect-id checkpoint-reconnect-id)
                  :request-function #'world-terminal--handle-request
                  :error-function   (lambda (condition)
                                      (world-terminal--endpoint-failed session condition)))
                 (application-channel-endpoint application) session)
           (let ((hub (application-client-hub application)))
             (with-lock-held ((client-hub-lock hub))
               (setf (client-hub-admitting-p hub) t)))
           (application-mission-schedules-host application session)
           (image-daemon:daemon-runtime-start session)
           (setf completed-p t)
           session)
      (unless completed-p
        (setf (application-channel-endpoint application) nil)
        (when session (image-daemon:daemon-runtime-stop session))))))

(-> world-terminal--endpoint-failed (channel-endpoint serious-condition) null)
(defun world-terminal--endpoint-failed (session condition)
  "Report the listener failure to the hosted owner or end the direct session."
  (if-let ((server (application-world-server (channel-endpoint-application session))))
    (world-server--endpoint-failed server session condition)
    (world-terminal--endpoint-fatal session condition (application-safe-backtrace)))
  nil)

(-> world-terminal--endpoint-fatal (channel-endpoint serious-condition list) null)
(defun world-terminal--endpoint-fatal (session condition backtrace)
  "End SESSION through the application fatal path and preserve accepted input."
  (let* ((application (channel-endpoint-application session))
         (controller  (application-input-controller application)))
    (when (and (eq session (application-channel-endpoint application))
               (not (image-daemon:daemon-runtime-stopping-p session))
               (not (channel-endpoint-closing-p session)))
      (if controller
          (unless (application-input-controller-stopping-p controller)
            (application-input-controller--record-failure controller condition backtrace))
          (format *error-output* "~&World-Terminal endpoint failed: ~A~%"
                  (condition--report-text condition)))))
  nil)

(-> world-terminal--message-dispatch (application list) t)
(defun world-terminal--message-dispatch (application envelope)
  "Answer one daemon message for APPLICATION: a channel's new inbox, or a mission event for its conversation.

Any other message is rejected, as a request this process has no use for."
  (case (getf envelope :action)
    (:channel
     (list :ok :queued (application-world-deliver application)))
    (:event
     (unless (and (equal (getf envelope :receiver)
                         (conversation-identifier (application-conversation application)))
                  (stringp (getf envelope :payload)))
       (world-terminal--reject-message ':event-scope))
     (application-mission-schedule-event application (getf envelope :payload) (getf envelope :id)))
    (otherwise
     (world-terminal--reject-message ':unsupported))))

(-> world-terminal--reject-message (keyword) nil)
(defun world-terminal--reject-message (reason)
  "Reject a daemon message for REASON without echoing its payload."
  (error 'image-daemon:daemon-message-error
         :reason    reason
         :operation ':message
         :message   (format nil "Daemon message rejected: ~(~A~)." reason)))

(-> world-terminal--handle-request
    (channel-endpoint list &key (:socket t) (:stream t))
    null)
(defun world-terminal--handle-request (session request &key socket stream)
  "Dispatch authenticated requests according to Autolith's command and client policy."
  (case (world-terminal--request-field request :operation)
    (:client
     (world-terminal--serve-client session request :socket socket :stream stream))
    (:message
     (image-daemon:daemon-message-handle-request
      session request :stream stream
      :dispatcher
      (lambda (envelope &key cancelled-p)
        (declare (ignore cancelled-p))
        (world-terminal--message-dispatch (channel-endpoint-application session) envelope))
      :character-limit 32768 :request-timeout 2))
    (otherwise
     (let* ((response (world-terminal--dispatch-request session request))
            (kill-request-p
              (and (eq (world-terminal--request-field request :operation) ':kill)
                   (eq (first response) ':ok))))
       (daemon-write-packet stream response)
       (when kill-request-p
         (when-let ((controller
                      (application-input-controller
                       (channel-endpoint-application session))))
           (application-input-controller--request-exit
            controller ':world-terminal-kill))))))
  nil)

(define-condition world-terminal-stop-error (error)
  ((session :initarg :session :reader world-terminal-stop-error-session
            :documentation "The endpoint whose cleanup remains owned.")
   (failures :initarg :failures :reader world-terminal-stop-error-failures
             :documentation "Ordered cleanup stage and condition pairs."))
  (:documentation "Endpoint cleanup failed and can be retried.")
  (:report (lambda (condition stream)
             (format stream "World-Terminal cleanup failed in ~{~A~^, ~}; retry stop to finish cleanup."
                     (mapcar #'first (world-terminal-stop-error-failures condition))))))

(-> world-terminal-stop (application) null)
(defun world-terminal-stop (application)
  "Stop the channel endpoint and retain ownership if cleanup fails."
  (when-let ((session (application-channel-endpoint application)))
    (handler-case (image-daemon:daemon-runtime-stop session)
      (error (condition)
        (error 'world-terminal-stop-error :session session
              :failures                            (list (list ':runtime condition)))))
    (when (eq session (application-channel-endpoint application))
      (setf (application-channel-endpoint application) nil)))
  nil)
