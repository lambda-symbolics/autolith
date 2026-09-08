(in-package #:autolith)

;;;; -- Application Session State --

(defclass localgroup-session (image-daemon:daemon-runtime)
  ((application
    :initarg :application
    :reader localgroup-session-application
    :type application
    :documentation "The primary application exposed by this endpoint.")
   (paused-p
    :initform nil
    :accessor localgroup-session-paused-p
    :type boolean
    :documentation "Whether queued primary work must wait for explicit input.")
   (handoff-mode
    :initform nil
    :accessor localgroup-session-handoff-mode
    :type (option keyword)
    :documentation "The detach or take-over process handoff waiting for strict idle.")
   (handoff-running-p
    :initform nil
    :accessor localgroup-session-handoff-running-p
    :type boolean
    :documentation "Whether a detached replacement is being started.")
   (detached-explicitly-p
    :initarg :detached-explicitly-p
    :initform nil
    :accessor localgroup-session-detached-explicitly-p
    :type boolean
    :documentation "Whether the controlling terminal left through an explicit detach.")
   (controller-seen-p
    :initform nil
    :accessor localgroup-session-controller-seen-p
    :type boolean
    :documentation "Whether any controlling client has ever attached to this endpoint.")
   (attach-watchdog-thread
    :initform nil
    :accessor localgroup-session-attach-watchdog-thread
    :type t
    :documentation "The bounded thread exiting a launch nobody attaches to."))
  (:documentation "One authenticated local control endpoint for a primary application."))

(-> localgroup-registry-directory (configuration) pathname)
(defun localgroup-registry-directory (configuration)
  "Return CONFIGURATION's private localgroup endpoint directory."
  (merge-pathnames "localgroup/" (configuration-state-root configuration)))

(defun localgroup-registry-pathname (configuration session-id)
  "Return the discovery pathname for SESSION-ID under CONFIGURATION."
  (image-daemon:daemon-registry-pathname
   (localgroup-registry-directory configuration) session-id))


(-> localgroup--registry-record (localgroup-session) list)
(defun localgroup--registry-record (session)
  "Return SESSION's private endpoint discovery record."
  (image-daemon:daemon-runtime-record session))

(defun localgroup--publish-registry (session)
  "Publish SESSION's authenticated discovery record."
  (image-daemon:daemon-registry-publish
   (image-daemon:daemon-runtime-registry-pathname session)
   (localgroup--registry-record session)))

(-> localgroup--delete-owned-registry (localgroup-session) null)

(defun localgroup--delete-owned-registry (session)
  "Delete SESSION's registry record only while it still names this endpoint."
  (image-daemon:daemon-registry-delete-matching
   (image-daemon:daemon-runtime-registry-pathname session)
   (localgroup--registry-record session))
  nil)

(-> application-localgroup-sync-conversation (application) null)

(defun application-localgroup-sync-conversation (application)
  "Rekey APPLICATION's endpoint to its active conversation without reconnecting clients.

Publication failure restores the old endpoint identity so the surrounding
conversation transaction can roll back before releasing either lease."
  (let ((session (application-localgroup-session application))
        (identifier (conversation-identifier (application-conversation application))))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (unless (string= identifier (image-daemon:daemon-runtime-identifier session))
          (let ((old-identifier (image-daemon:daemon-runtime-identifier session))
                (old-pathname (image-daemon:daemon-runtime-registry-pathname session))
                (old-record (localgroup--registry-record session))
                (completed-p nil))
            (unwind-protect
                (progn
                 (setf (image-daemon:daemon-runtime-identifier session) identifier
                       (image-daemon:daemon-runtime-registry-pathname session)
                         (localgroup-registry-pathname
                          (application-configuration application) identifier))
                 (localgroup--publish-registry session)
                 (image-daemon:daemon-registry-call-with-lock old-pathname
                  (lambda ()
                    (when
                        (equal old-record
                               (image-daemon:daemon-registry-read old-pathname))
                      (delete-file old-pathname))))
                 (setf completed-p t))
              (unless completed-p
                (localgroup--delete-owned-registry session)
                (setf (image-daemon:daemon-runtime-identifier session) old-identifier
                      (image-daemon:daemon-runtime-registry-pathname session)
                        old-pathname))))))))
  nil)

(defun localgroup-endpoint-records (configuration)
  "Return CONFIGURATION's discovery records ordered by newest publication."
  (image-daemon:daemon-registry-discover
   (localgroup-registry-directory configuration)))

(defun localgroup-reconcile-endpoint-records (configuration)
  "Remove definitely dead endpoint records beneath CONFIGURATION."
  (image-daemon:daemon-registry-reconcile
   (localgroup-registry-directory configuration)))

(-> application-localgroup-paused-p (application) boolean)

(defun application-localgroup-paused-p (application)
  "Return true when APPLICATION is deliberately holding queued work."
  (let ((session (application-localgroup-session application)))
    (and session
         (not
          (null
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (localgroup-session-paused-p session)))))))

(-> application-localgroup-resume (application) boolean)

(defun application-localgroup-resume (application)
  "Resume APPLICATION's queued work and report whether it had been paused."
  (let ((session (application-localgroup-session application)) (resumed-p nil))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf resumed-p (localgroup-session-paused-p session)
              (localgroup-session-paused-p session) nil))
      (when resumed-p
        (let ((controller (application-input-controller application)))
          (when controller
            (with-lock-held ((application-input-controller-lock controller))
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller)))))))
    (not (null resumed-p))))

(-> application-localgroup-pause (application) boolean)

(defun application-localgroup-pause (application)
  "Pause queued work and request cancellation of APPLICATION's active turn."
  (let ((session (application-localgroup-session application))
        (controller (application-input-controller application)))
    (unless (and session controller) (return-from application-localgroup-pause nil))
    (with-lock-held ((image-daemon:daemon-runtime-lock session))
      (when
          (or (localgroup-session-handoff-mode session)
              (localgroup-session-handoff-running-p session))
        (return-from application-localgroup-pause nil))
      (setf (localgroup-session-paused-p session) t))
    (application-input-controller--request-active-turn-cancellation controller)
    (with-lock-held ((application-input-controller-lock controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    t))

(-> localgroup--task-counts (application) (values (integer 0) (integer 0)))
(defun localgroup--task-counts (application)
  "Return APPLICATION's live and actively running child-job counts."
  (let ((orchestrator (application-task-presentation-orchestrator application)))
    (if orchestrator
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (values (task-orchestrator-live-count orchestrator)
                  (task-orchestrator-active-count orchestrator)))
        (values 0 0))))

(-> localgroup-status-snapshot (localgroup-session) list)

(defun localgroup-status-snapshot (session)
  "Return a portable, internally consistent status snapshot for SESSION."
  (let* ((application (localgroup-session-application session))
         (configuration (application-configuration application))
         (conversation (application-conversation application))
         (controller (application-input-controller application))
         (paused-p (application-localgroup-paused-p application))
         (handoff-p (application-localgroup-handoff-pending-p application))
         (active-p nil)
         (queued-count 0)
         (steering-count 0)
         (recalled-p nil)
         (stopping-p t)
         (cancelling-p nil)
         (reader-paused-p nil)
         (failed-p nil)
         (task-live-count 0)
         (task-active-count 0))
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
    (multiple-value-setq (task-live-count task-active-count)
      (localgroup--task-counts application))
    (let* ((waiting-for-input-p
            (and controller (not active-p) (zerop queued-count) (zerop steering-count)
                 (not recalled-p) (not stopping-p) (not cancelling-p) (not handoff-p)
                 (not reader-paused-p) (not failed-p)))
           (idle-p (and waiting-for-input-p (not paused-p) (zerop task-live-count)))
           (state
            (cond (stopping-p ':stopping) (failed-p ':failed) (handoff-p ':detaching)
                  (paused-p ':paused) (cancelling-p ':cancelling) (active-p ':active)
                  ((or (plusp queued-count) (plusp steering-count) recalled-p
                       (plusp task-live-count))
                   ':working)
                  (idle-p ':idle) (t ':starting))))
      (list :localgroup-status :version *daemon-protocol-version* :session-id
            (conversation-identifier conversation) :pid (sb-posix:getpid)
            :autolith-version *autolith-version* :state state :idle-p (not (null idle-p))
            :waiting-for-input-p (not (null waiting-for-input-p)) :paused-p
            (not (null paused-p)) :handoff-p (not (null handoff-p)) :cwd
            (namestring (configuration-working-directory configuration)) :conversation-id
            (conversation-identifier conversation) :conversation-display-id
            (conversation-identifier-display (conversation-identifier conversation))
            :conversation-title (conversation-title conversation)
            :conversation-persisted-p
            (not (null (conversation-persisted-p conversation))) :model
            (configuration-model configuration) :reasoning-effort
            (configuration-reasoning-effort configuration) :permission-mode
            (application-permission-mode application) :active-turn-p
            (not (null active-p)) :queued-input-count queued-count :steering-input-count
            steering-count :recalled-input-p (not (null recalled-p)) :turn-cancelling-p
            (not (null cancelling-p)) :reader-paused-p (not (null reader-paused-p))
            :task-live-count task-live-count :task-active-count task-active-count
            :terminal-attached-p
            (let ((terminal (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (image-daemon:relay-attached-p terminal)
                  (not (null (terminal-interactive-p terminal)))))
            :terminal-attachment
            (let ((terminal (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (image-daemon:relay-attachment-kind terminal)
                  (if (terminal-interactive-p terminal)
                      ':foreground
                      ':detached)))
            :observer-count
            (let ((terminal (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (image-daemon:relay-observer-count terminal)
                  0))
            :created-at (image-daemon:daemon-runtime-created-at session)))))


;;;; -- Localgroup Terminal Ownership --

(-> localgroup--terminal (localgroup-session) localgroup-terminal)

(defun localgroup--terminal (session)
  "Return SESSION's attachable relay terminal."
  (let ((terminal
         (terminal-ui-terminal
          (application-ui (localgroup-session-application session)))))
    (unless (typep terminal 'localgroup-terminal)
      (error 'localgroup-error :message
             "This Autolith session has no attachable terminal relay." :operation
             ':attach :session-id (image-daemon:daemon-runtime-identifier session)))
    terminal))

(-> localgroup--attachment-mode (list) keyword)
(defun localgroup--attachment-mode (arguments)
  "Return the validated attachment mode selected by ARGUMENTS."
  (let ((mode (or (getf arguments :mode) ':control)))
    (unless (member mode '(:read-only :control :take-over))
      (error 'localgroup-error
             :message "The localgroup attachment mode is invalid."
             :operation ':attach))
    mode))

(defparameter *localgroup-first-attach-timeout-seconds* 60
  "The maximum seconds a launch waits for the client it was started for.")

(-> localgroup--attach-expected-p (list) boolean)
(defun localgroup--attach-expected-p (startup-values)
  "Return true when STARTUP-VALUES promise a controlling client attaches promptly.

Client-first launches and take-over replacements are started for a
terminal that connects right away; a detach handoff is started to live
without one."
  (or (not (null (getf startup-values :attach-expected-p)))
      (eq (getf startup-values :mode) ':take-over)))

(-> localgroup--initially-detached-p (list) boolean)
(defun localgroup--initially-detached-p (startup-values)
  "Return true when STARTUP-VALUES describe a deliberately detached launch."
  (and (not (null startup-values))
       (eq (getf startup-values :mode) ':detach)
       (not (localgroup--attach-expected-p startup-values))))

(-> localgroup--mark-explicit-detach (localgroup-session) null)

(defun localgroup--mark-explicit-detach (session)
  "Record that SESSION's controlling terminal is leaving on purpose."
  (with-lock-held ((image-daemon:daemon-runtime-lock session))
    (setf (localgroup-session-detached-explicitly-p session) t))
  nil)

(-> localgroup--note-controller-attached (localgroup-session) null)

(defun localgroup--note-controller-attached (session)
  "Record that a controlling client owns SESSION's terminal from now on."
  (with-lock-held ((image-daemon:daemon-runtime-lock session))
    (setf (localgroup-session-controller-seen-p session) t
          (localgroup-session-detached-explicitly-p session) nil))
  nil)

(-> localgroup--exit-abandoned-session (localgroup-session) null)
(defun localgroup--exit-abandoned-session (session)
  "Stop SESSION because the terminal it was serving is gone for good."
  (let ((controller (application-input-controller
                     (localgroup-session-application session))))
    (when controller
      (application-input-controller--request-exit
       controller ':localgroup-abandoned)))
  nil)

(-> localgroup--controller-lost (localgroup-session) boolean)

(defun localgroup--controller-lost (session)
  "Handle SESSION losing its controlling client and report whether it exits.

A session with no connection left lingers only when its controller left
through an explicit detach. A closed window, a dropped connection, or a
client that ended without detaching exits the session instead, so
abandoned launches never accumulate as idle background processes. The
exit persists pending input and cancels an active turn the way any
forced shutdown does; the conversation stays resumable."
  (let ((exit-p
         (with-lock-held ((image-daemon:daemon-runtime-lock session))
           (not (localgroup-session-detached-explicitly-p session)))))
    (when exit-p (localgroup--exit-abandoned-session session))
    exit-p))

(-> localgroup--first-attach-overdue-p (localgroup-session) boolean)

(defun localgroup--first-attach-overdue-p (session)
  "Return true when SESSION never met the controlling client it was launched for."
  (let ((terminal
         (terminal-ui-terminal
          (application-ui (localgroup-session-application session)))))
    (and
     (not
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (localgroup-session-controller-seen-p session)))
     (or (not (typep terminal 'localgroup-terminal))
         (not (image-daemon:relay-attached-p terminal))))))

(-> localgroup--attach-watchdog (localgroup-session) null)

(defun localgroup--attach-watchdog (session)
  "Exit SESSION when no controlling client attaches within the first-attach timeout."
  (let ((deadline
         (+ (get-internal-real-time)
            (* *localgroup-first-attach-timeout-seconds*
               internal-time-units-per-second))))
    (loop
     (when
         (with-lock-held ((image-daemon:daemon-runtime-lock session))
           (or (image-daemon:daemon-runtime-stopping-p session)
               (localgroup-session-controller-seen-p session)))
       (return))
     (when (>= (get-internal-real-time) deadline)
       (when (localgroup--first-attach-overdue-p session)
         (localgroup--exit-abandoned-session session))
       (return))
     (sleep 0.25)))
  nil)

(-> localgroup--detach-live-region (application) null)
(defun localgroup--detach-live-region (application)
  "Retract APPLICATION's live terminal rows after controlling ownership ends."
  (let ((ui (application-ui application)))
    (when ui
      (terminal-ui-detach ui)))
  nil)

(-> localgroup--serve-attachment
    (localgroup-session sb-bsd-sockets:socket stream list)
    null)
(defun localgroup--serve-attachment (session socket stream request)
  "Attach REQUEST's persistent SOCKET while coordinating the application reader."
  (unless (localgroup--valid-request-p request session)
    (daemon-write-packet
     stream (list :error :message "The localgroup request was rejected."))
    (return-from localgroup--serve-attachment nil))
  (let* ((arguments (localgroup--request-field request :arguments))
         (mode (localgroup--attachment-mode arguments))
         (rows (or (getf arguments :rows) *terminal-default-rows*))
         (columns (or (getf arguments :columns) *terminal-default-columns*))
         (styled-p (not (null (getf arguments :styled-p))))
         (application (localgroup-session-application session))
         (controller (application-input-controller application))
         (terminal (localgroup--terminal session)))
    (when (and (eq mode ':take-over)
               (eq (image-daemon:relay-attachment-kind terminal) ':foreground))
      (let ((response (application-localgroup-request-handoff application ':take-over)))
        (daemon-write-packet
         stream (list :handoff
                      :session-id (image-daemon:daemon-runtime-identifier session)
                      :old-pid (getf (rest response) :old-pid))))
      (return-from localgroup--serve-attachment nil))
    (let ((attachment (image-daemon:attachment-create socket stream mode)))
      (labels ((attach ()
                 (image-daemon:relay-attach
                  terminal attachment :rows rows :columns columns :styled-p styled-p
                  :session-id (image-daemon:daemon-runtime-identifier session))))
        (unwind-protect
             (let ((attached-p
                     (if (eq mode ':read-only)
                         (attach)
                         (application-input-controller-call-with-reader-paused
                          controller
                          (lambda ()
                            (let ((attached-p (attach)))
                              (when attached-p
                                (application-input-controller--open-prompt-if-ready controller))
                              attached-p))))))
               (unless attached-p
                 (return-from localgroup--serve-attachment nil))
               (unless (eq mode ':read-only)
                 (localgroup--note-controller-attached session))
               (image-daemon:relay-read-attachment terminal attachment))
          (let ((controlled-p (image-daemon:relay-detach terminal attachment)))
            (when controlled-p
              (localgroup--detach-live-region application))
            (image-daemon:attachment-close attachment)
            (when controlled-p
              (localgroup--controller-lost session)))))))
  nil)

(-> localgroup--detach-terminal (localgroup-session) list)

(defun localgroup--detach-terminal (session)
  "Release SESSION's current controlling terminal or schedule foreground detach."
  (let* ((application (localgroup-session-application session))
         (terminal (localgroup--terminal session)))
    (if (eq (image-daemon:relay-attachment-kind terminal) ':foreground)
        (application-localgroup-request-handoff application ':detach)
        (progn
         (localgroup--mark-explicit-detach session)
         (image-daemon:relay-release-control terminal)
         (localgroup--detach-live-region application)
         (list :ok :operation :detach :scheduled-p nil :session-id
               (image-daemon:daemon-runtime-identifier session))))))


;;;; -- Localgroup Request Handling --

(-> localgroup--request-field (list keyword) t)
(defun localgroup--request-field (request key)
  "Return KEY from REQUEST's property list."
  (getf (rest request) key))

(defun localgroup--valid-request-p (request session)
  "Validate REQUEST against SESSION's image-daemon capability."
  (image-daemon:daemon-request-valid-p request (image-daemon:daemon-runtime-token session)))

(-> localgroup--tell (localgroup-session list) list)

(defun localgroup--tell (session arguments)
  "Submit ARGUMENTS' message through the ordinary responsive input path."
  (let* ((application (localgroup-session-application session))
         (controller (application-input-controller application))
         (message (getf arguments :message)))
    (unless (and controller (stringp message))
      (error 'localgroup-error :message "localgroup tell requires one string message."
             :operation ':tell :session-id
             (image-daemon:daemon-runtime-identifier session)))
    (when (application-localgroup-handoff-pending-p application)
      (error 'localgroup-error :message
             "The localgroup session is detaching and no longer accepts input."
             :operation ':tell :session-id
             (image-daemon:daemon-runtime-identifier session)))
    (application-localgroup-resume application)
    (application-input-controller--handle-submission controller message :steer-p
                                                     (application-input-controller-turn-active-p
                                                      controller))
    (list :ok :operation :tell :session-id
          (image-daemon:daemon-runtime-identifier session))))

(-> localgroup--dispatch-request (localgroup-session list) list)

(defun localgroup--dispatch-request (session request)
  "Return SESSION's response to one authenticated REQUEST."
  (unless (localgroup--valid-request-p request session)
    (return-from localgroup--dispatch-request
      (list :error :message "The localgroup request was rejected.")))
  (let ((operation (localgroup--request-field request :operation))
        (arguments (localgroup--request-field request :arguments)))
    (case operation
      (:status (list :ok :status (localgroup-status-snapshot session)))
      (:tell (localgroup--tell session arguments))
      (:pause
       (if (application-localgroup-pause (localgroup-session-application session))
           (list :ok :operation :pause :session-id
                 (image-daemon:daemon-runtime-identifier session))
           (list :error :message
                 "The localgroup session cannot pause while detaching.")))
      (:detach (localgroup--detach-terminal session))
      (:kill
       (let ((controller
              (application-input-controller (localgroup-session-application session))))
         (when controller
           (application-input-controller--request-exit controller ':localgroup-kill))
         (list :ok :operation :kill :session-id
               (image-daemon:daemon-runtime-identifier session))))
      (otherwise
       (list :error :message
             (format nil "Unknown localgroup operation ~S." operation))))))

(defun localgroup-start
    (application &key token created-at detached-explicitly-p)
  "Publish APPLICATION's active conversation with its product startup choreography."
  (let* ((restart-p (not (null token)))
         (startup-values (and *localgroup-startup-record* (rest *localgroup-startup-record*)))
         (configuration (application-configuration application))
         (session nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf session
                 (image-daemon:daemon-runtime-create
                  :directory (localgroup-registry-directory configuration)
                  :identifier (conversation-identifier (application-conversation application))
                  :token (or token (getf startup-values :token))
                  :created-at (or created-at (getf startup-values :created-at))
                  :class 'localgroup-session
                  :initargs (list :application application
                                  :detached-explicitly-p
                                  (or detached-explicitly-p
                                      (localgroup--initially-detached-p startup-values)))
                  :request-function #'localgroup--handle-request
                  :error-function (lambda (condition)
                                    (format *error-output*
                                            "~&Localgroup endpoint failed: ~A~%" condition)))
                 (application-localgroup-session application) session)
           (let ((terminal (terminal-ui-terminal (application-ui application)))
                 (controller (application-input-controller application)))
             (when (and controller (typep terminal 'localgroup-terminal))
               (image-daemon:relay-set-wake-function
                terminal
                (lambda ()
                  (with-lock-held ((application-input-controller-lock controller))
                    (sb-thread:condition-broadcast
                     (application-input-controller-condition-variable controller)))))))
           (unless restart-p (localgroup-handoff-assert-startup-active))
           (image-daemon:daemon-runtime-start session)
           (setf completed-p t)
           (when (and (not restart-p) (localgroup--attach-expected-p startup-values))
             (setf (localgroup-session-attach-watchdog-thread session)
                   (make-thread (lambda () (localgroup--attach-watchdog session))
                                :name "Autolith localgroup attach watchdog")))
           (unless restart-p (localgroup-handoff-finish-startup application))
           session)
      (unless completed-p
        (setf (application-localgroup-session application) nil)
        (when session (image-daemon:daemon-runtime-stop session))))))

(defun localgroup--handle-request (session request &key socket stream)
  "Dispatch authenticated requests according to Autolith's command and attachment policy."
  (if (eq (localgroup--request-field request :operation) ':attach)
      (localgroup--serve-attachment session socket stream request)
      (daemon-write-packet stream (localgroup--dispatch-request session request))))

(defun localgroup-stop (application)
  "Stop APPLICATION's watchdog and its owned image-daemon endpoint."
  (let ((session (application-localgroup-session application)))
    (when session
      (setf (application-localgroup-session application) nil)
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf (image-daemon:daemon-runtime-stopping-p session) t))
      (image-daemon:daemon-stop-thread (localgroup-session-attach-watchdog-thread session))
      (image-daemon:daemon-runtime-stop session)))
  nil)
