(in-package #:autolith)

(defclass localgroup-session (image-daemon:daemon-runtime)
          ((application :initarg :application :reader localgroup-session-application :type
            application :documentation "The primary application exposed by this endpoint.")
           (paused-p :initform nil :accessor localgroup-session-paused-p :type boolean
            :documentation "Whether queued primary work must wait for explicit input.")
           (closing-p :initform nil :accessor localgroup-session-closing-p :type boolean
            :documentation
            "Whether a close was committed while no terminal controlled the session, so no terminal may attach now.")
           (checkpoint-reconnect-id :initarg :checkpoint-reconnect-id :initform nil :accessor
            localgroup-session-checkpoint-reconnect-id :type (option string) :documentation
            "The checkpoint transition that created this endpoint.")
           (detached-explicitly-p :initarg :detached-explicitly-p :initform nil :accessor
            localgroup-session-detached-explicitly-p :type boolean :documentation
            "Whether the controlling terminal left through an explicit detach.")
           (controller-seen-p :initform nil :accessor localgroup-session-controller-seen-p :type
            boolean :documentation
            "Whether any controlling client has ever attached to this endpoint.")
           (linger-p :initarg :linger-p :initform nil :accessor localgroup-session-linger-p :type
            boolean :documentation
            "Whether the session outlives every client, as a world server's channels do.")
           (attach-watchdog-thread :initform nil :accessor
            localgroup-session-attach-watchdog-thread :type t :documentation
            "The bounded thread exiting a launch nobody attaches to.")
           (owner-thread :initform (current-thread) :reader localgroup-session-owner-thread
            :documentation "The serving thread that published this endpoint.")
           (attachment-failure :initform nil :accessor localgroup-session-attachment-failure
            :documentation
            "The latest failed attachment's original condition, bounded backtrace and cleanup failures."))
          (:documentation "One authenticated local control endpoint for a primary application."))

(-> localgroup-registry-directory (configuration) pathname)
(defun localgroup-registry-directory (configuration)
  "Return CONFIGURATION's private localgroup endpoint directory."
  (merge-pathnames "localgroup/" (config :state-root configuration)))

(-> localgroup-world-directory (configuration) pathname)
(defun localgroup-world-directory (configuration)
  "Return CONFIGURATION's private directory holding the world server's endpoint record.

It sits apart from the channel endpoints, so session listings never show the
server itself."
  (merge-pathnames "world/" (localgroup-registry-directory configuration)))

(-> localgroup-registry-pathname (configuration string) pathname)
(defun localgroup-registry-pathname (configuration session-id)
  "Return the discovery pathname for SESSION-ID under CONFIGURATION."
  (image-daemon:daemon-registry-pathname
   (localgroup-registry-directory configuration) session-id))

(-> localgroup--registry-record (localgroup-session) list)
(defun localgroup--registry-record (session)
  "Return SESSION's authenticated discovery record."
  (append (image-daemon:daemon-runtime-record session)
          (when (localgroup-session-checkpoint-reconnect-id session)
            (list :checkpoint-reconnect-id
                  (localgroup-session-checkpoint-reconnect-id session)))))

(-> localgroup--publish-registry (localgroup-session) null)
(defun localgroup--publish-registry (session)
  "Publish SESSION's authenticated discovery record."
  (image-daemon:daemon-registry-publish
   (image-daemon:daemon-runtime-registry-pathname session)
   (localgroup--registry-record session))
  nil)

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
  (let ((session    (application-localgroup-session application))
        (identifier (conversation-identifier (application-conversation application))))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (unless (string= identifier (image-daemon:daemon-runtime-identifier session))
          (let ((old-identifier (image-daemon:daemon-runtime-identifier session))
                (old-pathname   (image-daemon:daemon-runtime-registry-pathname session))
                (old-record     (localgroup--registry-record session))
                (completed-p    nil))
            (unwind-protect
                (progn
                 (setf (image-daemon:daemon-runtime-identifier session) identifier
                       (image-daemon:daemon-runtime-registry-pathname session)
                       (localgroup-registry-pathname
                        (application-configuration application) identifier))
                 (localgroup--publish-registry session)
                 (image-daemon:daemon-registry-delete-matching
                  old-pathname old-record :errorp t)
                 (setf completed-p t))
              (unless completed-p
                (localgroup--delete-owned-registry session)
                (setf (image-daemon:daemon-runtime-identifier session) old-identifier
                      (image-daemon:daemon-runtime-registry-pathname session)
                      old-pathname))))))))
  (application-mission-schedules-notify application)
  nil)

(-> localgroup-endpoint-records (configuration) list)
(defun localgroup-endpoint-records (configuration)
  "Return CONFIGURATION's discovery records ordered by newest publication."
  (image-daemon:daemon-registry-discover
   (localgroup-registry-directory configuration)))

(-> localgroup-reconcile-endpoint-records (configuration) (integer 0))
(defun localgroup-reconcile-endpoint-records (configuration)
  "Remove definitely dead endpoint records beneath CONFIGURATION and return their count."
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
    (let ((session    (application-localgroup-session application))
          (controller (application-input-controller application)))
      (unless (and session controller) (return-from application-localgroup-pause nil))
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf (localgroup-session-paused-p session) t))
      (application-input-controller--request-active-turn-cancellation controller)
      (with-lock-held ((application-input-controller-lock controller))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))
      t))

(-> localgroup--task-counts (application) (values (integer 0) (integer 0) (integer 0)))
(defun localgroup--task-counts (application)
  "Return live children, active children, and all live jobs for APPLICATION."
  (let ((orchestrator (application-task-presentation-orchestrator application)))
    (if orchestrator
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (values (task-orchestrator-live-count orchestrator)
                  (task-orchestrator-active-count orchestrator)
                  (task-orchestrator-session-live-count orchestrator)))
        (values 0 0 0))))

(-> application-terminal-attached-p (application) boolean)
(defun application-terminal-attached-p (application)
  "Return true while a terminal shows APPLICATION: a relay with an attached client, or an interactive terminal of its own."
  (let ((terminal (and (slot-boundp application 'ui)
                       (application-ui application)
                       (terminal-ui-terminal (application-ui application)))))
    (and terminal
         (if (typep terminal 'localgroup-terminal)
             (image-daemon:relay-attached-p terminal)
             (terminal-interactive-p terminal))
         t)))

(-> localgroup-status-snapshot (localgroup-session) list)
(defun localgroup-status-snapshot (session)
  "Return a portable, internally consistent status snapshot for SESSION."
  (let* ((application       (localgroup-session-application session))
         (configuration     (application-configuration application))
         (conversation      (application-conversation application))
         (terminal          (terminal-ui-terminal (application-ui application)))
         (client-pids       (multiple-value-list (localgroup-terminal-client-pids terminal)))
         (controller        (application-input-controller application))
         (paused-p          (application-localgroup-paused-p application))
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
      (localgroup--task-counts application))
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
      (list :localgroup-status :version *daemon-protocol-version* :session-id
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
            (let ((person (application-world-human-identifier application)))
              (if person
                  (gethash person (conversation-channel-mention-counts conversation) 0)
                  0))
            :terminal-attached-p
            (application-terminal-attached-p application)
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
            :created-at         (image-daemon:daemon-runtime-created-at session)))))

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
             :message   "The localgroup attachment mode is invalid."
             :operation ':attach))
    mode))

(defparameter *localgroup-first-attach-timeout-seconds* 60
  "The maximum seconds a launch waits for the client it was started for.")

(-> localgroup--attach-expected-p (list) boolean)
(defun localgroup--attach-expected-p (startup-values)
  "Return true when a controlling client is expected to attach at startup."
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

(-> localgroup-session-endpoint-live-p (localgroup-session) boolean)
(defun localgroup-session-endpoint-live-p (session)
  "Return whether SESSION owns a healthy listener and has not committed to close.

Startup and checkpoint pause are separate lifecycle states; use this predicate
for an endpoint already published as ready."
  (and (image-daemon:daemon-runtime-live-p session)
       (with-lock-held ((image-daemon:daemon-runtime-lock session))
         (and (not (localgroup-session-closing-p session))
              (localgroup-session-serving-p session)))))

(-> localgroup-session-serving-p (localgroup-session) boolean)
(defun localgroup-session-serving-p (session)
  "Return whether SESSION's owner and input controller can serve it.

Call with the runtime lock held; do not acquire the controller or world server
lock. Controller callbacks take the runtime lock while holding their own lock.
Sample its monotonic stopping flag alongside its failure state instead."
  (let* ((application (localgroup-session-application session))
         (controller  (application-input-controller application))
         (owner       (localgroup-session-owner-thread session)))
    (and owner (thread-alive-p owner)
         (null (image-daemon:daemon-runtime-failure session))
         controller (null (application-input-controller-failure controller))
         (not (application-input-controller-stopping-p controller)) t)))

(-> localgroup-session-admits-attachment-p (localgroup-session) boolean)
(defun localgroup-session-admits-attachment-p (session)
  "Return true while SESSION admits a new terminal: it is neither stopping nor committed to close.

Call it with SESSION's runtime lock held, the lock a committed close takes
before it checks for a controlling terminal, so no terminal is admitted
between that check and the close."
  (and (not (image-daemon:daemon-runtime-stopping-p session))
       (not (localgroup-session-closing-p session))
       (localgroup-session-serving-p session)))

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
  "Exit SESSION after controller loss unless explicitly detached or hosted to linger.
Persist pending input and cancel any active turn when exiting. Return true on exit."
  (let ((exit-p
         (with-lock-held ((image-daemon:daemon-runtime-lock session))
           (not (or (localgroup-session-detached-explicitly-p session)
                    (localgroup-session-linger-p session))))))
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

(-> localgroup--terminal-event-p (t) boolean)
(defun localgroup--terminal-event-p (event)
  "Accept relay input, bounded pointer events and the sidebar's channel commands."
  (not (null (or (image-daemon:daemon-terminal-event-p event)
                 (and (typep event '(cons (eql :application-command) (cons string null)))
                      (member (second event) '("(new)" "(delete)") :test #'equal))
                 (typep event '(cons (eql :scroll) (cons (member -1 1) null)))
                 (typep event '(cons (member :click :drag :release)
                                (cons (integer 1 10000)
                                 (cons (integer 1 10000) null))))))))

(-> localgroup--version-mismatch-notice (application t) (option terminal-styled-text))
(defun localgroup--version-mismatch-notice (application client-version)
  "Return an informational transcript note when CLIENT-VERSION differs from this server."
  (when (and (non-empty-string-p client-version)
             (not (string= client-version *autolith-version*)))
    (application-notice-line
     (format nil "Client/server version mismatch: client Autolith ~A; ~A Autolith ~A."
             (bounded-string client-version :limit 64)
             (if (application-world-server application)
                 "world server"
                 "server")
             *autolith-version*))))

(-> localgroup--attachment-failure-message (condition) string)
(defun localgroup--attachment-failure-message (condition)
  "Describe a refusal without printing arbitrary condition arguments."
  (handler-case
      (bounded-string
       (typecase condition
         (autolith-error
          (autolith-error-message condition))
         (image-daemon:daemon-error
          (image-daemon:daemon-error-message condition))
         (t
          (application-crash-condition-report condition)))
       :limit 500)
    (error () "The endpoint could not complete this attachment.")))

(-> localgroup--serve-attachment
    (localgroup-session list &key (:socket sb-bsd-sockets:socket) (:stream stream))
    null)
(defun localgroup--serve-attachment (session request &key socket stream)
  "Refuse before queue commitment, or send a terminal failure after commitment.

Capture the original condition and backtrace before any diagnostic or cleanup
operation. Drain through the sole attachment writer outside endpoint locks."
  (let* ((application      (localgroup-session-application session))
         (controller       (application-input-controller application))
         (attachment       nil)
         (terminal         nil)
         (stage            "request")
         (failure          nil)
         (backtrace        nil)
         (record           nil)
         (cleanup-failures nil))
    (labels ((capture (condition)
               (unless failure
                 (setf failure condition
                       backtrace (handler-case (application-safe-backtrace) (error nil nil)))))

             (attempt (name function)
               (handler-case (funcall function)
                             (error (condition) (push (list name condition) cleanup-failures)
                                    nil)))

             (retain-failure ()
               (setf record (list :condition failure :backtrace backtrace :stage stage)
                     (localgroup-session-attachment-failure session) record))

             (attach (rows columns styled-p)
               (let ((previous nil))
                 (unwind-protect
                     (with-lock-held ((image-daemon:daemon-runtime-lock session))
                       (unless (localgroup-session-admits-attachment-p session)
                         (error 'localgroup-error :message
                                (if (localgroup-session-closing-p session)
                                    "The localgroup session is closing."
                                    "The localgroup session is stopping.")
                                :operation ':attach))
                       (multiple-value-bind (attached-p direct-p replaced)
                           (image-daemon:relay-attach terminal attachment :rows rows :columns
                                                      columns :styled-p styled-p :session-id
                                                      (image-daemon:daemon-runtime-identifier
                                                       session)
                                                      :defer-revocation-p t)
                         (setf previous replaced)
                         (values attached-p direct-p)))
                   (when previous
                     (attempt "revocation"
                      (lambda ()
                        (image-daemon:relay-retire-attachment terminal previous :packet '(:revoked)))))))))
      (unwind-protect
          (handler-case
           (handler-bind ((error #'capture))
             (unless (localgroup--valid-request-p request session)
               (error 'localgroup-error :message "The localgroup request was rejected." :operation
                      ':attach))
             (let* ((arguments (localgroup--request-field request :arguments))
                    (mode      (localgroup--attachment-mode arguments))
                    (rows      (or (getf arguments :rows) *terminal-default-rows*))
                    (columns   (or (getf arguments :columns) *terminal-default-columns*))
                    (styled-p  (not (null (getf arguments :styled-p)))))
               (setf terminal (localgroup--terminal session)
                     attachment
                       (localgroup-attachment-create socket stream mode :client-pid
                                                     (getf arguments :client-pid))
                     stage "admission")
               (unless
                   (if (eq mode ':read-only)
                       (attach rows columns styled-p)
                       (application-input-controller-call-with-exclusive-input controller
                                                                               (lambda ()
                                                                                 (let ((attached-p
                                                                                        (attach
                                                                                         rows
                                                                                         columns
                                                                                         styled-p)))
                                                                                   (when attached-p
                                                                                     (application-input-controller--open-prompt-if-ready
                                                                                      controller))
                                                                                   attached-p))))
                 (error 'localgroup-error :message "The attachment handshake could not be queued."
                        :operation                 ':attach))
               (setf stage "replay")
               (application-sync-window-title application)
               (localgroup-terminal-replay-screen terminal attachment)
               (let ((notice
                      (localgroup--version-mismatch-notice application
                                                           (getf arguments :client-version))))
                 (when notice (application-present application notice)))
               (unless (eq mode ':read-only)
                 (localgroup--note-controller-attached session)
                 (application-world-attached application)
                 (let ((notice (getf arguments :notice)))
                   (when (stringp notice)
                     (application-present application
                                          (application-failure-line
                                           (bounded-string notice :limit 500))))))
               (setf stage "input")
               (image-daemon:relay-read-attachment terminal attachment :event-validator
                                                   #'localgroup--terminal-event-p)))
           (error (condition) (capture condition) (retain-failure)
                  (let* ((message (localgroup--attachment-failure-message failure))
                         (packet
                          (if (and attachment
                                   (image-daemon:attachment-handshake-committed-p attachment))
                              (list :exit :status 1 :message message :details
                                    (list :reason "attachment-failure" :stage stage))
                              (list :error :message message))))
                    (attempt "failure-delivery"
                     (lambda ()
                       (if attachment
                           (image-daemon:attachment-finish attachment packet)
                           (daemon-write-packet stream packet)))))))
        (when attachment
          (let ((controlled-p nil))
            (when terminal
              (attempt "detach"
               (lambda () (setf controlled-p (image-daemon:relay-detach terminal attachment)))))
            (when controlled-p
              (attempt "live-region" (lambda () (localgroup--detach-live-region application))))
            (attempt "transport" (lambda () (image-daemon:attachment-close attachment)))
            (when controlled-p
              (attempt "controller-loss" (lambda () (localgroup--controller-lost session)))))
          (when (and (null record) (image-daemon:attachment-failure attachment))
            (setf record (copy-list (image-daemon:attachment-failure attachment))
                  (localgroup-session-attachment-failure session) record)))
        (when cleanup-failures
          (unless record
            (setf record (list :stage "cleanup")
                  (localgroup-session-attachment-failure session) record))
          (setf (getf record :cleanup-failures) (nreverse cleanup-failures)
                (localgroup-session-attachment-failure session) record))))
    nil))

(-> localgroup--detach-terminal (localgroup-session) list)
(defun localgroup--detach-terminal (session)
  "Release SESSION's current controlling terminal."
  (let* ((application (localgroup-session-application session))
         (terminal    (localgroup--terminal session)))
    (when (eq (image-daemon:relay-attachment-kind terminal) ':foreground)
      (error 'localgroup-error
             :message    "This foreground process cannot detach. Open a supervised session first."
             :operation  ':detach
             :session-id (image-daemon:daemon-runtime-identifier session)))
    (localgroup--mark-explicit-detach session)
    (image-daemon:relay-release-control terminal)
    (localgroup--detach-live-region application)
    (list :ok :operation :detach :scheduled-p nil :session-id
          (image-daemon:daemon-runtime-identifier session))))

(-> localgroup-finish-attachments
    ((option application)
     &key (:status (integer 0 255)) (:message (option string)) (:details list))
    boolean)
(defun localgroup-finish-attachments (application &key status message details)
  "Tell APPLICATION's attached terminals the session exits with STATUS and MESSAGE.

A detached session's own launcher only logs its exit status, so the relaying
terminal must exit with it instead for the outer launcher to update, roll back,
or recover. DETAILS carries what that launcher needs besides the status, such
as the session's crash capsule. Return true when APPLICATION's terminal is a
relay; a terminal that already disconnected costs nothing."
  (let* ((ui       (and application (application-ui application)))
         (terminal (and ui (terminal-ui-terminal ui))))
    (when (typep terminal 'localgroup-terminal)
      (handler-case
          (image-daemon:relay-finish terminal :status  status
                                              :message message
                                              :details details)
        (error ()
          nil))
      t)))


;;;; -- Localgroup Request Handling --

(-> localgroup--request-field (list keyword) t)
(defun localgroup--request-field (request key)
  "Return KEY from REQUEST's property list."
  (getf (rest request) key))

(-> localgroup--valid-request-p (t localgroup-session) boolean)
(defun localgroup--valid-request-p (request session)
  "Validate REQUEST against SESSION's image-daemon capability."
  (and (image-daemon:daemon-request-valid-p
        request (image-daemon:daemon-runtime-token session))
       t))

(-> localgroup--dispatch-request (localgroup-session list) list)
(defun localgroup--dispatch-request (session request)
  "Return SESSION's response to one authenticated REQUEST."
  (unless (localgroup--valid-request-p request session)
    (return-from localgroup--dispatch-request
      (list :error :message "The localgroup request was rejected.")))
  (let ((operation (localgroup--request-field request :operation)))
    (case operation
      (:status
       (list :ok :status (localgroup-status-snapshot session)))
      (:pause
       (if (application-localgroup-pause (localgroup-session-application session))
           (list :ok :operation :pause :session-id
                 (image-daemon:daemon-runtime-identifier session))
           (list :error :message
                 "The localgroup session cannot pause while detaching.")))
      (:detach
       (localgroup--detach-terminal session))
      (:kill
       (list :ok :operation :kill :session-id
             (image-daemon:daemon-runtime-identifier session)))
      (otherwise
       (list :error :message
             (format nil "Unknown localgroup operation ~S." operation))))))

(-> localgroup-start
    (application
     &key (:token (option string)) (:created-at (option integer))
       (:detached-explicitly-p t) (:checkpoint-reconnect-id (option string))
       (:linger-p boolean))
    localgroup-session)
(defun localgroup-start
    (application &key token created-at detached-explicitly-p checkpoint-reconnect-id
                      linger-p)
  "Publish APPLICATION's active conversation with its product startup choreography.

With LINGER-P the session outlives its clients instead of exiting when the
last controlling client vanishes."
  (let* ((restart-p      (not (null token)))
         (startup-values (and *localgroup-startup-record* (rest *localgroup-startup-record*)))
         (configuration  (application-configuration application))
         (session        nil)
         (completed-p    nil))
    (unwind-protect
         (progn
           (setf session
                 (image-daemon:daemon-runtime-create
                  :directory        (localgroup-registry-directory configuration)
                  :identifier       (conversation-identifier (application-conversation application))
                  :token            (or token (getf startup-values :token))
                  :created-at       (or created-at (getf startup-values :created-at))
                  :class            'localgroup-session
                  :initargs         (list :application             application
                                          :detached-explicitly-p
                                          (or detached-explicitly-p
                                              (localgroup--initially-detached-p startup-values))
                                          :checkpoint-reconnect-id checkpoint-reconnect-id
                                          :linger-p                linger-p)
                  :request-function #'localgroup--handle-request
                  :error-function   (lambda (condition)
                                      (localgroup--endpoint-failed session condition)))
                 (application-localgroup-session application) session)
           (let ((terminal   (terminal-ui-terminal (application-ui application)))
                 (controller (application-input-controller application)))
             (when (and controller (typep terminal 'localgroup-terminal))
               (image-daemon:relay-set-wake-function
                terminal
                (lambda ()
                  (with-lock-held ((application-input-controller-lock controller))
                    (sb-thread:condition-broadcast
                     (application-input-controller-condition-variable controller)))))))
           (unless restart-p (localgroup-handoff-assert-startup-active))
           (let ((hub (application-client-hub application)))
             (with-lock-held ((client-hub-lock hub))
               (setf (client-hub-admitting-p hub) t)))
           (image-daemon:daemon-runtime-start session)
           (localgroup--publish-registry session)
           (application-mission-schedules-host application session)
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

(-> localgroup--endpoint-failed (localgroup-session serious-condition) null)
(defun localgroup--endpoint-failed (session condition)
  "End SESSION through its application's fatal path, preserving accepted input.

A failed scheduling callback is not retried. Other channels in the world
continue independently while this application's normal teardown releases its lease."
  (let* ((application (localgroup-session-application session))
         (controller  (application-input-controller application)))
    (when (and (eq session (application-localgroup-session application))
               (not (image-daemon:daemon-runtime-stopping-p session)))
      (if controller
          (application-input-controller--record-failure
           controller condition (application-safe-backtrace))
          (format *error-output* "~&Localgroup endpoint failed: ~A~%" condition))))
  nil)

(-> localgroup--message-dispatch (application list) t)
(defun localgroup--message-dispatch (application envelope)
  "Answer one daemon message for APPLICATION: a channel's new inbox, or a mission event for its conversation.

Any other message is rejected, as a request this process has no use for."
  (case (getf envelope :action)
    (:channel
     (list :ok :queued (application-world-deliver application)))
    (:event
     (unless (and (equal (getf envelope :receiver)
                         (conversation-identifier (application-conversation application)))
                  (stringp (getf envelope :payload)))
       (localgroup--reject-message ':event-scope))
     (application-mission-schedule-event application (getf envelope :payload) (getf envelope :id)))
    (otherwise
     (localgroup--reject-message ':unsupported))))

(-> localgroup--reject-message (keyword) nil)
(defun localgroup--reject-message (reason)
  "Reject a daemon message for REASON without echoing its payload."
  (error 'image-daemon:daemon-message-error
         :reason    reason
         :operation ':message
         :message   (format nil "Daemon message rejected: ~(~A~)." reason)))

(-> localgroup--handle-request
    (localgroup-session list &key (:socket t) (:stream t))
    null)
(defun localgroup--handle-request (session request &key socket stream)
  "Dispatch authenticated requests according to Autolith's command and attachment policy."
  (case (localgroup--request-field request :operation)
    (:attach
     (localgroup--serve-attachment session request :socket socket :stream stream))
    (:client
     (localgroup--serve-client session request :socket socket :stream stream))
    (:message
     (image-daemon:daemon-message-handle-request
      session request :stream stream
      :dispatcher
      (lambda (envelope &key cancelled-p)
        (declare (ignore cancelled-p))
        (localgroup--message-dispatch (localgroup-session-application session) envelope))
      :character-limit 32768 :request-timeout 2))
    (otherwise
     (let* ((response (localgroup--dispatch-request session request))
            (kill-request-p
              (and (eq (localgroup--request-field request :operation) ':kill)
                   (eq (first response) ':ok))))
       (daemon-write-packet stream response)
       (when kill-request-p
         (let ((controller
                 (application-input-controller
                  (localgroup-session-application session))))
           (when controller
             (application-input-controller--request-exit
              controller ':localgroup-kill)))))))
  nil)

(define-condition localgroup-stop-error (error)
  ((session :initarg :session :reader localgroup-stop-error-session
            :documentation "The endpoint whose cleanup remains owned.")
   (failures :initarg :failures :reader localgroup-stop-error-failures
             :documentation "Ordered cleanup stage and condition pairs."))
  (:documentation "Endpoint cleanup failed and can be retried.")
  (:report (lambda (condition stream)
             (format stream "Localgroup cleanup failed in ~{~A~^, ~}; retry stop to finish cleanup."
                     (mapcar #'first (localgroup-stop-error-failures condition))))))

(-> localgroup-stop (application) null)
(defun localgroup-stop (application)
  "Attempt every endpoint cleanup stage, retaining ownership until all succeed."
  (let ((session  (application-localgroup-session application))
        (failures nil))
    (when session
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (setf (image-daemon:daemon-runtime-stopping-p session) t))
      (flet ((attempt (stage function)
               (handler-case (funcall function)
                 (error (condition) (push (list stage condition) failures)))))
        (attempt ':watchdog
                 (lambda ()
                   (image-daemon:daemon-stop-thread
                    (localgroup-session-attach-watchdog-thread session))))
        (attempt ':runtime (lambda () (image-daemon:daemon-runtime-stop session)))
        (attempt ':registry (lambda () (localgroup--delete-owned-registry session))))
      (when failures
        (error 'localgroup-stop-error :session session :failures (nreverse failures)))
      (when (eq session (application-localgroup-session application))
        (setf (application-localgroup-session application) nil))))
  nil)
