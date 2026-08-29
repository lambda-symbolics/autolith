(in-package #:autolith)

;;;; -- Localgroup Runtime State --

(defparameter *localgroup-registry-version* 1
  "The readable localgroup endpoint-record version.")

(defclass localgroup-session ()
  ((application
    :initarg :application
    :reader localgroup-session-application
    :type application
    :documentation "The primary application exposed by this endpoint.")
   (identifier
    :initarg :identifier
    :reader localgroup-session-identifier
    :type non-empty-string
    :documentation "The process-lifetime identifier used by localgroup commands.")
   (token
    :initarg :token
    :reader localgroup-session-token
    :type non-empty-string
    :documentation "The private capability authenticating loopback requests.")
   (listener
    :initarg :listener
    :accessor localgroup-session-listener
    :type t
    :documentation "The loopback listening socket, or NIL after shutdown.")
   (port
    :initarg :port
    :reader localgroup-session-port
    :type (integer 1 65535)
    :documentation "The ephemeral loopback TCP port.")
   (registry-pathname
    :initarg :registry-pathname
    :reader localgroup-session-registry-pathname
    :type pathname
    :documentation "The private endpoint-discovery record.")
   (created-at
    :initarg :created-at
    :reader localgroup-session-created-at
    :type timestamp
    :documentation "The universal time at which the endpoint started.")
   (lock
    :initform (make-lock "Autolith localgroup session")
    :reader localgroup-session-lock
    :type t
    :documentation "The lock protecting lifecycle and pause state.")
   (paused-p
    :initform nil
    :accessor localgroup-session-paused-p
    :type boolean
    :documentation "Whether queued primary work must wait for explicit input.")
   (stopping-p
    :initform nil
    :accessor localgroup-session-stopping-p
    :type boolean
    :documentation "Whether endpoint shutdown has begun.")
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
    :documentation "The bounded thread exiting a launch nobody attaches to, when running.")
   (server-thread
    :initform nil
    :accessor localgroup-session-server-thread
    :type t
    :documentation "The accept-loop thread, when running.")
   (client-threads
    :initform nil
    :accessor localgroup-session-client-threads
    :type list
    :documentation "The bounded request threads not yet reaped.")
   (client-sockets
    :initform nil
    :accessor localgroup-session-client-sockets
    :type list
    :documentation "The accepted sockets closed during endpoint shutdown."))
  (:documentation "One authenticated local control endpoint for a primary application."))

(-> localgroup-registry-directory (configuration) pathname)
(defun localgroup-registry-directory (configuration)
  "Return CONFIGURATION's private localgroup endpoint directory."
  (merge-pathnames "localgroup/" (configuration-state-root configuration)))

(-> localgroup-registry-pathname (configuration string) pathname)
(defun localgroup-registry-pathname (configuration session-id)
  "Return the endpoint record pathname for SESSION-ID."
  (merge-pathnames (make-pathname :name session-id :type "sexp")
                   (localgroup-registry-directory configuration)))

(-> localgroup-session-identifier-generate (configuration timestamp) string)
(defun localgroup-session-identifier-generate (configuration timestamp)
  "Allocate one canonical timestamp-bearing identifier for a local process session."
  (let ((directory (localgroup-registry-directory configuration)))
    (ensure-directories-exist directory)
    (identifier-generate
     :timestamp timestamp
     :namespace (namestring directory)
     :occupied-p
     (lambda (candidate)
       (not
        (null
         (probe-file
          (localgroup-registry-pathname configuration candidate))))))))


(-> localgroup--registry-record (localgroup-session) list)
(defun localgroup--registry-record (session)
  "Return SESSION's private endpoint discovery record."
  (list :localgroup-endpoint
        :version *localgroup-registry-version*
        :session-id (localgroup-session-identifier session)
        :pid (sb-posix:getpid)
        :address "127.0.0.1"
        :port (localgroup-session-port session)
        :token (localgroup-session-token session)
        :created-at (localgroup-session-created-at session)))

(-> localgroup--publish-registry (localgroup-session) null)
(defun localgroup--publish-registry (session)
  "Atomically publish SESSION's private discovery record."
  (let* ((directory
           (localgroup-registry-directory
            (application-configuration
             (localgroup-session-application session))))
         (pathname (localgroup-session-registry-pathname session)))
    (ensure-directories-exist pathname)
    (sb-posix:chmod (namestring directory) #o700)
    (snapshot-write pathname (localgroup--registry-record session))
    (sb-posix:chmod (namestring pathname) #o600))
  nil)

(-> localgroup--delete-owned-registry (localgroup-session) null)
(defun localgroup--delete-owned-registry (session)
  "Delete SESSION's registry record only while it still names this endpoint."
  (let* ((pathname (localgroup-session-registry-pathname session))
         (current (localgroup--read-endpoint-record pathname)))
    (when (equal current (localgroup--registry-record session))
      (ignore-errors (delete-file pathname))))
  nil)

(-> localgroup--endpoint-record-p (t) boolean)
(defun localgroup--endpoint-record-p (record)
  "Return true when RECORD is one supported localgroup endpoint record."
  (and (localgroup--proper-list-p record)
       (eq (first record) ':localgroup-endpoint)
       (= (or (getf (rest record) :version) 0)
          *localgroup-registry-version*)
       (non-empty-string-p (getf (rest record) :session-id))
       (typep (getf (rest record) :pid) '(integer 1))
       (string= (or (getf (rest record) :address) "") "127.0.0.1")
       (typep (getf (rest record) :port) '(integer 1 65535))
       (non-empty-string-p (getf (rest record) :token))
       (typep (getf (rest record) :created-at) 'timestamp)))

(-> localgroup--read-endpoint-record (pathname) (option list))
(defun localgroup--read-endpoint-record (pathname)
  "Return PATHNAME's valid endpoint record, or NIL."
  (handler-case
      (multiple-value-bind (record complete-p)
          (snapshot-read pathname)
        (and complete-p
             (localgroup--endpoint-record-p record)
             record))
    (error () nil)))

(-> localgroup-endpoint-records (configuration) list)
(defun localgroup-endpoint-records (configuration)
  "Return valid endpoint records ordered by newest publication first."
  (let ((directory (localgroup-registry-directory configuration)))
    (sort
     (loop for pathname in (uiop:directory-files directory "*.sexp")
           for record = (localgroup--read-endpoint-record pathname)
           when record
             collect (cons pathname record))
     #'>
     :key (lambda (entry)
            (or (ignore-errors (file-write-date (first entry))) 0)))))

(-> application-localgroup-paused-p (application) boolean)
(defun application-localgroup-paused-p (application)
  "Return true when APPLICATION is deliberately holding queued work."
  (let ((session (application-localgroup-session application)))
    (and session
         (not
          (null
           (with-lock-held ((localgroup-session-lock session))
             (localgroup-session-paused-p session)))))))

(-> application-localgroup-resume (application) boolean)
(defun application-localgroup-resume (application)
  "Resume APPLICATION's queued work and report whether it had been paused."
  (let ((session (application-localgroup-session application))
        (resumed-p nil))
    (when session
      (with-lock-held ((localgroup-session-lock session))
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
    (unless (and session controller)
      (return-from application-localgroup-pause nil))
    (with-lock-held ((localgroup-session-lock session))
      (when (or (localgroup-session-handoff-mode session)
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
               (null
                (application-input-controller-follow-up-edit-work controller)))
              stopping-p
              (application-input-controller-stopping-p controller)
              cancelling-p
              (application-input-controller-turn-cancellation-p controller)
              reader-paused-p
              (application-input-controller-reader-paused-p controller)
              failed-p
              (not
               (null (application-input-controller-failure controller))))))
    (multiple-value-setq (task-live-count task-active-count)
      (localgroup--task-counts application))
    (let* ((waiting-for-input-p
             (and controller
                  (not active-p)
                  (zerop queued-count)
                  (zerop steering-count)
                  (not recalled-p)
                  (not stopping-p)
                  (not cancelling-p)
                  (not handoff-p)
                  (not reader-paused-p)
                  (not failed-p)))
           (idle-p
             (and waiting-for-input-p
                  (not paused-p)
                  (zerop task-live-count)))
           (state
             (cond (stopping-p ':stopping)
                   (failed-p ':failed)
                   (handoff-p ':detaching)
                   (paused-p ':paused)
                   (cancelling-p ':cancelling)
                   (active-p ':active)
                   ((or (plusp queued-count)
                        (plusp steering-count)
                        recalled-p
                        (plusp task-live-count))
                    ':working)
                   (idle-p ':idle)
                   (t ':starting))))
      (list :localgroup-status
            :version *daemon-protocol-version*
            :session-id (localgroup-session-identifier session)
            :pid (sb-posix:getpid)
            :autolith-version *autolith-version*
            :state state
            :idle-p (not (null idle-p))
            :waiting-for-input-p (not (null waiting-for-input-p))
            :paused-p (not (null paused-p))
            :handoff-p (not (null handoff-p))
            :cwd (namestring (configuration-working-directory configuration))
            :conversation-id (conversation-identifier conversation)
            :conversation-display-id
            (conversation-identifier-display (conversation-identifier conversation))
            :conversation-title (conversation-title conversation)
            :conversation-persisted-p
            (not (null (conversation-persisted-p conversation)))
            :model (configuration-model configuration)
            :reasoning-effort (configuration-reasoning-effort configuration)
            :permission-mode (application-permission-mode application)
            :active-turn-p (not (null active-p))
            :queued-input-count queued-count
            :steering-input-count steering-count
            :recalled-input-p (not (null recalled-p))
            :turn-cancelling-p (not (null cancelling-p))
            :reader-paused-p (not (null reader-paused-p))
            :task-live-count task-live-count
            :task-active-count task-active-count
            :terminal-attached-p
            (let ((terminal
                    (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (localgroup-terminal-attached-p terminal)
                  (not (null (terminal-interactive-p terminal)))))
            :terminal-attachment
            (let ((terminal
                    (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (localgroup-terminal-attachment-kind terminal)
                  (if (terminal-interactive-p terminal)
                      ':foreground
                      ':detached)))
            :observer-count
            (let ((terminal
                    (terminal-ui-terminal (application-ui application))))
              (if (typep terminal 'localgroup-terminal)
                  (localgroup-terminal-observer-count terminal)
                  0))
            :created-at (localgroup-session-created-at session)))))


;;;; -- Localgroup Terminal Ownership --

(-> localgroup--terminal (localgroup-session) localgroup-terminal)
(defun localgroup--terminal (session)
  "Return SESSION's attachable relay terminal."
  (let ((terminal
          (terminal-ui-terminal
           (application-ui (localgroup-session-application session)))))
    (unless (typep terminal 'localgroup-terminal)
      (error 'localgroup-error
             :message "This Autolith session has no attachable terminal relay."
             :operation ':attach
             :session-id (localgroup-session-identifier session)))
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

(-> localgroup--terminal-event-p (t) boolean)
(defun localgroup--terminal-event-p (event)
  "Return true when EVENT is one supported terminal semantic event."
  (or (keywordp event)
      (characterp event)
      (and (localgroup--proper-list-p event)
           (= (length event) 2)
           (member (first event) '(:insert :paste :line))
           (stringp (second event)))))

(-> localgroup--attachment-read-loop
    (localgroup-terminal localgroup-attachment)
    null)
(defun localgroup--attachment-read-loop (terminal attachment)
  "Relay input packets from ATTACHMENT until detach or end of stream."
  (loop
    for packet =
      (handler-case
          (daemon-read-packet (localgroup-attachment-stream attachment))
        (error () nil))
    while packet
    do (case (first packet)
         (:detach
          (return))
         (:event
          (when (and (not (eq (localgroup-attachment-mode attachment)
                              ':read-only))
                     (= (length packet) 2)
                     (localgroup--terminal-event-p (second packet)))
            (localgroup-terminal-enqueue-event
             terminal attachment (second packet))))
         (:resize
          (when (not (eq (localgroup-attachment-mode attachment) ':read-only))
            (let ((rows (getf (rest packet) :rows))
                  (columns (getf (rest packet) :columns))
                  (styled-p (getf (rest packet) :styled-p)))
              (when (and (typep rows '(integer 1 10000))
                         (typep columns '(integer 1 10000))
                         (typep styled-p 'boolean))
                (localgroup-terminal-resize
                 terminal rows columns styled-p)))))))
  nil)

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
  (with-lock-held ((localgroup-session-lock session))
    (setf (localgroup-session-detached-explicitly-p session) t))
  nil)

(-> localgroup--note-controller-attached (localgroup-session) null)
(defun localgroup--note-controller-attached (session)
  "Record that a controlling client owns SESSION's terminal from now on."
  (with-lock-held ((localgroup-session-lock session))
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
  (let ((exit-p (with-lock-held ((localgroup-session-lock session))
                  (not (localgroup-session-detached-explicitly-p session)))))
    (when exit-p
      (localgroup--exit-abandoned-session session))
    exit-p))

(-> localgroup--first-attach-overdue-p (localgroup-session) boolean)
(defun localgroup--first-attach-overdue-p (session)
  "Return true when SESSION never met the controlling client it was launched for."
  (let ((terminal (terminal-ui-terminal
                   (application-ui (localgroup-session-application session)))))
    (and (not (with-lock-held ((localgroup-session-lock session))
                (localgroup-session-controller-seen-p session)))
         (or (not (typep terminal 'localgroup-terminal))
             (not (localgroup-terminal-attached-p terminal))))))

(-> localgroup--attach-watchdog (localgroup-session) null)
(defun localgroup--attach-watchdog (session)
  "Exit SESSION when no controlling client attaches within the first-attach timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (* *localgroup-first-attach-timeout-seconds*
                        internal-time-units-per-second))))
    (loop
      (when (with-lock-held ((localgroup-session-lock session))
              (or (localgroup-session-stopping-p session)
                  (localgroup-session-controller-seen-p session)))
        (return))
      (when (>= (get-internal-real-time) deadline)
        (when (localgroup--first-attach-overdue-p session)
          (localgroup--exit-abandoned-session session))
        (return))
      (sleep 0.25)))
  nil)

(-> localgroup--serve-attachment
    (localgroup-session sb-bsd-sockets:socket stream list)
    null)
(defun localgroup--serve-attachment (session socket stream request)
  "Authenticate REQUEST, attach its persistent SOCKET, and relay terminal packets."
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
               (eq (localgroup-terminal-attachment-kind terminal) ':foreground))
      (let ((response
              (application-localgroup-request-handoff application ':take-over)))
        (daemon-write-packet
         stream
         (list :handoff
               :session-id (localgroup-session-identifier session)
               :old-pid (getf (rest response) :old-pid))))
      (return-from localgroup--serve-attachment nil))
    (let ((attachment (localgroup-attachment-create socket stream mode)))
      (unwind-protect
           (multiple-value-bind (attached-p released-direct-p)
               (if (eq mode ':read-only)
                   (localgroup-terminal-attach
                    terminal attachment
                    :rows rows
                    :columns columns
                    :styled-p styled-p
                    :session-id (localgroup-session-identifier session))
                   (application-input-controller-call-with-reader-paused
                    controller
                    (lambda ()
                      (multiple-value-bind (attached-p released-direct-p)
                          (localgroup-terminal-attach
                           terminal attachment
                           :rows rows
                           :columns columns
                           :styled-p styled-p
                           :session-id
                           (localgroup-session-identifier session))
                        (when attached-p
                          (application-input-controller--open-prompt-if-ready
                           controller))
                        (values attached-p released-direct-p)))))
             (declare (ignore released-direct-p))
             (unless attached-p
               (return-from localgroup--serve-attachment nil))
             (unless (eq mode ':read-only)
               (localgroup--note-controller-attached session))
             (localgroup--attachment-read-loop terminal attachment))
        (let ((controlled-p (localgroup-terminal-detach terminal attachment)))
          (localgroup-attachment-close attachment)
          (when controlled-p
            (localgroup--controller-lost session))))))
  nil)

(-> localgroup--detach-terminal (localgroup-session) list)
(defun localgroup--detach-terminal (session)
  "Release SESSION's current controlling terminal or schedule foreground detach."
  (let* ((application (localgroup-session-application session))
         (controller (application-input-controller application))
         (terminal (localgroup--terminal session)))
    (if (eq (localgroup-terminal-attachment-kind terminal) ':foreground)
        (application-localgroup-request-handoff application ':detach)
        (progn
          ;; Mark the detach before the controller's stream closes, so the
          ;; attachment thread observing the closure sees a deliberate
          ;; release rather than a vanished client.
          (localgroup--mark-explicit-detach session)
          (application-input-controller-call-with-reader-paused
           controller
           (lambda ()
             (localgroup-terminal-release-control terminal)))
          (list :ok :operation :detach
                :scheduled-p nil
                :session-id (localgroup-session-identifier session))))))


;;;; -- Localgroup Request Handling --

(-> localgroup--request-field (list keyword) t)
(defun localgroup--request-field (request key)
  "Return KEY from REQUEST's property list."
  (getf (rest request) key))

(-> localgroup--valid-request-p (list localgroup-session) boolean)
(defun localgroup--valid-request-p (request session)
  "Return true when REQUEST is structurally valid and authentic for SESSION."
  (and (eq (first request) ':localgroup-request)
       (= (or (localgroup--request-field request :version) 0)
          *daemon-protocol-version*)
       (stringp (localgroup--request-field request :token))
       (string= (localgroup--request-field request :token)
                (localgroup-session-token session))
       (keywordp (localgroup--request-field request :operation))))

(-> localgroup--tell (localgroup-session list) list)
(defun localgroup--tell (session arguments)
  "Submit ARGUMENTS' message through the ordinary responsive input path."
  (let* ((application (localgroup-session-application session))
         (controller (application-input-controller application))
         (message (getf arguments :message)))
    (unless (and controller (stringp message))
      (error 'localgroup-error
             :message "localgroup tell requires one string message."
             :operation ':tell
             :session-id (localgroup-session-identifier session)))
    (when (application-localgroup-handoff-pending-p application)
      (error 'localgroup-error
             :message "The localgroup session is detaching and no longer accepts input."
             :operation ':tell
             :session-id (localgroup-session-identifier session)))
    (application-localgroup-resume application)
    (application-input-controller--handle-submission
     controller message
     :steer-p (application-input-controller-turn-active-p controller))
    (list :ok :operation :tell
          :session-id (localgroup-session-identifier session))))

(-> localgroup--dispatch-request (localgroup-session list) list)
(defun localgroup--dispatch-request (session request)
  "Return SESSION's response to one authenticated REQUEST."
  (unless (localgroup--valid-request-p request session)
    (return-from localgroup--dispatch-request
      (list :error :message "The localgroup request was rejected.")))
  (let ((operation (localgroup--request-field request :operation))
        (arguments (localgroup--request-field request :arguments)))
    (case operation
      (:status
       (list :ok :status (localgroup-status-snapshot session)))
      (:tell
       (localgroup--tell session arguments))
      (:pause
       (if (application-localgroup-pause
            (localgroup-session-application session))
           (list :ok :operation :pause
                 :session-id (localgroup-session-identifier session))
           (list :error
                 :message "The localgroup session cannot pause while detaching.")))
      (:detach
       (localgroup--detach-terminal session))
      (:kill
       (let ((controller
               (application-input-controller
                (localgroup-session-application session))))
         (when controller
           (application-input-controller--request-exit
            controller ':localgroup-kill))
         (list :ok :operation :kill
               :session-id (localgroup-session-identifier session))))
      (otherwise
       (list :error
             :message (format nil "Unknown localgroup operation ~S." operation))))))

(-> localgroup--handle-client
    (localgroup-session sb-bsd-sockets:socket)
    null)
(defun localgroup--handle-client (session socket)
  "Read, answer, and close one localgroup client SOCKET."
  (let ((stream nil)
        (attachment-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf stream (daemon-socket-stream socket))
               (let ((request (daemon-read-packet stream)))
                 (cond
                   ((null request)
                    (daemon-write-packet
                     stream
                     (list :error :message "The localgroup request was empty.")))
                   ((eq (localgroup--request-field request :operation) ':attach)
                    (setf attachment-p t)
                    (localgroup--serve-attachment session socket stream request))
                   (t
                    (daemon-write-packet
                     stream
                     (localgroup--dispatch-request session request))))))
           (error (condition)
             (when (and stream (not attachment-p))
               (ignore-errors
                 (daemon-write-packet
                  stream
                  (list :error :message (princ-to-string condition)))))))
      (if stream
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> localgroup--run-client
    (localgroup-session sb-bsd-sockets:socket)
    null)
(defun localgroup--run-client (session socket)
  "Handle SOCKET and remove its runtime resources from SESSION."
  (unwind-protect
       (localgroup--handle-client session socket)
    (with-lock-held ((localgroup-session-lock session))
      (setf (localgroup-session-client-threads session)
            (delete (current-thread)
                    (localgroup-session-client-threads session))
            (localgroup-session-client-sockets session)
            (delete socket (localgroup-session-client-sockets session)))))
  nil)

(-> localgroup--serve (localgroup-session) null)
(defun localgroup--serve (session)
  "Accept bounded localgroup requests until SESSION stops."
  (loop
    (when (with-lock-held ((localgroup-session-lock session))
            (localgroup-session-stopping-p session))
      (return))
    (handler-case
        (let ((socket
                (sb-bsd-sockets:socket-accept
                 (localgroup-session-listener session))))
          (if (with-lock-held ((localgroup-session-lock session))
                (localgroup-session-stopping-p session))
              (ignore-errors (sb-bsd-sockets:socket-close socket))
              (with-lock-held ((localgroup-session-lock session))
                (let ((thread
                        (make-thread
                         (lambda ()
                           (localgroup--run-client session socket))
                         :name "Autolith localgroup request")))
                  (push socket (localgroup-session-client-sockets session))
                  (push thread (localgroup-session-client-threads session))))))
      (error (condition)
        (unless (with-lock-held ((localgroup-session-lock session))
                  (localgroup-session-stopping-p session))
          (format *error-output* "~&Localgroup endpoint failed: ~A~%" condition))
        (return))))
  nil)

(-> localgroup-start
    (application &key (:identifier (option string))
                      (:token (option string))
                      (:created-at (option timestamp))
                      (:detached-explicitly-p boolean))
    localgroup-session)
(defun localgroup-start
    (application &key identifier token created-at detached-explicitly-p)
  "Start and publish APPLICATION's authenticated loopback endpoint.

Optional identity values preserve one session across quiescence or process
handoff, and DETACHED-EXPLICITLY-P carries a deliberate detach across the
same boundary. A fresh launch started for a client that never arrives exits
after the first-attach timeout instead of lingering."
  (let* ((restart-p (not (null identifier)))
         (startup-values (and *localgroup-startup-record*
                              (rest *localgroup-startup-record*)))
         (identifier (or identifier (getf startup-values :session-id)))
         (token (or token (getf startup-values :token)))
         (created-at (or created-at (getf startup-values :created-at)))
         (configuration (application-configuration application))
         (listener (make-instance 'sb-bsd-sockets:inet-socket
                                  :type ':stream
                                  :protocol ':tcp))
         (session nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind
            listener
            (sb-bsd-sockets:make-inet-address "127.0.0.1")
            0)
           (sb-bsd-sockets:socket-listen listener 16)
           (multiple-value-bind (address port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore address))
              (let* ((created-at
                       (or created-at
                           (and identifier
                                (session-identifier-timestamp identifier))
                           (get-universal-time)))
                     (identifier
                       (session-identifier-normalize
                        (or identifier
                            (localgroup-session-identifier-generate
                             configuration created-at))))
                     (token (or token (daemon-random-token))))
                (setf session
                      (make-instance
                       'localgroup-session
                       :application application
                       :identifier identifier
                       :token token
                       :listener listener
                       :port port
                       :registry-pathname
                       (localgroup-registry-pathname configuration identifier)
                       :created-at created-at
                       :detached-explicitly-p
                       (or detached-explicitly-p
                           (localgroup--initially-detached-p startup-values)))
                      (application-localgroup-session application) session)
                (let ((agent (and (slot-boundp application 'agent)
                                  (application-agent application))))
                  (when (typep agent 'agent)
                    (setf (agent-session-id agent) identifier)))
               (let ((terminal
                       (terminal-ui-terminal (application-ui application)))
                     (controller (application-input-controller application)))
                 (when (and controller (typep terminal 'localgroup-terminal))
                   (localgroup-terminal-set-wake-function
                    terminal
                    (lambda ()
                      (with-lock-held
                          ((application-input-controller-lock controller))
                        (sb-thread:condition-broadcast
                         (application-input-controller-condition-variable
                          controller)))))))
               (localgroup-handoff-assert-startup-active)
               (localgroup--publish-registry session)
               (setf (localgroup-session-server-thread session)
                     (make-thread
                      (lambda () (localgroup--serve session))
                      :name "Autolith localgroup endpoint")
                     completed-p t)
               (when (and (not restart-p)
                          (localgroup--attach-expected-p startup-values))
                 (setf (localgroup-session-attach-watchdog-thread session)
                       (make-thread
                        (lambda () (localgroup--attach-watchdog session))
                        :name "Autolith localgroup attach watchdog")))
               (localgroup-handoff-finish-startup application)
               session)))
      (unless completed-p
        (when session
          (setf (application-localgroup-session application) nil)
          (localgroup--delete-owned-registry session))
        (ignore-errors (sb-bsd-sockets:socket-close listener))))))

(-> localgroup--wake-server (localgroup-session) null)
(defun localgroup--wake-server (session)
  "Wake SESSION's blocking accept loop without authenticating a request."
  (handler-case
      (multiple-value-bind (socket stream)
          (daemon-connect (localgroup-session-port session))
        (declare (ignore socket))
        (close stream))
    (error () nil))
  nil)

(-> localgroup-stop (application) null)
(defun localgroup-stop (application)
  "Stop and unpublish APPLICATION's localgroup endpoint idempotently."
  (let ((session (application-localgroup-session application)))
    (when session
      (setf (application-localgroup-session application) nil)
      (let ((server-thread nil)
            (watchdog-thread nil)
            (client-threads nil)
            (client-sockets nil)
            (listener nil))
        (with-lock-held ((localgroup-session-lock session))
          (setf (localgroup-session-stopping-p session) t
                server-thread (localgroup-session-server-thread session)
                watchdog-thread
                (localgroup-session-attach-watchdog-thread session)
                client-threads
                (copy-list (localgroup-session-client-threads session))
                client-sockets
                (copy-list (localgroup-session-client-sockets session))
                listener (localgroup-session-listener session)))
        (localgroup-stop-thread watchdog-thread)
        (when listener
          (localgroup--wake-server session))
        (dolist (socket client-sockets)
          (ignore-errors
            (sb-bsd-sockets:socket-shutdown socket :direction ':io))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (localgroup-stop-thread server-thread)
        (when listener
          (ignore-errors (sb-bsd-sockets:socket-close listener))
          (with-lock-held ((localgroup-session-lock session))
            (when (eq listener (localgroup-session-listener session))
              (setf (localgroup-session-listener session) nil))))
        (dolist (thread client-threads)
          (localgroup-stop-thread thread))
        (localgroup--delete-owned-registry session))))
  nil)
