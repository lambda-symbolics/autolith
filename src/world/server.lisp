(in-package #:autolith)

;;;; -- World Server --

;;; One process serves a world. Every open channel is an application of its
;;; own, hosted on its own thread with its own localgroup endpoint, so
;;; terminals attach, tell, detach and kill per channel exactly as before.
;;; The server answers at one more endpoint, "world", through which clients
;;; open channels, and it stays alive without clients until it is stopped.

(defparameter *world-server-identifier* "world"
  "The endpoint identifier under which a world server answers.")

(defparameter *world-server-status-version* 1
  "The world status wire version.")

(defparameter *world-server-open-timeout-seconds* 60
  "How long an open waits for its channel's endpoint before it fails.")

(defclass world-host ()
  ((application
    :initarg       :application
    :reader        world-host-application
    :type          application
    :documentation "The hosted application serving one channel.")
   (thread
    :initform      nil
    :accessor      world-host-thread
    :type          t
    :documentation "The thread serving the application.")
   (ready-p
    :initform      nil
    :accessor      world-host-ready-p
    :type          boolean
    :documentation "Whether the channel's endpoint has been published.")
   (failure
    :initform      nil
    :accessor      world-host-failure
    :type          (option condition)
    :documentation "The condition that ended the session, or NIL.")
   (exit-reason
    :initform      nil
    :accessor      world-host-exit-reason
    :type          (option keyword)
    :documentation "The session's exit reason once it ended, or NIL while it runs.")
   (paused-p
    :initform      nil
    :accessor      world-host-paused-p
    :type          boolean
    :documentation "Whether the session ended for a checkpoint and waits to be served again.")
   (identity
    :initform      nil
    :accessor      world-host-identity
    :type          list
    :documentation "The :TOKEN and :CREATED-AT of the endpoint a paused session's clients wait for.")
   (quiesced-tools
    :initform      nil
    :accessor      world-host-quiesced-tools
    :type          list
    :documentation "The tool runtimes a checkpoint closed in this channel, restarted when it is served again."))
  (:documentation "One channel's application and the thread serving it.

A host paused for a checkpoint keeps its application; its thread ended so
the process could fork, and a new thread serves the application again."))

(defclass world-server ()
  ((configuration
    :initarg       :configuration
    :reader        world-server-configuration
    :type          configuration
    :documentation "The bootstrapped configuration hosted applications derive from.")
   (permission-mode
    :initarg       :permission-mode
    :reader        world-server-permission-mode
    :type          (member :ask :auto :sandboxed :full-access)
    :documentation "The permission mode a channel opens with unless its client chooses one.")
   (pristine-p
    :initarg       :pristine-p
    :reader        world-server-pristine-p
    :type          boolean
    :documentation "Whether the server boots tracked source without private mutations.")
   (mutation-replay-failures
    :initarg       :mutation-replay-failures
    :reader        world-server-mutation-replay-failures
    :type          list
    :documentation "The private image replay failures every hosted application reports.")
   (checkpointing-p
    :initform      nil
    :accessor      world-server-checkpointing-p
    :type          boolean
    :documentation "Whether a checkpoint pauses the channels; opens are refused until it is over.")
   (holding-p
    :initform      nil
    :accessor      world-server-holding-p
    :type          boolean
    :documentation "Whether a checkpoint waits for the work under way; no channel starts a new turn meanwhile.")
   (lease
    :initform      nil
    :accessor      world-server-lease
    :type          t
    :documentation "The process lease on the world, held while this server runs.")
   (token
    :initform      nil
    :accessor      world-server-token
    :type          (option string)
    :documentation "The endpoint token clients hold, kept when the endpoint is published again.")
   (created-at
    :initform      nil
    :accessor      world-server-created-at
    :type          (option integer)
    :documentation "The endpoint creation time clients know, kept when the endpoint is published again.")
   (lock
    :initform      (make-lock "Autolith world server")
    :reader        world-server-lock
    :type          t
    :documentation "Protects the hosts, the jobs and the stop flag.")
   (open-lock
    :initform      (make-lock "Autolith world server opens")
    :reader        world-server-open-lock
    :type          t
    :documentation "Serializes channel selection and host registration.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith world server")
    :reader        world-server-condition-variable
    :type          t
    :documentation "Wakes the main thread for jobs and waiters for readiness and exits.")
   (hosts
    :initform      nil
    :accessor      world-server-hosts
    :type          list
    :documentation "The hosts of every open channel, newest first.")
   (jobs
    :initform      nil
    :accessor      world-server-jobs
    :type          list
    :documentation "Thunks waiting for the main thread, oldest first.")
   (stopping-p
    :initform      nil
    :accessor      world-server-stopping-p
    :type          boolean
    :documentation "Whether the server is closing its channels to exit.")
   (exit-status
    :initform      nil
    :accessor      world-server-exit-status
    :type          (option integer)
    :documentation "The launcher status a hosted session asked the process to exit with.")
   (runtime
    :initform      nil
    :accessor      world-server-runtime
    :type          t
    :documentation "The published world endpoint, or NIL before start and after stop.")
   (management-repl-endpoint
    :initform      nil
    :accessor      world-server-management-repl-endpoint
    :type          t
    :documentation "The process's management evaluation endpoint, when enabled."))
  (:documentation "The process hosting every open channel of one world."))

(defclass world-endpoint (image-daemon:daemon-runtime)
  ((server
    :initarg       :server
    :reader        world-endpoint-server
    :type          world-server
    :documentation "The server answering at this endpoint."))
  (:documentation "The authenticated endpoint through which clients open channels."))

(defmethod management-repl-owner-configuration ((owner world-server))
  "The server configures the endpoint from its base configuration."
  (world-server-configuration owner))

(defmethod management-repl-owner-endpoint ((owner world-server))
  "The server keeps the process's endpoint in its own slot."
  (world-server-management-repl-endpoint owner))

(defmethod (setf management-repl-owner-endpoint) (endpoint (owner world-server))
  "The server keeps the process's endpoint in its own slot."
  (setf (world-server-management-repl-endpoint owner) endpoint))


;;;; -- Lifecycle --

(-> world-server-create
    (configuration &key (:permission-mode (member :ask :auto :sandboxed :full-access))
                        (:pristine-p boolean) (:token (option string))
                        (:created-at (option integer)))
    world-server)
(defun world-server-create
    (configuration &key (permission-mode ':ask) pristine-p token created-at)
  "Prepare this process as CONFIGURATION's world server and publish its endpoint.

TOKEN and CREATED-AT name the endpoint the client that started this process
waits for; without them the endpoint gets fresh ones. The process services,
the management REPL and Slynk, start with the server."
  (let* ((failures     (application--prepare-process configuration
                                                     :pristine-p pristine-p))
         (bootstrapped (with-extension-registry-transaction
                         (application--load-extension-configuration
                          configuration :pristine-p pristine-p)
                         (provider-bootstrap-configuration configuration)))
         (server       (make-instance 'world-server
                                      :configuration            bootstrapped
                                      :permission-mode          permission-mode
                                      :pristine-p               pristine-p
                                      :mutation-replay-failures failures))
         (completed-p  nil))
    (unwind-protect
         (progn
           (setf (world-server-lease server) (world-server--lease-acquire bootstrapped))
           (world-compact-when-grown bootstrapped)
           (localgroup-handoff-assert-startup-active)
           (world-server--runtime-start server :token token :created-at created-at)
           (localgroup-handoff-finish-startup nil)
           (management-repl-start server)
           (slynk-start bootstrapped :background-p t)
           (setf completed-p t)
           server)
      (unless completed-p
        (world-server--shutdown server)))))

(-> world-server-lock-pathname (configuration) pathname)
(defun world-server-lock-pathname (configuration)
  "Return the file whose process lease says a world server runs for CONFIGURATION's data root."
  (merge-pathnames "server.lock" (localgroup-world-directory configuration)))

(-> world-server--lease-acquire (configuration) t)
(defun world-server--lease-acquire (configuration)
  "Take the process lease on CONFIGURATION's world, or signal that another server holds it."
  (let ((pathname (world-server-lock-pathname configuration)))
    (ensure-directories-exist pathname)
    (handler-case
        (lease-acquire pathname)
      (file-lock-busy ()
        (error 'world-error
               :message   "Another world server already runs for this data root."
               :pathname  pathname
               :operation ':start)))))

(-> world-server-running-p (configuration) boolean)
(defun world-server-running-p (configuration)
  "Return true while a world server runs, whether or not its endpoint answers.

A server runs while a process holds the world's lease, and while it restarts
from its saved heap with the lease free. It publishes its endpoint again after
a checkpoint, so a client that finds it running waits for the endpoint
instead of starting a second server."
  (let ((pathname (world-server-lock-pathname configuration)))
    (or (and (probe-file pathname)
             (handler-case
                 (progn
                   (lease-release (lease-acquire pathname))
                   nil)
               (file-lock-busy ()
                 t)))
        (world-server-restarting-p configuration))))

(-> world-server--runtime-start
    (world-server &key (:token (option string)) (:created-at (option integer)))
    null)
(defun world-server--runtime-start (server &key token created-at)
  "Publish SERVER's endpoint under TOKEN and CREATED-AT, or under the ones clients already hold."
  (let ((runtime (image-daemon:daemon-runtime-create
                  :directory        (localgroup-world-directory (world-server-configuration server))
                  :identifier       *world-server-identifier*
                  :token            (or token (world-server-token server))
                  :created-at       (or created-at (world-server-created-at server))
                  :class            'world-endpoint
                  :initargs         (list :server server)
                  :request-function #'world-server--handle-request
                  :error-function   (lambda (condition)
                                      (format *error-output*
                                              "~&World endpoint failed: ~A~%" condition)))))
    (image-daemon:daemon-runtime-start runtime)
    (setf (world-server-runtime server) runtime
          (world-server-token server) (image-daemon:daemon-runtime-token runtime)
          (world-server-created-at server) (image-daemon:daemon-runtime-created-at runtime)))
  nil)

(-> world-server-run
    (configuration &key (:permission-mode (member :ask :auto :sandboxed :full-access))
                        (:pristine-p boolean))
    (option integer))
(defun world-server-run (configuration &key (permission-mode ':ask) pristine-p)
  "Serve CONFIGURATION's world from this process until it is stopped.

The endpoint takes the token of the startup handoff this process was
launched with, so the launching client finds it. Return the launcher status
a hosted session asked the process to exit with, or NIL."
  (let* ((startup-values (and *localgroup-startup-record*
                              (rest *localgroup-startup-record*)))
         (server         (world-server-create configuration
                                              :permission-mode permission-mode
                                              :pristine-p      pristine-p
                                              :token           (getf startup-values :token)
                                              :created-at      (getf startup-values :created-at))))
    (unwind-protect
         (world-server-serve server)
      (world-server--shutdown server))))

(-> world-server-serve (world-server) (option integer))
(defun world-server-serve (server)
  "Run SERVER's main-thread jobs until it is stopped and its last channel has ended.

Return the launcher status a hosted session asked for, or NIL."
  (let ((lock (world-server-lock server)))
    (loop
      (let ((job (with-lock-held (lock)
                   (loop
                     (cond
                       ((world-server-jobs server)
                        (return (pop (world-server-jobs server))))
                       ((and (world-server-stopping-p server)
                             (null (world-server-hosts server)))
                        (return nil))
                       (t
                        (condition-wait (world-server-condition-variable server) lock)))))))
        (if job
            (funcall job)
            (return (world-server-exit-status server)))))))

(-> world-server-stop (world-server) null)
(defun world-server-stop (server)
  "Ask every channel of SERVER to end and let the main thread exit after the last one."
  (let ((hosts (with-lock-held ((world-server-lock server))
                 (setf (world-server-stopping-p server) t)
                 (copy-list (world-server-hosts server)))))
    (dolist (host hosts)
      (world-server--close-host host))
    (world-server--wake server))
  nil)

(-> world-server--shutdown (world-server) null)
(defun world-server--shutdown (server)
  "Stop SERVER's endpoint and the process services after its channels ended."
  (world-server-stop server)
  (dolist (host (world-server--hosts server))
    (let ((thread (world-host-thread host)))
      (when (and thread (not (eq thread (current-thread))))
        (join-thread thread))))
  (let ((runtime (world-server-runtime server)))
    (when runtime
      (setf (world-server-runtime server) nil)
      (image-daemon:daemon-runtime-stop runtime)))
  ; mallet:suppress no-ignore-errors -- best-effort shutdown must close every service
  (ignore-errors (management-repl-stop server))
  ; mallet:suppress no-ignore-errors -- best-effort shutdown must close every service
  (ignore-errors (slynk-stop))
  ; mallet:suppress no-ignore-errors -- best-effort shutdown must close every service
  (ignore-errors (rlm-environment-pool-flush))
  (telemetry--call-safely #'telemetry-shutdown)
  (let ((lease (shiftf (world-server-lease server) nil)))
    (when lease
      ; mallet:suppress no-ignore-errors -- the lease dies with the process anyway
      (ignore-errors (lease-release lease))))
  nil)

(-> world-server--hosts (world-server) list)
(defun world-server--hosts (server)
  "Return a snapshot of SERVER's hosts, newest first."
  (with-lock-held ((world-server-lock server))
    (copy-list (world-server-hosts server))))

(-> world-server--wake (world-server) null)
(defun world-server--wake (server)
  "Wake SERVER's main thread and every waiter."
  (with-lock-held ((world-server-lock server))
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  nil)


;;;; -- Requests --

(-> world-server--handle-request
    (world-endpoint list &key (:socket t) (:stream t))
    null)
(defun world-server--handle-request (runtime request &key socket stream)
  "Answer one authenticated world REQUEST on STREAM."
  (declare (ignore socket))
  (let* ((server    (world-endpoint-server runtime))
         (operation (getf (rest request) :operation))
         (arguments (getf (rest request) :arguments))
         (response
           (handler-case
               (world-server--dispatch server operation arguments)
             (error (condition)
               (append (list :error :message (bounded-string (princ-to-string condition) :limit 1000))
                       (when (and (typep condition 'world-error)
                                  (eq (world-error-operation condition) ':checkpointing))
                         (list :retry-p t)))))))
    (daemon-write-packet stream response))
  nil)

(-> world-server--dispatch (world-server keyword t) list)
(defun world-server--dispatch (server operation arguments)
  "Return SERVER's response to OPERATION with ARGUMENTS."
  (case operation
    (:status
     (list :ok :status (world-server-status server)))
    (:open
     (world-server-open server arguments))
    (:channels
     (list :ok :channels (world-server-channels server)))
    (:close
     (world-server-close server (getf arguments :channel)
                         :unattached-only-p (not (null (getf arguments :unattached-only-p)))))
    (:move
     (world-server-move server (getf arguments :participant) (getf arguments :channel)))
    (:dismiss
     (world-server-dismiss server (getf arguments :participant)))
    (:person-unread
     (world-server-person-unread-changed server)
     (list :ok :operation ':person-unread))
    (:stop
     (world-server-stop server)
     (list :ok :operation ':stop :session-id *world-server-identifier*))
    (otherwise
     (list :error :message (format nil "Unknown world operation ~S." operation)))))

(-> world-server-status (world-server) list)
(defun world-server-status (server)
  "Return SERVER's portable status: its process, version, channels and stop state."
  (let ((runtime (world-server-runtime server)))
    (list :world-status
          :version          *world-server-status-version*
          :session-id       *world-server-identifier*
          :pid              (current-process-id)
          :autolith-version *autolith-version*
          :created-at       (and runtime (image-daemon:daemon-runtime-created-at runtime))
          :stopping-p       (not (null (world-server-stopping-p server)))
          :channels         (mapcar #'world-host-channel (world-server--hosts server)))))

(-> world-server-channels (world-server) list)
(defun world-server-channels (server)
  "Return the localgroup status of every channel SERVER serves."
  (loop for host in (world-server--hosts server)
        for session = (application-localgroup-session (world-host-application host))
        when (and session (world-host-ready-p host) (null (world-host-exit-reason host)))
          collect (localgroup-status-snapshot session)))

(-> world-server-person-unread-changed (world-server) null)
(defun world-server-person-unread-changed (server)
  "Count the messages waiting in the person's inbox again and show the count in every channel SERVER serves."
  (let ((unread (handler-case (world-person-unread (world-server-configuration server))
                  (autolith-error ()
                    nil))))
    (when unread
      (dolist (host (world-server--hosts server))
        (application-world-show-person-unread (world-host-application host) unread))))
  nil)

(-> world-server-open (world-server list) list)
(defun world-server-open (server arguments)
  "Open the channel ARGUMENTS select and answer with its session identifier.

ARGUMENTS name a :CHANNEL or a :DIRECTORY, and carry the client's
:PERMISSION-MODE, :FULLSCREEN-P, :RESUME-COMMAND-P, :RECOVERY-DIAGNOSIS,
:ROWS, :COLUMNS and :STYLED-P. :IMAGES, absolute namestrings of local
images, replace the channel's composer draft, whether the channel started
now or ran already. The answer arrives once the channel's endpoint is
published, so the client can attach at once."
  (multiple-value-bind (host started-p)
      (with-lock-held ((world-server-open-lock server))
        ;; Admission and host registration happen under one lock, the one a
        ;; checkpoint takes before it declares itself, so no host can start
        ;; between the checkpoint's snapshot of the hosts and its pause.
        (when (world-server-stopping-p server)
          (error 'world-error
                 :message   "The world server is stopping."
                 :operation ':open))
        (when (world-server-checkpointing-p server)
          (error 'world-error
                 :message   "The world server is checkpointing; the channel opens once it resumes."
                 :operation ':checkpointing))
        (world-server--host-for server arguments))
    (world-server--wait-ready server host)
    (let ((images (getf arguments :images)))
      (when images
        (terminal-ui-set-input (application-ui (world-host-application host))
                               (terminal-ui-image-draft
                                (mapcar #'image-input-validate-pathname images)))))
    (list :ok
          :session-id (world-host-channel host)
          :started-p  (not (null started-p)))))

(-> world-server-close (world-server t &key (:unattached-only-p boolean)) list)
(defun world-server-close (server channel &key unattached-only-p)
  "End the session of CHANNEL and answer once its end is requested.

With UNATTACHED-ONLY-P the channel is closed only while no terminal controls
it, and the answer says whether it was: a client cleaning up a channel it
started must not end one another terminal attached to since."
  (unless (stringp channel)
    (error 'world-error
           :message   "A close names the channel to close."
           :operation ':close))
  (let ((host (world-server--find-host server channel)))
    (unless host
      (error 'world-error
             :message    (format nil "Channel ~A is not open in this world server."
                                 (conversation-identifier-display channel))
             :operation  ':close
             :identifier channel))
    (let ((session (application-localgroup-session (world-host-application host))))
      (flet ((close-host ()
               "Close HOST and answer that it was."
               (world-server--close-host host)
               (list :ok :operation ':close :session-id (world-host-channel host) :closed-p t)))
        (cond
          ((not unattached-only-p)
           (close-host))
          ((null session)
           ;; No endpoint yet, so nothing can attach before the close.
           (close-host))
          (t
           ;; The check and the commitment share the lock under which the
           ;; endpoint admits terminals, so none slips in between them, and
           ;; none is admitted after the close is committed.
           (let ((committed-p
                   (with-lock-held ((image-daemon:daemon-runtime-lock session))
                     (unless (world-host-attached-p host)
                       (setf (localgroup-session-closing-p session) t)))))
             (if committed-p
                 (close-host)
                 (list :ok :operation ':close :session-id (world-host-channel host)
                           :closed-p nil)))))))))


;;;; -- Moves --

(-> world-server-move (world-server t t) list)
(defun world-server-move (server participant channel)
  "Move agent PARTICIPANT into CHANNEL, both designators, and answer with what happened.

The channel it leaves, when served, stops it first; the destination, when
served, adopts it with the context it carries, else that context waits for
the channel's next open. Nothing changes when the agent cannot stop or the
destination cannot take it."
  (unless (and (stringp participant) (stringp channel))
    (error 'world-error
           :message   "A move names the participant to move and the channel to move it into."
           :operation ':move))
  (let* ((configuration (world-server-configuration server))
         (world         (values (world-reconcile-sessions configuration)))
         (agent         (world-server--designated-participant world participant ':move))
         (target        (channel-identifier (world-channel-designated world configuration channel)))
         (source-host   (and (participant-residence agent)
                             (world-server--find-host server (participant-residence agent))))
         (target-host   (world-server--find-host server target))
         (result        (world-move-agent configuration agent target
                                          :source-application      (and source-host
                                                                        (world-host-application source-host))
                                          :destination-application (and target-host
                                                                        (world-host-application target-host))
                                          :interrupt-task          (lambda (participant)
                                                                     (world-server--interrupt-task
                                                                      server participant)))))
    (list :ok :operation ':move :participant (participant-identifier agent)
              :source (getf result :source) :channel target :dropped (getf result :dropped))))

(-> world-server-dismiss (world-server t) list)
(defun world-server-dismiss (server participant)
  "Park agent PARTICIPANT, a designator, leaving its channel without an agent, and answer with what happened.

The channel, when served, stops the agent first. The context it carries
waits in its context file for the channel a later move brings it into."
  (unless (stringp participant)
    (error 'world-error
           :message   "A dismissal names the agent to park."
           :operation ':park))
  (let* ((configuration (world-server-configuration server))
         (world         (values (world-reconcile-sessions configuration)))
         (agent         (world-server--designated-participant world participant ':park))
         (residence     (participant-residence agent))
         (host          (and residence (world-server--find-host server residence)))
         (result        (world-park-agent configuration agent
                                          :application    (and host (world-host-application host))
                                          :interrupt-task (lambda (participant)
                                                            (world-server--interrupt-task
                                                             server participant)))))
    (list :ok :operation ':park :participant (participant-identifier agent)
              :name (participant-name agent)
              :channel (getf result :channel) :dropped (getf result :dropped))))

(-> world-server--interrupt-task (world-server participant) boolean)
(defun world-server--interrupt-task (server participant)
  "Stop the child task any channel SERVER hosts runs agent PARTICIPANT in, and return true when there was one."
  (let ((interrupted-p nil))
    (dolist (host (world-server--hosts server))
      (when (application-world-interrupt-task (world-host-application host) participant)
        (setf interrupted-p t)))
    interrupted-p))

(-> world-server--designated-participant (world string keyword) participant)
(defun world-server--designated-participant (world designator operation)
  "Return the participant DESIGNATOR names in WORLD, or refuse OPERATION."
  (or (world-participant-designated world designator)
      (error 'world-error
             :message    (format nil "No participant is named ~A." designator)
             :pathname   nil
             :operation  operation
             :identifier designator)))


;;;; -- Hosting --

(-> world-host-channel (world-host) string)
(defun world-host-channel (host)
  "Return the identifier of the channel HOST serves."
  (conversation-identifier
   (application-conversation (world-host-application host))))

(-> world-host-workspace (world-host) string)
(defun world-host-workspace (host)
  "Return the canonical workspace of the channel HOST serves."
  (world-workspace-name
   (config :working-directory
           (application-configuration (world-host-application host)))))

(-> world-host-attached-p (world-host) boolean)
(defun world-host-attached-p (host)
  "Return true while a terminal controls HOST's channel or a structured client follows it."
  (let* ((application (world-host-application host))
         (ui          (application-ui application))
         (terminal    (and ui (terminal-ui-terminal ui))))
    (or (and (typep terminal 'localgroup-terminal)
             (image-daemon:relay-attached-p terminal))
        (application-clients-attached-p application))))

(-> world-host-live-p (world-host) boolean)
(defun world-host-live-p (host)
  "Return true while HOST serves its channel and has not begun to end."
  (and (world-host-ready-p host)
       (null (world-host-exit-reason host))
       (let ((controller (application-input-controller (world-host-application host))))
         (and controller
              (not (application-input-controller-exit-requested-p controller))))))

(-> world-host-reserved-p (world-host) boolean)
(defun world-host-reserved-p (host)
  "Return true while HOST holds its channel: still starting, or serving and not ending.

A starting host is a reservation: an open of the same channel or workspace
waits for it rather than starting another, so one channel never gets two
hosts, two leases or two threads."
  (if (world-host-ready-p host)
      (world-host-live-p host)
      (null (world-host-failure host))))

(-> world-server--find-host (world-server string) (option world-host))
(defun world-server--find-host (server identifier)
  "Return SERVER's host holding channel IDENTIFIER, starting or serving, or NIL."
  (find-if (lambda (host)
             (and (world-host-reserved-p host)
                  (string= identifier (world-host-channel host))))
           (world-server--hosts server)))

(-> world-server--host-for (world-server list) (values world-host boolean))
(defun world-server--host-for (server arguments)
  "Return the host selected by ARGUMENTS and whether it was started now.

For a named channel, reuse its host or start one. For a directory, try its
top-level channels by latest activity. Reuse an unattached compatible host,
or start an unhosted channel whose conversation is free. Create a channel
if none qualifies, or at once when ARGUMENTS ask for a new one with :new-p.
The caller waits for the host to publish its endpoint."
  (let ((channel   (getf arguments :channel))
        (directory (getf arguments :directory)))
    (cond
      ((stringp channel)
       (let* ((configuration (world-server-configuration server))
              (identifier    (conversation-identifier-migration-resolve configuration channel))
              (host          (world-server--find-host server identifier)))
         (if host
             (progn
               (when (getf arguments :mcp-servers)
                 (error 'world-error
                        :message    (format nil "Channel ~A is already running, so its tools cannot take the editor's MCP servers. Open it without them, or close it elsewhere first."
                                            (conversation-identifier-display identifier))
                        :operation  ':open
                        :identifier identifier))
               (world-server--reconcile-launch server host (getf arguments :launch))
               (values host nil))
             (values (world-server--start-host
                      server
                      :conversation-id identifier
                      :directory       (world-server--channel-workspace server identifier)
                      :arguments       arguments)
                     t))))
      ((stringp directory)
       (unless (uiop:directory-exists-p directory)
         (error 'world-error
                :message   (format nil "The directory ~A does not exist." directory)
                :operation ':open))
       (let ((workspace (world-workspace-name directory)))
         (multiple-value-bind (host started-p)
             (if (getf arguments :new-p)
                 (values nil nil)
                 (world-server--latest-host server workspace arguments))
           (if host
               (values host started-p)
               (values (world-server--start-host server
                                                 :directory workspace
                                                 :arguments arguments)
                       t)))))
      (t
       (error 'world-error
              :message   "An open names a channel or a directory."
              :operation ':open)))))

(-> world-server--channel-workspace (world-server string) string)
(defun world-server--channel-workspace (server identifier)
  "Return the workspace channel IDENTIFIER records, or the server's own directory."
  (let* ((configuration (world-server-configuration server))
         (world         (values (world-reconcile-sessions configuration)))
         (channel       (world-channel-find world identifier)))
    (if channel
        (channel-workspace channel)
        (world-workspace-name (config :working-directory configuration)))))

(-> world-server--latest-candidates (world-server string) list)
(defun world-server--latest-candidates (server workspace)
  "Return (IDENTIFIER . HOST) for WORKSPACE's top-level channels, the latest touched first.

HOST is the server's host of the channel, starting or serving, or NIL. Latest
touched is the newest write of the channel's conversation file; a hosted
channel with no file yet was started just now and counts as the newest. The
person's inbox is never a candidate: it is opened only by name."
  (let* ((configuration (world-server-configuration server))
         (world         (values (world-reconcile-sessions configuration)))
         (hosts         (remove-if-not (lambda (host)
                                         (and (world-host-reserved-p host)
                                              (string= workspace (world-host-workspace host))))
                                       (world-server--hosts server)))
         (candidates    nil))
    (dolist (channel (world-channels world))
      (when (and (null (channel-parent channel))
                 (not (channel-inbox-p channel))
                 (string= workspace (channel-workspace channel)))
        (push (cons (channel-identifier channel)
                    (find (channel-identifier channel) hosts
                          :key #'world-host-channel :test #'string=))
              candidates)))
    (dolist (host hosts)
      (unless (or (assoc (world-host-channel host) candidates :test #'string=)
                  (world-channel-find world (world-host-channel host)))
        (push (cons (world-host-channel host) host) candidates)))
    (flet ((touched (candidate)
             (let ((written (conversation-storage-write-date
                             (conversation-pathname-for-id configuration (first candidate)))))
               (if (and (zerop written) (rest candidate))
                   (get-universal-time)
                   written))))
      (mapcar #'rest
              (stable-sort (mapcar (lambda (candidate) (cons (touched candidate) candidate))
                                   candidates)
                           #'> :key #'first)))))

(-> world-server--latest-host (world-server string list) (values (option world-host) boolean))
(defun world-server--latest-host (server workspace arguments)
  "Return the host of WORKSPACE's latest touched free channel, and whether it was started now.

A hosted channel serves when no client controls it, the launch options of
ARGUMENTS can apply to it and they bring no MCP servers its tools would need;
an unhosted one starts when its conversation is free. Return NIL when no
channel qualifies."
  (let ((launch (getf arguments :launch)))
    (dolist (candidate (world-server--latest-candidates server workspace) (values nil nil))
      (destructuring-bind (identifier . host) candidate
        (cond
          (host
           (unless (or (world-host-attached-p host)
                       (getf arguments :mcp-servers)
                       (world-server--launch-conflict host launch))
             (world-server--reconcile-launch server host launch)
             (return (values host nil))))
          (t
           (handler-case
               (return (values (world-server--start-host server
                                                         :conversation-id identifier
                                                         :directory       workspace
                                                         :arguments       arguments)
                               t))
             (conversation-in-use ()
               nil))))))))

(-> world-server--launch-conflict (world-host list) (option string))
(defun world-server--launch-conflict (host launch)
  "Return the option of LAUNCH that channel HOST runs differently and cannot change, or NIL.

The model, provider, immutable mode, fullscreen or plain presentation and
theme are fixed when a channel's application is built. The permission mode
changes only while the channel is not working: a turn or a job already
running keeps the mode it started under."
  (let* ((application   (world-host-application host))
         (configuration (application-configuration application))
         (model         (getf launch :model))
         (provider      (getf launch :provider))
         (theme         (getf launch :theme))
         (mode          (getf launch :permission-mode)))
    (cond
      ((and mode
            (not (eq mode (application-permission-mode application)))
            (world-host-working-p host))
       (format nil "--permissions ~(~A~) while it is working" mode))
      ((and model (not (equal model (config :model configuration))))
       (format nil "--model ~A; it runs ~A" model (config :model configuration)))
      ((and provider (not (equal provider (config :model-provider configuration))))
       (format nil "--provider ~A" provider))
      ((and (getf launch :immutable-p) (not (config :immutable-p configuration)))
       "--immutable")
      ((and (getf launch :fullscreen-p) (not (config :fullscreen-p configuration)))
       "--fullscreen")
      ((and (getf launch :plain-p) (config :fullscreen-p configuration))
       "--plain")
      ((and theme (not (eq theme (config :terminal-theme configuration))))
       "--almighty")
      (t
       nil))))

(-> world-server--reconcile-launch (world-server world-host list) null)
(defun world-server--reconcile-launch (server host launch)
  "Apply compatible LAUNCH options to HOST or signal a conflict.

Change the permission mode only when no terminal controls the channel and
no turn or job is running. A request for the current mode needs no change."
  (let ((conflict (world-server--launch-conflict host launch))
        (display  (conversation-identifier-display (world-host-channel host))))
    (when conflict
      (error 'world-error
             :message    (format nil "Channel ~A is already running and cannot take ~A. Open it without that option, or open another channel."
                                 display conflict)
             :operation  ':open
             :identifier (world-host-channel host)))
    (let ((mode        (getf launch :permission-mode))
          (application (world-host-application host)))
      (when (and mode (not (eq mode (application-permission-mode application))))
        (when (world-host-attached-p host)
          (error 'world-error
                 :message    (format nil "Channel ~A is controlled by another terminal and keeps its permission mode ~(~A~)."
                                     display (application-permission-mode application))
                 :operation  ':open
                 :identifier (world-host-channel host)))
        (setf (application-permission-mode application)
              (world-server--permission-mode server mode))
        (application-present application
                             (format nil "· permission mode changed to ~(~A~) by a terminal opening this channel"
                                     (application-permission-mode application))))))
  nil)

(-> world-host-working-p (world-host) boolean)
(defun world-host-working-p (host)
  "Return true while HOST's channel runs a turn or a job."
  (let* ((application (world-host-application host))
         (controller  (application-input-controller application)))
    (or (and controller (application-input-controller-turn-active-p controller))
        (plusp (nth-value 0 (localgroup--task-counts application))))))

(-> world-server--permission-mode (world-server t) keyword)
(defun world-server--permission-mode (server value)
  "Return the permission mode VALUE selects, or SERVER's default."
  (cond
    ((null value)
     (world-server-permission-mode server))
    ((member value '(:ask :auto :sandboxed :full-access))
     value)
    (t
     (error 'world-error
            :message   (format nil "Unknown permission mode ~S." value)
            :operation ':open))))

(-> world-server--start-host
    (world-server &key (:conversation-id (option string)) (:directory string)
                       (:arguments list))
    world-host)
(defun world-server--start-host (server &key conversation-id directory arguments)
  "Build the application of CONVERSATION-ID, or a new one, in DIRECTORY and serve it.

ARGUMENTS carry the opening client's choices, among them the :LAUNCH options
it stated explicitly, which the new channel takes, and an editor's
:MCP-SERVERS, which its tools include. The host is listed before
its thread starts, so a second open of the same channel waits for it, and is
forgotten again when no thread could start."
  (let ((launch (getf arguments :launch)))
    (when (and (getf launch :provider) (null (getf launch :model)))
      (error 'world-error
             :message   "--provider requires --model MODEL; a channel cannot take a provider alone."
             :operation ':open)))
  (let* ((base          (world-server-configuration server))
         (launch        (getf arguments :launch))
         (configuration (apply #'configuration-copy base
                               :working-directory (uiop:ensure-directory-pathname directory)
                               (append
                                (list :fullscreen-p (not (null (or (getf arguments :fullscreen-p)
                                                                   (getf launch :fullscreen-p)))))
                                (when (getf launch :model)
                                  (list :model                      (getf launch :model)
                                        :model-provider             (or (getf launch :provider) "")
                                        :model-selection-override-p t))
                                (when (getf launch :immutable-p)
                                  (list :immutable-p t))
                                (when (getf launch :theme)
                                  (list :terminal-theme (getf launch :theme))))))
         (application   (application--build
                         configuration
                         :conversation-id          conversation-id
                         :permission-mode          (world-server--permission-mode
                                                    server (or (getf launch :permission-mode)
                                                               (getf arguments :permission-mode)))
                         :pristine-p               (world-server-pristine-p server)
                         :mutation-replay-failures (world-server-mutation-replay-failures server)
                         :relay-presentation       (list :rows     (getf arguments :rows)
                                                         :columns  (getf arguments :columns)
                                                         :styled-p (getf arguments :styled-p))
                         :tool-registry-function   (let ((servers (getf arguments :mcp-servers)))
                                                     (when servers
                                                       (lambda (configuration)
                                                         (acp-mcp-create-tool-registry
                                                          configuration
                                                          (coerce (client-json servers) 'list)))))))
         (host          (make-instance 'world-host :application application)))
    (setf (application-world-server application) server)
    (with-lock-held ((world-server-lock server))
      (push host (world-server-hosts server)))
    (handler-case
        (progn
          ;; A context waiting for the channel's agent is adopted now; one
          ;; that cannot be is told and tried again before the agent's turn.
          (application-world-adopt-pending-context application)
          (world-server--serve-host server host
                                    :initial-command    (and (getf arguments :resume-command-p)
                                                             "(switch)")
                                    :recovery-diagnosis (getf arguments :recovery-diagnosis)
                                    :resume-offer-p     (not (null (getf arguments :resume-command-p)))
                                    :boot-screen-p      (not (getf arguments :switch-p))))
      (error (condition)
        (application--discard-connection-resources application
                                                   (application-tool-registry application)
                                                   (application-worker application))
        (application-release-conversation-lease application)
        (world-server--release-host server host)
        (error condition)))
    host))

(-> world-server--serve-host
    (world-server world-host &key (:initial-command (option string))
                                  (:recovery-diagnosis (option string))
                                  (:resume-offer-p boolean)
                                  (:resume-p boolean)
                                  (:reconnect-id (option string))
                                  (:notice (or null string list))
                                  (:boot-screen-p boolean))
    null)
(defun world-server--serve-host (server host &key initial-command recovery-diagnosis
                                                  resume-offer-p resume-p reconnect-id notice
                                                  (boot-screen-p t))
  "Start the thread serving HOST's application, serving it again after a checkpoint with RESUME-P.

Without BOOT-SCREEN-P the channel opens without the boot screen."
  (setf (world-host-thread host)
        (make-thread
         (lambda ()
           (world-server--host server host
                               :initial-command    initial-command
                               :recovery-diagnosis recovery-diagnosis
                               :resume-offer-p     resume-offer-p
                               :resume-p           resume-p
                               :reconnect-id       reconnect-id
                               :notice             notice
                               :boot-screen-p      boot-screen-p))
         :name (format nil "Autolith channel ~A"
                       (conversation-identifier-display (world-host-channel host)))))
  nil)

(-> world-server--thread-bindings (application) list)
(defun world-server--thread-bindings (application)
  "Return the special bindings threads serving APPLICATION start with.

Threads inherit no dynamic bindings, so the hosting thread's APPLICATION
reaches endpoint, tool and job threads through this alist, which names
itself so that threads those threads start inherit it too."
  (let* ((inherited (cons 'bordeaux-threads:*default-special-bindings* nil))
         (bindings  (list (cons '*active-application* (list 'quote application))
                          (cons '*localgroup-startup-record* nil)
                          inherited)))
    (setf (rest inherited) (list 'quote bindings))
    bindings))

(-> world-server--host
    (world-server world-host &key (:initial-command (option string))
                                  (:recovery-diagnosis (option string))
                                  (:resume-offer-p boolean)
                                  (:resume-p boolean)
                                  (:reconnect-id (option string))
                                  (:notice (or null string list))
                                  (:boot-screen-p boolean))
    null)
(defun world-server--host (server host &key initial-command recovery-diagnosis resume-offer-p
                                            resume-p reconnect-id notice (boot-screen-p t))
  "Serve HOST's application on this thread until its session ends.

A launcher exit condition reaches the attached clients with its status, as a
direct session's would; an update or rollback then stops the whole server,
because the image is shared, while a fatal error ends this channel alone. A
session that ends for a checkpoint leaves HOST paused, to be served again on
another thread with RESUME-P, RECONNECT-ID for its clients and NOTICE.
Without BOOT-SCREEN-P the channel opens without the boot screen."
  (let* ((application                 (world-host-application host))
         (configuration               (application-configuration application))
         (bordeaux-threads:*default-special-bindings*
           (world-server--thread-bindings application))
         (*active-application*        application)
         (*localgroup-startup-record* nil)
         (*default-pathname-defaults* (config :working-directory configuration)))
    (unwind-protect
         (handler-case
             (handler-bind ((main-launcher-exit-condition
                              (lambda (condition)
                                (multiple-value-bind (status message details)
                                    (main--launcher-exit-text condition)
                                  (localgroup-finish-attachments application
                                                                 :status  status
                                                                 :message message
                                                                 :details details)))))
               (setf (world-host-exit-reason host)
                     (or (application-serve
                          application
                          :initial-command    initial-command
                          :recovery-diagnosis recovery-diagnosis
                          :resume-offer-p     resume-offer-p
                          :hosted-p           t
                          :resume-p           resume-p
                          :notice             notice
                          :boot-screen-p      boot-screen-p
                          :localgroup-identity
                          (and reconnect-id
                               (append (world-host-identity host)
                                       (list :checkpoint-reconnect-id reconnect-id)))
                          :quiescer           (lambda (function)
                                                (declare (ignore function))
                                                (error 'checkpoint-error
                                                       :message  "A hosted channel checkpoints through its world server."
                                                       :stage    ':validation
                                                       :pathname nil))
                          :ready-function     (lambda (served)
                                                (declare (ignore served))
                                                (world-server--note-ready server host)))
                         ':finished)))
           (main-launcher-exit-condition (condition)
             (world-server--note-launcher-exit server host condition))
           (error (condition)
             (world-server--note-failure server host condition)))
      (if (eq (world-host-exit-reason host) ':checkpoint)
          (with-lock-held ((world-server-lock server))
            (setf (world-host-paused-p host) t)
            (sb-thread:condition-broadcast (world-server-condition-variable server)))
          (world-server--release-host server host))))
  nil)

(-> world-server--note-ready (world-server world-host) null)
(defun world-server--note-ready (server host)
  "Record that HOST published its endpoint, closing it again when SERVER is stopping."
  (let ((stopping-p nil))
    (with-lock-held ((world-server-lock server))
      (setf (world-host-ready-p host) t
            stopping-p (world-server-stopping-p server))
      (sb-thread:condition-broadcast (world-server-condition-variable server)))
    (when stopping-p
      (world-server--close-host host)))
  nil)

(-> world-server--note-failure (world-server world-host condition) null)
(defun world-server--note-failure (server host condition)
  "Record CONDITION as what ended HOST's session, so an opener learns why."
  (with-lock-held ((world-server-lock server))
    (setf (world-host-failure host) condition
          (world-host-exit-reason host) ':failed)
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  (format *error-output* "~&Channel ~A ended: ~A~%"
          (conversation-identifier-display (world-host-channel host))
          condition)
  nil)

(-> world-server--note-launcher-exit (world-server world-host condition) null)
(defun world-server--note-launcher-exit (server host condition)
  "End HOST for launcher exit CONDITION, stopping SERVER for an update or rollback."
  (with-lock-held ((world-server-lock server))
    (setf (world-host-exit-reason host) ':launcher-exit)
    (unless (typep condition 'fatal-control-path-error)
      (setf (world-server-exit-status server)
            (values (main--launcher-exit condition)))))
  (if (typep condition 'fatal-control-path-error)
      (world-server--wake server)
      (world-server-stop server))
  nil)

(-> world-server--release-host (world-server world-host) null)
(defun world-server--release-host (server host)
  "Forget HOST after its session ended, drop the editor credentials its MCP servers held, and wake whoever waits for it."
  (acp-mcp-release-credentials
   (mcp-tool-registry-manager (application-tool-registry (world-host-application host))))
  (with-lock-held ((world-server-lock server))
    (setf (world-server-hosts server) (remove host (world-server-hosts server)))
    (unless (or (world-host-ready-p host) (world-host-failure host))
      (setf (world-host-failure host)
            (make-condition 'world-error
                            :message    "The channel's session ended before its endpoint was published."
                            :operation  ':open
                            :identifier (world-host-channel host))))
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  nil)

(-> world-server--wait-ready (world-server world-host) world-host)
(defun world-server--wait-ready (server host)
  "Wait for HOST's endpoint, signaling a session failure or timeout."
  (let ((lock     (world-server-lock server))
        (deadline (+ (get-internal-real-time)
                     (* *world-server-open-timeout-seconds*
                        internal-time-units-per-second))))
    (with-lock-held (lock)
      (loop
        (cond
          ((world-host-failure host)
           (error 'world-error
                  :message    (format nil "The channel failed: ~A"
                                      (bounded-string (princ-to-string (world-host-failure host)) :limit 400))
                  :operation  ':open
                  :identifier (world-host-channel host)))
          ((world-host-ready-p host)
           (return host))
          ((>= (get-internal-real-time) deadline)
           (error 'world-error
                  :message    "The channel did not publish its endpoint in time."
                  :operation  ':open
                  :identifier (world-host-channel host)))
          (t
           (condition-wait (world-server-condition-variable server) lock
                           :timeout 0.5)))))))

(-> world-server--close-host (world-host) null)
(defun world-server--close-host (host)
  "Ask HOST's session to end the way a localgroup kill does."
  (let ((controller (application-input-controller (world-host-application host))))
    (when controller
      (application-input-controller--request-exit controller ':localgroup-kill)))
  nil)


;;;; -- Checkpoints --

;;; A fork checkpoint needs the forking thread to be the only live thread.
;;; The server's main thread takes it. First it holds every channel's new
;;; turns and waits, however long that takes, until no turn runs and no job
;;; is live: work already under way finishes, with any jobs it starts and
;;; waits for. Then every channel's session ends, its clients told to
;;; reconnect, its thread joined, its tool runtimes and job pools closed, and
;;; the endpoints, the management REPL and Slynk stop around the fork. Every
;;; channel is then served again on a new thread and the held turns start.
;;; A channel asks for a checkpoint and lets its turn end, since its own
;;; thread is among those that must end.

(defparameter *world-server-checkpoint-poll-seconds* 0.05
  "How often a waiting checkpoint looks again for work under way.")

(defparameter *world-server-checkpoint-join-seconds* 60
  "How long a checkpoint waits for a pausing channel's thread to end before it gives up.")

(-> world-server-checkpoint (application t) string)
(defun world-server-checkpoint (application backend)
  "Schedule a checkpoint of the world server hosting APPLICATION with BACKEND, and say so.

The checkpoint runs on the server's main thread once this channel's turn has
ended, pausing every channel; the outcome is presented in each channel when
it is served again. Where the host cannot fork, the server saves its exact
heap and restarts from it instead."
  (let ((server       (application-world-server application))
        (reconnect-id (make-identifier))
        (restart-p    (platform-supports-p *platform* ':restartable-image-saver)))
    (unless (typep server 'world-server)
      (error 'checkpoint-error
             :message  "This session is not hosted by a world server."
             :stage    ':validation
             :pathname nil))
    (world-server--enqueue server
                           (lambda ()
                             (funcall (if restart-p
                                          #'world-server--restart-job
                                          #'world-server--checkpoint-job)
                                      server
                                      :application  application
                                      :backend      backend
                                      :reconnect-id reconnect-id)))
    (if restart-p
        "checkpoint scheduled: every channel will be paused while a checkpoint is saving, then resumed after the world server restarts from it"
        "checkpoint scheduled: every channel will be paused while a checkpoint is saving, then resumed")))

(-> world-server--enqueue (world-server function) null)
(defun world-server--enqueue (server job)
  "Run JOB on SERVER's main thread after the jobs before it."
  (with-lock-held ((world-server-lock server))
    (setf (world-server-jobs server) (append (world-server-jobs server) (list job)))
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  nil)

(-> world-server-call-quiesced (world-server application function) t)
(defun world-server-call-quiesced (server application function)
  "Run FUNCTION on SERVER's main thread with APPLICATION's checkpoint quiescer bound, and return its values.

FUNCTION may call the quiescer with a thunk, which then runs with every
channel paused and the process services stopped, as a checkpoint backend
does; the channels are served again once FUNCTION returns. The caller must
not be a channel's thread, which could not end for the pause."
  (let ((lock    (world-server-lock server))
        (done-p  nil)
        (results nil)
        (failure nil))
    (world-server--enqueue
     server
     (lambda ()
       (handler-case
           (setf results
                 (multiple-value-list
                  (world-server--call-with-quiescer server
                                                    :application  application
                                                    :function     function
                                                    :reconnect-id (make-identifier))))
         (serious-condition (condition)
           (setf failure condition)))
       (with-lock-held (lock)
         (setf done-p t)
         (sb-thread:condition-broadcast (world-server-condition-variable server)))))
    (with-lock-held (lock)
      (loop until done-p
            do (condition-wait (world-server-condition-variable server) lock)))
    (if failure
        (error failure)
        (values-list results))))

(-> world-server--checkpoint-job
    (world-server &key (:application application) (:backend t) (:reconnect-id string))
    null)
(defun world-server--checkpoint-job (server &key application backend reconnect-id)
  "Take the checkpoint APPLICATION asked for with BACKEND, pausing every channel for the fork."
  (let ((outcome nil))
    (flet ((notice (served)
             "Return what channel SERVED is told about the outcome."
             (cond
               ((typep outcome 'condition)
                (application-failure-line
                 (format nil "~:[world server checkpoint~;checkpoint~] failed: ~A"
                         (eq served application)
                         (bounded-string (princ-to-string outcome) :limit 400))))
               ((eq served application)
                (application-notice-line
                 (format nil "checkpoint ~A is publishing in process ~D"
                         (generation-identifier outcome)
                         (generation-coordinator-pid outcome))))
               (t
                (application-notice-line
                 (format nil "world server saved checkpoint ~A"
                         (generation-identifier outcome)))))))
      (world-server--call-with-quiescer
       server
       :application     application
       :function        (lambda ()
                          (handler-case
                              (setf outcome (checkpoint-create backend))
                            (serious-condition (condition)
                              (setf outcome condition))))
       :reconnect-id    reconnect-id
       :notice-function #'notice)))
  nil)

(-> world-server--call-with-quiescer
    (world-server &key (:application application) (:function function) (:reconnect-id string)
                       (:notice-function (option function)))
    t)
(defun world-server--call-with-quiescer (server &key application function reconnect-id
                                                     notice-function)
  "Call FUNCTION with the checkpoint quiescer bound, then serve every paused channel again.

The quiescer pauses the channels, telling their clients to reconnect under
RECONNECT-ID, and stops the process services around its thunk. The process
saves its heap with APPLICATION as the active application, so a rollback
into the checkpoint resumes that channel. NOTICE-FUNCTION, given an
application, returns what its channel is told once served again."
  (unwind-protect
       (progn
         (world-server--drain server)
         ;; An open admitted before this point holds the open lock until its
         ;; host is registered and served, so the checkpoint declares itself
         ;; only once every host it will pause is listed, and later opens are
         ;; refused.
         (with-lock-held ((world-server-open-lock server))
           (with-lock-held ((world-server-lock server))
             (setf (world-server-checkpointing-p server) t)))
         (setf *active-application* application)
         (let ((*checkpoint-thread-quiescer*
                 (lambda (thunk)
                   (world-server--pause-channels server application reconnect-id)
                   (world-server--call-with-services-stopped server thunk))))
           (funcall function)))
    (setf *active-application* nil)
    (world-server--resume-channels server application reconnect-id
                                   :notice-function notice-function)))

(-> world-server--drain (world-server) null)
(defun world-server--drain (server)
  "Hold every channel's new turns and wait until no turn runs and no job is live.

Opens still pass meanwhile: a channel opened now is held like the others.
Work already under way finishes, and so do the jobs it starts, since a turn
or a job may be waiting for them. The wait has no deadline."
  (with-lock-held ((world-server-lock server))
    (setf (world-server-holding-p server) t))
  (loop while (some #'world-server--host-busy-p (world-server--hosts server))
        do (sleep *world-server-checkpoint-poll-seconds*))
  nil)

(-> world-server--host-busy-p (world-host) boolean)
(defun world-server--host-busy-p (host)
  "Return true while HOST is starting, runs a turn, or has a live job."
  (let* ((application  (world-host-application host))
         (controller   (application-input-controller application))
         (orchestrator (application-task-presentation-orchestrator application)))
    (cond
      ((or (world-host-failure host) (world-host-exit-reason host))
       nil)
      ((null controller)
       t)
      ((application-input-controller-turn-active-p controller)
       t)
      ((and orchestrator (plusp (task-orchestrator-session-live-count orchestrator)))
       t)
      (t
       nil))))

(-> application-world-held-p (application) boolean)
(defun application-world-held-p (application)
  "Return true while APPLICATION's world server holds new turns for a checkpoint.

The input controller asks under its own lock, so the flag is read without the
server's lock; a turn that slips past a stale read is one the checkpoint then
waits for."
  (let ((server (application-world-server application)))
    (and (typep server 'world-server)
         (world-server-holding-p server)
         t)))

(-> world-server--pause-channels (world-server application string) null)
(defun world-server--pause-channels (server application reconnect-id)
  "End every channel's session for a checkpoint, join its thread and close its tool runtimes.

Each session's clients are told to reconnect under RECONNECT-ID first. The
server waits for a channel still starting or working, which after the drain
is only one opened since. The tool runtimes of every channel but
APPLICATION's, which its checkpoint backend closes, are closed and kept on
the host to restart: their job pools own threads the fork must not see."
  (let ((hosts (world-server--hosts server)))
    (flet ((controller (host)
             "Return HOST's input controller, or NIL."
             (application-input-controller (world-host-application host))))
      (loop while (some #'world-server--host-busy-p hosts)
            do (sleep *world-server-checkpoint-poll-seconds*))
      (dolist (host hosts)
        (let ((session (application-localgroup-session (world-host-application host))))
          (when (and session (controller host) (null (world-host-exit-reason host)))
            (setf (world-host-identity host)
                  (list :token      (image-daemon:daemon-runtime-token session)
                        :created-at (image-daemon:daemon-runtime-created-at session)))
            (localgroup--finish-checkpoint-attachments session reconnect-id)
            (application-input-controller--request-exit (controller host) ':checkpoint))))
      (dolist (host hosts)
        (let ((thread (world-host-thread host)))
          (when (and thread
                     (world-host-identity host)
                     (not (eq thread (current-thread)))
                     (eq (sb-thread:join-thread thread
                                                :default ':timeout
                                                :timeout *world-server-checkpoint-join-seconds*)
                         ':timeout))
            (error 'checkpoint-error
                   :message  (format nil "Channel ~A did not pause in time."
                                     (conversation-identifier-display (world-host-channel host)))
                   :stage    ':fork
                   :pathname nil))))
      (dolist (host hosts)
        (when (and (world-host-paused-p host)
                   (not (eq (world-host-application host) application)))
          (world-server--quiesce-host-tools host)))))
  nil)

(-> world-server--quiesce-host-tools (world-host) null)
(defun world-server--quiesce-host-tools (host)
  "Close HOST's tool runtimes for a checkpoint and keep them on HOST to restart."
  (multiple-value-bind (quiesced failure)
      (tool-registry-quiesce-runtime-state
       (application-tool-registry (world-host-application host)))
    (setf (world-host-quiesced-tools host) quiesced)
    (when failure
      (error 'checkpoint-error
             :message  (format nil "Channel ~A could not stop its tools: ~A"
                               (conversation-identifier-display (world-host-channel host))
                               (bounded-string (princ-to-string failure) :limit 400))
             :stage    ':fork
             :pathname nil)))
  nil)

(-> world-server--call-with-services-stopped (world-server function) t)
(defun world-server--call-with-services-stopped (server thunk)
  "Call THUNK with the world endpoint, the management REPL and Slynk stopped, then publish them again."
  (let ((runtime (shiftf (world-server-runtime server) nil)))
    (when runtime
      (image-daemon:daemon-runtime-stop runtime))
    (unwind-protect
         (management-repl-call-quiesced server (lambda () (slynk-call-quiesced thunk)))
      (world-server--runtime-start server))))

(-> world-server--resume-channels
    (world-server application string &key (:notice-function (option function)))
    null)
(defun world-server--resume-channels (server application reconnect-id &key notice-function)
  "Resume SERVER's paused channels, then admit opens and held turns.

Restart closed tool runtimes first. Report restart failures to the affected
channel and serve it without the tools that failed. Send APPLICATION its
notice directly if it never paused, as when a checkpoint fails early."
  (dolist (host (world-server--hosts server))
    (let ((served (world-host-application host))
          (tools  (shiftf (world-host-quiesced-tools host) nil)))
      (when tools
        (handler-case
            (tool-registry-resume-runtime-state (application-tool-registry served) :tools tools)
          (serious-condition (condition)
            (application-present served
                                 (application-failure-line
                                  (format nil "some tools did not restart after the checkpoint: ~A"
                                          (bounded-string (princ-to-string condition) :limit 400)))))))
      (cond
        ((world-host-paused-p host)
         (with-lock-held ((world-server-lock server))
           (setf (world-host-paused-p host) nil
                 (world-host-ready-p host) nil
                 (world-host-exit-reason host) nil))
         (world-server--serve-host server host
                                   :resume-p     t
                                   :reconnect-id reconnect-id
                                   :notice       (and notice-function
                                                      (funcall notice-function served))))
        ((and notice-function (eq served application))
         (application-present served (funcall notice-function served))))))
  (with-lock-held ((world-server-lock server))
    (setf (world-server-checkpointing-p server) nil
          (world-server-holding-p server)       nil)
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  (dolist (host (world-server--hosts server))
    (let ((controller (application-input-controller (world-host-application host))))
      (when controller
        (with-lock-held ((application-input-controller-lock controller))
          (sb-thread:condition-broadcast
           (application-input-controller-condition-variable controller))))))
  nil)
