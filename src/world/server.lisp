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
    :documentation "The condition that ended the session before it was ready, or NIL.")
   (exit-reason
    :initform      nil
    :accessor      world-host-exit-reason
    :type          (option keyword)
    :documentation "The session's exit reason once it ended, or NIL while it runs."))
  (:documentation "One channel's application and the thread serving it."))

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
   (lock
    :initform      (make-lock "Autolith world server")
    :reader        world-server-lock
    :type          t
    :documentation "Protects the hosts, the jobs and the stop flag.")
   (open-lock
    :initform      (make-lock "Autolith world server opens")
    :reader        world-server-open-lock
    :type          t
    :documentation "Serializes channel selection, so two opens of one directory share a channel.")
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
           (setf (world-server-runtime server)
                 (image-daemon:daemon-runtime-create
                  :directory        (localgroup-world-directory bootstrapped)
                  :identifier       *world-server-identifier*
                  :token            token
                  :created-at       created-at
                  :class            'world-endpoint
                  :initargs         (list :server server)
                  :request-function #'world-server--handle-request
                  :error-function   (lambda (condition)
                                      (format *error-output*
                                              "~&World endpoint failed: ~A~%" condition))))
           (localgroup-handoff-assert-startup-active)
           (image-daemon:daemon-runtime-start (world-server-runtime server))
           (localgroup-handoff-finish-startup nil)
           (management-repl-start server)
           (slynk-start bootstrapped :background-p t)
           (setf completed-p t)
           server)
      (unless completed-p
        (world-server--shutdown server)))))

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
               (list :error :message (bounded-string (princ-to-string condition) :limit 1000))))))
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
     (world-server-close server (getf arguments :channel)))
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

(-> world-server-open (world-server list) list)
(defun world-server-open (server arguments)
  "Open the channel ARGUMENTS select and answer with its session identifier.

ARGUMENTS name a :CHANNEL or a :DIRECTORY, and carry the client's
:PERMISSION-MODE, :FULLSCREEN-P, :RESUME-COMMAND-P, :RECOVERY-DIAGNOSIS,
:ROWS, :COLUMNS and :STYLED-P. The answer arrives once the channel's
endpoint is published, so the client can attach at once."
  (when (world-server-stopping-p server)
    (error 'world-error
           :message   "The world server is stopping."
           :operation ':open))
  (multiple-value-bind (host started-p)
      (with-lock-held ((world-server-open-lock server))
        (world-server--host-for server arguments))
    (world-server--wait-ready server host)
    (list :ok
          :session-id (world-host-channel host)
          :started-p  (not (null started-p)))))

(-> world-server-close (world-server t) list)
(defun world-server-close (server channel)
  "End the session of CHANNEL and answer once its end is requested."
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
    (world-server--close-host host)
    (list :ok :operation ':close :session-id (world-host-channel host))))


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
  "Return true while a client controls HOST's terminal."
  (let* ((ui       (application-ui (world-host-application host)))
         (terminal (and ui (terminal-ui-terminal ui))))
    (and (typep terminal 'localgroup-terminal)
         (image-daemon:relay-attached-p terminal))))

(-> world-host-live-p (world-host) boolean)
(defun world-host-live-p (host)
  "Return true while HOST serves its channel and has not begun to end."
  (and (world-host-ready-p host)
       (null (world-host-exit-reason host))
       (let ((controller (application-input-controller (world-host-application host))))
         (and controller
              (not (application-input-controller-exit-requested-p controller))))))

(-> world-server--find-host (world-server string) (option world-host))
(defun world-server--find-host (server identifier)
  "Return SERVER's live host of channel IDENTIFIER, or NIL."
  (find-if (lambda (host)
             (and (world-host-live-p host)
                  (string= identifier (world-host-channel host))))
           (world-server--hosts server)))

(-> world-server--host-for (world-server list) (values world-host boolean))
(defun world-server--host-for (server arguments)
  "Return the host serving the channel ARGUMENTS select, and whether it was started now.

A named channel is served by its live host or started. A directory is served
by a live host of that workspace nobody controls, else by the most recently
changed top-level channel of that workspace whose conversation is free, else
by a new channel there."
  (let ((channel   (getf arguments :channel))
        (directory (getf arguments :directory)))
    (cond
      ((stringp channel)
       (let* ((configuration (world-server-configuration server))
              (identifier    (conversation-identifier-migration-resolve configuration channel))
              (host          (world-server--find-host server identifier)))
         (if host
             (values host nil)
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
       (let* ((workspace (world-workspace-name directory))
              (idle      (world-server--idle-host server workspace)))
         (cond
           (idle
            (values idle nil))
           (t
            (values (or (world-server--resume-host server workspace arguments)
                        (world-server--start-host server
                                                  :directory workspace
                                                  :arguments arguments))
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

(-> world-server--idle-host (world-server string) (option world-host))
(defun world-server--idle-host (server workspace)
  "Return SERVER's live host of WORKSPACE that no client controls, or NIL."
  (find-if (lambda (host)
             (and (world-host-live-p host)
                  (string= workspace (world-host-workspace host))
                  (not (world-host-attached-p host))))
           (world-server--hosts server)))

(-> world-server--resume-candidates
    (configuration world &key (:workspace string) (:hosted list))
    list)
(defun world-server--resume-candidates (configuration world &key workspace hosted)
  "Return WORKSPACE's top-level channels of WORLD not among HOSTED, the most recently active first.

Activity is the newest durable write of a channel's conversation, so recent
work in an old channel outranks a newer channel's creation or a title change,
which only move the channel's own record."
  (stable-sort (remove-if-not (lambda (channel)
                                (and (null (channel-parent channel))
                                     (string= workspace (channel-workspace channel))
                                     (not (member (channel-identifier channel) hosted
                                                  :test #'string=))))
                              (world-channels world))
               #'>
               :key (lambda (channel)
                      (conversation-storage-write-date
                       (conversation-pathname-for-id configuration (channel-identifier channel))))))

(-> world-server--resume-host (world-server string list) (option world-host))
(defun world-server--resume-host (server workspace arguments)
  "Start the host of WORKSPACE's most recently active free channel, or return NIL."
  (let* ((configuration (world-server-configuration server))
         (world         (values (world-reconcile-sessions configuration)))
         (hosted        (mapcar #'world-host-channel (world-server--hosts server))))
    (dolist (channel (world-server--resume-candidates configuration world
                                                      :workspace workspace
                                                      :hosted    hosted)
                     nil)
      (handler-case
          (return (world-server--start-host server
                                            :conversation-id (channel-identifier channel)
                                            :directory       workspace
                                            :arguments       arguments))
        (conversation-in-use ()
          nil)))))

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

ARGUMENTS carry the opening client's choices. The host is listed before its
thread starts, so a second open of the same channel waits for it, and is
forgotten again when no thread could start."
  (let* ((base          (world-server-configuration server))
         (configuration (apply #'configuration-copy base
                               :working-directory (uiop:ensure-directory-pathname directory)
                               (when (getf arguments :fullscreen-p)
                                 (list :fullscreen-p t))))
         (application   (application--build
                         configuration
                         :conversation-id          conversation-id
                         :permission-mode          (world-server--permission-mode
                                                    server (getf arguments :permission-mode))
                         :pristine-p               (world-server-pristine-p server)
                         :mutation-replay-failures (world-server-mutation-replay-failures server)
                         :relay-presentation       (list :rows     (getf arguments :rows)
                                                         :columns  (getf arguments :columns)
                                                         :styled-p (getf arguments :styled-p))))
         (host          (make-instance 'world-host :application application)))
    (with-lock-held ((world-server-lock server))
      (push host (world-server-hosts server)))
    (handler-case
        (setf (world-host-thread host)
              (make-thread
               (lambda ()
                 (world-server--host server host
                                     :initial-command    (and (getf arguments :resume-command-p)
                                                              "(resume)")
                                     :recovery-diagnosis (getf arguments :recovery-diagnosis)
                                     :resume-offer-p     (not (null (getf arguments :resume-command-p)))))
               :name (format nil "Autolith channel ~A"
                             (conversation-identifier-display (world-host-channel host)))))
      (error (condition)
        (application--discard-connection-resources application
                                                   (application-tool-registry application)
                                                   (application-worker application))
        (application-release-conversation-lease application)
        (world-server--release-host server host)
        (error condition)))
    host))

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
                                  (:resume-offer-p boolean))
    null)
(defun world-server--host (server host &key initial-command recovery-diagnosis resume-offer-p)
  "Serve HOST's application on this thread until its session ends.

A launcher exit condition reaches the attached clients with its status, as a
direct session's would; an update or rollback then stops the whole server,
because the image is shared, while a fatal error ends this channel alone."
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
                          :quiescer           (lambda (function)
                                                (world-server-call-quiesced
                                                 server application function))
                          :ready-function     (lambda (served)
                                                (declare (ignore served))
                                                (world-server--note-ready server host)))
                         ':finished)))
           (main-launcher-exit-condition (condition)
             (world-server--note-launcher-exit server host condition))
           (error (condition)
             (world-server--note-failure server host condition)))
      (world-server--release-host server host)))
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
  "Forget HOST after its session ended and wake whoever waits for it."
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
  "Return HOST once its endpoint is published, or signal why it never was."
  (let ((lock     (world-server-lock server))
        (deadline (+ (get-internal-real-time)
                     (* *world-server-open-timeout-seconds*
                        internal-time-units-per-second))))
    (with-lock-held (lock)
      (loop
        (cond
          ((world-host-failure host)
           (error 'world-error
                  :message    (format nil "The channel could not start: ~A"
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

(-> world-server-call-quiesced (world-server application function) t)
(defun world-server-call-quiesced (server application function)
  "Run FUNCTION on SERVER's main thread with every hosted session stopped.

APPLICATION is the channel checkpointing, whose hosting thread waits here.
Return FUNCTION's values, or signal what it or the quiescence signaled."
  (let ((lock    (world-server-lock server))
        (done-p  nil)
        (results nil)
        (failure nil))
    (with-lock-held (lock)
      (setf (world-server-jobs server)
            (append (world-server-jobs server)
                    (list (lambda ()
                            (handler-case
                                (setf results
                                      (multiple-value-list
                                       (world-server--run-quiesced server application function)))
                              (serious-condition (condition)
                                (setf failure condition)))
                            (with-lock-held (lock)
                              (setf done-p t)
                              (sb-thread:condition-broadcast
                               (world-server-condition-variable server)))))))
      (sb-thread:condition-broadcast (world-server-condition-variable server))
      (loop until done-p
            do (condition-wait (world-server-condition-variable server) lock)))
    (if failure
        (error failure)
        (values-list results))))

(-> world-server--run-quiesced (world-server application function) t)
(defun world-server--run-quiesced (server application function)
  "Quiesce every hosted session, the management REPL and Slynk around FUNCTION.

The process saves its heap with APPLICATION as the active application, so a
rollback into the checkpoint resumes that channel. Another channel mid-turn
refuses the checkpoint, since its request would be cut in half."
  (let ((hosts (world-server--hosts server)))
    (dolist (host hosts)
      (let* ((other      (world-host-application host))
             (controller (application-input-controller other)))
        (when (and (not (eq other application))
                   controller
                   (application-input-controller-turn-active-p controller))
          (error 'checkpoint-error
                 :message  (format nil "Channel ~A is working; checkpoint when every channel is idle."
                                   (conversation-identifier-display (world-host-channel host)))
                 :stage    ':validation
                 :pathname nil))))
    (setf *active-application* application)
    (unwind-protect
         (labels ((quiesce (remaining)
                    "Stop the sessions of REMAINING hosts, then the process services."
                    (if remaining
                        (application-call-with-session-quiesced
                         (world-host-application (first remaining))
                         (lambda () (quiesce (rest remaining))))
                        (management-repl-call-quiesced
                         server
                         (lambda () (slynk-call-quiesced function))))))
           (quiesce hosts))
      (setf *active-application* nil))))
