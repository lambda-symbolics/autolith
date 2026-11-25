(in-package #:autolith)

;;;; -- World Server --

;;; One process serves a world. Every open channel is an application of its
;;; own, hosted on its own thread with its own world-terminal endpoint, so
;;; users can attach terminals, pause work or detach individual channels.
;;; The server answers at one more endpoint, "world", through which clients
;;; open channels, and it stays alive without clients until it is stopped.

(define-condition world-session-warning (warning)
  ((pathname
    :initarg       :pathname
    :reader        world-session-warning-pathname
    :type          pathname
    :documentation "The world catalogue or channel state pathname involved.")
   (cause
    :initarg       :cause
    :reader        world-session-warning-cause
    :type          t
    :documentation "The failure encountered while updating world state."))
  (:report (lambda (condition stream)
             (format stream "World operation at ~A failed: ~A"
                     (world-session-warning-pathname condition)
                     (world-session-warning-cause condition))))
  (:documentation "An optional world state operation failed."))

(defparameter *world-server-identifier* "world"
  "The endpoint identifier under which a world server answers.")

(defparameter *world-server-status-version* 1
  "The world status wire version.")

(defparameter *world-server-sweep-seconds* 60
  "How often the server looks for channels gone cold.")

(defclass world-host nil
          ((retirement-lock :initform (make-lock "Autolith host retirement")
            :reader world-host-retirement-lock :documentation
            "Serialize final lease release and retries after cleanup failures.")
           (application :initarg :application :reader world-host-application
            :type application :documentation
            "The hosted application serving one channel.")
           (state :initform ':starting :accessor world-host-state :type
            (member :starting :ready :closing :paused :retired) :documentation
            "The owned lifecycle through mandatory cleanup and lease release.")
           (thread :initform nil :accessor world-host-thread :type t
            :documentation "The thread serving the application.")
           (ready-p :initform nil :accessor world-host-ready-p :type boolean
            :documentation
            "Whether the channel's endpoint has been published.")
           (failure :initform nil :accessor world-host-failure :type
            (option condition) :documentation
            "The condition that ended the session, or NIL.")
           (exit-reason :initform nil :accessor world-host-exit-reason :type
            (option keyword) :documentation
            "The session's exit reason once it ended, or NIL while it runs.")
           (paused-p :initform nil :accessor world-host-paused-p :type boolean
            :documentation
            "Whether the session ended for a checkpoint and waits to be served again.")
           (identity :initform nil :accessor world-host-identity :type list
                     :documentation
                     "The :TOKEN and :CREATED-AT of the endpoint a paused session's clients wait for.")
           (quiesced-tools :initform nil :accessor world-host-quiesced-tools
            :type list :documentation
            "The tool runtimes a checkpoint closed in this channel, restarted when it is served again.")
           (idle-since :initform nil :accessor world-host-idle-since :type
            (option real) :documentation
            "The internal real time the server first found the channel idle, or NIL while it is in use.")
           (idle-grace-seconds :initform nil :accessor world-host-idle-grace-seconds
                               :type          (option (real 0 *))
                               :documentation "A shorter idle delay for an unviewed completed assignment, cleared by use."))
          (:documentation "One channel's application and the thread serving it.

A host paused for a checkpoint keeps its application; its thread ended so
the process could fork, and a new thread serves the application again."))

(defclass world-startup ()
  ((cancelled-p
    :initform      nil
    :accessor      world-startup-cancelled-p
    :type          boolean
    :documentation "Whether an explicit close or server stop cancelled construction.")
   (key
    :initarg       :key
    :reader        world-startup-key
    :type          list
    :documentation "The channel or workspace reservation key.")
   (identifier
    :initarg       :identifier
    :reader        world-startup-identifier
    :type          string
    :documentation "The queryable identity of this owned startup.")
   (thread
    :initform      nil
    :accessor      world-startup-thread
    :type          t
    :documentation "The owned construction thread.")
   (host
    :initform      nil
    :accessor      world-startup-host
    :type          (option world-host)
    :documentation "The constructed host, once publication completes.")
   (failure
    :initform      nil
    :accessor      world-startup-failure
    :type          (option condition)
    :documentation "The settled construction failure, when any.")
   (phase
    :initform      ':reserved
    :accessor      world-startup-phase
    :type          keyword
    :documentation "The latest construction phase.")
   (progress
    :initform      nil
    :accessor      world-startup-progress
    :type          list
    :documentation "The latest loading step and description for every waiter.")
   (timings
    :initarg       :timings
    :accessor      world-startup-timings
    :type          list
    :documentation "Monotonic reservation, build and endpoint ticks and admission wait."))

  (:documentation "One owned channel construction shared by concurrent open callers."))(defclass world-server ()
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
                                                                                         (generation-warning-taken-p
                                                                                          :initform      nil
                                                                                          :accessor      world-server--generation-warning-taken-p
                                                                                          :type          boolean
                                                                                          :documentation "Whether a channel already presented the checkpoint health warning.")
                                                                                         (checkpoint-cancel-p
                                                                                          :initform      nil
                                                                                          :accessor      world-server-checkpoint-cancel-p
                                                                                          :type          boolean
                                                                                          :documentation "Whether someone cancelled the checkpoint that is waiting for the work under way.")
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
                                                                                          :documentation "Protects admission transitions, never construction, endpoint I/O or joins.")
                                                                                         (open-count
                                                                                          :initform      0
                                                                                          :accessor      world-server-open-count
                                                                                          :type          (integer 0)
                                                                                          :documentation "Admitted open operations and responses not yet completed.")
                                                                                         (startups
                                                                                          :initform      nil
                                                                                          :accessor      world-server-startups
                                                                                          :type          list
                                                                                          :documentation "Owned channel constructions, including their published outcomes.")
                                                                                         (condition-variable
                                                                                          :initform      (make-condition-variable :name "Autolith world server")
                                                                                          :reader        world-server-condition-variable
                                                                                          :type          t
                                                                                          :documentation "Wakes the main thread for jobs and waiters for readiness and exits.")
                                                                                         (change-feed
                                                                                          :initform      (make-instance 'world-change-feed)
                                                                                          :reader        world-server-change-feed
                                                                                          :documentation "The world's reactive sidebar subscriptions and shared snapshots.")
                                                                                         (hosts
                                                                                          :initform      nil
                                                                                          :accessor      world-server-hosts
                                                                                          :type          list
                                                                                          :documentation "The hosts of every open channel, newest first.")
                                                                                         (jobs
                                                                                          :initform      (make-deque)
                                                                                          :accessor      world-server-jobs
                                                                                          :type          deque
                                                                                          :documentation "Thunks waiting for the main thread, oldest first.")
                                                                                         (child-activity-refresh-pending-p
                                                                                          :initform      nil
                                                                                          :accessor      world-server-child-activity-refresh-pending-p
                                                                                          :type          boolean
                                                                                          :documentation "Whether one child-activity refresh is queued.")
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

(defclass world-endpoint (recoverable-world-endpoint)
  ((server
    :initarg       :server
    :reader        world-endpoint-server
    :type          world-server
    :documentation "The server answering at this endpoint."))
  (:documentation "The authenticated endpoint through which clients open channels."))

(defmethod image-daemon:daemon-runtime-request-valid-p ((endpoint world-endpoint) request)
  "Authenticate world control requests using the channel terminal protocol."
  (image-daemon:daemon-request-valid-p
   request (image-daemon:daemon-runtime-token endpoint)
   :request-tag ':world-terminal-request))

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
                        (:token (option string)) (:created-at (option integer)))
    world-server)
(defun world-server-create
    (configuration &key (permission-mode ':ask) token created-at)
  "Prepare this process as CONFIGURATION's world server and publish its endpoint.

TOKEN and CREATED-AT name the endpoint the client that started this process
waits for; without them the endpoint gets fresh ones. The process services,
the management REPL and Slynk, start with the server. Pristine and retained
images use their own endpoint namespace."
  (let* ((pristine-p   (equal *world-image-selection* "pristine"))
         (failures     (if *checkpoint-boot-generation*
                           (application--prepare-process configuration
                                                         :pristine-p            pristine-p
                                                         :image-replay-failures nil)
                           (application--prepare-process configuration
                                                         :pristine-p pristine-p)))
         (bootstrapped (with-extension-registry-transaction
                         (application--load-extension-configuration
                          configuration :pristine-p pristine-p)
                         (provider-bootstrap-configuration configuration)))
         (server       (make-instance 'world-server
                                      :configuration            bootstrapped
                                      :permission-mode          permission-mode
                                      :mutation-replay-failures failures))
         (completed-p  nil))
    (unwind-protect
         (progn
           (setf (world-server-lease server) (world-server--lease-acquire bootstrapped))
           (world-change-feed-start (world-server-change-feed server) bootstrapped)
           (world-compact-when-grown bootstrapped)
           (world-server--reconcile bootstrapped)
           (setf *world-message-unserved-function*
                 (lambda (identifier) (world-server-wake-channel server identifier)))
           (world-startup-assert-startup-active)
           (world-server--runtime-start server :token token :created-at created-at)
           (world-startup-finish-startup nil)
           (management-repl-start server)
           (slynk-start bootstrapped :background-p t)
           (setf completed-p t)
           server)
      (unless completed-p
        (world-server--shutdown server)))))

(-> world-server--reconcile (configuration) null)
(defun world-server--reconcile (configuration)
  "Recover interrupted channel removals and require canonical catalogue identities."
  ;; Removal recovery can do filesystem work. Keep it out of presentation locks.
  (handler-case (channel-remove-recover configuration)
    (error (cause)
      (warn 'world-session-warning
            :pathname (channel--removal-root configuration)
            :cause    cause)))
  (world-ensure-channel-identities configuration)
  nil)

(-> world-server-lock-pathname (configuration) pathname)
(defun world-server-lock-pathname (configuration)
  "Return the file whose process lease says a world server runs for CONFIGURATION's data root."
  (merge-pathnames "server.lock" (world-endpoint-directory configuration)))

(-> world-server--lease-acquire (configuration &key (:restart list)) t)
(defun world-server--lease-acquire (configuration &key restart)
  "Acquire the world lease of CONFIGURATION for a new server or matching RESTART."
  (let* ((pathname    (world-server-lock-pathname configuration))
         (lease
           (handler-case (lease-acquire pathname)
             (file-lock-busy ()
               (error 'world-error
                      :message   "Another world server already runs for this data root."
                      :pathname  pathname
                      :operation ':start))))
         (completed-p nil))
    (unwind-protect
         (progn
           (when (world-server-restarting-p configuration)
             (let ((marker (world-server--restart-mark-read
                            (world-server-restart-pathname configuration))))
               (unless (and restart
                            (eql (getf restart :launcher-pid) (getf marker :launcher-pid))
                            (equal (getf restart :generation-id) (getf marker :generation-id)))
                 (error 'world-error
                        :message   "The world server is restarting from its saved heap."
                        :pathname  pathname
                        :operation ':start))))
           (unless restart
             (world-server--restart-unmark configuration))
           (setf completed-p t)
           lease)
      (unless completed-p
        (lease-release lease)))))

(-> world-server-running-p (configuration) boolean)
(defun world-server-running-p (configuration)
  "Return true while a world server runs, whether or not its endpoint answers.

A server runs while a process holds the world's lease, and while it restarts
from its saved heap with the lease free. It publishes its endpoint again after
a checkpoint, so a client that finds it running waits for the endpoint
instead of starting a second server."
  (let ((pathname (world-server-lock-pathname configuration)))
    (or (ls-flock:file-lock-busy-p pathname)
        (world-server-restarting-p configuration))))

(-> world-server--runtime-stop (world-server) null)
(defun world-server--runtime-stop (server)
  "Stop the owned endpoint of SERVER and clear the reference after cleanup succeeds."
  (when-let ((runtime (world-server-runtime server)))
    (image-daemon:daemon-runtime-stop runtime)
    (setf (world-server-runtime server) nil))
  nil)

(-> world-server--runtime-start
    (world-server &key (:token (option string)) (:created-at (option integer)))
    null)
(defun world-server--runtime-start (server &key token created-at)
  "Clean up the owned endpoint of SERVER and publish a replacement under TOKEN and CREATED-AT.

If omitted, use the token and creation time already recorded on SERVER."
  (world-server--runtime-stop server)
  (let ((runtime nil))
    (setf runtime
          (image-daemon:daemon-runtime-create
           :directory        (world-endpoint-directory (world-server-configuration server))
           :identifier       *world-server-identifier*
           :token            (or token (world-server-token server))
           :created-at       (or created-at (world-server-created-at server))
           :class            'world-endpoint
           :initargs         (list :server server)
           :request-function #'world-server--handle-request
           :error-function   (lambda (condition)
                               (world-server--endpoint-failed server runtime condition)))
          (world-server-runtime server) runtime)
    (image-daemon:daemon-runtime-start runtime)
    (setf (world-server-token server) (image-daemon:daemon-runtime-token runtime)
          (world-server-created-at server) (image-daemon:daemon-runtime-created-at runtime)))
  nil)

(-> world-server--endpoint-failed (world-server recoverable-world-endpoint serious-condition) null)
(defun world-server--endpoint-failed (server runtime condition)
  "Record the failed listener generation and wake SERVER to recover it."
  (with-lock-held ((world-server-lock server))
    (unless (world-endpoint-failure runtime)
      (setf (world-endpoint-failure-thread runtime)    (current-thread)
            (world-endpoint-failure runtime)           condition
            (world-endpoint-failure-backtrace runtime) (application-safe-backtrace))
      (sb-thread:condition-broadcast (world-server-condition-variable server))))
  nil)

(defgeneric world-endpoint-current-p (runtime server)
  (:documentation "Return true if RUNTIME is still an endpoint owned by SERVER."))

(defmethod world-endpoint-current-p ((runtime world-endpoint) (server world-server))
  "Check ownership of the world control endpoint."
  (eq runtime (world-server-runtime server)))

(defmethod world-endpoint-current-p ((runtime channel-endpoint) (server world-server))
  "Check ownership of a channel endpoint that is not closing."
  (let* ((application (channel-endpoint-application runtime))
         (host        (find application (world-server-hosts server) :key #'world-host-application))
         (controller  (application-input-controller application)))
    (and host
         (eq server (application-world-server application))
         (eq runtime (application-channel-endpoint application))
         (not (member (world-host-state host) '(:closing :retired)))
         (null (world-host-failure host))
         (or (null (world-host-exit-reason host))
             (eq (world-host-exit-reason host) ':checkpoint))
         (not (channel-endpoint-closing-p runtime))
         (not (and controller (application-input-controller-stopping-p controller)))
         t)))

(defgeneric world-endpoint-fatal (runtime server condition &key backtrace)
  (:documentation "Report a fatal endpoint failure to the owner of RUNTIME."))

(defmethod world-endpoint-fatal ((runtime world-endpoint) (server world-server) condition &key backtrace)
  "Stop the world after a fatal control endpoint failure."
  (declare (ignore runtime backtrace))
  (handler-case
      (format *error-output* "~&World endpoint failed: ~A~%" (condition--report-text condition))
    (serious-condition ()
      nil))
  (world-server-stop
   server :reason "the world's control endpoint failed; stopping the world so it can be opened again"))

(defmethod world-endpoint-fatal ((runtime channel-endpoint) (server world-server) condition &key backtrace)
  "End the channel after a fatal endpoint failure."
  (declare (ignore server))
  (world-terminal--endpoint-fatal runtime condition backtrace))

(-> world-server--recover-endpoint (world-server recoverable-world-endpoint) null)
(defun world-server--recover-endpoint (server runtime)
  "Replace a failed listener or report a timer or replacement failure to the owner."
  (let ((failure   nil)
        (backtrace nil))
    (with-lock-held ((world-server-lock server))
      (let ((thread    (world-endpoint-failure-thread runtime))
            (condition (world-endpoint-failure runtime)))
        (when condition
          (cond
            ((or (world-server-stopping-p server)
                 (not (world-endpoint-current-p runtime server))
                 (image-daemon:daemon-runtime-stopping-p runtime)
                 (not (eq thread (image-daemon:daemon-runtime-server-thread runtime))))
             (setf (world-endpoint-failure runtime)           nil
                   (world-endpoint-failure-thread runtime)    nil
                   (world-endpoint-failure-backtrace runtime) nil))
            ((or (world-server-checkpointing-p server)
                 (world-server-holding-p server)
                 (and thread (thread-alive-p thread)))
             nil)
            (t
             (setf backtrace                                 (world-endpoint-failure-backtrace runtime)
                   (world-endpoint-failure runtime)           nil
                   (world-endpoint-failure-thread runtime)    nil
                   (world-endpoint-failure-backtrace runtime) nil)
             (if (and (typep condition 'image-daemon:daemon-error)
                      (eq (image-daemon:daemon-error-operation condition) ':timer))
                 (setf failure condition)
                 (handler-case (image-daemon:daemon-runtime-start runtime)
                   (serious-condition (condition)
                     (unless (image-daemon:daemon-runtime-stopping-p runtime)
                       (setf failure   condition
                             backtrace (application-safe-backtrace)))))))))))
    (when failure
      (world-endpoint-fatal runtime server failure :backtrace backtrace)))
  nil)

(-> world-server--pending-endpoints (world-server) list)
(defun world-server--pending-endpoints (server)
  "Return the endpoints with a pending listener failure under the server lock."
  (remove-if-not
   (lambda (runtime) (and runtime (world-endpoint-failure runtime)))
   (cons (world-server-runtime server)
         (loop for host in (world-server-hosts server)
               collect (application-channel-endpoint (world-host-application host))))))

(-> world-server-run
    (configuration &key (:permission-mode (member :ask :auto :sandboxed :full-access)))
    (option integer))
(defun world-server-run (configuration &key (permission-mode ':ask))
  "Serve CONFIGURATION's world from this process until it is stopped.

The endpoint takes the token of the startup handoff this process was
launched with, so the launching client finds it. Return the launcher status
a hosted session asked the process to exit with, or NIL."
  (let* ((startup-values (and *world-startup-record*
                              (rest *world-startup-record*)))
         (server         (world-server-create configuration
                                              :permission-mode permission-mode
                                              :token           (getf startup-values :token)
                                              :created-at      (getf startup-values :created-at))))
    (unwind-protect
         (world-server-serve server)
      (world-server--shutdown server))))

(-> world-server-serve (world-server) (option integer))
(defun world-server-serve (server)
  "Run SERVER's main-thread jobs until it is stopped and its last channel has ended.

Return the launcher status a hosted session asked for, or NIL."
  (let* ((lock (world-server-lock server))
         (interval
          (* *world-server-sweep-seconds* internal-time-units-per-second))
         (due  (+ (get-internal-real-time) interval))
         (sweep
          (lambda ()
            (world-server--retire-unhealthy-hosts server)
            (world-server--unload-cold-hosts server)
            (world-server--maintain-shell-logs server)
            (setf due (+ (get-internal-real-time) interval)))))
    (loop
     (if-let ((job
                (with-lock-held (lock)
                  (loop
                   (let* ((left    (- due (get-internal-real-time)))
                          (pending (unless (or (world-server-checkpointing-p server)
                                               (world-server-holding-p server))
                                     (world-server--pending-endpoints server)))
                          (ready   (find-if-not (lambda (runtime)
                                                  (when-let ((thread (world-endpoint-failure-thread runtime)))
                                                    (thread-alive-p thread)))
                                                pending)))
                     (cond
                       ((and (deque-empty-p (world-server-jobs server))
                             (world-server-stopping-p server)
                             (null (world-server-hosts server))
                             (notany
                              (lambda (startup)
                                (member (world-startup-phase startup)
                                        '(:reserved :building)))
                              (world-server-startups server)))
                        (return nil))
                       ((<= left 0)
                        (return sweep))
                       (ready
                        (return (lambda () (world-server--recover-endpoint server ready))))
                       ((not (deque-empty-p (world-server-jobs server)))
                        (return (deque-pop-front (world-server-jobs server))))
                       (t
                        ;; The failure hook returns before its listener exits.
                        (let ((seconds (/ left internal-time-units-per-second)))
                          (condition-wait (world-server-condition-variable server)
                                          lock :timeout (if pending (min seconds 1/20) seconds))))))))))
         (funcall job)
         (return (world-server-exit-status server))))))

(-> world-server--maintain-shell-logs (world-server) null)
(defun world-server--maintain-shell-logs (server)
  "Run one retention batch on the world thread and yield to other server jobs."
  (unless (or (world-server-stopping-p server) (world-server-checkpointing-p server))
    (handler-case
        (when (shell-log-maintenance-step (world-server-configuration server))
          (world-server--enqueue server (lambda () (world-server--maintain-shell-logs server))))
      (serious-condition (condition)
        (handler-case
            (format *error-output* "~&Shell log maintenance failed: ~A~%"
                    (condition--report-text condition))
          (serious-condition ()
            nil)))))
  nil)

(-> world-server-wake-channel (world-server string) null)
(defun world-server-wake-channel (server identifier)
  "Serve channel IDENTIFIER without a terminal, so its agent reads the message that just reached it.

Nothing happens when nobody resides there, when it is the person's inbox, or
when a host serves it already. Nothing happens either while another holder
leases its conversation, as a running child does: that holder reads the
message. The channel goes cold again once it has been idle long enough. A
channel that cannot be served keeps its message waiting."
  (world-server--enqueue server
                         (lambda ()
                           (handler-case
                            (let* ((configuration
                                    (world-server-configuration server))
                                   (world (world-load configuration))
                                   (channel
                                    (world-channel-find world identifier)))
                              (when
                                  (and channel (not (channel-inbox-p channel))
                                       (find ':agent
                                             (world-channel-residents world
                                                                      identifier)
                                             :key #'participant-kind)
                                       (null
                                        (world-server--find-host server
                                                                 identifier))
                                       (not
                                        (conversation-lease-busy-p
                                         configuration identifier))
                                       (not (world-server-stopping-p server))
                                       (not
                                        (world-server-checkpointing-p server)))
                                (world-server--start-host server
                                                          :conversation-id
                                                          identifier :directory
                                                          (channel-workspace
                                                           channel)
                                                          :arguments
                                                          (list :switch-p t))))
                            (serious-condition (condition)
                              (handler-case
                                  (format *error-output*
                                          "~&World server could not serve channel ~A for its message: ~A~%"
                                          (conversation-identifier-display identifier)
                                          (condition--report-text condition))
                                (serious-condition ()
                                  nil))))))
  nil)

(-> world-server--host-idle-p (world-host) boolean)
(defun world-server--host-idle-p (host)
  "Return true while HOST's channel is served, no terminal shows it, and nothing runs or waits in it."
  (if-let ((controller (application-input-controller (world-host-application host))))
    (with-lock-held ((application-input-controller-lock controller))
      (world-server--host-idle-locked-p host))
    (world-server--host-idle-locked-p host)))

(-> world-server--host-idle-locked-p (world-host) boolean)
(defun world-server--host-idle-locked-p (host)
  "Return whether HOST is idle without acquiring the input-controller lock.

The controller lock must be held if HOST has a controller."
  (let* ((application  (world-host-application host))
         (controller   (application-input-controller application))
         (orchestrator (application--task-orchestrator application)))
    (and (world-host-ready-p host)
         (not (world-host-paused-p host))
         (null (world-host-exit-reason host))
         (not (world-host-attached-p host))
         (not (and controller (application-input-controller--busy-locked-p controller)))
         (not (and orchestrator (plusp (task-orchestrator-session-live-count orchestrator)))))))

(-> world-server--retire-unhealthy-hosts (world-server) null)
(defun world-server--retire-unhealthy-hosts (server)
  "Request exit or retry cleanup of failed owners without releasing reservations early."
  (unless (world-server-checkpointing-p server)
    (dolist (host (world-server--hosts server))
      (when (world-host-closing-p host)
        (handler-case (world-server--close-host host)
          (serious-condition (condition)
            (unless (world-host-failure host)
              (setf (world-host-failure host) condition)))))))
  nil)

(-> world-server--unload-cold-hosts (world-server &key (:now real)) null)
(defun world-server--unload-cold-hosts (server &key (now (get-internal-real-time)))
  "Close every channel of SERVER that stayed idle the configured minutes, and note those just gone idle.

A closed channel leaves memory with its history; opening it, or a message to
its agent, serves it again. Nothing is unloaded while a checkpoint runs."
  (unless (world-server-checkpointing-p server)
    (let ((limit (* 60 (config :channel-idle-minutes (world-server-configuration server))
                    internal-time-units-per-second)))
      (dolist (host (world-server--hosts server))
        (if (and (world-host-live-p host) (world-server--host-idle-p host))
            (let ((since (world-host-idle-since host)))
              (cond
                ((null since)
                 (setf (world-host-idle-since host) now))
                ((>= (- now since)
                     (if (world-host-idle-grace-seconds host)
                         (min limit (* (world-host-idle-grace-seconds host)
                                       internal-time-units-per-second))
                         limit))
                 (world-server--close-host host))))
            (setf (world-host-idle-since host) nil
                  (world-host-idle-grace-seconds host) nil)))))
  nil)

(-> world-server-stop (world-server &key (:reason (option string))) null)
(defun world-server-stop (server &key reason)
  "Ask every channel of SERVER to end and let the main thread exit after the last one.

REASON, when given, is what each attached terminal is told before it lets go."
  (let ((hosts
         (with-lock-held ((world-server-lock server))
           (progn
            (setf (world-server-stopping-p server) t)
            (dolist (startup (world-server-startups server))
              (setf (world-startup-cancelled-p startup) t)))
           (copy-list (world-server-hosts server)))))
    (dolist (host hosts)
      (when reason (world-server--tell-attached host reason))
      (world-server--close-host host))
    (world-server--wake server))
  nil)

(-> world-server--tell-attached (world-host string) null)
(defun world-server--tell-attached (host reason)
  "End HOST's attached terminals with REASON as a notice line, rendered for those terminals."
  (let* ((application (world-host-application host))
         (ui          (application-ui application)))
    (world-terminal-finish-attachments
     application
     :status  0
     :message (if ui
                  (terminal--render-spans (terminal-ui-terminal ui) (application-notice-line reason))
                  (terminal--spans-text (application-notice-line reason)))))
  nil)

(-> world-server--shutdown (world-server) null)
(defun world-server--shutdown (server)
  "Stop SERVER's endpoint and the process services after its channels ended."
  (setf *world-message-unserved-function* nil)
  (world-server-stop server)
  (world-change-feed-close (world-server-change-feed server))
  (dolist
      (startup
       (with-lock-held ((world-server-lock server))
         (copy-list (world-server-startups server))))
    (let ((thread (world-startup-thread startup)))
      (when (and thread (not (eq thread (current-thread))))
        (join-thread thread))))
  (dolist (host (world-server--hosts server))
    (let ((thread (world-host-thread host)))
      (when (and thread (not (eq thread (current-thread))))
        (join-thread thread))))
  (unwind-protect
       (world-server--runtime-stop server)
    (handler-case (management-repl-stop server) (error nil nil))
    (handler-case (slynk-stop) (error nil nil))
    (handler-case (rlm-environment-pool-flush) (error nil nil))
    (telemetry--call-safely #'telemetry-shutdown)
    (when-let ((lease (shiftf (world-server-lease server) nil)))
      (handler-case (lease-release lease) (error nil nil))))
  nil)

(-> world-server--hosts (world-server) list)
(defun world-server--hosts (server)
  "Return a snapshot of SERVER's hosts, newest first."
  (with-lock-held ((world-server-lock server))
    (copy-list (world-server-hosts server))))

(-> world-server--refresh-child-activities (world-server) null)
(defun world-server--refresh-child-activities (server)
  "Publish current child-channel turns to their parent channels."
  (let* ((hosts (remove-if-not #'world-host-live-p (world-server--hosts server)))
         (world (when hosts (world-load (world-server-configuration server))))
         (rows  (make-hash-table :test #'equal)))
    (dolist (host hosts)
      (let ((channel (world-host-channel host)))
        (when (application-world--agent-turn-active-p (world-host-application host))
          (let* ((agent  (find ':agent (world-channel-residents world channel) :key #'participant-kind))
                 (parent (and agent (participant-parent agent))))
            (when parent
              (push (append (list :channel    channel
                                  :id         (conversation-identifier-display channel)
                                  :agent      (participant-name agent)
                                  :index      most-positive-fixnum
                                  :state      ':running
                                  :assignment ""
                                  :detached   t)
                            (or (application-world-child-progress (world-host-application host))
                                '(:request-count 0)))
                    (gethash parent rows)))))))
    (dolist (host hosts)
      (let ((agent (find ':agent (world-channel-residents world (world-host-channel host))
                         :key #'participant-kind)))
        (application-set-world-child-activities
         (world-host-application host)
         (when agent (sort (gethash (participant-identifier agent) rows) #'string<
                           :key (lambda (activity) (getf activity :channel))))))))
  nil)

(-> world-server-note-child-activities (world-server) null)
(defun world-server-note-child-activities (server)
  "Coalesce child-activity updates into one queued refresh on SERVER."
  (with-lock-held ((world-server-lock server))
    (unless (world-server-child-activity-refresh-pending-p server)
      (world-server--enqueue-locked
       server
       (lambda ()
         (with-lock-held ((world-server-lock server))
           (setf (world-server-child-activity-refresh-pending-p server) nil))
         (handler-case (world-server--refresh-child-activities server)
           (world-error (cause)
             (warn 'world-session-warning
                   :pathname (configuration-world-path (world-server-configuration server))
                   :cause    cause)))))
      (setf (world-server-child-activity-refresh-pending-p server) t)))
  nil)

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
  (let* ((server    (world-endpoint-server runtime))
         (operation (getf (rest request) :operation))
         (arguments (getf (rest request) :arguments)))
    (when (eq operation ':watch-world)
      (world-change-feed-serve (world-server-change-feed server) :socket socket :stream stream
                              :transport-lock (image-daemon:daemon-runtime-lock runtime))
      (return-from world-server--handle-request nil))
    (flet ((answer ()
             "Dispatch the request and flush its response before releasing admission."
             (let ((response
                    (handler-case
                     (handler-bind ((channel-loading-progress
                                     (lambda (progress)
                                       (when (getf arguments :progress-p)
                                         (daemon-write-packet stream
                                                              (list :progress
                                                                    :step
                                                                    (channel-loading-progress-step
                                                                     progress)
                                                                    :description
                                                                    (channel-loading-progress-description
                                                                     progress)))))))
                       (world-server--dispatch server operation arguments))
                     (serious-condition (condition)
                            (append
                             (list :error :message
                                   (condition--report-text condition))
                             (when
                                 (and (typep condition 'world-error)
                                      (member (world-error-operation condition)
                                              '(:checkpointing :recovering)))
                               (list :retry-p t)))))))
               (daemon-write-packet stream response))))
      (if (eq operation ':open)
          (world-server--call-with-open-admission server #'answer :check-p nil)
          (answer))))
  nil)

(-> world-server--dispatch (world-server keyword t) list)
(defun world-server--dispatch (server operation arguments)
  "Return SERVER's response to OPERATION with ARGUMENTS."
  (case operation
    (:status
     (list :ok :status (world-server-status server)))
    (:open
     (world-server-open server arguments))
    (:changed
     (world-change-notify (world-server-configuration server) :relay-p nil)
     (list :ok))
    (:channels
     (list :ok :channels (world-server-channels server)))
    (:close
     (world-server-close server (getf arguments :channel)
                         :unattached-only-p (not (null (getf arguments :unattached-only-p)))))
    (:quiesce
     (world-server-quiesce server (getf arguments :channels)))
    (:person-unread
     (world-server-person-unread-changed server)
     (list :ok :operation ':person-unread))
    (:tell
     (world-server-tell server (getf arguments :channel) (getf arguments :text)))
    (:stop
     (world-server-stop server :reason "world server stopped by autolith stop")
     (list :ok :operation ':stop :session-id *world-server-identifier*))
    (otherwise
     (list :error :message (format nil "Unknown world operation ~S." operation)))))

(-> world-server-tell (world-server t t) list)
(defun world-server-tell (server channel text)
  "Deliver prose TEXT as the person, or submit Lisp to CHANNEL's input controller.

Lisp uses canonical channel admission without terminal pickers or restart selection.
An unhosted target is served before admitting Lisp; prose keeps durable message delivery."
  (unless (and (stringp channel) (stringp text))
    (error 'world-error
           :message   "A tell names the channel and the message."
           :operation ':tell))
  (let* ((configuration (world-server-configuration server))
         (world         (world-load configuration))
         (person        (or (world-human-participant world)
                            (error 'world-error
                                   :message   "No person is in the world yet."
                                   :operation ':tell)))
         (target        (world-channel-designated world configuration channel))
         (identifier    (channel-identifier target)))
    (if (input-lisp-p text)
        (let* ((host        (world-server--start-host
                             server :conversation-id identifier
                                    :directory       (channel-workspace target)
                                     :arguments      (list :switch-p t)))
               (application (world-host-application (world-server--wait-ready server host))))
          (application-input-controller--handle-submission
           (application-input-controller application) text :interactive-p nil))
        (world-message-send configuration :sender  (participant-identifier person)
                                          :channel identifier
                                          :text    text))
    (list :ok :operation ':tell :channel identifier)))

(-> world-server-status (world-server) list)
(defun world-server-status (server)
  "Return SERVER's portable status: its process, version, channels and stop state."
  (let ((runtime (world-server-runtime server)))
    (list :world-status :version *world-server-status-version* :session-id
          *world-server-identifier* :pid (current-process-id) :autolith-version
          *autolith-version* :created-at
          (and runtime (image-daemon:daemon-runtime-created-at runtime))
          :stopping-p (not (null (world-server-stopping-p server))) :channels
          (mapcar #'world-host-channel (world-server--hosts server)) :startups
          (with-lock-held ((world-server-lock server))
            (mapcar #'world-server--startup-status
                    (world-server-startups server))))))

(-> world-server-channels (world-server) list)
(defun world-server-channels (server)
  "Return shared channel snapshots with authoritative host availability."
  (copy-tree
   (world-change-feed-observe (world-server-change-feed server)
                             (lambda () (world-server--channel-snapshots server)))))

(-> world-server--channel-snapshots (world-server) list)
(defun world-server--channel-snapshots (server)
  "Collect hosted state once after a semantic world or runtime change."
  (loop for host in (world-server--hosts server)
        for health = (world-host-health host)
        for session = (application-channel-endpoint (world-host-application host))
        collect (if (and session (eq health ':ready))
                    (append (world-terminal-status-snapshot session) (list :host-health health))
                    (list :world-terminal-status :version *daemon-protocol-version*
                          :session-id            (world-host-channel host)
                          :state health :host-health health :terminal-attached-p nil))))

(defmethod (setf world-host-state) :after (value (host world-host))
  "Invalidate the world's runtime snapshot when HOST changes lifecycle state."
  (declare (ignore value))
  (world-change-notify (application-configuration (world-host-application host))))

(-> world-server-person-unread-changed (world-server) null)
(defun world-server-person-unread-changed (server)
  "Count the messages waiting in the person's inbox again and show the count in every channel SERVER serves."
  (when-let ((unread (handler-case (world-person-unread (world-server-configuration server))
                       (autolith-error ()
                         nil))))
    (dolist (host (world-server--hosts server))
      (application-world-show-person-unread (world-host-application host) unread)))
  nil)

(-> world-server--call-with-open-admission (world-server function &key (:check-p boolean)) t)
(defun world-server--call-with-open-admission (server function &key (check-p t))
  "Count an admitted operation through its result or response publication, without holding admission while it runs."
  (with-lock-held ((world-server-open-lock server))
    (with-lock-held ((world-server-lock server))
      (when check-p
        (world-server--check-open-admission server))
      (incf (world-server-open-count server))))
  (unwind-protect (funcall function)
    (with-lock-held ((world-server-lock server))
      (decf (world-server-open-count server))
      (sb-thread:condition-broadcast (world-server-condition-variable server)))))

(-> world-server--check-open-admission (world-server) null)
(defun world-server--check-open-admission (server)
  "Reject a new open after stop or checkpoint admission; the caller holds admission."
  (when (world-server-stopping-p server)
    (error 'world-error :message "The world server is stopping." :operation ':open))
  (when (world-server-checkpointing-p server)
    (error 'world-error :message  "The world server is checkpointing; retry after it resumes."
                       :operation ':checkpointing))
  nil)

(-> world-server--startup-active-p (world-startup) boolean)
(defun world-server--startup-active-p (startup)
  "Return true until STARTUP has settled and its construction thread has exited."
  (not (null (or (member (world-startup-phase startup) '(:reserved :building))
                 (let ((thread (world-startup-thread startup)))
                   (and thread (thread-alive-p thread)))))))

(-> world-server-open (world-server list) list)
(defun world-server-open (server arguments)
  "Open the channel ARGUMENTS select and answer with its session identifier.

ARGUMENTS name a :CHANNEL or a :DIRECTORY, and carry the client's
:PERMISSION-MODE, :PICK-CHANNEL-P,
:ROWS, :COLUMNS and :STYLED-P. The answer arrives once the channel's endpoint
is published, so the client can attach at once. A workspace channel whose
conversation cannot be loaded is passed over, and the channel that opens
instead says so."
  (world-server--call-with-open-admission server
   (lambda ()
     (multiple-value-bind (host started-p skipped)
         (world-server--host-for server arguments)
       (channel-loading-report 5 "Starting channel")
       (world-server--wait-ready server host)
       (unless started-p
         (world-server--reconcile-launch server host (getf arguments :launch)))
       (loop for (identifier . message) in skipped
             do (application-present (world-host-application host)
                                     (application-failure-line
                                      (format nil
                                              "channel ~A could not open: ~A"
                                              (conversation-identifier-display identifier)
                                              message))))
       (list :ok :session-id (world-host-channel host) :started-p
             (not (null started-p)))))))

(-> world-server--cancel-startup (world-server string) (option string))
(defun world-server--cancel-startup (server channel)
  "Cancel an explicitly closed construction without abandoning its eventual cleanup."
  (with-lock-held ((world-server-lock server))
    (when-let ((startup (find-if (lambda (candidate)
                                   (and (equal (world-startup-key candidate) (list ':channel channel))
                                        (world-server--startup-active-p candidate)))
                                 (world-server-startups server))))
      (setf (world-startup-cancelled-p startup) t)
      (sb-thread:condition-broadcast (world-server-condition-variable server))
      (world-startup-identifier startup))))

(-> world-server-close (world-server t &key (:unattached-only-p boolean)) list)
(defun world-server-close (server channel &key unattached-only-p)
  "End the session of CHANNEL and answer once its end is requested.

With UNATTACHED-ONLY-P the channel is closed only while no terminal controls
it, and the answer says whether it was: a client cleaning up a channel it
started must not end one another terminal attached to since."
  (unless (stringp channel)
    (error 'world-error :message "A close names the channel to close."
           :operation            ':close))
  (let ((host (world-server--find-host server channel)))
    (unless host
      (when-let ((startup (world-server--cancel-startup server channel)))
        (return-from world-server-close
          (list :ok :operation ':close :session-id channel :closed-p t
                :startup startup :pending-p t)))
      (error 'world-error :message
             (format nil "Channel ~A is not open in this world server."
                     (conversation-identifier-display channel))
             :operation ':close :identifier channel))
    (let ((session
           (application-channel-endpoint (world-host-application host))))
      (flet ((close-host ()
               "Close HOST and answer that it was."
               (world-server--close-host host)
               (list :ok :operation ':close :session-id
                     (world-host-channel host) :closed-p t)))
        (cond ((not unattached-only-p)
               (close-host))
              ((null session)
               (close-host))
              (t
               (if-let ((committed-p
                         (with-lock-held ((image-daemon:daemon-runtime-lock
                                           session))
                           (unless (world-host-attached-p host)
                             (setf (channel-endpoint-closing-p session) t)))))
                   (close-host)
                   (list :ok :operation ':close :session-id
                         (world-host-channel host) :closed-p nil))))))))

(-> world-server-quiesce (world-server list) list)
(defun world-server-quiesce (server channels)
  "Close idle unattended hosts in CHANNELS and wait outside admission locks for lease release. Return channel/state pairs; refuse attached, running and unavailable hosts."
  (progn
   (with-lock-held ((world-server-open-lock server))
     (when (world-server-checkpointing-p server)
       (error 'world-error :message
              "The world is checkpointing; retry after it resumes." :operation
              ':checkpointing)))
   (let* ((hosts
           (loop for channel in channels
                 for host = (world-server--find-host server channel)
                 when host
                 collect host))
          (states
           (mapcar
            (lambda (host)
              (cons (world-host-channel host)
                    (cond ((world-host-attached-p host)
                           ':attached)
                          ((world-server--host-idle-p host)
                           ':idle)
                          (t
                           ':running))))
            hosts)))
     (when (every (lambda (state) (eq (rest state) ':idle)) states)
       (dolist (host hosts)
         (let* ((application (world-host-application host))
                (session     (application-channel-endpoint application))
                (controller  (application-input-controller application))
                (state
                 (assoc (world-host-channel host) states :test #'string=)))
           (setf (rest state)
                 (if (and session controller)
                      ;; Match input dispatch: take the controller lock before the endpoint lock.
                     (with-lock-held ((application-input-controller-lock controller))
                       (with-lock-held ((image-daemon:daemon-runtime-lock session))
                         (cond ((world-host-attached-p host)
                                ':attached)
                               ((not (world-server--host-idle-locked-p host))
                                ':running)
                               ((application-input-controller--request-idle-exit-locked controller)
                                (setf (channel-endpoint-closing-p session) t
                                      (world-host-state host) ':closing)
                                ':closing)
                               (t
                                ':hosted))))
                     ':hosted))))
       (dolist (host hosts)
         (let ((state (assoc (world-host-channel host) states :test #'string=)))
           (when (eq (rest state) ':closing)
             (let ((thread (world-host-thread host)))
               (when thread
                 (sb-thread:join-thread thread :timeout 10 :default nil))
               (setf (rest state)
                     (if (and thread (thread-alive-p thread))
                         ':hosted
                         ':closed)))))))
     (list :ok :states states))))


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
  "Return true while a client follows HOST: a terminal that controls or watches it, or another structured client."
  (application-clients-attached-p (world-host-application host)))

(-> world-host-health (world-host) keyword)
(defun world-host-health (host)
  "Return HOST's availability without changing its reservation or lease."
  (let* ((application (world-host-application host))
         (thread      (world-host-thread host))
         (controller  (application-input-controller application))
         (session     (application-channel-endpoint application)))
    (cond
      ((eq (world-host-state host) ':retired)
       ':retired)
      ((eq (world-host-state host) ':closing)
       ':closing)
      ((world-host-failure host)
       ':failed)
      ((or (world-host-paused-p host) (eq (world-host-exit-reason host) ':checkpoint))
       ':paused)
      ((world-host-exit-reason host)
       ':closing)
      ((and thread (not (thread-alive-p thread)))
       ':failed)
      ((or (eq (world-host-state host) ':starting) (null thread))
       ':starting)
      ((and session controller
            (not (channel-endpoint-closing-p session))
            (not (image-daemon:daemon-runtime-stopping-p session))
            (not (application-input-controller-stopping-p controller))
            (image-daemon:daemon-runtime-failure session))
       ':recovering)
      ((and (world-host-ready-p host) controller
            (not (application-input-controller-exit-requested-p controller))
            (null (application-input-controller-failure controller))
            session (channel-endpoint-endpoint-live-p session))
       ':ready)
      (t
       ':failed))))

(-> world-host-live-p (world-host) boolean)
(defun world-host-live-p (host)
  "Return true when HOST is available for attachment."
  (eq (world-host-health host) ':ready))

(-> world-host-closing-p (world-host) boolean)
(defun world-host-closing-p (host)
  "Return true while unavailable HOST retains mandatory cleanup ownership."
  (not (null (member (world-host-health host) '(:closing :failed)))))

(-> world-server--check-host-opening (world-host) null)
(defun world-server--check-host-opening (host)
  "Refuse unavailable retained owners instead of constructing competitors."
  (let ((health (world-host-health host)))
    (when (member health '(:closing :failed :retired :paused :recovering))
      (error 'world-error :operation (case health
                                       (:paused
                                        ':checkpointing)
                                       (:recovering
                                        ':recovering)
                                       (otherwise
                                        ':closing))
                         :identifier (world-host-channel host)
                         :message    (format nil "Channel ~A is ~A; retry after its owner finishes the transition."
                                             (conversation-identifier-display (world-host-channel host))
                                             (string-downcase (symbol-name health))))))
  nil)

(-> world-host-reserved-p (world-host) boolean)
(defun world-host-reserved-p (host)
  "Return true until HOST retires after mandatory cleanup and actual lease release."
  (not (eq (world-host-state host) ':retired)))

(-> world-server--find-host (world-server string) (option world-host))
(defun world-server--find-host (server identifier)
  "Return SERVER's host holding channel IDENTIFIER, starting or serving, or NIL."
  (find-if (lambda (host)
             (and (world-host-reserved-p host)
                  (string= identifier (world-host-channel host))))
           (world-server--hosts server)))

(-> world-server--host-for (world-server list) (values world-host boolean))
(defun world-server--host-for (server arguments)
  "Return the host selected by ARGUMENTS, whether it was started now, and the channels passed over.

For a named channel or :INBOX-P, reuse its host or start one. With :LATEST-P,
select the directory's latest durable top-level channel and open it by name.
Before the first message makes a new channel durable, reuse an available
workspace host. Other directory opens try available channels by latest activity.
Create a channel if none qualifies, or at once with :NEW-P.
The channels passed over are (IDENTIFIER . MESSAGE) pairs for workspace
channels whose conversation could not be loaded. The caller waits for the
host to publish its endpoint."
  (let* ((configuration (world-server-configuration server))
         (channel       (or (getf arguments :channel)
                           (when (getf arguments :inbox-p)
                             (channel-identifier (world-ensure-inbox configuration)))))
         (directory     (getf arguments :directory)))
    (cond
     ((stringp channel)
      (let* ((identifier
              (conversation-identifier-migration-resolve configuration
                                                         channel))
             (host          (world-server--find-host server identifier)))
       (if host
           (progn
            (world-server--check-host-opening host)
            (when (getf arguments :mcp-servers)
              (error 'world-error :message
                     (format nil
                             "Channel ~A is already running, so its tools cannot take the editor's MCP servers. Open it without them, or close it elsewhere first."
                             (conversation-identifier-display identifier))
                     :operation ':open :identifier identifier))
            (values host nil))
           (world-server--start-host server :conversation-id identifier
                                     :directory
                                     (world-server--channel-workspace server
                                                                      identifier)
                                     :arguments              arguments))))
     ((stringp directory)
      (unless (uiop/filesystem:directory-exists-p directory)
        (error 'world-error :message
               (format nil "The directory ~A does not exist." directory)
               :operation ':open))
      (let ((workspace (world-workspace-name directory)))
         (when (and (getf arguments :latest-p) (not (getf arguments :new-p)))
           (when-let ((latest (world-channel-latest-top-level configuration workspace)))
             (return-from world-server--host-for
               (world-server--host-for server (list* :channel latest arguments)))))
        (multiple-value-bind (host started-p skipped)
            (if (getf arguments :new-p)
                (values nil nil nil)
                (world-server--latest-host server workspace arguments))
          (if host
              (values host started-p skipped)
              (multiple-value-bind (host started-p)
                  (world-server--start-host server :directory workspace
                                            :arguments        arguments)
                (values host started-p skipped))))))
     (t
      (error 'world-error :message "An open names a channel or a directory."
             :operation            ':open)))))

(-> world-server--channel-workspace (world-server string) string)
(defun world-server--channel-workspace (server identifier)
  "Return the workspace channel IDENTIFIER records, the directory its conversation began in, or the server's own directory.

A conversation that the world does not record yet registers the directory it
began in as its workspace once it is served."
  (let* ((configuration (world-server-configuration server))
         (world         (world-load configuration))
         (channel       (world-channel-find world identifier)))
    (if channel
        (channel-workspace channel)
        (let ((origin (getf (rest (conversation-peek-header
                                   (conversation-pathname-for-id configuration identifier)))
                            :directory)))
          (world-workspace-name (if (non-empty-string-p origin)
                                    origin
                                    (config :working-directory configuration)))))))

(-> world-server-take-generation-warning (world-server) boolean)
(defun world-server-take-generation-warning (server)
  "Return true the first time a channel of SERVER asks to present the checkpoint health warning."
  (with-lock-held ((world-server-lock server))
    (not (shiftf (world-server--generation-warning-taken-p server) t))))

(-> world-server--latest-candidates (world-server string) list)
(defun world-server--latest-candidates (server workspace)
  "Return (IDENTIFIER . HOST) for WORKSPACE's top-level channels, the latest touched first.

HOST is the server's host of the channel, starting or serving, or NIL. Latest
touched is the newest write of the channel's conversation file; a hosted
channel with no file yet was started just now and counts as the newest. The
person's inbox is never a candidate: it is opened only by name."
  (let* ((configuration (world-server-configuration server))
         (world         (world-load configuration))
         (hosts         (remove-if-not (lambda (host)
                                         (and (world-host-reserved-p host)
                                              (string= workspace (world-host-workspace host))))
                                       (world-server--hosts server)))
         (candidates    nil))
    (dolist (channel (world-channels world))
      (when (and (world-channel-top-level-p world channel)
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

(-> world-server--workspace-host-suitable-p (world-host list) boolean)
(defun world-server--workspace-host-suitable-p (host arguments)
  "Return whether HOST can accept a workspace open with ARGUMENTS."
  (not (or (world-host-attached-p host)
           (getf arguments :mcp-servers)
           (world-server--launch-conflict host (getf arguments :launch)))))

(-> world-server--latest-host (world-server string list) (values (option world-host) boolean list))
(defun world-server--latest-host (server workspace arguments)
  "Return the host of WORKSPACE's latest touched free channel, whether it was started now, and the channels passed over.

A hosted channel serves when no client controls it, the launch options of
ARGUMENTS can apply to it and they bring no MCP servers its tools would need;
an unhosted one starts when its conversation is free and loads. A channel
whose conversation cannot be loaded is passed over, as an (IDENTIFIER .
MESSAGE) pair, so one damaged conversation never keeps the workspace from
opening. Return NIL for the host when no channel qualifies."
  (let ((skipped nil))
    (dolist
        (candidate (world-server--latest-candidates server workspace)
                   (values nil nil (nreverse skipped)))
      (destructuring-bind
          (identifier . host)
          candidate
        (cond
         (host
          (world-server--check-host-opening host)
          (when (world-server--workspace-host-suitable-p host arguments)
             (return (values host nil (nreverse skipped)))))
         (t
          (handler-case
           (return
            (multiple-value-bind (host started-p)
                (world-server--start-host server :conversation-id identifier
                                          :directory workspace :arguments
                                          arguments)
              (values host started-p (nreverse skipped))))
           (conversation-in-use nil nil)
           (conversation-error (condition)
            (push (cons identifier (autolith-error-message condition))
                  skipped)))))))))

(-> world-server--launch-conflict (world-host list) (option string))
(defun world-server--launch-conflict (host launch)
  "Return the option of LAUNCH that channel HOST runs differently and cannot change, or NIL.

The model, provider and immutable mode are fixed when a channel's
application is built. The permission mode changes only while the channel
is not working: a turn or a job already
running keeps the mode it started under."
  (let* ((application   (world-host-application host))
         (configuration (application-configuration application))
         (model         (getf launch :model))
         (provider      (getf launch :provider))
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
      (t
       nil))))

(-> world-server--reconcile-launch (world-server world-host list) null)
(defun world-server--reconcile-launch (server host launch)
  "Apply compatible LAUNCH options at the channel boundary or signal a conflict."
  (let* ((conflict    (world-server--launch-conflict host launch))
         (display     (conversation-identifier-display (world-host-channel host)))
         (application (world-host-application host))
         (requested   (getf launch :permission-mode)))
    (when conflict
      (error 'world-error
             :message    (format nil "Channel ~A is already running and cannot take ~A. Open it without that option, or open another channel."
                                 display conflict)
             :operation  ':open
             :identifier (world-host-channel host)))
    (when requested
      (let ((mode (world-server--permission-mode server requested))
            (hub  (application-client-hub application)))
        (flet ((may-select-mode-p ()
                 "Refuse a different mode on a controlled or working channel; the caller holds the hub lock."
                 (let ((reason (cond ((client-hub-clients hub)
                                      "controlled by another terminal")
                                     ((world-host-jobs-live-p host)
                                      "working"))))
                   (when (and reason (not (eq mode (application-permission-mode application))))
                     (error 'world-error
                            :message    (format nil "Channel ~A is ~A and keeps its permission mode ~(~A~)."
                                                display reason (application-permission-mode application))
                            :operation  ':open
                            :identifier (world-host-channel host)))
                   (null reason))))
          (when (with-lock-held ((client-hub-lock hub)) (may-select-mode-p))
            (application-clients-call-as-work
             application
             (lambda ()
               (let ((changed-p nil))
                 (with-lock-held ((client-hub-lock hub))
                   (when (may-select-mode-p)
                     (setf changed-p (not (eq mode (application-permission-mode application))))
                     (application-set-channel-permission-mode application mode)))
                 (when changed-p
                   (application-present application
                                        (format nil "permission mode changed to ~(~A~) by a terminal opening this channel"
                                                mode))
                   (application-clients-publish-state application))))))))))
  nil)

(-> world-host-working-p (world-host) boolean)
(defun world-host-working-p (host)
  "Return true while HOST's channel runs a turn or a job."
  (let* ((application (world-host-application host))
         (controller  (application-input-controller application)))
    (or (and controller (application-input-controller-turn-active-p controller))
        (world-host-jobs-live-p host))))

(-> world-host-jobs-live-p (world-host) boolean)
(defun world-host-jobs-live-p (host)
  "Return true while HOST owns a child or session job."
  (multiple-value-bind (children active-children session-jobs)
      (world-terminal--task-counts (world-host-application host))
    (declare (ignore active-children))
    (or (plusp children) (plusp (or session-jobs 0)))))

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

(-> world-server--startup-status (world-startup) list)
(defun world-server--startup-status (startup)
  "Return bounded ownership, phase and monotonic measurements for one startup."
  (let ((host (world-startup-host startup)))
    (list :identifier (world-startup-identifier startup)
          :key        (world-startup-key startup)
          :phase      (cond ((and host (world-host-failure host))
                             ':failed)
                            ((and host (world-host-ready-p host))
                             ':ready)
                            (host
                             ':endpoint)
                            (t
                             (world-startup-phase startup)))
          :pending-p  (not (null (or (world-server--startup-active-p startup)
                                    (and host (not (world-host-ready-p host))
                                         (not (world-host-failure host))))))
          :channel    (when host (world-host-channel host))
          :timings    (copy-list (world-startup-timings startup)))))

(-> world-server--wait-startup (world-server world-startup) world-host)
(defun world-server--wait-startup (server startup)
  "Await the shared construction and report progress until its outcome is available."
  (let ((last-progress nil))
    (loop
     (let ((host nil) (failure nil) (progress nil))
       (with-lock-held ((world-server-lock server))
         (setf host (world-startup-host startup)
               failure (world-startup-failure startup)
               progress (world-startup-progress startup))
         (unless (or host failure (not (equal last-progress progress)))
           (condition-wait (world-server-condition-variable server)
                           (world-server-lock server))))
       (when (and progress (not (equal last-progress progress)))
         (setf last-progress progress)
         (apply #'channel-loading-report progress))
       (when failure (error failure))
       (when host (return host))))))

(-> world-server--construct-startup (world-server world-startup function) null)
(defun world-server--construct-startup (server startup function)
  "Construct STARTUP once and publish either its host or its original failure to all waiters."
  (with-lock-held ((world-server-lock server))
    (setf (world-startup-phase startup) ':building
          (getf (world-startup-timings startup) :build-started-at)
          (get-internal-real-time)))
  (unwind-protect
      (handler-case
       (handler-bind ((channel-loading-progress
                       (lambda (progress)
                         (with-lock-held ((world-server-lock server))
                           (setf (world-startup-progress startup)
                                 (list
                                  (channel-loading-progress-step progress)
                                  (channel-loading-progress-description
                                   progress)))
                           (sb-thread:condition-broadcast
                            (world-server-condition-variable server))))))
         (let ((host (funcall function)))
           (with-lock-held ((world-server-lock server))
             (progn
              (setf (world-startup-host startup) host
                    (world-startup-phase startup) ':constructed
                    (getf (world-startup-timings startup) :constructed-at)
                    (get-internal-real-time))
              (when (world-host-ready-p host)
                (setf (getf (world-startup-timings startup) :endpoint-ready-at)
                      (get-internal-real-time)))))))
       (serious-condition (condition)
              (with-lock-held ((world-server-lock server))
                (setf (world-startup-failure startup) condition
                      (world-startup-phase startup) ':failed))))
    (with-lock-held ((world-server-lock server))
      (unless (or (world-startup-host startup) (world-startup-failure startup))
        (setf (world-startup-failure startup)
              (make-condition 'world-error :operation ':open :identifier
                              (world-startup-identifier startup) :message
                              "The channel construction exited without an outcome.")
              (world-startup-phase startup) ':failed))
      (sb-thread:condition-broadcast
       (world-server-condition-variable server))))
  nil)

(-> world-server--start-host
    (world-server &key (:conversation-id (option string)) (:directory string) (:arguments list))
    (values world-host boolean &optional))
(defun world-server--start-host
       (server &key conversation-id directory arguments)
  "Reserve one channel before constructing it off the admission path; return its host and whether this caller owns the new admission."
  (let* ((key
          (if conversation-id
              (list ':channel conversation-id)
              (if (getf arguments :new-p)
                  (list ':new (gensym))
                  (list ':workspace directory))))
         (startup    nil)
         (existing   nil)
         (owner-p    nil)
         (waiting-at (get-internal-real-time)))
    (with-lock-held ((world-server-open-lock server))
      (world-server--check-open-admission server)
      (setf existing
              (and conversation-id
                   (world-server--find-host server conversation-id)))
      (progn
       (when existing (world-server--check-host-opening existing))
       (unless existing
         (with-lock-held ((world-server-lock server))
           (setf (world-server-startups server)
                 (remove-if
                  (lambda (candidate)
                    (and (not (world-server--startup-active-p candidate))
                         (let ((host (world-startup-host candidate)))
                           (or (null host)
                               (not (world-host-reserved-p host))))))
                  (world-server-startups server)))
           (setf startup
                   (find-if
                    (lambda (candidate)
                      (and (equal key (world-startup-key candidate))
                           (if-let ((host (world-startup-host candidate)))
                               (and (world-host-reserved-p host)
                                    (or conversation-id
                                        (world-server--workspace-host-suitable-p
                                         host arguments)))
                               (world-server--startup-active-p candidate))))
                    (world-server-startups server)))
           (unless startup
             (setf startup
                     (make-instance 'world-startup :key key :identifier
                                    (symbol-name (gensym "startup-")) :timings
                                    (list :reserved-at (get-internal-real-time)
                                          :lock-wait
                                          (- (get-internal-real-time)
                                             waiting-at)))
                   owner-p t)
             (push startup (world-server-startups server))))
         (when owner-p
           (handler-case
            (setf (world-startup-thread startup)
                  (make-thread
                   (lambda ()
                     (world-server--construct-startup server startup
                      (lambda ()
                        (world-server--build-host server :conversation-id
                         conversation-id :directory directory :arguments
                         arguments :startup startup))))
                   :name
                   (format nil "Autolith ~A"
                           (world-startup-identifier startup))))
            (serious-condition (condition)
                   (with-lock-held ((world-server-lock server))
                     (setf (world-startup-failure startup) condition
                           (world-startup-phase startup) ':failed)
                     (sb-thread:condition-broadcast
                      (world-server-condition-variable server)))))))))
    (values (or existing (world-server--wait-startup server startup)) owner-p)))

(-> world-server--build-host
    (world-server &key (:conversation-id (option string)) (:directory string)
     (:arguments list) (:startup (option world-startup)))
    world-host)
(defun world-server--build-host
       (server &key conversation-id directory arguments startup)
  "Build the application of CONVERSATION-ID, or a new one, in DIRECTORY and serve it.

ARGUMENTS carry the opening client's choices, among them the :LAUNCH options
it stated explicitly, which the new channel takes, and an editor's
:MCP-SERVERS, which its tools include. The host is listed before
its thread starts, so a second open of the same channel waits for it, and is
forgotten again when no thread could start."
  (let ((launch (getf arguments :launch)))
    (when (and (getf launch :provider) (null (getf launch :model)))
      (error 'world-error :message
             "--provider requires --model MODEL; a channel cannot take a provider alone."
             :operation ':open)))
  (let* ((base   (world-server-configuration server))
         (launch (getf arguments :launch))
         (configuration
          (apply #'configuration-copy base :working-directory
                 (uiop/pathname:ensure-directory-pathname directory)
                 (append
                  (when (getf launch :model)
                    (list :model (getf launch :model) :model-provider
                          (or (getf launch :provider) "")
                          :model-selection-override-p t))
                  (when (getf launch :immutable-p) (list :immutable-p t)))))
         (application
          (application--build configuration :conversation-id conversation-id
                              :pristine-p                    (equal *world-image-selection* "pristine")
                              :permission-mode
                              (world-server--permission-mode server (getf arguments :permission-mode))
                              :explicit-permission-mode
                              (when (getf launch :permission-mode)
                                (world-server--permission-mode server (getf launch :permission-mode)))
                              :mutation-replay-failures
                              (world-server-mutation-replay-failures server)
                              :client-pane
                              (list :rows    (getf arguments :rows)
                                    :columns (getf arguments :columns))
                              :tool-registry-function
                              (when-let ((servers (getf arguments :mcp-servers)))
                                (lambda (configuration conversation)
                                  (declare (ignore conversation))
                                  (acp-mcp-create-tool-registry
                                   configuration (coerce servers 'list))))))
         (host   (make-instance 'world-host :application application)))
    (setf (application-world-server application) server)
    (with-lock-held ((world-server-lock server))
      (push host (world-server-hosts server)))
    (handler-case
     (progn
      (when
          (or (world-server-stopping-p server)
              (and startup (world-startup-cancelled-p startup)))
        (error 'world-error :operation ':open-cancelled :identifier
               (world-host-channel host) :message
               "The owned channel startup was cancelled."))
      (world-server--serve-host server host :initial-command
                                (and (getf arguments :pick-channel-p) "(open)")
                                :adaptation-offer-p t
                                :boot-screen-p
                                (not (getf arguments :switch-p))))
     (serious-condition (condition)
            (unwind-protect
                (application--discard-connection-resources application
                                                           (application-tool-registry
                                                            application)
                                                           (application-worker
                                                            application))
              (setf (world-host-failure host) condition)
              (world-server--release-host server host))
            (error condition)))
    host))

(-> world-server--serve-host
           (world-server world-host &key (:initial-command (option string))
                                         (:adaptation-offer-p boolean)
                                         (:resume-p boolean)
                                         (:reconnect-id (option string))
                                         (:notice (or null string list))
                                         (:boot-screen-p boolean))
           null)
(defun world-server--serve-host (server host &key initial-command
                                                  adaptation-offer-p resume-p reconnect-id notice
                                                  (boot-screen-p t))
  "Start the thread serving HOST's application, serving it again after a checkpoint with RESUME-P.

Without BOOT-SCREEN-P the channel opens without the boot screen."
  (setf (client-hub-change-function (application-client-hub (world-host-application host)))
        (lambda () (world-change-notify (application-configuration (world-host-application host)))))
  (setf (world-host-thread host)
        (make-thread
         (lambda ()
           (world-server--host server host
                               :initial-command    initial-command
                               :adaptation-offer-p adaptation-offer-p
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
                          (cons '*world-startup-record* nil)
                          inherited)))
    (setf (rest inherited) (list 'quote bindings))
    bindings))

(-> world-server--host
    (world-server world-host &key (:initial-command (option string))
                                  (:adaptation-offer-p boolean)
                                  (:resume-p boolean)
                                  (:reconnect-id (option string))
                                  (:notice (or null string list))
                                  (:boot-screen-p boolean))
    null)
(defun world-server--host
       (server host
        &key initial-command adaptation-offer-p resume-p
        reconnect-id notice (boot-screen-p t))
  "Serve HOST's application on this thread until its session ends.

A launcher exit condition reaches the attached clients with its status, as a
direct session's would; an update or rollback then stops the whole server,
because the image is shared, while a fatal error ends this channel alone. A
session that ends for a checkpoint leaves HOST paused, to be served again on
another thread with RESUME-P, RECONNECT-ID for its clients and NOTICE.
Without BOOT-SCREEN-P the channel opens without the boot screen."
  (let* ((application            (world-host-application host))
         (configuration          (application-configuration application))
         (bordeaux-threads:*default-special-bindings*
          (world-server--thread-bindings application))
         (*active-application*   application)
         (*world-startup-record* nil)
         (*default-pathname-defaults*
          (config :working-directory configuration)))
    (unwind-protect
        (handler-case
         (handler-bind ((main-launcher-exit-condition
                         (lambda (condition)
                           (multiple-value-bind (status message details)
                               (main--launcher-exit-text condition)
                             (world-terminal-finish-attachments application :status
                                                            status :message
                                                            message :details
                                                            details)))))
           (setf (world-host-exit-reason host)
                 (or
                  (application-serve application :initial-command
                                     initial-command :adaptation-offer-p
                                     adaptation-offer-p :resume-p
                                     resume-p :notice notice :boot-screen-p
                                     boot-screen-p :world-terminal-identity
                                     (and reconnect-id
                                          (append (world-host-identity host)
                                                  (list
                                                   :checkpoint-reconnect-id
                                                   reconnect-id)))
                                     :quiescer
                                     (lambda (function)
                                       (declare (ignore function))
                                       (error 'checkpoint-error :message
                                              "A hosted channel checkpoints through its world server."
                                              :stage ':validation :pathname
                                              nil))
                                     :ready-function
                                     (lambda (served)
                                       (declare (ignore served))
                                       (world-server--note-ready server
                                                                 host)))
                  ':finished)))
         (main-launcher-exit-condition (condition)
          (world-server--note-launcher-exit server host condition))
         (serious-condition (condition)
                (world-server--note-failure server host condition)))
      (if (eq (world-host-exit-reason host) ':checkpoint)
          (with-lock-held ((world-server-lock server))
            (setf (world-host-paused-p host) t
                  (world-host-state host) ':paused)
            (sb-thread:condition-broadcast
             (world-server-condition-variable server)))
          (handler-case (world-server--release-host server host)
            (serious-condition (condition)
              (world-server--note-failure server host condition))))))
  nil)

(-> world-server--note-ready (world-server world-host) null)
(defun world-server--note-ready (server host)
  "Record endpoint readiness without surrendering a close already requested."
  (let ((closing-p nil))
    (with-lock-held ((world-server-lock server))
      (setf closing-p (or (world-server-stopping-p server)
                         (eq (world-host-state host) ':closing))
            (world-host-ready-p host) t
            (world-host-state host) (if closing-p ':closing ':ready))
      (dolist (startup (world-server-startups server))
        (when (eq (world-startup-host startup) host)
          (setf (getf (world-startup-timings startup) :endpoint-ready-at) (get-internal-real-time))))
      (sb-thread:condition-broadcast (world-server-condition-variable server)))
    (if closing-p
        (world-server--close-host host)
        (world-server-note-child-activities server)))
  nil)

(-> world-server--note-failure (world-server world-host condition) null)
(defun world-server--note-failure (server host condition)
  "Record CONDITION as what ended HOST's session, so an opener learns why."
  (with-lock-held ((world-server-lock server))
    (unless (world-host-failure host)
      (setf (world-host-failure host) condition))
    (setf (world-host-exit-reason host) ':failed)
    (sb-thread:condition-broadcast (world-server-condition-variable server)))
  (handler-case
      (format *error-output* "~&Channel ~A ended: ~A~%"
              (conversation-identifier-display (world-host-channel host))
              (condition--report-text condition))
    (serious-condition ()
      nil))
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
      (progn
        (multiple-value-bind (status message) (main--launcher-exit condition)
          (declare (ignore status))
          (format *error-output* "~&~A~%" message)
          (finish-output *error-output*))
        (world-server--wake server))
      (world-server-stop server))
  nil)

(-> world-server--release-host (world-server world-host) null)
(defun world-server--release-host (server host)
  "Forget HOST after its session ended, drop the editor credentials its MCP servers held, and wake whoever waits for it."
  (with-lock-held ((world-host-retirement-lock host))
    (unless (eq (world-host-state host) ':retired)
      (setf (world-host-state host) ':closing)
      (let ((application (world-host-application host))
            (failures    nil))
        (application-assignment-finish application :stop-p t)
        (flet ((attempt (function)
                 (handler-case (funcall function)
                   (serious-condition (condition) (push condition failures)))))
          (attempt (lambda () (world-terminal-stop application)))
         (attempt (lambda () (tool-registry-close-runtime-state (application-tool-registry application))))
         (attempt (lambda () (application-disconnect-task-presentation application)))
         (attempt (lambda ()
                    (when (application-worker application)
                      (lisp-worker-manager-stop (application-worker application)))))
         (attempt (lambda ()
                    (acp-mcp-release-credentials
                     (mcp-tool-registry-manager (application-tool-registry application))))))
        (when failures
          (unless (world-host-failure host)
            (setf (world-host-failure host) (first (last failures))))
          (error 'world-error :operation ':closing :identifier (world-host-channel host)
                             :message "Channel cleanup is incomplete; its owner retains the conversation lease."))
        (application-release-conversation-lease application))
      (with-lock-held ((world-server-lock server))
        (setf (world-server-hosts server)
              (remove host (world-server-hosts server))
              (world-server-startups server)
              (remove host (world-server-startups server) :key
                      #'world-startup-host)
              (world-host-state host) ':retired)
        (unless (or (world-host-ready-p host) (world-host-failure host))
          (setf (world-host-failure host)
                (make-condition 'world-error :message
                                "The channel's session ended before its endpoint was published."
                                :operation ':open :identifier
                                (world-host-channel host))))
        (sb-thread:condition-broadcast
         (world-server-condition-variable server)))
      nil)))

(-> world-server--wait-ready (world-server world-host) world-host)
(defun world-server--wait-ready (server host)
  "Wait for HOST's endpoint or signal its session failure."
  (let ((lock (world-server-lock server)))
    (with-lock-held (lock)
      (loop
       (cond
        ((world-host-failure host)
         (error 'world-error :message
                (format nil "The channel failed: ~A"
                        (condition--report-text (world-host-failure host) :limit 400))
                :operation ':open :identifier (world-host-channel host)))
        ((world-host-live-p host)
         (return host))
        ((world-host-closing-p host)
         (world-server--check-host-opening host))
        (t
         (condition-wait (world-server-condition-variable server) lock :timeout
                         0.5)))))))

(-> world-server--close-host (world-host) null)
(defun world-server--close-host (host)
  "Request exit while retaining ownership, or retry retirement after serving ended."
  (unless (eq (world-host-state host) ':retired)
    (setf (world-host-state host) ':closing)
    (let* ((application (world-host-application host))
           (controller  (application-input-controller application))
           (thread      (world-host-thread host)))
      (if (or (and thread (not (thread-alive-p thread)))
              (and (null thread) (world-host-failure host)))
          (world-server--release-host (application-world-server application) host)
          (when controller
            (application-input-controller--request-exit controller ':world-terminal-kill)))))
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
    (world-server--enqueue-locked server job))
  nil)

(-> world-server--enqueue-locked (world-server function) null)
(defun world-server--enqueue-locked (server job)
  "Queue JOB under the held lock of SERVER and wake the main thread."
  (deque-push-back (world-server-jobs server) job)
  (sb-thread:condition-broadcast (world-server-condition-variable server))
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
       :notice-function #'notice
       :cancel-function (lambda ()
                          (setf outcome (world-server--cancelled-condition))))))
  nil)

(-> world-server--cancelled-condition () checkpoint-error)
(defun world-server--cancelled-condition ()
  "Return the condition a checkpoint cancelled while it waited ends with."
  (make-condition 'checkpoint-error
                  :message  "cancelled while it waited for the work under way"
                  :stage    ':validation
                  :pathname nil))

(-> world-server--call-with-quiescer
    (world-server &key (:application application) (:function function) (:reconnect-id string)
                       (:notice-function (option function)) (:cancel-function (option function)))
    t)
(defun world-server--call-with-quiescer
       (server
        &key application function reconnect-id notice-function cancel-function)
  "Call FUNCTION with the checkpoint quiescer bound, then serve every paused channel again.

The quiescer pauses the channels, telling their clients to reconnect under
RECONNECT-ID, and stops the process services around its thunk. The process
saves its heap with APPLICATION as the active application, so a rollback
into the checkpoint resumes that channel. NOTICE-FUNCTION, given an
application, returns what its channel is told once served again. A
checkpoint cancelled while it waits for the work under way calls
CANCEL-FUNCTION instead of FUNCTION."
  (unwind-protect
      (if (not (world-server--drain server))
          (when cancel-function (funcall cancel-function))
          (progn
           (setf *active-application* application)
           (let ((*checkpoint-thread-quiescer*
                  (lambda (thunk)
                    (world-server--pause-channels server application
                                                  reconnect-id)
                    (world-server--call-with-services-stopped server thunk))))
             (funcall function))))
    (setf *active-application* nil)
    (unless (world-server-stopping-p server)
      (world-server--resume-channels server application reconnect-id
                                     :notice-function notice-function))))

(-> world-server--drain (world-server) boolean)
(defun world-server--drain (server)
  "Hold new turns, then admit a checkpoint after construction and open responses finish; cancellation returns NIL."
  (with-lock-held ((world-server-lock server))
    (setf (world-server-holding-p server) t
          (world-server-checkpoint-cancel-p server) nil))
  (loop
    (when (world-server-checkpoint-cancel-p server) (return nil))
    (let ((hosts (world-server--hosts server)))
      (unless (some #'world-server--host-busy-p hosts)
        (when (with-lock-held ((world-server-open-lock server))
                (with-lock-held ((world-server-lock server))
                  (when (and (equal hosts (world-server-hosts server))
                             (zerop (world-server-open-count server))
                             (notany #'world-server--startup-active-p (world-server-startups server))
                             (not (world-server-checkpoint-cancel-p server)))
                    (setf (world-server-checkpointing-p server) t))))
          (return t))))
    (sleep *world-server-checkpoint-poll-seconds*)))

(-> world-server-cancel-checkpoint (world-server) boolean)
(defun world-server-cancel-checkpoint (server)
  "Cancel SERVER's checkpoint while it waits for the work under way, and return whether one was waiting.

Once the checkpoint pauses the channels to save, it can no longer be cancelled."
  (with-lock-held ((world-server-lock server))
    (and (world-server-holding-p server)
         (not (world-server-checkpointing-p server))
         (setf (world-server-checkpoint-cancel-p server) t))))

(-> world-server--host-busy-p (world-host) boolean)
(defun world-server--host-busy-p (host)
  "Return true while HOST is starting, runs a turn, or has a live job."
  (let* ((application (world-host-application host))
         (controller  (application-input-controller application))
         (orchestrator
          (application-task-presentation-orchestrator application)))
    (cond ((or (world-host-failure host) (world-host-exit-reason host))
           nil)
          ((not (world-host-ready-p host))
           t)
          ((null controller)
           t)
          ((application-input-controller-turn-active-p controller)
           t)
          ((and orchestrator
                (plusp (task-orchestrator-session-live-count orchestrator)))
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
        (let ((session (application-channel-endpoint (world-host-application host))))
          (when (and session (controller host) (null (world-host-exit-reason host)))
            (setf (world-host-identity host)
                  (list :token      (image-daemon:daemon-runtime-token session)
                        :created-at (image-daemon:daemon-runtime-created-at session)))
            (world-terminal--finish-checkpoint-attachments session reconnect-id)
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
  "Call THUNK with the process services stopped, then restore the world endpoint.

Stop SERVER if endpoint cleanup or publication fails during recovery."
  (unwind-protect
       (progn
         (world-server--runtime-stop server)
         (management-repl-call-quiesced server
                                       (lambda () (slynk-call-quiesced thunk))))
    (handler-case
        (unless (world-server-stopping-p server)
          (world-server--runtime-start server))
      (serious-condition (condition)
        (world-server-stop
         server :reason "The world endpoint could not restart after the checkpoint.")
        (error condition)))))

(-> world-server--resume-channels
    (world-server application string &key (:notice-function (option function)))
    null)
(defun world-server--resume-channels
       (server application reconnect-id &key notice-function)
  "Resume the paused channels of SERVER and clear the checkpoint admission flags.

Record resume failures on the affected hosts and continue with the other channels."
  (unwind-protect
       (dolist (host (world-server--hosts server))
         (handler-case
             (world-server--resume-host server host :application    application
                                                   :reconnect-id    reconnect-id
                                                   :notice-function notice-function)
           (serious-condition (condition)
             (with-lock-held ((world-server-lock server))
               (let* ((thread   (world-host-thread host))
                      (paused-p (not (and thread (thread-alive-p thread)))))
                 (setf (world-host-ready-p host) nil
                       (world-host-paused-p host) paused-p
                       (world-host-state host) (if paused-p ':paused ':closing))))
             ;; The condition is recorded before diagnostics are written.
             (handler-case (world-server--note-failure server host condition)
               (error () nil)))))
    (with-lock-held ((world-server-lock server))
      (setf (world-server-checkpointing-p server) nil
            (world-server-holding-p server) nil)
      (sb-thread:condition-broadcast (world-server-condition-variable server)))
    (dolist (host (world-server--hosts server))
      (when-let ((controller
                  (application-input-controller (world-host-application host))))
        (with-lock-held ((application-input-controller-lock controller))
          (sb-thread:condition-broadcast
           (application-input-controller-condition-variable controller))))))
  nil)

(-> world-server--resume-host
    (world-server world-host &key (:application application) (:reconnect-id string)
                                 (:notice-function (option function)))
    null)
(defun world-server--resume-host (server host &key application reconnect-id notice-function)
  "Restart quiesced tools and resume HOST if paused.

Report tool restart failures to the channel. Present the notice directly to
APPLICATION if the channel did not pause."
  (let ((served (world-host-application host))
        (tools  (shiftf (world-host-quiesced-tools host) nil)))
    (when tools
      (handler-case
          (tool-registry-resume-runtime-state (application-tool-registry served) :tools tools)
        (serious-condition (condition)
          (application-present
           served
           (application-failure-line
            (format nil "some tools did not restart after the checkpoint: ~A"
                    (bounded-string (princ-to-string condition) :limit 400)))))))
    (cond
      ((world-host-paused-p host)
       (with-lock-held ((world-server-lock server))
         (setf (world-host-paused-p host) nil
               (world-host-ready-p host) nil
               (world-host-exit-reason host) nil
               (world-host-state host) ':starting))
       (world-server--serve-host server host :resume-p t :reconnect-id reconnect-id
                                            :notice (and notice-function
                                                         (funcall notice-function served))))
      ((and notice-function (eq served application))
       (application-present served (funcall notice-function served)))))
  nil)
