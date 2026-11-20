(in-package #:autolith)

;;;; -- World Server Tests --

(-> world-server-tests--stubs () list)
(defun world-server-tests--stubs ()
  "Return the loader replacements keeping a test world server off private state."
  (flet ((none (&rest arguments)
           "Load nothing."
           (declare (ignore arguments))
           nil))
    (list (list 'image-state-load #'none)
          (list 'durable-mutations-load #'none)
          (list 'mcp-configuration-load #'none)
          (list 'user-init-load #'none))))

(-> world-server-tests--call (function) null)
(defun world-server-tests--call (function)
  "Call FUNCTION with a served test world server, its configuration and root, then stop it."
  (with-test-configuration (configuration root)
    (world-server-tests--call-with-server
     configuration
     (lambda (server)
       (funcall function server configuration root))))
  nil)

(-> world-server-tests--call-with-server (configuration function &key (:permission-mode keyword)) null)
(defun world-server-tests--call-with-server (configuration function &key (permission-mode ':auto))
  "Call FUNCTION with a world server served for CONFIGURATION in this process, then stop it."
  (configuration-ensure-directories configuration)
  (test-call-with-function-replacements
   (world-server-tests--stubs)
   (lambda ()
      (let* ((server        (world-server-create configuration :permission-mode permission-mode))
             (sweep-seconds *world-server-sweep-seconds*)
             (main          (make-thread
                             (lambda ()
                               (let ((*world-server-sweep-seconds* sweep-seconds))
                                 (world-server-serve server)))
                             :name "Autolith world server test main")))
       (unwind-protect
            (funcall function server)
         (world-server-stop server)
         (test-assert (task-tests--wait-until
                       (lambda () (not (thread-alive-p main))) 30)
                      "a stopped world server ends once its channels ended")
         (image-daemon:daemon-stop-thread main)
         (world-server--shutdown server)))))
  nil)

(-> world-server-tests--open (cons string &rest t) list)
(defun world-server-tests--open (entry workspace &rest arguments)
  "Open a channel for WORKSPACE through the world ENTRY with ARGUMENTS and return the response."
  (localgroup-query-record entry ':open
                           (append (list :directory workspace
                                         :rows      30
                                         :columns   90
                                         :styled-p  nil)
                                   arguments)))

(-> world-server-tests--session (world-server string) localgroup-session)
(defun world-server-tests--session (server identifier)
  "Return the localgroup session of channel IDENTIFIER hosted by SERVER."
  (let ((host (world-server--find-host server identifier)))
    (unless host
      (error "Channel ~A is not hosted." identifier))
    (or (application-localgroup-session (world-host-application host))
        (error "Channel ~A has no session." identifier))))

(-> world-server-tests--call-with-endpoint-failure
    (image-daemon:daemon-runtime serious-condition function) null)
(defun world-server-tests--call-with-endpoint-failure (runtime failure function)
  "Call FUNCTION with an injector for FAILURE inside the captured accept handler.

FUNCTION calls its injector within any hook-release cleanup it needs. A private
connection wakes an existing wait before the synchronous failure is released.
Keep the override until the captured thread exits; replacement threads pass through."
  (let ((thread    (image-daemon:daemon-runtime-server-thread runtime))
        (connected (sb-thread:make-semaphore))
        (wait      (symbol-function 'image-daemon::daemon-runtime--wait-for-client)))
    (test-call-with-function-replacements
     (list (list 'image-daemon::daemon-runtime--wait-for-client
                 (lambda (candidate)
                   (if (and (eq candidate runtime) (eq (current-thread) thread))
                       (progn
                         (unless (sb-thread:wait-on-semaphore connected :timeout 10)
                           (error "The failure injection connection was not established."))
                         (error failure))
                       (funcall wait candidate)))))
     (lambda ()
       (flet ((inject ()
                "Wake the listener and release the synchronous failure after socket cleanup."
                (unwind-protect
                     (multiple-value-bind (socket stream)
                         (daemon-connect (image-daemon:daemon-runtime-port runtime))
                       (unwind-protect (close stream)
                         (sb-bsd-sockets:socket-close socket)))
                  (sb-thread:signal-semaphore connected))))
         (unwind-protect (funcall function #'inject)
           (sb-thread:signal-semaphore connected)
           (test-assert
            (task-tests--wait-until (lambda () (not (thread-alive-p thread))) 10)
            "the failed accept thread exits without cancelling a healthy listener"))))))
  nil)

(-> test-world-server-endpoint-recovery () null)
(defun test-world-server-endpoint-recovery ()
  "Exercise one owner repair, accepted-client survival and repeated-failure shutdown."
  (let ((*world-server-sweep-seconds* 1/100))
    (world-server-tests--call
     (lambda (server configuration root)
       (declare (ignore root))
       (let* ((runtime    (world-server-runtime server))
              (thread     (image-daemon:daemon-runtime-server-thread runtime))
              (token      (image-daemon:daemon-runtime-token runtime))
              (created-at (image-daemon:daemon-runtime-created-at runtime))
              (hook       (image-daemon:daemon-runtime-error-function runtime))
              (entered    (sb-thread:make-semaphore))
              (release    (sb-thread:make-semaphore))
              (barrier    (sb-thread:make-semaphore))
              (failure    (make-condition 'simple-error :format-control "Injected accept failure.")))
         (test-call-with-function-replacements
          (list (list 'image-daemon:daemon-runtime-error-function
                      (let ((reader (symbol-function 'image-daemon:daemon-runtime-error-function)))
                        (lambda (candidate)
                          (if (eq candidate runtime)
                              (lambda (condition)
                                (funcall hook condition)
                                (when (eq (current-thread) thread)
                                  (funcall hook condition)
                                  (sb-thread:signal-semaphore entered)
                                  (sb-thread:wait-on-semaphore release)))
                              (funcall reader candidate))))))
          (lambda ()
            (multiple-value-bind (socket stream)
                (daemon-connect (image-daemon:daemon-runtime-port runtime))
              (unwind-protect
                   (progn
                     (test-assert
                      (task-tests--wait-until
                       (lambda () (image-daemon:daemon-runtime-client-sockets runtime)) 10)
                      "a request connection is accepted before the listener fails")
                     (world-server-tests--call-with-endpoint-failure
                      runtime failure
                      (lambda (inject)
                        (unwind-protect
                             (progn
                               (funcall inject)
                               (test-assert (sb-thread:wait-on-semaphore entered :timeout 10)
                                            "the real accept thread enters its installed owner failure hook")
                               (world-server--enqueue server (lambda () (sb-thread:signal-semaphore barrier)))
                               (test-assert (sb-thread:wait-on-semaphore barrier :timeout 10)
                                            "an exiting handler does not block unrelated owner jobs")
                               (test-assert
                                (and (thread-alive-p thread)
                                     (not (world-endpoint-repair-attempted-p runtime))
                                     (eq (world-endpoint-failure runtime) failure))
                                "duplicate reports coalesce and repair waits for the old handler to exit"))
                          (sb-thread:signal-semaphore release))))
                     (test-assert
                      (task-tests--wait-until
                       (lambda ()
                         (let ((replacement (image-daemon:daemon-runtime-server-thread runtime)))
                           (and (not (eq thread replacement))
                                (thread-alive-p replacement)))) 10)
                      "the ordinary sweep starts a replacement accept thread")
                     (test-assert
                      (and (eq runtime (world-server-runtime server))
                           (equal token (image-daemon:daemon-runtime-token runtime))
                           (eql created-at (image-daemon:daemon-runtime-created-at runtime))
                           (world-endpoint-repair-attempted-p runtime)
                           (world-client-entry configuration))
                      "repair preserves runtime identity and discoverability")
                     (daemon-write-packet
                      stream (list :localgroup-request :version *daemon-protocol-version*
                                   :token token :operation ':status :arguments nil))
                     (test-assert (eq (first (test-localgroup--read-packet stream)) ':ok)
                                  "a previously accepted connection completes after listener repair")
                     (test-assert
                      (eq (first (localgroup-query-record (world-client-entry configuration) ':status)) ':ok)
                      "new authenticated connections reach the repaired endpoint")
                     (let ((replacement (image-daemon:daemon-runtime-server-thread runtime)))
                       (world-server-tests--call-with-endpoint-failure
                        runtime failure
                        (lambda (inject)
                          (funcall inject)
                          (test-assert
                           (task-tests--wait-until (lambda () (world-server-stopping-p server)) 10)
                           "a failure of the repaired accept thread requests normal world stop")
                          (test-assert (eq replacement (image-daemon:daemon-runtime-server-thread runtime))
                                       "the lifetime repair budget forbids a second transport restart")))))
                (close stream)
                (sb-bsd-sockets:socket-close socket)))))))))
  nil)

(-> world-server-tests--fail-endpoint (image-daemon:daemon-runtime serious-condition) null)
(defun world-server-tests--fail-endpoint (runtime failure)
  "Fail the captured accept thread through its installed owner hook and await exit."
  (let ((thread (image-daemon:daemon-runtime-server-thread runtime)))
    (world-server-tests--call-with-endpoint-failure
     runtime failure (lambda (inject) (funcall inject)))
    (test-assert (and (eq (world-endpoint-failure-thread runtime) thread)
                      (eq (world-endpoint-failure runtime) failure))
                 "the installed owner hook captures the actual endpoint failure"))
  nil)

(-> test-world-server-endpoint-precedence () null)
(defun test-world-server-endpoint-precedence ()
  "Exercise stop, checkpoint deferral and superseded-runtime precedence over repair."
  (with-test-configuration (configuration)
    (test-call-with-function-replacements
     (world-server-tests--stubs)
     (lambda ()
       (let* ((server  (world-server-create configuration :permission-mode ':auto))
              (runtime (world-server-runtime server))
              (thread  (image-daemon:daemon-runtime-server-thread runtime))
              (failure (make-condition 'simple-error :format-control "Deferred accept failure."))
              (starts  0))
         (unwind-protect
              (progn
                (world-server-tests--fail-endpoint runtime failure)
                (test-call-with-function-replacements
                 (list (list 'image-daemon:daemon-runtime-start
                             (lambda (candidate)
                               (test-assert (eq candidate runtime) "only the captured runtime is repaired")
                               (incf starts)
                               candidate)))
                 (lambda ()
                   (flet ((pending ()
                            "Install the captured failure of the now-dead accept thread."
                            (setf (world-endpoint-failure-thread runtime) thread
                                  (world-endpoint-failure runtime) failure)))
                     (dolist (state '(:holding :checkpointing))
                       (pending)
                       (setf (world-server-holding-p server) (eq state ':holding)
                             (world-server-checkpointing-p server) (eq state ':checkpointing))
                       (world-server--recover-endpoint server runtime)
                       (test-assert (and (zerop starts) (eq failure (world-endpoint-failure runtime)))
                                    "checkpoint ownership defers rather than loses the pending failure"))
                     (setf (world-server-holding-p server) nil
                           (world-server-checkpointing-p server) nil)
                     (world-server--recover-endpoint server runtime)
                     (test-assert (= starts 1) "cancelling a drain permits the deferred repair")
                     (pending)
                     (setf (world-server-runtime server) nil)
                     (world-server--recover-endpoint server runtime)
                     (test-assert (and (= starts 1) (null (world-endpoint-failure runtime))
                                       (not (world-server-stopping-p server)))
                                  "a checkpoint-superseded runtime cannot repair or stop its owner")
                     (setf (world-server-runtime server) runtime)
                     (pending)
                     (world-server-stop server)
                     (world-server--recover-endpoint server runtime)
                     (test-assert (and (= starts 1) (null (world-endpoint-failure runtime)))
                                  "STOP wins over a queued recovery")))))
           (setf (world-server-runtime server) runtime)
           (world-server--shutdown server))))))
  nil)

(-> test-world-server-endpoint-escalation () null)
(defun test-world-server-endpoint-escalation ()
  "Exercise repeated, timer and repair failures through normal shutdown and lease release."
  (dolist (mode '(:repeated :timer :repair))
    (with-test-configuration (configuration root)
      (let ((timer-calls 0))
        (world-server-tests--call-with-server
         configuration
         (lambda (server)
           (let* ((runtime (world-server-runtime server))
                  (thread  (image-daemon:daemon-runtime-server-thread runtime))
                  (identifier
                    (getf (rest (world-server-tests--open
                                 (world-client-entry configuration)
                                 (world-tests--workspace root "endpoint-failure"))) :session-id))
                  (starts  0)
                  (done    (sb-thread:make-semaphore)))
             (multiple-value-bind (connection seen await)
                 (test-localgroup--present (world-server-tests--session server identifier))
               (declare (ignore seen))
               (setf (world-endpoint-repair-attempted-p runtime) (eq mode ':repeated))
               (unwind-protect
                    (test-call-with-function-replacements
                     (list (list 'image-daemon:daemon-runtime-start
                                 (lambda (candidate)
                                   (declare (ignore candidate))
                                   (incf starts)
                                   (error "Injected transport repair failure."))))
                     (lambda ()
                       (setf (world-server-checkpointing-p server) t)
                       (if (eq mode ':timer)
                           (progn
                             (image-daemon:daemon-runtime-set-timer
                              runtime
                              :next-deadline-function (lambda (candidate)
                                                        (declare (ignore candidate))
                                                        (get-universal-time))
                              :wake-function          (lambda (candidate)
                                                       (declare (ignore candidate))
                                                       (incf timer-calls)
                                                       (error 'image-daemon:daemon-error
                                                              :message   "Injected timer failure."
                                                              :operation ':timer)))
                             (test-assert
                              (task-tests--wait-until (lambda () (not (thread-alive-p thread))) 10)
                              "a due callback fails on the real endpoint thread"))
                           (world-server-tests--fail-endpoint
                            runtime (make-condition 'image-daemon:daemon-error
                                                    :message   "Injected endpoint failure."
                                                    :operation ':accept)))
                       (world-server--enqueue
                        server (lambda ()
                                 (unwind-protect
                                      (progn
                                        (setf (world-server-checkpointing-p server) nil)
                                        (world-server--recover-endpoint server runtime))
                                   (sb-thread:signal-semaphore done))))
                       (test-assert (sb-thread:wait-on-semaphore done :timeout 10)
                                    "escalation returns through the normal owner job boundary")
                       (test-assert (and (world-server-stopping-p server)
                                         (= starts (if (eq mode ':repair) 1 0))
                                         (= timer-calls (if (eq mode ':timer) 1 0))
                                         (funcall await ':exit
                                                  (lambda (event)
                                                    (non-empty-string-p (getf (rest event) :message)))))
                                    "unrepairable failure stops normally and tells attached terminals why")))
                 (setf (world-server-checkpointing-p server) nil)
                 (localgroup-client-close connection))))))
        (test-assert (and (not (world-server-running-p configuration))
                          (null (world-client-entry configuration)))
                     "normal final shutdown releases the lease and unpublishes discovery"))))
  nil)

(-> test-world-server-cold-channels () null)
(defun test-world-server-cold-channels ()
  "Test a channel nobody uses unloads after the idle minutes, and stays while it is in use."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry      (world-client-entry configuration))
            (identifier (getf (rest (world-server-tests--open entry (world-tests--workspace root "cold")))
                              :session-id))
            (host       (world-server--find-host server identifier))
            (minute     (* 60 internal-time-units-per-second)))
       (test-assert (task-tests--wait-until (lambda () (world-server--host-idle-p host)) 30)
                    "a served channel nobody watches is idle")
       (world-server--unload-cold-hosts server :now 0)
       (world-server--unload-cold-hosts server :now (* 29 minute))
       (test-assert (eq (world-server--find-host server identifier) host)
                    "a channel idle for less than the idle minutes stays loaded")
       (world-server--unload-cold-hosts server :now (* 31 minute))
       (test-assert (task-tests--wait-until
                     (lambda () (null (world-server--find-host server identifier))) 30)
                    "a channel idle past the idle minutes is unloaded"))))
  nil)

(-> test-world-server-channel-permission-modes () null)
(defun test-world-server-channel-permission-modes ()
  "Restore independent channel modes on cold open and after replacing the server."
  (with-test-configuration (configuration root)
    (let ((channels nil))
      (labels ((open-channel (identifier &optional launch)
                 "Open IDENTIFIER using a caller default that cannot override a channel choice."
                 (localgroup-query-record
                  (world-client-entry configuration) ':open
                  (list :channel identifier :launch launch :permission-mode ':auto
                        :rows 30 :columns 90 :styled-p nil)))

               (application-for (server identifier)
                 "Find the owner of a channel after its open completed."
                 (world-host-application (world-server--find-host server identifier)))

               (check-modes (server)
                 "Open and verify every mode from a newly constructed application."
                 (dolist (entry channels)
                   (open-channel (first entry))
                   (test-assert (eq (application-permission-mode (application-for server (first entry)))
                                    (rest entry))
                                "cold channel restores its own mode over the caller default"))))
        (world-server-tests--call-with-server
         configuration
         (lambda (server)
           (test-call-with-function-replacements
            (list (list 'application--command-sandbox-available-p (constantly t)))
            (lambda ()
              (dolist (entry '((:ask . :ask) (:auto . :auto) (:sandbox . :sandboxed) (:full . :full-access)))
                (let* ((workspace (world-tests--workspace root (string-downcase (symbol-name (first entry)))))
                       (response  (world-server-tests--open (world-client-entry configuration) workspace :new-p t))
                       (id        (getf (rest response) :session-id))
                       (owner     (application-for server id)))
                  (application-clients-call-as-work
                   owner (lambda ()
                           (conversation-append-user-message (application-conversation owner) "saved channel")
                           (application-permissions-command owner (first entry))))
                  (push (cons id (rest entry)) channels)))))
           (let ((minute (* 60 internal-time-units-per-second)))
             (world-server--unload-cold-hosts server :now 0)
             (world-server--unload-cold-hosts server :now (* 31 minute)))
           (test-assert (task-tests--wait-until (lambda () (null (world-server--hosts server))) 30)
                        "selected permissions alone do not keep hosts loaded")
           (check-modes server)
           (let* ((entry (first channels))
                  (id    (first entry)))
             (open-channel id '(:permission-mode :ask))
             (setf (rest entry) ':ask)
             (test-assert (eq (application-permission-mode (application-for server id)) ':ask)
                          "an explicit open replaces the saved full mode"))))
        (world-server-tests--call-with-server configuration #'check-modes))))
  nil)

(-> test-world-server-child-maintenance () null)
(defun test-world-server-child-maintenance ()
  "Test maintenance releases visited idle children and refuses attached or running hosts."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (let* ((parent  (world-tests--persisted-conversation configuration "bmaint11" 1000))
            (context (make-instance 'tool-context :configuration configuration :conversation parent
                                                 :worker nil :agent nil :registry (make-instance 'tool-registry)))
            (entry   (world-client-entry configuration)))
       (world-tests--child-conversation configuration "bmaintkid" :root "bmaint11" :parent "bmaint11"
                                                                :name "helper" :role "general" :created-at 1500)
       (world-reconcile-sessions configuration)
       (flet ((visit ()
                "Open the child and wait until its host is idle."
                (localgroup-query-record entry ':open (list :channel "bmaintkid" :switch-p t))
                (let ((host (world-server--find-host server "bmaintkid")))
                  (test-assert (task-tests--wait-until (lambda () (world-server--host-idle-p host)) 30)
                               "the visited finished child has an idle host")
                  host)))
         (dolist (operation '("child.reset" "child.compact"))
           (visit)
           (test-assert (conversation-lease-busy-p configuration "bmaintkid")
                        "visiting holds the child's conversation lease")
           (let ((called nil))
             (task-child--call-with-lease
              context "bmaintkid"
              (lambda ()
                (setf called t)
                (test-assert (null (world-server--find-host server "bmaintkid"))
                             "maintenance waits until the idle host has ended")
                (test-assert (conversation-lease-busy-p configuration "bmaintkid")
                             "maintenance holds the child's lease"))
              :tool-name operation)
             (test-assert called "reset and compaction can operate on a visited child")))
         (let* ((host        (visit))
                (application (world-host-application host))
                (controller  (application-input-controller application))
                (connection  (test-localgroup--present (world-server-tests--session server "bmaintkid"))))
           (unwind-protect
                (test-assert
                 (handler-case (progn (channel-remove configuration '("bmaintkid")) nil)
                   (task-error (condition) (search "terminal" (autolith-error-message condition))))
                 "removing an attached child refuses with an attachment-specific reason")
             (localgroup-client-close connection))
           (test-assert (task-tests--wait-until (lambda () (not (world-host-attached-p host))) 10)
                        "the terminal that left no longer shows the child")
           (with-lock-held ((application-input-controller-lock controller))
             (setf (application-input-controller-active-p controller) t))
           (unwind-protect
                (test-assert
                 (handler-case (progn (channel-remove configuration '("bmaintkid")) nil)
                   (task-error (condition) (search "running" (autolith-error-message condition))))
                 "removing a running child refuses without cancelling it")
             (with-lock-held ((application-input-controller-lock controller))
               (setf (application-input-controller-active-p controller) nil)))
           (test-assert (world-host-live-p host) "refused maintenance keeps the host alive"))
         (test-assert (= 1 (channel-remove configuration '("bmaintkid")))
                      "an idle visited child can be removed immediately")
         (test-assert (null (world-channel-find (world-load configuration) "bmaintkid"))
                      "removal deletes the visited child's world channel")))))
  nil)

(-> test-world-server-child-message-activity () null)
(defun test-world-server-child-message-activity ()
  "Show a reactivated child to its parent during messages from any sender."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "bactivityparent" 1000)
     (world-tests--persisted-conversation configuration "bactivitypeer" 2000)
     (world-tests--child-conversation configuration "bactivitykid" :root "bactivityparent"
                                     :parent "bactivityparent" :name "helper" :role "general" :created-at 1500)
     (world-reconcile-sessions configuration)
     (localgroup-query-record (world-client-entry configuration) ':open
                              (list :channel "bactivityparent" :switch-p t))
     (let* ((parent  (world-host-application (world-server--find-host server "bactivityparent")))
            (ui      (application-presentation parent))
            (human   (world-human-participant (world-load configuration)))
            (entered (sb-thread:make-semaphore))
            (release (sb-thread:make-semaphore))
            (run     (symbol-function 'application--run-turn)))
       (flet ((activities ()
                "Read the agent rows sent to presenting terminals."
                (with-terminal-ui-locked (ui)
                  (getf (channel-presentation-state ui ':agent-activities) :activities))))
         (test-call-with-function-replacements
          (list (list 'application--run-turn
                      (lambda (application text &rest arguments)
                        (if (equal "bactivitykid" (application-world--served-channel application))
                            (progn
                              (sb-thread:signal-semaphore entered)
                              (sb-thread:wait-on-semaphore release :timeout 20)
                              ':continue)
                            (apply run application text arguments)))))
          (lambda ()
            (dolist (sender (list (participant-identifier human) "al-bactivityparent" "al-bactivitypeer"))
              (test-assert (null (activities)) "An idle child has no activity row.")
              (unwind-protect
                   (progn
                     (world-message-send configuration :sender sender :channel "bactivitykid"
                                         :text "check child activity")
                     (test-assert (sb-thread:wait-on-semaphore entered :timeout 20)
                                  "The existing child starts the delivered turn.")
                     (test-assert
                      (task-tests--wait-until
                       (lambda ()
                         (let ((rows (activities)))
                           (and (= (length rows) 1)
                                (equal (getf (first rows) :agent) "helper")
                                (eq (getf (first rows) :state) ':running))))
                       10)
                      "The parent receives one running child row without a spawn job."))
                (sb-thread:signal-semaphore release))
              (test-assert (task-tests--wait-until (lambda () (null (activities))) 10)
                           "The row clears when the child finishes its turn."))))))))
  nil)

(-> test-world-server-wake-channel () null)
(defun test-world-server-wake-channel ()
  "Test a message serves the unserved channel its agent resides in, and nothing else."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "bwake11" 1000)
     (world-server--reconcile configuration)
     (let ((started nil))
       (test-call-with-function-replacements
        (list (list 'world-server--start-host
                    (lambda (candidate &key conversation-id directory arguments)
                      (declare (ignore candidate directory))
                      (push (list conversation-id (getf arguments :switch-p)
                                  (getf arguments :fullscreen-p))
                            started)
                      nil)))
        (lambda ()
          (flet ((wake (identifier)
                   "Wake IDENTIFIER and wait until the server's main thread has acted on it.

The main thread runs its jobs in order, so a job queued after the wake has
run once the wake has."
                   (let ((done-p nil))
                     (setf started nil)
                     (funcall *world-message-unserved-function* identifier)
                     (world-server--enqueue server (lambda () (setf done-p t)))
                     (task-tests--wait-until (lambda () done-p) 10))))
            (setf (world-server-fullscreen-p server) t)
            (wake "bwake11")
            (test-assert (equal started '(("bwake11" t t)))
                         "a message to an agent's unserved channel serves it without a boot screen, as terminals last opened channels")
            (wake (world-tests--inbox configuration))
            (test-assert (null started) "a message for the person in the inbox serves nothing")
            (wake "bnone11")
            (test-assert (null started) "an unknown channel serves nothing")
            (let ((lease (conversation-lease-acquire configuration "bwake11")))
              (unwind-protect
                   (progn
                     (wake "bwake11")
                     (test-assert (null started)
                                  "a channel whose conversation another holder leases, as a running child does, is left to that holder"))
                (conversation-lease-release lease)))))))))
  nil)

(-> test-world-server-stop-reason () null)
(defun test-world-server-stop-reason ()
  "Test stopping the server tells each attached terminal why before it lets go."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (identifier  (getf (rest (world-server-tests--open entry (world-tests--workspace root "stop")))
                               :session-id))
            (application (world-host-application (world-server--find-host server identifier))))
       (multiple-value-bind (connection seen await)
           (test-localgroup--present (world-server-tests--session server identifier))
         (declare (ignore seen))
         (unwind-protect
              (progn
                (test-assert (application-operation-find application 'stop-server)
                             "the server stop command is a callable operation")
                (test-assert (find "(stop-server)" (application-operation-completion-entries application)
                                   :key (lambda (entry) (getf entry :name)) :test #'string=)
                             "the stop operation participates in command completion")
                (application-operation-call application 'stop-server)
                (test-assert (world-server-stopping-p server)
                             "the interactive stop operation uses the established server shutdown")
                (test-assert (funcall await ':exit
                                      (lambda (event)
                                        (let ((message (getf (rest event) :message)))
                                          (and (stringp message)
                                               (search "world server stopped by autolith stop"
                                                       message)))))
                             "the attached terminal is told the server was stopped by autolith stop"))
           (localgroup-client-close connection))))))
  nil)

(-> world-server-tests--call-with-terminal-client
    (configuration string function &key (:read-only-p boolean) (:client-version (option string)))
    null)
(defun world-server-tests--call-with-terminal-client
    (configuration identifier function &key read-only-p client-version)
  "Run a terminal client's view of channel IDENTIFIER on a thread while FUNCTION drives it.

FUNCTION is called once the channel accepted the view, with a function that
types one terminal event at the client, a function that waits for the view
to end, and the view. The waiting function returns why the view ended, which
is NIL after a detach, and as a second value whether it ended in time. The
keys come from a test terminal. Everything else is the real view: it
attaches as a client that presents, applies what the channel publishes, and
acts on the keys. With READ-ONLY-P the view only watches. CLIENT-VERSION is
the version the client says it is, when it says one."
  (let* ((terminal (make-instance 'queued-recording-terminal :rows 31 :columns 91))
         (screen   (localgroup-screen-create :terminal terminal))
         (view     (channel-view-create configuration identifier screen :read-only-p read-only-p))
         (attached (sb-thread:make-semaphore))
         (exit     nil)
         (ended-p  nil)
         (thread   nil))
    (test-call-with-function-replacements
     (list (list 'terminal--terminal-mode-or-nil
                 (lambda (candidate)
                   (declare (ignore candidate))
                   t))
           (list 'platform-watch-terminal-resize
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   nil)))
     (lambda ()
       (setf thread
             (make-thread
              (lambda ()
                (setf exit
                      (handler-case
                          (channel-view-run view
                                            :client-version    client-version
                                            :attached-function (lambda ()
                                                                 (sb-thread:signal-semaphore attached)))
                        (error (condition)
                          (sb-thread:signal-semaphore attached)
                          (list ':failed :message (princ-to-string condition))))
                      ended-p t))
              :name "Autolith test terminal client"))
       (unwind-protect
            (progn
              (test-assert (and (sb-thread:wait-on-semaphore attached :timeout 20)
                                (not ended-p))
                           "the terminal client attaches to its channel")
              (funcall function
                       (lambda (event)
                         (queued-recording-terminal-enqueue terminal event))
                       (lambda ()
                         (sb-thread:join-thread thread :default nil :timeout 10)
                         (values exit ended-p))
                       view))
         ;; A view that still runs ends at the detach key.
         (queued-recording-terminal-enqueue terminal ':detach)
         (sb-thread:join-thread thread :default nil :timeout 10)
         (localgroup-client-screen-release screen)))))
  nil)

(-> test-world-server-control-z-detach () null)
(defun test-world-server-control-z-detach ()
  "Detach a terminal client through Ctrl-Z while its channel's work completes."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (identifier  (getf (rest (world-server-tests--open
                                      entry (world-tests--workspace root "detach")))
                               :session-id))
            (host        (world-server--find-host server identifier))
            (application (world-host-application host))
            (controller  (application-input-controller application)))
       (dolist (text (list (string (code-char 26))
                           (format nil "~C[122;5u" #\Escape)
                           (format nil "~C[90;5u" #\Escape)))
         (let ((gate      (sb-thread:make-semaphore :count 0))
               (started   (sb-thread:make-semaphore :count 0))
               (completed (sb-thread:make-semaphore :count 0))
               (runner    nil)
               (run-input (symbol-function 'application-run-lisp-input))
               (source    "(world-server-detach-test-work)"))
           (unwind-protect
                (test-call-with-function-replacements
                 (list
                  (list 'application-run-lisp-input
                        (lambda (candidate input &rest arguments)
                          (if (and (eq candidate application) (string= input source))
                              (progn
                                (setf runner sb-thread:*current-thread*)
                                (sb-thread:signal-semaphore started)
                                (sb-thread:wait-on-semaphore gate)
                                (conversation-append-user-message
                                 (application-conversation application) "completed after detach")
                                (sb-thread:signal-semaphore completed)
                                :continue)
                              (apply run-input candidate input arguments)))))
                 (lambda ()
                   (world-server-tests--call-with-terminal-client
                    configuration identifier
                    (lambda (type await-exit view)
                      (declare (ignore view))
                      (test-assert (world-host-attached-p host)
                                   "the terminal client shows the channel")
                      ;; Compile cold native highlighting before timing the detach event.
                      (application-input-controller--handle-submission controller "(values)" :steer-p nil)
                      (test-assert (task-tests--wait-until
                                    (lambda () (not (application-input-controller-busy-p controller))) 60)
                                   "the host completes its initial Lisp presentation before timed input")
                      (application-input-controller--handle-submission controller source :steer-p nil)
                      (test-assert (sb-thread:wait-on-semaphore started :timeout 5)
                                   "the host runner starts the queued work")
                      (funcall type (terminal--decode-editing-event (make-string-input-stream text)))
                      (test-assert (multiple-value-bind (exit ended-p) (funcall await-exit)
                                     (and ended-p (null exit)))
                                   "Ctrl-Z ends the terminal client's view as a detach")
                      (test-assert (task-tests--wait-until
                                    (lambda () (not (world-host-attached-p host))) 5)
                                   "Ctrl-Z releases its client")
                      (test-assert (and runner (thread-alive-p runner)
                                        (application-input-controller-busy-p controller)
                                        (not (world-server-stopping-p server))
                                        (eq host (world-server--find-host server identifier)))
                                   "detachment does not cancel ongoing work or close its channel")
                      (sb-thread:signal-semaphore gate)
                      (test-assert (sb-thread:wait-on-semaphore completed :timeout 5)
                                   "the same work completes after its client detached")
                      (test-assert (task-tests--wait-until
                                    (lambda () (not (application-input-controller-busy-p controller))) 5)
                                   "the host runner returns to idle after completing its work")))))
             (sb-thread:signal-semaphore gate)))))))
  nil)

(-> test-world-server-status-line () null)
(defun test-world-server-status-line ()
  "Test status leads with the world server's process and version while a server runs."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore server root))
     (let ((output (test-call-with-function-replacements
                    (list (list 'localgroup--client-configuration (lambda () configuration)))
                    (lambda ()
                      (with-output-to-string (*standard-output*)
                         (let ((command (parse-command-line (main--top-level-command) '("status"))))
                           (funcall (command-handler command) command)))))))
       (test-assert (and (search "· world server PID" output)
                         (search (princ-to-string (current-process-id)) output))
                    "status names the world server's process first"))))
  nil)

(-> test-world-client-stop-outcomes () null)
(defun test-world-client-stop-outcomes ()
  "Exercise stop CLI acknowledgement, exit observation, styling and failure statuses."
  (with-test-configuration (configuration)
    (let ((pid                         42)
          (present-p                   t)
          (reply                       ':ok)
          (states                      '(:dead))
          (probes                      nil)
          (styles                      nil)
          (styled-p                    t)
          (*world-client-stop-timeout* 0)
          (write-line                  (symbol-function 'localgroup--write-styled-line))
          (*standard-output*           (make-string-output-stream))
          (*error-output*              (make-string-output-stream)))
      (test-call-with-function-replacements
       (list
        (list 'uiop:quit
              (lambda (status &optional finish-output-p)
                (declare (ignore finish-output-p))
                (throw 'stop-cli-exit status)))
        (list 'localgroup--client-configuration (lambda () configuration))
        (list 'world-client-entry
              (lambda (client)
                (declare (ignore client))
                (when present-p
                  (cons #P"unused.sexp"
                        (list :localgroup-endpoint :session-id "world"
                              :pid pid :port 1234 :token "test")))))
        (list 'daemon-call
              (lambda (port token operation &key arguments)
                (declare (ignore port token arguments))
                (test-assert (eq operation ':stop) "stop sends one shutdown request")
                (if (eq reply ':transport-error)
                    (error "Transport failed.")
                    (list reply :operation operation :message "Rejected."))))
        (list 'localgroup--record-stale-p
              (lambda (record condition) (declare (ignore record condition)) nil))
        (list 'localgroup--status-output-styled-p
              (lambda (stream) (declare (ignore stream)) styled-p))
        (list 'localgroup--write-styled-line
              (lambda (stream spans enabled-p)
                (push (mapcar #'terminal-span-style spans) styles)
                (funcall write-line stream spans enabled-p)))
        (list 'ls-compat.posix:process-state
              (lambda (actual-pid)
                (test-assert (notany (lambda (line) (member ':success line)) styles)
                             "acknowledgement never reports success before exit")
                (push actual-pid probes)
                (let ((state (if (rest states) (pop states) (first states))))
                  (if (eq state ':error) (error "Inspection failed.") state)))))
       (lambda ()
         (labels ((invoke ()
                    "Return the actual CLI status without exiting the worker."
                    (catch 'stop-cli-exit
                      (main '("stop"))
                      0)))
           (dolist (case '((42 (:dead) :ok t 0)
                           (42 (:alive) :ok t 64)
                           (42 (:unknown) :ok t 64)
                           (42 (:error) :ok t 64)
                           (nil (:dead) :ok t 64)
                           (0 (:dead) :ok t 64)
                           (-1 (:dead) :ok t 64)
                           (42 (:dead) :error t 64)
                           (42 (:dead) :transport-error t 64)
                           (42 (:dead) :ok nil 0)))
             (setf pid       (first case)
                   states    (copy-list (second case))
                   reply     (third case)
                   present-p (fourth case)
                   probes    nil
                   styles    nil)
             (test-assert (= (fifth case) (invoke))
                          "stop succeeds only on confirmed exit or an absent server")
             (test-assert (equal probes (and present-p (eq reply ':ok)
                                             (typep pid '(integer 1 *)) (list pid)))
                          "only an accepted request's positive recorded PID is observed")
             (test-assert (eq (not (null (some (lambda (line) (member ':success line)) styles)))
                              (and present-p (zerop (fifth case))))
                          "only confirmed shutdown receives success styling")
             (when (and present-p (eq reply ':ok))
               (test-assert (some (lambda (line) (member ':notice line)) styles)
                            "the pending shutdown is highlighted as a notice"))
             (get-output-stream-string *standard-output*))
           (let ((*world-client-stop-timeout* 1))
             (setf pid 42 present-p t reply ':ok states '(:alive :unknown :dead)
                   probes nil styles nil)
             (test-assert (zerop (invoke)) "stop waits through a delayed process exit")
             (test-assert (equal probes '(42 42 42))
                          "the process is observed until confirmed absence")
             (test-assert (search (terminal-style-sequence ':success)
                                  (get-output-stream-string *standard-output*))
                          "interactive confirmation emits its trusted success style"))
           (setf states '(:dead) styles nil probes nil styled-p nil)
           (test-assert (zerop (invoke)) "plain output observes the same shutdown lifecycle")
           (test-assert (not (find #\Escape (get-output-stream-string *standard-output*)))
                        "redirected output contains no terminal styles"))))))
  nil)

(-> test-world-client-exit-observation () null)
(defun test-world-client-exit-observation ()
  "Observe bounded PID snapshots without inferring identity or treating errors as exit."
  (let ((calls                       0)
        (state                       ':alive)
        (*world-client-stop-timeout* 0))
    (test-call-with-function-replacements
     (list (list 'ls-compat.posix:process-state
                 (lambda (pid)
                   (test-assert (= pid 42) "only the positive recorded PID is probed")
                   (incf calls)
                   (cond
                     ((eq state ':error)
                      (error "Inspection failed."))
                     ((eq state ':exiting)
                      (if (< calls 3) ':unknown ':dead))
                     (t
                      state)))))
     (lambda ()
       (dolist (pid '(nil 0 -1 "42" 1/2))
         (test-assert (eq ':unknown (world-client--observe-exit pid))
                      "invalid PIDs yield uncertainty"))
       (test-assert (zerop calls) "invalid PIDs never reach the process probe")
       (dolist (case '((:alive :alive) (:dead :dead) (:unknown :unknown)
                       (:error :unknown)))
         (setf state (first case) calls 0)
         (test-assert (eq (second case) (world-client--observe-exit 42))
                      "zero budget returns one immediate observation, including uncertainty")
         (test-assert (= 1 calls) "zero budget never polls again"))
       (let ((*world-client-stop-timeout* 1)
             (start                       (get-internal-real-time)))
         (setf state ':dead calls 0)
         (test-assert (eq ':dead (world-client--observe-exit 42))
                      "confirmed exit completes immediately")
         (test-assert (and (= calls 1)
                           (< (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second) 0.5))
                      "confirmed exit does not consume the observation budget"))
       (let ((*world-client-stop-timeout* 1))
         (setf state ':exiting calls 0)
         (test-assert (eq ':dead (world-client--observe-exit 42))
                      "uncertain observations may resolve to confirmed absence")
         (test-assert (= calls 3) "observation stops at the first confirmed absence"))
       (let ((*world-client-stop-timeout* 0.02)
             (start                       (get-internal-real-time)))
         (setf state ':alive calls 0)
         (test-assert (eq ':alive (world-client--observe-exit 42))
                      "a live PID remains a snapshot, including possible PID reuse")
         (test-assert (>= calls 2) "the PID is observed again at the deadline")
         (test-assert (< (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second) 0.5)
                      "a short budget caps sleep rather than using a full default interval")))))
  nil)

(-> test-world-server-new-channel () null)
(defun test-world-server-new-channel ()
  "Test (new) lays the new channel out for the current terminal and shows it what was presented before it attached."
  (dolist (presentation '((nil nil 30 90) (t nil 31 97) (t t 37 109)))
    (destructuring-bind (attached-p styled-p rows columns) presentation
      (world-server-tests--call
       (lambda (server configuration root)
         (let* ((entry       (world-client-entry configuration))
                (first-id    (getf (rest (world-server-tests--open
                                          entry (world-tests--workspace root "new") :fullscreen-p t))
                                  :session-id))
                (application (world-host-application (world-server--find-host server first-id)))
                (ui          (application-ui application))
                (terminal    (terminal-ui-terminal ui)))
           (multiple-value-bind (connection seen await)
               (if attached-p
                   (test-localgroup--present (world-server-tests--session server first-id)
                                             :rows rows :columns columns :styled-p styled-p)
                   (values nil nil nil))
             (declare (ignore seen))
             (unwind-protect
                  (progn
                    (when attached-p
                      (with-terminal-ui-locked (ui)
                        (test-assert (and (= rows (terminal-rows terminal))
                                          (= columns (terminal-columns terminal))
                                          (eq styled-p (not (null (terminal-styled-p terminal)))))
                                     "the current channel is laid out for the terminal client that presents it")))
                    (application--builtin-new-command application)
                    (let* ((hosts  (mapcar #'world-host-channel (world-server--hosts server)))
                           (second (find-if-not (lambda (identifier) (string= identifier first-id)) hosts)))
                      (test-assert (and (= (length hosts) 2)
                                        (string= (conversation-identifier (application-conversation application))
                                                 first-id))
                                   "the old channel keeps its conversation while the new channel runs beside it")
                      (when attached-p
                        (test-assert (and second
                                          (funcall await ':switch
                                                   (lambda (event)
                                                     (equal (getf (rest event) :session-id) second))))
                                     "the terminal is sent to the new channel"))
                      (let* ((next-application (world-host-application (world-server--find-host server second)))
                             (next-ui          (application-ui next-application))
                             (next-terminal    (terminal-ui-terminal next-ui)))
                        (with-terminal-ui-locked (next-ui)
                          (test-assert (and (= rows (terminal-rows next-terminal))
                                            (= columns (terminal-columns next-terminal)))
                                       "the new channel is laid out at the current channel's dimensions before a terminal attaches"))
                        (application-present next-application
                                             (list (terminal-span ':brand "new-channel presentation")))
                        (multiple-value-bind (next-connection next-seen next-await)
                            (test-localgroup--present (world-server-tests--session server second)
                                                      :rows rows :columns columns :styled-p styled-p)
                          (declare (ignore next-seen))
                          (unwind-protect
                               (test-assert
                                (funcall next-await ':append
                                         (lambda (event)
                                           (let ((entry (getf (rest event) :entry)))
                                             (and (listp entry)
                                                  (find '(:brand "new-channel presentation") entry
                                                        :test #'equal)))))
                                "the terminal that attaches is shown what the new channel presented before, with its style")
                            (localgroup-client-close next-connection))))))
               (when connection
                 (localgroup-client-close connection)))))))))
  nil)

(-> test-world-client-new-command () null)
(defun test-world-client-new-command ()
  "Create two distinct unattached channels in the requested workspace through the CLI."
  (world-server-tests--call
   (lambda (server configuration root)
     (test-call-with-function-replacements
      (list (list 'localgroup--client-configuration (lambda () configuration)))
      (lambda ()
        (let ((identifiers nil))
          (dotimes (index 2)
            (let* ((output
                     (with-output-to-string (*standard-output*)
                       (let ((command (parse-command-line (main--top-level-command)
                                                          (list "new" (namestring root)))))
                         (funcall (command-handler command) command))))
                   (host (find-if-not (lambda (host)
                                        (member (world-host-channel host) identifiers :test #'equal))
                                      (world-server--hosts server))))
              (test-assert host "each invocation hosts a fresh channel in the same workspace")
              (let* ((identifier (world-host-channel host))
                     (session    (world-server-tests--session server identifier))
                     (status     (rest (localgroup-status-snapshot session))))
                (push identifier identifiers)
                (test-assert (search (channel-open-command identifier) output)
                             "new prints a usable open command for the channel it created")
                (test-assert (and (equal (getf status :cwd) (world-workspace-name root))
                                  (not (getf status :terminal-client-pid)))
                             "the created channel serves the requested directory without a client")))))))))
  nil)

(-> test-world-server-tell () null)
(defun test-world-server-tell ()
  "Exercise CLI tells and interchannel messages through durable delivery and cold-host scheduling."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "btell11" 1000)
     (world-tests--persisted-conversation configuration "btell22" 2000)
     (world-server--reconcile configuration)
     (world-participant-rename configuration "al-btell11" "receiver")
     (let ((started nil))
       (test-call-with-function-replacements
        (list (list 'localgroup--client-configuration (lambda () configuration))
              (list 'world-server--start-host
                    (lambda (candidate &key conversation-id directory arguments)
                      (declare (ignore candidate directory arguments))
                      (push conversation-id started)
                      nil)))
        (lambda ()
          (labels ((drain-wakeups ()
                     "Wait for the server's main thread to finish the queued wakeups."
                     (let ((done-p nil))
                       (world-server--enqueue server (lambda () (setf done-p t)))
                       (test-assert (task-tests--wait-until (lambda () done-p) 10)
                                    "message wakeups complete on the server's main thread")))

                   (delivery (text)
                     "Return the receiver's durable copy of TEXT."
                     (find text (world-inbox-deliveries configuration "btell11")
                           :key #'world-delivery-text :test #'string=)))
            (loop for selector in '("btell11" "b-tell11" "tell11" "receiver"
                                    "@RECEIVER" "al-btell11")
                  for index from 1
                  for text = (format nil "from CLI ~D" index)
                  do (setf started nil)
                     (let ((output
                             (with-output-to-string (*standard-output*)
                               (let ((command (parse-command-line
                                               (main--top-level-command)
                                               (list "tell" selector text))))
                                 (funcall (command-handler command) command)))))
                       (drain-wakeups)
                       (test-assert
                        (and (equal started '("btell11"))
                             (search (conversation-identifier-display "btell11") output)
                             (delivery text)
                             (eq (world-delivery-sender-kind (delivery text)) ':human))
                        "CLI IDs and agent designators deliver as the person and schedule the cold recipient")))
            (dolist (case '(("receiver" "from another channel" :post)
                            ("btell22" "@receiver please review" :mention)))
              (destructuring-bind (channel text kind) case
                (setf started nil)
                (world-message-send configuration :sender "al-btell22" :channel channel :text text)
                (drain-wakeups)
                (test-assert (and (equal started '("btell11"))
                                  (delivery text)
                                  (eq (world-delivery-sender-kind (delivery text)) ':agent)
                                  (eq (world-delivery-kind (delivery text)) kind))
                             "a direct message or mention from another channel schedules its cold recipient")))))))))
  nil)

(-> test-world-server-tell-input-dispatch () null)
(defun test-world-server-tell-input-dispatch ()
  "Dispatch CLI Lisp in cold and busy channels without sending it to a model or opening approvals."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "btell31" 1000)
     (world-server--reconcile configuration)
     (world-participant-rename configuration "al-btell31" "cli-receiver")
     (let ((application nil)
           (picks       0))
       (test-call-with-function-replacements
        (list (list 'localgroup--client-configuration (lambda () configuration))
              (list 'terminal-ui-select
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf picks)
                      nil)))
        (lambda ()
          (labels ((send (selector source)
                     "Submit SOURCE through the CLI and return its acknowledgment."
                     (with-output-to-string (*standard-output*)
                       (let ((command (parse-command-line (main--top-level-command)
                                                          (list "tell" selector source))))
                         (funcall (command-handler command) command))))

                   (operations ()
                     "Return the recipient's durable local evaluations."
                     (conversation-user-operation-snapshot (application-conversation application)))

                   (await-operation (source count)
                     "Wait for the exact SOURCE after COUNT earlier evaluations and return its record."
                     (let ((record nil))
                       (test-assert
                        (task-tests--wait-until
                         (lambda ()
                           (setf record (find source (nthcdr count (operations))
                                              :key  (lambda (item) (getf (rest item) :source))
                                              :test #'string=)))
                         30)
                        (format nil "~S completes in its destination; operations: ~S; turn: ~S"
                                source (operations)
                                (application-input-controller-turn-state
                                 (application-input-controller application))))
                       record)))
            (let* ((source "(format t \"hello from autolith tell\")")
                   (output (send "@CLI-RECEIVER" source)))
              (setf application (world-host-application (world-server--find-host server "btell31")))
              (let ((record (await-operation source 0)))
                (test-assert (and (eq (getf (rest record) :status) ':ok)
                                  (search "hello from autolith tell" (getf (rest record) :result)))
                             "a tell to a cold channel evaluates Lisp and retains captured output"))
              (test-assert (and (search (conversation-identifier-display "btell31") output)
                                (not (search "hello from autolith tell" output)))
                           "tell acknowledges routing without forwarding destination evaluation output"))
            (let* ((source "(permissions :auto)")
                   (count  (length (operations))))
              (send "btell31" source)
              (test-assert (eq (getf (rest (await-operation source count)) :status) ':ok)
                           "an explicit authorized action executes without an interactive choice"))
            (let* ((source "(delete)")
                   (count  (length (operations))))
              (send "tell31" source)
              (let ((record (await-operation source count)))
                (test-assert (and (eq (getf (rest record) :status) ':aborted)
                                  (non-empty-string-p (getf (rest record) :result)))
                             (format nil "bare delete aborts with a refusal explanation: ~S" record))))
            (let* ((controller (application-input-controller application))
                   (source     "(format t \"queued CLI Lisp\")")
                   (count      (length (operations))))
              (test-assert (task-tests--wait-until
                            (lambda () (not (application-input-controller-busy-p controller))) 10)
                           "earlier work finishes before the busy fixture takes ownership")
              (application-input-controller-begin-external-work controller)
              (test-assert (application-input-controller-turn-active-p controller)
                           "the busy fixture owns an active turn")
              (unwind-protect
                   (progn
                     (send "btell31" source)
                     (test-assert (= (length (operations)) count)
                                  "ordinary Lisp waits for the busy channel's idle boundary")
                     (let ((before (length (operations))))
                       (send "btell31" "(delete)")
                       (let ((record (await-operation "(delete)" before)))
                         (test-assert (and (eq (getf (rest record) :status) ':aborted)
                                           (non-empty-string-p (getf (rest record) :result)))
                                      (format nil "busy delete aborts with a refusal explanation: ~S" record)))))
                (application-input-controller-finish-external-work controller))
              (test-assert (eq (getf (rest (await-operation source (1+ count))) :status) ':ok)
                           "deferred CLI Lisp executes through the ordinary work queue"))
            (test-assert (and (zerop picks)
                              (world-channel-find (world-load configuration) "btell31")
                              (null (world-inbox-deliveries configuration "btell31")))
                         "Lisp opens no picker, deletes nothing without confirmation, and never enters message delivery")))))))
  nil)

(-> test-world-server-checkpoint-cancel () null)
(defun test-world-server-checkpoint-cancel ()
  "Test a checkpoint waiting for running work can be cancelled, and the channels go on without it."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (identifier  (getf (rest (world-server-tests--open entry (world-tests--workspace root "c")))
                               :session-id))
            (application (world-host-application (world-server--find-host server identifier)))
            (controller  (application-input-controller application))
            (ran-p       nil)
            (thread      nil))
       (test-assert (not (world-server-cancel-checkpoint server))
                    "with no checkpoint waiting there is nothing to cancel")
       (application-input-controller-begin-external-work controller)
       (unwind-protect
            (progn
              (setf thread (make-thread
                            (lambda ()
                              (world-server-call-quiesced server application
                                                          (lambda () (setf ran-p t))))
                            :name "Autolith world server test cancelled checkpoint"))
              (test-assert (task-tests--wait-until (lambda () (application-world-held-p application)) 10)
                           "the checkpoint holds the channel while its work runs")
              (test-assert (world-server-cancel-checkpoint server)
                           "a waiting checkpoint can be cancelled")
              (sb-thread:join-thread thread :default nil :timeout 30)
              (test-assert (and (not ran-p)
                                (not (application-world-held-p application))
                                (not (world-server-checkpointing-p server)))
                           "the cancelled checkpoint never saves, and the channel goes on"))
         (application-input-controller-finish-external-work controller)))))
  nil)

(-> test-world-server-hosting () null)
(defun test-world-server-hosting ()
  "Test opening, sharing, closing and resuming channels through a world server."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((workspace (world-tests--workspace root "project"))
            (entry     (world-client-entry configuration))
            (opened    (world-server-tests--open entry workspace))
            (first-id  (getf (rest opened) :session-id)))
       (test-assert (and entry
                         (non-empty-string-p first-id)
                         (getf (rest opened) :started-p))
                    "the first open of a directory starts a channel there")
       (test-assert (eql (getf (rest (rest (localgroup--find-record configuration first-id))) :pid)
                         (current-process-id))
                    "the opened channel publishes its own endpoint from the server")
       (test-assert (string= workspace
                             (world-host-workspace (world-server--find-host server first-id)))
                    "the channel works in the opened directory")
       (test-assert (equal (world-server--channels-of entry) (list first-id))
                    "the server lists the channel it hosts")
       ;; An empty conversation never reaches disk, so give the channel a
       ;; history the world can find again after it closes.
       (conversation-append-user-message
        (application-conversation
         (world-host-application (world-server--find-host server first-id)))
        "keep this channel")
       (let ((joined (world-server-tests--open entry workspace)))
         (test-assert (and (string= (getf (rest joined) :session-id) first-id)
                           (not (getf (rest joined) :started-p)))
                      "a second open of the directory joins the uncontrolled channel"))
       (let ((session (world-server-tests--session server first-id)))
         (test-assert (and (eq (first (daemon-call (image-daemon:daemon-runtime-port session)
                                                   (image-daemon:daemon-runtime-token session)
                                                   ':attach
                                                   :arguments (list :mode ':control :rows 31 :columns 91)))
                               ':error)
                           (not (world-host-attached-p (world-server--find-host server first-id))))
                      "a hosted channel answers the attach operation of an older terminal client with an error"))
       (multiple-value-bind (connection seen await)
           (test-localgroup--present (world-server-tests--session server first-id) :client-pid 424242)
         (declare (ignore seen))
         (unwind-protect
              (let* ((host       (world-server--find-host server first-id))
                     (other      (world-server-tests--open entry workspace))
                     (other-id   (getf (rest other) :session-id))
                     (other-host (world-server--find-host server other-id)))
                (test-assert (and (eq (first (localgroup-client-connection-attached connection))
                                      ':attached)
                                  (funcall await ':reset)
                                  (world-host-attached-p host))
                             "a terminal attaches to the hosted channel as a client that presents it")
                (let ((status (rest (getf (localgroup-client-call connection ':state) :state))))
                  (test-assert (and (getf status :terminal-attached-p)
                                    (eql (getf status :terminal-client-pid) 424242))
                               "the channel's status says a terminal shows it and names its process"))
                (test-assert (and (getf (rest other) :started-p)
                                  (not (string= other-id first-id))
                                  other-host
                                  (string= workspace (world-host-workspace other-host)))
                             "opening the directory of a controlled channel starts another channel")
                (test-assert (equal (sort (world-server--channels-of entry) #'string<)
                                    (sort (list first-id other-id) #'string<))
                             "the server lists both channels")
                (localgroup-query-record entry ':close (list :channel other-id))
                ;; The host leaves the server only after its session released
                ;; every resource, including the endpoint record.
                (test-assert (task-tests--wait-until
                              (lambda () (not (member other-host (world-server--hosts server))))
                              20)
                             "closing a channel ends its session")
                (test-assert (and (null (world-server--find-host server other-id))
                                  (null (localgroup--find-record-or-nil configuration other-id)))
                             "a closed channel unpublishes its endpoint"))
           (localgroup-client-close connection)))
       (test-assert (task-tests--wait-until
                     (lambda ()
                       (not (world-host-attached-p (world-server--find-host server first-id))))
                     5)
                    "the terminal's departure releases the channel")
       (test-assert (world-host-live-p (world-server--find-host server first-id))
                    "a channel outlives the terminal that left without detaching")
       (localgroup-query-record entry ':close (list :channel first-id))
       (test-assert (task-tests--wait-until
                     (lambda () (null (world-server--hosts server)))
                     20)
                    "closing the last channel leaves the server running without channels")
       (test-assert (getf (rest (localgroup-query-record entry ':status)) :status)
                    "the server answers with no channels open")
       (let ((resumed (world-server-tests--open entry workspace)))
         (test-assert (and (getf (rest resumed) :started-p)
                           (string= (getf (rest resumed) :session-id) first-id))
                      "opening the directory again resumes its latest channel")
         ;; The composer is the terminal client's, so the client's view recalls the input.
         (world-server-tests--call-with-terminal-client
          configuration first-id
          (lambda (type await-exit view)
            (declare (ignore await-exit))
            (let* ((ui     (channel-view-ui view))
                   (editor (terminal-ui-editor ui)))
              (flet ((composer-p (text)
                       "Wait until the terminal's composer holds TEXT and return whether it does."
                       (task-tests--wait-until
                        (lambda () (string= text (line-editor-text editor))) 10)))
                (let ((given-p (task-tests--wait-until
                                (lambda ()
                                  (find "keep this channel" (line-editor-history editor)
                                        :test #'equal))
                                10)))
                  (test-assert given-p
                               "the terminal that attaches to a reopened channel is given its durable input")
                  (when given-p
                    (with-terminal-ui-locked (ui)
                      (line-editor-set-text editor "draft" :cursor 2))
                    (funcall type ':history-previous)
                    (test-assert (composer-p "keep this channel")
                                 "a reopened channel immediately recalls its durable input")
                    (funcall type ':history-next)
                    (test-assert (and (composer-p "draft")
                                      (= 2 (line-editor-cursor editor)))
                                 "the reopened channel restores its draft after recall"))))))))
       (localgroup-query-record entry ':close (list :channel first-id))
       (test-assert (task-tests--wait-until
                     (lambda () (null (world-server--hosts server)))
                     20)
                    "the resumed channel closes again")
       (let ((lease (conversation-lease-acquire configuration first-id)))
         (unwind-protect
              (let ((fresh (world-server-tests--open entry workspace)))
                (test-assert (and (getf (rest fresh) :started-p)
                                  (not (string= (getf (rest fresh) :session-id) first-id)))
                             "a channel whose conversation another process holds is skipped"))
           (conversation-lease-release lease)))
       (test-assert (handler-case
                        (progn
                          (localgroup-query-record entry ':open (list :directory "/nonexistent/autolith"))
                          nil)
                      (localgroup-error (condition)
                        (search "does not exist" (autolith-error-message condition))))
                    "opening a missing directory is refused")
       (test-assert (handler-case
                        (progn
                          (localgroup-query-record entry ':close (list :channel "nosuch1"))
                          nil)
                      (localgroup-error (condition)
                        (search "not open" (autolith-error-message condition))))
                    "closing an unknown channel is refused"))))
  nil)

(-> test-world-server-switch () null)
(defun test-world-server-switch ()
  "Test (open) asking the controlling terminal to move to another channel."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (workspace-a (world-tests--workspace root "a"))
            (workspace-b (world-tests--workspace root "b"))
            (id-a        (getf (rest (world-server-tests--open entry workspace-a)) :session-id))
            (id-b        (getf (rest (world-server-tests--open entry workspace-b)) :session-id))
            (application (world-host-application (world-server--find-host server id-a)))
            (presented   nil))
       (test-call-with-function-replacements
        (list (list 'application-present
                    (lambda (application entry &key identifier)
                      (declare (ignore application identifier))
                      (push entry presented))))
        (lambda ()
          (application--builtin-open-command application id-a)
          (test-assert (search "already" (terminal--spans-text (first presented)))
                       "switching to the channel the terminal is in changes nothing")
          (application--builtin-open-command application (conversation-identifier-display id-b))
          (test-assert (search "no terminal controls" (terminal--spans-text (first presented)))
                       "without a controller there is no terminal to move")
          (multiple-value-bind (connection seen await)
              (test-localgroup--present (world-server-tests--session server id-a))
            (declare (ignore seen))
            (unwind-protect
                 (progn
                   (test-assert (application-controlling-client application)
                                "the terminal client that attached controls the channel")
                   (setf presented nil)
                   (application--builtin-open-command application (conversation-identifier-display id-b))
                   (test-assert (null presented)
                                "a controlled channel switches without a line about it")
                   (test-assert (funcall await ':switch
                                         (lambda (event)
                                           (equal (getf (rest event) :session-id) id-b)))
                                "the controller is sent the switch request naming the channel"))
              (localgroup-client-close connection)))
          (test-assert (and (eq (application--builtin-open-command application "zzzzzzz") ':continue)
                            (search "no channel matches" (terminal--spans-text (first presented))))
                       "an unknown channel is said in a line, not signaled"))))))
  nil)

(-> test-world-server-startup-reservations () null)
(defun test-world-server-startup-reservations ()
  "Exercise shared construction, unrelated admission, checkpoint cancellation and an end-to-end timeout."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((workspace-a  (world-tests--workspace root "reservation-a"))
            (workspace-b  (world-tests--workspace root "reservation-b"))
            (entry        (world-client-entry configuration))
            (ready        (world-server-tests--open entry workspace-b))
            (identifier-b (getf (rest ready) :session-id))
            (build        (symbol-function 'application--build))
            (lease        (symbol-function 'conversation-lease-acquire))
            (load         (symbol-function 'conversation-load))
            (entered      (sb-thread:make-semaphore))
            (release      (sb-thread:make-semaphore))
            (b-ready      (sb-thread:make-semaphore))
            (threads      nil)
            (results      (make-array 3 :initial-element nil))
            (builds 0) (leases 0) (loads 0) (locked-builds 0)
            (checkpoint nil) (checkpoint-ran-p nil))
       (test-call-with-function-replacements
        (list
         (list 'application--build
               (lambda (candidate &rest arguments)
                 (when (equal (config :working-directory candidate)
                              (uiop:ensure-directory-pathname workspace-a))
                   (incf builds)
                   (when (sb-thread:holding-mutex-p (world-server-open-lock server))
                     (incf locked-builds))
                   (sb-thread:signal-semaphore entered)
                   (sb-thread:wait-on-semaphore release))
                 (apply build candidate arguments)))
         (list 'conversation-lease-acquire
               (lambda (candidate identifier)
                 (when (equal (config :working-directory candidate)
                              (uiop:ensure-directory-pathname workspace-a))
                   (incf leases))
                 (funcall lease candidate identifier)))
         (list 'conversation-load
               (lambda (&rest arguments)
                 (incf loads)
                 (apply load arguments))))
        (lambda ()
          (labels ((open-on-thread (index arguments &optional done)
                     "Start an independent waiter and retain its response or failure."
                     (let ((thread (make-thread
                                    (lambda ()
                                      (setf (aref results index)
                                            (handler-case (world-server-open server arguments)
                                              (error (condition) condition)))
                                      (when done (sb-thread:signal-semaphore done)))
                                    :name "Autolith reservation test waiter")))
                       (push thread threads))))
            (unwind-protect
                 (progn
                   (open-on-thread 0 (list :directory workspace-a))
                   (test-assert (sb-thread:wait-on-semaphore entered :timeout 20)
                                "A construction reaches its deterministic latch")
                   (open-on-thread 1 (list :directory workspace-a))
                   (open-on-thread 2 (list :channel identifier-b) b-ready)
                   (test-assert (and (sb-thread:wait-on-semaphore b-ready :timeout 20)
                                     (eq (first (aref results 2)) ':ok)
                                     (= builds 1) (zerop locked-builds) (zerop loads))
                                "ready B opens with no transcript loads while one unlocked A construction is blocked")
                   (let* ((*world-server-open-timeout-seconds* 0.05)
                          (failure                             (handler-case (world-server-open server (list :directory workspace-a))
                                                                 (world-error (condition) condition)))
                          (status                              (world-server-status server))
                          (startup                             (and (typep failure 'world-error)
                                                                    (find (world-error-identifier failure) (getf (rest status) :startups)
                                                                          :key (lambda (state) (getf state :identifier)) :test #'equal))))
                     (test-assert (and startup
                                       (eq (world-error-operation failure) ':open-timeout)
                                       (eq (getf startup :phase) ':building)
                                       (getf startup :pending-p)
                                       (<= (getf (getf startup :timings) :reserved-at)
                                           (getf (getf startup :timings) :build-started-at))
                                       (>= (getf (getf startup :timings) :lock-wait) 0)
                                       (= builds 1))
                                  "a timed-out waiter identifies one still-owned construction and monotonic admission measurements"))
                   (setf checkpoint
                         (make-thread
                          (lambda ()
                            (world-server--call-with-quiescer
                             server :application (world-host-application (world-server--find-host server identifier-b))
                             :reconnect-id       "reservation-test"
                             :function           (lambda () (setf checkpoint-ran-p t))))
                          :name "Autolith reservation test checkpoint"))
                   (test-assert (and (task-tests--wait-until (lambda () (world-server-holding-p server)) 10)
                                     (not (world-server-checkpointing-p server))
                                     (world-server-cancel-checkpoint server))
                                "checkpoint admission waits for construction while status and cancellation remain available")
                   (join-thread checkpoint)
                   (setf checkpoint nil)
                   (test-assert (not checkpoint-ran-p) "cancellation prevents entry into the save boundary"))
              (sb-thread:signal-semaphore release)
              (when checkpoint (world-server-cancel-checkpoint server) (join-thread checkpoint))
              (dolist (thread threads) (join-thread thread)))
            (let* ((first (aref results 0)) (second (aref results 1))
                   (identifier (and (listp first) (getf (rest first) :session-id)))
                   (host       (and identifier (world-server--find-host server identifier))))
              (test-assert (and host (listp second)
                                (equal identifier (getf (rest second) :session-id))
                                (not (eq (getf (rest first) :started-p) (getf (rest second) :started-p)))
                                (= builds 1) (= leases 1)
                                (= 1 (count identifier (world-server--hosts server) :key #'world-host-channel :test #'equal))
                                (conversation-lease-held-p (application-conversation-lease (world-host-application host))))
                           "all A waiters settle on one host, one construction owner and one live lease"))))))))
  nil)

(-> test-world-server-starting-host () null)
(defun test-world-server-starting-host ()
  "Test an open joins a host that is still starting instead of starting another."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((workspace (world-tests--workspace root "a"))
            (channel
             (conversation-create
              (configuration-copy configuration :working-directory workspace)
              :identifier "bstart1"))
            (terminal  (make-instance 'recording-terminal :columns 80))
            (starting
             (make-instance 'world-host :application
                            (make-instance 'application :configuration
                                           (configuration-copy configuration
                                                               :working-directory
                                                               workspace)
                                           :conversation channel :ui
                                           (terminal-ui-create :terminal
                                                               terminal))))
            (started   nil))
       (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'world-server--start-host
                   (lambda (candidate &rest arguments)
                     (declare (ignore candidate))
                     (push arguments started)
                     (values starting t))))
            (lambda ()
              (with-lock-held ((world-server-lock server))
                (push starting (world-server-hosts server)))
              (multiple-value-bind (host started-p)
                  (world-server--host-for server (list :channel "bstart1"))
                (test-assert
                 (and (eq host starting) (not started-p) (null started))
                 "an open of a starting channel waits for its host"))
              (multiple-value-bind (host started-p)
                  (world-server--host-for server (list :directory workspace))
                (test-assert
                 (and (eq host starting) (not started-p) (null started))
                 "an open of its workspace joins the starting host nobody controls"))
              (setf (world-host-failure starting) (make-condition 'error)
                    (world-host-state starting) ':retired)
              (multiple-value-bind (host started-p)
                  (world-server--host-for server (list :channel "bstart1"))
                (test-assert
                 (and (eq host starting) started-p (= (length started) 1))
                 "a host that failed to start no longer holds its channel"))))
         (with-lock-held ((world-server-lock server))
           (setf (world-server-hosts server)
                 (remove starting (world-server-hosts server))))))))
  nil)

(-> test-world-server-launch-options () null)
(defun test-world-server-launch-options ()
  "Test a new channel takes the launch options stated, and a running one refuses those it cannot change."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry     (world-client-entry configuration))
            (workspace (world-tests--workspace root "a"))
            (id        (getf (rest (world-server-tests--open entry workspace
                                                             :launch '(:immutable-p t)))
                             :session-id))
            (host      (world-server--find-host server id))
            (built     (application-configuration (world-host-application host))))
       (test-assert (config :immutable-p built)
                    "a new channel is built with the immutable mode the open states")
       (test-assert (string= (getf (rest (world-server-tests--open
                                          entry workspace :launch '(:immutable-p t)))
                                   :session-id)
                             id)
                    "an open stating what the channel already runs joins it")
       (test-assert (and (string= (getf (rest (localgroup-query-record
                                               entry ':open
                                               (list :channel id
                                                     :launch  '(:permission-mode :full-access)
                                                     :rows 30 :columns 90 :styled-p nil)))
                                        :session-id)
                                  id)
                         (eq (application-permission-mode (world-host-application host))
                             ':full-access))
                    "a permission mode stated explicitly takes effect on a channel nobody controls")
       (test-call-with-function-replacements
        (list (list 'localgroup--task-counts
                    (lambda (application)
                      (declare (ignore application))
                      (values 1 1))))
        (lambda ()
          (test-assert (and (handler-case
                                (progn (localgroup-query-record
                                        entry ':open
                                        (list :channel id
                                              :launch  '(:permission-mode :ask)
                                              :rows 30 :columns 90 :styled-p nil))
                                       nil)
                              (autolith-error (condition)
                                (search "while it is working" (autolith-error-message condition))))
                            (eq (application-permission-mode (world-host-application host))
                                ':full-access))
                       "a channel running a job keeps its permission mode and refuses the open that would change it")))
       (test-assert (handler-case
                        (progn (localgroup-query-record
                                entry ':open
                                (list :channel id :launch '(:fullscreen-p t)
                                      :rows 30 :columns 90 :styled-p nil))
                               nil)
                      (autolith-error (condition)
                        (search "--fullscreen" (autolith-error-message condition))))
                    "a running channel refuses an option it cannot change, naming it")
       (test-assert (handler-case
                        (progn (world-server-tests--open entry (world-tests--workspace root "b")
                                                         :launch '(:provider "other"))
                               nil)
                      (autolith-error (condition)
                        (search "--provider" (autolith-error-message condition))))
                    "a provider without a model is refused for a new channel, as on the command line")
       (let ((other (getf (rest (world-server-tests--open entry workspace
                                                          :launch '(:fullscreen-p t)))
                          :session-id)))
         (test-assert (and other (not (string= other id))
                           (config :fullscreen-p
                                   (application-configuration
                                    (world-host-application
                                     (world-server--find-host server other)))))
                      "a workspace open with a conflicting option gets another channel that takes it")
         (test-assert (handler-case
                          (progn (localgroup-query-record
                                  entry ':open
                                  (list :channel other :launch '(:plain-p t)
                                        :rows 30 :columns 90 :styled-p nil))
                                 nil)
                        (autolith-error (condition)
                          (search "--plain" (autolith-error-message condition))))
                      "a fullscreen channel refuses a plain open, naming the option")))))
  nil)

(-> test-world-server-resume-order () null)
(defun test-world-server-resume-order ()
  "Test a workspace opens its latest touched conversation, not its newest channel record."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bolder1" 1000)
    (world-tests--persisted-conversation configuration "bnewer2" 2000)
    (world-reconcile-sessions configuration :now 3000)
    (world-channel-update configuration "bolder1" :title "Older work" :now 3500)
    (world-channel-update configuration "bnewer2" :title "Newer work" :now 3600)
    (let* ((world     (world-load configuration))
           (workspace (channel-workspace (world-channel-find world "bolder1")))
           (server    (make-instance 'world-server
                                     :configuration            configuration
                                     :permission-mode          ':ask
                                     :mutation-replay-failures nil))
           (dates     (list (cons "bolder1" 5000) (cons "bnewer2" 4000))))
      (test-call-with-function-replacements
       (list (list 'conversation-storage-write-date
                   (lambda (pathname)
                     (or (rest (assoc (pathname-name pathname) dates :test #'string=)) 0))))
       (lambda ()
         (test-assert (equal (world-server--latest-candidates server workspace)
                             '(("bolder1") ("bnewer2")))
                      "the conversation written last comes first whatever its channel's record says")
         (test-assert (null (world-server--latest-candidates server "/nowhere/"))
                      "another workspace has no candidates")))))
  nil)

(-> test-world-server-open-latest-touched () null)
(defun test-world-server-open-latest-touched ()
  "Open the newest unattached channel across hosted and cold candidates, streaming real milestones."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry     (world-client-entry configuration))
            (workspace (world-tests--workspace root "a"))
            (hosted    (getf (rest (world-server-tests--open entry workspace)) :session-id)))
       (world-tests--persisted-conversation
        (configuration-copy configuration :working-directory workspace) "blater1" 1000)
       (world-server--reconcile configuration)
       (let ((dates (list (cons hosted 6000) (cons "blater1" 5000)))
             (steps nil))
         (test-call-with-function-replacements
          (list (list 'conversation-storage-write-date
                      (lambda (pathname)
                        (or (rest (assoc (pathname-name pathname) dates :test #'string=)) 0))))
          (lambda ()
            (flet ((open-latest ()
                     "Open through the client transport and record its loading notifications."
                     (setf steps nil)
                     (handler-bind ((channel-loading-progress
                                      (lambda (progress)
                                        (push (channel-loading-progress-step progress) steps))))
                       (world-client-open configuration :directory workspace :switch-p t))))
              (multiple-value-bind (identifier started-p) (open-latest)
                (test-assert (and (string= identifier hosted) (not started-p))
                             "the newest unattended hosted channel wins over a cold channel")
                (test-assert (equal (reverse steps) '(1 2 5 6))
                             "reuse skips history and initialization milestones"))
              (setf (rest (assoc hosted dates :test #'string=)) 1000)
              (multiple-value-bind (identifier started-p) (open-latest)
                (test-assert (and started-p (string= identifier "blater1"))
                             "a newer cold channel wins over the hosted channel")
                (test-assert (equal (reverse steps) '(1 2 3 4 5 6))
                             "cold loading streams its milestones before the final response"))
              (multiple-value-bind (identifier started-p) (open-latest)
                (test-assert (and (string= identifier "blater1") (not started-p))
                             "the next open reuses the loaded newest channel"))
              (let ((connection (test-localgroup--present
                                 (world-server-tests--session server "blater1"))))
                (unwind-protect
                     (multiple-value-bind (identifier started-p) (open-latest)
                       (test-assert (and (string= identifier hosted) (not started-p))
                                    "the attached newest channel is skipped for the next unattended host"))
                  (localgroup-client-close connection))))))))))
  nil)

(-> test-world-server-generation-warning-once () null)
(defun test-world-server-generation-warning-once ()
  "Test the checkpoint warning shows in the first channel a world server opens, not in each."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore configuration root))
     (test-assert (and (world-server-take-generation-warning server)
                       (not (world-server-take-generation-warning server))
                       (not (world-server-take-generation-warning server)))
                  "only the first channel takes the warning")))
  nil)

(-> test-world-server-open-passes-damage () null)
(defun test-world-server-open-passes-damage ()
  "Test a workspace open passes over a channel whose conversation cannot be loaded, and names it."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((workspace (world-tests--workspace root "a"))
            (local     (configuration-copy configuration :working-directory workspace))
            (broken    (world-tests--persisted-conversation local "bbroken1" 1000)))
       (world-tests--persisted-conversation local "bgood1" 1000)
       (world-server--reconcile configuration)
       (with-open-file (stream (conversation-log-pathname broken)
                               :direction :output :if-exists :supersede)
         (write-line "(:not-a-conversation-header)" stream))
       (let ((dates '(("bbroken1" . 5000) ("bgood1" . 1000))))
         (test-call-with-function-replacements
          (list (list 'conversation-storage-write-date
                      (lambda (pathname)
                        (or (rest (assoc (pathname-name pathname) dates :test #'string=)) 0))))
          (lambda ()
            (multiple-value-bind (host started-p skipped)
                (world-server--host-for server (list :directory workspace))
              (test-assert (and started-p (string= (world-host-channel host) "bgood1"))
                           "the damaged newest channel is passed over for the next one")
              (test-assert (equal (mapcar #'first skipped) '("bbroken1"))
                           "the open names the channel it passed over"))))))))
  nil)

(-> test-world-server-open-images-and-new () null)
(defun test-world-server-open-images-and-new ()
  "Test an open's images become the channel's composer draft, new or running, :new-p opens a channel of its own, and a running channel refuses MCP servers."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry     (world-client-entry configuration))
            (workspace (world-tests--workspace root "a"))
            (first     (test-conversation--write-tiny-png (merge-pathnames "images/first.png" root)))
            (second    (test-conversation--write-tiny-png (merge-pathnames "images/second.png" root))))
       (flet ((draft (identifier)
                "Return the image pathnames in channel IDENTIFIER's composer draft."
                (mapcar (lambda (attachment)
                          (namestring (rest attachment)))
                        (terminal-ui-image-attachments
                         (application-ui (world-host-application
                                          (world-server--find-host server identifier)))))))
         (let ((opened (getf (rest (world-server-tests--open entry workspace
                                                             :images (list (namestring first))))
                             :session-id)))
           (test-assert (equal (draft opened) (list (namestring (truename first))))
                        "a new channel opens with the images as its draft")
           (test-assert (equal opened
                               (getf (rest (world-server-tests--open entry workspace
                                                                     :images (list (namestring second))))
                                     :session-id))
                        "a second open of the workspace joins the running channel")
           (test-assert (equal (draft opened) (list (namestring (truename second))))
                        "the running channel's draft takes the new images")
           (let ((fresh (getf (rest (world-server-tests--open entry workspace :new-p t)) :session-id)))
             (test-assert (and (stringp fresh) (not (equal fresh opened)))
                          "an open asking for a new channel gets one of its own"))
           (test-assert (handler-case
                            (progn (world-server-tests--open entry workspace
                                                             :channel     opened
                                                             :mcp-servers '((:object "name" "editor")))
                                   nil)
                          (localgroup-error (condition)
                            (search "cannot take the editor's MCP servers"
                                    (autolith-error-message condition))))
                        "a running channel refuses an editor's MCP servers its tools would need"))))))
  nil)

(-> test-world-client-script-submissions () null)
(defun test-world-client-script-submissions ()
  "Test piped input splits into submissions: a line each, a Lisp form until it closes, blank lines skipped."
  (loop for (input expected) in
        '(("/status" ("/status"))
          ("one~%~%two~%" ("one" "two"))
          ("(+ 1~%   2)~%next" ("(+ 1
   2)" "next"))
          ("(print \")\")~%after" ("(print \")\")" "after"))
          ("(list ; a comment )~%  3)" ("(list ; a comment )
  3)"))
          ("(list #| ) #| nested |# |#~%  3)~%next"
           ("(list #| ) #| nested |# |#
  3)" "next"))
          ("(list |)|~%  3)~%next" ("(list |)|
  3)" "next"))
          ("two forms (on one line)~%next" ("two forms (on one line)" "next"))
          ("(one) (two)~%next" ("(one) (two)" "next"))
          (")~%next" (")" "next"))
          ("#.(error \"Read-time evaluation\")~%next"
           ("#.(error \"Read-time evaluation\")" "next"))
          ("(unfinished~%" ("(unfinished")))
        do (let ((stream (make-string-input-stream (format nil input)))
                 (found  nil))
             (loop for submission = (world-client--next-submission stream)
                   while submission
                   do (push submission found))
             (test-assert (equal (nreverse found) expected)
                          (format nil "~S splits into ~S" input expected))))
  nil)

(-> test-world-client-script () null)
(defun test-world-client-script ()
  "Test piped input runs in a new channel of the world server, one submission after another, then closes it."
  (world-server-tests--call
   (lambda (server configuration root)
     (let ((output (make-string-output-stream)))
       (world-client-run-script
        (configuration-copy configuration :working-directory (world-tests--workspace root "a"))
        :input  (make-string-input-stream (format nil "(status)~%~%(status)~%"))
        :output output)
       (let ((text (get-output-stream-string output)))
          (test-assert (and (= 2 (terminal-tests--substring-count "› (status)" text))
                           (= 2 (terminal-tests--substring-count "conversation" text)))
                      (format nil "each submission is echoed and followed by its output: ~A" text)))
       (test-assert (task-tests--wait-until
                     (lambda ()
                       (null (with-lock-held ((world-server-lock server))
                               (copy-list (world-server-hosts server)))))
                     10)
                    "the channel started for the script closes after it"))))
  nil)

(-> world-server--channels-of (cons) list)
(defun world-server--channels-of (entry)
  "Return the channel identifiers the world ENTRY reports."
  (getf (rest (getf (rest (localgroup-query-record entry ':status)) :status)) :channels))

(-> localgroup--find-record-or-nil (configuration string) (option cons))
(defun localgroup--find-record-or-nil (configuration identifier)
  "Return the endpoint entry of IDENTIFIER, or NIL when none is published."
  (handler-case
      (localgroup--find-record configuration identifier)
    (localgroup-error ()
      nil)))

(-> world-server-tests--session-lock-order () null)
(defun world-server-tests--session-lock-order ()
  "Exercise admission from a controller callback without acquiring its mutex again."
  (let* ((application (make-instance 'application))
         (controller  (make-instance 'application-input-controller :application application))
         (session     (make-instance 'localgroup-session :application application))
         (failure     (make-condition 'simple-error :format-control "Session test failure.")))
    (setf (application-input-controller application) controller)
    (with-lock-held ((application-input-controller-lock controller))
      (with-lock-held ((image-daemon:daemon-runtime-lock session))
        (dolist (state '(:healthy :stopping :closing :failed-runtime :failed-controller
                         :stopped-controller))
          (setf (image-daemon:daemon-runtime-stopping-p session) (eq state ':stopping)
                (localgroup-session-closing-p session) (eq state ':closing)
                (image-daemon:daemon-runtime-failure session)
                (when (eq state ':failed-runtime) failure)
                (application-input-controller-failure controller)
                (when (eq state ':failed-controller) failure)
                (application-input-controller-stopping-p controller)
                (eq state ':stopped-controller))
          (test-assert (eq (localgroup-session-admits-attachment-p session)
                           (eq state ':healthy))
                       (format nil "admission observes ~S without reversing controller/runtime lock order"
                               state))))))
  nil)

(-> test-world-server-checkpoint-quiescence () null)
(defun test-world-server-checkpoint-quiescence ()
  "Test a checkpoint waits for the work under way, pauses every channel until only the server's thread lives, then serves them again."
  (world-server-tests--session-lock-order)
  (let ((baseline (sb-thread:list-all-threads)))
    (world-server-tests--call
     (lambda (server configuration root)
       (let* ((entry       (world-client-entry configuration))
              (workspace-a (world-tests--workspace root "a"))
              (workspace-b (world-tests--workspace root "b"))
              (id-a        (getf (rest (world-server-tests--open entry workspace-a)) :session-id))
              (id-b        (getf (rest (world-server-tests--open entry workspace-b)) :session-id))
              (host-a      (world-server--find-host server id-a))
              (host-b      (world-server--find-host server id-b))
              (application (world-host-application host-a))
              (other       (world-host-application host-b))
              (session-a   (world-server-tests--session server id-a))
              (observed    nil))
         (multiple-value-bind (connection seen) (test-localgroup--present session-a)
           (unwind-protect
                (progn
                  ;; A job run in B leaves its pool's reusable workers waiting.
                  (test-assert (eql 42 (world-server-tests--run-job other))
                               "a job runs in the other channel before the checkpoint")
                  (setf observed
                        (world-server-call-quiesced
                         server application
                         (lambda ()
                           (funcall *checkpoint-thread-quiescer*
                                    (lambda ()
                                      ;; The test's own terminal client reads until the pause closes its connection.
                                      (task-tests--wait-until
                                       (lambda ()
                                         (not (thread-alive-p
                                               (localgroup-client-connection-reader-thread connection))))
                                       10)
                                      (list (eq *active-application* application)
                                            (remove-if-not #'thread-alive-p
                                                           (set-difference (sb-thread:list-all-threads)
                                                                           baseline))
                                            (world-server-runtime server)
                                            (application-localgroup-session application)
                                            (world-host-paused-p host-a)
                                            (world-host-paused-p host-b)
                                            (sb-thread:thread-name (current-thread))))))))
                  (destructuring-bind (active-p threads runtime session paused-a paused-b name) observed
                    (test-assert (and active-p (null runtime) (null session) paused-a paused-b
                                      (string= name "Autolith world server test main"))
                                 "inside the fork boundary every channel is paused and the endpoints are stopped")
                    (test-assert (and (= (length threads) 1)
                                      (string= (sb-thread:thread-name (first threads))
                                               "Autolith world server test main"))
                                 (format nil "only the server's own thread outlives the pause: ~S"
                                         (mapcar #'sb-thread:thread-name threads))))
                  (test-assert (find ':checkpoint-reconnect (funcall seen) :key #'first)
                               "an attached terminal is told to reconnect"))
             (localgroup-client-close connection)))
         (test-assert (task-tests--wait-until
                       (lambda () (and (world-host-live-p host-a) (world-host-live-p host-b))) 30)
                      "both channels are served again")
         (test-assert (and (null *active-application*)
                           (world-server-runtime server)
                           (not (world-server-checkpointing-p server))
                           (world-client-entry configuration))
                      "the active application is cleared and the endpoint answers again")
         (let ((restarted-a (world-server-tests--session server id-a)))
           (test-assert (and (not (eq restarted-a session-a))
                             (string= (image-daemon:daemon-runtime-token restarted-a)
                                      (image-daemon:daemon-runtime-token session-a))
                             (localgroup-session-linger-p restarted-a))
                        "every endpoint restarts under its token and keeps lingering"))
         (test-assert (eql 42 (world-server-tests--run-job other))
                      "the other channel's jobs run again after the checkpoint")
         (let* ((controller (application-input-controller other))
                (outcome    nil)
                (thread     nil))
           (application-input-controller-begin-external-work controller)
           (unwind-protect
                (progn
                  (setf thread (make-thread
                                (lambda ()
                                  (setf outcome
                                        (world-server-call-quiesced
                                         server application
                                         (lambda ()
                                           (funcall *checkpoint-thread-quiescer* (lambda () ':saved))))))
                                :name "Autolith world server test checkpoint"))
                  (test-assert (task-tests--wait-until
                                (lambda () (application-world-held-p (world-host-application host-a)))
                                10)
                               "a checkpoint holds every channel's new turns while it waits")
                  (sleep 0.5)
                  (test-assert (and (null outcome)
                                    (world-host-live-p host-b)
                                    (not (world-server-checkpointing-p server)))
                               "a checkpoint waits for a working channel instead of refusing"))
             (application-input-controller-finish-external-work controller))
           (test-assert (and (eq (sb-thread:join-thread thread :default ':timeout :timeout 30) ':saved)
                             (eq outcome ':saved))
                        "the waiting checkpoint saves once the channel's work is done")
           (test-assert (and (task-tests--wait-until
                              (lambda () (and (world-host-live-p host-a) (world-host-live-p host-b)))
                              30)
                             (not (application-world-held-p (world-host-application host-a))))
                        "the channels serve again and their held turns may start"))
         (setf (world-server-checkpointing-p server) t)
         (test-assert (handler-case
                          (progn (world-server-tests--open (world-client-entry configuration)
                                                           workspace-a)
                                 nil)
                        (localgroup-error (condition)
                          (localgroup-error-retry-p condition)))
                      "an open during a checkpoint is told to come back")
         (setf (world-server-checkpointing-p server) nil)
         (test-call-with-function-replacements
           (list (list 'platform-supports-p
                       (let ((supports (symbol-function 'platform-supports-p)))
                         (lambda (platform capability)
                           ;; This synthetic fork backend never saves an image.
                           (and (not (eq capability ':restartable-image-saver))
                                (funcall supports platform capability)))))
                 (list 'checkpoint-backend-create
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         ':test-backend))
                (list 'checkpoint-create
                      (lambda (backend)
                        (declare (ignore backend))
                        (funcall *checkpoint-thread-quiescer* (lambda () nil))
                        (error 'checkpoint-error
                               :message  "synthetic fork failure"
                               :stage    ':fork
                               :pathname nil))))
          (lambda ()
            (application-checkpoint application)
            (test-assert (search "checkpoint scheduled" (test-localgroup--presented-text application))
                         "the channel is told its checkpoint is scheduled")
            (test-assert (task-tests--wait-until
                          (lambda ()
                            (and (search "! checkpoint failed: synthetic fork failure"
                                         (test-localgroup--presented-text application))
                                 (search "! world server checkpoint failed"
                                         (test-localgroup--presented-text other))))
                          30)
                         "each channel learns the outcome once it is served again")
            (test-assert (task-tests--wait-until
                          (lambda () (and (world-host-live-p host-a) (world-host-live-p host-b)))
                          30)
                        "both channels serve again after a failed checkpoint")
           (dolist (hosted (list application other))
             (let ((text (test-localgroup--presented-text hosted)))
               (test-assert
                (= 1 (loop for start = 0 then (+ position (length "failed: synthetic fork failure"))
                           for position = (search "failed: synthetic fork failure" text :start2 start)
                           while position count position))
                "each resumed channel presents the checkpoint failure exactly once")))))))))
  nil)

(-> world-server-tests--run-job (application) t)
(defun world-server-tests--run-job (application)
  "Run one job in APPLICATION's tool job pool, wait for it and return its result."
  (let* ((orchestrator (application-task-presentation-orchestrator application))
         (job          (first (job-pool-submit-batch
                               (task-orchestrator-execution-pool orchestrator)
                               (list (list :name     "probe"
                                           :function (lambda (job)
                                                       (declare (ignore job))
                                                       42)))))))
    (job-await job)
    (job-result job)))

(-> test-world-server-close-unattached () null)
(defun test-world-server-close-unattached ()
  "Test a close meant for an idle channel spares one a terminal controls."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry      (world-client-entry configuration))
            (id-a       (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                              :session-id))
            (id-b       (getf (rest (world-server-tests--open entry (world-tests--workspace root "b")))
                              :session-id))
            (host-a     (world-server--find-host server id-a))
            (host-b     (world-server--find-host server id-b))
            (session-a  (world-server-tests--session server id-a))
            (session-b  (world-server-tests--session server id-b))
            (connection (test-localgroup--present session-a)))
       (unwind-protect
            (let ((answer (localgroup-query-record entry ':close
                                                   (list :channel id-a :unattached-only-p t))))
              (test-assert (and (null (getf (rest answer) :closed-p))
                                (world-host-live-p host-a)
                                (localgroup-session-admits-attachment-p session-a)
                                (not (localgroup-client-connection-closed-p connection)))
                           "a channel another terminal controls survives a close meant for an idle one"))
         (localgroup-client-close connection))
       (let ((answer (localgroup-query-record entry ':close
                                              (list :channel id-b :unattached-only-p t))))
         (test-assert (and (getf (rest answer) :closed-p)
                           (not (localgroup-session-admits-attachment-p session-b)))
                      "a channel nobody controls is committed to close and admits no terminal from then on")
         (test-assert (task-tests--wait-until (lambda () (not (world-host-live-p host-b))) 30)
                      "the committed close ends the channel")))))
  nil)

(-> test-world-server-closing-reservation () null)
(defun test-world-server-closing-reservation ()
  "Retain routing and lease ownership through blocked cleanup, then reopen exactly once."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (workspace   (world-tests--workspace root "closing-owned"))
            (identifier  (getf (rest (world-server-tests--open entry workspace)) :session-id))
            (host        (world-server--find-host server identifier))
            (application (world-host-application host))
            (lease       (application-conversation-lease application))
            (releasing   (sb-thread:make-semaphore))
            (release     (sb-thread:make-semaphore))
            (original    (symbol-function 'conversation-lease-release))
            (build       (symbol-function 'application--build))
            (builds      0))
       (conversation-append-user-message (application-conversation application) "persist before close")
       (test-call-with-function-replacements
        (list
         (list 'conversation-lease-release
               (lambda (candidate)
                 (when (eq candidate lease)
                   (sb-thread:signal-semaphore releasing)
                   (sb-thread:wait-on-semaphore release))
                 (funcall original candidate)))
         (list 'application--build (lambda (&rest arguments) (incf builds) (apply build arguments))))
        (lambda ()
          (unwind-protect
               (progn
                 (localgroup-query-record entry ':close (list :channel identifier))
                 (test-assert (sb-thread:wait-on-semaphore releasing :timeout 20)
                              "cleanup reaches its deterministic lease-release latch")
                 (test-assert (and (eq host (world-server--find-host server identifier))
                                   (eq (world-host-state host) ':closing)
                                   (eq lease (application-conversation-lease application))
                                   (conversation-lease-held-p lease))
                              "closing routing and the actual lease handle retain the same owner")
                 (dolist (arguments (list (list :channel identifier) (list :directory workspace)))
                   (test-assert
                    (handler-case (progn (world-server-open server arguments) nil)
                      (world-error (condition)
                        (and (eq (world-error-operation condition) ':closing)
                             (equal identifier (world-error-identifier condition)))))
                    "named and workspace opens accurately report closing without choosing another conversation"))
                 (test-assert (and (zerop builds) (world-server-status server)
                                   (not (sb-thread:holding-mutex-p (world-server-open-lock server))))
                              "blocked cleanup starts no competing application and leaves global admission available"))
            (sb-thread:signal-semaphore release))
          (join-thread (world-host-thread host))
          (test-assert (and (eq (world-host-state host) ':retired)
                            (null (world-server--find-host server identifier))
                            (not (conversation-lease-held-p lease)))
                       "host retirement follows real lease release")
          (let* ((opened  (world-server-open server (list :channel identifier)))
                 (resumed (world-server-open server (list :directory workspace))))
            (test-assert (and (= builds 1) (getf (rest opened) :started-p)
                              (equal identifier (getf (rest opened) :session-id))
                              (equal identifier (getf (rest resumed) :session-id))
                              (not (getf (rest resumed) :started-p)))
                         "retirement permits exactly one usable replacement shared with workspace resume")))))))
  nil)

(-> test-world-server-close-without-search-rebuild () null)
(defun test-world-server-close-without-search-rebuild ()
  "Release a stale-search conversation without archive work and retain its lease after a release error."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry        (world-client-entry configuration))
            (identifier   (getf (rest (world-server-tests--open entry (world-tests--workspace root "close-search"))) :session-id))
            (host         (world-server--find-host server identifier))
            (application  (world-host-application host))
            (conversation (application-conversation application))
            (lease        (application-conversation-lease application))
            (original     (symbol-function 'conversation-lease-release))
            (find-index   (symbol-function 'conversation-picker-search-find))
            (map-records  (symbol-function 'conversation--map-storage-records))
            (queries 0) (scans 0))
       (dotimes (index 128)
         (conversation-append-user-message conversation (format nil "~D ~A" index (make-string 1024 :initial-element #\x))))
       (conversation-picker-search-find (conversation-pathname conversation))
       (conversation-append-user-message conversation "invalidate the prewarmed search sidecar")
       (test-call-with-function-replacements
        (list (list 'conversation-lease-release
                    (lambda (candidate)
                      (if (eq candidate lease)
                          (error 'world-error :operation ':close :message "Injected lease-release failure.")
                          (funcall original candidate)))))
        (lambda ()
          (test-assert (handler-case (progn (application-release-conversation-lease application) nil)
                         (world-error () t))
                       "the real release error is observable")
          (test-assert (and (eq lease (application-conversation-lease application))
                            (conversation-lease-held-p lease))
                       "a failed release retains the actual owned handle for retry")))
       (test-call-with-function-replacements
        (list (list 'conversation-picker-search-find
                    (lambda (pathname) (incf queries) (funcall find-index pathname)))
              (list 'conversation--map-storage-records
                    (lambda (&rest arguments) (incf scans) (apply map-records arguments))))
        (lambda ()
          (let ((started (get-internal-real-time)))
            (world-server-close server identifier)
            (join-thread (world-host-thread host))
            (test-assert (and (zerop queries) (zerop scans)
                              (not (conversation-lease-held-p lease))
                              (eq (world-host-state host) ':retired)
                              (>= (- (get-internal-real-time) started) 0))
                         "closing a large stale cache performs zero search queries or archive scans before releasing ownership")))))))
  nil)

(-> test-world-server-close-ownership () null)
(defun test-world-server-close-ownership ()
  "Test a closing host retains exclusive ownership until its serving thread retires."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (identifier
             (getf
              (rest
               (world-server-tests--open entry
                (world-tests--workspace root "closing")))
              :session-id))
            (host        (world-server--find-host server identifier))
            (application (world-host-application host))
            (lease       (application-conversation-lease application))
            (releasing   (sb-thread:make-semaphore))
            (release     (sb-thread:make-semaphore))
            (original    (symbol-function 'conversation-lease-release)))
       (conversation-append-user-message (application-conversation application)
                                         "persist before close")
       (test-call-with-function-replacements
        (list
         (list 'conversation-lease-release
               (lambda (candidate)
                 (when (eq candidate lease)
                   (sb-thread:signal-semaphore releasing)
                   (sb-thread:wait-on-semaphore release))
                 (funcall original candidate))))
        (lambda ()
          (unwind-protect
              (progn
               (localgroup-query-record entry ':close
                                        (list :channel identifier))
               (test-assert (sb-thread:wait-on-semaphore releasing :timeout 20)
                "the closing host reaches ownership release")
               (test-assert
                (and (eq host (world-server--find-host server identifier))
                     (thread-alive-p (world-host-thread host))
                     (member host (world-server--hosts server)))
                "a channel leaves routing before its host retires")
               (test-assert
                (and (conversation-lease-held-p lease)
                     (conversation-lease-busy-p configuration identifier))
                "a closing host keeps its lease until release completes")
               (test-assert
                (handler-case
                 (let ((unexpected
                        (conversation-lease-acquire configuration identifier)))
                   (conversation-lease-release unexpected)
                   nil)
                 (conversation-in-use (condition)
                  (eql (conversation-in-use-owner-pid condition)
                       (current-process-id))))
                "admission identifies the closing host's process as the owner"))
            (sb-thread:signal-semaphore release))
          (sb-thread:join-thread (world-host-thread host) :default nil :timeout
                                 20)
          (test-assert
           (and (not (thread-alive-p (world-host-thread host)))
                (not (conversation-lease-held-p lease))
                (not (member host (world-server--hosts server)))
                (not (conversation-lease-busy-p configuration identifier)))
           "retiring the serving thread releases ownership and removes its host")
          (let ((opened
                 (localgroup-query-record entry ':open
                                          (list :channel identifier))))
            (test-assert
             (and (getf (rest opened) :started-p)
                  (string= identifier (getf (rest opened) :session-id)))
             "the retired channel can be opened under a new owner")))))))
  nil)

(-> test-world-server-attachment-moves-nobody () null)
(defun test-world-server-attachment-moves-nobody ()
  "Test a terminal taking control of a hosted channel maps its channel and leaves the person in the inbox."
  (world-server-tests--call
   (lambda (server configuration root)
     (world-tests--persisted-conversation configuration "bhome11" 1000)
     (world-reconcile-sessions configuration :now 2000)
     (let* ((entry  (world-client-entry configuration))
            (id     (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                          :session-id))
            (hosted (world-host-application (world-server--find-host server id)))
            (inbox  (world-tests--inbox configuration)))
       (flet ((residence ()
                "Return the channel the person lives in."
                (participant-residence (world-human-participant (world-load configuration)))))
         (test-assert (string= (residence) inbox)
                      "opening a channel in the server moves nobody")
         (conversation-append-user-message (application-conversation hosted) "first words")
         (application-world-attached hosted)
         (test-assert (and (world-channel-find (world-load configuration) id)
                           (string= (residence) inbox))
                      "a control attachment maps the channel and the person stays in the inbox")))))
  nil)

(-> test-world-server-person-unread () null)
(defun test-world-server-person-unread ()
  "Test a message for the person shows its unread count on the modeline of every served channel."
  (world-server-tests--call
   (lambda (server configuration root)
     (world-tests--persisted-conversation configuration "bfirst1" 1000)
     (world-tests--persisted-conversation configuration "bsecnd2" 2000)
     (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
       (declare (ignore first-agent))
       (let* ((entry  (world-client-entry configuration))
              (id     (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                            :session-id))
              (hosted (world-host-application (world-server--find-host server id))))
         (flet ((modeline ()
                  "Return the hosted channel's idle modeline text."
                  (format nil "~{~A~}"
                          (mapcar #'terminal-span-text
                                  (terminal-ui-idle-status-details (application-ui hosted))))))
           (test-assert (not (search "unread" (modeline)))
                        "a channel shows no unread marker while the inbox is empty")
           (world-message-send configuration
                               :sender  (participant-identifier second-agent)
                               :channel "bsecnd2"
                               :text    (format nil "@~A the build is green" (participant-name human)))
           (test-assert (task-tests--wait-until (lambda () (search "1 unread" (modeline))) 10)
                        "a message for the person shows as unread on the modeline of a served channel")
           (world-inbox-settle configuration (world-tests--inbox configuration)
                               (mapcar #'world-delivery-identifier
                                       (world-inbox-deliveries configuration
                                                               (world-tests--inbox configuration))))
           (localgroup-query-record entry ':person-unread)
           (test-assert (not (search "unread" (modeline)))
                        "the marker leaves once nothing waits in the inbox"))))))
  nil)

(-> test-world-server-open-before-checkpoint () null)
(defun test-world-server-open-before-checkpoint ()
  "Test a checkpoint preserves an admitted open through host readiness and its flushed response."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry               (world-client-entry configuration))
            (id-a                (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                                       :session-id))
            (host-a              (world-server--find-host server id-a))
            (started             (sb-thread:make-semaphore))
            (release             (sb-thread:make-semaphore))
            (response-ready      (sb-thread:make-semaphore))
            (send-response       (sb-thread:make-semaphore))
            (start               (symbol-function 'world-server--start-host))
            (dispatch            (symbol-function 'world-server--dispatch))
            (write-packet        (symbol-function 'daemon-write-packet))
            (pause               (symbol-function 'world-server--pause-channels))
            (pending             nil)
            (sent-p              nil)
            (sent-before-pause-p nil)
            (opened              nil)
            (observed            nil))
       (test-call-with-function-replacements
        (list (list 'world-server--start-host
                    (lambda (candidate &rest arguments)
                      (sb-thread:signal-semaphore started)
                      (sb-thread:wait-on-semaphore release :timeout 30)
                      (apply start candidate arguments)))
              (list 'world-server--dispatch
                    (lambda (candidate operation arguments)
                      (let ((response (funcall dispatch candidate operation arguments)))
                        (when (eq operation ':open)
                          (setf pending response))
                        response)))
              (list 'daemon-write-packet
                    (lambda (stream packet)
                      (if (eq packet pending)
                          (progn
                            (sb-thread:signal-semaphore response-ready)
                            (sb-thread:wait-on-semaphore send-response :timeout 30)
                            (prog1 (funcall write-packet stream packet)
                              (setf sent-p t)))
                          (funcall write-packet stream packet))))
              (list 'world-server--pause-channels
                    (lambda (candidate application reconnect-id)
                      (setf sent-before-pause-p sent-p)
                      (funcall pause candidate application reconnect-id))))
        (lambda ()
          (let ((opener     nil)
                (checkpoint nil))
            (unwind-protect
                 (progn
                   (setf opener
                         (make-thread
                          (lambda ()
                            (setf opened
                                  (handler-case
                                      (world-server-tests--open entry (world-tests--workspace root "b"))
                                    (serious-condition (condition) condition))))
                          :name "Autolith test opener"))
                   (test-assert (sb-thread:wait-on-semaphore started :timeout 30)
                                "the open is admitted and starts its host")
                   (setf checkpoint
                         (make-thread
                          (lambda ()
                            (setf observed
                                  (handler-case
                                      (world-server-call-quiesced
                                       server (world-host-application host-a)
                                       (lambda ()
                                         (funcall *checkpoint-thread-quiescer*
                                                  (lambda ()
                                                    (mapcar #'world-host-paused-p
                                                            (world-server--hosts server))))))
                                    (serious-condition (condition) condition))))
                          :name "Autolith test checkpoint"))
                   (test-assert (task-tests--wait-until
                                 (lambda () (world-server-holding-p server)) 30)
                                "the checkpoint starts draining while the admitted host starts")
                   (test-assert (and (thread-alive-p checkpoint)
                                     (not (world-server-checkpointing-p server)))
                                "a checkpoint waits for the admitted open instead of declaring itself")
                   (sb-thread:signal-semaphore release)
                   (test-assert (sb-thread:wait-on-semaphore response-ready :timeout 30)
                                "the ready host's open response reaches publication")
                   (test-assert (not (world-server-checkpointing-p server))
                                "the checkpoint also waits for the admitted open's response")
                   (sb-thread:signal-semaphore send-response)
                   (join-thread opener)
                   (join-thread checkpoint)
                   (test-assert (and (listp opened) (eq (first opened) ':ok)
                                     (listp observed) (= (length observed) 2)
                                     (every #'identity observed)
                                     sent-before-pause-p)
                                (format nil "the open answers before both hosts pause: open ~S, checkpoint ~S"
                                        opened observed)))
              (sb-thread:signal-semaphore release)
              (sb-thread:signal-semaphore send-response)
              (when opener (join-thread opener))
              (when checkpoint (join-thread checkpoint))))))
       (test-assert (task-tests--wait-until
                     (lambda ()
                       (every #'world-host-live-p (world-server--hosts server)))
                     30)
                    "both channels are served again after the checkpoint"))))
  nil)

(-> test-world-client-joins-winner () null)
(defun test-world-client-joins-winner ()
  "Test a client whose own server launch lost the world's lease joins the server that won it."
  (with-test-configuration (configuration)
    (let ((polls   0)
          (winner  (cons "world" (list :session-id "world")))
          (running nil)
          (events  nil))
      (test-call-with-function-replacements
       (list (list 'world-client-entry
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (incf polls)
                     (and (>= polls 3) winner)))
             (list 'world-server-running-p
                   (lambda (candidate)
                     (declare (ignore candidate))
                     running))
             (list 'world-client-start-server
                   (lambda (candidate &rest arguments)
                     (declare (ignore candidate arguments))
                     (push ':spawn events)
                     (setf running t)
                     (error 'localgroup-error
                            :message   "The detached world server did not start within 1 seconds."
                            :operation ':spawn))))
       (lambda ()
         (handler-bind ((channel-loading-progress
                          (lambda (progress)
                            (push (channel-loading-progress-step progress) events))))
           (test-assert (eq (world-client--entry configuration
                                                 :deadline (+ (get-internal-real-time)
                                                              (* 10 internal-time-units-per-second)))
                            winner)
                        "a lost launch race ends at the winning server's endpoint, not in an error")
           (test-assert (equal (reverse events) '(1 :spawn))
                        "cold startup reports preparation before waiting for the world process")
           (setf polls 0 running nil events nil)
           (test-assert (handler-case
                            (progn
                              (test-call-with-function-replacements
                               (list (list 'world-server-running-p
                                           (lambda (candidate)
                                             (declare (ignore candidate))
                                             nil)))
                               (lambda ()
                                 (world-client--entry configuration
                                                      :deadline (+ (get-internal-real-time)
                                                                   internal-time-units-per-second))))
                              nil)
                          (localgroup-error () t))
                        "a launch that fails while nobody holds the world still fails")
           (test-assert (equal (reverse events) '(1 :spawn))
                        "a failed cold launch reports preparation before spawning"))))))
  nil)

(-> test-world-client-switch-navigates () null)
(defun test-world-client-switch-navigates ()
  "Test switch refusals preserve the current channel and final reporting follows terminal cleanup."
  (with-test-configuration (configuration)
   (let ((opens           nil)
         (attaches        nil)
         (client-versions nil)
         (refuse-p        nil)
         (rejects-p       nil)
         (quit-p          nil)
         (notices         nil)
         (loading         nil)
         (reports         nil)
         (lifecycle       nil)
         (release         (symbol-function 'localgroup-client-screen-release)))
     (flet ((run (expected-channel)
              "Run one switch and check its sole final report names EXPECTED-CHANNEL."
              (setf opens nil
                    attaches nil
                    loading nil
                    client-versions nil
                    notices nil
                    reports nil
                    lifecycle nil)
              (let ((calls 0))
                (multiple-value-prog1
                    (test-call-with-function-replacements
                     (list
                      (list 'main--open-world-channel
                            (lambda (candidate &rest arguments)
                              (declare (ignore candidate arguments))
                              (values "bfirst1" nil)))
                      (list 'channel-view-run
                            (lambda (view &key attached-function notice client-version boot-p)
                              (declare (ignore boot-p))
                              (let ((identifier (channel-subscription-identifier view)))
                                (push identifier attaches)
                                (push client-version client-versions)
                                (when notice (push notice notices))
                                (cond
                                  ((= (incf calls) 1)
                                   (when attached-function
                                     (funcall attached-function))
                                   (list ':switch :session-id "bsecnd2"))
                                  ((and rejects-p (equal identifier "bsecnd2"))
                                   (error 'localgroup-client-rejected
                                          :message   "Channel b-secnd2 is closing."
                                          :operation ':client))
                                  (t
                                   (when attached-function
                                     (funcall attached-function))
                                   ;; A channel that exited returns its fields; a detach returns nothing.
                                   (when quit-p
                                     (list :status 77 :message "Channel closed.")))))))
                      (list 'world-sidebar-start
                            (lambda (panel)
                              (declare (ignore panel))
                              (push ':start lifecycle)))
                      (list 'world-sidebar-stop
                            (lambda (panel)
                              (declare (ignore panel))
                              (push ':stop lifecycle)))
                      (list 'localgroup-client-screen-release
                            (lambda (screen)
                              (push ':release lifecycle)
                              (funcall release screen)))
                      (list 'localgroup-relay-exit
                            (lambda (candidate exit mode &key session-id)
                              (declare (ignore candidate))
                              (push (list session-id exit mode) reports)
                              (push ':report lifecycle)
                              (when quit-p
                                (throw 'client-exit (getf exit :status)))))
                      (list 'localgroup-screen-show-loading
                            (lambda (screen text &rest arguments)
                              (declare (ignore screen))
                              (push (list text (getf arguments :agent-name)) loading)))
                      (list 'world-client-open
                            (lambda (candidate &rest arguments)
                              (declare (ignore candidate))
                              (test-assert (equal (first loading) '("Loading channel" "Ada"))
                                           "Loading names the resident before the blocking channel open")
                              (channel-loading-report 4 "Initializing channel")
                              (test-assert (equal (second loading) (first loading))
                                           "The switch progress keeps its agent name")
                              (push arguments opens)
                              (if refuse-p
                                  (error 'localgroup-error :message
                                         "Channel b-secnd2 cannot take that."
                                         :operation ':open)
                                  (values (getf arguments :channel) nil)))))
                     (lambda ()
                       (catch 'client-exit
                         (let ((*error-output* (make-broadcast-stream)))
                           (main--run-world-client configuration
                                                   :permission-mode
                                                   ':full-access :launch
                                                   '(:model "other"
                                                     :permission-mode
                                                     :full-access))))))
                  (test-assert
                   (equal reports
                          (list
                           (list expected-channel
                                 (when quit-p
                                   (list :status 77 :message
                                         "Channel closed."))
                                 ':control)))
                   "Only the final successful attachment is reported, with its exit and mode")
                  (let ((events (reverse lifecycle)))
                    (test-assert
                     (and (= 1 (count ':release events))
                          (= 1 (count ':stop events))
                          (= 1 (count ':report events))
                          (< (position ':release events)
                             (position ':report events))
                          (< (position ':stop events)
                             (position ':report events)))
                     "Screen release and sidebar shutdown each happen once before the final report"))))))
       (world-tests--persisted-conversation configuration "bsecnd2" 1000)
       (world-reconcile-sessions configuration)
       (world-participant-rename configuration "al-bsecnd2" "Ada")
       (run "bsecnd2")
       (test-assert
        (and (= (length opens) 1) (null (getf (first opens) :launch))
             (equal (getf (first opens) :channel) "bsecnd2")
             (getf (first opens) :switch-p))
        "A switch opens the other channel without this start's launch options")
       (test-assert (equal (reverse attaches) '("bfirst1" "bsecnd2"))
        "The terminal attaches to the channel it switched to")
       (test-assert
        (equal (reverse client-versions) (list *autolith-version* nil))
        "Only the initially loaded channel receives this client's version")
       (setf refuse-p t)
       (run "bfirst1")
       (test-assert
        (and (equal (reverse attaches) '("bfirst1" "bfirst1"))
             (search "could not switch to b-secnd2" (first notices)))
        "An open refusal returns the terminal to its current channel with a notice")
       (setf refuse-p nil
             rejects-p t)
       (run "bfirst1")
       (test-assert
        (and (equal (reverse attaches) '("bfirst1" "bsecnd2" "bfirst1"))
             (search "is closing" (first notices)))
        "An attachment refusal returns the terminal to its current channel with a notice")
       (setf quit-p t)
       (test-assert (eql (run "bfirst1") 77)
        "A nonlocal exit from the final report propagates after cleanup")
       (setf rejects-p nil)
       (test-assert (eql (run "bsecnd2") 77)
        "A switched channel's nonlocal final report follows the same single cleanup"))))
  nil)

(-> test-world-client-switch-failures () null)
(defun test-world-client-switch-failures ()
  "Exercise discovery and pre-acknowledgment failures, source loss, and guarded unattached cleanup."
  (with-test-configuration (configuration)
    (dolist (failure '(:discover :target-dies :source-dies :transport-error))
      (let ((attaches nil)
            (reported nil)
            (current  nil)
            (closed   nil)
            (left-p   nil)
            (release  (symbol-function 'localgroup-client-screen-release)))
        (test-call-with-function-replacements
         (list
          (list 'main--open-world-channel
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values "bfirst1" nil)))
          (list 'world-client-open
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values "bsecnd2" (eq failure ':source-dies))))
          (list 'world-client-entry
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  '("world")))
          (list 'localgroup-query-record
                (lambda (entry operation arguments)
                  (declare (ignore entry))
                  (push (list operation arguments) closed)
                  '(:ok)))
          (list 'world-sidebar-start (lambda (panel) (declare (ignore panel))))
          (list 'world-sidebar-stop
                (lambda (panel) (setf current (world-sidebar-current panel))))
          (list 'localgroup-client-screen-release
                (lambda (screen) (funcall release screen)))
          (list 'channel-view-run
                (lambda (view &key attached-function notice client-version boot-p)
                  (declare (ignore client-version boot-p))
                  (let ((identifier (channel-subscription-identifier view))
                        (panel      (localgroup-screen-panel (channel-view-screen view))))
                    (push identifier attaches)
                    ;; A view discovers its channel's endpoint before it connects.
                    (when (or (and left-p (string= identifier "bfirst1")
                                   (eq failure ':source-dies))
                              (and (string= identifier "bsecnd2")
                                   (member failure '(:discover :source-dies))))
                      (error 'localgroup-error :operation ':discover
                                              :message    "Endpoint disappeared."))
                    (when (string= identifier "bsecnd2")
                      (test-assert (string= (world-sidebar-current panel) "bfirst1")
                                   "the navigator commits no unacknowledged target")
                      (if (eq failure ':transport-error)
                          (error "Injected transport failure.")
                          (error 'localgroup-error :operation ':connect
                                                  :message    "Target died before acknowledgment.")))
                    (funcall attached-function)
                    (cond
                      (left-p
                       (test-assert notice "the restored source receives the switch failure")
                       nil)
                      (t
                       (setf left-p t)
                       (list ':switch :session-id "bsecnd2"))))))
          (list 'localgroup-relay-exit
                (lambda (candidate exit mode &key session-id)
                  (declare (ignore candidate mode))
                  (setf reported (list session-id exit)))))
         (lambda ()
           (let ((*error-output* (make-broadcast-stream)))
             (main--run-world-client configuration))))
        (if (eq failure ':source-dies)
            (test-assert
             (and (null current) (null (first reported))
                  (= (getf (second reported) :status) 1)
                  (search "Disconnected" (getf (second reported) :message))
                  (equal closed
                         '((:close (:channel "bsecnd2" :unattached-only-p t)))))
             "a vanished source clears navigation, reports disconnected, and cleans only the newly started unattached target")
            (test-assert
             (and (equal current "bfirst1") (equal reported '("bfirst1" nil))
                  (null closed) (= (count "bfirst1" attaches :test #'equal) 2))
             "every pre-acknowledgment failure restores the live source without closing an existing target"))))
    (let ((discoveries 0)
          (attempts    nil)
          (screen      (localgroup-screen-create
                        :terminal (make-instance 'recording-terminal :columns 80))))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'terminal--terminal-mode-or-nil
                   (lambda (terminal)
                     (declare (ignore terminal))
                     t))
             (list 'localgroup--find-record
                   (lambda (candidate identifier)
                     (declare (ignore candidate identifier))
                     (list (incf discoveries))))
             (list 'localgroup-client-connect
                   (lambda (entry &rest arguments)
                     (declare (ignore arguments))
                     (push entry attempts)
                     (error 'localgroup-error
                            :operation ':connect
                            :message   (if (= (first entry) 1)
                                           "Endpoint was replaced."
                                           "The replacement endpoint was reached.")))))
            (lambda ()
              ;; Neither endpoint accepts the client; only the attempts matter here.
              (handler-case
                  (main--attach-world-channel configuration "bsecnd2" ':control :screen screen)
                (localgroup-error ()
                  nil))))
        (localgroup-client-screen-release screen))
      (test-assert
       (and (= discoveries 2) (equal (reverse attempts) '((1) (2))))
       "one changed endpoint is discovered again and tried once before acknowledgment")))
  nil)

(-> test-conversation-maintenance-admission-probe () null)
(defun test-conversation-maintenance-admission-probe ()
  "Admit only persisted conversations with actual projection repair pending."
  (with-test-configuration (configuration)
    (dolist (case '((nil nil nil nil) (nil t t nil) (t nil nil nil) (t t nil t) (t nil t t)))
      (destructuring-bind (persisted-p input-pending-p image-pending-p expected-job-p) case
        (let* ((conversation (conversation-create configuration))
               (application  (make-instance 'application :configuration configuration :conversation conversation))
               (runs 0) (callbacks 0))
          (when persisted-p (conversation-append-user-message conversation "persisted admission fixture"))
          (setf (conversation-input-history-repair-pending-p conversation) input-pending-p
                (conversation-image-artifact-repair-pending-p conversation) image-pending-p)
          (unwind-protect
               (let ((job (application-conversation-maintenance-start
                           application
                           :run-function      (lambda (candidate &key check-function)
                                                (declare (ignore candidate)) (incf runs) (funcall check-function)
                                                (list :input-complete-p input-pending-p :images-complete-p image-pending-p))
                           :complete-function (lambda (&rest arguments) (declare (ignore arguments)) (incf callbacks)))))
                 (if expected-job-p
                     (test-assert (and job (eq (getf (job-await job :timeout-seconds 3) :state) ':completed)
                                       (= runs 1) (= callbacks (if input-pending-p 1 0)))
                                  "persisted dirty input or image projections receive one managed repair")
                     (test-assert (and (null job) (null (application-conversation-maintenance-pool application))
                                       (zerop runs) (zerop callbacks))
                                  "fresh and valid projections admit no job, pool, archive scan, or completion work")))
            (application-conversation-maintenance-stop application))))))
  nil)

(-> test-application-conversation-maintenance () null)
(defun test-application-conversation-maintenance ()
  "Exercise nonblocking admission, retained shutdown ownership, timeout, and checkpoint resume."
  (with-test-configuration (configuration)
   (dolist (scenario '(:close :complete :timeout))
     (let* ((conversation (conversation-create configuration))
            (lease
             (conversation-lease-acquire configuration
                                         (conversation-identifier
                                          conversation)))
            (application
             (make-instance 'application :configuration configuration
                            :conversation conversation :conversation-lease
                            lease))
            (ready        (sb-thread:make-semaphore))
            (gate         (sb-thread:make-semaphore))
            (completed    0)
            (*conversation-maintenance-runtime-milliseconds*
             (if (eq scenario ':timeout)
                 30
                 30000)))
       (conversation-append-user-message conversation "pending repair fixture")
       (setf (conversation-input-history-repair-pending-p conversation) t)
       (unwind-protect
           (let* ((job
                   (application-conversation-maintenance-start application
                    :run-function
                    (lambda (candidate &key check-function)
                      (test-assert (eq candidate conversation)
                       "maintenance owns the admitted conversation")
                      (sb-thread:signal-semaphore ready)
                      (sb-thread:wait-on-semaphore gate)
                      (funcall check-function)
                      '(:input-complete-p t :images-complete-p t))
                    :complete-function
                    (lambda (candidate admitted result)
                      (test-assert
                       (and (eq candidate application)
                            (eq admitted conversation)
                            (getf result :input-complete-p))
                       "completion carries the original owner and completeness")
                      (incf completed))))
                  (pool
                   (application-conversation-maintenance-pool application)))
             (test-assert (sb-thread:wait-on-semaphore ready :timeout 3)
              "the managed job starts without blocking its caller")
             (test-assert
              (and
               (eq job
                   (application-conversation-maintenance-start application))
               (member
                (getf (application-conversation-maintenance-status application)
                      :state)
                '(:running :aborted)))
              "repeated admission returns one queryable job while archive work is blocked")
             (case scenario
               (:close
                (let ((close (symbol-function 'job-pool-close)))
                  (test-call-with-function-replacements
                   (list
                    (list 'job-pool-close
                          (lambda (candidate)
                            (if (eq candidate pool)
                                nil
                                (funcall close candidate)))))
                   (lambda ()
                     (test-assert
                      (handler-case
                       (progn
                        (application-release-conversation-lease application)
                        nil)
                       (conversation-maintenance-stop-error nil t))
                      "unfinished managed shutdown refuses actual lease release")
                     (test-assert
                      (and
                       (eq pool
                           (application-conversation-maintenance-pool
                            application))
                       (eq lease (application-conversation-lease application))
                       (conversation-lease-held-p lease))
                      "a failed join retains the pool and actual owned lease for retry"))))
                (application-release-conversation-lease application)
                (test-assert
                 (and (eq (getf (job-snapshot job) :state) ':aborted)
                      (zerop completed)
                      (null
                       (application-conversation-maintenance-pool application))
                      (not (conversation-lease-held-p lease)))
                 "retry cancels and joins archive work before releasing ownership"))
               (:complete
                (sb-thread:signal-semaphore gate)
                (test-assert
                 (eq (getf (job-await job :timeout-seconds 3) :state)
                     ':completed)
                 "the managed job publishes its repair result")
                (application-conversation-maintenance-stop application)
                (test-assert
                 (and (= completed 1) (conversation-lease-held-p lease)
                      (null
                       (application-conversation-maintenance-start
                        application)))
                 "checkpoint pause stops its pool but retains the lease and prevents accidental restart")
                (let ((resumed
                       (application-conversation-maintenance-start application
                        :resume-p t :run-function
                        (lambda (candidate &key check-function)
                          (declare (ignore candidate))
                          (funcall check-function)
                          '(:input-complete-p nil :images-complete-p t))
                        :complete-function
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (incf completed)))))
                  (test-assert
                   (and (not (eq resumed job))
                        (eq
                         (getf (job-await resumed :timeout-seconds 3) :state)
                         ':completed)
                        (= completed 1))
                   "resumed readiness admits one fresh job only after closed-pool pause, without publishing incomplete input history")))
               (:timeout
                (let ((snapshot (job-await job :timeout-seconds 3)))
                  (test-assert
                   (and (eq (getf snapshot :state) ':aborted)
                        (eq (getf snapshot :cancellation-reason) ':timeout)
                        (zerop completed) (conversation-lease-held-p lease))
                   "bounded maintenance times out without releasing its conversation owner")))))
         (application-release-conversation-lease application)))))
  nil)

(-> test-world-client-refusal-ends-start () null)
(defun test-world-client-refusal-ends-start ()
  "Test a start ends with the reason when the world server refuses it, cannot be reached, or cannot be started."
  (with-test-configuration (configuration)
    (flet ((open-with (condition)
             "Open this terminal's channel while the client open signals CONDITION; return the message the start ends with."
             (test-call-with-function-replacements
              (list (list 'world-client-open
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (error condition))))
              (lambda ()
                (handler-case (progn (main--open-world-channel configuration) nil)
                  (localgroup-error (failure)
                    (autolith-error-message failure)))))))
      (test-assert (search "cannot take --fullscreen"
                           (open-with (make-condition 'localgroup-error
                                                      :message   "Channel b-first1 is already running and cannot take --fullscreen."
                                                      :operation ':open)))
                   "a refusal the server answered ends the start with its reason")
      (test-assert (search "The world server is not available: Localgroup session world is unavailable."
                           (open-with (make-condition 'localgroup-error
                                                      :message   "Localgroup session world is unavailable."
                                                      :operation ':open
                                                      :cause     (make-condition 'simple-error
                                                                                 :format-control "refused"))))
                   "a server that cannot be reached ends the start instead of running it in this process")
      (test-assert (search "did not start within 1 seconds"
                           (open-with (make-condition 'localgroup-error
                                                      :message   "The detached world server did not start within 1 seconds."
                                                      :operation ':spawn)))
                   "a server that cannot be started ends the start with the reason")))
  nil)

(-> test-world-server-stop () null)
(defun test-world-server-stop ()
  "Test stopping the server ending every channel and the main thread."
  (with-test-configuration (configuration root)
    (configuration-ensure-directories configuration)
    (test-call-with-function-replacements
     (world-server-tests--stubs)
     (lambda ()
       (let* ((server (world-server-create configuration :permission-mode ':auto))
              (main   (make-thread (lambda () (world-server-serve server))
                                   :name "Autolith world server stop test")))
         (unwind-protect
              (let* ((workspace (world-tests--workspace root "project"))
                     (entry     (world-client-entry configuration))
                     (opened    (world-server-tests--open entry workspace)))
                (test-assert (getf (rest opened) :session-id)
                             "a channel opens before the stop")
                (localgroup--print-response (localgroup-query-record entry ':stop)
                                            (make-string-output-stream))
                (test-assert (task-tests--wait-until
                              (lambda () (not (thread-alive-p main))) 30)
                             "a stop request ends every channel and the main thread")
                (test-assert (and (null (world-server--hosts server))
                                  (null (localgroup-endpoint-records configuration))
                                  (null (join-thread main)))
                             "the stopped server leaves no channel endpoints and no exit status")
                (test-assert (handler-case
                                 (progn
                                   (world-server-tests--open entry workspace)
                                   nil)
                               (localgroup-error (condition)
                                 (search "stopping" (autolith-error-message condition))))
                             "a stopping server opens no channel"))
           (image-daemon:daemon-stop-thread main)
           (world-server--shutdown server)
           (test-assert (null (world-client-entry configuration))
                        "shutdown unpublishes the world endpoint"))))))
  nil)

(-> test-world-server-move-running-child nil null)
(defun test-world-server-move-running-child ()
  "Test moving or parking a child agent interrupts the task its parent's pool runs it in, and leaves its own children's tasks alone."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry (world-client-entry configuration))
            (id-a
             (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                   :session-id))
            (app-a (world-host-application (world-server--find-host server id-a))))
       (conversation-append-user-message (application-conversation app-a) "first words in a")
       (let* ((world   (values (world-reconcile-sessions configuration)))
              (human   (world-human-participant world))
              (agent-a (find ':agent (world-channel-residents world id-a) :key #'participant-kind)))
         (world-tests--persisted-conversation configuration "bdest11" 2000)
         (world-channel-create configuration :id "bdest11" :owner (participant-identifier human)
          :workspace (world-tests--workspace root "b"))
         (labels ((running-child (name)
                    "Return a child agent NAME and the task that runs it, in a channel of its own."
                    (let* ((job
                            (task-tests--register-job (application--task-orchestrator app-a)
                             (application-agent app-a) (acp-extension-job-tests--definition) :name
                             name))
                           (task (task-job-execution-identifier job)))
                      (setf (task-job-participant-state job)
                            (make-instance 'participant-state :conversation
                                           (conversation-create configuration :identifier task)
                                           :goal
                                           '(:objective "child ordinary goal" :status :active
                                             :continuations 0 :created-at 1)))
                      (conversation-append-user-message
                       (participant-state-conversation (task-job-participant-state job))
                       "child context follows its participant")
                      (world-channel-create configuration :id task :owner
                       (participant-identifier agent-a) :workspace
                       (world-tests--workspace root "a") :parent id-a)
                      (values
                       (world-participant-create configuration :kind ':agent :name name :residence
                        task :parent (participant-identifier agent-a))
                       job
                       (agent-create :class 'task-child-agent :configuration configuration
                                     :provider
                                     (make-instance 'task-test-provider :mode ':concurrent)
                                     :participant-state (task-job-participant-state job)
                                     :conversation
                                     (participant-state-conversation
                                      (task-job-participant-state job))
                                     :tool-registry     (agent-tool-registry (application-agent app-a))
                                     :worker nil :initargs
                                     (list :definition (task-job-definition job) :identity
                                           (task-job-identity job) :depth 1 :completion
                                           (make-instance 'task-completion) :orchestrator
                                           (application--task-orchestrator app-a) :job job)))))

                  (refusal (operation arguments)
                    "Return the message refusing OPERATION with ARGUMENTS, or NIL when it succeeds."
                    (handler-case (progn (localgroup-query-record entry operation arguments) nil)
                                  (localgroup-error (condition)
                                   (autolith-error-message condition))))

                  (residence (participant)
                    "Return where PARTICIPANT resides now."
                    (participant-residence
                     (world-participant-find (world-load configuration)
                      (participant-identifier participant))))

                  (queued-grandchild (parent)
                    "Queue a detached assignment with its actual child execution as parent."
                    (let ((job
                           (task-tests--register-job (application--task-orchestrator app-a) parent
                            (acp-extension-job-tests--definition) :name "grandchild")))
                      (setf (slot-value job 'completion-owner-conversation)
                            (conversation-identifier (agent-conversation parent))
                            (task-job-inherited-reference-p job) t
                            (task-job-inherited-reference-items job)
                            (conversation-inherited-reference-snapshot
                             (agent-conversation parent) 8192))
                      job))

                  (start-grandchild (job parent-channel)
                    "Run a queued descendant after departure and inspect its persisted ancestry."
                    (test-assert
                     (and (not (job-terminal-p job)) (null (job-cancellation-reason job))
                          (null (task-job--cancelled-ancestor-reason job)))
                     "a detached descendant is runnable after its participant parent departs")
                    (with-lock-held ((cl-jobpond::job--lock job))
                      (setf (job-state job) ':running))
                    (let* ((result (task-run-child job))
                           (conversation
                            (participant-state-conversation (task-job-participant-state job)))
                           (reference
                            (find ':inherited-reference
                                  (conversation--read-records (conversation-pathname conversation))
                                  :key #'first)))
                      (test-assert
                       (and (eq (getf result :status) ':success)
                            (equal (conversation-parent-identifier conversation) parent-channel)
                            (equal (getf (rest reference) :source-conversation-id) parent-channel))
                       "the descendant executes with its assignment's original parent and inherited-history source")
                      (task-tests--publish-terminal job ':completed result))))
           (multiple-value-bind (child job child-agent)
               (running-child "child")
             (let ((grandchild (queued-grandchild child-agent)))
               (test-assert
                (and
                 (null
                  (refusal ':move
                   (list :participant (participant-identifier child) :channel "bdest11")))
                 (equal (residence child) "bdest11") (job-terminal-p job)
                 (eq (job-cancellation-reason job) ':moved))
                "moving a child whose task runs interrupts the task and moves the child")
               (test-assert
                (and (not (job-terminal-p grandchild)) (null (job-cancellation-reason grandchild))
                     (null (task-job--cancelled-ancestor-reason grandchild)))
                "the task of the child's own child goes on, and may still start")
               (start-grandchild grandchild (task-job-execution-identifier job))
               (let* ((opened
                       (world-server-tests--open entry (world-tests--workspace root "b") :channel
                        "bdest11"))
                      (destination
                       (world-host-application
                        (world-server--find-host server (getf (rest opened) :session-id)))))
                 (test-assert
                  (and
                   (equal (getf (application-goal destination) :objective) "child ordinary goal")
                   (not (typep (application-agent destination) 'task-child-agent))
                   (eq (agent-participant-state (application-agent destination))
                       (application-participant-state destination))
                   (search "child context follows its participant"
                           (json-encode
                            (coerce
                             (conversation-input-items-for-request
                              (agent-conversation (application-agent destination)))
                             'vector))))
                  "pooled context and ordinary goal are adopted without the task's assignment execution"))
               (test-assert
                (null (refusal ':dismiss (list :participant (participant-identifier child))))
                "the adopted participant can leave a hosted destination")
               (multiple-value-bind (child job child-agent)
                   (running-child "hosted")
                 (let ((grandchild (queued-grandchild child-agent)))
                   (test-assert
                    (null
                     (refusal ':move
                      (list :participant (participant-identifier child) :channel "bdest11")))
                    "a pooled participant can move directly into a hosted destination")
                   (start-grandchild grandchild (task-job-execution-identifier job))))))
           (multiple-value-bind (child job child-agent)
               (running-child "second")
             (let ((grandchild (queued-grandchild child-agent)))
               (test-assert
                (and (null (refusal ':dismiss (list :participant (participant-identifier child))))
                     (null (residence child)) (eq (job-cancellation-reason job) ':moved))
                "dismissing a child whose task runs interrupts the task and parks the child")
               (start-grandchild grandchild (task-job-execution-identifier job))))
           (multiple-value-bind (child job)
               (running-child "timeout-child")
             (test-call-with-function-replacements
              (list
               (list 'session-job-await
                     (lambda (job timeout) (declare (ignore job timeout)) (values nil nil))))
              (lambda ()
                (test-assert
                 (and (refusal ':dismiss (list :participant (participant-identifier child)))
                      (equal (residence child) (task-job-execution-identifier job))
                      (eq (job-cancellation-reason job) ':moved))
                 "a cancellation timeout refuses the residence change without undoing cancellation")))))))))
  nil)

(-> test-world-server-move-goal () null)
(defun test-world-server-move-goal ()
  "Test an agent's goal goes where the agent goes, a mission arrives paused, the channel left keeps none, and a channel without an agent runs no turn."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry (world-client-entry configuration))
            (id-a  (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                         :session-id))
            (id-b  (getf (rest (world-server-tests--open entry (world-tests--workspace root "b")))
                         :session-id))
            (app-a (world-host-application (world-server--find-host server id-a)))
            (app-b (world-host-application (world-server--find-host server id-b))))
       (conversation-append-user-message (application-conversation app-a) "first words in a")
       (conversation-append-user-message (application-conversation app-b) "first words in b")
       (let* ((world   (values (world-reconcile-sessions configuration)))
              (agent-a (find ':agent (world-channel-residents world id-a) :key #'participant-kind))
              (agent-b (find ':agent (world-channel-residents world id-b) :key #'participant-kind))
              (state-a (application-participant-state app-a)))
         (labels ((query (operation &rest arguments)
                    "Send OPERATION with ARGUMENTS to the server and return the answer's plist."
                    (rest (localgroup-query-record entry operation arguments)))

                  (recorded-goal (application)
                    "Return the goal APPLICATION's conversation records last."
                    (goal-record->goal (conversation-latest-goal-record
                                        (application-conversation application))))

                  (objective (application)
                    "Return the objective of APPLICATION's goal, or NIL."
                    (getf (application-goal application) :objective)))
           (setf (application-goal app-a) (list :objective     "ship the release"
                                                :status        ':active
                                                :continuations 0
                                                :created-at    1))
           (application--record-goal app-a)
          (application--install-configuration app-a (application-configuration app-a))
          (test-assert (and (eq state-a (application-participant-state app-a))
                            (eq state-a (agent-participant-state (application-agent app-a)))
                            (eq (participant-state-conversation state-a)
                                (agent-conversation (application-agent app-a)))
                            (equal (getf (participant-state-goal state-a) :objective)
                                   "ship the release"))
                       "runtime recreation shares the actual context and ordinary goal")
          (query ':dismiss :participant (participant-identifier agent-b))
          (query ':move :participant (participant-identifier agent-a) :channel id-b)
          (test-assert (and (eq state-a (application-participant-state app-b))
                            (eq state-a (agent-participant-state (application-agent app-b)))
                            (not (eq state-a (application-participant-state app-a)))
                            (eq (participant-state-conversation state-a)
                                (application-conversation app-b))
                            (equal (participant-residence (participant-state-identity state-a)) id-b))
                       "a live handoff transfers one participant state and retargets its context")
          (test-assert (and (equal (objective app-b) "ship the release")
                            (eq (getf (application-goal app-b) :status) ':active)
                            (equal (getf (recorded-goal app-b) :objective) "ship the release"))
                       "a moved agent keeps pursuing its goal in the channel it moves into")
          (test-assert (and (null (application-goal app-a))
                            (null (recorded-goal app-a)))
                       "the channel it left keeps no goal of its own")
          (test-assert (handler-case
                           (progn (application-world-before-turn app-a ':operation-request) nil)
                         (world-error ()
                           t))
                       "a channel without an agent runs no agent turn")
          (application-world-before-turn app-a ':idle-compaction)
          (let ((domestic (application-world-domestic-agent app-a)))
            (test-assert (null (application-goal app-a))
                         "a new agent starts without a goal")
            (application-mission-start app-a (mission-test--specification))
            (query ':dismiss :participant (participant-identifier agent-a))
           (test-assert (and (null (participant-state-conversation state-a))
                             (equal (getf (participant-state-goal state-a) :objective)
                                    "ship the release"))
                        "parking serializes before releasing live context without clearing the participant goal")
           (test-assert (and (null (application-goal app-b))
                             (null (recorded-goal app-b)))
                        "a parked agent takes its goal out of the channel it leaves")
           (query ':move :participant (participant-identifier domestic) :channel id-b)
           (test-assert (and (mission-goal-p (application-goal app-b))
                             (eq (getf (application-goal app-b) :status) ':paused)
                             (mission-context-find (application-conversation app-b)))
                        "a running mission arrives paused with its mission state")
           (test-assert (null (recorded-goal app-a))
                        "the mission leaves no goal behind")
           (query ':move :participant (participant-identifier agent-a) :channel id-a)
           (test-assert (equal (objective app-a) "ship the release")
                        "a parked agent brings its goal back with it")))))))
  nil)

(-> test-world-server-move () null)
(defun test-world-server-move ()
  "Test parking and a move through the server stop the agent where it was, keep its context for a channel opened later, and adopt it where it goes."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry (world-client-entry configuration))
            (id-a  (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                         :session-id))
            (id-b  (getf (rest (world-server-tests--open entry (world-tests--workspace root "b")))
                         :session-id))
            (app-a (world-host-application (world-server--find-host server id-a)))
            (app-b (world-host-application (world-server--find-host server id-b))))
       (conversation-append-user-message (application-conversation app-a) "first words in a")
       (conversation-append-user-message (application-conversation app-b) "first words in b")
       (let* ((world   (values (world-reconcile-sessions configuration)))
              (human   (world-human-participant world))
              (agent-a (find ':agent (world-channel-residents world id-a) :key #'participant-kind))
              (agent-b (find ':agent (world-channel-residents world id-b) :key #'participant-kind)))
         (labels ((move (participant channel)
                    "Move PARTICIPANT into CHANNEL through the server and return the answer's plist."
                    (rest (localgroup-query-record entry ':move
                                                   (list :participant participant :channel channel))))

                  (refused-p (participant channel)
                    "Return whether moving PARTICIPANT into CHANNEL is refused."
                    (handler-case (progn (move participant channel) nil)
                      (localgroup-error () t)))

                  (close-channel (identifier)
                    "Close IDENTIFIER and wait for its ownership to retire."
                    (let ((thread (world-host-thread (world-server--find-host server identifier))))
                      (localgroup-query-record entry ':close (list :channel identifier))
                      ;; Routing ends as soon as close is requested, before
                      ;; APPLICATION-SERVE releases the conversation lease.
                      (sb-thread:join-thread thread :default nil :timeout 20)
                      (test-assert (not (thread-alive-p thread))
                                   "the closed channel's serving thread retires"))))
           (test-assert (and agent-a agent-b)
                        "both hosted channels have agents once their conversations persist")
           (test-assert (and (refused-p (participant-name agent-a) id-b)
                             (refused-p (participant-identifier human) id-a))
                        "a move into a channel with an agent is refused, and so is moving the person")
           (let ((answer (rest (localgroup-query-record
                                entry ':dismiss (list :participant (participant-name agent-b))))))
             (test-assert (and (string= (getf answer :participant) (participant-identifier agent-b))
                               (string= (getf answer :name) (participant-name agent-b))
                               (string= (getf answer :channel) id-b)
                               (not (application-world-agent-present-p app-b))
                               (null (participant-residence
                                      (world-participant-find (world-load configuration)
                                                              (participant-identifier agent-b))))
                               (string= (nth-value 1 (world-context-read
                                                      configuration (participant-identifier agent-b)))
                                        id-b))
                          "a dismissal through the server parks the agent with its context kept and leaves its hosted channel without one"))
           (test-call-with-function-replacements
            (list (list 'application-world-prepare-agent
                        (lambda (application participant)
                          (declare (ignore application participant))
                          (error 'world-error :message "synthetic provider failure" :operation ':move))))
            (lambda ()
              (test-assert (and (refused-p (participant-name agent-a) id-b)
                                (equal (participant-residence
                                        (world-participant-find (world-load configuration)
                                                                (participant-identifier agent-a)))
                                       id-a)
                                (application-world-agent-present-p app-a)
                                (not (application-world-agent-present-p app-b))
                                (not (application-localgroup-paused-p app-a)))
                           "a destination that cannot start the agent's runtime refuses the move and leaves the agent working where it was")))
           (close-channel id-b)
           (let* ((controller (application-input-controller app-a))
                  (answer     nil)
                  (thread     nil))
             ;; The agent is in a turn: the move waits for its safe boundary.
             (application-input-controller-begin-external-work controller)
             (unwind-protect
                  (progn
                    (setf thread (make-thread (lambda ()
                                                (setf answer (move (participant-name agent-a) id-b)))
                                              :name "Autolith world server test move"))
                    ;; The move asks the turn to stop; the turn here is this
                    ;; thread, which takes the request at its safe boundary.
                    (test-assert (handler-case (progn (sleep 5) nil)
                                   (application-turn-cancelled ()
                                     t))
                                 "a move asks the agent's turn to stop")
                    (sleep 0.3)
                    (test-assert (and (null answer)
                                      (equal (participant-residence
                                              (world-participant-find (world-load configuration)
                                                                      (participant-identifier agent-a)))
                                             id-a))
                                 "until the turn ends the move waits, and the world store has not moved the agent"))
               (application-input-controller-finish-external-work controller))
             (sb-thread:join-thread thread :default nil :timeout 30)
             (test-assert (and answer
                               (string= (getf answer :participant) (participant-identifier agent-a))
                               (string= (getf answer :source) id-a)
                               (string= (getf answer :channel) id-b)
                               (zerop (getf answer :dropped)))
                          "a move answers with the agent, the channel left and the destination"))
           (let ((waiting (world-context-read configuration (participant-identifier agent-a))))
             (test-assert (and (not (application-world-agent-present-p app-a))
                               (some (lambda (row) (search "first words in a" (getf row :wire-json)))
                                     waiting))
                          "the channel left has no agent, and the context waits for the unserved destination")
             (world-channel-create configuration
                                   :id        "bthird3"
                                   :owner     (participant-identifier human)
                                   :workspace (world-tests--workspace root "c"))
             (move (participant-identifier agent-a) "bthird3")
             (move (participant-identifier agent-a) id-b)
             (test-assert (equal (world-context-read configuration (participant-identifier agent-a))
                                 waiting)
                          "moves between channels nobody serves keep the waiting context with the agent"))
           (let* ((opened  (localgroup-query-record entry ':open (list :channel  id-b
                                                                       :rows     30
                                                                       :columns  90
                                                                       :styled-p nil)))
                  (app-b   (world-host-application
                            (world-server--find-host server (getf (rest opened) :session-id))))
                  (adopted (find ':context-adoption
                                 (application-tests--conversation-records
                                  (application-conversation app-b))
                                 :key #'first :from-end t)))
             (test-assert (and (string= (getf (rest opened) :session-id) id-b)
                               adopted
                               (string= (getf (rest adopted) :participant) (participant-identifier agent-a))
                               (not (probe-file (world-context-pathname
                                                 configuration (participant-identifier agent-a)))))
                          "a channel opened later adopts the context its agent carried, durably, and the waiting copy goes")
             (let ((answer (move (participant-identifier agent-a) id-a)))
               (test-assert (string= (getf answer :source) id-b)
                            "the agent moves on from the channel opened for it"))
             (let* ((record (first (last (application-tests--conversation-records
                                          (application-conversation app-a)))))
                    (text   (json-encode (coerce (conversation-input-items-for-request
                                                  (application-conversation app-a))
                                                 'vector))))
               (test-assert (and (application-world-agent-present-p app-a)
                                 (not (application-world-agent-present-p app-b))
                                 (string= (participant-identifier
                                           (world-session-participant
                                            configuration (application-conversation app-a)))
                                          (participant-identifier agent-a))
                                 (eq (first record) ':message)
                                 (getf (rest record) :automatic-p)
                                 (search "were moved into channel" (getf (rest record) :content))
                                 (zerop (deque-count (application-input-controller-work-items
                                                      (application-input-controller app-a)))))
                            "the destination adopts the agent, tells it so durably, and starts no turn")
               (test-assert (and (search "first words in a" text)
                                 (not (search "first words in a" text
                                              :start2 (1+ (search "first words in a" text))))
                                 (not (search "first words in b" text)))
                            "back where it began, the agent's context holds its own history once and nothing of the other channel's earlier agent")))
           (let ((app-b (world-host-application (world-server--find-host server id-b))))
            (flet ((adoptions ()
                     "Return how many context adoptions channel B's conversation holds."
                     (count ':context-adoption (application-tests--conversation-records
                                                (application-conversation app-b))
                            :key #'first))

                   (context-file-p ()
                     "Return whether agent A's context file exists."
                     (not (null (probe-file (world-context-pathname
                                             configuration (participant-identifier agent-a)))))))
              (let ((before (adoptions)))
                (test-call-with-function-replacements
                 (list (list 'application-world-install-agent
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (error 'world-error :message   "synthetic adoption failure."
                                                   :operation ':move))))
                 (lambda ()
                   (let ((answer (move (participant-identifier agent-a) id-b)))
                     (test-assert (and (string= (getf answer :channel) id-b)
                                       (equal (participant-residence
                                               (world-participant-find (world-load configuration)
                                                                       (participant-identifier agent-a)))
                                              id-b)
                                       (context-file-p)
                                       (equal (nth-value 2 (world-context-read
                                                            configuration (participant-identifier agent-a)))
                                              id-b)
                                       (= (adoptions) before))
                                  "a destination that fails to adopt after the move committed keeps the move and the context waiting for it"))))
                (application-world-before-turn app-b ':message)
                (test-assert (and (= (adoptions) (1+ before))
                                  (not (context-file-p)))
                             "the agent adopts its waiting context before its next turn")
                (world-context-write configuration (participant-identifier agent-a)
                                     :rows        (conversation-context-rows
                                                   (application-conversation app-b))
                                     :source      id-a
                                     :destination id-b
                                     :transition  (conversation-adopted-transition
                                                   (application-conversation app-b)))
                (application-world-before-turn app-b ':message)
                (test-assert (and (= (adoptions) (1+ before))
                                  (not (context-file-p)))
                             "a context file left behind after its adoption is removed without a second adoption"))
              (test-call-with-function-replacements
               (list (list 'world-participant-move
                           (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error 'world-error :message   "synthetic store failure."
                                                 :operation ':move))))
               (lambda ()
                 (test-assert (and (refused-p (participant-identifier agent-a) "bthird3")
                                   (not (context-file-p))
                                   (application-world-agent-present-p app-b)
                                   (not (application-localgroup-paused-p app-b)))
                              "a move failing at its commit leaves no context file behind and the agent working where it was")))
              (test-assert (handler-case
                               (progn
                                 (world-participant-move configuration (participant-identifier agent-a)
                                                         "bthird3" :expected-residence id-a)
                                 nil)
                             (world-error (condition)
                               (search "moved while" (autolith-error-message condition))))
                           "the store moves an agent only from where the move found it")
             (let* ((id-d  (getf (rest (world-server-tests--open entry (world-tests--workspace root "d")))
                                 :session-id))
                    (app-d (world-host-application (world-server--find-host server id-d))))
               (conversation-append-user-message (application-conversation app-d) "first words in d")
               (localgroup-query-record
                entry ':dismiss
                (list :participant (participant-identifier
                                    (find ':agent (world-channel-residents
                                                   (values (world-reconcile-sessions configuration))
                                                   id-d)
                                          :key #'participant-kind))))
               (close-channel id-d)
               (move (participant-identifier agent-a) id-d)
               (test-call-with-function-replacements
                (list (list 'application-world-install-agent
                            (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (error 'world-error :message   "synthetic adoption failure."
                                                  :operation ':move))))
                (lambda ()
                  (let ((opened (localgroup-query-record entry ':open (list :channel  id-d
                                                                            :rows     30
                                                                            :columns  90
                                                                            :styled-p nil))))
                    (test-assert (and (string= (getf (rest opened) :session-id) id-d)
                                      (world-server--find-host server id-d)
                                      (context-file-p))
                                 "a channel whose waiting context cannot be adopted still opens, and the context keeps waiting"))))
               (application-world-before-turn
                (world-host-application (world-server--find-host server id-d)) ':message)
               (test-assert (not (context-file-p))
                            "the opened channel adopts the context before the agent's first turn")))))))))
  nil)

(-> world-server-tests--adoption-failure (keyword application) list)
(defun world-server-tests--adoption-failure (stage application)
  "Return a one-shot failure after adoption STAGE in APPLICATION."
  (let* ((name     (ecase stage
                     (:context
                      'conversation-append-context-adoption)
                     (:context-record
                      'conversation-append-record)
                     (:announcement
                      'conversation-append-user-message)
                     (:announcement-record
                      'conversation-append-record)
                     (:goal
                      'application-world--install-goal)
                     (:goal-record
                      'conversation-append-record)
                     (:remap
                      'world-session-remap)
                     (:runtime
                      'application-refresh-context-meter)))
         (original (symbol-function name))
         (fired-p  nil))
    (list name
          (lambda (&rest arguments)
            (let* ((conversation (application-conversation application))
                   (target-p     (case stage
                                   ((:context :announcement)
                                    (eq (first arguments) conversation))
                                   ((:context-record :announcement-record :goal-record)
                                    (and (eq (first arguments) conversation)
                                         (eq (first (second arguments))
                                             (ecase stage
                                              (:context-record
                                               ':context-adoption)
                                              (:announcement-record
                                               ':message)
                                              (:goal-record
                                               ':goal)))))
                                   (:remap
                                    (and (conversation-adopted-transition conversation)
                                         (equal (second arguments) (conversation-identifier conversation))))
                                   (otherwise
                                    (eq (first arguments) application))))
                   (results      nil))
              (unless (and target-p (eq stage ':goal) (not fired-p))
                (setf results (multiple-value-list (apply original arguments))))
              (when (and target-p (not fired-p))
                (setf fired-p t)
                (error 'world-error :message "Injected adoption interruption." :operation ':move))
              (values-list results))))))

(-> world-server-tests--check-adoption
    (application configuration &key (:identifier string) (:transition string)) null)
(defun world-server-tests--check-adoption (application configuration &key identifier transition)
  "Check complete, unique adoption TRANSITION for participant IDENTIFIER in APPLICATION."
  (let* ((conversation (application-conversation application))
         (records      (application-tests--conversation-records conversation))
         (agent        (world-participant-find (world-load configuration) identifier)))
    (test-assert (and (not (probe-file (world-context-pathname configuration identifier)))
                      (equal (getf (application-goal application) :objective) "recover the carried goal")
                      (equal (getf (goal-record->goal (conversation-latest-goal-record conversation)) :objective)
                             "recover the carried goal"))
                 "complete adoption removes staged state only with the carried goal installed durably")
    (test-assert (and (= 1 (count ':context-adoption records :key #'first))
                      (= 1 (count-if (lambda (record)
                                       (and (eq (first record) ':message)
                                            (getf (rest record) :automatic-p)
                                            (search "were moved into" (getf (rest record) :content))))
                                     records))
                      (= 1 (count-if (lambda (record)
                                       (and (eq (first record) ':goal)
                                            (equal (getf (rest record) :objective) "recover the carried goal")))
                                     records)))
                 "context, arrival announcement and carried goal are each durable once")
    (test-assert (and (equal transition (conversation-adopted-transition conversation))
                      (equal (config :reasoning-effort (application-configuration application))
                             (getf (participant-settings agent) :reasoning-effort))
                      (eq (application-provider application) (agent-provider (application-agent application)))
                      (eq conversation (agent-conversation (application-agent application))))
                 "the recovered runtime uses the resident's configuration, provider and conversation")
    (let ((text (json-encode (coerce (conversation-input-items-for-request conversation) 'vector))))
      (test-assert (and (search "carried source context" text)
                        (search "were moved into" text)
                        (not (search "obsolete destination context" text)))
                   "the adopted provider context contains the source, not the previous resident")))
  nil)

(-> world-server-tests--recover-adoption-child (configuration string) null)
(defun world-server-tests--recover-adoption-child (configuration channel)
  "Recover CHANNEL in a fresh process booted from this check run's source-matched active core."
  (let* ((core      (config :active-image-core configuration))
         (form
           `(let* ((configuration
                     (make-configuration
                      ,@(loop for key in '(:source-root :working-directory :config-root :data-root :state-root :cache-root
                                           :codex-auth-path :grok-bootstrap-auth-path :active-image-core)
                              append (list key (config key configuration)))
                      :onboarding-p nil :fullscreen-p nil :telemetry-enabled-p nil))
                   (application (application-create configuration :conversation-id ,channel :pristine-p t)))
              (application-world-adopt-pending-context application :strict-p t)
              (assert (equal (getf (application-goal application) :objective) "recover the carried goal"))
              (assert (equal (config :reasoning-effort (application-configuration application)) "low"))
              (assert (eq (application-provider application) (agent-provider (application-agent application))))
              (write-line "ADOPTION-RECOVERED")))
         (init-root (merge-pathnames "recovery-process-config/" (config :working-directory configuration)))
         (init-path (merge-pathnames "autolith/init.lisp" init-root)))
    (test-assert (and core (probe-file core)) "restart recovery uses a saved source-matched active image")
    (ensure-directories-exist init-path)
    (with-open-file (stream init-path :direction ':output :if-exists ':supersede)
      (let ((*package* (find-package '#:cl-user)))
        (write `(progn ,form (uiop:quit 0)) :stream stream :readably t)))
    (multiple-value-bind (status output)
        (test-run-program-with-environment
         (active-image-process-command configuration '("models"))
         (list (format nil "XDG_CONFIG_HOME=~A" (namestring init-root))
               (format nil "XDG_DATA_HOME=~A" (namestring (config :data-root configuration)))
               (format nil "XDG_STATE_HOME=~A" (namestring (config :state-root configuration)))
               (format nil "HOME=~A" (namestring (config :working-directory configuration)))
               "AUTOLITH_SITE_CONFIG_ROOT=" "AUTOLITH_TELEMETRY_ENABLED=false"))
      (test-assert (and (zerop status) (search "ADOPTION-RECOVERED" output))
                   (format nil "a fresh process recovers the goal and runtime: ~A" output))))
  nil)

(-> test-world-server-adoption-recovery () null)
(defun test-world-server-adoption-recovery ()
  "Test interruptions at each adoption stage, with live retry and reloaded server recovery."
  (let* ((record '(:goal :kind :mission :objective "carry mission" :status :paused
                   :seq 1 :time 1 :adoption-transition "old-transition"))
         (goal   (goal-record->goal record)))
    (test-assert (and (mission-goal-p goal)
                      (not (getf goal :adoption-transition))
                      (equal (getf (rest (append (goal->record goal)
                                                '(:adoption-transition "new-transition")))
                                   :adoption-transition)
                             "new-transition"))
                 "mission serialization does not carry an earlier adoption identity into the next move"))
  (dolist (stage '(:context-record :context :announcement-record :announcement :goal :goal-record :remap :runtime))
    (dolist (restart-p '(nil t))
      (with-test-configuration (configuration root)
        (let ((destination nil)
              (identifier  nil)
              (transition  nil))
          (world-server-tests--call-with-server
           configuration
           (lambda (server)
             (let* ((entry  (world-client-entry configuration))
                    (source (getf (rest (world-server-tests--open entry (world-tests--workspace root "source")))
                                  :session-id))
                    (target (getf (rest (world-server-tests--open entry (world-tests--workspace root "target")))
                                  :session-id))
                    (app-a  (world-host-application (world-server--find-host server source)))
                    (app-b  (world-host-application (world-server--find-host server target))))
               (setf destination target)
               (conversation-append-user-message (application-conversation app-a) "carried source context")
               (conversation-append-user-message (application-conversation app-b) "obsolete destination context")
               (let* ((world   (values (world-reconcile-sessions configuration)))
                      (agent-a (find ':agent (world-channel-residents world source) :key #'participant-kind))
                      (agent-b (find ':agent (world-channel-residents world target) :key #'participant-kind)))
                 (setf identifier (participant-identifier agent-a))
                 (let ((settings (copy-list (participant-settings agent-a))))
                   (setf (getf settings :reasoning-effort) "low")
                   (world-participant-update configuration identifier :settings settings))
                 (setf (application-goal app-a)
                       '(:objective "recover the carried goal" :status :active :continuations 0 :created-at 1))
                 (application--record-goal app-a)
                 (localgroup-query-record entry ':dismiss (list :participant (participant-identifier agent-b)))
                 (test-call-with-function-replacements
                  (list (world-server-tests--adoption-failure stage app-b))
                  (lambda ()
                    (localgroup-query-record entry ':move (list :participant identifier :channel target))))
                 (setf transition (nth-value 3 (world-context-read configuration identifier)))
                 (test-assert (and transition (probe-file (world-context-pathname configuration identifier)))
                              "an interrupted installation retains the staged transition")
                 (unless restart-p
                   (application-world-adopt-pending-context app-b :strict-p t)
                   (world-server-tests--check-adoption app-b configuration :identifier identifier :transition transition))))))
          (when restart-p
            (world-server-tests--recover-adoption-child
             (configuration-copy configuration :working-directory (world-tests--workspace root "target"))
             destination)
            (world-server-tests--call-with-server
             configuration
             (lambda (server)
               (let* ((entry  (world-client-entry configuration))
                      (opened (world-server-tests--open entry (world-tests--workspace root "target")
                                                        :session-id destination))
                      (app    (world-host-application
                               (world-server--find-host server (getf (rest opened) :session-id)))))
                 (application-world-adopt-pending-context app :strict-p t)
                 (world-server-tests--check-adoption app configuration :identifier identifier :transition transition)))))))))
  nil)

(-> test-move-picker-available-destinations () null)
(defun test-move-picker-available-destinations ()
  "Offer only channels that can accept an agent, excluding occupied channels and the inbox."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry   (world-client-entry configuration))
            (id-a    (getf (rest (world-server-tests--open entry (world-tests--workspace root "a"))) :session-id))
            (id-b    (getf (rest (world-server-tests--open entry (world-tests--workspace root "b"))) :session-id))
            (app-a   (world-host-application (world-server--find-host server id-a)))
            (app-b   (world-host-application (world-server--find-host server id-b)))
            (offered nil))
       (conversation-append-user-message (application-conversation app-a) "occupied")
       (conversation-append-user-message (application-conversation app-b) "vacant later")
       (let* ((world (values (world-reconcile-sessions configuration)))
              (agent (find ':agent (world-channel-residents world id-b) :key #'participant-kind)))
         (world-server-dismiss server (participant-name agent)))
       (test-call-with-function-replacements
        (list (list 'application--pick-identifier
                    (lambda (application &key title items hint initial-value search-p usage empty-notice)
                      (declare (ignore application title hint initial-value search-p usage empty-notice))
                      (setf offered (mapcar (lambda (item) (getf item :value)) items))
                      nil)))
        (lambda ()
          (application--pick-destination app-a)
          (test-assert (equal offered (list id-b))
                       "the picker excludes occupied A and the human inbox, and offers vacant B")
          (application-world-domestic-agent app-b)
          (application--pick-destination app-a)
          (test-assert (null offered) "no destinations are offered when every channel has an agent"))))))
  nil)

(-> test-world-channel-command-admission () null)
(defun test-world-channel-command-admission ()
  "Execute channel navigation and placement through the reader while another channel works."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry        (world-client-entry configuration))
            (id-a         (getf (rest (world-server-tests--open entry (world-tests--workspace root "a"))) :session-id))
            (id-b         (getf (rest (world-server-tests--open entry (world-tests--workspace root "b"))) :session-id))
            (app-a        (world-host-application (world-server--find-host server id-a)))
            (app-b        (world-host-application (world-server--find-host server id-b)))
            (conversation (application-conversation app-a))
            (controller   (application-input-controller app-a))
            (picks        nil))
       (conversation-append-user-message (application-conversation app-a) "working in a")
       (conversation-append-user-message (application-conversation app-b) "working in b")
       (multiple-value-bind (connection seen await)
           (test-localgroup--present (world-server-tests--session server id-a) :columns 100)
         (let* ((world (values (world-reconcile-sessions configuration)))
                (agent (find ':agent (world-channel-residents world id-b) :key #'participant-kind))
                (name  (participant-name agent)))
           (application-input-controller-begin-external-work controller)
           (unwind-protect
                (test-call-with-function-replacements
                 (list (list 'world-server-wake-channel
                             (lambda (candidate identifier)
                               (declare (ignore candidate identifier))
                               nil))
                       (list 'application--pick-channel
                             (lambda (application)
                               (declare (ignore application))
                               (push ':switch picks)
                               id-b))
                       (list 'application--pick-agent
                             (lambda (application &key title usage)
                               (declare (ignore application title usage))
                               (push ':agent picks)
                               name))
                       (list 'application--pick-identifier
                             (lambda (application &key title items hint initial-value search-p usage empty-notice)
                               (declare (ignore application title hint initial-value search-p usage empty-notice))
                               (push ':channel picks)
                               (test-assert (find id-b items :key (lambda (item) (getf item :value)) :test #'equal)
                                            "the picker can select channel B")
                               id-b)))
                 (lambda ()
                   (labels ((submit (source)
                              "Submit SOURCE through the live busy controller and check it never queues."
                              (application-input-controller--handle-submission controller source)
                              (test-assert (and (application-input-controller-turn-active-p controller)
                                                (deque-empty-p (application-input-controller-work-items controller))
                                                (deque-empty-p (application-input-controller-pending-apply-items controller))
                                                (eq (application-conversation app-a) conversation))
                                           (format nil "~A executes while A keeps working" source)))

                            (switches ()
                              "Return how often the terminal client was asked to move to channel B."
                              (count-if (lambda (event)
                                          (and (eq (first event) ':ui)
                                               (eq (getf (rest event) :operation) ':switch)
                                               (equal (getf (rest event) :session-id) id-b)))
                                        (funcall seen))))
                      (let ((before (length (world-server--hosts server))))
                        (submit "(new)")
                        (test-assert (= (length (world-server--hosts server)) (1+ before))
                                     "new opens another hosted channel before A finishes"))
                      (let ((before (switches)))
                        (submit (format nil "(open ~S)" id-b))
                        (test-assert (task-tests--wait-until (lambda () (= (switches) (1+ before))) 10)
                                     "typed switch asks the terminal client to move while A works"))
                      (submit (format nil "(dismiss ~S)" name))
                      (test-assert (null (participant-residence
                                          (world-participant-find (world-load configuration) (participant-identifier agent))))
                                   "dismiss parks B's agent before A finishes")
                      (submit (format nil "(tell ~S ~S)" id-b "busy message"))
                      (test-assert (some (lambda (delivery) (string= (world-delivery-text delivery) "busy message"))
                                         (world-inbox-deliveries configuration id-b))
                                   "tell reaches B's durable inbox before A finishes")
                      (submit (format nil "(move ~S ~S)" name id-b))
                      (test-assert (equal id-b (participant-residence
                                                (world-participant-find (world-load configuration) (participant-identifier agent))))
                                   "move places B's agent before A finishes")
                     (submit "(open)")
                     (submit "(tell)")
                     (test-assert (funcall await ':input
                                           (lambda (event)
                                             (let ((text (getf (rest event) :text)))
                                               (and (stringp text) (search id-b text)))))
                                  "tell's picker prefills the terminal client's composer during inference")
                     (submit "(dismiss)")
                     (submit "(move)")
                     (test-assert (and (member ':switch picks) (member ':channel picks) (member ':agent picks))
                                  "argument-free channel commands reach their pickers while A works")
                     (test-assert (equal id-b (participant-residence
                                               (world-participant-find (world-load configuration) (participant-identifier agent))))
                                  "picker-driven move executes while A works"))))
             (application-input-controller-finish-external-work controller)
             (localgroup-client-close connection)))))))
  nil)

(-> test-world-execution-owner-claims () null)
(defun test-world-execution-owner-claims ()
  "Refuse foreign claims and stale residence without altering stored participant context."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (declare (ignore human))
      (world-park-agent configuration second-agent)
      (let ((lease (conversation-lease-acquire configuration "bfirst1")))
        (unwind-protect
             (test-assert
              (handler-case
                  (progn (world-move-agent configuration first-agent "bsecnd2") nil)
                (world-error ()
                  (equal (participant-residence
                          (world-participant-find (world-load configuration)
                                                  (participant-identifier first-agent)))
                         "bfirst1")))
              "a stored execution owner cannot claim another execution's channel lease")
          (conversation-lease-release lease)))
      (world-participant-park configuration (participant-identifier first-agent))
      (test-assert
       (handler-case
           (progn (world-move-agent configuration first-agent "bsecnd2") nil)
         (world-error ()
           (not (probe-file (world-context-pathname configuration
                                                    (participant-identifier first-agent))))))
       "a stale residence is refused before staging any context")))
  nil)

(-> test-world-execution-owner-staging () null)
(defun test-world-execution-owner-staging ()
  "Restore precommit staging and hosted execution after a failed store commit."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry (world-client-entry configuration))
            (id-a  (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                         :session-id))
            (id-b  (getf (rest (world-server-tests--open entry (world-tests--workspace root "b")))
                         :session-id))
            (app-a (world-host-application (world-server--find-host server id-a)))
            (app-b (world-host-application (world-server--find-host server id-b))))
       (conversation-append-user-message (application-conversation app-a) "original participant context")
       (conversation-append-user-message (application-conversation app-b) "destination context")
       (let* ((world      (values (world-reconcile-sessions configuration)))
              (agent-a    (find ':agent (world-channel-residents world id-a) :key #'participant-kind))
              (agent-b    (find ':agent (world-channel-residents world id-b) :key #'participant-kind))
              (identifier (participant-identifier agent-a))
              (state      (application-participant-state app-a)))
         (world-server-dismiss server (participant-identifier agent-b))
         (world-context-write configuration identifier :source id-a :rows nil
                              :goal '(:objective "previous staging" :status :paused)
                              :destination nil :transition "previous-stage")
         (let ((previous (multiple-value-list (world-context-read configuration identifier))))
           (test-call-with-function-replacements
            (list (list 'world-participant-move
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error 'world-error :message "Injected residence commit refusal."
                                 :operation            ':move))))
            (lambda ()
              (test-assert
               (handler-case
                   (progn (world-server-move server identifier id-b) nil)
                 (world-error ()
                   t))
               "the injected precommit refusal reaches the coordinator")))
           (test-assert
            (and (equal previous (multiple-value-list (world-context-read configuration identifier)))
                 (eq state (application-participant-state app-a))
                 (eq state (agent-participant-state (application-agent app-a)))
                 (equal (participant-residence
                         (world-participant-find (world-load configuration) identifier)) id-a)
                 (not (application-localgroup-paused-p app-a))
                 (handler-case
                     (let ((lease (conversation-lease-acquire configuration id-a)))
                       (conversation-lease-release lease)
                       nil)
                   (conversation-in-use ()
                     t)))
            "precommit abort restores staging, shared execution, residence and the hosted lease")
           (test-call-with-function-replacements
            (list (list 'application-world-surrender-goal
                        (lambda (application)
                          (declare (ignore application))
                          (error "Injected committed departure failure."))))
            (lambda ()
              (test-assert
               (handler-case
                   (progn (world-server-move server identifier id-b) nil)
                 (simple-error () t))
               "a serious departure failure propagates after commitment")))
           (test-assert
            (and (equal (participant-residence
                         (world-participant-find (world-load configuration) identifier)) id-b)
                 (not (application-localgroup-paused-p app-a))
                 (not (eq state (application-participant-state app-a)))
                 (null (participant-state-conversation state))
                 (conversation-lease-busy-p configuration id-a)
                 (probe-file (world-context-pathname configuration identifier)))
            "postcommit failure detaches and resumes the source without undoing residence or staged recovery"))))))
  nil)
