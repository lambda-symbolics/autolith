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
    (configuration-ensure-directories configuration)
    (test-call-with-function-replacements
     (world-server-tests--stubs)
     (lambda ()
       (let* ((server (world-server-create configuration :permission-mode ':auto))
              (main   (make-thread (lambda () (world-server-serve server))
                                   :name "Autolith world server test main")))
         (unwind-protect
              (funcall function server configuration root)
           (world-server-stop server)
           (test-assert (task-tests--wait-until
                         (lambda () (not (thread-alive-p main))) 30)
                        "a stopped world server ends once its channels ended")
           (image-daemon:daemon-stop-thread main)
           (world-server--shutdown server))))))
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
       (multiple-value-bind (socket stream response)
           (test-localgroup--attach (world-server-tests--session server first-id) ':control)
         (unwind-protect
              (let* ((host       (world-server--find-host server first-id))
                     (other      (world-server-tests--open entry workspace))
                     (other-id   (getf (rest other) :session-id))
                     (other-host (world-server--find-host server other-id)))
                (test-assert (and (eq (first response) ':attached)
                                  (world-host-attached-p host))
                             "a terminal attaches to the hosted channel")
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
           (close stream)
           (sb-bsd-sockets:socket-close socket)))
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
                      "opening the directory again resumes its latest channel"))
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
  "Test (switch) asking the controlling terminal to move to another channel."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (workspace-a (world-tests--workspace root "a"))
            (workspace-b (world-tests--workspace root "b"))
            (id-a        (getf (rest (world-server-tests--open entry workspace-a)) :session-id))
            (id-b        (getf (rest (world-server-tests--open entry workspace-b)) :session-id))
            (application (world-host-application (world-server--find-host server id-a)))
            (terminal    (terminal-ui-terminal (application-ui application)))
            (attachment  (make-instance 'image-daemon:attachment
                                        :socket nil
                                        :stream (make-broadcast-stream)
                                        :mode   ':control))
            (presented   nil))
       (test-call-with-function-replacements
        (list (list 'application-present
                    (lambda (application entry &key identifier)
                      (declare (ignore application identifier))
                      (push entry presented))))
        (lambda ()
          (application--builtin-switch-command application id-a)
          (test-assert (search "already" (first presented))
                       "switching to the channel the terminal is in changes nothing")
          (application--builtin-switch-command application (conversation-identifier-display id-b))
          (test-assert (search "No terminal controls" (first presented))
                       "without a controller there is no terminal to move")
          (multiple-value-bind (attached-p released-p)
              (image-daemon:relay-attach terminal attachment :rows 24 :columns 80
                                                             :styled-p nil :session-id id-a)
            (declare (ignore released-p))
            (test-assert attached-p "the switch test attaches a controller"))
          (application--builtin-switch-command application (conversation-identifier-display id-b))
          (test-assert (search "Switching" (first presented))
                       "a controlled channel reports the switch it asked for")
          (let ((packets (coerce (structlisp:deque->vector
                                  (image-daemon:attachment-queue attachment))
                                 'list)))
            (test-assert (some (lambda (packet)
                                 (and (search ":SWITCH" (string-upcase packet))
                                      (search id-b packet)))
                               packets)
                         "the controller is sent the switch request naming the channel"))
          (test-assert (handler-case
                           (progn (application--builtin-switch-command application "zzzzzzz") nil)
                         (world-error (condition)
                           (search "does not exist" (autolith-error-message condition))))
                       "an unknown channel is refused"))))))
  nil)

(-> test-world-server-starting-host () null)
(defun test-world-server-starting-host ()
  "Test an open joins a host that is still starting instead of starting another."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((workspace (world-tests--workspace root "a"))
            (channel   (conversation-create (configuration-copy configuration
                                                                :working-directory workspace)
                                            :identifier "bstart1"))
            (terminal  (make-instance 'recording-terminal :columns 80))
            (starting  (make-instance
                        'world-host
                        :application (make-instance
                                      'application
                                      :configuration (configuration-copy
                                                      configuration
                                                      :working-directory workspace)
                                      :conversation  channel
                                      :ui            (terminal-ui-create :terminal terminal))))
            (started   nil))
       (unwind-protect
            (test-call-with-function-replacements
             (list (list 'world-server--start-host
                         (lambda (candidate &rest arguments)
                           (declare (ignore candidate))
                           (push arguments started)
                           starting)))
             (lambda ()
               (with-lock-held ((world-server-lock server))
                 (push starting (world-server-hosts server)))
               (multiple-value-bind (host started-p)
                   (world-server--host-for server (list :channel "bstart1"))
                 (test-assert (and (eq host starting) (not started-p) (null started))
                              "an open of a starting channel waits for its host"))
               (multiple-value-bind (host started-p)
                   (world-server--host-for server (list :directory workspace))
                 (test-assert (and (eq host starting) (not started-p) (null started))
                              "an open of its workspace joins the starting host nobody controls"))
               (setf (world-host-failure starting) (make-condition 'error))
               (multiple-value-bind (host started-p)
                   (world-server--host-for server (list :channel "bstart1"))
                 (test-assert (and (eq host starting) started-p (= (length started) 1))
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
                                     :pristine-p               nil
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
  "Test a workspace open takes the latest touched channel even when an older one is hosted."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry     (world-client-entry configuration))
            (workspace (world-tests--workspace root "a"))
            (hosted    (getf (rest (world-server-tests--open entry workspace)) :session-id)))
       (world-tests--persisted-conversation
        (configuration-copy configuration :working-directory workspace) "blater1" 1000)
       (let ((dates (list (cons hosted 1000) (cons "blater1" 5000))))
         (test-call-with-function-replacements
          (list (list 'conversation-storage-write-date
                      (lambda (pathname)
                        (or (rest (assoc (pathname-name pathname) dates :test #'string=)) 0))))
          (lambda ()
            (multiple-value-bind (host started-p)
                (world-server--host-for server (list :directory workspace))
              (test-assert (and started-p (string= (world-host-channel host) "blater1"))
                           "a conversation written after the hosted one wins the open")
              (multiple-value-bind (again again-started-p)
                  (world-server--host-for server (list :directory workspace))
                (test-assert (and (eq again host) (not again-started-p))
                             "the next open joins it rather than starting it twice")))))))))
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

(-> test-world-server-checkpoint-quiescence () null)
(defun test-world-server-checkpoint-quiescence ()
  "Test a checkpoint waits for the work under way, pauses every channel until only the server's thread lives, then serves them again."
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
              (terminal-a  (terminal-ui-terminal (application-ui application)))
              (terminal-b  (terminal-ui-terminal (application-ui (world-host-application host-b))))
              (session-a   (world-server-tests--session server id-a))
              (attachment  (make-instance 'image-daemon:attachment
                                          :socket nil
                                          :stream (make-broadcast-stream)
                                          :mode   ':control))
              (finished    nil)
              (observed    nil))
         (image-daemon:relay-attach terminal-a attachment :rows 24 :columns 80 :styled-p nil
                                                          :session-id id-a)
         ;; A job run in B leaves its pool's reusable workers waiting.
         (test-assert (eql 42 (world-server-tests--run-job (world-host-application host-b)))
                      "a job runs in the other channel before the checkpoint")
         (test-call-with-function-replacements
          (list (list 'image-daemon:attachment-finish
                      (let ((finish (symbol-function 'image-daemon:attachment-finish)))
                        (lambda (candidate packet)
                          (push packet finished)
                          (funcall finish candidate packet)))))
          (lambda ()
            (setf observed
                  (world-server-call-quiesced
                   server application
                   (lambda ()
                     (funcall *checkpoint-thread-quiescer*
                              (lambda ()
                                (list (eq *active-application* application)
                                      (remove-if-not #'thread-alive-p
                                                     (set-difference (sb-thread:list-all-threads)
                                                                     baseline))
                                      (world-server-runtime server)
                                      (application-localgroup-session application)
                                      (world-host-paused-p host-a)
                                      (world-host-paused-p host-b)
                                      (sb-thread:thread-name (current-thread))))))))))
         (destructuring-bind (active-p threads runtime session paused-a paused-b name) observed
           (test-assert (and active-p (null runtime) (null session) paused-a paused-b
                             (string= name "Autolith world server test main"))
                        "inside the fork boundary every channel is paused and the endpoints are stopped")
           (test-assert (and (= (length threads) 1)
                             (string= (sb-thread:thread-name (first threads))
                                      "Autolith world server test main"))
                        (format nil "only the server's own thread outlives the pause: ~S"
                                (mapcar #'sb-thread:thread-name threads))))
         (test-assert (some (lambda (packet) (eq (first packet) ':checkpoint-reconnect)) finished)
                      "an attached terminal is told to reconnect")
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
         (test-assert (eql 42 (world-server-tests--run-job (world-host-application host-b)))
                      "the other channel's jobs run again after the checkpoint")
         (let* ((controller (application-input-controller (world-host-application host-b)))
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
          (list (list 'checkpoint-backend-create
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
            (test-assert (search "Checkpoint scheduled"
                                 (image-daemon:relay-history-text terminal-a))
                         "the channel is told its checkpoint is scheduled")
            (test-assert (task-tests--wait-until
                          (lambda ()
                            (and (search "Checkpoint failed: synthetic fork failure"
                                         (image-daemon:relay-history-text terminal-a))
                                 (search "A checkpoint of the world server failed"
                                         (image-daemon:relay-history-text terminal-b))))
                          30)
                         "each channel learns the outcome once it is served again")
            (test-assert (task-tests--wait-until
                          (lambda () (and (world-host-live-p host-a) (world-host-live-p host-b)))
                          30)
                         "both channels serve again after a failed checkpoint")))))))
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
            (terminal-a (terminal-ui-terminal (application-ui (world-host-application host-a))))
            (attachment (make-instance 'image-daemon:attachment
                                       :socket nil
                                       :stream (make-broadcast-stream)
                                       :mode   ':control)))
       (image-daemon:relay-attach terminal-a attachment :rows 24 :columns 80 :styled-p nil
                                                        :session-id id-a)
       (let ((answer (localgroup-query-record entry ':close
                                              (list :channel id-a :unattached-only-p t))))
         (test-assert (and (null (getf (rest answer) :closed-p))
                           (world-host-live-p host-a)
                           (localgroup-session-admits-attachment-p session-a))
                      "a channel another terminal controls survives a close meant for an idle one"))
       (let ((answer (localgroup-query-record entry ':close
                                              (list :channel id-b :unattached-only-p t))))
         (test-assert (and (getf (rest answer) :closed-p)
                           (not (localgroup-session-admits-attachment-p session-b)))
                      "a channel nobody controls is committed to close and admits no terminal from then on")
         (test-assert (task-tests--wait-until (lambda () (not (world-host-live-p host-b))) 30)
                      "the committed close ends the channel")))))
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
           (test-assert (task-tests--wait-until (lambda () (search "✉ 1 unread" (modeline))) 10)
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
  "Test an open admitted before a checkpoint finishes starting its host before the checkpoint pauses the channels."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry    (world-client-entry configuration))
            (id-a     (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                            :session-id))
            (host-a   (world-server--find-host server id-a))
            (started  (sb-thread:make-semaphore))
            (release  (sb-thread:make-semaphore))
            (start    (symbol-function 'world-server--start-host))
            (opened   nil)
            (observed nil))
       (test-call-with-function-replacements
        (list (list 'world-server--start-host
                    (lambda (candidate &rest arguments)
                      (sb-thread:signal-semaphore started)
                      (sb-thread:wait-on-semaphore release :timeout 30)
                      (apply start candidate arguments))))
        (lambda ()
          (let ((opener (make-thread
                         (lambda ()
                           (setf opened (world-server-tests--open entry (world-tests--workspace root "b"))))
                         :name "Autolith test opener")))
            (test-assert (sb-thread:wait-on-semaphore started :timeout 30)
                         "the open is admitted and starts its host")
            (let ((checkpoint (make-thread
                               (lambda ()
                                 (setf observed
                                       (world-server-call-quiesced
                                        server (world-host-application host-a)
                                        (lambda ()
                                          (funcall *checkpoint-thread-quiescer*
                                                   (lambda ()
                                                     (mapcar #'world-host-paused-p
                                                             (world-server--hosts server))))))))
                               :name "Autolith test checkpoint")))
              (sleep 0.5)
              (test-assert (and (thread-alive-p checkpoint)
                                (not (world-server-checkpointing-p server)))
                           "a checkpoint waits for the admitted open instead of declaring itself")
              (sb-thread:signal-semaphore release)
              (join-thread opener)
              (join-thread checkpoint)
              (test-assert (and (eq (first opened) ':ok)
                                (= (length observed) 2)
                                (every #'identity observed))
                           "the open completes, and the checkpoint then pauses both hosts")))))
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
          (running nil))
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
                     (setf running t)
                     (error 'localgroup-error
                            :message   "The detached world server did not start within 1 seconds."
                            :operation ':spawn))))
       (lambda ()
         (test-assert (eq (world-client--entry configuration
                                               :deadline (+ (get-internal-real-time)
                                                            (* 10 internal-time-units-per-second)))
                          winner)
                      "a lost launch race ends at the winning server's endpoint, not in an error")
         (setf polls 0 running nil)
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
                      "a launch that fails while nobody holds the world still fails")))))
  nil)

(-> test-world-client-switch-navigates () null)
(defun test-world-client-switch-navigates ()
  "Test a terminal switching channels states no launch options, and stays in its channel when the other refuses it."
  (with-test-configuration (configuration)
    (let ((opens     nil)
          (attaches  nil)
          (refuse-p  nil)
          (rejects-p nil))
      (flet ((run ()
               "Run the client loop for a terminal started with launch options, switching once."
               (setf opens nil attaches nil)
               (let ((calls 0))
                 (test-call-with-function-replacements
                  (list (list 'main--open-world-channel
                              (lambda (candidate &rest arguments)
                                (declare (ignore candidate arguments))
                                (values "bfirst1" nil)))
                        (list 'localgroup--find-record
                              (lambda (candidate session-id)
                                (declare (ignore candidate))
                                (list session-id)))
                        (list 'localgroup-attach-record
                              (lambda (candidate record mode &rest arguments)
                                (declare (ignore candidate mode arguments))
                                (push (first record) attaches)
                                (cond
                                  ((= (incf calls) 1)
                                   (list ':switch :session-id "bsecnd2"))
                                  ((and rejects-p (equal (first record) "bsecnd2"))
                                   (error 'localgroup-error
                                          :message   "Another terminal controls b-secnd2."
                                          :operation ':attach))
                                  (t
                                   ':detached))))
                        (list 'localgroup-relay-exit
                              (lambda (&rest arguments)
                                (declare (ignore arguments))
                                nil))
                        (list 'world-client-open
                              (lambda (candidate &rest arguments)
                                (declare (ignore candidate))
                                (push arguments opens)
                                (if refuse-p
                                    (error 'localgroup-error
                                           :message   "Channel b-secnd2 cannot take that."
                                           :operation ':open)
                                    (values (getf arguments :channel) nil)))))
                  (lambda ()
                    (let ((*error-output* (make-broadcast-stream)))
                      (main--run-world-client configuration
                                              :permission-mode ':full-access
                                              :launch          '(:model "other"
                                                                 :permission-mode :full-access))))))))
        (run)
        (test-assert (and (= (length opens) 1)
                          (null (getf (first opens) :launch))
                          (equal (getf (first opens) :channel) "bsecnd2"))
                     "a switch opens the other channel without this start's launch options")
        (test-assert (equal (reverse attaches) '("bfirst1" "bsecnd2"))
                     "the terminal attaches to the channel it switched to")
        (setf refuse-p t)
        (run)
        (test-assert (equal (reverse attaches) '("bfirst1" "bfirst1"))
                     "a channel that refuses the switch leaves the terminal in the channel it came from")
        (setf refuse-p  nil
              rejects-p t)
        (run)
        (test-assert (equal (reverse attaches) '("bfirst1" "bsecnd2" "bfirst1"))
                     "a channel that opens but rejects the terminal's attachment returns it to the channel it came from"))))
  nil)

(-> test-world-client-refusal-ends-start () null)
(defun test-world-client-refusal-ends-start ()
  "Test a world server's refusal ends a start with its reason, while an unreachable server sends it direct."
  (with-test-configuration (configuration)
    (flet ((open-with (condition)
             "Open this terminal's channel while the client open signals CONDITION; return :direct or :refused."
             (test-call-with-function-replacements
              (list (list 'world-client-open
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (error condition))))
              (lambda ()
                (handler-case
                    (let ((*error-output* (make-broadcast-stream)))
                      (if (main--open-world-channel configuration) :opened :direct))
                  (localgroup-error ()
                    :refused))))))
      (test-assert (eq (open-with (make-condition 'localgroup-error
                                                  :message   "Channel b-first1 is already running and cannot take --fullscreen."
                                                  :operation ':open))
                       :refused)
                   "a refusal the server answered ends the start instead of running direct")
      (test-assert (eq (open-with (make-condition 'localgroup-error
                                                  :message   "Localgroup session world is unavailable."
                                                  :operation ':open
                                                  :cause     (make-condition 'simple-error
                                                                             :format-control "refused")))
                       :direct)
                   "a server that cannot be reached sends the start direct")
      (test-assert (eq (open-with (make-condition 'localgroup-error
                                                  :message   "The detached world server did not start within 1 seconds."
                                                  :operation ':spawn))
                       :direct)
                   "a server that cannot be started sends the start direct")))
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
                      (localgroup-error () t))))
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
           (localgroup-query-record entry ':close (list :channel id-b))
           (test-assert (task-tests--wait-until
                         (lambda () (null (world-server--find-host server id-b)))
                         20)
                        "the channel without an agent closes like any other")
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
                           "the store moves an agent only from where the move found it"))))))))
  nil)
