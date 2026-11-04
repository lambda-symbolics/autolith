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
         (let ((binding (assoc '*world-selection-p*
                               (world-server--thread-bindings
                                (world-host-application (world-server--find-host server other))))))
           (test-assert (and binding (null (rest binding)))
                        "a channel's threads run without world selection, so hosting moves nobody"))))))
  nil)

(-> test-world-server-resume-order () null)
(defun test-world-server-resume-order ()
  "Test a workspace reopens its most recently active conversation, not its newest channel record."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bolder1" 1000)
    (world-tests--persisted-conversation configuration "bnewer2" 2000)
    (world-reconcile-sessions configuration :now 3000)
    (world-channel-update configuration "bolder1" :title "Older work" :now 3500)
    (world-channel-update configuration "bnewer2" :title "Newer work" :now 3600)
    (let* ((world     (world-load configuration))
           (workspace (channel-workspace (world-channel-find world "bolder1")))
           (dates     (list (cons "bolder1" 5000) (cons "bnewer2" 4000))))
      (test-call-with-function-replacements
       (list (list 'conversation-storage-write-date
                   (lambda (pathname)
                     (or (rest (assoc (pathname-name pathname) dates :test #'string=)) 0))))
       (lambda ()
         (test-assert (equal (mapcar #'channel-identifier
                                     (world-server--resume-candidates configuration world
                                                                      :workspace workspace))
                             '("bolder1" "bnewer2"))
                      "the conversation written last comes first whatever its channel's record says")
         (test-assert (equal (mapcar #'channel-identifier
                                     (world-server--resume-candidates configuration world
                                                                      :workspace workspace
                                                                      :hosted    '("bolder1")))
                             '("bnewer2"))
                      "a channel already hosted is not a candidate")))
      (test-assert (null (world-server--resume-candidates configuration world
                                                          :workspace "/nowhere/"))
                   "another workspace has no candidates")))
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
  "Test a checkpoint pauses every channel until only the server's thread lives, then serves them again."
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
         (let ((controller (application-input-controller (world-host-application host-b))))
           (application-input-controller-begin-external-work controller)
           (unwind-protect
                (test-assert
                 (handler-case
                     (progn
                       (world-server-call-quiesced
                        server application
                        (lambda () (funcall *checkpoint-thread-quiescer* (lambda () t))))
                       nil)
                   (checkpoint-error (condition)
                     (search "is working" (autolith-error-message condition))))
                 "a working channel refuses another channel's checkpoint")
             (application-input-controller-finish-external-work controller)))
         (test-assert (and (world-host-live-p host-a) (world-host-live-p host-b))
                      "a refused checkpoint leaves the channels serving")
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
                           (world-host-live-p host-a))
                      "a channel another terminal controls survives a close meant for an idle one"))
       (let ((answer (localgroup-query-record entry ':close
                                              (list :channel id-b :unattached-only-p t))))
         (test-assert (and (getf (rest answer) :closed-p)
                           (task-tests--wait-until (lambda () (not (world-host-live-p host-b))) 30))
                      "a channel nobody controls is closed on request")))))
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
