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
     (let* ((server (world-server-create configuration :permission-mode permission-mode))
            (main   (make-thread (lambda () (world-server-serve server))
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
  (with-test-configuration (configuration root)
    (let ((finished nil))
      (world-server-tests--call-with-server
       configuration
       (lambda (server)
         (let* ((entry       (world-client-entry configuration))
                (identifier  (getf (rest (world-server-tests--open entry (world-tests--workspace root "stop")))
                                   :session-id))
                (application (world-host-application (world-server--find-host server identifier)))
                (attachment  (make-instance 'image-daemon:attachment
                                            :socket nil
                                            :stream (make-broadcast-stream)
                                            :mode   ':control)))
           (image-daemon:relay-attach (terminal-ui-terminal (application-ui application)) attachment
                                      :rows 24 :columns 80 :styled-p nil :session-id identifier)
           (test-call-with-function-replacements
            (list (list 'image-daemon:attachment-finish
                        (let ((finish (symbol-function 'image-daemon:attachment-finish)))
                          (lambda (candidate packet)
                            (push packet finished)
                            (funcall finish candidate packet)))))
            (lambda ()
              (world-server--dispatch server ':stop nil)
              (test-assert (some (lambda (packet)
                                   (search "world server stopped by autolith lg stop"
                                           (princ-to-string packet)))
                                 finished)
                           "the attached terminal is told the server was stopped by lg stop")))))))
    nil))

(-> test-world-server-status-line () null)
(defun test-world-server-status-line ()
  "Test lg status leads with the world server's process and version while a server runs."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore server root))
     (let ((output (test-call-with-function-replacements
                    (list (list 'localgroup--client-configuration (lambda () configuration)))
                    (lambda ()
                      (with-output-to-string (*standard-output*)
                        (localgroup--run-status :sexp-p nil))))))
       (test-assert (and (search "· world server PID" output)
                         (search (princ-to-string (current-process-id)) output))
                    "lg status names the world server's process first"))))
  nil)

(-> test-world-server-new-channel () null)
(defun test-world-server-new-channel ()
  "Test (new) in a hosted channel opens another channel in its workspace and moves the terminal there."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (first-id    (getf (rest (world-server-tests--open entry (world-tests--workspace root "new")))
                               :session-id))
            (application (world-host-application (world-server--find-host server first-id)))
            (attachment  (make-instance 'image-daemon:attachment
                                        :socket nil
                                        :stream (make-broadcast-stream)
                                        :mode   ':control)))
       (image-daemon:relay-attach (terminal-ui-terminal (application-ui application)) attachment
                                  :rows 24 :columns 80 :styled-p nil :session-id first-id)
       (application--builtin-new-command application)
       (let* ((hosts   (mapcar #'world-host-channel (world-server--hosts server)))
              (second  (find-if-not (lambda (identifier) (string= identifier first-id)) hosts))
              (packets (coerce (structlisp:deque->vector (image-daemon:attachment-queue attachment))
                               'list)))
         (test-assert (and (= (length hosts) 2)
                           (string= (conversation-identifier (application-conversation application))
                                    first-id))
                      "the channel keeps its conversation, and a second channel runs beside it")
         (test-assert (and second
                           (some (lambda (packet)
                                   (and (search ":SWITCH" (string-upcase packet))
                                        (search second packet)))
                                 packets))
                      "the terminal is sent to the new channel")))))
  nil)

(-> test-world-server-tell () null)
(defun test-world-server-tell ()
  "Test lg tell's server operation sends a channel a message from the person, by a fragment of its identifier."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "btell11" 1000)
     (world-server--reconcile configuration)
     (test-call-with-function-replacements
      (list (list 'world-server-wake-channel
                  (lambda (candidate identifier)
                    (declare (ignore candidate identifier))
                    nil)))
      (lambda ()
        (let ((answer (world-server--dispatch server ':tell (list :channel "btell" :text "from outside"))))
          (test-assert (and (eq (first answer) ':ok)
                            (equal (getf (rest answer) :channel) "btell11"))
                       "a fragment names the channel told")
          (let ((delivery (first (world-inbox-deliveries configuration "btell11"))))
            (test-assert (and delivery
                              (string= (world-delivery-text delivery) "from outside")
                              (eq (world-delivery-sender-kind delivery) ':human))
                         "the channel's inbox holds the message from the person")))))))
  nil)

(-> test-world-server-kill-channel () null)
(defun test-world-server-kill-channel ()
  "Test lg kill of a hosted channel succeeds once the channel is gone, though the server keeps running."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry      (world-client-entry configuration))
            (identifier (getf (rest (world-server-tests--open entry (world-tests--workspace root "kill")))
                              :session-id)))
       (test-assert
        (handler-case
            (test-call-with-function-replacements
             (list (list 'localgroup--client-configuration (lambda () configuration)))
             (lambda ()
               (let ((command (parse-command-line (main--top-level-command)
                                                  (list "lg" "kill" identifier))))
                 (with-output-to-string (*standard-output*)
                   (funcall (command-handler command) command)))
               t))
          (localgroup-error ()
            nil))
        "killing a hosted channel confirms its exit")
       (test-assert (and (null (world-server--find-host server identifier))
                         (world-client-entry configuration))
                    "the channel is gone and the world server still runs"))))
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
          (test-assert (search "already" (terminal--spans-text (first presented)))
                       "switching to the channel the terminal is in changes nothing")
          (application--builtin-switch-command application (conversation-identifier-display id-b))
          (test-assert (search "no terminal controls" (terminal--spans-text (first presented)))
                       "without a controller there is no terminal to move")
          (setf presented nil)
          (multiple-value-bind (attached-p released-p)
              (image-daemon:relay-attach terminal attachment :rows 24 :columns 80
                                                             :styled-p nil :session-id id-a)
            (declare (ignore released-p))
            (test-assert attached-p "the switch test attaches a controller"))
          (application--builtin-switch-command application (conversation-identifier-display id-b))
          (test-assert (null presented)
                       "a controlled channel switches without a line about it")
          (let ((packets (coerce (structlisp:deque->vector
                                  (image-daemon:attachment-queue attachment))
                                 'list)))
            (test-assert (some (lambda (packet)
                                 (and (search ":SWITCH" (string-upcase packet))
                                      (search id-b packet)))
                               packets)
                         "the controller is sent the switch request naming the channel"))
          (test-assert (and (eq (application--builtin-switch-command application "zzzzzzz") ':continue)
                            (search "no channel matches" (terminal--spans-text (first presented))))
                       "an unknown channel is said in a line, not signaled"))))))
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
       (world-server--reconcile configuration)
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
        :input  (make-string-input-stream (format nil "/status~%~%/status~%"))
        :output output)
       (let ((text (get-output-stream-string output)))
         (test-assert (and (= 2 (terminal-tests--substring-count "› /status" text))
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
            (test-assert (search "checkpoint scheduled"
                                 (image-daemon:relay-history-text terminal-a))
                         "the channel is told its checkpoint is scheduled")
            (test-assert (task-tests--wait-until
                          (lambda ()
                            (and (search "! checkpoint failed: synthetic fork failure"
                                         (image-daemon:relay-history-text terminal-a))
                                 (search "! world server checkpoint failed"
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
          (rejects-p nil)
          (notices   nil))
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
                              (lambda (candidate record mode &key panel screen notice)
                                (declare (ignore candidate mode panel screen))
                                (push (first record) attaches)
                                (when notice
                                  (push notice notices))
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
                          (equal (getf (first opens) :channel) "bsecnd2")
                          (getf (first opens) :switch-p))
                     "a switch opens the other channel as a switch, without this start's launch options")
        (test-assert (equal (reverse attaches) '("bfirst1" "bsecnd2"))
                     "the terminal attaches to the channel it switched to")
        (setf refuse-p t)
        (run)
        (test-assert (and (equal (reverse attaches) '("bfirst1" "bfirst1"))
                          (search "could not switch to b-secnd2" (first notices)))
                     "a channel that refuses the switch leaves the terminal in the channel it came from, told why")
        (setf notices nil)
        (setf refuse-p  nil
              rejects-p t)
        (run)
        (test-assert (and (equal (reverse attaches) '("bfirst1" "bsecnd2" "bfirst1"))
                          (search "Another terminal controls" (first notices)))
                     "a channel that opens but rejects the terminal's attachment returns it to the channel it came from, told why"))))
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

(-> test-world-server-move-running-child () null)
(defun test-world-server-move-running-child ()
  "Test moving or parking a child agent interrupts the task its parent's pool runs it in, and leaves its own children's tasks alone."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry (world-client-entry configuration))
            (id-a  (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                         :session-id))
            (app-a (world-host-application (world-server--find-host server id-a))))
       (conversation-append-user-message (application-conversation app-a) "first words in a")
       (let* ((world   (values (world-reconcile-sessions configuration)))
              (human   (world-human-participant world))
              (agent-a (find ':agent (world-channel-residents world id-a) :key #'participant-kind)))
         (world-channel-create configuration
                               :id        "bdest11"
                               :owner     (participant-identifier human)
                               :workspace (world-tests--workspace root "b"))
         (labels ((running-child (name)
                    "Return a child agent NAME and the task that runs it, in a channel of its own."
                    (let* ((job  (task-tests--register-job (application--task-orchestrator app-a)
                                                           (application-agent app-a)
                                                           (acp-extension-job-tests--definition)
                                                           :name name))
                           (task (task-job-execution-identifier job)))
                      (world-channel-create configuration
                                            :id        task
                                            :owner     (participant-identifier agent-a)
                                            :workspace (world-tests--workspace root "a")
                                            :parent    id-a)
                      (values (world-participant-create configuration
                                                        :kind      ':agent
                                                        :name      name
                                                        :residence task
                                                        :parent    (participant-identifier agent-a))
                              job)))

                  (refusal (operation arguments)
                    "Return the message refusing OPERATION with ARGUMENTS, or NIL when it succeeds."
                    (handler-case (progn (localgroup-query-record entry operation arguments) nil)
                      (localgroup-error (condition)
                        (autolith-error-message condition))))

                  (residence (participant)
                    "Return where PARTICIPANT resides now."
                    (participant-residence
                     (world-participant-find (world-load configuration)
                                             (participant-identifier participant)))))
           (multiple-value-bind (child job) (running-child "child")
             (let ((grandchild (task-tests--register-job (application--task-orchestrator app-a)
                                                         (application-agent app-a)
                                                         (acp-extension-job-tests--definition)
                                                         :name              "grandchild"
                                                         :owner-identifiers (list (job-identifier job)))))
               (test-assert (and (null (refusal ':move (list :participant (participant-identifier child)
                                                             :channel     "bdest11")))
                                 (equal (residence child) "bdest11")
                                 (job-terminal-p job)
                                 (eq (job-cancellation-reason job) ':moved))
                            "moving a child whose task runs interrupts the task and moves the child")
               (test-assert (and (not (job-terminal-p grandchild))
                                 (null (job-cancellation-reason grandchild))
                                 (null (task-job--cancelled-ancestor-reason grandchild)))
                            "the task of the child's own child goes on, and may still start")))
           (multiple-value-bind (child job) (running-child "second")
             (test-assert (and (null (refusal ':dismiss (list :participant (participant-identifier child))))
                               (null (residence child))
                               (eq (job-cancellation-reason job) ':moved))
                          "dismissing a child whose task runs interrupts the task and parks the child")))))))
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
              (agent-b (find ':agent (world-channel-residents world id-b) :key #'participant-kind)))
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
           (query ':dismiss :participant (participant-identifier agent-b))
           (query ':move :participant (participant-identifier agent-a) :channel id-b)
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
               (localgroup-query-record entry ':close (list :channel id-d))
               (task-tests--wait-until (lambda () (null (world-server--find-host server id-d))) 20)
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
            (terminal     (terminal-ui-terminal (application-ui app-a)))
            (attachment   (make-instance 'image-daemon:attachment
                                         :socket nil :stream (make-broadcast-stream) :mode ':control))
            (picks        nil))
       (conversation-append-user-message (application-conversation app-a) "working in a")
       (conversation-append-user-message (application-conversation app-b) "working in b")
       (let* ((world (values (world-reconcile-sessions configuration)))
              (agent (find ':agent (world-channel-residents world id-b) :key #'participant-kind))
              (name  (participant-name agent)))
         (image-daemon:relay-attach terminal attachment :rows 24 :columns 100
                                                       :styled-p nil :session-id id-a)
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
                                         (format nil "~A executes while A keeps working" source))))
                   (dolist (syntax '(:lisp :slash))
                     (let ((before (length (world-server--hosts server))))
                       (submit (if (eq syntax ':lisp) "(new)" "/new"))
                       (test-assert (= (length (world-server--hosts server)) (1+ before))
                                    "new opens another hosted channel before A finishes"))
                     (submit (if (eq syntax ':lisp) (format nil "(switch ~S)" id-b)
                                 (format nil "/switch ~A" id-b)))
                     (test-assert (some (lambda (packet)
                                          (and (search ":SWITCH" (string-upcase packet)) (search id-b packet)))
                                        (coerce (structlisp:deque->vector (image-daemon:attachment-queue attachment)) 'list))
                                  "typed switch sends the client's attachment request while A works")
                     (submit (if (eq syntax ':lisp) (format nil "(dismiss ~S)" name)
                                 (format nil "/dismiss ~A" name)))
                     (test-assert (null (participant-residence
                                         (world-participant-find (world-load configuration) (participant-identifier agent))))
                                  "dismiss parks B's agent before A finishes")
                     (submit (if (eq syntax ':lisp) (format nil "(tell ~S ~S)" id-b "busy message")
                                 (format nil "/tell ~A ~S" id-b "busy message")))
                     (test-assert (some (lambda (delivery) (string= (world-delivery-text delivery) "busy message"))
                                        (world-inbox-deliveries configuration id-b))
                                  "tell reaches B's durable inbox before A finishes")
                     (submit (if (eq syntax ':lisp) (format nil "(move ~S ~S)" name id-b)
                                 (format nil "/move ~A ~A" name id-b)))
                     (test-assert (equal id-b (participant-residence
                                               (world-participant-find (world-load configuration) (participant-identifier agent))))
                                  "move places B's agent before A finishes"))
                   (submit "(switch)")
                   (submit "(tell)")
                   (test-assert (search id-b (line-editor-text (terminal-ui-editor (application-ui app-a))))
                                "tell's picker prefills the composer during inference")
                   (submit "(dismiss)")
                   (submit "(move)")
                   (test-assert (and (member ':switch picks) (member ':channel picks) (member ':agent picks))
                                "argument-free channel commands reach their pickers while A works")
                   (test-assert (equal id-b (participant-residence
                                             (world-participant-find (world-load configuration) (participant-identifier agent))))
                                "picker-driven move executes while A works"))))
           (application-input-controller-finish-external-work controller))))))
  nil)
