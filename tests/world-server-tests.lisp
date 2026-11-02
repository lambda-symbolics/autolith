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
  "Test checkpoint work running on the main thread with every channel quiesced."
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
            (session-a   (world-server-tests--session server id-a))
            (session-b   (world-server-tests--session server id-b))
            (observed    nil))
       (setf observed
             (world-server-call-quiesced
              server application
              (lambda ()
                (list (eq *active-application* application)
                      (application-localgroup-session application)
                      (application-localgroup-session (world-host-application host-b))
                      (sb-thread:thread-name (current-thread))))))
       (test-assert (and (first observed)
                         (null (second observed))
                         (null (third observed))
                         (string= (fourth observed) "Autolith world server test main"))
                    "checkpoint work runs on the main thread with every endpoint stopped")
       (test-assert (and (null *active-application*)
                         (world-host-live-p host-a)
                         (world-host-live-p host-b))
                    "the active application is cleared and the channels serve again")
       (let ((restarted-a (world-server-tests--session server id-a))
             (restarted-b (world-server-tests--session server id-b)))
         (test-assert (and (not (eq restarted-a session-a))
                           (not (eq restarted-b session-b))
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
                     (world-server-call-quiesced server application (lambda () t))
                     nil)
                 (checkpoint-error (condition)
                   (search "is working" (autolith-error-message condition))))
               "a working channel refuses another channel's checkpoint")
           (application-input-controller-finish-external-work controller)))
       (test-assert (handler-case
                        (progn
                          (world-server-call-quiesced server application
                                                      (lambda () (error "injected")))
                          nil)
                      (simple-error (condition)
                        (search "injected" (format nil "~A" condition))))
                    "a failing checkpoint reaches the requesting channel")
       (test-assert (and (world-host-live-p host-a) (world-host-live-p host-b))
                    "a failed checkpoint leaves the channels serving"))))
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
