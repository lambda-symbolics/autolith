(in-package #:autolith)

;;;; -- World Client --

;;; A terminal is a client of the world server. It finds the server through
;;; the world endpoint record, starts the server when none answers, asks it
;;; to open a channel, and attaches to that channel's endpoint like any
;;; localgroup client. The server outlives the terminal.

(-> world-client-entry (configuration) (option cons))
(defun world-client-entry (configuration)
  "Return the reachable world endpoint entry of CONFIGURATION, or NIL.

Records of dead processes are removed on the way."
  (let ((directory (localgroup-world-directory configuration)))
    (image-daemon:daemon-registry-reconcile directory)
    (loop for entry in (image-daemon:daemon-registry-discover directory)
          when (handler-case
                   (localgroup-query-record entry ':status)
                 (localgroup-error ()
                   nil))
            return entry)))

(-> world-client-start-server
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:attach-expected-p boolean))
    cons)
(defun world-client-start-server
    (configuration &key (permission-mode ':ask) immutable-p (attach-expected-p t))
  "Start CONFIGURATION's world server and return its endpoint entry.

Without ATTACH-EXPECTED-P no terminal is about to attach, as for lg start."
  (localgroup-handoff-spawn-fresh configuration
                                  :permission-mode   permission-mode
                                  :immutable-p       immutable-p
                                  :attach-expected-p attach-expected-p
                                  :world-p           t)
  (or (world-client-entry configuration)
      (error 'localgroup-error
             :message   "The world server started but its endpoint does not answer."
             :operation ':spawn)))

(defparameter *world-client-wait-seconds* 60
  "How long a client waits for a server that holds the world but does not answer yet.

A server publishes its endpoint again after a checkpoint, and a channel
opens once the checkpoint is over.")

(-> world-client--entry
    (configuration &key (:permission-mode keyword) (:immutable-p boolean) (:deadline real))
    cons)
(defun world-client--entry (configuration &key (permission-mode ':ask) immutable-p deadline)
  "Return the world server's endpoint entry: the one answering, or a new server's.

A server that holds the world without answering, as during a checkpoint, is
waited for until DEADLINE, an internal real time, rather than doubled. Two
clients that find no server start one each; the launch that loses the
world's lease fails, and its client then waits for the winner like any
other."
  (loop
    (let ((entry (world-client-entry configuration)))
      (cond
        (entry
         (return entry))
        ((not (world-server-running-p configuration))
         (handler-case
             (return (world-client-start-server configuration
                                                :permission-mode permission-mode
                                                :immutable-p     immutable-p))
           (localgroup-error (condition)
             ;; Another client's server took the lease meanwhile: join it.
             (unless (world-server-running-p configuration)
               (error condition)))))
        ((>= (get-internal-real-time) deadline)
         (error 'localgroup-error
                :message   "World server response timeout"
                :operation ':open))
        (t
         (sleep 0.25))))))

(-> world-client-open
    (configuration &key (:directory (or null string pathname)) (:channel (option string))
                        (:permission-mode keyword) (:immutable-p boolean)
                        (:pick-channel-p boolean)
                        (:recovery-diagnosis (option string)) (:launch list) (:new-p boolean)
                        (:images list) (:mcp-servers list) (:switch-p boolean))
    (values string boolean))
(defun world-client-open
    (configuration &key directory channel (permission-mode ':ask) immutable-p
                        pick-channel-p recovery-diagnosis launch new-p images mcp-servers
                        switch-p)
  "Open CHANNEL, or a channel for DIRECTORY, in CONFIGURATION's world server.

NEW-P opens a new channel for DIRECTORY instead of its latest one. IMAGES,
absolute namestrings of local images, become the opened channel's composer
draft. MCP-SERVERS, an editor's server declarations in portable form, join
the tools of a channel that starts for this open. SWITCH-P says a terminal
moves here from another channel, so a channel that starts for it skips the
boot screen.
The first client starts the server; later ones find it running. LAUNCH is
the plist of launch options this start stated explicitly; a new channel
takes them, and a channel already served refuses the ones it cannot change.
Return the opened channel's session identifier, whose endpoint answers by
then, and whether the server started the channel for this open rather than
sharing one it already served."
  (channel-loading-report 1 "Connecting to world")
  (let ((deadline (+ (get-internal-real-time)
                     (* *world-client-wait-seconds* internal-time-units-per-second))))
    (multiple-value-bind (rows columns)
        (terminal-current-size)
      (loop
        (let* ((entry    (world-client--entry configuration
                                              :permission-mode permission-mode
                                              :immutable-p     immutable-p
                                              :deadline        deadline))
               (response
                 (handler-case
                     ;; Wait for the answer instead of sending the open again.
                     ;; The first open can be slow because it migrates every
                     ;; conversation, and a second open could start another channel.
                     (let ((image-daemon:*daemon-connect-timeout-seconds*
                             (max image-daemon:*daemon-connect-timeout-seconds*
                                  (/ (- deadline (get-internal-real-time))
                                     internal-time-units-per-second))))
                       (channel-loading-report 2 "Selecting an available channel")
                       (localgroup-query-record
                        entry ':open
                        (append (when channel
                                  (list :channel channel))
                                (when directory
                                  (list :directory (namestring directory)))
                                (when launch
                                  (list :launch launch))
                                (when new-p
                                  (list :new-p t))
                                (when images
                                  (list :images images))
                                (when mcp-servers
                                  (list :mcp-servers mcp-servers))
                                (when switch-p
                                  (list :switch-p t))
                                (list :progress-p         t
                                      :permission-mode    permission-mode
                                      :fullscreen-p       (not (null (config :fullscreen-p configuration)))
                                      :pick-channel-p     (not (null pick-channel-p))
                                      :recovery-diagnosis recovery-diagnosis
                                      :rows               rows
                                      :columns            columns
                                      :styled-p           (not (null (terminal-environment-styling-p)))))))
                   (localgroup-error (condition)
                     ;; A checkpoint refuses opens until the channels resume,
                     ;; and drops connections while the endpoint is away.
                     (if (and (< (get-internal-real-time) deadline)
                              (or (localgroup-error-retry-p condition)
                                  (and (image-daemon:daemon-error-cause condition)
                                       (world-server-running-p configuration))))
                         nil
                         (error condition))))))
          (if response
              (let ((identifier (getf (rest response) :session-id)))
                (unless (non-empty-string-p identifier)
                  (error 'localgroup-error
                         :message   "The world server did not name the opened channel."
                         :operation ':open))
                (channel-loading-report 6 "Attaching terminal")
                (return (values identifier (not (null (getf (rest response) :started-p))))))
              (sleep 0.5)))))))


;;;; -- Scripted Input --

;;; A start whose standard input is not a terminal reads its input from it:
;;; each line is one submission, and a line opening a Lisp form continues
;;; until the form closes. The channel runs each submission to its end before
;;; the next, and the transcript goes to standard output as plain text.

(defclass script-channel-subscription (channel-subscription)
  ((output :initarg :output :reader script-channel-subscription-output :type stream
           :documentation "The plain transcript destination."))
  (:documentation "A plain-text client with shared channel lifecycle and submission admission."))

(-> world-client--next-submission (stream) (option string))
(defun world-client--next-submission (stream)
  "Read the next submission from STREAM, or return NIL at its end.

Blank lines submit nothing. A line that opens a Lisp form takes the lines
after it until the form closes or the input ends."
  (loop
    (let ((line (read-line stream nil nil)))
      (cond
        ((null line)
         (return nil))
        ((zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
         nil)
        (t
         (let ((text line))
            (loop while (application-lisp-input-incomplete-p text)
                 do (let ((next (read-line stream nil nil)))
                      (unless next
                        (return))
                      (setf text (format nil "~A~%~A" text next))))
           (return (string-right-trim '(#\Return) text))))))))

(-> world-client--script-event (list stream) null)
(defun world-client--script-event (event output)
  "Write the part of channel EVENT a plain transcript shows to OUTPUT."
  (let ((fields (rest event)))
    (case (first event)
      (:present
       (let ((text (getf fields :text)))
         (when (stringp text)
           (fresh-line output)
           (write-string text output)
           (fresh-line output))))
      (:text
       (let ((delta (getf fields :delta)))
         (when (stringp delta)
           (write-string delta output))))
      (:status
       (case (getf fields :status)
         (:assistant-response-persisted
          (fresh-line output))
         (:tool-call-started
          (fresh-line output)
          (format output "● ~A~%" (getf (getf fields :details) :tool))))))
    (force-output output))
  nil)

(defmethod channel-subscription-present-event ((subscription script-channel-subscription) event)
  "Render one ordered channel event before acknowledging it."
  (world-client--script-event event (script-channel-subscription-output subscription)))

(-> world-client-run-script
    (configuration &key (:input stream) (:output stream) (:conversation-id (option string))
                        (:permission-mode keyword) (:immutable-p boolean) (:launch list))
    null)
(defun world-client-run-script
    (configuration &key (input *standard-input*) (output *standard-output*) conversation-id
                        (permission-mode ':ask) immutable-p launch)
  "Run INPUT's submissions, waiting for each submission's own completion."
  (multiple-value-bind (channel started-p)
      (world-client-open configuration :directory       (config :working-directory configuration)
                                       :channel         conversation-id
                                       :new-p           (null conversation-id)
                                       :permission-mode permission-mode
                                       :immutable-p     immutable-p
                                       :launch          launch)
    (let ((subscription (make-instance 'script-channel-subscription
                                       :identifier    channel
                                       :configuration configuration
                                       :started-p     started-p
                                       :name          "script"
                                       :approves-p    nil
                                       :output        output)))
      (unwind-protect
           (progn
             (channel-subscription-attach subscription)
             (loop for text = (world-client--next-submission input)
                   while text
                   do (with-lock-held ((channel-subscription-event-lock subscription))
                        (fresh-line output)
                        (format output "› ~A~%" text)
                        (force-output output))
                     (channel-subscription-await-submission subscription text)))
        (channel-subscription-close subscription))))
  nil)


;;;; -- Command-Line Entry --

(-> world-client--print-absent (stream) null)
(defun world-client--print-absent (stream)
  "Report on STREAM that no world server is reachable."
  (localgroup--write-styled-line
   stream
   (list (terminal-span ':notice "· no world server is running"))
   (localgroup--status-output-styled-p stream))
  nil)

(-> world-client-print-server (configuration stream) null)
(defun world-client-print-server (configuration stream)
  "Write one line about CONFIGURATION's world server on STREAM: its process, version and open channels.

Nothing is written while no server runs."
  (let ((entry (world-client-entry configuration)))
    (when entry
      (let ((status (getf (rest (localgroup-query-record entry ':status)) :status)))
        (localgroup--write-styled-line
         stream
         (list (terminal-span ':notice "· world server PID ")
               (terminal-span ':success (princ-to-string (getf (rest status) :pid)))
               (terminal-span ':notice
                              (format nil ", Autolith ~A, ~D open channel~:P~:[~;, stopping~]"
                                      (getf (rest status) :autolith-version)
                                      (length (getf (rest status) :channels))
                                      (getf (rest status) :stopping-p))))
         (localgroup--status-output-styled-p stream)))))
  nil)

(-> world-client--start-command () clingon:command)
(defun world-client--start-command ()
  "Return the localgroup start sub-command, starting the world server in the background."
  (make-command
   :name        "start"
   :description "start the world server in the background"
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message   "localgroup start accepts no arguments."
              :operation ':arguments))
     (let ((configuration (localgroup--client-configuration)))
       (unless (world-client-entry configuration)
         (world-client-start-server configuration :attach-expected-p nil))
       (world-client-print-server configuration *standard-output*)))))

(-> world-client--tell-command () clingon:command)
(defun world-client--tell-command ()
  "Return the localgroup tell sub-command, sending a channel a message as the person."
  (make-command
   :name        "tell"
   :description "send a channel a message, as you would with (tell)"
   :usage       "CHANNEL MESSAGE"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (= (length arguments) 2)
         (error 'localgroup-error
                :message   "localgroup tell takes the channel and one quoted message."
                :operation ':arguments))
       (let* ((configuration (localgroup--client-configuration))
              (entry         (or (world-client-entry configuration)
                                 (error 'localgroup-error
                                        :message   "No world server is running; start it with autolith lg start."
                                        :operation ':tell)))
              (answer        (rest (localgroup-query-record
                                    entry ':tell (list :channel (first arguments)
                                                       :text    (second arguments))))))
         (localgroup--write-styled-line
          *standard-output*
          (list (terminal-span ':notice
                               (format nil "· sent to ~A"
                                       (conversation-identifier-display (getf answer :channel)))))
          (localgroup--status-output-styled-p *standard-output*)))))))

(defparameter *world-client-stop-timeout* 60
  "Seconds to wait for the recorded world server process to exit after accepting stop.")

(-> world-client--stop-server (cons stream) null)
(defun world-client--stop-server (entry stream)
  "Ask ENTRY to stop, report progress on STREAM, and confirm its process has exited."
  (let ((pid (getf (rest (rest entry)) :pid)))
    (localgroup--print-response (localgroup-query-record entry ':stop) stream)
    (force-output stream)
    (let ((*localgroup-kill-exit-timeout* *world-client-stop-timeout*))
      (case (localgroup--observe-kill-exit pid)
        (:dead
         (localgroup--write-styled-line
          stream
          (list (terminal-span ':success "· world server stopped"))
          (localgroup--status-output-styled-p stream))
         (force-output stream))
        (:alive
         (error 'localgroup-error
                :operation ':stop
                :message   (format nil "Could not confirm world server exit: recorded PID ~D is still present (possibly reused)."
                                   pid)))
        (otherwise
         (error 'localgroup-error
                :operation ':stop
                :message   (format nil "Could not confirm world server exit for recorded PID ~S."
                                   pid))))))
  nil)

(-> world-client--stop-command () clingon:command)
(defun world-client--stop-command ()
  "Return the localgroup stop sub-command, stopping the world server and its channels."
  (make-command
   :name        "stop"
   :description "stop the world server and every channel it hosts"
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message   "localgroup stop accepts no arguments."
              :operation ':arguments))
     (let* ((configuration (localgroup--client-configuration))
            (entry         (world-client-entry configuration)))
       (if entry
            (world-client--stop-server entry *standard-output*)
           (world-client--print-absent *standard-output*))))))
