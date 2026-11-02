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
                        (:pristine-p boolean))
    cons)
(defun world-client-start-server
    (configuration &key (permission-mode ':ask) immutable-p pristine-p)
  "Start CONFIGURATION's world server and return its endpoint entry."
  (localgroup-handoff-spawn-fresh configuration
                                  :permission-mode permission-mode
                                  :immutable-p     immutable-p
                                  :pristine-p      pristine-p
                                  :world-p         t)
  (or (world-client-entry configuration)
      (error 'localgroup-error
             :message   "The world server started but its endpoint does not answer."
             :operation ':spawn)))

(defparameter *world-client-wait-seconds* 60
  "How long a client waits for a server that holds the world but does not answer yet.

A server publishes its endpoint again after a checkpoint, and a channel
opens once the checkpoint is over.")

(-> world-client--entry
    (configuration &key (:permission-mode keyword) (:immutable-p boolean) (:pristine-p boolean)
                        (:deadline real))
    cons)
(defun world-client--entry (configuration &key (permission-mode ':ask) immutable-p pristine-p
                                               deadline)
  "Return the world server's endpoint entry: the one answering, or a new server's.

A server that holds the world without answering, as during a checkpoint, is
waited for until DEADLINE, an internal real time, rather than doubled."
  (loop
    (let ((entry (world-client-entry configuration)))
      (cond
        (entry
         (return entry))
        ((not (world-server-running-p configuration))
         (return (world-client-start-server configuration
                                            :permission-mode permission-mode
                                            :immutable-p     immutable-p
                                            :pristine-p      pristine-p)))
        ((>= (get-internal-real-time) deadline)
         (error 'localgroup-error
                :message   "The world server holds the world but did not answer in time."
                :operation ':open))
        (t
         (sleep 0.25))))))

(-> world-client-open
    (configuration &key (:directory (or null string pathname)) (:channel (option string))
                        (:permission-mode keyword) (:immutable-p boolean)
                        (:pristine-p boolean) (:resume-command-p boolean)
                        (:recovery-diagnosis (option string)) (:launch list))
    (values string boolean))
(defun world-client-open
    (configuration &key directory channel (permission-mode ':ask) immutable-p pristine-p
                        resume-command-p recovery-diagnosis launch)
  "Open CHANNEL, or a channel for DIRECTORY, in CONFIGURATION's world server.

The first client starts the server; later ones find it running. LAUNCH is
the plist of launch options this start stated explicitly; a new channel
takes them, and a channel already served refuses the ones it cannot change.
Return the opened channel's session identifier, whose endpoint answers by
then, and whether the server started the channel for this open rather than
sharing one it already served."
  (let ((deadline (+ (get-internal-real-time)
                     (* *world-client-wait-seconds* internal-time-units-per-second))))
    (multiple-value-bind (rows columns)
        (terminal-current-size)
      (loop
        (let* ((entry    (world-client--entry configuration
                                              :permission-mode permission-mode
                                              :immutable-p     immutable-p
                                              :pristine-p      pristine-p
                                              :deadline        deadline))
               (response
                 (handler-case
                     (localgroup-query-record
                      entry ':open
                      (append (when channel
                                (list :channel channel))
                              (when directory
                                (list :directory (namestring directory)))
                              (when launch
                                (list :launch launch))
                              (list :permission-mode    permission-mode
                                    :fullscreen-p       (not (null (config :fullscreen-p configuration)))
                                    :resume-command-p   (not (null resume-command-p))
                                    :recovery-diagnosis recovery-diagnosis
                                    :rows               rows
                                    :columns            columns
                                    :styled-p           (not (null (terminal-environment-styling-p))))))
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
                (return (values identifier (not (null (getf (rest response) :started-p))))))
              (sleep 0.5)))))))


;;;; -- Command-Line Entry --

(-> world-client--print-absent (stream) null)
(defun world-client--print-absent (stream)
  "Say on STREAM that no world server runs."
  (localgroup--write-styled-line
   stream
   (list (terminal-span ':notice "No world server is running."))
   (localgroup--status-output-styled-p stream))
  nil)

(-> world-client--server-command () clingon:command)
(defun world-client--server-command ()
  "Return the localgroup server sub-command, describing the world server and its channels."
  (make-command
   :name        "server"
   :description "show the world server and the channels it hosts"
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message   "localgroup server accepts no arguments."
              :operation ':arguments))
     (let* ((configuration (localgroup--client-configuration))
            (entry         (world-client-entry configuration))
            (stream        *standard-output*))
       (if entry
           (let ((status   (getf (rest (localgroup-query-record entry ':status)) :status))
                 (channels (getf (rest (localgroup-query-record entry ':channels)) :channels)))
             (localgroup--write-styled-line
              stream
              (list (terminal-span ':notice "World server PID ")
                    (terminal-span ':success (princ-to-string (getf (rest status) :pid)))
                    (terminal-span ':notice
                                   (format nil ", Autolith ~A, ~D open channel~:P~:[~;, stopping~]."
                                           (getf (rest status) :autolith-version)
                                           (length (getf (rest status) :channels))
                                           (getf (rest status) :stopping-p))))
              (localgroup--status-output-styled-p stream))
             (when channels
               (localgroup-print-statuses channels :stream stream)))
           (world-client--print-absent stream))))))

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
           (localgroup--print-response (localgroup-query-record entry ':stop)
                                       *standard-output*)
           (world-client--print-absent *standard-output*))))))
