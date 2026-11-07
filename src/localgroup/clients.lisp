(in-package #:autolith)

;;;; -- Structured Client Protocol --

;;; A structured client opens a channel's localgroup endpoint with the :client
;;; operation, naming itself with :name and saying with :approves-p whether
;;; it answers approvals, and keeps the connection. The server answers with
;;;
;;;   (:attached :client-id ID :session-id CHANNEL :state STATE)
;;;
;;; and from then on writes the channel's events: (:record :record RECORD),
;;; (:text :delta TEXT), (:reasoning :delta TEXT), (:status :status STATUS
;;; :details DETAILS), (:present :text TEXT), (:approval :id ID :kind KIND
;;; :title TITLE :entry TEXT :options OPTIONS), (:approval-settled :id ID
;;; :value CHOICE), (:work-finished :kind KIND :outcome OUTCOME :idle-p
;;; IDLE-P) and (:state :state STATE). The client writes commands, each with
;;; an :id it chooses:
;;;
;;;   (:submit :id N :text TEXT [:images PATHNAMES] [:steer-p BOOLEAN])
;;;   (:cancel :id N)
;;;   (:answer :id N :approval ID :value CHOICE)
;;;   (:mode :id N :value MODE)
;;;   (:state :id N)
;;;   (:operation :id N :method METHOD :params PARAMS)
;;;
;;; and the server answers each with (:reply :id N :ok T ...) or
;;; (:reply :id N :error MESSAGE), in order with the events. An operation
;;; runs on a thread of its own, since it may wait for the channel while the
;;; channel waits for the client; its reply may come after later ones. A submit's reply
;;; carries :BUSY-P, whether the channel has work after it: when it does not,
;;; the submission ran at once or its work already finished, and its
;;; :WORK-FINISHED event came before the reply.

(defparameter *localgroup-client-permission-modes* '(:ask :auto :sandboxed :full-access)
  "The permission modes a structured client may select for its channel.")


;;;; -- Serving --

(-> localgroup--serve-client (localgroup-session list &key (:socket t) (:stream stream)) null)
(defun localgroup--serve-client (session request &key socket stream)
  "Serve REQUEST's structured client on STREAM until it disconnects."
  (unless (localgroup--valid-request-p request session)
    (daemon-write-packet stream (list :error :message "The localgroup request was rejected."))
    (return-from localgroup--serve-client nil))
  (let* ((application (localgroup-session-application session))
         (arguments   (localgroup--request-field request :arguments))
         (name        (getf arguments :name))
         (client      (make-instance 'channel-client
                                     :name           (if (non-empty-string-p name) name "client")
                                     :approves-p     (getf arguments :approves-p t)
                                     :stream         stream
                                     :socket         socket
                                     :transport-lock (image-daemon:daemon-runtime-lock session))))
    (unwind-protect
         (progn
           (channel-client-enqueue client
                                   (client-portable
                                    (list :attached
                                          :client-id  (channel-client-identifier client)
                                          :session-id (image-daemon:daemon-runtime-identifier session)
                                          :state      (localgroup-status-snapshot session))))
           (application-clients-add application client)
           (loop for packet = (handler-case (daemon-read-packet stream)
                                (error ()
                                  nil))
                 while (and packet (not (channel-client-closed-p client)))
                 do (let* ((command packet)
                           (reply   (lambda ()
                                      (channel-client-enqueue client
                                                              (localgroup--client-reply session command)))))
                      ;; Operations may wait for an approval answered on this connection.
                      (if (eq (first command) ':operation)
                          (make-thread reply :name "Autolith client operation")
                          (funcall reply)))))
      (channel-client-close client)
      (let ((writer (channel-client-writer-thread client)))
        (when writer
          (join-thread writer)))))
  nil)

(-> localgroup--client-reply (localgroup-session list) list)
(defun localgroup--client-reply (session packet)
  "Run the client command PACKET in SESSION's channel and return its reply."
  (let ((identifier (getf (rest packet) :id)))
    (handler-case
        (client-portable
         (list* :reply :id identifier :ok t
                (localgroup--client-command session (first packet) (rest packet))))
      (error (condition)
        (list :reply        :id identifier
                     :error (bounded-string (princ-to-string condition) :limit 1000))))))

(-> localgroup--client-command (localgroup-session t list) list)
(defun localgroup--client-command (session command arguments)
  "Run client COMMAND with ARGUMENTS in SESSION's channel and return the reply's extra fields."
  (let* ((application (localgroup-session-application session))
         (controller  (application-input-controller application)))
    (flet ((refuse (message)
             (error 'localgroup-error
                    :message    message
                    :operation  ':client
                    :session-id (image-daemon:daemon-runtime-identifier session))))
      (case command
        (:submit
         (let ((text   (getf arguments :text))
               (images (getf arguments :images)))
           (unless (and controller (stringp text) (listp images) (every #'stringp images))
             (refuse "A submit carries text and optional image pathnames."))
           (when (application-localgroup-handoff-pending-p application)
             (refuse "The channel is detaching and no longer accepts input."))
           (application-localgroup-resume application)
           (application-input-controller--handle-submission
            controller
            (user-message-input-create
             :text            text
             :image-pathnames (mapcar #'image-input-validate-pathname images))
            :steer-p (not (null (getf arguments :steer-p))))
           (list :busy-p (application-input-controller-busy-p controller))))
        (:cancel
         (list :accepted-p
               (and controller
                    (not (null (application-input-controller--request-active-turn-cancellation
                                controller))))))
        (:answer
         (let ((approval (getf arguments :approval))
               (value    (getf arguments :value)))
           (unless (and (stringp approval) (stringp value))
             (refuse "An answer names the approval and the chosen value."))
           (list :accepted-p (application-answer-approval application approval value))))
        (:mode
         (let ((mode (getf arguments :value)))
           (unless (member mode *localgroup-client-permission-modes*)
             (refuse (format nil "The permission mode must be one of ~{~(~A~)~^, ~}."
                             *localgroup-client-permission-modes*)))
           (when (and (eq mode ':sandboxed)
                      (not (application--command-sandbox-available-p)))
             (refuse (application--command-sandbox-unavailable-message)))
           (setf (application-permission-mode application) mode)
           (application-clients-publish-state application)
           nil))
        (:state
         (list :state (localgroup-status-snapshot session)))
        (:operation
         (let ((method (getf arguments :method))
               (params (client-json (getf arguments :params))))
           (unless (stringp method)
             (refuse "An operation names its method."))
           (list :result (acp-extension-run application method
                                            (if (hash-table-p params) params (json-object))))))
        (otherwise
         (refuse (format nil "Unknown client command ~S." command)))))))


;;;; -- State --

(-> application-clients-publish-state (application) null)
(defun application-clients-publish-state (application)
  "Send APPLICATION's channel state to its structured clients, as after a mode or setting change."
  (let ((session (application-localgroup-session application)))
    (when session
      (application-clients-publish application
                                   (list :state :state (localgroup-status-snapshot session)))))
  nil)


;;;; -- Connecting --

(defclass localgroup-client-connection ()
  ((socket
    :initarg       :socket
    :reader        localgroup-client-connection-socket
    :documentation "The socket of the connection.")
   (stream
    :initarg       :stream
    :reader        localgroup-client-connection-stream
    :type          stream
    :documentation "The stream commands are written to and events read from.")
   (lock
    :initform      (make-lock "Autolith client connection")
    :reader        localgroup-client-connection-lock
    :documentation "The lock guarding writes, the next command identifier and the replies.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith client connection")
    :reader        localgroup-client-connection-condition-variable
    :documentation "Signaled when a reply arrives or the connection ends.")
   (next-identifier
    :initform      0
    :accessor      localgroup-client-connection-next-identifier
    :type          (integer 0)
    :documentation "The identifier the next command takes.")
   (replies
    :initform      (make-hash-table)
    :reader        localgroup-client-connection-replies
    :documentation "Replies that arrived for commands still waited on, by identifier.")
   (closed-p
    :initform      nil
    :accessor      localgroup-client-connection-closed-p
    :type          boolean
    :documentation "Whether the connection ended.")
   (attached
    :initarg       :attached
    :reader        localgroup-client-connection-attached
    :type          list
    :documentation "The server's (:attached ...) greeting.")
   (reader-thread
    :initform      nil
    :accessor      localgroup-client-connection-reader-thread
    :documentation "The thread reading events and replies."))
  (:documentation "A structured client's connection to one channel."))

(-> localgroup-client-connect
    (cons &key (:name string) (:event-function function) (:approves-p boolean))
    localgroup-client-connection)
(defun localgroup-client-connect (entry &key (name "client") event-function (approves-p t))
  "Connect to the channel of localgroup endpoint ENTRY as a structured client NAME.

EVENT-FUNCTION is called on the reader thread with each event, and with
(:closed) once the connection ends. Replies go to the command that waits
for them. APPROVES-P says whether this client answers approvals; one that
does not leaves them to a terminal or another client."
  (let ((record (rest entry))
        (socket nil)
        (stream nil))
    (multiple-value-setq (socket stream)
      (daemon-connect (localgroup--record-port record)))
    (handler-case
        (progn
          (daemon-write-packet stream
                               (list :localgroup-request
                                     :version   *daemon-protocol-version*
                                     :token     (localgroup--record-token record)
                                     :operation ':client
                                     :arguments (list :name name :approves-p approves-p)))
          (let ((greeting (daemon-read-response stream ':client)))
            (unless (eq (first greeting) ':attached)
              (error 'localgroup-error
                     :message    (or (getf (rest greeting) :message)
                                     "The channel did not accept the client.")
                     :operation  ':client
                     :session-id (localgroup--record-session-id record)))
            (let ((connection (make-instance 'localgroup-client-connection
                                             :socket   socket
                                             :stream   stream
                                             :attached greeting)))
              (setf (localgroup-client-connection-reader-thread connection)
                    (make-thread (lambda ()
                                   (localgroup-client--read connection event-function))
                                 :name (format nil "Autolith ~A client reader" name)))
              connection)))
      (error (condition)
        ; mallet:suppress no-ignore-errors -- closing a failed connection must not mask its cause
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error condition)))))

(-> localgroup-client-send (localgroup-client-connection keyword &rest t) integer)
(defun localgroup-client-send (connection command &rest arguments)
  "Write client COMMAND with ARGUMENTS on CONNECTION and return the identifier its reply carries."
  (with-lock-held ((localgroup-client-connection-lock connection))
    (when (localgroup-client-connection-closed-p connection)
      (error 'localgroup-error
             :message   "The client connection is closed."
             :operation ':client))
    (let ((identifier (incf (localgroup-client-connection-next-identifier connection))))
      (daemon-write-packet (localgroup-client-connection-stream connection)
                           (list* command :id identifier arguments))
      identifier)))

(-> localgroup-client-call (localgroup-client-connection keyword &rest t) list)
(defun localgroup-client-call (connection command &rest arguments)
  "Send client COMMAND with ARGUMENTS on CONNECTION and return the reply's fields.

A reply carrying an error signals it as a localgroup error."
  (let ((identifier (apply #'localgroup-client-send connection command arguments)))
    (with-lock-held ((localgroup-client-connection-lock connection))
      (loop
        (let ((reply (gethash identifier (localgroup-client-connection-replies connection))))
          (cond
            (reply
             (remhash identifier (localgroup-client-connection-replies connection))
             (when (getf (rest reply) :error)
               (error 'localgroup-error
                      :message   (getf (rest reply) :error)
                      :operation ':client))
             (return (rest reply)))
            ((localgroup-client-connection-closed-p connection)
             (error 'localgroup-error
                    :message   "The client connection closed before the reply arrived."
                    :operation ':client))
            (t
             (condition-wait (localgroup-client-connection-condition-variable connection)
                             (localgroup-client-connection-lock connection)))))))))

(-> localgroup-client-close (localgroup-client-connection) null)
(defun localgroup-client-close (connection)
  "Close CONNECTION and wait for its reader to end.

Closing a socket does not wake a thread blocked reading it, and a close
right after a shutdown races that wakeup, so this only shuts the socket down;
the reader closes it once it stops."
  (with-lock-held ((localgroup-client-connection-lock connection))
    (setf (localgroup-client-connection-closed-p connection) t))
  ; mallet:suppress no-ignore-errors -- the server may have closed the connection already
  (ignore-errors (sb-bsd-sockets:socket-shutdown (localgroup-client-connection-socket connection)
                                                 :direction ':io))
  (let ((thread (localgroup-client-connection-reader-thread connection)))
    (when (and thread (not (eq thread (current-thread))))
      (join-thread thread)))
  nil)

(-> localgroup-client--read (localgroup-client-connection (option function)) null)
(defun localgroup-client--read (connection event-function)
  "Read CONNECTION's events and replies until it ends, close its socket, then report (:closed)."
  (unwind-protect
       (loop for packet = (handler-case (daemon-read-packet (localgroup-client-connection-stream connection))
                            (error ()
                              nil))
             while packet
             do (if (eq (first packet) ':reply)
                    (with-lock-held ((localgroup-client-connection-lock connection))
                      (setf (gethash (getf (rest packet) :id)
                                     (localgroup-client-connection-replies connection))
                            packet)
                      (sb-thread:condition-broadcast (localgroup-client-connection-condition-variable connection)))
                    (when event-function
                      (funcall event-function packet))))
    (with-lock-held ((localgroup-client-connection-lock connection))
      (setf (localgroup-client-connection-closed-p connection) t)
      (sb-thread:condition-broadcast (localgroup-client-connection-condition-variable connection)))
    ; mallet:suppress no-ignore-errors -- the server may have closed the connection already
    (ignore-errors (sb-bsd-sockets:socket-close (localgroup-client-connection-socket connection)))
    (when event-function
      (funcall event-function (list :closed))))
  nil)
