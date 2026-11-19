(in-package #:autolith)

;;;; -- Structured Client Protocol --

;;; A structured client opens a channel's localgroup endpoint with the :client
;;; operation, naming itself with :name and saying with :approves-p whether
;;; it answers approvals and with :evaluates-p whether it evaluates Common
;;; Lisp forms, and keeps the connection. The server answers with
;;;
;;;   (:attached :client-id ID :session-id CHANNEL :state STATE)
;;;
;;; and from then on writes the channel's events: (:record :record RECORD),
;;; (:text :delta TEXT), (:reasoning :delta TEXT), (:status :status STATUS
;;; :details DETAILS), (:present :text TEXT), (:approval :id ID :kind KIND
;;; :title TITLE :entry TEXT :options OPTIONS), (:approval-settled :id ID
;;; :value CHOICE), (:work-finished :kind KIND :outcome OUTCOME :idle-p
;;; IDLE-P) and (:state :state STATE). A client that evaluates is also sent
;;; (:evaluation :id ID :forms SOURCES [:restart NAME] [:restart-value
;;; SOURCE]). The client writes commands, each with an :id it chooses:
;;;
;;;   (:submit :id N :text TEXT [:images PATHNAMES] [:steer-p BOOLEAN])
;;;   (:cancel :id N)
;;;   (:answer :id N :approval ID :value CHOICE)
;;;   (:evaluated :id N :evaluation ID :succeeded-p BOOLEAN :text TEXT)
;;;   (:mode :id N :value MODE)
;;;   (:state :id N)
;;;   (:operation :id N :method METHOD :params PARAMS)
;;;
;;; A client that says :presents-p when it attaches, with the :rows and
;;; :columns of its pane, renders the channel itself: it receives the
;;; channel's (:ui :operation OPERATION ...) presentation events, reads its
;;; own keys, and also writes
;;;
;;;   (:input :id N :action ACTION [:text TEXT] [:images PATHNAMES])
;;;   (:event :id N :event EVENT), a key for the channel's open picker
;;;   (:geometry :id N :rows ROWS :columns COLUMNS)
;;;   (:input-history :id N :start START :limit LIMIT)
;;;
;;; and the server answers each with (:reply :id N :ok T ...) or
;;; (:reply :id N :error MESSAGE), in order with the events. An operation
;;; runs on a thread of its own, since it may wait for the channel while the
;;; channel waits for the client; its reply may come after later ones. A submit's reply
;;; carries :BUSY-P, whether the channel has work after it: when it does not,
;;; the submission ran at once or its work already finished, and its
;;; :WORK-FINISHED event came before the reply.
;;;
;;; Client and server may be of different versions. Each side reads the
;;; fields it knows and ignores the rest, a command the server does not know
;;; gets an error reply, an event the client does not know is its to ignore,
;;; and a packet that cannot be decoded at all is skipped. None of these ends
;;; the connection.

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
         (client      (channel-client-create
                       :name                 (if (non-empty-string-p name) name "client")
                       :approves-p           (getf arguments :approves-p t)
                       :evaluates-p          (not (null (getf arguments :evaluates-p)))
                       :cancels-evaluation-p (eq (getf arguments :cancels-evaluation-p) t)
                       :presents-p           (not (null (getf arguments :presents-p)))
                       :pid                  (let ((pid (getf arguments :client-pid)))
                                               (and (typep pid '(integer 1)) pid))
                       :stream               stream
                       :socket               socket
                       :transport-lock       (image-daemon:daemon-runtime-lock session)))
         (inputs      (make-instance 'client-input-queue)))
    (unwind-protect
         (progn
           (when (channel-client-presents-p client)
             (localgroup--client-geometry application arguments))
           (handler-case
               (application-clients-add
                application client
                :after-sequence (getf arguments :after-sequence)
                :attached       (list :attached
                                      :client-id      (channel-client-identifier client)
                                      :session-id     (image-daemon:daemon-runtime-identifier session)
                                      :server-version *autolith-version*
                                      :state          (localgroup-status-snapshot session)))
             (localgroup-error (condition)
              (daemon-write-packet stream (list :error  :message (autolith-error-message condition)
                                               :retry-p (localgroup-error-retry-p condition)))
              (return-from localgroup--serve-client nil)))
           (application-clients-note-controller application client)
           (when (channel-client-presents-p client)
             ;; A terminal shows the channel from now on.
             (localgroup--note-controller-attached session)
             (application-sync-window-title application)
             (application-world-attached application))
           ;; A client of another version may send what this one cannot read:
           ;; an undecodable packet is skipped and a malformed command dropped.
           (loop for command = (handler-case (daemon-read-decodable-packet stream)
                                 (error ()
                                   nil))
                 while (and command (not (channel-client-closed-p client)))
                 do (when (localgroup-message-p command)
                      (application-clients-note-controller application client)
                      (localgroup--run-client-command session command
                                                      :client client
                                                      :inputs inputs))))
      (client-input-queue-close inputs)
      (channel-client-close client)))
  nil)

(-> localgroup--queued-input-p (list) boolean)
(defun localgroup--queued-input-p (command)
  "Return true when client COMMAND is input that waits its turn: every action but cancelling."
  (and (eq (first command) ':input)
       (not (member (getf (rest command) :action) '(:interrupt :escape)))))

(-> localgroup--client-geometry (application list) null)
(defun localgroup--client-geometry (application arguments)
  "Lay APPLICATION's presentation out for a client whose pane has ARGUMENTS' :ROWS and :COLUMNS."
  (let ((presentation (application-presentation application)))
    (when presentation
      (channel-presentation-set-geometry presentation
                                         :rows     (getf arguments :rows)
                                         :columns  (getf arguments :columns)
                                         :styled-p (getf arguments :styled-p t))))
  nil)


;;;; -- Ordered Input --

(defclass client-input-queue ()
  ((lock
    :initform      (make-lock "Autolith client input")
    :reader        client-input-queue-lock
    :documentation "The lock guarding the waiting input.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith client input")
    :reader        client-input-queue-condition-variable
    :documentation "Signaled when input arrives or the queue closes.")
   (functions
    :initform      (make-deque)
    :reader        client-input-queue-functions
    :documentation "The input not yet run, oldest first.")
   (thread
    :initform      nil
    :accessor      client-input-queue-thread
    :documentation "The thread running the input, started with the first one.")
   (closed-p
    :initform      nil
    :accessor      client-input-queue-closed-p
    :type          boolean
    :documentation "Whether the client left."))
  (:documentation "One client's input, run in the order it arrived on a thread beside the connection's reader."))

(-> client-input-queue--run (client-input-queue) null)
(defun client-input-queue--run (queue)
  "Run QUEUE's input in order until its client leaves."
  (loop
    (let ((function (with-lock-held ((client-input-queue-lock queue))
                      (loop
                        (cond
                          ((client-input-queue-closed-p queue)
                           (return nil))
                          ((not (deque-empty-p (client-input-queue-functions queue)))
                           (return (deque-pop-front (client-input-queue-functions queue))))
                          (t
                           (condition-wait (client-input-queue-condition-variable queue)
                                           (client-input-queue-lock queue))))))))
      (unless function
        (return nil))
      (funcall function))))

(-> client-input-queue-push (client-input-queue function) null)
(defun client-input-queue-push (queue function)
  "Run FUNCTION after the input that arrived before it."
  (with-lock-held ((client-input-queue-lock queue))
    (unless (client-input-queue-closed-p queue)
      (deque-push-back (client-input-queue-functions queue) function)
      (unless (client-input-queue-thread queue)
        (setf (client-input-queue-thread queue)
              (make-thread (lambda () (client-input-queue--run queue))
                           :name "Autolith client input")))
      (condition-notify (client-input-queue-condition-variable queue))))
  nil)

(-> client-input-queue-close (client-input-queue) null)
(defun client-input-queue-close (queue)
  "Drop QUEUE's waiting input and let its thread end after what it runs now."
  (with-lock-held ((client-input-queue-lock queue))
    (setf (client-input-queue-closed-p queue) t)
    (deque-clear (client-input-queue-functions queue))
    (condition-notify (client-input-queue-condition-variable queue)))
  nil)

(-> localgroup--run-client-command
    (localgroup-session list &key (:client channel-client) (:inputs client-input-queue)) null)
(defun localgroup--run-client-command (session command &key client inputs)
  "Run COMMAND for CLIENT and queue its reply: at once, beside the reader, or on a thread of its own.

An operation may wait for an approval answered on this connection, so it gets
a thread. Input may run a command that asks this client a question, so it
runs beside the reader through INPUTS, in the order it arrived. Each command
is this call's own, however many the reader takes meanwhile."
  (flet ((reply ()
           (channel-client-enqueue
            client (localgroup--client-reply session command
                                             :attachment (channel-client-attachment client)))))
    (cond
      ((eq (first command) ':operation)
       (make-thread #'reply :name "Autolith client operation"))
      ((localgroup--queued-input-p command)
       (client-input-queue-push inputs #'reply))
      (t
       (reply))))
  nil)

(-> localgroup--client-reply (localgroup-session list &key (:attachment (option image-daemon:attachment))) list)
(defun localgroup--client-reply (session packet &key attachment)
  "Run the client command PACKET in SESSION's channel and return its reply."
  (let ((identifier (getf (rest packet) :id)))
    (handler-case
        (client-portable
         (list* :reply :id identifier :ok t
                 (localgroup--client-command session (first packet) (rest packet) :attachment attachment)))
      (error (condition)
        (list :reply        :id identifier
                     :error (bounded-string (princ-to-string condition) :limit 1000))))))

(-> localgroup--client-command
    (localgroup-session t list &key (:attachment (option image-daemon:attachment))) list)
(defun localgroup--client-command (session command arguments &key attachment)
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
           (application-localgroup-resume application)
           (if (and (not (non-empty-string-p text)) (null images))
               (application-clients-publish
                application (list :work-finished :kind ':submission
                                  :submission (getf arguments :submission) :outcome ':done))
               (application-input-controller--handle-submission
                controller
                (user-message-input-create
                 :text            text
                 :image-pathnames (mapcar #'image-input-validate-pathname images)
                 :submission      (getf arguments :submission))
                :steer-p (not (null (getf arguments :steer-p)))))
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
        (:evaluated
         (list :accepted-p
               (and (client-evaluation-reply-p (cons :evaluated arguments))
                    (application-answer-evaluation application (getf arguments :evaluation)
                                                   :attachment  attachment
                                                   :succeeded-p (getf arguments :succeeded-p)
                                                   :text        (getf arguments :text)))))
        (:mode
         (let ((mode (getf arguments :value)))
           (unless (member mode (application-client-permission-modes))
             (refuse (format nil "The permission mode must be one of ~{~(~A~)~^, ~}."
                             (application-client-permission-modes))))
           (let* ((operation (application-client-operation-find application "permissions"))
                  (input     (application-client-permission-input mode)))
             (application-client-operation-invoke
              application operation
              (application-client-command-invocation
               (application-operation-backend operation) input)))
           nil))
        (:state
         (list :state (localgroup-status-snapshot session)))
        (:input
         (let ((action (getf arguments :action))
               (text   (getf arguments :text))
               (images (getf arguments :images)))
           (unless (and controller (keywordp action) (typep text '(or null string))
                        (listp images) (every #'stringp images))
             (refuse "An input names its action and may carry text and image pathnames."))
           (application-localgroup-resume application)
           (list :handled-p
                 (not (null (input-port-apply
                             controller action
                             (cond
                               (images
                                (user-message-input-create
                                 :text            (or text "")
                                 :image-pathnames (mapcar #'image-input-validate-pathname images)))
                               (t
                                text))))))))
        (:geometry
         (localgroup--client-geometry application arguments)
         nil)
        (:event
         (let ((event        (getf arguments :event))
               (presentation (application-presentation application)))
           (list :accepted-p
                 (and presentation
                      (localgroup--terminal-event-p event)
                      (presentation-terminal-offer-event
                       (terminal-ui-terminal presentation) event)))))
        (:input-history
         (let ((start (getf arguments :start))
               (limit (getf arguments :limit)))
           (unless (and (typep start '(integer 0)) (typep limit '(integer 1)))
             (refuse "An input history page names its start and its limit."))
           (list :entries (application-input-history-page application :start start :limit limit))))
        (:operation
         (let ((method (getf arguments :method))
               (params (client-json (getf arguments :params))))
           (unless (stringp method)
             (refuse "An operation names its method."))
           (list :result (application-client-run
                          application method
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
    :documentation "The lock guarding closure, command identifiers and pending replies.")
   (write-lock
    :initform      (make-lock "Autolith client writes")
    :reader        localgroup-client-connection-write-lock
    :documentation "The lock serializing socket writes without blocking reply delivery or closure.")
   (write-failed-p
    :initform      nil
    :accessor      localgroup-client-connection-write-failed-p
    :type          boolean
    :documentation "Writer-lock guarded retirement flag set before releasing an interrupted frame.")
   (evaluator
    :initform      (make-instance 'client-evaluator)
    :reader        localgroup-client-connection-evaluator
    :documentation "Attachment-owned evaluation state, severed on disconnect.")
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
   (entry
    :initarg       :entry
    :reader        localgroup-client-connection-entry
    :documentation "The original authenticated endpoint entry used for this connection.")
   (checkpoint-reconnect-id
    :initform      nil
    :accessor      localgroup-client-connection-checkpoint-reconnect-id
    :type          (option string)
    :documentation "The planned restart announced before this connection closes.")
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

(-> localgroup-client--read-greeting (stream (option function)) list)
(defun localgroup-client--read-greeting (stream cancelled-p)
  "Read the required greeting boundedly, checking cancellation before it arrives."
  (sb-sys:with-deadline (:seconds image-daemon:*daemon-connect-timeout-seconds*)
    (when cancelled-p
      (loop until (listen stream)
            do (when (funcall cancelled-p)
                 (error 'localgroup-error :message  "Client attachment was cancelled."
                                         :operation ':client))
               (sleep 0.01))
      (when (funcall cancelled-p)
        (error 'localgroup-error :message  "Client attachment was cancelled."
                                :operation ':client)))
    (daemon-read-response stream ':client)))

(-> localgroup-client-connect
    (cons &key (:name string) (:event-function function) (:approves-p boolean)
          (:evaluates-p boolean) (:after-sequence (option integer))
          (:cancelled-p (option function)) (:arguments list))
    localgroup-client-connection)
(defun localgroup-client-connect (entry &key (name "client") event-function (approves-p t)
                                           evaluates-p after-sequence cancelled-p arguments)
  "Connect to the channel of localgroup endpoint ENTRY as a structured client NAME.

EVENT-FUNCTION is called on the reader thread with each event, and with
(:closed) once the connection ends. Replies go to the command that waits
for them. APPROVES-P says whether this client answers approvals; one that
does not leaves them to a terminal or another client. With EVALUATES-P the
connection runs the forms the channel sends in this image and answers them
itself; EVENT-FUNCTION never sees those requests. ARGUMENTS is a plist of
what else the client states, such as :PRESENTS-P and its pane's size."
  (let ((record (rest entry))
        (socket nil)
        (stream nil))
    (multiple-value-setq (socket stream)
      (daemon-connect (localgroup--record-port record)))
    (handler-case
        (sb-sys:with-deadline (:seconds image-daemon:*daemon-connect-timeout-seconds*)
          (daemon-write-packet stream
                               (list :localgroup-request
                                     :version   *daemon-protocol-version*
                                     :token     (localgroup--record-token record)
                                     :operation ':client
                                     :arguments (list* :name                 name
                                                       :approves-p           approves-p
                                                       :evaluates-p          evaluates-p
                                                       :cancels-evaluation-p evaluates-p
                                                       :after-sequence       after-sequence
                                                       arguments)))
          (let ((greeting (localgroup-client--read-greeting stream cancelled-p)))
            (unless (eq (first greeting) ':attached)
              (error 'localgroup-client-rejected
                     :message    (or (getf (rest greeting) :message)
                                     "The channel did not accept the client.")
                     :operation  ':client
                     :retry-p    (getf (rest greeting) :retry-p)
                     :session-id (localgroup--record-session-id record)))
            (let ((connection (make-instance 'localgroup-client-connection
                                             :entry    entry
                                             :socket   socket
                                             :stream   stream
                                             :attached greeting)))
              (setf (localgroup-client-connection-reader-thread connection)
                    (make-thread (lambda ()
                                   (localgroup-client--read connection event-function
                                                            :evaluates-p evaluates-p))
                                 :name (format nil "Autolith ~A client reader" name)))
              connection)))
      (error (condition)
        ; mallet:suppress no-ignore-errors -- closing a failed connection must not mask its cause
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error condition)))))

(-> localgroup-client--write-frame (localgroup-client-connection string) null)
(defun localgroup-client--write-frame (connection frame)
  "Serialize FRAME and retire an interrupted stream before another writer can enter."
  (with-lock-held ((localgroup-client-connection-write-lock connection))
    (when (or (localgroup-client-connection-write-failed-p connection)
              (with-lock-held ((localgroup-client-connection-lock connection))
                (localgroup-client-connection-closed-p connection)))
      (error 'localgroup-error :message "The client transport is closed." :operation ':client))
    (let ((finished-p nil))
      (unwind-protect
           (progn
             (write-string frame (localgroup-client-connection-stream connection))
             (finish-output (localgroup-client-connection-stream connection))
             (setf finished-p t))
        (unless finished-p
          (setf (localgroup-client-connection-write-failed-p connection) t)))))
  nil)

(-> localgroup-client--take-identifier (localgroup-client-connection &key (:await-p boolean)) integer)
(defun localgroup-client--take-identifier (connection &key await-p)
  "Return the identifier CONNECTION's next command takes, registering a reply to wait for when AWAIT-P."
  (with-lock-held ((localgroup-client-connection-lock connection))
    (when (or (localgroup-client-connection-closed-p connection)
              (localgroup-client-connection-checkpoint-reconnect-id connection))
      (error 'localgroup-error
             :message   "The client connection is not accepting requests."
             :operation ':client))
    (let ((identifier (incf (localgroup-client-connection-next-identifier connection))))
      (when await-p
        (setf (gethash identifier (localgroup-client-connection-replies connection)) nil))
      identifier)))

(-> localgroup-client--write
    (localgroup-client-connection keyword &key (:identifier integer) (:arguments list)) null)
(defun localgroup-client--write (connection command &key identifier arguments)
  "Write COMMAND with IDENTIFIER and ARGUMENTS, disconnecting when the transport fails."
  (let ((frame (image-daemon:daemon-packet-string (list* command :id identifier arguments))))
    (handler-case (localgroup-client--write-frame connection frame)
      (error (condition)
        (localgroup-client--disconnect connection)
        (error 'localgroup-request-interrupted
               :message      (format nil "The request was interrupted during transport: ~A. Its execution outcome is unknown."
                                     condition)
               :operation ':client :identifier identifier :command command
               :reconnect-id (localgroup-client-connection-checkpoint-reconnect-id connection)))))
  nil)

(-> localgroup-client-call (localgroup-client-connection keyword &rest t) list)
(defun localgroup-client-call (connection command &rest arguments)
  "Send COMMAND with ARGUMENTS and return its reply's fields, or signal its error.

Each call owns one pending reply entry, removed on every exit."
  (let ((identifier (localgroup-client--take-identifier connection :await-p t)))
    (unwind-protect
         (progn
           (localgroup-client--write connection command :identifier identifier
                                                        :arguments  arguments)
           (with-lock-held ((localgroup-client-connection-lock connection))
             (loop
               (let ((reply (gethash identifier (localgroup-client-connection-replies connection))))
                 (cond
                   (reply
                    (when (getf (rest reply) :error)
                      (error 'localgroup-error :message (getf (rest reply) :error) :operation ':client))
                    (return (rest reply)))
                   ((localgroup-client-connection-closed-p connection)
                    (error 'localgroup-request-interrupted
                           :message      "The client connection closed before the reply arrived. The request's execution outcome is unknown."
                           :operation ':client :identifier identifier :command command
                           :reconnect-id (localgroup-client-connection-checkpoint-reconnect-id connection)))
                   (t
                    (condition-wait (localgroup-client-connection-condition-variable connection)
                                    (localgroup-client-connection-lock connection))))))))
      (with-lock-held ((localgroup-client-connection-lock connection))
        (remhash identifier (localgroup-client-connection-replies connection))))))

(-> localgroup-client-send (localgroup-client-connection keyword &rest t) null)
(defun localgroup-client-send (connection command &rest arguments)
  "Send COMMAND with ARGUMENTS and return at once.

Its reply is dropped when it comes, unless the channel refused the command:
then the connection's event function receives (:REFUSED :MESSAGE TEXT).

Use this for a command whose outcome arrives as events, and which the
channel may answer by asking this client something first."
  (localgroup-client--write connection command
                            :identifier (localgroup-client--take-identifier connection)
                            :arguments  arguments))

(-> localgroup-client-close (localgroup-client-connection) null)
(defun localgroup-client-close (connection)
  "Shut down CONNECTION and wait for its reader to close the socket."
  (localgroup-client--disconnect connection)
  (let ((thread (localgroup-client-connection-reader-thread connection)))
    (when (and thread (not (eq thread (current-thread))))
      (join-thread thread)))
  nil)

(defparameter *localgroup-client-evaluation-write-seconds* 1
  "Maximum time an evaluation answer may spend waiting for a write lock and transport.")

(-> localgroup-client--evaluation-answer (localgroup-client-connection list) null)
(defun localgroup-client--evaluation-answer (connection packet)
  "Write PACKET with bounded transport time and no retained acknowledgement entry."
  (handler-case
      (sb-sys:with-deadline (:seconds *localgroup-client-evaluation-write-seconds*)
        (let* ((identifier
                 (with-lock-held ((localgroup-client-connection-lock connection))
                   (when (localgroup-client-connection-closed-p connection)
                     (return-from localgroup-client--evaluation-answer nil))
                   (incf (localgroup-client-connection-next-identifier connection))))
               (frame (image-daemon:daemon-packet-string
                       (list* (first packet) :id identifier (rest packet)))))
          (localgroup-client--write-frame connection frame)))
    (sb-sys:deadline-timeout ()
      (localgroup-client--disconnect connection))
    (error ()
      (localgroup-client--disconnect connection)))
  nil)

(-> localgroup-client--evaluate (localgroup-client-connection t) null)
(defun localgroup-client--evaluate (connection request)
  "Admit REQUEST through the image's shared bounded evaluator."
  (let ((evaluator (localgroup-client-connection-evaluator connection)))
    (with-lock-held (*client-evaluator-lock*)
      (unless (client-evaluator-closed-p evaluator)
        (setf (client-evaluator-delivery evaluator)
              (lambda (packet) (localgroup-client--evaluation-answer connection packet)))))
    (client-evaluator-start evaluator request))
  nil)

(-> localgroup-client--read
    (localgroup-client-connection (option function) &key (:evaluates-p boolean))
    null)
(defun localgroup-client--read (connection event-function &key evaluates-p)
  "Read events and pending replies, then close the socket and report (:closed).

With EVALUATES-P, evaluation requests are run and answered here. A server of
another version may send what this client cannot read: an undecodable packet
is skipped and a malformed one dropped."
  (unwind-protect
       (loop for packet = (handler-case (daemon-read-decodable-packet
                                         (localgroup-client-connection-stream connection))
                            (error ()
                              nil))
             while packet
             do (cond
                  ((not (localgroup-message-p packet))
                   nil)
                  ((eq (first packet) ':reply)
                   (let ((awaited-p
                           (with-lock-held ((localgroup-client-connection-lock connection))
                             (let ((identifier (getf (rest packet) :id))
                                   (replies    (localgroup-client-connection-replies connection)))
                               (when (nth-value 1 (gethash identifier replies))
                                 (setf (gethash identifier replies) packet)
                                 (sb-thread:condition-broadcast
                                  (localgroup-client-connection-condition-variable connection))
                                 t)))))
                     ;; Nobody waits for the reply to a command that was only
                     ;; sent; its refusal still has to reach the client.
                     (when (and (not awaited-p) event-function
                                (stringp (getf (rest packet) :error)))
                       (funcall event-function
                                (list :refused :message (getf (rest packet) :error))))))
                  ((and evaluates-p (eq (first packet) ':evaluation))
                   (localgroup-client--evaluate connection packet))
                  ((and evaluates-p (eq (first packet) ':cancel-evaluation))
                   (client-evaluator-cancel (localgroup-client-connection-evaluator connection)
                                            (getf (rest packet) :id)))
                  (t
                   (when (eq (first packet) ':checkpoint-reconnect)
                     (with-lock-held ((localgroup-client-connection-lock connection))
                       (setf (localgroup-client-connection-checkpoint-reconnect-id connection)
                             (getf (rest packet) :id))))
                   (when event-function
                     (funcall event-function packet)))))
    (localgroup-client--disconnect connection)
    ; mallet:suppress no-ignore-errors -- the server may have closed the connection already
    (ignore-errors (sb-bsd-sockets:socket-close (localgroup-client-connection-socket connection)))
    (when event-function
      (funcall event-function (list :closed))))
  nil)

(-> localgroup-client--disconnect (localgroup-client-connection) null)
(defun localgroup-client--disconnect (connection)
  "Mark CONNECTION closed, wake callers and shut down I/O once.

The reader owns final socket closure. Hold the state lock through shutdown
so it cannot race that closure; never close a stream under this lock."
  (client-evaluator-close (localgroup-client-connection-evaluator connection))
  (with-lock-held ((localgroup-client-connection-lock connection))
    (unless (localgroup-client-connection-closed-p connection)
      (setf (localgroup-client-connection-closed-p connection) t)
      ; mallet:suppress no-ignore-errors -- the peer may have closed already
      (ignore-errors (sb-bsd-sockets:socket-shutdown (localgroup-client-connection-socket connection)
                                                    :direction ':io)))
    (sb-thread:condition-broadcast (localgroup-client-connection-condition-variable connection)))
  nil)
