(in-package #:autolith)

;;;; -- Channel Clients --

;;; A channel client is a program other than a terminal that follows one
;;; channel through the structured client protocol: an editor speaking ACP
;;; today, a web, GUI or phone client later. It receives the channel's events
;;; as portable plists and sends commands back. Each client has its own queue
;;; and writer thread, so a slow client never holds up the channel's turn.

(defclass channel-client ()
  ((identifier
    :initform      (make-identifier)
    :reader        channel-client-identifier
    :type          string
    :documentation "The identifier this client is known by while it stays attached.")
   (name
    :initarg       :name
    :initform      "client"
    :reader        channel-client-name
    :type          string
    :documentation "What the client says it is, such as acp.")
   (approves-p
    :initarg       :approves-p
    :initform      t
    :reader        channel-client-approves-p
    :type          boolean
    :documentation "Whether the client answers approvals, as an editor does and a script does not.")
   (stream
    :initarg       :stream
    :reader        channel-client-stream
    :type          stream
    :documentation "The connection events are written to.")
   (lock
    :initform      (make-lock "Autolith channel client")
    :reader        channel-client-lock
    :documentation "The lock guarding the queue and the closed flag.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith channel client")
    :reader        channel-client-condition-variable
    :documentation "Signaled when an event is queued or the client closes.")
   (events
    :initform      (make-deque)
    :reader        channel-client-events
    :documentation "Events queued for the writer thread, oldest first.")
   (closed-p
    :initform      nil
    :accessor      channel-client-closed-p
    :type          boolean
    :documentation "Whether the client is gone and takes no more events.")
   (writer-thread
    :initform      nil
    :accessor      channel-client-writer-thread
    :documentation "The thread writing queued events to the connection."))
  (:documentation "One structured client following a channel."))

(defclass client-hub ()
  ((lock
    :initform      (make-lock "Autolith channel clients")
    :reader        client-hub-lock
    :documentation "The lock guarding the client list.")
   (clients
    :initform      nil
    :accessor      client-hub-clients
    :type          list
    :documentation "The structured clients attached to the channel.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith channel approvals")
    :reader        client-hub-condition-variable
    :documentation "Signaled when a client answers the pending approval or a client leaves.")
   (approval
    :initform      nil
    :accessor      client-hub-approval
    :documentation "The approval the channel waits on, or NIL.")
   (usage
    :initform      nil
    :accessor      client-hub-usage
    :type          list
    :documentation "The latest provider usage the channel measured, as (NAME COUNT) pairs.")
   (verified-models
    :initform      (make-hash-table :test #'equal :synchronized t)
    :reader        client-hub-verified-models
    :documentation "The models a request answered recently, mapped to when it did."))
  (:documentation "The structured clients of one application's channel."))


;;;; -- Policy --

(defparameter *channel-client-queue-limit* 20000
  "How many events may wait for a client before it counts as gone.

A client that falls this far behind is disconnected; it reconnects and reads
the channel's state and history again.")


;;;; -- Publishing --

(-> application-clients (application) list)
(defun application-clients (application)
  "Return the structured clients attached to APPLICATION's channel."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (copy-list (client-hub-clients hub)))))

(-> application-clients-attached-p (application) boolean)
(defun application-clients-attached-p (application)
  "Return true while a structured client follows APPLICATION's channel."
  (not (null (application-clients application))))

(-> application-clients-approving-p (application) boolean)
(defun application-clients-approving-p (application)
  "Return true while a structured client that answers approvals follows APPLICATION's channel."
  (not (null (some #'channel-client-approves-p (application-clients application)))))

(-> application-clients-publish (application list) null)
(defun application-clients-publish (application event)
  "Queue EVENT, a portable plist, for every structured client of APPLICATION's channel."
  (let ((clients (application-clients application)))
    (when clients
      (let ((portable (client-portable event)))
        (dolist (client clients)
          (channel-client-enqueue client portable)))))
  nil)

(-> application-clients-add (application channel-client) null)
(defun application-clients-add (application client)
  "Attach CLIENT to APPLICATION's channel and start writing its events."
  (setf (channel-client-writer-thread client)
        (make-thread (lambda () (channel-client--write-events client))
                     :name (format nil "Autolith ~A client writer" (channel-client-name client))))
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (push client (client-hub-clients hub))))
  (application-clients--follow-conversation application)
  nil)

(-> application-clients-remove (application channel-client) null)
(defun application-clients-remove (application client)
  "Detach CLIENT from APPLICATION's channel and stop its writer."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (setf (client-hub-clients hub) (remove client (client-hub-clients hub)))
      (condition-notify (client-hub-condition-variable hub))))
  (channel-client-close client)
  nil)

(-> application-clients--follow-conversation (application) null)
(defun application-clients--follow-conversation (application)
  "Publish every record APPLICATION's current conversation appends to its clients."
  (when (slot-boundp application 'conversation)
    (let ((conversation (application-conversation application)))
      (when conversation
        (setf (conversation-record-listener conversation)
              (lambda (record)
                (application-clients-publish application (list :record :record record)))))))
  nil)


;;;; -- Work for Clients --

(defclass client-work ()
  ((function
    :initarg       :function
    :reader        client-work-function
    :type          function
    :documentation "What the channel runs for the client, on its main thread.")
   (lock
    :initform      (make-lock "Autolith client work")
    :reader        client-work-lock
    :documentation "The lock guarding the outcome.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith client work")
    :reader        client-work-condition-variable
    :documentation "Signaled once the work has run.")
   (done-p
    :initform      nil
    :accessor      client-work-done-p
    :type          boolean
    :documentation "Whether the work has run.")
   (values
    :initform      nil
    :accessor      client-work-values
    :type          list
    :documentation "What the function returned.")
   (failure
    :initform      nil
    :accessor      client-work-failure
    :documentation "The condition the function signaled, or NIL."))
  (:documentation "A function a client has a channel run as one of its own work items."))

(defparameter *client-work-poll-seconds* 0.25
  "How often a client waiting for its work checks that the channel still runs.")

(-> application-clients-call-as-work (application function) t)
(defun application-clients-call-as-work (application function)
  "Run FUNCTION as a work item on APPLICATION's channel and return what it returns.

The channel runs it between its other work, as it runs typed input, so it
never overlaps a turn. A condition FUNCTION signals is signaled here; a
channel that stops before running it refuses it."
  (let ((controller (application-input-controller application))
        (work       (make-instance 'client-work :function function)))
    (unless (and controller (application-input-controller-enqueue-work controller (list :client-work work)))
      (error 'localgroup-error :message "The channel does not accept work now." :operation ':client))
    (with-lock-held ((client-work-lock work))
      (loop until (client-work-done-p work)
            do (when (application-input-controller-stopping-p controller)
                 (error 'localgroup-error
                        :message   "The channel stopped before it ran the work."
                        :operation ':client))
               (condition-wait (client-work-condition-variable work) (client-work-lock work)
                               :timeout *client-work-poll-seconds*)))
    (let ((failure (client-work-failure work)))
      (when failure
        (error failure)))
    (values-list (client-work-values work))))

(-> client-work-run (client-work) null)
(defun client-work-run (work)
  "Run WORK's function and hand its values or its failure to the waiting client."
  (let ((results nil)
        (failure nil))
    (unwind-protect
         (handler-case (setf results (multiple-value-list (funcall (client-work-function work))))
           (error (condition)
             (setf failure condition)))
      (with-lock-held ((client-work-lock work))
        (setf (client-work-values work)  results
              (client-work-failure work) failure
              (client-work-done-p work)  t)
        (condition-notify (client-work-condition-variable work)))))
  nil)


;;;; -- Measurements --

(-> application-clients-note-usage (application t) null)
(defun application-clients-note-usage (application usage)
  "Keep the provider USAGE a completed request measured, and that the channel's model answered just now."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (setf (client-hub-usage hub)
            (loop for name in '("input_tokens" "output_tokens" "cached_input_tokens")
                  collect (list name (conversation--usage-field usage name)))))
    (setf (gethash (config :model (application-configuration application))
                   (client-hub-verified-models hub))
          (get-universal-time)))
  nil)


;;;; -- One Client --

(-> channel-client-enqueue (channel-client list) null)
(defun channel-client-enqueue (client event)
  "Queue EVENT for CLIENT, closing a client that fell too far behind."
  (with-lock-held ((channel-client-lock client))
    (unless (channel-client-closed-p client)
      (if (>= (deque-count (channel-client-events client)) *channel-client-queue-limit*)
          (setf (channel-client-closed-p client) t)
          (deque-push-back (channel-client-events client) event))
      (condition-notify (channel-client-condition-variable client))))
  nil)

(-> channel-client-close (channel-client) null)
(defun channel-client-close (client)
  "Close CLIENT: it takes no more events and its writer ends."
  (with-lock-held ((channel-client-lock client))
    (setf (channel-client-closed-p client) t)
    (condition-notify (channel-client-condition-variable client)))
  nil)

(-> channel-client--next-event (channel-client) (option list))
(defun channel-client--next-event (client)
  "Wait for CLIENT's next queued event and return it, or NIL once CLIENT closed."
  (with-lock-held ((channel-client-lock client))
    (loop
      (cond
        ((channel-client-closed-p client)
         (return nil))
        ((not (deque-empty-p (channel-client-events client)))
         (return (deque-pop-front (channel-client-events client))))
        (t
         (condition-wait (channel-client-condition-variable client)
                         (channel-client-lock client)))))))

(-> channel-client--write-events (channel-client) null)
(defun channel-client--write-events (client)
  "Write CLIENT's queued events to its connection until it closes or the connection fails.

An event too large for one packet is replaced by a notice naming its kind, so
the client knows something was left out."
  (handler-case
      (loop for event = (channel-client--next-event client)
            while event
            do (handler-case (daemon-write-packet (channel-client-stream client) event)
                 (image-daemon:daemon-error ()
                   (daemon-write-packet (channel-client-stream client)
                                        (list :omitted :kind (first event))))))
    (error ()
      (channel-client-close client)))
  nil)


;;;; -- Portable Events --

(-> client-portable (t) t)
(defun client-portable (value)
  "Return VALUE with everything a client cannot read replaced by its printed form.

Events carry strings, numbers, keywords, T and lists of them. A JSON object
becomes (:OBJECT KEY VALUE ...), a JSON array (:ARRAY VALUE ...) and JSON
false :FALSE, other symbols their lowercase names, pathnames their
namestrings, and other objects, such as conditions, their printed text.
CLIENT-JSON turns the JSON forms back."
  (typecase value
    (null
     nil)
    ((satisfies json-false-p)
     ':false)
    ((or string number keyword character (eql t))
     value)
    (hash-table
     (cons ':object
           (loop for key being the hash-keys of value using (hash-value item)
                 append (list (client-portable key) (client-portable item)))))
    (vector
     (cons ':array (map 'list #'client-portable value)))
    (cons
     (let ((result nil))
       (loop for rest on value
             do (push (client-portable (first rest)) result)
                (unless (listp (rest rest))
                  (return-from client-portable
                    (nreconc result (client-portable (rest rest))))))
       (nreverse result)))
    (symbol
     (string-downcase (symbol-name value)))
    (pathname
     (namestring value))
    (t
     (princ-to-string value))))

(-> client-json (t) t)
(defun client-json (value)
  "Return the JSON value whose portable form CLIENT-PORTABLE made VALUE.

(:OBJECT KEY VALUE ...) becomes a JSON object, (:ARRAY VALUE ...) a vector,
:FALSE JSON false, and :NULL stays the JSON null."
  (cond
    ((and (consp value) (eq (first value) ':object))
     (let ((object (make-hash-table :test #'equal)))
       (loop for (key item) on (rest value) by (partial #'nthcdr 2)
             do (setf (gethash key object) (client-json item)))
       object))
    ((and (consp value) (eq (first value) ':array))
     (map 'vector #'client-json (rest value)))
    ((eq value ':false)
     (json-false))
    ((consp value)
     (mapcar #'client-json value))
    (t
     value)))
