(in-package #:autolith)

;;;; -- Channel Clients --

;;; A channel client is a program other than a terminal that follows one
;;; channel through the structured client protocol: an editor speaking ACP
;;; today, a web, GUI or phone client later. It receives the channel's events
;;; as portable plists and sends commands back. An image-daemon attachment
;;; handles each client's bounded output queue and writer.

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
   (attachment
    :accessor      channel-client-attachment
    :type          image-daemon:attachment
    :documentation "The attachment owning this client's output lifecycle.")
   (hub
    :initform      nil
    :accessor      channel-client-hub
    :documentation "The hub owning this client's registration, or NIL before attachment."))
  (:documentation "One structured client following a channel."))

(defclass channel-client-transport (image-daemon:attachment)
  ((client
    :initarg       :client
    :reader        channel-client-transport-client
    :type          channel-client
    :documentation "The structured client notified when its transport stops."))
  (:documentation "Attachment output with structured framing and daemon-owned closure."))

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
   (admitting-p
    :initform      t
    :accessor      client-hub-admitting-p
    :documentation "Whether new structured attachments may join this running channel.")
   (event-sequence
    :initform      0
    :accessor      client-hub-event-sequence
    :documentation "The last allocated channel event cursor, guarded by the hub lock.")
   (event-journal
    :initform      (make-deque)
    :reader        client-hub-event-journal
    :documentation "The bounded chronological tail of sequenced channel events.")
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

(defparameter *channel-client-journal-limit* 10000
  "Maximum channel events retained for cursor-based reconnection.")

(-> channel-client--queue-event (channel-client list) boolean)
(defun channel-client--queue-event (client event)
  "Queue EVENT without transport cleanup; return whether CLIENT must disconnect."
  (handler-case
      (eq (image-daemon:attachment-try-send (channel-client-attachment client) event) ':overflow)
    ;; A malformed packet disconnects its recipient, not the publishing channel.
    (error ()
      t)))

(-> application-clients--record-sequence (application) integer)
(defun application-clients--record-sequence (application)
  "Return the last durable sequence of APPLICATION's current conversation."
  (if (and (slot-boundp application 'conversation) (application-conversation application))
      (1- (conversation-next-sequence (application-conversation application)))
      0))


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
  "Sequence and retain EVENT, then queue it for every subscriber in that same order."
  (let ((hub      (application-client-hub application))
        (overflow nil))
    (with-lock-held ((client-hub-lock hub))
      (let ((portable (client-portable
                       (append event
                               (list :sequence        (incf (client-hub-event-sequence hub))
                                     :record-sequence (application-clients--record-sequence application))))))
        (deque-push-back (client-hub-event-journal hub) portable)
        (loop while (> (deque-count (client-hub-event-journal hub)) *channel-client-journal-limit*)
              do (deque-pop-front (client-hub-event-journal hub)))
        (dolist (client (client-hub-clients hub))
          (when (channel-client--queue-event client portable)
            (push client overflow)))))
    (dolist (client overflow)
      (channel-client-close client)))
  nil)

(-> application-clients-add
    (application channel-client &key (:attached list) (:after-sequence t)) null)
(defun application-clients-add (application client &key attached after-sequence)
  "Atomically attach CLIENT after its greeting and any retained cursor tail.

The greeting contains the event watermark and durable replay boundary. A
cursor outside the retained tail is refused rather than skipping events."
  (unless (or (null after-sequence) (typep after-sequence '(integer 0 *)))
    (error 'localgroup-error :operation ':client
           :message                     "A channel event cursor must be a nonnegative integer."))
  (let* ((hub          (application-client-hub application))
         (conversation (and (slot-boundp application 'conversation) (application-conversation application))))
    (flet ((subscribe ()
             (with-lock-held ((client-hub-lock hub))
               (unless (client-hub-admitting-p hub)
                 (error 'localgroup-error :operation ':client :retry-p t
                        :message "The channel is restarting; retry attachment after it resumes."))
               (let* ((sequence (client-hub-event-sequence hub))
                      (events   (deque->list (client-hub-event-journal hub)))
                      (oldest   (if events (getf (rest (first events)) :sequence) (1+ sequence))))
                 (when (and after-sequence
                            (not (<= (1- oldest) after-sequence sequence)))
                   (error 'localgroup-error :operation ':client
                          :message                     "The channel event cursor has expired or is ahead of the channel."))
                 (when attached
                   (when (channel-client--queue-event
                          client (client-portable
                                  (append attached
                                          (list :sequence        sequence
                                                :record-sequence (application-clients--record-sequence application)))))
                     (error 'localgroup-error :operation ':client :message "The client greeting queue is full.")))
                 (dolist (event events)
                   (when (and after-sequence (> (getf (rest event) :sequence) after-sequence))
                     (when (channel-client--queue-event client event)
                       (error 'localgroup-error :operation ':client
                              :message                     "The retained event tail exceeds the client queue."))))
                 (with-lock-held ((image-daemon:attachment-lock (channel-client-attachment client)))
                   (unless (channel-client-closed-p client)
                     (setf (channel-client-hub client) hub)
                     (push client (client-hub-clients hub))))))))
      (if conversation
          (with-recursive-lock-held ((conversation-append-lock conversation))
            (application-clients--follow-conversation application)
            (subscribe))
          (subscribe)))
    ;; The serving scope owns cleanup, after transmitting an attachment refusal.
    (image-daemon:attachment-start (channel-client-attachment client)))
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

(-> client-work-cancel (client-work keyword) null)
(defun client-work-cancel (work reason)
  "Cancel unstarted WORK for REASON and wake its waiting client."
  (with-lock-held ((client-work-lock work))
    (unless (client-work-done-p work)
      (setf (client-work-failure work)
            (make-condition 'client-work-cancelled
                            :message   (format nil "The queued editor operation was cancelled because the agent ~A."
                                               (ecase reason
                                                 (:moved
                                                  "moved to another channel")
                                                 (:dismissed
                                                  "was dismissed")))
                            :operation ':client
                            :reason    reason)
            (client-work-done-p work) t)
      (condition-notify (client-work-condition-variable work))))
  nil)

(define-condition client-work-cancelled (localgroup-error)
  ((reason
    :initarg       :reason
    :reader        client-work-cancelled-reason
    :type          keyword
    :documentation "Why movement removed the queued operation."))
  (:documentation "An editor operation was cancelled before the channel ran it."))


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

(-> channel-client-create
    (&key (:socket t) (:stream stream) (:transport-lock t) (:name string) (:approves-p boolean))
    channel-client)
(defun channel-client-create (&key socket stream transport-lock (name "client") (approves-p t))
  "Create a structured client and its deferred output attachment."
  (let ((client (make-instance 'channel-client :name name :approves-p approves-p)))
    (setf (channel-client-attachment client)
          (make-instance 'channel-client-transport
                         :client client :socket socket :stream stream :mode ':read-only
                         :transport-lock transport-lock))
    client))

(-> channel-client-closed-p (channel-client) boolean)
(defun channel-client-closed-p (client)
  "Return whether CLIENT's attachment has stopped accepting output."
  (image-daemon:attachment-closed-p (channel-client-attachment client)))

(-> channel-client-enqueue (channel-client list) null)
(defun channel-client-enqueue (client event)
  "Queue EVENT for CLIENT, disconnecting a client that fell too far behind."
  (when (channel-client--queue-event client event)
    (channel-client-close client))
  nil)

(-> channel-client-finish (channel-client list) null)
(defun channel-client-finish (client event)
  "Seal CLIENT with EVENT, then drain and reap its writer within the daemon deadline."
  (image-daemon:attachment-finish (channel-client-attachment client) event))

(-> channel-client-close (channel-client) null)
(defun channel-client-close (client)
  "Detach CLIENT and reap its writer outside hub and attachment locks."
  (image-daemon:attachment-close (channel-client-attachment client)))

(defmethod image-daemon:attachment-release-transport ((transport channel-client-transport))
  "Shut down TRANSPORT, unregister its client and wake approvals; the daemon closes it."
  (unwind-protect
       (image-daemon:attachment-shutdown transport)
    (let* ((client (channel-client-transport-client transport))
           (hub    (channel-client-hub client)))
      (when hub
        (with-lock-held ((client-hub-lock hub))
          (when (eq (channel-client-hub client) hub)
            (setf (client-hub-clients hub) (delete client (client-hub-clients hub))
                  (channel-client-hub client) nil)
            (sb-thread:condition-broadcast (client-hub-condition-variable hub)))))))
  nil)

(defmethod image-daemon:attachment-packet-string ((transport channel-client-transport) event)
  "Frame EVENT, retaining reply identity or event cursors when its payload is too large."
  (declare (ignore transport))
  (handler-case (image-daemon:daemon-packet-string event)
    (image-daemon:daemon-error ()
      (image-daemon:daemon-packet-string
       (if (eq (first event) ':reply)
           (list :reply :id (getf (rest event) :id)
                 :error "The reply exceeded the packet size limit.")
           (list :omitted         :kind (first event)
                 :sequence        (getf (rest event) :sequence)
                 :record-sequence (getf (rest event) :record-sequence)))))))


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
