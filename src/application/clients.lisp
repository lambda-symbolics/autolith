(in-package #:autolith)

;;;; -- Channel Clients --

;;; A channel client is a program that follows one channel through the
;;; structured client protocol: the terminal client, an editor speaking ACP,
;;; a GUI. It receives the channel's events as portable plists and sends
;;; commands back. A client that presents, as the terminal client does, also
;;; receives the channel's (:ui ...) presentation events and renders them
;;; itself. An image-daemon attachment handles each client's bounded output
;;; queue and writer.

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
   (evaluates-p
    :initarg       :evaluates-p
    :initform      nil
    :reader        channel-client-evaluates-p
    :type          boolean
    :documentation "Whether the client evaluates Common Lisp forms, as one written in Lisp may.")
   (cancels-evaluation-p
    :initarg       :cancels-evaluation-p
    :initform      nil
    :reader        channel-client-cancels-evaluation-p
    :type          boolean
    :documentation "Whether this client explicitly supports evaluation cancellation.")
   (presents-p
    :initarg       :presents-p
    :initform      nil
    :reader        channel-client-presents-p
    :type          boolean
    :documentation "Whether the client renders the channel's presentation, as a terminal client does.")
   (watches-p
    :initarg       :watches-p
    :initform      nil
    :reader        channel-client-watches-p
    :type          boolean
    :documentation "Whether a client that presents only watches: the channel takes no input from it and is not laid out for it.")
   (pid
    :initarg       :pid
    :initform      nil
    :reader        channel-client-pid
    :type          (option (integer 1))
    :documentation "The process a presenting client says it is, or NIL.")
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
   (controller
    :initform      nil
    :accessor      client-hub-controller
    :documentation "The presenting client that attached or sent a command last, or NIL.")
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
   (evaluations
    :initform      nil
    :accessor      client-hub-evaluations
    :type          list
    :documentation "The evaluations the channel waits for clients to finish.")
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

(-> channel-client--receives-p (channel-client list) boolean)
(defun channel-client--receives-p (client event)
  "Return true when EVENT is CLIENT's to receive.

A client that presents receives the presentation events and nothing else; it
renders what they describe. Every other client receives the rest: records,
streamed text and status, which it presents its own way."
  (eq (eq (first event) ':ui)
      (channel-client-presents-p client)))

(-> channel-client-controls-p (channel-client) boolean)
(defun channel-client-controls-p (client)
  "Return true when CLIENT presents the channel to someone who can answer it, not only watch it."
  (and (channel-client-presents-p client)
       (not (channel-client-watches-p client))))

(-> application-clients-controlled-p (application &key (:except (option channel-client))) boolean)
(defun application-clients-controlled-p (application &key except)
  "Return true while a client whose user can answer the channel presents APPLICATION's channel.

EXCEPT, a client that is leaving, does not count."
  (not (null (some (lambda (client)
                     (and (not (eq client except))
                          (channel-client-controls-p client)))
                   (application-clients application)))))

(-> application-clients-note-controller (application channel-client) null)
(defun application-clients-note-controller (application client)
  "Remember CLIENT as the one its user is at, when it controls: questions and channel switches go to it."
  (when (channel-client-controls-p client)
    (let ((hub (application-client-hub application)))
      (with-lock-held ((client-hub-lock hub))
        (setf (client-hub-controller hub) client))))
  nil)

(-> application-controlling-client (application) (option channel-client))
(defun application-controlling-client (application)
  "Return the client APPLICATION's user is at, or NIL when none controls the channel.

That is the controlling client that attached or sent a command last, while it
stays attached, and otherwise the one that attached last. A client that only
watches is never it."
  (let ((hub (application-client-hub application)))
    (with-lock-held ((client-hub-lock hub))
      (let ((controller (client-hub-controller hub)))
        (if (and controller (member controller (client-hub-clients hub)))
            controller
            (find-if #'channel-client-controls-p (client-hub-clients hub)))))))

(-> channel-client-tell (channel-client keyword &rest t) boolean)
(defun channel-client-tell (client operation &rest fields)
  "Send presentation OPERATION with FIELDS to CLIENT alone and return true while it is attached."
  (unless (channel-client-closed-p client)
    (channel-client-enqueue client (client-portable (list* :ui :operation operation fields)))
    t))

(-> application-client-tell (application keyword &rest t) boolean)
(defun application-client-tell (application operation &rest fields)
  "Send presentation OPERATION with FIELDS to the controlling client alone and return true when it has one.

Use this for what only the terminal its user is at should do, such as moving
to another channel."
  (let ((client (application-controlling-client application)))
    (and client
         (apply #'channel-client-tell client operation fields))))

(-> application-clients-finish-presenting (application keyword &rest t) null)
(defun application-clients-finish-presenting (application operation &rest fields)
  "Tell every presenting client presentation OPERATION with FIELDS as the last thing it hears, then let it go.

Each client is given the daemon's stop deadline to take what was queued for
it, so the word reaches a client that reads before the endpoint closes."
  (let ((event (client-portable (list* :ui :operation operation fields))))
    (dolist (client (remove-if-not #'channel-client-presents-p (application-clients application)))
      (channel-client-finish client event)))
  nil)

(-> application-client-attached-p (application channel-client) boolean)
(defun application-client-attached-p (application client)
  "Return true while CLIENT follows APPLICATION's channel."
  (not (null (member client (application-clients application)))))

(-> application-presenting-client-pids (application) (values (option integer) list))
(defun application-presenting-client-pids (application)
  "Return the controlling client's process and the sorted processes of the other presenting clients."
  (let ((controller (application-controlling-client application)))
    (values (and controller (channel-client-pid controller))
            (sort (remove-duplicates
                   (loop for client in (application-clients application)
                         for pid = (channel-client-pid client)
                         when (and pid (channel-client-presents-p client)
                                   (not (eq client controller)))
                           collect pid))
                  #'<))))

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
          (when (and (channel-client--receives-p client portable)
                     (channel-client--queue-event client portable))
            (push client overflow)))))
    (dolist (client overflow)
      (channel-client-close client)))
  nil)

(-> application-presentation (application) (option channel-presentation))
(defun application-presentation (application)
  "Return the presentation APPLICATION's channel publishes to its clients, or NIL in a direct session."
  (let ((ui (and (slot-boundp application 'ui) (application-ui application))))
    (and (typep ui 'channel-presentation) ui)))

(-> application-presentation-connect (application) null)
(defun application-presentation-connect (application)
  "Send what APPLICATION's presentation publishes to its clients, and let it ask whether someone can answer it."
  (let ((presentation (application-presentation application)))
    (when presentation
      (setf (channel-presentation-publish-function presentation)
            (lambda (event)
              (application-clients-publish application event))
            (channel-presentation-turn-state-function presentation)
            (lambda ()
              (let ((controller (application-input-controller application)))
                (and controller (application-input-controller-turn-state controller))))
            (channel-presentation-record-boundary-function presentation)
            (lambda ()
              (when (and (slot-boundp application 'conversation)
                         (application-conversation application))
                (let ((conversation (application-conversation application)))
                  (list (conversation-identifier conversation)
                        (application-clients--record-sequence application)))))
            (channel-presentation-history-function presentation)
            (lambda ()
              (if (slot-boundp application 'conversation)
                  (application-input-history-window presentation (application-conversation application))
                  (values nil 0 nil)))
            (presentation-terminal-interactive-function (terminal-ui-terminal presentation))
            (lambda ()
              (application-clients-controlled-p application)))))
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
         (conversation (and (slot-boundp application 'conversation) (application-conversation application)))
         (presentation (and (channel-client-presents-p client)
                            (application-presentation application))))
    (flet ((subscribe (replay)
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
                                          (list :sequence              sequence
                                                :record-sequence       (application-clients--record-sequence application)
                                                :presentation-replay-p (not (null replay))))))
                     (error 'localgroup-error :operation ':client :message "The client greeting queue is full.")))
                 (dolist (event events)
                   (when (and after-sequence (> (getf (rest event) :sequence) after-sequence)
                              (channel-client--receives-p client event))
                     (when (channel-client--queue-event client event)
                       (error 'localgroup-error :operation ':client
                              :message                     "The retained event tail exceeds the client queue."))))
                 (dolist (event replay)
                   (when (channel-client--queue-event client (client-portable event))
                     (error 'localgroup-error :operation ':client
                            :message                     "The channel's presentation exceeds the client queue.")))
                 (with-lock-held ((image-daemon:attachment-lock (channel-client-attachment client)))
                   (unless (channel-client-closed-p client)
                     (setf (channel-client-hub client) hub)
                     (push client (client-hub-clients hub))))))))
      (flet ((attach ()
               (application-clients--follow-conversation application)
                ;; A fresh presenting client starts from durable history; a reconnect
                ;; retains its own viewport and resumes only the exact event tail.
               (subscribe
                (and presentation (null after-sequence)
                     (application-client-history-replay
                      application presentation :character-limit
                      (max 0 (- image-daemon:*attachment-queue-character-limit*
                                (if attached
                                    (or (application-client-history--frame-size attached) 0)
                                    0))))))))
        (flet ((attach-appending ()
                 (if conversation
                     (with-recursive-lock-held ((conversation-append-lock conversation))
                       (attach))
                     (attach))))
          ;; Stream rendering, presentation, durable append, then hub admission.
          ;; Hold this cut through registration, with no transport cleanup inside it.
          (if presentation
              (with-recursive-lock-held ((application-render-lock application))
                (with-terminal-ui-locked (presentation)
                  (attach-appending)))
              (attach-appending)))))
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

(defclass client-work (job)
  ((failure
    :initform      nil
    :accessor      client-work-failure
    :type          (option error)
    :documentation "The original local condition, beyond the job's portable report."))
  (:documentation "An inline job whose channel thread runs a client's operation."))

(defmethod job-interrupt-on-cancellation-p ((work client-work))
  "Never interrupt the shared channel thread when its waiting client leaves."
  nil)

(defparameter *client-work-poll-seconds* 0.25
  "How often a client waiting for its work checks that the channel still runs.")

(-> application-clients-call-as-work (application function) t)
(defun application-clients-call-as-work (application function)
  "Run FUNCTION between turns on APPLICATION's channel, returning all its values.

Re-signal the original local error. Refuse work when the channel stops. The
waiting caller owns the pool; cancellation never interrupts the channel thread."
  (let ((controller (application-input-controller application))
        (pool       (make-job-pool :name                         "Autolith client work"
                                   :job-class                    'client-work
                                   :maximum-concurrency          1
                                   :maximum-runtime-milliseconds 0
                                   :terminal-retention-limit     1
                                   :start-threads-p              nil)))
    (unwind-protect
         (let ((work (job-pool-submit
                      pool
                      (lambda (work)
                        (handler-case (multiple-value-list (funcall function))
                          (error (condition)
                            (setf (client-work-failure work) condition)
                            (error condition))))
                      :inline-only-p t)))
           (unless (and controller (application-input-controller-enqueue-work controller (list :client-work work)))
             (error 'localgroup-error :message "The channel does not accept work now." :operation ':client))
           (let ((snapshot
                   (loop
                     (multiple-value-bind (snapshot terminal-p)
                         (job-await work :timeout-seconds *client-work-poll-seconds*)
                       (when terminal-p
                         (return snapshot)))
                     (when (application-input-controller-stopping-p controller)
                       (error 'localgroup-error
                              :message   "The channel stopped before it ran the work."
                              :operation ':client)))))
             (when (client-work-failure work)
               (error (client-work-failure work)))
             (case (getf snapshot :state)
               (:completed
                (values-list (getf snapshot :result)))
               (:aborted
                (let ((reason (getf snapshot :cancellation-reason)))
                  (error 'client-work-cancelled
                         :message   (format nil "The queued editor operation was cancelled: ~(~A~)." reason)
                         :operation ':client
                         :reason    reason)))
               (otherwise
                (error 'localgroup-error
                       :message   (or (getf snapshot :condition-report) "The client operation did not finish.")
                       :operation ':client)))))
      (job-pool-close pool))))

(define-condition client-work-cancelled (localgroup-error)
  ((reason
    :initarg       :reason
    :reader        client-work-cancelled-reason
    :type          keyword
    :documentation "Why movement or shutdown cancelled the operation."))
  (:documentation "A client operation was cancelled through its inline job lifecycle."))


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
    (&key (:socket t) (:stream stream) (:transport-lock t) (:name string) (:approves-p boolean)
          (:evaluates-p boolean) (:cancels-evaluation-p boolean) (:presents-p boolean)
          (:watches-p boolean) (:pid (option (integer 1))))
    channel-client)
(defun channel-client-create
    (&key socket stream transport-lock (name "client") (approves-p t) evaluates-p cancels-evaluation-p
          presents-p watches-p pid)
  "Create a structured client and its deferred output attachment."
  (let ((client (make-instance 'channel-client :name name :approves-p approves-p
                                               :evaluates-p          evaluates-p
                                               :cancels-evaluation-p cancels-evaluation-p
                                               :presents-p           presents-p
                                               :watches-p            watches-p
                                               :pid                  pid)))
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

(defmethod attachment-evaluates-p ((transport channel-client-transport))
  "Return whether TRANSPORT's structured client said it evaluates Common Lisp forms."
  (channel-client-evaluates-p (channel-client-transport-client transport)))

(defmethod attachment-cancels-evaluation-p ((transport channel-client-transport))
  "Return the cancellation support advertised by TRANSPORT's structured client."
  (channel-client-cancels-evaluation-p (channel-client-transport-client transport)))

(defmethod image-daemon:attachment-release-transport ((transport channel-client-transport))
  "Shut down TRANSPORT, unregister its client and wake approvals; the daemon closes it."
  (unwind-protect
       (image-daemon:attachment-shutdown transport)
    (let* ((client (channel-client-transport-client transport))
           (hub    (channel-client-hub client)))
      (when hub
        (with-lock-held ((client-hub-lock hub))
          (when (eq (channel-client-hub client) hub)
            (setf (client-hub-clients hub) (cl:delete client (client-hub-clients hub))
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
  (portable-value-map
   value
   (lambda (item child)
     (typecase item
       (null
        nil)
       ((satisfies json-false-p)
        ':false)
       (string
        (copy-seq item))
       ((or number keyword character (eql t))
        item)
       (hash-table
        (cons ':object
              (loop for key being the hash-keys of item using (hash-value member)
                    append (list (funcall child key) (funcall child member)))))
       (vector
        (cons ':array (map 'list child item)))
       (symbol
        (string-downcase (symbol-name item)))
       (pathname
        (namestring item))
       (t
        (princ-to-string item))))
   :containers-p t))

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
