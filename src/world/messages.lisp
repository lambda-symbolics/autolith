(in-package #:autolith)

;;;; -- Channel Messages --

;;; A message to a channel goes into that channel's inbox, a small snapshot
;;; beside the world log. The process serving the channel drains the inbox
;;; into the channel's history and wakes its residents. Subscribers and
;;; mentioned participants residing elsewhere get a copy in the inbox of
;;; their own residence channel. A delivery is settled once the serving
;;; conversation records its receipt, so a crash between the two never
;;; repeats or loses a message.

(defparameter *world-inbox-version* 1
  "The readable inbox format version.")

(defparameter *world-message-text-limit* 8192
  "The maximum characters in one channel message.")

(defparameter *world-inbox-limit* 256
  "The most undelivered messages one channel inbox holds.")

(defparameter *world-outbox-version* 1
  "The readable outbox format version.")

(defvar *world-message-unserved-function* nil
  "A function called with each channel a message reached that no process serves, or NIL.

The world server sets it while it runs, so a message wakes the agent it is for.")

(deftype world-delivery-kind ()
  "Why a delivery reached a channel: addressed to it, subscribed, mentioned, or a notice for the person residing there."
  '(member :post :subscription :mention :notice))

(defclass world-delivery ()
  ((identifier
    :initarg       :identifier
    :reader        world-delivery-identifier
    :type          non-empty-string
    :documentation "The stable delivery identifier, shared by every copy of one message.")
   (time
    :initarg       :time
    :reader        world-delivery-time
    :type          timestamp
    :documentation "When the message was sent.")
   (sender
    :initarg       :sender
    :reader        world-delivery-sender
    :type          non-empty-string
    :documentation "The sending participant's identifier.")
   (sender-name
    :initarg       :sender-name
    :reader        world-delivery-sender-name
    :type          non-empty-string
    :documentation "The sending participant's name when the message was sent.")
   (sender-kind
    :initarg       :sender-kind
    :initform      nil
    :reader        world-delivery-sender-kind
    :type          (option participant-kind)
    :documentation "Whether the sender is the person or an agent, or NIL in a copy an unreleased build wrote before deliveries recorded it; that allowance goes at 0.68.0.")
   (source
    :initarg       :source
    :reader        world-delivery-source
    :type          non-empty-string
    :documentation "The channel the message was sent from, by default the sender's residence.")
   (target
    :initarg       :target
    :reader        world-delivery-target
    :type          non-empty-string
    :documentation "The channel the message was addressed to.")
   (text
    :initarg       :text
    :reader        world-delivery-text
    :type          non-empty-string
    :documentation "The message text.")
   (mentions
    :initarg       :mentions
    :initform      nil
    :reader        world-delivery-mentions
    :type          list
    :documentation "The identifiers of the participants the message mentions.")
   (kind
    :initarg       :kind
    :reader        world-delivery-kind
    :type          world-delivery-kind
    :documentation "Why this copy reached the inbox it sits in."))
  (:documentation "One copy of a channel message waiting in a channel inbox."))


;;;; -- Inbox Store --

(-> world-inbox-pathname (configuration string) pathname)
(defun world-inbox-pathname (configuration channel-identifier)
  "Return the inbox snapshot of channel CHANNEL-IDENTIFIER."
  (merge-pathnames (make-pathname :name channel-identifier :type "sexp")
                   (merge-pathnames "world/inbox/" (config :data-root configuration))))

(-> world-delivery--record (world-delivery) list)
(defun world-delivery--record (delivery)
  "Return DELIVERY as one portable readable record."
  (list :delivery
        :id          (world-delivery-identifier delivery)
        :time        (world-delivery-time delivery)
        :sender      (world-delivery-sender delivery)
        :sender-name (world-delivery-sender-name delivery)
        :sender-kind (world-delivery-sender-kind delivery)
        :source      (world-delivery-source delivery)
        :target      (world-delivery-target delivery)
        :text        (world-delivery-text delivery)
        :mentions    (copy-list (world-delivery-mentions delivery))
        :kind        (world-delivery-kind delivery)))

(-> world-delivery--record-p (t) boolean)
(defun world-delivery--record-p (form)
  "Return true when FORM is one valid delivery record."
  (and (listp form)
       (eq (first form) ':delivery)
       (sexp-store:plist-schema-p (rest form)
                                 :required-keys  '(:id :time :sender :sender-name :source
                                                   :target :text :mentions :kind)
                                 :allowed-keys   '(:id :time :sender :sender-name :sender-kind :source
                                                   :target :text :mentions :kind)
                                 :keyword-keys-p t)
       (let ((properties (rest form)))
         (and (non-empty-string-p (getf properties :id))
              (typep (getf properties :time) 'timestamp)
              (non-empty-string-p (getf properties :sender))
              (world--name-p (getf properties :sender-name))
              (typep (getf properties :sender-kind) '(option participant-kind))
              (non-empty-string-p (getf properties :source))
              (non-empty-string-p (getf properties :target))
              (non-empty-string-p (getf properties :text))
              (<= (length (getf properties :text)) *world-message-text-limit*)
              (world--identifier-list-p (getf properties :mentions))
              (typep (getf properties :kind) 'world-delivery-kind)
              t))))

(-> world-delivery--record->delivery (list) world-delivery)
(defun world-delivery--record->delivery (record)
  "Return the delivery validated RECORD describes."
  (let ((properties (rest record)))
    (make-instance 'world-delivery
                   :identifier  (getf properties :id)
                   :time        (getf properties :time)
                   :sender      (getf properties :sender)
                   :sender-name (getf properties :sender-name)
                   :sender-kind (getf properties :sender-kind)
                   :source      (getf properties :source)
                   :target      (getf properties :target)
                   :text        (getf properties :text)
                   :mentions    (copy-list (getf properties :mentions))
                   :kind        (getf properties :kind))))

(-> world-inbox--form-p (t) boolean)
(defun world-inbox--form-p (form)
  "Return true when FORM is a complete inbox snapshot."
  (and (listp form)
       (eq (first form) ':inbox)
       (sexp-store:plist-schema-p (rest form)
                                 :required-keys  '(:version :deliveries)
                                 :allowed-keys   '(:version :deliveries)
                                 :keyword-keys-p t)
       (eql (getf (rest form) :version) *world-inbox-version*)
       (let ((deliveries (getf (rest form) :deliveries)))
         (and (listp deliveries)
              (handler-case (list-length deliveries) (error () nil))
              (every #'world-delivery--record-p deliveries)
              t))))

(-> world-inbox--store (configuration string) sexp-store:snapshot-store)
(defun world-inbox--store (configuration channel-identifier)
  "Describe channel CHANNEL-IDENTIFIER's inbox snapshot and its paths."
  (let ((pathname (world-inbox-pathname configuration channel-identifier)))
    (ensure-directories-exist pathname)
    (make-instance
     'sexp-store:snapshot-store
     :pathname      pathname
     :lock-pathname (readable-state-lock-pathname
                     pathname (format nil "~A.lock" channel-identifier))
     :initial-state (lambda () nil)
     :validator     #'world-inbox--form-p
     :decoder       (lambda (form)
                      (mapcar #'world-delivery--record->delivery
                              (getf (rest form) :deliveries)))
     :encoder       (lambda (deliveries)
                      (list :inbox
                            :version    *world-inbox-version*
                            :deliveries (mapcar #'world-delivery--record deliveries))))))

(-> world-inbox--transact (configuration string function) t)
(defun world-inbox--transact (configuration channel-identifier update)
  "Apply UPDATE to CHANNEL-IDENTIFIER's fresh inbox and return its result.

UPDATE receives the deliveries and returns the replacement list, a result and
whether to write."
  (handler-case
      (sexp-store:store-transact (world-inbox--store configuration channel-identifier) update)
    (sexp-store:store-error (cause)
      (world--store-error cause))))

(-> world-inbox-deliveries (configuration string) list)
(defun world-inbox-deliveries (configuration channel-identifier)
  "Return the undelivered messages of channel CHANNEL-IDENTIFIER, oldest first."
  (handler-case
      (sexp-store:store-read (world-inbox--store configuration channel-identifier))
    (sexp-store:store-error (cause)
      (world--store-error cause))))

(-> world-person-unread (configuration) (integer 0))
(defun world-person-unread (configuration)
  "Return how many messages wait unread in the person's inbox, none before the inbox exists."
  (let ((inbox (world-inbox-channel (world-load configuration))))
    (if inbox
        (length (world-inbox-deliveries configuration (channel-identifier inbox)))
        0)))

(-> world-inbox-append (configuration string list) null)
(defun world-inbox-append (configuration channel-identifier deliveries)
  "Add DELIVERIES to channel CHANNEL-IDENTIFIER's inbox, ignoring identifiers already present in that inbox."
  (world-inbox--transact
   configuration channel-identifier
   (lambda (current)
     (let ((added (remove-if (lambda (delivery)
                               (find (world-delivery-identifier delivery) current
                                     :key #'world-delivery-identifier :test #'string=))
                             deliveries)))
       (when (> (+ (length current) (length added)) *world-inbox-limit*)
         (error 'world-error
                :message    (format nil "Channel ~A's inbox holds ~D undelivered messages; the new deliveries would exceed its limit."
                                    (conversation-identifier-display channel-identifier)
                                    (length current))
                :pathname   (world-inbox-pathname configuration channel-identifier)
                :operation  ':send
                :identifier channel-identifier))
       (values (append current added) nil (not (null added))))))
  nil)

(-> world-inbox-settle (configuration string list) null)
(defun world-inbox-settle (configuration channel-identifier identifiers)
  "Remove the deliveries named by IDENTIFIERS from channel CHANNEL-IDENTIFIER's inbox."
  (when identifiers
    (world-inbox--transact
     configuration channel-identifier
     (lambda (current)
       (let ((kept (remove-if (lambda (delivery)
                                (member (world-delivery-identifier delivery) identifiers
                                        :test #'string=))
                              current)))
         (values kept nil (< (length kept) (length current)))))))
  nil)

(defparameter *world-delivery-receipt-prefix* "channel:"
  "What begins the pending-input identifier of a channel delivery's receipt.")

(-> world-delivery-receipt (string) string)
(defun world-delivery-receipt (identifier)
  "Return the pending-input identifier whose durable receipt settles delivery IDENTIFIER."
  (concatenate 'string *world-delivery-receipt-prefix* identifier))

(-> world-delivery-receipt-delivery (string) string)
(defun world-delivery-receipt-delivery (receipt)
  "Return the delivery identifier channel delivery RECEIPT settles."
  (subseq receipt (length *world-delivery-receipt-prefix*)))

(-> world-delivery-pending-identifier (world-delivery) string)
(defun world-delivery-pending-identifier (delivery)
  "Return the pending-input identifier whose durable receipt settles DELIVERY."
  (world-delivery-receipt (world-delivery-identifier delivery)))

(-> world-delivery-pending-identifier-p (t) boolean)
(defun world-delivery-pending-identifier-p (identifier)
  "Return true when IDENTIFIER is the receipt of a channel delivery."
  (and (stringp identifier)
       (uiop:string-prefix-p *world-delivery-receipt-prefix* identifier)
       t))

(-> world-inbox-unsettled (configuration string conversation) (values list list))
(defun world-inbox-unsettled (configuration channel-identifier conversation)
  "Return CHANNEL-IDENTIFIER's deliveries CONVERSATION has not received, and the settled identifiers."
  (let ((pending nil) (settled nil))
    (dolist (delivery (world-inbox-deliveries configuration channel-identifier))
      (if (conversation-pending-input-identifier-p
           conversation (world-delivery-pending-identifier delivery))
          (push (world-delivery-identifier delivery) settled)
          (push delivery pending)))
    (values (nreverse pending) (nreverse settled))))


;;;; -- Outbox Store --

;;; A send is recorded, with its identifier and every inbox it must reach,
;;; before any inbox is written. Each copy leaves the record once it is in
;;; its inbox, and the record goes once every copy is. A copy an inbox
;;; refused, or a crash between two inboxes, leaves the record to be resumed
;;; under the same identifier, which inboxes use to reject copies they still hold.

(-> world-outbox-pathname (configuration string) pathname)
(defun world-outbox-pathname (configuration identifier)
  "Return the outbox snapshot of the send IDENTIFIER."
  (merge-pathnames (make-pathname :name identifier :type "sexp")
                   (merge-pathnames "world/outbox/" (config :data-root configuration))))

(-> world-outbox--form-p (t) boolean)
(defun world-outbox--form-p (form)
  "Return true when FORM is a complete outbox snapshot: a send's copies not yet in their inboxes."
  (and (listp form)
       (eq (first form) ':outbox)
       (sexp-store:plist-schema-p (rest form)
                                 :required-keys  '(:version :id :pending)
                                 :allowed-keys   '(:version :id :pending)
                                 :keyword-keys-p t)
       (eql (getf (rest form) :version) *world-outbox-version*)
       (non-empty-string-p (getf (rest form) :id))
       (let ((pending (getf (rest form) :pending)))
         (and (listp pending)
              (handler-case (list-length pending) (error () nil))
              (every (lambda (entry)
                       (and (consp entry)
                            (non-empty-string-p (first entry))
                            (world-delivery--record-p (rest entry))))
                     pending)
              t))))

(-> world-outbox--store (configuration string) sexp-store:snapshot-store)
(defun world-outbox--store (configuration identifier)
  "Describe the outbox snapshot of the send IDENTIFIER and its paths."
  (let ((pathname (world-outbox-pathname configuration identifier)))
    (ensure-directories-exist pathname)
    (make-instance
     'sexp-store:snapshot-store
     :pathname      pathname
     :lock-pathname (readable-state-lock-pathname pathname (format nil "~A.lock" identifier))
     :initial-state (lambda () nil)
     :validator     #'world-outbox--form-p
     :decoder       (lambda (form)
                      (mapcar (lambda (entry)
                                (cons (first entry)
                                      (world-delivery--record->delivery (rest entry))))
                              (getf (rest form) :pending)))
     :encoder       (lambda (pairs)
                      (list :outbox
                            :version *world-outbox-version*
                            :id      identifier
                            :pending (mapcar (lambda (pair)
                                               (cons (first pair)
                                                     (world-delivery--record (rest pair))))
                                             pairs))))))

(-> world-outbox--transact
    (configuration string function &key (:publish (option function))) t)
(defun world-outbox--transact (configuration identifier update &key publish)
  "Apply UPDATE to send IDENTIFIER's fresh pending copies, then PUBLISH under the same lock."
  (handler-case
      (sexp-store:store-transact (world-outbox--store configuration identifier) update
                                 :publish publish)
    (sexp-store:store-error (cause)
      (world--store-error cause))))

(-> world-outbox-write (configuration string list) null)
(defun world-outbox-write (configuration identifier pairs)
  "Record the send IDENTIFIER with PAIRS, its (CHANNEL-ID . DELIVERY) copies still to deliver."
  (world-outbox--transact configuration identifier
                          (lambda (current)
                            (declare (ignore current))
                            (values pairs nil t)))
  nil)

(-> world-outbox-pending (configuration string) list)
(defun world-outbox-pending (configuration identifier)
  "Return the (CHANNEL-ID . DELIVERY) copies of send IDENTIFIER not yet in their inboxes."
  (handler-case
      (values (sexp-store:store-read (world-outbox--store configuration identifier)))
    (sexp-store:store-error (cause)
      (world--store-error cause))))

(-> world-outbox-identifiers (configuration) list)
(defun world-outbox-identifiers (configuration)
  "Return the send identifiers with outbox snapshot files."
  (let ((directory (merge-pathnames "world/outbox/" (config :data-root configuration))))
    (if (uiop:directory-exists-p directory)
        (mapcar #'pathname-name (uiop:directory-files directory "*.sexp"))
        nil)))

(-> world-message--deliver (configuration string) (values list list))
(defun world-message--deliver (configuration identifier)
  "Deliver send IDENTIFIER's pending copies under its outbox lock.

Return the reached channels and (CHANNEL-ID . CONDITION) pairs for refused
inbox appends. Retain refused copies for retry. Always read pending copies
under the lock, so a stale resumer cannot repeat a completed send.
The lock order is outbox then inbox; receipt handlers release the inbox
lock before resuming sends."
  (destructuring-bind (delivered refused)
      (world-outbox--transact
       configuration identifier
       (lambda (pending)
         (let ((delivered nil)
               (refused   nil)
               (kept      nil))
           (dolist (pair pending)
             (handler-case
                 (progn
                   (world-inbox-append configuration (first pair) (list (rest pair)))
                   (push (first pair) delivered))
               (world-error (condition)
                 (push pair kept)
                 (push (cons (first pair) condition) refused))))
           (values (nreverse kept)
                   (list (nreverse delivered) (nreverse refused))
                   (not (null delivered)))))
       :publish (lambda (pending)
                  (unless pending
                    (uiop:delete-file-if-exists (world-outbox-pathname configuration identifier)))))
    (values delivered refused)))

(-> world-message-resume (configuration &key (:notify-p boolean)) list)
(defun world-message-resume (configuration &key (notify-p t))
  "Deliver the copies earlier sends left in the outbox and return the channels that received one.

With NOTIFY-P, the process serving each of those channels is told to read."
  (let ((reached (loop for identifier in (world-outbox-identifiers configuration)
                       append (world-message--deliver configuration identifier))))
    (when (and notify-p reached)
      (world-message-notify configuration reached))
    reached))


;;;; -- Model-Facing Text --

(-> world-delivery-label (world-delivery) string)
(defun world-delivery-label (delivery)
  "Return DELIVERY as the labeled message its recipients read.

The first line says who sent it, the person or an agent, and why it arrived;
every line of the message itself is quoted under it, so no text a sender
writes can pass for that header."
  (format nil "[Message from ~@[~A ~]~A (~A) ~A]~%~{> ~A~^~%~}"
          (case (world-delivery-sender-kind delivery)
            (:human
             "the person")
            (:agent
             "agent")
            (otherwise
             nil))
          (world-delivery-sender-name delivery)
          (conversation-identifier-display (world-delivery-sender delivery))
          (let ((target (conversation-identifier-display (world-delivery-target delivery))))
            (ecase (world-delivery-kind delivery)
              (:post
               (format nil "to channel ~A, where you reside" target))
              (:subscription
               (format nil "to channel ~A, which you subscribe to" target))
              (:mention
               (format nil "to channel ~A, mentioning you" target))
              (:notice
               (format nil "to channel ~A, for the person" target))))
          (uiop:split-string (world-delivery-text delivery) :separator '(#\Newline))))

(-> world-delivery-steering-input (world-delivery) agent-steering-input)
(defun world-delivery-steering-input (delivery)
  "Return DELIVERY as steering input that records the same request as a delivery between turns."
  (agent-steering-input-create :identifier        (world-delivery-pending-identifier delivery)
                               :content           (world-delivery-label delivery)
                               :operation-request (world-delivery-request delivery)))

(-> world-delivery-request (world-delivery) list)
(defun world-delivery-request (delivery)
  "Return the portable operation-request metadata the transcript shows for DELIVERY."
  (list :operation "channel"
        :summary   (world-delivery-text delivery)
        :message   (list :id          (world-delivery-identifier delivery)
                         :sender      (world-delivery-sender delivery)
                         :sender-name (world-delivery-sender-name delivery)
                         :source      (world-delivery-source delivery)
                         :target      (world-delivery-target delivery)
                         :kind        (world-delivery-kind delivery)
                         :mentions    (copy-list (world-delivery-mentions delivery)))))


;;;; -- Mentions --

(-> world-message-participant (world string &key (:sender (option participant))) (option participant))
(defun world-message-participant (world designator &key sender)
  "Return the participant DESIGNATOR names by identifier, display identifier or unique name.

With SENDER, @parent names the participant its message to its parent
reaches; see `world-message-parent-recipient'."
  (let ((canonical (handler-case (identifier-normalize designator)
                     (identifier-error ()
                       nil))))
    (or (and sender
             (world-handle-reserved-p designator)
             (world-message-parent-recipient world sender))
        (world-participant-find world designator)
        (and canonical (world-participant-find world canonical))
        (let ((named (remove-if-not (lambda (participant)
                                      (string-equal (participant-name participant) designator))
                                    (world-participants world))))
          (and (= (length named) 1) (first named))))))

(-> world-message-parent-recipient (world participant) (option participant))
(defun world-message-parent-recipient (world sender)
  "Return the participant a message from SENDER to @parent reaches.

That is SENDER's parent. A parked parent resides nowhere, so the message
goes on to the nearest ancestor that resides somewhere, and to the person
when no such agent is left."
  (let ((current (let ((parent (participant-parent sender)))
                   (and parent (world-participant-find world parent)))))
    (loop while current
          do (if (participant-residence current)
                 (return current)
                 (setf current (let ((parent (participant-parent current)))
                                 (and parent (world-participant-find world parent)))))
          finally (return (world-human-participant world)))))

(-> world-mention-boundary-p (string (integer 0)) boolean)
(defun world-mention-boundary-p (text at)
  "Return true when the @ at index AT of TEXT begins a mention rather than continuing a word.

A mention starts the text or follows a character other than a letter,
digit, period, hyphen, underscore, plus sign, slash or backslash.
For example, alice@bob.com is not a mention."
  (or (zerop at)
      (let ((before (char text (1- at))))
        (not (or (alphanumericp before)
                 (member before '(#\. #\- #\_ #\+ #\/ #\\)))))))

(-> world-mention-end (string (integer 0)) (integer 0))
(defun world-mention-end (text at)
  "Return the index after the designator of the mention beginning with the @ at AT in TEXT."
  (or (position-if-not (lambda (character)
                         (or (alphanumericp character)
                             (member character '(#\- #\_))))
                       text :start (1+ at))
      (length text)))

(-> world-message-mentions (world string &key (:sender (option participant))) list)
(defun world-message-mentions (world text &key sender)
  "Return the identifiers of the participants TEXT mentions as @name or @identifier.

An @ immediately after a letter or digit, as in alice@bob.com, mentions
nobody. With SENDER, @parent mentions the participant its message to its
parent reaches."
  (let ((mentions nil) (start 0))
    (loop
      (let ((at (position #\@ text :start start)))
        (unless at (return))
        (let* ((end         (world-mention-end text at))
               (designator  (subseq text (1+ at) end))
               (participant (and (world-mention-boundary-p text at)
                                 (plusp (length designator))
                                 (world-message-participant world designator :sender sender))))
          (when (and participant
                     (not (member (participant-identifier participant) mentions
                                  :test #'string=)))
            (push (participant-identifier participant) mentions))
          (setf start (max end (1+ at))))))
    (nreverse mentions)))


;;;; -- Sending --

(-> world-message--text (t) string)
(defun world-message--text (text)
  "Return TEXT when it is a bounded message."
  (unless (and (non-empty-string-p text) (<= (length text) *world-message-text-limit*))
    (error 'world-error
           :message   (format nil "A channel message is 1 to ~D characters of text."
                              *world-message-text-limit*)
           :pathname  nil
           :operation ':send))
  text)

(-> world-message-plan
    (world &key (:sender participant) (:channel channel) (:text string) (:mentions list)
                (:identifier string) (:now timestamp) (:post-p boolean)
                (:subscribers-p boolean) (:source (option string)))
    list)
(defun world-message-plan (world &key sender channel text mentions identifier
                                      (now (get-universal-time)) (post-p t)
                                      (subscribers-p post-p) source)
  "Return the deliveries one message from SENDER to CHANNEL produces, as (CHANNEL-ID . DELIVERY) pairs.

The channel itself gets a post for its residents. Except for the sender, each
subscriber and each mentioned participant residing elsewhere gets a copy in its
residence channel, one per channel, subscriptions before mentions, except that a
notice for the person replaces a subscription copy, so the person always sees the
message in full; a parked agent resides nowhere and gets none. Without POST-P the
channel gets nothing: the message was said there already. SUBSCRIBERS-P, which
follows POST-P unless given, says whether its subscribers still hear of it, as they
do of every ordinary message said in the channel. SOURCE is the channel the message
was said from, the sender's residence unless given: the person resides in the inbox
but speaks in the channel on screen."
  (let ((target  (channel-identifier channel))
        (pairs   nil)
        (covered (if post-p nil (list (channel-identifier channel)))))
    (labels ((delivery (kind)
               "Return one copy of the message of KIND."
               (make-instance 'world-delivery
                              :identifier  identifier
                              :time        now
                              :sender      (participant-identifier sender)
                              :sender-name (participant-name sender)
                              :sender-kind (participant-kind sender)
                              :source      (or source (participant-residence sender))
                              :target      target
                              :text        text
                              :mentions    mentions
                              :kind        kind))

             (deliver (channel-identifier kind)
               "Plan one copy of KIND for CHANNEL-IDENTIFIER unless it has one already."
               (let ((planned (assoc channel-identifier pairs :test #'string=)))
                 (cond
                   ((not (member channel-identifier covered :test #'string=))
                    (push channel-identifier covered)
                    (push (cons channel-identifier (delivery kind)) pairs))
                   ((and planned
                         (eq kind ':notice)
                         (eq (world-delivery-kind (rest planned)) ':subscription))
                    (setf (rest planned) (delivery kind)))
                   (t
                    nil)))))
      ;; A participant speaking in its own residence is the one saying it
      ;; there, so that channel gets no copy to read back.
      (when (and post-p (not (equal target (participant-residence sender))))
        (deliver target ':post))
      (when subscribers-p
        (dolist (subscriber (world-channel-subscribers world target))
          (unless (or (string= (participant-identifier subscriber) (participant-identifier sender))
                      (null (participant-residence subscriber)))
            (deliver (participant-residence subscriber) ':subscription))))
      (dolist (mention mentions)
        (let ((mentioned (world-participant-find world mention)))
          (when (and mentioned
                     (participant-residence mentioned)
                     (not (string= mention (participant-identifier sender))))
            ;; A message for the person is shown to the person, not to the
            ;; agent of the channel the person happens to be in.
            (deliver (participant-residence mentioned)
                     (if (eq (participant-kind mentioned) ':human) ':notice ':mention))))))
    (nreverse pairs)))

(-> world-message-notify (configuration list) list)
(defun world-message-notify (configuration channel-identifiers)
  "Tell the live process serving each of CHANNEL-IDENTIFIERS to read its inbox.

Returns the identifiers that were reached. A channel nobody serves keeps its
inbox until a process installs its conversation. When one of them is the
person's inbox, every channel the world server serves shows the new unread
count."
  (let ((records (handler-case (localgroup-endpoint-records configuration)
                   (error ()
                     nil)))
        (reached nil))
    (dolist (channel-identifier channel-identifiers)
      (let ((record (find channel-identifier records
                          :key  (lambda (entry) (getf (rest (rest entry)) :session-id))
                          :test #'equal)))
        (when record
          (handler-case
              (progn
                (image-daemon:daemon-message-call (rest record)
                                                  :action   ':channel
                                                  :id       channel-identifier
                                                  :sender   channel-identifier
                                                  :receiver channel-identifier
                                                  :payload  nil
                                                  :timeout  2)
                (push channel-identifier reached))
            (error ()
              nil)))))
    (when (world-message--person-inbox-among-p configuration channel-identifiers)
      (application-world-notify-person configuration))
    (when *world-message-unserved-function*
      (dolist (channel-identifier channel-identifiers)
        (unless (member channel-identifier reached :test #'string=)
          (funcall *world-message-unserved-function* channel-identifier))))
    (nreverse reached)))

(-> world-message--person-inbox-among-p (configuration list) boolean)
(defun world-message--person-inbox-among-p (configuration channel-identifiers)
  "Return true when the person's inbox is one of CHANNEL-IDENTIFIERS, false when the world cannot be read."
  (let ((inbox (handler-case (world-inbox-channel (world-load configuration))
                 (autolith-error ()
                   nil))))
    (and inbox
         (member (channel-identifier inbox) channel-identifiers :test #'string=)
         t)))

(-> world-message-send
    (configuration &key (:sender string) (:channel string) (:text string) (:mentions list)
                        (:notify-p boolean) (:now timestamp) (:post-p boolean)
                        (:subscribers-p boolean) (:source (option string)))
    list)
(defun world-message-send (configuration &key sender channel text mentions (notify-p t)
                                              (now (get-universal-time)) (post-p t)
                                              (subscribers-p post-p) source)
  "Send TEXT from participant SENDER to channel CHANNEL and return what happened.

MENTIONS are explicit participant designators added to the @mentions found in
TEXT. Without POST-P the channel itself gets no copy; SUBSCRIBERS-P, which defaults
to POST-P, controls subscriber copies. SOURCE names the channel it was said from, as
in `world-message-plan'. The send is recorded before any inbox is written, so a copy
an inbox refuses, or one a crash skips, is delivered later under the same identifier.
The result is a plist of :id, :channels (channels whose delivery attempt completed),
:pending (channels whose attempt raised WORLD-ERROR), :parked (the names of the
mentioned agents that are parked and so got no copy) and :notified (the channels
whose serving process was reached)."
  (world-message-resume configuration :notify-p notify-p)
  (let* ((world       (world-load configuration))
         (participant (or (world-participant-find world sender)
                          (error 'world-error
                                 :message    (format nil "Participant ~A does not exist." sender)
                                 :pathname   nil
                                 :operation  ':send
                                 :identifier sender)))
         (target      (world-channel-designated world configuration channel))
         (text        (world-message--text text))
         (identifier  (make-identifier)))
    (let* ((mentioned (append (world-message-mentions world text :sender participant)
                              (loop for designator in mentions
                                    for found = (world-message-participant world designator
                                                                           :sender participant)
                                    do (unless found
                                         (error 'world-error
                                                :message    (format nil "No participant is named ~A." designator)
                                                :pathname   nil
                                                :operation  ':send
                                                :identifier designator))
                                    collect (participant-identifier found))))
           (pairs     (world-message-plan world
                                          :sender        participant
                                          :channel       target
                                          :text          text
                                          :mentions      (remove-duplicates mentioned :test     #'string=
                                                                                      :from-end t)
                                          :identifier    identifier
                                          :now           now
                                          :post-p        post-p
                                          :subscribers-p subscribers-p
                                          :source        source)))
      (when pairs
        (world-outbox-write configuration identifier pairs))
      (multiple-value-bind (delivered refused)
          (world-message--deliver configuration identifier)
        (list :id       identifier
              :channels delivered
              :pending  (mapcar #'first refused)
              :parked   (loop for mention in mentioned
                              for participant = (world-participant-find world mention)
                              when (and participant (null (participant-residence participant)))
                                collect (participant-name participant))
              :notified (and notify-p (world-message-notify configuration delivered)))))))


;;;; -- Steering Delivery --

(-> world-inbox-steering-take
    (configuration &key (:channel string) (:conversation conversation) (:taken hash-table))
    list)
(defun world-inbox-steering-take (configuration &key channel conversation taken)
  "Return CHANNEL's unread messages, except :NOTICE deliveries, for CONVERSATION as labeled steering input.

TAKEN remembers what this process took, so each message is taken once.
Deliveries CONVERSATION already recorded are settled on the way."
  (multiple-value-bind (pending settled)
      (world-inbox-unsettled configuration channel conversation)
    (when settled
      (world-inbox-settle configuration channel settled)
      (dolist (identifier settled)
        (remhash (world-delivery-receipt identifier) taken)))
    (loop for delivery in pending
          for identifier = (world-delivery-pending-identifier delivery)
          unless (or (eq (gethash identifier taken) ':taken)
                     (eq (world-delivery-kind delivery) ':notice))
            collect (progn
                      (setf (gethash identifier taken) ':taken)
                      (world-delivery-steering-input delivery)))))

(-> world-inbox-steering-ack
    (configuration &key (:channel string) (:identifier string) (:taken hash-table))
    boolean)
(defun world-inbox-steering-ack (configuration &key channel identifier taken)
  "Settle the delivery IDENTIFIER names in CHANNEL's inbox, or return NIL for other input.

Settling makes room in the inbox, so a send that found it full is resumed
at once."
  (when (world-delivery-pending-identifier-p identifier)
    (world-inbox-settle configuration channel
                        (list (world-delivery-receipt-delivery identifier)))
    (remhash identifier taken)
    (world-message-resume configuration)
    t))
