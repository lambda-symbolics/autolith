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

(deftype world-delivery-kind ()
  "Why a delivery reached a channel: addressed to it, subscribed, or mentioned."
  '(member :post :subscription :mention))

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
   (source
    :initarg       :source
    :reader        world-delivery-source
    :type          non-empty-string
    :documentation "The channel the message was sent from, the sender's residence.")
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
                                 :allowed-keys   '(:id :time :sender :sender-name :source
                                                   :target :text :mentions :kind)
                                 :keyword-keys-p t)
       (let ((properties (rest form)))
         (and (non-empty-string-p (getf properties :id))
              (typep (getf properties :time) 'timestamp)
              (non-empty-string-p (getf properties :sender))
              (world--name-p (getf properties :sender-name))
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

(-> world-inbox-append (configuration string list) null)
(defun world-inbox-append (configuration channel-identifier deliveries)
  "Add DELIVERIES to channel CHANNEL-IDENTIFIER's inbox, ignoring repeated identifiers."
  (world-inbox--transact
   configuration channel-identifier
   (lambda (current)
     (let ((added (remove-if (lambda (delivery)
                               (find (world-delivery-identifier delivery) current
                                     :key #'world-delivery-identifier :test #'string=))
                             deliveries)))
       (when (> (+ (length current) (length added)) *world-inbox-limit*)
         (error 'world-error
                :message    (format nil "Channel ~A's inbox holds ~D undelivered messages; nobody is reading it."
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

(-> world-delivery-pending-identifier (world-delivery) string)
(defun world-delivery-pending-identifier (delivery)
  "Return the pending-input identifier whose durable receipt settles DELIVERY."
  (format nil "channel:~A" (world-delivery-identifier delivery)))

(-> world-delivery-pending-identifier-p (t) boolean)
(defun world-delivery-pending-identifier-p (identifier)
  "Return true when IDENTIFIER is the receipt of a channel delivery."
  (and (stringp identifier) (uiop:string-prefix-p "channel:" identifier) t))

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
;;; under the same identifier, which the inboxes deduplicate.

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

(-> world-outbox--transact (configuration string function) t)
(defun world-outbox--transact (configuration identifier update)
  "Apply UPDATE to the fresh pending copies of send IDENTIFIER and return its result."
  (handler-case
      (sexp-store:store-transact (world-outbox--store configuration identifier) update)
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
  "Return the identifiers of the sends with copies still to deliver."
  (let ((directory (merge-pathnames "world/outbox/" (config :data-root configuration))))
    (if (uiop:directory-exists-p directory)
        (mapcar #'pathname-name (uiop:directory-files directory "*.sexp"))
        nil)))

(-> world-outbox--finish (configuration string string) null)
(defun world-outbox--finish (configuration identifier channel-identifier)
  "Drop the copy of send IDENTIFIER for CHANNEL-IDENTIFIER, and the record once none is left."
  (let ((remaining (world-outbox--transact
                    configuration identifier
                    (lambda (current)
                      (let ((rest (remove channel-identifier current
                                          :key #'first :test #'string=)))
                        (values rest rest t))))))
    (unless remaining
      (uiop:delete-file-if-exists (world-outbox-pathname configuration identifier))))
  nil)

(-> world-message--deliver (configuration string list) (values list list))
(defun world-message--deliver (configuration identifier pairs)
  "Put each copy of send IDENTIFIER in PAIRS into its inbox, leaving refused ones in the outbox.

Return the channels that received their copy and (CHANNEL-ID . CONDITION)
pairs for the inboxes that refused one. An inbox that holds the copy
already keeps the one it has."
  (let ((delivered nil)
        (refused   nil))
    (dolist (pair pairs)
      (handler-case
          (progn
            (world-inbox-append configuration (first pair) (list (rest pair)))
            (world-outbox--finish configuration identifier (first pair))
            (push (first pair) delivered))
        (world-error (condition)
          (push (cons (first pair) condition) refused))))
    (values (nreverse delivered) (nreverse refused))))

(-> world-message-resume (configuration &key (:notify-p boolean)) list)
(defun world-message-resume (configuration &key (notify-p t))
  "Deliver the copies earlier sends left in the outbox and return the channels that received one.

With NOTIFY-P, the process serving each of those channels is told to read."
  (let ((reached nil))
    (dolist (identifier (world-outbox-identifiers configuration))
      (let ((pairs (world-outbox-pending configuration identifier)))
        (if pairs
            (setf reached (append reached
                                  (world-message--deliver configuration identifier pairs)))
            (uiop:delete-file-if-exists (world-outbox-pathname configuration identifier)))))
    (when (and notify-p reached)
      (world-message-notify configuration reached))
    reached))


;;;; -- Model-Facing Text --

(-> world-delivery-label (world-delivery) string)
(defun world-delivery-label (delivery)
  "Return DELIVERY as the labeled message its recipients read."
  (format nil "[Message from ~A (~A) ~A]~%~A"
          (world-delivery-sender-name delivery)
          (conversation-identifier-display (world-delivery-sender delivery))
          (let ((target (conversation-identifier-display (world-delivery-target delivery))))
            (ecase (world-delivery-kind delivery)
              (:post
               (format nil "to channel ~A, where you reside" target))
              (:subscription
               (format nil "to channel ~A, which you subscribe to" target))
              (:mention
               (format nil "to channel ~A, mentioning you" target))))
          (world-delivery-text delivery)))

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

(-> world-message-participant (world string) (option participant))
(defun world-message-participant (world designator)
  "Return the participant DESIGNATOR names by identifier, display identifier or unique name."
  (let ((canonical (handler-case (identifier-normalize designator)
                     (identifier-error ()
                       nil))))
    (or (world-participant-find world designator)
        (and canonical (world-participant-find world canonical))
        (let ((named (remove-if-not (lambda (participant)
                                      (string-equal (participant-name participant) designator))
                                    (world-participants world))))
          (and (= (length named) 1) (first named))))))

(-> world-message-mentions (world string) list)
(defun world-message-mentions (world text)
  "Return the identifiers of the participants TEXT mentions as @name or @identifier."
  (let ((mentions nil) (start 0))
    (loop
      (let ((at (position #\@ text :start start)))
        (unless at (return))
        (let* ((end         (or (position-if-not (lambda (character)
                                                   (or (alphanumericp character)
                                                       (member character '(#\- #\_))))
                                                 text :start (1+ at))
                                (length text)))
               (designator  (subseq text (1+ at) end))
               (participant (and (plusp (length designator))
                                 (world-message-participant world designator))))
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
                (:identifier string) (:now timestamp) (:post-p boolean))
    list)
(defun world-message-plan (world &key sender channel text mentions identifier
                                      (now (get-universal-time)) (post-p t))
  "Return the deliveries one message from SENDER to CHANNEL produces, as (CHANNEL-ID . DELIVERY) pairs.

The channel itself gets a post for its residents. Each subscriber and each
mentioned participant residing elsewhere gets a copy in its residence channel,
one per channel, subscriptions before mentions. Without POST-P the channel
and its subscribers get nothing: the message was said there already, and
only the participants it mentions elsewhere hear of it."
  (let ((target  (channel-identifier channel))
        (pairs   nil)
        (covered (if post-p nil (list (channel-identifier channel)))))
    (flet ((deliver (channel-identifier kind)
             "Plan one copy of KIND for CHANNEL-IDENTIFIER unless it has one already."
             (unless (member channel-identifier covered :test #'string=)
               (push channel-identifier covered)
               (push (cons channel-identifier
                           (make-instance 'world-delivery
                                          :identifier  identifier
                                          :time        now
                                          :sender      (participant-identifier sender)
                                          :sender-name (participant-name sender)
                                          :source      (participant-residence sender)
                                          :target      target
                                          :text        text
                                          :mentions    mentions
                                          :kind        kind))
                     pairs))))
      (when post-p
        (deliver target ':post)
        (dolist (subscriber (world-channel-subscribers world target))
          (unless (string= (participant-identifier subscriber) (participant-identifier sender))
            (deliver (participant-residence subscriber) ':subscription))))
      (dolist (mention mentions)
        (let ((mentioned (world-participant-find world mention)))
          (when (and mentioned
                     (not (string= mention (participant-identifier sender))))
            (deliver (participant-residence mentioned) ':mention)))))
    (nreverse pairs)))

(-> world-message-notify (configuration list) list)
(defun world-message-notify (configuration channel-identifiers)
  "Tell the live process serving each of CHANNEL-IDENTIFIERS to read its inbox.

Returns the identifiers that were reached. A channel nobody serves keeps its
inbox until a process installs its conversation."
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
    (nreverse reached)))

(-> world-message-send
    (configuration &key (:sender string) (:channel string) (:text string) (:mentions list)
                        (:notify-p boolean) (:now timestamp) (:post-p boolean))
    list)
(defun world-message-send (configuration &key sender channel text mentions (notify-p t)
                                              (now (get-universal-time)) (post-p t))
  "Send TEXT from participant SENDER to channel CHANNEL and return what happened.

MENTIONS are explicit participant designators added to the @mentions found in
TEXT. Without POST-P only the mentioned participants residing elsewhere are
told. The send is recorded before any inbox is written, so a copy an inbox
refuses, or one a crash skips, is delivered later under the same identifier.
The result is a plist of :id, :channels (every inbox written), :pending (the
channels whose inbox refused its copy for now) and :notified (the channels
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
    (let* ((mentioned (append (world-message-mentions world text)
                              (loop for designator in mentions
                                    for found = (world-message-participant world designator)
                                    do (unless found
                                         (error 'world-error
                                                :message    (format nil "No participant is named ~A." designator)
                                                :pathname   nil
                                                :operation  ':send
                                                :identifier designator))
                                    collect (participant-identifier found))))
           (pairs     (world-message-plan world
                                          :sender     participant
                                          :channel    target
                                          :text       text
                                          :mentions   (remove-duplicates mentioned :test     #'string=
                                                                                   :from-end t)
                                          :identifier identifier
                                          :now        now
                                          :post-p     post-p)))
      (when pairs
        (world-outbox-write configuration identifier pairs))
      (multiple-value-bind (delivered refused)
          (world-message--deliver configuration identifier pairs)
        (list :id       identifier
              :channels delivered
              :pending  (mapcar #'first refused)
              :notified (and notify-p (world-message-notify configuration delivered)))))))


;;;; -- Steering Delivery --

(-> world-inbox-steering-take
    (configuration &key (:channel string) (:conversation conversation) (:taken hash-table))
    list)
(defun world-inbox-steering-take (configuration &key channel conversation taken)
  "Return CHANNEL's unread messages for CONVERSATION as labeled steering input.

TAKEN remembers what this process took, so each message is taken once.
Deliveries CONVERSATION already recorded are settled on the way."
  (multiple-value-bind (pending settled)
      (world-inbox-unsettled configuration channel conversation)
    (when settled
      (world-inbox-settle configuration channel settled)
      (dolist (identifier settled)
        (remhash (format nil "channel:~A" identifier) taken)))
    (loop for delivery in pending
          for identifier = (world-delivery-pending-identifier delivery)
          unless (eq (gethash identifier taken) ':taken)
            collect (progn
                      (setf (gethash identifier taken) ':taken)
                      (agent-steering-input-create :identifier identifier
                                                   :content    (world-delivery-label delivery))))))

(-> world-inbox-steering-ack
    (configuration &key (:channel string) (:identifier string) (:taken hash-table))
    boolean)
(defun world-inbox-steering-ack (configuration &key channel identifier taken)
  "Settle the delivery IDENTIFIER names in CHANNEL's inbox, or return NIL for other input."
  (when (world-delivery-pending-identifier-p identifier)
    (world-inbox-settle configuration channel
                        (list (subseq identifier (length "channel:"))))
    (remhash identifier taken)
    t))
