(in-package #:autolith)

;;;; -- World of Channels and Participants --

;;; The world stores human and agent participants and their channels. Each
;;; participant may reside in one channel and subscribe to or own others;
;;; parked agents have no residence. A channel's history is the conversation
;;; log with the same identifier. Residence and subscriptions are stored on
;;; the participant; channel residents and subscribers are derived. Records
;;; contain identifiers and portable data, not runtime objects.

(defparameter *world-format-version* 1
  "The readable world record format version.")

(defparameter *world-name-limit* 80
  "The maximum characters in one participant name or channel title.")

(defparameter *world-handle-limit* 32
  "The most characters in a participant handle, the name an @mention uses.")

(defparameter *world-parent-handle* "parent"
  "The mention that names the sender's parent; no participant may take it as a name.")

(defparameter *world-settings-limit* 32
  "The maximum plist elements in one participant settings plist.")

(defparameter *world-inbox-designator* "inbox"
  "The word that names the person's inbox wherever a channel is designated.")

(defvar *world-lock* (make-recursive-lock "Autolith world")
  "Serializes in-process world transactions.")

(defvar *world-followers* (make-hash-table :test #'equal)
  "This process's follower of each world log it reads, keyed by the log's namestring.

A follower folds only what other processes appended since it last read, so a
world read costs a file status and the new records, not the whole log.")

(defvar *world-checked* (make-hash-table :test #'equal)
  "The world last found free of dangling references, keyed like the followers.

A world the follower returns again unchanged needs no second check.")

(deftype participant-kind ()
  "Whether a person or an agent client operates a participant."
  '(member :human :agent))

(defclass participant ()
  ((identifier
    :initarg       :identifier
    :reader        participant-identifier
    :type          non-empty-string
    :documentation "The stable participant identifier.")
   (kind
    :initarg       :kind
    :reader        participant-kind
    :type          participant-kind
    :documentation "Whether a person or an agent client operates this participant.")
   (name
    :initarg       :name
    :reader        participant-name
    :type          non-empty-string
    :documentation "The short name shown beside the participant's messages.")
   (parent
    :initarg       :parent
    :initform      nil
    :reader        participant-parent
    :type          (option non-empty-string)
    :documentation "The participant that created this one, when any.")
   (residence
    :initarg       :residence
    :initform      nil
    :reader        participant-residence
    :type          (option non-empty-string)
    :documentation "The channel this participant resides in, or NIL for a parked agent, which resides nowhere.")
   (subscriptions
    :initarg       :subscriptions
    :initform      nil
    :reader        participant-subscriptions
    :type          list
    :documentation "The channels whose future messages reach this participant.")
   (settings
    :initarg       :settings
    :initform      nil
    :reader        participant-settings
    :type          list
    :documentation "The portable plist of settings following this participant, such as its model.")
   (created-at
    :initarg       :created-at
    :reader        participant-created-at
    :type          timestamp
    :documentation "The creation time as universal time.")
   (updated-at
    :initarg       :updated-at
    :reader        participant-updated-at
    :type          timestamp
    :documentation "The last change time as universal time."))
  (:documentation "One human or agent identity with private state and an optional channel residence."))

(defclass channel ()
  ((identifier
    :initarg       :identifier
    :reader        channel-identifier
    :type          non-empty-string
    :documentation "The stable channel identifier, also naming its history conversation.")
   (parent
    :initarg       :parent
    :initform      nil
    :reader        channel-parent
    :type          (option non-empty-string)
    :documentation "The channel this one nests under, when any.")
   (owner
    :initarg       :owner
    :reader        channel-owner
    :type          non-empty-string
    :documentation "The participant that controls this channel.")
   (workspace
    :initarg       :workspace
    :reader        channel-workspace
    :type          non-empty-string
    :documentation "The canonical working directory new work in this channel uses.")
   (title
    :initarg       :title
    :initform      nil
    :reader        channel-title
    :type          (option non-empty-string)
    :documentation "The human-readable channel title, when one is known.")
   (inbox-p
    :initarg       :inbox-p
    :initform      nil
    :reader        channel-inbox-p
    :type          boolean
    :documentation "Whether this is the person's inbox, where the person resides and no agent ever does.")
   (created-at
    :initarg       :created-at
    :reader        channel-created-at
    :type          timestamp
    :documentation "The creation time as universal time.")
   (updated-at
    :initarg       :updated-at
    :reader        channel-updated-at
    :type          timestamp
    :documentation "The last change time as universal time."))
  (:documentation "One addressable conversation and execution location."))

(defclass world ()
  ((participants
    :initform      (make-hash-table :test #'equal)
    :reader        world--participant-table
    :type          hash-table
    :documentation "Participants by identifier.")
   (channels
    :initform      (make-hash-table :test #'equal)
    :reader        world--channel-table
    :type          hash-table
    :documentation "Channels by identifier."))
  (:documentation "The folded state of every participant and channel."))


;;;; -- Records --

(-> world--name-p (t) boolean)
(defun world--name-p (value)
  "Return true when VALUE is a bounded single-line name."
  (and (non-empty-string-p value)
       (<= (length value) *world-name-limit*)
       (notany (lambda (character) (member character '(#\Newline #\Return))) value)))

(-> world-handle-p (t) boolean)
(defun world-handle-p (value)
  "Return true when VALUE is a handle: letters, digits, hyphens and underscores, bounded in length."
  (and (non-empty-string-p value)
       (<= (length value) *world-handle-limit*)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-) (char= character #\_)))
              value)))

(-> world-handle-base (string &key (:fallback string)) string)
(defun world-handle-base (text &key (fallback "agent"))
  "Return TEXT as the base of a handle, or FALLBACK when nothing of it remains.

Letters are lowercased, digits kept, and every run of other characters
becomes one hyphen, with no hyphen at either end."
  (let ((base (string-trim "-"
                           (with-output-to-string (stream)
                             (let ((hyphen-p nil))
                               (loop for character across text
                                     do (cond
                                          ((alphanumericp character)
                                           (write-char (char-downcase character) stream)
                                           (setf hyphen-p nil))
                                          ((not hyphen-p)
                                           (write-char #\- stream)
                                           (setf hyphen-p t)))))))))
    (if (world-handle-p base)
        (subseq base 0 (min (length base) *world-handle-limit*))
        fallback)))

(-> world-workspace-leaf (string) string)
(defun world-workspace-leaf (workspace)
  "Return the last directory name of WORKSPACE, or WORKSPACE itself."
  (let ((name (first (last (pathname-directory (uiop:ensure-directory-pathname workspace))))))
    (if (stringp name)
        name
        workspace)))

(-> world--identifier-list-p (t) boolean)
(defun world--identifier-list-p (value)
  "Return true when VALUE is a proper list of unique identifiers."
  (and (listp value)
       (handler-case (list-length value) (error () nil))
       (every #'non-empty-string-p value)
       (= (length value) (length (remove-duplicates value :test #'string=)))))

(-> world--portable-value-p (t) boolean)
(defun world--portable-value-p (value)
  "Return true when VALUE is a string, keyword, rational number, T or NIL."
  (or (stringp value) (keywordp value) (rationalp value) (eq value t) (null value)))

(-> world-settings-p (t) boolean)
(defun world-settings-p (value)
  "Return true when VALUE is a bounded plist of keywords to portable values."
  (and (sexp-store:plist-schema-p value
                                 :keyword-keys-p t
                                 :maximum-length *world-settings-limit*)
       (loop for (nil item) on value by (partial #'nthcdr 2)
             always (world--portable-value-p item))))

(-> world-portable-string ((option string)) (option string))
(defun world-portable-string (value)
  "Return VALUE, a string or NIL, as a fresh string of characters.

Identifiers often come from pathnames as base strings, which some Lisps
print with implementation syntax; and one string written twice prints as a
shared reference. A fresh character string avoids both, so a file of world
records stays plain, portable text."
  (and value (replace (make-string (length value)) value)))

(-> world--participant-record (participant) list)
(defun world--participant-record (participant)
  "Return PARTICIPANT as one portable readable record."
  (list :participant
        :version       *world-format-version*
        :id            (world-portable-string (participant-identifier participant))
        :kind          (participant-kind participant)
        :name          (world-portable-string (participant-name participant))
        :parent        (world-portable-string (participant-parent participant))
        :residence     (world-portable-string (participant-residence participant))
        :subscriptions (mapcar #'world-portable-string (participant-subscriptions participant))
        :settings      (copy-list (participant-settings participant))
        :created-at    (participant-created-at participant)
        :updated-at    (participant-updated-at participant)))

(-> world--channel-record (channel) list)
(defun world--channel-record (channel)
  "Return CHANNEL as one portable readable record."
  (list :channel
        :version    *world-format-version*
        :id         (world-portable-string (channel-identifier channel))
        :parent     (world-portable-string (channel-parent channel))
        :owner      (world-portable-string (channel-owner channel))
        :workspace  (world-portable-string (channel-workspace channel))
        :title      (world-portable-string (channel-title channel))
        :inbox-p    (channel-inbox-p channel)
        :created-at (channel-created-at channel)
        :updated-at (channel-updated-at channel)))

(-> world--record->participant (pathname list) participant)
(defun world--record->participant (pathname record)
  "Validate and convert one participant RECORD read from PATHNAME."
  (let ((properties (rest record)))
    (unless (and (eql (getf properties :version) *world-format-version*)
                 (non-empty-string-p (getf properties :id))
                 (typep (getf properties :kind) 'participant-kind)
                 (world--name-p (getf properties :name))
                 (or (null (getf properties :parent))
                     (non-empty-string-p (getf properties :parent)))
                 (or (non-empty-string-p (getf properties :residence))
                     (and (null (getf properties :residence))
                          (eq (getf properties :kind) ':agent)))
                 (world--identifier-list-p (getf properties :subscriptions))
                 (world-settings-p (getf properties :settings))
                 (typep (getf properties :created-at) 'timestamp)
                 (typep (getf properties :updated-at) 'timestamp)
                 (<= (getf properties :created-at) (getf properties :updated-at)))
      (error 'world-error
             :message    "A participant record has invalid metadata."
             :pathname   pathname
             :operation  ':read
             :identifier (let ((identifier (getf properties :id)))
                           (and (stringp identifier) identifier))))
    (make-instance 'participant
                   :identifier    (getf properties :id)
                   :kind          (getf properties :kind)
                   :name          (getf properties :name)
                   :parent        (getf properties :parent)
                   :residence     (getf properties :residence)
                   :subscriptions (copy-list (getf properties :subscriptions))
                   :settings      (copy-list (getf properties :settings))
                   :created-at    (getf properties :created-at)
                   :updated-at    (getf properties :updated-at))))

(-> world--record->channel (pathname list) channel)
(defun world--record->channel (pathname record)
  "Validate and convert one channel RECORD read from PATHNAME."
  (let ((properties (rest record)))
    (unless (and (eql (getf properties :version) *world-format-version*)
                 (non-empty-string-p (getf properties :id))
                 (or (null (getf properties :parent))
                     (non-empty-string-p (getf properties :parent)))
                 (non-empty-string-p (getf properties :owner))
                 (non-empty-string-p (getf properties :workspace))
                 (or (null (getf properties :title))
                     (world--name-p (getf properties :title)))
                 (typep (getf properties :inbox-p) 'boolean)
                 (typep (getf properties :created-at) 'timestamp)
                 (typep (getf properties :updated-at) 'timestamp)
                 (<= (getf properties :created-at) (getf properties :updated-at)))
      (error 'world-error
             :message    "A channel record has invalid metadata."
             :pathname   pathname
             :operation  ':read
             :identifier (let ((identifier (getf properties :id)))
                           (and (stringp identifier) identifier))))
    (make-instance 'channel
                   :identifier (getf properties :id)
                   :parent     (getf properties :parent)
                   :owner      (getf properties :owner)
                   :workspace  (getf properties :workspace)
                   :title      (getf properties :title)
                   :inbox-p    (getf properties :inbox-p)
                   :created-at (getf properties :created-at)
                   :updated-at (getf properties :updated-at))))

(-> world--tombstone-identifier (pathname list) string)
(defun world--tombstone-identifier (pathname record)
  "Return the identifier a deletion RECORD names after validating it."
  (let ((properties (rest record)))
    (unless (and (eql (getf properties :version) *world-format-version*)
                 (non-empty-string-p (getf properties :id))
                 (typep (getf properties :time) 'timestamp))
      (error 'world-error
             :message    "A world deletion record has invalid metadata."
             :pathname   pathname
             :operation  ':read
             :identifier (let ((identifier (getf properties :id)))
                           (and (stringp identifier) identifier))))
    (getf properties :id)))


;;;; -- Folding --

(-> world--copy-participant (participant &rest t) participant)
(defun world--copy-participant (participant &rest changes)
  "Return PARTICIPANT with CHANGES, a plist of initargs, applied."
  (apply #'make-instance 'participant
         (append changes
                 (list :identifier    (participant-identifier participant)
                       :kind          (participant-kind participant)
                       :name          (participant-name participant)
                       :parent        (participant-parent participant)
                       :residence     (participant-residence participant)
                       :subscriptions (copy-list (participant-subscriptions participant))
                       :settings      (copy-list (participant-settings participant))
                       :created-at    (participant-created-at participant)
                       :updated-at    (participant-updated-at participant)))))

(-> world--copy-channel (channel &rest t) channel)
(defun world--copy-channel (channel &rest changes)
  "Return CHANNEL with CHANGES, a plist of initargs, applied."
  (apply #'make-instance 'channel
         (append changes
                 (list :identifier (channel-identifier channel)
                       :parent     (channel-parent channel)
                       :owner      (channel-owner channel)
                       :workspace  (channel-workspace channel)
                       :title      (channel-title channel)
                       :inbox-p    (channel-inbox-p channel)
                       :created-at (channel-created-at channel)
                       :updated-at (channel-updated-at channel)))))

(-> world--copy (world) world)
(defun world--copy (world)
  "Return a world holding the same participants and channels."
  (let ((copy (make-instance 'world)))
    (maphash (lambda (identifier participant)
               (setf (gethash identifier (world--participant-table copy)) participant))
             (world--participant-table world))
    (maphash (lambda (identifier channel)
               (setf (gethash identifier (world--channel-table copy)) channel))
             (world--channel-table world))
    copy))

(-> world--reduce-record (pathname world list) world)
(defun world--reduce-record (pathname world record)
  "Apply one RECORD to fresh WORLD state read from PATHNAME."
  (case (first record)
    (:participant
     (let ((participant (world--record->participant pathname record)))
       (setf (gethash (participant-identifier participant)
                      (world--participant-table world))
             participant)))
    (:participant-deleted
     (remhash (world--tombstone-identifier pathname record)
              (world--participant-table world)))
    (:channel
     (let ((channel (world--record->channel pathname record)))
       (setf (gethash (channel-identifier channel) (world--channel-table world))
             channel)))
    (:channel-deleted
     (let ((identifier (world--tombstone-identifier pathname record)))
       (remhash identifier (world--channel-table world))
       (maphash (lambda (key participant)
                  (when (member identifier (participant-subscriptions participant)
                                :test #'string=)
                    (setf (gethash key (world--participant-table world))
                          (world--copy-participant
                           participant
                           :subscriptions (remove identifier
                                                  (participant-subscriptions participant)
                                                  :test #'string=)))))
                (world--participant-table world))))
    (otherwise
     (error 'world-error
            :message   (format nil "Unsupported world record ~S." (first record))
            :pathname  pathname
            :operation ':read)))
  world)

(-> world--check-integrity (pathname world &key (:operation keyword)) world)
(defun world--check-integrity (pathname world &key (operation ':read))
  "Signal WORLD-ERROR unless every reference in WORLD names an existing record."
  (flet ((fail (message identifier)
           (error 'world-error
                  :message    message
                  :pathname   pathname
                  :operation  operation
                  :identifier identifier)))
    (maphash (lambda (identifier participant)
               (let ((residence (participant-residence participant)))
                 (unless (or (and (null residence) (eq (participant-kind participant) ':agent))
                             (and residence (gethash residence (world--channel-table world))))
                   (fail "A participant resides in a channel that does not exist." identifier))
                 (when (and residence
                            (eq (participant-kind participant) ':agent)
                            (channel-inbox-p (gethash residence (world--channel-table world))))
                   (fail "An agent cannot reside in the person's inbox." identifier)))
               (let ((parent (participant-parent participant)))
                 (when (and parent (not (gethash parent (world--participant-table world))))
                   (fail "A participant's parent does not exist." identifier)))
               (dolist (subscription (participant-subscriptions participant))
                 (unless (gethash subscription (world--channel-table world))
                   (fail "A participant subscribes to a channel that does not exist."
                         identifier))))
             (world--participant-table world))
    (let ((inboxes (loop for channel being the hash-values of (world--channel-table world)
                         when (channel-inbox-p channel)
                           collect (channel-identifier channel))))
      (when (rest inboxes)
        (fail "The world has more than one inbox." (second inboxes)))
      (when inboxes
        (maphash (lambda (identifier participant)
                   (when (and (eq (participant-kind participant) ':human)
                              (not (equal (participant-residence participant) (first inboxes))))
                     (fail "The person resides in the inbox; viewing another channel moves nobody."
                           identifier)))
                 (world--participant-table world))))
    (maphash (lambda (identifier channel)
               (unless (gethash (channel-owner channel) (world--participant-table world))
                 (fail "A channel's owner does not exist." identifier))
               (let ((parent (channel-parent channel)))
                 (when (and parent (not (gethash parent (world--channel-table world))))
                   (fail "A channel's parent does not exist." identifier))))
             (world--channel-table world)))
  world)


;;;; -- Store --

(-> world--store (configuration) sexp-store:log-store)
(defun world--store (configuration)
  "Describe the world schema, reducer, and product-owned paths."
  (let ((pathname (configuration-world-path configuration)))
    (ensure-directories-exist pathname)
    (make-instance
     'sexp-store:log-store
     :pathname      pathname
     :lock-pathname (readable-state-lock-pathname pathname "world.lock")
     :header        (list :world :version *world-format-version*)
     :header-validator
     (lambda (form)
       (sexp-store:record-check form :tag ':world :versions (list *world-format-version*)))
     :validator     (lambda (form) (keywordp (first form)))
     :initial-state (lambda () (make-instance 'world))
     :reducer       (lambda (world record)
                      (world--reduce-record pathname world record))
     :compactor     #'world--records)))

(-> world--records (world) list)
(defun world--records (world)
  "Return the fewest records that fold to WORLD: each channel, then each participant."
  (append (loop for channel being the hash-values of (world--channel-table world)
                collect (world--channel-record channel))
          (loop for participant being the hash-values of (world--participant-table world)
                collect (world--participant-record participant))))

(defparameter *world-compaction-ratio* 2
  "How many log records per live channel or participant make the world log worth compacting.")

(-> world-compact-when-grown (configuration) boolean)
(defun world-compact-when-grown (configuration)
  "Compact CONFIGURATION's world log once it holds *WORLD-COMPACTION-RATIO* times the records it needs.

A log only grows: every change appends, and nothing old is dropped. Return
whether it was compacted."
  (let ((pathname (configuration-world-path configuration)))
    (with-recursive-lock-held (*world-lock*)
      (handler-case
          (let* ((world   (world-load configuration))
                 (live    (+ (hash-table-count (world--participant-table world))
                             (hash-table-count (world--channel-table world))))
                 (records (nth-value 2 (sexp-store:log-map (constantly nil) pathname))))
            (when (> records (1+ (* *world-compaction-ratio* (max 1 live))))
              (sexp-store:store-compact (world--store configuration))
              t))
        (sexp-store:store-error (cause)
          (world--store-error cause))))))

(-> world--store-error (sexp-store:store-error) nil)
(defun world--store-error (cause)
  "Translate storage CAUSE into a WORLD-ERROR."
  (error 'world-error
         :message   (sexp-store:store-error-message cause)
         :pathname  (sexp-store:store-error-pathname cause)
         :operation (sexp-store:store-error-operation cause)))

(-> world--follower (configuration) sexp-store:log-follower)
(defun world--follower (configuration)
  "Return this process's follower of CONFIGURATION's world log, made on first use.

The caller holds *WORLD-LOCK*."
  (let ((key (namestring (configuration-world-path configuration))))
    (or (gethash key *world-followers*)
        (setf (gethash key *world-followers*)
              (sexp-store:log-follower-create (world--store configuration)
                                              :copier #'world--copy)))))

(-> world-load (configuration) world)
(defun world-load (configuration)
  "Return CONFIGURATION's complete world, checked for dangling references.

The world returned is never changed afterwards; a later change comes back as
another world."
  (with-recursive-lock-held (*world-lock*)
    (handler-case
        (let* ((pathname (configuration-world-path configuration))
               (key      (namestring pathname))
               (world    (sexp-store:log-follower-read (world--follower configuration))))
          (unless (eq world (gethash key *world-checked*))
            (world--check-integrity pathname world)
            (setf (gethash key *world-checked*) world))
          world)
      (sexp-store:store-error (cause)
        (world--store-error cause)))))

(-> world--transact (configuration function) t)
(defun world--transact (configuration update)
  "Apply UPDATE to fresh world state and append the records it returns.

UPDATE receives the world and returns the records to append and a result. The
records are folded into a private copy and checked before publication, so a
batch that would leave a dangling reference is refused whole."
  (let ((pathname (configuration-world-path configuration)))
    (with-recursive-lock-held (*world-lock*)
      (handler-case
          (sexp-store:log-follower-transact
           (world--follower configuration)
           (lambda (world)
             (multiple-value-bind (records result) (funcall update world)
               (when records
                 (let ((preview (world--copy world)))
                   (dolist (record records)
                     (world--reduce-record pathname preview record))
                   (world--check-integrity pathname preview :operation ':write)))
               (values records result (not (null records))))))
        (sexp-store:store-error (cause)
          (world--store-error cause))))))


;;;; -- Reading --

(-> world-participant-find (world string) (option participant))
(defun world-participant-find (world identifier)
  "Return WORLD's participant IDENTIFIER, or NIL."
  (values (gethash identifier (world--participant-table world))))

(-> world-channel-find (world string) (option channel))
(defun world-channel-find (world identifier)
  "Return WORLD's channel IDENTIFIER, or NIL."
  (values (gethash identifier (world--channel-table world))))

(-> world--channel-label (channel) string)
(defun world--channel-label (channel)
  "Return CHANNEL's display identifier followed by its title, when it has one."
  (format nil "~A~@[  ~A~]"
          (conversation-identifier-display (channel-identifier channel))
          (channel-title channel)))

(-> world-channel-resolve (world string) channel)
(defun world-channel-resolve (world fragment)
  "Return the one channel of WORLD that FRAGMENT names in full or in part.

An exact identifier in canonical or display form wins; otherwise the one
channel whose identifier contains FRAGMENT, compared without regard to case.
Several matches are reported together, newest change first, and select
nothing; so does no match."
  (let ((matches (identifier-match fragment (world-channels world) :key #'channel-identifier)))
    (cond
      ((null matches)
       (error 'world-error
              :message    (format nil "No channel matches ~A."
                                  fragment)
              :pathname   nil
              :operation  ':resolve
              :identifier fragment))
      ((rest matches)
       (error 'world-error
              :message    (format nil "Channel ID ~S is ambiguous. Matching channels:~%~{  ~A~^~%~}"
                                  fragment
                                  (mapcar #'world--channel-label matches))
              :pathname   nil
              :operation  ':resolve
              :identifier fragment))
      (t
       (first matches)))))

(-> world-participant-named-p (world string &key (:except (option string))) boolean)
(defun world-participant-named-p (world name &key except)
  "Return true when a participant of WORLD other than EXCEPT is named NAME, ignoring case."
  (loop for participant being the hash-values of (world--participant-table world)
        thereis (and (not (equal (participant-identifier participant) except))
                     (string-equal (participant-name participant) name))))

(-> world-handle-suffixed (string (integer 1)) string)
(defun world-handle-suffixed (base index)
  "Return BASE for INDEX 1, else BASE numbered with INDEX, cut so the whole stays a handle.

A base already at the handle length limit loses its tail to make room for
the suffix, so a numbered handle is valid and the search for a free one
ends."
  (if (= index 1)
      base
      (let* ((suffix (format nil "-~D" index))
             (room   (max 1 (- *world-handle-limit* (length suffix)))))
        (concatenate 'string (subseq base 0 (min (length base) room)) suffix))))

(-> world-unique-handle (world string) string)
(defun world-unique-handle (world base)
  "Return BASE, or BASE with the lowest numbered suffix, that names no participant of WORLD and is not reserved.

The names taken are gathered once, since a common base such as agent can
need hundreds of suffixes."
  (let ((taken (make-hash-table :test #'equalp)))
    (loop for participant being the hash-values of (world--participant-table world)
          do (setf (gethash (participant-name participant) taken) t))
    (loop for index from 1
          for candidate = (world-handle-suffixed base index)
          unless (or (gethash candidate taken)
                     (world-handle-reserved-p candidate))
            return candidate)))

(-> world-handle-reserved-p (string) boolean)
(defun world-handle-reserved-p (name)
  "Return true when NAME is a mention with a meaning of its own, which no participant may take."
  (string-equal name *world-parent-handle*))

(-> world-participant-designated (world string) (option participant))
(defun world-participant-designated (world designator)
  "Return the participant DESIGNATOR names by identifier, display identifier, unique name or unique fragment, or NIL."
  (or (let ((canonical (handler-case (identifier-normalize designator)
                         (identifier-error ()
                           nil))))
        (or (world-participant-find world designator)
            (and canonical (world-participant-find world canonical))))
      (world-participant-named world designator)
      (let ((matches (identifier-match designator
                                       (loop for participant being the hash-values
                                               of (world--participant-table world)
                                             collect participant)
                                       :key #'participant-identifier)))
        (and (= (length matches) 1) (first matches)))))

(-> world-participant-named (world string) (option participant))
(defun world-participant-named (world name)
  "Return the one participant of WORLD named NAME, ignoring case, or NIL when none or several are."
  (let ((named nil))
    (loop for participant being the hash-values of (world--participant-table world)
          when (string-equal (participant-name participant) name)
            do (push participant named))
    (and (= (length named) 1) (first named))))

(-> world-channel-designated (world configuration string) channel)
(defun world-channel-designated (world configuration designator)
  "Return the channel DESIGNATOR names in WORLD by identifier, legacy alias, agent or fragment.

The word inbox names the person's inbox, and @NAME the channel where the
participant NAME resides. A full identifier, in either form or as a retained
legacy alias of CONFIGURATION, names its channel directly; anything else is
resolved as a fragment through WORLD-CHANNEL-RESOLVE."
  (or (and (string-equal designator *world-inbox-designator*)
           (world-inbox-channel world))
      (and (uiop:string-prefix-p "@" designator)
           (world--residence-channel world (subseq designator 1)))
      (let ((resolved (handler-case
                          (conversation-identifier-migration-resolve configuration designator)
                        (conversation-error ()
                          nil))))
        (and resolved (world-channel-find world resolved)))
      (world-channel-resolve world designator)))

(-> world-channel-groups (list (option string)) list)
(defun world-channel-groups (channels current)
  "Return (WORKSPACE . CHANNELS) groups of CHANNELS, the group of CURRENT first.

CHANNELS arrive newest first, which orders the groups and their members."
  (let ((groups nil))
    (dolist (channel channels)
      (let ((group (assoc (channel-workspace channel) groups :test #'string=)))
        (if group
            (setf (rest group) (append (rest group) (list channel)))
            (setf groups (append groups (list (list (channel-workspace channel) channel)))))))
    (let ((home (find-if (lambda (group)
                           (find current (rest group) :key #'channel-identifier :test #'equal))
                         groups)))
      (if home
          (cons home (remove home groups))
          groups))))

(-> world-channel-forest (list) list)
(defun world-channel-forest (members)
  "Return MEMBERS as (CHANNEL . CHILDREN) trees, each channel under its parent.

A channel whose parent is not among MEMBERS is a root."
  (labels ((children (parent)
             "Return the trees of PARENT's members, or of the roots without a PARENT."
             (loop for channel in members
                   when (if parent
                            (equal (channel-parent channel) (channel-identifier parent))
                            (or (null (channel-parent channel))
                                (not (find (channel-parent channel) members
                                           :key #'channel-identifier :test #'equal))))
                     collect (cons channel (children channel)))))
    (children nil)))

(-> world--residence-channel (world string) channel)
(defun world--residence-channel (world name)
  "Return the channel where the participant NAME, an identifier or a name, resides."
  (let ((participant (world-message-participant world name)))
    (cond
      ((null participant)
       (error 'world-error
              :message    (format nil "No participant is named ~A." name)
              :pathname   nil
              :operation  ':resolve
              :identifier name))
      ((null (participant-residence participant))
       (error 'world-error
              :message    (format nil "@~A is parked in no channel; mention it in a channel's message instead."
                                  (participant-name participant))
              :pathname   nil
              :operation  ':resolve
              :identifier name))
      (t
       (world--require-channel world (participant-residence participant) ':resolve)))))

(-> world--newest-first (list function function) list)
(defun world--newest-first (items updated-at identifier)
  "Return ITEMS ordered by descending UPDATED-AT, then by IDENTIFIER."
  (sort (copy-list items)
        (lambda (left right)
          (or (> (funcall updated-at left) (funcall updated-at right))
              (and (= (funcall updated-at left) (funcall updated-at right))
                   (string< (funcall identifier left) (funcall identifier right)))))))

(-> world--participants-where (world function) list)
(defun world--participants-where (world predicate)
  "Return WORLD's participants satisfying PREDICATE, most recently changed first.

Only the matches get sorted, not every participant."
  (world--newest-first
   (loop for participant being the hash-values of (world--participant-table world)
         when (funcall predicate participant)
           collect participant)
   #'participant-updated-at #'participant-identifier))

(-> world--channels-where (world function) list)
(defun world--channels-where (world predicate)
  "Return WORLD's channels satisfying PREDICATE, most recently changed first.

Only the matches get sorted, not every channel."
  (world--newest-first
   (loop for channel being the hash-values of (world--channel-table world)
         when (funcall predicate channel)
           collect channel)
   #'channel-updated-at #'channel-identifier))

(-> world-participants (world) list)
(defun world-participants (world)
  "Return WORLD's participants, most recently changed first."
  (world--participants-where world (constantly t)))

(-> world-channels (world) list)
(defun world-channels (world)
  "Return WORLD's channels, most recently changed first."
  (world--channels-where world (constantly t)))

(-> world-channel-residents (world string) list)
(defun world-channel-residents (world channel-identifier)
  "Return the participants residing in CHANNEL-IDENTIFIER, most recently changed first."
  (world--participants-where world
                             (lambda (participant)
                               (equal (participant-residence participant) channel-identifier))))

(-> world-channel-subscribers (world string) list)
(defun world-channel-subscribers (world channel-identifier)
  "Return the participants subscribed to CHANNEL-IDENTIFIER, most recently changed first."
  (world--participants-where world
                             (lambda (participant)
                               (not (null (member channel-identifier
                                                  (participant-subscriptions participant)
                                                  :test #'string=))))))

(-> world-channel-children (world string) list)
(defun world-channel-children (world channel-identifier)
  "Return the channels nested directly under CHANNEL-IDENTIFIER."
  (world--channels-where world
                         (lambda (channel)
                           (equal (channel-parent channel) channel-identifier))))

(-> world-participant-children (world string) list)
(defun world-participant-children (world participant-identifier)
  "Return the participants whose parent is PARTICIPANT-IDENTIFIER."
  (world--participants-where world
                             (lambda (participant)
                               (equal (participant-parent participant) participant-identifier))))

(-> world-participant-descendants (world string) list)
(defun world-participant-descendants (world participant-identifier)
  "Return every participant below PARTICIPANT-IDENTIFIER, children first, wherever each resides."
  (loop for child in (world-participant-children world participant-identifier)
        collect child
        append (world-participant-descendants world (participant-identifier child))))

(-> world-inbox-channel (world) (option channel))
(defun world-inbox-channel (world)
  "Return WORLD's inbox, the channel where the person resides, or NIL before it exists."
  (loop for channel being the hash-values of (world--channel-table world)
        when (channel-inbox-p channel)
          return channel))

(-> world-inbox-refuse-agent (world string keyword) null)
(defun world-inbox-refuse-agent (world channel-identifier operation)
  "Signal WORLD-ERROR for OPERATION when CHANNEL-IDENTIFIER is the person's inbox, where no agent resides."
  (let ((channel (world-channel-find world channel-identifier)))
    (when (and channel (channel-inbox-p channel))
      (error 'world-error
             :message    "The inbox is where the person resides; no agent lives there. Pick another channel."
             :pathname   nil
             :operation  operation
             :identifier channel-identifier)))
  nil)

(-> world-human-participant (world) (option participant))
(defun world-human-participant (world)
  "Return WORLD's oldest human participant, or NIL."
  (first (sort (world--participants-where world
                                         (lambda (participant)
                                           (eq (participant-kind participant) ':human)))
               #'< :key #'participant-created-at)))


;;;; -- Mutation --

(-> world--require-residence (participant (option string) keyword) null)
(defun world--require-residence (participant expected operation)
  "Refuse OPERATION unless PARTICIPANT still resides in channel EXPECTED, NIL for parked."
  (unless (equal (participant-residence participant) expected)
    (error 'world-error
           :message    (format nil "~A moved while this change was prepared; nothing changed. Try again."
                               (participant-name participant))
           :pathname   nil
           :operation  operation
           :identifier (participant-identifier participant)))
  nil)

(-> world--require-participant (world string keyword) participant)
(defun world--require-participant (world identifier operation)
  "Return participant IDENTIFIER, or signal WORLD-ERROR for OPERATION."
  (or (world-participant-find world identifier)
      (error 'world-error
             :message    (format nil "Participant ~A does not exist." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier)))

(-> world--require-channel (world string keyword) channel)
(defun world--require-channel (world identifier operation)
  "Return channel IDENTIFIER, or signal WORLD-ERROR for OPERATION."
  (or (world-channel-find world identifier)
      (error 'world-error
             :message    (format nil "Channel ~A does not exist." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier)))

(-> world-workspace-name ((or pathname string)) string)
(defun world-workspace-name (location)
  "Return LOCATION as the canonical absolute directory name a channel stores.

An existing directory is canonicalized through its truename. A directory that
no longer exists keeps its absolute name, so a channel outlives its workspace."
  (if (uiop:directory-exists-p location)
      (workspace-directory-name location)
      (namestring (uiop:ensure-pathname (platform-pathname location)
                                        :ensure-absolute  t
                                        :ensure-directory t
                                        :want-non-wild    t))))

(-> world--validate-name (t keyword) string)
(defun world--validate-name (value operation)
  "Return VALUE when it is a bounded single-line name."
  (unless (world--name-p value)
    (error 'world-error
           :message   (format nil "A name must be one line of at most ~D characters."
                              *world-name-limit*)
           :pathname  nil
           :operation operation))
  value)

(-> world--generate-identifier (world timestamp) string)
(defun world--generate-identifier (world timestamp)
  "Allocate a participant identifier unused in WORLD for TIMESTAMP."
  (handler-case
      (identifier-generate
       :timestamp  timestamp
       :namespace  "world-participant"
       :occupied-p (lambda (candidate)
                     (not (null (world-participant-find world candidate)))))
    (identifier-error (condition)
      (error 'world-error
             :message   (identifier-error-message condition)
             :pathname  nil
             :operation ':create-participant))))

(-> world-channel-create
    (configuration &key (:id (option string)) (:owner string) (:workspace string)
                        (:title (option string)) (:parent (option string))
                        (:now timestamp))
    channel)
(defun world-channel-create
    (configuration &key id owner workspace title parent (now (get-universal-time)))
  "Create a channel owned by existing participant OWNER with canonical WORKSPACE.

ID names the channel and its history conversation; when omitted, a conversation
identifier is allocated for it."
  (world--transact
   configuration
   (lambda (world)
     (world--require-participant world owner ':create-channel)
     (let ((channel (world--new-channel configuration world
                                       :home      (list :id        id
                                                        :workspace workspace
                                                        :title     title
                                                        :parent    parent)
                                       :owner     owner
                                       :now       now
                                       :operation ':create-channel)))
       (values (list (world--channel-record channel)) channel)))))

(-> world--new-channel
    (configuration world &key (:home list) (:owner string) (:now timestamp)
                              (:operation keyword))
    channel)
(defun world--new-channel (configuration world &key home owner now operation)
  "Return the channel HOME describes, owned by OWNER, after checking it against WORLD.

HOME is a plist of :id, :workspace, :title and :parent. A missing :id
allocates a conversation identifier for the channel's history. OPERATION
names the caller in refusals."
  (let ((identifier (or (getf home :id)
                        (conversation-identifier-generate
                         (configuration-conversation-root configuration)
                         :timestamp now)))
        (parent     (getf home :parent))
        (title      (getf home :title)))
    (when (world-channel-find world identifier)
      (error 'world-error
             :message    (format nil "Channel ~A already exists." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier))
    (when parent (world--require-channel world parent operation))
    (make-instance 'channel
                   :identifier identifier
                   :parent     parent
                   :owner      owner
                   :workspace  (world-workspace-name (getf home :workspace))
                   :title      (and title (world--validate-name title operation))
                   :created-at now
                   :updated-at now)))

(-> world-participant-create
    (configuration &key (:kind participant-kind) (:name string) (:residence (option string))
                        (:home list) (:parent (option string)) (:settings list)
                        (:id (option string)) (:now timestamp))
    (values participant channel))
(defun world-participant-create
    (configuration &key kind name residence home parent (settings nil settings-p)
                        id (now (get-universal-time)))
  "Create a participant of KIND named NAME and return it with its residence channel.

RESIDENCE names an existing channel. HOME instead describes a new channel the
participant owns and resides in, as a plist of :id, :workspace, :title and
:parent, created in the same transaction. Exactly one of the two is given.
A generated identifier is reserved only until the participant's record is
written, so reservations never pile up in a long-running process."
  (let ((generated nil))
    (unwind-protect
         (values-list
          (world--transact
           configuration
           (lambda (world)
            (let ((identifier (or id (setf generated (world--generate-identifier world now)))))
             (when (world-participant-find world identifier)
               (error 'world-error
                      :message    (format nil "Participant ~A already exists." identifier)
                      :pathname   nil
                      :operation  ':create-participant
                      :identifier identifier))
             (unless (if residence (null home) home)
               (error 'world-error
                      :message    "A participant is created with either an existing residence or a new home channel."
                      :pathname   nil
                      :operation  ':create-participant
                      :identifier identifier))
             (when parent (world--require-participant world parent ':create-participant))
             (unless (world-settings-p (and settings-p settings))
               (error 'world-error
                      :message    "Participant settings must be a bounded plist of portable values."
                      :pathname   nil
                      :operation  ':create-participant
                      :identifier identifier))
             (let* ((channel     (if home
                                     (world--new-channel configuration world
                                                         :home      home
                                                         :owner     identifier
                                                         :now       now
                                                         :operation ':create-participant)
                                     (world--require-channel world residence ':create-participant)))
                    (participant (make-instance 'participant
                                                :identifier identifier
                                                :kind       kind
                                                :name       (world--validate-name
                                                             name ':create-participant)
                                                :parent     parent
                                                :residence  (channel-identifier channel)
                                                :settings   (copy-list settings)
                                                :created-at now
                                                :updated-at now)))
               (values (append (when home (list (world--channel-record channel)))
                               (list (world--participant-record participant)))
                       (list participant channel)))))))
      (when generated
        (identifier-release generated :namespace "world-participant")))))

(-> world--update-participant
    (configuration string &key (:operation keyword) (:now timestamp) (:change function))
    participant)
(defun world--update-participant (configuration identifier &key operation now change)
  "Apply CHANGE to participant IDENTIFIER in a fresh world transaction.

CHANGE receives the world and participant and returns changed initargs, or
NIL for no change. Persist one replacement with a monotonic timestamp and
return it; a no-op returns the existing participant without appending."
  (world--transact
   configuration
   (lambda (world)
     (let* ((participant (world--require-participant world identifier operation))
            (changes     (funcall change world participant)))
       (if changes
           (let ((updated (apply #'world--copy-participant participant
                                 :updated-at (max now (participant-updated-at participant))
                                 changes)))
             (values (list (world--participant-record updated)) updated))
           (values nil participant))))))

(-> world-participant-update
    (configuration string &key (:settings list) (:now timestamp))
    participant)
(defun world-participant-update
    (configuration identifier &key (settings nil settings-p) (now (get-universal-time)))
  "Replace IDENTIFIER's settings and return the participant."
  (world--update-participant
   configuration identifier :operation ':update-participant :now now
   :change (lambda (world participant)
             (declare (ignore world))
             (when (and settings-p (not (world-settings-p settings)))
               (error 'world-error
                      :message    "Participant settings must be a bounded plist of portable values."
                      :pathname   nil
                      :operation  ':update-participant
                      :identifier identifier))
             (list :settings (if settings-p
                                 (copy-list settings)
                                 (participant-settings participant))))))

(-> world-participant-rename (configuration string string &key (:now timestamp)) participant)
(defun world-participant-rename (configuration identifier name &key (now (get-universal-time)))
  "Name participant IDENTIFIER NAME, a handle no other participant has, and return it."
  (world--update-participant
   configuration identifier :operation ':rename-participant :now now
   :change (lambda (world participant)
             (declare (ignore participant))
             (unless (world-handle-p name)
               (error 'world-error
                      :message    (format nil "A name is up to ~D letters, digits, hyphens and underscores, so @mentions can use it."
                                          *world-handle-limit*)
                      :pathname   nil
                      :operation  ':rename-participant
                      :identifier identifier))
             (when (world-handle-reserved-p name)
               (error 'world-error
                      :message    (format nil "@~A always names the sender's parent; choose another name." name)
                      :pathname   nil
                      :operation  ':rename-participant
                      :identifier identifier))
             (when (world-participant-named-p world name :except identifier)
               (error 'world-error
                      :message    (format nil "Another participant is already named ~A." name)
                      :pathname   nil
                      :operation  ':rename-participant
                      :identifier identifier))
             (list :name name))))

(-> world-channel-update
    (configuration string &key (:title (option string)) (:workspace (option string))
                          (:now timestamp))
    channel)
(defun world-channel-update
    (configuration identifier &key title workspace (now (get-universal-time)))
  "Replace the given fields of channel IDENTIFIER and return it."
  (world--transact
   configuration
   (lambda (world)
     (let* ((channel (world--require-channel world identifier ':update-channel))
            (updated (world--copy-channel
                      channel
                      :title      (if title
                                      (world--validate-name title ':update-channel)
                                      (channel-title channel))
                      :workspace  (if workspace
                                      (world-workspace-name workspace)
                                      (channel-workspace channel))
                      :updated-at (max now (channel-updated-at channel)))))
       (values (list (world--channel-record updated)) updated)))))

(-> world-participant-move
    (configuration string string &key (:now timestamp) (:expected-residence (option string)))
    participant)
(defun world-participant-move (configuration identifier channel-identifier
                               &key (now (get-universal-time))
                                    (expected-residence nil expected-residence-p))
  "Move participant IDENTIFIER into channel CHANNEL-IDENTIFIER and return it.

Other residents of either channel stay where they are. A channel has at most
one resident agent, so an agent is refused a channel where another already
resides. Once the inbox exists the person resides there and is not moved.
With EXPECTED-RESIDENCE the move happens only while the participant still
resides there, NIL for parked."
  (world--update-participant
   configuration identifier :operation ':move-participant :now now
   :change (lambda (world participant)
             (when expected-residence-p
               (world--require-residence participant expected-residence ':move-participant))
             (world--require-channel world channel-identifier ':move-participant)
             (when (eq (participant-kind participant) ':agent)
               (let ((resident (find-if (lambda (candidate)
                                          (and (eq (participant-kind candidate) ':agent)
                                               (not (string= (participant-identifier candidate)
                                                             identifier))))
                                        (world-channel-residents world channel-identifier))))
                 (when resident
                   (error 'world-error
                          :message    (format nil "Channel ~A already has resident agent ~A; a channel has one agent. Move that one away first."
                                              (conversation-identifier-display channel-identifier)
                                              (participant-name resident))
                          :pathname   nil
                          :operation  ':move-participant
                          :identifier identifier))))
             (unless (equal (participant-residence participant) channel-identifier)
               (list :residence channel-identifier)))))

(-> world-participant-park
    (configuration string &key (:now timestamp) (:expected-residence (option string)))
    participant)
(defun world-participant-park (configuration identifier
                               &key (now (get-universal-time))
                                    (expected-residence nil expected-residence-p))
  "Park agent IDENTIFIER, taking it out of every channel, and return it.

A parked agent keeps its identity, its children and its subscriptions, and a
move brings it into a channel again. Only agents park; the person always
resides somewhere. With EXPECTED-RESIDENCE the park happens only while the
agent still resides there."
  (world--update-participant
   configuration identifier :operation ':park :now now
   :change (lambda (world participant)
             (declare (ignore world))
             (when expected-residence-p
               (world--require-residence participant expected-residence ':park))
             (unless (eq (participant-kind participant) ':agent)
               (error 'world-error
                      :message    (format nil "~A is not an agent; only agents are parked."
                                          (participant-name participant))
                      :pathname   nil
                      :operation  ':park
                      :identifier identifier))
             (when (participant-residence participant)
               (list :residence nil)))))

(-> world-parked-agents (world) list)
(defun world-parked-agents (world)
  "Return WORLD's parked agents, oldest first."
  (sort (world--participants-where world
                                   (lambda (participant)
                                     (and (eq (participant-kind participant) ':agent)
                                          (null (participant-residence participant)))))
        #'< :key #'participant-created-at))

(-> world-participant-subscribe (configuration string string &key (:now timestamp)) participant)
(defun world-participant-subscribe (configuration identifier channel-identifier &key (now (get-universal-time)))
  "Subscribe participant IDENTIFIER to channel CHANNEL-IDENTIFIER and return it."
  (world--update-participant
   configuration identifier :operation ':subscribe :now now
   :change (lambda (world participant)
             (world--require-channel world channel-identifier ':subscribe)
             (unless (member channel-identifier (participant-subscriptions participant) :test #'string=)
               (list :subscriptions (append (participant-subscriptions participant)
                                            (list channel-identifier)))))))

(-> world-participant-unsubscribe (configuration string string &key (:now timestamp)) participant)
(defun world-participant-unsubscribe (configuration identifier channel-identifier &key (now (get-universal-time)))
  "Remove channel CHANNEL-IDENTIFIER from participant IDENTIFIER's subscriptions."
  (world--update-participant
   configuration identifier :operation ':unsubscribe :now now
   :change (lambda (world participant)
             (declare (ignore world))
             (when (member channel-identifier (participant-subscriptions participant) :test #'string=)
               (list :subscriptions (remove channel-identifier
                                            (participant-subscriptions participant)
                                            :test #'string=))))))

(-> world--participant-deletion-records
    (world participant timestamp &key (:except (option string)) (:deleted list)
                                      (:pending list))
    list)
(defun world--participant-deletion-records (world participant now &key except deleted pending)
  "Return the records deleting PARTICIPANT and its descendants.

Channels owned by the deleted participants pass to PARTICIPANT's surviving
parent, or otherwise to the oldest surviving human participant. Refuse the
deletion if owned channels would have no successor.
Channel EXCEPT is being deleted itself, participants in DELETED already have
their records, and those in PENDING get theirs later in the same batch, so
none of them is reassigned or chosen as the heir."
  (let* ((doomed     (labels ((collect (participant)
                                (if (member (participant-identifier participant) deleted
                                            :test #'string=)
                                    nil
                                    (cons participant
                                          (loop for child in (world-participant-children
                                                              world
                                                              (participant-identifier participant))
                                                append (collect child))))))
                       (collect participant)))
         (doomed-ids (mapcar #'participant-identifier doomed))
         (lost-ids   (append deleted pending doomed-ids))
         (surviving-parent
           (let ((parent (participant-parent participant)))
             (and parent
                  (not (member parent lost-ids :test #'string=))
                  (world-participant-find world parent))))
         (heir       (or surviving-parent
                         (find-if (lambda (candidate)
                                    (and (eq (participant-kind candidate) ':human)
                                         (not (member (participant-identifier candidate)
                                                      lost-ids :test #'string=))))
                                  (sort (world-participants world) #'<
                                        :key #'participant-created-at))))
         (owned      (remove-if-not (lambda (channel)
                                      (and (not (equal (channel-identifier channel) except))
                                           (member (channel-owner channel) doomed-ids
                                                   :test #'string=)))
                                    (world-channels world))))
    (when (and owned (null heir))
      (error 'world-error
             :message    "The channels this participant owns would be left without an owner."
             :pathname   nil
             :operation  ':delete-participant
             :identifier (participant-identifier participant)))
    (append (mapcar (lambda (channel)
                      (world--channel-record
                       (world--copy-channel channel
                                            :owner      (participant-identifier heir)
                                            :updated-at (max now (channel-updated-at channel)))))
                    owned)
            (mapcar (lambda (participant)
                      (list :participant-deleted
                            :version *world-format-version*
                            :id   (participant-identifier participant)
                            :time now))
                    doomed))))

(-> world-participant-delete
    (configuration string &key (:recursive-p boolean) (:now timestamp))
    list)
(defun world-participant-delete (configuration identifier &key recursive-p (now (get-universal-time)))
  "Delete participant IDENTIFIER and return the deleted identifiers.

A participant with children is deleted only with RECURSIVE-P, together with
every descendant. Private state the participant owns is the caller's to
remove; this store holds only identities and relationships."
  (world--transact
   configuration
   (lambda (world)
     (let ((participant (world--require-participant world identifier ':delete-participant)))
       (when (and (not recursive-p) (world-participant-children world identifier))
         (error 'world-error
                :message    (format nil "Participant ~A has children; delete them first or recursively."
                                    identifier)
                :pathname   nil
                :operation  ':delete-participant
                :identifier identifier))
       (let ((records (world--participant-deletion-records world participant now)))
         (values records
                 (loop for record in records
                       when (eq (first record) ':participant-deleted)
                         collect (getf (rest record) :id))))))))

(-> world-channel-delete
    (configuration string &key (:participants list) (:now timestamp))
    list)
(defun world-channel-delete (configuration identifier &key participants (now (get-universal-time)))
  "Delete channel IDENTIFIER and return the deleted participant identifiers.

Move each resident away first or list it in PARTICIPANTS for deletion with
the channel. Also list every descendant of a resident being deleted,
wherever it lives: participant deletion includes all descendants. Delete
nested channels first. The caller must check for jobs bound to the channel."
  (world--transact
   configuration
   (lambda (world)
     (world--require-channel world identifier ':delete-channel)
     (when (world-channel-children world identifier)
       (error 'world-error
              :message    (format nil "Channel ~A has nested channels; delete them first."
                                  identifier)
              :pathname   nil
              :operation  ':delete-channel
              :identifier identifier))
     (let ((residents (sort (world-channel-residents world identifier) #'<
                            :key #'participant-created-at)))
       (dolist (resident residents)
         (unless (member (participant-identifier resident) participants :test #'string=)
           (error 'world-error
                  :message    (format nil "Channel ~A still has resident ~A; move it or include it in the deletion."
                                      identifier (participant-identifier resident))
                  :pathname   nil
                  :operation  ':delete-channel
                  :identifier identifier))
         (dolist (descendant (world-participant-descendants world (participant-identifier resident)))
           (unless (member (participant-identifier descendant) participants :test #'string=)
             (error 'world-error
                    :message    (format nil "Deleting ~A would delete its descendant ~A ~:[parked in no channel~;in channel ~:*~A~]; include it in the deletion."
                                        (participant-identifier resident)
                                        (participant-identifier descendant)
                                        (participant-residence descendant))
                    :pathname   nil
                    :operation  ':delete-channel
                    :identifier identifier))))
       (let ((records nil) (deleted nil))
         (dolist (resident residents)
           (unless (member (participant-identifier resident) deleted :test #'string=)
             (let ((deletion (world--participant-deletion-records
                              world resident now
                              :except  identifier
                              :deleted deleted
                              :pending (mapcar #'participant-identifier residents))))
               (setf records (append records deletion))
               (dolist (record deletion)
                 (when (eq (first record) ':participant-deleted)
                   (push (getf (rest record) :id) deleted))))))
         (values (append records
                         (list (list :channel-deleted
                                     :version *world-format-version*
                                     :id   identifier
                                     :time now)))
                 (nreverse deleted)))))))
