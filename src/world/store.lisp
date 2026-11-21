(in-package #:autolith)

;;;; -- World of Channels and Participants --

;;; The world stores human and agent participants and their channels. Each
;;; participant may reside in one channel and subscribe to or own others;
;;; archived legacy agents have no residence. A channel's history is the conversation
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
   (legacy-identifier
    :initarg       :legacy-identifier
    :initform      nil
    :reader        participant-legacy-identifier
    :type          (option non-empty-string)
    :documentation "A historical agent ID that may address this channel's agent in old records.")
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
   (name-source
    :initarg       :name-source
    :initform      ':explicit
    :reader        participant-name-source
    :type          (member :initial :generated :explicit)
    :documentation "Whether the handle is the one the agent started with, a name inferred from its work, or an explicitly assigned name.")
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
    :documentation "The channel this participant resides in, or NIL for an archived participant.")
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
   (repository-key
    :initarg       :repository-key
    :initform      nil
    :reader        channel-repository-key
    :type          (option non-empty-string)
    :documentation "The last positively resolved local Git group identity.")
   (repository-directory
    :initarg       :repository-directory
    :initform      nil
    :reader        channel-repository-directory
    :type          (option non-empty-string)
    :documentation "The originating checkout folder, retained after workspace removal.")
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
   (participant-aliases
    :initform      (make-hash-table :test #'equal)
    :reader        world--participant-aliases
    :type          hash-table
    :documentation "One historical identifier per converted agent, indexed to the channel ID.")
   (channels
    :initform      (make-hash-table :test #'equal)
    :reader        world--channel-table
    :type          hash-table
    :documentation "Channels by identifier.")
   (deleted-channels
    :initform      (make-hash-table :test #'equal)
    :reader        world--deleted-channel-table
    :type          hash-table
    :documentation "Storage-deleted channel identities and times, retained against stale discovery."))
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

(-> world-repository-identity-p (t t) boolean)
(defun world-repository-identity-p (key directory)
  "Validate an optional repository KEY and DIRECTORY without consulting the filesystem."
  (or (and (null key) (null directory))
      (and (non-empty-string-p key)
           (uiop:string-prefix-p "git:" key)
           (> (length key) 4)
           (non-empty-string-p directory)
           (handler-case
               (and (uiop:absolute-pathname-p (platform-pathname (subseq key 4)))
                    (uiop:absolute-pathname-p (platform-pathname directory))
                    (notany (lambda (path)
                              (or (wild-pathname-p path)
                                  (member :up (pathname-directory path))))
                            (list (platform-pathname (subseq key 4))
                                  (platform-pathname directory)))
                    t)
             (error () nil)))))

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
        :legacy-id     (world-portable-string (participant-legacy-identifier participant))
        :kind          (participant-kind participant)
        :name          (world-portable-string (participant-name participant))
        :name-source   (participant-name-source participant)
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
        :id                   (world-portable-string (channel-identifier channel))
        :parent               (world-portable-string (channel-parent channel))
        :owner                (world-portable-string (channel-owner channel))
        :workspace            (world-portable-string (channel-workspace channel))
        :repository-key       (world-portable-string (channel-repository-key channel))
        :repository-directory (world-portable-string (channel-repository-directory channel))
        :title                (world-portable-string (channel-title channel))
        :inbox-p              (channel-inbox-p channel)
        :created-at           (channel-created-at channel)
        :updated-at           (channel-updated-at channel)))

(-> world--record->participant (pathname list) participant)
(defun world--record->participant (pathname record)
  "Validate and convert one participant RECORD read from PATHNAME."
  (let ((properties (rest record)))
    (unless (and (eql (getf properties :version) *world-format-version*)
                 (non-empty-string-p (getf properties :id))
                 (or (null (getf properties :legacy-id))
                     (and (eq (getf properties :kind) ':agent)
                          (equal (getf properties :id) (getf properties :residence))
                          (non-empty-string-p (getf properties :legacy-id))
                          (not (equal (getf properties :id) (getf properties :legacy-id)))))
                 (typep (getf properties :kind) 'participant-kind)
                 (world--name-p (getf properties :name))
                 (member (getf properties :name-source ':explicit) '(:initial :generated :explicit))
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
                   :legacy-identifier (getf properties :legacy-id)
                   :kind          (getf properties :kind)
                   :name          (getf properties :name)
                   :name-source   (getf properties :name-source ':explicit)
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
                 (world-repository-identity-p (getf properties :repository-key)
                                              (getf properties :repository-directory))
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
                   :identifier           (getf properties :id)
                   :parent               (getf properties :parent)
                   :owner                (getf properties :owner)
                   :workspace            (getf properties :workspace)
                   :repository-key       (getf properties :repository-key)
                   :repository-directory (getf properties :repository-directory)
                   :title                (getf properties :title)
                   :inbox-p              (getf properties :inbox-p)
                   :created-at           (getf properties :created-at)
                   :updated-at           (getf properties :updated-at))))

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
                       :legacy-identifier (participant-legacy-identifier participant)
                       :kind          (participant-kind participant)
                       :name          (participant-name participant)
                       :name-source   (participant-name-source participant)
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
                 (list :identifier           (channel-identifier channel)
                       :parent               (channel-parent channel)
                       :owner                (channel-owner channel)
                       :workspace            (channel-workspace channel)
                       :repository-key       (channel-repository-key channel)
                       :repository-directory (channel-repository-directory channel)
                       :title                (channel-title channel)
                       :inbox-p              (channel-inbox-p channel)
                       :created-at           (channel-created-at channel)
                       :updated-at           (channel-updated-at channel)))))

(-> world--copy (world) world)
(defun world--copy (world)
  "Return a world holding the same participants and channels."
  (let ((copy (make-instance 'world)))
    (maphash (lambda (identifier participant)
               (setf (gethash identifier (world--participant-table copy)) participant))
             (world--participant-table world))
    (maphash (lambda (old identifier)
               (setf (gethash old (world--participant-aliases copy)) identifier))
             (world--participant-aliases world))
    (maphash (lambda (identifier channel)
               (setf (gethash identifier (world--channel-table copy)) channel))
             (world--channel-table world))
    (maphash (lambda (identifier deleted-p)
               (setf (gethash identifier (world--deleted-channel-table copy)) deleted-p))
             (world--deleted-channel-table world))
    copy))

(-> world--reduce-record (pathname world list) world)
(defun world--reduce-record (pathname world record)
  "Apply one RECORD to fresh WORLD state read from PATHNAME."
  (case (first record)
    (:participant
     (let* ((participant (world--record->participant pathname record))
            (identifier  (participant-identifier participant))
            (previous    (gethash identifier (world--participant-table world)))
            (alias       (participant-legacy-identifier participant)))
       (when (and previous (participant-legacy-identifier previous))
         (remhash (participant-legacy-identifier previous) (world--participant-aliases world)))
       (when alias
         (let ((mapped (gethash alias (world--participant-aliases world))))
           (when (or (gethash alias (world--participant-table world))
                     (and mapped (not (equal mapped identifier))))
             (error 'world-error :message "A historical agent identifier is not unique."
                    :pathname pathname :operation ':read :identifier alias)))
         (setf (gethash alias (world--participant-aliases world)) identifier))
       (setf (gethash identifier (world--participant-table world)) participant)))
    (:participant-deleted
     (let* ((identifier (world--tombstone-identifier pathname record))
            (previous   (gethash identifier (world--participant-table world))))
       (when (and previous (participant-legacy-identifier previous))
         (remhash (participant-legacy-identifier previous) (world--participant-aliases world)))
       (remhash identifier (world--participant-table world))))
    (:channel
     (let ((channel (world--record->channel pathname record)))
       (remhash (channel-identifier channel) (world--deleted-channel-table world))
       (setf (gethash (channel-identifier channel) (world--channel-table world))
             channel)))
    (:channel-deleted
     (let ((identifier (world--tombstone-identifier pathname record)))
       (when (getf (rest record) :storage-cleanup-p)
         (setf (gethash identifier (world--deleted-channel-table world)) (getf (rest record) :time)))
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
  "Return the records that fold to WORLD, including storage-deletion tombstones."
  (append (loop for identifier being the hash-keys of (world--deleted-channel-table world)
                  using (hash-value time)
                collect (list :channel-deleted :version *world-format-version*
                              :id identifier :time time :storage-cleanup-p t))
          (loop for channel being the hash-values of (world--channel-table world)
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
                             (hash-table-count (world--channel-table world))
                             (hash-table-count (world--deleted-channel-table world))))
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
  "Return the participant or resolve an old agent ID to its canonical channel."
  (values (gethash (or (gethash identifier (world--participant-aliases world)) identifier)
                   (world--participant-table world))))

(-> world-channel-find (world string) (option channel))
(defun world-channel-find (world identifier)
  "Return WORLD's channel IDENTIFIER, or NIL."
  (values (gethash identifier (world--channel-table world))))

(-> world-channel-paths (world) hash-table)
(defun world-channel-paths (world)
  "Return current absolute ancestry paths keyed by channel ID, without changing durable identity.
Each component is a sole resident agent's escaped name, or the displayed channel
ID when unnamed, shared or conflicting with a sibling name or fallback ID.
The inbox is the address root: its own path is /inbox; its children start at /."
  (let ((channels   (world-channels world))
        (residents  (make-hash-table :test #'equal))
        (siblings   (make-hash-table :test #'equal))
        (preferred  (make-hash-table :test #'equal))
        (components (make-hash-table :test #'equal))
        (paths      (make-hash-table :test #'equal))
        (visiting   (make-hash-table :test #'equal)))
    (dolist (participant (world-participants world))
      (when (and (eq (participant-kind participant) ':agent) (participant-residence participant))
        (push participant (gethash (participant-residence participant) residents))))
    (dolist (channel channels)
      (let* ((identifier (channel-identifier channel))
             (agents     (gethash identifier residents))
             (parent     (and (channel-parent channel)
                              (world-channel-find world (channel-parent channel)))))
        (push channel (gethash (and parent (not (channel-inbox-p parent))
                                   (channel-identifier parent)) siblings))
        (setf (gethash identifier preferred)
              (resource-item-encode-identifier
               (cond
                 ((channel-inbox-p channel)
                  *world-inbox-designator*)
                 ((and agents (null (rest agents)))
                  (participant-name (first agents)))
                 (t
                  (conversation-identifier-display identifier)))))))
    (maphash
     (lambda (parent members)
       (declare (ignore parent))
       (let ((names       (make-hash-table :test #'equalp))
             (identifiers (make-hash-table :test #'equalp)))
         (dolist (channel members)
           (let ((identifier (channel-identifier channel)))
             (incf (gethash (gethash identifier preferred) names 0))
             (setf (gethash (resource-item-encode-identifier
                             (conversation-identifier-display identifier)) identifiers) identifier)))
         (dolist (channel members)
           (let* ((identifier (channel-identifier channel))
                  (name       (gethash identifier preferred))
                  (reserved   (gethash name identifiers)))
             (setf (gethash identifier components)
                   (if (or (channel-inbox-p channel)
                           (and (= (gethash name names) 1)
                                (or (null reserved) (equal reserved identifier))))
                       name
                       (resource-item-encode-identifier (conversation-identifier-display identifier))))))))
     siblings)
    (labels ((path (channel)
               "Compute CHANNEL's path once, refusing cyclic ancestry."
               (let ((identifier (channel-identifier channel)))
                 (or (gethash identifier paths)
                     (progn
                       (when (gethash identifier visiting)
                         (error 'world-error :message "Channel ancestry is cyclic."
                                :pathname nil :operation ':resolve :identifier identifier))
                       (setf (gethash identifier visiting) t)
                       (let ((parent (and (channel-parent channel)
                                          (world-channel-find world (channel-parent channel)))))
                         (prog1
                             (setf (gethash identifier paths)
                                   (format nil "~A/~A" (if (and parent (not (channel-inbox-p parent)))
                                                           (path parent) "")
                                           (gethash identifier components)))
                           (remhash identifier visiting))))))))
      (dolist (channel channels) (path channel)))
    paths))

(-> world-channel-path (world channel &key (:paths hash-table)) string)
(defun world-channel-path (world channel &key (paths (world-channel-paths world)))
  "Return CHANNEL's exact current human address; PATHS may reuse a catalogue's computed paths."
  (gethash (channel-identifier channel) paths))

(-> world-channel-at-path (world string &key (:error-p boolean)) (option channel))
(defun world-channel-at-path (world path &key (error-p t))
  "Resolve an exact canonical PATH, never a prefix, suffix or normalized filesystem path."
  (let* ((paths   (world-channel-paths world))
         (matches (remove-if-not (lambda (channel)
                                   (string= path (world-channel-path world channel :paths paths)))
                                 (world-channels world))))
    (cond
      ((rest matches)
       (error 'world-channel-ambiguous :message (format nil "Channel path ~S is ambiguous." path)
              :channels matches :pathname nil :operation ':resolve :identifier path))
      (matches
       (first matches))
      (error-p
       (error 'world-error :message (format nil "No channel has the exact path ~S." path)
              :pathname nil :operation ':resolve :identifier path)))))

(-> world--participant-name-target (world string) (option participant))
(defun world--participant-name-target (world name)
  "Resolve an active participant NAME exactly, reporting ambiguous channel addresses."
  (let ((matches (world--participants-where world
                                           (lambda (participant)
                                             (and (participant-residence participant)
                                                  (string-equal name (participant-name participant)))))))
    (when (rest matches)
      (error 'world-error :pathname nil :operation ':resolve :identifier name
             :message (format nil "Agent name ~S is ambiguous. Candidates:~%~{  ~A~^~%~}" name
                              (sort (loop for participant in matches
                                          for residence = (participant-residence participant)
                                          for channel = (and residence (world-channel-find world residence))
                                          collect (cond
                                                    ((and channel
                                                          (= 1 (count ':agent
                                                                (world-channel-residents world residence)
                                                                :key #'participant-kind)))
                                                     (world-channel-path world channel))
                                                    (channel
                                                     (format nil "~A (agent ~A)" (world-channel-path world channel)
                                                             (conversation-identifier-display
                                                              (participant-identifier participant))))
                                                    (t
                                                      (conversation-identifier-display
                                                       (participant-identifier participant)))))
                                    #'string<))))
    (first matches)))

(-> world--path-participant (world string) (option participant))
(defun world--path-participant (world path)
  "Return the sole agent residing at PATH; refuse an agentless or shared channel."
  (let* ((channel (world-channel-at-path world path))
         (agents  (remove-if-not (lambda (participant) (eq (participant-kind participant) ':agent))
                                 (world-channel-residents world (channel-identifier channel)))))
    (unless (and agents (null (rest agents)))
      (error 'world-error :pathname nil :operation ':resolve :identifier path
             :message (format nil "Channel ~A has ~D resident agents; address an agent by name."
                              path (length agents))))
    (first agents)))

(-> world--channel-label (world channel) string)
(defun world--channel-label (world channel)
  "Return CHANNEL's current path followed by its stable identifier and available title."
  (format nil "~A  ~A~@[  ~A~]"
          (world-channel-path world channel)
          (conversation-identifier-display (channel-identifier channel))
          (channel-title channel)))

(-> world-channel-resolve (world string &key (:error-p boolean)) (option channel))
(defun world-channel-resolve (world fragment &key (error-p t))
  "Return the one channel of WORLD that FRAGMENT names in full or in part.

An exact identifier in canonical or display form wins; otherwise the one
channel whose identifier contains FRAGMENT, compared without regard to case.
Several matches are reported together, newest change first, and select
nothing. No match signals an error unless ERROR-P is NIL."
  (let ((matches (identifier-match fragment (world-channels world) :key #'channel-identifier)))
    (cond
      ((null matches)
       (when error-p
         (error 'world-error
                :message    (format nil "No channel matches ~A." fragment)
                :pathname   nil
                :operation  ':resolve
                :identifier fragment)))
      ((rest matches)
       (error 'world-channel-ambiguous
              :message    (format nil "Channel ID ~S is ambiguous. Matching channels:~%~{  ~A~^~%~}"
                                  fragment
                                  (mapcar (lambda (channel) (world--channel-label world channel)) matches))
              :channels   matches
              :pathname   nil
              :operation  ':resolve
              :identifier fragment))
      (t
       (first matches)))))

(-> world-participant-named-p (world string &key (:except (option string))) boolean)
(defun world-participant-named-p (world name &key except)
  "Return true when a participant of WORLD other than EXCEPT is named NAME, ignoring case."
  (loop for participant being the hash-values of (world--participant-table world)
        thereis (and (participant-residence participant)
                     (not (equal (participant-identifier participant) except))
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
  "Return an active participant by ID, @name, exact channel path or unique ID fragment."
  (let* ((explicit-name-p (uiop:string-prefix-p "@" designator))
         (designator      (if explicit-name-p (subseq designator 1) designator)))
    (if (uiop:string-prefix-p "/" designator)
        (world--path-participant world designator)
        (or (world-message-participant world designator :name-first-p explicit-name-p)
            (let ((matches (identifier-match designator (world--participants-where world #'participant-residence)
                                              :key #'participant-identifier)))
              (and (= (length matches) 1) (first matches)))))))

(-> world-participant-named (world string) (option participant))
(defun world-participant-named (world name)
  "Return the one participant of WORLD named NAME, ignoring case, or NIL when none or several are."
  (let ((named nil))
    (loop for participant being the hash-values of (world--participant-table world)
          when (and (participant-residence participant)
                    (string-equal (participant-name participant) name))
            do (push participant named))
    (and (= (length named) 1) (first named))))

(-> world-channel-designated
    (world configuration string &key (:error-p boolean) (:sender (option participant))) (option channel))
(defun world-channel-designated (world configuration designator &key (error-p t) sender)
  "Return the channel DESIGNATOR names in WORLD by identifier, legacy alias, agent or fragment.

The word inbox names the person's inbox, and @NAME the participant NAME's
residence. With SENDER, @parent names the residence of its parent recipient.
A full channel identifier, including a retained legacy alias of CONFIGURATION,
takes priority over a bare participant name or identifier. Otherwise resolve
a channel ID fragment. With ERROR-P NIL, return NIL for an unknown fragment;
ambiguous or unresolved explicit addresses still signal an error."
  (when (uiop:string-prefix-p "/" designator)
    (return-from world-channel-designated (world-channel-at-path world designator :error-p error-p)))
  (or (and (string-equal designator *world-inbox-designator*)
           (world-inbox-channel world))
      (and (uiop:string-prefix-p "@" designator)
           (world--residence-channel world (subseq designator 1) :sender sender :name-first-p t))
      (let ((resolved (handler-case
                          (conversation-identifier-migration-resolve configuration designator)
                        ((or conversation-error conversation-identifier-error) ()
                          nil))))
        (and resolved (world-channel-find world resolved)))
      (and (or (world--participant-name-target world designator)
               (world-participant-find world designator))
           (world--residence-channel world designator))
      (world-channel-resolve world designator :error-p error-p)))

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
  (let ((nodes (make-hash-table :test #'equal))
        (roots nil))
    (dolist (channel members)
      (setf (gethash (channel-identifier channel) nodes) (list channel)))
    ;; Prepending in reverse presentation order preserves both roots and siblings.
    (dolist (channel (reverse members) roots)
      (let ((node   (gethash (channel-identifier channel) nodes))
            (parent (gethash (channel-parent channel) nodes)))
        (if parent
            (push node (rest parent))
            (push node roots))))))

(-> world--residence-channel
    (world string &key (:sender (option participant)) (:name-first-p boolean)) channel)
(defun world--residence-channel (world name &key sender name-first-p)
  "Return the residence of NAME, which may name SENDER's parent recipient."
  (let ((participant (world-message-participant world name :sender sender :name-first-p name-first-p)))
    (cond
      ((null participant)
       (error 'world-error
              :message    (format nil "No participant is named ~A." name)
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
                               (and (participant-residence participant)
                                    (not (null (member channel-identifier
                                                       (participant-subscriptions participant)
                                                       :test #'string=)))))))

(-> world-channel-children (world string) list)
(defun world-channel-children (world channel-identifier)
  "Return the channels nested directly under CHANNEL-IDENTIFIER."
  (world--channels-where world
                         (lambda (channel)
                           (equal (channel-parent channel) channel-identifier))))

(-> world-channel-top-level-p (world channel) boolean)
(defun world-channel-top-level-p (world channel)
  "Recognize a non-inbox CHANNEL directly under home, or a legacy unparented one."
  (and (not (channel-inbox-p channel))
       (let* ((identifier (channel-parent channel))
              (parent     (and identifier (world-channel-find world identifier))))
         (or (null identifier) (and parent (channel-inbox-p parent) t)))))

(-> world-channel-latest-top-level (configuration string) (option string))
(defun world-channel-latest-top-level (configuration workspace)
  "Return the ID of the latest written top-level channel in WORKSPACE.

Compare storage timestamps without reading any conversation records."
  (let ((world        (world-load configuration))
        (latest       nil)
        (latest-write -1))
    (loop for channel being the hash-values of (world--channel-table world)
          when (and (world-channel-top-level-p world channel)
                    (string= workspace (channel-workspace channel)))
            do (let* ((identifier (channel-identifier channel))
                      (written    (conversation-storage-write-date
                                   (conversation-pathname-for-id configuration identifier))))
                 (when (or (null latest)
                           (> written latest-write)
                           (and (= written latest-write) (string< identifier latest)))
                   (setf latest       identifier
                         latest-write written))))
    latest))
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

(-> world--require-participant (world string keyword) participant)
(defun world--require-participant (world identifier operation)
  "Return participant IDENTIFIER, or signal WORLD-ERROR for OPERATION."
  (or (world-participant-find world identifier)
      (error 'world-error
             :message    (format nil "Participant ~A does not exist." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier)))

(-> world--require-active-participant (world string keyword) participant)
(defun world--require-active-participant (world identifier operation)
  "Require the exact canonical ID of an active participant for OPERATION."
  (let ((participant (gethash identifier (world--participant-table world))))
    (unless (and participant (participant-residence participant))
      (error 'world-error :message (format nil "No active participant has ID ~A." identifier)
             :pathname nil :operation operation :identifier identifier))
    participant))
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
  (let* ((workspace  (world-workspace-name (getf home :workspace)))
         (repository (world-repository-resolve-directory workspace))
         (identifier (or (getf home :id)
                        (conversation-identifier-generate
                         (configuration-conversation-root configuration)
                         :timestamp now)))
         (parent     (or (getf home :parent)
                         (let ((inbox (world-inbox-channel world)))
                           (and inbox (channel-identifier inbox)))))
         (title      (getf home :title)))
    (when (gethash identifier (world--deleted-channel-table world))
      (error 'world-error
             :message    (format nil "Channel ~A was removed; use a new channel identity." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier))
    (when (world-channel-find world identifier)
      (error 'world-error
             :message    (format nil "Channel ~A already exists." identifier)
             :pathname   nil
             :operation  operation
             :identifier identifier))
    (when parent (world--require-channel world parent operation))
    (make-instance 'channel
                   :identifier           identifier
                   :parent               parent
                   :owner                owner
                   :workspace            workspace
                   :repository-key       (getf repository :key)
                   :repository-directory (getf repository :directory)
                   :title                (and title (world--validate-name title operation))
                   :created-at           now
                   :updated-at           now)))

(-> world-ensure-channel-identities (configuration) null)
(defun world-ensure-channel-identities (configuration)
  "Convert legacy active agent IDs to their existing channel IDs in one world transaction.

Only catalogue metadata changes. Original agent IDs are historical aliases;
inactive records and all conversation and transfer files are left alone. A
legacy agent is retired without conversion if a participant already has the
channel ID and records the legacy ID as its alias."
  (world--transact
   configuration
   (lambda (world)
     (let ((identifiers (make-hash-table :test #'equal))
           (duplicates  (make-hash-table :test #'equal))
           (retired     nil)
           (records     nil)
           (now         (get-universal-time)))
       (maphash (lambda (identifier agent)
                  (let ((channel (participant-residence agent)))
                    (when (and (eq (participant-kind agent) ':agent)
                               channel (not (equal identifier channel)))
                      (let ((converted (world-participant-find world channel)))
                        (cond
                          ((null converted)
                           nil)
                          ;; A process of an older version wrote the legacy record again.
                          ((equal (participant-legacy-identifier converted) identifier)
                           (setf (gethash identifier duplicates) t))
                          (t
                           (error 'world-error :operation ':channel-identities :identifier channel
                                  :message "A participant already uses a channel's canonical identity."))))
                      (setf (gethash identifier identifiers) channel)
                      (push (list :participant-deleted :version *world-format-version*
                                  :id identifier :time now)
                            retired))))
                (world--participant-table world))
       (unless (zerop (hash-table-count identifiers))
         (maphash
          (lambda (identifier participant)
            (let ((channel (gethash identifier identifiers))
                  (parent  (gethash (participant-parent participant) identifiers)))
              (when (and (or channel parent) (not (gethash identifier duplicates)))
                (push (world--participant-record
                       (world--copy-participant participant
                                                :identifier (or channel identifier)
                                                :legacy-identifier (if channel identifier
                                                                       (participant-legacy-identifier participant))
                                                :parent (or parent (participant-parent participant))))
                      records))))
          (world--participant-table world))
         (maphash (lambda (identifier channel)
                    (declare (ignore identifier))
                    (let ((owner (gethash (channel-owner channel) identifiers)))
                      (when owner
                        (push (world--channel-record (world--copy-channel channel :owner owner)) records))))
                  (world--channel-table world)))
       (values (nconc retired records) nil))))
  nil)

(-> world-ensure-inbox (configuration &key (:now timestamp)) channel)
(defun world-ensure-inbox (configuration &key (now (get-universal-time)))
  "Return the person's durable inbox, creating it without consulting session history."
  (or (world-inbox-channel (world-load configuration))
      (world--transact
       configuration
       (lambda (world)
         (let ((existing (world-inbox-channel world)))
           (if existing
               (values nil existing)
               (let* ((person       (world-human-participant world))
                      (owner        (if person (participant-identifier person) "user"))
                      (workspace    (namestring (user-homedir-pathname)))
                      (conversation (conversation-create configuration :inbox-p t
                                                         :origin-directory workspace :created-at now))
                      (identifier   (conversation-identifier conversation))
                      (inbox        (make-instance 'channel :identifier identifier :owner owner
                                                    :workspace workspace :title *world-inbox-designator*
                                                    :inbox-p t :created-at now :updated-at now)))
                 (when (and (null person) (world-participant-find world owner))
                   (error 'world-error :operation ':register-inbox :identifier owner
                          :message "The person identifier belongs to another participant."))
                 (conversation-append-record conversation
                                             (list :title :value *world-inbox-designator* :source ':initial))
                 (values (list (world--channel-record inbox)
                               (world--participant-record
                                (if person
                                    (world--copy-participant person :residence identifier
                                                             :updated-at (max now (participant-updated-at person)))
                                    (make-instance 'participant :identifier owner :kind ':human :name "user"
                                                   :residence identifier :created-at now :updated-at now))))
                         inbox))))))))

(-> world-channel-register
    (configuration string &key (:workspace string) (:parent (option string)) (:title (option string))
                   (:name (option string)) (:settings list) (:now timestamp))
    (values channel participant))
(defun world-channel-register (configuration identifier &key workspace parent title name settings
                                                            (now (get-universal-time)))
  "Register IDENTIFIER as a durable channel and agent, returning those two records.

IDENTIFIER is the already selected conversation ID. Existing channel and
agent metadata wins over startup defaults. Registration never reads or changes
the channel's history."
  (conversation-identifier-validate-path-component identifier)
  (let* ((world    (world-load configuration))
         (channel  (world-channel-find world identifier))
         (resident (gethash identifier (world--participant-table world))))
    (when (and channel resident (eq (participant-kind resident) ':agent)
               (equal identifier (participant-residence resident)))
      (return-from world-channel-register (values channel resident)))
    (when (and channel (find ':agent (world-channel-residents world identifier) :key #'participant-kind))
      (world-ensure-channel-identities configuration)))
  (world-ensure-inbox configuration :now now)
  (values-list
   (world--transact
    configuration
    (lambda (world)
      (world-inbox-refuse-agent world identifier ':register-channel)
      (let* ((existing (world-channel-find world identifier))
             (resident (and existing (find ':agent (world-channel-residents world identifier)
                                           :key #'participant-kind))))
        (if resident
            (values nil (list existing resident))
            (let* ((person (world-human-participant world))
                   (owner  (if existing
                               (channel-owner existing)
                               (or (and parent
                                        (find ':agent (world-channel-residents world parent)
                                              :key #'participant-kind))
                                   person)))
                   (keeper (if (stringp owner) owner (participant-identifier owner)))
                   (channel (or existing
                                (world--new-channel configuration world
                                                    :home (list :id identifier :workspace workspace
                                                                :title title :parent parent)
                                                    :owner keeper :now now :operation ':register-channel))))
              (when (world-participant-find world identifier)
                (error 'world-error :operation ':register-channel :identifier identifier
                       :message "The channel identifier belongs to another participant."))
              (unless (world-settings-p settings)
                (error 'world-error :operation ':register-channel :identifier identifier
                       :message "The agent settings are not portable."))
              (let ((agent (make-instance
                            'participant :identifier identifier :kind ':agent
                            :name (if name
                                      (world-unique-handle world (world-handle-base name))
                                      (prezdivka:unique-name
                                       identifier
                                       (lambda (handle)
                                         (or (world-participant-named-p world handle)
                                             (world-handle-reserved-p handle)))))
                            :name-source (if name ':explicit ':initial)
                            :parent keeper :residence identifier :settings settings
                            :created-at (channel-created-at channel) :updated-at now)))
                (values (append (unless existing (list (world--channel-record channel)))
                                (list (world--participant-record agent)))
                        (list channel agent))))))))))

(-> world-register-conversation (configuration conversation) (option participant))
(defun world-register-conversation (configuration conversation)
  "Register a durable CONVERSATION using its in-memory metadata, without replaying history."
  (when (and (conversation-persisted-p conversation) (not (conversation-inbox-p conversation)))
    (let* ((identifier (conversation-identifier conversation))
           (title      (conversation-title conversation))
           (name       (conversation-agent-name conversation))
           (parent     (conversation-parent-identifier conversation))
           (world      (world-load configuration)))
      (multiple-value-bind (channel participant)
          (world-channel-register
           configuration identifier
           :workspace (or (conversation-origin-directory conversation)
                          (config :working-directory configuration))
           :title     (and (world--name-p title) title)
           :name      (and (non-empty-string-p name) name)
           :parent    (and parent (world-channel-find world parent) parent)
           :now       (conversation-created-at conversation)
           :settings  (loop for (key value) on (list :model            (conversation-model conversation)
                                                     :model-provider   (conversation-model-provider conversation)
                                                     :reasoning-effort (conversation-reasoning-effort conversation))
                           by (partial #'nthcdr 2)
                            when (and value (world--portable-value-p value))
                              append (list key value)))
        (when (and (world--name-p title) (not (equal title (channel-title channel))))
          (world-channel-update configuration identifier :title title))
        participant))))

(-> world-participant-create
    (configuration &key (:kind participant-kind) (:name string) (:residence (option string))
                        (:name-source (member :initial :explicit))
                        (:home list) (:parent (option string)) (:settings list)
                        (:id (option string)) (:now timestamp))
    (values participant channel))
(defun world-participant-create
    (configuration &key kind name residence home parent (settings nil settings-p)
                        (name-source ':explicit) id (now (get-universal-time)))
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
             (check-type name-source (member :initial :explicit))
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
                                                :identifier  identifier
                                                :kind        kind
                                                :name        (world--validate-name
                                                              name ':create-participant)
                                                :name-source name-source
                                                :parent      parent
                                                :residence   (channel-identifier channel)
                                                :settings    (copy-list settings)
                                                :created-at  now
                                                :updated-at  now)))
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
     (let* ((participant (world--require-active-participant world identifier operation))
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

(-> world-participant-rename
    (configuration string string
     &key (:now timestamp) (:source (member :explicit :generated))
          (:expected-residence (option string)))
    participant)
(defun world-participant-rename
    (configuration identifier name
     &key (now (get-universal-time)) (source ':explicit) expected-residence)
  "Name participant IDENTIFIER NAME and return it.

An explicit name must be one no other participant has. A generated name, one
inferred from the agent's work, replaces only the name an agent started with,
and only while the agent still resides in EXPECTED-RESIDENCE; a name another
participant has is given a numbered suffix in the same transaction."
  (flet ((refuse (control &rest arguments)
           (error 'world-error
                  :message    (apply #'format nil control arguments)
                  :pathname   nil
                  :operation  ':rename-participant
                  :identifier identifier)))
    (world--update-participant
     configuration identifier :operation ':rename-participant :now now
     :change (lambda (world participant)
               (block nil
                 (when (and (eq source ':generated)
                            (not (and (eq (participant-kind participant) ':agent)
                                      (eq (participant-name-source participant) ':initial)
                                      expected-residence
                                      (equal expected-residence
                                             (participant-residence participant)))))
                   (return nil))
                 (cond
                   ((not (world-handle-p name))
                    (refuse "A name is up to ~D letters, digits, hyphens and underscores, so @mentions can use it."
                            *world-handle-limit*))
                   ((world-handle-reserved-p name)
                    (refuse "@~A always names the sender's parent; choose another name." name)))
                 (let ((taken-p (world-participant-named-p world name :except identifier)))
                   (when (and taken-p (eq source ':explicit))
                     (refuse "Another participant is already named ~A." name))
                   (list :name        (if taken-p
                                          (world-unique-handle world name)
                                          name)
                         :name-source source)))))))

(-> world-channel-update
    (configuration string &key (:title (option string)) (:workspace (option string))
                          (:now timestamp))
    channel)
(defun world-channel-update
    (configuration identifier &key title workspace (now (get-universal-time)))
  "Replace the given channel fields; an execution-directory move replaces repository attribution."
  (let* ((directory  (and workspace (world-workspace-name workspace)))
         (repository (and directory (world-repository-resolve-directory directory))))
    (world--transact
     configuration
     (lambda (world)
       (let* ((channel (world--require-channel world identifier ':update-channel))
              (moved-p (and directory (not (equal directory (channel-workspace channel)))))
              (updated (world--copy-channel
                        channel
                        :title                (if title
                                                  (world--validate-name title ':update-channel)
                                                  (channel-title channel))
                        :workspace            (or directory (channel-workspace channel))
                        :repository-key       (if (or moved-p repository) (getf repository :key)
                                                  (channel-repository-key channel))
                        :repository-directory (if (or moved-p repository) (getf repository :directory)
                                                  (channel-repository-directory channel))
                        :updated-at           (max now (channel-updated-at channel)))))
         (values (list (world--channel-record updated)) updated))))))

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
     (let ((participant (world--require-active-participant world identifier ':delete-participant)))
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
  "Delete IDENTIFIER and its listed resident PARTICIPANTS, after deleting nested channels."
  (world-channels-delete configuration (list identifier) :participants participants :now now))

(-> world-channels-delete
    (configuration list &key (:participants list) (:now timestamp)
                             (:prepare-function (option function)))
    list)
(defun world-channels-delete (configuration identifiers &key participants (now (get-universal-time))
                                                           prepare-function)
  "Delete IDENTIFIERS deepest first in one transaction, returning deleted participants.

Every resident and its descendants must occur in PARTICIPANTS. Validate the
complete batch before publication. PREPARE-FUNCTION, when supplied, receives
the actual deleted participant IDs under the store lock after validation and
before publication. It must not destructively modify storage."
  (world--transact
   configuration
   (lambda (world)
     (let ((preview (world--copy world))
           (records nil)
           (deleted nil))
       (dolist (identifier identifiers)
         (multiple-value-bind (batch doomed)
             (world--channel-deletion-records preview identifier :participants participants :now now)
           (dolist (record batch)
             (when (and prepare-function (eq (first record) ':channel-deleted))
               (setf (getf (rest record) :storage-cleanup-p) t))
             (world--reduce-record (configuration-world-path configuration) preview record))
           (setf records (nconc records batch)
                 deleted (nconc deleted doomed))))
       (world--check-integrity (configuration-world-path configuration) preview :operation ':write)
       (when prepare-function
         (funcall prepare-function deleted))
       (values records deleted)))))

(-> world--channel-deletion-records
    (world string &key (:participants list) (:now timestamp))
    (values list list))
(defun world--channel-deletion-records (world identifier &key participants now)
  "Return deletion records and deleted participants for one leaf IDENTIFIER in WORLD."
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
              (nreverse deleted)))))
