(in-package #:autolith)

;;;; -- Conversation Object --

(-> resource-observation-state-weight (t t) (integer 0))
(defgeneric resource-observation-state-weight (alias state)
  (:documentation "Return STATE's retained byte weight under opaque ALIAS."))

(defparameter *conversation-title-maximum-characters* 64
  "The maximum number of characters retained in one session title.")

(defparameter *conversation-title-refresh-turn-count* 3
  "The durable user-turn count after which the initial session title is refreshed.")

(defparameter *conversation-image-only-title* "Image attachment"
  "The immediate local title and transcript marker for image-only turns.")

(defparameter *conversation-title-context-maximum-characters* 12000
  "The maximum transcript characters supplied to automatic title generation.")

(defparameter *conversation-title-generation-output-tokens* 64
  "The provider output-token ceiling for one automatic title request.")

(defparameter *conversation-title-generation-minimum-words* 3
  "The minimum accepted word count for one generated session title.")

(defparameter *conversation-title-generation-maximum-words* 8
  "The maximum accepted word count for one generated session title.")

(defparameter *conversation-title-generation-rejected-prefixes*
  '("here is " "here's " "i cannot " "i can't " "sorry " "the title " "title:")
  "Lowercase explanatory prefixes rejected from generated session titles.")

(defparameter *conversation-turn-aborted-message-maximum-characters* 1000
  "The maximum condition-message characters retained by one aborted turn.")

(defparameter *conversation-turn-aborted-condition-type-maximum-characters* 160
  "The maximum condition-type characters retained by one aborted turn.")

(-> conversation-title--collapse-whitespace (string) string)
(defun conversation-title--collapse-whitespace (text)
  "Return TEXT with control characters removed and whitespace collapsed."
  (with-output-to-string (stream)
    (loop with pending-space-p = nil
          with wrote-p = nil
          for character across text
          do (cond
               ((member character '(#\Space #\Tab #\Newline #\Return))
                (when wrote-p
                  (setf pending-space-p t)))
               ((graphic-char-p character)
                (when pending-space-p
                  (write-char #\Space stream))
                (write-char character stream)
                (setf pending-space-p nil
                      wrote-p t))))))

(-> conversation-title--truncate (non-empty-string) non-empty-string)
(defun conversation-title--truncate (title)
  "Return TITLE bounded to the configured title length at a word boundary."
  (if (<= (length title) *conversation-title-maximum-characters*)
      title
      (let* ((limit (1- *conversation-title-maximum-characters*))
             (boundary (position #\Space title :end limit :from-end t))
             (end (if (and boundary (>= boundary 16)) boundary limit)))
        (concatenate 'string
                     (string-right-trim '(#\Space) (subseq title 0 end))
                     "…"))))

(-> conversation-title-normalize (string) (option non-empty-string))
(defun conversation-title-normalize (text)
  "Return display-safe bounded session title TEXT, or NIL when it is empty."
  (let* ((collapsed (conversation-title--collapse-whitespace text))
         (trimmed
           (string-trim '(#\Space #\" #\' #\`)
                        collapsed))
         (without-leading-markup
           (string-left-trim '(#\Space #\# #\* #\_ #\> #\-)
                             trimmed))
         (without-label
           (if (and (>= (length without-leading-markup) 6)
                    (string-equal "title:" without-leading-markup :end2 6))
               (subseq without-leading-markup 6)
               without-leading-markup))
         (clean
           (string-right-trim
            '(#\Space #\. #\, #\: #\; #\! #\? #\- #\* #\_)
            (string-left-trim '(#\Space #\# #\* #\_ #\> #\-)
                              without-label))))
    (when (plusp (length clean))
      (let ((title (copy-seq (conversation-title--truncate clean))))
        (when (lower-case-p (char title 0))
          (setf (char title 0) (char-upcase (char title 0))))
        title))))

(-> conversation-title-generated-normalize (string) (option non-empty-string))
(defun conversation-title-generated-normalize (text)
  "Return TEXT as a valid generated title, or NIL when its shape is unsuitable."
  (let* ((collapsed (conversation-title--collapse-whitespace text))
         (lowercase (string-downcase collapsed))
         (normalized (conversation-title-normalize text))
         (words
           (remove ""
                   (uiop:split-string collapsed
                                      :separator '(#\Space #\Tab #\Newline #\Return))
                   :test #'string=)))
    (when (and normalized
               (<= (length collapsed) *conversation-title-maximum-characters*)
               (<= *conversation-title-generation-minimum-words*
                   (length words)
                   *conversation-title-generation-maximum-words*)
               (not (find-if
                     (lambda (character)
                       (member character
                               '(#\Newline #\Return #\" #\` #\{ #\} #\[ #\])))
                     text))
               (not (and (plusp (length collapsed))
                         (member (char collapsed 0) '(#\# #\* #\>))))
               (not (some (lambda (prefix)
                            (uiop:string-prefix-p prefix lowercase))
                          *conversation-title-generation-rejected-prefixes*)))
      normalized)))

(-> conversation-title-derive (string) (option non-empty-string))
(defun conversation-title-derive (prompt)
  "Derive an immediate session title from the leading sentence of PROMPT."
  (let* ((collapsed (conversation-title--collapse-whitespace prompt))
         (boundary
           (position-if
            (lambda (character)
              (member character '(#\. #\! #\?)))
            collapsed))
         (candidate
           (if (and boundary (>= boundary 8))
               (subseq collapsed 0 boundary)
               collapsed)))
    (conversation-title-normalize candidate)))

(-> conversation-title-valid-p (t) boolean)
(defun conversation-title-valid-p (value)
  "Return true when VALUE is already a normalized bounded session title."
  (and (non-empty-string-p value)
       (<= (length value) *conversation-title-maximum-characters*)
       (let ((normalized (conversation-title-normalize value)))
         (and normalized (string= value normalized)))
       t))

(defclass conversation ()
  ((identifier
    :initarg :identifier
    :reader conversation-identifier
    :type non-empty-string
    :documentation "The stable conversation identifier.")
   (prompt-cache-key
    :initarg :prompt-cache-key
    :initform nil
    :accessor conversation-prompt-cache-key
    :type (option non-empty-string)
    :documentation "The root-lineage key shared by provider prompt caches.")
   (pathname
    :initarg :pathname
    :reader conversation-pathname
    :type pathname
    :documentation "The stable top-level identity pathname for this conversation.")
   (log-pathname
    :initarg :log-pathname
    :accessor conversation-log-pathname
    :type pathname
    :documentation "The active legacy log or deterministic chunk file.")
   (persisted-p
    :initarg :persisted-p
    :accessor conversation-persisted-p
    :type boolean
    :documentation "True after the header and first durable record are published.")
   (incomplete-tail-p
    :initarg :incomplete-tail-p
    :initform nil
    :accessor conversation-incomplete-tail-p
    :type boolean
    :documentation "Whether the next append must repair an interrupted final form.")
   (log-generation
    :initform 0
    :accessor conversation-log-generation
    :type (integer 0)
    :documentation "The count of active-log replacements since this object loaded.")
   (append-lock
    :initform (make-recursive-lock "Autolith conversation append")
    :reader conversation-append-lock
    :type t
    :documentation "The lock serializing durable record sequence assignment.")
   (created-at
    :initarg :created-at
    :reader conversation-created-at
    :type timestamp
    :documentation "The creation time as Common Lisp universal time.")
   (origin-directory
    :initarg :origin-directory
    :initform nil
    :reader conversation-origin-directory
    :type (option string)
    :documentation "The workspace directory in which this conversation began.")
   (title
    :initarg :title
    :initform nil
    :accessor conversation-title
    :type (option non-empty-string)
    :documentation "The current human-readable session title.")
   (title-source
    :initarg :title-source
    :initform nil
    :accessor conversation-title-source
    :type (member nil :initial :generated)
    :documentation "Whether the title came from the initial prompt or a later model pass.")
   (title-refresh-in-progress-p
    :initform nil
    :accessor conversation-title-refresh-in-progress-p
    :type boolean
    :documentation "Whether this object has reserved the automatic generated-title request.")
   (model
    :initarg :model
    :initform nil
    :accessor conversation-model
    :type (option non-empty-string)
    :documentation "The provider model most recently selected for this conversation.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :accessor conversation-reasoning-effort
    :type (option non-empty-string)
    :documentation "The reasoning effort most recently selected for this conversation.")
   (next-sequence
    :initarg :next-sequence
    :accessor conversation-next-sequence
    :type integer
    :documentation "The sequence number assigned to the next appended event.")
    (last-aborted-turn-start-sequence
     :initform nil
     :accessor conversation-last-aborted-turn-start-sequence
     :type (option integer)
     :documentation "The start sequence of the latest turn-aborted marker in memory.")
   (projection
    :initform (clinker-transcript:make-projection)
    :reader conversation-projection
    :type clinker-transcript:projection
    :documentation "The ordered provider projection and its item metadata.")
   (ephemeral-input-entries
    :initform nil
    :accessor conversation-ephemeral-input-entries
    :type list
    :documentation
    "Request-local provider items and owned attachments awaiting one response.")
    (image-artifact-names
     :initform (make-hash-table :test #'equal)
     :reader conversation-image-artifact-names
     :type hash-table
     :documentation "The basenames of image artifacts referenced by durable records.")
   (resource-observations
    :initform (make-fifo-cache
               :test            #'equal
               :weight-function #'resource-observation-state-weight)
    :reader conversation-resource-observations
    :type fifo-cache
    :documentation
    "Transient model-visible resource revisions in FIFO insertion order.")
   (resource-observation-lock
    :initform (make-recursive-lock "Autolith resource observations")
    :reader conversation-resource-observation-lock
    :type t
    :documentation
    "The lock serializing transient resource observations and gated edits.")
   (turn-state
    :initform nil
    :accessor conversation-turn-state
    :type (option string)
    :documentation "The transient provider routing token for one user turn.")
   (last-total-tokens
    :initform 0
    :accessor conversation-last-total-tokens
    :type (integer 0)
    :documentation "The total token usage reported by the newest provider step.")
   (last-activity-at
    :initform nil
    :accessor conversation-last-activity-at
    :type (option timestamp)
    :documentation "The newest timestamp observed in a durable record.")
   (user-turn-count
    :initform 0
    :accessor conversation-user-turn-count
    :type (integer 0)
    :documentation "The number of durable user message records.")
   (working-seconds
    :initform 0
    :accessor conversation-working-seconds
    :type (integer 0)
    :documentation
    "Accumulated seconds of agent work, excluding gaps before user messages.")
   (picker-search-message-count
    :initform 0
    :accessor conversation-picker-search-message-count
    :type (integer 0)
    :documentation "The count of durable user and assistant search messages.")
   (pending-input-identifiers
    :initform nil
    :accessor conversation-durable-pending-input-identifiers
    :type list
    :documentation "Durable pending-input identifiers in chronological first-seen order.")
   (picker-preview
    :initform nil
    :accessor conversation-picker-preview
    :type (option string)
    :documentation "The newest user or assistant text retained for conversation pickers.")
   (user-operation-records
    :initform (make-deque
               :maximum-count 16
               :weight-function
               (lambda (record)
                 (let ((properties (rest record)))
                   (+ (length (getf properties :source))
                      (length (getf properties :result)))))
               :maximum-weight 32000)
    :reader conversation-user-operation-records
    :type deque
    :documentation
    "Recent bounded local user operations in chronological durable order.")
   (latest-goal-record
    :initform nil
    :accessor conversation-latest-goal-record
    :type (option list)
    :documentation "The newest durable goal record observed in this conversation."))
  (:documentation "An append-only conversation and its provider projection."))

(defmethod initialize-instance
    :after ((conversation conversation) &key input-items &allow-other-keys)
  "Initialize CONVERSATION's provider projection and lineage key."
  (unless (conversation-prompt-cache-key conversation)
    (setf (conversation-prompt-cache-key conversation)
          (conversation-identifier conversation)))
  (clinker-transcript:projection-replace
   (conversation-projection conversation) input-items))

(-> conversation-input-items (conversation) list)
(defun conversation-input-items (conversation)
  "Return a chronological snapshot of CONVERSATION's provider items."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (clinker-transcript:projection-items (conversation-projection conversation))))

(defun (setf conversation-input-items) (items conversation)
  "Replace CONVERSATION's provider projection and prune discarded metadata."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (clinker-transcript:projection-replace (conversation-projection conversation) items))
  items)

(-> conversation-input-item-families (conversation) hash-table)
(defun conversation-input-item-families (conversation)
  "Return the item-identity table of producing model families."
  (clinker-transcript:projection-metadata-table
   (conversation-projection conversation) ':family))

(-> conversation-portable-handoff-families (conversation) hash-table)
(defun conversation-portable-handoff-families (conversation)
  "Return the item-identity table of native handoff exclusions."
  (clinker-transcript:projection-metadata-table
   (conversation-projection conversation) ':handoff-family))


(defclass conversation-picker-metadata ()
  ((source-segment
    :initarg :source-segment
    :reader conversation-picker-metadata-source-segment
    :type non-empty-string
    :documentation "The namestring of the indexed active conversation segment.")
   (source-size
    :initarg :source-size
    :reader conversation-picker-metadata-source-size
    :type (integer 0)
    :documentation "The byte size of the indexed active conversation segment.")
   (source-write-date
    :initarg :source-write-date
    :reader conversation-picker-metadata-source-write-date
    :type (integer 0)
    :documentation "The indexed active conversation segment's write date.")
   (source-revision
    :initarg :source-revision
    :reader conversation-picker-metadata-source-revision
    :type (integer 0)
    :documentation "The durable picker-cache revision captured with the log.")
   (working-seconds
    :initarg :working-seconds
    :reader conversation-picker-metadata-working-seconds
    :type (integer 0)
    :documentation "The accumulated durable agent-working seconds.")
   (user-turn-count
    :initarg :user-turn-count
    :reader conversation-picker-metadata-user-turn-count
    :type (integer 0)
    :documentation "The count of durable user-message records.")
   (title
    :initarg :title
    :initform nil
    :reader conversation-picker-metadata-title
    :type (option non-empty-string)
    :documentation "The current human-readable session title.")
   (search-message-count
    :initarg :search-message-count
    :reader conversation-picker-metadata-search-message-count
    :type (integer 0)
    :documentation "The count of indexed durable user and assistant messages.")
   (preview
    :initarg :preview
    :reader conversation-picker-metadata-preview
    :type (option string)
    :documentation "The newest user or assistant message text.")
   (incomplete-tail-p
    :initarg :incomplete-tail-p
    :reader conversation-picker-metadata-incomplete-tail-p
    :type boolean
    :documentation "Whether indexing stopped before an interrupted final form."))
  (:documentation "A validated compact cache of one conversation's picker fields."))


(defclass conversation-picker-search-index ()
  ((source-revision
    :initarg :source-revision
    :reader conversation-picker-search-index-source-revision
    :type (integer 0)
    :documentation "The durable message-search revision captured with the log.")
   (message-count
    :initarg :message-count
    :reader conversation-picker-search-index-message-count
    :type (integer 0)
    :documentation "The count of indexed durable user and assistant messages.")
   (messages
    :initarg :messages
    :reader conversation-picker-search-index-messages
    :type list
    :documentation "Chronological durable user and assistant message text."))
  (:documentation "A validated durable search index for one conversation picker."))


;;;; -- Primary Application Ownership --

(defclass conversation-lease ()
  ((identifier
    :initarg :identifier
    :reader conversation-lease-identifier
    :type non-empty-string
    :documentation "The normalized conversation identifier held by this lease.")
   (lock-pathname
    :initarg :lock-pathname
    :reader conversation-lease--lock-pathname
    :type pathname
    :documentation "The per-conversation lock file removed after a normal release.")
   (guard-pathname
    :initarg :guard-pathname
    :reader conversation-lease--guard-pathname
    :type pathname
    :documentation "The shared lock serializing per-conversation lock-file lifecycle.")
   (lock-lease
    :initarg :lock-lease
    :reader conversation-lease--lock-lease
    :type ls-flock:lease
    :documentation "The process-shared exclusive lock lease."))
  (:documentation
   "A process-lifetime exclusive lease on one primary conversation."))

(-> conversation--lease-pathname (configuration string) pathname)
(defun conversation--lease-pathname (configuration identifier)
  "Return the process-shared lease pathname for normalized IDENTIFIER."
  (merge-pathnames
   (make-pathname :name identifier :type "lock")
   (merge-pathnames
    "conversation-leases/"
    (configuration-state-root configuration))))

(-> conversation--lease-guard-pathname (configuration) pathname)
(defun conversation--lease-guard-pathname (configuration)
  "Return the shared lock serializing conversation lease-file lifecycle."
  (merge-pathnames "conversation-leases.lock"
                   (configuration-state-root configuration)))

(-> conversation--lease-in-use (string pathname pathname) null)
(defun conversation--lease-in-use
    (identifier conversation-pathname lease-pathname)
  "Signal that normalized IDENTIFIER already has a live primary owner."
  (error
   'conversation-in-use
   :message
   (format
    nil
    "Conversation ~A is already active in another Autolith process."
    (conversation-identifier-display identifier))
   :pathname conversation-pathname
   :sequence nil
   :identifier identifier
   :lease-pathname lease-pathname))

(-> conversation-lease-held-p (conversation-lease) boolean)
(defun conversation-lease-held-p (lease)
  "Return true when LEASE still owns an open lock descriptor."
  (lease-held-p (conversation-lease--lock-lease lease)))

(-> conversation-lease-matches-p (conversation-lease string) boolean)
(defun conversation-lease-matches-p (lease identifier)
  "Return true when held LEASE owns normalized IDENTIFIER."
  (and (conversation-lease-held-p lease)
       (string= (conversation-lease-identifier lease) identifier)))

(-> conversation-lease-acquire (configuration string) conversation-lease)
(defun conversation-lease-acquire (configuration identifier)
  "Acquire the primary process lease for IDENTIFIER without waiting.

The kernel lock is authoritative. Normal release removes its empty per-ID file;
a crash may leave one that a later lease acquisition can reuse safely."
  (let* ((normalized
           (conversation-identifier-migration-resolve
            configuration identifier))
         (conversation-pathname
           (conversation-pathname-for-id configuration normalized))
         (lease-pathname
           (conversation--lease-pathname configuration normalized))
         (guard-pathname
           (conversation--lease-guard-pathname configuration)))
    (handler-case
        (progn
          (ensure-directories-exist lease-pathname)
          (call-with-file-lock
           guard-pathname
           (lambda ()
             (make-instance 'conversation-lease
                            :identifier normalized
                            :lock-pathname lease-pathname
                            :guard-pathname guard-pathname
                            :lock-lease (lease-acquire lease-pathname)))))
      (file-lock-busy ()
        (conversation--lease-in-use
         normalized conversation-pathname lease-pathname))
      (conversation-error (condition)
        (error condition))
      (error (condition)
        (error
         'conversation-invariant-error
         :message
         (format nil
                 "Could not claim conversation ~A: ~A"
                 (conversation-identifier-display normalized)
                 condition)
         :pathname conversation-pathname
         :sequence nil)))))

(-> conversation-lease-release (conversation-lease) null)
(defun conversation-lease-release (lease)
  "Release LEASE idempotently and remove its empty per-ID lock file."
  (let ((lock-lease (conversation-lease--lock-lease lease)))
    (when (lease-held-p lock-lease)
      (handler-case
          (call-with-file-lock
           (conversation-lease--guard-pathname lease)
           (lambda ()
             (lease-release lock-lease)
             (ignore-errors
               (let ((pathname (conversation-lease--lock-pathname lease)))
                 (when (probe-file pathname)
                   (delete-file pathname))))))
        (error ()
          (lease-release lock-lease)))))
  nil)


;;;; -- Durable Projection --

(-> conversation--activity-after-record
    (list &key (:working-seconds (integer 0))
               (:user-turn-count (integer 0))
               (:last-activity-at (option timestamp)))
    (values (integer 0) (integer 0) (option timestamp)))
(defun conversation--activity-after-record
    (record &key (working-seconds 0) (user-turn-count 0) last-activity-at)
  "Return activity values after applying durable RECORD to a picker summary."
  (let ((time (and (consp record) (getf (rest record) :time)))
         (user-message-p
           (and (consp record)
                (eq (first record) ':message)
                (eq (getf (rest record) :role) ':user)
                (not (getf (rest record) :automatic-p)))))
    (when (typep time 'timestamp)
      (when (and last-activity-at
                 (not user-message-p)
                 (> time last-activity-at))
        (incf working-seconds (- time last-activity-at)))
      (setf last-activity-at (max (or last-activity-at 0) time)))
    (when user-message-p
      (incf user-turn-count))
    (values working-seconds user-turn-count last-activity-at)))

(-> conversation--record-preview (list) (option string))
(defun conversation--record-preview (record)
  "Return the user or assistant text represented by durable RECORD."
  (case (first record)
      (:message
       (let ((content (getf (rest record) :content)))
         (when (and (eq (getf (rest record) :role) ':user)
                    (not (getf (rest record) :automatic-p))
                    (stringp content))
           (if (non-empty-string-p content)
               content
               (and (getf (rest record) :images)
                    *conversation-image-only-title*)))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       ;; Only assistant messages yield previews, and their locally
       ;; encoded wire JSON always carries the literal role string, so
       ;; other items skip the decode entirely.
       (when (and (stringp wire-json)
                  (search "\"assistant\"" wire-json))
         (handler-case
             (let ((item (json-decode wire-json)))
               (when (json-object-p item)
                 (item-assistant-text item)))
           (error ()
             nil)))))))


(-> conversation--note-picker-search-message (conversation string) string)
(defun conversation--note-picker-search-message (conversation message)
  "Retain MESSAGE as the newest preview and count it for search staleness."
  (setf (conversation-picker-preview conversation) message)
  (incf (conversation-picker-search-message-count conversation))
  message)

(-> conversation--note-pending-input-identifier (conversation list) null)
(defun conversation--note-pending-input-identifier (conversation record)
  "Retain RECORD's durable pending-input identifier once, when present."
  (let ((identifier
          (and (eq (first record) ':message)
               (getf (rest record) :pending-input-identifier))))
    (when (and (non-empty-string-p identifier)
               (not (member identifier
                            (conversation-durable-pending-input-identifiers
                             conversation)
                            :test #'string=)))
      (setf (conversation-durable-pending-input-identifiers conversation)
            (append
             (conversation-durable-pending-input-identifiers conversation)
             (list (copy-seq identifier))))))
  nil)

(-> conversation--note-activity (conversation list) null)
(defun conversation--note-activity (conversation record)
  "Project RECORD's activity metadata into CONVERSATION."
  (multiple-value-bind (working-seconds user-turn-count last-activity-at)
      (conversation--activity-after-record
       record
       :working-seconds (conversation-working-seconds conversation)
       :user-turn-count (conversation-user-turn-count conversation)
       :last-activity-at (conversation-last-activity-at conversation))
    (setf (conversation-working-seconds conversation) working-seconds
          (conversation-user-turn-count conversation) user-turn-count
          (conversation-last-activity-at conversation) last-activity-at))
  nil)

(-> conversation--header-record
    (conversation &key (:chunk-start-sequence (integer 1)))
    list)
(defun conversation--header-record
    (conversation &key (chunk-start-sequence 1))
  "Return a self-contained portable chunk header for CONVERSATION."
  (list :conversation
        :version 2
        :id (conversation-identifier conversation)
        :created-at (conversation-created-at conversation)
        :directory (conversation-origin-directory conversation)
        :prompt-cache-key (conversation-prompt-cache-key conversation)
        :title (conversation-title conversation)
        :title-source (conversation-title-source conversation)
        :model (conversation-model conversation)
        :reasoning-effort (conversation-reasoning-effort conversation)
        :chunk-start-sequence chunk-start-sequence
        :working-seconds (conversation-working-seconds conversation)
        :user-turn-count (conversation-user-turn-count conversation)
        :last-activity-at (conversation-last-activity-at conversation)
        :picker-search-message-count
        (conversation-picker-search-message-count conversation)
        :picker-preview
        (let ((preview (conversation-picker-preview conversation)))
          (and preview (copy-seq preview)))
        :pending-input-identifiers
        (mapcar #'copy-seq
                (conversation-durable-pending-input-identifiers conversation))
        :user-operation-records
        (copy-tree (deque->list
                    (conversation-user-operation-records conversation)))
        :latest-goal-record
        (copy-tree (conversation-latest-goal-record conversation))))

(-> conversation--initial-publication-lock-pathname (conversation) pathname)
(defun conversation--initial-publication-lock-pathname (conversation)
  "Return the shared lock serializing first publication in one storage root."
  (merge-pathnames
   ".conversation-publication.lock"
   (uiop:pathname-directory-pathname (conversation-pathname conversation))))

(-> conversation--write-initial-record (conversation list) null)
(defun conversation--write-initial-record (conversation record)
  "Publish the first chunk before adopting its durable state."
  (sexp-store:segment-publish
   (conversation-log-pathname conversation)
   (conversation--header-record conversation :chunk-start-sequence 1)
   record
   :lock-pathname (conversation--initial-publication-lock-pathname conversation)
   :occupied-p (lambda ()
                 (conversation-storage-occupied-p
                  (conversation-pathname conversation))))
  (setf (conversation-persisted-p conversation) t
        (conversation-incomplete-tail-p conversation) nil)
  nil)

(-> conversation--compaction-record-p (list) boolean)
(defun conversation--compaction-record-p (record)
  "Return true when RECORD begins a new durable compaction chunk."
  (not (null (member (first record) '(:summary :native-compaction)))))

(-> conversation--write-rotated-record (conversation list) null)
(defun conversation--write-rotated-record (conversation record)
  "Publish a compaction checkpoint before adopting its active chunk."
  (let* ((sequence (getf (rest record) :seq))
         (pathname (conversation-chunk-pathname
                    (conversation-pathname conversation) sequence)))
    (sexp-store:segment-publish
     pathname
     (conversation--header-record conversation :chunk-start-sequence sequence)
     record)
    (setf (conversation-log-pathname conversation) pathname
          (conversation-incomplete-tail-p conversation) nil)
    (incf (conversation-log-generation conversation)))
  nil)


;;;; -- Conversation Picker Metadata --

(-> conversation-picker-revision-read (pathname) (integer 0))
(defun conversation-picker-revision-read (conversation-pathname)
  "Return the durable cache revision, treating malformed revisions as misses."
  (sexp-store:revision-read
   (conversation-picker-revision-pathname conversation-pathname)
   :tag ':conversation-picker-revision))

(-> conversation-picker-revision-write (pathname (integer 0)) (integer 0))
(defun conversation-picker-revision-write (conversation-pathname revision)
  "Publish the cache revision before mutating conversation data."
  (sexp-store:revision-write
   (conversation-picker-revision-pathname conversation-pathname)
   revision :tag ':conversation-picker-revision))

(-> conversation-picker-metadata-invalidate (conversation) (integer 0))
(defun conversation-picker-metadata-invalidate (conversation)
  "Advance CONVERSATION's picker revision before its durable log changes."
  (let ((pathname (conversation-pathname conversation)))
    (handler-case
        (conversation-picker-revision-write
         pathname
         (1+ (conversation-picker-revision-read pathname)))
      (error (condition)
        (error 'conversation-invariant-error
               :message (format nil
                                "Could not invalidate conversation picker metadata: ~A"
                                condition)
               :pathname pathname
               :sequence (conversation-next-sequence conversation))))))

(-> conversation--file-identity (pathname)
    (values non-empty-string (integer 0) (integer 0)))
(defun conversation--file-identity (pathname)
  "Return the active segment's pathname, byte size and write date."
  (sexp-store:file-revision
   (or (conversation-storage-active-pathname pathname) pathname)))

(-> conversation-picker-metadata-record (conversation-picker-metadata) list)
(defun conversation-picker-metadata-record (metadata)
  "Return METADATA as one portable atomically published picker-cache form."
  (list :conversation-picker-metadata
        :version 3
        :source-segment
        (conversation-picker-metadata-source-segment metadata)
        :source-size (conversation-picker-metadata-source-size metadata)
        :source-write-date
        (conversation-picker-metadata-source-write-date metadata)
        :source-revision (conversation-picker-metadata-source-revision metadata)
        :working-seconds (conversation-picker-metadata-working-seconds metadata)
        :user-turn-count (conversation-picker-metadata-user-turn-count metadata)
        :title (conversation-picker-metadata-title metadata)
        :search-message-count
        (conversation-picker-metadata-search-message-count metadata)
        :preview (conversation-picker-metadata-preview metadata)
        :incomplete-tail-p
        (conversation-picker-metadata-incomplete-tail-p metadata)))

(-> conversation-picker-metadata-from-record (t)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-from-record (record)
  "Return validated picker metadata represented by RECORD, or NIL."
  (handler-case
      (when (and (conversation--record-form-p record)
                 (eq (first record) :conversation-picker-metadata)
                 (= (or (getf (rest record) :version) 0) 3))
        (let* ((source-segment (getf (rest record) :source-segment))
               (source-size (getf (rest record) :source-size))
               (source-write-date (getf (rest record) :source-write-date))
               (source-revision (getf (rest record) :source-revision))
               (working-seconds (getf (rest record) :working-seconds))
               (user-turn-count (getf (rest record) :user-turn-count))
               (title (getf (rest record) :title))
               (normalized-title
                 (and (stringp title)
                      (conversation-title-normalize title)))
               (search-message-count (getf (rest record) :search-message-count))
               (preview (getf (rest record) :preview))
               (incomplete-tail-p (getf (rest record) :incomplete-tail-p)))
          (when (and (non-empty-string-p source-segment)
                     (typep source-size '(integer 0))
                     (typep source-write-date '(integer 0))
                     (typep source-revision '(integer 0))
                     (typep working-seconds '(integer 0))
                     (typep user-turn-count '(integer 0))
                     (or (null title) normalized-title)
                     (typep search-message-count '(integer 0))
                     (or (null preview) (stringp preview))
                     (typep incomplete-tail-p 'boolean))
            (make-instance 'conversation-picker-metadata
                           :source-segment source-segment
                           :source-size source-size
                           :source-write-date source-write-date
                           :source-revision source-revision
                           :working-seconds working-seconds
                           :user-turn-count user-turn-count
                           :title normalized-title
                           :search-message-count search-message-count
                           :preview preview
                           :incomplete-tail-p incomplete-tail-p))))
    (error ()
      nil)))

(-> conversation-picker-metadata-read (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-read (pathname)
  "Return the validated picker sidecar only when its source stamp is current."
  (sexp-store:sidecar-read
   (conversation-picker-metadata-pathname pathname)
   :decode #'conversation-picker-metadata-from-record
   :source-token (lambda () (conversation-picker-metadata-source-token pathname))
   :value-token #'conversation-picker-metadata-token))

(-> conversation-picker-metadata-source-token (pathname) list)
(defun conversation-picker-metadata-source-token (pathname)
  "Return the active segment identity together with its durable picker revision."
  (multiple-value-bind (segment size write-date) (conversation--file-identity pathname)
    (list segment size write-date (conversation-picker-revision-read pathname))))

(-> conversation-picker-metadata-token (conversation-picker-metadata) list)
(defun conversation-picker-metadata-token (metadata)
  "Return the source stamp represented by conversation picker METADATA."
  (list (conversation-picker-metadata-source-segment metadata)
        (conversation-picker-metadata-source-size metadata)
        (conversation-picker-metadata-source-write-date metadata)
        (conversation-picker-metadata-source-revision metadata)))

(-> conversation-picker-metadata-write
    (pathname conversation-picker-metadata)
    conversation-picker-metadata)
(defun conversation-picker-metadata-write (pathname metadata)
  "Publish picker METADATA only while its source stamp is current."
  (sexp-store:sidecar-write
   (conversation-picker-metadata-pathname pathname) metadata
   :encode #'conversation-picker-metadata-record
   :source-token (lambda () (conversation-picker-metadata-source-token pathname))
   :value-token #'conversation-picker-metadata-token)
  metadata)

(-> conversation-picker-metadata-publish (conversation) null)
(defun conversation-picker-metadata-publish (conversation)
  "Best-effort publish CONVERSATION's compact picker cache after a durable append."
  (ignore-errors
    (multiple-value-bind (segment size write-date)
        (conversation--file-identity (conversation-pathname conversation))
      (conversation-picker-metadata-write
       (conversation-pathname conversation)
       (make-instance 'conversation-picker-metadata
                       :source-segment       segment
                       :source-size          size
                       :source-write-date    write-date
                       :source-revision
                       (conversation-picker-revision-read
                        (conversation-pathname conversation))
                       :working-seconds
                       (conversation-working-seconds conversation)
                       :user-turn-count
                       (conversation-user-turn-count conversation)
                       :title                (conversation-title conversation)
                       :search-message-count
                       (conversation-picker-search-message-count conversation)
                       :preview              (conversation-picker-preview conversation)
                       :incomplete-tail-p
                       (conversation-incomplete-tail-p conversation)))))
  nil)


;;;; -- Conversation Picker Search --

(-> conversation-picker-search-revision-read (pathname) (integer 0))
(defun conversation-picker-search-revision-read (conversation-pathname)
  "Return the durable cache revision, treating malformed revisions as misses."
  (sexp-store:revision-read
   (conversation-picker-search-revision-pathname conversation-pathname)
   :tag ':conversation-picker-search-revision))

(-> conversation-picker-search-revision-write
    (pathname (integer 0))
    (integer 0))
(defun conversation-picker-search-revision-write (conversation-pathname revision)
  "Publish the cache revision before mutating conversation data."
  (sexp-store:revision-write
   (conversation-picker-search-revision-pathname conversation-pathname)
   revision :tag ':conversation-picker-search-revision))

(-> conversation-picker-search-invalidate (conversation) (integer 0))
(defun conversation-picker-search-invalidate (conversation)
  "Advance CONVERSATION's message-search revision before its log changes."
  (let ((pathname (conversation-pathname conversation)))
    (handler-case
        (conversation-picker-search-revision-write
         pathname
         (1+ (conversation-picker-search-revision-read pathname)))
      (error (condition)
        (error 'conversation-invariant-error
               :message
               (format nil
                       "Could not invalidate conversation picker search: ~A"
                       condition)
               :pathname pathname
               :sequence (conversation-next-sequence conversation))))))


(-> conversation-picker-search--messages-p (t) boolean)
(defun conversation-picker-search--messages-p (value)
  "Return true when VALUE is a finite proper list of strings."
  (handler-case
      (not
       (null
        (and (listp value)
             (or (null value) (list-length value))
             (every #'stringp value))))
    (type-error ()
      nil)))


(-> conversation-picker-search-index-record
    (conversation-picker-search-index)
    list)
(defun conversation-picker-search-index-record (index)
  "Return INDEX as one portable atomically published search form."
  (list :conversation-picker-search
        :version 1
        :source-revision
        (conversation-picker-search-index-source-revision index)
        :message-count
        (conversation-picker-search-index-message-count index)
        :messages
        (conversation-picker-search-index-messages index)))


(-> conversation-picker-search-index-from-record
    (t)
    (option conversation-picker-search-index))
(defun conversation-picker-search-index-from-record (record)
  "Return the validated picker search INDEX represented by RECORD, or NIL."
  (handler-case
      (when (and (listp record)
                 (eq (first record) :conversation-picker-search)
                 (= (or (getf (rest record) :version) 0) 1))
        (let ((source-revision (getf (rest record) :source-revision))
              (message-count (getf (rest record) :message-count))
              (messages (getf (rest record) :messages)))
          (when (and (typep source-revision '(integer 0))
                     (typep message-count '(integer 0))
                     (conversation-picker-search--messages-p messages)
                     (= message-count (length messages)))
            (make-instance 'conversation-picker-search-index
                           :source-revision source-revision
                           :message-count message-count
                           :messages messages))))
    (error ()
      nil)))


(-> conversation-picker-search-read
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-read (pathname)
  "Return current searchable text whose count agrees with the picker metadata."
  (let ((metadata (conversation-picker-metadata-read pathname)))
    (when metadata
      (sexp-store:sidecar-read
       (conversation-picker-search-pathname pathname)
       :decode #'conversation-picker-search-index-from-record
       :source-token (lambda () (conversation-picker-search-revision-read pathname))
       :value-token #'conversation-picker-search-index-source-revision
       :validate (lambda (index)
                   (= (conversation-picker-search-index-message-count index)
                      (conversation-picker-metadata-search-message-count metadata)))))))

(-> conversation-picker-search-write
    (pathname conversation-picker-search-index)
    conversation-picker-search-index)
(defun conversation-picker-search-write (pathname index)
  "Publish searchable text only while its source revision is current."
  (sexp-store:sidecar-write
   (conversation-picker-search-pathname pathname) index
   :encode #'conversation-picker-search-index-record
   :source-token (lambda () (conversation-picker-search-revision-read pathname))
   :value-token #'conversation-picker-search-index-source-revision)
  index)

(-> conversation-create
    (configuration &key (:identifier (option string))
                        (:prompt-cache-key (option string))
                        (:storage-root (option pathname))
                        (:created-at (option timestamp)))
    conversation)
(defun conversation-create
    (configuration &key identifier prompt-cache-key storage-root created-at)
  "Create an in-memory conversation that persists under optional STORAGE-ROOT."
  (let* ((created-at (or created-at (get-universal-time)))
         (root (uiop:ensure-directory-pathname
                (or storage-root
                    (configuration-conversation-root configuration))))
         (conversation-id
           (conversation-identifier-validate-path-component
            (or identifier
                (conversation-identifier-generate root :timestamp created-at))))
         (origin-directory (namestring
                            (configuration-working-directory configuration)))
         (identity (merge-pathnames
                    (make-pathname :name conversation-id :type "sexp")
                    root))
         (log-pathname (conversation-chunk-pathname identity 1)))
    (when (conversation-storage-occupied-p identity)
      (error 'conversation-error
             :message (format nil "Conversation ~A already exists." conversation-id)
             :pathname identity
             :sequence nil))
    (make-instance 'conversation
                   :identifier conversation-id
                   :prompt-cache-key prompt-cache-key
                   :pathname identity
                   :log-pathname log-pathname
                   :persisted-p nil
                   :incomplete-tail-p nil
                   :created-at created-at
                   :origin-directory origin-directory
                   :model (configuration-model configuration)
                   :reasoning-effort
                   (configuration-reasoning-effort configuration)
                   :next-sequence 1
                   :input-items nil)))

(-> conversation--repair-incomplete-tail (conversation) null)
(defun conversation--repair-incomplete-tail (conversation)
  "Repair the active torn tail while interrupts remain enabled."
  (handler-case
      (progn
        (when (sexp-store:log-repair-tail (conversation-log-pathname conversation))
          (incf (conversation-log-generation conversation)))
        (setf (conversation-incomplete-tail-p conversation) nil))
    (error (condition)
      (error 'conversation-invariant-error
             :message (format nil "Could not repair incomplete conversation tail: ~A"
                              condition)
             :pathname (conversation-log-pathname conversation)
             :sequence (conversation-next-sequence conversation))))
  nil)

(-> conversation-append-record (conversation list) list)
(defgeneric conversation-append-record (conversation record)
  (:documentation "Append portable RECORD to CONVERSATION and return the sequenced form."))

(defmethod conversation-append-record :around ((conversation conversation) (record list))
  "Exclude picker reconstruction across invalidation, log writes and publication."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda () (call-next-method)))))

(defmethod conversation-append-record ((conversation conversation) (record list))
  "Assign metadata, initialize persistence if needed, and append RECORD."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (unless (keywordp (first record))
      (error 'conversation-invariant-error
             :message "A conversation record must begin with a keyword."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (when (and (conversation-persisted-p conversation)
               (conversation-incomplete-tail-p conversation))
      (conversation--repair-incomplete-tail conversation))
    (let* ((sequence (conversation-next-sequence conversation))
           (sequenced (list* (first record)
                             :seq sequence
                             :time (get-universal-time)
                             (rest record)))
           (picker-search-message (conversation--record-preview sequenced)))
      ;; Invalidate under the shared source lock before changing durable bytes.
      ;; Cache reconstruction holds this same lock through publication.
      (when picker-search-message
        (conversation-picker-search-invalidate conversation))
      (conversation-picker-metadata-invalidate conversation)
      (sb-sys:without-interrupts
        (handler-case
            (if (conversation-persisted-p conversation)
                (let ((pathname (conversation-log-pathname conversation)))
                  (unless (probe-file pathname)
                    (error 'conversation-invariant-error
                           :message "The active persisted conversation log is missing."
                           :pathname pathname
                           :sequence sequence))
                  (if (conversation--compaction-record-p sequenced)
                      (conversation--write-rotated-record conversation sequenced)
                      (progn
                        (log-append pathname sequenced :repair-tail-p nil)
                        (setf (conversation-incomplete-tail-p conversation) nil))))
                (conversation--write-initial-record conversation sequenced))
          (error (condition)
            (when (and (not (conversation-persisted-p conversation))
                       (not (conversation-storage-occupied-p
                             (conversation-pathname conversation))))
              (ignore-errors
                (conversation-picker-sidecars-delete
                 (conversation-pathname conversation))))
            (error 'conversation-invariant-error
                   :message
                   (format nil
                           "Could not append conversation record: ~A"
                           condition)
                   :pathname (conversation-pathname conversation)
                   :sequence sequence)))
        (incf (conversation-next-sequence conversation))
        (conversation--note-activity conversation sequenced)
        (conversation--note-pending-input-identifier conversation sequenced)
        (when picker-search-message
          (conversation--note-picker-search-message
           conversation picker-search-message))
        (when (eq (first sequenced) :goal)
          (setf (conversation-latest-goal-record conversation) sequenced)))
      (conversation-picker-metadata-publish conversation)
      sequenced)))

(-> conversation--bounded-turn-abort-text (t string integer) non-empty-string)
(defun conversation--bounded-turn-abort-text (value field maximum-characters)
  "Return bounded nonempty string VALUE for aborted-turn FIELD."
  (unless (non-empty-string-p value)
    (error 'conversation-invariant-error
           :message (format nil "Turn-aborted ~A must be a non-empty string." field)
           :pathname #P"conversation.sexp"
           :sequence nil))
  (if (<= (length value) maximum-characters)
      value
      (subseq value 0 maximum-characters)))

(-> conversation-append-turn-aborted
    (conversation
     &key (:turn-start-sequence (integer 1))
          (:reason (member :cancelled :agent-loop :application-error))
          (:condition-type string)
          (:message string)
          (:request-number (option (integer 1))))
    (option list))
(defun conversation-append-turn-aborted
    (conversation &key turn-start-sequence reason condition-type message
                       request-number)
  "Append one idempotent aborted-turn boundary when its turn has durable work."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((last-complete-sequence (1- (conversation-next-sequence conversation))))
      (when (or (not (conversation-persisted-p conversation))
                (< last-complete-sequence turn-start-sequence))
        (return-from conversation-append-turn-aborted nil))
      (when (eql turn-start-sequence
                 (conversation-last-aborted-turn-start-sequence conversation))
        (return-from conversation-append-turn-aborted nil))
      (unless (member reason '(:cancelled :agent-loop :application-error))
        (error 'conversation-invariant-error
               :message "A turn-aborted record has an unsupported reason."
               :pathname (conversation-pathname conversation)
               :sequence (conversation-next-sequence conversation)))
      (unless (or (null request-number)
                  (typep request-number '(integer 1)))
        (error 'conversation-invariant-error
               :message "A turn-aborted request number must be positive."
               :pathname (conversation-pathname conversation)
               :sequence (conversation-next-sequence conversation)))
      (let ((record
              (conversation-append-record
               conversation
               (list :turn-aborted
                     :turn-start-seq turn-start-sequence
                     :last-complete-seq last-complete-sequence
                     :reason reason
                     :condition-type
                     (conversation--bounded-turn-abort-text
                      condition-type
                      "condition type"
                      *conversation-turn-aborted-condition-type-maximum-characters*)
                     :message
                     (conversation--bounded-turn-abort-text
                      message
                      "message"
                      *conversation-turn-aborted-message-maximum-characters*)
                     :request-number request-number))))
        (setf (conversation-last-aborted-turn-start-sequence conversation)
              turn-start-sequence)
        record))))

(-> conversation-title-refresh-due-p (conversation) boolean)
(defun conversation-title-refresh-due-p (conversation)
  "Return true when CONVERSATION needs its one automatic generated title."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (and (conversation-persisted-p conversation)
         (>= (conversation-user-turn-count conversation)
             *conversation-title-refresh-turn-count*)
         (not (eq (conversation-title-source conversation) ':generated))
         t)))

(-> conversation-title-refresh-reserve-p (conversation) boolean)
(defun conversation-title-refresh-reserve-p (conversation)
  "Atomically reserve CONVERSATION's one automatic generated-title request."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (and (conversation-title-refresh-due-p conversation)
         (not (conversation-title-refresh-in-progress-p conversation))
         (progn
           (setf (conversation-title-refresh-in-progress-p conversation) t)
           t))))

(-> conversation-title-refresh-release (conversation) null)
(defun conversation-title-refresh-release (conversation)
  "Release CONVERSATION's automatic generated-title request reservation."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (setf (conversation-title-refresh-in-progress-p conversation) nil))
  nil)

(-> conversation--published-title-record
    (conversation (integer 1) non-empty-string keyword)
    (option list))
(defun conversation--published-title-record
    (conversation sequence title source)
  "Return the expected final title record when it was durably published."
  (handler-case
      (multiple-value-bind (forms incomplete-tail-p)
          (log-read (conversation-log-pathname conversation))
        (let ((record (and forms (first (last forms)))))
          (and (not incomplete-tail-p)
               (listp record)
               (eq (first record) ':title)
               (= (or (getf (rest record) :seq) 0) sequence)
               (string= (or (getf (rest record) :value) "") title)
               (eq (getf (rest record) :source) source)
               record)))
    (error ()
      nil)))

(-> conversation--title-transition-valid-p (t keyword) boolean)
(defun conversation--title-transition-valid-p (current-source next-source)
  "Return true when NEXT-SOURCE may durably follow CURRENT-SOURCE."
  (or (and (null current-source)
           (member next-source '(:initial :generated))
           t)
      (and (eq current-source ':initial)
           (eq next-source ':generated)
           t)))

(-> conversation-set-title
    (conversation string &key (:source keyword))
    non-empty-string)
(defun conversation-set-title (conversation title &key (source ':generated))
  "Normalize and durably replace CONVERSATION's title from SOURCE."
  (let ((normalized (conversation-title-normalize title)))
    (unless normalized
      (error 'conversation-invariant-error
             :message "A conversation title must contain displayable text."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (unless (member source '(:initial :generated))
      (error 'conversation-invariant-error
             :message "A conversation title has an unsupported source."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (let ((current-title (conversation-title conversation))
            (current-source (conversation-title-source conversation)))
        (when (and (eq source ':generated)
                   (eq current-source ':generated))
          (return-from conversation-set-title current-title))
        (when (and (string= normalized (or current-title ""))
                   (eq source current-source))
          (return-from conversation-set-title normalized))
        (unless (conversation--title-transition-valid-p current-source source)
          (error 'conversation-invariant-error
                 :message "A conversation title has an invalid source transition."
                 :pathname (conversation-pathname conversation)
                 :sequence (conversation-next-sequence conversation)))
        (let ((sequence (conversation-next-sequence conversation)))
          (setf (conversation-title conversation) normalized
                (conversation-title-source conversation) source)
          (handler-case
              (conversation-append-record
               conversation
               (list :title :value normalized :source source))
            (error (condition)
              (let ((published-record
                      (conversation--published-title-record
                       conversation sequence normalized source)))
                (if published-record
                    (progn
                      (setf (conversation-next-sequence conversation) (1+ sequence)
                            (conversation-incomplete-tail-p conversation) nil)
                      (conversation--note-activity conversation published-record)
                      (ignore-errors
                        (call-with-file-lock
                         (conversation-picker-source-lock-pathname
                          (conversation-pathname conversation))
                         (lambda () (conversation-picker-metadata-publish conversation)))))
                    (progn
                      (setf (conversation-title conversation) current-title
                            (conversation-title-source conversation) current-source)
                      (error condition)))))))))
    normalized))

(-> conversation--append-input-item (conversation json-object) json-object)
(defun conversation--append-input-item (conversation item)
  "Append ITEM and attribute it to CONVERSATION's current model family."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((model (conversation-model conversation)))
      (when (non-empty-string-p model)
        (setf (gethash item (conversation-input-item-families conversation))
              (model-family model)))
      (clinker-transcript:projection-append (conversation-projection conversation) item))))

(-> conversation--append-ephemeral-input-item
    (conversation json-object &key (:attachments list))
    json-object)
(defun conversation--append-ephemeral-input-item
    (conversation item &key attachments)
  "Append request-local ITEM and record any owned ATTACHMENTS for cleanup."
  (let ((entries
          (append
           (conversation-ephemeral-input-entries conversation)
           (list (list :item item :attachments attachments)))))
    ;; Publish ownership before mutating the provider projection. An interrupt
    ;; after the projection append can then never leave an untagged item.
    (setf (conversation-ephemeral-input-entries conversation) entries)
    (conversation--append-input-item conversation item))
  item)

(-> conversation-input-items-for-request
    (conversation &key (:include-ephemeral-p boolean))
    list)
(defun conversation-input-items-for-request
    (conversation &key (include-ephemeral-p t))
  "Return a fresh provider projection, optionally excluding request-local items."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (if include-ephemeral-p
        (copy-list (conversation-input-items conversation))
        (let ((ephemeral-items (make-hash-table :test #'eq)))
          (dolist (entry (conversation-ephemeral-input-entries conversation))
            (setf (gethash (getf entry :item) ephemeral-items) t))
          (remove-if
           (lambda (item)
             (gethash item ephemeral-items))
           (conversation-input-items conversation))))))

(-> conversation-input-item-family (conversation json-object) (option keyword))
(defun conversation-input-item-family (conversation item)
  "Return the model family that produced ITEM, or NIL when it is unknown."
  (values (gethash item (conversation-input-item-families conversation))))

(-> conversation-input-items-for-family
    (conversation keyword &key (:include-ephemeral-p boolean))
    list)
(defun conversation-input-items-for-family
    (conversation family &key (include-ephemeral-p t))
  "Return CONVERSATION's provider projection usable by FAMILY."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (clinker-transcript:items-for-family
     (conversation-input-items-for-request
      conversation :include-ephemeral-p include-ephemeral-p)
     family
     :item-families (conversation-input-item-families conversation)
     :handoff-families (conversation-portable-handoff-families conversation))))

(-> conversation-native-compaction-summary-view
    (conversation json-object keyword)
    conversation)
(defun conversation-native-compaction-summary-view (conversation item family)
  "Return a transient request view containing native compaction ITEM only."
  (let ((view
          (make-instance
           'conversation
           :identifier (conversation-identifier conversation)
           :prompt-cache-key (conversation-prompt-cache-key conversation)
           :pathname (conversation-pathname conversation)
           :log-pathname (conversation-log-pathname conversation)
           :persisted-p nil
           :created-at (conversation-created-at conversation)
           :origin-directory (conversation-origin-directory conversation)
           :next-sequence (conversation-next-sequence conversation)
           :input-items (list item))))
    (setf (gethash item (conversation-input-item-families view)) family)
    view))

(defparameter *conversation-inherited-reference-boundary*
    (concatenate
     'string
     "Everything before this message is inherited reference context from the "
     "parent task. It is not your current assignment. Use it only as background, "
     "then follow the child role instructions and the user assignment that follows.")
  "The developer boundary separating inherited parent history from child work.")

(-> conversation--inherited-reference-text-part (t) (option json-object))
(defun conversation--inherited-reference-text-part (part)
  "Return a detached portable text PART, or NIL for non-text content."
  (when (json-object-p part)
    (let ((type (json-get part "type"))
          (text (json-get part "text")))
      (when (and (stringp type)
                 (member type '("input_text" "output_text" "text")
                         :test #'string=)
                 (stringp text))
        (json-object "type" type "text" text)))))

(-> conversation--inherited-reference-message (t) (option json-object))
(defun conversation--inherited-reference-message (item)
  "Return ITEM's safe user or final-assistant reference message, or NIL."
  (when (and (json-object-p item)
             (string= (or (json-get item "type") "") "message"))
    (let ((role (json-get item "role"))
          (phase (json-get item "phase"))
          (content (json-get item "content")))
      (when (and (stringp role)
                 (member role '("user" "assistant") :test #'string=)
                 (or (not (string= role "assistant"))
                     (null phase)
                     (and (stringp phase) (string= phase "final_answer")))
                 (vectorp content))
        (let ((parts
                (loop for part across content
                      for copy = (conversation--inherited-reference-text-part part)
                      when copy collect copy)))
          (when parts
            (json-object "type" "message"
                         "role" role
                         "content" (coerce parts 'vector))))))))

(-> conversation--inherited-reference-boundary-item () json-object)
(defun conversation--inherited-reference-boundary-item ()
  "Return the durable developer boundary following inherited parent history."
  (json-object
   "type" "message"
   "role" "developer"
   "content"
   (json-array
    (json-object "type" "input_text"
                 "text" *conversation-inherited-reference-boundary*))))

(-> conversation--inherited-reference-boundary-p (t) boolean)
(defun conversation--inherited-reference-boundary-p (item)
  "Return true when ITEM is the exact inherited-reference developer boundary."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "message")
       (string= (or (json-get item "role") "") "developer")
       (let ((content (json-get item "content")))
         (and (vectorp content)
              (= (length content) 1)
              (let ((part (aref content 0)))
                (and (json-object-p part)
                     (string= (or (json-get part "type") "") "input_text")
                     (string= (or (json-get part "text") "")
                              *conversation-inherited-reference-boundary*)))))
       t))

(-> conversation--inherited-reference-wire-byte-length (list) (integer 0))
(defun conversation--inherited-reference-wire-byte-length (messages)
  "Return the UTF-8 wire bytes for MESSAGES and their developer boundary."
  (length
   (sb-ext:string-to-octets
    (json-encode
     (coerce
      (append messages
              (list (conversation--inherited-reference-boundary-item)))
      'vector))
    :external-format ':utf-8)))

(-> conversation-inherited-reference-snapshot
    (conversation (integer 1))
    list)
(defun conversation-inherited-reference-snapshot
    (conversation maximum-wire-bytes)
  "Return a bounded detached snapshot of CONVERSATION's reference messages.

The snapshot excludes request-local items, reasoning, tool traffic, images,
intermediate assistant phases, and developer policy. Compaction bridge messages
remain as ordinary portable user text. Whole newest messages are retained in
wire order while the snapshot plus its developer boundary fits within
MAXIMUM-WIRE-BYTES. Only candidate messages within the retained suffix are
copied."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((items (coerce (conversation-input-items conversation) 'vector))
          (ephemeral-items (make-hash-table :test #'eq))
          (selected nil)
          (wire-bytes
            (conversation--inherited-reference-wire-byte-length nil)))
      (dolist (entry (conversation-ephemeral-input-entries conversation))
        (setf (gethash (getf entry :item) ephemeral-items) t))
      (loop for index downfrom (1- (length items)) to 0
            for item = (aref items index)
            unless (gethash item ephemeral-items)
              do (let ((message
                         (conversation--inherited-reference-message item)))
                   (when message
                     (let ((candidate-bytes
                             (+ wire-bytes
                                1
                                (length
                                 (sb-ext:string-to-octets
                                  (json-encode message)
                                  :external-format ':utf-8)))))
                       (if (<= candidate-bytes maximum-wire-bytes)
                           (setf wire-bytes candidate-bytes
                                 selected (cons message selected))
                           (return))))))
      selected)))

(-> conversation-append-inherited-reference
    (conversation non-empty-string list)
    list)
(defun conversation-append-inherited-reference
    (conversation source-conversation-identifier items)
  "Persist and project spawn-time parent reference ITEMS into CONVERSATION."
  (let* ((messages
           (loop for item in items
                 for message = (conversation--inherited-reference-message item)
                 when message collect message))
         (projection
           (append messages
                   (list (conversation--inherited-reference-boundary-item))))
         (record
           (conversation-append-record
            conversation
            (list :inherited-reference
                  :source-conversation-id source-conversation-identifier
                  :wire-json (json-encode (coerce projection 'vector))))))
    (dolist (item projection)
      (conversation--append-input-item conversation item))
    record))

(-> conversation-clear-ephemeral-input-items (conversation) null)
(defun conversation-clear-ephemeral-input-items (conversation)
  "Remove all request-local provider items and their owned image artifacts."
  (let ((entries (conversation-ephemeral-input-entries conversation)))
    (when entries
      (let ((ephemeral-items (make-hash-table :test #'eq)))
        (dolist (entry entries)
          (setf (gethash (getf entry :item) ephemeral-items) t))
        (setf (conversation-input-items conversation)
              (remove-if
               (lambda (item)
                 (gethash item ephemeral-items))
               (conversation-input-items conversation))
              (conversation-ephemeral-input-entries conversation) nil))
      (dolist (entry entries)
        (dolist (attachment (getf entry :attachments))
          (ignore-errors
            (when (probe-file (image-attachment-pathname attachment))
              (delete-file (image-attachment-pathname attachment))))))))
  nil)

(-> conversation-image-artifact-root (conversation) pathname)
(defun conversation-image-artifact-root (conversation)
  "Return CONVERSATION's private binary image artifact directory."
  (let* ((conversation-root
           (uiop:pathname-directory-pathname
            (conversation-pathname conversation)))
         (data-root (uiop:pathname-parent-directory-pathname conversation-root)))
    (merge-pathnames
     (format nil "conversation-images/~A/"
             (conversation-identifier conversation))
     data-root)))

(-> conversation--remember-image-attachment
    (conversation image-attachment)
    image-attachment)
(defun conversation--remember-image-attachment (conversation attachment)
  "Record ATTACHMENT as durably referenced by CONVERSATION."
  (setf (gethash (file-namestring (image-attachment-pathname attachment))
                 (conversation-image-artifact-names conversation))
        t)
  attachment)

(-> conversation--remember-image-attachments (conversation list) list)
(defun conversation--remember-image-attachments (conversation attachments)
  "Record ATTACHMENTS as durably referenced by CONVERSATION."
  (dolist (attachment attachments)
    (conversation--remember-image-attachment conversation attachment))
  attachments)

(-> conversation--image-artifact-pathnames (pathname) list)
(defun conversation--image-artifact-pathnames (root)
  "Return every prepared or temporary image artifact immediately below ROOT."
  (when (uiop:directory-exists-p root)
    (remove-duplicates
     (mapcan
      (lambda (pattern)
        (uiop:directory-files root pattern))
      '("*.png" "*.jpg" "*.webp" "*.tmp"))
     :test #'equal)))

(-> conversation--prune-unreferenced-image-artifacts (conversation) null)
(defun conversation--prune-unreferenced-image-artifacts (conversation)
  "Delete image artifacts not referenced by CONVERSATION's durable records."
  (let ((references (conversation-image-artifact-names conversation)))
    (dolist (pathname
             (conversation--image-artifact-pathnames
              (conversation-image-artifact-root conversation)))
      (unless (gethash (file-namestring pathname) references)
        (delete-file pathname))))
  nil)

(-> user-message-item (string &optional list) json-object)
(defun user-message-item (content &optional attachments)
  "Return a Responses API user message containing CONTENT and ATTACHMENTS."
  (clinker-transcript:user-message-item
   content
   (loop for attachment in attachments
         for label-number from 1
         append (image-input-content-items attachment label-number))))

(-> conversation--prepare-images (conversation list) list)
(defun conversation--prepare-images (conversation image-pathnames)
  "Prepare IMAGE-PATHNAMES transactionally for CONVERSATION."
  (let ((attachments nil))
    (handler-case
        (progn
          (dolist (pathname image-pathnames)
            (push (image-input-prepare
                   pathname
                   (conversation-image-artifact-root conversation))
                  attachments))
          (nreverse attachments))
      (error (condition)
        (dolist (attachment attachments)
          (when (probe-file (image-attachment-pathname attachment))
            (delete-file (image-attachment-pathname attachment))))
        (error condition)))))

(-> conversation--delete-image-attachments (list) null)
(defun conversation--delete-image-attachments (attachments)
  "Delete newly prepared ATTACHMENTS after a failed durable append."
  (dolist (attachment attachments)
    (when (probe-file (image-attachment-pathname attachment))
      (delete-file (image-attachment-pathname attachment))))
  nil)

(-> conversation-append-user-message
    (conversation (or string user-message-input)
     &key (:pending-input-identifier (option non-empty-string))
          (:automatic-p boolean))
    (values json-object list))
(defun conversation-append-user-message
    (conversation input &key pending-input-identifier automatic-p)
  "Persist user INPUT and return its provider item and sequenced record."
  (when (and (stringp input)
             (not (non-empty-string-p input)))
    (error 'configuration-error
           :message "A user submission requires text or an image."))
  (let* ((content (user-message-input-text input))
         (attachments
           (conversation--prepare-images
            conversation
            (user-message-input-image-pathnames input)))
         (item (user-message-item content attachments)))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (let* ((initial-title
               (and (not automatic-p)
                    (zerop (conversation-user-turn-count conversation))
                    (null (conversation-title conversation))
                    (conversation-title-derive
                     (if (non-empty-string-p content)
                         content
                         *conversation-image-only-title*))))
             (previous-title (conversation-title conversation))
             (previous-title-source (conversation-title-source conversation))
             (record nil)
             (durable-p nil))
        (when initial-title
          (setf (conversation-title conversation) initial-title
                (conversation-title-source conversation) ':initial))
        (unwind-protect
             (progn
               (setf record
                     (conversation-append-record
                      conversation
                      (append
                       (list :message
                             :role :user
                             :content content)
                       (when automatic-p
                         (list :automatic-p t))
                       (when pending-input-identifier
                         (list :pending-input-identifier
                               (copy-seq pending-input-identifier)))
                       (when attachments
                         (list :images
                               (mapcar #'image-attachment-record attachments)))
                       (unless attachments
                         (list :wire-json (json-encode item))))))
                (conversation--remember-image-attachments conversation attachments)
               (setf durable-p t
                     (conversation-turn-state conversation) nil)
               (values (conversation--append-input-item conversation item)
                       record))
          (unless durable-p
            (setf (conversation-title conversation) previous-title
                  (conversation-title-source conversation) previous-title-source)
            (conversation--delete-image-attachments attachments)))))))

(-> conversation--validate-provider-item (conversation json-object) json-object)
(defun conversation--validate-provider-item (conversation item)
  "Return ITEM after rejecting provider calls that would poison durable replay."
  (when (function-call-item-p item)
    (handler-case
        (clinker-transcript:validate-function-call
         item :preceding-items (conversation-input-items conversation))
      (clinker-transcript:reconciliation-error (condition)
        (error 'conversation-invariant-error
               :message (format nil "Invalid provider function call: ~A" condition)
               :pathname (conversation-pathname conversation)
               :sequence (conversation-next-sequence conversation))))
    (unless (json-object-source-p (json-get item "arguments"))
      (error 'conversation-invariant-error
             :message
             "A provider function call has arguments that are not exactly one JSON object."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation))))
  item)

(-> conversation-append-provider-item
    (conversation json-object
     &key (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-provider-item
    (conversation item &key (persistence ':durable))
  "Append one authoritative provider ITEM with the requested PERSISTENCE."
  (conversation--validate-provider-item conversation item)
  (ecase persistence
    (:durable
     (conversation-append-record
      conversation
      (list :provider-item
            :wire-json (json-encode item)))
     (conversation--append-input-item conversation item))
    (:next-response
     (conversation--append-ephemeral-input-item conversation item))))

(-> conversation--tool-content-output (list) vector)
(defun conversation--tool-content-output (blocks)
  "Return ordered string and image BLOCKS as native tool-output content."
  (coerce
   (mapcar
    (lambda (block)
      (etypecase block
        (string
         (json-object "type" "input_text" "text" block))
        (image-attachment
         (image-input-content-item block))))
    blocks)
   'vector))

(-> conversation--tool-content-block-record (t) list)
(defun conversation--tool-content-block-record (block)
  "Return one portable durable descriptor for provider content BLOCK."
  (etypecase block
    (string
     (list :text block))
    (image-attachment
     (list :image (image-attachment-record block)))))

(-> conversation--tool-content-images (list) list)
(defun conversation--tool-content-images (blocks)
  "Return every image attachment in ordered provider BLOCKS."
  (remove-if-not
   (lambda (block)
     (typep block 'image-attachment))
   blocks))

(defparameter *conversation-interrupted-tool-output*
  "Autolith interrupted this tool call before recording its result. The call may have changed external state. Inspect the relevant state before deciding whether to retry it."
  "The provider-visible result synthesized for a tool call with an unknown outcome.")

(-> conversation-append-tool-result
    (conversation string
     &key (:tool-name string)
          (:output string)
          (:image-attachments list)
          (:content-blocks list)
          (:success-p boolean)
          (:category (member :success :failure :neutral :mechanics))
          (:cpu-microseconds (option (integer 0)))
          (:real-microseconds (option (integer 0)))
          (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-tool-result
    (conversation call-id
     &key tool-name output image-attachments content-blocks success-p
       (category (if success-p ':success ':failure))
       cpu-microseconds real-microseconds (persistence ':durable))
  "Append one categorized tool OUTPUT, optional content, timing, and PERSISTENCE."
  (when (and image-attachments content-blocks)
    (error 'conversation-invariant-error
           :message
           "Tool output cannot provide both image attachments and content blocks."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let* ((blocks
           (or content-blocks
               (when image-attachments
                 (append
                  (when (non-empty-string-p output)
                    (list output))
                  image-attachments))))
         (attachments (conversation--tool-content-images blocks))
         (retained-p nil))
    (unwind-protect
         (progn
           (unless (or (and (null cpu-microseconds)
                            (null real-microseconds))
                       (and (typep cpu-microseconds '(integer 0))
                            (typep real-microseconds '(integer 0))))
             (error 'conversation-invariant-error
                    :message
                    "Tool timing must contain both nonnegative microsecond values."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (unless (every
                    (lambda (block)
                      (or (stringp block)
                          (typep block 'image-attachment)))
                    blocks)
             (error 'conversation-invariant-error
                    :message "Tool output contains an invalid content block."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (when (and attachments (not success-p))
             (error 'conversation-invariant-error
                    :message "A failed tool result cannot contain image output."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (let* ((wire-output
                    (if attachments
                        (conversation--tool-content-output blocks)
                        output))
                  (item (function-call-output-item call-id wire-output)))
             (ecase persistence
               (:durable
                (conversation-append-record
                 conversation
                 (append
                   (list :tool-result
                         :call-id call-id
                         :tool tool-name
                         :status (ecase category
                                   (:success ':ok)
                                   (:failure ':error)
                                   (:neutral ':neutral)
                                   (:mechanics ':mechanics))
                         :category category
                         :output output)
                  (when attachments
                    (list
                     :content-blocks
                     (mapcar #'conversation--tool-content-block-record blocks)))
                  (when cpu-microseconds
                    (list :cpu-microseconds cpu-microseconds
                          :real-microseconds real-microseconds))
                  (unless attachments
                    (list :wire-json (json-encode item)))))
                 (conversation--remember-image-attachments conversation attachments)
                (setf retained-p t)
                (conversation--append-input-item conversation item))
               (:next-response
                (conversation--append-ephemeral-input-item
                 conversation
                 item
                 :attachments attachments)
                (setf retained-p t)))
             item))
      (unless retained-p
        (conversation--delete-image-attachments attachments)))))

(-> conversation--tool-call-name (json-object) string)
(defun conversation--tool-call-name (item)
  "Return a readable canonical name for function call ITEM."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (cond
      ((and (non-empty-string-p namespace) (non-empty-string-p name))
       (format nil "~A.~A" namespace name))
      ((non-empty-string-p name)
       name)
      ((non-empty-string-p namespace)
       namespace)
      (t
       "unknown"))))

(-> conversation--repair-incomplete-tool-calls (conversation) null)
(defun conversation--repair-incomplete-tool-calls (conversation)
  "Publish missing-output repairs before installing the reconciled projection."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (handler-case
        (let ((plan
                (clinker-transcript:reconcile-items
                 (conversation-input-items conversation)
                 :repaired-output-p
                 (lambda (output)
                   (equal (json-get output "output")
                          *conversation-interrupted-tool-output*)))))
          (setf (conversation-input-items conversation)
                (clinker-transcript:reconciliation-items
                 plan :repair-output
                 (lambda (intent)
                   (conversation-append-tool-result
                    conversation
                    (clinker-transcript:missing-output-repair-call-id intent)
                    :tool-name
                    (conversation--tool-call-name
                     (clinker-transcript:missing-output-repair-call intent))
                    :output *conversation-interrupted-tool-output*
                    :success-p nil)))))
      (clinker-transcript:reconciliation-error (condition)
        (error 'conversation-invariant-error
               :message
               (format nil "Persisted tool history is invalid (~A)~@[ for call ~S~]."
                       (clinker-transcript:projection-error-reason condition)
                       (clinker-transcript:reconciliation-error-call-id condition))
               :pathname (conversation-pathname conversation)
               :sequence nil))))
  nil)

(-> conversation--usage-total (t) (option integer))
(defun conversation--usage-total (usage)
  "Return the total token count carried by portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (let ((total (json-get usage "total_tokens")))
       (and (integerp total) total)))
    ((listp usage)
     (let ((total (second (assoc "total_tokens" usage :test #'equal))))
       (and (integerp total) total)))
    (t
     nil)))

(-> conversation-append-provider-metadata (conversation list) list)
(defun conversation-append-provider-metadata (conversation metadata)
  "Persist portable provider METADATA that is not part of request history."
  (let ((total (conversation--usage-total (getf metadata :usage))))
    (when total
      (setf (conversation-last-total-tokens conversation) total)))
  (conversation-append-record
   conversation
   (list :provider :metadata metadata)))

(-> conversation--model-selection-p (t t) boolean)
(defun conversation--model-selection-p (model reasoning-effort)
  "Return true when MODEL and REASONING-EFFORT form a restorable selection."
  (and (non-empty-string-p model)
       (non-empty-string-p reasoning-effort)
       (not
        (null
         (member reasoning-effort
                 *supported-reasoning-efforts*
                 :test #'string=)))))

(-> conversation--persisted-model-selection-p (t t) boolean)
(defun conversation--persisted-model-selection-p (model reasoning-effort)
  "Return true when MODEL and REASONING-EFFORT are a possible stored selection."
  (and (non-empty-string-p model)
       (non-empty-string-p reasoning-effort)))

(-> conversation-set-model-selection (conversation string string) null)
(defun conversation-set-model-selection (conversation model reasoning-effort)
  "Remember MODEL and REASONING-EFFORT without persisting an empty conversation."
  (unless (conversation--model-selection-p model reasoning-effort)
    (error 'conversation-invariant-error
           :message "A conversation model selection is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (unless (and (string= model (or (conversation-model conversation) ""))
               (string= reasoning-effort
                        (or (conversation-reasoning-effort conversation) "")))
    (when (conversation-persisted-p conversation)
      (conversation-append-record
       conversation
       (list :configuration
             :model model
             :reasoning-effort reasoning-effort)))
    (setf (conversation-model conversation) model
          (conversation-reasoning-effort conversation) reasoning-effort))
  nil)

(defparameter *conversation-summary-prefix*
  "A previous segment of this conversation was compacted. The summary below replaces that segment; use it to continue seamlessly without repeating completed work."
  "The bridge text introducing a compaction summary to the model.")

(-> conversation-summary-item (string) json-object)
(defun conversation-summary-item (content)
  "Return the replayable wire item carrying a compaction summary CONTENT."
  (json-object
   "type" "message"
   "role" "user"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" (format nil "~A~2%~A"
                              *conversation-summary-prefix*
                              content)))))

(-> conversation-append-summary (conversation string) list)
(defun conversation-append-summary (conversation content)
  "Persist a compaction summary and replace CONVERSATION's projection with it.

The durable record covers every record before it, so replay reproduces the
same compacted projection. The provider turn-state token is dropped because
it described the uncompacted context."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((ephemeral-items
             (mapcar
              (lambda (entry)
                (getf entry :item))
              (conversation-ephemeral-input-entries conversation)))
           (record
             (conversation-append-record
              conversation
              (list :summary
                    :through-seq (1- (conversation-next-sequence conversation))
                    :content content))))
      (setf (conversation-input-items conversation)
            (cons (conversation-summary-item content) ephemeral-items)
            (conversation-turn-state conversation) nil
            (conversation-last-total-tokens conversation) 0)
      record)))

(-> conversation-append-native-compaction
    (conversation json-object &key (:family keyword) (:summary string))
    list)
(defun conversation-append-native-compaction
    (conversation item &key family summary)
  "Persist opaque native ITEM and portable SUMMARY as one compaction checkpoint.

ITEM retains private model context for FAMILY. SUMMARY is deliberately kept
alongside it so a later provider family can continue from a readable handoff.
The producing family receives only ITEM. Both replace every preceding durable
provider item while pending request-local items remain available for the next
ordinary request."
  (native-compaction-item-canonicalize item)
  (unless (and (keywordp family)
               (native-compaction-item-p item)
               (non-empty-string-p summary))
    (error 'conversation-invariant-error
           :message "A native compaction checkpoint is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((ephemeral-items
             (mapcar
              (lambda (entry)
                (getf entry :item))
              (conversation-ephemeral-input-entries conversation)))
           (summary-item (conversation-summary-item summary))
           (record
             (conversation-append-record
              conversation
              (list :native-compaction
                    :through-seq (1- (conversation-next-sequence conversation))
                    :family family
                    :wire-json (json-encode item)
                    :summary summary))))
      (setf (conversation-input-items conversation)
            (append (list item summary-item) ephemeral-items)
            (conversation-turn-state conversation) nil
            (conversation-last-total-tokens conversation) 0
            (gethash item (conversation-input-item-families conversation))
            family
            (gethash summary-item
                     (conversation-portable-handoff-families conversation))
            family)
      record)))


;;;; -- Conversation Loading --

(-> conversation--call-with-record-mapper (function function) (values &rest t))
(defun conversation--call-with-record-mapper (function mapper)
  "Call MAPPER with guarded FUNCTION and return all mapping values.

Translate storage failures into conversation corruption conditions without
translating a condition signaled by the record visitor."
  (let ((callback-store-error nil))
    (handler-case
        (funcall mapper
                 (lambda (record)
                   (handler-case
                       (funcall function record)
                     (store-error (condition)
                       (setf callback-store-error condition)
                       (error condition)))))
      (store-error (condition)
        (if (eq condition callback-store-error)
            (error condition)
            (error 'conversation-invariant-error
                   :message (format nil "Malformed conversation storage: ~A" condition)
                   :pathname (sexp-store:store-error-pathname condition)
                   :sequence (when (typep condition 'sexp-store:segment-error)
                               (sexp-store:segment-error-sequence condition))))))))

(-> conversation--map-records
    (pathname function &key (:start-position (integer 0)))
    (values integer boolean integer))
(defun conversation--map-records (pathname function &key (start-position 0))
  "Visit complete records, returning byte position, incomplete-tail flag and count."
  (conversation--call-with-record-mapper
   function
   (lambda (visit)
     (log-map visit pathname :start-position start-position))))


(-> conversation--segment-options
    (pathname &key (:on-header (option function))) list)
(defun conversation--segment-options (identity &key on-header)
  "Supply the conversation header schema and checkpoint policy to segmented logs."
  (list :header-function
        (lambda (pathname header)
          (let ((conversation (conversation--from-header identity pathname header)))
            (when on-header
              (funcall on-header conversation))
            (values (conversation-next-sequence conversation)
                    (= (getf (rest header) :version) 1))))
        :record-sequence (lambda (record) (getf (rest record) :seq))
        :validate-record
        (lambda (pathname record)
          (unless (conversation--record-form-p record)
            (error 'conversation-invariant-error
                   :message "A conversation segment record is malformed."
                   :pathname pathname :sequence nil)))
        :validate-first-record #'conversation--validate-segment-first-record))

(-> conversation--map-segment-records
    (pathname pathname function
     &key (:expected-start-sequence (option (integer 1))))
    (values integer boolean integer (integer 1) (integer 1)))
(defun conversation--map-segment-records
    (identity segment function &key expected-start-sequence)
  "Map one validated segment, retaining conversation corruption conditions."
  (conversation--call-with-record-mapper
   function
   (lambda (visit)
     (apply #'sexp-store:segment-map visit segment
            :expected-start-sequence expected-start-sequence
            (conversation--segment-options identity)))))

(-> conversation--map-storage-records (pathname function)
    (values boolean integer))
(defun conversation--map-storage-records (pathname function)
  "Map all durable records after validating segment sequences and boundaries."
  (let ((identity (conversation-storage-identity-pathname pathname)))
    (conversation--call-with-record-mapper
     function
     (lambda (visit)
       (multiple-value-bind (incomplete-p count next)
           (apply #'sexp-store:segments-map visit
                  (conversation-storage-pathnames identity)
                  (conversation--segment-options identity))
         (declare (ignore next))
         (values incomplete-p count))))))

(-> conversation-map-records (conversation function) (values boolean integer))
(defun conversation-map-records (conversation function)
  "Call FUNCTION for every durable record in CONVERSATION."
  (conversation--map-storage-records
   (conversation-pathname conversation) function))

(-> conversation-pending-input-identifiers (conversation) list)
(defun conversation-pending-input-identifiers (conversation)
  "Return detached pending-input identifiers durably recorded in CONVERSATION."
  (mapcar #'copy-seq
          (conversation-durable-pending-input-identifiers conversation)))

(-> conversation-picker-metadata-scan (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-scan (pathname)
  "Scan PATHNAME's segments once to create exact resume-picker metadata."
  (let ((working-seconds 0)
        (user-turn-count 0)
         (title
           (let* ((header (ignore-errors (conversation-peek-header pathname)))
                  (candidate (and header (getf (rest header) :title))))
             (and (conversation-title-valid-p candidate) candidate)))
        (search-message-count 0)
        (last-activity-at nil)
        (preview nil))
    (handler-case
        (multiple-value-bind
            (initial-segment initial-size initial-write-date)
            (conversation--file-identity pathname)
          (let ((initial-revision (conversation-picker-revision-read pathname)))
            (multiple-value-bind (incomplete-tail-p record-count)
                (conversation--map-storage-records
                 pathname
                 (lambda (record)
                   (multiple-value-setq
                       (working-seconds user-turn-count last-activity-at)
                     (conversation--activity-after-record
                      record
                      :working-seconds working-seconds
                      :user-turn-count user-turn-count
                      :last-activity-at last-activity-at))
                   (case (first record)
                     (:conversation
                      (let ((candidate (getf (rest record) :title)))
                        (when (conversation-title-valid-p candidate)
                          (setf title candidate))))
                     (:message
                      (when (and (null title)
                                 (= user-turn-count 1)
                                 (eq (getf (rest record) :role) ':user)
                                 (not (getf (rest record) :automatic-p))
                                 (stringp (getf (rest record) :content)))
                        (setf title
                              (conversation-title-derive
                               (getf (rest record) :content)))))
                     (:title
                      (let ((candidate (getf (rest record) :value)))
                        (when (conversation-title-valid-p candidate)
                          (setf title candidate)))))
                   (let ((record-preview
                           (conversation--record-preview record)))
                     (when record-preview
                       (incf search-message-count)
                       (setf preview record-preview)))))
              (when (plusp record-count)
                (multiple-value-bind
                    (final-segment final-size final-write-date)
                    (conversation--file-identity pathname)
                  (when (and (string= initial-segment final-segment)
                             (= initial-size final-size)
                             (= initial-write-date final-write-date)
                             (= initial-revision
                                (conversation-picker-revision-read pathname)))
                    (make-instance 'conversation-picker-metadata
                                   :source-segment initial-segment
                                   :source-size initial-size
                                   :source-write-date initial-write-date
                                   :source-revision initial-revision
                                   :working-seconds working-seconds
                                   :user-turn-count user-turn-count
                                   :title title
                                   :search-message-count search-message-count
                                   :preview preview
                                   :incomplete-tail-p incomplete-tail-p)))))))
      (error ()
        nil))))

(-> conversation-picker-metadata-find (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-find (pathname)
  "Return PATHNAME's picker cache, rebuilding under the source writer lock."
  (or (conversation-picker-metadata-read pathname)
      (let ((metadata nil))
        (ignore-errors
          (sexp-store:sidecar-rebuild
           (conversation-picker-metadata-pathname pathname)
           :lock-pathname (conversation-picker-source-lock-pathname pathname)
           :build (lambda ()
                    (setf metadata (conversation-picker-metadata-scan pathname)))
           :encode #'conversation-picker-metadata-record
           :source-token (lambda () (conversation-picker-metadata-source-token pathname))
           :value-token #'conversation-picker-metadata-token))
        metadata)))


(-> conversation-picker-search-scan
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-scan (pathname)
  "Scan PATHNAME's segments once for complete durable user and assistant text."
  (let ((messages nil)
        (message-count 0))
    (handler-case
        (multiple-value-bind
            (initial-segment initial-size initial-write-date)
            (conversation--file-identity pathname)
          (let ((initial-search-revision
                  (conversation-picker-search-revision-read pathname)))
            (multiple-value-bind (incomplete-tail-p record-count)
                (conversation--map-storage-records
                 pathname
                 (lambda (record)
                   (let ((message (conversation--record-preview record)))
                     (when message
                       (incf message-count)
                       (push message messages)))))
              (declare (ignore incomplete-tail-p))
              (when (plusp record-count)
                (multiple-value-bind
                    (final-segment final-size final-write-date)
                    (conversation--file-identity pathname)
                  (when (and (string= initial-segment final-segment)
                             (= initial-size final-size)
                             (= initial-write-date final-write-date)
                             (= initial-search-revision
                                (conversation-picker-search-revision-read
                                 pathname)))
                    (make-instance
                     'conversation-picker-search-index
                     :source-revision initial-search-revision
                     :message-count message-count
                     :messages (nreverse messages))))))))
      (error ()
        nil))))


(-> conversation-picker-search-find
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-find (pathname)
  "Return PATHNAME's search index, rebuilding under the source writer lock."
  (or (conversation-picker-search-read pathname)
      (when (conversation-picker-metadata-find pathname)
        (or (conversation-picker-search-read pathname)
            (progn
              (ignore-errors
                (sexp-store:sidecar-rebuild
                 (conversation-picker-search-pathname pathname)
                 :lock-pathname (conversation-picker-source-lock-pathname pathname)
                 :build (lambda () (conversation-picker-search-scan pathname))
                 :encode #'conversation-picker-search-index-record
                 :source-token (lambda () (conversation-picker-search-revision-read pathname))
                 :value-token #'conversation-picker-search-index-source-revision))
              (conversation-picker-search-read pathname))))))

(-> conversation-picker-search-close (conversation) null)
(defun conversation-picker-search-close (conversation)
  "Best-effort publish CONVERSATION's search sidecar before its release.

Appends only advance the search revision and leave the sidecar stale by
design; searches rebuild it on demand. Closing pre-warms the sidecar so
later picker searches read it without scanning the log."
  (when (conversation-persisted-p conversation)
    (ignore-errors
      (conversation-picker-search-find (conversation-pathname conversation))))
  nil)


(-> conversation-picker-search-index-text
    (conversation-picker-search-index)
    string)
(defun conversation-picker-search-index-text (index)
  "Return INDEX's chronological message corpus as one searchable string."
  (format nil
          "~{~A~^~%~}"
          (conversation-picker-search-index-messages index)))

(-> conversation--record-source-pathname (pathname) pathname)
(defun conversation--record-source-pathname (pathname)
  "Return the physical segment read when PATHNAME denotes a conversation."
  (if (conversation-chunk-start-sequence pathname)
      pathname
      (or (conversation-storage-active-pathname pathname)
          pathname)))

(-> conversation--read-records (pathname) (values list boolean))
(defun conversation--read-records (pathname)
  "Read complete forms from PATHNAME's active segment and report an incomplete tail."
  (let ((source (conversation--record-source-pathname pathname)))
    (handler-case
        (log-read source)
      (error (condition)
        (error 'conversation-invariant-error
               :message (format nil "Malformed conversation record: ~A"
                                condition)
               :pathname source
               :sequence nil)))))

(-> conversation--record-error (conversation list string) null)
(defun conversation--record-error (conversation properties message)
  "Signal persisted record invariant failure MESSAGE for CONVERSATION."
  (error 'conversation-invariant-error
         :message message
         :pathname (conversation-pathname conversation)
         :sequence (getf properties :seq)))

(-> conversation--record-form-p (t) boolean)
(defun conversation--record-form-p (value)
  "Validate finite keyword records using the conversation's first-key policy."
  (sexp-store:record-shape-p value :duplicate-keys ':first))

(-> conversation--tool-content-block-from-record (conversation list list) t)
(defun conversation--tool-content-block-from-record
    (conversation descriptor properties)
  "Restore one durable tool content DESCRIPTOR for CONVERSATION."
  (cond
    ((and (listp descriptor)
          (stringp (getf descriptor :text))
          (null (getf descriptor :image)))
     (getf descriptor :text))
    ((and (listp descriptor)
          (getf descriptor :image)
          (null (getf descriptor :text)))
     (conversation--remember-image-attachment
      conversation
      (image-attachment-from-record
       (getf descriptor :image)
       (conversation-image-artifact-root conversation))))
    (t
     (conversation--record-error
      conversation properties "A persisted tool content block is invalid."))))

(-> conversation--property-present-p (list keyword) boolean)
(defun conversation--property-present-p (properties indicator)
  "Return true when property list PROPERTIES contains INDICATOR."
  (loop for tail on properties by #'cddr
        thereis (eq (first tail) indicator)))

(-> conversation--record-images (conversation list) list)
(defun conversation--record-images (conversation descriptors)
  "Restore and remember image DESCRIPTORS for CONVERSATION."
  (conversation--remember-image-attachments
   conversation
   (mapcar
    (lambda (descriptor)
      (image-attachment-from-record
       descriptor (conversation-image-artifact-root conversation)))
    descriptors)))

(-> conversation--project-record (keyword conversation list) t)
(defgeneric conversation--project-record (kind conversation properties)
  (:documentation "Project KIND-specific record PROPERTIES into CONVERSATION.")
  (:method ((kind t) conversation properties)
    nil))

(defmethod conversation--project-record
    ((kind (eql :message)) conversation properties)
  (let ((content (getf properties :content))
        (images (getf properties :images)))
    (when (and (null (conversation-title conversation))
               (= (conversation-user-turn-count conversation) 1)
               (eq (getf properties :role) ':user)
               (not (getf properties :automatic-p))
               (stringp content))
      (let ((title (conversation-title-derive content)))
        (when title
          (setf (conversation-title conversation) title
                (conversation-title-source conversation) ':initial))))
    (when images
      (let ((attachments (conversation--record-images conversation images)))
        (unless (stringp content)
          (conversation--record-error
           conversation properties
           "A persisted image message has invalid text content."))
        (conversation--append-input-item
         conversation (user-message-item content attachments))))))

(defmethod conversation--project-record
    ((kind (eql :title)) conversation properties)
  (let* ((title (getf properties :value))
         (normalized-title
           (and (stringp title)
                (conversation-title-normalize title)))
         (source (getf properties :source)))
    (unless (and normalized-title
                 (member source '(:initial :generated))
                 (conversation--title-transition-valid-p
                  (conversation-title-source conversation) source))
      (conversation--record-error
       conversation properties "A persisted conversation title is invalid."))
    (setf (conversation-title conversation) (copy-seq normalized-title)
          (conversation-title-source conversation) source)))

(defmethod conversation--project-record
    ((kind (eql :tool-result)) conversation properties)
  (cond
    ((conversation--property-present-p properties :content-blocks)
     (let ((call-id (getf properties :call-id))
           (descriptors (getf properties :content-blocks)))
       (unless (consp descriptors)
         (conversation--record-error
          conversation properties
          "A persisted multimodal tool result has no content blocks."))
       (unless (non-empty-string-p call-id)
         (conversation--record-error
          conversation properties
          "A persisted multimodal tool result has no call identifier."))
       (conversation--append-input-item
        conversation
        (function-call-output-item
         call-id
         (conversation--tool-content-output
          (mapcar
           (lambda (descriptor)
             (conversation--tool-content-block-from-record
              conversation descriptor properties))
           descriptors))))))
    ((conversation--property-present-p properties :images)
     (let ((call-id (getf properties :call-id))
           (descriptors (getf properties :images)))
       (unless (consp descriptors)
         (conversation--record-error
          conversation properties "A persisted image tool result has no images."))
       (unless (non-empty-string-p call-id)
         (conversation--record-error
          conversation properties
          "A persisted image tool result has no call identifier."))
       (conversation--append-input-item
        conversation
        (function-call-output-item
         call-id
         (conversation--tool-content-output
          (append
           (when (non-empty-string-p (or (getf properties :output) ""))
             (list (getf properties :output)))
           (conversation--record-images conversation descriptors)))))))))

(defmethod conversation--project-record
    ((kind (eql :inherited-reference)) conversation properties)
  (let ((wire-json (getf properties :wire-json)))
    (unless (and (non-empty-string-p
                  (getf properties :source-conversation-id))
                 (stringp wire-json))
      (conversation--record-error
       conversation properties
       "A persisted inherited reference has invalid metadata."))
    (let ((items
            (handler-case
                (json-decode wire-json)
              (error ()
                (conversation--record-error
                 conversation properties
                 "A persisted inherited reference contains invalid JSON.")))))
      (unless (and (vectorp items)
                   (plusp (length items))
                   (conversation--inherited-reference-boundary-p
                    (aref items (1- (length items)))))
        (conversation--record-error
         conversation properties
         "A persisted inherited reference has an invalid boundary."))
      (let ((messages
              (loop for index below (1- (length items))
                    for message =
                      (conversation--inherited-reference-message
                       (aref items index))
                    when message collect message)))
        (unless (= (length messages) (1- (length items)))
          (conversation--record-error
           conversation properties
           "A persisted inherited reference contains a nonportable item."))
        (dolist (item messages)
          (conversation--append-input-item conversation item))
        (conversation--append-input-item
         conversation
         (conversation--inherited-reference-boundary-item))))))

(defmethod conversation--project-record
    ((kind (eql :summary)) conversation properties)
  (let ((content (getf properties :content)))
    (unless (stringp content)
      (conversation--record-error
       conversation properties
       "A persisted summary checkpoint has invalid content."))
    (setf (conversation-input-items conversation)
          (list (conversation-summary-item content))
          (conversation-last-total-tokens conversation) 0)))

(defmethod conversation--project-record
    ((kind (eql :native-compaction)) conversation properties)
  (let ((family (getf properties :family))
        (wire-json (getf properties :wire-json))
        (summary (getf properties :summary)))
    (unless (and (keywordp family)
                 (stringp wire-json)
                 (non-empty-string-p summary))
      (conversation--record-error
       conversation properties
       "A persisted native compaction checkpoint is invalid."))
    (let ((item
            (handler-case
                (json-decode wire-json)
              (error ()
                (conversation--record-error
                 conversation properties
                 "A persisted native compaction checkpoint is not JSON.")))))
      (native-compaction-item-canonicalize item)
      (unless (native-compaction-item-p item)
        (conversation--record-error
         conversation properties
         "A persisted native compaction item is unsupported."))
      (let ((summary-item (conversation-summary-item summary)))
        (setf (conversation-input-items conversation)
              (list item summary-item)
              (conversation-turn-state conversation) nil
              (conversation-last-total-tokens conversation) 0
              (gethash item (conversation-input-item-families conversation))
              family
              (gethash summary-item
                       (conversation-portable-handoff-families conversation))
              family)))))

(defmethod conversation--project-record
    ((kind (eql :provider)) conversation properties)
  (let ((total (conversation--usage-total
                (getf (getf properties :metadata) :usage))))
    (when total
      (setf (conversation-last-total-tokens conversation) total))))

(defmethod conversation--project-record
    ((kind (eql :configuration)) conversation properties)
  (let ((model (getf properties :model))
        (reasoning-effort (getf properties :reasoning-effort)))
    (unless (conversation--persisted-model-selection-p model reasoning-effort)
      (conversation--record-error
       conversation properties
       "A persisted conversation model selection is invalid."))
    (setf (conversation-model conversation) model
          (conversation-reasoning-effort conversation) reasoning-effort)))

(defmethod conversation--project-record
    ((kind (eql :turn-aborted)) conversation properties)
  "Validate and project one durable aborted-turn boundary."
  (let ((sequence (getf properties :seq))
        (turn-start-sequence (getf properties :turn-start-seq))
        (last-complete-sequence (getf properties :last-complete-seq))
        (reason (getf properties :reason))
        (condition-type (getf properties :condition-type))
        (message (getf properties :message))
        (request-number (getf properties :request-number)))
    (unless (and (typep sequence '(integer 1))
                 (typep turn-start-sequence '(integer 1))
                 (typep last-complete-sequence '(integer 1))
                 (<= turn-start-sequence last-complete-sequence)
                 (< last-complete-sequence sequence)
                 (member reason '(:cancelled :agent-loop :application-error))
                 (non-empty-string-p condition-type)
                 (<= (length condition-type)
                     *conversation-turn-aborted-condition-type-maximum-characters*)
                 (non-empty-string-p message)
                 (<= (length message)
                     *conversation-turn-aborted-message-maximum-characters*)
                 (or (null request-number)
                     (typep request-number '(integer 1))))
      (conversation--record-error
       conversation properties "A persisted turn-aborted record is invalid."))
    (when (eql turn-start-sequence
               (conversation-last-aborted-turn-start-sequence conversation))
      (conversation--record-error
       conversation properties "A persisted turn is marked aborted more than once."))
    (setf (conversation-last-aborted-turn-start-sequence conversation)
          turn-start-sequence)))

(-> conversation--repair-provider-item-arguments (json-object) json-object)
(defun conversation--repair-provider-item-arguments (item)
  "Repair malformed function-call arguments persisted by older Autolith releases."
  ;; Compatibility reader for records written through v0.46.0. Remove only when
  ;; upgrading from v0.46.x conversation histories is no longer supported.
  (when (and (function-call-item-p item)
             (not (json-object-source-p (json-get item "arguments"))))
    (setf (gethash "arguments" item) "{}"))
  item)

(-> conversation--apply-record (conversation list) null)
(defun conversation--apply-record (conversation record)
  "Project one persisted RECORD into CONVERSATION's in-memory state."
  (unless (conversation--record-form-p record)
    (conversation--record-error
     conversation nil "A persisted conversation record is not a keyword property list."))
  (let* ((kind (first record))
         (properties (rest record))
         (sequence (getf properties :seq))
         (wire-json (getf properties :wire-json))
         (content-blocks-p
           (conversation--property-present-p properties :content-blocks))
         (images-p
           (conversation--property-present-p properties :images))
         (wire-json-p
           (conversation--property-present-p properties :wire-json))
         (picker-search-message (conversation--record-preview record)))
    (when (eq kind :tool-result)
      (when (> (count t (list content-blocks-p images-p wire-json-p)) 1)
        (conversation--record-error
         conversation properties
         "A persisted tool result contains multiple wire projections."))
      (when (and (or content-blocks-p images-p)
                 (not (eq (getf properties :status) :ok)))
        (conversation--record-error
         conversation properties
         "A failed persisted tool result cannot contain image output.")))
    (conversation--note-activity conversation record)
    (conversation--note-pending-input-identifier conversation record)
    (when picker-search-message
      (conversation--note-picker-search-message
       conversation picker-search-message))
    (when (eq kind :goal)
      (setf (conversation-latest-goal-record conversation) record))
    (when (integerp sequence)
      (setf (conversation-next-sequence conversation)
            (max (conversation-next-sequence conversation) (1+ sequence))))
    (conversation--project-record kind conversation properties)
    (when (and (member kind '(:message :provider-item :tool-result))
               (stringp wire-json)
               (not images-p)
               (not content-blocks-p))
      (let ((item (json-decode wire-json)))
        (unless (json-object-p item)
          (conversation--record-error
           conversation properties
           "A persisted provider item is not a JSON object."))
        (when (eq kind ':provider-item)
          (conversation--repair-provider-item-arguments item))
        (conversation--append-input-item conversation item)))
    nil))

(-> conversation--peek-segment-header (pathname) (option list))
(defun conversation--peek-segment-header (pathname)
  "Return exact physical PATHNAME's leading conversation header, or NIL."
  (handler-case
      (with-open-file (stream pathname :direction ':input :external-format ':utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker)))
          (if (and (conversation--record-form-p form)
                   (eq (first form) :conversation))
              form
              nil)))
    (error ()
      nil)))

(-> conversation-peek-header (pathname) (option list))
(defun conversation-peek-header (pathname)
  "Return PATHNAME's active leading conversation header, or NIL when unreadable."
  (conversation--peek-segment-header
   (conversation--record-source-pathname pathname)))

(-> conversation--header-string-list-p (t) boolean)
(defun conversation--header-string-list-p (value)
  "Return true when VALUE is a finite proper list of nonempty strings."
  (handler-case
      (and (listp value)
           (or (null value) (list-length value))
           (every #'non-empty-string-p value)
           t)
    (error ()
      nil)))

(-> conversation--header-record-list-p (t keyword) boolean)
(defun conversation--header-record-list-p (value kind)
  "Return true when VALUE is a finite proper list of KIND records."
  (handler-case
      (and (listp value)
           (or (null value) (list-length value))
           (every (lambda (record)
                    (and (conversation--record-form-p record)
                         (eq (first record) kind)))
                  value)
           t)
    (error ()
      nil)))

(-> conversation--from-header (pathname pathname list) conversation)
(defun conversation--from-header (identity log-pathname header)
  "Validate HEADER and return its empty resumable conversation projection."
  (flet ((invalid (message)
           (error 'conversation-invariant-error
                  :message message
                  :pathname identity
                  :sequence nil)))
    (unless (and (conversation--record-form-p header)
                 (eq (first header) :conversation)
                 (member (getf (rest header) :version) '(1 2))
                 (non-empty-string-p (getf (rest header) :id))
                 (typep (getf (rest header) :created-at) 'timestamp))
      (invalid "The conversation header is missing or unsupported."))
    (let* ((properties (rest header))
           (version (getf properties :version))
           (identifier (getf properties :id))
           (identity-identifier (pathname-name identity))
           (directory (getf properties :directory))
           (title (and (= version 2) (getf properties :title)))
           (normalized-title
             (and (stringp title)
                  (conversation-title-normalize title)))
           (title-source (and (= version 2) (getf properties :title-source)))
           (model (getf properties :model))
           (reasoning-effort (getf properties :reasoning-effort))
           (prompt-cache-key (getf properties :prompt-cache-key))
           (chunk-start-sequence
             (if (= version 2)
                 (getf properties :chunk-start-sequence)
                 1))
           (working-seconds
             (if (= version 2)
                 (getf properties :working-seconds)
                 0))
           (user-turn-count
             (if (= version 2)
                 (getf properties :user-turn-count)
                 0))
           (last-activity-at
             (and (= version 2)
                  (getf properties :last-activity-at)))
           (search-message-count
             (if (= version 2)
                 (getf properties :picker-search-message-count)
                 0))
           (preview
             (and (= version 2)
                  (getf properties :picker-preview)))
           (pending-identifiers
             (if (= version 2)
                 (getf properties :pending-input-identifiers)
                 nil))
           (user-operation-records
             (if (= version 2)
                 (getf properties :user-operation-records)
                 nil))
           (latest-goal-record
             (and (= version 2)
                  (getf properties :latest-goal-record))))
      (unless (or (and (null title) (null title-source))
                  (and normalized-title
                       (member title-source '(:initial :generated))))
        (invalid "The conversation header has an invalid title."))
      (unless (and (stringp identity-identifier)
                   (string= identifier identity-identifier))
        (invalid
         "The conversation header identifier disagrees with its storage identity."))
      (unless (or (and (null model) (null reasoning-effort))
                  (conversation--persisted-model-selection-p model reasoning-effort))
        (invalid "The conversation header has an invalid model selection."))
      (when (and prompt-cache-key
                 (not (non-empty-string-p prompt-cache-key)))
        (invalid "The conversation header has an invalid prompt cache key."))
      (if (= version 1)
          (unless (equal log-pathname identity)
            (invalid "A legacy conversation header is not in its identity file."))
          (unless (and (typep chunk-start-sequence '(integer 1))
                       (equal log-pathname
                              (conversation-chunk-pathname
                               identity chunk-start-sequence))
                       (typep working-seconds '(integer 0))
                       (typep user-turn-count '(integer 0))
                       (or (null last-activity-at)
                           (typep last-activity-at 'timestamp))
                       (typep search-message-count '(integer 0))
                       (or (null preview) (stringp preview))
                       (conversation--header-string-list-p pending-identifiers)
                       (= (length pending-identifiers)
                          (length (remove-duplicates pending-identifiers
                                                     :test #'string=)))
                       (conversation--header-record-list-p
                        user-operation-records ':user-operation)
                       (or (null latest-goal-record)
                           (and (conversation--record-form-p latest-goal-record)
                                (eq (first latest-goal-record) :goal))))
            (invalid
             "The conversation chunk header contains invalid resumable state.")))
      (let ((conversation
              (make-instance 'conversation
                             :identifier identifier
                             :prompt-cache-key prompt-cache-key
                             :pathname identity
                             :log-pathname log-pathname
                             :persisted-p t
                             :incomplete-tail-p nil
                             :created-at (getf properties :created-at)
                             :origin-directory
                             (and (stringp directory) directory)
                             :title (and normalized-title
                                         (copy-seq normalized-title))
                             :title-source title-source
                             :model (and (non-empty-string-p model) model)
                             :reasoning-effort
                             (and (non-empty-string-p reasoning-effort)
                                  reasoning-effort)
                             :next-sequence chunk-start-sequence
                             :input-items nil)))
        (when (= version 2)
          (setf (conversation-working-seconds conversation) working-seconds
                (conversation-user-turn-count conversation) user-turn-count
                (conversation-last-activity-at conversation) last-activity-at
                (conversation-picker-search-message-count conversation)
                search-message-count
                (conversation-picker-preview conversation)
                (and preview (copy-seq preview))
                (conversation-durable-pending-input-identifiers conversation)
                (mapcar #'copy-seq pending-identifiers)
                (conversation-latest-goal-record conversation)
                (copy-tree latest-goal-record))
          (dolist (record user-operation-records)
            (conversation--project-record
             ':user-operation conversation (rest record))))
        conversation))))

(-> conversation--validate-segment-first-record (pathname list list) null)
(defun conversation--validate-segment-first-record (pathname header record)
  "Validate RECORD as PATHNAME's deterministic first durable record."
  (unless (conversation--record-form-p record)
    (error 'conversation-invariant-error
           :message "A conversation chunk begins with a malformed record."
           :pathname pathname
           :sequence nil))
  (when (= (getf (rest header) :version) 2)
    (let* ((properties (rest record))
           (start-sequence (getf (rest header) :chunk-start-sequence))
           (sequence (getf properties :seq))
           (compaction-p (conversation--compaction-record-p record)))
      (unless (and (typep sequence '(integer 1))
                   (= sequence start-sequence))
        (error 'conversation-invariant-error
               :message
               "A conversation chunk does not begin at its declared sequence."
               :pathname pathname
               :sequence sequence))
      (when (and (> start-sequence 1)
                 (not compaction-p))
        (error 'conversation-invariant-error
               :message
               "A rotated conversation chunk does not begin with a compaction checkpoint."
               :pathname pathname
               :sequence sequence))
      (when compaction-p
        (unless (and (typep (getf properties :through-seq) '(integer 0))
                     (= (getf properties :through-seq)
                        (1- start-sequence)))
          (error 'conversation-invariant-error
                 :message
                 "A conversation compaction checkpoint has an invalid boundary."
                 :pathname pathname
                 :sequence sequence))
        (when (and (eq (first record) :summary)
                   (not (stringp (getf properties :content))))
          (error 'conversation-invariant-error
                 :message "A conversation summary checkpoint has invalid content."
                 :pathname pathname
                 :sequence sequence)))))
  nil)

(-> conversation--load-active-segment (pathname pathname) conversation)
(defun conversation--load-active-segment (identity pathname)
  "Load one validated self-contained active chunk at PATHNAME."
  (let* ((header (conversation--peek-segment-header pathname))
         (conversation
           (conversation--from-header identity pathname header)))
    (multiple-value-bind
        (position incomplete-tail-p record-count start-sequence next-sequence)
        (conversation--map-segment-records
         identity
         pathname
         (lambda (record)
           (conversation--apply-record conversation record)))
      (declare (ignore position start-sequence next-sequence))
      (unless (plusp record-count)
        (error 'conversation-invariant-error
               :message "The conversation chunk has no durable record."
               :pathname pathname
               :sequence nil))
      (setf (conversation-incomplete-tail-p conversation) incomplete-tail-p)
      conversation)))

(-> conversation--load-all-segments (pathname list) conversation)
(defun conversation--load-all-segments (identity pathnames)
  "Replay ordered storage with the conversation schema and checkpoint policy."
  (let ((conversation nil))
    (conversation--call-with-record-mapper
     (lambda (record) (conversation--apply-record conversation record))
     (lambda (visit)
       (multiple-value-bind (incomplete-p count next)
           (apply #'sexp-store:segments-map visit pathnames
                  (conversation--segment-options
                   identity :on-header (lambda (candidate)
                                         (unless conversation
                                           (setf conversation candidate)))))
         (declare (ignore count next))
         (unless conversation
           (error 'conversation-invariant-error
                  :message "The conversation header is missing or unsupported."
                  :pathname identity :sequence nil))
         (setf (conversation-log-pathname conversation) (first (last pathnames))
               (conversation-incomplete-tail-p conversation) incomplete-p)
         conversation)))))

(-> conversation-load (pathname) conversation)
(defun conversation-load (pathname)
  "Load PATHNAME from its newest self-contained chunk or legacy segments."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (pathnames (conversation-storage-pathnames identity))
         (active (first (last pathnames))))
    (unless active
      (error 'conversation-invariant-error
             :message "The conversation does not contain a durable segment."
             :pathname identity
             :sequence nil))
    (let* ((header (conversation-peek-header active))
           (conversation
             (if (= (or (and header (getf (rest header) :version)) 0) 2)
                 (conversation--load-active-segment identity active)
                 (conversation--load-all-segments identity pathnames))))
      (conversation--repair-incomplete-tool-calls conversation)
      conversation)))

(-> conversation-pathname-for-id (configuration string) pathname)
(defun conversation-pathname-for-id (configuration identifier)
  "Return CONFIGURATION's stable conversation identity for IDENTIFIER."
  (merge-pathnames (make-pathname
                    :name
                    (conversation-identifier-migration-resolve
                     configuration identifier)
                    :type "sexp")
                   (configuration-conversation-root configuration)))

(-> conversation-load-by-id (configuration string) conversation)
(defun conversation-load-by-id (configuration identifier)
  "Load IDENTIFIER from CONFIGURATION's conversation storage."
  (let ((pathname (conversation-pathname-for-id configuration identifier)))
    (unless (conversation-storage-active-pathname pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A does not exist." identifier)
             :pathname pathname
             :sequence nil))
    (conversation-load pathname)))

(-> conversation--active-pathname-non-empty-p (pathname) boolean)
(defun conversation--active-pathname-non-empty-p (pathname)
  "Return true when active segment PATHNAME has a header and durable record."
  (handler-case
      (let ((header-seen-p nil))
        (conversation--map-records
         pathname
         (lambda (record)
           (cond
             ((not header-seen-p)
              (unless (and (listp record)
                           (eq (first record) :conversation))
                (return-from conversation--active-pathname-non-empty-p nil))
              (setf header-seen-p t))
             (t
              (return-from conversation--active-pathname-non-empty-p t)))))
        nil)
    (error ()
      nil)))

(-> conversation--pathname-non-empty-p (pathname) boolean)
(defun conversation--pathname-non-empty-p (pathname)
  "Return true when PATHNAME's active segment has a header and durable record."
  (let ((active (conversation-storage-active-pathname pathname)))
    (and active
         (conversation--active-pathname-non-empty-p active))))

(-> conversation-list (configuration) list)
(defun conversation-list (configuration)
  "Return non-empty stable conversation identities, newest first."
  (let ((root       (configuration-conversation-root configuration))
        (identities nil)
        (summaries  nil))
    (when (uiop:directory-exists-p root)
      (dolist (pathname (uiop:directory-files root "*.sexp"))
        (pushnew pathname identities :test #'equal))
      (dolist (directory (uiop:subdirectories root))
        (let ((identifier (first (last (pathname-directory directory)))))
          (when (stringp identifier)
            (pushnew
             (merge-pathnames
              (make-pathname :name identifier :type "sexp")
              root)
             identities
             :test #'equal)))))
    (dolist (identity identities)
      (let ((active (conversation-storage-active-pathname identity)))
        (when (and active
                   (conversation--active-pathname-non-empty-p active))
          (push (list identity (or (file-write-date active) 0)) summaries))))
    (mapcar #'first
            (sort summaries #'> :key #'second))))


(-> conversation-activity-summary (pathname)
    (values (integer 0) (integer 0)))
(defun conversation-activity-summary (pathname)
  "Return PATHNAME's cached working seconds and user-turn count.

A missing or stale cache is rebuilt once from durable records. Subsequent
resume-picker reads use the compact sidecar instead of replaying the log."
  (let ((metadata (conversation-picker-metadata-find pathname)))
    (if metadata
        (values (conversation-picker-metadata-working-seconds metadata)
                (conversation-picker-metadata-user-turn-count metadata))
        (values 0 0))))


(defparameter *conversation-delete-directory-tree-function*
  #'uiop:delete-directory-tree
  "Function used to remove private artifact trees after conversation deletion.")


(-> conversation-delete (configuration string) pathname)
(defun conversation-delete (configuration identifier)
  "Delete IDENTIFIER's conversation segments, sidecars, and private artifacts.

Returns the stable removed identity pathname. Signals CONVERSATION-ERROR when
storage is missing, IDENTIFIER is invalid, or another process owns it."
  (let* ((normalized
           (conversation-identifier-migration-resolve configuration identifier))
         (pathname (conversation-pathname-for-id configuration normalized))
         (chunk-directory
           (conversation-storage-directory-pathname pathname))
         (sidecar-pathnames
           (conversation-picker-sidecar-pathnames pathname))
         (task-fragment
           (or (conversation-identifier-path-fragment normalized)
               (string-downcase normalized)))
         (artifact-roots
           (list
            (merge-pathnames
             (format nil "conversation-images/~A/" normalized)
             (configuration-data-root configuration))
            (merge-pathnames
             (format nil "tasks/~A/" task-fragment)
             (configuration-data-root configuration))))
         (lease nil))
    (unwind-protect
         (progn
           (setf lease (conversation-lease-acquire configuration normalized))
           (unless (conversation-storage-active-pathname pathname)
             (error 'conversation-error
                    :message
                    (format nil "Conversation ~A does not exist."
                            (conversation-identifier-display normalized))
                    :pathname pathname
                    :sequence nil))
           (handler-case
               (progn
                 (when (probe-file pathname)
                   (delete-file pathname))
                 (when (uiop:directory-exists-p chunk-directory)
                   (uiop:delete-directory-tree
                    chunk-directory
                    :validate t
                    :if-does-not-exist ':ignore)))
             (error (condition)
               (error 'conversation-invariant-error
                      :message
                      (format nil "Could not delete conversation ~A: ~A"
                              (conversation-identifier-display normalized)
                              condition)
                      :pathname pathname
                      :sequence nil)))
           (handler-case
               (progn
                 (dolist (sidecar-pathname sidecar-pathnames)
                   (when (probe-file sidecar-pathname)
                     (delete-file sidecar-pathname)))
                 (dolist (root artifact-roots)
                   (when (probe-file root)
                     (funcall *conversation-delete-directory-tree-function*
                              root
                              :validate t
                              :if-does-not-exist ':ignore))))
             (error (condition)
               (error 'conversation-invariant-error
                      :message
                      (format
                       nil
                       "Conversation ~A was deleted, but private artifacts remain: ~A"
                       (conversation-identifier-display normalized)
                       condition)
                      :pathname pathname
                      :sequence nil))))
      (when lease
        (conversation-lease-release lease)))
    pathname))
