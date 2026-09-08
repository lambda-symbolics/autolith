(in-package #:autolith)

;;;; -- Persistent Memories --

(defparameter *memory-format-version* 1
  "The readable persistent memory format version.")

(defparameter *memory-title-limit* 200
  "The maximum characters in one memory title.")

(defparameter *memory-content-limit* 5000
  "The maximum characters in one memory body.")

(defparameter *memory-tag-count-limit* 16
  "The maximum tags attached to one memory.")

(defparameter *memory-tag-limit* 80
  "The maximum characters in one memory tag.")

(defparameter *memory-search-term-limit* 24
  "The maximum distinct terms considered by one memory query.")

(defvar *memory-lock* (make-recursive-lock "Autolith persistent memories")
  "Serialize same-process memory reads, appends, and guarded transactions.")

(defclass memory ()
  ((identifier
    :initarg :identifier
    :reader memory-identifier
    :type non-empty-string
    :documentation "The stable identifier shared by replacement records.")
   (created-at
    :initarg :created-at
    :reader memory-created-at
    :type timestamp
    :documentation "The creation time as Common Lisp universal time.")
   (updated-at
    :initarg :updated-at
    :reader memory-updated-at
    :type timestamp
    :documentation "The newest replacement time as Common Lisp universal time.")
   (scope
    :initarg :scope
    :reader memory-scope
    :type memory-scope
    :documentation "Whether the memory is global or local to one workspace.")
   (workspace
    :initarg :workspace
    :reader memory-workspace
    :type (option string)
    :documentation "The workspace owning a workspace-scoped memory.")
   (title
    :initarg :title
    :reader memory-title
    :type non-empty-string
    :documentation "A short retrieval-oriented description.")
   (content
    :initarg :content
    :reader memory-content
    :type non-empty-string
    :documentation "The durable fact, preference, decision, or guidance.")
   (tags
    :initarg :tags
    :reader memory-tags
    :type list
    :documentation "Short retrieval terms associated with the memory.")
   (source-conversation
    :initarg :source-conversation
    :reader memory-source-conversation
    :type (option string)
    :documentation "The conversation that most recently wrote the memory."))
  (:documentation "One active persistent memory reconstructed from the memory log."))

(defclass memory-match ()
  ((memory
    :initarg :memory
    :reader memory-match-memory
    :type memory
    :documentation "The persistent memory selected by one relevance query.")
   (score
    :initarg :score
    :reader memory-match-score
    :type (integer 1)
    :documentation "The deterministic weighted lexical relevance score.")
   (terms
    :initarg :terms
    :reader memory-match-terms
    :type list
    :documentation "The normalized query terms found in this memory."))
  (:documentation "One ranked persistent-memory search result."))


;;;; -- Validation and Records --

(-> memory--validate-text (t string integer) string)
(defun memory--validate-text (value field limit)
  "Return non-empty string VALUE after validating FIELD and LIMIT."
  (unless (non-empty-string-p value)
    (error 'memory-error
           :message (format nil "Memory ~A must be a non-empty string." field)
           :pathname #P"memories.sexp"
           :identifier nil))
  (when (> (length value) limit)
    (error 'memory-error
           :message (format nil "Memory ~A exceeds the ~:D-character limit."
                            field
                            limit)
           :pathname #P"memories.sexp"
           :identifier nil))
  value)

(-> memory--validate-tags (t) list)
(defun memory--validate-tags (tags)
  "Return validated, case-insensitively unique memory TAGS."
  (unless (handler-case
              (let ((length (list-length tags)))
                (and (integerp length) t))
            (type-error ()
              nil))
    (error 'memory-error
           :message "Memory tags must be a proper, non-circular list of strings."
           :pathname #P"memories.sexp"
           :identifier nil))
  (when (> (length tags) *memory-tag-count-limit*)
    (error 'memory-error
           :message (format nil "A memory may have at most ~:D tags."
                            *memory-tag-count-limit*)
           :pathname #P"memories.sexp"
           :identifier nil))
  (dolist (tag tags)
    (memory--validate-text tag "tag" *memory-tag-limit*))
  (remove-duplicates (copy-list tags) :test #'string-equal :from-end t))

(-> memory--record (memory) list)
(defun memory--record (memory)
  "Return the complete portable replacement record for MEMORY."
  (list :memory
        :version *memory-format-version*
        :id (memory-identifier memory)
        :created-at (memory-created-at memory)
        :updated-at (memory-updated-at memory)
        :scope (memory-scope memory)
        :workspace (memory-workspace memory)
        :title (memory-title memory)
        :content (memory-content memory)
        :tags (memory-tags memory)
        :source-conversation (memory-source-conversation memory)))

(-> memory--record->memory (pathname list) memory)
(defun memory--record->memory (pathname record)
  "Validate and convert one portable memory RECORD from PATHNAME."
  (let ((version (getf (rest record) :version))
        (identifier (getf (rest record) :id))
        (created-at (getf (rest record) :created-at))
        (updated-at (getf (rest record) :updated-at))
        (scope (getf (rest record) :scope))
        (workspace (getf (rest record) :workspace))
        (title (getf (rest record) :title))
        (content (getf (rest record) :content))
        (tags (getf (rest record) :tags))
        (source-conversation (getf (rest record) :source-conversation)))
    (unless (and (eql version *memory-format-version*)
                 (non-empty-string-p identifier)
                 (typep created-at 'timestamp)
                 (typep updated-at 'timestamp)
                 (<= created-at updated-at)
                 (typep scope 'memory-scope)
                 (if (eq scope :workspace)
                     (non-empty-string-p workspace)
                     (null workspace))
                 (or (null source-conversation)
                     (non-empty-string-p source-conversation)))
      (error 'memory-error
             :message "A persistent memory record has invalid metadata."
             :pathname pathname
             :identifier (and (stringp identifier) identifier)))
    (handler-case
        (make-instance 'memory
                       :identifier identifier
                       :created-at created-at
                       :updated-at updated-at
                       :scope scope
                       :workspace workspace
                       :title (memory--validate-text
                               title "title" *memory-title-limit*)
                       :content (memory--validate-text
                                 content "content" *memory-content-limit*)
                       :tags (memory--validate-tags tags)
                       :source-conversation source-conversation)
      (memory-error (condition)
        (error 'memory-error
               :message (autolith-error-message condition)
               :pathname pathname
               :identifier identifier)))))


;;;; -- Readable Log --

(-> memory--store (configuration) sexp-store:log-store)
(defun memory--store (configuration)
  "Describe the memory schema, reducer, and product-owned paths."
  (let ((pathname (configuration-memory-path configuration)))
    (make-instance
     'sexp-store:log-store
     :pathname pathname
     :lock-pathname (readable-state-lock-pathname pathname "memories.lock")
     :header (list :memories :version *memory-format-version*)
     :header-validator
     (lambda (form)
       (sexp-store:record-check form :tag ':memories
                               :versions (list *memory-format-version*)))
     :validator (lambda (form) (keywordp (first form)))
     :initial-state (lambda () (make-hash-table :test #'equal))
     :reducer (lambda (active record)
                (memory--reduce-record pathname active record))
     :finalizer #'memory--sort-active)))

(-> memory--reduce-record (pathname hash-table list) hash-table)
(defun memory--reduce-record (pathname active record)
  "Apply one memory replacement or attributed tombstone to fresh ACTIVE state."
  (case (first record)
    (:memory
     (let ((memory (memory--record->memory pathname record)))
       (setf (gethash (memory-identifier memory) active) memory)))
    (:memory-forgotten
     (let ((identifier (getf (rest record) :id))
           (source-conversation (getf (rest record) :source-conversation)))
       (unless (and (eql (getf (rest record) :version) *memory-format-version*)
                    (non-empty-string-p identifier)
                    (typep (getf (rest record) :time) 'timestamp)
                    (or (null source-conversation)
                        (non-empty-string-p source-conversation)))
         (error 'memory-error
                :message "A memory tombstone has invalid metadata."
                :pathname pathname
                :identifier (and (stringp identifier) identifier)))
       (remhash identifier active)))
    (otherwise
     (error 'memory-error
            :message (format nil "Unsupported persistent memory record ~S."
                             (first record))
            :pathname pathname
            :identifier nil)))
  active)

(-> memory--sort-active (hash-table) list)
(defun memory--sort-active (active)
  "Return active memories in deterministic newest-first retrieval order."
  (sort (loop for memory being the hash-values of active collect memory)
        (lambda (left right)
          (or (> (memory-updated-at left) (memory-updated-at right))
              (and (= (memory-updated-at left) (memory-updated-at right))
                   (string< (memory-identifier left)
                            (memory-identifier right)))))))

(-> memory--transact (configuration function) t)
(defun memory--transact (configuration update)
  "Apply UPDATE through the store, translating storage failures for callers."
  (handler-case
      (sexp-store:store-transact (memory--store configuration) update)
    (sexp-store:store-error (cause)
      (error 'memory-error
             :message (sexp-store:store-error-message cause)
             :pathname (sexp-store:store-error-pathname cause)
             :identifier nil))))


;;;; -- Selection and Mutation --

(-> memory--visible-p (memory configuration memory-visibility) boolean)
(defun memory--visible-p (memory configuration visibility)
  "Return true when MEMORY belongs to CONFIGURATION under VISIBILITY."
  (let ((current-workspace
          (namestring (configuration-working-directory configuration))))
    (case visibility
      (:all
       t)
      (:global
       (eq (memory-scope memory) :global))
      (:workspace
       (and (eq (memory-scope memory) :workspace)
            (string= (memory-workspace memory) current-workspace)))
      (:relevant
       (or (eq (memory-scope memory) :global)
           (and (eq (memory-scope memory) :workspace)
                (string= (memory-workspace memory) current-workspace)))))))

(-> memory-list
    (configuration &key (:visibility memory-visibility))
    list)
(defun memory-list (configuration &key (visibility :relevant))
  "Return active memories selected by VISIBILITY, newest first."
  (with-recursive-lock-held (*memory-lock*)
    (memory--transact
     configuration
     (lambda (active)
       (values nil
               (remove-if-not
                (lambda (memory)
                  (memory--visible-p memory configuration visibility))
                active)
               nil)))))

(-> memory-find (configuration string) (option memory))
(defun memory-find (configuration identifier)
  "Return active memory IDENTIFIER, regardless of its scope."
  (find identifier
        (memory-list configuration :visibility ':all)
        :test #'string=
        :key #'memory-identifier))

(-> memory-remember
    (configuration
     &key
     (:identifier (option string))
     (:title string)
     (:content string)
     (:scope (option memory-scope))
     (:tags list)
     (:source-conversation (option string)))
    memory)
(defun memory-remember
    (configuration &key identifier title content scope tags source-conversation)
  "Create or completely replace one durable memory and return its active value."
  (let ((validated-title
          (memory--validate-text title "title" *memory-title-limit*))
        (validated-content
          (memory--validate-text content "content" *memory-content-limit*))
        (validated-tags (memory--validate-tags tags)))
    (unless (or (null identifier) (non-empty-string-p identifier))
      (error 'memory-error
             :message "A replacement memory identifier must be non-empty."
             :pathname (configuration-memory-path configuration)
             :identifier nil))
    (unless (or (null scope) (typep scope 'memory-scope))
      (error 'memory-error
             :message "Memory scope must be GLOBAL or WORKSPACE."
             :pathname (configuration-memory-path configuration)
             :identifier identifier))
    (with-recursive-lock-held (*memory-lock*)
      (memory--transact
       configuration
       (lambda (active)
         (let ((existing (and identifier
                              (find identifier active
                                    :test #'string=
                                    :key #'memory-identifier))))
           (when (and identifier (null existing))
             (error 'memory-error
                    :message (format nil "Memory ~A does not exist." identifier)
                    :pathname (configuration-memory-path configuration)
                    :identifier identifier))
           (let* ((now (get-universal-time))
                  (selected-scope (or scope
                                      (and existing (memory-scope existing))
                                      :workspace))
                  (memory
                    (make-instance
                     'memory
                     :identifier (or identifier (make-identifier))
                     :created-at (if existing (memory-created-at existing) now)
                     :updated-at now
                     :scope selected-scope
                     :workspace (if (eq selected-scope :workspace)
                                    (if (and existing
                                             (null scope)
                                             (eq (memory-scope existing) :workspace))
                                        (memory-workspace existing)
                                        (namestring
                                         (configuration-working-directory
                                          configuration)))
                                    nil)
                     :title validated-title
                     :content validated-content
                     :tags validated-tags
                     :source-conversation source-conversation)))
             (values (list (memory--record memory)) memory t))))))))

(-> memory-forget
    (configuration string &key (:source-conversation (option string)))
    memory)
(defun memory-forget (configuration identifier &key source-conversation)
  "Stop recalling active memory IDENTIFIER by appending an attributed tombstone."
  (unless (or (null source-conversation)
              (non-empty-string-p source-conversation))
    (error 'memory-error
           :message "Memory source conversation must be non-empty when supplied."
           :pathname (configuration-memory-path configuration)
           :identifier identifier))
  (with-recursive-lock-held (*memory-lock*)
    (memory--transact
     configuration
     (lambda (active)
       (let ((memory (find identifier active
                           :test #'string=
                           :key #'memory-identifier)))
         (unless memory
           (error 'memory-error
                  :message (format nil "Memory ~A does not exist." identifier)
                  :pathname (configuration-memory-path configuration)
                  :identifier identifier))
         (values
          (list (list :memory-forgotten
                      :version *memory-format-version*
                      :id identifier
                      :time (get-universal-time)
                      :source-conversation source-conversation))
          memory t))))))

(-> memory--search-terms (string) list)
(defun memory--search-terms (query)
  "Return bounded, distinct lowercase whitespace-delimited terms from QUERY."
  (unless (non-empty-string-p query)
    (error 'memory-error
           :message "A memory search query must be non-empty."
           :pathname #P"memories.sexp"
           :identifier nil))
  (let ((terms
          (remove-duplicates
           (mapcar #'string-downcase
                   (remove-if-not
                    #'non-empty-string-p
                    (uiop:split-string
                     query
                     :separator '(#\Space #\Tab #\Newline #\Return))))
           :test #'string=
           :from-end t)))
    (subseq terms 0 (min *memory-search-term-limit* (length terms)))))

(-> memory--term-score (string memory) integer)
(defun memory--term-score (term memory)
  "Return the field-weighted relevance of TERM to MEMORY."
  (+ (if (search term (memory-title memory) :test #'char-equal) 12 0)
     (if (find term (memory-tags memory) :test #'string-equal) 9 0)
     (if (search term (memory-content memory) :test #'char-equal) 4 0)
     (if (and (memory-workspace memory)
              (search term (memory-workspace memory) :test #'char-equal))
         1
         0)))

(-> memory--match (memory string list) (option memory-match))
(defun memory--match (memory query terms)
  "Return MEMORY's ranked match for QUERY and TERMS, or NIL when unrelated."
  (let* ((term-scores
           (mapcar (lambda (term)
                     (cons term (memory--term-score term memory)))
                   terms))
         (matched
           (remove-if-not #'plusp term-scores :key #'rest)))
    (when matched
      (let* ((phrase (string-trim '(#\Space #\Tab #\Newline #\Return) query))
             (all-terms-p (= (length matched) (length terms)))
             (phrase-title-p
               (search phrase (memory-title memory) :test #'char-equal))
             (phrase-content-p
               (search phrase (memory-content memory) :test #'char-equal))
             (score
               (+ (reduce #'+ matched :key #'rest)
                  (* 3 (length matched))
                  (if all-terms-p 15 0)
                  (if phrase-title-p 18 0)
                  (if phrase-content-p 5 0))))
        (make-instance 'memory-match
                       :memory memory
                       :score score
                       :terms (mapcar #'first matched))))))

(-> memory--match-before-p (memory-match memory-match) boolean)
(defun memory--match-before-p (left right)
  "Return true when LEFT ranks before RIGHT deterministically."
  (let ((left-memory (memory-match-memory left))
        (right-memory (memory-match-memory right)))
    (or (> (memory-match-score left) (memory-match-score right))
        (and (= (memory-match-score left) (memory-match-score right))
             (or (> (memory-updated-at left-memory)
                    (memory-updated-at right-memory))
                 (and (= (memory-updated-at left-memory)
                         (memory-updated-at right-memory))
                      (string< (memory-identifier left-memory)
                               (memory-identifier right-memory))))))))

(-> memory-rank
    (configuration string &key (:visibility memory-visibility))
    list)
(defun memory-rank (configuration query &key (visibility :relevant))
  "Return weighted lexical matches for QUERY in descending relevance order."
  (let ((terms (memory--search-terms query)))
    (sort
     (remove nil
             (mapcar (lambda (memory)
                       (memory--match memory query terms))
                     (memory-list configuration :visibility visibility)))
     #'memory--match-before-p)))

(-> memory-search
    (configuration string &key (:visibility memory-visibility))
    list)
(defun memory-search (configuration query &key (visibility :relevant))
  "Return memories matching QUERY in descending lexical relevance order."
  (mapcar #'memory-match-memory
          (memory-rank configuration query :visibility visibility)))


;;;; -- Presentation --

(-> memory--timestamp-string (timestamp) string)
(defun memory--timestamp-string (timestamp)
  "Return TIMESTAMP as an ISO-8601 UTC string."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

(-> memory--excerpt (string integer) string)
(defun memory--excerpt (content limit)
  "Return a single-line prefix of CONTENT no longer than LIMIT characters."
  (let* ((single-line
           (with-output-to-string (stream)
             (loop with spacing-p = nil
                   for character across content
                   if (find character '(#\Space #\Tab #\Newline #\Return))
                     do (unless spacing-p
                          (write-char #\Space stream)
                          (setf spacing-p t))
                   else
                     do (write-char character stream)
                        (setf spacing-p nil))))
         (trimmed (string-trim '(#\Space) single-line)))
    (if (<= (length trimmed) limit)
        trimmed
        (format nil "~A..." (subseq trimmed 0 (max 0 (- limit 3)))))))
