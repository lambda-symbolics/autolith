(in-package #:autolith)

;;;; -- Memory Sources and Premises --

(defparameter *memory-content-limit* 5000
  "The maximum characters in one memory body.")

(defparameter *memory-source-count-limit* 8
  "The maximum sources cited by one memory.")

(defparameter *memory-premise-count-limit* 8
  "The maximum premises one memory may be derived from.")

(defparameter *memory-quote-limit* 400
  "The maximum characters in one quoted source passage.")

(defparameter *memory-quote-minimum* 8
  "The minimum characters in one whitespace-normalized quoted passage.")

(defparameter *memory-source-path-limit* 1024
  "The maximum characters in one cited file path.")

(defparameter *memory-source-url-limit* 2048
  "The maximum characters in one cited web address.")

(defparameter *memory-source-file-byte-limit* (* 4 1024 1024)
  "The largest cited file read when checking whether a source still holds.")


;;;; -- Quoted Passages --

(-> memory-source-quote-normalize (string) string)
(defun memory-source-quote-normalize (text)
  "Return TEXT with whitespace runs collapsed to single spaces and trimmed.

Quotes are compared in this form, so reindenting or rewrapping a cited passage
does not invalidate the memory that cites it."
  (string-trim
   " "
   (with-output-to-string (stream)
     (loop with spacing-p = nil
           for character across text
           if (find character '(#\Space #\Tab #\Newline #\Return #\Page))
             do (unless spacing-p
                  (write-char #\Space stream)
                  (setf spacing-p t))
           else
             do (write-char character stream)
                (setf spacing-p nil)))))

(-> memory-source-quote-p (t) boolean)
(defun memory-source-quote-p (value)
  "Return true when VALUE is a bounded quote with enough normalized text."
  (and (stringp value)
       (<= (length value) *memory-quote-limit*)
       (>= (length (memory-source-quote-normalize value)) *memory-quote-minimum*)))

(-> memory-source-quote-found-p (string string) boolean)
(defun memory-source-quote-found-p (quote text)
  "Return true when QUOTE occurs in TEXT, ignoring whitespace differences."
  (and (search (memory-source-quote-normalize quote)
               (memory-source-quote-normalize text))
       t))


;;;; -- Digests --

(-> memory-source-text-digest (string) string)
(defun memory-source-text-digest (text)
  "Return the lowercase hexadecimal SHA-256 of TEXT's UTF-8 octets."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence ':sha256 (utf8-string-to-octets text))))

(-> memory-content-digest (string string) string)
(defun memory-content-digest (title content)
  "Return the digest identifying one exact TITLE and CONTENT of a memory.

Derived memories record their premises' digests, so replacing a premise's text
is distinguishable from merely touching it."
  (memory-source-text-digest
   (format nil "~A~C~A" title (code-char 0) content)))

(-> memory-source-digest-p (t) boolean)
(defun memory-source-digest-p (value)
  "Return true when VALUE is a lowercase hexadecimal SHA-256 digest."
  (and (stringp value)
       (= (length value) 64)
       (every (lambda (character)
                (find character "0123456789abcdef"))
              value)
       t))


;;;; -- Record Validation --

(-> memory-source--optional-quote-p (t) boolean)
(defun memory-source--optional-quote-p (value)
  "Return true when VALUE is absent or a valid quoted passage."
  (or (null value) (memory-source-quote-p value)))

(-> memory-source-record-p (t) boolean)
(defun memory-source-record-p (record)
  "Return true when RECORD is one complete portable memory source."
  (and (proper-list-p record)
       (evenp (length (rest record)))
       (let ((properties (rest record)))
         (and (typep (getf properties :observed-at) 'timestamp)
              (case (first record)
                (:file
                 (let ((path (getf properties :path)))
                   (and (non-empty-string-p path)
                        (<= (length path) *memory-source-path-limit*)
                        (member (getf properties :relative) '(nil t))
                        (memory-source--optional-quote-p (getf properties :quote))
                        (memory-source-digest-p (getf properties :digest)))))
                (:url
                 (let ((url (getf properties :url)))
                   (and (non-empty-string-p url)
                        (<= (length url) *memory-source-url-limit*)
                        (memory-source--optional-quote-p (getf properties :quote)))))
                (:user
                 (let ((conversation (getf properties :conversation)))
                   (and (memory-source-quote-p (getf properties :quote))
                        (or (null conversation)
                            (non-empty-string-p conversation)))))
                (otherwise
                 nil))))
       t))

(-> memory-premise-record-p (t) boolean)
(defun memory-premise-record-p (record)
  "Return true when RECORD names one premise and its observed digest.

A premise is another memory, or with :KIND :CLAIM a structured knowledge claim."
  (and (proper-list-p record)
       (evenp (length record))
       (non-empty-string-p (getf record :id))
       (memory-source-digest-p (getf record :digest))
       (member (getf record :kind) '(nil :claim))
       (or (null (getf record :text))
           (and (stringp (getf record :text))
                (<= (length (getf record :text)) *memory-content-limit*)))
       t))

(-> memory-sources-valid-p (t memory-scope) boolean)
(defun memory-sources-valid-p (sources scope)
  "Return true when SOURCES is a bounded source list valid for SCOPE.

Global memories apply in every workspace, so their file sources are absolute."
  (and (proper-list-p sources)
       (<= (length sources) *memory-source-count-limit*)
       (every #'memory-source-record-p sources)
       (or (eq scope ':workspace)
           (notany (lambda (source)
                     (and (eq (first source) :file)
                          (getf (rest source) :relative)))
                   sources))
       t))

(-> memory-premises-valid-p (t) boolean)
(defun memory-premises-valid-p (premises)
  "Return true when PREMISES is a bounded list of distinct premise records."
  (and (proper-list-p premises)
       (<= (length premises) *memory-premise-count-limit*)
       (every #'memory-premise-record-p premises)
       (= (length premises)
          (length (remove-duplicates premises
                                     :key  (lambda (premise) (getf premise :id))
                                     :test #'string=)))
       t))


;;;; -- File Sources --

(-> memory-source-file-pathname (list (option string)) pathname)
(defun memory-source-file-pathname (source workspace)
  "Return the absolute pathname cited by file SOURCE of a memory in WORKSPACE.

A relative path needs the WORKSPACE it was recorded against."
  (let ((path (getf (rest source) :path)))
    (cond
      ((not (getf (rest source) :relative))
       (uiop:parse-native-namestring path))
      (workspace
       (uiop:merge-pathnames* (uiop:parse-unix-namestring path)
                              (uiop:ensure-directory-pathname workspace)))
      (t
       (error 'memory-error
              :message    (format nil "Relative cited path ~A has no workspace." path)
              :pathname   (uiop:parse-unix-namestring path)
              :identifier nil)))))

(-> memory-source-file-display (list) string)
(defun memory-source-file-display (source)
  "Return the path a person reads for file SOURCE."
  (getf (rest source) :path))

(-> memory-source-read-file (pathname) (values (option string) (option keyword)))
(defun memory-source-read-file (pathname)
  "Return PATHNAME's text, or NIL and why it could not be read.

The second value is :MISSING when nothing exists at PATHNAME, :TOO-LARGE
beyond the checked size limit, :NOT-TEXT for non-UTF-8 content, and
:UNREADABLE for any other failure."
  (handler-case
      (values (read-file-text pathname
                              :maximum-octets *memory-source-file-byte-limit*
                              :follow-links-p t)
              nil)
    (file-operation-failed (condition)
      (values nil (if (eq (file-operation-failed-reason condition) ':missing)
                      ':missing
                      ':unreadable)))
    (file-too-large ()
      (values nil ':too-large))
    (file-not-utf-8 ()
      (values nil ':not-text))
    (error ()
      (values nil ':unreadable))))


;;;; -- Per-pass Source Observations --

(defparameter *source-observation-file-limit* 256
  "Maximum distinct file observations retained by one assessment pass.")

(defparameter *source-observation-character-limit* (* 16 1024 1024)
  "Maximum file-text characters retained by one assessment pass.")

(defclass source-observation ()
  ((text :initarg :text :reader source-observation-text :type (option string)
         :documentation "The file text, or NIL after a failed or refused read.")
   (failure :initarg :failure :reader source-observation-failure :type (option keyword)
            :documentation "The read failure, including :OBSERVATION-LIMIT.")
   (status :initarg :status :reader source-observation-status
           :documentation "Filesystem identity and times captured before reading.")
   (digest :initarg :digest :reader source-observation-digest :type (option string)
           :documentation "The digest computed once from the retained text."))
  (:documentation "An immutable file observation shared by prose and structured evidence."))

(defclass source-observation-context ()
  ((files :initform      (make-hash-table :test #'equal)
          :reader source-observation-context-files :type hash-table
          :documentation "Admitted observations by absolute pathname namestring.")
   (file-limit :initarg :file-limit :type (integer 0 *)
               :reader        source-observation-context-file-limit
               :documentation "Maximum admitted observations, including failed reads.")
   (character-limit :initarg :character-limit :type (integer 0 *)
                    :reader        source-observation-context-character-limit
                    :documentation "Maximum retained file-text characters.")
   (characters :initform 0 :accessor source-observation-context-characters
               :type (integer 0 *) :documentation "Characters retained so far.")
   (exhausted-p :initform nil :accessor source-observation-context-exhausted-p
                :type boolean :documentation "Whether a source exceeded either bound."))
  (:documentation "One bounded assessment pass; never evicts or rereads an admitted file.

Ownership belongs to the caller, never to a global cache. Use a fresh context
for each pass. A saturated context returns unverified limit observations."))

(-> source-observation-context-create
    (&key (:file-limit (integer 0 *)) (:character-limit (integer 0 *)))
    source-observation-context)
(defun source-observation-context-create
    (&key (file-limit *source-observation-file-limit*)
          (character-limit *source-observation-character-limit*))
  "Return an empty pass bounded by FILE-LIMIT and CHARACTER-LIMIT."
  (make-instance 'source-observation-context
                 :file-limit file-limit :character-limit character-limit))

(-> source-observation-file-status (pathname) t)
(defun source-observation-file-status (pathname)
  "Return PATHNAME's identity, size and timestamps, or :MISSING."
  (let ((status (handler-case
                    (platform-path-status *platform* pathname :follow-links-p t)
                  (platform-error ()
                    nil))))
    (if status
        (list (platform-file-status-identity status)
              (platform-file-status-size status)
              (platform-file-status-modification-time status)
              (platform-file-status-change-time status))
        ':missing)))

(-> source-observation-context-file (source-observation-context pathname) source-observation)
(defun source-observation-context-file (context pathname)
  "Return the first observation of PATHNAME in CONTEXT, including failures.

Capture status before reading. Refuse new reads after either bound is reached;
when a read exceeds the remaining text budget, retain only its limit failure."
  (let* ((key   (namestring pathname))
         (files (source-observation-context-files context)))
    (or (gethash key files)
        (if (or (>= (hash-table-count files) (source-observation-context-file-limit context))
                (>= (source-observation-context-characters context)
                    (source-observation-context-character-limit context)))
            (progn
              (setf (source-observation-context-exhausted-p context) t)
              (make-instance 'source-observation :text nil :failure ':observation-limit
                             :status nil :digest nil))
            (let ((status (source-observation-file-status pathname)))
              (multiple-value-bind (text failure) (memory-source-read-file pathname)
                (when (and text
                           (> (+ (length text) (source-observation-context-characters context))
                              (source-observation-context-character-limit context)))
                  (setf text nil
                        failure ':observation-limit
                        (source-observation-context-exhausted-p context) t))
                (when text
                  (incf (source-observation-context-characters context) (length text)))
                (setf (gethash key files)
                      (make-instance 'source-observation :text text :failure failure
                                     :status status
                                     :digest (and text (memory-source-text-digest text))))))))))

(-> memory-source-file-holds
    (list (option string) (option keyword) &key (:digest (option string)))
    (values keyword (option keyword)))
(defun memory-source-file-holds (source text failure &key digest)
  "Return whether file SOURCE holds for file TEXT read with FAILURE.

The first value is :HOLDS, :FAILED, or :UNVERIFIED; the second names the
reason for any other outcome: :CHANGED, :MISSING, or the read FAILURE."
  (cond
    (text
     (let ((quote (getf (rest source) :quote)))
       (if (if quote
               (memory-source-quote-found-p quote text)
               (string= (or digest (memory-source-text-digest text))
                       (getf (rest source) :digest)))
           (values ':holds nil)
           (values ':failed ':changed))))
    ((eq failure ':missing)
     (values ':failed ':missing))
    (t
     (values ':unverified failure))))

(-> memory-source-observe-file
    (pathname &key (:quote (option string)) (:time timestamp))
    list)
(defun memory-source-observe-file (pathname &key quote time)
  "Return an absolute file source record for PATHNAME as observed at TIME.

QUOTE must occur in the file. Storing the memory records a file inside its
workspace relatively, so the memory survives moving or importing the
workspace. Signal MEMORY-ERROR with a corrective message when the file cannot
support the memory."
  (flet ((fail (control &rest arguments)
           (error 'memory-error
                  :message    (apply #'format nil control arguments)
                  :pathname   pathname
                  :identifier nil)))
    (when (and quote (not (memory-source-quote-p quote)))
      (fail "A quoted source passage needs ~D to ~D characters."
            *memory-quote-minimum* *memory-quote-limit*))
    (multiple-value-bind (text failure)
        (memory-source-read-file pathname)
      (unless text
        (fail (ecase failure
                (:missing
                 "Cited file ~A does not exist.")
                (:too-large
                 "Cited file ~A is larger than the checked limit.")
                (:not-text
                 "Cited file ~A is not UTF-8 text.")
                (:unreadable
                 "Cited file ~A cannot be read."))
              (uiop:native-namestring pathname)))
      (when (and quote (not (memory-source-quote-found-p quote text)))
        (fail "The quoted passage does not occur in ~A. Quote the file's current text exactly."
              (uiop:native-namestring pathname)))
      (list :file
            :path (uiop:native-namestring pathname)
            :relative    nil
            :quote       quote
            :digest      (memory-source-text-digest text)
            :observed-at time))))
