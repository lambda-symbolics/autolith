(in-package #:autolith-supervisor)

(defparameter *memory-content-limit* 5000 "The maximum characters in one memory body.")

(defparameter *memory-source-count-limit* 8 "The maximum sources cited by one memory.")

(defparameter *memory-premise-count-limit* 8 "The maximum premises one memory may be derived from.")

(defparameter *memory-quote-limit* 400 "The maximum characters in one quoted source passage.")

(defparameter *memory-quote-minimum*
  8
  "The minimum characters in one whitespace-normalized quoted passage.")

(defparameter *memory-source-path-limit* 1024 "The maximum characters in one cited file path.")

(defparameter *memory-source-url-limit* 2048 "The maximum characters in one cited web address.")

(defparameter *memory-source-file-byte-limit*
  (* 4 1024 1024)
  "The largest cited file read when checking whether a source still holds.")

(serapeum/types:-> memory-source-quote-normalize (string) string)
(defun memory-source-quote-normalize (text)
  "Return TEXT with whitespace runs collapsed to single spaces and trimmed.

Quotes are compared in this form, so reindenting or rewrapping a cited passage
does not invalidate the memory that cites it."
  (string-trim " "
               (with-output-to-string (stream)
                 (loop with spacing-p = nil
                       for character across text
                       if (find character '(#\  #\Tab #\Newline #\Return #\Page))
                       do (unless spacing-p (write-char #\  stream) (setf spacing-p t)) else
                       do (write-char character stream) (setf spacing-p nil)))))

(serapeum/types:-> memory-source-quote-p (t) boolean)
(defun memory-source-quote-p (value)
  "Return true when VALUE is a bounded quote with enough normalized text."
  (and (stringp value) (<= (length value) *memory-quote-limit*)
       (>= (length (memory-source-quote-normalize value)) *memory-quote-minimum*)))

(serapeum/types:-> memory-source-quote-found-p (string string) boolean)
(defun memory-source-quote-found-p (quote text)
  "Return true when QUOTE occurs in TEXT, ignoring whitespace differences."
  (and (search (memory-source-quote-normalize quote) (memory-source-quote-normalize text)) t))

(serapeum/types:-> memory-source-text-digest (string) string)
(defun memory-source-text-digest (text)
  "Return the lowercase hexadecimal SHA-256 of TEXT's UTF-8 octets."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence ':sha256 (ls-compat:utf8-string-to-octets text))))

(serapeum/types:-> memory-content-digest (string string) string)
(defun memory-content-digest (title content)
  "Return the digest identifying one exact TITLE and CONTENT of a memory.

Derived memories record their premises' digests, so replacing a premise's text
is distinguishable from merely touching it."
  (memory-source-text-digest (format nil "~A~C~A" title (code-char 0) content)))

(serapeum/types:-> memory-source-digest-p (t) boolean)
(defun memory-source-digest-p (value)
  "Return true when VALUE is a lowercase hexadecimal SHA-256 digest."
  (and (stringp value) (= (length value) 64)
       (every (lambda (character) (find character "0123456789abcdef")) value) t))

(serapeum/types:-> memory-source--optional-quote-p (t) boolean)
(defun memory-source--optional-quote-p (value)
  "Return true when VALUE is absent or a valid quoted passage."
  (or (null value) (memory-source-quote-p value)))

(serapeum/types:-> memory-source-record-p (t) boolean)
(defun memory-source-record-p (record)
  "Return true when RECORD is one complete portable memory source."
  (and (alexandria:proper-list-p record) (evenp (length (rest record)))
       (let ((properties (rest record)))
         (and (typep (getf properties :observed-at) 'timestamp)
              (case (first record)
                (:file
                 (let ((path (getf properties :path)))
                   (and (non-empty-string-p path) (<= (length path) *memory-source-path-limit*)
                        (member (getf properties :relative) '(nil t))
                        (memory-source--optional-quote-p (getf properties :quote))
                        (memory-source-digest-p (getf properties :digest)))))
                (:url
                 (let ((url (getf properties :url)))
                   (and (non-empty-string-p url) (<= (length url) *memory-source-url-limit*)
                        (memory-source--optional-quote-p (getf properties :quote)))))
                (:user
                 (let ((conversation (getf properties :conversation)))
                   (and (memory-source-quote-p (getf properties :quote))
                        (or (null conversation) (non-empty-string-p conversation)))))
                (otherwise
                 nil))))
       t))

(serapeum/types:-> memory-premise-record-p (t) boolean)
(defun memory-premise-record-p (record)
  "Return true when RECORD names one premise and its observed digest.

A premise is another memory, or with :KIND :CLAIM a structured knowledge claim."
  (and (alexandria:proper-list-p record) (evenp (length record))
       (non-empty-string-p (getf record :id)) (memory-source-digest-p (getf record :digest))
       (member (getf record :kind) '(nil :claim))
       (or (null (getf record :text))
           (and (stringp (getf record :text))
                (<= (length (getf record :text)) *memory-content-limit*)))
       t))

(serapeum/types:-> memory-sources-valid-p (t memory-scope) boolean)
(defun memory-sources-valid-p (sources scope)
  "Return true when SOURCES is a bounded source list valid for SCOPE.

Global memories apply in every workspace, so their file sources are absolute."
  (and (alexandria:proper-list-p sources) (<= (length sources) *memory-source-count-limit*)
       (every #'memory-source-record-p sources)
       (or (eq scope ':workspace)
           (notany (lambda (source) (and (eq (first source) :file) (getf (rest source) :relative)))
                   sources))
       t))

(serapeum/types:-> memory-premises-valid-p (t) boolean)
(defun memory-premises-valid-p (premises)
  "Return true when PREMISES is a bounded list of distinct premise records."
  (and (alexandria:proper-list-p premises) (<= (length premises) *memory-premise-count-limit*)
       (every #'memory-premise-record-p premises)
       (= (length premises)
          (length
           (remove-duplicates premises :key (lambda (premise) (getf premise :id)) :test
                              #'string=)))
       t))

(serapeum/types:-> memory-source-file-pathname (list (option string)) pathname)
(defun memory-source-file-pathname (source workspace)
  "Return the absolute pathname cited by file SOURCE of a memory in WORKSPACE.

A relative path needs the WORKSPACE it was recorded against."
  (let ((path (getf (rest source) :path)))
    (cond ((not (getf (rest source) :relative))
           (uiop/filesystem:parse-native-namestring path))
          (workspace
           (uiop/pathname:merge-pathnames* (uiop/pathname:parse-unix-namestring path)
                                           (uiop/pathname:ensure-directory-pathname workspace)))
          (t
           (error 'memory-error :message
                  (format nil "Relative cited path ~A has no workspace." path) :pathname
                  (uiop/pathname:parse-unix-namestring path) :identifier nil)))))

(serapeum/types:-> memory-source-file-display (list) string)
(defun memory-source-file-display (source)
  "Return the path a person reads for file SOURCE."
  (getf (rest source) :path))

(serapeum/types:-> memory-source-read-file (pathname) (values (option string) (option keyword)))
(defun memory-source-read-file (pathname)
  "Return source text, or NIL and the precise read failure."
  (handler-case
   (values
    (if *memory-store-context*
        (memory-context-read-file *memory-store-context* pathname)
        (ls-compat.files:read-file-text pathname :maximum-octets *memory-source-file-byte-limit*
                                        :follow-links-p          t))
    nil)
   (ls-compat.posix:file-operation-failed (condition)
    (values nil
            (if (eq (ls-compat.posix:file-operation-failed-reason condition) :missing)
                :missing
                :unreadable)))
   (ls-compat.files:file-too-large nil (values nil :too-large))
   (ls-compat.files:file-not-utf-8 nil (values nil :not-text))
   (memory-policy-error nil (values nil :unreadable)) (error nil (values nil :unreadable))))

(serapeum/types:-> memory-source-file-holds (list (option string) (option keyword))
                   (values keyword (option keyword)))
(defun memory-source-file-holds (source text failure)
  "Return whether file SOURCE holds for file TEXT read with FAILURE.

The first value is :HOLDS, :FAILED, or :UNVERIFIED; the second names the
reason for any other outcome: :CHANGED, :MISSING, or the read FAILURE."
  (cond
   (text
    (let ((quote (getf (rest source) :quote)))
      (if (if quote
              (memory-source-quote-found-p quote text)
              (string= (memory-source-text-digest text) (getf (rest source) :digest)))
          (values ':holds nil)
          (values ':failed ':changed))))
   ((eq failure ':missing)
    (values ':failed ':missing)) (t
                                  (values ':unverified failure))))

(serapeum/types:-> memory-source-observe-file
                   (pathname &key (:quote (option string)) (:time timestamp)) list)
(defun memory-source-observe-file (pathname &key quote time)
  "Return an absolute file source record for PATHNAME as observed at TIME.

QUOTE must occur in the file. Storing the memory records a file inside its
workspace relatively, so the memory survives moving or importing the
workspace. Signal MEMORY-ERROR with a corrective message when the file cannot
support the memory."
  (flet ((fail (control &rest arguments)
           (error 'memory-error :message (apply #'format nil control arguments) :pathname pathname
                  :identifier nil)))
    (when (and quote (not (memory-source-quote-p quote)))
      (fail "A quoted source passage needs ~D to ~D characters." *memory-quote-minimum*
       *memory-quote-limit*))
    (multiple-value-bind (text failure)
        (memory-source-read-file pathname)
      (unless text
        (fail
         (ecase failure
           (:missing
            "Cited file ~A does not exist.")
           (:too-large
            "Cited file ~A is larger than the checked limit.")
           (:not-text
            "Cited file ~A is not UTF-8 text.")
           (:unreadable
            "Cited file ~A cannot be read."))
         (uiop/filesystem:native-namestring pathname)))
      (when (and quote (not (memory-source-quote-found-p quote text)))
        (fail "The quoted passage does not occur in ~A. Quote the file's current text exactly."
         (uiop/filesystem:native-namestring pathname)))
      (list :file :path (uiop/filesystem:native-namestring pathname) :relative nil :quote quote
            :digest (memory-source-text-digest text) :observed-at time))))
