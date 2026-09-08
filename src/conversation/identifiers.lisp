(in-package #:autolith)

;;;; -- Conversation Identifier Format --

(-> conversation-identifier-normalize (t) string)
(defun conversation-identifier-normalize (value)
  "Return VALUE as a canonical stored identifier, accepting its display form."
  (handler-case
      (identifier-normalize value)
    (identifier-error ()
      (error 'conversation-identifier-error
             :message
             "A conversation identifier must contain seven case-sensitive Bitcoin Base58 characters, with an optional hyphen after the first."
             :value value))))

(-> conversation-identifier-path-component-p (t) boolean)
(defun conversation-identifier-path-component-p (value)
  "Return true when VALUE is one literal, non-wild pathname component."
  (and (non-empty-string-p value)
       (not (member value '("." "..") :test #'string=))
       (notany
        (lambda (character)
          (member character '(#\/ #\\ #\* #\? #\[ #\] #\Null)))
        value)
       t))

(-> conversation-identifier-validate-path-component (t) string)
(defun conversation-identifier-validate-path-component (value)
  "Return VALUE after rejecting path separators, traversal, and wildcards."
  (unless (conversation-identifier-path-component-p value)
    (error 'conversation-identifier-error
           :message
           "A conversation identifier must be one literal pathname component without traversal or wildcard syntax."
           :value value))
  value)

(-> conversation-identifier-display (string) string)
(defun conversation-identifier-display (identifier)
  "Return IDENTIFIER with its visual hyphen, retaining legacy values verbatim."
  (handler-case
      (identifier-display identifier)
    (identifier-error ()
      identifier)))

(-> conversation-identifier-path-fragment (string) (option string))
(defun conversation-identifier-path-fragment (identifier)
  "Return canonical IDENTIFIER unchanged for case-sensitive private paths."
  (and (identifier-p identifier) identifier))



;;;; -- Chunk Storage Paths --

(defparameter *conversation-chunk-sequence-width* 20
  "The minimum decimal width of deterministic conversation chunk names.")

(-> conversation-chunk-start-sequence (pathname) (option (integer 1)))
(defun conversation-chunk-start-sequence (pathname)
  "Return PATHNAME's canonical positive chunk start sequence, or NIL."
  (let ((name (pathname-name pathname)))
    (when (and (stringp name)
               (every #'digit-char-p name))
      (let ((sequence (parse-integer name :junk-allowed nil)))
        (and (typep sequence '(integer 1))
             (string= name
                      (format nil "~V,'0D"
                              *conversation-chunk-sequence-width*
                              sequence))
             sequence)))))

(-> conversation-storage-identity-pathname (pathname) pathname)
(defun conversation-storage-identity-pathname (pathname)
  "Return PATHNAME's stable top-level conversation identity pathname.

Legacy logs already use this pathname. A chunk pathname maps to the sibling
identity named by its containing conversation directory."
  (if (conversation-chunk-start-sequence pathname)
      (let* ((directory (pathname-directory pathname))
             (identifier (and directory (first (last directory)))))
        (if (stringp identifier)
            (make-pathname :directory (butlast directory)
                           :name identifier
                           :type "sexp"
                           :defaults pathname)
            pathname))
      pathname))

(-> conversation-storage-directory-pathname (pathname) pathname)
(defun conversation-storage-directory-pathname (pathname)
  "Return the chunk directory belonging to identity or chunk PATHNAME."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (identifier (pathname-name identity)))
    (make-pathname :directory (append (pathname-directory identity)
                                      (list identifier))
                   :name nil
                   :type nil
                   :defaults identity)))

(-> conversation-chunk-pathname (pathname (integer 1)) pathname)
(defun conversation-chunk-pathname (pathname start-sequence)
  "Return PATHNAME's deterministic chunk beginning at START-SEQUENCE."
  (merge-pathnames
   (make-pathname :name (format nil "~V,'0D"
                                *conversation-chunk-sequence-width*
                                start-sequence)
                  :type "sexp")
   (conversation-storage-directory-pathname pathname)))

(-> conversation-storage-pathnames (pathname) list)
(defun conversation-storage-pathnames (pathname)
  "Return PATHNAME's durable log segments in chronological order.

A legacy top-level file, when present, is the oldest segment. Deterministic
chunk files follow in numeric start-sequence order."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (directory (conversation-storage-directory-pathname identity))
         (chunks
           (if (uiop:directory-exists-p directory)
               (sort
                (remove-if-not #'conversation-chunk-start-sequence
                               (uiop:directory-files directory "*.sexp"))
                #'<
                :key #'conversation-chunk-start-sequence)
               nil)))
    (if (probe-file identity)
        (cons identity chunks)
        chunks)))

(-> conversation-storage-active-pathname (pathname) (option pathname))
(defun conversation-storage-active-pathname (pathname)
  "Return PATHNAME's newest durable log segment, or NIL when absent."
  (first (last (conversation-storage-pathnames pathname))))

(-> conversation-storage-occupied-p (pathname) boolean)
(defun conversation-storage-occupied-p (pathname)
  "Return true when PATHNAME's legacy file or chunk directory exists."
  (let ((identity (conversation-storage-identity-pathname pathname)))
    (or (not (null (probe-file identity)))
        (not (null
              (uiop:directory-exists-p
               (conversation-storage-directory-pathname identity)))))))

(-> conversation-storage-write-date (pathname) (integer 0))
(defun conversation-storage-write-date (pathname)
  "Return PATHNAME's newest durable segment write date, or zero."
  (let ((active (conversation-storage-active-pathname pathname)))
    (or (and active (file-write-date active)) 0)))

;;;; -- Conversation Identifier Allocation --

(-> conversation-identifier--pathname (pathname string) pathname)
(defun conversation-identifier--pathname (storage-root identifier)
  "Return IDENTIFIER's stable conversation identity beneath STORAGE-ROOT."
  (merge-pathnames (make-pathname :name identifier :type "sexp")
                   storage-root))

(-> conversation-identifier--occupied-p (pathname string) boolean)
(defun conversation-identifier--occupied-p (storage-root identifier)
  "Return true when IDENTIFIER has a legacy file or chunk directory."
  (conversation-storage-occupied-p
   (conversation-identifier--pathname storage-root identifier)))

(-> conversation-identifier--reserved-p (pathname string) boolean)
(defun conversation-identifier--reserved-p (storage-root identifier)
  "Return true when IDENTIFIER is occupied or reserved beneath STORAGE-ROOT."
  (let ((root (uiop:ensure-directory-pathname storage-root)))
    (or (conversation-identifier--occupied-p root identifier)
        (identifier-reserved-p identifier :namespace (namestring root)))))

(-> conversation-identifier-generate
    (pathname &key (:timestamp timestamp) (:reserved-identifiers list))
    string)
(defun conversation-identifier-generate
    (storage-root &key (timestamp (get-universal-time)) reserved-identifiers)
  "Allocate one stored identifier for TIMESTAMP beneath STORAGE-ROOT.

The identifier stays reserved for the life of this process. Its conversation
file is written after allocation returns, so releasing the reservation earlier
would let a second allocation in the same second choose the same identifier."
  (let ((root (uiop:ensure-directory-pathname storage-root)))
    (handler-case
        (identifier-generate
         :timestamp timestamp
         :namespace (namestring root)
         :reserved-identifiers reserved-identifiers
         :occupied-p (lambda (candidate)
                       (conversation-identifier--occupied-p root candidate)))
      (identifier-space-exhausted ()
        (error 'conversation-identifier-space-exhausted
               :message
               (format nil
                       "All conversation identifier seeds are occupied for Universal Time ~D."
                       timestamp)
               :pathname root
               :sequence nil
               :timestamp timestamp))
      (identifier-error (condition)
        (error 'conversation-identifier-error
               :message (identifier-error-message condition)
               :value timestamp)))))


;;;; -- Conversation Picker Sidecar Paths --

(-> conversation-picker-metadata-pathname (pathname) pathname)
(defun conversation-picker-metadata-pathname (conversation-pathname)
  "Return the compact picker-cache pathname for CONVERSATION-PATHNAME."
  (let* ((identity
           (conversation-storage-identity-pathname conversation-pathname))
         (conversation-root
           (uiop:pathname-directory-pathname identity))
         (data-root
           (uiop:pathname-parent-directory-pathname conversation-root)))
    (merge-pathnames
     (make-pathname :name (pathname-name identity) :type "sexp")
     (merge-pathnames "conversation-picker/" data-root))))


(-> conversation-picker-source-lock-pathname (pathname) pathname)
(defun conversation-picker-source-lock-pathname (conversation-pathname)
  "Return the persistent lock shared by source appends and picker reconstruction."
  (make-pathname :type "lock"
                 :defaults (conversation-picker-metadata-pathname conversation-pathname)))

(-> conversation-picker-revision-pathname (pathname) pathname)
(defun conversation-picker-revision-pathname (conversation-pathname)
  "Return the durable picker-cache revision pathname for CONVERSATION-PATHNAME."
  (make-pathname :type "revision"
                 :defaults
                 (conversation-picker-metadata-pathname conversation-pathname)))


(-> conversation-picker-search-pathname (pathname) pathname)
(defun conversation-picker-search-pathname (conversation-pathname)
  "Return the durable message-search pathname for CONVERSATION-PATHNAME."
  (let ((metadata-pathname
          (conversation-picker-metadata-pathname conversation-pathname)))
    (make-pathname :name (format nil "~A.search"
                                 (pathname-name metadata-pathname))
                   :type "sexp"
                   :defaults metadata-pathname)))


(-> conversation-picker-search-revision-pathname (pathname) pathname)
(defun conversation-picker-search-revision-pathname (conversation-pathname)
  "Return the durable message-search revision path for CONVERSATION-PATHNAME."
  (make-pathname :type "revision"
                 :defaults
                 (conversation-picker-search-pathname conversation-pathname)))


(-> conversation-picker-sidecar-pathnames (pathname) list)
(defun conversation-picker-sidecar-pathnames (conversation-pathname)
  "Return every picker sidecar pathname owned by CONVERSATION-PATHNAME."
  (list (conversation-picker-metadata-pathname conversation-pathname)
        (conversation-picker-revision-pathname conversation-pathname)
        (conversation-picker-search-pathname conversation-pathname)
        (conversation-picker-search-revision-pathname conversation-pathname)))

(-> conversation-picker-sidecars-delete (pathname) null)
(defun conversation-picker-sidecars-delete (conversation-pathname)
  "Delete every picker sidecar owned by CONVERSATION-PATHNAME."
  (dolist (pathname (conversation-picker-sidecar-pathnames conversation-pathname))
    (when (probe-file pathname)
      (delete-file pathname)))
  nil)
