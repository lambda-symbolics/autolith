(in-package #:autolith-supervisor)

(defvar *memory-store-context*
  nil
  "The trusted context of the current memory operation, when present.")

(defclass memory-store-context nil
          ((memory-path :initarg :memory-path :reader memory-context-memory-path :documentation
            "The trusted private memory log pathname.")
           (knowledge-path :initarg :knowledge-path :reader memory-context-knowledge-path
            :documentation "The trusted private knowledge log pathname.")
           (workspace :initarg :workspace :reader memory-context-workspace :documentation
            "The workspace directory approved by the parent.")
           (source-roots :initarg :source-roots :reader memory-context-source-roots :documentation
            "Trusted directories from which evidence may be read.")
           (mode :initarg :mode :reader memory-context-mode :documentation
            "The trusted knowledge capture mode."))
          (:documentation "Explicit private paths and source policy supplied by the parent."))

(defgeneric memory-context-make-identifier (context)
  (:documentation "Return a fresh memory identity from the trusted context."))

(defmethod memory-context-make-identifier ((context memory-store-context))
  "Use a random UUID for parent-owned memory records."
  (declare (ignore context))
  (memory--make-identifier))

(defgeneric memory-context-read-file
    (context pathname)
  (:documentation "Read approved source PATHNAME under trusted CONTEXT."))

(defgeneric memory-context-file-status
    (context pathname)
  (:documentation "Return the native status of approved source PATHNAME."))

(define-condition memory-policy-error
    (error)
    ((pathname :initarg :pathname :reader memory-policy-error-pathname :documentation
               "The source path outside the trusted read policy."))
  (:documentation "A source path is outside the parent-approved read roots.")
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Memory source is outside the approved read roots." stream))))

(defun make-memory-store-context
       (&key memory-path knowledge-path workspace source-roots (mode ':active))
  "Create a context from explicit trusted absolute paths and approved source roots."
  (dolist (pathname (list memory-path knowledge-path workspace))
    (unless (and (pathnamep pathname) (uiop/pathname:absolute-pathname-p pathname))
      (error 'type-error :datum pathname :expected-type 'pathname)))
  (make-instance 'memory-store-context :memory-path memory-path :knowledge-path knowledge-path
                 :workspace (uiop/pathname:ensure-directory-pathname workspace) :source-roots
                 (mapcar #'uiop/pathname:ensure-directory-pathname
                         (or source-roots (list workspace)))
                 :mode mode))

(defun memory-context--check-source (context pathname)
  "Resolve PATHNAME and reject sources outside the trusted roots."
  (let ((resolved (ls-compat.posix:resolve-pathname pathname)))
    (unless
        (some
         (lambda (root)
           (ls-compat.posix:pathname-within-p resolved (ls-compat.posix:resolve-pathname root)))
         (memory-context-source-roots context))
      (error 'memory-policy-error :pathname pathname))
    resolved))

;; The source policy is defined in memory-sources.lisp.
(declaim (special *memory-source-file-byte-limit*))

(defmethod memory-context-read-file ((context memory-store-context) pathname)
  "Read the source under the trusted context policy."
  (ls-compat.files:read-file-text (memory-context--check-source context pathname) :maximum-octets
                                  *memory-source-file-byte-limit* :follow-links-p t))

(defmethod memory-context-file-status ((context memory-store-context) pathname)
  "Read the source under the trusted context policy."
  (ls-compat.posix:file-information (memory-context--check-source context pathname) :follow-links-p
                                    t))
