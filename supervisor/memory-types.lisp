(in-package #:autolith-supervisor)

(deftype timestamp () "A Common Lisp universal-time timestamp." '(integer 0))

(deftype memory-scope ()
  "The global or workspace-local reach of one persistent memory."
  '(member :global :workspace))

(deftype memory-visibility ()
  "The subset of persistent memories selected for one operation."
  '(member :relevant :global :workspace :all))

(serapeum/types:-> non-empty-string-p (t) boolean)
(defun non-empty-string-p (value)
  "Return true when VALUE is a string containing a non-whitespace character."
  (and (stringp value)
       (not
        (every (lambda (character) (find character '(#\  #\Tab #\Newline #\Return #\Page)))
               value))))

(deftype non-empty-string ()
  "A string containing at least one non-whitespace character."
  '(satisfies non-empty-string-p))

(define-condition memory-error
    (autolith-error)
    ((pathname :initarg :pathname :reader memory-error-pathname :type pathname :documentation
               "The persistent memory file being processed.")
     (identifier :initarg :identifier :initform nil :reader memory-error-identifier :type
      (option string) :documentation "The memory identifier involved in the failure, when known."))
  (:documentation "Persistent memory data is invalid or cannot be updated."))

(define-condition knowledge-error
    (autolith-error)
    ((code :initarg :code :initform ':invalid :reader knowledge-error-code :type keyword
      :documentation "A stable reason callers and tools can distinguish.")
     (subject :initarg :subject :initform nil :reader knowledge-error-subject :documentation
      "The identifier, predicate, or value involved, when known."))
  (:documentation "Structured knowledge, or an operation on it, is invalid."))
