(in-package #:autolith)

;;;; -- Fundamental Types --

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))

(deftype timestamp ()
  "A Common Lisp universal-time timestamp."
  '(integer 0))

(deftype memory-scope ()
  "The global or workspace-local reach of one persistent memory."
  '(member :global :workspace))

(deftype prose-memory-scope ()
  "The shared or participant-personal reach of one prose memory."
  '(member :personal :global :workspace))

(deftype memory-visibility ()
  "The subset of persistent memories selected for one operation."
  '(member :personal :relevant :global :workspace :all))

(deftype tool-conversation-persistence ()
  "The lifetime of one tool call and its correlated provider result."
  '(member :durable :next-response))

(defparameter *line-whitespace*
  (remove-if-not (lambda (character)
                   (member character '(#\Space #\Tab #\Newline #\Return)))
                 serapeum:whitespace)
  "Characters used to trim text at line-oriented protocol boundaries.")

(-> non-empty-string-p (t) boolean)
(defun non-empty-string-p (value)
  "Return true when VALUE is a string containing a non-whitespace character."
  (and (stringp value)
       ;; Retain the persisted string contract for VT and NBSP.
       (or (not (serapeum:blankp value))
           (and (position-if (lambda (character)
                              (or (char= character #\Vt)
                                  (char= character (code-char #xa0))))
                            value)
                t))))

(deftype non-empty-string ()
  "A string containing at least one non-whitespace character."
  '(satisfies non-empty-string-p))
