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

(deftype memory-visibility ()
  "The subset of persistent memories selected for one operation."
  '(member :relevant :global :workspace :all))

(deftype tool-conversation-persistence ()
  "The lifetime of one tool call and its correlated provider result."
  '(member :durable :next-response))

(-> non-empty-string-p (t) boolean)
(defun non-empty-string-p (value)
  "Return true when VALUE is a string containing a non-whitespace character."
  (and (stringp value)
       (not (every (lambda (character)
                     (find character
                           '(#\Space #\Tab #\Newline #\Return #\Page)))
                   value))))

(deftype non-empty-string ()
  "A string containing at least one non-whitespace character."
  '(satisfies non-empty-string-p))

(-> json-object-p (t) boolean)
(defun json-object-p (value)
  "Return true when VALUE is a string-keyed hash table suitable for JSON."
  (and (hash-table-p value)
       (loop for key being the hash-keys of value
             always (stringp key))))

(deftype json-object ()
  "A string-keyed hash table representing a JSON object."
  '(satisfies json-object-p))

(deftype json-value ()
  "A value accepted by Autolith's JSON codec."
  '(or null string number symbol vector list json-object))
