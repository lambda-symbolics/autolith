(in-package #:autolith)

;;;; -- Lisp Source Layout --

;;; Compact Lisp written on one line is laid out the way an editor would
;;; indent it: one top-level form per line, bindings and bodies on lines of
;;; their own, arguments aligned. Only whitespace between tokens changes. The
;;; text of every token, string, comment and reader prefix is copied as
;;; written, because the source is displayed, never read.
;;;
;;; The source is parsed into elements. An element is a list of adjacent
;;; pieces: a token's text, or (:LIST . ELEMENTS) for a parenthesized list.
;;; So '#(a b) is one element of two pieces, the prefix '# and the list.

(defparameter *lisp-layout-operators*
  '(("let" :body :count 1 :rows 1) ("let*" :body :count 1 :rows 1)
    ("flet" :body :count 1 :rows 1) ("labels" :body :count 1 :rows 1)
    ("macrolet" :body :count 1 :rows 1) ("symbol-macrolet" :body :count 1 :rows 1)
    ("handler-bind" :body :count 1 :rows 1) ("restart-bind" :body :count 1 :rows 1)
    ("defclass" :body :count 2 :rows 3) ("define-condition" :body :count 2 :rows 3)
    ("defstruct" :body :count 1) ("defpackage" :body :count 1)
    ("defmethod" :body :count :lambda-list)
    ("multiple-value-bind" :body :count 2) ("destructuring-bind" :body :count 2)
    ("do" :body :count 2) ("do*" :body :count 2)
    ("dolist" :body :count 1) ("dotimes" :body :count 1)
    ("handler-case" :body :count 1) ("restart-case" :body :count 1)
    ("unwind-protect" :body :count 1)
    ("case" :body :count 1) ("ecase" :body :count 1) ("ccase" :body :count 1)
    ("typecase" :body :count 1) ("etypecase" :body :count 1) ("ctypecase" :body :count 1)
    ("block" :body :count 1) ("eval-when" :body :count 1) ("prog1" :body :count 1)
    ("progn" :body :count 0) ("tagbody" :body :count 0)
    ("lambda" :block :count 1) ("when" :block :count 1) ("unless" :block :count 1)
    ("if" :aligned) ("cond" :clauses) ("loop" :loop)
    ("setf" :pairs) ("setq" :pairs) ("psetf" :pairs) ("psetq" :pairs))
  "The layout style of each operator's forms, with the style's arguments.

A :BODY form keeps :COUNT arguments beside its operator and gives every later
one a line of its own; :ROWS names the argument whose elements each take a
line, as bindings do. A :BLOCK form breaks its body only when it does not fit.")

(defparameter *lisp-layout-loop-clauses*
  '("for" "as" "with" "repeat" "while" "until" "when" "unless" "if" "else" "do" "doing"
    "collect" "collecting" "append" "appending" "nconc" "nconcing" "sum" "summing"
    "count" "counting" "maximize" "maximizing" "minimize" "minimizing"
    "always" "never" "thereis" "initially" "finally" "return")
  "The loop keywords that start a clause on a line of its own.")

(defgeneric lisp-layout--write-form (style stream elements &key count rows)
  (:documentation "Write the list ELEMENTS to pretty STREAM in layout STYLE.

COUNT is the number of arguments kept beside the operator and ROWS the index
of the argument written one element per line, for the styles that have them."))


;;;; -- Parsing --

(-> lisp-layout--closings (string simple-vector) (option simple-vector))
(defun lisp-layout--closings (source roles)
  "Return each opening parenthesis's closing position in SOURCE, or NIL when they do not balance."
  (let ((closings (make-array (length source) :initial-element nil))
        (stack    nil))
    (loop for index below (length source)
          when (aref roles index)
            do (cond
                 ((char= (char source index) #\()
                  (push index stack))
                 (stack
                  (setf (aref closings (pop stack)) index))
                 (t
                  (return-from lisp-layout--closings nil))))
    (unless stack
      closings)))

(-> lisp-layout--elements
    (string &key (:start integer) (:end integer) (:closings simple-vector)
            (:whitespace simple-vector))
    list)
(defun lisp-layout--elements (source &key start end closings whitespace)
  "Return the elements of SOURCE between START and END."
  (let ((elements nil)
        (pieces   nil)
        (index    start))
    (flet ((finish ()
             (when pieces
               (push (nreverse pieces) elements)
               (setf pieces nil))))
      (loop while (< index end)
            do (cond
                 ((aref whitespace index)
                  (finish)
                  (incf index))
                 ((aref closings index)
                  (let ((closing (aref closings index)))
                    (push (cons ':list
                                (lisp-layout--elements source
                                                       :start      (1+ index)
                                                       :end        closing
                                                       :closings   closings
                                                       :whitespace whitespace))
                          pieces)
                    (setf index (1+ closing))))
                 (t
                  (let ((next (loop for position from (1+ index) below end
                                    when (or (aref whitespace position)
                                             (aref closings position))
                                      return position
                                    finally (return end))))
                    (push (subseq source index next) pieces)
                    (setf index next)))))
      (finish))
    (nreverse elements)))

(-> lisp-layout--token (list) (option string))
(defun lisp-layout--token (element)
  "Return ELEMENT's text when it is one token, or NIL for a list or a prefixed list."
  (and (null (rest element))
       (stringp (first element))
       (first element)))

(-> lisp-layout--name (list) (option string))
(defun lisp-layout--name (element)
  "Return the lower-case symbol name ELEMENT's token writes, without its package, or NIL."
  (let ((token (lisp-layout--token element)))
    (when token
      (string-downcase (subseq token (1+ (or (position #\: token :from-end t) -1)))))))

(-> lisp-layout--code-p (list) boolean)
(defun lisp-layout--code-p (element)
  "Return true when ELEMENT's list is a form: bare, in a template, or a function designator.

A quoted list, a vector and a read-time value are data, laid out as a plain sequence."
  (let ((prefix (apply #'concatenate 'string (remove-if-not #'stringp element))))
    (or (string= prefix "#'")
        (every (lambda (character) (find character "`,@")) prefix))))

(-> lisp-layout--style ((option string)) list)
(defun lisp-layout--style (name)
  "Return the layout style and its arguments for a form whose operator is NAME."
  (let ((known (and name (rest (assoc name *lisp-layout-operators* :test #'string=)))))
    (cond
      ((null name)
       (list ':sequence))
      (known
       known)
      ((string-prefix-p "def" name)
       (list ':body :count 2))
      ((or (string-prefix-p "with-" name) (string-prefix-p "do-" name))
       (list ':body :count 1))
      (t
       (list ':call)))))


;;;; -- Writing --

(-> lisp-layout--break (stream list keyword) null)
(defun lisp-layout--break (stream previous kind)
  "Write the space after element PREVIOUS and a newline of KIND to pretty STREAM.

The pretty printer drops the blanks before a line break, so no break follows a
token that ends in one, as the character literal for a space does."
  (write-char #\Space stream)
  (let ((last (first (last previous))))
    (unless (and (stringp last) (char= (char last (1- (length last))) #\Space))
      (pprint-newline kind stream)))
  nil)

(-> lisp-layout--write-element (stream list &key (:rows-p boolean)) null)
(defun lisp-layout--write-element (stream element &key rows-p)
  "Write ELEMENT's pieces to pretty STREAM, its list one element per line with ROWS-P."
  (dolist (piece element)
    (if (stringp piece)
        (write-string piece stream)
        (pprint-logical-block (stream nil :prefix "(" :suffix ")")
          (let ((elements (rest piece)))
            (cond
              ((null elements)
               nil)
              (rows-p
               (lisp-layout--write-sequence stream elements ':mandatory))
              ((lisp-layout--code-p element)
               (destructuring-bind (style &rest arguments)
                   (lisp-layout--style (lisp-layout--name (first elements)))
                 (apply #'lisp-layout--write-form style stream elements arguments)))
              (t
               (lisp-layout--write-sequence stream elements ':fill)))))))
  nil)

(-> lisp-layout--write-sequence (stream list (or keyword function)) null)
(defun lisp-layout--write-sequence (stream elements kind)
  "Write ELEMENTS to pretty STREAM, separated by spaces and newlines of KIND.

KIND may be a function of the next element's index and the element, returning the kind."
  (loop for (element . more) on elements
        for index from 1
        do (lisp-layout--write-element stream element)
           (when more
             (lisp-layout--break stream element
                                 (if (functionp kind)
                                     (funcall kind index (first more))
                                     kind))))
  nil)

(-> lisp-layout--write-aligned (stream list (or keyword function)) null)
(defun lisp-layout--write-aligned (stream elements kind)
  "Write the operator, then its arguments under the first one with newlines of KIND."
  (lisp-layout--write-element stream (first elements))
  (when (rest elements)
    (write-char #\Space stream)
    (pprint-indent ':current 0 stream)
    (lisp-layout--write-sequence stream (rest elements) kind))
  nil)

(defmethod lisp-layout--write-form ((style (eql :sequence)) stream elements &key count rows)
  "Write ELEMENTS, which no operator leads, as a filled sequence."
  (declare (ignore count rows))
  (lisp-layout--write-sequence stream elements ':fill))

(defmethod lisp-layout--write-form ((style (eql :call)) stream elements &key count rows)
  "Write a call with its arguments filled under the first one."
  (declare (ignore count rows))
  (lisp-layout--write-aligned stream elements ':fill))

(defmethod lisp-layout--write-form ((style (eql :aligned)) stream elements &key count rows)
  "Write a form whose arguments all break under the first one when it does not fit."
  (declare (ignore count rows))
  (lisp-layout--write-aligned stream elements ':linear))

(defmethod lisp-layout--write-form ((style (eql :pairs)) stream elements &key count rows)
  "Write places and values two to a line, under the first place."
  (declare (ignore count rows))
  (lisp-layout--write-aligned stream elements
                              (lambda (index next)
                                (declare (ignore next))
                                (if (evenp index) ':mandatory ':fill))))

(defmethod lisp-layout--write-form ((style (eql :loop)) stream elements &key count rows)
  "Write a loop with each clause on a line under the first one."
  (declare (ignore count rows))
  (lisp-layout--write-aligned stream elements
                              (lambda (index next)
                                (declare (ignore index))
                                (if (member (lisp-layout--name next) *lisp-layout-loop-clauses*
                                            :test #'equal)
                                    ':mandatory
                                    ':fill))))

(defmethod lisp-layout--write-form ((style (eql :clauses)) stream elements &key count rows)
  "Write a form whose arguments are clauses, each on a line under the first one.

A clause's test and body forms all break when the clause does not fit."
  (declare (ignore count rows))
  (lisp-layout--write-element stream (first elements))
  (when (rest elements)
    (write-char #\Space stream)
    (pprint-indent ':current 0 stream)
    (loop for (clause . more) on (rest elements)
          do (dolist (piece clause)
               (if (stringp piece)
                   (write-string piece stream)
                   (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                     (lisp-layout--write-sequence stream (rest piece) ':linear))))
             (when more
               (lisp-layout--break stream clause ':mandatory)))))

(defmethod lisp-layout--write-form ((style (eql :body)) stream elements &key count rows)
  "Write COUNT arguments beside the operator and every body form on a line of its own."
  (lisp-layout--write-body stream elements ':mandatory :count count :rows rows))

(defmethod lisp-layout--write-form ((style (eql :block)) stream elements &key count rows)
  "Write COUNT arguments beside the operator and break the body only when it does not fit."
  (lisp-layout--write-body stream elements ':linear :count count :rows rows))

(-> lisp-layout--write-body
    (stream list keyword &key (:count (or integer keyword)) (:rows (option integer)))
    null)
(defun lisp-layout--write-body (stream elements kind &key count rows)
  "Write the operator and COUNT arguments, then the body with newlines of KIND.

The first of the COUNT arguments always stays beside the operator. A COUNT of
:LAMBDA-LIST keeps the arguments up to the first list after the name, so a
method's qualifiers stay with it. The argument at index ROWS is written one
element per line."
  (let ((count (if (eq count ':lambda-list)
                   (1+ (or (position-if-not #'lisp-layout--token (rest elements) :start 1) 1))
                   count)))
    (lisp-layout--write-element stream (first elements))
    (loop for (previous element) on elements
          for index from 1
          while element
          do (cond
               ((> index count)
                (pprint-indent ':block 1 stream)
                (lisp-layout--break stream previous kind))
               ((> index 1)
                (pprint-indent ':block 3 stream)
                (lisp-layout--break stream previous ':fill))
               (t
                (write-char #\Space stream)))
             (lisp-layout--write-element stream element :rows-p (eql index rows))))
  nil)


;;;; -- Layout --

(-> lisp-source-layout (string (integer 1)) string)
(defun lisp-source-layout (source columns)
  "Lay out one-line Lisp SOURCE within COLUMNS without reading or changing its tokens.

Each top-level form starts a line; a trailing line comment stays on its form's
line. Source that already has line breaks, and source whose parentheses do not
balance, is returned as it is."
  (when (find-if (lambda (character) (find character '(#\Newline #\Return))) source)
    (return-from lisp-source-layout source))
  (multiple-value-bind (roles whitespace)
      (syntax--lisp-parenthesis-roles source :whitespace-p t)
    (let ((closings (lisp-layout--closings source roles)))
      (unless closings
        (return-from lisp-source-layout source))
      (let ((*print-pretty*       t)
            (*print-right-margin* columns)
            (*print-miser-width*  nil)
            (*print-lines*        nil))
        (with-output-to-string (stream)
          (pprint-logical-block (stream nil)
            (loop for (element . more) on (lisp-layout--elements source
                                                                 :start      0
                                                                 :end        (length source)
                                                                 :closings   closings
                                                                 :whitespace whitespace)
                  do (lisp-layout--write-element stream element)
                     (when more
                       (if (string-prefix-p ";" (or (lisp-layout--token (first more)) ""))
                           (write-char #\Space stream)
                           (lisp-layout--break stream element ':mandatory))))))))))
