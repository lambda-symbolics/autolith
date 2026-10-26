(in-package #:autolith)

;;;; -- Source Rules --

;;; A rule inspects one source file and reports findings at lines and
;;; columns. A rule that can repair what it finds returns the repaired text;
;;; a layout rule may change only whitespace outside string literals, which
;;; the runner verifies before it writes anything.

(defclass source-file ()
  ((path
    :initarg       :path
    :reader        source-file-path
    :type          string
    :documentation "The file's path relative to the repository root.")
   (text
    :initarg       :text
    :reader        source-file-text
    :type          string
    :documentation "The file's complete text.")
   (lines
    :reader        source-file--lines
    :documentation "The file's lines as a vector, computed when first needed.")
   (offsets
    :reader        source-file--offsets
    :documentation "Each line's starting character offset, computed when first needed.")
   (nodes
    :reader        source-file--nodes
    :documentation "The file's top-level source nodes, computed when first needed.")
   (comments
    :reader        source-file--comments
    :documentation "The file's comments, computed when first needed."))
  (:documentation "One repository file's text and its parsed layout."))

(defclass source-finding ()
  ((rule
    :initarg       :rule
    :reader        source-finding-rule
    :type          string
    :documentation "The name of the rule that reported the finding.")
   (path
    :initarg       :path
    :reader        source-finding-path
    :type          string
    :documentation "The repository-relative path of the file.")
   (line
    :initarg       :line
    :reader        source-finding-line
    :type          (integer 1)
    :documentation "The line the finding is about.")
   (column
    :initarg       :column
    :initform      0
    :reader        source-finding-column
    :type          (integer 0)
    :documentation "The column the finding is about.")
   (message
    :initarg       :message
    :reader        source-finding-message
    :type          string
    :documentation "What is wrong, in one sentence."))
  (:documentation "One rule violation at one place in one file."))

(defclass source-rule ()
  ((name
    :initarg       :name
    :reader        source-rule-name
    :type          string
    :documentation "The rule's short name, shown with each finding.")
   (description
    :initarg       :description
    :reader        source-rule-description
    :type          string
    :documentation "What the rule requires, citing AGENTS.md where it comes from."))
  (:documentation "A repository source rule."))


;;;; -- Rule Protocol --

(defgeneric source-rule-applies-p (rule file)
  (:documentation "Return true when RULE inspects FILE. By default rules inspect Lisp source."))

(defgeneric source-rule-prepare (rule file)
  (:documentation "Let RULE note what FILE contributes before it judges any file, for rules spanning files."))

(defgeneric source-rule-findings (rule file)
  (:documentation "Return RULE's findings in FILE, in order."))

(defgeneric source-rule-fix (rule file)
  (:documentation "Return FILE's text with RULE's findings repaired, or NIL when RULE cannot repair them."))

(defgeneric source-rule-layout-only-p (rule)
  (:documentation "Return true when RULE's repairs change only whitespace outside string literals."))

(defmethod source-rule-applies-p :around ((rule source-rule) (file source-file))
  "Exclude the checked schema compiler output from hand-written source rules."
  (and (not (source-file-generated-p file))
       (call-next-method)))

(defmethod source-rule-applies-p ((rule source-rule) (file source-file))
  "Inspect Lisp source and system definitions."
  (declare (ignore rule))
  (source-file-lisp-p file))

(defmethod source-rule-prepare ((rule source-rule) (file source-file))
  "Judge each file on its own."
  (declare (ignore rule file))
  nil)

(defmethod source-rule-fix ((rule source-rule) (file source-file))
  "Repair nothing."
  (declare (ignore rule file))
  nil)

(defmethod source-rule-layout-only-p ((rule source-rule))
  "Assume a repair changes layout only."
  (declare (ignore rule))
  t)


;;;; -- Source Files --

(-> source-file-create (string string) source-file)
(defun source-file-create (path text)
  "Return a source file at repository-relative PATH holding TEXT."
  (make-instance 'source-file :path path :text text))

(-> source-file-lisp-p (source-file) boolean)
(defun source-file-lisp-p (file)
  "Return true when FILE is Lisp source or a system definition."
  (let ((type (pathname-type (source-file-path file))))
    (and (member type '("lisp" "asd") :test #'equal) t)))

(-> source-file-generated-p (source-file) boolean)
(defun source-file-generated-p (file)
  "Return true for the two checked Protobuf binding files, which ASDF still compiles."
  (and (member (source-file-path file)
               '("proto/generated/value.lisp" "proto/generated/protocol.lisp")
               :test #'equal)
       t))

(-> source-file-lines (source-file) simple-vector)
(defun source-file-lines (file)
  "Return FILE's lines; the text after the last newline is the last line."
  (unless (slot-boundp file 'lines)
    (setf (slot-value file 'lines)
          (coerce (uiop:split-string (source-file-text file) :separator '(#\Newline))
                  'simple-vector)))
  (source-file--lines file))

(-> source-file-release (source-file) null)
(defun source-file-release (file)
  "Forget FILE's lines and parsed layout, keeping its text, so a run over many files stays small."
  (dolist (slot '(lines offsets nodes comments))
    (slot-makunbound file slot))
  nil)

(-> source-file-line (source-file (integer 1)) string)
(defun source-file-line (file line)
  "Return line number LINE of FILE, counting from one."
  (aref (source-file-lines file) (1- line)))

(-> source-file-nodes (source-file) list)
(defun source-file-nodes (file)
  "Return FILE's top-level source nodes."
  (source-file--parse file)
  (source-file--nodes file))

(-> source-file-comments (source-file) list)
(defun source-file-comments (file)
  "Return FILE's comments in order."
  (source-file--parse file)
  (source-file--comments file))

(-> source-file-string-lines (source-file) hash-table)
(defun source-file-string-lines (file)
  "Return the set of lines that start inside a string literal or block comment.

Their leading whitespace is content, so layout repairs never change it."
  (let ((lines (make-hash-table)))
    (dolist (node (source-file-nodes file))
      (source-node-walk node
                        (lambda (node)
                          (when (source-node-string-p node)
                            (loop for line from (1+ (source-node-line node))
                                    to (source-node-end-line node)
                                  do (setf (gethash line lines) t))))))
    (dolist (comment (source-file-comments file))
      (loop for line from (1+ (source-comment-line comment))
              to (source-comment-end-line comment)
            do (setf (gethash line lines) t)))
    lines))

(-> source-file-line-offset (source-file (integer 1)) (integer 0))
(defun source-file-line-offset (file line)
  "Return the character offset where LINE of FILE starts."
  (unless (slot-boundp file 'offsets)
    (let ((offset 0))
      (setf (slot-value file 'offsets)
            (map 'simple-vector
                 (lambda (text)
                   (prog1 offset
                     (incf offset (1+ (length text)))))
                 (source-file-lines file)))))
  (aref (source-file--offsets file) (1- line)))

(-> source-finding-create
    (source-rule source-file &key (:line (integer 1)) (:message string) (:column (integer 0)))
    source-finding)
(defun source-finding-create (rule file &key line message (column 0))
  "Return RULE's finding about LINE of FILE described by MESSAGE."
  (make-instance 'source-finding :rule    (source-rule-name rule)
                                 :path    (source-file-path file)
                                 :line    line
                                 :column  column
                                 :message message))


;;;; -- Edits --

;;; An edit is (START END REPLACEMENT): the text between offsets START and END
;;; becomes REPLACEMENT. A repair collects edits against one text and applies
;;; them together, keeping only edits whose lines do not overlap an earlier
;;; one; the runner applies the rule again for what remains.

(-> source-edits-apply (string list) string)
(defun source-edits-apply (text edits)
  "Return TEXT with non-overlapping EDITS applied."
  (let ((result text))
    (dolist (edit (sort (copy-list edits) #'> :key #'first) result)
      (destructuring-bind (start end replacement) edit
        (setf result (concatenate 'string
                                  (subseq result 0 start)
                                  replacement
                                  (subseq result end)))))))

(-> source-shift-edits
    (source-file &key (:first-line (integer 1)) (:last-line (integer 1)) (:delta integer)
                      (:protected hash-table))
    list)
(defun source-shift-edits (file &key first-line last-line delta protected)
  "Return edits moving the lines FIRST-LINE to LAST-LINE of FILE right by DELTA columns.

Blank lines and lines in PROTECTED, those starting inside strings, are left
alone, and no line moves left of its first column."
  (loop for line from first-line to last-line
        for text = (source-file-line file line)
        for indentation = (or (position #\Space text :test-not #'char=) (length text))
        for offset = (source-file-line-offset file line)
        unless (or (zerop delta)
                   (= indentation (length text))
                   (and protected (gethash line protected)))
          collect (list offset (+ offset indentation)
                        (make-string (max 0 (+ indentation delta))
                                     :initial-element #\Space))))

(-> source-edit-group-lines (list) list)
(defun source-edit-group-lines (group)
  "Return the (FIRST LAST) lines touched by edit GROUP, a plist with :FIRST-LINE and :LAST-LINE."
  (list (getf group :first-line) (getf group :last-line)))

(-> source-edit-groups-apply (string list) string)
(defun source-edit-groups-apply (text groups)
  "Apply the edit GROUPS to TEXT, skipping a group whose lines overlap one already taken.

Each group is a plist of :FIRST-LINE, :LAST-LINE, and :EDITS."
  (let ((taken nil)
        (edits nil))
    (dolist (group groups)
      (destructuring-bind (first last) (source-edit-group-lines group)
        (unless (some (lambda (range)
                        (and (<= first (second range)) (<= (first range) last)))
                      taken)
          (push (list first last) taken)
          (setf edits (append (getf group :edits) edits)))))
    (source-edits-apply text edits)))


;;;; -- Private Functions --

(-> source-file--parse (source-file) null)
(defun source-file--parse (file)
  "Parse FILE's text once, recording its nodes and comments."
  (unless (slot-boundp file 'nodes)
    (multiple-value-bind (nodes comments) (source-parse (source-file-text file))
      (setf (slot-value file 'nodes) nodes
            (slot-value file 'comments) comments)))
  nil)
