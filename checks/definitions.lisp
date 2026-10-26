(in-package #:autolith)

;;;; -- Definition Rules --

;;; These rules hold what AGENTS.md asks of definitions: documentation, ->
;;; declarations for functions and none for generic functions, keyword
;;; arguments past three positional parameters, first and rest instead of car
;;; and cdr, % in format controls, no constants, no TODOs, and one package.

(defparameter *source-typed-directories* '("src/" "tests/" "checks/" "server/")
  "Directories whose functions declare their types with ->.

Scripts run in CL-USER before Serapeum loads. Recovery and the supervisor
load without it.")

(defparameter *source-package-directories* '("src/" "tests/" "checks/" "server/" "supervisor/")
  "Directories whose system files belong to the agent or supervisor package.")

(defparameter *source-package-definition-path* "src/core/package.lisp"
  "The one file that defines the project package.")

(defparameter *source-list-accessor-replacements*
  '(("car" . "first") ("cdr" . "rest") ("cadr" . "second") ("caddr" . "third")
    ("cadddr" . "fourth"))
  "List accessors and the readable names that replace them.")

(defparameter *source-list-accessors*
  '("car" "cdr" "caar" "cadr" "cdar" "cddr" "caaar" "caadr" "cadar" "caddr" "cdaar"
    "cdadr" "cddar" "cdddr" "cadddr" "cddddr")
  "The cons accessors application code replaces with first, rest, and their relatives.")

(defparameter *source-format-operators* '("format" "error" "warn" "cerror" "formatter")
  "Operators whose literal control string follows the destination or comes first.")

(defvar *source-check-system-paths* nil
  "The repository-relative paths of the systems' source files, or NIL to judge by directory.

The runner binds it so rules about project source skip standalone scripts.")

(defparameter *source-positional-limit* 3
  "The most required and optional parameters a function or method takes before keywords.")

(defclass source-documentation-rule (source-rule)
  ()
  (:default-initargs
   :name "documentation"
   :description "Give functions, macros, methods, generic functions, classes, slots, and conditions documentation (AGENTS.md, Types and Documentation).")
  (:documentation "Definitions must carry documentation."))

(defclass source-generic-declaration-rule (source-rule)
  ()
  (:default-initargs
   :name "generic-declaration"
   :description "Generic functions take no -> declaration; SBCL discards it with a warning.")
  (:documentation "No -> declaration names a generic function."))

(defclass source-function-declaration-rule (source-rule)
  ()
  (:default-initargs
   :name "function-declaration"
   :description "Declare each function's type with -> (AGENTS.md, Types and Documentation).")
  (:documentation "Every top-level function has a -> declaration in its file."))

(defclass source-declaration-uniqueness-rule (source-rule)
  ((declarations
    :initform      (make-hash-table :test #'equal)
    :reader        source-declaration-uniqueness-rule-declarations
    :type          hash-table
    :documentation "Each declared function name's (PATH LINE) places across the repository."))
  (:default-initargs
   :name "declaration-uniqueness"
   :description "Declare each function's type once, where it is defined; a second declaration makes SBCL warn about the mismatch.")
  (:documentation "No function has two -> declarations."))

(defclass source-positional-parameters-rule (source-rule)
  ((specialized
    :initform      (make-hash-table :test #'equal)
    :reader        source-positional-parameters-rule-specialized
    :type          hash-table
    :documentation "Each generic function name's number of leading positions some method specializes."))
  (:default-initargs
   :name "positional-parameters"
   :description "Functions and methods with four or more parameters use keyword arguments (AGENTS.md, Naming).")
  (:documentation "At most three required and optional parameters.

A method's parameters follow its generic function, so the rule judges the
generic function, not each method. A parameter some method specializes must
stay positional, so a generic function may take as many positional
parameters as its methods dispatch on."))

(defclass source-list-accessor-rule (source-rule)
  ()
  (:default-initargs
   :name "list-accessor"
   :description "Prefer first and rest over car and cdr in application code (AGENTS.md, Naming).")
  (:documentation "Cons accessors are written first, rest, second, and so on."))

(defclass source-format-percent-rule (source-rule)
  ()
  (:default-initargs
   :name "format-percent"
   :description "A literal percent sign in a format control string is %, not %% (AGENTS.md, Formatting).")
  (:documentation "Format control strings do not double percent signs."))

(defclass source-constant-rule (source-rule)
  ()
  (:default-initargs
   :name "constant"
   :description "Do not use defconstant or define-constant (AGENTS.md, Naming).")
  (:documentation "No constant definitions."))

(defclass source-todo-rule (source-rule)
  ()
  (:default-initargs
   :name "todo"
   :description "Do not leave TODOs or FIXMEs (AGENTS.md).")
  (:documentation "Comments do not leave work for later."))

(defclass source-package-rule (source-rule)
  ()
  (:default-initargs
   :name "package"
   :description "One package per process component, defined once (AGENTS.md, Package Policy).")
  (:documentation "The agent and supervisor package policy."))


;;;; -- Documentation --

(defmethod source-rule-findings ((rule source-documentation-rule) (file source-file))
  "Report undocumented functions, macros, methods, generic functions, classes, slots, and conditions."
  (let ((findings nil))
    (flet ((report (node message)
             (push (source-finding-create rule file
                                          :line    (source-node-line node)
                                          :message message
                                          :column  (source-node-column node))
                   findings)))
      (dolist (node (source-definition-forms file))
        (let ((operator (source-node-operator node))
              (name     (source-definition-name node)))
          (cond
            ((member operator '("defun" "defmacro" "defmethod") :test #'equal)
             (unless (source-definition--documented-body-p node)
               (report node (format nil "~A ~A has no documentation string." operator name))))
            ((equal operator "defgeneric")
             (unless (source-definition--option node ":documentation")
               (report node (format nil "Generic function ~A has no :documentation." name))))
            ((member operator '("defclass" "define-condition") :test #'equal)
             (unless (source-definition--option node ":documentation")
               (report node (format nil "~A has no :documentation." name)))
             (let ((slots (fourth (source-node-children node))))
               (when (source-node-list-p slots)
                 (dolist (slot (source-node-children slots))
                   (unless (and (source-node-list-p slot)
                                (source-definition--plist-value slot ":documentation"))
                     (report slot (format nil "Slot ~A of ~A has no :documentation."
                                          (source-definition-name-of slot) name))))))))))
      (nreverse findings))))


;;;; -- Function Declarations --

(defmethod source-rule-applies-p ((rule source-generic-declaration-rule) (file source-file))
  "Inspect project files that use -> declarations."
  (declare (ignore rule))
  (source-file-typed-p file))

(defmethod source-rule-findings ((rule source-generic-declaration-rule) (file source-file))
  "Report -> declarations naming a generic function defined in the same file."
  (mapcar (lambda (node)
            (source-finding-create rule file
                                   :line    (source-node-line node)
                                   :message (format nil "Generic function ~A takes no -> declaration."
                                             (source-definition-name node))))
          (source-declaration--generic file)))

(defmethod source-rule-fix ((rule source-generic-declaration-rule) (file source-file))
  "Delete -> declarations of generic functions with the lines they occupy."
  (declare (ignore rule))
  (source-edits-apply
   (source-file-text file)
   (loop for node in (source-declaration--generic file)
         for start = (source-file-line-offset file (source-node-prefix-line node))
         for end = (+ (source-file-line-offset file (source-node-end-line node))
                      (length (source-file-line file (source-node-end-line node))))
         when (and (source-text--blank-between-p file start (source-node-prefix-start node))
                   (source-text--blank-between-p file (source-node-end node) end))
           collect (list start (min (length (source-file-text file)) (1+ end)) ""))))

(defmethod source-rule-layout-only-p ((rule source-generic-declaration-rule))
  "Removing a declaration changes the forms."
  (declare (ignore rule))
  nil)

(defmethod source-rule-applies-p ((rule source-function-declaration-rule) (file source-file))
  "Inspect project files that use -> declarations."
  (declare (ignore rule))
  (source-file-typed-p file))

(defmethod source-rule-findings ((rule source-function-declaration-rule) (file source-file))
  "Report top-level functions without a -> declaration in their file."
  (let ((declared (source-declaration--names file)))
    (loop for node in (source-definition-forms file)
          for name = (source-definition-name node)
          when (and (equal (source-node-operator node) "defun")
                    name
                    (not (gethash name declared)))
            collect (source-finding-create rule file
                                           :line    (source-node-line node)
                                           :message (format nil "Function ~A has no -> declaration." name)
                                           :column  (source-node-column node)))))

(defmethod source-rule-applies-p ((rule source-declaration-uniqueness-rule) (file source-file))
  "Inspect project files that use -> declarations."
  (declare (ignore rule))
  (source-file-typed-p file))

(defmethod source-rule-prepare ((rule source-declaration-uniqueness-rule) (file source-file))
  "Record where each -> declaration of FILE names a function."
  (loop for (name node) in (source-declaration--named file)
        do (push (list (source-file-path file) (source-node-line node))
                 (gethash name (source-declaration-uniqueness-rule-declarations rule)))))

(defmethod source-rule-findings ((rule source-declaration-uniqueness-rule) (file source-file))
  "Report this file's -> declarations of a function declared elsewhere too."
  (let ((declarations (source-declaration-uniqueness-rule-declarations rule)))
    (loop for (name node) in (source-declaration--named file)
          for places = (gethash name declarations)
          when (rest places)
            collect (source-finding-create
                     rule file
                     :line    (source-node-line node)
                     :message (format nil "~A is declared ~D times: ~{~{~A:~D~}~^, ~}."
                               name (length places) (reverse places))
                     :column  (source-node-column node)))))

(defmethod source-rule-prepare ((rule source-positional-parameters-rule) (file source-file))
  "Record how many leading parameter positions FILE's methods specialize, per generic function."
  (let ((specialized (source-positional-parameters-rule-specialized rule)))
    (dolist (node (source-definition-forms file))
      (dolist (method (source-definition--method-lambda-lists node))
        (destructuring-bind (name lambda-list) method
          (setf (gethash name specialized)
                (max (gethash name specialized 0)
                     (source-definition--specialized-count lambda-list))))))))

(defmethod source-rule-findings ((rule source-positional-parameters-rule) (file source-file))
  "Report functions and generic functions taking more than the limit of positional parameters."
  (loop for node in (source-definition-forms file)
        for operator = (source-node-operator node)
        for parameters = (and (member operator '("defun" "defgeneric") :test #'equal)
                              (source-definition--lambda-list node))
        for count = (and parameters (source-definition--positional-count parameters))
        for allowed = (max *source-positional-limit*
                           (gethash (source-definition-name node)
                                    (source-positional-parameters-rule-specialized rule)
                                    0))
        when (and count (> count allowed))
          collect (source-finding-create
                   rule file
                   :line    (source-node-line node)
                   :message (format nil "~A takes ~D positional parameters; pass the rest as keywords."
                             (source-definition-name node) count)
                   :column  (source-node-column node))))


;;;; -- Expressions --

(defmethod source-rule-findings ((rule source-list-accessor-rule) (file source-file))
  "Report calls and function references of cons accessors."
  (mapcar (lambda (node)
            (source-finding-create
             rule file
             :line    (source-node-line node)
             :message (let ((replacement (source-expression--accessor-replacement node)))
                       (if replacement
                           (format nil "Write ~A instead of ~A." replacement (source-node-text node))
                           (format nil "Write ~A with first, rest, and nth." (source-node-text node))))
             :column  (source-node-column node)))
          (source-expression--accessors file)))

(defmethod source-rule-fix ((rule source-list-accessor-rule) (file source-file))
  "Rename car, cdr, cadr, caddr, and cadddr to first, rest, second, third, and fourth."
  (declare (ignore rule))
  (source-edits-apply
   (source-file-text file)
   (loop for node in (source-expression--accessors file)
         for replacement = (source-expression--accessor-replacement node)
         when replacement
           collect (list (source-node-start node) (source-node-end node) replacement))))

(defmethod source-rule-layout-only-p ((rule source-list-accessor-rule))
  "Renaming an accessor changes the forms."
  (declare (ignore rule))
  nil)

(defmethod source-rule-findings ((rule source-format-percent-rule) (file source-file))
  "Report literal format control strings holding %%."
  (mapcar (lambda (node)
            (source-finding-create rule file
                                   :line    (source-node-line node)
                                   :message "A literal percent sign is written %, not %%."
                                   :column  (source-node-column node)))
          (source-expression--doubled-percent file)))

(defmethod source-rule-fix ((rule source-format-percent-rule) (file source-file))
  "Write each doubled percent sign once."
  (declare (ignore rule))
  (source-edits-apply
   (source-file-text file)
   (loop for node in (source-expression--doubled-percent file)
         collect (list (source-node-start node) (source-node-end node)
                       (source-expression--single-percent (source-node-text node))))))

(defmethod source-rule-layout-only-p ((rule source-format-percent-rule))
  "Changing a control string changes the forms."
  (declare (ignore rule))
  nil)

(defmethod source-rule-findings ((rule source-constant-rule) (file source-file))
  "Report defconstant and define-constant forms."
  (let ((findings nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk top
                        (lambda (node)
                          (when (member (source-node-operator node)
                                        '("defconstant" "define-constant") :test #'equal)
                            (push (source-finding-create
                                   rule file
                                   :line    (source-node-line node)
                                   :message "Use defparameter for policy instead of a constant."
                                   :column  (source-node-column node))
                                  findings)))))
    (nreverse findings)))

(defmethod source-rule-findings ((rule source-todo-rule) (file source-file))
  "Report comments that leave a TODO or FIXME."
  (loop for comment in (source-file-comments file)
        when (source-expression--marker-p (source-comment-text comment))
          collect (source-finding-create rule file
                                         :line    (source-comment-line comment)
                                         :message "Finish the work instead of leaving a TODO or FIXME."
                                         :column  (source-comment-column comment))))

(defmethod source-rule-applies-p ((rule source-package-rule) (file source-file))
  "Inspect project files."
  (declare (ignore rule))
  (and (source-file-lisp-p file)
       (some (lambda (directory) (uiop:string-prefix-p directory (source-file-path file)))
             *source-package-directories*)
       (source-file-system-p file)
       t))

(defmethod source-rule-findings ((rule source-package-rule) (file source-file))
  "Require the package assigned to the agent or the separate supervisor."
  (let* ((findings     nil)
         (first-form   (first (source-file-nodes file)))
         (supervisor-p (uiop:string-prefix-p "supervisor/" (source-file-path file)))
         (package-name (if supervisor-p "autolith-supervisor" "autolith"))
         (definition   (if supervisor-p "supervisor/package.lisp"
                           *source-package-definition-path*))
         (definition-p (equal (source-file-path file) definition)))
    (dolist (top (source-file-nodes file))
      (source-node-walk top
                        (lambda (node)
                          (when (and (member (source-node-operator node)
                                             '("defpackage" "define-package" "make-package")
                                             :test #'equal)
                                     (not definition-p))
                            (push (source-finding-create
                                   rule file
                                   :line    (source-node-line node)
                                   :message "Define the component package only in its package file."
                                   :column  (source-node-column node))
                                  findings)))))
    (unless (or definition-p
                (and first-form
                     (equal (source-node-operator first-form) "in-package")
                     (member (string-downcase
                              (source-node-text (second (source-node-children first-form))))
                             (list (concatenate 'string "#:" package-name)
                                   (concatenate 'string ":" package-name))
                             :test #'equal)))
      (push (source-finding-create rule file :line 1
                                  :message         (format nil "Start the file with (in-package #:~A)."
                                                           package-name))
            findings))
    (nreverse findings)))


;;;; -- Public Functions --

(-> source-file-typed-p (source-file) boolean)
(defun source-file-typed-p (file)
  "Return true when FILE is project Lisp source whose functions declare types with ->."
  (and (source-file-lisp-p file)
       (some (lambda (directory) (uiop:string-prefix-p directory (source-file-path file)))
             *source-typed-directories*)
       (source-file-system-p file)
       t))

(-> source-file-system-p (source-file) boolean)
(defun source-file-system-p (file)
  "Return true when FILE is a source file of the systems, or when no system paths are known."
  (or (null *source-check-system-paths*)
      (and (gethash (source-file-path file) *source-check-system-paths*) t)))

(-> source-definition-forms (source-file) list)
(defun source-definition-forms (file)
  "Return FILE's top-level definition forms, including those inside progn and eval-when."
  (let ((forms nil))
    (labels ((visit (node)
               (let ((operator (source-node-operator node)))
                 (cond
                   ((member operator '("progn" "eval-when") :test #'equal)
                    (dolist (child (if (equal operator "eval-when")
                                       (nthcdr 2 (source-node-children node))
                                       (rest (source-node-children node))))
                      (visit child)))
                   ((and operator (null (source-node-prefixes node)))
                    (push node forms))))))
      (dolist (node (source-file-nodes file))
        (visit node)))
    (nreverse forms)))

(-> source-definition-name (source-node) (option string))
(defun source-definition-name (node)
  "Return the lowercase name a definition NODE defines, or NIL."
  (source-definition-name-of (second (source-node-children node))))

(-> source-definition-name-of (t) (option string))
(defun source-definition-name-of (node)
  "Return the lowercase name of NODE, the name of a (setf name) list, or of a slot list's first element."
  (cond
    ((source-node-atom-p node)
     (string-downcase (source-node-text node)))
    ((source-node-list-p node)
     (let ((first-child (first (source-node-children node))))
       (if (equal (source-node-name first-child) "setf")
           (format nil "(setf ~A)" (string-downcase (source-node-text
                                                     (second (source-node-children node)))))
           (and (source-node-atom-p first-child)
                (string-downcase (source-node-text first-child))))))
    (t
     nil)))


;;;; -- Private Functions --

(-> source-definition--body (source-node) list)
(defun source-definition--body (node)
  "Return the body forms of a defun, defmacro, or defmethod NODE after its lambda list."
  (let ((remaining (nthcdr 2 (source-node-children node))))
    (loop while (and remaining (not (source-node-list-p (first remaining))))
          do (setf remaining (rest remaining)))
    (rest remaining)))

(-> source-definition--lambda-list (source-node) (option source-node))
(defun source-definition--lambda-list (node)
  "Return the lambda list of a defun, defmethod, or defgeneric NODE, after any method qualifiers."
  (find-if #'source-node-list-p (nthcdr 2 (source-node-children node))))

(-> source-definition--positional-count (source-node) (integer 0))
(defun source-definition--positional-count (lambda-list)
  "Return how many required and optional parameters LAMBDA-LIST names."
  (let ((count      0)
        (counting-p t))
    (dolist (parameter (source-node-children lambda-list) count)
      (let ((name (source-node-name parameter)))
        (cond
          ((equal name "&optional")
           (setf counting-p t))
          ((and name (char= (char name 0) #\&))
           (setf counting-p nil))
          (counting-p
           (incf count)))))))

(-> source-definition--specialized-count (source-node) (integer 0))
(defun source-definition--specialized-count (lambda-list)
  "Return how many leading positions of method LAMBDA-LIST reach its last specialized parameter."
  (let ((last 0))
    (loop for parameter in (source-node-children lambda-list)
          for position from 1
          until (let ((name (source-node-name parameter)))
                  (and name (char= (char name 0) #\&)))
          when (source-node-list-p parameter)
            do (setf last position))
    last))

(-> source-definition--method-lambda-lists (source-node) list)
(defun source-definition--method-lambda-lists (node)
  "Return (NAME LAMBDA-LIST) for each method a defmethod or defgeneric NODE defines."
  (let ((operator (source-node-operator node)))
    (cond
      ((equal operator "defmethod")
       (let ((lambda-list (source-definition--lambda-list node)))
         (and lambda-list
              (list (list (source-definition-name node) lambda-list)))))
      ((equal operator "defgeneric")
       (loop for option in (nthcdr 3 (source-node-children node))
             for lambda-list = (and (equal (source-node-operator option) ":method")
                                    (find-if #'source-node-list-p
                                             (rest (source-node-children option))))
             when lambda-list
               collect (list (source-definition-name node) lambda-list))))))

(-> source-definition--documented-body-p (source-node) boolean)
(defun source-definition--documented-body-p (node)
  "Return true when NODE's body starts, after declarations, with a documentation string and more."
  (let ((body (source-definition--body node)))
    (loop while (and body (equal (source-node-operator (first body)) "declare"))
          do (setf body (rest body)))
    (and (source-node-string-p (first body))
         (rest body)
         t)))

(-> source-definition--option (source-node string) (option source-node))
(defun source-definition--option (node keyword)
  "Return the (KEYWORD ...) option list of a defclass, define-condition, or defgeneric NODE."
  (find-if (lambda (child)
             (and (source-node-list-p child)
                  (source-node-keyword-p (first (source-node-children child)))
                  (equal (string-downcase (source-node-text (first (source-node-children child))))
                         keyword)))
           (nthcdr 2 (source-node-children node))))

(-> source-definition--plist-value (source-node string) (option source-node))
(defun source-definition--plist-value (node keyword)
  "Return the value after KEYWORD among list NODE's elements, or NIL."
  (loop for (key value) on (rest (source-node-children node))
        when (and (source-node-keyword-p key)
                  (equal (string-downcase (source-node-text key)) keyword))
          return value))

(-> source-declaration--names (source-file) hash-table)
(defun source-declaration--names (file)
  "Return the set of lowercase function names FILE's -> declarations name."
  (let ((names (make-hash-table :test #'equal)))
    (loop for (name) in (source-declaration--named file)
          do (setf (gethash name names) t))
    names))

(-> source-declaration--named (source-file) list)
(defun source-declaration--named (file)
  "Return (NAME NODE) for each function each of FILE's -> declarations names."
  (loop for node in (source-definition-forms file)
        when (equal (source-node-operator node) "->")
          append (let ((target (second (source-node-children node))))
                   (if (and (source-node-list-p target)
                            (not (equal (source-node-name (first (source-node-children target)))
                                        "setf")))
                       (mapcar (lambda (name) (list (string-downcase (source-node-text name)) node))
                               (source-node-children target))
                       (list (list (source-definition-name-of target) node))))))

(-> source-declaration--generic (source-file) list)
(defun source-declaration--generic (file)
  "Return FILE's top-level -> declarations naming one generic function defined in FILE."
  (let ((generics (make-hash-table :test #'equal)))
    (dolist (node (source-definition-forms file))
      (when (equal (source-node-operator node) "defgeneric")
        (setf (gethash (source-definition-name node) generics) t)))
    (remove-if-not (lambda (node)
                     (and (equal (source-node-operator node) "->")
                          (gethash (source-definition-name node) generics)))
                   (source-file-nodes file))))

(-> source-expression--accessors (source-file) list)
(defun source-expression--accessors (file)
  "Return FILE's atoms calling or naming a cons accessor as a function."
  (let ((found nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (cond
           ((and (source-node-list-p node)
                 (null (source-node-prefixes node))
                 (member (source-node-operator node) *source-list-accessors* :test #'equal))
            (push (first (source-node-children node)) found))
           ((and (source-node-atom-p node)
                 (equal (source-node-prefixes node) '("#'"))
                 (member (string-downcase (source-node-text node)) *source-list-accessors*
                         :test #'equal))
            (push node found))))))
    (remove-duplicates (nreverse found))))

(-> source-expression--accessor-replacement (source-node) (option string))
(defun source-expression--accessor-replacement (node)
  "Return the readable name replacing accessor atom NODE, or NIL when there is none."
  (let ((entry (assoc (string-downcase (source-node-text node))
                      *source-list-accessor-replacements* :test #'equal)))
    (and entry
         (if (every (lambda (character) (or (not (alpha-char-p character)) (upper-case-p character)))
                    (source-node-text node))
             (string-upcase (rest entry))
             (rest entry)))))

(-> source-expression--doubled-percent (source-file) list)
(defun source-expression--doubled-percent (file)
  "Return FILE's literal format control strings holding %%."
  (let ((found nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (let* ((operator (source-node-operator node))
                (control  (cond
                            ((equal operator "format")
                             (third (source-node-children node)))
                            ((member operator *source-format-operators* :test #'equal)
                             (second (source-node-children node))))))
           (when (and (source-node-string-p control)
                      (null (source-node-prefixes control))
                      (source-expression--doubled-percent-positions (source-node-text control)))
             (push control found))))))
    (nreverse found)))

(-> source-expression--single-percent (string) string)
(defun source-expression--single-percent (text)
  "Return string literal TEXT with each literal %% written as %."
  (let ((positions (source-expression--doubled-percent-positions text)))
    (with-output-to-string (stream)
      (loop for character across text
            for index from 0
            unless (member (1- index) positions)
              do (write-char character stream)))))

(-> source-expression--doubled-percent-positions (string) list)
(defun source-expression--doubled-percent-positions (text)
  "Return the positions where a literal %% starts in format control TEXT.

The percent sign ending a ~% directive is not literal, so ~%% is a newline
followed by one percent sign."
  (let ((positions nil)
        (index     0))
    (loop while (< index (length text))
          do (cond
               ((char= (char text index) #\~)
                (setf index (source-expression--directive-end text (1+ index))))
               ((and (char= (char text index) #\%)
                     (< (1+ index) (length text))
                     (char= (char text (1+ index)) #\%))
                (push index positions)
                (incf index 2))
               (t
                (incf index))))
    (nreverse positions)))

(-> source-expression--directive-end (string (integer 0)) (integer 0))
(defun source-expression--directive-end (text start)
  "Return the position after the format directive whose parameters begin at START in TEXT."
  (let ((index start))
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 ((char= character #\')
                  (incf index 2))
                 ((find character "0123456789+-,vV#:@")
                  (incf index))
                 (t
                  (return)))))
    (min (length text) (1+ index))))

(-> source-expression--marker-p (string) boolean)
(defun source-expression--marker-p (text)
  "Return true when comment TEXT holds TODO or FIXME as a word."
  (and (some (lambda (marker)
               (let ((position (search marker text)))
                 (and position
                      (or (zerop position)
                          (not (alphanumericp (char text (1- position)))))
                      (let ((end (+ position (length marker))))
                        (or (= end (length text))
                            (not (alphanumericp (char text end))))))))
             '("TODO" "FIXME"))
       t))
