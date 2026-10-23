(in-package #:autolith)

;;;; -- Predicate Schemas and Propositions --

;;; A schema declares one predicate: its qualified name, typed arguments, the
;;; argument positions identifying its subject, and a sentence template. A
;;; proposition applies a predicate to canonical terms, or negates one. Claims
;;; are identified by their scope and canonical proposition, so one fact
;;; stated twice is one claim with two supports.

(defparameter *knowledge-argument-limit* 8
  "The maximum arguments of one predicate.")

(defparameter *knowledge-text-limit* 400
  "The maximum characters of a schema template or description.")

(defparameter *knowledge-identifier-digits* 20
  "The hexadecimal digits of a digest identifier after its one-letter prefix.")


;;;; -- Names and Identifiers --

(-> knowledge-predicate-name-p (t) boolean)
(defun knowledge-predicate-name-p (value)
  "Return true when VALUE is a qualified predicate name such as event.start.

Names are lowercase dotted segments of letters, digits, and hyphens, each
starting with a letter, with at least two segments."
  (and (stringp value)
       (<= 3 (length value) 80)
       (let ((segments (uiop:split-string value :separator '(#\.))))
         (and (>= (length segments) 2)
              (every (lambda (segment)
                       (and (plusp (length segment))
                            (lower-case-p (char segment 0))
                            (every (lambda (character)
                                     (or (lower-case-p character)
                                         (digit-char-p character)
                                         (char= character #\-)))
                                   segment)))
                     segments)))
       t))

(-> knowledge-argument-name-p (t) boolean)
(defun knowledge-argument-name-p (value)
  "Return true when VALUE names a schema argument: lowercase letters, digits, hyphens."
  (and (stringp value)
       (<= 1 (length value) 40)
       (lower-case-p (char value 0))
       (every (lambda (character)
                (or (lower-case-p character)
                    (digit-char-p character)
                    (char= character #\-)))
              value)
       t))

(-> knowledge-digest-identifier (character t) string)
(defun knowledge-digest-identifier (prefix value)
  "Return PREFIX followed by digest digits identifying portable VALUE."
  (format nil "~C~A"
          prefix
          (subseq (memory-source-text-digest
                   (with-standard-io-syntax
                     (let ((*print-readably* nil))
                       (prin1-to-string value))))
                  0 *knowledge-identifier-digits*)))


;;;; -- Schemas --

(-> knowledge-schema-predicate (list) string)
(defun knowledge-schema-predicate (schema)
  "Return SCHEMA's qualified predicate name."
  (getf (rest schema) :predicate))

(-> knowledge-schema-arguments (list) list)
(defun knowledge-schema-arguments (schema)
  "Return SCHEMA's argument plists in order."
  (getf (rest schema) :arguments))

(-> knowledge-schema-key (list) list)
(defun knowledge-schema-key (schema)
  "Return SCHEMA's identifying argument positions, empty for a plain relation."
  (getf (rest schema) :key))

(-> knowledge-schema-argument-types (list) list)
(defun knowledge-schema-argument-types (schema)
  "Return SCHEMA's argument types in order."
  (mapcar (lambda (argument) (getf argument :type))
          (knowledge-schema-arguments schema)))

(-> knowledge--schema-argument-p (t) boolean)
(defun knowledge--schema-argument-p (argument)
  "Return true when ARGUMENT is one valid schema argument plist."
  (and (proper-list-p argument)
       (evenp (length argument))
       (knowledge-argument-name-p (getf argument :name))
       (typep (getf argument :type) 'knowledge-term-type)
       (let ((unit (getf argument :unit)))
         (or (null unit)
             (and (eq (getf argument :type) ':quantity)
                  (knowledge-unit-dimension unit))))
       t))

(-> knowledge-schema-p (t) boolean)
(defun knowledge-schema-p (schema)
  "Return true when SCHEMA is one complete portable predicate schema record."
  (and (proper-list-p schema)
       (eq (first schema) :schema)
       (evenp (length (rest schema)))
       (let* ((properties (rest schema))
              (arguments (getf properties :arguments))
              (key (getf properties :key))
              (template (getf properties :template))
              (description (getf properties :description)))
         (and (knowledge-predicate-name-p (getf properties :predicate))
              (proper-list-p arguments)
              (<= 1 (length arguments) *knowledge-argument-limit*)
              (every #'knowledge--schema-argument-p arguments)
              (= (length arguments)
                 (length (remove-duplicates
                          (mapcar (lambda (argument) (getf argument :name)) arguments)
                          :test #'string=)))
              (proper-list-p key)
              (< (length key) (length arguments))
              (every (lambda (position)
                       (and (integerp position) (< -1 position (length arguments))))
                     key)
              (= (length key) (length (remove-duplicates key)))
              (or (null template)
                  (and (stringp template) (<= (length template) *knowledge-text-limit*)))
              (or (null description)
                  (and (stringp description)
                       (<= (length description) *knowledge-text-limit*)))))
       t))

(-> knowledge-make-schema
    (&key (:predicate string) (:arguments list) (:key list)
          (:template (option string)) (:description (option string)))
    list)
(defun knowledge-make-schema (&key predicate arguments key template description)
  "Return a validated schema record, or signal KNOWLEDGE-ERROR correctively.

ARGUMENTS are plists of :NAME, :TYPE, and for a quantity optionally :UNIT,
requiring values convertible to that unit. KEY lists the argument names or positions identifying the subject;
supported claims that agree on them and differ elsewhere conflict."
  (let* ((positions
           (mapcar (lambda (entry)
                     (if (integerp entry)
                         entry
                         (or (position entry arguments
                                       :key (lambda (argument) (getf argument :name))
                                       :test #'equal)
                             (knowledge--fail ':invalid-schema entry
                                              "Key argument ~S is not an argument of ~A."
                                              entry predicate))))
                   key))
         (schema (list :schema
                       :predicate predicate
                       :arguments (mapcar (lambda (argument)
                                            (append (list :name (getf argument :name)
                                                          :type (getf argument :type))
                                                    (when (getf argument :unit)
                                                      (list :unit
                                                            (getf argument :unit)))))
                                          arguments)
                       :key positions
                       :template template
                       :description description)))
    (unless (knowledge-predicate-name-p predicate)
      (knowledge--fail ':invalid-schema predicate
                       "Predicate ~S must be lowercase dotted segments such as event.start."
                       predicate))
    (unless (knowledge-schema-p schema)
      (knowledge--fail ':invalid-schema predicate
                       "Schema ~A needs 1 to ~D uniquely named arguments of types entity, string, number, boolean, instant, date, duration, or quantity, and a key naming fewer than all of them."
                       predicate *knowledge-argument-limit*))
    schema))


;;;; -- Propositions --

(-> knowledge-variable-p (t) boolean)
(defun knowledge-variable-p (value)
  "Return true when VALUE is a rule variable such as ?event."
  (and (stringp value)
       (> (length value) 1)
       (char= (char value 0) #\?)
       (knowledge-argument-name-p (subseq value 1))
       t))

(-> knowledge-negation-p (t) boolean)
(defun knowledge-negation-p (proposition)
  "Return true when PROPOSITION is an explicit negation."
  (and (consp proposition) (eq (first proposition) :not) t))

(-> knowledge-proposition-atom (list) list)
(defun knowledge-proposition-atom (proposition)
  "Return PROPOSITION without its negation."
  (if (knowledge-negation-p proposition) (second proposition) proposition))

(-> knowledge-proposition-predicate (list) string)
(defun knowledge-proposition-predicate (proposition)
  "Return the predicate name of PROPOSITION or of the atom it negates."
  (first (knowledge-proposition-atom proposition)))

(-> knowledge-proposition-arguments (list) list)
(defun knowledge-proposition-arguments (proposition)
  "Return the argument terms of PROPOSITION or of the atom it negates."
  (rest (knowledge-proposition-atom proposition)))

(-> knowledge-proposition-valid-p (t list) boolean)
(defun knowledge-proposition-valid-p (proposition schema)
  "Return true when PROPOSITION is a canonical, ground application of SCHEMA."
  (let ((atom (and (proper-list-p proposition)
                   (if (knowledge-negation-p proposition)
                       (and (= (length proposition) 2) (second proposition))
                       proposition))))
    (and (proper-list-p atom)
         (equal (first atom) (knowledge-schema-predicate schema))
         (= (length (rest atom)) (length (knowledge-schema-arguments schema)))
         (every (lambda (term argument)
                  (and (knowledge-term-matches-type-p term (getf argument :type))
                       (not (knowledge-variable-p term))
                       (or (null (getf argument :unit))
                           (equal (knowledge-unit-dimension (third term))
                                  (knowledge-unit-dimension (getf argument :unit))))))
                (rest atom)
                (knowledge-schema-arguments schema))
         t)))

(-> knowledge--argument-term (list t list) t)
(defun knowledge--argument-term (schema input argument)
  "Return INPUT read as schema ARGUMENT, naming the argument in any error."
  (handler-case (knowledge-term-from-input input (getf argument :type))
    (knowledge-error (condition)
      (knowledge--fail (knowledge-error-code condition) input
                       "Argument ~A of ~A: ~A"
                       (getf argument :name)
                       (knowledge-schema-predicate schema)
                       (autolith-error-message condition)))))

(-> knowledge-make-proposition (list list &key (:negated boolean)) list)
(defun knowledge-make-proposition (schema inputs &key negated)
  "Return the canonical proposition applying SCHEMA to input values INPUTS.

INPUTS are written as people and models write them, and are read by each
argument's type. NEGATED makes an explicit negation."
  (let ((arguments (knowledge-schema-arguments schema)))
    (unless (and (proper-list-p inputs) (= (length inputs) (length arguments)))
      (knowledge--fail ':invalid-claim (knowledge-schema-predicate schema)
                       "~A takes ~D arguments: ~{~A~^, ~}."
                       (knowledge-schema-predicate schema)
                       (length arguments)
                       (mapcar (lambda (argument)
                                 (format nil "~A (~(~A~))"
                                         (getf argument :name) (getf argument :type)))
                               arguments)))
    (let* ((atom (cons (knowledge-schema-predicate schema)
                       (mapcar (lambda (input argument)
                                 (let ((term (knowledge--argument-term
                                              schema input argument)))
                                   (when (knowledge-variable-p term)
                                     (knowledge--fail ':invalid-claim term
                                                      "Claim arguments cannot be rule variables such as ~A."
                                                      term))
                                   term))
                               inputs arguments)))
           (proposition (if negated (list :not atom) atom)))
      (unless (knowledge-proposition-valid-p proposition schema)
        (knowledge--fail ':invalid-claim (knowledge-schema-predicate schema)
                         "Arguments of ~A do not match its schema."
                         (knowledge-schema-predicate schema)))
      proposition)))

(-> knowledge-proposition-key (list list) (option list))
(defun knowledge-proposition-key (proposition schema)
  "Return the conflict key of positive PROPOSITION under SCHEMA, or NIL.

Two supported claims with equal keys and different values conflict."
  (let ((key (knowledge-schema-key schema)))
    (when (and key (not (knowledge-negation-p proposition)))
      (cons (knowledge-proposition-predicate proposition)
            (mapcar (lambda (position)
                      (nth position (knowledge-proposition-arguments proposition)))
                    key)))))

(-> knowledge-propositions-equal-p (list list) boolean)
(defun knowledge-propositions-equal-p (left right)
  "Return true when LEFT and RIGHT state the same thing, comparing values."
  (and (eq (knowledge-negation-p left) (knowledge-negation-p right))
       (equal (knowledge-proposition-predicate left)
              (knowledge-proposition-predicate right))
       (= (length (knowledge-proposition-arguments left))
          (length (knowledge-proposition-arguments right)))
       (every #'knowledge-term=
              (knowledge-proposition-arguments left)
              (knowledge-proposition-arguments right))
       t))

(-> knowledge-claim-identifier (list &key (:scope memory-scope) (:workspace (option string)))
    string)
(defun knowledge-claim-identifier (proposition &key scope workspace)
  "Return the stable identifier of the claim PROPOSITION makes in SCOPE.

Claims of the same canonical proposition in the same scope share it."
  (knowledge-digest-identifier #\c (list scope workspace proposition)))


;;;; -- Presentation --

(-> knowledge-proposition-display (list (option list)) string)
(defun knowledge-proposition-display (proposition schema)
  "Return PROPOSITION as a sentence using SCHEMA's template when it has one."
  (let* ((arguments (knowledge-proposition-arguments proposition))
         (template (and schema (getf (rest schema) :template)))
         (text
           (if template
               (let ((result template))
                 (loop for argument in (knowledge-schema-arguments schema)
                       for term in arguments
                       do (setf result
                                (uiop:frob-substrings
                                 result
                                 (list (format nil "{~A}" (getf argument :name)))
                                 (knowledge-term-display term))))
                 result)
               (format nil "~A(~{~A~^, ~})"
                       (knowledge-proposition-predicate proposition)
                       (mapcar #'knowledge-term-display arguments)))))
    (if (knowledge-negation-p proposition)
        (format nil "not: ~A" text)
        text)))
