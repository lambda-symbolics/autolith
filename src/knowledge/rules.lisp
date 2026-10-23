(in-package #:autolith)

;;;; -- Knowledge Rules --

;;; A rule derives one conclusion from claims matched by its premises:
;;;
;;;   (:rule :id "departure"
;;;    :premises (("event.start" "?event" "?start")
;;;               ("event.travel" "?event" "?travel")
;;;               (:not ("event.cancelled" "?event")))
;;;    :guards (("<" "?travel" (:duration 7200)))
;;;    :bindings (("?leave" ("-" "?start" "?travel")))
;;;    :conclusion ("event.departure" "?event" "?leave"))
;;;
;;; Rules are range restricted: every variable of a guard, binding, negated
;;; premise, or the conclusion is bound by a positive premise or an earlier
;;; binding. A negated premise matches only an explicitly supported negation;
;;; a missing claim never satisfies it. Computations are registered pure
;;; functions with static signatures, so a rule is type-checked against its
;;; schemas before it can be admitted.

(defparameter *knowledge-rule-premise-limit* 8
  "The maximum premises of one rule.")

(defparameter *knowledge-rule-step-limit* 8
  "The maximum guards, and separately bindings, of one rule.")

(defparameter *knowledge-expression-depth-limit* 6
  "The deepest nesting of one rule expression.")

(defparameter *knowledge-comparisons* '("<" "<=" ">" ">=" "=" "/=")
  "The comparison operators a rule guard may use.")


;;;; -- Computations --

(-> knowledge--days-only (integer) integer)
(defun knowledge--days-only (seconds)
  "Return whole days in SECONDS, or signal when SECONDS is not whole days."
  (multiple-value-bind (days rest) (floor seconds 86400)
    (unless (zerop rest)
      (knowledge--fail ':evaluation seconds "Dates move only by whole days."))
    days))

(-> knowledge--date-add (string integer) list)
(defun knowledge--date-add (date seconds)
  "Return the date term SECONDS of whole days after canonical DATE."
  (list :date (subseq (knowledge-format-instant
                       (+ (knowledge-date-universal-time date)
                          (* 86400 (knowledge--days-only seconds))))
                      0 10)))

(-> knowledge--quantity-in (list string) list)
(defun knowledge--quantity-in (quantity unit)
  "Return QUANTITY expressed in UNIT of the same dimension."
  (unless (equal (knowledge-unit-dimension (third quantity)) (knowledge-unit-dimension unit))
    (knowledge--fail ':evaluation (list quantity unit)
                     "Cannot express ~A in ~A." (knowledge-term-display quantity) unit))
  (list :quantity
        (/ (* (second quantity) (knowledge-unit-factor (third quantity)))
           (knowledge-unit-factor unit))
        unit))

(-> knowledge--add (t t) t)
(defun knowledge--add (left right)
  "Add two terms whose types support addition."
  (let ((types (list (knowledge-term-type-of left) (knowledge-term-type-of right))))
    (cond
      ((equal types '(:number :number)) (+ left right))
      ((equal types '(:duration :duration)) (list :duration (+ (second left) (second right))))
      ((equal types '(:instant :duration))
       (knowledge-make-instant (+ (knowledge-instant-time left) (second right))))
      ((equal types '(:duration :instant)) (knowledge--add right left))
      ((equal types '(:date :duration)) (knowledge--date-add (second left) (second right)))
      ((equal types '(:quantity :quantity))
       (list :quantity (+ (second left) (second (knowledge--quantity-in right (third left))))
             (third left)))
      (t (knowledge--fail ':evaluation types "Cannot add ~(~A~) and ~(~A~)."
                          (first types) (second types))))))

(-> knowledge--subtract (t t) t)
(defun knowledge--subtract (left right)
  "Subtract RIGHT from LEFT where their types support subtraction."
  (let ((types (list (knowledge-term-type-of left) (knowledge-term-type-of right))))
    (cond
      ((equal types '(:number :number)) (- left right))
      ((equal types '(:duration :duration)) (list :duration (- (second left) (second right))))
      ((equal types '(:instant :duration))
       (knowledge-make-instant (- (knowledge-instant-time left) (second right))))
      ((equal types '(:instant :instant))
       (list :duration (- (knowledge-instant-time left) (knowledge-instant-time right))))
      ((equal types '(:date :duration)) (knowledge--date-add (second left) (- (second right))))
      ((equal types '(:date :date))
       (list :duration (- (knowledge-date-universal-time (second left))
                          (knowledge-date-universal-time (second right)))))
      ((equal types '(:quantity :quantity))
       (list :quantity (- (second left) (second (knowledge--quantity-in right (third left))))
             (third left)))
      (t (knowledge--fail ':evaluation types "Cannot subtract ~(~A~) from ~(~A~)."
                          (second types) (first types))))))

(-> knowledge--multiply (t t) t)
(defun knowledge--multiply (left right)
  "Multiply a number, duration, or quantity by a number."
  (let ((types (list (knowledge-term-type-of left) (knowledge-term-type-of right))))
    (cond
      ((equal types '(:number :number)) (* left right))
      ((equal types '(:duration :number)) (list :duration (round (* (second left) right))))
      ((equal types '(:number :duration)) (knowledge--multiply right left))
      ((equal types '(:quantity :number)) (list :quantity (* (second left) right) (third left)))
      ((equal types '(:number :quantity)) (knowledge--multiply right left))
      (t (knowledge--fail ':evaluation types "Cannot multiply ~(~A~) by ~(~A~)."
                          (first types) (second types))))))

(-> knowledge--divide (t t) t)
(defun knowledge--divide (left right)
  "Divide numbers, durations, or quantities where the quotient is defined."
  (let ((types (list (knowledge-term-type-of left) (knowledge-term-type-of right))))
    (when (case (second types)
            (:number (zerop right))
            (:duration (zerop (second right)))
            (:quantity (zerop (second right))))
      (knowledge--fail ':evaluation right "Division by zero."))
    (cond
      ((equal types '(:number :number)) (/ left right))
      ((equal types '(:duration :number)) (list :duration (round (second left) right)))
      ((equal types '(:duration :duration)) (/ (second left) (second right)))
      ((equal types '(:quantity :number)) (list :quantity (/ (second left) right) (third left)))
      ((equal types '(:quantity :quantity))
       (/ (second left) (second (knowledge--quantity-in right (third left)))))
      (t (knowledge--fail ':evaluation types "Cannot divide ~(~A~) by ~(~A~)."
                          (first types) (second types))))))

(-> knowledge--extreme (function t t) t)
(defun knowledge--extreme (test left right)
  "Return LEFT when TEST holds of its comparison with RIGHT, otherwise RIGHT."
  (if (funcall test (knowledge-term-compare left right)) left right))

(defparameter *knowledge-functions*
  (list
   (list "+" #'knowledge--add
         '(((:number :number) :number) ((:duration :duration) :duration)
           ((:instant :duration) :instant) ((:duration :instant) :instant)
           ((:date :duration) :date) ((:quantity :quantity) :quantity)))
   (list "-" #'knowledge--subtract
         '(((:number :number) :number) ((:duration :duration) :duration)
           ((:instant :duration) :instant) ((:instant :instant) :duration)
           ((:date :duration) :date) ((:date :date) :duration)
           ((:quantity :quantity) :quantity)))
   (list "*" #'knowledge--multiply
         '(((:number :number) :number) ((:duration :number) :duration)
           ((:number :duration) :duration) ((:quantity :number) :quantity)
           ((:number :quantity) :quantity)))
   (list "/" #'knowledge--divide
         '(((:number :number) :number) ((:duration :number) :duration)
           ((:duration :duration) :number) ((:quantity :number) :quantity)
           ((:quantity :quantity) :number)))
   (list "min" (lambda (left right) (knowledge--extreme (lambda (order) (<= order 0)) left right))
         :same)
   (list "max" (lambda (left right) (knowledge--extreme (lambda (order) (>= order 0)) left right))
         :same)
   (list "abs" (lambda (value)
                 (case (knowledge-term-type-of value)
                   (:number (abs value))
                   (:duration (list :duration (abs (second value))))
                   (:quantity (list :quantity (abs (second value)) (third value)))
                   (otherwise (knowledge--fail ':evaluation value "Cannot take abs of ~S."
                                               value))))
         '(((:number) :number) ((:duration) :duration) ((:quantity) :quantity)))
   (list "date" (lambda (instant)
                  (list :date (subseq (second instant) 0 10)))
         '(((:instant) :date)))
   (list "start-of-day" (lambda (date)
                          (knowledge-make-instant (knowledge-date-universal-time (second date))))
         '(((:date) :instant)))
   (list "weekday" (lambda (date)
                     (1+ (nth-value 6 (decode-universal-time
                                       (knowledge-date-universal-time (second date)) 0))))
         '(((:date) :number)))
   (list "days" (lambda (count) (list :duration (round (* count 86400))))
         '(((:number) :duration)))
   (list "hours" (lambda (count) (list :duration (round (* count 3600))))
         '(((:number) :duration)))
   (list "minutes" (lambda (count) (list :duration (round (* count 60))))
         '(((:number) :duration)))
   (list "in-unit" (lambda (quantity unit) (knowledge--quantity-in quantity unit))
         '(((:quantity :string) :quantity))))
  "Pure rule computations as (NAME FUNCTION SIGNATURES).

SIGNATURES lists ((ARGUMENT-TYPES) RESULT-TYPE) alternatives; :SAME accepts two
arguments of one comparable type and returns that type. Weekday counts Monday
as 1.")

(-> knowledge-function (string) (option list))
(defun knowledge-function (name)
  "Return the registered computation NAME as (NAME FUNCTION SIGNATURES), or NIL."
  (assoc name *knowledge-functions* :test #'string=))


;;;; -- Expressions --

(-> knowledge-expression-call-p (t) boolean)
(defun knowledge-expression-call-p (expression)
  "Return true when EXPRESSION is a computation call such as (\"-\" \"?a\" \"?b\")."
  (and (consp expression) (stringp (first expression)) t))

(-> knowledge-expression-variables (t) list)
(defun knowledge-expression-variables (expression)
  "Return the distinct variables EXPRESSION reads."
  (cond
    ((knowledge-variable-p expression)
     (list expression))
    ((knowledge-expression-call-p expression)
     (remove-duplicates (mapcan #'knowledge-expression-variables (rest expression))
                        :test #'string=))
    (t
     nil)))

(-> knowledge-expression-from-input (t &optional integer) t)
(defun knowledge-expression-from-input (value &optional (depth 0))
  "Return the rule expression VALUE as written by a model or person.

A string is a variable when it starts with ?, a duration, instant, or date when
it parses as one, and otherwise text. An array starting with a computation name
is a call. An object with one of instant, date, duration, number, or a
magnitude and unit is a typed literal."
  (when (> depth *knowledge-expression-depth-limit*)
    (knowledge--fail ':invalid-rule value "Rule expressions nest at most ~D levels."
                     *knowledge-expression-depth-limit*))
  (cond
    ((knowledge-variable-p value)
     value)
    ((and (stringp value) (plusp (length value)) (char= (char value 0) #\?))
     (knowledge--fail ':invalid-rule value
                      "Variable ~S must be ? followed by lowercase letters, digits, or hyphens."
                      value))
    ((and (stringp value) (knowledge-parse-duration value)
          (plusp (length value)) (find (char value 0) "P-"))
     (list :duration (knowledge-parse-duration value)))
    ((and (stringp value) (knowledge-parse-instant value))
     (knowledge-make-instant (knowledge-parse-instant value)))
    ((and (stringp value) (knowledge-parse-date value))
     (list :date value))
    ((or (stringp value) (rationalp value) (member value '(:true :false)))
     value)
    ((floatp value)
     (rationalize value))
    ((hash-table-p value)
     (let ((keys (loop for key being the hash-keys of value collect key)))
       (cond
         ((equal (sort (copy-list keys) #'string<) '("magnitude" "unit"))
          (knowledge-term-from-input value ':quantity))
         ((and (= (length keys) 1)
               (member (first keys) '("instant" "date" "duration" "number")
                       :test #'string=))
          (knowledge-term-from-input (gethash (first keys) value)
                                     (intern (string-upcase (first keys)) :keyword)))
         (t
          (knowledge--fail ':invalid-rule keys "Unrecognized typed literal ~{~A~^, ~}." keys)))))
    ((and (or (vectorp value) (consp value)) (not (knowledge-term-p value)))
     (let ((items (coerce value 'list)))
       (unless (and items (stringp (first items)) (knowledge-function (first items)))
         (knowledge--fail ':invalid-rule value
                          "A computation starts with one of ~{~A~^, ~}."
                          (mapcar #'first *knowledge-functions*)))
       (cons (first items)
             (mapcar (lambda (item) (knowledge-expression-from-input item (1+ depth)))
                     (rest items)))))
    ((knowledge-term-p value)
     value)
    (t
     (knowledge--fail ':invalid-rule value "Unrecognized rule expression ~S." value))))

(-> knowledge-evaluate-expression (t hash-table) t)
(defun knowledge-evaluate-expression (expression environment)
  "Return the term EXPRESSION computes with variable bindings in ENVIRONMENT.

Signal KNOWLEDGE-ERROR with code :EVALUATION when a computation is undefined
for its values, such as a division by zero."
  (cond
    ((knowledge-variable-p expression)
     (multiple-value-bind (value present-p) (gethash expression environment)
       (unless present-p
         (knowledge--fail ':evaluation expression "Variable ~A is unbound." expression))
       value))
    ((knowledge-expression-call-p expression)
     (let ((entry (knowledge-function (first expression))))
       (apply (second entry)
              (mapcar (lambda (argument)
                        (knowledge-evaluate-expression argument environment))
                      (rest expression)))))
    (t
     expression)))

(-> knowledge-guard-holds-p (list hash-table) boolean)
(defun knowledge-guard-holds-p (guard environment)
  "Return true when comparison GUARD holds for the values in ENVIRONMENT."
  (let ((order (knowledge-term-compare
                (knowledge-evaluate-expression (second guard) environment)
                (knowledge-evaluate-expression (third guard) environment))))
    (and (cond
           ((string= (first guard) "<") (< order 0))
           ((string= (first guard) "<=") (<= order 0))
           ((string= (first guard) ">") (> order 0))
           ((string= (first guard) ">=") (>= order 0))
           ((string= (first guard) "=") (zerop order))
           (t (/= order 0)))
         t)))


;;;; -- Static Types --

(-> knowledge--expression-type (t hash-table) keyword)
(defun knowledge--expression-type (expression types)
  "Return the static type of EXPRESSION given variable TYPES, or signal correctively."
  (cond
    ((knowledge-variable-p expression)
     (or (gethash expression types)
         (knowledge--fail ':invalid-rule expression
                          "Variable ~A is used before a premise or binding gives it a value."
                          expression)))
    ((knowledge-expression-call-p expression)
     (let* ((entry (knowledge-function (first expression)))
            (argument-types (mapcar (lambda (argument)
                                      (knowledge--expression-type argument types))
                                    (rest expression)))
            (signatures (third entry)))
       (or (if (eq signatures :same)
               (and (= (length argument-types) 2)
                    (eq (first argument-types) (second argument-types))
                    (first argument-types))
               (second (find argument-types signatures :key #'first :test #'equal)))
           (knowledge--fail ':invalid-rule expression
                            "~A does not accept ~{~(~A~)~^, ~}."
                            (first expression) argument-types))))
    (t
     (let ((type (knowledge-term-type-of expression)))
       (or type
           (knowledge--fail ':invalid-rule expression "~S is not a value." expression))))))

(-> knowledge--type-compatible-p (keyword keyword) boolean)
(defun knowledge--type-compatible-p (actual expected)
  "Return true when a value of type ACTUAL may fill a slot of type EXPECTED."
  (and (or (eq actual expected)
           (and (eq expected ':entity) (eq actual ':string))
           (and (eq expected ':string) (eq actual ':entity)))
       t))


;;;; -- Rule Validation --

(-> knowledge-rule-name-p (t) boolean)
(defun knowledge-rule-name-p (value)
  "Return true when VALUE names a rule: lowercase letters, digits, hyphens, dots."
  (and (stringp value)
       (<= 1 (length value) 80)
       (lower-case-p (char value 0))
       (every (lambda (character)
                (or (lower-case-p character) (digit-char-p character)
                    (find character "-.")))
              value)
       t))

(-> knowledge-rule-premise-atom (list) list)
(defun knowledge-rule-premise-atom (premise)
  "Return PREMISE's pattern without its negation."
  (if (knowledge-negation-p premise) (second premise) premise))

(-> knowledge-rule-premise-predicates (list) list)
(defun knowledge-rule-premise-predicates (rule)
  "Return the predicates RULE's premises read."
  (remove-duplicates (mapcar (lambda (premise)
                               (first (knowledge-rule-premise-atom premise)))
                             (getf (rest rule) :premises))
                     :test #'string=))

(-> knowledge-rule-conclusion-predicate (list) string)
(defun knowledge-rule-conclusion-predicate (rule)
  "Return the predicate RULE concludes."
  (first (knowledge-rule-premise-atom (getf (rest rule) :conclusion))))

(-> knowledge--pattern-from-input (t function &key (:negatable boolean)) list)
(defun knowledge--pattern-from-input (value schema-function &key negatable)
  "Return a canonical pattern from VALUE, reading literals by the predicate's schema.

VALUE is (PREDICATE ARGUMENT...) or, when NEGATABLE, (:not PATTERN)."
  (let ((items (and (or (consp value) (vectorp value)) (coerce value 'list))))
    (cond
      ((and negatable (member (first items) '(:not "not") :test #'equal))
       (list :not (knowledge--pattern-from-input (second items) schema-function)))
      ((and items (stringp (first items)))
       (let ((schema (funcall schema-function (first items))))
         (unless schema
           (knowledge--fail ':unknown-predicate (first items)
                            "Define predicate ~A before using it in a pattern." (first items)))
         (unless (= (length (rest items)) (length (knowledge-schema-arguments schema)))
           (knowledge--fail ':invalid-rule (first items)
                            "~A takes ~D arguments." (first items)
                            (length (knowledge-schema-arguments schema))))
         (cons (first items)
               (mapcar (lambda (item argument)
                         (if (knowledge-variable-p item)
                             item
                             (knowledge--argument-term schema item argument)))
                       (rest items)
                       (knowledge-schema-arguments schema)))))
      (t
       (knowledge--fail ':invalid-rule value
                        "A pattern is a predicate name followed by its arguments.")))))

(-> knowledge--check-pattern-types (list function hash-table &key (:binding boolean)) null)
(defun knowledge--check-pattern-types (pattern schema-function types &key binding)
  "Check PATTERN's variables against its schema, recording new ones when BINDING."
  (let ((schema (funcall schema-function (first pattern))))
    (loop for item in (rest pattern)
          for argument in (knowledge-schema-arguments schema)
          for expected = (getf argument :type)
          when (knowledge-variable-p item)
            do (let ((known (gethash item types)))
                 (cond
                   ((and known (not (knowledge--type-compatible-p known expected)))
                    (knowledge--fail ':invalid-rule item
                                     "~A is ~(~A~) but argument ~A of ~A needs ~(~A~)."
                                     item known (getf argument :name) (first pattern) expected))
                   ((and (null known) (not binding))
                    (knowledge--fail ':invalid-rule item
                                     "~A must be bound by a positive premise or binding first."
                                     item))
                   ((null known)
                    (setf (gethash item types) expected))))))
  nil)

(-> knowledge-make-rule
    (&key (:id string) (:premises sequence) (:guards sequence) (:bindings sequence)
          (:conclusion t) (:description (option string)) (:schema-function function))
    list)
(defun knowledge-make-rule
    (&key id premises guards bindings conclusion description schema-function)
  "Return a validated, type-checked rule built from input forms.

SCHEMA-FUNCTION maps a predicate name to its visible schema or NIL. Signal
KNOWLEDGE-ERROR with a corrective message when the rule is not range
restricted, not well typed, or exceeds its bounds."
  (unless (knowledge-rule-name-p id)
    (knowledge--fail ':invalid-rule id
                     "Rule names are lowercase letters, digits, hyphens, and dots."))
  (let* ((premises (coerce premises 'list))
         (guards (coerce guards 'list))
         (bindings (coerce bindings 'list))
         (types (make-hash-table :test #'equal)))
    (unless (<= 1 (length premises) *knowledge-rule-premise-limit*)
      (knowledge--fail ':invalid-rule id "A rule needs 1 to ~D premises."
                       *knowledge-rule-premise-limit*))
    (unless (and (<= (length guards) *knowledge-rule-step-limit*)
                 (<= (length bindings) *knowledge-rule-step-limit*))
      (knowledge--fail ':invalid-rule id "A rule has at most ~D guards and ~D bindings."
                       *knowledge-rule-step-limit* *knowledge-rule-step-limit*))
    (let* ((patterns (mapcar (lambda (premise)
                               (knowledge--pattern-from-input premise schema-function
                                                              :negatable t))
                             premises))
           (positive (remove-if #'knowledge-negation-p patterns)))
      (unless positive
        (knowledge--fail ':invalid-rule id "A rule needs at least one positive premise."))
      (dolist (pattern positive)
        (knowledge--check-pattern-types pattern schema-function types :binding t))
      (dolist (pattern (remove-if-not #'knowledge-negation-p patterns))
        (knowledge--check-pattern-types (second pattern) schema-function types))
      (let ((normalized-guards
              (mapcar
               (lambda (guard)
                 (let ((items (coerce guard 'list)))
                   (unless (and (= (length items) 3)
                                (member (first items) *knowledge-comparisons* :test #'equal))
                     (knowledge--fail ':invalid-rule guard
                                      "A guard is a comparison (~{~A~^ ~}) of two expressions."
                                      *knowledge-comparisons*))
                   (let ((left (knowledge-expression-from-input (second items)))
                         (right (knowledge-expression-from-input (third items))))
                     (list (first items) left right))))
               guards))
            (normalized-bindings
              (mapcar
               (lambda (binding)
                 (let ((items (coerce binding 'list)))
                   (unless (and (= (length items) 2) (knowledge-variable-p (first items)))
                     (knowledge--fail ':invalid-rule binding
                                      "A binding is a new variable and the expression computing it."))
                   (when (gethash (first items) types)
                     (knowledge--fail ':invalid-rule (first items)
                                      "~A is already bound." (first items)))
                   (let ((expression (knowledge-expression-from-input (second items))))
                     (setf (gethash (first items) types)
                           (knowledge--expression-type expression types))
                     (list (first items) expression))))
               bindings)))
        (dolist (guard normalized-guards)
          (let ((left (knowledge--expression-type (second guard) types))
                (right (knowledge--expression-type (third guard) types)))
            (unless (or (knowledge--type-compatible-p left right)
                        (and (member (first guard) '("=" "/=") :test #'string=)
                             (knowledge--type-compatible-p right left)))
              (knowledge--fail ':invalid-rule guard
                               "Cannot compare ~(~A~) with ~(~A~)." left right))))
        (let ((conclusion-pattern
                (knowledge--pattern-from-input conclusion schema-function :negatable t)))
          (knowledge--check-pattern-types (knowledge-rule-premise-atom conclusion-pattern)
                                          schema-function types)
          (when (find (first (knowledge-rule-premise-atom conclusion-pattern))
                      (mapcar (lambda (pattern) (first (knowledge-rule-premise-atom pattern)))
                              patterns)
                      :test #'string=)
            (knowledge--fail ':rule-cycle id
                             "A rule cannot conclude a predicate it reads."))
          (list :rule
                :id id
                :premises patterns
                :guards normalized-guards
                :bindings normalized-bindings
                :conclusion conclusion-pattern
                :description description))))))

(-> knowledge-rule-p (t) boolean)
(defun knowledge-rule-p (rule)
  "Return true when RULE has the structure of a validated rule record."
  (and (proper-list-p rule)
       (eq (first rule) :rule)
       (evenp (length (rest rule)))
       (knowledge-rule-name-p (getf (rest rule) :id))
       (proper-list-p (getf (rest rule) :premises))
       (getf (rest rule) :premises)
       (proper-list-p (getf (rest rule) :guards))
       (proper-list-p (getf (rest rule) :bindings))
       (consp (getf (rest rule) :conclusion))
       t))

(-> knowledge-rules-acyclic-p (list) boolean)
(defun knowledge-rules-acyclic-p (rules)
  "Return true when no predicate depends on itself through RULES."
  (let ((edges (make-hash-table :test #'equal))
        (state (make-hash-table :test #'equal)))
    (dolist (rule rules)
      (dolist (predicate (knowledge-rule-premise-predicates rule))
        (pushnew (knowledge-rule-conclusion-predicate rule) (gethash predicate edges)
                 :test #'string=)))
    (labels ((visit (predicate)
               (case (gethash predicate state)
                 (:visiting nil)
                 (:done t)
                 (otherwise
                  (setf (gethash predicate state) ':visiting)
                  (prog1 (every #'visit (gethash predicate edges))
                    (setf (gethash predicate state) ':done))))))
      (and (loop for predicate being the hash-keys of edges always (visit predicate)) t))))


;;;; -- Stored Rules --

(-> knowledge-base-rule (knowledge-base string &key (:workspace (option string)))
    (option list))
(defun knowledge-base-rule (base identifier &key workspace)
  "Return the entry of rule IDENTIFIER visible from WORKSPACE, preferring its own."
  (or (and workspace
           (gethash (list ':workspace workspace identifier) (knowledge-base-rules base)))
      (gethash (list ':global nil identifier) (knowledge-base-rules base))))

(-> knowledge-rule-entry-version (list integer) (option list))
(defun knowledge-rule-entry-version (entry version)
  "Return VERSION's plist from rule ENTRY, or NIL."
  (find version (getf entry :versions) :key (lambda (item) (getf item :version))))

(-> knowledge-base-rule-entries (knowledge-base &key (:workspace (option string))) list)
(defun knowledge-base-rule-entries (base &key workspace)
  "Return every rule entry visible from WORKSPACE."
  (loop for entry being the hash-values of (knowledge-base-rules base)
        when (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
          collect entry))

(-> knowledge-base-active-rules (knowledge-base &key (:workspace (option string))) list)
(defun knowledge-base-active-rules (base &key workspace)
  "Return (ENTRY VERSION RULE) for each admitted rule visible from WORKSPACE."
  (loop for entry in (knowledge-base-rule-entries base :workspace workspace)
        for version = (getf entry :active)
        when version
          collect (list entry version
                        (getf (knowledge-rule-entry-version entry version) :rule))))

(-> knowledge--rule-owner-key (list) list)
(defun knowledge--rule-owner-key (entry)
  "Return rule ENTRY's (SCOPE WORKSPACE IDENTIFIER) key."
  (list (getf entry :scope) (getf entry :workspace) (getf entry :id)))

(-> knowledge--check-acyclic (knowledge-base list list (option string)) null)
(defun knowledge--check-acyclic (base key rule workspace)
  "Refuse RULE under KEY when it would make the visible active rules cyclic."
  (unless (knowledge-rules-acyclic-p
           (cons rule
                 (loop for (entry nil active) in (knowledge-base-active-rules
                                                  base :workspace workspace)
                       unless (equal (knowledge--rule-owner-key entry) key)
                         collect active)))
    (knowledge--fail ':rule-cycle (getf (rest rule) :id)
                     "Rule ~A would make a predicate depend on itself through other rules."
                     (getf (rest rule) :id)))
  nil)

(-> knowledge-propose-rule
    (configuration &key (:id string) (:premises t) (:guards t) (:bindings t)
                        (:conclusion t) (:description (option string))
                        (:scope memory-scope) (:source-conversation (option string)))
    (values string integer boolean))
(defun knowledge-propose-rule
    (configuration &key id premises guards bindings conclusion description
                        (scope ':workspace) source-conversation)
  "Validate and record a new version of rule ID, inactive until admitted.

Return the rule ID, its version, and whether it differs from the latest one."
  (let ((workspace (knowledge--owner configuration scope))
        (viewer (knowledge--workspace configuration)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((rule (knowledge-make-rule
                      :id id :premises premises :guards guards :bindings bindings
                      :conclusion conclusion :description description
                      :schema-function
                      (lambda (predicate)
                        (knowledge-base-schema base predicate
                                               :workspace (and (eq scope ':workspace) viewer)))))
               (key (list scope workspace id))
               (entry (gethash key (knowledge-base-rules base)))
               (latest (first (last (getf entry :versions)))))
          (knowledge--check-acyclic base key rule viewer)
          (if (and latest (equal (getf latest :rule) rule))
              (values nil (list id (getf latest :version) nil) nil)
              (values (list (list :rule-defined
                                  :version *knowledge-format-version*
                                  :scope scope
                                  :workspace workspace
                                  :rule rule
                                  :time (get-universal-time)
                                  :source-conversation source-conversation))
                      (list id (1+ (length (getf entry :versions))) t)
                      t))))))))

(-> knowledge--rule-transition
    (configuration string keyword &key (:scope memory-scope) (:version (option integer))
                                       (:by (option string)))
    integer)
(defun knowledge--rule-transition (configuration id kind &key (scope ':workspace) version by)
  "Record admission or retirement KIND of rule ID and return the affected version."
  (let ((workspace (knowledge--owner configuration scope))
        (viewer (knowledge--workspace configuration)))
    (knowledge-transact
     configuration
     (lambda (base)
       (let* ((key (list scope workspace id))
              (entry (gethash key (knowledge-base-rules base)))
              (selected (or version (length (getf entry :versions)))))
         (unless entry
           (knowledge--fail ':unknown-rule id "No ~(~A~) rule ~A exists." scope id))
         (unless (knowledge-rule-entry-version entry selected)
           (knowledge--fail ':unknown-rule id "Rule ~A has no version ~A." id selected))
         (when (eq kind :rule-admitted)
           (knowledge--check-acyclic base key
                                     (getf (knowledge-rule-entry-version entry selected) :rule)
                                     viewer))
         (if (eql (getf entry :active) (and (eq kind :rule-admitted) selected))
             (values nil selected nil)
             (values (list (list kind
                                 :version *knowledge-format-version*
                                 :scope scope
                                 :workspace workspace
                                 :id id
                                 :rule-version selected
                                 :by by
                                 :time (get-universal-time)))
                     selected
                     t)))))))

(-> knowledge-admit-rule
    (configuration string &key (:scope memory-scope) (:version (option integer))
                               (:by (option string)))
    integer)
(defun knowledge-admit-rule (configuration id &key (scope ':workspace) version by)
  "Activate VERSION of rule ID, by default its latest, and return that version.

BY records who admitted it. The version replaces any earlier active one, so its
conclusions replace those derived by the earlier version."
  (knowledge--rule-transition configuration id :rule-admitted
                              :scope scope :version version :by by))

(-> knowledge-retire-rule (configuration string &key (:scope memory-scope)) integer)
(defun knowledge-retire-rule (configuration id &key (scope ':workspace))
  "Deactivate rule ID; what it derived is no longer supported by it."
  (knowledge--rule-transition configuration id :rule-retired :scope scope))
