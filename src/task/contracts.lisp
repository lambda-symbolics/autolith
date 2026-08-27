(in-package #:autolith)

;;;; -- Task Conditions --

(define-condition task-error
    (tool-error)
  ((task-id
    :initarg :task-id
    :initform nil
    :reader task-error-task-id
    :type (option string)
    :documentation "The child or job identifier involved in the failure, when known."))
  (:documentation
   "A task request, child run, or task job violated its contract."))

(define-condition task-agent-definition-error
    (task-error)
  ((pathname
    :initarg :pathname
    :initform nil
    :reader task-agent-definition-error-pathname
    :type (option pathname)
    :documentation "The external role file containing the invalid definition, when any.")
   (source
    :initarg :source
    :reader task-agent-definition-error-source
    :type keyword
    :documentation "The project, user, site, bundled, or programmatic definition origin.")
   (line
    :initarg :line
    :initform nil
    :reader task-agent-definition-error-line
    :type (option (integer 1))
    :documentation "The source line nearest the invalid value, when known.")
   (field
    :initarg :field
    :initform nil
    :reader task-agent-definition-error-field
    :type (option keyword)
    :documentation "The native plist field whose contract was violated, when known.")
   (cause
    :initarg :cause
    :reader task-agent-definition-error-cause
    :type t
    :documentation "The original condition or concise structural failure.")
   (definition-name
    :initarg :definition-name
    :initform nil
    :reader task-agent-definition-error-definition-name
    :type (option string)
    :documentation "The normalized role basename reserved by this diagnostic."))
  (:documentation "A native child-role definition is unsafe or malformed.")
  (:report
   (lambda (condition stream)
     (format stream "Invalid ~A task agent~@[ ~S~]~@[ in ~A~]~@[ at line ~D~]~@[, field ~S~]: ~A"
             (string-downcase
              (symbol-name (task-agent-definition-error-source condition)))
             (task-agent-definition-error-definition-name condition)
             (task-agent-definition-error-pathname condition)
             (task-agent-definition-error-line condition)
             (task-agent-definition-error-field condition)
             (task-agent-definition-error-cause condition)))))

;;; A cancelled child unwinds on CL-JOBPOND:JOB-ABORTED.

(define-condition task-yield-error
    (task-error)
  nil
  (:documentation
   "A child agent supplied an invalid or duplicate terminal yield."))


;;;; -- Native Definition Bounds --

(defparameter *task-agent-file-maximum-bytes* 131072
  "The largest external child-role file accepted by the native reader.")

(defparameter *task-agent-form-maximum-nodes* 8192
  "The largest readable object tree accepted from one role file.")

(defparameter *task-agent-form-maximum-depth* 128
  "The deepest list nesting accepted in a native child-role file.")

(defparameter *task-agent-string-maximum-characters* 32768
  "The largest individual string accepted in a role form.")

(defparameter *task-agent-name-maximum-characters* 64
  "The maximum normalized child-role name length.")

(defparameter *task-agent-description-maximum-characters* 512
  "The maximum child-role description length.")

(defparameter *task-agent-instructions-maximum-characters* 32768
  "The maximum child-role instruction body length.")

(defparameter *task-agent-definition-fields*
  '(:name :description :instructions :tools :spawns :models
    :reasoning-effort :output :blocking-p)
  "The complete native child-role plist vocabulary.")

(defparameter *task-output-types*
  '(:object :array :string :number :integer :boolean :null)
  "The value types supported by native task output contracts.")

(defparameter *task-forbidden-child-tool-namespaces*
  '("self" "task" "job" "yield")
  "Tool namespaces structurally unavailable to ordinary child-role grants.")

(defparameter *task-model-aliases*
  '("@task" "@parent" "@auto" "@smol" "@slow" "@designer")
  "The model aliases accepted by child-role definitions.")

(defparameter *task-agent-native-keyword-names*
  '("name" "description" "instructions" "tools" "spawns" "models"
    "reasoning-effort" "output" "blocking-p" "type" "enum" "properties"
    "required" "additional-properties" "items" "min-items" "max-items"
    "all" "auto" "object" "array" "string" "number" "integer" "boolean"
    "null")
  "Native child-role keywords other than live reasoning-effort names.")

(-> task-agent-native-keyword-name-p (string) boolean)
(defun task-agent-native-keyword-name-p (name)
  "Return true when NAME is accepted by the native child-role reader."
  (not
   (null
    (or (member name *task-agent-native-keyword-names* :test #'string-equal)
        (member name *supported-reasoning-efforts* :test #'string-equal)))))


;;;; -- Contract Diagnostics --

(-> task-agent-definition--error
    (&key (:pathname (option pathname))
          (:source keyword)
          (:line (option (integer 1)))
          (:field (option keyword))
          (:cause t)
          (:definition-name (option string)))
    null)
(defun task-agent-definition--error
    (&key pathname source line field cause definition-name)
  "Signal a structured child-role diagnostic with source context."
  (let ((message
          (format nil "Invalid ~A task agent~@[ ~S~]~@[ in ~A~]~@[ at line ~D~]~@[, field ~S~]: ~A"
                  (string-downcase (symbol-name source))
                  definition-name pathname line field cause)))
    (error 'task-agent-definition-error
           :message message
           :tool-name "task.run"
           :pathname pathname
           :source source
           :line line
           :field field
           :cause cause
           :definition-name definition-name)))

(-> task--trim (string) string)
(defun task--trim (text)
  "Return TEXT without surrounding horizontal or line whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(-> task--split-lines (string) list)
(defun task--split-lines (text)
  "Return TEXT as a list of lines without newline characters."
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim
                 '(#\Return)
                 (subseq text start (or end (length text))))
        while end
        do (setf start (1+ end))))

(-> task--proper-list-p (t) boolean)
(defun task--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (or (null value)
          (and (consp value)
               (integerp (list-length value))))
    (type-error ()
      nil)))

(-> task--plist-key-present-p (list keyword) boolean)
(defun task--plist-key-present-p (plist key)
  "Return true when proper PLIST contains KEY in a key position."
  (loop for tail on plist by #'cddr
        thereis (eq (first tail) key)))

(-> task--plist-alist
    (t list &key (:pathname (option pathname))
                  (:source keyword)
                  (:line (option (integer 1)))
                  (:definition-name (option string)))
    list)
(defun task--plist-alist
    (value allowed-fields &key pathname source line definition-name)
  "Validate native plist VALUE and return its ordered key-value pairs."
  (unless (task--proper-list-p value)
    (task-agent-definition--error
     :pathname pathname :source source :line line
     :cause "The value must be a proper list."
     :definition-name definition-name))
  (unless (evenp (length value))
    (task-agent-definition--error
     :pathname pathname :source source :line line
     :cause "The property list has a key without a value."
     :definition-name definition-name))
  (let ((seen (make-hash-table :test #'eq))
        (pairs nil))
    (loop for (key child) on value by #'cddr
          do
             (unless (keywordp key)
               (task-agent-definition--error
                :pathname pathname :source source :line line
                :cause (format nil "Property key ~S is not a keyword." key)
                :definition-name definition-name))
             (unless (member key allowed-fields :test #'eq)
               (task-agent-definition--error
                :pathname pathname :source source :line line :field key
                :cause "The property is not part of the native role contract."
                :definition-name definition-name))
             (when (gethash key seen)
               (task-agent-definition--error
                :pathname pathname :source source :line line :field key
                :cause "The property occurs more than once."
                :definition-name definition-name))
             (setf (gethash key seen) t)
             (push (cons key child) pairs))
    (nreverse pairs)))

(-> task--alist-value (keyword list) (values t boolean))
(defun task--alist-value (key pairs)
  "Return KEY's value and presence flag from ordered PAIRS."
  (let ((pair (assoc key pairs :test #'eq)))
    (values (and pair (rest pair)) (and pair t))))

(-> task--unique-list-p (list &key (:test function)) boolean)
(defun task--unique-list-p (values &key (test #'equal))
  "Return true when VALUES has no duplicate elements under TEST."
  (loop for tail on values
        always (not (member (first tail) (rest tail) :test test))))


;;;; -- Native Output Contracts --

(-> task-output--json-number-p (t) boolean)
(defun task-output--json-number-p (value)
  "Return true when VALUE is a finite JSON-representable number."
  (or (integerp value)
      (and (floatp value)
           #+sbcl
           (not (or (sb-ext:float-nan-p value)
                    (sb-ext:float-infinity-p value)))
           #-sbcl
           t)))

(-> task-output--enum-value-p (t) boolean)
(defun task-output--enum-value-p (value)
  "Return true when VALUE is a scalar supported by native output enum syntax."
  (or (stringp value)
      (task-output--json-number-p value)
      (eq value t)
      (null value)
      (eq value :null)))

(-> task-output--value-has-type-p (t keyword) boolean)
(defun task-output--value-has-type-p (value type)
  "Return true when native enum VALUE has schema TYPE."
  (case type
    (:string (stringp value))
    (:integer (integerp value))
    (:number (task-output--json-number-p value))
    (:boolean (or (eq value t) (null value)))
    (:null (eq value :null))
    (otherwise nil)))

(-> task-output--native-value-equal-p (t t) boolean)
(defun task-output--native-value-equal-p (left right)
  "Return true when native scalar values are equal under JSON semantics."
  (if (and (numberp left) (numberp right))
      (= left right)
      (equal left right)))

(-> task-output--normalize-enum
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string))
            (:type (option keyword)))
    list)
(defun task-output--normalize-enum
    (value &key pathname source definition-name type)
  "Validate and copy one native output enum VALUE."
  (unless (and (task--proper-list-p value) value)
    (task-agent-definition--error
     :pathname pathname :source source :field ':enum
     :cause "An output enum must be a non-empty proper list."
     :definition-name definition-name))
  (unless (every #'task-output--enum-value-p value)
    (task-agent-definition--error
     :pathname pathname :source source :field ':enum
     :cause "Output enum values must be strings, JSON numbers, T, NIL, or :NULL."
     :definition-name definition-name))
  (unless (task--unique-list-p
           value :test #'task-output--native-value-equal-p)
    (task-agent-definition--error
     :pathname pathname :source source :field ':enum
     :cause "Output enum values must be unique."
     :definition-name definition-name))
  (when (and type
             (not (every (lambda (item)
                           (task-output--value-has-type-p item type))
                         value)))
    (task-agent-definition--error
     :pathname pathname :source source :field ':enum
     :cause (format nil "An enum value does not have declared type ~S." type)
     :definition-name definition-name))
  (copy-list value))

(-> task-output--normalize-properties
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    list)
(defun task-output--normalize-properties
    (value &key pathname source definition-name)
  "Validate and normalize a native object property association list."
  (unless (task--proper-list-p value)
    (task-agent-definition--error
     :pathname pathname :source source :field ':properties
     :cause "Output properties must be a proper association list."
     :definition-name definition-name))
  (let ((names nil)
        (properties nil))
    (dolist (entry value)
      (unless (and (task--proper-list-p entry) (= (length entry) 2)
                   (non-empty-string-p (first entry)))
        (task-agent-definition--error
         :pathname pathname :source source :field ':properties
         :cause "Each output property must be a two-element list of name and schema."
         :definition-name definition-name))
      (when (> (length (first entry)) *task-agent-string-maximum-characters*)
        (task-agent-definition--error
         :pathname pathname :source source :field ':properties
         :cause "An output property name exceeds the string bound."
         :definition-name definition-name))
      (when (member (first entry) names :test #'string=)
        (task-agent-definition--error
         :pathname pathname :source source :field ':properties
         :cause (format nil "Output property ~S occurs more than once."
                        (first entry))
         :definition-name definition-name))
      (push (first entry) names)
      (push (list (first entry)
                  (task-output-schema-normalize
                   (second entry)
                   :pathname pathname
                   :source source
                   :definition-name definition-name))
            properties))
    (nreverse properties)))

(-> task-output-schema-normalize
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    list)
(defun task-output-schema-normalize
    (schema &key pathname source definition-name)
  "Validate and return a canonical copy of native output SCHEMA.

The supported syntax is deliberately smaller than JSON Schema. A schema is a
proper plist with :TYPE and optional :ENUM. Object schemas may add
:PROPERTIES, :REQUIRED, and :ADDITIONAL-PROPERTIES. Array schemas require
:ITEMS and may add :MIN-ITEMS and :MAX-ITEMS. An enum-only schema may omit
:TYPE. Native NIL and :NULL denote JSON false and JSON null respectively only
inside enum value positions."
  (let* ((pairs
           (task--plist-alist
            schema
            '(:type :enum :properties :required :additional-properties
              :items :min-items :max-items)
            :pathname pathname
            :source source
            :definition-name definition-name))
         (type nil)
         (type-present-p nil)
         (enum nil)
         (enum-present-p nil))
    (multiple-value-setq (type type-present-p)
      (task--alist-value :type pairs))
    (multiple-value-setq (enum enum-present-p)
      (task--alist-value :enum pairs))
    (unless (or type-present-p enum-present-p)
      (task-agent-definition--error
       :pathname pathname :source source :field ':type
       :cause "An output schema requires :TYPE or :ENUM."
       :definition-name definition-name))
    (when (and type-present-p
               (not (member type *task-output-types* :test #'eq)))
      (task-agent-definition--error
       :pathname pathname :source source :field ':type
       :cause (format nil "Unsupported output type ~S." type)
       :definition-name definition-name))
    (let* ((allowed
             (case type
               (:object
                '(:type :enum :properties :required :additional-properties))
               (:array '(:type :enum :items :min-items :max-items))
               ((:string :number :integer :boolean :null) '(:type :enum))
               (otherwise '(:enum))))
           (unknown
             (find-if-not (lambda (pair)
                            (member (first pair) allowed :test #'eq))
                          pairs)))
      (when unknown
        (task-agent-definition--error
         :pathname pathname :source source :field (first unknown)
         :cause (format nil "The property is not valid for output type ~S."
                        type)
         :definition-name definition-name)))
    (when (and enum-present-p (member type '(:object :array) :test #'eq))
      (task-agent-definition--error
       :pathname pathname :source source :field ':enum
       :cause "Object and array output schemas do not support scalar enums."
       :definition-name definition-name))
    (let ((normalized (list :type type)))
      (unless type-present-p
        (setf normalized nil))
      (when enum-present-p
        (setf normalized
              (append normalized
                      (list :enum
                            (task-output--normalize-enum
                             enum
                             :pathname pathname
                             :source source
                             :definition-name definition-name
                             :type type)))))
      (case type
        (:object
         (multiple-value-bind (properties properties-present-p)
             (task--alist-value :properties pairs)
           (let ((normalized-properties
                   (if properties-present-p
                       (task-output--normalize-properties
                        properties
                        :pathname pathname
                        :source source
                        :definition-name definition-name)
                       nil)))
             (when properties-present-p
               (setf normalized
                     (append normalized
                             (list :properties normalized-properties))))
             (multiple-value-bind (required required-present-p)
                 (task--alist-value :required pairs)
               (when required-present-p
                 (unless (and (task--proper-list-p required)
                              (every #'non-empty-string-p required)
                              (task--unique-list-p required :test #'string=))
                   (task-agent-definition--error
                    :pathname pathname :source source :field ':required
                    :cause "Object :REQUIRED must be a proper list of unique non-empty strings."
                    :definition-name definition-name))
                 (unless (every
                          (lambda (name)
                            (assoc name normalized-properties :test #'string=))
                          required)
                   (task-agent-definition--error
                    :pathname pathname :source source :field ':required
                    :cause "Every required name must have a declared property schema."
                    :definition-name definition-name))
                 (setf normalized
                       (append normalized (list :required (copy-list required))))))
             (multiple-value-bind (additional additional-present-p)
                 (task--alist-value :additional-properties pairs)
               (when additional-present-p
                 (unless (typep additional 'boolean)
                   (task-agent-definition--error
                    :pathname pathname :source source
                    :field ':additional-properties
                    :cause ":ADDITIONAL-PROPERTIES must be T or NIL."
                    :definition-name definition-name))
                 (setf normalized
                       (append normalized
                               (list :additional-properties additional))))))))
        (:array
         (multiple-value-bind (items items-present-p)
             (task--alist-value :items pairs)
           (unless items-present-p
             (task-agent-definition--error
              :pathname pathname :source source :field ':items
              :cause "An array output schema requires :ITEMS."
              :definition-name definition-name))
           (setf normalized
                 (append normalized
                         (list :items
                               (task-output-schema-normalize
                                items
                                :pathname pathname
                                :source source
                                :definition-name definition-name)))))
         (multiple-value-bind (minimum minimum-present-p)
             (task--alist-value :min-items pairs)
           (multiple-value-bind (maximum maximum-present-p)
               (task--alist-value :max-items pairs)
             (when (and minimum-present-p
                        (not (typep minimum '(integer 0))))
               (task-agent-definition--error
                :pathname pathname :source source :field ':min-items
                :cause ":MIN-ITEMS must be a nonnegative integer."
                :definition-name definition-name))
             (when (and maximum-present-p
                        (not (typep maximum '(integer 0))))
               (task-agent-definition--error
                :pathname pathname :source source :field ':max-items
                :cause ":MAX-ITEMS must be a nonnegative integer."
                :definition-name definition-name))
             (when (and minimum-present-p maximum-present-p
                        (> minimum maximum))
               (task-agent-definition--error
                :pathname pathname :source source :field ':max-items
                :cause ":MAX-ITEMS must not be smaller than :MIN-ITEMS."
                :definition-name definition-name))
             (when minimum-present-p
               (setf normalized
                     (append normalized (list :min-items minimum))))
             (when maximum-present-p
               (setf normalized
                     (append normalized (list :max-items maximum))))))))
      normalized)))

(-> task-output--native-json-value (t) t)
(defun task-output--native-json-value (value)
  "Convert one native enum value to its JSON representation."
  (cond
    ((null value) false)
    ((eq value :null) :null)
    (t value)))

(-> task-output-schema->json (list) json-object)
(defun task-output-schema->json (schema)
  "Convert validated native output SCHEMA to provider JSON Schema."
  (let ((object (json-object)))
    (loop for (key value) on schema by #'cddr
          do
             (setf
              (gethash
               (case key
                 (:additional-properties "additionalProperties")
                 (:min-items "minItems")
                 (:max-items "maxItems")
                 (otherwise (string-downcase (symbol-name key))))
               object)
              (case key
                (:type (string-downcase (symbol-name value)))
                (:enum
                 (coerce (mapcar #'task-output--native-json-value value)
                         'vector))
                (:properties
                 (let ((properties (json-object)))
                   (dolist (entry value)
                     (setf (gethash (first entry) properties)
                           (task-output-schema->json (second entry))))
                   properties))
                (:required (coerce value 'vector))
                (:additional-properties (if value t false))
                (:items (task-output-schema->json value))
                (otherwise value))))
    object))

(-> task-output--candidate-matches-type-p (t keyword) boolean)
(defun task-output--candidate-matches-type-p (value type)
  "Return true when provider JSON VALUE has native schema TYPE."
  (case type
    (:object (json-object-p value))
    (:array (and (vectorp value) (not (stringp value))))
    (:string (stringp value))
    (:integer (integerp value))
    (:number (task-output--json-number-p value))
    (:boolean (or (eq value t) (eq value false)))
    (:null (eq value :null))
    (otherwise nil)))

(-> task-output--candidate-enum-value (t) t)
(defun task-output--candidate-enum-value (value)
  "Convert provider JSON VALUE to the corresponding native enum value."
  (cond
    ((eq value false) nil)
    ((eq value :null) :null)
    (t value)))

(-> task-output-schema-valid-p (t list) boolean)
(defun task-output-schema-valid-p (value schema)
  "Return true when provider JSON VALUE satisfies validated native SCHEMA."
  (let ((type (getf schema :type))
        (enum (getf schema :enum)))
    (and
     (or (null type)
         (task-output--candidate-matches-type-p value type))
     (or (null enum)
         (member (task-output--candidate-enum-value value)
                 enum
                 :test #'task-output--native-value-equal-p))
     (case type
       (:object
        (let ((properties (getf schema :properties))
              (required (getf schema :required))
              (additional
                (if (task--plist-key-present-p
                     schema :additional-properties)
                    (getf schema :additional-properties)
                    t)))
          (and
           (every (lambda (name)
                    (nth-value 1 (gethash name value)))
                  required)
           (loop for name being the hash-keys of value
                   using (hash-value child)
                 for property = (assoc name properties :test #'string=)
                 always (if property
                            (task-output-schema-valid-p child (second property))
                            additional)))))
       (:array
        (let ((minimum (getf schema :min-items))
              (maximum (getf schema :max-items))
              (items (getf schema :items)))
          (and (or (null minimum) (>= (length value) minimum))
               (or (null maximum) (<= (length value) maximum))
               (loop for child across value
                     always (task-output-schema-valid-p child items)))))
       (otherwise t)))))


;;;; -- Exact JSON Preservation --

(-> task-json-decode (string &key (:tool-name string)) json-value)
(defun task-json-decode (source &key (tool-name "task"))
  "Decode exactly one task JSON value while distinguishing false from null."
  (handler-case
      (with-input-from-string (stream source)
        (let ((yason:*parse-json-arrays-as-vectors* t)
              (yason:*parse-json-booleans-as-symbols* t)
              (yason:*parse-json-null-as-keyword* t)
              (yason:true t)
              (end (gensym "END")))
          (let ((value (yason:parse stream)))
            (unless (eq (peek-char t stream nil end) end)
              (error 'task-error
                     :message "Task tool arguments contain trailing JSON data."
                     :tool-name tool-name))
            value)))
    (task-error (condition)
      (error condition))
    (error (condition)
      (error 'task-error
             :message (format nil "Could not decode task tool arguments: ~A"
                              condition)
             :tool-name tool-name))))

(-> task-json->sexp (t) t)
(defun task-json->sexp (value)
  "Convert validated provider JSON VALUE to a portable tagged native tree."
  (cond
    ((json-object-p value)
     (cons :object
           (sort
            (loop for key being the hash-keys of value
                    using (hash-value child)
                  collect (list key (task-json->sexp child)))
            #'string<
            :key #'first)))
    ((stringp value)
     value)
    ((vectorp value)
     (cons :array
           (loop for child across value
                 collect (task-json->sexp child))))
    ((eq value false) nil)
    ((eq value :null) :null)
    ((or (task-output--json-number-p value) (eq value t))
     value)
    (t
     (error 'task-yield-error
            :message (format nil "Yield data contains unsupported JSON value ~S."
                             value)
            :tool-name "yield.submit"))))

(-> task-sexp->json (t) json-value)
(defun task-sexp->json (value)
  "Reconstruct provider JSON from a portable tagged task result tree."
  (cond
    ((and (task--proper-list-p value)
          (consp value)
          (eq (first value) :object))
     (let ((object (json-object)))
       (dolist (entry (rest value))
         (unless (and (task--proper-list-p entry) (= (length entry) 2)
                      (stringp (first entry)))
           (error 'task-error
                  :message "A durable task object entry is malformed."
                  :tool-name "task.run"))
         (when (nth-value 1 (gethash (first entry) object))
           (error 'task-error
                  :message "A durable task object contains a duplicate key."
                  :tool-name "task.run"))
         (setf (gethash (first entry) object)
               (task-sexp->json (second entry))))
       object))
    ((and (task--proper-list-p value)
          (consp value)
          (eq (first value) :array))
     (coerce (mapcar #'task-sexp->json (rest value)) 'vector))
    ((null value) false)
    ((eq value :null) :null)
    ((or (stringp value) (task-output--json-number-p value) (eq value t))
     value)
    (t
     (error 'task-error
            :message "A durable task result contains an unsupported value."
            :tool-name "task.run"))))

(-> task--write-readable-sexp (t &key (:pretty-p boolean)) string)
(defun task--write-readable-sexp (value &key pretty-p)
  "Return VALUE as one portable readable s-expression."
  (with-standard-io-syntax
    (write-to-string value
                     :readably t
                     :escape t
                     :circle nil
                     :pretty pretty-p)))
