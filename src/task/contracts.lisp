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
    :documentation "The project, user, bundled, or programmatic definition origin.")
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


;;;; -- Structured Output Adapters --

(-> task-output--json-number-p (t) boolean)
(defun task-output--json-number-p (value)
  "Check numeric values at native role and task-result boundaries."
  (cl-llm-provider-api:output-json-number-p value))

(-> task-output-schema-normalize
    (t &key (:pathname (option pathname)) (:source keyword)
       (:definition-name (option string))) list)
(defun task-output-schema-normalize (schema &key pathname source definition-name)
  "Validate native output SCHEMA, adding child-role provenance to library diagnostics."
  (handler-case
      (cl-llm-provider-api:output-schema-normalize
       schema :maximum-nodes *task-agent-form-maximum-nodes*
              :maximum-depth *task-agent-form-maximum-depth*
              :property-name-limit *task-agent-string-maximum-characters*)
    (cl-llm-provider-api:output-contract-error (condition)
      (task-agent-definition--error
       :pathname pathname :source source :definition-name definition-name
       :field (cl-llm-provider-api:output-contract-error-field condition)
       :cause (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-output-schema->json (list) json-object)
(defun task-output-schema->json (schema)
  "Project a validated task output contract into provider JSON Schema."
  (cl-llm-provider-api:output-schema->json schema))

(-> task-output-schema-valid-p (t list) boolean)
(defun task-output-schema-valid-p (value schema)
  "Check a provider value against its validated task contract."
  (cl-llm-provider-api:output-schema-valid-p value schema))

(-> task-json-decode (string &key (:tool-name string)) json-value)
(defun task-json-decode (source &key (tool-name "task"))
  "Decode exact JSON arguments and attach canonical tool failure metadata."
  (handler-case
      (cl-llm-provider-api:output-json-decode source)
    (cl-llm-provider-api:output-value-error (condition)
      (error 'task-error :tool-name tool-name
             :message (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-json->sexp (t) t)
(defun task-json->sexp (value)
  "Project provider output into a portable tagged task result."
  (handler-case
      (cl-llm-provider-api:output-json->sexp value)
    (cl-llm-provider-api:output-value-error (condition)
      (error 'task-yield-error :tool-name "yield.submit"
             :message (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-sexp->json (t) json-value)
(defun task-sexp->json (value)
  "Restore provider JSON from a durable tagged task result."
  (handler-case
      (cl-llm-provider-api:output-sexp->json value)
    (cl-llm-provider-api:output-value-error (condition)
      (error 'task-error :tool-name "task.run"
             :message (cl-llm-provider-api:provider-api-error-message condition)))))
(-> task--write-readable-sexp (t &key (:pretty-p boolean)) string)
(defun task--write-readable-sexp (value &key pretty-p)
  "Return VALUE as one portable readable s-expression."
  (with-standard-io-syntax
    (write-to-string value
                     :readably t
                     :escape t
                     :circle nil
                     :pretty pretty-p)))
