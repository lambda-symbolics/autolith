(in-package #:autolith)

;;;; -- Task Conditions --

(define-condition task-error
    (tool-error)
  ((task-id
    :initarg       :task-id
    :initform      nil
    :reader        task-error-task-id
    :type          (option string)
    :documentation "The child or job identifier involved in the failure, when known."))
  (:documentation
   "A task request, child run, or task job violated its contract."))

(-> task--trim (string) string)
(defun task--trim (text)
  "Return TEXT without surrounding horizontal or line whitespace."
  (serapeum:trim-whitespace text))

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
  "Check numeric values at task-result boundaries."
  (cl-llm-provider-api:output-json-number-p value))

(-> task-output-schema-normalize (t &key (:source keyword)) list)
(defun task-output-schema-normalize (schema &key (source ':programmatic))
  "Validate a native output schema and retain structured boundary failures."
  (declare (ignore source))
  (handler-case
      (cl-llm-provider-api:output-schema-normalize
       schema :maximum-nodes 8192 :maximum-depth 128 :property-name-limit 32768)
    (cl-llm-provider-api:output-contract-error (condition)
      (error 'task-error :message (princ-to-string condition)))))

(-> task-output-schema->json (list) json-object)
(defun task-output-schema->json (schema)
  "Project a validated task output contract into provider JSON Schema."
  (cl-llm-provider-api:output-schema->json schema))

(-> task-json-decode (string &key (:tool-name string)) json-value)
(defun task-json-decode (source &key (tool-name "task"))
  "Decode exact JSON arguments and attach canonical tool failure metadata."
  (handler-case
      (cl-llm-provider-api:output-json-decode source)
    (cl-llm-provider-api:output-value-error (condition)
      (error 'task-error :tool-name tool-name
             :message               (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-json-normalize (t &key (:tool-name string)) json-value)
(defun task-json-normalize (arguments &key (tool-name "task"))
  "Detach native arguments using the task decoder's output marker policy."
  (handler-case
      (json-normalize arguments :input-false-value (json-false)
                                :false-value 'yason:false :null-value ':null)
    (error ()
      (error 'task-error :tool-name tool-name
                        :message    "Could not decode the JSON value."))))

(-> task--character-strings (t) t)
(defun task--character-strings (value)
  "Copy string leaves in a bounded acyclic cons tree into fresh character strings.

Fresh strings avoid implementation array syntax and shared-string reader labels.
Repeated subtrees are copied independently; cyclic conses signal TASK-ERROR."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (declare (ignore child))
         (if (stringp item)
             (make-array (length item) :element-type 'character :initial-contents item)
             item)))
    (portable-value-error ()
      (error 'task-error :tool-name "child.spawn"
             :message               "Task result data is cyclic or exceeds its structural bounds."))))

(-> task--write-readable-sexp (t &key (:pretty-p boolean)) string)
(defun task--write-readable-sexp (value &key pretty-p)
  "Return VALUE as one portable readable s-expression."
  (with-standard-io-syntax
    (write-to-string (task--character-strings value)
                     :readably t
                     :escape   t
                     :circle   nil
                     :pretty   pretty-p)))
