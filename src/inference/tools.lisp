(in-package #:autolith)

;;;; -- Recursive Inference Tools --

(defparameter *rlm-frame-read-tool-names*
  '("resource.read" "search.files" "search.glob" "search.content")
  "The read-only tools copied from the source registry into frames.")

(defparameter *rlm-tool-maximum-call-budget* 32
  "The largest call allowance one rlm.infer tool call may request.")

(defparameter *rlm-tool-maximum-token-budget* 400000
  "The largest token allowance one rlm.infer tool call may request.")

(defparameter *rlm-tool-maximum-depth-budget* 4
  "The largest recursion depth one rlm.infer tool call may request.")

(defclass rlm-infer-tool (tool)
  ((provider
    :initarg :provider
    :initform nil
    :reader rlm-infer-tool--provider
    :type (option model-provider)
    :documentation "The frame provider, or NIL for the active application's.")
   (budget
    :initarg :budget
    :initform nil
    :reader rlm-infer-tool--budget
    :type (option rlm-budget)
    :documentation "The enclosing frame budget nested calls descend, when inside a frame."))
  (:documentation "Run one bounded inference frame as a model-visible operation."))

(-> rlm-infer-tool-create
    (&key (:provider (option model-provider)) (:budget (option rlm-budget)))
    rlm-infer-tool)
(defun rlm-infer-tool-create (&key provider budget)
  "Create the rlm.infer tool, nesting under BUDGET when inside a frame."
  (make-instance
   'rlm-infer-tool
   :namespace "rlm"
   :name "infer"
   :provider provider
   :budget budget
   :description
   "Run one bounded inference frame: a separate model call over only the supplied read-only views, isolated from this conversation. Use it to analyze inputs without loading them here, or to fan a question out over many snippets. The frame sees nothing but its views, so pass everything it needs. Returns the frame's value, its trace identifier, and the remaining budget."
   :parameters
   (tool-object-schema
    (json-object
     "task"
     (tool-string-property
      "The single question or instruction the frame must answer.")
     "views"
     (json-object
      "type" "array"
      "description"
      "Read-only context views. Each view carries text or the path of a file to read, plus an optional label."
      "items"
      (json-object
       "type" "object"
       "properties"
       (json-object
        "label" (tool-string-property "Optional short view name.")
        "text" (tool-string-property "Literal view content.")
        "path" (tool-string-property "File whose content becomes the view."))))
     "contract"
     (json-object
      "type" "object"
      "description"
      "Optional JSON Schema the answer object must satisfy. Omit it for a plain text answer.")
     "capabilities"
     (json-object
      "type" "string"
      "enum" (json-array "none" "read")
      "description"
      "Frame capabilities: none for a pure call over the views, read to also allow workspace resource reads, content search, and nested rlm.infer.")
     "calls"
     (tool-integer-property
      "Provider call allowance for the frame subtree. Ignored inside a frame, where the enclosing budget is shared.")
     "tokens"
     (tool-integer-property
      "Token allowance for the frame subtree. Ignored inside a frame.")
     "depth"
     (tool-integer-property
      "Recursion depth allowed below the frame. Ignored inside a frame."))
    '("task"))))

(-> rlm--frame-tool-allowlist () list)
(defun rlm--frame-tool-allowlist ()
  "Return the canonical tool names a read-capability frame may call."
  (cons "rlm.infer" (copy-list *rlm-frame-read-tool-names*)))

(-> rlm--frame-registry (tool-registry model-provider rlm-budget) tool-registry)
(defun rlm--frame-registry (source provider budget)
  "Build a frame registry of SOURCE's read-only tools plus nested rlm.infer."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools source))
      (when (member (tool-canonical-name tool)
                    *rlm-frame-read-tool-names*
                    :test #'string=)
        (tool-registry-register registry tool)))
    (tool-registry-register registry
                            (rlm-infer-tool-create :provider provider
                                                   :budget budget))
    registry))

(-> rlm--tool-views (t) list)
(defun rlm--tool-views (views)
  "Convert the tool VIEWS argument into view designator plists."
  (unless (or (null views) (vectorp views) (listp views))
    (error 'rlm-view-error
           :designator views
           :message "expected an array of view objects"))
  (loop for view in (coerce views 'list)
        collect (progn
                  (unless (json-object-p view)
                    (error 'rlm-view-error
                           :designator view
                           :message "expected a view object"))
                  (let ((label (json-get view "label"))
                        (text (json-get view "text"))
                        (path (json-get view "path")))
                    (append
                     (when label (list ':label label))
                     (when text (list ':content text))
                     (when path
                       (list ':path (parse-namestring path))))))))

(-> rlm--json-schema-type (t) keyword)
(defun rlm--json-schema-type (type)
  "Convert a JSON Schema TYPE string to the native schema keyword."
  (unless (stringp type)
    (error 'rlm-inference-error
           :message "A contract type must be a JSON Schema type string."))
  (intern (string-upcase type) '#:keyword))

(-> rlm--json-schema->contract (t) list)
(defun rlm--json-schema->contract (schema)
  "Convert the JSON Schema object SCHEMA to a native output schema plist."
  (unless (json-object-p schema)
    (error 'rlm-inference-error
           :message "A contract must be one JSON Schema object."))
  (let ((contract nil))
    (let ((maximum (or (json-get schema "maxItems")
                       (json-get schema "max-items"))))
      (when maximum
        (setf contract (list* ':max-items maximum contract))))
    (let ((minimum (or (json-get schema "minItems")
                       (json-get schema "min-items"))))
      (when minimum
        (setf contract (list* ':min-items minimum contract))))
    (let ((items (json-get schema "items")))
      (when items
        (setf contract
              (list* ':items (rlm--json-schema->contract items) contract))))
    (multiple-value-bind (additional additional-present-p)
        (gethash "additionalProperties" schema)
      (when additional-present-p
        (setf contract
              (list* ':additional-properties (and additional t) contract))))
    (let ((required (json-get schema "required")))
      (when required
        (setf contract
              (list* ':required (coerce required 'list) contract))))
    (let ((properties (json-get schema "properties")))
      (when properties
        (unless (json-object-p properties)
          (error 'rlm-inference-error
                 :message "Contract properties must be one JSON object."))
        (setf contract
              (list* ':properties
                     (sort
                      (loop for name being the hash-keys of properties
                              using (hash-value child)
                            collect (list name
                                          (rlm--json-schema->contract child)))
                      #'string<
                      :key #'first)
                     contract))))
    (let ((enum (json-get schema "enum")))
      (when enum
        (setf contract (list* ':enum (coerce enum 'list) contract))))
    (let ((type (json-get schema "type")))
      (when type
        (setf contract (list* ':type (rlm--json-schema-type type) contract))))
    (unless contract
      (error 'rlm-inference-error
             :message "A contract requires at least a type or an enum."))
    contract))

(-> rlm--tool-capabilities (t) (option keyword))
(defun rlm--tool-capabilities (capabilities)
  "Convert the tool CAPABILITIES argument to the infer capabilities keyword."
  (cond
    ((or (null capabilities) (equal capabilities "none")) nil)
    ((equal capabilities "read") ':read)
    (t (error 'rlm-inference-error
              :message "Frame capabilities must be none or read."))))

(-> rlm--bounded-tool-integer
    (hash-table string (integer 0) (integer 0) (integer 0))
    (integer 0))
(defun rlm--bounded-tool-integer (arguments name fallback minimum maximum)
  "Return the integer argument NAME clamped into [MINIMUM, MAXIMUM]."
  (let ((value (gethash name arguments)))
    (cond
      ((null value) fallback)
      ((integerp value) (max minimum (min maximum value)))
      (t (error 'rlm-inference-error
                :message (format nil "The ~A allowance must be an integer."
                                 name))))))

(-> rlm--tool-budget (rlm-infer-tool hash-table string) rlm-budget)
(defun rlm--tool-budget (tool arguments task)
  "Return the frame budget for one tool call, descending inside frames."
  (let ((parent (rlm-infer-tool--budget tool)))
    (if parent
        (rlm-budget-descend parent :task task)
        (rlm-budget-create
         :calls (rlm--bounded-tool-integer
                 arguments "calls" *rlm-default-call-budget*
                 1 *rlm-tool-maximum-call-budget*)
         :tokens (rlm--bounded-tool-integer
                  arguments "tokens" *rlm-default-token-budget*
                  1 *rlm-tool-maximum-token-budget*)
         :depth (rlm--bounded-tool-integer
                 arguments "depth" *rlm-default-depth-budget*
                 0 *rlm-tool-maximum-depth-budget*)))))

(defmethod tool-execute
    ((tool rlm-infer-tool) (context tool-context) (arguments hash-table))
  "Run one inference frame and return its value, trace, and remaining budget."
  (handler-case
      (let* ((task (tool-argument arguments "task" :required t))
             (task (if (stringp task)
                       task
                       (error 'rlm-inference-error
                              :message "The frame task must be a string.")))
             (views (rlm--tool-views (gethash "views" arguments)))
             (contract (let ((schema (gethash "contract" arguments)))
                         (if schema
                             (rlm--json-schema->contract schema)
                             ':text)))
             (capabilities (rlm--tool-capabilities
                            (gethash "capabilities" arguments)))
             (budget (rlm--tool-budget tool arguments task))
             (provider (or (rlm-infer-tool--provider tool)
                           (rlm--environment))))
        (multiple-value-bind (value trace-identifier)
            (infer task
                   :context views
                   :contract contract
                   :budget budget
                   :capabilities capabilities
                   :provider provider
                   :configuration (tool-context-configuration context)
                   :source-registry (tool-context-registry context))
          (tool-success
           (task--write-readable-sexp
            (list ':value value
                  ':trace trace-identifier
                  ':calls-remaining (rlm-budget-remaining-calls budget)
                  ':tokens-remaining (rlm-budget-remaining-tokens budget))
            :pretty-p t))))
    ((or rlm-budget-exhausted rlm-inference-error rlm-view-error task-error)
      (condition)
      (tool-failure (format nil "~A" condition)))))
