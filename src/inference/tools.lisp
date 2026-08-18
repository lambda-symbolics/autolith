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

(defparameter *rlm-complete-call-budget* 24
  "The default provider calls one root completion subtree may spend.")

(defparameter *rlm-complete-token-budget* 240000
  "The default combined token allowance for one root completion subtree.")

(defparameter *rlm-complete-depth-budget* 2
  "The default recursion depth below one root completion.")

(defparameter *rlm-tool-maximum-value-characters* 12000
  "The largest printed final value returned inline by rlm.complete.")

(defparameter *rlm-tool-value-preview-characters* 2000
  "The preview length shown for an externalized rlm.complete value.")

(defparameter *rlm-tool-maximum-map-tasks* 16
  "The most tasks one rlm.map tool call may fan out.")

(defparameter *rlm-map-default-concurrency* 4
  "The default number of frames one RLM-MAP runs concurrently.")

(defparameter *rlm-map-maximum-concurrency* 8
  "The largest supported RLM-MAP worker pool.")

(defclass rlm-frame-tool (tool)
  ((provider
    :initarg :provider
    :initform nil
    :reader rlm-frame-tool--provider
    :type (option model-provider)
    :documentation "The frame provider, or NIL for the active application's.")
   (budget
    :initarg :budget
    :initform nil
    :reader rlm-frame-tool--budget
    :type (option rlm-budget)
    :documentation "The enclosing frame budget nested calls descend, when inside a frame."))
  (:documentation "A model-visible operation creating bounded inference frames."))

(defclass rlm-infer-tool (rlm-frame-tool)
  ()
  (:documentation "Run one bounded inference frame as a model-visible operation."))

(defclass rlm-map-tool (rlm-frame-tool)
  ()
  (:documentation "Fan tasks out as concurrent inference frames sharing one budget."))

(defclass rlm-complete-tool (rlm-frame-tool)
  ()
  (:documentation "Run a root recursive language model over one external context."))

(-> rlm--views-parameter (string) json-object)
(defun rlm--views-parameter (description)
  "Return the tool schema for one read-only view array with DESCRIPTION."
  (json-object
   "type" "array"
   "description" description
   "items"
   (json-object
    "type" "object"
    "properties"
    (json-object
     "label" (tool-string-property "Optional short view name.")
     "text" (tool-string-property "Literal view content.")
     "uri" (tool-string-property
            "Resource whose observation becomes the view, for example workspace:src/main.lisp.")
     "object" (tool-string-property
               "Stored context object reference: context:<sha256> or the bare digest.")))))

(-> rlm--allowance-parameters () list)
(defun rlm--allowance-parameters ()
  "Return the budget allowance tool schema properties."
  (list
   "calls"
   (tool-integer-property
    "Provider call allowance for the frame subtree. Ignored inside a frame, where the enclosing budget is shared.")
   "tokens"
   (tool-integer-property
    "Token allowance for the frame subtree. Ignored inside a frame.")
   "depth"
   (tool-integer-property
    "Recursion depth allowed below the frame. Ignored inside a frame.")))

(-> rlm--shared-frame-parameters () list)
(defun rlm--shared-frame-parameters ()
  "Return the contract, capability, and allowance tool schema properties."
  (append
   (list
    "contract"
    (json-object
     "type" "object"
     "description"
     "Optional JSON Schema the answer value must satisfy. Omit it for a plain text answer.")
    "capabilities"
    (json-object
     "type" "string"
     "enum" (json-array "none" "read")
     "description"
     "Frame capabilities: none for a pure call over the views, read to also allow workspace resource reads, content search, and nested rlm calls."))
   (rlm--allowance-parameters)))

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
   "Run one bounded inference frame: a separate model call over only the supplied read-only views, isolated from this conversation. Use it to analyze inputs without loading them here. The frame sees nothing but its views, so pass everything it needs. Returns the frame's value, its trace identifier, and the remaining budget."
   :parameters
   (tool-object-schema
    (apply #'json-object
           "task"
           (tool-string-property
            "The single question or instruction the frame must answer.")
           "views"
           (rlm--views-parameter
            "Read-only context views. Each view carries text, a resource URI, or a stored context object reference, plus an optional label.")
           (rlm--shared-frame-parameters))
    '("task"))))

(-> rlm-map-tool-create
    (&key (:provider (option model-provider)) (:budget (option rlm-budget)))
    rlm-map-tool)
(defun rlm-map-tool-create (&key provider budget)
  "Create the rlm.map tool, nesting under BUDGET when inside a frame."
  (make-instance
   'rlm-map-tool
   :namespace "rlm"
   :name "map"
   :provider provider
   :budget budget
   :description
   "Fan tasks out as concurrent bounded inference frames sharing one budget, isolated from this conversation. Use it to apply one question to many snippets or files at once. Results keep task order; a failed frame reports its error without discarding the others. Returns each frame's value and trace plus the remaining budget."
   :parameters
   (tool-object-schema
    (apply #'json-object
           "tasks"
           (json-object
            "type" "array"
            "description"
            (format nil
                    "The frames to run, at most ~D. Each carries its task and optional views."
                    *rlm-tool-maximum-map-tasks*)
            "items"
            (json-object
             "type" "object"
             "properties"
             (json-object
              "task" (tool-string-property
                      "The question or instruction for this frame.")
              "views" (rlm--views-parameter
                       "Read-only context views for this frame."))
             "required" (json-array "task")))
           "concurrency"
           (tool-integer-property
            "How many frames run at once.")
           (rlm--shared-frame-parameters))
    '("tasks"))))

(-> rlm-complete-tool-create
    (&key (:provider (option model-provider)) (:budget (option rlm-budget)))
    rlm-complete-tool)
(defun rlm-complete-tool-create (&key provider budget)
  "Create the rlm.complete tool, nesting under BUDGET when inside a frame."
  (make-instance
   'rlm-complete-tool
   :namespace "rlm"
   :name "complete"
   :provider provider
   :budget budget
   :description
   "Run a root recursive language model over one large external context: the content stays outside the root model context, only selected bounded slices enter sub-inferences, and a dedicated Lisp environment programmatically slices it, fans sub-inferences over the pieces, and records the final value. Use it when the input is far too large to read into this conversation and must be processed nearly in full. Returns the recorded value, the root trace identifier, and the remaining budget."
   :parameters
   (tool-object-schema
    (apply #'json-object
           "task"
           (tool-string-property
            "The question or instruction the run must answer.")
           "context"
           (json-object
            "type" "object"
            "description"
            "The external context: text, a resource URI, or a stored context object reference, plus an optional label."
            "properties"
            (json-object
             "label" (tool-string-property "Optional short context name.")
             "text" (tool-string-property "Literal context content.")
             "uri" (tool-string-property
                    "Resource whose observation becomes the context.")
             "object" (tool-string-property
                       "Stored context object reference: context:<sha256> or the bare digest.")))
           ;; Root runs return whatever finish records and decide their own
           ;; decomposition, so only the allowance parameters apply.
           (rlm--allowance-parameters))
    '("task" "context"))))

(-> rlm--frame-tool-allowlist () list)
(defun rlm--frame-tool-allowlist ()
  "Return the canonical tool names a read-capability frame may call."
  (list* "rlm.infer" "rlm.map" (copy-list *rlm-frame-read-tool-names*)))

(-> rlm-register-tools
    (tool-registry
     &key (:provider (option model-provider))
          (:budget (option rlm-budget))
          (:complete-p boolean))
    tool-registry)
(defun rlm-register-tools (registry &key provider budget (complete-p t))
  "Register the rlm namespace in REGISTRY, nesting under BUDGET in frames.

COMPLETE-P is refused inside frames: rlm.complete starts an
environment evaluating arbitrary Lisp with user privileges, so only
the primary agent may launch one."
  (tool-registry-register registry
                          (rlm-infer-tool-create :provider provider
                                                 :budget budget))
  (tool-registry-register registry
                          (rlm-map-tool-create :provider provider
                                               :budget budget))
  (when complete-p
    (tool-registry-register registry
                            (rlm-complete-tool-create :provider provider
                                                      :budget budget)))
  registry)

(-> rlm--frame-registry (tool-registry model-provider rlm-budget) tool-registry)
(defun rlm--frame-registry (source provider budget)
  "Build a frame registry of SOURCE's read-only tools plus nested rlm calls."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools source))
      (when (member (tool-canonical-name tool)
                    *rlm-frame-read-tool-names*
                    :test #'string=)
        (tool-registry-register registry tool)))
    (rlm-register-tools registry :provider provider :budget budget
                                 :complete-p nil)))

(-> rlm--result-sexp (t) string)
(defun rlm--result-sexp (value)
  "Render VALUE as one model-visible s-expression.

Tool results are read by the model, not by the Lisp reader, so plain
printing keeps base strings as ordinary string syntax instead of the
strict writer's #A array forms."
  (with-standard-io-syntax
    (write-to-string value :readably nil :escape t :circle t :pretty t)))

(-> rlm--tool-resource-registry (tool-context) resource-registry)
(defun rlm--tool-resource-registry (context)
  "Return the resource registry serving CONTEXT's uri designators.

Frame registries are fresh, so the resolvers ride on the copied
resource.read tool when one exists."
  (let* ((registry (tool-context-registry context))
         (read-tool (and registry
                         (tool-registry-find registry "resource" "read"))))
    (cond
      (read-tool (resource-tool-resource-registry read-tool))
      (registry (tool-registry-resource-registry registry))
      (t (error 'rlm-view-error
                :designator "uri"
                :message "no resource registry serves this call")))))

(-> rlm--tool-resource-content (tool-context string) string)
(defun rlm--tool-resource-content (context uri)
  "Materialize URI through the resource protocol under CONTEXT's authority.

Resolution honors the restricted readable scheme set, so frames stay
confined to the schemes their restriction permits."
  (let* ((resource (resource-registry-resolve
                    (rlm--tool-resource-registry context) uri context))
         (content (resource-observation-content
                   (resource-observe resource context))))
    (unless (stringp content)
      (error 'rlm-view-error
             :designator uri
             :message "the resource observation carries no text"))
    content))

(-> rlm--tool-object-content (tool-context string) string)
(defun rlm--tool-object-content (context reference)
  "Return the stored context object REFERENCE names, by digest or URI.

Dereferencing routes through the context resource scheme, so object
references obey the same authority and scheme restrictions as every
other uri designator."
  (rlm--tool-resource-content
   context
   (if (uiop:string-prefix-p "context:" reference)
       reference
       (format nil "context:~A" reference))))

(-> rlm--tool-views (t tool-context) list)
(defun rlm--tool-views (views context)
  "Convert the tool VIEWS argument into view designator plists.

Model-visible views carry literal text, a resource URI resolved under
CONTEXT's authority, or a stored context object reference; raw
filesystem paths are only a programmatic Lisp designator."
  (unless (or (null views) (vectorp views) (listp views))
    (error 'rlm-view-error
           :designator views
           :message "expected an array of view objects"))
  (loop for view in (coerce views 'list)
        collect
        (progn
          (unless (json-object-p view)
            (error 'rlm-view-error
                   :designator view
                   :message "expected a view object"))
          (let* ((label (json-get view "label"))
                 (text (json-get view "text"))
                 (uri (json-get view "uri"))
                 (object (json-get view "object"))
                 (effective-label (or label (and (stringp uri) uri))))
            (unless (= (count-if #'stringp (list text uri object)) 1)
              (error 'rlm-view-error
                     :designator view
                     :message
                     "expected exactly one of text, a resource uri, or a context object reference"))
            (append
             (when effective-label (list ':label effective-label))
             (list ':content
                   (cond
                     ((stringp text) text)
                     ((stringp uri)
                      (rlm--tool-resource-content context uri))
                     (t
                      (rlm--tool-object-content context object)))))))))

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

(-> rlm--tool-budget
    (rlm-frame-tool hash-table string
     &key (:calls (integer 1)) (:tokens (integer 1)) (:depth (integer 0)))
    rlm-budget)
(defun rlm--tool-budget
    (tool arguments task &key (calls *rlm-default-call-budget*)
                              (tokens *rlm-default-token-budget*)
                              (depth *rlm-default-depth-budget*))
  "Return the frame budget for one tool call, descending inside frames."
  (let ((parent (rlm-frame-tool--budget tool)))
    (if parent
        (rlm-budget-descend parent :task task)
        (rlm-budget-create
         :calls (rlm--bounded-tool-integer
                 arguments "calls" calls
                 1 *rlm-tool-maximum-call-budget*)
         :tokens (rlm--bounded-tool-integer
                  arguments "tokens" tokens
                  1 *rlm-tool-maximum-token-budget*)
         :depth (rlm--bounded-tool-integer
                 arguments "depth" depth
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
             (views (rlm--tool-views (gethash "views" arguments) context))
             (contract (let ((schema (gethash "contract" arguments)))
                         (if schema
                             (rlm--json-schema->contract schema)
                             ':text)))
             (capabilities (rlm--tool-capabilities
                            (gethash "capabilities" arguments)))
             (budget (rlm--tool-budget tool arguments task))
             (provider (or (rlm-frame-tool--provider tool)
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
           (rlm--result-sexp
            (list ':value value
                  ':trace trace-identifier
                  ':calls-remaining (rlm-budget-remaining-calls budget)
                  ':tokens-remaining (rlm-budget-remaining-tokens budget))))))
    ((or rlm-budget-exhausted rlm-inference-error rlm-view-error task-error
         resource-scheme-unknown resource-access-denied
         resource-operation-unsupported)
      (condition)
      (tool-failure (format nil "~A" condition)))))

(-> rlm--tool-complete-object (tool-context t) rlm-context-object)
(defun rlm--tool-complete-object (context argument)
  "Intern the tool's root context ARGUMENT as a context object."
  (unless (json-object-p argument)
    (error 'rlm-view-error
           :designator argument
           :message "expected one context object"))
  (let ((configuration (tool-context-configuration context))
        (label (json-get argument "label"))
        (text (json-get argument "text"))
        (uri (json-get argument "uri"))
        (object (json-get argument "object")))
    (unless (= (count-if #'stringp (list text uri object)) 1)
      (error 'rlm-view-error
             :designator argument
             :message
             "expected exactly one of text, a resource uri, or a context object reference"))
    (cond
      ((stringp text)
       (rlm-context-intern configuration text :label label))
      ((stringp uri)
       (rlm-context-intern configuration
                           (rlm--tool-resource-content context uri)
                           :label (or label uri)))
      (t
       (rlm-context-intern configuration
                           (rlm--tool-object-content context object)
                           :label label)))))

(defmethod tool-execute
    ((tool rlm-complete-tool) (context tool-context) (arguments hash-table))
  "Run one root recursive language model and return its recorded value."
  (handler-case
      (let* ((task (tool-argument arguments "task" :required t))
             (task (if (stringp task)
                       task
                       (error 'rlm-inference-error
                              :message "The run task must be a string.")))
             (object (rlm--tool-complete-object
                      context (gethash "context" arguments)))
             (budget (rlm--tool-budget tool arguments task
                                       :calls *rlm-complete-call-budget*
                                       :tokens *rlm-complete-token-budget*
                                       :depth *rlm-complete-depth-budget*))
             (provider (or (rlm-frame-tool--provider tool)
                           (rlm--environment))))
        (multiple-value-bind (value trace-identifier)
            (rlm-complete task
                          :context object
                          :budget budget
                          :provider provider
                          :configuration (tool-context-configuration context))
          (let* ((printed (rlm--result-sexp value))
                 (value-fields
                   ;; A large final value is externalized as a stored context
                   ;; object with a bounded preview instead of flooding the
                   ;; caller's conversation.
                   (if (<= (length printed)
                           *rlm-tool-maximum-value-characters*)
                       (list ':value value)
                       (list ':value-preview
                             (subseq printed 0
                                     *rlm-tool-value-preview-characters*)
                             ':value-context
                             (format nil "context:~A"
                                     (rlm-context-object-digest
                                      (rlm-context-intern
                                       (tool-context-configuration context)
                                       printed
                                       :label "rlm result")))))))
            (tool-success
             (rlm--result-sexp
              (append
               value-fields
               (list ':trace trace-identifier
                     ':context (format nil "context:~A"
                                       (rlm-context-object-digest object))
                     ':calls-remaining (rlm-budget-remaining-calls budget)
                     ':tokens-remaining (rlm-budget-remaining-tokens
                                         budget))))))))
    ((or rlm-budget-exhausted rlm-inference-error rlm-view-error task-error
         resource-scheme-unknown resource-access-denied
         resource-operation-unsupported)
      (condition)
      (tool-failure (format nil "~A" condition)))))

(-> rlm--tool-map-tasks (t tool-context) list)
(defun rlm--tool-map-tasks (tasks context)
  "Convert the tool TASKS argument into RLM-MAP task plists."
  (unless (and (vectorp tasks) (plusp (length tasks)))
    (error 'rlm-inference-error
           :message "The map tasks must be a non-empty array of task objects."))
  (when (> (length tasks) *rlm-tool-maximum-map-tasks*)
    (error 'rlm-inference-error
           :message (format nil "One rlm.map call may fan out at most ~D tasks."
                            *rlm-tool-maximum-map-tasks*)))
  (loop for element across tasks
        collect (progn
                  (unless (json-object-p element)
                    (error 'rlm-inference-error
                           :message "Each map task must be one task object."))
                  (let ((task (json-get element "task")))
                    (unless (and (stringp task) (non-empty-string-p task))
                      (error 'rlm-inference-error
                             :message "Each map task requires non-empty task text."))
                    (append
                     (list ':task task)
                     (let ((views (rlm--tool-views
                                   (json-get element "views")
                                   context)))
                       (when views
                         (list ':context views))))))))

(defmethod tool-execute
    ((tool rlm-map-tool) (context tool-context) (arguments hash-table))
  "Fan tool tasks out as inference frames and return their ordered results."
  (handler-case
      (let* ((tasks (rlm--tool-map-tasks (gethash "tasks" arguments) context))
             (contract (let ((schema (gethash "contract" arguments)))
                         (if schema
                             (rlm--json-schema->contract schema)
                             ':text)))
             (capabilities (rlm--tool-capabilities
                            (gethash "capabilities" arguments)))
             (budget (rlm--tool-budget tool arguments "rlm.map"))
             (provider (or (rlm-frame-tool--provider tool)
                           (rlm--environment)))
             (concurrency (rlm--bounded-tool-integer
                           arguments "concurrency"
                           *rlm-map-default-concurrency*
                           1 *rlm-map-maximum-concurrency*))
             (results
               (rlm-map tasks
                        :contract contract
                        :budget budget
                        :capabilities capabilities
                        :provider provider
                        :configuration (tool-context-configuration context)
                        :source-registry (tool-context-registry context)
                        :concurrency concurrency)))
        (tool-success
         (rlm--result-sexp
          (list ':results results
                ':calls-remaining (rlm-budget-remaining-calls budget)
                ':tokens-remaining (rlm-budget-remaining-tokens budget)))))
    ((or rlm-budget-exhausted rlm-inference-error rlm-view-error task-error
         resource-scheme-unknown resource-access-denied
         resource-operation-unsupported)
      (condition)
      (tool-failure (format nil "~A" condition)))))
