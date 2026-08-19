(in-package #:autolith)

;;;; -- Tool Metadata --

(defclass tool ()
  ((namespace
    :initarg :namespace
    :reader tool-namespace
    :type non-empty-string
    :documentation "The Responses namespace containing this tool.")
   (name
    :initarg :name
    :reader tool-name
    :type non-empty-string
    :documentation "The function name inside the namespace.")
   (description
    :initarg :description
    :reader tool-description
    :type non-empty-string
    :documentation "The model-visible operation documentation.")
   (parameters
    :initarg :parameters
    :reader tool-parameters
    :type json-object
    :documentation "The JSON Schema accepted by this tool."))
  (:documentation "A documented, model-visible operation."))

(defclass resource-tool (tool)
  ((resource-registry
    :initarg :resource-registry
    :reader resource-tool-resource-registry
    :type resource-registry
    :documentation "The exact per-agent resolver registry owning this tool."))
  (:documentation "A model-facing tool resolving resources in one agent registry."))

(defclass resource-read-tool (resource-tool)
  ()
  (:documentation "Read one resource through its model-facing CLOS protocol."))

(defclass resource-edit-tool (resource-tool)
  ()
  (:documentation "Edit one resource through its model-facing CLOS protocol."))

(defclass lisp-tool (tool)
  ()
  (:documentation "A tool whose operation is isolated in a named Lisp worker."))

(defclass web-run-tool (tool)
  ()
  (:documentation "A tool that performs provider-backed web search."))

(defclass self-tool (tool)
  ()
  (:documentation "A tool whose operation targets the active Autolith image."))

(defclass mutable-self-tool (self-tool)
  ()
  (:documentation "A self tool omitted when Autolith runs in immutable mode."))

(defclass lisp-eval-tool (lisp-tool)
  ()
  (:documentation "Evaluate one Common Lisp form in the worker."))


(defclass lisp-load-system-tool (lisp-tool)
  ()
  (:documentation "Load one ASDF or Quicklisp system in the worker."))

(defclass lisp-describe-tool (lisp-tool)
  ()
  (:documentation "Describe one Lisp object or symbol in the worker."))

(defclass lisp-source-tool (lisp-tool)
  ()
  (:documentation "Read exact matching source for one worker definition."))

(defclass lisp-run-tests-tool (lisp-tool)
  ()
  (:documentation "Run an ASDF system's tests in the worker."))

(defclass lisp-reset-tool (lisp-tool)
  ()
  (:documentation "Reset one named Lisp REPL from a selected image."))

(defclass lisp-start-tool (lisp-tool)
  ()
  (:documentation "Start one named Lisp REPL from a selected image."))

(defclass lisp-stop-tool (lisp-tool)
  ()
  (:documentation "Stop and forget one named Lisp REPL."))

(defclass lisp-repls-tool (lisp-tool)
  ()
  (:documentation "List the active named Lisp REPLs."))

(defclass lisp-images-tool (lisp-tool)
  ()
  (:documentation "List pristine and saved Lisp worker images with notes."))

(defclass lisp-save-image-tool (lisp-tool)
  ()
  (:documentation "Save one named REPL as an immutable worker image."))


(defclass self-eval-tool (mutable-self-tool)
  ()
  (:documentation "Evaluate one exploratory form in the active image."))

(defclass self-redefine-tool (mutable-self-tool)
  ()
  (:documentation "Compile and install one exploratory top-level definition."))

(defclass self-set-tool (mutable-self-tool)
  ()
  (:documentation "Set one active global binding to an evaluated value."))

(defclass self-persist-definition-tool (mutable-self-tool)
  ()
  (:documentation "Install and privately commit one complete definition."))

(defclass self-status-tool (self-tool)
  ()
  (:documentation "Summarize active-image mutation and recovery state."))

(defclass self-discard-tool (mutable-self-tool)
  ()
  (:documentation "Restore and discard one effective exploratory mutation."))

(defclass self-exercise-tool (mutable-self-tool)
  ()
  (:documentation "Run and journal one focused live-mutation exercise."))

(defclass self-diff-tool (self-tool)
  ()
  (:documentation "Show uncommitted reconstructible active-image mutations."))

(defclass self-commit-tool (mutable-self-tool)
  ()
  (:documentation "Persist pending live mutations as a private image commit."))

(defclass self-checkpoint-tool (mutable-self-tool)
  ()
  (:documentation "Save the active image as a retained working generation."))

(defclass self-generations-tool (self-tool)
  ()
  (:documentation "List retained working generations and compatibility."))

(defclass self-rollback-tool (mutable-self-tool)
  ()
  (:documentation "Select a retained generation and request immediate rollback."))

(-> tool-canonical-name (tool) string)
(defun tool-canonical-name (tool)
  "Return TOOL's dotted human-readable name."
  (format nil "~A.~A" (tool-namespace tool) (tool-name tool)))

(-> tool-authorization-identity-fields (tool) list)
(defgeneric tool-authorization-identity-fields (tool)
  (:documentation
   "Return ordered label and exact value pairs identifying TOOL for approval."))

(defmethod tool-authorization-identity-fields ((tool tool))
  "Identify an ordinary TOOL by its canonical Autolith name."
  (list (list "tool" (tool-canonical-name tool))))

(-> tool-object-schema (json-object list) json-object)
(defun tool-object-schema (properties required)
  "Return a closed JSON object schema with PROPERTIES and REQUIRED names."
  (json-object
   "type" "object"
   "properties" properties
   "required" (coerce required 'vector)
   "additionalProperties" false))

(-> tool-string-property (string) json-object)
(defun tool-string-property (description)
  "Return a documented JSON string property schema."
  (json-object "type" "string" "description" description))

(-> tool-string-array-property (string) json-object)
(defun tool-string-array-property (description)
  "Return a documented array-of-strings property schema."
  (json-object
   "type" "array"
   "description" description
   "items" (json-object "type" "string")))

(-> tool-integer-property (string) json-object)
(defun tool-integer-property (description)
  "Return a documented JSON integer property schema."
  (json-object "type" "integer" "description" description))

(-> tool-boolean-property (string) json-object)
(defun tool-boolean-property (description)
  "Return a documented JSON boolean property schema."
  (json-object "type" "boolean" "description" description))

(-> tool-restart-property () json-object)
(defun tool-restart-property ()
  "Return the shared schema of the optional restart selection argument."
  (tool-string-property
   "A restart name to invoke when the operation signals a correctable condition, for example CONTINUE."))

(-> tool-restart-value-property () json-object)
(defun tool-restart-value-property ()
  "Return the shared schema of the optional restart value argument."
  (tool-string-property
   "A value form passed to a restart that consumes a value, such as use-value or store-value."))

(-> tool-namespace-description (string) string)
(defun tool-namespace-description (namespace)
  "Return the model-visible description of tool NAMESPACE."
  (cond
    ((string= namespace "resource")
     "Revision-gated observation and structured editing of model-addressable resources.")
    ((string= namespace "fs")
     "Workspace file reading, listing, complete writes, and compatibility editing.")
    ((string= namespace "search")
     "Fast indexed workspace path and content discovery through fff.")
    ((string= namespace "shell")
     "External commands run in the workspace.")
    ((string= namespace "memory")
     "Persistent facts, preferences, decisions, and guidance across conversations.")
    ((string= namespace "agenda")
     "Short persistent tasks and notes keyed by workspace directory.")
    ((string= namespace "lisp")
     "Named Common Lisp workers, scratchpads, and Lisp-family source checks.")
    ((string= namespace "self")
     "Operations on the active Autolith Common Lisp image.")
    ((string= namespace "task")
     "In-process child-agent delegation with batching and detached jobs.")
    ((string= namespace "job")
     "Inspection, waiting, and cancellation for task jobs.")
    ((string= namespace "yield")
     "Required terminal result submission for child agents.")
    ((string= namespace "skill")
     "Request-local loading of discovered Autolith Skills.")
    ((string= namespace "mcp")
     "MCP server status, discovery refresh, resources, and prompts.")
    ((uiop:string-prefix-p "mcp__" namespace)
     "Tools supplied by one configured external MCP server.")
    (t
     "Autolith operations.")))

(-> tool-provider-schema (tool) json-object)
(defun tool-provider-schema (tool)
  "Return TOOL in the Responses API namespaced function schema."
  (json-object
   "type" "function"
   "name" (tool-name tool)
   "description" (tool-description tool)
   "strict" false
   "parameters" (tool-parameters tool)))


;;;; -- Tool Context and Results --

(defclass tool-context ()
  ((configuration
    :initarg :configuration
    :reader tool-context-configuration
    :type configuration
    :documentation "The active process configuration.")
   (worker
    :initarg :worker
    :reader tool-context-worker
    :type t
    :documentation "The named Lisp worker manager.")
   (conversation
    :initarg :conversation
    :reader tool-context-conversation
    :type conversation
    :documentation "The conversation requesting the operation.")
   (mutation-checker
    :initarg :mutation-checker
    :initform nil
    :reader tool-context-mutation-checker
    :type t
    :documentation "The optional durable-mutation check strategy.")
   (registry
    :initarg :registry
    :initform nil
    :reader tool-context-registry
    :type t
    :documentation "The registry dispatching this execution, when available.")
   (command-authorization-function
    :initarg :command-authorization-function
    :initform nil
    :reader tool-context-command-authorization-function
    :type (option function)
    :documentation "The callback deciding whether and how shell commands may run.")
   (tool-authorization-function
    :initarg :tool-authorization-function
    :initform nil
    :reader tool-context-tool-authorization-function
    :type (option function)
    :documentation
    "The callback deciding whether one externally implemented tool may run.")
   (agent
    :initarg :agent
    :initform nil
    :reader tool-context-agent
    :type t
    :documentation "The agent whose provider call requested this tool.")
   (observer
    :initarg :observer
    :initform nil
    :reader tool-context-observer
    :type t
    :documentation "The observer presenting the parent tool lifecycle.")
   (call-id
    :initarg :call-id
    :initform nil
    :reader tool-context-call-id
    :type (option string)
    :documentation "The provider function call identifier for this execution."))
  (:documentation "The explicit capabilities supplied to one tool execution."))

(defmethod resource-context-child-agent-p ((context tool-context))
  "Return true when CONTEXT's agent is a restricted task child agent."
  (let ((class (find-class 'task-child-agent nil)))
    (and class
         (typep (tool-context-agent context) class)
         t)))

(-> tool-context-authorize-command
    (tool-context string pathname)
    keyword)
(defun tool-context-authorize-command (context command directory)
  "Return :DENY, :SANDBOXED, or :FULL-ACCESS for COMMAND in DIRECTORY."
  (let* ((function (tool-context-command-authorization-function context))
         (decision (if function
                       (funcall function command directory)
                       ':deny)))
    (unless (member decision '(:deny :sandboxed :full-access))
      (error 'tool-error
             :message (format nil "Command authorization returned invalid decision ~S."
                              decision)
             :tool-name "shell.run"))
    decision))

(-> tool-context-authorize-tool
    (tool-context tool json-object)
    keyword)
(defun tool-context-authorize-tool (context tool arguments)
  "Return :ALLOW or :DENY for externally implemented TOOL and ARGUMENTS."
  (let* ((function (tool-context-tool-authorization-function context))
         (decision (if function
                       (funcall function tool arguments)
                       ':deny)))
    (unless (member decision '(:allow :deny))
      (error 'tool-error
             :message
             (format nil "Tool authorization returned invalid decision ~S."
                     decision)
             :tool-name (tool-canonical-name tool)))
    decision))

(defclass tool-result ()
  ((content
    :initarg :content
    :reader tool-result-content
    :type string
    :documentation "The bounded model-visible result.")
   (image-attachments
    :initarg :image-attachments
    :initform nil
    :reader tool-result-image-attachments
    :type list
    :documentation
    "Provider-visible local images returned by a successful tool operation.")
   (content-blocks
    :initarg :content-blocks
    :initform nil
    :reader tool-result-content-blocks
    :type list
    :documentation
    "Ordered provider-visible strings and image attachments, when multimodal.")
   (success-p
    :initarg :success-p
    :reader tool-result-success-p
    :type boolean
    :documentation "True when the tool operation succeeded."))
  (:documentation "The model-visible outcome of exactly one tool call."))

(defgeneric tool-result-details (result)
  (:documentation "Return RESULT's machine-readable details, or NIL when plain.")
  (:method ((result tool-result))
    nil))

(-> tool-result--normalize-content-blocks (list) list)
(defun tool-result--normalize-content-blocks (blocks)
  "Validate BLOCKS and bound their aggregate model-visible text."
  (let ((remaining 8000)
        (normalized nil))
    (dolist (block blocks)
      (etypecase block
        (string
         (when (plusp remaining)
           (let ((bounded
                   (subseq block 0 (min remaining (length block)))))
             (push bounded normalized)
             (decf remaining (length bounded)))))
        (image-attachment
         (push block normalized))))
    (nreverse normalized)))

(-> tool-success
    (t &key (:image-attachments list) (:content-blocks list))
    tool-result)
(defun tool-success (content &key image-attachments content-blocks)
  "Return a successful bounded tool result with optional multimodal output."
  (when (and image-attachments content-blocks)
    (error 'tool-error
           :message
           "Tool results cannot mix legacy image and ordered content arguments."
           :tool-name "unknown"))
  (unless (every (lambda (attachment)
                   (typep attachment 'image-attachment))
                 image-attachments)
    (error 'tool-error
           :message "Tool image results must contain image attachments."
           :tool-name "unknown"))
  (let* ((raw-blocks
           (or content-blocks
               (when image-attachments
                 (append
                  (when (non-empty-string-p (format nil "~A" content))
                    (list (format nil "~A" content)))
                  image-attachments))))
         (blocks (tool-result--normalize-content-blocks raw-blocks))
         (attachments
           (remove-if-not
            (lambda (block)
              (typep block 'image-attachment))
            blocks)))
    (make-instance 'tool-result
                   :content (bounded-string content)
                   :image-attachments attachments
                   :content-blocks blocks
                   :success-p t)))

(-> tool-failure (t) tool-result)
(defun tool-failure (content)
  "Return a failed bounded tool result containing CONTENT."
  (make-instance 'tool-result
                 :content (bounded-string content)
                 :success-p nil))

(-> tool-execute (tool tool-context json-object) tool-result)
(defgeneric tool-execute (tool context arguments)
  (:documentation "Execute TOOL with validated JSON ARGUMENTS inside CONTEXT."))

(-> tool-decode-arguments (tool string) json-object)
(defgeneric tool-decode-arguments (tool source)
  (:documentation
   "Decode one provider argument SOURCE according to TOOL's JSON requirements."))

(defmethod tool-decode-arguments ((tool tool) source)
  "Decode ordinary TOOL arguments with the application's default JSON policy."
  (declare (ignore tool))
  (json-decode source))

(-> tool-child-safe-p (tool) boolean)
(defgeneric tool-child-safe-p (tool)
  (:documentation
   "Return true when TOOL may cross the ordinary child-agent boundary."))

(defmethod tool-child-safe-p ((tool tool))
  "Keep tools unavailable to child agents unless their class opts in."
  nil)

(defmethod tool-child-safe-p ((tool lisp-tool))
  "Permit disposable Lisp-worker tools inside child agents."
  t)

(-> tool-conversation-persistence (tool) tool-conversation-persistence)
(defgeneric tool-conversation-persistence (tool)
  (:documentation
   "Return how TOOL's call and result participate in conversation persistence."))

(defmethod tool-conversation-persistence ((tool tool))
  "Persist ordinary tool calls and their results in durable conversation history."
  ':durable)

(-> tool-provider-round-trip-barrier-p (tool) boolean)
(defgeneric tool-provider-round-trip-barrier-p (tool)
  (:documentation
   "Return true when TOOL must finish before the provider may issue later calls."))

(defmethod tool-provider-round-trip-barrier-p ((tool tool))
  "Allow ordinary tool calls to execute in provider wire order."
  nil)

(-> tool-compact-result-visible-p (tool) boolean)
(defgeneric tool-compact-result-visible-p (tool)
  (:documentation
   "Return true when TOOL's successful result remains visible in compact mode."))

(defmethod tool-compact-result-visible-p ((tool tool))
  "Hide ordinary successful results in compact presentation."
  nil)

(defmethod tool-compact-result-visible-p ((tool resource-edit-tool))
  "Keep successful resource mutations visible in compact presentation."
  (declare (ignore tool))
  t)

(-> tool-runtime-identity (tool) t)
(defgeneric tool-runtime-identity (tool)
  (:documentation
   "Return TOOL's shared ephemeral runtime identity, or NIL when it owns none."))

(defmethod tool-runtime-identity ((tool tool))
  "Return NIL because ordinary tools own no ephemeral runtime."
  nil)

(-> tool-runtime-close-priority (tool) integer)
(defgeneric tool-runtime-close-priority (tool)
  (:documentation
   "Return TOOL runtime shutdown priority; larger values close first."))

(defmethod tool-runtime-close-priority ((tool tool))
  "Give ordinary independent runtimes the baseline shutdown priority."
  0)

(-> tool-runtime-close (tool) null)
(defgeneric tool-runtime-close (tool)
  (:documentation "Stop TOOL's ephemeral runtime and release its resources."))

(defmethod tool-runtime-close ((tool tool))
  "Leave an ordinary stateless TOOL unchanged."
  nil)

(-> tool-runtime-detach (tool) null)
(defgeneric tool-runtime-detach (tool)
  (:documentation
   "Detach TOOL's inherited external resources before saving a forked image."))

(defmethod tool-runtime-detach ((tool tool))
  "Leave an ordinary stateless TOOL unchanged."
  nil)

(-> tool-argument (json-object string &key (:required boolean)) t)
(defun tool-argument (arguments name &key required)
  "Return NAME from ARGUMENTS, signaling TOOL-ERROR when REQUIRED and absent."
  (multiple-value-bind (value present-p)
      (gethash name arguments)
    (when (and required (not present-p))
      (error 'tool-error
             :message (format nil "Required tool argument ~S is missing." name)
             :tool-name "unknown"))
    value))

(-> tool-boolean-argument
    (json-object string &key (:default boolean) (:tool-name (option string)))
    boolean)
(defun tool-boolean-argument (arguments name &key default tool-name)
  "Return optional JSON Boolean NAME from ARGUMENTS, defaulting to DEFAULT."
  (multiple-value-bind (value present-p)
      (gethash name arguments)
    (cond
      ((not present-p)
       (and default t))
      ((eq value t)
       t)
      ((eq value false)
       nil)
      (t
       (error 'tool-error
              :message (format nil "Tool argument ~S must be a boolean." name)
              :tool-name (or tool-name name))))))


;;;; -- Resource Tool Dispatch --

(-> resource-tool-read
    (resource resource-read-tool tool-context json-object)
    tool-result)
(defgeneric resource-tool-read (resource tool context arguments)
  (:documentation
   "Read RESOURCE for the model through TOOL under CONTEXT and ARGUMENTS."))

(defmethod resource-tool-read
    ((resource resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Reject model-facing reads not implemented by RESOURCE's concrete class."
  (declare (ignore tool context arguments))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':read))

(-> resource-tool-edit
    (resource resource-edit-tool tool-context json-object)
    tool-result)
(defgeneric resource-tool-edit (resource tool context arguments)
  (:documentation
   "Edit RESOURCE for the model through TOOL under CONTEXT and ARGUMENTS."))

(defmethod resource-tool-edit
    ((resource resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Reject model-facing edits not implemented by RESOURCE's concrete class."
  (declare (ignore tool context arguments))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':edit))

(-> resource-tool--resolve (resource-tool tool-context json-object) resource)
(defun resource-tool--resolve (tool context arguments)
  "Resolve TOOL's required resource URI under exact authority CONTEXT."
  (resource-registry-resolve
   (resource-tool-resource-registry tool)
   (tool-argument arguments "uri" :required t)
   context))

(defmethod tool-execute
    ((tool resource-read-tool) (context tool-context) (arguments hash-table))
  "Resolve and dispatch one model-facing resource read."
  (resource-tool-read (resource-tool--resolve tool context arguments)
                      tool context arguments))

(defmethod tool-execute
    ((tool resource-edit-tool) (context tool-context) (arguments hash-table))
  "Resolve and dispatch one model-facing resource edit."
  (resource-tool-edit (resource-tool--resolve tool context arguments)
                      tool context arguments))

(defmethod tool-child-safe-p ((tool resource-read-tool))
  "Permit authority-confined resource reads inside child agents."
  t)

(defmethod tool-child-safe-p ((tool resource-edit-tool))
  "Permit authority-confined resource edits inside child agents."
  t)


;;;; -- Registry and Dispatch --

(defclass tool-registry ()
  ((tools
    :initform (make-ordered-map :test #'equal)
    :reader tool-registry-tool-map
    :type ordered-map
    :documentation
    "Canonical dotted tool names mapped to tools in presentation order.")
   (runtime-bindings
    :initform (make-hash-table :test #'eq)
    :reader tool-registry-runtime-bindings
    :type hash-table
    :documentation
    "Subsystem-specific bindings used to reconcile shared dynamic runtimes.")
   (resource-registry
    :initform (make-resource-registry)
    :reader tool-registry-resource-registry
    :type resource-registry
    :documentation "The per-agent registry of authority-neutral resource resolvers."))
  (:documentation "The model-visible tools and their active dispatch objects."))

(-> tool-registry-tools (tool-registry) list)
(defun tool-registry-tools (registry)
  "Return a fresh list of REGISTRY's tools in presentation order."
  (coerce (ordered-map-values (tool-registry-tool-map registry)) 'list))

(-> tool-runtime-resume (tool tool-registry) null)
(defgeneric tool-runtime-resume (tool registry)
  (:documentation
   "Restart TOOL's shared ephemeral runtime and reconcile REGISTRY."))

(defmethod tool-runtime-resume ((tool tool) (registry tool-registry))
  "Leave an ordinary stateless TOOL unchanged."
  (declare (ignore tool registry))
  nil)

(-> tool-registry-runtime-binding (tool-registry symbol) t)
(defun tool-registry-runtime-binding (registry key)
  "Return REGISTRY's runtime binding under KEY, or NIL."
  (gethash key (tool-registry-runtime-bindings registry)))

(-> tool-registry-bind-runtime (tool-registry symbol t) t)
(defun tool-registry-bind-runtime (registry key binding)
  "Associate BINDING with KEY in REGISTRY and return BINDING."
  (setf (gethash key (tool-registry-runtime-bindings registry)) binding))

(-> tool-registry-unbind-runtime (tool-registry symbol) boolean)
(defun tool-registry-unbind-runtime (registry key)
  "Remove KEY's runtime binding from REGISTRY and report whether it existed."
  (and (remhash key (tool-registry-runtime-bindings registry)) t))

(-> tool-registry--runtime-representatives
    (tool-registry &key (:reverse-p boolean))
    list)
(defun tool-registry--runtime-representatives (registry &key reverse-p)
  "Return one representative tool for every distinct runtime in REGISTRY."
  (let ((tools
          (remove-if-not #'tool-runtime-identity
                         (tool-registry-tools registry))))
    (remove-duplicates (if reverse-p (nreverse tools) tools)
                       :key #'tool-runtime-identity
                       :test #'eq
                       :from-end t)))

(-> tool-registry--ordered-runtime-representatives
    (tool-registry &key (:reverse-p boolean) (:priority-p boolean))
    list)
(defun tool-registry--ordered-runtime-representatives
    (registry &key reverse-p priority-p)
  "Return REGISTRY runtime representatives in the requested operation order."
  (let ((representatives
          (tool-registry--runtime-representatives
           registry :reverse-p reverse-p)))
    (if priority-p
        (stable-sort representatives #'>
                     :key #'tool-runtime-close-priority)
        representatives)))

(-> tool-registry--attempt-runtime-operation (list function)
    (values list (option serious-condition)))
(defun tool-registry--attempt-runtime-operation (tools function)
  "Apply FUNCTION to TOOLS, returning successful tools and the first failure."
  (let ((completed nil)
        (first-failure nil))
    (dolist (tool tools)
      (handler-case
          (progn
            (funcall function tool)
            (push tool completed))
        (serious-condition (condition)
          (unless first-failure
            (setf first-failure condition)))))
    (values (nreverse completed) first-failure)))

(-> tool-registry--apply-runtime-operation
    (tool-registry function &key (:reverse-p boolean) (:priority-p boolean))
    null)
(defun tool-registry--apply-runtime-operation
    (registry function &key reverse-p priority-p)
  "Call FUNCTION once per runtime in registration or shutdown order."
  (multiple-value-bind (completed first-failure)
      (tool-registry--attempt-runtime-operation
       (tool-registry--ordered-runtime-representatives
        registry
        :reverse-p reverse-p
        :priority-p priority-p)
       function)
    (declare (ignore completed))
    (when first-failure
      (error first-failure)))
  nil)

(-> tool-registry-quiesce-runtime-state
    (tool-registry)
    (values list (option serious-condition)))
(defun tool-registry-quiesce-runtime-state (registry)
  "Close every runtime and return the successfully quiesced tools and failure."
  (tool-registry--attempt-runtime-operation
   (tool-registry--ordered-runtime-representatives
    registry :reverse-p t :priority-p t)
   #'tool-runtime-close))

(-> tool-registry-close-runtime-state (tool-registry) null)
(defun tool-registry-close-runtime-state (registry)
  "Stop runtimes by explicit dependency priority, then reverse registration."
  (tool-registry--apply-runtime-operation
   registry #'tool-runtime-close :reverse-p t :priority-p t))

(-> tool-registry-resume-runtime-state
    (tool-registry &key (:tools list))
    null)
(defun tool-registry-resume-runtime-state
    (registry &key (tools nil tools-supplied-p))
  "Restart selected quiesced runtimes in dependency order and reconcile REGISTRY."
  (let ((ordered
          (stable-sort
           (copy-list
            (if tools-supplied-p
                tools
                (tool-registry--runtime-representatives registry)))
           #'<
           :key #'tool-runtime-close-priority)))
    (multiple-value-bind (completed first-failure)
        (tool-registry--attempt-runtime-operation
         ordered
         (lambda (tool)
           (tool-runtime-resume tool registry)))
      (declare (ignore completed))
      (when first-failure
        (error first-failure))))
  nil)

(-> tool-runtime-prune-checkpoint-state (tool tool-registry) null)
(defgeneric tool-runtime-prune-checkpoint-state (tool registry)
  (:documentation
   "Remove TOOL state that must not survive in a checkpointed REGISTRY."))

(defmethod tool-runtime-prune-checkpoint-state
    ((tool tool) (registry tool-registry))
  "Leave an ordinary stateless TOOL in the checkpoint registry."
  (declare (ignore tool registry))
  nil)

(-> tool-registry-detach-runtime-state (tool-registry) null)
(defun tool-registry-detach-runtime-state (registry)
  "Detach every distinct inherited tool runtime owned by REGISTRY."
  (tool-registry--apply-runtime-operation
   registry #'tool-runtime-detach)
  (tool-registry--apply-runtime-operation
   registry
   (lambda (tool)
     (tool-runtime-prune-checkpoint-state tool registry))))

(-> tool-registry-register (tool-registry tool) tool)
(defun tool-registry-register (registry tool)
  "Register TOOL, preserving an existing name's presentation position."
  (ordered-map-set (tool-registry-tool-map registry)
                   (tool-canonical-name tool)
                   tool))

(-> tool-registry-delete-if (tool-registry function) tool-registry)
(defun tool-registry-delete-if (registry predicate)
  "Delete every tool satisfying PREDICATE from REGISTRY and return REGISTRY."
  (ordered-map-delete-if
   (lambda (canonical-name tool)
     (declare (ignore canonical-name))
     (funcall predicate tool))
   (tool-registry-tool-map registry))
  registry)

(-> tool-registry-find (tool-registry string string) (option tool))
(defun tool-registry-find (registry namespace name)
  "Return the tool named NAMESPACE.NAME from REGISTRY, or NIL."
  (ordered-map-get (tool-registry-tool-map registry)
                   (format nil "~A.~A" namespace name)))

(-> tool-context-execution-runtime (tool-context) t)
(defun tool-context-execution-runtime (context)
  "Return CONTEXT's shared inspectable execution runtime, or NIL when absent."
  (let* ((registry (tool-context-registry context))
         (job-tool
           (and (typep registry 'tool-registry)
                (tool-registry-find registry "job" "list"))))
    (and job-tool (tool-runtime-identity job-tool))))

(-> tool-execution-invoke
    (t t
     &key (:tool-name non-empty-string)
       (:summary string)
       (:operation-function function)
       (:async-p boolean)
       (:parent-call-id (option string)))
    tool-result)
(defgeneric tool-execution-invoke
    (runtime parent
     &key tool-name summary operation-function async-p parent-call-id)
  (:documentation
   "Run one shell or Lisp operation synchronously or through inspectable RUNTIME."))

(defmethod tool-execution-invoke
    ((runtime null) parent
     &key tool-name summary operation-function async-p parent-call-id)
  "Run directly when no session execution runtime is available."
  (declare (ignore runtime parent summary parent-call-id))
  (when async-p
    (error 'tool-error
           :message
           "Asynchronous execution requires the active session job runtime."
           :tool-name tool-name))
  (funcall operation-function))

(-> tool-registry-provider-schemas
    (tool-registry &key (:canonical-names (option list)))
    vector)
(defun tool-registry-provider-schemas
    (registry &key (canonical-names nil canonical-names-supplied-p))
  "Return REGISTRY grouped into provider schemas, optionally filtered by name."
  (let ((namespace-tools (make-ordered-map :test #'equal))
        (schemas (make-deque)))
    (dolist (tool (tool-registry-tools registry))
      (when (or (not canonical-names-supplied-p)
                (member (tool-canonical-name tool)
                        canonical-names
                        :test #'string=))
        (let ((tools
                (or (ordered-map-get namespace-tools (tool-namespace tool))
                    (ordered-map-set namespace-tools
                                     (tool-namespace tool)
                                     (make-deque)))))
          (deque-push-back tools (tool-provider-schema tool)))))
    (ordered-map-map
     (lambda (namespace tools)
       (deque-push-back
        schemas
        (json-object
         "type" "namespace"
         "name" namespace
         "description" (tool-namespace-description namespace)
         "tools" (deque->vector tools))))
     namespace-tools)
    (deque->vector schemas)))

(-> function-call-canonical-name (json-object) string)
(defun function-call-canonical-name (call)
  "Return the dotted canonical name carried by function CALL.

A call without a namespace reads as its bare name instead of gaining
a spurious leading dot."
  (let ((namespace (json-get call "namespace"))
        (name (or (json-get call "name") "")))
    (if (non-empty-string-p namespace)
        (format nil "~A.~A" namespace name)
        name)))

(-> tool-registry-execute-call
    (tool-registry json-object tool-context)
    tool-result)
(-> tool-registry--only-named (tool-registry string) (option tool))
(defun tool-registry--only-named (registry name)
  "Return the single tool called NAME across namespaces, when unique.

Some Chat Completions models echo only the bare half of an advertised
tool name; a unique bare name still dispatches, while an ambiguous one
signals with the candidate canonical names."
  (let ((matches (remove-if-not (lambda (tool)
                                  (string= (tool-name tool) name))
                                (tool-registry-tools registry))))
    (cond
      ((null matches) nil)
      ((rest matches)
       (error 'tool-error
              :message
              (format nil "Ambiguous tool name ~A: use one of ~{~A~^, ~}."
                      name (mapcar #'tool-canonical-name matches))
              :tool-name name))
      (t (first matches)))))

(defun tool-registry-execute-call (registry call context)
  "Validate and execute one Responses function CALL through REGISTRY."
  (let* ((namespace (json-get call "namespace"))
         (name (json-get call "name"))
         (canonical-name (function-call-canonical-name call)))
    (handler-case
        (progn
          (unless (non-empty-string-p name)
            (error 'tool-error
                   :message "The provider returned a function call without a tool name."
                   :tool-name canonical-name))
          (let ((tool (if (non-empty-string-p namespace)
                          (tool-registry-find registry namespace name)
                          (tool-registry--only-named registry name))))
            (unless tool
              (error 'tool-error
                     :message (format nil "Unknown tool ~A." canonical-name)
                     :tool-name canonical-name))
            (let ((arguments
                    (tool-decode-arguments
                     tool
                     (or (json-get call "arguments") "{}"))))
              (unless (json-object-p arguments)
                (error 'tool-error
                       :message (format nil "Arguments for ~A are not a JSON object."
                                        canonical-name)
                       :tool-name canonical-name))
              (tool-execute tool context arguments))))
      (rollback-requested (condition)
        (error condition))
      (active-image-corruption (condition)
        (error condition))
      (error (condition)
        (tool-failure
         (format nil "~A failed: ~A" canonical-name condition))))))
