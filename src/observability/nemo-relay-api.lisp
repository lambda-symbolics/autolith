(in-package #:autolith)

;;;; -- Relay Native Handles --

(defvar *nemo-relay-observability-library* nil
  "The foreign library handle loaded for direct Relay observability calls.")

(defclass nemo-relay-handle ()
  ((pointer
    :initarg :pointer
    :reader nemo-relay-handle-pointer
    :documentation "The opaque foreign pointer owned by this handle.")
   (free-function
    :initarg :free-function
    :reader nemo-relay-handle-free-function
    :initform nil
    :documentation "The native function that releases this handle.")
   (freed-p
    :initform nil
    :accessor nemo-relay-handle-freed-p
    :type boolean
    :documentation "Whether the native resource has been released."))
  (:documentation "An opaque owned NeMo Relay native handle."))

(-> nemo-relay--signal-error (string string &optional (option integer)) *)
(defun nemo-relay--signal-error (message operation &optional status)
  "Signal a Relay error with MESSAGE, OPERATION, and optional STATUS."
  (error 'nemo-relay-error
         :message message
         :operation operation
         :status status))

(-> nemo-relay--ensure-native-library () t)
(defun nemo-relay--ensure-native-library ()
  "Load the configured Relay library for a direct observability operation."
  (unless *nemo-relay-observability-library-loaded-p*
    (handler-case
        (setf *nemo-relay-observability-library*
              (nemo-relay--load-library (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))
              *nemo-relay-observability-library-loaded-p* t)
      (serious-condition (condition)
        (nemo-relay--signal-error
         (format nil "Unable to load the NeMo Relay foreign library: ~A"
                 (nemo-relay--condition-summary condition))
         "Relay foreign library"))))
  t)

(-> nemo-relay--require-status (string function) boolean)
(defun nemo-relay--require-status (operation function)
  "Call status-returning FUNCTION and signal RELAY-ERROR on failure."
  (nemo-relay--ensure-native-library)
  (if (nemo-relay--ffi-status operation function)
      t
      (nemo-relay--signal-error
       (or (nemo-relay-last-error)
           (format nil "~A failed without a Relay diagnostic." operation))
       operation)))

(-> nemo-relay--json-value (t string) (option string))
(defun nemo-relay--json-value (value operation)
  "Serialize VALUE as JSON text, preserving NIL as an omitted argument."
  (when value
    (or (nemo-relay--json-argument value)
        (nemo-relay--signal-error
         (format nil "~A cannot be represented as JSON." operation)
         operation))))

(-> nemo-relay--json-object-value (t string) (option string))
(defun nemo-relay--json-object-value (value operation)
  "Serialize VALUE and require that it represent a JSON object."
  (let ((json (nemo-relay--json-value value operation)))
    (when json
      (handler-case
          (unless (json-object-p (json-decode json))
            (nemo-relay--signal-error
             (format nil "~A must be a JSON object." operation)
             operation))
        (nemo-relay-error (condition)
          (error condition))
        (serious-condition (condition)
          (nemo-relay--signal-error
           (format nil "~A is not valid JSON: ~A"
                   operation (nemo-relay--condition-summary condition))
           operation))))
    json))

(-> nemo-relay--required-native-string (t string) string)
(defun nemo-relay--required-native-string (value operation)
  "Validate one required non-empty native string VALUE."
  (unless (and (stringp value) (non-empty-string-p value))
    (nemo-relay--signal-error
     (format nil "~A requires a non-empty string." operation)
     operation))
  value)

(-> nemo-relay--uint32-value (t string) integer)
(defun nemo-relay--uint32-value (value operation)
  "Validate one unsigned 32-bit native VALUE."
  (unless (and (integerp value) (<= 0 value #xffffffff))
    (nemo-relay--signal-error
     (format nil "~A must be a uint32." operation)
     operation))
  value)

(-> nemo-relay--handle-pointer (t string) t)
(defun nemo-relay--handle-pointer (value operation)
  "Return VALUE's live foreign pointer or signal a handle error."
  (if (typep value 'nemo-relay-handle)
      (if (nemo-relay-handle-freed-p value)
          (nemo-relay--signal-error
           (format nil "The Relay handle for ~A has already been freed."
                   operation)
           operation)
          (nemo-relay-handle-pointer value))
      (nemo-relay--signal-error
       (format nil "~A requires a Relay handle." operation)
       operation)))

(-> nemo-relay--optional-handle-pointer (t string) t)
(defun nemo-relay--optional-handle-pointer (value operation)
  "Return an optional foreign handle pointer, using NULL for NIL."
  (if value
      (nemo-relay--handle-pointer value operation)
      (cffi:null-pointer)))

(-> nemo-relay--new-handle (string (option function) function) nemo-relay-handle)
(defun nemo-relay--new-handle (operation free-function function)
  "Call FUNCTION with an output slot and wrap its pointer in a handle."
  (nemo-relay--ensure-native-library)
  (let ((slot (nemo-relay--make-output-slot)))
    (unwind-protect
         (progn
           (nemo-relay--require-status operation
                                        (lambda () (funcall function slot)))
           (let ((pointer (nemo-relay--output-slot-value slot)))
             (if (nemo-relay--pointer-present-p pointer)
                 (make-instance 'nemo-relay-handle
                                :pointer pointer
                                :free-function free-function)
                 (nemo-relay--signal-error
                  (format nil "~A returned no handle." operation)
                  operation))))
      (cffi:foreign-free slot))))

(-> nemo-relay-handle-free ((option nemo-relay-handle)) boolean)
(defun nemo-relay-handle-free (handle)
  "Release HANDLE idempotently."
  (if (null handle)
      t
      (progn
        (unless (typep handle 'nemo-relay-handle)
          (nemo-relay--signal-error
           "Expected a Relay handle."
           "nemo_relay_handle_free"))
        (unless (nemo-relay-handle-freed-p handle)
          (let ((pointer (nemo-relay-handle-pointer handle))
                (free-function (nemo-relay-handle-free-function handle)))
            (when (and free-function
                       (nemo-relay--pointer-present-p pointer))
              (funcall free-function pointer))
            (setf (nemo-relay-handle-freed-p handle) t)))
        t)))

(-> nemo-relay--string-output (string function) (option string))
(defun nemo-relay--string-output (operation function)
  "Return a copied optional native string after a status-returning call."
  (nemo-relay--ensure-native-library)
  (multiple-value-bind (result status)
      (nemo-relay--take-string-output operation function)
    (unless status
      (nemo-relay--signal-error
       (or (nemo-relay-last-error)
           (format nil "~A failed without a Relay diagnostic." operation))
       operation))
    result))

(-> nemo-relay--required-string-output (string function) string)
(defun nemo-relay--required-string-output (operation function)
  "Return a required native string or signal a Relay error."
  (or (nemo-relay--string-output operation function)
      (nemo-relay--signal-error
       (format nil "~A returned no string." operation)
       operation)))

;;;; -- Lifecycle Values --

(defparameter *nemo-relay-scope-type-codes*
  '(("agent" . 0)
    ("tool" . 2)
    ("llm" . 3))
  "The Relay scope types used by Autolith's lifecycle adapter.")

(-> nemo-relay--normalized-name (t) (option string))
(defun nemo-relay--normalized-name (value)
  "Return VALUE as a lowercase name when it is a symbol or string."
  (cond
    ((stringp value)
     (string-downcase value))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    (t
     nil)))

(-> nemo-relay--scope-type-code (t) integer)
(defun nemo-relay--scope-type-code (scope-type)
  "Return the Relay integer code for SCOPE-TYPE."
  (or (and (integerp scope-type)
           (when (rassoc scope-type *nemo-relay-scope-type-codes* :test #'=)
             scope-type))
      (cdr (assoc (nemo-relay--normalized-name scope-type)
                  *nemo-relay-scope-type-codes*
                  :test #'string=))
      (nemo-relay--signal-error
       (format nil "Unknown Relay scope type ~S." scope-type)
       "Relay scope type")))

;;;; -- Scopes, Events, and Manual Lifecycles --

(-> nemo-relay-push-scope
    (&key (:name string) (:scope-type t) (:parent (option t))
          (:attributes integer) (:data t) (:metadata t) (:input t)
          (:timestamp (option integer)))
    nemo-relay-handle)
(defun nemo-relay-push-scope
    (&key name scope-type parent (attributes 0) data metadata input timestamp)
  "Push an Agent, Tool, or LLM scope and return its opaque handle."
  (let* ((operation "nemo_relay_push_scope")
         (scope-code (nemo-relay--scope-type-code scope-type))
         (parent-pointer (nemo-relay--optional-handle-pointer parent operation))
         (data-json (nemo-relay--json-value data "Relay scope data"))
         (metadata-json (nemo-relay--json-value metadata "Relay scope metadata"))
         (input-json (nemo-relay--json-value input "Relay scope input")))
    (nemo-relay--required-native-string name operation)
    (nemo-relay--uint32-value attributes "Relay scope attributes")
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay scope timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--new-handle
     operation
     #'%nemo-relay-scope-handle-free
     (lambda (slot)
       (nemo-relay--call-with-c-strings
        (list name data-json metadata-json input-json)
        (lambda (name-pointer data-pointer metadata-pointer input-pointer)
          (nemo-relay--call-with-timestamp
           timestamp
           (lambda (timestamp-pointer)
             (%nemo-relay-push-scope
              name-pointer scope-code parent-pointer attributes
              data-pointer metadata-pointer input-pointer timestamp-pointer
              slot)))))))))

(-> nemo-relay-pop-scope
    (&key (:handle nemo-relay-handle) (:output t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-pop-scope (&key handle output metadata timestamp)
  "Pop HANDLE and emit its Relay scope-end event."
  (let* ((operation "nemo_relay_pop_scope")
         (pointer (nemo-relay--handle-pointer handle operation))
         (output-json (nemo-relay--json-value output "Relay scope output"))
         (metadata-json (nemo-relay--json-value metadata "Relay scope metadata")))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay scope timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--call-with-c-strings
     (list output-json metadata-json)
     (lambda (output-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           operation
           (lambda ()
             (%nemo-relay-pop-scope
              pointer output-pointer metadata-pointer timestamp-pointer)))))))
    t))

(-> nemo-relay-tool-call
    (&key (:name string) (:arguments t) (:parent (option t))
          (:attributes integer) (:data t) (:metadata t)
          (:call-id (option string)) (:timestamp (option integer)))
    nemo-relay-handle)
(defun nemo-relay-tool-call
    (&key name arguments parent (attributes 0) data metadata call-id timestamp)
  "Begin a Relay tool lifecycle and return its opaque handle."
  (let* ((operation "nemo_relay_tool_call")
         (arguments-json (nemo-relay--json-value arguments "Relay tool arguments"))
         (parent-pointer (nemo-relay--optional-handle-pointer parent operation))
         (data-json (nemo-relay--json-value data "Relay tool data"))
         (metadata-json (nemo-relay--json-value metadata "Relay tool metadata")))
    (nemo-relay--required-native-string name operation)
    (unless arguments-json
      (nemo-relay--signal-error "Relay tool arguments are required." operation))
    (nemo-relay--uint32-value attributes "Relay tool attributes")
    (when (and call-id (not (stringp call-id)))
      (nemo-relay--signal-error "Relay tool call ID must be a string." operation))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay tool timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--new-handle
     operation
     #'%nemo-relay-tool-handle-free
     (lambda (slot)
       (nemo-relay--call-with-c-strings
        (list name arguments-json data-json metadata-json call-id)
        (lambda
            (name-pointer arguments-pointer data-pointer metadata-pointer call-id-pointer)
          (nemo-relay--call-with-timestamp
           timestamp
           (lambda (timestamp-pointer)
             (%nemo-relay-tool-call
              name-pointer arguments-pointer parent-pointer attributes
              data-pointer metadata-pointer call-id-pointer timestamp-pointer
              slot)))))))))

(-> nemo-relay-tool-call-end
    (&key (:handle nemo-relay-handle) (:result t) (:data t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-tool-call-end (&key handle result data metadata timestamp)
  "Finish a Relay tool lifecycle."
  (let* ((operation "nemo_relay_tool_call_end")
         (handle-pointer (nemo-relay--handle-pointer handle operation))
         (result-json (nemo-relay--json-value result "Relay tool result"))
         (data-json (nemo-relay--json-value data "Relay tool end data"))
         (metadata-json (nemo-relay--json-value metadata "Relay tool end metadata")))
    (unless result-json
      (nemo-relay--signal-error "Relay tool result is required." operation))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay tool timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--call-with-c-strings
     (list result-json data-json metadata-json)
     (lambda (result-pointer data-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           operation
           (lambda ()
             (%nemo-relay-tool-call-end
              handle-pointer result-pointer data-pointer metadata-pointer
              timestamp-pointer)))))))
    t))

(-> nemo-relay-llm-call
    (&key (:name string) (:native t) (:parent (option t))
          (:attributes integer) (:data t) (:metadata t)
          (:model-name (option string)) (:timestamp (option integer)))
    nemo-relay-handle)
(defun nemo-relay-llm-call
    (&key name native parent (attributes 0) data metadata model-name timestamp)
  "Begin a Relay LLM lifecycle and return its opaque handle."
  (let* ((operation "nemo_relay_llm_call")
         (native-json (nemo-relay--json-object-value native "Relay LLM request"))
         (parent-pointer (nemo-relay--optional-handle-pointer parent operation))
         (data-json (nemo-relay--json-value data "Relay LLM data"))
         (metadata-json (nemo-relay--json-value metadata "Relay LLM metadata")))
    (nemo-relay--required-native-string name operation)
    (unless native-json
      (nemo-relay--signal-error "Relay LLM request is required." operation))
    (nemo-relay--uint32-value attributes "Relay LLM attributes")
    (when (and model-name (not (stringp model-name)))
      (nemo-relay--signal-error "Relay LLM model name must be a string." operation))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay LLM timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--new-handle
     operation
     #'%nemo-relay-llm-handle-free
     (lambda (slot)
       (nemo-relay--call-with-c-strings
        (list name native-json data-json metadata-json model-name)
        (lambda
            (name-pointer native-pointer data-pointer metadata-pointer model-pointer)
          (nemo-relay--call-with-timestamp
           timestamp
           (lambda (timestamp-pointer)
             (%nemo-relay-llm-call
              name-pointer native-pointer parent-pointer attributes
              data-pointer metadata-pointer model-pointer timestamp-pointer
              slot)))))))))

(-> nemo-relay-llm-call-end
    (&key (:handle nemo-relay-handle) (:response t) (:data t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-llm-call-end (&key handle response data metadata timestamp)
  "Finish a Relay LLM lifecycle."
  (let* ((operation "nemo_relay_llm_call_end")
         (handle-pointer (nemo-relay--handle-pointer handle operation))
         (response-json (nemo-relay--json-value response "Relay LLM response"))
         (data-json (nemo-relay--json-value data "Relay LLM end data"))
         (metadata-json (nemo-relay--json-value metadata "Relay LLM end metadata")))
    (unless response-json
      (nemo-relay--signal-error "Relay LLM response is required." operation))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay LLM timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--call-with-c-strings
     (list response-json data-json metadata-json)
     (lambda (response-pointer data-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           operation
           (lambda ()
             (%nemo-relay-llm-call-end
              handle-pointer response-pointer data-pointer metadata-pointer
              timestamp-pointer)))))))
    t))

(-> nemo-relay-event
    (&key (:name string) (:parent (option t)) (:data t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-event (&key name parent data metadata timestamp)
  "Emit a Relay lifecycle mark on the current scope."
  (let* ((operation "nemo_relay_event")
         (parent-pointer (nemo-relay--optional-handle-pointer parent operation))
         (data-json (nemo-relay--json-value data "Relay event data"))
         (metadata-json (nemo-relay--json-value metadata "Relay event metadata")))
    (nemo-relay--required-native-string name operation)
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error
       "Relay event timestamp must be an integer Unix microsecond value."
       operation))
    (nemo-relay--call-with-c-strings
     (list name data-json metadata-json)
     (lambda (name-pointer data-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           operation
           (lambda ()
             (%nemo-relay-event
              name-pointer parent-pointer data-pointer metadata-pointer
              timestamp-pointer)))))))
    t))

;;;; -- Scope Stack Propagation --

(-> nemo-relay-capture-propagation-context () string)
(defun nemo-relay-capture-propagation-context ()
  "Capture the current Relay propagation context as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_capture_propagation_context_json"
   #'%nemo-relay-capture-propagation-context-json))

(-> nemo-relay-scope-stack-create () nemo-relay-handle)
(defun nemo-relay-scope-stack-create ()
  "Create an isolated Relay scope stack."
  (nemo-relay--new-handle
   "nemo_relay_scope_stack_create"
   #'%nemo-relay-scope-stack-free
   #'%nemo-relay-scope-stack-create))

(-> nemo-relay-scope-stack-create-from-propagation-context (t) nemo-relay-handle)
(defun nemo-relay-scope-stack-create-from-propagation-context (context)
  "Create an isolated scope stack from propagation-context JSON CONTEXT."
  (let ((context-json (nemo-relay--json-value
                       context "Relay propagation context")))
    (unless context-json
      (nemo-relay--signal-error
       "Relay propagation context is required."
       "nemo_relay_scope_stack_create_from_propagation_json"))
    (nemo-relay--call-with-c-strings
     (list context-json)
     (lambda (context-pointer)
       (nemo-relay--new-handle
        "nemo_relay_scope_stack_create_from_propagation_json"
        #'%nemo-relay-scope-stack-free
        (lambda (slot)
          (%nemo-relay-scope-stack-create-from-propagation-json
           context-pointer slot)))))))

(-> nemo-relay-scope-stack-set-thread (nemo-relay-handle) boolean)
(defun nemo-relay-scope-stack-set-thread (stack)
  "Bind STACK to the current thread without consuming it."
  (nemo-relay--require-status
   "nemo_relay_scope_stack_set_thread"
   (lambda ()
     (%nemo-relay-scope-stack-set-thread
      (nemo-relay--handle-pointer
       stack "nemo_relay_scope_stack_set_thread"))))
  t)

(-> nemo-relay-scope-stack-capture-thread () nemo-relay-handle)
(defun nemo-relay-scope-stack-capture-thread ()
  "Capture the current thread's Relay scope-stack binding."
  (nemo-relay--new-handle
   "nemo_relay_scope_stack_capture_thread"
   nil
   #'%nemo-relay-scope-stack-capture-thread))

(-> nemo-relay-scope-stack-restore-thread (nemo-relay-handle) boolean)
(defun nemo-relay-scope-stack-restore-thread (binding)
  "Restore and consume a captured thread scope-stack binding."
  (nemo-relay--require-status
   "nemo_relay_scope_stack_restore_thread"
   (lambda ()
     (%nemo-relay-scope-stack-restore-thread
      (nemo-relay--handle-pointer
       binding "nemo_relay_scope_stack_restore_thread"))))
  (setf (nemo-relay-handle-freed-p binding) t)
  t)

(-> nemo-relay-scope-stack-free ((option nemo-relay-handle)) boolean)
(defun nemo-relay-scope-stack-free (stack)
  "Release an isolated Relay scope stack."
  (nemo-relay-handle-free stack))
