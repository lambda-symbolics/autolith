(in-package #:autolith)

;;;; -- Relay Native Handle Protocol --

(defvar *nemo-relay-observability-library* nil
  "The foreign library handle loaded for direct Relay observability calls.")


(defvar *nemo-relay-missing-value* (gensym "MISSING-VALUE-")
  "Sentinel for absent typed measurement fields.")

(defclass nemo-relay-native-handle ()
  ((pointer
    :initarg :pointer
    :reader nemo-relay-native-handle-pointer
    :documentation "The opaque foreign pointer owned by this handle.")
   (freed-p
    :initform nil
    :accessor nemo-relay-native-handle-freed-p
    :documentation "Whether the foreign handle has been released.")
   (shutdown-p
    :initform nil
    :accessor nemo-relay-native-handle-shutdown-p
    :documentation "Whether the native handle has been explicitly shut down."))
  (:documentation "Base class for an explicitly owned NeMo Relay foreign handle."))

(defgeneric nemo-relay-native-handle-free (handle)
  (:documentation "Release the foreign resource owned by HANDLE."))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-native-handle))
  "Mark an unsupported base handle as released without guessing its ABI."
  (setf (nemo-relay-native-handle-freed-p handle) t)
  t)

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
         "Relay foreign library"
         nil))))
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

(-> nemo-relay--native-pointer (t string) t)
(defun nemo-relay--native-pointer (value operation)
  "Return VALUE's foreign pointer or signal when it is not an owned handle."
  (cond
    ((typep value 'nemo-relay-native-handle)
     (if (nemo-relay-native-handle-freed-p value)
         (nemo-relay--signal-error
          (format nil "The Relay handle for ~A has already been freed." operation)
          operation)
         (nemo-relay-native-handle-pointer value)))
    ((cffi:pointerp value)
     value)
    ((null value)
     (cffi:null-pointer))
    (t
     (nemo-relay--signal-error
      (format nil "~A requires a Relay handle or foreign pointer." operation)
      operation))))

(-> nemo-relay--optional-native-pointer (t string) t)
(defun nemo-relay--optional-native-pointer (value operation)
  "Return an optional foreign pointer, using NULL for NIL."
  (if value
      (nemo-relay--native-pointer value operation)
      (cffi:null-pointer)))

(-> nemo-relay--new-native-handle (symbol string function) nemo-relay-native-handle)
(defun nemo-relay--new-native-handle (class operation function)
  "Call FUNCTION with an output slot and wrap its owned pointer in CLASS."
  (nemo-relay--ensure-native-library)
  (let ((slot (nemo-relay--make-output-slot)))
    (unwind-protect
         (progn
           (nemo-relay--require-status operation (lambda () (funcall function slot)))
           (let ((pointer (nemo-relay--output-slot-value slot)))
             (if (nemo-relay--pointer-present-p pointer)
                 (make-instance class :pointer pointer)
                 (nemo-relay--signal-error
                  (format nil "~A returned no handle." operation)
                  operation))))
      (cffi:foreign-free slot))))

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


;;;; -- Relay Scope and Metric Values --

(defparameter *nemo-relay-scope-type-codes*
  '(("agent" . 0)
    ("function" . 1)
    ("tool" . 2)
    ("llm" . 3)
    ("retriever" . 4)
    ("embedder" . 5)
    ("reranker" . 6)
    ("guardrail" . 7)
    ("evaluator" . 8)
    ("custom" . 9)
    ("unknown" . 10))
  "The pinned Relay scope type names and their C ABI values.")

(defparameter *nemo-relay-metric-kind-codes*
  '(("counter" . 1)
    ("up_down_counter" . 2)
    ("gauge" . 3)
    ("histogram" . 4))
  "The pinned Relay metric instrument kind names and their C ABI values.")

(defparameter *nemo-relay-metric-value-type-codes*
  '(("u64" . 1)
    ("i64" . 2)
    ("f64" . 3))
  "The pinned Relay metric value type names and their C ABI values.")

(defparameter *nemo-relay-uint64-max* (1- (expt 2 64))
  "The maximum unsigned 64-bit metric value accepted by Relay.")

(defparameter *nemo-relay-int64-min* (- (expt 2 63))
  "The minimum signed 64-bit metric value accepted by Relay.")

(defparameter *nemo-relay-int64-max* (1- (expt 2 63))
  "The maximum signed 64-bit metric value accepted by Relay.")

(defparameter *nemo-relay-event-severity-codes*
  '(("trace" . 0)
    ("debug" . 1)
    ("info" . 2)
    ("warn" . 3)
    ("warning" . 3)
    ("error" . 4))
  "The pinned Relay event severity names and their C ABI values.")

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

(-> nemo-relay-scope-type-code (t) integer)
(defun nemo-relay-scope-type-code (scope-type)
  "Return the pinned integer code for SCOPE-TYPE."
  (if (integerp scope-type)
      (if (rassoc scope-type *nemo-relay-scope-type-codes* :test #'=)
          scope-type
          (nemo-relay--signal-error
           (format nil "Invalid Relay scope type code ~D." scope-type)
           "Relay scope type"))
      (or (cdr (assoc (nemo-relay--normalized-name scope-type)
                      *nemo-relay-scope-type-codes*
                      :test #'string=))
          (nemo-relay--signal-error
           (format nil "Unknown Relay scope type ~S." scope-type)
           "Relay scope type"))))

(-> nemo-relay-scope-type-name (integer) keyword)
(defun nemo-relay-scope-type-name (scope-type-code)
  "Return the keyword name for a pinned Relay scope type code."
  (let ((name (first (rassoc scope-type-code *nemo-relay-scope-type-codes* :test #'=))))
    (if name
        (intern (string-upcase name) :keyword)
        (nemo-relay--signal-error
         (format nil "Unknown Relay scope type code ~D." scope-type-code)
         "Relay scope type"))))

(-> nemo-relay-metric-kind-code (t) integer)
(defun nemo-relay-metric-kind-code (kind)
  "Return the pinned integer code for metric KIND."
  (if (integerp kind)
      (if (rassoc kind *nemo-relay-metric-kind-codes* :test #'=)
          kind
          (nemo-relay--signal-error
           (format nil "Invalid Relay metric kind code ~D." kind)
           "Relay metric kind"))
      (or (cdr (assoc (nemo-relay--normalized-name kind)
                      *nemo-relay-metric-kind-codes*
                      :test #'string=))
          (nemo-relay--signal-error
           (format nil "Unknown Relay metric kind ~S." kind)
           "Relay metric kind"))))

(-> nemo-relay-metric-value-type-code (t) integer)
(defun nemo-relay-metric-value-type-code (value-type)
  "Return the pinned integer code for metric VALUE-TYPE."
  (if (integerp value-type)
      (if (rassoc value-type *nemo-relay-metric-value-type-codes* :test #'=)
          value-type
          (nemo-relay--signal-error
           (format nil "Invalid Relay metric value type code ~D." value-type)
           "Relay metric value type"))
      (or (cdr (assoc (nemo-relay--normalized-name value-type)
                      *nemo-relay-metric-value-type-codes*
                      :test #'string=))
          (nemo-relay--signal-error
           (format nil "Unknown Relay metric value type ~S." value-type)
           "Relay metric value type"))))

(defclass nemo-relay-scope-handle (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned pinned NeMo Relay scope handle."))

(defclass nemo-relay-tool-handle (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned pinned NeMo Relay tool handle."))

(defclass nemo-relay-llm-handle (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned pinned NeMo Relay LLM handle."))

(defclass nemo-relay-scope-stack (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned isolated NeMo Relay scope stack."))

(defclass nemo-relay-thread-scope-stack-binding (nemo-relay-native-handle) ()
  (:documentation "A captured thread binding consumed by Relay restoration."))

(defclass nemo-relay-event-view ()
  ((pointer
    :initarg :pointer
    :reader nemo-relay-event-view-pointer
    :documentation "The callback-scoped FfiEvent pointer."))
  (:documentation
   "A non-owning view of an event valid only during a Relay callback."))

(defclass nemo-relay-metric-measurement ()
  ((name
    :initarg :name
    :reader nemo-relay-metric-measurement-name
    :type string
    :documentation "The OpenTelemetry instrument name.")
   (kind
    :initarg :kind
    :reader nemo-relay-metric-measurement-kind
    :documentation "The metric kind symbol or pinned integer.")
   (value-type
    :initarg :value-type
    :reader nemo-relay-metric-measurement-value-type
    :documentation "The numeric value type symbol or pinned integer.")
   (value
    :initarg :value
    :reader nemo-relay-metric-measurement-value
    :documentation "The numeric value selected by VALUE-TYPE.")
   (unit
    :initarg :unit
    :initform nil
    :reader nemo-relay-metric-measurement-unit
    :type (option string)
    :documentation "The optional metric unit.")
   (description
    :initarg :description
    :initform nil
    :reader nemo-relay-metric-measurement-description
    :type (option string)
    :documentation "The optional metric description.")
   (attributes
    :initarg :attributes
    :initform nil
    :reader nemo-relay-metric-measurement-attributes
    :documentation "Optional JSON object attributes.")
   (boundaries
    :initarg :boundaries
    :initform nil
    :reader nemo-relay-metric-measurement-boundaries
    :documentation
    "Optional histogram boundaries; NIL and an empty list retain distinct meanings."))
  (:documentation "One typed measurement for Relay's metric C ABI."))


;;;; -- Generic Scopes, Events, and Metrics --

(-> nemo-relay-get-handle () nemo-relay-scope-handle)
(defun nemo-relay-get-handle ()
  "Return the current top-of-stack Relay scope handle."
  (nemo-relay--new-native-handle
   'nemo-relay-scope-handle
   "nemo_relay_get_handle"
   #'%nemo-relay-get-handle))

(-> nemo-relay-push-scope
    (&key (:name string) (:scope-type t) (:parent (option t))
          (:attributes integer) (:data t) (:metadata t) (:input t)
          (:timestamp (option integer)))
    nemo-relay-scope-handle)
(defun nemo-relay-push-scope
    (&key name scope-type parent (attributes 0) data metadata input timestamp)
  "Push a generic Relay scope and return its explicitly owned handle."
  (unless (and (stringp name) (non-empty-string-p name))
    (nemo-relay--signal-error "Relay scope name must be a non-empty string."
                         "nemo_relay_push_scope"))
  (unless (and (integerp attributes) (<= 0 attributes #xffffffff))
    (nemo-relay--signal-error "Relay scope attributes must be a uint32."
                         "nemo_relay_push_scope"))
  (let ((scope-code (nemo-relay-scope-type-code scope-type))
        (parent-pointer (nemo-relay--optional-native-pointer
                         parent "nemo_relay_push_scope parent"))
        (data-json (nemo-relay--json-value data "Relay scope data"))
        (metadata-json (nemo-relay--json-value metadata "Relay scope metadata"))
        (input-json (nemo-relay--json-value input "Relay scope input")))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error "Relay scope timestamp must be an integer Unix microsecond value."
                           "nemo_relay_push_scope"))
    (nemo-relay--new-native-handle
     'nemo-relay-scope-handle
     "nemo_relay_push_scope"
     (lambda (slot)
       (nemo-relay--call-with-c-strings
        (list name data-json metadata-json input-json)
        (lambda (name-pointer data-pointer metadata-pointer input-pointer)
          (nemo-relay--call-with-timestamp
           timestamp
           (lambda (timestamp-pointer)
             (%nemo-relay-push-scope
              name-pointer scope-code parent-pointer attributes
              data-pointer metadata-pointer input-pointer timestamp-pointer slot)))))))))

(-> nemo-relay-pop-scope
    (&key (:handle t) (:output t) (:metadata t) (:timestamp (option integer)))
    boolean)
(defun nemo-relay-pop-scope (&key handle output metadata timestamp)
  "Pop HANDLE and emit its Relay scope-end event."
  (let ((pointer (nemo-relay--native-pointer handle "nemo_relay_pop_scope handle"))
        (output-json (nemo-relay--json-value output "Relay scope output"))
        (metadata-json (nemo-relay--json-value metadata "Relay scope metadata")))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error "Relay scope timestamp must be an integer Unix microsecond value."
                           "nemo_relay_pop_scope"))
    (nemo-relay--call-with-c-strings
     (list output-json metadata-json)
     (lambda (output-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           "nemo_relay_pop_scope"
           (lambda ()
             (%nemo-relay-pop-scope
              pointer output-pointer metadata-pointer timestamp-pointer)))))))
    t))


;;;; -- Manual Tool and LLM Lifecycles --

(-> nemo-relay-tool-call
    (&key (:name string) (:arguments t) (:parent (option t))
          (:attributes integer) (:data t) (:metadata t)
          (:call-id (option string)) (:timestamp (option integer)))
    nemo-relay-tool-handle)
(defun nemo-relay-tool-call
    (&key name arguments parent (attributes 0) data metadata call-id timestamp)
  "Begin a manual Relay tool lifecycle span and return its owned handle."
  (let* ((operation "nemo_relay_tool_call")
         (arguments-json (nemo-relay--json-value arguments "Relay tool arguments"))
         (parent-pointer (nemo-relay--optional-native-pointer parent operation))
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
    (nemo-relay--new-native-handle
     'nemo-relay-tool-handle
     operation
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
              data-pointer metadata-pointer call-id-pointer timestamp-pointer slot)))))))))

(-> nemo-relay-tool-call-end
    (&key (:handle t) (:result t) (:data t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-tool-call-end (&key handle result data metadata timestamp)
  "Finish a manual Relay tool lifecycle span."
  (let* ((operation "nemo_relay_tool_call_end")
         (handle-pointer
           (nemo-relay--typed-native-pointer
            handle 'nemo-relay-tool-handle operation))
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
    nemo-relay-llm-handle)
(defun nemo-relay-llm-call
    (&key name native parent (attributes 0) data metadata model-name timestamp)
  "Begin a manual Relay LLM lifecycle span and return its owned handle."
  (let* ((operation "nemo_relay_llm_call")
         (native-json (nemo-relay--json-object-value native "Relay LLM request"))
         (parent-pointer (nemo-relay--optional-native-pointer parent operation))
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
    (nemo-relay--new-native-handle
     'nemo-relay-llm-handle
     operation
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
              data-pointer metadata-pointer model-pointer timestamp-pointer slot)))))))))

(-> nemo-relay-llm-call-end
    (&key (:handle t) (:response t) (:data t) (:metadata t)
          (:timestamp (option integer)))
    boolean)
(defun nemo-relay-llm-call-end (&key handle response data metadata timestamp)
  "Finish a manual Relay LLM lifecycle span."
  (let* ((operation "nemo_relay_llm_call_end")
         (handle-pointer
           (nemo-relay--typed-native-pointer
            handle 'nemo-relay-llm-handle operation))
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
  "Emit a Relay mark event without a schema or severity."
  (nemo-relay-event-v2 :name name
                  :parent parent
                  :data data
                  :metadata metadata
                  :timestamp timestamp))

(-> nemo-relay--severity-code (t) (option integer))
(defun nemo-relay--severity-code (severity)
  "Return the pinned integer code for an optional event severity."
  (when severity
      (if (integerp severity)
          (if (rassoc severity *nemo-relay-event-severity-codes* :test #'=)
              severity
              (nemo-relay--signal-error
               (format nil "Invalid Relay event severity code ~D." severity)
               "nemo_relay_event_v2"))
          (or (cdr (assoc (nemo-relay--normalized-name severity)
                          *nemo-relay-event-severity-codes*
                          :test #'string=))
            (nemo-relay--signal-error
             (format nil "Unknown Relay event severity ~S." severity)
             "nemo_relay_event_v2")))))

(-> nemo-relay-event-v2
    (&key (:name string) (:parent (option t)) (:data t) (:data-schema t)
          (:metadata t) (:severity (option t)) (:timestamp (option integer)))
    boolean)
(defun nemo-relay-event-v2
    (&key name parent data data-schema metadata severity timestamp)
  "Emit a Relay mark event with optional schema and log severity."
  (unless (and (stringp name) (non-empty-string-p name))
    (nemo-relay--signal-error "Relay event name must be a non-empty string."
                         "nemo_relay_event_v2"))
  (let* ((parent-pointer (nemo-relay--optional-native-pointer
                          parent "nemo_relay_event_v2 parent"))
         (data-json (nemo-relay--json-value data "Relay event data"))
         (schema-json (nemo-relay--json-object-value
                       data-schema "Relay event data schema"))
         (metadata-json (nemo-relay--json-value metadata "Relay event metadata"))
         (severity-code (nemo-relay--severity-code severity)))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error "Relay event timestamp must be an integer Unix microsecond value."
                           "nemo_relay_event_v2"))
    (nemo-relay--call-with-c-strings
     (list name data-json schema-json metadata-json)
     (lambda (name-pointer data-pointer schema-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--call-with-severity
           severity-code
           (lambda (severity-pointer)
             (nemo-relay--require-status
              "nemo_relay_event_v2"
              (lambda ()
                (%nemo-relay-event-v2
                 name-pointer parent-pointer data-pointer schema-pointer
                 metadata-pointer severity-pointer timestamp-pointer)))))))))
    t))

(-> nemo-relay-metric-json
    (&key (:name string) (:parent (option t)) (:measurements t)
          (:metadata t) (:timestamp (option integer)))
    boolean)
(defun nemo-relay-metric-json (&key name parent measurements metadata timestamp)
  "Emit a Relay metric mark from canonical measurement JSON."
  (unless (and (stringp name) (non-empty-string-p name))
    (nemo-relay--signal-error "Relay metric name must be a non-empty string."
                         "nemo_relay_metric_json"))
  (let ((measurements-json (nemo-relay--json-value
                            measurements "Relay metric measurements"))
        (parent-pointer (nemo-relay--optional-native-pointer
                         parent "nemo_relay_metric_json parent"))
        (metadata-json (nemo-relay--json-value metadata "Relay metric metadata")))
    (unless measurements-json
      (nemo-relay--signal-error "Relay metric measurements are required."
                           "nemo_relay_metric_json"))
    (let ((decoded
            (handler-case
                (json-decode measurements-json)
              (nemo-relay-error (condition)
                (error condition))
              (serious-condition (condition)
                (nemo-relay--signal-error
                 (format nil "Relay metric measurements are not valid JSON: ~A"
                         (nemo-relay--condition-summary condition))
                 "nemo_relay_metric_json")))))
      (unless (vectorp decoded)
        (nemo-relay--signal-error
         "Relay metric measurements must be a JSON array."
         "nemo_relay_metric_json"))
      (when (zerop (length decoded))
        (nemo-relay--signal-error
         "Relay metric measurements must not be empty."
         "nemo_relay_metric_json")))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error "Relay metric timestamp must be an integer Unix microsecond value."
                           "nemo_relay_metric_json"))
    (nemo-relay--call-with-c-strings
     (list name measurements-json metadata-json)
     (lambda (name-pointer measurements-pointer metadata-pointer)
       (nemo-relay--call-with-timestamp
        timestamp
        (lambda (timestamp-pointer)
          (nemo-relay--require-status
           "nemo_relay_metric_json"
           (lambda ()
             (%nemo-relay-metric-json
              name-pointer parent-pointer measurements-pointer metadata-pointer
              timestamp-pointer)))))))
      t))


(-> nemo-relay--typed-native-pointer (t symbol string) t)
(defun nemo-relay--typed-native-pointer (handle class operation)
  "Return HANDLE's pointer after enforcing the expected native handle class."
  (unless (or (typep handle class) (cffi:pointerp handle))
    (nemo-relay--signal-error
     (format nil "~A requires a ~A or foreign pointer." operation class)
     operation))
  (nemo-relay--native-pointer handle operation))

(-> nemo-relay--free-native-handle
    (t symbol function &key (:expected string) (:operation string))
    boolean)
(defun nemo-relay--free-native-handle
    (handle class free-function &key expected operation)
  "Release HANDLE when it is an instance of CLASS or a raw foreign pointer."
  (cond
    ((typep handle class)
     (unless (nemo-relay-native-handle-freed-p handle)
       (funcall free-function (nemo-relay-native-handle-pointer handle))
       (setf (nemo-relay-native-handle-freed-p handle) t))
     t)
    ((cffi:pointerp handle)
     (funcall free-function handle)
     t)
    ((null handle)
     t)
    (t
     (nemo-relay--signal-error
      (format nil "Expected a ~A." expected)
      operation))))

(-> nemo-relay--handle-accessor-pointer
    (t symbol function &key (:operation string))
    t)
(defun nemo-relay--handle-accessor-pointer
    (handle class accessor &key operation)
  "Call ACCESSOR on a typed native HANDLE and return its pointer or integer."
  (nemo-relay--ensure-native-library)
  (funcall accessor
           (nemo-relay--typed-native-pointer handle class operation)))

;;;; -- Scope and Event Accessors --

(-> nemo-relay-scope-handle-free (t) boolean)
(defun nemo-relay-scope-handle-free (handle)
  "Release HANDLE, accepting either a wrapped or raw foreign pointer."
  (nemo-relay--free-native-handle
   handle
   'nemo-relay-scope-handle
   #'%nemo-relay-scope-handle-free
   :expected "Relay scope handle"
   :operation "nemo_relay_scope_handle_free"))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-scope-handle))
  "Release a wrapped Relay scope handle."
  (nemo-relay-scope-handle-free handle))

(-> nemo-relay--scope-accessor-pointer (t string function) t)
(defun nemo-relay--scope-accessor-pointer (handle operation accessor)
  "Call ACCESSOR on a Relay scope HANDLE and return its foreign value."
  (nemo-relay--handle-accessor-pointer
   handle 'nemo-relay-scope-handle accessor :operation operation))

(-> nemo-relay-scope-handle-uuid (t) (option string))
(defun nemo-relay-scope-handle-uuid (handle)
  "Return HANDLE's UUID as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--scope-accessor-pointer
    handle "nemo_relay_scope_handle_uuid"
    #'%nemo-relay-scope-handle-uuid)))

(-> nemo-relay-scope-handle-name (t) (option string))
(defun nemo-relay-scope-handle-name (handle)
  "Return HANDLE's name as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--scope-accessor-pointer
    handle "nemo_relay_scope_handle_name"
    #'%nemo-relay-scope-handle-name)))

(-> nemo-relay-scope-handle-scope-type-code (t) integer)
(defun nemo-relay-scope-handle-scope-type-code (handle)
  "Return HANDLE's pinned integer scope type code."
  (nemo-relay--scope-accessor-pointer
   handle "nemo_relay_scope_handle_scope_type"
   #'%nemo-relay-scope-handle-scope-type))

(-> nemo-relay-scope-handle-scope-type (t) keyword)
(defun nemo-relay-scope-handle-scope-type (handle)
  "Return HANDLE's scope type as a keyword."
  (nemo-relay-scope-type-name (nemo-relay-scope-handle-scope-type-code handle)))

(-> nemo-relay-scope-handle-attributes (t) integer)
(defun nemo-relay-scope-handle-attributes (handle)
  "Return HANDLE's raw scope attribute bitfield."
  (nemo-relay--scope-accessor-pointer
   handle "nemo_relay_scope_handle_attributes"
   #'%nemo-relay-scope-handle-attributes))

(-> nemo-relay-scope-handle-parent-uuid (t) (option string))
(defun nemo-relay-scope-handle-parent-uuid (handle)
  "Return HANDLE's optional parent UUID."
  (nemo-relay--returned-string
   (nemo-relay--scope-accessor-pointer
    handle "nemo_relay_scope_handle_parent_uuid"
    #'%nemo-relay-scope-handle-parent-uuid)))

(-> nemo-relay-scope-handle-data (t) t)
(defun nemo-relay-scope-handle-data (handle)
  "Return HANDLE's optional JSON data payload."
  (let ((source
          (nemo-relay--returned-string
           (nemo-relay--scope-accessor-pointer
            handle "nemo_relay_scope_handle_data"
            #'%nemo-relay-scope-handle-data))))
    (and source (json-decode source))))

(-> nemo-relay-scope-handle-metadata (t) t)
(defun nemo-relay-scope-handle-metadata (handle)
  "Return HANDLE's optional JSON metadata payload."
  (let ((source
          (nemo-relay--returned-string
           (nemo-relay--scope-accessor-pointer
            handle "nemo_relay_scope_handle_metadata"
            #'%nemo-relay-scope-handle-metadata))))
    (and source (json-decode source))))


;;;; -- Tool and LLM Handle Accessors --

(-> nemo-relay-tool-handle-free (t) boolean)
(defun nemo-relay-tool-handle-free (handle)
  "Release HANDLE, accepting either a wrapped or raw foreign pointer."
  (nemo-relay--free-native-handle
   handle
   'nemo-relay-tool-handle
   #'%nemo-relay-tool-handle-free
   :expected "Relay tool handle"
   :operation "nemo_relay_tool_handle_free"))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-tool-handle))
  "Release a wrapped Relay tool handle."
  (nemo-relay-tool-handle-free handle))

(-> nemo-relay-tool-handle-uuid (t) (option string))
(defun nemo-relay-tool-handle-uuid (handle)
  "Return HANDLE's UUID as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-tool-handle #'%nemo-relay-tool-handle-uuid
    :operation "nemo_relay_tool_handle_uuid")))

(-> nemo-relay-tool-handle-name (t) (option string))
(defun nemo-relay-tool-handle-name (handle)
  "Return HANDLE's name as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-tool-handle #'%nemo-relay-tool-handle-name
    :operation "nemo_relay_tool_handle_name")))

(-> nemo-relay-tool-handle-attributes (t) integer)
(defun nemo-relay-tool-handle-attributes (handle)
  "Return HANDLE's raw tool attribute bitfield."
  (nemo-relay--handle-accessor-pointer
   handle 'nemo-relay-tool-handle #'%nemo-relay-tool-handle-attributes
   :operation "nemo_relay_tool_handle_attributes"))

(-> nemo-relay-tool-handle-parent-uuid (t) (option string))
(defun nemo-relay-tool-handle-parent-uuid (handle)
  "Return HANDLE's optional parent UUID."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-tool-handle #'%nemo-relay-tool-handle-parent-uuid
    :operation "nemo_relay_tool_handle_parent_uuid")))

(-> nemo-relay-llm-handle-free (t) boolean)
(defun nemo-relay-llm-handle-free (handle)
  "Release HANDLE, accepting either a wrapped or raw foreign pointer."
  (nemo-relay--free-native-handle
   handle
   'nemo-relay-llm-handle
   #'%nemo-relay-llm-handle-free
   :expected "Relay LLM handle"
   :operation "nemo_relay_llm_handle_free"))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-llm-handle))
  "Release a wrapped Relay LLM handle."
  (nemo-relay-llm-handle-free handle))

(-> nemo-relay-llm-handle-uuid (t) (option string))
(defun nemo-relay-llm-handle-uuid (handle)
  "Return HANDLE's UUID as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-llm-handle #'%nemo-relay-llm-handle-uuid
    :operation "nemo_relay_llm_handle_uuid")))

(-> nemo-relay-llm-handle-name (t) (option string))
(defun nemo-relay-llm-handle-name (handle)
  "Return HANDLE's name as a copied string."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-llm-handle #'%nemo-relay-llm-handle-name
    :operation "nemo_relay_llm_handle_name")))

(-> nemo-relay-llm-handle-attributes (t) integer)
(defun nemo-relay-llm-handle-attributes (handle)
  "Return HANDLE's raw LLM attribute bitfield."
  (nemo-relay--handle-accessor-pointer
   handle 'nemo-relay-llm-handle #'%nemo-relay-llm-handle-attributes
   :operation "nemo_relay_llm_handle_attributes"))

(-> nemo-relay-llm-handle-parent-uuid (t) (option string))
(defun nemo-relay-llm-handle-parent-uuid (handle)
  "Return HANDLE's optional parent UUID."
  (nemo-relay--returned-string
   (nemo-relay--handle-accessor-pointer
    handle 'nemo-relay-llm-handle #'%nemo-relay-llm-handle-parent-uuid
    :operation "nemo_relay_llm_handle_parent_uuid")))
(-> nemo-relay--event-pointer (t) t)
(defun nemo-relay--event-pointer (event)
  "Return an event view's borrowed pointer or accept a raw pointer."
  (cond
    ((typep event 'nemo-relay-event-view)
     (nemo-relay-event-view-pointer event))
    ((cffi:pointerp event)
     event)
    (t
     (nemo-relay--signal-error "Expected a Relay event view or foreign pointer."
                          "Relay event accessor"))))

(-> nemo-relay--event-string (t string function) (option string))
(defun nemo-relay--event-string (event operation accessor)
  "Return a copied optional event string from ACCESSOR."
  (nemo-relay--ensure-native-library)
  (nemo-relay--returned-string (funcall accessor (nemo-relay--event-pointer event))))

(-> nemo-relay--event-json (t string function) t)
(defun nemo-relay--event-json (event operation accessor)
  "Return a decoded optional event JSON value from ACCESSOR."
  (let ((source (nemo-relay--event-string event operation accessor)))
    (and source
         (handler-case
             (json-decode source)
           (serious-condition (condition)
             (nemo-relay--signal-error
              (format nil "~A returned invalid JSON: ~A"
                      operation (nemo-relay--condition-summary condition))
              operation))))))

(-> nemo-relay-event-uuid (t) (option string))
(defun nemo-relay-event-uuid (event)
  "Return an event UUID."
  (nemo-relay--event-string event "nemo_relay_event_uuid" #'%nemo-relay-event-uuid))

(-> nemo-relay-event-name (t) (option string))
(defun nemo-relay-event-name (event)
  "Return an event name."
  (nemo-relay--event-string event "nemo_relay_event_name" #'%nemo-relay-event-name))

(-> nemo-relay-event-kind (t) (option string))
(defun nemo-relay-event-kind (event)
  "Return the event discriminator."
  (nemo-relay--event-string event "nemo_relay_event_kind" #'%nemo-relay-event-kind))

(-> nemo-relay-event-json (t) t)
(defun nemo-relay-event-json (event)
  "Return the canonical event JSON as a decoded JSON value."
  (nemo-relay--event-json event "nemo_relay_event_json" #'%nemo-relay-event-json))

(-> nemo-relay-event-atof-version (t) (option string))
(defun nemo-relay-event-atof-version (event)
  "Return the event's ATOF version."
  (nemo-relay--event-string event "nemo_relay_event_atof_version"
                       #'%nemo-relay-event-atof-version))

(-> nemo-relay-event-scope-category (t) (option string))
(defun nemo-relay-event-scope-category (event)
  "Return START or END for a scope event, or NIL for a mark."
  (nemo-relay--event-string event "nemo_relay_event_scope_category"
                       #'%nemo-relay-event-scope-category))

(-> nemo-relay-event-category (t) (option string))
(defun nemo-relay-event-category (event)
  "Return the optional ATOF event category."
  (nemo-relay--event-string event "nemo_relay_event_category"
                       #'%nemo-relay-event-category))

(-> nemo-relay-event-attributes (t) integer)
(defun nemo-relay-event-attributes (event)
  "Return the raw event attribute bitfield."
  (nemo-relay--ensure-native-library)
  (%nemo-relay-event-attributes (nemo-relay--event-pointer event)))

(-> nemo-relay-event-attributes-json (t) t)
(defun nemo-relay-event-attributes-json (event)
  "Return the event's optional JSON attribute array."
  (nemo-relay--event-json event "nemo_relay_event_attributes_json"
                     #'%nemo-relay-event-attributes-json))

(-> nemo-relay-event-category-profile (t) t)
(defun nemo-relay-event-category-profile (event)
  "Return the event's optional category profile."
  (nemo-relay--event-json event "nemo_relay_event_category_profile"
                     #'%nemo-relay-event-category-profile))

(-> nemo-relay-event-data (t) t)
(defun nemo-relay-event-data (event)
  "Return the event's optional application data."
  (nemo-relay--event-json event "nemo_relay_event_data" #'%nemo-relay-event-data))

(-> nemo-relay-event-data-schema (t) t)
(defun nemo-relay-event-data-schema (event)
  "Return the event's optional data schema."
  (nemo-relay--event-json event "nemo_relay_event_data_schema"
                     #'%nemo-relay-event-data-schema))

(-> nemo-relay-event-metadata (t) t)
(defun nemo-relay-event-metadata (event)
  "Return the event's optional metadata."
  (nemo-relay--event-json event "nemo_relay_event_metadata"
                     #'%nemo-relay-event-metadata))

(-> nemo-relay-event-timestamp (t) (option string))
(defun nemo-relay-event-timestamp (event)
  "Return the event timestamp as RFC 3339 text."
  (nemo-relay--event-string event "nemo_relay_event_timestamp"
                       #'%nemo-relay-event-timestamp))

(-> nemo-relay-event-input (t) t)
(defun nemo-relay-event-input (event)
  "Return the event's optional semantic input."
  (nemo-relay--event-json event "nemo_relay_event_input" #'%nemo-relay-event-input))

(-> nemo-relay-event-output (t) t)
(defun nemo-relay-event-output (event)
  "Return the event's optional semantic output."
  (nemo-relay--event-json event "nemo_relay_event_output" #'%nemo-relay-event-output))

(-> nemo-relay-event-model-name (t) (option string))
(defun nemo-relay-event-model-name (event)
  "Return the event's optional model name."
  (nemo-relay--event-string event "nemo_relay_event_model_name"
                       #'%nemo-relay-event-model-name))

(-> nemo-relay-event-tool-call-id (t) (option string))
(defun nemo-relay-event-tool-call-id (event)
  "Return the event's optional tool call ID."
  (nemo-relay--event-string event "nemo_relay_event_tool_call_id"
                       #'%nemo-relay-event-tool-call-id))

(-> nemo-relay-event-parent-uuid (t) (option string))
(defun nemo-relay-event-parent-uuid (event)
  "Return the event's optional parent UUID."
  (nemo-relay--event-string event "nemo_relay_event_parent_uuid"
                       #'%nemo-relay-event-parent-uuid))

(-> nemo-relay-event-scope-type (t) (option string))
(defun nemo-relay-event-scope-type (event)
  "Return the event's optional scope type name."
  (nemo-relay--event-string event "nemo_relay_event_scope_type"
                       #'%nemo-relay-event-scope-type))

(-> nemo-relay-event-annotated-request (t) t)
(defun nemo-relay-event-annotated-request (event)
  "Return an optional codec-normalized LLM request."
  (nemo-relay--event-json event "nemo_relay_event_annotated_request"
                     #'%nemo-relay-event-annotated-request))

(-> nemo-relay-event-annotated-response (t) t)
(defun nemo-relay-event-annotated-response (event)
  "Return an optional codec-normalized LLM response."
  (nemo-relay--event-json event "nemo_relay_event_annotated_response"
                     #'%nemo-relay-event-annotated-response))


;;;; -- Scope Stack Propagation --

(-> nemo-relay-capture-propagation-context () string)
(defun nemo-relay-capture-propagation-context ()
  "Capture the current Relay propagation context as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_capture_propagation_context_json"
   #'%nemo-relay-capture-propagation-context-json))

(-> nemo-relay-capture-propagation-context-with-root
    (&key (:root-uuid (option string)))
    string)
(defun nemo-relay-capture-propagation-context-with-root (&key root-uuid)
  "Capture propagation context JSON with an optional application root UUID."
  (when (and root-uuid (not (stringp root-uuid)))
    (nemo-relay--signal-error "Relay root UUID must be a string or NIL."
                         "nemo_relay_capture_propagation_context_with_root_json"))
  (nemo-relay--call-with-c-strings
   (list root-uuid)
   (lambda (root-pointer)
     (nemo-relay--required-string-output
      "nemo_relay_capture_propagation_context_with_root_json"
      (lambda (slot)
        (%nemo-relay-capture-propagation-context-with-root-json
         root-pointer slot))))))

(-> nemo-relay-capture-traceparent () string)
(defun nemo-relay-capture-traceparent ()
  "Capture the current Relay context as a W3C traceparent value."
  (nemo-relay--required-string-output
   "nemo_relay_capture_traceparent"
   #'%nemo-relay-capture-traceparent))

(-> nemo-relay-propagation-context-to-traceparent (t) string)
(defun nemo-relay-propagation-context-to-traceparent (context)
  "Convert propagation-context JSON CONTEXT to a W3C traceparent value."
  (let ((context-json (nemo-relay--json-value
                       context "Relay propagation context")))
    (unless context-json
      (nemo-relay--signal-error "Relay propagation context is required."
                           "nemo_relay_propagation_context_to_traceparent"))
    (nemo-relay--call-with-c-strings
     (list context-json)
     (lambda (context-pointer)
       (nemo-relay--required-string-output
        "nemo_relay_propagation_context_to_traceparent"
        (lambda (slot)
          (%nemo-relay-propagation-context-to-traceparent
           context-pointer slot)))))))

(-> nemo-relay-scope-stack-create () nemo-relay-scope-stack)
(defun nemo-relay-scope-stack-create ()
  "Create an isolated Relay scope stack."
  (nemo-relay--new-native-handle
   'nemo-relay-scope-stack
   "nemo_relay_scope_stack_create"
   #'%nemo-relay-scope-stack-create))

(-> nemo-relay-scope-stack-create-from-propagation-context (t) nemo-relay-scope-stack)
(defun nemo-relay-scope-stack-create-from-propagation-context (context)
  "Create an isolated scope stack from propagation-context JSON CONTEXT."
  (let ((context-json (nemo-relay--json-value
                       context "Relay propagation context")))
    (unless context-json
      (nemo-relay--signal-error "Relay propagation context is required."
                           "nemo_relay_scope_stack_create_from_propagation_json"))
    (nemo-relay--call-with-c-strings
     (list context-json)
     (lambda (context-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-scope-stack
        "nemo_relay_scope_stack_create_from_propagation_json"
        (lambda (slot)
          (%nemo-relay-scope-stack-create-from-propagation-json
           context-pointer slot)))))))

(-> nemo-relay-scope-stack-set-thread (t) boolean)
(defun nemo-relay-scope-stack-set-thread (stack)
  "Bind STACK to the current thread without consuming it."
  (nemo-relay--require-status
   "nemo_relay_scope_stack_set_thread"
   (lambda ()
     (%nemo-relay-scope-stack-set-thread
      (nemo-relay--native-pointer stack "nemo_relay_scope_stack_set_thread"))))
  t)

(-> nemo-relay-scope-stack-capture-thread () nemo-relay-thread-scope-stack-binding)
(defun nemo-relay-scope-stack-capture-thread ()
  "Capture the current thread's Relay scope-stack binding."
  (nemo-relay--new-native-handle
   'nemo-relay-thread-scope-stack-binding
   "nemo_relay_scope_stack_capture_thread"
   #'%nemo-relay-scope-stack-capture-thread))

(-> nemo-relay-scope-stack-restore-thread (t) boolean)
(defun nemo-relay-scope-stack-restore-thread (binding)
  "Restore and consume a captured thread scope-stack BINDING."
  (let ((pointer (nemo-relay--native-pointer
                  binding "nemo_relay_scope_stack_restore_thread")))
    (nemo-relay--require-status
     "nemo_relay_scope_stack_restore_thread"
     (lambda () (%nemo-relay-scope-stack-restore-thread pointer)))
    (when (typep binding 'nemo-relay-thread-scope-stack-binding)
      (setf (nemo-relay-native-handle-freed-p binding) t))
    t))

(-> nemo-relay-scope-stack-free (t) boolean)
(defun nemo-relay-scope-stack-free (stack)
  "Release an isolated Relay scope stack."
  (cond
    ((typep stack 'nemo-relay-scope-stack)
     (unless (nemo-relay-native-handle-freed-p stack)
       (%nemo-relay-scope-stack-free (nemo-relay-native-handle-pointer stack))
       (setf (nemo-relay-native-handle-freed-p stack) t))
     t)
    ((cffi:pointerp stack)
     (%nemo-relay-scope-stack-free stack)
     t)
    ((null stack)
     t)
    (t
     (nemo-relay--signal-error "Expected a Relay scope stack."
                          "nemo_relay_scope_stack_free"))))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-scope-stack))
  "Release a wrapped Relay scope stack."
  (nemo-relay-scope-stack-free handle))

(-> nemo-relay-thread-scope-stack-binding-free (t) boolean)
(defun nemo-relay-thread-scope-stack-binding-free (binding)
  "Restore and consume BINDING, which Relay frees during restoration."
  (nemo-relay-scope-stack-restore-thread binding))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-thread-scope-stack-binding))
  "Restore a wrapped captured thread binding."
  (nemo-relay-thread-scope-stack-binding-free handle))

(-> nemo-relay-scope-stack-active-p () boolean)
(defun nemo-relay-scope-stack-active-p ()
  "Return whether the current thread has an explicit Relay scope stack."
  (nemo-relay--ensure-native-library)
  (if (%nemo-relay-scope-stack-active) t nil))


;;;; -- Typed Metrics --

(-> nemo-relay--measurement-field (t string t) t)
(defun nemo-relay--measurement-field (measurement key accessor)
  "Read a field from a measurement object, JSON object, or property list."
  (cond
    ((typep measurement 'nemo-relay-metric-measurement)
     (funcall accessor measurement))
    ((json-object-p measurement)
     (json-get measurement key *nemo-relay-missing-value*))
    ((listp measurement)
     (multiple-value-bind (value indicator)
         (get-properties
          measurement
          (list (intern (string-upcase key) :keyword)
                (intern (string-upcase (substitute #\- #\_ key)) :keyword)))
       (if indicator value *nemo-relay-missing-value*)))
    (t
     *nemo-relay-missing-value*)))


(-> nemo-relay--measurement-required-field (t string t) t)
(defun nemo-relay--measurement-required-field (measurement key accessor)
  "Read a required measurement field or signal a descriptive error."
  (let ((value (nemo-relay--measurement-field measurement key accessor)))
    (if (eq value *nemo-relay-missing-value*)
        (nemo-relay--signal-error
         (format nil "Relay metric measurement requires ~A." key)
         "nemo_relay_metric")
        value)))

(-> nemo-relay--measurement-accessor (string) function)
(defun nemo-relay--measurement-accessor (key)
  "Return the class slot accessor for optional measurement KEY."
  (cond
    ((string= key "unit") #'nemo-relay-metric-measurement-unit)
    ((string= key "description") #'nemo-relay-metric-measurement-description)
    ((or (string= key "attributes")
         (string= key "attributes_json"))
     #'nemo-relay-metric-measurement-attributes)
    ((string= key "boundaries") #'nemo-relay-metric-measurement-boundaries)
    (t (lambda (measurement)
         (declare (ignore measurement))
         *nemo-relay-missing-value*))))

(-> nemo-relay--measurement-optional-field (t string string) t)
(defun nemo-relay--measurement-optional-field (measurement key alternate-key)
  "Read an optional measurement field using two accepted spellings."
  (let ((value (nemo-relay--measurement-field
                measurement key (nemo-relay--measurement-accessor key))))
    (if (eq value *nemo-relay-missing-value*)
        (nemo-relay--measurement-field
         measurement alternate-key (nemo-relay--measurement-accessor alternate-key))
        value)))

(-> nemo-relay--metric-boundary-values (t) t)
(defun nemo-relay--metric-boundary-values (value)
  "Validate metric boundaries while preserving explicit empty vectors."
  (cond
    ((or (eq value *nemo-relay-missing-value*) (null value))
     nil)
    ((or (vectorp value) (listp value))
     value)
    (t
     (nemo-relay--signal-error "Relay metric boundaries must be a list or vector."
                          "nemo_relay_metric"))))

(-> nemo-relay--typed-measurements (t) list)
(defun nemo-relay--typed-measurements (measurements)
  "Normalize a typed measurement sequence to a list."
  (cond
    ((vectorp measurements) (coerce measurements 'list))
    ((listp measurements) measurements)
    (t
     (nemo-relay--signal-error "Relay typed measurements must be a list or vector."
                          "nemo_relay_metric"))))

(-> nemo-relay--metric-attributes (t) (option string))
(defun nemo-relay--metric-attributes (measurement)
  "Return an optional encoded measurement attributes object."
  (let ((value (nemo-relay--measurement-optional-field
                measurement "attributes" "attributes_json")))
    (unless (eq value *nemo-relay-missing-value*)
      (nemo-relay--json-object-value value "Relay metric attributes"))))

(-> nemo-relay--metric-string (t string string) (option string))
(defun nemo-relay--metric-string (measurement key operation)
  "Read and validate one optional metric string field."
  (let ((value (nemo-relay--measurement-optional-field measurement key key)))
    (cond
      ((eq value *nemo-relay-missing-value*) nil)
      ((null value) nil)
      ((stringp value) value)
      (t
       (nemo-relay--signal-error
        (format nil "Relay metric ~A must be a string." key)
        operation)))))

(-> nemo-relay--metric-native-call
    (string t list (option string) (option integer))
    boolean)
(defun nemo-relay--metric-native-call
    (name parent-pointer measurements metadata-json timestamp)
  "Marshal typed MEASUREMENTS and invoke Relay's typed metric function."
  (unless measurements
    (nemo-relay--signal-error "Relay metric measurements must not be empty."
                         "nemo_relay_metric"))
  (let* ((count (length measurements))
         (array (and (plusp count)
                     (cffi:foreign-alloc
                      '(:struct nemo-relay-metric-measurement)
                      :count count)))
         (owned nil))
    (labels
        ((remember-string (value)
           (if value
               (let ((pointer (cffi:foreign-string-alloc
                               value :encoding ':utf-8)))
                 (push (cons ':string pointer) owned)
                 pointer)
               (cffi:null-pointer)))

          (remember-boundaries (values)
            (if values
                (let ((pointer (cffi:foreign-alloc
                                :double :count (max 1 (length values)))))
                  (push (cons ':foreign pointer) owned)
                  (loop for index below (length values)
                        for value = (elt values index)
                        do (unless (realp value)
                             (nemo-relay--signal-error
                              "Relay metric histogram boundaries must be real numbers."
                              "nemo_relay_metric"))
                           (setf (cffi:mem-aref pointer :double index)
                                 (float value 1.0d0)))
                  pointer)
                (cffi:null-pointer)))

         (cleanup ()
           (dolist (entry owned)
             (if (eq (car entry) ':string)
                 (cffi:foreign-string-free (cdr entry))
                 (cffi:foreign-free (cdr entry))))))
      (unwind-protect
           (progn
             (loop for measurement in measurements
                   for index from 0
                   do (unless (or (typep measurement 'nemo-relay-metric-measurement)
                                  (json-object-p measurement)
                                  (listp measurement))
                        (nemo-relay--signal-error
                         (format nil "Relay metric measurement ~D has an unsupported shape."
                                 index)
                         "nemo_relay_metric"))
                      (let* ((struct (cffi:mem-aptr
                                      array
                                      '(:struct nemo-relay-metric-measurement)
                                      index))
                             (measurement-name
                               (nemo-relay--measurement-required-field
                                measurement "name"
                                #'nemo-relay-metric-measurement-name))
                             (kind
                               (nemo-relay-metric-kind-code
                                (nemo-relay--measurement-required-field
                                 measurement "kind"
                                 #'nemo-relay-metric-measurement-kind)))
                             (value-type
                               (nemo-relay-metric-value-type-code
                                (nemo-relay--measurement-required-field
                                 measurement "value_type"
                                 #'nemo-relay-metric-measurement-value-type)))
                             (value
                               (nemo-relay--measurement-required-field
                                measurement "value"
                                #'nemo-relay-metric-measurement-value))
                             (unit (nemo-relay--metric-string
                                    measurement "unit" "nemo_relay_metric"))
                             (description (nemo-relay--metric-string
                                           measurement "description"
                                           "nemo_relay_metric"))
                             (attributes (nemo-relay--metric-attributes measurement))
                             (boundaries
                               (nemo-relay--metric-boundary-values
                                (nemo-relay--measurement-optional-field
                                 measurement "boundaries" "boundaries"))))
                        (unless (and (stringp measurement-name)
                                     (non-empty-string-p measurement-name))
                          (nemo-relay--signal-error
                           (format nil "Relay metric measurement ~D requires a non-empty name."
                                   index)
                           "nemo_relay_metric"))
                        (setf (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'name)
                              (remember-string measurement-name)
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'kind)
                              kind
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'value-type)
                              value-type
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'u64-value)
                              0
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'i64-value)
                              0
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'f64-value)
                              0.0d0
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'unit)
                              (remember-string unit)
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'description)
                              (remember-string description)
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement)
                               'attributes-json)
                              (remember-string attributes)
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement) 'boundaries)
                              (remember-boundaries boundaries)
                              (cffi:foreign-slot-value
                               struct '(:struct nemo-relay-metric-measurement)
                               'boundaries-len)
                              (if boundaries (length boundaries) 0))
                        (case value-type
                          (1
                           (unless (and (integerp value)
                                        (<= 0 value *nemo-relay-uint64-max*))
                             (nemo-relay--signal-error
                              (format nil
                                      "Relay metric measurement ~D requires a uint64 value."
                                      index)
                              "nemo_relay_metric"))
                           (setf (cffi:foreign-slot-value
                                  struct '(:struct nemo-relay-metric-measurement)
                                  'u64-value)
                                 value))
                          (2
                           (unless (and (integerp value)
                                        (<= *nemo-relay-int64-min* value *nemo-relay-int64-max*))
                             (nemo-relay--signal-error
                              (format nil
                                      "Relay metric measurement ~D requires an int64 value."
                                      index)
                              "nemo_relay_metric"))
                           (setf (cffi:foreign-slot-value
                                  struct '(:struct nemo-relay-metric-measurement)
                                  'i64-value)
                                 value))
                          (3
                           (unless (realp value)
                             (nemo-relay--signal-error
                              (format nil
                                      "Relay metric measurement ~D requires a real F64 value."
                                      index)
                              "nemo_relay_metric"))
                           (setf (cffi:foreign-slot-value
                                  struct '(:struct nemo-relay-metric-measurement)
                                  'f64-value)
                                 (float value 1.0d0))))))
             (nemo-relay--call-with-c-strings
              (list name metadata-json)
              (lambda (name-pointer metadata-pointer)
                (nemo-relay--call-with-timestamp
                 timestamp
                 (lambda (timestamp-pointer)
                   (nemo-relay--require-status
                    "nemo_relay_metric"
                    (lambda ()
                      (%nemo-relay-metric
                       name-pointer parent-pointer array count metadata-pointer
                       timestamp-pointer)))))))
             t)
        (when array
          (cffi:foreign-free array))
        (cleanup)))))

(-> nemo-relay-metric
    (&key (:name string) (:parent (option t)) (:measurements t)
          (:metadata t) (:timestamp (option integer)))
    boolean)
(defun nemo-relay-metric (&key name parent measurements metadata timestamp)
  "Emit an atomically validated typed Relay metric mark."
  (unless (and (stringp name) (non-empty-string-p name))
    (nemo-relay--signal-error "Relay metric name must be a non-empty string."
                         "nemo_relay_metric"))
  (let ((parent-pointer (nemo-relay--optional-native-pointer
                         parent "nemo_relay_metric parent"))
        (metadata-json (nemo-relay--json-value metadata "Relay metric metadata"))
        (measurements (nemo-relay--typed-measurements measurements)))
    (unless measurements
      (nemo-relay--signal-error "Relay metric measurements must not be empty."
                           "nemo_relay_metric"))
    (when (and timestamp (not (integerp timestamp)))
      (nemo-relay--signal-error "Relay metric timestamp must be an integer Unix microsecond value."
                           "nemo_relay_metric"))
    (nemo-relay--metric-native-call name parent-pointer measurements metadata-json timestamp)))
