(in-package #:autolith)

;;;; -- Relay Callback State --

(defclass nemo-relay-callback-state ()
  ((callback
    :initarg :callback
    :accessor nemo-relay-callback-state-callback
    :documentation "The Lisp callback retained for a native registration.")
   (validator
    :initarg :validator
    :initform nil
    :accessor nemo-relay-callback-state-validator
    :documentation "The optional Lisp plugin validator retained for a registration.")
   (kind
    :initarg :kind
    :reader nemo-relay-callback-state-kind
    :documentation "The native callback family represented by this state.")
   (token
    :initarg :token
    :reader nemo-relay-callback-state-token
    :documentation "The stable foreign user-data token passed to Relay.")
   (released-p
    :initform nil
    :accessor nemo-relay-callback-state-released-p
    :documentation "Whether Relay has released this callback state."))
  (:documentation "Lisp state retained while Relay owns a callback registration."))

(defvar *nemo-relay-callback-state-lock*
  (make-lock "Autolith Relay callback states")
  "The lock protecting the native callback state registry.")

(defvar *nemo-relay-callback-states* (make-hash-table :test #'eql)
  "Callback states indexed by the address of their foreign user-data token.")

(-> nemo-relay--callback-state-key (t) (option integer))
(defun nemo-relay--callback-state-key (pointer)
  "Return POINTER's hash key, or NIL for a null pointer."
  (and pointer
       (not (cffi:null-pointer-p pointer))
       (cffi:pointer-address pointer)))

(-> nemo-relay--callback-state-create
    (keyword function &key (:validator (option function)))
    (values nemo-relay-callback-state t))
(defun nemo-relay--callback-state-create (kind callback &key validator)
  "Create and retain a callback state for KIND and CALLBACK."
  (unless (functionp callback)
    (nemo-relay--signal-error "Relay callback must be a function."
                         "Relay callback registration"))
  (let* ((token (cffi:foreign-alloc :uint8 :count 1))
         (state (make-instance 'nemo-relay-callback-state
                               :callback callback
                               :validator validator
                               :kind kind
                               :token token)))
    (with-lock-held (*nemo-relay-callback-state-lock*)
      (setf (gethash (nemo-relay--callback-state-key token)
                     *nemo-relay-callback-states*)
            state))
    (values state token)))

(-> nemo-relay--callback-state-for-pointer (t) (option nemo-relay-callback-state))
(defun nemo-relay--callback-state-for-pointer (pointer)
  "Find the callback state associated with foreign POINTER."
  (let ((key (nemo-relay--callback-state-key pointer)))
    (and key
         (with-lock-held (*nemo-relay-callback-state-lock*)
           (gethash key *nemo-relay-callback-states*)))))

(-> nemo-relay--callback-state-release (t) boolean)
(defun nemo-relay--callback-state-release (pointer)
  "Release and forget the callback state associated with POINTER."
  (let* ((key (nemo-relay--callback-state-key pointer))
         (state nil))
    (when key
      (with-lock-held (*nemo-relay-callback-state-lock*)
        (setf state (gethash key *nemo-relay-callback-states*))
        (when state
          (remhash key *nemo-relay-callback-states*)
          (setf (nemo-relay-callback-state-released-p state) t)))
      (when state
        (cffi:foreign-free (nemo-relay-callback-state-token state))))
    t))

(-> nemo-relay--callback-borrowed-string (t) (option string))
(defun nemo-relay--callback-borrowed-string (pointer)
  "Copy a borrowed native string without attempting to free it."
  (when (and pointer (not (cffi:null-pointer-p pointer)))
    (handler-case
        (cffi:foreign-string-to-lisp pointer :encoding ':utf-8)
      (serious-condition () nil))))

(-> nemo-relay--callback-json-input (t string) t)
(defun nemo-relay--callback-json-input (pointer operation)
  "Copy and decode borrowed JSON POINTER for a callback."
  (let ((source (nemo-relay--callback-borrowed-string pointer)))
    (and source
         (handler-case
             (json-decode source)
           (serious-condition (condition)
             (nemo-relay--set-last-error
              (format nil "~A received invalid JSON: ~A"
                      operation (nemo-relay--condition-summary condition)))
             nil)))))

(-> nemo-relay--callback-result-pointer
    (t string &key (:required-kind (option keyword)))
    t)
(defun nemo-relay--callback-result-pointer (value operation &key required-kind)
  "Encode callback VALUE as a Relay-owned JSON string pointer."
  (handler-case
      (let ((json (if (stringp value)
                      value
                      (nemo-relay--safe-json-encode value))))
        (unless json
          (nemo-relay--signal-error
           (format nil "~A callback returned an unencodable value." operation)
           operation))
        (let ((decoded (json-decode json)))
          (when (and required-kind
                     (not (ecase required-kind
                            (:object (json-object-p decoded))
                            (:array (or (vectorp decoded) (listp decoded))))))
            (nemo-relay--signal-error
             (format nil "~A callback must return a JSON ~A."
                     operation required-kind)
             operation)))
        (cffi:foreign-string-alloc json :encoding ':utf-8))
    (nemo-relay-error (condition)
      (nemo-relay--set-last-error
       (format nil "~A: ~A" operation (autolith-error-message condition)))
      (cffi:null-pointer))
    (serious-condition (condition)
      (nemo-relay--set-last-error
       (format nil "~A callback failed: ~A"
               operation (nemo-relay--condition-summary condition)))
      (cffi:null-pointer))))

(-> nemo-relay--callback-failure (string serious-condition) t)
(defun nemo-relay--callback-failure (operation condition)
  "Record a callback CONDITION and return a null foreign pointer."
  (nemo-relay--set-last-error
   (format nil "~A callback failed: ~A"
           operation (nemo-relay--condition-summary condition)))
  (cffi:null-pointer))

(cffi:defcallback nemo-relay--native-callback-free :void ((user-data :pointer))
  (nemo-relay--callback-state-release user-data)
  nil)

(cffi:defcallback nemo-relay--native-event-subscriber-callback
    :void ((user-data :pointer) (event :pointer))
  (let ((state (nemo-relay--callback-state-for-pointer user-data)))
    (when state
      (handler-case
          (funcall (nemo-relay-callback-state-callback state)
                   (make-instance 'nemo-relay-event-view :pointer event))
        (serious-condition (condition)
          (nemo-relay--set-last-error
           (format nil "Relay subscriber callback failed: ~A"
                   (nemo-relay--condition-summary condition)))))))
  nil)

(cffi:defcallback nemo-relay--native-event-metadata-injector-callback
    :pointer ((user-data :pointer) (event :pointer))
  (let ((state (nemo-relay--callback-state-for-pointer user-data)))
    (if state
        (handler-case
            (nemo-relay--callback-result-pointer
             (funcall (nemo-relay-callback-state-callback state)
                      (make-instance 'nemo-relay-event-view :pointer event))
             "Relay event metadata injector"
             :required-kind ':object)
          (serious-condition (condition)
            (nemo-relay--callback-failure
             "Relay event metadata injector" condition)))
        (progn
          (nemo-relay--set-last-error "Relay event metadata callback state is unavailable.")
          (cffi:null-pointer)))))

(cffi:defcallback nemo-relay--native-event-sanitize-callback
    :pointer ((user-data :pointer) (event :pointer) (fields-json :pointer))
  (let ((state (nemo-relay--callback-state-for-pointer user-data)))
    (if state
        (handler-case
            (nemo-relay--callback-result-pointer
             (funcall (nemo-relay-callback-state-callback state)
                      (make-instance 'nemo-relay-event-view :pointer event)
                      (nemo-relay--callback-json-input
                       fields-json "Relay event sanitizer"))
             "Relay event sanitizer"
             :required-kind ':object)
          (serious-condition (condition)
            (nemo-relay--callback-failure "Relay event sanitizer" condition)))
        (progn
          (nemo-relay--set-last-error "Relay event sanitizer callback state is unavailable.")
          (cffi:null-pointer)))))

(cffi:defcallback nemo-relay--native-plugin-validate-callback
    :pointer ((user-data :pointer) (plugin-config-json :pointer))
  (let ((state (nemo-relay--callback-state-for-pointer user-data)))
    (if (and state (nemo-relay-callback-state-validator state))
        (handler-case
            (nemo-relay--callback-result-pointer
             (funcall (nemo-relay-callback-state-validator state)
                      (nemo-relay--callback-json-input
                       plugin-config-json "Relay plugin validator"))
             "Relay plugin validator"
             :required-kind ':array)
          (serious-condition (condition)
            (nemo-relay--callback-failure "Relay plugin validator" condition)))
        (nemo-relay--callback-result-pointer
         (json-array)
         "Relay plugin validator"
         :required-kind ':array))))

(cffi:defcallback nemo-relay--native-plugin-register-callback
    :int32 ((user-data :pointer) (plugin-config-json :pointer) (context :pointer))
  (let ((state (nemo-relay--callback-state-for-pointer user-data)))
    (if state
        (handler-case
            (let ((result
                   (funcall (nemo-relay-callback-state-callback state)
                            (nemo-relay--callback-json-input
                             plugin-config-json "Relay plugin registration")
                            (make-instance 'nemo-relay-plugin-context
                                           :pointer context))))
              (cond
                ((or (null result) (eq result t)) 0)
                ((integerp result) result)
                (t
                 (nemo-relay--set-last-error
                  "Relay plugin registration callback must return T, NIL, or a status integer.")
                 5)))
          (serious-condition (condition)
            (nemo-relay--set-last-error
             (format nil "Relay plugin registration callback failed: ~A"
                     (nemo-relay--condition-summary condition)))
            5))
        (progn
          (nemo-relay--set-last-error "Relay plugin registration callback state is unavailable.")
          5))))


;;;; -- Callback Registration --

(defclass nemo-relay-plugin-context ()
  ((pointer
    :initarg :pointer
    :reader nemo-relay-plugin-context-pointer
    :documentation "The borrowed plugin context pointer."))
  (:documentation
   "A non-owning observability-only plugin context valid during registration."))

(-> nemo-relay--callback-registration-name (t string) string)
(defun nemo-relay--callback-registration-name (name operation)
  "Validate and return a callback registration NAME."
  (unless (and (stringp name) (non-empty-string-p name))
    (nemo-relay--signal-error
     (format nil "~A requires a non-empty registration name." operation)
     operation))
  name)

(-> nemo-relay--callback-priority (t string) integer)
(defun nemo-relay--callback-priority (priority operation)
  "Validate one native signed 32-bit callback PRIORITY."
  (unless (and (integerp priority) (<= -2147483648 priority 2147483647))
    (nemo-relay--signal-error
     (format nil "~A priority must be a signed 32-bit integer." operation)
     operation))
  priority)

(-> nemo-relay-register-subscriber (string function) boolean)
(defun nemo-relay-register-subscriber (name callback)
  "Register CALLBACK as a global Relay event subscriber under NAME.

CALLBACK receives one non-owning RELAY-EVENT-VIEW valid for the callback."
  (let ((operation "nemo_relay_register_subscriber"))
    (nemo-relay--callback-registration-name name operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':subscriber callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list name)
             (lambda (name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-register-subscriber
                   name-pointer
                   (cffi:callback nemo-relay--native-event-subscriber-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay-deregister-subscriber (string) boolean)
(defun nemo-relay-deregister-subscriber (name)
  "Deregister a global Relay event subscriber by NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_deregister_subscriber")
  (nemo-relay--require-status
   "nemo_relay_deregister_subscriber"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-deregister-subscriber name-pointer)))))
  t)

(-> nemo-relay-scope-register-subscriber (t string function) boolean)
(defun nemo-relay-scope-register-subscriber (scope name callback)
  "Register CALLBACK as a scope-local Relay event subscriber under NAME.

CALLBACK receives one non-owning RELAY-EVENT-VIEW valid for the callback."
  (let ((operation "nemo_relay_scope_register_subscriber")
        (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':subscriber callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list scope-uuid name)
             (lambda (scope-pointer name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-scope-register-subscriber
                   scope-pointer name-pointer
                   (cffi:callback nemo-relay--native-event-subscriber-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay-scope-deregister-subscriber (t string) boolean)
(defun nemo-relay-scope-deregister-subscriber (scope name)
  "Deregister a scope-local Relay event subscriber by NAME."
  (let ((operation "nemo_relay_scope_deregister_subscriber")
        (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--require-status
     operation
     (lambda ()
       (nemo-relay--call-with-c-strings
        (list scope-uuid name)
        (lambda (scope-pointer name-pointer)
          (%nemo-relay-scope-deregister-subscriber
           scope-pointer name-pointer)))))
    t))

(-> nemo-relay-flush-subscribers () boolean)
(defun nemo-relay-flush-subscribers ()
  "Wait for queued native subscriber callbacks to complete."
  (nemo-relay--require-status
   "nemo_relay_flush_subscribers"
   #'%nemo-relay-flush-subscribers)
  t)

(-> nemo-relay-register-event-metadata-injector
    (string function &key (:priority integer))
    boolean)
(defun nemo-relay-register-event-metadata-injector (name callback &key (priority 0))
  "Register a global event metadata injector.

CALLBACK receives a RELAY-EVENT-VIEW and returns a JSON object or NIL."
  (let ((operation "nemo_relay_register_event_metadata_injector"))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':metadata-injector callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list name)
             (lambda (name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-register-event-metadata-injector
                   name-pointer priority
                   (cffi:callback nemo-relay--native-event-metadata-injector-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay-deregister-event-metadata-injector (string) boolean)
(defun nemo-relay-deregister-event-metadata-injector (name)
  "Deregister a global event metadata injector by NAME."
  (nemo-relay--callback-registration-name
   name "nemo_relay_deregister_event_metadata_injector")
  (nemo-relay--require-status
   "nemo_relay_deregister_event_metadata_injector"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-deregister-event-metadata-injector name-pointer)))))
  t)

(-> nemo-relay-scope-register-event-metadata-injector
    (t string function &key (:priority integer))
    boolean)
(defun nemo-relay-scope-register-event-metadata-injector
    (scope name callback &key (priority 0))
  "Register an event metadata injector owned by SCOPE."
  (let ((operation "nemo_relay_scope_register_event_metadata_injector")
        (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':metadata-injector callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list scope-uuid name)
             (lambda (scope-pointer name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-scope-register-event-metadata-injector
                   scope-pointer name-pointer priority
                   (cffi:callback nemo-relay--native-event-metadata-injector-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay-scope-deregister-event-metadata-injector (t string) boolean)
(defun nemo-relay-scope-deregister-event-metadata-injector (scope name)
  "Deregister a scope-owned event metadata injector by NAME."
  (let ((operation "nemo_relay_scope_deregister_event_metadata_injector")
        (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--require-status
     operation
     (lambda ()
       (nemo-relay--call-with-c-strings
        (list scope-uuid name)
        (lambda (scope-pointer name-pointer)
          (%nemo-relay-scope-deregister-event-metadata-injector
           scope-pointer name-pointer)))))
    t))

(-> nemo-relay--sanitizer-kind (t) keyword)
(defun nemo-relay--sanitizer-kind (kind)
  "Normalize an event sanitizer KIND to MARK, SCOPE-START, or SCOPE-END."
  (let ((name (nemo-relay--normalized-name kind)))
    (cond
      ((member name '("mark" "mark_sanitize_guardrail") :test #'string=)
       ':mark)
      ((member name '("scope-start" "scope_start" "scope_sanitize_start_guardrail")
                 :test #'string=)
       ':scope-start)
      ((member name '("scope-end" "scope_end" "scope_sanitize_end_guardrail")
                 :test #'string=)
       ':scope-end)
      (t
       (nemo-relay--signal-error
        (format nil "Unknown Relay event sanitizer kind ~S." kind)
        "Relay event sanitizer")))))

(-> nemo-relay--global-sanitizer-functions (keyword) (values function function))
(defun nemo-relay--global-sanitizer-functions (kind)
  "Return the native register and deregister functions for KIND."
  (ecase kind
    (:mark
     (values #'%nemo-relay-register-mark-sanitize-guardrail
             #'%nemo-relay-deregister-mark-sanitize-guardrail))
    (:scope-start
     (values #'%nemo-relay-register-scope-sanitize-start-guardrail
             #'%nemo-relay-deregister-scope-sanitize-start-guardrail))
    (:scope-end
     (values #'%nemo-relay-register-scope-sanitize-end-guardrail
             #'%nemo-relay-deregister-scope-sanitize-end-guardrail))))

(-> nemo-relay--scope-sanitizer-functions (keyword) (values function function))
(defun nemo-relay--scope-sanitizer-functions (kind)
  "Return the native scope-local register and deregister functions for KIND."
  (ecase kind
    (:mark
     (values #'%nemo-relay-scope-register-mark-sanitize-guardrail
             #'%nemo-relay-scope-deregister-mark-sanitize-guardrail))
    (:scope-start
     (values #'%nemo-relay-scope-register-scope-sanitize-start-guardrail
             #'%nemo-relay-scope-deregister-scope-sanitize-start-guardrail))
    (:scope-end
     (values #'%nemo-relay-scope-register-scope-sanitize-end-guardrail
             #'%nemo-relay-scope-deregister-scope-sanitize-end-guardrail))))

(-> nemo-relay-register-event-sanitizer
    (&key (:kind t) (:name string) (:callback function) (:priority integer))
    boolean)
(defun nemo-relay-register-event-sanitizer
    (&key kind name callback (priority 0))
  "Register a global observability event sanitizer.

CALLBACK receives an event view and decoded fields JSON, and returns replacement
fields JSON or NIL."
  (let* ((normalized-kind (nemo-relay--sanitizer-kind kind))
         (operation "Relay event sanitizer registration"))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (register-function deregister-function)
        (nemo-relay--global-sanitizer-functions normalized-kind)
      (declare (ignore deregister-function))
      (multiple-value-bind (state token)
          (nemo-relay--callback-state-create ':event-sanitizer callback)
        (declare (ignore state))
        (handler-case
            (progn
              (nemo-relay--call-with-c-strings
               (list name)
               (lambda (name-pointer)
                 (nemo-relay--require-status
                  operation
                  (lambda ()
                    (funcall register-function
                             name-pointer priority
                             (cffi:callback nemo-relay--native-event-sanitize-callback)
                             token
                             (cffi:callback nemo-relay--native-callback-free))))))
              t)
          (serious-condition (condition)
            (nemo-relay--callback-state-release token)
            (error condition)))))))

(-> nemo-relay-deregister-event-sanitizer (t string) boolean)
(defun nemo-relay-deregister-event-sanitizer (kind name)
  "Deregister a global observability event sanitizer by KIND and NAME."
  (let ((normalized-kind (nemo-relay--sanitizer-kind kind)))
    (nemo-relay--callback-registration-name name
                                       "Relay event sanitizer deregistration")
    (multiple-value-bind (register-function deregister-function)
        (nemo-relay--global-sanitizer-functions normalized-kind)
      (declare (ignore register-function))
      (nemo-relay--require-status
       "Relay event sanitizer deregistration"
       (lambda ()
         (nemo-relay--call-with-c-strings
          (list name)
          (lambda (name-pointer)
            (funcall deregister-function name-pointer))))))
    t))

(-> nemo-relay-scope-register-event-sanitizer
    (&key (:scope t) (:kind t) (:name string) (:callback function)
          (:priority integer))
    boolean)
(defun nemo-relay-scope-register-event-sanitizer
    (&key scope kind name callback (priority 0))
  "Register an observability event sanitizer owned by SCOPE."
  (let* ((normalized-kind (nemo-relay--sanitizer-kind kind))
         (operation "Relay scope event sanitizer registration")
         (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (register-function deregister-function)
        (nemo-relay--scope-sanitizer-functions normalized-kind)
      (declare (ignore deregister-function))
      (multiple-value-bind (state token)
          (nemo-relay--callback-state-create ':event-sanitizer callback)
        (declare (ignore state))
        (handler-case
            (progn
              (nemo-relay--call-with-c-strings
               (list scope-uuid name)
               (lambda (scope-pointer name-pointer)
                 (nemo-relay--require-status
                  operation
                  (lambda ()
                    (funcall register-function
                             scope-pointer name-pointer priority
                             (cffi:callback nemo-relay--native-event-sanitize-callback)
                             token
                             (cffi:callback nemo-relay--native-callback-free))))))
              t)
          (serious-condition (condition)
            (nemo-relay--callback-state-release token)
            (error condition)))))))

(-> nemo-relay-scope-deregister-event-sanitizer (t t string) boolean)
(defun nemo-relay-scope-deregister-event-sanitizer (scope kind name)
  "Deregister a scope-owned event sanitizer by KIND and NAME."
  (let ((normalized-kind (nemo-relay--sanitizer-kind kind))
        (operation "Relay scope event sanitizer deregistration")
        (scope-uuid (nemo-relay-scope-handle-uuid scope)))
    (unless scope-uuid
      (nemo-relay--signal-error "Relay scope has no UUID." operation))
    (nemo-relay--callback-registration-name name operation)
    (multiple-value-bind (register-function deregister-function)
        (nemo-relay--scope-sanitizer-functions normalized-kind)
      (declare (ignore register-function))
      (nemo-relay--require-status
       operation
       (lambda ()
         (nemo-relay--call-with-c-strings
          (list scope-uuid name)
          (lambda (scope-pointer name-pointer)
            (funcall deregister-function scope-pointer name-pointer))))))
    t))

(defun nemo-relay-register-mark-sanitize-guardrail (name callback &key (priority 0))
  "Register a global mark event sanitizer."
  (nemo-relay-register-event-sanitizer :kind ':mark :name name :callback callback
                                  :priority priority))

(defun nemo-relay-deregister-mark-sanitize-guardrail (name)
  "Deregister a global mark event sanitizer."
  (nemo-relay-deregister-event-sanitizer ':mark name))

(defun nemo-relay-register-scope-sanitize-start-guardrail
    (name callback &key (priority 0))
  "Register a global scope-start event sanitizer."
  (nemo-relay-register-event-sanitizer :kind ':scope-start :name name :callback callback
                                  :priority priority))

(defun nemo-relay-deregister-scope-sanitize-start-guardrail (name)
  "Deregister a global scope-start event sanitizer."
  (nemo-relay-deregister-event-sanitizer ':scope-start name))

(defun nemo-relay-register-scope-sanitize-end-guardrail
    (name callback &key (priority 0))
  "Register a global scope-end event sanitizer."
  (nemo-relay-register-event-sanitizer :kind ':scope-end :name name :callback callback
                                  :priority priority))

(defun nemo-relay-deregister-scope-sanitize-end-guardrail (name)
  "Deregister a global scope-end event sanitizer."
  (nemo-relay-deregister-event-sanitizer ':scope-end name))

(defun nemo-relay-scope-register-mark-sanitize-guardrail
    (scope name callback &key (priority 0))
  "Register a scope-owned mark event sanitizer."
  (nemo-relay-scope-register-event-sanitizer
   :scope scope :kind ':mark :name name :callback callback :priority priority))

(defun nemo-relay-scope-deregister-mark-sanitize-guardrail (scope name)
  "Deregister a scope-owned mark event sanitizer."
  (nemo-relay-scope-deregister-event-sanitizer scope ':mark name))

(defun nemo-relay-scope-register-scope-sanitize-start-guardrail
    (scope name callback &key (priority 0))
  "Register a scope-owned scope-start event sanitizer."
  (nemo-relay-scope-register-event-sanitizer
   :scope scope :kind ':scope-start :name name :callback callback :priority priority))

(defun nemo-relay-scope-deregister-scope-sanitize-start-guardrail (scope name)
  "Deregister a scope-owned scope-start event sanitizer."
  (nemo-relay-scope-deregister-event-sanitizer scope ':scope-start name))

(defun nemo-relay-scope-register-scope-sanitize-end-guardrail
    (scope name callback &key (priority 0))
  "Register a scope-owned scope-end event sanitizer."
  (nemo-relay-scope-register-event-sanitizer
   :scope scope :kind ':scope-end :name name :callback callback :priority priority))

(defun nemo-relay-scope-deregister-scope-sanitize-end-guardrail (scope name)
  "Deregister a scope-owned scope-end event sanitizer."
  (nemo-relay-scope-deregister-event-sanitizer scope ':scope-end name))


;;;; -- Observability-Only Plugin Context --

(-> nemo-relay--plugin-context-pointer (t string) t)
(defun nemo-relay--plugin-context-pointer (context operation)
  "Return CONTEXT's borrowed pointer for an observability operation."
  (cond
    ((typep context 'nemo-relay-plugin-context)
     (nemo-relay-plugin-context-pointer context))
    ((cffi:pointerp context) context)
    (t
     (nemo-relay--signal-error
      (format nil "~A requires a Relay plugin context." operation)
      operation))))

(-> nemo-relay-plugin-context-register-subscriber (t string function) boolean)
(defun nemo-relay-plugin-context-register-subscriber (context name callback)
  "Register an observability subscriber through CONTEXT."
  (let ((operation "nemo_relay_plugin_context_register_subscriber")
        (context-pointer (nemo-relay--plugin-context-pointer
                          context "nemo_relay_plugin_context_register_subscriber")))
    (nemo-relay--callback-registration-name name operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':subscriber callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list name)
             (lambda (name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-plugin-context-register-subscriber
                   context-pointer name-pointer
                   (cffi:callback nemo-relay--native-event-subscriber-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay--plugin-context-register-sanitizer
    (t keyword string function integer function) boolean)
(defun nemo-relay--plugin-context-register-sanitizer
    (context kind name callback priority native-function)
  "Register one observability sanitizer through a borrowed plugin CONTEXT."
  (let ((operation "Relay plugin context event sanitizer registration")
        (context-pointer (nemo-relay--plugin-context-pointer
                          context "Relay plugin context event sanitizer registration")))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create kind callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list name)
             (lambda (name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (funcall native-function
                           context-pointer name-pointer priority
                           (cffi:callback nemo-relay--native-event-sanitize-callback)
                           token
                           (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(defun nemo-relay-plugin-context-register-event-metadata-injector
    (context name callback &key (priority 0))
  "Register an event metadata injector through CONTEXT."
  (let ((operation "nemo_relay_plugin_context_register_event_metadata_injector")
        (context-pointer (nemo-relay--plugin-context-pointer
                          context "nemo_relay_plugin_context_register_event_metadata_injector")))
    (nemo-relay--callback-registration-name name operation)
    (nemo-relay--callback-priority priority operation)
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':metadata-injector callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list name)
             (lambda (name-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-plugin-context-register-event-metadata-injector
                   context-pointer name-pointer priority
                   (cffi:callback nemo-relay--native-event-metadata-injector-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(defun nemo-relay-plugin-context-register-mark-sanitize-guardrail
    (context name callback &key (priority 0))
  "Register a mark event sanitizer through CONTEXT."
  (nemo-relay--plugin-context-register-sanitizer
   context ':mark name callback priority
   #'%nemo-relay-plugin-context-register-mark-sanitize-guardrail))

(defun nemo-relay-plugin-context-register-scope-sanitize-start-guardrail
    (context name callback &key (priority 0))
  "Register a scope-start event sanitizer through CONTEXT."
  (nemo-relay--plugin-context-register-sanitizer
   context ':scope-start name callback priority
   #'%nemo-relay-plugin-context-register-scope-sanitize-start-guardrail))

(defun nemo-relay-plugin-context-register-scope-sanitize-end-guardrail
    (context name callback &key (priority 0))
  "Register a scope-end event sanitizer through CONTEXT."
  (nemo-relay--plugin-context-register-sanitizer
   context ':scope-end name callback priority
   #'%nemo-relay-plugin-context-register-scope-sanitize-end-guardrail))

(-> nemo-relay--ensure-custom-plugin-kind (t string) string)
(defun nemo-relay--ensure-custom-plugin-kind (plugin-kind operation)
  "Reject Relay built-in kinds from custom observability registration."
  (nemo-relay--callback-registration-name plugin-kind operation)
  (when (nemo-relay--reserved-plugin-kind-p plugin-kind)
    (nemo-relay--signal-error
     (format nil
             "~A cannot register or deregister reserved Relay plugin kind ~A."
             operation plugin-kind)
     operation))
  plugin-kind)

(-> nemo-relay-register-plugin (string function &key (:validate-callback (option function))) boolean)
(defun nemo-relay-register-plugin (plugin-kind register-callback &key validate-callback)
  "Register a static plugin with observability-only context methods.

REGISTER-CALLBACK receives decoded plugin config and a borrowed
RELAY-PLUGIN-CONTEXT. VALIDATE-CALLBACK, when supplied, receives decoded config
and returns a diagnostics list or JSON array."
  (let ((operation "nemo_relay_register_plugin"))
      (nemo-relay--ensure-custom-plugin-kind plugin-kind operation)
    (unless (functionp register-callback)
      (nemo-relay--signal-error "Relay plugin registration callback must be a function."
                           operation))
    (when validate-callback
      (unless (functionp validate-callback)
        (nemo-relay--signal-error "Relay plugin validation callback must be a function."
                             operation)))
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':plugin register-callback
                                      :validator validate-callback)
      (declare (ignore state))
      (handler-case
          (progn
            (nemo-relay--call-with-c-strings
             (list plugin-kind)
             (lambda (kind-pointer)
               (nemo-relay--require-status
                operation
                (lambda ()
                  (%nemo-relay-register-plugin
                   kind-pointer
                   (if validate-callback
                       (cffi:callback nemo-relay--native-plugin-validate-callback)
                       (cffi:null-pointer))
                   (cffi:callback nemo-relay--native-plugin-register-callback)
                   token
                   (cffi:callback nemo-relay--native-callback-free))))))
            t)
        (serious-condition (condition)
          (nemo-relay--callback-state-release token)
          (error condition))))))

(-> nemo-relay-deregister-plugin (string) boolean)
(defun nemo-relay-deregister-plugin (plugin-kind)
  "Deregister a static plugin by KIND."
    (nemo-relay--ensure-custom-plugin-kind plugin-kind "nemo_relay_deregister_plugin")
  (nemo-relay--require-status
   "nemo_relay_deregister_plugin"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list plugin-kind)
      (lambda (kind-pointer)
        (%nemo-relay-deregister-plugin kind-pointer)))))
  t)


;;;; -- Exporter Handles --

(defclass nemo-relay-atif-exporter (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned Relay ATIF exporter."))

(defclass nemo-relay-atof-exporter (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned Relay ATOF exporter."))

(defclass nemo-relay-otel-subscriber (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned Relay OTLP trace subscriber."))

(defclass nemo-relay-otel-log-subscriber (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned Relay OTLP log subscriber."))

(defclass nemo-relay-otel-metric-subscriber (nemo-relay-native-handle) ()
  (:documentation "An explicitly owned Relay OTLP metric subscriber."))

(-> nemo-relay--required-observability-string (t string) string)
(defun nemo-relay--required-observability-string (value operation)
  "Validate one required non-empty string VALUE."
  (unless (and (stringp value) (non-empty-string-p value))
    (nemo-relay--signal-error
     (format nil "~A requires a non-empty string." operation)
     operation))
  value)

(-> nemo-relay--observability-json (t string keyword) (option string))
(defun nemo-relay--observability-json (value operation kind)
  "Encode optional VALUE as JSON and require KIND when supplied."
  (when value
    (let ((json (nemo-relay--json-argument value)))
      (unless json
        (nemo-relay--signal-error
         (format nil "~A cannot be represented as JSON." operation)
         operation))
      (handler-case
          (let ((decoded (json-decode json)))
            (unless (ecase kind
                      (:object (json-object-p decoded))
                      (:array (or (vectorp decoded) (listp decoded))))
              (nemo-relay--signal-error
               (format nil "~A must be a JSON ~A." operation kind)
               operation))
            json)
        (nemo-relay-error (condition)
          (error condition))
        (serious-condition (condition)
          (nemo-relay--signal-error
           (format nil "~A is not valid JSON: ~A"
                   operation (nemo-relay--condition-summary condition))
           operation))))))

(-> nemo-relay--required-observability-json (string function) t)
(defun nemo-relay--required-observability-json (operation function)
  "Return decoded JSON from a successful native output call."
  (let ((value (nemo-relay--take-json-output operation function)))
    (if (null value)
        (nemo-relay--signal-error
         (format nil "~A returned no JSON." operation)
         operation)
        value)))

(-> nemo-relay--observability-enum (t string list) string)
(defun nemo-relay--observability-enum (value operation allowed)
  "Normalize VALUE to one of the lowercase strings in ALLOWED."
  (let ((name (nemo-relay--normalized-name value)))
    (or (find name allowed :test #'string=)
        (nemo-relay--signal-error
         (format nil "~A does not accept ~S." operation value)
         operation))))

(-> nemo-relay--observability-uint64 (t string integer) integer)
(defun nemo-relay--observability-uint64 (value operation default)
  "Validate an optional nonnegative integer VALUE, using DEFAULT for NIL."
  (if (null value)
      default
      (if (and (integerp value) (<= 0 value))
          value
          (nemo-relay--signal-error
           (format nil "~A must be a nonnegative integer." operation)
           operation))))

(-> nemo-relay--observability-positive-uint64 (t string integer) integer)
(defun nemo-relay--observability-positive-uint64 (value operation default)
  "Validate an optional positive integer VALUE, using DEFAULT for NIL."
  (let ((result (nemo-relay--observability-uint64 value operation default)))
    (if (plusp result)
        result
        (nemo-relay--signal-error
         (format nil "~A must be positive." operation)
         operation))))

(-> nemo-relay--release-owned-handle (t symbol string function) boolean)
(defun nemo-relay--release-owned-handle (handle class operation free-function)
  "Release one wrapped or raw native HANDLE with FREE-FUNCTION."
  (cond
    ((typep handle class)
     (unless (nemo-relay-native-handle-freed-p handle)
       (nemo-relay--ensure-native-library)
       (funcall free-function (nemo-relay-native-handle-pointer handle))
       (setf (nemo-relay-native-handle-freed-p handle) t))
     t)
    ((typep handle 'nemo-relay-native-handle)
     (nemo-relay--signal-error
      (format nil "~A received a handle of the wrong type." operation)
      operation))
    ((cffi:pointerp handle)
     (nemo-relay--ensure-native-library)
     (funcall free-function handle)
     t)
    ((null handle) t)
    (t
     (nemo-relay--signal-error
      (format nil "~A requires an owned Relay handle." operation)
      operation))))

(defun nemo-relay-atif-exporter-create
    (&key session-id agent-name agent-version model-name)
  "Create an ATIF exporter with session and agent metadata."
  (let ((operation "nemo_relay_atif_exporter_create"))
    (nemo-relay--required-observability-string session-id operation)
    (nemo-relay--required-observability-string agent-name operation)
    (nemo-relay--required-observability-string agent-version operation)
    (unless (or (null model-name) (stringp model-name))
      (nemo-relay--signal-error "ATIF model name must be a string or NIL." operation))
    (nemo-relay--call-with-c-strings
     (list session-id agent-name agent-version model-name)
     (lambda (session-pointer agent-pointer version-pointer model-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-atif-exporter operation
        (lambda (slot)
          (%nemo-relay-atif-exporter-create
           session-pointer agent-pointer version-pointer model-pointer slot)))))))

(-> nemo-relay-atif-exporter-register (t string) boolean)
(defun nemo-relay-atif-exporter-register (exporter name)
  "Register an ATIF EXPORTER as a subscriber under NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_atif_exporter_register")
  (nemo-relay--require-status
   "nemo_relay_atif_exporter_register"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-atif-exporter-register
         (nemo-relay--native-pointer exporter "nemo_relay_atif_exporter_register")
         name-pointer)))))
  t)

(-> nemo-relay-atif-exporter-deregister (string) boolean)
(defun nemo-relay-atif-exporter-deregister (name)
  "Deregister an ATIF exporter subscriber by NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_atif_exporter_deregister")
  (nemo-relay--require-status
   "nemo_relay_atif_exporter_deregister"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-atif-exporter-deregister name-pointer)))))
  t)

(-> nemo-relay-atif-exporter-export-json (t) string)
(defun nemo-relay-atif-exporter-export-json (exporter)
  "Export an ATIF EXPORTER as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_atif_exporter_export"
   (lambda (slot)
     (%nemo-relay-atif-exporter-export
      (nemo-relay--native-pointer exporter "nemo_relay_atif_exporter_export")
      slot))))

(-> nemo-relay-atif-exporter-export (t) t)
(defun nemo-relay-atif-exporter-export (exporter)
  "Export an ATIF EXPORTER as decoded JSON."
  (json-decode (nemo-relay-atif-exporter-export-json exporter)))

(-> nemo-relay-atif-exporter-clear (t) boolean)
(defun nemo-relay-atif-exporter-clear (exporter)
  "Clear collected events from an ATIF EXPORTER."
  (nemo-relay--require-status
   "nemo_relay_atif_exporter_clear"
   (lambda ()
     (%nemo-relay-atif-exporter-clear
      (nemo-relay--native-pointer exporter "nemo_relay_atif_exporter_clear"))))
  t)

(-> nemo-relay-atif-exporter-free (t) boolean)
(defun nemo-relay-atif-exporter-free (exporter)
  "Release an ATIF EXPORTER."
  (nemo-relay--release-owned-handle
   exporter 'nemo-relay-atif-exporter "nemo_relay_atif_exporter_free"
   #'%nemo-relay-atif-exporter-free))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-atif-exporter))
  "Release an ATIF exporter handle."
  (nemo-relay-atif-exporter-free handle))

(defun nemo-relay-atof-exporter-create
    (&key output-directory mode filename)
  "Create a filesystem-backed ATOF JSONL exporter."
  (let* ((operation "nemo_relay_atof_exporter_create")
         (mode-name (and mode
                         (nemo-relay--observability-enum
                          mode operation '("append" "overwrite")))))
    (dolist (value (list output-directory filename))
      (unless (or (null value) (stringp value))
        (nemo-relay--signal-error
         (format nil "~A path options must be strings or NIL." operation)
         operation)))
    (nemo-relay--call-with-c-strings
     (list output-directory mode-name filename)
     (lambda (directory-pointer mode-pointer filename-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-atof-exporter operation
        (lambda (slot)
          (%nemo-relay-atof-exporter-create
           directory-pointer mode-pointer filename-pointer slot)))))))

(-> nemo-relay-atof-exporter-create-from-json (t) nemo-relay-atof-exporter)
(defun nemo-relay-atof-exporter-create-from-json (config)
  "Create an ATOF exporter from a JSON object or JSON object value."
  (let ((config-json (nemo-relay--json-object-value
                      config "Relay ATOF exporter config")))
    (unless config-json
      (nemo-relay--signal-error "Relay ATOF exporter config is required."
                           "nemo_relay_atof_exporter_create_from_json"))
    (nemo-relay--call-with-c-strings
     (list config-json)
     (lambda (config-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-atof-exporter
        "nemo_relay_atof_exporter_create_from_json"
        (lambda (slot)
          (%nemo-relay-atof-exporter-create-from-json config-pointer slot)))))))

(-> nemo-relay-atof-exporter-register (t string) boolean)
(defun nemo-relay-atof-exporter-register (exporter name)
  "Register an ATOF EXPORTER globally under NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_atof_exporter_register")
  (nemo-relay--require-status
   "nemo_relay_atof_exporter_register"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-atof-exporter-register
         (nemo-relay--native-pointer exporter "nemo_relay_atof_exporter_register")
         name-pointer)))))
  t)

(-> nemo-relay-atof-exporter-deregister (string) boolean)
(defun nemo-relay-atof-exporter-deregister (name)
  "Deregister an ATOF exporter subscriber by NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_atof_exporter_deregister")
  (nemo-relay--require-status
   "nemo_relay_atof_exporter_deregister"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-atof-exporter-deregister name-pointer)))))
  t)

(-> nemo-relay-atof-exporter-force-flush (t) boolean)
(defun nemo-relay-atof-exporter-force-flush (exporter)
  "Flush queued ATOF events and its configured sink."
  (nemo-relay--require-status
   "nemo_relay_atof_exporter_force_flush"
   (lambda ()
     (%nemo-relay-atof-exporter-force-flush
      (nemo-relay--native-pointer exporter "nemo_relay_atof_exporter_force_flush"))))
  t)

(-> nemo-relay-atof-exporter-shutdown (t) boolean)
(defun nemo-relay-atof-exporter-shutdown (exporter)
  "Flush and shut down an ATOF EXPORTER sink."
  (nemo-relay--require-status
   "nemo_relay_atof_exporter_shutdown"
   (lambda ()
     (%nemo-relay-atof-exporter-shutdown
      (nemo-relay--native-pointer exporter "nemo_relay_atof_exporter_shutdown"))))
  (when (typep exporter 'nemo-relay-atof-exporter)
    (setf (nemo-relay-native-handle-shutdown-p exporter) t))
  t)

(-> nemo-relay-atof-exporter-path (t) (option string))
(defun nemo-relay-atof-exporter-path (exporter)
  "Return an ATOF EXPORTER's file output path, or NIL for stream sinks."
  (nemo-relay--string-output
   "nemo_relay_atof_exporter_path"
   (lambda (slot)
     (%nemo-relay-atof-exporter-path
      (nemo-relay--native-pointer exporter "nemo_relay_atof_exporter_path")
      slot))))

(-> nemo-relay-atof-exporter-pathname (t) (option pathname))
(defun nemo-relay-atof-exporter-pathname (exporter)
  "Return an ATOF EXPORTER's file output path as a pathname."
  (let ((path (nemo-relay-atof-exporter-path exporter)))
    (and path (pathname path))))

(-> nemo-relay-atof-exporter-free (t) boolean)
(defun nemo-relay-atof-exporter-free (exporter)
  "Release an ATOF EXPORTER."
  (nemo-relay--release-owned-handle
   exporter 'nemo-relay-atof-exporter "nemo_relay_atof_exporter_free"
   #'%nemo-relay-atof-exporter-free))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-atof-exporter))
  "Release an ATOF exporter handle."
  (nemo-relay-atof-exporter-free handle))


;;;; -- OpenTelemetry Subscribers --

(-> nemo-relay--otel-type-name (t) string)
(defun nemo-relay--otel-type-name (value)
  "Normalize one OpenTelemetry trace projection type."
  (nemo-relay--observability-enum
   (if (eq value ':gen-ai) "gen_ai" value)
   "Relay OTLP trace type"
   '("full" "gen_ai" "openinference")))

(-> nemo-relay--otel-transport-name (t) (option string))
(defun nemo-relay--otel-transport-name (value)
  "Normalize an optional OpenTelemetry transport name."
  (and value
       (nemo-relay--observability-enum
        value "Relay OTLP transport" '("http_binary" "grpc"))))

(-> nemo-relay--otel-log-severity-name (t) (option string))
(defun nemo-relay--otel-log-severity-name (value)
  "Normalize an optional OTLP log minimum severity."
  (and value
       (nemo-relay--observability-enum
        value "Relay OTLP log severity"
        '("trace" "debug" "info" "warn" "error"))))

(-> nemo-relay--otel-temporality-name (t) (option string))
(defun nemo-relay--otel-temporality-name (value)
  "Normalize an optional OTLP metric temporality."
  (and value
       (nemo-relay--observability-enum
        value "Relay OTLP metric temporality"
        '("cumulative" "delta" "low_memory"))))

(defun nemo-relay-otel-subscriber-create
    (&key type transport endpoint headers header-env resource-attributes
          service-name service-namespace service-version instrumentation-scope
          timeout-millis mark-projection mark-exclude-names attribute-mappings
          promote-metadata-prefixes completed-span-context-ttl-millis)
  "Create an OTLP trace subscriber with optional projection controls."
  (let* ((operation "nemo_relay_otel_subscriber_create")
         (type-name (nemo-relay--otel-type-name type))
         (transport-name (nemo-relay--otel-transport-name transport))
         (endpoint-name (nemo-relay--required-observability-string endpoint operation))
         (headers-json (nemo-relay--observability-json
                        headers "Relay OTLP headers" ':object))
         (header-env-json (nemo-relay--observability-json
                           header-env "Relay OTLP header environment" ':object))
         (resource-json (nemo-relay--observability-json
                         resource-attributes "Relay OTLP resource attributes" ':object))
         (projection-name (and mark-projection
                               (nemo-relay--observability-enum
                                mark-projection "Relay OTLP mark projection"
                                '("inherit" "event" "tool"))))
         (exclude-json (nemo-relay--observability-json
                        mark-exclude-names "Relay OTLP mark exclusions" ':array))
         (mappings-json (nemo-relay--observability-json
                         attribute-mappings "Relay OTLP attribute mappings" ':array))
         (promote-json (nemo-relay--observability-json
                        promote-metadata-prefixes
                        "Relay OTLP metadata promotion prefixes" ':array))
         (ttl-supplied-p (not (null completed-span-context-ttl-millis)))
         (ttl (nemo-relay--observability-positive-uint64
               completed-span-context-ttl-millis
               "Relay OTLP completed span context TTL" 60000))
         (timeout (nemo-relay--observability-uint64
                   timeout-millis "Relay OTLP timeout" 0))
         (projection-p (or projection-name exclude-json mappings-json)))
    (dolist (value (list service-name service-namespace service-version
                         instrumentation-scope))
      (unless (or (null value) (stringp value))
        (nemo-relay--signal-error "Relay OTLP service options must be strings or NIL."
                             operation)))
    (nemo-relay--call-with-c-strings
     (list type-name transport-name endpoint-name headers-json header-env-json
           resource-json service-name service-namespace service-version
           instrumentation-scope projection-name exclude-json mappings-json
           promote-json)
     (lambda (type-pointer transport-pointer endpoint-pointer headers-pointer
              header-env-pointer resource-pointer service-name-pointer
              namespace-pointer version-pointer instrumentation-pointer
              projection-pointer exclude-pointer mappings-pointer promote-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-otel-subscriber operation
        (lambda (slot)
          (cond
            (header-env-json
             (%nemo-relay-otel-subscriber-create-with-projection-options-v4
              type-pointer transport-pointer endpoint-pointer headers-pointer
              header-env-pointer resource-pointer service-name-pointer
              namespace-pointer version-pointer instrumentation-pointer timeout
              projection-pointer exclude-pointer mappings-pointer promote-pointer
              ttl slot))
            ((or promote-json ttl-supplied-p)
             (%nemo-relay-otel-subscriber-create-with-projection-options-v3
              type-pointer transport-pointer endpoint-pointer headers-pointer
              resource-pointer service-name-pointer namespace-pointer
              version-pointer instrumentation-pointer timeout projection-pointer
              exclude-pointer mappings-pointer promote-pointer ttl slot))
            (projection-p
             (%nemo-relay-otel-subscriber-create-with-projection-options
              type-pointer transport-pointer endpoint-pointer headers-pointer
              resource-pointer service-name-pointer namespace-pointer
              version-pointer instrumentation-pointer timeout projection-pointer
              exclude-pointer mappings-pointer slot))
            (t
             (%nemo-relay-otel-subscriber-create
              type-pointer transport-pointer endpoint-pointer headers-pointer
              resource-pointer service-name-pointer namespace-pointer
               version-pointer instrumentation-pointer timeout slot)))))))))

(-> nemo-relay-otel-subscriber-register (t string) boolean)
(defun nemo-relay-otel-subscriber-register (subscriber name)
  "Register an OTLP trace SUBSCRIBER globally under NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_otel_subscriber_register")
  (nemo-relay--require-status
   "nemo_relay_otel_subscriber_register"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-subscriber-register
         (nemo-relay--native-pointer subscriber "nemo_relay_otel_subscriber_register")
         name-pointer)))))
  t)

(-> nemo-relay-otel-subscriber-deregister (string) boolean)
(defun nemo-relay-otel-subscriber-deregister (name)
  "Deregister an OTLP trace subscriber by NAME."
  (nemo-relay--callback-registration-name name "nemo_relay_otel_subscriber_deregister")
  (nemo-relay--require-status
   "nemo_relay_otel_subscriber_deregister"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-subscriber-deregister name-pointer)))))
  t)

(-> nemo-relay-otel-subscriber-force-flush (t) boolean)
(defun nemo-relay-otel-subscriber-force-flush (subscriber)
  "Flush finished spans through an OTLP trace exporter."
  (nemo-relay--require-status
   "nemo_relay_otel_subscriber_force_flush"
   (lambda ()
     (%nemo-relay-otel-subscriber-force-flush
      (nemo-relay--native-pointer subscriber "nemo_relay_otel_subscriber_force_flush"))))
  t)

(-> nemo-relay-otel-subscriber-runtime-diagnostics-json (t) string)
(defun nemo-relay-otel-subscriber-runtime-diagnostics-json (subscriber)
  "Return bounded OTLP trace runtime diagnostics as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_otel_subscriber_runtime_diagnostics_json"
   (lambda (slot)
     (%nemo-relay-otel-subscriber-runtime-diagnostics-json
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_subscriber_runtime_diagnostics_json")
      slot))))

(-> nemo-relay-otel-subscriber-runtime-diagnostics (t) t)
(defun nemo-relay-otel-subscriber-runtime-diagnostics (subscriber)
  "Return bounded OTLP trace runtime diagnostics as decoded JSON."
  (json-decode (nemo-relay-otel-subscriber-runtime-diagnostics-json subscriber)))

(-> nemo-relay-otel-subscriber-shutdown (t) boolean)
(defun nemo-relay-otel-subscriber-shutdown (subscriber)
  "Shut down an OTLP trace subscriber."
  (nemo-relay--require-status
   "nemo_relay_otel_subscriber_shutdown"
   (lambda ()
     (%nemo-relay-otel-subscriber-shutdown
      (nemo-relay--native-pointer subscriber "nemo_relay_otel_subscriber_shutdown"))))
  (when (typep subscriber 'nemo-relay-otel-subscriber)
    (setf (nemo-relay-native-handle-shutdown-p subscriber) t))
  t)

(-> nemo-relay-otel-subscriber-free (t) boolean)
(defun nemo-relay-otel-subscriber-free (subscriber)
  "Release an OTLP trace subscriber."
  (nemo-relay--release-owned-handle
   subscriber 'nemo-relay-otel-subscriber "nemo_relay_otel_subscriber_free"
   #'%nemo-relay-otel-subscriber-free))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-otel-subscriber))
  "Release an OTLP trace subscriber handle."
  (nemo-relay-otel-subscriber-free handle))

(defun nemo-relay-otel-log-subscriber-create
    (&key endpoint transport headers header-env resource-attributes service-name
          service-namespace service-version instrumentation-scope timeout-millis
          minimum-severity max-queue-size max-export-batch-size
          scheduled-delay-millis completed-span-context-ttl-millis)
  "Create an OTLP log subscriber."
  (let* ((operation "nemo_relay_otel_log_subscriber_create")
         (endpoint-name (nemo-relay--required-observability-string endpoint operation))
         (transport-name (nemo-relay--otel-transport-name transport))
         (headers-json (nemo-relay--observability-json
                        headers "Relay OTLP log headers" ':object))
         (header-env-json (nemo-relay--observability-json
                           header-env "Relay OTLP log header environment" ':object))
         (resource-json (nemo-relay--observability-json
                         resource-attributes "Relay OTLP log resource attributes" ':object))
         (severity-name (nemo-relay--otel-log-severity-name minimum-severity))
         (timeout (nemo-relay--observability-uint64
                   timeout-millis "Relay OTLP log timeout" 0))
         (queue-size (nemo-relay--observability-uint64
                      max-queue-size "Relay OTLP log queue size" 0))
         (batch-size (nemo-relay--observability-uint64
                      max-export-batch-size "Relay OTLP log batch size" 0))
         (delay (nemo-relay--observability-uint64
                 scheduled-delay-millis "Relay OTLP log scheduled delay" 0))
         (ttl (nemo-relay--observability-positive-uint64
               completed-span-context-ttl-millis
               "Relay OTLP log completed span context TTL" 60000)))
    (dolist (value (list service-name service-namespace service-version
                         instrumentation-scope))
      (unless (or (null value) (stringp value))
        (nemo-relay--signal-error "Relay OTLP log service options must be strings or NIL."
                             operation)))
    (nemo-relay--call-with-c-strings
     (list transport-name endpoint-name headers-json header-env-json resource-json
           service-name service-namespace service-version instrumentation-scope
           severity-name)
     (lambda (transport-pointer endpoint-pointer headers-pointer header-env-pointer
              resource-pointer service-name-pointer namespace-pointer version-pointer
              instrumentation-pointer severity-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-otel-log-subscriber operation
        (lambda (slot)
          (if header-env-json
              (%nemo-relay-otel-log-subscriber-create-v2
               transport-pointer endpoint-pointer headers-pointer header-env-pointer
               resource-pointer service-name-pointer namespace-pointer version-pointer
               instrumentation-pointer timeout severity-pointer queue-size batch-size
               delay ttl slot)
              (%nemo-relay-otel-log-subscriber-create
               transport-pointer endpoint-pointer headers-pointer resource-pointer
               service-name-pointer namespace-pointer version-pointer instrumentation-pointer
               timeout severity-pointer queue-size batch-size delay ttl slot))))))))

(-> nemo-relay-otel-log-subscriber-register (t string) boolean)
(defun nemo-relay-otel-log-subscriber-register (subscriber name)
  "Register an OTLP log SUBSCRIBER globally under NAME."
  (nemo-relay--callback-registration-name name
                                     "nemo_relay_otel_log_subscriber_register")
  (nemo-relay--require-status
   "nemo_relay_otel_log_subscriber_register"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-log-subscriber-register
         (nemo-relay--native-pointer subscriber
                                "nemo_relay_otel_log_subscriber_register")
         name-pointer)))))
  t)

(-> nemo-relay-otel-log-subscriber-deregister (string) boolean)
(defun nemo-relay-otel-log-subscriber-deregister (name)
  "Deregister an OTLP log subscriber by NAME."
  (nemo-relay--callback-registration-name name
                                     "nemo_relay_otel_log_subscriber_deregister")
  (nemo-relay--require-status
   "nemo_relay_otel_log_subscriber_deregister"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-log-subscriber-deregister name-pointer)))))
  t)

(-> nemo-relay-otel-log-subscriber-force-flush (t) boolean)
(defun nemo-relay-otel-log-subscriber-force-flush (subscriber)
  "Flush queued Relay events and OTLP log batches."
  (nemo-relay--require-status
   "nemo_relay_otel_log_subscriber_force_flush"
   (lambda ()
     (%nemo-relay-otel-log-subscriber-force-flush
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_log_subscriber_force_flush"))))
  t)

(-> nemo-relay-otel-log-subscriber-runtime-diagnostics-json (t) string)
(defun nemo-relay-otel-log-subscriber-runtime-diagnostics-json (subscriber)
  "Return bounded OTLP log runtime diagnostics as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_otel_log_subscriber_runtime_diagnostics_json"
   (lambda (slot)
     (%nemo-relay-otel-log-subscriber-runtime-diagnostics-json
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_log_subscriber_runtime_diagnostics_json")
      slot))))

(-> nemo-relay-otel-log-subscriber-runtime-diagnostics (t) t)
(defun nemo-relay-otel-log-subscriber-runtime-diagnostics (subscriber)
  "Return bounded OTLP log runtime diagnostics as decoded JSON."
  (json-decode (nemo-relay-otel-log-subscriber-runtime-diagnostics-json subscriber)))

(-> nemo-relay-otel-log-subscriber-shutdown (t) boolean)
(defun nemo-relay-otel-log-subscriber-shutdown (subscriber)
  "Shut down an OTLP log subscriber."
  (nemo-relay--require-status
   "nemo_relay_otel_log_subscriber_shutdown"
   (lambda ()
     (%nemo-relay-otel-log-subscriber-shutdown
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_log_subscriber_shutdown"))))
  (when (typep subscriber 'nemo-relay-otel-log-subscriber)
    (setf (nemo-relay-native-handle-shutdown-p subscriber) t))
  t)

(-> nemo-relay-otel-log-subscriber-free (t) boolean)
(defun nemo-relay-otel-log-subscriber-free (subscriber)
  "Release an OTLP log subscriber."
  (nemo-relay--release-owned-handle
   subscriber 'nemo-relay-otel-log-subscriber
   "nemo_relay_otel_log_subscriber_free"
   #'%nemo-relay-otel-log-subscriber-free))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-otel-log-subscriber))
  "Release an OTLP log subscriber handle."
  (nemo-relay-otel-log-subscriber-free handle))

(defun nemo-relay-otel-metric-subscriber-create
    (&key endpoint transport headers header-env resource-attributes service-name
          service-namespace service-version instrumentation-scope timeout-millis
          export-interval-millis temporality max-instruments cardinality-limit)
  "Create an OTLP metric subscriber."
  (let* ((operation "nemo_relay_otel_metric_subscriber_create")
         (endpoint-name (nemo-relay--required-observability-string endpoint operation))
         (transport-name (nemo-relay--otel-transport-name transport))
         (headers-json (nemo-relay--observability-json
                        headers "Relay OTLP metric headers" ':object))
         (header-env-json (nemo-relay--observability-json
                           header-env "Relay OTLP metric header environment" ':object))
         (resource-json (nemo-relay--observability-json
                         resource-attributes
                         "Relay OTLP metric resource attributes" ':object))
         (temporality-name (nemo-relay--otel-temporality-name temporality))
         (timeout (nemo-relay--observability-uint64
                   timeout-millis "Relay OTLP metric timeout" 0))
         (interval (nemo-relay--observability-uint64
                    export-interval-millis "Relay OTLP metric export interval" 0))
         (max-instrument-count (nemo-relay--observability-uint64
                                max-instruments
                                "Relay OTLP metric instrument limit" 0))
         (cardinality (nemo-relay--observability-uint64
                       cardinality-limit
                       "Relay OTLP metric cardinality limit" 0)))
    (dolist (value (list service-name service-namespace service-version
                         instrumentation-scope))
      (unless (or (null value) (stringp value))
        (nemo-relay--signal-error "Relay OTLP metric service options must be strings or NIL."
                             operation)))
    (nemo-relay--call-with-c-strings
     (list transport-name endpoint-name headers-json header-env-json resource-json
           service-name service-namespace service-version instrumentation-scope
           temporality-name)
     (lambda (transport-pointer endpoint-pointer headers-pointer header-env-pointer
              resource-pointer service-name-pointer namespace-pointer version-pointer
              instrumentation-pointer temporality-pointer)
       (nemo-relay--new-native-handle
        'nemo-relay-otel-metric-subscriber operation
        (lambda (slot)
          (if header-env-json
              (%nemo-relay-otel-metric-subscriber-create-v2
               transport-pointer endpoint-pointer headers-pointer header-env-pointer
               resource-pointer service-name-pointer namespace-pointer version-pointer
               instrumentation-pointer timeout interval temporality-pointer
               max-instrument-count cardinality slot)
              (%nemo-relay-otel-metric-subscriber-create
               transport-pointer endpoint-pointer headers-pointer resource-pointer
               service-name-pointer namespace-pointer version-pointer instrumentation-pointer
               timeout interval temporality-pointer max-instrument-count cardinality
               slot))))))))

(-> nemo-relay-otel-metric-subscriber-register (t string) boolean)
(defun nemo-relay-otel-metric-subscriber-register (subscriber name)
  "Register an OTLP metric SUBSCRIBER globally under NAME."
  (nemo-relay--callback-registration-name name
                                     "nemo_relay_otel_metric_subscriber_register")
  (nemo-relay--require-status
   "nemo_relay_otel_metric_subscriber_register"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-metric-subscriber-register
         (nemo-relay--native-pointer subscriber
                                "nemo_relay_otel_metric_subscriber_register")
         name-pointer)))))
  t)

(-> nemo-relay-otel-metric-subscriber-deregister (string) boolean)
(defun nemo-relay-otel-metric-subscriber-deregister (name)
  "Deregister an OTLP metric subscriber by NAME."
  (nemo-relay--callback-registration-name name
                                     "nemo_relay_otel_metric_subscriber_deregister")
  (nemo-relay--require-status
   "nemo_relay_otel_metric_subscriber_deregister"
   (lambda ()
     (nemo-relay--call-with-c-strings
      (list name)
      (lambda (name-pointer)
        (%nemo-relay-otel-metric-subscriber-deregister name-pointer)))))
  t)

(-> nemo-relay-otel-metric-subscriber-force-flush (t) boolean)
(defun nemo-relay-otel-metric-subscriber-force-flush (subscriber)
  "Flush queued Relay events and collect current OTLP metric aggregates."
  (nemo-relay--require-status
   "nemo_relay_otel_metric_subscriber_force_flush"
   (lambda ()
     (%nemo-relay-otel-metric-subscriber-force-flush
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_metric_subscriber_force_flush"))))
  t)

(-> nemo-relay-otel-metric-subscriber-runtime-diagnostics-json (t) string)
(defun nemo-relay-otel-metric-subscriber-runtime-diagnostics-json (subscriber)
  "Return bounded OTLP metric runtime diagnostics as JSON text."
  (nemo-relay--required-string-output
   "nemo_relay_otel_metric_subscriber_runtime_diagnostics_json"
   (lambda (slot)
     (%nemo-relay-otel-metric-subscriber-runtime-diagnostics-json
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_metric_subscriber_runtime_diagnostics_json")
      slot))))

(-> nemo-relay-otel-metric-subscriber-runtime-diagnostics (t) t)
(defun nemo-relay-otel-metric-subscriber-runtime-diagnostics (subscriber)
  "Return bounded OTLP metric runtime diagnostics as decoded JSON."
  (json-decode (nemo-relay-otel-metric-subscriber-runtime-diagnostics-json subscriber)))

(-> nemo-relay-otel-metric-subscriber-shutdown (t) boolean)
(defun nemo-relay-otel-metric-subscriber-shutdown (subscriber)
  "Shut down an OTLP metric subscriber."
  (nemo-relay--require-status
   "nemo_relay_otel_metric_subscriber_shutdown"
   (lambda ()
     (%nemo-relay-otel-metric-subscriber-shutdown
      (nemo-relay--native-pointer subscriber
                             "nemo_relay_otel_metric_subscriber_shutdown"))))
  (when (typep subscriber 'nemo-relay-otel-metric-subscriber)
    (setf (nemo-relay-native-handle-shutdown-p subscriber) t))
  t)

(-> nemo-relay-otel-metric-subscriber-free (t) boolean)
(defun nemo-relay-otel-metric-subscriber-free (subscriber)
  "Release an OTLP metric subscriber."
  (nemo-relay--release-owned-handle
   subscriber 'nemo-relay-otel-metric-subscriber
   "nemo_relay_otel_metric_subscriber_free"
   #'%nemo-relay-otel-metric-subscriber-free))

(defmethod nemo-relay-native-handle-free ((handle nemo-relay-otel-metric-subscriber))
  "Release an OTLP metric subscriber handle."
  (nemo-relay-otel-metric-subscriber-free handle))
