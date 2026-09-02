(in-package #:autolith)

;;;; -- Relay State and Conditions --

(define-condition nemo-relay-error (autolith-error)
  ((operation
    :initarg :operation
    :reader nemo-relay-error-operation
    :type string
    :documentation "The Relay operation that failed.")
   (status
    :initarg :status
    :initform nil
    :reader nemo-relay-error-status
    :type (option integer)
    :documentation "The numeric Relay status, when the failure came from FFI.")
   (cause
    :initarg :cause
    :initform nil
    :reader nemo-relay-error-cause
    :type (option serious-condition)
    :documentation "The underlying condition, when Relay raised one."))
  (:documentation "A failure reported by the optional NeMo Relay integration."))

;;;; -- Observability Boundary --

(defparameter *nemo-relay-observability-plugin-kind* "observability"
  "The pinned built-in Relay plugin kind for observability.")

(defparameter *nemo-relay-non-observability-plugin-kinds*
  '("nemo_guardrails" "pricing")
  "Pinned Relay built-in plugin kinds outside the observability boundary.

Relay's dynamic manifest capabilities describe the execution lane and optional
schema support, not the behavior a plugin implements. Custom plugin approval
therefore remains an explicit trust decision rather than a classification claim.")

(-> nemo-relay--non-observability-plugin-kind-p (string) boolean)
(defun nemo-relay--non-observability-plugin-kind-p (kind)
  "Return true when KIND names a pinned non-observability built-in."
  (not (null
        (find kind *nemo-relay-non-observability-plugin-kinds*
              :test #'string-equal))))

(-> nemo-relay--reserved-plugin-kind-p (string) boolean)
(defun nemo-relay--reserved-plugin-kind-p (kind)
  "Return true when KIND names any pinned built-in Relay plugin."
  (or (string-equal kind *nemo-relay-observability-plugin-kind*)
      (nemo-relay--non-observability-plugin-kind-p kind)))

(-> nemo-relay--custom-observability-kind-authorized-p (string list) boolean)
(defun nemo-relay--custom-observability-kind-authorized-p
    (kind allowed-component-kinds)
  "Return true when KIND is the built-in or an explicitly trusted custom kind.

Known non-observability built-ins are never authorized by the custom allowlist."
  (and (stringp kind)
       (not (nemo-relay--non-observability-plugin-kind-p kind))
       (or (string-equal kind *nemo-relay-observability-plugin-kind*)
           (not (null (member kind allowed-component-kinds :test #'string=))))))

(defvar *nemo-relay-runtime* nil
  "The active Relay runtime, or NIL when Relay is disabled or unavailable.")

(defvar *nemo-relay-configuration* nil
  "The explicitly configured Relay settings, or NIL for environment discovery.")

(defvar *nemo-relay-observability-library-loaded-p* nil
  "Whether the direct Relay observability foreign library has been loaded.")

(defvar *nemo-relay-runtime-lock*
  (make-lock "Autolith Relay runtime")
  "The lock protecting Relay runtime startup and shutdown.")

(defvar *nemo-relay-diagnostic-lock*
  (make-lock "Autolith Relay diagnostics")
  "The lock protecting the process-wide latest Relay diagnostic.")

(defvar *nemo-relay-last-error* nil
  "The most recent non-fatal Relay integration diagnostic.")

(defvar *nemo-relay-propagation-context-json* nil
  "The propagation context captured for the current agent or tool execution.")

(defvar *nemo-relay-instrumentation-suppressed-p* nil
  "Whether Relay calls are suppressed for a failed scope-stack setup.")

;;;; -- Foreign Values --

(-> nemo-relay--pointer-present-p (t) boolean)
(defun nemo-relay--pointer-present-p (pointer)
  "Return true when POINTER is a non-null foreign pointer."
  (and pointer (not (cffi:null-pointer-p pointer))))

(-> nemo-relay--make-output-slot () t)
(defun nemo-relay--make-output-slot ()
  "Allocate and clear one foreign pointer output slot."
  (let ((slot (cffi:foreign-alloc :pointer)))
    (setf (cffi:mem-ref slot :pointer) (cffi:null-pointer))
    slot))

(-> nemo-relay--output-slot-value (t) t)
(defun nemo-relay--output-slot-value (slot)
  "Read one foreign pointer output slot."
  (cffi:mem-ref slot :pointer))

(-> nemo-relay--call-with-c-strings (list function) t)
(defun nemo-relay--call-with-c-strings (strings function)
  "Call FUNCTION with borrowed C string pointers for STRINGS.

NIL values become null pointers. All allocated input strings are freed after
FUNCTION returns, including when it signals a condition."
  (let ((pointers nil))
    (unwind-protect
         (progn
           (dolist (string strings)
             (push (if string
                       (cffi:foreign-string-alloc string :encoding ':utf-8)
                       (cffi:null-pointer))
                   pointers))
           (apply function (reverse pointers)))
      (dolist (pointer pointers)
        (when (nemo-relay--pointer-present-p pointer)
          (cffi:foreign-string-free pointer))))))

(-> nemo-relay--returned-string (t) (option string))
(defun nemo-relay--returned-string (pointer)
  "Copy and free a Relay-owned C string POINTER."
  (when (nemo-relay--pointer-present-p pointer)
    (unwind-protect
         (cffi:foreign-string-to-lisp pointer :encoding ':utf-8)
      (%nemo-relay-string-free pointer))))

(-> nemo-relay--ffi-error-message () string)
(defun nemo-relay--ffi-error-message ()
  "Read Relay's thread-local error without retaining its foreign pointer."
  (or (handler-case (%nemo-relay-last-error)
        (serious-condition () nil))
      "unknown Relay error"))

(-> nemo-relay--condition-summary (serious-condition) string)
(defun nemo-relay--condition-summary (condition)
  "Return a bounded, non-secret description of CONDITION."
  (let ((message
          (when (typep condition 'autolith-error)
            (handler-case
                (autolith-error-message condition)
              (serious-condition ()
                nil)))))
    (if (and (stringp message) (non-empty-string-p message))
        (subseq message 0 (min (length message) 512))
        "Autolith operation failed; inspect the terminal or conversation for details.")))

(-> nemo-relay--set-native-last-error (string) null)
(defun nemo-relay--set-native-last-error (message)
  "Copy MESSAGE into Relay's native thread-local diagnostic when available."
  (when *nemo-relay-observability-library-loaded-p*
    (handler-case
        (nemo-relay--call-with-c-strings
         (list message)
         (lambda (message-pointer)
           (%nemo-relay-set-last-error-message message-pointer)))
      (serious-condition () nil)))
  nil)

(-> nemo-relay--set-last-error (string) string)
(defun nemo-relay--set-last-error (message)
  "Record MESSAGE in Autolith and Relay's latest thread-local diagnostics."
  (with-lock-held (*nemo-relay-diagnostic-lock*)
    (setf *nemo-relay-last-error* message))
  (nemo-relay--set-native-last-error message)
  message)

(-> nemo-relay--clear-last-error () null)
(defun nemo-relay--clear-last-error ()
  "Clear the latest process-wide Relay diagnostic."
  (with-lock-held (*nemo-relay-diagnostic-lock*)
    (setf *nemo-relay-last-error* nil))
  nil)

(-> nemo-relay-last-error () (option string))
(defun nemo-relay-last-error ()
  "Return the latest non-fatal Relay integration diagnostic."
  (with-lock-held (*nemo-relay-diagnostic-lock*)
    *nemo-relay-last-error*))

;;;; -- Status and Serialization --

(-> nemo-relay--ffi-status (string function) boolean)
(defun nemo-relay--ffi-status (operation function)
  "Run status-returning FUNCTION and retain a thread-safe diagnostic on failure."
  (handler-case
      (let ((status (funcall function)))
        (if (zerop status)
            t
            (progn
              (nemo-relay--set-last-error
               (format nil "~A failed with Relay status ~D: ~A"
                       operation status (nemo-relay--ffi-error-message)))
              nil)))
    (serious-condition (condition)
      (nemo-relay--set-last-error
       (format nil "~A raised ~A: ~A"
               operation
               (type-of condition)
               (nemo-relay--condition-summary condition)))
      nil)))

(-> nemo-relay--safe-call (string function) t)
(defun nemo-relay--safe-call (operation function)
  "Call Relay setup FUNCTION without allowing it to affect Autolith."
  (handler-case
      (funcall function)
    (serious-condition (condition)
      (nemo-relay--set-last-error
       (format nil "~A raised ~A: ~A"
               operation
               (type-of condition)
               (nemo-relay--condition-summary condition)))
      nil)))

(-> nemo-relay--safe-json-encode (t) (option string))
(defun nemo-relay--safe-json-encode (value)
  "Encode VALUE as JSON, returning NIL when it cannot be represented safely."
  (handler-case
      (json-encode value)
    (serious-condition ()
      nil)))

(-> nemo-relay--optional-json (t) (option string))
(defun nemo-relay--optional-json (value)
  "Encode VALUE as optional JSON, preserving NIL as an omitted payload."
  (and value (nemo-relay--safe-json-encode value)))

(-> nemo-relay--json-argument (t) (option string))
(defun nemo-relay--json-argument (value)
  "Return VALUE as JSON text, accepting either a JSON value or JSON text."
  (and value
       (if (stringp value)
           value
           (nemo-relay--safe-json-encode value))))

(-> nemo-relay--take-string-output (string function)
    (values (option string) boolean))
(defun nemo-relay--take-string-output (operation function)
  "Call FUNCTION with an output slot and return its string and status."
  (let ((slot (nemo-relay--make-output-slot)))
    (unwind-protect
         (let ((status
                 (nemo-relay--ffi-status operation
                                    (lambda () (funcall function slot)))))
           (values (and status
                        (nemo-relay--returned-string (nemo-relay--output-slot-value slot)))
                   status))
      (cffi:foreign-free slot))))

(-> nemo-relay--take-json-output (string function) t)
(defun nemo-relay--take-json-output (operation function)
  "Call FUNCTION and decode its Relay-owned JSON result."
  (let ((source (nemo-relay--take-string-output operation function)))
    (and source
         (handler-case
             (json-decode source)
           (serious-condition (condition)
             (nemo-relay--set-last-error
              (format nil "~A returned invalid JSON: ~A"
                      operation
                      (nemo-relay--condition-summary condition)))
             nil)))))

(-> nemo-relay--call-with-timestamp ((option integer) function) t)
(defun nemo-relay--call-with-timestamp (timestamp function)
  "Call FUNCTION with a pointer to TIMESTAMP or a null timestamp pointer."
  (if timestamp
      (cffi:with-foreign-object (slot :int64)
        (setf (cffi:mem-ref slot :int64) timestamp)
        (funcall function slot))
      (funcall function (cffi:null-pointer))))

(-> nemo-relay--call-with-severity ((option integer) function) t)
(defun nemo-relay--call-with-severity (severity function)
  "Call FUNCTION with a pointer to SEVERITY or a null severity pointer."
  (if severity
      (cffi:with-foreign-object (slot :int32)
        (setf (cffi:mem-ref slot :int32) severity)
        (funcall function slot))
      (funcall function (cffi:null-pointer))))

(-> nemo-relay--load-library ((option string)) t)
(defun nemo-relay--load-library (library-path)
  "Load the configured Relay shared library through CFFI."
  (if library-path
      (cffi:load-foreign-library library-path)
      (cffi:use-foreign-library nemo-relay-ffi)))
