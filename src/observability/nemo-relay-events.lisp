(in-package #:autolith)
;;;; -- Scope Stack Propagation --

(-> nemo-relay-current-propagation-context () (option string))
(defun nemo-relay-current-propagation-context ()
  "Return the context explicitly captured for the current execution."
  *nemo-relay-propagation-context-json*)

(-> nemo-relay--call-with-scope-stack ((option string) function) t)
(defun nemo-relay--call-with-scope-stack (context-json function)
  "Run FUNCTION on an isolated stack with explicit propagation context."
  (if (not (nemo-relay--active-p))
      (funcall function)
      (let ((stack
              (nemo-relay--safe-call
               "nemo_relay_scope_stack_create"
               (lambda ()
                 (if context-json
                     (nemo-relay-scope-stack-create-from-propagation-context
                      context-json)
                     (nemo-relay-scope-stack-create))))))
        (if (null stack)
            (let ((*nemo-relay-instrumentation-suppressed-p* t))
              (funcall function))
            (let ((binding
                    (nemo-relay--safe-call
                     "nemo_relay_scope_stack_capture_thread"
                     #'nemo-relay-scope-stack-capture-thread)))
              (if (null binding)
                  (progn
                    (ignore-errors (nemo-relay-scope-stack-free stack))
                    (let ((*nemo-relay-instrumentation-suppressed-p* t))
                      (funcall function)))
                  (if (not
                       (nemo-relay--safe-call
                        "nemo_relay_scope_stack_set_thread"
                        (lambda ()
                          (nemo-relay-scope-stack-set-thread stack))))
                      (progn
                        (ignore-errors
                          (nemo-relay-scope-stack-restore-thread binding))
                        (ignore-errors (nemo-relay-scope-stack-free stack))
                        (let ((*nemo-relay-instrumentation-suppressed-p* t))
                          (funcall function)))
                      (unwind-protect
                           (funcall function)
                        (ignore-errors
                          (nemo-relay-scope-stack-restore-thread binding))
                        (ignore-errors (nemo-relay-scope-stack-free stack))))))))))


(-> nemo-relay-mark (string &key (:data (option t)) (:metadata (option t))) null)
(defun nemo-relay-mark (name &key data metadata)
  "Emit a non-blocking named lifecycle mark on the current Relay scope."
  (when (nemo-relay--active-p)
    (nemo-relay--safe-call
     "nemo_relay_event"
     (lambda ()
       (nemo-relay-event :name name :data data :metadata metadata))))
  nil)


;;;; -- Agent, Provider, and Tool Lifecycles --

(-> observability-agent-child-p (t) boolean)
(defgeneric observability-agent-child-p (agent)
  (:documentation "Return true when AGENT is a nested task child."))

(defmethod observability-agent-child-p ((agent t))
  "Return false for the primary agent and unknown agent implementations."
  (declare (ignore agent))
  nil)

(-> observability-agent-name (t) string)
(defgeneric observability-agent-name (agent)
  (:documentation "Return the stable observability name for AGENT."))

(defmethod observability-agent-name ((agent t))
  "Return the primary Autolith agent name by default."
  (declare (ignore agent))
  "autolith")

(-> observability-agent-parent-name (t) (option string))
(defgeneric observability-agent-parent-name (agent)
  (:documentation "Return AGENT's observability parent name, if nested."))

(defmethod observability-agent-parent-name ((agent t))
  "Return no parent name for the primary agent and unknown implementations."
  (declare (ignore agent))
  nil)


(-> nemo-relay--call-with-agent
    (&key (:name string) (:input t) (:metadata t) (:function function))
    t)

(defun nemo-relay--call-with-agent (&key name input metadata function)
  "Run FUNCTION as an Agent scope with isolated child propagation."
  (if (not (nemo-relay--active-p))
      (funcall function)
      (nemo-relay--call-with-scope-stack
       *nemo-relay-propagation-context-json*
       (lambda ()
         (let* ((child-p
                  (and (json-object-p metadata)
                       (eq (json-get metadata "child") t)))
                (handle
                  (nemo-relay--safe-call
                   "nemo_relay_push_scope"
                   (lambda ()
                     (nemo-relay-push-scope
                      :name name
                      :scope-type ':agent
                      :input input
                      :metadata metadata)))))
           (if (null handle)
               (let ((*nemo-relay-instrumentation-suppressed-p* t))
                 (funcall function))
               (let ((values nil)
                     (failure nil)
                     (*nemo-relay-propagation-context-json* nil))
                 (when child-p
                   (ignore-errors
                     (nemo-relay-mark
                      "autolith.agent.child.started"
                      :data
                      (json-object
                       "agent_name" (json-get metadata "agent_name")
                       "parent_agent" (json-get metadata "parent_agent")))))
                 (setf *nemo-relay-propagation-context-json*
                       (nemo-relay--safe-call
                        "nemo_relay_capture_propagation_context_json"
                        #'nemo-relay-capture-propagation-context))
                 (unwind-protect
                      (handler-case
                          (setf values (multiple-value-list (funcall function)))
                        (serious-condition (condition)
                          (setf failure condition)
                          (error condition)))
                   (when failure
                     (ignore-errors
                       (nemo-relay-mark
                        "autolith.error"
                        :data (json-object
                               "condition"
                               (string-downcase
                                (symbol-name (type-of failure)))))))
                   (when child-p
                     (ignore-errors
                       (nemo-relay-mark
                        "autolith.agent.child.completed"
                        :data
                        (json-object
                         "agent_name" (json-get metadata "agent_name")
                         "parent_agent" (json-get metadata "parent_agent")
                         "success" (if failure false t)))))
                   (ignore-errors
                     (nemo-relay-mark
                      "autolith.turn.completed"
                      :data (json-object "success" (if failure false t))))
                   (ignore-errors
                     (nemo-relay-pop-scope
                      :handle handle
                      :output (and (null failure)
                                   (json-object "success" t))
                      :metadata (json-object "success" (if failure false t))))
                   (ignore-errors (nemo-relay-scope-handle-free handle)))
                  (values-list values))))))))

(-> nemo-relay--agent-input (t) json-object)
(defun nemo-relay--agent-input (content)
  "Build a non-sensitive Relay input object for a user message."
  (json-object
   "text" (or (and (fboundp 'user-message-input-text)
                    (funcall (symbol-function 'user-message-input-text) content))
               "")
   "image_count"
   (length
    (or (and (fboundp 'user-message-input-image-pathnames)
             (funcall (symbol-function 'user-message-input-image-pathnames)
                      content))
        nil))))


(defmacro with-nemo-relay-agent ((name &key input metadata) &body body)
  "Run BODY in a Relay Agent scope when observability is active."
  `(if (nemo-relay--active-p)
       (nemo-relay--call-with-agent
        :name ,name
        :input ,input
        :metadata ,metadata
        :function (lambda () ,@body))
       (progn ,@body)))

(-> nemo-relay--provider-model ((option model-provider)) (option string))
(defun nemo-relay--provider-model (provider)
  "Return PROVIDER's configured model without making it a required dependency."
  (handler-case
      (and provider
           (configuration-model (provider-configuration provider)))
    (serious-condition () nil)))

(-> nemo-relay--provider-result-json (t) string)
(defun nemo-relay--provider-result-json (result)
  "Serialize a provider RESULT without retaining provider-specific objects."
  (handler-case
      (cond
        ((and result (typep result 'provider-result))
         (json-encode
          (json-object
           "response_id" (provider-result-response-id result)
           "output" (provider-result-output-items result)
           "tool_calls" (provider-result-tool-calls result)
           "usage" (provider-result-usage result))))
        ((json-object-p result)
         (json-encode result))
        ((null result)
         "null")
        (t
         (json-encode
          (json-object "value" (bounded-string result :limit 2000)))))
    (serious-condition ()
      "{\"error\":true}")))

(-> nemo-relay--error-json (serious-condition) string)
(defun nemo-relay--error-json (condition)
  "Serialize a non-secret error response for a Relay lifecycle end event."
  (json-encode
   (json-object
    "error" t
    "condition" (string-downcase (symbol-name (type-of condition))))))

(-> nemo-relay--error-metadata ((option serious-condition)) (option json-object))
(defun nemo-relay--error-metadata (condition)
  "Return non-secret error metadata for a lifecycle end event."
  (and condition
       (let ((metadata
               (json-object
                "success" false
                "condition" (string-downcase (symbol-name (type-of condition)))
                "error.type" (string-downcase (symbol-name (type-of condition)))
                "otel.status_code" "ERROR"
                "otel.status_description"
                (nemo-relay--condition-summary condition))))
         metadata)))

(-> nemo-relay--tool-result-p (t) boolean)
(defun nemo-relay--tool-result-p (result)
  "Return true when RESULT is an available tool-result instance."
  (and (find-class 'tool-result nil)
       (typep result 'tool-result)))

(-> nemo-relay--tool-result-details (t) t)
(defun nemo-relay--tool-result-details (result)
  "Return RESULT's details through the late-loaded tool-result protocol."
  (and (fboundp 'tool-result-details)
       (funcall (symbol-function 'tool-result-details) result)))

(-> nemo-relay--tool-result-metadata (t) (option json-object))
(defun nemo-relay--tool-result-metadata (result)
  "Return error metadata for a failed tool RESULT, or NIL for success."
  (when (and (nemo-relay--tool-result-p result)
             (not (slot-value result 'success-p)))
    (let ((metadata
            (json-object
             "success" false
             "otel.status_code" "ERROR")))
      (let ((error-code (slot-value result 'error-code)))
        (when error-code
          (let ((code (string-downcase (symbol-name error-code))))
            (setf (gethash "error_code" metadata) code
                  (gethash "error.type" metadata) code))))
      (let ((details (nemo-relay--tool-result-details result)))
        (when details
          (setf (gethash "details" metadata) details)
          (when (json-object-p details)
            (multiple-value-bind (exit-code present-p)
                (gethash "process.exit.code" details)
              (when present-p
                (setf (gethash "process.exit.code" metadata) exit-code))))))
      (setf (gethash "otel.status_description" metadata)
            (slot-value result 'content))
      metadata)))

(-> nemo-relay--call-with-llm
    (&key (:name string) (:model (option string)) (:request json-object)
          (:function function))
    t)
(defun nemo-relay--call-with-llm (&key name model request function)
  "Run FUNCTION inside a Relay LLM lifecycle span."
  (if (not (nemo-relay--active-p))
      (funcall function)
      (let ((handle
              (nemo-relay--safe-call
               "nemo_relay_llm_call"
               (lambda ()
                 (let ((native-json
                         (nemo-relay--safe-json-encode
                          (json-object
                           "headers" (json-object)
                           "content" request))))
                   (when native-json
                     (nemo-relay-llm-call
                      :name name
                      :native native-json
                      :model-name model)))))))
        (if (null handle)
            (let ((*nemo-relay-instrumentation-suppressed-p* t))
              (funcall function))
            (let ((values nil)
                  (failure nil))
              (unwind-protect
                   (handler-case
                       (setf values (multiple-value-list (funcall function)))
                     (serious-condition (condition)
                       (setf failure condition)
                       (error condition)))
                (nemo-relay--safe-call
                 "nemo_relay_llm_call_end"
                 (lambda ()
                   (nemo-relay-llm-call-end
                    :handle handle
                    :response (if failure
                                  (nemo-relay--error-json failure)
                                  (nemo-relay--provider-result-json (first values)))
                    :metadata (nemo-relay--error-metadata failure))))
                (ignore-errors (nemo-relay-llm-handle-free handle)))
                (values-list values))))))

(defmacro with-nemo-relay-llm ((name model request) &body body)
  "Run BODY in a Relay LLM scope for one provider request."
  `(if (nemo-relay--active-p)
       (nemo-relay--call-with-llm
        :name ,name
        :model ,model
        :request ,request
        :function (lambda () ,@body))
       (progn ,@body)))

(-> nemo-relay--tool-result-json (t) string)
(defun nemo-relay--tool-result-json (result)
  "Serialize a tool RESULT using Relay's result and annotation contract."
  (handler-case
      (if (and result
               (slot-boundp result 'content))
          (json-encode
           (json-object
            "result" (slot-value result 'content)
            "annotation"
            (json-object
             "success" (if (slot-value result 'success-p) t false)
             "error_code" (slot-value result 'error-code)
             "details" (nemo-relay--tool-result-details result))))
          (json-encode
           (json-object
            "result" ""
            "annotation" (json-object "success" false))))
    (serious-condition ()
      "{\"result\":\"\",\"annotation\":{\"success\":false}}")))

(-> nemo-relay--call-with-tool
    (&key (:name string) (:call-id string) (:arguments t) (:function function))
    t)
(defun nemo-relay--call-with-tool (&key name call-id arguments function)
  "Run FUNCTION inside a Relay Tool lifecycle span on an isolated stack."
  (if (not (nemo-relay--active-p))
      (funcall function)
      (nemo-relay--call-with-scope-stack
       *nemo-relay-propagation-context-json*
       (lambda ()
         (let ((handle
                 (nemo-relay--safe-call
                  "nemo_relay_tool_call"
                  (lambda ()
                    (nemo-relay-tool-call
                     :name name
                     :arguments (if (stringp arguments)
                                    arguments
                                    (or (nemo-relay--safe-json-encode arguments)
                                        "{}"))
                     :call-id call-id)))))
           (if (null handle)
               (let ((*nemo-relay-instrumentation-suppressed-p* t))
                 (funcall function))
               (let ((values nil)
                     (failure nil)
                     (*nemo-relay-propagation-context-json* nil))
                 (setf *nemo-relay-propagation-context-json*
                       (nemo-relay--safe-call
                        "nemo_relay_capture_propagation_context_json"
                        #'nemo-relay-capture-propagation-context))
                 (unwind-protect
                      (handler-case
                          (setf values (multiple-value-list (funcall function)))
                        (serious-condition (condition)
                          (setf failure condition)
                          (error condition)))
                    (nemo-relay--safe-call
                     "nemo_relay_tool_call_end"
                     (lambda ()
                       (nemo-relay-tool-call-end
                        :handle handle
                        :result (if failure
                                    (nemo-relay--error-json failure)
                                    (nemo-relay--tool-result-json (first values)))
                        :metadata
                        (or (and (null failure)
                                 (nemo-relay--tool-result-metadata
                                  (first values)))
                            (nemo-relay--error-metadata failure)))))
                    (ignore-errors (nemo-relay-tool-handle-free handle)))
                   (values-list values))))))))

(defmacro with-nemo-relay-tool ((name call-id arguments) &body body)
  "Run BODY in a Relay Tool scope for one local tool call."
  `(if (nemo-relay--active-p)
       (nemo-relay--call-with-tool
        :name ,name
        :call-id ,call-id
        :arguments ,arguments
        :function (lambda () ,@body))
       (progn ,@body)))
