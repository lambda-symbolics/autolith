(in-package #:autolith)

(define-condition acp-extension-unavailable
    (application-client-unavailable)
    nil
  (:documentation "An ACP frontend operation is unavailable."))

(-> acp-extension-unavailable-reason (acp-extension-unavailable) string)
(defun acp-extension-unavailable-reason (condition)
  "Return the shared reason for an unavailable frontend operation."
  (application-client-unavailable-reason condition))

(-> acp-extension-value (t) t)
(defun acp-extension-value (value)
  "Adapt shared portable-value rejection to the ACP frontend condition."
  (handler-case (application-client-value value)
                (application-client-unavailable (condition)
                 (error 'acp-extension-unavailable :reason
                        (application-client-unavailable-reason condition)))))

(defparameter *acp-extension-methods*
  (append *application-client-methods*
          '(("_autolith/steer" acp-extension-steer :client)
            ("_autolith/queue" acp-extension-queue :client)))
  "Shared server methods and ACP-local input handlers.")

(-> acp-extension-result (keyword &key (:value t) (:report (option string))
                                     (:condition-type (option string))) hash-table)
(defun acp-extension-result (outcome &key value report condition-type)
  "Construct one typed outcome, retaining no condition object or executable form."
  (agentcomms:json-object
   "outcome" (string-downcase (symbol-name outcome))
   "value" (when (eq outcome ':ok) (acp-extension-value value))
   "report" (when report (subseq report 0 (min 2000 (length report))))
   "conditionType" condition-type))

(-> acp-extension-capabilities () hash-table)
(defun acp-extension-capabilities ()
  "Describe only the installed typed extension handlers."
  (agentcomms:json-object
   "schemaVersion" 1
   "methods" (coerce (cons "_autolith/capabilities"
                            (loop for (name handler) in *acp-extension-methods*
                                  when (fboundp handler) collect name))
                     'vector)
   "features" (agentcomms:json-object
                "events" t "sessionScoped" t
                "turnEvents" t
                "resourceSchemes" (vector "workspace" "shell-log")
                "automaticContinuation" t)))
