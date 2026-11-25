(in-package #:autolith)

;;;; -- Procedure Tools --

(defclass procedure-define-tool (tool)
  ()
  (:documentation "A tool for defining procedures."))

(defclass procedure-run-tool (tool)
  ()
  (:documentation "A tool for running procedures."))

(defmethod tool-execution-policy ((tool procedure-run-tool))
  "Return the exclusive execution policy for procedure calls."
  (declare (ignore tool))
  ':exclusive)

(-> procedure-run (tool-context string list) (values string list))
(defun procedure-run (context identifier arguments)
  "Call the procedure IDENTIFIER with the keyword ARGUMENTS.

The tool calls in the procedure are executed in CONTEXT. Return the rendered
result and the evaluation sections. Signal KNOWLEDGE-ERROR if no active
procedure is named IDENTIFIER."
  (let* ((configuration (tool-context-configuration context))
         (entry         (knowledge-base-procedure (knowledge-base-read configuration)
                                                  (knowledge--workspace configuration)
                                                  identifier)))
    (when (or (null entry) (getf entry :retired-p))
      (knowledge--fail ':unknown-procedure identifier "No active procedure is named ~A." identifier))
    (application-operation-install-tool-bindings (tool-context-registry context))
    (let ((*tool-lisp-context* context))
      (multiple-value-bind (result-values output)
          (self-capture-evaluation
           (lambda ()
             (apply (eval (procedure-form (getf entry :source))) arguments)))
        (self-evaluation-result result-values output)))))

(-> procedure--arguments ((option json-object)) list)
(defun procedure--arguments (object)
  "Return the keyword arguments named by the keys of the JSON OBJECT."
  (when object
    (loop for key being the hash-keys of object using (hash-value value)
          append (list (intern (string-upcase key) '#:keyword) value))))

(defmethod tool-execute ((tool procedure-define-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Define a procedure of the workspace."
  (declare (ignore tool))
  (let ((identifier   (tool-argument arguments "id" :required t))
        (conversation (tool-context-conversation context)))
    (handler-case
        (procedure-define (tool-context-configuration context) identifier
                          (tool-argument arguments "source" :required t)
                          :source-conversation (and conversation
                                                    (conversation-identifier conversation)))
      (knowledge-error (condition)
        (error 'tool-error :message   (autolith-error-message condition)
                           :tool-name "procedure.define")))
    (tool-success (format nil "Procedure ~A is defined. Run it with procedure.run." identifier))))

(defmethod tool-execute ((tool procedure-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run a procedure of the workspace."
  (declare (ignore tool))
  (handler-case
      (multiple-value-bind (text evaluation)
          (procedure-run context (tool-argument arguments "id" :required t)
                         (procedure--arguments (tool-argument arguments "arguments")))
        (tool-success text :details (list :evaluation evaluation)))
    (knowledge-error (condition)
      (error 'tool-error :message   (autolith-error-message condition)
                         :tool-name "procedure.run"))))
