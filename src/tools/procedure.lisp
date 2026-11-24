(in-package #:autolith)

;;;; -- Procedure Tools --

(defclass procedure-run-tool (tool)
  ()
  (:documentation "A tool for running learned procedures."))

(defclass procedure-define-tool (tool)
  ()
  (:documentation "A tool for defining procedures from tool calls and parameters."))

(defmethod tool-execution-policy ((tool procedure-run-tool))
  "Run procedure calls separately from other tool calls in the provider batch."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-execute ((tool procedure-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run a learned procedure."
  (declare (ignore tool))
  (handler-case
      (let* ((identifier (tool-argument arguments "id" :required t))
             (run        (procedure-run context identifier
                                        :parameters (procedure-parameters-from-json
                                                     (tool-argument arguments "parameters"))
                                        :caller     ':model))
             (report     (procedure-run-report identifier run)))
        (if (eq (getf run :status) ':ok)
            (tool-success report)
            (tool-failure report :code ':procedure-failed)))
    (knowledge-error (condition)
      (error 'tool-error :message   (autolith-error-message condition)
                         :tool-name "procedure.run"))))

(defmethod tool-execute ((tool procedure-define-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Define the procedure described by ARGUMENTS and report its version and steps."
  (declare (ignore tool))
  (handler-case
      (let* ((configuration (tool-context-configuration context))
             (conversation  (tool-context-conversation context))
             (procedure     (procedure-from-json arguments)))
        (multiple-value-bind (identifier version added-p)
            (procedure-define configuration procedure
                              :source-conversation (and conversation
                                                        (conversation-identifier conversation)))
          (tool-success
           (procedure-definition-report
            procedure
            :version version
            :added-p added-p
            :status  (procedure-entry-status
                      (knowledge-base-procedure (knowledge-base-read configuration)
                                                (knowledge--workspace configuration)
                                                identifier))))))
    (knowledge-error (condition)
      (error 'tool-error :message   (autolith-error-message condition)
                         :tool-name "procedure.define"))))
