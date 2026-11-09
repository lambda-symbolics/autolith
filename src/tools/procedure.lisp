(in-package #:autolith)

;;;; -- Procedure Tool --

(defclass procedure-run-tool (tool)
  ()
  (:documentation "A tool for running learned procedures."))

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
