(in-package #:autolith)

;;;; -- Procedure Tool --

(defclass procedure-run-tool (tool)
  ()
  (:documentation "A tool making a learned procedure's tool calls in one step."))

(defmethod tool-execution-policy ((tool procedure-run-tool))
  "Run a procedure's calls apart from the rest of one provider batch."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-execute ((tool procedure-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run a learned procedure and report each call it made, or where it stopped."
  (declare (ignore tool))
  (handler-case
      (let* ((identifier (tool-argument arguments "id" :required t))
             (run (procedure-run context identifier
                                 :parameters (procedure-parameters-from-json
                                              (tool-argument arguments "parameters"))
                                 :caller ':model))
             (report (procedure-run-report identifier run)))
        (if (eq (getf run :status) ':ok)
            (tool-success report)
            (tool-failure report :code ':procedure-failed)))
    (knowledge-error (condition)
      (error 'tool-error :message (autolith-error-message condition)
                         :tool-name "procedure.run"))))
