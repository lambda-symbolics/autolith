(in-package #:autolith)

;;;; -- Plan Tool Classes --

(defclass plan-tool (tool)
  ()
  (:documentation "A tool reading or updating the current workspace plan."))

(defclass plan-list-tool (plan-tool)
  ()
  (:documentation "A tool reading the current workspace plan."))

(defclass plan-update-tool (plan-tool)
  ()
  (:documentation "A tool replacing the current workspace plan."))

(defclass plan-expand-tool (plan-tool)
  ()
  (:documentation "A tool replacing the plan with a task expanded by its methods."))

(defmethod tool-execution-policy ((tool plan-expand-tool))
  "Serialize plan replacement against the rest of one provider batch."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-storm-guard-exempt-p ((tool plan-list-tool))
  "Exempt current-plan inspection from the mutating-call storm guard."
  t)

(defmethod tool-execution-policy ((tool plan-update-tool))
  "Serialize plan replacement against the rest of one provider batch."
  (declare (ignore tool))
  ':exclusive)


;;;; -- Tool Executions --

(-> plan-tool--structured-p ((option workspace-plan)) boolean)
(defun plan-tool--structured-p (plan)
  "Return true when PLAN has a step with requirements or a verify pattern."
  (and plan
       (some (lambda (step) (or (plan-step-requires step) (plan-step-verify step)))
             (workspace-plan-steps plan))
       t))

(defmethod tool-execute ((tool plan-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read the current workspace plan."
  (declare (ignore tool arguments))
  (handler-case
      (let* ((configuration (tool-context-configuration context))
             (plan (plan-load configuration))
             (view (and (plan-tool--structured-p plan) (knowledge-view-current configuration))))
        (plan-context-note-delivered configuration (tool-context-conversation context) plan view)
        (tool-success (plan-render plan :view view)))
    (plan-error (condition)
      (error 'tool-error
             :message (format nil "~A" condition)
             :tool-name "plan.list"))))

(-> plan-tool--patterns (t function) list)
(defun plan-tool--patterns (value schema-function)
  "Return the fact patterns in JSON array VALUE, read by their predicates' schemas."
  (unless (vectorp value)
    (error 'tool-error :message "requires must be an array of fact patterns."
                       :tool-name "plan.update"))
  (map 'list (lambda (pattern)
               (knowledge--pattern-from-input pattern schema-function :negatable t))
       value))

(-> plan-tool--step (t (option function)) list)
(defun plan-tool--step (item schema-function)
  "Return the step plist for JSON ITEM, reading its patterns with SCHEMA-FUNCTION."
  (unless (json-object-p item)
    (error 'tool-error :message "Each plan step must be an object." :tool-name "plan.update"))
  (let ((requires (json-get item "requires"))
        (verify (json-get item "verify")))
    (append (list :text (or (json-get item "step") (json-get item "text"))
                  :status (json-get item "status"))
            (when (json-get item "id")
              (list :id (json-get item "id")))
            (when requires
              (list :requires (plan-tool--patterns requires schema-function)))
            (when verify
              (list :verify (knowledge--pattern-from-input verify schema-function
                                                           :negatable t))))))

(-> plan-tool--receipts (list (option workspace-plan) (option knowledge-view)) list)
(defun plan-tool--receipts (steps previous view)
  "Return STEPS with a receipt on each structured step newly marked done.

A step keeps the receipt it had under the same id; a step done now records the
bindings and facts satisfying its requirements at this moment, so a later
change to those facts can be reported without repeating the step."
  (mapcar
   (lambda (step)
     (let* ((identifier (getf step :id))
            (old (and identifier previous
                      (find identifier (workspace-plan-steps previous)
                            :key #'plan-step-identifier :test #'equal))))
       (cond
         ((not (and (eq (plan--status (getf step :status)) ':done)
                    (or (getf step :requires) (getf step :verify))
                    view))
          step)
         ((and old (eq (plan-step-status old) ':done) (plan-step-receipt old))
          (append step (list :receipt (plan-step-receipt old))))
         (t
          (let ((solution (first (knowledge-view-solve view (getf step :requires) :limit 1))))
            (append step
                    (list :receipt (list :time (knowledge-now)
                                         :bindings (first solution)
                                         :facts (mapcar #'knowledge-node-identifier
                                                        (rest solution))
                                         :texts (mapcar #'knowledge-node-text
                                                        (rest solution))))))))))
   steps))

(defmethod tool-execute ((tool plan-update-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Replace the current workspace plan, checking steps that depend on facts."
  (declare (ignore tool))
  (handler-case
      (let* ((configuration (tool-context-configuration context))
             (raw-steps (gethash "steps" arguments))
             (explanation (gethash "explanation" arguments))
             (base (knowledge-base-read configuration))
             (schema-function
               (lambda (predicate)
                 (knowledge-base-schema base predicate
                                        :workspace (knowledge--workspace configuration))))
             (steps
               (if (vectorp raw-steps)
                   (map 'list (lambda (item) (plan-tool--step item schema-function)) raw-steps)
                   (error 'tool-error
                          :message "plan.update requires a steps array."
                          :tool-name "plan.update")))
             (view (and (some (lambda (step) (or (getf step :requires) (getf step :verify)))
                              steps)
                        (knowledge-view-current configuration)))
             (plan
               (if steps
                   (plan-update configuration
                                (plan-tool--receipts steps (plan-load configuration) view)
                                :explanation (and (stringp explanation) explanation))
                   (progn
                     (plan-clear configuration)
                     nil))))
        (plan-context-note-delivered configuration (tool-context-conversation context) plan view)
        (tool-success (plan-render plan :view view)))
    (plan-error (condition)
      (error 'tool-error
             :message (format nil "~A" condition)
             :tool-name "plan.update"))
    (knowledge-error (condition)
      (error 'tool-error
             :message (autolith-error-message condition)
             :tool-name "plan.update"))))

(defmethod tool-execute ((tool plan-expand-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Expand a task with its methods into a checked plan, without a model."
  (declare (ignore tool))
  (handler-case
      (let* ((configuration (tool-context-configuration context))
             (task (knowledge--task-from-input (tool-argument arguments "task" :required t)))
             (view (knowledge-view-current configuration)))
        (when (some #'knowledge-variable-p (rest task))
          (error 'tool-error :message "Give the task's arguments as values, not variables."
                             :tool-name "plan.expand"))
        (multiple-value-bind (steps outcome details) (knowledge-expand-task view task)
          (ecase outcome
            (:planned
             (let ((plan (plan-update configuration steps
                                      :explanation (format nil "Expanded ~A by its methods."
                                                           (knowledge-pattern-text task)))))
               (plan-context-note-delivered configuration (tool-context-conversation context)
                                            plan view)
               (tool-success (plan-render plan :view view))))
            (:unknown-task
             (tool-success
              (format nil "No method describes ~{~A~^, ~}; the plan is unchanged. Define one with method-define on memory:facts, or write the plan with plan.update."
                      (getf details :tasks))))
            (:no-plan
             (tool-success
              (format nil "No method for ~A applies to the current facts; the plan is unchanged.~{~%~A~}"
                      (knowledge-pattern-text task)
                      (mapcar (lambda (entry)
                                (format nil "method ~A needs ~{~A~^ and ~}"
                                        (getf entry :method) (getf entry :conditions)))
                              (getf details :unmet)))))
            (:exhausted
             (tool-success
              (format nil "Expanding ~A tried ~D tasks without finishing; the plan is unchanged. The methods may recurse or branch too widely."
                      (knowledge-pattern-text task) (getf details :nodes)))))))
    (knowledge-error (condition)
      (error 'tool-error :message (autolith-error-message condition) :tool-name "plan.expand"))
    (plan-error (condition)
      (error 'tool-error :message (format nil "~A" condition) :tool-name "plan.expand"))))
