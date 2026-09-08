(in-package #:autolith)

;;;; -- Trace-Driven Policy Distillation --

(defparameter *rlm-distill-maximum-traces* 8
  "The most traces one distillation review may read.")

(defparameter *rlm-distill-trace-characters* 20000
  "The most characters of one trace supplied to distillation frames.")

(defparameter *rlm-distill-planner-attempts* 2
  "The most planner frames one distillation runs before giving up.")

(defparameter *rlm-distill-gate-task*
  "Decide whether the attached inference traces contain evidence worth promoting into a reusable decomposition policy. Approve only a repeated, successful, generalizable decomposition pattern grounded in the traces; reject one-off tactics, unsupported hypotheses, and noise. Prefer rejecting: an empty review is cheaper than a speculative policy."
  "The gate frame task deciding whether distillation should run.")

(defparameter *rlm-distill-gate-contract*
  '(:type :object
    :properties (("worth" (:type :boolean))
                 ("rationale" (:type :string)))
    :required ("worth" "rationale"))
  "The answer shape of the distillation gate frame.")

(defparameter *rlm-distill-plan-task*
  "Propose exactly one new decomposition policy distilled from the attached inference traces. Answer with the policy name (a fresh keyword name without the leading colon, never direct), a short title, an evidence-backed rationale citing the traces, and method-source holding exactly one well-formed method definition of this shape:
(defmethod rlm-decompose-inference-task ((policy (eql ':name)) (task string) (views list) (budget rlm-budget)) ...)
The body returns subtask plists (:task ... :context ...) for tasks matching the distilled pattern, or NIL to decline so the task runs directly. Keep the body a pure function of its arguments with no side effects."
  "The planner frame task proposing one distilled policy method.")

(defparameter *rlm-distill-plan-contract*
  '(:type :object
    :properties (("policy" (:type :string))
                 ("title" (:type :string))
                 ("rationale" (:type :string))
                 ("method-source" (:type :string)))
    :required ("policy" "title" "rationale" "method-source"))
  "The answer shape of the distillation planner frame.")

(-> rlm-distill--object-field (t string) t)
(defun rlm-distill--object-field (value name)
  "Return field NAME of the portable tagged object VALUE, or NIL."
  (and (listp value)
       (eq (first value) ':object)
       (second (assoc name (rest value) :test #'string=))))

(-> rlm-distill--trace-view (configuration string) list)
(defun rlm-distill--trace-view (configuration identifier)
  "Return one bounded labeled view of the trace named IDENTIFIER."
  (let ((content (rlm--trace-content configuration identifier)))
    (unless content
      (error 'rlm-inference-error
             :message (format nil "No inference trace is named ~A."
                              identifier)))
    (list ':label (format nil "trace ~A" identifier)
          ':content (rlm--bounded-excerpt content
                                          *rlm-distill-trace-characters*))))

(-> rlm-distill--policy-keyword (t) keyword)
(defun rlm-distill--policy-keyword (name)
  "Return the fresh policy keyword NAME denotes, refusing the base policy."
  (unless (and (stringp name) (non-empty-string-p name))
    (error 'rlm-inference-error
           :message "A distilled policy requires a non-empty name."))
  (let ((keyword (intern (string-upcase (string-left-trim ":" name))
                         '#:keyword)))
    (when (eq keyword ':direct)
      (error 'rlm-inference-error
             :message "The :DIRECT base policy is not distillable."))
    keyword))

(-> rlm-distill--specializer-keyword (t) (option keyword))
(defun rlm-distill--specializer-keyword (parameter)
  "Return the eql-specializer keyword of PARAMETER, or NIL."
  (when (and (listp parameter)
             (= (length parameter) 2)
             (listp (second parameter))
             (eq (first (second parameter)) 'eql))
    (let ((value (second (second parameter))))
      (cond
        ((keywordp value)
         value)
        ((and (listp value)
              (eq (first value) 'quote)
              (keywordp (second value)))
         (second value))
        (t
         nil)))))

(-> rlm-distill-validate-method (keyword string) list)
(defun rlm-distill-validate-method (policy method-source)
  "Return METHOD-SOURCE's parsed form after structural validation.

The source must hold exactly one RLM-DECOMPOSE-INFERENCE-TASK defmethod
whose first parameter is eql-specialized on POLICY and whose remaining
parameters keep the generic's (task string) (views list)
(budget rlm-budget) shape. Reading disables evaluation; nothing here
evaluates or installs the method."
  (flet ((reject (problem)
           (error 'rlm-inference-error
                  :message (format nil "The distilled method was rejected: ~A"
                                   problem))))
    (let ((form
            (with-standard-io-syntax
              (let ((*read-eval* nil)
                    (*package* (find-package '#:autolith)))
                (multiple-value-bind (form position)
                    (handler-case
                        (read-from-string method-source)
                      (error (condition)
                        (reject (format nil "unreadable source: ~A"
                                        condition))))
                  (unless (eq (read-from-string method-source nil ':eof
                                                :start position)
                              ':eof)
                    (reject "the source must hold exactly one form"))
                  form)))))
      (unless (and (listp form) (eq (first form) 'defmethod))
        (reject "the form must be one defmethod"))
      (unless (eq (second form) 'rlm-decompose-inference-task)
        (reject "the method must specialize rlm-decompose-inference-task"))
      (let ((lambda-list (third form)))
        (unless (and (listp lambda-list) (= (length lambda-list) 4))
          (reject "the method takes exactly the generic's four parameters"))
        (let ((specializer (rlm-distill--specializer-keyword
                            (first lambda-list))))
          (unless specializer
            (reject "the policy parameter must carry an eql specializer"))
          (unless (eq specializer policy)
            (reject (format nil "the eql specializer ~S does not match policy ~S"
                            specializer policy)))
          (when (eq specializer ':direct)
            (reject "the :DIRECT base policy is not editable")))
        (unless (and (equal (second lambda-list) '(task string))
                     (equal (third lambda-list) '(views list))
                     (equal (fourth lambda-list) '(budget rlm-budget)))
          (reject "the task, views, and budget parameters must keep the generic's specializers")))
      form)))

(-> rlm-distill
    (&key (:traces list)
          (:instructions (option string))
          (:budget (option rlm-budget))
          (:model (option string))
          (:effort (option string))
          (:provider (option model-provider))
          (:configuration (option configuration))
          (:activity-callback (option function)))
    list)
(defun rlm-distill (&key traces instructions budget model effort provider
                         configuration activity-callback)
  "Review inference TRACES and propose one distilled decomposition policy.

A cheap gate frame reads the bounded traces first and prefers declining;
a declined review returns (:worth nil :rationale ... :gate-trace ...).
Otherwise a planner frame proposes one eql-specialized
RLM-DECOMPOSE-INFERENCE-TASK method, structurally validated and
re-asked with the rejection reason within the attempt bound. The
returned plist carries the proposal for the ordinary durable mutation
pipeline; nothing is evaluated or installed here."
  (unless (and traces
               (every #'stringp traces)
               (<= (length traces) *rlm-distill-maximum-traces*))
    (error 'rlm-inference-error
           :message (format nil "Distillation reviews 1 to ~D trace identifiers."
                            *rlm-distill-maximum-traces*)))
  (multiple-value-bind (provider configuration)
      (rlm--resolve-environment :model model :effort effort
                                :provider provider
                                :configuration configuration)
    (let* ((budget (or budget (rlm-budget-create)))
           (views (loop for identifier in traces
                        collect (rlm-distill--trace-view configuration
                                                         identifier)))
           (instruction-views
             (and (non-empty-string-p instructions)
                  (list (list ':label "instructions"
                              ':content instructions)))))
      (multiple-value-bind (gate-value gate-trace)
          (infer *rlm-distill-gate-task*
                 :context (append views instruction-views)
                 :contract *rlm-distill-gate-contract*
                 :budget budget
                 :provider provider
                 :configuration configuration
                 :activity-callback activity-callback)
        (let ((rationale (rlm-distill--object-field gate-value "rationale")))
          (unless (eq (rlm-distill--object-field gate-value "worth") t)
            (return-from rlm-distill
              (list ':worth nil
                    ':rationale rationale
                    ':gate-trace gate-trace)))
          (let ((problem nil))
            (dotimes (attempt *rlm-distill-planner-attempts*
                              (error 'rlm-inference-error
                                     :message
                                     (format nil "Distillation planning failed: ~A"
                                             problem)))
              (declare (ignore attempt))
              (multiple-value-bind (plan-value plan-trace)
                  (infer *rlm-distill-plan-task*
                         :context (append
                                   views instruction-views
                                   (when problem
                                     (list (list ':label "previous rejection"
                                                 ':content problem))))
                         :contract *rlm-distill-plan-contract*
                         :budget budget
                         :provider provider
                         :configuration configuration
                         :activity-callback activity-callback)
                (handler-case
                    (let* ((policy (rlm-distill--policy-keyword
                                    (rlm-distill--object-field plan-value
                                                               "policy")))
                           (method-source (rlm-distill--object-field
                                           plan-value "method-source")))
                      (unless (stringp method-source)
                        (error 'rlm-inference-error
                               :message "The proposal carries no method source."))
                      (rlm-distill-validate-method policy method-source)
                      (return-from rlm-distill
                        (list ':worth t
                              ':policy policy
                              ':title (rlm-distill--object-field plan-value
                                                                 "title")
                              ':rationale (rlm-distill--object-field
                                           plan-value "rationale")
                              ':method-source method-source
                              ':gate-trace gate-trace
                              ':plan-trace plan-trace)))
                  (rlm-inference-error (condition)
                    (setf problem
                          (rlm-inference-error-message condition))))))))))))
