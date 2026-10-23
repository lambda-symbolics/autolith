(in-package #:autolith)

;;;; -- Task Methods --

;;; A method says how to do a task when facts allow it:
;;;
;;;   (:method :id "prepare-appointment"
;;;    :task ("prepare" "?event")
;;;    :when (("event.start" "?event" "?start"))
;;;    :steps ((:task ("check-route" "?event"))
;;;            (:step "Leave at {?leave}"
;;;             :requires (("event.departure" "?event" "?leave")))))
;;;
;;; Expanding a task tries its methods in order, each with the solutions of its
;;; conditions, and expands subtasks the same way, until only plain steps
;;; remain. The search is bounded in depth and in expanded tasks. Its outcome
;;; distinguishes a task no method describes, a task whose methods do not
;;; apply to the current facts, and a search that ran out of budget.

(defparameter *knowledge-method-depth-limit* 8
  "The deepest nesting of subtasks one expansion follows.")

(defparameter *knowledge-method-node-limit* 1000
  "The most tasks one expansion tries to expand.")

(defparameter *knowledge-method-step-limit* 16
  "The most steps and subtasks one method lists.")


;;;; -- Validation --

(-> knowledge--task-call-p (t) boolean)
(defun knowledge--task-call-p (value)
  "Return true when VALUE is a task name followed by variables or values."
  (and (proper-list-p value)
       (knowledge-rule-name-p (first value))
       (every (lambda (item) (or (knowledge-variable-p item) (knowledge-term-p item)))
              (rest value))
       t))

(-> knowledge-method-p (t) boolean)
(defun knowledge-method-p (method)
  "Return true when METHOD has the structure of a validated method."
  (and (proper-list-p method)
       (eq (first method) :method)
       (evenp (length (rest method)))
       (let ((properties (rest method)))
         (and (knowledge-rule-name-p (getf properties :id))
              (knowledge--task-call-p (getf properties :task))
              (proper-list-p (getf properties :when))
              (proper-list-p (getf properties :steps))
              (getf properties :steps)
              (every (lambda (step)
                       (and (proper-list-p step)
                            (case (first step)
                              (:task (knowledge--task-call-p (second step)))
                              (:step (non-empty-string-p (second step)))
                              (otherwise nil))))
                     (getf properties :steps))))
       t))

(-> knowledge--task-from-input (t) list)
(defun knowledge--task-from-input (value)
  "Return the task call VALUE, a task name and its variables or values, as written."
  (let ((items (and (or (consp value) (vectorp value)) (coerce value 'list))))
    (unless (and items (knowledge-rule-name-p (first items)))
      (knowledge--fail ':invalid-method value
                       "A task is a lowercase name followed by its arguments."))
    (cons (first items)
          (mapcar (lambda (item)
                    (cond
                      ((knowledge-variable-p item) item)
                      ((or (stringp item) (rationalp item)) item)
                      ((floatp item) (rationalize item))
                      (t (knowledge--fail ':invalid-method item
                                          "Task arguments are variables, text, or numbers."))))
                  (rest items)))))

(-> knowledge--pattern-variables (list) list)
(defun knowledge--pattern-variables (pattern)
  "Return the variables of PATTERN, negated or not."
  (remove-if-not #'knowledge-variable-p (rest (knowledge-rule-premise-atom pattern))))

(-> knowledge-make-method
    (&key (:id string) (:task t) (:when sequence) (:steps sequence)
          (:description (option string)) (:schema-function function))
    list)
(defun knowledge-make-method (&key id task when steps description schema-function)
  "Return a validated method built from input forms.

Conditions and step requirements are read by their predicates' schemas. Every
variable a subtask passes on must be a task parameter or bound by a positive
condition; plain steps may introduce variables their requirements bind."
  (unless (knowledge-rule-name-p id)
    (knowledge--fail ':invalid-method id
                     "Method names are lowercase letters, digits, hyphens, and dots."))
  (let* ((task (knowledge--task-from-input task))
         (conditions (mapcar (lambda (pattern)
                               (knowledge--pattern-from-input pattern schema-function
                                                              :negatable t))
                             (coerce when 'list)))
         (bound (append (remove-if-not #'knowledge-variable-p (rest task))
                        (loop for pattern in conditions
                              unless (knowledge-negation-p pattern)
                                append (knowledge--pattern-variables pattern))))
         (items (coerce steps 'list)))
    (unless (<= 1 (length items) *knowledge-method-step-limit*)
      (knowledge--fail ':invalid-method id "A method lists 1 to ~D steps and subtasks."
                       *knowledge-method-step-limit*))
    (dolist (pattern conditions)
      (when (knowledge-negation-p pattern)
        (dolist (variable (knowledge--pattern-variables pattern))
          (unless (member variable bound :test #'string=)
            (knowledge--fail ':invalid-method variable
                             "~A in a negated condition must be bound first." variable)))))
    (list :method
          :id id
          :task task
          :when conditions
          :steps
          (mapcar
           (lambda (item)
             (unless (hash-table-p item)
               (knowledge--fail ':invalid-method item
                                "Each method step is an object with task, or with step and optional requires and verify."))
             (cond
               ((nth-value 1 (gethash "task" item))
                (let ((call (knowledge--task-from-input (gethash "task" item))))
                  (dolist (argument (rest call))
                    (when (and (knowledge-variable-p argument)
                               (not (member argument bound :test #'string=)))
                      (knowledge--fail ':invalid-method argument
                                       "Subtask argument ~A is not a task parameter or bound by a condition."
                                       argument)))
                  (list :task call)))
               ((non-empty-string-p (gethash "step" item))
                (append (list :step (gethash "step" item))
                        (when (gethash "requires" item)
                          (list :requires
                                (mapcar (lambda (pattern)
                                          (knowledge--pattern-from-input pattern schema-function
                                                                         :negatable t))
                                        (coerce (gethash "requires" item) 'list))))
                        (when (gethash "verify" item)
                          (list :verify
                                (knowledge--pattern-from-input (gethash "verify" item)
                                                               schema-function :negatable t)))))
               (t
                (knowledge--fail ':invalid-method item
                                 "Each method step names a task or has step text."))))
           items)
          :description description)))


;;;; -- Stored Methods --

(-> knowledge-base-methods (knowledge-base string integer &key (:workspace (option string)))
    list)
(defun knowledge-base-methods (base task arity &key workspace)
  "Return the methods for TASK with ARITY visible from WORKSPACE, workspace ones first."
  (let ((found nil))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (let ((method (getf entry :method)))
                 (when (and (knowledge-visible-p (getf entry :scope) (getf entry :workspace)
                                                 workspace)
                            (string= (first (getf (rest method) :task)) task)
                            (= (length (rest (getf (rest method) :task))) arity))
                   (push entry found))))
             (knowledge-base-methods-table base))
    (mapcar (lambda (entry) (getf entry :method))
            (sort found (lambda (left right)
                          (or (and (eq (getf left :scope) ':workspace)
                                   (eq (getf right :scope) ':global))
                              (and (eq (getf left :scope) (getf right :scope))
                                   (string< (getf (rest (getf left :method)) :id)
                                            (getf (rest (getf right :method)) :id)))))))))

(-> knowledge-define-method
    (configuration &key (:id string) (:task t) (:when sequence) (:steps sequence)
                        (:description (option string)) (:scope memory-scope)
                        (:source-conversation (option string)))
    (values string boolean))
(defun knowledge-define-method
    (configuration &key id task when steps description (scope ':workspace) source-conversation)
  "Validate and record method ID, replacing an earlier definition of it in SCOPE.

Return its ID and whether the definition changed."
  (let ((workspace (knowledge--owner configuration scope))
        (viewer (knowledge--workspace configuration)))
    (values-list
     (knowledge-transact
      configuration
      (lambda (base)
        (let* ((method (knowledge-make-method
                        :id id :task task :when when :steps steps :description description
                        :schema-function
                        (lambda (predicate)
                          (knowledge-base-schema base predicate
                                                 :workspace (and (eq scope ':workspace) viewer)))))
               (existing (gethash (list scope workspace id) (knowledge-base-methods-table base))))
          (if (equal (getf existing :method) method)
              (values nil (list id nil) nil)
              (values (list (list :method-defined
                                  :version *knowledge-format-version*
                                  :scope scope
                                  :workspace workspace
                                  :method method
                                  :time (get-universal-time)
                                  :source-conversation source-conversation))
                      (list id t)
                      t))))))))


;;;; -- Expansion --

(-> knowledge--instantiate-term (t list) t)
(defun knowledge--instantiate-term (item environment)
  "Return ITEM with its value in ENVIRONMENT when it is a bound variable."
  (if (knowledge-variable-p item)
      (let ((binding (assoc item environment :test #'string=)))
        (if binding (rest binding) item))
      item))

(-> knowledge--instantiate-pattern (list list) list)
(defun knowledge--instantiate-pattern (pattern environment)
  "Return PATTERN with its bound variables replaced from ENVIRONMENT."
  (if (knowledge-negation-p pattern)
      (list :not (knowledge--instantiate-pattern (second pattern) environment))
      (cons (first pattern)
            (mapcar (lambda (item) (knowledge--instantiate-term item environment))
                    (rest pattern)))))

(-> knowledge--bind-task (list list) (values list boolean))
(defun knowledge--bind-task (parameters arguments)
  "Bind method task PARAMETERS to call ARGUMENTS, matching literals by value."
  (let ((environment nil))
    (loop for parameter in parameters
          for argument in arguments
          do (cond
               ((knowledge-variable-p parameter)
                (push (cons parameter argument) environment))
               ((not (knowledge-term= parameter argument))
                (return-from knowledge--bind-task (values nil nil)))))
    (values environment t)))

(-> knowledge-expand-task
    (knowledge-view list &key (:depth-limit integer) (:node-limit integer))
    (values list keyword list))
(defun knowledge-expand-task
    (view task &key (depth-limit *knowledge-method-depth-limit*)
                    (node-limit *knowledge-method-node-limit*))
  "Expand TASK, a task name and ground arguments, into plain plan steps using VIEW's facts.

Return the steps, the outcome, and details. The outcome is :PLANNED; :UNKNOWN-TASK
when no method describes a task, naming it; :NO-PLAN when methods exist but
none applies to the current facts, naming the unmet conditions; or :EXHAUSTED
when the depth or task budget ran out first. Each step is a plist of :TEXT,
:ID, :REQUIRES, and :VERIFY ready for a plan."
  (let ((base (knowledge-view-base view))
        (workspace (knowledge-view-workspace view))
        (nodes 0)
        (exhausted-p nil)
        (unknown nil)
        (unmet nil))
    (labels ((expand (call depth path)
               (incf nodes)
               (cond
                 ((or (> depth depth-limit) (> nodes node-limit))
                  (setf exhausted-p t)
                  (values nil nil))
                 (t
                  (let ((methods (knowledge-base-methods base (first call) (length (rest call))
                                                         :workspace workspace)))
                    (unless methods
                      (pushnew (first call) unknown :test #'string=))
                    (dolist (method methods (values nil nil))
                      (multiple-value-bind (environment bound-p)
                          (knowledge--bind-task (rest (getf (rest method) :task)) (rest call))
                        (when bound-p
                          (let* ((conditions
                                   (mapcar (lambda (pattern)
                                             (knowledge--instantiate-pattern pattern environment))
                                           (getf (rest method) :when)))
                                 (solutions (if conditions
                                                (knowledge-view-solve view conditions)
                                                (list (list nil)))))
                            (unless solutions
                              (push (list :method (getf (rest method) :id)
                                          :conditions (mapcar #'knowledge-pattern-text conditions))
                                    unmet))
                            (dolist (solution solutions)
                              (multiple-value-bind (steps planned-p)
                                  (expand-steps method (append (first solution) environment)
                                                depth path)
                                (when planned-p
                                  (return-from expand (values steps t)))))))))))))

             (expand-steps (method environment depth path)
               (let ((steps nil))
                 (loop for item in (getf (rest method) :steps)
                       for index from 1
                       do (if (eq (first item) :task)
                              (let ((call (cons (first (second item))
                                                (mapcar (lambda (argument)
                                                          (knowledge--instantiate-term
                                                           argument environment))
                                                        (rest (second item))))))
                                (multiple-value-bind (expanded planned-p)
                                    (expand call (1+ depth)
                                            (append path (list (getf (rest method) :id) index)))
                                  (unless planned-p
                                    (return-from expand-steps (values nil nil)))
                                  (setf steps (append steps expanded))))
                              (setf steps
                                    (append
                                     steps
                                     (list
                                      (append
                                       (list :text (plan-step-text-with
                                                    (make-instance 'plan-step :text (second item)
                                                                              :status ':pending)
                                                    environment)
                                             :status "pending"
                                             :id (format nil "~{~A~^/~}"
                                                         (append path
                                                                 (list (getf (rest method) :id)
                                                                       index))))
                                       (when (getf (rest (rest item)) :requires)
                                         (list :requires
                                               (mapcar (lambda (pattern)
                                                         (knowledge--instantiate-pattern
                                                          pattern environment))
                                                       (getf (rest (rest item)) :requires))))
                                       (when (getf (rest (rest item)) :verify)
                                         (list :verify
                                               (knowledge--instantiate-pattern
                                                (getf (rest (rest item)) :verify) environment)))))))))
                 (values steps t))))
      (multiple-value-bind (steps planned-p) (expand task 0 nil)
        (cond
          (planned-p (values steps ':planned nil))
          (exhausted-p (values nil ':exhausted (list :nodes nodes)))
          ((and unknown (null unmet)) (values nil ':unknown-task (list :tasks unknown)))
          (t (values nil ':no-plan (list :unmet (reverse unmet) :tasks unknown))))))))
