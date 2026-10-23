(in-package #:autolith)

;;;; -- Workspace Plan --

(defparameter *plan-version* 1
  "The readable workspace plan format version.")

(defparameter *plan-maximum-steps* 32
  "The maximum number of steps retained in one workspace plan.")

(defparameter *plan-step-text-limit* 500
  "The maximum character count of one plan step.")

(deftype plan-status ()
  "The lifecycle of one plan step."
  '(member :pending :doing :done))

(defclass plan-step ()
  ((text
    :initarg :text
    :reader plan-step-text
    :type non-empty-string
    :documentation "The ordered work described by this plan step.")
   (status
    :initarg :status
    :reader plan-step-status
    :type plan-status
    :documentation "Whether this step is pending, in progress, or done.")
   (identifier
    :initarg :identifier
    :initform nil
    :reader plan-step-identifier
    :type (option string)
    :documentation "A stable name keeping the step's receipt across plan updates.")
   (requires
    :initarg :requires
    :initform nil
    :reader plan-step-requires
    :type list
    :documentation "Fact patterns that must hold for the step; their variables fill {?var} in its text.")
   (verify
    :initarg :verify
    :initform nil
    :reader plan-step-verify
    :type (option list)
    :documentation "A fact pattern whose observation confirms the step was done.")
   (receipt
    :initarg :receipt
    :initform nil
    :reader plan-step-receipt
    :type list
    :documentation "When the step was done, the bindings and facts it relied on."))
  (:documentation "One ordered step in the current workspace plan."))

(defclass workspace-plan ()
  ((directory
    :initarg :directory
    :reader workspace-plan-directory
    :type non-empty-string
    :documentation "The workspace directory this plan belongs to.")
   (explanation
    :initarg :explanation
    :initform nil
    :reader workspace-plan-explanation
    :type (option string)
    :documentation "Optional short explanation of the current plan.")
   (steps
    :initarg :steps
    :initform nil
    :reader workspace-plan-steps
    :type list
    :documentation "Ordered plan steps from first to last.")
   (updated-at
    :initarg :updated-at
    :reader workspace-plan-updated-at
    :type timestamp
    :documentation "The universal time at which this plan last changed."))
  (:documentation "The current ordered plan for one workspace."))


(define-condition plan-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader plan-error-pathname
    :type (option pathname)
    :documentation "The plan state pathname related to the failure.")
   (operation
    :initarg :operation
    :reader plan-error-operation
    :type keyword
    :documentation "The plan operation that failed."))
  (:documentation "A durable workspace plan operation failed.")
  (:report
   (lambda (condition stream)
     (format stream "Plan ~A failed~@[ at ~A~]: ~A"
             (plan-error-operation condition)
             (plan-error-pathname condition)
             (autolith-error-message condition)))))


(-> plan--status (t) (option plan-status))
(defun plan--status (value)
  "Return VALUE as a plan status keyword, or NIL when invalid."
  (cond
    ((typep value 'plan-status) value)
    ((not (stringp value)) nil)
    ((string-equal value "pending") ':pending)
    ((or (string-equal value "doing")
         (string-equal value "in_progress")
         (string-equal value "inProgress"))
     ':doing)
    ((or (string-equal value "done")
         (string-equal value "completed"))
     ':done)
    (t nil)))

(-> plan--step-form-p (t) boolean)
(defun plan--step-form-p (form)
  "Return true when FORM is one portable plan step."
  (and (listp form)
       (eq (first form) :step)
       (let ((text (getf (rest form) :text))
             (status (getf (rest form) :status))
             (identifier (getf (rest form) :id))
             (requires (getf (rest form) :requires))
             (verify (getf (rest form) :verify))
             (receipt (getf (rest form) :receipt)))
         (and (non-empty-string-p text)
              (<= (length text) *plan-step-text-limit*)
              (typep status 'plan-status)
              (or (null identifier) (non-empty-string-p identifier))
              (proper-list-p requires)
              (every #'consp requires)
              (or (null verify) (consp verify))
              (proper-list-p receipt)
              (evenp (length receipt))))
       t))

(-> plan--steps-p (t) boolean)
(defun plan--steps-p (steps)
  "Return true when STEPS is a bounded list of portable plan steps."
  (and (listp steps)
       (<= (length steps) *plan-maximum-steps*)
       (every #'plan--step-form-p steps)))

(-> plan--form-p (t) boolean)
(defun plan--form-p (form)
  "Return true when FORM is a complete portable workspace plan."
  (values
   (record-check form
                 :tag ':plan
                 :versions (list *plan-version*)
                 :fields (list (list :indicator ':directory
                                     :validate #'non-empty-string-p
                                     :required t)
                               (list :indicator ':updated-at
                                     :validate (lambda (value)
                                                 (typep value 'timestamp))
                                     :required t)
                               (list :indicator ':explanation
                                     :validate (lambda (value)
                                                 (or (null value)
                                                     (stringp value))))
                               (list :indicator ':steps
                                     :validate #'plan--steps-p
                                     :required t)))))

(-> plan-step->form (plan-step) list)
(defun plan-step->form (step)
  "Return STEP as one portable form.

Optional structure appears only when present, so a prose step keeps its
earlier form."
  (append (list :step
                :text (plan-step-text step)
                :status (plan-step-status step))
          (when (plan-step-identifier step)
            (list :id (plan-step-identifier step)))
          (when (plan-step-requires step)
            (list :requires (copy-tree (plan-step-requires step))))
          (when (plan-step-verify step)
            (list :verify (copy-tree (plan-step-verify step))))
          (when (plan-step-receipt step)
            (list :receipt (copy-tree (plan-step-receipt step))))))

(-> plan->form (workspace-plan) list)
(defun plan->form (plan)
  "Return PLAN as one portable form."
  (list :plan
        :version *plan-version*
        :directory (workspace-plan-directory plan)
        :explanation (workspace-plan-explanation plan)
        :updated-at (workspace-plan-updated-at plan)
        :steps (mapcar #'plan-step->form (workspace-plan-steps plan))))

(-> form->plan-step (list) plan-step)
(defun form->plan-step (form)
  "Return the plan step represented by FORM."
  (make-instance 'plan-step
                 :text (getf (rest form) :text)
                 :status (getf (rest form) :status)
                 :identifier (getf (rest form) :id)
                 :requires (getf (rest form) :requires)
                 :verify (getf (rest form) :verify)
                 :receipt (getf (rest form) :receipt)))

(-> form->plan (list) workspace-plan)
(defun form->plan (form)
  "Return the workspace plan represented by FORM."
  (make-instance 'workspace-plan
                 :directory (getf (rest form) :directory)
                 :explanation (getf (rest form) :explanation)
                 :updated-at (getf (rest form) :updated-at)
                 :steps (mapcar #'form->plan-step (getf (rest form) :steps))))

(-> plan--directory-name (configuration) string)
(defun plan--directory-name (configuration)
  "Return CONFIGURATION's canonical workspace directory key."
  (workspace-directory-name
   (config :working-directory configuration)))

(-> plan--pathname (configuration) pathname)
(defun plan--pathname (configuration)
  "Return CONFIGURATION's workspace-specific plan pathname."
  (configuration-plan-path
   configuration
   (workspace-directory-identifier
    (config :working-directory configuration))))

(-> plan--read-path (pathname) (option workspace-plan))
(defun plan--read-path (pathname)
  "Return the valid plan at PATHNAME, or NIL when absent or malformed."
  (when (probe-file pathname)
    (handler-case
        (multiple-value-bind (form complete-p)
            (snapshot-read pathname)
          (when (and complete-p (plan--form-p form))
            (form->plan form)))
      (error (condition)
        (error 'plan-error
               :message (format nil "Could not load the workspace plan: ~A"
                                condition)
               :pathname pathname
               :operation ':load)))))

(-> plan--write-path (pathname workspace-plan) workspace-plan)
(defun plan--write-path (pathname plan)
  "Atomically publish PLAN at PATHNAME."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (snapshot-write pathname (plan->form plan))
        plan)
    (error (condition)
      (error 'plan-error
             :message (format nil "Could not write the workspace plan: ~A"
                              condition)
             :pathname pathname
             :operation ':write))))

(-> plan--delete-path (pathname keyword) null)
(defun plan--delete-path (pathname operation)
  "Remove PATHNAME when present, reporting failures as OPERATION."
  (when (probe-file pathname)
    (handler-case
        (delete-file pathname)
      (error (condition)
        (error 'plan-error
               :message (format nil "Could not remove the workspace plan: ~A"
                                condition)
               :pathname pathname
               :operation operation))))
  nil)

(-> plan--retire-matching-legacy (configuration string) null)
(defun plan--retire-matching-legacy (configuration directory)
  "Remove the legacy global plan when it belongs to DIRECTORY."
  (let* ((pathname (configuration-legacy-plan-path configuration))
         (plan (plan--read-path pathname)))
    (when (and plan
               (string= (workspace-plan-directory plan) directory))
      (plan--delete-path pathname ':migrate)))
  nil)

(-> plan--load-unlocked (configuration) (option workspace-plan))
(defun plan--load-unlocked (configuration)
  "Return CONFIGURATION's workspace plan, migrating legacy state when needed."
  (let* ((directory (plan--directory-name configuration))
         (pathname (plan--pathname configuration))
         (plan (plan--read-path pathname)))
    (cond
      (plan
       (and (string= (workspace-plan-directory plan) directory)
            plan))
      ((probe-file pathname)
       nil)
      (t
       (let* ((legacy-pathname
                (configuration-legacy-plan-path configuration))
              (legacy-plan (plan--read-path legacy-pathname)))
         (when (and legacy-plan
                    (string= (workspace-plan-directory legacy-plan) directory))
           (plan--write-path pathname legacy-plan)
           (plan--delete-path legacy-pathname ':migrate)
           legacy-plan))))))

(-> plan--write-unlocked (configuration workspace-plan) workspace-plan)
(defun plan--write-unlocked (configuration plan)
  "Atomically publish PLAN for CONFIGURATION without affecting other workspaces."
  (let ((directory (plan--directory-name configuration)))
    (plan--write-path (plan--pathname configuration) plan)
    (plan--retire-matching-legacy configuration directory)
    plan))

(-> plan--clear-unlocked (configuration) null)
(defun plan--clear-unlocked (configuration)
  "Remove only CONFIGURATION's workspace plan when present."
  (let ((directory (plan--directory-name configuration)))
    (plan--delete-path (plan--pathname configuration) ':clear)
    (plan--retire-matching-legacy configuration directory))
  nil)

(defvar *plan-lock* (make-recursive-lock "Autolith workspace plans")
  "Serialize plan mutations and import rollback in this process.")

(-> plan--call-with-lock (configuration function) t)
(defun plan--call-with-lock (configuration function)
  "Call FUNCTION under the process-local and shared plan publication locks."
  (with-recursive-lock-held (*plan-lock*)
    (call-with-file-lock (merge-pathnames "plans.lock" (config :state-root configuration))
                         function)))

(-> plan-load (configuration) (option workspace-plan))
(defun plan-load (configuration)
  "Read or migrate a workspace plan under the shared publication lock."
  (plan--call-with-lock configuration (lambda () (plan--load-unlocked configuration))))

(-> plan-write (configuration workspace-plan) workspace-plan)
(defun plan-write (configuration plan)
  "Publish a workspace plan under the shared publication lock."
  (plan--call-with-lock configuration (lambda () (plan--write-unlocked configuration plan))))

(-> plan-clear (configuration) null)
(defun plan-clear (configuration)
  "Remove a workspace plan under the shared publication lock."
  (plan--call-with-lock configuration (lambda () (plan--clear-unlocked configuration))))

(-> plan-update
    (configuration list &key (:explanation (option string)))
    workspace-plan)
(defun plan-update (configuration steps &key explanation)
  "Replace CONFIGURATION's plan with ordered STEPS and optional EXPLANATION.

STEPS is a list of (:text string :status plan-status) property lists, which
may add :ID, :REQUIRES fact patterns, a :VERIFY pattern, and a :RECEIPT."
  (unless (and (listp steps) (plusp (length steps)))
    (error 'plan-error
           :message "A plan requires at least one step."
           :pathname (plan--pathname configuration)
           :operation ':update))
  (when (> (length steps) *plan-maximum-steps*)
    (error 'plan-error
           :message (format nil "A plan may contain at most ~D steps."
                            *plan-maximum-steps*)
           :pathname (plan--pathname configuration)
           :operation ':update))
  (when (and explanation
             (not (stringp explanation)))
    (error 'plan-error
           :message "Plan explanation must be a string."
           :pathname (plan--pathname configuration)
           :operation ':update))
  (let ((plan-steps
          (mapcar
           (lambda (step)
             (let* ((text (getf step :text))
                    (status (plan--status (getf step :status))))
               (unless (and (non-empty-string-p text)
                            (<= (length text) *plan-step-text-limit*))
                 (error 'plan-error
                        :message "Each plan step needs bounded non-empty text."
                        :pathname (plan--pathname configuration)
                        :operation ':update))
               (unless status
                 (error 'plan-error
                        :message "Plan status must be pending, doing, or done."
                        :pathname (plan--pathname configuration)
                        :operation ':update))
               (make-instance 'plan-step
                              :text text
                              :status status
                              :identifier (getf step :id)
                              :requires (getf step :requires)
                              :verify (getf step :verify)
                              :receipt (getf step :receipt))))
           steps)))
    (plan-write
     configuration
     (make-instance
      'workspace-plan
      :directory (plan--directory-name configuration)
      :explanation (and (non-empty-string-p explanation) explanation)
      :updated-at (get-universal-time)
      :steps plan-steps))))

(-> plan-step-solutions (plan-step knowledge-view) (values list boolean))
(defun plan-step-solutions (step view)
  "Return the ways VIEW satisfies STEP's requirements, and whether the search was cut short."
  (knowledge-view-solve view (plan-step-requires step)))

(-> plan--unmet-pattern (plan-step knowledge-view) (option list))
(defun plan--unmet-pattern (step view)
  "Return the first of STEP's required patterns no fact in VIEW satisfies with those before it."
  (loop for count from 1 to (length (plan-step-requires step))
        unless (knowledge-view-solve view (subseq (plan-step-requires step) 0 count) :limit 1)
          return (nth (1- count) (plan-step-requires step))))

(-> plan-step-text-with (plan-step list) string)
(defun plan-step-text-with (step bindings)
  "Return STEP's text with each {?var} placeholder replaced from BINDINGS."
  (let ((text (plan-step-text step)))
    (loop for (variable . term) in bindings
          do (setf text (uiop:frob-substrings text (list (format nil "{~A}" variable))
                                              (knowledge-term-display term))))
    text))

(-> plan-step-check (plan-step knowledge-view) list)
(defun plan-step-check (step view)
  "Return STEP's state against VIEW as a plist.

:STATE is :READY when its requirements hold, :BLOCKED with the :UNMET pattern,
:UNKNOWN when the search was cut short, or :PROSE without requirements. A done
step also reports :CHANGED, the facts it relied on that no longer hold, and
:UNVERIFIED when its verify pattern is not observed."
  (multiple-value-bind (solutions cut-p)
      (if (plan-step-requires step)
          (plan-step-solutions step view)
          (values nil nil))
    (let ((solution (first solutions)))
      (list :state (cond
                     ((null (plan-step-requires step)) ':prose)
                     (solution ':ready)
                     (cut-p ':unknown)
                     (t ':blocked))
            :bindings (first solution)
            :facts (mapcar #'knowledge-node-identifier (rest solution))
            :conflicted (remove-if-not (lambda (node) (knowledge-node-conflicts node))
                                       (rest solution))
            :unmet (and (plan-step-requires step) (null solution) (not cut-p)
                        (plan--unmet-pattern step view))
            :changed (and (eq (plan-step-status step) ':done)
                          (remove-if (lambda (identifier)
                                       (let ((node (knowledge-view-node view identifier)))
                                         (and node (knowledge-node-supported-p node))))
                                     (getf (plan-step-receipt step) :facts)))
            :unverified (and (eq (plan-step-status step) ':done)
                             (plan-step-verify step)
                             (null (knowledge-view-solve view (list (plan-step-verify step))
                                                         :limit 1)))))))

(-> plan--step-note (plan-step list knowledge-view) (option string))
(defun plan--step-note (step check view)
  "Return the parenthesized note explaining STEP's CHECK, or NIL when none applies."
  (flet ((fact-text (identifier)
           (let ((node (knowledge-view-node view identifier))
                 (recorded (nth (position identifier (getf (plan-step-receipt step) :facts)
                                          :test #'equal)
                                (getf (plan-step-receipt step) :texts))))
             (cond
               (node (knowledge-node-text node))
               (recorded recorded)
               (t identifier)))))
    (cond
      ((getf check :changed)
       (format nil "done; it relied on ~{~A~^, ~}, which no longer hold~:[s~;~]"
               (mapcar #'fact-text (getf check :changed))
               (rest (getf check :changed))))
      ((getf check :unverified)
       (format nil "done, but ~A is not observed"
               (knowledge-pattern-text (plan-step-verify step))))
      ((eq (plan-step-status step) ':done)
       nil)
      ((eq (getf check :state) ':blocked)
       (format nil "blocked: nothing satisfies ~A" (knowledge-pattern-text (getf check :unmet))))
      ((eq (getf check :state) ':unknown)
       "requirements not fully checked: the search reached its limit")
      ((getf check :conflicted)
       (format nil "relies on conflicting facts: ~{~A~^, ~}"
               (mapcar #'knowledge-node-text (getf check :conflicted))))
      (t
       nil))))

(-> plan-render ((option workspace-plan) &key (:view (option knowledge-view))) string)
(defun plan-render (plan &key view)
  "Return PLAN as compact model-visible text.

With VIEW, steps with requirements show their text filled from current facts
and a note when they are blocked, rely on conflicting facts, or were done on
facts that no longer hold. Prose steps render as always."
  (if (or (null plan) (null (workspace-plan-steps plan)))
      "No active plan."
      (with-output-to-string (stream)
        (when (workspace-plan-explanation plan)
          (format stream "Explanation: ~A~%"
                  (workspace-plan-explanation plan)))
        (loop for step in (workspace-plan-steps plan)
              for index from 1
              for check = (and view (or (plan-step-requires step) (plan-step-verify step))
                               (plan-step-check step view))
              for bindings = (if (eq (plan-step-status step) ':done)
                                 (or (getf (plan-step-receipt step) :bindings)
                                     (getf check :bindings))
                                 (getf check :bindings))
              do (format stream "~D. [~(~A~)] ~A~@[ (~A)~]~%"
                         index
                         (plan-step-status step)
                         (plan-step-text-with step bindings)
                         (and check (plan--step-note step check view)))))))

(-> plan-step-snapshot ((option workspace-plan) knowledge-view) list)
(defun plan-step-snapshot (plan view)
  "Return how PLAN's fact-dependent steps read against VIEW, for change detection.

Each entry is (:KEY :INDEX :TEXT :NOTE); prose steps are left out."
  (and plan
       (loop for step in (workspace-plan-steps plan)
             for index from 1
             when (or (plan-step-requires step) (plan-step-verify step))
               collect (let ((check (plan-step-check step view)))
                         (list :key (or (plan-step-identifier step) (plan-step-text step))
                               :index index
                               :text (plan-step-text-with
                                      step
                                      (if (eq (plan-step-status step) ':done)
                                          (or (getf (plan-step-receipt step) :bindings)
                                              (getf check :bindings))
                                          (getf check :bindings)))
                               :note (plan--step-note step check view))))))

(-> plan-snapshot-changes (list list) list)
(defun plan-snapshot-changes (previous current)
  "Return one line for each step in both snapshots whose text or note changed."
  (loop for entry in current
        for old = (find (getf entry :key) previous
                        :key (lambda (item) (getf item :key)) :test #'equal)
        when (and old (or (string/= (getf old :text) (getf entry :text))
                          (not (equal (getf old :note) (getf entry :note)))))
          collect (format nil "step ~D now reads \"~A\"~:[ and is ready~; (~:*~A)~]~@[, was \"~A\"~]"
                          (getf entry :index) (getf entry :text) (getf entry :note)
                          (and (string/= (getf old :text) (getf entry :text))
                               (getf old :text)))))
