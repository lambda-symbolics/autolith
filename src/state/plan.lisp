(in-package #:autolith)

(define-condition plan-error (autolith-error)
  ((pathname
    :initarg       :pathname
    :reader        plan-error-pathname
    :type          (option pathname)
    :documentation "The plan state pathname related to the failure.")
   (operation
    :initarg       :operation
    :reader        plan-error-operation
    :type          keyword
    :documentation "The plan operation that failed."))
  (:documentation "A durable workspace plan operation failed.")
  (:report
   (lambda (condition stream)
     (format stream "Plan ~A failed~@[ at ~A~]: ~A"
             (plan-error-operation condition)
             (plan-error-pathname condition)
             (autolith-error-message condition)))))

(-> plan--step-form-p (t) boolean)
(defun plan--step-form-p (form)
  "Return true when FORM is one portable plan step."
  (and (listp form)
       (eq (first form) :step)
       (let ((text       (getf (rest form) :text))
             (status     (getf (rest form) :status))
             (identifier (getf (rest form) :id))
             (requires   (getf (rest form) :requires))
             (verify     (getf (rest form) :verify))
             (receipt    (getf (rest form) :receipt)))
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
                 :tag      ':plan
                 :versions (list *plan-version*)
                 :fields   (list (list :indicator ':directory
                                       :validate  #'non-empty-string-p
                                       :required  t)
                                 (list :indicator ':updated-at
                                       :validate  (lambda (value)
                                                    (typep value 'timestamp))
                                       :required  t)
                                 (list :indicator ':explanation
                                       :validate  (lambda (value)
                                                    (or (null value)
                                                        (stringp value))))
                                 (list :indicator ':steps
                                       :validate  #'plan--steps-p
                                       :required  t)))))

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
        :directory   (workspace-plan-directory plan)
        :explanation (workspace-plan-explanation plan)
        :updated-at  (workspace-plan-updated-at plan)
        :steps       (mapcar #'plan-step->form (workspace-plan-steps plan))))

(-> form->plan-step (list) plan-step)
(defun form->plan-step (form)
  "Return the plan step represented by FORM."
  (make-instance 'plan-step
                 :text       (getf (rest form) :text)
                 :status     (getf (rest form) :status)
                 :identifier (getf (rest form) :id)
                 :requires   (getf (rest form) :requires)
                 :verify     (getf (rest form) :verify)
                 :receipt    (getf (rest form) :receipt)))

(-> form->plan (list) workspace-plan)
(defun form->plan (form)
  "Return the workspace plan represented by FORM."
  (make-instance 'workspace-plan
                 :directory   (getf (rest form) :directory)
                 :explanation (getf (rest form) :explanation)
                 :updated-at  (getf (rest form) :updated-at)
                 :steps       (mapcar #'form->plan-step (getf (rest form) :steps))))

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
               :message   (format nil "Could not load the workspace plan: ~A"
                                  condition)
               :pathname  pathname
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
             :message   (format nil "Could not write the workspace plan: ~A"
                                condition)
             :pathname  pathname
             :operation ':write))))

(-> plan--delete-path (pathname keyword) null)
(defun plan--delete-path (pathname operation)
  "Remove PATHNAME when present, reporting failures as OPERATION."
  (when (probe-file pathname)
    (handler-case
        (delete-file pathname)
      (error (condition)
        (error 'plan-error
               :message   (format nil "Could not remove the workspace plan: ~A"
                                  condition)
               :pathname  pathname
               :operation operation))))
  nil)

(-> plan--retire-matching-legacy (configuration string) null)
(defun plan--retire-matching-legacy (configuration directory)
  "Remove the legacy global plan when it belongs to DIRECTORY."
  (let* ((pathname (configuration-legacy-plan-path configuration))
         (plan     (plan--read-path pathname)))
    (when (and plan
               (string= (workspace-plan-directory plan) directory))
      (plan--delete-path pathname ':migrate)))
  nil)

(-> plan--load-unlocked (configuration) (option workspace-plan))
(defun plan--load-unlocked (configuration)
  "Return CONFIGURATION's workspace plan, migrating legacy state when needed."
  (let* ((directory (plan--directory-name configuration))
         (pathname  (plan--pathname configuration))
         (plan      (plan--read-path pathname)))
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
           :message   "A plan requires at least one step."
           :pathname  (plan--pathname configuration)
           :operation ':update))
  (when (> (length steps) *plan-maximum-steps*)
    (error 'plan-error
           :message   (format nil "A plan may contain at most ~D steps."
                              *plan-maximum-steps*)
           :pathname  (plan--pathname configuration)
           :operation ':update))
  (when (and explanation
             (not (stringp explanation)))
    (error 'plan-error
           :message   "Plan explanation must be a string."
           :pathname  (plan--pathname configuration)
           :operation ':update))
  (let ((plan-steps
          (mapcar
           (lambda (step)
             (let* ((text   (getf step :text))
                    (status (plan--status (getf step :status))))
               (unless (and (non-empty-string-p text)
                            (<= (length text) *plan-step-text-limit*))
                 (error 'plan-error
                        :message   "Each plan step needs bounded non-empty text."
                        :pathname  (plan--pathname configuration)
                        :operation ':update))
               (unless status
                 (error 'plan-error
                        :message   "Plan status must be pending, doing, or done."
                        :pathname  (plan--pathname configuration)
                        :operation ':update))
               (make-instance 'plan-step
                              :text       text
                              :status     status
                              :identifier (getf step :id)
                              :requires   (getf step :requires)
                              :verify     (getf step :verify)
                              :receipt    (getf step :receipt))))
           steps)))
    (plan-write
     configuration
     (make-instance
      'workspace-plan
      :directory   (plan--directory-name configuration)
      :explanation (and (non-empty-string-p explanation) explanation)
      :updated-at  (get-universal-time)
      :steps       plan-steps))))
