(in-package #:autolith-supervisor)

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
    :initarg       :text
    :reader        plan-step-text
    :type          non-empty-string
    :documentation "The ordered work described by this plan step.")
   (status
    :initarg       :status
    :reader        plan-step-status
    :type          plan-status
    :documentation "Whether this step is pending, in progress, or done.")
   (identifier
    :initarg       :identifier
    :initform      nil
    :reader        plan-step-identifier
    :type          (option string)
    :documentation "A stable name keeping the step's receipt across plan updates.")
   (requires
    :initarg       :requires
    :initform      nil
    :reader        plan-step-requires
    :type          list
    :documentation "Fact patterns that must hold for the step; their variables fill {?var} in its text.")
   (verify
    :initarg       :verify
    :initform      nil
    :reader        plan-step-verify
    :type          (option list)
    :documentation "A fact pattern whose observation confirms the step was done.")
   (receipt
    :initarg       :receipt
    :initform      nil
    :reader        plan-step-receipt
    :type          list
    :documentation "When the step was done, the bindings and facts it relied on."))
  (:documentation "One ordered step in the current workspace plan."))

(defclass workspace-plan ()
  ((directory
    :initarg       :directory
    :reader        workspace-plan-directory
    :type          non-empty-string
    :documentation "The workspace directory this plan belongs to.")
   (explanation
    :initarg       :explanation
    :initform      nil
    :reader        workspace-plan-explanation
    :type          (option string)
    :documentation "Optional short explanation of the current plan.")
   (steps
    :initarg       :steps
    :initform      nil
    :reader        workspace-plan-steps
    :type          list
    :documentation "Ordered plan steps from first to last.")
   (updated-at
    :initarg       :updated-at
    :reader        workspace-plan-updated-at
    :type          timestamp
    :documentation "The universal time at which this plan last changed."))
  (:documentation "The current ordered plan for one workspace."))

(serapeum/types:-> plan--status (t) (option plan-status))
(defun plan--status (value)
  "Return VALUE as a plan status keyword, or NIL when invalid."
  (cond
    ((typep value 'plan-status)
     value)
    ((not (stringp value))
     nil)
    ((string-equal value "pending")
     ':pending)
    ((or (string-equal value "doing")
         (string-equal value "in_progress")
         (string-equal value "inProgress"))
     ':doing)
    ((or (string-equal value "done")
         (string-equal value "completed"))
     ':done)
    (t
     nil)))

(serapeum/types:-> plan-step-solutions (plan-step knowledge-view) (values list boolean))
(defun plan-step-solutions (step view)
  "Return the ways VIEW satisfies STEP's requirements, and whether the search was cut short."
  (knowledge-view-solve view (plan-step-requires step)))

(serapeum/types:-> plan--unmet-pattern (plan-step knowledge-view) (option list))
(defun plan--unmet-pattern (step view)
  "Return the first of STEP's required patterns no fact in VIEW satisfies with those before it."
  (loop for count from 1 to (length (plan-step-requires step))
        unless (knowledge-view-solve view (subseq (plan-step-requires step) 0 count) :limit 1)
          return (nth (1- count) (plan-step-requires step))))

(serapeum/types:-> plan-step-text-with (plan-step list) string)
(defun plan-step-text-with (step bindings)
  "Return STEP's text with each {?var} placeholder replaced from BINDINGS."
  (let ((text (plan-step-text step)))
    (loop for (variable . term) in bindings
          do (setf text (uiop:frob-substrings text (list (format nil "{~A}" variable))
                                              (knowledge-term-display term))))
    text))

(serapeum/types:-> plan-step-check (plan-step knowledge-view) list)
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
      (list :state      (cond
                          ((null (plan-step-requires step))
                           ':prose)
                          (solution
                           ':ready)
                          (cut-p
                           ':unknown)
                          (t
                           ':blocked))
            :bindings   (first solution)
            :facts      (mapcar #'knowledge-node-identifier (rest solution))
            :conflicted (remove-if-not (lambda (node) (knowledge-node-conflicts node))
                                       (rest solution))
            :unmet      (and (plan-step-requires step) (null solution) (not cut-p)
                             (plan--unmet-pattern step view))
            :changed    (and (eq (plan-step-status step) ':done)
                             (remove-if (lambda (identifier)
                                          (let ((node (knowledge-view-node view identifier)))
                                            (and node (knowledge-node-supported-p node))))
                                        (getf (plan-step-receipt step) :facts)))
            :unverified (and (eq (plan-step-status step) ':done)
                             (plan-step-verify step)
                             (null (knowledge-view-solve view (list (plan-step-verify step))
                                                         :limit 1)))))))

(serapeum/types:-> plan--step-note (plan-step list knowledge-view) (option string))
(defun plan--step-note (step check view)
  "Return the parenthesized note explaining STEP's CHECK, or NIL when none applies."
  (flet ((fact-text (identifier)
           (let ((node     (knowledge-view-node view identifier))
                 (recorded (nth (position identifier (getf (plan-step-receipt step) :facts)
                                          :test #'equal)
                                (getf (plan-step-receipt step) :texts))))
             (cond
               (node
                (knowledge-node-text node))
               (recorded
                recorded)
               (t
                identifier)))))
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

(serapeum/types:-> plan-render ((option workspace-plan) &key (:view (option knowledge-view))) string)
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

(serapeum/types:-> plan-step-snapshot ((option workspace-plan) knowledge-view) list)
(defun plan-step-snapshot (plan view)
  "Return how PLAN's fact-dependent steps read against VIEW, for change detection.

Each entry is (:KEY :INDEX :TEXT :NOTE); prose steps are left out."
  (and plan
       (loop for step in (workspace-plan-steps plan)
             for index from 1
             when (or (plan-step-requires step) (plan-step-verify step))
               collect (let ((check (plan-step-check step view)))
                         (list :key   (or (plan-step-identifier step) (plan-step-text step))
                               :index index
                               :text  (plan-step-text-with
                                       step
                                       (if (eq (plan-step-status step) ':done)
                                           (or (getf (plan-step-receipt step) :bindings)
                                               (getf check :bindings))
                                           (getf check :bindings)))
                               :note  (plan--step-note step check view))))))

(serapeum/types:-> plan-snapshot-changes (list list) list)
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

(serapeum/types:-> plan-steps-with-receipts (list (option workspace-plan) (option knowledge-view)) list)
(defun plan-steps-with-receipts (steps previous view)
  "Return STEPS with a receipt on each structured step newly marked done.

A step keeps the receipt it had under the same id; a step done now records the
bindings and facts satisfying its requirements at this moment, so a later
change to those facts can be reported without repeating the step."
  (mapcar
   (lambda (step)
     (let* ((identifier (getf step :id))
            (old        (and identifier previous
                             (find identifier (workspace-plan-steps previous)
                                   :key #'plan-step-identifier :test #'equal))))
       (cond
         ((not (and (eq (plan--status (getf step :status)) ':done)
                    (or (getf step :requires) (getf step :verify))
                    view))
          step)
         ((and old (eq (plan-step-status old) ':done) (plan-step-receipt old))
          (let ((copy (copy-list step)))
            (setf (getf copy :receipt) (plan-step-receipt old))
            copy))
         (t
          (let ((solution (first (knowledge-view-solve view (getf step :requires) :limit 1)))
                (copy     (copy-list step)))
            (setf (getf copy :receipt)
                  (list :time     (knowledge-now)
                        :bindings (first solution)
                        :facts    (mapcar #'knowledge-node-identifier (rest solution))
                        :texts    (mapcar #'knowledge-node-text (rest solution))))
            copy)))))
   steps))

(serapeum/types:-> knowledge-plan-data->term (t) t)
(defun knowledge-plan-data->term (data)
  "Decode canonical pattern data without evaluating or interning input."
  (cond
    ((hash-table-p data)
     (cond
       ((and (= (hash-table-count data) 1) (gethash "term_keyword" data))
        (or (find (gethash "term_keyword" data)
                  '(:not :true :false :instant :date :duration :quantity)
                  :test #'equal :key (lambda (item) (string-downcase (symbol-name item))))
            (knowledge--fail ':invalid-term data "Unknown canonical term tag.")))
       ((and (= (hash-table-count data) 1) (gethash "term_ratio" data))
        (let ((ratio (gethash "term_ratio" data)))
          (unless (and (simple-vector-p ratio) (= (length ratio) 2)
                       (every #'integerp ratio) (plusp (aref ratio 1)))
            (knowledge--fail ':invalid-term data "Invalid canonical ratio."))
          (/ (aref ratio 0) (aref ratio 1))))
       (t
        (knowledge--fail ':invalid-term data "Invalid canonical term object."))))
    ((and (vectorp data) (not (stringp data)))
     (map 'list #'knowledge-plan-data->term data))
    (t
     data)))

(serapeum/types:-> knowledge-plan-term->data (t) t)
(defun knowledge-plan-term->data (value)
  "Encode canonical pattern terms as bounded wire data."
  (cond
    ((null value)
     #())
    ((listp value)
     (map 'vector #'knowledge-plan-term->data value))
    ((keywordp value)
     (argo:json-object "term_keyword" (string-downcase (symbol-name value))))
    ((and (rationalp value) (not (integerp value)))
     (argo:json-object "term_ratio" (vector (numerator value) (denominator value))))
    (t
     value)))
