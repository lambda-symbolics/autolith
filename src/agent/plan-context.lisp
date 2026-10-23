(in-package #:autolith)

;;;; -- Plan Change Advice --

;;; When facts a plan step depends on change between turns, the model is told
;;; once which steps now read differently, are blocked, or were done on facts
;;; that stopped holding. Plan tool results count as delivery, so a change the
;;; model already saw in its own plan.update or plan.list is not repeated.

(defclass plan-change-contribution (context-contribution)
  ((count :initarg :count
          :reader plan-change-contribution-count
          :type (integer 1)
          :documentation "How many plan steps changed."))
  (:documentation "Advice explaining plan steps changed by their facts."))

(defclass plan-change-event (provider-event)
  ((count :initarg :count
          :reader plan-change-event-count
          :type (integer 1)
          :documentation "How many changed plan steps were explained to the model."))
  (:documentation "A presentation event for delivered plan change advice."))

(defmethod context--copy-contribution :around
    ((contribution plan-change-contribution) contributor source)
  "Preserve the change count when attaching contribution provenance."
  (change-class (call-next-method) 'plan-change-contribution
                :count (plan-change-contribution-count contribution)))

(defmethod context-contribution-provider-event ((contribution plan-change-contribution))
  "Project the number of changed steps without their contents."
  (make-instance 'plan-change-event :count (plan-change-contribution-count contribution)))

(defvar *plan-context-turn-changes* (make-hash-table :test #'equal :synchronized t)
  "Each conversation's plan change advice for its current turn, computed once.")

(-> plan-context--viewer (conversation) string)
(defun plan-context--viewer (conversation)
  "Return the viewer name recording which plan state CONVERSATION last saw."
  (format nil "plan:~A" (conversation-identifier conversation)))

(-> plan-context-note-delivered
    (configuration conversation (option workspace-plan) (option knowledge-view))
    null)
(defun plan-context-note-delivered (configuration conversation plan view)
  "Record that CONVERSATION's model just saw PLAN as checked against VIEW."
  (when (and plan view)
    (knowledge-viewer-update configuration (plan-context--viewer conversation)
                             (knowledge-view-workspace view) (knowledge-view-now view)
                             (lambda (previous time first-p)
                               (declare (ignore previous time first-p))
                               (values (plan-step-snapshot plan view) nil))))
  nil)

(-> plan-changes-context (request-context) (option context-contribution))
(defun plan-changes-context (request)
  "Explain once per turn which fact-dependent plan steps changed since the model saw them."
  (let ((configuration (request-context-configuration request))
        (conversation (request-context-conversation request)))
    (when (and (not (request-context-compaction-p request))
               (knowledge-active-p configuration))
      (let* ((identifier (conversation-identifier conversation))
             (turn (memory-context--turn-key conversation))
             (cached (gethash identifier *plan-context-turn-changes*)))
        (if (and cached (eql (first cached) turn))
            (rest cached)
            (let* ((plan (plan-load configuration))
                   (advice
                     (when (plan-tool--structured-p plan)
                       (let ((view (knowledge-view-current configuration)))
                         (knowledge-viewer-update
                          configuration (plan-context--viewer conversation)
                          (knowledge-view-workspace view) (knowledge-view-now view)
                          (lambda (previous time first-p)
                            (declare (ignore time))
                            (let* ((current (plan-step-snapshot plan view))
                                   (changes (and (not first-p)
                                                 (plan-snapshot-changes previous current))))
                              (values current
                                      (and changes
                                           (change-class
                                            (make-context-contribution
                                             :identifier "plan-changes"
                                             :instruction
                                             "Plan steps that depend on facts changed since you last saw the plan. Adjust the plan or the work in progress accordingly."
                                             :evidence (format nil "~{~A~^~%~}" changes)
                                             :priority 30
                                             :lifetime ':next-request
                                             :deduplication-key "plan-changes")
                                            'plan-change-contribution
                                            :count (length changes)))))))))))
              (setf (gethash identifier *plan-context-turn-changes*) (cons turn advice))
              advice))))))

(register-context-contributor "plan-changes" 'plan-changes-context :source ':built-in)
