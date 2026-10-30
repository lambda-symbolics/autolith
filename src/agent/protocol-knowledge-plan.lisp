(in-package #:autolith)

;;;; -- Parent-Checked Workspace Plans --

(-> agent-knowledge-plan--steps ((option workspace-plan)) simple-vector)
(defun agent-knowledge-plan--steps (plan)
  "Send public plan data and opaque parent receipt tokens, never graph paths."
  (map 'vector
       (lambda (step)
         (let ((data (json-object "text" (plan-step-text step)
                                  "status" (string-downcase (symbol-name (plan-step-status step)))
                                  "requires" (knowledge-plan-term->data (plan-step-requires step))
                                  "verify" (knowledge-plan-term->data (plan-step-verify step)))))
           (when (plan-step-identifier step)
             (setf (gethash "id" data) (plan-step-identifier step)))
           (when (getf (plan-step-receipt step) :parent-token)
             (setf (gethash "receipt" data) (getf (plan-step-receipt step) :parent-token)))
           data))
       (and plan (workspace-plan-steps plan))))

(-> agent-knowledge-plan--request
    (configuration string &key (:plan (option workspace-plan)) (:arguments (option hash-table))
     (:active boolean) (:turn (option integer))) hash-table)
(defun agent-knowledge-plan--request (configuration action &key plan arguments active turn)
  "Ask the parent to check, render, or record the public workspace plan."
  (let* ((scope  (agent-knowledge-context-scope configuration))
         (fields (json-object "action" action "canonical" (not arguments)
                              "steps" (if arguments (gethash "steps" (agent-context--data arguments))
                                          (agent-knowledge-plan--steps plan))
                              "active" active)))
    (when arguments
      (setf (gethash "previous" fields) (agent-knowledge-plan--steps plan)))
    (let ((explanation (if arguments (gethash "explanation" arguments)
                          (and plan (workspace-plan-explanation plan)))))
      (when (stringp explanation) (setf (gethash "explanation" fields) explanation)))
    (when turn (setf (gethash "turn" fields) turn))
    (let* ((adapter (and scope (agent-record-resource-scope-knowledge scope)))
           (data    (if adapter
                        (agent-protocol-knowledge-response-fields
                         (agent-protocol-knowledge-request adapter ':read :scope ':workspace
                                                           :view "plan" :fields fields))
                        (error 'agent-protocol-knowledge-error :code ':operation-unsupported))))
      (unless (and (stringp (gethash "rendered" data))
                   (<= (length (gethash "rendered" data)) 131072)
                   (simple-vector-p (gethash "steps" data))
                   (<= (length (gethash "steps" data)) *plan-maximum-steps*)
                   (simple-vector-p (gethash "changes" data))
                   (<= (length (gethash "changes" data)) *plan-maximum-steps*)
                   (every (lambda (line) (and (stringp line) (<= (length line) 131072)))
                          (gethash "changes" data)))
        (error 'agent-protocol-knowledge-error :code ':invalid-response))
      data)))

(-> agent-knowledge-plan--decoded-steps (simple-vector) list)
(defun agent-knowledge-plan--decoded-steps (raw)
  "Decode checked step data while retaining only opaque receipt tokens."
  (map 'list
       (lambda (step)
         (unless (and (hash-table-p step) (stringp (gethash "text" step))
                      (plan--status (gethash "status" step))
                      (or (null (gethash "id" step)) (stringp (gethash "id" step)))
                      (or (null (gethash "receipt" step)) (stringp (gethash "receipt" step))))
           (error 'agent-protocol-knowledge-error :code ':invalid-response))
         (list :text (gethash "text" step) :status (gethash "status" step) :id (gethash "id" step)
               :requires (knowledge-plan-data->term (gethash "requires" step #()))
               :verify   (knowledge-plan-data->term (gethash "verify" step #()))
               :receipt  (when (gethash "receipt" step)
                           (list :parent-token (gethash "receipt" step)))))
       raw))

(defmethod knowledge-plan-tool-remote ((context tool-context) (arguments hash-table) action)
  "Run normal list/update tools through the parent's graph authority."
  (let* ((configuration (tool-context-configuration context))
         (scope         (agent-knowledge-context-scope configuration)))

    (when (and scope (not (agent-record-resource-scope-knowledge scope)))
      (when (ecase action
              (:list
               (plan-tool--structured-p (plan-load configuration)))
              (:update
               (let ((steps (gethash "steps" arguments)))
                 (and (vectorp steps)
                      (some (lambda (step)
                              (and (hash-table-p step)
                                   (or (gethash "requires" step) (gethash "verify" step)))) steps)))))
        (error 'agent-protocol-knowledge-error :code ':operation-unsupported))
      (return-from knowledge-plan-tool-remote (values nil nil)))
    (if scope
        (handler-case
            (let* ((previous (plan-load configuration))
                   (data     (agent-knowledge-plan--request
                              configuration (ecase action (:list
                                                           "render") (:update
                                                                      "prepare"))
                              :plan previous :arguments (and (eq action ':update) arguments)))
                   (steps    (and (eq action ':update)
                                  (agent-knowledge-plan--decoded-steps (gethash "steps" data)))))
              (when (eq action ':update)
                (if steps
                    (plan-update configuration steps
                                 :explanation (let ((value (gethash "explanation" arguments)))
                                                (and (stringp value) value)))
                    (plan-clear configuration)))
              (when (eq action ':list)
                (agent-knowledge-plan--request configuration "delivered" :plan previous))
              (values (tool-success (gethash "rendered" data)) t))
          (plan-error (condition)
            (error 'tool-error :message   (format nil "~A" condition)
                               :tool-name (if (eq action ':list) "plan.list" "plan.update"))))
        (values nil nil))))

(defmethod knowledge-plan-delivered-remote
    ((configuration configuration) (conversation conversation) plan)
  "Record plan delivery in the parent's durable viewer state."
  (declare (ignore conversation))
  (let ((scope (agent-knowledge-context-scope configuration)))
    (when scope
      (when (agent-record-resource-scope-knowledge scope)
        (agent-knowledge-plan--request configuration "delivered" :plan plan))
      t)))

(defmethod knowledge-plan-context-remote ((request request-context))
  "Use parent plan changes without consulting a local graph, in every mode."
  (let* ((configuration (request-context-configuration request))
         (scope         (agent-knowledge-context-scope configuration)))
    (if scope
        (values
         (when (and (agent-record-resource-scope-knowledge scope)
                    (not (request-context-compaction-p request)) (knowledge-active-p configuration))
           (let* ((conversation (request-context-conversation request))
                  (identifier   (conversation-identifier conversation))
                  (turn         (memory-context--turn-key conversation))
                  (cached       (gethash identifier *plan-context-turn-changes*)))
             (if (and cached (eql (first cached) turn))
                 (rest cached)
                 (let* ((data    (agent-knowledge-plan--request configuration "changes"
                                                                :plan (plan-load configuration)
                                                                :active t :turn turn))
                        (changes (gethash "changes" data))
                        (advice  (when (plusp (length changes))
                                   (change-class
                                    (make-context-contribution
                                     :identifier  "plan-changes"
                                     :instruction "Plan steps that depend on facts changed since you last saw the plan. Adjust the plan or the work in progress accordingly."
                                     :evidence    (format nil "~{~A~^~%~}" (coerce changes 'list))
                                     :priority 30 :lifetime ':next-request :deduplication-key "plan-changes")
                                    'plan-change-contribution :count (length changes)))))
                   (setf (gethash identifier *plan-context-turn-changes*) (cons turn advice))
                   advice))))
         t)
        (values nil nil))))
