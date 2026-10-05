(in-package #:autolith)

;;;; -- Refinement Operation --

(defclass refinement-tool (mutable-self-tool) ()
  (:documentation "The unified proposal, experiment and scoped promotion lifecycle."))

(defmethod tool-execution-policy ((tool refinement-tool))
  "Serialize refinement effects with other tool mutations."
  (declare (ignore tool))
  ':exclusive)

(-> refinement--choice (t list) keyword)
(defun refinement--choice (name choices)
  "Resolve an explicit supported keyword without interning model input."
  (or (and (stringp name) (find name choices :key #'symbol-name :test #'string-equal))
      (refinement--reject ':invalid "Unsupported refinement choice.")))

(-> refinement-register-tools (tool-registry) tool-registry)
(defun refinement-register-tools (registry)
  "Register self.refine. Task judgment is local, not a model-facing verdict tool."
  (tool-registry-register
   registry
   (make-instance
    'refinement-tool :namespace "self" :name "refine"
    :description
    "Unify trajectory lessons, memories, context guidance, skills, roles, RLM policies and exploratory mutations. Propose the smallest useful target or none. Execute existing owner tools with explicit authority; evaluate records actual task observations. A trusted local reviewer or /refine command must accept efficacy before promotion session -> project -> global. Owner mechanical replay checks remain separate."
    :parameters
    (tool-object-schema
     (json-object
      "action" (cl-llm-provider-api:provider-enum-schema
                '("status" "propose" "experiment" "evaluate" "promote" "recover")
                "The explicit lifecycle transition.")
      "id" (tool-string-property "Existing proposal identifier.")
      "revision" (tool-integer-property "Observed current proposal revision.")
      "target" (cl-llm-provider-api:provider-enum-schema
                '("none" "memory" "repository-fact" "context" "skill" "role" "rlm-policy" "live-change")
                "Smallest useful adaptation, or none for a normal no-op.")
      "evidence" (json-object "type" "array" "items" (tool-string-property "Trajectory evidence or reference.")
                              "maxItems" 16 "description" "Concrete evidence for a proposal.")
      "reason" (tool-string-property "Why this target is useful and less invasive targets are insufficient; or why there is no useful lesson.")
      "benefit" (tool-string-property "Expected task-level improvement.")
      "criterion" (tool-string-property "The task-level measurement and acceptance criterion, separate from replay survival.")
      "operation" (tool-string-property "JSON owner call with namespace, name and JSON-encoded arguments. Uses existing tool authority; no secrets.")
      "guard" (tool-string-property "JSON read-only owner call capturing the actual adaptation. Required for experiment; optionally rebind after promotion.")
      "scope" (cl-llm-provider-api:provider-enum-schema '("project" "global") "Exactly the next promotion scope.")
      "recovery-evidence" (tool-string-property "Actual inspection/recovery performed through the original owner."))
     '("action"))))
  registry)

(defmethod tool-execute ((tool refinement-tool) (context tool-context) (arguments hash-table))
  "Dispatch lifecycle decisions; never accept a model-provided efficacy verdict."
  (declare (ignore tool))
  (handler-case
      (let* ((action (refinement--choice (tool-argument arguments "action" :required t)
                                         '(:status :propose :experiment :evaluate :promote :recover)))
             (configuration (tool-context-configuration context))
             (identifier (tool-argument arguments "id")))
        (when (resource-context-child-agent-p context)
          (refinement--reject ':authority "Refinement belongs to the primary agent."))
        (tool-success
         (let ((*print-readably* nil))
           (prin1-to-string
            (case action
              (:status (if identifier (refinement-find configuration identifier)
                           (refinement-list configuration)))
              (:propose
               (refinement-propose
                configuration :owner (conversation-identifier (tool-context-conversation context))
                :target (refinement--choice (tool-argument arguments "target" :required t)
                                            '(:none :memory :repository-fact :context :skill :role :rlm-policy :live-change))
                :evidence (coerce (tool-argument arguments "evidence" :required t) 'list)
                :reason (tool-argument arguments "reason" :required t)
                :benefit (tool-argument arguments "benefit")
                :criterion (tool-argument arguments "criterion")))
              (:recover
               (refinement-recover context :identifier (tool-argument arguments "id" :required t)
                                   :revision (tool-argument arguments "revision" :required t)
                                   :evidence (tool-argument arguments "recovery-evidence" :required t)))
              (otherwise
               (refinement-run
                context :identifier (tool-argument arguments "id" :required t)
                :revision (tool-argument arguments "revision" :required t)
                :operation (tool-argument arguments "operation" :required t)
                :guard (tool-argument arguments "guard") :phase action
                :scope (and (tool-argument arguments "scope")
                            (refinement--choice (tool-argument arguments "scope") '(:project :global))))))))))
    (refinement-error (condition)
      (tool-failure (princ-to-string condition)
                    :code (refinement-error-code condition)))))

;;;; -- Local Task Assessment --

(-> application-refinement-command (application string) null)
(defun application-refinement-command (application remainder)
  "Show /refine state, or accept (:assess ID REVISION VERDICT EVIDENCE) from the user.
Only the local command supplies the reviewer closure. The model-facing operation
can request observations and promotion, but cannot manufacture accepted efficacy."
  (handler-case
      (let ((input (string-trim '(#\Space #\Tab #\Newline #\Return) remainder)))
        (if (zerop (length input))
            (application-present application
                                 (prin1-to-string (refinement-list (application-configuration application))))
            (let ((fields (read-one-form input :read-eval nil :package (find-package '#:autolith))))
              (unless (and (listp fields) (= (length fields) 5) (eq (first fields) :assess)
                           (non-empty-string-p (second fields)) (integerp (third fields))
                           (member (fourth fields) '(:passed :failed))
                           (non-empty-string-p (fifth fields)))
                (refinement--reject ':invalid "Use (:assess \"ID\" REVISION :passed-or-failed \"TASK EVIDENCE\")."))
              (application-present
               application
               (prin1-to-string
                (refinement-assess
                 (application-operation--tool-context application)
                 :identifier (second fields) :revision (third fields)
                 :reviewer (lambda (proposal)
                             (declare (ignore proposal))
                             (values (fourth fields) (fifth fields)))))))))
    (refinement-error (condition) (application-present application (princ-to-string condition))))
  nil)
