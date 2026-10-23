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
    "Unify trajectory lessons, memories, context guidance, skills, roles, RLM policies and exploratory mutations. Propose the smallest useful target or none. Execute existing owner tools with explicit authority; evaluate records actual task observations. A trusted local reviewer or the (refine) command must accept efficacy before promotion session -> project -> global. Owner mechanical replay checks remain separate."
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

(-> application-refinement-slash-arguments (string) list)
(defun application-refinement-slash-arguments (remainder)
  "Translate the legacy local assessment form only at the slash boundary."
  (if (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) remainder)))
      nil
      (let ((fields (handler-case
                        (read-one-form remainder :read-eval nil :package (find-package '#:autolith))
                      (error ()
                        (refinement--reject ':invalid "Expected exactly one assessment data form.")))))
        (unless (and (proper-list-p fields) (= (length fields) 5)
                     (eq (first fields) ':assess))
          (refinement--reject ':invalid "Expected (:assess ID REVISION VERDICT EVIDENCE)."))
        (list ':assess (second fields) :revision (third fields)
              :verdict (fourth fields) :evidence (fifth fields)))))

(-> application-refinement-command (application &optional t t &rest t) null)
(defun application-refinement-command (application &optional action identifier &rest options)
  "List proposals or supply a native local task assessment through the reviewer."
  (unless (or (and (null action) (null identifier) (null options))
              (and (eq action ':assess) (non-empty-string-p identifier)
                   (= (length options) 6)
                   (every (lambda (key) (member key '(:revision :verdict :evidence)))
                          (loop for (key value) on options by #'cddr collect key))
                   (integerp (getf options :revision)) (plusp (getf options :revision))
                   (member (getf options :verdict) '(:passed :failed))
                   (non-empty-string-p (getf options :evidence))))
    (refinement--reject ':invalid "Use (refine :assess ID :revision N :verdict :passed-or-failed :evidence TEXT)."))
  (application-present
   application
   (prin1-to-string
    (if (null action)
        (refinement-list (application-configuration application))
        (refinement-assess
         (application-operation--tool-context application)
         :identifier identifier :revision (getf options :revision)
         :reviewer (lambda (proposal)
                     (declare (ignore proposal))
                     (values (getf options :verdict) (getf options :evidence)))))))
  nil)

(define-application-command application--builtin-refine-command
    (:name "/refine"
     :argument "[(:assess ID REVISION VERDICT EVIDENCE)]"
     :description "inspect refinement proposals or assess task efficacy"
     :tip "records a local task-level assessment before scoped promotion."
     :busy-behavior :hold
     :terminal-behavior :shared
     :slash-parser application-refinement-slash-arguments
     :callable t)
    (application &optional action identifier &rest options
                 &key revision verdict evidence &allow-other-keys)
  (declare (ignore revision verdict evidence))
  (apply #'application-refinement-command application action identifier options)
  ':continue)
