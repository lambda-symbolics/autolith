(in-package #:autolith)

;;;; -- Mission Review Attachment and Decisions --

(defclass mission-review-tool (tool) ()
  (:documentation "Primary-only optional mission review configuration and decisions."))

(-> mission-review--application (tool-context) application)
(defun mission-review--application (context)
  "Require the owning primary application, excluding delegated agents."
  (let* ((mission (mission-context-find (tool-context-conversation context)))
         (application (and mission (mission-context-application mission))))
    (unless (and application
                 (eq (tool-context-agent context) (application-agent application))
                 (not (typep (tool-context-agent context) 'task-child-agent)))
      (mission--reject ':review-authority "Mission review is owned by the primary agent."))
    application))

(defmethod tool-execute ((tool mission-review-tool) (context tool-context) arguments)
  "Execute review operations without granting a reviewer decision authority."
  (let* ((application (mission-review--application context))
         (operation (tool-name tool))
         (result
           (cond
             ((string= operation "configure")
              (task--validate-tool-arguments arguments
                                            '("id" "phase" "inputs" "context" "models" "turn-limit" "token-limit" "run-limit")
                                            "mission-review.configure")
              (application-mission-review-configure
               application
               (list :id (tool-argument arguments "id")
                     :phase (let ((phase (tool-argument arguments "phase")))
                              (cond ((equal phase "before-integration") ':before-integration)
                                    ((equal phase "final-acceptance") ':final-acceptance)
                                    ((equal phase "after-edit-set") ':after-edit-set)))
                     :inputs (coerce (or (tool-argument arguments "inputs") #()) 'list)
                     :context (tool-argument arguments "context")
                     :models (coerce (or (tool-argument arguments "models") #()) 'list)
                     :turn-limit (tool-argument arguments "turn-limit")
                     :token-limit (tool-argument arguments "token-limit")
                     :run-limit (tool-argument arguments "run-limit"))))
             ((string= operation "trigger")
              (task--validate-tool-arguments arguments '("id") "mission-review.trigger")
              (application-mission-review-trigger application (tool-argument arguments "id")
                                                  :tool-context context))
             ((string= operation "decide")
              (task--validate-tool-arguments arguments '("id" "finding" "decision" "reason") "mission-review.decide")
              (application-mission-review-decide
               application (tool-argument arguments "id") (tool-argument arguments "finding")
               :agent (tool-context-agent context)
               :decision (cond ((equal (tool-argument arguments "decision") "resolve") ':resolve)
                               ((equal (tool-argument arguments "decision") "reject") ':reject))
               :reason (tool-argument arguments "reason")))
             ((string= operation "inspect")
              (task--validate-tool-arguments arguments nil "mission-review.inspect")
              (copy-tree (getf (application-goal application) :reviews)))
             (t (mission--reject ':invalid "Unknown mission review operation.")))))
    (task-tool-result (task--write-readable-sexp result :pretty-p t) result)))

(-> mission-review-augment-tool-registry (tool-registry) tool-registry)
(defun mission-review-augment-tool-registry (registry)
  "Register explicit primary mission reviewer operations in REGISTRY."
  (tool-registry-describe-namespace registry "mission-review" "Optional independent reviewer checkpoints and primary decisions.")
  (let* ((id (tool-string-property "Attached checkpoint ID."))
         (strings (json-object "type" "array" "items" (json-object "type" "string")))
         (specifications
           (list
            (list "configure" "Attach an optional explicitly budgeted independent review checkpoint."
                  (tool-object-schema
                   (json-object "id" id "phase" (json-object "type" "string" "enum" #("before-integration" "final-acceptance" "after-edit-set"))
                                "inputs" strings "context" (tool-string-property "Independent review background.")
                                "models" strings "turn-limit" (tool-integer-property "Total reviewer call allowance.")
                                "token-limit" (tool-integer-property "Total reviewer billable token allowance.")
                                "run-limit" (tool-integer-property "Maximum changed snapshots to review, 1 to 32."))
                   '("id" "phase" "inputs" "context" "turn-limit" "token-limit" "run-limit")))
            (list "trigger" "Review one changed checkpoint through an actual contracted task; unchanged runs are reused."
                  (tool-object-schema (json-object "id" id) '("id")))
            (list "decide" "Retain a primary resolve/reject decision and reason for a pending advisory finding."
                  (tool-object-schema (json-object "id" id "finding" (tool-string-property "Finding ID.")
                                                  "decision" (json-object "type" "string" "enum" #("resolve" "reject"))
                                                  "reason" (tool-string-property "Decision evidence or reason."))
                                      '("id" "finding" "decision" "reason")))
            (list "inspect" "Inspect durable reviewer budgets, snapshots, findings and decisions."
                  (tool-object-schema (json-object) nil)))))
    (dolist (specification specifications)
      (tool-registry-register registry
                              (make-instance 'mission-review-tool :namespace "mission-review"
                                             :name (first specification) :description (second specification)
                                             :parameters (third specification)))))
  registry)
