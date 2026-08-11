(in-package #:autolith)

;;;; -- Skill Selection Tool --

(defclass skill-load-tool (tool)
  ()
  (:documentation
   "Select one discovered Autolith skill for the active logical turn."))

(defclass skill-run-tool (tool)
  ()
  (:documentation
   "Execute one discovered Skill's declared Common Lisp workflow."))

(-> skill-workflow-execute (tool-context skill-metadata) tool-result)
(defgeneric skill-workflow-execute (context metadata)
  (:documentation
   "Execute METADATA's declared workflow through CONTEXT's tool surface."))

(defmethod tool-child-safe-p ((tool skill-load-tool))
  "Permit child agents to select skills from their own request context."
  (declare (ignore tool))
  t)

(defmethod tool-conversation-persistence ((tool skill-load-tool))
  "Keep skill selection calls only through their next provider response."
  (declare (ignore tool))
  ':next-response)

(defmethod tool-provider-round-trip-barrier-p ((tool skill-load-tool))
  "Require a provider round trip before any action may follow skill selection."
  (declare (ignore tool))
  t)

(-> skill-load-tool--name (json-object) string)
(defun skill-load-tool--name (arguments)
  "Return the exact valid skill name supplied in ARGUMENTS."
  (let ((name (tool-argument arguments "name" :required t)))
    (unless (skill--valid-name-p name)
      (error 'tool-error
             :message
             (format nil
                     "skill.load name must be at most ~D characters and contain only lowercase ASCII letters, digits, and nonconsecutive interior hyphens."
                     *skill-name-character-limit*)
             :tool-name "skill.load"))
    name))

(defmethod tool-execute
    ((tool skill-load-tool) (context tool-context) (arguments hash-table))
  "Select one exact skill name without putting its instruction body in history."
  (declare (ignore tool))
  (let ((name (skill-load-tool--name arguments)))
    (multiple-value-bind (metadata newly-selected-p)
        (skill-select-for-logical-turn
         (tool-context-configuration context)
         name)
      (declare (ignore metadata))
      (tool-success
       (if newly-selected-p
           (format nil
                   "Selected skill ~A for this logical turn. Autolith will inject its current :instructions string ephemerally into subsequent provider requests in this turn."
                   name)
           (format nil
                   "Skill ~A is already selected for this logical turn. Its current :instructions string remains available ephemerally."
                   name))))))

(defmethod tool-execute
    ((tool skill-run-tool) (context tool-context) (arguments hash-table))
  "Discover and execute one exact workflow Skill."
  (declare (ignore tool))
  (let* ((name (skill-load-tool--name arguments))
         (catalog
           (skill-catalog-for-configuration
            (tool-context-configuration context)))
         (metadata (skill-catalog-find catalog name)))
    (unless metadata
      (error 'tool-error
             :message (format nil "No discovered Skill is named ~S." name)
             :tool-name "skill.run"))
    (unless (skill-metadata-workflow metadata)
      (error 'tool-error
             :message (format nil "Skill ~A does not declare a workflow." name)
             :tool-name "skill.run"))
    (skill-workflow-execute context metadata)))

(-> skill-augment-tool-registry (tool-registry) tool-registry)
(defun skill-augment-tool-registry (registry)
  "Register Autolith's native Skill selection and workflow tools in REGISTRY."
  (unless (tool-registry-find registry "skill" "load")
    (tool-registry-register
     registry
     (make-instance
      'skill-load-tool
      :namespace "skill"
      :name "load"
      :description
      "Select one discovered Autolith skill by exact name. Use this when a request names a skill or matches catalog metadata instead of reading SKILL.sexp; Autolith injects only the complete current :instructions string ephemerally into subsequent provider requests in the logical turn."
      :parameters
      (tool-object-schema
       (json-object
        "name"
        (tool-string-property
         "The exact case-sensitive name from the request's Skills catalog."))
       '("name")))))
  (unless (tool-registry-find registry "skill" "run")
    (tool-registry-register
     registry
     (make-instance
      'skill-run-tool
      :namespace "skill"
      :name "run"
      :description
      "Execute the Common Lisp workflow declared by a discovered Skill. The workflow can branch with ordinary Lisp and call the same registered operations available to the primary agent."
      :parameters
      (tool-object-schema
       (json-object
        "name"
        (tool-string-property
         "The exact case-sensitive name of a discovered workflow Skill."))
       '("name")))))
  registry)
