(in-package #:autolith)

;;;; -- Skill Selection Tool --

(defclass skill-load-result (tool-result)
  ((name
    :initarg :name
    :reader skill-load-result-name
    :type string
    :documentation "The exact selected Skill name shown to the user.")
   (newly-selected-p
    :initarg :newly-selected-p
    :reader skill-load-result-newly-selected-p
    :type boolean
    :documentation "True when this call selected the Skill for the first time."))
  (:documentation
   "A request-local Skill selection result with presentation metadata."))

(defmethod tool-result-details ((result skill-load-result))
  "Return bounded metadata for presenting one Skill selection."
  (list :kind ':skill-load
        :name (skill-load-result-name result)
        :newly-selected-p (skill-load-result-newly-selected-p result)))

(defclass skill-load-tool (tool)
  ()
  (:documentation
   "Select one discovered Autolith skill for the active logical turn."))

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
  "Return the exact skill name supplied in ARGUMENTS."
  (let ((name (tool-argument arguments "name" :required t)))
    (unless (and (stringp name) (plusp (length name)))
      (error 'tool-error
             :message "skill.load name must be a non-empty string."
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
      (make-instance
       'skill-load-result
       :name name
       :newly-selected-p newly-selected-p
       :content
       (bounded-string
        (if newly-selected-p
            (format nil
                    "Selected skill ~A for this logical turn. Autolith will inject its current :instructions string ephemerally into subsequent provider requests in this turn."
                    name)
            (format nil
                    "Skill ~A is already selected for this logical turn. Its current :instructions string remains available ephemerally."
                    name)))
       :success-p t))))

(-> skill-augment-tool-registry (tool-registry) tool-registry)
(defun skill-augment-tool-registry (registry)
  "Register Autolith's native request-local skill selector in REGISTRY."
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
  registry)
