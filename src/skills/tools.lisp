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
    (tool-registry-describe-namespace
     registry "skill"
     "Request-local loading of discovered Autolith Skills.")
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
  (skill-edit-augment-tool-registry registry)
  registry)


;;;; -- Skill Authoring Tool --

(defparameter *skill-edit-maximum-content-characters* (* 256 1024)
  "The maximum source text accepted by one skill.edit call.")

(defclass skill-edit-tool (tool) ()
  (:documentation "Create or replace one validated global Autolith skill."))

(-> skill-edit-tool--name-valid-p (string) boolean)
(defun skill-edit-tool--name-valid-p (name)
  "Return true when NAME follows the portable Autolith skill-name grammar."
  (and (<= 1 (length name) 64)
       (not (char= (char name 0) #\-))
       (not (char= (char name (1- (length name))) #\-))
       (not (search "--" name))
       (every (lambda (character)
                (or (and (char>= character #\a) (char<= character #\z))
                    (and (char>= character #\0) (char<= character #\9))
                    (char= character #\-)))
              name)))

(-> skill-edit-tool--validate (configuration pathname string string) null)
(defun skill-edit-tool--validate (configuration root name content)
  "Validate CONTENT through the same catalog discovery used at skill load time."
  (let* ((probe-root (merge-pathnames (format nil "skill-edit-~A/" (gensym))
                                      (configuration-cache-root configuration)))
         (probe-file (merge-pathnames (format nil "~A/SKILL.md" name) probe-root)))
    (unwind-protect
         (progn
           (ensure-directories-exist probe-file)
           (with-open-file (stream probe-file
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :external-format :utf-8)
             (write-string content stream))
           (let* ((catalog (skill-catalog-discover (list probe-root)
                                                   :cache-root root))
                  (metadata (skill-catalog-find catalog name))
                  (diagnostics (skill-catalog-diagnostics catalog)))
             (unless metadata
               (error 'tool-error
                      :tool-name "skill.edit"
                      :message
                      (if diagnostics
                          (format nil "Skill validation failed: ~{~A~^; ~}"
                                  (mapcar #'skill-diagnostic-message diagnostics))
                          "Skill validation failed: required name and description frontmatter are missing or invalid.")))))
      (when (probe-file probe-root)
        (platform-delete-directory-tree *platform* probe-root
                                        :validate t
                                        :if-does-not-exist ':ignore)))))

(defmethod tool-execute ((tool skill-edit-tool) (context tool-context) (arguments hash-table))
  "Validate then atomically replace one global SKILL.md source file."
  (declare (ignore tool))
  (let* ((name (skill-load-tool--name arguments))
         (content (tool-argument arguments "content" :required t))
         (configuration (tool-context-configuration context))
         (root (skill-global-root configuration)))
    (unless (skill-edit-tool--name-valid-p name)
      (error 'tool-error
             :tool-name "skill.edit"
             :message "skill.edit name must be 1-64 lowercase letters, digits, or single internal hyphens."))
    (unless (and (stringp content)
                 (<= (length content) *skill-edit-maximum-content-characters*))
      (error 'tool-error
             :tool-name "skill.edit"
             :message "skill.edit content must be a string no larger than 262144 characters."))
    (skill-edit-tool--validate configuration root name content)
    (let ((pathname (merge-pathnames (format nil "~A/SKILL.md" name) root)))
      (ensure-directories-exist pathname)
      (with-open-file (stream pathname
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format :utf-8)
        (write-string content stream))
      (make-instance 'tool-result
                     :success-p t
                     :content (format nil "Validated and wrote global skill ~A at ~A."
                                      name (namestring pathname))))))


(-> skill-edit-augment-tool-registry (tool-registry) tool-registry)
(defun skill-edit-augment-tool-registry (registry)
  "Register global skill authoring independently of skill.load availability."
  (unless (tool-registry-find registry "skill" "edit")
    (tool-registry-describe-namespace
     registry "skill" "Request-local loading and global authoring of Autolith Skills.")
    (tool-registry-register
     registry
     (make-instance
      'skill-edit-tool
      :namespace "skill"
      :name "edit"
      :description "Create or replace a global SKILL.md after runtime-equivalent validation."
      :parameters
      (tool-object-schema
       (json-object
        "name" (tool-string-property "New or existing global skill name.")
        "content" (tool-string-property "Complete SKILL.md content, including frontmatter."))
       '("name" "content")))))
  registry)
