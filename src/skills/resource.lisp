(in-package #:autolith)

;;;; -- Skill Description Resources --

;;; The catalog shortens a long description. skill:NAME returns the whole
;;; description of one skill, and reading it never selects the skill.

(defclass skill-resource (read-only-resource)
  ((metadata
    :initarg       :metadata
    :reader        skill-resource-metadata
    :type          skill-metadata
    :documentation "The discovered skill this resource describes."))
  (:documentation "The read-only whole description of one discovered skill."))

(defclass skill-resolver (resource-resolver)
  ()
  (:documentation "Resolve skill:NAME to a skill of the caller's catalog."))

(defmethod resource-resolver-child-safe-p ((resolver skill-resolver) context)
  "Let a child agent read the descriptions of the skills it may load."
  (declare (ignore resolver context))
  t)

(defmethod resource-resolver-read-documentation ((resolver skill-resolver))
  "Document skill: reads."
  (declare (ignore resolver))
  "skill:NAME returns the whole description of one skill in the Skills catalog.")

(defmethod resource-resolver-resolve
    ((resolver skill-resolver) identifier (context tool-context))
  "Resolve IDENTIFIER as the exact name of a skill discovered for CONTEXT."
  (let ((metadata (skill-catalog-find
                   (skill-catalog-for-configuration (tool-context-configuration context))
                   identifier)))
    (unless metadata
      (error 'tool-error
             :message   (format nil "No discovered skill has the exact case-sensitive name ~S."
                                identifier)
             :tool-name "resource.read"))
    (make-instance 'skill-resource
                   :uri      (format nil "~A:~A" (resource-resolver-scheme resolver) identifier)
                   :metadata metadata)))

(defmethod resource-tool-read
    ((resource skill-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return the whole description of RESOURCE's skill without selecting it."
  (declare (ignore tool context arguments))
  (tool-success
   (format nil "URI: ~A~%Description: ~A"
           (resource-uri resource)
           (skill-metadata-description (skill-resource-metadata resource)))))


;;;; -- Global Skill Source Resources --

(defclass skill-source-resource (workspace-file-resource)
  ((name :initarg :name :reader skill-source-resource-name :type string
         :documentation "Validated name requested in the authoring URI."))
  (:documentation "The effective source file in one named user-global skill directory."))

(defclass skill-source-resolver (resource-resolver)
  ()
  (:documentation "Resolve a user-global skill directly, without scanning the skill catalog."))

(defmethod resource-resolver-read-documentation ((resolver skill-source-resolver))
  "Document bounded reads of global sources."
  (declare (ignore resolver))
  "skill-source:NAME reads the user-global skill source as revisioned lines, selecting SKILL.sexp ahead of SKILL.md. Read an unused name to observe a missing Markdown source before creating it.")

(defmethod resource-resolver-edit-documentation ((resolver skill-source-resolver))
  "Document revisioned global skill authoring."
  (declare (ignore resolver))
  "skill-source:NAME accepts original-line operations and replace-empty for a missing source. Autolith validates the complete result before publishing it.")

(-> skill-source--check-entry (pathname symbol) null)
(defun skill-source--check-entry (pathname kind)
  "Require a missing entry or an entry of KIND, without following a final link."
  (let ((status (platform-path-status *platform* pathname :follow-links-p nil)))
    (when (and status (not (eq kind (platform-file-status-kind status))))
      (error 'tool-error :message "A global skill source must be a regular file in its own skill directory, without symbolic links.")))
  nil)

(-> skill-source--check-directory (pathname pathname) null)
(defun skill-source--check-directory (directory root)
  "Check caller-owned directory confinement before probing source entries."
  (skill-source--check-entry root ':directory)
  (skill-source--check-entry directory ':directory)
  (unless (workspace-tool--read-path-allowed-p directory (list root))
    (error 'tool-error :message "A global skill source must be in its configured skill root."))
  nil)

(defmethod resource-resolver-resolve ((resolver skill-source-resolver) identifier (context tool-context))
  "Resolve an exact skill directory under the caller's global config root."
  (unless (cl-skills:skill-name-valid-p identifier)
    (error 'tool-error :message "Use a skill name of 1-64 lowercase ASCII letters, digits or single internal hyphens."))
  (let* ((root      (skill-global-root (tool-context-configuration context)))
         (directory (merge-pathnames (format nil "~A/" identifier) root)))
    (skill-source--check-directory directory root)
    (multiple-value-bind (selected markdown) (cl-skills:skill-directory-source-pathname directory)
      (let ((resource (make-instance 'skill-source-resource
                                     :name     identifier
                                     :uri      (format nil "~A:~A" (resource-resolver-scheme resolver) identifier)
                                     :pathname (or selected markdown))))
        (workspace-file-check-path resource context)
        resource))))

(defmethod workspace-file-resource-access-roots ((resource skill-source-resource) (context tool-context))
  "Allow this resource to access its configured user-global skill root."
  (declare (ignore resource))
  (list (skill-global-root (tool-context-configuration context))))

(defmethod workspace-file-check-path ((resource skill-source-resource) (context tool-context))
  "Reject links and refuse a Markdown source shadowed by a native definition."
  (let* ((path      (workspace-file-resource-pathname resource))
         (directory (uiop:pathname-directory-pathname path))
         (root      (first (workspace-file-resource-access-roots resource context))))
    (skill-source--check-directory directory root)
    (skill-source--check-entry path ':file)
    (unless (workspace-tool--read-path-allowed-p path (list root))
      (error 'tool-error :message "A global skill source must be in its configured skill root."))
    (multiple-value-bind (selected markdown) (cl-skills:skill-directory-source-pathname directory)
      (unless (equal path (or selected markdown))
        (error 'tool-error :message "The effective skill source changed. Reread the skill source before editing.")))))

(defmethod resource-capabilities ((resource skill-source-resource) (context tool-context))
  "Allow only direct observation and validated revisioned edits."
  (declare (ignore resource context))
  '(:read :edit))

(defmethod resource-observe :around ((resource skill-source-resource) (context tool-context))
  "Read through the skill root rather than the workspace roots."
  (let ((*workspace-tool-readable-roots* (workspace-file-resource-access-roots resource context)))
    (call-next-method)))

(defmethod resource-whole-text :around
    ((resource skill-source-resource) (context tool-context) &key maximum-bytes tool-name)
  "Read a whole skill source through the skill root and the caller's byte bound."
  (declare (ignore maximum-bytes tool-name))
  (let ((*workspace-tool-readable-roots* (workspace-file-resource-access-roots resource context)))
    (call-next-method)))

(defmethod resource-apply-operations :around
    ((resource skill-source-resource) (context tool-context) &key base-revision operations)
  "Check the source under the configured root throughout its revisioned edit."
  (declare (ignore base-revision operations))
  (let ((*workspace-tool-readable-roots* (workspace-file-resource-access-roots resource context)))
    (call-next-method)))

(defmethod text-resource-check-edit-base
    ((resource skill-source-resource) (context tool-context) (base workspace-file-observation))
  "Require a fresh observation when native precedence changes the source pathname."
  (unless (equal (workspace-file-resource-pathname resource)
                 (getf (resource-observation-metadata base) :pathname))
    (workspace-file--signal-stale resource base))
  (call-next-method))

(defmethod text-resource-commit ((resource skill-source-resource) (context tool-context) &key base content)
  "Validate the complete replacement before staging any bytes."
  (declare (ignore base))
  (let ((pathname (workspace-file-resource-pathname resource)))
    (handler-case
        (let ((name (cl-skills:skill-source-validate
                     content pathname :file-character-limit
                     (min (* 256 1024) cl-skills:*skill-file-character-limit*))))
          (unless (string= name (skill-source-resource-name resource))
            (error 'tool-error :tool-name "resource.edit" :message "A global skill name must match its directory.")))
      (cl-skills:skill-validation-error (condition)
        (error 'tool-error :tool-name "resource.edit"
                           :message   (format nil "Skill validation failed: ~A" condition)))))
  (call-next-method))
