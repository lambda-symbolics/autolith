(in-package #:autolith)

;;;; -- Skill Description Resources --

;;; The catalog shortens a long description. skill:NAME returns the whole
;;; description of one skill, and reading it never selects the skill.

(defclass skill-resource (resource)
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

(defmethod resource-capabilities ((resource skill-resource) (context tool-context))
  "Expose a skill description for reading only."
  (declare (ignore resource context))
  '(:read))

(defmethod resource-tool-read
    ((resource skill-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return the whole description of RESOURCE's skill without selecting it."
  (declare (ignore tool context arguments))
  (tool-success
   (format nil "URI: ~A~%Description: ~A"
           (resource-uri resource)
           (skill-metadata-description (skill-resource-metadata resource)))))
