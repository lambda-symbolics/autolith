(in-package #:autolith)

;;;; -- Procedure Catalog --

(-> procedure-catalog-context (request-context) (option context-contribution))
(defun procedure-catalog-context (request)
  "Return the catalog of the active procedures of the workspace of REQUEST, or NIL if there are none."
  (let ((configuration (request-context-configuration request)))
    (when (and (not (request-context-compaction-p request))
               (knowledge-active-p configuration))
      (when-let ((entries (remove-if (lambda (entry) (getf entry :retired-p))
                                     (handler-case
                                         (knowledge-base-procedures
                                          (knowledge-base-read configuration)
                                          (knowledge--workspace configuration))
                                       (knowledge-error ()
                                         nil)))))
        (change-class
         (make-context-contribution
          :identifier        "procedure-catalog"
          :instruction       (format nil "## Procedures~2%The procedures of this workspace follow. Run one with procedure.run.~2%~:{- ~A: ~A~%~}"
                                     (mapcar (lambda (entry)
                                               (handler-case
                                                   (list (procedure-signature entry)
                                                         (procedure-documentation entry))
                                                 (knowledge-error (condition)
                                                   (list (getf entry :id)
                                                         (autolith-error-message condition)))))
                                             entries))
          :priority          800
          :class             ':mandatory
          :deduplication-key "procedure-catalog")
         'anchored-context-contribution)))))

(register-context-contributor "procedure-catalog" 'procedure-catalog-context :source ':built-in)
