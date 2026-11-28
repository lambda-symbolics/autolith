(in-package #:autolith)

;;;; -- Historical Context Adoption --

;;; Existing adoption checkpoints replace the provider projection on replay.
;;; Their earlier channel history is still available in the durable log.
(defmethod conversation--project-record
    ((kind (eql :context-adoption)) conversation properties)
  "Replace the provider projection with the context an adopted agent brought."
  (unless (and (non-empty-string-p (getf properties :participant))
               (listp (getf properties :rows))
               (or (null (getf properties :source))
                   (non-empty-string-p (getf properties :source)))
               (or (null (getf properties :transition))
                   (non-empty-string-p (getf properties :transition))))
    (conversation--record-error
     conversation properties "A context adoption record is invalid."))
  (let ((projection (clinker-transcript:make-projection)))
    (conversation-compaction--append-rows conversation properties projection
                                          :rows (getf properties :rows))
    (conversation-compaction--check-correlations conversation properties projection)
    (setf (slot-value conversation 'projection)      projection
          (conversation-adopted-participant conversation) (getf properties :participant)
          (conversation-adopted-transition conversation)  (getf properties :transition)
          (conversation-last-total-tokens conversation)   0))

  (conversation--advance-provider-item-generation conversation)
  nil)
