(in-package #:autolith)

;;;; -- Display-Only History --

(-> application-side-status--validate-record (conversation list) list)
(defun application-side-status--validate-record (conversation properties)
  "Validate bounded semantic output before replay without projecting model input."
  (unless (and (record-check
                properties :properties-p t :keyword-keys-p t :maximum-length 8
                :fields '((:indicator :seq :required t)
                          (:indicator :time :required t)
                          (:indicator :source :required t)
                          (:indicator :spans :required t)))
               (typep (getf properties :seq) '(integer 1))
               (typep (getf properties :time) 'timestamp)
               (typep (getf properties :source) 'string)
               (<= (length (getf properties :source)) 4096)
               (proper-list-p (getf properties :spans))
               (<= (length (getf properties :spans)) 128)
               (every #'terminal-span-p (getf properties :spans))
               (<= (loop for span in (getf properties :spans)
                         sum (length (terminal-span-text span)))
                   16384))
    (conversation--record-error conversation properties
                                "A persisted side status has invalid display data."))
  properties)

(defmethod conversation--project-record
    ((kind (eql :side-status)) (conversation conversation) properties)
  "Validate display-only history without adding input or operation context."
  (declare (ignore kind))
  (application-side-status--validate-record conversation properties)
  nil)

(-> application-side-status-present (application string list) null)
(defun application-side-status-present (application source spans)
  "Persist SPANS once and present them with the same identity used after resume."
  (let* ((conversation (application-conversation application))
         (properties   (list :source (conversation-user-operation--bounded-text source 4096)
                             :spans  spans)))
    (application-side-status--validate-record
     conversation (list* :seq  (conversation-next-sequence conversation)
                         :time (get-universal-time) properties))
    (let ((record (conversation-append-record
                   conversation (list* :side-status properties))))
      (when (application-ui application)
        (application--present-conversation-record application record))))
  nil)
