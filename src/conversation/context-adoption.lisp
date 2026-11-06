(in-package #:autolith)

;;;; -- Context Adoption --

;;; An agent moved into a channel brings the provider context it built
;;; elsewhere. The destination conversation records that context durably as
;;; one :CONTEXT-ADOPTION record, which replaces its provider projection the
;;; way a compaction checkpoint does: the agent's requests then hold its own
;;; context followed by what is said here from the move onward, and the
;;; channel's earlier history stays in its records for anyone who reads it.

(-> conversation-context-rows (conversation) list)
(defun conversation-context-rows (conversation)
  "Return CONVERSATION's durable provider context as portable rows, oldest first.

Each row holds an item's wire JSON and its family metadata. Request-local
items are left out."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((projection (conversation-projection conversation))
          (ephemeral  (make-hash-table :test #'eq)))
      (dolist (entry (conversation-ephemeral-input-entries conversation))
        (setf (gethash (getf entry :item) ephemeral) t))
      (loop for item in (clinker-transcript:projection-items projection)
            unless (gethash item ephemeral)
              collect (conversation-compaction--serialize-item projection item)))))

(-> conversation-append-context-adoption
    (conversation list &key (:participant string) (:source (option string))
                            (:transition (option string)))
    list)
(defun conversation-append-context-adoption (conversation rows &key participant source transition)
  "Make ROWS, the context agent PARTICIPANT brings from channel SOURCE by move TRANSITION, CONVERSATION's provider context.

The record is appended durably before the projection it describes is
installed, and a conversation reloaded later rebuilds the same projection
from it. Return the sequenced record."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((record     (list* :context-adoption
                             :participant participant
                             :rows        rows
                             (append (and source (list :source source))
                                     (and transition (list :transition transition)))))
          (projection (clinker-transcript:make-projection)))
      ;; Validate the rows into the projection first, so a bad context is
      ;; refused before anything durable changes.
      (conversation-compaction--append-rows conversation (rest record) projection :rows rows)
      (conversation-compaction--check-correlations conversation (rest record) projection)
      (sb-sys:without-interrupts
        (let ((published (conversation-append-record conversation record)))
          (setf (slot-value conversation 'projection)      projection
                (conversation-adopted-participant conversation) participant
                (conversation-adopted-transition conversation)  transition
                (conversation-last-total-tokens conversation)   0)
          published)))))

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
  nil)
