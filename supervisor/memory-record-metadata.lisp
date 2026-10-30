(in-package #:autolith-supervisor)

;;;; -- Public Memory Record Metadata --

(serapeum/types:-> memory-record-metadata--reason (list) hash-table)
(defun memory-record-metadata--reason (reason)
  "Copy only public reason fields into a portable map."
  (let ((fields (make-hash-table :test #'equal)))
    (dolist (key '(:kind :path :why :id :title :applicability))
      (let ((value (getf reason key)))
        (when value
          (setf (gethash (string-downcase (symbol-name key)) fields)
                (if (keywordp value) (string-downcase (symbol-name value)) value)))))
    fields))

(serapeum/types:-> memory-record-metadata--outcome (list) hash-table)
(defun memory-record-metadata--outcome (outcome)
  "Copy the outcome and its optional public reason without internal assessment objects."
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "outcome" fields) (string-downcase (symbol-name (getf outcome :outcome))))
    (when (getf outcome :reason)
      (setf (gethash "reason" fields) (memory-record-metadata--reason (getf outcome :reason))))
    fields))

(serapeum/types:-> memory-record-metadata--source (list) hash-table)
(defun memory-record-metadata--source (outcome)
  "Encode one source outcome using the memory service's evidence representation."
  (let ((fields (memory-record-metadata--outcome outcome)))
    (setf (gethash "source" fields)
          (aref (memory-service--sources-data (list (getf outcome :source))) 0))
    fields))

(serapeum/types:-> memory-record-metadata--premise (list) hash-table)
(defun memory-record-metadata--premise (outcome)
  "Encode one premise outcome without copying its memory, claim, or assessment object."
  (let* ((fields (memory-record-metadata--outcome outcome))
         (memory (getf outcome :memory))
         (claim  (getf outcome :claim))
         (reason (getf outcome :reason))
         (title  (cond
                   (memory
                    (memory-title memory))
                   (claim
                    (knowledge-node-text claim))
                   (t
                    (getf reason :title)))))
    (setf (gethash "id" fields) (getf outcome :id)
          (gethash "kind" fields) (if (or claim (eq (getf reason :kind) ':premise-claim))
                                     "claim"
                                     "memory"))
    (when title (setf (gethash "title" fields) title))
    fields))

(serapeum/types:-> memory-service-record-metadata (memory-store-context memory) hash-table)
(defun memory-service-record-metadata (context memory)
  "Return read-only public metadata assessed under CONTEXT's approved source roots.

Assess evidence with the shared freshness engine. Do not expose store paths,
internal objects, or recursive assessments in the returned portable map."
  (let* ((*memory-store-context* context)
         (assessor               (memory-assessor-create context))
         (assessment             (memory-assess assessor memory))
         (result                 (make-hash-table :test #'equal))
         (fields                 (make-hash-table :test #'equal)))
    (setf (gethash "status" fields)
          (string-downcase (symbol-name (memory-assessment-status assessment)))
          (gethash "summary" fields) (or (memory-assessment-summary assessment) "")
          (gethash "source_outcomes" fields)
          (map 'vector #'memory-record-metadata--source (memory-assessment-source-outcomes assessment))
          (gethash "premise_outcomes" fields)
          (map 'vector #'memory-record-metadata--premise (memory-assessment-premise-outcomes assessment))
          (gethash "created_at" result) (memory-created-at memory)
          (gethash "updated_at" result) (memory-updated-at memory)
          (gethash "assessment" result) fields
          (gethash "dependents" result)
          (map 'vector #'memory-identifier
               (sort (memory-assessor-dependents assessor (memory-identifier memory))
                     #'string< :key #'memory-identifier)))
    (when (memory-source-conversation memory)
      (setf (gethash "source_conversation" result) (memory-source-conversation memory)))
    result))
