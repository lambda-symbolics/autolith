(in-package #:autolith)

;;;; -- Knowledge in Portable Archives --

;;; An archive carries the knowledge log as its records in order. A workspace
;;; archive carries that workspace's predicates, claims, rules, and their
;;; evidence and retractions, plus the global predicates they use. Claim and
;;; evidence identities include the workspace, so relocating a workspace
;;; recomputes them and rewrites every reference, including memory premises
;;; naming claims. Importing appends the records the destination lacks and
;;; refuses records whose identity it already holds with other content.

(-> data-transfer--knowledge-owner (list) (option string))
(defun data-transfer--knowledge-owner (record)
  "Return the workspace owning knowledge RECORD, or NIL for global or ownerless records."
  (getf (rest record) :workspace))

(-> data-transfer--knowledge-records (configuration) list)
(defun data-transfer--knowledge-records (configuration)
  "Return the complete records of CONFIGURATION's knowledge log, without its header."
  (let ((pathname (configuration-knowledge-path configuration)))
    (if (probe-file pathname)
        (loop for record in (data-transfer--log-records pathname ':knowledge)
              append (if (eq (first record) :transaction)
                         (copy-list (getf (rest record) :records))
                         (list record)))
        nil)))

(-> data-transfer--collect-knowledge (configuration t) list)
(defun data-transfer--collect-knowledge (configuration workspace)
  "Return the knowledge records an archive of WORKSPACE, or of everything, carries."
  (let ((records (data-transfer--knowledge-records configuration)))
    (if (null workspace)
        records
        (let* ((owned (lambda (record)
                        (and (member (first record)
                                     '(:schema-defined :claim-asserted :rule-defined
                                       :rule-admitted :rule-retired :method-defined
                                       :contract-defined :procedure-defined
                                       :procedure-admitted :procedure-retired
                                       :procedure-run :diagnosis-defined
                                       :diagnosis-recorded))
                             (eq (getf (rest record) :scope) ':workspace)
                             (equal (data-transfer--knowledge-owner record) workspace))))
               (claims (remove-if-not (lambda (record)
                                        (and (eq (first record) :claim-asserted)
                                             (funcall owned record)))
                                      records))
               (supports (mapcar (lambda (record) (getf (rest record) :support)) claims))
               (evidence (remove nil (mapcar (lambda (record) (getf (rest record) :evidence))
                                             claims)))
               (predicates
                 (remove-duplicates
                  (append
                   (mapcar (lambda (record)
                             (knowledge-proposition-predicate (getf (rest record) :proposition)))
                           claims)
                   (loop for record in records
                         when (and (eq (first record) :rule-defined) (funcall owned record))
                           append (cons (knowledge-rule-conclusion-predicate
                                         (getf (rest record) :rule))
                                        (knowledge-rule-premise-predicates
                                         (getf (rest record) :rule)))))
                  :test #'string=)))
          (remove-if-not
           (lambda (record)
             (case (first record)
               ((:evidence :evidence-renewed)
                (member (getf (rest record) :id) evidence :test #'string=))
               (:support-retracted
                (member (getf (rest record) :support) supports :test #'string=))
               (:schema-defined
                (or (funcall owned record)
                    (and (eq (getf (rest record) :scope) ':global)
                         (member (knowledge-schema-predicate (getf (rest record) :schema))
                                 predicates :test #'string=))))
               (otherwise
                (funcall owned record))))
           records)))))

(-> data-transfer--validate-knowledge (list t) null)
(defun data-transfer--validate-knowledge (archive pathname)
  "Require ARCHIVE's knowledge records to be well formed and to fold consistently."
  (let ((records (getf archive :knowledge)))
    (unless (and (listp records) (every #'knowledge--record-valid-p records))
      (data-transfer--fail pathname ':invalid "Malformed knowledge record."))
    (handler-case
        (let ((base (make-instance 'knowledge-base)))
          (dolist (record records)
            (knowledge--reduce pathname base record)))
      (knowledge-error (condition)
        (data-transfer--fail pathname ':invalid
                             (format nil "Inconsistent knowledge records: ~A"
                                     (autolith-error-message condition))))))
  nil)

(-> data-transfer--knowledge-workspaces (list) list)
(defun data-transfer--knowledge-workspaces (archive)
  "Return the workspaces ARCHIVE's knowledge records belong to."
  (remove nil (mapcar #'data-transfer--knowledge-owner (getf archive :knowledge))))

(-> data-transfer--remap-knowledge (list string string) list)
(defun data-transfer--remap-knowledge (archive old new)
  "Return ARCHIVE with its knowledge and claim premises moved from workspace OLD to NEW.

Claim and evidence identifiers that depend on the workspace are recomputed and
every reference to them, in supports and in memory premises, follows."
  (let ((claims (make-hash-table :test #'equal))
        (evidence (make-hash-table :test #'equal))
        (copy (copy-tree archive)))
    (dolist (record (getf copy :knowledge))
      (let ((properties (rest record)))
        (when (equal (getf properties :workspace) old)
          (setf (getf (rest record) :workspace) new)
          (case (first record)
            (:claim-asserted
             (let ((identifier (knowledge-claim-identifier (getf properties :proposition)
                                                           :scope (getf properties :scope)
                                                           :workspace new)))
               (setf (gethash (getf properties :claim) claims) identifier
                     (getf (rest record) :claim) identifier)))
            (:evidence
             (let ((identifier (knowledge-digest-identifier
                                #\e (list (getf properties :source) (getf properties :digest)
                                          (getf properties :adapter) new))))
               (setf (gethash (getf properties :id) evidence) identifier
                     (getf (rest record) :id) identifier)))))))
    (dolist (record (getf copy :knowledge))
      (case (first record)
        (:claim-asserted
         (when (gethash (getf (rest record) :evidence) evidence)
           (setf (getf (rest record) :evidence)
                 (gethash (getf (rest record) :evidence) evidence))))
        (:evidence-renewed
         (when (gethash (getf (rest record) :id) evidence)
           (setf (getf (rest record) :id) (gethash (getf (rest record) :id) evidence))))))
    (dolist (record (getf copy :memories))
      (when (eq (first record) :memory)
        (dolist (premise (getf (rest record) :premises))
          (when (and (eq (getf premise :kind) ':claim) (gethash (getf premise :id) claims))
            (setf (getf premise :id) (gethash (getf premise :id) claims)
                  (getf premise :digest) (memory-source-text-digest (getf premise :id)))))))
    copy))

(-> data-transfer--knowledge-identity (list) (option list))
(defun data-transfer--knowledge-identity (record)
  "Return the identity under which knowledge RECORD may appear only once."
  (case (first record)
    (:evidence (list :evidence (getf (rest record) :id)))
    (:claim-asserted (list :support (getf (rest record) :support)))
    (:support-retracted (list :retraction (getf (rest record) :support)))
    (otherwise nil)))

(-> data-transfer--merge-knowledge (list list t) list)
(defun data-transfer--merge-knowledge (existing imported pathname)
  "Return IMPORTED records EXISTING lacks, refusing divergent identities or an inconsistent result.

Contract admissions are local authorization receipts. Preserve exported metadata
in the archive, but import contract definitions as inactive proposals."
  (unless (every #'knowledge--record-valid-p imported)
    (data-transfer--fail pathname ':invalid "Malformed imported knowledge record."))
  (let ((added nil))
    (dolist (source-record (loop for record in imported
                                 append (if (eq (first record) :transaction)
                                            (copy-list (getf (rest record) :records))
                                            (list record))))
      (let* ((record (if (eq (first source-record) :contract-defined)
                         (let ((copy (copy-tree source-record)))
                           (remf (rest copy) :admission)
                           copy)
                         source-record))
             (identity (data-transfer--knowledge-identity record))
             (match (if identity
                        (find identity existing :key #'data-transfer--knowledge-identity
                                                :test #'equal)
                        (find record existing :test #'equal))))
        (cond
          ((null match)
           (push record added))
          ((not (equal match record))
           (data-transfer--fail pathname ':conflict
                                (format nil "Knowledge identity ~S already has different content."
                                        identity))))))
    (setf added (nreverse added))
    (handler-case
        (let ((base (make-instance 'knowledge-base)))
          (dolist (record (append existing added))
            (knowledge--reduce pathname base record)))
      (knowledge-error (condition)
        (data-transfer--fail pathname ':conflict
                             (format nil "Imported knowledge does not fit existing knowledge: ~A"
                                     (autolith-error-message condition)))))
    added))
