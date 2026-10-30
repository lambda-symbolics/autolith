(in-package #:autolith-supervisor)

(define-condition memory-record-error
    (error)
    ((code :initarg :code :reader memory-record-error-code :documentation
      "The stable public protocol failure code."))
  (:documentation "A record request failed without disclosing private data.")
  (:report
   (lambda (condition stream)
     (format stream "Memory record request failed: ~A" (memory-record-error-code condition)))))

(defun memory-record--reject (code)
  "Signal a public record rejection CODE."
  (error 'memory-record-error :code code))

(defun memory-record-snapshot (memory)
  "Return the complete portable structural snapshot of MEMORY."
  (copy-tree (memory--record memory)))

(defun memory-record-revision (snapshot)
  "Return the digest revision of a complete portable SNAPSHOT."
  (memory-source-text-digest
   (with-standard-io-syntax
    (let ((*package* (find-package '#:keyword)))
      (write-to-string snapshot :readably t :pretty nil)))))

(defun memory-records-read (context &key (visibility :workspace) query project)
  "Return matched projections and the complete structural scope snapshot under one lock."
  (let ((snapshot nil))
    (let ((result
            (memory-state-transact
             context
             (lambda (state)
               (declare (ignore state))
               (setf snapshot (mapcar #'memory-record-snapshot
                                     (memory-list context :visibility visibility)))
               (let ((matches (if (and query (plusp (length query)))
                                  (memory-search context query :visibility visibility)
                                  (memory-list context :visibility visibility))))
                 (if project (mapcar project matches) matches))))))
      (values result snapshot))))(defun memory-call-with-guard (context &key expected snapshot-function update)
                                  "Compare EXPECTED and the current structural snapshot, then run UPDATE under one lock.

SNAPSHOT-FUNCTION and UPDATE run in the same memory store transaction. A
mismatch cannot release the lock before an append. Return all UPDATE values."
                                  (let ((result nil))
                                    (memory-state-transact context
                                                           (lambda (state)
                                                             (declare (ignore state))
                                                             (unless (equal expected (funcall snapshot-function))
                                                               (memory-record--reject ':revision-conflict))
                                                             (setf result (multiple-value-list (funcall update)))))
                                    (values-list result)))

(defun memory-records-mutate
       (context
        &key session-id operation-id operation record-id expected-revision expected-snapshot fields
        (scope ':workspace) visibility prepare-fields validate-result collection-revision check-collection)
  "Apply a memory mutation and its durable receipt in one store transaction.

SCOPE and VISIBILITY come from trusted parent policy. FIELDS contains normalized
writable values. PREPARE-FIELDS checks evidence after receipt and revision checks,
so a retry does not observe new evidence. VALIDATE-RESULT checks resulting memory
objects before append, including receipt replay. Return resulting memory objects."
  (unless
      (and (non-empty-string-p session-id) (<= (length session-id) 1024)
           (non-empty-string-p operation-id) (<= (length operation-id) 1024)
           (member operation '(:create :replace :delete)) (typep scope 'memory-scope))
    (memory-record--reject ':invalid-request))
  (let ((identity
         (with-standard-io-syntax
          (write-to-string (append (list operation record-id expected-revision fields scope)
                                  (when collection-revision (list collection-revision))) :readably t
                           :pretty                                                                 nil))))
    (memory-state-transact context
                           (lambda (state)
                             (let ((receipt
                                    (gethash (list session-id operation-id)
                                             (memory-state-receipts state))))
                               (when receipt
                                 (unless (equal identity (getf (rest receipt) :identity))
                                   (memory-record--reject ':invalid-request))
                                 (let ((result
                                         (mapcar
                                          (lambda (record)
                                            (memory--record->memory (memory-context-memory-path context)
                                                                    record))
                                          (getf (rest receipt) :result))))
                                   (when validate-result (funcall validate-result result))
                                   (return-from memory-records-mutate result))))
                             (when check-collection (funcall check-collection))
                             (let* ((visibility
                                     (or visibility
                                         (if (eq scope ':global) ':global ':workspace)))
                                    (existing
                                     (and record-id
                                          (find record-id
                                                (memory-list context :visibility visibility) :test
                                                #'string= :key #'memory-identifier))))
                              (case operation
                                (:create
                                 (when (or record-id expected-revision expected-snapshot)
                                   (memory-record--reject ':invalid-request)))
                                (otherwise
                                 (unless
                                     (and existing expected-snapshot
                                          (equal expected-snapshot
                                                 (memory-record-snapshot existing))
                                          (equal expected-revision
                                                 (memory-record-revision
                                                  (memory-record-snapshot existing))))
                                   (memory-record--reject ':revision-conflict))))
                              (let* ((result
                                      (case operation
                                        (:delete
                                         (memory-forget context record-id :source-conversation
                                                        session-id)
                                         nil)
                                        (otherwise
                                         (list
                                          (apply #'memory-remember context :identifier record-id
                                                 :source-conversation                  session-id
                                                 (let* ((prepared     (if prepare-fields
                                                                          (funcall prepare-fields fields)
                                                                          (copy-list fields)))
                                                        (target-scope (or (getf prepared :scope)
                                                                          (and existing (memory-scope existing))
                                                                          scope)))
                                                   (remf prepared :scope)
                                                   (list* :scope target-scope prepared)))))))
                                     (records (mapcar #'memory-record-snapshot result))
                                     (receipt
                                      (list ':memory-operation :version 1 :session session-id
                                            :operation operation-id :identity identity :result
                                            records)))
                                 (when validate-result (funcall validate-result result))
                                (memory-state-collect state (list receipt))
                                result))))))
