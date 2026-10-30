(in-package #:autolith-supervisor)

(defvar *memory-transaction*
  nil
  "The in-progress memory transaction, bound only under the store lock.")

;; The shared lock is defined with the memory domain.
(declaim (special *memory-lock*))

(defclass memory-state nil
          ((active :initarg :active :accessor memory-state-active :documentation
            "Active memory objects from the locked log snapshot.")
           (receipts :initarg :receipts :accessor memory-state-receipts :documentation
            "Durable session-scoped operation identities and results.")
           (pending :initform nil :accessor memory-state-pending :documentation
            "Validated records collected before one atomic append."))
          (:documentation "A memory log snapshot and its transaction-local append batch."))

(defun memory--finish-state (table)
  "Return the complete memory snapshot from a fresh reducer TABLE."
  (let ((receipts (gethash ':operations table)))
    (remhash ':operations table)
    (make-instance 'memory-state :active (memory--sort-active table) :receipts
                   (or receipts (make-hash-table :test #'equal)))))

(defun memory--reduce-operation (pathname table record)
  "Validate and fold an operation receipt into the same log as its memory write."
  (unless
      (and (sexp-store:record-shape-p record) (= (length record) 11)
           (equal
            (loop for tail on (rest record) by #'(lambda (tail) (rest (rest tail)))
                  collect (first tail))
            '(:version :session :operation :identity :result))
           (eql (getf (rest record) :version) 1) (non-empty-string-p (getf (rest record) :session))
           (non-empty-string-p (getf (rest record) :operation))
           (stringp (getf (rest record) :identity)) (listp (getf (rest record) :result)))
    (error 'memory-error :message "Invalid memory operation receipt." :pathname pathname
           :identifier nil))
  (let* ((receipts
          (or (gethash ':operations table)
              (setf (gethash ':operations table) (make-hash-table :test #'equal))))
         (key      (list (getf (rest record) :session) (getf (rest record) :operation)))
         (existing (gethash key receipts)))
    (when (and existing (not (equal existing record)))
      (error 'memory-error :message "Conflicting memory operation receipt." :pathname pathname
             :identifier nil))
    (setf (gethash key receipts) (copy-tree record)))
  table)

(defun memory-state-transact (context update)
  "Run UPDATE under the common process lock and file lock, then append its batch."
  (bordeaux-threads:with-recursive-lock-held (*memory-lock*)
    (let ((*memory-store-context* context))
      (handler-case
          (sexp-store:store-transact
           (memory--store context)
           (lambda (state)
             (let ((*memory-transaction* state))
               (let ((result (funcall update state)))
                 (values (nreverse (memory-state-pending state)) result
                         (not (null (memory-state-pending state))))))))
        (sexp-store:store-error (cause)
          (error 'memory-error :message   (sexp-store:store-error-message cause)
                              :pathname   (sexp-store:store-error-pathname cause)
                              :identifier nil))))))

(defun memory-state-collect (state records)
  "Fold domain RECORDS into STATE and collect the atomic append batch."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (memory (memory-state-active state))
      (setf (gethash (memory-identifier memory) table) memory))
    (setf (gethash :operations table) (memory-state-receipts state))
    (dolist (record records)
      (memory--reduce-record (memory-context-memory-path *memory-store-context*) table record)
      (push record (memory-state-pending state)))
    (remhash :operations table)
    (setf (memory-state-active state) (memory--sort-active table))))
