(in-package #:autolith)

;;;; -- Owned Edit Proposals --

(defclass edit-proposal-store ()
  ((entries :initform      (make-hash-table :test #'eq :weakness :key)
            :reader        edit-proposal-store-entries
            :documentation "Newest-first proposals per conversation, without owning conversations.")
   (lock :initform (make-lock "edit proposals") :reader edit-proposal-store-lock
         :documentation "Serializes retention, reservation and consumption."))
  (:documentation "Bounded owner-local proposals, independent of ordinary resource-read retention."))

(defvar *edit-proposal-ownership-lock* (make-lock "edit proposal ownership")
  "Serializes assignment of a proposal to its unique retention store.")

(defclass edit-proposal ()
  ((identifier :initform (daemon-random-token) :reader edit-proposal-identifier
               :documentation "Opaque conversation-local identity.")
   (snapshots :initarg :snapshots :reader edit-proposal-snapshots
              :documentation "Resource-keyed lists of resource, owned observation state and optional version.")
   (store :initform nil :accessor edit-proposal-store
          :documentation "Owning retention store, assigned on admission.")
   (applying-p :initform nil :accessor edit-proposal-applying-p
               :documentation "Whether an invocation reserved publication.")
   (consumed-p :initform nil :accessor edit-proposal-consumed-p
               :documentation "Whether publication completed successfully."))
  (:documentation "Immutable changes against owned baselines with one-time successful application."))

(defgeneric edit-proposal-store-limits (store)
  (:documentation "Return maximum entries, snapshots and snapshot bytes for STORE."))

(defgeneric edit-proposal-fail (store code message)
  (:documentation "Signal the owner's typed policy failure with CODE and MESSAGE."))

(defgeneric edit-proposal-requests (context proposal)
  (:documentation "Construct ordinary cl-resources changes from a validated proposal."))

(defgeneric edit-proposal-validate (context proposal)
  (:documentation "Reauthorize baselines and reject drift before publication."))

(defgeneric edit-proposal-access (proposal)
  (:documentation "Return the access required when checking proposal baselines."))

(defmethod edit-proposal-access ((proposal edit-proposal))
  "Require edit authority for ordinary proposed changes."
  ':edit)

(defmethod edit-proposal-validate ((context tool-context) (proposal edit-proposal))
  "Compare fresh observations with independently retained baselines."
  (maphash
   (lambda (key record)
     (declare (ignore key))
     (destructuring-bind (resource state &optional version) record
       (declare (ignore version))
       (workspace-file--call-with-authorized-access
        resource context (edit-proposal-access proposal)
        :function
        (lambda ()
          (let ((before  (resource-observation-state-observation state))
                (current (resource-observe resource context)))
            (unless (workspace-file--same-observation-p before current)
              (workspace-file--signal-stale resource before current)))))))
   (edit-proposal-snapshots proposal))
  nil)

(-> edit-proposal--weight (edit-proposal) (values (integer 0) (integer 0)))
(defun edit-proposal--weight (proposal)
  "Return retained snapshot count and UTF-8 bytes."
  (values (hash-table-count (edit-proposal-snapshots proposal))
          (loop for record being the hash-values of (edit-proposal-snapshots proposal)
                sum (workspace-file--observation-retained-bytes
                     (resource-observation-state-observation (second record))))))

(-> edit-proposal-retain (edit-proposal-store tool-context edit-proposal) edit-proposal)
(defun edit-proposal-retain (store context proposal)
  "Retain PROPOSAL atomically, evicting the oldest idle entries as needed to meet STORE's limits.
Once assigned to a retention store, a proposal cannot change stores."
  (with-lock-held (*edit-proposal-ownership-lock*)
    (when (and (edit-proposal-store proposal)
               (not (eq (edit-proposal-store proposal) store)))
      (edit-proposal-fail store ':proposal-owned
                          "Proposal is already assigned to another retention store."))
    (with-lock-held ((edit-proposal-store-lock store))
      (when (edit-proposal-consumed-p proposal)
        (edit-proposal-fail store ':unknown-proposal
                            "Proposal is consumed or evicted; prepare a new proposal."))
      (multiple-value-bind (maximum-entries maximum-count maximum-bytes)
          (edit-proposal-store-limits store)
        (multiple-value-bind (count bytes) (edit-proposal--weight proposal)
          (when (or (< maximum-entries 1) (> count maximum-count) (> bytes maximum-bytes))
            (edit-proposal-fail store ':proposal-limit
                                "Proposal exceeds its retained resource budget; narrow the request."))
          (let* ((conversation     (tool-context-conversation context))
                 (entries          (copy-list (gethash conversation (edit-proposal-store-entries store))))
                 (already-retained (member proposal entries)))
            (unless already-retained
              (dolist (entry entries)
                (multiple-value-bind (entry-count entry-bytes) (edit-proposal--weight entry)
                  (incf count entry-count)
                  (incf bytes entry-bytes)))
              (loop while (or (>= (length entries) maximum-entries)
                              (> count maximum-count) (> bytes maximum-bytes))
                    for oldest = (find-if-not #'edit-proposal-applying-p entries :from-end t)
                    do (unless oldest
                         (edit-proposal-fail store ':proposal-limit
                                             "All retained proposal slots are reserved; retry after application completes."))
                       (multiple-value-bind (entry-count entry-bytes) (edit-proposal--weight oldest)
                         (decf count entry-count)
                         (decf bytes entry-bytes))
                       (setf entries (remove oldest entries)))
              (setf (edit-proposal-store proposal) store
                    (gethash conversation (edit-proposal-store-entries store))
                    (cons proposal entries))))))))
  proposal)

(-> edit-proposal-find (edit-proposal-store tool-context string) edit-proposal)
(defun edit-proposal-find (store context identifier)
  "Find an unconsumed identity only in the calling conversation."
  (or (with-lock-held ((edit-proposal-store-lock store))
        (find identifier (gethash (tool-context-conversation context) (edit-proposal-store-entries store))
              :key #'edit-proposal-identifier :test #'equal))
      (edit-proposal-fail store ':unknown-proposal "Proposal is unknown, consumed or evicted; prepare a new proposal.")))

(-> edit-proposal--reserve (edit-proposal-store tool-context edit-proposal) null)
(defun edit-proposal--reserve (store context proposal)
  "Reserve exactly one retained proposal under its owner's lock."
  (with-lock-held ((edit-proposal-store-lock store))
    (unless (and (not (edit-proposal-consumed-p proposal))
                 (member proposal (gethash (tool-context-conversation context) (edit-proposal-store-entries store))))
      (edit-proposal-fail store ':unknown-proposal "Proposal is consumed or evicted; prepare a new proposal."))
    (when (edit-proposal-applying-p proposal)
      (edit-proposal-fail store ':proposal-busy "Proposal is already being applied; wait for that invocation."))
    (setf (edit-proposal-applying-p proposal) t))
  nil)

(-> edit-proposal-call-with-observations (tool-context edit-proposal function) t)
(defun edit-proposal-call-with-observations (context proposal function)
  "Supply owned revision aliases to ordinary cl-resources staging and publication."
  (let ((*workspace-file-proposal-observations* (make-hash-table :test #'equal)))
    (maphash
     (lambda (key record)
       (declare (ignore key))
       (let ((resource (first record)) (state (second record)))
         (setf (gethash (list (tool-context-conversation context) (resource-uri resource)
                             (resource-observation-state-alias state))
                        *workspace-file-proposal-observations*) state)))
     (edit-proposal-snapshots proposal))
    (funcall function)))

(-> edit-proposal-apply
    (edit-proposal-store tool-context edit-proposal &key (:before (option function))) list)
(defun edit-proposal-apply (store context proposal &key before)
  "Publish PROPOSAL successfully at most once; retain it for inspection and retry if validation or publication fails."
  (sb-sys:without-interrupts
    (edit-proposal--reserve store context proposal)
    (unwind-protect
         (let ((results
                 (sb-sys:with-local-interrupts
                   (when before (funcall before))
                   (edit-proposal-call-with-observations
                    context proposal
                    (lambda ()
                      (let ((requests (edit-proposal-requests context proposal)))
                        (if requests
                            (with-recursive-lock-held (*workspace-file-mutation-lock*)
                              (edit-proposal-validate context proposal)
                              (workspace-change-set-apply context requests))
                            (progn
                              (edit-proposal-validate context proposal)
                              nil))))))))
           (with-lock-held ((edit-proposal-store-lock store))
             (setf (edit-proposal-consumed-p proposal) t
                   (gethash (tool-context-conversation context) (edit-proposal-store-entries store))
                   (remove proposal (gethash (tool-context-conversation context) (edit-proposal-store-entries store)))))
           results)
      (with-lock-held ((edit-proposal-store-lock store))
        (setf (edit-proposal-applying-p proposal) nil)))))
