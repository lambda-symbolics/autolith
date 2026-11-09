(in-package #:autolith)

;;;; -- Task Channel Storage --

(-> task-child-delete-storage
    (configuration list &key (:identities (option hash-table)) (:leased-p boolean)
     (:prepared-p boolean))
    null)
(defun task-child-delete-storage (configuration channels &key identities leased-p prepared-p)
  "Delete idle child storage after preparing delivery evidence for every root.

Prepare every root before deleting the first directory. PREPARED-P means the
caller already preserved that evidence while holding all conversation leases."
  (let ((identities
         (or identities (conversation-pathnames-for-ids configuration channels :prefer-task-p t))))
    (if leased-p
        (progn
         (unless prepared-p (task-child--prepare-storage configuration channels identities))
         (maphash
          (lambda (root entries) (task-child--delete-root-storage configuration root entries))
          (task-child--storage-roots configuration channels identities)))
        (channel--call-with-removal-leases configuration channels :identities identities :function
         (lambda ()
           (task-child-delete-storage configuration channels :identities identities :leased-p t
            :prepared-p prepared-p)))))
  nil)

(-> task-child--storage-roots (configuration list hash-table) hash-table)
(defun task-child--storage-roots (configuration channels identities)
  "Group task CHANNELS by their storage owner using exact resolved IDENTITIES."
  (let ((roots (make-hash-table :test #'equal)))
    (dolist (channel channels)
      (multiple-value-bind (root directory)
          (conversation-task-storage-root configuration (gethash channel identities))
        (when root (push (cons channel directory) (gethash root roots)))))
    roots))

(-> task-child--prepare-storage (configuration list hash-table) null)
(defun task-child--prepare-storage (configuration channels identities)
  "Persist every root's delivery evidence before any destructive cleanup."
  (maphash
   (lambda (root entries)
     (shell-log-persist-owner-delivery configuration root (mapcar #'first entries)))
   (task-child--storage-roots configuration channels identities))
  nil)

(-> task-child--delete-root-storage (configuration string list) null)
(defun task-child--delete-root-storage (configuration root entries)
  "Delete prepared ROOT's child ENTRIES while the caller holds every lease."
  (declare (ignore root))
  (labels ((delete-storage ()
             (dolist (entry entries)
               (conversation-delete-artifacts configuration (first entry))
               (platform-delete-directory-tree *platform* (rest entry) :validate t
                                               :if-does-not-exist                ':ignore)))

           (with-source-locks (remaining)
             (if remaining
                 (let* ((entry (first remaining))
                        (path
                         (merge-pathnames (make-pathname :name (first entry) :type "sexp")
                                          (rest entry))))
                   (call-with-file-lock (conversation-picker-source-lock-pathname path)
                                        (lambda () (with-source-locks (rest remaining)))))
                 (delete-storage))))
    (with-source-locks (sort (copy-list entries) #'string< :key #'first)))
  nil)
