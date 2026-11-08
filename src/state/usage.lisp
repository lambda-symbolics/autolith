(in-package #:autolith)

;;;; -- Usage Observations --

(defstruct usage-storage
  "Logical regular-file storage under one configured root, without following links."
  (name "" :type string)
  (pathname #P"" :type pathname)
  (bytes 0 :type (integer 0))
  (files 0 :type (integer 0))
  (links 0 :type (integer 0))
  (errors 0 :type (integer 0)))

(defstruct usage-report
  "Best-effort storage and process observations, with unavailable values kept distinct."
  (counts nil :type list)
  (storage nil :type list)
  (disk-bytes 0 :type (integer 0))
  (disk-partial-p nil :type boolean)
  (sessions nil :type (option integer))
  (processes nil :type (option integer))
  (resident-bytes nil :type (option integer))
  (resident-missing 0 :type (integer 0))
  (descendants-available-p nil :type boolean)
  (reporter-bytes nil :type (option integer))
  (warnings nil :type list))

(-> usage--entry-pathname (pathname) pathname)
(defun usage--entry-pathname (pathname)
  "Use file form for a non-root directory so metadata inspection sees its link."
  (let* ((directory (pathname-directory pathname))
         (name      (first (last directory))))
    (if (and (uiop:directory-pathname-p pathname) (stringp name))
        (make-pathname :directory (butlast directory) :name name :type nil
                       :defaults pathname)
        pathname)))

(-> usage--storage-scan (string pathname hash-table) (values usage-storage integer))
(defun usage--storage-scan (name pathname total-identities)
  "Measure PATHNAME, returning its observation and bytes new to TOTAL-IDENTITIES.

Deduplicate hard links within each root and across the union. Do not open file
contents or follow symbolic links. Errors and disappearing entries mark a partial
observation; an absent root is empty. Directory sizes are not file content bytes."
  (let ((result (make-usage-storage :name name :pathname pathname))
        (seen   (make-hash-table :test #'equal))
        (queue  (list pathname))
        (added  0))
    (loop while queue
          for path = (pop queue)
          do (handler-case
                 (let ((status (platform-path-status *platform* (usage--entry-pathname path))))
                   (cond
                     ((null status)
                      (unless (equal path pathname)
                        (incf (usage-storage-errors result))))
                     ((eq (platform-file-status-kind status) ':symbolic-link)
                      (incf (usage-storage-links result)))
                     (t
                      (let ((identity (platform-file-status-identity status)))
                        (unless (gethash identity seen)
                          (setf (gethash identity seen) t)
                          (case (platform-file-status-kind status)
                            (:file
                             (let ((size (platform-file-status-size status)))
                               (incf (usage-storage-files result))
                               (incf (usage-storage-bytes result) size)
                               (unless (gethash identity total-identities)
                                 (setf (gethash identity total-identities) t)
                                 (incf added size))))
                            (:directory
                             (let ((directory (uiop:ensure-directory-pathname path)))
                               (multiple-value-bind (names truncated-p)
                                   (platform-list-directory *platform* directory)
                                 (when truncated-p
                                   (incf (usage-storage-errors result)))
                                 (dolist (entry names)
                                   (push (make-pathname :name entry :type nil
                                                       :defaults directory)
                                         queue)))))))))))
               ((or platform-error file-error) ()
                 (incf (usage-storage-errors result)))))
    (values result added)))

(-> usage--collect-storage (configuration usage-report) null)
(defun usage--collect-storage (configuration report)
  "Measure configured storage roots once each and retain a deduplicated union."
  (let ((identities (make-hash-table :test #'equal)))
    (dolist (entry '(("Data" . :data-root) ("State" . :state-root)
                     ("Cache" . :cache-root) ("Config" . :config-root)))
      (multiple-value-bind (storage added)
          (usage--storage-scan (first entry) (config (rest entry) configuration) identities)
        (push storage (usage-report-storage report))
        (incf (usage-report-disk-bytes report) added)
        (when (plusp (usage-storage-errors storage))
          (setf (usage-report-disk-partial-p report) t))))
    (setf (usage-report-storage report) (nreverse (usage-report-storage report))))
  nil)

(-> usage--observe-count (usage-report string function) null)
(defun usage--observe-count (report label function)
  "Add FUNCTION's count to REPORT; retain an unavailable marker on store failure."
  (let ((count (handler-case (funcall function)
                 (error ()
                   (push label (usage-report-warnings report))
                   nil))))
    (push (cons label count) (usage-report-counts report)))
  nil)

(-> usage--conversation-count (list) (integer 0))
(defun usage--conversation-count (identities)
  "Count nonempty regular active log segments in IDENTITIES using metadata only."
  (loop for identity in identities
        for active = (conversation-storage-active-pathname identity)
        for status = (and active (platform-path-status *platform* active))
        count (and status
                   (eq (platform-file-status-kind status) ':file)
                   (plusp (platform-file-status-size status)))))

(-> usage--collect-counts (configuration usage-report) null)
(defun usage--collect-counts (configuration report)
  "Count current stored definitions across all workspaces without running rules."
  (usage--observe-count
   report "Conversations"
   (lambda ()
     (usage--conversation-count
      (conversation-storage-identities (configuration-conversation-root configuration)))))
  (usage--observe-count
   report "Task conversations"
   (lambda ()
     (usage--conversation-count
      (conversation-task-pathnames (config :data-root configuration)))))
  (usage--observe-count report "Memories"
                        (lambda () (length (memory-list configuration :visibility ':all))))
  (usage--observe-count
   report "Agenda items"
   (lambda ()
     (loop for agenda in (agenda-state-records (agenda--read configuration))
           sum (length (workspace-agenda-items agenda)))))
  (usage--observe-count report "Retained checkpoints"
                        (lambda () (length (generation-list configuration))))
  (handler-case
      (let ((base (knowledge-base-read configuration)))
        (push (cons "Fact assertions"
                    (loop for support being the hash-values of (knowledge-base-supports base)
                          count (null (getf support :retracted-at))))
              (usage-report-counts report))
        (dolist (entry (list (cons "Predicate definitions" (knowledge-base-schemas base))
                            (cons "Rule definitions" (knowledge-base-rules base))
                            (cons "Task methods" (knowledge-base-methods-table base))
                            (cons "Tool contracts" (knowledge-base-contracts-table base))))
          (push (cons (first entry) (hash-table-count (rest entry)))
                (usage-report-counts report))))
    (error ()
      (push "Structured knowledge" (usage-report-warnings report))
      (dolist (label '("Fact assertions" "Predicate definitions" "Rule definitions"
                       "Task methods" "Tool contracts"))
        (push (cons label nil) (usage-report-counts report)))))
  (setf (usage-report-counts report) (nreverse (usage-report-counts report)))
  nil)

(-> usage--process-closure (list list) list)
(defun usage--process-closure (roots parents)
  "Return unique ROOTS and descendants in PARENTS, tolerating duplicate rows and cycles."
  (let ((children (make-hash-table))
        (seen     (make-hash-table))
        (pending  (copy-list roots))
        (result   nil))
    (dolist (entry parents)
      (push (first entry) (gethash (rest entry) children)))
    (loop while pending
          for pid = (pop pending)
          unless (gethash pid seen)
            do (setf (gethash pid seen) t)
               (push pid result)
               (dolist (child (gethash pid children))
                 (push child pending)))
    (nreverse result)))

(-> usage--collect-processes (configuration usage-report) null)
(defun usage--collect-processes (configuration report)
  "Measure reachable registered session hosts and their discovered descendants.

Do not prune discovery records. Exclude the reporting CLI and its descendants
from the session sum; measure the CLI separately. Missing metrics are explicit."
  (setf (usage-report-reporter-bytes report)
        (platform-process-resident-bytes *platform* (current-process-id)))
  (let ((roots            nil)
        (roots-complete-p t))
    (handler-case
        (let ((records (localgroup-endpoint-records configuration))
              (unknown 0))
          (dolist (entry records)
            (let* ((record (rest entry))
                   (pid    (getf (rest record) :pid)))
              (if (eq (localgroup-owner-reachability
                       (first entry) (localgroup--record-session-id record) pid)
                      ':reachable)
                  (pushnew pid roots)
                  (incf unknown))))
          (setf (usage-report-sessions report) (length roots))
          (when (plusp unknown)
            (setf roots-complete-p nil)
            (push "Some registered session endpoints were unavailable"
                  (usage-report-warnings report))))
      (error ()
        (push "Session discovery" (usage-report-warnings report))
        (return-from usage--collect-processes nil)))
    (multiple-value-bind (parents available-p)
        (platform-process-parent-snapshot *platform*)
      (let ((pids     (set-difference
                       (usage--process-closure roots parents)
                       (usage--process-closure (list (current-process-id)) parents)
                       :test #'eql))
            (sum      0)
            (measured 0))
        (setf (usage-report-descendants-available-p report) available-p
              (usage-report-processes report) (length pids))
        (dolist (pid pids)
          (let ((bytes (platform-process-resident-bytes *platform* pid)))
            (if bytes
                (progn (incf sum bytes) (incf measured))
                (incf (usage-report-resident-missing report)))))
        (when (or (plusp measured)
                  (and available-p roots-complete-p (null pids)))
          (setf (usage-report-resident-bytes report) sum)))))
  nil)

(-> usage-collect (configuration) usage-report)
(defun usage-collect (configuration)
  "Collect best-effort user-wide stored-data, logical disk, and process RSS usage.

This snapshot is not atomic across stores or processes. Counts do not replay
conversation bodies, evaluate knowledge rules, or inspect credential contents."
  (let ((report (make-usage-report)))
    (usage--collect-counts configuration report)
    (usage--collect-processes configuration report)
    (usage--collect-storage configuration report)
    report))
