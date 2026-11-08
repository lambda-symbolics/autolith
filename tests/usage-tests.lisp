(in-package #:autolith)

;;;; -- Usage Collection Tests --

(-> test-usage-storage-union () null)
(defun test-usage-storage-union ()
  "Measure metadata only, deduplicating hard links and overlapping roots."
  (let ((total (make-hash-table :test #'equal)))
    (test-call-with-function-replacements
     (list
      (list 'platform-path-status
            (lambda (platform path &key follow-links-p)
              (declare (ignore platform))
              (test-assert (not follow-links-p) "storage status does not follow links")
              (let ((name (file-namestring path)))
                (cond
                  ((member name '("usage" "nested") :test #'string=)
                   (make-instance 'platform-file-status
                                  :kind ':directory :identity (if (string= name "usage") 1 2)
                                  :size 0))
                  ((member name '("a" "hardlink") :test #'string=)
                   (make-instance 'platform-file-status :kind ':file :identity 3 :size 17))
                  ((string= name "b")
                   (make-instance 'platform-file-status :kind ':file :identity 4 :size 5))
                  ((string= name "link")
                   (make-instance 'platform-file-status :kind ':symbolic-link
                                  :identity 5 :size 1000))
                  (t
                   nil)))))
      (list 'platform-list-directory
            (lambda (platform path &key limit)
              (declare (ignore platform limit))
              (values (if (equal (last (pathname-directory path)) '("nested"))
                          '("b")
                          '("a" "hardlink" "nested" "link" "vanished"))
                      nil))))
     (lambda ()
       (multiple-value-bind (storage added)
           (usage--storage-scan "Data" #P"/usage/" total)
         (test-assert (= (usage-storage-bytes storage) 22) "hard links count once per root")
         (test-assert (= (usage-storage-files storage) 2) "unique regular files are counted")
         (test-assert (= (usage-storage-links storage) 1) "links are reported without traversal")
         (test-assert (= (usage-storage-errors storage) 1) "disappearing entries mark partial data")
         (test-assert (= added 22) "union starts with unique logical file bytes"))
       (multiple-value-bind (storage added)
           (usage--storage-scan "Overlap" #P"/usage/" total)
         (test-assert (= (usage-storage-bytes storage) 22) "root measurements are independent")
         (test-assert (zerop added) "overlapping roots add no duplicate bytes")))))
  nil)

(-> test-usage-storage-failures () null)
(defun test-usage-storage-failures ()
  "Distinguish an absent root from an inaccessible directory."
  (dolist (inaccessible-p '(nil t))
    (test-call-with-function-replacements
     (list (list 'platform-path-status
                 (lambda (platform path &key follow-links-p)
                   (declare (ignore platform path follow-links-p))
                   (when inaccessible-p
                     (error 'platform-error :operation ':status :pathname #P"/usage/"
                                            :message "Unavailable")))))
     (lambda ()
       (let ((storage (usage--storage-scan "Data" #P"/usage/" (make-hash-table :test #'equal))))
         (test-assert (zerop (usage-storage-bytes storage)) "no bytes were observed")
         (test-assert (= (usage-storage-errors storage) (if inaccessible-p 1 0))
                      "inaccessible storage is partial, absent storage is empty")))))
  nil)

(-> test-usage-storage-root-link () null)
(defun test-usage-storage-root-link ()
  "Skip a linked configured root even when its pathname has directory form."
  (with-test-fixture (':symbolic-links "usage root metadata")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let ((target (merge-pathnames "target/" root))
            (link   (merge-pathnames "linked" root)))
        (ensure-directories-exist target)
        (with-open-file (stream (merge-pathnames "contents" target) :direction ':output)
          (write-string "outside" stream))
        (test-fixture-make-symbolic-link *platform* (uiop:native-namestring target)
                                         (uiop:native-namestring link))
        (let ((storage (usage--storage-scan
                        "Linked" (uiop:ensure-directory-pathname link)
                        (make-hash-table :test #'equal))))
          (test-assert (= (usage-storage-links storage) 1) "the root link is observed itself")
          (test-assert (zerop (usage-storage-files storage)) "linked contents are not traversed")
          (test-assert (zerop (usage-storage-bytes storage)) "linked bytes are excluded")
          (test-assert (zerop (usage-storage-errors storage)) "skipping a root link is complete")))))
  nil)

(-> test-usage-process-snapshot () null)
(defun test-usage-process-snapshot ()
  "Deduplicate session hosts and descendants, retaining missing RSS measurements."
  (with-test-configuration (configuration)
    (let ((measured nil)
          (report   (make-usage-report)))
      (test-call-with-function-replacements
       (list
        (list 'current-process-id (lambda () 99))
        (list 'localgroup-endpoint-records
              (lambda (configuration)
                (declare (ignore configuration))
                (list (cons #P"/one" '(:registry :session-id "one" :pid 10))
                      (cons #P"/two" '(:registry :session-id "two" :pid 10)))))
        (list 'localgroup-owner-reachability
              (lambda (path identifier pid)
                (declare (ignore path identifier pid))
                ':reachable))
        (list 'platform-process-parent-snapshot
              (lambda (platform)
                (declare (ignore platform))
                (values '((10 . 12) (11 . 10) (11 . 10) (12 . 11)
                          (99 . 10) (100 . 99))
                        t)))
        (list 'platform-process-resident-bytes
              (lambda (platform pid)
                (declare (ignore platform))
                (push pid measured)
                (case pid (10
                           1024)
                          (11
                           0)
                          (99
                           4096)))))
       (lambda () (usage--collect-processes configuration report)))
      (test-assert (= (usage-report-sessions report) 1) "duplicate registry PIDs are one host")
      (test-assert (= (usage-report-processes report) 3) "cyclic descendants are unique, reporter excluded")
      (test-assert (= (usage-report-resident-bytes report) 1024) "RSS uses measured bytes including real zero")
      (test-assert (= (usage-report-resident-missing report) 1) "missing process is explicit")
      (test-assert (= (usage-report-reporter-bytes report) 4096) "reporter RSS is separate")
      (test-assert (not (member 100 measured)) "reporter descendants are excluded")
      (test-assert (= (length measured) (length (remove-duplicates measured)))
                   "each PID is queried once")))
  nil)

(-> test-usage-stored-counts () null)
(defun test-usage-stored-counts ()
  "Count active notes and agenda entries across stored workspaces without rule evaluation."
  (with-test-configuration (configuration)
    (let ((report  (make-usage-report))
          (removed (memory-remember configuration :title "Removed" :content "Old note")))
      (memory-forget configuration (memory-identifier removed))
      (memory-remember configuration :title "Global note" :content "One" :scope ':global)
      (memory-remember configuration :title "Workspace note" :content "Two")
      (agenda-add :configuration configuration :state (agenda-load configuration) :text "One item")
      (test-call-with-function-replacements
       (list (list 'knowledge-base-read
                   (lambda (configuration)
                     (declare (ignore configuration))
                     (error 'file-error :pathname #P"knowledge.sexp"))))
       (lambda () (usage--collect-counts configuration report)))
      (flet ((count-of (label)
               (rest (assoc label (usage-report-counts report) :test #'string=))))
        (test-assert (= (count-of "Memories") 2) "only active memories count across scopes")
        (test-assert (= (count-of "Agenda items") 1) "agenda item count is current")
        (test-assert (eql (count-of "Conversations") 0) "absent conversation store is measured empty")
        (test-assert (eql (count-of "Task conversations") 0) "absent task store is measured empty")
        (test-assert (null (count-of "Fact assertions")) "unreadable knowledge is unavailable")
        (test-assert (member "Structured knowledge" (usage-report-warnings report) :test #'string=)
                     "store failure is visible"))))
  nil)

(-> test-usage-task-counts () null)
(defun test-usage-task-counts ()
  "Count nonempty primary and task stores from metadata without parsing records."
  (with-test-configuration (configuration)
    (dolist (task-p '(nil t))
      (dolist (layout '(:legacy :chunk))
        (let* ((directory (if task-p
                              (merge-pathnames (format nil "tasks/group/~(~A~)/" layout)
                                               (config :data-root configuration))
                              (configuration-conversation-root configuration)))
               (identity  (merge-pathnames (format nil "~(~A~).sexp" layout) directory))
               (segment   (if (eq layout ':chunk)
                              (conversation-chunk-pathname identity 1)
                              identity)))
          (ensure-directories-exist segment)
          (with-open-file (stream segment :direction ':output)
            (write-string "metadata-only fixture" stream))
          (when (eq layout ':legacy)
            (let ((chunk (conversation-chunk-pathname identity 2)))
              (ensure-directories-exist chunk)
              (with-open-file (stream chunk :direction ':output)
                (write-string "metadata-only fixture" stream)))))))
    (ensure-directories-exist (merge-pathnames "tasks/group/empty/empty/"
                                              (config :data-root configuration)))
    (ensure-directories-exist (merge-pathnames "empty/"
                                              (configuration-conversation-root configuration)))
    (let ((report (make-usage-report)))
      (usage--collect-counts configuration report)
      (dolist (label '("Conversations" "Task conversations"))
        (test-assert (eql (rest (assoc label (usage-report-counts report) :test #'string=)) 2)
                     "nonempty stores count once across legacy and chunked layouts"))))
  nil)
