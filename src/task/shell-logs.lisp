(in-package #:autolith)

;;;; -- Private Shell Capture Artifacts --

(defparameter *shell-log-capture-byte-limit* (* 64 1024 1024)
  "Maximum retained raw bytes per stream.")

(defparameter *shell-log-retention-byte-limit* (* 1024 1024 1024)
  "Maximum reserved capture bytes across private artifacts.")

(defparameter *shell-log-retention-count-limit* 256
  "Maximum artifacts containing capture files.")

(defparameter *shell-log-retention-age* (* 7 24 60 60)
  "Completed logs become age-eligible after this interval; prune on next allocation.")

(defparameter *shell-log-tombstone-count-limit* 512
  "Maximum retained pruned manifests; older references then report missing.")

(defparameter *shell-log-tombstone-age* (* 30 24 60 60)
  "Maximum tombstone age in seconds.")

(defparameter *shell-log-manifest-byte-limit* 16384
  "Maximum portable manifest bytes, separate from raw capture reservations.")

(defvar *shell-log-lock* (make-lock "shell log artifacts")
  "Serialize reservation, manifest publication and pruning.")

(defvar *shell-log-active* (make-hash-table :test #'equal)
  "Capture directories currently owned by running operations.")

(define-condition shell-log-error (tool-error)
  ((path :initarg :path :initform nil :reader shell-log-error-path
         :documentation "Managed path involved in the failure."))
  (:documentation "Private shell capture could not be safely allocated or inspected."))

(defclass shell-log-artifact ()
  ((configuration :initarg :configuration :reader shell-log-artifact-configuration
                  :documentation "Configuration owning private storage.")
   (job :initarg :job :initform nil :reader shell-log-artifact-job
        :documentation "Optional live job, used to capture final detachment state.")
   (capture-directory :initarg :capture-directory :reader shell-log-capture-directory
                      :documentation "Caller-owned private capture directory.")
   (metadata :initarg :metadata :accessor shell-log-artifact-metadata
             :documentation "Portable durable capture manifest."))
  (:documentation "One invocation's private raw captures and durable ownership."))

(-> shell-log--fail (string &optional t) nil)
(defun shell-log--fail (message &optional path)
  "Signal a typed refusal before an unsafe filesystem operation."
  (error 'shell-log-error :message message :path path :tool-name "shell.run"))

(-> shell-log--component-p (t) boolean)
(defun shell-log--component-p (value)
  "Accept only opaque identifiers safe in both native paths and URI segments."
  (and (stringp value) (plusp (length value)) (<= (length value) 128)
       (every (lambda (character)
                (or (find character "-_" :test #'char=)
                    (find character "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          :test #'char=))) value)
       t))

(-> shell-log--capture-file-p (t) boolean)
(defun shell-log--capture-file-p (value)
  "Accept only the executor's plain basename, never a path or manifest name."
  (and (stringp value) (<= 1 (length value) 240)
       (not (member value '("." ".." "manifest.sexp" "owner.lock") :test #'string=))
       (every (lambda (character)
                (or (find character "-." :test #'char=)
                    (find character "_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          :test #'char=))) value)
       t))

(-> shell-log--safe-path (configuration pathname) pathname)
(defun shell-log--safe-path (configuration path)
  "Reject links and non-directory ancestors under the managed data root."
  (handler-case
      (let ((root (uiop:ensure-directory-pathname (config :data-root configuration))))
        (unless (uiop:subpathp path root)
          (shell-log--fail "Shell log path escaped private storage." path))
        (loop for current = path then (uiop:pathname-parent-directory-pathname current)
              while (and current (uiop:subpathp current root))
              for status = (platform-path-status *platform* current :follow-links-p nil)
              when (and status (not (member (platform-file-status-kind status)
                                           '(:file :directory))))
                do (shell-log--fail "Shell log storage contains a link or special file." current)
              when (equal current root) do (return)))
    (platform-error (condition)
      (shell-log--fail (format nil "Cannot inspect shell log storage: ~A" condition) path)))
  path)

(-> shell-log--manifest-path (pathname) pathname)
(defun shell-log--manifest-path (directory)
  "Return the private portable manifest in DIRECTORY."
  (merge-pathnames "manifest.sexp" directory))

(defmacro with-shell-log-lock ((configuration) &body body)
  "Serialize lifecycle changes in this image and across processes sharing storage."
  (let ((path     (gensym "PATH"))
        (settings (gensym "CONFIGURATION")))
    `(let* ((,settings ,configuration)
            (,path (merge-pathnames "tasks/shell-log.lock" (config :data-root ,settings))))
       (with-lock-held (*shell-log-lock*)
         (shell-log--safe-path ,settings ,path)
         (ensure-directories-exist ,path)
         (call-with-file-lock ,path
           (lambda ()
             (platform-make-private *platform* ,path)
             ,@body))))))

(-> shell-log--release-owner (pathname) null)
(defun shell-log--release-owner (capture-directory)
  "Release the process lease and remove its local thread pin."
  (let ((entry (gethash (namestring capture-directory) *shell-log-active*)))
    (when (consp entry) (lease-release (rest entry)))
    (remhash (namestring capture-directory) *shell-log-active*))
  nil)

(-> shell-log--active-p (pathname) boolean)
(defun shell-log--active-p (directory)
  "Test a live local thread or a foreign process lease, never a stale heap pin."
  (let ((entry (gethash (namestring directory) *shell-log-active*))
        (path  (merge-pathnames "owner.lock" directory)))
    (when entry
      (if (and (consp entry) (thread-alive-p (first entry)) (lease-held-p (rest entry)))
          (return-from shell-log--active-p t)
          (shell-log--release-owner directory)))
    (let ((status (platform-path-status *platform* path :follow-links-p nil)))
      (when status
        (unless (eq (platform-file-status-kind status) ':file)
          (shell-log--fail "Unsafe shell log owner lock." path))
        (handler-case
            (let ((lease (lease-acquire path)))
              (lease-release lease)
              nil)
          (file-lock-busy () t))))))

(-> shell-log--measure-captures (configuration pathname list) integer)
(defun shell-log--measure-captures (configuration directory metadata)
  "Measure only safe regular capture files and report oversized storage truthfully."
  (let ((total         0)
        (active-p      (eq (getf metadata :state) ':active))
        (interrupted-p (eq (getf metadata :state) ':interrupted)))
    (unless (eq (getf metadata :state) ':pruned)
      (loop for captures on (getf metadata :captures)
            for capture = (first captures)
            do (unless (shell-log--capture-file-p (getf capture :file))
                 (shell-log--fail "Invalid shell capture filename in manifest."))
               (let* ((path   (shell-log--safe-path
                               configuration (merge-pathnames (getf capture :file) directory)))
                      (status (handler-case
                                  (platform-path-status *platform* path :follow-links-p nil)
                                (platform-error (condition)
                                  (shell-log--fail
                                   (format nil "Cannot inspect shell capture: ~A" condition)
                                   path))))
                      (size   (if status (platform-file-status-size status) 0))
                      (limit  (getf metadata :capture-byte-limit)))
                 (when (and status (not (eq (platform-file-status-kind status) ':file)))
                   (shell-log--fail "Shell capture is not a regular file." path))
                 (unless (and (integerp limit) (plusp limit))
                   (shell-log--fail "Shell capture manifest has no finite byte allowance."))
                 (incf total size)
                 (cond
                   ((> size limit)
                    (setf (getf capture :oversized-p) t
                          (getf capture :complete-p) nil
                          (getf capture :observed-byte-count-known-p) nil
                          (getf capture :status) ':limit))
                   ((or active-p interrupted-p)
                    (setf (getf capture :complete-p) nil
                          (getf capture :observed-byte-count-known-p) nil
                          (getf capture :status) (if active-p ':capturing ':interrupted)))
                   ((null status)
                    (setf (getf capture :complete-p) nil
                          (getf capture :status) ':missing))
                   ((/= size (getf capture :byte-count 0))
                    (setf (getf capture :complete-p) nil
                          (getf capture :observed-byte-count-known-p) nil
                          (getf capture :status) ':interrupted)))
                 (setf (getf capture :byte-count) size
                       (first captures) capture))))
    total))

(-> shell-log--read-manifest (configuration pathname &key (:measure-p boolean)) list)
(defun shell-log--read-manifest (configuration directory &key (measure-p t))
  "Read bounded ownership metadata; optionally reconcile safe capture file sizes.
Disable measurement until resource resolution has checked ancestor ownership."
  (let* ((path     (shell-log--safe-path configuration (shell-log--manifest-path directory)))
         (metadata (task-continuity--read path)))
    (when (and (eq (getf metadata :state) ':active)
               (not (shell-log--active-p directory)))
      (setf (getf metadata :state) ':interrupted
            (getf metadata :status) ':interrupted))
    (when measure-p
      (setf (getf metadata :accounted-bytes)
            (max (if (member (getf metadata :state) '(:closed :pruned))
                     0
                     (getf metadata :reserved-bytes 0))
                 (shell-log--measure-captures configuration directory metadata))))
    metadata))

(-> shell-log--write-manifest (shell-log-artifact) pathname)
(defun shell-log--write-manifest (artifact)
  "Atomically publish ARTIFACT metadata and restrict its permissions."
  (let* ((path     (shell-log--manifest-path (shell-log-capture-directory artifact)))
         (metadata (shell-log-artifact-metadata artifact)))
    (when (> (length (utf8-string-to-octets (task--write-readable-sexp metadata)))
             *shell-log-manifest-byte-limit*)
      (shell-log--fail "Shell capture ownership metadata exceeds its finite allowance."))
    (shell-log--safe-path (shell-log-artifact-configuration artifact) path)
    (task-continuity--write path metadata)
    (platform-make-private *platform* path)
    path))

(-> shell-log-reference (shell-log-artifact &optional keyword) string)
(defun shell-log-reference (artifact &optional (stream ':output))
  "Return a stable opaque raw stream reference, independent of filesystem paths."
  (unless (member stream '(:output :error))
    (shell-log--fail "Unknown shell log stream."))
  (let ((metadata (shell-log-artifact-metadata artifact)))
    (format nil "shell-log:~A/~A/~A/~(~A~)"
            (getf metadata :root-conversation) (getf metadata :execution-id)
            (getf metadata :artifact-id) stream)))

(-> shell-log-capture-byte-limit (shell-log-artifact) (integer 1))
(defun shell-log-capture-byte-limit (artifact)
  "Return the per-stream allowance reserved before ARTIFACT's launch."
  (getf (shell-log-artifact-metadata artifact) :capture-byte-limit))

(-> shell-log--group-directories (configuration pathname) list)
(defun shell-log--group-directories (configuration group)
  "Enumerate one task group's captures without traversing links."
  (shell-log--safe-path configuration group)
  (when (uiop:directory-exists-p group)
    (loop for execution in (uiop:subdirectories group)
          for logs = (merge-pathnames "shell-log/" execution)
          append (progn
                   (shell-log--safe-path configuration logs)
                   (when (uiop:directory-exists-p logs)
                     (loop for directory in (uiop:subdirectories logs)
                           collect (shell-log--safe-path configuration directory)))))))

(-> shell-log--directories (configuration) list)
(defun shell-log--directories (configuration)
  "Enumerate managed artifact directories without traversing links."
  (let ((root (merge-pathnames "tasks/" (config :data-root configuration))))
    (shell-log--safe-path configuration root)
    (when (uiop:directory-exists-p root)
      (loop for session in (uiop:subdirectories root)
            append (shell-log--group-directories configuration session)))))

(defparameter *shell-log-sync-evidence-cache* nil
  "Owner synchronous delivery indexes, freshly bound for each prune; NIL means uncached.")

(-> shell-log--sync-evidence (pathname) hash-table)
(defun shell-log--sync-evidence (identity)
  "Index the captures that IDENTITY's active segment records as delivered.

Keys are lists of an execution and an artifact identifier. Each value lists the
provider call identifiers of the tool results that delivered the capture, with
:GOAL for mission gate evidence. Only the active segment is read, so one check
costs at most one segment however long the history grows."
  (let ((index  (make-hash-table :test #'equal))
        (active (conversation-storage-active-pathname identity)))
    (flet ((note (details marker)
             (when (and (proper-list-p details) (evenp (length details)))
               (dolist (log (getf details :shell-logs))
                 (when (eq (getf log :state) ':closed)
                   (push marker
                         (gethash (list (getf log :execution-id) (getf log :artifact-id))
                                  index)))))))
      (when active
        (conversation--map-segment-records
         identity active
         (lambda (record)
           (case (first record)
             (:tool-result
              (when (member (getf (rest record) :status) '(:ok :error))
                (note (getf (rest record) :details) (getf (rest record) :call-id))))
             (:goal
              (dolist (gate (getf (rest record) :gates))
                (when (member (getf gate :status) '(:passed :failed))
                  (note (getf gate :tool-details) ':goal)))))))))
    index))

(-> shell-log--cached-sync-evidence (pathname) hash-table)
(defun shell-log--cached-sync-evidence (identity)
  "Return IDENTITY's delivery index, reading its active segment once per prune."
  (if *shell-log-sync-evidence-cache*
      (let ((key (namestring identity)))
        (or (gethash key *shell-log-sync-evidence-cache*)
            (setf (gethash key *shell-log-sync-evidence-cache*)
                  (shell-log--sync-evidence identity))))
      (shell-log--sync-evidence identity)))

(-> shell-log--sync-settled-p (list) boolean)
(defun shell-log--sync-settled-p (metadata)
  "Recognize a synchronous capture old enough that no delivery can still be pending.

A synchronous tool call records its result when the command ends, or never if
the process died first, so after the retention age the capture is history."
  (> (- (get-universal-time) (getf metadata :created-at 0))
     *shell-log-retention-age*))

(-> shell-log--sync-delivered-p (configuration list) boolean)
(defun shell-log--sync-delivered-p (configuration metadata)
  "Require exact-owner durable tool or mission-gate evidence for terminal sync logs.
Match execution, artifact and originating provider call, including stale
manifests. Evidence comes from the owner's active segment; captures older than
the retention age need none."
  (let ((root      (getf metadata :root-conversation))
        (owner     (getf metadata :completion-owner-conversation))
        (execution (getf metadata :execution-id))
        (artifact  (getf metadata :artifact-id))
        (call-id   (getf metadata :parent-call-id)))
    (and (member (getf metadata :state) '(:closed :interrupted))
         (not (getf metadata :detached-p t))
         (every #'shell-log--component-p (list root owner))
         (or (shell-log--sync-settled-p metadata)
             (handler-case
                 (let* ((identity
                          (if (equal owner root)
                              (conversation-pathname-for-id configuration owner)
                              (shell-log--safe-path
                               configuration
                               (merge-pathnames
                                (make-pathname :name owner :type "sexp")
                                (merge-pathnames (format nil "~A/" owner)
                                                 (task--artifact-group-root configuration root))))))
                        (header (conversation-peek-header identity)))
                   (and (equal owner (getf (rest header) :id))
                        (let ((markers (gethash (list execution artifact)
                                                (shell-log--cached-sync-evidence identity))))
                          (if call-id
                              (member call-id markers :test #'equal)
                              markers))
                        t))
               (error ()
                 nil)))
         t)))

(defparameter *shell-log-receipt-cache* nil
  "Exact owner receipt snapshots, freshly bound for each prune; NIL means uncached.")

(-> shell-log--delivery-recorded-p (list) boolean)
(defun shell-log--delivery-recorded-p (metadata)
  "Recognize positive delivery authority recorded before owner retention."
  (let ((time (getf metadata :delivered-at)))
    (and (integerp time) (plusp time))))

(-> shell-log--delivered-p (configuration list) boolean)
(defun shell-log--delivered-p (configuration metadata)
  "Require existing exact-owner synchronous evidence or a durable completion receipt."
  (or (shell-log--sync-delivered-p configuration metadata)
      (let ((receipts (task--owner-receipts configuration (getf metadata :root-conversation)
                                           (getf metadata :completion-owner-conversation)
                                           :cache *shell-log-receipt-cache*)))
        (and receipts
             (gethash (task-completion--receipt (getf metadata :execution-id)) receipts)
             t))))

(-> shell-log--prunable-p (configuration pathname list) boolean)
(defun shell-log--prunable-p (configuration directory metadata)
  "Protect active captures and undelivered job completion evidence."
  (and (not (shell-log--active-p directory))
       (not (eq (getf metadata :state) ':active))
       (or (null (getf metadata :job-id))
           (null (getf metadata :captures))
           (shell-log--delivery-recorded-p metadata)
           (shell-log--delivered-p configuration metadata))
       t))

(-> shell-log-persist-owner-delivery (configuration string string) null)
(defun shell-log-persist-owner-delivery (configuration root owner)
  "Preserve exact positive delivery evidence before deleting a child owner's storage.
Any manifest inspection or publication failure vetoes retention of that owner."
  (unless (and (every #'shell-log--component-p (list root owner))
               (not (equal root owner)))
    (shell-log--fail "Invalid retained shell log owner identity."))
  (with-shell-log-lock (configuration)
    (let ((*shell-log-receipt-cache*       (make-hash-table :test #'equal))
          (*shell-log-sync-evidence-cache* (make-hash-table :test #'equal))
          (group                           (task--artifact-group-root configuration root)))
      (dolist (directory (shell-log--group-directories configuration group))
        (let ((metadata (shell-log--read-manifest configuration directory :measure-p nil)))
          (unless metadata
            (shell-log--fail "Cannot read shell capture ownership before retention." directory))
          (when (and (equal root (getf metadata :root-conversation))
                     (equal owner (getf metadata :completion-owner-conversation)))
            (let ((execution (getf metadata :execution-id))
                  (artifact  (getf metadata :artifact-id)))
              (unless (and (every #'shell-log--component-p (list execution artifact))
                           (uiop:pathname-equal directory
                             (merge-pathnames (format nil "~A/shell-log/~A/" execution artifact) group)))
                (shell-log--fail "Shell capture identity disagrees with its directory." directory)))
            (when (and (member (getf metadata :state) '(:closed :interrupted))
                       (getf metadata :job-id) (getf metadata :captures)
                       (not (shell-log--delivery-recorded-p metadata))
                       (not (shell-log--active-p directory))
                       (shell-log--delivered-p configuration metadata))
              (setf (getf metadata :delivered-at) (get-universal-time))
              (shell-log--write-manifest
               (make-instance 'shell-log-artifact :configuration configuration
                              :capture-directory directory :metadata metadata))))))))
  nil)

(-> shell-log--manifest-staging-p (pathname) boolean)
(defun shell-log--manifest-staging-p (file)
  "Recognize the private staging names used by sexp-store snapshot publication."
  (let ((name (pathname-name file)))
    (and (equal "staging" (pathname-type file))
         (stringp name) (<= 13 (length name) 24)
         (uiop:string-prefix-p ".sexp-store-" name)
         (every (lambda (character) (find character "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                (subseq name 12))
         t)))

(-> shell-log--scan-manifest (configuration pathname) list)
(defun shell-log--scan-manifest (configuration capture-directory)
  "Read a manifest, reclaiming interrupted unpublished allocations under the lock."
  (if (probe-file (shell-log--safe-path configuration (shell-log--manifest-path capture-directory)))
      (shell-log--read-manifest configuration capture-directory)
      (progn
        (unless (shell-log--active-p capture-directory)
          (unless (and (null (uiop:subdirectories capture-directory))
                       (every (lambda (file)
                                (and (or (string= "owner.lock" (file-namestring file))
                                         (shell-log--manifest-staging-p file))
                                     (shell-log--safe-path configuration file)
                                     (eq ':file (platform-file-status-kind
                                                 (platform-path-status *platform* file :follow-links-p nil)))))
                              (uiop:directory-files capture-directory)))
            (shell-log--fail "Unpublished shell log contains unexpected files." capture-directory))
          (platform-delete-directory-tree *platform* capture-directory :validate t))
        nil)))

(-> shell-log--trim-tombstones (configuration) null)
(defun shell-log--trim-tombstones (configuration)
  "Bound tombstone metadata separately, deleting only already-pruned artifacts."
  (let* ((entries
           (loop for directory in (shell-log--directories configuration)
                 for metadata = (shell-log--scan-manifest configuration directory)
                 when (eq (getf metadata :state) ':pruned)
                   collect (list directory metadata)))
         (count                     (length entries)))
    (dolist (entry (sort entries #'< :key (lambda (entry) (getf (second entry) :pruned-at 0))))
      (destructuring-bind (directory metadata) entry
        (when (or (> count *shell-log-tombstone-count-limit*)
                  (> (- (get-universal-time) (getf metadata :pruned-at 0))
                     *shell-log-tombstone-age*))
          (unless (and (null (uiop:subdirectories directory))
                       (every (lambda (file)
                                (and (or (member (file-namestring file) '("manifest.sexp" "owner.lock") :test #'string=)
                                         (shell-log--manifest-staging-p file))
                                     (shell-log--safe-path configuration file)
                                     (eq ':file (platform-file-status-kind
                                                 (platform-path-status *platform* file :follow-links-p nil)))))
                              (uiop:directory-files directory)))
            (shell-log--fail "Pruned shell log directory contains unexpected files." directory))
          (platform-delete-directory-tree *platform* directory :validate t)
          (decf count)))))
  nil)

(-> shell-log--prune (configuration &key (:reservation integer)) null)
(defun shell-log--prune (configuration &key (reservation 0))
  "Reserve finite storage, pruning only closed receipt-backed captures.
Keep manifests as tombstones so old references have truthful diagnostics."
  (let* ((*shell-log-receipt-cache*       (make-hash-table :test #'equal))
         (*shell-log-sync-evidence-cache* (make-hash-table :test #'equal))
         (entries
           (loop for directory in (shell-log--directories configuration)
                 for metadata = (shell-log--scan-manifest configuration directory)
                 when (and metadata (not (eq (getf metadata :state) ':pruned)))
                   collect (list directory metadata (getf metadata :accounted-bytes 0))))
         (total                           (loop for entry in entries sum (third entry)))
         (count                           (length entries)))
    (dolist (entry (sort entries #'< :key (lambda (entry) (getf (second entry) :created-at 0))))
      (destructuring-bind (directory metadata accounted-bytes) entry
        (when (and (or (> (+ total reservation) *shell-log-retention-byte-limit*)
                       (>= count *shell-log-retention-count-limit*)
                       (> (- (get-universal-time) (getf metadata :created-at 0))
                          *shell-log-retention-age*))
                   (shell-log--prunable-p configuration directory metadata))
          (dolist (capture (getf metadata :captures))
            (unless (shell-log--capture-file-p (getf capture :file))
              (shell-log--fail "Invalid shell capture filename in manifest."))
            (let ((path (shell-log--safe-path configuration
                                            (merge-pathnames (getf capture :file) directory))))
              (when (probe-file path) (platform-delete-file *platform* path))))
          (decf total accounted-bytes)
          (decf count)
          (setf (getf metadata :state) ':pruned
                (getf metadata :pruned-at) (get-universal-time))
          (shell-log--write-manifest
           (make-instance 'shell-log-artifact :configuration configuration
                          :capture-directory directory :metadata metadata)))))
    (shell-log--trim-tombstones configuration)
    (when (or (> (+ total reservation) *shell-log-retention-byte-limit*)
              (>= count *shell-log-retention-count-limit*))
      (shell-log--fail "Private shell log storage is full; protected captures cannot be pruned.")))
  nil)

(-> shell-log-allocate
    (tool-context &key (:job t) (:merge-output-p boolean) (:receive (option function)))
    shell-log-artifact)
(defun shell-log-allocate (context &key job (merge-output-p t) receive)
  "Allocate private execution-owned storage and persist ownership BEFORE launch.

Pruning runs with interrupts enabled, so garbage collection can proceed while
it reads manifests and owner evidence. RECEIVE, when supplied, gets the
artifact with interrupts disabled at the instant it is published, so a caller
that releases it from an UNWIND-PROTECT cleanup never misses a published
capture."
  (with-shell-log-lock ((tool-context-configuration context))
    (let* ((configuration        (tool-context-configuration context))
           (agent                (tool-context-agent context))
           (parent-job           (and (typep agent 'task-child-agent) (task-child-agent-job agent)))
           (root                 (if job (session-job-root-conversation-identifier job)
                                     (if agent (task-parent-root-conversation-identifier agent)
                                         (conversation-identifier (tool-context-conversation context)))))
           (execution            (if job (session-job-execution-identifier job) (daemon-random-token)))
           (identifier           (daemon-random-token))
           (reservation          (* *shell-log-capture-byte-limit* (if merge-output-p 1 2)))
           (directory            (merge-pathnames (format nil "~A/shell-log/~A/" execution identifier)
                                                  (task--artifact-group-root configuration root)))
           (allocation-started-p nil)
           (published-p          nil)
           (lease                nil))
      (unless (every #'shell-log--component-p (list root execution identifier))
        (shell-log--fail "Invalid private shell log execution identity."))
      (unwind-protect
           (handler-case
               (progn
                 (shell-log--prune configuration :reservation reservation)
                 (shell-log--safe-path configuration directory)
                 (setf allocation-started-p t)
                 (ensure-directories-exist (merge-pathnames "manifest.sexp" directory))
                 (loop for current = directory then (uiop:pathname-parent-directory-pathname current)
                       while (uiop:subpathp current (config :data-root configuration))
                       do (platform-make-private *platform* current)
                       when (equal current (config :data-root configuration)) do (return))
                 (let ((artifact
                         (make-instance 'shell-log-artifact :configuration configuration :job job
                                        :capture-directory directory
                                        :metadata
                                        (list :version 1 :root-conversation root :execution-id execution
                                              :artifact-id identifier :state ':active
                                              :created-at         (get-universal-time)
                                              :job-id             (and job (session-job-identifier job))
                                              :parent-call-id     (and job (session-job-parent-call-id job))
                                              :detached-p         (and job (session-job-detached-p job))
                                              :owner-execution-ids
                                              (remove-duplicates
                                               (append (when parent-job
                                                         (list (session-job-execution-identifier parent-job)))
                                                       (when (and job agent)
                                                         (task-continuity--owner-executions job agent)))
                                               :test #'equal)
                                              :completion-owner-conversation
                                              (and job (session-job-completion-owner-conversation job))
                                              :merge-output-p     merge-output-p
                                              :capture-byte-limit *shell-log-capture-byte-limit*
                                              :reserved-bytes     reservation))))
                   (setf lease (lease-acquire (merge-pathnames "owner.lock" directory)))
                   (platform-make-private *platform* (merge-pathnames "owner.lock" directory))
                   (shell-log--write-manifest artifact)
                   (sb-sys:without-interrupts
                     (setf (gethash (namestring directory) *shell-log-active*) (cons (current-thread) lease)
                           published-p t)
                     (when receive
                       (funcall receive artifact)))
                   artifact))
             (shell-log-error (condition)
               (error condition))
             (error (condition)
               (shell-log--fail (format nil "Could not allocate private shell capture: ~A" condition)
                                directory)))
        (unless published-p
          (when lease (lease-release lease))
          (remhash (namestring directory) *shell-log-active*)
          (when allocation-started-p
            (sb-sys:without-interrupts
              ; mallet:suppress no-ignore-errors -- best-effort removal of an unpublished capture during unwinding
              (ignore-errors
                (platform-delete-directory-tree *platform* directory :validate t)))))))))

(-> shell-log--capture-metadata (shell-log-artifact t keyword) list)
(defun shell-log--capture-metadata (artifact capture stream)
  "Project generic capture evidence without exposing model-controlled paths."
  (when capture
    (let* ((path      (cl-exec-sandbox:sandbox-capture-path capture))
           (directory (shell-log-capture-directory artifact)))
      (unless (equal (uiop:pathname-directory-pathname path) directory)
        (shell-log--fail "Executor returned a capture outside its private directory." path))
      (shell-log--safe-path (shell-log-artifact-configuration artifact) path)
      (when (probe-file path) (platform-make-private *platform* path))
      (list :stream stream :reference (shell-log-reference artifact stream)
            :file                (file-namestring path)
            :byte-count          (cl-exec-sandbox:sandbox-capture-byte-count capture)
            :observed-byte-count (cl-exec-sandbox:sandbox-capture-observed-byte-count capture)
            :complete-p          (cl-exec-sandbox:sandbox-capture-complete-p capture)
            :truncated-p         (cl-exec-sandbox:sandbox-capture-truncated-p capture)
            :status              (cl-exec-sandbox:sandbox-capture-status capture)))))

(-> shell-log-record-capture (shell-log-artifact t &key (:closed-p boolean)) list)
(defun shell-log-record-capture (artifact result &key (closed-p t))
  "Persist allocated filenames before launch or closed evidence on every unwind.
With CLOSED-P false, retain the active reservation and prevent pruning."
  (with-shell-log-lock ((shell-log-artifact-configuration artifact))
    (unwind-protect
         (let ((metadata (copy-tree (shell-log-artifact-metadata artifact))))
           (setf (getf metadata :state) (if closed-p ':closed ':active)
                 (getf metadata :exit-code) (cl-exec-sandbox:sandbox-result-exit-code result)
                 (getf metadata :timed-out-p) (cl-exec-sandbox:sandbox-result-timed-out-p result)
                 (getf metadata :cancelled-p) (cl-exec-sandbox:sandbox-result-cancelled-p result)
                 (getf metadata :status) (cl-exec-sandbox:sandbox-result-status result)
                 (getf metadata :captures)
                 (remove nil (list (shell-log--capture-metadata
                                    artifact (cl-exec-sandbox:sandbox-result-output-capture result) ':output)
                                   (shell-log--capture-metadata
                                    artifact (cl-exec-sandbox:sandbox-result-error-capture result) ':error))))
           (when (shell-log-artifact-job artifact)
             (setf (getf metadata :detached-p)
                   (session-job-detached-p (shell-log-artifact-job artifact))))
           (when closed-p (setf (getf metadata :closed-at) (get-universal-time)))
           (setf (shell-log-artifact-metadata artifact) metadata)
           (shell-log--write-manifest artifact)
           (copy-tree metadata))
      (when closed-p
        (shell-log--release-owner (shell-log-capture-directory artifact))))))

(-> shell-log-release-artifact (shell-log-artifact) null)
(defun shell-log-release-artifact (artifact)
  "Idempotently close an abandoned allocation without fabricating capture data.
Always release the active pin, including when manifest publication fails. Callers
handling another condition may ignore this logging failure to preserve that cause."
  (with-shell-log-lock ((shell-log-artifact-configuration artifact))
    (let ((directory (shell-log-capture-directory artifact)))
      (when (or (gethash (namestring directory) *shell-log-active*)
                (eq (getf (shell-log-artifact-metadata artifact) :state) ':active))
        (unwind-protect
             (let ((metadata (copy-tree (shell-log-artifact-metadata artifact))))
               (setf (getf metadata :state) ':closed
                     (getf metadata :status) ':interrupted
                     (getf metadata :closed-at) (get-universal-time)
                     (getf metadata :reserved-bytes) 0)
               (setf (getf metadata :captures)
                     (mapcar (lambda (capture)
                               (setf (getf capture :complete-p) nil
                                     (getf capture :observed-byte-count-known-p) nil
                                     (getf capture :status) ':interrupted)
                               capture)
                             (getf metadata :captures)))
               (setf (shell-log-artifact-metadata artifact) metadata)
               (shell-log--write-manifest artifact))
          (shell-log--release-owner directory)))))
  nil)

(-> shell-log--public-metadata (list) list)
(defun shell-log--public-metadata (metadata)
  "Project bounded public evidence, excluding filenames and ancestor ownership."
  (unless (every #'shell-log--component-p
                 (list (getf metadata :root-conversation) (getf metadata :execution-id)
                       (getf metadata :artifact-id)))
    (shell-log--fail "Shell log manifest identity is invalid."))
  (let ((public
          (loop for key in '(:version :execution-id :artifact-id :state :created-at :closed-at
                             :merge-output-p :capture-byte-limit :exit-code :timed-out-p
                             :cancelled-p :status)
                append (list key (getf metadata key)))))
    (setf (getf public :captures)
          (loop for capture in (getf metadata :captures)
                for stream = (getf capture :stream)
                unless (member stream '(:output :error))
                  do (shell-log--fail "Shell log manifest stream is invalid.")
                collect
                (append
                 (list :stream    stream
                       :reference (format nil "shell-log:~A/~A/~A/~(~A~)"
                                          (getf metadata :root-conversation)
                                          (getf metadata :execution-id)
                                          (getf metadata :artifact-id) stream))
                 (loop for key in '(:byte-count :observed-byte-count :complete-p
                                    :truncated-p :status :oversized-p)
                       append (list key (getf capture key)))
                 (list :observed-byte-count-known-p
                       (getf capture :observed-byte-count-known-p t)))))
    (unless (and (<= (length (getf public :captures)) 2)
                 (<= (length (utf8-string-to-octets (task--write-readable-sexp public))) 2048))
      (shell-log--fail "Shell log public metadata exceeds its bounded allowance."))
    public))

(-> shell-log--unavailable-metadata (session-job &optional pathname) list)
(defun shell-log--unavailable-metadata (job &optional directory)
  "Return a bounded missing-manifest diagnostic without exposing private paths."
  (list :execution-id (session-job-execution-identifier job)
        :artifact-id  (and directory (first (last (pathname-directory directory))))
        :state ':unavailable :diagnostic "Shell log manifest is missing or unreadable."))

(-> shell-log-job-metadata (configuration session-job) list)
(defun shell-log-job-metadata (configuration job)
  "Return bounded public logs, preferring a terminal result over stale disk evidence.
  Reopen storage without replay when no terminal result is available."
  (let* ((result (and (job-terminal-p job) (job-result job)))
         (logs   (and (listp result) (getf result :shell-logs))))
    (when logs
      (return-from shell-log-job-metadata
        (list :shell-logs         (copy-tree (subseq logs 0 (min 16 (length logs))))
              :shell-logs-omitted (+ (getf result :shell-logs-omitted 0)
                                     (max 0 (- (length logs) 16)))))))
  (handler-case
      (let* ((root    (merge-pathnames
                       (format nil "~A/shell-log/" (session-job-execution-identifier job))
                       (task--artifact-group-root configuration
                                                  (session-job-root-conversation-identifier job))))
             (entries
               (progn
                 (shell-log--safe-path configuration root)
                 (loop for directory in (when (uiop:directory-exists-p root)
                                          (uiop:subdirectories root))
                       collect (handler-case
                                   (shell-log--public-metadata
                                    (shell-log--read-manifest configuration directory))
                                 (error () (shell-log--unavailable-metadata job directory))))))
             (ordered (sort entries #'> :key (lambda (entry) (or (getf entry :created-at) 0))))
             (count   (length ordered)))
        (list :shell-logs         (subseq ordered 0 (min 16 count))
              :shell-logs-omitted (max 0 (- count 16))))
    (error ()
      (list :shell-logs         (list (shell-log--unavailable-metadata job))
            :shell-logs-omitted 0))))
