(in-package #:autolith)

;;;; -- Durable Shell Capture Accounting --

(-> shell-log-accounting--path (configuration) pathname)
(defun shell-log-accounting--path (configuration)
  "Return the private accounting checkpoint protected by the shell-log lock."
  (shell-log--safe-path configuration
                       (merge-pathnames "tasks/shell-log-accounting.sexp"
                                        (config :data-root configuration))))

(-> shell-log-accounting--read (configuration) list)
(defun shell-log-accounting--read (configuration)
  "Read a bounded checkpoint, returning NIL when a validated rebuild is needed."
  (let* ((path (shell-log-accounting--path configuration))
         (status (platform-path-status *platform* path :follow-links-p nil))
         (state (when (and status (eq (platform-file-status-kind status) ':file)
                           (<= (platform-file-status-size status) 4096))
                  (handler-case (task-continuity--read path) (error () nil)))))
    (when (and (proper-list-p state) (evenp (length state))
               (getf state :version) (not (eql (getf state :version) 1)))
      (shell-log--fail "Unsupported shell capture accounting version." path))
    (when (and (proper-list-p state) (evenp (length state))
               (eql (getf state :version) 1)
               (typep (getf state :bytes) '(integer 0))
               (typep (getf state :clean-p) 'boolean)
               (typep (getf state :maintained-at) '(integer 0)))
      state)))

(-> shell-log-accounting--publish (configuration list) pathname)
(defun shell-log-accounting--publish (configuration state)
  "Atomically replace the bounded accounting checkpoint under the artifact lock."
  (let ((path (shell-log-accounting--path configuration)))
    (task-continuity--write path state)
    (platform-make-private *platform* path)
    path))

(-> shell-log-accounting--dirty (configuration) list)
(defun shell-log-accounting--dirty (configuration)
  "Invalidate the checkpoint before a mutation whose byte delta is not yet committed."
  (let ((state (or (shell-log-accounting--read configuration)
                   (list :version 1 :bytes 0 :clean-p nil :maintained-at 0))))
    (when (getf state :clean-p)
      (setf state (copy-list state)
            (getf state :clean-p) nil))
    (shell-log-accounting--publish configuration state)
    state))

(-> shell-log-accounting--charge (configuration pathname list) (integer 0))
(defun shell-log-accounting--charge (configuration directory metadata)
  "Charge retained bytes and all capacity a writer or publication owner can still use."
  (if (eq (getf metadata :state) ':pruned)
      0
      (max (getf metadata :reserved-bytes 0)
           (shell-log--measure-captures configuration directory metadata))))

(-> shell-log-accounting--locked-p (configuration) boolean)
(defun shell-log-accounting--locked-p (configuration)
  "Require the held lock to serialize this data root before reusing its scope."
  (when *shell-log-locked-root*
    (unless (uiop:pathname-equal *shell-log-locked-root* (config :data-root configuration))
      (shell-log--fail "Shell capture accounting cannot nest locks for different data roots."))
    t))

(-> shell-log-accounting-write (configuration pathname list &key (:publish function)) pathname)
(defun shell-log-accounting-write (configuration directory metadata &key publish)
  "Publish METADATA through PUBLISH, invalidating accounting before any charge change.

The previous charge is read from the durable manifest, not a measured heap copy.
A missing or interrupted checkpoint is rebuilt by retention before admission."
  (flet ((write-locked ()
           (let* ((path (shell-log--manifest-path directory))
                  (exists-p (probe-file (shell-log--safe-path configuration path)))
                  (previous-status (when exists-p
                                     (platform-path-status *platform* path :follow-links-p nil)))
                  (previous (when (and previous-status
                                       (<= (platform-file-status-size previous-status)
                                           *shell-log-manifest-byte-limit*))
                              (handler-case (task-continuity--read path) (error () nil))))
                  (old-charge (if exists-p
                                  (and (proper-list-p previous) (evenp (length previous))
                                       (getf previous :quota-charge))
                                  0))
                  (charge (shell-log-accounting--charge configuration directory metadata))
                  (state (shell-log-accounting--read configuration))
                  (clean-p (and state (getf state :clean-p)
                                (typep old-charge '(integer 0))
                                (<= old-charge (getf state :bytes)))))
             (when (and clean-p (not exists-p)
                        (> (+ (getf state :bytes) charge) *shell-log-retention-byte-limit*))
               (shell-log--fail "Private shell log storage has no capacity for this reservation." directory))
             (setf (getf metadata :quota-charge) charge)
             (cond
               ((and clean-p (= old-charge charge))
                (funcall publish metadata))
               (t
                (unless (and state (not (getf state :clean-p)))
                  (shell-log-accounting--dirty configuration))
                (let ((result (funcall publish metadata)))
                  (when clean-p
                    (setf state (copy-list state)
                          (getf state :bytes) (+ (- (getf state :bytes) old-charge) charge))
                    (shell-log-accounting--publish configuration state))
                  result))))))
    (if (shell-log-accounting--locked-p configuration)
        (write-locked)
        (with-shell-log-lock (configuration) (write-locked)))))

(-> shell-log-accounting-observe (configuration list) null)
(defun shell-log-accounting-observe (configuration metadata)
  "Invalidate a cached total when inspection discovers growth beyond the durable charge."
  (when (and (typep (getf metadata :quota-charge) '(integer 0))
             (> (getf metadata :accounted-bytes 0) (getf metadata :quota-charge)))
    (if (shell-log-accounting--locked-p configuration)
        (shell-log-accounting--dirty configuration)
        (with-shell-log-lock (configuration)
          (shell-log-accounting--dirty configuration))))
  nil)

(-> shell-log-accounting-rebuild (configuration list (integer 0)) null)
(defun shell-log-accounting-rebuild (configuration inventory total)
  "Checkpoint a completely validated INVENTORY after retention, with its TOTAL charge."
  (dolist (entry inventory)
    (destructuring-bind (directory metadata &optional accounted-bytes) entry
      (declare (ignore accounted-bytes))
      (let ((charge (if (eq (getf metadata :state) ':pruned)
                        0
                        (getf metadata :accounted-bytes 0))))
        (when (and (eq (getf metadata :state) ':closed)
                   (not (shell-log--active-p directory)))
          (setf (getf metadata :reserved-bytes) 0))
        (unless (eql (getf metadata :quota-charge) charge)
          (setf (getf metadata :quota-charge) charge)
          (shell-log--write-manifest
           (make-instance 'shell-log-artifact :configuration configuration
                          :capture-directory directory :metadata metadata))))))
  (shell-log-accounting--publish
   configuration (list :version 1 :bytes total :clean-p t
                       :maintained-at (get-universal-time)))
  nil)

(-> shell-log-call-with-allocation-lock (configuration (integer 0) function) t)
(defun shell-log-call-with-allocation-lock (configuration reservation function)
  "Admit FUNCTION without historical reads when the durable checkpoint has capacity.

Recovery or storage pressure uses the existing evidence-aware retention boundary.
No capture is pruned and no evidence snapshot is needed on the ordinary path."
  (multiple-value-bind (value admitted-p)
      (with-shell-log-lock (configuration)
        (let ((state (shell-log-accounting--read configuration)))
          (when (and state (getf state :clean-p)
                     (<= (+ (getf state :bytes) reservation) *shell-log-retention-byte-limit*))
            (values (funcall function) t))))
    (if admitted-p
        value
        (progn
          (shell-log--backfill-delivery configuration)
          (shell-log-call-with-retention-lock
           configuration
           (lambda ()
             (shell-log--prune configuration :reservation reservation)
             (funcall function)))))))
