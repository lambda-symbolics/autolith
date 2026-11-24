(in-package #:autolith)

;;;; -- Incremental Shell Capture Maintenance --

(defparameter *shell-log-maintenance-interval* 300
  "Minimum seconds between ordinary retention sweeps.")

(defparameter *shell-log-maintenance-batch-size* 16
  "Maximum captures inspected by one world-server maintenance job.")

(defvar *shell-log-maintenance-states* (make-hash-table :test #'equal)
  "In-progress sweep cursors, used only by the owning world-server thread.")

(defvar *shell-log-maintenance-state-lock* (make-lock "shell log maintenance cursors")
  "Protect the cursor registry across independent world-server threads.")
(defstruct shell-log-maintenance-state
  "A sweep cursor with no filesystem or knowledge locks held between batches."
  (directories nil :type list)
  (tombstones nil :type list)
  (trimming-p nil :type boolean))

(-> shell-log-maintenance--visit (configuration pathname) list)
(defun shell-log-maintenance--visit (configuration directory)
  "Recheck one capture and its authoritative evidence before age-based deletion."
  (let ((snapshot
          (with-shell-log-lock (configuration)
            (when (uiop:directory-exists-p directory)
              (shell-log--scan-manifest configuration directory)))))
    (when (eq (getf snapshot :state) ':pruned)
      (return-from shell-log-maintenance--visit (list directory snapshot)))
    (unless (and snapshot
                 (or (eq (getf snapshot :state) ':interrupted)
                     (> (- (get-universal-time) (getf snapshot :created-at 0))
                        *shell-log-retention-age*)))
      (return-from shell-log-maintenance--visit nil))
    (when (and (not (getf snapshot :detached-p t))
               (not (shell-log--delivery-recorded-p snapshot))
               (every #'shell-log--component-p
                      (list (getf snapshot :root-conversation)
                            (getf snapshot :completion-owner-conversation))))
      (shell-log--backfill-owner-step configuration (getf snapshot :root-conversation)
                                      (getf snapshot :completion-owner-conversation)))
    (shell-log-call-with-retention-lock
     configuration
     (lambda ()
       (when (uiop:directory-exists-p directory)
         (let ((metadata (shell-log--scan-manifest configuration directory)))
           (when metadata
             (shell-log--reconcile-reservation configuration directory metadata)
             (when (and (not (eq (getf metadata :state) ':pruned))
                        (> (- (get-universal-time) (getf metadata :created-at 0))
                           *shell-log-retention-age*)
                        (shell-log--prunable-p configuration directory metadata))
               (setf metadata (shell-log--prune-artifact configuration directory metadata)))
             (when (eq (getf metadata :state) ':pruned)
               (list directory metadata)))))))))

(-> shell-log-maintenance--trim (configuration pathname) null)
(defun shell-log-maintenance--trim (configuration directory)
  "Remove one previously selected tombstone after rechecking the current manifest."
  (with-shell-log-lock (configuration)
    (when (uiop:directory-exists-p directory)
      (let ((metadata (shell-log--scan-manifest configuration directory)))
        (when (eq (getf metadata :state) ':pruned)
          ;; The cursor already selected this tombstone by global count or age.
          (let ((*shell-log-tombstone-count-limit* 0))
            (shell-log--trim-tombstones configuration (list (list directory metadata))))))))
  nil)

(-> shell-log-maintenance-step (configuration) boolean)
(defun shell-log-maintenance-step (configuration)
  "Inspect one bounded sweep batch and return true while another batch is needed.

Directory discovery and receipt backfill run outside the artifact lock. Each
capture is authorized separately, so ordinary allocations can run between them."
  (let* ((key (namestring (config :data-root configuration)))
         (cursor (with-lock-held (*shell-log-maintenance-state-lock*)
                   (gethash key *shell-log-maintenance-states*)))
         (completed-p nil))
    (unwind-protect
         (progn
           (unless cursor
             (when (with-shell-log-lock (configuration)
                     (let ((state (shell-log-accounting--read configuration)))
                       (when (and state (getf state :clean-p)
                                  (>= (- (get-universal-time) (getf state :maintained-at))
                                      *shell-log-maintenance-interval*))
                         (setf (getf state :maintained-at) (get-universal-time))
                         (shell-log-accounting--publish configuration state)
                         t)))
               (setf cursor (make-shell-log-maintenance-state
                             :directories (shell-log--directories configuration)))
               (with-lock-held (*shell-log-maintenance-state-lock*)
                 (setf (gethash key *shell-log-maintenance-states*) cursor))))
           (when cursor
             (loop repeat *shell-log-maintenance-batch-size*
                   while (shell-log-maintenance-state-directories cursor)
                   for directory = (pop (shell-log-maintenance-state-directories cursor))
                   do (if (shell-log-maintenance-state-trimming-p cursor)
                          (shell-log-maintenance--trim configuration directory)
                          (let ((tombstone (shell-log-maintenance--visit configuration directory)))
                            (when tombstone
                              (push tombstone (shell-log-maintenance-state-tombstones cursor))))))
             (unless (shell-log-maintenance-state-directories cursor)
               (if (shell-log-maintenance-state-trimming-p cursor)
                 (with-lock-held (*shell-log-maintenance-state-lock*)
                   (remhash key *shell-log-maintenance-states*))
                   (let* ((entries (sort (shell-log-maintenance-state-tombstones cursor)
                                         #'< :key (lambda (entry) (getf (second entry) :pruned-at 0))))
                          (excess (max 0 (- (length entries) *shell-log-tombstone-count-limit*))))
                     (setf (shell-log-maintenance-state-trimming-p cursor) t
                           (shell-log-maintenance-state-tombstones cursor) nil
                           (shell-log-maintenance-state-directories cursor)
                           (loop for (directory metadata) in entries
                                 for index from 0
                                 when (or (< index excess)
                                          (> (- (get-universal-time) (getf metadata :pruned-at 0))
                                             *shell-log-tombstone-age*))
                                   collect directory))
                     (unless (shell-log-maintenance-state-directories cursor)
                     (with-lock-held (*shell-log-maintenance-state-lock*)
                       (remhash key *shell-log-maintenance-states*)))))))
           (setf completed-p t)
           (not (null (with-lock-held (*shell-log-maintenance-state-lock*)
                        (gethash key *shell-log-maintenance-states*)))))
      (unless completed-p
        (with-lock-held (*shell-log-maintenance-state-lock*)
          (remhash key *shell-log-maintenance-states*))))))
