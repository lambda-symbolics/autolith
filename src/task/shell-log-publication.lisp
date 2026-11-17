(in-package #:autolith)

;;;; -- Shell Evidence Publication Ownership --

(defclass shell-log-publication ()
  ((configuration :initarg :configuration :reader shell-log-publication-configuration
                  :documentation "Configuration used to serialize scope registration.")
   (owner :initform (current-thread) :reader shell-log-publication-owner
          :documentation "Thread responsible for this publication boundary.")
   (open-p :initform t :accessor shell-log-publication-open-p
           :documentation "Whether the boundary still owns its registered artifacts.")
   (artifacts :initform nil :accessor shell-log-publication-artifacts
              :documentation "Captures registered under the shell-log lock."))
  (:documentation "Internal ownership through evidence recording and durable publication."))

(-> shell-log--publication-live-p (shell-log-publication) boolean)
(defun shell-log--publication-live-p (publication)
  "Require an open boundary and its actual live publishing thread."
  (and (shell-log-publication-open-p publication)
       (thread-alive-p (shell-log-publication-owner publication)) t))

(-> shell-log--artifact-owned-p (shell-log-artifact) boolean)
(defun shell-log--artifact-owned-p (artifact)
  "Recognize native writers and live evidence/publication owners."
  (let ((capture-owner (shell-log-artifact-capture-owner artifact)))
    (or (and capture-owner (thread-alive-p capture-owner) t)
        (not (null (some #'thread-alive-p (shell-log-artifact-native-owners artifact))))
        (not (null (some #'shell-log--publication-live-p
                         (shell-log-artifact-publications artifact)))))))

(-> shell-log--release-unowned (shell-log-artifact) null)
(defun shell-log--release-unowned (artifact)
  "Release only after all live native and publication owners finish.
Call with the shell-log lock held."
  (unless (shell-log--artifact-owned-p artifact)
    (shell-log--release-owner (shell-log-capture-directory artifact)))
  nil)

(-> shell-log--finish-publication ((option shell-log-publication)) null)
(defun shell-log--finish-publication (publication)
  "Close one boundary, retaining captures still owned by another live boundary."
  (when publication
    (setf (shell-log-publication-open-p publication) nil)
    (let ((artifacts
            (with-shell-log-lock ((shell-log-publication-configuration publication))
              (prog1 (shell-log-publication-artifacts publication)
                (setf (shell-log-publication-artifacts publication) nil)))))
      (dolist (artifact artifacts)
        (with-shell-log-lock ((shell-log-artifact-configuration artifact))
          (setf (shell-log-artifact-publications artifact)
                (remove publication (shell-log-artifact-publications artifact)))
          (shell-log--release-unowned artifact)))))
  nil)

(-> shell-log-call-with-publication (configuration function) t)
(defun shell-log-call-with-publication (configuration function)
  "Keep output through FUNCTION's evidence/publication boundary.
Nested calls reuse live boundaries; supervised jobs add independent boundaries."
  (if (some #'shell-log--publication-live-p *shell-log-publications*)
      (funcall function)
      (let* ((publication              (make-instance 'shell-log-publication :configuration configuration))
             (*shell-log-publications* (list publication))
             (completed-p              nil))
       (unwind-protect
            (multiple-value-prog1 (funcall function)
              (setf completed-p t))
         (if completed-p
             (shell-log--finish-publication publication)
             ; mallet:suppress no-ignore-errors -- preserve the operation's original nonlocal exit
             (ignore-errors (shell-log--finish-publication publication)))))))

(-> shell-log--pending-owner-p
    (configuration &key (:root string) (:execution string) (:owner string)) boolean)
(defun shell-log--pending-owner-p (configuration &key root execution owner)
  "Require existing OWNER storage and its exact outstanding execution index.
An indexed owner must have a readable header with its admitted identity."
  (let* ((identity   (shell-log--owner-identity configuration root owner))
         (group      (task--artifact-group-root configuration root))
         (index-path (shell-log--safe-path configuration
                                          (task-completion--index-path group owner))))
    (unless (conversation-storage-occupied-p identity)
      (return-from shell-log--pending-owner-p nil))
    (let ((index (and (probe-file index-path) (task-continuity--read index-path))))
      (when (and index
                 (not (and (eql 1 (getf index :version))
                           (proper-list-p (getf index :execution-ids))
                           (every #'shell-log--component-p (getf index :execution-ids)))))
        (shell-log--fail "Cannot establish pending shell log delivery from its index." index-path))
      (when (member execution (getf index :execution-ids) :test #'equal)
        (let ((header (conversation-peek-header identity)))
          (unless (and (consp header) (equal owner (getf (rest header) :id)))
            (shell-log--fail "Cannot validate the pending shell log delivery owner." identity)))
        t))))

(-> shell-log--publication-pending-p (configuration list) boolean)
(defun shell-log--publication-pending-p (configuration metadata)
  "Require a live owner or exact admitted outstanding detached terminal delivery.
Inspect bounded records under the shell-log lock, never transcript history.
Corrupt required identities refuse deletion; absent obligations grant no pin."
  (let ((root      (getf metadata :root-conversation))
        (owner     (getf metadata :completion-owner-conversation))
        (execution (getf metadata :execution-id))
        (artifact  (getf metadata :artifact-id)))
    (unless (every #'shell-log--component-p (list root execution artifact))
      (shell-log--fail "Invalid shell log publication identity."))
    (let* ((group     (task--artifact-group-root configuration root))
           (directory (merge-pathnames (format nil "~A/shell-log/~A/" execution artifact) group)))
      (handler-case
          (progn
            (when (shell-log--active-p directory)
              (return-from shell-log--publication-pending-p t))
            (let* ((continuity (shell-log--safe-path configuration
                                                      (merge-pathnames (format nil "~A/continuity.sexp" execution) group)))
                   (record     (and (probe-file continuity) (task-continuity--read continuity))))
              (let* ((terminal       (shell-log--safe-path configuration
                                                          (merge-pathnames (format nil "~A/terminal.sexp" execution) group)))
                     (result         (and (probe-file terminal) (task-continuity--read terminal)))
                     (admitted-owner (getf record :completion-owner-conversation)))
                (unless (find-if (lambda (log)
                                   (and (equal execution (getf log :execution-id))
                                        (equal artifact (getf log :artifact-id))))
                                 (getf (getf result :result) :shell-logs))
                  (return-from shell-log--publication-pending-p nil))
                (when (or (and (shell-log--component-p admitted-owner)
                               (shell-log--pending-owner-p configuration :root root :execution execution
                                                                        :owner admitted-owner))
                          (and (not (equal owner admitted-owner))
                               (shell-log--component-p owner)
                               (shell-log--pending-owner-p configuration :root root :execution execution :owner owner)))
                  (unless (and (eql 1 (getf record :version))
                               (equal root (getf record :root-conversation))
                               (equal execution (getf record :execution-id))
                               (equal (getf metadata :job-id) (getf record :job-id))
                               (equal owner admitted-owner)
                               (eq (getf record :detached-p) t)
                               (member (getf record :completion-policy) '(:notify :continue)))
                    (shell-log--fail "Pending shell log delivery identities or admission flags disagree." continuity))
                  (return-from shell-log--publication-pending-p t))
                nil)))
        (shell-log-error (condition)
          (error condition))
        (error (condition)
          (shell-log--fail (format nil "Cannot inspect pending shell log delivery: ~A" condition) directory))))))
