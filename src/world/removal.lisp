(in-package #:autolith)

;;;; -- Channel Removal --

(-> channel-remove (configuration list &key (:tool-name string)) (integer 0))
(defun channel-remove (configuration channels &key (tool-name "child.remove"))
  "Remove CHANNELS after validating fresh world state and preparing every root.

All leases precede publication. A durable cleanup intent precedes the atomic
world deletion; destructive storage cleanup follows it. Failed cleanup retains
that intent and the private contexts for retry at server start or RETRY-CLEANUP."
  (channel-remove-recover configuration)
  (call-with-file-lock
   (channel--removal-lock-pathname configuration)
   (lambda ()
     (let* ((world        (world-load configuration))
            (all          (remove-duplicates
                           (loop for channel in channels
                                 do (when (channel-inbox-p (world--require-channel world channel ':delete-channel))
                                      (error 'task-error :message   "The inbox stays; only other channels can be removed."
                                                         :tool-name tool-name))
                                 append (channel--subtree world channel))
                           :test #'string= :from-end t))
            (identities   (conversation-pathnames-for-ids configuration all))
            (participants (remove-duplicates
                           (loop for identifier in all
                                 append (loop for resident in (world-channel-residents world identifier)
                                              append (cons (participant-identifier resident)
                                                           (mapcar #'participant-identifier
                                                                   (world-participant-descendants
                                                                    world (participant-identifier resident))))))
                           :test #'string=))
            (intent       (merge-pathnames (format nil "~A.sexp" (first all))
                                           (channel--removal-root configuration))))
       (channel--quiesce configuration all tool-name)
       (channel--call-with-removal-leases
        configuration all
        :identities identities
        :function   (lambda ()
                      (world-channels-delete
                       configuration (reverse all) :participants participants
                       :prepare-function
                       (lambda (deleted)
                         ;; Validation and preparation hold the world store lock.
                         (task-child--prepare-storage configuration all identities)
                         (snapshot-write
                          intent
                          (list :version 1 :channels all :participants deleted
                                :identities (loop for identifier in all
                                                  collect (list identifier
                                                                (namestring (gethash identifier identities))))))))
                      (channel--finish-removal configuration intent))
        :tool-name  tool-name)
       (length all)))))

(-> channel-remove-recover (configuration) null)
(defun channel-remove-recover (configuration)
  "Retry durable channel cleanup intents at server start or before another deletion.

An intent whose world transaction never published is simply retired. A
published deletion is resumed idempotently under all its conversation leases.
An exception leaves the intent on disk and exposes RETRY-CLEANUP."
  (call-with-file-lock
   (channel--removal-lock-pathname configuration)
   (lambda ()
     (dolist (intent (uiop:directory-files (channel--removal-root configuration) "*.sexp"))
       (let* ((plan       (channel--read-removal intent))
              (channels   (getf plan :channels))
              (world      (world-load configuration))
              (identities (channel--removal-identities plan)))
         (cond
           ((every (lambda (identifier) (world-channel-find world identifier)) channels)
            ;; No destructive work preceded publication, so an uncommitted
            ;; intent needs no rollback.
            (delete-file intent))
           ((some (lambda (identifier) (world-channel-find world identifier)) channels)
            (error 'task-error
                   :message   (format nil "Cleanup intent ~A conflicts with live channel identities; storage is untouched." intent)
                   :tool-name "child.remove"))
           (t
            (channel--call-with-removal-leases
             configuration channels
             :identities identities
             :function   (lambda () (channel--finish-removal configuration intent))
             :tool-name  "child.remove")))))))
  nil)

(-> channel--removal-root (configuration) pathname)
(defun channel--removal-root (configuration)
  "Return the private directory of outstanding channel cleanup intents."
  (let ((root (merge-pathnames "channel-removals/" (config :data-root configuration))))
    (ensure-directories-exist root)
    root))

(-> channel--removal-lock-pathname (configuration) pathname)
(defun channel--removal-lock-pathname (configuration)
  "Return the guard serializing deletion and recovery, including other processes."
  (merge-pathnames "cleanup.lock" (channel--removal-root configuration)))

(-> channel--read-removal (pathname) list)
(defun channel--read-removal (intent)
  "Read and validate INTENT before interpreting its identities or retiring it."
  (let ((plan (snapshot-read intent)))
    (unless (and (eql (getf plan :version) 1)
                 (getf plan :channels)
                 (world--identifier-list-p (getf plan :channels))
                 (world--identifier-list-p (getf plan :participants))
                 (every (lambda (entry)
                          (and (listp entry) (= (length entry) 2)
                               (non-empty-string-p (first entry))
                               (non-empty-string-p (second entry))))
                        (getf plan :identities))
                 (equal (mapcar #'first (getf plan :identities)) (getf plan :channels)))
      (error 'task-error
             :message   (format nil "Invalid channel cleanup intent ~A; preserve it for recovery." intent)
             :tool-name "child.remove"))
    plan))

(-> channel--removal-identities (list) hash-table)
(defun channel--removal-identities (plan)
  "Restore the conversation identity paths retained in cleanup PLAN."
  (let ((identities (make-hash-table :test #'equal)))
    (dolist (entry (getf plan :identities))
      (setf (gethash (first entry) identities) (pathname (second entry))))
    identities))

(-> channel--call-with-removal-leases
    (configuration list &key (:identities hash-table) (:function function) (:tool-name string)) t)
(defun channel--call-with-removal-leases (configuration channels
                                             &key identities function (tool-name "child.remove"))
  "Call FUNCTION holding every channel lease, releasing all on any exception."
  (let ((leases nil))
    (unwind-protect
         (progn
           (dolist (identifier channels)
             (push (handler-case
                       (conversation-lease-acquire configuration identifier
                                                   :pathname (gethash identifier identities))
                     (conversation-in-use ()
                       (channel--busy identifier tool-name)))
                   leases))
           (funcall function))
      (mapc #'conversation-lease-release leases))))

(define-condition channel-removal-pending (task-error)
  ((intent
    :initarg       :intent
    :reader        channel-removal-pending-intent
    :documentation "Durable cleanup intent to retry, containing the exact identities.")
   (cause
    :initarg       :cause
    :reader        channel-removal-pending-cause
    :documentation "The cleanup exception; the deletion itself has already published."))
  (:documentation "A channel deletion published, but its durable cleanup is incomplete."))

(-> channel--finish-removal (configuration pathname) null)
(defun channel--finish-removal (configuration intent)
  "Finish committed INTENT idempotently, retaining it on failure for automatic retry."
  (loop
    (restart-case
        (handler-case
            (let* ((plan       (channel--read-removal intent))
                   (channels   (getf plan :channels))
                   (identities (channel--removal-identities plan)))
              ;; Do the most failure-prone work first. Contexts and mapping
              ;; evidence survive until every history and worktree is cleaned.
              (dolist (identifier channels)
                (task-worktree-discard configuration identifier :pathname (gethash identifier identities)))
              (task-child-delete-storage configuration channels :identities identities
                                                               :leased-p t :prepared-p t)
              (dolist (identifier channels)
                (let ((pathname (gethash identifier identities)))
                  (when (equal (pathname-directory pathname)
                               (pathname-directory (configuration-conversation-root configuration)))
                    (when (conversation-storage-active-pathname pathname)
                      (conversation-delete-history configuration identifier :pathname pathname))
                    (conversation-delete-artifacts configuration identifier))))
              (let ((world (world-load configuration)))
                (dolist (participant (getf plan :participants))
                  (unless (world-participant-find world participant)
                    (world-context-delete configuration participant))))
              (dolist (identifier channels)
                (world-inbox-delete configuration identifier))
              (world-sessions-forget configuration channels)
              (delete-file intent)
              (return nil))
          (error (cause)
            (error 'channel-removal-pending
                   :message   (format nil "Channels removed; cleanup is pending in ~A: ~A. Retry with RETRY-CLEANUP, or automatically at the next server start or channel deletion."
                                      intent cause)
                   :tool-name "child.remove"
                   :intent    intent
                   :cause     cause)))
      (retry-cleanup ()
        :report "Retry this channel deletion's durable cleanup intent."))))

(-> channel--quiesce (configuration list string) null)
(defun channel--quiesce (configuration channels tool-name)
  "Ask the world to release idle unattended hosts before TOOL-NAME leases CHANNELS."
  (let ((entry (world-client-entry configuration)))
    (when entry
      (let ((states (getf (rest (localgroup-query-record entry ':quiesce
                                                      (list :channels channels)))
                         :states)))
        (dolist (state states)
          (unless (member (rest state) '(:closed :idle))
            (channel--busy (first state) tool-name :state (rest state)))))))
  nil)

(-> channel--busy (string string &key (:state keyword)) nil)
(defun channel--busy (channel tool-name &key (state ':leased))
  "Refuse TOOL-NAME with the specific reason CHANNEL cannot be leased."
  (error 'task-error
         :message   (format nil "Channel ~A ~A"
                            (conversation-identifier-display channel)
                            (case state
                              (:attached
                               "is shown in a terminal; switch away before trying again.")
                              (:running
                               "has running or queued work; finish or cancel it before trying again.")
                              (:hosted
                               "is hosted but could not close; retry after its host finishes shutting down.")
                              (otherwise
                               "has a conversation lease held by another operation; retry after that operation finishes.")))
         :tool-name tool-name
         :task-id   channel))

(-> channel--subtree (world string) list)
(defun channel--subtree (world channel)
  "Return CHANNEL and every channel nested below it, each before those nested in it."
  (cons channel
        (loop for child in (world-channel-children world channel)
              append (channel--subtree world (channel-identifier child)))))
