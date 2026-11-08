(in-package #:autolith)

;;;; -- Child Lifecycle Tools --

;;; A parent keeps its children: it sends them work through their channels,
;;; and child.rename, child.compact, child.reset and child.remove look after
;;; them. Each names one child by its name or its channel, among the channels
;;; nested in the caller's own. A child that runs, or whose channel is open,
;;; is busy, and only renaming does not wait for it to be idle.

(defclass child-tool (tool)
  ((orchestrator
    :initarg       :orchestrator
    :reader        child-tool-orchestrator
    :type          task-orchestrator
    :documentation "The orchestrator that runs the caller's children."))
  (:documentation "A tool a parent uses to look after one of its children."))

(defclass child-rename-tool (child-tool)
  ()
  (:documentation "Give a child a new name."))

(defclass child-compact-tool (child-tool)
  ()
  (:documentation "Compact an idle child's history, as (compact) does for a session."))

(defclass child-reset-tool (child-tool)
  ()
  (:documentation "Clear an idle child's history, keeping its name, role and channel."))

(defclass child-remove-tool (child-tool)
  ()
  (:documentation "Delete an idle child with its channel, its history and its own children."))


;;;; -- Tool Protocol --

(defmethod tool-child-safe-p ((tool child-tool))
  "Let a child that may spawn look after its own children."
  (declare (ignore tool))
  t)

(defmethod tool-execute ((tool child-rename-tool) (context tool-context) arguments)
  "Rename the child ARGUMENTS name."
  (task--validate-tool-arguments arguments '("child" "name") "child.rename")
  (let ((name (tool-argument arguments "name" :required t)))
    (multiple-value-bind (channel agent)
        (task-child--find context (tool-argument arguments "child" :required t) "child.rename")
      (declare (ignore channel))
      (world-participant-rename (tool-context-configuration context)
                                (participant-identifier agent)
                                name)
      (tool-success (format nil "@~A is now @~A." (participant-name agent) name)))))

(defmethod tool-execute ((tool child-compact-tool) (context tool-context) arguments)
  "Compact the history of the idle child ARGUMENTS name."
  (task--validate-tool-arguments arguments '("child") "child.compact")
  (multiple-value-bind (channel agent)
      (task-child--find context (tool-argument arguments "child" :required t) "child.compact")
    (task-child--call-with-lease
     context channel
     (lambda ()
       (agent-compact-conversation (task-child--agent tool context channel)
                                   (make-instance 'agent-observer)))
     :tool-name "child.compact")
    (tool-success (format nil "Compacted @~A's history." (participant-name agent)))))

(defmethod tool-execute ((tool child-reset-tool) (context tool-context) arguments)
  "Clear the history of the idle child ARGUMENTS name."
  (declare (ignore tool))
  (task--validate-tool-arguments arguments '("child") "child.reset")
  (multiple-value-bind (channel agent)
      (task-child--find context (tool-argument arguments "child" :required t) "child.reset")
    (task-child--call-with-lease
     context channel
     (lambda ()
       (task-child--reset-history (tool-context-configuration context) channel))
     :tool-name "child.reset")
    (tool-success (format nil "Cleared @~A's history; its name, role and channel stay."
                          (participant-name agent)))))

(defmethod tool-execute ((tool child-remove-tool) (context tool-context) arguments)
  "Delete the idle children ARGUMENTS name, with everything they own, in one pass."
  (declare (ignore tool))
  (task--validate-tool-arguments arguments '("children") "child.remove")
  (let* ((designators (coerce (tool-argument arguments "children" :required t) 'list))
         (found       (progn
                        (unless (and designators (every #'non-empty-string-p designators))
                          (error 'task-error
                                 :message   "child.remove takes children, a list of one or more names or channels."
                                 :tool-name "child.remove"))
                        (mapcar (lambda (designator)
                                  (multiple-value-list (task-child--find context designator "child.remove")))
                                designators)))
         (removed     (channel-remove (tool-context-configuration context) (mapcar #'first found)))
         (below       (- removed (length found))))
    (tool-success (format nil "Removed ~{@~A~^, ~}~[~:;, and the ~:*~D channels below them~]."
                          (mapcar (lambda (entry) (participant-name (second entry))) found)
                          below))))


;;;; -- Public Functions --

(-> task-children-register-tools (tool-registry task-orchestrator) tool-registry)
(defun task-children-register-tools (registry orchestrator)
  "Register the tools a parent looks after its children with."
  (let ((child (tool-string-property "The child's name, or its channel.")))
    (flet ((register (class name description
                      &optional (properties (json-object "child" child)) (required '("child")))
             (tool-registry-register
              registry
              (make-instance class
                             :orchestrator orchestrator
                             :namespace    "child"
                             :name         name
                             :description  description
                             :parameters   (tool-object-schema properties required)))))
      (register 'child-rename-tool "rename"
                "Give one of your children a new name, which @mentions use."
                (json-object "child" child
                             "name"  (tool-string-property "The new name: letters, digits, hyphens and underscores."))
                '("child" "name"))
      (register 'child-compact-tool "compact"
                "Compact an idle child's history into a summary, so it keeps working with less context.")
      (register 'child-reset-tool "reset"
                "Clear an idle child's history. It keeps its name, role and channel, and starts fresh on the next work you send it.")
      (register 'child-remove-tool "remove"
                "Delete idle children for good, several at once: their channels, histories, worktrees and own children."
                (json-object "children" (json-object "type"        "array"
                                                     "description" "The children to remove, by name or channel."
                                                     "minItems"    1
                                                     "items"       child))
                '("children"))))
  registry)

(-> channel-remove (configuration list &key (:tool-name string)) (integer 0))
(defun channel-remove (configuration channels &key (tool-name "child.remove"))
  "Remove CHANNELS after validating fresh world state and preparing every root.

All leases precede publication. A durable cleanup intent precedes the atomic
world deletion; destructive storage cleanup follows it. Failed cleanup retains
that intent and the private contexts for retry at server start or RETRY-CLEANUP."
  (channel-remove-recover configuration)
  (call-with-file-lock
   (task-child--removal-lock-pathname configuration)
   (lambda ()
     (let* ((world        (world-load configuration))
            (all          (remove-duplicates
                           (loop for channel in channels
                                 do (when (channel-inbox-p (world--require-channel world channel ':delete-channel))
                                      (error 'task-error :message   "The inbox stays; only other channels can be removed."
                                                         :tool-name tool-name))
                                 append (task-child--subtree world channel))
                           :test #'string= :from-end t))
            (identities   (task-child--storage-identities configuration all))
            (participants (remove-duplicates
                           (loop for identifier in all
                                 append (loop for resident in (world-channel-residents world identifier)
                                              append (cons (participant-identifier resident)
                                                           (mapcar #'participant-identifier
                                                                   (world-participant-descendants
                                                                    world (participant-identifier resident))))))
                           :test #'string=))
            (intent       (merge-pathnames (format nil "~A.sexp" (first all))
                                           (task-child--removal-root configuration))))
       (task-child--quiesce configuration all tool-name)
       (task-child--call-with-removal-leases
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
                      (task-child--finish-removal configuration intent))
        :tool-name  tool-name)
       (length all)))))

(-> channel-remove-recover (configuration) null)
(defun channel-remove-recover (configuration)
  "Retry durable channel cleanup intents at server start or before another deletion.

An intent whose world transaction never published is simply retired. A
published deletion is resumed idempotently under all its conversation leases.
An exception leaves the intent on disk and exposes RETRY-CLEANUP."
  (call-with-file-lock
   (task-child--removal-lock-pathname configuration)
   (lambda ()
     (dolist (intent (uiop:directory-files (task-child--removal-root configuration) "*.sexp"))
       (let* ((plan       (task-child--read-removal intent))
              (channels   (getf plan :channels))
              (world      (world-load configuration))
              (identities (task-child--removal-identities plan)))
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
            (task-child--call-with-removal-leases
             configuration channels
             :identities identities
             :function   (lambda () (task-child--finish-removal configuration intent))
             :tool-name  "child.remove")))))))
  nil)

(-> task-child-delete-storage
    (configuration list &key (:identities (option hash-table)) (:leased-p boolean)
                             (:prepared-p boolean)) null)
(defun task-child-delete-storage (configuration channels &key identities leased-p prepared-p)
  "Delete idle child storage after preparing delivery evidence for every root.

Prepare every root before deleting the first directory. PREPARED-P means the
caller already preserved that evidence while holding all conversation leases."
  (let ((identities (or identities (task-child--storage-identities configuration channels :prefer-task-p t))))
    (if leased-p
        (progn
          (unless prepared-p
            (task-child--prepare-storage configuration channels identities))
          (maphash (lambda (root entries)
                     (task-child--delete-root-storage configuration root entries
                                                      :leased-p t :prepared-p t))
                   (task-child--storage-roots configuration channels identities)))
        (task-child--call-with-removal-leases
         configuration channels
         :identities identities
         :function   (lambda ()
                       (task-child-delete-storage configuration channels
                                                  :identities identities
                                                  :leased-p   t
                                                  :prepared-p prepared-p)))))
  nil)


;;;; -- Private Functions --

(-> task-child--removal-root (configuration) pathname)
(defun task-child--removal-root (configuration)
  "Return the private directory of outstanding channel cleanup intents."
  (let ((root (merge-pathnames "channel-removals/" (config :data-root configuration))))
    (ensure-directories-exist root)
    root))

(-> task-child--removal-lock-pathname (configuration) pathname)
(defun task-child--removal-lock-pathname (configuration)
  "Return the guard serializing deletion and recovery, including other processes."
  (merge-pathnames "cleanup.lock" (task-child--removal-root configuration)))

(-> task-child--read-removal (pathname) list)
(defun task-child--read-removal (intent)
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

(-> task-child--removal-identities (list) hash-table)
(defun task-child--removal-identities (plan)
  "Restore the conversation identity paths retained in cleanup PLAN."
  (let ((identities (make-hash-table :test #'equal)))
    (dolist (entry (getf plan :identities))
      (setf (gethash (first entry) identities) (pathname (second entry))))
    identities))

(-> task-child--call-with-removal-leases
    (configuration list &key (:identities hash-table) (:function function) (:tool-name string)) t)
(defun task-child--call-with-removal-leases (configuration channels
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
                       (task-child--busy identifier tool-name)))
                   leases))
           (funcall function))
      (mapc #'conversation-lease-release leases))))

(-> task-child--storage-roots (configuration list hash-table) hash-table)
(defun task-child--storage-roots (configuration channels identities)
  "Group child storage CHANNELS by task root without rediscovering their paths."
  (let ((roots (make-hash-table :test #'equal)))
    (dolist (channel channels)
      (let ((pathname (gethash channel identities)))
        (unless (equal (pathname-directory pathname)
                       (pathname-directory (configuration-conversation-root configuration)))
          (let ((directory (uiop:pathname-directory-pathname pathname)))
            (push (cons channel directory)
                  (gethash (first (last (pathname-directory
                                         (uiop:pathname-parent-directory-pathname directory))))
                           roots))))))
    roots))

(-> task-child--prepare-storage (configuration list hash-table) null)
(defun task-child--prepare-storage (configuration channels identities)
  "Persist every root's delivery evidence before any destructive cleanup."
  (maphash (lambda (root entries)
             (shell-log-persist-owner-delivery configuration root (mapcar #'first entries)))
           (task-child--storage-roots configuration channels identities))
  nil)

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

(-> task-child--finish-removal (configuration pathname) null)
(defun task-child--finish-removal (configuration intent)
  "Finish committed INTENT idempotently, retaining it on failure for automatic retry."
  (loop
    (restart-case
        (handler-case
            (let* ((plan       (task-child--read-removal intent))
                   (channels   (getf plan :channels))
                   (identities (task-child--removal-identities plan)))
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

(-> task-child--find (tool-context string string) (values string participant))
(defun task-child--find (context designator tool-name)
  "Return the channel and agent of the caller's child DESIGNATOR names, by name or channel."
  (let* ((world    (world-load (tool-context-configuration context)))
         (parent   (conversation-identifier (tool-context-conversation context)))
         (key      (string-left-trim "@" designator))
         (children (world-channel-children world parent)))
    (dolist (child children)
      (let* ((identifier (channel-identifier child))
             (agent      (find ':agent (world-channel-residents world identifier)
                               :key #'participant-kind)))
        (when (and agent
                   (or (string= key identifier)
                       (string= key (conversation-identifier-display identifier))
                       (string-equal key (participant-name agent))))
          (return-from task-child--find (values identifier agent)))))
    (error 'task-error
           :message   (format nil "~A names none of your children; channel:current lists them. A child appears there once its first run ends."
                              designator)
           :tool-name tool-name)))

(-> task-child--call-with-lease (tool-context string function &key (:tool-name string)) null)
(defun task-child--call-with-lease (context channel function &key tool-name)
  "Call FUNCTION holding the conversation lease of child CHANNEL, and refuse TOOL-NAME while the child is busy."
  (task-child--quiesce (tool-context-configuration context) (list channel) tool-name)
  (let ((lease (handler-case (conversation-lease-acquire (tool-context-configuration context) channel)
                 (conversation-in-use ()
                   (task-child--busy channel tool-name)))))
    (unwind-protect (funcall function)
      (conversation-lease-release lease)))
  nil)

(-> task-child--quiesce (configuration list string) null)
(defun task-child--quiesce (configuration channels tool-name)
  "Ask the world to release idle unattended hosts before TOOL-NAME leases CHANNELS."
  (let ((entry (world-client-entry configuration)))
    (when entry
      (let ((states (getf (rest (localgroup-query-record entry ':quiesce
                                                      (list :channels channels)))
                         :states)))
        (dolist (state states)
          (unless (member (rest state) '(:closed :idle))
            (task-child--busy (first state) tool-name :state (rest state)))))))
  nil)

(-> task-child--busy (string string &key (:state keyword)) nil)
(defun task-child--busy (channel tool-name &key (state ':leased))
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

(-> task-child--agent (child-tool tool-context string) agent)
(defun task-child--agent (tool context channel)
  "Return an agent over the child CHANNEL's history, in its role, as its runs see it."
  (let* ((parent        (tool-context-agent context))
         (conversation  (conversation-load-by-id (tool-context-configuration context) channel))
         (definition    (task-role-definition (agent-configuration parent)
                                              (or (conversation-role conversation) "task")))
         (configuration (task-configuration-for-definition (agent-configuration parent) definition)))
    (agent-create :configuration configuration
                  :provider      (provider-with-configuration (agent-provider parent) configuration)
                  :conversation  conversation
                  :tool-registry (task-child-tool-registry (agent-tool-registry parent) definition
                                                           :orchestrator (child-tool-orchestrator tool)
                                                           :depth        (1+ (task-parent-depth parent)))
                  :worker        nil)))

(-> task-child--reset-history (configuration string) null)
(defun task-child--reset-history (configuration channel)
  "Replace the history of child CHANNEL, whose lease the caller holds, with an empty one of the same child."
  (let* ((old      (conversation-load-by-id configuration channel))
         (pathname (conversation-pathname old)))
    (conversation-delete-history configuration channel)
    (conversation-append-record
     (conversation-create configuration
                          :identifier        channel
                          :prompt-cache-key  (conversation-prompt-cache-key old)
                          :storage-root      (uiop:pathname-directory-pathname pathname)
                          :parent-identifier (conversation-parent-identifier old)
                          :agent-name        (conversation-agent-name old)
                          :role              (conversation-role old)
                          :origin-directory  (conversation-origin-directory old))
     (goal->record nil)))
  nil)

(-> task-child--subtree (world string) list)
(defun task-child--subtree (world channel)
  "Return CHANNEL and every channel nested below it, each before those nested in it."
  (cons channel
        (loop for child in (world-channel-children world channel)
              append (task-child--subtree world (channel-identifier child)))))

(-> task-child--storage-identities (configuration list &key (:prefer-task-p boolean)) hash-table)
(defun task-child--storage-identities (configuration channels &key prefer-task-p)
  "Resolve CHANNELS once, sharing task-root discovery across the deletion plan.

PREFER-TASK-P selects child storage ahead of a same-ID primary conversation."
  (let* ((tasks      (merge-pathnames "tasks/" (config :data-root configuration)))
         (groups     (when (uiop:directory-exists-p tasks) (uiop:subdirectories tasks)))
         (identities (make-hash-table :test #'equal)))
    (dolist (channel channels)
      (let* ((top-level (merge-pathnames (make-pathname :name channel :type "sexp")
                                        (configuration-conversation-root configuration)))
             (pathname  (or (when (and (not prefer-task-p)
                                      (conversation-storage-active-pathname top-level))
                              top-level)
                            (loop for group in groups
                                  for candidate = (merge-pathnames
                                                   (make-pathname :directory (list ':relative channel)
                                                                  :name channel :type "sexp")
                                                   group)
                                  when (conversation-storage-active-pathname candidate)
                                    return candidate)
                            top-level)))
        (setf (gethash channel identities) pathname)))
    identities))

(-> task-child--delete-root-storage
    (configuration string list &key (:leased-p boolean) (:prepared-p boolean)) null)
(defun task-child--delete-root-storage (configuration root entries &key leased-p prepared-p)
  "Delete ROOT's child ENTRIES under their leases, preserving delivery evidence first."
  (let ((leases nil))
    (unwind-protect
         (progn
           (unless leased-p
             (dolist (entry entries)
               (push (handler-case
                         (conversation-lease-acquire
                          configuration (first entry)
                          :pathname (merge-pathnames (make-pathname :name (first entry) :type "sexp")
                                                    (rest entry)))
                       (conversation-in-use ()
                         (task-child--busy (first entry) "child.remove")))
                     leases)))
           (labels ((delete-storage ()
                      (dolist (entry entries)
                        (conversation-delete-artifacts configuration (first entry))
                        (platform-delete-directory-tree *platform* (rest entry)
                                                        :validate          t
                                                        :if-does-not-exist ':ignore)))

                    (with-source-locks (remaining)
                      (if remaining
                          (let* ((entry (first remaining))
                                 (path  (merge-pathnames
                                         (make-pathname :name (first entry) :type "sexp") (rest entry))))
                            (call-with-file-lock
                             (conversation-picker-source-lock-pathname path)
                             (lambda () (with-source-locks (rest remaining)))))
                          (delete-storage))))
             (if prepared-p
                 (with-source-locks (sort (copy-list entries) #'string< :key #'first))
                 (shell-log-persist-owner-delivery
                  configuration root (mapcar #'first entries) :delete-function #'delete-storage))))
      (mapc #'conversation-lease-release leases)))
  nil)
