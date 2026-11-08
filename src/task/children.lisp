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
  "Delete CHANNELS with their agents, histories, worktrees and every channel nested below them, and return how many went.

This removes children, or any channels the person no longer wants; never the
inbox. Nothing is deleted while any of them is busy. A child's isolated
worktree is discarded with it, edits included. The children of one root
share one pass over that root's shell logs, and the deepest channels leave
the world first, so a failure partway leaves no channel without its parent.
TOOL-NAME names the refusing operation."
  (let* ((world (world-load configuration))
         (all   (remove-duplicates
                 (loop for channel in channels
                       do (when (channel-inbox-p (world--require-channel world channel ':delete-channel))
                            (error 'task-error
                                   :message   "The inbox stays; only other channels can be removed."
                                   :tool-name tool-name))
                       append (task-child--subtree world channel))
                 :test     #'string=
                 :from-end t)))
    (dolist (identifier all)
      (when (conversation-lease-busy-p configuration identifier)
        (task-child--busy identifier tool-name)))
    (dolist (identifier all)
      (task-worktree-discard configuration identifier))
    (let ((children (remove-if-not (lambda (identifier)
                                     (conversation-task-pathname-for-id configuration identifier))
                                   all)))
      (task-child-delete-storage configuration children)
      (dolist (identifier (set-difference all children :test #'string=))
        (when (conversation-storage-active-pathname (conversation-pathname-for-id configuration identifier))
          (conversation-delete configuration identifier))))
    (dolist (identifier (reverse all))
      (task-child--forget configuration identifier))
    (length all)))

(-> task-child-delete-storage (configuration list) null)
(defun task-child-delete-storage (configuration channels)
  "Delete the histories and run artifacts of the idle children CHANNELS, keeping what their shell logs delivered.

The children of one root share one pass over its shell logs: their
directories go under their source locks, after the delivery evidence is saved
outside them, so a later sweep of shell logs still knows which captures
reached their owners. A failure keeps the directories."
  (let ((roots (make-hash-table :test #'equal)))
    (dolist (channel channels)
      (let ((pathname (conversation-task-pathname-for-id configuration channel)))
        (when pathname
          (let ((directory (uiop:pathname-directory-pathname pathname)))
            (push (cons channel directory)
                  (gethash (first (last (pathname-directory
                                         (uiop:pathname-parent-directory-pathname directory))))
                           roots))))))
    (maphash (lambda (root entries)
               (task-child--delete-root-storage configuration root entries))
             roots))
  nil)


;;;; -- Private Functions --

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
  (let ((lease (handler-case (conversation-lease-acquire (tool-context-configuration context) channel)
                 (conversation-in-use ()
                   (task-child--busy channel tool-name)))))
    (unwind-protect (funcall function)
      (conversation-lease-release lease)))
  nil)

(-> task-child--busy (string string) nil)
(defun task-child--busy (channel tool-name)
  "Refuse TOOL-NAME because the agent of CHANNEL is busy."
  (error 'task-error
         :message   (format nil "Channel ~A is busy: its agent runs, or a terminal shows it. Wait until it is idle~:[~;, or stop a child's run with job.cancel~]."
                            (conversation-identifier-display channel)
                            (uiop:string-prefix-p "child." tool-name))
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

(-> task-child--delete-root-storage (configuration string list) null)
(defun task-child--delete-root-storage (configuration root entries)
  "Delete the (CHANNEL . DIRECTORY) ENTRIES of ROOT's children under their leases, in one shell log pass."
  (let ((leases nil))
    (unwind-protect
         (progn
           (dolist (entry entries)
             (push (handler-case (conversation-lease-acquire configuration (first entry))
                     (conversation-in-use ()
                       (task-child--busy (first entry) "child.remove")))
                   leases))
           (shell-log-persist-owner-delivery
            configuration root (mapcar #'first entries)
            :delete-function (lambda ()
                               (dolist (entry entries)
                                 (conversation-delete-artifacts configuration (first entry))
                                 (platform-delete-directory-tree *platform* (rest entry)
                                                                 :validate          t
                                                                 :if-does-not-exist ':ignore)))))
      (mapc #'conversation-lease-release leases)))
  nil)

(-> task-child--forget (configuration string) null)
(defun task-child--forget (configuration channel)
  "Take channel CHANNEL, whose storage is gone and none of whose nested channels remain, out of the world."
  (let* ((world  (world-load configuration))
         (doomed (loop for resident in (world-channel-residents world channel)
                       append (cons (participant-identifier resident)
                                    (mapcar #'participant-identifier
                                            (world-participant-descendants
                                             world (participant-identifier resident)))))))
    (world-channel-delete configuration channel :participants doomed)
    (dolist (participant doomed)
      (world-context-delete configuration participant))
    (world-inbox-delete configuration channel)
    (world-session-forget configuration channel))
  nil)
