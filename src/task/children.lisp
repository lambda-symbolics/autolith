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
  "Delete the idle child ARGUMENTS name, with everything it owns."
  (declare (ignore tool))
  (task--validate-tool-arguments arguments '("child") "child.remove")
  (multiple-value-bind (channel agent)
      (task-child--find context (tool-argument arguments "child" :required t) "child.remove")
    (let ((below (1- (channel-remove (tool-context-configuration context) channel))))
      (tool-success (if (zerop below)
                        (format nil "Removed @~A." (participant-name agent))
                        (format nil "Removed @~A and the ~D children below it." (participant-name agent) below))))))


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
                "Delete an idle child for good: its channel, its history, and its own children. A child with an isolated worktree is removed only after child.worktree cleans it up.")))
  registry)

(-> channel-remove (configuration string &key (:tool-name string)) (integer 1))
(defun channel-remove (configuration channel &key (tool-name "child.remove"))
  "Delete channel CHANNEL with its agent, its history and every channel nested below it, and return how many went.

This removes a child, or any channel the person no longer wants; never the
inbox. Nothing is deleted while any of them is busy or keeps an isolated
worktree. The deepest channels go first, so a failure partway leaves no
channel without its parent. TOOL-NAME names the refusing operation."
  (let* ((world    (world-load configuration))
         (target   (world--require-channel world channel ':delete-channel))
         (channels (task-child--subtree world channel)))
    (when (channel-inbox-p target)
      (error 'task-error
             :message   "The inbox stays; only other channels can be removed."
             :tool-name tool-name))
    (dolist (identifier channels)
      (when (conversation-lease-busy-p configuration identifier)
        (task-child--busy identifier tool-name))
      (when (uiop:directory-exists-p (task-child--worktree-directory configuration identifier))
        (error 'task-error
               :message   (format nil "Channel ~A keeps an isolated worktree; clean it up with child.worktree first."
                                  (conversation-identifier-display identifier))
               :tool-name tool-name
               :task-id   identifier)))
    (dolist (identifier (reverse channels))
      (task-child--delete configuration identifier))
    (length channels)))

(-> task-child-delete-storage (configuration string) null)
(defun task-child-delete-storage (configuration channel)
  "Delete the history and run artifacts of the idle child CHANNEL, keeping what its shell logs delivered.

The child's directory goes under its shell logs' source lock, after their
delivery evidence is saved outside it, so a later sweep of shell logs still
knows which captures reached their owner. A failure keeps the directory."
  (let ((pathname (conversation-task-pathname-for-id configuration channel)))
    (when pathname
      (let* ((directory (uiop:pathname-directory-pathname pathname))
             (root      (first (last (pathname-directory
                                      (uiop:pathname-parent-directory-pathname directory)))))
             (lease     (handler-case (conversation-lease-acquire configuration channel)
                          (conversation-in-use ()
                            (task-child--busy channel "child.remove")))))
        (unwind-protect
             (shell-log-persist-owner-delivery
              configuration root channel
              :delete-function (lambda ()
                                 (conversation-delete-artifacts configuration channel)
                                 (platform-delete-directory-tree *platform* directory
                                                                 :validate          t
                                                                 :if-does-not-exist ':ignore)))
          (conversation-lease-release lease)))))
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

(-> task-child--worktree-directory (configuration string) pathname)
(defun task-child--worktree-directory (configuration channel)
  "Return where the isolated worktree of the child run CHANNEL would be checked out."
  (merge-pathnames (format nil "task-worktrees/checkouts/~A/" channel)
                   (config :data-root configuration)))

(-> task-child--delete (configuration string) null)
(defun task-child--delete (configuration channel)
  "Delete channel CHANNEL, none of whose nested channels remain: its storage, then its place in the world.

A child's storage keeps what its shell logs delivered; any other channel's
conversation goes with its private artifacts."
  (let* ((world  (world-load configuration))
         (doomed (loop for resident in (world-channel-residents world channel)
                       append (cons (participant-identifier resident)
                                    (mapcar #'participant-identifier
                                            (world-participant-descendants
                                             world (participant-identifier resident)))))))
    (if (conversation-task-pathname-for-id configuration channel)
        (task-child-delete-storage configuration channel)
        (when (conversation-storage-active-pathname (conversation-pathname-for-id configuration channel))
          (conversation-delete configuration channel)))
    (world-channel-delete configuration channel :participants doomed)
    (dolist (participant doomed)
      (world-context-delete configuration participant))
    (world-inbox-delete configuration channel)
    (world-session-forget configuration channel))
  nil)
