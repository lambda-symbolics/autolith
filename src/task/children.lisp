(in-package #:autolith)

;;;; -- Child Lifecycle Tools --

;;; A parent keeps its children: it sends them work through their channels,
;;; and child.remove looks after them. Each names one child by its name or its channel, among the channels
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
      (register 'child-remove-tool "remove"
                "Delete idle children for good, several at once: their channels, histories, worktrees and own children."
                (json-object "children" (json-object "type"        "array"
                                                     "description" "The children to remove, by name or channel."
                                                     "minItems"    1
                                                     "items"       child))
                '("children"))))
  registry)


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
           :message   (format nil "Could not find a child agent named ~S. Read channel:current to see your child agents. If you just started this agent, wait until its first run finishes, then check again."
                              designator)
           :tool-name tool-name)))

(-> task-child--call-with-lease (tool-context string function &key (:tool-name string)) null)
(defun task-child--call-with-lease (context channel function &key tool-name)
  "Call FUNCTION holding the conversation lease of child CHANNEL, and refuse TOOL-NAME while the child is busy."
  (channel--quiesce (tool-context-configuration context) (list channel) tool-name)
  (let ((lease
         (handler-case (conversation-lease-acquire (tool-context-configuration context) channel)
                       (conversation-in-use nil (channel--busy channel tool-name)))))
    (unwind-protect (funcall function) (conversation-lease-release lease)))
  nil)

(-> task-child--agent (child-tool tool-context string) agent)
(defun task-child--agent (tool context channel)
  "Create an agent over the child's history and its recorded direct tool grants."
  (let* ((parent        (tool-context-agent context))
         (conversation  (conversation-load-by-id (tool-context-configuration context) channel))
         (item          (list :tools (conversation-tool-grants conversation)))
         (configuration (task-child-configuration (agent-configuration parent) item)))
    (agent-create :configuration configuration
                  :provider      (provider-with-configuration (agent-provider parent) configuration)
                  :conversation  conversation
                  :tool-registry (task-child-tool-registry
                                  (agent-tool-registry parent) item
                                  :orchestrator (child-tool-orchestrator tool)
                                  :depth        (task-conversation-depth configuration conversation))
                  :worker        nil)))
