(in-package #:autolith)

;;;; -- Child Lifecycle Tools --

;;; A parent manages the children in its nested channels. Renaming and empty
;;; updates can run while a child is busy. Other updates and removal require
;;; an idle child.

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

(defclass child-update-tool (child-tool)
  ()
  (:documentation "Update an idle child's model, effort, permissions or history."))

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

(defmethod tool-execute ((tool child-update-tool) (context tool-context) arguments)
  "Update the named child's requested options, or report that nothing changed."
  (let ((mode (task-child--validate-update arguments context)))
    (multiple-value-bind (channel participant)
        (task-child--find context (tool-argument arguments "child" :required t) "child.update")
      (let ((changed nil))
        (when (or (eq (gethash "compact" arguments) t)
                  (gethash "model" arguments) (gethash "effort" arguments) mode)
          (task-child--call-with-lease
           context channel
           (lambda ()
              (setf changed (task-child--apply-update tool context :channel channel
                                                     :participant           participant
                                                     :arguments arguments :mode mode)))
           :tool-name "child.update"))
        (tool-success
         (if changed
             (format nil "Updated @~A: ~{~A~^, ~}." (participant-name participant) changed)
             (format nil "Nothing changed for @~A." (participant-name participant))))))))

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
  "Register tools for renaming, updating and removing children."
  (let ((child (tool-string-property "The child's name, or its channel.")))
    (flet ((register (class name description &key properties required)
             "Register a child lifecycle tool with its argument schema."
             (tool-registry-register
              registry
              (make-instance class :orchestrator orchestrator :namespace "child"
                                   :name name :description description
                                   :parameters (tool-object-schema properties required)))))
      (register 'child-rename-tool "rename"
                "Give one of your children a new name, which @mentions use."
                :properties (json-object "child" child
                             "name" (tool-string-property "The new name: letters, digits, hyphens and underscores."))
                :required   '("child" "name"))
      (register 'child-update-tool "update"
                "Update an idle child's model, reasoning effort, permissions or history. With no options, report that nothing changed."
                :properties (json-object
                             "child" child
                             "compact" (tool-boolean-property "Compact the child's history. Defaults to false.")
                             "model" (tool-string-property "An optional registered model name, with a provider prefix if needed.")
                             "effort" (tool-string-property "An optional supported reasoning effort. A model change preserves the current effort if supported. Otherwise, it uses medium or the first supported effort.")
                             "permissions" (cl-llm-provider-api:provider-enum-schema
                                            '("ask" "auto" "sandboxed" "full-access")
                                            "An optional permission mode within the parent's authority. A full-access parent may select any mode. Other parents may select their own mode. An auto parent may also select ask."))
                :required   '("child"))
      (register 'child-remove-tool "remove"
                "Delete idle children, including their channels, histories and worktrees."
                :properties (json-object "children" (json-object "type" "array" "minItems" 1
                                                     "description" "The children to remove, by name or channel."
                                                     "items" child))
                :required   '("children"))))
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
  "Create an agent with the child's saved model, effort and direct tool grants."
  (let* ((parent       (tool-context-agent context))
         (conversation (conversation-load-by-id (tool-context-configuration context) channel))
         (item         (list :tools (conversation-tool-grants conversation)))
         (configuration
           (application--configuration-for-conversation
            (configuration-copy (agent-configuration parent) :model-selection-override-p nil)
            conversation)))
    (agent-create :configuration configuration
                  :provider      (provider-with-configuration (agent-provider parent) configuration)
                  :conversation  conversation
                  :tool-registry (task-child-tool-registry
                                  (agent-tool-registry parent) item
                                  :orchestrator (child-tool-orchestrator tool)
                                  :depth        (task-conversation-depth configuration conversation))
                  :worker        nil)))


;;; Child update options

(-> task-child--permission-mode (tool-context) (member :ask :auto :sandboxed :full-access))
(defun task-child--permission-mode (context)
  "Return the caller's live or inherited permission mode, with a saved-mode fallback."
  (declare (special *active-application*))
  (let* ((configuration (tool-context-configuration context))
         (conversation  (tool-context-conversation context))
         (origin        (and (typep *active-application* 'application)
                             *active-application*))
         (server        (and origin (application-world-server origin))))
    (labels ((owner (identifier)
               "Return the application hosting IDENTIFIER, or NIL."
               (when origin
                 (if (equal identifier
                            (conversation-identifier (application-conversation origin)))
                     origin
                     (when server
                       (when-let ((host (world-server--find-host server identifier)))
                         (world-host-application host)))))))
      (let ((application
              (or (owner (conversation-identifier conversation))
                  (loop for agent = (tool-context-agent context)
                          then (and (typep agent 'task-child-agent)
                                    (task-job-parent-agent (task-child-agent-job agent)))
                        for application = (and agent (owner (conversation-identifier (agent-conversation agent))))
                        while agent
                        when application return application))))
        (or (and application (application-permission-mode application))
            (permissions-channel-mode configuration (conversation-pathname conversation)
                                      (conversation-created-at conversation))
            ':ask)))))

(-> task-child--validate-update (json-object tool-context) (option keyword))
(defun task-child--validate-update (arguments context)
  "Validate update options and the caller's permission ceiling before changing a child."
  (task--validate-tool-arguments arguments
                                 '("child" "compact" "model" "effort" "permissions")
                                 "child.update")
  (tool-boolean-argument arguments "compact" :tool-name "child.update")
  (dolist (key '("model" "effort"))
    (multiple-value-bind (value present-p) (gethash key arguments)
      (when (and present-p (not (non-empty-string-p value)))
        (error 'task-error :tool-name "child.update"
                           :message   (format nil "~A must be a non-empty string." key)))))
  (multiple-value-bind (value present-p) (gethash "permissions" arguments)
    (when present-p
      (let* ((modes       '(("ask" . :ask) ("auto" . :auto)
                            ("sandboxed" . :sandboxed) ("full-access" . :full-access)))
             (mode        (and (stringp value) (rest (assoc value modes :test #'string=))))
             (parent-mode (task-child--permission-mode context)))
        (unless mode
          (error 'task-error :tool-name "child.update"
                             :message   "permissions must be ask, auto, sandboxed or full-access."))
        (unless (or (eq parent-mode :full-access)
                    (eq mode parent-mode)
                    (and (eq parent-mode :auto) (eq mode :ask)))
          (error 'task-error :tool-name "child.update" :code ':authority
                             :message (format nil "Cannot grant ~A permissions from a parent with ~(~A~) permissions."
                                              value parent-mode)))
        (when (and (eq mode :sandboxed)
                   (not (application--command-sandbox-available-p)))
          (error 'task-error :tool-name "child.update" :code ':authority
                             :message (application--command-sandbox-unavailable-message)))
        mode))))

(-> task-child--updated-configuration (configuration json-object) configuration)
(defun task-child--updated-configuration (configuration arguments)
  "Resolve a child's requested model and effort without changing persistent state."
  (let ((model    (config :model configuration))
        (provider (config :model-provider configuration)))
    (when (gethash "model" arguments)
      (multiple-value-bind (identifier registration)
          (provider-resolve-model (gethash "model" arguments))
        (unless registration
          (error 'task-error :tool-name "child.update"
                             :message   (format nil "Unknown model ~S." (gethash "model" arguments))))
        (setf model identifier
              provider (provider-registration-name registration))))
    (let* ((efforts (configuration--reasoning-efforts-for model provider))
           (effort  (or (gethash "effort" arguments)
                        (and (member (config :reasoning-effort configuration) efforts :test #'string=)
                             (config :reasoning-effort configuration))
                        (find "medium" efforts :test #'string=)
                        (first efforts))))
      (unless (member effort efforts :test #'string=)
        (error 'task-error :tool-name "child.update"
                           :message   (format nil "Model ~A supports these efforts: ~{~A~^, ~}."
                                              model efforts)))
      (configuration-copy configuration :model model :model-provider provider
                                        :reasoning-effort effort))))

(-> task-child--apply-update (child-tool tool-context &key (:channel string) (:participant participant) (:arguments json-object) (:mode (option keyword))) list)
(defun task-child--apply-update (tool context &key channel participant arguments mode)
  "Apply validated options under the child's lease and report any completed changes."
  (let* ((conversation (conversation-load-by-id (tool-context-configuration context) channel))
         (configuration
           (application--configuration-for-conversation
            (configuration-copy (tool-context-configuration context)
                                :model-selection-override-p nil)
            conversation))
         (updated      (task-child--updated-configuration configuration arguments))
         (selection-changed-p
           (or (not (equal (config :model configuration) (config :model updated)))
               (not (equal (config :model-provider configuration) (config :model-provider updated)))
               (not (equal (config :reasoning-effort configuration) (config :reasoning-effort updated)))))
         (changed      nil))
    (handler-case
        (progn
          (when (eq (gethash "compact" arguments) t)
            (agent-compact-conversation (task-child--agent tool context channel)
                                        (make-instance 'agent-observer))
            (push "history" changed)
            (setf conversation
                  (conversation-load-by-id (tool-context-configuration context) channel)))
          (when selection-changed-p
            (conversation-set-model-selection conversation (config :model updated)
                                              (config :reasoning-effort updated)
                                              :provider (config :model-provider updated))
            (push "model/effort" changed))
          (when (or (gethash "model" arguments) (gethash "effort" arguments))
            (let ((settings (copy-list (participant-settings participant))))
              (setf (getf settings :model) (config :model updated)
                    (getf settings :model-provider) (config :model-provider updated)
                    (getf settings :reasoning-effort) (config :reasoning-effort updated))
              (unless (equal settings (participant-settings participant))
                (world-participant-update (tool-context-configuration context)
                                          (participant-identifier participant)
                                          :settings settings)
                (unless selection-changed-p
                  (push "model metadata" changed)))))
          (when (and mode
                     (not (eq mode
                              (permissions-channel-mode
                               (tool-context-configuration context)
                               (conversation-pathname conversation)
                               (conversation-created-at conversation)))))
            (task-child--validate-update arguments context)
            (permissions-save-channel-mode (tool-context-configuration context)
                                           (conversation-pathname conversation)
                                           (conversation-created-at conversation)
                                           :mode mode)
            (push "permissions" changed))
          (nreverse changed))
      (error (condition)
        (error 'task-error :tool-name "child.update"
                           :message   (format nil "Update of @~A failed~@[ after changing ~{~A~^, ~}~]: ~A"
                                              (participant-name participant) (reverse changed)
                                              condition))))))
