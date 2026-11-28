(in-package #:autolith)

;;;; -- Child Channel Creation --

(-> world-child-check-admission (agent) null)
(defun world-child-check-admission (parent)
  "Check the parent depth and current number of child channels before preparation."
  (let* ((configuration (agent-configuration parent))
         (conversation  (agent-conversation parent))
         (identifier    (conversation-identifier conversation))
         (maximum-depth (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                                    *channel-default-maximum-depth* :minimum 1)))
    (unless (< (task-conversation-depth configuration conversation) maximum-depth)
      (error 'task-error :tool-name "child.spawn" :code ':authority
                         :message "The channel depth limit does not permit another child."))
    (world-check-child-capacity (world-load configuration) identifier)))

(-> world-child-create
    (agent &key (:name (option string)) (:identifier (option string))
                (:configuration configuration) (:permission-mode (option keyword))
                (:inherit-reference-p boolean) (:lease (option conversation-lease)))
    conversation)
(defun world-child-create (parent &key name identifier (configuration (agent-configuration parent))
                                       permission-mode (inherit-reference-p t) lease)
  "Create a durable child channel and subscribe PARENT without starting a turn."
  (let* ((origin    (agent-conversation parent))
         (parent-id (conversation-identifier origin))
         (mode      (or permission-mode
                        (permissions-channel-mode (agent-configuration parent)
                                                  (conversation-pathname origin)
                                                  (conversation-created-at origin))
                        ':ask)))
    (unless (world-register-conversation (agent-configuration parent) origin)
      (error 'task-error :tool-name "child.spawn"
                         :message   "Save the parent conversation before creating a child."))
    (let* ((configuration (task-child-configuration configuration nil))
           (conversation (conversation-create configuration :identifier identifier
                                                            :parent-identifier parent-id
                                                            :agent-name name))
           (child-id     (conversation-identifier conversation))
           (held-lease   (or lease (conversation-lease-acquire configuration child-id))))
      (unwind-protect
           (progn
             (unless (conversation-lease-matches-p held-lease child-id)
               (error 'task-error :tool-name "child.spawn" :message "The lease does not own this child."))
             (conversation-append-record
              conversation (list :configuration :model (conversation-model conversation)
                                                 :model-provider (conversation-model-provider conversation)
                                                 :reasoning-effort (conversation-reasoning-effort conversation)))
             (permissions-save-channel-mode configuration (conversation-pathname conversation)
                                            (conversation-created-at conversation) :mode mode)
             (when-let ((limit (and inherit-reference-p
                                    (task-child-reference-history-p parent configuration)
                                    (task-child-reference-history-byte-limit configuration))))
               (conversation-append-inherited-reference
                conversation parent-id (conversation-inherited-reference-snapshot origin limit)))
             (unless (world-register-conversation configuration conversation :subscribe-parent-p t)
               (error 'task-error :tool-name "child.spawn" :message "The child could not be registered."))
             conversation)
        (unless lease (conversation-lease-release held-lease))))))

(-> world-child-send (agent conversation string) list)
(defun world-child-send (parent child text)
  "Deliver TEXT from PARENT to CHILD through the same durable inbox as channel.send."
  (let* ((configuration (agent-configuration parent))
         (source        (agent-conversation parent))
         (sender        (world-register-conversation configuration source)))
    (unless sender
      (error 'task-error :tool-name "child.spawn" :message "The parent has no channel participant."))
    (world-message-send configuration :sender  (participant-identifier sender)
                                      :channel (conversation-identifier child)
                                      :text    text
                                      :source  (conversation-identifier source))))
