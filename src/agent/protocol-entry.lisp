;;;; -- Optional Parent-Backed Child Entry --

(in-package #:autolith)

(-> agent-protocol-entry--configuration
    (agent-wire:welcome) (values configuration string (member :deny :inherited-scope) (option hash-table)))
(defun agent-protocol-entry--configuration (welcome)
  "Build approved child settings without opening parent configuration files."
  (let* ((data                   (autolith-supervisor::protocol-map->data (agent-wire:welcome.configuration welcome)))
         (profile                (gethash "profile_id" data))
         (model                  (gethash "model" data))
         (workspace              (gethash "workspace" data))
         (effort                 (gethash "reasoning_effort" data "none"))
         (worker-core            (gethash "worker_core" data))
         (knowledge-mode         (gethash "knowledge_mode" data "off"))
         (settings               (gethash "settings" data))
         (reasoning-capabilities (gethash "reasoning_capabilities" data))
         (command-policy         (gethash "command_policy" data)))
    (unless (and (stringp profile) (plusp (length profile))
                 (stringp model) (plusp (length model))
                 (stringp workspace) (uiop:absolute-pathname-p workspace)
                 (stringp effort)
                 (member knowledge-mode '("active" "passive" "off") :test #'equal)
                 (or (null settings) (hash-table-p settings))
                 (or (null command-policy) (equal command-policy "inherited-native-scope"))
                 (or (null worker-core) (and (stringp worker-core) (uiop:absolute-pathname-p worker-core))))
      (error 'agent-protocol-error :reason ':invalid-startup-configuration))
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (member key '("profile_id" "model" "workspace" "reasoning_effort" "worker_core"
                                     "command_policy" "knowledge_mode" "allow_global" "settings"
                                     "reasoning_capabilities") :test #'equal)
                 (error 'agent-protocol-error :reason ':unknown-startup-setting))) data)
    (let ((configuration
            (apply #'make-configuration
              :provider-validation-p nil :model model :model-provider "parent"
              :working-directory (uiop:ensure-directory-pathname workspace)
              :reasoning-effort effort :knowledge-mode knowledge-mode
              (when worker-core (list :active-image-core (pathname worker-core))))))
      (when settings
        (agent-protocol-application-settings-apply configuration settings))
      (values configuration profile (if command-policy ':inherited-scope ':deny)
              (agent-protocol-provider--capabilities reasoning-capabilities)))))

(-> agent-protocol-entry-run
    (&key (:stream (option stream)) (:instance-id (option string))
          (:tool-registry (option tool-registry)) (:worker t)
          (:observer-factory (option function)))
    null)
(defun agent-protocol-entry-run (&key stream instance-id tool-registry worker observer-factory)
  "Run a fresh child through the real provider/tool loop on its inherited private channel.
Load this optional entry in an agent image, never in the trusted parent. Arguments
are child-local setup, not Lisp received over the connection. No private account,
credential, preferences file, or user initialization is read by this entry."
  (let* ((connection           (or stream (autolith-supervisor::private-process-child-connection)))
         (identity             (or instance-id (uiop:getenv "AUTOLITH_PARENT_INSTANCE_ID")))
         (client               (and identity (agent-protocol-open connection connection identity)))
         (session              nil)
         (pool                 nil)
         (record-configuration nil))
    (unless client
      (error 'agent-protocol-error :reason ':missing-instance-id))
    (unwind-protect
         (progn
           (agent-protocol-handshake client
                                      :features '("session.v1" "model.v1" "model-items-v1" "records.v1" "access.v1"
                                                  "knowledge.v1" "record-scopes-v1" "application.v1"))
           (multiple-value-bind (configuration profile command-policy reasoning-capabilities)
               (agent-protocol-entry--configuration (agent-protocol-client-welcome client))
             (setf pool (or worker (lisp-worker-pool-create configuration))
                   session
                   (agent-protocol-session-start
                    client configuration profile :tool-registry tool-registry :worker pool
                    :reasoning-capabilities reasoning-capabilities
                    :observer-factory
                     (lambda (driver session)
                       (when (member "records.v1" (agent-wire:welcome.features
                                                   (agent-protocol-client-welcome client)) :test #'equal)
                          (agent-record-resources-install driver configuration :session session)
                          (setf record-configuration configuration))
                       (if observer-factory
                           (funcall observer-factory driver session)
                           (let ((turn-id (lambda ()
                                            (agent-session-conversation-turn-id
                                             (agent-protocol-session-conversation session)))))
                             (if (member "access.v1" (agent-wire:welcome.features
                                                      (agent-protocol-client-welcome client)) :test #'equal)
                                 (make-agent-access-observer
                                  driver :turn-id turn-id :command-policy command-policy
                                  :cancelled-p    (lambda () (agent-protocol-session-cancelled-p session)))
                                (make-agent-protocol-observer driver :turn-id        turn-id
                                                                     :command-policy command-policy)))))))
             (loop until (agent-protocol-session-finished session)
                   do (sleep 0.01))
             (agent-protocol-session-join session)
             (when (agent-protocol-session-failure session)
               (error 'agent-protocol-error :reason ':child-session-failed))))
      (when session
        (agent-protocol-session-close session)
        (agent-protocol-session-join session))
      (agent-protocol-close client)
      (when record-configuration
        (agent-record-resources-uninstall record-configuration))
      (when (typep pool 'lisp-worker-pool)
        (lisp-worker-pool-stop-all pool))))
  nil)
