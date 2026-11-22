(in-package #:autolith)

;;;; -- Hosted Pooled Assignments --

(defparameter *world-assignment-idle-grace-seconds* 60
  "Idle seconds before an unviewed completed assignment may retire through the server sweeper.")

(-> world-server-prepare-assignment (task-job agent conversation-lease) (option world-host))
(defun world-server-prepare-assignment (job child lease)
  "Register CHILD's existing execution resources with its parent's world server.
The caller retains HOST before starting its serving thread. A standalone child
without a world server keeps its task-scoped resource lifetime."
  (let* ((origin *active-application*)
         (server (and (typep origin 'application) (application-world-server origin))))
    (when server
      (let* ((configuration (agent-configuration child))
             (application
               (application--make-connected-application
                :configuration           configuration
                :conversation            (agent-conversation child)
                :conversation-lease      lease
                :provider                (agent-provider child)
                :tool-registry           (agent-tool-registry child)
                :worker                  (agent-worker child)
                :agent                   child
                :permission-state        (permissions-load configuration)
                :permission-mode         (application-permission-mode origin)
                :installation-provenance (application-installation-provenance origin)
                :client-pane             (list :rows 24 :columns 80)))
             (host          (make-instance 'world-host :application application)))
        (setf (application-world-server application) server
              (application-assignment-job application) job)
        (application--initialize-connection application)
        (with-lock-held ((world-server-lock server))
          (when (or (world-server-stopping-p server)
                    (find (world-host-channel host) (world-server-hosts server)
                          :key #'world-host-channel :test #'equal))
            (error 'world-error :operation ':open :identifier (world-host-channel host)
                   :message "The assignment's channel cannot be registered with this world server."))
          (push host (world-server-hosts server)))
        host))))

(-> world-server-start-assignment (world-host) null)
(defun world-server-start-assignment (host)
  "Publish HOST's serving thread; the caller waits for readiness with interrupts enabled."
  (let* ((application (world-host-application host))
         (server      (application-world-server application)))
    (handler-case
        (world-server--serve-host server host :resume-p t :boot-screen-p nil)
      (error (condition)
        ;; No provider has started, and no serving thread adopted these resources.
        (setf (application-assignment-job application) nil
              (application-tool-registry application) (make-instance 'tool-registry))
        (world-server--note-failure server host condition)
        (world-server--release-host server host)
        (error condition))))
  nil)

(-> application-assignment-cancel (application &key (:reason keyword)) boolean)
(defun application-assignment-cancel (application &key (reason ':cancelled))
  "Cancel APPLICATION's pooled provider owner, never its parked serving thread.
Return true when a pooled assignment owns execution, including during settlement."
  (when-let ((job (application-assignment-job application)))
    (when (job-cancel job :reason reason)
      (session-job-note-cancellation job reason))
    t))

(-> application-assignment-begin (application-input-controller) null)
(defun application-assignment-begin (controller)
  "Connect the busy pooled assignment before admitting any client to its endpoint."
  (let ((application (application-input-controller-application controller)))
    (when (application-assignment-job application)
      (with-lock-held ((application-input-controller-lock controller))
        (setf (application-input-controller-active-p controller) t
              (application-input-controller-active-work-kind controller) ':job-completion))
      (application-connect-task-presentation application)))
  nil)

(-> application-assignment-finish (application &key (:stop-p boolean)) null)
(defun application-assignment-finish (application &key stop-p)
  "Await pooled settlement before adopting ordinary resources or closing the host.
The borrowed assignment registry belongs to the parent's task runtime and is
never closed here. The worker, participant state and conversation lease retain
one lifetime owner, APPLICATION, throughout the transition."
  (when-let ((job (application-assignment-job application)))
    (when stop-p
      (application-assignment-cancel application :reason ':shutdown))
    (job-await job :timeout-seconds (and stop-p *world-server-checkpoint-join-seconds*))
    (unless (job-terminal-p job)
      (error 'world-error :operation ':closing
             :identifier             (conversation-identifier (application-conversation application))
             :message                "The assignment has not ended; its host retains its resources and conversation lease."))
    (let* ((controller  (application-input-controller application))
           (stopping-p  (or stop-p
                            (and controller
                                 (application-input-controller-stopping-p controller))))
           (registry    (if stopping-p
                            (make-instance 'tool-registry)
                            (application--create-tool-registry
                             (application-configuration application)
                             :conversation (application-conversation application))))
           (installed-p nil))
      (unwind-protect
           (let ((agent (unless stopping-p
                          (agent-create
                           :configuration     (application-configuration application)
                           :participant-state (application-participant-state application)
                           :provider          (application-provider application)
                           :conversation      (application-conversation application)
                           :tool-registry     registry
                           :worker            (application-worker application)))))
             (application-disconnect-task-presentation application)
             (setf (application-tool-registry application) registry)
             (when agent
               (setf (application-agent application) agent))
             (setf installed-p t
                   (application-assignment-job application) nil)
             (unless stopping-p
               (application-connect-task-presentation application))
             (when controller
               (application-input-controller--finish-work controller))
             (unless stopping-p
               (let* ((server (application-world-server application))
                      (host   (and server
                                   (world-server--find-host
                                    server (conversation-identifier (application-conversation application))))))
                 (when (and host (not (world-host-attached-p host)))
                   (setf (world-host-idle-grace-seconds host) *world-assignment-idle-grace-seconds*)))
               (application-world-deliver application)))
        (unless installed-p
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> application-assignment-observer (application agent-observer task-job) agent-observer)
(defun application-assignment-observer (application assignment job)
  "Fan assignment output out to ordinary clients without changing job authority.
The assignment's steering and acknowledgements have one original consumer."
  (let ((presentation
          (application-agent-observer
           application
           :user-message-input (getf (task-job-item job) :task)
           :operation-request  (task-child-assignment-request job (application-conversation application)))))
    (callback-agent-observer-create
     :text-callback
     (lambda (text)
       (agent-observer-text assignment text)
       (agent-observer-text presentation text))
     :reasoning-callback
     (lambda (text)
       (agent-observer-reasoning assignment text)
       (agent-observer-reasoning presentation text))
     :status-callback
     (lambda (status details)
       (agent-observer-status assignment status details)
       (agent-observer-status presentation status details))
     :steering-callback (lambda () (agent-observer-take-steering assignment))
     :steering-persisted-callback
     (lambda (identifier) (agent-observer-steering-persisted assignment identifier))
     :steering-entry-callback
     (lambda (agent entry) (agent-observer-apply-steering-entry assignment agent entry))
     :pending-operations-callback
     (lambda (agent) (agent-observer-apply-pending-operations assignment agent))
     :command-authorization-callback
     (lambda (command directory) (agent-observer-authorize-command assignment command directory))
     :tool-authorization-callback
     (lambda (tool arguments) (agent-observer-authorize-tool assignment tool arguments)))))
