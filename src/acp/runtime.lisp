(in-package #:autolith)

;;;; -- ACP Agent Methods --

(defmethod agentcomms:agent-implementation ((service acp-service))
  "Identify the loaded Autolith source version."
  (declare (ignore service))
  (agentcomms:acp-implementation "autolith" *autolith-version*))

(defmethod agentcomms:agent-capabilities ((service acp-service))
  "Advertise durable loading, text context, HTTP MCP, and session cleanup."
  (declare (ignore service))
  (agentcomms:acp-agent-capabilities :load-session t :embedded-context t :mcp-http t :close t))

(-> acp-service--modes (acp-session) hash-table)
(defun acp-service--modes (session)
  "Return the permission modes and current selection for SESSION."
  (agentcomms:json-object
   "modes"
   (agentcomms:acp-session-mode-state
    (string-downcase (symbol-name (acp-session-mode session)))
    (list (agentcomms:acp-session-mode "ask" "Ask" :description "Approve commands and external tools in the editor.")
          (agentcomms:acp-session-mode "auto" "Auto" :description "Classify commands using the configured permission policy.")
          (agentcomms:acp-session-mode "sandboxed" "Sandboxed" :description "Run commands in the configured workspace sandbox.")
          (agentcomms:acp-session-mode "full-access" "Full access" :description "Allow commands and external tools without approval.")))))

(-> acp-service--open-session
    (acp-service &key (:cwd string) (:identifier (option string)) (:mcp-servers list)) acp-session)
(defun acp-service--open-session (service &key cwd identifier mcp-servers)
  "Construct a leased headless application, publishing it only after successful setup."
  (acp-service--call-with-operation
   service (gensym "SESSION-SETUP")
   (lambda ()
     (when (>= (hash-table-count (acp-service-sessions service)) *acp-session-maximum-count*)
       (error 'agentcomms:acp-state-error :message "The live session limit is reached."))
     (when (and identifier (gethash identifier (acp-service-sessions service)))
       (agentcomms:acp-invalid-params "Session ~A is already open." identifier))
     (agentcomms:acp-validate-absolute-path cwd "cwd")
     (unless (uiop:directory-exists-p cwd)
       (agentcomms:acp-invalid-params "The working directory does not exist: ~A." cwd))
     (let* ((configuration (configuration-copy
                            (acp-service-configuration service)
                            :working-directory (platform-truename *platform* (uiop:ensure-directory-pathname cwd))))
            (conversation nil) (lease nil) (registry nil) (worker nil)
            (completed-p nil))
       (unwind-protect
            (progn
              (if identifier
                  (progn
                    (setf lease (conversation-lease-acquire configuration identifier)
                          conversation (conversation-load-by-id configuration identifier))
                    (unless (equal identifier (conversation-identifier conversation))
                      (agentcomms:acp-invalid-params "Use the canonical conversation identifier.")))
                  (setf conversation (conversation-create configuration)
                        lease (conversation-lease-acquire configuration
                                                          (conversation-identifier conversation))))
              (setf configuration (application--configuration-for-conversation configuration conversation))
              (let ((provider (provider-create configuration
                                               :reasoning-summaries-p (config :reasoning-traces-p configuration))))
                (setf registry (acp-mcp-create-tool-registry configuration mcp-servers)
                      worker (lisp-worker-pool-create configuration))
                (let* ((application
                        (make-instance 'application
                                       :configuration configuration :conversation conversation
                                       :conversation-lease lease :provider provider
                                       :tool-registry registry :worker worker
                                       :agent (agent-create :configuration configuration :conversation conversation
                                                            :provider provider :tool-registry registry :worker worker)
                                       :ui nil :permission-mode (acp-service-permission-mode service)
                                       :permission-state (permissions-load configuration)))
                       (session (make-instance 'acp-session :service service
                                               :identifier (conversation-identifier conversation)
                                               :application application
                                               :mode (acp-service-permission-mode service))))
                  (acp-input-initialize session)
                  (with-lock-held ((acp-service-lock service))
                    (when (acp-service-closed-p service)
                      (error 'agentcomms:acp-connection-closed))
                    (setf (gethash (acp-session-identifier session) (acp-service-sessions service)) session
                          completed-p t))
                  session)))
         (unless completed-p
           (let ((failures (application--discard-connection-resources nil registry worker)))
             (unless failures
               (when registry
                 (acp-mcp-release-credentials (mcp-tool-registry-manager registry)))))
           (when lease (conversation-lease-release lease))))))))

(defmethod agentcomms:agent-new-session
    ((service acp-service) &key cwd mcp-servers additional-directories params)
  "Create a durable session with session-local client MCP servers."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (acp-completion-start session)
          (values (acp-session-identifier session) (acp-service--modes session)))
      (serious-condition (condition)
        (acp-session-close session)
        (with-lock-held ((acp-service-lock service))
          (remhash (acp-session-identifier session) (acp-service-sessions service)))
        (error condition)))))

(-> acp-replay--record-update (list integer) (option hash-table))
(defun acp-replay--record-update (record turn-sequence)
  "Project one durable record into the canonical ACP transcript update."
  (let ((fields (rest record)))
    (case (first record)
      (:message
       (unless (getf fields :automatic-p)
         (funcall (if (eq (getf fields :role) ':user)
                      #'agentcomms:acp-update-user-message
                      #'agentcomms:acp-update-agent-message)
                  (agentcomms:acp-text-content (getf fields :content)))))
      (:provider-item
       (let* ((item (json-decode (getf fields :wire-json)))
              (type (json-get item "type")))
         (cond
           ((equal type "message")
            (let ((text (response-item-assistant-text item)))
              (when text
                (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text)))))
           ((equal type "reasoning")
            (let ((text (response-item-reasoning-summary item)))
              (when text
                (agentcomms:acp-update-agent-thought (agentcomms:acp-text-content text)))))
           ((equal type "function_call")
            (let ((name (function-call-canonical-name item))
                  (arguments (json-decode (json-get item "arguments"))))
              (agentcomms:acp-update-tool-call
               (agentcomms:acp-tool-call
                (acp-tool-identifier turn-sequence (json-get item "call_id"))
                (acp-tool-title name arguments)
                :name name :kind (acp-tool-kind name) :status ':pending
                :raw-input arguments)))))))
      (:tool-result
       (let ((output (getf fields :output)))
         (agentcomms:acp-update-tool-call-progress
          (agentcomms:acp-tool-call-update
           (acp-tool-identifier turn-sequence (getf fields :call-id))
           :status (if (member (getf fields :status) '(:ok :success)) ':completed ':failed)
           :raw-output output
           :content (list (agentcomms:acp-tool-call-content
                           (agentcomms:acp-text-content output))))))))))

(-> acp-replay-map-updates (conversation function) null)
(defun acp-replay-map-updates (conversation function)
  "Call FUNCTION on chronological updates with durable turn-qualified tool identities."
  (let ((turn-sequence 0))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (and (eq (first record) ':message)
                  (eq (getf (rest record) :role) ':user))
         (setf turn-sequence (getf (rest record) :seq)))
       (let ((update (acp-replay--record-update record turn-sequence)))
         (when update (funcall function update))))))
  nil)

(-> acp-session-replay (acp-session) null)
(defun acp-session-replay (session)
  "Replay chronological durable messages, reasoning, calls, and results before load returns."
  (let ((observer (acp-observer-create session)))
    (acp-replay-map-updates
     (application-conversation (acp-session-application session))
     (lambda (update) (acp-observer--send observer update)))
    (acp-observer--plan observer))
  nil)

(defmethod agentcomms:agent-load-session
    ((service acp-service) &key session-id cwd mcp-servers additional-directories params)
  "Load a leased conversation and replay its history before answering."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :identifier session-id
                                            :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (acp-session-replay session)
          (acp-completion-start session)
          (acp-service--modes session))
      (serious-condition (condition)
        (acp-session-close session)
        (with-lock-held ((acp-service-lock service))
          (remhash session-id (acp-service-sessions service)))
        (error condition)))))

(-> acp-prompt->text (list) string)
(defun acp-prompt->text (blocks)
  "Preserve ordered text and embedded text context, rejecting unsupported content."
  (with-output-to-string (stream)
    (loop for block in blocks for first-p = t then nil
          for type = (agentcomms:acp-content-type block)
          do (unless first-p (terpri stream))
          (case type
            (:text
             (write-string (agentcomms:acp-content-text block) stream))
            (:resource-link
             (format stream "Resource ~A: ~A" (agentcomms:json-get block "name")
                     (agentcomms:json-get block "uri")))
            (:resource
             (let* ((resource (agentcomms:json-get block "resource"))
                    (text (agentcomms:json-get resource "text")))
               (unless (stringp text)
                 (agentcomms:acp-invalid-params "Binary embedded context is not supported."))
               (format stream "Resource ~A:~%~A" (agentcomms:json-get resource "uri") text)))
            (otherwise
             (agentcomms:acp-invalid-params "Unsupported prompt content: ~A." type))))))

(defmethod agentcomms:peer-handle-notification :before
    ((service acp-service) connection method params)
  "Apply cancellation to headless continuations and explicit tool invocations too."
  (declare (ignore connection))
  (when (and (string= method "session/cancel") (hash-table-p params))
    (let* ((identifier (agentcomms:json-get params "sessionId"))
           (session (with-lock-held ((acp-service-lock service))
                      (gethash identifier (acp-service-sessions service))))
           (epoch (and session
                       (with-lock-held ((acp-session-lock session))
                         (when (acp-session-prompt-thread session)
                           (acp-session-epoch session))))))
      (when epoch (acp-session-cancel session :expected-epoch epoch))))
  nil)

(-> acp-session-run-turn (acp-session string &key (:automatic-p boolean)
                                                (:queued-p boolean)
                                                (:prepare (option function))) keyword)
(defun acp-session-run-turn (session text &key automatic-p queued-p prepare)
  "Run one admitted turn, preserving cancellation and durable repair for either input source."
  (let* ((service (acp-session-service session))
         (application (acp-session-application session))
         (conversation (application-conversation application))
         (controller (acp-input-controller session))
         (*acp-extension-invocation-p* (or automatic-p queued-p))
         (start-sequence nil)
         (observer nil)
         (source (cond (queued-p "queue") (automatic-p "completion") (t "prompt")))
         (outcome ':condition)
         (claimed-p nil))
    (acp-service--call-with-operation
     service session
     (lambda ()
       (unwind-protect
            (handler-case
                (progn
                  (with-lock-held ((acp-session-lock session))
                    (when (or (acp-session-closed-p session) (acp-session-closing-p session))
                      (agentcomms:acp-invalid-params "The session is closing or closed."))
                    (when (and (or automatic-p queued-p) (acp-session-cancelled-p session))
                      (error 'application-turn-cancelled))
                    (unless (or automatic-p queued-p)
                      (setf (acp-session-cancelled-p session) nil
                            (acp-session-completion-report session) nil))
                    (incf (acp-session-epoch session))
                    (setf (acp-session-prompt-thread session) (current-thread)
                          (acp-session-prompt-interruptible-p session) t
                          claimed-p t))
                  (let ((*active-application* application)
                        (*configuration* (application-configuration application))
                        (*default-pathname-defaults*
                          (config :working-directory (application-configuration application))))
                    (acp-extension-turn-notify session "running" :source source)
                    (acp-session-check-cancelled session)
                    (let ((prepared (if prepare (funcall prepare) t)))
                      (setf outcome
                            (if (not prepared)
                                ':end-turn
                                (progn
                                  (when controller
                                    (application-input-controller-begin-external-work controller))
                                  (when (and controller (not (or automatic-p queued-p)))
                                    (with-lock-held ((application-input-controller-lock controller))
                                      (setf (application-input-controller-queued-work-paused-p controller) nil)))
                                  (setf start-sequence (conversation-next-sequence conversation)
                                        observer (acp-observer-create session))
                                  (acp-extension-notify session "state")
                                  (agent-run-user-turn
                                   (application-agent application) (if queued-p prepared text)
                                   :observer observer :automatic-p automatic-p
                                   :pending-input-identifier
                                   (and queued-p controller
                                        (application-input-controller-active-work-identifier controller)))
                                  (acp-session--call-with-finalization
                                   session
                                   (lambda ()
                                     (acp-session-check-cancelled session)
                                     (acp-observer-flush observer)
                                     ':end-turn))))))))
              (application-turn-cancelled (condition)
                (setf outcome ':cancelled)
                (acp-session--call-with-finalization
                 session
                 (lambda ()
                   (when start-sequence
                     (application--record-turn-aborted application condition
                                                       :turn-start-sequence start-sequence
                                                       :reason ':cancelled))
                   (when observer
                     (handler-case (acp-observer-flush observer)
                       (serious-condition () nil)))
                   ':cancelled)))
              (serious-condition (condition)
                (acp-session--call-with-finalization
                 session
                 (lambda ()
                   (when start-sequence
                     (application--record-turn-aborted application condition
                                                       :turn-start-sequence start-sequence
                                                       :reason ':application-error))
                   (when observer
                     (handler-case (acp-observer-flush observer)
                       (serious-condition () nil)))
                    (with-lock-held ((acp-session-lock session))
                      (let ((report (princ-to-string condition)))
                        (setf (acp-session-completion-report session)
                              (subseq report 0 (min 2000 (length report))))))
                   (error condition)))))
         (when claimed-p
           (sb-sys:without-interrupts
             (unwind-protect
                  (when controller
                    (application-input-controller-finish-external-work
                     controller :pause-p (not (eq outcome ':end-turn))))
               (with-lock-held ((acp-session-lock session))
                 (setf (acp-session-prompt-interruptible-p session) nil
                       (acp-session-prompt-thread session) nil))
               (acp-extension-turn-notify session "idle" :source source :outcome outcome)
               (acp-extension-notify session "state")
               (acp-completion-wake session)))))))))

(defmethod agentcomms:agent-prompt ((service acp-service) session-id prompt params)
  "Run one explicit user prompt through the shared primary turn boundary."
  (declare (ignore params))
  (acp-session-run-turn (acp-service--session service session-id) (acp-prompt->text prompt)))

(defmethod agentcomms:agent-cancel ((service acp-service) session-id)
  "Interrupt the prompt and its concurrent tools without waiting on the reader."
  (acp-session-cancel (acp-service--session service session-id)))

(defmethod agentcomms:agent-set-mode ((service acp-service) session-id mode-id params)
  "Validate and publish a permission mode change between turns."
  (declare (ignore params))
  (let* ((session (acp-service--session service session-id))
         (mode (find mode-id '(:ask :auto :sandboxed :full-access)
                     :key (lambda (value) (string-downcase (symbol-name value))) :test #'equal)))
    (unless mode (agentcomms:acp-invalid-params "Unknown mode ~A." mode-id))
    (with-lock-held ((acp-session-lock session))
      (when (acp-session-prompt-thread session)
        (error 'agentcomms:acp-state-error :message "Change mode between prompt turns."))
      (setf (acp-session-mode session) mode
            (application-permission-mode (acp-session-application session)) mode)
      (clrhash (acp-session-permissions session)))
    (agentcomms:agent-send-update service session-id (agentcomms:acp-update-current-mode mode-id)))
  nil)

(defmethod agentcomms:agent-close-session ((service acp-service) session-id params)
  "Cancel work and release this session's runtime resources and lease."
  (declare (ignore params))
  (acp-session-close (acp-service--session service session-id))
  (with-lock-held ((acp-service-lock service))
    (remhash session-id (acp-service-sessions service)))
  nil)

(defmethod agentcomms:peer-connection-closed :before ((service acp-service) connection reason)
  "Stop admission and cancel work before the library joins request handlers."
  (declare (ignore connection reason))
  (let ((sessions nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session)))
    (mapc #'acp-session-cancel sessions)))

(defmethod agentcomms:peer-handle-request :around ((service acp-service) connection method params)
  "Keep diagnostics off the protocol stream in every inbound handler thread."
  (let* ((*standard-output* *error-output*) (*trace-output* *error-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io* (make-two-way-stream *standard-input* *error-output*))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*))
    (handler-case (call-next-method)
      (authentication-error ()
        (error 'agentcomms:acp-method-error :code -32000
               :message "Provider authentication required. Configure credentials before starting ACP.")))))

(-> acp-service-close (acp-service) null)
(defun acp-service-close (service)
  "Stop admission and release all sessions, retaining failed ownership for a retry."
  (let ((sessions nil)
        (failed-p nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session)))
    (mapc #'acp-session-cancel sessions)
    (dolist (session sessions)
      (handler-case
          (progn
            (acp-session-close session)
            (with-lock-held ((acp-service-lock service))
              (remhash (acp-session-identifier session) (acp-service-sessions service))))
        (serious-condition ()
          (setf failed-p t))))
    (telemetry--call-safely #'telemetry-shutdown)
    (when failed-p
      (error 'agentcomms:acp-state-error :message "ACP session cleanup failed.")))
  nil)

(-> acp-service-serve (acp-service agentcomms:acp-channel) null)
(defun acp-service-serve (service channel)
  "Own SERVICE and its channel through disconnect and resource cleanup."
  (unwind-protect
       (agentcomms:acp-agent-serve service channel :name "Autolith ACP"
                                   :log-function #'agentcomms:acp-standard-error-log)
    (unwind-protect (acp-service-close service)
      (agentcomms:channel-close channel)))
  nil)

(-> acp-run (configuration &key (:permission-mode keyword) (:pristine-p boolean)) null)
(defun acp-run (configuration &key (permission-mode ':ask) pristine-p)
  "Bootstrap a headless ACP endpoint and serve UTF-8 standard I/O until disconnect."
  (let* ((channel (agentcomms:acp-standard-io-channel))
         (*standard-output* *error-output*)
         (*trace-output* *error-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io* (make-two-way-stream *standard-input* *error-output*))
         (*query-io* *terminal-io*)
         (*debug-io* *terminal-io*))
    (unwind-protect
         ;; Client declarations can contain credentials for the service lifetime.
         (call-with-secret-use
          (lambda ()
            (configuration-ensure-directories configuration)
            (conversation-identifier-migrate configuration)
            (context-runtime-reset)
            (unless pristine-p (durable-mutations-load configuration))
            (image-state-load configuration :pristine-p pristine-p)
            (application--load-extension-configuration configuration :pristine-p pristine-p)
            (setf configuration (provider-bootstrap-configuration configuration))
            (acp-service-serve (make-instance 'acp-service :configuration configuration
                                              :permission-mode permission-mode)
                               channel)))
      (agentcomms:channel-close channel)))
  nil)
