;;;; -- Parent ACP Presentation and Completion Ownership --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-acp-event (parent-acp-session keyword hash-table) null)
(defun parent-acp-event (session kind fields)
  "Admit bounded child ACP data without blocking the private storage owner on editor I/O."
  (when (eq kind ':ui)
    (parent-interface-decode-ui fields)
    (return-from parent-acp-event nil))
  (unless (eq kind ':acp)
    (error 'parent-application-operation-error :code ':invalid-request))
  (let* ((text  (agentcomms:json-encode fields))
         (bytes (session-output--bytes text)))
    (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
      (when (or (parent-acp-session-closed-p session)
                (> bytes 32768) (>= (length (parent-acp-session-events session)) 128)
                (> (+ bytes (parent-acp-session-event-bytes session)) 65536))
        (error 'parent-application-operation-error :code ':resource-limit))
      (setf (parent-acp-session-events session)
            (nconc (parent-acp-session-events session) (list text)))
      (incf (parent-acp-session-event-bytes session) bytes)
      (bordeaux-threads:condition-notify (parent-acp-session-event-ready session))))
  nil)

(serapeum/types:-> parent-acp-notification (parent-acp-session keyword string) null)
(defun parent-acp-notification (session kind identifier)
  "Latch an explicit child completion notification, never an output-text marker."
  (declare (ignore identifier))
  (when (eq kind ':ui-completion-ready)
    (return-from parent-acp-notification nil))
  (unless (eq kind ':acp-completion-ready)
    (error 'parent-application-operation-error :code ':invalid-request))
  (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
    (unless (parent-acp-session-closed-p session)
      (setf (parent-acp-session-completion-ready session) t)
      (bordeaux-threads:condition-notify (parent-acp-session-completion-condition session))))
  nil)

(serapeum/types:-> parent-acp--forward-event (parent-acp-session string) null)
(defun parent-acp--forward-event (session text)
  "Forward only fixed ACP presentation notifications and replace child session labels."
  (let* ((message    (agentcomms:json-decode text))
         (method     (agentcomms:json-get message "method"))
         (params     (agentcomms:json-get message "params"))
         (service    (parent-acp-session-service session))
         (identifier (parent-acp-session-identifier session)))
    (unless (and (hash-table-p message) (hash-table-p params)
                 (not (nth-value 1 (gethash "id" message))))
      (error 'parent-terminal-error :reason ':invalid-acp-update))
    (cond
      ((equal method "session/update")
       (let* ((update (agentcomms:json-get params "update"))
              (name   (agentcomms:json-get update "sessionUpdate")))
         (unless (and (hash-table-p update)
                      (member name '("user_message_chunk" "agent_message_chunk" "agent_thought_chunk"
                                     "tool_call" "tool_call_update" "plan" "available_commands_update"
                                     "current_mode_update" "config_option_update" "usage_update")
                              :test #'equal))
           (error 'parent-terminal-error :reason ':invalid-acp-update))
         (agentcomms:agent-send-update service identifier update)))
      ((equal method "_autolith/event")
       (setf (gethash "sessionId" params) identifier)
       (agentcomms:agent-client-notify service method params))
      (t
       (error 'parent-terminal-error :reason ':invalid-acp-update))))
  nil)

(serapeum/types:-> parent-acp--present (parent-acp-session) null)
(defun parent-acp--present (session)
  "Drain accepted events outside storage ownership, with a bounded transport write."
  (handler-case
      (loop
        (let ((text (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                      (loop until (or (parent-acp-session-events session)
                                      (parent-acp-session-closed-p session))
                            do (bordeaux-threads:condition-wait
                                (parent-acp-session-event-ready session) (parent-acp-session-lock session)))
                      (let ((text (pop (parent-acp-session-events session))))
                          (when text
                            (decf (parent-acp-session-event-bytes session) (session-output--bytes text))
                            (setf (parent-acp-session-event-in-flight-p session) t))
                          text))))
          (unless text (return))
          (unwind-protect
               (sb-ext:with-timeout 2 (parent-acp--forward-event session text))
            (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
              (setf (parent-acp-session-event-in-flight-p session) nil)))))
    (error ()
      (when (parent-acp-session-launch session)
        (parent-launch--fail (parent-acp-session-launch session) ':acp-presentation-failed))))
  nil)

(serapeum/types:-> parent-acp-flush-events (parent-acp-session) null)
(defun parent-acp-flush-events (session)
  "Wait until replay notifications reach the editor before answering session/load."
  (sb-ext:with-timeout 10
    (loop while (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                  (or (parent-acp-session-events session)
                      (parent-acp-session-event-in-flight-p session)))
          do (sleep 0.001)))
  nil)

(serapeum/types:-> parent-acp--schedule (parent-acp-session) null)
(defun parent-acp--schedule (session)
  "Run automatic work only after a child notification and inside a fresh durable scope."
  (handler-case
      (loop
        (when (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                (loop until (or (parent-acp-session-completion-ready session)
                                (parent-acp-session-closed-p session))
                      do (bordeaux-threads:condition-wait
                          (parent-acp-session-completion-condition session) (parent-acp-session-lock session)))
                (setf (parent-acp-session-completion-ready session) nil)
                (parent-acp-session-closed-p session))
          (return))
        (handler-case
            (parent-acp-session-execute
             session ':acp-extension
             (agentcomms:json-object "method" "_autolith/pump"
                                    "params" (agentcomms:json-object "sessionId" (parent-acp-session-identifier session))))
          (agentcomms:acp-prompt-cancelled () nil)))
    (error ()
      (unless (parent-acp-session-closed-p session)
        (when (parent-acp-session-launch session)
          (parent-launch--fail (parent-acp-session-launch session) ':acp-completion-failed)))))
  nil)

(serapeum/types:-> parent-acp--configure (parent-acp-session) null)
(defun parent-acp--configure (session)
  "Configure one replacement child from exact parent projections and public MCP metadata."
  (let ((application (parent-acp-session-application session))
        (launch      (parent-acp-session-launch session)))
    (parent-application-configure application launch)
    (when (plusp (length (parent-acp-session-mcp-descriptors session)))
      (multiple-value-call #'parent-acp--result
        (parent-launch-application-run
         launch (parent-application--identifier)
         (agentcomms:json-encode
          (agentcomms:json-object "method" "_autolith/configure-mcp"
                                 "params" (agentcomms:json-object "tools" (parent-acp-session-mcp-descriptors session))))
         :kind ':acp-extension))))
  nil)

(serapeum/types:-> parent-acp--policy (parent-acp-session) null)
(defun parent-acp--policy (session)
  "Stop the old native scope before applying approved access or revocation."
  (handler-case
      (loop until (parent-acp-session-closed-p session)
            do (let ((application (parent-acp-session-application session))
                     (launch      (parent-acp-session-launch session)))
                 (when (and launch (parent-application--restart-p application))
                   (parent-launch-close launch)
                   (bordeaux-threads:with-lock-held ((parent-acp-session-operation-lock session))
                     (unless (parent-acp-session-closed-p session)
                       (let ((after-stop (parent-application-after-stop application)))
                         (when after-stop
                           (setf (parent-application-after-stop application) nil)
                           (funcall after-stop)))
                       (bordeaux-threads:with-lock-held ((parent-application-lock application))
                         (setf (parent-application-restart application) nil))
                       (setf (parent-acp-session-launch session) (parent-application-open application))
                       (parent-acp--configure session)))))
               (sleep 0.01))
    (error ()
      (when (parent-acp-session-launch session)
        (parent-launch--fail (parent-acp-session-launch session) ':acp-policy-replacement-failed))))
  nil)

(serapeum/types:-> parent-acp-start-controllers (parent-acp-session) null)
(defun parent-acp-start-controllers (session)
  "Start the parent presentation and automatic-scope owners once."
  (unless (parent-acp-session-presenter session)
    (setf (parent-acp-session-presenter session)
          (bordeaux-threads:make-thread (lambda () (parent-acp--present session)) :name "parent ACP presentation")))
  (unless (parent-acp-session-scheduler session)
    (setf (parent-acp-session-scheduler session)
          (bordeaux-threads:make-thread (lambda () (parent-acp--schedule session)) :name "parent ACP completions")))
  (unless (parent-acp-session-policy-controller session)
    (setf (parent-acp-session-policy-controller session)
          (bordeaux-threads:make-thread (lambda () (parent-acp--policy session)) :name "parent ACP policy")))
  nil)

(serapeum/types:-> parent-acp-stop-controllers (parent-acp-session) null)
(defun parent-acp-stop-controllers (session)
  "Wake closing controllers and verify that both parent-owned threads exit."
  (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
    (setf (parent-acp-session-closed-p session) t)
    (bordeaux-threads:condition-notify (parent-acp-session-event-ready session))
    (bordeaux-threads:condition-notify (parent-acp-session-completion-condition session)))
  (dolist (thread (list (parent-acp-session-presenter session) (parent-acp-session-scheduler session)
                       (parent-acp-session-policy-controller session)))
    (when (and thread (not (eq thread (bordeaux-threads:current-thread))))
      (multiple-value-bind (value reason) (sb-thread:join-thread thread :timeout 5 :default nil)
        (declare (ignore value))
        (when reason
          (error 'parent-launch-error :reason ':acp-controller-cleanup-failed)))))
  nil)
