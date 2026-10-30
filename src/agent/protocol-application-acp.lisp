;;;; -- ACP in the Isolated Application --

(in-package #:autolith)

(defclass agent-protocol-acp-channel (agentcomms:acp-channel)
  ((session :initarg :session :reader agent-protocol-acp-channel-session
            :documentation "The sole-reader parent protocol session.")
   (open-p :initform t :accessor agent-protocol-acp-channel-open-p
           :documentation "Whether this notification-only adapter remains open."))
  (:documentation "A child-only notification sink, with no direct editor connection."))

(defclass agent-protocol-acp-session (acp-session)
  ((protocol-session :initarg :protocol-session :reader agent-protocol-acp-protocol-session
                     :documentation "The native-isolated session that owns every action."))
  (:documentation "ACP public state projected from the existing isolated application."))

(defclass agent-protocol-acp-observer (acp-observer)
  ((delegate :initarg :delegate :reader agent-protocol-acp-observer-delegate
             :documentation "The native-scope observer that owns authorization and lifecycle checks."))
  (:documentation "The current ACP presentation with explicit parent-backed authorization."))

(defvar *agent-protocol-acp-sessions* (make-hash-table :test #'eq)
  "Child-local ACP state keyed by application identity; never a parent configuration store.")

(defmethod agentcomms:channel-open-p ((channel agent-protocol-acp-channel))
  "Read the child notification adapter's state."
  (agent-protocol-acp-channel-open-p channel))

(defmethod agentcomms:channel-close ((channel agent-protocol-acp-channel))
  "Close only this child adapter, not the parent protocol connection."
  (setf (agent-protocol-acp-channel-open-p channel) nil)
  nil)

(defmethod agentcomms:channel-read-message ((channel agent-protocol-acp-channel))
  "Reject reads: the parent protocol driver is the sole connection reader."
  (declare (ignore channel))
  (error 'agent-protocol-error :reason ':acp-direct-read-forbidden))

(defmethod agentcomms:channel-write-message ((channel agent-protocol-acp-channel) message)
  "Forward bounded ACP notifications as inert child output, never as parent requests."
  (unless (and (agentcomms:channel-open-p channel) (stringp message) (<= (length message) 32768))
    (error 'agent-protocol-error :reason ':acp-notification-limit))
  (let* ((data    (agentcomms:json-decode message))
         (method  (agentcomms:json-get data "method"))
         (session (agent-protocol-acp-channel-session channel)))
    (unless (and (hash-table-p data)
                 (not (nth-value 1 (gethash "id" data)))
                 (member method '("session/update" "_autolith/event") :test #'equal))
      (error 'agent-protocol-error :reason ':acp-notification-refused))
    (agent-protocol-application-event session ':acp data))
  nil)

(defmethod acp-completion-wake ((session agent-protocol-acp-session))
  "Ask the parent to admit a fresh completion scope."
  (agent-protocol-application-notify
   (agent-protocol-acp-protocol-session session) ':acp-completion-ready)
  nil)

(defmethod acp-observer-create ((session agent-protocol-acp-session))
  "Use native-scope authorization while retaining the current ACP presentation contract."
  (make-instance 'agent-protocol-acp-observer :session session
                 :delegate                             (agent-protocol-session-observer (agent-protocol-acp-protocol-session session))
                 :turn-sequence                        (conversation-next-sequence
                                                        (application-conversation (acp-session-application session)))))

(defmethod agent-observer-authorize-command ((observer agent-protocol-acp-observer) command directory)
  "Authorize the command through the parent-backed native scope."
  (agent-observer-authorize-command (agent-protocol-acp-observer-delegate observer) command directory))

(defmethod agent-observer-authorize-path ((observer agent-protocol-acp-observer) path operations)
  "Request precise filesystem authority through the parent."
  (agent-observer-authorize-path (agent-protocol-acp-observer-delegate observer) path operations))

(defmethod agent-observer-authorize-tool ((observer agent-protocol-acp-observer) tool arguments)
  "Authorize external tools through the parent-backed scope."
  (agent-observer-authorize-tool (agent-protocol-acp-observer-delegate observer) tool arguments))

(defmethod agent-observer-apply-pending-operations ((observer agent-protocol-acp-observer) agent)
  "Check the parent lifecycle before applying pending operations."
  (agent-observer-apply-pending-operations (agent-protocol-acp-observer-delegate observer) agent))

(defmethod agent-observer-take-steering ((observer agent-protocol-acp-observer))
  "Consume native editor steering and parent FIFO messages at the same safe boundary."
  (append (call-next-method)
          (agent-observer-take-steering (agent-protocol-acp-observer-delegate observer))))

(defmethod agent-observer-steering-persisted ((observer agent-protocol-acp-observer) identifier)
  "Acknowledge the input owner only after the conversation append succeeds."
  (call-next-method)
  (let ((protocol (agent-protocol-acp-protocol-session (acp-observer-session observer))))
    (when (with-lock-held ((agent-protocol-session-lock protocol))
            (gethash identifier (agent-protocol-session-application-identifiers protocol)))
      (agent-application-input-steering-persisted protocol identifier)))
  nil)

(defmethod agent-observer-status :before ((observer agent-protocol-acp-observer) status details)
  "Check the native scope at every provider status boundary."
  (declare (ignore status details))
  (agent-session--check-running
   (agent-protocol-acp-protocol-session (acp-observer-session observer))))

(-> agent-protocol-acp--session (agent-protocol-application) agent-protocol-acp-session)
(defun agent-protocol-acp--session (application)
  "Construct ACP state directly from the public projection, without reading private settings."
  (or (gethash application *agent-protocol-acp-sessions*)
      (let* ((protocol   (agent-protocol-application-session application))
             (identifier (conversation-identifier (application-conversation application)))
             (service    (make-instance 'acp-service :configuration (application-configuration application)))
             (session    (make-instance 'agent-protocol-acp-session :service service :identifier identifier
                                        :application application :protocol-session protocol)))
        (agentcomms:acp-agent-connect service (make-instance 'agent-protocol-acp-channel :session protocol))
        (setf (gethash identifier (acp-service-sessions service)) session
              (gethash application *agent-protocol-acp-sessions*) session)
        (unless (application-input-controller application)
          (setf (application-input-controller application)
                (make-instance 'application-input-controller :application application :main-thread nil)))
        (task-completion-connect
         (application-agent application) (lambda () (acp-completion-wake session)))
        session)))

(-> agent-protocol-acp--private-kind (string) (option keyword))
(defun agent-protocol-acp--private-kind (method)
  "Classify fixed private-state methods without accepting function names or forms."
  (cond
    ((member method '("_autolith/settings" "_autolith/set-setting") :test #'equal)
     ':preferences)
    ((string= method "_autolith/models")
     ':model)
    ((string= method "_autolith/conversations")
     ':session)
    ((string= method "_autolith/set-permission-mode")
     ':permissions)
    (t
     nil)))

(-> agent-protocol-acp--prompt (agent-protocol-acp-session hash-table) hash-table)
(defun agent-protocol-acp--prompt (session fields)
  "Run the established ACP text-context conversion and observer inside one durable scope."
  (let* ((protocol (agent-protocol-acp-protocol-session session))
         (prompt   (agentcomms:json-sequence->list (gethash "prompt" fields))))
    (agent-session--check-running protocol)
    (let ((outcome (acp-session-run-turn session (acp-prompt->text prompt))))
      (agent-session--check-running protocol)
      (agentcomms:json-object "stopReason" (if (eq outcome ':cancelled) "cancelled" "end_turn")))))

(defclass agent-protocol-mcp-tool (tool)
  ((application :initarg :application :reader agent-protocol-mcp-tool-application
                :documentation "The child application with parent operation authority.")
   (server :initarg :server :reader agent-protocol-mcp-tool-server
           :documentation "Opaque parent server handle.")
   (raw-name :initarg :raw-name :reader agent-protocol-mcp-tool-raw-name
             :documentation "The exact discovered MCP tool name."))
  (:documentation "A public tool descriptor with no transport or credentials."))

(defmethod tool-execute ((tool agent-protocol-mcp-tool) (context tool-context) (arguments hash-table))
  "Call a registered parent MCP handle and render its bounded public content."
  (let* ((reply   (agent-application--parent-call
                   (agent-protocol-mcp-tool-application tool) ':mcp
                   (agentcomms:json-object
                    "operation" "call" "server_id" (agent-protocol-mcp-tool-server tool)
                    "tool_name" (agent-protocol-mcp-tool-raw-name tool) "arguments" arguments)))
         (result  (gethash "result" reply))
         (error-p (eq (gethash "isError" result) t)))
    (multiple-value-bind (content attachments blocks sections)
        (mcp-tools--render-content
         context (gethash "content" result)
         (format nil "mcp://~A/~A" (tool-namespace tool) (agent-protocol-mcp-tool-raw-name tool))
         :structured-content (gethash "structuredContent" result) :include-images-p (not error-p))
      (change-class
       (if error-p
           (tool-failure content)
           (tool-success content :content-blocks (when attachments blocks)))
       'mcp-content-result :sections sections))))

(-> agent-protocol-acp--configure-mcp (agent-protocol-acp-session hash-table) hash-table)
(defun agent-protocol-acp--configure-mcp (session params)
  "Register sanitized public descriptors without creating child transports."
  (let* ((application (acp-session-application session))
         (descriptors (agentcomms:json-sequence->list (gethash "tools" params)))
         (servers     (remove-duplicates (mapcar (lambda (item) (gethash "serverName" item)) descriptors)
                                         :test #'equal))
         (namespaces  (mcp-tools--identifier-map servers :prefix "mcp__")))
    (dolist (server servers)
      (let* ((tools (remove-if-not (lambda (item) (equal server (gethash "serverName" item))) descriptors))
             (names (mcp-tools--identifier-map (mapcar (lambda (item) (gethash "name" item)) tools)
                                               :identity-scope server)))
        (dolist (item tools)
          (let ((name (gethash "name" item)))
            (tool-registry-register
             (agent-tool-registry (application-agent application))
             (make-instance 'agent-protocol-mcp-tool
                            :namespace (gethash server namespaces) :name (gethash name names)
                            :description (or (gethash "description" item) (format nil "Call ~A." name))
                            :parameters (gethash "inputSchema" item) :application application
                            :server (gethash "serverId" item) :raw-name name))))))
    (agentcomms:json-object "registered" (length descriptors))))

(-> agent-protocol-acp--extension (agent-protocol-acp-session hash-table) hash-table)
(defun agent-protocol-acp--extension (session fields)
  "Invoke the existing fixed extension registry, with private operations in the parent."
  (let* ((application          (acp-session-application session))
         (method               (agentcomms:acp-field fields "method" :type ':string :required-p t))
         (params               (or (gethash "params" fields) (agentcomms:json-object)))
         (private-kind         (agent-protocol-acp--private-kind method))
         (*active-application* application)
         (*configuration*      (application-configuration application)))
    (cond
      ((string= method "_autolith/configure-mcp")
       (agent-protocol-acp--configure-mcp session params))
      ((string= method "_autolith/replay")
       (acp-session-replay session)
       (agentcomms:json-object "replayed" t))
      (private-kind
       (agent-application--parent-call
        application private-kind (agentcomms:json-object "acp_method" method "params" params)))
      ((string= method "_autolith/capabilities")
       (acp-extension-capabilities))
      ((string= method "_autolith/pump")
       (acp-completion--cycle session)
       (agentcomms:json-object "pending" (if (acp-input--pending-p session) t (argo:json-false))))
      (t
       (let ((entry (assoc method *acp-extension-methods* :test #'equal)))
         (unless (and entry (fboundp (second entry)))
           (error 'agentcomms:acp-method-error :code -32601 :message "Unknown extension method."))
         (unless (equal (gethash "sessionId" params) (acp-session-identifier session))
           (agentcomms:acp-invalid-params "The extension does not name this session."))
         (handler-case
             (acp-extension-result ':ok :value (funcall (second entry) session params))
           (application-turn-cancelled (condition)
             (error condition))
           (acp-extension-unavailable (condition)
             (acp-extension-result ':unavailable :report (acp-extension-unavailable-reason condition)))
           (error (condition)
             (acp-extension-result ':condition :report (princ-to-string condition)
                                   :condition-type     (princ-to-string (type-of condition))))))))))

(-> agent-protocol-application-acp
    (agent-protocol-application agent-wire:application-action) hash-table)
(defun agent-protocol-application-acp (application action)
  "Execute bounded inert ACP input only in the isolated application and return public data."
  (let* ((source  (agent-wire:application-action.source action))
         (fields  (agentcomms:json-decode source))
         (session (agent-protocol-acp--session application)))
    (unless (and (<= (length source) 262144) (hash-table-p fields))
      (error 'agent-protocol-error :reason ':invalid-acp-action))
    (agent-session--check-running (agent-protocol-acp-protocol-session session))
    (case (agent-wire:application-action.kind action)
      (:acp-prompt
       (agent-protocol-acp--prompt session fields))
      (:acp-extension
       (agent-protocol-acp--extension session fields))
      (otherwise
       (error 'agent-protocol-error :reason ':invalid-acp-action)))))
