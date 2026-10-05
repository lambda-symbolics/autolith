(in-package #:autolith)

;;;; -- Peer Tool Boundary --

(defclass peer-message-tool (tool)
  ((orchestrator :initarg :orchestrator :reader peer-message-tool-orchestrator
                 :documentation "The ordinary shared task orchestrator."))
  (:documentation "Granted peer operation under ordinary registry and worker-host authority."))

(defmethod tool-child-safe-p ((tool peer-message-tool))
  "Expose communication to children, keeping grants and uncertain-outcome decisions primary-only."
  (not (null (member (tool-name tool) '("discover" "inspect" "send" "receive" "ack" "wait") :test #'equal))))

(defmethod tool-execution-policy ((tool peer-message-tool))
  "Use independent bounded waits, serializing each operation with its own authority lock."
  (declare (ignore tool))
  ':parallel)

(-> peer-message--tool-service (peer-message-tool tool-context) peer-message-service)
(defun peer-message--tool-service (tool context)
  "Bind the real agent's root session, creating authority only for an actual primary."
  (let ((actor (tool-context-agent context)))
    (unless (typep actor 'agent) (peer-message--reject ':actor))
    (or (peer-message-service-find actor)
        (progn
          (when (typep actor 'task-child-agent) (peer-message--reject ':peer-scope))
          (with-lock-held (*peer-message-services-lock*)
            (or (gethash (peer-message--actor-session actor) *peer-message-services*)
                (setf (gethash (peer-message--actor-session actor) *peer-message-services*)
                      (peer-message-service-create
                       actor (peer-message-tool-orchestrator tool)
                       :remote-call (lambda (session envelope &key cancelled-p)
                                      (peer-message-daemon-call (agent-configuration actor) session envelope
                                                                :cancelled-p cancelled-p))))))))))

(-> peer-message--tool-grant (peer-message-service agent hash-table) list)
(defun peer-message--tool-grant (service actor arguments)
  "Resolve local primary-owned endpoints and explicitly named optional remote endpoints."
  (peer-message--owner service actor)
  (let* ((session (tool-argument arguments "remote-session"))
         (direction (tool-argument arguments "direction"))
         (sender (tool-argument arguments "sender"))
         (receiver (tool-argument arguments "receiver"))
         (remote (when session
                   (peer-message--identifier session)
                   (let ((job (tool-argument arguments "remote-job"))
                         (execution (tool-argument arguments "remote-execution")))
                     (peer-message--endpoint
                      (list :session session :job job :execution execution))))))
    (when (and session (equal session (peer-message-session service))) (peer-message--reject ':session))
    (when (and (null session) (or direction (tool-argument arguments "remote-job")
                                 (tool-argument arguments "remote-execution")))
      (peer-message--reject ':identity))
    (peer-message-grant
     service actor :id (tool-argument arguments "id" :required t)
     :sender (cond ((and remote (equal direction "incoming")) remote)
                   ((or (null remote) (equal direction "outgoing"))
                    (peer-message--local-endpoint service actor sender))
                   (t (peer-message--reject ':direction)))
     :receiver (cond ((and remote (equal direction "outgoing")) remote)
                     ((or (null remote) (equal direction "incoming"))
                      (peer-message--local-endpoint service actor receiver))
                     (t (peer-message--reject ':direction)))
     :context-p (let ((value (tool-argument arguments "context")))
                  (if (null value) nil (task--json-boolean value "context"))))))

(defmethod tool-execute ((tool peer-message-tool) (context tool-context) arguments)
  "Execute peer operations without changing the caller's captured normal tool policy."
  (let* ((service (peer-message--tool-service tool context))
         (actor (tool-context-agent context))
         (name (tool-name tool))
         (result
           (cond
             ((string= name "grant")
              (task--validate-tool-arguments arguments '("id" "sender" "receiver" "context" "remote-session" "remote-job" "remote-execution" "direction") "peer.grant")
              (peer-message--tool-grant service actor arguments))
             ((string= name "revoke")
              (task--validate-tool-arguments arguments '("id") "peer.revoke")
              (peer-message-revoke service actor (tool-argument arguments "id" :required t)))
             ((string= name "discover")
              (task--validate-tool-arguments arguments nil "peer.discover")
              (peer-message-discover service actor))
             ((string= name "inspect")
              (task--validate-tool-arguments arguments '("id" "offset" "limit") "peer.inspect")
              (peer-message-inspect service actor :id (tool-argument arguments "id")
                                    :offset (or (tool-argument arguments "offset") 0)
                                    :limit (or (tool-argument arguments "limit") 16)))
             ((string= name "send")
              (task--validate-tool-arguments arguments '("id" "receiver" "text") "peer.send")
              (peer-message-send service actor :id (tool-argument arguments "id" :required t)
                                 :receiver (tool-argument arguments "receiver" :required t)
                                 :text (tool-argument arguments "text" :required t)))
             ((string= name "receive")
              (task--validate-tool-arguments arguments '("id") "peer.receive")
              (peer-message-receive service actor :id (tool-argument arguments "id")))
             ((string= name "ack")
              (task--validate-tool-arguments arguments '("id" "token") "peer.ack")
              (peer-message-ack service actor :id (tool-argument arguments "id" :required t)
                                :token (tool-argument arguments "token" :required t)))
             ((string= name "wait")
              (task--validate-tool-arguments arguments '("id" "timeout") "peer.wait")
              (peer-message-wait service actor :id (tool-argument arguments "id")
                                 :timeout (or (tool-argument arguments "timeout") 2)
                                 :cancelled-p (when (typep actor 'task-child-agent)
                                                (lambda () (job-cancellation-reason (task-child-agent-job actor))))))
             ((string= name "resolve")
              (task--validate-tool-arguments arguments '("id" "token" "action") "peer.resolve")
              (peer-message-resolve
               service actor :id (tool-argument arguments "id" :required t)
               :token (tool-argument arguments "token" :required t)
               :action (let ((action (tool-argument arguments "action" :required t)))
                         (cond ((equal action "acknowledge") ':acknowledge)
                               ((equal action "retry") ':retry)
                               ((equal action "cancel") ':cancel)
                               (t (peer-message--reject ':resolution))))))
             (t (peer-message--reject ':operation)))))
    (task-tool-result (task--write-readable-sexp result :pretty-p t) result)))

(-> peer-message-register-tools (tool-registry &key (:orchestrator (option task-orchestrator))) tool-registry)
(defun peer-message-register-tools (registry &key orchestrator)
  "Register peer tools using the existing shared task authority, never a global peer registry."
  (let ((orchestrator (or orchestrator (tool-registry-runtime-binding registry 'task-orchestrator))))
    (unless (typep orchestrator 'task-orchestrator) (peer-message--reject ':orchestrator))
    (tool-registry-describe-namespace registry "peer" "Explicitly granted bounded peer messaging.")
    (flet ((register (name description properties required)
             (tool-registry-register registry
                                     (make-instance 'peer-message-tool :namespace "peer" :name name :orchestrator orchestrator
                                                    :description description :parameters (tool-object-schema properties required)))))
      (register "grant" "Primary-only directional grant. Cross-session grants must be explicitly paired on both sessions."
                (json-object "id" (tool-string-property "Immutable grant ID.")
                             "sender" (tool-string-property "Primary-visible sender job ID, or primary.")
                             "receiver" (tool-string-property "Primary-visible receiver job ID, or primary.")
                             "context" (json-object "type" "boolean")
                             "remote-session" (tool-string-property "Explicit remote session identity.")
                             "remote-job" (tool-string-property "Exact remote job ID; omit for remote primary.")
                             "remote-execution" (tool-string-property "Exact remote execution ID.")
                             "direction" (json-object "type" "string" "enum" #("incoming" "outgoing"))) '("id"))
      (register "revoke" "Primary-only durable grant revocation." (json-object "id" (tool-string-property "Grant ID.")) '("id"))
      (register "discover" "List only granted current peers." (json-object) nil)
      (register "inspect" "Read bounded scoped message metadata and recovery tokens; an exact ID includes its payload."
                (json-object "id" (tool-string-property "Optional exact message ID.")
                             "offset" (tool-integer-property "Metadata page offset.")
                             "limit" (tool-integer-property "Metadata records, 1 to 32.")) nil)
      (register "send" "Send bounded text as the actual actor. Keep the same stable ID when delivery is uncertain."
                (json-object "id" (tool-string-property "Stable message ID.") "receiver" (tool-string-property "Exact granted endpoint ID.")
                             "text" (tool-string-property "1 to 8192 characters.")) '("id" "receiver" "text"))
      (register "receive" "Claim one granted incoming message, returning its acknowledgment token."
                (json-object "id" (tool-string-property "Optional exact message ID.")) nil)
      (register "ack" "Acknowledge the actual receiver's exact delivery token."
                (json-object "id" (tool-string-property "Message ID.") "token" (tool-integer-property "Delivery token.")) '("id" "token"))
      (register "wait" "Wait at most five seconds for incoming work or a message acknowledgment."
                (json-object "id" (tool-string-property "Optional exact sent/received message ID.")
                             "timeout" (json-object "type" "number" "minimum" 0 "maximum" 5)) nil)
      (register "resolve" "Primary-only uncertain delivery decision; retry requires a current grant."
                (json-object "id" (tool-string-property "Message ID.") "token" (tool-integer-property "Current delivery token.")
                             "action" (json-object "type" "string" "enum" #("acknowledge" "retry" "cancel"))) '("id" "token" "action"))))
  registry)
