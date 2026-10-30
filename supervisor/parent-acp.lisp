;;;; -- Trusted Parent ACP Endpoint --

(in-package #:autolith-supervisor)

(defparameter *parent-acp-session-limit* 32
  "Maximum isolated sessions admitted by one ACP connection.")

(defclass parent-acp-service (agentcomms:acp-agent)
  ((configuration :initarg :configuration :reader parent-acp-configuration
                  :documentation "Private configuration retained by the parent.")
   (request :initarg :request :reader parent-acp-request
            :documentation "Validated command-line options for this connection.")
   (sessions :initform (make-hash-table :test #'equal) :reader parent-acp-sessions
             :documentation "Parent-owned live sessions, keyed by durable identity.")
   (admission-lock :initform      (bordeaux-threads:make-lock "parent ACP admission")
                   :reader        parent-acp-admission-lock
                   :documentation "Serializes complete session opens before publication.")
   (lock :initform (bordeaux-threads:make-lock "parent ACP sessions") :reader parent-acp-lock
         :documentation "Protects session publication and connection shutdown.")
   (closed-p :initform nil :accessor parent-acp-closed-p
             :documentation "Whether connection shutdown has stopped admission."))
  (:documentation "An ACP transport endpoint with no agent code in its heap."))

(defclass parent-acp-session ()
  ((service :initarg :service :reader parent-acp-session-service
            :documentation "The trusted ACP connection owner.")
   (identifier :initarg :identifier :reader parent-acp-session-identifier
               :documentation "Parent-assigned durable identity.")
   (application :initform nil :accessor parent-acp-session-application
                :documentation "The trusted owner of one isolated child.")
   (launch :initform nil :accessor parent-acp-session-launch
           :documentation "The current native child connection.")
   (mcp :initform (make-hash-table :test #'equal) :reader parent-acp-session-mcp
        :documentation "Private registered MCP clients; no declarations cross to the child.")
   (mcp-descriptors :initform #() :accessor parent-acp-session-mcp-descriptors
                    :documentation "Sanitized tool metadata for each replacement child.")
   (events :initform nil :accessor parent-acp-session-events
           :documentation "At most 128 bounded child presentation events.")
   (event-bytes :initform 0 :accessor parent-acp-session-event-bytes
                :documentation "Bytes pending in the presentation queue.")
   (event-in-flight-p :initform nil :accessor parent-acp-session-event-in-flight-p
                      :documentation "Whether the presenter has removed an event but has not sent it.")
   (event-ready :initform (bordeaux-threads:make-condition-variable)
                :reader parent-acp-session-event-ready :documentation "Wake the parent presentation owner.")
   (completion-ready :initform nil :accessor parent-acp-session-completion-ready
                     :documentation "A child callback requested a fresh completion scope.")
   (completion-condition :initform      (bordeaux-threads:make-condition-variable)
                         :reader        parent-acp-session-completion-condition
                         :documentation "Wake the parent completion scheduler.")
   (presenter :initform nil :accessor parent-acp-session-presenter
              :documentation "The parent event writer, never the storage owner.")
   (scheduler :initform nil :accessor parent-acp-session-scheduler
              :documentation "The parent owner of automatic durable application scopes.")
   (policy-controller :initform nil :accessor parent-acp-session-policy-controller
                      :documentation "The parent owner of native policy replacement.")
   (lock :initform (bordeaux-threads:make-lock "parent ACP session")
         :reader parent-acp-session-lock :documentation "Protects admission and approval state.")
   (operation-lock :initform      (bordeaux-threads:make-lock "parent ACP operation")
                   :reader        parent-acp-session-operation-lock
                   :documentation "Orders complete action and close operations.")
   (approval :initform nil :accessor parent-acp-session-approval
             :documentation "The exact parent-generated approval awaiting a client decision.")
   (permission-thread :initform nil :accessor parent-acp-session-permission-thread
                      :documentation "The parent thread waiting for exact client consent.")
   (permission-token :initform nil :accessor parent-acp-session-permission-token
                     :documentation "Identity of the current consent wait, invalidated on cancellation.")
   (mode :initform "ask" :accessor parent-acp-session-mode
         :documentation "The selected permission policy, independent of native confinement.")
   (closed-p :initform nil :accessor parent-acp-session-closed-p
             :documentation "Whether the parent has stopped this session."))
  (:documentation "A private parent application exposed through ACP, not a child-side ACP server."))

(serapeum/types:-> parent-acp--session (parent-acp-service string) parent-acp-session)
(defun parent-acp--session (service identifier)
  "Find a live parent session or report a fixed ACP parameter error."
  (bordeaux-threads:with-lock-held ((parent-acp-lock service))
    (or (gethash identifier (parent-acp-sessions service))
        (agentcomms:acp-invalid-params "Unknown session."))))

(serapeum/types:-> parent-acp--modes (parent-acp-session) hash-table)
(defun parent-acp--modes (session)
  "Describe the current policy without changing mandatory native isolation."
  (agentcomms:json-object
   "modes" (agentcomms:acp-session-mode-state
             (parent-acp-session-mode session)
             (list (agentcomms:acp-session-mode "ask" "Ask")
                   (agentcomms:acp-session-mode "auto" "Auto")
                   (agentcomms:acp-session-mode "sandboxed" "Sandboxed")
                   (agentcomms:acp-session-mode "full-access" "Full access")))))

(serapeum/types:-> parent-acp--cancel-permission (parent-acp-session) null)
(defun parent-acp--cancel-permission (session)
  "Invalidate pending consent and interrupt only its exact wait so transport cleanup runs."
  (let ((thread nil) (token nil))
    (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
      (setf (parent-acp-session-approval session) nil
            thread (parent-acp-session-permission-thread session)
            token (parent-acp-session-permission-token session))
      (when token (setf (first token) t)))
    (when (and thread (bordeaux-threads:thread-alive-p thread))
      (sb-thread:interrupt-thread
       thread
       (lambda ()
         (when (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                 (eq token (parent-acp-session-permission-token session)))
           (error 'agentcomms:acp-prompt-cancelled))))))
  nil)

(serapeum/types:-> parent-acp--permission-input (parent-acp-session real) (values t keyword))
(defun parent-acp--permission-input (session timeout)
  "Request exact consent with bounded waiting and cancellation-safe transport cleanup."
  (let ((approval nil) (token (list nil)))
    (handler-case
        (unwind-protect
             (progn
               (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                 (unless (parent-acp-session-closed-p session)
                   (setf approval (parent-acp-session-approval session)
                         (parent-acp-session-approval session) nil)
                   (when approval
                     (setf (parent-acp-session-permission-thread session) (bordeaux-threads:current-thread)
                           (parent-acp-session-permission-token session) token))))
               (if approval
                   (multiple-value-bind (outcome choice)
                       (agentcomms:agent-request-permission
                        (parent-acp-session-service session) (parent-acp-session-identifier session)
                        (agentcomms:acp-tool-call
                         (or (getf approval :operation-id) (parent-application--identifier))
                         "Approve parent operation" :status ':pending
                         :raw-input                         (agentcomms:json-object "approval" (getf approval :description)))
                        (list (agentcomms:acp-permission-option "allow" "Allow exact operation" ':allow-once)
                              (agentcomms:acp-permission-option "deny" "Reject" ':reject-once))
                        :timeout (max 0.01 timeout))
                     (values (if (and (eq outcome ':selected) (equal choice "allow")
                                      (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
                                        (not (first token)))) "YES" "NO") ':line))
                   (progn
                     (sleep (min 0.05 (max 0 timeout)))
                     (values nil (if (parent-acp-session-closed-p session) ':eof ':timeout)))))
          (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
            (when (eq token (parent-acp-session-permission-token session))
              (setf (parent-acp-session-permission-thread session) nil
                    (parent-acp-session-permission-token session) nil))))
      (agentcomms:acp-error ()
        (values "NO" ':line)))))

(serapeum/types:-> parent-acp--render
                  (parent-acp-session &key (:region keyword) (:kind keyword) (:payload list)) null)
(defun parent-acp--render (session &key region kind payload)
  "Queue child text separately from parent-only approval requests."
  (cond
    ((and (eq region ':trusted) (eq kind ':network-approval))
     (error 'parent-terminal-error :reason ':unsupported-network-approval))
    ((and (eq region ':trusted) (member kind '(:approval :knowledge-approval)))
     (bordeaux-threads:with-lock-held ((parent-acp-session-lock session))
       (when (parent-acp-session-approval session)
         (error 'parent-terminal-error :reason ':concurrent-approval))
       (setf (parent-acp-session-approval session)
             (list :operation-id (getf payload :operation-id)
                   :description  (let ((*print-readably* t) (*print-circle* t))
                                   (write-to-string payload))))))
    ((eq region ':agent)
     (let ((text (getf payload :text)))
       (when text
         (parent-acp-event
          session ':acp
          (agentcomms:json-object
           "method" "session/update"
           "params" (agentcomms:json-object
                     "sessionId" (parent-acp-session-identifier session)
                     "update" (if (eq kind ':diagnostic)
                                  (agentcomms:acp-update-agent-thought (agentcomms:acp-text-content text))
                                  (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text))))))))))
  nil)

(serapeum/types:-> parent-acp--open
                  (parent-acp-service string &key (:identifier (or null string)) (:mcp-servers list))
                  parent-acp-session)
(defun parent-acp--open (service cwd &key identifier mcp-servers)
  "Open an externally confined child and publish it only after complete setup."
  (bordeaux-threads:with-lock-held ((parent-acp-admission-lock service))
   (agentcomms:acp-validate-absolute-path cwd "cwd")
   (unless (uiop:directory-exists-p cwd)
     (agentcomms:acp-invalid-params "The working directory does not exist."))
   (let* ((identity    (or identifier (parent-application--identifier)))
          (session     (make-instance 'parent-acp-session :service service :identifier identity))
          (values      (copy-tree (parent-configuration-values (parent-acp-configuration service))))
          (completed-p nil))
     (unless (parent-application--session-id-p identity)
       (agentcomms:acp-invalid-params "Invalid session identity."))
     (bordeaux-threads:with-lock-held ((parent-acp-lock service))
       (when (or (parent-acp-closed-p service)
                 (>= (hash-table-count (parent-acp-sessions service)) *parent-acp-session-limit*)
                 (gethash identity (parent-acp-sessions service)))
         (error 'agentcomms:acp-state-error :message "Session admission refused.")))
     (when identifier
       (parent-session-ensure (parent-acp-configuration service) identifier))
     (setf (getf values :workspace) cwd)
     (unwind-protect
          (progn
            (setf (parent-acp-session-application session)
                  (make-parent-application
                   (parent-application-prepare-configuration (make-parent-configuration values)) :session-id identity
                   :input-source                                                                             (lambda (timeout) (parent-acp--permission-input session timeout))
                    :renderer                                                                                (lambda (region kind payload)
                                                                                                               (parent-acp--render session :region region :kind kind :payload payload))
                   :event-callback                                                                           (lambda (kind fields) (parent-acp-event session kind fields))
                   :notification-callback                                                                    (lambda (kind identifier) (parent-acp-notification session kind identifier))
                   :operation-handlers                                                                       (parent-acp-operation-handlers session)))
            (setf (parent-acp-session-mode session)
                  (string-downcase
                   (symbol-name
                    (getf (parent-configuration-get
                           (parent-application-configuration (parent-acp-session-application session))
                           ':settings)
                          :permission-mode ':ask)))
                  (parent-acp-session-mcp-descriptors session) (parent-acp-mcp-open session mcp-servers)
                  (parent-acp-session-launch session)
                  (parent-application-open (parent-acp-session-application session)))
            (parent-acp--configure session)
            (bordeaux-threads:with-lock-held ((parent-acp-lock service))
              (when (or (parent-acp-closed-p service) (gethash identity (parent-acp-sessions service)))
                (error 'agentcomms:acp-state-error :message "Session admission lost ownership."))
              (setf (gethash identity (parent-acp-sessions service)) session
                    completed-p t))
            (parent-acp-start-controllers session)
            session)
       (unless completed-p
         (parent-acp-session-close session))))))

(serapeum/types:-> parent-acp--result (keyword t) hash-table)
(defun parent-acp--result (outcome result)
  "Validate a durable action result before exposing public fields."
  (case outcome
    (:cancelled
     (error 'agentcomms:acp-prompt-cancelled))
    (:completed
     (unless (and result (eq (parent-proto:application-result.status result) ':ok))
       (error 'agentcomms:acp-state-error :message "The isolated application action failed."))
     (protocol-map->data (parent-proto:application-result.fields result)))
    (otherwise
     (error 'agentcomms:acp-state-error :message "The isolated application outcome is uncertain."))))

(serapeum/types:-> parent-acp--active-extension-p (parent-acp-session keyword hash-table) boolean)
(defun parent-acp--active-extension-p (session kind fields)
  "Admit editor reads and input changes through a nested durable scope during a prompt."
  (let ((launch (parent-acp-session-launch session)))
    (and launch (eq kind ':acp-extension)
         (member (gethash "method" fields)
                 '("_autolith/state" "_autolith/steer" "_autolith/queue" "_autolith/jobs"
                   "_autolith/job" "_autolith/job-transcript" "_autolith/job-send" "_autolith/job-cancel")
                 :test #'equal)
         (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
           (not (null (parent-launch-turn launch)))))))

(defgeneric parent-acp-session-execute (session kind fields)
  (:documentation "Run one typed child action. Return only its bounded public ACP result."))

(defmethod parent-acp-session-execute ((session parent-acp-session) kind fields)
  "Run root actions in order and admit active editor input without waiting for the prompt."
  (flet ((execute ()
           (when (parent-acp-session-closed-p session)
             (error 'agentcomms:acp-state-error :message "The session is closed."))
           (multiple-value-call #'parent-acp--result
             (parent-launch-application-run
              (parent-acp-session-launch session) (parent-application--identifier)
              (agentcomms:json-encode fields) :kind kind))))
    (if (parent-acp--active-extension-p session kind fields)
        (execute)
        (loop
          (multiple-value-bind (completed-p result)
              (bordeaux-threads:with-lock-held ((parent-acp-session-operation-lock session))
                (if (and (not (parent-acp-session-closed-p session))
                         (parent-acp-session-application session)
                         (parent-application--restart-p (parent-acp-session-application session)))
                    (values nil nil)
                    (values t (execute))))
            (when completed-p (return result)))
          (sleep 0.01)))))

(serapeum/types:-> parent-acp-session-close (parent-acp-session) null)
(defun parent-acp-session-close (session)
  "Cancel active work, stop the native scope, then release the published identity."
  (setf (parent-acp-session-closed-p session) t)
  (parent-acp--cancel-permission session)
  (when (parent-acp-session-launch session)
    (parent-launch-cancel (parent-acp-session-launch session)))
  (bordeaux-threads:with-lock-held ((parent-acp-session-operation-lock session))
    (when (parent-acp-session-launch session)
      (parent-launch-close (parent-acp-session-launch session))
      (setf (parent-acp-session-launch session) nil)))
  (parent-acp-stop-controllers session)
  (parent-acp-mcp-close session)
  (let ((service (parent-acp-session-service session)))
    (bordeaux-threads:with-lock-held ((parent-acp-lock service))
      (when (eq session (gethash (parent-acp-session-identifier session) (parent-acp-sessions service)))
        (remhash (parent-acp-session-identifier session) (parent-acp-sessions service)))))
  nil)

(defmethod agentcomms:agent-implementation ((service parent-acp-service))
  "Identify the trusted parent ACP endpoint."
  (declare (ignore service))
  (agentcomms:acp-implementation "autolith" (string-trim '(#\Newline #\Return)
                                                        (uiop:read-file-string
                                                         (asdf:system-relative-pathname "autolith-supervisor" "VERSION")))))

(defmethod agentcomms:agent-capabilities ((service parent-acp-service))
  "Advertise the parent-backed durable session and text-context interface."
  (declare (ignore service))
  (agentcomms:acp-agent-capabilities :load-session t :embedded-context t :mcp-http t :close t))

(defmethod agentcomms:agent-new-session
    ((service parent-acp-service) &key cwd mcp-servers additional-directories params)
  "Start one native-isolated session from parent settings."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (parent-acp--open service cwd :mcp-servers mcp-servers)))
    (values (parent-acp-session-identifier session) (parent-acp--modes session))))

(defmethod agentcomms:agent-load-session
    ((service parent-acp-service) &key session-id cwd mcp-servers additional-directories params)
  "Resume a durable parent session without loading private files in the child."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (parent-acp--open service cwd :identifier session-id :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (parent-acp-session-execute
           session ':acp-extension (agentcomms:json-object "method" "_autolith/replay"))
          (parent-acp-flush-events session)
          (values (parent-acp-session-identifier session) (parent-acp--modes session)))
      (serious-condition (condition)
        (parent-acp-session-close session)
        (error condition)))))

(defmethod agentcomms:agent-prompt ((service parent-acp-service) session-id prompt params)
  "Run the existing ACP content contract inside the isolated application."
  (let ((result (parent-acp-session-execute
                 (parent-acp--session service session-id) ':acp-prompt
                 (agentcomms:json-object "prompt" (coerce prompt 'vector) "params" params))))
    (or (gethash "stopReason" result) "end_turn")))

(defmethod agentcomms:agent-cancel ((service parent-acp-service) session-id)
  "Cancel the current durable scope without evaluating client code in the parent."
  (let ((session (parent-acp--session service session-id)))
    (parent-acp--cancel-permission session)
    (when (parent-acp-session-launch session)
      (parent-launch-cancel (parent-acp-session-launch session))))
  nil)

(defmethod agentcomms:agent-set-mode ((service parent-acp-service) session-id mode-id params)
  "Apply a fixed permission setting through the child-to-parent operation contract."
  (declare (ignore params))
  (unless (member mode-id '("ask" "auto" "sandboxed" "full-access") :test #'equal)
    (agentcomms:acp-invalid-params "Unknown permission mode."))
  (let* ((session (parent-acp--session service session-id))
         (result  (parent-acp-session-execute
                   session ':acp-extension
                   (agentcomms:json-object "method" "_autolith/set-permission-mode"
                                           "params" (agentcomms:json-object "modeId" mode-id)))))
    (unless (equal (gethash "outcome" result) "ok")
      (error 'agentcomms:acp-state-error :message "Permission mode was not applied."))
    (setf (parent-acp-session-mode session) mode-id)
    (agentcomms:agent-send-update service session-id (agentcomms:acp-update-current-mode mode-id)))
  nil)

(defmethod agentcomms:agent-close-session ((service parent-acp-service) session-id params)
  "Close the exact native session and retain its parent-owned durable records."
  (declare (ignore params))
  (parent-acp-session-close (parent-acp--session service session-id))
  nil)

(defmethod agentcomms:agent-extension-request ((service parent-acp-service) method params)
  "Dispatch bounded inert extension data to the isolated application."
  (if (string= method "_autolith/capabilities")
      (let* ((identifier (agentcomms:acp-field params "sessionId" :type ':string))
             (existing   (if identifier
                             (parent-acp--session service identifier)
                             (bordeaux-threads:with-lock-held ((parent-acp-lock service))
                               (loop for session being the hash-values of (parent-acp-sessions service)
                                     unless (parent-acp-session-closed-p session) return session))))
             (session    (or existing
                             (parent-acp--open service
                                               (getf (parent-configuration-values
                                                      (parent-acp-configuration service)) :workspace)))))
        (unwind-protect
             (parent-acp-session-execute
              session ':acp-extension (agentcomms:json-object "method" method "params" params))
          (unless existing (parent-acp-session-close session))))
      (let ((identifier (agentcomms:acp-field params "sessionId" :type ':string :required-p t)))
        (parent-acp-session-execute
         (parent-acp--session service identifier) ':acp-extension
         (agentcomms:json-object "method" method "params" params)))))

(serapeum/types:-> parent-acp-cli-dispatch
                  (parent-configuration parent-cli-request &key (:input stream) (:output stream))
                  (values boolean t))
(defun parent-acp-cli-dispatch (configuration request &key (input *standard-input*) (output *standard-output*))
  "Serve the current ACP interface through a trusted parent. Return handled-p and outcome."
  (if (not (eq (parent-cli-request-command request) ':acp))
      (values nil nil)
      (let ((service (make-instance 'parent-acp-service :configuration configuration :request request)))
        (unwind-protect
             (progn
               (agentcomms:acp-agent-serve service (agentcomms:make-acp-stream-channel :input input :output output)
                                          :name "Autolith trusted parent ACP")
               (values t ':completed))
          (let ((sessions (bordeaux-threads:with-lock-held ((parent-acp-lock service))
                            (setf (parent-acp-closed-p service) t)
                            (loop for session being the hash-values of (parent-acp-sessions service)
                                  collect session))))
            (dolist (session sessions)
              (parent-acp-session-close session)))))))
