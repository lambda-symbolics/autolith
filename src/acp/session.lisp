(in-package #:autolith)

;;;; -- ACP Sessions --

;; ACP sessions bind one client session id to one connected Autolith
;; application. This file owns the session table, the client-to-agent method
;; dispatch, session lifetime, and conversation replay. Presentation mapping
;; lives in acp/observer.lisp.

;;;; -- Server State --

(defclass acp-session ()
  ((identifier
    :initarg :identifier
    :reader acp-session-identifier
    :type string
    :documentation "The ACP session id shared with the client.")
   (server
    :initarg :server
    :reader acp-session-server
    :type acp-server
    :documentation "The server owning this session.")
   (application
    :initarg :application
    :reader acp-session-application
    :type application
    :documentation "The connected application backing this session.")
   (mode
    :initarg :mode
    :accessor acp-session-mode
    :type keyword
    :documentation "The current command permission mode keyword.")
   (cancellation-lock
    :initform (make-lock "Autolith ACP cancellation")
    :reader acp-session-cancellation-lock
    :documentation "The lock guarding the turn cancellation flag.")
    (cancelled-p
      :initform nil
      :accessor acp-session-cancelled-p
      :type boolean
      :documentation "Whether the client asked to cancel the running turn.")
   (reasoning-lock
     :initform (make-lock "Autolith ACP reasoning")
     :reader acp-session-reasoning-lock
     :documentation
     "The lock guarding the buffered reasoning text of the turn.")
    (reasoning-text
      :initform ""
      :accessor acp-session-reasoning-text
      :type string
      :documentation
      "The reasoning text buffered before its next thought chunk."))
  (:documentation "One ACP session bound to one connected application."))

(defclass acp-server ()
  ((connection
    :initarg :connection
    :reader acp-server-connection
    :type acp-connection
    :documentation "The JSON-RPC connection serving this server.")
   (configuration
    :initarg :configuration
    :reader acp-server-configuration
    :type configuration
    :documentation "The startup configuration shared by sessions.")
   (sessions
    :initform (make-hash-table :test #'equal)
    :reader acp-server-sessions
    :type hash-table
    :documentation "The table from session id to session.")
   (lock
    :initform (make-lock "Autolith ACP sessions")
    :reader acp-server-lock
    :documentation "The lock guarding the session table."))
  (:documentation "One ACP agent server over one connection."))


;;;; -- Turn Cancellation --

(define-condition acp-turn-cancelled (autolith-error)
  ((session
    :initarg :session
    :reader acp-turn-cancelled-session
    :type acp-session
    :documentation "The session whose client cancelled the turn."))
  (:documentation
   "Signals that the client asked to cancel SESSION's running prompt.")
  (:report (lambda (condition stream)
             (format stream "The client cancelled the ACP turn for session ~A."
                     (acp-session-identifier
                      (acp-turn-cancelled-session condition))))))

(-> acp--cancel-session-turn (acp-session) null)
(defun acp--cancel-session-turn (session)
  "Record that SESSION's client asked to cancel its running turn."
  (with-lock-held ((acp-session-cancellation-lock session))
    (setf (acp-session-cancelled-p session) t))
  nil)

(-> acp--reset-session-turn (acp-session) null)
(defun acp--reset-session-turn (session)
  "Clear SESSION's turn cancellation flag before one new prompt."
  (with-lock-held ((acp-session-cancellation-lock session))
    (setf (acp-session-cancelled-p session) nil))
  nil)

(-> acp--session-turn-cancelled-p (acp-session) boolean)
(defun acp--session-turn-cancelled-p (session)
  "Return true when SESSION's client asked to cancel its running turn."
  (with-lock-held ((acp-session-cancellation-lock session))
    (acp-session-cancelled-p session)))

(-> acp--session-check-cancellation (acp-session) null)
(defun acp--session-check-cancellation (session)
  "Signal acp-turn-cancelled when SESSION's client cancelled its turn.
  Observer callbacks call this at every safe boundary."
  (when (acp--session-turn-cancelled-p session)
    (error 'acp-turn-cancelled :session session)))
;;;; -- Mode Mapping --

(defparameter *acp-mode-ids*
  (list (list ':ask "ask" "Ask"
              "Prompt unless this exact command was saved")
        (list ':auto "auto" "Auto"
              "Classify commands automatically without prompting")
        (list ':sandboxed "sandbox" "Sandbox"
              "Allow commands inside the workspace sandbox")
        (list ':full-access "full" "Full access"
              "Run commands with full user privileges"))
  "The permission mode keyword with its ACP id, name, and description per row.")

(-> acp--mode-id (keyword) string)
(defun acp--mode-id (mode)
  "Return MODE's ACP mode id."
  (second (find mode *acp-mode-ids* :key #'first)))

(-> acp--available-modes () list)
(defun acp--available-modes ()
  "Return the mode rows Autolith can offer, honoring sandbox availability."
  (if (application--command-sandbox-available-p)
      *acp-mode-ids*
      (remove ':sandboxed *acp-mode-ids* :key #'first)))

(-> acp--mode-object (list) json-object)
(defun acp--mode-object (row)
  "Return the ACP mode object for one *acp-mode-ids* row."
  (json-object "id" (second row)
               "name" (third row)
               "description" (fourth row)))

(-> acp--modes-object (keyword) json-object)
(defun acp--modes-object (mode)
  "Return the ACP modes object for MODE."
  (json-object "currentModeId" (acp--mode-id mode)
               "availableModes"
               (apply #'json-array
                      (mapcar #'acp--mode-object
                              (acp--available-modes)))))

(-> acp--initial-mode (configuration) keyword)
(defun acp--initial-mode (configuration)
  "Return the starting command permission mode for ACP sessions."
  (or (preferences-permission-mode configuration) ':ask))

(-> acp--mode-from-id (string) (option keyword))
(defun acp--mode-from-id (mode-id)
  "Return the permission mode keyword for MODE-ID, or NIL if it is unknown."
  (first (find mode-id *acp-mode-ids* :key #'second :test #'string=)))

(-> acp--current-mode-update (acp-session) null)
(defun acp--current-mode-update (session)
  "Notify SESSION's client that its permission mode changed."
  (acp--session-update
   session "current_mode_update"
   (acp--modes-object (acp-session-mode session))))

;;;; -- Session Configuration --

(-> acp--session-configuration (acp-server json-object) configuration)
(defun acp--session-configuration (server params)
  "Return the session configuration honoring the client's cwd."
  (let ((cwd (json-get params "cwd")))
    (cond
      ((null cwd)
       (acp-server-configuration server))
      ((not (stringp cwd))
       (error 'acp-method-error
              :code *acp-invalid-params-code*
              :message "The session cwd field must be a string."))
      (t
       (handler-case
           (configuration-with-working-directory
            (acp-server-configuration server) cwd)
         (configuration-error (condition)
           (error 'acp-method-error
                  :code *acp-invalid-params-code*
                  :message (autolith-error-message condition))))))))

;;;; -- Initialize --

(-> acp--agent-version () string)
(defun acp--agent-version ()
  "Return the running Autolith version."
  (handler-case
      (asdf:component-version (asdf:find-system '#:autolith))
    (error ()
      "unknown")))

(-> acp--handle-initialize (acp-server json-object) json-object)
(defun acp--handle-initialize (server params)
  "Reply to the client's initialize request with Autolith's capabilities."
  (declare (ignore server))
  (let ((version (json-get params "protocolVersion")))
    (unless (integerp version)
      (error 'acp-method-error
             :code *acp-invalid-params-code*
             :message "The initialize protocolVersion field must be an integer."))
    (json-object
     "protocolVersion" version
     "agentCapabilities"
     (json-object "loadSession" t
                  "promptCapabilities"
                  (json-object "image" *json-decoded-false*
                               "audio" *json-decoded-false*
                               "embeddedContext" *json-decoded-false*))
     ;; The authMethods array stays empty until Autolith grows a
     ;; provider login flow, so clients never send authenticate.
     "agentInfo"
     (json-object "name" "Autolith" "title" "Autolith"
                  "version" (acp--agent-version))
     "authMethods" (json-array))))

;;;; -- Session Registry --

(defparameter *acp-session-id-prefix* "sess_"
  "The prefix of every ACP session id Autolith issues.")

(-> acp--session-identifier (string) string)
(defun acp--session-identifier (conversation-identifier)
  "Return the ACP session id for CONVERSATION-IDENTIFIER."
  (concatenate 'string *acp-session-id-prefix* conversation-identifier))

(-> acp--session-conversation-identifier (string) (option string))
(defun acp--session-conversation-identifier (session-id)
  "Return the conversation identifier encoded in SESSION-ID, or NIL."
  (let ((prefix *acp-session-id-prefix*))
    (and (> (length session-id) (length prefix))
         (string= (subseq session-id 0 (length prefix)) prefix)
         (subseq session-id (length prefix)))))

(-> acp-server-register (acp-server acp-session) null)
(defun acp-server-register (server session)
  "Add SESSION to SERVER's session table."
  (with-lock-held ((acp-server-lock server))
    (setf (gethash (acp-session-identifier session)
                   (acp-server-sessions server))
          session))
  nil)

(-> acp-server-session (acp-server string) (option acp-session))
(defun acp-server-session (server session-id)
  "Return the session SESSION-ID registered on SERVER, or NIL."
  (with-lock-held ((acp-server-lock server))
    (gethash session-id (acp-server-sessions server))))

;;;; -- Session Updates --

(-> acp--session-update (acp-session string json-object) null)
(defun acp--session-update (session update-name update)
  "Send one session/update notification naming UPDATE-NAME to SESSION's client."
  (let ((update-object (json-object-copy update)))
    (setf (gethash "sessionUpdate" update-object) update-name)
    (acp-connection-notify
     (acp-server-connection (acp-session-server session))
     "session/update"
     (json-object "sessionId" (acp-session-identifier session)
                  "update" update-object))))

(-> acp--session-chunk (acp-session string string) null)
(defun acp--session-chunk (session update-name text)
  "Send one text chunk notification of kind UPDATE-NAME to SESSION's client."
  (acp--session-update
   session update-name
   (json-object "content" (json-object "type" "text" "text" text))))

(defparameter *acp-reasoning-flush-chars* 800
  "The buffered reasoning length that forces one thought-chunk notification.
Smaller fragments ride along so the editor renders one thinking batch
instead of one message per fragment.")

(-> acp--session-reasoning (acp-session string) null)
(defun acp--session-reasoning (session text)
  "Buffer one reasoning TEXT fragment for SESSION and flush the buffer once
it grows past *acp-reasoning-flush-chars*."
  (block nil
    (let (flush)
      (with-lock-held ((acp-session-reasoning-lock session))
        (let ((current
                (concatenate 'string
                             (acp-session-reasoning-text session)
                             text)))
          (if (>= (length current) *acp-reasoning-flush-chars*)
              (progn
                (setf (acp-session-reasoning-text session) "")
                (setf flush current))
              (setf (acp-session-reasoning-text session) current))))
      (when flush
        (acp--session-chunk session "agent_thought_chunk" flush)))
    nil))

(-> acp--flush-reasoning (acp-session) null)
(defun acp--flush-reasoning (session)
  "Emit SESSION's buffered reasoning text, if any, as one thought chunk.
Other update kinds call this first so thinking never lags its context."
  (let ((pending
          (with-lock-held ((acp-session-reasoning-lock session))
            (prog1 (acp-session-reasoning-text session)
              (setf (acp-session-reasoning-text session) "")))))
    (when (non-empty-string-p pending)
      (acp--session-chunk session "agent_thought_chunk" pending))
    nil))

(-> acp--reset-reasoning (acp-session) null)
(defun acp--reset-reasoning (session)
  "Discard SESSION's buffered reasoning text before one new prompt."
  (with-lock-held ((acp-session-reasoning-lock session))
    (setf (acp-session-reasoning-text session) ""))
  nil)

;;;; -- Conversation Replay --

(defparameter *acp-replay-call-id-prefix* "replay_"
  "The prefix of tool call ids issued during session replay.")

(-> acp--replay-call-id (integer) string)
(defun acp--replay-call-id (sequence)
  "Return the stable replay tool call id for one record SEQUENCE."
  (format nil "~A~D" *acp-replay-call-id-prefix* sequence))

(-> acp--replay-tool-call (acp-session integer string) null)
(defun acp--replay-tool-call (session sequence tool)
  "Report one replayed tool call as still running."
  (acp--session-update
   session "tool_call"
   (json-object "toolCallId" (acp--replay-call-id sequence)
                "title" (or tool "tool")
                "kind" "other"
                "status" "in_progress"))
  nil)

(-> acp--replay-tool-result (acp-session integer keyword string) null)
(defun acp--replay-tool-result (session sequence status output)
  "Report one replayed tool result with its final status."
  (acp--session-update
   session "tool_call_update"
   (json-object "toolCallId" (acp--replay-call-id sequence)
                "status" (ecase status
                           (':ok "completed")
                           (':error "failed")
                           (':neutral "failed")
                           (':mechanics "completed"))
                "content"
                (json-array
                 (json-object "type" "content"
                              "content"
                              (json-object "type" "text"
                                           "text" (or output ""))))))
  nil)

(-> acp--replay-web-search (acp-session integer string) null)
(defun acp--replay-web-search (session sequence detail)
  "Report one replayed web search as a completed fetch call."
  (acp--session-update
   session "tool_call"
   (json-object "toolCallId" (acp--replay-call-id sequence)
                "title" "web search"
                "kind" "fetch"
                "status" "completed"
                "content"
                (json-array
                 (json-object "type" "content"
                              "content"
                              (json-object "type" "text"
                                           "text" (or detail ""))))))
  nil)

(-> acp--replay-record (acp-session list) null)
(defun acp--replay-record (session record)
  "Stream one durable replay record to SESSION's client."
  (let ((projected (conversation-replay--project-record record)))
    (when projected
      (let ((kind (first projected))
            (fields (rest projected)))
        (cond
          ((eq kind ':message)
           (let ((role (getf fields :role))
                 (automatic-p (getf fields :automatic-p))
                 (content (getf fields :content)))
             (when (and (stringp content) (not automatic-p))
               (cond
                 ((eq role ':user)
                  (acp--session-chunk session "user_message_chunk" content))
                 ((eq role ':assistant)
                  (acp--session-chunk session "agent_message_chunk"
                                      content))))))
          ((eq kind ':assistant)
           (let ((content (getf fields :content)))
             (when (stringp content)
               (acp--session-chunk session "agent_message_chunk" content))))
          ((eq kind ':reasoning)
           (let ((content (getf fields :content)))
             (when (stringp content)
               (acp--session-chunk session "agent_thought_chunk" content))))
          ((eq kind ':tool-call)
           (acp--replay-tool-call session
                                  (getf fields :seq)
                                  (getf fields :tool)))
          ((eq kind ':tool-result)
           (acp--replay-tool-result session
                                    (getf fields :seq)
                                    (getf fields :status)
                                    (getf fields :output)))
          ((eq kind ':web-search)
           (acp--replay-web-search session
                                   (getf fields :seq)
                                   (getf fields :detail)))))))
  nil)

(-> acp--replay-session (acp-session) null)
(defun acp--replay-session (session)
  "Stream SESSION's durable conversation to the client as session updates."
  (conversation-replay--map-records
   (application-conversation (acp-session-application session))
   (lambda (record)
     (acp--replay-record session record)))
  nil)

;;;; -- Session Handlers --

(-> acp--handle-session-new (acp-server json-object) json-object)
(defun acp--handle-session-new (server params)
  "Create one new session with its own durable conversation."
  (let* ((configuration (acp--session-configuration server params))
         (mode (acp--initial-mode configuration))
         (application
          (application-create configuration :permission-mode mode))
         (identifier
          (conversation-identifier (application-conversation application)))
         (session
          (make-instance 'acp-session
                         :identifier (acp--session-identifier identifier)
                         :server server
                         :application application
                         :mode mode)))
    (acp-server-register server session)
    (json-object "sessionId" (acp-session-identifier session)
                 "modes" (acp--modes-object mode))))

(-> acp--handle-session-load (acp-server json-object) json-object)
(defun acp--handle-session-load (server params)
  "Load one existing conversation and replay it to the client."
  (let ((session-id (json-get params "sessionId")))
    (unless (stringp session-id)
      (error 'acp-method-error
             :code *acp-invalid-params-code*
             :message "The session/load sessionId field is required."))
    (let ((conversation-id
            (acp--session-conversation-identifier session-id)))
      (unless conversation-id
        (error 'acp-method-error
               :code *acp-invalid-params-code*
               :message (format nil "Session ~A is unknown." session-id)))
      (handler-case
          (let* ((configuration (acp--session-configuration server params))
                 (mode (acp--initial-mode configuration))
                 (application
                  (application-create configuration
                                      :conversation-id conversation-id
                                      :permission-mode mode))
                 (session
                  (make-instance 'acp-session
                                 :identifier session-id
                                 :server server
                                 :application application
                                 :mode mode)))
            (acp-server-register server session)
            (acp--replay-session session)
            (json-object "modes" (acp--modes-object mode)))
        (conversation-error (condition)
          (error 'acp-method-error
                 :code *acp-invalid-params-code*
                 :message (format nil "Session ~A could not be loaded: ~A"
                                  session-id
                                  (autolith-error-message condition))))))))

;;;; -- Prompt Mapping --

(-> acp--resolved-session (acp-server json-object) acp-session)
(defun acp--resolved-session (server params)
  "Return the registered session named by PARAMS's sessionId field."
  (let ((session-id (json-get params "sessionId")))
    (unless (stringp session-id)
      (error 'acp-method-error
             :code *acp-invalid-params-code*
             :message "The request sessionId field is required."))
    (or (acp-server-session server session-id)
        (error 'acp-method-error
               :code *acp-invalid-params-code*
               :message (format nil "Session ~A is unknown." session-id)))))

(-> acp--content-block-text (t) (option string))
(defun acp--content-block-text (block)
  "Return a textual representation of one ACP prompt content BLOCK, or NIL."
  (unless (json-object-p block)
    (error 'acp-method-error
           :code *acp-invalid-params-code*
           :message "Each session/prompt content block must be an object."))
  (let ((type (json-get block "type")))
    (cond
      ((json-string= type "text")
       (let ((text (json-get block "text")))
         (if (stringp text) text "")))
      ((json-string= type "resource_link")
       (let ((name (json-get block "name"))
             (uri (json-get block "uri"))
             (description (json-get block "description")))
         (format nil "<resource name=\"~A\" uri=\"~A\"~@[ description=\"~A\"~] />"
                 (if (stringp name) name "")
                 (if (stringp uri) uri "")
                 (if (stringp description) description nil))))
      (t
       ;; Unknown block type: preserve it as JSON so the agent can see it.
       (json-encode block)))))

(-> acp--prompt-text (json-object) string)
(defun acp--prompt-text (params)
  "Return the text of PARAMS's prompt content blocks as one string."
  (let ((blocks (json-get params "prompt")))
    (unless (vectorp blocks)
      (error 'acp-method-error
             :code *acp-invalid-params-code*
             :message "The session/prompt prompt field must be an array."))
    (let ((parts nil))
      (loop for block across blocks
            for text = (acp--content-block-text block)
            when text
              do (push text parts))
      (if parts
          (format nil "~{~A~^~%~}" (nreverse parts))
          ""))))


(-> acp--stop-reason (provider-result) string)
(defun acp--stop-reason (result)
  "Return the ACP stopReason for RESULT."
  (ecase (provider-result-turn-completion result)
    ((:continue)
     "end_turn")
    ((:end :unspecified)
     "end_turn")))

(-> acp--handle-session-prompt (acp-server json-object) json-object)
(defun acp--handle-session-prompt (server params)
  "Run one user turn for SESSION and stream its updates to the client.
Interrupts the turn at observer boundaries and reports stopReason
cancelled when the client sent session/cancel while it ran."
    (let* ((session (acp--resolved-session server params))
         (text (acp--prompt-text params)))
    (acp--reset-session-turn session)
    (acp--reset-reasoning session)
    (handler-case
        (let ((result
                (agent-run-user-turn
                 (application-agent (acp-session-application session))
                 text
                 :observer (acp--session-observation session))))
          (acp--flush-reasoning session)
          (json-object "stopReason" (acp--stop-reason result)))
      (acp-turn-cancelled ()
        (acp--flush-reasoning session)
        (json-object "stopReason" "cancelled")))))

(-> acp--handle-session-set-mode (acp-server json-object) json-object)
(defun acp--handle-session-set-mode (server params)
  "Change SESSION's command permission mode and report the new mode."
  (let* ((session (acp--resolved-session server params))
         (mode-id (json-get params "modeId")))
    (unless (stringp mode-id)
      (error 'acp-method-error
             :code *acp-invalid-params-code*
             :message "The session/set_mode modeId field is required."))
    (let ((mode (acp--mode-from-id mode-id)))
      (unless mode
        (error 'acp-method-error
               :code *acp-invalid-params-code*
               :message (format nil "Mode ~A is unknown." mode-id)))
      (setf (application-permission-mode (acp-session-application session)) mode)
      (setf (acp-session-mode session) mode)
      (acp--current-mode-update session)
      (json-object))))

(-> acp--handle-authenticate (acp-server json-object) json-object)
(defun acp--handle-authenticate (server params)
  "Reply to authenticate with an empty result."
  (declare (ignore server params))
  (json-object))

(-> acp--handle-logout (acp-server json-object) json-object)
(defun acp--handle-logout (server params)
  "Reply to logout with an empty result."
  (declare (ignore server params))
  (json-object))

(-> acp-server-unregister (acp-server string) null)
(defun acp-server-unregister (server session-id)
  "Remove SESSION-ID from SERVER's session table."
  (with-lock-held ((acp-server-lock server))
    (remhash session-id (acp-server-sessions server)))
  nil)

(-> acp--handle-session-close (acp-server json-object) json-object)
(defun acp--handle-session-close (server params)
  "Close SESSION and remove it from the server's table."
  (let ((session (acp--resolved-session server params)))
    (acp-server-unregister server (acp-session-identifier session))
    (json-object)))

(-> acp--handle-session-delete (acp-server json-object) json-object)
(defun acp--handle-session-delete (server params)
  "Delete SESSION's durable conversation and remove it from the server."
  (let* ((session (acp--resolved-session server params))
         (configuration (application-configuration (acp-session-application session)))
         (conversation-id (acp--session-conversation-identifier
                           (acp-session-identifier session))))
    (conversation-delete configuration conversation-id)
    (acp-server-unregister server (acp-session-identifier session))
    (json-object)))

(-> acp--handle-session-list (acp-server json-object) json-value)
(defun acp--handle-session-list (server params)
  "Return the active ACP sessions."
  (declare (ignore params))
  (apply #'json-array
         (loop for session-id being the hash-keys of (acp-server-sessions server)
               collect (json-object "sessionId" session-id))))

(-> acp--handle-session-cancel (acp-server json-object) null)
(defun acp--handle-session-cancel (server params)
  "Record the client's request to interrupt SESSION's running prompt.
The running turn checks the flag at its observer boundaries and unwinds,
so a provider wait in progress ends first."
  (let ((session (acp--resolved-session server params)))
    (acp--cancel-session-turn session)
    (acp--log "The client cancelled the turn for session ~A."
              (acp-session-identifier session)))
  nil)

;;;; -- Method Dispatch --

(defparameter *acp-request-handlers*
  (list (cons "initialize" #'acp--handle-initialize)
        (cons "authenticate" #'acp--handle-authenticate)
        (cons "logout" #'acp--handle-logout)
        (cons "session/new" #'acp--handle-session-new)
        (cons "session/load" #'acp--handle-session-load)
        (cons "session/list" #'acp--handle-session-list)
        (cons "session/delete" #'acp--handle-session-delete)
        (cons "session/close" #'acp--handle-session-close)
        (cons "session/set_mode" #'acp--handle-session-set-mode)
        (cons "session/prompt" #'acp--handle-session-prompt))
  "The client-to-agent request handlers by method name.")

(defparameter *acp-notification-handlers*
  (list (cons "session/cancel" #'acp--handle-session-cancel))
  "The client-to-agent notification handlers by method name.")

(-> acp--dispatch-request (acp-server string json-object) json-value)
(defun acp--dispatch-request (server method params)
  "Run METHOD's request handler on SERVER and return its JSON result."
  (let ((handler (cdr (assoc method *acp-request-handlers* :test #'string=))))
    (unless handler
      (error 'acp-method-error
             :code *acp-method-not-found-code*
             :message (format nil "Method ~A is not available." method)))
    (funcall handler server (if (json-object-p params) params (json-object)))))

(-> acp--dispatch-notification (acp-server string json-object) null)
(defun acp--dispatch-notification (server method params)
  "Run METHOD's notification handler on SERVER when one is registered."
  (let ((handler (cdr (assoc method *acp-notification-handlers*
                             :test #'string=))))
    (if handler
        (funcall handler server (if (json-object-p params) params (json-object)))
        (acp--log "Ignored unknown ACP notification ~A." method)))
  nil)

;;;; -- Server Entry --

(-> acp-serve
    (&key (:configuration configuration)
          (:input-stream stream)
          (:output-stream stream))
    null)

(defun acp-serve
    (&key ((:configuration configuration)
           (configuration-create :defer-provider-validation-p t))
          ((:input-stream input-stream) *standard-input*)
          ((:output-stream output-stream) *standard-output*))
  "Run one ACP agent server over INPUT-STREAM and OUTPUT-STREAM."
  (let* ((server nil)
         (connection
           (acp-connection-create
            :input-stream input-stream
            :output-stream output-stream
            :request-dispatcher
            (lambda (method params)
              (acp--dispatch-request server method params))
            :notification-dispatcher
            (lambda (method params)
              (acp--dispatch-notification server method params)))))
    (setf server
          (make-instance 'acp-server
                         :connection connection
                         :configuration configuration))
    (acp-connection-start connection)
    (acp-connection-join connection)
    (acp--log "The ACP agent server stopped.")
    nil))
