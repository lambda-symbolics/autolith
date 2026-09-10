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
    :documentation "The current command permission mode keyword."))
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

;;;; -- Method Dispatch --

(defparameter *acp-request-handlers*
  (list (cons "session/new" #'acp--handle-session-new)
        (cons "session/load" #'acp--handle-session-load))
  "The client-to-agent request handlers by method name.")

(defparameter *acp-notification-handlers*
  nil
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
