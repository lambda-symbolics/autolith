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
    (acp-service &key (:cwd string) (:identifier (option string)) (:mcp-servers t)) acp-session)
(defun acp-service--open-session (service &key cwd identifier mcp-servers)
  "Open the channel of IDENTIFIER, or a new one for CWD, in the world server and return its session.

The editor's MCP-SERVERS join the tools of a channel the server starts for
this session. A loaded session replays its history before it follows the
channel's live events, so the two never interleave."
  (when (>= (hash-table-count (acp-service-sessions service)) *acp-session-maximum-count*)
    (error 'agentcomms:acp-state-error :message "The live session limit is reached."))
  (when (and identifier (gethash identifier (acp-service-sessions service)))
    (agentcomms:acp-invalid-params "Session ~A is already open." identifier))
  (agentcomms:acp-validate-absolute-path cwd "cwd")
  (unless (uiop:directory-exists-p cwd)
    (agentcomms:acp-invalid-params "The working directory does not exist: ~A." cwd))
  (let* ((directory     (platform-truename *platform* (uiop:ensure-directory-pathname cwd)))
         (configuration (configuration-copy (acp-service-configuration service)
                                            :working-directory directory))
         (servers       (and mcp-servers (coerce mcp-servers 'list))))
    (multiple-value-bind (channel started-p)
        (handler-case
            (world-client-open configuration
                               :directory       directory
                               :channel         identifier
                               :new-p           (null identifier)
                               :permission-mode (acp-service-permission-mode service)
                               :pristine-p      (acp-service-pristine-p service)
                               :mcp-servers     (and servers (client-portable servers)))
          (localgroup-error (condition)
            (error 'agentcomms:acp-state-error :message (autolith-error-message condition))))
      (let ((session     (make-instance 'acp-session
                                        :service       service
                                        :identifier    channel
                                        :configuration configuration
                                        :started-p     started-p
                                        :mode          (acp-service-permission-mode service)))
            (completed-p nil))
        (setf (acp-session-update-buffer session)
              (agentcomms:make-agent-update-buffer service channel))
        (unwind-protect
             (progn
               (when (and identifier (not (equal identifier channel)))
                 (agentcomms:acp-invalid-params "Use the canonical conversation identifier."))
               (when identifier
                 (acp-session-replay session))
               (let ((connection (localgroup-client-connect
                                  (localgroup--find-record configuration channel)
                                  :name           "acp"
                                  :event-function (lambda (event)
                                                    (acp-session-event session event)))))
                 (setf (acp-session-connection session) connection)
                 (let ((mode (getf (rest (getf (rest (localgroup-client-connection-attached connection)) :state))
                                   :permission-mode)))
                   (when (keywordp mode)
                     (setf (acp-session-mode session) mode))))
               (with-lock-held ((acp-service-lock service))
                 (when (acp-service-closed-p service)
                   (error 'agentcomms:acp-connection-closed))
                 (setf (gethash channel (acp-service-sessions service)) session
                       completed-p                                      t))
               session)
          (unless completed-p
            (acp-session-close session)))))))

(defmethod agentcomms:agent-new-session
    ((service acp-service) &key cwd mcp-servers additional-directories params)
  "Open a new channel for CWD, with the editor's MCP servers among its tools."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :mcp-servers mcp-servers)))
    (values (acp-session-identifier session) (acp-service--modes session))))

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
            (let ((name      (function-call-canonical-name item))
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
           :status     (if (member (getf fields :status) '(:ok :success)) ':completed ':failed)
           :raw-output output
           :content    (list (agentcomms:acp-tool-call-content
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
  "Replay the channel's durable messages, reasoning, calls and results, then its plan."
  (let ((conversation (conversation-replay-load (acp-session-configuration session)
                                                (acp-session-identifier session))))
    (with-lock-held ((acp-session-lock session))
      (acp-replay-map-updates conversation
                              (lambda (update) (acp-session--send session update)))
      (acp-session--plan session)))
  (acp-session-flush session)
  nil)

(defmethod agentcomms:agent-load-session
    ((service acp-service) &key session-id cwd mcp-servers additional-directories params)
  "Open the channel SESSION-ID and replay its history before answering."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (acp-service--modes
   (acp-service--open-session service :cwd cwd :identifier session-id :mcp-servers mcp-servers)))

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
                    (text     (agentcomms:json-get resource "text")))
               (unless (stringp text)
                 (agentcomms:acp-invalid-params "Binary embedded context is not supported."))
               (format stream "Resource ~A:~%~A" (agentcomms:json-get resource "uri") text)))
            (otherwise
             (agentcomms:acp-invalid-params "Unsupported prompt content: ~A." type))))))

(-> acp-session-prompt (acp-session string) keyword)
(defun acp-session-prompt (session text)
  "Submit the editor's prompt TEXT to the channel and return its stop reason once the channel is idle.

A turn the editor cancelled stops with :CANCELLED; a turn that failed signals
the failure to the editor."
  (let ((mark nil))
    (with-lock-held ((acp-session-lock session))
      (when (acp-session-closed-p session)
        (error 'agentcomms:acp-state-error :message "The session is closed."))
      (when (acp-session-prompt-active-p session)
        (error 'agentcomms:acp-state-error :message "A prompt is already running in this session."))
      (setf (acp-session-prompt-active-p session) t
            (acp-session-cancelled-p session)     nil
            (acp-session-last-outcome session)    nil
            (acp-session-own-prompts session)     (append (acp-session-own-prompts session) (list text))
            (acp-session-sources session)         (append (acp-session-sources session) (list "prompt"))
            mark                                  (acp-session-idle-count session)))
    (unwind-protect
         (let ((busy-p (getf (acp-session-call session ':submit :text text) :busy-p)))
           (with-lock-held ((acp-session-lock session))
             (loop until (or (acp-session-closed-p session)
                             (not busy-p)
                             (> (acp-session-idle-count session) mark))
                   do (condition-wait (acp-session-condition-variable session)
                                      (acp-session-lock session))))
           (acp-session-flush session)
           (let ((outcome (with-lock-held ((acp-session-lock session))
                            (cond
                              ((acp-session-cancelled-p session)
                               ':cancelled)
                              ((and (acp-session-closed-p session)
                                    (= (acp-session-idle-count session) mark))
                               (error 'agentcomms:acp-state-error
                                      :message "The channel went away before the turn finished."))
                              (t
                               (or (acp-session-last-outcome session) ':end-turn))))))
             (when (eq outcome ':condition)
               (error 'agentcomms:acp-method-error
                      :code    -32603
                      :message "The turn failed; the channel's transcript has its report."))
             outcome))
      (with-lock-held ((acp-session-lock session))
        (setf (acp-session-prompt-active-p session) nil
              (acp-session-sources session)       (remove "prompt" (acp-session-sources session)
                                                          :test #'equal :count 1))))))

(defmethod agentcomms:agent-prompt ((service acp-service) session-id prompt &key params)
  "Run one editor prompt in the session's channel."
  (declare (ignore params))
  (acp-session-prompt (acp-service--session service session-id) (acp-prompt->text prompt)))

(defmethod agentcomms:agent-cancel ((service acp-service) session-id)
  "Cancel the turn the session's channel runs."
  (acp-session-cancel (acp-service--session service session-id)))

(defmethod agentcomms:agent-set-mode ((service acp-service) session-id mode-id &key params)
  "Set the channel's permission mode and tell the editor."
  (declare (ignore params))
  (let* ((session (acp-service--session service session-id))
         (mode    (find mode-id '(:ask :auto :sandboxed :full-access)
                        :key (lambda (value) (string-downcase (symbol-name value))) :test #'equal)))
    (unless mode (agentcomms:acp-invalid-params "Unknown mode ~A." mode-id))
    (acp-session-call session ':mode :value mode)
    (with-lock-held ((acp-session-lock session))
      (setf (acp-session-mode session) mode))
    (agentcomms:agent-send-update service session-id (agentcomms:acp-update-current-mode mode-id)))
  nil)

(defmethod agentcomms:agent-close-session ((service acp-service) session-id params)
  "Leave the session's channel."
  (declare (ignore params))
  (acp-session-close (acp-service--session service session-id))
  (with-lock-held ((acp-service-lock service))
    (remhash session-id (acp-service-sessions service)))
  nil)

(defmethod agentcomms:peer-connection-closed :before ((service acp-service) connection reason)
  "Stop admission and cancel the turns the editor's prompts started."
  (declare (ignore connection reason))
  (let ((sessions nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session)))
    (dolist (session sessions)
      (when (acp-session-prompt-active-p session)
        (acp-session-cancel session)))))

(defmethod agentcomms:agent-extension-request ((service acp-service) method params)
  "Run the editor's extension METHOD: in the ACP process, or in the session's channel.

A request for a missing, malformed or foreign session, or one that fails on
its way, answers with a typed :CONDITION outcome; a closed service refuses."
  (let ((entry (assoc method *acp-extension-methods* :test #'string=)))
    (cond
      ((string= method "_autolith/capabilities")
       (acp-extension-result ':ok :value (acp-extension-capabilities)))
      ((null entry)
       (call-next-method))
      ((acp-service-closed-p service)
       (error 'agentcomms:acp-connection-closed))
      (t
       (handler-case
           (let ((session (acp-service--session
                           service (agentcomms:acp-field params "sessionId" :type ':string :required-p t))))
             (if (eq (third entry) ':client)
                 (handler-case (acp-extension-result ':ok :value (funcall (second entry) session params))
                   (acp-extension-unavailable (condition)
                     (acp-extension-result ':unsupported :report (acp-extension-unavailable-reason condition))))
                 (let ((outcome (getf (acp-session-call session ':operation
                                                        :method method
                                                        :params (client-portable params))
                                      :result)))
                   (acp-extension-result (getf outcome :outcome)
                                         :value          (client-json (getf outcome :value))
                                         :report         (getf outcome :report)
                                         :condition-type (getf outcome :condition-type)))))
         (error (condition)
           (acp-extension-result ':condition
                                 :report         (princ-to-string condition)
                                 :condition-type (string-downcase (symbol-name (type-of condition))))))))))

(defmethod agentcomms:peer-handle-request :around ((service acp-service) connection method &key params)
  "Keep diagnostics off the protocol stream in every inbound handler thread."
  (declare (ignore params))
  (let* ((*standard-output* *error-output*) (*trace-output* *error-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io*    (make-two-way-stream *standard-input* *error-output*))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*))
    (handler-case (call-next-method)
      (authentication-error ()
        (error 'agentcomms:acp-method-error :code -32000
               :message                           "Provider authentication required. Configure credentials before starting ACP.")))))

(-> acp-service-close (acp-service) null)
(defun acp-service-close (service)
  "Stop admission and leave every session's channel."
  (let ((sessions nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session))
      (clrhash (acp-service-sessions service)))
    (mapc #'acp-session-close sessions))
  nil)

(-> acp-service-serve (acp-service agentcomms:acp-channel) null)
(defun acp-service-serve (service channel)
  "Own SERVICE and its channel through disconnect and session cleanup."
  (unwind-protect
       (agentcomms:acp-agent-serve service channel :name "Autolith ACP"
                                   :log-function         #'agentcomms:acp-standard-error-log)
    (unwind-protect (acp-service-close service)
      (agentcomms:channel-close channel)))
  nil)

(-> acp-run (configuration &key (:permission-mode keyword) (:pristine-p boolean)
                                (:endpoint (or null acp-endpoint)))
    null)
(defun acp-run (configuration &key (permission-mode ':ask) pristine-p endpoint)
  "Serve editors as clients of the world server until they leave or Ctrl-C.

Without ENDPOINT, the one editor speaks on UTF-8 standard I/O. With ENDPOINT,
remote editors connect over the network. Every editor gets its own service,
and every session is a channel of the world server, which this process
starts when none runs; PRISTINE-P starts it pristine."
  (let* ((channel           (and (null endpoint) (agentcomms:acp-standard-io-channel)))
         (*standard-output* *error-output*)
         (*trace-output*    *error-output*)
         (*standard-input*  (make-string-input-stream ""))
         (*terminal-io*     (make-two-way-stream *standard-input* *error-output*))
         (*query-io*        *terminal-io*)
         (*debug-io*        *terminal-io*))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (flet ((serve (client-channel)
                    ;; Client declarations can contain credentials for the client's lifetime.
                    (call-with-secret-use
                     (lambda ()
                       (acp-service-serve (make-instance 'acp-service :configuration   configuration
                                                                      :permission-mode permission-mode
                                                                      :pristine-p      pristine-p)
                                          client-channel)))))
             (if endpoint
                 (acp-serve-remote endpoint #'serve)
                 (serve channel))))
      (when channel
        (agentcomms:channel-close channel))))
  nil)
