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
this session. A loaded session subscribes before replay and drains the
subsequent events after its bounded history snapshot."
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
               (acp-session-begin-attachment session)
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
               (acp-session-finish-attachment
                session (localgroup-client-connection-attached (acp-session-connection session))
                :replay-p (not (null identifier)))
               (with-lock-held ((acp-service-lock service))
                 (when (or (acp-service-closed-p service) (acp-session-closed-p session))
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
      (:side-status
       (agentcomms:acp-update-agent-message
        (agentcomms:acp-text-content
         (format nil "~{~A~}" (mapcar #'terminal-span-text (getf fields :spans))))))
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

(-> acp-replay-map-updates
    (conversation function &key (:through-sequence (option integer))) (values null integer))
(defun acp-replay-map-updates (conversation function &key through-sequence)
  "Call FUNCTION on updates through the durable watermark and return NIL and the last turn sequence."
  (let ((turn-sequence 0))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (or (null through-sequence) (<= (getf (rest record) :seq) through-sequence))
         (when (and (eq (first record) ':message)
                    (eq (getf (rest record) :role) ':user))
           (setf turn-sequence (getf (rest record) :seq)))
         (let ((update (acp-replay--record-update record turn-sequence)))
           (when update (funcall function update))))))
    (values nil turn-sequence)))

(-> acp-session-replay (acp-session &key (:through-sequence (option integer))) null)
(defun acp-session-replay (session &key through-sequence)
  "Replay durable history through THROUGH-SEQUENCE, then the channel's current plan."
  (let ((conversation (conversation-replay-load (acp-session-configuration session)
                                                (acp-session-identifier session))))
    (with-lock-held ((acp-session-lock session))
      (setf (acp-session-turn-sequence session)
            (nth-value 1 (acp-replay-map-updates
                          conversation (lambda (update) (acp-session--send session update))
                          :through-sequence through-sequence)))
      (acp-session--plan session)))
  (acp-session-flush session)
  nil)

(-> acp-session-begin-attachment (acp-session) integer)
(defun acp-session-begin-attachment (session)
  "Buffer incoming callbacks until the connection is installed; return its processed cursor."
  (with-lock-held ((acp-session-event-lock session))
    (setf (acp-session-attaching-p session) t)
    (acp-session-event-sequence session)))

(-> acp-session-drain-events (acp-session) null)
(defun acp-session-drain-events (session)
  "Deliver buffered callbacks in order before admitting live delivery."
  (with-lock-held ((acp-session-event-lock session))
    (loop until (deque-empty-p (acp-session-attachment-events session))
          do (acp-session--consume-event session (deque-pop-front (acp-session-attachment-events session))))
    (setf (acp-session-attaching-p session) nil))
  nil)

(-> acp-session-finish-attachment (acp-session list &key (:replay-p boolean)) null)
(defun acp-session-finish-attachment (session greeting &key replay-p)
  "Replay through GREETING's watermark, then deliver buffered later events before going live."
  (let ((sequence (getf (rest greeting) :sequence))
        (boundary (getf (rest greeting) :record-sequence)))
    (unless (and (integerp sequence) (integerp boundary))
      (error 'agentcomms:acp-state-error :message "The channel did not provide an attachment cursor."))
    (setf (acp-session-replay-boundary session) boundary
          (acp-session-catch-up-p session) t)
    (when replay-p
      (acp-session-replay session :through-sequence boundary))
    (with-lock-held ((acp-session-event-lock session))
      (setf (acp-session-event-sequence session) sequence))
    (acp-session-drain-events session))
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
  "Submit TEXT and await this submission's terminal outcome, independently of other channel work."
  (let ((submission-id (make-identifier)))
    (with-lock-held ((acp-session-lock session))
      (when (acp-session-closed-p session)
        (error 'agentcomms:acp-state-error :message "The session is closed."))
      (when (acp-session-prompt-active-p session)
        (error 'agentcomms:acp-state-error :message "A prompt is already running in this session."))
      (setf (acp-session-prompt-active-p session) t
            (acp-session-cancelled-p session) nil
            (acp-session-prompt-submission session) submission-id
            (acp-session-prompt-outcome session) nil))
    (unwind-protect
         (progn
           (acp-session-submit session text :submission-id submission-id)
           (let ((outcome
                   (with-lock-held ((acp-session-lock session))
                     (loop until (or (acp-session-closed-p session)
                                     (acp-session-prompt-outcome session))
                           do (condition-wait (acp-session-condition-variable session)
                                              (acp-session-lock session)))
                     (or (acp-session-prompt-outcome session)
                         (error 'agentcomms:acp-state-error
                                :message "The channel went away before the submission finished.")))))
             (acp-session-flush session)
             (when (eq outcome ':condition)
               (error 'agentcomms:acp-method-error
                      :code    -32603
                      :message "The turn failed; the channel's transcript has its report."))
             outcome))
      (with-lock-held ((acp-session-lock session))
        (setf (acp-session-prompt-active-p session) nil
              (acp-session-prompt-submission session) nil)))))

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
  "Keep diagnostics off stdout and reconcile transport failure during session setup."
  (let* ((*standard-output* *error-output*) (*trace-output* *error-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io*    (make-two-way-stream *standard-input* *error-output*))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*)
         (opening-p        (member method '("session/new" "session/load") :test #'string=)))
    (flet ((handle-request ()
             (handler-case
                 (let ((response (call-next-method)))
                   (when opening-p
                     (let ((identifier (agentcomms:json-get
                                        (if (string= method "session/new") response params) "sessionId")))
                       (with-lock-held ((acp-service-lock service))
                         (let ((session (gethash identifier (acp-service-sessions service))))
                           (unless (and session (not (acp-session-closed-p session))
                                        (not (acp-service-closed-p service)))
                             ;; Setup may have lost its socket before the library registered it.
                             (agentcomms::agent--forget-session service identifier)
                             (error 'agentcomms:acp-connection-closed))))))
                   response)
               (authentication-error ()
                 (error 'agentcomms:acp-method-error :code -32000
                        :message                           "Provider authentication required. Configure credentials before starting ACP.")))))
      (if opening-p
          (with-lock-held ((acp-service-open-lock service)) (handle-request))
          (handle-request)))))

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

(-> acp-run (configuration &key (:permission-mode keyword) (:endpoint (or null acp-endpoint)))
    null)
(defun acp-run (configuration &key (permission-mode ':ask) endpoint)
  "Serve editors as clients of the world server until they leave or Ctrl-C.

Without ENDPOINT, the one editor speaks on UTF-8 standard I/O. With ENDPOINT,
remote editors connect over the network. Every editor gets its own service,
and every session is a channel of the world server, which this process
starts when none runs."
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
                                                                      :permission-mode permission-mode)
                                          client-channel)))))
             (if endpoint
                 (acp-serve-remote endpoint #'serve)
                 (serve channel))))
      (when channel
        (agentcomms:channel-close channel))))
  nil)
