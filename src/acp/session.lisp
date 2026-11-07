(in-package #:autolith)

;;;; -- ACP Sessions as Channel Clients --

;;; An ACP session is a structured client of one channel in the world server.
;;; The channel runs the agent, its tools, its jobs and its approvals; the
;;; session turns the channel's events into ACP updates for the editor and
;;; the editor's requests into the channel's commands.

(defparameter *acp-session-maximum-count* 32
  "Maximum live editor sessions in one ACP process.")

(defclass acp-service (agentcomms:acp-agent)
  ((configuration
    :initarg       :configuration
    :accessor      acp-service-configuration
    :type          configuration
    :documentation "The configuration locating the world server and its data.")
   (permission-mode
    :initarg       :permission-mode
    :initform      ':ask
    :reader        acp-service-permission-mode
    :type          (member :ask :auto :sandboxed :full-access)
    :documentation "The permission mode a session asks for when it opens its channel.")
   (pristine-p
    :initarg       :pristine-p
    :initform      nil
    :reader        acp-service-pristine-p
    :type          boolean
    :documentation "Whether a world server this process starts runs pristine.")
   (sessions
    :initform      (make-hash-table :test #'equal)
    :reader        acp-service-sessions
    :documentation "Live sessions keyed by channel identifier.")
   (lock
    :initform      (make-lock "Autolith ACP service")
    :reader        acp-service-lock
    :documentation "The lock protecting session admission.")
   (open-lock
    :initform      (make-lock "Autolith ACP session setup")
    :reader        acp-service-open-lock
    :documentation "Serializes setup through the protocol library's registration.")
   (closed-p
    :initform      nil
    :accessor      acp-service-closed-p
    :type          boolean
    :documentation "Whether disconnect has stopped further session admission."))
  (:documentation "An ACP endpoint whose sessions are channels of the world server."))

(defclass acp-session ()
  ((service
    :initarg       :service
    :reader        acp-session-service
    :type          acp-service
    :documentation "The endpoint owning this session.")
   (identifier
    :initarg       :identifier
    :reader        acp-session-identifier
    :type          string
    :documentation "The channel identifier, which is also the conversation identifier.")
   (configuration
    :initarg       :configuration
    :reader        acp-session-configuration
    :type          configuration
    :documentation "The configuration of the session's working directory, for reading its plan and history.")
   (connection
    :initform      nil
    :accessor      acp-session-connection
    :documentation "The structured client connection to the channel.")
   (reconnect-id
    :initform      nil
    :accessor      acp-session-reconnect-id
    :type          (option string)
    :documentation "The authenticated checkpoint transition awaiting a replacement endpoint.")
   (reconnect-thread
    :initform      nil
    :accessor      acp-session-reconnect-thread
    :documentation "The worker reconnecting this session, or NIL.")
   (reconnect-failure
    :initform      nil
    :accessor      acp-session-reconnect-failure
    :type          (option string)
    :documentation "The reported reason a planned reconnect failed, or NIL.")
   (started-p
    :initarg       :started-p
    :initform      nil
    :reader        acp-session-started-p
    :type          boolean
    :documentation "Whether the world server started the channel for this session.")
   (mode
    :initarg       :mode
    :initform      ':ask
    :accessor      acp-session-mode
    :documentation "The channel's permission mode as last reported.")
   (lock
    :initform      (make-lock "Autolith ACP session")
    :reader        acp-session-lock
    :documentation "The lock protecting prompt, turn and translation state.")
   (event-lock
    :initform      (make-lock "Autolith ACP event delivery")
    :reader        acp-session-event-lock
    :documentation "Serializes replay, buffered attachment events and live delivery.")
   (attaching-p
    :initform      nil
    :accessor      acp-session-attaching-p
    :documentation "Whether incoming events wait for the initial history replay.")
   (attachment-events
    :initform      (make-deque)
    :reader        acp-session-attachment-events
    :documentation "Events received after subscription while initial history is replayed.")
   (event-sequence
    :initform      0
    :accessor      acp-session-event-sequence
    :documentation "The cursor of the last completely processed channel event.")
   (replay-boundary
    :initform      0
    :accessor      acp-session-replay-boundary
    :documentation "The durable record watermark already delivered by initial replay.")
   (catch-up-p
    :initform      nil
    :accessor      acp-session-catch-up-p
    :documentation "Whether an already running provider response uses durable record projection until its next request.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith ACP session")
    :reader        acp-session-condition-variable
    :documentation "Signaled when the channel finishes work or the connection ends.")
   (prompt-active-p
    :initform      nil
    :accessor      acp-session-prompt-active-p
    :type          boolean
    :documentation "Whether an editor prompt waits for the channel.")
   (cancelled-p
    :initform      nil
    :accessor      acp-session-cancelled-p
    :type          boolean
    :documentation "Whether the editor cancelled the current prompt.")
   (prompt-submission
    :initform      nil
    :accessor      acp-session-prompt-submission
    :documentation "The exact submission awaited by the editor's active prompt.")
   (prompt-outcome
    :initform      nil
    :accessor      acp-session-prompt-outcome
    :documentation "The terminal outcome of the active prompt's submission.")
   (idle-count
    :initform      0
    :accessor      acp-session-idle-count
    :type          (integer 0)
    :documentation "How many times the channel finished its work and went idle.")
   (last-outcome
    :initform      nil
    :accessor      acp-session-last-outcome
    :documentation "The outcome of the channel's last finished work.")
   (closed-p
    :initform      nil
    :accessor      acp-session-closed-p
    :type          boolean
    :documentation "Whether the session closed or lost its channel.")
   (epoch
    :initform      0
    :accessor      acp-session-epoch
    :type          integer
    :documentation "The generation of the latest turn, numbering turn events.")
   (turn
    :initform      nil
    :accessor      acp-session-turn
    :documentation "The running turn as (EPOCH . SOURCE), or NIL.")
   (client-identifier
    :initform      (make-identifier)
    :reader        acp-session-client-identifier
    :type          string
    :documentation "The submitting editor identity, distinct from other clients of this channel.")
   (turn-sequence
    :initform      0
    :accessor      acp-session-turn-sequence
    :type          integer
    :documentation "The durable user-message sequence qualifying tool identifiers.")
   (streamed-text
    :initform      (text-buffer-create)
    :accessor      acp-session-streamed-text
    :type          string
    :documentation "Text already sent for the current provider response.")
   (thought-state
    :initform      nil
    :accessor      acp-session-thought-state
    :type          (member nil :streaming :boundary)
    :documentation "Whether contiguous thoughts continue or cross a provider response boundary.")
   (update-buffer
    :initform      nil
    :accessor      acp-session-update-buffer
    :documentation "The ordered buffer coalescing visible thought fragments.")
   (tool-call
    :initform      nil
    :accessor      acp-session-tool-call
    :type          (option string)
    :documentation "The latest tool call the channel started, which an approval belongs to.")
   (extension-lock
    :initform      (make-lock "Autolith ACP extension events")
    :reader        acp-session-extension-lock
    :documentation "Orders extension sequence allocation and wire notification writes.")
   (extension-sequence
    :initform      0
    :accessor      acp-session-extension-sequence
    :type          integer
    :documentation "The last extension notification sequence."))
  (:documentation "One editor session following one channel of the world server."))


;;;; -- Lookup --

(-> acp-service--session (acp-service string) acp-session)
(defun acp-service--session (service identifier)
  "Return the live session IDENTIFIER, or signal Invalid Params."
  (with-lock-held ((acp-service-lock service))
    (or (gethash identifier (acp-service-sessions service))
        (agentcomms:acp-invalid-params "Unknown session ~A." identifier))))


;;;; -- Commands --

(-> acp-session-call (acp-session keyword &rest t) list)
(defun acp-session-call (session command &rest arguments)
  "Send COMMAND with ARGUMENTS to SESSION's channel and return the reply's fields."
  (let ((connection (acp-session--await-connection session)))
    (handler-case (apply #'localgroup-client-call connection command arguments)
      (localgroup-request-interrupted (condition)
        (error 'acp-request-interrupted
               :message (autolith-error-message condition)
               :data    (agentcomms:json-object
                         "outcome" "interrupted"
                         "executionOutcome" "unknown"
                         "requestId" (localgroup-request-interrupted-identifier condition)
                         "command" (string-downcase
                                    (symbol-name (localgroup-request-interrupted-command condition)))
                         "reconnectId" (localgroup-request-interrupted-reconnect-id condition))))
      (localgroup-error (condition)
        (error 'agentcomms:acp-state-error :message (autolith-error-message condition))))))

(-> acp-session-submit
    (acp-session string &key (:source string) (:steer-p boolean) (:submission-id string)) list)
(defun acp-session-submit (session text &key (source "prompt") steer-p (submission-id (make-identifier)))
  "Submit TEXT with its editor identity and turn SOURCE, returning the channel reply."
  (acp-session-call session ':submit :text text :steer-p steer-p
                    :submission (list :id     submission-id
                                      :client (acp-session-client-identifier session)
                                      :source source)))

(-> acp-session-cancel (acp-session) null)
(defun acp-session-cancel (session)
  "Ask the channel to cancel its running turn for the editor."
  (with-lock-held ((acp-session-lock session))
    (setf (acp-session-cancelled-p session) t))
  (unless (acp-session-closed-p session)
    (handler-case (acp-session-call session ':cancel)
      (agentcomms:acp-state-error ()
        nil)))
  nil)

(-> acp-session-close (acp-session) null)
(defun acp-session-close (session)
  "Leave the channel and cancel any planned reconnect before releasing its registration."
  (let ((connection nil)
        (reconnect  nil))
    (with-lock-held ((acp-session-lock session))
      (setf (acp-session-closed-p session) t
            (acp-session-reconnect-id session) nil
            connection (acp-session-connection session)
            reconnect (acp-session-reconnect-thread session))
      (sb-thread:condition-broadcast (acp-session-condition-variable session)))
    (acp-session--forget session)
    (when connection
      (localgroup-client-close connection))
    (when (and reconnect (not (eq reconnect (current-thread))))
      (join-thread reconnect))
    (when (and connection (acp-session-started-p session))
      (let* ((configuration (acp-service-configuration (acp-session-service session)))
             (world         (world-client-entry configuration)))
        (when world
          (handler-case
              (localgroup-query-record world ':close
                                       (list :channel           (acp-session-identifier session)
                                             :unattached-only-p t))
            (localgroup-error ()
              nil))))))
  nil)

(define-condition acp-operation-busy (agentcomms:acp-method-error)
  ()
  (:default-initargs :code -32603 :message "Another primary operation is active.")
  (:documentation "The channel works on something that does not admit this operation now."))

(define-condition acp-request-interrupted (agentcomms:acp-state-error agentcomms:acp-method-error)
  ()
  (:default-initargs :code -32603)
  (:documentation "A channel request lost its reply; DATA identifies its unknown execution outcome."))
