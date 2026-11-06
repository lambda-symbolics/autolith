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
   (sources
    :initform      nil
    :accessor      acp-session-sources
    :type          list
    :documentation "The sources of messages this session submitted and the channel has not started yet, oldest first.")
   (own-prompts
    :initform      nil
    :accessor      acp-session-own-prompts
    :type          list
    :documentation "Texts of prompts the editor already shows, so their records are not shown twice.")
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
  (let ((connection (acp-session-connection session)))
    (unless (and connection (not (acp-session-closed-p session)))
      (error 'agentcomms:acp-state-error :message "The session's channel is gone."))
    (handler-case (apply #'localgroup-client-call connection command arguments)
      (localgroup-error (condition)
        (error 'agentcomms:acp-state-error :message (autolith-error-message condition))))))

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
  "Leave the channel, and close it when the server started it for this session and nobody else follows it."
  (let ((connection nil))
    (with-lock-held ((acp-session-lock session))
      (unless (acp-session-closed-p session)
        (setf (acp-session-closed-p session) t
              connection                     (acp-session-connection session)))
      (sb-thread:condition-broadcast (acp-session-condition-variable session)))
    (when connection
      (localgroup-client-close connection)
      (when (acp-session-started-p session)
        (let* ((configuration (acp-service-configuration (acp-session-service session)))
               (world         (world-client-entry configuration)))
          (when world
            (handler-case
                (localgroup-query-record world ':close
                                         (list :channel           (acp-session-identifier session)
                                               :unattached-only-p t))
              (localgroup-error ()
                nil)))))))
  nil)

(define-condition acp-operation-busy (agentcomms:acp-method-error)
  ()
  (:default-initargs :code -32603 :message "Another primary operation is active.")
  (:documentation "The channel works on something that does not admit this operation now."))
