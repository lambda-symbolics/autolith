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

(defclass acp-session (channel-subscription)
  ((service :initarg :service :reader acp-session-service :type acp-service
            :documentation "The endpoint owning this session.")
   (mode :initarg :mode :initform ':ask :accessor acp-session-mode
         :documentation "The channel's permission mode as last reported.")
   (replay-boundary :initform 0 :accessor acp-session-replay-boundary
                    :documentation "The durable watermark already delivered by initial replay.")
   (catch-up-p :initform nil :accessor acp-session-catch-up-p
               :documentation "Whether the provider request active at attachment uses durable projection.")
   (cancelled-p :initform nil :accessor acp-session-cancelled-p :type boolean
                :documentation "Whether the editor cancelled the current prompt.")
   (idle-count :initform 0 :accessor acp-session-idle-count :type (integer 0)
               :documentation "How many times the channel finished its work and went idle.")
   (last-outcome :initform nil :accessor acp-session-last-outcome
                 :documentation "The outcome of the channel's last finished work.")
   (epoch :initform 0 :accessor acp-session-epoch :type integer
          :documentation "The generation of the latest turn, numbering turn events.")
   (turn :initform nil :accessor acp-session-turn
         :documentation "The running turn as (EPOCH . SOURCE), or NIL.")
   (turn-sequence :initform 0 :accessor acp-session-turn-sequence :type integer
                  :documentation "The durable user-message sequence qualifying tool identifiers.")
   (streamed-text :initform (text-buffer-create) :accessor acp-session-streamed-text :type string
                  :documentation "Text already sent for the current provider response.")
   (thought-state :initform nil :accessor acp-session-thought-state
                  :type          (member nil :streaming :boundary)
                  :documentation "Whether thoughts continue or cross a provider response boundary.")
   (update-buffer :initform nil :accessor acp-session-update-buffer
                  :documentation "The ordered buffer coalescing visible thought fragments.")
   (tool-call :initform nil :accessor acp-session-tool-call :type (option string)
              :documentation "The latest tool call, which an approval belongs to.")
   (extension-lock :initform      (make-lock "Autolith ACP extension events")
                   :reader        acp-session-extension-lock
                   :documentation "Orders extension sequence allocation and wire notification writes.")
   (extension-sequence :initform 0 :accessor acp-session-extension-sequence :type integer
                       :documentation "The last extension notification sequence."))
  (:default-initargs :name "acp")
  (:documentation "One editor session following one channel of the world server."))


;;;; -- Lookup --

(-> acp-service--session (acp-service string) acp-session)
(defun acp-service--session (service identifier)
  "Return the live session IDENTIFIER, or signal Invalid Params."
  (with-lock-held ((acp-service-lock service))
    (or (gethash identifier (acp-service-sessions service))
        (agentcomms:acp-invalid-params "Unknown session ~A." identifier))))

(defmethod channel-subscription-call :around ((session acp-session) command &rest arguments)
  "Translate neutral transport failures at the ACP boundary without resending commands."
  (declare (ignore command arguments))
  (handler-case (call-next-method)
                (localgroup-request-interrupted (condition)
                 (error 'acp-request-interrupted :message (autolith-error-message condition) :data
                        (agentcomms:json-object "outcome" "interrupted" "executionOutcome"
                                                "unknown" "requestId"
                                                (localgroup-request-interrupted-identifier
                                                 condition)
                                                "command"
                                                (string-downcase
                                                 (symbol-name
                                                  (localgroup-request-interrupted-command
                                                   condition)))
                                                "reconnectId"
                                                (localgroup-request-interrupted-reconnect-id
                                                 condition))))
                (localgroup-error (condition)
                 (error 'agentcomms:acp-state-error :message (autolith-error-message condition)))
                (channel-subscription-unavailable (condition)
                 (error 'agentcomms:acp-state-error :message (autolith-error-message condition)))))

(-> acp-session-cancel (acp-session) null)
(defun acp-session-cancel (session)
  "Cancel through the installed transport, including during endpoint shutdown."
  (let ((connection nil))
    (with-lock-held ((channel-subscription-lock session))
      (setf (acp-session-cancelled-p session) t)
      (unless (channel-subscription-closed-p session)
        (setf connection (channel-subscription-connection session))))
    (when connection
      (handler-case (localgroup-client-call connection ':cancel)
        (localgroup-error ()
          nil))))
  nil)

(define-condition acp-operation-busy (agentcomms:acp-method-error)
  ()
  (:default-initargs :code -32603 :message "Another primary operation is active.")
  (:documentation "The channel works on something that does not admit this operation now."))

(define-condition acp-request-interrupted (agentcomms:acp-state-error agentcomms:acp-method-error)
  ()
  (:default-initargs :code -32603)
  (:documentation "A channel request lost its reply; DATA identifies its unknown execution outcome."))

(defmethod channel-subscription-stopping-p ((session acp-session))
  "Follow the ACP endpoint's shutdown state."
  (acp-service-closed-p (acp-session-service session)))
