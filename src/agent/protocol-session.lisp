;;;; -- Optional Agent Turn Lifecycle Bridge --

(in-package #:autolith)

(defclass agent-protocol-session ()
  ((client :initarg :client :reader agent-protocol-session-client
           :documentation "The handshaken private protocol client.")
   (driver :initform nil :accessor agent-protocol-session-driver
           :documentation "The sole-reader driver.")
   (agent :initform nil :accessor agent-protocol-session-agent
          :documentation "The agent with an explicit parent provider and conversation.")
   (conversation :initform nil :accessor agent-protocol-session-conversation
                 :documentation "The parent-backed conversation projection.")
   (observer :initarg :observer :reader agent-protocol-session-observer
             :documentation "The caller's presentation and authorization observer.")
   (run-turn :initarg :run-turn :reader agent-protocol-session-run-turn
             :documentation "The turn runner, called with session and guarded observer.")
   (lock :initform (bordeaux-threads:make-lock "agent session lifecycle")
         :reader agent-protocol-session-lock :documentation "Protect lifecycle work and latches.")
   (ready :initform (bordeaux-threads:make-condition-variable)
          :reader agent-protocol-session-ready :documentation "Wake the separate turn worker.")
   (turn :initform nil :accessor agent-protocol-session-turn
         :documentation "The queued or running TurnStart envelope.")
   (cancelled :initform nil :accessor agent-protocol-session-cancelled
              :documentation "Monotonic cancellation latch for the active turn.")
   (stop :initform nil :accessor agent-protocol-session-stop
         :documentation "Whether PrepareStop requires quiescence and ReadyToStop.")
   (failure :initform nil :accessor agent-protocol-session-failure
            :documentation "The first local failure or disconnect.")
   (worker :initform nil :accessor agent-protocol-session-worker
           :documentation "The single turn worker, distinct from the lifecycle dispatcher.")
   (stopped :initform nil :accessor agent-protocol-session-stopped
            :documentation "Whether ReadyToStop was sent after quiescence.")
   (finished :initform nil :accessor agent-protocol-session-finished
             :documentation "Whether the turn worker has exited."))
  (:documentation "An optional session bridge. It does not select a launch path or read credentials."))

(defclass agent-session-observer (agent-observer)
  ((session :initarg :session :reader agent-session-observer-session
            :documentation "The session whose lifecycle gates new effects.")
   (delegate :initarg :delegate :reader agent-session-observer-delegate
             :documentation "The caller's observer."))
  (:documentation "Forward presentation and authorization while recording effect starts."))

(-> agent-protocol-session-cancelled-p (agent-protocol-session) boolean)
(defun agent-protocol-session-cancelled-p (session)
  "Read the cancellation, stop, and disconnect latches without waiting on turn work."
  (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
    (not (null (or (agent-protocol-session-cancelled session)
                   (agent-protocol-session-stop session)
                   (agent-protocol-session-failure session))))))

(-> agent-session--check-running (agent-protocol-session) null)
(defun agent-session--check-running (session)
  "Stop before a new model request or effect after cancellation or disconnection."
  (when (agent-protocol-session-cancelled-p session)
    (error 'application-turn-cancelled))
  nil)

(defmethod agent-observer-text ((observer agent-session-observer) text)
  "Forward text without granting it authority to execute effects."
  (agent-observer-text (agent-session-observer-delegate observer) text))

(defmethod agent-observer-reasoning ((observer agent-session-observer) text)
  "Forward reasoning as presentation data."
  (agent-observer-reasoning (agent-session-observer-delegate observer) text))

(defmethod agent-observer-status ((observer agent-session-observer) status details)
  "Check cancellation at runtime boundaries before forwarding status."
  (agent-session--check-running (agent-session-observer-session observer))
  (agent-observer-status (agent-session-observer-delegate observer) status details))

(defmethod agent-observer-take-steering ((observer agent-session-observer))
  "Keep parent input ordering; this bridge does not accept private terminal steering."
  (declare (ignore observer))
  nil)

(defmethod agent-observer-apply-pending-operations ((observer agent-session-observer) agent)
  "Check the lifecycle before invoking caller-owned pending operations."
  (agent-session--check-running (agent-session-observer-session observer))
  (agent-observer-apply-pending-operations (agent-session-observer-delegate observer) agent))

(defmethod agent-observer-authorize-command ((observer agent-session-observer) command directory)
  "Authorize through the caller's observer inside the existing access boundary."
  (agent-session--check-running (agent-session-observer-session observer))
  (agent-observer-authorize-command (agent-session-observer-delegate observer) command directory))

(defmethod agent-observer-authorize-tool ((observer agent-session-observer) tool arguments)
  "Check the lifecycle before asking the caller to authorize a tool."
  (agent-session--check-running (agent-session-observer-session observer))
  (agent-observer-authorize-tool (agent-session-observer-delegate observer) tool arguments))

(defmethod agent-observer-call-with-tool-execution ((observer agent-session-observer) identifier function)
  "Acknowledge ToolStarted before invoking FUNCTION. Never retry uncertain effects."
  (let* ((session      (agent-session-observer-session observer))
         (conversation (agent-protocol-session-conversation session))
         (call         (find identifier (conversation-input-items conversation)
                             :test #'equal :key (lambda (item)
                                                  (and (function-call-item-p item) (json-get item "call_id")))))
         (payload      (make-hash-table :test #'equal)))
    (agent-session--check-running session)
    (unless call (error 'agent-protocol-error :reason ':unknown-tool-call))
    (setf (gethash "call_id" payload) identifier
          (gethash "name" payload) (conversation--tool-call-name call)
          (gethash "arguments" payload) (agent-context--data (json-decode (json-get call "arguments"))))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (let* ((client (agent-protocol-session-client session))
             (record (agent-wire:make-session-record
                      :event-id (format nil "agent-start/~A/~D"
                                        (autolith-supervisor::parent-connection-instance-id client)
                                        (incf (agent-session-conversation-counter conversation)))
                      :turn-id (agent-session-conversation-turn-id conversation) :kind ':tool-started
                      :payload  (autolith-supervisor::protocol-data->value payload)))
             (ack    (agent-session--append conversation record)))
        (setf (agent-wire:session-record.sequence record) (agent-wire:session-ack.sequence ack)
              (agent-wire:session-record.origin record) ':agent
              (gethash identifier (agent-session-conversation-started-calls conversation))
              (agent-session-conversation-turn-id conversation))
        (agent-session--retain conversation record)))
    (agent-session--check-running session)
    (agent-observer-call-with-tool-execution (agent-session-observer-delegate observer) identifier function)))

(-> agent-session--run-default (agent-protocol-session agent-observer) t)
(defun agent-session--run-default (session observer)
  "Run the normal provider/tool loop over the already-durable parent input."
  (let* ((agent        (agent-protocol-session-agent session))
         (conversation (agent-conversation agent)))
    (with-lock-held ((agent-turn-lock agent))
      (context-rule-start-turn agent)
      (unwind-protect
           (agent--run-provider-loop agent observer)
        (conversation-clear-ephemeral-input-items conversation)
        (setf (conversation-turn-state conversation) nil)))))

(-> agent-protocol-session-start
    (agent-protocol-client configuration string
     &key (:tool-registry (option tool-registry)) (:worker t)
          (:observer (option agent-observer)) (:run-turn function))
    agent-protocol-session)
(defun agent-protocol-session-start
    (client configuration profile-id &key tool-registry worker observer (run-turn #'agent-session--run-default))
  "Start an optional real agent session after handshake, with explicit configuration/profile.
Own the client receive lifecycle until CLOSE and JOIN. The worker reads and
projects parent history before a turn; callbacks never run turn work themselves."
  (let* ((session      (make-instance 'agent-protocol-session :client client :run-turn run-turn
                                     :observer (or observer (make-instance 'agent-observer))))
         (conversation (make-instance 'agent-session-conversation
                                      :identifier (agent-wire:welcome.session-id (agent-protocol-client-welcome client))
                                      :pathname #P"parent-session" :log-pathname #P"parent-session"
                                      :persisted-p t :created-at (get-universal-time)
                                      :model      (config :model configuration)
                                      :model-provider "parent" :reasoning-effort (config :reasoning-effort configuration)
                                      :next-sequence 1 :input-items nil :driver nil)))
    (setf (agent-protocol-session-conversation session) conversation)
    (handler-case
        (progn
          (setf (agent-protocol-session-driver session)
                (agent-protocol-driver-start
                 client :lifecycle (lambda (driver envelope)
                                     (declare (ignore driver))
                                     (agent-session--lifecycle session envelope))
                 :disconnect       (lambda (driver condition)
                                     (declare (ignore driver))
                                     (agent-session--disconnect session condition))))
          (setf (agent-session-conversation-driver conversation) (agent-protocol-session-driver session)
                (agent-protocol-session-agent session)
                (agent-create :configuration configuration :conversation conversation
                              :provider (make-agent-protocol-provider configuration (agent-protocol-session-driver session) profile-id)
                              :tool-registry tool-registry :worker worker))
          (setf (agent-protocol-session-worker session)
                (bordeaux-threads:make-thread (lambda () (agent-session--work session)) :name "parent-backed agent turn"))
          session)
      (error (condition)
        (when (agent-protocol-session-driver session)
          (agent-protocol-driver-close (agent-protocol-session-driver session) :condition condition)
          (agent-protocol-driver-join (agent-protocol-session-driver session)))
        (error condition)))))

(-> agent-session--lifecycle (agent-protocol-session agent-wire:envelope) null)
(defun agent-session--lifecycle (session envelope)
  "Latch lifecycle messages in wire order; leave all slow work to the turn worker."
  (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
    (case (agent-wire:envelope.body-case envelope)
      (agent-wire:turn-start
       (when (or (agent-protocol-session-turn session) (agent-protocol-session-stop session))
         (error 'agent-protocol-error :reason ':overlapping-turn))
       (setf (agent-protocol-session-turn session) envelope
             (agent-protocol-session-cancelled session) nil))
      (agent-wire:turn-cancel
       (let ((turn (agent-protocol-session-turn session)))
         (when (and turn (equal (agent-wire:turn-start.turn-id (agent-wire:envelope.turn-start turn))
                               (agent-wire:turn-cancel.turn-id (agent-wire:envelope.turn-cancel envelope))))
           (setf (agent-protocol-session-cancelled session) t))))
      (agent-wire:prepare-stop
       (setf (agent-protocol-session-stop session) t)))
    (bordeaux-threads:condition-notify (agent-protocol-session-ready session)))
  nil)

(-> agent-session--disconnect (agent-protocol-session condition) null)
(defun agent-session--disconnect (session condition)
  "Latch disconnection. Do not retry appends or claim that unfinished effects stopped."
  (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
    (unless (or (agent-protocol-session-failure session) (agent-protocol-session-stopped session))
      (setf (agent-protocol-session-failure session) condition))
    (setf (agent-protocol-session-cancelled session) t)
    (bordeaux-threads:condition-notify (agent-protocol-session-ready session)))
  nil)

(-> agent-session--work (agent-protocol-session) null)
(defun agent-session--work (session)
  "Serialize resume, turn work, cancellation cleanup, and the final stop response."
  (unwind-protect
       (handler-case
           (progn
             (agent-session-conversation-sync (agent-protocol-session-conversation session))
             (agent-session-conversation-reconcile (agent-protocol-session-conversation session))
             (loop
               (let ((turn nil) (stop nil))
                 (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
                   (loop until (or (agent-protocol-session-turn session) (agent-protocol-session-stop session)
                                   (agent-protocol-session-failure session))
                         do (bordeaux-threads:condition-wait (agent-protocol-session-ready session)
                                                            (agent-protocol-session-lock session)))
                   (when (agent-protocol-session-failure session) (return))
                   (setf turn (agent-protocol-session-turn session)
                         stop (agent-protocol-session-stop session)))
                 (when turn (agent-session--turn session turn))
                 (when (or stop (agent-protocol-session-stop session))
                   (agent-protocol-ready-to-stop (agent-protocol-session-client session))
                   (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
                     (setf (agent-protocol-session-stopped session) t))
                   (agent-protocol-driver-close (agent-protocol-session-driver session))
                   (return)))))
         (serious-condition (condition)
           (agent-session--disconnect session condition)
           (agent-protocol-driver-close (agent-protocol-session-driver session) :condition condition)))
    (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
      (setf (agent-protocol-session-finished session) t)))
  nil)

(-> agent-session--turn (agent-protocol-session agent-wire:envelope) null)
(defun agent-session--turn (session envelope)
  "Project the referenced input, publish recovery, and send exactly one terminal reply."
  (let* ((conversation                               (agent-protocol-session-conversation session))
         (start                                      (agent-wire:envelope.turn-start envelope))
         (outcome                                    ':failed)
         (*agent-protocol-provider-cancel-predicate* (lambda () (agent-protocol-session-cancelled-p session))))
    (declare (special *agent-protocol-provider-cancel-predicate*))
    (agent-session-conversation-sync conversation)
    (unless (find (agent-wire:turn-start.input-sequence start)
                  (agent-session-conversation-records conversation)
                  :key #'agent-wire:session-record.sequence)
      (error 'agent-protocol-error :reason ':missing-turn-input))
    (setf (agent-session-conversation-turn-id conversation) (agent-wire:turn-start.turn-id start))
    (unwind-protect
         (handler-case
             (progn
               (agent-session-conversation-publish-repairs conversation)
               (agent-session--check-running session)
               (funcall (agent-protocol-session-run-turn session) session
                        (make-instance 'agent-session-observer :session session
                                       :delegate                        (agent-protocol-session-observer session)))
               (agent-session--check-running session)
               (setf outcome ':completed))
           (application-turn-cancelled () (setf outcome ':cancelled))
           (error () (setf outcome (if (agent-protocol-session-cancelled-p session) ':cancelled ':failed))))
      (setf (agent-session-conversation-turn-id conversation) nil))
    ;; Clear the local turn before sending: the peer may immediately submit its next turn.
    (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
      (setf (agent-protocol-session-turn session) nil))
    (agent-protocol-finish-turn (agent-protocol-session-client session) outcome))
  nil)

(-> agent-protocol-session-close (agent-protocol-session) null)
(defun agent-protocol-session-close (session)
  "Close transport and request cooperative quiescence; do not claim workers are joined."
  (agent-protocol-driver-close (agent-protocol-session-driver session))
  nil)

(-> agent-protocol-session-join (agent-protocol-session &key (:timeout (real (0)))) boolean)
(defun agent-protocol-session-join (session &key (timeout 30))
  "Join both protocol threads and the turn worker, or report a bounded cleanup failure."
  (when (eq (bordeaux-threads:current-thread) (agent-protocol-session-worker session))
    (error 'agent-protocol-error :reason ':session-self-join))
  (handler-case
      (sb-ext:with-timeout timeout
        (let ((thread (agent-protocol-session-worker session)))
          (when thread (bordeaux-threads:join-thread thread)))
        (agent-protocol-driver-join (agent-protocol-session-driver session) :timeout timeout))
    (sb-ext:timeout ()
      (error 'agent-protocol-error :reason ':session-join-timeout)))
  t)
