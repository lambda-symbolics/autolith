(in-package #:autolith)

(defclass channel-subscription ()
  ((identifier :initarg :identifier :reader channel-subscription-identifier :type string
               :documentation "The subscribed channel identifier.")
   (configuration :initarg :configuration :reader channel-subscription-configuration
                  :type configuration :documentation "The channel's workspace configuration.")
   (connection :initform nil :accessor channel-subscription-connection
               :documentation "The installed structured transport connection.")
   (reconnect-id :initform nil :accessor channel-subscription-reconnect-id
                 :type (option string) :documentation "The authenticated pending checkpoint transition.")
   (reconnect-thread :initform nil :accessor channel-subscription-reconnect-thread
                     :documentation "The worker owning replacement attachment and buffered delivery.")
   (reconnect-failure :initform nil :accessor channel-subscription-reconnect-failure
                     :type (option string) :documentation "The terminal reconnect diagnostic, or NIL.")
   (started-p :initarg :started-p :initform nil :reader channel-subscription-started-p
              :type boolean :documentation "Whether this subscription started the channel.")
   (lock :initform (make-lock "Autolith channel subscription") :reader channel-subscription-lock
         :documentation "Protects admission, reconnect ownership and the submission waiter.")
   (event-lock :initform      (make-lock "Autolith channel delivery")
               :reader        channel-subscription-event-lock
               :documentation "Serializes replay, attachment buffering and live presentation.")
   (attaching-p :initform nil :accessor channel-subscription-attaching-p
                :documentation "Whether callbacks and request admission await attachment delivery.")
   (attachment-events :initform (make-deque) :reader channel-subscription-attachment-events
                      :documentation "Callbacks buffered during attachment or reconnect.")
   (event-sequence :initform 0 :accessor channel-subscription-event-sequence
                   :documentation "The last completely presented event cursor.")
   (condition-variable :initform      (make-condition-variable :name "Autolith channel subscription")
                       :reader        channel-subscription-condition-variable
                       :documentation "Signals completion, admission changes and closure.")
   (submission-active-p :initform nil :accessor channel-subscription-submission-active-p
                        :type boolean :documentation "Whether a submission waiter is registered.")
   (submission-id :initform nil :accessor channel-subscription-submission-id
                  :documentation "The exact submission awaited by the registered caller.")
   (submission-result :initform nil :accessor channel-subscription-submission-result
                      :documentation "The registered submission's terminal event fields.")
   (closed-p :initform nil :accessor channel-subscription-closed-p :type boolean
             :documentation "Whether the subscription closed or lost its channel.")
   (client-identifier :initform (make-identifier) :reader channel-subscription-client-identifier
                      :type string :documentation "The stable submitting identity across reconnects.")
   (name :initarg :name :initform "client" :reader channel-subscription-name
         :documentation "The transport client name.")
   (approves-p :initarg :approves-p :initform t :reader channel-subscription-approves-p
               :documentation "Whether this client answers approvals."))
  (:documentation "A channel subscription with ordered delivery and exact submission completion."))

(define-condition channel-subscription-unavailable (autolith-error)
  ()
  (:documentation "A closed or occupied channel subscription cannot admit a request."))

(defgeneric channel-subscription-stopping-p (subscription)
  (:documentation "Return whether the subscription's owner is shutting down."))

(defmethod channel-subscription-stopping-p ((subscription channel-subscription))
  "Ordinary clients have no independently stopping owner."
  (declare (ignore subscription))
  nil)

(defgeneric channel-subscription-forget (subscription &key wire-p)
  (:documentation "Release any frontend registration after closing SUBSCRIPTION."))

(defmethod channel-subscription-forget ((subscription channel-subscription) &key wire-p)
  "Ordinary clients have no frontend registration."
  (declare (ignore subscription wire-p))
  nil)

(defgeneric channel-subscription-present-event (subscription event)
  (:documentation "Synchronously present one admitted event before acknowledging its delivery."))

(defgeneric channel-subscription-prepare-attachment (subscription greeting &key replay-p)
  (:documentation "Prepare frontend history while live callbacks are buffered."))

(defmethod channel-subscription-prepare-attachment
    ((subscription channel-subscription) greeting &key replay-p)
  "Ordinary clients require no durable-history projection."
  (declare (ignore subscription greeting replay-p))
  nil)

(-> channel-subscription-begin-attachment (channel-subscription) integer)
(defun channel-subscription-begin-attachment (subscription)
  "Buffer callbacks before opening a connection and return the processed cursor."
  (with-lock-held ((channel-subscription-event-lock subscription))
    (with-lock-held ((channel-subscription-lock subscription))
      (setf (channel-subscription-attaching-p subscription) t))
    (channel-subscription-event-sequence subscription)))

(-> channel-subscription--drain-events (channel-subscription) null)
(defun channel-subscription--drain-events (subscription)
  "Drain buffered callbacks with the event lock held, then reopen request admission."
  (loop until (deque-empty-p (channel-subscription-attachment-events subscription))
        do (channel-subscription--consume-event
            subscription (deque-pop-front (channel-subscription-attachment-events subscription))))
  (with-lock-held ((channel-subscription-lock subscription))
    (setf (channel-subscription-attaching-p subscription) nil)
    (sb-thread:condition-broadcast (channel-subscription-condition-variable subscription)))
  nil)

(-> channel-subscription-drain-events (channel-subscription) null)
(defun channel-subscription-drain-events (subscription)
  "Deliver buffered callbacks before admitting live delivery and new requests."
  (with-lock-held ((channel-subscription-event-lock subscription))
    (channel-subscription--drain-events subscription))
  nil)

(-> channel-subscription-finish-attachment
    (channel-subscription list &key (:replay-p boolean)) null)
(defun channel-subscription-finish-attachment (subscription greeting &key replay-p)
  "Prepare the initial history snapshot and drain callbacks newer than its cursor."
  (let ((sequence (getf (rest greeting) :sequence))
        (boundary (getf (rest greeting) :record-sequence)))
    (unless (and (integerp sequence) (integerp boundary))
      (error 'channel-subscription-unavailable
             :message "The channel did not provide an attachment cursor."))
    (channel-subscription-prepare-attachment subscription greeting :replay-p replay-p)
    (with-lock-held ((channel-subscription-event-lock subscription))
      (setf (channel-subscription-event-sequence subscription) sequence)
      (channel-subscription--drain-events subscription)))
  nil)

(-> channel-subscription-attach (channel-subscription &key (:replay-p boolean)) null)
(defun channel-subscription-attach (subscription &key replay-p)
  "Subscribe before preparing history, buffering callbacks until installation finishes."
  (channel-subscription-begin-attachment subscription)
  (let ((connection
          (localgroup-client-connect
           (localgroup--find-record (channel-subscription-configuration subscription)
                                   (channel-subscription-identifier subscription))
           :name           (channel-subscription-name subscription)
           :approves-p     (channel-subscription-approves-p subscription)
           :event-function (lambda (event) (channel-subscription-event subscription event)))))
    (setf (channel-subscription-connection subscription) connection)
    (channel-subscription-finish-attachment
     subscription (localgroup-client-connection-attached connection) :replay-p replay-p))
  nil)

(-> channel-subscription-event (channel-subscription list) null)
(defun channel-subscription-event (subscription event)
  "Serialize live callbacks with attachment replay and completed event delivery."
  (with-lock-held ((channel-subscription-event-lock subscription))
    (if (channel-subscription-attaching-p subscription)
        (deque-push-back (channel-subscription-attachment-events subscription) event)
        (channel-subscription--consume-event subscription event)))
  nil)

(-> channel-subscription--consume-event (channel-subscription list) null)
(defun channel-subscription--consume-event (subscription event)
  "Present EVENT, commit its cursor, and only then settle a matching submission."
  (when (channel-subscription-closed-p subscription)
    (return-from channel-subscription--consume-event nil))
  (let* ((fields (rest event)) (sequence (getf fields :sequence)))
    (unless
        (and (not (eq (first event) ':checkpoint-reconnect)) sequence
             (<= sequence (channel-subscription-event-sequence subscription)))
      (case (first event)
        (:checkpoint-reconnect
         (channel-subscription--checkpoint-reconnect subscription fields))
        (:closed
         (channel-subscription--connection-closed subscription))
        (otherwise
         (channel-subscription-present-event subscription event)))
      (when (and sequence (not (eq (first event) ':checkpoint-reconnect)))
        (setf (channel-subscription-event-sequence subscription) sequence))
      (when (eq (first event) ':work-finished)
        (let ((submission (getf fields :submission)))
          (with-lock-held ((channel-subscription-lock subscription))
            (when
                (and (channel-subscription-submission-id subscription)
                     (equal (getf submission :id)
                            (channel-subscription-submission-id subscription))
                     (equal (getf submission :client)
                            (channel-subscription-client-identifier subscription)))
              (setf (channel-subscription-submission-result subscription) fields))
            (sb-thread:condition-broadcast
             (channel-subscription-condition-variable subscription)))))))
  nil)

(defgeneric channel-subscription-call (subscription command &rest arguments)
  (:documentation "Send one request without retrying an already transmitted command."))

(defmethod channel-subscription-call ((subscription channel-subscription) command &rest arguments)
  "Wait for a usable connection, then preserve the transport's outcome."
  (apply #'localgroup-client-call (channel-subscription--await-connection subscription)
         command arguments))

(-> channel-subscription-submit
    (channel-subscription string &key (:source string) (:steer-p boolean) (:submission-id string)) list)
(defun channel-subscription-submit
    (subscription text &key (source "prompt") steer-p (submission-id (make-identifier)))
  "Submit TEXT with the client's identity and SOURCE, returning the channel reply."
  (channel-subscription-call
   subscription ':submit :text text :steer-p steer-p
   :submission (list :id     submission-id
                     :client (channel-subscription-client-identifier subscription)
                     :source source)))

(-> channel-subscription-await-submission
    (channel-subscription string &key (:source string)) list)
(defun channel-subscription-await-submission (subscription text &key (source "prompt"))
  "Submit TEXT and await its own fully presented completion, independently of channel idleness."
  (let ((identifier (make-identifier)))
    (with-lock-held ((channel-subscription-lock subscription))
      (when (channel-subscription-closed-p subscription)
        (error 'channel-subscription-unavailable :message "The subscription is closed."))
      (when (channel-subscription-submission-active-p subscription)
        (error 'channel-subscription-unavailable :message "A submission is already running in this client."))
      (setf (channel-subscription-submission-active-p subscription) t
            (channel-subscription-submission-id subscription) identifier
            (channel-subscription-submission-result subscription) nil))
    (unwind-protect
         (progn
           (channel-subscription-submit subscription text :source source :submission-id identifier)
           (with-lock-held ((channel-subscription-lock subscription))
             (loop until (or (channel-subscription-closed-p subscription)
                             (channel-subscription-submission-result subscription))
                   do (condition-wait (channel-subscription-condition-variable subscription)
                                      (channel-subscription-lock subscription)))
             (or (channel-subscription-submission-result subscription)
                 (error 'channel-subscription-unavailable
                        :message (or (channel-subscription-reconnect-failure subscription)
                                     "The channel went away before the submission finished.")))))
      (with-lock-held ((channel-subscription-lock subscription))
        (setf (channel-subscription-submission-active-p subscription) nil
              (channel-subscription-submission-id subscription) nil)))))

(-> channel-subscription-close (channel-subscription) null)
(defun channel-subscription-close (subscription)
  "Close admission, wake waiters and release transport, reconnect worker and any owned channel."
  (let ((connection nil) (reconnect nil))
    (with-lock-held ((channel-subscription-lock subscription))
      (setf (channel-subscription-closed-p subscription) t
            (channel-subscription-reconnect-id subscription) nil
            connection (channel-subscription-connection subscription)
            reconnect (channel-subscription-reconnect-thread subscription))
      (sb-thread:condition-broadcast (channel-subscription-condition-variable subscription)))
    (channel-subscription-forget subscription)
    (when connection (localgroup-client-close connection))
    (when (and reconnect (not (eq reconnect (current-thread))))
      (join-thread reconnect))
    (when (and connection (channel-subscription-started-p subscription))
      (let ((world (world-client-entry (channel-subscription-configuration subscription))))
        (when world
          (handler-case
              (localgroup-query-record world ':close
                                       (list :channel           (channel-subscription-identifier subscription)
                                             :unattached-only-p t))
            (localgroup-error () nil))))))
  nil)

(-> channel-subscription--mark-closed (channel-subscription &optional (option string)) null)
(defun channel-subscription--mark-closed (session &optional report)
  "Close SESSION's admission, wake waiting prompts and forget its stale registration."
  (with-lock-held ((channel-subscription-lock session))
    (setf (channel-subscription-closed-p session) t
          (channel-subscription-reconnect-id session) nil)
    (when report (setf (channel-subscription-reconnect-failure session) report))
    (sb-thread:condition-broadcast (channel-subscription-condition-variable session)))
  (channel-subscription-forget session :wire-p t)
  nil)

(-> channel-subscription--await-connection (channel-subscription) localgroup-client-connection)
(defun channel-subscription--await-connection (subscription)
  "Wait through reconnect and attachment delivery before admitting a new request."
  (with-lock-held ((channel-subscription-lock subscription))
    (loop while (and (or (channel-subscription-reconnect-id subscription)
                         (channel-subscription-attaching-p subscription))
                     (not (channel-subscription-closed-p subscription))
                     (not (channel-subscription-stopping-p subscription)))
          do (condition-wait (channel-subscription-condition-variable subscription)
                             (channel-subscription-lock subscription)))
    (when (or (channel-subscription-closed-p subscription)
              (channel-subscription-stopping-p subscription)
              (null (channel-subscription-connection subscription)))
      (error 'channel-subscription-unavailable
             :message (or (channel-subscription-reconnect-failure subscription)
                          "The client's channel is gone.")))
    (channel-subscription-connection subscription)))

(-> channel-subscription--checkpoint-reconnect (channel-subscription list) null)
(defun channel-subscription--checkpoint-reconnect (session fields)
  "Remember an authenticated planned restart before the old connection closes."
  (let ((identifier (getf fields :id)))
    (when (non-empty-string-p identifier)
      (with-lock-held ((channel-subscription-lock session))
        (unless (channel-subscription-closed-p session)
          (setf (channel-subscription-reconnect-id session) identifier
                (channel-subscription-reconnect-failure session) nil)))))
  nil)

(-> channel-subscription--connection-closed (channel-subscription) null)
(defun channel-subscription--connection-closed (subscription)
  "Reconnect a planned restart, or forget a channel that closed unexpectedly."
  (let ((closed-p nil))
    (with-lock-held ((channel-subscription-lock subscription))
      (cond
        ((channel-subscription-closed-p subscription)
         nil)
        ((channel-subscription-reconnect-id subscription)
         (unless (channel-subscription-reconnect-thread subscription)
           (let ((connection (channel-subscription-connection subscription))
                 (identifier (channel-subscription-reconnect-id subscription))
                 (sequence   (channel-subscription-event-sequence subscription)))
             (setf (channel-subscription-reconnect-thread subscription)
                   (make-thread
                    (lambda ()
                      (channel-subscription--reconnect subscription connection
                                                       :identifier identifier :sequence sequence))
                    :name "Autolith channel reconnect")))))
        (t
         (setf closed-p t))))
    (when closed-p
      (channel-subscription--mark-closed subscription)))
  nil)

(-> channel-subscription--connect-replacement
    (channel-subscription localgroup-client-connection &key (:identifier string)
     (:sequence (integer 0)))
    localgroup-client-connection)
(defun channel-subscription--connect-replacement (subscription previous &key identifier sequence)
  "Subscribe by cursor, retrying a checkpoint that races endpoint discovery.

Only the subscription handshake is retried. No channel command has been sent.
Explicit rejection, including an expired replay cursor, is terminal."
  (let ((allow-later-p nil)
        (deadline      (+ (get-internal-real-time)
                          (* *localgroup-handoff-start-timeout-seconds* internal-time-units-per-second))))
    (flet ((cancelled-p ()
             (or (channel-subscription-closed-p subscription)
                 (channel-subscription-stopping-p subscription))))
      (loop
        (let ((entry (localgroup--wait-for-checkpoint-entry
                      (localgroup-client-connection-entry previous) identifier
                      :configuration (channel-subscription-configuration subscription)
                      :cancelled-p #'cancelled-p :allow-later-p allow-later-p)))
          (handler-case
              (return (localgroup-client-connect
                       entry :name     (channel-subscription-name subscription)
                       :approves-p     (channel-subscription-approves-p subscription)
                       :after-sequence sequence :cancelled-p #'cancelled-p
                       :event-function (lambda (event) (channel-subscription-event subscription event))))
            (localgroup-error (condition)
              (when (or (cancelled-p) (>= (get-internal-real-time) deadline)
                        (and (typep condition 'localgroup-client-rejected)
                             (not (localgroup-error-retry-p condition))))
                (error condition))
              (setf allow-later-p t)
              (sleep 0.05))))))))

(-> channel-subscription--reconnect
    (channel-subscription localgroup-client-connection &key (:identifier string)
     (:sequence (integer 0)))
    null)
(defun channel-subscription--reconnect (subscription previous &key identifier sequence)
  "Resume from the processed cursor, draining replay before admitting channel requests."
  (let ((connection  nil)
        (installed-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf sequence (channel-subscription-begin-attachment subscription)
                     connection (channel-subscription--connect-replacement
                                 subscription previous :identifier identifier :sequence sequence))
               (with-lock-held ((channel-subscription-event-lock subscription))
                 (with-lock-held ((channel-subscription-lock subscription))
                   (unless (or (channel-subscription-closed-p subscription)
                               (channel-subscription-stopping-p subscription))
                     (setf (channel-subscription-connection subscription) connection
                           (channel-subscription-reconnect-id subscription) nil
                           installed-p t)))
                 (when installed-p
                   (channel-subscription--drain-events subscription))))
           (error (condition)
             (unless (channel-subscription-closed-p subscription)
               (channel-subscription--mark-closed subscription (princ-to-string condition)))))
      (when (or (not installed-p) (channel-subscription-closed-p subscription))
        (when connection
          (localgroup-client-close connection)))
      (with-lock-held ((channel-subscription-lock subscription))
        (when (eq (current-thread) (channel-subscription-reconnect-thread subscription))
          (setf (channel-subscription-reconnect-thread subscription) nil))
        (sb-thread:condition-broadcast (channel-subscription-condition-variable subscription)))
      ;; A second checkpoint can close its connection during the buffered drain.
      (when (and installed-p (localgroup-client-connection-closed-p connection))
        (channel-subscription--connection-closed subscription))))
  nil)
