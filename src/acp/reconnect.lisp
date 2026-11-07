(in-package #:autolith)

;;;; -- ACP Channel Reconnection --

(-> acp-session--forget (acp-session &key (:wire-p boolean)) null)
(defun acp-session--forget (session &key wire-p)
  "Remove SESSION's registration without removing a replacement for its channel.

WIRE-P also releases the protocol library's registration after asynchronous
transport failure. An explicit protocol close leaves that removal to its
caller, which owns the prompt-cancellation and close transition."
  (let ((service    (acp-session-service session))
        (identifier (acp-session-identifier session)))
    (with-lock-held ((acp-service-lock service))
      (when (eq session (gethash identifier (acp-service-sessions service)))
        (remhash identifier (acp-service-sessions service))
        (when wire-p
          ;; The library has no public asynchronous session-release operation.
          (agentcomms::agent--forget-session service identifier)))))
  nil)

(-> acp-session--mark-closed (acp-session &optional (option string)) null)
(defun acp-session--mark-closed (session &optional report)
  "Close SESSION's admission, wake waiting prompts and forget its stale registration."
  (with-lock-held ((acp-session-lock session))
    (setf (acp-session-closed-p session) t
          (acp-session-reconnect-id session) nil)
    (when report
      (setf (acp-session-reconnect-failure session) report))
    (sb-thread:condition-broadcast (acp-session-condition-variable session)))
  (acp-session--forget session :wire-p t)
  nil)

(-> acp-session--await-connection (acp-session) localgroup-client-connection)
(defun acp-session--await-connection (session)
  "Wait through a planned restart before admitting a new channel request.

An already transmitted request is never resent; its transport reports an
interrupted outcome if the old socket closes before its reply arrives."
  (with-lock-held ((acp-session-lock session))
    (loop while (and (acp-session-reconnect-id session)
                     (not (acp-session-closed-p session))
                     (not (acp-service-closed-p (acp-session-service session))))
          do (condition-wait (acp-session-condition-variable session) (acp-session-lock session)))
    (when (or (acp-session-closed-p session)
              (acp-service-closed-p (acp-session-service session))
              (null (acp-session-connection session)))
      (error 'agentcomms:acp-state-error
             :message (or (acp-session-reconnect-failure session) "The session's channel is gone.")))
    (acp-session-connection session)))

(-> acp-session--checkpoint-reconnect (acp-session list) null)
(defun acp-session--checkpoint-reconnect (session fields)
  "Remember an authenticated planned restart before the old connection closes."
  (let ((identifier (getf fields :id)))
    (when (non-empty-string-p identifier)
      (with-lock-held ((acp-session-lock session))
        (unless (acp-session-closed-p session)
          (setf (acp-session-reconnect-id session) identifier
                (acp-session-reconnect-failure session) nil)))))
  nil)

(-> acp-session--connection-closed (acp-session) null)
(defun acp-session--connection-closed (session)
  "Reconnect a planned restart, or forget a channel that closed unexpectedly."
  (let ((closed-p nil))
    (with-lock-held ((acp-session-lock session))
      (cond
        ((acp-session-closed-p session)
         nil)
        ((acp-session-reconnect-id session)
         (unless (acp-session-reconnect-thread session)
           (let ((connection (acp-session-connection session))
                 (identifier (acp-session-reconnect-id session))
                 (sequence   (acp-session-event-sequence session)))
             (setf (acp-session-reconnect-thread session)
                   (make-thread
                    (lambda () (acp-session--reconnect session connection
                                                      :identifier identifier :sequence sequence))
                    :name "Autolith ACP checkpoint reconnect")))))
        (t
         (setf closed-p t))))
    (when closed-p
      (acp-session--mark-closed session)))
  nil)

(-> acp-session--connect-replacement
    (acp-session localgroup-client-connection &key (:identifier string) (:sequence (integer 0)))
    localgroup-client-connection)
(defun acp-session--connect-replacement (session previous &key identifier sequence)
  "Subscribe by cursor, retrying a checkpoint that races endpoint discovery.

Only the subscription handshake is retried. No channel command has been sent.
Explicit rejection, including an expired replay cursor, is terminal."
  (let ((allow-later-p nil)
        (deadline      (+ (get-internal-real-time)
                          (* *localgroup-handoff-start-timeout-seconds* internal-time-units-per-second))))
    (flet ((cancelled-p ()
             (or (acp-session-closed-p session)
                 (acp-service-closed-p (acp-session-service session)))))
      (loop
        (let ((entry (localgroup--wait-for-checkpoint-entry
                      (localgroup-client-connection-entry previous) identifier
                      :configuration (acp-session-configuration session)
                      :cancelled-p #'cancelled-p :allow-later-p allow-later-p)))
          (handler-case
              (return
                (localgroup-client-connect
                 entry :name "acp" :after-sequence sequence :cancelled-p #'cancelled-p
                 :event-function (lambda (event) (acp-session-event session event))))
            (localgroup-error (condition)
              (when (or (cancelled-p)
                        (>= (get-internal-real-time) deadline)
                        (and (typep condition 'localgroup-client-rejected)
                             (not (localgroup-error-retry-p condition))))
                (error condition))
              (setf allow-later-p t)
              (sleep 0.05))))))))

(-> acp-session--reconnect
    (acp-session localgroup-client-connection &key (:identifier string) (:sequence (integer 0))) null)
(defun acp-session--reconnect (session previous &key identifier sequence)
  "Resume SESSION after IDENTIFIER from its last delivered event SEQUENCE.

The endpoint must preserve the old channel identity and authentication. A
later checkpoint may supersede IDENTIFIER during subscription. A missing
endpoint or an expired cursor closes the session with a reported failure."
  (let ((connection  nil)
        (installed-p nil))
    (unwind-protect
         (handler-case
              (progn
                (setf sequence (acp-session-begin-attachment session))
                (setf connection (acp-session--connect-replacement
                                  session previous :identifier identifier :sequence sequence))
                (with-lock-held ((acp-session-lock session))
                  (unless (or (acp-session-closed-p session)
                              (acp-service-closed-p (acp-session-service session)))
                    (setf (acp-session-connection session) connection
                          (acp-session-reconnect-id session) nil
                          (acp-session-reconnect-thread session) nil
                          installed-p t))
                  (sb-thread:condition-broadcast (acp-session-condition-variable session)))
                (when installed-p
                  (acp-session-drain-events session)))
           (error (condition)
             (unless (acp-session-closed-p session)
               (acp-session--mark-closed session (princ-to-string condition)))))
      (when (or (not installed-p) (acp-session-closed-p session))
        (when connection
          (localgroup-client-close connection)))
      (with-lock-held ((acp-session-lock session))
        (when (eq (current-thread) (acp-session-reconnect-thread session))
          (setf (acp-session-reconnect-thread session) nil))
        (sb-thread:condition-broadcast (acp-session-condition-variable session)))))
  nil)
