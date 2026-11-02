(in-package #:autolith)

;;;; -- World Hooks of the Application --

;;; The application keeps the world in step with the conversations it opens.
;;; The reconciliation hook is release machinery that leaves with
;;; src/world/sessions.lisp.

(-> application-world-sync (application &key (:all-p boolean)) null)
(defun application-world-sync (application &key all-p)
  "Reconcile the active conversation, or every conversation with ALL-P, and select its channel.

Failures are reported as warnings: the world is a projection of the
conversations and never blocks a session."
  (let* ((configuration (application-configuration application))
         (conversation  (and (slot-boundp application 'conversation)
                             (application-conversation application))))
    (handler-case
        (progn
          (if all-p
              (world-reconcile-sessions configuration)
              (when (and conversation (conversation-persisted-p conversation))
                (world-reconcile-sessions configuration
                                          :identities (list (conversation-pathname conversation)))))
          (when conversation
            (world-session-select configuration (conversation-identifier conversation))))
      (error (cause)
        (warn 'world-session-warning
              :pathname (configuration-world-sessions-path configuration)
              :cause    cause))))
  nil)

(-> application-channels-entry (application) list)
(defun application-channels-entry (application)
  "Return every channel with its residents and subscribers as semantic terminal spans."
  (let* ((world    (handler-case
                       (values (world-reconcile-sessions (application-configuration application)))
                     (error ()
                       (world-load (application-configuration application)))))
         (channels (world-channels world)))
    (if channels
        (append (list (terminal-span ':brand (format nil "channels~%")))
                (loop for channel in channels
                      for identifier = (channel-identifier channel)
                      append (list (terminal-span ':code (conversation-identifier-display identifier))
                                   (terminal-span ':dim
                                                  (format nil "~@[ ~A~]~%  ~A~%  residents: ~A; subscribers: ~A~%"
                                                          (channel-title channel)
                                                          (channel-workspace channel)
                                                          (channel-resource--participants-text
                                                           (world-channel-residents world identifier))
                                                          (channel-resource--participants-text
                                                           (world-channel-subscribers world identifier)))))))
        (list (terminal-span ':hint "No channels exist yet.")))))


;;;; -- Channel Delivery --

(-> application-world--served-channel (application) (option string))
(defun application-world--served-channel (application)
  "Return the identifier of the channel APPLICATION serves, or NIL before its conversation persists."
  (let ((conversation (and (slot-boundp application 'conversation)
                           (application-conversation application))))
    (and conversation
         (conversation-persisted-p conversation)
         (conversation-identifier conversation))))

(-> application-world--unsettled (application string) list)
(defun application-world--unsettled (application channel)
  "Return CHANNEL's deliveries APPLICATION's conversation has not received, settling the rest."
  (let ((configuration (application-configuration application))
        (conversation  (application-conversation application)))
    (multiple-value-bind (pending settled)
        (with-recursive-lock-held ((conversation-append-lock conversation))
          (world-inbox-unsettled configuration channel conversation))
      (when settled
        (world-inbox-settle configuration channel settled)
        (dolist (identifier settled)
          (remhash (format nil "channel:~A" identifier)
                   (application-world-deliveries application))))
      pending)))

(-> application-world-deliver (application) (integer 0))
(defun application-world-deliver (application)
  "Queue every unread message of APPLICATION's channel as an automatic turn and return the count.

A message queued while a turn runs is taken as steering first; the queued
turn then finds its receipt and is skipped. Without an input controller, as
in a headless job, messages wait in the inbox."
  (let ((channel    (application-world--served-channel application))
        (controller (application-input-controller application))
        (queued     0))
    (when (and channel controller)
      (dolist (delivery (application-world--unsettled application channel))
        (let ((identifier (world-delivery-pending-identifier delivery))
              (table      (application-world-deliveries application)))
          (unless (gethash identifier table)
            (when (application-input-controller-enqueue-operation-request
                   controller
                   (world-delivery-label delivery)
                   (world-delivery-request delivery)
                   :pending-input-identifier identifier)
              (setf (gethash identifier table) ':queued)
              (incf queued))))))
    queued))

(-> application-world-steering-take (application) list)
(defun application-world-steering-take (application)
  "Return the unread messages of APPLICATION's channel as labeled steering input.

Each message is taken once; a message already queued as a turn is taken as
well, since the steer persists first and the queued turn then skips."
  (let ((channel (application-world--served-channel application)))
    (if channel
        (world-inbox-steering-take (application-configuration application)
                                   :channel      channel
                                   :conversation (application-conversation application)
                                   :taken        (application-world-deliveries application))
        nil)))

(-> application-world-steering-ack (application string) boolean)
(defun application-world-steering-ack (application identifier)
  "Settle the channel delivery IDENTIFIER names after its durable record, or return NIL for other input."
  (let ((channel (application-world--served-channel application)))
    (or (and channel
             (world-inbox-steering-ack (application-configuration application)
                                       :channel    channel
                                       :identifier identifier
                                       :taken      (application-world-deliveries application)))
        (world-delivery-pending-identifier-p identifier))))

(-> application-world-human-identifier (application) (option string))
(defun application-world-human-identifier (application)
  "Return the identifier of the world's human participant, or NIL before the world has one."
  (or (application-world-human application)
      (let ((person (handler-case
                        (world-human-participant
                         (world-load (application-configuration application)))
                      (world-error ()
                        nil))))
        (and person
             (setf (application-world-human application)
                   (participant-identifier person))))))

(-> application-world-channel-identifier (application string) string)
(defun application-world-channel-identifier (application designator)
  "Return the identifier of the channel DESIGNATOR names in full, by legacy alias or in part."
  (let ((configuration (application-configuration application)))
    (channel-identifier
     (world-channel-designated (values (world-reconcile-sessions configuration))
                               configuration
                               designator))))

(-> application-world-send
    (application &key (:channel string) (:text string) (:mentions list))
    list)
(defun application-world-send (application &key channel text mentions)
  "Send TEXT to CHANNEL as the human participant and deliver it locally when this process serves it."
  (let* ((configuration (application-configuration application))
         (person        (world-human-participant
                         (values (world-reconcile-sessions configuration))))
         (result        (progn
                          (unless person
                            (error 'world-error
                                   :message   "No human participant exists yet; a channel appears with the first durable conversation."
                                   :pathname  nil
                                   :operation ':send))
                          (world-message-send configuration
                                              :sender   (participant-identifier person)
                                              :channel  channel
                                              :text     text
                                              :mentions mentions)))
         (served        (application-world--served-channel application)))
    (when (and served (member served (getf result :channels) :test #'string=))
      (application-world-deliver application))
    result))

(-> application--channel-message-entry (application list (option timestamp)) list)
(defun application--channel-message-entry (application request timestamp)
  "Return the transcript entry showing a channel message with its sender and channel."
  (let* ((message        (getf request :message))
         (name           (sanitize-text (or (getf message :sender-name) "participant")
                                        :single-line-p t))
         (channel        (conversation-identifier-display (or (getf message :target) "?")))
         (timestamp-text (application--timestamp-text timestamp)))
    (append (list (terminal-span ':brand "● ")
                  (terminal-span ':agent-name name)
                  (terminal-span ':dim (format nil " [channel ~A]~@[ ~A~]~%" channel timestamp-text)))
            (application--markdown-body application (or (getf request :summary) "")))))
