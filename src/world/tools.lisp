(in-package #:autolith)

;;;; -- Channel Tools --

;;; channel.send, channel.subscribe and channel.unsubscribe act as the
;;; participant the requesting conversation was migrated onto. Reading
;;; channels is the channel: resource's job.

(defclass channel-tool (tool)
  ()
  (:documentation "A channel operation performed as the requesting conversation's participant."))

(defclass channel-send-tool (channel-tool)
  ()
  (:documentation "A channel message with optional native rich-input expansion."))

(defmethod tool-normalize-local-arguments ((tool channel-send-tool) arguments)
  "Expand rich text and image paths without changing the caller's argument list."
  (let ((input (getf arguments :text)))
    (if (typep input 'user-message-input)
        (progn
            (when (nth-value 0 (get-properties arguments '(:images)))
             (error 'tool-error :tool-name (tool-canonical-name tool) :message
                    "Rich content already supplies its images."))
          (let ((normalized (copy-list arguments)))
            (setf (getf normalized :text) (user-message-input-text input)
                  (getf normalized :images) (user-message-input-image-pathnames input))
            normalized))
        arguments)))

(defmethod tool-child-safe-p ((tool channel-tool))
  "Children are participants too: a task's conversation is a nested channel with its own resident, so it messages, subscribes and unsubscribes like any participant."
  (declare (ignore tool))
  t)

(defmethod tool-execution-policy ((tool channel-tool))
  "Channel operations touch only the world stores, so they run in parallel."
  (declare (ignore tool))
  ':parallel)

(-> channel-tool--sender (tool-context string) participant)
(defun channel-tool--sender (context tool-name)
  "Return the participant acting for CONTEXT's conversation, or signal for TOOL-NAME."
  (declare (special *application-local-user-evaluation-p*))
  (let ((participant (world-session-participant (tool-context-configuration context)
                                                (tool-context-conversation context))))
    (or (if *application-local-user-evaluation-p*
            (world-human-participant (world-load (tool-context-configuration context)))
            participant)
        (error 'tool-error
               :message   "This conversation has no participant yet; it gets one with its first durable message."
               :tool-name tool-name))))

(defparameter *channel-tool-channel-description*
  "The channel: an agent name, @name, exact absolute channel path, identifier or unique ID fragment; inbox names the person's inbox."
  "How every channel tool describes the channel it acts on.")

(-> channel-tool--channel (tool-context hash-table) string)
(defun channel-tool--channel (context arguments)
  "Return the identifier of the channel ARGUMENTS designate, as every channel tool resolves it."
  (let ((configuration (tool-context-configuration context)))
    (channel-identifier
     (world-channel-designated (world-load configuration)
                               configuration
                               (tool-argument arguments "channel" :required t)))))

(-> channel-tool--current-channel (tool-context participant) string)
(defun channel-tool--current-channel (context sender)
  "Register a new current channel for its first send and return its identifier."
  (let* ((configuration (tool-context-configuration context))
         (conversation  (tool-context-conversation context))
         (identifier    (conversation-identifier conversation)))
    (unless (conversation-persisted-p conversation)
      (world--transact
       configuration
       (lambda (world)
         (unless (world-channel-find world identifier)
           (world--require-participant world (participant-identifier sender) ':create-channel)
           (let ((channel
                   (world--new-channel
                    configuration world
                    :home      (list :id        identifier
                                     :workspace (conversation-origin-directory conversation)
                                     :title     (conversation-title conversation)
                                     :parent    (conversation-parent-identifier conversation))
                    :owner     (participant-identifier sender)
                    :now       (conversation-created-at conversation)
                    :operation ':create-channel)))
             (values (list (world--channel-record channel)) channel))))))
    identifier))

(-> channel-tool--send-text (configuration list) string)
(defun channel-tool--send-text (configuration result)
  "Return what the send RESULT of `world-message-send' did, as the sender reads it."
  (let ((world (world-load configuration)))
    (flet ((names (identifiers)
             "Return IDENTIFIERS as the channels' display identifiers."
             (mapcar (lambda (identifier)
                       (let ((channel (world-channel-find world identifier)))
                         (if (and channel (channel-inbox-p channel))
                             "inbox"
                             (conversation-identifier-display identifier))))
                     identifiers)))
      (format nil "Sent message ~A.~@[ Delivered to ~{~A~^, ~}; each resident reads it at its next turn boundary, or wakes for it.~]~@[ Not delivered yet to ~{~A~^, ~}; it is retried with the next message.~]~@[ Parked, so got no copy: ~{@~A~^, ~}.~]"
              (getf result :id)
              (names (getf result :channels))
              (names (getf result :pending))
              (getf result :parked)))))

(defmethod tool-execute ((tool channel-tool) (context tool-context) arguments)
  "Send to, subscribe to or unsubscribe from a channel as the requesting participant."
  (let* ((operation     (tool-operation tool))
         (tool-name     (tool-canonical-name tool))
         (configuration (tool-context-configuration context))
         (sender        (channel-tool--sender context tool-name))
         (channel       (progn
                          (task--validate-tool-arguments
                           arguments
                           (if (eq operation ':send) '("channel" "text" "mentions" "images") '("channel"))
                           tool-name)
                          (if (and (eq operation ':send)
                                   (null (tool-argument arguments "channel")))
                              (channel-tool--current-channel context sender)
                              (channel-tool--channel context arguments))))
         (display       (conversation-identifier-display channel)))
    (case operation
      (:send
       (let ((result (world-message-send configuration
                                         :sender   (participant-identifier sender)
                                         :channel  channel
                                         :text     (tool-argument arguments "text" :required t)
                                         :source   (conversation-identifier (tool-context-conversation context))
                                          :images  (let ((images (tool-argument arguments "images")))
                                                     (mapcar (lambda (path)
                                                               (workspace-tool-path context path
                                                                                    :tool-name tool-name))
                                                             (if (stringp images) (list images)
                                                                 (and images (coerce images 'list)))))
                                         :mentions (let ((mentions (tool-argument arguments "mentions")))
                                                     (and mentions (coerce mentions 'list))))))
         (task-tool-result (channel-tool--send-text configuration result) result)))
      (:subscribe
       (world-participant-subscribe configuration (participant-identifier sender) channel)
       (tool-success (format nil "Subscribed to channel ~A: its messages now reach you." display)))
      (:unsubscribe
       (world-participant-unsubscribe configuration (participant-identifier sender) channel)
       (tool-success (format nil "Unsubscribed from channel ~A." display)))
      (t
       (error 'tool-error
              :message   (format nil "Unknown channel operation ~(~A~)." operation)
              :tool-name tool-name)))))

(-> channel-register-tools (tool-registry) tool-registry)
(defun channel-register-tools (registry)
  "Register the channel tools in REGISTRY."
  (tool-registry-describe-namespace
   registry "channel"
   "Messages to channels and subscriptions, as this conversation's participant.")
  (flet ((register (name description &key (class 'channel-tool) properties required)
           "Register channel tool NAME."
           (tool-registry-register
            registry
             (make-instance class
                           :namespace   "channel"
                           :name        name
                           :description description
                           :parameters  (tool-object-schema properties required)))))
    (register "send"
              "Send a message with optional images. Omit channel to address the current channel. It reaches residents, subscribers and @mentions. @parent reaches your parent agent, or the nearest active ancestor or person."
              :class    'channel-send-tool
              :properties
              (json-object "channel" (tool-string-property *channel-tool-channel-description*)
                           "text" (tool-string-property
                                   (format nil "The message, 1 to ~:D characters."
                                           *world-message-text-limit*))
                            "images" (json-object
                                      "description" "Local image pathnames to attach."
                                      "oneOf" (vector
                                               (json-object "type" "string")
                                               (json-object "type" "array" "items"
                                                            (json-object "type" "string"))))
                           "mentions" (json-object
                                       "type" "array"
                                       "description" "Participants to deliver to as well, by name or identifier."
                                       "items" (json-object "type" "string" "minLength" 1)))
              :required '("text"))
    (register "subscribe"
              "Receive future messages of a channel without residing in it."
              :properties (json-object "channel" (tool-string-property *channel-tool-channel-description*))
              :required   '("channel"))
    (register "unsubscribe"
              "Remove your subscription to a channel."
              :properties (json-object "channel" (tool-string-property *channel-tool-channel-description*))
              :required   '("channel")))
  registry)
