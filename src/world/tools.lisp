(in-package #:autolith)

;;;; -- Channel Tools --

;;; channel.send, channel.subscribe and channel.unsubscribe act as the
;;; participant the requesting conversation was migrated onto. Reading
;;; channels is the channel: resource's job.

(defclass channel-tool (tool)
  ()
  (:documentation "A channel operation performed as the requesting conversation's participant."))

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
  (or (world-session-participant (tool-context-configuration context)
                                 (tool-context-conversation context))
      (error 'tool-error
             :message   "This conversation has no participant yet; it gets one with its first durable message."
             :tool-name tool-name)))

(-> channel-tool--channel (tool-context hash-table) string)
(defun channel-tool--channel (context arguments)
  "Return the canonical channel identifier ARGUMENTS name."
  (conversation-identifier-migration-resolve
   (tool-context-configuration context)
   (tool-argument arguments "channel" :required t)))

(defmethod tool-execute ((tool channel-tool) (context tool-context) arguments)
  "Send to, subscribe to or unsubscribe from a channel as the requesting participant."
  (let* ((name          (tool-name tool))
         (tool-name     (format nil "channel.~A" name))
         (configuration (tool-context-configuration context))
         (sender        (channel-tool--sender context tool-name))
         (result
           (cond
             ((string= name "send")
              (task--validate-tool-arguments arguments '("channel" "text" "mentions") tool-name)
              (world-message-send configuration
                                  :sender   (participant-identifier sender)
                                  :channel  (channel-tool--channel context arguments)
                                  :text     (tool-argument arguments "text" :required t)
                                  :mentions (let ((mentions (tool-argument arguments "mentions")))
                                              (and mentions (coerce mentions 'list)))))
             ((string= name "subscribe")
              (task--validate-tool-arguments arguments '("channel") tool-name)
              (let ((channel (channel-tool--channel context arguments)))
                (world-participant-subscribe configuration (participant-identifier sender) channel)
                (list :subscribed channel :participant (participant-identifier sender))))
             ((string= name "unsubscribe")
              (task--validate-tool-arguments arguments '("channel") tool-name)
              (let ((channel (channel-tool--channel context arguments)))
                (world-participant-unsubscribe configuration (participant-identifier sender) channel)
                (list :unsubscribed channel :participant (participant-identifier sender))))
             (t
              (error 'tool-error
                     :message   (format nil "Unknown channel operation ~A." name)
                     :tool-name tool-name)))))
    (task-tool-result (task--write-readable-sexp result :pretty-p t) result)))

(-> channel-register-tools (tool-registry) tool-registry)
(defun channel-register-tools (registry)
  "Register the channel tools in REGISTRY."
  (tool-registry-describe-namespace
   registry "channel"
   "Messages to channels and subscriptions, as this conversation's participant.")
  (flet ((register (name description properties required)
           "Register channel tool NAME."
           (tool-registry-register
            registry
            (make-instance 'channel-tool
                           :namespace   "channel"
                           :name        name
                           :description description
                           :parameters  (tool-object-schema properties required)))))
    (register "send"
              "Send a message to a channel. It reaches the channel's residents, its subscribers and every participant mentioned as @name or @id. @parent reaches your parent agent; when it is parked, the nearest ancestor that is not, or the person. Read channel:list for channels and participants."
              (json-object "channel" (tool-string-property "The channel identifier.")
                           "text" (tool-string-property
                                   (format nil "The message, 1 to ~:D characters."
                                           *world-message-text-limit*))
                           "mentions" (json-object
                                       "type" "array"
                                       "description" "Participants to deliver to as well, by name or identifier."
                                       "items" (json-object "type" "string" "minLength" 1)))
              '("channel" "text"))
    (register "subscribe"
              "Receive future messages of a channel without residing in it."
              (json-object "channel" (tool-string-property "The channel identifier."))
              '("channel"))
    (register "unsubscribe"
              "Remove your subscription to a channel."
              (json-object "channel" (tool-string-property "The channel identifier."))
              '("channel")))
  registry)
