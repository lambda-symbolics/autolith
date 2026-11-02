(in-package #:autolith)

;;;; -- Channel Resources --

;;; channel:current describes the channel of the requesting conversation and
;;; reads its history the way conversation:current does. channel:id/<id>
;;; names another channel, and channel:list surveys every channel with its
;;; residents and subscribers. Reads never subscribe the reader.

(defparameter *channel-resource-current-identifier* "current"
  "The identifier naming the requesting conversation's channel.")

(defparameter *channel-resource-list-identifier* "list"
  "The identifier naming the survey of every channel.")

(defparameter *channel-resource-identifier-prefix* "id/"
  "The prefix before a channel identifier.")

(defclass channel-resource (resource)
  ((world
    :initarg       :world
    :reader        channel-resource-world
    :type          world
    :documentation "The world the channel was read from.")
   (channel
    :initarg       :channel
    :reader        channel-resource-channel
    :type          channel
    :documentation "The channel this resource describes.")
   (conversation
    :initarg       :conversation
    :initform      nil
    :reader        channel-resource-conversation
    :type          (option conversation)
    :documentation "The channel's history, or NIL before it has any."))
  (:documentation "The read-only state and history of one channel."))

(defclass channel-list-resource (resource)
  ((world
    :initarg       :world
    :reader        channel-list-resource-world
    :type          world
    :documentation "The world whose channels are listed."))
  (:documentation "The read-only survey of every channel."))

(defclass channel-resolver (resource-resolver)
  ()
  (:documentation "Resolve the current channel, a named channel, or the channel list."))


;;;; -- URI Resolution --

(defmethod resource-resolver-read-documentation ((resolver channel-resolver))
  "Document channel: reads."
  (declare (ignore resolver))
  "channel:current describes the current channel (residents, subscribers, workspace) and then reads its history with the conversation: arguments. channel:id/<id> names another channel; channel:list surveys every channel.")

(-> channel-resource--uri (channel-resolver string) string)
(defun channel-resource--uri (resolver identifier)
  "Return the canonical URI of IDENTIFIER under RESOLVER's scheme."
  (format nil "~A:~A" (resource-resolver-scheme resolver) identifier))

(-> channel-resource--world (tool-context) world)
(defun channel-resource--world (context)
  "Return CONTEXT's world after bringing it in step with the conversations."
  (values (world-reconcile-sessions (tool-context-configuration context))))

(-> channel-resource--history (tool-context channel) (option conversation))
(defun channel-resource--history (context channel)
  "Return the conversation holding CHANNEL's history, or NIL without one.

A child task's history lives in its root conversation's task directory,
which the channel's parent names."
  (let ((configuration (tool-context-configuration context))
        (identifier    (channel-identifier channel))
        (current       (tool-context-conversation context)))
    (cond
      ((string= identifier (conversation-identifier current))
       current)
      (t
       (handler-case
           (conversation-replay-load configuration identifier)
         (autolith-error ()
           (let ((root (channel-parent channel)))
             (and root
                  (handler-case
                      (conversation-replay-load-identity
                       (conversation-task-identity-pathname configuration root identifier))
                    (autolith-error ()
                      nil))))))))))

(-> channel-resource--make
    (channel-resolver &key (:uri-identifier string) (:context tool-context) (:world world)
                           (:identifier string))
    channel-resource)
(defun channel-resource--make (resolver &key uri-identifier context world identifier)
  "Return the resource of channel IDENTIFIER in WORLD, named URI-IDENTIFIER under CONTEXT."
  (let ((channel (world-channel-find world identifier)))
    (unless channel
      (error 'tool-error
             :message   (format nil "Channel ~A does not exist. Read channel:list for the channels there are."
                                (conversation-identifier-display identifier))
             :code      ':not-found
             :tool-name "resource.read"))
    (make-instance 'channel-resource
                   :uri          (channel-resource--uri resolver uri-identifier)
                   :world        world
                   :channel      channel
                   :conversation (channel-resource--history context channel))))

(defmethod resource-resolver-resolve
    ((resolver channel-resolver) identifier (context tool-context))
  "Resolve channel:current, channel:list, or one named channel:id/<id>."
  (let ((prefix *channel-resource-identifier-prefix*))
    (cond
      ((string= identifier *channel-resource-list-identifier*)
       (make-instance 'channel-list-resource
                      :uri   (channel-resource--uri resolver identifier)
                      :world (channel-resource--world context)))
      ((string= identifier *channel-resource-current-identifier*)
       (channel-resource--make resolver
                               :uri-identifier identifier
                               :context        context
                               :world          (channel-resource--world context)
                               :identifier     (conversation-identifier
                                                (tool-context-conversation context))))
      ((and (> (length identifier) (length prefix))
            (string= prefix identifier :end2 (length prefix)))
       (let* ((world (channel-resource--world context))
              (named (channel-identifier
                      (handler-case
                          (world-channel-designated world
                                                    (tool-context-configuration context)
                                                    (subseq identifier (length prefix)))
                        (world-error (condition)
                          (error 'tool-error
                                 :message   (autolith-error-message condition)
                                 :code      ':not-found
                                 :tool-name "resource.read"))))))
         (channel-resource--make resolver
                                 :uri-identifier (concatenate 'string prefix named)
                                 :context        context
                                 :world          world
                                 :identifier     named)))
      (t
       (error 'resource-operation-unsupported
              :uri       (channel-resource--uri resolver identifier)
              :operation ':resolve)))))

(defmethod resource-capabilities
    ((resource channel-resource) (context tool-context))
  "Expose channel state and history as read-only."
  (declare (ignore resource context))
  '(:read))

(defmethod resource-capabilities
    ((resource channel-list-resource) (context tool-context))
  "Expose the channel survey as read-only."
  (declare (ignore resource context))
  '(:read))


;;;; -- Rendering --

(-> channel-resource--participant-text (participant) string)
(defun channel-resource--participant-text (participant)
  "Return PARTICIPANT's name, kind, identifier and presence on one line."
  (format nil "~A (~(~A~) ~A, ~(~A~))"
          (participant-name participant)
          (participant-kind participant)
          (conversation-identifier-display (participant-identifier participant))
          (participant-presence participant)))

(-> channel-resource--participants-text (list) string)
(defun channel-resource--participants-text (participants)
  "Return PARTICIPANTS as one comma-separated line, or \"none\"."
  (if participants
      (format nil "~{~A~^, ~}" (mapcar #'channel-resource--participant-text participants))
      "none"))

(-> channel-resource--owner-text (world channel) string)
(defun channel-resource--owner-text (world channel)
  "Return CHANNEL's owner as a name and identifier."
  (let ((owner (world-participant-find world (channel-owner channel))))
    (if owner
        (format nil "~A (~A)" (participant-name owner)
                (conversation-identifier-display (participant-identifier owner)))
        (conversation-identifier-display (channel-owner channel)))))

(-> channel-resource--state-text (world channel) string)
(defun channel-resource--state-text (world channel)
  "Return CHANNEL's identity, relationships and participants."
  (let ((identifier (channel-identifier channel)))
    (with-output-to-string (stream)
      (format stream "Channel ~A~@[ ~S~], workspace ~A, owner ~A~@[, nested under ~A~], created ~A, changed ~A.~%"
              (conversation-identifier-display identifier)
              (channel-title channel)
              (channel-workspace channel)
              (channel-resource--owner-text world channel)
              (let ((parent (channel-parent channel)))
                (and parent (conversation-identifier-display parent)))
              (conversation-replay-timestamp-string (channel-created-at channel))
              (conversation-replay-timestamp-string (channel-updated-at channel)))
      (format stream "Residents: ~A.~%"
              (channel-resource--participants-text (world-channel-residents world identifier)))
      (format stream "Subscribers: ~A.~%"
              (channel-resource--participants-text (world-channel-subscribers world identifier)))
      (let ((children (world-channel-children world identifier)))
        (when children
          (format stream "Nested channels: ~{~A~^, ~}.~%"
                  (mapcar (lambda (child)
                            (conversation-identifier-display (channel-identifier child)))
                          children)))))))

(-> channel-resource--list-text (world) string)
(defun channel-resource--list-text (world)
  "Return every channel of WORLD with its residents, most recently changed first."
  (let ((channels (world-channels world)))
    (with-output-to-string (stream)
      (if (null channels)
          (format stream "No channels exist yet.~%")
          (progn
            (format stream "~D channel~:P, most recently changed first. Read channel:id/<id> for one channel's history.~%"
                    (length channels))
            (dolist (channel channels)
              (let ((identifier (channel-identifier channel)))
                (format stream "~A~@[ ~S~] workspace ~A; residents: ~A; subscribers: ~A~%"
                        (conversation-identifier-display identifier)
                        (channel-title channel)
                        (channel-workspace channel)
                        (channel-resource--participants-text
                         (world-channel-residents world identifier))
                        (channel-resource--participants-text
                         (world-channel-subscribers world identifier))))))))))


;;;; -- Model-Facing Reads --

(defmethod resource-tool-read
    ((resource channel-list-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return the survey of every channel."
  (declare (ignore tool context arguments))
  (tool-success (channel-resource--list-text (channel-list-resource-world resource))))

(defmethod resource-tool-read
    ((resource channel-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return RESOURCE's state followed by its history read with ARGUMENTS."
  (let* ((world        (channel-resource-world resource))
         (channel      (channel-resource-channel resource))
         (conversation (channel-resource-conversation resource))
         (state        (channel-resource--state-text world channel)))
    (if conversation
        (let ((history (resource-tool-read
                        (make-instance 'conversation-resource
                                       :uri          (resource-uri resource)
                                       :conversation conversation)
                        tool context arguments)))
          (tool-success (format nil "~A~A" state (tool-result-content history))))
        (tool-success (format nil "~ANo history yet.~%" state)))))
