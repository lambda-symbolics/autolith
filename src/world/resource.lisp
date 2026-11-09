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

(defparameter *channel-resource-list-default-results* 40
  "How many channels channel:list shows unless asked for more.")

(defparameter *channel-resource-list-maximum-results* 200
  "The most channels one read of channel:list shows.")

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
    :documentation "The channel's history, or NIL if unavailable."))
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
  "channel:current describes the current channel (residents, subscribers, workspace, your children and whether each is busy) and then reads its history with the conversation: arguments. channel:id/<id> names another channel; channel:list shows the most recently changed channels, filtered by query and bounded by max-results.")

(-> channel-resource--uri (channel-resolver string) string)
(defun channel-resource--uri (resolver identifier)
  "Return the canonical URI of IDENTIFIER under RESOLVER's scheme."
  (format nil "~A:~A" (resource-resolver-scheme resolver) identifier))

(-> channel-resource--world (tool-context) world)
(defun channel-resource--world (context)
  "Return CONTEXT's channel catalogue, including newly hosted channels."
  (world-channel-catalogue (tool-context-configuration context)))

(-> channel-resource--history (tool-context channel) (option conversation))
(defun channel-resource--history (context channel)
  "Return CHANNEL's history through the shared stable storage locator."
  (let ((configuration (tool-context-configuration context))
        (identifier    (channel-identifier channel))
        (current       (tool-context-conversation context)))
    (if (string= identifier (conversation-identifier current))
        current
        (handler-case
         (conversation-replay-load-identity
          (conversation-pathname-for-id configuration identifier))
         (autolith-error nil nil)))))

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


(-> channel-resource--availability (participant list) (option string))
(defun channel-resource--availability (participant served)
  "Return whether agent PARTICIPANT is online, given the SERVED channel identifiers, or NIL for the person.

An agent is online while a process serves the channel it resides in, and
parked while it resides nowhere."
  (let ((residence (participant-residence participant)))
    (cond
      ((eq (participant-kind participant) ':human)
       nil)
      ((null residence)
       "parked")
      ((member residence served :test #'equal)
       "online")
      (t
       "offline"))))

(-> channel-resource--participant-text (participant list) string)
(defun channel-resource--participant-text (participant served)
  "Return PARTICIPANT's name, kind, identifier and availability on one line, given the SERVED channels."
  (format nil "~A (~(~A~) ~A~@[, ~A~])"
          (participant-name participant)
          (participant-kind participant)
          (conversation-identifier-display (participant-identifier participant))
          (channel-resource--availability participant served)))

(-> channel-resource--participants-text (list list) string)
(defun channel-resource--participants-text (participants served)
  "Return PARTICIPANTS as one comma-separated line, or \"none\", given the SERVED channels."
  (if participants
      (format nil "~{~A~^, ~}"
              (mapcar (lambda (participant)
                        (channel-resource--participant-text participant served))
                      participants))
      "none"))

(-> channel-resource--owner-text (world channel) string)
(defun channel-resource--owner-text (world channel)
  "Return CHANNEL's owner as a name and identifier, falling back to its identifier alone."
  (let ((owner (world-participant-find world (channel-owner channel))))
    (if owner
        (format nil "~A (~A)" (participant-name owner)
                (conversation-identifier-display (participant-identifier owner)))
        (conversation-identifier-display (channel-owner channel)))))

(-> channel-resource--state-text (world channel &key (:served list) (:configuration configuration)) string)
(defun channel-resource--state-text (world channel &key served configuration)
  "Return CHANNEL's identity, relationships, participants and children, given the SERVED channels of CONFIGURATION."
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
              (channel-resource--participants-text (world-channel-residents world identifier)
                                                   served))
      (format stream "Subscribers: ~A.~%"
              (channel-resource--participants-text (world-channel-subscribers world identifier)
                                                   served))
      (let ((children (world-channel-children world identifier)))
        (when children
          (format stream "Children, ~D of the ~D an agent may have; send work to one through its channel. A busy child runs or is open, and child.* tools wait until it is idle.~%"
                  (length children) *task-children-maximum*)
          (dolist (child children)
            (write-string (channel-resource--child-text world child configuration) stream)))))))

(-> channel-resource--child-text (world channel configuration) string)
(defun channel-resource--child-text (world channel configuration)
  "Return one line naming the child agent in nested CHANNEL, its role, its channel, and whether it is busy."
  (let* ((identifier (channel-identifier channel))
         (agent      (find ':agent (world-channel-residents world identifier) :key #'participant-kind))
         (pathname   (conversation-task-pathname-for-id configuration identifier))
         (header     (and pathname (rest (conversation-peek-header pathname)))))
    (format nil "  ~A~@[ (~A)~] channel ~A, ~:[idle~;busy~]~%"
            (cond
              (agent
               (participant-name agent))
              ((getf header :agent-name))
              (t
               "no agent"))
            (getf header :role)
            (conversation-identifier-display identifier)
            (conversation-lease-busy-p configuration identifier))))

(-> channel-resource--list-text
    (world list &key (:query (option string)) (:maximum (integer 1)))
    string)
(defun channel-resource--list-text (world served &key query (maximum *channel-resource-list-default-results*))
  "Return up to MAXIMUM channels of WORLD with their residents, most recently changed first, given the SERVED channels.

QUERY keeps the channels whose identifier, title, workspace or participants'
names contain it, ignoring case."
  (let* ((all      (world-channels world))
         (matching (if query
                       (remove-if-not (lambda (channel)
                                        (channel-resource--matches-p world channel query))
                                      all)
                       all))
         (channels (subseq matching 0 (min maximum (length matching)))))
    (with-output-to-string (stream)
      (if (null channels)
          (format stream "~:[No channels exist yet.~;No channel matches ~:*~S.~]~%" query)
          (progn
            (format stream "~D of ~D channel~:P~@[ matching ~S~], most recently changed first.~:[~; Narrow them with query, or raise max-results up to ~D.~] Read channel:id/<id> for one channel's history.~%"
                    (length channels) (length matching) query
                    (< (length channels) (length matching))
                    *channel-resource-list-maximum-results*)
            (dolist (channel channels)
              (let ((identifier (channel-identifier channel)))
                (format stream "~A~@[ ~S~] workspace ~A; residents: ~A; subscribers: ~A~%"
                        (conversation-identifier-display identifier)
                        (channel-title channel)
                        (channel-workspace channel)
                        (channel-resource--participants-text
                         (world-channel-residents world identifier) served)
                        (channel-resource--participants-text
                         (world-channel-subscribers world identifier) served)))))))))


;;;; -- Model-Facing Reads --

(defmethod resource-tool-read
    ((resource channel-list-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return the most recently changed channels, those matching the query argument when given, up to max-results."
  (declare (ignore tool))
  (let ((query   (tool-argument arguments "query"))
        (maximum (or (tool-argument arguments "max-results") *channel-resource-list-default-results*)))
    (unless (or (null query) (non-empty-string-p query))
      (error 'tool-error
             :message   "channel:list query must be non-empty text."
             :tool-name "resource.read"))
    (unless (typep maximum `(integer 1 ,*channel-resource-list-maximum-results*))
      (error 'tool-error
             :message   (format nil "channel:list max-results must be an integer from 1 to ~D."
                                *channel-resource-list-maximum-results*)
             :tool-name "resource.read"))
    (tool-success (channel-resource--list-text
                   (channel-list-resource-world resource)
                   (world-channel-served-identifiers (tool-context-configuration context))
                   :query   query
                   :maximum maximum))))

(-> channel-resource--matches-p (world channel string) boolean)
(defun channel-resource--matches-p (world channel query)
  "Return true when CHANNEL's identifier, title, workspace or participants' names contain QUERY, ignoring case."
  (let ((identifier (channel-identifier channel)))
    (and (some (lambda (text)
                 (and text (search query text :test #'char-equal)))
               (append (list identifier
                             (conversation-identifier-display identifier)
                             (channel-title channel)
                             (channel-workspace channel))
                       (mapcar #'participant-name
                               (append (world-channel-residents world identifier)
                                       (world-channel-subscribers world identifier)))))
         t)))

(defmethod resource-tool-read
    ((resource channel-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return RESOURCE's state followed by its history read with ARGUMENTS."
  (let* ((world        (channel-resource-world resource))
         (channel      (channel-resource-channel resource))
         (conversation (channel-resource-conversation resource))
         (state        (channel-resource--state-text
                        world channel
                        :served        (world-channel-served-identifiers
                                        (tool-context-configuration context))
                        :configuration (tool-context-configuration context))))
    (if conversation
        (let ((history (resource-tool-read
                        (make-instance 'conversation-resource
                                       :uri          (resource-uri resource)
                                       :conversation conversation)
                        tool context arguments)))
          (tool-success (format nil "~A~A" state (tool-result-content history))))
        (tool-success (format nil "~AChannel history is unavailable.~%" state)))))
