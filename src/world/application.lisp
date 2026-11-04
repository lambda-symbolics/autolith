(in-package #:autolith)

;;;; -- World Hooks of the Application --

;;; The application keeps the world in step with the conversations it opens.
;;; The reconciliation hook is release machinery that leaves with
;;; src/world/sessions.lisp.

(defvar *world-selection-p* t
  "Whether reconciling an application's conversation also moves the person into its channel.

True in a terminal running its own session, where the conversation on screen
is the person's choice. The world server binds it to NIL: the hosts it
builds and the conversations they install are not where the person is, and
only a terminal's own attachment or switch moves the person.")

(-> application-world-sync (application &key (:all-p boolean)) null)
(defun application-world-sync (application &key all-p)
  "Reconcile the active conversation, or every conversation with ALL-P, and select its channel.

The selection moves the person only while *WORLD-SELECTION-P* holds.
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
          (when (and conversation *world-selection-p*)
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

(-> application-world--unsettled (application string) (values list list))
(defun application-world--unsettled (application channel)
  "Return CHANNEL's deliveries APPLICATION's conversation has not received, settling the rest.

The second value lists the identifiers settled, so the caller knows the
inbox made room."
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
      (values pending settled))))

(-> application-world-deliver (application) (integer 0))
(defun application-world-deliver (application)
  "Queue every unread message of APPLICATION's channel as an automatic turn and return the count.

A message queued while a turn runs is taken as steering first; the queued
turn then finds its receipt and is skipped. Without an input controller, as
in a headless job, messages wait in the inbox. Settling received messages
makes room in the inbox, so a send that found it full is resumed right
after, rather than waiting for the next send to come by."
  (let ((channel    (application-world--served-channel application))
        (controller (application-input-controller application))
        (queued     0))
    (when (and channel controller)
      (world-message-resume (application-configuration application))
      (dolist (delivery (multiple-value-bind (pending settled)
                            (application-world--unsettled application channel)
                          (if settled
                              (progn
                                (world-message-resume (application-configuration application))
                                (values (application-world--unsettled application channel)))
                              pending)))
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

(-> world-mention-completion-entries (world string) list)
(defun world-mention-completion-entries (world token)
  "Return completion entries for the participants of WORLD whose names start with TOKEN."
  (loop for participant in (sort (copy-list (world-participants world)) #'string-lessp
                                 :key #'participant-name)
        for name = (participant-name participant)
        when (and (world-handle-p name)
                  (uiop:string-prefix-p (string-downcase token) (string-downcase name)))
          collect (let ((channel (world-channel-find world (participant-residence participant))))
                    (list :name        (format nil "@~A" name)
                          :description (format nil "~(~A~) in ~A~@[ ~A~]"
                                               (participant-kind participant)
                                               (conversation-identifier-display
                                                (participant-residence participant))
                                               (and channel (channel-title channel)))))))

(-> application-world-mention-function (configuration) function)
(defun application-world-mention-function (configuration)
  "Return a function completing @ participant names from CONFIGURATION's world as it is."
  (let ((cache nil))
    (lambda (token)
      (setf cache (world--cached configuration cache))
      (world-mention-completion-entries (rest cache) token))))

(-> world--source-token (configuration) list)
(defun world--source-token (configuration)
  "Return what identifies the content of CONFIGURATION's world log: its write date and size.

The size tells two writes within one second apart. An absent log is the
empty token."
  (let ((pathname (configuration-world-path configuration)))
    (if (probe-file pathname)
        (list (file-write-date pathname)
              (with-open-file (stream pathname :element-type '(unsigned-byte 8))
                (file-length stream)))
        nil)))

(-> world--cached (configuration (option cons)) cons)
(defun world--cached (configuration cache)
  "Return CACHE, a (TOKEN . WORLD) pair, while its token is the log's, else a fresh pair.

The token is read before and after the world, and the world is read again
until both agree, so a write during the read never pairs an old world with a
new token. A world that cannot be read presents as empty."
  (let ((token (world--source-token configuration)))
    (if (and cache (equal (first cache) token))
        cache
        (loop for attempt from 1 to 3
              for before = token then after
              for world = (handler-case (world-load configuration)
                            (autolith-error ()
                              (make-instance 'world)))
              for after = (world--source-token configuration)
              when (or (equal before after) (= attempt 3))
                return (cons after world)))))

(-> application-world (application) world)
(defun application-world (application)
  "Return the world as APPLICATION last read it for presentation, rereading it once it changed."
  (let ((cache (world--cached (application-configuration application)
                              (application-world-cache application))))
    (setf (application-world-cache application) cache)
    (rest cache)))

(-> application-world-mention-spans (application list) list)
(defun application-world-mention-spans (application spans)
  "Return SPANS with every @mention of a participant styled: the person's own in the user style, others accented.

The world is read only when some span holds an @, so plain text costs
nothing."
  (if (notany (lambda (span)
                (and (consp span) (stringp (rest span)) (find #\@ (rest span))))
              spans)
      spans
      (let ((world (application-world application)))
        (if (null (world-participants world))
            spans
            (loop for span in spans
                  append (if (and (consp span) (stringp (rest span)))
                             (world-mention--split-span world span)
                             (list span)))))))

(-> world-mention--split-span (world cons) list)
(defun world-mention--split-span (world span)
  "Return SPAN cut around the @mentions its text resolves in WORLD, each styled.

The pieces concatenate back to SPAN's text exactly: a token that names no
participant is left in place, only the scan moves past it."
  (let ((style   (first span))
        (text    (rest span))
        (pieces  nil)
        (emitted 0)
        (scan    0))
    (loop
      (let ((at (position #\@ text :start scan)))
        (unless at
          (return))
        (let* ((end         (world-mention-end text at))
               (designator  (subseq text (1+ at) end))
               (participant (and (world-mention-boundary-p text at)
                                 (plusp (length designator))
                                 (world-message-participant world designator))))
          (if participant
              (progn
                (when (> at emitted)
                  (push (cons style (subseq text emitted at)) pieces))
                (push (terminal-span (if (eq (participant-kind participant) ':human)
                                         ':user
                                         ':legend-accent)
                                     (subseq text at end))
                      pieces)
                (setf emitted end
                      scan    end))
              (setf scan (max end (1+ at)))))))
    (when (< emitted (length text))
      (push (cons style (subseq text emitted)) pieces))
    (if pieces
        (nreverse pieces)
        (list span))))

(-> application-world-attached (application) null)
(defun application-world-attached (application)
  "Move the person into APPLICATION's channel: a terminal just took control of it.

Only a channel the world server hosts moves anyone, and only once the
attachment is admitted; a direct session selected its conversation when it
opened, and a terminal whose attachment fails moves nobody. A world that
cannot be read only warns."
  (when (and (application-world-server application)
             (slot-boundp application 'configuration))
    (let ((channel       (application-world--served-channel application))
          (configuration (application-configuration application)))
      (when channel
        (handler-case
            (progn
              ;; A conversation that persisted since the last reconciliation
              ;; gets its channel first, so there is one to move into.
              (world-reconcile-sessions configuration
                                        :identities (list (conversation-pathname
                                                           (application-conversation application))))
              (world-session-select configuration channel))
          (error (cause)
            (warn 'world-session-warning
                  :pathname (configuration-world-sessions-path
                             (application-configuration application))
                  :cause    cause))))))
  nil)

(-> application-world-share (application t) list)
(defun application-world-share (application input)
  "Share INPUT, the person's message to this channel, with the participants elsewhere it reaches, and return the channels told.

The message itself goes to this channel's agent as the person's turn. A
copy goes to every subscriber of this channel residing elsewhere, and to a
participant residing elsewhere that it names as @name or @id. The share runs
once, when the person submits, so a replay of the turn after a restart
never sends a second copy. A delivery that fails is reported and never
stops the turn."
  (let ((text (cond
                ((stringp input)
                 input)
                ((typep input 'user-message-input)
                 (user-message-input-text input))
                (t
                 nil))))
    (when (and (non-empty-string-p text)
               (slot-boundp application 'configuration))
      (handler-case
          (let* ((configuration (application-configuration application))
                 (channel       (application-world--served-channel application))
                 (world         (and channel (values (world-reconcile-sessions configuration))))
                 (person        (and world (world-human-participant world)))
                 (elsewhere     (and person
                                     (find #\@ text)
                                     (remove-if (lambda (identifier)
                                                  (let ((mentioned (world-participant-find
                                                                    world identifier)))
                                                    (or (null mentioned)
                                                        (string= (participant-residence mentioned)
                                                                 channel))))
                                                (world-message-mentions world text))))
                 (subscribed    (and person
                                     (some (lambda (subscriber)
                                             (not (string= (participant-residence subscriber)
                                                           channel)))
                                           (world-channel-subscribers world channel)))))
            (when (or elsewhere subscribed)
              (getf (world-message-send configuration
                                        :sender        (participant-identifier person)
                                        :channel       channel
                                        :text          text
                                        :mentions      elsewhere
                                        :post-p        nil
                                        :subscribers-p t)
                    :channels)))
        (autolith-error (condition)
          (application-present application
                               (format nil "The message was not shared: ~A"
                                       (bounded-string (princ-to-string condition) :limit 300)))
          nil)))))

(-> application-world-share-reply (application t) list)
(defun application-world-share-reply (application text)
  "Share TEXT, this channel's agent's reply, with the channel's subscribers elsewhere, and return the channels told.

The reply was said in this channel already; a subscriber residing elsewhere
gets a copy, and so does a participant elsewhere the reply mentions. A
reply that is not verbal text, or a channel without an agent of its own,
shares nothing. A delivery that fails is reported and never stops the turn."
  (when (and (non-empty-string-p text)
             (slot-boundp application 'configuration))
    (handler-case
        (let* ((configuration (application-configuration application))
               (channel       (application-world--served-channel application))
               (world         (and channel (values (world-reconcile-sessions configuration))))
               (agent         (and world
                                   (find ':agent (world-channel-residents world channel)
                                         :key #'participant-kind)))
               (elsewhere     (and agent
                                   (find #\@ text)
                                   (remove-if (lambda (identifier)
                                                (let ((mentioned (world-participant-find
                                                                  world identifier)))
                                                  (or (null mentioned)
                                                      (string= (participant-residence mentioned)
                                                               channel))))
                                              (world-message-mentions world text))))
               (subscribed    (and agent
                                   (some (lambda (subscriber)
                                           (not (string= (participant-residence subscriber)
                                                         channel)))
                                         (world-channel-subscribers world channel)))))
          (when (or elsewhere subscribed)
            (getf (world-message-send configuration
                                      :sender        (participant-identifier agent)
                                      :channel       channel
                                      :text          text
                                      :mentions      elsewhere
                                      :post-p        nil
                                      :subscribers-p t)
                  :channels)))
      (autolith-error (condition)
        (application-present application
                             (format nil "The reply was not shared: ~A"
                                     (bounded-string (princ-to-string condition) :limit 300)))
        nil))))

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
            (application-world-mention-spans
             application
             (application--markdown-body application (or (getf request :summary) ""))))))
