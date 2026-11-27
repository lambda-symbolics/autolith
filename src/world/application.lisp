(in-package #:autolith)

;;;; -- World Hooks of the Application --

;;; The application keeps the world in step with the conversation it hosts.

(defparameter *world-agent-name* "autolith"
  "The display name for an application without a named resident.")

(-> application-hosted-p (application) boolean)
(defun application-hosted-p (application)
  "Return true when a world server hosts APPLICATION beside other channels."
  (not (null (application-world-server application))))

(-> application-world-sync (application) null)
(defun application-world-sync (application)
  "Register APPLICATION's durable conversation and refresh its participant projection."
  (let* ((configuration (application-configuration application))
         (conversation  (and (slot-boundp application 'conversation)
                             (application-conversation application))))
    (handler-case
        (when conversation
          (setf (participant-state-identity (application-participant-state application))
                (world-register-conversation configuration conversation)))
      (error (cause)
        (warn 'world-session-warning :pathname (configuration-world-path configuration) :cause cause))))
  nil)

(-> application-world-sync-child (application list) null)
(defun application-world-sync-child (application event)
  "Assign the registered child participant to the task named by lifecycle EVENT."
  (let ((execution (getf event :execution))
        (runtime   (application--task-orchestrator application)))
    (when (and runtime (stringp execution))
      (let* ((job   (find (getf event :id) (task-orchestrator-list-jobs runtime)
                          :test #'equal :key #'job-identifier))
             (state (and job (task-job-participant-state job))))
        (when state
          (handler-case
              (let* ((world (world-load (application-configuration application)))
                     (agent (world-participant-find world execution)))
                (when (and agent (equal execution (participant-residence agent)))
                  (setf (participant-state-identity state) agent)))
            (error (cause)
              (warn 'world-session-warning
                    :pathname (configuration-world-path (application-configuration application))
                    :cause    cause)))))))
  nil)


;;;; -- Channel Delivery --

(-> application-world--served-channel (application) (option string))
(defun application-world--served-channel (application)
  "Return the identifier of the channel APPLICATION serves, or NIL before its conversation persists."
  (let ((conversation (and (slot-boundp application 'conversation)
                           (application-conversation application))))
    (and conversation
         (conversation-persisted-p conversation)
         (conversation-identifier conversation))))

(-> application-world-notify-idle (application) null)
(defun application-world-notify-idle (application)
  "Send the channel's idle wakeups without interrupting its input loop on delivery failure."
  (when-let ((channel (application-world--served-channel application)))
    (let ((configuration (application-configuration application)))
      (handler-case
          (world-message-idle-wakeups configuration channel)
        (error (cause)
          (warn 'world-session-warning :pathname (configuration-world-path configuration) :cause cause)))))
  nil)

(-> application-world-inbox-p (application) boolean)
(defun application-world-inbox-p (application)
  "Return true when APPLICATION's conversation is the person's inbox."
  (let ((conversation (and (slot-boundp application 'conversation)
                           (application-conversation application))))
    (and conversation (conversation-inbox-p conversation))))

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
          (remhash (world-delivery-receipt identifier)
                   (application-world-deliveries application))))
      (values pending settled))))

(-> application-world-deliver (application) (integer 0))
(defun application-world-deliver (application)
  "Queue every unread message of APPLICATION's channel as an automatic turn and return the count.

In the person's inbox every message is for the person: no agent takes it,
and it is recorded in the inbox's history once a terminal shows the inbox;
until then it waits, unread. A message queued while a turn runs is taken as
steering first; the queued turn then finds its receipt and is skipped.
Without an input controller, as in a headless job, or without a resident
agent to read them, messages wait in the inbox. Settling received messages
makes room in the inbox, so a send that found it full is resumed right
after, rather than waiting for the next send to come by."
  ;; The pooled observer alone consumes assignment inbox steering and receipts.
  (when (application-assignment-job application)
    (return-from application-world-deliver 0))
  (let ((channel       (application-world--served-channel application))
        (controller    (application-input-controller application))
        (configuration (application-configuration application))
        (queued        0))
    (when channel
      (world-message-resume configuration)
      (let ((pending (multiple-value-bind (pending settled)
                         (application-world--unsettled application channel)
                       (if settled
                           (progn
                             (world-message-resume configuration)
                             (values (application-world--unsettled application channel)))
                           pending))))
        (when (and pending
                   (application-world-inbox-p application)
                   (application-terminal-attached-p application))
          (application-world--record-notices application channel pending))
        (when (and controller
                   (not (application-world-inbox-p application))
                   (application-world-agent-present-p application))
          (dolist (delivery pending)
            (let ((identifier (world-delivery-pending-identifier delivery))
                  (table      (application-world-deliveries application)))
              (unless (gethash identifier table)
                (when (application-input-controller-enqueue-operation-request
                       controller
                       (world-delivery-input delivery)
                       (world-delivery-request delivery)
                       :pending-input-identifier identifier)
                  (setf (gethash identifier table) ':queued)
                  (incf queued))))))))
    queued))

(-> application-world--record-notices (application string list) null)
(defun application-world--record-notices (application channel notices)
  "Record NOTICES, messages for the person in CHANNEL's inbox, in the inbox's history, show them and settle them.

Each is a durable automatic message carrying its pending-input identifier,
so the history is its receipt: a crash after the record and before the
settlement settles it on the next delivery instead of showing it twice."
  (let ((conversation (application-conversation application)))
    (dolist (notice notices)
      (let ((record (nth-value 1 (conversation-append-user-message
                                  conversation (world-delivery-input notice)
                                  :automatic-p              t
                                  :pending-input-identifier (world-delivery-pending-identifier notice)
                                  :operation-request        (world-delivery-request notice)))))
        (when (application-ui application)
          (application--present-conversation-record application record))))
    (world-inbox-settle (application-configuration application) channel
                        (mapcar #'world-delivery-identifier notices))
    (world-message-resume (application-configuration application))
    (if-let ((server (application-world-server application)))
        (world-server-person-unread-changed server)
        (application-world-show-person-unread application 0)))
  nil)

(-> application-world-notify-person (configuration) null)
(defun application-world-notify-person (configuration)
  "Tell the world server that the person's inbox changed, so every channel it serves shows the unread count.

Inside the server, where the active application knows it, the change is
shown directly; elsewhere the server is asked over its endpoint. Without a
server nothing shows the count, and the next channel a terminal opens counts
again."
  (let* ((application *active-application*)
         (server      (and application (application-world-server application))))
    (if server
        (world-server-person-unread-changed server)
        (handler-case
            (when-let ((entry (world-client-entry configuration)))
              (world-terminal-query-record entry ':person-unread))
          (autolith-error ()
            ;; A checkpointing or stopping server counts again on the next attach.
            nil))))
  nil)

(-> application-world-show-person-unread (application (integer 0)) null)
(defun application-world-show-person-unread (application unread)
  "Show UNREAD, the messages waiting in the person's inbox, on APPLICATION's modeline."
  (setf (application-world-inbox-unread application) unread)
  (when-let ((ui (and (slot-boundp application 'ui)
                      (application-ui application))))
    (terminal-ui-set-idle-status-details ui (application--status-details application)))
  nil)

(-> application-world-steering-take (application) list)
(defun application-world-steering-take (application)
  "Return the unread messages of APPLICATION's channel as labeled steering input.

Each message is taken once; a message already queued as a turn is taken as
well, since the steer persists first and the queued turn then skips."
  (if-let ((channel (application-world--served-channel application)))
      (world-inbox-steering-take (application-configuration application)
                                 :channel      channel
                                 :conversation (application-conversation application)
                                 :taken        (application-world-deliveries application))
      nil))

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
  "Return completion entries for the participants of WORLD that TOKEN matches, best first.

TOKEN matches a name or the title of the participant's channel loosely: its
letters in order. Without a TOKEN, offer active participants by name."
  (flet ((channel (participant)
           (let ((residence (participant-residence participant)))
             (and residence (world-channel-find world residence)))))
    (loop for participant in (sito:rank
                              token
                              (sort (world--participants-where
                                     world
                                     (lambda (participant)
                                        (and (participant-residence participant)
                                             (world-handle-p (participant-name participant)))))
                                    #'string-lessp
                                    :key #'participant-name)
                              :key (lambda (participant)
                                     (let ((channel (channel participant)))
                                       (list (participant-name participant)
                                             (and channel (channel-title channel))))))
          for residence = (participant-residence participant)
          for channel = (channel participant)
          collect (list :name        (format nil "@~A" (participant-name participant))
                        :description (format nil "~(~A~) in ~A~@[ ~A~]"
                                             (participant-kind participant)
                                             (conversation-identifier-display residence)
                                             (and channel (channel-title channel)))))))

(-> application-world-mention-function (configuration) function)
(defun application-world-mention-function (configuration)
  "Return a participant completer using the local world, without channel-status RPC."
  (lambda (token)
    (handler-case
        (world-mention-completion-entries (world-load configuration) token)
      (autolith-error () nil))))

(-> world-presented (configuration) world)
(defun world-presented (configuration)
  "Return CONFIGURATION's channel catalogue, or an empty world when unreadable."
  (handler-case (world-channel-catalogue configuration)
                (autolith-error nil (make-instance 'world))))

(-> application-world (application) world)
(defun application-world (application)
  "Return APPLICATION's world for presentation, empty when it cannot be read."
  (world-presented (application-configuration application)))

(-> application-agent-name (application) string)
(defun application-agent-name (application)
  "Return the name of the agent residing in APPLICATION's channel, for the transcript's headers.

Outside a channel, without a resident agent, or when the world cannot be read,
the name is autolith."
  (let ((channel (application-world--served-channel application)))
    (or (and channel
             (handler-case
                 ;; Residents are durable, so the stored world answers without
                 ;; asking the server for the live catalogue on every header.
                 (let ((agent (find ':agent (world-channel-residents
                                             (world-load (application-configuration application)) channel)
                                    :key #'participant-kind)))
                   (and agent (participant-name agent)))
               (autolith-error ()
                 nil)))
        *world-agent-name*)))

(-> application--mention-resolver (application) function)
(defun application--mention-resolver (application)
  "Resolve displayed participant designators against one lazily read world."
  (let ((world nil))
    (lambda (name)
      (world-mention--role (or world (setf world (application-world application))) name))))

(-> world-mention--role (world string) (option keyword))
(defun world-mention--role (world name)
  "Return the transcript style of participant NAME in WORLD, if present."
  (when-let ((participant (world-message-participant world name)))
    (if (eq (participant-kind participant) ':human) ':user ':legend-accent)))

(-> world-mention--split-span (world cons) list)
(defun world-mention--split-span (world span)
  "Return SPAN cut around the @mentions its text resolves in WORLD."
  (let ((text (rest span)))
    (terminal-ui--mention-item span text 0 :end    (length text)
                                          :resolve (lambda (name) (world-mention--role world name)))))

(-> application--message-mentions (string function) list)
(defun application--message-mentions (source resolve)
  "Keep only resolved designators from SOURCE for world-independent reflow.

Without escapes, raw tokens suffice as candidates; the decorator checks actual
prose and word boundaries. With escapes, collect the parser's visible tokens
as well, so a literal backslash cannot hide the end of an escaped designator."
  (when (find #\@ source)
    (let ((seen     (make-hash-table :test #'equal))
          (mentions nil))
      (flet ((remember (name)
               "Remember NAME's portable style once without changing the rendered text."
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (when-let ((role (funcall resolve name))) (push (list name role) mentions)))
               nil))
        (loop for at = (position #\@ source) then (position #\@ source :start end)
              while at
              for end = (world-mention-end source at)
              when (> end (1+ at)) do (remember (subseq source (1+ at) end)))
        (when (find #\\ source)
          (termdown:markdown-render-chunk
           (terminal-ui--markdown-renderer 80 :resolve #'remember) source :finished-p t)))
      (nreverse mentions))))

(-> application-world-attached (application) null)
(defun application-world-attached (application)
  "Map APPLICATION's conversation onto its channel and deliver what waits: a terminal just took control of it.

Taking control moves nobody; the person resides in the inbox. Messages
waiting in the inbox show now that a terminal shows it, and any other
channel shows on its modeline how many wait there. A world that cannot be
read only warns."
  (when (and (application-world-server application)
             (slot-boundp application 'configuration))
    (let ((channel       (application-world--served-channel application))
          (configuration (application-configuration application)))
      (when channel
        (handler-case
            (progn
              (application-world-sync application)
              (application-world-deliver application)
              (application-world-show-person-unread application
                                                    (world-person-unread configuration)))
          (error (cause)
            (warn 'world-session-warning
                  :pathname (configuration-world-path configuration)
                  :cause    cause))))))
  nil)

(-> application-world--share-elsewhere
    (configuration world &key (:sender participant) (:channel string) (:text string)
                              (:source (option string)))
    list)
(defun application-world--share-elsewhere (configuration world &key sender channel text source)
  "Send TEXT to the participants it mentions outside CHANNEL, and return the channels told.

SOURCE, when supplied, identifies where the sender spoke; otherwise delivery
uses the sender's residence. The local turn already records the message."
  (let ((mentions (and (find #\@ text)
                       (remove-if (lambda (identifier)
                                    (let ((participant (world-participant-find world identifier)))
                                      (or (null participant)
                                          (equal (participant-residence participant) channel))))
                                  (world-message-mentions world text)))))
    (when mentions
      (getf (apply #'world-message-send configuration
                   :sender   (participant-identifier sender)
                   :channel  channel
                   :text     text
                   :mentions mentions
                   :post-p   nil
                   (when source (list :source source)))
            :channels))))

(-> application-world-share (application t) list)
(defun application-world-share (application input)
  "Share INPUT, the person's message to this channel, with the participants elsewhere it reaches, and return the channels told.

The message itself goes to this channel's agent as the person's turn. A
participant residing elsewhere gets a copy if the person names it as @name or
@id. The share runs once, when the person submits, so a replay of the turn after
a restart never sends a second copy. A delivery failure does not stop the turn."
  (let ((text (cond
                ((stringp input)
                 input)
                ((typep input 'user-message-input)
                 (user-message-input-text input))
                (t
                 nil))))
    (when (and (non-empty-string-p text)
               (slot-boundp application 'configuration))
      ;; A conversation with nothing durable yet has no channel to share
      ;; from; the share waits for the message to persist.
      (when (and (null (application-world--served-channel application))
                 (slot-boundp application 'conversation)
                 (application-conversation application))
        (setf (application-world-pending-share application)
              (append (application-world-pending-share application) (list text)))
        (return-from application-world-share nil))
      (handler-case
          (let* ((configuration (application-configuration application))
                 (channel       (application-world--served-channel application))
                 (world         (and channel (world-load configuration)))
                 (person        (and world (world-human-participant world))))
            (when person
              (application-world--share-elsewhere configuration world
                                                   :sender person :channel channel
                                                   :text text :source channel)))
        (autolith-error (condition)
          (application-present application
                               (application-failure-line
                                (format nil "the message was not shared: ~A"
                                        (bounded-string (princ-to-string condition) :limit 300))))
          nil)))))

(-> application-world-share-pending (application) list)
(defun application-world-share-pending (application)
  "Share the messages the person submitted before the conversation persisted, in order, now that it has.

The conversation's first durable message is what gives its channel a place
in the world, so it is brought in first. Returns the channels told, or NIL
when nothing waited or the conversation is still not durable. Each pending
message is taken once, so a replay of a turn shares nothing again."
  (let ((texts (application-world-pending-share application)))
    (when texts
      (application-world-sync application))
    (when (and texts (application-world--served-channel application))
      (setf (application-world-pending-share application) nil)
      (remove-duplicates (loop for text in texts
                               append (application-world-share application text))
                         :test #'string= :from-end t))))

(-> application-world-share-reply (application t) list)
(defun application-world-share-reply (application text)
  "Share the agent's reply with the participants it mentions elsewhere, and return the channels told.

The reply was said in this channel already. A reply that is not verbal text,
or a channel without an agent of its own, shares nothing. A delivery failure
does not stop the turn."
  (when (and (non-empty-string-p text)
             (slot-boundp application 'configuration))
    (handler-case
        (let* ((configuration (application-configuration application))
               (channel       (application-world--served-channel application))
               (world         (and channel (world-load configuration)))
               (agent         (and world
                                   (find ':agent (world-channel-residents world channel)
                                         :key #'participant-kind))))
          (when agent
            (application-world--share-elsewhere configuration world
                                                 :sender agent :channel channel :text text)))
      (autolith-error (condition)
        (application-present application
                             (application-failure-line
                              (format nil "the reply was not shared: ~A"
                                      (bounded-string (princ-to-string condition) :limit 300))))
        nil))))


;;;; -- Residents --

(defparameter *world-agent-turn-kinds*
  '(:message :operation-request :job-completion :idle-compaction)
  "Work kinds in which a channel's agent is in a turn.")

(-> application-world-agent-present-p (application) boolean)
(defun application-world-agent-present-p (application)
  "Return true while APPLICATION's durable channel has a resident agent, or before it exists.

Hosted-only catalogue entries do not establish agent absence. A world that
cannot be read counts as present: the world never blocks a session."
  (let ((channel (application-world--served-channel application)))
    (or (null channel)
        (handler-case
            (let ((world (world-load (application-configuration application))))
              (or (null (world-channel-find world channel))
                  (not (null (find ':agent (world-channel-residents world channel)
                                   :key #'participant-kind)))))
          (error ()
            t)))))

(-> application-world-agent-settings-update (application list) null)
(defun application-world-agent-settings-update (application settings)
  "Persist SETTINGS, the model, provider and effort for APPLICATION's channel."
  (when-let ((channel (application-world--served-channel application)))
    (handler-case
        (let* ((configuration (application-configuration application))
               (agent         (find ':agent (world-channel-residents (world-load configuration) channel)
                                    :key #'participant-kind)))
          (when (and agent (not (equal (participant-settings agent) settings)))
            (world-participant-update configuration (participant-identifier agent)
                                      :settings settings)))
      (autolith-error ()
        nil)))
  nil)

(-> application-world-before-turn (application keyword) null)
(defun application-world-before-turn (application kind)
  "Refuse work of KIND requiring an agent in a channel not yet registered for execution."
  (when (and (member kind *world-agent-turn-kinds*)
             (not (eq kind ':idle-compaction))
             (not (application-world-agent-present-p application)))
    (error 'world-error
           :message   "This channel has no agent to take this turn."
           :pathname  nil
           :operation ':turn))
  nil)

(-> application-world-note-child-turn (application keyword) null)
(defun application-world-note-child-turn (application kind)
  "Reset hosted turn progress and refresh its parent's activity rows."
  (let ((server (application-world-server application)))
    (when (and server (member kind *world-agent-turn-kinds*))
      (let ((active-p (application-world--agent-turn-active-p application)))
        (with-lock-held ((application-task-presentation-lock application))
          (setf (application-world-turn-progress application)
                (when active-p
                  (let ((progress (make-instance 'task-progress)))
                    (setf (task-progress-started-at progress) (get-internal-real-time))
                    progress)))))
      (world-server-note-child-activities server)))
  nil)

(-> application-world-note-child-progress (application keyword list) null)
(defun application-world-note-child-progress (application status details)
  "Record a hosted agent's observer event and schedule a parent activity refresh."
  (let ((server (application-world-server application)))
    (when (and server (member status '(:provider-request-started :provider-request-completed
                                       :tool-call-started :tool-call-completed)))
      (with-lock-held ((application-task-presentation-lock application))
        (when-let ((progress (application-world-turn-progress application)))
          (with-lock-held ((task-progress-lock progress))
            (task-progress--note-status-locked progress :status status :details details
                                                      :now (get-internal-real-time)))))
      (world-server-note-child-activities server)))
  nil)

(-> application-world-child-progress (application) list)
(defun application-world-child-progress (application)
  "Snapshot a hosted turn's request count and bounded chronological tool trace."
  (with-lock-held ((application-task-presentation-lock application))
    (when-let ((progress (application-world-turn-progress application)))
      (with-lock-held ((task-progress-lock progress))
        (task-progress--snapshot-locked progress (get-internal-real-time))))))

(-> application-world--agent-turn-active-p (application) boolean)
(defun application-world--agent-turn-active-p (application)
  "Return true while APPLICATION's agent is in a turn rather than idle or running a command."
  (let ((controller (application-input-controller application)))
    (and controller
         (with-lock-held ((application-input-controller-lock controller))
           (and (application-input-controller-active-p controller)
                (member (application-input-controller-active-work-kind controller)
                        *world-agent-turn-kinds*)
                t)))))

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
     (world-channel-designated (world-load configuration)
                               configuration
                               designator))))

(-> application-world-send
    (application &key (:channel string) (:text string) (:mentions list))
    list)
(defun application-world-send (application &key channel text mentions)
  "Send TEXT to CHANNEL as the human participant and deliver it locally when this process serves it."
  (let* ((configuration (application-configuration application))
         (person        (world-human-participant
                         (world-load configuration)))
         (served        (application-world--served-channel application))
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
                                              :mentions mentions
                                              :source   served))))
    (when (and served (member served (getf result :channels) :test #'string=))
      (application-world-deliver application))
    result))

(-> application--channel-message-entry (application list (option timestamp)) (values list &optional list))
(defun application--channel-message-entry (application request timestamp)
  "Return the transcript entry showing a channel message with its sender and channel.

A message said in this channel, and a notice for the person, show in full.
A copy that arrived from another channel, because it mentioned a participant
here or a participant here subscribes to that one, shows as one line naming
where it came from; its text went to the model and stays readable in the
inbox and the channel resource."
  (let* ((message        (getf request :message))
         (name           (sanitize-text (or (getf message :sender-name) "participant")
                                        :single-line-p t))
         (channel        (conversation-identifier-display (or (getf message :target) "?")))
         (kind           (getf message :kind))
         (timestamp-text (application--timestamp-text timestamp)))
    (if (member kind '(:mention :subscription))
        (let ((mentioned (application--mentioned-names application (getf message :mentions))))
          (list (terminal-span ':brand "● ")
                (terminal-span ':dim
                               (format nil "~A from #~A by ~A~@[ ~A~]~%"
                                       (if (eq kind ':mention)
                                           (format nil "~{@~A~^, ~} mentioned" mentioned)
                                           "subscribed message")
                                       channel name timestamp-text))))
        (application--render-message
         application (or (getf request :summary) "")
         (list (terminal-span ':brand "● ")
               (terminal-span ':agent-name name)
               (terminal-span ':dim (if (eq kind ':notice)
                                        (format nil " to you, from #~A~@[ ~A~]"
                                                (conversation-identifier-display
                                                 (or (getf message :source) (getf message :target) "?"))
                                                timestamp-text)
                                        (format nil " [channel ~A]~@[ ~A~]" channel timestamp-text)))
               (terminal-span ':plain (string #\Newline)))
         :footer (when (eq kind ':notice)
                   (application--channel-link-spans
                    (or (getf message :source) (getf message :target))))))))

(-> application--channel-link-spans ((option string)) list)
(defun application--channel-link-spans (channel)
  "Return a line holding the (open ...) form that goes to CHANNEL, or NIL without one.

The form is highlighted Lisp, so a plain terminal shows what to type; in
fullscreen each of its tokens is a widget that submits it when clicked."
  (when (non-empty-string-p channel)
    (let* ((form   (format nil "(open ~S)" (conversation-identifier-display channel)))
           (action (list ':submit form)))
      (append (list (terminal-span ':dim "  ↪ "))
              (mapcar (lambda (token)
                        (termdown:make-widget (terminal-span-style token)
                                              (terminal-span-text token)
                                              action))
                      (or (syntax--highlight-spans form :language *application-common-lisp-language*)
                          (list (terminal-span ':code form))))
              (list (terminal-span ':plain (format nil "~%")))))))

(-> application--mentioned-names (application list) list)
(defun application--mentioned-names (application identifiers)
  "Return the names of the participants IDENTIFIERS, or their display identifiers when the world has no name for one."
  (let ((world (handler-case (application-world application)
                 (error ()
                   nil))))
    (loop for identifier in identifiers
          for participant = (and world (world-participant-find world identifier))
          collect (if participant
                      (participant-name participant)
                      (conversation-identifier-display identifier)))))
