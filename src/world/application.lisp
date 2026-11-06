(in-package #:autolith)

;;;; -- World Hooks of the Application --

;;; The application keeps the world in step with the conversations it opens.
;;; The reconciliation hook is release machinery that leaves with
;;; src/world/sessions.lisp.

(-> application-world-sync (application &key (:all-p boolean)) null)
(defun application-world-sync (application &key all-p)
  "Reconcile the active conversation, or every conversation with ALL-P.

Viewing a channel moves nobody: the person resides in the inbox wherever
the terminal is. Failures are reported as warnings: the world is a
projection of the conversations and never blocks a session."
  (let* ((configuration (application-configuration application))
         (conversation  (and (slot-boundp application 'conversation)
                             (application-conversation application))))
    (handler-case
        (if all-p
            (world-reconcile-sessions configuration)
            (when (and conversation (conversation-persisted-p conversation))
              (world-reconcile-sessions configuration
                                        :identities (list (conversation-pathname conversation)))))
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
         (channels (world-channels world))
         (parked   (world-parked-agents world)))
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
                                                           (world-channel-subscribers world identifier))))))
                (and parked
                     (list (terminal-span ':dim
                                          (format nil "parked, in no channel: ~A~%"
                                                  (channel-resource--participants-text parked))))))
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
          (remhash (format nil "channel:~A" identifier)
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
                       (world-delivery-label delivery)
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
                                  conversation (world-delivery-label notice)
                                  :automatic-p              t
                                  :pending-input-identifier (world-delivery-pending-identifier notice)
                                  :operation-request        (world-delivery-request notice)))))
        (when (application-ui application)
          (application--present-conversation-record application record))))
    (world-inbox-settle (application-configuration application) channel
                        (mapcar #'world-delivery-identifier notices))
    (world-message-resume (application-configuration application))
    (let ((server (application-world-server application)))
      (if server
          (world-server-person-unread-changed server)
          (application-world-show-person-unread application 0))))
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
            (let ((entry (world-client-entry configuration)))
              (when entry
                (localgroup-query-record entry ':person-unread)))
          (autolith-error ()
            ;; A checkpointing or stopping server counts again on the next attach.
            nil))))
  nil)

(-> application-world-show-person-unread (application (integer 0)) null)
(defun application-world-show-person-unread (application unread)
  "Show UNREAD, the messages waiting in the person's inbox, on APPLICATION's modeline."
  (setf (application-world-inbox-unread application) unread)
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when ui
      (terminal-ui-set-idle-status-details ui (application--status-details application))))
  nil)

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
          collect (let* ((residence (participant-residence participant))
                         (channel   (and residence (world-channel-find world residence))))
                    (list :name        (format nil "@~A" name)
                          :description (if residence
                                           (format nil "~(~A~) in ~A~@[ ~A~]"
                                                   (participant-kind participant)
                                                   (conversation-identifier-display residence)
                                                   (and channel (channel-title channel)))
                                           (format nil "~(~A~), parked"
                                                   (participant-kind participant)))))))

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
              (world-reconcile-sessions configuration
                                        :identities (list (conversation-pathname
                                                           (application-conversation application))))
              (application-world-deliver application)
              (application-world-show-person-unread application
                                                    (world-person-unread configuration)))
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
                 (world         (and channel (values (world-reconcile-sessions configuration))))
                 (person        (and world (world-human-participant world)))
                 (elsewhere     (and person
                                     (find #\@ text)
                                     (remove-if (lambda (identifier)
                                                  (let ((mentioned (world-participant-find
                                                                    world identifier)))
                                                    (or (null mentioned)
                                                        (equal (participant-residence mentioned)
                                                               channel))))
                                                (world-message-mentions world text))))
                 (subscribed    (and person
                                     (some (lambda (subscriber)
                                             (not (equal (participant-residence subscriber)
                                                         channel)))
                                           (world-channel-subscribers world channel)))))
            (when (or elsewhere subscribed)
              (getf (world-message-send configuration
                                        :sender        (participant-identifier person)
                                        :channel       channel
                                        :text          text
                                        :mentions      elsewhere
                                        :post-p        nil
                                        :subscribers-p t
                                        :source        channel)
                    :channels)))
        (autolith-error (condition)
          (application-present application
                               (format nil "The message was not shared: ~A"
                                       (bounded-string (princ-to-string condition) :limit 300)))
          nil)))))

(-> application-world-share-pending (application) list)
(defun application-world-share-pending (application)
  "Share the messages the person submitted before the conversation persisted, in order, now that it has.

Returns the channels told, or NIL when nothing waited or the conversation
is still not durable. Each pending message is taken once, so a replay of a
turn shares nothing again."
  (let ((texts (application-world-pending-share application)))
    (when (and texts (application-world--served-channel application))
      (setf (application-world-pending-share application) nil)
      (remove-duplicates (loop for text in texts
                               append (application-world-share application text))
                         :test #'string= :from-end t))))

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
                                                      (equal (participant-residence mentioned)
                                                             channel))))
                                              (world-message-mentions world text))))
               (subscribed    (and agent
                                   (some (lambda (subscriber)
                                           (not (equal (participant-residence subscriber)
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


;;;; -- Residents --

;;; A channel has one resident agent. Moving it elsewhere stops it at a safe
;;; boundary, leaves the channel without an agent, and makes it the
;;; destination's agent, idle there until addressed. The agent takes along
;;; the provider context it built: a served destination adopts it at once,
;;; durably, and any other keeps it waiting in the agent's context file until
;;; the channel is served. The world store moves the identity only once the
;;; agent stopped and its destination is ready.

(defparameter *world-agent-turn-kinds*
  '(:message :operation-request :job-completion :recovery-diagnosis :idle-compaction)
  "The work kinds in which a channel's agent is in a turn, the boundary a move or a park waits for.")

(defparameter *world-agent-stop-seconds* 120
  "How long a move or a park waits for the agent's turn to reach its safe boundary.")

(-> application-world-agent-present-p (application) boolean)
(defun application-world-agent-present-p (application)
  "Return true while APPLICATION's channel has a resident agent, or before it is a channel at all.

A world that cannot be read counts as present: the world never blocks a
session."
  (let ((channel (application-world--served-channel application)))
    (or (null channel)
        (handler-case
            (let ((world (application-world application)))
              (or (null (world-channel-find world channel))
                  (not (null (find ':agent (world-channel-residents world channel)
                                   :key #'participant-kind)))))
          (error ()
            t)))))

(-> application-world-agent-settings-update (application list) null)
(defun application-world-agent-settings-update (application settings)
  "Give the agent residing in APPLICATION's channel SETTINGS, its model, provider and effort.

The agent keeps them wherever it moves. A world that cannot be written
leaves the session's own choice in place: the world never blocks a session."
  (let ((channel (application-world--served-channel application)))
    (when channel
      (handler-case
          (let* ((configuration (application-configuration application))
                 (agent         (find ':agent (world-channel-residents (world-load configuration) channel)
                                      :key #'participant-kind)))
            (when (and agent (not (equal (participant-settings agent) settings)))
              (world-participant-update configuration (participant-identifier agent)
                                        :settings settings)))
        (autolith-error ()
          nil))))
  nil)

(-> application-world-stop-agent (application) null)
(defun application-world-stop-agent (application)
  "Hold APPLICATION's new work and wait until its agent's turn reached a safe boundary.

The active turn is cancelled the way steering is taken: the tool call in
flight finishes and no new request starts. A (move) typed in this channel is
not a turn and does not wait for itself. When the turn does not stop in time
the channel's work resumes and the stop is refused."
  (let ((controller (application-input-controller application)))
    (when controller
      ;; A session's pause asks for the cancellation itself; without one, as
      ;; outside a world server, the turn is asked to stop directly.
      (unless (application-localgroup-pause application)
        (application-input-controller--request-active-turn-cancellation controller))
      (let ((deadline (+ (get-internal-real-time)
                         (* *world-agent-stop-seconds* internal-time-units-per-second))))
        (loop while (application-world--agent-turn-active-p application)
              do (when (>= (get-internal-real-time) deadline)
                   (application-localgroup-resume application)
                   (error 'world-error
                          :message   "The agent did not finish its turn in time; nothing changed."
                          :pathname  nil
                          :operation ':move))
                 (sleep 0.05)))))
  nil)

(-> application-world-resume-agent (application) null)
(defun application-world-resume-agent (application)
  "Let APPLICATION's held work start again after a move or a park that did not happen."
  (application-localgroup-resume application)
  nil)

(-> application-world-release-agent
    (application participant &key (:destination (option string)))
    (integer 0))
(defun application-world-release-agent (application participant &key destination)
  "Let APPLICATION's stopped agent PARTICIPANT go to channel DESTINATION, or parked, and return the queued messages dropped.

The unstarted queue is dropped; running jobs stay with the channel. The
channel's record of messages queued for the agent is forgotten, so the next
resident receives every message the conversation has not durably received.
The channel is told and its work resumes."
  (let ((controller (application-input-controller application))
        (dropped    0))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (dolist (items (list (application-input-controller-work-items controller)
                             (application-input-controller-steering-items controller)))
          (loop until (deque-empty-p items)
                do (deque-pop-front items)
                   (incf dropped))))
      ;; Receipts in the conversation say what was received; the queue's
      ;; bookkeeping only kept a message from being queued twice.
      (clrhash (application-world-deliveries application))
      (application-localgroup-resume application)
      (application-input-controller--publish-counts controller))
    (application-present
     application
     (format nil "Agent ~A ~:[is parked~;moved to channel ~:*~A~]; this channel has no agent now~@[, and ~D queued message~:P ~:*~[~;was~:;were~] dropped~]. (move \"WHO\") brings an agent here, (domestic) creates a new one."
             (participant-name participant)
             (and destination (conversation-identifier-display destination))
             (and (plusp dropped) dropped)))
    dropped))

(-> world-participant-configuration (configuration participant) configuration)
(defun world-participant-configuration (base participant)
  "Return BASE with agent PARTICIPANT's model, provider and effort, the configuration it runs under in any channel."
  (let ((settings (participant-settings participant)))
    (if (getf settings :model)
        (configuration-copy base
                            :model                      (getf settings :model)
                            :model-provider             (or (getf settings :model-provider) "")
                            :model-selection-override-p t
                            :reasoning-effort           (or (getf settings :reasoning-effort)
                                                            (config :reasoning-effort base)))
        base)))

(-> application-world-prepare-agent (application participant) (values configuration t))
(defun application-world-prepare-agent (application participant)
  "Return the configuration and provider PARTICIPANT runs under in APPLICATION's channel.

They are built before a move commits, so a provider that cannot start
refuses the move instead of leaving the agent without a runtime."
  (let ((configuration (world-participant-configuration (application-configuration application)
                                                        participant)))
    (values configuration
            (provider-create configuration
                             :reasoning-summaries-p (application-reasoning-traces-p application)))))

(-> application-world-install-agent
    (application participant &key (:rows list) (:source (option string)) (:goal list)
                                  (:transition (option string))
                                  (:configuration (option configuration)) (:provider t))
    null)
(defun application-world-install-agent
    (application participant &key rows source goal transition configuration provider)
  "Make moved agent PARTICIPANT APPLICATION's agent, speaking with ROWS and pursuing GOAL, the context and goal move TRANSITION brings from channel SOURCE.

CONFIGURATION and PROVIDER come from `application-world-prepare-agent', and
are prepared here when not given. The conversation records the arrival
durably before the runtime changes; the tool registry stays."
  (multiple-value-bind (configuration provider)
      (if (and configuration provider)
          (values configuration provider)
          (application-world-prepare-agent application participant))
    (let ((agent (agent-create :configuration configuration
                               :provider      provider
                               :conversation  (application-conversation application)
                               :tool-registry (application-tool-registry application)
                               :hurry-up-p    (application-hurry-up-p application)
                               :worker        (application-worker application))))
      (application-world--record-arrival application participant configuration
                                         :rows       rows
                                         :source     source
                                         :goal       goal
                                         :transition transition
                                         :arrival    ':moved)
      (setf (application-configuration application) configuration
            (application-provider application)      provider
            (application-agent application)         agent)
      (application-refresh-context-meter application)
      (application-present
       application
       (format nil "Agent ~A now resides here~@[, moved from channel ~A~]; it waits for your message."
               (participant-name participant)
               (and source (conversation-identifier-display source))))))
  nil)

(-> application-world--record-arrival
    (application participant configuration &key (:rows list) (:source (option string))
                                                (:goal list) (:transition (option string))
                                                (:arrival keyword))
    null)
(defun application-world--record-arrival (application participant configuration
                                          &key rows source goal transition arrival)
  "Record durably that agent PARTICIPANT now speaks in APPLICATION's channel under CONFIGURATION.

The conversation takes the agent's model selection, its provider context
becomes ROWS, the context it brings from channel SOURCE, and an automatic
message tells the agent where it is: ARRIVAL is :MOVED for an agent that came
from elsewhere and :CREATED for a new one. The agent reads it on its next
turn, which comes only when it is addressed. The channel's session then
names the agent."
  (let* ((conversation (application-conversation application))
         (channel      (conversation-identifier conversation)))
    (conversation-set-model-selection
     conversation
     (config :model configuration)
     (config :reasoning-effort configuration)
     :provider (let ((name (config :model-provider configuration)))
                 (and (non-empty-string-p name) name)))
    (conversation-append-context-adoption conversation rows
                                          :participant (participant-identifier participant)
                                          :source      source
                                          :transition  transition)
    (conversation-append-user-message
     conversation
     (format nil "[You, ~A, ~:[now live in~;were moved into~] channel ~A~@[ from channel ~A~]. ~:[~;Your context continues from where you were. ~]This channel's earlier history is at channel:id/~A. Nothing is expected of you until someone writes here or mentions you.]"
             (participant-name participant)
             (eq arrival ':moved)
             (conversation-identifier-display channel)
             (and source (conversation-identifier-display source))
             rows
             (conversation-identifier-display channel))
     :automatic-p t)
    (application-world--install-goal application goal)
    (world-session-remap configuration channel (participant-identifier participant)))
  nil)

(-> application-world--install-goal (application list) null)
(defun application-world--install-goal (application goal)
  "Make GOAL, the one an arriving agent brings, APPLICATION's goal, replacing any the channel kept.

A running mission arrives paused: its admitted jobs stayed behind, so it
resumes only when asked."
  (let ((goal (copy-tree goal)))
    (when (and (mission-goal-p goal) (eq (getf goal :status) ':active))
      (setf (getf goal :status) ':paused))
    (when (or goal (application-goal application))
      (setf (application-goal application) goal)
      (application--record-goal application)
      (when (mission-goal-p goal)
        (mission-restore application)
        (application-mission-schedules application))))
  nil)

(-> world-agent-context
    (configuration participant &key (:application (option application)))
    (values list (option string) list))
(defun world-agent-context (configuration participant &key application)
  "Return the context rows agent PARTICIPANT carries, the channel they come from and the agent's goal.

APPLICATION, serving the agent's channel, has them live once it adopted
any context waiting for the agent. Otherwise that waiting context comes
first, then its channel's conversation on disk."
  (let ((residence (participant-residence participant)))
    (cond
      (application
       (application-world-adopt-pending-context application :strict-p t)
       (values (conversation-context-rows (application-conversation application))
               residence
               (copy-tree (application-goal application))))
      ((world-context-waiting-p configuration participant)
       (multiple-value-bind (rows source destination transition goal)
           (world-context-read configuration (participant-identifier participant))
         (declare (ignore destination transition))
         (values rows source goal)))
      ((world-agent--stored-channel-p configuration residence)
       (let ((conversation (conversation-load-by-id configuration residence)))
         (values (conversation-context-rows conversation)
                 residence
                 (goal-record->goal (conversation-latest-goal-record conversation)))))
      (t
       (values nil residence nil)))))

(-> world-agent--stored-channel-p (configuration (option string)) boolean)
(defun world-agent--stored-channel-p (configuration channel)
  "Return true when CHANNEL has a conversation on disk."
  (and channel
       (conversation-storage-active-pathname (conversation-pathname-for-id configuration channel))
       t))

(-> world-move-agent
    (configuration participant string &key (:source-application (option application))
                                           (:destination-application (option application))
                                           (:interrupt-task function))
    list)
(defun world-move-agent (configuration participant channel &key source-application
                                                                 destination-application
                                                                 (interrupt-task (constantly nil)))
  "Move agent PARTICIPANT into CHANNEL, and return a plist of :source, NIL for a parked agent, :channel and :dropped.

SOURCE-APPLICATION, when it serves the channel the agent leaves, stops it at
a safe boundary first; DESTINATION-APPLICATION, when it serves CHANNEL, has
its runtime prepared. The context the agent carries is kept in its context
file, naming CHANNEL, before the world store moves the agent, and a failure
before that leaves everything as it was. After it nothing is undone: CHANNEL
adopts the kept context once, when served now, else when it is next served
or before the agent's next turn there. Moves and parks run one at a time,
and the store moves the agent only from where this move found it.
INTERRUPT-TASK stops the task a task pool runs the agent in as a child, so
the agent moves as it stood when interrupted."
  (world-call-with-transition
   configuration
   (lambda ()
     (let ((identifier (participant-identifier participant))
           (source     (participant-residence participant))
           (world      (world-load configuration)))
       (world-agent--require-movable world participant ':move)
       (when (equal source channel)
         (error 'world-error
                :message    (format nil "~A already resides in channel ~A."
                                    (participant-name participant) (conversation-identifier-display channel))
                :pathname   nil
                :operation  ':move
                :identifier identifier))
       (world-inbox-refuse-agent world channel ':move)
       (when (find ':agent (world-channel-residents world channel) :key #'participant-kind)
         (error 'world-error
                :message    (format nil "Channel ~A already has an agent." (conversation-identifier-display channel))
                :pathname   nil
                :operation  ':move
                :identifier channel))
       (funcall interrupt-task participant)
       (multiple-value-bind (prepared provider dropped)
           (world-agent--transition
            configuration participant
            :application source-application
            :destination channel
            :prepare     (lambda ()
                           (if destination-application
                               (application-world-prepare-agent destination-application participant)
                               (values nil nil)))
            :commit      (lambda ()
                           (world-participant-move configuration identifier channel
                                                   :expected-residence source)))
         ;; The destination's session names the agent at once; its
         ;; adoption names it again, should this write fail.
         (handler-case (world-session-remap configuration channel identifier)
           (autolith-error ()
             nil))
         (when destination-application
           (application-world-adopt-pending-context destination-application
                                                    :configuration prepared
                                                    :provider      provider))
         (list :source source :channel channel :dropped dropped))))))

(-> world-park-agent
    (configuration participant &key (:application (option application)) (:interrupt-task function))
    list)
(defun world-park-agent (configuration participant &key application (interrupt-task (constantly nil)))
  "Park agent PARTICIPANT, leaving its channel without an agent, and return a plist of :channel and :dropped.

APPLICATION, when it serves the channel, stops the agent at a safe boundary
and drops its unstarted prompts. The agent keeps its identity, its children
and the context it carries, kept in its context file until a move brings it
into a channel again; its children carry on untouched. The channel keeps its
history, the person and its subscribers, and offers a move or a domestic
agent. The context is kept before the world store parks the agent, as a
move keeps it, and INTERRUPT-TASK stops a child's running task, as a move
stops it."
  (world-call-with-transition
   configuration
   (lambda ()
     (let ((identifier (participant-identifier participant))
           (channel    (participant-residence participant)))
       (world-agent--require-movable (world-load configuration) participant ':park)
       (funcall interrupt-task participant)
       (list :channel channel
             :dropped (nth-value 2 (world-agent--transition
                                    configuration participant
                                    :application application
                                    :destination nil
                                    :prepare     (lambda ()
                                                   (values nil nil))
                                    :commit      (lambda ()
                                                   (world-participant-park
                                                    configuration identifier
                                                    :expected-residence channel)))))))))

(-> world-agent--require-movable (world participant keyword) null)
(defun world-agent--require-movable (world participant operation)
  "Refuse OPERATION unless PARTICIPANT is an agent that still resides where the caller found it."
  (let ((current (world-participant-find world (participant-identifier participant))))
    (unless (eq (participant-kind participant) ':agent)
      (error 'world-error
             :message    (format nil "~A is not an agent; only agents move or park."
                                 (participant-name participant))
             :pathname   nil
             :operation  operation
             :identifier (participant-identifier participant)))
    (unless current
      (error 'world-error
             :message    (format nil "Agent ~A no longer exists." (participant-name participant))
             :pathname   nil
             :operation  operation
             :identifier (participant-identifier participant)))
    (world--require-residence current (participant-residence participant) operation))
  nil)

(-> application-world-interrupt-task (application participant) boolean)
(defun application-world-interrupt-task (application participant)
  "Stop the task APPLICATION's task pool runs child agent PARTICIPANT in, and return true when there was one.

The task's conversation is the child's channel. A move interrupts the task
the way a cancellation does, for the reason :moved, and waits until it ended,
so the agent leaves with the context it had then; its parent receives the
cancelled result. Only the task itself is cancelled, with its tool runs: the
tasks of the child's own children go on, since a move never affects them.
A task that does not end in time refuses the move."
  (let* ((channel (participant-residence participant))
         (runtime (and channel (application--task-orchestrator application)))
         (jobs    (and runtime
                       (remove-if-not (lambda (job)
                                        (and (typep job 'task-job)
                                             (equal (task-job-execution-identifier job) channel)
                                             (not (job-terminal-p job))))
                                      (task-orchestrator-list-jobs runtime)))))
    (dolist (job jobs)
      (when (job-cancel job :reason ':moved)
        (session-job-note-cancellation job ':moved)))
    (dolist (job jobs)
      (unless (nth-value 1 (session-job-await job *world-agent-stop-seconds*))
        (error 'world-error
               :message    (format nil "Agent ~A's task was interrupted but has not ended yet; nothing moved. Try again."
                                   (participant-name participant))
               :pathname   nil
               :operation  ':move
               :identifier (participant-identifier participant))))
    (not (null jobs))))

(-> world-agent--transition
    (configuration participant &key (:application (option application))
                                    (:destination (option string))
                                    (:prepare function) (:commit function))
    (values t t (integer 0)))
(defun world-agent--transition (configuration participant &key application destination prepare commit)
  "Stop PARTICIPANT, keep its context and goal for DESTINATION, call COMMIT and leave its channel without it.

Return what PREPARE returned and the queued messages dropped. APPLICATION
serves the agent's channel; an unserved channel is held by its conversation
lease meanwhile, and one another process serves refuses the change. PREPARE
readies the destination before anything durable changes. Until COMMIT
returns, a failure restores the context file as it was and lets
APPLICATION's work resume, so nothing has changed."
  (let* ((identifier (participant-identifier participant))
         (source     (participant-residence participant))
         (previous   (and (probe-file (world-context-pathname configuration identifier))
                          (multiple-value-list (world-context-read configuration identifier))))
         (lease      (and (null application)
                          (world-agent--stored-channel-p configuration source)
                          (world-agent--lease configuration participant))))
    (unwind-protect
         (let ((written-p   nil)
               (committed-p nil))
           (when application
             (application-world-stop-agent application))
           (multiple-value-bind (prepared provider)
               (unwind-protect
                    (multiple-value-bind (rows from goal)
                        (world-agent-context configuration participant :application application)
                      (multiple-value-prog1 (funcall prepare)
                        (setf written-p t)
                        (world-context-write configuration identifier
                                             :source      from
                                             :rows        rows
                                             :goal        goal
                                             :destination destination
                                             :transition  (make-identifier))
                        (funcall commit)
                        (setf committed-p t)))
                 (unless committed-p
                   (when written-p
                     (if previous
                         (destructuring-bind (rows from destination transition goal) previous
                           (world-context-write configuration identifier
                                                :source      from
                                                :rows        rows
                                                :goal        goal
                                                :destination destination
                                                :transition  transition))
                         (world-context-delete configuration identifier)))
                   (when application
                     (application-world-resume-agent application))))
             (values prepared provider
                     (world-agent--settle-source configuration participant source
                                                 :application application
                                                 :destination destination))))
      (when lease
        (conversation-lease-release lease)))))

(-> world-agent--lease (configuration participant) conversation-lease)
(defun world-agent--lease (configuration participant)
  "Hold the conversation of agent PARTICIPANT's unserved channel, or refuse while another process serves it."
  (handler-case (conversation-lease-acquire configuration (participant-residence participant))
    (conversation-in-use ()
      (error 'world-error
             :message    (format nil "Agent ~A's channel ~A is open in another Autolith process; nothing changed."
                                 (participant-name participant)
                                 (conversation-identifier-display (participant-residence participant)))
             :pathname   nil
             :operation  ':move
             :identifier (participant-identifier participant)))))

(-> world-agent--settle-source
    (configuration participant (option string) &key (:application (option application))
                                                    (:destination (option string)))
    (integer 0))
(defun world-agent--settle-source (configuration participant source &key application destination)
  "Leave channel SOURCE without agent PARTICIPANT after its move or park committed, and return the queued messages dropped.

Its session stops naming the agent; a session still naming it would find the
agent resides elsewhere, so a mapping that cannot be rewritten changes
nothing. The goal the agent took along leaves the channel too: no agent
pursues it there. APPLICATION, serving SOURCE, drops the agent's queue and
resumes."
  (when source
    (handler-case (world-session-remap configuration source nil)
      (autolith-error ()
        nil))
    (handler-case
        (if application
            (application-world-surrender-goal application)
            (let ((conversation (conversation-load-by-id configuration source)))
              (when (goal-record->goal (conversation-latest-goal-record conversation))
                (conversation-append-record conversation (goal->record nil)))))
      (autolith-error ()
        nil)))
  (if application
      (application-world-release-agent application participant :destination destination)
      0))

(-> application-world-surrender-goal (application) null)
(defun application-world-surrender-goal (application)
  "Clear the goal of APPLICATION's departed agent, which took it along.

A mission still running stops as blocked first, which cancels the jobs it
admitted: they worked for the agent, while the channel's other jobs stay."
  (let ((goal (application-goal application)))
    (when goal
      (when (and (mission-goal-p goal)
                 (member (getf goal :status) '(:active :paused :blocked)))
        (application-mission-stop application ':blocked "The agent left this channel and took its mission along."))
      (setf (application-goal application) nil)
      (application--record-goal application)))
  nil)

(-> application-world-adopt-pending-context
    (application &key (:strict-p boolean) (:configuration (option configuration)) (:provider t))
    boolean)
(defun application-world-adopt-pending-context (application &key strict-p configuration provider)
  "Let the agent residing in APPLICATION's channel adopt the context a move brought it, once, and return whether it did.

The agent is the channel's resident in the world store: the channel may
have been opened before it ever had a durable message, so its session
mapping cannot be relied on. CONFIGURATION and PROVIDER come from
`application-world-prepare-agent' when the move prepared them. A context
this conversation adopted already, before a crash kept its file, is only
removed. A failure leaves the context waiting for the next try and is
signalled with STRICT-P, else told in the channel: a turn must not start
without the agent's context, while an open goes on."
  (let ((channel (application-world--served-channel application)))
    (when channel
      (handler-case
          (let* ((world-configuration (application-configuration application))
                 (agent               (find ':agent (world-channel-residents
                                                     (application-world application) channel)
                                            :key #'participant-kind))
                 (identifier          (and agent (participant-identifier agent))))
            (when (and agent (probe-file (world-context-pathname world-configuration identifier)))
              (multiple-value-bind (rows source destination transition goal)
                  (world-context-read world-configuration identifier)
                (when (equal destination channel)
                  (unless (and transition
                               (equal transition (conversation-adopted-transition
                                                  (application-conversation application))))
                    (application-world-install-agent application agent
                                                     :rows          rows
                                                     :source        source
                                                     :goal          goal
                                                     :transition    transition
                                                     :configuration configuration
                                                     :provider      provider))
                  (world-context-delete world-configuration identifier)
                  t))))
        (autolith-error (condition)
          (if strict-p
              (error condition)
              (progn
                (application-present
                 application
                 (format nil "The context a moved agent brings could not be adopted yet: ~A It is tried again before the agent's next turn."
                         condition))
                nil)))))))

(-> application-world-before-turn (application keyword) null)
(defun application-world-before-turn (application kind)
  "Ready APPLICATION's agent for a turn of work KIND, or refuse the turn in a channel without an agent.

The agent adopts any context a move brought it first. Idle compaction
summarizes the channel's history for whoever speaks there next and needs no
agent."
  (when (member kind *world-agent-turn-kinds*)
    (application-world-adopt-pending-context application :strict-p t)
    (unless (or (eq kind ':idle-compaction)
                (application-world-agent-present-p application))
      (error 'world-error
             :message   "This channel has no agent, so nothing runs here; (move \"WHO\") or (domestic) brings one."
             :pathname  nil
             :operation ':turn)))
  nil)

(-> application-world-domestic-agent (application) participant)
(defun application-world-domestic-agent (application)
  "Create a new agent participant resident in APPLICATION's channel and return it.

The channel must be served and without an agent. The new agent takes the
application's current model and effort, a handle from the workspace, and
the person as its parent. It starts with no context of its own; the
channel's history is there for it to read."
  (let* ((configuration (application-configuration application))
         (channel       (or (application-world--served-channel application)
                            (error 'world-error
                                   :message   "This conversation is not a channel yet; it becomes one with its first durable message."
                                   :pathname  nil
                                   :operation ':domestic)))
         (world         (values (world-reconcile-sessions configuration)))
         (person        (world-human-participant world)))
    (world-inbox-refuse-agent world channel ':domestic)
    (when (find ':agent (world-channel-residents world channel) :key #'participant-kind)
      (error 'world-error
             :message    (format nil "Channel ~A already has an agent."
                                 (conversation-identifier-display channel))
             :pathname   nil
             :operation  ':domestic
             :identifier channel))
    (let ((agent (world-participant-create
                  configuration
                  :kind      ':agent
                  :name      (world-unique-handle world (world-handle-base
                                                         (world-workspace-leaf
                                                          (world-workspace-name
                                                           (config :working-directory configuration)))
                                                         :fallback *world-agent-name*))
                  :residence channel
                  :parent    (and person (participant-identifier person))
                  :settings  (append (list :model (config :model configuration))
                                     (let ((provider (config :model-provider configuration)))
                                       (and (non-empty-string-p provider)
                                            (list :model-provider provider)))
                                     (let ((effort (config :reasoning-effort configuration)))
                                       (and effort (list :reasoning-effort effort)))))))
      ;; The new agent runs under this session's model, so the runtime stays;
      ;; only its context starts fresh.
      (application-world--record-arrival application agent configuration :arrival ':created)
      (application-present application
                           (format nil "Agent ~A now lives in this channel." (participant-name agent)))
      agent)))

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

(-> application--channel-message-entry (application list (option timestamp)) list)
(defun application--channel-message-entry (application request timestamp)
  "Return the transcript entry showing a channel message with its sender and channel.

A message said in this channel, and a notice for the person, show in full.
A copy that arrived from another channel, because it mentioned a participant
here or this channel subscribes to that one, shows as one line naming where
it came from; its text went to the model and stays readable in the inbox and
the channel resource."
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
        (append (list (terminal-span ':brand "● ")
                      (terminal-span ':agent-name name)
                      (terminal-span ':dim (if (eq kind ':notice)
                                               (format nil " to you, from #~A~@[ ~A~]~%"
                                                       (conversation-identifier-display
                                                        (or (getf message :source) (getf message :target) "?"))
                                                       timestamp-text)
                                               (format nil " [channel ~A]~@[ ~A~]~%" channel timestamp-text))))
                (application-world-mention-spans
                 application
                 (application--markdown-body application (or (getf request :summary) "")))
                (when (eq kind ':notice)
                  (application--channel-link-spans
                   (or (getf message :source) (getf message :target))))))))

(-> application--channel-link-spans ((option string)) list)
(defun application--channel-link-spans (channel)
  "Return a line holding the (switch ...) form that goes to CHANNEL, or NIL without one.

The form is highlighted Lisp, so a plain terminal shows what to type; in
fullscreen each of its tokens is a widget that submits it when clicked."
  (when (non-empty-string-p channel)
    (let* ((form   (format nil "(switch ~S)" (conversation-identifier-display channel)))
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
