(in-package #:autolith)

;;;; -- World Store Tests --

(-> world-tests--workspace (pathname string) string)
(defun world-tests--workspace (root name)
  "Create and return the canonical name of workspace NAME below ROOT."
  (let ((directory (merge-pathnames (format nil "~A/" name) root)))
    (ensure-directories-exist (merge-pathnames "marker" directory))
    (workspace-directory-name directory)))

(-> test-world-persistence () null)
(defun test-world-persistence ()
  "Test participants and channels persist with derived residents and subscribers."
  (with-test-configuration (configuration root)
    (let ((workspace (world-tests--workspace root "project")))
      (multiple-value-bind (human home)
          (world-participant-create configuration
                                    :kind ':human
                                    :name "user"
                                    :home (list :workspace workspace :title "Lobby")
                                    :now  1000)
        (test-assert (and (eq (participant-kind human) ':human)
                          (string= (participant-residence human) (channel-identifier home))
                          (string= (channel-owner home) (participant-identifier human))
                          (string= (channel-workspace home) workspace)
                          (string= (channel-title home) "Lobby"))
                     "a participant created with a home owns and resides in that channel")
        (let* ((side  (world-channel-create configuration
                                            :id        "b-side"
                                            :owner     (participant-identifier human)
                                            :workspace workspace
                                            :parent    (channel-identifier home)
                                            :now       1001))
               (agent (world-participant-create configuration
                                                :kind      ':agent
                                                :name      "autolith"
                                                :residence (channel-identifier side)
                                                :parent    (participant-identifier human)
                                                :settings  '(:model "gpt-5" :reasoning-effort "medium")
                                                :now       1002)))
          (test-assert (and (string= (channel-identifier side) "b-side")
                            (equal (channel-parent side) (channel-identifier home)))
                       "an explicit channel identifier and parent are kept")
          (world-participant-subscribe configuration (participant-identifier human) "b-side"
                                       :now 1003)
          (world-participant-move configuration (participant-identifier agent)
                                  (channel-identifier home) :now 1004)
          (world-participant-update configuration (participant-identifier agent)
                                    :presence ':present :now 1005)
          (world-channel-update configuration "b-side" :title "Side work" :now 1006)
          (let* ((world (world-load configuration))
                 (agent (world-participant-find world (participant-identifier agent)))
                 (human (world-participant-find world (participant-identifier human)))
                 (side  (world-channel-find world "b-side")))
            (test-assert (and (string= (participant-residence agent) (channel-identifier home))
                              (eq (participant-presence agent) ':present)
                              (= (participant-updated-at agent) 1005)
                              (equal (participant-settings agent)
                                     '(:model "gpt-5" :reasoning-effort "medium")))
                         "participant moves and updates reload from the log")
            (test-assert (and (string= (channel-title side) "Side work")
                              (= (channel-updated-at side) 1006))
                         "channel updates reload from the log")
            (test-assert (equal (mapcar #'participant-identifier
                                        (world-channel-residents world (channel-identifier home)))
                                (list (participant-identifier agent)
                                      (participant-identifier human)))
                         "residents are derived from residence, newest change first")
            (test-assert (null (world-channel-residents world "b-side"))
                         "a channel left by its only resident is empty")
            (test-assert (equal (mapcar #'participant-identifier
                                        (world-channel-subscribers world "b-side"))
                                (list (participant-identifier human)))
                         "subscribers are derived from subscriptions")
            (test-assert (equal (mapcar #'channel-identifier (world-channels world))
                                (list "b-side" (channel-identifier home)))
                         "channels list most recently changed first")
            (test-assert (equal (mapcar #'channel-identifier
                                        (world-channel-children world (channel-identifier home)))
                                (list "b-side"))
                         "nested channels are found by parent")
            (test-assert (eq (world-human-participant world) human)
                         "the human participant is found by kind")
            (test-assert (equal (participant-subscriptions human) '("b-side"))
                         "subscriptions are stored on the participant")
            (world-participant-unsubscribe configuration (participant-identifier human) "b-side"
                                           :now 1007)
            (test-assert (null (participant-subscriptions
                                (world-participant-find (world-load configuration)
                                                        (participant-identifier human))))
                         "unsubscribing removes the channel")
            (test-assert (handler-case
                             (progn (world-participant-move configuration
                                                            (participant-identifier agent)
                                                            "missing")
                                    nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':move-participant)))
                         "moving into an unknown channel is refused")
            (test-assert (handler-case
                             (progn (world-participant-create configuration
                                                              :kind      ':agent
                                                              :name      "two homes"
                                                              :residence "b-side"
                                                              :home      (list :workspace workspace))
                                    nil)
                           (world-error () t))
                         "a participant takes either a residence or a home, not both"))))))
  nil)

(-> test-world-deletion-rules () null)
(defun test-world-deletion-rules ()
  "Test deletion keeps residents explicit and channels under human control."
  (with-test-configuration (configuration root)
    (let ((workspace (world-tests--workspace root "project")))
      (multiple-value-bind (human lobby)
          (world-participant-create configuration
                                    :kind ':human
                                    :name "user"
                                    :home (list :workspace workspace :id "b-lobby")
                                    :now  2000)
        (multiple-value-bind (lead task)
            (world-participant-create configuration
                                      :kind   ':agent
                                      :name   "lead"
                                      :parent (participant-identifier human)
                                      :home   (list :workspace workspace :id "b-task"
                                                    :parent (channel-identifier lobby))
                                      :now    2001)
          (let ((helper (world-participant-create configuration
                                                  :kind      ':agent
                                                  :name      "helper"
                                                  :parent    (participant-identifier lead)
                                                  :residence (channel-identifier task)
                                                  :now       2002)))
            (world-participant-subscribe configuration (participant-identifier human)
                                         (channel-identifier task))
            (test-assert (handler-case
                             (progn (world-channel-delete configuration "b-task") nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':delete-channel)))
                         "a channel with residents is not deleted implicitly")
            (test-assert (handler-case
                             (progn (world-channel-delete configuration "b-lobby") nil)
                           (world-error () t))
                         "a channel with nested channels is refused")
            (test-assert (handler-case
                             (progn (world-participant-delete configuration
                                                              (participant-identifier lead))
                                    nil)
                           (world-error () t))
                         "a participant with children needs a recursive deletion")
            (test-assert (handler-case
                             (progn (world-participant-delete configuration
                                                              (participant-identifier human)
                                                              :recursive-p t)
                                    nil)
                           (world-error () t))
                         "deleting the only human would leave channels without a human owner")
            (world-channel-create configuration
                                  :id        "b-other"
                                  :owner     (participant-identifier human)
                                  :workspace workspace
                                  :now       2002)
            (let ((remote (world-participant-create configuration
                                                    :kind      ':agent
                                                    :name      "remote"
                                                    :parent    (participant-identifier lead)
                                                    :residence "b-other"
                                                    :now       2002)))
              (test-assert (handler-case
                               (progn (world-channel-delete configuration "b-task"
                                                            :participants (list (participant-identifier lead)
                                                                                (participant-identifier helper)))
                                      nil)
                             (world-error (condition)
                               (and (eq (world-error-operation condition) ':delete-channel)
                                    (world-channel-find (world-load configuration) "b-task")
                                    (world-participant-find (world-load configuration)
                                                            (participant-identifier remote)))))
                           "a descendant living elsewhere is never deleted implicitly, and nothing is written")
              (let ((deleted (world-channel-delete configuration "b-task"
                                                   :participants (list (participant-identifier lead)
                                                                       (participant-identifier helper)
                                                                       (participant-identifier remote))
                                                   :now          2003)))
                (test-assert (null (set-exclusive-or deleted
                                                     (list (participant-identifier lead)
                                                           (participant-identifier helper)
                                                           (participant-identifier remote))
                                                     :test #'string=))
                             "deleting a channel with its listed residents reports every deleted participant"))
              (let* ((world (world-load configuration))
                     (human (world-participant-find world (participant-identifier human))))
                (test-assert (and (null (world-channel-find world "b-task"))
                                  (null (world-participant-find world (participant-identifier lead)))
                                  (null (world-participant-find world (participant-identifier helper)))
                                  (null (world-participant-find world (participant-identifier remote)))
                                  (world-channel-find world "b-other")
                                  (world-channel-find world "b-lobby"))
                             "the channel and its residents are gone, the parent and the other channel stay")
                (test-assert (null (participant-subscriptions human))
                             "subscriptions to a deleted channel are dropped")))
            (let ((agent (world-participant-create configuration
                                                  :kind      ':agent
                                                  :name      "owner"
                                                  :parent    (participant-identifier human)
                                                  :residence "b-lobby"
                                                  :now       2004)))
              (world-channel-create configuration
                                    :id        "b-owned"
                                    :owner     (participant-identifier agent)
                                    :workspace workspace
                                    :now       2005)
              (world-participant-delete configuration (participant-identifier agent) :now 2006)
              (test-assert (string= (channel-owner (world-channel-find (world-load configuration)
                                                                       "b-owned"))
                                    (participant-identifier human))
                           "channels of a deleted participant pass to its parent")))))))
  nil)

(-> test-world-malformed-state () null)
(defun test-world-malformed-state ()
  "Test malformed world data is reported without reader evaluation or partial state."
  (with-test-configuration (configuration)
    (let ((pathname                   (configuration-world-path configuration))
          (*world-reader-evaluated-p* nil))
      (declare (special *world-reader-evaluated-p*))
      (ensure-directories-exist pathname)
      (with-open-file (stream pathname
                              :direction         ':output
                              :if-exists         ':supersede
                              :if-does-not-exist ':create)
        (write-string "(:world :version 1)" stream)
        (terpri stream)
        (write-string "#.(setf autolith::*world-reader-evaluated-p* t)" stream))
      (test-assert (handler-case (progn (world-load configuration) nil)
                     (world-error () t))
                   "malformed world data signals a world error")
      (test-assert (null *world-reader-evaluated-p*)
                   "world data disables reader evaluation")
      (with-open-file (stream pathname
                              :direction         ':output
                              :if-exists         ':supersede
                              :if-does-not-exist ':create)
        (write-string "(:world :version 1)" stream)
        (terpri stream)
        (write-string "(:participant :version 1 :id \"p\" :kind :agent :name \"x\" :residence \"missing\" :subscriptions nil :presence :offline :settings nil :created-at 1 :updated-at 1)"
                      stream))
      (test-assert (handler-case (progn (world-load configuration) nil)
                     (world-error (condition)
                       (equal (world-error-identifier condition) "p")))
                   "a dangling residence is reported with its participant")))
  nil)

(-> world-tests--persisted-conversation (configuration string timestamp) conversation)
(defun world-tests--persisted-conversation (configuration identifier created-at)
  "Create conversation IDENTIFIER with one durable message."
  (let ((conversation (conversation-create configuration
                                          :identifier identifier
                                          :created-at created-at)))
    (conversation-append-user-message conversation (format nil "hello from ~A" identifier))
    conversation))

(-> test-world-session-reconciliation () null)
(defun test-world-session-reconciliation ()
  "Test conversations map onto channels and agents, re-sync when newer, and retire when deleted."
  (with-test-configuration (configuration)
    (let ((first  (world-tests--persisted-conversation configuration "b-first" 1000))
          (second (world-tests--persisted-conversation configuration "b-second" 2000)))
      (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3000)
        (let ((human          (world-human-participant world))
              (first-channel  (world-channel-find world "b-first"))
              (second-channel (world-channel-find world "b-second")))
          (test-assert (= count 2) "every persisted conversation is migrated")
          (test-assert (and human first-channel second-channel
                            (string= (channel-owner first-channel) (participant-identifier human))
                            (string= (channel-owner second-channel) (participant-identifier human))
                            (= (channel-created-at first-channel) 1000)
                            (string= (channel-workspace first-channel)
                                     (workspace-directory-name
                                      (config :working-directory configuration))))
                       "each conversation becomes a channel of its identifier owned by the human")
          (let ((agent (find ':agent (world-channel-residents world "b-second")
                             :key #'participant-kind)))
            (test-assert (and agent
                              (equal (participant-parent agent) (participant-identifier human))
                              (world-handle-p (participant-name agent))
                              (uiop:string-prefix-p
                               (world-session--handle-base
                                (namestring (config :working-directory configuration)))
                               (participant-name agent))
                              (equal (getf (participant-settings agent) :model)
                                     (conversation-model second)))
                         "a migrated session's agent resides in its channel under the human, named after the workspace")
            (test-assert (= (nth-value 1 (world-reconcile-sessions configuration :now 3001)) 0)
                         "an unchanged conversation is not migrated again")
            (conversation-append-user-message first "a newer message")
            (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3002)
              (test-assert (= count 1) "a newer conversation is migrated again")
              (test-assert (equal (channel-title (world-channel-find world "b-first"))
                                  (conversation-title first))
                           "the channel title follows the conversation")
              (test-assert (= (length (remove ':human (world-participants world)
                                              :key #'participant-kind))
                              2)
                           "re-migration keeps the same agent participants"))
            (test-assert (equal (mapcar #'world-session-mapping-conversation
                                        (world-session-mappings configuration))
                                '("b-first" "b-second"))
                         "mappings are kept per conversation")
            (platform-delete-directory-tree
             *platform*
             (conversation-storage-directory-pathname (conversation-pathname second))
             :validate t)
            (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3003)
              (test-assert (= count 1) "a vanished conversation is retired")
              (test-assert (and (null (world-channel-find world "b-second"))
                                (null (world-participant-find world (participant-identifier agent)))
                                (string= (participant-residence (world-human-participant world))
                                         "b-first"))
                           "the retired channel and agent are gone and the human moved")
              (test-assert (equal (mapcar #'world-session-mapping-conversation
                                          (world-session-mappings configuration))
                                  '("b-first"))
                           "the retired mapping is removed"))
            (test-assert (string= (participant-residence
                                   (world-session-select configuration "b-first"))
                                  "b-first")
                         "selecting a conversation moves the human into its channel"))))))
  nil)

(-> test-world-application-sync () null)
(defun test-world-application-sync ()
  "Test the application hook migrates sessions and tolerates a missing conversation."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "b-hook" 1000)
    (let ((application (make-instance 'application
                                      :configuration configuration
                                      :ui            (terminal-ui-create
                                                      :terminal (make-instance 'recording-terminal
                                                                               :columns 80)))))
      (application-world-sync application :all-p t)
      (test-assert (world-channel-find (world-load configuration) "b-hook")
                   "startup sync migrates persisted conversations")
      (test-assert (handler-case (progn (application-world-sync application) t)
                     (warning () nil))
                   "sync without a conversation warns about nothing")
      (world-tests--persisted-conversation configuration "b-other" 2000)
      (let ((hosted (make-instance 'application
                                   :configuration configuration
                                   :ui            (terminal-ui-create
                                                   :terminal (make-instance 'recording-terminal
                                                                            :columns 80)))))
        (flet ((residence ()
                 "Return the channel the person lives in."
                 (participant-residence (world-human-participant (world-load configuration)))))
          (let ((before (residence)))
            (let ((*world-selection-p* nil))
              (application-world-sync hosted :all-p t))
            (test-assert (and (world-channel-find (world-load configuration) "b-other")
                              (string= (residence) before))
                         "a sync without selection, as a world server's, migrates but moves nobody")
            (world-session-select configuration "b-other")
            (test-assert (string= (residence) "b-other")
                         "an explicit selection is what moves the person"))))))
  nil)

(-> world-tests--read (configuration conversation string &rest t) string)
(defun world-tests--read (configuration conversation identifier &rest arguments)
  "Read channel IDENTIFIER for CONVERSATION with ARGUMENTS and return its text."
  (let* ((context  (make-instance 'tool-context
                                  :configuration configuration
                                  :worker        nil
                                  :conversation  conversation
                                  :registry      (make-instance 'tool-registry)))
         (tool     (make-instance 'resource-read-tool
                                  :namespace         "resource"
                                  :name              "read"
                                  :description       "Test resource read."
                                  :parameters        (tool-object-schema (json-object) '())
                                  :resource-registry (make-resource-registry)))
         (resolver (make-instance 'channel-resolver :scheme "channel"))
         (resource (resource-resolver-resolve resolver identifier context))
         (result   (resource-tool-read resource tool context (apply #'json-object arguments))))
    (test-assert (tool-result-success-p result) "channel resource reads succeed")
    (tool-result-content result)))

(-> test-world-channel-resource () null)
(defun test-world-channel-resource ()
  "Test channel: reads describe a channel and page its history."
  (with-test-configuration (configuration)
    (let ((first  (world-tests--persisted-conversation configuration "bfirst1" 1000))
          (second (world-tests--persisted-conversation configuration "bsecnd2" 2000)))
      (let ((listing (world-tests--read configuration first "list")))
        (test-assert (and (search "2 channels" listing)
                          (search "b-first1" listing)
                          (search "b-secnd2" listing)
                          (search "(agent " listing)
                          (search "user (human" listing))
                     "channel:list surveys every migrated conversation with its residents"))
      (let ((current (world-tests--read configuration first "current")))
        (test-assert (and (search "Channel b-first1" current)
                          (search "Residents: " current)
                          (search "hello from bfirst1" current))
                     "channel:current describes the current channel and reads its history"))
      (let ((other (world-tests--read configuration first "id/b-secnd2" "record-count" 1)))
        (test-assert (and (search "Channel b-secnd2" other)
                          (search "hello from bsecnd2" other)
                          (not (search "hello from bfirst1" other)))
                     "channel:id/<id> reads another channel's history with the usual arguments"))
      (conversation-append-user-message second "a second message")
      (test-assert (search "a second message"
                           (world-tests--read configuration first "id/bsecnd2"))
                   "a read reconciles a conversation that grew since the last read")
      (test-assert (search "Channel b-secnd2"
                           (world-tests--read configuration first "id/SECND"))
                   "channel:id/ accepts a unique fragment of the identifier")
      (test-assert (handler-case (progn (world-tests--read configuration first "id/bnone11") nil)
                     (tool-error (condition)
                       (eq (tool-error-code condition) ':not-found)))
                   "an unknown channel is a not-found failure")
      (test-assert (handler-case (progn (world-tests--read configuration first "id/b") nil)
                     (tool-error (condition)
                       (and (search "b-first1" (autolith-error-message condition))
                            (search "b-secnd2" (autolith-error-message condition)))))
                   "an ambiguous fragment lists every matching channel")))
  nil)

(-> test-world-channel-resolve () null)
(defun test-world-channel-resolve ()
  "Test channels resolve by full identifier, unique fragment, or report their candidates."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (let ((world (values (world-reconcile-sessions configuration))))
      (world-channel-update configuration "bsecnd2" :title "Second thoughts")
      (setf world (world-load configuration))
      (dolist (case '(("bfirst1" "bfirst1") ("b-first1" "bfirst1") ("FIRST" "bfirst1")
                      ("secnd" "bsecnd2") ("b-s" "bsecnd2")))
        (test-assert (string= (channel-identifier (world-channel-resolve world (first case)))
                              (second case))
                     "a full identifier or a unique fragment resolves to its channel"))
      (dolist (case '(("b" ("b-secnd2  Second thoughts" "b-first1"))
                      ("" ("does not exist"))
                      ("zzz" ("does not exist"))))
        (let ((condition (handler-case (progn (world-channel-resolve world (first case)) nil)
                           (world-error (condition) condition))))
          (test-assert (and condition
                            (every (lambda (detail)
                                     (search detail (autolith-error-message condition)))
                                   (second case)))
                       "several or no matches are reported with every candidate and title")))
      (test-assert (string= (channel-identifier
                             (world-channel-designated world configuration "b-first1"))
                            "bfirst1")
                   "a designated full identifier names its channel directly")
      (test-assert (string= (channel-identifier
                             (world-channel-designated world configuration "cnd"))
                            "bsecnd2")
                   "a designated fragment resolves like any other")))
  nil)

(-> test-world-channels-command () null)
(defun test-world-channels-command ()
  "Test (channels) presents every channel with its residents."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "b-listed" 1000)
    (let* ((terminal    (make-instance 'recording-terminal :columns 80))
           (ui          (terminal-ui-create :terminal terminal))
           (application (make-instance 'application
                                       :configuration configuration
                                       :ui            ui)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (test-assert (eq (application-command application "/channels") ':continue)
                          "/channels remains inside the interactive application")
             (let ((output (recording-terminal-output terminal)))
               (test-assert (and (search "channels" output)
                                 (search "b-listed" output)
                                 (search "(agent " output))
                            "/channels shows the channel and its agent resident")))
        (ignore-errors (terminal-ui-stop ui)))))
  nil)

(-> world-tests--agents (configuration) (values participant participant participant))
(defun world-tests--agents (configuration)
  "Return the human and the agents of channels bfirst1 and bsecnd2 after reconciliation."
  (world-reconcile-sessions configuration :now 3000)
  (let ((world (world-load configuration)))
    (flet ((agent (channel)
             "Return CHANNEL's agent resident."
             (find ':agent (world-channel-residents world channel) :key #'participant-kind)))
      (values (world-human-participant world) (agent "bfirst1") (agent "bsecnd2")))))

(-> test-world-message-send () null)
(defun test-world-message-send ()
  "Test messages reach a channel, its subscribers and mentioned participants, and settle by receipt."
  (with-test-configuration (configuration)
    (let ((first (world-tests--persisted-conversation configuration "bfirst1" 1000)))
      (world-tests--persisted-conversation configuration "bsecnd2" 2000)
      (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
        (world-participant-subscribe configuration (participant-identifier second-agent) "bfirst1")
        (let ((result (world-message-send configuration
                                          :sender   (participant-identifier human)
                                          :channel  "bfirst1"
                                          :text     "hello @autolith"
                                          :notify-p nil)))
          (test-assert (equal (getf result :channels) '("bfirst1" "bsecnd2"))
                       "a post reaches the channel and each subscriber's residence")
          (let ((post (first (world-inbox-deliveries configuration "bfirst1")))
                (copy (first (world-inbox-deliveries configuration "bsecnd2"))))
            (test-assert (and (eq (world-delivery-kind post) ':post)
                              (eq (world-delivery-kind copy) ':subscription)
                              (string= (world-delivery-identifier post)
                                       (world-delivery-identifier copy))
                              (null (world-delivery-mentions post))
                              (search "where you reside" (world-delivery-label post))
                              (search "which you subscribe to" (world-delivery-label copy)))
                         "copies share the identifier and are labeled by why they arrived")))
        (let ((result (world-message-send configuration
                                          :sender   (participant-identifier second-agent)
                                          :channel  "bsecnd2"
                                          :text     (format nil "see @~A please"
                                                            (conversation-identifier-display
                                                             (participant-identifier first-agent)))
                                          :notify-p nil)))
          (test-assert (equal (getf result :channels) '("bsecnd2" "bfirst1"))
                       "a mention reaches the mentioned participant's residence")
          (let ((mention (second (world-inbox-deliveries configuration "bfirst1"))))
            (test-assert (and (eq (world-delivery-kind mention) ':mention)
                              (equal (world-delivery-mentions mention)
                                     (list (participant-identifier first-agent))))
                         "the mention names the participant and is labeled as one")))
        (let ((delivery (first (world-inbox-deliveries configuration "bfirst1"))))
          (conversation-append-user-message first (world-delivery-label delivery)
                                            :pending-input-identifier
                                            (world-delivery-pending-identifier delivery)
                                            :automatic-p t)
          (multiple-value-bind (pending settled)
              (world-inbox-unsettled configuration "bfirst1" first)
            (test-assert (and (= (length pending) 1)
                              (equal settled (list (world-delivery-identifier delivery))))
                         "a recorded receipt settles its delivery")
            (world-inbox-settle configuration "bfirst1" settled)
            (test-assert (= (length (world-inbox-deliveries configuration "bfirst1")) 1)
                         "settled deliveries leave the inbox")))
        (test-assert (handler-case
                         (progn (world-message-send configuration
                                                    :sender   (participant-identifier human)
                                                    :channel  "bnone11"
                                                    :text     "lost"
                                                    :notify-p nil)
                                nil)
                       (world-error () t))
                     "sending to an unknown channel is refused")
        (test-assert (handler-case
                         (progn (world-message-send configuration
                                                    :sender   (participant-identifier human)
                                                    :channel  "bfirst1"
                                                    :text     "for someone"
                                                    :mentions '("nobody")
                                                    :notify-p nil)
                                nil)
                       (world-error () t))
                     "an unknown explicit mention is refused"))))
  nil)

(-> test-world-message-outbox () null)
(defun test-world-message-outbox ()
  "Test a send is recorded before fan-out, resumes under its identifier, and reaches each inbox once."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (declare (ignore first-agent))
      (world-participant-subscribe configuration (participant-identifier second-agent) "bfirst1")
      (flet ((delivery (identifier target)
               "Return a post IDENTIFIER from the person to TARGET."
               (make-instance 'world-delivery
                              :identifier  identifier
                              :time        1
                              :sender      (participant-identifier human)
                              :sender-name "user"
                              :source      "bfirst1"
                              :target      target
                              :text        identifier
                              :mentions    nil
                              :kind        ':post)))
        (let ((*world-inbox-limit* 1))
          (world-inbox-append configuration "bsecnd2" (list (delivery "filler" "bsecnd2")))
          (let* ((result (world-message-send configuration
                                             :sender   (participant-identifier human)
                                             :channel  "bfirst1"
                                             :text     "hello"
                                             :notify-p nil))
                 (id     (getf result :id)))
            (test-assert (and id
                              (equal (getf result :channels) '("bfirst1"))
                              (equal (getf result :pending) '("bsecnd2")))
                         "the target receives the post while a full subscriber inbox leaves its copy pending")
            (test-assert (equal (mapcar #'first (world-outbox-pending configuration id))
                                '("bsecnd2"))
                         "the pending copy waits in the outbox under the send's identifier")
            (test-assert (and (null (world-message-resume configuration :notify-p nil))
                              (= 1 (length (world-inbox-deliveries configuration "bfirst1"))))
                         "a retry against the full inbox delivers nothing and repeats nothing")
            (world-inbox-settle configuration "bfirst1" (list id))
            (world-inbox-settle configuration "bsecnd2" '("filler"))
            (test-assert (equal (world-message-resume configuration :notify-p nil) '("bsecnd2"))
                         "once the inbox drains the pending copy is delivered")
            (let ((copy (first (world-inbox-deliveries configuration "bsecnd2"))))
              (test-assert (and copy
                                (string= (world-delivery-identifier copy) id)
                                (eq (world-delivery-kind copy) ':subscription))
                           "the copy keeps the send's identifier and its reason"))
            (test-assert (and (null (world-inbox-deliveries configuration "bfirst1"))
                              (not (probe-file (world-outbox-pathname configuration id)))
                              (null (world-outbox-identifiers configuration)))
                         "a recipient that read its copy gets no second one, and the record is gone"))
          (world-inbox-settle configuration "bsecnd2"
                              (mapcar #'world-delivery-identifier
                                      (world-inbox-deliveries configuration "bsecnd2")))
          (world-inbox-append configuration "bsecnd2" (list (delivery "filler2" "bsecnd2")))
          (let* ((result (world-message-send configuration
                                             :sender   (participant-identifier human)
                                             :channel  "bfirst1"
                                             :text     "again"
                                             :notify-p nil))
                 (id     (getf result :id))
                 (taken  (make-hash-table :test #'equal)))
            (test-assert (equal (getf result :pending) '("bsecnd2"))
                         "a second send against the full inbox leaves its copy pending again")
            (setf (gethash "channel:filler2" taken) ':taken)
            (test-assert (world-inbox-steering-ack configuration
                                                   :channel    "bsecnd2"
                                                   :identifier "channel:filler2"
                                                   :taken      taken)
                         "a steering receipt settles the delivery it names")
            (test-assert (and (= 1 (length (world-inbox-deliveries configuration "bsecnd2")))
                              (string= (world-delivery-identifier
                                        (first (world-inbox-deliveries configuration "bsecnd2")))
                                       id)
                              (null (world-outbox-identifiers configuration)))
                         "settling by receipt frees the inbox and delivers the pending copy at once")
            (world-inbox-settle configuration "bfirst1" (list id))
            (world-inbox-settle configuration "bsecnd2" (list id))))
        (world-outbox-write configuration "crash11"
                            (list (cons "bfirst1" (delivery "crash11" "bfirst1"))
                                  (cons "bsecnd2" (delivery "crash11" "bsecnd2"))))
        (world-inbox-append configuration "bfirst1" (list (delivery "crash11" "bfirst1")))
        (test-assert (equal (world-message-resume configuration :notify-p nil) '("bfirst1" "bsecnd2"))
                     "a send cut off between two inboxes resumes")
        (test-assert (and (= 1 (count "crash11" (world-inbox-deliveries configuration "bfirst1")
                                      :key #'world-delivery-identifier :test #'string=))
                          (= 1 (count "crash11" (world-inbox-deliveries configuration "bsecnd2")
                                      :key #'world-delivery-identifier :test #'string=))
                          (null (world-outbox-identifiers configuration)))
                     "the inbox that had the copy keeps one, the other gets one, the record is gone"))))
  nil)

(-> test-world-channel-tools () null)
(defun test-world-channel-tools ()
  "Test the channel tools act as the conversation's participant."
  (with-test-configuration (configuration)
    (let ((first (world-tests--persisted-conversation configuration "bfirst1" 1000)))
      (world-tests--persisted-conversation configuration "bsecnd2" 2000)
      (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
        (declare (ignore human second-agent))
        (let* ((registry (channel-register-tools (make-instance 'tool-registry)))
               (agent    (agent-create :configuration configuration
                                       :conversation  first
                                       :provider      (make-instance 'model-provider)
                                       :tool-registry registry
                                       :worker        nil))
               (context  (make-instance 'tool-context
                                        :configuration configuration
                                        :conversation  first
                                        :agent         agent
                                        :registry      registry
                                        :worker        nil)))
          (flet ((call (name &rest arguments)
                   "Execute channel tool NAME with ARGUMENTS and return its result."
                   (tool-registry-execute-call
                    registry
                    (agent-test-call :namespace "channel"
                                     :name      name
                                     :arguments (json-encode (apply #'json-object arguments)))
                    context)))
            (test-assert (tool-result-success-p (call "send" "channel" "b-secnd2" "text" "ping"))
                         "channel.send succeeds for a known channel")
            (let ((delivery (first (world-inbox-deliveries configuration "bsecnd2"))))
              (test-assert (and delivery
                                (string= (world-delivery-sender delivery)
                                         (participant-identifier first-agent))
                                (string= (world-delivery-text delivery) "ping"))
                           "the message is sent as the conversation's agent participant"))
            (test-assert (not (tool-result-success-p (call "send" "channel" "bnone11" "text" "x")))
                         "channel.send to an unknown channel fails")
            (test-assert (tool-result-success-p (call "subscribe" "channel" "bsecnd2"))
                         "channel.subscribe succeeds")
            (test-assert (equal (participant-subscriptions
                                 (world-participant-find (world-load configuration)
                                                         (participant-identifier first-agent)))
                                '("bsecnd2"))
                         "the subscription is recorded on the participant")
            (test-assert (tool-result-success-p (call "unsubscribe" "channel" "bsecnd2"))
                         "channel.unsubscribe succeeds")
            (test-assert (null (participant-subscriptions
                                (world-participant-find (world-load configuration)
                                                        (participant-identifier first-agent))))
                         "the subscription is removed"))))))
  nil)

(-> test-world-handles () null)
(defun test-world-handles ()
  "Test agents get unique workspace handles, duplicates are renamed, and renaming is checked."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (let ((base  (world-session--handle-base
                    (namestring (config :working-directory configuration))))
            (names (list (participant-name first-agent) (participant-name second-agent))))
        (test-assert (and (member base names :test #'string=)
                          (member (format nil "~A-2" base) names :test #'string=))
                     "two agents in one workspace are named after it, the second numbered")
        (world-participant-create configuration
                                  :kind      ':agent
                                  :name      (participant-name first-agent)
                                  :residence "bfirst1"
                                  :parent    (participant-identifier human)
                                  :now       3000)
        (let* ((world (values (world-reconcile-sessions configuration :now 3001)))
               (all   (mapcar #'participant-name (world-participants world))))
          (test-assert (and (= (length all) (length (remove-duplicates all :test #'string-equal)))
                            (string= (participant-name
                                      (world-participant-find world
                                                              (participant-identifier first-agent)))
                                     (participant-name first-agent)))
                       "reconciliation renames a later agent sharing a name and keeps the older one"))
        (let ((renamed (world-participant-rename configuration
                                                 (participant-identifier first-agent)
                                                 "Frobnicator")))
          (test-assert (string= (participant-name renamed) "Frobnicator")
                       "a participant takes a new handle"))
        (dolist (name (list "bad name" "" (participant-name second-agent)
                            (string-upcase (participant-name second-agent))))
          (test-assert (handler-case
                           (progn (world-participant-rename configuration
                                                           (participant-identifier first-agent)
                                                           name)
                                  nil)
                         (world-error () t))
                       "a handle with other characters, or one another participant has, is refused"))
        (let ((world (world-load configuration)))
          (test-assert (and (eq (world-participant-designated world "frobnicator")
                                (world-participant-find world (participant-identifier first-agent)))
                            (eq (world-participant-designated
                                 world (conversation-identifier-display
                                        (participant-identifier second-agent)))
                                (world-participant-find world (participant-identifier second-agent)))
                            (null (world-participant-designated world "nobody")))
                       "participants are designated by name, display identifier or unique fragment"))
        (let* ((leaf (make-string *world-handle-limit* :initial-element #\a))
               (long (configuration-copy configuration
                                         :working-directory
                                         (merge-pathnames (concatenate 'string leaf "/")
                                                          (config :working-directory configuration)))))
          (world-tests--persisted-conversation long "bthird3" 4000)
          (world-tests--persisted-conversation long "bfourt4" 5000)
          (flet ((names ()
                   "Return every participant name after a reconciliation, sorted."
                   (sort (mapcar #'participant-name
                                 (world-participants (values (world-reconcile-sessions long :now 6000))))
                         #'string<)))
            (let ((first-pass (names)))
              (test-assert (every #'world-handle-p first-pass)
                           "agents in a workspace at the handle length limit get valid numbered handles")
              (test-assert (equal first-pass (names))
                           "a second reconciliation of colliding maximum-length handles changes nothing")))))))
  nil)

(-> test-world-composer-mentions () null)
(defun test-world-composer-mentions ()
  "Test a typed message reaches the participants it mentions elsewhere, and mentions are highlighted."
  (with-test-configuration (configuration)
    (let ((first (world-tests--persisted-conversation configuration "bfirst1" 1000)))
      (world-tests--persisted-conversation configuration "bsecnd2" 2000)
      (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
        (let* ((terminal    (make-instance 'recording-terminal :columns 80))
               (application (make-instance 'application
                                           :configuration configuration
                                           :conversation  first
                                           :ui            (terminal-ui-create :terminal terminal)))
               (other       (participant-name second-agent)))
          (let ((entries (world-mention-completion-entries (world-load configuration)
                                                           (subseq other 0 (min 3 (length other))))))
            (test-assert (and (find (format nil "@~A" other) entries
                                    :key (lambda (entry) (getf entry :name)) :test #'string=)
                              (every (lambda (entry) (search " in b-" (getf entry :description)))
                                     entries))
                         "mention completion offers the handles starting with the typed text"))
          (test-assert (null (application-world-share
                              application (format nil "hi @~A" (participant-name first-agent))))
                       "mentioning this channel's own agent tells nobody else")
          (test-assert (equal (application-world-share
                               application (format nil "ask @~A about it" other))
                              '("bsecnd2"))
                       "mentioning an agent elsewhere tells its channel")
          (let ((delivery (first (world-inbox-deliveries configuration "bsecnd2"))))
            (test-assert (and delivery
                              (eq (world-delivery-kind delivery) ':mention)
                              (string= (world-delivery-sender delivery)
                                       (participant-identifier human))
                              (string= (world-delivery-target delivery) "bfirst1"))
                         "the copy arrives as a mention from the person in their channel"))
          (test-assert (null (world-inbox-deliveries configuration "bfirst1"))
                       "nothing is posted back to the person's own channel")
          (test-assert (null (application-world-share application "a plain remark"))
                       "a plain message with no subscriber elsewhere tells nobody")
          (world-participant-subscribe configuration (participant-identifier second-agent) "bfirst1")
          (test-assert (equal (application-world-share application "a plain remark") '("bsecnd2"))
                       "a plain message reaches a subscriber residing elsewhere")
          (let ((copy (first (last (world-inbox-deliveries configuration "bsecnd2")))))
            (test-assert (and copy
                              (eq (world-delivery-kind copy) ':subscription)
                              (string= (world-delivery-text copy) "a plain remark")
                              (null (world-inbox-deliveries configuration "bfirst1")))
                         "the subscriber's copy is labeled a subscription and nothing is posted back"))
          (test-assert (equal (application-world-share-reply application "the agent answers") '("bsecnd2"))
                       "the agent's reply reaches the subscriber elsewhere too")
          (let ((copy (first (last (world-inbox-deliveries configuration "bsecnd2")))))
            (test-assert (and copy
                              (eq (world-delivery-kind copy) ':subscription)
                              (string= (world-delivery-sender copy)
                                       (participant-identifier first-agent))
                              (string= (world-delivery-text copy) "the agent answers"))
                         "the reply's copy comes from this channel's agent"))
          (test-assert (null (application-world-share-reply application nil))
                       "a reply without verbal text shares nothing")
          (let* ((fresh  (conversation-create configuration :identifier "bfresh4" :created-at 4000))
                 (new    (make-instance 'application
                                        :configuration configuration
                                        :conversation  fresh
                                        :ui            (terminal-ui-create
                                                        :terminal (make-instance 'recording-terminal
                                                                                 :columns 80))))
                 (text   (format nil "ask @~A now" other))
                 (before (length (world-inbox-deliveries configuration "bsecnd2"))))
            (test-assert (and (null (application-world-share new text))
                              (string= (application-world-pending-share new) text)
                              (null (application-world-share-pending new))
                              (= before (length (world-inbox-deliveries configuration "bsecnd2"))))
                         "a message typed before the conversation persists waits, and sharing it early sends nothing")
            (conversation-append-user-message fresh text)
            (test-assert (and (equal (application-world-share-pending new) '("bsecnd2"))
                              (= (1+ before) (length (world-inbox-deliveries configuration "bsecnd2")))
                              (null (application-world-share-pending new)))
                         "once the first message persists the waiting share goes out exactly once"))
          (let* ((entry  (application--user-message-entry
                          application (format nil "ping @~A, @user and @nobody" other) 1000))
                 (styled (loop for span in entry
                               when (and (consp span) (stringp (rest span))
                                         (plusp (length (rest span)))
                                         (char= (char (rest span) 0) #\@))
                                 collect span)))
            (test-assert (equal styled (list (cons ':legend-accent (format nil "@~A" other))
                                             (cons ':user "@user")))
                         "mentions of others are accented, of the person styled as the user, unknown ones left alone")))))
    nil))

(-> test-world-presentation-cache () null)
(defun test-world-presentation-cache ()
  "Test the presentation world follows every change to the log, even within one second."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent) (world-tests--agents configuration)
      (declare (ignore human))
      (let* ((terminal    (make-instance 'recording-terminal :columns 80))
             (application (make-instance 'application
                                         :configuration configuration
                                         :ui            (terminal-ui-create :terminal terminal)))
             (identifier  (participant-identifier first-agent))
             (loads       0))
        (flet ((name ()
                 "Return the agent's name as the application presents it."
                 (participant-name (world-participant-find (application-world application)
                                                           identifier))))
          (test-call-with-function-replacements
           (list (list 'world-load
                       (let ((load (symbol-function 'world-load)))
                         (lambda (candidate)
                           (incf loads)
                           (funcall load candidate)))))
           (lambda ()
             (name)
             (name)
             (test-assert (= loads 1) "an unchanged log is read once")
             (world-participant-rename configuration identifier "first-name")
             (world-participant-rename configuration identifier "second-name")
             (test-assert (string= (name) "second-name")
                          "two renames in one second both reach presentation")
             (test-assert (= loads 2) "a changed log is read again once"))))))
    nil))

(-> test-world-mention-highlighting () null)
(defun test-world-mention-highlighting ()
  "Test highlighting styles the mentions it resolves and keeps every character of the text."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (declare (ignore human))
      (let* ((world (world-load configuration))
             (known (participant-name first-agent))
             (other (participant-name second-agent))
             (texts (list "email a@example.com tomorrow"
                          "@nobody said hi"
                          (format nil "@~A then @nobody" known)
                          (format nil "@nobody then @~A" known)
                          (format nil "@~A@~A" known other)
                          "a bare @ and @ again"
                          ""
                          (format nil "@~A" known)
                          (format nil "tail @~A" known))))
        (dolist (text texts)
          (let ((pieces (world-mention--split-span world (cons ':plain text))))
            (test-assert (string= text (format nil "~{~A~}" (mapcar #'rest pieces)))
                         (format nil "highlighting keeps the text of ~S" text))
            (test-assert (every (lambda (piece)
                                  (or (eq (first piece) ':plain)
                                      (and (member (first piece) '(:legend-accent :user))
                                           (char= (char (rest piece) 0) #\@))))
                                pieces)
                         "only resolved mentions change style")))
        (let ((pieces (world-mention--split-span
                       world (cons ':plain (format nil "@~A then @nobody" known)))))
          (test-assert (and (eq (first (first pieces)) ':legend-accent)
                            (string= (rest (first pieces)) (format nil "@~A" known))
                            (string= (rest (second pieces)) " then @nobody"))
                       "a known mention is styled and the unknown one stays in the plain text")))))
  nil)

(-> test-world-mention-boundaries () null)
(defun test-world-mention-boundaries ()
  "Test an @ inside a word, an address or a path mentions nobody, in sends and in highlighting alike."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (declare (ignore human second-agent))
      (let* ((world (world-load configuration))
             (known (participant-name first-agent))
             (id    (participant-identifier first-agent))
             (cases (list (list (format nil "Internal account alice@~A.com" known) nil)
                          (list (format nil "mail me at alice@~A" known) nil)
                          (list (format nil "see src/@~A/notes" known) nil)
                          (list (format nil "v1.2@~A" known) nil)
                          (list (format nil "@~A" known) (list id))
                          (list (format nil "ask @~A." known) (list id))
                          (list (format nil "(@~A)" known) (list id))
                          (list (format nil "hi, @~A, and @~A" known known) (list id))
                          (list (format nil "quote \"@~A\"" known) (list id))
                          (list (format nil "newline~%@~A" known) (list id)))))
        (dolist (case cases)
          (destructuring-bind (text expected) case
            (test-assert (equal (world-message-mentions world text) expected)
                         (format nil "~S mentions ~S" text expected))
            (let* ((pieces (world-mention--split-span world (cons ':plain text)))
                   (styled (remove ':plain pieces :key #'first)))
              (test-assert (and (string= text (format nil "~{~A~}" (mapcar #'rest pieces)))
                                (eq (not (null styled)) (not (null expected))))
                           (format nil "highlighting of ~S agrees with the scanner" text)))))
        (test-assert (equal (getf (world-message-send configuration
                                                      :sender   (participant-identifier
                                                                 (world-human-participant world))
                                                      :channel  "bsecnd2"
                                                      :text     (format nil "Internal account alice@~A.com"
                                                                        known)
                                                      :notify-p nil)
                                  :channels)
                            '("bsecnd2"))
                     "an address in a sent message adds no recipient"))))
  nil)

(-> test-world-message-presentation () null)
(defun test-world-message-presentation ()
  "Test delivered channel messages show their sender and channel, and delivery without a controller waits."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (declare (ignore first-agent second-agent))
      (let* ((terminal    (make-instance 'recording-terminal :columns 80))
             (application (make-instance 'application
                                         :configuration configuration
                                         :ui            (terminal-ui-create :terminal terminal)))
             (result      (world-message-send configuration
                                              :sender   (participant-identifier human)
                                              :channel  "bsecnd2"
                                              :text     "look at **this**"
                                              :notify-p nil))
             (delivery    (first (world-inbox-deliveries configuration "bsecnd2")))
             (entry       (conversation-record-entry
                           application
                           (list :message
                                 :role              :user
                                 :content           (world-delivery-label delivery)
                                 :automatic-p       t
                                 :operation-request (world-delivery-request delivery)
                                 :time              1000)))
             (text        (format nil "~{~A~}"
                                  (loop for span in entry
                                        when (and (consp span) (stringp (rest span)))
                                          collect (rest span)))))
        (test-assert (getf result :id) "sending returns the delivery identifier")
        (test-assert (and (search "user" text)
                          (search "[channel b-secnd2]" text)
                          (search "look at" text))
                     "a channel message shows its sender and channel, then its text")
        (test-assert (zerop (application-world-deliver application))
                     "without an input controller, deliveries stay in the inbox")
        (test-assert (= (length (world-inbox-deliveries configuration "bsecnd2")) 1)
                     "the inbox keeps the undelivered message")
        (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
          (declare (ignore human))
          (world-message-send configuration
                              :sender   (participant-identifier second-agent)
                              :channel  "bsecnd2"
                              :text     (format nil "@~A please look at the secret plan"
                                                (participant-name first-agent))
                              :notify-p nil)
          (let* ((copy  (first (world-inbox-deliveries configuration "bfirst1")))
                 (entry (conversation-record-entry
                         application
                         (list :message
                               :role              :user
                               :content           (world-delivery-label copy)
                               :automatic-p       t
                               :operation-request (world-delivery-request copy)
                               :time              1001)))
                 (text  (format nil "~{~A~}"
                                (loop for span in entry
                                      when (and (consp span) (stringp (rest span)))
                                        collect (rest span)))))
            (test-assert (and (eq (world-delivery-kind copy) ':mention)
                              (search (format nil "@~A mentioned from #b-secnd2 by ~A"
                                              (participant-name first-agent)
                                              (participant-name second-agent))
                                      text)
                              (not (search "secret plan" text))
                              (search "secret plan" (world-delivery-label copy)))
                         "a copy from another channel shows as one line without its text, which the model still gets"))))))
  nil)

(-> test-world-one-resident-agent () null)
(defun test-world-one-resident-agent ()
  "Test a channel holds one resident agent: an agent is refused a channel another agent lives in, the person is not."
  (with-test-configuration (configuration root)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
      (test-assert (handler-case
                       (progn (world-participant-move configuration
                                                      (participant-identifier first-agent)
                                                      "bsecnd2")
                              nil)
                     (world-error (condition)
                       (and (eq (world-error-operation condition) ':move-participant)
                            (search (participant-name second-agent)
                                    (autolith-error-message condition)))))
                   "an agent is refused a channel whose resident agent is named in the refusal")
      (test-assert (string= (participant-residence
                             (world-participant-move configuration
                                                     (participant-identifier human) "bsecnd2"))
                            "bsecnd2")
                   "the person moves into a channel with an agent")
      (world-channel-create configuration
                            :id        "bempty3"
                            :owner     (participant-identifier human)
                            :workspace (world-tests--workspace root "empty")
                            :now       3000)
      (test-assert (string= (participant-residence
                             (world-participant-move configuration
                                                     (participant-identifier first-agent) "bempty3"))
                            "bempty3")
                   "an agent moves into a channel without one")
      (test-assert (string= (participant-residence
                             (world-participant-move configuration
                                                     (participant-identifier first-agent) "bempty3"))
                            "bempty3")
                   "moving an agent where it already is changes nothing")))
  nil)

(-> test-world-staggered-retirement () null)
(defun test-world-staggered-retirement ()
  "Test a root whose storage vanishes before its child's keeps its mapping and retires with the child."
  (with-test-configuration (configuration)
    (let ((root  (world-tests--persisted-conversation configuration "broot11" 1000))
          (child (conversation-create configuration
                                      :identifier   "exec-one"
                                      :storage-root (merge-pathnames
                                                     "tasks/broot11/exec-one/"
                                                     (config :data-root configuration))
                                      :created-at   1500)))
      (conversation-append-user-message child "do the task")
      (world-tests--persisted-conversation configuration "bthird3" 2500)
      (world-reconcile-sessions configuration :now 3000)
      (platform-delete-directory-tree *platform*
                                      (conversation-storage-directory-pathname
                                       (conversation-pathname root))
                                      :validate t)
      (let ((world (values (world-reconcile-sessions configuration :now 3001))))
        (test-assert (and (world-channel-find world "broot11")
                          (world-channel-find world "exec-one")
                          (find ':agent (world-channel-residents world "broot11") :key #'participant-kind)
                          (member "broot11" (mapcar #'world-session-mapping-conversation
                                                    (world-session-mappings configuration))
                                  :test #'string=))
                     "a root whose child still lives keeps its channel, its agent and its mapping"))
      (platform-delete-directory-tree *platform*
                                      (merge-pathnames "tasks/broot11/"
                                                       (config :data-root configuration))
                                      :validate t)
      (let ((world (values (world-reconcile-sessions configuration :now 3002))))
        (test-assert (and (null (world-channel-find world "exec-one"))
                          (null (world-channel-find world "broot11"))
                          (equal (mapcar #'channel-identifier (world-channels world)) '("bthird3"))
                          (equal (mapcar #'world-session-mapping-conversation
                                         (world-session-mappings configuration))
                                 '("bthird3")))
                     "once the child goes, the root retires with it and no mapping lingers"))
      (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3003)
        (test-assert (and (zerop count)
                          (equal (mapcar #'channel-identifier (world-channels world)) '("bthird3")))
                     "a later reconciliation has nothing left to retire"))))
  nil)

(-> test-world-child-participants () null)
(defun test-world-child-participants ()
  "Test child task conversations become nested channels with steerable participants."
  (with-test-configuration (configuration)
    (let* ((root  (world-tests--persisted-conversation configuration "broot11" 1000))
           (child (conversation-create configuration
                                       :identifier   "exec-one"
                                       :storage-root (merge-pathnames
                                                      "tasks/broot11/exec-one/"
                                                      (config :data-root configuration))
                                       :created-at   1500)))
      (conversation-append-user-message child "do the task")
      (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3000)
        (let ((root-agent (find ':agent (world-channel-residents world "broot11")
                                :key #'participant-kind))
              (channel    (world-channel-find world "exec-one"))
              (task-agent (first (world-channel-residents world "exec-one"))))
          (test-assert (= count 2) "the root and its child are migrated")
          (test-assert (and channel
                            (equal (channel-parent channel) "broot11")
                            (string= (channel-owner channel)
                                     (participant-identifier root-agent)))
                       "a child channel nests under its root and belongs to the root's agent")
          (test-assert (and task-agent
                            (string= (participant-name task-agent) "task")
                            (equal (participant-parent task-agent)
                                   (participant-identifier root-agent)))
                       "a child's participant is named task under the root's agent")
          (test-assert (string= (participant-residence (world-human-participant world)) "broot11")
                       "the human stays in the root channel")
          (let ((text (world-tests--read configuration root "id/exec-one")))
            (test-assert (and (search "Channel exec-one" text)
                              (search "nested under b-root11" text)
                              (search "do the task" text))
                         "channel:id/<execution> reads a child's history"))
          (world-message-send configuration
                              :sender   (participant-identifier root-agent)
                              :channel  "exec-one"
                              :text     "stop and report"
                              :notify-p nil)
          (let* ((taken   (make-hash-table :test #'equal))
                 (entries (world-inbox-steering-take configuration
                                                     :channel      "exec-one"
                                                     :conversation child
                                                     :taken        taken)))
            (test-assert (and (= (length entries) 1)
                              (search "stop and report"
                                      (agent-steering-input-content (first entries))))
                         "a message to a child's channel becomes a labeled steer")
            (test-assert (null (world-inbox-steering-take configuration
                                                          :channel      "exec-one"
                                                          :conversation child
                                                          :taken        taken))
                         "a taken message is not taken twice")
            (test-assert (world-inbox-steering-ack configuration
                                                   :channel    "exec-one"
                                                   :identifier (agent-steering-input-identifier
                                                                (first entries))
                                                   :taken      taken)
                         "the steer's receipt settles the delivery")
            (test-assert (null (world-inbox-deliveries configuration "exec-one"))
                         "the settled delivery leaves the inbox"))
          (let* ((registry   (channel-register-tools (make-instance 'tool-registry)))
                 (definition (make-instance 'task-agent-definition
                                            :name "worker" :description "worker"
                                            :instructions "work" :source ':test :tools ':all))
                 (agent      (agent-create :configuration configuration
                                           :conversation  child
                                           :provider      (make-instance 'model-provider)
                                           :tool-registry registry
                                           :worker        nil))
                 (context    (make-instance 'tool-context
                                            :configuration configuration
                                            :conversation  child
                                            :agent         agent
                                            :registry      registry
                                            :worker        nil)))
            (test-assert (every (lambda (tool) (task--definition-allows-tool-p definition tool))
                                (tool-registry-tools registry))
                         "a child definition with every tool admits the channel tools")
            (test-assert (tool-result-success-p
                          (tool-registry-execute-call
                           registry
                           (agent-test-call :namespace "channel"
                                            :name      "send"
                                            :arguments (json-encode (json-object "channel" "b-root11"
                                                                                 "text"    "done here")))
                           context))
                         "a child sends to its parent's channel through the channel tool")
            (let ((delivery (first (last (world-inbox-deliveries configuration "broot11")))))
              (test-assert (and delivery
                                (string= (world-delivery-sender delivery)
                                         (participant-identifier task-agent))
                                (string= (world-delivery-text delivery) "done here"))
                           "the message arrives as the child's own participant")))
          (world-tests--persisted-conversation configuration "bthird3" 2500)
          (platform-delete-directory-tree *platform*
                                          (merge-pathnames "tasks/broot11/"
                                                           (config :data-root configuration))
                                          :validate t)
          (platform-delete-directory-tree *platform*
                                          (conversation-storage-directory-pathname
                                           (conversation-pathname root))
                                          :validate t)
          (let ((world (values (world-reconcile-sessions configuration :now 3001))))
            (test-assert (and (null (world-channel-find world "exec-one"))
                              (null (world-channel-find world "broot11"))
                              (null (world-participant-find
                                     world (participant-identifier task-agent)))
                              (null (world-participant-find
                                     world (participant-identifier root-agent)))
                              (string= (participant-residence (world-human-participant world))
                                       "bthird3"))
                         "pruning a root with its child retires both channels and participants, parent last")
            (test-assert (equal (mapcar #'world-session-mapping-conversation
                                        (world-session-mappings configuration))
                                '("bthird3"))
                         "only the surviving conversation keeps a mapping"))
          (multiple-value-bind (world count) (world-reconcile-sessions configuration :now 3002)
            (test-assert (and (zerop count)
                              (equal (mapcar #'channel-identifier (world-channels world))
                                     '("bthird3")))
                         "a second reconciliation finds nothing left to retire"))))))
  nil)

(-> test-world-agent-move () null)
(defun test-world-agent-move ()
  "Test a dismissal leaves a channel without an agent, a move brings one and carries the mapping along, and neither invents an agent later."
  (with-test-configuration (configuration root)
    (let ((first  (world-tests--persisted-conversation configuration "bfirst1" 1000))
          (second (world-tests--persisted-conversation configuration "bsecnd2" 2000)))
      (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
        (flet ((agent-of (channel)
                 "Return CHANNEL's resident agent."
                 (find ':agent (world-channel-residents (world-load configuration) channel)
                       :key #'participant-kind)))
          (test-assert (null (world-session-remap configuration "bnone99" nil))
                       "a channel without a session mapping has nothing to rewrite")
          (world-channel-create configuration
                                :id        "btask11"
                                :owner     (participant-identifier human)
                                :workspace (world-tests--workspace root "task")
                                :parent    "bfirst1"
                                :now       2500)
          (let ((child (world-participant-create configuration
                                                 :kind      ':agent
                                                 :name      "task"
                                                 :residence "btask11"
                                                 :parent    (participant-identifier first-agent))))
            (test-assert (handler-case
                             (progn (world-dismiss-agent configuration first-agent) nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':delete-participant)))
                         "an agent with a child participant is not dismissed")
            (test-assert (handler-case
                             (progn (world-dismiss-agent configuration human) nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':dismiss)))
                         "the person is not dismissed")
            (world-participant-delete configuration (participant-identifier child)))
          (let ((result (world-dismiss-agent configuration second-agent)))
            (test-assert (and (string= (getf result :channel) "bsecnd2")
                              (zerop (getf result :dropped))
                              (null (agent-of "bsecnd2"))
                              (null (world-participant-find (world-load configuration)
                                                            (participant-identifier second-agent)))
                              (null (world-session-participant configuration second)))
                         "a dismissed agent is gone and its channel has none"))
          (conversation-append-user-message second "still here")
          (world-reconcile-sessions configuration :now 3500)
          (test-assert (null (agent-of "bsecnd2"))
                       "a newer conversation in a channel without an agent invents none")
          (let ((result (world-move-agent configuration first-agent "bsecnd2")))
            (test-assert (and (string= (getf result :source) "bfirst1")
                              (string= (getf result :channel) "bsecnd2")
                              (zerop (getf result :dropped)))
                         "a move reports the channel left and the destination, dropping nothing without an application"))
          (test-assert (and (null (agent-of "bfirst1"))
                            (string= (participant-identifier (agent-of "bsecnd2"))
                                     (participant-identifier first-agent))
                            (null (world-session-participant configuration first))
                            (string= (participant-identifier
                                      (world-session-participant configuration second))
                                     (participant-identifier first-agent)))
                       "the agent lives in the destination, whose session names it, and the channel it left has none")
          (conversation-append-user-message first "still here too")
          (world-reconcile-sessions configuration :now 4000)
          (test-assert (and (null (agent-of "bfirst1"))
                            (= (count ':agent (world-channel-residents (world-load configuration)
                                                                       "bsecnd2")
                                      :key #'participant-kind)
                               1))
                       "reconciliation invents no agent for either channel after the move")
          (world-move-agent configuration (agent-of "bsecnd2") "bfirst1")
          (test-assert (and (null (agent-of "bsecnd2"))
                            (string= (participant-identifier
                                      (world-session-participant configuration first))
                                     (participant-identifier first-agent)))
                       "moving back restores the first channel's agent and leaves the other without one")
          (platform-delete-directory-tree
           *platform*
           (conversation-storage-directory-pathname (conversation-pathname second))
           :validate t)
          (let ((world (values (world-reconcile-sessions configuration :now 5000))))
            (test-assert (and (null (world-channel-find world "bsecnd2"))
                              (world-participant-find world (participant-identifier first-agent)))
                         "retiring a channel without an agent deletes the channel and nobody"))))))
  nil)

(-> test-world-agentless-channel () null)
(defun test-world-agentless-channel ()
  "Test a channel whose agent was dismissed refuses typed prompts and holds deliveries, and a domestic agent takes it back."
  (with-test-configuration (configuration)
    (let ((first (world-tests--persisted-conversation configuration "bfirst1" 1000)))
      (world-tests--persisted-conversation configuration "bsecnd2" 2000)
      (multiple-value-bind (human first-agent second-agent) (world-tests--agents configuration)
        (declare (ignore second-agent))
        (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 80))
               (ui          (terminal-ui-create :terminal terminal))
               (application (make-instance 'application
                                           :configuration configuration
                                           :conversation  first
                                           :ui            ui))
               (controller  nil))
          (with-terminal-ui (active-ui ui)
            (setf controller (application-input-controller-create application
                                                                  :load-pending-p nil
                                                                  :start-reader-p nil))
            (unwind-protect
                 (flet ((submit (text)
                          "Submit TEXT as a primary prompt and return its delivery mode."
                          (nth-value 1 (application-input-controller-submit-primary-prompt
                                        controller text))))
                   (test-assert (eq (submit "first words") ':queued)
                                "a channel with its agent takes a typed prompt")
                   (let ((result (world-dismiss-agent configuration first-agent
                                                      :application application)))
                     (test-assert (and (= (getf result :dropped) 1)
                                       (search "was dismissed" (recording-terminal-output terminal)))
                                  "dismissing the agent drops the prompt it had not started and says so"))
                   (test-assert (and (eq (submit "more words") ':rejected)
                                     (search "this channel has no agent"
                                             (recording-terminal-output terminal)))
                                "a channel without an agent refuses a typed prompt and offers a move or a domestic agent")
                   (world-message-send configuration
                                       :sender   (participant-identifier human)
                                       :channel  "bfirst1"
                                       :text     "anyone here?"
                                       :notify-p nil)
                   (test-assert (and (zerop (application-world-deliver application))
                                     (= (length (world-inbox-deliveries configuration "bfirst1")) 1))
                                "a message to a channel without an agent waits in the inbox")
                   (let ((agent (application-world-domestic-agent application)))
                     (test-assert (and (eq (participant-kind agent) ':agent)
                                       (string= (participant-residence agent) "bfirst1")
                                       (equal (participant-parent agent)
                                              (participant-identifier human))
                                       (equal (getf (participant-settings agent) :model)
                                              (config :model configuration))
                                       (string= (participant-identifier
                                                 (world-session-participant configuration first))
                                                (participant-identifier agent)))
                                  "a domestic agent lives in the channel under the person with the session's model, and the session names it")
                     (test-assert (handler-case
                                      (progn (application-world-domestic-agent application) nil)
                                    (world-error (condition)
                                      (eq (world-error-operation condition) ':domestic)))
                                  "a second domestic agent is refused")
                     (test-assert (eq (submit "again") ':queued)
                                  "the channel takes typed prompts again")
                     (test-assert (= (application-world-deliver application) 1)
                                  "the waiting message reaches the new agent")))
              (application-input-controller-stop controller)))))))
  nil)
