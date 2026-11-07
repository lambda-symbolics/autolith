(in-package #:autolith)

;;;; -- Legacy Session Reconciliation --

;;; Until clients select channels directly, every durable conversation maps
;;; onto one channel of the same identifier and one agent participant that
;;; resides there, both owned by the one human participant. The mapping keeps
;;; the source token each conversation was synced from, so a conversation
;;; newer than its mapping is migrated again onto the same identities, and a
;;; conversation that vanished takes its participant and channel with it.
;;;
;;; This is release machinery. Remove this file,
;;; configuration-world-sessions-path and the application-world-sync hooks
;;; once the channel model replaces conversation-centric startup.

(defparameter *world-sessions-version* 1
  "The readable session mapping format version.")

(defparameter *world-human-name* "user"
  "The name of the one human participant a world starts with.")

(defparameter *world-human-identifier* "user"
  "The identifier of the one human participant a world starts with.

It is fixed, so every migration gives the person the same identifier.")

(defparameter *world-agent-name* "autolith"
  "The handle base of a migrated session's agent when its workspace name yields none.

An agent is named after its workspace's directory, so @frob reaches the
agent working in frob; a second agent there is frob-2.")

(defparameter *world-task-name* "agent"
  "The handle base of a child agent whose conversation records no name of its own.")

(defvar *world-sessions-lock* (make-recursive-lock "Autolith world sessions")
  "Serializes in-process reconciliation.")

(defvar *world-session-cache* (make-hash-table :test #'equal)
  "This process's last read of each mapping file, keyed by its namestring, as (STAMP . MAPPINGS).

The file is read again only once its stamp changed.")

(defvar *world-session-tokens* (make-hash-table :test #'equal)
  "The newest source token this process synchronized for each conversation, keyed by mapping file.

A token changes with every message, while the mapping file is written only
when a mapping does; a token older on disk costs the next process one header
read.")

(defclass world-session-mapping ()
  ((conversation
    :initarg       :conversation
    :reader        world-session-mapping-conversation
    :type          non-empty-string
    :documentation "The conversation identifier, which is also the channel identifier.")
   (participant
    :initarg       :participant
    :reader        world-session-mapping-participant
    :type          (option non-empty-string)
    :documentation "The agent participant resident in the conversation's channel, or NIL for a channel deliberately without one: its agent moved away, and none is invented in its place.")
   (token
    :initarg       :token
    :reader        world-session-mapping-token
    :type          list
    :documentation "The conversation source token at the last synchronization."))
  (:documentation "One legacy conversation's place in the world."))


;;;; -- Mapping Store --

(-> world-session--token (pathname) list)
(defun world-session--token (identity)
  "Return conversation IDENTITY's current portable source token."
  (let ((token (conversation-picker-metadata-source-token identity)))
    (cons (let ((segment (first token)))
            (if (pathnamep segment) (namestring segment) segment))
          (rest token))))

(-> world-session--token-p (t) boolean)
(defun world-session--token-p (value)
  "Return true when VALUE is a portable source token."
  (and (listp value)
       (handler-case (list-length value) (error () nil))
       (every (lambda (item) (or (stringp item) (integerp item) (null item))) value)
       t))

(-> world-session--mapping-form-p (t) boolean)
(defun world-session--mapping-form-p (form)
  "Return true when FORM is one valid session mapping record."
  (and (listp form)
       (eq (first form) ':session)
       (sexp-store:plist-schema-p (rest form)
                                 :required-keys  '(:conversation :participant :token)
                                 :allowed-keys   '(:conversation :participant :token)
                                 :keyword-keys-p t)
       (non-empty-string-p (getf (rest form) :conversation))
       (let ((participant (getf (rest form) :participant)))
         (or (null participant) (non-empty-string-p participant)))
       (world-session--token-p (getf (rest form) :token))))

(-> world-session--form-p (t) boolean)
(defun world-session--form-p (form)
  "Return true when FORM is a complete session mapping snapshot."
  (and (listp form)
       (eq (first form) ':world-sessions)
       (sexp-store:plist-schema-p (rest form)
                                 :required-keys  '(:version :records)
                                 :allowed-keys   '(:version :records)
                                 :keyword-keys-p t)
       (eql (getf (rest form) :version) *world-sessions-version*)
       (let ((records (getf (rest form) :records)))
         (and (listp records)
              (handler-case (list-length records) (error () nil))
              (every #'world-session--mapping-form-p records)
              t))))

(-> world-session--mapping->form (world-session-mapping) list)
(defun world-session--mapping->form (mapping)
  "Return MAPPING as one portable readable record."
  (list :session
        :conversation (world-session-mapping-conversation mapping)
        :participant (world-session-mapping-participant mapping)
        :token       (copy-list (world-session-mapping-token mapping))))

(-> world-session--form->mapping (list) world-session-mapping)
(defun world-session--form->mapping (form)
  "Return the mapping one validated record FORM describes."
  (make-instance 'world-session-mapping
                 :conversation (getf (rest form) :conversation)
                 :participant  (getf (rest form) :participant)
                 :token        (copy-list (getf (rest form) :token))))

(-> world-session--store (configuration) sexp-store:snapshot-store)
(defun world-session--store (configuration)
  "Describe the session mapping snapshot and its product-owned paths."
  (let ((pathname (configuration-world-sessions-path configuration)))
    (ensure-directories-exist pathname)
    (make-instance
     'sexp-store:snapshot-store
     :pathname      pathname
     :lock-pathname (readable-state-lock-pathname pathname "sessions.lock")
     :initial-state (lambda () nil)
     :validator     #'world-session--form-p
     :decoder       (lambda (form)
                      (mapcar #'world-session--form->mapping (getf (rest form) :records)))
     :encoder       (lambda (mappings)
                      (list :world-sessions
                            :version *world-sessions-version*
                            :records (mapcar #'world-session--mapping->form mappings)))
     :recover-read-error
     (lambda (cause)
       (warn 'world-session-warning :pathname pathname :cause cause)
       nil))))

(-> world-session--stamp (pathname) list)
(defun world-session--stamp (pathname)
  "Return what changes whenever PATHNAME is written or replaced, or NIL while it does not exist."
  (let ((status (platform-path-status *platform* pathname)))
    (and status
         (list (platform-file-status-identity status)
               (platform-file-status-size status)
               (platform-file-status-modification-time status)))))

(-> world-session-mappings (configuration) list)
(defun world-session-mappings (configuration)
  "Return every conversation mapping, ordered by conversation identifier.

The file is read again only once it changed since this process last read it."
  (with-recursive-lock-held (*world-sessions-lock*)
    (let* ((pathname (configuration-world-sessions-path configuration))
           (key      (namestring pathname))
           (stamp    (world-session--stamp pathname))
           (cached   (gethash key *world-session-cache*)))
      (if (and cached stamp (equal (first cached) stamp))
          (rest cached)
          (handler-case
              (let ((mappings (sort (copy-list (sexp-store:store-read
                                                (world-session--store configuration)))
                                    #'string< :key #'world-session-mapping-conversation)))
                (setf (gethash key *world-session-cache*) (cons stamp mappings))
                mappings)
            (sexp-store:store-error (cause)
              (world--store-error cause)))))))

(-> world-session--tokens (configuration) hash-table)
(defun world-session--tokens (configuration)
  "Return the tokens this process synchronized for CONFIGURATION's conversations, by conversation."
  (let ((key (namestring (configuration-world-sessions-path configuration))))
    (or (gethash key *world-session-tokens*)
        (setf (gethash key *world-session-tokens*) (make-hash-table :test #'equal)))))

(-> world-session--current-mappings (configuration) list)
(defun world-session--current-mappings (configuration)
  "Return the mappings with the newest token this process synchronized for each."
  (let ((tokens (world-session--tokens configuration)))
    (mapcar (lambda (mapping)
              (let ((token (gethash (world-session-mapping-conversation mapping) tokens)))
                (if token
                    (make-instance 'world-session-mapping
                                   :conversation (world-session-mapping-conversation mapping)
                                   :participant  (world-session-mapping-participant mapping)
                                   :token        token)
                    mapping)))
            (world-session-mappings configuration))))

(-> world-session--placement (list) list)
(defun world-session--placement (mappings)
  "Return MAPPINGS as (CONVERSATION . PARTICIPANT) pairs, the part of a mapping worth writing down."
  (mapcar (lambda (mapping)
            (cons (world-session-mapping-conversation mapping)
                  (world-session-mapping-participant mapping)))
          mappings))

(-> world-session--transact (configuration function) t)
(defun world-session--transact (configuration update)
  "Call UPDATE with fresh mappings under the store lock and return its result.

UPDATE returns replacement mappings, its result, and whether to publish."
  (with-recursive-lock-held (*world-sessions-lock*)
    (handler-case
        (nth-value 0
                   (sexp-store:store-transact
                    (world-session--store configuration)
                    (lambda (current)
                      (multiple-value-bind (mappings result write-p) (funcall update current)
                        (values (when write-p
                                  (sort (copy-list mappings) #'string<
                                        :key #'world-session-mapping-conversation))
                                result write-p)))))
      (sexp-store:store-error (cause)
        (world--store-error cause)))))


;;;; -- Conversation Facts --

(-> world-session--settings (list) list)
(defun world-session--settings (header)
  "Return the participant settings conversation HEADER carries."
  (loop for key in '(:model :model-provider :reasoning-effort)
        for value = (getf (rest header) key)
        when (and value (world--portable-value-p value))
          append (list key value)))

(-> world-session--title (pathname list) (option string))
(defun world-session--title (identity header)
  "Return conversation IDENTITY's current title, or the one its HEADER recorded.

The picker sidecar is consulted only while it is current, so a stale cache
costs nothing here; the header's title is older but always available."
  (let* ((metadata (handler-case (conversation-picker-metadata-read identity)
                     (error () nil)))
         (title    (or (and metadata (conversation-picker-metadata-title metadata))
                       (getf (rest header) :title))))
    (and (world--name-p title) title)))

(-> world-session--workspace (configuration list) string)
(defun world-session--workspace (configuration header)
  "Return the canonical workspace conversation HEADER began in."
  (world-workspace-name
   (or (let ((directory (getf (rest header) :directory)))
         (and (non-empty-string-p directory) directory))
       (config :working-directory configuration))))

(-> world-session--mapped-participant (world (option world-session-mapping) string) (option participant))
(defun world-session--mapped-participant (world mapping channel-identifier)
  "Return the agent participant MAPPING names while it resides in CHANNEL-IDENTIFIER, or the migrated resident.

A mapping names no participant for a channel deliberately left without an
agent; it stays without one. A mapping whose agent moved elsewhere leaves
the channel without one too. A mapping lost after its world records were
published still finds the agent resident the migration created, so a
session never gets a second agent."
  (cond
    ((and mapping (null (world-session-mapping-participant mapping)))
     nil)
    (mapping
     (let ((participant (world-participant-find world (world-session-mapping-participant mapping))))
       (and participant
            (equal (participant-residence participant) channel-identifier)
            participant)))
    (t
     (find-if (lambda (participant)
                (and (eq (participant-kind participant) ':agent)
                     (participant-parent participant)))
              (sort (world-channel-residents world channel-identifier) #'<
                    :key #'participant-created-at)))))

(-> world-session-participant (configuration conversation) (option participant))
(defun world-session-participant (configuration conversation)
  "Return the agent participant CONVERSATION was migrated onto, or NIL before it has one.

A persisted conversation is reconciled first, so a fresh session that has
its first message finds its participant on the first call."
  (when (conversation-persisted-p conversation)
    (world-reconcile-sessions configuration
                              :identities (list (conversation-pathname conversation))))
  (let ((mapping (find (conversation-identifier conversation)
                       (world-session-mappings configuration)
                       :key #'world-session-mapping-conversation :test #'string=)))
    (and mapping
         (world-session-mapping-participant mapping)
         (world-participant-find (world-load configuration)
                                 (world-session-mapping-participant mapping)))))

(-> world-session-remap (configuration string (option string)) boolean)
(defun world-session-remap (configuration conversation-identifier participant-identifier)
  "Record PARTICIPANT-IDENTIFIER as the agent of CONVERSATION-IDENTIFIER's channel, or NIL for none, and return whether a mapping was rewritten.

A move writes the destination's mapping with the moved agent and the
source's with NIL, so neither channel gets a second agent invented at its
next reconciliation. A channel without a conversation has no mapping to
rewrite and nothing to reconcile, so the move leaves it to the world store."
  (world-session--transact
   configuration
   (lambda (mappings)
     (let ((mapping (find conversation-identifier mappings
                          :key #'world-session-mapping-conversation :test #'string=)))
       (if mapping
           (values (cons (make-instance 'world-session-mapping
                                        :conversation conversation-identifier
                                        :participant  participant-identifier
                                        :token        (world-session-mapping-token mapping))
                         (remove mapping mappings))
                   t t)
           (values nil nil nil))))))


;;;; -- Reconciliation --

(-> world-session--changes (list list &key (:all-p boolean) (:orphan-p function)) (values list list))
(defun world-session--changes (identities mappings &key all-p (orphan-p (constantly nil)))
  "Return the conversations to migrate and the mappings whose conversation vanished.

IDENTITIES holds identity pathnames, or (IDENTITY ROOT-IDENTIFIER) pairs for
child agent conversations. Each change is a plist of :id, :identity, :root,
:token, :header and :mapping. An unchanged child is migrated again when
ORPHAN-P, given its identifier, its root and whether the root migrates in
this pass, says its channel still lacks the ancestry it was migrated without
and its root is known by now. A
mapping has vanished when its conversation is among IDENTITIES without durable
storage, or, with ALL-P, when IDENTITIES is every conversation and does not
name it."
  (let ((changes   nil)
        (vanished  nil)
        (requested (make-hash-table :test #'equal))
        (mapped    (make-hash-table :test #'equal)))
    (dolist (mapping mappings)
      (setf (gethash (world-session-mapping-conversation mapping) mapped) mapping))
    (dolist (entry identities)
      (let* ((identity   (if (consp entry) (first entry) entry))
             (root       (and (consp entry) (second entry)))
             (identifier (pathname-name identity))
             (mapping    (gethash identifier mapped))
             (token      (and (conversation-storage-active-pathname identity)
                              (handler-case (world-session--token identity)
                                (error () nil)))))
        (setf (gethash identifier requested) (or token ':absent))
        (when (and token
                   (or (not (and mapping (equal token (world-session-mapping-token mapping))))
                       (and root (funcall orphan-p identifier root (gethash root requested)))))
          (let ((header (conversation-peek-header identity)))
            (when header
              (push (list :id       identifier
                          :identity identity
                          :root     root
                          :token    token
                          :header   header
                          :mapping  mapping)
                    changes))))))
    (dolist (mapping mappings)
      (let ((state (gethash (world-session-mapping-conversation mapping) requested)))
        (when (or (eq state ':absent) (and all-p (null state)))
          (push mapping vanished))))
    (values (nreverse changes) (nreverse vanished))))

(-> world-session--agent-identifier (string) string)
(defun world-session--agent-identifier (conversation-identifier)
  "Return the identifier of the agent a migration makes for CONVERSATION-IDENTIFIER.

It comes from the conversation, so migrating twice gives the same agent, and
there is no limit on how many conversations migrate per second."
  (concatenate 'string "al-" conversation-identifier))

(-> world-session--plan
    (configuration world &key (:changes list) (:vanished list) (:mappings list)
                              (:now timestamp))
    (values list list list))
(defun world-session--plan (configuration world &key changes vanished mappings now)
  "Return the world records migrating CHANGES and retiring VANISHED, with the mappings each yields.

MAPPINGS are the existing mappings, consulted for the root of a child task.
The records are folded into a private copy of WORLD as they are planned, so
every later record sees the identities the earlier ones introduce. The third
value lists the conversation identifiers whose mappings end."
  (let ((preview (world--copy world))
        (records nil)
        (planned nil)
        (ended   nil)
        (ending  (mapcar #'world-session-mapping-conversation vanished)))
    (labels ((emit (record)
               "Append RECORD and fold it into the preview."
               (push record records)
               (world--reduce-record (configuration-world-path configuration) preview record))

             (human ()
               "Return the human participant, or NIL before one exists."
               (world-human-participant preview))

             (parent-of (change)
               "Return the conversation of the agent that created CHANGE's, or NIL for a top-level one.

A child agent's conversation names its parent; one written before it did,
or whose parent is gone, falls back to its root."
               (let ((parent (getf (rest (getf change :header)) :parent)))
                 (if (and (non-empty-string-p parent) (world-channel-find preview parent))
                     parent
                     (getf change :root))))

             (retire (mapping)
               "Delete MAPPING's participant with its descendants; its channel ends in the sweep.

A parent whose child channel still has a live conversation keeps its
mapping and its participant: the child belongs to it, and the parent is
retired on the pass that retires the child."
               (let* ((channel-id  (world-session-mapping-conversation mapping))
                      ;; A channel without an agent, or whose agent lives
                      ;; elsewhere now, retires nobody.
                      (participant (world-session--mapped-participant
                                    preview mapping channel-id))
                      (blocked-p   (some (lambda (child)
                                           (not (member (channel-identifier child) ending
                                                        :test #'string=)))
                                         (world-channel-children preview channel-id))))
                 (unless blocked-p
                   (when participant
                     (dolist (record (world--participant-deletion-records
                                      preview participant now :except channel-id))
                       (emit record)))
                   (push channel-id ended))))

             (relocate-human (channel-id)
               "Move the human out of CHANNEL-ID into any other channel, when it lives there."
               (let ((person (human)))
                 (when (and person (equal (participant-residence person) channel-id))
                   (let ((elsewhere (find-if-not (lambda (candidate)
                                                   (string= (channel-identifier candidate)
                                                            channel-id))
                                                 (world-channels preview))))
                     (when elsewhere
                       (emit (world--participant-record
                              (world--copy-participant
                               person
                               :residence  (channel-identifier elsewhere)
                               :updated-at (max now (participant-updated-at person))))))))))

             (sweep ()
               "Delete every ended channel nobody lives in and nothing nests under.

A root whose child channel ends in the same pass is empty only once the
child is gone, so the sweep repeats until no further channel can go, and a
parent is never left behind its retired children."
               (loop
                 (let ((gone-p nil))
                   (dolist (channel-id ended)
                     (when (and (world-channel-find preview channel-id)
                                (null (world-channel-children preview channel-id)))
                       (relocate-human channel-id)
                       (when (null (world-channel-residents preview channel-id))
                         (emit (list :channel-deleted
                                     :version *world-format-version*
                                     :id   channel-id
                                     :time now))
                         (setf gone-p t))))
                   (unless gone-p
                     (return))))))
      ;; The inbox first, so the person is created there, then top-level
      ;; conversations, then child agents in the order they were created,
      ;; so a parent is always planned before its children.
      (dolist (change (stable-sort (copy-list changes) #'<
                                   :key (lambda (change)
                                          (cond
                                            ((world-session--inbox-header-p (getf change :header))
                                             -2)
                                            ((parent-of change)
                                             (getf (rest (getf change :header)) :created-at))
                                            (t
                                             -1)))))
        (let* ((identifier (getf change :id))
               (header     (getf change :header))
               (created-at (getf (rest header) :created-at))
               (inbox-p    (world-session--inbox-header-p header))
               (workspace  (world-session--workspace configuration header))
               (title      (if inbox-p
                               *world-inbox-designator*
                               (world-session--title (getf change :identity) header)))
               (parent-id  (parent-of change))
               (parent     (and parent-id (world-channel-find preview parent-id)))
               (ancestor   (and parent
                                (world-session--mapped-participant
                                 preview
                                 (find parent-id (append planned mappings)
                                       :key #'world-session-mapping-conversation :test #'string=)
                                 parent-id)))
               (owner      (or (human)
                               (make-instance 'participant
                                              :identifier *world-human-identifier*
                                              :kind       ':human
                                              :name       *world-human-name*
                                              :residence  identifier
                                              :presence   ':present
                                              :created-at now
                                              :updated-at now)))
               (channel    (world-channel-find preview identifier))
               (adopted-p  (and channel parent (null (channel-parent channel))))
               (keeper     (participant-identifier (or ancestor owner)))
               (mapping    (getf change :mapping))
               (existing   (world-session--mapped-participant preview mapping identifier)))
          (cond
            ((null channel)
             (emit (world--channel-record
                    (make-instance 'channel
                                   :identifier identifier
                                   :parent     (and parent parent-id)
                                   :owner      keeper
                                   :workspace  workspace
                                   :title      title
                                   :inbox-p    inbox-p
                                   :created-at created-at
                                   :updated-at (max now created-at)))))
            ((or (not (string= (channel-workspace channel) workspace))
                 (not (equal (channel-title channel) title))
                 adopted-p)
             (emit (world--channel-record
                    (world--copy-channel channel
                                         :workspace  workspace
                                         :title      title
                                         :parent     (if adopted-p parent-id (channel-parent channel))
                                         :owner      (if adopted-p keeper (channel-owner channel))
                                         :updated-at (max now (channel-updated-at channel)))))))
          (cond
            ((null (human))
             (emit (world--participant-record owner)))
            ((and inbox-p (not (equal (participant-residence owner) identifier)))
             (emit (world--participant-record
                    (world--copy-participant owner
                                             :residence  identifier
                                             :updated-at (max now (participant-updated-at owner)))))))
          ;; A mapping deliberately without an agent, or whose agent moved
          ;; away, stays without one. Only a deleted agent is recreated.
          (unless (or inbox-p
                      (and mapping (null existing)
                           (or (null (world-session-mapping-participant mapping))
                               (world-participant-find preview (world-session-mapping-participant mapping)))))
            (cond
              ((null existing)
               (let ((agent-id (world-session--agent-identifier identifier)))
                 ;; If an earlier migration made this agent but its mapping
                 ;; got lost, reuse the agent.
                 (setf existing (and (null mapping) (world-participant-find preview agent-id)))
                 (unless existing
                   (setf existing
                         (make-instance 'participant
                                        :identifier agent-id
                                        :kind       ':agent
                                        :name       (world-unique-handle
                                                     preview
                                                     (if parent-id
                                                         (world-handle-base (or (getf (rest header) :agent-name) "")
                                                                            :fallback *world-task-name*)
                                                         (world-session--handle-base workspace)))
                                        :parent     keeper
                                        :residence  identifier
                                        :settings   (world-session--settings header)
                                        :created-at created-at
                                        :updated-at (max now created-at)))
                   (emit (world--participant-record existing)))))
              ;; Existing agents keep their settings. Adopt a late parent's
              ;; ancestry only when adopting its channel in the same pass.
              ((and adopted-p ancestor
                    (not (equal (participant-parent existing) keeper)))
               (emit (world--participant-record
                      (world--copy-participant existing
                                               :parent     keeper
                                               :updated-at (max now (participant-updated-at existing))))))))
          (push (make-instance 'world-session-mapping
                               :conversation identifier
                               :participant  (and (not inbox-p) existing
                                                  (participant-identifier existing))
                               :token        (getf change :token))
                planned)))
      (dolist (mapping vanished)
        (retire mapping))
      (sweep)
      (dolist (record (world-session--handle-records preview now))
        (emit record)))
    (values (nreverse records) (nreverse planned) (nreverse ended))))

(-> world-session--handle-base (string) string)
(defun world-session--handle-base (workspace)
  "Return the handle base of an agent working in WORKSPACE, its directory's name."
  (world-handle-base (world-workspace-leaf workspace) :fallback *world-agent-name*))

(-> world-session--handle-records (world timestamp) list)
(defun world-session--handle-records (world now)
  "Return the records renaming WORLD's agents whose names another participant also has.

The oldest participant keeps a shared name; each later agent is renamed after
its channel's workspace, so every participant can be mentioned by name."
  (let ((seen    nil)
        (records nil))
    (dolist (participant (sort (copy-list (world-participants world)) #'<
                               :key #'participant-created-at))
      (let ((name (participant-name participant)))
        (if (or (member name seen :test #'string-equal)
                (and (eq (participant-kind participant) ':agent)
                     (or (not (world-handle-p name))
                         (world-handle-reserved-p name))))
            (let* ((residence (participant-residence participant))
                   (channel   (and residence (world-channel-find world residence)))
                   (base      (if channel
                                  (world-session--handle-base (channel-workspace channel))
                                  *world-agent-name*))
                   (handle    (loop for index from 1
                                    for candidate = (world-handle-suffixed base index)
                                    unless (or (member candidate seen :test #'string-equal)
                                               (world-participant-named-p world candidate))
                                      return candidate)))
              (push handle seen)
              (push (world--participant-record
                     (world--copy-participant participant
                                              :name       handle
                                              :updated-at (max now (participant-updated-at
                                                                    participant))))
                    records))
            (push name seen))))
    (nreverse records)))

(-> world-session--duplicate-names-p (world) boolean)
(defun world-session--duplicate-names-p (world)
  "Return true when two participants of WORLD share a name, or an agent's name is no handle or a reserved one."
  (let ((names nil))
    (dolist (participant (world-participants world) nil)
      (let ((name (participant-name participant)))
        (when (or (member name names :test #'string-equal)
                  (and (eq (participant-kind participant) ':agent)
                       (or (not (world-handle-p name))
                           (world-handle-reserved-p name))))
          (return t))
        (push name names)))))

(-> world-reconcile-sessions
    (configuration &key (:identities list) (:prune-p boolean) (:now timestamp))
    (values world (integer 0)))
(defun world-reconcile-sessions (configuration &key (identities nil identities-p) (prune-p t)
                                                    (now (get-universal-time)))
  "Reconcile conversation mappings and return the world and change count.

IDENTITIES limits the work to those conversation identity pathnames; otherwise
every durable conversation is examined, primaries before child tasks. A
matching source token usually avoids rereading the conversation header. With
PRUNE-P, mappings whose conversation no longer exists retire their
participant and, once empty, their channel. A world that has anything in it
gets the person's inbox: the first reconciliation without one examines every
conversation and creates an inbox conversation when none is found."
  (with-recursive-lock-held (*world-sessions-lock*)
    (let* ((discovery  (world-load configuration))
           (mappings   (world-session--current-mappings configuration))
           ;; Discover against one snapshot; WORLD--TRANSACT reloads under
           ;; the store lock before planning any records.
           (all-p      (or (not identities-p)
                           (null (world-inbox-channel discovery))))
           (identities (if all-p
                           (append (conversation-list configuration)
                                   (conversation-task-identities configuration))
                           identities)))
      (multiple-value-bind (changes vanished)
          (flet ((changes (identities)
                   "Return the changes and vanished mappings among IDENTITIES."
                   (world-session--changes identities mappings
                                           :all-p    all-p
                                           :orphan-p (lambda (identifier root root-requested-p)
                                                       (let ((channel (world-channel-find discovery identifier)))
                                                         (and channel
                                                              (null (channel-parent channel))
                                                              (or root-requested-p
                                                                  (world-channel-find discovery root))
                                                              t))))))
            (multiple-value-bind (changes vanished) (changes identities)
              (if (and all-p
                       (or changes (world-human-participant discovery))
                       (null (world-inbox-channel discovery))
                       (notany (lambda (change)
                                 (world-session--inbox-header-p (getf change :header)))
                               changes))
                  (changes (append identities (list (world-session--create-inbox configuration))))
                  (values changes vanished))))
        (unless prune-p
          (setf vanished nil))
        (if (and (null changes)
                 (null vanished)
                 (not (world-session--duplicate-names-p discovery)))
            (values discovery 0)
            (let ((updated nil) (ended nil))
              (let ((committed
                      (nth-value 1
                                 (world--transact
                                  configuration
                                  (lambda (world)
                                    (multiple-value-bind (records planned retired)
                                        (world-session--plan configuration world
                                                             :changes  changes
                                                             :vanished vanished
                                                             :mappings mappings
                                                             :now      now)
                                      (setf updated planned ended retired)
                                      (values records nil)))))))
                (let ((tokens (world-session--tokens configuration)))
                  (dolist (mapping updated)
                    (setf (gethash (world-session-mapping-conversation mapping) tokens)
                          (world-session-mapping-token mapping)))
                  (dolist (identifier ended)
                    (remhash identifier tokens)))
                ;; Tokens change with every message; the file is written only
                ;; when a conversation's place in the world changed.
                (world-session--transact
                 configuration
                 (lambda (current)
                   (let ((next (append updated
                                       (remove-if (lambda (mapping)
                                                    (let ((identifier (world-session-mapping-conversation mapping)))
                                                      (or (member identifier ended :test #'string=)
                                                          (member identifier updated
                                                                  :key  #'world-session-mapping-conversation
                                                                  :test #'string=))))
                                                  current))))
                     (values next nil
                             (not (equal (sort (world-session--placement next) #'string< :key #'first)
                                         (sort (world-session--placement current) #'string< :key #'first)))))))
                (values committed (+ (length changes) (length vanished))))))))))

(-> world-session--inbox-header-p (list) boolean)
(defun world-session--inbox-header-p (header)
  "Return true when conversation HEADER marks the person's inbox."
  (eq (getf (rest header) :inbox-p) t))

(-> world-session--create-inbox (configuration) pathname)
(defun world-session--create-inbox (configuration)
  "Create the durable conversation of the person's inbox and return its identity pathname.

The inbox begins in the home directory, since it belongs to no workspace,
and its first record names it."
  (let ((conversation (conversation-create configuration
                                           :inbox-p          t
                                           :origin-directory (namestring
                                                              (user-homedir-pathname)))))
    (conversation-append-record conversation
                                (list :title
                                      :value  *world-inbox-designator*
                                      :source ':initial))
    (conversation-pathname conversation)))


;;;; -- Conditions --

(define-condition world-session-warning (warning)
  ((pathname
    :initarg       :pathname
    :reader        world-session-warning-pathname
    :type          pathname
    :documentation "The session mapping pathname involved.")
   (cause
    :initarg       :cause
    :reader        world-session-warning-cause
    :type          t
    :documentation "The failure encountered while updating world session state."))
  (:report (lambda (condition stream)
             (format stream "World session operation at ~A failed: ~A"
                     (world-session-warning-pathname condition)
                     (world-session-warning-cause condition))))
  (:documentation "An error occurred while updating world session state."))
