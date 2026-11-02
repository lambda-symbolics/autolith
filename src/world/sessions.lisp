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

(defparameter *world-agent-name* "autolith"
  "The name of the agent participant a migrated session gets.")

(defvar *world-sessions-lock* (make-recursive-lock "Autolith world sessions")
  "Serializes in-process reconciliation.")

(defclass world-session-mapping ()
  ((conversation
    :initarg       :conversation
    :reader        world-session-mapping-conversation
    :type          non-empty-string
    :documentation "The conversation identifier, which is also the channel identifier.")
   (participant
    :initarg       :participant
    :reader        world-session-mapping-participant
    :type          non-empty-string
    :documentation "The agent participant the conversation was migrated onto.")
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
       (non-empty-string-p (getf (rest form) :participant))
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

(-> world-session-mappings (configuration) list)
(defun world-session-mappings (configuration)
  "Return every conversation mapping, ordered by conversation identifier."
  (with-recursive-lock-held (*world-sessions-lock*)
    (handler-case
        (sort (copy-list (sexp-store:store-read (world-session--store configuration)))
              #'string< :key #'world-session-mapping-conversation)
      (sexp-store:store-error (cause)
        (world--store-error cause)))))

(-> world-session--write-mappings (configuration list) null)
(defun world-session--write-mappings (configuration mappings)
  "Replace the stored mappings with MAPPINGS."
  (handler-case
      (sexp-store:store-transact
       (world-session--store configuration)
       (lambda (current)
         (declare (ignore current))
         (values (sort (copy-list mappings) #'string<
                       :key #'world-session-mapping-conversation)
                 nil t)))
    (sexp-store:store-error (cause)
      (world--store-error cause)))
  nil)


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
  "Return the agent participant MAPPING names, or the migrated resident of CHANNEL-IDENTIFIER.

A mapping lost after its world records were published still finds the agent
resident the migration created, so a session never gets a second agent."
  (or (and mapping (world-participant-find world (world-session-mapping-participant mapping)))
      (find-if (lambda (participant)
                 (and (eq (participant-kind participant) ':agent)
                      (string= (participant-name participant) *world-agent-name*)
                      (let ((parent (participant-parent participant)))
                        (and parent
                             (let ((owner (world-participant-find world parent)))
                               (and owner (eq (participant-kind owner) ':human)))))))
               (sort (world-channel-residents world channel-identifier) #'<
                     :key #'participant-created-at))))


;;;; -- Reconciliation --

(-> world-session--changes (list list &key (:all-p boolean)) (values list list))
(defun world-session--changes (identities mappings &key all-p)
  "Return the conversations to migrate and the mappings whose conversation vanished.

Each change is a plist of :id, :identity, :token, :header and :mapping. A
mapping has vanished when its conversation is among IDENTITIES without
durable storage, or, with ALL-P, when IDENTITIES is every conversation and
does not name it."
  (let ((changes   nil)
        (vanished  nil)
        (requested (make-hash-table :test #'equal)))
    (dolist (identity identities)
      (let* ((identifier (pathname-name identity))
             (mapping    (find identifier mappings
                               :key #'world-session-mapping-conversation :test #'string=))
             (token      (and (conversation-storage-active-pathname identity)
                              (handler-case (world-session--token identity)
                                (error () nil)))))
        (setf (gethash identifier requested) (or token ':absent))
        (when (and token
                   (not (and mapping (equal token (world-session-mapping-token mapping)))))
          (let ((header (conversation-peek-header identity)))
            (when header
              (push (list :id       identifier
                          :identity identity
                          :token    token
                          :header   header
                          :mapping  mapping)
                    changes))))))
    (dolist (mapping mappings)
      (let ((state (gethash (world-session-mapping-conversation mapping) requested)))
        (when (or (eq state ':absent) (and all-p (null state)))
          (push mapping vanished))))
    (values (nreverse changes) (nreverse vanished))))

(-> world-session--plan
    (configuration world &key (:changes list) (:vanished list) (:now timestamp))
    (values list list list))
(defun world-session--plan (configuration world &key changes vanished now)
  "Return the world records migrating CHANGES and retiring VANISHED, with the mappings each yields.

The records are folded into a private copy of WORLD as they are planned, so
every later record sees the identities the earlier ones introduce. The third
value lists the conversation identifiers whose mappings end."
  (let ((preview  (world--copy world))
        (records  nil)
        (mappings nil)
        (ended    nil))
    (labels ((emit (record)
               "Append RECORD and fold it into the preview."
               (push record records)
               (world--reduce-record (configuration-world-path configuration) preview record))

             (human ()
               "Return the human participant, or NIL before one exists."
               (world-human-participant preview))

             (created-at (change)
               "Return the creation time CHANGE's conversation header records."
               (getf (rest (getf change :header)) :created-at))

             (ensure-human (change)
               "Create the human participant at home in CHANGE's channel when none exists."
               (or (human)
                   (let* ((header     (getf change :header))
                          (identifier (world--generate-identifier preview now))
                          (channel    (make-instance 'channel
                                                     :identifier (getf change :id)
                                                     :owner      identifier
                                                     :workspace  (world-session--workspace
                                                                  configuration header)
                                                     :title      (world-session--title
                                                                  (getf change :identity) header)
                                                     :created-at (created-at change)
                                                     :updated-at (max now (created-at change))))
                          (person     (make-instance 'participant
                                                     :identifier identifier
                                                     :kind       ':human
                                                     :name       *world-human-name*
                                                     :residence  (getf change :id)
                                                     :presence   ':present
                                                     :created-at now
                                                     :updated-at now)))
                     (emit (world--channel-record channel))
                     (emit (world--participant-record person))
                     person)))

             (ensure-channel (change owner)
               "Create or refresh CHANGE's channel, owned by OWNER when created."
               (let* ((header    (getf change :header))
                      (existing  (world-channel-find preview (getf change :id)))
                      (workspace (world-session--workspace configuration header))
                      (title     (world-session--title (getf change :identity) header)))
                 (cond
                   ((null existing)
                    (emit (world--channel-record
                           (make-instance 'channel
                                          :identifier (getf change :id)
                                          :owner      (participant-identifier owner)
                                          :workspace  workspace
                                          :title      title
                                          :created-at (created-at change)
                                          :updated-at (max now (created-at change))))))
                   ((or (not (string= (channel-workspace existing) workspace))
                        (not (equal (channel-title existing) title)))
                    (emit (world--channel-record
                           (world--copy-channel existing
                                                :workspace  workspace
                                                :title      title
                                                :updated-at (max now (channel-updated-at
                                                                      existing)))))))))

             (ensure-agent (change owner)
               "Create or refresh CHANGE's agent participant under OWNER and return its identifier."
               (let* ((header   (getf change :header))
                      (settings (world-session--settings header))
                      (existing (world-session--mapped-participant
                                 preview (getf change :mapping) (getf change :id))))
                 (cond
                   ((null existing)
                    (let ((agent (make-instance 'participant
                                                :identifier (world--generate-identifier preview now)
                                                :kind       ':agent
                                                :name       *world-agent-name*
                                                :parent     (participant-identifier owner)
                                                :residence  (getf change :id)
                                                :settings   settings
                                                :created-at (created-at change)
                                                :updated-at (max now (created-at change)))))
                      (emit (world--participant-record agent))
                      (participant-identifier agent)))
                   ((not (equal (participant-settings existing) settings))
                    (emit (world--participant-record
                           (world--copy-participant existing
                                                    :settings   settings
                                                    :updated-at (max now (participant-updated-at
                                                                          existing)))))
                    (participant-identifier existing))
                   (t
                    (participant-identifier existing)))))

             (retire (mapping)
               "Delete MAPPING's participant, and its channel once nobody else lives there."
               (let* ((channel-id  (world-session-mapping-conversation mapping))
                      (participant (world-participant-find
                                    preview (world-session-mapping-participant mapping)))
                      (channel     (world-channel-find preview channel-id)))
                 (when participant
                   (dolist (record (world--participant-deletion-records
                                    preview participant now :except channel-id))
                     (emit record)))
                 (when channel
                   (let ((person (human)))
                     (when (and person (string= (participant-residence person) channel-id))
                       (let ((elsewhere (find-if-not (lambda (candidate)
                                                       (string= (channel-identifier candidate)
                                                                channel-id))
                                                     (world-channels preview))))
                         (when elsewhere
                           (emit (world--participant-record
                                  (world--copy-participant
                                   person
                                   :residence  (channel-identifier elsewhere)
                                   :updated-at (max now (participant-updated-at person)))))))))
                   (when (and (null (world-channel-residents preview channel-id))
                              (null (world-channel-children preview channel-id)))
                     (emit (list :channel-deleted
                                 :version *world-format-version*
                                 :id   channel-id
                                 :time now))))
                 (push channel-id ended))))
      (dolist (change changes)
        (let ((owner (ensure-human change)))
          (ensure-channel change owner)
          (push (make-instance 'world-session-mapping
                               :conversation (getf change :id)
                               :participant  (ensure-agent change owner)
                               :token        (getf change :token))
                mappings)))
      (dolist (mapping vanished)
        (retire mapping)))
    (values (nreverse records) (nreverse mappings) (nreverse ended))))

(-> world-reconcile-sessions
    (configuration &key (:identities list) (:prune-p boolean) (:now timestamp))
    (values world (integer 0)))
(defun world-reconcile-sessions (configuration &key (identities nil identities-p) (prune-p t)
                                                    (now (get-universal-time)))
  "Map every conversation onto its channel and participant, and return the world and the change count.

IDENTITIES limits the work to those conversation identity pathnames; otherwise
every durable conversation is examined. A conversation whose source token
matches its mapping costs one stat. With PRUNE-P, mappings whose conversation
no longer exists retire their participant and, once empty, their channel."
  (with-recursive-lock-held (*world-sessions-lock*)
    (let ((mappings   (world-session-mappings configuration))
          (identities (if identities-p identities (conversation-list configuration))))
      (multiple-value-bind (changes vanished)
          (world-session--changes identities mappings :all-p (not identities-p))
        (unless prune-p
          (setf vanished nil))
        (if (and (null changes) (null vanished))
            (values (world-load configuration) 0)
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
                                                             :now      now)
                                      (setf updated planned ended retired)
                                      (values records nil)))))))
                (world-session--write-mappings
                 configuration
                 (append updated
                         (remove-if (lambda (mapping)
                                      (let ((identifier (world-session-mapping-conversation mapping)))
                                        (or (member identifier ended :test #'string=)
                                            (member identifier updated
                                                    :key  #'world-session-mapping-conversation
                                                    :test #'string=))))
                                    mappings)))
                (values committed (+ (length changes) (length vanished))))))))))

(-> world-session-select (configuration string &key (:now timestamp)) (option participant))
(defun world-session-select (configuration conversation-identifier &key (now (get-universal-time)))
  "Move the human participant into CONVERSATION-IDENTIFIER's channel and return it.

Returns NIL when the channel or the human participant does not exist yet."
  (let* ((world  (world-load configuration))
         (person (world-human-participant world)))
    (and person
         (world-channel-find world conversation-identifier)
         (world-participant-move configuration (participant-identifier person)
                                 conversation-identifier :now now))))


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
    :documentation "The failure that interrupted reconciliation."))
  (:report (lambda (condition stream)
             (format stream "Session reconciliation at ~A was skipped: ~A"
                     (world-session-warning-pathname condition)
                     (world-session-warning-cause condition))))
  (:documentation "Legacy sessions could not be reconciled with the world this time."))
