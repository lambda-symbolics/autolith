(in-package #:autolith)

;;;; -- Active Application --

(defclass application ()
  ((configuration
    :initarg :configuration
    :accessor application-configuration
    :type configuration
    :documentation "The current paths, model, and provider choices.")
   (conversation
    :initarg :conversation
    :accessor application-conversation
    :type conversation
    :documentation "The durable conversation currently shown to the user.")
   (conversation-lease
    :initarg :conversation-lease
    :initform nil
    :accessor application-conversation-lease
    :type (option conversation-lease)
    :documentation "The process-lifetime exclusive lease on the primary conversation.")
   (provider
    :initarg :provider
    :accessor application-provider
    :type (option model-provider)
    :documentation "The reconnectable model provider.")
   (tool-registry
    :initarg :tool-registry
    :accessor application-tool-registry
    :type tool-registry
    :documentation "The live, checkpointed tool registry.")
   (worker
    :initarg :worker
    :accessor application-worker
    :type t
    :documentation "The reconnectable pool of named Lisp REPL workers.")
   (agent
    :initarg :agent
    :accessor application-agent
    :type (option agent)
    :documentation "The reconnectable provider and tool coordinator.")
   (ui
    :initarg :ui
    :accessor application-ui
    :type (option terminal-ui)
    :documentation "The reconnectable primary-screen terminal UI.")
   (lisp-hinted-command-names
    :initform nil
    :accessor application-lisp-hinted-command-names
    :type list
    :documentation "Canonical slash commands already given a Lisp spelling hint.")
   (goal
    :initform nil
    :accessor application-goal
    :type list
    :documentation "The session goal plist holding objective, status, and continuations.")
   (reasoning-traces-p
    :initarg :reasoning-traces-p
    :initform nil
    :accessor application-reasoning-traces-p
    :type boolean
    :documentation "Whether provider-visible reasoning summaries appear in the transcript.")
   (compact-view-p
    :initarg :compact-view-p
    :initform t
    :accessor application-compact-view-p
    :type boolean
    :documentation
    "Whether verbose tool calls are condensed and successful routine results hidden.")
   (turn-timestamps-p
    :initarg :turn-timestamps-p
    :initform nil
    :accessor application-turn-timestamps-p
    :type boolean
    :documentation "Whether user and assistant headers show local timestamps.")
   (hurry-up-p
    :initarg :hurry-up-p
    :initform nil
    :accessor application-hurry-up-p
    :type boolean
    :documentation "Whether urgent prompt and child-agent limits are active.")
   (installation-provenance
    :initarg :installation-provenance
    :initform nil
    :accessor application-installation-provenance
    :type (option installation-provenance)
    :documentation "The structurally validated installation method for this run.")
   (update-availability
    :initarg :update-availability
    :initform nil
    :accessor application-update-availability
    :type (option update-availability)
    :documentation "The newer nondismissed release cached before startup.")
   (update-check-thread
    :initform nil
    :accessor application-update-check-thread
    :type t
    :documentation "The bounded availability thread awaiting checkpoint quiescence.")
   (permission-state
    :initarg :permission-state
    :initform (make-instance 'permission-state)
    :accessor application-permission-state
    :type permission-state
    :documentation "The exact command approvals loaded from private state.")
   (permission-mode
    :initarg :permission-mode
    :initform ':ask
    :accessor application-permission-mode
    :type (member :ask :auto :sandboxed :full-access)
    :documentation "The command approval behavior for this process session.")
   (command-classifications
    :initform (make-hash-table :test #'equal)
    :reader application-command-classifications
    :type hash-table
    :documentation "Session-cached model permission decisions per command and directory.")
   (command-authorization-lock
    :initform (make-lock "Autolith command authorization")
    :reader application-command-authorization-lock
    :documentation "The lock serializing command prompts from concurrent agents.")
   (task-presentation-lock
    :initform (make-lock "Autolith task presentation")
    :reader application-task-presentation-lock
    :documentation "The lock serializing scheduler snapshots into the active UI.")
   (task-presentation-orchestrator
    :initform nil
    :accessor application-task-presentation-orchestrator
    :type (option task-orchestrator)
    :documentation "The task runtime currently projected into the terminal.")
   (task-presentation-listener
    :initform nil
    :accessor application-task-presentation-listener
    :type (option function)
    :documentation "The exact task event callback removed during reconnection.")
   (input-controller
    :initform nil
    :accessor application-input-controller
    :type t
    :documentation "The active responsive input controller, when one is running.")
   (recovery-startup-p
    :initarg :recovery-startup-p
    :initform nil
    :accessor application-recovery-startup-p
    :type boolean
    :documentation
    "Whether this application reconnected the launcher-selected crash conversation.")
   (recovery-input-vault-failure
    :initform nil
    :accessor application-recovery-input-vault-failure
    :type t
    :documentation
    "The startup failure that kept recovered input outside the vault, when present.")
   (localgroup-session
    :initform nil
    :accessor application-localgroup-session
    :type t
    :documentation "The process-local discovery and control endpoint, when running.")
   (mutation-replay-failures
    :initform nil
    :accessor application-mutation-replay-failures
    :type list
    :documentation "Private replay scripts that failed at startup, with reasons.")
   (render-lock
    :initform (make-recursive-lock "Autolith transcript rendering")
    :reader application-render-lock
    :type t
    :documentation "The lock serializing transcript cursors and history boundaries.")
   (rendered-sequence
    :initform 0
    :accessor application-rendered-sequence
    :type integer
    :documentation "The last durable conversation sequence printed to scrollback.")
   (render-position
    :initform 0
    :accessor application-render-position
    :type (integer 0)
    :documentation "The next append-log file position to inspect for transcript output.")
   (render-generation
    :initform 0
    :accessor application-render-generation
    :type (integer 0)
    :documentation "The conversation log generation matching RENDER-POSITION.")
   (transcript-synchronized-p
    :initform nil
    :accessor application-transcript-synchronized-p
    :type boolean
    :documentation "Whether bounded startup replay has reached the durable log tail.")
   (history-floor-sequence
    :initform nil
    :accessor application-history-floor-sequence
    :type (option (integer 0))
    :documentation "The oldest transcript candidate explicitly shown in this process.")
   (recovery-session-publication
    :initform nil
    :accessor application-recovery-session-publication
    :type t
    :documentation "The last recovery-session record published by this process.")
   (presentation-counter
    :initform 0
    :accessor application-presentation-counter
    :type integer
    :documentation "The identifier source for non-conversation terminal notices.")
   (project-adaptation-offer-p
    :initform nil
    :accessor application-project-adaptation-offer-p
    :type boolean
    :documentation
    "Whether command-line resume should offer project adaptation notes."))
  (:documentation "The globally rooted logical state and reconnectable resources of Autolith."))

(defvar *active-application* nil
  "The live application root retained in saved generations.")

(defvar *terminal-resize-pending-p* nil
  "True after SIGWINCH until the active UI recomputes its dimensions.")


;;;; -- Construction and Reconnection --

(-> application--recovery-session-pointer
    (configuration)
    (option pathname))
(defun application--recovery-session-pointer (configuration)
  "Return this launcher's contained recovery-session pointer, when configured."
  (let ((value (uiop:getenv "AUTOLITH_RECOVERY_SESSION_POINTER")))
    (when (non-empty-string-p value)
      (let* ((pathname (pathname value))
             (root
               (merge-pathnames
                "recovery-session-pointers/"
                (configuration-state-root configuration))))
        (and (uiop:absolute-pathname-p pathname)
             (uiop:subpathp pathname root)
             pathname)))))

(-> application-publish-recovery-session (application) null)
(defun application-publish-recovery-session (application)
  "Best-effort publish the active durable conversation for hard-crash recovery."
  (handler-case
      (with-recursive-lock-held ((application-render-lock application))
        (let* ((configuration (application-configuration application))
               (pathname
                 (application--recovery-session-pointer configuration))
               (conversation (application-conversation application)))
          (when pathname
            (let* ((record
                     (and
                      (conversation-persisted-p conversation)
                      (list :recovery-session
                            :version 1
                            :conversation-id
                            (conversation-identifier conversation)
                            :rendered-sequence
                            (max
                             0
                             (application-rendered-sequence application))
                            :history-floor-sequence
                            (application-history-floor-sequence application))))
                   (publication
                     (list (namestring pathname) record)))
              (unless
                  (equal publication
                         (application-recovery-session-publication
                          application))
                (if record
                    (snapshot-write pathname record)
                    (when (probe-file pathname)
                      (delete-file pathname)))
                (setf (application-recovery-session-publication application)
                      publication))))))
    (error ()
      nil))
  nil)

(-> application--recovery-conversation-id () (option string))
(defun application--recovery-conversation-id ()
  "Return the conversation selected by automatic recovery, when present."
  (let ((value (uiop:getenv "AUTOLITH_RECOVERY_CONVERSATION_ID")))
    (and (non-empty-string-p value) value)))

(-> application--recovery-sequence (string) (option integer))
(defun application--recovery-sequence (name)
  "Return one nonnegative recovery sequence from environment variable NAME."
  (handler-case
      (let ((value (uiop:getenv name)))
        (and
         (non-empty-string-p value)
         (let ((parsed (parse-integer value :junk-allowed nil)))
           (and (not (minusp parsed)) parsed))))
    (error ()
      nil)))

(-> application--selected-conversation-id
    ((option string) (option string))
    (option string))
(defun application--selected-conversation-id
    (conversation-id recovery-conversation-id)
  "Return explicit CONVERSATION-ID before automatic recovery selection."
  (or conversation-id recovery-conversation-id))

(-> application--clear-recovery-environment () null)
(defun application--clear-recovery-environment ()
  "Remove one-shot recovery reconnection metadata after successful startup."
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_CONVERSATION_ID")
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")
  nil)

(-> terminal--positive-integer-or-nil ((option string)) (option integer))
(defun terminal--positive-integer-or-nil (value)
  "Parse VALUE as a positive integer, returning NIL on failure."
  (handler-case
      (let ((parsed (and (non-empty-string-p value)
                         (parse-integer value :junk-allowed t))))
        (and parsed (plusp parsed) parsed))
    (error ()
      nil)))

(-> terminal--query-dimension (string) (option integer))
(defun terminal--query-dimension (capability)
  "Return positive TPUT CAPABILITY output when a terminal is attached."
  (when (interactive-stream-p *terminal-io*)
    (handler-case
        (terminal--positive-integer-or-nil
         (uiop:run-program (list "tput" capability)
                           :output ':string
                           :error-output ':output))
      (error ()
        nil))))

(-> terminal-current-size () (values integer integer))
(defun terminal-current-size ()
  "Return current terminal rows and columns, preferring its kernel dimensions."
  (multiple-value-bind (terminal-rows terminal-columns)
      (terminal-file-descriptor-size 0)
    (values
     (or terminal-rows
         (terminal--query-dimension "lines")
         (terminal--positive-integer-or-nil (uiop:getenv "LINES"))
         *terminal-default-rows*)
     (or terminal-columns
         (terminal--query-dimension "cols")
         (terminal--positive-integer-or-nil (uiop:getenv "COLUMNS"))
         *terminal-default-columns*))))

(-> application-pending-terminal-size
    ()
    (option (cons (integer 1) (integer 1))))
(defun application-pending-terminal-size ()
  "Consume a pending SIGWINCH and return refreshed rows and columns."
  (when *terminal-resize-pending-p*
    (setf *terminal-resize-pending-p* nil)
    (multiple-value-bind (rows columns)
        (terminal-current-size)
      (cons rows columns))))

(defparameter *application-prompt* "❯ "
  "The styled input prompt shown on the live editor row.")

(defparameter *application-placeholder*
  "Ask Autolith anything. Type (help) for operations."
  "The dim hint shown on the prompt row while input is empty.")

(-> application-terminal-ui-create () terminal-ui)
(defun application-terminal-ui-create ()
  "Create the standard interactive terminal UI at the current terminal size."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (terminal-ui-create
     :terminal
     (localgroup-terminal-create
      (unless (localgroup-startup-detached-p)
        (stream-terminal-create :rows rows :columns columns)))
     :prompt *application-prompt*
     :placeholder *application-placeholder*
     :completion-function #'application-command-completion-entries)))

(-> application--configuration-for-conversation
    (configuration conversation)
    configuration)
(defun application--configuration-for-conversation (configuration conversation)
  "Restore CONVERSATION's model selection over CONFIGURATION when it remains valid.

A recorded model that no effective provider registration serves falls back to
CONFIGURATION's own model, so removing a provider cannot block reopening a
conversation. Omitting an unsupported recorded effort leaves the selected
model's effort choice to CONFIGURATION--CLONE."
  (let* ((recorded-model (conversation-model conversation))
         (model (if (and recorded-model
                         (configuration--model-supported-p recorded-model))
                    recorded-model
                    (configuration-model configuration)))
         (recorded-effort (conversation-reasoning-effort conversation)))
    (if (and recorded-effort
             (member recorded-effort
                     (configuration--reasoning-efforts-for model)
                     :test #'string=))
        (configuration--clone configuration
                              :model model
                              :reasoning-effort recorded-effort)
        (configuration--clone configuration :model model))))

(-> application--task-orchestrator
    (application)
    (option task-orchestrator))
(defun application--task-orchestrator (application)
  "Return APPLICATION's task runtime, when its registry provides one."
  (let ((registry
          (and (slot-boundp application 'tool-registry)
               (application-tool-registry application))))
    (when (typep registry 'tool-registry)
      (let ((tool (tool-registry-find registry "task" "run")))
        (and (typep tool 'task-run-tool)
             (task-run-tool-orchestrator tool))))))

(-> application--refresh-task-presentation-locked
    (application task-orchestrator)
    null)
(defun application--refresh-task-presentation-locked (application orchestrator)
  "Project ORCHESTRATOR's live children while APPLICATION's task lock is held."
  (when (eq orchestrator
            (application-task-presentation-orchestrator application))
    (let ((ui
            (and (slot-boundp application 'ui)
                 (application-ui application))))
      (when (typep ui 'terminal-ui)
        (terminal-ui-set-agent-activities
         ui
         (task-orchestrator-live-activities orchestrator)))))
  nil)

(-> application--refresh-task-presentation
    (application task-orchestrator)
    null)
(defun application--refresh-task-presentation (application orchestrator)
  "Project ORCHESTRATOR's newest live children into APPLICATION's UI."
  (with-lock-held ((application-task-presentation-lock application))
    (application--refresh-task-presentation-locked application orchestrator))
  nil)

(-> application--present-task-response
    (application task-orchestrator function list)
    null)
(defun application--present-task-response
    (application orchestrator listener payload)
  "Present one response from APPLICATION's exact current task LISTENER."
  (with-lock-held ((application-task-presentation-lock application))
    (when (and (eq orchestrator
                   (application-task-presentation-orchestrator application))
               (eq listener
                   (application-task-presentation-listener application)))
      (let ((entry (application--child-response-entry application payload)))
        (when entry
          (application-present application entry)))))
  nil)

(-> application--disconnect-task-presentation-locked (application) null)
(defun application--disconnect-task-presentation-locked (application)
  "Disconnect APPLICATION's task observer while its presentation lock is held."
  (let ((orchestrator
          (application-task-presentation-orchestrator application))
        (listener
          (application-task-presentation-listener application)))
    (setf (application-task-presentation-orchestrator application) nil
          (application-task-presentation-listener application) nil)
    (when (and orchestrator listener)
      (task-orchestrator-remove-listener orchestrator listener))
    (let ((ui
            (and (slot-boundp application 'ui)
                 (application-ui application))))
      (when (typep ui 'terminal-ui)
        (terminal-ui-set-agent-activities ui nil))))
  nil)

(-> application-disconnect-task-presentation (application) null)
(defun application-disconnect-task-presentation (application)
  "Disconnect APPLICATION's task observer and clear its child-agent rows."
  (with-lock-held ((application-task-presentation-lock application))
    (application--disconnect-task-presentation-locked application))
  nil)

(-> application-connect-task-presentation (application) null)
(defun application-connect-task-presentation (application)
  "Atomically replace APPLICATION's task listener and active UI projection."
  (with-lock-held ((application-task-presentation-lock application))
    (application--disconnect-task-presentation-locked application)
    (let ((agent (and (slot-boundp application 'agent)
                      (application-agent application))))
      (when (typep agent 'agent)
        (setf (agent-hurry-up-p agent) (application-hurry-up-p application))))
    (let ((orchestrator (application--task-orchestrator application)))
      (when orchestrator
        (task-orchestrator-set-hurry-up
         orchestrator (application-hurry-up-p application))
        (let ((listener nil))
          (setf listener
                (lambda (channel payload)
                  (case channel
                    ((:task-subagent-lifecycle :task-subagent-progress)
                     (application--refresh-task-presentation
                      application orchestrator))
                    (:task-subagent-verbal-response
                     (application--present-task-response
                      application orchestrator listener payload)))))
          (setf (application-task-presentation-orchestrator application)
                orchestrator
                (application-task-presentation-listener application)
                listener)
          (task-orchestrator-add-listener orchestrator listener)
          (application--refresh-task-presentation-locked
           application orchestrator)))))
  nil)

(-> application--create-tool-registry (configuration) tool-registry)
(defun application--create-tool-registry (configuration)
  "Create one complete registry and close its base runtimes on later failure."
  (let ((registry nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf registry
                 (make-default-tool-registry
                  :immutable-p
                  (configuration-immutable-p configuration)))
           (setf registry
                 (task-augment-tool-registry registry))
           (multiple-value-bind (augmented manager)
               (mcp-tool-registry-augment registry configuration)
             (declare (ignore manager))
             (setf registry augmented
                   completed-p t)
             registry))
      (unless completed-p
        (when registry
          (ignore-errors
            (tool-registry-close-runtime-state registry)))))))

(-> application--extension-registry-snapshot () list)
(defun application--extension-registry-snapshot ()
  "Return exact MCP, context, command, and provider snapshots."
  (list :mcp (mcp--registry-snapshot)
        :context (context--registry-snapshot)
        :command (application-command--registry-snapshot)
        :provider (provider--registry-snapshot)))

(-> application--extension-registry-restore (list) null)
(defun application--extension-registry-restore (snapshot)
  "Restore exact extension registrations from SNAPSHOT."
  (mcp--registry-restore (getf snapshot :mcp))
  (context--registry-restore (getf snapshot :context))
  (application-command--registry-restore (getf snapshot :command))
  (provider--registry-restore (getf snapshot :provider))
  nil)

(-> application--load-extension-configuration (configuration) null)
(defun application--load-extension-configuration (configuration)
  "Load native MCP and executable user configuration for CONFIGURATION."
  (mcp-configuration-load configuration)
  (user-init-load configuration)
  nil)

(-> application--discard-connection-resources
    ((option application) (option tool-registry) t)
    list)
(defun application--discard-connection-resources
    (application registry worker)
  "Best-effort close partial resources and return every serious cleanup failure."
  (let ((failures nil))
    (when application
      (handler-case
          (application-disconnect-task-presentation application)
        (serious-condition (condition)
          (push condition failures))))
    (when registry
      (handler-case
          (tool-registry-close-runtime-state registry)
        (serious-condition (condition)
          (push condition failures))))
    (when worker
      (handler-case
          (lisp-worker-manager-stop worker)
        (serious-condition (condition)
          (push condition failures))))
    (nreverse failures)))

(-> application--restore-retired-tool-registry
    (application tool-registry list &key (:reconnect-p boolean))
    list)
(defun application--restore-retired-tool-registry
    (application registry quiesced-tools &key reconnect-p)
  "Resume QUIESCED-TOOLS and reconnect APPLICATION, returning restoration failures."
  (let ((failures nil))
    (handler-case
        (tool-registry-resume-runtime-state
         registry :tools quiesced-tools)
      (serious-condition (condition)
        (push condition failures)))
    (when reconnect-p
      (handler-case
          (application-connect-task-presentation application)
        (serious-condition (condition)
          (push condition failures))))
    (nreverse failures)))

(-> application--raise-runtime-replacement-error
    (&key (:operation keyword)
          (:stage keyword)
          (:cause serious-condition)
          (:rollback-causes list))
    null)
(defun application--raise-runtime-replacement-error
    (&key operation stage cause rollback-causes)
  "Signal a structured runtime replacement failure for OPERATION and STAGE."
  (error
   'application-runtime-replacement-error
   :message
   (format nil
           "Could not replace the application runtime during ~(~A~) ~(~A~).~:[~; The previous runtime could not be restored completely.~]"
           operation
           stage
           (not (null rollback-causes)))
   :operation operation
   :stage stage
   :cause cause
   :rollback-causes rollback-causes))

(-> application--validate-provider-registration (model-provider configuration) null)
(defun application--validate-provider-registration (provider configuration)
  "Reject a reload that loses the provider serving the active model."
  (let* ((previous-registration (model-provider-registration provider))
         (selected-registration
           (provider-registration-for-model (configuration-model configuration))))
    (when (and previous-registration
               (or (null selected-registration)
                   (not (and
                         (string= (provider-registration-name previous-registration)
                                  (provider-registration-name selected-registration))
                         (eq (provider-registration-source previous-registration)
                             (provider-registration-source selected-registration))))))
      (error 'configuration-error
             :message
             (format nil
                     "Provider ~A no longer serves active model ~A after configuration reload."
                     (provider-registration-name previous-registration)
                     (configuration-model configuration)))))
  nil)

(-> application-reload-mcp (application) null)
(defun application-reload-mcp (application)
  "Reload native extensions and user init registrations into a fresh runtime."
  (let* ((configuration (application-configuration application))
         (extension-registry-snapshot nil)
         (old-configuration configuration)
         (old-provider
           (and (slot-boundp application 'provider)
                (application-provider application)))
         (old-registry (application-tool-registry application))
         (old-agent
           (and (slot-boundp application 'agent)
                (application-agent application)))
         (new-provider nil)
         (new-registry nil)
         (new-agent nil)
         (application-swapped-p nil)
         (retirement-started-p nil)
         (presentation-disconnected-p nil)
         (quiesced-tools nil)
         (failure nil)
         (failure-stage ':prepare)
         (rollback-failures nil)
         (committed-p nil))
    (with-extension-registry-transaction
      (setf extension-registry-snapshot
            (application--extension-registry-snapshot))
      (unwind-protect
           (handler-case
               (progn
                 (mcp-configuration-load configuration)
                 (user-init-load configuration)
                 (setf configuration
                       (provider-bootstrap-configuration configuration))
                 (when old-provider
                   (application--validate-provider-registration
                    old-provider configuration))
                 (setf new-provider
                       (provider-create
                        configuration
                        :reasoning-summaries-p
                        (application-reasoning-traces-p application))
                       new-registry
                       (application--create-tool-registry configuration))
                 (setf new-agent
                       (agent-create
                        :configuration configuration
                        :provider new-provider
                        :conversation (application-conversation application)
                        :tool-registry new-registry
                        :worker (application-worker application))
                       retirement-started-p t
                       failure-stage ':retire)
                 (application-disconnect-task-presentation application)
                 (setf presentation-disconnected-p t)
                 (multiple-value-bind (completed retirement-failure)
                     (tool-registry-quiesce-runtime-state old-registry)
                   (setf quiesced-tools completed)
                   (when retirement-failure
                     (error retirement-failure)))
                 (setf failure-stage ':install
                       application-swapped-p t
                       (application-configuration application) configuration
                       (application-provider application) new-provider
                       (application-tool-registry application) new-registry
                       (application-agent application) new-agent)
                 (application-connect-task-presentation application)
                 (context-runtime-reset)
                 (setf committed-p t))
             (serious-condition (condition)
               (setf failure condition)))
        (unless committed-p
          (when application-swapped-p
            (handler-case
                (application-disconnect-task-presentation application)
              (serious-condition (condition)
                (push condition rollback-failures))))
          (when application-swapped-p
            (setf (application-configuration application) old-configuration
                  (application-provider application) old-provider
                  (application-tool-registry application) old-registry
                  (application-agent application) old-agent))
          (application--extension-registry-restore
           extension-registry-snapshot)
          (when new-registry
            (handler-case
                (tool-registry-close-runtime-state new-registry)
              (serious-condition (condition)
                (push condition rollback-failures))))
          (when retirement-started-p
            (setf rollback-failures
                  (nconc
                   rollback-failures
                   (application--restore-retired-tool-registry
                    application
                    old-registry
                    quiesced-tools
                    :reconnect-p presentation-disconnected-p)))))))
    (when failure
      (if retirement-started-p
          (application--raise-runtime-replacement-error
           :operation ':mcp-reload
           :stage failure-stage
           :cause failure
           :rollback-causes rollback-failures)
          (error failure))))
  nil)

(-> application--conversation-lease-select
    ((option application) configuration string)
    (values conversation-lease boolean))
(defun application--conversation-lease-select
    (application configuration identifier)
  "Return IDENTIFIER's held or newly acquired application lease.

The second value is true only when the caller owns cleanup of a newly acquired
lease."
  (let* ((normalized
           (conversation-identifier-migration-resolve
            configuration identifier))
         (current
           (and application
                (slot-boundp application 'conversation-lease)
                (application-conversation-lease application))))
    (if (and current
             (conversation-lease-matches-p current normalized))
        (values current nil)
        (values
         (conversation-lease-acquire configuration normalized)
         t))))

(-> application--conversation-load-owned
    ((option application) configuration string)
    (values conversation conversation-lease boolean))
(defun application--conversation-load-owned
    (application configuration identifier)
  "Claim IDENTIFIER before loading and possibly repairing its conversation.

Return the conversation, its lease, and whether the caller owns cleanup of a
newly acquired lease."
  (multiple-value-bind (lease acquired-p)
      (application--conversation-lease-select
       application configuration identifier)
    (let ((completed-p nil))
      (unwind-protect
           (let ((conversation
                   (conversation-load-by-id configuration identifier)))
             (setf completed-p t)
             (values conversation lease acquired-p))
        (unless completed-p
          (when acquired-p
            (conversation-lease-release lease)))))))

(-> application--conversation-create-owned
    ((option application) configuration
     &key (:timestamp (option timestamp)))
    (values conversation conversation-lease boolean))
(defun application--conversation-create-owned
    (application configuration &key timestamp)
  "Create and claim a fresh conversation, probing another seed on lease races."
  (let ((last-conflict nil)
        (created-at (or timestamp (get-universal-time))))
    (loop repeat (identifier-base)
          do (let ((conversation
                     (conversation-create
                      configuration :created-at created-at)))
               (handler-case
                   (multiple-value-bind (lease acquired-p)
                       (application--conversation-lease-select
                        application
                        configuration
                        (conversation-identifier conversation))
                     (return-from application--conversation-create-owned
                       (values conversation lease acquired-p)))
                 (conversation-in-use (condition)
                   (setf last-conflict condition)))))
    (error last-conflict)))

(-> application-release-conversation-lease (application) null)
(defun application-release-conversation-lease (application)
  "Release APPLICATION's primary conversation lease idempotently."
  (when (slot-boundp application 'conversation-lease)
    (let ((lease (application-conversation-lease application)))
      (when lease
        (setf (application-conversation-lease application) nil)
        (conversation-lease-release lease))))
  nil)

(-> application-create
    (configuration &key (:conversation-id (option string))
                        (:permission-mode (member :ask :auto :sandboxed :full-access)))
    application)
(defun application-create
    (configuration &key conversation-id (permission-mode ':ask))
  "Create a connected application, loading CONVERSATION-ID with PERMISSION-MODE."
  (let ((preferred-configuration configuration))
    (context-runtime-reset)
    (configuration-ensure-directories preferred-configuration)
    (conversation-identifier-migrate preferred-configuration)
    (durable-mutations-load preferred-configuration)
    (let* ((recovery-state
             (multiple-value-list
              (application-recovery-state preferred-configuration)))
           (recovery-conversation-id (first recovery-state))
           (selected-conversation-id
             (application--selected-conversation-id
              conversation-id recovery-conversation-id))
           (recovery-rendered-sequence (second recovery-state))
           (recovery-history-floor-sequence (third recovery-state))
           (mutation-replay-failures (image-state-load preferred-configuration))
           (extension-registry-snapshot nil)
           (registry nil)
           (worker nil)
           (conversation nil)
           (conversation-lease nil)
           (conversation-lease-acquired-p nil)
           (application nil)
           (completed-p nil))
      (unwind-protect
           (with-extension-registry-transaction
             (setf extension-registry-snapshot
                   (application--extension-registry-snapshot))
             (unwind-protect
                  (progn
                    (mcp-configuration-load preferred-configuration)
                    (user-init-load preferred-configuration)
                    (setf preferred-configuration
                          (provider-bootstrap-configuration
                           preferred-configuration))
                    (setf preferred-configuration
                          (preferences-apply-model-selection
                           preferred-configuration))
                    (multiple-value-setq
                        (conversation
                         conversation-lease
                         conversation-lease-acquired-p)
                      (if selected-conversation-id
                          (application--conversation-load-owned
                           nil
                           preferred-configuration
                           selected-conversation-id)
                          (application--conversation-create-owned
                           nil preferred-configuration)))
                    (let* ((reasoning-traces-p
                             (preferences-reasoning-traces-p
                              preferred-configuration))
                           (compact-view-p
                             (preferences-compact-view-p
                              preferred-configuration))
                           (turn-timestamps-p
                             (preferences-turn-timestamps-p
                              preferred-configuration))
                           (permission-state
                             (permissions-load preferred-configuration))
                           (configuration
                             (application--configuration-for-conversation
                              preferred-configuration
                              conversation))
                           (recovering-conversation-p
                             (and
                              (null conversation-id)
                              recovery-conversation-id
                              (string=
                               (conversation-identifier conversation)
                               (conversation-identifier-migration-resolve
                                preferred-configuration
                                recovery-conversation-id))))
                           (installation-provenance
                             (installation-provenance-detect configuration))
                           (update-availability
                             (update-availability-current
                              configuration installation-provenance))
                           (provider
                             (provider-create
                              configuration
                              :reasoning-summaries-p reasoning-traces-p)))
                      (setf registry
                            (application--create-tool-registry configuration)
                            worker
                            (lisp-worker-pool-create configuration))
                      (let* ((agent
                               (agent-create
                                :configuration configuration
                                :provider provider
                                :conversation conversation
                                :tool-registry registry
                                :worker worker))
                             (ui (application-terminal-ui-create)))
                        (setf application
                              (make-instance
                               'application
                               :configuration configuration
                               :conversation conversation
                               :conversation-lease conversation-lease
                               :provider provider
                               :tool-registry registry
                               :worker worker
                               :agent agent
                               :ui ui
                               :permission-state permission-state
                               :permission-mode permission-mode
                               :reasoning-traces-p reasoning-traces-p
                               :compact-view-p compact-view-p
                               :turn-timestamps-p turn-timestamps-p
                               :installation-provenance
                               installation-provenance
                               :update-availability update-availability
                               :recovery-startup-p
                               (not (null recovering-conversation-p)))
                              (application-mutation-replay-failures application)
                              mutation-replay-failures)
                        (multiple-value-bind
                              (restored-rendered-sequence
                               restored-history-floor-sequence)
                            (application--normalize-recovery-cursors
                             conversation
                             (not (null recovering-conversation-p))
                             recovery-rendered-sequence
                             recovery-history-floor-sequence)
                          (setf
                           (application-rendered-sequence application)
                           restored-rendered-sequence
                           (application-render-position application) 0
                           (application-render-generation application)
                           (conversation-log-generation conversation)
                           (application-transcript-synchronized-p application)
                           nil
                           (application-history-floor-sequence application)
                           restored-history-floor-sequence))
                        (application-connect-task-presentation application)
                        (application--load-goal application)
                        (application-publish-recovery-session application)
                        (setf completed-p t)
                        application)))
               (unless completed-p
                 (application--extension-registry-restore
                  extension-registry-snapshot))))
        (unless completed-p
          (application--discard-connection-resources
           application registry worker)
          (when conversation-lease-acquired-p
            (conversation-lease-release conversation-lease)))))))

(-> application--normalize-recovery-cursors
    (conversation boolean (option integer) (option integer))
    (values (integer 0) (option (integer 1))))
(defun application--normalize-recovery-cursors
    (conversation recovering-p rendered-sequence history-floor-sequence)
  "Clamp recovered transcript cursors to CONVERSATION's durable sequence range."
  (let* ((latest-sequence
           (max 0 (1- (conversation-next-sequence conversation))))
         (rendered
           (if (and recovering-p
                    (typep rendered-sequence '(integer 0)))
               (min rendered-sequence latest-sequence)
               0))
         (history-floor
           (and recovering-p
                (typep history-floor-sequence '(integer 1))
                (<= history-floor-sequence (1+ rendered))
                history-floor-sequence)))
    (values rendered history-floor)))

(-> application-reconnect
    (application &key (:conversation-id (option string))
                      (:immutable-p boolean)
                      (:permission-mode (member :ask :auto :sandboxed :full-access)))
    application)
(defun application-reconnect
    (application &key conversation-id
                   (immutable-p nil immutable-p-supplied-p)
                   (permission-mode ':ask))
  "Build and commit a replacement using PERMISSION-MODE without damaging APPLICATION."
  (let* ((previous (application-configuration application))
         (effective-immutable-p
           (if immutable-p-supplied-p
               immutable-p
               (configuration-immutable-p previous)))
         (retained-conversation (application-conversation application))
         (retained-conversation-lease
           (and (slot-boundp application 'conversation-lease)
                (application-conversation-lease application)))
         (extension-registry-snapshot nil)
         (registry nil)
         (worker nil)
         (selected-conversation-lease nil)
         (selected-conversation-lease-acquired-p nil)
         (new-application nil)
         (old-registry (application-tool-registry application))
         (retirement-started-p nil)
         (presentation-disconnected-p nil)
         (quiesced-tools nil)
         (failure-stage ':prepare)
         (rollback-failures nil)
         (completed-p nil))
    (handler-case
        (unwind-protect
             (with-extension-registry-transaction
           (setf extension-registry-snapshot
                 (application--extension-registry-snapshot))
           (unwind-protect
                (let* ((retained-configuration
                  (configuration-create
                   :working-directory (uiop:getcwd)
                   :model (configuration-model previous)
                   :reasoning-effort
                   (configuration-reasoning-effort previous)
                   :immutable-p effective-immutable-p
                   :defer-provider-validation-p t))
                (prepared-configuration
                  (progn
                    (configuration-ensure-directories retained-configuration)
                    (conversation-identifier-migrate retained-configuration)
                    (mcp-configuration-load retained-configuration)
                    (user-init-load retained-configuration)
                    (setf retained-configuration
                          (provider-bootstrap-configuration
                           retained-configuration))
                    retained-configuration))
                (reasoning-traces-p
                  (preferences-reasoning-traces-p prepared-configuration))
                (compact-view-p
                  (preferences-compact-view-p prepared-configuration))
                (turn-timestamps-p
                  (preferences-turn-timestamps-p prepared-configuration))
                (permission-state (permissions-load prepared-configuration))
                (recovery-state
                  (multiple-value-list
                   (application-recovery-state prepared-configuration)))
                (recovery-conversation-id (first recovery-state))
                (selected-conversation-id
                  (application--selected-conversation-id
                   conversation-id recovery-conversation-id))
                (conversation
                  (multiple-value-bind
                      (owned-conversation lease acquired-p)
                      (cond
                        (selected-conversation-id
                         (application--conversation-load-owned
                          application
                          prepared-configuration
                          selected-conversation-id))
                        ((conversation-persisted-p retained-conversation)
                         (application--conversation-load-owned
                          application
                          prepared-configuration
                          (conversation-identifier retained-conversation)))
                        (t
                         (application--conversation-create-owned
                          application prepared-configuration)))
                    (setf selected-conversation-lease lease
                          selected-conversation-lease-acquired-p acquired-p)
                    owned-conversation))
                (configuration
                  (application--configuration-for-conversation
                   prepared-configuration
                   conversation))
                (installation-provenance
                  (installation-provenance-detect configuration))
                (update-availability
                  (update-availability-current
                   configuration installation-provenance))
                (provider
                  (progn
                    (when (and (slot-boundp application 'provider)
                               (application-provider application))
                      (application--validate-provider-registration
                       (application-provider application)
                       configuration))
                    (provider-create
                     configuration
                     :reasoning-summaries-p reasoning-traces-p)))
                (recovery-rendered-sequence (second recovery-state))
                (recovery-history-floor-sequence (third recovery-state))
                (recovering-conversation-p
                  (and
                   (null conversation-id)
                   recovery-conversation-id
                   (string=
                    (conversation-identifier conversation)
                    (conversation-identifier-migration-resolve
                     prepared-configuration
                     recovery-conversation-id)))))
           (setf worker (lisp-worker-pool-create configuration)
                 registry (application--create-tool-registry configuration))
           (let* ((agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry registry
                                  :worker worker))
                  (ui (application-terminal-ui-create)))
             (setf new-application
                   (make-instance
                    'application
                    :configuration configuration
                    :conversation conversation
                    :conversation-lease selected-conversation-lease
                    :provider provider
                    :tool-registry registry
                    :worker worker
                    :agent agent
                    :ui ui
                    :permission-state permission-state
                    :permission-mode permission-mode
                    :reasoning-traces-p reasoning-traces-p
                    :compact-view-p compact-view-p
                    :turn-timestamps-p turn-timestamps-p
                    :installation-provenance installation-provenance
                    :update-availability update-availability
                    :recovery-startup-p
                    (not (null recovering-conversation-p))))
             (multiple-value-bind
                 (restored-rendered-sequence
                  restored-history-floor-sequence)
                 (application--normalize-recovery-cursors
                  conversation
                  (not (null recovering-conversation-p))
                  recovery-rendered-sequence
                  recovery-history-floor-sequence)
               (setf
                (application-rendered-sequence new-application)
                restored-rendered-sequence
                (application-render-position new-application) 0
                (application-render-generation new-application)
                (conversation-log-generation conversation)
                (application-transcript-synchronized-p new-application) nil
                (application-history-floor-sequence new-application)
                restored-history-floor-sequence
                (application-mutation-replay-failures new-application) nil))
             (application--load-goal new-application)
             (image-state-reconnect)
             (setf retirement-started-p t
                   failure-stage ':retire)
             (application-disconnect-task-presentation application)
             (setf presentation-disconnected-p t)
             (multiple-value-bind (completed retirement-failure)
                 (tool-registry-quiesce-runtime-state old-registry)
               (setf quiesced-tools completed)
               (when retirement-failure
                 (error retirement-failure)))
             (setf failure-stage ':install)
             (application-connect-task-presentation new-application)
             (context-runtime-reset)
             (application-publish-recovery-session new-application)
             (setf completed-p t)
             (when (and retained-conversation-lease
                        (not
                         (eq retained-conversation-lease
                             selected-conversation-lease)))
               (conversation-lease-release retained-conversation-lease)
               (setf (application-conversation-lease application) nil))
             (when (and (slot-boundp application 'worker)
                        (application-worker application))
               (ignore-errors
                 (lisp-worker-manager-stop
                  (application-worker application))))
             new-application))
             (unless completed-p
               (application--extension-registry-restore
                extension-registry-snapshot))))
          (unless completed-p
            (setf rollback-failures
                  (application--discard-connection-resources
                   new-application registry worker))
            (when selected-conversation-lease-acquired-p
              (conversation-lease-release selected-conversation-lease))
            (when retirement-started-p
              (setf rollback-failures
                    (nconc
                     rollback-failures
                     (application--restore-retired-tool-registry
                      application
                      old-registry
                      quiesced-tools
                      :reconnect-p presentation-disconnected-p))))))
      (serious-condition (condition)
        (if retirement-started-p
            (application--raise-runtime-replacement-error
             :operation ':reconnect
             :stage failure-stage
             :cause condition
             :rollback-causes rollback-failures)
            (error condition))))))

(-> application--quiesce-update-check (application) null)
(defun application--quiesce-update-check (application)
  "Join APPLICATION's bounded availability thread before a checkpoint fork."
  (let ((thread (application-update-check-thread application)))
    (when thread
      (when (eq thread (current-thread))
        (error 'checkpoint-error
               :message "An update check cannot quiesce its own thread."
               :stage ':fork
               :pathname nil))
      (join-thread thread)
      (when (eq thread (application-update-check-thread application))
        (setf (application-update-check-thread application) nil))))
  nil)

(defmethod checkpoint-detach-state ((application application))
  "Detach APPLICATION's ephemeral object graph in a checkpoint saver child."
  (context-runtime-reset)
  (let ((conversation (application-conversation application)))
    (setf (conversation-turn-state conversation) nil)
    (conversation-clear-ephemeral-input-items conversation))
  (application-release-conversation-lease application)
  (tool-registry-detach-runtime-state
   (application-tool-registry application))
  (setf (application-provider application) nil
        (application-worker application) nil
        (application-agent application) nil
        (application-ui application) nil
        (application-task-presentation-orchestrator application) nil
        (application-task-presentation-listener application) nil
        (application-input-controller application) nil
        (application-update-check-thread application) nil)
  application)

(-> application--install-configuration
    (application configuration &key (:conversation (option conversation)))
    null)
(defun application--install-configuration (application configuration
                                            &key conversation)
  "Switch APPLICATION to CONFIGURATION and optionally CONVERSATION."
  (let* ((active-conversation (or conversation
                                  (application-conversation application)))
         (previous-provider (application-provider application))
         (provider
           (if previous-provider
               (provider-with-configuration previous-provider configuration)
               (provider-create configuration)))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation active-conversation
                              :tool-registry (application-tool-registry
                                              application)
                              :worker (application-worker application))))
    (setf (application-configuration application) configuration
          (application-conversation application) active-conversation
          (application-provider application) provider
          (application-agent application) agent))
  nil)

(-> application--agent-adopt-runtime (application agent) null)
(defun application--agent-adopt-runtime (application agent)
  "Let running AGENT adopt APPLICATION's replaced runtime at a safe boundary.

Boundary-applied commands install a fresh configuration, provider, or tool
registry on APPLICATION while AGENT still executes the current turn. Adoption
between provider requests lets the change reach the very next request. The
running turn owns its conversation, so adoption is skipped entirely when a
command replaced the active conversation."
  (when (eq (agent-conversation agent)
            (application-conversation application))
    (let ((configuration (application-configuration application))
          (provider (application-provider application))
          (registry (application-tool-registry application)))
      (unless (eq (agent-configuration agent) configuration)
        (setf (agent-configuration agent) configuration))
      (unless (eq (agent-provider agent) provider)
        (setf (agent-provider agent) provider))
      (unless (eq (agent-tool-registry agent) registry)
        (setf (agent-tool-registry agent) registry))))
  nil)

(-> application--prepare-runtime-replacement
    (application configuration conversation)
    (values model-provider tool-registry agent))
(defun application--prepare-runtime-replacement
    (application configuration conversation)
  "Prepare a fresh provider, registry, and agent without touching APPLICATION."
  (let ((registry nil)
        (completed-p nil))
    (unwind-protect
         (let* ((previous-provider (application-provider application))
                (provider
                  (if previous-provider
                      (provider-with-configuration
                       previous-provider configuration)
                      (provider-create configuration))))
           (setf registry
                 (application--create-tool-registry configuration))
           (let ((agent
                   (agent-create
                    :configuration configuration
                    :provider provider
                    :conversation conversation
                    :tool-registry registry
                    :worker (application-worker application))))
             (setf completed-p t)
             (values provider registry agent)))
      (unless completed-p
        (when registry
          (ignore-errors
            (tool-registry-close-runtime-state registry)))))))

(-> application--working-directory-failure
    (&key (:requested-path t)
          (:previous-directory pathname)
          (:stage keyword)
          (:cause t)
          (:rollback-cause t))
    null)
(defun application--working-directory-failure
    (&key requested-path previous-directory stage cause rollback-cause)
  "Signal a structured workspace change failure and include any rollback failure."
  (error 'working-directory-error
         :message
         (format nil
                 "Could not change working directory from ~A to ~S during ~(~A~): ~A~@[ Rollback also failed: ~A~]"
                 previous-directory
                 requested-path
                 stage
                 cause
                 rollback-cause)
         :requested-path requested-path
         :previous-directory previous-directory
         :stage stage
         :cause cause
         :rollback-cause rollback-cause))

(-> application-set-working-directory
    (application (or pathname string))
    pathname)
(defun application-set-working-directory (application location)
  "Atomically replace APPLICATION's runtime in existing directory LOCATION."
  (let* ((previous-configuration (application-configuration application))
         (configuration
           (configuration-with-working-directory previous-configuration location))
         (previous-directory
           (configuration-working-directory previous-configuration))
         (directory (configuration-working-directory configuration))
         (manager (application-worker application))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (previous-conversation (application-conversation application))
         (previous-provider (application-provider application))
         (previous-registry (application-tool-registry application))
         (previous-agent (application-agent application))
         (provider nil)
         (registry nil)
         (agent nil)
         (workers-moved-p nil)
         (process-moved-p nil)
         (application-swapped-p nil)
         (retirement-started-p nil)
         (presentation-disconnected-p nil)
         (quiesced-tools nil)
         (committed-p nil)
         (failure nil)
         (failure-stage ':tools)
         (rollback-failures nil)
         (extension-registry-snapshot nil))
    (with-extension-registry-transaction
      (setf extension-registry-snapshot
            (application--extension-registry-snapshot))
      (unwind-protect
           (handler-case
               (progn
                 (setf retirement-started-p t
                       failure-stage ':tools)
                 (application-disconnect-task-presentation application)
                 (setf presentation-disconnected-p t)
                 (multiple-value-bind (completed retirement-failure)
                     (tool-registry-quiesce-runtime-state previous-registry)
                   (setf quiesced-tools completed)
                   (when retirement-failure
                     (error retirement-failure)))
                 (setf failure-stage ':workers)
                 (lisp-worker-manager-change-working-directory
                  manager configuration)
                 (setf workers-moved-p t
                       failure-stage ':process)
                 (uiop:chdir directory)
                 (setf process-moved-p t
                       *default-pathname-defaults* directory)
                 (setf failure-stage ':tools)
                 (application--load-extension-configuration configuration)
                 (setf configuration
                       (provider-bootstrap-configuration configuration))
                 (application--validate-provider-registration
                  previous-provider configuration)
                 (multiple-value-setq (provider registry agent)
                   (application--prepare-runtime-replacement
                    application configuration previous-conversation))
                 (setf failure-stage ':application
                       application-swapped-p t)
                 (setf (application-configuration application) configuration
                       (application-provider application) provider
                       (application-tool-registry application) registry
                       (application-agent application) agent)
                 (application-connect-task-presentation application)
                 (context-runtime-reset)
                 (setf committed-p t))
             (serious-condition (condition)
               (setf failure condition)))
        (unless committed-p
          (handler-case
              (application--extension-registry-restore
               extension-registry-snapshot)
            (serious-condition (condition)
              (push condition rollback-failures)))
          (when application-swapped-p
            (handler-case
                (application-disconnect-task-presentation application)
              (serious-condition (condition)
                (push condition rollback-failures)))
            (setf (application-configuration application)
                  previous-configuration
                  (application-conversation application)
                  previous-conversation
                  (application-provider application)
                  previous-provider
                  (application-tool-registry application)
                  previous-registry
                  (application-agent application)
                  previous-agent))
          (when process-moved-p
            (handler-case
                (progn
                  (uiop:chdir previous-process-directory)
                  (setf *default-pathname-defaults* previous-defaults))
              (serious-condition (condition)
                (push condition rollback-failures))))
          (when workers-moved-p
            (handler-case
                (lisp-worker-manager-change-working-directory
                 manager previous-configuration)
              (serious-condition (condition)
                (push condition rollback-failures))))
          (when registry
            (handler-case
                (tool-registry-close-runtime-state registry)
              (serious-condition (condition)
                (push condition rollback-failures))))
          (when retirement-started-p
            (setf rollback-failures
                  (nconc
                   rollback-failures
                   (application--restore-retired-tool-registry
                    application
                    previous-registry
                    quiesced-tools
                    :reconnect-p presentation-disconnected-p))))))
      (when failure
        (application--working-directory-failure
         :requested-path location
         :previous-directory previous-directory
         :stage failure-stage
         :cause failure
         :rollback-cause (nreverse rollback-failures)))
      directory)))

(-> application--conversation-input-history
    (conversation &key (:limit (integer 0)))
    list)
(defun application--conversation-input-history
    (conversation &key (limit *terminal-history-limit*))
  "Return bounded editable input history recovered from durable CONVERSATION."
  (let ((entries (make-deque :maximum-count limit)))
    (conversation-map-records
     conversation
     (lambda (record)
       (let ((properties (rest record)))
         (case (first record)
           (:message
            (let ((content (getf properties :content)))
              (when (and (eq (getf properties :role) ':user)
                         (non-empty-string-p content))
                (deque-push-back entries (copy-seq content)))))
           (:user-operation
            (let ((source (getf properties :source)))
              (when (and (member (getf properties :kind) '(:command :lisp))
                         (non-empty-string-p source))
                (deque-push-back entries (copy-seq source)))))))))
    (deque->list entries)))


(-> application--install-owned-conversation
    (application conversation &key (:conversation-lease conversation-lease))
    application)
(defun application--install-owned-conversation
    (application conversation &key conversation-lease)
  "Atomically install owned CONVERSATION with a fresh tool runtime and agent."
  (let* ((configuration
           (application--configuration-for-conversation
            (application-configuration application)
            conversation))
         (ui (application-ui application))
         (history-limit
           (if ui
               (line-editor-history-limit (terminal-ui-editor ui))
               *terminal-history-limit*))
         (input-history
           (application--conversation-input-history
            conversation :limit history-limit))
         (previous-configuration (application-configuration application))
         (previous-conversation (application-conversation application))
         (previous-conversation-lease
           (and (slot-boundp application 'conversation-lease)
                (application-conversation-lease application)))
         (previous-provider (application-provider application))
         (previous-registry (application-tool-registry application))
         (previous-agent (application-agent application))
         (previous-rendered-sequence
           (application-rendered-sequence application))
         (previous-render-position
           (application-render-position application))
         (previous-render-generation
           (application-render-generation application))
         (previous-transcript-synchronized-p
           (application-transcript-synchronized-p application))
         (previous-history-floor-sequence
           (application-history-floor-sequence application))
         (previous-goal (copy-list (application-goal application)))
         (provider nil)
         (registry nil)
         (agent nil)
         (application-swapped-p nil)
         (retirement-started-p nil)
         (presentation-disconnected-p nil)
         (quiesced-tools nil)
         (committed-p nil)
         (failure nil)
         (failure-stage ':retire)
         (rollback-failures nil))
    (multiple-value-setq (provider registry agent)
      (application--prepare-runtime-replacement
       application configuration conversation))
    (unwind-protect
         (handler-case
             (progn
               (setf retirement-started-p t
                     failure-stage ':retire)
               (application-disconnect-task-presentation application)
               (setf presentation-disconnected-p t)
               (multiple-value-bind (completed retirement-failure)
                   (tool-registry-quiesce-runtime-state previous-registry)
                 (setf quiesced-tools completed)
                 (when retirement-failure
                   (error retirement-failure)))
               (setf failure-stage ':install
                     application-swapped-p t)
               (setf (application-configuration application) configuration
                     (application-conversation application) conversation
                     (application-conversation-lease application)
                     conversation-lease
                     (application-provider application) provider
                     (application-tool-registry application) registry
                     (application-agent application) agent)
               (application-connect-task-presentation application)
               (setf (application-rendered-sequence application) 0
                     (application-render-position application) 0
                     (application-render-generation application)
                     (conversation-log-generation conversation)
                     (application-transcript-synchronized-p application) nil
                     (application-history-floor-sequence application) nil)
               (application--load-goal application)
               (context-runtime-reset)
               (application-publish-recovery-session application)
               (when ui
                 (terminal-ui-load-history ui input-history))
               (setf committed-p t))
           (serious-condition (condition)
             (setf failure condition)))
      (unless committed-p
        (when application-swapped-p
          (handler-case
              (application-disconnect-task-presentation application)
            (serious-condition (condition)
              (push condition rollback-failures)))
          (setf (application-configuration application)
                previous-configuration
                (application-conversation application)
                previous-conversation
                (application-conversation-lease application)
                previous-conversation-lease
                (application-provider application)
                previous-provider
                (application-tool-registry application)
                previous-registry
                (application-agent application)
                previous-agent
                (application-rendered-sequence application)
                previous-rendered-sequence
                (application-render-position application)
                previous-render-position
                (application-render-generation application)
                previous-render-generation
                (application-transcript-synchronized-p application)
                previous-transcript-synchronized-p
                (application-history-floor-sequence application)
                previous-history-floor-sequence
                (application-goal application)
                previous-goal))
        (when registry
          (handler-case
              (tool-registry-close-runtime-state registry)
            (serious-condition (condition)
              (push condition rollback-failures))))
        (when retirement-started-p
          (setf rollback-failures
                (nconc
                 rollback-failures
                 (application--restore-retired-tool-registry
                  application
                  previous-registry
                  quiesced-tools
                  :reconnect-p presentation-disconnected-p))))))
    (when failure
      (application--raise-runtime-replacement-error
       :operation ':conversation
       :stage failure-stage
       :cause failure
       :rollback-causes rollback-failures))
    (when (and previous-conversation-lease
               (not
                (eq previous-conversation-lease conversation-lease)))
      (conversation-lease-release previous-conversation-lease))
    application))

(-> application-install-conversation (application conversation) application)
(defun application-install-conversation (application conversation)
  "Claim and atomically install CONVERSATION with a fresh runtime and agent."
  (multiple-value-bind (lease acquired-p)
      (application--conversation-lease-select
       application
       (application-configuration application)
       (conversation-identifier conversation))
    (let ((completed-p nil))
      (unwind-protect
           (prog1
               (application--install-owned-conversation
                application conversation :conversation-lease lease)
             (setf completed-p t))
        (unless completed-p
          (when acquired-p
            (conversation-lease-release lease)))))))

(-> application-resume-conversation (application string) application)
(defun application-resume-conversation (application identifier)
  "Claim IDENTIFIER before loading and atomically install its conversation."
  (multiple-value-bind (conversation lease acquired-p)
      (application--conversation-load-owned
       application
       (application-configuration application)
       identifier)
    (let ((completed-p nil))
      (unwind-protect
           (prog1
               (application--install-owned-conversation
                application conversation :conversation-lease lease)
             (setf completed-p t))
        (unless completed-p
          (when acquired-p
            (conversation-lease-release lease)))))))


;;;; -- Transcript Projection --

(-> application--transcript-width (application) integer)
(defun application--transcript-width (application)
  "Return APPLICATION's terminal width available to wrapped transcript bodies."
  (max 20
       (- (terminal-columns (terminal-ui-terminal (application-ui application)))
          3)))

(-> application--indented-body (application string) string)
(defun application--indented-body (application text)
  "Return sanitized TEXT wrapped and indented under its transcript header."
  (format nil "~{  ~A~^~%~}"
          (wrap-text (string-right-trim
                      '(#\Space #\Tab #\Newline #\Return)
                      (sanitize-text text))
                     (application--transcript-width application))))

(-> application--markdown-renderer (application) termdown:markdown-renderer)
(defun application--markdown-renderer (application)
  "Return a markdown renderer sized to APPLICATION's current terminal width."
  (termdown:markdown-renderer-create
   :width (max 24
               (1- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))))))

(-> application--markdown-body (application string) list)
(defun application--markdown-body (application text)
  "Return sanitized TEXT rendered as markdown transcript spans."
  (let ((renderer (application--markdown-renderer application))
        (trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                    (sanitize-text text))))
    (loop for line in (or (uiop:split-string trimmed :separator '(#\Newline))
                          (list ""))
          append (loop for row in (termdown:markdown-render-line renderer line)
                       append (append row
                                      (list (terminal-span
                                             ':plain
                                             (string #\Newline))))))))

(-> application--timestamp-text
    ((option timestamp))
    (option string))
(defun application--timestamp-text (timestamp)
  "Return TIMESTAMP as a local minute-resolution label."
  (when (typep timestamp 'timestamp)
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time timestamp)
      (declare (ignore second))
      (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
              year month date hour minute))))

(-> application--turn-timestamp-text
    (application (option timestamp))
    (option string))
(defun application--turn-timestamp-text (application timestamp)
  "Return TIMESTAMP as a turn-header label when that display is enabled."
  (and (application-turn-timestamps-p application)
       (application--timestamp-text timestamp)))

(-> application--child-response-entry (application list) (option list))
(defun application--child-response-entry (application payload)
  "Return one canonical primary-window entry for a portable child response."
  (let* ((child-name (getf payload :child-name))
         (text (getf payload :text))
         (timestamp (getf payload :time))
         (safe-child-name
           (and (stringp child-name)
                (sanitize-text child-name :single-line-p t)))
         (timestamp-text (application--timestamp-text timestamp)))
    (when (and (non-empty-string-p safe-child-name)
               (stringp text)
               (plusp
                (length
                 (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
               timestamp-text)
      (append
       (list (terminal-span ':brand "● autolith [")
             (terminal-span ':child-name safe-child-name)
             (terminal-span ':brand "] ")
             (terminal-span ':dim timestamp-text)
             (terminal-span ':plain (string #\Newline)))
       (application--markdown-body application text)))))

(-> application--transcript-entry
    (application &key (:style terminal-style) (:header string)
                 (:detail (option string)) (:body (option string))
                 (:body-style terminal-style) (:timestamp (option timestamp)))
    list)
(defun application--transcript-entry
    (application &key (style ':plain) (header "") detail body
                 (body-style ':plain) timestamp)
  "Return one styled transcript entry with HEADER, optional DETAIL, and BODY rows."
  (let ((spans (list (terminal-span style header)))
        (detail (or detail
                    (application--turn-timestamp-text application timestamp))))
    (when detail
      (let ((available (- (terminal-columns
                           (terminal-ui-terminal (application-ui application)))
                          3
                          (text-cell-width header)))
            (safe-detail (sanitize-text detail :single-line-p t)))
        (when (> available 1)
          (let ((visible (text-cell-prefix safe-detail (1- available))))
            (setf spans
                  (append spans
                          (list (terminal-span
                                 :dim
                                 (format nil "  ~A~:[~;…~]"
                                         visible
                                         (< (length visible)
                                            (length safe-detail)))))))))))
    (when body
      (setf spans
            (append spans
                    (list (terminal-span ':plain (string #\Newline))
                          (terminal-span body-style
                                         (application--indented-body
                                          application
                                          body))))))
    spans))

;;;; -- Tool Transcript Protocol --

(-> application-tool-call-entry ((option tool) application json-object) list)
(defgeneric application-tool-call-entry (tool application call)
  (:documentation "Return the styled transcript entry for one function CALL."))

(-> application-tool-result-entry ((option tool) application list) list)
(defgeneric application-tool-result-entry (tool application record)
  (:documentation "Return the styled transcript entry for one tool result RECORD."))

(-> application--find-tool (application string) (option tool))
(defun application--find-tool (application canonical-name)
  "Return APPLICATION's registered tool named CANONICAL-NAME, when available."
  (let ((separator (position #\. canonical-name))
        (registry (and (slot-boundp application 'tool-registry)
                       (application-tool-registry application))))
    (when (and separator (typep registry 'tool-registry))
      (tool-registry-find registry
                          (subseq canonical-name 0 separator)
                          (subseq canonical-name (1+ separator))))))

(defmethod tool-compact-result-visible-p ((tool shell-run-tool))
  "Keep successful external commands visible in compact presentation."
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-eval-tool))
  "Keep successful Lisp evaluation visible in compact presentation."
  t)

(defmethod tool-compact-result-visible-p ((tool self-eval-tool))
  "Keep successful active-image evaluation visible in compact presentation."
  t)

(-> application--compact-tool-result-visible-p
    (application (option string))
    boolean)
(defun application--compact-tool-result-visible-p
    (application canonical-name)
  "Return true when CANONICAL-NAME remains visible in compact presentation."
  (let ((tool (and (stringp canonical-name)
                   (application--find-tool application canonical-name))))
    (and tool
         (tool-compact-result-visible-p tool))))


(-> application--field-spans (string string) list)
(defun application--field-spans (label value)
  "Return one aligned dim LABEL and plain VALUE line as transcript spans."
  (list (terminal-span :dim (format nil "  ~13A " label))
        (terminal-span :plain (format nil "~A~%" value))))

(-> web-search-call-detail (json-object) string)
(defun web-search-call-detail (item)
  "Return a short human-readable description of a web search call ITEM."
  (let* ((action (json-get item "action"))
         (action-type (if (json-object-p action)
                          (or (json-get action "type") "")
                          "")))
    (cond
      ((string= action-type "search")
       (let ((query (json-get action "query"))
             (queries (json-get action "queries")))
         (cond
           ((non-empty-string-p query)
            query)
           ((and (vectorp queries)
                 (plusp (length queries))
                 (stringp (aref queries 0)))
            (format nil "~A~:[~;, more~]"
                    (aref queries 0)
                    (> (length queries) 1)))
           (t
            ""))))
      ((string= action-type "open_page")
       (or (json-get action "url") ""))
      ((string= action-type "find_in_page")
       (format nil "~@[~A in ~]~A"
               (json-get action "pattern")
               (or (json-get action "url") "")))
      (t
       ""))))

(defparameter *application-reasoning-preview-row-limit* 5
  "The maximum rows shown for one unfinished reasoning summary.")

(-> application--reasoning-summary-rows (application string) list)
(defun application--reasoning-summary-rows (application summary)
  "Return styled rows for one provider-visible reasoning SUMMARY."
  (let* ((safe-summary
           (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                              (sanitize-text summary)))
         (body-width
           (max 1
                (- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))
                   5))))
    (cons
     (list (terminal-span ':hint "◇ reasoning summary"))
     (loop for row in (termdown:markdown-render-inline safe-summary body-width)
           collect (append (list (terminal-span ':dim "  │ ")) row)))))

(-> application--reasoning-preview-rows (application string) list)
(defun application--reasoning-preview-rows (application summary)
  "Return a tail-biased bounded live preview of reasoning SUMMARY."
  (let ((rows (application--reasoning-summary-rows application summary)))
    (if (<= (length rows) *application-reasoning-preview-row-limit*)
        rows
        (append (list (first rows)
                      (list (terminal-span ':dim "  │ …")))
                (last rows (- *application-reasoning-preview-row-limit* 2))))))

(-> application--reasoning-summary-entry (application string) list)
(defun application--reasoning-summary-entry (application summary)
  "Return one railed transcript entry for provider-visible reasoning SUMMARY."
  (loop for row in (application--reasoning-summary-rows application summary)
        for first-row-p = t then nil
        append (if first-row-p
                   row
                   (cons (terminal-span ':plain (string #\Newline)) row))))

(-> response-item-entry
    (application json-object &key (:timestamp (option timestamp)))
    (option list))
(defun response-item-entry (application item &key timestamp)
  "Return a styled transcript entry for completed provider ITEM."
  (let ((type (json-get item "type")))
    (cond
      ((and (string= (or type "") "message")
            (string= (or (json-get item "role") "") "assistant"))
       (let ((text (response-item-assistant-text item)))
         (when text
           (append (application--transcript-entry
                    application
                    :style ':brand
                    :header "● autolith"
                    :timestamp timestamp)
                   (list (terminal-span ':plain (string #\Newline)))
                   (application--markdown-body application text)))))
      ((string= (or type "") "reasoning")
       (when (application-reasoning-traces-p application)
         (let ((summary (response-item-reasoning-summary item)))
           (when summary
             (application--reasoning-summary-entry application summary)))))
      ((string= (or type "") "function_call")
       (let* ((canonical-name (function-call-canonical-name item))
              (tool (application--find-tool application canonical-name)))
         (application-tool-call-entry tool application item)))
      ((string= (or type "") "web_search_call")
       (application--transcript-entry
        application
        :style ':tool
        :header "▸ web search"
        :detail (let ((detail (web-search-call-detail item)))
                  (and (non-empty-string-p detail)
                       detail))))
      (t
       nil))))

(-> conversation-record-entry (application list) (option list))
(defun conversation-record-entry (application record)
  "Return the styled transcript entry represented by durable RECORD."
  (case (first record)
    (:message
     (when (eq (getf (rest record) :role) :user)
       (let ((content (getf (rest record) :content)))
         (if (application--goal-continuation-message-p content)
             (list (terminal-span ':hint "∙ goal continues"))
             (application--transcript-entry application
                                            :style ':user
                                            :header "❯ you"
                                            :timestamp (getf (rest record) :time)
                                            :body content)))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (response-item-entry application
                                 (json-decode wire-json)
                                 :timestamp (getf (rest record) :time)))))
    (:tool-result
     (let* ((canonical-name (getf (rest record) :tool))
            (tool (application--find-tool application canonical-name)))
       (when (or (not (application-compact-view-p application))
                 (not (eq (getf (rest record) :status) ':ok))
                 (application--compact-tool-result-visible-p
                  application canonical-name))
         (application-tool-result-entry tool application record))))
    (:summary
     (list (terminal-span
            ':hint
            (format nil "∙ context compacted through sequence ~A"
                    (getf (rest record) :through-seq)))))
    (otherwise
     nil)))

(-> application--command-presentation-entry
    (application-command-invocation (or string list))
    terminal-styled-text)
(defun application--command-presentation-entry (invocation entry)
  "Return ENTRY beneath a visible heading for its originating INVOCATION."
  (append
   (list (terminal-span ':notice "› ")
         (terminal-span
          ':code
          (string-trim
           *application-command-whitespace*
           (application-command-invocation-input invocation)))
         (terminal-span ':plain (string #\Newline))
         (terminal-span ':notice "└ "))
   (if (stringp entry)
       (list (terminal-span ':plain entry))
       entry)))

(-> application-present (application (or string list)) boolean)
(defun application-present (application entry)
  "Append non-conversation ENTRY once to APPLICATION's normal scrollback."
  (let* ((ui (application-ui application))
         (invocation
           (and *application-command-presentation-pending-p*
                (typep *application-command-presentation-invocation*
                       'application-command-invocation)
                *application-command-presentation-invocation*))
         (presented-entry
           (if invocation
               (application--command-presentation-entry invocation entry)
               entry)))
    (with-terminal-ui-locked (ui)
      (let* ((identifier
               (incf (application-presentation-counter application)))
             (emitted-p
               (terminal-ui-append-finalized
                ui
                (list :presentation identifier)
                presented-entry)))
        (when (and invocation emitted-p)
          (setf *application-command-presentation-pending-p* nil))
        emitted-p))))

(-> application--assistant-message-record-text (list) (option string))
(defun application--assistant-message-record-text (record)
  "Return the visible assistant text carried by durable RECORD, when present."
  (when (eq (first record) :provider-item)
    (let ((wire-json (getf (rest record) :wire-json)))
      (when (stringp wire-json)
        (let ((item (json-decode wire-json)))
          (and (json-object-p item)
               (response-item-assistant-text item)))))))

(-> application--reasoning-record-text (list) (option string))
(defun application--reasoning-record-text (record)
  "Return the visible reasoning summary carried by durable RECORD, when present."
  (when (eq (first record) :provider-item)
    (let ((wire-json (getf (rest record) :wire-json)))
      (when (stringp wire-json)
        (let ((item (json-decode wire-json)))
          (and (json-object-p item)
               (response-item-reasoning-summary item)))))))

(defparameter *application-history-page-size* 250
  "The maximum transcript candidates replayed automatically or per history page.")

(-> application--history-page-size () (integer 1))
(defun application--history-page-size ()
  "Return the validated positive transcript history page size."
  (unless (typep *application-history-page-size* '(integer 1))
    (error 'configuration-error
           :message "The transcript history page size must be a positive integer."))
  *application-history-page-size*)

(-> application--record-sequence (list) (option integer))
(defun application--record-sequence (record)
  "Return RECORD's durable sequence when it is a nonnegative integer."
  (let ((sequence (getf (rest record) :seq)))
    (and (typep sequence '(integer 0)) sequence)))

(-> application--response-item-visible-p
    (application json-object)
    boolean)
(defun application--response-item-visible-p (application item)
  "Return true when completed provider ITEM has a visible transcript entry."
  (let ((type (json-get item "type")))
    (cond
      ((and (string= (or type "") "message")
            (string= (or (json-get item "role") "") "assistant"))
       (not (null (response-item-assistant-text item))))
      ((string= (or type "") "reasoning")
       (and (application-reasoning-traces-p application)
            (not (null (response-item-reasoning-summary item)))))
      ((and (stringp type)
            (member type '("function_call" "web_search_call")
                    :test #'string=))
       t)
      (t
       nil))))

(-> application--record-visible-p (application list) boolean)
(defun application--record-visible-p (application record)
  "Return true when RECORD produces a visible transcript entry."
  (case (first record)
    (:message
     (eq (getf (rest record) :role) ':user))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (let ((item (json-decode wire-json)))
              (and (json-object-p item)
                   (application--response-item-visible-p
                    application item))))))
    (:tool-result
     (let ((canonical-name (getf (rest record) :tool)))
       (or (not (application-compact-view-p application))
           (not (eq (getf (rest record) :status) ':ok))
           (application--compact-tool-result-visible-p
            application canonical-name))))
    (:summary
     t)
    (otherwise
     nil)))


(-> application--present-conversation-record
    (application list
     &key (:suppress-assistant-p boolean)
          (:suppress-reasoning-p boolean))
    boolean)
(defun application--present-conversation-record
    (application record
     &key (suppress-assistant-p nil) (suppress-reasoning-p nil))
  "Present one sequenced RECORD unless its streamed form is already visible."
  (let ((sequence (application--record-sequence record)))
    (when sequence
      (let* ((ui (application-ui application))
             (identifier
               (list :conversation
                     (conversation-identifier
                      (application-conversation application))
                     sequence))
             (assistant-text
               (application--assistant-message-record-text record))
             (reasoning-text
               (application--reasoning-record-text record)))
        (if (or (and suppress-assistant-p assistant-text)
                (and suppress-reasoning-p reasoning-text))
            (terminal-ui-mark-finalized ui identifier)
            (let ((entry (conversation-record-entry application record)))
              (and entry
                   (terminal-ui-append-finalized ui identifier entry))))))))

(-> application--present-conversation-records
    (application list)
    (integer 0))
(defun application--present-conversation-records (application records)
  "Present ordered RECORDS with one terminal-region update."
  (let ((conversation-id
          (conversation-identifier
           (application-conversation application))))
    (terminal-ui-append-finalized-batch
     (application-ui application)
     (loop for record in records
           for sequence = (application--record-sequence record)
           for entry = (and sequence
                            (conversation-record-entry application record))
           when entry
             collect (list (list :conversation
                                 conversation-id
                                 sequence)
                           entry)))))

(-> application--render-position-invalidate (application) null)
(defun application--render-position-invalidate (application)
  "Invalidate APPLICATION's append-log position after active-segment replacement."
  (setf (application-render-position application) 0
        (application-render-generation application)
        (conversation-log-generation
         (application-conversation application))
        (application-transcript-synchronized-p application) nil)
  nil)

(-> application--render-position-reset-if-needed (application) null)
(defun application--render-position-reset-if-needed (application)
  "Reset APPLICATION's active-segment cursor after chunk rotation."
  (let ((generation
          (conversation-log-generation
           (application-conversation application))))
    (unless (= generation (application-render-generation application))
      (application--render-position-invalidate application)))
  nil)

(-> application--finish-render-scan
    (application integer integer)
    boolean)
(defun application--finish-render-scan (application position generation)
  "Commit POSITION only when its scanned log GENERATION is still current."
  (let ((conversation (application-conversation application)))
    (if (= generation (conversation-log-generation conversation))
        (progn
          (setf (application-render-position application) position
                (application-render-generation application) generation
                (application-transcript-synchronized-p application) t)
          (if (= generation (conversation-log-generation conversation))
              t
              (progn
                (application--render-position-invalidate application)
                nil)))
        (progn
          (application--render-position-invalidate application)
          nil))))

(-> application--history-segment-records
    (application pathname (integer 0) (integer 1))
    (values list (integer 0) (integer 0) (integer 0)))
(defun application--history-segment-records
    (application pathname after-sequence limit)
  "Return one validated segment's newest visible records and scan metadata.

The values are the newest LIMIT visible records after AFTER-SEQUENCE, the
unbounded visible candidate count, the newest durable sequence, and the next
readable byte position."
  (let ((records (make-deque :maximum-count limit))
        (candidate-count 0)
        (latest-sequence 0)
        (conversation (application-conversation application)))
    (multiple-value-bind
        (position incomplete-tail-p record-count start-sequence next-sequence)
        (conversation--map-segment-records
         (conversation-pathname conversation)
         pathname
         (lambda (record)
           (let ((sequence (application--record-sequence record)))
             (when sequence
               (setf latest-sequence (max latest-sequence sequence))
               (when (and (> sequence after-sequence)
                          (application--record-visible-p application record))
                  (deque-push-back records record)
                  (incf candidate-count))))))
      (declare (ignore incomplete-tail-p record-count
                       start-sequence next-sequence))
      (values (coerce (deque->vector records) 'list)
              candidate-count
              latest-sequence
              position))))

(-> application--render-startup-history
    (application (integer 0))
    null)
(defun application--render-startup-history (application after-sequence)
  "Replay a bounded newest page after durable sequence AFTER-SEQUENCE.

A normal resume scans the active chunk first. When that chunk contains fewer
visible entries than the configured page size, exactly one preceding segment
may fill the remainder."
  (let* ((conversation (application-conversation application))
         (identity (conversation-pathname conversation))
         (limit (application--history-page-size)))
    (loop
      (let* ((generation (conversation-log-generation conversation))
             (pathnames (conversation-storage-pathnames identity))
             (active (first (last pathnames)))
             (previous (and (rest pathnames)
                            (first (last (butlast pathnames))))))
        (if (null active)
            (if (conversation-persisted-p conversation)
                (error 'conversation-invariant-error
                       :message "The active conversation segment is missing."
                       :pathname identity
                       :sequence nil)
                (progn
                  (setf (application-history-floor-sequence application) nil
                        (application-rendered-sequence application) 0)
                  (when (application--finish-render-scan
                         application 0 generation)
                    (return))))
            (multiple-value-bind
                (active-records active-count latest-sequence position)
                (application--history-segment-records
                 application active after-sequence limit)
              (if (/= generation (conversation-log-generation conversation))
                  (application--render-position-invalidate application)
                  (let ((records active-records)
                        (older-history-p (> active-count limit)))
                    (when (and (not (application-recovery-startup-p application))
                               (zerop after-sequence)
                               (< active-count limit)
                               previous)
                      (let ((remaining (- limit active-count)))
                        (multiple-value-bind
                            (previous-records previous-count
                             previous-latest-sequence previous-position)
                            (application--history-segment-records
                             application previous 0 remaining)
                          (declare (ignore previous-latest-sequence
                                           previous-position))
                          (setf records (append previous-records active-records)
                                older-history-p
                                (> previous-count remaining)))))
                    (when (and records older-history-p)
                      (application-present
                       application
                       (list
                        (terminal-span
                         :hint
                         (format nil
                                 "∙ showing ~D newest ~A; /history loads an earlier page"
                                 (length records)
                                 (if (plusp after-sequence)
                                     "entries added after recovery"
                                     (if (= (length records) 1)
                                         "transcript entry"
                                         "transcript entries")))))))
                    (application--present-conversation-records application records)
                    (setf (application-history-floor-sequence application)
                          (cond
                            ((or (zerop after-sequence)
                                 older-history-p
                                 (null
                                  (application-history-floor-sequence application)))
                             (or
                              (and records
                                   (application--record-sequence (first records)))
                              (1+ latest-sequence)))
                            (t
                             (application-history-floor-sequence application)))
                          (application-rendered-sequence application)
                          latest-sequence)
                    (when (application--finish-render-scan
                           application position generation)
                      (return)))))))))
  nil)

(-> application--render-unread-records (application) null)
(defun application--render-unread-records (application)
  "Stream and present only records after APPLICATION's durable render cursor."
  (let ((conversation (application-conversation application)))
    (loop
      (application--render-position-reset-if-needed application)
      (let ((generation (application-render-generation application))
            (records nil))
        (multiple-value-bind (position incomplete-tail-p record-count)
            (conversation--map-records
             (conversation-log-pathname conversation)
             (lambda (record)
               (let ((sequence (application--record-sequence record)))
                 (when (and sequence
                            (> sequence
                               (application-rendered-sequence application)))
                   (push record records))))
             :start-position (application-render-position application))
          (declare (ignore incomplete-tail-p record-count))
          (if (/= generation (conversation-log-generation conversation))
              (application--render-position-invalidate application)
              (progn
                (dolist (record (nreverse records))
                  (application--present-conversation-record application record)
                  (setf (application-rendered-sequence application)
                        (application--record-sequence record)))
                (when (application--finish-render-scan
                       application position generation)
                  (return))))))))
  nil)

(-> application--render-streamed-records
    (application (option string) (option string))
    null)
(defun application--render-streamed-records
    (application streamed-assistant-text streamed-reasoning-text)
  "Reconcile and present APPLICATION's unread records after streamed output."
  (let ((conversation (application-conversation application)))
    (loop
      (application--render-position-reset-if-needed application)
      (let ((generation (application-render-generation application))
            (records nil))
        (multiple-value-bind (position incomplete-tail-p record-count)
            (conversation--map-records
             (conversation-log-pathname conversation)
             (lambda (record)
               (let ((sequence (application--record-sequence record)))
                 (when (and sequence
                            (> sequence
                               (application-rendered-sequence application)))
                   (push record records))))
             :start-position (application-render-position application))
          (declare (ignore incomplete-tail-p record-count))
          (if (/= generation (conversation-log-generation conversation))
              (application--render-position-invalidate application)
              (let* ((records (nreverse records))
                     (assistant-texts
                       (loop for record in records
                             for text =
                               (application--assistant-message-record-text
                                record)
                             when text
                               collect text))
                     (reasoning-texts
                       (loop for record in records
                             for text =
                               (application--reasoning-record-text record)
                             when text
                               collect text))
                     (assistant-stream-match-p
                       (and streamed-assistant-text
                            assistant-texts
                            (string=
                             streamed-assistant-text
                             (format nil "~{~A~^~%~}" assistant-texts))))
                     (reasoning-stream-match-p
                       (and streamed-reasoning-text
                            reasoning-texts
                            (string=
                             streamed-reasoning-text
                             (format nil "~{~A~^~2%~}" reasoning-texts)))))
                (dolist (record records)
                  (application--present-conversation-record
                   application
                   record
                   :suppress-assistant-p assistant-stream-match-p
                   :suppress-reasoning-p reasoning-stream-match-p)
                  (setf (application-rendered-sequence application)
                        (application--record-sequence record)))
                (when (application--finish-render-scan
                       application position generation)
                  (return))))))))
  nil)

(-> application-render-records
    (application &key (:streamed-assistant-text (option string))
                      (:streamed-reasoning-text (option string)))
    null)
(defun application-render-records
    (application &key streamed-assistant-text streamed-reasoning-text)
  "Append APPLICATION's not-yet-rendered durable transcript records once.

Assistant and reasoning records are suppressed only when their joined durable
text exactly matches the corresponding streamed text. Suppressed identifiers
remain finalized so later conversation replay cannot duplicate streamed rows."
  (with-recursive-lock-held ((application-render-lock application))
    (cond
      ((and (not (application-transcript-synchronized-p application))
            (null streamed-assistant-text)
            (null streamed-reasoning-text))
       (application--render-startup-history
        application
        (application-rendered-sequence application)))
      ((and (application-transcript-synchronized-p application)
            (= (application-render-generation application)
               (conversation-log-generation
                (application-conversation application)))
            (>= (application-rendered-sequence application)
                (1- (conversation-next-sequence
                     (application-conversation application)))))
       nil)
      ((or streamed-assistant-text streamed-reasoning-text)
       (application--render-streamed-records
        application streamed-assistant-text streamed-reasoning-text))
      (t
       (application--render-unread-records application))))
  (application-publish-recovery-session application)
  nil)

(-> application--history-page-records
    (application integer integer)
    (values list integer))
(defun application--history-page-records (application floor limit)
  "Return the newest LIMIT replay candidates before sequence FLOOR."
  (let ((records (make-deque :maximum-count limit))
        (candidate-count 0))
    (conversation-map-records
     (application-conversation application)
     (lambda (record)
       (let ((sequence (application--record-sequence record)))
         (when (and sequence
                    (< sequence floor)
                    (application--record-visible-p application record))
            (deque-push-back records record)
            (incf candidate-count)))))
    (values (coerce (deque->vector records) 'list)
            candidate-count)))

(-> application--history-floor-ensure
    (application (integer 1))
    integer)
(defun application--history-floor-ensure (application limit)
  "Return a paging floor, deriving the newest visible page when necessary."
  (or (application-history-floor-sequence application)
      (multiple-value-bind (records candidate-count)
          (application--history-page-records
           application
           (1+ (application-rendered-sequence application))
           limit)
        (declare (ignore candidate-count))
        (setf (application-history-floor-sequence application)
              (or (and records
                       (application--record-sequence (first records)))
                  (1+ (application-rendered-sequence application)))))))

(-> application-reset-history-pagination (application) null)
(defun application-reset-history-pagination (application)
  "Restart explicit history paging above the newest durable record."
  (with-recursive-lock-held ((application-render-lock application))
    (setf (application-history-floor-sequence application)
          (1+ (application-rendered-sequence application))))
  nil)

(-> application-render-history (application) null)
(defun application-render-history (application)
  "Append one bounded page of explicitly requested older transcript history."
  (with-recursive-lock-held ((application-render-lock application))
    (let* ((limit (application--history-page-size))
           (floor (application--history-floor-ensure application limit)))
      (multiple-value-bind (records candidate-count)
          (application--history-page-records
           application floor limit)
        (if records
            (let ((presented-count
                    (application--present-conversation-records
                     application records)))
              (setf (application-history-floor-sequence application)
                    (application--record-sequence (first records)))
              (when (plusp presented-count)
                (application-present
                 application
                 (list
                  (terminal-span
                   :hint
                   (format nil
                           "∙ loaded ~D earlier transcript entr~:@P"
                           presented-count)))))
              (when (> candidate-count limit)
                (application-present
                 application
                 (list
                  (terminal-span
                   :hint
                   "∙ more earlier history remains; run /history again"))))
              (when (and (zerop presented-count)
                         (<= candidate-count limit))
                (application-present
                 application
                 "No earlier transcript history remains.")))
            (application-present application
                                 "No earlier transcript history remains.")))))
  (application-publish-recovery-session application)
  nil)


;;;; -- Session Goal --

(defparameter *application-goal-continuation-limit* 8
  "The automatic goal continuation turns allowed per user message.")

(defparameter *application-goal-continuation-prompt*
  "Continue working toward the session goal."
  "The synthetic user message driving one goal continuation turn.")

(defparameter *application-goal-complete-marker* "[GOAL-COMPLETE]"
  "The literal marker the model includes once the goal is met.")

(-> application--record-goal (application) null)
(defun application--record-goal (application)
  "Append APPLICATION's current goal state to the durable conversation."
  (let ((goal (application-goal application)))
    (conversation-append-record
     (application-conversation application)
     (if goal
         (list :goal
               :objective (getf goal :objective)
               :status (getf goal :status)
               :continuations (getf goal :continuations)
               :created-at (getf goal :created-at))
         (list :goal
               :objective nil
               :status ':cleared
               :continuations 0
               :created-at nil))))
  nil)

(-> application--load-goal (application) null)
(defun application--load-goal (application)
  "Restore APPLICATION's goal from the newest durable goal record."
  (let* ((record
           (conversation-latest-goal-record
            (application-conversation application)))
         (objective (and record (getf (rest record) :objective)))
         (status (and record (getf (rest record) :status))))
    (setf (application-goal application)
          (and (non-empty-string-p objective)
               (member status '(:active :paused :complete))
               (list :objective objective
                     :status status
                     :continuations
                     (let ((count (getf (rest record) :continuations)))
                       (if (integerp count) count 0))
                     :created-at
                     (let ((time (getf (rest record) :created-at)))
                       (if (integerp time)
                           time
                           (getf (rest record) :time)))))))
  nil)

(-> application-goal-context (application) (option string))
(defun application-goal-context (application)
  "Return the transient goal instructions for the next provider request."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (format nil
              "<goal_context>~%The session goal: ~A~%Work autonomously ~
               toward this goal every turn. When the goal is genuinely ~
               complete, include the literal marker ~A in your final ~
               message. If you cannot continue without the user, state ~
               plainly what you need and stop.~%</goal_context>"
              (getf goal :objective)
              *application-goal-complete-marker*))))

(-> application--goal-continuation-message-p (string) boolean)
(defun application--goal-continuation-message-p (content)
  "Return true when CONTENT is the synthetic goal continuation prompt."
  (string= content *application-goal-continuation-prompt*))


;;;; -- Agent Presentation --

(defparameter *application-thinking-words*
  '("pondering" "exploring" "untangling" "crafting" "verifying" "connecting")
  "One-word activity labels sampled while the model prepares its next action.")

(-> application--git-branch (pathname) (option string))
(defun application--git-branch (directory)
  "Return the enclosing Git worktree branch for DIRECTORY, when attached."
  (handler-case
      (let ((branch
              (string-trim
               '(#\Space #\Tab #\Newline #\Return)
               (uiop:run-program
                (list "git" "-C" (namestring directory)
                      "symbolic-ref" "--quiet" "--short" "HEAD")
                :output ':string
                :error-output ':string
                :ignore-error-status t))))
        (and (non-empty-string-p branch) branch))
    (error ()
      nil)))

(-> application--status-details (application) terminal-styled-text)
(defun application--status-details (application)
  "Return cached-phase model, effort, and repository status spans."
  (when (slot-boundp application 'configuration)
    (let* ((configuration (application-configuration application))
           (branch
             (application--git-branch
              (configuration-working-directory configuration))))
      (append
       (list (terminal-span ':status-dim "  ")
             (terminal-span ':status-model
                            (configuration-model configuration))
             (terminal-span ':status-dim " · ")
             (terminal-span ':status-effort
                            (configuration-reasoning-effort configuration)))
       (when branch
         (list (terminal-span ':status-dim " · git ")
               (terminal-span ':status-branch branch)))))))

(-> application--worked-seconds (application) (option (integer 0)))
(defun application--worked-seconds (application)
  "Return the active conversation's accumulated working seconds, when bound."
  (and (slot-boundp application 'conversation)
       (conversation-working-seconds
        (application-conversation application))))

(-> application-set-activity (application (option string)) terminal-ui)
(defun application-set-activity (application status)
  "Set APPLICATION's live STATUS and snapshot its contextual details once."
  (terminal-ui-set-status
   (application-ui application)
   status
   :details (and status (application--status-details application))
   :worked-seconds (and status (application--worked-seconds application))))

(-> application-set-local-activity
    (application (option string))
    terminal-ui)
(defun application-set-local-activity (application activity)
  "Set APPLICATION's explicit local Lisp ACTIVITY without replacing agent status."
  (terminal-ui-set-local-activity (application-ui application) activity))

(-> application-thinking-label () string)
(defun application-thinking-label ()
  "Return one interesting activity word for the next provider step."
  (if *application-thinking-words*
      (nth (random (length *application-thinking-words*))
           *application-thinking-words*)
      "pondering"))

(-> application--present-transient-tool-result (application list) boolean)
(defun application--present-transient-tool-result (application details)
  "Present one tool result carried only through observer DETAILS."
  (let* ((tool-name (getf details :tool))
         (tool (and (stringp tool-name)
                    (application--find-tool application tool-name)))
         (record
           (list :tool-result
                 :tool tool-name
                 :status (if (getf details :success-p) ':ok ':error)
                 :output (or (getf details :output) ""))))
    (and tool
         (application-present
          application
          (application-tool-result-entry tool application record)))))

(-> application--present-skill-load-result (application list) boolean)
(defun application--present-skill-load-result (application details)
  "Present a finalized transcript marker for one request-local Skill selection."
  (let* ((result-details (getf details :details))
         (name
           (and (listp result-details)
                (eq (getf result-details :kind) ':skill-load)
                (getf result-details :name))))
    (and (non-empty-string-p name)
         (application-present
          application
          (list
           (terminal-span
            ':notice
            (if (getf result-details :newly-selected-p)
                "◆ loaded skill: "
                "◆ skill already loaded: "))
           (terminal-span
            ':strong
            (sanitize-text name :single-line-p t)))))))

(-> application-agent-observer
    (application
     &key (:steering-function (option function))
          (:steering-persisted-function (option function))
          (:user-message-persisted-function (option function))
          (:pending-operations-function (option function))
          (:user-message-input (option (or string user-message-input)))
          (:continuation-p boolean))
    agent-observer)
(defun application-agent-observer
    (application
     &key steering-function steering-persisted-function
          user-message-persisted-function pending-operations-function
          user-message-input (continuation-p nil))
  "Return a terminal observer streaming one APPLICATION turn as stable lines."
  (let ((ui (application-ui application))
        (activity-label (application-thinking-label))
        (reasoning-text (text-buffer-create))
        (presented-reasoning-text nil)
        (stream-text (text-buffer-create))
        (stream-pending "")
        (stream-open-p nil)
        (stream-started-at nil)
        (stream-renderer nil))
    (labels ((reasoning-flush ()
               "Finalize the visible reasoning summary before assistant output."
               (terminal-ui-set-preview-rows ui nil)
               (when (and (application-reasoning-traces-p application)
                          (plusp (length reasoning-text))
                          (null presented-reasoning-text))
                 (let ((summary (text-buffer-string reasoning-text)))
                   (application-present
                    application
                    (application--reasoning-summary-entry application summary))
                   (setf presented-reasoning-text summary))))

             (stream-text-delta (delta)
               "Commit DELTA's completed markdown rows and repaint the fluid tail."
               (when (plusp (length delta))
                 (terminal-ui-note-status-progress ui)
                 (text-buffer-append stream-text delta)
                 (setf stream-pending
                       (sanitize-text
                        (concatenate 'string stream-pending delta)))
                 (let ((rows nil))
                   (unless stream-open-p
                     (reasoning-flush)
                     (setf stream-open-p t
                           stream-started-at (get-universal-time)
                           stream-renderer (application--markdown-renderer
                                            application))
                     (application-set-activity application "receiving response")
                     (push
                      (application--transcript-entry
                       application
                       :style ':brand
                       :header "● autolith"
                       :timestamp stream-started-at)
                      rows))
                   (loop for newline = (position #\Newline stream-pending)
                         while newline
                         do (setf rows
                                  (append rows
                                          (termdown:markdown-render-line
                                           stream-renderer
                                           (subseq stream-pending 0 newline)))
                                  stream-pending
                                  (subseq stream-pending (1+ newline))))
                   (multiple-value-bind (overflow-rows tail-rows retained)
                       (termdown:markdown-render-partial stream-renderer stream-pending)
                     (setf stream-pending retained)
                     (terminal-ui-stream-update
                      ui
                      :rows (append rows overflow-rows)
                      :tail tail-rows)))))

             (stream-flush ()
               "Finish the streamed block with its remaining text and separator."
               (when stream-open-p
                 (terminal-ui-stream-update
                  ui
                  :rows (append (when (plusp (length stream-pending))
                                  (termdown:markdown-render-line stream-renderer
                                                        stream-pending))
                                (list nil))
                  :tail nil)
                 (setf stream-pending ""
                       stream-open-p nil
                       stream-renderer nil))))
      (callback-agent-observer-create
       :text-callback #'stream-text-delta
       :reasoning-callback
       (lambda (delta)
         (when (plusp (length delta))
           (terminal-ui-note-status-progress ui))
         (when (and (application-reasoning-traces-p application)
                    (null presented-reasoning-text)
                    (plusp (length delta)))
           (text-buffer-append reasoning-text delta)
           (terminal-ui-set-preview-rows
            ui
            (application--reasoning-preview-rows application reasoning-text))))
       :status-callback
       (lambda (status details)
         (case status
           (:user-message-persisted
            (let ((pending-input-identifier
                    (getf details :pending-input-identifier)))
              (when (and pending-input-identifier
                         user-message-persisted-function)
                (funcall user-message-persisted-function
                         pending-input-identifier)))
            (let ((sequence (getf details :sequence))
                  (timestamp (getf details :time)))
              (unless (typep sequence '(integer 0))
                (error 'conversation-invariant-error
                       :message
                       "The persisted user message has no valid sequence."
                       :pathname
                       (conversation-pathname
                        (application-conversation application))
                       :sequence sequence))
              (with-recursive-lock-held
                  ((application-render-lock application))
                (terminal-ui-append-finalized
                 ui
                 (list :conversation
                       (conversation-identifier
                        (application-conversation application))
                       sequence)
                 (if continuation-p
                     (list (terminal-span ':hint "∙ goal continues"))
                     (application--transcript-entry
                      application
                      :style ':user
                      :header "❯ you"
                      :timestamp timestamp
                      :body
                      (user-message-input-text user-message-input))))
                (setf (application-rendered-sequence application) sequence)))
            (application-publish-recovery-session application))
           (:provider-progress
            (terminal-ui-note-status-progress ui))
           (:provider-request-started
            (terminal-ui-set-preview-rows ui nil)
            (text-buffer-clear reasoning-text)
            (text-buffer-clear stream-text)
            (setf presented-reasoning-text nil
                  activity-label (application-thinking-label))
            (application-publish-recovery-session application)
            (application-set-activity application activity-label))
           (:provider-retrying
            (let ((partial-output-p
                    (or stream-open-p
                        (plusp (length reasoning-text)))))
              (stream-flush)
              (terminal-ui-set-preview-rows ui nil)
              (when partial-output-p
                (application-present
                 application
                 (list
                  (terminal-span
                   ':hint
                   (format nil
                           "∙ provider stream interrupted; retrying ~D/~D"
                           (getf details :attempt)
                           (getf details :maximum-attempts))))))
              (text-buffer-clear reasoning-text)
              (text-buffer-clear stream-text)
              (setf presented-reasoning-text nil
                    stream-pending ""
                    stream-open-p nil
                    stream-renderer nil)
              (application-set-activity
               application
               (format nil "reconnecting ~D/~D in ~Ds"
                       (getf details :attempt)
                       (getf details :maximum-attempts)
                       (getf details :delay)))))
           (:provider-request-completed
            (reasoning-flush)
            (let ((completed-stream-text
                    (and stream-open-p (text-buffer-string stream-text)))
                  (completed-reasoning-text presented-reasoning-text))
              (stream-flush)
              (application-render-records
               application
               :streamed-assistant-text completed-stream-text
               :streamed-reasoning-text completed-reasoning-text)))
           (:tool-call-started
            (application-set-activity
             application
             (format nil "running ~A" (getf details :tool))))
           (:tool-call-completed
            (application-render-records application)
            (let ((tool-name (or (getf details :tool) "")))
              (cond
                ((string= tool-name "papercut.report")
                 (application--present-transient-tool-result
                  application details))
                ((and (string= tool-name "skill.load")
                      (getf details :success-p))
                 (application--present-skill-load-result
                  application details))))
            (application-set-activity application activity-label))
           (:steering-applied
            (application-render-records application)
            (application-set-activity application activity-label))
           (:compaction-started
            (application-set-activity application "compacting the conversation")
            (terminal-ui-set-compacting ui t))
           (:compaction-completed
            (terminal-ui-set-compacting ui nil)
            (application-render-records application)
            (application-set-activity application activity-label))
           (:turn-completed
            (terminal-ui-set-compacting ui nil)
            (terminal-ui-set-preview-rows ui nil)
            (application-set-activity application nil))))
       :steering-callback steering-function
       :steering-persisted-callback steering-persisted-function
       :pending-operations-callback pending-operations-function
       :command-authorization-callback
       (lambda (command directory)
         (application-authorize-command application command directory))
       :tool-authorization-callback
       (lambda (tool arguments)
         (application-authorize-tool application tool arguments))))))

(-> application--turn-final-text (provider-result) (option string))
(defun application--turn-final-text (result)
  "Return the joined assistant text of RESULT's final provider step."
  (let ((parts (loop for item in (provider-result-output-items result)
                     for text = (and (json-object-p item)
                                     (response-item-assistant-text item))
                     when text
                       collect text)))
    (when parts
      (format nil "~{~A~^~%~}" parts))))

(-> application--note-goal-turn (application provider-result) null)
(defun application--note-goal-turn (application result)
  "Mark the goal complete when RESULT's final message carries the marker."
  (let ((goal (application-goal application))
        (text (application--turn-final-text result)))
    (when (and goal
               (eq (getf goal :status) ':active)
               text
               (search *application-goal-complete-marker* text))
      (setf (getf (application-goal application) :status) ':complete)
      (application--record-goal application)
      (application-present
       application
       (list (terminal-span ':success "✓ goal complete")
             (terminal-span ':plain (string #\Newline))
             (terminal-span ':dim
                            (format nil "  ~A" (getf goal :objective)))))))
  nil)

(-> application--run-turn
    (application (or string user-message-input)
     &key (:continuation-p boolean)
          (:steering-function (option function))
          (:steering-persisted-function (option function))
          (:user-message-persisted-function (option function))
          (:pending-operations-function (option function))
          (:pending-input-identifier (option non-empty-string))
          (:tools-p boolean)
          (:tool-allowlist (option list))
          (:tool-restriction-p boolean))
    null)
(defun application--run-turn
    (application content
     &key continuation-p steering-function steering-persisted-function
          user-message-persisted-function pending-operations-function
          pending-input-identifier (tools-p t)
          tool-allowlist (tool-restriction-p nil))
  "Persist and run one model turn for CONTENT while retaining editable input."
  (let ((conversation (application-conversation application))
        (ui (application-ui application)))
    (unwind-protect
         (progn
           (application-set-activity application (application-thinking-label))
           (application--note-goal-turn
            application
            (agent-run-user-turn
              (application-agent application)
              content
              :observer (application-agent-observer
                         application
                         :steering-function steering-function
                         :steering-persisted-function steering-persisted-function
                         :user-message-persisted-function
                         user-message-persisted-function
                         :pending-operations-function pending-operations-function
                         :user-message-input content
                         :continuation-p continuation-p)
              :goal-context (application-goal-context application)
              :tools-p tools-p
              :tool-allowlist tool-allowlist
              :tool-restriction-p tool-restriction-p
              :pending-input-identifier pending-input-identifier)))
      (terminal-ui-set-compacting ui nil)
      (terminal-ui-set-preview-rows ui nil)
      (application-set-activity application nil)
      (terminal-ui-stream-update ui :tail nil)
      (application-render-records application)))
  nil)

(-> application--run-goal-continuations
    (application
     &key (:steering-function (option function))
          (:steering-persisted-function (option function))
          (:pending-operations-function (option function)))
    null)
(defun application--run-goal-continuations
    (application &key steering-function steering-persisted-function
                      pending-operations-function)
  "Run bounded automatic continuation turns while the session goal is active."
  (loop
    (let ((goal (application-goal application)))
      (unless (and goal (eq (getf goal :status) ':active))
        (return))
      (when (>= (getf goal :continuations)
                *application-goal-continuation-limit*)
        (setf (getf (application-goal application) :status) ':paused)
        (application--record-goal application)
        (application-present
         application
         (format nil
                 "The goal paused after ~D automatic continuations. ~
                  Use /goal resume or send a message to keep going."
                 *application-goal-continuation-limit*))
        (return))
      (incf (getf (application-goal application) :continuations))
      (application--run-turn application
                             *application-goal-continuation-prompt*
                             :continuation-p t
                             :steering-function steering-function
                             :steering-persisted-function
                             steering-persisted-function
                             :pending-operations-function
                             pending-operations-function)))
  nil)

(-> application-run-message
    (application (or string user-message-input)
     &key (:steering-function (option function))
          (:steering-persisted-function (option function))
          (:user-message-persisted-function (option function))
          (:pending-operations-function (option function))
          (:pending-input-identifier (option non-empty-string))
          (:tools-p boolean)
          (:tool-allowlist (option list))
          (:tool-restriction-p boolean)
          (:goal-continuations-p boolean))
    null)
(defun application-run-message
    (application content
     &key steering-function steering-persisted-function
          user-message-persisted-function pending-operations-function
          pending-input-identifier (tools-p t)
          tool-allowlist (tool-restriction-p nil) (goal-continuations-p t))
  "Run one user turn for CONTENT with optional tools and goal continuations."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (setf (getf (application-goal application) :continuations) 0)))
  (application--run-turn application
                         content
                         :steering-function steering-function
                         :steering-persisted-function steering-persisted-function
                         :user-message-persisted-function
                         user-message-persisted-function
                         :pending-operations-function pending-operations-function
                         :pending-input-identifier pending-input-identifier
                         :tools-p tools-p
                         :tool-allowlist tool-allowlist
                         :tool-restriction-p tool-restriction-p)
  (when goal-continuations-p
    (application--run-goal-continuations
     application
     :steering-function steering-function
     :steering-persisted-function steering-persisted-function
     :pending-operations-function pending-operations-function))
  nil)
