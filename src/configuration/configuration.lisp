(in-package #:autolith)

;;;; -- Configuration Object --

(defvar *configuration* nil
  "The configuration CONFIG reads and writes when no instance is given.")

(defvar *configuration-durable-values-function* nil
  "A function of a configuration returning its persisted durable values as a plist.

The preferences module installs it; without it nothing durable is read.")

(defvar *configuration-persist-function* nil
  "A function of (configuration name value) storing one durable value.

The preferences module installs it; without it durable changes stay in memory.")

(defclass configuration ()
  ((settings
    :initarg :settings
    :reader configuration-settings
    :type ordered-map
    :documentation "The settings this configuration understands, by name.")
   (values
    :initform (make-hash-table :test #'eq)
    :reader configuration-values
    :type hash-table
    :documentation "Stored values by setting name; absent names take their default.")
   (sources
    :initform (make-hash-table :test #'eq)
    :reader configuration-sources
    :type hash-table
    :documentation "Where each stored value came from: :override, :environment, :durable, or :session.")
   (lock
    :initform (make-lock "Autolith configuration")
    :reader configuration-lock
    :type t
    :documentation "The lock serializing value reads and writes.")
   (listeners
    :initform nil
    :accessor configuration-listeners
    :type list
    :documentation "Functions of (configuration setting old new) called after a value changes.")
   (provider-validation-p
    :initarg :provider-validation-p
    :initform t
    :accessor configuration-provider-validation-p
    :type boolean
    :documentation "Whether model choices are checked against the provider registry.")
   (preferences-lock
    :initform (make-lock "Autolith preferences store")
    :reader configuration-preferences-lock
    :type t
    :documentation "The lock serializing durable value reads and writes."))
  (:documentation "The settings of one Autolith process, agent, or frame."))

(deftype configuration-source ()
  "How a stored configuration value was chosen."
  '(member :override :environment :durable :session))

(-> configuration--required ((option configuration)) configuration)
(defun configuration--required (configuration)
  "Return CONFIGURATION, or signal when no configuration is current."
  (or configuration
      (error 'configuration-error
             :message "No configuration is current; pass one or bind *configuration*.")))

(-> configuration-setting (configuration keyword) setting)
(defun configuration-setting (configuration name)
  "Return the setting NAME known to CONFIGURATION."
  (find-setting name (configuration-settings configuration)))

(-> configuration-setting-list (configuration) list)
(defun configuration-setting-list (configuration)
  "Return CONFIGURATION's settings in definition order."
  (settings-list (configuration-settings configuration)))

(-> configuration--stored-value (configuration setting) (values t boolean))
(defun configuration--stored-value (configuration setting)
  "Return SETTING's stored value in CONFIGURATION and whether one is stored."
  (with-lock-held ((configuration-lock configuration))
    (gethash (setting-name setting) (configuration-values configuration))))

(-> configuration-setting-value (configuration setting) t)
(defun configuration-setting-value (configuration setting)
  "Return SETTING's current value in CONFIGURATION, computing derived and default values."
  (if (typep setting 'derived-setting)
      (funcall (setting-function setting) configuration)
      (multiple-value-bind (value present-p)
          (configuration--stored-value configuration setting)
        (if present-p
            value
            (setting-default-value setting configuration)))))

(-> configuration-group-values (configuration keyword) list)
(defun configuration-group-values (configuration group)
  "Return CONFIGURATION's stored or default values of GROUP's settings as a plist."
  (loop for setting in (configuration-setting-list configuration)
        when (and (eq (setting-group setting) group)
                  (not (eq (setting-scope setting) ':derived)))
          append (list (setting-name setting)
                       (configuration-setting-value configuration setting))))

(-> configuration-setting-source (configuration keyword) (option configuration-source))
(defun configuration-setting-source (configuration name)
  "Return where CONFIGURATION's value for NAME came from, or NIL for a default."
  (with-lock-held ((configuration-lock configuration))
    (values (gethash name (configuration-sources configuration)))))

(-> config (keyword &optional (option configuration)) t)
(defun config (name &optional (configuration *configuration*))
  "Return the value of setting NAME in CONFIGURATION, by default the current one."
  (let ((configuration (configuration--required configuration)))
    (configuration-setting-value
     configuration (configuration-setting configuration name))))

(-> (setf config) (t keyword &optional (option configuration)) t)
(defun (setf config) (value name &optional (configuration *configuration*))
  "Store VALUE as setting NAME in CONFIGURATION after coercion and validation."
  (let ((configuration (configuration--required configuration)))
    (configuration-set configuration (configuration-setting configuration name) value
                       :source ':session)))

(-> configuration-set
    (configuration setting t &key (:source configuration-source))
    t)
(defgeneric configuration-set (configuration setting value &key source)
  (:documentation
   "Coerce, validate, store VALUE for SETTING in CONFIGURATION, and notify listeners.

SOURCE records how the value was chosen. Returns the stored value."))

(defmethod configuration-set
    ((configuration configuration) (setting setting) value &key (source ':session))
  "Store the coerced value and tell listeners about a change."
  (when (eq (setting-scope setting) ':derived)
    (error 'configuration-error
           :message (format nil "~A is derived and cannot be set."
                            (setting-label setting))))
  (let ((coerced (setting-coerce setting value configuration)))
    (setting-validate setting coerced configuration)
    (let ((old (configuration-setting-value configuration setting)))
      (with-lock-held ((configuration-lock configuration))
        (setf (gethash (setting-name setting) (configuration-values configuration))
              coerced
              (gethash (setting-name setting) (configuration-sources configuration))
              source))
      (when (and (eq (setting-scope setting) ':durable)
                 (eq source ':session)
                 *configuration-persist-function*)
        (funcall *configuration-persist-function* configuration
                 (setting-name setting) coerced))
      (configuration--note-change configuration setting old coerced)
      coerced)))

(-> configuration-persist (configuration keyword) null)
(defun configuration-persist (configuration name)
  "Write CONFIGURATION's current value of durable setting NAME to the preferences store."
  (let ((setting (configuration-setting configuration name)))
    (unless (eq (setting-scope setting) ':durable)
      (error 'configuration-error
             :message (format nil "~A is not a durable setting." (setting-label setting))))
    (when *configuration-persist-function*
      (funcall *configuration-persist-function* configuration name
               (configuration-setting-value configuration setting))))
  nil)

(-> configuration-unset (configuration keyword) null)
(defun configuration-unset (configuration name)
  "Forget CONFIGURATION's stored value for NAME so its default applies again."
  (let* ((setting (configuration-setting configuration name))
         (old (configuration-setting-value configuration setting)))
    (with-lock-held ((configuration-lock configuration))
      (remhash name (configuration-values configuration))
      (remhash name (configuration-sources configuration)))
    (configuration--note-change
     configuration setting old (configuration-setting-value configuration setting)))
  nil)

(-> configuration-add-listener (configuration function) null)
(defun configuration-add-listener (configuration listener)
  "Call LISTENER with (configuration setting old new) after each value change."
  (with-lock-held ((configuration-lock configuration))
    (pushnew listener (configuration-listeners configuration)))
  nil)

(-> configuration-remove-listener (configuration function) null)
(defun configuration-remove-listener (configuration listener)
  "Stop calling LISTENER for CONFIGURATION's changes."
  (with-lock-held ((configuration-lock configuration))
    (setf (configuration-listeners configuration)
          (remove listener (configuration-listeners configuration))))
  nil)

(-> configuration--note-change (configuration setting t t) null)
(defun configuration--note-change (configuration setting old new)
  "Tell CONFIGURATION's listeners that SETTING changed from OLD to NEW."
  (unless (equal old new)
    (dolist (listener (with-lock-held ((configuration-lock configuration))
                        (copy-list (configuration-listeners configuration))))
      (funcall listener configuration setting old new)))
  nil)

(defmacro with-configuration ((configuration) &body body)
  "Evaluate BODY with CONFIGURATION as the current configuration for CONFIG."
  `(let ((*configuration* ,configuration))
     ,@body))


;;;; -- Setting Kinds Specific to Autolith --

(defclass model-setting (choice-setting)
  ()
  (:default-initargs :type 'non-empty-string)
  (:documentation "The provider model, validated against the registry when enabled."))

(defclass reasoning-effort-setting (choice-setting)
  ()
  (:default-initargs :type 'non-empty-string)
  (:documentation "The reasoning effort, validated against the current model's efforts."))

(defclass working-directory-setting (pathname-setting)
  ()
  (:documentation "The workspace directory, resolved to an existing directory on change."))

(defclass absolute-file-setting (pathname-setting)
  ()
  (:documentation "A file pathname anchored to the process directory when relative."))

(defclass management-transport-setting (choice-setting)
  ()
  (:default-initargs :type 'keyword :options '(:unix :tcp))
  (:documentation "The management endpoint transport, limited by host socket support."))

(defclass web-search-mode-setting (choice-setting)
  ()
  (:default-initargs :type 'non-empty-string)
  (:documentation "The standalone web search mode, read case-insensitively."))

(defclass directory-setting (pathname-setting)
  ()
  (:documentation "A directory pathname, always stored in directory form."))

(defmethod setting-coerce ((setting web-search-mode-setting) (value string) configuration)
  "Read the mode name in lower case."
  (declare (ignore setting configuration))
  (string-downcase value))

(defmethod setting-coerce ((setting directory-setting) value configuration)
  "Store directory designators in directory form."
  (declare (ignore setting configuration))
  (uiop:ensure-directory-pathname
   (if (stringp value) (parse-namestring value) value)))

(defmethod setting-options ((setting model-setting) configuration)
  "Offer every model the effective provider registry serves."
  (declare (ignore setting configuration))
  (copy-list *supported-models*))

(defmethod setting-validate ((setting model-setting) value configuration)
  "Require a registered model unless CONFIGURATION defers provider validation."
  (unless (typep value (setting-type setting))
    (error 'configuration-error
           :message (format nil "~A does not accept ~S." (setting-label setting) value)))
  (when (and (configuration-provider-validation-p configuration)
             (not (configuration--model-supported-p value)))
    (error 'configuration-error
           :message (format nil "Unsupported model ~S. The choices are ~{~A~^, ~}."
                            value *supported-models*)))
  nil)

(defmethod setting-options ((setting reasoning-effort-setting) configuration)
  "Offer the efforts the current model supports."
  (declare (ignore setting))
  (configuration--reasoning-efforts-for (config :model configuration)))

(defmethod setting-validate ((setting reasoning-effort-setting) value configuration)
  "Require an effort the current model supports, once providers are validated.

Before executable user initialization registers providers, a model's efforts
are unknown, so deferred validation accepts any effort name."
  (unless (typep value (setting-type setting))
    (error 'configuration-error
           :message (format nil "~A does not accept ~S." (setting-label setting) value)))
  (let ((model (config :model configuration))
        (efforts (setting-options setting configuration)))
    (unless (or (not (configuration-provider-validation-p configuration))
                (member value efforts :test #'string=))
      (error 'configuration-error
             :message
             (format nil "Unsupported reasoning effort ~S for model ~A. The choices are ~{~A~^, ~}."
                     value model efforts))))
  nil)

(defmethod configuration-set :after
    ((configuration configuration) (setting model-setting) value &key source)
  "Keep the reasoning effort valid for the newly selected model."
  (declare (ignore value source))
  (when (configuration-provider-validation-p configuration)
    (let ((effort (configuration-setting configuration :reasoning-effort))
          (efforts (configuration--reasoning-efforts-for (config :model configuration))))
      (unless (member (configuration-setting-value configuration effort) efforts
                      :test #'string=)
        (configuration-set configuration effort (first efforts) :source ':session)))))

(defmethod setting-coerce ((setting working-directory-setting) value configuration)
  "Resolve user-supplied text to an existing directory; store pathnames as given.

A string is what a person typed, so it is expanded, made absolute against the
current workspace, and must name an existing directory. A pathname is a
program's choice, such as a child agent's or test's workspace that may not
exist yet, and is only put in directory form."
  (declare (ignore setting))
  (if (stringp value)
      (configuration--resolve-working-directory configuration value)
      (uiop:ensure-directory-pathname value)))

(defmethod setting-coerce ((setting absolute-file-setting) value configuration)
  "Anchor relative file pathnames to the process directory."
  (declare (ignore setting configuration))
  (configuration--absolute-file-pathname
   (if (stringp value) (parse-namestring value) value)))

(defmethod setting-coerce ((setting management-transport-setting) (value string) configuration)
  "Read the transport name case-insensitively."
  (declare (ignore setting configuration))
  (intern (string-upcase value) '#:keyword))

(defmethod setting-validate ((setting management-transport-setting) value configuration)
  "Require a transport the host supports."
  (call-next-method)
  (when (and (eq value ':unix)
             (not (platform-supports-p *platform* ':local-sockets)))
    (error 'configuration-error
           :message "AUTOLITH_MANAGEMENT_REPL_TRANSPORT=unix needs filesystem sockets, which this platform lacks; use tcp."))
  nil)


;;;; -- Settings --

(define-setting :source-root (directory-setting)
  :label "Source root"
  :group :paths
  :documentation "The tracked Autolith source root."
  :environment "AUTOLITH_SOURCE_ROOT"
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (asdf:system-source-directory :autolith)))

(define-setting :working-directory (working-directory-setting)
  :label "Working directory"
  :group :paths
  :documentation "The workspace visible to the agent and Lisp worker."
  :default (lambda (configuration)
             (declare (ignore configuration))
             (uiop:ensure-directory-pathname (uiop:getcwd))))

(define-setting :config-root (directory-setting)
  :label "Config root"
  :group :paths
  :documentation "The root for user-editable Autolith configuration."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (platform-application-root *platform* ':config)))

(define-setting :data-root (directory-setting)
  :label "Data root"
  :group :paths
  :documentation "The root for durable user data such as conversations."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (platform-application-root *platform* ':data)))

(define-setting :state-root (directory-setting)
  :label "State root"
  :group :paths
  :documentation "The root for mutable runtime state such as queues and journals."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (platform-application-root *platform* ':state)))

(define-setting :cache-root (directory-setting)
  :label "Cache root"
  :group :paths
  :documentation "The root for replaceable caches and temporary artifacts."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (platform-application-root *platform* ':cache)))

(define-setting :codex-auth-path (pathname-setting)
  :label "Codex auth file"
  :group :paths
  :documentation "The optional Codex OAuth bootstrap file."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (merge-pathnames
              "auth.json"
              (environment-directory
               "CODEX_HOME"
               (merge-pathnames ".codex/" (user-homedir-pathname))))))

(define-setting :grok-bootstrap-auth-path (pathname-setting)
  :label "Grok auth file"
  :group :paths
  :documentation "The optional Grok Build OAuth bootstrap file."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (configuration--default-grok-bootstrap-path)))

(define-setting :model (model-setting)
  :label "Model"
  :group :model
  :documentation "The provider model identifier."
  :scope :durable
  :environment "AUTOLITH_MODEL"
  :default (lambda (configuration)
             (declare (ignore configuration))
             *default-model*))

(define-setting :reasoning-effort (reasoning-effort-setting)
  :label "Reasoning effort"
  :group :model
  :documentation "The user-visible reasoning effort."
  :scope :durable
  :environment "AUTOLITH_REASONING_EFFORT"
  :default (lambda (configuration)
             (declare (ignore configuration))
             *default-reasoning-effort*))

(define-setting :codex-fast-mode-p (boolean-setting)
  :label "Codex Fast mode"
  :group :model
  :documentation "Whether Codex requests opt in to Fast mode at double plan usage."
  :scope :durable
  :environment "AUTOLITH_CODEX_FAST_MODE")

(define-setting :web-search-mode (web-search-mode-setting)
  :label "Web search"
  :group :model
  :documentation "The provider web search mode."
  :type 'non-empty-string
  :environment "AUTOLITH_WEB_SEARCH"
  :options (lambda (configuration)
             (declare (ignore configuration))
             (copy-list *supported-web-search-modes*))
  :default "cached")

(define-setting :context-window (derived-setting)
  :label "Context window"
  :group :model
  :documentation "The provider context window in tokens for the model."
  :type '(integer 1)
  :function (lambda (configuration)
              (configuration--context-window-for (config :model configuration))))

(define-setting :compaction-threshold-percent (integer-setting)
  :label "Compaction threshold"
  :group :model
  :documentation "The context window percentage that triggers compaction."
  :minimum 1
  :maximum 95
  :environment "AUTOLITH_COMPACTION_THRESHOLD"
  :default (lambda (configuration)
             (declare (ignore configuration))
             *default-compaction-threshold-percent*))

(define-setting :provider-endpoint (derived-setting)
  :label "Provider endpoint"
  :group :model
  :documentation "The streaming Responses endpoint."
  :type 'non-empty-string
  :visible-p nil
  :function (lambda (configuration)
              (configuration--provider-endpoint-for (config :model configuration))))

(define-setting :reasoning-traces-p (boolean-setting)
  :label "Reasoning traces"
  :group :transcript
  :documentation "Whether provider reasoning summaries are requested and shown."
  :scope :durable)

(define-setting :compact-view-p (boolean-setting)
  :label "Compact view"
  :group :transcript
  :documentation "Whether verbose tool calls are condensed and routine results hidden."
  :scope :durable
  :default t)

(define-setting :turn-timestamps-p (boolean-setting)
  :label "Turn timestamps"
  :group :transcript
  :documentation "Whether transcript turn headers include local timestamps."
  :scope :durable)

(define-setting :cache-miss-notices-p (boolean-setting)
  :label "Cache miss notices"
  :group :transcript
  :documentation "Whether requests that re-read uncached context are reported."
  :scope :durable)

(define-setting :simple-technical-english-p (boolean-setting)
  :label "Simple Technical English"
  :group :behavior
  :documentation "Whether natural-language replies use Simple Technical English."
  :scope :durable)

(define-setting :session-title-generation-p (boolean-setting)
  :label "Generated session titles"
  :group :behavior
  :documentation "Whether the provider may refresh locally derived session titles."
  :scope :durable
  :default t)

(define-setting :hurry-up-p (boolean-setting)
  :label "Hurry-up mode"
  :group :behavior
  :documentation "Whether requests carry hurry-up guidance favoring direct work."
  :scope :session)

(define-setting :fullscreen-p (boolean-setting)
  :label "Fullscreen"
  :group :terminal
  :documentation "Whether interactive sessions use the fullscreen terminal UI."
  :scope :durable)

(define-setting :permission-mode (choice-setting)
  :label "Saved permission mode"
  :group :behavior
  :documentation "The durable command-permission mode, or unset."
  :scope :durable
  :type '(option (member :ask :auto))
  :options '(:ask :auto))

(define-setting :immutable-p (boolean-setting)
  :label "Immutable image"
  :group :process
  :documentation "Whether mutation-capable active-image tools are disabled.")

(define-setting :management-repl-enabled-p (boolean-setting)
  :label "Management REPL"
  :group :management
  :documentation "Whether the authenticated active-image management endpoint starts."
  :environment "AUTOLITH_MANAGEMENT_REPL")

(define-setting :management-repl-transport (management-transport-setting)
  :label "Management transport"
  :group :management
  :documentation "The management endpoint transport."
  :environment "AUTOLITH_MANAGEMENT_REPL_TRANSPORT"
  :default (lambda (configuration)
             (declare (ignore configuration))
             (if (platform-supports-p *platform* ':local-sockets) ':unix ':tcp)))

(define-setting :management-repl-unix-socket-path (absolute-file-setting)
  :label "Management socket"
  :group :management
  :documentation "The private Unix management socket pathname."
  :environment "AUTOLITH_MANAGEMENT_REPL_UNIX_SOCKET"
  :default (lambda (configuration)
             (merge-pathnames "management/repl.sock" (config :state-root configuration))))

(define-setting :management-repl-tcp-address (string-setting)
  :label "Management address"
  :group :management
  :documentation "The IPv4 loopback management listener address."
  :type 'non-empty-string
  :environment "AUTOLITH_MANAGEMENT_REPL_TCP_ADDRESS"
  :default "127.0.0.1")

(define-setting :management-repl-tcp-port (integer-setting)
  :label "Management port"
  :group :management
  :documentation "The management TCP listener port."
  :minimum 1
  :maximum 65535
  :environment "AUTOLITH_MANAGEMENT_REPL_TCP_PORT"
  :default 4141)

(define-setting :management-repl-token-file-path (absolute-file-setting)
  :label "Management token file"
  :group :management
  :documentation "The external owner-only authentication token file pathname."
  :environment "AUTOLITH_MANAGEMENT_REPL_TOKEN_FILE"
  :default (lambda (configuration)
             (merge-pathnames "management-repl.token" (config :config-root configuration))))

(define-setting :management-repl-evaluation-timeout (integer-setting)
  :label "Management timeout"
  :group :management
  :documentation "The management request deadline in seconds."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_TIMEOUT"
  :default 10)

(define-setting :management-repl-maximum-frame-size (integer-setting)
  :label "Management frame size"
  :group :management
  :documentation "The maximum management wire frame size in octets."
  :minimum 128
  :environment "AUTOLITH_MANAGEMENT_REPL_MAX_FRAME"
  :default 1048576)

(define-setting :management-repl-maximum-source-size (integer-setting)
  :label "Management source size"
  :group :management
  :documentation "The maximum evaluation source size in UTF-8 octets."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_MAX_SOURCE"
  :default 262144)

(define-setting :management-repl-maximum-output-size (integer-setting)
  :label "Management output size"
  :group :management
  :documentation "The maximum captured output and value text size."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_MAX_OUTPUT"
  :default 262144)

(define-setting :management-repl-queue-capacity (integer-setting)
  :label "Management queue"
  :group :management
  :documentation "The maximum queued management evaluations."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_QUEUE_CAPACITY"
  :default 8)

(define-setting :management-repl-maximum-clients (integer-setting)
  :label "Management clients"
  :group :management
  :documentation "The maximum accepted management clients, including authentication."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_MAX_CLIENTS"
  :default 8)

(define-setting :management-repl-authentication-timeout (integer-setting)
  :label "Management auth timeout"
  :group :management
  :documentation "The absolute authentication deadline in seconds."
  :minimum 1
  :environment "AUTOLITH_MANAGEMENT_REPL_AUTH_TIMEOUT"
  :default 10)


;;;; -- Construction --

(-> configuration--settings-snapshot () ordered-map)
(defun configuration--settings-snapshot ()
  "Return a fresh ordered map of the registered settings."
  (let ((snapshot (make-ordered-map :test #'eq)))
    (dolist (setting (settings-list))
      (ordered-map-set snapshot (setting-name setting) setting))
    snapshot))

(-> configuration--override-order (configuration list) list)
(defun configuration--override-order (configuration overrides)
  "Return OVERRIDES as (name . value) pairs in setting definition order."
  (let ((pairs nil))
    (dolist (setting (configuration-setting-list configuration))
      (let ((cell (member (setting-name setting) overrides)))
        (when cell
          (push (cons (setting-name setting) (second cell)) pairs))))
    (loop for (name value) on overrides by #'cddr
          unless (assoc name pairs)
            do (find-setting name (configuration-settings configuration)))
    (nreverse pairs)))

(-> make-configuration (&rest t) configuration)
(defun make-configuration (&rest overrides &key (provider-validation-p t) &allow-other-keys)
  "Return a configuration holding only OVERRIDES over the setting defaults.

Neither the environment nor the preferences file is consulted, which makes
the result deterministic for tests and child agents. Overrides apply in
setting definition order, so a model override precedes its reasoning effort."
  (let ((configuration (make-instance 'configuration
                                      :settings (configuration--settings-snapshot)
                                      :provider-validation-p provider-validation-p))
        (overrides (let ((copy (copy-list overrides)))
                     (remf copy :provider-validation-p)
                     copy)))
    (loop for (name . value) in (configuration--override-order configuration overrides)
          do (configuration-set configuration (configuration-setting configuration name)
                                value :source ':override))
    configuration))

(-> configuration-copy (configuration &rest t) configuration)
(defun configuration-copy
    (configuration &rest overrides
     &key (provider-validation-p (configuration-provider-validation-p configuration))
     &allow-other-keys)
  "Return a copy of CONFIGURATION with OVERRIDES applied in setting order.

PROVIDER-VALIDATION-P defaults to the original's; passing NIL lets a copy name
a model the registry does not serve. Listeners stay with the original; a child
or frame reacts to its own changes."
  (let ((copy (make-instance 'configuration
                             :settings (configuration-settings configuration)
                             :provider-validation-p provider-validation-p))
        (overrides (let ((copy (copy-list overrides)))
                     (remf copy :provider-validation-p)
                     copy)))
    (with-lock-held ((configuration-lock configuration))
      (maphash (lambda (name value)
                 (setf (gethash name (configuration-values copy)) value))
               (configuration-values configuration))
      (maphash (lambda (name source)
                 (setf (gethash name (configuration-sources copy)) source))
               (configuration-sources configuration)))
    (loop for (name . value) in (configuration--override-order copy overrides)
          do (configuration-set copy (configuration-setting copy name) value
                                :source ':override))
    copy))

(-> configuration--environment-value (setting) (values t boolean))
(defun configuration--environment-value (setting)
  "Return SETTING's environment variable text and whether it is set and non-empty."
  (let* ((variable (setting-environment setting))
         (value (and variable (uiop:getenv variable))))
    (if (non-empty-string-p value)
        (values value t)
        (values nil nil))))

(-> configuration-create (&rest t) configuration)
(defun configuration-create
    (&rest overrides &key defer-provider-validation-p (durable-p t) &allow-other-keys)
  "Create the process configuration from OVERRIDES, the environment, and preferences.

Each setting takes the first source that supplies it: an explicit override,
its environment variable, the durable preferences file for durable settings,
then its default. Overrides and the environment apply first so the file is
read from the roots they select. A durable value that no longer validates is
dropped rather than applied. DEFER-PROVIDER-VALIDATION-P leaves model checks
to CONFIGURATION-VALIDATE-MODEL once executable user initialization has
registered providers. DURABLE-P NIL skips the preferences file."
  (let ((configuration (make-instance 'configuration
                                      :settings (configuration--settings-snapshot)
                                      :provider-validation-p
                                      (not defer-provider-validation-p)))
        (overrides (let ((copy (copy-list overrides)))
                     (remf copy :defer-provider-validation-p)
                     (remf copy :durable-p)
                     copy)))
    (dolist (setting (configuration-setting-list configuration))
      (unless (eq (setting-scope setting) ':derived)
        (let ((override (member (setting-name setting) overrides)))
          (multiple-value-bind (environment environment-p)
              (configuration--environment-value setting)
            (cond
              (override
               (configuration-set configuration setting (second override)
                                  :source ':override))
              (environment-p
               (configuration-set configuration setting environment
                                  :source ':environment)))))))
    (when durable-p
      (let ((durable (configuration-durable-values configuration)))
        (dolist (setting (configuration-setting-list configuration))
          (let ((cell (member (setting-name setting) durable)))
            (when (and cell
                       (eq (setting-scope setting) ':durable)
                       (not (nth-value 1 (configuration--stored-value
                                          configuration setting))))
              (handler-case
                  (configuration-set configuration setting (second cell)
                                     :source ':durable)
                (configuration-error ()
                  nil)))))))
    (configuration--validate-defaults configuration)
    configuration))

(-> configuration--validate-defaults (configuration) null)
(defun configuration--validate-defaults (configuration)
  "Validate every unset process setting's default, surfacing host limits early."
  (dolist (setting (configuration-setting-list configuration))
    (when (and (eq (setting-scope setting) ':process)
               (not (nth-value 1 (configuration--stored-value configuration setting))))
      (setting-validate setting (configuration-setting-value configuration setting)
                        configuration)))
  nil)

(-> configuration-durable-values (configuration) list)
(defun configuration-durable-values (configuration)
  "Return CONFIGURATION's persisted durable settings as a (name value ...) plist."
  (if *configuration-durable-values-function*
      (funcall *configuration-durable-values-function* configuration)
      nil))

(-> configuration-validate-model (configuration) configuration)
(defun configuration-validate-model (configuration)
  "Enable provider validation on CONFIGURATION and check its model and effort.

A durable model the effective registry no longer serves reverts to the
default; an explicit or environment model that fails validation signals."
  (setf (configuration-provider-validation-p configuration) t)
  (let ((model (configuration-setting configuration :model)))
    (handler-case
        (setting-validate model (config :model configuration) configuration)
      (configuration-error (condition)
        (if (eq (configuration-setting-source configuration :model) ':durable)
            (configuration-unset configuration :model)
            (error condition))))
    (let ((efforts (configuration--reasoning-efforts-for (config :model configuration))))
      (unless (member (config :reasoning-effort configuration) efforts :test #'string=)
        (if (member (configuration-setting-source configuration :reasoning-effort)
                    '(:durable nil))
            (configuration-unset configuration :reasoning-effort)
            (setting-validate (configuration-setting configuration :reasoning-effort)
                              (config :reasoning-effort configuration)
                              configuration)))))
  configuration)


;;;; -- Derived Helpers --

(-> configuration-codex-fast-mode-available-p (configuration) boolean)
(defun configuration-codex-fast-mode-available-p (configuration)
  "Return true when CONFIGURATION's current Codex model supports Fast mode."
  (and (eq (model-family (config :model configuration)) ':codex)
       (not (null (member (config :model configuration)
                          *codex-fast-mode-models*
                          :test #'string=)))))

(-> configuration-codex-fast-mode-active-p (configuration) boolean)
(defun configuration-codex-fast-mode-active-p (configuration)
  "Return true when Fast mode is enabled and available for CONFIGURATION."
  (and (config :codex-fast-mode-p configuration)
       (configuration-codex-fast-mode-available-p configuration)))

(-> configuration-compaction-token-limit (configuration) integer)
(defun configuration-compaction-token-limit (configuration)
  "Return the token count at which CONFIGURATION compacts the conversation."
  (floor (* (config :context-window configuration)
            (config :compaction-threshold-percent configuration))
         100))

(-> configuration--absolute-file-pathname (pathname) pathname)
(defun configuration--absolute-file-pathname (pathname)
  "Anchor PATHNAME to the process directory captured during configuration."
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname (uiop:getcwd))))

(-> configuration--expanded-working-directory
    ((or pathname string))
    (or pathname string))
(defun configuration--expanded-working-directory (location)
  "Expand a leading ~/ in LOCATION while leaving other paths unchanged."
  (if (stringp location)
      (cond
        ((string= location "~")
         (user-homedir-pathname))
        ((uiop:string-prefix-p "~/" location)
         (merge-pathnames (subseq location 2) (user-homedir-pathname)))
        (t
         location))
      location))

(-> configuration--resolve-working-directory
    (configuration (or pathname string))
    pathname)
(defun configuration--resolve-working-directory (configuration location)
  "Resolve LOCATION against CONFIGURATION and return its existing directory truename."
  (let ((previous (config :working-directory configuration)))
    (handler-case
        (let* ((candidate
                 (uiop:ensure-pathname
                  (platform-pathname
                   (configuration--expanded-working-directory location))
                  :defaults previous
                  :ensure-absolute t
                  :ensure-directory t
                  :want-non-wild t))
               (directory (uiop:directory-exists-p candidate)))
          (unless directory
            (error 'working-directory-error
                   :message (format nil "Working directory ~S does not exist or is not a directory."
                                    location)
                   :requested-path location
                   :previous-directory previous
                   :stage ':validation
                   :cause nil))
          (uiop:ensure-directory-pathname (platform-truename *platform* directory)))
      (working-directory-error (condition)
        (error condition))
      (error (condition)
        (error 'working-directory-error
               :message (format nil "Cannot use ~S as a working directory: ~A"
                                location condition)
               :requested-path location
               :previous-directory previous
               :stage ':validation
               :cause condition)))))

(-> configuration-ensure-directories (configuration) configuration)
(defun configuration-ensure-directories (configuration)
  "Create CONFIGURATION's private config, data, state, and cache directories."
  (dolist (directory (list (config :config-root configuration)
                            (config :data-root configuration)
                            (config :state-root configuration)
                            (config :cache-root configuration)))
    (multiple-value-bind (pathname created-p)
        (ensure-directories-exist directory)
      (when created-p
        (platform-make-private *platform* pathname))))
  configuration)

(-> configuration-conversation-root (configuration) pathname)
(defun configuration-conversation-root (configuration)
  "Return the directory containing conversation identities and chunk logs."
  (merge-pathnames "conversations/" (config :data-root configuration)))

(-> configuration-inference-root (configuration) pathname)
(defun configuration-inference-root (configuration)
  "Return the directory containing inference frame trace conversations."
  (merge-pathnames "inferences/" (config :data-root configuration)))

(-> configuration-conversation-identifier-migration-path (configuration) pathname)
(defun configuration-conversation-identifier-migration-path (configuration)
  "Return the durable legacy conversation identifier migration record."
  (merge-pathnames "conversation-identifier-migration.sexp"
                   (config :state-root configuration)))

(-> configuration-user-init-path (configuration) pathname)
(defun configuration-user-init-path (configuration)
  "Return the user-authored Lisp initialization pathname."
  (merge-pathnames "init.lisp" (config :config-root configuration)))

(-> configuration-directory-scopes-path (configuration) pathname)
(defun configuration-directory-scopes-path (configuration)
  "Return the user-owned directory-scope trust manifest pathname."
  (merge-pathnames "directory-scopes.sexp"
                   (config :config-root configuration)))

(-> configuration-memory-path (configuration) pathname)
(defun configuration-memory-path (configuration)
  "Return the append-only persistent memory pathname."
  (merge-pathnames "memories.sexp" (config :data-root configuration)))

(-> configuration-papercut-path (configuration) pathname)
(defun configuration-papercut-path (configuration)
  "Return the append-only persistent papercut pathname."
  (merge-pathnames "papercuts.sexp" (config :data-root configuration)))

(-> configuration-agenda-path (configuration) pathname)
(defun configuration-agenda-path (configuration)
  "Return the atomic workspace-agenda pathname."
  (merge-pathnames "agendas.sexp" (config :data-root configuration)))


(-> configuration-image-commit-root (configuration) pathname)
(defun configuration-image-commit-root (configuration)
  "Return the directory containing immutable private image commits."
  (merge-pathnames "image-commits/" (config :data-root configuration)))

(-> configuration-mutation-history-root (configuration) pathname)
(defun configuration-mutation-history-root (configuration)
  "Return the private Git repository backing durable mutation snapshots."
  (merge-pathnames "mutation-history/"
                   (config :state-root configuration)))

(-> configuration-lisp-image-root (configuration) pathname)
(defun configuration-lisp-image-root (configuration)
  "Return the directory containing immutable saved Lisp worker images."
  (merge-pathnames "lisp-images/" (config :data-root configuration)))

(-> configuration-current-image-commit-path (configuration) pathname)
(defun configuration-current-image-commit-path (configuration)
  "Return the atomic pointer to the image commit used by normal startup."
  (merge-pathnames "current-image-commit.sexp"
                   (config :state-root configuration)))

(-> configuration-preferences-path (configuration) pathname)
(defun configuration-preferences-path (configuration)
  "Return the atomic global preferences pathname."
  (merge-pathnames "preferences.sexp" (config :state-root configuration)))

(-> configuration-project-adaptation-offers-path (configuration) pathname)
(defun configuration-project-adaptation-offers-path (configuration)
  "Return the atomic per-project AUTOLITH.org offer-state pathname."
  (merge-pathnames "project-adaptation-offers.sexp"
                   (config :state-root configuration)))

(-> configuration-update-state-path (configuration) pathname)
(defun configuration-update-state-path (configuration)
  "Return the atomic cached release-availability state pathname."
  (merge-pathnames "update-state.sexp" (config :state-root configuration)))

(-> configuration-permissions-path (configuration) pathname)
(defun configuration-permissions-path (configuration)
  "Return the atomic persistent command-permission pathname."
  (merge-pathnames "permissions.sexp" (config :state-root configuration)))

(-> configuration-pending-inputs-path (configuration pathname) pathname)
(defun configuration-pending-inputs-path (configuration conversation-pathname)
  "Return one conversation's atomic unprocessed-input pathname."
  (merge-pathnames
   (make-pathname :name (pathname-name conversation-pathname) :type "sexp")
   (merge-pathnames "pending-inputs/"
                    (config :state-root configuration))))

(-> configuration-recovery-input-vault-path
    (configuration pathname)
    pathname)
(defun configuration-recovery-input-vault-path
    (configuration conversation-pathname)
  "Return one conversation's atomic recovered-input vault pathname."
  (merge-pathnames
   (make-pathname :name (pathname-name conversation-pathname) :type "sexp")
   (merge-pathnames "recovery-input-vault/"
                    (config :state-root configuration))))

(-> configuration-legacy-pending-inputs-path (configuration) pathname)
(defun configuration-legacy-pending-inputs-path (configuration)
  "Return the legacy process-global unprocessed-input pathname."
  (merge-pathnames "pending-inputs.sexp"
                   (config :state-root configuration)))

(-> configuration-plan-path (configuration string) pathname)
(defun configuration-plan-path (configuration workspace-identifier)
  "Return one workspace's atomic plan pathname."
  (merge-pathnames
   (make-pathname :name workspace-identifier :type "sexp")
   (merge-pathnames "plans/" (config :state-root configuration))))

(-> configuration-legacy-plan-path (configuration) pathname)
(defun configuration-legacy-plan-path (configuration)
  "Return the legacy process-global workspace plan pathname."
  (merge-pathnames "plan.sexp" (config :state-root configuration)))

(-> configuration-auth-path (configuration) pathname)
(defun configuration-auth-path (configuration)
  "Return Autolith's private provider credential pathname."
  (merge-pathnames "auth.sexp" (config :state-root configuration)))

(-> configuration-grok-auth-path (configuration) pathname)
(defun configuration-grok-auth-path (configuration)
  "Return Autolith's private Grok OAuth credential pathname."
  (merge-pathnames "grok-auth.sexp" (config :state-root configuration)))

(-> configuration-nous-auth-path (configuration) pathname)
(defun configuration-nous-auth-path (configuration)
  "Return Autolith's private Nous OAuth credential pathname."
  (merge-pathnames "nous-auth.sexp" (config :state-root configuration)))

(-> configuration-api-keys-path (configuration) pathname)
(defun configuration-api-keys-path (configuration)
  "Return Autolith's private OpenAI-compatible API-key pathname."
  (merge-pathnames "api-keys.sexp" (config :state-root configuration)))

(-> configuration-fireworks-auth-path (configuration) pathname)
(defun configuration-fireworks-auth-path (configuration)
  "Return Autolith's private Fireworks API key credential pathname."
  (merge-pathnames "fireworks-auth.sexp" (config :state-root configuration)))

(-> configuration-opencode-auth-path (configuration) pathname)
(defun configuration-opencode-auth-path (configuration)
  "Return Autolith's private OpenCode API key credential pathname."
  (merge-pathnames "opencode-auth.sexp" (config :state-root configuration)))

(-> configuration-provider-model-cache-path (configuration) pathname)
(defun configuration-provider-model-cache-path (configuration)
  "Return the private cache of successful provider model discovery results."
  (merge-pathnames "provider-models.sexp" (config :state-root configuration)))

(-> configuration-journal-path (configuration) pathname)
(defun configuration-journal-path (configuration)
  "Return the append-only live-mutation journal pathname."
  (merge-pathnames "mutations.sexp" (config :state-root configuration)))

(-> configuration-wire-effort (configuration) string)
(defun configuration-wire-effort (configuration)
  "Return the provider effort, mapping user-visible Ultra to wire-level Max."
  (if (string= (config :reasoning-effort configuration) "ultra")
      "max"
      (config :reasoning-effort configuration)))

(-> configuration-grok-wire-effort (configuration) string)
(defun configuration-grok-wire-effort (configuration)
  "Return the Grok provider effort, clamped to the low, medium, high scale."
  (let ((effort (config :reasoning-effort configuration)))
    (cond
      ((member effort '("none" "low") :test #'string=)
       "low")
      ((string= effort "medium")
       "medium")
      (t
       "high"))))

(-> configuration-fireworks-wire-effort (configuration) string)
(defun configuration-fireworks-wire-effort (configuration)
  "Return the Fireworks provider effort, clamped to the low, medium, high scale."
  (let ((effort (config :reasoning-effort configuration)))
    (cond
      ((member effort '("none" "low") :test #'string=)
       "low")
      ((string= effort "medium")
       "medium")
      (t
       "high"))))

(-> make-identifier () string)
(defun make-identifier ()
  "Return a process-independent identifier suitable for conversations and requests."
  (flet ((fallback ()
           (format nil "~36R-~16,'0X"
                   (get-universal-time)
                   (random (ash 1 64)))))
    (handler-case
        (platform-unique-identifier *platform*)
      (error ()
        (fallback)))))
