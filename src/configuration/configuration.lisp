(in-package #:autolith)

;;;; -- Configuration Access --

;; The setting protocol, the configuration object, and CONFIG come from
;; setinka. Autolith defines its setting kinds and settings below, persists
;; durable values through the preferences store, and keeps its constructors,
;; which name validation deferral after the provider registry it waits for.

;;;; -- Setting Kinds Specific to Autolith --

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

(defclass site-config-directory-setting (directory-setting)
  ()
  (:default-initargs :type '(option pathname))
  (:documentation "An optional existing canonical site configuration directory."))

(defclass optional-directory-setting (directory-setting)
  ()
  (:default-initargs :type '(option pathname))
  (:documentation "An optional directory pathname; empty text means none."))

(defclass optional-absolute-file-setting (absolute-file-setting)
  ()
  (:default-initargs :type '(option pathname))
  (:documentation "An optional absolute file pathname; empty text means none."))

(defmethod setting-coerce ((setting web-search-mode-setting) (value string) configuration)
  "Read the mode name in lower case."
  (declare (ignore setting configuration))
  (string-downcase value))

(defmethod setting-coerce ((setting directory-setting) value configuration)
  "Store directory designators in directory form."
  (declare (ignore setting configuration))
  (uiop:ensure-directory-pathname
   (if (stringp value) (parse-namestring value) value)))

(defmethod setting-coerce ((setting site-config-directory-setting) value configuration)
  "Resolve an existing site configuration directory to its canonical pathname."
  (declare (ignore setting))
  (when value
    (let ((pathname (uiop:ensure-directory-pathname
                     (if (stringp value) (parse-namestring value) value))))
      (configuration--resolve-site-config-root pathname))))

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

(defmethod setting-coerce ((setting optional-directory-setting) value configuration)
  "Store a directory designator in directory form, reading empty text as none."
  (declare (ignore setting configuration))
  (unless (or (null value) (equal value ""))
    (uiop:ensure-directory-pathname
     (if (stringp value) (parse-namestring value) value))))

(defmethod setting-coerce ((setting optional-absolute-file-setting) value configuration)
  "Anchor a relative file pathname to the process directory, reading empty text as none."
  (declare (ignore setting configuration))
  (unless (or (null value) (equal value ""))
    (configuration--absolute-file-pathname
     (if (stringp value) (parse-namestring value) value))))

(defmethod setting-coerce ((setting management-transport-setting) (value string) configuration)
  "Read the transport name case-insensitively."
  (declare (ignore setting configuration))
  (intern (string-upcase value) '#:keyword))

(defmethod setting-validate ((setting management-transport-setting) value configuration)
  "Require a transport the host supports."
  (call-next-method)
  (when (and (eq value ':unix)
             (not (platform-supports-p *platform* ':local-sockets)))
    (setting-reject setting value
                    "AUTOLITH_MANAGEMENT_REPL_TRANSPORT=unix needs filesystem sockets, which this platform lacks; use tcp."))
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

(define-setting :site-config-root (site-config-directory-setting)
  :label "Site config root"
  :group :paths
  :documentation "The optional site-managed configuration root."
  :scope :process
  :environment "AUTOLITH_SITE_CONFIG_ROOT"
  :default nil)

(define-setting :data-root (directory-setting)
  :label "Data root"
  :group :paths
  :documentation "The root for durable user data such as conversations."
  :visible-p nil
  :default (lambda (configuration)
             (declare (ignore configuration))
             (platform-application-root *platform* ':data)))

(define-setting :active-image-core (absolute-file-setting)
  :label "Active image core"
  :group :paths
  :documentation "The preloaded active image that starts fresh Autolith processes."
  :environment "AUTOLITH_ACTIVE_CORE"
  :visible-p nil
  :default (lambda (configuration)
             (merge-pathnames "active/autolith-active.core"
                              (config :data-root configuration))))

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

(define-setting :model (choice-setting)
  :label "Model"
  :group :model
  :documentation "The provider model identifier."
  :type 'non-empty-string
  :scope :durable
  :environment "AUTOLITH_MODEL"
  :deferrable-p t
  :options (lambda (configuration)
             (declare (ignore configuration))
             (copy-list *supported-models*))
  :validator 'configuration--model-problem
  :default (lambda (configuration)
             (declare (ignore configuration))
             *default-model*))

(define-setting :reasoning-effort (choice-setting)
  :label "Reasoning effort"
  :group :model
  :documentation "The user-visible reasoning effort."
  :type 'non-empty-string
  :scope :durable
  :environment "AUTOLITH_REASONING_EFFORT"
  :deferrable-p t
  :depends-on '(:model)
  :options (lambda (configuration)
             (configuration--reasoning-efforts-for (config :model configuration)))
  :validator 'configuration--reasoning-effort-problem
  :default (lambda (configuration)
             (declare (ignore configuration))
             *default-reasoning-effort*))

(define-setting :openrouter-provider-routing (string-setting)
  :label "OpenRouter provider routing"
  :group :model
  :documentation "OpenRouter provider preferences as a JSON object keyed by wire model name, with an optional wildcard entry for other models."
  :scope :durable
  :environment "AUTOLITH_OPENROUTER_PROVIDER_ROUTING"
  :validator 'configuration--openrouter-provider-routing-problem
  :default "")

(define-setting :codex-fast-mode-p (boolean-setting)
  :label "Codex Fast mode"
  :group :model
  :documentation "Whether Codex requests opt in to the Fast service tier, which is faster and counts more heavily against plan usage."
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

(define-setting :terminal-theme (choice-setting)
  :label "Theme"
  :group :terminal
  :documentation "The presentation theme: autolith, or almighty for yellow on blue."
  :scope :durable
  :type '(member :autolith :almighty)
  :options '(:autolith :almighty)
  :default ':autolith)

(define-setting :boot-screen-p (boolean-setting)
  :label "Boot screen"
  :group :terminal
  :documentation "Whether a fullscreen start plays the boot screen before opening the listener."
  :scope :durable
  :environment "AUTOLITH_BOOT_SCREEN"
  :default t)

(define-setting :boot-screen-seconds (real-setting)
  :label "Boot screen duration"
  :group :terminal
  :documentation "Total seconds of the boot animation, shared evenly by its phases."
  :scope :durable
  :minimum 0
  :maximum 60
  :environment "AUTOLITH_BOOT_DURATION"
  :default 3.5)

(define-setting :boot-screen-linger-p (boolean-setting)
  :label "Boot screen waits for Space"
  :group :terminal
  :documentation "Whether the fullscreen boot screen waits for Space before opening the listener."
  :scope :durable
  :environment "AUTOLITH_BOOT_LINGER"
  :default t)

(define-setting :boot-screen-tip-seconds (integer-setting)
  :label "Boot tip rotation"
  :group :terminal
  :documentation "Seconds between startup tips while the boot screen waits."
  :scope :durable
  :minimum 1
  :maximum 3600
  :environment "AUTOLITH_BOOT_TIP_SECONDS"
  :default 10)

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

(define-setting :slynk-directory (optional-directory-setting)
  :label "Slynk directory"
  :group :editor
  :documentation "The slynk/ directory of the sly installation whose Slynk serves this image."
  :scope :process
  :environment "AUTOLITH_SLYNK_DIRECTORY"
  :default nil)

(define-setting :slynk-port-file (optional-absolute-file-setting)
  :label "Slynk port file"
  :group :editor
  :documentation "The file that receives the Slynk port, or the start failure, for an editor."
  :scope :process
  :environment "AUTOLITH_SLYNK_PORT_FILE"
  :default nil)

(define-setting :emacs-server-socket (optional-absolute-file-setting)
  :label "Emacs server socket"
  :group :editor
  :documentation "The Emacs server socket the emacs.* tools talk to; without it they are absent."
  :scope :process
  :environment "AUTOLITH_EMACS_SERVER"
  :default nil)


;;;; -- Construction --

(-> make-configuration (&rest t) configuration)
(defun make-configuration (&rest overrides &key (provider-validation-p t) &allow-other-keys)
  "Return a configuration holding only OVERRIDES over the setting defaults.

Neither the environment nor the preferences file is consulted, which makes
the result deterministic for tests and child agents. Overrides apply in
setting definition order, so a model override precedes its reasoning effort.
PROVIDER-VALIDATION-P NIL defers model checks until the provider registry is
known."
  (setinka:make-configuration
   :overrides (configuration--overrides overrides '(:provider-validation-p))
   :validation-deferred-p (not provider-validation-p)))

(-> configuration-copy (configuration &rest t) configuration)
(defun configuration-copy
    (configuration &rest overrides
     &key (provider-validation-p
           (not (configuration-validation-deferred-p configuration)))
     &allow-other-keys)
  "Return a copy of CONFIGURATION with OVERRIDES applied in setting order.

PROVIDER-VALIDATION-P defaults to the original's; passing NIL lets a copy name
a model the registry does not serve. Listeners stay with the original; a child
or frame reacts to its own changes."
  (setinka:configuration-copy
   configuration
   :overrides (configuration--overrides overrides '(:provider-validation-p))
   :validation-deferred-p (not provider-validation-p)))

(-> configuration-create (&rest t) configuration)
(defun configuration-create
    (&rest overrides &key defer-provider-validation-p (durable-p t) &allow-other-keys)
  "Create the process configuration from OVERRIDES, the environment, and preferences.

Each setting takes the first source that supplies it: an explicit override,
its environment variable, the durable preferences file for durable settings,
then its default. A durable value that no longer validates is dropped rather
than applied. DEFER-PROVIDER-VALIDATION-P leaves model checks to
CONFIGURATION-VALIDATE-DEFERRED once executable user initialization has
registered providers. DURABLE-P NIL skips the preferences file."
  (setinka:configuration-load
   :overrides (configuration--overrides
               overrides '(:defer-provider-validation-p :durable-p))
   :validation-deferred-p defer-provider-validation-p
   :durable-p durable-p))

(-> configuration--overrides (list list) list)
(defun configuration--overrides (arguments options)
  "Return the setting overrides in ARGUMENTS without the constructor OPTIONS."
  (loop for (name value) on arguments by #'cddr
        unless (member name options)
          append (list name value)))


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
  "Return the Grok effort, preserving Grok 4.7's four-level reasoning scale."
  (let ((effort (config :reasoning-effort configuration)))
    (cond
      ((string= (config :model configuration) "grok-4.7")
       effort)
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
  "Return a random version 4 UUID, unguessable and unique across processes.

Its octets come from ironclad's operating-system random source on every host."
  (let ((octets (random-data 16)))
    (setf (aref octets 6) (logior #x40 (logand (aref octets 6) #x0F))
          (aref octets 8) (logior #x80 (logand (aref octets 8) #x3F)))
    (format nil "~(~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}~)"
            (coerce (subseq octets 0 4) 'list)
            (coerce (subseq octets 4 6) 'list)
            (coerce (subseq octets 6 8) 'list)
            (coerce (subseq octets 8 10) 'list)
            (coerce (subseq octets 10 16) 'list))))

(-> configuration--resolve-site-config-root ((option pathname)) (option pathname))
(defun configuration--resolve-site-config-root (site-config-root)
  "Return SITE-CONFIG-ROOT as an existing canonical absolute directory."
  (when site-config-root
    (unless (uiop:absolute-pathname-p site-config-root)
      (error 'configuration-error
             :message (format nil "Site configuration root ~S must be absolute."
                              (namestring site-config-root))))
    (handler-case
        (let ((directory
                (uiop:directory-exists-p
                 (uiop:ensure-pathname site-config-root
                                       :ensure-directory t
                                       :want-non-wild t))))
          (unless directory
            (error 'configuration-error
                   :message (format nil "Site configuration root ~S does not exist."
                                    (namestring site-config-root))))
          (uiop:ensure-directory-pathname
           (platform-truename *platform* directory)))
      (configuration-error (condition)
        (error condition))
      (serious-condition (cause)
        (error 'configuration-error
               :message (format nil "Could not resolve site configuration root ~S: ~A"
                                (namestring site-config-root) cause))))))

(-> configuration-site-init-path (configuration) (option pathname))
(defun configuration-site-init-path (configuration)
  "Return the site-authored Lisp initialization pathname, when configured."
  (let ((root (config :site-config-root configuration)))
    (and root (merge-pathnames "init.lisp" root))))
