(in-package #:autolith)

;;;; -- Defaults --

(defparameter *autolith-version* "0.33.8"
  "The user-visible Autolith version.")

(defparameter *default-model* "gpt-5.6-sol"
  "The default model requested from the subscription provider.")

(defparameter *default-reasoning-effort* "ultra"
  "The user-visible default reasoning effort.")

(defparameter *codex-responses-endpoint*
  "https://chatgpt.com/backend-api/codex/responses"
  "The current ChatGPT Codex Responses endpoint.")

(defparameter *openai-oauth-token-endpoint*
  "https://auth.openai.com/oauth/token"
  "The OpenAI OAuth token endpoint.")

(defparameter *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "The public OAuth client identifier used by Codex-compatible clients.")

;; The subscription proxy serving Grok Build sessions, read from grok-build
;; reference commit 47348d13 (crates/codegen/xai-grok-env).
(defparameter *grok-responses-endpoint*
  "https://cli-chat-proxy.grok.com/v1/responses"
  "The current Grok subscription Responses endpoint.")

(defparameter *grok-oauth-issuer* "https://auth.x.ai"
  "The xAI OAuth issuer serving Grok subscription authentication.")

(defparameter *grok-oauth-client-id* "b1a00492-073a-47ea-816f-4c329264a828"
  "The public OAuth client identifier used by Grok Build compatible clients.")

;; The proxy gates requests on this protocol revision and rejects requests
;; without it as HTTP 426. Autolith implements the wire dialect of this
;; grok-build release, reference commit 5163763e, while reporting its own
;; identity through User-Agent and x-grok-client-identifier.
(defparameter *grok-client-protocol-version* "1.0.4"
  "The grok-build release whose Grok proxy wire protocol Autolith implements.")

;; The public Fireworks Responses API, verified against
;; accounts/fireworks/models/kimi-k3 on 2026-08-06: the endpoint accepts the
;; standard streaming Responses dialect with function tools, reasoning
;; effort, store=false, and prompt_cache_key.
(defparameter *fireworks-responses-endpoint*
  "https://api.fireworks.ai/inference/v1/responses"
  "The Fireworks AI Responses API endpoint.")

(defparameter *default-fireworks-model* "accounts/fireworks/models/kimi-k3"
  "The Fireworks model identifier offered by default.")

;; The public Anthropic Messages API, verified against claude-haiku-4-5 on
;; 2026-08-08: the endpoint accepts the streaming Messages dialect with a
;; top-level system field, strictly alternating user/assistant messages,
;; input_schema function tools, and tool_choice {"type": "auto"}.
(defparameter *anthropic-messages-endpoint*
  "https://api.anthropic.com/v1/messages"
  "The Anthropic Messages API endpoint.")

;; The public OpenCode Chat Completions API, verified on 2026-08-12: the
;; endpoint accepts the standard streaming OpenAI Chat Completions dialect
;; with function tools and dynamic model discovery.
(defparameter *opencode-chat-completions-endpoint*
  "https://opencode.ai/zen/go/v1/chat/completions"
  "The OpenCode Chat Completions API endpoint.")

(defparameter *opencode-models-endpoint*
  "https://opencode.ai/zen/go/v1/models"
  "The OpenCode models endpoint used for dynamic model discovery.")

(defparameter *opencode-models-environment-variable*
  "AUTOLITH_OPENCODE_MODELS_ENDPOINT"
  "The environment variable overriding OpenCode dynamic model discovery.")

(-> opencode-models-endpoint () string)
(defun opencode-models-endpoint ()
  "Return the configured OpenCode models endpoint."
  (let ((override (uiop:getenv *opencode-models-environment-variable*)))
    (if (non-empty-string-p override)
        override
        *opencode-models-endpoint*)))

(defparameter *anthropic-models-endpoint*
  "https://api.anthropic.com/v1/models"
  "The Anthropic models endpoint used to validate API keys.")

(defparameter *anthropic-api-version* "2023-06-01"
  "The Anthropic API version header sent with every request.")

(defparameter *grok-oauth-scopes*
  '("openid" "profile" "email" "offline_access"
    "grok-cli:access" "api:access"
    "conversations:read" "conversations:write"
    "workspaces:read" "workspaces:write")
  "The OAuth scopes requested for Grok subscription access.")

(defparameter *supported-reasoning-efforts*
  '("none" "low" "medium" "high" "xhigh" "max" "ultra")
  "Reasoning effort names accepted by Autolith configuration.")

(defparameter *supported-web-search-modes*
  '("cached" "indexed" "live" "disabled")
  "Standalone web search modes accepted by Autolith configuration.")

(defparameter *supported-models*
  '("gpt-5.6-sol" "gpt-5.6-luna" "gpt-5.6-terra" "grok-4.5"
    "accounts/fireworks/models/kimi-k3")
  "The model identifiers offered by the interactive model picker.")

;; GPT window sizes read from the live Codex model catalog on 2026-07-19 and
;; confirmed in Codex reference commit 0fb559f0f6e231a88ac02ea002d3ecd248e2b515.
;; The Grok window comes from default_models.json in grok-build reference
;; commit 47348d13.
(defparameter *model-context-windows*
  '(("gpt-5.6-sol"   . 272000)
    ("gpt-5.6-luna"  . 272000)
    ("gpt-5.6-terra" . 272000)
    ("grok-4.5"      . 500000)
    ("accounts/fireworks/models/kimi-k3" . 1048576))
  "Provider context window sizes in tokens for known models.")

(-> model-family (string) keyword)
(defun model-family (model)
  "Return the provider family serving MODEL.

Registered providers take precedence over built-in model fallbacks so
configuration can be created before executable user initialization loads."
  (or (and (fboundp 'provider-model-family)
           (provider-model-family model))
      (cond
        ((uiop:string-prefix-p "grok" model)
         ':grok)
        ((uiop:string-prefix-p "accounts/fireworks/models/" model)
         ':fireworks)
        (t
         ':codex))))

(-> configuration--reasoning-efforts-for (string) list)
(defun configuration--reasoning-efforts-for (model)
  "Return the reasoning efforts supported by MODEL, or the global defaults."
  (or (and (fboundp 'provider-model-reasoning-efforts-for)
           (provider-model-reasoning-efforts-for model))
      (copy-list *supported-reasoning-efforts*)))

(-> configuration--model-supported-p (string) boolean)
(defun configuration--model-supported-p (model)
  "Return true when an effective provider registration serves MODEL."
  (not (null (member model *supported-models* :test #'string=))))

(defparameter *default-context-window* 272000
  "The conservative context window assumed for unknown models.")

(defparameter *default-compaction-threshold-percent* 80
  "The context window percentage that triggers compaction.")


(-> environment-directory (string pathname) pathname)
(defun environment-directory (variable fallback)
  "Return directory VARIABLE as a pathname, or FALLBACK when it is unset."
  (let ((value (uiop:getenv variable)))
    (uiop:ensure-directory-pathname
     (if (non-empty-string-p value)
         (pathname value)
         fallback))))

(-> configuration--default-config-root () pathname)
(defun configuration--default-config-root ()
  "Return Autolith's default XDG configuration directory."
  (merge-pathnames
   "autolith/"
   (environment-directory "XDG_CONFIG_HOME"
                          (merge-pathnames ".config/"
                                           (user-homedir-pathname)))))

(-> configuration--default-grok-bootstrap-path () pathname)
(defun configuration--default-grok-bootstrap-path ()
  "Return the default Grok Build auth.json bootstrap pathname."
  (merge-pathnames
   "auth.json"
   (environment-directory "GROK_HOME"
                          (merge-pathnames ".grok/"
                                           (user-homedir-pathname)))))


;;;; -- Configuration Object --

(defclass configuration ()
  ((source-root
    :initarg :source-root
    :reader configuration-source-root
    :type pathname
    :documentation "The tracked Autolith source root.")
   (working-directory
    :initarg :working-directory
    :reader configuration-working-directory
    :type pathname
    :documentation "The workspace visible to the agent and Lisp worker.")
   (config-root
    :initarg :config-root
    :initform (configuration--default-config-root)
    :reader configuration-config-root
    :type pathname
    :documentation "The root for user-editable Autolith configuration.")
   (data-root
    :initarg :data-root
    :reader configuration-data-root
    :type pathname
    :documentation "The root for durable user data such as conversations.")
   (state-root
    :initarg :state-root
    :reader configuration-state-root
    :type pathname
    :documentation "The root for mutable runtime state such as queues and journals.")
   (cache-root
    :initarg :cache-root
    :reader configuration-cache-root
    :type pathname
    :documentation "The root for replaceable caches and temporary artifacts.")
   (codex-auth-path
    :initarg :codex-auth-path
    :reader configuration-codex-auth-path
    :type pathname
    :documentation "The optional Codex OAuth bootstrap file.")
   (grok-bootstrap-auth-path
    :initarg :grok-bootstrap-auth-path
    :initform (configuration--default-grok-bootstrap-path)
    :reader configuration-grok-bootstrap-auth-path
    :type pathname
    :documentation "The optional Grok Build OAuth bootstrap file.")
   (model
    :initarg :model
    :reader configuration-model
    :type non-empty-string
    :documentation "The provider model identifier.")
   (reasoning-effort
    :initarg :reasoning-effort
    :reader configuration-reasoning-effort
    :type non-empty-string
    :documentation "The user-visible reasoning effort.")
   (immutable-p
    :initarg :immutable-p
    :initform nil
    :reader configuration-immutable-p
    :type boolean
    :documentation "Whether mutation-capable active-image tools are disabled.")
   (web-search-mode
    :initarg :web-search-mode
    :initform "cached"
    :reader configuration-web-search-mode
    :type non-empty-string
    :documentation "The provider web search mode: cached, indexed, live, or disabled.")
   (context-window
    :initarg :context-window
    :initform *default-context-window*
    :reader configuration-context-window
    :type (integer 1)
    :documentation "The provider context window in tokens for the model.")
   (compaction-threshold-percent
    :initarg :compaction-threshold-percent
    :initform *default-compaction-threshold-percent*
    :reader configuration-compaction-threshold-percent
    :type (integer 1 95)
    :documentation "The context window percentage that triggers compaction.")
   (provider-endpoint
    :initarg :provider-endpoint
    :reader configuration-provider-endpoint
    :type non-empty-string
    :documentation "The streaming Responses endpoint."))
  (:documentation "Immutable paths and model choices for one Autolith process."))

(-> configuration--provider-endpoint-for (string) string)
(defun configuration--provider-endpoint-for (model)
  "Return MODEL's environment override, registered endpoint, or family default.

AUTOLITH_PROVIDER_ENDPOINT overrides the Codex family endpoint,
AUTOLITH_GROK_PROVIDER_ENDPOINT overrides the Grok family endpoint,
AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT overrides the Fireworks family endpoint, and
AUTOLITH_OPENCODE_PROVIDER_ENDPOINT overrides the OpenCode family endpoint."
  (let* ((family (model-family model))
         (override
           (case family
             (:codex
              (uiop:getenv "AUTOLITH_PROVIDER_ENDPOINT"))
             (:grok
              (uiop:getenv "AUTOLITH_GROK_PROVIDER_ENDPOINT"))
             (:fireworks
              (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT"))
             (:opencode
              (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"))))
         (registered
           (and (fboundp 'provider-model-endpoint)
                (provider-model-endpoint model))))
    (or (and (non-empty-string-p override) override)
        registered
        (case family
          (:codex
           *codex-responses-endpoint*)
          (:grok
           *grok-responses-endpoint*)
          (:fireworks
           *fireworks-responses-endpoint*)
          (:opencode
           *opencode-chat-completions-endpoint*)
          (otherwise
           (error 'configuration-error
                  :message
                  (format nil
                          "Registered provider ~A did not declare an endpoint for model ~A."
                          (or (and (fboundp 'provider-model-provider-name)
                                   (provider-model-provider-name model))
                              family)
                          model)))))))

(-> configuration--context-window-for (string) integer)
(defun configuration--context-window-for (model)
  "Return MODEL's context window from the environment, registry, or fallback."
  (let ((override (uiop:getenv "AUTOLITH_CONTEXT_WINDOW")))
    (or (and (non-empty-string-p override)
             (let ((parsed (parse-integer override :junk-allowed t)))
               (and parsed (plusp parsed) parsed)))
        (and (fboundp 'provider-model-context-window-for)
             (provider-model-context-window-for model))
        (rest (assoc model *model-context-windows* :test #'string=))
        *default-context-window*)))

(-> configuration--compaction-threshold () integer)
(defun configuration--compaction-threshold ()
  "Return the validated compaction threshold percentage from the environment."
  (let ((override (uiop:getenv "AUTOLITH_COMPACTION_THRESHOLD")))
    (if (non-empty-string-p override)
        (let ((parsed (parse-integer override :junk-allowed t)))
          (unless (and parsed (<= 1 parsed 95))
            (error 'configuration-error
                   :message (format nil "AUTOLITH_COMPACTION_THRESHOLD must be ~
                                         a percentage between 1 and 95, not ~S."
                                    override)))
          parsed)
        *default-compaction-threshold-percent*)))

(-> configuration-compaction-token-limit (configuration) integer)
(defun configuration-compaction-token-limit (configuration)
  "Return the token count at which CONFIGURATION compacts the conversation."
  (floor (* (configuration-context-window configuration)
            (configuration-compaction-threshold-percent configuration))
         100))

(-> configuration-create (&key
                           (:source-root (option pathname))
                           (:working-directory (option pathname))
                           (:model (option string))
                           (:reasoning-effort (option string))
                           (:immutable-p boolean)
                           (:defer-provider-validation-p boolean))
    configuration)
(defun configuration-create
    (&key source-root working-directory model reasoning-effort immutable-p
          defer-provider-validation-p)
  "Create runtime configuration from explicit values and the environment.

Provider-specific validation can be deferred until executable user
initialization registers the selected model."
  (let* ((home (user-homedir-pathname))
           (config-home (environment-directory
                         "XDG_CONFIG_HOME"
                         (merge-pathnames ".config/" home)))
           (data-home (environment-directory
                     "XDG_DATA_HOME"
                     (merge-pathnames ".local/share/" home)))
           (state-home (environment-directory
                        "XDG_STATE_HOME"
                        (merge-pathnames ".local/state/" home)))
           (cache-home (environment-directory
                        "XDG_CACHE_HOME"
                        (merge-pathnames ".cache/" home)))
           (codex-home (environment-directory
                        "CODEX_HOME"
                        (merge-pathnames ".codex/" home)))
           (environment-source-root (uiop:getenv "AUTOLITH_SOURCE_ROOT"))
           (selected-model (or model (uiop:getenv "AUTOLITH_MODEL") *default-model*))
           (selected-effort (or reasoning-effort
                                (uiop:getenv "AUTOLITH_REASONING_EFFORT")
                                *default-reasoning-effort*))
           (selected-web-search (let ((mode (uiop:getenv "AUTOLITH_WEB_SEARCH")))
                                  (if (non-empty-string-p mode)
                                      (string-downcase mode)
                                      "cached"))))
    (unless defer-provider-validation-p
      (unless (member selected-effort
                      (configuration--reasoning-efforts-for selected-model)
                      :test #'string=)
        (error 'configuration-error
               :message (format nil "Unsupported reasoning effort ~S." selected-effort))))
    (unless (member selected-web-search *supported-web-search-modes*
                    :test #'string=)
      (error 'configuration-error
             :message (format nil "Unsupported web search mode ~S."
                              selected-web-search)))
    (make-instance 'configuration
                   :source-root (uiop:ensure-directory-pathname
                                 (or source-root
                                     (and (non-empty-string-p environment-source-root)
                                          (pathname environment-source-root))
                                     (asdf:system-source-directory :autolith)))
                   :working-directory (uiop:ensure-directory-pathname
                                       (or working-directory (uiop:getcwd)))
                   :config-root (merge-pathnames "autolith/" config-home)
                   :data-root (merge-pathnames "autolith/" data-home)
                   :state-root (merge-pathnames "autolith/" state-home)
                   :cache-root (merge-pathnames "autolith/" cache-home)
                   :codex-auth-path (merge-pathnames "auth.json" codex-home)
                   :grok-bootstrap-auth-path
                   (configuration--default-grok-bootstrap-path)
                   :model selected-model
                   :reasoning-effort selected-effort
                   :immutable-p immutable-p
                   :web-search-mode selected-web-search
                   :context-window (configuration--context-window-for
                                    selected-model)
                   :compaction-threshold-percent
                   (configuration--compaction-threshold)
                   :provider-endpoint (configuration--provider-endpoint-for
                                       selected-model))))

(-> configuration--clone
    (configuration &key (:working-directory (option pathname))
                   (:model (option string))
                   (:reasoning-effort (option string))
                   (:immutable-p boolean)
                   (:web-search-mode (option string)))
    configuration)
(defun configuration--clone
    (configuration
     &key working-directory model
       (reasoning-effort nil reasoning-effort-supplied-p)
       (immutable-p nil immutable-p-supplied-p)
       (web-search-mode nil web-search-mode-supplied-p))
  "Copy CONFIGURATION, replacing only supplied workspace or model choices.

Selecting a different model recomputes its context window and preserves the old
reasoning effort only when that effort is supported by the selected model."
  (let* ((selected-model (or model (configuration-model configuration)))
         (supported-efforts (configuration--reasoning-efforts-for selected-model))
         (selected-effort
           (if reasoning-effort-supplied-p
               reasoning-effort
               (let ((previous-effort (configuration-reasoning-effort configuration)))
                 (if (member previous-effort supported-efforts :test #'string=)
                     previous-effort
                     (first supported-efforts)))))
         (effective-working-directory
           (or working-directory (configuration-working-directory configuration))))
    (unless (member selected-effort supported-efforts :test #'string=)
      (error 'configuration-error
             :message (format nil "Unsupported reasoning effort ~S for model ~A."
                              selected-effort selected-model)))
    (make-instance 'configuration
                   :source-root (configuration-source-root configuration)
                   :working-directory effective-working-directory
                   :config-root (configuration-config-root configuration)
                   :data-root (configuration-data-root configuration)
                   :state-root (configuration-state-root configuration)
                   :cache-root (configuration-cache-root configuration)
                   :codex-auth-path (configuration-codex-auth-path configuration)
                   :grok-bootstrap-auth-path
                   (configuration-grok-bootstrap-auth-path configuration)
                   :model selected-model
                   :reasoning-effort selected-effort
                   :immutable-p (if immutable-p-supplied-p
                                    immutable-p
                                    (configuration-immutable-p configuration))
                   :web-search-mode
                   (if web-search-mode-supplied-p
                       web-search-mode
                       (configuration-web-search-mode configuration))
                   :context-window (if model
                                       (configuration--context-window-for model)
                                       (configuration-context-window
                                        configuration))
                   :compaction-threshold-percent
                   (configuration-compaction-threshold-percent configuration)
                   :provider-endpoint
                   (if model
                       (configuration--provider-endpoint-for model)
                       (configuration-provider-endpoint configuration)))))

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
  (let ((previous (configuration-working-directory configuration)))
    (handler-case
        (let* ((candidate
                 (uiop:ensure-pathname
                  (configuration--expanded-working-directory location)
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
          (uiop:ensure-directory-pathname (truename directory)))
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

(-> configuration-with-working-directory
    (configuration (or pathname string))
    configuration)
(defun configuration-with-working-directory (configuration location)
  "Copy CONFIGURATION with its workspace changed to existing directory LOCATION."
  (configuration--clone
   configuration
   :working-directory
   (configuration--resolve-working-directory configuration location)))

(-> configuration-with-reasoning-effort (configuration string) configuration)
(defun configuration-with-reasoning-effort (configuration reasoning-effort)
  "Copy CONFIGURATION with only its REASONING-EFFORT changed."
  (unless (member reasoning-effort
                  (configuration--reasoning-efforts-for
                   (configuration-model configuration))
                  :test #'string=)
    (error 'configuration-error
           :message
           (format nil "Unsupported reasoning effort ~S for model ~A."
                   reasoning-effort
                   (configuration-model configuration))))
  (configuration--clone configuration :reasoning-effort reasoning-effort))

(-> configuration-with-model (configuration string) configuration)
(defun configuration-with-model (configuration model)
  "Copy CONFIGURATION with only its MODEL changed."
  (unless (configuration--model-supported-p model)
    (error 'configuration-error
           :message (format nil "Unsupported model ~S. The choices are ~{~A~^, ~}."
                            model
                            *supported-models*)))
  (configuration--clone configuration :model model))

(-> configuration-ensure-directories (configuration) configuration)
(defun configuration-ensure-directories (configuration)
  "Create CONFIGURATION's private config, data, state, and cache directories."
  (dolist (directory (list (configuration-config-root configuration)
                            (configuration-data-root configuration)
                            (configuration-state-root configuration)
                            (configuration-cache-root configuration)))
    (ensure-directories-exist directory))
  configuration)

(-> configuration-conversation-root (configuration) pathname)
(defun configuration-conversation-root (configuration)
  "Return the directory containing conversation identities and chunk logs."
  (merge-pathnames "conversations/" (configuration-data-root configuration)))

(-> configuration-inference-root (configuration) pathname)
(defun configuration-inference-root (configuration)
  "Return the directory containing inference frame trace conversations."
  (merge-pathnames "inferences/" (configuration-data-root configuration)))

(-> configuration-conversation-identifier-migration-path (configuration) pathname)
(defun configuration-conversation-identifier-migration-path (configuration)
  "Return the durable legacy conversation identifier migration record."
  (merge-pathnames "conversation-identifier-migration.sexp"
                   (configuration-state-root configuration)))

(-> configuration-user-init-path (configuration) pathname)
(defun configuration-user-init-path (configuration)
  "Return the user-authored Lisp initialization pathname."
  (merge-pathnames "init.lisp" (configuration-config-root configuration)))

(-> configuration-directory-scopes-path (configuration) pathname)
(defun configuration-directory-scopes-path (configuration)
  "Return the user-owned directory-scope trust manifest pathname."
  (merge-pathnames "directory-scopes.sexp"
                   (configuration-config-root configuration)))

(-> configuration-memory-path (configuration) pathname)
(defun configuration-memory-path (configuration)
  "Return the append-only persistent memory pathname."
  (merge-pathnames "memories.sexp" (configuration-data-root configuration)))

(-> configuration-papercut-path (configuration) pathname)
(defun configuration-papercut-path (configuration)
  "Return the append-only persistent papercut pathname."
  (merge-pathnames "papercuts.sexp" (configuration-data-root configuration)))

(-> configuration-agenda-path (configuration) pathname)
(defun configuration-agenda-path (configuration)
  "Return the atomic workspace-agenda pathname."
  (merge-pathnames "agendas.sexp" (configuration-data-root configuration)))


(-> configuration-image-commit-root (configuration) pathname)
(defun configuration-image-commit-root (configuration)
  "Return the directory containing immutable private image commits."
  (merge-pathnames "image-commits/" (configuration-data-root configuration)))

(-> configuration-mutation-history-root (configuration) pathname)
(defun configuration-mutation-history-root (configuration)
  "Return the private Git repository backing durable mutation snapshots."
  (merge-pathnames "mutation-history/"
                   (configuration-state-root configuration)))

(-> configuration-lisp-image-root (configuration) pathname)
(defun configuration-lisp-image-root (configuration)
  "Return the directory containing immutable saved Lisp worker images."
  (merge-pathnames "lisp-images/" (configuration-data-root configuration)))

(-> configuration-current-image-commit-path (configuration) pathname)
(defun configuration-current-image-commit-path (configuration)
  "Return the atomic pointer to the image commit used by normal startup."
  (merge-pathnames "current-image-commit.sexp"
                   (configuration-state-root configuration)))

(-> configuration-preferences-path (configuration) pathname)
(defun configuration-preferences-path (configuration)
  "Return the atomic global preferences pathname."
  (merge-pathnames "preferences.sexp" (configuration-state-root configuration)))

(-> configuration-project-adaptation-offers-path (configuration) pathname)
(defun configuration-project-adaptation-offers-path (configuration)
  "Return the atomic per-project AUTOLITH.org offer-state pathname."
  (merge-pathnames "project-adaptation-offers.sexp"
                   (configuration-state-root configuration)))

(-> configuration-update-state-path (configuration) pathname)
(defun configuration-update-state-path (configuration)
  "Return the atomic cached release-availability state pathname."
  (merge-pathnames "update-state.sexp" (configuration-state-root configuration)))

(-> configuration-permissions-path (configuration) pathname)
(defun configuration-permissions-path (configuration)
  "Return the atomic persistent command-permission pathname."
  (merge-pathnames "permissions.sexp" (configuration-state-root configuration)))

(-> configuration-later-path (configuration) pathname)
(defun configuration-later-path (configuration)
  "Return the atomic deferred-input queue pathname."
  (merge-pathnames "later.sexp" (configuration-state-root configuration)))

(-> configuration-pending-inputs-path (configuration pathname) pathname)
(defun configuration-pending-inputs-path (configuration conversation-pathname)
  "Return one conversation's atomic unprocessed-input pathname."
  (merge-pathnames
   (make-pathname :name (pathname-name conversation-pathname) :type "sexp")
   (merge-pathnames "pending-inputs/"
                    (configuration-state-root configuration))))

(-> configuration-recovery-input-vault-path
    (configuration pathname)
    pathname)
(defun configuration-recovery-input-vault-path
    (configuration conversation-pathname)
  "Return one conversation's atomic recovered-input vault pathname."
  (merge-pathnames
   (make-pathname :name (pathname-name conversation-pathname) :type "sexp")
   (merge-pathnames "recovery-input-vault/"
                    (configuration-state-root configuration))))

(-> configuration-legacy-pending-inputs-path (configuration) pathname)
(defun configuration-legacy-pending-inputs-path (configuration)
  "Return the legacy process-global unprocessed-input pathname."
  (merge-pathnames "pending-inputs.sexp"
                   (configuration-state-root configuration)))

(-> configuration-plan-path (configuration string) pathname)
(defun configuration-plan-path (configuration workspace-identifier)
  "Return one workspace's atomic plan pathname."
  (merge-pathnames
   (make-pathname :name workspace-identifier :type "sexp")
   (merge-pathnames "plans/" (configuration-state-root configuration))))

(-> configuration-legacy-plan-path (configuration) pathname)
(defun configuration-legacy-plan-path (configuration)
  "Return the legacy process-global workspace plan pathname."
  (merge-pathnames "plan.sexp" (configuration-state-root configuration)))

(-> configuration-auth-path (configuration) pathname)
(defun configuration-auth-path (configuration)
  "Return Autolith's private provider credential pathname."
  (merge-pathnames "auth.sexp" (configuration-state-root configuration)))

(-> configuration-grok-auth-path (configuration) pathname)
(defun configuration-grok-auth-path (configuration)
  "Return Autolith's private Grok OAuth credential pathname."
  (merge-pathnames "grok-auth.sexp" (configuration-state-root configuration)))

(-> configuration-api-keys-path (configuration) pathname)
(defun configuration-api-keys-path (configuration)
  "Return Autolith's private OpenAI-compatible API-key pathname."
  (merge-pathnames "api-keys.sexp" (configuration-state-root configuration)))

(-> configuration-fireworks-auth-path (configuration) pathname)
(defun configuration-fireworks-auth-path (configuration)
  "Return Autolith's private Fireworks API key credential pathname."
  (merge-pathnames "fireworks-auth.sexp" (configuration-state-root configuration)))

(-> configuration-opencode-auth-path (configuration) pathname)
(defun configuration-opencode-auth-path (configuration)
  "Return Autolith's private OpenCode API key credential pathname."
  (merge-pathnames "opencode-auth.sexp" (configuration-state-root configuration)))

(-> configuration-provider-model-cache-path (configuration) pathname)
(defun configuration-provider-model-cache-path (configuration)
  "Return the private cache of successful provider model discovery results."
  (merge-pathnames "provider-models.sexp" (configuration-state-root configuration)))

(-> configuration-journal-path (configuration) pathname)
(defun configuration-journal-path (configuration)
  "Return the append-only live-mutation journal pathname."
  (merge-pathnames "mutations.sexp" (configuration-state-root configuration)))

(-> configuration-wire-effort (configuration) string)
(defun configuration-wire-effort (configuration)
  "Return the provider effort, mapping user-visible Ultra to wire-level Max."
  (if (string= (configuration-reasoning-effort configuration) "ultra")
      "max"
      (configuration-reasoning-effort configuration)))

(-> configuration-grok-wire-effort (configuration) string)
(defun configuration-grok-wire-effort (configuration)
  "Return the Grok provider effort, clamped to the low, medium, high scale."
  (let ((effort (configuration-reasoning-effort configuration)))
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
  (let ((effort (configuration-reasoning-effort configuration)))
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
  (handler-case
      (with-open-file (stream #P"/proc/sys/kernel/random/uuid"
                              :direction ':input
                              :external-format ':utf-8)
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (read-line stream)))
    (error ()
      (format nil "~36R-~16,'0X"
              (get-universal-time)
              (random (ash 1 64))))))
