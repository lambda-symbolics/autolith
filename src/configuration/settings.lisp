(in-package #:autolith)

;;;; -- Defaults --

(defparameter *autolith-version* "0.46.0"
  "The user-visible Autolith version.")

(defparameter *default-model* "gpt-5.6-sol"
  "The default model requested from the subscription provider.")

(defparameter *default-reasoning-effort* "ultra"
  "The user-visible default reasoning effort.")

(defparameter *codex-responses-endpoint*
  "https://chatgpt.com/backend-api/codex/responses"
  "The current ChatGPT Codex Responses endpoint.")

;; ChatGPT browser OAuth behavior inspected at
;; https://github.com/openai/codex commit
;; 94cbbddafc1776d5e377bca1b05932c697e82238.
(defparameter *openai-oauth-issuer* "https://auth.openai.com"
  "The OpenAI issuer serving ChatGPT browser OAuth.")

(defparameter *openai-oauth-token-endpoint*
  "https://auth.openai.com/oauth/token"
  "The OpenAI OAuth token endpoint.")

(defparameter *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "The public OAuth client identifier used by Codex-compatible clients.")

(defparameter *openai-oauth-scopes*
  '("openid"
    "profile"
    "email"
    "offline_access"
    "api.connectors.read"
    "api.connectors.invoke")
  "The scopes requested by ChatGPT browser OAuth.")

(defparameter *openai-oauth-originator* "autolith"
  "The honest client originator sent during ChatGPT browser OAuth.")

(defparameter *chatgpt-oauth-callback-ports* '(1455 1457)
  "The localhost callback ports allowed by the ChatGPT OAuth client.")

(defparameter *chatgpt-oauth-callback-timeout* 900
  "The maximum seconds to wait for the ChatGPT browser callback.")

(defparameter *chatgpt-oauth-request-timeout* 5
  "The maximum seconds allowed to read one local callback request line.")

(defparameter *chatgpt-oauth-request-line-limit* 8192
  "The maximum characters accepted in one local callback request line.")

;; Gemini CLI OAuth behavior inspected at google-gemini/gemini-cli commit
;; 0bd1d439751478771c45d3d0895a6a9760554bf4. The installed application uses
;; PKCE as a public client. Autolith deliberately does not embed its client
;; secret; deployments that require one may provide it through the environment.
(defparameter *gemini-oauth-client-id*
  "681255809395-oo8ft2oprdrnp9e3aqf6av3hmdib135j.apps.googleusercontent.com"
  "The public Google installed-application client identifier used by Gemini CLI.")

(defparameter *gemini-oauth-authorization-endpoint*
  "https://accounts.google.com/o/oauth2/v2/auth"
  "The Google OAuth authorization endpoint used for Gemini subscription login.")

(defparameter *gemini-oauth-token-endpoint*
  "https://oauth2.googleapis.com/token"
  "The Google OAuth token endpoint used for Gemini subscription credentials.")

(defparameter *gemini-oauth-scopes*
  '("https://www.googleapis.com/auth/cloud-platform"
    "https://www.googleapis.com/auth/userinfo.email"
    "https://www.googleapis.com/auth/userinfo.profile")
  "The Google scopes required by Gemini Code Assist subscription access.")

(defparameter *gemini-oauth-callback-timeout* 300
  "The maximum seconds to wait for the installed-app loopback callback.")

(-> gemini-oauth-client-id () string)
(defun gemini-oauth-client-id ()
  "Return the configured Google installed-app OAuth client identifier."
  (let ((override (uiop:getenv "AUTOLITH_GEMINI_OAUTH_CLIENT_ID")))
    (if (non-empty-string-p override) override *gemini-oauth-client-id*)))

(-> gemini-oauth-client-secret () (option string))
(defun gemini-oauth-client-secret ()
  "Return an optional configured Google installed-app client secret."
  (let ((secret (uiop:getenv "AUTOLITH_GEMINI_OAUTH_CLIENT_SECRET")))
    (and (non-empty-string-p secret) secret)))

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


;; Nous Portal authentication and inference behavior verified against Hermes
;; Agent reference commit f293e7206b4ddd66042329442c6afebc19a8808d.
(defparameter *nous-portal-url* "https://portal.nousresearch.com"
  "The Nous Research portal serving OAuth device authentication.")

(defparameter *nous-inference-base-url*
  "https://inference-api.nousresearch.com/v1"
  "The Nous Research inference API base URL.")

(defparameter *nous-oauth-client-id* "hermes-cli"
  "The public OAuth client identifier accepted by Nous Portal.")

(defparameter *nous-oauth-scope* "inference:invoke"
  "The OAuth scope required to invoke Nous Research inference models.")

(-> nous-portal-url () string)
(defun nous-portal-url ()
  "Return the configured Nous Research portal URL."
  (let ((override (uiop:getenv "AUTOLITH_NOUS_PORTAL_URL")))
    (string-right-trim
     '(#\/)
     (if (non-empty-string-p override) override *nous-portal-url*))))

(-> nous-inference-base-url () string)
(defun nous-inference-base-url ()
  "Return the configured Nous Research inference API base URL."
  (let ((override (uiop:getenv "AUTOLITH_NOUS_INFERENCE_BASE_URL")))
    (string-right-trim
     '(#\/)
     (if (non-empty-string-p override)
         override
         *nous-inference-base-url*))))

(-> nous-chat-completions-endpoint () string)
(defun nous-chat-completions-endpoint ()
  "Return the configured Nous Chat Completions endpoint."
  (concatenate 'string (nous-inference-base-url) "/chat/completions"))

(-> nous-messages-endpoint () string)
(defun nous-messages-endpoint ()
  "Return the configured Nous Anthropic Messages endpoint."
  (concatenate 'string (nous-inference-base-url) "/messages"))

(-> nous-models-endpoint () string)
(defun nous-models-endpoint ()
  "Return the configured Nous model discovery endpoint."
  (concatenate 'string (nous-inference-base-url) "/models"))

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

;; The public OpenRouter Chat Completions API, verified on 2026-08-24: the
;; endpoint accepts streaming text, function tools, normalized reasoning
;; controls, and dynamic model discovery across upstream model vendors.
(defparameter *openrouter-chat-completions-endpoint*
  "https://openrouter.ai/api/v1/chat/completions"
  "The OpenRouter Chat Completions API endpoint.")

(defparameter *openrouter-models-endpoint*
  "https://openrouter.ai/api/v1/models"
  "The OpenRouter models endpoint used for dynamic model discovery.")

(-> openrouter-models-endpoint () string)
(defun openrouter-models-endpoint ()
  "Return the configured OpenRouter model discovery endpoint."
  (let ((override (uiop:getenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT")))
    (if (non-empty-string-p override)
        override
        *openrouter-models-endpoint*)))

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

(defparameter *mistral-chat-completions-endpoint*
  "https://api.mistral.ai/v1/chat/completions"
  "The Mistral Chat Completions API endpoint.")

(defparameter *mistral-models-endpoint*
  "https://api.mistral.ai/v1/models"
  "The Mistral models endpoint used for discovery and key validation.")

(-> mistral-models-endpoint () string)
(defun mistral-models-endpoint ()
  "Return the configured Mistral model discovery endpoint."
  (let ((override (uiop:getenv "AUTOLITH_MISTRAL_MODELS_ENDPOINT")))
    (if (non-empty-string-p override)
        override
        *mistral-models-endpoint*)))

(defparameter *supported-reasoning-efforts*
  '("none" "low" "medium" "high" "xhigh" "max" "ultra")
  "Reasoning effort names accepted by Autolith configuration.")

(defparameter *supported-web-search-modes*
  '("cached" "indexed" "live" "disabled")
  "Standalone web search modes accepted by Autolith configuration.")

;; DEFVAR, deliberately: the provider registry rewrites this table at
;; runtime with every registered model, so a self-reload through
;; ql:quickload must not reset it to the built-in list and invalidate
;; the very model the image is running on.
(defvar *supported-models*
  '("gpt-5.6-sol" "gpt-5.6-luna" "gpt-5.6-terra" "grok-4.5"
    "accounts/fireworks/models/kimi-k3")
  "The model identifiers offered by the interactive model picker.")

;; Fast capability metadata read from Codex reference commit
;; 287587c32c9cbc1e78edbf2aaae6a6d84f5b0c56. Unknown and future models use
;; the standard path until their catalog metadata is verified.
(defparameter *codex-fast-mode-models*
  '("gpt-5.6-sol" "gpt-5.6-luna" "gpt-5.6-terra")
  "Codex model identifiers verified to support the Fast service tier.")

;; GPT window sizes read from the live Codex model catalog on 2026-07-19 and
;; confirmed in Codex reference commit 0fb559f0f6e231a88ac02ea002d3ecd248e2b515.
;; The Grok window comes from default_models.json in grok-build reference
;; commit 47348d13.
;; DEFVAR for the same reason as *SUPPORTED-MODELS*: the provider
;; registry rewrites this table at runtime.
(defvar *model-context-windows*
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
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let* ((value (uiop:getenv variable))
         (pathname (and (non-empty-string-p value)
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

(-> environment-boolean (string boolean) boolean)
(defun environment-boolean (variable fallback)
  "Return boolean VARIABLE, or FALLBACK when it is unset."
  (let ((value (uiop:getenv variable)))
    (if (non-empty-string-p value)
        (let ((normalized (string-downcase value)))
          (cond
            ((member normalized '("1" "true" "yes" "on") :test #'string=)
             t)
            ((member normalized '("0" "false" "no" "off") :test #'string=)
             nil)
            (t
             (error 'configuration-error
                    :message
                    (format nil
                            "~A must be on or off, not ~S."
                            variable
                            value)))))
        fallback)))

(-> environment-positive-integer (string (integer 1)) (integer 1))
(defun environment-positive-integer (variable fallback)
  "Return positive integer VARIABLE, or FALLBACK when it is unset."
  (let ((value (uiop:getenv variable)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (unless (plusp parsed)
                (error 'configuration-error
                       :message (format nil "~A must be a positive integer." variable)))
              parsed)
          (configuration-error (condition)
            (error condition))
          (error ()
            (error 'configuration-error
                   :message (format nil "~A must be a positive integer." variable))))
        fallback)))

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
   (site-config-root
    :initarg :site-config-root
    :initform nil
    :reader configuration-site-config-root
    :type (option pathname)
    :documentation "The optional site-managed configuration root.")
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
   (codex-fast-mode-p
    :initarg :codex-fast-mode-p
    :initform nil
    :reader configuration-codex-fast-mode-p
    :type boolean
    :documentation "Whether Codex requests opt in to Fast mode.")
   (immutable-p
    :initarg :immutable-p
    :initform nil
    :reader configuration-immutable-p
    :type boolean
    :documentation "Whether mutation-capable active-image tools are disabled.")
   (management-repl-enabled-p
    :initarg :management-repl-enabled-p
    :initform nil
    :reader configuration-management-repl-enabled-p
    :type boolean
    :documentation "Whether the authenticated active-image management endpoint starts.")
   (management-repl-transport
    :initarg :management-repl-transport
    :initform ':unix
    :reader configuration-management-repl-transport
    :type (member :unix :tcp)
    :documentation "The management endpoint transport.")
   (management-repl-unix-socket-path
    :initarg :management-repl-unix-socket-path
    :initform #P"management/repl.sock"
    :reader configuration-management-repl-unix-socket-path
    :type pathname
    :documentation "The private Unix management socket pathname.")
   (management-repl-tcp-address
    :initarg :management-repl-tcp-address
    :initform "127.0.0.1"
    :reader configuration-management-repl-tcp-address
    :type non-empty-string
    :documentation "The IPv4 loopback management listener address.")
   (management-repl-tcp-port
    :initarg :management-repl-tcp-port
    :initform 4141
    :reader configuration-management-repl-tcp-port
    :type (integer 1 65535)
    :documentation "The management TCP listener port.")
   (management-repl-token-file-path
    :initarg :management-repl-token-file-path
    :initform #P"management-repl.token"
    :reader configuration-management-repl-token-file-path
    :type pathname
    :documentation "The external owner-only authentication token file pathname.")
   (management-repl-evaluation-timeout
    :initarg :management-repl-evaluation-timeout
    :initform 10
    :reader configuration-management-repl-evaluation-timeout
    :type (integer 1)
    :documentation "The management request deadline in seconds.")
   (management-repl-maximum-frame-size
    :initarg :management-repl-maximum-frame-size
    :initform 1048576
    :reader configuration-management-repl-maximum-frame-size
    :type (integer 1)
    :documentation "The maximum management wire frame size in octets.")
   (management-repl-maximum-source-size
    :initarg :management-repl-maximum-source-size
    :initform 262144
    :reader configuration-management-repl-maximum-source-size
    :type (integer 1)
    :documentation "The maximum evaluation source size in UTF-8 octets.")
   (management-repl-maximum-output-size
    :initarg :management-repl-maximum-output-size
    :initform 262144
    :reader configuration-management-repl-maximum-output-size
    :type (integer 1)
    :documentation "The maximum captured output and value text size.")
   (management-repl-queue-capacity
    :initarg :management-repl-queue-capacity
    :initform 8
    :reader configuration-management-repl-queue-capacity
    :type (integer 1)
    :documentation "The maximum queued management evaluations.")
   (management-repl-maximum-clients
    :initarg :management-repl-maximum-clients
    :initform 8
    :reader configuration-management-repl-maximum-clients
    :type (integer 1)
    :documentation "The maximum accepted management clients, including authentication.")
   (management-repl-authentication-timeout
    :initarg :management-repl-authentication-timeout
    :initform 10
    :reader configuration-management-repl-authentication-timeout
    :type (integer 1)
    :documentation "The absolute authentication deadline in seconds.")
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

(-> configuration-codex-fast-mode-available-p (configuration) boolean)
(defun configuration-codex-fast-mode-available-p (configuration)
  "Return true when CONFIGURATION's current Codex model supports Fast mode."
  (and (eq (model-family (configuration-model configuration)) ':codex)
       (not (null (member (configuration-model configuration)
                          *codex-fast-mode-models*
                          :test #'string=)))))

(-> configuration-codex-fast-mode-active-p (configuration) boolean)
(defun configuration-codex-fast-mode-active-p (configuration)
  "Return true when Fast mode is enabled and available for CONFIGURATION."
  (and (configuration-codex-fast-mode-p configuration)
       (configuration-codex-fast-mode-available-p configuration)))

(-> configuration--provider-endpoint-for (string) string)
(defun configuration--provider-endpoint-for (model)
  "Return MODEL's environment override, registered endpoint, or family default.

AUTOLITH_PROVIDER_ENDPOINT overrides the Codex family endpoint,
AUTOLITH_GROK_PROVIDER_ENDPOINT overrides the Grok family endpoint,
AUTOLITH_NOUS_PROVIDER_ENDPOINT overrides the Nous family endpoint,
AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT overrides the Fireworks family endpoint,
AUTOLITH_OPENCODE_PROVIDER_ENDPOINT overrides the OpenCode family endpoint,
AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT overrides the OpenRouter family endpoint,
and AUTOLITH_MISTRAL_PROVIDER_ENDPOINT overrides the Mistral family endpoint."
  (let* ((family (model-family model))
         (override
           (case family
             (:codex
              (uiop:getenv "AUTOLITH_PROVIDER_ENDPOINT"))
             (:grok
              (uiop:getenv "AUTOLITH_GROK_PROVIDER_ENDPOINT"))
             (:nous
              (or (uiop:getenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT")
                  (let ((base (uiop:getenv "AUTOLITH_NOUS_INFERENCE_BASE_URL")))
                    (and (non-empty-string-p base)
                         (concatenate
                          'string
                          (string-right-trim '(#\/) base)
                          "/chat/completions")))))
             (:fireworks
              (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT"))
             (:opencode
              (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"))
             (:openrouter
              (uiop:getenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT"))
             (:mistral
              (uiop:getenv "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT"))))
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
          (:nous
           (nous-chat-completions-endpoint))
          (:fireworks
           *fireworks-responses-endpoint*)
          (:opencode
           *opencode-chat-completions-endpoint*)
          (:openrouter
           *openrouter-chat-completions-endpoint*)
          (:mistral
           *mistral-chat-completions-endpoint*)
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

(-> configuration--absolute-file-pathname (pathname) pathname)
(defun configuration--absolute-file-pathname (pathname)
  "Anchor PATHNAME to the process directory captured during configuration."
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname (uiop:getcwd))))

(-> configuration--resolve-site-config-root
    ((option pathname))
    (option pathname))
(defun configuration--resolve-site-config-root (site-config-root)
  "Return SITE-CONFIG-ROOT as an existing canonical absolute directory."
  (when site-config-root
    (unless (uiop:absolute-pathname-p site-config-root)
      (error 'configuration-error
             :message
             (format nil "Site configuration root ~S must be absolute."
                     (namestring site-config-root))))
    (handler-case
        (let ((directory
                (uiop:directory-exists-p
                 (uiop:ensure-pathname site-config-root
                                       :ensure-directory t
                                       :want-non-wild t))))
          (unless directory
            (error 'configuration-error
                   :message
                   (format nil "Site configuration root ~S does not exist."
                           (namestring site-config-root))))
          (uiop:ensure-directory-pathname (truename directory)))
      (configuration-error (condition)
        (error condition))
      (serious-condition (cause)
        (error 'configuration-error
               :message
               (format nil "Could not resolve site configuration root ~S: ~A"
                       (namestring site-config-root)
                       cause))))))

(-> configuration-create
    (&key (:source-root (option pathname))
          (:working-directory (option pathname))
          (:site-config-root (option pathname))
          (:model (option string))
          (:reasoning-effort (option string))
          (:codex-fast-mode-p boolean)
          (:immutable-p boolean)
          (:defer-provider-validation-p boolean)
          (:management-repl-enabled-p boolean)
          (:management-repl-transport (option keyword))
          (:management-repl-unix-socket-path (option pathname))
          (:management-repl-tcp-address (option string))
          (:management-repl-tcp-port (option integer))
          (:management-repl-token-file-path (option pathname))
          (:management-repl-evaluation-timeout (option integer))
          (:management-repl-maximum-frame-size (option integer))
          (:management-repl-maximum-source-size (option integer))
          (:management-repl-maximum-output-size (option integer))
          (:management-repl-queue-capacity (option integer))
          (:management-repl-maximum-clients (option integer))
          (:management-repl-authentication-timeout (option integer)))
    configuration)
(defun configuration-create
    (&key source-root working-directory site-config-root model reasoning-effort
      (codex-fast-mode-p nil codex-fast-mode-p-supplied-p)
      immutable-p defer-provider-validation-p
      (management-repl-enabled-p nil management-repl-enabled-p-supplied-p)
      management-repl-transport management-repl-unix-socket-path
      management-repl-tcp-address management-repl-tcp-port
      management-repl-token-file-path management-repl-evaluation-timeout
      management-repl-maximum-frame-size management-repl-maximum-source-size
      management-repl-maximum-output-size management-repl-queue-capacity
      management-repl-maximum-clients management-repl-authentication-timeout)
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
         (environment-site-config-root
           (uiop:getenv "AUTOLITH_SITE_CONFIG_ROOT"))
         (selected-site-config-root
           (configuration--resolve-site-config-root
            (or site-config-root
                (and (non-empty-string-p environment-site-config-root)
                     (pathname environment-site-config-root)))))
         (selected-model (or model (uiop:getenv "AUTOLITH_MODEL") *default-model*))
         (selected-effort (or reasoning-effort
                              (uiop:getenv "AUTOLITH_REASONING_EFFORT")
                              *default-reasoning-effort*))
         (selected-codex-fast-mode-p
           (if codex-fast-mode-p-supplied-p
               codex-fast-mode-p
               (environment-boolean "AUTOLITH_CODEX_FAST_MODE" nil)))
         (selected-web-search (let ((mode (uiop:getenv "AUTOLITH_WEB_SEARCH")))
                                (if (non-empty-string-p mode)
                                    (string-downcase mode)
                                    "cached")))
         (selected-management-enabled-p
           (if management-repl-enabled-p-supplied-p
               management-repl-enabled-p
               (environment-boolean "AUTOLITH_MANAGEMENT_REPL" nil)))
         (selected-management-transport
           (or management-repl-transport
               (let ((value (uiop:getenv "AUTOLITH_MANAGEMENT_REPL_TRANSPORT")))
                 (if (non-empty-string-p value)
                     (intern (string-upcase value) '#:keyword)
                     ':unix))))
         (selected-management-address
           (or management-repl-tcp-address
               (uiop:getenv "AUTOLITH_MANAGEMENT_REPL_TCP_ADDRESS")
               "127.0.0.1")))
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
    (unless (member selected-management-transport '(:unix :tcp))
      (error 'configuration-error
             :message "AUTOLITH_MANAGEMENT_REPL_TRANSPORT must be unix or tcp."))
    (let ((maximum-frame-size
            (or management-repl-maximum-frame-size
                (environment-positive-integer
                 "AUTOLITH_MANAGEMENT_REPL_MAX_FRAME" 1048576))))
      (when (< maximum-frame-size 128)
        (error 'configuration-error
               :message "AUTOLITH_MANAGEMENT_REPL_MAX_FRAME must be at least 128.")))
    (make-instance 'configuration
                   :source-root
                   (uiop:ensure-directory-pathname
                    (or source-root
                        (and (non-empty-string-p environment-source-root)
                             (pathname environment-source-root))
                        (asdf:system-source-directory :autolith)))
                   :working-directory
                   (uiop:ensure-directory-pathname
                    (or working-directory (uiop:getcwd)))
                   :config-root (merge-pathnames "autolith/" config-home)
                   :site-config-root selected-site-config-root
                   :data-root (merge-pathnames "autolith/" data-home)
                   :state-root (merge-pathnames "autolith/" state-home)
                   :cache-root (merge-pathnames "autolith/" cache-home)
                   :codex-auth-path (merge-pathnames "auth.json" codex-home)
                   :grok-bootstrap-auth-path
                   (configuration--default-grok-bootstrap-path)
                   :model selected-model
                   :reasoning-effort selected-effort
                   :codex-fast-mode-p selected-codex-fast-mode-p
                   :immutable-p immutable-p
                   :management-repl-enabled-p selected-management-enabled-p
                   :management-repl-transport selected-management-transport
                   :management-repl-unix-socket-path
                   (configuration--absolute-file-pathname
                    (or management-repl-unix-socket-path
                        (let ((value
                                (uiop:getenv
                                 "AUTOLITH_MANAGEMENT_REPL_UNIX_SOCKET")))
                          (if (non-empty-string-p value)
                              (pathname value)
                              (merge-pathnames
                               "management/repl.sock"
                               (merge-pathnames "autolith/" state-home))))))
                   :management-repl-tcp-address selected-management-address
                   :management-repl-tcp-port
                   (or management-repl-tcp-port
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_TCP_PORT" 4141))
                   :management-repl-token-file-path
                   (configuration--absolute-file-pathname
                    (or management-repl-token-file-path
                        (let ((value
                                (uiop:getenv
                                 "AUTOLITH_MANAGEMENT_REPL_TOKEN_FILE")))
                          (if (non-empty-string-p value)
                              (pathname value)
                              (merge-pathnames
                               "management-repl.token"
                               (merge-pathnames "autolith/" config-home))))))
                   :management-repl-evaluation-timeout
                   (or management-repl-evaluation-timeout
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_TIMEOUT" 10))
                   :management-repl-maximum-frame-size
                   (or management-repl-maximum-frame-size
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_MAX_FRAME" 1048576))
                   :management-repl-maximum-source-size
                   (or management-repl-maximum-source-size
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_MAX_SOURCE" 262144))
                   :management-repl-maximum-output-size
                   (or management-repl-maximum-output-size
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_MAX_OUTPUT" 262144))
                   :management-repl-queue-capacity
                   (or management-repl-queue-capacity
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_QUEUE_CAPACITY" 8))
                   :management-repl-maximum-clients
                   (or management-repl-maximum-clients
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_MAX_CLIENTS" 8))
                   :management-repl-authentication-timeout
                   (or management-repl-authentication-timeout
                       (environment-positive-integer
                        "AUTOLITH_MANAGEMENT_REPL_AUTH_TIMEOUT" 10))
                   :web-search-mode selected-web-search
                   :context-window
                   (configuration--context-window-for selected-model)
                   :compaction-threshold-percent
                   (configuration--compaction-threshold)
                   :provider-endpoint
                   (configuration--provider-endpoint-for selected-model))))

(-> configuration--clone
    (configuration &key (:working-directory (option pathname))
                   (:model (option string))
                   (:reasoning-effort (option string))
                   (:codex-fast-mode-p boolean)
                   (:immutable-p boolean)
                   (:web-search-mode (option string)))
    configuration)
(defun configuration--clone
    (configuration
     &key working-directory model
       (reasoning-effort nil reasoning-effort-supplied-p)
       (codex-fast-mode-p nil codex-fast-mode-p-supplied-p)
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
                   :site-config-root
                   (configuration-site-config-root configuration)
                   :data-root (configuration-data-root configuration)
                   :state-root (configuration-state-root configuration)
                   :cache-root (configuration-cache-root configuration)
                   :management-repl-enabled-p
                   (configuration-management-repl-enabled-p configuration)
                   :management-repl-transport
                   (configuration-management-repl-transport configuration)
                   :management-repl-unix-socket-path
                   (configuration-management-repl-unix-socket-path configuration)
                   :management-repl-tcp-address
                   (configuration-management-repl-tcp-address configuration)
                   :management-repl-tcp-port
                   (configuration-management-repl-tcp-port configuration)
                   :management-repl-token-file-path
                   (configuration-management-repl-token-file-path configuration)
                   :management-repl-evaluation-timeout
                   (configuration-management-repl-evaluation-timeout configuration)
                   :management-repl-maximum-frame-size
                   (configuration-management-repl-maximum-frame-size configuration)
                   :management-repl-maximum-source-size
                   (configuration-management-repl-maximum-source-size configuration)
                   :management-repl-maximum-output-size
                   (configuration-management-repl-maximum-output-size configuration)
                   :management-repl-queue-capacity
                   (configuration-management-repl-queue-capacity configuration)
                   :management-repl-maximum-clients
                   (configuration-management-repl-maximum-clients configuration)
                   :management-repl-authentication-timeout
                   (configuration-management-repl-authentication-timeout configuration)
                   :codex-auth-path (configuration-codex-auth-path configuration)
                   :grok-bootstrap-auth-path
                   (configuration-grok-bootstrap-auth-path configuration)
                   :model selected-model
                   :reasoning-effort selected-effort
                   :codex-fast-mode-p
                   (if codex-fast-mode-p-supplied-p
                       codex-fast-mode-p
                       (configuration-codex-fast-mode-p configuration))
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

(-> configuration-with-codex-fast-mode (configuration boolean) configuration)
(defun configuration-with-codex-fast-mode (configuration enabled-p)
  "Copy CONFIGURATION with Codex Fast mode set to ENABLED-P."
  (configuration--clone configuration :codex-fast-mode-p enabled-p))

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
    (multiple-value-bind (pathname created-p)
        (ensure-directories-exist directory)
      (when created-p
        (sb-posix:chmod (namestring pathname) #o700))))
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

(-> configuration-site-init-path (configuration) (option pathname))
(defun configuration-site-init-path (configuration)
  "Return the site-authored Lisp initialization pathname, when configured."
  (let ((root (configuration-site-config-root configuration)))
    (and root (merge-pathnames "init.lisp" root))))

(-> configuration-site-mcp-path (configuration) (option pathname))
(defun configuration-site-mcp-path (configuration)
  "Return the site-authored native MCP pathname, when configured."
  (let ((root (configuration-site-config-root configuration)))
    (and root (merge-pathnames "mcp.sexp" root))))

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

(-> configuration-nous-auth-path (configuration) pathname)
(defun configuration-nous-auth-path (configuration)
  "Return Autolith's private Nous OAuth credential pathname."
  (merge-pathnames "nous-auth.sexp" (configuration-state-root configuration)))

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
  (flet ((fallback ()
           (format nil "~36R-~16,'0X"
                   (get-universal-time)
                   (random (ash 1 64)))))
    (handler-case
        (cond
          #+linux
          (t
           (with-open-file (stream #P"/proc/sys/kernel/random/uuid"
                                   :direction ':input
                                   :external-format ':utf-8)
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (read-line stream))))
          #+(and (not linux) (or darwin macos macosx bsd))
          (t
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (uiop:run-program '("/usr/bin/uuidgen") :output :string)))
          (t
           (fallback)))
      (error ()
        (fallback)))))
