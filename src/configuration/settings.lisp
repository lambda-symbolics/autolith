(in-package #:autolith)

;;;; -- Defaults --

(defparameter *autolith-version* "0.51.1"
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

;; Fast capability metadata from https://github.com/openai/codex at
;; 27969c0ae9c1ec23e359ff5133b4c36a8dd5b1ac for GPT-6 Sol and Luna, and
;; 287587c32c9cbc1e78edbf2aaae6a6d84f5b0c56 for GPT-5.6.
(defparameter *codex-fast-mode-models*
  '("gpt-6-sol" "gpt-6-luna" "gpt-5.6-sol" "gpt-5.6-luna" "gpt-5.6-terra")
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

(-> environment-positive-real (string real) real)
(defun environment-positive-real (variable fallback)
  "Return positive real-valued VARIABLE, or FALLBACK when it is unset.

Parses with *READ-EVAL* disabled so the environment cannot smuggle in a
read-time evaluation form; the parsed value must still be a positive real."
  (let ((value (uiop:getenv variable)))
    (if (non-empty-string-p value)
        (let ((parsed (let ((*read-eval* nil))
                        (ignore-errors (read-from-string value)))))
          (unless (and (realp parsed) (plusp parsed))
            (error 'configuration-error
                   :message (format nil "~A must be a positive number, not ~S."
                                    variable value)))
          parsed)
        fallback)))

(-> configuration--default-config-root () pathname)
(defun configuration--default-config-root ()
  "Return Autolith's default configuration directory for this host."
  (platform-application-root *platform* ':config))

(-> configuration--default-grok-bootstrap-path () pathname)
(defun configuration--default-grok-bootstrap-path ()
  "Return the default Grok Build auth.json bootstrap pathname."
  (merge-pathnames
   "auth.json"
   (environment-directory "GROK_HOME"
                          (merge-pathnames ".grok/"
                                           (user-homedir-pathname)))))


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
  (environment-positive-integer
   "AUTOLITH_CONTEXT_WINDOW"
   (or (and (fboundp 'provider-model-context-window-for)
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
