(in-package #:autolith)

;;;; -- Built-in Provider Registrations --

(-> provider--codex-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--codex-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in ChatGPT provider."
  (provider-family-create ':codex
                           configuration
                           :reasoning-summaries-p reasoning-summaries-p))

(-> provider--grok-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--grok-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in Grok provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':grok configuration))

(-> provider--gemini-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--gemini-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Gemini Code Assist subscription provider."
  (declare (ignore reasoning-summaries-p))
  (gemini-code-assist-provider-create configuration))

(-> provider--gemini-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--gemini-registration-authenticator
    (provider &key stream open-browser-p)
  "Run installed-application OAuth for the built-in Gemini provider."
  (gemini-oauth-login (provider-credential-manager provider)
                      :stream         (or stream *standard-output*)
                      :open-browser-p open-browser-p)
  "Gemini authentication was saved by Autolith.")

(-> provider--nous-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--nous-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Nous Research provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':nous configuration))

(-> provider--nous-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--nous-registration-authenticator
    (provider &key stream open-browser-p)
  "Run browser device login for the built-in Nous Research provider."
  (nous-provider-authenticate provider
                              :stream         stream
                              :open-browser-p open-browser-p))

(-> provider--fireworks-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--fireworks-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Fireworks provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':fireworks configuration))

(-> provider--opencode-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--opencode-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenCode provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':opencode configuration))

(-> provider--opencode-zen-registration-factory
    (configuration &key (:reasoning-summaries-p boolean)) model-provider)
(defun provider--opencode-zen-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenCode Zen provider."
  (provider-family-create ':opencode-zen configuration
                          :reasoning-summaries-p reasoning-summaries-p))

(-> provider--opencode-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--opencode-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenCode provider's API key."
  (declare (ignore open-browser-p))
  (funcall (ecase (provider-family provider)
             (:opencode
              #'opencode-api-key-login)
             (:opencode-zen
              #'opencode-zen-api-key-login))
           (provider-credential-manager provider)
           :stream (or stream *standard-output*)))

(-> provider--openrouter-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--openrouter-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenRouter provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':openrouter configuration))

(-> provider--openrouter-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--openrouter-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenRouter provider's API key."
  (declare (ignore open-browser-p))
  (openrouter-api-key-login (provider-credential-manager provider)
                            :stream (or stream *standard-output*)))

(-> provider--mistral-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--mistral-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Mistral provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':mistral configuration))

(-> provider--mistral-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--mistral-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Mistral provider's API key."
  (declare (ignore open-browser-p))
  (mistral-api-key-login (provider-credential-manager provider)
                         :stream (or stream *standard-output*)))

(-> provider--fireworks-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--fireworks-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Fireworks provider's API key."
  (declare (ignore open-browser-p))
  (fireworks-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(-> provider--anthropic-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--anthropic-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Anthropic provider."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':anthropic configuration))

(-> provider--anthropic-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--anthropic-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Anthropic provider's API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


;; ChatGPT subscription metadata: https://github.com/openai/codex,
;; codex-rs/models-manager/models.json at c465563aa9626d99420062135b187d299f904f81.
(defparameter *codex-models*
  '((:name              "gpt-6.1-sol"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-6.1-terra"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-6-astra"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-6-sol"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-6-luna"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max"))
    (:name              "gpt-5.6-sol"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
    (:name              "gpt-5.6-luna"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max"))
    (:name              "gpt-5.6-terra"
     :context-window    272000
     :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra")))
  "Codex subscription defaults, independent of public OpenAI API metadata.

gpt-6.1-terra is not yet in that catalog; it takes gpt-5.6-terra's window and
efforts until upstream publishes its entry.")

(register-provider
 "chatgpt"
 :description "ChatGPT Codex subscription"
 :family      ':codex
 :protocol    ':responses
 :models      *codex-models*
 :factory     #'provider--codex-registration-factory
 :source      ':builtin)

(-> provider-codex-context-window-for
    (string &key (:provider (option string)))
    (option integer))
(defun provider-codex-context-window-for (model &key provider)
  "Return Codex's default window for a known built-in ChatGPT MODEL, or NIL."
  (multiple-value-bind (identifier registration)
      (provider--resolve-model-metadata model :provider provider)
    (when (and registration
               (eq (provider-registration-source registration) ':builtin)
               (equal (provider-registration-name registration) "chatgpt"))
      (getf (find identifier *codex-models*
                  :key (lambda (spec) (getf spec :name)) :test #'string=)
            :context-window))))

(register-provider
 "openai-api"
 :description              "OpenAI API (pay-per-token)"
 :family                   ':openai-api
 :protocol                 ':responses
 :endpoint                 "https://api.openai.com/v1/responses"
 :model-discovery          #'openai-api--fetch-models
 :model-discovery-endpoint "https://api.openai.com/v1/models"
 :authenticator            #'openai-compatible--authenticate
 :factory                  #'openai-api--registration-factory
 :source                   ':builtin)

(register-provider
 "gemini"
 :description   "Gemini Code Assist subscription"
 :family        ':gemini-code-assist
 :protocol      ':gemini-code-assist
 :models        *gemini-code-assist-models*
 :factory       #'provider--gemini-registration-factory
 :authenticator #'provider--gemini-registration-authenticator
 :endpoint      *gemini-code-assist-endpoint*
 :source        ':builtin)

(register-provider
 "grok"
 :description "Grok subscription"
 :family      ':grok
 :protocol    ':responses
 :models      '((:name "grok-4.7" :context-window 500000
                 :reasoning-efforts ("low" "medium" "high" "xhigh"))
                (:name "grok-4.6" :context-window 500000)
                (:name "grok-4.5" :context-window 500000))
 :factory     #'provider--grok-registration-factory
 :source      ':builtin)

(register-provider
 "nous"
 :description                       "Nous Research subscription"
 :family                            ':nous
 :protocol                          ':chat-completions+messages
 :endpoint                          (nous-chat-completions-endpoint)
 :factory                           #'provider--nous-registration-factory
 :authenticator                     #'provider--nous-registration-authenticator
 :model-discovery                   #'nous--fetch-models
 :model-discovery-endpoint          (nous-models-endpoint)
 :model-discovery-endpoint-resolver #'nous-models-endpoint
 :source                            ':builtin)

(register-provider
 "fireworks"
 :description   "Fireworks AI"
 :family        ':fireworks
 :protocol      ':responses
 :models        '((:name           "accounts/fireworks/models/kimi-k3"
                   :context-window 1048576)
                  ;; Fireworks advertises a 262k token context window.
                  (:name              "accounts/fireworks/models/qwen3p7-plus"
                   :context-window    262144
                   :reasoning-efforts ("none")))
 :factory       #'provider--fireworks-registration-factory
 :authenticator #'provider--fireworks-registration-authenticator
 :endpoint      *fireworks-responses-endpoint*
 :source        ':builtin)

(register-provider
 "anthropic"
 :description   "Anthropic API (pay-per-token)"
 :family        ':anthropic
 :protocol      ':messages
 :models        '((:name "claude-opus-5" :context-window 200000
                   :reasoning-efforts ("low" "medium" "high"))
                  (:name "claude-sonnet-5" :context-window 200000
                   :reasoning-efforts ("low" "medium" "high"))
                  (:name "claude-haiku-4-5-20251001" :context-window 200000
                   :reasoning-efforts ("low" "medium" "high")))
 :factory       #'provider--anthropic-registration-factory
 :authenticator #'provider--anthropic-registration-authenticator
 :endpoint      *anthropic-messages-endpoint*
 :source        ':builtin)

(register-provider
 "opencode-go"
 :description                       "OpenCode Go"
 :family                            ':opencode
 :protocol                          ':chat-completions
 :endpoint                          *opencode-chat-completions-endpoint*
 :factory                           #'provider--opencode-registration-factory
 :authenticator                     #'provider--opencode-registration-authenticator
 :model-discovery                   #'opencode--fetch-models
 :model-discovery-endpoint          *opencode-models-endpoint*
 :model-discovery-endpoint-resolver #'opencode-models-endpoint
 :source                            ':builtin)


(register-provider
 "opencode-zen"
 :description                       "OpenCode Zen (pay-per-token)"
 :family                            ':opencode-zen
 :protocol                          ':chat-completions
 :endpoint                          *opencode-zen-chat-completions-endpoint*
 :factory                           #'provider--opencode-zen-registration-factory
 :authenticator                     #'provider--opencode-registration-authenticator
 :model-discovery                   #'opencode-zen--fetch-models
 :model-discovery-endpoint          *opencode-zen-models-endpoint*
 :model-discovery-endpoint-resolver #'opencode-zen-models-endpoint
 :source                            ':builtin)

(register-provider
 "openrouter"
 :description                       "OpenRouter (pay-per-token)"
 :family                            ':openrouter
 :protocol                          ':chat-completions
 :endpoint                          *openrouter-chat-completions-endpoint*
 :factory                           #'provider--openrouter-registration-factory
 :authenticator                     #'provider--openrouter-registration-authenticator
 :model-discovery                   #'openrouter--fetch-models
 :model-discovery-endpoint          *openrouter-models-endpoint*
 :model-discovery-endpoint-resolver #'openrouter-models-endpoint
 :source                            ':builtin)

(register-provider
 "mistral"
 :description                       "Mistral AI"
 :family                            ':mistral
 :protocol                          ':chat-completions
 :endpoint                          *mistral-chat-completions-endpoint*
 :factory                           #'provider--mistral-registration-factory
 :authenticator                     #'provider--mistral-registration-authenticator
 :model-discovery                   #'mistral--fetch-models
 :model-discovery-endpoint          *mistral-models-endpoint*
 :model-discovery-endpoint-resolver #'mistral-models-endpoint
 :source                            ':builtin)

(register-provider
 "vertex-gemini"
 :description "Gemini on Google Vertex AI"
 :family      ':gemini
 :protocol    ':gemini-generate-content
 :models      '((:name "gemini-3.5-flash" :context-window 1048576)
                (:name "gemini-3.1-pro-preview" :context-window 1048576))
 :factory     #'vertex-gemini-provider-create
 :endpoint    "https://aiplatform.googleapis.com"
 :source      ':builtin)

(register-provider
 "vertex-anthropic"
 :description "Claude on Google Vertex AI"
 :family      ':anthropic
 :protocol    ':messages
 :models      '((:name "claude-opus-5-5" :context-window 1000000)
                (:name "claude-sonnet-4-6" :context-window 1000000)
                (:name "claude-haiku-4-5@20251001" :context-window 200000))
 :factory     #'vertex-anthropic-provider-create
 :endpoint    "https://aiplatform.googleapis.com"
 :source      ':builtin)
