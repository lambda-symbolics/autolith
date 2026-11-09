(in-package #:autolith)

;;;; -- Built-in Static API-Key Providers --

(define-static-api-key-provider anthropic
  :display-name         "Anthropic"
  :environment-variable "ANTHROPIC_API_KEY"
  :models-endpoint      *anthropic-models-endpoint*
  :key-headers          (lambda (key)
                          (list (cons "x-api-key" key)
                                (cons "anthropic-version" *anthropic-api-version*)
                                (cons "User-Agent" (provider-user-agent)))))

(define-static-api-key-provider openrouter
  :display-name         "OpenRouter"
  :environment-variable "OPENROUTER_API_KEY"
  :models-endpoint      (openrouter-models-endpoint)
  :key-headers          (lambda (key)
                          (list (cons "Authorization" (concatenate 'string "Bearer " key)))))

(define-static-api-key-provider mistral
  :display-name         "Mistral"
  :environment-variable "MISTRAL_API_KEY"
  :models-endpoint      (mistral-models-endpoint)
  :key-headers          (lambda (key)
                          (list (cons "Authorization" (concatenate 'string "Bearer " key)))))

(define-static-api-key-provider opencode
  :display-name         "OpenCode Go"
  :environment-variable "OPENCODE_API_KEY"
  :source-class         autolith-credential-source
  :source-path          (configuration-opencode-auth-path configuration)
  :login-hint           "run autolith auth opencode")

(define-static-api-key-provider opencode-zen
  :display-name         "OpenCode Zen"
  :environment-variable "OPENCODE_ZEN_API_KEY"
  :source-class         autolith-credential-source
  :source-path          (configuration-opencode-zen-auth-path configuration)
  :login-hint           "run autolith auth opencode-zen")

(-> fireworks-validate-api-key (string) null)
(defun fireworks-validate-api-key (key)
  "Probe the Fireworks Responses API with KEY, signaling on rejection.

Fireworks has no authenticated model list, so the probe is a minimal request."
  (api-key-validate-probe
   "Fireworks"
   (lambda ()
     (let ((request
             (json-object
              "model" *default-fireworks-model*
              "input" "Reply with the single word: ok"
              "store" (json-false)
              "stream" (json-false))))
       (provider-call-with-response-deadline
        60
        (lambda ()
          (dexador:post
           (or (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT")
               *fireworks-responses-endpoint*)
           :headers         (list (cons "Authorization" (format nil "Bearer ~A" key))
                                  (cons "Content-Type" "application/json")
                                  (cons "Accept" "application/json")
                                  (cons "User-Agent" (provider-user-agent)))
           :content         (json-encode-utf8 request)
           :force-string    t
           :keep-alive      nil
           :connect-timeout 30
           :read-timeout    60)))))))

(define-static-api-key-provider fireworks
  :display-name         "Fireworks"
  :environment-variable "FIREWORKS_API_KEY"
  :source-class         autolith-credential-source
  :source-path          (configuration-fireworks-auth-path configuration)
  :login-hint           "run autolith auth fireworks"
  :validate-function    fireworks-validate-api-key)
