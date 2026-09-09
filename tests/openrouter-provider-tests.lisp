(in-package #:autolith)

;;;; -- OpenRouter Provider Tests --

(-> openrouter-provider-test--configuration
    (&key
     (:model non-empty-string)
     (:reasoning-effort non-empty-string)
     (:provider-endpoint non-empty-string))
    configuration)
(defun openrouter-provider-test--configuration
    (&key
       (model (openrouter--model-name "openai/gpt-5-mini"))
       (reasoning-effort *default-reasoning-effort*)
       (provider-endpoint *openrouter-chat-completions-endpoint*))
  "Return an isolated OpenRouter test configuration."
  (let ((base (test-configuration)))
    (make-instance 'configuration
                   :source-root (configuration-source-root base)
                   :working-directory (configuration-working-directory base)
                   :config-root (configuration-config-root base)
                   :data-root (configuration-data-root base)
                   :state-root (configuration-state-root base)
                   :cache-root (configuration-cache-root base)
                   :codex-auth-path (configuration-codex-auth-path base)
                   :grok-bootstrap-auth-path
                   (configuration-grok-bootstrap-auth-path base)
                   :model model
                   :reasoning-effort reasoning-effort
                   :provider-endpoint provider-endpoint)))

(-> openrouter-provider-test--restore-environment
    (string (option string))
    null)
(defun openrouter-provider-test--restore-environment (name value)
  "Restore environment variable NAME to VALUE."
  (if value
      (platform-setenv name value)
      (platform-unsetenv name))
  nil)

(-> openrouter-provider-test--credentials () null)
(defun openrouter-provider-test--credentials ()
  "Test OpenRouter environment precedence and private-store fallback."
  (let* ((configuration (openrouter-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved (uiop:getenv *openrouter-environment-variable*)))
    (unwind-protect
         (progn
           (platform-setenv *openrouter-environment-variable* "openrouter-environment-key")
           (let ((manager (openrouter-credential-manager-create configuration)))
             (api-key-credential-manager-save-key manager "openrouter-private-key"))
           (let* ((manager (openrouter-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "openrouter-environment-key")
              "OpenRouter environment credentials take precedence"))
           (platform-unsetenv *openrouter-environment-variable*)
           (let* ((manager (openrouter-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "openrouter-private-key")
              "OpenRouter uses its private key when the environment is absent")))
      (openrouter-provider-test--restore-environment
       *openrouter-environment-variable* saved)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--model-decoding () null)
(defun openrouter-provider-test--model-decoding ()
  "Test OpenRouter local and wire model namespacing."
  (test-assert
   (string= (openrouter--wire-model-name
             "openrouter/anthropic/claude-haiku-4.5")
            "anthropic/claude-haiku-4.5")
   "OpenRouter preserves the upstream vendor and model slug on the wire")
  (test-assert
   (handler-case
       (progn
         (openrouter--wire-model-name "openai/gpt-5-mini")
         nil)
     (configuration-error () t))
   "OpenRouter rejects an unnamespaced local model identifier")
  nil)

(-> openrouter-provider-test--provider () null)
(defun openrouter-provider-test--provider ()
  "Test OpenRouter attribution and request translation."
  (let* ((configuration
           (openrouter-provider-test--configuration :reasoning-effort "medium"))
         (root (test-configuration-root configuration))
         (provider (openrouter-provider-create configuration))
         (conversation
           (conversation-create configuration :identifier "openrouter-request"))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "synthetic-openrouter-key"
                          :account-id "openrouter"
                          :source-path #P"/tmp/openrouter-test-key")))
    (unwind-protect
         (progn
           (let ((headers
                   (openai-compatible--request-headers
                    provider credentials conversation)))
             (test-assert
              (and (string= (rest (assoc "HTTP-Referer" headers :test #'string=))
                            "https://github.com/lambda-symbolics/autolith")
                   (string= (rest (assoc "X-OpenRouter-Title" headers
                                         :test #'string=))
                            "Autolith"))
              "OpenRouter requests include application attribution"))
           (conversation-append-user-message conversation "Hello OpenRouter.")
            (let ((*provider-maximum-output-tokens* 321))
              (multiple-value-bind (request delivery)
                  (provider-request-object provider conversation (json-array)
                                           :goal-context nil
                                           :compaction-p nil)
                (declare (ignore delivery))
                (test-assert
                 (string= (json-get request "model") "openai/gpt-5-mini")
                 "OpenRouter strips its namespace from the wire model")
                (test-assert
                 (string= (json-get (json-get request "reasoning") "effort")
                          "medium")
                 "OpenRouter uses its normalized reasoning schema")
                (test-assert
                 (= (json-get request "max_tokens") 321)
                 "OpenRouter requests include max_tokens")
                (test-assert
                 (null (json-get request "max_completion_tokens"))
                "OpenRouter requests omit max_completion_tokens"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--endpoints-and-discovery () null)
(defun openrouter-provider-test--endpoints-and-discovery ()
  "Test endpoint overrides, authenticated discovery, filtering, and validation."
  (let* ((configuration (openrouter-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved-chat (uiop:getenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT"))
         (saved-models (uiop:getenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT"))
         (saved-key (uiop:getenv *openrouter-environment-variable*))
         (observed nil))
    (unwind-protect
         (progn
           ;; A key in the host environment would outrank the stored one.
           (platform-unsetenv *openrouter-environment-variable*)
           (platform-setenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT"
                            "https://chat.openrouter.invalid/v1/chat/completions")
           (platform-setenv "AUTOLITH_OPENROUTER_MODELS_ENDPOINT"
                            "https://models.openrouter.invalid/v1/models")
           (test-assert
            (string= (configuration--provider-endpoint-for
                      "openrouter/openai/gpt-5-mini")
                     "https://chat.openrouter.invalid/v1/chat/completions")
            "OpenRouter chat endpoint overrides registered metadata")
           (test-assert
            (string= (openrouter-models-endpoint)
                     "https://models.openrouter.invalid/v1/models")
            "OpenRouter model discovery has an independent override")
           (api-key-credential-manager-save-key
            (openrouter-credential-manager-create configuration)
            "openrouter-discovery-key")
           (test-call-with-function-replacements
            (list
             (list 'dexador:get
                   (lambda (url &key headers &allow-other-keys)
                     (push (list url headers) observed)
                     (values
                      (json-encode
                       (json-object
                        "data"
                        (json-array
                         (json-object
                          "id" "google/gemini-3.5-flash"
                          "architecture"
                          (json-object "output_modalities" (json-array "text"))
                          "supported_parameters"
                          (json-array "tools" "tool_choice" "reasoning"))
                         (json-object
                          "id" "vendor/text-only"
                          "architecture"
                          (json-object "output_modalities" (json-array "text"))
                          "supported_parameters" (json-array "temperature")))))
                      200
                      nil))))
            (lambda ()
              (test-assert
               (equal (openrouter--fetch-models configuration)
                      '("openrouter/google/gemini-3.5-flash"))
               "OpenRouter fetches and namespaces compatible models")
              (openrouter-validate-api-key "openrouter-validation-key")))
            (test-assert
             (= (length observed) 2)
             "OpenRouter discovery and validation each issue a request")
            (test-assert
             (every
              (lambda (request)
                (string= (first request)
                         "https://models.openrouter.invalid/v1/models"))
              observed)
             "OpenRouter discovery and validation use the configured endpoint")
            (test-assert
             (some
              (lambda (request)
                (equal (rest (assoc "Authorization" (second request)
                                    :test #'string=))
                       "Bearer openrouter-discovery-key"))
              observed)
             "OpenRouter discovery authenticates with the stored key")
            (test-assert
             (some
              (lambda (request)
                (equal (rest (assoc "Authorization" (second request)
                                    :test #'string=))
                       "Bearer openrouter-validation-key"))
              observed)
             "OpenRouter validation authenticates with the supplied key")
            (test-assert
             (some
              (lambda (request)
                (string= (rest (assoc "X-OpenRouter-Title" (second request)
                                      :test #'string=))
                         "Autolith"))
              observed)
             "OpenRouter external-boundary requests include attribution"))
      (openrouter-provider-test--restore-environment
       "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT" saved-chat)
      (openrouter-provider-test--restore-environment
       "AUTOLITH_OPENROUTER_MODELS_ENDPOINT" saved-models)
      (openrouter-provider-test--restore-environment
       *openrouter-environment-variable* saved-key)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> openrouter-provider-test--builtin-registration () null)
(defun openrouter-provider-test--builtin-registration ()
  "Test the built-in OpenRouter external-boundary registration."
  (let ((registration (provider-registration-find "openrouter")))
    (test-assert registration
                 "the built-in OpenRouter registration exists")
    (test-assert
     (eq (provider-registration-family registration) ':openrouter)
     "the built-in OpenRouter registration declares its family")
    (test-assert
     (string= (provider-registration-endpoint registration)
              *openrouter-chat-completions-endpoint*)
     "the built-in OpenRouter registration declares its chat endpoint")
    (test-assert
     (string= (provider-registration-model-discovery-endpoint registration)
              *openrouter-models-endpoint*)
     "the built-in OpenRouter registration declares its discovery endpoint")
    (test-assert
     (functionp (provider-registration-model-discovery registration))
     "the built-in OpenRouter registration declares discovery behavior")
    nil))

(-> test-openrouter-provider () null)
(defun test-openrouter-provider ()
  "Run the offline OpenRouter provider tests."
  (let ((registry-snapshot (provider--registry-snapshot)))
    (unwind-protect
         (progn
           (register-provider
            "openrouter-test"
            :family ':openrouter
            :protocol ':chat-completions
            :models '("openrouter/openai/gpt-5-mini")
            :endpoint *openrouter-chat-completions-endpoint*
            :factory #'provider--openrouter-registration-factory
            :source ':runtime)
           (openrouter-provider-test--credentials)
           (openrouter-provider-test--model-decoding)
           (openrouter-provider-test--provider)
           (openrouter-provider-test--endpoints-and-discovery)
           (openrouter-provider-test--builtin-registration))
      (provider--registry-restore registry-snapshot)))
  nil)
