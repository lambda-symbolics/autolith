(in-package #:autolith)

;;;; -- Mistral Provider Tests --

(-> mistral-provider-test--configuration
    (&key (:model non-empty-string) (:provider-endpoint non-empty-string))
    configuration)
(defun mistral-provider-test--configuration
    (&key
       (model "mistral-test")
       (provider-endpoint *mistral-chat-completions-endpoint*))
  "Return an isolated Mistral test configuration."
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
                   :reasoning-effort *default-reasoning-effort*
                   :provider-endpoint provider-endpoint)))

(-> mistral-provider-test--restore-environment
    (string (option string))
    null)
(defun mistral-provider-test--restore-environment (name value)
  "Restore environment variable NAME to VALUE."
  (if value
      (platform-setenv name value)
      (platform-unsetenv name))
  nil)

(-> mistral-provider-test--credentials () null)
(defun mistral-provider-test--credentials ()
  "Test Mistral environment precedence and private-store fallback."
  (let* ((configuration (mistral-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved (uiop:getenv *mistral-environment-variable*)))
    (unwind-protect
         (progn
           (platform-setenv *mistral-environment-variable* "mistral-environment-key")
           (let ((manager (mistral-credential-manager-create configuration)))
             (api-key-credential-manager-save-key manager "mistral-private-key"))
           (let* ((manager (mistral-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "mistral-environment-key")
              "Mistral environment credentials take precedence"))
           (platform-unsetenv *mistral-environment-variable*)
           (let* ((manager (mistral-credential-manager-create configuration))
                  (loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-access-token loaded)
                       "mistral-private-key")
              "Mistral uses its private key when the environment is absent")))
      (mistral-provider-test--restore-environment
       *mistral-environment-variable* saved)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> mistral-provider-test--model-decoding () null)
(defun mistral-provider-test--model-decoding ()
  "Test Mistral model capability filtering."
  (let ((body
          (json-encode
           (json-object
            "data"
            (json-array
             (json-object
              "id" "mistral-large-latest"
              "capabilities" (json-object "completion_chat" t))
             (json-object
              "id" "codestral-embed"
              "capabilities" (json-object "completion_chat" false)))))))
    (test-assert
     (equal (openai-compatible--decode-model-list
             body
             :entry-predicate #'mistral--chat-model-p)
            '("mistral-large-latest"))
     "Mistral discovery exposes only chat-capable models"))
  nil)

(-> mistral-provider-test--provider () null)
(defun mistral-provider-test--provider ()
  "Test Mistral's output-token request field."
  (let* ((configuration (mistral-provider-test--configuration))
         (root (test-configuration-root configuration))
         (provider (mistral-provider-create configuration))
         (conversation
           (conversation-create configuration :identifier "mistral-request")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "Hello Mistral.")
           (let ((*provider-maximum-output-tokens* 321))
             (multiple-value-bind (request delivery)
                 (provider-request-object provider conversation (json-array)
                                          :goal-context nil
                                          :compaction-p nil)
               (declare (ignore delivery))
              (test-assert
               (= (json-get request "max_tokens") 321)
               "Mistral requests include max_tokens")
              (test-assert
               (null (json-get request "max_completion_tokens"))
              "Mistral requests omit max_completion_tokens"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> mistral-provider-test--endpoints-and-discovery () null)
(defun mistral-provider-test--endpoints-and-discovery ()
  "Test endpoint overrides, authenticated discovery, and key validation."
  (let* ((configuration (mistral-provider-test--configuration))
         (root (test-configuration-root configuration))
         (saved-chat (uiop:getenv "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT"))
         (saved-models (uiop:getenv "AUTOLITH_MISTRAL_MODELS_ENDPOINT"))
         (observed nil))
    (unwind-protect
         (progn
           (platform-setenv "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT"
                            "https://chat.mistral.invalid/v1/chat/completions")
           (platform-setenv "AUTOLITH_MISTRAL_MODELS_ENDPOINT"
                            "https://models.mistral.invalid/v1/models")
           (test-assert
            (string= (configuration--provider-endpoint-for "mistral-test")
                     "https://chat.mistral.invalid/v1/chat/completions")
            "Mistral chat endpoint overrides registered metadata")
           (test-assert
            (string= (mistral-models-endpoint)
                     "https://models.mistral.invalid/v1/models")
            "Mistral model discovery has an independent override")
           (api-key-credential-manager-save-key
            (mistral-credential-manager-create configuration)
            "mistral-discovery-key")
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
                          "id" "mistral-small-latest"
                          "capabilities" (json-object "completion_chat" t)))))
                      200
                      nil))))
            (lambda ()
              (test-assert
               (equal (mistral--fetch-models configuration)
                      '("mistral-small-latest"))
               "Mistral fetches chat-capable models")
              (mistral-validate-api-key "mistral-validation-key")))
           (test-assert
            (and (= (length observed) 2)
                 (every
                  (lambda (request)
                    (string= (first request)
                             "https://models.mistral.invalid/v1/models"))
                  observed)
                 (some
                  (lambda (request)
                    (equal (rest (assoc "Authorization" (second request)
                                        :test #'string=))
                           "Bearer mistral-discovery-key"))
                  observed)
                 (some
                  (lambda (request)
                    (equal (rest (assoc "Authorization" (second request)
                                        :test #'string=))
                           "Bearer mistral-validation-key"))
                  observed))
            "Mistral discovery and validation use the configured endpoint and key"))
      (mistral-provider-test--restore-environment
       "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT" saved-chat)
      (mistral-provider-test--restore-environment
       "AUTOLITH_MISTRAL_MODELS_ENDPOINT" saved-models)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-mistral-provider () null)
(defun test-mistral-provider ()
  "Run the offline Mistral provider tests."
  (let ((registry-snapshot (provider--registry-snapshot)))
    (unwind-protect
         (progn
           (register-provider
            "mistral-test"
            :family ':mistral
            :protocol ':chat-completions
            :models '("mistral-test")
            :endpoint *mistral-chat-completions-endpoint*
            :factory #'provider--mistral-registration-factory
            :source ':runtime)
           (mistral-provider-test--credentials)
           (mistral-provider-test--model-decoding)
           (mistral-provider-test--provider)
           (mistral-provider-test--endpoints-and-discovery))
      (provider--registry-restore registry-snapshot)))
  nil)