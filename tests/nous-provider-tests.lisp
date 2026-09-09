(in-package #:autolith)

;;;; -- Nous Provider Test Support --

(-> nous-provider-test--restore-environment
    (string (option string))
    null)
(defun nous-provider-test--restore-environment (name value)
  "Restore environment variable NAME to VALUE or its absent state."
  (if value
      (platform-setenv name value)
      (platform-unsetenv name))
  nil)

(-> nous-provider-test--save-credentials (configuration) oauth-credentials)
(defun nous-provider-test--save-credentials (configuration)
  "Save one valid scoped Nous credential record for CONFIGURATION."
  (let* ((manager (nous-credential-manager-create configuration))
         (source (credential-manager-primary-source manager))
         (credentials
           (nous-authentication-test--credentials
            (credential-source-pathname source)
            :access-token (nous-test--jwt)
            :refresh-token "nous-provider-refresh")))
    (credential-source-save source credentials)))

(-> nous-provider-test--header-value (string list) (option string))
(defun nous-provider-test--header-value (name headers)
  "Return NAME's case-insensitive value from HEADERS."
  (rest (assoc name headers :test #'string-equal)))


;;;; -- Registration and Discovery --

(-> nous-provider-test--registration-and-discovery () null)
(defun nous-provider-test--registration-and-discovery ()
  "Test built-in registration, auth bootstrap, endpoint overrides, and discovery."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registration (provider-registration-find "nous"))
         (original-get (symbol-function 'dexador:get))
         (saved-portal (uiop:getenv "AUTOLITH_NOUS_PORTAL_URL"))
         (saved-base (uiop:getenv "AUTOLITH_NOUS_INFERENCE_BASE_URL"))
         (saved-provider (uiop:getenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT"))
         (captured-requests nil))
    (unwind-protect
         (progn
           (test-assert
            (and registration
                 (eq (provider-registration-family registration) ':nous)
                 (functionp
                  (provider-registration-model-discovery registration))
                 (functionp
                  (provider-registration-authenticator registration))
                 (null (provider-registration-models registration)))
            "Nous is a dynamic built-in provider with explicit authentication")
           (let ((authentication-provider
                   (provider-authentication-provider configuration "nous")))
             (test-assert
              (and (typep authentication-provider
                          'nous-chat-completions-provider)
                   (typep (provider-credential-manager authentication-provider)
                          'nous-credential-manager)
                   (equal
                    (credential-source-pathname
                     (credential-manager-primary-source
                      (provider-credential-manager authentication-provider)))
                    (configuration-nous-auth-path configuration)))
              "autolith auth nous constructs a provider before model discovery"))
           (platform-setenv "AUTOLITH_NOUS_PORTAL_URL"
                            "https://portal.nous.test/")
           (platform-setenv "AUTOLITH_NOUS_INFERENCE_BASE_URL"
                            "https://inference.nous.test/v1/")
           (platform-setenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT"
                            "https://override.nous.test/chat")
           (test-assert
            (and (string= (nous-portal-url) "https://portal.nous.test")
                 (string= (nous-models-endpoint)
                          "https://inference.nous.test/v1/models")
                 (string= (nous-messages-endpoint)
                          "https://inference.nous.test/v1/messages"))
            "Nous portal and inference base URL overrides are independent")
           (nous-provider-test--save-credentials configuration)
           (setf (symbol-function 'dexador:get)
                 (lambda (url &rest arguments)
                   (push (list :url url
                               :headers (getf arguments :headers))
                         captured-requests)
                   (values
                    (json-encode
                     (json-object
                      "data"
                      (json-array
                       (json-object "id" "anthropic/claude-test")
                       (json-object "id" "Nous-Hermes-4")
                       (json-object "id" "open-model")
                       (json-object "id" "open-model"))))
                    200
                    nil)))
           (let ((models (nous--fetch-models configuration)))
             (test-assert
              (equal models '("anthropic/claude-test" "open-model"))
              "Nous discovery filters Hermes identifiers and removes duplicates"))
           (let* ((request (first captured-requests))
                  (authorization
                    (nous-provider-test--header-value
                     "Authorization"
                     (getf request :headers))))
             (test-assert
              (and (string= (getf request :url)
                            "https://inference.nous.test/v1/models")
                   (uiop:string-prefix-p "Bearer " authorization)
                   (search (nous-test--jwt) authorization))
              "Nous model discovery uses the scoped access JWT as a Bearer credential"))
           (test-assert
            (handler-case
                (progn
                  (openai-compatible--signal-model-discovery-status
                   "Nous Research"
                   (nous-credential-manager-create configuration)
                   401)
                  nil)
              (authentication-error (condition)
                (and (test-object-contains-string-p condition "credentials")
                     (not (test-object-contains-string-p condition "API key")))))
            "OAuth model-discovery failures describe credentials rather than an API key")
           (provider--refresh-registration-models registration configuration)
           (let* ((chat-configuration
                    (configuration-with-model configuration "open-model"))
                  (messages-configuration
                    (configuration-with-model
                     configuration
                     "anthropic/claude-test")))
             (test-assert
              (string= (configuration-provider-endpoint chat-configuration)
                       "https://override.nous.test/chat")
              "AUTOLITH_NOUS_PROVIDER_ENDPOINT overrides the Chat Completions route")
             (let ((chat (provider-create chat-configuration))
                   (messages (provider-create messages-configuration)))
               (test-assert
                (and (typep chat 'nous-chat-completions-provider)
                     (typep messages 'nous-messages-provider)
                     (eq (provider-family chat) ':nous)
                     (eq (provider-family messages) ':nous))
                "Nous selects Chat Completions or Messages from the model prefix")
               (let ((switched
                       (provider-with-configuration
                        chat
                        messages-configuration)))
                 (test-assert
                  (and (typep switched 'nous-messages-provider)
                       (eq (provider-credential-manager switched)
                           (provider-credential-manager chat))
                       (string= (provider-session-id switched)
                                (provider-session-id chat)))
                  "a Nous model switch preserves credentials and session state")))))
      (setf (symbol-function 'dexador:get) original-get)
      (provider--registry-restore registry-snapshot)
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_PORTAL_URL"
       saved-portal)
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_INFERENCE_BASE_URL"
       saved-base)
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_PROVIDER_ENDPOINT"
       saved-provider)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)


;;;; -- Dual-Wire Transport --

(-> nous-provider-test--transport () null)
(defun nous-provider-test--transport ()
  "Test Chat Completions and Messages endpoints and Bearer headers."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registration (provider-registration-find "nous"))
         (original-get (symbol-function 'dexador:get))
         (original-post (symbol-function 'dexador:post))
         (saved-base (uiop:getenv "AUTOLITH_NOUS_INFERENCE_BASE_URL"))
         (saved-provider (uiop:getenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT"))
         (posts nil))
    (unwind-protect
         (progn
           (platform-setenv "AUTOLITH_NOUS_INFERENCE_BASE_URL"
                            "https://transport.nous.test/v1")
           (platform-unsetenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT")
           (nous-provider-test--save-credentials configuration)
           (setf (symbol-function 'dexador:get)
                 (lambda (url &rest arguments)
                   (declare (ignore url arguments))
                   (values
                    (json-encode
                     (json-object
                      "data"
                      (json-array
                       (json-object "id" "open-model")
                       (json-object "id" "anthropic/claude-test"))))
                    200
                    nil)))
           (provider--refresh-registration-models registration configuration)
           (let* ((chat-configuration
                    (configuration-with-model configuration "open-model"))
                  (messages-configuration
                    (configuration-with-model
                     configuration
                     "anthropic/claude-test"))
                  (chat (provider-create chat-configuration))
                  (messages (provider-create messages-configuration))
                  (credentials
                    (nous-authentication-test--credentials
                     (configuration-nous-auth-path configuration)
                     :access-token "transport-access-secret"
                     :refresh-token "transport-refresh-secret"))
                  (chat-conversation
                    (conversation-create
                     chat-configuration
                     :identifier "nous-chat-transport"))
                  (messages-conversation
                    (conversation-create
                     messages-configuration
                     :identifier "nous-messages-transport")))
             (setf (symbol-function 'dexador:post)
                   (lambda (url &rest arguments)
                     (push (list :url url
                                 :headers (getf arguments :headers)
                                 :content (getf arguments :content))
                           posts)
                     (values (make-string-input-stream "") 200 nil)))
             (provider-open-response-stream
              chat
              (json-object "model" "open-model")
              :credentials credentials
              :conversation chat-conversation)
             (provider-open-response-stream
              messages
              (json-object "model" "anthropic/claude-test")
              :credentials credentials
              :conversation messages-conversation)
             (let* ((ordered (reverse posts))
                    (chat-request (first ordered))
                    (messages-request (second ordered))
                    (chat-headers (getf chat-request :headers))
                    (messages-headers (getf messages-request :headers)))
               (test-assert
                (and (string= (getf chat-request :url)
                              "https://transport.nous.test/v1/chat/completions")
                     (string=
                      (nous-provider-test--header-value
                       "Authorization"
                       chat-headers)
                      "Bearer transport-access-secret"))
                "Nous Chat Completions uses the configured endpoint and Bearer auth")
               (test-assert
                (and (string= (getf messages-request :url)
                              "https://transport.nous.test/v1/messages")
                     (string=
                      (nous-provider-test--header-value
                       "Authorization"
                       messages-headers)
                      "Bearer transport-access-secret")
                     (string=
                      (nous-provider-test--header-value
                       "anthropic-version"
                       messages-headers)
                      *anthropic-api-version*)
                     (null
                      (nous-provider-test--header-value
                       "x-api-key"
                       messages-headers)))
                "Nous Messages uses native Anthropic framing without x-api-key auth"))))
      (setf (symbol-function 'dexador:get) original-get
            (symbol-function 'dexador:post) original-post)
      (provider--registry-restore registry-snapshot)
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_INFERENCE_BASE_URL"
       saved-base)
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_PROVIDER_ENDPOINT"
       saved-provider)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-nous-provider () null)
(defun test-nous-provider ()
  "Run the built-in Nous Research provider tests."
  (nous-provider-test--registration-and-discovery)
  (nous-provider-test--transport)
  nil)
