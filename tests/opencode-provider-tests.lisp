(in-package #:autolith)

;;;; -- OpenCode Chat Completions Provider Tests --

(-> opencode-provider-test--configuration
    (&key (:model non-empty-string) (:provider-endpoint non-empty-string))
    configuration)
(defun opencode-provider-test--configuration
    (&key
       (model (opencode--model-name "gpt-5.6-luna"))
       (provider-endpoint *opencode-chat-completions-endpoint*))
  "Return an isolated test configuration for one namespaced OpenCode MODEL."
  (declare (ignore provider-endpoint))
  (let ((configuration (configuration-copy (test-configuration))))
    (setf (configuration-validation-deferred-p configuration) t
          (config :model configuration) model
          (config :reasoning-effort configuration) *default-reasoning-effort*)
    configuration))

(-> opencode-provider-test--restore-environment
    (string (option string))
    null)
(defun opencode-provider-test--restore-environment (name value)
  "Restore NAME to VALUE, preserving an originally absent variable."
  (if value
      (platform-setenv name value)
      (platform-unsetenv name))
  nil)

(-> opencode-provider-test--selection () null)
(defun opencode-provider-test--selection ()
  "Test OpenCode endpoint defaults and independent environment overrides."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration     (test-configuration))
         (root              (test-configuration-root configuration))
         (saved-chat        (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"))
         (saved-models
           (uiop:getenv *opencode-models-environment-variable*)))
    (unwind-protect
         (progn
           (register-provider
            "opencode-endpoint-test"
            :family   ':opencode
            :models   '("opencode/endpoint-test")
            :endpoint *opencode-chat-completions-endpoint*
            :factory
            (lambda (selected &key reasoning-summaries-p)
              (declare (ignore reasoning-summaries-p))
              (opencode-provider-create selected))
            :source   ':runtime)
           (platform-setenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT" "")
           (platform-setenv *opencode-models-environment-variable* "")
           (let ((selected
                   (configuration-copy
                    configuration :model "opencode/endpoint-test")))
             (test-assert
              (string= (config :provider-endpoint selected)
                       *opencode-chat-completions-endpoint*)
              "OpenCode uses its default chat endpoint")
             (test-assert
              (string= (opencode-models-endpoint) *opencode-models-endpoint*)
              "OpenCode uses its default models endpoint"))
           (platform-setenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"
                            "https://chat.invalid/v1/chat/completions")
           (platform-setenv *opencode-models-environment-variable*
                            "https://models.invalid/v1/models")
           (let ((selected
                   (configuration-copy
                    configuration :model "opencode/endpoint-test")))
             (test-assert
              (string= (config :provider-endpoint selected)
                       "https://chat.invalid/v1/chat/completions")
              "the OpenCode chat override takes precedence over registration metadata")
             (let* ((provider     (opencode-provider-create selected))
                    (conversation
                      (conversation-create
                       selected :identifier "opencode-chat-endpoint"))
                    (credentials
                      (make-instance 'oauth-credentials
                                     :access-token "synthetic-opencode-key"
                                     :account-id   "opencode"
                                     :source-path  #P"/tmp/opencode-test-key"))
                    (observed-url nil))
               (test-call-with-function-replacements
                (list
                 (list 'dexador:post
                       (lambda (url &rest arguments)
                         (declare (ignore arguments))
                         (setf observed-url url)
                         (values (make-string-input-stream "") 200 nil nil))))
                (lambda ()
                  (provider-open-response-stream
                   provider
                   (json-object "model" "endpoint-test")
                   :credentials  credentials
                   :conversation conversation)))
               (test-assert
                (string= observed-url "https://chat.invalid/v1/chat/completions")
                "OpenCode transport sends chat requests to the chat override"))
             (test-assert
              (string= (opencode-models-endpoint)
                       "https://models.invalid/v1/models")
              "the models override remains independent of the chat endpoint")))
      (opencode-provider-test--restore-environment
       "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT" saved-chat)
      (opencode-provider-test--restore-environment
       *opencode-models-environment-variable* saved-models)
      (provider--registry-restore registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--credential-source () null)
(defun opencode-provider-test--credential-source ()
  "Test the OpenCode environment credential source without network access."
  (let* ((configuration (opencode-provider-test--configuration))
         (root          (test-configuration-root configuration))
         (manager       (opencode-credential-manager-create configuration))
         (saved         (uiop:getenv "OPENCODE_API_KEY")))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source manager)
            (make-instance 'oauth-credentials
                           :access-token  "saved-opencode-key"
                           :refresh-token nil
                           :id-token      nil
                           :account-id    "opencode"
                           :expires-at    nil
                           :source-path
                           (configuration-opencode-auth-path configuration)))
           (platform-setenv "OPENCODE_API_KEY" "environment-key-a")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key-a")
            "the environment key takes precedence over the saved key")
           (platform-setenv "OPENCODE_API_KEY" "")
           (let* ((credentials (credential-manager-load manager))
                  (secrets     (cl-rfc8628:oauth-credentials-secret-values credentials)))
             (test-assert
              (string= (oauth-credentials-access-token credentials) "saved-opencode-key")
              "the saved interactive key is the environment fallback")
             (test-assert
              (string= (oauth-credentials-account-id credentials) "api-key/opencode")
              "legacy and environment keys identify the same synthetic account")
             (test-assert
              (equal (oauth-credentials-source-path credentials)
                     (configuration-opencode-auth-path configuration))
              "normalizing a legacy account preserves its credential source")
             (test-assert
              (string= (cl-rfc8628:redact-exact-string-values
                        "opencode saved-opencode-key" secrets "[redacted]")
                       "opencode [redacted]")
              "request redaction removes the actual key without corrupting provider text"))
           (platform-setenv "OPENCODE_API_KEY" "environment-key-b")
           (test-assert
            (string= (oauth-credentials-access-token (credential-manager-load manager))
                     "environment-key-b")
            "the same account can return from the saved key to the environment"))
      (opencode-provider-test--restore-environment "OPENCODE_API_KEY" saved)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--login () null)
(defun opencode-provider-test--login ()
  "Test hidden OpenCode key entry, secret scope, and private persistence."
  (let* ((configuration            (opencode-provider-test--configuration))
         (root                     (test-configuration-root configuration))
         (manager                  (opencode-credential-manager-create configuration))
         (original-save            (symbol-function 'credential-source-save))
         (read-secret-use-active-p nil)
         (save-secret-use-active-p nil)
         (observed-provider        nil)
         (observed-note            nil)
         (observed-descriptor      nil)
         (stored                   nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'api-key-read-hidden
                 (lambda (provider-name
                          &key input input-file-descriptor stream note)
                   (declare (ignore input stream))
                   (setf read-secret-use-active-p (secret-use-active-p)
                         observed-provider provider-name
                         observed-note note
                         observed-descriptor input-file-descriptor)
                   "  opencode-test-key  "))
           (list 'credential-source-save
                 (lambda (source credentials)
                   (setf save-secret-use-active-p (secret-use-active-p))
                   (funcall original-save source credentials))))
          (lambda ()
            (let ((*api-key-input-file-descriptor* 23))
              (test-assert
               (string=
                (opencode-api-key-login
                 manager
                 :stream (make-string-output-stream)
                 :input  *standard-input*)
                "OpenCode Go authentication was saved by Autolith.")
               "OpenCode login reports successful private persistence"))
            (setf stored
                  (credential-source-load
                   (credential-manager-primary-source manager)))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    (test-assert
     (and read-secret-use-active-p
          save-secret-use-active-p
          (not (secret-use-active-p)))
     "OpenCode reads and saves the key only inside secret-use scope")
    (test-assert
     (and (string= observed-provider "OpenCode Go")
          (= observed-descriptor 23)
          (search "OPENCODE_API_KEY overrides the stored key" observed-note))
     "OpenCode uses the shared hidden prompt and forwards its input descriptor")
    (test-assert
     (and (typep stored 'oauth-credentials)
          (string= (oauth-credentials-access-token stored)
                   "opencode-test-key"))
     "OpenCode trims and persists the entered key"))
  (let* ((configuration (opencode-provider-test--configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'api-key-read-hidden
                 (lambda (provider-name
                          &key input input-file-descriptor stream note)
                   (declare
                    (ignore provider-name input input-file-descriptor stream note))
                   "  ")))
          (lambda ()
            (test-assert
             (handler-case
                 (progn
                   (opencode-api-key-login
                    (opencode-credential-manager-create configuration)
                    :stream (make-string-output-stream)
                    :input  (make-string-input-stream ""))
                   nil)
               (authentication-error ()
                 t))
             "OpenCode rejects an empty entered key")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (test-assert (not (secret-use-active-p))
               "OpenCode releases secret-use scope after login failure")
  nil)

(-> opencode-provider-test--authentication-bootstrap () null)
(defun opencode-provider-test--authentication-bootstrap ()
  "Test named authentication bootstraps OpenCode and refreshes model discovery."
  (let* ((registry-snapshot  (provider--registry-snapshot))
         (configuration      (test-configuration))
         (root               (test-configuration-root configuration))
         (discovery-called-p nil)
         (login-called-p     nil))
    (unwind-protect
         (progn
           (register-provider
            "opencode"
            :family                   ':opencode
            :endpoint                 *opencode-chat-completions-endpoint*
            :model-discovery
            (lambda (selected)
              (declare (ignore selected))
              (setf discovery-called-p t)
              '("opencode/live-model"))
            :model-discovery-endpoint *opencode-models-endpoint*
            :factory
            (lambda (selected &key reasoning-summaries-p)
              (declare (ignore reasoning-summaries-p))
              (opencode-provider-create selected))
            :authenticator            #'provider--opencode-registration-authenticator
            :source                   ':runtime)
           (test-call-with-function-replacements
            (list
             (list 'opencode-api-key-login
                   (lambda (manager &key stream input input-file-descriptor)
                     (declare
                      (ignore manager stream input input-file-descriptor))
                     (setf login-called-p t)
                     "OpenCode authentication was saved by Autolith.")))
            (lambda ()
              (let* ((provider
                       (provider-authentication-provider
                        configuration "opencode"))
                     (message
                       (provider-authenticate
                        provider
                        :stream         (make-string-output-stream)
                        :open-browser-p nil)))
                (test-assert
                 (typep provider 'opencode-chat-completions-provider)
                 "named OpenCode authentication creates its provider without models")
                (test-assert
                 (and login-called-p
                      (string= message
                               "OpenCode authentication was saved by Autolith."))
                 "named OpenCode authentication dispatches to hidden-key login"))))
           (test-assert
            (and discovery-called-p
                 (member "opencode/live-model"
                         (provider-model-identifiers)
                         :test #'string=))
            "named OpenCode authentication immediately refreshes model discovery"))
      (provider--registry-restore registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--request-model () null)
(defun opencode-provider-test--request-model ()
  "Test namespaced model selection and raw OpenCode wire transmission."
  (let* ((configuration
           (opencode-provider-test--configuration
            :model (opencode--model-name "gpt-5.6-luna")))
         (root     (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "opencode-wire-model"))
         (provider (opencode-provider-create configuration)))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "hello")
           (multiple-value-bind (request delivery)
               (provider-request-object provider conversation #())
             (declare (ignore delivery))
             (test-assert
              (string= (json-get request "model") "gpt-5.6-luna")
              "OpenCode strips its namespace from the transmitted model")
             (test-assert
              (string= (config :model configuration)
                       "opencode/gpt-5.6-luna")
              "OpenCode retains the namespaced model in local configuration"))
           (test-assert
            (handler-case
                (progn
                  (opencode--wire-model-name "gpt-5.6-luna")
                  nil)
              (configuration-error ()
                t))
            "OpenCode rejects an unnamespaced local model identifier"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--session-header () null)
(defun opencode-provider-test--session-header ()
  "Test that OpenCode transport sends its stable session caching header."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration     (opencode-provider-test--configuration))
         (root              (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "opencode-session"))
         (provider          (opencode-provider-create configuration))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "synthetic-opencode-key"
                          :account-id   "opencode"
                          :source-path  #P"/tmp/opencode-test-key"))
         (captured-headers  nil))
    (unwind-protect
         (progn
           (register-provider
            "opencode-session-test"
            :family   ':opencode
            :models   '("opencode/session-test")
            :endpoint *opencode-chat-completions-endpoint*
            :factory
            (lambda (selected &key reasoning-summaries-p)
              (declare (ignore reasoning-summaries-p))
              (opencode-provider-create selected))
            :source   ':runtime)
           (test-call-with-function-replacements
            (list
             (list 'dexador:post
                   (lambda (url &key headers &allow-other-keys)
                     (declare (ignore url))
                     (setf captured-headers headers)
                     (values (make-string-input-stream "") 200 nil nil))))
            (lambda ()
              (provider-open-response-stream
               provider
               (json-object "model" "gpt-5.6-luna")
               :credentials  credentials
               :conversation conversation)))
           (test-assert
            (string= (rest (assoc "x-opencode-session" captured-headers
                                  :test #'string-equal))
                     (provider-session-id provider))
            "OpenCode transport sends the stable session caching header")
           (test-assert
            (string= (rest (assoc "x-opencode-session"
                                  (openai-compatible-provider-headers provider)
                                  :test #'string-equal))
                     (provider-session-id provider))
            "the OpenCode session header carries the provider session identity")
           (let ((reconfigured
                   (provider-with-configuration
                    provider
                    (configuration-copy
                     configuration :model "opencode/session-test"))))
             (test-assert
              (equal (openai-compatible-provider-headers reconfigured)
                     (list (cons "x-opencode-session"
                                 (provider-session-id provider))))
              "OpenCode reconfiguration preserves the session caching header")
             (test-assert
              (string= (provider-session-id reconfigured)
                       (provider-session-id provider))
              "OpenCode reconfiguration preserves the session identity")))
      (provider--registry-restore registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--discovery () null)
(defun opencode-provider-test--discovery ()
  "Test OpenCode discovery namespacing, collisions, caching, and failures."
  (let* ((registry-snapshot     (provider--registry-snapshot))
         (configuration         (opencode-provider-test--configuration))
         (root                  (test-configuration-root configuration))
         (original-get          (symbol-function 'dexador:get))
         (original-registration (provider-registration-find "opencode"))
         (saved-discovered-models
           (copy-list
            (provider-registration-discovered-models original-registration)))
         (saved-key             (uiop:getenv *opencode-environment-variable*))
         (saved-endpoint
           (uiop:getenv *opencode-models-environment-variable*))
         (observed-url          nil)
         (observed-headers      nil)
         (body
           "{\"data\":[{\"id\":\"gpt-5.6-luna\",\"limit\":{\"context\":1000000}},{\"id\":\"grok-4.5\"},{\"id\":\"kimi-k3\"}]}"))
    (unwind-protect
         (progn
           (platform-setenv *opencode-environment-variable* "synthetic-opencode-key")
           (platform-setenv *opencode-models-environment-variable*
                            "https://models.invalid/v1/models")
           (setf (symbol-function 'dexador:get)
                 (lambda (url &rest arguments)
                   (setf observed-url url
                         observed-headers (getf arguments :headers))
                   (values body 200 nil nil)))
           (test-assert
            (null (provider-refresh-models
                   configuration :provider-name "opencode"))
            "OpenCode model discovery succeeds")
           (test-assert
            (and (string= observed-url "https://models.invalid/v1/models")
                 (search "synthetic-opencode-key"
                         (rest (assoc "Authorization"
                                      observed-headers
                                      :test #'string=))))
            "OpenCode discovery uses its independent endpoint and request-time key")
           (dolist (model
                    '("opencode/gpt-5.6-luna"
                      "opencode/grok-4.5"
                      "opencode/kimi-k3"))
             (test-assert
              (member model (provider-model-identifiers) :test #'string=)
              (format nil "OpenCode discovery publishes namespaced model ~A" model)))
           (test-assert
            (eql (provider-model-context-window-for "opencode/gpt-5.6-luna")
                 1000000)
            "OpenCode discovery propagates the nested context limit")
           (test-assert
            (and
             (string= (provider-registration-name
                       (provider-registration-for-model "gpt-5.6-luna"))
                      "chatgpt")
             (string= (provider-registration-name
                       (provider-registration-for-model "grok-4.5"))
                      "grok")
             (string= (provider-registration-name
                       (provider-registration-for-model
                        "opencode/gpt-5.6-luna"))
                      "opencode-go"))
            "OpenCode discovery cannot steal colliding built-in model names")
           (let ((items
                   (application--model-items
                    (make-instance 'application :configuration configuration))))
             (dolist (model '("opencode/gpt-5.6-luna" "opencode/grok-4.5"))
               (let ((item (find model items :key (lambda (item) (getf item :model))
                                            :test #'string=)))
                  (test-assert (and item (equal (getf item :provider) "opencode-go"))
                              "/models offers dynamic OpenCode models with their provider identity")
                 (when item
                   (multiple-value-bind (identifier registration)
                       (provider-resolve-model (getf item :name))
                     (test-assert
                      (and (equal identifier model)
                           (equal (provider-registration-name registration) "opencode-go"))
                      "offered OpenCode selections retain the adapter's namespaced catalog ID"))))))
           (let* ((cache   (provider--read-model-cache configuration))
                  (entries (getf (rest cache) :providers))
                  (entry
                    (find "opencode-go" entries
                          :key  (lambda (candidate)
                                  (getf candidate :provider-name))
                          :test #'string=))
                  (models
                    (mapcar (lambda (model) (getf model :name)) (getf entry :models))))
             (test-assert
              (and entry
                   (every
                    (lambda (model)
                      (uiop:string-prefix-p *opencode-model-prefix* model))
                    models))
              "the provider cache persists only namespaced OpenCode models")
             (setf (getf entry :provider-name) "opencode")
             (provider--write-model-cache configuration cache)
             (provider--registry-restore registry-snapshot)
             (provider-load-model-cache configuration)
             (test-assert (not (provider-model-for "opencode/gpt-5.6-luna"))
                          "The old mixed-tier provider cache cannot populate Go.")
             (setf (getf entry :provider-name) "opencode-go")
             (provider--write-model-cache configuration cache))
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
           (let ((registration (provider-registration-find "opencode")))
             (test-assert
              (and (null (provider-registration-discovered-models registration))
                   (not (member "opencode/gpt-5.6-luna"
                                (provider-model-identifiers)
                                :test #'string=)))
              "re-registering resolver-backed OpenCode discards stale discoveries"))
           (provider--registry-restore registry-snapshot)
           (test-assert
            (equal (provider-registration-discovered-models
                    original-registration)
                   saved-discovered-models)
            "registry restoration restores discovered provider models")
           (platform-setenv *opencode-models-environment-variable*
                            "https://other-models.invalid/v1/models")
           (provider-load-model-cache configuration)
           (test-assert
            (not (member "opencode/gpt-5.6-luna"
                         (provider-model-identifiers)
                         :test #'string=))
            "OpenCode rejects cached models from another discovery endpoint")
           (platform-setenv *opencode-models-environment-variable*
                            "https://models.invalid/v1/models")
           (provider-load-model-cache configuration)
           (test-assert
            (member "opencode/gpt-5.6-luna"
                    (provider-model-identifiers)
                    :test #'string=)
            "matching cached OpenCode model names remain namespaced after reload")
           (setf (symbol-function 'dexador:get)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error
                    (make-condition
                     'dexador.error:http-request-unauthorized
                     :body    "unauthorized"
                     :status  401
                     :headers nil
                     :uri     "https://models.invalid/v1/models"
                     :method  ':get))))
           (test-assert
            (handler-case
                (progn
                  (opencode--fetch-models configuration)
                  nil)
              (authentication-error (condition)
                (let ((message (princ-to-string condition)))
                  (and (search "auth opencode" message)
                       (not (search "could not be reached" message))))))
            "OpenCode discovery reports HTTP 401 as an authentication failure")
           (setf (symbol-function 'dexador:get)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error
                    (make-condition
                     'http-request-failed
                     :body    "overloaded"
                     :status  503
                     :headers nil
                     :uri     "https://models.invalid/v1/models"
                     :method  ':get))))
           (test-assert
            (handler-case
                (progn
                  (opencode--fetch-models configuration)
                  nil)
              (configuration-error (condition)
                (search "HTTP 503" (princ-to-string condition))))
            "OpenCode discovery preserves non-authentication HTTP status")
           (setf (symbol-function 'dexador:get)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error "Injected OpenCode discovery transport failure.")))
           (test-assert
            (handler-case
                (progn
                  (opencode--fetch-models configuration)
                  nil)
              (configuration-error (condition)
                (search "could not be reached" (princ-to-string condition))))
            "OpenCode discovery distinguishes transport failures from HTTP status"))
      (setf (symbol-function 'dexador:get) original-get)
      (opencode-provider-test--restore-environment
       *opencode-environment-variable* saved-key)
      (opencode-provider-test--restore-environment
       *opencode-models-environment-variable* saved-endpoint)
      (provider--registry-restore registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--builtin-registration () null)
(defun opencode-provider-test--builtin-registration ()
  "Test the built-in OpenCode external-boundary registration."
  (let ((registration (provider-registration-find "opencode")))
    (test-assert
     (and registration
          (eq (provider-registration-family registration) ':opencode)
          (string= (provider-registration-endpoint registration)
                   *opencode-chat-completions-endpoint*)
          (string= (provider-registration-model-discovery-endpoint registration)
                   *opencode-models-endpoint*)
          (functionp (provider-registration-model-discovery registration)))
     "OpenCode registers its family, endpoints, and discovery callback"))
  nil)

(-> opencode-provider-test--tiers () null)
(defun opencode-provider-test--tiers ()
  "Exercise independent credentials, request namespaces and transports for both tiers."
  (with-test-configuration (configuration)
    (let* ((snapshot  (provider--registry-snapshot))
           (variables '("OPENCODE_API_KEY" "OPENCODE_ZEN_API_KEY"
                        "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"
                        "AUTOLITH_OPENCODE_ZEN_PROVIDER_ENDPOINT"))
           (saved     (mapcar #'uiop:getenv variables)))
      (unwind-protect
           (progn
             (platform-setenv "OPENCODE_API_KEY" "synthetic-go-key")
             (platform-setenv "OPENCODE_ZEN_API_KEY" "synthetic-zen-key")
             (platform-setenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT" "https://go.invalid/chat/completions")
             (platform-setenv "AUTOLITH_OPENCODE_ZEN_PROVIDER_ENDPOINT" "https://zen.invalid/chat/completions")
             (dolist (tier '(("opencode-go" "opencode/" "https://go.invalid/chat/completions"
                              "synthetic-go-key" :opencode)
                             ("opencode-zen" "opencode-zen/" "https://zen.invalid/chat/completions"
                              "synthetic-zen-key" :opencode-zen)))
               (destructuring-bind (name prefix endpoint key family) tier
                 (let* ((registration (provider-registration-find name))
                        (model        (concatenate 'string prefix "deepseek-v4.1-flash")))
                   (register-provider name :models (list model)
                                      :family      family
                                      :endpoint    (provider-registration-endpoint registration)
                                      :factory     (provider-registration-factory registration)
                                      :source      ':runtime)
                   (let* ((selected     (configuration-copy configuration :model model :model-provider name))
                          (provider     (provider-create selected))
                          (manager      (provider-credential-manager provider))
                          (credentials  (credential-manager-load manager))
                          (conversation (conversation-create selected))
                          (observed-url nil))
                     (test-assert (string= (oauth-credentials-access-token credentials) key)
                                  "Each tier bootstraps only its own environment key.")
                     (test-assert (equal (config :provider-endpoint selected) endpoint)
                                  "Tier endpoint overrides are independent.")
                     (test-call-with-function-replacements
                      (list (list 'dexador:post
                                  (lambda (url &rest arguments)
                                    (declare (ignore arguments))
                                    (setf observed-url url)
                                    (values (make-string-input-stream "") 200 nil nil))))
                      (lambda ()
                        (multiple-value-bind (request delivery)
                            (provider-request-object provider conversation #())
                          (declare (ignore delivery))
                          (test-assert (string= (json-get request "model") "deepseek-v4.1-flash")
                                       "The Chat Completions request strips only its tier namespace.")
                          (provider-open-response-stream provider request
                                                         :credentials  credentials
                                                         :conversation conversation))))
                     (test-assert (string= observed-url endpoint)
                                  "Each transport sends requests to its selected tier.")
                     (test-assert
                      (handler-case
                          (progn (opencode--wire-model-name
                                  (concatenate 'string (if (eq family ':opencode)
                                                           "opencode-zen/" "opencode/")
                                               "deepseek-v4.1-flash") family)
                                 nil)
                        (configuration-error () t))
                      "A tier rejects the other namespace instead of rerouting it.")))))
             (let ((go  (opencode-credential-manager-create configuration))
                   (zen (opencode-zen-credential-manager-create configuration)))
              (test-call-with-function-replacements
               (list (list 'api-key-read-hidden
                           (lambda (label &rest arguments)
                             (declare (ignore arguments))
                             (if (string= label "OpenCode Go") "saved-go-key" "saved-zen-key"))))
               (lambda ()
                 (opencode-api-key-login go :stream (make-broadcast-stream))
                 (opencode-zen-api-key-login zen :stream (make-broadcast-stream))))
              (test-assert
               (and (string= (oauth-credentials-access-token
                              (credential-source-load (credential-manager-primary-source go)))
                             "saved-go-key")
                    (string= (oauth-credentials-access-token
                              (credential-source-load (credential-manager-primary-source zen)))
                             "saved-zen-key"))
               "Logging into either tier persists its own key without replacing the other.")))
        (loop for name in variables for value in saved
              do (opencode-provider-test--restore-environment name value))
        (provider--registry-restore snapshot))))
  nil)

(-> opencode-provider-test--startup-upgrade () null)
(defun opencode-provider-test--startup-upgrade ()
  "Keep an exact durable Go selection offline without trusting old tier metadata."
  (let ((registry-snapshot (provider--registry-snapshot)))
    (unwind-protect
         (with-test-environment (("AUTOLITH_MODEL" nil)
                                 ("AUTOLITH_MODEL_PROVIDER" nil)
                                 ("AUTOLITH_REASONING_EFFORT" nil)
                                 ("OPENCODE_API_KEY" "synthetic-upgrade-key"))
           (dolist (endpoint '(nil "https://go.invalid/v1/models"))
             (with-test-environment (("AUTOLITH_OPENCODE_MODELS_ENDPOINT" endpoint))
              (dolist (fresh-id '("upgrade-model" "replacement-model"))
               (with-test-configuration (configuration)
                 (let* ((model  "opencode/upgrade-model")
                        (legacy (list :provider-model-cache :version 3
                                      :providers
                                      (list (list :provider-name      "opencode"
                                                  :discovery-endpoint "https://old.invalid/models"
                                                  :models
                                                  (list (list :name model :context-window 200000
                                                              :description "Untrusted Zen metadata")
                                                        '(:name "opencode/zen-only-free")))))))
                   (labels ((startup (&rest overrides)
                              "Bootstrap preferences after resetting process-local metadata."
                              (provider--registry-restore registry-snapshot)
                              (register-provider "upgrade-model-collision"
                                                 :models  (list model)
                                                 :factory (provider-registration-factory
                                                           (provider-registration-find "opencode-go"))
                                                 :source  ':runtime)
                              (provider-bootstrap-configuration
                               (apply #'preferences-tests--create configuration
                                      :defer-provider-validation-p t overrides))))
                     (snapshot-write (configuration-preferences-path configuration)
                                     (list :preferences :version 8 :model model
                                           :model-provider "opencode"))
                     (provider--write-model-cache configuration legacy)
                    (let* ((created  (startup))
                           (metadata (provider-model-for model :provider "opencode-go")))
                      (test-assert (eq *provider-cache-selection-hint-model* metadata)
                                   "the upgrade hint belongs to Go despite another provider's exact ID")
                      (test-assert (equal (config :model created) model)
                                   "offline startup keeps the exact durable model")
                      (test-assert
                       (equal (provider-registration-name
                               (configuration-model-registration created)) "opencode-go")
                       "the legacy provider selection routes to Go")
                      (test-assert
                       (and metadata (not (provider-model-context-window-specified-p metadata))
                            (not (equal (provider-model-description metadata)
                                        "Untrusted Zen metadata")))
                       "the selection hint carries no old context or description")
                      (test-assert (not (provider-model-for "opencode/zen-only-free"))
                                   "the hint does not import other legacy models")
                      (test-assert
                       (null (provider-registration-models
                              (provider-registration-find "opencode-zen")))
                       "the hint grants no Zen membership")
                      (let ((*model-catalog-fetch-function*
                              (lambda (context)
                                (declare (ignore context))
                                (error 'simple-error :format-control "offline"))))
                        (test-call-with-function-replacements
                         (list (list 'dexador:get
                                     (lambda (&rest arguments)
                                       (declare (ignore arguments))
                                       (error 'simple-error :format-control "offline"))))
                         (lambda ()
                           (test-assert
                            (provider-refresh-models created :provider-name "opencode-go")
                            "offline refresh reports its discovery failure"))))
                      (test-assert
                       (and (provider-model-for model :provider "opencode-go")
                            (equal (config :model (startup)) model))
                       "failed refresh and another startup retain the saved selection")
                      (test-assert (equal legacy (provider--read-model-cache configuration))
                                   "startup hints never become successful cache metadata"))
                    (test-assert
                     (handler-case (progn (startup :model model :model-provider "opencode") nil)
                       (setting-invalid () t))
                     "explicit launch overrides do not acquire legacy hints")
                    (provider--registry-restore registry-snapshot)
                    (let ((created (startup)))
                      (if endpoint
                          (test-call-with-function-replacements
                           (list (list 'dexador:get
                                       (lambda (&rest arguments)
                                         (declare (ignore arguments))
                                         (values (format nil "{\"data\":[{\"id\":\"~A\",\"context_window\":1000000}]}" fresh-id)
                                                 200 nil nil))))
                           (lambda ()
                             (test-assert
                              (null (provider-refresh-models created :provider-name "opencode-go"))
                              "tier-specific account discovery replaces the hint")))
                          (let ((*model-catalog-fetch-function*
                                  (lambda (context)
                                    (declare (ignore context))
                                   (format nil "{\"opencode-go\":{\"models\":{\"~A\":{\"id\":\"~A\",\"tool_call\":true,\"modalities\":{\"output\":[\"text\"]},\"limit\":{\"context\":1000000}}}}}" fresh-id fresh-id))))
                            (test-assert
                             (null (provider-refresh-models created :provider-name "opencode-go"))
                             "the Go catalog replaces the hint")))
                     (test-assert
                      (= (provider-model-context-window
                          (provider-model-for (concatenate 'string "opencode/" fresh-id)
                                              :provider "opencode-go"))
                         1000000)
                      "fresh Go metadata replaces the unspecified context despite another provider's exact ID")
                     (test-assert
                      (eq (not (null (provider-model-for model :provider "opencode-go")))
                          (equal fresh-id "upgrade-model"))
                      "authoritative Go metadata retires unsupported hints despite another provider's exact ID")
                     (let ((reloaded (startup)))
                       (test-assert
                        (eq (and (equal (config :model reloaded) model)
                                 (equal (provider-registration-name
                                         (configuration-model-registration reloaded))
                                        "opencode-go"))
                            (equal fresh-id "upgrade-model"))
                        "a later startup validates the saved selection against fresh Go metadata")))
                     ;; A valid empty Go discovery result is authoritative too.
                    (when endpoint
                      (provider--write-model-cache
                       configuration
                       (list :provider-model-cache :version 3
                             :providers            (cons (list :provider-name "opencode-go"
                                                               :discovery-endpoint endpoint :models nil)
                                                         (getf (rest legacy) :providers))))
                     (let ((reloaded (startup)))
                       (test-assert
                        (not (and (equal (config :model reloaded) model)
                                  (equal (provider-registration-name
                                          (configuration-model-registration reloaded))
                                         "opencode-go")))
                        "authoritative unsupported models use normal durable fallback"))))))))))
      (provider--registry-restore registry-snapshot)))
  nil)
