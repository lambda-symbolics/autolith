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

(-> opencode-provider-test--restore-environment
    (string (option string))
    null)
(defun opencode-provider-test--restore-environment (name value)
  "Restore NAME to VALUE, preserving an originally absent variable."
  (if value
      (sb-posix:setenv name value 1)
      (sb-posix:unsetenv name))
  nil)

(-> opencode-provider-test--selection () null)
(defun opencode-provider-test--selection ()
  "Test OpenCode endpoint defaults and independent environment overrides."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (saved-chat (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"))
         (saved-models
           (uiop:getenv *opencode-models-environment-variable*)))
    (unwind-protect
         (progn
           (register-provider
            "opencode-endpoint-test"
            :family ':opencode
            :models '("opencode/endpoint-test")
            :endpoint *opencode-chat-completions-endpoint*
            :factory
            (lambda (selected &key reasoning-summaries-p)
              (declare (ignore reasoning-summaries-p))
              (opencode-provider-create selected))
            :source ':runtime)
           (setf (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT") ""
                 (uiop:getenv *opencode-models-environment-variable*) "")
           (let ((selected
                   (configuration-with-model
                    configuration "opencode/endpoint-test")))
             (test-assert
              (string= (configuration-provider-endpoint selected)
                       *opencode-chat-completions-endpoint*)
              "OpenCode uses its default chat endpoint")
             (test-assert
              (string= (opencode-models-endpoint) *opencode-models-endpoint*)
              "OpenCode uses its default models endpoint"))
           (setf (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT")
                 "https://chat.invalid/v1/chat/completions"
                 (uiop:getenv *opencode-models-environment-variable*)
                 "https://models.invalid/v1/models")
           (let ((selected
                   (configuration-with-model
                    configuration "opencode/endpoint-test")))
             (test-assert
              (string= (configuration-provider-endpoint selected)
                       "https://chat.invalid/v1/chat/completions")
              "the OpenCode chat override takes precedence over registration metadata")
             (let* ((provider (opencode-provider-create selected))
                    (conversation
                      (conversation-create
                       selected :identifier "opencode-chat-endpoint"))
                    (credentials
                      (make-instance 'oauth-credentials
                                     :access-token "synthetic-opencode-key"
                                     :account-id "opencode"
                                     :source-path #P"/tmp/opencode-test-key"))
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
                   :credentials credentials
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--credential-source () null)
(defun opencode-provider-test--credential-source ()
  "Test the OpenCode environment credential source without network access."
  (let* ((configuration (opencode-provider-test--configuration))
         (root (test-configuration-root configuration))
         (manager (opencode-credential-manager-create configuration))
         (saved (uiop:getenv "OPENCODE_API_KEY")))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source manager)
            (make-instance 'oauth-credentials
                           :access-token "saved-opencode-key"
                           :refresh-token nil
                           :id-token nil
                           :account-id "opencode"
                           :expires-at nil
                           :source-path
                           (configuration-opencode-auth-path configuration)))
           (setf (uiop:getenv "OPENCODE_API_KEY") "environment-key-a")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key-a")
            "the environment key takes precedence over the saved key")
           (setf (uiop:getenv "OPENCODE_API_KEY") "")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "saved-opencode-key")
            "the saved interactive key is the environment fallback"))
      (opencode-provider-test--restore-environment "OPENCODE_API_KEY" saved)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--login () null)
(defun opencode-provider-test--login ()
  "Test hidden OpenCode key entry, secret scope, and private persistence."
  (let* ((configuration (opencode-provider-test--configuration))
         (root (test-configuration-root configuration))
         (manager (opencode-credential-manager-create configuration))
         (original-save (symbol-function 'credential-source-save))
         (read-secret-use-active-p nil)
         (save-secret-use-active-p nil)
         (observed-provider nil)
         (observed-note nil)
         (observed-descriptor nil)
         (stored nil))
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
                 :input *standard-input*)
                "OpenCode authentication was saved by Autolith.")
                "OpenCode login reports successful private persistence"))
            (setf stored
                  (credential-source-load
                   (credential-manager-primary-source manager)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))
    (test-assert
     (and read-secret-use-active-p
          save-secret-use-active-p
          (not (secret-use-active-p)))
     "OpenCode reads and saves the key only inside secret-use scope")
    (test-assert
     (and (string= observed-provider "OpenCode")
          (= observed-descriptor 23)
          (search "OPENCODE_API_KEY overrides the stored key" observed-note))
     "OpenCode uses the shared hidden prompt and forwards its input descriptor")
    (test-assert
     (and (typep stored 'oauth-credentials)
          (string= (oauth-credentials-access-token stored)
                   "opencode-test-key"))
     "OpenCode trims and persists the entered key"))
  (let* ((configuration (opencode-provider-test--configuration))
         (root (test-configuration-root configuration)))
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
                    :input (make-string-input-stream ""))
                   nil)
               (authentication-error ()
                 t))
             "OpenCode rejects an empty entered key")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (test-assert (not (secret-use-active-p))
               "OpenCode releases secret-use scope after login failure")
  nil)

(-> opencode-provider-test--authentication-bootstrap () null)
(defun opencode-provider-test--authentication-bootstrap ()
  "Test named authentication bootstraps OpenCode and refreshes model discovery."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (discovery-called-p nil)
         (login-called-p nil))
    (unwind-protect
         (progn
           (register-provider
            "opencode"
            :family ':opencode
            :endpoint *opencode-chat-completions-endpoint*
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
            :authenticator #'provider--opencode-registration-authenticator
            :source ':runtime)
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
                        :stream (make-string-output-stream)
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> opencode-provider-test--request-model () null)
(defun opencode-provider-test--request-model ()
  "Test namespaced model selection and raw OpenCode wire transmission."
  (let* ((configuration
           (opencode-provider-test--configuration
            :model (opencode--model-name "gpt-5.6-luna")))
         (root (test-configuration-root configuration))
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
              (string= (configuration-model configuration)
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--discovery () null)
(defun opencode-provider-test--discovery ()
  "Test OpenCode discovery namespacing, collisions, caching, and failures."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (opencode-provider-test--configuration))
         (root (test-configuration-root configuration))
         (original-get (symbol-function 'dexador:get))
         (original-registration (provider-registration-find "opencode"))
         (saved-discovered-models
           (copy-list
            (provider-registration-discovered-models original-registration)))
         (saved-key (uiop:getenv *opencode-environment-variable*))
         (saved-endpoint
           (uiop:getenv *opencode-models-environment-variable*))
         (observed-url nil)
         (observed-headers nil)
         (body
           "{\"data\":[{\"id\":\"gpt-5.6-luna\"},{\"id\":\"grok-4.5\"},{\"id\":\"kimi-k3\"}]}"))
    (unwind-protect
         (progn
           (setf (uiop:getenv *opencode-environment-variable*)
                 "synthetic-opencode-key"
                 (uiop:getenv *opencode-models-environment-variable*)
                 "https://models.invalid/v1/models"
                 (symbol-function 'dexador:get)
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
                      "opencode"))
            "OpenCode discovery cannot steal colliding built-in model names")
           (let ((description
                   (application--models-description
                    (make-instance 'application :configuration configuration))))
             (test-assert
              (and (search "opencode / opencode/gpt-5.6-luna" description)
                   (search "opencode / opencode/grok-4.5" description))
              "/models exposes dynamic-only OpenCode models under their namespace"))
           (let* ((entries
                    (provider--read-model-cache
                     (configuration-provider-model-cache-path configuration)))
                  (entry
                    (find "opencode" entries
                          :key (lambda (candidate)
                                 (getf candidate :provider-name))
                          :test #'string=))
                  (models
                    (mapcar #'provider-model-name (getf entry :models))))
             (test-assert
              (and entry
                   (every
                    (lambda (model)
                      (uiop:string-prefix-p *opencode-model-prefix* model))
                    models))
              "the provider cache persists only namespaced OpenCode models"))
           (register-provider
            "opencode"
            :description "OpenCode (zen/go)"
            :family ':opencode
            :protocol ':chat-completions
            :endpoint *opencode-chat-completions-endpoint*
            :factory #'provider--opencode-registration-factory
            :authenticator #'provider--opencode-registration-authenticator
            :model-discovery #'opencode--fetch-models
            :model-discovery-endpoint *opencode-models-endpoint*
            :model-discovery-endpoint-resolver #'opencode-models-endpoint
            :source ':builtin)
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
           (setf (uiop:getenv *opencode-models-environment-variable*)
                 "https://other-models.invalid/v1/models")
           (provider-load-model-cache configuration)
           (test-assert
            (not (member "opencode/gpt-5.6-luna"
                         (provider-model-identifiers)
                         :test #'string=))
            "OpenCode rejects cached models from another discovery endpoint")
           (setf (uiop:getenv *opencode-models-environment-variable*)
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
                     :body "unauthorized"
                     :status 401
                     :headers nil
                     :uri "https://models.invalid/v1/models"
                     :method ':get))))
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
                     :body "overloaded"
                     :status 503
                     :headers nil
                     :uri "https://models.invalid/v1/models"
                     :method ':get))))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> opencode-provider-test--legacy-registry-snapshot () null)
(defun opencode-provider-test--legacy-registry-snapshot ()
  "Test legacy registry snapshots recover dynamic models across re-registration."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider-name "opencode-legacy-snapshot-test")
         (static-model "opencode/legacy-static")
         (dynamic-model "opencode/legacy-dynamic"))
    (labels ((register ()
               "Register the legacy-snapshot test provider."
               (register-provider
                provider-name
                :family ':opencode
                :models (list static-model)
                :factory #'provider--opencode-registration-factory
                :model-discovery
                (lambda (selected-configuration)
                  (declare (ignore selected-configuration))
                  (list dynamic-model))
                :model-discovery-endpoint
                "https://legacy-snapshot.invalid/v1/models"
                :source ':runtime)))
      (unwind-protect
           (progn
             (register)
             (test-assert
              (null (provider-refresh-models
                     configuration :provider-name provider-name))
              "the legacy-snapshot provider discovers its dynamic model")
             (let* ((registration
                      (provider-registration-find provider-name))
                    (current-snapshot (provider--registry-snapshot))
                    (legacy-snapshot
                      (list
                       :registrations
                       (copy-list (getf current-snapshot ':registrations))
                       :models
                       (loop for model-snapshot
                               in (getf current-snapshot ':models)
                             collect
                             (list (first model-snapshot)
                                   (copy-list (second model-snapshot))))
                       :sequence (getf current-snapshot ':sequence))))
               (with-recursive-lock-held (*provider-registry-lock*)
                 (setf (slot-value registration 'discovered-models) nil
                       (slot-value registration 'models)
                       (copy-list
                        (provider-registration-declared-models registration)))
                 (provider--refresh-model-settings))
               (provider--registry-restore legacy-snapshot)
               (test-assert
                (equal
                 (mapcar #'provider-model-name
                         (provider-registration-discovered-models registration))
                 (list dynamic-model))
                "legacy registry restoration recovers exposed dynamic models")
               (register)
               (test-assert
                (equal
                 (mapcar
                  #'provider-model-name
                  (provider-registration-models
                   (provider-registration-find provider-name)))
                 (list static-model dynamic-model))
                "re-registration retains legacy-restored dynamic models")))
        (provider--registry-restore registry-snapshot)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
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


(-> test-opencode-provider () null)
(defun test-opencode-provider ()
  "Run the complete OpenCode provider test suite."
  (opencode-provider-test--selection)
  (opencode-provider-test--credential-source)
  (opencode-provider-test--login)
  (opencode-provider-test--authentication-bootstrap)
  (opencode-provider-test--request-model)
  (opencode-provider-test--discovery)
  (opencode-provider-test--legacy-registry-snapshot)
  (opencode-provider-test--builtin-registration)
  nil)
