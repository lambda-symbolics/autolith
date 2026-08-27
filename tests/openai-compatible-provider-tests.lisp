(in-package #:autolith)

;;;; -- OpenAI-Compatible Provider Tests --

(-> openai-compatible-provider-tests--stream-event
    (json-object string &optional t)
    json-object)
(defun openai-compatible-provider-tests--stream-event (delta response-id
                                                        &optional finish-reason)
  "Return one Chat Completions stream event containing DELTA."
  (json-object
   "id" response-id
   "choices"
   (json-array
    (json-object
     "delta" delta
     "finish_reason" finish-reason))))

(-> openai-compatible-provider-tests--save-key
    (configuration string string)
    oauth-credentials)
(defun openai-compatible-provider-tests--save-key (configuration provider-name api-key)
  "Save one test API key in CONFIGURATION's private provider-key store."
  (api-key-credential-manager-save-key
   (api-key-credential-manager-create
    :provider-name provider-name
    :pathname (configuration-api-keys-path configuration))
   api-key))

(-> openai-compatible-provider-tests--call-with-input
    (string function)
    t)
(defun openai-compatible-provider-tests--call-with-input (content function)
  "Call FUNCTION with a descriptor-backed input stream containing CONTENT."
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input nil)
          (output nil))
      (unwind-protect
           (progn
             (setf input
                   (sb-sys:make-fd-stream
                    read-descriptor
                    :input t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil)
                   output
                   (sb-sys:make-fd-stream
                    write-descriptor
                    :output t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil))
              (write-string content output)
              (finish-output output)
              (close output)
              (setf output nil)
              (funcall function input))
        (when input
          (close input))
        (when output
          (close output))
        (ignore-errors (sb-posix:close read-descriptor))
        (ignore-errors (sb-posix:close write-descriptor))))))

(-> test-openai-compatible-provider-bootstrap () null)
(defun test-openai-compatible-provider-bootstrap ()
  "Test deferred startup validation for user-defined model metadata."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (model "bootstrap/chat-model")
         (old-environment-model (uiop:getenv "AUTOLITH_MODEL"))
         (old-environment-effort (uiop:getenv "AUTOLITH_REASONING_EFFORT")))
    (unwind-protect
         (progn
           (preferences--write
            configuration
            (make-instance 'preference-state
                           :model model
                           :reasoning-effort "minimal"))
           (register-openai-compatible-provider
            :name "bootstrap-openai"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models
            '((:name "bootstrap/chat-model"
               :reasoning-efforts ("minimal")))
            :source ':runtime)
           (let ((selected (preferences-apply-model-selection configuration)))
             (test-assert
              (and (string= (configuration-model selected) model)
                   (string= (configuration-reasoning-effort selected) "minimal"))
              "registered model metadata completes deferred preference selection"))
           (let ((conversation
                   (conversation-create
                    (configuration-with-model configuration model)
                    :identifier "bootstrap-unserved-model")))
             (unregister-provider "bootstrap-openai" :source ':runtime)
             (let ((selected (preferences-apply-model-selection configuration)))
               (test-assert
                (and (not (string= (configuration-model selected) model))
                     (string= (configuration-model selected)
                              (configuration-model configuration))
                     (string= (configuration-reasoning-effort selected)
                              (configuration-reasoning-effort configuration)))
                "a saved model no registered provider serves is dropped"))
             (let ((restored
                     (application--configuration-for-conversation
                      configuration
                      conversation)))
               (test-assert
                (and (string= (configuration-model restored)
                              (configuration-model configuration))
                     (member (configuration-reasoning-effort restored)
                             (configuration--reasoning-efforts-for
                              (configuration-model configuration))
                             :test #'string=))
                "an unserved conversation model falls back to the active model"))
             (setf (conversation-model conversation) nil
                   (conversation-reasoning-effort conversation) nil)
             (let ((restored
                     (application--configuration-for-conversation
                      configuration
                      conversation)))
               (test-assert
                (and (string= (configuration-model restored)
                              (configuration-model configuration))
                     (string= (configuration-reasoning-effort restored)
                              (configuration-reasoning-effort configuration)))
                "a conversation without a recorded selection keeps the active choices")))
           (sb-posix:setenv "AUTOLITH_MODEL" model 1)
           (sb-posix:setenv "AUTOLITH_REASONING_EFFORT" "minimal" 1)
           (let ((selected (configuration-create
                            :defer-provider-validation-p t)))
             (test-assert
              (and (string= (configuration-model selected) model)
                   (string= (configuration-reasoning-effort selected) "minimal"))
              "startup defers environment effort validation for init-defined models")))
      (if old-environment-model
          (sb-posix:setenv "AUTOLITH_MODEL" old-environment-model 1)
          (sb-posix:unsetenv "AUTOLITH_MODEL"))
      (if old-environment-effort
          (sb-posix:setenv "AUTOLITH_REASONING_EFFORT" old-environment-effort 1)
          (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT"))
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-openai-compatible-provider-deferred-main-validation () null)
(defun test-openai-compatible-provider-deferred-main-validation ()
  "Test main startup defers validation until registered providers are available."
  (let ((model "main-bootstrap/model")
        (old-environment-model (uiop:getenv "AUTOLITH_MODEL"))
        (old-environment-effort (uiop:getenv "AUTOLITH_REASONING_EFFORT")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_MODEL" model 1)
           (sb-posix:setenv "AUTOLITH_REASONING_EFFORT" "minimal" 1)
           (let ((observed-configuration nil)
                 (*active-application* nil))
             (test-call-with-function-replacements
              (list
               (list 'localgroup-handoff-selection
                     (lambda (configuration arguments)
                       (declare (ignore configuration arguments))
                       nil))
               (list 'application-recovery-state
                     (lambda (configuration)
                       (declare (ignore configuration))
                       (values nil nil nil)))
               (list 'application-recovery-diagnosis-prompt
                     (lambda (configuration)
                       (declare (ignore configuration))
                       nil))
               (list 'application-create
                     (lambda (configuration &key conversation-id permission-mode)
                       (declare (ignore conversation-id permission-mode))
                       (setf observed-configuration configuration)
                       (make-instance 'application)))
               (list 'application-run
                     (lambda (application &rest arguments)
                       (declare (ignore application arguments))
                       nil)))
              (lambda ()
                (main-dispatch nil)))
             (test-assert
              (and observed-configuration
                   (string= (configuration-model observed-configuration) model)
                   (string= (configuration-reasoning-effort
                             observed-configuration)
                            "minimal"))
              "main startup defers custom provider validation until user init"))
           (let ((observed-configuration nil))
             (test-call-with-function-replacements
              (list
               (list 'localgroup-statuses
                     (lambda (configuration)
                       (setf observed-configuration configuration)
                       nil))
               (list 'localgroup-print-statuses
                     (lambda (statuses)
                       (declare (ignore statuses))
                       nil)))
              (lambda ()
                (main-dispatch '("localgroup" "status"))))
             (test-assert
              (and observed-configuration
                   (string= (configuration-model observed-configuration) model)
                   (string= (configuration-reasoning-effort
                             observed-configuration)
                            "minimal"))
              "localgroup commands do not validate custom providers before init")))
      (if old-environment-model
          (sb-posix:setenv "AUTOLITH_MODEL" old-environment-model 1)
          (sb-posix:unsetenv "AUTOLITH_MODEL"))
      (if old-environment-effort
          (sb-posix:setenv "AUTOLITH_REASONING_EFFORT"
                          old-environment-effort 1)
          (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT"))))
  nil)

(-> test-openai-compatible-provider-bare-auth-selection () null)
(defun test-openai-compatible-provider-bare-auth-selection ()
  "Test bare authentication selects the persisted registered provider."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (model "bare-auth/model")
         (old-environment-model (uiop:getenv "AUTOLITH_MODEL"))
         (old-environment-effort (uiop:getenv "AUTOLITH_REASONING_EFFORT")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_MODEL")
           (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT")
           (preferences--write
            configuration
            (make-instance 'preference-state
                           :model model
                           :reasoning-effort "minimal"))
           (let ((authentication-calls nil)
                 (live-application (make-instance 'application))
                 (reconnect-called-p nil))
             (test-call-with-function-replacements
              (list
               (list 'configuration-create
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       configuration))
               (list 'localgroup-handoff-selection
                     (lambda (configuration arguments)
                       (declare (ignore configuration arguments))
                       nil))
               (list 'application-recovery-state
                     (lambda (configuration)
                       (declare (ignore configuration))
                       (values nil nil nil)))
               (list 'application-recovery-diagnosis-prompt
                     (lambda (configuration)
                       (declare (ignore configuration))
                       nil))
               (list 'user-init-load
                     (lambda (configuration)
                       (declare (ignore configuration))
                       (register-openai-compatible-provider
                        :name "bare-auth"
                        :endpoint "https://provider.invalid/v1/chat/completions"
                        :models
                        '((:name "bare-auth/model"
                           :reasoning-efforts ("minimal"))))
                       nil))
               (list 'main-authenticate
                     (lambda (configuration selection &optional method)
                       (push (list configuration selection method)
                             authentication-calls)
                       nil))
               (list 'application-reconnect
                     (lambda (application &rest arguments)
                       (declare (ignore arguments))
                       (setf reconnect-called-p t)
                       application))
               ;; Authentication continues into a session; a real interactive
               ;; loop against the test's non-terminal stdin can wait forever.
               (list 'application-run
                     (lambda (application &rest arguments)
                       (declare (ignore application arguments))
                       nil)))
              (lambda ()
                (let ((*active-application* live-application))
                  (let ((*active-application* nil))
                    (main-dispatch '("auth")))
                  (let ((*active-application* nil))
                    (main-dispatch '("auth" "chatgpt" "device"))))))
             (test-assert
              (not reconnect-called-p)
              "auth commands do not reconnect the active application")
             (let ((calls (nreverse authentication-calls)))
               (test-assert (= (length calls) 2)
                            "auth commands authenticate exactly once each")
               (destructuring-bind (bare explicit) calls
                 (test-assert
                  (and (typep (first bare) 'configuration)
                       (null (second bare))
                       (null (third bare))
                       (string= (configuration-model (first bare)) model)
                       (string= (configuration-reasoning-effort (first bare))
                                "minimal"))
                  "bare auth selects the persisted registered provider")
                 (test-assert
                  (and (typep (first explicit) 'configuration)
                       (string= (second explicit) "chatgpt")
                       (string= (third explicit) "device"))
                  "command-line auth passes its explicit provider and method")))))
      (if old-environment-model
          (sb-posix:setenv "AUTOLITH_MODEL" old-environment-model 1)
          (sb-posix:unsetenv "AUTOLITH_MODEL"))
      (if old-environment-effort
          (sb-posix:setenv "AUTOLITH_REASONING_EFFORT"
                          old-environment-effort 1)
          (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT"))
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-openai-compatible-provider-discovery-is-on-demand () null)
(defun test-openai-compatible-provider-discovery-is-on-demand ()
  "Test startup uses cached metadata and leaves remote discovery to /models."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (original-get (symbol-function 'dexador:get))
         (request-count 0)
         (response-body "{\"data\":[{\"id\":\"on-demand/model\"}]}"))
    (unwind-protect
         (progn
           (register-openai-compatible-provider
            :name "on-demand-test"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models-endpoint "https://provider.invalid/v1/models")
           (openai-compatible-provider-tests--save-key
            configuration "on-demand-test" "synthetic-discovery-key")
           (provider-bootstrap-configuration configuration)
           (test-assert
            (zerop request-count)
            "provider bootstrap does not query remote model APIs")
           (setf (symbol-function 'dexador:get)
                 (lambda (url &rest arguments)
                   (declare (ignore url arguments))
                   (incf request-count)
                   (values response-body 200 nil nil)))
           (test-assert
            (null (provider-refresh-models
                   configuration :provider-name "on-demand-test"))
            "/models refreshes dynamic metadata explicitly")
           (test-assert
            (= request-count 1)
            "explicit model refresh makes the remote request")
           (test-assert
            (probe-file (configuration-provider-model-cache-path configuration))
            "successful discovery writes the private model cache")
           (unregister-provider "on-demand-test" :source ':runtime)
           (register-openai-compatible-provider
            :name "on-demand-test"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models-endpoint "https://provider.invalid/v1/models")
           (provider-bootstrap-configuration configuration)
           (test-assert
            (member "on-demand/model"
                    (mapcar #'provider-model-name
                            (provider-registration-models
                             (provider-registration-find "on-demand-test")))
                    :test #'string=)
            "startup loads the last successful model list from cache"))
      (setf (symbol-function 'dexador:get) original-get)
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-openai-compatible-provider-model-cache-boundary () null)
(defun test-openai-compatible-provider-model-cache-boundary ()
  "Test the model cache persists dynamic metadata without retired static entries."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (original-get (symbol-function 'dexador:get))
         (provider-name "cache-boundary-test")
         (endpoint "https://provider.invalid/v1/chat/completions")
         (models-endpoint "https://provider.invalid/v1/models"))
    (labels ((registration-models ()
               "Return the effective model identifiers for the test provider."
               (mapcar #'provider-model-name
                       (provider-registration-models
                        (provider-registration-find provider-name))))

             (register (&optional models)
               "Register the test provider with optional static MODELS."
               (register-openai-compatible-provider
                :name provider-name
                :endpoint endpoint
                :models-endpoint models-endpoint
                :models models)))
      (unwind-protect
           (progn
             (register '("cache/static"))
             (openai-compatible-provider-tests--save-key
              configuration provider-name "synthetic-cache-key")
             (setf (symbol-function 'dexador:get)
                   (lambda (url &rest arguments)
                     (declare (ignore url arguments))
                     (values "{\"data\":[{\"id\":\"cache/live\"}]}"
                             200 nil nil)))
             (test-assert
              (null (provider-refresh-models
                     configuration :provider-name provider-name))
              "the cache-boundary provider refresh succeeds")
             (test-assert
              (equal (registration-models)
                     '("cache/static" "cache/live"))
              "static metadata precedes the discovered model")
             (register)
             (test-assert
              (equal (registration-models) '("cache/live"))
              "runtime re-registration does not retain removed static models")
             (unregister-provider provider-name :source ':runtime)
             (register)
             (provider-bootstrap-configuration configuration)
             (test-assert
              (equal (registration-models) '("cache/live"))
              "cache reload does not resurrect removed static models"))
        (setf (symbol-function 'dexador:get) original-get)
        (provider--registry-restore registry-snapshot)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> test-openai-compatible-provider-discovery () null)
(defun test-openai-compatible-provider-discovery ()
  "Test OpenAI-compatible model discovery, metadata overrides, and failures."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (original-get (symbol-function 'dexador:get))
         (response-body
           "{\"data\":[{\"id\":\"dynamic/model-a\"},{\"id\":\"dynamic/model-b\"}]}")
         (response-status 200)
         (response-error-p nil)
         (observed-url nil)
         (observed-headers nil))
    (unwind-protect
         (progn
           (register-openai-compatible-provider
            :name "dynamic-test"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models-endpoint "https://provider.invalid/v1/models"
            :models
            '((:name "dynamic/model-a"
               :description "Preferred dynamic model"
               :context-window 456789
               :reasoning-efforts ("low"))))
           (register-openai-compatible-provider
            :name "dynamic-only"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models-endpoint "https://provider.invalid/v1/models")
           (openai-compatible-provider-tests--save-key
            configuration "dynamic-test" "synthetic-discovery-key")
           (openai-compatible-provider-tests--save-key
            configuration "dynamic-only" "synthetic-discovery-key")
            (setf (symbol-function 'dexador:get)
                  (lambda (url &rest arguments)
                    (setf observed-url url
                          observed-headers (getf arguments :headers))
                    (when response-error-p
                      (error "Synthetic model discovery transport failure."))
                    (values response-body response-status nil nil)))
           (let ((failures
                   (provider-refresh-models
                    configuration
                    :provider-name "dynamic-test")))
             (test-assert
              (null failures)
              "dynamic provider model discovery succeeds")
             (test-assert
              (and (string= observed-url
                            "https://provider.invalid/v1/models")
                   (search "synthetic-discovery-key"
                           (rest (assoc "Authorization"
                                        observed-headers
                                        :test #'string=))))
              "model discovery uses the request-time bearer key")
             (test-assert
              (and (member "dynamic/model-a"
                           (provider-model-identifiers)
                           :test #'string=)
                   (member "dynamic/model-b"
                           (provider-model-identifiers)
                           :test #'string=))
              "discovered model identifiers enter the registry")
             (let ((metadata (provider-model-for "dynamic/model-a")))
               (test-assert
                (and (= (provider-model-context-window metadata) 456789)
                     (string= (provider-model-description metadata)
                              "Preferred dynamic model")
                     (equal (provider-model-reasoning-efforts metadata)
                            '("low")))
                "static model metadata overrides discovered defaults")))
           (let ((failures
                   (provider-refresh-models
                    configuration
                    :provider-name "dynamic-only")))
             (test-assert
              (null failures)
              "dynamic providers may omit static models")
             (test-assert
              (member "dynamic/model-b"
                      (provider-model-identifiers)
                      :test #'string=)
              "model discovery supplies models for an empty static declaration"))
           (setf response-body
                 "{\"data\":[{\"id\":\"dynamic/model-a\"},{\"id\":17}]}"
                 response-status 200)
           (let ((failures
                   (provider-refresh-models
                    configuration
                    :provider-name "dynamic-test")))
             (test-assert
              (and (= (length failures) 1)
                   (typep (first failures)
                          'provider-model-discovery-error))
              "invalid model-list entries fail discovery")
             (test-assert
              (and (member "dynamic/model-a"
                           (provider-model-identifiers)
                           :test #'string=)
                   (member "dynamic/model-b"
                           (provider-model-identifiers)
                           :test #'string=))
              "invalid model-list entries retain the last successful list"))
           (let ((snapshot (application--extension-registry-snapshot)))
             (setf response-body "{\"data\":[{\"id\":\"dynamic/model-c\"}]}"
                   response-status 200)
             (provider-refresh-models configuration :provider-name "dynamic-test")
             (let ((models
                     (provider-registration-models
                      (provider-registration-find "dynamic-test"))))
               (test-assert
                (and (member "dynamic/model-c"
                             (mapcar #'provider-model-name models)
                             :test #'string=)
                     (not (member "dynamic/model-b"
                                  (mapcar #'provider-model-name models)
                                  :test #'string=)))
                "successful model refresh replaces stale discovered identifiers"))
             (application--extension-registry-restore snapshot)
             (let ((models
                     (provider-registration-models
                      (provider-registration-find "dynamic-test"))))
               (test-assert
                (and (member "dynamic/model-b"
                             (mapcar #'provider-model-name models)
                             :test #'string=)
                     (not (member "dynamic/model-c"
                                  (mapcar #'provider-model-name models)
                                  :test #'string=)))
                "extension registry snapshots restore effective model lists")))
           (setf response-status 503)
           (let ((failures
                   (provider-refresh-models
                    configuration
                    :provider-name "dynamic-test")))
             (test-assert
              (and (= (length failures) 1)
                   (typep (first failures)
                          'provider-model-discovery-error))
              "model discovery reports endpoint failures without signaling")
             (test-assert
              (and (member "dynamic/model-a"
                           (provider-model-identifiers)
                           :test #'string=)
                   (member "dynamic/model-b"
                           (provider-model-identifiers)
                           :test #'string=))
              "model discovery failures retain the last successful model list")))
            (setf response-error-p t)
            (let ((failures
                    (provider-refresh-models
                     configuration
                     :provider-name "dynamic-test")))
              (test-assert
               (and (= (length failures) 1)
                    (typep (first failures)
                           'provider-model-discovery-error))
               "model discovery normalizes dependency transport failures"))
      (setf (symbol-function 'dexador:get) original-get)
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-openai-compatible-provider-registration-identity () null)
(defun test-openai-compatible-provider-registration-identity ()
  "Test reload validation distinguishes user providers from built-ins by source."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((provider (provider-create configuration)))
           (flet ((register-layer (source model)
                    "Register one provider layer for source-precedence testing."
                    (let ((*extension-registration-source* source))
                      (register-openai-compatible-provider
                       :name "layered-openai"
                       :endpoint "https://provider.invalid/v1/chat/completions"
                       :models (list model)))))
             (register-layer ':builtin "builtin/model")
             (register-layer ':site "site/model")
             (register-layer ':user "user/model")
             (register-layer ':runtime "runtime/model")
             (dolist (case '((:runtime . "runtime/model")
                             (:user . "user/model")
                             (:site . "site/model")
                             (:builtin . "builtin/model")))
               (let ((registration
                       (provider-registration-find "layered-openai")))
                 (test-assert
                  (and
                   (eq (provider-registration-source registration)
                       (first case))
                   (member (rest case)
                           (mapcar #'provider-model-name
                                   (provider-registration-models registration))
                           :test #'string=))
                  (format nil
                          "~A provider registration is effective at its precedence layer"
                          (first case))))
               (unless (eq (first case) ':builtin)
                 (unregister-provider "layered-openai"
                                      :source (first case)))))
           (register-openai-compatible-provider
            :name "chatgpt"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models '((:name "gpt-5.6-sol"))
            :source ':user)
           (test-assert
            (handler-case
                (progn
                  (application--validate-provider-registration
                   provider configuration)
                  nil)
              (configuration-error () t))
            "reload rejects a same-name user provider replaced by a built-in"))
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-openai-compatible-provider-authentication-bootstrap () null)
(defun test-openai-compatible-provider-authentication-bootstrap ()
  "Test named authentication refreshes dynamic models after saving an API key."
  (let* ((registry-snapshot (provider--registry-snapshot))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (register-openai-compatible-provider
            :name "bootstrap-auth"
            :endpoint "https://provider.invalid/v1/chat/completions"
            :models-endpoint "https://provider.invalid/v1/models")
           (let* ((provider
                    (provider-authentication-provider
                     configuration "bootstrap-auth"))
                  (output (make-string-output-stream))
                  (input
                    (make-string-input-stream
                     (format nil
                             "~C[200~~bootstrap-key~C[201~~~%"
                             *terminal-escape-character*
                             *terminal-escape-character*)))
                  (message
                    (test-call-with-function-replacements
                     (list
                      (list 'dexador:get
                            (lambda (url &rest arguments)
                              (declare (ignore url arguments))
                              (values
                               (json-encode
                                (json-object
                                 "data"
                                 (json-array
                                  (json-object "id" "bootstrap-auth/live-model"))))
                               200 nil))))
                     (lambda ()
                       (let ((*standard-input* input)
                             (*standard-output* output)
                             (*api-key-input-file-descriptor* -1))
                         (provider-authenticate provider
                                                :stream output
                                                :open-browser-p nil))))))
             (test-assert
              (typep provider 'openai-compatible-provider)
              "named authentication creates a discovery-only provider before a key exists")
             (test-assert
              (search "API key was saved" message)
              "discovery-only provider authentication saves the first key")
             (test-assert
              (api-key-credential-available-p
               (provider-credential-manager provider))
              "the bootstrapped provider reports its stored key")
             (test-assert
              (member "bootstrap-auth/live-model"
                      (provider-model-identifiers)
                      :test #'string=)
              "authentication immediately refreshes dynamic provider models")
             (let ((credentials
                     (credential-source-load
                      (credential-manager-primary-source
                       (provider-credential-manager provider)))))
               (test-assert
                (and credentials
                     (string= (oauth-credentials-access-token credentials)
                              "bootstrap-key"))
                "bracketed paste saves the exact API key without terminal markers"))
             (let ((failure-message
                     (test-call-with-function-replacements
                      (list
                       (list 'dexador:get
                             (lambda (url &rest arguments)
                               (declare (ignore url arguments))
                               (error "synthetic discovery failure"))))
                      (lambda ()
                        (let ((*standard-input*
                                (make-string-input-stream
                                 (format nil "replacement-key~%")))
                              (*standard-output* output)
                              (*api-key-input-file-descriptor* -1))
                          (provider-authenticate provider
                                                 :stream output
                                                 :open-browser-p nil))))))
               (test-assert
                (and (search "API key was saved" failure-message)
                     (search "Model discovery warnings:" failure-message)
                     (search "Could not discover models for provider bootstrap-auth."
                             failure-message))
                "authentication reports model discovery failure without failing login")
               (test-assert
                (member "bootstrap-auth/live-model"
                        (provider-model-identifiers)
                        :test #'string=)
                "failed authentication refresh retains the last dynamic model list"))))
      (provider--registry-restore registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-sse-bounds () null)
(defun test-provider-sse-bounds ()
  "Test SSE line and joined-event bounds at and above their limits."
  (let ((*sse-maximum-line-characters* 7)
        (*sse-maximum-event-characters* 16))
    (test-assert
     (string= (read-sse-data
               (make-string-input-stream (format nil "data: x~%~%")))
              "x")
     "an SSE line at the configured limit is accepted")
    (test-assert
     (handler-case
         (progn
           (read-sse-data
            (make-string-input-stream (format nil "data: xx~%~%")))
           nil)
       (response-stream-error () t))
     "an SSE line above the configured limit is rejected"))
  (let ((*sse-maximum-line-characters* 32)
        (*sse-maximum-event-characters* 3))
    (test-assert
     (string= (read-sse-data
               (make-string-input-stream
                (format nil "data: a~%data: b~%~%")))
              (format nil "a~%b"))
     "an SSE event at the configured joined-data limit is accepted")
    (let ((*sse-maximum-event-characters* 2))
      (test-assert
       (handler-case
           (progn
             (read-sse-data
              (make-string-input-stream
               (format nil "data: a~%data: b~%~%")))
             nil)
         (response-stream-error () t))
       "an SSE event above the configured joined-data limit is rejected")))
  nil)

(-> test-openai-compatible-provider () null)
(defun test-openai-compatible-provider ()
  "Test registration, request projection, authentication, and stream decoding."
  (let ((registry-snapshot (provider--registry-snapshot))
        (configuration nil)
        (root nil)
        (key (make-string 12 :initial-element #\k)))
    (unwind-protect
         (progn
           (register-openai-compatible-provider
            :name "test-openai"
            :description "Synthetic OpenAI-compatible endpoint"
            :family ':test-openai
            :models
            '((:name "test/chat-model"
               :description "Synthetic chat model"
               :context-window 123456
               :reasoning-efforts ("none" "high")))
            :endpoint "https://provider.invalid/v1/chat/completions"
            :headers '(("X-Test-Provider" . "synthetic"))
            :reasoning-parameter "reasoning_effort")
           (setf configuration
                 (test-configuration)
                 root
                 (test-configuration-root configuration))
           (openai-compatible-provider-tests--save-key
            configuration "test-openai" key)
           (let ((pathname (configuration-api-keys-path configuration)))
             (test-assert
              (probe-file pathname)
              "API keys are persisted in the private provider-key store")
             (let ((mode (sb-posix:stat-mode (sb-posix:stat (namestring pathname)))))
               (test-assert
                (= (logand mode #o777) #o600)
                "the provider-key store has mode 0600")))
           (let* ((model "test/chat-model")
                  (provider-configuration
                    (configuration-with-model configuration model))
                  (provider (provider-create provider-configuration))
                  (registration (provider-registration-find "test-openai"))
                  (metadata (provider-model-for model)))
             (test-assert
               (equal (provider-model-identifiers)
                      (append (list "gpt-5.6-sol"
                                    "gpt-5.6-luna"
                                    "gpt-5.6-terra")
                              (mapcar (lambda (entry) (getf entry ':name))
                                      *gemini-code-assist-models*)
                              (list "grok-4.6"
                                    "grok-4.5"
                                    "accounts/fireworks/models/kimi-k3"
                                    "accounts/fireworks/models/qwen3p7-plus"
                                    "claude-opus-5"
                                    "claude-sonnet-5"
                                    "claude-haiku-4-5-20251001"
                                    model)))
              "registered models appear after the built-in provider models")
             (test-assert
              (and registration
                   (string= (provider-registration-description registration)
                            "Synthetic OpenAI-compatible endpoint"))
              "provider registrations retain their display metadata")
             (test-assert
              (and metadata
                   (= (provider-model-context-window metadata) 123456)
                   (equal (provider-model-reasoning-efforts metadata)
                          '("none" "high")))
              "registered models retain context and reasoning metadata")
             (test-assert
              (and (equal (configuration--reasoning-efforts-for model)
                          '("none" "high"))
                   (string= (configuration-reasoning-effort
                             provider-configuration)
                            "none"))
              "model selection uses the registered reasoning efforts")
             (test-assert
               (and (typep provider 'openai-compatible-provider)
                    (typep provider 'chat-completions-provider)
                    (eq (provider-wire-protocol provider) ':chat-completions)
                    (eq (provider-family provider) ':test-openai)
                    (string= (configuration-provider-endpoint provider-configuration)
                             "https://provider.invalid/v1/chat/completions"))
              "registered models create the configured OpenAI-compatible provider")
              (let* ((reconfiguration
                       (configuration-with-reasoning-effort
                        provider-configuration "high"))
                     (reconfigured
                       (provider-with-configuration provider reconfiguration)))
                (test-assert
                 (and (eq (class-of reconfigured) (class-of provider))
                      (eq (provider-configuration reconfigured) reconfiguration)
                      (eq (model-provider-registration reconfigured)
                          (model-provider-registration provider))
                      (eq (provider-credential-manager reconfigured)
                          (provider-credential-manager provider))
                      (string= (provider-session-id reconfigured)
                               (provider-session-id provider))
                      (string= (openai-compatible-provider-display-name
                                reconfigured)
                               "test-openai")
                      (eq (openai-compatible-provider-family reconfigured)
                          ':test-openai)
                      (equal (openai-compatible-provider-headers reconfigured)
                             '(("X-Test-Provider" . "synthetic")))
                      (not (eq (openai-compatible-provider-headers reconfigured)
                               (openai-compatible-provider-headers provider)))
                      (string= (openai-compatible-provider-reasoning-parameter
                                reconfigured)
                               "reasoning_effort"))
                 "OpenAI-compatible reconfiguration composes session and wire initargs"))
             (let ((application
                     (make-instance 'application
                                    :configuration provider-configuration)))
               (test-assert
                (search "test-openai / test/chat-model"
                        (application--models-description application))
                "/models describes registered provider models")
               (let ((named-provider
                       (application--authentication-provider
                        application "test-openai")))
                 (test-assert
                  (and (typep named-provider 'openai-compatible-provider)
                       (string= (openai-compatible-provider-display-name
                                 named-provider)
                                "test-openai"))
                  "/auth PROVIDER selects the named registration")))
             (unregister-provider "test-openai" :source ':runtime)
             (test-assert
              (handler-case
                  (progn
                    (application--validate-provider-registration
                     provider provider-configuration)
                    nil)
                (configuration-error () t))
              "reload rejects a lost active custom provider instead of falling back")
              (let* ((output (make-string-output-stream))
                     (message
                       (openai-compatible-provider-tests--call-with-input
                        (format nil "~A~%" key)
                        (lambda (input)
                          (let ((*standard-input* input)
                                (*standard-output* output))
                            (provider-authenticate provider
                                                   :stream output
                                                   :open-browser-p nil))))))
               (test-assert
                (and (search "API key was saved" message)
                     (not (search key message))
                     (not (search key (get-output-stream-string output)))
                     (not (secret-use-active-p)))
                "API-key authentication replaces keys without exposing them"))
             (let ((conversation
                     (conversation-create provider-configuration
                                          :identifier "openai-compatible-request"))
                   (tools
                     (json-array
                      (json-object
                       "type" "namespace"
                       "name" "fs"
                       "tools"
                       (json-array
                        (json-object
                         "type" "function"
                         "name" "read"
                         "description" "Read one file."
                         "strict" false
                         "parameters"
                         (json-object "type" "object")))))))
               (conversation-append-user-message conversation "Read the file.")
               (multiple-value-bind (request delivery)
                   (provider-request-object
                    provider conversation tools
                    :goal-context "keep the active goal"
                    :compaction-p nil)
                 (let* ((messages (json-get request "messages"))
                        (wire-tools (json-get request "tools"))
                        (wire-tool (aref wire-tools 0))
                        (wire-function (json-get wire-tool "function")))
                   (test-assert
                    (and (vectorp messages)
                         (search "Read the file" (json-encode messages)))
                    "Chat Completions requests carry projected conversation messages")
                    (test-assert
                     (let ((system-messages
                             (remove-if-not
                              (lambda (message)
                                (string= (json-get message "role") "system"))
                              (coerce messages 'list))))
                       (and delivery
                            (= (length system-messages) 1)
                            (eq (aref messages 0) (first system-messages))
                            (search "keep the active goal"
                                    (json-get (first system-messages) "content"))
                            (search "Temporary context"
                                    (json-get (first system-messages) "content"))))
                     "goal and mutable context share one leading system message")
                   (test-assert
                    (null (json-get request "prompt_cache_key"))
                    "generic OpenAI-compatible requests omit unsupported cache fields")
                   (test-assert
                    (and (vectorp wire-tools)
                         (string= (json-get wire-tool "type") "function")
                         (json-object-p wire-function)
                         (not (find #\. (json-get wire-function "name")))
                         (json-object-p (json-get wire-function "parameters")))
                    "namespaced tools use a grammar-safe Chat Completions function wrapper"))))
              (let ((conversation
                      (conversation-create provider-configuration
                                           :identifier "openai-compatible-compaction")))
                (conversation-append-user-message conversation "Compact this history.")
                (multiple-value-bind (request delivery)
                    (provider-request-object provider conversation #()
                                             :compaction-p t)
                  (let* ((messages (json-get request "messages"))
                         (system-message (aref messages 0)))
                    (test-assert
                     (and (null delivery)
                          (string= (json-get system-message "role") "system")
                          (search *compaction-instructions*
                                  (json-get system-message "content"))
                          (= 1
                             (count "system" messages
                                    :key (lambda (message)
                                           (json-get message "role"))
                                    :test #'string=)))
                     "compaction also uses one leading system message"))))
             (let* ((call-a
                      (json-object
                       "type" "function_call"
                       "call_id" "call-a"
                       "namespace" "fs"
                       "name" "read"
                       "arguments" "{}"))
                    (call-b
                      (json-object
                       "type" "function_call"
                       "call_id" "call-b"
                       "namespace" "fs"
                       "name" "write"
                       "arguments" "{}"))
                    (tool-output
                      (json-object
                       "type" "function_call_output"
                       "call_id" "call-a"
                       "output" "done"))
                    (reasoning-item
                      (json-object
                       "type" "reasoning_content"
                       "content" "let me think"))
                    (messages
                      (openai-compatible--chat-input-messages
                       (list reasoning-item call-a call-b tool-output))))
               (test-assert
                (and (= (length messages) 2)
                     (string= (json-get (first messages) "role") "assistant")
                     (= (length (json-get (first messages) "tool_calls")) 2))
                "multiple function calls share one Chat Completions assistant message")
               (test-assert
                (string= (json-get (first messages) "reasoning_content")
                         "let me think")
                "captured thinking rides on the same round's tool-call message")
               (test-assert
                (family-private-item-p reasoning-item)
                "thinking items stay private to their producing family")
               (test-assert
                (string= (json-get (second messages) "role") "tool")
                "Chat Completions tool results follow the grouped assistant message"))
             (let* ((text-event
                      (openai-compatible-provider-tests--stream-event
                       (json-object "content" "hello")
                       "chat-test"))
                    (reasoning-event
                      (openai-compatible-provider-tests--stream-event
                       (json-object "reasoning_content" "pondering")
                       "chat-test"))
                    (first-tool
                      (json-object
                       "index" 0
                       "id" "call-test"
                       "function"
                       (json-object
                        "name" (openai-compatible--wire-tool-name "fs" "read")
                        "arguments" "{\"path\":")))
                    (second-tool
                      (json-object
                       "index" 0
                       "function"
                       (json-object "arguments" "\"x\"}")))
                    (first-tool-event
                      (openai-compatible-provider-tests--stream-event
                       (json-object
                        "tool_calls" (json-array first-tool))
                       "chat-test"))
                    (second-tool-event
                      (openai-compatible-provider-tests--stream-event
                       (json-object
                        "tool_calls" (json-array second-tool))
                       "chat-test"))
                    (finish-event
                      (openai-compatible-provider-tests--stream-event
                       (json-object)
                       "chat-test"
                       "tool_calls"))
                    (source
                      (concatenate
                       'string
                       (test-sse-event-string reasoning-event)
                       (test-sse-event-string text-event)
                       (test-sse-event-string first-tool-event)
                       (test-sse-event-string second-tool-event)
                       (test-sse-event-string finish-event)
                       (format nil "data: [DONE]~%~%")))
                    (events nil)
                    (result
                      (provider-consume-stream
                       provider
                       (make-string-input-stream source)
                       nil
                       (lambda (event) (push event events))))
                    (call (first (provider-result-tool-calls result))))
               (test-assert
                (and (string= (provider-result-response-id result) "chat-test")
                     (eq (provider-result-turn-completion result) ':continue)
                     (= (length (provider-result-output-items result)) 3))
                "Chat Completions streams produce a normalized continuing result")
               (test-assert
                (let ((item (first (provider-result-output-items result))))
                  (and (chat-reasoning-item-p item)
                       (string= (json-get item "content") "pondering")))
                "streamed thinking persists as the leading reasoning item")
               (test-assert
                (and call
                     (string= (json-get call "namespace") "fs")
                     (string= (json-get call "name") "read")
                     (string= (json-get call "arguments") "{\"path\":\"x\"}"))
                "fragmented Chat Completions tool calls become namespaced calls")
               (test-assert
                (and (find-if (lambda (event)
                                (typep event 'assistant-delta-event))
                             events)
                     (find-if (lambda (event)
                                (typep event 'provider-completed-event))
                             events))
                "Chat Completions streams emit assistant and completion events")
               (test-assert
                (handler-case
                    (progn
                      (provider-consume-stream
                       provider
                       (make-string-input-stream
                        (test-sse-event-string text-event))
                       nil
                       #'identity)
                      nil)
                  (response-stream-error () t))
                "truncated Chat Completions streams remain retryable failures"))
              (let* ((tool-event
                       (openai-compatible-provider-tests--stream-event
                        (json-object
                         "tool_calls"
                         (json-array
                          (json-object
                           "index" 0
                           "id" "call-empty"
                           "function"
                           (json-object
                            "name"
                            (openai-compatible--wire-tool-name "fs" "read")))))
                        "chat-empty"))
                     (finish-event
                       (openai-compatible-provider-tests--stream-event
                        (json-object) "chat-empty" "tool_calls"))
                     (result
                       (provider-consume-stream
                        provider
                        (make-string-input-stream
                         (concatenate
                          'string
                          (test-sse-event-string tool-event)
                          (test-sse-event-string finish-event)
                          (format nil "data: [DONE]~%~%")))
                        nil
                        #'identity))
                     (call (first (provider-result-tool-calls result))))
                (test-assert
                 (and call (string= (json-get call "arguments") "{}"))
                 "empty Chat Completions tool arguments normalize to an object"))
              (dolist (case
                       (list
                        (list "missing call id"
                              (json-object
                               "index" 0
                               "function"
                               (json-object "name" "read" "arguments" "{}")))
                        (list "missing function name"
                              (json-object
                               "index" 0
                               "id" "call-missing-name"
                               "function"
                               (json-object "arguments" "{}")))))
                (let* ((tool-event
                         (openai-compatible-provider-tests--stream-event
                          (json-object "tool_calls" (json-array (second case)))
                          "chat-incomplete"))
                       (finish-event
                         (openai-compatible-provider-tests--stream-event
                          (json-object) "chat-incomplete" "tool_calls"))
                       (source
                         (concatenate
                          'string
                          (test-sse-event-string tool-event)
                          (test-sse-event-string finish-event)
                          (format nil "data: [DONE]~%~%"))))
                  (test-assert
                   (handler-case
                       (progn
                         (provider-consume-stream
                          provider
                          (make-string-input-stream source)
                          nil
                          #'identity)
                         nil)
                     (response-stream-error () t))
                   (format nil "Chat Completions rejects ~A" (first case)))))
              (dolist (item
                       (list
                        (json-object "type" 17 "role" "assistant")
                        (json-object "type" "message" "role" 17)
                        (json-object "type" 17 "summary" (json-array))))
                (test-assert
                 (and (null (response-item-assistant-text item))
                      (null (response-item-reasoning-summary item))
                      (not (function-call-item-p item)))
                 "non-string response discriminators are ignored safely"))
              (let* ((malformed-event
                       (openai-compatible-provider-tests--stream-event
                        (json-object
                         "content" 17
                         "tool_calls"
                         (json-array
                          (json-object
                           "index" 0
                           "id" 17
                           "function"
                           (json-object "name" 17 "arguments" 17))))
                        "chat-malformed"))
                     (finish-event
                       (openai-compatible-provider-tests--stream-event
                        (json-object) "chat-malformed" "stop"))
                     (source
                       (concatenate
                        'string
                        (test-sse-event-string malformed-event)
                        (test-sse-event-string finish-event)
                        (format nil "data: [DONE]~%~%"))))
                (test-assert
                 (handler-case
                     (progn
                       (provider-consume-stream
                        provider
                        (make-string-input-stream source)
                        nil
                        #'identity)
                       nil)
                   (response-stream-error () t))
                 "malformed tool fields become typed stream failures"))
           (let* ((prompt-provider
                    (openai-compatible-provider-create
                     configuration
                     :name "prompt-openai"
                     :family ':prompt-openai
                     :headers nil
                     :reasoning-parameter nil))
                   (prompt-output (make-string-output-stream))
                   (message
                     (openai-compatible-provider-tests--call-with-input
                      "prompt-key\n"
                      (lambda (input)
                        (let ((*standard-input* input)
                              (*standard-output* prompt-output))
                          (provider-authenticate prompt-provider
                                                 :stream prompt-output
                                                 :open-browser-p nil))))))
             (test-assert
              (and (search "API key was saved" message)
                   (not (search "prompt-key" message))
                   (not (search "prompt-key"
                                (get-output-stream-string prompt-output))))
              "API-key authentication stores prompted keys without echoing them")
             (test-assert
              (api-key-credential-available-p
               (provider-credential-manager prompt-provider))
              "prompted API keys are available to later requests"))
           (let ((empty-manager
                   (api-key-credential-manager-create
                    :provider-name "empty-test"
                    :pathname (configuration-api-keys-path configuration))))
             (test-assert
              (typep (credential-manager-primary-source empty-manager)
                     'autolith-credential-source)
              "API-key sources use the private credential-source contract")
             (test-assert
              (not (api-key-credential-available-p empty-manager))
              "missing API keys are reported as unavailable")
             (test-assert
              (not (secret-use-active-p))
              "API-key availability checks release request-scope secret accounting"))
           (register-provider
            "chatgpt"
            :source ':user
            :family ':codex
            :models '("shadow/chatgpt")
            :factory
            (lambda (configuration &key reasoning-summaries-p)
              (declare (ignore configuration reasoning-summaries-p))
              (make-instance 'model-provider)))
           (test-assert
            (and (member "shadow/chatgpt" (provider-model-identifiers)
                         :test #'string=)
                 (not (member "gpt-5.6-sol" (provider-model-identifiers)
                              :test #'string=)))
            "a higher-precedence provider registration hides its shadowed models")
           (configuration-ensure-directories configuration)
           (let ((pathname (configuration-user-init-path configuration)))
             (with-open-file (stream pathname
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create
                                     :external-format ':utf-8)
               (write-string
                "(register-openai-compatible-provider
                    :name \"init-openai\"
                    :description \"Provider from init.lisp\"
                    :endpoint \"https://init.invalid/v1/chat/completions\"
                    :models '(\"init/chat-model\"))"
                stream))
             (test-assert
              (equal (user-init-load configuration) pathname)
              "init.lisp loads an OpenAI-compatible provider registration")
             (let ((registration (provider-registration-find "init-openai")))
               (test-assert
                (and registration
                     (eq (provider-registration-source registration) ':user)
                     (member "init/chat-model"
                             (provider-model-identifiers)
                             :test #'string=))
                "init.lisp provider registrations expose user models"))
             (delete-file pathname)
             (user-init-load configuration)
             (test-assert
              (not (member "init/chat-model"
                           (provider-model-identifiers)
                           :test #'string=))
              "removing init.lisp removes its provider registrations")))
      (provider--registry-restore registry-snapshot)
      (when root
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
  nil))

(-> test-openai-compatible-tool-name-recovery () null)
(defun test-openai-compatible-tool-name-recovery ()
  "Test readable, legacy, bounded, and fallback tool-name recovery."
  (let ((wire-name
          (openai-compatible--wire-tool-name "mcp__demo" "call_name")))
    (multiple-value-bind (namespace name)
        (openai-compatible--decode-wire-tool-name wire-name)
      (test-assert
       (and (equal namespace "mcp__demo") (equal name "call_name"))
       "readable OpenAI-compatible wire names round-trip")))
  (let ((wire-name
          (openai-compatible--wire-tool-name "mcp__server" "get__status")))
    (multiple-value-bind (namespace name)
        (openai-compatible--decode-wire-tool-name wire-name)
      (test-assert
       (and (equal namespace "mcp__server") (equal name "get__status"))
       "namespaced wire names with double underscores in both halves round-trip")))
  (multiple-value-bind (namespace name)
      (openai-compatible--decode-wire-tool-name "acmVzb3VyY2UAcmVhZA")
    (test-assert
     (and (equal namespace "resource") (equal name "read"))
     "legacy Base64 wire names remain decodable"))
  (let* ((component (make-string 20 :initial-element #\_))
         (wire-name (openai-compatible--wire-tool-name component component)))
    (test-assert
     (<= (length wire-name)
         *openai-compatible-wire-tool-name-maximum-length*)
     "OpenAI-compatible wire names respect the protocol limit"))
  (multiple-value-bind (namespace name)
      (openai-compatible--fallback-tool-name "fs.view-image")
    (test-assert
     (and (equal namespace "fs") (equal name "view-image"))
     "canonical dotted echoes recover their namespace"))
  (let* ((configuration (test-configuration))
         (registry (make-instance 'tool-registry))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation (conversation-create configuration)
                                 :registry registry))
         (parameters (tool-object-schema
                      (json-object "value" (tool-string-property "Value."))
                      '("value")))
         (call (json-object "type" "function_call"
                            "call_id" "bare-1"
                            "name" "echo"
                            "arguments" "{\"value\":\"hi\"}")))
    (tool-registry-register
     registry
     (make-instance 'agent-test-echo-tool
                    :namespace "first" :name "echo"
                    :description "Echo a value."
                    :parameters parameters))
    (test-assert
     (tool-result-success-p (tool-registry-execute-call registry call context))
     "a unique bare tool name dispatches to its only implementation")
    (tool-registry-register
     registry
     (make-instance 'agent-test-echo-tool
                    :namespace "second" :name "echo"
                    :description "Echo a value."
                    :parameters parameters))
    (let ((result (tool-registry-execute-call registry call context)))
      (test-assert
       (and (not (tool-result-success-p result))
            (search "Ambiguous tool name echo" (tool-result-content result))
            (search "first.echo" (tool-result-content result)))
       "an ambiguous bare tool name fails with its candidates"))
    (let ((result
            (tool-registry-execute-call
             registry
             (json-object "type" "function_call"
                          "call_id" "bare-2"
                          "name" "missing"
                          "arguments" "{}")
             context)))
      (test-assert
       (and (not (tool-result-success-p result))
            (search "Unknown tool missing." (tool-result-content result)))
       "an unknown bare tool name fails with its own name")))
  nil)
