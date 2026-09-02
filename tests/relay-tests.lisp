(in-package #:autolith)

;;;; -- Relay Tests --

(-> nemo-relay-test--temporary-root () pathname)
(defun nemo-relay-test--temporary-root ()
  "Create one isolated temporary directory for Relay output tests."
  (let ((root
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "autolith-relay-tests-~A/" (make-identifier))
            (uiop:temporary-directory)))))
    (uiop:ensure-all-directories-exist (list root))
    (uiop:ensure-directory-pathname (truename root))))

(-> nemo-relay-test--write-config (pathname pathname) null)
(defun nemo-relay-test--write-config (pathname output-directory)
  "Write a representative Relay TOML PluginConfig to PATHNAME."
  (uiop:ensure-all-directories-exist (list output-directory))
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (format stream
            "version = 1~%~%[[components]]~%kind = \"observability\"~%enabled = true~%~%[components.config]~%version = 4~%enable_full_payloads = false~%~%[components.config.atof]~%enabled = true~%~%[[components.config.atof.sinks]]~%type = \"file\"~%output_directory = ~S~%filename = \"events.jsonl\"~%mode = \"overwrite\"~%~%[components.config.atif]~%enabled = false~%~%[components.config.opentelemetry]~%enabled = false~%"
            (namestring output-directory)))
  nil)

(-> nemo-relay-test--write-json-config (pathname pathname) null)
(defun nemo-relay-test--write-json-config (pathname output-directory)
  "Write a minimal legacy JSON PluginConfig to PATHNAME."
  (uiop:ensure-all-directories-exist (list output-directory))
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string
     (json-encode
      (json-object
       "version" 1
       "components"
       (json-array
        (json-object
         "kind" "observability"
         "enabled" t
         "config"
         (json-object
          "atof"
          (json-object
           "enabled" t
           "sinks"
           (json-array
            (json-object
             "type" "file"
             "output_directory" (namestring output-directory)
             "filename" "events.jsonl"
             "mode" "overwrite"))))))))
     stream)
    (terpri stream))
  nil)

(-> test-nemo-relay-configuration () null)
(defun test-nemo-relay-configuration ()
  "Test Relay TOML PluginConfig settings and explicit precedence."
  (let* ((root (nemo-relay-test--temporary-root))
         (config-path (merge-pathnames "plugins.toml" root))
         (json-path (merge-pathnames "legacy.json" root))
         (saved-environment
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   '("AUTOLITH_RELAY"
                     "AUTOLITH_RELAY_CONFIG"
                     "AUTOLITH_RELAY_DYNAMIC_PLUGINS_CONFIG"
                     "AUTOLITH_RELAY_LIBRARY"
                     "XDG_CONFIG_HOME")))
         (saved-configuration *nemo-relay-configuration*))
    (unwind-protect
         (progn
           (dolist (name '("AUTOLITH_RELAY"
                           "AUTOLITH_RELAY_CONFIG"
                           "AUTOLITH_RELAY_DYNAMIC_PLUGINS_CONFIG"
                           "AUTOLITH_RELAY_LIBRARY"
                           "XDG_CONFIG_HOME"))
             (sb-posix:unsetenv name))
           (setf *nemo-relay-configuration* nil)
           (let ((settings (nemo-relay--runtime-configuration nil)))
             (test-assert
              (null (nemo-relay-configuration-config-path settings))
              "Relay does not invent an Autolith-local configuration pathname")
             (let ((failed-p nil))
               (handler-case
                   (nemo-relay--configuration-plugin-config-json settings)
                 (nemo-relay-error ()
                   (setf failed-p t)))
               (test-assert
                failed-p
                "Relay requires an explicit PluginConfig instead of native discovery")))
           (let ((config-home (merge-pathnames "config/" root)))
             (uiop:ensure-all-directories-exist (list config-home))
             (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config-home) 1)
             (test-assert
              (null (nemo-relay-configuration-config-path
                     (nemo-relay--runtime-configuration nil)))
              "Relay ignores the removed Autolith-local JSON default")
             (test-assert
              (handler-case
                  (progn
                    (nemo-relay--ensure-no-implicit-plugin-config)
                    t)
                (serious-condition ()
                  nil))
              "Relay starts with no implicit plugin configuration present")
             (let ((discovered-path
                     (merge-pathnames "nemo-relay/plugins.toml" config-home)))
               (uiop:ensure-all-directories-exist
                (list (uiop:pathname-directory-pathname discovered-path)))
               (with-open-file (stream discovered-path
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create
                                       :external-format ':utf-8)
                 (write-string "{}" stream))
               (let ((failed-p nil))
                 (handler-case
                     (nemo-relay--ensure-no-implicit-plugin-config)
                   (nemo-relay-error ()
                     (setf failed-p t)))
                 (test-assert
                  failed-p
                  "Relay rejects native implicit plugin configuration"))
                (test-assert
                 (handler-case
                     (progn
                       (nemo-relay--ensure-no-implicit-plugin-config
                        :explicit-p t)
                       t)
                   (serious-condition ()
                     nil))
                 "Relay allows explicit PluginConfig beside ambient discovery")
                (test-assert
                 (string= (nemo-relay--condition-summary
                           (make-condition 'nemo-relay-error
                                           :message "explicit Relay config failed"
                                           :operation "Relay test"))
                          "explicit Relay config failed")
                 "Relay diagnostics retain safe Autolith error messages")
               (delete-file discovered-path)))
           (let ((output-directory (merge-pathnames "traces/" root)))
             (nemo-relay-test--write-config config-path output-directory)
             (let ((settings
                     (nemo-relay-configuration-create
                      :enabled-p t
                      :config config-path
                      :library-path "configured-library")))
               (test-assert
                (nemo-relay-configuration-enabled-p settings)
                "explicit Relay enabled value enables instrumentation")
               (test-assert
                (equal (nemo-relay-configuration-config-path settings) config-path)
                "Relay selects the TOML PluginConfig pathname")
               (test-assert
                (string= (nemo-relay-configuration-library-path settings)
                         "configured-library")
                "Relay selects the configured library")
               (test-assert
                (null (nemo-relay-configuration-plugin-config-json settings))
                "Relay defers reading a configured PluginConfig file")
               (let ((yason:*parse-json-booleans-as-symbols* t)
                     (yason:true t))
                 (let* ((document
                          (json-decode
                           (nemo-relay--configuration-plugin-config-json settings)))
                        (components (json-get document "components"))
                        (component (and (vectorp components)
                                        (aref components 0)))
                        (component-config (and component
                                               (json-get component "config")))
                        (atof (and component-config
                                    (json-get component-config "atof")))
                        (atif (and component-config
                                    (json-get component-config "atif")))
                        (opentelemetry (and component-config
                                             (json-get component-config
                                                       "opentelemetry"))))
                   (test-assert (= (json-get document "version") 1)
                                "Relay converts the TOML document version")
                   (test-assert
                    (and (json-object-p component)
                         (string= (json-get component "kind") "observability"))
                    "Relay converts the TOML observability component")
                   (test-assert
                    (and (json-object-p atof)
                         (eq (json-get atof "enabled") t))
                    "Relay converts the TOML ATOF section")
                   (test-assert
                    (and (json-object-p atif)
                         (eq (json-get atif "enabled") false))
                    "Relay converts the TOML ATIF section")
                   (test-assert
                    (and (json-object-p opentelemetry)
                         (eq (json-get opentelemetry "enabled") false))
                    "Relay converts the TOML OpenTelemetry section")
                   (multiple-value-bind (plugins present-p)
                       (gethash "plugins" document)
                     (declare (ignore plugins))
                     (test-assert
                      (not present-p)
                      "Relay removes host-only dynamic declarations before FFI")))))
             (let* ((plugin-config
                      (json-object
                       "version" 1
                       "components" (json-array)))
                    (settings
                      (nemo-relay-configuration-create
                       :enabled-p t
                       :config config-path
                       :plugin-config plugin-config)))
               (test-assert
                (= (json-get
                    (json-decode
                     (nemo-relay-configuration-plugin-config-json settings))
                    "version")
                   1)
                "Relay accepts a PluginConfig JSON object directly"))
              (let ((failed-p nil))
                (handler-case
                    (nemo-relay-configuration-create
                     :plugin-config
                     (json-object
                      "version" 1
                      "components"
                      (json-array
                       (json-object "kind" "nemo_guardrails" "enabled" t))))
                  (nemo-relay-error ()
                    (setf failed-p t)))
                (test-assert
                 failed-p
                 "Relay rejects unrelated static component kinds"))
              (let ((settings
                      (nemo-relay-configuration-create
                       :plugin-config
                       (json-object
                        "version" 1
                        "components"
                        (json-array
                         (json-object
                          "kind" "example.custom_observer"
                          "enabled" t)))
                       :allowed-component-kinds
                       '("example.custom_observer"))))
                (test-assert
                 (string= (json-get
                           (aref
                            (json-get
                             (json-decode
                              (nemo-relay-configuration-plugin-config-json settings))
                             "components")
                            0)
                           "kind")
                          "example.custom_observer")
                 "Relay accepts an explicitly allowed custom observability component"))
             (nemo-relay-test--write-json-config json-path output-directory)
             (let ((settings
                     (nemo-relay-configuration-create
                      :enabled-p t
                      :config json-path)))
               (test-assert
                (= (json-get
                    (json-decode
                     (nemo-relay--configuration-plugin-config-json settings))
                    "version")
                   1)
                "Relay retains legacy JSON file compatibility")))
           (sb-posix:setenv "AUTOLITH_RELAY" "true" 1)
           (sb-posix:setenv "AUTOLITH_RELAY_CONFIG" (namestring config-path) 1)
           (sb-posix:setenv "AUTOLITH_RELAY_LIBRARY" "environment-library" 1)
           (let ((settings (nemo-relay--runtime-configuration nil)))
             (test-assert
              (nemo-relay-configuration-enabled-p settings)
              "environment Relay enablement is honored")
             (test-assert
              (equal (nemo-relay-configuration-config-path settings) config-path)
              "environment Relay TOML pathname is honored")
             (test-assert
              (string= (nemo-relay-configuration-library-path settings)
                       "environment-library")
              "environment Relay library is honored"))
           (let ((settings
                   (nemo-relay-configuration-create
                    :enabled-p nil
                    :config config-path
                    :config-json "{\"version\":1,\"components\":[]}"
                    :library-path "explicit-library")))
             (test-assert
              (not (nemo-relay-configuration-enabled-p settings))
              "explicit Relay enabled value overrides the environment")
             (test-assert
              (string= (nemo-relay-configuration-library-path settings)
                       "explicit-library")
              "explicit Relay library overrides the environment")
             (test-assert
              (equal (nemo-relay-configuration-config-path settings) config-path)
              "direct PluginConfig JSON keeps its source pathname available")
             (test-assert
              (let ((components
                      (json-get
                       (json-decode
                        (nemo-relay-configuration-plugin-config-json settings))
                       "components")))
                (and (vectorp components) (zerop (length components))))
              "direct PluginConfig JSON is retained"))
           (let ((failed-p nil))
             (handler-case
                 (nemo-relay-configuration-create :plugin-config "{")
               (nemo-relay-error ()
                 (setf failed-p t)))
             (test-assert failed-p "invalid Relay PluginConfig JSON is rejected")))
      (dolist (entry saved-environment)
        (if (rest entry)
            (sb-posix:setenv (first entry) (rest entry) 1)
            (sb-posix:unsetenv (first entry))))
      (setf *nemo-relay-configuration* saved-configuration)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-toml-dynamic-plugins () null)
(defun test-nemo-relay-toml-dynamic-plugins ()
  "Test TOML dynamic manifests become Relay activation specifications."
  (let* ((root (nemo-relay-test--temporary-root))
         (plugin-directory (merge-pathnames "plugin/" root))
         (config-path (merge-pathnames "plugins.toml" root))
         (manifest-path (merge-pathnames "relay-plugin.toml" plugin-directory)))
    (uiop:ensure-all-directories-exist (list plugin-directory))
    (unwind-protect
         (progn
           (with-open-file (stream config-path
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream
                     "version = 1~%~%[[components]]~%kind = \"example.custom_observer\"~%enabled = true~%~%[[plugins.dynamic]]~%manifest = \"./plugin/relay-plugin.toml\"~%~%[plugins.dynamic.config]~%mode = \"audit\"~%~%[plugins.dynamic.config.executor]~%worker_threads = 4~%"))
           (with-open-file (stream manifest-path
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
              (format stream
                      "manifest_version = 1~%~%[plugin]~%id = \"example.custom_observer\"~%kind = \"rust_dynamic\"~%~%[compat]~%relay = \">=0.8.0,<1.0\"~%native_api = \"1\"~%~%[defaults]~%enabled = false~%~%[capabilities]~%items = [\"plugin_native\"]~%~%[load]~%library = \"libcustom.dylib\"~%symbol = \"nemo_relay_register_plugin\"~%"))
           (let* ((settings
                    (nemo-relay-configuration-create
                     :enabled-p t
                     :config config-path
                     :allowed-component-kinds
                     '("example.custom_observer")
                     :allowed-dynamic-plugin-ids
                     '("example.custom_observer")))
                 (specs
                   (json-decode
                    (nemo-relay--configuration-dynamic-plugins-json settings)))
                 (spec (and (vectorp specs) (aref specs 0)))
                 (config (and spec (json-get spec "config")))
                 (executor (and config (json-get config "executor"))))
             (test-assert
              (and (vectorp specs) (= (length specs) 1))
              "Relay reads one TOML dynamic-plugin declaration")
             (test-assert
              (and spec
                   (string= (json-get spec "plugin_id")
                            "example.custom_observer"))
              "Relay reads the custom plugin ID from its manifest")
             (test-assert
              (and spec
                   (string= (json-get spec "kind") "rust_dynamic"))
              "Relay reads the dynamic plugin lane from its manifest")
             (test-assert
              (and spec
                   (string= (json-get spec "manifest_ref")
                            (namestring (truename manifest-path))))
              "Relay resolves dynamic manifests relative to plugins.toml")
             (test-assert
              (and (json-object-p config)
                   (string= (json-get config "mode") "audit")
                   (= (json-get executor "worker_threads") 4))
              "Relay preserves nested custom plugin configuration"))
           (let ((failed-p nil))
             (handler-case
                 (nemo-relay--configuration-dynamic-plugins-json
                  (nemo-relay-configuration-create
                   :config config-path
                   :allowed-component-kinds
                   '("example.custom_observer")))
               (nemo-relay-error ()
                 (setf failed-p t)))
             (test-assert
              failed-p
              "Relay rejects dynamic plugins without an explicit ID allowlist"))
           (with-open-file (stream config-path
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream
                     "version = 1~%~%[[plugins.dynamic]]~%manifest = \"./plugin/relay-plugin.toml\"~%~%[[plugins.dynamic]]~%manifest = \"./plugin/relay-plugin.toml\"~%"))
           (let ((failed-p nil))
             (handler-case
                   (nemo-relay--configuration-dynamic-plugins-json
                    (nemo-relay-configuration-create
                     :config config-path
                     :allowed-dynamic-plugin-ids
                     '("example.custom_observer")))
               (nemo-relay-error ()
                 (setf failed-p t)))
             (test-assert
              failed-p
              "Relay rejects duplicate dynamic plugin IDs")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-observability-boundary () null)
(defun test-nemo-relay-observability-boundary ()
  "Test that Relay registration and configuration stay observability-only."
  (let ((reserved-kinds
          '("observability" "nemo_guardrails" "pricing")))
    (dolist (kind (rest reserved-kinds))
      (let ((failed-p nil))
        (handler-case
            (nemo-relay-configuration-create
             :enabled-p t
             :plugin-config
             (json-object
              "version" 1
              "components"
              (json-array
               (json-object "kind" kind "enabled" t)))
             :allowed-component-kinds (list kind))
          (nemo-relay-error ()
            (setf failed-p t)))
        (test-assert
         failed-p
         (format nil "Relay rejects non-observability built-in ~A even when allowlisted."
                 kind))))
    (dolist (kind reserved-kinds)
      (let ((failed-p nil))
        (handler-case
            (nemo-relay-configuration-create
             :enabled-p t
             :allowed-dynamic-plugin-ids (list kind))
          (nemo-relay-error ()
            (setf failed-p t)))
        (test-assert
         failed-p
         (format nil "Relay rejects reserved dynamic plugin ID ~A." kind))))
    (let ((failed-p nil))
      (handler-case
          (nemo-relay-configuration-create
           :enabled-p t
           :plugin-config
           (json-object
            "version" 1
            "components"
            (json-array
             (json-object "kind" "example.custom_observer" "enabled" t))))
        (nemo-relay-error ()
          (setf failed-p t)))
      (test-assert
       failed-p
       "Relay rejects an unallowlisted custom static component."))
    (let ((settings
            (nemo-relay-configuration-create
             :enabled-p t
             :plugin-config
             (json-object
              "version" 1
              "components"
              (json-array
               (json-object "kind" "example.custom_observer" "enabled" t)))
             :allowed-component-kinds '("example.custom_observer"))))
      (test-assert
       (typep settings 'nemo-relay-configuration)
       "Relay accepts an explicitly allowlisted custom static component."))
    (dolist (kind reserved-kinds)
      (test-assert
       (handler-case
           (progn
             (nemo-relay-register-plugin kind #'identity)
             nil)
         (nemo-relay-error ()
           t)
         (serious-condition ()
           nil))
       (format nil "Relay rejects registration of reserved plugin kind ~A." kind))
      (test-assert
       (handler-case
           (progn
             (nemo-relay-deregister-plugin kind)
             nil)
         (nemo-relay-error ()
           t)
         (serious-condition ()
           nil))
       (format nil "Relay rejects deregistration of reserved plugin kind ~A." kind))))
  nil)

(-> test-nemo-relay-home-implicit-discovery () null)
(defun test-nemo-relay-home-implicit-discovery ()
  "Test that Relay's HOME fallback is included in implicit discovery checks."
  (let* ((root (nemo-relay-test--temporary-root))
         (home (merge-pathnames "home/" root))
         (discovered-path
           (merge-pathnames ".config/nemo-relay/plugins.toml" home))
         (saved-environment
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   '("HOME" "USERPROFILE" "XDG_CONFIG_HOME"))))
    (uiop:ensure-all-directories-exist
     (list (uiop:pathname-directory-pathname discovered-path)))
    (unwind-protect
         (progn
           (with-open-file (stream discovered-path
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "{}" stream))
           (sb-posix:setenv "HOME" (namestring home) 1)
           (sb-posix:unsetenv "USERPROFILE")
           (sb-posix:unsetenv "XDG_CONFIG_HOME")
           (let ((message nil))
             (test-assert
              (handler-case
                  (progn
                    (nemo-relay--ensure-no-implicit-plugin-config)
                    nil)
                (nemo-relay-error (condition)
                  (setf message (autolith-error-message condition))
                  t))
              "Relay rejects HOME-based implicit plugin discovery")
             (test-assert
              (and message
                   (search (namestring discovered-path) message :test #'char=))
              "Relay reports the HOME-based implicit plugin pathname")))
      (dolist (entry saved-environment)
        (tests--restore-environment (first entry) (rest entry)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> nemo-relay-test--valid-rust-dynamic-manifest () json-object)
(defun nemo-relay-test--valid-rust-dynamic-manifest ()
  "Return a complete valid native dynamic-plugin manifest for tests."
  (json-object
   "manifest_version" 1
   "plugin"
   (json-object
    "id" "example.custom_observer"
    "kind" "rust_dynamic")
   "compat"
   (json-object
    "relay" ">=0.8.0,<1.0"
    "native_api" "1")
   "defaults"
   (json-object "enabled" false)
   "capabilities"
   (json-object "items" (json-array "plugin_native"))
   "load"
   (json-object
    "library" "libcustom.dylib"
    "symbol" "nemo_relay_register_plugin")))

(-> nemo-relay-test--manifest-rejected-p (json-object) boolean)
(defun nemo-relay-test--manifest-rejected-p (manifest)
  "Return true when MANIFEST fails the local upstream-shape checks."
  (handler-case
      (progn
        (nemo-relay--validate-dynamic-plugin-manifest
         manifest #P"/tmp/relay-plugin.toml")
        nil)
    (nemo-relay-error ()
      t)))

(-> test-nemo-relay-dynamic-manifest-metadata () null)
(defun test-nemo-relay-dynamic-manifest-metadata ()
  "Test strict validation of upstream dynamic-plugin manifest metadata."
  (let ((manifest (nemo-relay-test--valid-rust-dynamic-manifest)))
    (test-assert
     (not (nemo-relay-test--manifest-rejected-p manifest))
     "Relay accepts a complete custom dynamic-plugin manifest")
    (setf (gethash "manifest_version" manifest) 2)
    (test-assert
     (nemo-relay-test--manifest-rejected-p manifest)
     "Relay rejects an unsupported dynamic-plugin manifest version")
    (setf (gethash "manifest_version" manifest) 1
          (gethash "capabilities" manifest)
          (json-object "items" (json-array "plugin_worker")))
    (test-assert
     (nemo-relay-test--manifest-rejected-p manifest)
     "Relay rejects a dynamic-plugin capability from the opposite lane")
    (setf (gethash "capabilities" manifest)
          (json-object "items" (json-array "plugin_native"))
          (gethash "load" manifest)
          (json-object "runtime" "python" "entrypoint" "custom:register"))
    (test-assert
     (nemo-relay-test--manifest-rejected-p manifest)
     "Relay rejects worker load fields for a native dynamic plugin")
    (setf (gethash "load" manifest)
          (json-object
           "library" "libcustom.dylib"
           "symbol" "nemo_relay_register_plugin")
          (gethash "defaults" manifest)
          (json-object "enabled" t))
    (test-assert
     (nemo-relay-test--manifest-rejected-p manifest)
     "Relay rejects enabled-by-default dynamic plugins"))
  nil)

(-> test-nemo-relay-disabled-wrappers () null)
(defun test-nemo-relay-disabled-wrappers ()
  "Test that every Relay wrapper preserves values while disabled."
  (let ((*nemo-relay-runtime* nil)
        (*nemo-relay-instrumentation-suppressed-p* nil)
        (evaluated-p nil))
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-agent
            ((progn
               (setf evaluated-p t)
               "disabled-agent")
             :input (progn
                      (setf evaluated-p t)
                      (json-object "input" t))
             :metadata (progn
                        (setf evaluated-p t)
                        (json-object "metadata" t)))
          (values :agent-one :agent-two))
      (test-assert
       (and (eq first-value ':agent-one)
            (eq second-value ':agent-two))
       "disabled Agent wrapper preserves multiple values"))
    (test-assert
     (not evaluated-p)
     "disabled Agent wrapper does not build Relay payloads")
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-llm
            ((progn
               (setf evaluated-p t)
               "disabled-llm")
             (progn
               (setf evaluated-p t)
               "model")
             (progn
               (setf evaluated-p t)
               (json-object "x" 1)))
          (values :llm-one :llm-two))
      (test-assert
       (and (eq first-value ':llm-one)
            (eq second-value ':llm-two))
       "disabled LLM wrapper preserves multiple values"))
    (test-assert
     (not evaluated-p)
     "disabled LLM wrapper does not build Relay payloads")
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-tool
            ((progn
               (setf evaluated-p t)
               "disabled-tool")
             (progn
               (setf evaluated-p t)
               "call")
             (progn
               (setf evaluated-p t)
               (json-object "x" 1)))
          (values :tool-one :tool-two))
      (test-assert
       (and (eq first-value ':tool-one)
            (eq second-value ':tool-two))
       "disabled Tool wrapper preserves multiple values"))
    (test-assert
     (not evaluated-p)
     "disabled Tool wrapper does not build Relay payloads")
    (test-assert
     (null (nemo-relay-flush))
     "disabled Relay flush is a no-op")
    (test-assert
     (null (nemo-relay-mark "disabled.mark"))
     "disabled Relay marks are a no-op"))
    (let ((*nemo-relay-runtime* :sentinel)
          (*nemo-relay-last-error* "stale")
          (*nemo-relay-propagation-context-json* "{}")
          (*nemo-relay-instrumentation-suppressed-p* t))
      (nemo-relay--detach-for-checkpoint)
      (test-assert
       (and (null *nemo-relay-runtime*)
            (null *nemo-relay-last-error*)
            (null *nemo-relay-propagation-context-json*)
            (null *nemo-relay-instrumentation-suppressed-p*))
       "checkpoint preparation drops process-local Relay state"))
  nil)


(-> test-nemo-relay-unavailable-library () null)
(defun test-nemo-relay-unavailable-library ()
  "Test a missing Relay library degrades to a diagnostic."
  (let ((root (nemo-relay-test--temporary-root)))
    (let ((*nemo-relay-runtime* nil)
          (*nemo-relay-configuration* nil)
          (*nemo-relay-last-error* nil))
      (unwind-protect
           (progn
              (nemo-relay-configure
               :enabled t
               :config-json "{\"version\":1,\"components\":[]}"
               :library (namestring
                         (merge-pathnames
                          "missing/libnemo-relay-ffi.dylib"
                          root)))
             (test-assert
              (not (nemo-relay-start))
              "Relay startup failure does not affect the caller")
             (test-assert
              (and (null *nemo-relay-runtime*)
                   (non-empty-string-p (nemo-relay-last-error)))
              "Relay startup retains an actionable diagnostic"))
        (nemo-relay-shutdown)
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist ':ignore))))
  nil)



(-> test-nemo-relay-observability-bindings () null)
(defun test-nemo-relay-observability-bindings ()
  "Test Relay observability value wrappers and callback state ownership."
  (let ((measurement
          (make-instance 'nemo-relay-metric-measurement
                         :name "requests"
                         :kind ':counter
                         :value-type ':u64
                         :value 3)))
    (test-assert (string= (nemo-relay-metric-measurement-name measurement) "requests")
                 "typed measurement retains its name")
    (test-assert (eq (nemo-relay-metric-measurement-kind measurement) ':counter)
                 "typed measurement retains its kind")
    (test-assert (eq (nemo-relay-metric-measurement-value-type measurement) ':u64)
                 "typed measurement retains its value type")
    (test-assert (= (nemo-relay-metric-measurement-value measurement) 3)
                 "typed measurement retains its value")
    (test-assert (null (nemo-relay-metric-measurement-unit measurement))
                 "typed measurement unit defaults to NIL")
    (test-assert (null (nemo-relay-metric-measurement-description measurement))
                 "typed measurement description defaults to NIL")
    (test-assert (null (nemo-relay-metric-measurement-attributes measurement))
                 "typed measurement attributes default to NIL")
    (test-assert (null (nemo-relay-metric-measurement-boundaries measurement))
                 "typed measurement boundaries default to NIL"))
  (let ((measurement
          (make-instance 'nemo-relay-metric-measurement
                         :name "latency"
                         :kind ':histogram
                         :value-type ':f64
                         :value 1.5d0
                         :unit "ms"
                         :description "request latency"
                         :attributes (json-object "source" "test")
                         :boundaries #(0.0d0 1.0d0))))
    (test-assert (string= (nemo-relay-metric-measurement-unit measurement) "ms")
                 "typed measurement accepts an optional unit")
    (test-assert
     (string= (nemo-relay-metric-measurement-description measurement) "request latency")
     "typed measurement accepts an optional description")
    (test-assert (json-object-p (nemo-relay-metric-measurement-attributes measurement))
                 "typed measurement accepts optional attributes")
    (test-assert
     (equalp (nemo-relay-metric-measurement-boundaries measurement) #(0.0d0 1.0d0))
     "typed measurement accepts optional boundaries"))
  (let ((before (hash-table-count *nemo-relay-callback-states*)))
    (multiple-value-bind (state token)
        (nemo-relay--callback-state-create ':test #'identity)
      (unwind-protect
           (progn
             (test-assert (= (hash-table-count *nemo-relay-callback-states*) (1+ before))
                          "callback state registration retains one state")
             (test-assert (eq (nemo-relay--callback-state-for-pointer token) state)
                          "callback state lookup uses the foreign token")
             (test-assert (nemo-relay--callback-state-release token)
                          "callback state release succeeds"))
        (nemo-relay--callback-state-release token))
      (test-assert (nemo-relay-callback-state-released-p state)
                   "callback state release marks the state released")
      (test-assert (= (hash-table-count *nemo-relay-callback-states*) before)
                   "callback state release removes the registry entry")))
  (let ((result nil))
    (test-call-with-function-replacements
     (list
      (list 'nemo-relay--ffi-status
            (lambda (operation function)
              (declare (ignore operation function))
              nil)))
     (lambda ()
       (multiple-value-bind (value status)
           (nemo-relay--take-string-output
            "relay-test-string-output"
            (lambda (slot)
              (declare (ignore slot))
              0))
         (setf result (list value status)))))
    (test-assert (equal result '(nil nil))
                 "string output preserves an explicit failed status"))
  (let ((failure nil)
        (*nemo-relay-observability-library-loaded-p* t)
        (*nemo-relay-last-error* "stale Relay diagnostic"))
    (test-call-with-function-replacements
     (list
      (list 'nemo-relay--ffi-status
            (lambda (operation function)
              (declare (ignore operation function))
              nil)))
     (lambda ()
       (handler-case
           (nemo-relay--string-output
            "relay-test-string-output"
            (lambda (slot)
              (declare (ignore slot))
              0))
         (nemo-relay-error (condition)
           (setf failure condition)))))
    (test-assert
     (and (typep failure 'nemo-relay-error)
          (string= (nemo-relay-error-operation failure) "relay-test-string-output")
          (string= (autolith-error-message failure) "stale Relay diagnostic"))
     "string output signals on a failed status despite a null result"))
  nil)


(-> test-nemo-relay-observability-correctness () null)
(defun test-nemo-relay-observability-correctness ()
  "Test Relay observability ABI boundaries and callback diagnostics."
  (test-assert (eq (nemo-relay-scope-type-name 0) ':agent)
               "scope type accessors return keywords")
  (let ((failure nil))
    (handler-case
        (nemo-relay-scope-type-name 99)
      (nemo-relay-error (condition)
        (setf failure condition)))
    (test-assert (typep failure 'nemo-relay-error)
                 "unknown scope type codes signal Relay errors"))
  (test-assert
   (and (fboundp '%nemo-relay-otel-subscriber-create-with-projection-options-v2)
        (fboundp '%nemo-relay-scope-register-subscriber)
        (fboundp '%nemo-relay-scope-deregister-subscriber)
        (fboundp '%nemo-relay-set-last-error-message)
        (fboundp '%nemo-relay-tool-handle-free)
        (fboundp '%nemo-relay-tool-handle-uuid)
        (fboundp '%nemo-relay-tool-handle-name)
        (fboundp '%nemo-relay-tool-handle-attributes)
        (fboundp '%nemo-relay-tool-handle-parent-uuid)
        (fboundp '%nemo-relay-llm-handle-free)
        (fboundp '%nemo-relay-llm-handle-uuid)
        (fboundp '%nemo-relay-llm-handle-name)
        (fboundp '%nemo-relay-llm-handle-attributes)
        (fboundp '%nemo-relay-llm-handle-parent-uuid))
   "manual lifecycle and handle FFI entrypoints are bound")
  (test-assert
   (and (find-class 'nemo-relay-tool-handle nil)
        (find-class 'nemo-relay-llm-handle nil)
        (fboundp 'nemo-relay-tool-handle-free)
        (fboundp 'nemo-relay-tool-handle-uuid)
        (fboundp 'nemo-relay-tool-handle-name)
        (fboundp 'nemo-relay-tool-handle-attributes)
        (fboundp 'nemo-relay-tool-handle-parent-uuid)
        (fboundp 'nemo-relay-llm-handle-free)
        (fboundp 'nemo-relay-llm-handle-uuid)
        (fboundp 'nemo-relay-llm-handle-name)
        (fboundp 'nemo-relay-llm-handle-attributes)
        (fboundp 'nemo-relay-llm-handle-parent-uuid)
        (fboundp 'nemo-relay-tool-call)
        (fboundp 'nemo-relay-tool-call-end)
        (fboundp 'nemo-relay-llm-call)
        (fboundp 'nemo-relay-llm-call-end))
   "manual lifecycle handle classes, accessors, and wrappers are bound")
  (let ((tool-free-count 0)
        (llm-free-count 0))
    (test-call-with-function-replacements
     (list
      (list '%nemo-relay-tool-handle-free
            (lambda (pointer)
              (declare (ignore pointer))
              (incf tool-free-count)))
      (list '%nemo-relay-llm-handle-free
            (lambda (pointer)
              (declare (ignore pointer))
              (incf llm-free-count))))
     (lambda ()
       (let ((tool (make-instance 'nemo-relay-tool-handle :pointer (cffi:null-pointer)))
             (llm (make-instance 'nemo-relay-llm-handle :pointer (cffi:null-pointer))))
         (test-assert (nemo-relay-tool-handle-free tool)
                      "tool handle free succeeds")
         (test-assert (nemo-relay-native-handle-freed-p tool)
                      "tool handle free marks the wrapper")
         (test-assert (nemo-relay-tool-handle-free tool)
                      "tool handle free is idempotent")
         (test-assert (= tool-free-count 1)
                      "tool handle free releases native storage once")
         (test-assert (nemo-relay-llm-handle-free llm)
                      "LLM handle free succeeds")
         (test-assert (nemo-relay-native-handle-freed-p llm)
                      "LLM handle free marks the wrapper")
         (test-assert (nemo-relay-llm-handle-free llm)
                      "LLM handle free is idempotent")
         (test-assert (= llm-free-count 1)
                      "LLM handle free releases native storage once")))))
  (labels ((measurement (value-type value)
             (make-instance 'nemo-relay-metric-measurement
                            :name "value"
                            :kind ':counter
                            :value-type value-type
                            :value value))
           (metric-error-p (value-type value)
             (handler-case
                 (progn
                   (nemo-relay--metric-native-call
                    "relay-test.metric"
                    (cffi:null-pointer)
                    (list (measurement value-type value))
                    nil
                    nil)
                   nil)
               (nemo-relay-error () t))))
    (test-assert (metric-error-p ':u64 -1)
                 "typed metrics reject negative U64 values")
    (test-assert (metric-error-p ':u64 (1+ *nemo-relay-uint64-max*))
                 "typed metrics reject overflowing U64 values")
    (test-assert (metric-error-p ':i64 (1- *nemo-relay-int64-min*))
                 "typed metrics reject underflowing I64 values")
    (test-assert (metric-error-p ':i64 (1+ *nemo-relay-int64-max*))
                 "typed metrics reject overflowing I64 values"))
  (let ((failure nil))
    (handler-case
        (nemo-relay-metric :name "relay-test.empty" :measurements #())
      (nemo-relay-error (condition)
        (setf failure condition)))
    (test-assert (typep failure 'nemo-relay-error)
                 "typed metrics reject empty measurement arrays"))
  (let ((failure nil))
    (handler-case
        (nemo-relay-metric-json :name "relay-test.empty" :measurements #())
      (nemo-relay-error (condition)
        (setf failure condition)))
    (test-assert (typep failure 'nemo-relay-error)
                 "JSON metrics reject empty measurement arrays"))
  (labels ((capture-boundaries (boundaries)
             (let ((present-p nil)
                   (length 0))
               (test-call-with-function-replacements
                (list
                 (list 'nemo-relay--require-status
                       (lambda (operation function)
                         (declare (ignore operation))
                         (funcall function)))
                 (list '%nemo-relay-metric
                       (lambda (name parent measurements count metadata timestamp)
                         (declare (ignore name parent count metadata timestamp))
                         (let ((struct
                                 (cffi:mem-aptr
                                  measurements
                                  '(:struct nemo-relay-metric-measurement)
                                  0)))
                            (setf present-p
                                  (nemo-relay--pointer-present-p
                                   (cffi:foreign-slot-value
                                    struct
                                    '(:struct nemo-relay-metric-measurement)
                                    'boundaries))
                                  length
                                 (cffi:foreign-slot-value
                                  struct
                                  '(:struct nemo-relay-metric-measurement)
                                  'boundaries-len)))
                         0)))
                (lambda ()
                  (nemo-relay--metric-native-call
                   "relay-test.boundaries"
                   (cffi:null-pointer)
                   (list (make-instance 'nemo-relay-metric-measurement
                                        :name "histogram"
                                        :kind ':histogram
                                        :value-type ':f64
                                        :value 1.0d0
                                        :boundaries boundaries))
                   nil
                   nil)))
               (values present-p length))))
    (multiple-value-bind (present-p length)
        (capture-boundaries nil)
      (test-assert (and (not present-p) (zerop length))
                   "absent metric boundaries use a null pointer"))
    (multiple-value-bind (present-p length)
        (capture-boundaries #())
      (test-assert (and present-p (zerop length))
                   "explicit empty metric boundaries use a non-null pointer")))
  (let ((native-message nil)
        (*nemo-relay-observability-library-loaded-p* t))
    (test-call-with-function-replacements
     (list
      (list '%nemo-relay-set-last-error-message
            (lambda (pointer)
              (setf native-message
                    (cffi:foreign-string-to-lisp pointer :encoding ':utf-8)))))
     (lambda ()
       (nemo-relay--callback-failure
        "relay-test-callback"
        (make-condition 'simple-error
                        :format-control "callback failure"
                        :format-arguments nil))))
    (test-assert (and (non-empty-string-p native-message)
                      (string= native-message (nemo-relay-last-error)))
                 "callback failures update Relay's native last-error channel"))
  (let ((captured-scope nil)
        (captured-name nil)
        (captured-token nil)
        (before (hash-table-count *nemo-relay-callback-states*)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'nemo-relay-scope-handle-uuid
                 (lambda (scope)
                   (declare (ignore scope))
                   "scope-uuid"))
           (list 'nemo-relay--require-status
                 (lambda (operation function)
                   (declare (ignore operation))
                   (funcall function)))
           (list '%nemo-relay-scope-register-subscriber
                 (lambda (scope-pointer name-pointer callback user-data free-callback)
                   (declare (ignore callback free-callback))
                   (setf captured-scope
                         (cffi:foreign-string-to-lisp scope-pointer)
                         captured-name
                         (cffi:foreign-string-to-lisp name-pointer)
                         captured-token user-data)
                   0))
           (list '%nemo-relay-scope-deregister-subscriber
                 (lambda (scope-pointer name-pointer)
                   (declare (ignore scope-pointer name-pointer))
                   0)))
          (lambda ()
            (test-assert
             (nemo-relay-scope-register-subscriber
              :test-scope
              "scope-subscriber"
              #'identity)
             "scope-local subscriber registration succeeds")
            (test-assert (string= captured-scope "scope-uuid")
                         "scope-local registration passes the scope UUID")
            (test-assert (string= captured-name "scope-subscriber")
                         "scope-local registration passes the subscriber name")
            (test-assert (= (hash-table-count *nemo-relay-callback-states*) (1+ before))
                         "scope-local registration retains callback state")
            (test-assert
             (nemo-relay-scope-deregister-subscriber :test-scope "scope-subscriber")
             "scope-local subscriber deregistration succeeds")))
      (when captured-token
        (nemo-relay--callback-state-release captured-token)))
    (test-assert (= (hash-table-count *nemo-relay-callback-states*) before)
                 "scope-local subscriber cleanup releases callback state"))
  nil)


(-> test-nemo-relay-native-observability-lifecycle () null)
(defun test-nemo-relay-native-observability-lifecycle ()
  "Exercise direct Relay observability constructors when native tests are enabled."
  (let* ((library (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))
         (requested (nemo-relay--environment-boolean "AUTOLITH_RELAY_TESTS" nil)))
    (cond
      ((and (non-empty-string-p library) (probe-file library))
       (let* ((root (nemo-relay-test--temporary-root))
              (output-directory (merge-pathnames "events/" root))
              (subscriber-name
                (format nil "autolith.test.observability.~A" (make-identifier)))
              (event-name
                (format nil "autolith.test.observability.event.~A" (make-identifier)))
              (saved-library *nemo-relay-observability-library*)
              (saved-loaded-p *nemo-relay-observability-library-loaded-p*)
              (saved-error *nemo-relay-last-error*)
              (saved-header-env-value (uiop:getenv "AUTOLITH_RELAY_TEST_HEADER_VALUE"))
              (callback-state-count (hash-table-count *nemo-relay-callback-states*))
              (callback-name nil)
              (callback-kind nil)
              (callback-json nil)
              (atif nil)
              (atof nil)
              (otel nil)
              (otel-log nil)
              (otel-metric nil))
         (uiop:ensure-all-directories-exist (list output-directory))
         (sb-posix:setenv "AUTOLITH_RELAY_TEST_HEADER_VALUE" "unused" 1)
         (unwind-protect
              (progn
                (nemo-relay--ensure-native-library)
                (test-assert
                 (nemo-relay-register-subscriber
                  subscriber-name
                  (lambda (event)
                    (setf callback-name (nemo-relay-event-name event)
                          callback-kind (nemo-relay-event-kind event)
                          callback-json (nemo-relay-event-json event))))
                 "direct subscriber registration succeeds")
                (test-assert
                 (= (hash-table-count *nemo-relay-callback-states*)
                    (1+ callback-state-count))
                 "direct subscriber retains callback state")
                (test-assert
                 (nemo-relay-event
                  :name event-name
                  :data (json-object "ok" t)
                  :metadata (json-object "source" "test"))
                 "direct event emission succeeds")
                (test-assert (nemo-relay-flush-subscribers)
                             "direct subscriber flush succeeds")
                (test-assert (string= callback-name event-name)
                             "direct subscriber receives the event name")
                (test-assert (non-empty-string-p callback-kind)
                             "direct subscriber receives the event kind")
                (test-assert (json-object-p callback-json)
                             "direct subscriber can read canonical event JSON")
                (test-assert (nemo-relay-deregister-subscriber subscriber-name)
                             "direct subscriber deregistration succeeds")
                (test-assert (nemo-relay-flush-subscribers)
                             "direct subscriber cleanup flush succeeds")
                (test-assert (= (hash-table-count *nemo-relay-callback-states*)
                                callback-state-count)
                             "direct subscriber cleanup releases callback state")
                (setf atif
                      (nemo-relay-atif-exporter-create
                       :session-id "autolith-test-session"
                       :agent-name "autolith-test"
                       :agent-version "test"
                       :model-name "test-model"))
                (test-assert (typep atif 'nemo-relay-atif-exporter)
                             "ATIF exporter construction returns its handle class")
                (test-assert (nemo-relay-atif-exporter-free atif)
                             "ATIF exporter free succeeds")
                (test-assert (nemo-relay-native-handle-freed-p atif)
                             "ATIF exporter free marks its handle")
                (setf atif nil)
                (setf atof
                      (nemo-relay-atof-exporter-create
                       :output-directory (namestring output-directory)
                       :mode ':overwrite
                       :filename "events.jsonl"))
                (test-assert (typep atof 'nemo-relay-atof-exporter)
                             "ATOF exporter construction returns its handle class")
                (test-assert (pathnamep (nemo-relay-atof-exporter-pathname atof))
                             "ATOF exporter exposes its output pathname")
                (test-assert (nemo-relay-atof-exporter-free atof)
                             "ATOF exporter free succeeds")
                (test-assert (nemo-relay-native-handle-freed-p atof)
                             "ATOF exporter free marks its handle")
                (setf atof nil)
                (setf otel
                      (nemo-relay-otel-subscriber-create
                       :type ':full
                       :transport ':http_binary
                       :endpoint "http://127.0.0.1:4318"
                       :header-env (json-object "x-autolith-test" "AUTOLITH_RELAY_TEST_HEADER_VALUE")))
                (test-assert (typep otel 'nemo-relay-otel-subscriber)
                             "OTLP trace construction returns its handle class")
                 (test-assert
                  (let ((diagnostics (nemo-relay-otel-subscriber-runtime-diagnostics otel)))
                    (or (vectorp diagnostics) (listp diagnostics)))
                  "OTLP trace diagnostics return an array")
                (test-assert (nemo-relay-otel-subscriber-free otel)
                             "OTLP trace free succeeds")
                (test-assert (nemo-relay-native-handle-freed-p otel)
                             "OTLP trace free marks its handle")
                (setf otel nil)
                (setf otel-log
                      (nemo-relay-otel-log-subscriber-create
                       :transport ':http_binary
                       :endpoint "http://127.0.0.1:4318"
                       :header-env (json-object "x-autolith-test" "AUTOLITH_RELAY_TEST_HEADER_VALUE")
                       :minimum-severity ':info))
                (test-assert (typep otel-log 'nemo-relay-otel-log-subscriber)
                             "OTLP log construction returns its handle class")
                 (test-assert
                  (let ((diagnostics (nemo-relay-otel-log-subscriber-runtime-diagnostics otel-log)))
                    (or (vectorp diagnostics) (listp diagnostics)))
                  "OTLP log diagnostics return an array")
                (test-assert (nemo-relay-otel-log-subscriber-free otel-log)
                             "OTLP log free succeeds")
                (test-assert (nemo-relay-native-handle-freed-p otel-log)
                             "OTLP log free marks its handle")
                (setf otel-log nil)
                (setf otel-metric
                      (nemo-relay-otel-metric-subscriber-create
                       :transport ':http_binary
                       :endpoint "http://127.0.0.1:4318"
                       :header-env (json-object "x-autolith-test" "AUTOLITH_RELAY_TEST_HEADER_VALUE")
                       :temporality ':cumulative))
                (test-assert (typep otel-metric 'nemo-relay-otel-metric-subscriber)
                             "OTLP metric construction returns its handle class")
                 (test-assert
                  (let ((diagnostics (nemo-relay-otel-metric-subscriber-runtime-diagnostics otel-metric)))
                    (or (vectorp diagnostics) (listp diagnostics)))
                  "OTLP metric diagnostics return an array")
                (test-assert (nemo-relay-otel-metric-subscriber-free otel-metric)
                             "OTLP metric free succeeds")
                (test-assert (nemo-relay-native-handle-freed-p otel-metric)
                             "OTLP metric free marks its handle")
                (setf otel-metric nil))
           (ignore-errors (nemo-relay-deregister-subscriber subscriber-name))
           (ignore-errors (nemo-relay-flush-subscribers))
           (when atif
             (ignore-errors (nemo-relay-atif-exporter-free atif)))
           (when atof
             (ignore-errors (nemo-relay-atof-exporter-free atof)))
           (when otel
             (ignore-errors (nemo-relay-otel-subscriber-free otel)))
           (when otel-log
             (ignore-errors (nemo-relay-otel-log-subscriber-free otel-log)))
           (when otel-metric
             (ignore-errors (nemo-relay-otel-metric-subscriber-free otel-metric)))
            (if saved-header-env-value
                (sb-posix:setenv "AUTOLITH_RELAY_TEST_HEADER_VALUE"
                                 saved-header-env-value 1)
                (sb-posix:unsetenv "AUTOLITH_RELAY_TEST_HEADER_VALUE"))
           (setf *nemo-relay-observability-library* saved-library
                 *nemo-relay-observability-library-loaded-p* saved-loaded-p
                 *nemo-relay-last-error* saved-error)
           (uiop:delete-directory-tree
            root :validate t :if-does-not-exist ':ignore))
         (test-assert (= (hash-table-count *nemo-relay-callback-states*)
                         callback-state-count)
                      "native observability cleanup releases all callback states")))
      (requested
       (test-assert
        nil
        "AUTOLITH_RELAY_TESTS requires a usable AUTOLITH_RELAY_LIBRARY"))
      (t
       (test-assert
        t
        "native Relay observability tests are disabled without AUTOLITH_RELAY_TESTS"))))
  nil)


(-> nemo-relay-test--read-events (pathname) list)
(defun nemo-relay-test--read-events (pathname)
  "Read newline-delimited JSON Relay events from PATHNAME."
  (with-open-file (stream pathname
                          :direction ':input
                          :external-format ':utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect (json-decode line))))

(-> nemo-relay-test--find-event
    (list &key (:name string) (:category (option string))
          (:scope-category (option string))
          (:uuid (option string)))
    (option json-object))
(defun nemo-relay-test--find-event
    (events &key name category scope-category uuid)
  "Find one event matching the supplied Relay event fields."
  (find-if
   (lambda (event)
     (and (string= (json-get event "name") name)
          (or (null category)
              (string= (json-get event "category") category))
          (or (null scope-category)
              (string= (json-get event "scope_category") scope-category))
          (or (null uuid)
              (string= (json-get event "uuid") uuid))))
   events))

(-> nemo-relay-test--find-events
    (list &key (:name string) (:category (option string))
          (:scope-category (option string))
          (:parent-uuid (option string)))
    list)
(defun nemo-relay-test--find-events
    (events &key name category scope-category parent-uuid)
  "Find all events matching the supplied Relay event fields."
  (remove-if-not
   (lambda (event)
     (and (or (null name)
              (string= (json-get event "name") name))
          (or (null category)
              (string= (json-get event "category") category))
          (or (null scope-category)
              (string= (json-get event "scope_category") scope-category))
          (or (null parent-uuid)
              (string= (json-get event "parent_uuid") parent-uuid))))
   events))

(-> nemo-relay-test--assert-scope-pair
    (list &key (:name string) (:category string) (:parent-uuid string))
    null)
(defun nemo-relay-test--assert-scope-pair
    (events &key name category parent-uuid)
  "Assert that EVENTS contain a child scope start and matching end."
  (let* ((start
           (nemo-relay-test--find-event
            events :name name :category category :scope-category "start"))
         (uuid (and start (json-get start "uuid")))
         (end
           (and uuid
                (nemo-relay-test--find-event
                 events
                 :name name
                 :category category
                 :scope-category "end"
                 :uuid uuid))))
    (test-assert start (format nil "Relay emits ~A start" name))
    (test-assert
     (and start (string= (json-get start "parent_uuid") parent-uuid))
     (format nil "Relay ~A start keeps the agent parent" name))
    (test-assert end (format nil "Relay emits ~A end" name))
    (test-assert
     (and end (string= (json-get end "parent_uuid") parent-uuid))
     (format nil "Relay ~A end keeps the agent parent" name)))
  nil)

(-> nemo-relay-test--run-concurrent-tools (string) list)
(defun nemo-relay-test--run-concurrent-tools (propagation-context)
  "Run three tool spans on independent worker threads."
  (let ((results (make-array 3))
        (threads nil))
    (dotimes (index 3)
      (let ((worker-index index))
        (push
         (make-thread
          (lambda ()
            (let ((*nemo-relay-propagation-context-json* propagation-context))
              (setf (aref results worker-index)
                    (with-nemo-relay-tool
                        ((format nil "tool.concurrent.~D" worker-index)
                         (format nil "call-~D" worker-index)
                         (json-object "worker" worker-index))
                      (sleep 0.01)
                      (tool-success (format nil "worker ~D" worker-index))))))
          :name "autolith-relay-test-tool")
         threads)))
    (mapc #'join-thread threads)
    (coerce results 'list)))

(-> test-nemo-relay-tool-failure-metadata () null)
(defun test-nemo-relay-tool-failure-metadata ()
  "Test failed tool results produce OpenTelemetry error metadata."
  (let* ((details (json-object "process.exit.code" 7))
         (result (tool-failure "exit 7" :code ':process-exit :details details))
         (metadata (nemo-relay--tool-result-metadata result)))
    (multiple-value-bind (success present-p)
        (and metadata (gethash "success" metadata))
      (test-assert (and metadata present-p (null success))
                   "failed Relay tool results are marked unsuccessful"))
    (test-assert
     (and metadata
          (string= (json-get metadata "otel.status_code") "ERROR"))
     "failed Relay tool results set the OpenTelemetry error status")
    (test-assert
     (and metadata
          (string= (json-get metadata "error.type") "process-exit")
          (= (json-get metadata "process.exit.code") 7)
          (eq (json-get metadata "details") details))
     "failed Relay tool results retain error details and exit code"))
    (let* ((worker-result
             (worker-response-tool-result
              '(:response :id 2 :status :error
                :message "division by zero")))
           (metadata (nemo-relay--tool-result-metadata worker-result)))
      (multiple-value-bind (success present-p)
          (and metadata (gethash "success" metadata))
        (test-assert
         (and (not (tool-result-success-p worker-result))
              metadata
              present-p
              (null success)
              (string= (json-get metadata "otel.status_code") "ERROR"))
         "failed lisp.eval worker responses become Relay errors")))
  nil)

(-> test-nemo-relay-native-lifecycle () null)
(defun test-nemo-relay-native-lifecycle ()
  "Exercise Relay hierarchy, errors, and concurrent propagation when requested."
  (let* ((library (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))
           (requested (nemo-relay--environment-boolean "AUTOLITH_RELAY_TESTS" nil)))
    (cond
      ((and (non-empty-string-p library) (probe-file library))
         (let* ((root (nemo-relay-test--temporary-root))
                (output-directory (merge-pathnames "events/" root))
                (config-path (merge-pathnames "plugins.toml" root))
                (ambient-config-path
                  (merge-pathnames
                   "nemo-relay/plugins.toml"
                   (merge-pathnames "config/" root)))
                (saved-configuration *nemo-relay-configuration*)
                (saved-error *nemo-relay-last-error*)
                (saved-xdg-config-home (uiop:getenv "XDG_CONFIG_HOME")))
          (unwind-protect
               (progn
                 (sb-posix:setenv
                  "XDG_CONFIG_HOME"
                  (namestring (merge-pathnames "config/" root))
                  1)
                 (uiop:ensure-all-directories-exist
                  (list (uiop:pathname-directory-pathname ambient-config-path)))
                 (with-open-file (stream ambient-config-path
                                         :direction ':output
                                         :if-exists ':supersede
                                         :if-does-not-exist ':create
                                         :external-format ':utf-8)
                   (write-string "version = [" stream)
                   (terpri stream))
                 (nemo-relay-test--write-config config-path output-directory)
                  (nemo-relay-configure
                  :enabled t
                  :config config-path
                  :library library)
                (test-assert (nemo-relay-start) "Relay native exporter starts")
                (with-nemo-relay-agent
                    ("autolith.test"
                     :input (json-object "text" "hello")
                     :metadata (json-object "kind" "test"))
                  (with-nemo-relay-llm
                      ("provider.request" "gpt-test"
                       (json-object "model" "gpt-test"))
                    (with-nemo-relay-tool
                        ("tool.echo" "call-1" (json-object "value" 42))
                      (test-assert
                       (typep (tool-success "ok") 'tool-result)
                       "Relay test tool returns a normal tool result")))
                  (nemo-relay-mark
                   "autolith.test.mark"
                   :data (json-object "ok" t)))
                (with-nemo-relay-agent
                    ("autolith.concurrent"
                     :input (json-object "text" "concurrent")
                     :metadata (json-object "kind" "concurrency"))
                  (let ((results
                          (nemo-relay-test--run-concurrent-tools
                           (nemo-relay-current-propagation-context))))
                    (test-assert
                     (every (lambda (result) (typep result 'tool-result)) results)
                     "concurrent Relay tool bodies preserve tool results")))
                (let ((signaled-p nil))
                  (handler-case
                      (with-nemo-relay-agent
                          ("autolith.error"
                           :input (json-object "text" "error"))
                        (with-nemo-relay-llm
                            ("provider.error" "gpt-test"
                             (json-object "model" "gpt-test"))
                          (error "expected Relay lifecycle failure")))
                    (serious-condition ()
                      (setf signaled-p t)))
                  (test-assert signaled-p "Relay lifecycle rethrows provider failures"))
                 (test-assert (nemo-relay-flush) "Relay native subscribers flush")
                (let* ((pathname (nemo-relay-output-pathname))
                       (events (nemo-relay-test--read-events pathname))
                       (agent-start
                         (nemo-relay-test--find-event
                          events
                          :name "autolith.test"
                          :category "agent"
                          :scope-category "start"))
                       (agent-uuid (and agent-start (json-get agent-start "uuid"))))
                  (test-assert agent-start "Relay emits the test agent start")
                  (test-assert
                   (and agent-start
                        (nemo-relay-test--find-event
                         events
                         :name "autolith.test"
                         :category "agent"
                         :scope-category "end"
                         :uuid agent-uuid))
                   "Relay closes the test agent scope")
                  (nemo-relay-test--assert-scope-pair
                   events
                   :name "provider.request"
                   :category "llm"
                   :parent-uuid agent-uuid)
                  (nemo-relay-test--assert-scope-pair
                   events
                   :name "tool.echo"
                   :category "tool"
                   :parent-uuid agent-uuid)
                  (let ((mark
                          (nemo-relay-test--find-event
                           events
                           :name "autolith.test.mark")))
                    (test-assert
                     (and mark (string= (json-get mark "parent_uuid") agent-uuid))
                     "Relay marks stay under the test agent"))
                  (let* ((concurrent-agent-start
                           (nemo-relay-test--find-event
                            events
                            :name "autolith.concurrent"
                            :category "agent"
                            :scope-category "start"))
                         (concurrent-agent-uuid
                           (and concurrent-agent-start
                                (json-get concurrent-agent-start "uuid")))
                         (concurrent-starts
                           (nemo-relay-test--find-events
                            events
                            :category "tool"
                            :scope-category "start"))
                         (concurrent-ends
                           (nemo-relay-test--find-events
                            events
                            :category "tool"
                            :scope-category "end"))
                         (concurrent-tool-starts
                           (remove-if-not
                            (lambda (event)
                              (uiop:string-prefix-p
                               "tool.concurrent."
                               (json-get event "name")))
                            concurrent-starts)))
                    (test-assert concurrent-agent-start
                                 "Relay emits the concurrent agent start")
                    (test-assert
                     (= (length concurrent-starts) 4)
                     "Relay emits one ordinary and three concurrent tool starts")
                    (test-assert
                     (= (length concurrent-ends) 4)
                     "Relay emits one ordinary and three concurrent tool ends")
                    (test-assert
                     (= (length concurrent-tool-starts) 3)
                     "Relay emits all three concurrent tool names")
                    (test-assert
                     (every
                      (lambda (event)
                        (and concurrent-agent-uuid
                             (string= (json-get event "parent_uuid")
                                      concurrent-agent-uuid)))
                      concurrent-tool-starts)
                     "concurrent Relay tools share their agent parent")))
                (let* ((error-starts
                         (nemo-relay-test--find-events
                          (nemo-relay-test--read-events (nemo-relay-output-pathname))
                          :name "autolith.error"
                          :category "agent"
                          :scope-category "start"))
                       (error-events (nemo-relay-test--read-events (nemo-relay-output-pathname)))
                       (error-start (first error-starts))
                       (error-uuid (and error-start (json-get error-start "uuid")))
                       (error-end
                         (and error-uuid
                              (nemo-relay-test--find-event
                               error-events
                               :name "autolith.error"
                               :category "agent"
                               :scope-category "end"
                               :uuid error-uuid))))
                  (let ((metadata (and error-end (json-get error-end "metadata"))))
                    (multiple-value-bind (success present-p)
                        (and metadata (gethash "success" metadata))
                      (test-assert
                         (and metadata present-p (null success))
                       "Relay marks the failed agent scope unsuccessful")))))
           (nemo-relay-shutdown)
           (setf *nemo-relay-configuration* saved-configuration
                 *nemo-relay-last-error* saved-error)
             (if saved-xdg-config-home
                 (sb-posix:setenv "XDG_CONFIG_HOME" saved-xdg-config-home 1)
                 (sb-posix:unsetenv "XDG_CONFIG_HOME"))
           (uiop:delete-directory-tree
            root :validate t :if-does-not-exist ':ignore))))
      (requested
       (test-assert
        nil
        "AUTOLITH_RELAY_TESTS requires a usable AUTOLITH_RELAY_LIBRARY"))
      (t
       (test-assert
        t
        "native Relay tests are disabled without AUTOLITH_RELAY_TESTS"))))
  nil)
