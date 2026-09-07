(in-package #:autolith)

;;;; -- Relay Tests --

(-> nemo-relay-test--temporary-root () pathname)
(defun nemo-relay-test--temporary-root ()
  "Create one isolated temporary directory for Relay tests."
  (let ((root
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "autolith-relay-tests-~A/" (make-identifier))
            (uiop:temporary-directory)))))
    (uiop:ensure-all-directories-exist (list root))
    (uiop:ensure-directory-pathname (truename root))))

(-> nemo-relay-test--write-config (pathname pathname) null)
(defun nemo-relay-test--write-config (pathname output-directory)
  "Write a configuration-driven ATOF Relay PluginConfig to PATHNAME."
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

(-> nemo-relay-test--write-exporter-json (pathname pathname) null)
(defun nemo-relay-test--write-exporter-json (pathname output-directory)
  "Write a JSON config containing the ATIF, ATOF, and OTEL sections."
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
          "version" 4
          "enable_full_payloads" false
          "atof"
          (json-object
           "enabled" t
           "sinks"
           (json-array
            (json-object
             "type" "file"
             "output_directory" (namestring output-directory)
             "filename" "events.jsonl"
             "mode" "overwrite")))
          "atif" (json-object "enabled" t)
          "opentelemetry" (json-object "enabled" t))))))
     stream)
    (terpri stream))
  nil)

(-> test-nemo-relay-configuration () null)
(defun test-nemo-relay-configuration ()
  "Test explicit PluginConfig selection, normalization, and isolation policy."
  (let* ((root (nemo-relay-test--temporary-root))
         (config-path (merge-pathnames "plugins.toml" root))
         (json-path (merge-pathnames "plugins.json" root))
         (output-directory (merge-pathnames "events/" root))
         (saved-configuration *nemo-relay-configuration*)
         (saved-environment
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   '("AUTOLITH_RELAY"
                     "AUTOLITH_RELAY_CONFIG"
                     "AUTOLITH_RELAY_LIBRARY")))
         (application-configuration (test-configuration))
         (application-root (test-configuration-root application-configuration)))
    (unwind-protect
         (progn
           (dolist (name (mapcar #'first saved-environment))
             (sb-posix:unsetenv name))
           (setf *nemo-relay-configuration* nil)
           (let ((settings (nemo-relay--runtime-configuration nil)))
             (test-assert
              (null (nemo-relay-configuration-config-path settings))
              "Relay does not invent a configuration pathname")
             (test-assert
              (handler-case
                  (progn
                    (nemo-relay--configuration-plugin-config-json settings)
                    nil)
                (nemo-relay-error () t))
              "Relay requires an explicit PluginConfig"))
             (test-assert
              (typep
               (nemo-relay--runtime-configuration application-configuration)
               'nemo-relay-configuration)
              "Relay ignores the Autolith application configuration argument")
           (nemo-relay-test--write-config config-path output-directory)
           (nemo-relay-test--write-exporter-json json-path output-directory)
           (let* ((settings
                    (nemo-relay-configuration-create
                     :enabled-p t
                     :config config-path
                     :library-path "/tmp/libnemo_relay_ffi.dylib"))
                  (config
                    (json-decode
                     (nemo-relay--configuration-plugin-config-json settings)))
                  (components (json-get config "components"))
                  (component (aref components 0))
                  (component-config (json-get component "config")))
             (test-assert (nemo-relay-configuration-enabled-p settings)
                          "Relay accepts explicit enabled settings")
             (test-assert
              (equal (nemo-relay-configuration-config-path settings) config-path)
              "Relay retains the explicit TOML pathname")
             (test-assert
              (string= (nemo-relay-configuration-library-path settings)
                       "/tmp/libnemo_relay_ffi.dylib")
              "Relay retains the explicit library pathname")
             (test-assert
              (and (vectorp components)
                   (= (length components) 1)
                   (string= (json-get component "kind") "observability")
                   (json-get component "enabled"))
              "Relay normalizes TOML into one observability component")
             (test-assert
              (and (= (json-get component-config "version") 4)
                   (null (json-get component-config "enable_full_payloads"))
                   (json-object-p (json-get component-config "atof")))
              "Relay preserves the TOML component configuration"))
           (let* ((settings
                    (nemo-relay-configuration-create
                     :enabled-p t
                     :config json-path))
                  (config
                    (json-decode
                     (nemo-relay--configuration-plugin-config-json settings)))
                  (components (json-get config "components"))
                  (component (aref components 0))
                  (observability (json-get component "config")))
             (test-assert
              (and (vectorp components) (= (length components) 1))
              "Relay reads one JSON observability component")
             (test-assert
              (and (json-object-p (json-get observability "atof"))
                   (json-object-p (json-get observability "atif"))
                   (json-object-p (json-get observability "opentelemetry")))
              "Relay retains configuration-driven ATIF, ATOF, and OTEL sections")))
      (setf *nemo-relay-configuration* saved-configuration)
      (dolist (entry saved-environment)
        (if (rest entry)
            (sb-posix:setenv (first entry) (rest entry) 1)
            (sb-posix:unsetenv (first entry))))
       (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)
       (uiop:delete-directory-tree application-root
                                   :validate t
                                   :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-toml-dynamic-plugins () null)
(defun test-nemo-relay-toml-dynamic-plugins ()
  "Test manifest-backed dynamic plugins and explicit approval."
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
                  (spec (aref specs 0))
                  (config (json-get spec "config")))
             (test-assert
              (and (vectorp specs) (= (length specs) 1))
              "Relay reads one dynamic-plugin declaration")
             (test-assert
              (and (string= (json-get spec "plugin_id") "example.custom_observer")
                   (string= (json-get spec "kind") "rust_dynamic")
                   (string= (json-get spec "manifest_ref")
                            (namestring (truename manifest-path))))
              "Relay resolves and validates a dynamic-plugin manifest")
             (test-assert
              (and (json-object-p config)
                   (string= (json-get config "mode") "audit")
                   (= (json-get (json-get config "executor") "worker_threads") 4))
              "Relay preserves nested dynamic-plugin configuration"))
           (test-assert
            (handler-case
                (progn
                  (nemo-relay--configuration-dynamic-plugins-json
                   (nemo-relay-configuration-create
                    :config config-path
                    :allowed-component-kinds
                    '("example.custom_observer")))
                  nil)
              (nemo-relay-error () t))
            "Relay rejects dynamic plugins without an ID allowlist"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-observability-boundary () null)
(defun test-nemo-relay-observability-boundary ()
  "Test static and dynamic allowlists exclude non-observability built-ins."
  (dolist (kind '("nemo_guardrails" "pricing"))
    (test-assert
     (handler-case
         (progn
           (nemo-relay-configuration-create
            :enabled-p t
            :plugin-config
            (json-object
             "version" 1
             "components"
             (json-array (json-object "kind" kind "enabled" t)))
            :allowed-component-kinds (list kind))
           nil)
       (nemo-relay-error () t))
     (format nil "Relay rejects non-observability built-in ~A" kind)))
  (dolist (kind '("observability" "nemo_guardrails" "pricing"))
    (test-assert
     (handler-case
         (progn
           (nemo-relay-configuration-create
            :enabled-p t
            :allowed-dynamic-plugin-ids (list kind))
           nil)
       (nemo-relay-error () t))
     (format nil "Relay rejects reserved dynamic plugin ID ~A" kind)))
  (test-assert
   (handler-case
       (progn
         (nemo-relay-configuration-create
          :enabled-p t
          :plugin-config
          (json-object
           "version" 1
           "components"
           (json-array
            (json-object "kind" "example.custom_observer" "enabled" t))))
         nil)
     (nemo-relay-error () t))
   "Relay rejects an unallowlisted custom component")
  (test-assert
   (typep
    (nemo-relay-configuration-create
     :enabled-p t
     :plugin-config
     (json-object
      "version" 1
      "components"
      (json-array
       (json-object "kind" "example.custom_observer" "enabled" t)))
     :allowed-component-kinds '("example.custom_observer"))
    'nemo-relay-configuration)
   "Relay accepts an explicitly allowlisted custom component")
  nil)

(-> test-nemo-relay-dynamic-manifest-metadata () null)
(defun test-nemo-relay-dynamic-manifest-metadata ()
  "Test that Autolith extracts only dynamic-plugin policy identity."
  (let* ((root (nemo-relay-test--temporary-root))
         (manifest-path (merge-pathnames "relay-plugin.toml" root)))
    (uiop:ensure-all-directories-exist
     (list (uiop:pathname-directory-pathname manifest-path)))
    (labels ((write-plugin (plugin-id kind)
               "Write the identity section of a test plugin manifest."
               (with-open-file (stream manifest-path
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create
                                       :external-format ':utf-8)
                 (format stream
                         "[plugin]~%id = ~S~%kind = ~S~%"
                         plugin-id kind))))
      (unwind-protect
           (progn
             (write-plugin "example.custom_observer" "rust_dynamic")
             (multiple-value-bind (plugin-id kind resolved-path)
                 (nemo-relay--dynamic-plugin-manifest-metadata manifest-path)
               (test-assert
                (and (string= plugin-id "example.custom_observer")
                     (string= kind "rust_dynamic")
                     (equal resolved-path (truename manifest-path)))
                "Relay extracts dynamic-plugin identity without requiring upstream fields"))
             (write-plugin "example.custom_observer" "unsupported")
             (test-assert
              (handler-case
                  (progn
                    (nemo-relay--dynamic-plugin-manifest-metadata manifest-path)
                    nil)
                (nemo-relay-error () t))
              "Relay rejects unsupported dynamic-plugin kinds")
             (with-open-file (stream manifest-path
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create
                                     :external-format ':utf-8)
               (write-string "[plugin]~%kind = \"rust_dynamic\"~%" stream))
             (test-assert
              (handler-case
                  (progn
                    (nemo-relay--dynamic-plugin-manifest-metadata manifest-path)
                    nil)
                (nemo-relay-error () t))
              "Relay rejects manifests without a plugin ID"))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-nemo-relay-home-implicit-discovery () null)
(defun test-nemo-relay-home-implicit-discovery ()
  "Test HOME-based implicit plugin discovery is rejected."
  (let* ((root (nemo-relay-test--temporary-root))
         (home (merge-pathnames "home/" root))
         (discovered-path
           (merge-pathnames ".config/nemo-relay/plugins.toml" home))
         (saved-environment
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   '("HOME" "USERPROFILE" "XDG_CONFIG_HOME"))))
    (uiop:ensure-all-directories-exist
     (list (uiop:pathname-directory-pathname discovered-path)))
    (with-open-file (stream discovered-path
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (write-string "{}" stream))
    (unwind-protect
         (progn
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
              "Relay reports the discovered pathname")))
      (dolist (entry saved-environment)
        (tests--restore-environment (first entry) (rest entry)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-exporter-configuration () null)
(defun test-nemo-relay-exporter-configuration ()
  "Test that exporter configuration stays native and configuration-driven."
  (let* ((root (nemo-relay-test--temporary-root))
         (config-path (merge-pathnames "exporters.json" root))
         (output-directory (merge-pathnames "events/" root)))
    (unwind-protect
         (progn
           (nemo-relay-test--write-exporter-json config-path output-directory)
           (let* ((settings (nemo-relay-configuration-create
                             :enabled-p t :config config-path))
                  (source (nemo-relay--configuration-plugin-config-json settings))
                  (root-object (json-decode source))
                  (component (aref (json-get root-object "components") 0))
                  (config (json-get component "config"))
                  (output
                    (nemo-relay--configuration-output-pathname
                     source config-path root)))
             (test-assert
              (and (json-object-p (json-get config "atif"))
                   (json-object-p (json-get config "atof"))
                   (json-object-p (json-get config "opentelemetry")))
              "ATIF, ATOF, and OTEL remain PluginConfig sections")
             (test-assert
              (and output
                   (string= (namestring output)
                            (namestring (merge-pathnames
                                         "events/events.jsonl" root))))
              "ATOF output paths remain derived from PluginConfig")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-adapter-surface () null)
(defun test-nemo-relay-adapter-surface ()
  "Test that the Relay binding exposes only the lifecycle adapter surface."
  (dolist (name '(nemo-relay-metric
                  nemo-relay-metric-json
                  nemo-relay-event-v2
                  nemo-relay-get-handle
                  nemo-relay-register-subscriber
                  nemo-relay-register-plugin
                  nemo-relay-atif-exporter-create
                  nemo-relay-atof-exporter-create
                  nemo-relay-otel-subscriber-create))
    (test-assert
     (not (fboundp name))
     (format nil "Relay SDK entrypoint ~A is removed" name)))
  (dolist (class '(nemo-relay-metric-measurement
                   nemo-relay-event-view
                   nemo-relay-atif-exporter
                   nemo-relay-atof-exporter
                   nemo-relay-otel-subscriber))
    (test-assert
     (null (find-class class nil))
     (format nil "Relay SDK class ~A is removed" class)))
  (test-assert (fboundp 'nemo-relay-push-scope)
               "Relay Agent scope adapter remains available")
  (test-assert (fboundp 'nemo-relay-tool-call)
               "Relay Tool lifecycle adapter remains available")
  (test-assert (fboundp 'nemo-relay-llm-call)
               "Relay LLM lifecycle adapter remains available")
  (test-assert (fboundp 'nemo-relay-scope-stack-create)
               "Relay scope-stack propagation remains available")
  nil)

(-> test-nemo-relay-disabled-wrappers () null)
(defun test-nemo-relay-disabled-wrappers ()
  "Test disabled wrappers preserve values and avoid payload evaluation."
  (let ((*nemo-relay-runtime* nil)
        (*nemo-relay-instrumentation-suppressed-p* nil)
        (evaluated-p nil))
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-agent
            ((progn (setf evaluated-p t) "disabled-agent")
             :input (progn (setf evaluated-p t) (json-object "input" t))
             :metadata (progn (setf evaluated-p t) (json-object "metadata" t)))
          (values :agent-one :agent-two))
      (test-assert
       (and (eq first-value ':agent-one) (eq second-value ':agent-two))
       "disabled Agent wrapper preserves multiple values"))
    (test-assert (not evaluated-p)
                 "disabled Agent wrapper does not evaluate payloads")
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-llm
            ((progn (setf evaluated-p t) "disabled-llm")
             (progn (setf evaluated-p t) "model")
             (progn (setf evaluated-p t) (json-object "x" 1)))
          (values :llm-one :llm-two))
      (test-assert
       (and (eq first-value ':llm-one) (eq second-value ':llm-two))
       "disabled LLM wrapper preserves multiple values"))
    (test-assert (not evaluated-p)
                 "disabled LLM wrapper does not evaluate payloads")
    (multiple-value-bind (first-value second-value)
        (with-nemo-relay-tool
            ((progn (setf evaluated-p t) "disabled-tool")
             (progn (setf evaluated-p t) "call")
             (progn (setf evaluated-p t) (json-object "x" 1)))
          (values :tool-one :tool-two))
      (test-assert
       (and (eq first-value ':tool-one) (eq second-value ':tool-two))
       "disabled Tool wrapper preserves multiple values"))
    (test-assert (not evaluated-p)
                 "disabled Tool wrapper does not evaluate payloads")
    (test-assert (null (nemo-relay-flush)) "disabled Relay flush is a no-op")
    (test-assert (null (nemo-relay-mark "disabled.mark"))
                 "disabled Relay marks are a no-op"))
  nil)

(-> test-nemo-relay-unavailable-library () null)
(defun test-nemo-relay-unavailable-library ()
  "Test that a missing Relay library remains non-fatal."
  (let* ((root (nemo-relay-test--temporary-root))
         (library (merge-pathnames "missing/libnemo_relay_ffi.dylib" root))
         (saved-configuration *nemo-relay-configuration*)
         (saved-runtime *nemo-relay-runtime*)
         (saved-error *nemo-relay-last-error*)
         (saved-library *nemo-relay-native-library*))
    (unwind-protect
         (progn
           (setf *nemo-relay-configuration* nil
                 *nemo-relay-runtime* nil
                 *nemo-relay-last-error* nil
                 *nemo-relay-native-library* nil)
           (nemo-relay-configure
            :enabled t
            :config-json "{\"version\":1,\"components\":[]}"
            :library (namestring library))
           (test-assert (not (nemo-relay-start))
                        "Relay startup failure does not affect the caller")
           (test-assert
            (and (null *nemo-relay-runtime*)
                 (non-empty-string-p (nemo-relay-last-error)))
            "Relay startup retains an actionable diagnostic"))
      (nemo-relay-shutdown)
      (setf *nemo-relay-configuration* saved-configuration
            *nemo-relay-runtime* saved-runtime
            *nemo-relay-last-error* saved-error
            *nemo-relay-native-library* saved-library)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nemo-relay-configured-library-selection () null)
(defun test-nemo-relay-configured-library-selection ()
  "Test that direct Relay calls use the configured library pathname."
  (let* ((library "/custom/path/libnemo_relay_ffi.dylib")
         (saved-environment (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))
         (selected nil))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_RELAY_LIBRARY")
           (let ((*nemo-relay-configuration*
                   (nemo-relay-configuration-create
                    :enabled-p t
                    :library-path library))
                 (*nemo-relay-runtime* nil)
                 (*nemo-relay-native-library* nil))
             (test-call-with-function-replacements
              (list
               (list 'nemo-relay--load-library
                     (lambda (path)
                       (setf selected path
                             *nemo-relay-native-library* ':fake)
                       ':fake)))
              (lambda ()
                (nemo-relay--ensure-native-library)))
             (test-assert (string= selected library)
                          "Relay direct calls select the configured library")
             (test-assert (eq *nemo-relay-native-library* ':fake)
                          "Relay direct calls retain the selected library")))
      (tests--restore-environment "AUTOLITH_RELAY_LIBRARY"
                                  saved-environment)))
  nil)

(-> test-nemo-relay-checkpoint-detach () null)
(defun test-nemo-relay-checkpoint-detach ()
  "Test that checkpoint preparation closes all process-local Relay state."
  (let ((closed nil)
        (*nemo-relay-runtime* nil)
        (*nemo-relay-native-library* ':fake)
        (*nemo-relay-last-error* "stale")
        (*nemo-relay-propagation-context-json* "{}")
        (*nemo-relay-instrumentation-suppressed-p* t))
    (test-call-with-function-replacements
     (list
      (list 'cffi:close-foreign-library
            (lambda (library)
              (setf closed library)
              t)))
     (lambda ()
       (nemo-relay--detach-for-checkpoint)
       (test-assert (eq closed ':fake)
                    "checkpoint preparation closes the Relay library")
       (test-assert (and (null *nemo-relay-native-library*)
                         (null *nemo-relay-runtime*)
                         (null *nemo-relay-last-error*)
                         (null *nemo-relay-propagation-context-json*)
                         (null *nemo-relay-instrumentation-suppressed-p*))
                    "checkpoint preparation clears process-local Relay state")))
  nil))

(-> nemo-relay-test--record (t t t) null)
(defun nemo-relay-test--record (lock events entry)
  "Append ENTRY to the mutable EVENTS box under LOCK."
  (with-lock-held (lock)
    (push entry (first events)))
  nil)

(-> test-nemo-relay-context-propagation () null)
(defun test-nemo-relay-context-propagation ()
  "Test lifecycle nesting and concurrent scope-stack propagation with mocks."
  (let ((*nemo-relay-runtime* :mock)
        (*nemo-relay-instrumentation-suppressed-p* nil)
        (*nemo-relay-propagation-context-json* nil)
        (*nemo-relay-last-error* nil)
        (events (list nil))
        (lock (make-lock "Relay test events")))
    (test-call-with-function-replacements
     (list
      (list 'nemo-relay-scope-stack-create
            (lambda ()
              (nemo-relay-test--record lock events '(:stack-created))
              :stack))
      (list 'nemo-relay-scope-stack-create-from-propagation-context
            (lambda (context)
              (nemo-relay-test--record lock events (list :stack-from context))
              :stack))
      (list 'nemo-relay-scope-stack-capture-thread
            (lambda ()
              (nemo-relay-test--record lock events '(:thread-captured))
              :binding))
      (list 'nemo-relay-scope-stack-set-thread
            (lambda (stack)
              (nemo-relay-test--record lock events (list :thread-set stack))
              t))
      (list 'nemo-relay-scope-stack-restore-thread
            (lambda (binding)
              (nemo-relay-test--record lock events (list :thread-restored binding))
              t))
      (list 'nemo-relay-scope-stack-free
            (lambda (stack)
              (nemo-relay-test--record lock events (list :stack-freed stack))
              t))
      (list 'nemo-relay-push-scope
            (lambda (&key name scope-type &allow-other-keys)
              (nemo-relay-test--record lock events
                                        (list :push name scope-type))
              (list :scope name)))
      (list 'nemo-relay-pop-scope
            (lambda (&key handle &allow-other-keys)
              (nemo-relay-test--record lock events (list :pop handle))
              t))
      (list 'nemo-relay-handle-free
            (lambda (handle)
              (nemo-relay-test--record lock events (list :handle-freed handle))
              t))
      (list 'nemo-relay-capture-propagation-context
            (lambda () "{\"trace\":\"mock\"}"))
      (list 'nemo-relay-mark
            (lambda (name &rest arguments)
              (declare (ignore arguments))
              (nemo-relay-test--record lock events (list :mark name))
              nil))
      (list 'nemo-relay-llm-call
            (lambda (&key name &allow-other-keys)
              (nemo-relay-test--record lock events (list :llm name))
              :llm-handle))
      (list 'nemo-relay-llm-call-end
            (lambda (&key handle &allow-other-keys)
              (nemo-relay-test--record lock events (list :llm-end handle))
              t))
      (list 'nemo-relay-tool-call
            (lambda (&key name &allow-other-keys)
              (nemo-relay-test--record lock events (list :tool name))
              :tool-handle))
      (list 'nemo-relay-tool-call-end
            (lambda (&key handle &allow-other-keys)
              (nemo-relay-test--record lock events (list :tool-end handle))
              t)))
     (lambda ()
       (multiple-value-bind (first-value second-value)
           (nemo-relay--call-with-agent
            :name "agent"
            :input (json-object "text" "hello")
            :metadata
            (json-object "child" t "agent_name" "child" "parent_agent" "parent")
            :function
            (lambda ()
              (nemo-relay--call-with-llm
               :name "provider"
               :model "model"
               :request (json-object "model" "model")
               :function
               (lambda ()
                 (nemo-relay--call-with-tool
                  :name "tool"
                  :call-id "call"
                  :arguments (json-object "value" 1)
                  :function (lambda () (values :one :two)))))))
         (test-assert (and (eq first-value ':one) (eq second-value ':two))
                      "nested Relay lifecycles preserve multiple values"))
       (let ((threads
               (loop repeat 3
                     collect
                     (make-thread
                        (let ((context (or (nemo-relay-current-propagation-context)
                                           "{\"trace\":\"mock\"}")))
                         (lambda ()
                           (let ((*nemo-relay-runtime* :mock)
                                 (*nemo-relay-instrumentation-suppressed-p* nil)
                                 (*nemo-relay-propagation-context-json* context))
                             (nemo-relay--call-with-scope-stack
                              context
                               (lambda () :child)))))))))
         (mapc #'join-thread threads)
         (test-assert
          (>= (count :stack-from (first events) :key #'first) 4)
          "concurrent child executions create isolated propagated stacks"))
       (test-assert
        (and (find :push (first events) :key #'first)
             (find :llm (first events) :key #'first)
             (find :tool (first events) :key #'first)
             (find :llm-end (first events) :key #'first)
             (find :tool-end (first events) :key #'first)
             (find :pop (first events) :key #'first))
        "Agent, LLM, and Tool lifecycle calls remain nested")))
  nil))

(-> test-nemo-relay-error-isolation () null)
(defun test-nemo-relay-error-isolation ()
  "Test that Relay setup and lifecycle failures do not escape core execution."
  (let ((*nemo-relay-runtime* :mock)
        (*nemo-relay-instrumentation-suppressed-p* nil)
        (*nemo-relay-last-error* nil))
    (test-call-with-function-replacements
     (list
      (list 'nemo-relay-scope-stack-create
            (lambda () (error "scope setup failed")))
      (list 'nemo-relay-llm-call
            (lambda (&key &allow-other-keys)
              (error "LLM setup failed")))
      (list 'nemo-relay-mark
            (lambda (&rest arguments)
              (declare (ignore arguments))
              nil)))
     (lambda ()
       (multiple-value-bind (first-value second-value)
           (nemo-relay--call-with-scope-stack
            "{\"trace\":\"failed\"}"
            (lambda () (values :body-one :body-two)))
         (test-assert
          (and (eq first-value ':body-one) (eq second-value ':body-two))
          "scope-stack setup failure preserves the caller's values"))
       (multiple-value-bind (first-value second-value)
           (nemo-relay--call-with-llm
            :name "provider"
            :model "model"
            :request (json-object "model" "model")
            :function (lambda () (values :llm-one :llm-two)))
         (test-assert
          (and (eq first-value ':llm-one) (eq second-value ':llm-two))
          "LLM setup failure preserves the caller's values"))
       (test-assert (non-empty-string-p (nemo-relay-last-error))
                    "Relay setup failures retain a diagnostic")))
  nil))

(-> nemo-relay-test--read-text (pathname) string)
(defun nemo-relay-test--read-text (pathname)
  "Read PATHNAME as UTF-8 text."
  (with-open-file (stream pathname :direction ':input :external-format ':utf-8)
    (with-output-to-string (output)
      (loop for line = (read-line stream nil nil)
            while line
            do (write-line line output)))))

(-> test-nemo-relay-native-lifecycle () null)
(defun test-nemo-relay-native-lifecycle ()
  "Exercise configuration-driven native lifecycle behavior when requested."
  (let* ((library (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))
         (requested (nemo-relay--environment-boolean "AUTOLITH_RELAY_TESTS" nil)))
    (cond
      ((and (non-empty-string-p library) (probe-file library))
       (let* ((root (nemo-relay-test--temporary-root))
              (output-directory (merge-pathnames "events/" root))
              (config-path (merge-pathnames "plugins.toml" root))
              (config-home (merge-pathnames "config/" root))
              (saved-configuration *nemo-relay-configuration*)
              (saved-error *nemo-relay-last-error*)
              (saved-environment
                (mapcar (lambda (name) (cons name (uiop:getenv name)))
                        '("HOME"
                          "USERPROFILE"
                          "XDG_CONFIG_HOME"
                          "AUTOLITH_RELAY_LIBRARY"))))
         (uiop:ensure-all-directories-exist (list config-home))
         (unwind-protect
              (progn
                (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config-home) 1)
                (sb-posix:unsetenv "HOME")
                (sb-posix:unsetenv "USERPROFILE")
                (sb-posix:unsetenv "AUTOLITH_RELAY_LIBRARY")
                (nemo-relay-test--write-config config-path output-directory)
                (nemo-relay-configure
                 :enabled t
                 :config config-path
                 :library library)
                (test-assert (nemo-relay-start)
                             "Relay starts from an explicit PluginConfig")
                (with-nemo-relay-agent
                    ("autolith.test"
                     :input (json-object "text" "hello")
                     :metadata (json-object "kind" "test"))
                  (with-nemo-relay-llm
                      ("provider.request" "test-model"
                       (json-object "model" "test-model"))
                    (with-nemo-relay-tool
                        ("tool.echo" "call-1" (json-object "value" 42))
                      (test-assert (typep (tool-success "ok") 'tool-result)
                                   "Relay lifecycle preserves tool results")))
                  (nemo-relay-mark "autolith.test.mark"
                                   :data (json-object "ok" t)))
                (test-assert (nemo-relay-flush)
                             "Relay flush completes configuration-driven exporters")
                (let ((output (nemo-relay-output-pathname)))
                  (test-assert (and output (probe-file output))
                               "Relay exposes the configured ATOF output")
                  (test-assert (search "autolith.test.mark"
                                       (nemo-relay-test--read-text output))
                               "ATOF receives lifecycle marks"))
                (nemo-relay--detach-for-checkpoint)
                (test-assert (null *nemo-relay-runtime*)
                             "checkpoint preparation detaches Relay runtime")
                (test-assert (nemo-relay-start)
                             "Relay restarts after checkpoint detachment"))
           (nemo-relay-shutdown)
           (setf *nemo-relay-configuration* saved-configuration
                 *nemo-relay-last-error* saved-error)
           (dolist (entry saved-environment)
             (tests--restore-environment (first entry) (rest entry)))
           (uiop:delete-directory-tree root
                                       :validate t
                                       :if-does-not-exist ':ignore))))
      (requested
       (test-assert nil
                    "AUTOLITH_RELAY_TESTS requires a usable Relay library"))
      (t
       (test-assert t
                    "native Relay tests are disabled without AUTOLITH_RELAY_TESTS")))
  nil))
