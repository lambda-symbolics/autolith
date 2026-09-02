(in-package #:autolith)

(-> nemo-relay--string-list-p (t) boolean)
(defun nemo-relay--string-list-p (value)
  "Return true when VALUE is a proper list of unique non-empty strings."
  (handler-case
      (and (listp value)
           (every #'non-empty-string-p value)
           (= (length value)
              (length (remove-duplicates value :test #'string=))))
    (type-error ()
      nil)))

;;;; -- Plugin Configuration --

(defclass nemo-relay-configuration ()
  ((enabled-p
    :initarg :enabled-p
    :reader nemo-relay-configuration-enabled-p
    :type boolean
    :documentation "Whether Relay instrumentation is enabled.")
   (config-path
    :initarg :config-path
    :initform nil
    :reader nemo-relay-configuration-config-path
    :type (option pathname)
    :documentation "The TOML or JSON PluginConfig pathname, when configured.")
   (plugin-config-json
    :initarg :plugin-config-json
    :initform nil
    :reader nemo-relay-configuration-plugin-config-json
    :type (option string)
    :documentation "The Relay PluginConfig document as JSON text.")
   (dynamic-plugins-json
    :initarg :dynamic-plugins-json
    :initform nil
    :reader nemo-relay-configuration-dynamic-plugins-json
    :type (option string)
    :documentation "The Relay dynamic-plugin specification array as JSON text.")
   (allowed-component-kinds
    :initarg :allowed-component-kinds
    :initform nil
    :reader nemo-relay-configuration-allowed-component-kinds
    :type (satisfies nemo-relay--string-list-p)
    :documentation "Custom static Relay component kinds explicitly allowed for observability.")
   (allowed-dynamic-plugin-ids
    :initarg :allowed-dynamic-plugin-ids
    :initform nil
    :reader nemo-relay-configuration-allowed-dynamic-plugin-ids
    :type (satisfies nemo-relay--string-list-p)
    :documentation "Dynamic Relay plugin IDs explicitly allowed for observability.")
   (library-path
    :initarg :library-path
    :initform nil
    :reader nemo-relay-configuration-library-path
    :type (option string)
    :documentation "The optional path to libnemo_relay_ffi."))
  (:documentation
   "Autolith process settings surrounding Relay's native PluginConfig."))

(defclass nemo-relay-runtime ()
  ((configuration
    :initarg :configuration
    :reader nemo-relay-runtime-configuration
    :type nemo-relay-configuration
    :documentation "The settings used to start this Relay runtime.")
   (library
    :initarg :library
    :reader nemo-relay-runtime-library
    :documentation "The loaded CFFI foreign library handle.")
   (activation
    :initarg :activation
    :initform nil
    :reader nemo-relay-runtime-activation
    :documentation "The owned dynamic-plugin activation, when one was used.")
   (plugin-config-json
    :initarg :plugin-config-json
    :reader nemo-relay-runtime-plugin-config-json
    :type string
    :documentation "The exact PluginConfig document used for initialization.")
   (report
    :initarg :report
    :initform nil
    :reader nemo-relay-runtime-report
    :documentation "The native plugin initialization report.")
   (output-pathname
    :initarg :output-pathname
    :initform nil
    :reader nemo-relay-runtime-output-pathname
    :type (option pathname)
    :documentation "The first configured local ATOF file path, when known.")
   (full-payloads-enabled-p
    :initarg :full-payloads-enabled-p
    :initform nil
    :reader nemo-relay-runtime-full-payloads-enabled-p
    :type boolean
    :documentation "Whether the Relay observability config opts into full payloads."))
  (:documentation "Native Relay state owned by one Autolith process."))

(-> nemo-relay--environment-boolean (string boolean) boolean)
(defun nemo-relay--environment-boolean (name default)
  "Read a boolean environment variable named NAME or return DEFAULT."
  (let ((value (uiop:getenv name)))
    (if (non-empty-string-p value)
          (not (null
                (member (string-downcase value) '("1" "on" "true" "yes")
                        :test #'string=)))
        default)))

(-> nemo-relay--path-value ((or pathname string)) pathname)
(defun nemo-relay--path-value (value)
  "Convert VALUE to a pathname while expanding a leading home shortcut."
  (let ((text (and (stringp value) value)))
    (cond
      ((and text
            (or (string= text "~")
                (uiop:string-prefix-p "~/" text)))
       (merge-pathnames
        (if (string= text "~") "" (subseq text 2))
        (user-homedir-pathname)))
      ((pathnamep value)
       value)
      (t
       (pathname value)))))

(-> nemo-relay--absolute-pathname ((or pathname string) &optional pathname)
    pathname)
(defun nemo-relay--absolute-pathname (value &optional base)
  "Return VALUE as an absolute pathname relative to BASE or the cwd."
  (let ((path (nemo-relay--path-value value)))
    (if (uiop:absolute-pathname-p path)
        path
        (merge-pathnames
         path
         (uiop:ensure-directory-pathname
          (or base (uiop:getcwd)))))))

(-> nemo-relay--default-data-root () pathname)
(defun nemo-relay--default-data-root ()
  "Return Autolith's conventional data root for standalone Relay use."
  (merge-pathnames
   "autolith/"
   (let ((home (user-homedir-pathname)))
     (nemo-relay--path-value
      (or (uiop:getenv "XDG_DATA_HOME")
          (merge-pathnames ".local/share/" home))))))


(-> nemo-relay--read-file-text (pathname) string)
(defun nemo-relay--read-file-text (pathname)
  "Read UTF-8 text from PATHNAME."
  (with-open-file (stream pathname
                          :direction ':input
                          :external-format ':utf-8)
    (with-output-to-string (output)
      (loop for line = (read-line stream nil nil)
            while line
            do (write-line line output)))))

(-> nemo-relay--normalize-json-document (string string) string)
(defun nemo-relay--normalize-json-document (source operation)
  "Validate SOURCE as one JSON value and return its compact representation."
  (handler-case
      (let ((yason:*parse-json-booleans-as-symbols* t)
            (yason:true t))
        (let ((value (json-decode source)))
          (json-encode value)))
    (serious-condition (condition)
      (error 'nemo-relay-error
             :message (format nil "~A is not valid JSON: ~A"
                              operation
                              (nemo-relay--condition-summary condition))
             :operation operation
             :cause condition))))

(-> nemo-relay--normalize-plugin-config (t) string)
(defun nemo-relay--normalize-plugin-config (value)
  "Validate and serialize one Relay PluginConfig value."
  (let* ((source (if (stringp value) value (json-encode value)))
         (normalized (nemo-relay--normalize-json-document source "Relay PluginConfig")))
    (unless (json-object-p (json-decode normalized))
      (error 'nemo-relay-error
             :message "Relay PluginConfig must be a JSON object."
             :operation "Relay PluginConfig"))
    normalized))

(-> nemo-relay--normalize-dynamic-plugins (t) string)
(defun nemo-relay--normalize-dynamic-plugins (value)
  "Validate and serialize Relay's dynamic-plugin specification array."
  (let* ((source (if (stringp value) value (json-encode value)))
         (normalized
           (nemo-relay--normalize-json-document source
                                           "Relay dynamic-plugin specifications"))
         (decoded (json-decode normalized)))
    (unless (or (vectorp decoded) (listp decoded))
      (error 'nemo-relay-error
             :message "Relay dynamic-plugin specifications must be a JSON array."
             :operation "Relay dynamic-plugin specifications"))
    normalized))

(-> nemo-relay--normalize-string-list (t string) list)
(defun nemo-relay--normalize-string-list (value field)
  "Validate and copy VALUE as a unique non-empty string list for FIELD."
  (unless (nemo-relay--string-list-p value)
    (error 'nemo-relay-error
           :message (format nil "~A must be a proper list of unique non-empty strings."
                            field)
           :operation "Relay configuration"))
  (copy-list value))

(-> nemo-relay--normalize-custom-observability-kinds (list) list)
(defun nemo-relay--normalize-custom-observability-kinds (value)
  "Validate custom component approvals without admitting pinned built-ins."
  (let ((kinds (nemo-relay--normalize-string-list
                value "Relay allowed component kinds")))
    (dolist (kind kinds)
      (when (and (nemo-relay--reserved-plugin-kind-p kind)
                 (not (string= kind *nemo-relay-observability-plugin-kind*)))
        (error 'nemo-relay-error
               :message
               (format nil
                       "Relay component kind ~A is a reserved non-observability built-in and cannot be allowlisted."
                       kind)
               :operation "Relay component allowlist")))
    kinds))

(-> nemo-relay--normalize-custom-observability-plugin-ids (list) list)
(defun nemo-relay--normalize-custom-observability-plugin-ids (value)
  "Validate dynamic plugin approvals without admitting pinned built-ins."
  (let ((plugin-ids (nemo-relay--normalize-string-list
                     value "Relay allowed dynamic plugin IDs")))
    (dolist (plugin-id plugin-ids)
      (when (nemo-relay--reserved-plugin-kind-p plugin-id)
        (error 'nemo-relay-error
               :message
               (format nil
                       "Relay dynamic plugin ID ~A names a reserved built-in and cannot be allowlisted."
                       plugin-id)
               :operation "Relay dynamic-plugin allowlist")))
    plugin-ids))

(-> nemo-relay--json-array-values (t string) list)
(defun nemo-relay--json-array-values (value field)
  "Return JSON array VALUE as a list or signal a FIELD configuration error."
  (cond
    ((vectorp value)
     (coerce value 'list))
    ((null value)
     nil)
    ((listp value)
     value)
    (t
     (error 'nemo-relay-error
            :message (format nil "~A must be a JSON array." field)
            :operation field))))

(-> nemo-relay--validate-observability-plugin-config (json-object list) null)
(defun nemo-relay--validate-observability-plugin-config
    (root allowed-component-kinds)
  "Reject static Relay components outside the observability allowlist."
  (multiple-value-bind (components components-present-p)
      (gethash "components" root)
    (when components-present-p
      (loop for component in
              (nemo-relay--json-array-values components "Relay PluginConfig components")
            for index from 0
            do (unless (json-object-p component)
                 (error 'nemo-relay-error
                        :message
                        (format nil
                                "Relay PluginConfig component ~D must be an object."
                                index)
                        :operation "Relay PluginConfig component allowlist"))
               (let ((kind (json-get component "kind")))
                 (unless (and (stringp kind) (plusp (length kind)))
                   (error 'nemo-relay-error
                          :message
                          (format nil
                                  "Relay PluginConfig component ~D requires a non-empty string kind."
                                  index)
                          :operation "Relay PluginConfig component allowlist"))
                   (unless (nemo-relay--custom-observability-kind-authorized-p
                            kind allowed-component-kinds)
                     (error 'nemo-relay-error
                            :message
                            (format nil
                                    "Relay component kind ~A is outside the observability boundary or is not explicitly allowed."
                                    kind)
                            :operation "Relay PluginConfig component allowlist"))))))
  nil)

(-> nemo-relay--validate-dynamic-plugin-config (string list) null)
(defun nemo-relay--validate-dynamic-plugin-config
    (dynamic-plugins-json allowed-dynamic-plugin-ids)
  "Reject unapproved dynamic plugins and malformed manifest metadata."
  (loop for specification in
          (nemo-relay--json-array-values
           (json-decode dynamic-plugins-json)
           "Relay dynamic-plugin specifications")
        for index from 0
        do (unless (json-object-p specification)
             (error 'nemo-relay-error
                    :message
                    (format nil "Relay dynamic plugin ~D must be an object."
                            index)
                    :operation "Relay dynamic-plugin allowlist"))
           (let ((plugin-id (json-get specification "plugin_id"))
                 (kind (json-get specification "kind"))
                 (manifest-ref (json-get specification "manifest_ref")))
             (unless (and (stringp plugin-id) (plusp (length plugin-id)))
               (error 'nemo-relay-error
                      :message
                      (format nil
                              "Relay dynamic plugin ~D requires a non-empty string plugin_id."
                              index)
                      :operation "Relay dynamic-plugin allowlist"))
             (unless (and (stringp kind) (plusp (length kind)))
               (error 'nemo-relay-error
                      :message
                      (format nil
                              "Relay dynamic plugin ~D requires a non-empty string kind."
                              index)
                      :operation "Relay dynamic-plugin allowlist"))
             (unless (member kind '("rust_dynamic" "worker") :test #'string=)
               (error 'nemo-relay-error
                      :message
                      (format nil "Unsupported Relay dynamic plugin kind: ~A" kind)
                      :operation "Relay dynamic-plugin allowlist"))
             (unless (and (stringp manifest-ref) (plusp (length manifest-ref)))
               (error 'nemo-relay-error
                      :message
                      (format nil
                              "Relay dynamic plugin ~D requires a non-empty string manifest_ref."
                              index)
                      :operation "Relay dynamic-plugin allowlist"))
             (when (nemo-relay--reserved-plugin-kind-p plugin-id)
               (error 'nemo-relay-error
                      :message
                      (format nil
                              "Relay dynamic plugin ID ~A names a reserved built-in."
                              plugin-id)
                      :operation "Relay dynamic-plugin allowlist"))
             (unless (member plugin-id allowed-dynamic-plugin-ids :test #'string=)
               (error 'nemo-relay-error
                      :message
                      (format nil
                              "Relay dynamic plugin ID ~A is not explicitly allowed."
                              plugin-id)
                      :operation "Relay dynamic-plugin allowlist"))
             (multiple-value-bind (manifest-id manifest-kind manifest-path)
                 (nemo-relay--dynamic-plugin-manifest-metadata manifest-ref)
               (declare (ignore manifest-path))
               (unless (string= plugin-id manifest-id)
                 (error 'nemo-relay-error
                        :message
                        (format nil
                                "Relay dynamic plugin ID ~A does not match manifest plugin ID ~A."
                                plugin-id manifest-id)
                        :operation "Relay dynamic-plugin manifest"))
               (unless (string= kind manifest-kind)
                 (error 'nemo-relay-error
                        :message
                        (format nil
                                "Relay dynamic plugin kind ~A does not match manifest kind ~A."
                                kind manifest-kind)
                        :operation "Relay dynamic-plugin manifest")))
             (let ((config (json-get specification "config")))
               (when (and config (not (json-object-p config)))
                 (error 'nemo-relay-error
                        :message
                        (format nil "Relay dynamic plugin ~D config must be an object."
                                index)
                        :operation "Relay dynamic-plugin allowlist")))))
  nil)

(-> nemo-relay--normalize-observability-plugin-config (t list) string)
(defun nemo-relay--normalize-observability-plugin-config
    (value allowed-component-kinds)
  "Normalize VALUE after enforcing its observability component allowlist."
  (let* ((normalized (nemo-relay--normalize-plugin-config value))
         (root (json-decode normalized)))
    (nemo-relay--validate-observability-plugin-config
     root allowed-component-kinds)
    normalized))

(-> nemo-relay-configuration-create
    (&key (:enabled-p boolean)
          (:config (option (or pathname string)))
          (:config-json (option string))
          (:plugin-config (option t))
          (:dynamic-plugins-json (option string))
          (:dynamic-plugins (option t))
          (:allowed-component-kinds list)
          (:allowed-dynamic-plugin-ids list)
          (:library-path (option string)))
    nemo-relay-configuration)
(defun nemo-relay-configuration-create
    (&key (enabled-p nil enabled-p-supplied-p)
          config config-json plugin-config dynamic-plugins-json dynamic-plugins
          allowed-component-kinds allowed-dynamic-plugin-ids
          (library-path nil library-path-supplied-p))
  "Create settings for Relay's native PluginConfig and optional dynamic plugins."
  (let* ((allowed-component-kinds
           (nemo-relay--normalize-custom-observability-kinds
            allowed-component-kinds))
         (allowed-dynamic-plugin-ids
           (nemo-relay--normalize-custom-observability-plugin-ids
            allowed-dynamic-plugin-ids))
         (config-path
           (and config (nemo-relay--absolute-pathname config)))
         (environment-config
           (and (null config-path)
                (let ((value (uiop:getenv "AUTOLITH_RELAY_CONFIG")))
                  (and (non-empty-string-p value)
                       (nemo-relay--absolute-pathname value)))))
         (selected-config-path (or config-path environment-config))
         (selected-config-json
           (or config-json
               (and plugin-config (nemo-relay--normalize-plugin-config plugin-config))))
         (selected-dynamic-plugins-json
           (or dynamic-plugins-json
               (and dynamic-plugins
                    (nemo-relay--normalize-dynamic-plugins dynamic-plugins))))
         (selected-library
           (if library-path-supplied-p
               library-path
               (uiop:getenv "AUTOLITH_RELAY_LIBRARY"))))
    (when selected-config-json
      (setf selected-config-json
            (nemo-relay--normalize-observability-plugin-config
             selected-config-json
             allowed-component-kinds)))
    (when selected-dynamic-plugins-json
      (setf selected-dynamic-plugins-json
            (nemo-relay--normalize-dynamic-plugins selected-dynamic-plugins-json))
      (nemo-relay--validate-dynamic-plugin-config
       selected-dynamic-plugins-json
       allowed-dynamic-plugin-ids))
    (make-instance
     'nemo-relay-configuration
     :enabled-p
     (if enabled-p-supplied-p
         (not (null enabled-p))
         (nemo-relay--environment-boolean "AUTOLITH_RELAY" nil))
     :config-path selected-config-path
     :plugin-config-json selected-config-json
     :dynamic-plugins-json selected-dynamic-plugins-json
     :allowed-component-kinds allowed-component-kinds
     :allowed-dynamic-plugin-ids allowed-dynamic-plugin-ids
     :library-path (and (non-empty-string-p selected-library) selected-library))))

(-> nemo-relay-enabled-p () boolean)
(defun nemo-relay-enabled-p ()
  "Return true when Relay is enabled by explicit or environment-selected settings."
  (handler-case
      (nemo-relay-configuration-enabled-p
       (or *nemo-relay-configuration*
           (nemo-relay-configuration-create)))
    (serious-condition ()
      nil)))

(-> nemo-relay-configure
    (&key (:enabled boolean)
          (:config (option (or pathname string)))
          (:config-json (option string))
          (:plugin-config (option t))
          (:dynamic-plugins-json (option string))
          (:dynamic-plugins (option t))
          (:allowed-component-kinds list)
          (:allowed-dynamic-plugin-ids list)
          (:library (option string)))
    nemo-relay-configuration)
(defun nemo-relay-configure
    (&key (enabled nil enabled-p-supplied-p)
          config config-json plugin-config dynamic-plugins-json dynamic-plugins
          allowed-component-kinds allowed-dynamic-plugin-ids
          (library nil library-supplied-p))
  "Set Relay settings using a TOML or JSON PluginConfig document.

CONFIG names a Relay plugins.toml file or a legacy JSON PluginConfig file.
CONFIG-JSON and PLUGIN-CONFIG accept the JSON document directly. DYNAMIC-PLUGINS
names Relay's optional dynamic-plugin specification array. ALLOWED-COMPONENT-KINDS
and ALLOWED-DYNAMIC-PLUGIN-IDS explicitly authorize custom observability
components and dynamic plugins. Explicit values override environment discovery."
  (when *nemo-relay-runtime*
    (nemo-relay-shutdown))
  (let ((arguments nil))
    (when enabled-p-supplied-p
      (setf arguments (list* :enabled-p enabled arguments)))
    (when config
      (setf arguments (list* :config config arguments)))
    (when config-json
      (setf arguments (list* :config-json config-json arguments)))
    (when plugin-config
      (setf arguments (list* :plugin-config plugin-config arguments)))
    (when dynamic-plugins-json
      (setf arguments (list* :dynamic-plugins-json dynamic-plugins-json arguments)))
    (when dynamic-plugins
      (setf arguments (list* :dynamic-plugins dynamic-plugins arguments)))
    (when allowed-component-kinds
      (setf arguments (list* :allowed-component-kinds
                             allowed-component-kinds
                             arguments)))
    (when allowed-dynamic-plugin-ids
      (setf arguments (list* :allowed-dynamic-plugin-ids
                             allowed-dynamic-plugin-ids
                             arguments)))
    (when library-supplied-p
      (setf arguments (list* :library-path library arguments)))
    (setf *nemo-relay-configuration*
          (apply #'nemo-relay-configuration-create arguments)))
  *nemo-relay-configuration*)

;;;; -- Relay Plugin JSON Helpers --

(-> nemo-relay-observability-plugin-kind () string)
(defun nemo-relay-observability-plugin-kind ()
  "Return Relay's built-in observability plugin kind."
  (or (nemo-relay--returned-string (%nemo-relay-observability-plugin-kind))
      (error 'nemo-relay-error
             :message "Relay did not return its observability plugin kind."
             :operation "nemo_relay_observability_plugin_kind")))

(-> nemo-relay-observability-default-config () json-object)
(defun nemo-relay-observability-default-config ()
  "Return Relay's current default observability configuration as a JSON object."
  (or (nemo-relay--take-json-output
       "nemo_relay_observability_default_config_json"
       #'%nemo-relay-observability-default-config-json)
      (error 'nemo-relay-error
             :message "Relay did not return its default observability config."
             :operation "nemo_relay_observability_default_config_json")))

(-> nemo-relay-observability-component-spec
    (&key (:config (option t)) (:enabled boolean))
    json-object)
(defun nemo-relay-observability-component-spec (&key config (enabled t))
  "Wrap CONFIG in Relay's top-level observability PluginComponentSpec."
  (let ((config-json (and config (nemo-relay--json-argument config))))
    (or (nemo-relay--take-json-output
         "nemo_relay_observability_component_spec_json"
         (lambda (out-json)
           (nemo-relay--call-with-c-strings
            (list config-json)
            (lambda (config-pointer)
              (%nemo-relay-observability-component-spec-json
               config-pointer enabled out-json)))))
        (error 'nemo-relay-error
               :message "Relay did not return an observability component spec."
               :operation "nemo_relay_observability_component_spec_json"))))

(-> nemo-relay--implicit-plugin-config-paths () list)
(defun nemo-relay--implicit-plugin-config-paths ()
  "Return the pathnames used by Relay's native implicit discovery."
  (let* ((config-home
           (or (uiop:getenv "XDG_CONFIG_HOME")
               (let ((home (or (uiop:getenv "HOME")
                               (uiop:getenv "USERPROFILE"))))
                 (and home
                      (merge-pathnames
                       ".config/"
                       (uiop:ensure-directory-pathname
                        (nemo-relay--path-value home)))))))
         (user-directory
           (and config-home
                (merge-pathnames
                 "nemo-relay/"
                 (uiop:ensure-directory-pathname
                   (nemo-relay--path-value config-home)))))
         (system-directory
           (if (uiop/os:os-windows-p)
               (nemo-relay--path-value
                (or (uiop:getenv "ProgramData") "C:/ProgramData/"))
               (pathname "/etc/nemo-relay/"))))
    (remove-duplicates
     (append
      (when user-directory
        (list
         (merge-pathnames
          "plugins.toml"
          (uiop:ensure-directory-pathname user-directory))))
      (list
       (merge-pathnames
        "plugins.toml"
        (uiop:ensure-directory-pathname system-directory))))
     :test #'equal)))

(-> nemo-relay--ensure-no-implicit-plugin-config
    (&key (:explicit-p boolean))
    null)
(defun nemo-relay--ensure-no-implicit-plugin-config (&key (explicit-p nil))
  "Reject native Relay plugin files unless an explicit config is in use."
  (unless explicit-p
    (let ((paths (remove-if-not #'uiop:file-exists-p
                                (nemo-relay--implicit-plugin-config-paths))))
      (when paths
        (error 'nemo-relay-error
               :message
               (format nil
                       "Relay implicit plugins.toml discovery is disabled; native discovery found ~{~A~^, ~}."
                       paths)
               :operation "Relay PluginConfig discovery"))))
  nil)

(-> nemo-relay--configuration-plugin-config-json
    (nemo-relay-configuration)
    string)
(defun nemo-relay--configuration-plugin-config-json (configuration)
  "Resolve CONFIGURATION to an explicit observability-only PluginConfig."
  (let ((source
          (or (nemo-relay-configuration-plugin-config-json configuration)
              (let ((path (nemo-relay-configuration-config-path configuration)))
                (cond
                  ((null path)
                   (error 'nemo-relay-error
                          :message
                          "Relay requires an explicit PluginConfig; implicit discovery is disabled."
                          :operation "Relay PluginConfig configuration"))
                  ((not (uiop:file-exists-p path))
                   (error 'nemo-relay-error
                          :message
                          (format nil "Relay PluginConfig file does not exist: ~A" path)
                          :operation "Relay PluginConfig file"))
                  ((nemo-relay--toml-pathname-p path)
                   (json-encode
                    (nemo-relay--toml-document-plugin-config
                     (nemo-relay--read-toml-document path))))
                  (t
                   (nemo-relay--read-file-text path)))))))
    (nemo-relay--normalize-observability-plugin-config
     source
       (nemo-relay-configuration-allowed-component-kinds configuration))))

(-> nemo-relay--toml-required-string (json-object string string) string)
(defun nemo-relay--toml-required-string (object key context)
  "Return a non-empty string KEY from OBJECT or signal a configuration error."
  (let ((value (json-get object key)))
    (if (and (stringp value) (non-empty-string-p value))
        value
        (error 'nemo-relay-error
               :message
               (format nil "Relay ~A requires a non-empty string field ~A."
                       context key)
               :operation "Relay dynamic-plugin configuration"))))

(-> nemo-relay--toml-required-object (json-object string string) json-object)
(defun nemo-relay--toml-required-object (object key context)
  "Return JSON object KEY from OBJECT or signal a manifest error."
  (let ((value (json-get object key)))
    (if (json-object-p value)
        value
        (error 'nemo-relay-error
               :message
               (format nil "Relay ~A requires an object field ~A."
                       context key)
               :operation "Relay dynamic-plugin manifest"))))

(-> nemo-relay--toml-optional-string (json-object string string) (option string))
(defun nemo-relay--toml-optional-string (object key context)
  "Validate optional non-empty string KEY in OBJECT."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (cond
      ((not present-p)
       nil)
      ((and (stringp value) (non-empty-string-p value))
       value)
      (t
       (error 'nemo-relay-error
              :message
              (format nil "Relay ~A field ~A must be a non-empty string."
                      context key)
              :operation "Relay dynamic-plugin manifest")))))

(-> nemo-relay--toml-boolean-p (t) boolean)
(defun nemo-relay--toml-boolean-p (value)
  "Return true when VALUE is one of the TOML boolean values."
  (or (eq value t) (eq value false)))

(-> nemo-relay--toml-capability-items (json-object) list)
(defun nemo-relay--toml-capability-items (manifest-document)
  "Read and validate Relay's dynamic-plugin capability declarations."
  (let* ((capabilities
           (nemo-relay--toml-required-object
            manifest-document "capabilities" "plugin manifest"))
         (value (json-get capabilities "items")))
    (unless (or (vectorp value) (listp value))
      (error 'nemo-relay-error
             :message
             "Relay plugin manifest capabilities.items must be an array."
             :operation "Relay dynamic-plugin manifest"))
    (let ((items (if (vectorp value) (coerce value 'list) value)))
      (unless (and (plusp (length items))
                   (every #'non-empty-string-p items))
        (error 'nemo-relay-error
               :message
               "Relay plugin manifest capabilities.items must contain non-empty strings."
               :operation "Relay dynamic-plugin manifest"))
      (dolist (item items)
        (unless (member item '("plugin_native" "plugin_worker" "config_schema")
                         :test #'string=)
          (error 'nemo-relay-error
                 :message
                 (format nil "Unsupported Relay dynamic-plugin capability: ~A" item)
                 :operation "Relay dynamic-plugin manifest")))
      (when (/= (length items)
                (length (remove-duplicates items :test #'string=)))
        (error 'nemo-relay-error
               :message
               "Relay plugin manifest capabilities.items must not contain duplicates."
               :operation "Relay dynamic-plugin manifest"))
      items)))

(-> nemo-relay--toml-local-path-p (string) boolean)
(defun nemo-relay--toml-local-path-p (value)
  "Return true when VALUE is a local filesystem path rather than a URI/share."
  (and (not (uiop:string-prefix-p "//" value))
       (not (uiop:string-prefix-p "\\\\" value))
       (not (uiop:string-prefix-p "file:" (string-downcase value)))
       (not (search "://" value))))

(-> nemo-relay--validate-dynamic-plugin-manifest
    (json-object pathname)
    (values string string))
(defun nemo-relay--validate-dynamic-plugin-manifest
    (manifest-document manifest-path)
  "Validate upstream dynamic-plugin manifest metadata and return its identity."
  (let* ((manifest-version (json-get manifest-document "manifest_version"))
         (plugin
           (nemo-relay--toml-required-object
            manifest-document "plugin" "plugin manifest"))
         (plugin-id
           (nemo-relay--toml-required-string plugin "id" "plugin manifest"))
         (kind
           (nemo-relay--toml-required-string plugin "kind" "plugin manifest"))
         (compat
           (nemo-relay--toml-required-object
            manifest-document "compat" "plugin manifest"))
         (defaults
           (nemo-relay--toml-required-object
            manifest-document "defaults" "plugin manifest"))
         (capabilities (nemo-relay--toml-capability-items manifest-document))
         (load
           (nemo-relay--toml-required-object
            manifest-document "load" "plugin manifest")))
    (unless (and (integerp manifest-version) (= manifest-version 1))
      (error 'nemo-relay-error
             :message
             (format nil
                     "Relay dynamic-plugin manifest ~A requires manifest_version = 1."
                     manifest-path)
             :operation "Relay dynamic-plugin manifest"))
    (unless (member kind '("rust_dynamic" "worker") :test #'string=)
      (error 'nemo-relay-error
             :message (format nil "Unsupported Relay dynamic plugin kind: ~A" kind)
             :operation "Relay dynamic-plugin manifest"))
    (nemo-relay--toml-optional-string plugin "name" "plugin manifest")
    (nemo-relay--toml-optional-string plugin "version" "plugin manifest")
    (nemo-relay--toml-optional-string manifest-document "description" "plugin manifest")
    (multiple-value-bind (source source-present-p)
        (gethash "source" manifest-document)
      (when source-present-p
        (unless (json-object-p source)
          (error 'nemo-relay-error
                 :message "Relay plugin manifest source must be an object."
                 :operation "Relay dynamic-plugin manifest"))
        (nemo-relay--toml-optional-string source "manifest_root" "plugin manifest")
        (nemo-relay--toml-optional-string source "artifact" "plugin manifest")))
    (multiple-value-bind (integrity integrity-present-p)
        (gethash "integrity" manifest-document)
      (when integrity-present-p
        (unless (json-object-p integrity)
          (error 'nemo-relay-error
                 :message "Relay plugin manifest integrity must be an object."
                 :operation "Relay dynamic-plugin manifest"))
        (nemo-relay--toml-optional-string integrity "sha256" "plugin manifest")
        (nemo-relay--toml-optional-string integrity "signature" "plugin manifest")))
    (multiple-value-bind (enabled enabled-present-p)
        (gethash "enabled" defaults)
      (when enabled-present-p
        (unless (nemo-relay--toml-boolean-p enabled)
          (error 'nemo-relay-error
                 :message
                 "Relay plugin manifest defaults.enabled must be a boolean."
                 :operation "Relay dynamic-plugin manifest"))
        (when (eq enabled t)
          (error 'nemo-relay-error
                 :message
                 "Relay dynamic-plugin manifests must set defaults.enabled = false."
                 :operation "Relay dynamic-plugin manifest"))))
    (let* ((native-api
             (nemo-relay--toml-optional-string compat "native_api" "plugin manifest"))
           (worker-protocol
             (nemo-relay--toml-optional-string
              compat "worker_protocol" "plugin manifest")))
      (nemo-relay--toml-required-string compat "relay" "plugin manifest")
      (if (string= kind "rust_dynamic")
          (progn
            (unless (and native-api (string= native-api "1"))
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins must declare compat.native_api = \"1\"."
                     :operation "Relay dynamic-plugin manifest"))
            (when worker-protocol
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins must not declare compat.worker_protocol."
                     :operation "Relay dynamic-plugin manifest")))
          (progn
            (unless (and worker-protocol
                         (string= worker-protocol "grpc-v1"))
              (error 'nemo-relay-error
                     :message
                     "worker plugins must declare compat.worker_protocol = \"grpc-v1\"."
                     :operation "Relay dynamic-plugin manifest"))
            (when native-api
              (error 'nemo-relay-error
                     :message
                     "worker plugins must not declare compat.native_api."
                     :operation "Relay dynamic-plugin manifest")))))
    (let* ((runtime
             (nemo-relay--toml-optional-string load "runtime" "plugin manifest"))
           (entrypoint
             (nemo-relay--toml-optional-string load "entrypoint" "plugin manifest"))
           (library
             (nemo-relay--toml-optional-string load "library" "plugin manifest"))
           (symbol
             (nemo-relay--toml-optional-string load "symbol" "plugin manifest"))
           (worker-fields-p (or runtime entrypoint))
           (native-fields-p (or library symbol)))
      (when (or (and worker-fields-p native-fields-p)
                (and (not worker-fields-p) (not native-fields-p)))
        (error 'nemo-relay-error
               :message
               "Relay plugin manifest load must declare exactly one execution lane."
               :operation "Relay dynamic-plugin manifest"))
      (if (string= kind "rust_dynamic")
          (progn
            (when worker-fields-p
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins must not declare worker load fields."
                     :operation "Relay dynamic-plugin manifest"))
            (unless (and library symbol)
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins require load.library and load.symbol."
                     :operation "Relay dynamic-plugin manifest")))
          (progn
            (when native-fields-p
              (error 'nemo-relay-error
                     :message
                     "worker plugins must not declare native load fields."
                     :operation "Relay dynamic-plugin manifest"))
            (unless (and runtime entrypoint)
              (error 'nemo-relay-error
                     :message
                     "worker plugins require load.runtime and load.entrypoint."
                     :operation "Relay dynamic-plugin manifest"))
            (unless (member runtime '("python" "rust" "command") :test #'string=)
              (error 'nemo-relay-error
                     :message (format nil "Unsupported worker runtime: ~A" runtime)
                     :operation "Relay dynamic-plugin manifest")))))
    (let ((has-native-p
             (not (null (member "plugin_native" capabilities :test #'string=))))
          (has-worker-p
             (not (null (member "plugin_worker" capabilities :test #'string=))))
          (has-schema-p
             (not (null (member "config_schema" capabilities :test #'string=)))))
      (if (string= kind "rust_dynamic")
          (progn
            (unless has-native-p
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins must declare plugin_native."
                     :operation "Relay dynamic-plugin manifest"))
            (when has-worker-p
              (error 'nemo-relay-error
                     :message
                     "rust_dynamic plugins must not declare plugin_worker."
                     :operation "Relay dynamic-plugin manifest")))
          (progn
            (unless has-worker-p
              (error 'nemo-relay-error
                     :message
                     "worker plugins must declare plugin_worker."
                     :operation "Relay dynamic-plugin manifest"))
            (when has-native-p
              (error 'nemo-relay-error
                     :message
                     "worker plugins must not declare plugin_native."
                     :operation "Relay dynamic-plugin manifest"))))
      (multiple-value-bind (schema schema-present-p)
          (gethash "config_schema" manifest-document)
        (cond
          ((and has-schema-p (not schema-present-p))
           (error 'nemo-relay-error
                  :message
                  "Relay plugin manifest config_schema capability requires [config_schema]."
                  :operation "Relay dynamic-plugin manifest"))
          ((and (not has-schema-p) schema-present-p)
           (error 'nemo-relay-error
                  :message
                  "Relay plugin manifest [config_schema] requires the config_schema capability."
                  :operation "Relay dynamic-plugin manifest"))
          (schema-present-p
           (unless (json-object-p schema)
             (error 'nemo-relay-error
                    :message "Relay plugin manifest config_schema must be an object."
                    :operation "Relay dynamic-plugin manifest"))
           (let ((path
                   (nemo-relay--toml-required-string
                    schema "path" "plugin manifest config_schema")))
             (unless (nemo-relay--toml-local-path-p path)
               (error 'nemo-relay-error
                      :message
                      "Relay plugin manifest config_schema.path must be a local path."
                      :operation "Relay dynamic-plugin manifest")))))))
      (values plugin-id kind)))

(-> nemo-relay--dynamic-plugin-manifest-metadata
    ((or pathname string) &optional pathname)
    (values string string pathname))
(defun nemo-relay--dynamic-plugin-manifest-metadata (manifest-value &optional base)
  "Load and validate one dynamic-plugin manifest from MANIFEST-VALUE."
  (let* ((manifest-path
           (if base
               (nemo-relay--absolute-pathname manifest-value base)
               (nemo-relay--absolute-pathname manifest-value)))
         (manifest-file (probe-file manifest-path)))
    (unless (and manifest-file
                 (not (uiop:directory-pathname-p manifest-file)))
      (error 'nemo-relay-error
             :message
             (format nil
                     "Relay dynamic plugin manifest does not exist: ~A"
                     manifest-path)
             :operation "Relay dynamic-plugin manifest"))
    (let ((manifest-path (truename manifest-file)))
      (multiple-value-bind (plugin-id kind)
          (nemo-relay--validate-dynamic-plugin-manifest
           (nemo-relay--read-toml-document manifest-path)
           manifest-path)
        (values plugin-id kind manifest-path)))))

(-> nemo-relay--toml-dynamic-plugin-spec (json-object pathname integer) json-object)
(defun nemo-relay--toml-dynamic-plugin-spec (record config-path index)
  "Convert one TOML dynamic-plugin RECORD to a Relay activation spec."
  (unless (json-object-p record)
    (error 'nemo-relay-error
           :message (format nil "Relay dynamic plugin ~D must be a table." index)
           :operation "Relay dynamic-plugin configuration"))
  (let ((manifest-value
          (nemo-relay--toml-required-string record "manifest" "dynamic plugin")))
    (multiple-value-bind (config config-present-p)
        (gethash "config" record)
      (when (and config-present-p (not (json-object-p config)))
        (error 'nemo-relay-error
               :message
               (format nil "Relay dynamic plugin ~D config must be a table." index)
               :operation "Relay dynamic-plugin configuration"))
      (multiple-value-bind (plugin-id kind manifest-path)
          (nemo-relay--dynamic-plugin-manifest-metadata
           manifest-value
           (uiop:pathname-directory-pathname config-path))
        (json-object
         "plugin_id" plugin-id
         "kind" kind
         "manifest_ref" (namestring manifest-path)
         "config" (if config-present-p config (json-object)))))))

(-> nemo-relay--toml-dynamic-plugin-specs
    ((option vector) pathname)
    (option vector))
(defun nemo-relay--toml-dynamic-plugin-specs (records config-path)
  "Convert TOML dynamic-plugin RECORDS into unique Relay activation specs."
  (when (and records (plusp (length records)))
    (let ((seen (make-hash-table :test #'equal))
          (specs nil))
      (loop for record across records
            for index from 0
            for spec = (nemo-relay--toml-dynamic-plugin-spec record config-path index)
            for plugin-id = (json-get spec "plugin_id")
            do (when (gethash plugin-id seen)
                 (error 'nemo-relay-error
                        :message
                        (format nil "Duplicate Relay dynamic plugin ID: ~A"
                                plugin-id)
                        :operation "Relay dynamic-plugin configuration"))
               (setf (gethash plugin-id seen) t)
               (push spec specs))
      (coerce (nreverse specs) 'vector))))

(-> nemo-relay--configuration-dynamic-plugins-json
    (nemo-relay-configuration)
    (option string))
(defun nemo-relay--configuration-dynamic-plugins-json (configuration)
  "Resolve and authorize CONFIGURATION's dynamic-plugin specifications."
  (let ((source
          (or (nemo-relay-configuration-dynamic-plugins-json configuration)
              (let ((config-path (nemo-relay-configuration-config-path configuration)))
                (and config-path
                     (nemo-relay--toml-pathname-p config-path)
                     (let* ((document (nemo-relay--read-toml-document config-path))
                            (records (nemo-relay--toml-document-dynamic-plugins document))
                            (specs
                              (nemo-relay--toml-dynamic-plugin-specs
                               records config-path)))
                       (and specs (json-encode specs)))))
              (let ((path-value (uiop:getenv "AUTOLITH_RELAY_DYNAMIC_PLUGINS_CONFIG")))
                (and (non-empty-string-p path-value)
                     (let ((path (nemo-relay--absolute-pathname path-value)))
                       (if (uiop:file-exists-p path)
                           (nemo-relay--read-file-text path)
                           (error 'nemo-relay-error
                                  :message
                                  (format nil
                                          "Relay dynamic-plugin configuration file does not exist: ~A"
                                          path)
                                  :operation "Relay dynamic-plugin configuration file"))))))))
    (when source
      (let ((normalized (nemo-relay--normalize-dynamic-plugins source)))
        (nemo-relay--validate-dynamic-plugin-config
         normalized
           (nemo-relay-configuration-allowed-dynamic-plugin-ids configuration))
        normalized))))

(-> nemo-relay--runtime-configuration ((option configuration)) nemo-relay-configuration)
(defun nemo-relay--runtime-configuration (configuration)
  "Resolve Relay settings against CONFIGURATION and environment."
  (or *nemo-relay-configuration*
      (let ((config-path
              (let ((value (uiop:getenv "AUTOLITH_RELAY_CONFIG")))
                (and (non-empty-string-p value)
                     (nemo-relay--absolute-pathname value)))))
        (declare (ignore configuration))
        (nemo-relay-configuration-create :config config-path))))

;;;; -- Plugin Lifecycle --

(-> nemo-relay-validate-plugin-config
    (t &key (:allowed-component-kinds list))
    (option json-object))
(defun nemo-relay-validate-plugin-config (config &key (allowed-component-kinds nil))
  "Validate CONFIG with Relay after enforcing its observability allowlist."
  (let ((config-json
          (nemo-relay--normalize-observability-plugin-config
           config allowed-component-kinds)))
    (nemo-relay--take-json-output
     "nemo_relay_validate_plugin_config"
     (lambda (out-json)
       (nemo-relay--call-with-c-strings
        (list config-json)
        (lambda (config-pointer)
          (%nemo-relay-validate-plugin-config config-pointer out-json)))))))

(-> nemo-relay-initialize-plugins
    (t &key (:allowed-component-kinds list))
    (option json-object))
(defun nemo-relay-initialize-plugins
    (config &key (allowed-component-kinds nil))
  "Initialize allowed static observability components and return its report."
  (nemo-relay--ensure-no-implicit-plugin-config :explicit-p t)
  (let ((config-json
          (nemo-relay--normalize-observability-plugin-config
           config allowed-component-kinds)))
    (nemo-relay--take-json-output
     "nemo_relay_initialize_plugins"
     (lambda (out-json)
       (nemo-relay--call-with-c-strings
        (list config-json)
        (lambda (config-pointer)
          (%nemo-relay-initialize-plugins config-pointer out-json)))))))

(-> nemo-relay-initialize-with-dynamic-plugins
    (t t &key (:allowed-component-kinds list)
            (:allowed-dynamic-plugin-ids list))
    (values t (option json-object)))
(defun nemo-relay-initialize-with-dynamic-plugins
    (config dynamic-plugins
     &key (allowed-component-kinds nil)
          (allowed-dynamic-plugin-ids nil))
  "Initialize explicitly allowed observability and dynamic Relay plugins.

The first returned value is an opaque activation handle that must eventually be
passed to RELAY-CLEAR-DYNAMIC-PLUGIN-ACTIVATION."
  (nemo-relay--ensure-no-implicit-plugin-config :explicit-p t)
  (let* ((config-json
           (nemo-relay--normalize-observability-plugin-config
            config allowed-component-kinds))
         (dynamic-json (nemo-relay--normalize-dynamic-plugins dynamic-plugins)))
    (nemo-relay--validate-dynamic-plugin-config
     dynamic-json allowed-dynamic-plugin-ids)
    (let ((activation-slot (nemo-relay--make-output-slot))
          (report-slot (nemo-relay--make-output-slot)))
      (unwind-protect
           (nemo-relay--call-with-c-strings
            (list config-json dynamic-json)
            (lambda (config-pointer dynamic-pointer)
              (if (nemo-relay--ffi-status
                   "nemo_relay_initialize_with_dynamic_plugins"
                   (lambda ()
                     (%nemo-relay-initialize-with-dynamic-plugins
                      config-pointer dynamic-pointer activation-slot report-slot)))
                  (let ((activation (nemo-relay--output-slot-value activation-slot))
                        (report-pointer (nemo-relay--output-slot-value report-slot)))
                    (when (nemo-relay--pointer-present-p report-pointer)
                      (setf (cffi:mem-ref report-slot :pointer)
                            (cffi:null-pointer)))
                    (values
                     activation
                     (and (nemo-relay--pointer-present-p report-pointer)
                          (let ((source (nemo-relay--returned-string report-pointer)))
                            (handler-case
                                (json-decode source)
                              (serious-condition (condition)
                                (nemo-relay--set-last-error
                                 (format nil
                                         "nemo_relay_initialize_with_dynamic_plugins returned invalid JSON: ~A"
                                         (nemo-relay--condition-summary condition)))
                                nil))))))
                  (values nil nil))))
        (let ((report-pointer (nemo-relay--output-slot-value report-slot)))
          (when (nemo-relay--pointer-present-p report-pointer)
            (setf (cffi:mem-ref report-slot :pointer) (cffi:null-pointer))
            (%nemo-relay-string-free report-pointer)))
        (cffi:foreign-free activation-slot)
        (cffi:foreign-free report-slot)))))

(-> nemo-relay-clear-dynamic-plugin-activation (t) boolean)
(defun nemo-relay-clear-dynamic-plugin-activation (activation)
  "Clear and free an activation returned by RELAY-INITIALIZE-WITH-DYNAMIC-PLUGINS."
  (if (not (nemo-relay--pointer-present-p activation))
      t
      (let ((cleared-p
              (nemo-relay--ffi-status
               "nemo_relay_plugin_activation_clear"
               (lambda () (%nemo-relay-plugin-activation-clear activation)))))
        (cffi:with-foreign-object (slot :pointer)
          (setf (cffi:mem-ref slot :pointer) activation)
          (%nemo-relay-plugin-activation-free slot))
        cleared-p)))

(-> nemo-relay-clear-plugin-configuration () boolean)
(defun nemo-relay-clear-plugin-configuration ()
  "Clear Relay's process-wide static plugin configuration."
  (nemo-relay--ffi-status
   "nemo_relay_clear_plugin_configuration"
   #'%nemo-relay-clear-plugin-configuration))

(-> nemo-relay-active-plugin-report () (option json-object))
(defun nemo-relay-active-plugin-report ()
  "Return Relay's active plugin report, when one has been configured."
  (nemo-relay--take-json-output
   "nemo_relay_active_plugin_report_json"
   #'%nemo-relay-active-plugin-report-json))

(-> nemo-relay-list-plugin-kinds () (option vector))
(defun nemo-relay-list-plugin-kinds ()
  "Return registered custom and observability Relay plugin kinds."
  (let ((kinds
          (nemo-relay--take-json-output
           "nemo_relay_list_plugin_kinds_json"
           #'%nemo-relay-list-plugin-kinds-json)))
    (and kinds
         (coerce
          (remove-if #'nemo-relay--non-observability-plugin-kind-p
                     (coerce kinds 'list))
          'vector))))

;;;; -- Runtime Lifecycle --

(-> nemo-relay--json-component (json-object string) (option json-object))
(defun nemo-relay--json-component (root kind)
  "Find the first enabled or disabled plugin component of KIND in ROOT."
  (let ((components (json-get root "components")))
    (find-if
     (lambda (component)
       (and (json-object-p component)
            (string= (json-get component "kind") kind)))
     (if (vectorp components) (coerce components 'list) components))))

(-> nemo-relay--configuration-full-payloads-enabled-p (string) boolean)
(defun nemo-relay--configuration-full-payloads-enabled-p (plugin-config-json)
  "Read Relay's official observability full-payload policy from PLUGIN-CONFIG-JSON."
  (handler-case
      (let* ((root (json-decode plugin-config-json))
             (kind (nemo-relay-observability-plugin-kind))
             (component (and (json-object-p root)
                             (nemo-relay--json-component root kind)))
             (config (and component (json-get component "config"))))
        (not (null (and (json-object-p config)
                        (json-get config "enable_full_payloads")))))
    (serious-condition ()
      nil)))

(-> nemo-relay--configuration-output-pathname
    (string (option pathname) (option pathname))
    (option pathname))
(defun nemo-relay--configuration-output-pathname
    (plugin-config-json config-path data-root)
  "Derive the first configured local ATOF file path from Relay's config."
  (handler-case
      (let* ((root (json-decode plugin-config-json))
             (kind (nemo-relay-observability-plugin-kind))
             (component (and (json-object-p root)
                             (nemo-relay--json-component root kind)))
             (observability (and component (json-get component "config")))
             (atof (and (json-object-p observability)
                        (json-get observability "atof")))
             (sinks (and (json-object-p atof) (json-get atof "sinks")))
             (sink
               (find-if
                (lambda (candidate)
                  (and (json-object-p candidate)
                       (string= (json-get candidate "type") "file")))
                (if (vectorp sinks) (coerce sinks 'list) sinks)))
             (filename (and sink (json-get sink "filename")))
             (root-directory (or data-root (nemo-relay--default-data-root)))
             (base-directory
               (or (and config-path
                        (uiop:pathname-directory-pathname config-path))
                   root-directory))
             (directory
               (and sink
                    (nemo-relay--absolute-pathname
                     (or (json-get sink "output_directory") root-directory)
                     base-directory))))
        (and (stringp filename)
             directory
             (merge-pathnames filename directory)))
    (serious-condition ()
      nil)))
(-> nemo-relay-start (&optional (option configuration)) boolean)
(defun nemo-relay-start (&optional configuration)
  "Start optional Relay PluginConfig observability for CONFIGURATION.

Startup failures are retained in RELAY-LAST-ERROR and never change Autolith's
provider, tool, conversation, or shutdown behavior."
  (with-lock-held (*nemo-relay-runtime-lock*)
    (if *nemo-relay-runtime*
        t
        (let ((settings
                (handler-case
                    (nemo-relay--runtime-configuration configuration)
                  (serious-condition (condition)
                    (nemo-relay--set-last-error
                     (format nil "Relay configuration failed: ~A"
                             (nemo-relay--condition-summary condition)))
                    nil))))
          (if (or (null settings)
                  (not (nemo-relay-configuration-enabled-p settings)))
              nil
              (let ((activation nil))
                (handler-case
                    (progn
                      (nemo-relay--ensure-no-implicit-plugin-config
                       :explicit-p
                       (not (null
                             (or (nemo-relay-configuration-config-path settings)
                                 (nemo-relay-configuration-plugin-config-json
                                  settings)))))
                      (let* ((plugin-config-json
                               (nemo-relay--configuration-plugin-config-json settings))
                             (dynamic-json
                               (nemo-relay--configuration-dynamic-plugins-json settings))
                             (library
                               (nemo-relay--load-library
                                (nemo-relay-configuration-library-path settings))))
                        (let ((report nil))
                          (if dynamic-json
                              (multiple-value-bind (new-activation new-report)
                                  (nemo-relay-initialize-with-dynamic-plugins
                                   plugin-config-json
                                   dynamic-json
                                   :allowed-component-kinds
                                   (nemo-relay-configuration-allowed-component-kinds
                                    settings)
                                   :allowed-dynamic-plugin-ids
                                   (nemo-relay-configuration-allowed-dynamic-plugin-ids
                                    settings))
                                (setf activation new-activation
                                      report new-report))
                              (setf report
                                    (nemo-relay-initialize-plugins
                                     plugin-config-json
                                     :allowed-component-kinds
                                     (nemo-relay-configuration-allowed-component-kinds
                                      settings))))
                          (unless report
                            (error 'nemo-relay-error
                                   :message (or (nemo-relay-last-error)
                                                "Relay plugin initialization failed.")
                                   :operation "Relay plugin initialization"))
                          (setf *nemo-relay-runtime*
                                (make-instance
                                 'nemo-relay-runtime
                                 :configuration settings
                                 :library library
                                 :activation activation
                                 :plugin-config-json plugin-config-json
                                 :report report
                                 :output-pathname
                                 (nemo-relay--configuration-output-pathname
                                  plugin-config-json
                                  (nemo-relay-configuration-config-path settings)
                                  (and configuration
                                       (configuration-data-root configuration)))
                                 :full-payloads-enabled-p
                                 (nemo-relay--configuration-full-payloads-enabled-p
                                  plugin-config-json))
                                *nemo-relay-last-error* nil)
                          t)))
                  (serious-condition (condition)
                    (when activation
                      (ignore-errors
                        (nemo-relay-clear-dynamic-plugin-activation activation)))
                    (ignore-errors (%nemo-relay-clear-plugin-configuration))
                    (nemo-relay--set-last-error
                     (format nil "Relay startup failed: ~A"
                             (nemo-relay--condition-summary condition)))
                    nil))))))))

(-> nemo-relay-output-pathname () (option pathname))
(defun nemo-relay-output-pathname ()
  "Return the first configured local ATOF file pathname, when known."
  (with-lock-held (*nemo-relay-runtime-lock*)
    (and *nemo-relay-runtime*
         (nemo-relay-runtime-output-pathname *nemo-relay-runtime*))))

(-> nemo-relay-flush () boolean)
(defun nemo-relay-flush ()
  "Wait for queued Relay subscriber callbacks to complete."
  (and *nemo-relay-runtime*
       (with-lock-held (*nemo-relay-runtime-lock*)
         (and *nemo-relay-runtime*
              (nemo-relay--ffi-status
               "nemo_relay_flush_subscribers"
               #'%nemo-relay-flush-subscribers)))))

(-> nemo-relay-shutdown () null)
(defun nemo-relay-shutdown ()
  "Flush and clear Relay's active plugin configuration idempotently."
  (with-lock-held (*nemo-relay-runtime-lock*)
    (let ((runtime *nemo-relay-runtime*))
      (when runtime
        (setf *nemo-relay-runtime* nil)
        (ignore-errors
          (nemo-relay--ffi-status
           "nemo_relay_flush_subscribers"
           #'%nemo-relay-flush-subscribers))
        (if (nemo-relay--pointer-present-p (nemo-relay-runtime-activation runtime))
            (ignore-errors
              (nemo-relay-clear-dynamic-plugin-activation
               (nemo-relay-runtime-activation runtime)))
            (ignore-errors
              (%nemo-relay-clear-plugin-configuration))))
    nil)))

(-> nemo-relay--detach-for-checkpoint () null)
(defun nemo-relay--detach-for-checkpoint ()
  "Drop process-local Relay handles before a checkpoint image is saved."
  (setf *nemo-relay-runtime* nil
        *nemo-relay-last-error* nil
        *nemo-relay-propagation-context-json* nil
        *nemo-relay-instrumentation-suppressed-p* nil)
  nil)

(-> nemo-relay--active-p () boolean)
(defun nemo-relay--active-p ()
  "Return true when Relay calls are safe for the current dynamic context."
  (and *nemo-relay-runtime*
       (not *nemo-relay-instrumentation-suppressed-p*)))

(-> nemo-relay--full-payloads-enabled-p () boolean)
(defun nemo-relay--full-payloads-enabled-p ()
  "Return whether the active native Relay config permits full payloads."
  (and *nemo-relay-runtime*
       (nemo-relay-runtime-full-payloads-enabled-p *nemo-relay-runtime*)))
