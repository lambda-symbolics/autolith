(in-package #:autolith)

;;;; -- ACP Settings and Models Extensions --

(-> application-client--setting-type (setting configuration) string)
(defun application-client--setting-type (setting configuration)
  "Project registered types and dynamic options into the setting editor's wire types."
  (let ((type (setting-type setting)))
    (cond
      ((eq (setting-scope setting) ':derived)
       "derived")
      ((eq type 'boolean)
       "boolean")
      ((setting-options setting configuration)
       "choice")
      ((subtypep type 'integer)
       "integer")
      ((or (subtypep type 'pathname) (equal type '(option pathname)))
       "pathname")
      (t
       "string"))))

(-> application-client--setting-scope (t) string)
(defun application-client--setting-scope (setting)
  "Return SETTING's canonical ACP scope name."
  (case (setting-scope setting)
    (:process
     "process")
    (:durable
     "durable")
    (:session
     "session")
    (:derived
     "derived")
    (t
     "process")))

(-> application-client--setting-source (configuration setting) string)
(defun application-client--setting-source (configuration setting)
  "Return CONFIGURATION's source label for SETTING."
  (case (configuration-setting-source configuration (setting-name setting))
    (:environment
     "environment")
    (:override
     "explicit")
    (:durable
     "saved")
    (:session
     "session")
    (t
     "default")))

(-> application-client--setting-value (setting t) t)
(defun application-client--setting-value (setting value)
  "Return SETTING's value as a native Lisp value."
  (declare (ignore setting))
  value)

(-> application-client--setting-options (setting configuration) list)
(defun application-client--setting-options (setting configuration)
  "Return SETTING's typed options as native values."
  (mapcar (lambda (value) (application-client--setting-value setting value))
          (setting-options setting configuration)))

(-> application-client--setting-adjustable-p (application setting) boolean)
(defun application-client--setting-adjustable-p (application setting)
  "Use the application's ordinary runtime setting admission."
  (declare (ignore application))
  (application--setting-adjustable-p setting))

(-> application-client--setting-item (application configuration setting) list)
(defun application-client--setting-item (application configuration setting)
  "Return the native plist describing SETTING in CONFIGURATION."
  (let* ((configuration (if (member (setting-name setting)
                                    '(:telemetry-enabled-p :telemetry-diagnostics-p))
                            (telemetry-consent-configuration configuration)
                            configuration))
         (value         (configuration-setting-value configuration setting))
         (adjustable-p  (application-client--setting-adjustable-p application setting)))
    (list :name          (setting-name setting)
          :label         (setting-label setting)
          :documentation (setting-documentation setting)
          :group         (setting-group setting)
          :type          (application-client--setting-type setting configuration)
          :options       (application-client--setting-options setting configuration)
          :value         (application-client--setting-value setting value)
          :source        (configuration-setting-source configuration (setting-name setting))
          :scope         (setting-scope setting)
          :adjustable-p  adjustable-p
          :reason        (unless adjustable-p "The setting is fixed by process configuration."))))

(-> application-client-settings (application) list)
(defun application-client-settings (application)
  "Return all visible settings and their current native values."
  (let ((configuration (application-configuration application)))
    (list :items
          (loop for setting in (configuration-setting-list configuration)
                when (setting-visible-p setting)
                  collect (application-client--setting-item application configuration setting)))))

(-> application-client--credential-stored-p (configuration provider-registration) boolean)
(defun application-client--credential-stored-p (configuration registration)
  "Probe stored credentials within transient secret accounting, without refreshing them."
  (handler-case
      (call-with-secret-use
       (lambda ()
         (let ((provider (funcall (provider-registration-factory registration) configuration)))
           (credential-manager-load (provider-credential-manager provider))
           t)))
    (credentials-unavailable ()
      nil)))

(-> application-client--model-item (application provider-registration provider-model &key (:stored-p boolean)) list)
(defun application-client--model-item (application registration model &key stored-p)
  "Return native model metadata and only locally observed credential verification."
  (let ((verified-at (gethash (provider-model-name model)
                              (client-hub-verified-models (application-client-hub application)))))
    (list :id                  (provider-model-name model)
          :provider            (provider-registration-name registration)
          :credential-stored-p stored-p
          :recently-verified-p (and verified-at (< (- (get-universal-time) verified-at) 3600))
          :context-window      (provider-model-context-window model))))

(-> application-client-models (application) list)
(defun application-client-models (application)
  "Return the registered provider models without exposing credentials."
  (let ((configuration (application-configuration application)))
    (list :items
          (loop for registration in (provider-registrations)
                for stored-p = (application-client--credential-stored-p configuration registration)
                append (loop for model in (provider-registration-models registration)
                             collect (application-client--model-item application registration model
                                                                    :stored-p stored-p))))))

(-> application-client-set-setting (application symbol t) list)
(defun application-client-set-setting (application name value)
  "Apply one existing adjustable setting and return its complete native item."
  (let* ((configuration (application-configuration application))
         (setting       (find name (configuration-setting-list configuration)
                              :test (if (symbolp name) #'eq #'string-equal)
                              :key  (lambda (item)
                                      (if (symbolp name)
                                          (setting-name item)
                                          (string-downcase (symbol-name (setting-name item))))))))
    (unless setting
      (error 'application-client-unavailable :reason "The requested setting does not exist."))
    (unless (application-client--setting-adjustable-p application setting)
      (error 'application-client-unavailable :reason "The requested setting is not adjustable."))
    (application-apply-setting application (setting-name setting) value)
    (application-clients-publish-state application)
    (application-client--setting-item application (application-configuration application) setting)))
