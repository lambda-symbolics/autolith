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
  "Preserve boolean false, unknown optional values, and portable choice spellings."
  (cond
    ((eq (setting-type setting) 'boolean)
     (if value t (argo:json-false)))
    ((and (realp value) (not (integerp value)))
     (princ-to-string value))
    (t
     (application-client-value value))))

(-> application-client--setting-options (setting configuration) vector)
(defun application-client--setting-options (setting configuration)
  "Return SETTING's typed options as a JSON vector."
  (map 'vector (lambda (value) (application-client--setting-value setting value))
       (setting-options setting configuration)))

(-> application-client--setting-adjustable-p (application setting) boolean)
(defun application-client--setting-adjustable-p (application setting)
  "Use the application's ordinary runtime setting admission."
  (declare (ignore application))
  (application--setting-adjustable-p setting))

(-> application-client--setting-item (application configuration setting) hash-table)
(defun application-client--setting-item (application configuration setting)
  "Return the JSON object describing SETTING in CONFIGURATION."
  (let* ((configuration (if (member (setting-name setting)
                                  '(:telemetry-enabled-p :telemetry-diagnostics-p))
                            (telemetry-consent-configuration configuration)
                            configuration))
         (value         (configuration-setting-value configuration setting))
         (adjustable-p  (application-client--setting-adjustable-p application setting)))
    (agentcomms:json-object
     "name" (string-downcase (symbol-name (setting-name setting)))
     "label" (setting-label setting)
     "documentation" (or (setting-documentation setting) ':null)
     "group" (string-downcase (symbol-name (setting-group setting)))
     "type" (application-client--setting-type setting configuration)
     "options" (application-client--setting-options setting configuration)
     "value" (application-client--setting-value setting value)
     "source" (application-client--setting-source configuration setting)
     "scope" (application-client--setting-scope setting)
     "adjustable" (if adjustable-p t (argo:json-false))
     "reason" (if adjustable-p
                  ':null
                  "The setting is fixed by process configuration."))))

(-> application-client-settings (application hash-table) hash-table)
(defun application-client-settings (application params)
  "Return all visible settings and their current portable values."
  (declare (ignore params))
  (let* ((application   application)
         (configuration (application-configuration application)))
    (agentcomms:json-object
     "items"
     (coerce (loop for setting in (configuration-setting-list configuration)
                   when (setting-visible-p setting)
                     collect (application-client--setting-item application configuration setting))
             'vector))))

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

(-> application-client--model-item (application provider-registration provider-model &key (:stored-p boolean)) hash-table)
(defun application-client--model-item (application registration model &key stored-p)
  "Return model metadata and only locally observed credential verification."
  (let ((verified-at (gethash (provider-model-name model)
                              (client-hub-verified-models (application-client-hub application)))))
    (agentcomms:json-object
     "id" (provider-model-name model)
     "provider" (provider-registration-name registration)
     "credentialStored" (if stored-p t (argo:json-false))
     "recentlyVerified" (if (and verified-at (< (- (get-universal-time) verified-at) 3600))
                            t (argo:json-false))
     "contextWindow" (or (provider-model-context-window model) ':null))))

(-> application-client-models (application hash-table) hash-table)
(defun application-client-models (application params)
  "Return the registered provider models without exposing credentials."
  (declare (ignore params))
  (let ((configuration (application-configuration application)))
    (agentcomms:json-object
     "items"
     (coerce (loop for registration in (provider-registrations)
                   for stored-p = (application-client--credential-stored-p configuration registration)
                   append (loop for model in (provider-registration-models registration)
                                collect (application-client--model-item application registration model :stored-p stored-p)))
             'vector))))

(-> application-client-set-setting (application hash-table) hash-table)
(defun application-client-set-setting (application params)
  "Apply one existing adjustable setting and return its complete updated item."
  (let* ((application   application)
         (configuration (application-configuration application))
         (name          (agentcomms:acp-field params "name" :type ':string :required-p t))
         (value         (agentcomms:acp-field params "value" :required-p t))
         (setting       (find name (configuration-setting-list configuration)
                              :test #'string-equal
                              :key  (lambda (item)
                                      (string-downcase (symbol-name (setting-name item)))))))
    (unless setting
      (error 'application-client-unavailable :reason "The requested setting does not exist."))
    (unless (application-client--setting-adjustable-p application setting)
      (error 'application-client-unavailable :reason "The requested setting is not adjustable."))
    (application-apply-setting application (setting-name setting)
                               (if (argo:json-false-p value) nil value))
    (application-clients-publish-state application)
    (application-client--setting-item application (application-configuration application) setting)))
