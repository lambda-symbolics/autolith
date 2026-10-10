(in-package #:autolith)

;;;; -- ACP Settings and Models Extensions --

(-> acp-extension--setting-type (setting configuration) string)
(defun acp-extension--setting-type (setting configuration)
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

(-> acp-extension--setting-scope (t) string)
(defun acp-extension--setting-scope (setting)
  "Return SETTING's canonical ACP scope name."
  (case (setting-scope setting)
    (:process "process")
    (:durable "durable")
    (:session "session")
    (:derived "derived")
    (t "process")))

(-> acp-extension--setting-source (configuration setting) string)
(defun acp-extension--setting-source (configuration setting)
  "Return CONFIGURATION's source label for SETTING."
  (case (configuration-setting-source configuration (setting-name setting))
    (:environment "environment")
    (:override "explicit")
    (:durable "saved")
    (:session "session")
    (t "default")))

(-> acp-extension--setting-value (setting t) t)
(defun acp-extension--setting-value (setting value)
  "Preserve boolean false, unknown optional values, and portable choice spellings."
  (cond
    ((eq (setting-type setting) 'boolean)
     (if value t (argo:json-false)))
    ((and (realp value) (not (integerp value)))
     (princ-to-string value))
    (t
     (acp-extension-value value))))

(-> acp-extension--setting-options (setting configuration) vector)
(defun acp-extension--setting-options (setting configuration)
  "Return SETTING's typed options as a JSON vector."
  (map 'vector (lambda (value) (acp-extension--setting-value setting value))
       (setting-options setting configuration)))

(-> acp-extension--setting-adjustable-p (application setting) boolean)
(defun acp-extension--setting-adjustable-p (application setting)
  "Use the application's ordinary runtime setting admission."
  (declare (ignore application))
  (application--setting-adjustable-p setting))

(-> acp-extension--setting-item (application configuration setting) hash-table)
(defun acp-extension--setting-item (application configuration setting)
  "Return the JSON object describing SETTING in CONFIGURATION."
  (let* ((value (configuration-setting-value configuration setting))
         (adjustable-p (acp-extension--setting-adjustable-p application setting)))
    (agentcomms:json-object
     "name" (string-downcase (symbol-name (setting-name setting)))
     "label" (setting-label setting)
     "documentation" (or (setting-documentation setting) ':null)
     "group" (string-downcase (symbol-name (setting-group setting)))
     "type" (acp-extension--setting-type setting configuration)
     "options" (acp-extension--setting-options setting configuration)
     "value" (acp-extension--setting-value setting value)
     "source" (acp-extension--setting-source configuration setting)
     "scope" (acp-extension--setting-scope setting)
     "adjustable" (if adjustable-p t (argo:json-false))
     "reason" (if adjustable-p
                  ':null
                  "The setting is fixed by process configuration."))))

(-> acp-extension-settings (acp-session hash-table) hash-table)
(defun acp-extension-settings (session params)
  "Return all visible settings and their current portable values."
  (declare (ignore params))
  (let* ((application (acp-session-application session))
         (configuration (application-configuration application)))
    (agentcomms:json-object
     "items"
     (coerce (loop for setting in (configuration-setting-list configuration)
                   when (setting-visible-p setting)
                     collect (acp-extension--setting-item application configuration setting))
             'vector))))

(-> acp-extension--credential-stored-p (configuration provider-registration) boolean)
(defun acp-extension--credential-stored-p (configuration registration)
  "Probe stored credentials within transient secret accounting, without refreshing them."
  (handler-case
      (call-with-secret-use
       (lambda ()
         (let ((provider (funcall (provider-registration-factory registration) configuration)))
           (credential-manager-load (provider-credential-manager provider))
           t)))
    (credentials-unavailable ()
      nil)))

(-> acp-extension--model-item (acp-session provider-registration provider-model &key (:stored-p boolean)) hash-table)
(defun acp-extension--model-item (session registration model &key stored-p)
  "Return model metadata and only locally observed credential verification."
  (let ((verified-at (with-lock-held ((acp-session-extension-lock session))
                       (gethash (provider-model-name model) (acp-session-extension-verified-models session)))))
    (agentcomms:json-object
     "id" (provider-model-name model)
     "provider" (provider-registration-name registration)
     "credentialStored" (if stored-p t (argo:json-false))
     "recentlyVerified" (if (and verified-at (< (- (get-universal-time) verified-at) 3600))
                             t (argo:json-false))
     "contextWindow" (or (provider-model-context-window model) ':null))))

(-> acp-extension-models (acp-session hash-table) hash-table)
(defun acp-extension-models (session params)
  "Return the registered provider models without exposing credentials."
  (declare (ignore params))
  (let ((configuration (application-configuration (acp-session-application session))))
    (agentcomms:json-object
     "items"
     (coerce (loop for registration in (provider-registrations)
                   for stored-p = (acp-extension--credential-stored-p configuration registration)
                   append (loop for model in (provider-registration-models registration)
                                collect (acp-extension--model-item session registration model :stored-p stored-p)))
             'vector))))

(-> acp-extension-set-setting (acp-session hash-table) hash-table)
(defun acp-extension-set-setting (session params)
  "Apply one existing adjustable setting and return its complete updated item."
  (let* ((application (acp-session-application session))
         (configuration (application-configuration application))
         (name (agentcomms:acp-field params "name" :type ':string :required-p t))
         (value (agentcomms:acp-field params "value" :required-p t))
         (setting (find name (configuration-setting-list configuration)
                        :test #'string-equal
                        :key (lambda (item)
                               (string-downcase (symbol-name (setting-name item)))))))
    (unless setting
      (error 'acp-extension-unavailable :reason "The requested setting does not exist."))
    (unless (acp-extension--setting-adjustable-p application setting)
      (error 'acp-extension-unavailable :reason "The requested setting is not adjustable."))
    (application-apply-setting application (setting-name setting)
                               (if (argo:json-false-p value) nil value))
    (when (eq (setting-name setting) ':permission-mode)
      (with-lock-held ((acp-session-lock session))
        (setf (acp-session-mode session) (application-permission-mode application))
        (clrhash (acp-session-permissions session)))
      (agentcomms:agent-send-update
       (acp-session-service session) (acp-session-identifier session)
       (agentcomms:acp-update-current-mode (string-downcase (symbol-name (acp-session-mode session))))))
    (acp-extension--setting-item application (application-configuration application) setting)))
