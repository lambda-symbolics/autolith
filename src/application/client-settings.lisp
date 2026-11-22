(in-package #:autolith)

;;;; -- ACP Settings and Models Extensions --

(-> application-client--setting-type (setting list) string)
(defun application-client--setting-type (setting options)
  "Project SETTING and its effective OPTIONS into the editor's native types."
  (let ((type (setting-type setting)))
    (cond
      ((eq (setting-scope setting) ':derived)
       "derived")
      ((eq type 'boolean)
       "boolean")
      (options
       "choice")
      ((subtypep type 'integer)
       "integer")
      ((or (subtypep type 'pathname) (equal type '(option pathname)))
       "pathname")
      (t
       "string"))))

(-> application-client--setting-item (application setting) list)
(defun application-client--setting-item (application setting)
  "Return SETTING's effective state and metadata as a native plist."
  (multiple-value-bind (value source options adjustable-p)
      (application--setting-state application setting)
    (list :name          (setting-name setting)
          :label         (setting-label setting)
          :documentation (setting-documentation setting)
          :group         (setting-group setting)
          :type          (application-client--setting-type setting options)
          :options       options
          :value         value
          :source        source
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
                    collect (application-client--setting-item application setting)))))

(-> application-client--model-item (application provider-registration provider-model &key (:stored-p boolean)) list)
(defun application-client--model-item (application registration model &key stored-p)
  "Return native model metadata and only locally observed credential verification."
  (let* ((hub         (application-client-hub application))
         (key         (list (provider-registration-name registration) (provider-model-name model)))
         (verified-at (gethash key (client-hub-verified-models hub))))
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
                for stored-p = (eq (provider-credential-status configuration registration :refresh-p nil) ':available)
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
    (unless (application--setting-adjustable-p setting)
      (error 'application-client-unavailable :reason "The requested setting is not adjustable."))
    (application-apply-setting application (setting-name setting) value)
    (application-clients-publish-state application)
    (application-client--setting-item application setting)))
