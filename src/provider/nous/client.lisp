(in-package #:autolith)

;;;; -- Nous Research Provider --

(defclass nous-provider-mixin ()
  ()
  (:documentation "Shared Nous OAuth behavior across its two wire protocols."))

(defclass nous-chat-completions-provider
    (nous-provider-mixin openai-compatible-provider)
  ()
  (:documentation "A Nous OAuth client using Chat Completions."))

(defclass nous-messages-provider
    (nous-provider-mixin anthropic-api-key-provider)
  ()
  (:documentation "A Nous OAuth client using native Anthropic Messages."))

(defmethod provider-account-label ((provider nous-provider-mixin))
  "Name the Nous Research account service."
  (declare (ignore provider))
  "Nous Research")

(defmethod provider-family ((provider nous-provider-mixin))
  "Return the Nous conversation family."
  (declare (ignore provider))
  ':nous)

(defmethod provider-device-authentication-client ((provider nous-provider-mixin))
  "Return a fresh Nous Portal device authentication client."
  (declare (ignore provider))
  (nous-device-authentication-client-create))

(-> nous-provider-authenticate
    (nous-provider-mixin &key (:stream stream) (:open-browser-p boolean))
    string)
(defun nous-provider-authenticate (provider &key stream open-browser-p)
  "Run Nous Portal device login for PROVIDER and report successful persistence."
  (device-authentication-login
   (provider-device-authentication-client provider)
   (provider-credential-manager provider)
   :stream         (or stream *standard-output*)
   :open-browser-p open-browser-p)
  "Nous Research authentication was saved by Autolith.")

(defmethod provider-authenticate ((provider nous-provider-mixin)
                                  &key stream open-browser-p)
  "Authenticate PROVIDER through Nous Portal's browser device flow."
  (nous-provider-authenticate provider
                              :stream         stream
                              :open-browser-p open-browser-p))

(-> nous-provider--messages-model-p (string) boolean)
(defun nous-provider--messages-model-p (model)
  "Return true when MODEL uses Nous's native Anthropic Messages route."
  (if (uiop:string-prefix-p "anthropic/" (string-downcase model)) t nil))

(-> nous-provider--make
    (configuration credential-manager non-empty-string
     &key (:registration (option provider-registration)))
    model-provider)
(defun nous-provider--make (configuration manager session-id &key registration)
  "Create a Nous Messages client for anthropic/* models, or a Chat Completions client."
  (if (nous-provider--messages-model-p (config :model configuration))
      (make-instance
       'nous-messages-provider
       :configuration      configuration
       :registration       registration
       :credential-manager manager
       :session-id         session-id)
      (make-instance
       'nous-chat-completions-provider
       :configuration       configuration
       :registration        registration
       :credential-manager  manager
       :session-id          session-id
       :display-name        "Nous Research"
       :family              ':nous
       :headers             nil
       :reasoning-parameter nil)))

(-> nous-provider-create (configuration) model-provider)
(defun nous-provider-create (configuration)
  "Create the Nous provider selected by CONFIGURATION's model identifier."
  (nous-provider--make
   configuration
   (nous-credential-manager-create configuration)
   (make-identifier)))

(defmethod provider-family-create
    ((family (eql ':nous))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Nous provider selected by CONFIGURATION's model."
  (declare (ignore family reasoning-summaries-p))
  (nous-provider-create configuration))

(-> nous-provider--with-configuration
    (nous-provider-mixin configuration)
    model-provider)
(defun nous-provider--with-configuration (provider configuration)
  "Copy PROVIDER for CONFIGURATION while retaining credentials and session state."
  (nous-provider--make
   configuration
   (provider-credential-manager provider)
   (provider-session-id provider)
   :registration (model-provider-registration provider)))

(defmethod provider-with-configuration
    ((provider nous-chat-completions-provider)
     (configuration configuration))
  "Reconfigure a Nous Chat Completions provider, switching wire protocols if needed."
  (nous-provider--with-configuration provider configuration))

(defmethod provider-with-configuration
    ((provider nous-messages-provider)
     (configuration configuration))
  "Reconfigure a Nous Messages provider, switching wire protocols if needed."
  (nous-provider--with-configuration provider configuration))


;;;; -- Model Discovery --

(-> nous-provider--model-identifier-usable-p (string) boolean)
(defun nous-provider--model-identifier-usable-p (identifier)
  "Return true when IDENTIFIER is a non-Hermes Nous model."
  (if (null (search "hermes" identifier :test #'char-equal)) t nil))

(-> nous--fetch-models (configuration) list)
(defun nous--fetch-models (configuration)
  "Fetch Nous model identifiers, excluding Hermes models unsuitable for tools."
  (let ((seen nil))
    (loop for spec
            in (openai-compatible--fetch-models
                configuration
                :provider-name "Nous Research"
                :endpoint      (nous-models-endpoint)
                :credential-manager
                (nous-credential-manager-create configuration))
          for identifier = (or (model-spec-name spec) "")
          for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     identifier)
          when (and (non-empty-string-p trimmed)
                    (nous-provider--model-identifier-usable-p trimmed)
                    (not (member trimmed seen :test #'string=)))
            do (push trimmed seen)
            and collect (model-spec-rename spec trimmed))))


;;;; -- Nous Messages Transport --

(-> nous-messages--request-headers (oauth-credentials) list)
(defun nous-messages--request-headers (credentials)
  "Return bearer-authenticated headers for Nous's native Messages route."
  (openai-compatible--authenticated-headers
   credentials
   :accept       "text/event-stream"
   :content-type "application/json"
   :custom       (list (cons "anthropic-version" *anthropic-api-version*))))

(defmethod provider-open-response-stream
    ((provider nous-messages-provider)
     (request hash-table)
     &key credentials conversation)
  "Open one bearer-authenticated streaming Nous Messages request."
  (declare (type oauth-credentials credentials)
           (type conversation conversation)
           (ignore provider conversation))
  (provider-post-event-stream
   (nous-messages-endpoint)
   (json-encode-utf8 request)
   :headers (nous-messages--request-headers credentials)))
