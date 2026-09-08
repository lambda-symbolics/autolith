(in-package #:autolith)

;;;; -- OpenAI-Compatible Chat Completions Provider --

(defclass openai-compatible-provider
    (session-preserving-provider-mixin chat-completions-provider)
  ((display-name
    :initarg :display-name
    :reader openai-compatible-provider-display-name
    :type non-empty-string
    :documentation "The user-visible provider name.")
   (family
    :initarg :family
    :reader openai-compatible-provider-family
    :type keyword
    :documentation "The provider family used for conversation projection.")
   (headers
    :initarg :headers
    :initform nil
    :reader openai-compatible-provider-headers
    :type list
    :documentation "Additional non-secret HTTP headers for provider requests.")
   (reasoning-parameter
    :initarg :reasoning-parameter
    :initform nil
    :reader openai-compatible-provider-reasoning-parameter
    :type (option string)
    :documentation "The optional request field receiving the reasoning effort.")
   (stream-usage-p
    :initarg :stream-usage-p
    :initform t
    :reader openai-compatible-provider-stream-usage-p
    :type boolean
    :documentation "Whether streaming requests ask the provider to report usage."))
  (:documentation
   "A provider for the streaming OpenAI Chat Completions wire protocol."))

(defmethod provider-output-ceiling-p ((provider openai-compatible-provider))
  "Chat Completions accepts max_completion_tokens."
  (declare (ignore provider))
  t)

(-> openai-compatible-provider-output-ceiling-field
    (openai-compatible-provider)
    non-empty-string)
(defgeneric openai-compatible-provider-output-ceiling-field (provider)
  (:documentation "Return the request field carrying PROVIDER's output limit."))

(defmethod openai-compatible-provider-output-ceiling-field
    ((provider openai-compatible-provider))
  "Use the modern OpenAI-compatible output limit field."
  (declare (ignore provider))
  "max_completion_tokens")

(defmethod provider-account-label ((provider openai-compatible-provider))
  "Return the configured OpenAI-compatible provider name."
  (openai-compatible-provider-display-name provider))

(defmethod provider-family ((provider openai-compatible-provider))
  "Return the configured OpenAI-compatible conversation family."
  (openai-compatible-provider-family provider))

(-> openai-compatible-provider-create
    (configuration &key
                   (:name non-empty-string)
                   (:family keyword)
                   (:headers list)
                   (:reasoning-parameter (option string))
                   (:stream-usage-p boolean))
    openai-compatible-provider)
(defun openai-compatible-provider-create
    (configuration &key name family headers reasoning-parameter
                        (stream-usage-p t))
  "Create one OpenAI-compatible provider from registered endpoint metadata."
  (make-instance
   'openai-compatible-provider
   :configuration configuration
   :credential-manager
   (api-key-credential-manager-create
    :provider-name name
    :pathname (configuration-api-keys-path configuration))
   :session-id (make-identifier)
   :display-name name
   :family family
   :headers (copy-tree headers)
   :reasoning-parameter reasoning-parameter
   :stream-usage-p stream-usage-p))

(defmethod provider-reconfiguration-initargs append
    ((provider openai-compatible-provider))
  "Preserve PROVIDER's display, family, headers, reasoning, and usage options."
  (list :display-name (openai-compatible-provider-display-name provider)
        :family (openai-compatible-provider-family provider)
        :headers (copy-tree (openai-compatible-provider-headers provider))
        :reasoning-parameter
        (openai-compatible-provider-reasoning-parameter provider)
        :stream-usage-p
        (openai-compatible-provider-stream-usage-p provider)))

(-> openai-compatible--authenticate
    (openai-compatible-provider &key
                                (:stream stream)
                                (:open-browser-p boolean))
    string)
(defun openai-compatible--authenticate (provider &key stream open-browser-p)
  "Interactively store or replace PROVIDER's private API key."
  (declare (ignore open-browser-p))
  (let* ((manager (provider-credential-manager provider))
         (source (credential-manager-primary-source manager))
         (provider-name (provider-account-label provider)))
    (call-with-secret-use
     (lambda ()
        (let ((api-key (api-key-read-hidden
                        provider-name
                        :stream (or stream *standard-output*))))
         (setf api-key
               (and api-key
                    (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 api-key)))
         (unless (non-empty-string-p api-key)
           (error 'credentials-unavailable
                  :message
                  (format nil "No ~A API key was entered; ~A."
                          provider-name
                          (credential-manager-login-hint manager))
                  :searched-paths
                  (list (credential-source-pathname source))))
         (api-key-credential-manager-save-key manager api-key)
         (format nil
                 "~A API key was saved in Autolith's private credential store."
                 provider-name))))))

(defmethod provider-authenticate ((provider openai-compatible-provider)
                                  &key stream open-browser-p)
  "Authenticate PROVIDER with its private API key."
  (openai-compatible--authenticate provider
                                   :stream stream
                                   :open-browser-p open-browser-p))


;;;; -- Model Discovery --

(-> openai-compatible--authenticated-headers
    (oauth-credentials &key
                       (:accept non-empty-string)
                       (:content-type (option string))
                       (:custom list))
    list)
(defun openai-compatible--authenticated-headers
    (credentials &key accept content-type custom)
  "Return authenticated JSON HTTP headers with CUSTOM overrides."
  (let ((default
          (remove nil
                  (list
                   (cons "Authorization"
                         (format nil "Bearer ~A"
                                 (oauth-credentials-access-token credentials)))
                   (and content-type (cons "Content-Type" content-type))
                   (cons "Accept" accept)
                   (cons "User-Agent" (provider-user-agent)))))
        (reserved '("authorization" "content-type" "accept" "user-agent")))
    (append
     default
     (remove-if
      (lambda (header)
        (member (string-downcase (first header)) reserved :test #'string=))
      (copy-tree custom)))))

(-> openai-compatible--decode-model-list
    (string &key (:entry-predicate (option function)))
    list)
(defun openai-compatible--decode-model-list (body &key entry-predicate)
  "Decode and optionally filter an OpenAI-compatible model-list response."
  (let* ((decoded
           (handler-case
               (json-decode body)
             (error ()
               (error 'configuration-error
                      :message "The model discovery response was not valid JSON."))))
         (data (and (json-object-p decoded)
                    (json-get decoded "data"))))
    (unless (vectorp data)
      (error 'configuration-error
             :message "The model discovery response did not contain a data array."))
    (let ((models nil))
      (loop for entry across data
            for identifier = (and (json-object-p entry)
                                  (json-get entry "id"))
            do (unless (non-empty-string-p identifier)
                 (error 'configuration-error
                        :message
                        "The model discovery response contained an invalid model entry."))
               (when (or (null entry-predicate)
                         (funcall entry-predicate entry))
                 (push identifier models)))
      (nreverse models))))

(-> openai-compatible--signal-model-discovery-status
    (non-empty-string credential-manager integer)
    null)
(defun openai-compatible--signal-model-discovery-status
    (provider-name manager status)
  "Signal the typed model-discovery failure for provider HTTP STATUS."
  (if (= status 401)
      (error 'authentication-error
             :message
             (format nil "~A rejected Autolith's ~A; ~A."
                     provider-name
                     (credential-manager-credential-description manager)
                     (credential-manager-login-hint manager)))
      (error 'configuration-error
             :message
             (format nil "The model discovery endpoint returned HTTP ~D."
                     status))))

(-> openai-compatible--fetch-models
    (configuration &key
                   (:provider-name non-empty-string)
                   (:endpoint non-empty-string)
                   (:headers list)
                   (:credential-manager (option credential-manager))
                   (:entry-predicate (option function)))
    list)
(defun openai-compatible--fetch-models
    (configuration &key provider-name endpoint headers credential-manager
                        entry-predicate)
  "Fetch and optionally filter models from one OpenAI-compatible endpoint.

When CREDENTIAL-MANAGER is supplied, use it instead of creating a default API-key
manager from PROVIDER-NAME."
  (let ((manager
          (or credential-manager
              (api-key-credential-manager-create
               :provider-name provider-name
               :pathname (configuration-api-keys-path configuration)))))
    (call-with-credentials
     manager
     (lambda (credentials)
       (multiple-value-bind (body status response-headers)
           (handler-case
              (provider-call-with-response-deadline
               30
               (lambda ()
                 (dexador:get
                  endpoint
                  :headers
                  (openai-compatible--authenticated-headers
                   credentials
                   :accept "application/json"
                   :custom headers)
                  :force-string t
                  :connect-timeout 10
                  :read-timeout 30)))
            (sb-sys:deadline-timeout ()
              (error 'configuration-error
                     :message
                     "The model discovery endpoint could not be reached."))
              (dexador.error:http-request-unauthorized (condition)
                (provider--error-body-text (response-body condition))
                (openai-compatible--signal-model-discovery-status
                 provider-name manager 401))
              (http-request-failed (condition)
                (provider--error-body-text (response-body condition))
                (let ((status (response-status condition)))
                  (if (integerp status)
                      (openai-compatible--signal-model-discovery-status
                       provider-name manager status)
                      (error 'configuration-error
                             :message
                             "The model discovery endpoint could not be reached."))))
             (error ()
               (error 'configuration-error
                      :message
                      "The model discovery endpoint could not be reached.")))
         (declare (ignore response-headers))
         (unless (and (integerp status) (<= 200 status 299))
           (openai-compatible--signal-model-discovery-status
            provider-name manager status))
          (openai-compatible--decode-model-list
           body
           :entry-predicate entry-predicate))))))


;;;; -- Provider Registration --

(-> register-openai-compatible-provider
    (&key
     (:name non-empty-string)
     (:description (option string))
     (:family (option keyword))
     (:models (option list))
     (:models-endpoint (option string))
     (:endpoint non-empty-string)
     (:headers list)
     (:reasoning-parameter (option string))
     (:stream-usage-p boolean)
     (:source keyword))
    string)
(defun register-openai-compatible-provider
    (&key name description family models models-endpoint endpoint
      headers reasoning-parameter (stream-usage-p t)
      (source (provider--current-registration-source)))
  "Register an OpenAI-compatible Chat Completions provider.

The provider resolves its bearer key from Autolith's private API-key store using
NAME. MODELS contains optional static strings or model property lists accepted by
REGISTER-PROVIDER. MODELS-ENDPOINT discovers additional model identifiers.
STREAM-USAGE-P controls whether streaming requests ask for a final usage chunk."
  (unless (or models models-endpoint)
    (error 'configuration-error
           :message
           (format nil
                   "OpenAI-compatible provider ~A needs :models or :models-endpoint."
                   name)))
  (when (and models-endpoint
             (not (non-empty-string-p models-endpoint)))
    (error 'configuration-error
           :message
           (format nil "Provider ~A has an invalid models endpoint."
                   name)))
  (dolist (header headers)
    (unless (and (consp header)
                 (non-empty-string-p (first header))
                 (stringp (rest header)))
      (error 'configuration-error
             :message
             (format nil
                     "Provider ~A has an invalid additional HTTP header ~S."
                     name header))))
  (let* ((effective-family (or family (provider--family-keyword name)))
         (model-discovery
           (and models-endpoint
                (lambda (configuration)
                  (openai-compatible--fetch-models
                   configuration
                   :provider-name name
                   :endpoint models-endpoint
                   :headers headers)))))
    (register-provider
     name
     :description description
     :family effective-family
     :models models
     :model-discovery model-discovery
     :model-discovery-endpoint models-endpoint
     :protocol ':chat-completions
     :endpoint endpoint
     :authenticator #'openai-compatible--authenticate
     :factory
     (lambda (configuration &key reasoning-summaries-p)
       (declare (ignore reasoning-summaries-p))
       (openai-compatible-provider-create
        configuration
        :name name
        :family effective-family
        :headers headers
        :reasoning-parameter reasoning-parameter
        :stream-usage-p stream-usage-p))
     :source source)))


(defmethod provider-request-object
           ((provider openai-compatible-provider) (conversation conversation)
            (tool-namespaces vector)
            &key goal-context compaction-p)
  "Project product history and context into a Chat Completions request."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
          (if compaction-p
              #()
              (openai-compatible--wire-tools
               (provider-request-tool-namespaces configuration tool-namespaces))))
         (delivery
          (unless compaction-p
            (context-resolve-request configuration conversation effective-tools
                                     :goal-context goal-context :compaction-p
                                     compaction-p)))
         (projection
          (make-instance 'cl-llm-provider-api::wire-request :model
                         (configuration-model configuration) :items
                         (conversation-input-items-for-family conversation
                                                              (provider-family
                                                               provider)
                                                              :include-ephemeral-p
                                                              (not compaction-p))
                         :prefix
                         (list (system-prompt configuration)
                               (and compaction-p *compaction-instructions*))
                         :suffix
                         (unless compaction-p
                           (list goal-context
                                 (and delivery (context-delivery-rendered delivery))))
                         :options
                         (list :stream-usage-p
                               (openai-compatible-provider-stream-usage-p provider)
                               :reasoning-parameter
                               (openai-compatible-provider-reasoning-parameter
                                provider)
                               :reasoning-effort
                               (configuration-reasoning-effort configuration)
                               :maximum-output-tokens
                               (and (provider-output-ceiling-p provider)
                                    *provider-maximum-output-tokens*)
                               :output-ceiling-field
                               (openai-compatible-provider-output-ceiling-field
                                provider)))))
    (values (provider-request-object provider projection effective-tools) delivery)))


;;;; -- Chat Completions Transport --

(-> openai-compatible--request-headers
    (openai-compatible-provider oauth-credentials conversation)
    list)
(defun openai-compatible--request-headers (provider credentials conversation)
  "Return authenticated headers for one Chat Completions request."
  (declare (ignore conversation))
  (openai-compatible--authenticated-headers
   credentials
   :accept "text/event-stream"
   :content-type "application/json"
   :custom (openai-compatible-provider-headers provider)))

(defmethod provider-open-response-stream
    ((provider openai-compatible-provider)
     (request hash-table)
     &key credentials conversation)
  "Open one authenticated streaming Chat Completions request."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (provider-call-with-response-deadline
   300
   (lambda ()
     (dexador:post
      (configuration-provider-endpoint (provider-configuration provider))
      :headers (openai-compatible--request-headers provider credentials conversation)
      :content (json-encode-utf8 request)
      :want-stream t
      :force-string t
      :keep-alive nil
      :connect-timeout 30
      :read-timeout 300))))
