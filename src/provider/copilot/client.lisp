(in-package #:autolith)

;;;; -- Copilot Model Discovery --

(defparameter *copilot-enable-models-p* nil
  "Whether explicit Copilot authentication may enable unconfigured models.")

(defparameter *copilot-auto-request* nil
  "The wire adapter and Auto token dynamically owned by one request only.")

(-> copilot--catalog-path (configuration) pathname)
(defun copilot--catalog-path (configuration)
  "Return the private route catalog accompanying the shared model cache."
  (merge-pathnames "copilot-models.sexp" (config :cache-root configuration)))

(-> copilot--enable-model (string string string) boolean)
(defun copilot--enable-model (base-url token model)
  "Enable an unconfigured picker model during explicit login, if permitted."
  (handler-case
      (multiple-value-bind (body status)
          (provider-call-with-response-deadline
           10 (lambda ()
                (dexador:post
                 (format nil "~A/models/~A/policy" base-url (quri:url-encode model))
                 :headers (append (copilot--headers token)
                                  (list (cons "Content-Type" "application/json")
                                        (cons "openai-intent" "chat-policy")
                                        (cons "x-interaction-type" "chat-policy")))
                 :content "{\"state\":\"enabled\"}"
                 :max-redirects 0 :connect-timeout 5 :read-timeout 10)))
        (declare (ignore body))
        (and (integerp status) (<= 200 status 299) t))
    (error () nil)))

(-> copilot--fetch-models (configuration &key (:enable-p boolean)) list)
(defun copilot--fetch-models (configuration &key (enable-p *copilot-enable-models-p*))
  "Discover picker models and Auto, preserving hidden routes for automatic selection."
  (with-credentials (credentials (copilot-credential-manager-create configuration))
    (let* ((base-url (copilot--base-url credentials))
           (token (oauth-credentials-access-token credentials))
           (document (copilot--get-json
                      (concatenate 'string base-url "/models") token
                      :headers (list (cons "X-GitHub-Api-Version" "2026-06-01"))))
           (models (copilot-model-catalog
                    document :model-prefix "copilot/" :include-auto-p t
                    :enable-model-function
                    (when enable-p
                      (lambda (model) (copilot--enable-model base-url token model))))))
      (snapshot-write (copilot--catalog-path configuration)
                      (list :domain (copilot--domain) :models models
                            :document (json-encode document))
                      :mode #o600)
      models)))

(-> copilot--cached-protocol (configuration) keyword)
(defun copilot--cached-protocol (configuration)
  "Return the selected model's validated cached route without a network request."
  (block nil
    (unless (uiop:string-prefix-p "copilot/" (config :model configuration))
      (return ':chat-completions))
    (handler-case
        (let* ((path (copilot--catalog-path configuration))
               (catalog (and (probe-file path) (read-portable-form path)))
               (spec (and (equal (getf catalog :domain) (copilot--domain))
                          (find (config :model configuration) (getf catalog :models)
                                :key (lambda (model) (getf model :name)) :test #'string=)))
               (protocol (and spec (getf spec :protocol))))
          (unless (member protocol '(:auto :chat-completions :messages :responses))
            (error 'configuration-error
                   :message "Copilot route cache is missing or invalid; refresh with (models) or autolith auth copilot."))
          protocol)
      (configuration-error (condition) (error condition))
      (error ()
        (error 'configuration-error
               :message "Copilot route cache is invalid; refresh with (models) or autolith auth copilot.")))))


;;;; -- Account Adapters --

(defclass copilot-provider-mixin () ()
  (:documentation "Copilot account behavior shared across its three wire protocols."))

(defclass copilot-chat-provider (copilot-provider-mixin openai-compatible-provider) ()
  (:documentation "Copilot Chat Completions account adapter."))

(defclass copilot-messages-provider (copilot-provider-mixin anthropic-api-key-provider) ()
  (:documentation "Copilot bearer-authenticated Messages account adapter."))

(defclass copilot-responses-provider
    (copilot-provider-mixin session-preserving-provider-mixin responses-api-provider) ()
  (:documentation "Copilot Responses account adapter without ChatGPT extensions."))

(-> copilot-provider-protocol (copilot-provider-mixin) keyword)
(defgeneric copilot-provider-protocol (provider)
  (:documentation "Return the wire protocol used by PROVIDER's account adapter."))

(defmethod copilot-provider-protocol ((provider copilot-chat-provider))
  "Use Chat Completions for this adapter."
  (declare (ignore provider))
  ':chat-completions)

(defmethod copilot-provider-protocol ((provider copilot-messages-provider))
  "Use Messages for this adapter."
  (declare (ignore provider))
  ':messages)

(defmethod copilot-provider-protocol ((provider copilot-responses-provider))
  "Use Responses for this adapter."
  (declare (ignore provider))
  ':responses)

(defmethod provider-account-label ((provider copilot-provider-mixin))
  "Name the Copilot account service."
  (declare (ignore provider))
  "GitHub Copilot")

(defmethod provider-family ((provider copilot-provider-mixin))
  "Keep Copilot history independent of the upstream model vendor."
  (declare (ignore provider))
  ':copilot)

(-> copilot-provider--make
    (configuration credential-manager string
     &key (:registration (option provider-registration)) (:protocol (option keyword)))
    model-provider)
(defun copilot-provider--make (configuration manager session-id &key registration protocol)
  "Construct the wire adapter with shared credentials and session identity."
  (let* ((protocol (or protocol
                       (if *provider-authentication-bootstrap-p*
                           ':chat-completions
                           (copilot--cached-protocol configuration))))
         (class (ecase protocol
                  (:auto 'copilot-auto-provider)
                  (:chat-completions 'copilot-chat-provider)
                  (:messages 'copilot-messages-provider)
                  (:responses 'copilot-responses-provider))))
    (apply #'make-instance class
           :configuration configuration :credential-manager manager
           :registration registration :session-id session-id
           (when (member protocol '(:auto :chat-completions))
             (list :display-name "GitHub Copilot" :family ':copilot
                   :stream-usage-p nil :reasoning-parameter nil)))))

(-> copilot-provider-create (configuration &key (:reasoning-summaries-p boolean)) model-provider)
(defun copilot-provider-create (configuration &key reasoning-summaries-p)
  "Create a Copilot account adapter using the shared streaming implementations."
  (declare (ignore reasoning-summaries-p))
  (copilot-provider--make configuration (copilot-credential-manager-create configuration)
                          (make-identifier)))

(defmethod provider-wire-tool-name
    ((provider copilot-responses-provider) (namespace string) (name string))
  "Encode local names for Copilot's grammar-safe Responses function names."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

(defmethod provider-normalize-output-item
    ((provider copilot-responses-provider) (item hash-table))
  "Restore a completed Responses tool call to its local namespace for replay."
  (call-next-method)
  (when (function-call-item-p item)
    (multiple-value-bind (namespace name)
        (provider-wire-function-name--decode (json-get item "name"))
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name))))
  item)

(defmethod provider-responses-wire-effort ((provider copilot-responses-provider) configuration)
  "Send a reasoning effort only when the selected model advertises support."
  (declare (ignore provider))
  (let ((effort (configuration-wire-effort configuration)))
    (unless (equal effort "none") effort)))

(defmethod provider-with-configuration ((provider copilot-provider-mixin) (configuration configuration))
  "Switch wire adapters while preserving the account manager and session identity."
  (copilot-provider--make configuration (provider-credential-manager provider)
                          (provider-session-id provider)
                          :registration (model-provider-registration provider)))

(-> copilot-provider-authenticate
    (model-provider &key (:stream stream) (:open-browser-p boolean)) string)
(defun copilot-provider-authenticate (provider &key stream open-browser-p)
  "Authorize GitHub and save credentials before the shared post-login discovery."
  (device-authentication-login
   (copilot-device-authentication-client-create)
   (provider-credential-manager provider)
   :stream (or stream *standard-output*) :open-browser-p open-browser-p)
  "GitHub Copilot authentication was saved by Autolith.")

(defmethod provider-authenticate :around
    ((provider copilot-provider-mixin) &key stream open-browser-p)
  "Allow policy enablement during the single shared post-login model refresh."
  (declare (ignore stream open-browser-p))
  (let ((*copilot-enable-models-p* t))
    (call-next-method)))

(defmethod provider-authenticated-model
    ((configuration configuration) (provider copilot-provider-mixin))
  "Select a discovered Copilot model after login, preserving an eligible selection."
  (let* ((previous (model-provider-registration provider))
         (registration (and previous
                            (provider-registration-find
                             (provider-registration-name previous)))))
    (when registration
      (let* ((models (loop for model in (provider-registration-models registration)
                          for name = (provider-model-name model)
                          when (eq (provider-registration-for-model name) registration)
                            collect name))
             (current (config :model configuration)))
        (unless models
          (error 'configuration-error
                 :message "Authentication was saved, but provider copilot has no available models. Enable a tool-capable model for this account and authenticate again."))
        (or (find current models :test #'string=) (first models))))))

(defmethod provider-request-object :around
    ((provider copilot-provider-mixin) (conversation conversation) (tools vector)
     &key goal-context compaction-p)
  "Remove the local model namespace only at the wire boundary."
  (declare (ignore goal-context compaction-p))
  (let ((model (config :model (provider-configuration provider))))
    (unless (uiop:string-prefix-p "copilot/" model)
      (error 'configuration-error :message "A Copilot request requires a copilot/ model."))
    (multiple-value-bind (request delivery) (call-next-method)
      (setf (gethash "model" request) (subseq model (length "copilot/")))
      (values request delivery))))

(-> copilot--request-headers
    (oauth-credentials conversation json-object
     &key (:protocol keyword) (:session-token (option string))) list)
(defun copilot--request-headers
    (credentials conversation request &key (protocol ':chat-completions) session-token)
  "Add account credentials and distinguish user input from agent continuations."
  (let* ((last-item (first (last (conversation-input-items-for-family conversation ':copilot))))
         (initiator (if (or (null last-item) (equal (json-get last-item "role") "user"))
                        "user" "agent")))
    (append (copilot-stream-headers
             (oauth-credentials-access-token credentials) request
             :protocol protocol :initiator initiator
             :user-agent (provider-user-agent)
             :anthropic-version *anthropic-api-version*)
            (when session-token
              (list (cons "Copilot-Session-Token" session-token))))))

(defmethod provider-open-response-stream
    ((provider copilot-provider-mixin) (request hash-table) &key credentials conversation)
  "Open the current token's endpoint, including after credential renewal."
  (let ((protocol (copilot-provider-protocol provider)))
    (provider-post-event-stream
     (concatenate 'string (copilot--base-url credentials) (copilot-protocol-endpoint protocol))
     (json-encode-utf8 request)
     :headers (copilot--request-headers
               credentials conversation request :protocol protocol
               :session-token (when (eq provider (first *copilot-auto-request*))
                                (second *copilot-auto-request*))))))

(-> copilot--models-cache-key () string)
(defun copilot--models-cache-key ()
  "Partition account catalogs by the configured GitHub host."
  (format nil "https://api.~A/copilot/models" (copilot--domain)))

(register-provider
 "copilot" :description "GitHub Copilot subscription"
 :family ':copilot :protocol ':custom
 :factory #'copilot-provider-create :authenticator #'copilot-provider-authenticate
 :model-discovery #'copilot--fetch-models
 :model-discovery-endpoint "https://api.individual.githubcopilot.com/models"
 :model-discovery-endpoint-resolver #'copilot--models-cache-key
 :source ':builtin)
