(in-package #:autolith)

;;;; -- Anthropic Account Adapter --

(defclass anthropic-product-provider-mixin ()
  ()
  (:documentation "Autolith history and context projection for Messages transports."))

(defclass anthropic-api-key-provider
    (anthropic-product-provider-mixin session-preserving-provider-mixin
     subscription-provider cl-llm-provider-api:anthropic-messages-provider)
  ()
  (:documentation "A static API key client for the Anthropic Messages API."))

(defmethod provider-account-label ((provider anthropic-api-key-provider))
  "Name the Anthropic account service in user-visible failures."
  (declare (ignore provider))
  "Anthropic")

(defmethod provider-family ((provider anthropic-api-key-provider))
  "The Anthropic provider serves the Anthropic model family."
  (declare (ignore provider))
  ':anthropic)

(defmethod provider-family-create
    ((family (eql ':anthropic))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Anthropic API key provider; reasoning stays provider-internal."
  (declare (ignore reasoning-summaries-p))
  (anthropic-provider-create configuration))

(-> anthropic-provider-create (configuration) anthropic-api-key-provider)
(defun anthropic-provider-create (configuration)
  "Create the Anthropic API key provider for CONFIGURATION."
  (make-instance 'anthropic-api-key-provider
                 :configuration      configuration
                 :credential-manager (anthropic-credential-manager-create
                                      configuration)
                 :session-id         (make-identifier)))

(defmethod provider-authenticate ((provider anthropic-api-key-provider)
                                  &key stream open-browser-p)
  "Prompt for, validate, and save the Anthropic API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


;;;; -- Anthropic Product Request Projection --

(defmethod provider-request-object
    ((provider anthropic-product-provider-mixin) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Project product prompts and history into the shared Anthropic wire request.
Return the encoded request and its unconsumed context delivery."
  (let* ((configuration   (provider-configuration provider))
         (request-namespaces
           (if compaction-p
               #()
               (provider-request-tool-namespaces configuration tool-namespaces)))
         (effective-tools (provider-wire-tools provider request-namespaces))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation request-namespaces
                                      :goal-context goal-context)))
         (durable-items
           (conversation-input-items-for-family
            conversation (provider-family provider) :include-ephemeral-p nil))
         (ephemeral-items
           (unless compaction-p
             (remove-if
              (lambda (item) (member item durable-items :test #'eq))
              (conversation-input-items-for-family
               conversation (provider-family provider) :include-ephemeral-p t))))
         (projection
           ;; Anchored context joins the durable history before its cache
           ;; marker; the goal and the rest of the context follow it.
           (let ((items (context-delivery-project-items delivery conversation durable-items)))
             (make-instance
              'cl-llm-provider-api:wire-request
              :model   (config :model configuration)
              :items   items
              :prefix  (append
                        (list (let ((*system-prompt-hosted-web-search-p* nil))
                                (system-prompt configuration)))
                        (when compaction-p (list *compaction-instructions*)))
              :suffix  (remove-if-not
                        #'non-empty-string-p
                        (list (and (not compaction-p) goal-context)
                              (context-delivery-rendered-tail delivery)))
              :options (list :ephemeral-items       ephemeral-items
                             :tool-choice           *provider-tool-choice*
                             :maximum-output-tokens *provider-maximum-output-tokens*
                             :cache-p               (not compaction-p))))))
    (values (provider-request-object provider projection effective-tools)
            delivery)))


;;;; -- Anthropic Transport --

(-> anthropic--request-headers (oauth-credentials) list)
(defun anthropic--request-headers (credentials)
  "Return authenticated headers for one Anthropic Messages request."
  (list (cons "x-api-key" (oauth-credentials-access-token credentials))
        (cons "anthropic-version" *anthropic-api-version*)
        (cons "Content-Type" "application/json")
        (cons "Accept" "text/event-stream")
        (cons "User-Agent" (provider-user-agent))))

(defmethod provider-open-response-stream
    ((provider anthropic-api-key-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the Anthropic Messages API."
  (declare (type oauth-credentials credentials)
           (type conversation conversation)
           (ignorable conversation))
  (provider-post-event-stream
   (config :provider-endpoint (provider-configuration provider))
   (json-encode-utf8 request)
   :headers (anthropic--request-headers credentials)))
