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

(defmethod provider-prompt-cache-retention-estimate
    ((provider anthropic-api-key-provider))
  "Estimate Anthropic cache lifetime from the requested TTL only."
  (let ((ttl (config :prompt-cache-ttl (provider-configuration provider))))
    (if (stringp ttl)
        (cond
          ((string= ttl "5m")
           300)
          ((string= ttl "1h")
           3600)
          (t
           nil))
        nil)))

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

(defparameter *anthropic-model-prices*
  '(("claude-fable-5-1"  10 1/4  50)
    ("claude-fable-5"    10 1    50)
    ("claude-opus-5-5"   4  1/5  20)
    ("claude-opus-5"     5  1/2  25)
    ("claude-opus-4-8"   5  1/2  25)
    ("claude-opus-4-7"   5  1/2  25)
    ("claude-opus-4-6"   5  1/2  25)
    ("claude-sonnet-5-5" 2  1/5  10)
    ("claude-sonnet-5"   2  1/5  10)
    ("claude-sonnet-4-6" 3  3/10 15)
    ("claude-haiku-4-5"  1  1/10 5))
  "Anthropic API list prices as (MODEL INPUT CACHE-READ OUTPUT), US dollars per million tokens.

Taken from Anthropic's published prices as of 2026-10-06. Models priced by
prompt length, such as Claude Haiku 5.5, are left to the model prices setting.")

(defmethod provider-model-price
    ((provider anthropic-api-key-provider) (configuration configuration))
  "Use configured prices, else Anthropic's list prices for the requested cache lifetime.

A cache write costs 1.25 times input for five minutes and twice input for one hour."
  (or (call-next-method)
      (let ((entry (assoc (config :model configuration) *anthropic-model-prices*
                          :test #'string=)))
        (when entry
          (destructuring-bind (model input cache-read output) entry
            (declare (ignore model))
            (make-model-price :input       input
                              :cache-read  cache-read
                              :cache-write (* input (if (provider-prompt-cache-ttl configuration)
                                                        2
                                                        5/4))
                              :output      output
                              :source      "Anthropic list prices, 2026-10-06"))))))

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
           (multiple-value-bind (shared items)
               (provider-shared-items-split
                (context-delivery-project-items delivery conversation durable-items)
                durable-items)
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
              :options (list :shared-items          shared
                             :ephemeral-items       ephemeral-items
                             :tool-choice           *provider-tool-choice*
                             :maximum-output-tokens *provider-maximum-output-tokens*
                             :cache-p               (not compaction-p)
                             :cache-ttl             (provider-prompt-cache-ttl
                                                     configuration))))))
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
