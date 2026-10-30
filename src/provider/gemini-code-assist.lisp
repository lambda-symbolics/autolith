(in-package #:autolith)

;;;; -- Gemini Code Assist Protocol --

(defparameter *gemini-code-assist-endpoint*
  "https://cloudcode-pa.googleapis.com/v1internal"
  "Base endpoint for the private Gemini Code Assist subscription API.")

(defparameter *gemini-code-assist-models*
  '((:name           "gemini-3.1-flash-lite"
     :description    "Gemini 3.1 Flash Lite through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-3.1-pro-preview"
     :description    "Gemini 3.1 Pro Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-3-pro-preview"
     :description    "Gemini 3 Pro Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-3-flash-preview"
     :description    "Gemini 3 Flash Preview through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-3.5-flash"
     :description    "Gemini 3.5 Flash through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-2.5-pro"
     :description    "Gemini 2.5 Pro through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-2.5-flash"
     :description    "Gemini 2.5 Flash through a Google Code Assist subscription."
     :context-window 1048576)
    (:name           "gemini-2.5-flash-lite"
     :description    "Gemini 2.5 Flash Lite through a Google Code Assist subscription."
     :context-window 1048576))
  "Known Code Assist models exposed when no model-list RPC exists.")

(defparameter *gemini-code-assist-nonstream-maximum-attempts* 4
  "Maximum attempts for transient Code Assist non-streaming RPC failures.")

(defparameter *gemini-code-assist-nonstream-retry-delay* 1
  "Seconds between transient Code Assist non-streaming RPC attempts.")

(define-condition gemini-code-assist-error (provider-error)
  ()
  (:documentation "A Gemini Code Assist protocol operation failed."))

(define-condition gemini-code-assist-setup-error (gemini-code-assist-error)
  ((stage
    :initarg       :stage
    :reader        gemini-code-assist-setup-error-stage
    :type          keyword
    :documentation "The load, tier selection, onboarding, or operation stage."))
  (:documentation "Gemini Code Assist account setup could not complete."))

(define-condition gemini-code-assist-project-required
    (gemini-code-assist-setup-error)
  ()
  (:documentation "The Code Assist account requires a Google Cloud project."))

(define-condition gemini-code-assist-invalid-project
    (gemini-code-assist-setup-error)
  ((project
    :initarg       :project
    :reader        gemini-code-assist-invalid-project-project
    :type          non-empty-string
    :documentation "The invalid numeric Google Cloud project identifier."))
  (:documentation "A numeric project number was supplied where a project ID is required."))

(defclass gemini-code-assist-provider
    (session-preserving-provider-mixin subscription-provider
     gemini-generate-content-provider)
  ((endpoint
    :initarg       :endpoint
    :initform      *gemini-code-assist-endpoint*
    :reader        gemini-code-assist-provider-endpoint
    :type          non-empty-string
    :documentation "The v1internal Code Assist endpoint.")
   (project
    :initarg       :project
    :initform      nil
    :accessor      gemini-code-assist-provider-project
    :type          (option string)
    :documentation "The effective Code Assist companion project.")
   (tier
    :initarg       :tier
    :initform      nil
    :accessor      gemini-code-assist-provider-tier
    :type          (option string)
    :documentation "The effective Code Assist subscription tier identifier.")
   (setup-complete-p
    :initarg       :setup-complete-p
    :initform      nil
    :accessor      gemini-code-assist-provider-setup-complete-p
    :type          boolean
    :documentation "Whether loadCodeAssist and any onboarding have completed."))
  (:documentation "A direct Gemini CLI subscription provider for Code Assist."))

(defgeneric gemini-code-assist-credential-manager-create (configuration)
  (:documentation "Return the OAuth credential manager used by Gemini Code Assist."))

(defmethod gemini-code-assist-credential-manager-create
    ((configuration configuration))
  "Create the installed-application OAuth manager for Code Assist."
  (gemini-credential-manager-create configuration))

(-> gemini-code-assist-provider-create
    (configuration &key
                   (:credential-manager (option credential-manager))
                   (:endpoint non-empty-string))
    gemini-code-assist-provider)
(defun gemini-code-assist-provider-create
    (configuration &key credential-manager
                          (endpoint *gemini-code-assist-endpoint*))
  "Create a Code Assist provider."
  (make-instance
   'gemini-code-assist-provider
   :configuration      configuration
   :credential-manager (or credential-manager
                           (gemini-code-assist-credential-manager-create
                            configuration))
   :session-id         (make-identifier)
   :endpoint           endpoint))

(defmethod provider-account-label ((provider gemini-code-assist-provider))
  "Name the Google Code Assist account service."
  (declare (ignore provider))
  "Google Code Assist")

(defmethod provider-family ((provider gemini-code-assist-provider))
  "Use a dedicated family for Gemini wire history."
  (declare (ignore provider))
  ':gemini-code-assist)

(defmethod provider-reconfiguration-initargs append
    ((provider gemini-code-assist-provider))
  "Preserve Code Assist setup state and endpoint across reconfiguration."
  (list :endpoint (gemini-code-assist-provider-endpoint provider)
        :project  (gemini-code-assist-provider-project provider)
        :tier     (gemini-code-assist-provider-tier provider)
        :setup-complete-p
        (gemini-code-assist-provider-setup-complete-p provider)))

(-> gemini-code-assist-model-name (string) string)
(defun gemini-code-assist-model-name (name)
  "Resolve a Code Assist model alias NAME to its wire identifier."
  (autolith-supervisor:family-domain-gemini-model-name name))

(-> gemini-code-assist-discover-models (gemini-code-assist-provider) list)
(defun gemini-code-assist-discover-models (provider)
  "Return Code Assist's known model catalog.

The v1internal service used by Gemini CLI exposes no model-list RPC, so this
catalog follows the exact model identifiers consumed by streamGenerateContent."
  (declare (ignore provider))
  (copy-tree *gemini-code-assist-models*))


;;;; -- JSON RPC Transport --

(-> gemini-code-assist--headers (oauth-credentials non-empty-string) list)
(defun gemini-code-assist--headers (credentials accept)
  "Return authenticated Code Assist HTTP headers."
  (list (cons "Authorization"
              (format nil "Bearer ~A"
                      (oauth-credentials-access-token credentials)))
        (cons "Content-Type" "application/json")
        (cons "Accept" accept)
        (cons "User-Agent" (provider-user-agent))))

(-> gemini-code-assist--method-url
    (gemini-code-assist-provider non-empty-string) non-empty-string)
(defun gemini-code-assist--method-url (provider method)
  "Return PROVIDER's v1internal METHOD RPC URL."
  (format nil "~A:~A" (gemini-code-assist-provider-endpoint provider) method))

(-> gemini-code-assist--operation-url
    (gemini-code-assist-provider non-empty-string) non-empty-string)
(defun gemini-code-assist--operation-url (provider operation)
  "Return the long-running OPERATION URL."
  (format nil "~A/~A" (gemini-code-assist-provider-endpoint provider) operation))

(-> gemini-code-assist--condition-string (t) (option string))
(defun gemini-code-assist--condition-string (value)
  "Return wire VALUE as a credential-sanitized condition string, when valid."
  (and (stringp value)
       (provider--sanitize-wire-string value)))

(-> gemini-code-assist--condition-status (t) (option integer))
(defun gemini-code-assist--condition-status (value)
  "Return wire VALUE as a credential-sanitized provider status, when valid."
  (let ((sanitized (provider--sanitize-wire-value value)))
    (and (integerp sanitized) sanitized)))

(-> gemini-code-assist--decode-json-response (string keyword) json-object)
(defun gemini-code-assist--decode-json-response (body stage)
  "Decode BODY as a JSON object for setup STAGE."
  (let ((value
          (handler-case
              (json-decode body)
            (error ()
              (error 'gemini-code-assist-setup-error
                     :message    "Gemini Code Assist returned invalid JSON."
                     :stage      stage
                     :status     nil
                     :request-id nil
                     :response
                     (bounded-string
                      (provider--sanitize-wire-string body)
                      :limit 2000))))))
    (unless (json-object-p value)
      (error 'gemini-code-assist-setup-error
             :message    "Gemini Code Assist returned a non-object JSON response."
             :stage      stage
             :status     nil
             :request-id nil
             :response
             (bounded-string
              (provider--sanitize-wire-string body)
              :limit 2000)))
    value))

(-> gemini-code-assist--post-once
    (gemini-code-assist-provider oauth-credentials
     &key (:method non-empty-string) (:request json-object))
    (values string integer t))
(defun gemini-code-assist--post-once (provider credentials &key method request)
  "Perform one non-streaming Code Assist METHOD request."
  (handler-case
      (provider-call-with-response-deadline
       300
       (lambda ()
         (dexador:post
          (gemini-code-assist--method-url provider method)
          :headers         (gemini-code-assist--headers credentials "application/json")
          :content         (json-encode-utf8 request)
          :force-string    t
          :keep-alive      nil
          :connect-timeout 30
          :read-timeout    300)))
    (sb-sys:deadline-timeout (condition)
      (provider--signal-transport-failure
       (provider--transport-failure-message
        "The Gemini Code Assist response exceeded its deadline."
        condition)
       :retryable-p t))
    (http-request-failed (condition)
      (provider-signal-http-failure provider condition))))

(-> gemini-code-assist--get-once
    (gemini-code-assist-provider oauth-credentials non-empty-string)
    (values string integer t))
(defun gemini-code-assist--get-once (provider credentials operation)
  "Perform one non-streaming Code Assist long-running operation request."
  (handler-case
      (provider-call-with-response-deadline
       300
       (lambda ()
         (dexador:get
          (gemini-code-assist--operation-url provider operation)
          :headers         (gemini-code-assist--headers credentials "application/json")
          :force-string    t
          :keep-alive      nil
          :connect-timeout 30
          :read-timeout    300)))
    (sb-sys:deadline-timeout (condition)
      (provider--signal-transport-failure
       (provider--transport-failure-message
        "The Gemini Code Assist response exceeded its deadline."
        condition)
       :retryable-p t))
    (http-request-failed (condition)
      (provider-signal-http-failure provider condition))))

(-> gemini-code-assist--nonstream-request
    (gemini-code-assist-provider oauth-credentials
     &key (:stage keyword) (:request-function function))
    json-object)
(defun gemini-code-assist--nonstream-request
    (provider credentials &key stage request-function)
  "Run one setup RPC with Gemini CLI's bounded transient retry behavior.

A retryable HTTP status or transport failure is retried after
*GEMINI-CODE-ASSIST-NONSTREAM-RETRY-DELAY* seconds, up to
*GEMINI-CODE-ASSIST-NONSTREAM-MAXIMUM-ATTEMPTS* attempts; the last failure
propagates."
  (declare (ignore credentials))
  (call-with-bounded-retries
   (lambda ()
     (multiple-value-bind (body status headers) (funcall request-function)
       (if (<= 200 status 299)
           (gemini-code-assist--decode-json-response body stage)
           (provider--signal-http-status-failure
            provider status :headers headers :raw-body body))))
   (lambda (event) (declare (ignore event)) nil)
   :maximum-retries (1- *gemini-code-assist-nonstream-maximum-attempts*)
   :delay-function  (lambda (retry-number condition)
                      (declare (ignore retry-number condition))
                      *gemini-code-assist-nonstream-retry-delay*)))

(-> gemini-code-assist--post
    (gemini-code-assist-provider oauth-credentials
     &key (:method non-empty-string) (:stage keyword) (:request json-object))
    json-object)
(defun gemini-code-assist--post (provider credentials &key method stage request)
  "POST one non-streaming Code Assist RPC."
  (gemini-code-assist--nonstream-request
   provider credentials
   :stage            stage
   :request-function (lambda ()
                       (gemini-code-assist--post-once provider credentials
                                                      :method  method
                                                      :request request))))

(-> gemini-code-assist--operation
    (gemini-code-assist-provider oauth-credentials non-empty-string)
    json-object)
(defun gemini-code-assist--operation (provider credentials operation)
  "GET one Code Assist long-running OPERATION."
  (gemini-code-assist--nonstream-request
   provider credentials
   :stage            ':operation
   :request-function (lambda ()
                       (gemini-code-assist--get-once provider credentials operation))))


;;;; -- Account Setup --

(-> gemini-code-assist--environment-project () (option string))
(defun gemini-code-assist--environment-project ()
  "Return the configured Google Cloud project ID, if any."
  (or (let ((value (uiop:getenv "GOOGLE_CLOUD_PROJECT")))
        (and (non-empty-string-p value) value))
      (let ((value (uiop:getenv "GOOGLE_CLOUD_PROJECT_ID")))
        (and (non-empty-string-p value) value))))

(-> gemini-code-assist--project-required
    (json-object keyword) null)
(defun gemini-code-assist--project-required (load-response stage)
  "Signal a structured missing-project or ineligibility failure."
  (let* ((tiers      (json-get load-response "ineligibleTiers"))
         (first-tier (and (vectorp tiers) (plusp (length tiers)) (aref tiers 0)))
         (reason     (and (json-object-p first-tier)
                          (json-get first-tier "reasonMessage"))))
    (error 'gemini-code-assist-project-required
           :message
           (or (and (non-empty-string-p reason)
                    (gemini-code-assist--condition-string reason))
               "This Google account requires GOOGLE_CLOUD_PROJECT to be set.")
           :stage      stage
           :status     nil
           :request-id nil
           :response   nil)))

(-> gemini-code-assist--wait (real) null)
(defun gemini-code-assist--wait (seconds)
  "Wait between setup polls under the current host deadline."
  (sleep seconds)
  nil)

(-> gemini-code-assist-ensure-setup
    (gemini-code-assist-provider oauth-credentials)
    gemini-code-assist-provider)
(defun gemini-code-assist-ensure-setup (provider credentials)
  "Load Code Assist state through the shared account setup policy."
  (unless (gemini-code-assist-provider-setup-complete-p provider)
    (multiple-value-bind (project tier)
        (autolith-supervisor:family-domain-gemini-setup
         :project   (or (gemini-code-assist-provider-project provider)
                        (gemini-code-assist--environment-project))
         :rpc       (lambda (method stage request)
                      (gemini-code-assist--post provider credentials
                                                :method method :stage stage :request request))
         :operation (lambda (name)
                      (gemini-code-assist--operation provider credentials name))
         :wait      #'gemini-code-assist--wait
         :check     (lambda () nil)
         :fail      (lambda (reason &key stage project response)
                      (case reason
                        (:numeric-gemini-project
                         (error 'gemini-code-assist-invalid-project
                                :message (format nil "Google Cloud project ~A is numeric; Code Assist requires a project ID."
                                                 project)
                                :project project :stage stage :status nil
                                :request-id nil :response nil))
                        (:gemini-project-required
                         (gemini-code-assist--project-required response stage))
                        (otherwise
                         (error 'gemini-code-assist-setup-error
                                :message "Code Assist returned an invalid operation name."
                                :stage stage :status nil :request-id nil :response nil)))))
      (setf (gemini-code-assist-provider-project provider) project
            (gemini-code-assist-provider-tier provider) tier
            (gemini-code-assist-provider-setup-complete-p provider) t)))
  provider)


;;;; -- Request Projection --

(defmethod provider-request-object
    ((provider gemini-code-assist-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Project history and prompts through the shared GenerateContent encoding.

Code Assist wraps that request with its model, project, and prompt identifiers
and adds the session to it. Return the request and its context delivery."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
           (if compaction-p
               #()
               (provider-request-tool-namespaces configuration tool-namespaces)))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation effective-tools
                                      :goal-context goal-context
                                      :compaction-p compaction-p)))
         (projection
           (make-instance
            'cl-llm-provider-api:wire-request
            :model   (gemini-code-assist-model-name (config :model configuration))
            :items   (conversation-input-items-for-family
                      conversation (provider-family provider)
                      :include-ephemeral-p (not compaction-p))
            :prefix  (list (system-prompt configuration)
                           (and compaction-p *compaction-instructions*))
            :suffix  (unless compaction-p
                       (list goal-context
                             (and delivery (context-delivery-rendered delivery))))
            :options (list :maximum-output-tokens *provider-maximum-output-tokens*
                           :include-thoughts-p
                           (not (string= (config :reasoning-effort configuration)
                                         "none")))))
         (inner         (provider-request-object provider projection
                                                 (provider-wire-tools provider effective-tools))))
    (setf (gethash "session_id" inner) (provider-session-id provider))
    (values
     (json-object
      "model" (gemini-code-assist-model-name
               (config :model configuration))
      "project" (gemini-code-assist-provider-project provider)
      "user_prompt_id" (make-identifier)
      "request" inner)
     delivery)))


;;;; -- Streaming Transport and Decoding --

(defmethod provider-open-response-stream
    ((provider gemini-code-assist-provider)
     (request hash-table)
     &key credentials conversation)
  "Open one authenticated Code Assist streamGenerateContent SSE response."
  (declare (ignore conversation)
           (type oauth-credentials credentials))
  (gemini-code-assist-ensure-setup provider credentials)
  (setf (gethash "project" request)
        (gemini-code-assist-provider-project provider))
  (provider-post-event-stream
   (format nil "~A?alt=sse"
           (gemini-code-assist--method-url provider "streamGenerateContent"))
   (json-encode-utf8 request)
   :headers (gemini-code-assist--headers credentials "text/event-stream")))

(defmethod provider-gemini-stream-response
    ((provider gemini-code-assist-provider) event)
  "Unwrap one Code Assist stream EVENT into its response and trace identifier."
  (declare (ignore provider))
  (values (json-get event "response") (json-get event "traceId")))
