(in-package #:autolith)

;;;; -- Vertex Local ADC Credentials --

(defclass vertex-credential-manager (credential-manager)
  ()
  (:documentation "Read-only local Google ADC with fresh tokens on every request."))

(defmethod credential-manager-provider-label ((manager vertex-credential-manager))
  "Name the Google Cloud credential service."
  (declare (ignore manager))
  "Google Cloud")

(defmethod credential-manager-refreshable-p ((manager vertex-credential-manager))
  "Do not retry rejected tokens; credentials are refreshed on every load."
  (declare (ignore manager))
  nil)

(-> vertex--token-request (string &key (:headers list) (:content list))
    (values string integer &optional))
(defun vertex--token-request (url &key headers content)
  "Exchange a Google refresh grant under the provider response deadline."
  (provider-call-with-response-deadline
   60
   (lambda ()
     (handler-case
         (multiple-value-bind (body status)
             (dexador:post url :headers headers :content content :force-string t
                              :keep-alive nil :connect-timeout 30 :read-timeout 60)
           (values body status))
       (dexador.error:http-request-failed (condition)
         (let ((body (dexador.error:response-body condition)))
           (when (streamp body) (close body :abort t)))
         (values "" (dexador.error:response-status condition)))))))

(defmethod credential-manager-load ((manager vertex-credential-manager))
  "Read local ADC and return a fresh token without retaining or saving credentials."
  (declare (ignore manager))
  (handler-case
      (cl-llm-provider-api:google-cloud-user-credentials
       (cl-llm-provider-api:google-cloud-default-credentials-path)
       #'vertex--token-request)
    (cl-llm-provider-api:google-cloud-authentication-error (condition)
      (error 'authentication-error :message (princ-to-string condition)))))

(defmethod call-with-credentials
    ((manager vertex-credential-manager) function &key force-refresh)
  "Call FUNCTION with a newly refreshed token."
  (declare (ignore force-refresh))
  (call-with-secret-use
   (lambda ()
     (funcall function (credential-manager-load manager)))))


;;;; -- Vertex Product Adapters --

(defclass vertex-product-provider (session-preserving-provider-mixin subscription-provider)
  ()
  (:documentation "Vertex provider with Autolith configuration and local ADC."))

(defclass vertex-gemini-provider
    (gemini-product-provider-mixin vertex-product-provider
     cl-llm-provider-api:vertex-gemini-provider)
  ()
  (:documentation "Gemini through Google Vertex AI with local user ADC."))

(defclass vertex-anthropic-provider
    (anthropic-product-provider-mixin vertex-product-provider
     cl-llm-provider-api:vertex-anthropic-provider)
  ()
  (:documentation "Claude through Google Vertex AI with local user ADC."))

(defmethod provider-account-label ((provider vertex-product-provider))
  "Name the shared Vertex account service."
  (declare (ignore provider))
  "Google Vertex AI")

(defmethod provider-family ((provider vertex-gemini-provider))
  "Select Gemini history projection."
  (declare (ignore provider))
  ':gemini)

(defmethod provider-family ((provider vertex-anthropic-provider))
  "Select Anthropic history projection."
  (declare (ignore provider))
  ':anthropic)

(-> vertex--routing-initargs (configuration) list)
(defun vertex--routing-initargs (configuration)
  "Read Vertex routing options without loading credentials."
  (let ((project (config :vertex-project configuration))
        (quota   (config :vertex-quota-project configuration)))
    (unless (non-empty-string-p project)
      (error 'configuration-error
             :message "Set GOOGLE_CLOUD_PROJECT or :vertex-project for Vertex AI."))
    (list :project project :location (config :vertex-location configuration)
          :quota-project (and (non-empty-string-p quota) quota))))

(-> vertex--provider-create (configuration symbol) vertex-product-provider)
(defun vertex--provider-create (configuration class)
  "Create a Vertex provider without retaining credentials."
  (apply #'make-instance class
         :configuration      configuration
         :credential-manager (make-instance 'vertex-credential-manager)
         :session-id         (make-identifier)
         (vertex--routing-initargs configuration)))

(-> vertex-gemini-provider-create
    (configuration &key (:reasoning-summaries-p boolean)) vertex-gemini-provider)
(defun vertex-gemini-provider-create (configuration &key reasoning-summaries-p)
  "Create the registered Vertex Gemini transport."
  (declare (ignore reasoning-summaries-p))
  (vertex--provider-create configuration 'vertex-gemini-provider))

(-> vertex-anthropic-provider-create
    (configuration &key (:reasoning-summaries-p boolean)) vertex-anthropic-provider)
(defun vertex-anthropic-provider-create (configuration &key reasoning-summaries-p)
  "Create the registered Vertex Claude transport."
  (declare (ignore reasoning-summaries-p))
  (vertex--provider-create configuration 'vertex-anthropic-provider))

(defmethod provider-with-configuration
    ((provider vertex-product-provider) (configuration configuration))
  "Apply updated Vertex routing while preserving session identity."
  (apply #'make-instance (class-of provider)
         :configuration configuration
         (append (vertex--routing-initargs configuration)
                 (provider-reconfiguration-initargs provider))))

(defmethod provider-open-response-stream
    ((provider vertex-product-provider) (request hash-table) &key credentials conversation)
  "Open the library's native Vertex transport inside the product request scope."
  (declare (ignore conversation))
  (cl-llm-provider-api:vertex-open-response-stream
   provider request :credentials credentials))

(defmethod provider-authenticate
    ((provider vertex-product-provider) &key stream open-browser-p)
  "Validate existing local ADC without copying it into Autolith's state."
  (declare (ignore stream open-browser-p))
  (with-credentials (credentials (provider-credential-manager provider))
    (declare (ignore credentials))
    "Google Cloud ADC authentication succeeded."))
