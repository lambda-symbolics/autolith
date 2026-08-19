(in-package #:autolith)

;;;; -- Provider Wire Protocols --

(defclass responses-api-provider (subscription-provider)
  ()
  (:documentation
   "A provider using the standard streaming Responses API wire protocol."))

(defclass chat-completions-provider (subscription-provider)
  ()
  (:documentation
   "A provider using the streaming Chat Completions wire protocol."))

(-> provider-wire-protocol (model-provider) keyword)
(defgeneric provider-wire-protocol (provider)
  (:documentation "Return the wire protocol family implemented by PROVIDER."))

(defmethod provider-wire-protocol ((provider model-provider))
  "Identify a provider without a declared shared wire protocol."
  (declare (ignore provider))
  ':custom)

(defmethod provider-wire-protocol ((provider codex-subscription-provider))
  "Identify the Codex provider's Responses Lite dialect."
  (declare (ignore provider))
  ':responses-lite)

(defmethod provider-wire-protocol ((provider responses-api-provider))
  "Identify a standard streaming Responses API provider."
  (declare (ignore provider))
  ':responses-api)

(defmethod provider-wire-protocol ((provider chat-completions-provider))
  "Identify a streaming Chat Completions provider."
  (declare (ignore provider))
  ':chat-completions)

(-> provider-wire-tool-name (responses-api-provider string string) string)
(defgeneric provider-wire-tool-name (provider namespace name)
  (:documentation
   "Return NAME inside NAMESPACE encoded for PROVIDER's flat function namespace."))

(defmethod provider-wire-tool-name
    ((provider responses-api-provider) (namespace string) (name string))
  "Join a Responses API tool namespace and name with a dot."
  (declare (ignore provider))
  (format nil "~A.~A" namespace name))

(-> provider-wire-tool (responses-api-provider string json-object) json-object)
(defgeneric provider-wire-tool (provider namespace tool)
  (:documentation "Return namespaced TOOL encoded for PROVIDER's wire protocol."))

(defmethod provider-wire-tool
    ((provider responses-api-provider) (namespace string) (tool hash-table))
  "Encode one namespaced tool as a standard Responses function tool."
  (json-object
   "type" "function"
   "name" (provider-wire-tool-name provider namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "strict" false
   "parameters" (json-get tool "parameters")))

(-> provider-wire-tools (responses-api-provider vector) vector)
(defgeneric provider-wire-tools (provider tool-namespaces)
  (:documentation
   "Return TOOL-NAMESPACES encoded for PROVIDER's request protocol."))

(defmethod provider-wire-tools
    ((provider responses-api-provider) (tool-namespaces vector))
  "Flatten namespaced tools while retaining hosted tool declarations."
  (coerce
   (loop for entry across tool-namespaces
         if (and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (vectorp (json-get entry "tools"))
                 (non-empty-string-p (json-get entry "name")))
           append (loop for tool across (json-get entry "tools")
                        when (and (json-object-p tool)
                                  (non-empty-string-p
                                   (json-get tool "name")))
                          collect (provider-wire-tool
                                   provider
                                   (json-get entry "name")
                                   tool))
         else
           collect entry)
   'vector))

(-> provider-wire-input-item (responses-api-provider t) t)
(defgeneric provider-wire-input-item (provider item)
  (:documentation "Return conversation ITEM encoded for PROVIDER's wire protocol."))

(defmethod provider-wire-input-item ((provider responses-api-provider) item)
  "Flatten a namespaced function-call ITEM for a standard Responses request."
  (if (and (json-object-p item)
           (function-call-item-p item)
           (non-empty-string-p (json-get item "namespace"))
           (non-empty-string-p (json-get item "name")))
      (let ((copy (json-object-copy item)))
        (setf (gethash "name" copy)
              (provider-wire-tool-name
               provider
               (json-get item "namespace")
               (json-get item "name")))
        (remhash "namespace" copy)
        copy)
      item))

(-> provider-responses-wire-effort (responses-api-provider configuration) (option string))
(defgeneric provider-responses-wire-effort (provider configuration)
  (:documentation
   "Return CONFIGURATION's reasoning effort encoded for PROVIDER.
NIL means the serving stack rejects the reasoning parameter entirely;
the request builder then omits the reasoning object."))

(-> provider-responses-reasoning-summary
    (responses-api-provider configuration)
    (option string))
(defgeneric provider-responses-reasoning-summary (provider configuration)
  (:documentation
   "Return CONFIGURATION's reasoning summary style for PROVIDER, or NIL."))

(defmethod provider-responses-reasoning-summary
    ((provider responses-api-provider) (configuration configuration))
  "Request no reasoning summary style unless a concrete provider opts in."
  (declare (ignore provider configuration))
  nil)

(-> provider-responses-hosted-tools
    (responses-api-provider configuration)
    list)
(defgeneric provider-responses-hosted-tools (provider configuration)
  (:documentation
   "Return PROVIDER's server-executed hosted tool declarations for CONFIGURATION."))

(defmethod provider-responses-hosted-tools
    ((provider responses-api-provider) (configuration configuration))
  "Expose no hosted tools unless a concrete provider opts in."
  (declare (ignore provider configuration))
  nil)

(-> provider-responses-request-namespaces
    (responses-api-provider vector)
    vector)
(defgeneric provider-responses-request-namespaces (provider tool-namespaces)
  (:documentation
   "Return TOOL-NAMESPACES filtered to the local tools PROVIDER can serve."))

(defmethod provider-responses-request-namespaces
    ((provider responses-api-provider) (tool-namespaces vector))
  "Serve every local tool namespace unless a concrete provider excludes one."
  (declare (ignore provider))
  tool-namespaces)

(-> provider-responses-request-fields
    (responses-api-provider conversation)
    list)
(defgeneric provider-responses-request-fields (provider conversation)
  (:documentation
   "Return PROVIDER-specific alternating JSON fields for one Responses request."))

(defmethod provider-responses-request-fields
    ((provider responses-api-provider) (conversation conversation))
  "Add no provider-specific request fields by default."
  (declare (ignore provider conversation))
  nil)

(defmethod provider-normalize-output-item
    ((provider responses-api-provider) (item hash-table))
  "Strip server identifiers and restore flat wire calls to namespaced calls."
  (call-next-method)
  (when (function-call-item-p item)
    (let* ((name (json-get item "name"))
           (dot (and (stringp name) (position #\. name))))
      (when (and dot (plusp dot) (< (1+ dot) (length name)))
        (setf (gethash "namespace" item) (subseq name 0 dot)
              (gethash "name" item) (subseq name (1+ dot))))))
  item)

(defmethod provider-request-object
    ((provider responses-api-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build a standard stateless Responses API request for CONVERSATION.

Concrete providers specialize the reasoning effort, hosted tools, served
namespaces, and extra request fields. The second value is the context
delivery consumed only after a completed response."
  (let* ((configuration (provider-configuration provider))
         (hosted-tools
           (and (not compaction-p)
                (provider-responses-hosted-tools provider configuration)))
         (effective-namespaces
           (if compaction-p
               #()
               (concatenate 'vector
                            (provider-responses-request-namespaces
                             provider tool-namespaces)
                            (coerce hosted-tools 'vector))))
         (prefix
           (append
            (list (responses-lite-developer-message
                   (let ((*system-prompt-hosted-web-search-p*
                           (not (null hosted-tools))))
                     (system-prompt configuration))))
            (when (and goal-context (not compaction-p))
              (list (responses-lite-developer-message goal-context)))))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-namespaces
              :goal-context goal-context)))
         (context-message
           (and delivery
                (context-delivery-rendered delivery)
                (responses-lite-developer-message
                 (context-delivery-rendered delivery))))
         (input
           (coerce
            (append
             prefix
             (mapcar
              (lambda (item)
                (provider-wire-input-item provider item))
              (conversation-input-items-for-family
               conversation
               (provider-family provider)
               :include-ephemeral-p (not compaction-p)))
             (when context-message
               (list context-message))
             (when compaction-p
               (list (responses-lite-developer-message
                      *compaction-instructions*))))
            'vector))
         (tools (provider-wire-tools provider effective-namespaces)))
    (values
     (apply #'json-object
            (append
             (list "model" (configuration-model configuration)
                   "input" input
                   "tools" tools)
             (when (plusp (length tools))
               (list "tool_choice" "auto"))
             (list "parallel_tool_calls" false)
             ;; A NIL wire effort means the serving stack rejects the
             ;; reasoning parameter entirely; omit the reasoning object.
             (let ((effort
                     (provider-responses-wire-effort provider configuration))
                   (summary
                     (provider-responses-reasoning-summary
                      provider configuration)))
               (when effort
                 (list "reasoning"
                       (apply #'json-object
                              (append
                               (list "effort" effort)
                               (when summary
                                 (list "summary" summary)))))))
             (when (and *provider-maximum-output-tokens*
                        (provider-output-ceiling-p provider))
               (list "max_output_tokens" *provider-maximum-output-tokens*))
             (list "store" false
                   "stream" t)
             (provider-responses-request-fields provider conversation)))
     delivery)))
