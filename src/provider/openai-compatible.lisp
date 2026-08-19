(in-package #:autolith)

;;;; -- OpenAI-Compatible Chat Completions Provider --

(defclass openai-compatible-provider (chat-completions-provider)
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
    :documentation "The optional request field receiving the reasoning effort."))
  (:documentation
   "A provider for the streaming OpenAI Chat Completions wire protocol."))

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
                  (:reasoning-parameter (option string)))
    openai-compatible-provider)
(defun openai-compatible-provider-create
    (configuration &key name family headers reasoning-parameter)
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
   :reasoning-parameter reasoning-parameter))

(defmethod provider-with-configuration
    ((provider openai-compatible-provider) (configuration configuration))
  "Copy PROVIDER for CONFIGURATION while retaining its key source and session."
  (make-instance
   'openai-compatible-provider
   :configuration configuration
   :credential-manager (provider-credential-manager provider)
   :session-id (provider-session-id provider)
   :registration (model-provider-registration provider)
   :display-name (openai-compatible-provider-display-name provider)
   :family (openai-compatible-provider-family provider)
   :headers (copy-tree (openai-compatible-provider-headers provider))
   :reasoning-parameter
   (openai-compatible-provider-reasoning-parameter provider)))


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

(-> openai-compatible--decode-model-list (string) list)
(defun openai-compatible--decode-model-list (body)
  "Decode an OpenAI-compatible model-list response into model identifiers."
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
               (push identifier models))
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
             (format nil "~A rejected Autolith's API key; ~A."
                     provider-name
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
                   (:credential-manager (option credential-manager)))
    list)
(defun openai-compatible--fetch-models
    (configuration &key provider-name endpoint headers credential-manager)
  "Fetch model identifiers from one OpenAI-compatible endpoint.

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
               (dexador:get
                endpoint
                :headers
                (openai-compatible--authenticated-headers
                 credentials
                 :accept "application/json"
                 :custom headers)
                :force-string t
                :connect-timeout 10
                :read-timeout 30)
             (dexador.error:http-request-unauthorized ()
               (openai-compatible--signal-model-discovery-status
                provider-name manager 401))
             (http-request-failed (condition)
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
         (openai-compatible--decode-model-list body))))))


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
     (:source keyword))
    string)
(defun register-openai-compatible-provider
    (&key name description family models models-endpoint endpoint
      headers reasoning-parameter
      (source (provider--current-registration-source)))
  "Register an OpenAI-compatible Chat Completions provider.

The provider resolves its bearer key from Autolith's private API-key store using
NAME. MODELS contains optional static strings or model property lists accepted by
REGISTER-PROVIDER. MODELS-ENDPOINT discovers additional model identifiers."
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
        :reasoning-parameter reasoning-parameter))
     :source source)))


;;;; -- Chat Completions Tool Encoding --

(defparameter *openai-compatible-wire-tool-name-maximum-length* 64
  "The maximum Chat Completions function name length accepted by Autolith.")

(-> openai-compatible--wire-tool-name (string string) string)
(defun openai-compatible--wire-tool-name (namespace name)
  "Return a reversible grammar-safe name for NAMESPACE and NAME.

The Chat Completions protocol does not accept dots in function names. A compact
URL-safe Base64 payload keeps the namespace separator unambiguous while using only
letters, digits, hyphens, and underscores on the wire."
  (let* ((payload
           (concatenate 'string namespace (string (code-char 0)) name))
         (encoded
           (string-right-trim
            "."
            (usb8-array-to-base64-string
             (sb-ext:string-to-octets payload :external-format ':utf-8)
             :uri t)))
         (wire-name (format nil "a~A" encoded)))
    (when (> (length wire-name) *openai-compatible-wire-tool-name-maximum-length*)
      (error 'configuration-error
             :message
             (format nil
                     "OpenAI-compatible tool name ~A.~A is too long for Chat Completions."
                     namespace name)))
    wire-name))

(-> openai-compatible--decode-wire-tool-name
    (string)
    (values (option string) (option string)))
(defun openai-compatible--decode-wire-tool-name (wire-name)
  "Decode a namespaced Chat Completions function name, if it is ours."
  (if (and (plusp (length wire-name))
           (char= (char wire-name 0) #\a))
      (handler-case
          (let* ((decoded
                   (base64-string-to-string
                    (padded-base64url (subseq wire-name 1))
                    :uri t))
                 (separator (position (code-char 0) decoded)))
            (if (and separator
                     (plusp separator)
                     (< (1+ separator) (length decoded)))
                (values (subseq decoded 0 separator)
                        (subseq decoded (1+ separator)))
                (values nil nil)))
        (error ()
          (values nil nil)))
      (values nil nil)))

(-> openai-compatible--wire-function (string json-object) json-object)
(defun openai-compatible--wire-function (name tool)
  "Return one Chat Completions function declaration for TOOL NAME."
  (json-object
   "name" name
   "description" (json-get tool "description")
   "parameters" (json-get tool "parameters")
   "strict" false))

(-> openai-compatible--wire-tool (string json-object) json-object)
(defun openai-compatible--wire-tool (namespace tool)
  "Return one namespaced Autolith TOOL as a Chat Completions function tool."
  (json-object
   "type" "function"
   "function"
   (openai-compatible--wire-function
    (openai-compatible--wire-tool-name namespace (json-get tool "name"))
    tool)))

(-> openai-compatible--standalone-wire-tool (json-object) (option json-object))
(defun openai-compatible--standalone-wire-tool (entry)
  "Normalize one standalone Responses function ENTRY for Chat Completions."
  (when (and (json-object-p entry)
             (json-string= (json-get entry "type") "function")
             (non-empty-string-p (json-get entry "name")))
    (json-object
     "type" "function"
     "function" (openai-compatible--wire-function (json-get entry "name") entry))))

(-> openai-compatible--wire-tools (vector) vector)
(defun openai-compatible--wire-tools (tool-namespaces)
  "Flatten Autolith namespaces into standard Chat Completions tools."
  (coerce
   (loop for entry across tool-namespaces
         append
         (cond
           ((and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (non-empty-string-p (json-get entry "name"))
                 (vectorp (json-get entry "tools")))
            (loop for tool across (json-get entry "tools")
                  when (json-object-p tool)
                    collect
                    (openai-compatible--wire-tool
                     (json-get entry "name")
                     tool)))
           (t
            (let ((standalone (openai-compatible--standalone-wire-tool entry)))
              (if standalone (list standalone) nil)))))
   'vector))


;;;; -- Chat Completions Message Encoding --

(-> openai-compatible--chat-content-part (json-object) (option json-object))
(defun openai-compatible--chat-content-part (part)
  "Translate one Responses content PART into a Chat Completions part."
  (let ((type (json-get part "type")))
    (cond
      ((and (json-string-member-p
             type '("input_text" "output_text" "text" "refusal"))
            (stringp (json-get part "text")))
       (json-object "type" "text" "text" (json-get part "text")))
      ((and (json-string= type "input_image")
            (non-empty-string-p (json-get part "image_url")))
       (json-object
        "type" "image_url"
        "image_url" (json-object "url" (json-get part "image_url"))))
      ((and (json-string= type "image_url")
            (non-empty-string-p (json-get part "url")))
       (json-object
        "type" "image_url"
        "image_url" (json-object "url" (json-get part "url"))))
      (t
       nil))))

(-> openai-compatible--chat-content (t) t)
(defun openai-compatible--chat-content (content)
  "Translate Responses CONTENT into a Chat Completions content value."
  (cond
    ((stringp content)
     content)
    ((vectorp content)
     (coerce
      (loop for part across content
            for translated = (and (json-object-p part)
                                  (openai-compatible--chat-content-part part))
            when translated collect translated)
      'vector))
    ((null content)
     "")
    (t
     (bounded-string content :limit 2000))))

(-> openai-compatible--wire-call-name (json-object) string)
(defun openai-compatible--wire-call-name (item)
  "Return the flat Chat Completions function name for function-call ITEM."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (if (non-empty-string-p namespace)
        (openai-compatible--wire-tool-name namespace name)
        name)))

(-> openai-compatible--chat-message (json-object) (option json-object))
(defun openai-compatible--chat-message (item)
  "Translate a Responses message ITEM into a Chat Completions message."
  (let ((role (json-get item "role")))
    (when (json-string-member-p
           role '("user" "assistant" "developer" "system"))
      (json-object
       "role" (if (json-string-member-p role '("developer" "system"))
                  "system"
                  role)
       "content" (openai-compatible--chat-content
                   (json-get item "content"))))))

(-> openai-compatible--chat-function-call-entry (json-object) json-object)
(defun openai-compatible--chat-function-call-entry (item)
  "Translate one Responses function-call ITEM into a Chat tool-call entry."
  (json-object
   "id" (json-get item "call_id")
   "type" "function"
   "function"
   (json-object
    "name" (openai-compatible--wire-call-name item)
    "arguments" (or (json-get item "arguments") "{}"))))

(-> openai-compatible--chat-function-calls (list) json-object)
(defun openai-compatible--chat-function-calls (items)
  "Translate consecutive Responses function-call ITEMS into one assistant message."
  (json-object
   "role" "assistant"
   "content" nil
   "tool_calls"
   (apply #'json-array
          (mapcar #'openai-compatible--chat-function-call-entry items))))

(-> openai-compatible--chat-tool-output (json-object) json-object)
(defun openai-compatible--chat-tool-output (item)
  "Translate a Responses function-call output ITEM into a tool message."
  (json-object
   "role" "tool"
   "tool_call_id" (json-get item "call_id")
   "content" (openai-compatible--chat-content (json-get item "output"))))

(-> openai-compatible--chat-input-item (json-object) (option json-object))
(defun openai-compatible--chat-input-item (item)
  "Translate one portable Responses input ITEM for Chat Completions."
  (when (json-object-p item)
    (cond
      ((json-string= (json-get item "type") "message")
       (openai-compatible--chat-message item))
      ((json-string= (json-get item "type") "function_call_output")
       (openai-compatible--chat-tool-output item))
      (t
       nil))))

(-> openai-compatible--chat-input-messages (list) list)
(defun openai-compatible--chat-input-messages (items)
  "Translate portable Responses ITEMS into valid Chat Completions messages."
  (let ((messages nil)
        (function-calls nil))
    (labels ((flush-function-calls ()
               "Append one assistant message for pending function calls."
               (when function-calls
                 (push (openai-compatible--chat-function-calls
                        (nreverse function-calls))
                       messages)
                 (setf function-calls nil))))
      (dolist (item items)
        (if (and (json-object-p item)
                 (function-call-item-p item))
            (push item function-calls)
            (progn
              (flush-function-calls)
              (let ((message (openai-compatible--chat-input-item item)))
                (when message
                  (push message messages))))))
      (flush-function-calls)
      (nreverse messages))))

(-> openai-compatible--chat-system-message (string) json-object)
(defun openai-compatible--chat-system-message (text)
  "Return one Chat Completions system message containing TEXT."
  (json-object "role" "system" "content" text))


;;;; -- Chat Completions Requests --

(defmethod provider-request-object
    ((provider openai-compatible-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build a stateless streaming Chat Completions request."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
           (if compaction-p
               #()
               (openai-compatible--wire-tools tool-namespaces)))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-tools
              :goal-context goal-context
              :compaction-p compaction-p)))
         (input-items
           (conversation-input-items-for-family
            conversation
            (provider-family provider)
            :include-ephemeral-p (not compaction-p)))
         (messages
           (append
            (list (openai-compatible--chat-system-message
                   (system-prompt configuration)))
            (when (and goal-context (not compaction-p))
              (list (openai-compatible--chat-system-message goal-context)))
            (openai-compatible--chat-input-messages input-items)
            (when (and delivery
                       (non-empty-string-p
                        (context-delivery-rendered delivery)))
              (list (openai-compatible--chat-system-message
                     (context-delivery-rendered delivery))))
            (when compaction-p
              (list (openai-compatible--chat-system-message
                     *compaction-instructions*)))))
         (request
           (json-object
            "model" (configuration-model configuration)
            "messages" (coerce messages 'vector)
            "stream" t)))
    (when (plusp (length effective-tools))
      (setf (gethash "tools" request) effective-tools
            (gethash "tool_choice" request) "auto"))
    (when (openai-compatible-provider-reasoning-parameter provider)
      (setf (gethash (openai-compatible-provider-reasoning-parameter provider)
                     request)
            (configuration-reasoning-effort configuration)))
    (when *provider-maximum-output-tokens*
      (setf (gethash "max_completion_tokens" request)
            *provider-maximum-output-tokens*))
    (values request delivery)))


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
  (dexador:post
   (configuration-provider-endpoint (provider-configuration provider))
   :headers (openai-compatible--request-headers provider credentials conversation)
   :content (json-encode-utf8 request)
   :want-stream t
   :force-string t
   :keep-alive nil
   :connect-timeout 30
   :read-timeout 300))


;;;; -- Chat Completions Stream Decoding --

(-> openai-compatible--delta-text (t) (option string))
(defun openai-compatible--delta-text (value)
  "Return visible text from a Chat Completions delta VALUE."
  (cond
    ((stringp value)
     value)
    ((vectorp value)
     (let ((parts
             (loop for part across value
                   when (and (json-object-p part)
                             (stringp (json-get part "text")))
                     collect (json-get part "text"))))
       (and parts (format nil "~{~A~^~%~}" parts))))
    (t
     nil)))

(defstruct (openai-compatible-tool-state
            (:constructor openai-compatible--tool-state (index)))
  "Mutable accumulator for one Chat Completions tool call."
  (index 0 :type (integer 0) :read-only t)
  (id nil :type (option string))
  (name-stream (make-string-output-stream) :type stream :read-only t)
  (name-character-count 0 :type (integer 0))
  (arguments-stream (make-string-output-stream) :type stream :read-only t)
  (arguments-character-count 0 :type (integer 0)))

(-> openai-compatible--append-tool-delta
    (hash-table (integer 0) json-object)
    openai-compatible-tool-state)
(defun openai-compatible--append-tool-delta (states index delta)
  "Merge one Chat Completions tool DELTA into STATES and return its state."
  (let ((state (or (gethash index states)
                   (setf (gethash index states)
                         (openai-compatible--tool-state index)))))
    (let* ((function (and (json-object-p delta)
                          (json-get delta "function")))
           (id (and (json-object-p delta) (json-get delta "id")))
           (name (or (and (json-object-p function)
                          (json-get function "name"))
                     (and (json-object-p delta)
                          (json-get delta "name"))))
           (arguments (or (and (json-object-p function)
                               (json-get function "arguments"))
                          (and (json-object-p delta)
                               (json-get delta "arguments")))))
      (when (non-empty-string-p id)
        (setf (openai-compatible-tool-state-id state) id))
      (when (non-empty-string-p name)
        (write-string name (openai-compatible-tool-state-name-stream state))
        (incf (openai-compatible-tool-state-name-character-count state)
              (length name)))
      (when (stringp arguments)
        (write-string arguments
                      (openai-compatible-tool-state-arguments-stream state))
        (incf (openai-compatible-tool-state-arguments-character-count state)
              (length arguments))))
    state))

(-> openai-compatible--tool-delta-index (json-object (integer 0)) (integer 0))
(defun openai-compatible--tool-delta-index (tool fallback-index)
  "Return TOOL's wire index, or FALLBACK-INDEX when it declares no valid one."
  (let ((index (json-get tool "index")))
    (if (and (integerp index) (not (minusp index)))
        index
        fallback-index)))

(-> openai-compatible--choice-tool-deltas (json-object) list)
(defun openai-compatible--choice-tool-deltas (delta)
  "Return validated indexed tool DELTA pairs from one Chat Completions delta."
  (let ((tool-deltas (json-get delta "tool_calls")))
    (if (vectorp tool-deltas)
        (loop for tool across tool-deltas
              for fallback-index from 0
              when (json-object-p tool)
                collect (list (openai-compatible--tool-delta-index
                               tool fallback-index)
                              tool))
        (let ((function-call (json-get delta "function_call")))
          (if (json-object-p function-call)
              (list (list 0 function-call))
              nil)))))

(-> openai-compatible--stream-tool-states (hash-table) list)
(defun openai-compatible--stream-tool-states (states)
  "Return accumulated tool STATES in wire index order."
  (sort
   (loop for state being the hash-values of states collect state)
   #'<
   :key #'openai-compatible-tool-state-index))

(-> openai-compatible--function-call-item
    (openai-compatible-tool-state t)
    json-object)
(defun openai-compatible--function-call-item (state headers)
  "Return one complete normalized Responses function-call item from STATE."
  (let ((id (openai-compatible-tool-state-id state))
        (name
          (get-output-stream-string
           (openai-compatible-tool-state-name-stream state)))
        (arguments
          (get-output-stream-string
           (openai-compatible-tool-state-arguments-stream state))))
    (unless (and (non-empty-string-p id) (non-empty-string-p name))
      (provider--signal-stream-interruption
       headers
       "The provider returned an incomplete tool call."))
    (json-object
     "type" "function_call"
     "call_id" id
     "name" name
     "arguments" (if (zerop (length arguments)) "{}" arguments))))

(-> openai-compatible--fallback-tool-name
    (string)
    (values (option string) (option string)))
(defun openai-compatible--fallback-tool-name (name)
  "Split a canonical dotted tool NAME a model echoed instead of ours.

Models that do not repeat the encoded function names verbatim commonly
reproduce the dotted canonical name from the prompt, sometimes with a
stray leading dot. A bare name without a namespace half is left for
the registry's unique-name dispatch."
  (let* ((trimmed (string-left-trim "." name))
         (separator (position #\. trimmed)))
    (if (and separator
             (plusp separator)
             (< (1+ separator) (length trimmed)))
        (values (subseq trimmed 0 separator)
                (subseq trimmed (1+ separator)))
        (values nil (and (plusp (length trimmed)) trimmed)))))

(defmethod provider-normalize-output-item
    ((provider openai-compatible-provider) (item hash-table))
  "Strip server identifiers and split encoded names into namespaced calls."
  (call-next-method)
  (when (function-call-item-p item)
    (let ((name (json-get item "name")))
      (when (stringp name)
        (multiple-value-bind (namespace tool-name)
            (openai-compatible--decode-wire-tool-name name)
          (unless (and namespace tool-name)
            (multiple-value-setq (namespace tool-name)
              (openai-compatible--fallback-tool-name name)))
          (when tool-name
            (when namespace
              (setf (gethash "namespace" item) namespace))
            (setf (gethash "name" item) tool-name))))))
  item)

(defmethod provider-consume-stream
    ((provider openai-compatible-provider) stream headers event-callback)
  "Consume a Chat Completions SSE stream into a provider result."
  (let ((response-id nil)
        (usage nil)
        (finish-reason nil)
        (text-stream (make-string-output-stream))
        (tool-states (make-hash-table :test #'equal))
        (completed-p nil))
    (loop until completed-p
          for data = (provider--read-sse-data stream headers)
          do (cond
               ((eq data *sse-end-of-stream*)
                (if finish-reason
                    (setf completed-p t)
                    (provider--signal-stream-interruption
                     headers
                     "The provider stream closed before a terminal event.")))
               ((string= data "[DONE]")
                (setf completed-p t))
               (t
                (let* ((event (provider--decode-sse-data data headers))
                       (error-object (and (json-object-p event)
                                          (provider--event-error-object event))))
                  (when error-object
                    (provider--signal-event-failure
                     event
                     :type "error"
                     :data data
                     :headers headers
                     :response-id response-id))
                  (when (json-object-p event)
                    (let ((event-id (json-get event "id"))
                          (event-usage (json-get event "usage")))
                      (when (non-empty-string-p event-id)
                        (setf response-id event-id))
                      (when event-usage
                        (setf usage event-usage)))
                    (let ((choices (json-get event "choices")))
                      (if (vectorp choices)
                          (loop for choice across choices
                                when (json-object-p choice)
                                  do
                                     (let* ((delta
                                              (or (json-get choice "delta")
                                                  (json-get choice "message")))
                                            (text
                                              (and (json-object-p delta)
                                                   (openai-compatible--delta-text
                                                    (json-get delta "content"))))
                                            (reasoning
                                              (and (json-object-p delta)
                                                   (or (json-get delta
                                                                 "reasoning_content")
                                                       (json-get delta "reasoning")
                                                       (json-get delta "thinking"))))
                                            (finish
                                              (json-get choice "finish_reason")))
                                       (when text
                                         (write-string text text-stream)
                                         (funcall event-callback
                                                  (make-instance
                                                   'assistant-delta-event
                                                   :text text)))
                                       (when (stringp reasoning)
                                         (funcall event-callback
                                                  (make-instance
                                                   'reasoning-delta-event
                                                   :text reasoning)))
                                       (when (json-object-p delta)
                                         (dolist (tool-delta
                                                  (openai-compatible--choice-tool-deltas
                                                   delta))
                                           (openai-compatible--append-tool-delta
                                            tool-states
                                            (first tool-delta)
                                            (second tool-delta))))
                                       (when (stringp finish)
                                         (setf finish-reason finish))))
                          (funcall event-callback
                                   (make-instance 'provider-progress-event)))))))))
    (let ((output-items nil)
          (assistant-text (get-output-stream-string text-stream)))
      (when (plusp (length assistant-text))
        (push
         (json-object
          "type" "message"
          "role" "assistant"
          "content"
          (json-array
           (json-object "type" "output_text" "text" assistant-text)))
         output-items))
      (let* ((states (openai-compatible--stream-tool-states tool-states))
             (tool-call-p
               (some (lambda (state)
                       (plusp
                        (openai-compatible-tool-state-name-character-count
                         state)))
                     states)))
        (dolist (state states)
          (let ((item (openai-compatible--function-call-item state headers)))
            (provider-normalize-output-item provider item)
            (push item output-items)))
        (setf output-items (nreverse output-items))
        (dolist (item output-items)
          (funcall event-callback
                   (make-instance 'provider-item-event :item item)))
        (let ((turn-completion
                (if (or tool-call-p
                        (member finish-reason '("tool_calls" "function_call")
                                :test #'string=))
                    ':continue
                    ':end)))
          (funcall event-callback
                   (make-instance
                    'provider-completed-event
                    :response-id response-id
                    :usage usage
                    :turn-completion turn-completion))
          (make-instance
           'provider-result
           :response-id response-id
           :output-items output-items
           :tool-calls (remove-if-not #'function-call-item-p output-items)
           :usage usage
           :turn-state nil
           :turn-completion turn-completion))))))

