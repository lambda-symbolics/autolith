(in-package #:autolith)

;;;; -- Anthropic Messages API Provider --

;;; The Anthropic Messages API speaks its own streaming dialect, verified
;;; against claude-haiku-4-5 on 2026-08-08: requests carry the system prompt
;;; as a top-level field, messages strictly alternate user and assistant
;;; roles, tools declare input_schema and tool_choice {"type": "auto"}, and
;;; streams emit message_start, content_block_start/delta/stop,
;;; message_delta, and message_stop events. Tool calls are assistant
;;; tool_use content blocks and results are user tool_result blocks. The
;;; wire namespace accepts only letters, digits, hyphens, and underscores,
;;; so this provider shares the Chat Completions Base64 tool-name encoding.
;;; Conversations therefore persist in the same namespaced shape regardless
;;; of the serving provider.

(defclass anthropic-api-key-provider (subscription-provider)
  ()
  (:documentation
   "A static API key client for the Anthropic Messages API."))

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
                 :configuration configuration
                 :credential-manager (anthropic-credential-manager-create
                                      configuration)
                 :session-id (make-identifier)))

(defmethod provider-with-configuration
    ((provider anthropic-api-key-provider) (configuration configuration))
  "Copy PROVIDER with CONFIGURATION, retaining credentials and session state."
  (make-instance 'anthropic-api-key-provider
                 :configuration configuration
                 :registration (model-provider-registration provider)
                 :credential-manager (provider-credential-manager provider)
                 :session-id (provider-session-id provider)))

(defmethod provider-authenticate ((provider anthropic-api-key-provider)
                                  &key stream open-browser-p)
  "Prompt for, validate, and save the Anthropic API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


;;;; -- Anthropic Tool Encoding --

(-> anthropic--wire-tool (string json-object) json-object)
(defun anthropic--wire-tool (namespace tool)
  "Return one namespaced Autolith TOOL as an Anthropic tool declaration."
  (json-object
   "name" (openai-compatible--wire-tool-name namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "input_schema" (json-get tool "parameters")))

(-> anthropic--wire-tools (vector) vector)
(defun anthropic--wire-tools (tool-namespaces)
  "Flatten Autolith namespaces into Anthropic's flat tools array."
  (coerce
   (loop for entry across tool-namespaces
         when (and (json-object-p entry)
                   (json-string= (json-get entry "type") "namespace")
                   (non-empty-string-p (json-get entry "name"))
                   (vectorp (json-get entry "tools")))
           append (loop for tool across (json-get entry "tools")
                        when (and (json-object-p tool)
                                  (non-empty-string-p (json-get tool "name")))
                          collect (anthropic--wire-tool
                                   (json-get entry "name")
                                   tool)))
   'vector))


;;;; -- Anthropic Message Encoding --

(-> anthropic--image-source (string) (option json-object))
(defun anthropic--image-source (image-url)
  "Return one Anthropic base64 or remote image source for IMAGE-URL."
  (cond
    ((uiop:string-prefix-p "data:" image-url)
     (let* ((metadata-end (position #\, image-url))
            (metadata (and metadata-end
                           (subseq image-url (length "data:") metadata-end)))
            (base64-p (and metadata
                           (uiop:string-suffix-p metadata ";base64")))
            (media-type (and base64-p
                             (subseq metadata 0
                                     (- (length metadata)
                                        (length ";base64")))))
            (data (and metadata-end (subseq image-url (1+ metadata-end)))))
       (when (and base64-p (non-empty-string-p media-type)
                  (non-empty-string-p data))
         (json-object "type" "base64"
                      "media_type" media-type
                      "data" data))))
    ((or (uiop:string-prefix-p "https://" image-url)
         (uiop:string-prefix-p "http://" image-url))
     (json-object "type" "url" "url" image-url))
    (t
     nil)))

(-> anthropic--signal-request-conversion-failure (string) null)
(defun anthropic--signal-request-conversion-failure (message)
  "Signal a terminal Anthropic request conversion failure described by MESSAGE."
  (error 'provider-error
         :message message
         :status nil
         :code "unsupported_content"
         :request-id nil
         :response-id nil
         :response nil))

(-> anthropic--content-block (json-object) (option json-object))
(defun anthropic--content-block (part)
  "Translate one Responses content PART into an Anthropic content block."
  (let ((type (json-get part "type")))
    (cond
      ((and (json-string-member-p
             type '("input_text" "output_text" "text" "refusal"))
            (stringp (json-get part "text")))
       (json-object "type" "text" "text" (json-get part "text")))
      ((and (json-string= type "input_image")
            (non-empty-string-p (json-get part "image_url")))
       (let ((source (anthropic--image-source (json-get part "image_url"))))
         (when source
           (json-object "type" "image" "source" source))))
      (t
       nil))))

(-> anthropic--content-blocks (t) (values list boolean))
(defun anthropic--content-blocks (content)
  "Translate Responses CONTENT and report whether every part was supported."
  (cond
    ((stringp content)
     (values (list (json-object "type" "text" "text" content)) t))
    ((vectorp content)
     (let ((blocks nil)
           (complete-p t))
       (loop for part across content
             for block = (and (json-object-p part)
                              (anthropic--content-block part))
             do (if block
                    (push block blocks)
                    (setf complete-p nil)))
       (values (nreverse blocks) complete-p)))
    (t
     (values nil nil))))

(-> anthropic--tool-use-block (json-object) json-object)
(defun anthropic--tool-use-block (item)
  "Translate one Responses function-call ITEM into an Anthropic tool_use block."
  (let* ((call-id (json-get item "call_id"))
         (tool-name (json-get item "name"))
         (namespace (json-get item "namespace"))
         (arguments (json-get item "arguments"))
         (input
           (and (non-empty-string-p arguments)
                (handler-case (json-decode arguments)
                  (error ()
                    nil)))))
    (unless (and (non-empty-string-p call-id)
                 (non-empty-string-p tool-name)
                 (json-object-p input))
      (anthropic--signal-request-conversion-failure
       "A replayed function call is not valid Anthropic tool use."))
    (json-object
     "type" "tool_use"
     "id" call-id
     "name" (if (non-empty-string-p namespace)
                (openai-compatible--wire-tool-name namespace tool-name)
                tool-name)
     "input" input)))

(-> anthropic--tool-result-block (json-object) json-object)
(defun anthropic--tool-result-block (item)
  "Translate one Responses function-call output ITEM into a tool_result block."
  (let ((call-id (json-get item "call_id"))
        (output (json-get item "output")))
    (unless (non-empty-string-p call-id)
      (anthropic--signal-request-conversion-failure
       "A replayed tool result has no valid Anthropic tool-use identifier."))
    (json-object
     "type" "tool_result"
     "tool_use_id" call-id
     "content"
     (cond
       ((stringp output)
        output)
       ((vectorp output)
        (multiple-value-bind (blocks complete-p)
            (anthropic--content-blocks output)
          (cond
            ((and complete-p blocks)
             (coerce blocks 'vector))
            ((null blocks)
             (bounded-string (json-encode output) :limit 2000))
            (t
             (anthropic--signal-request-conversion-failure
              "A tool result mixes supported and unsupported content.")))))
       (t
        (bounded-string output :limit 2000))))))

(defstruct (anthropic--message-state
            (:constructor anthropic--message-state (role)))
  "Mutable accumulator for one Anthropic message under construction."
  (role "user" :type string :read-only t)
  (blocks nil :type list))

(-> anthropic--message-state-push (anthropic--message-state json-object) null)
(defun anthropic--message-state-push (state block)
  "Append BLOCK to STATE, merging adjacent text blocks of one origin."
  (push block (anthropic--message-state-blocks state))
  nil)

(-> anthropic--message-state-render (anthropic--message-state) json-object)
(defun anthropic--message-state-render (state)
  "Render STATE as one Anthropic message with content blocks in order."
  (json-object "role" (anthropic--message-state-role state)
               "content" (coerce (nreverse (anthropic--message-state-blocks state))
                                 'vector)))

(-> anthropic--input-messages (list) (values list list))
(defun anthropic--input-messages (items)
  "Translate portable Responses ITEMS into alternating Anthropic messages.

Returns the message list and the developer/system texts that Anthropic
carries as top-level system blocks rather than as a message role. The inherited
reference boundary remains at its transcript position as user content because
moving it into the top-level system field would reverse its positional scope."
  (let ((messages        nil)
        (system-texts    nil)
        (state           nil)
        (tool-use-states (make-hash-table :test #'equal)))
    (labels ((flush ()
               "Append the pending message, merging with a same-role tail."
               (when (and state (anthropic--message-state-blocks state))
                 (let ((rendered (anthropic--message-state-render state)))
                   (if (and messages
                            (string= (json-get (first messages) "role")
                                     (json-get rendered "role")))
                       (setf (gethash "content" (first messages))
                             (concatenate 'vector
                                          (json-get (first messages) "content")
                                          (json-get rendered "content")))
                       (push rendered messages))))
               (setf state nil))

             (begin (role)
               "Start or continue a message with ROLE, flushing on changes."
               (when (and state
                          (not (string= (anthropic--message-state-role state)
                                        role)))
                 (flush))
               (unless state
                 (setf state (anthropic--message-state role))))

             (append-message (role content)
               "Append translated CONTENT to a message with ROLE."
               (multiple-value-bind (blocks complete-p)
                   (anthropic--content-blocks content)
                 (unless (and complete-p blocks)
                   (anthropic--signal-request-conversion-failure
                    "A replayed message contains unsupported Anthropic content."))
                 (flush)
                 (begin role)
                 (dolist (block blocks)
                   (anthropic--message-state-push state block)))))
      (dolist (item items)
        (when (json-object-p item)
          (let ((type (json-get item "type")))
            (cond
              ((json-string= type "message")
               (let ((role (json-get item "role")))
                 (cond
                   ((conversation--inherited-reference-boundary-p item)
                    (append-message "user" (json-get item "content")))
                   ((json-string-member-p role '("developer" "system"))
                    (multiple-value-bind (blocks complete-p)
                        (anthropic--content-blocks (json-get item "content"))
                      (unless (and complete-p
                                   blocks
                                   (every (lambda (block)
                                            (json-string= (json-get block "type")
                                                          "text"))
                                          blocks))
                        (anthropic--signal-request-conversion-failure
                         "A system message contains unsupported Anthropic content."))
                      (push (format nil "~{~A~^~2%~}"
                                    (mapcar (lambda (block)
                                              (json-get block "text"))
                                            blocks))
                            system-texts)))
                   ((json-string-member-p role '("user" "assistant"))
                    (append-message role (json-get item "content")))
                   (t
                    nil))))
              ((and (json-string= type "function_call")
                    (function-call-item-p item))
               (let* ((block (anthropic--tool-use-block item))
                      (call-id (json-get block "id")))
                 (when (gethash call-id tool-use-states)
                   (anthropic--signal-request-conversion-failure
                    "A replayed function call reuses an Anthropic tool-use identifier."))
                 (setf (gethash call-id tool-use-states) ':pending)
                 (flush)
                 (begin "assistant")
                 (anthropic--message-state-push state block)))
              ((json-string= type "function_call_output")
               (let* ((block (anthropic--tool-result-block item))
                      (call-id (json-get block "tool_use_id")))
                 (unless (eq (gethash call-id tool-use-states) ':pending)
                   (anthropic--signal-request-conversion-failure
                    "A replayed tool result does not match one pending Anthropic tool use."))
                 (setf (gethash call-id tool-use-states) ':completed)
                 (flush)
                 (begin "user")
                 (anthropic--message-state-push state block)))
              (t
               nil)))))
      (flush)
      (values (nreverse messages) (nreverse system-texts)))))


;;;; -- Anthropic Requests --

(defparameter *anthropic-maximum-output-tokens* 32000
  "The output token budget requested for Anthropic streaming turns.")

(-> anthropic--system-blocks (list) (option vector))
(defun anthropic--system-blocks (texts)
  "Return Anthropic system blocks for the nonempty TEXTS, or NIL."
  (let ((kept (remove-if-not #'non-empty-string-p texts)))
    (when kept
      (coerce (mapcar (lambda (text)
                        (json-object "type" "text" "text" text))
                      kept)
              'vector))))

(defmethod provider-request-object
    ((provider anthropic-api-key-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build the complete stateless Anthropic Messages request for CONVERSATION.

The system prompt, GOAL-CONTEXT, and resolved context contributions ride as
top-level system blocks. COMPACTION-P builds a tool-free summarization
request whose trailing system block asks for a context checkpoint handoff.
The second value is the context delivery that the transport consumes only
after a completed response."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
           (if compaction-p #() (anthropic--wire-tools tool-namespaces)))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-tools
              :goal-context goal-context)))
         (input-items
           (conversation-input-items-for-family
            conversation
            (provider-family provider)
            :include-ephemeral-p (not compaction-p))))
    (multiple-value-bind (messages system-texts)
        (anthropic--input-messages input-items)
      (let ((request
              (json-object
               "model" (configuration-model configuration)
               "max_tokens" (if *provider-maximum-output-tokens*
                                (min *provider-maximum-output-tokens*
                                     *anthropic-maximum-output-tokens*)
                                *anthropic-maximum-output-tokens*)
               "messages" (coerce messages 'vector)
               "stream" t)))
        (let ((system
                (anthropic--system-blocks
                 (append
                  (list (let ((*system-prompt-hosted-web-search-p* nil))
                          (system-prompt configuration)))
                  (when (and goal-context (not compaction-p))
                    (list goal-context))
                  system-texts
                  (when (and delivery
                             (non-empty-string-p
                              (context-delivery-rendered delivery)))
                    (list (context-delivery-rendered delivery)))
                  (when compaction-p
                    (list *compaction-instructions*))))))
          (when system
            (setf (gethash "system" request) system)))
        (when (plusp (length effective-tools))
          (setf (gethash "tools" request) effective-tools
                (gethash "tool_choice" request)
                (json-object "type" "auto")))
        (values request delivery)))))


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
           (type conversation conversation))
  (dexador:post
   (configuration-provider-endpoint (provider-configuration provider))
   :headers (anthropic--request-headers credentials)
   :content (json-encode-utf8 request)
   :want-stream t
   :force-string t
   :keep-alive nil
   :connect-timeout 30
   :read-timeout 300))


;;;; -- Anthropic Stream Decoding --

(defstruct (anthropic--block-state
            (:constructor anthropic--block-state (index type)))
  "Mutable accumulator for one streamed Anthropic content block."
  (index 0 :type (integer 0) :read-only t)
  (type "" :type string :read-only t)
  (id nil :type (option string))
  (name nil :type (option string))
  (initial-input nil :type t)
  (text-stream (make-string-output-stream) :type stream :read-only t)
  (json-stream (make-string-output-stream) :type stream :read-only t))

(-> anthropic--request-id (t) (option string))
(defun anthropic--request-id (headers)
  "Return Anthropic's request identifier from response HEADERS, if present."
  (provider--response-request-id headers))

(-> anthropic--signal-protocol-failure
    (string &key (:headers t)
                 (:response-id (option string))
                 (:data (option string)))
    null)
(defun anthropic--signal-protocol-failure
    (message &key headers response-id data)
  "Signal a terminal Anthropic stream protocol failure described by MESSAGE."
  (error 'provider-error
         :message (provider--sanitize-wire-string message)
         :status nil
         :code "invalid_stream"
         :request-id (anthropic--request-id headers)
         :response-id response-id
         :response
         (and data
              (bounded-string (provider--sanitize-wire-string data) :limit 2000))))

(-> anthropic--signal-incomplete-response
    (non-empty-string &key (:headers t)
                           (:response-id (option string))
                           (:data (option string)))
    null)
(defun anthropic--signal-incomplete-response
    (reason &key headers response-id data)
  "Signal Anthropic's retryable incomplete response REASON."
  (error 'provider-incomplete-response
         :message
         (format nil "The provider returned an incomplete response (~A)." reason)
         :reason reason
         :status nil
         :code "response_incomplete"
         :request-id (anthropic--request-id headers)
         :response-id response-id
         :response
         (and data
              (bounded-string (provider--sanitize-wire-string data) :limit 2000))))

(-> anthropic--event-index
    (json-object &key (:headers t)
                      (:response-id (option string))
                      (:data (option string)))
    (integer 0))
(defun anthropic--event-index (event &key headers response-id data)
  "Return EVENT's validated nonnegative content block index."
  (let ((index (json-get event "index")))
    (unless (typep index '(integer 0))
      (anthropic--signal-protocol-failure
       "The provider returned an invalid content block index."
       :headers headers :response-id response-id :data data))
    index))

(-> anthropic--tool-arguments
    (anthropic--block-state &key (:headers t)
                                 (:response-id (option string))
                                 (:data (option string)))
    string)
(defun anthropic--tool-arguments (state &key headers response-id data)
  "Return STATE's validated JSON-object tool arguments as wire text."
  (let* ((fragments
           (get-output-stream-string
            (anthropic--block-state-json-stream state)))
         (arguments
           (if (non-empty-string-p fragments)
               fragments
               (json-encode
                (or (anthropic--block-state-initial-input state)
                    (json-object)))))
         (valid-p
           (handler-case
               (json-object-p (json-decode arguments))
             (error ()
               nil))))
    (unless valid-p
      (anthropic--signal-protocol-failure
       "The provider returned malformed tool-use arguments."
       :headers headers :response-id response-id :data data))
    arguments))

(-> anthropic--block-item
    (anthropic--block-state &key (:headers t)
                                 (:response-id (option string))
                                 (:data (option string)))
    (option json-object))
(defun anthropic--block-item (state &key headers response-id data)
  "Return STATE's completed portable output item, or NIL for empty text."
  (if (string= (anthropic--block-state-type state) "tool_use")
      (let ((item
              (json-object
               "type" "function_call"
               "call_id" (anthropic--block-state-id state)
               "name" (anthropic--block-state-name state)
               "arguments"
               (anthropic--tool-arguments
                state :headers headers :response-id response-id :data data)
               "status" "completed")))
        (multiple-value-bind (namespace name)
            (openai-compatible--decode-wire-tool-name (json-get item "name"))
          (when (and namespace name)
            (setf (gethash "namespace" item) namespace
                  (gethash "name" item) name)))
        item)
      (let ((text
              (get-output-stream-string
               (anthropic--block-state-text-stream state))))
        (when (plusp (length text))
          (json-object
           "type" "message"
           "status" "completed"
           "role" "assistant"
           "content"
           (json-array
            (json-object "type" "output_text"
                         "text" text
                         "annotations" (json-array))))))))

(-> anthropic--ordered-block-items
    (hash-table &key (:headers t)
                     (:response-id (option string))
                     (:data (option string)))
    list)
(defun anthropic--ordered-block-items
    (completed-blocks &key headers response-id data)
  "Return COMPLETED-BLOCKS in contiguous Anthropic content index order."
  (let ((indices
          (sort (loop for index being the hash-keys of completed-blocks
                      collect index)
                #'<)))
    (unless (equal indices
                   (loop for index below (length indices) collect index))
      (anthropic--signal-protocol-failure
       "The provider returned noncontiguous content block indices."
       :headers headers :response-id response-id :data data))
    (loop for index in indices
          for item = (gethash index completed-blocks)
          when item collect item)))

(-> anthropic--stop-reason-completion
    (non-empty-string &key (:headers t)
                           (:response-id (option string))
                           (:data (option string)))
    turn-completion)
(defun anthropic--stop-reason-completion
    (reason &key headers response-id data)
  "Return the portable turn completion for Anthropic stop REASON."
  (cond
    ((member reason '("end_turn" "stop_sequence" "refusal") :test #'string=)
     ':end)
    ((string= reason "tool_use")
     ':continue)
    ((string= reason "pause_turn")
     (anthropic--signal-protocol-failure
      "The provider requested unsupported server-tool continuation (pause_turn)."
      :headers headers :response-id response-id :data data))
    ((member reason '("max_tokens" "model_context_window_exceeded")
             :test #'string=)
     (anthropic--signal-incomplete-response
      reason :headers headers :response-id response-id :data data))
    (t
     (anthropic--signal-protocol-failure
      (format nil "The provider returned an unknown stop reason: ~A." reason)
      :headers headers :response-id response-id :data data))))

(-> anthropic--usage-valid-p (t list) boolean)
(defun anthropic--usage-valid-p (usage required-fields)
  "Return true when USAGE has nonnegative integer REQUIRED-FIELDS."
  (and (json-object-p usage)
       (every
        (lambda (name)
          (multiple-value-bind (value present-p)
              (gethash name usage)
            (and present-p (typep value '(integer 0)))))
        required-fields)))

(-> anthropic--usage-field (json-object string) (option integer))
(defun anthropic--usage-field (usage name)
  "Return the nonnegative integer USAGE field NAME, or NIL."
  (let ((value (and (json-object-p usage) (json-get usage name))))
    (and (typep value '(integer 0)) value)))

(-> anthropic--portable-usage (json-object json-object) json-object)
(defun anthropic--portable-usage (start-usage delta-usage)
  "Combine Anthropic message_start and message_delta USAGE into portable form."
  (let ((input (or (anthropic--usage-field start-usage "input_tokens") 0))
        (output (or (anthropic--usage-field delta-usage "output_tokens")
                    (anthropic--usage-field start-usage "output_tokens")
                    0)))
    (json-object "input_tokens" input
                 "output_tokens" output
                 "total_tokens" (+ input output))))

(defmethod provider-consume-stream
    ((provider anthropic-api-key-provider) stream headers event-callback)
  "Consume PROVIDER's Anthropic SSE stream into a provider result."
  (declare (ignore provider))
  (let ((open-blocks (make-hash-table))
        (seen-blocks (make-hash-table))
        (completed-blocks (make-hash-table))
        (output-items nil)
        (response-id nil)
        (start-usage (json-object))
        (delta-usage (json-object))
        (stop-reason nil)
        (turn-completion :unspecified)
        (started-p nil)
        (message-delta-seen-p nil)
        (completed-p nil))
    (labels ((require-started (data)
               "Reject content events before the stream's message_start event."
               (unless started-p
                 (anthropic--signal-protocol-failure
                  "The provider returned content before message_start."
                  :headers headers :response-id response-id :data data)))

             (reject-late-block-event (data)
               "Reject block events after top-level message deltas begin."
               (when message-delta-seen-p
                 (anthropic--signal-protocol-failure
                  "The provider returned content after message_delta."
                  :headers headers :response-id response-id :data data))))
      (loop until completed-p
            for data = (provider--read-sse-data stream headers)
            do (when (eq data *sse-end-of-stream*)
                 (provider--signal-stream-interruption
                  headers
                  "The provider stream closed before a terminal event."))
               (let ((event (provider--decode-sse-data data headers)))
                 (unless (json-object-p event)
                   (anthropic--signal-protocol-failure
                    "The provider returned a non-object stream event."
                    :headers headers :response-id response-id :data data))
                 (let ((type (json-get event "type")))
                   (unless (non-empty-string-p type)
                     (anthropic--signal-protocol-failure
                      "The provider returned a stream event without a type."
                      :headers headers :response-id response-id :data data))
                   (cond
                     ((string= type "error")
                      (provider--signal-event-failure
                       event :type "error" :data data :headers headers
                       :response-id response-id))
                     ((string= type "message_start")
                      (when started-p
                        (anthropic--signal-protocol-failure
                         "The provider returned duplicate message_start events."
                         :headers headers :response-id response-id :data data))
                      (let ((message (json-get event "message")))
                        (unless (and (json-object-p message)
                                     (non-empty-string-p (json-get message "id"))
                                     (json-string= (json-get message "role")
                                                   "assistant")
                                     (vectorp (json-get message "content"))
                                     (zerop (length (json-get message "content"))))
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid message_start event."
                           :headers headers :response-id response-id :data data))
                        (setf response-id (json-get message "id")
                              started-p t)
                        (multiple-value-bind (usage present-p)
                            (gethash "usage" message)
                          (unless (and present-p
                                       (anthropic--usage-valid-p
                                        usage '("input_tokens" "output_tokens")))
                            (anthropic--signal-protocol-failure
                             "The provider returned invalid initial usage."
                             :headers headers :response-id response-id :data data))
                          (setf start-usage usage)))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "content_block_start")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (block (json-get event "content_block")))
                        (when (gethash index seen-blocks)
                          (anthropic--signal-protocol-failure
                           "The provider returned a duplicate content block."
                           :headers headers :response-id response-id :data data))
                        (when (plusp (hash-table-count open-blocks))
                          (anthropic--signal-protocol-failure
                           "The provider started a content block before the previous block stopped."
                           :headers headers :response-id response-id :data data))
                        (unless (= index (hash-table-count seen-blocks))
                          (anthropic--signal-protocol-failure
                           "The provider returned an out-of-order content block index."
                           :headers headers :response-id response-id :data data))
                        (unless (json-object-p block)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid content block."
                           :headers headers :response-id response-id :data data))
                        (let ((block-type (json-get block "type")))
                          (unless (json-string-member-p block-type '("text" "tool_use"))
                            (anthropic--signal-protocol-failure
                             (format nil
                                     "The provider returned an unsupported content block: ~A."
                                     (or block-type "unknown"))
                             :headers headers :response-id response-id :data data))
                          (let ((state (anthropic--block-state index block-type)))
                            (cond
                              ((string= block-type "text")
                               (multiple-value-bind (text present-p)
                                   (gethash "text" block)
                                 (when (and present-p (not (stringp text)))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned invalid initial text."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (when (and present-p (plusp (length text)))
                                   (write-string
                                    text
                                    (anthropic--block-state-text-stream state))
                                   (funcall
                                    event-callback
                                    (make-instance 'assistant-delta-event
                                                   :text text)))))
                              ((string= block-type "tool_use")
                               (let ((id (json-get block "id"))
                                     (name (json-get block "name")))
                                 (unless (and (non-empty-string-p id)
                                              (non-empty-string-p name))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned an incomplete tool-use block."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (setf (anthropic--block-state-id state) id
                                       (anthropic--block-state-name state) name))
                               (multiple-value-bind (input present-p)
                                   (gethash "input" block)
                                 (when (and present-p (not (json-object-p input)))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned invalid initial tool input."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (when present-p
                                   (setf (anthropic--block-state-initial-input state)
                                         input)))))
                            (setf (gethash index seen-blocks) t
                                  (gethash index open-blocks) state)))
                      (funcall event-callback
                               (make-instance 'provider-progress-event))))
                     ((string= type "content_block_delta")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (state (gethash index open-blocks))
                             (delta (json-get event "delta")))
                        (unless state
                          (anthropic--signal-protocol-failure
                           "The provider returned a delta for an unopened content block."
                           :headers headers :response-id response-id :data data))
                        (unless (json-object-p delta)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid content block delta."
                           :headers headers :response-id response-id :data data))
                        (let ((delta-type (json-get delta "type")))
                          (cond
                            ((and (string= (anthropic--block-state-type state) "text")
                                  (json-string= delta-type "text_delta"))
                             (let ((text (json-get delta "text")))
                               (unless (stringp text)
                                 (anthropic--signal-protocol-failure
                                  "The provider returned invalid text delta content."
                                  :headers headers :response-id response-id
                                  :data data))
                               (write-string
                                text (anthropic--block-state-text-stream state))
                               (funcall event-callback
                                        (make-instance 'assistant-delta-event
                                                       :text text))))
                            ((and (string= (anthropic--block-state-type state)
                                          "tool_use")
                                  (json-string= delta-type "input_json_delta"))
                             (let ((partial-json (json-get delta "partial_json")))
                               (unless (stringp partial-json)
                                 (anthropic--signal-protocol-failure
                                  "The provider returned invalid tool input JSON."
                                  :headers headers :response-id response-id
                                  :data data))
                               (write-string
                                partial-json
                                (anthropic--block-state-json-stream state))
                               (funcall event-callback
                                        (make-instance 'provider-progress-event))))
                            (t
                             (anthropic--signal-protocol-failure
                              (format nil
                                      "The provider returned an unsupported ~A delta for a ~A block."
                                      (or delta-type "unknown")
                                      (anthropic--block-state-type state))
                              :headers headers :response-id response-id
                              :data data))))))
                     ((string= type "content_block_stop")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (state (gethash index open-blocks)))
                        (unless state
                          (anthropic--signal-protocol-failure
                           "The provider stopped an unopened content block."
                           :headers headers :response-id response-id :data data))
                        (setf (gethash index completed-blocks)
                              (anthropic--block-item
                               state :headers headers :response-id response-id
                               :data data))
                        (remhash index open-blocks))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "message_delta")
                      (require-started data)
                      (when (plusp (hash-table-count open-blocks))
                        (anthropic--signal-protocol-failure
                         "The provider returned message_delta with an open content block."
                         :headers headers :response-id response-id :data data))
                      (setf message-delta-seen-p t)
                      (multiple-value-bind (usage present-p)
                          (gethash "usage" event)
                        (unless (and present-p
                                     (anthropic--usage-valid-p
                                      usage '("output_tokens")))
                          (anthropic--signal-protocol-failure
                           "The provider returned invalid delta usage."
                           :headers headers :response-id response-id :data data))
                        (setf delta-usage usage))
                      (let ((delta (json-get event "delta")))
                        (unless (json-object-p delta)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid message_delta event."
                           :headers headers :response-id response-id :data data))
                        (multiple-value-bind (reason present-p)
                            (gethash "stop_reason" delta)
                          (when (and present-p reason
                                     (not (non-empty-string-p reason)))
                            (anthropic--signal-protocol-failure
                             "The provider returned an invalid stop reason."
                             :headers headers :response-id response-id :data data))
                          (when (and present-p reason)
                            (when (and stop-reason
                                       (not (string= stop-reason reason)))
                              (anthropic--signal-protocol-failure
                               "The provider changed its stop reason mid-stream."
                               :headers headers :response-id response-id
                               :data data))
                            (setf stop-reason reason
                                  turn-completion
                                  (anthropic--stop-reason-completion
                                   reason :headers headers
                                   :response-id response-id :data data)))))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "message_stop")
                      (require-started data)
                      (unless message-delta-seen-p
                        (anthropic--signal-protocol-failure
                         "The provider stopped before message_delta."
                         :headers headers :response-id response-id :data data))
                      (when (plusp (hash-table-count open-blocks))
                        (anthropic--signal-protocol-failure
                         "The provider stopped with an open content block."
                         :headers headers :response-id response-id :data data))
                      (unless (non-empty-string-p stop-reason)
                        (anthropic--signal-protocol-failure
                         "The provider stopped without a stop reason."
                         :headers headers :response-id response-id :data data))
                      (let* ((items
                               (anthropic--ordered-block-items
                                completed-blocks :headers headers
                                :response-id response-id :data data))
                             (tool-calls
                               (remove-if-not #'function-call-item-p items)))
                        (when (and (string= stop-reason "tool_use")
                                   (null tool-calls))
                          (anthropic--signal-protocol-failure
                           "The provider reported tool_use without a tool call."
                           :headers headers :response-id response-id :data data))
                        (when (and tool-calls
                                   (not (string= stop-reason "tool_use")))
                          (anthropic--signal-protocol-failure
                           "The provider returned a tool call without tool_use."
                           :headers headers :response-id response-id :data data))
                        (setf output-items items
                              completed-p t)
                        (dolist (item output-items)
                          (funcall event-callback
                                   (make-instance 'provider-item-event :item item)))
                        (funcall
                         event-callback
                         (make-instance
                          'provider-completed-event
                          :response-id response-id
                          :usage (anthropic--portable-usage start-usage delta-usage)
                          :turn-completion turn-completion))))
                     (t
                      ;; Anthropic may add top-level event types. Ignore their
                      ;; unknown metadata without discarding content blocks.
                      (funcall event-callback
                               (make-instance 'provider-progress-event))))))))
    (make-instance 'provider-result
                   :response-id response-id
                   :output-items output-items
                   :tool-calls (remove-if-not #'function-call-item-p output-items)
                   :usage (anthropic--portable-usage start-usage delta-usage)
                   :turn-state nil
                   :turn-completion turn-completion)))
