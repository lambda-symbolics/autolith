(in-package #:autolith)

;;;; -- Anthropic Product Integration Tests --

(-> anthropic-provider-test--configuration () configuration)
(defun anthropic-provider-test--configuration ()
  "Return an isolated configuration selecting the Anthropic model."
  (configuration-copy (test-configuration) :model
                            "claude-haiku-4-5-20251001"))

(-> anthropic-provider-test--selection () null)
(defun anthropic-provider-test--selection ()
  "Test Anthropic model family resolution and endpoint selection."
  (test-assert (eq (model-family "claude-haiku-4-5-20251001") ':anthropic)
               "Claude identifiers resolve to the Anthropic family")
  (dolist (model '("claude-fable-5-2" "claude-opus-5-5"
                   "claude-sonnet-5-5" "claude-haiku-5-5"))
    (let ((configuration (configuration-copy (test-configuration)
                                             :model model)))
      (test-assert
       (and (eq (model-family model) ':anthropic)
            (= (config :context-window configuration) 1000000)
            (equal (provider-model-reasoning-efforts-for model :provider "anthropic")
                   '("low" "medium" "high" "xhigh" "max")))
       "current direct Claude models resolve with their context and supported efforts")))
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider      (anthropic-provider-create configuration)))
    (test-assert
     (string= (config :provider-endpoint configuration)
              *anthropic-messages-endpoint*)
     "Anthropic configurations select the Anthropic Messages endpoint")
    (test-assert (= (config :context-window configuration) 200000)
                 "Claude models carry the Anthropic context window")
    (test-assert
     (and (handler-case
              (progn
                (provider--signal-http-status-failure provider 529)
                nil)
            (provider-retryable-error (error)
              (= (provider-error-status error) 529)))
          (not (provider-retryable-status-p provider 528 nil)))
     "Anthropic treats its overload status as retryable"))
  (anthropic-provider-test--effort-controls)
  nil)

(-> anthropic-provider-test--tool-schemas () null)
(defun anthropic-provider-test--tool-schemas ()
  "Project the default tool registry for Messages and enforce search arguments locally."
  (with-test-configuration (configuration)
    (setf (config :model configuration) "claude-haiku-4-5-20251001")
    (let* ((provider     (anthropic-provider-create configuration))
           (registry     (make-default-tool-registry :configuration configuration))
           (conversation (conversation-create configuration))
           (search-tool  (tool-registry-find registry "search" "content"))
           (canonical    (json-encode (tool-parameters search-tool)))
           (context      (make-instance 'tool-context
                                        :configuration configuration
                                        :conversation  conversation
                                        :registry      registry
                                        :worker        nil))
           (request      (json-decode
                          (json-encode
                           (provider-request-object
                            provider conversation (tool-registry-provider-schemas registry)))))
           (tools        (json-get request "tools")))
      (test-assert
       (and (plusp (length tools))
            (every (lambda (tool)
                     (let ((schema (json-get tool "input_schema")))
                       (and (json-string= (json-get schema "type") "object")
                            (notany (lambda (keyword)
                                      (nth-value 1 (gethash keyword schema)))
                                    '("oneOf" "allOf" "anyOf")))))
                   tools))
       "the complete default toolset has Anthropic-compatible root schemas")
      (test-assert
       (and (gethash "oneOf" (tool-parameters search-tool))
            (string= canonical (json-encode (tool-parameters search-tool))))
       "search retains its canonical mutually exclusive argument schema")
      (dolist (arguments '("{}" "{\"query\":\"needle\",\"patterns\":[\"needle\"]}"))
        (let ((result (tool-registry-execute-call
                       registry
                       (json-object "namespace" "search" "name" "content"
                                    "arguments" arguments)
                       context)))
          (test-assert (not (tool-result-success-p result))
                       "dispatch rejects neither or both search inputs after wire projection")))))
  nil)

(-> anthropic-provider-test--thinking-continuation () null)
(defun anthropic-provider-test--thinking-continuation ()
  "Replay signed thinking after durable conversation reload and a user follow-up."
  (with-test-configuration (configuration)
    (setf (config :model configuration) "claude-haiku-4-5-20251001")
    (dolist (thinking '("reasoning text" ""))
      (let* ((provider     (anthropic-provider-create configuration))
             (conversation (conversation-create configuration))
             (signed       (json-object "type" "thinking"
                                        "thinking" thinking
                                        "signature" "fixture-signature"))
             (redacted     (json-object "type" "redacted_thinking"
                                        "data" "opaque-fixture-payload")))
        (conversation-append-user-message conversation "First question.")
        (dolist (part (list signed redacted
                            (json-object "type" "output_text" "text" "Answer.")))
          (conversation-append-provider-item
           conversation
           (json-object "type" "message" "role" "assistant"
                        "content" (json-array part))))
        (conversation-append-user-message conversation "Follow-up question.")
        (let* ((loaded    (conversation-load (conversation-pathname conversation)))
               (request   (json-decode
                           (json-encode (provider-request-object provider loaded #()))))
               (messages  (json-get request "messages"))
               (assistant (find "assistant" messages :test #'string=
                                :key                       (lambda (message) (json-get message "role"))))
               (content   (json-get assistant "content")))
          (test-assert
           (equal (loop for message across messages collect (json-get message "role"))
                  '("user" "assistant" "user"))
           "reloaded thinking retains assistant and follow-up message order")
          (test-assert
           (and (= (length content) 3)
                (equalp signed (aref content 0))
                (equalp redacted (aref content 1))
                (json-string= (json-get (aref content 2) "type") "text"))
           "signed and opaque thinking replay unchanged before the assistant text")))))
  (anthropic-provider-test--tool-prefix)
  (anthropic-provider-test--binding-recovery)
  nil)

(-> anthropic-provider-test--credential-source () null)
(defun anthropic-provider-test--credential-source ()
  "Test Anthropic credential precedence at the provider boundary."
  (let* ((configuration (anthropic-provider-test--configuration))
         (manager       (anthropic-credential-manager-create configuration))
         (saved         (uiop:getenv "ANTHROPIC_API_KEY")))
    (unwind-protect
         (progn
           (platform-setenv "ANTHROPIC_API_KEY" "")
           (test-assert
            (handler-case
                (progn (credential-manager-load manager) nil)
              (credentials-unavailable () t))
            "Anthropic requires credentials when no source is configured")
           (platform-setenv "ANTHROPIC_API_KEY" "environment-key")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key")
            "Anthropic loads its environment credential"))
      (if saved
          (platform-setenv "ANTHROPIC_API_KEY" saved)
          (platform-unsetenv "ANTHROPIC_API_KEY"))))
  nil)

(-> anthropic-provider-test--ephemeral-cache-boundary () null)
(defun anthropic-provider-test--ephemeral-cache-boundary ()
  "Test volatile input follows the explicit durable-history cache breakpoint."
  (let* ((configuration (anthropic-provider-test--configuration))
         (root          (test-configuration-root configuration))
         (provider      (anthropic-provider-create configuration))
         (conversation
           (conversation-create configuration
                                :identifier "anthropic-ephemeral-cache")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "durable question")
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "durable answer"))))
           (conversation-append-provider-item
            conversation
            (json-object "type" "function_call"
                         "namespace" "skill"
                         "name" "load"
                         "call_id" "volatile-call"
                         "arguments" "{}")
            :persistence ':next-response)
           (let* ((request  (provider-request-object provider conversation #()))
                  (messages (json-get request "messages"))
                  (assistant
                    (find "assistant" messages
                          :test #'string=
                          :key  (lambda (message)
                                  (json-get message "role"))))
                  (content  (json-get assistant "content")))
             (test-assert
              (and (= (length content) 2)
                   (json-string=
                    (json-get (json-get (aref content 0) "cache_control") "type")
                    "ephemeral")
                   (json-string= (json-get (aref content 1) "type") "tool_use")
                   (null (json-get (aref content 1) "cache_control")))
              "ephemeral calls follow the durable-history cache breakpoint"))
           (test-assert
            (not (search "cache_control"
                         (json-encode (conversation-input-items conversation))))
            "request cache annotations never mutate conversation input")
           (test-assert
            (search "cache_control"
                    (json-encode
                     (provider-request-object
                           provider conversation #() :compaction-p t)))
            "compaction keeps the cache breakpoints, so it reads what ordinary requests cached"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> anthropic-provider-test--anchored-context-prefix () null)
(defun anthropic-provider-test--anchored-context-prefix ()
  "Keep an anchored skill body in place before the history cache marker across tool rounds."
  (let* ((*context-contributors*        nil)
         (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
         (*context-last-deliveries*     (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil)
         (body                          (concatenate 'string "ANCHORED-SKILL-BODY "
                                                     (make-string 40000 :initial-element #\s)))
         (configuration                 (anthropic-provider-test--configuration))
         (provider                      (anthropic-provider-create configuration))
         (conversation                  (conversation-create configuration)))
    (register-context-contributor
     "anthropic-anchor-test"
     (lambda (context)
       (declare (ignore context))
       (list (change-class (make-context-contribution :identifier "skill" :instruction body
                                                          :class      ':mandatory)
                           'anchored-context-contribution)
             (make-context-contribution :identifier "state" :instruction "Mutable state."))))
    (conversation-append-user-message conversation "Use the skill.")
    (flet ((placement ()
             "Return the index of the skill body's message and of the history cache marker."
             (let ((messages (json-get (provider-request-object provider conversation #())
                                       "messages")))
               (values (position-if (lambda (message) (search "ANCHORED-SKILL-BODY"
                                                              (json-encode message)))
                                    messages)
                       (position-if (lambda (message) (search "cache_control"
                                                              (json-encode message)))
                                    messages :from-end t)))))
      (multiple-value-bind (body-index marker-index) (placement)
        (conversation-append-provider-item
         conversation (json-object "type" "function_call" "name" "sample"
                                   "call_id" "anchor-round" "arguments" "{}"))
        (conversation-append-provider-item
         conversation (json-object "type" "function_call_output"
                                   "call_id" "anchor-round" "output" "done"))
        (multiple-value-bind (next-body-index next-marker-index) (placement)
          (test-assert (and body-index marker-index (<= body-index marker-index)
                            (eql body-index next-body-index)
                            (< next-body-index next-marker-index))
                       "the skill body keeps its message inside the cached history")))))
  nil)

(-> anthropic-provider-test--inherited-reference-order () null)
(defun anthropic-provider-test--inherited-reference-order ()
  "Test that a child-reference boundary retains its transcript position."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider      (anthropic-provider-create configuration))
         (conversation  (conversation-create configuration)))
    (conversation-append-inherited-reference
     conversation
     "parent-conversation"
     (list
      (json-object
       "type" "message" "role" "user"
       "content" (json-array
                  (json-object "type" "input_text" "text" "Parent question.")))
      (json-object
       "type" "message" "role" "assistant"
       "content" (json-array
                  (json-object "type" "output_text" "text" "Parent answer.")))))
    (conversation-append-provider-item
     conversation
     (json-object
      "type" "message" "role" "developer"
      "content" (json-array
                 (json-object "type" "input_text"
                              "text" "Child guidance."))))
    (conversation-append-user-message conversation "Child assignment.")
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation #())
      (declare (ignore delivery))
      (let* ((messages      (json-get request "messages"))
             (system        (json-get request "system"))
             (final-user    (aref messages (1- (length messages))))
             (final-content (json-get final-user "content")))
        (test-assert
         (equal (loop for message across messages
                      collect (json-get message "role"))
                '("user" "assistant" "user"))
         "inherited history and child input preserve transcript role order")
        (test-assert
         (and (= (length final-content) 4)
              (string= (json-get (aref final-content 0) "text")
                       *conversation-inherited-reference-boundary*)
              (string= (json-get (aref final-content 1) "text")
                       "Child guidance.")
              (string= (json-get (aref final-content 2) "text")
                       "Child assignment.")
              (json-string=
               (json-get
                (json-get (aref final-content 2) "cache_control") "type")
               "ephemeral")
              (search "Temporary context"
                      (json-get (aref final-content 3) "text")))
         "developer guidance and child input retain transcript order")
        (test-assert
         (not
          (some (lambda (block)
                  (or (search *conversation-inherited-reference-boundary*
                              (or (json-get block "text") ""))
                      (search "Child guidance."
                              (or (json-get block "text") ""))))
                (coerce system 'list)))
         "positional developer content is not hoisted into system"))))
  nil)

(-> anthropic-provider-test--compaction-request () null)
(defun anthropic-provider-test--compaction-request ()
  "Keep compaction compatible with thinking and the ordinary cached prefix."
  (with-test-configuration (configuration)
    (dolist (model '("claude-haiku-4-5-20251001" "claude-fable-5-2" "claude-opus-5-5"
                     "claude-sonnet-5-5" "claude-haiku-5-5"))
      (setf (config :model configuration) model
            (config :reasoning-effort configuration) "high")
      (let* ((provider     (anthropic-provider-create configuration))
             (conversation (conversation-create configuration))
             (tools        (continuation-tool-namespaces)))
        (conversation-append-user-message conversation "Conversation to summarize.")
        (test-compaction-request provider conversation :forced-p nil)
        (let* ((ordinary               (provider-request-object provider conversation tools))
               (*provider-tool-choice* *continuation-submit-tool*))
          (multiple-value-bind (request delivery)
              (provider-request-object provider conversation tools
                                       :goal-context "request-local goal"
                                       :compaction-p t)
            (let* ((system   (json-get request "system"))
                   (messages (json-get request "messages"))
                   (submit   (aref (json-get request "tools") 0))
                   (texts    (loop for block across system
                                  collect (json-get block "text"))))
              (test-assert
               (and (equal (json-get (json-get request "tool_choice") "type") "auto")
                    (equalp (json-get ordinary "tool_choice") (json-get request "tool_choice"))
                    (equalp (json-get ordinary "thinking") (json-get request "thinking"))
                    (equalp (json-get ordinary "output_config") (json-get request "output_config"))
                    (json-false-p (json-get (json-get submit "input_schema") "additionalProperties")))
               "compaction preserves automatic choice, thinking, effort and the closed submission schema")
              (test-assert (null delivery)
                           "compaction resolves no request-local context delivery")
              (test-assert (= 1 (length (json-get request "tools")))
                           "compaction requests declare only the submit tool")
              (test-assert
               (and (not (find *compaction-instructions* texts :test #'string=))
                    (test-object-contains-string-p (aref messages (1- (length messages)))
                                                  *compaction-instructions*)
                    (not (test-object-contains-string-p request "request-local goal")))
               "compaction instructions follow the history, and the goal context stays out")))))))
  nil)

(-> anthropic-provider-test--transport () null)
(defun anthropic-provider-test--transport ()
  "Test Anthropic transport endpoints, headers, and UTF-8 request bodies."
  (let* ((configuration    (anthropic-provider-test--configuration))
         (provider         (anthropic-provider-create configuration))
         (conversation     (conversation-create configuration))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "synthetic-anthropic-key"
                          :account-id   "anthropic"
                          :source-path
                          (configuration-api-keys-path configuration)))
         (captured-url     nil)
         (captured-headers nil)
         (captured-content nil)
         (captured-options nil))
    (test-call-with-function-replacements
     (list
      (list
       'dexador:post
       (lambda (url &key headers content want-stream force-string keep-alive
                    connect-timeout read-timeout &allow-other-keys)
         (setf captured-url url
               captured-headers headers
               captured-content content
               captured-options
               (list want-stream force-string keep-alive
                     connect-timeout read-timeout))
         (values (make-string-input-stream "") 200
                 '(("request-id" . "request-transport"))))))
     (lambda ()
       (provider-open-response-stream
        provider
        (json-object "model" "claude-haiku-4-5-20251001" "stream" t)
        :credentials  credentials
        :conversation conversation)))
    (test-assert (string= captured-url *anthropic-messages-endpoint*)
                 "Anthropic turns post to the Messages endpoint")
    (test-assert
     (and (string= (response-header captured-headers "x-api-key")
                   "synthetic-anthropic-key")
          (string= (response-header captured-headers "anthropic-version")
                   *anthropic-api-version*)
          (string= (response-header captured-headers "Accept")
                   "text/event-stream"))
     "Anthropic transport sends static-key, version, and SSE headers")
    (test-assert
     (and (equal captured-options '(t t nil 30 300))
          (typep captured-content '(vector (unsigned-byte 8)))
          (string=
           (json-get
            (json-decode
             (sb-ext:octets-to-string captured-content :external-format ':utf-8))
            "model")
           "claude-haiku-4-5-20251001"))
     "Anthropic transport sends bounded streaming options and UTF-8 JSON")
    (let ((validation-url     nil)
          (validation-headers nil))
      (test-call-with-function-replacements
       (list
        (list
         'dexador:get
         (lambda (url &key headers &allow-other-keys)
           (setf validation-url url
                 validation-headers headers)
           (values "{}" 200 '(("request-id" . "request-validation"))))))
       (lambda ()
         (anthropic-validate-api-key "synthetic-validation-key")))
      (test-assert
       (and (string= validation-url *anthropic-models-endpoint*)
            (string= (response-header validation-headers "x-api-key")
                     "synthetic-validation-key")
            (string= (response-header validation-headers "anthropic-version")
                     *anthropic-api-version*))
       "API-key validation probes the models endpoint with Anthropic headers")))
  nil)

(-> anthropic-provider-test--binding-recovery () null)
(defun anthropic-provider-test--binding-recovery ()
  "Exercise signature-scoped binding recovery at the request transport boundary."
  (with-test-configuration (configuration)
    (setf (config :model configuration) "claude-opus-5-5"
          (config :reasoning-effort configuration) "high")
    (let* ((provider         (anthropic-provider-create configuration))
           (conversation     (conversation-create configuration))
           (signed           (json-object "type" "thinking" "thinking" "reasoning"
                                          "signature" "binding-fixture-signature"))
           (credentials      (make-instance 'oauth-credentials :access-token "fixture-key"))
           (message          "messages.1.content.0: Invalid signature in thinking block, bound to another conversation. Set thinking.block_binding.prefix_mismatch_behavior to drop_block with thinking-binding-controls-2026-08-01.")
           (attempts         0)
           (captured-headers nil))
      (conversation-append-user-message conversation "Question")
      (conversation-append-provider-item
       conversation (json-object "type" "message" "role" "assistant"
                                 "content" (json-array signed
                                                       (json-object "type" "output_text" "text" "Answer"))))
      (conversation-append-user-message conversation "Continue")
      (let ((request (provider-request-object provider conversation #())))
        (test-assert (null (json-get (json-get request "thinking") "block_binding"))
                     "valid signed history has no binding override")
        (test-call-with-function-replacements
         (list (list 'provider-post-event-stream
                     (lambda (url content &key headers)
                       (declare (ignore url content))
                       (push headers captured-headers)
                       (if (= (incf attempts) 1)
                           (error 'provider-error :status 400 :message message)
                           (values (make-string-input-stream "") 200 nil)))))
         (lambda ()
           (provider-open-response-stream provider request :credentials credentials
                                          :conversation                 conversation)))
        (test-assert (and (= attempts 2)
                          (null (response-header (second captured-headers) "anthropic-beta"))
                          (equal (response-header (first captured-headers) "anthropic-beta")
                                 "thinking-binding-controls-2026-08-01")
                          (= (length (anthropic--request-signatures request)) 1))
                     "one recognized binding rejection retries with the required beta and signed block")
        (let* ((loaded  (conversation-load (conversation-pathname conversation)))
               (resumed (provider-request-object provider loaded #())))
          (test-assert (equal (json-get (json-get (json-get resumed "thinking") "block_binding")
                                        "prefix_mismatch_behavior")
                              "drop_block")
                       "reopening a conversation recovers only the rejected signed identity")
          (dolist (mode '("disabled" "between_tools"))
            (let ((probe (json-object "messages" (json-get resumed "messages")
                                      "thinking" (json-object "type" mode))))
              (anthropic--apply-binding-policy probe loaded)
              (test-assert (null (json-get (json-get probe "thinking") "block_binding"))
                           "unsupported thinking modes omit binding controls")))
          (let* ((next-signed (json-object "type" "thinking" "thinking" "next"
                                           "signature" "valid-next-signature"))
                 (next        (json-object "thinking" (json-object "type" "adaptive")
                                           "messages" (json-array
                                                       (json-object "role" "assistant"
                                                                    "content" (json-array next-signed))))))
            (anthropic--apply-binding-policy next loaded)
            (test-assert (and (null (json-get (json-get next "thinking") "block_binding"))
                              (equalp next-signed
                                      (aref (json-get (aref (json-get next "messages") 0)
                                                     "content") 0)))
                         "the next valid signed continuation has no generic override")))
        (dolist (failure (list (make-condition 'provider-error :status 400 :message "Invalid signature")
                              (make-condition 'provider-error :status 500 :message message)))
          (test-assert (null (anthropic--binding-rejected-signature failure request))
                       "unrelated statuses and signature failures are not binding recovery"))
        (setf attempts 0)
        (test-call-with-function-replacements
         (list (list 'provider-post-event-stream
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf attempts)
                       (error 'provider-error :status 400 :message message))))
         (lambda ()
           (test-assert
            (handler-case
                (progn (provider-open-response-stream provider request :credentials credentials
                                                       :conversation                conversation) nil)
              (provider-error () t))
            "a rejected recovery attempt propagates")))
        (test-assert (= attempts 1) "an already recovered request never retries again")
        (let ((digest (first (conversation-anthropic-rejected-thinking conversation))))
          (anthropic--note-binding-rejection conversation digest)
          (test-assert (= 1 (length (conversation-anthropic-rejected-thinking conversation)))
                       "repeated rejected identities are deduplicated"))
        (dotimes (index 33)
          (anthropic--note-binding-rejection
           conversation (context-anchors--digest (format nil "rejection-~D" index))))
        (let ((loaded (conversation-load (conversation-pathname conversation))))
          (test-assert (and (= 32 (length (conversation-anthropic-rejected-thinking loaded)))
                            (equal (first (conversation-anthropic-rejected-thinking loaded))
                                   (context-anchors--digest "rejection-32")))
                       "replay retains only the newest bounded rejection set")))))
  nil)

(-> anthropic-provider-test--effort-controls () null)
(defun anthropic-provider-test--effort-controls ()
  "Check configured effort and effective default thinking at the wire boundary."
  (with-test-configuration (configuration)
    (dolist (model '("claude-fable-5-2" "claude-opus-5-5" "claude-sonnet-5-5" "claude-haiku-5-5"))
      (setf (config :model configuration) model)
      (dolist (effort '("low" "medium" "high" "xhigh" "max"))
        (setf (config :reasoning-effort configuration) effort)
        (let ((request (provider-request-object (anthropic-provider-create configuration)
                                               (conversation-create configuration) #())))
          (test-assert (and (equal (json-get (json-get request "thinking") "type") "adaptive")
                            (equal (json-get (json-get request "output_config") "effort") effort))
                       "each current model sends its configured supported effort")))
      (dolist (effort '(nil "" "none" "disabled" "between_tools" "ultra"))
        (let ((request (json-object "model" model)))
          (anthropic--configure-effort request effort)
          (test-assert (and (null (json-get request "thinking"))
                            (null (json-get request "output_config"))
                            (anthropic--binding-supported-p request))
                       "absent or unsupported effort does not override default adaptive thinking")))
      (dolist (mode '("disabled" "between_tools"))
        (let ((request (json-object "model" model "thinking" (json-object "type" mode))))
          (anthropic--configure-effort request "high")
          (test-assert (and (equal (json-get (json-get request "thinking") "type") mode)
                            (null (json-get request "output_config"))
                            (not (anthropic--binding-supported-p request)))
                       "explicit off modes are preserved and reject binding controls"))))
    (let ((legacy (json-object "model" "claude-haiku-4-5")))
      (anthropic--configure-effort legacy "high")
      (test-assert (null (json-get legacy "thinking")) "legacy effort behavior is unchanged")))
  nil)

(-> anthropic-provider-test--tool-prefix () null)
(defun anthropic-provider-test--tool-prefix ()
  "Preserve dispatched prefixes across tools, input and context changes with bounded markers."
  (dolist (signed-p '(nil t))
    (let ((*context-contributors*        nil)
          (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
          (*context-last-deliveries*     (make-hash-table :test #'equal))
          (*context-last-delivery-order* nil)
          (body                          "PREFIX-CONTEXT-ONE"))
      (with-test-configuration (configuration)
        (setf (config :model configuration) "claude-haiku-5-5")
        (register-context-contributor
         "anthropic-tool-prefix-test"
         (lambda (context)
           (declare (ignore context))
           (list (make-context-contribution :identifier "temporary" :instruction body))))
        (let* ((provider     (anthropic-provider-create configuration))
               (conversation (conversation-create configuration))
               (credentials  (make-instance 'oauth-credentials :access-token "fixture-key"))
               (tools        (continuation-tool-namespaces))
               (previous     nil)
               (sent         nil))
            (labels ((unmarked (messages)
                       "Copy wire messages without cache placement metadata independently."
                       (let ((copy (json-normalize messages)))
                         (loop for message across copy
                               do (loop for block across (json-get message "content")
                                        do (remhash "cache_control" block)))
                         copy))

                     (build ()
                       "Build and dispatch one request, checking the prior wire prefix and marker budget."
                       (let* ((*provider-context-request* (list :conversation conversation))
                              (request                    (provider-request-object provider conversation tools))
                              (messages                   (unmarked (json-get request "messages")))
                              (markers
                                (+ (loop for key in '("system" "tools")
                                         sum (count-if (lambda (block) (json-get block "cache_control"))
                                                       (or (json-get request key) #())))
                                   (loop for message across (json-get request "messages")
                                         sum (count-if (lambda (block) (json-get block "cache_control"))
                                                       (json-get message "content"))))))
                         (when previous
                           (test-assert (and (<= (length previous) (length messages))
                                             (equalp previous (subseq messages 0 (length previous))))
                                        "Every dispatched message is an exact prefix across append-only changes"))
                         (test-assert (<= markers 4) "The complete request uses at most four cache breakpoints")
                         (test-assert
                          (cl-llm-provider-api:wire-request-suffix
                           (getf *provider-context-request* :projection))
                          "Internal canonical projection preserves ordinary admission evidence")
                         (let ((*provider-mission-output-allowance* 128))
                           (provider-open-response-stream provider request :credentials credentials
                                                                          :conversation conversation))
                         (test-assert (and (= (json-get sent "max_tokens") 128)
                                           (> (json-get request "max_tokens") 128))
                                      "Admission dispatches a capped copy without changing the built request")
                         (setf previous messages)
                         request))

                     (tool-round (index)
                       "Append a complete tool round, optionally with signed thinking."
                       (conversation-append-provider-item
                        conversation (json-object "type" "message" "role" "assistant"
                                                  "content" (if signed-p
                                                                (json-array (json-object "type" "thinking" "thinking" "signed"
                                                                                         "signature" (format nil "prefix-signature-~D" index)))
                                                                (json-array (json-object "type" "output_text" "text" "Tool work")))))
                       (conversation-append-provider-item
                        conversation (json-object "type" "function_call" "namespace" "shell" "name" "run"
                                                  "call_id" (format nil "prefix-call-~D" index) "arguments" "{}"))
                       (conversation-append-provider-item
                        conversation (json-object "type" "function_call_output"
                                                  "call_id" (format nil "prefix-call-~D" index) "output" "fresh tool result"))))
             (test-call-with-function-replacements
              (list (list 'provider-post-event-stream
                          (lambda (url content &key headers)
                            (declare (ignore url headers))
                            (setf sent (json-decode (babel:octets-to-string content :encoding ':utf-8)))
                            (values (make-string-input-stream "") 200 nil))))
              (lambda ()
                (conversation-append-user-message conversation "Question")
                (build)
                (let* ((shared       (json-object "nested" (json-object "label" "same")))
                       (wire         (json-array
                                      (json-object "role" "user" "content"
                                                   (json-array
                                                    (json-object "type" "text" "payload" shared)
                                                    (json-object "type" "text" "payload" shared)))))
                       (copy         (anthropic--unmarked-messages wire))
                       (first-block  (aref (json-get (aref copy 0) "content") 0))
                       (second-block (aref (json-get (aref copy 0) "content") 1)))
                  (let* ((first-nested  (json-get (json-get first-block "payload") "nested"))
                         (second-nested (json-get (json-get second-block "payload") "nested"))
                         (first-label   (gethash "label" first-nested))
                         (first-key     (first (loop for key being the hash-keys of first-nested collect key))))
                    (setf (char first-label 0) #\X
                          (char first-key 0) #\X)
                    (test-assert (and (equal (gethash "label" second-nested) "same")
                                      (equal (gethash "label" (json-get (json-get (aref (json-get (aref wire 0) "content") 0) "payload") "nested")) "same"))
                                 "Repeated Anthropic values detach mutable strings and keys"))
                 (multiple-value-bind (tail matches-p)
                     (anthropic--appended-messages
                      (json-array
                       (json-object "role" "user" "content"
                                    (json-array (json-object "type" "text" "text" "Same"))))
                      (json-array
                       (json-object "role" "user" "content"
                                    (json-array (json-object "type" "text" "text" "same")))))
                   (test-assert (and (null matches-p) (zerop (length tail)))
                                "Changed-case prefix content does not reuse the cache"))
                 (let ((request
                         (json-object "messages"
                                      (json-array
                                       (json-object "role" "user" "content"
                                                    (json-array
                                                     (json-object "type" "text" "cache_control" (json-object "type" "ephemeral"))
                                                     (json-object "type" "text" "cache_control" (json-object "type" "ephemeral"))
                                                     (json-object "type" "text" "cache_control" (json-object "type" "ephemeral"))
                                                     (json-object "type" "text" "cache_control" (json-object "type" "ephemeral"))
                                                     (json-object "type" "text" "cache_control" (json-object "type" "ephemeral"))))))))
                   (anthropic--bound-cache-markers request)
                   (test-assert (= 4 (count-if (lambda (block) (json-get block "cache_control"))
                                               (json-get (aref (json-get request "messages") 0) "content")))
                                "Anthropic cache marker admission remains bounded"))
                 (dotimes (index 6)
                   (tool-round index)
                   (let ((request (build)))
                     (test-assert (= 1 (cl-ppcre:count-matches "PREFIX-CONTEXT-ONE" (json-encode request)))
                                  "Unchanged request-local context occurs once across multiple tool rounds")))
                 (setf body "PREFIX-CONTEXT-TWO")
                 (let ((changed (build)))
                   (test-assert (and (= 1 (cl-ppcre:count-matches "PREFIX-CONTEXT-ONE" (json-encode changed)))
                                     (= 1 (cl-ppcre:count-matches body (json-encode changed))))
                                "Changed context is appended once without rewriting the dispatched prefix"))
                 (build)
                 (conversation-append-provider-item
                  conversation (json-object "type" "message" "role" "assistant"
                                            "content" (json-array (json-object "type" "output_text" "text" "Answer"))))
                 (conversation-append-user-message conversation "Fresh user input")
                 (test-assert (search "Fresh user input" (json-encode (build)))
                              "New user input follows the dispatched context without removing it")
                 (conversation-append-user-message conversation "Additional steering input")
                 (test-assert (search "Additional steering input" (json-encode (build)))
                              "Consecutive user input extends the wire tail without rewriting an old message")
                 (setf (config :model configuration) "claude-sonnet-5-5"
                       previous nil)
                 (let ((reset (build)))
                   (test-assert (and (not (search "PREFIX-CONTEXT-ONE" (json-encode reset)))
                                     (search body (json-encode reset)))
                                "Changed request headers start a fresh projection with current context"))
                 (let ((loaded (conversation-load (conversation-pathname conversation))))
                   (test-assert (and (null (conversation-anthropic-request-state loaded))
                                     (not (search "PREFIX-CONTEXT" (json-encode (conversation-input-items loaded)))))
                                "Request-local prefixes are neither durable history nor restart state")))))))))
   nil))
