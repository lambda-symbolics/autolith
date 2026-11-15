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
  "Test Anthropic's compaction request."
  (let* ((configuration (anthropic-provider-test--configuration))
         (provider      (anthropic-provider-create configuration))
         (conversation  (conversation-create configuration)))
    (conversation-append-user-message conversation "Conversation to summarize.")
    (test-compaction-request provider conversation)
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation (continuation-tool-namespaces)
                                 :goal-context "request-local goal"
                                 :compaction-p t)
      (let* ((system   (json-get request "system"))
             (messages (json-get request "messages"))
             (texts    (loop for block across system
                             collect (json-get block "text"))))
        (test-assert (null delivery)
                     "compaction resolves no request-local context delivery")
        (test-assert (= 1 (length (json-get request "tools")))
                     "compaction requests declare only the submit tool")
        (test-assert
         (and (not (find *compaction-instructions* texts :test #'string=))
              (test-object-contains-string-p (aref messages (1- (length messages)))
                                             *compaction-instructions*)
              (not (test-object-contains-string-p request "request-local goal")))
         "compaction instructions follow the history, and the goal context stays out"))))
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
