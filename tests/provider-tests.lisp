(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-provider-rate-limits () null)
(defun test-provider-rate-limits ()
  "Test rate limit header parsing into portable snapshots."
  (let ((snapshot (provider-rate-limit-snapshot
                   '(("x-codex-primary-used-percent" . "28.5")
                     ("x-codex-primary-window-minutes" . "300")
                     ("x-codex-primary-reset-at" . "1783000000")
                     ("x-codex-secondary-used-percent" . "45")
                     ("x-codex-secondary-window-minutes" . "10080")))))
    (test-assert (= (getf (getf snapshot :primary) :window-minutes) 300)
                 "primary rate limit windows parse their minutes")
    (test-assert (= (round (* 10 (getf (getf snapshot :primary)
                                       :used-percent)))
                    285)
                 "decimal used percents parse without the Lisp reader")
    (test-assert (= (getf (getf snapshot :primary) :resets-at)
                    (unix-time->universal-time 1783000000))
                 "reset times convert from the POSIX epoch")
    (test-assert (= (getf (getf snapshot :secondary) :used-percent) 45)
                 "secondary rate limit windows parse")
    (test-assert (null (getf (getf snapshot :secondary) :resets-at))
                 "missing reset headers stay absent"))
  (test-assert (null (provider-rate-limit-snapshot
                      '(("content-type" . "text/event-stream"))))
               "absent rate limit headers produce no snapshot")
  (test-assert
   (search "model_not_found means this model is unavailable"
           (provider--http-error-message
            404
            "{\"error\":{\"message\":\"model_not_found means this model is unavailable\"}}"))
   "HTTP errors surface the provider's own explanation")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration))
         (condition
           (make-condition
            'http-request-failed
            :body "rate limited"
            :status 429
            :headers '(("x-request-id" . "request-429")
                       ("x-codex-primary-used-percent" . "100")
                       ("x-codex-primary-window-minutes" . "300"))
            :uri nil
            :method ':post)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-retryable-error (error)
                (and (= (provider-error-status error) 429)
                     (string= (provider-error-request-id error)
                              "request-429"))))
            "HTTP 429 is eligible for bounded retry")
           (test-assert
            (and (handler-case
                     (progn
                       (provider--signal-http-status-failure
                        provider 408
                        :headers '(("Retry-After" . "3")))
                       nil)
                   (provider-retryable-error (error)
                     (= (provider-error-status error) 408)))
                 (not (provider-retryable-status-p provider 408 nil)))
            "Retry-After makes an otherwise terminal status retryable")
           (test-assert
            (= (getf (getf (provider-rate-limits provider) :primary)
                     :used-percent)
               100)
            "HTTP error headers refresh the visible rate limit snapshot")
           (let ((stream-condition
                   (make-condition
                    'http-request-failed
                    :body (make-string-input-stream
                           "{\"error\":{\"message\":\"input item is not supported\"}}")
                    :status 400
                    :headers '(("request-id" . "request-400"))
                    :uri nil
                    :method ':post)))
             (test-assert
              (handler-case
                  (progn
                    (provider-signal-http-failure provider stream-condition)
                    nil)
                (provider-error (error)
                  (and (= (provider-error-status error) 400)
                       (string= (provider-error-request-id error)
                                "request-400")
                       (search "input item is not supported"
                               (format nil "~A" error))
                       (search "input item is not supported"
                               (or (provider-error-response error) ""))
                       t)))
              "a streamed failure body reaches both the message and the response"))
           (dolist (status '(500 503))
             (let ((transient-condition
                     (make-condition
                      'http-request-failed
                      :body "temporary provider failure"
                      :status status
                      :headers nil
                      :uri nil
                      :method ':post)))
               (test-assert
                (handler-case
                    (progn
                      (provider-signal-http-failure provider transient-condition)
                      nil)
                  (provider-retryable-error (error)
                    (= (provider-error-status error) status)))
                (format nil "HTTP ~D is eligible for bounded retry" status)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--request-tools (json-object) vector)
(defun provider-tests--request-tools (request)
  "Return REQUEST's top-level standard Responses tools."
  (json-get request "tools"))

(-> provider-tests--request-tool-of-type
    (json-object string) (option json-object))
(defun provider-tests--request-tool-of-type (request type)
  "Return REQUEST's first standard Responses tool whose type equals TYPE."
  (find type
        (provider-tests--request-tools request)
        :key (lambda (tool)
               (and (json-object-p tool)
                    (json-get tool "type")))
        :test #'equal))

(-> provider-tests--search-filter-schemas () vector)
(defun provider-tests--search-filter-schemas ()
  "Return one ordinary namespace and the local web namespace."
  (json-array
   (json-object
    "type" "namespace"
    "name" "resource"
    "tools" (json-array
             (json-object "name" "read"
                          "description" "Read a resource."
                          "parameters" (json-object "type" "object"))))
   (json-object
    "type" "namespace"
    "name" "web"
    "tools" (json-array
             (json-object "name" "run"
                          "description" "Search the web."
                          "parameters" (json-object "type" "object"))
             (json-object "name" "gist"
                          "description" "Retrieve one page as Markdown."
                          "parameters" (json-object "type" "object"))))))

(-> test-provider-request-tool-filtering () null)
(defun test-provider-request-tool-filtering ()
  "Test local web schemas are sent only when the request can execute them."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (disabled-configuration
           (configuration--clone base-configuration :web-search-mode "disabled"))
         (schemas (provider-tests--search-filter-schemas)))
    (unwind-protect
         (progn
           (let ((filtered
                   (provider-request-tool-namespaces
                    base-configuration schemas :hosted-web-search-p t))
                 (web (find-if
                       (lambda (entry)
                         (json-string= (json-get entry "name") "web"))
                       (coerce schemas 'list))))
             (test-assert
              (= (length filtered) 2)
              "hosted search suppresses only the provider-backed web.run tool")
             (test-assert
              (and web
                   (= (length (json-get web "tools")) 2))
              "the unfiltered fixture advertises both web tools")
             (let ((entry (find-if
                           (lambda (candidate)
                             (json-string= (json-get candidate "name") "web"))
                           (coerce filtered 'list))))
               (test-assert
                (and entry
                     (= (length (json-get entry "tools")) 1)
                     (json-string=
                      (json-get (aref (json-get entry "tools") 0) "name")
                      "gist"))
                "hosted search keeps the independent web_extra.gist tool")))
           (test-assert
            (and (provider-hosted-web-search-tools-p
                  (list (json-object "type" "web_search_preview")))
                 (not (provider-hosted-web-search-tools-p
                       (list (json-object "type" "code_interpreter")))))
            "only actual hosted search declarations suppress local web tools")
           (let* ((conversation
                    (conversation-create disabled-configuration
                                         :identifier "responses-search-filter"))
                  (provider (provider-create disabled-configuration))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools (json-get request "tools"))
                  (namespace (aref tools 0))
                  (web (aref tools 1)))
             (test-assert
              (and (= (length tools) 3)
                   (json-string= (json-get namespace "type") "namespace")
                   (json-string= (json-get namespace "name") "resource")
                   (json-string= (json-get web "name") "web")
                   (= (length (json-get web "tools")) 1)
                   (json-string=
                    (json-get (aref (json-get web "tools") 0) "name")
                    "gist")
                   (json-string= (json-get (aref tools 2) "type")
                                 "tool_search"))
              "Responses keeps web_extra.gist but omits web.run when search is disabled"))
           (let* ((conversation
                    (conversation-create disabled-configuration
                                         :identifier "chat-search-filter"))
                  (provider
                    (openai-compatible-provider-create
                     disabled-configuration
                     :name "search-filter"
                     :family ':codex
                     :headers nil
                     :reasoning-parameter nil))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools (json-get request "tools")))
             (test-assert
              (and (= (length tools) 2)
                   (string= (json-get
                             (json-get (aref tools 0) "function")
                             "description")
                            "Read a resource.")
                   (string= (json-get
                             (json-get (aref tools 1) "function")
                             "description")
                            "Retrieve one page as Markdown."))
              "Chat Completions keeps web_extra.gist but omits web.run when search is disabled"))
           (let* ((configuration
                    (configuration--clone
                     (configuration-with-model
                      disabled-configuration "claude-haiku-4-5-20251001")
                     :web-search-mode "disabled"))
                  (conversation
                    (conversation-create configuration
                                         :identifier "anthropic-search-filter"))
                  (provider (anthropic-provider-create configuration))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools (json-get request "tools")))
             (test-assert
              (and (= (length tools) 2)
                   (string= (json-get (aref tools 0) "description")
                            "Read a resource.")
                   (string= (json-get (aref tools 1) "description")
                            "Retrieve one page as Markdown."))
              "Anthropic keeps web_extra.gist but omits web.run when search is disabled"))
           (dolist (configuration (list base-configuration disabled-configuration))
             (let* ((filtered
                      (provider-responses-request-namespaces
                       (grok-provider-create configuration) schemas))
                    (web (find "web" filtered
                               :key (lambda (entry) (json-get entry "name"))
                               :test #'equal)))
               (test-assert
                (and web
                     (= (length (json-get web "tools")) 1)
                     (json-string=
                      (json-get (aref (json-get web "tools") 0) "name") "gist"))
                "Grok advertises page retrieval with backend search enabled or disabled")))
           (let ((search-only
                   (json-array
                    (json-object "type" "namespace" "name" "web"
                                 "tools" (json-array (json-object "name" "run"))))))
             (test-assert
              (and (zerop (length (provider-request-tool-namespaces
                                   disabled-configuration search-only)))
                   (zerop (length (provider-responses-request-namespaces
                                   (grok-provider-create base-configuration)
                                   search-only))))
              "filtering the sole search tool omits the empty namespace")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-deferred-tool-loading () null)
(defun test-provider-deferred-tool-loading ()
  "Test Codex native namespace discovery and exact eager fallback."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (schemas
           (json-array
            (json-object
             "type" "namespace"
             "name" "resource"
             "description" "Read and edit model-addressable resources."
             "tools"
             (json-array
              (json-object
               "name" "read"
               "description" "Read one resource."
               "parameters"
               (json-object
                "type" "object"
                "properties" (json-object
                              "uri" (json-object "type" "string")))))))))
    (unwind-protect
         (let* ((configuration
                  (configuration--clone base-configuration
                                        :model "gpt-5.6-terra"
                                        :working-directory root))
                (provider (provider-create configuration))
                (conversation
                  (conversation-create configuration
                                       :identifier "deferred-tools"))
                (request (provider-request-object provider conversation schemas))
                (tools (json-get request "tools"))
                (namespace (aref tools 0))
                (child (aref (json-get namespace "tools") 0))
                (call (json-object "type" "function_call"
                                   "call_id" "call-1"
                                   "namespace" "resource"
                                   "name" "read"
                                   "arguments" "{}")))
           (test-assert
            (and (provider-deferred-tool-loading-p provider)
                 (= (length tools) 2)
                 (json-string= (json-get namespace "type") "namespace")
                 (json-string= (json-get namespace "name") "resource")
                 (json-string= (json-get child "name") "read")
                 (eq (json-get child "defer_loading") t)
                 (json-string= (json-get (aref tools 1) "type") "tool_search"))
            "capable Codex requests defer tools inside native namespaces")
           (test-assert
            (eq (provider-wire-input-item provider call) call)
            "native namespaced calls replay without flattening")
           (let* ((future-configuration
                    (configuration--clone configuration
                                          :model "gpt-5.7-codex"))
                  (future-provider (provider-create future-configuration)))
             (test-assert
              (provider-deferred-tool-loading-p future-provider)
              "future GPT models retain documented deferred-tool support"))
           (let* ((fallback-configuration
                    (configuration--clone configuration :model "gpt-5.3-codex"))
                  (fallback-provider (provider-create fallback-configuration))
                  (fallback-tools (provider-wire-tools fallback-provider schemas))
                  (fallback-call
                    (provider-wire-input-item fallback-provider call)))
             (test-assert
              (and (not (provider-deferred-tool-loading-p fallback-provider))
                   (= (length fallback-tools) 1)
                   (json-string= (json-get (aref fallback-tools 0) "type")
                                 "function")
                   (null (find "tool_search" fallback-tools
                               :key (lambda (tool) (json-get tool "type"))
                               :test #'string=))
                   (null (json-get fallback-call "namespace"))
                   (not (string= (json-get fallback-call "name") "read")))
              "unsupported Codex models retain the exact eager wire fallback"))
           (let* ((eager-characters
                    (length (json-encode
                             (provider-wire-tools
                              (provider-create
                               (configuration--clone configuration
                                                     :model "gpt-5.3-codex"))
                              schemas))))
                  (visible-characters
                    (length
                     (json-encode
                      (json-array
                       (json-object "type" "namespace"
                                    "name" (json-get namespace "name")
                                    "description"
                                    (json-get namespace "description"))
                       (json-object "type" "tool_search"))))))
             (test-assert
              (< visible-characters eager-characters)
              "deferred discovery reduces model-visible schema characters"))
           ;; The server's tool_search_output embeds deferred functions with
           ;; null parameters, which its own input validator refuses.
           (let* ((search-output
                    (json-object
                     "type" "tool_search_output"
                     "status" "completed"
                     "call_id" nil
                     "execution" "server"
                     "tools" (json-array
                              (json-object
                               "type" "namespace"
                               "name" "plan"
                               "tools" (json-array
                                        (json-object
                                         "type" "function"
                                         "name" "update"
                                         "defer_loading" t
                                         "parameters" nil
                                         "output_schema" nil))))))
                  (replayed (provider-wire-input-item provider search-output)))
             (test-assert
              (and (json-string= (json-get replayed "type")
                                 "tool_search_output")
                   (zerop (length (json-get replayed "tools")))
                   (plusp (length (json-get search-output "tools"))))
              "tool search results replay without their invalid expansions")
             (test-assert
              (and (family-private-item-p search-output)
                   (family-private-item-p
                    (json-object "type" "tool_search_call"
                                 "status" "completed")))
              "tool search items never replay into another provider family")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-request () null)
(defun test-provider-request ()
  "Test the standard Codex Responses request shape without network access."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration
                                 :working-directory root)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "request-shape"))
                (provider (provider-create configuration))
                (schemas (json-array
                          (json-object
                           "type" "namespace"
                           "name" "test"
                           "description" "Test tools."
                           "tools"
                           (json-array
                            (json-object
                             "name" "inspect"
                             "description" "Inspect a value."
                             "parameters" (json-object "type" "object"))))))
                (request nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert (provider-child-reference-history-p provider)
                        "Codex enables inherited child reference history")
           (test-assert (null (json-get request "service_tier"))
                        "standard Codex requests omit the service tier")
           (let* ((model (first *codex-fast-mode-models*))
                  (fast-configuration
                    (configuration-with-codex-fast-mode
                     (configuration--clone configuration :model model) t))
                  (fast-request
                    (provider-request-object
                     (provider-create fast-configuration) conversation schemas)))
             (test-assert
              (and (configuration-codex-fast-mode-active-p fast-configuration)
                   (string= (json-get fast-request "service_tier") "priority"))
              "a supported Codex model requests the Fast service tier"))
           (let* ((unknown-configuration
                    (configuration-with-codex-fast-mode
                     (configuration--clone configuration
                                           :model "gpt-future-unknown")
                     t))
                  (unknown-provider (provider-create unknown-configuration))
                  (unknown-request
                    (provider-request-object
                     unknown-provider conversation schemas)))
             (test-assert
              (and (configuration-codex-fast-mode-p unknown-configuration)
                   (not (configuration-codex-fast-mode-active-p
                         unknown-configuration))
                   (null (json-get unknown-request "service_tier")))
              "unknown Codex models use the standard service tier"))
           (test-assert (null (json-get request "max_output_tokens"))
                        "requests omit the output ceiling when none is bound")
           (let ((input (json-get request "input")))
             (test-assert
              (and (= (length input) 2)
                   (string= (json-get (aref input 0) "role") "user")
                   (string= (json-get (aref input 1) "role") "developer")
                   (search "Current workspace agenda"
                           (context--message-text (aref input 1))))
              "standard Responses input ends with mutable request context"))
           (test-assert
            (non-empty-string-p (json-get request "instructions"))
            "standard Responses uses top-level stable instructions")
           (let* ((goal-text "<goal_context>persist</goal_context>")
                  (goal-request
                    (provider-request-object
                     provider conversation schemas :goal-context goal-text))
                  (goal-input (json-get goal-request "input")))
             (test-assert
              (and (= (length goal-input) 3)
                   (search goal-text
                           (context--message-text (aref goal-input 1)))
                   (search "Current workspace agenda"
                           (context--message-text (aref goal-input 2))))
              "goal and mutable context follow durable conversation input")
             (test-assert
              (not (search goal-text (json-get goal-request "instructions")))
              "goal context stays outside the stable instructions"))
           (with-recursive-lock-held (*agenda-lock*)
             (let ((state (agenda-load configuration)))
               (agenda-add :configuration configuration
                           :state state
                           :text "cache-prefix mutation"
                           :status ':doing
                           :memory-identifiers nil)))
           (let* ((mutated-request
                    (provider-request-object provider conversation schemas))
                  (mutated-input (json-get mutated-request "input")))
             (test-assert
              (string= (json-get request "instructions")
                       (json-get mutated-request "instructions"))
              "an agenda mutation preserves byte-identical stable instructions")
             (test-assert
              (search "cache-prefix mutation"
                      (context--message-text
                       (aref mutated-input (1- (length mutated-input)))))
              "an agenda mutation appears in trailing request context"))
           (test-assert
            (null (provider-web-search-tool configuration))
            "the nonfunctional native web search tool stays disabled")
           (test-assert
            (null (provider-tests--request-tool-of-type request "web_search"))
            "requests omit the nonfunctional native web search tool")
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "max")
            "the provider request maps Ultra reasoning to Max")
           (multiple-value-bind (value present-p)
               (gethash "summary" (json-get request "reasoning"))
             (declare (ignore value))
             (test-assert (not present-p)
                          "hidden traces do not request reasoning summaries"))
           (let* ((trace-provider
                    (provider-create configuration :reasoning-summaries-p t))
                  (trace-request
                    (provider-request-object
                     trace-provider conversation schemas))
                  (trace-reasoning (json-get trace-request "reasoning")))
             (test-assert
              (string= (json-get trace-reasoning "summary") "auto")
              "visible traces request the best supported reasoning summary")
             (let ((compaction-reasoning
                     (json-get
                      (provider-request-object
                       trace-provider conversation schemas :compaction-p t)
                      "reasoning")))
               (multiple-value-bind (value present-p)
                   (gethash "summary" compaction-reasoning)
                 (declare (ignore value))
                 (test-assert
                  (not present-p)
                  "side-channel compaction does not request unused summaries")))
             (setf (provider-rate-limits trace-provider)
                   '((:primary (:used-percent 42))))
             (let* ((reconfiguration
                      (configuration-with-reasoning-effort configuration "high"))
                    (reconfigured
                      (provider-with-configuration
                       trace-provider reconfiguration)))
               (test-assert
                (and (eq (class-of reconfigured) (class-of trace-provider))
                     (eq (provider-configuration reconfigured) reconfiguration)
                     (eq (model-provider-registration reconfigured)
                         (model-provider-registration trace-provider))
                     (eq (provider-credential-manager reconfigured)
                         (provider-credential-manager trace-provider))
                     (string= (provider-session-id reconfigured)
                              (provider-session-id trace-provider))
                     (provider-reasoning-summaries-p reconfigured)
                     (equal (provider-rate-limits reconfigured)
                            (provider-rate-limits trace-provider))
                     (not (eq (provider-rate-limits reconfigured)
                              (provider-rate-limits trace-provider))))
                "Codex reconfiguration preserves copied protocol and session state")))
           (test-assert
            (and (string= (json-get request "tool_choice") "auto")
                 (eq (json-get request "parallel_tool_calls") t)
                 (eq (json-get request "store") false)
                 (eq (json-get request "stream") t))
            "standard Responses requests carry their transport controls")
           (test-assert
            (equalp (json-get request "include")
                    (json-array "reasoning.encrypted_content"))
            "the provider request retains encrypted reasoning for replay")
           (test-assert
            (string= (json-get request "prompt_cache_key")
                     (conversation-identifier conversation))
            "the root conversation is the stable prompt cache key")
           (let* ((child-conversation
                    (conversation-create
                     configuration
                     :identifier "child-request-shape"
                     :prompt-cache-key
                     (conversation-prompt-cache-key conversation)))
                  (child-provider
                    (provider-with-configuration provider configuration)))
             (conversation-append-user-message child-conversation "inspect")
             (let ((child-request
                     (provider-request-object
                      child-provider child-conversation schemas)))
               (test-assert
                (not (string= (conversation-identifier child-conversation)
                              (conversation-identifier conversation)))
                "parent and child conversations keep distinct thread identities")
               (test-assert
                (string= (json-get child-request "prompt_cache_key")
                         (json-get request "prompt_cache_key"))
                "reconfigured child providers share the parent prompt cache key")))
           (let* ((other-conversation
                    (conversation-create configuration
                                         :identifier "other-request-shape"))
                  (other-request
                    (provider-request-object provider other-conversation schemas)))
             (test-assert
              (not (string= (json-get other-request "prompt_cache_key")
                            (json-get request "prompt_cache_key")))
              "independent root conversations keep isolated prompt cache keys"))
           (test-assert
            (string= (json-get (json-get request "text") "verbosity") "low")
            "the provider request asks for restrained text verbosity")
           (let* ((fallback-configuration
                    (configuration--clone configuration :model "gpt-5.3-codex"))
                  (fallback-provider (provider-create fallback-configuration))
                  (tools (provider-wire-tools fallback-provider schemas))
                  (wire-name
                    (provider-wire-tool-name fallback-provider "test" "inspect")))
             (test-assert
              (and (= (length tools) 1)
                   (string= (json-get (aref tools 0) "name") wire-name)
                   (provider-wire-function-name--valid-p wire-name))
              "eager Responses fallback uses the shared grammar-safe tool codec"))
           (let ((compaction-request
                   (provider-request-object
                    provider conversation schemas :compaction-p t)))
             (test-assert
              (and (eq (json-get compaction-request "parallel_tool_calls") false)
                   (zerop (length (json-get compaction-request "tools")))
                   (search "context checkpoint compaction"
                           (json-get compaction-request "instructions")))
              "portable compaction fallback is tool-free and serial"))
           (let* ((fallback-configuration
                    (configuration--clone configuration :model "gpt-5.3-codex"))
                  (fallback-provider (provider-create fallback-configuration))
                  (local-call
                    (json-object
                     "type" "function_call"
                     "namespace" "test"
                     "name" "inspect"
                     "call_id" "call-standard"))
                  (wire-call
                    (provider-wire-input-item fallback-provider local-call))
                  (normalized
                    (provider-normalize-output-item
                     fallback-provider (json-object-copy wire-call))))
             (test-assert
              (and (null (json-get wire-call "namespace"))
                   (provider-wire-function-name--valid-p
                    (json-get wire-call "name"))
                   (string= (json-get normalized "namespace") "test")
                   (string= (json-get normalized "name") "inspect"))
              "eager Codex wire hooks round-trip the local tool namespace")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-native-compaction () null)
(defun test-provider-native-compaction ()
  "Test the Codex native checkpoint request, transport, and fallback boundary."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "native-request"))
         (provider (provider-create configuration))
         (credentials (provider-tests--credentials configuration))
         (schemas
           (json-array
            (json-object "type" "namespace"
                         "name" "test"
                         "description" "Test tools."
                         "tools" (json-array)))))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "retain this work")
           (conversation-append-provider-item
            conversation
            (json-object "type" "reasoning"
                         "encrypted_content" "retained-reasoning"
                         "summary" (json-array)))
           (conversation-append-provider-item
            conversation
            (json-object "type" "function_call"
                         "call_id" "ephemeral-call"
                         "name" "test"
                         "arguments" "{}")
            :persistence ':next-response)
           (let* ((request
                    (provider-native-compaction-request-object
                     provider conversation schemas))
                  (input (json-get request "input")))
             (test-assert
              (and (= (length input) 2)
                   (string= (json-get (aref input 0) "role") "user"))
              "native compaction carries durable standard Responses input")
             (test-assert
              (and (reasoning-item-p (aref input 1))
                   (string= (json-get (aref input 1) "encrypted_content")
                            "retained-reasoning"))
              "native compaction carries retained encrypted reasoning")
             (test-assert
              (non-empty-string-p (json-get request "instructions"))
              "native compaction uses top-level instructions")
             (test-assert
              (string= (json-get request "prompt_cache_key")
                       (conversation-prompt-cache-key conversation))
              "native compaction shares the root conversation cache key")
             (test-assert (null (json-get request "service_tier"))
                          "standard native compaction omits a service tier")
             (let* ((fast-configuration
                      (configuration-with-codex-fast-mode configuration t))
                    (fast-provider (provider-create fast-configuration))
                    (fast-request
                      (provider-native-compaction-request-object
                       fast-provider conversation schemas)))
               (test-assert
                (string= (json-get fast-request "service_tier") "priority")
                "Codex Fast mode applies to native compaction"))
             (let* ((unknown-configuration
                      (configuration-with-codex-fast-mode
                       (configuration--clone configuration
                                             :model "gpt-future-unknown")
                       t))
                    (unknown-provider (provider-create unknown-configuration))
                    (unknown-request
                      (provider-native-compaction-request-object
                       unknown-provider conversation schemas)))
               (test-assert
                (null (json-get unknown-request "service_tier"))
                "unknown Codex models omit Fast mode during compaction"))
             (dolist (name '("stream" "store" "include" "tool_choice"
                             "parallel_tool_calls" "reasoning" "text" "tools"))
               (multiple-value-bind (value present-p) (gethash name request)
                 (declare (ignore value))
                 (test-assert (not present-p)
                              (format nil "native compaction omits ~A" name)))))
           (let ((captured-url nil)
                 (captured-headers nil)
                 (captured-content nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (url &key headers content &allow-other-keys)
                  (setf captured-url url
                        captured-headers headers
                        captured-content content)
                  (values "{\"output\":[]}" 200 nil))))
              (lambda ()
                (provider-open-native-compaction
                 provider
                 (json-object "model" *default-model*)
                 :credentials credentials
                 :conversation conversation)))
             (flet ((header (name)
                      (rest (assoc name captured-headers :test #'string-equal))))
               (test-assert
                (string= captured-url
                         "https://chatgpt.com/backend-api/codex/responses/compact")
                "native compaction derives the Responses compact endpoint")
               (test-assert (string= (header "Accept") "application/json")
                            "native compaction requests one JSON response")
               (test-assert
                (typep captured-content '(array (unsigned-byte 8) (*)))
                "native compaction sends JSON as direct UTF-8 octets")
               (test-assert
                (string=
                 (json-get
                  (json-decode
                   (sb-ext:octets-to-string
                    captured-content :external-format ':utf-8))
                  "model")
                 *default-model*)
                "direct UTF-8 request content retains its JSON payload")
               (test-assert
                (string= (header "Authorization")
                         "Bearer provider-test-access-7f386d")
                "native compaction uses the ordinary subscription credentials")))
           (credential-source-save
            (credential-manager-primary-source
             (provider-credential-manager provider))
            credentials)
           (let ((result nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values
                   (json-encode
                    (json-object
                     "output"
                     (json-array
                      (json-object
                       "type" "message"
                       "role" "assistant"
                       "content"
                       (json-array
                        (json-object "type" "output_text"
                                     "text" "Ignored compact reply.")))
                      (json-object "id" "cmp_123"
                                   "type" "compaction"
                                   "encrypted_content" "opaque-checkpoint"))))
                   200
                   '(("x-request-id" . "native-success"))))))
              (lambda ()
                (setf result
                      (provider-native-compact-conversation
                       provider conversation
                       :tool-namespaces schemas
                       :event-callback #'identity))))
             (test-assert (native-compaction-item-p result)
                          "native compaction finds its opaque checkpoint among output")
             (multiple-value-bind (value present-p) (gethash "id" result)
               (declare (ignore value))
               (test-assert (not present-p)
                            "native compaction drops transient server item identifiers")))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")
                       (json-object "type" "context_compaction"
                                    "encrypted_content" "current-checkpoint")))))))
             (test-assert
              (and (native-compaction-item-p result)
                   (string= (json-get result "type") "context_compaction")
                   (string= (json-get result "encrypted_content")
                            "current-checkpoint"))
              "native compaction keeps the newest current checkpoint encoding"))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")))))))
             (test-assert
              (string= (json-get result "type") "compaction")
              "native compaction canonicalizes Codex's legacy checkpoint alias"))
           (test-assert
            (null
             (provider--decode-native-compaction-response
              provider (json-encode (json-object "output" (json-array)))))
            "an opaque-free compact transcript falls back to the portable summary")
           (test-call-with-function-replacements
            (list
             (list
              'dexador:post
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (values "not found" 404 nil))))
            (lambda ()
              (test-assert
               (null
                (provider-native-compact-conversation
                 provider conversation
                 :tool-namespaces schemas
                 :event-callback #'identity))
               "an unavailable native endpoint falls back without failing compaction"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-sse-event-string (json-object) string)
(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

(defclass test-character-input-stream
    (sb-gray:fundamental-character-input-stream)
  ((source
    :initarg :source
    :reader test-character-input-stream-source
    :type string
    :documentation "The deterministic character source.")
   (position
    :initform 0
    :accessor test-character-input-stream-position
    :type integer
    :documentation "The next source character offset."))
  (:documentation "A test stream implementing character reads but not line reads."))

(defmethod sb-gray:stream-read-char ((stream test-character-input-stream))
  "Read one character from STREAM, returning the Gray-stream EOF marker at its end."
  (let ((position (test-character-input-stream-position stream))
        (source (test-character-input-stream-source stream)))
    (if (< position (length source))
        (prog1 (char source position)
          (incf (test-character-input-stream-position stream)))
        :eof)))

(defclass test-failing-close-stream (test-character-input-stream)
  ((close-abort-p
    :initform nil
    :accessor test-failing-close-stream-close-abort-p
    :type boolean
    :documentation "Whether the attempted close was abortive."))
  (:documentation "A deterministic provider stream whose close operation fails."))

(defmethod close ((stream test-failing-close-stream) &key abort)
  "Record ABORT and inject the low-level TLS cleanup failure under test."
  (setf (test-failing-close-stream-close-abort-p stream)
        (not (null abort)))
  (error 'ssl-error-syscall
         :queue nil
         :printed-queue nil
         :ret -1
         :handle nil
         :syscall 'close))

(define-condition test-provider-tls-error (cl+ssl-error)
  ((detail
    :initarg :detail
    :reader test-provider-tls-error-detail
    :type string
    :documentation "The synthetic TLS diagnostic shown by the condition."))
  (:report
   (lambda (condition stream)
     (write-string (test-provider-tls-error-detail condition) stream)))
  (:documentation "A TLS failure with deterministic model-visible detail."))

(defclass test-transport-provider (codex-subscription-provider)
  ((outcomes
    :initarg :outcomes
    :accessor test-transport-provider-outcomes
    :type list
    :documentation "The connection outcomes returned or signaled in order.")
   (attempt-count
    :initform 0
    :accessor test-transport-provider-attempt-count
    :type integer
    :documentation "The number of response streams requested."))
  (:documentation "A subscription provider injecting transport boundary outcomes."))

(defmethod provider-open-response-stream
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Return or signal the next scripted transport outcome for PROVIDER."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (cond
      ((eq outcome ':syscall)
       (error 'ssl-error-syscall
              :queue nil
              :printed-queue nil
              :ret -1
              :handle nil
              :syscall 'connect))
      ((eq outcome ':tls)
       (error 'test-provider-tls-error
              :detail "certificate verification failed"))
      ((and (consp outcome) (eq (first outcome) ':tls))
       (error 'test-provider-tls-error :detail (second outcome)))
      ((eq outcome ':name-service)
       (error 'sb-bsd-sockets:name-service-error
              :errno 1
              :symbol 'getaddrinfo
              :syscall 'getaddrinfo))
      ((eq outcome ':simple-error)
       (error "Synthetic provider transport failure."))
      ((consp outcome)
       (values-list outcome))
      (t
       (values outcome 200 nil)))))

(defmethod provider-open-native-compaction
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Signal the next scripted transport outcome for native compaction."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (cond
      ((eq outcome ':name-service)
       (error 'sb-bsd-sockets:name-service-error
              :errno 1
              :symbol 'getaddrinfo
              :syscall 'getaddrinfo))
      ((eq outcome ':tls)
       (error 'test-provider-tls-error
              :detail "compaction certificate verification failed"))
      ((and (consp outcome) (eq (first outcome) ':tls))
       (error 'test-provider-tls-error :detail (second outcome)))
      (t
       (values outcome 200 nil)))))

(-> provider-tests--completed-sse-source (string) string)
(defun provider-tests--completed-sse-source (response-id)
  "Return a minimal successful SSE response carrying RESPONSE-ID."
  (concatenate
   'string
   (test-sse-event-string
    (json-object
     "type" "response.created"
     "response" (json-object "id" response-id)))
   (test-sse-event-string
    (json-object
     "type" "response.completed"
     "response" (json-object
                 "id" response-id
                 "usage" (json-object "input_tokens" 1))))))

(-> provider-tests--transport-provider
    (configuration list)
    test-transport-provider)
(defun provider-tests--transport-provider (configuration outcomes)
  "Return a test provider yielding transport OUTCOMES."
  (make-instance
   'test-transport-provider
   :configuration configuration
   :credential-manager (credential-manager-create configuration)
   :session-id (make-identifier)
   :outcomes outcomes))

(-> test-provider-transport-boundary () null)
(defun test-provider-transport-boundary ()
  "Test connection normalization and failure-proof provider stream cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-transport"))
         (credentials (provider-tests--credentials configuration)))
    (unwind-protect
         (progn
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "transport-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list :syscall success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "transport-retry-success")
                  "an open-time TLS syscall failure reconnects successfully")
                 (test-assert
                  (= (test-transport-provider-attempt-count provider) 2)
                  "a transient open failure consumes one bounded retry"))))
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "name-service-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list ':name-service success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "name-service-retry-success")
                  "an SBCL name-service error reconnects successfully")
                 (test-assert
                  (= (test-transport-provider-attempt-count provider) 2)
                  "an SBCL name-service error remains inside the retry boundary"))))
           (let* ((*provider-active-credential-values* '("tls-secret"))
                  (*provider-active-credential-redaction-marker* "[redacted]")
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list '(:tls "certificate rejected tls-secret")))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-response-stream
                     provider
                     (json-object)
                     :credentials credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (and
                   (not (typep condition 'provider-retryable-error))
                   (string=
                    (autolith-error-message condition)
                    "The provider TLS connection could not be established: certificate rejected [redacted]"))))
              "TLS failures append useful credential-redacted condition detail"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list '(:tls "compaction certificate expired")))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-native-compaction
                     provider
                     (json-object)
                     :credentials credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (string=
                   (autolith-error-message condition)
                   "The provider TLS connection could not be established: compaction certificate expired")))
              "native compaction preserves TLS condition detail"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list ':name-service))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-native-compaction
                     provider
                     (json-object)
                     :credentials credentials
                     :conversation conversation)
                    nil)
                (provider-transport-error ()
                  t))
              "native compaction normalizes an SBCL name-service error"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list ':simple-error))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-response-stream
                     provider
                     (json-object)
                     :credentials credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (and (not (typep condition 'provider-retryable-error))
                       (string=
                        (autolith-error-message condition)
                        "The provider transport failed before a response was received: Synthetic provider transport failure."))))
              "a raw transport SIMPLE-ERROR becomes a terminal provider failure"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source
                     (provider-tests--completed-sse-source
                      "close-failure-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((result
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback #'identity
                      :force-refresh nil
                      :goal-context nil
                      :compaction-p nil)))
               (test-assert
                (string= (provider-result-response-id result)
                         "close-failure-success")
                "cleanup failure cannot replace a completed provider result")
               (test-assert
                (test-failing-close-stream-close-abort-p stream)
                "provider response streams close abortively")))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "data: {"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity
                     :force-refresh nil
                     :goal-context nil
                     :compaction-p nil)
                    nil)
                (provider-protocol-error (condition)
                  (not (typep condition 'provider-retryable-error))))
              "cleanup failure preserves the terminal stream protocol failure")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "protocol-invalid provider streams also close abortively"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "retry buffer failure"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list (list stream 507 nil)))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (test-call-with-function-replacements
                   (list
                    (list
                     'provider--signal-http-status-failure
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "synthetic status normalization failure"))))
                   (lambda ()
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback #'identity
                      :force-refresh nil
                      :goal-context nil
                      :compaction-p nil)))
                (simple-error ()
                  t))
              "status normalization failures retain their original condition")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "non-success provider responses remain under stream cleanup"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list :tls))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity
                     :force-refresh nil
                     :goal-context nil
                     :compaction-p nil)
                    nil)
                (provider-error (condition)
                  (not (typep condition 'provider-retryable-error))))
              "non-transient TLS setup failures remain typed and terminal")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--credentials (configuration) oauth-credentials)
(defun provider-tests--credentials (configuration)
  "Return four distinct synthetic credentials for provider containment tests."
  (make-instance
   'oauth-credentials
   :access-token "provider-test-access-7f386d"
   :refresh-token "provider-test-refresh-a280c4"
   :id-token "provider-test-identity-f969b1"
   :account-id "provider-test-account-a0542e"
   :expires-at nil
   :source-path (configuration-auth-path configuration)))

(-> provider-tests--assert-credential-free (t list string) null)
(defun provider-tests--assert-credential-free (root secrets description)
  "Assert ROOT contains none of SECRETS, reporting DESCRIPTION."
  (dolist (secret secrets)
    (test-assert
     (not (test-object-contains-string-p root secret))
     description))
  nil)

(-> test-provider-credential-echo-containment () null)
(defun test-provider-credential-echo-containment ()
  "Test provider wire data cannot echo request credentials into retained state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-secret-echo"))
         (provider (provider-create configuration))
         (credentials (provider-tests--credentials configuration))
         (secrets (oauth-credentials-secret-values credentials)))
    (labels
        ((attempt (response-function event-callback)
           "Run one real provider attempt against RESPONSE-FUNCTION."
           (test-call-with-function-replacements
            (list
             (list
              'provider-open-response-stream
              (lambda (active-provider request
                       &key active-credentials active-conversation
                       &allow-other-keys)
                (declare
                 (ignore active-provider request active-credentials
                         active-conversation))
                (funcall response-function))))
            (lambda ()
              (provider-attempt-turn
               provider
               conversation
               :tool-namespaces #()
               :event-callback event-callback
               :force-refresh nil
               :goal-context nil
               :compaction-p nil)))))
      (unwind-protect
           (progn
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let* ((events nil)
                    (source
                      (concatenate
                       'string
                       (test-sse-event-string
                        (json-object
                         "type" "response.created"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_text.delta"
                         "delta"
                         (oauth-credentials-refresh-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.reasoning_summary_text.delta"
                         "output_index" 0
                         "summary_index" 0
                         "delta"
                         (oauth-credentials-id-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_item.done"
                         "item"
                         (json-object
                          "type" "message"
                          "role" "assistant"
                          "content"
                          (json-array
                           (json-object
                            "type" "output_text"
                            "text"
                            (oauth-credentials-account-id credentials))))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.completed"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials)
                          "usage"
                          (json-object
                           "echo"
                           (coerce secrets 'vector)))))))
                    (headers
                      (list
                       (cons "x-request-id"
                             (oauth-credentials-account-id credentials))
                       (cons "x-codex-turn-state"
                             (format nil "~{~A~^/~}" secrets))))
                    (result
                      (attempt
                       (lambda ()
                         (values
                          (make-instance
                           'test-character-input-stream
                           :source source)
                          200
                          headers))
                       (lambda (event)
                         (push event events)))))
               (provider-tests--assert-credential-free
                (list result events)
                secrets
                "successful provider results and callbacks contain no credential")
               (test-assert
                (test-object-contains-string-p
                 (list result events)
                 *provider-credential-redaction-marker*)
                "successful credential echoes carry an explicit redaction marker")
               (test-assert
                (and (provider-result-response-id result)
                     (provider-result-usage result)
                     (provider-result-turn-state result)
                     (find-if
                      (lambda (event)
                        (typep event 'assistant-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'reasoning-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'provider-item-event))
                      events))
                "successful containment retains each semantic provider channel"))
             (let* ((source
                      (test-sse-event-string
                       (json-object
                        "type" "response.failed"
                        "response"
                        (json-object
                         "id" (oauth-credentials-access-token credentials)
                         "error"
                         (json-object
                          "code" "invalid_prompt"
                          "message"
                          (oauth-credentials-refresh-token credentials)
                          "request_id"
                          (oauth-credentials-account-id credentials))))))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                nil))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "structured provider failures contain no credential")
               (test-assert
                (test-object-contains-string-p
                 condition
                 *provider-credential-redaction-marker*)
                "structured provider failures retain a redaction marker"))
             (let* ((source
                      (format
                       nil
                       "data: {\"type\":\"~A~%~%"
                       (oauth-credentials-access-token credentials)))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                (list
                                 (cons
                                  "x-request-id"
                                  (oauth-credentials-id-token credentials)))))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "malformed provider events contain no credential"))
             (dolist (signaled-p '(nil t))
               (let ((condition
                       (handler-case
                           (progn
                             (attempt
                              (lambda ()
                                (let ((headers
                                        (list
                                         (cons
                                          "x-request-id"
                                          (oauth-credentials-id-token
                                           credentials))))
                                      (body
                                        (json-encode
                                         (json-object
                                          "error"
                                          (json-object
                                           "message"
                                           (oauth-credentials-refresh-token
                                            credentials))))))
                                  (if signaled-p
                                      (error
                                       (make-condition
                                        'http-request-failed
                                        :body body
                                        :status 400
                                        :headers headers
                                        :uri nil
                                        :method ':post))
                                      (values
                                       (make-string-input-stream body)
                                       400
                                       headers))))
                              #'identity)
                             nil)
                         (provider-error (failure)
                           failure))))
                 (provider-tests--assert-credential-free
                  condition
                  secrets
                  "HTTP provider failures contain no credential")
                 (test-assert
                  (test-object-contains-string-p
                   condition
                   *provider-credential-redaction-marker*)
                  "HTTP credential echoes carry a redaction marker")))
             (let* ((collision "PROVIDER")
                    (marker
                      (safe-redaction-marker
                       *provider-credential-redaction-marker*
                       (list collision))))
               (test-assert
                (not (search collision marker))
                "credential collisions select a marker without the credential")))
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist ':ignore)))
    (test-assert
     (and (null *provider-active-credential-values*)
          (null *provider-active-credential-redaction-marker*))
     "provider attempts retain no dynamic credential redaction state"))
  nil)

(defclass test-codex-provider (codex-subscription-provider)
  ((outcomes
    :initarg :outcomes
    :accessor test-codex-provider-outcomes
    :type list
    :documentation "The attempt outcomes returned in order.")
   (refresh-flags
    :initform nil
    :accessor test-codex-provider-refresh-flags
    :type list
    :documentation "The force-refresh values observed by attempts."))
  (:documentation "A direct-provider test double for bounded authentication retries."))

(defmethod provider-attempt-turn
    ((provider test-codex-provider)
     (conversation conversation)
     &key
       tool-namespaces
       event-callback
       force-refresh
       goal-context
       compaction-p)
  "Return the next scripted PROVIDER outcome and record FORCE-REFRESH."
  (declare (ignore conversation tool-namespaces event-callback goal-context
                   compaction-p))
  (push force-refresh (test-codex-provider-refresh-flags provider))
  (let ((outcome (pop (test-codex-provider-outcomes provider))))
    (cond
      ((typep outcome 'provider-result)
       outcome)
      ((eq outcome :unauthorized)
       (error 'provider-unauthorized
              :message "Injected unauthorized response."
              :status 401
              :request-id nil
              :response nil))
      ((eq outcome :stream-error)
       (error 'response-stream-error
              :message "Injected stream interruption."
              :status nil
              :request-id nil
              :response nil))
      ((eq outcome :server-error)
       (error 'provider-retryable-error
              :message "Injected transient server failure."
              :status nil
              :code "server_error"
              :request-id "request-server-error"
              :response-id "response-server-error"
              :response nil))
      ((eq outcome :overloaded)
       (error 'provider-retryable-error
              :message "Injected provider overload."
              :status nil
              :code "server_is_overloaded"
              :request-id "request-overloaded"
              :response-id "response-overloaded"
              :response nil))
      ((eq outcome :slow-down)
       (error 'provider-retryable-error
              :message "Injected provider slowdown."
              :status nil
              :code "slow_down"
              :request-id "request-slow-down"
              :response-id "response-slow-down"
              :response nil))
      ((eq outcome :resample)
       (error 'provider-resample-requested
              :message "Injected provider loop report."
              :status nil
              :request-id nil
              :response nil
              :triggers (list "tail_repetition:4@thinking")
              :attempt 1
              :maximum-attempts 2))
      (t
       (error "Invalid scripted provider outcome ~S." outcome)))))

(-> test-codex-provider-create (configuration list) test-codex-provider)
(defun test-codex-provider-create (configuration outcomes)
  "Return a test direct provider yielding OUTCOMES."
  (make-instance 'test-codex-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)
                 :outcomes outcomes))

(-> test-provider-authentication-retries () null)
(defun test-provider-authentication-retries ()
  "Test immediate bounded credential refresh and final unauthorized normalization."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "provider-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "retry-success"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (progn
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized result))))
             (test-assert
              (eq (provider-stream-turn provider conversation
                                        :tool-namespaces #()
                                        :event-callback #'identity)
                  result)
              "one forced refresh may satisfy the first unauthorized response")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil t))
              "the first retry forces credential refresh"))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    '(:unauthorized :unauthorized))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (authentication-error ()
                  t))
              "a second unauthorized response becomes a typed authentication failure")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil t))
              "authentication failure follows one immediate forced refresh"))
           (let* ((manager
                    (api-key-credential-manager-create
                     :provider-name "retry-api-key"
                     :pathname (configuration-api-keys-path configuration)))
                  (provider
                    (make-instance 'test-codex-provider
                                   :configuration configuration
                                   :credential-manager manager
                                   :session-id (make-identifier)
                                   :outcomes '(:unauthorized :unexpected))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (authentication-error (condition)
                  (search "run autolith auth retry-api-key"
                          (princ-to-string condition))))
              "a rejected static API key gives its direct authentication hint")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil))
              "a rejected static API key is not retried or refreshed")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-persistent-transient-retries () null)
(defun test-provider-persistent-transient-retries ()
  "Test bounded jittered retries and complete-attempt DNS normalization."
  (let ((*provider-maximum-transient-retries* 3)
        (attempts 0)
        (events nil)
        (sleeps nil))
    (test-assert
     (eq ':ok
         (provider--call-with-transient-retries
          (lambda ()
            (incf attempts)
            (when (<= attempts 3)
              (error 'provider-retryable-error
                     :message "Injected transient failure."
                     :status nil
                     :request-id nil
                     :response-id nil
                     :response nil))
            ':ok)
          (lambda (event)
            (push event events))
          :sleep-function (lambda (delay) (push delay sleeps))
          :random-state (make-random-state t)))
     "retryable provider failures preserve and complete the same attempt")
    (setf events (nreverse events))
    (test-assert (= attempts 4)
                 "provider recovery uses every permitted transient retry")
    (test-assert (and (= (length sleeps) 3)
                      (every (lambda (delay) (<= 1 delay 60)) sleeps))
                 "provider recovery uses bounded jitter delays")
    (test-assert
     (equal (mapcar #'provider-retry-event-attempt events)
            '(1 1 2 2 3 3))
     "retry events report honest attempt numbers")
    (test-assert
     (every (lambda (event)
              (= (provider-retry-event-maximum-attempts event) 3))
            events)
     "retry events report the configured retry limit"))
  (let* ((*provider-maximum-transient-retries* 2)
         (attempts 0)
         (events nil)
         (sleeps nil)
         (injected-condition
           (make-condition 'provider-retryable-error
                           :message "Injected persistent transient failure."
                           :status nil
                           :request-id nil
                           :response-id nil
                           :response nil))
         (observed-condition nil))
    (handler-case
        (provider--call-with-transient-retries
         (lambda ()
           (incf attempts)
           (error injected-condition))
         (lambda (event)
           (push event events))
         :sleep-function (lambda (delay) (push delay sleeps))
         :random-state (make-random-state t))
      (provider-retryable-error (condition)
        (setf observed-condition condition)))
    (setf events (nreverse events)
          sleeps (nreverse sleeps))
    (test-assert (eq observed-condition injected-condition)
                 "retry exhaustion preserves the final provider condition")
    (test-assert (= attempts 3)
                 "two retries permit exactly three total attempts")
    (test-assert (= (length sleeps) 2)
                 "retry exhaustion does not sleep after the final failure")
    (test-assert
     (equal (mapcar #'provider-retry-event-attempt events) '(1 1 2 2))
     "retry exhaustion emits no event after the final failure")
    (test-assert
     (equal (mapcar #'provider-retry-event-delay events)
            (list (first sleeps) 0 (second sleeps) 0))
     "each permitted retry reports its wait and resumed attempt"))
  (let ((attempts 0)
        (events nil)
        (sleeps nil))
    (test-assert
     (handler-case
         (progn
           (provider--call-with-transient-retries
            (lambda ()
              (incf attempts)
              (error 'provider-incomplete-response
                     :message "Injected incomplete response."
                     :reason "max_output_tokens"
                     :status nil
                     :code "response_incomplete"
                     :request-id nil
                     :response-id nil
                     :response nil))
            (lambda (event)
              (push event events))
            :sleep-function (lambda (delay) (push delay sleeps)))
           nil)
       (provider-incomplete-response ()
         t))
     "incomplete provider responses fail without entering the retry ladder")
    (test-assert (= attempts 1)
                 "incomplete provider responses make exactly one attempt")
    (test-assert (and (null events) (null sleeps))
                 "incomplete provider responses emit no retry activity"))
  nil)

(-> provider-tests--connected-stream-pair () (values stream t t t))
(defun provider-tests--connected-stream-pair ()
  "Return a loopback character stream plus the sockets holding it open."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type ':stream
                                 :protocol ':tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener
                                (sb-bsd-sockets:make-inet-address "127.0.0.1")
                                0)
    (sb-bsd-sockets:socket-listen listener 1)
    (multiple-value-bind (address port) (sb-bsd-sockets:socket-name listener)
      (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                   :type ':stream
                                   :protocol ':tcp)))
        (sb-bsd-sockets:socket-connect client address port)
        (let ((accepted (sb-bsd-sockets:socket-accept listener)))
          (values (sb-bsd-sockets:socket-make-stream client
                                                     :input t
                                                     :output t
                                                     :element-type 'character)
                  client
                  accepted
                  listener))))))

(-> test-provider-stream-inactivity-deadline () null)
(defun test-provider-stream-inactivity-deadline ()
  "Test a silent provider stream reconnects instead of blocking forever."
  (multiple-value-bind (stream client accepted listener)
      (provider-tests--connected-stream-pair)
    (unwind-protect
         (let ((*provider-stream-inactivity-seconds* 1)
               (started (get-universal-time)))
           (test-assert
            (handler-case
                (progn (sse-read-line stream) nil)
              (response-stream-error (condition)
                (search "delivered nothing"
                        (autolith-error-message condition))))
            "a stalled provider stream signals a retryable transport failure")
           (test-assert (< (- (get-universal-time) started) 30)
                        "the stalled stream gives up on its own deadline")
           (let ((accepted-stream
                   (sb-bsd-sockets:socket-make-stream accepted
                                                      :input t
                                                      :output t
                                                      :element-type 'character)))
             (write-line "data: delivered" accepted-stream)
             (finish-output accepted-stream)
             (test-assert
              (string= (sse-read-line stream) "data: delivered")
              "a delivered line renews the stall deadline instead of failing")))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close accepted))
      (ignore-errors (sb-bsd-sockets:socket-close client))
      (ignore-errors (sb-bsd-sockets:socket-close listener))))
  nil)

(-> provider-tests--dexador-call-p (t) boolean)
(defun provider-tests--dexador-call-p (form)
  "Return true when FORM is a direct Dexador GET or POST call."
  (and (consp form)
       (symbolp (first form))
       (let ((package (symbol-package (first form))))
         (and package
              (string= (package-name package) "DEXADOR")
              (member (symbol-name (first form)) '("GET" "POST")
                      :test #'string=)))
       t))

(-> provider-tests--response-deadline-form-p (t) boolean)
(defun provider-tests--response-deadline-form-p (form)
  "Return true when FORM establishes the hard provider response deadline."
  (and (consp form)
       (eq (first form) 'provider-call-with-response-deadline)))

(-> provider-tests--audit-dexador-deadlines () (values integer list))
(defun provider-tests--audit-dexador-deadlines ()
  "Inspect every provider source form and report Dexador calls outside a deadline."
  (let ((count 0)
        (violations nil)
        (root (merge-pathnames "src/provider/"
                               (asdf:system-source-directory :autolith))))
    (labels ((walk (form deadline-p pathname)
               "Walk FORM under DEADLINE-P, recording violations from PATHNAME."
               (when (consp form)
                 (let ((nested-deadline-p
                         (or deadline-p
                             (provider-tests--response-deadline-form-p form))))
                   (when (provider-tests--dexador-call-p form)
                     (incf count)
                     (unless nested-deadline-p
                       (push (list (enough-namestring pathname root)
                                   (first form))
                             violations)))
                   (walk (first form) nested-deadline-p pathname)
                   (walk (rest form) nested-deadline-p pathname)))))
      (dolist (pathname (directory (merge-pathnames "**/*.lisp" root)))
        (with-open-file (stream pathname :direction ':input)
          (let ((*read-eval* nil)
                (*package* (find-package '#:autolith)))
            (loop for form = (read stream nil stream)
                  until (eq form stream)
                  do (walk form nil pathname))))))
    (values count (nreverse violations))))

(-> test-provider-response-deadlines () null)
(defun test-provider-response-deadlines ()
  "Test response deadlines, diagnostic body draining, and transport routing."
  (multiple-value-bind (call-count violations)
      (provider-tests--audit-dexador-deadlines)
    (test-assert (plusp call-count)
                 "the provider transport audit finds Dexador call sites")
    (test-assert (null violations)
                 (format nil "every provider Dexador call has a hard response deadline: ~S"
                         violations)))
  (multiple-value-bind (stream client accepted listener)
      (provider-tests--connected-stream-pair)
    (unwind-protect
         (let ((*provider-error-body-deadline-seconds* 1)
               (started (get-universal-time)))
           (test-assert (null (provider--drain-error-body stream))
                        "a silent optional error body produces no diagnostic text")
           (test-assert (< (- (get-universal-time) started) 30)
                        "optional error-body draining stops at its short deadline")
           (test-assert (not (open-stream-p stream))
                        "optional error-body draining closes the response stream"))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close accepted))
      (ignore-errors (sb-bsd-sockets:socket-close client))
      (ignore-errors (sb-bsd-sockets:socket-close listener))))
  (multiple-value-bind (stream client accepted listener)
      (provider-tests--connected-stream-pair)
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (provider (provider-create configuration))
           (*provider-error-body-deadline-seconds* 1)
           (condition
             (make-condition 'http-request-failed
                             :body stream
                             :status 500
                             :headers nil
                             :uri nil
                             :method ':post)))
      (unwind-protect
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-retryable-error (failure)
                (= (provider-error-status failure) 500)))
            "a body-drain deadline preserves the originating HTTP status failure")
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close accepted))
        (ignore-errors (sb-bsd-sockets:socket-close client))
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
  (let ((stream
          (make-instance 'test-failing-close-stream
                         :source "{\"error\":\"synthetic\"}")))
    (test-call-with-function-replacements
     (list
      (list
       'dexador:post
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (error 'http-request-failed
                :body stream
                :status 500
                :headers nil
                :uri nil
                :method ':post))))
     (lambda ()
       (multiple-value-bind (body status headers)
           (nous-authentication--request
            :method ':post
            :url "https://provider.invalid/oauth"
            :headers nil
            :content "")
         (declare (ignore headers))
         (test-assert
          (and (= status 500)
               (search "synthetic" body)
               (test-failing-close-stream-close-abort-p stream))
          "Nous OAuth drains and abort-closes a streamed error body")))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (manager
           (api-key-credential-manager-create
            :provider-name "deadline-model-errors"
            :pathname (configuration-api-keys-path configuration)))
         (stream
           (make-instance 'test-failing-close-stream
                          :source "{\"error\":\"synthetic\"}")))
    (unwind-protect
         (progn
           (api-key-credential-manager-save-key manager "deadline-key")
           (test-call-with-function-replacements
            (list
             (list
              'dexador:get
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error 'http-request-failed
                       :body stream
                       :status 500
                       :headers nil
                       :uri nil
                       :method ':get))))
            (lambda ()
              (test-assert
               (handler-case
                   (progn
                     (openai-compatible--fetch-models
                      configuration
                      :provider-name "deadline-model-errors"
                      :endpoint "https://provider.invalid/models"
                      :headers nil
                      :credential-manager manager)
                     nil)
                 (configuration-error ()
                   t))
               "model discovery preserves typed HTTP failure mapping")))
           (test-assert (test-failing-close-stream-close-abort-p stream)
                        "model discovery drains and abort-closes a streamed error body"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "deadline-access"
                          :refresh-token "deadline-refresh"
                          :account-id "deadline-account"
                          :expires-at nil))
         (grok-manager
           (make-instance
            'grok-credential-manager
            :primary-source
            (make-instance 'autolith-credential-source
                           :pathname (configuration-grok-auth-path configuration)))))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'provider-call-with-response-deadline
            (lambda (seconds function)
              (declare (ignore seconds function))
              (error 'sb-sys:deadline-timeout))))
          (lambda ()
            (flet ((signals-type-p (thunk type)
                     (handler-case
                         (progn (funcall thunk) nil)
                       (condition (condition)
                         (typep condition type)))))
              (dolist (thunk
                       (list
                        (lambda () (anthropic-validate-api-key "key"))
                        (lambda () (fireworks-validate-api-key "key"))
                        (lambda () (openrouter-validate-api-key "key"))
                        (lambda () (mistral-validate-api-key "key"))
                        (lambda ()
                          (chatgpt-oauth--request
                           :url "https://provider.invalid/oauth" :content ""))
                        (lambda ()
                          (gemini-oauth--request
                           :url "https://provider.invalid/oauth" :content ""))
                        (lambda ()
                          (nous-authentication--request
                           :method ':post
                           :url "https://provider.invalid/oauth"
                           :headers nil
                           :content ""))))
                (test-assert (signals-type-p thunk 'authentication-error)
                             "authentication response deadlines map to typed failures"))
              (test-assert
               (signals-type-p
                (lambda ()
                  (credential-manager-refresh-exchange
                   grok-manager credentials "deadline-refresh"))
                'token-refresh-failed)
               "Grok refresh deadlines map to a typed token failure"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "response-deadlines"))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "deadline-access"
                          :refresh-token "deadline-refresh"
                          :account-id "deadline-account"
                          :expires-at (+ (get-universal-time) 3600)))
         (codex (provider-create configuration))
         (openai
           (openai-compatible-provider-create
            configuration
            :name "deadline-openai"
            :family ':openai
            :headers nil
            :reasoning-parameter nil
            :stream-usage-p t))
         (anthropic (anthropic-provider-create configuration))
         (gemini
           (gemini-code-assist-provider-create
            configuration
            :credential-manager (credential-manager-create configuration)))
         (model-manager
           (api-key-credential-manager-create
            :provider-name "deadline-models"
            :pathname (configuration-api-keys-path configuration)))
         (deadlines nil))
    (unwind-protect
         (progn
           (api-key-credential-manager-save-key model-manager "deadline-key")
           (setf (gemini-code-assist-provider-project gemini) "deadline-project"
                 (gemini-code-assist-provider-setup-complete-p gemini) t)
           (test-call-with-function-replacements
            (list
             (list
              'provider-call-with-response-deadline
              (lambda (seconds function)
                (push seconds deadlines)
                (funcall function)))
             (list
              'dexador:post
              (lambda (url &rest arguments)
                (declare (ignore url))
                (if (getf arguments :want-stream)
                    (values (make-string-input-stream "") 200 nil)
                    (values "{}" 200 nil))))
             (list
              'dexador:get
              (lambda (url &rest arguments)
                (declare (ignore url arguments))
                (values "{\"data\":[]}" 200 nil))))
            (lambda ()
              (provider-open-response-stream
               codex (json-object) :credentials credentials :conversation conversation)
              (provider-open-native-compaction
               codex (json-object) :credentials credentials :conversation conversation)
              (openai-compatible--fetch-models
               configuration
               :provider-name "deadline-models"
               :endpoint "https://provider.invalid/models"
               :headers nil
               :credential-manager model-manager)
              (provider-open-response-stream
               openai (json-object) :credentials credentials :conversation conversation)
              (provider-open-response-stream
               anthropic (json-object) :credentials credentials :conversation conversation)
              (gemini-code-assist--post-once
               gemini credentials "loadCodeAssist" (json-object))
              (gemini-code-assist--get-once gemini credentials "operations/deadline")
              (provider-open-response-stream
               gemini (json-object) :credentials credentials :conversation conversation)))
           (test-assert
            (equal (nreverse deadlines) '(300 300 30 300 300 300 300 300))
            "every provider response and stream-open path uses its response deadline"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(defun test-provider-stream-retries ()
  "Test bounded stream reconnection, observer events, and final failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-stream-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "stream-retry-success"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (let ((*bounded-retry-sleep-function*
                 (lambda (seconds)
                   (declare (ignore seconds)))))
           (dolist (failure
                    '(:stream-error :server-error :overloaded :slow-down))
             (let ((events nil)
                   (provider
                     (test-codex-provider-create
                      configuration
                      (list failure result))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback (lambda (event) (push event events)))
                    result)
                "transient stream and server failures retry the provider turn")
               (let* ((retry-events
                        (reverse
                         (remove-if-not (lambda (event)
                                          (typep event 'provider-retry-event))
                                        events)))
                      (announcement (first retry-events))
                      (in-flight (second retry-events)))
                 (test-assert
                  (and announcement
                       (= (provider-retry-event-attempt announcement) 1)
                       (= (provider-retry-event-maximum-attempts announcement) 6)
                       (<= 1 (provider-retry-event-delay announcement) 60))
                  "provider retries expose their attempt and delay to the observer")
                 (test-assert
                  (and in-flight
                       (= (provider-retry-event-attempt in-flight) 1)
                       (zerop (provider-retry-event-delay in-flight)))
                  "the reconnect attempt itself reports no remaining countdown"))
               (test-assert
                (equal (nreverse
                        (test-codex-provider-refresh-flags provider))
                       '(nil nil))
                "provider retries do not force an authentication refresh")))
           (let ((delays nil)
                 (provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded :overloaded result))))
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (push seconds delays))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback #'identity)
                    result)
                "provider overload retries may recover")
               (test-assert
                (and (= (length delays) 2)
                     (every (lambda (delay) (<= 1 delay 60)) delays))
                "provider overload retries use bounded jittered backoff")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded result))))
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds))
                       (error
                        (make-condition 'application-turn-cancelled)))))
               (test-assert
                (handler-case
                    (progn
                      (provider-stream-turn
                       provider
                       conversation
                       :tool-namespaces #()
                       :event-callback #'identity)
                      nil)
                  (application-turn-cancelled ()
                    t))
                "turn cancellation interrupts provider overload backoff")
               (test-assert
                (= (length
                    (test-codex-provider-refresh-flags provider))
                   1)
                "turn cancellation prevents another provider attempt")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (append (make-list 7 :initial-element :server-error)
                            (list result)))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback #'identity)
                    nil)
                (provider-retryable-error ()
                  t))
              "transient provider recovery stops after six retries")
             (test-assert
              (= (length (test-codex-provider-refresh-flags provider)) 7)
              "retry exhaustion permits exactly seven total attempts")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
