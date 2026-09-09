(in-package #:autolith)

;;;; -- Grok Provider Tests --

(-> grok-provider-test--configuration () configuration)
(defun grok-provider-test--configuration ()
  "Return an isolated configuration selecting the Grok model."
  (configuration-with-model (test-configuration) "grok-4.5"))


(-> grok-provider-test--item-normalization () null)
(defun grok-provider-test--item-normalization ()
  "Test representative Grok output items retain their replay semantics."
  (let ((provider (grok-provider-create (grok-provider-test--configuration))))
    (let ((call
            (json-object "type" "function_call"
                         "id" "transient-call"
                         "call_id" "call-1"
                         "name" "resource.read"
                         "status" "completed"
                         "arguments" "{}")))
      (provider-normalize-output-item provider call)
      (test-assert
       (and (null (gethash "id" call))
            (null (gethash "status" call))
            (string= (json-get call "namespace") "resource")
            (string= (json-get call "name") "read"))
       "ordinary Grok function calls normalize to persisted tool identity"))
    (let ((reasoning
            (json-object "type" "reasoning"
                         "id" "reasoning-1"
                         "status" "completed"
                         "encrypted_content" "opaque")))
      (provider-normalize-output-item provider reasoning)
      (test-assert
       (and (string= (json-get reasoning "id") "reasoning-1")
            (null (gethash "status" reasoning))
            (string= (json-get reasoning "encrypted_content") "opaque"))
       "Grok reasoning retains replayable identifiers without output status"))
    (let ((search
            (json-object "type" "web_search_call"
                         "id" "search-1"
                         "status" "completed")))
      (provider-normalize-output-item provider search)
      (test-assert
       (and (string= (json-get search "id") "search-1")
            (string= (json-get search "status") "completed"))
       "Grok backend search output remains replayable")))
  nil)

(-> grok-provider-test--request-shape () null)
(defun grok-provider-test--request-shape ()
  "Test Grok-specific Responses request controls."
  (let* ((base-configuration (grok-provider-test--configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "grok-shape"))
                (provider (grok-provider-create configuration)))
           (conversation-append-user-message conversation "hello")
           (let* ((request
                    (provider-request-object provider conversation (json-array)))
                  (tools (json-get request "tools")))
            (test-assert
             (string= (json-get request "model") "grok-4.5")
             "Grok requests select the configured model")
            (test-assert
             (string= (json-get (json-get request "reasoning") "effort") "high")
             "Grok requests select the configured reasoning effort")
            (test-assert
             (eq (json-get request "parallel_tool_calls") false)
             "Grok requests disable parallel tool calls")
            (test-assert
             (find "web_search" tools
                   :key (lambda (tool) (json-get tool "type"))
                   :test #'string=)
             "enabled Grok search includes backend web search")
            (test-assert
             (find "x_search" tools
                   :key (lambda (tool) (json-get tool "type"))
                   :test #'string=)
             "enabled Grok search includes backend X search")
            (test-assert
             (equalp (json-get request "include")
                     (json-array "reasoning.encrypted_content"
                                 "no_inline_citations"))
             "enabled Grok search requests citation metadata")
            (let* ((disabled
                     (configuration--clone configuration
                                           :web-search-mode "disabled"))
                   (request
                     (provider-request-object
                      (grok-provider-create disabled) conversation (json-array))))
              (test-assert
               (zerop (length (json-get request "tools")))
               "disabled Grok search omits backend tools")
              (test-assert
               (equalp (json-get request "include")
                       (json-array "reasoning.encrypted_content"))
              "disabled Grok search omits citation metadata"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> grok-provider-test--transport-headers () null)
(defun grok-provider-test--transport-headers ()
  "Test the authenticated Grok transport headers without network access."
  (let* ((configuration (grok-provider-test--configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "grok-wire"))
                (provider (grok-provider-create configuration))
                (credentials
                  (make-instance 'oauth-credentials
                                 :access-token "grok-access-token"
                                 :refresh-token nil
                                 :id-token nil
                                 :account-id "grok-user-1"
                                 :expires-at nil
                                 :source-path
                                 (configuration-grok-auth-path configuration)))
                (captured-url nil)
                (captured-headers nil))
           (test-call-with-function-replacements
            (list
             (list
              'dexador:post
              (lambda (url &key headers content &allow-other-keys)
                (declare (ignore content))
                (setf captured-url url
                      captured-headers headers)
                (values (make-string-input-stream "") 200 nil))))
            (lambda ()
              (provider-open-response-stream
               provider
               (json-object "model" "grok-4.5")
               :credentials credentials
               :conversation conversation)))
           (flet ((header (name)
                    (rest (assoc name captured-headers :test #'string-equal))))
             (test-assert (string= captured-url
                                   (configuration-provider-endpoint
                                    configuration))
                          "the Grok transport posts to the configured proxy")
             (test-assert (string= (header "Authorization")
                                   "Bearer grok-access-token")
                          "the Grok transport sends the bearer access token")
             (test-assert (string= (header "X-XAI-Token-Auth") "xai-grok-cli")
                          "the Grok transport marks user token authentication")
             (test-assert (string= (header "x-authenticateresponse")
                                   "authenticate-response")
                          "the Grok transport requests authenticated responses")
             (test-assert (string= (header "x-grok-model-override") "grok-4.5")
                          "the Grok transport pins the requested model")
             (test-assert (string= (header "x-grok-client-version")
                                   *grok-client-protocol-version*)
                          "the Grok transport passes the proxy version gate")
             (test-assert (string= (header "x-grok-client-mode") "interactive")
                          "the Grok transport reports interactive client mode")
             (test-assert (string= (header "x-grok-conv-id") "grok-wire")
                          "the Grok transport carries the conversation identity")
             (test-assert (string= (header "x-grok-doom-loop-check")
                                   (format nil "~D"
                                           *grok-doom-loop-window-tokens*))
                          "the Grok transport opts into server loop detection")
             (test-assert (string= (header "Accept") "text/event-stream")
                          "the Grok transport requests an event stream")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> grok-provider-test--selection () null)
(defun grok-provider-test--selection ()
  "Test Grok provider, endpoint, and context selection."
  (let* ((configuration (grok-provider-test--configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration)))
    (unwind-protect
         (progn
           (test-assert (typep provider 'grok-subscription-provider)
                        "Grok models select the Grok subscription provider")
           (test-assert
            (string= (configuration-provider-endpoint configuration)
                     *grok-responses-endpoint*)
            "Grok models select the Grok proxy endpoint")
           (test-assert (= (configuration-context-window configuration) 500000)
                        "Grok models select the Grok context window"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> grok-provider-test--doom-loop-stream (list) string)
(defun grok-provider-test--doom-loop-stream (triggers)
  "Return one complete Grok SSE fixture reporting TRIGGERS mid-stream."
  (concatenate
   'string
   (test-sse-event-string
    (json-object "type" "response.created"
                 "response" (json-object "id" "grok-doom-1")))
   (test-sse-event-string
    (json-object "type" "response.doom_loop_check"
                 "sequence_number" 4176
                 "doom_loop_check"
                 (json-object "triggers" (apply #'json-array triggers))))
   (test-sse-event-string
    (json-object "type" "response.completed"
                 "response" (json-object "id" "grok-doom-1"
                                         "usage" (json-object))))))

(-> grok-provider-test--consume-doom-stream (list) t)
(defun grok-provider-test--consume-doom-stream (triggers)
  "Consume one Grok stream reporting TRIGGERS and return the outcome."
  (provider-consume-stream
   (grok-provider-create (grok-provider-test--configuration))
   (make-instance 'test-character-input-stream
                  :source (grok-provider-test--doom-loop-stream triggers))
   nil
   (lambda (event)
     (declare (ignore event)))))

(-> grok-provider-test--doom-loop-recovery () null)
(defun grok-provider-test--doom-loop-recovery ()
  "Test server-reported loop detection, confidence, budget, and resampling."
  (dolist (case '(("tail_repetition:2@thinking" t)
                  ("tail_repetition:65@thinking" nil)
                  ("low_logprob@thinking" nil)))
    (destructuring-bind (trigger expected) case
      (test-assert (eq (grok--doom-loop-confident-trigger-p trigger) expected)
                   (format nil "doom-loop confidence classifies ~S" trigger))))
  (let ((*grok-doom-loop-resamples-remaining* 2))
    (test-assert
     (handler-case
         (progn
           (grok-provider-test--consume-doom-stream
            (list "tail_repetition:4@response" "tail_repetition:8@thinking"))
           nil)
       (provider-resample-requested (condition)
         (and (equal (provider-resample-requested-triggers condition)
                     (list "tail_repetition:8@thinking"))
              (= (provider-resample-requested-attempt condition) 1)
              (= (provider-resample-requested-maximum-attempts condition) 2)
              (= *grok-doom-loop-resamples-remaining* 1))))
     "a confident loop report abandons the stream while budget remains"))
  (let ((*grok-doom-loop-resamples-remaining* 0))
    (test-assert
     (typep (grok-provider-test--consume-doom-stream
             (list "tail_repetition:2@thinking"))
            'provider-result)
     "a spent resample budget accepts the looping response as-is"))
  (let ((*grok-doom-loop-resamples-remaining* 2))
    (test-assert
     (typep (grok-provider-test--consume-doom-stream
             (list "tail_repetition:4@response" "low_logprob@thinking"))
            'provider-result)
     "warn-only triggers never abandon the stream"))
  (test-assert
   (typep (grok-provider-test--consume-doom-stream
           (list "tail_repetition:2@thinking"))
          'provider-result)
   "loop reports outside an armed turn are ignored")
  (let* ((configuration (grok-provider-test--configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "grok-doom-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "grok-doom-recovered"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (let ((events nil)
               (delays nil)
               (provider
                 (test-codex-provider-create
                  configuration
                  (list :resample :resample result))))
           (let ((*bounded-retry-sleep-function*
                   (lambda (seconds)
                     (push seconds delays))))
             (test-assert
              (eq (provider-stream-turn
                   provider
                   conversation
                   :tool-namespaces #()
                   :event-callback (lambda (event) (push event events)))
                  result)
              "resample requests retry the turn until a clean response"))
           (test-assert (null delays)
                        "resample retries never wait before the fresh sample")
           (let ((retry-events
                   (remove-if-not (lambda (event)
                                    (typep event 'provider-retry-event))
                                  events)))
             (test-assert
              (and (= (length retry-events) 2)
                   (every (lambda (event)
                            (zerop (provider-retry-event-delay event)))
                          retry-events))
              "resample retries surface immediate retry events")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (test-assert
   (grok--empty-response-p
    (make-instance 'provider-result
                   :response-id "empty"
                   :output-items (list (json-object "type" "reasoning"))
                   :tool-calls nil
                   :usage nil
                   :turn-state nil))
   "a reasoning-only completion counts as an empty response")
  (test-assert
   (not (grok--empty-response-p
         (make-instance 'provider-result
                        :response-id "visible"
                        :output-items (list (json-object "type" "message"))
                        :tool-calls nil
                        :usage nil
                        :turn-state nil)))
   "an assistant message keeps a response from counting as empty")
  nil)

(-> test-grok-provider () null)
(defun test-grok-provider ()
  "Test the Grok subscription provider without network access."
  (grok-provider-test--selection)
  (grok-provider-test--item-normalization)
  (grok-provider-test--doom-loop-recovery)
  (grok-provider-test--request-shape)
  (grok-provider-test--transport-headers)
  nil)
