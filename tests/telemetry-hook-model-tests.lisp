(in-package #:autolith)

;;;; -- Wire Model Metadata and Request Roles --

(defclass telemetry-test-stream-provider (responses-api-provider)
  ((data :initarg :data :accessor telemetry-test-stream-data
         :documentation "The synthetic SSE body consumed by this fixture."))
  (:documentation "Read real Responses SSE through the native telemetry boundary."))

(defmethod provider-stream-turn ((provider telemetry-test-stream-provider) conversation
                                 &key tool-namespaces event-callback goal-context compaction-p)
  "Consume PROVIDER's synthetic Responses SSE body through the real stream parser."
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (with-input-from-string (stream (telemetry-test-stream-data provider))
    (provider-consume-stream provider stream event-callback)))

(defclass telemetry-test-terminal-stream ()
  ((configuration :initarg :configuration :reader telemetry-test-terminal-configuration
                  :documentation "Private fixture configuration.")
   (data :initarg :data :reader telemetry-test-terminal-data
         :documentation "Synthetic terminal provider SSE events."))
  (:documentation "Supply real terminal protocol streams without a network request."))

(defmethod provider-configuration ((provider telemetry-test-terminal-stream))
  "Return the private fixture configuration of PROVIDER."
  (telemetry-test-terminal-configuration provider))

(defclass telemetry-test-anthropic-stream (telemetry-test-terminal-stream anthropic-messages-provider) ()
  (:documentation "Consume terminal Anthropic events with the pinned provider parser."))

(defclass telemetry-test-gemini-stream (telemetry-test-terminal-stream gemini-generate-content-provider) ()
  (:documentation "Consume terminal Gemini events with the pinned provider parser."))

(defclass telemetry-test-chat-stream (telemetry-test-terminal-stream chat-completions-provider) ()
  (:documentation "Consume Chat Completions usage with the pinned provider parser."))

(defmethod provider-stream-turn ((provider telemetry-test-terminal-stream) conversation
                                 &key tool-namespaces event-callback goal-context compaction-p)
  "Consume PROVIDER's synthetic terminal SSE events through its pinned parser."
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (with-input-from-string (stream (telemetry-test-terminal-data provider))
    (provider-consume-stream provider stream event-callback)))

(-> test-telemetry-context-window-streams () null)
(defun test-telemetry-context-window-streams ()
  "Keep observed terminal context-window reasons when the real parsers signal failure."
  (test-telemetry--call
   (lambda (configuration)
     (dolist (case '((telemetry-test-anthropic-stream "model_context_window_exceeded"
                      "{\"type\":\"message_start\",\"message\":{\"id\":\"fixture\",\"role\":\"assistant\",\"content\":[],\"usage\":{\"input_tokens\":1,\"output_tokens\":0,\"cache_creation_input_tokens\":37}}}"
                      "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"model_context_window_exceeded\"},\"usage\":{\"output_tokens\":0}}"
                      "{\"type\":\"message_stop\"}")
                     (telemetry-test-gemini-stream "MODEL_CONTEXT_WINDOW_EXCEEDED"
                      "{\"candidates\":[{\"finishReason\":\"MODEL_CONTEXT_WINDOW_EXCEEDED\"}]}")))
       (let* ((provider (make-instance (first case) :configuration configuration
                                      :data                        (format nil "~{data: ~A~%~%~}" (rest (rest case)))))
              (caught   nil)
              (bodies   (test-telemetry-hooks--capture
                         configuration
                         (lambda ()
                           (handler-case
                               (provider-stream-turn provider nil :event-callback #'identity)
                             (cl-llm-provider-api:provider-error (condition) (setf caught condition))))))
              (model    (first (test-telemetry-hooks--kind (first bodies) "model"))))
         (test-assert caught "The provider's terminal failure still reaches its caller.")
         (test-assert (equal (test-telemetry--attribute model "autolith.model.outcome") "error")
                      "A context-window terminal event exports a failed attempt.")
         (test-assert (equal (test-telemetry--attribute model "gen_ai.response.finish_reason")
                             (string-downcase (second case)))
                      "The observed terminal reason survives export.")
         (when (eq (first case) 'telemetry-test-anthropic-stream)
           (test-assert (equal (test-telemetry--attribute model "autolith.usage.cache_write_tokens") "37")
                        "Observed cache writes survive a failed stream without a final result."))
         (test-telemetry--collector-fixture
          (if (eq (first case) 'telemetry-test-anthropic-stream)
              "native-terminal-anthropic.json" "native-terminal-gemini.json")
          (json-object "finish_reason" (string-downcase (second case))
                       "envelope" (json-decode (first bodies))))))))
  nil)

(-> test-telemetry-returned-model-hook () null)
(defun test-telemetry-returned-model-hook ()
  "Read wire metadata, project aliases and public snapshots, and clear absent models."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (let ((*model-catalog-fetch-function*
             (lambda (run)
               (declare (ignore run))
               (test-model-catalog--fixture '("gpt-5.4-2026-03-05")))))
       (telemetry-attach configuration)
       (test-model-catalog--await *telemetry-controller*))
     (setf (config :telemetry-model-aliases configuration)
           '(("private-response-model" . "model-b")))
     (let ((provider (make-instance 'telemetry-test-stream-provider
                                    :configuration configuration
                                    :data          (format nil "data: {\"type\":\"response.completed\",~%data: \"response\":{\"id\":\"private-response-id\",\"model\":\"private-response-model\",\"usage\":{\"input_tokens\":100,\"input_tokens_details\":{\"cached_tokens\":20,\"cache_write_tokens\":30}}}}~%~%"))))
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (provider-stream-turn provider nil :event-callback #'identity)
                         (setf (telemetry-test-stream-data provider)
                               (format nil "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"second\",\"model\":\"gpt-5.4-2026-03-05\",\"usage\":{}}}~%~%"))
                         (provider-stream-turn provider nil :event-callback #'identity)
                         (setf (telemetry-test-stream-data provider)
                               (format nil "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"third\",\"usage\":{}}}~%~%"))
                         (provider-stream-turn provider nil :event-callback #'identity))))
              (first  (first (test-telemetry-hooks--kind (first bodies) "model")))
              (second (first (test-telemetry-hooks--kind (second bodies) "model")))
              (third  (first (test-telemetry-hooks--kind (third bodies) "model"))))
         (test-telemetry-hooks--assert-attributes
          first
          '((aliased-model "gen_ai.response.model" "model-b"
             "Returned model metadata uses the configured public alias.")
            (reported-cache-writes "autolith.usage.cache_write_tokens" "30"
             "OpenAI's reported nested cache-write counter is preserved.")
            (successful-request "autolith.model.outcome" "success"
             "A returned request exports its successful outcome.")
            (completed-response "gen_ai.response.finish_reason" "completed"
             "The Responses terminal status reaches export.")
            (successful-error-omission "error.type" nil
             "Successful requests have no error category.")))
         (test-telemetry-hooks--assert-attributes
          second
          '((public-snapshot "gen_ai.response.model" "gpt-5.4-2026-03-05"
             "A verified public response snapshot exports without an explicit alias.")))
         (test-telemetry-hooks--assert-attributes
          third
          '((absent-model "gen_ai.response.model" nil
             "An absent model is omitted instead of inheriting a previous response model.")
            (absent-cache-writes "autolith.usage.cache_write_tokens" nil
             "Unavailable cache writes stay absent instead of becoming zero.")))
         (dolist (body bodies)
           (test-assert (not (search "private-response" body))
                        "Neither raw response model nor response identity is exported."))))))
  nil)

(-> test-telemetry-cache-write-usage () null)
(defun test-telemetry-cache-write-usage ()
  "Read cache creation through real streaming parsers, preserving reported zero and omission."
  (test-telemetry--call
   (lambda (configuration)
     (let ((fixtures nil))
       (dolist (protocol '(telemetry-test-stream-provider telemetry-test-chat-stream telemetry-test-anthropic-stream))
         (dolist (writes '(30 0 nil))
           (let* ((usage    (json-object "input_tokens" 100 "output_tokens" 2))
                  (events
                    (case protocol
                      (telemetry-test-stream-provider
                       (when writes
                         (setf (gethash "input_tokens_details" usage)
                               (json-object "cache_write_tokens" writes)))
                       (list (json-object "type" "response.completed"
                                          "response" (json-object "id" "fixture" "usage" usage))))
                      (telemetry-test-chat-stream
                       (when writes (setf (gethash "cache_write_tokens" usage) writes))
                       (list (json-object "choices" (vector (json-object "index" 0
                                                                        "delta" (json-object "content" "ok")
                                                                        "finish_reason" "stop"))
                                          "usage" usage)))
                      (telemetry-test-anthropic-stream
                       ;; The pinned parser fills missing creation with zero when reads exist.
                       (setf (gethash "cache_read_input_tokens" usage) 20)
                       (when writes (setf (gethash "cache_creation_input_tokens" usage) writes))
                       (list (json-object "type" "message_start" "message"
                                          (json-object "id" "fixture" "role" "assistant" "content" #() "usage" usage))
                             (json-object "type" "content_block_start" "index" 0 "content_block"
                                          (json-object "type" "text" "text" ""))
                             (json-object "type" "content_block_delta" "index" 0 "delta"
                                          (json-object "type" "text_delta" "text" "ok"))
                             (json-object "type" "content_block_stop" "index" 0)
                             (json-object "type" "message_delta" "delta" (json-object "stop_reason" "end_turn")
                                          "usage" (json-object "output_tokens" 2))
                             (json-object "type" "message_stop")))))
                  (provider (make-instance protocol :configuration configuration
                                           :data                   (concatenate 'string
                                                                                (format nil "~{data: ~A~%~%~}" (mapcar #'json-encode events))
                                                                                (if (eq protocol 'telemetry-test-chat-stream)
                                                                                    (format nil "data: [DONE]~%~%") ""))))
                  (body     (first (test-telemetry-hooks--capture
                                    configuration (lambda () (provider-stream-turn provider nil :event-callback #'identity)))))
                  (span     (first (test-telemetry-hooks--kind body "model"))))
             (test-assert (equal (test-telemetry--attribute span "autolith.usage.cache_write_tokens")
                                 (when writes (write-to-string writes)))
                          "Wire counters survive parsing; absent counters stay absent.")
             (push (json-object "write_tokens" (or writes ':null) "envelope" (json-decode body)) fixtures))))
       (test-telemetry--collector-fixture "native-cache-usage.json" (coerce (nreverse fixtures) 'vector)))))
  nil)

(-> test-telemetry-unlisted-provider-model () null)
(defun test-telemetry-unlisted-provider-model ()
  "Resolve an unlisted custom model and export its real missing-credential failure."
  (test-telemetry--call
   (lambda (configuration)
     (let ((snapshot (provider--registry-snapshot))
           (model    "gpt-e2e-unlisted-9"))
       (unwind-protect
            (progn
              (test-assert (configuration--model-problem model configuration)
                           "An unregistered model fails validation before agent execution.")
              (register-openai-compatible-provider
               :name "telemetry-e2e" :family ':openai :models (list model)
               :endpoint "http://127.0.0.1:1/v1/chat/completions")
              (setf (config :model configuration) model)
              (let* ((provider (provider-create configuration))
                     (caught   nil)
                     (body     (first (test-telemetry-hooks--capture
                                       configuration
                                       (lambda ()
                                         (handler-case
                                             (provider-stream-turn provider (conversation-create configuration)
                                                                   :tool-namespaces #() :event-callback #'identity)
                                           (authentication-error () (setf caught t))))))))
                (test-assert caught "The real custom provider fails without credentials before network I/O.")
                (dolist (kind '("run" "model"))
                  (let ((span (first (test-telemetry-hooks--kind body kind))))
                    (test-assert (equal (test-telemetry--attribute span "gen_ai.request.model") "custom")
                                 "Local custom models export the private-model category.")
                    (test-assert (not (json-true-p (test-telemetry--attribute span "autolith.model.known")))
                                 "Local registration does not imply public catalog membership.")
                    (test-assert (equal (test-telemetry--attribute span "error.type") "authentication")
                                 "The failure category survives run and model export.")))
                (test-telemetry--collector-fixture "native-unlisted-model.json" (json-decode body))))
         (provider--registry-restore snapshot)))))
  nil)

(-> test-telemetry-chatgpt-cache-write-usage () null)
(defun test-telemetry-chatgpt-cache-write-usage ()
  "Preserve unknown cache writes through the real ChatGPT Responses request path."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :model configuration) "gpt-6.1-sol")
     (let* ((provider     (provider-create configuration))
            (conversation (conversation-create configuration))
            (usage        nil))
       (credential-source-save
        (credential-manager-primary-source (provider-credential-manager provider))
        (provider-tests--credentials configuration))
       (test-call-with-function-replacements
        (list (list 'provider-open-response-stream
                    (lambda (active-provider request &key credentials conversation)
                      (declare (ignore active-provider request credentials conversation))
                      (values
                       (make-string-input-stream
                        (test-sse-event-string
                         (json-object "type" "response.completed" "response"
                                      (json-object "id" "fixture" "model" "gpt-6.1-sol"
                                                   "usage" usage))))
                       200 nil))))
        (lambda ()
          (dolist (case '((unreported nil nil)
                          (compatibility-zero nil 0)
                          (compatibility-positive nil 7)
                          (structural-zero 0 nil)
                          (both-zero 0 0)
                          (structural-zero-compatibility-positive 0 7)))
            (destructuring-bind (case-name structural-writes writes) case
              (setf usage (json-object "input_tokens" 100 "output_tokens" 2
                                       "input_tokens_details" (json-object "cached_tokens" 20)))
              (when structural-writes
                (setf (gethash "cache_write_tokens" (json-get usage "input_tokens_details"))
                      structural-writes))
              (when writes
                (setf (gethash "prompt_tokens_details" usage)
                      (json-object "cache_write_tokens" writes)))
              (let* ((body   (first (test-telemetry-hooks--capture
                                     configuration
                                     (lambda ()
                                       (provider-stream-turn provider conversation
                                                             :tool-namespaces #() :event-callback #'identity)))))
                     (models (test-telemetry-hooks--kind body "model"))
                     (model  (first models)))
                (test-assert (= (length models) 1)
                             (format nil "~A: Each real Responses request exports one model row." case-name))
                (test-assert (equal (test-telemetry--attribute model "gen_ai.provider.name") "openai")
                             (format nil "~A: ChatGPT authentication exports the OpenAI provider." case-name))
                (test-assert (equal (test-telemetry--attribute model "autolith.usage.cache_read_tokens") "20")
                             (format nil "~A: An omitted write counter does not lose reported cache reads." case-name))
                (test-assert (equal (test-telemetry--attribute model "autolith.usage.cache_write_tokens")
                                    (when (and writes (plusp writes)) (write-to-string writes)))
                             (format nil "~A: Positive reported writes survive; omitted or unsupported writes stay unknown." case-name))))))))))
  nil)

(-> test-telemetry-compaction-roles () null)
(defun test-telemetry-compaction-roles ()
  "Classify native checkpoint and summary requests at the actual compaction entry point."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration))
            (provider     (make-instance 'native-scripted-provider
                                         :configuration configuration
                                         :native-items  (list (json-object "type" "compaction"
                                                                          "encrypted_content" "fixture"))
                                         :results       (list (agent-test-result
                                                               "summary" (list (agent-test-compaction-message))
                                                               :turn-completion ':end))))
            (agent        (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry (agent-test-registry) :worker nil)))
       (conversation-append-user-message conversation "Fixture summary context")
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (telemetry-call-with-run
                          configuration
                          (lambda () (agent-compact-conversation agent (make-instance 'agent-observer)))))))
              (models (test-telemetry-hooks--kind (first bodies) "model")))
         (test-assert (= (length models) 2) "Native checkpoint and summary each export an attempt.")
         (dolist (model models)
           (test-assert (equal (test-telemetry--attribute model "autolith.request.role") "compaction")
                        "Both compaction requests have the compaction purpose."))
         (test-telemetry--collector-fixture "native-compaction.json" (json-decode (first bodies)))))))
  nil)

(-> test-telemetry-request-roles () null)
(defun test-telemetry-request-roles ()
  "Distinguish real permission, inference, title, and ordinary provider requests."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((provider     (make-instance
                           'rlm-inference-test-provider
                           :results (mapcar (lambda (text) (rlm-inference-test-result "role" text 10))
                                            '("{\"decision\":\"full\",\"reason\":\"fixture\"}"
                                              "inference answer" "Generated title" "agent answer"))))
            (conversation (conversation-create configuration))
            (application  (make-instance 'application :configuration configuration
                                          :conversation conversation :provider provider))
            (bodies
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (telemetry-call-with-run
                  configuration
                  (lambda ()
                    (permissions-model-classify-command "pwd" (config :cache-root configuration)
                                                        :provider provider :configuration configuration)
                    (infer "Fixture inference" :provider provider :configuration configuration)
                    (conversation-append-user-message conversation "Fixture title context")
                    (application--conversation-title-generate application)
                    (agent-run-user-turn
                     (agent-create :configuration configuration :conversation conversation
                                   :provider provider :tool-registry (agent-test-registry) :worker nil)
                     "Fixture agent request")))))))
       (test-assert
        (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.request.role"))
                       (test-telemetry-hooks--kind (first bodies) "model"))
               '("permission_classifier" "inference" "title_generation" "agent"))
        "Public roles follow entry points and dynamic bindings restore after each request.")
       (test-telemetry--collector-fixture "native-roles.json" (json-decode (first bodies))))))
  nil)
