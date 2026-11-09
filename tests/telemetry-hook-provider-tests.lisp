(in-package #:autolith)

;;;; -- Provider Attempts and Recovery --

(-> test-telemetry-provider-retry-hooks () null)
(defun test-telemetry-provider-retry-hooks ()
  "Exercise inherited subscription streaming, authentication refresh and transport retry."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation                   (conversation-create configuration :identifier "telemetry-retry"))
            (result                         (agent-test-result "retry-result" nil))
            (provider                       (test-codex-provider-create
                                             configuration (list :unauthorized :server-error :unauthorized result)))
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (provider-stream-turn provider conversation
                                    :tool-namespaces #() :event-callback #'identity)))
         (test-assert (eq (first results) result) "Recovery returns the original provider result.")
         (test-assert (equal (reverse (test-codex-provider-refresh-flags provider))
                             '(nil t nil t))
                      "Transport recovery starts a fresh bounded authentication cycle.")
         (test-assert (= (length bodies) 1) "Standalone recovery has one logical root.")
         (test-telemetry-hooks--assert-attempts (first bodies) 4)
         (let ((models (test-telemetry-hooks--kind (first bodies) "model")))
           (dolist (span (butlast models))
             (test-assert (null (test-telemetry--attribute span "gen_ai.usage.input_tokens"))
                          "Failed attempts omit unknown usage, rather than recording zero."))
           (test-assert (equal (test-telemetry--attribute (first (last models))
                                                        "gen_ai.usage.input_tokens") "1")
                        "The successful attempt retains normalized usage."))))))
  nil)

(-> test-telemetry-grok-empty-response-hooks () null)
(defun test-telemetry-grok-empty-response-hooks ()
  "Record empty and reasoning-only Grok retries as failed attempts before a valid response."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :model configuration) "grok-4.7")
     (let* ((provider                       (grok-provider-create configuration))
            (conversation                   (conversation-create configuration))
            (outputs                        (list nil
                                                  (list (json-object "type" "reasoning" "id" "fixture-reasoning"))
                                                  (list (agent-test-message "Visible answer."))))
            (requests                       0)
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
       (credential-source-save
        (credential-manager-primary-source (provider-credential-manager provider))
        (provider-tests--credentials configuration))
       (test-call-with-function-replacements
        (list (list 'provider-open-response-stream
                    (lambda (active-provider request &key credentials conversation)
                      (declare (ignore active-provider request credentials conversation))
                      (incf requests)
                      (values
                       (make-string-input-stream
                        (concatenate
                         'string
                         (format nil "~{~A~}"
                                 (mapcar (lambda (item)
                                           (test-sse-event-string
                                            (json-object "type" "response.output_item.done" "item" item)))
                                         (pop outputs)))
                         (test-sse-event-string
                          (json-object "type" "response.completed"
                                       "response" (json-object "id" "fixture-response" "model" "grok-4.7"
                                                               "usage" (json-object
                                                                        "input_tokens" (* requests 10)
                                                                        "output_tokens" requests
                                                                        "input_tokens_details"
                                                                        (json-object "cached_tokens" requests
                                                                                     "cache_write_tokens" (* requests 2))))))))
                       200 nil))))
        (lambda ()
          (multiple-value-bind (bodies results)
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)))
            (test-assert (equal (provider-result-assistant-text (first results)) "Visible answer.")
                         "Grok still retries unusable results and returns the visible response.")
            (test-assert (= requests 3) "Exactly three real transports run.")
            (test-assert (null outputs) "Both unusable responses are consumed before success.")
            (test-telemetry-hooks--assert-attempts (first bodies) 3)
            (let ((models (test-telemetry-hooks--kind (first bodies) "model")))
              (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.model.outcome")) models)
                                  '("error" "error" "success"))
                           "Empty and reasoning-only completions export failed attempts.")
              (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "error.type")) models)
                                  '("provider" "provider" nil))
                           "Grok validation failures retain their provider error category.")
              (dolist (case '(("gen_ai.usage.input_tokens" ("10" "20" "30"))
                              ("gen_ai.usage.output_tokens" ("1" "2" "3"))
                              ("autolith.usage.cache_read_tokens" ("1" "2" "3"))
                              ("autolith.usage.cache_write_tokens" ("2" "4" "6"))))
                (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span (first case))) models)
                                    (second case))
                             "Rejected results retain their own reported counters without inheriting another attempt."))
              (dolist (model models)
                (test-assert (equal (test-telemetry--attribute model "gen_ai.response.finish_reason") "completed")
                             "Observed wire completion remains distinct from accepted attempt outcome.")))))))))
  nil)

(-> test-telemetry-provider-transport-hooks () null)
(defun test-telemetry-provider-transport-hooks ()
  "Classify normalized connection failures and SSE inactivity as transport attempts."
  (test-telemetry--call
   (lambda (configuration)
     (dolist (mode '(:connection :inactivity))
       (let* ((conversation                   (conversation-create configuration))
              (source                         (provider-tests--completed-sse-source "recovered"))
              (provider                       (provider-tests--transport-provider
                                               configuration
                                               (list (if (eq mode ':connection)
                                                         ':syscall
                                                         (make-string-input-stream source))
                                                     (make-string-input-stream source))))
              (failures                       nil)
              (expired                        nil)
              (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
         (credential-source-save
          (credential-manager-primary-source (provider-credential-manager provider))
          (provider-tests--credentials configuration))
         (test-call-with-function-replacements
          (when (eq mode ':inactivity)
            (list (list 'provider-call-with-response-deadline
                        (lambda (seconds function)
                          (test-assert (= seconds *sse-inactivity-seconds*) "The SSE reader owns this inactivity deadline.")
                          (unless expired
                            (setf expired t)
                            (error 'sb-sys:deadline-timeout))
                          (funcall function)))))
          (lambda ()
            (multiple-value-bind (bodies results)
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (provider-stream-turn
                    provider conversation :tool-namespaces #()
                    :event-callback                        (lambda (event)
                                                             (when (typep event 'provider-attempt-failed-event)
                                                               (push (provider-attempt-failed-event-condition event) failures))))))
              (test-assert (equal (provider-result-response-id (first results)) "recovered")
                           "The real subscription retry loop recovers from transport failure.")
              (test-assert (and (= (length failures) 1)
                                (typep (first failures) (if (eq mode ':connection)
                                                           'provider-transport-error 'response-stream-error)))
                           "Connection and inactivity adapters produce their expected typed conditions.")
              (test-telemetry-hooks--assert-attempts (first bodies) 2)
              (let ((models (test-telemetry-hooks--kind (first bodies) "model")))
                (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.model.outcome")) models)
                                    '("error" "success"))
                             "Transport failure and recovered success remain separate attempts.")
                (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "error.type")) models)
                                    '("transport" nil))
                             "The transport hierarchy takes precedence over generic provider errors.")
                (dolist (attribute '("gen_ai.usage.input_tokens" "gen_ai.usage.output_tokens"
                                     "autolith.usage.cache_read_tokens" "autolith.usage.cache_write_tokens"))
                  (test-assert (null (test-telemetry--attribute (first models) attribute))
                               "Unavailable transport usage remains absent."))))))))))
  nil)

(-> test-telemetry-provider-failure-hooks () null)
(defun test-telemetry-provider-failure-hooks ()
  "Failed refresh and cancellation during backoff preserve their conditions and counts."
  (test-telemetry--call
   (lambda (configuration)
     (let ((conversation (conversation-create configuration :identifier "telemetry-failures")))
       (dolist (case '((refresh-failed nil authentication-error 2 "error" "authentication")
                       (backoff-cancelled t application-turn-cancelled 1 "cancelled" "cancelled")))
         (destructuring-bind (case-name cancel-p condition-type attempt-count outcome category) case
           (let* ((provider (test-codex-provider-create
                             configuration
                             (if cancel-p (list :overloaded (agent-test-result "unused" nil))
                                 '(:unauthorized :unauthorized))))
                  (caught   nil)
                  (*bounded-retry-sleep-function*
                    (lambda (seconds)
                      (declare (ignore seconds))
                      (when cancel-p (error 'application-turn-cancelled)))))
             (let ((bodies
                     (test-telemetry-hooks--capture
                      configuration
                      (lambda ()
                        (handler-case
                            (provider-stream-turn provider conversation
                                                  :tool-namespaces #() :event-callback #'identity)
                          (authentication-error (condition) (setf caught condition))
                          (application-turn-cancelled (condition) (setf caught condition)))))))
               (test-assert (typep caught condition-type)
                            (format nil "~A: The original terminal condition survives telemetry cleanup." case-name))
               (test-telemetry-hooks--assert-attempts (first bodies) attempt-count)
               (dolist (span (test-telemetry-hooks--kind (first bodies) "model"))
                 (test-assert (equal (test-telemetry--attribute span "autolith.model.outcome") "error")
                              (format nil "~A: Failed provider attempts export an error outcome." case-name))
                 (test-assert (member (test-telemetry--attribute span "error.type")
                                      '("authentication" "provider") :test #'equal)
                              (format nil "~A: Structured failures export a bounded error category." case-name)))
               (test-assert (equal (test-telemetry--attribute
                                    (first (test-telemetry-hooks--kind (first bodies) "run"))
                                    "autolith.run.outcome")
                                  outcome)
                            (format nil "~A: Standalone terminal roots distinguish cancellation and failure." case-name))
               (test-assert
                (equal (test-telemetry--attribute
                        (first (test-telemetry-hooks--kind (first bodies) "run")) "error.type")
                       category)
                (format nil "~A: Run failures preserve the terminal condition category." case-name))
               (unless cancel-p
                 (test-telemetry--collector-fixture "native-run-authentication.json" (json-decode (first bodies)))))))))))
  nil)

(defclass telemetry-test-values-provider (scripted-provider) ()
  (:documentation "Custom provider bypassing the per-attempt protocol."))

(defmethod provider-configuration ((provider telemetry-test-values-provider))
  "Return the scripted configuration of PROVIDER."
  (scripted-provider-configuration provider))

(defmethod provider-family ((provider telemetry-test-values-provider))
  "Fail, simulating broken optional provider metadata."
  (declare (ignore provider))
  (error "Injected optional provider metadata failure."))

(defmethod provider-stream-turn
    ((provider telemetry-test-values-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return the next scripted result with an extra secondary value, bypassing attempts."
  (declare (ignore conversation tool-namespaces event-callback goal-context compaction-p))
  (values (pop (scripted-provider-results provider)) :secondary 17))

(defclass telemetry-test-mission-provider (telemetry-test-values-provider) ()
  (:documentation "Custom streaming and native-compaction provider for mission accounting."))

(defmethod provider-native-compact-conversation
    ((provider telemetry-test-mission-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Return a compacted marker, fixed usage and an extra secondary value."
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values :compacted (json-object "input_tokens" 2 "output_tokens" 3) :secondary))

(-> test-telemetry-hooks--mission-accounting () null)
(defun test-telemetry-hooks--mission-accounting ()
  "Compose enabled telemetry with mission admission and both usage protocols."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((result       (mission-test--result "done"))
            (provider     (make-instance 'telemetry-test-mission-provider
                                         :configuration configuration :results (list result result)))
            (application  (mission-test--application configuration))
            (conversation (application-conversation application)))
       (setf (application-provider application) provider)
       (application-mission-start application (mission-test--specification :turns 2))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (telemetry-call-with-run
               configuration
               (lambda ()
                 (test-assert
                  (equal (multiple-value-list
                          (provider-stream-turn provider conversation
                                                :tool-namespaces #() :event-callback #'identity))
                         (list result :secondary 17))
                  "Mission streaming retains all provider values with telemetry enabled.")
                 (multiple-value-bind (item usage extra)
                     (provider-native-compact-conversation
                      provider conversation :tool-namespaces #() :event-callback #'identity)
                   (test-assert (and (eq item :compacted) (eq extra :secondary)
                                     (= 2 (json-get usage "input_tokens")))
                                "Mission native compaction retains all provider values."))
                 (test-assert
                  (handler-case
                      (progn (provider-stream-turn provider conversation
                                                   :tool-namespaces #() :event-callback #'identity)
                             nil)
                    (mission-error (condition) (eq :exhausted (mission-error-reason condition))))
                  "Mission exhaustion rejects before telemetry or provider invocation.")))))
         (declare (ignore results))
         (test-telemetry-hooks--assert-attempts (first bodies) 2))
       (let ((goal (application-goal application)))
         (test-assert (= 2 (getf goal :turns-used)) "Both protocols charge one mission turn.")
         (test-assert (= 10 (getf goal :tokens-used)) "Both protocols settle exact reported usage.")
         (test-assert (eq :exhausted (getf goal :status)) "Mission exhaustion is durable.")
         (test-assert (= 1 (length (scripted-provider-results provider)))
                      "Rejected inference does not consume a scripted response.")))))
  nil)

(-> test-telemetry-provider-fallback-hooks () null)
(defun test-telemetry-provider-fallback-hooks ()
  "Custom providers get one standalone event despite broken metadata, with all values intact."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-fallback"))
            (result       (agent-test-result "custom-result" nil))
            (provider     (make-instance 'telemetry-test-values-provider
                                         :configuration configuration :results (list result))))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (provider-stream-turn provider conversation
                                    :tool-namespaces #() :event-callback #'identity)))
         (test-assert (equal results (list result :secondary 17)) "All custom provider values survive.")
         (test-telemetry-hooks--assert-attempts (first bodies) 1)
         (test-assert (null (test-telemetry--attribute
                             (first (test-telemetry-hooks--kind (first bodies) "model"))
                             "gen_ai.provider.name"))
                      "An inaccessible provider family is omitted."))
       ;; Scripted fixtures with no configuration are valid when telemetry is off.
       (let ((bare (make-instance 'telemetry-test-values-provider :results (list result))))
         (test-assert (equal (multiple-value-list
                              (provider-stream-turn bare conversation
                                                    :tool-namespaces #() :event-callback #'identity))
                            (list result :secondary 17))
                      "An unconfigured provider is usable outside an ambient run.")))))
  (test-telemetry-hooks--mission-accounting)
  nil)
