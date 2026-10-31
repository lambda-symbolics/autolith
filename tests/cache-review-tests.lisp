(in-package #:autolith)

;;;; -- Cache Lifetime and Final Wire Observation --

(defclass cache-review-test-provider (openrouter-chat-completions-provider) ()
  (:documentation "Construct real OpenRouter requests with an inert transport."))

(defmethod provider-open-response-stream
    ((provider cache-review-test-provider) (request hash-table) &key credentials conversation)
  "Accept the final request without credentials or network access."
  (declare (ignore provider request credentials conversation))
  (values nil 200 nil))

(-> test-cache-review-retention () null)
(defun test-cache-review-retention ()
  "Use Claude's requested lifetime for warm resumes and preserve override precedence."
  (with-test-configuration (configuration)
    (setf (configuration-validation-deferred-p configuration) t)
    (dolist (route '((anthropic-api-key-provider "claude-sonnet-5")
                     (openrouter-chat-completions-provider "openrouter/anthropic/claude-sonnet-4.5")))
      (destructuring-bind (class model) route
        (setf (config :model configuration) model)
        (let ((provider (if (eq class 'anthropic-api-key-provider)
                            (anthropic-provider-create configuration)
                            (openrouter-provider-create configuration))))
          (dolist (case '(("5m" 300) ("1h" 3600)))
            (destructuring-bind (ttl seconds) case
              (setf (config :prompt-cache-ttl configuration) ttl)
              (test-assert (= seconds (provider-prompt-cache-retention-seconds provider configuration))
                           "the local estimate follows the requested wire lifetime")
              (let* ((identity     (list (provider-family provider) model))
                     (conversation (conversation-create configuration)))
                (conversation-append-user-message conversation "start")
                (conversation-append-provider-metadata conversation
                                                      (list :request-number 1 :request-identity identity))
                (conversation-append-provider-item conversation (precompaction-tests--call "large"))
                (conversation-append-tool-result conversation "large" :tool-name "shell.run"
                                                 :output                         (make-string 10000 :initial-element #\x)
                                                 :success-p                      nil)
                (conversation-append-user-message conversation "next")
                (let ((loaded (conversation-load (conversation-pathname conversation))))
                  (test-assert (zerop (conversation-prepare-provider-request
                                       loaded :identity identity :idle-seconds seconds))
                               "a prompt Claude resume preserves its warm prefix"))
                (let ((loaded (conversation-load (conversation-pathname conversation))))
                  (test-assert (= 1 (conversation-prepare-provider-request
                                     loaded :identity identity :idle-seconds seconds
                                     :now (+ (get-universal-time) seconds 1)))
                               "a cold Claude resume records bounded output")))))
          (setf (config :prompt-cache-retention-seconds configuration) 1234)
          (test-assert (= 1234 (provider-prompt-cache-retention-seconds provider configuration))
                       "an explicit retention estimate overrides the requested TTL")
          (setf (config :prompt-cache-retention-seconds configuration) 0))))
    (setf (config :model configuration) "openrouter/openai/gpt-test")
    (test-assert (null (provider-prompt-cache-retention-estimate
                        (make-instance 'openrouter-chat-completions-provider :configuration configuration
                                                                            :family         ':openrouter)))
                 "unrelated OpenRouter routes have no implicit Claude lifetime"))
  nil)

(-> test-cache-review-final-wire () null)
(defun test-cache-review-final-wire ()
  "Observe routing fields and marker locations only after final request construction."
  (with-test-configuration (configuration)
    (setf (configuration-validation-deferred-p configuration) t)
    (setf (config :model configuration) "openrouter/anthropic/claude-sonnet-4.5")
    (let* ((provider                         (make-instance 'cache-review-test-provider :configuration configuration
                                                                                     :family           ':openrouter))
           (conversation                     (conversation-create configuration))
           (observed                         nil)
           (*provider-cache-request-prefix*  nil)
           (*provider-cache-prefix-function* (lambda (prefix) (setf observed prefix))))
      (conversation-append-user-message conversation "durable prefix")
      (flet ((request ()
               "Encode and observe one final inert transport request."
               (setf observed nil)
               (let ((request (provider-request-object provider conversation #())))
                 (test-assert (null observed) "intermediate encoding does not report a final wire plan")
                 (provider-open-response-stream provider request)
                 (test-assert (equal (getf observed :wire) (provider-cache-wire-plan request))
                              "the observed wire plan matches the final transport request")
                 (test-assert (plusp (provider-cache-prefix-characters observed))
                              "final routing preserves reusable history fingerprints")
                 observed)))
        (let ((base (request)))
          (setf (config :openrouter-provider-routing configuration) "{\"*\":{\"order\":[\"Anthropic\"]}}")
          (let* ((routed (request))
                 (change (provider-cache-wire-change (getf base :wire) (getf routed :wire))))
            (test-assert (member "provider" (getf change :fields) :test #'equal)
                         "routing-only changes are named in the final-request difference")
            (conversation-append-user-message conversation "appended durable item")
            (let* ((extended (request))
                   (change   (provider-cache-wire-change (getf routed :wire) (getf extended :wire))))
              (test-assert (getf change :breakpoints-moved-p)
                           "the observed difference includes final history cache marker movement")))))))
  nil)
