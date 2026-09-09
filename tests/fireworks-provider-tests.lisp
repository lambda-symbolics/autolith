(in-package #:autolith)

;;;; -- Fireworks API Key Provider Tests --

(-> fireworks-provider-test--chat-event (json-object string &optional t) json-object)
(defun fireworks-provider-test--chat-event (delta response-id &optional finish-reason)
  "Return one Chat Completions event for inherited provider routing checks."
  (json-object "id" response-id
               "choices" (json-array
                          (json-object "delta" delta
                                       "finish_reason" finish-reason))))

(-> fireworks-provider-test--configuration () configuration)
(defun fireworks-provider-test--configuration ()
  "Return an isolated configuration selecting the Fireworks model."
  (configuration-with-model (test-configuration)
                            "accounts/fireworks/models/kimi-k3"))

(-> fireworks-provider-test--selection () null)
(defun fireworks-provider-test--selection ()
  "Test Fireworks model selection and reasoning boundaries."
  (let ((configuration (fireworks-provider-test--configuration)))
    (test-assert
     (eq (model-family (configuration-model configuration)) ':fireworks)
     "the Fireworks model selects its provider family")
    (test-assert
     (string= (configuration-provider-endpoint configuration)
              *fireworks-responses-endpoint*)
     "the Fireworks model selects its endpoint")
    (test-assert
     (= (configuration-context-window configuration) 1048576)
     "the Fireworks model selects its context window")
    (test-assert
     (string= (configuration-fireworks-wire-effort
               (configuration-with-reasoning-effort configuration "none"))
              "low")
     "Fireworks clamps reasoning at its low boundary")
    (test-assert
     (string= (configuration-fireworks-wire-effort
               (configuration-with-reasoning-effort configuration "ultra"))
              "high")
     "Fireworks clamps reasoning at its high boundary"))
  nil)

(-> fireworks-provider-test--credential-source () null)
(defun fireworks-provider-test--credential-source ()
  "Test the Fireworks environment credential source without network access."
  (let* ((configuration (fireworks-provider-test--configuration))
         (root (test-configuration-root configuration))
         (manager (fireworks-credential-manager-create configuration))
         (saved (uiop:getenv "FIREWORKS_API_KEY")))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source manager)
            (make-instance 'oauth-credentials
                           :access-token "saved-fireworks-key"
                           :refresh-token nil
                           :id-token nil
                           :account-id "fireworks"
                           :expires-at nil
                           :source-path
                           (configuration-fireworks-auth-path configuration)))
           (platform-setenv "FIREWORKS_API_KEY" "environment-key-a")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "environment-key-a")
            "the environment key takes precedence over the saved key")
           (platform-setenv "FIREWORKS_API_KEY" "")
           (test-assert
            (string= (oauth-credentials-access-token
                      (credential-manager-load manager))
                     "saved-fireworks-key")
            "the saved interactive key is the environment fallback"))
      (platform-setenv "FIREWORKS_API_KEY" (or saved ""))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> fireworks-provider-test--request-shape () null)
(defun fireworks-provider-test--request-shape ()
  "Test Fireworks-specific Responses request controls."
  (let* ((configuration (fireworks-provider-test--configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "fireworks-shape"))
                (provider (fireworks-provider-create configuration)))
           (conversation-append-user-message conversation "hello")
           (let ((request
                   (provider-request-object provider conversation (json-array))))
            (test-assert
             (string= (json-get request "model")
                      "accounts/fireworks/models/kimi-k3")
             "Fireworks requests select the configured model")
            (test-assert
             (string= (json-get (json-get request "reasoning") "effort")
                      "high")
             "Fireworks requests use the clamped reasoning effort")
            (test-assert
             (null (json-get request "include"))
             "Fireworks requests omit Codex include extensions")
            (test-assert
             (notany (lambda (item)
                       (string= (or (json-get item "type") "")
                                "additional_tools"))
                     (json-get request "input"))
              "Fireworks requests omit Responses Lite additional tools")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> fireworks-provider-test--reasoning-omission () null)
(defun fireworks-provider-test--reasoning-omission ()
  "Test reasoning-free Fireworks models omit the reasoning object."
  (let* ((configuration
           (configuration-with-model
            (test-configuration) "accounts/fireworks/models/qwen3p7-plus"))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "fireworks-qwen-shape"))
                (provider (fireworks-provider-create configuration)))
           (conversation-append-user-message conversation "hello")
           (multiple-value-bind (reasoning present-p)
               (gethash "reasoning"
                        (provider-request-object provider conversation #()))
             (declare (ignore reasoning))
             (test-assert (not present-p)
                          "reasoning-free models omit the reasoning object")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> fireworks-provider-test--inherited-terminal-routing () null)
(defun fireworks-provider-test--inherited-terminal-routing ()
  "Test terminal policy inherited by Grok, Fireworks, and both Nous protocols."
  (labels ((consume (provider source)
             "Consume SOURCE through PROVIDER and return its result or typed condition."
             (handler-case
                 (provider-consume-stream
                  provider (make-string-input-stream source) nil #'identity)
               (provider-error (condition)
                 condition)))

           (assert-cases (name provider cases)
             "Assert terminal CASES for NAME and PROVIDER."
             (dolist (case cases)
               (destructuring-bind
                   (case-name source expected-type &optional expected-prompt-tokens)
                   case
                 (let ((outcome (consume provider source)))
                   (test-assert
                    (typep outcome expected-type)
                    (format nil "~A ~A yields ~A"
                            name case-name expected-type))
                   (when expected-prompt-tokens
                     (test-assert
                      (= (json-get (provider-result-usage outcome) "prompt_tokens")
                         expected-prompt-tokens)
                      (format nil "~A ~A retains trailing usage"
                              name case-name))))))))
    (let ((responses-cases
            `(("normal completion"
               ,(concatenate
                 'string
                 (test-sse-event-string
                  (json-object "type" "response.completed"
                               "response" (json-object "id" "inherited-response")))
                 (format nil "data: [DONE]~%~%"))
               provider-result)
              ("output truncation"
               ,(test-sse-event-string
                 (json-object
                  "type" "response.incomplete"
                  "response"
                  (json-object "id" "inherited-response"
                               "incomplete_details"
                               (json-object "reason" "max_output_tokens"))))
               provider-incomplete-response)
              ("completion without response"
               ,(test-sse-event-string
                 (json-object "type" "response.completed"))
               provider-protocol-error)
              ("incomplete without reason"
               ,(test-sse-event-string
                 (json-object
                  "type" "response.incomplete"
                  "response" (json-object "id" "inherited-response")))
               provider-protocol-error)
              ("incomplete with unknown reason"
               ,(test-sse-event-string
                 (json-object
                  "type" "response.incomplete"
                  "response"
                  (json-object "id" "inherited-response"
                               "incomplete_details"
                               (json-object "reason" "future_reason"))))
               provider-protocol-error)
              ("[DONE] before terminal"
               ,(format nil "data: [DONE]~%~%")
               response-stream-error)
              ("clean EOF before terminal"
               ,(test-sse-event-string
                 (json-object "type" "response.output_text.delta" "delta" "partial"))
               response-stream-error)))
          (chat-cases
            `(("normal completion"
               ,(concatenate
                 'string
                 (test-sse-event-string
                  (fireworks-provider-test--chat-event
                   (json-object "content" "complete") "inherited-chat" "stop"))
                 (test-sse-event-string
                  (json-object
                   "id" "inherited-chat"
                   "choices" (json-array)
                   "usage"
                   (json-object "prompt_tokens" 5
                                "completion_tokens" 3
                                "total_tokens" 8)))
                 (format nil "data: [DONE]~%~%"))
               provider-result
               5)
              ("output truncation"
               ,(test-sse-event-string
                 (fireworks-provider-test--chat-event
                  (json-object) "inherited-chat" "length"))
               provider-incomplete-response)
              ("[DONE] before terminal"
               ,(format nil "data: [DONE]~%~%")
               response-stream-error)
              ("clean EOF before terminal"
               ,(test-sse-event-string
                 (fireworks-provider-test--chat-event
                  (json-object "content" "partial") "inherited-chat"))
               response-stream-error)
              ("clean EOF after finish reason"
               ,(test-sse-event-string
                 (fireworks-provider-test--chat-event
                  (json-object) "inherited-chat" "stop"))
               response-stream-error))))
      (assert-cases
       "Grok"
       (allocate-instance (find-class 'grok-subscription-provider))
       responses-cases)
      (assert-cases
       "Fireworks"
       (allocate-instance (find-class 'fireworks-api-key-provider))
       responses-cases)
      (assert-cases
       "Nous Chat"
       (allocate-instance (find-class 'nous-chat-completions-provider))
       chat-cases))
    (dolist (case '(("end_turn" provider-result)
                    ("max_tokens" provider-incomplete-response)
                    (nil response-stream-error)))
      (destructuring-bind (stop-reason expected-type) case
        (let* ((events
                 (append
                  (list (json-object
                         "type" "message_start"
                         "message" (json-object "id" "inherited-messages"
                                                "type" "message"
                                                "role" "assistant"
                                                "content" #()
                                                "usage" (json-object "input_tokens" 1
                                                                     "output_tokens" 0))))
                  (when stop-reason
                    (list (json-object
                           "type" "message_delta"
                           "delta" (json-object "stop_reason" stop-reason)
                           "usage" (json-object "output_tokens" 1))
                          (json-object "type" "message_stop")))))
               (source (apply #'concatenate 'string
                              (mapcar #'test-sse-event-string events)))
               (outcome
                 (handler-case
                     (with-input-from-string (stream source)
                       (provider-consume-stream
                        (allocate-instance (find-class 'nous-messages-provider))
                        stream nil #'identity))
                   (provider-error (condition) condition))))
          (test-assert
           (typep outcome expected-type)
           (format nil "Nous Messages ~A yields ~A" stop-reason expected-type))))))
  nil)

(-> test-fireworks-provider () null)
(defun test-fireworks-provider ()
  "Test the Fireworks API key provider without network access."
  (fireworks-provider-test--selection)
  (fireworks-provider-test--credential-source)
  (fireworks-provider-test--request-shape)
  (fireworks-provider-test--reasoning-omission)
  (fireworks-provider-test--inherited-terminal-routing)
  nil)
