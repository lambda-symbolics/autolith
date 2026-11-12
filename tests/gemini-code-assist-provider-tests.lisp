(in-package #:autolith)

;;;; -- Gemini Code Assist Provider Tests --

(-> gemini-code-assist-test--provider () gemini-code-assist-provider)
(defun gemini-code-assist-test--provider ()
  "Return an isolated Code Assist provider with an existing test manager."
  (let* ((configuration
           (configuration-copy (test-configuration) :model
                                     "gemini-3-flash-preview")))
    (gemini-code-assist-provider-create
     configuration
     :credential-manager (credential-manager-create configuration))))

(-> gemini-code-assist-test--consume-condition
    (gemini-code-assist-provider json-object &key (:headers t))
    provider-error)
(defun gemini-code-assist-test--consume-condition (provider event &key headers)
  "Consume one Gemini EVENT and return its expected typed failure."
  (handler-case
      (progn
        (provider-consume-stream
         provider
         (make-string-input-stream
          (format nil "data: ~A~%~%" (json-encode event)))
         (lambda (provider-event) (declare (ignore provider-event)))
         :headers headers)
        (error "The Gemini stream fixture did not signal a provider failure."))
    (provider-error (condition)
      condition)))

(-> gemini-code-assist-test--model-catalog () null)
(defun gemini-code-assist-test--model-catalog ()
  "Test stable aliases and static discovery for the private API."
  (let ((provider (gemini-code-assist-test--provider)))
    (test-assert
     (string= (gemini-code-assist-model-name "pro")
              "gemini-3-pro-preview")
     "Code Assist resolves the pro alias")
    (test-assert
     (find "gemini-2.5-flash"
           (gemini-code-assist-discover-models provider)
           :key  (lambda (entry) (getf entry ':name))
           :test #'string=)
     "Code Assist discovery returns upstream model identifiers"))
  nil)

(-> gemini-code-assist-test--request-conversion () null)
(defun gemini-code-assist-test--request-conversion ()
  "Test messages, calls, results, declarations, and thinking conversion."
  (let* ((provider      (gemini-code-assist-test--provider))
         (configuration (provider-configuration provider))
         (conversation  (conversation-create configuration))
         (tools
           (json-array
            (json-object
             "type" "namespace"
             "name" "fs"
             "description" "Files"
             "tools"
             (json-array
              (json-object "name" "read"
                           "description" "Read a file"
                           "parameters"
                           (json-object "type" "object"
                                        "properties" (json-object))))))))
    (conversation-append-user-message conversation "hello")
    (conversation-append-provider-item
     conversation
     (json-object "type" "reasoning_content" "content" "considering"))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call" "namespace" "fs" "name" "read"
                  "call_id" "call-1" "arguments" "{\"path\":\"a\"}"))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call_output" "call_id" "call-1"
                  "output" "{\"text\":\"ok\"}"))
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation tools
                                 :goal-context "keep the active goal")
      (let* ((inner              (json-get request "request"))
             (contents           (json-get inner "contents"))
             (system-instruction (json-get inner "systemInstruction"))
             (system-text
               (json-get (aref (json-get system-instruction "parts") 0)
                         "text"))
             (context-content    (aref contents (1- (length contents))))
             (context-text
               (json-get (aref (json-get context-content "parts") 0) "text"))
             (declarations
               (json-get (aref (json-get inner "tools") 0)
                         "functionDeclarations"))
             (call-part
               (loop for content across contents
                     thereis (loop for part across (json-get content "parts")
                                   thereis (json-get part "functionCall"))))
             (response-part
               (loop for content across contents
                     thereis (loop for part across (json-get content "parts")
                                   thereis (json-get part "functionResponse"))))
             (thought-part
               (loop for content across contents
                     thereis (loop for part across (json-get content "parts")
                                   when (json-get part "thought")
                                     return part))))
        (test-assert
         (json-get (json-get (json-get request "request") "generationConfig")
                   "thinkingConfig")
         "Code Assist requests thinking at the configured effort")
        (let ((*provider-reasoning-effort* "none"))
          (multiple-value-bind (overridden ignored-delivery)
              (provider-request-object provider conversation tools
                                       :goal-context "keep the active goal")
            (declare (ignore ignored-delivery))
            (test-assert
             (let ((generation-config
                     (json-get (json-get overridden "request") "generationConfig")))
               (or (null generation-config)
                   (null (json-get generation-config "thinkingConfig"))))
             "a request-local effort disables Code Assist thinking")))
        (test-assert
         (string= (json-get (aref declarations 0) "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist emits flat function declarations")
        (test-assert
         (string= (json-get call-part "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist emits function calls")
        (test-assert
         (string= (json-get response-part "name")
                  (provider-wire-function-name--encode "fs" "read"))
         "Code Assist matches function responses to calls")
        (test-assert thought-part
                     "Code Assist preserves thinking parts")
        (test-assert
         (and delivery
              (string= system-text (system-prompt configuration))
              (null (search "keep the active goal" system-text))
              (null (search "Temporary context" system-text))
              (string= (json-get context-content "role") "user")
              (search "keep the active goal" context-text)
              (search "Temporary context" context-text))
         "Code Assist trails volatile context after cacheable conversation input"))))
  nil)

(-> gemini-code-assist-test--stream-fixture () null)
(defun gemini-code-assist-test--stream-fixture ()
  "Test streamed text, thinking, calls, usage, and finish handling."
  (let* ((provider  (gemini-code-assist-test--provider))
         (wire-name (provider-wire-function-name--encode "fs" "read"))
         (event
           (json-object
            "traceId" "trace-1"
            "response"
            (json-object
             "candidates"
             (json-array
              (json-object
               "content"
               (json-object
                "role" "model"
                "parts"
                (json-array
                 (json-object "text" "thinking" "thought" t)
                 (json-object "text" "answer")
                 (json-object
                  "functionCall"
                  (json-object "id" "call-1" "name" wire-name
                               "args" (json-object "path" "a")))))
               "finishReason" "STOP"))
             "usageMetadata"
             (json-object "promptTokenCount" 10
                          "candidatesTokenCount" 4
                          "thoughtsTokenCount" 2
                          "totalTokenCount" 16))))
         (stream    (make-string-input-stream
                     (format nil "data: ~A~%~%" (json-encode event))))
         (events    nil)
         (result
           (provider-consume-stream
            provider stream
            (lambda (provider-event) (push provider-event events)))))
    (test-assert
     (string= (provider-result-response-id result) "trace-1")
     "Code Assist retains the trace identifier")
    (test-assert
     (= (json-get (provider-result-usage result) "input_tokens") 10)
     "Code Assist normalizes prompt usage")
    (test-assert
     (= (length (provider-result-tool-calls result)) 1)
     "Code Assist returns one portable tool call")
    (test-assert
     (string= (json-get (first (provider-result-tool-calls result)) "namespace")
              "fs")
     "Code Assist decodes the tool namespace")
    (test-assert
     (find-if (lambda (event) (typep event 'reasoning-delta-event)) events)
     "Code Assist emits thinking deltas")
    (test-assert
     (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
     "Code Assist emits assistant deltas"))
  nil)

(-> gemini-code-assist-test--credential-redaction () null)
(defun gemini-code-assist-test--credential-redaction ()
  "Test credential redaction across Gemini stream failure conditions."
  (let* ((provider                                      (gemini-code-assist-test--provider))
         (access-token                                  "ya29.a0AfH6SMB-access-token-fixture")
         (refresh-token                                 "1//0g-refresh-token-fixture")
         (*provider-active-credential-values*           (list access-token refresh-token))
         (*provider-active-credential-redaction-marker* "[REDACTED]")
         (finish-reason
           (format nil "finish-before-~A-middle-~A-after"
                   access-token refresh-token))
         (finish-condition
           (gemini-code-assist-test--consume-condition
            provider
            (json-object
             "traceId" (format nil "trace-before-~A-after" access-token)
             "response"
             (json-object
              "candidates"
              (json-array (json-object "finishReason" finish-reason))))
            :headers
            (list
             (cons "x-request-id"
                   (format nil "request-before-~A-after" refresh-token)))))
         (finish-report                                 (format nil "~A" finish-condition))
         (finish-metadata                               (agent--provider-error-metadata finish-condition))
         (finish-metadata-text                          (prin1-to-string finish-metadata))
         (error-event
           (json-object
            "error"
            (json-object
             "message" (format nil "message-before-~A-after" access-token)
             "status" (format nil "status-before-~A-after" refresh-token)
             "code" 403)))
         (error-condition
           (gemini-code-assist-test--consume-condition
            provider error-event
            :headers
            (list
             (cons "request-id"
                   (format nil "request-before-~A-after" access-token)))))
         (error-report                                  (format nil "~A" error-condition))
         (error-response                                (provider-error-response error-condition))
         (error-metadata                                (agent--provider-error-metadata error-condition))
         (error-metadata-text                           (prin1-to-string error-metadata)))
    (dolist (secret (list access-token refresh-token))
      (test-assert
       (and (null (search secret finish-report))
            (null (search secret (autolith-error-message finish-condition)))
            (null (search secret (provider-error-code finish-condition)))
            (null (search secret (provider-error-request-id finish-condition)))
            (null (search secret (provider-error-response-id finish-condition)))
            (null (search secret finish-metadata-text)))
       "Gemini finish failures redact every active credential")
      (test-assert
       (and (null (search secret error-report))
            (null (search secret (autolith-error-message error-condition)))
            (null (search secret (provider-error-code error-condition)))
            (null (search secret (provider-error-request-id error-condition)))
            (null (search secret error-response))
            (null (search secret error-metadata-text)))
       "Gemini error events redact every active credential"))
    (test-assert
     (and (typep finish-condition 'provider-protocol-error)
          (string= (provider-error-code finish-condition) "invalid_finish_reason")
          (search "finish-before-" finish-report)
          (search "-middle-" finish-report)
          (search "-after" finish-report)
          (search "trace-before-" (provider-error-response-id finish-condition))
          (search "request-before-" (provider-error-request-id finish-condition))
          (search "finish-before-" finish-metadata-text))
     "Gemini finish failures preserve safe shared protocol metadata")
    (test-assert
     (and (= (provider-error-status error-condition) 403)
          (search "message-before-" error-report)
          (search "status-before-" (provider-error-code error-condition))
          (search "request-before-" (provider-error-request-id error-condition))
          (search "message-before-" error-response)
          (search "status-before-" error-response)
          (search "message-before-" error-metadata-text))
     "Gemini error events preserve typed fields and safe surrounding text"))
  nil)

(-> gemini-code-assist-test--setup-and-retry () null)
(defun gemini-code-assist-test--setup-and-retry ()
  "Test loadCodeAssist project setup and provider-specific HTTP retry."
  (let ((provider                                   (gemini-code-assist-test--provider))
        (attempts                                   0)
        (*gemini-code-assist-nonstream-retry-delay* 0))
    (test-assert
     (and (provider-retryable-status-p provider 499 nil)
          (provider-retryable-status-p provider 599 nil)
          (not (provider-retryable-status-p provider 498 nil)))
     "Code Assist recognizes its additional transient statuses")
    (test-call-with-function-replacements
     (list
      (list
       'gemini-code-assist--post-once
       (lambda (actual-provider credentials &key method request)
         (declare (ignore credentials method request))
         (incf attempts)
         (if (= attempts 1)
             (provider-signal-http-failure
              actual-provider
              (make-condition
               'http-request-failed
               :body    "temporary Code Assist failure"
               :status  599
               :headers nil
               :uri     nil
               :method  ':post))
             (values
              (json-encode
               (json-object "currentTier" (json-object "id" "free-tier")
                            "cloudaicompanionProject" "managed-project"))
              200
              nil)))))
     (lambda ()
       (gemini-code-assist-ensure-setup
        provider
        (make-instance 'oauth-credentials
                       :access-token "fixture-token"
                       :expires-at   (+ (get-universal-time) 3600)))))
    (test-assert (= attempts 2)
                 "Code Assist retries a raised provider-specific HTTP failure")
    (test-assert
     (string= (gemini-code-assist-provider-project provider) "managed-project")
     "Code Assist records the managed project")
    (test-assert
     (string= (gemini-code-assist-provider-tier provider) "free-tier")
     "Code Assist records the selected tier"))
  nil)

(-> gemini-code-assist-test--builtin-registration () null)
(defun gemini-code-assist-test--builtin-registration ()
  "Test built-in model selection and OAuth authentication wiring."
  (let* ((configuration
           (configuration-copy (test-configuration) :model
                                     "gemini-3-flash-preview"))
         (registration (provider-registration-find "gemini"))
         (provider     (provider-create configuration)))
    (test-assert
     (and registration
          (eq (provider-registration-family registration)
              ':gemini-code-assist)
          (eq (provider-registration-protocol registration)
              ':gemini-code-assist))
     "Gemini is registered as a built-in Code Assist provider")
    (test-assert
     (and (typep provider 'gemini-code-assist-provider)
          (typep (provider-credential-manager provider)
                 'gemini-credential-manager)
          (eq (model-provider-registration provider) registration))
     "Gemini model selection creates the subscription provider and OAuth manager"))
  nil)
