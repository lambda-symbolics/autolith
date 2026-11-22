(in-package #:autolith)

;;;; -- Vertex Product Integration Tests --

(-> vertex-test--selection-and-projection () null)
(defun vertex-test--selection-and-projection ()
  "Resolve both registrations, project tool history, and apply changed routing."
  (with-test-configuration (base)
    (dolist (entry '(("vertex-gemini" "gemini-3.5-flash" vertex-gemini-provider :gemini)
                     ("vertex-anthropic" "claude-haiku-4-5@20251001" vertex-anthropic-provider :anthropic)))
      (destructuring-bind (name model class family) entry
        (let* ((configuration (configuration-copy base :model model :model-provider name
                                                       :vertex-project "test-project"))
               (provider      (provider-create configuration))
               (conversation  (conversation-create configuration)))
          (test-assert (and (typep provider class) (eq (provider-family provider) family))
                       "Vertex selection uses its registered native protocol")
          (conversation-append-user-message conversation "Read a file")
          (conversation-append-provider-item
           conversation (json-object "type" "function_call" "namespace" "fs" "name" "read"
                                     "call_id" "call-1" "arguments" "{\"path\":\"a\"}"))
          (conversation-append-provider-item
           conversation (json-object "type" "function_call_output" "call_id" "call-1"
                                     "output" "file contents"))
          (let* ((request (provider-request-object provider conversation #()))
                 (body    (cl-llm-provider-api:vertex-request-body provider request)))
            (test-assert (and (equal (json-get request "model") model)
                              (not (json-get body "model"))
                              (search "file contents" (json-encode body))
                              (not (json-get body "session_id"))
                              (not (json-get body "request")))
                         "Vertex replays tool history without a Code Assist envelope"))
          (let ((updated (provider-with-configuration
                          provider (configuration-copy configuration :vertex-location "us-east5"))))
            (test-assert (and (equal (provider-session-id provider) (provider-session-id updated))
                              (search "us-east5-aiplatform.googleapis.com"
                                      (cl-llm-provider-api:vertex-request-url updated model)))
                         "reconfiguration applies routing and preserves the session")))))
    (test-assert
     (handler-case
         (progn (provider-create (configuration-copy base :model          "gemini-3.5-flash"
                                                          :model-provider "vertex-gemini"
                                                          :vertex-project ""))
                nil)
       (configuration-error () t))
     "missing resource projects fail before using credentials"))
  nil)

(-> vertex-test--fixture (string) string)
(defun vertex-test--fixture (name)
  "Return a complete native SSE response for either Vertex protocol."
  (if (string= name "vertex-gemini")
      (format nil "data: {\"responseId\":\"vertex-1\",\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":1,\"candidatesTokenCount\":1,\"totalTokenCount\":2}}~%~%")
      (format nil "event: message_start~%data: {\"type\":\"message_start\",\"message\":{\"id\":\"vertex-1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}~%~%event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}~%~%event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}~%~%event: content_block_stop~%data: {\"type\":\"content_block_stop\",\"index\":0}~%~%event: message_delta~%data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}~%~%event: message_stop~%data: {\"type\":\"message_stop\"}~%~%")))

(-> vertex-test--credential-scope-and-streaming () null)
(defun vertex-test--credential-scope-and-streaming ()
  "Exercise ADC loading, OAuth exchange, native routing, and both stream parsers."
  (with-test-configuration (base)
    (let* ((path      (merge-pathnames "adc.json" (config :state-root base)))
           (document  (json-encode (json-object "type" "authorized_user" "client_id" "test-client"
                                                "client_secret" "test-client-secret"
                                                "refresh_token" "test-refresh"
                                                "quota_project_id" "quota-project")))
           (exchanges 0))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction ':output :if-exists ':supersede)
        (write-string document stream))
      (with-test-environment (("GOOGLE_APPLICATION_CREDENTIALS" (namestring path)))
        (test-call-with-function-replacements
         (list (list 'vertex--token-request
                     (lambda (url &key headers content)
                       (declare (ignore headers content))
                       (test-assert (and *credentials-in-request-scope* (secret-use-active-p)
                                         (equal url "https://oauth2.googleapis.com/token"))
                                    "ADC refresh runs inside checkpoint secret accounting")
                       (incf exchanges)
                       (values (json-encode (json-object "access_token" "test-access" "expires_in" 60
                                                        "token_type" "Bearer"))
                              200))))
         (lambda ()
            (let* ((configuration (configuration-copy base :vertex-project "test-project"))
                   (registration  (provider-registration-find "vertex-gemini")))
              (test-assert (eq ':available (provider-credential-status configuration registration :refresh-p nil))
                           "valid local ADC can be inspected without a token exchange")
              (with-open-file (stream path :direction ':output :if-exists ':supersede)
                (write-string "{}" stream))
              (test-assert (eq ':failed (provider-credential-status configuration registration :refresh-p nil))
                           "malformed local ADC is a classified inspection failure")
              (delete-file path)
              (test-assert (eq ':missing (provider-credential-status configuration registration :refresh-p nil))
                           "absent local ADC is distinct from a malformed file")
              (test-assert (zerop exchanges) "none of the local inspections exchanges a token")
              (with-open-file (stream path :direction ':output :if-exists ':supersede)
                (write-string document stream)))
           (dolist (entry '(("vertex-gemini" "gemini-3.5-flash")
                            ("vertex-anthropic" "claude-haiku-4-5@20251001")))
             (destructuring-bind (name model) entry
               (let* ((configuration (configuration-copy base :model model :model-provider name
                                                              :vertex-project "test-project"))
                      (provider      (provider-create configuration))
                      (conversation  (conversation-create configuration))
                      (posted        nil))
                 (conversation-append-user-message conversation "hi")
                 (test-call-with-function-replacements
                  (list (list 'provider-post-event-stream
                              (lambda (url body &key headers)
                                (test-assert
                                 (and *credentials-in-request-scope* (secret-use-active-p)
                                      (equal (response-header headers "Authorization") "Bearer test-access")
                                      (equal (response-header headers "x-goog-user-project") "quota-project")
                                      (search "/projects/test-project/locations/global/" url)
                                      (not (json-get (json-decode body) "model")))
                                 "Vertex sends scoped OAuth credentials to the resource project")
                                (setf posted t)
                                (values (make-string-input-stream (vertex-test--fixture name)) 200 nil))))
                  (lambda ()
                    (let ((result (provider-attempt-turn
                                   provider conversation :tool-namespaces #()
                                   :event-callback                        (lambda (event) (declare (ignore event))))))
                      (test-assert (and posted (equal (provider-result-assistant-text result) "ok"))
                                   "both Vertex streams complete through the product request path")))))))
           (with-credentials (credentials (make-instance 'vertex-credential-manager) :force-refresh t)
             (test-assert (equal (oauth-credentials-access-token credentials) "test-access")
                          "forced refresh accepts a newly issued short-lived token")))))
      (test-assert (= exchanges 3) "each provider attempt or forced load exchanges a fresh token")
      (test-assert (and (not (secret-use-active-p))
                        (string= document (read-file-text path))
                        (not (probe-file (configuration-auth-path base))))
                   "credential ownership ends without copying or changing ADC")))
  nil)
