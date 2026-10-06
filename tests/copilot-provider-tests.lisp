(in-package #:autolith)

;;;; -- Copilot Subscription Checks --

(-> copilot-test--credentials (configuration &key (:expired-p boolean)) oauth-credentials)
(defun copilot-test--credentials (configuration &key expired-p)
  "Save deterministic Copilot credentials in an isolated private store."
  (let ((manager (copilot-credential-manager-create configuration)))
    (credential-source-save
     (credential-manager-primary-source manager)
     (make-instance 'oauth-credentials
                    :access-token "tid=test;proxy-ep=proxy.individual.githubcopilot.com;"
                    :refresh-token "github-secret" :account-id "github.com:42"
                    :expires-at (+ (get-universal-time) (if expired-p -10 3600))
                    :source-path (credential-source-pathname (credential-manager-primary-source manager))))))

(-> copilot-test--authentication () null)
(defun copilot-test--authentication ()
  "Exercise real device polling, HTTP 200 pending errors, exchange, storage, and renewal."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil))
    (with-test-configuration (configuration)
      (let* ((manager (copilot-credential-manager-create configuration))
             (client (copilot-device-authentication-client-create))
             (polls 0) (now 0) (exchanges 0))
        (setf (slot-value client 'cl-rfc8628::clock-function) (lambda () now)
              (slot-value client 'cl-rfc8628::sleep-function) (lambda (seconds) (incf now seconds)))
        (test-call-with-function-replacements
         (list
          (list 'dexador:post
                (lambda (url &rest arguments)
                  (test-assert (search *copilot-oauth-client-id* (getf arguments :content))
                               "device requests include the Copilot client ID")
                  (values
                   (if (search "/device/code" url)
                       "{\"device_code\":\"device-secret\",\"user_code\":\"ABCD-1234\",\"verification_uri\":\"https://github.com/login/device\",\"interval\":1,\"expires_in\":60}"
                       (case (incf polls)
                         (1 "{\"error\":\"authorization_pending\"}")
                         (2 "{\"error\":\"slow_down\"}")
                         (t "{\"access_token\":\"github-secret\"}")))
                   200 nil)))
          (list 'dexador:get
                (lambda (url &rest arguments)
                  (test-assert (equal (rest (assoc "Authorization" (getf arguments :headers) :test #'string-equal))
                                      "Bearer github-secret")
                               "only GitHub endpoints receive the GitHub token")
                  (values (json-encode
                           (if (search "/user" url)
                               (json-object "id" 42)
                               (progn (incf exchanges)
                                      (json-object "token" "tid=renewed;proxy-ep=proxy.business.githubcopilot.com;"
                                                   "expires_at" (+ (- (get-universal-time) 2208988800) 3600)))))
                          200 nil))))
         (lambda ()
           (device-authentication-complete client (device-authentication-request-code client) manager)
           (let ((credentials (credential-manager-load manager)))
             (test-assert (and (= polls 3) (= now 8)
                               (equal (oauth-credentials-refresh-token credentials) "github-secret")
                               (equal (oauth-credentials-account-id credentials) "github.com:42"))
                          "pending and slow_down poll before credentials are atomically saved")
             (test-assert (equal (copilot--base-url credentials) "https://api.business.githubcopilot.com")
                          "the token selects the subscription-specific API endpoint")
             (let ((renewed (credential-manager-refresh manager credentials)))
               (test-assert (and (= exchanges 2)
                                 (equal (oauth-credentials-refresh-token renewed) "github-secret")
                                 (equal (oauth-credentials-account-id renewed) "github.com:42"))
                            "force refresh keeps the GitHub credential and pinned account"))
             (test-assert (not (test-object-contains-string-p manager "github-secret"))
                          "managers do not retain tokens outside request scope")))))))
  nil)

(-> copilot-test--failures () null)
(defun copilot-test--failures ()
  "Reject hostile proxy hosts and malformed or rejected token responses without leaking secrets."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil))
    (with-test-configuration (configuration)
      (let* ((manager (copilot-credential-manager-create configuration))
             (credentials (copilot-test--credentials configuration)))
        (dolist (token '("proxy-ep=proxy.evil.example;" "proxy-ep=proxy.individual.githubcopilot.com/evil;"
                         "proxy-ep=proxy.individual.githubcopilot.com@evil.example;"))
          (let ((bad (make-instance 'oauth-credentials :access-token token :account-id "github.com:42")))
            (test-assert (handler-case (progn (copilot--base-url bad) nil)
                           (authentication-error () t)) "untrusted token hosts are rejected")))
        (dolist (response '("{}" "{\"token\":\"copilot-secret\",\"expires_at\":0}"))
          (test-call-with-function-replacements
           (list (list 'dexador:get (lambda (&rest ignored) (declare (ignore ignored)) (values response 200 nil))))
           (lambda ()
             (test-assert
              (handler-case (progn (credential-manager-refresh manager credentials) nil)
                (authentication-error (condition)
                  (not (test-object-contains-string-p condition "secret"))))
              "malformed exchange failures retain no secret values"))))
        (test-call-with-function-replacements
         (list (list 'dexador:get (lambda (&rest ignored) (declare (ignore ignored))
                                   (values "github-secret copilot-secret" 401 nil))))
         (lambda ()
           (test-assert (handler-case (progn (credential-manager-refresh manager credentials) nil)
                          (authentication-error (condition)
                            (not (test-object-contains-string-p condition "secret"))))
                        "HTTP rejection is typed and does not retain the body")))
        (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" "company.ghe.com"))
          (test-assert
           (handler-case (progn (credential-manager-validate-credentials
                                (copilot-credential-manager-create configuration) credentials) nil)
             (authentication-error () t)) "an issuer switch cannot reuse another host's GitHub token")))))
  nil)

(-> copilot-test--stream (string) stream)
(defun copilot-test--stream (url)
  "Return the smallest successful SSE stream for each Copilot wire endpoint."
  (make-string-input-stream
   (cond
     ((uiop:string-suffix-p url "/responses")
      (provider-tests--completed-sse-source "copilot-response"))
     ((uiop:string-suffix-p url "/v1/messages")
      (apply #'concatenate 'string
             (mapcar #'test-sse-event-string
                     (list (json-object "type" "message_start" "message"
                                        (json-object "id" "copilot-message" "type" "message"
                                                     "role" "assistant" "content" #()
                                                     "usage" (json-object "input_tokens" 1 "output_tokens" 0)))
                           (json-object "type" "message_delta" "delta"
                                        (json-object "stop_reason" "end_turn")
                                        "usage" (json-object "output_tokens" 1))
                           (json-object "type" "message_stop")))))
     (t (concatenate 'string
                     (test-sse-event-string
                      (json-object "id" "copilot-chat" "choices"
                                   (json-array (json-object "index" 0 "delta" (json-object "content" "ok")
                                                            "finish_reason" "stop"))))
                     (format nil "data: [DONE]~%~%"))))))

(-> copilot-test--discovery-and-transport () null)
(defun copilot-test--discovery-and-transport ()
  "Discover three wire routes, preserve namespaces, and send correct bearer and initiator headers."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil))
    (with-test-configuration (configuration)
      (let ((snapshot (provider--registry-snapshot)) (posts nil))
        (unwind-protect
             (progn
               (test-assert (typep (provider-authentication-provider configuration "copilot") 'copilot-chat-provider)
                            "autolith auth copilot bootstraps before discovery")
               (copilot-test--credentials configuration)
               (test-call-with-function-replacements
                (list
                 (list 'dexador:get
                       (lambda (url &rest arguments)
                         (declare (ignore arguments))
                         (test-assert (equal url "https://api.individual.githubcopilot.com/models") "discovery uses the token host")
                         (values
                          "{\"data\":[{\"id\":\"chat-test\",\"model_picker_enabled\":true,\"policy\":{\"state\":\"enabled\"},\"supported_endpoints\":[\"/chat/completions\"],\"capabilities\":{\"limits\":{\"max_prompt_tokens\":8192}}},{\"id\":\"claude-test\",\"model_picker_enabled\":true,\"supported_endpoints\":[\"/v1/messages\"]},{\"id\":\"gpt-test\",\"model_picker_enabled\":true,\"supported_endpoints\":[\"/responses\"]},{\"id\":\"disabled\",\"model_picker_enabled\":true,\"policy\":{\"state\":\"disabled\"}},{\"id\":\"no-tools\",\"model_picker_enabled\":true,\"capabilities\":{\"supports\":{\"tool_calls\":false}}}]}"
                          200 nil)))
                 (list 'provider-post-event-stream
                       (lambda (url body &key headers)
                         (push (list url (json-decode (babel:octets-to-string body :encoding :utf-8)) headers) posts)
                         (values (copilot-test--stream url) 200 nil))))
                (lambda ()
                  (let ((failures (provider-refresh-models configuration :provider-name "copilot")))
                    (when failures (error (first failures)))
                    (test-assert (null failures) "Copilot model discovery succeeds"))
                  (test-assert (equal (mapcar #'provider-model-name (provider-registration-models (provider-registration-find "copilot")))
                                      '("copilot/chat-test" "copilot/claude-test" "copilot/gpt-test"))
                               "disabled and non-tool models are excluded and vendor IDs are namespaced")
                  (dolist (spec '(("chat-test" copilot-chat-provider "/chat/completions")
                                  ("claude-test" copilot-messages-provider "/v1/messages")
                                  ("gpt-test" copilot-responses-provider "/responses")))
                    (let* ((selected (configuration-copy configuration :model (concatenate 'string "copilot/" (first spec)) :reasoning-effort "none"))
                           (provider (provider-create selected))
                           (conversation (conversation-create selected))
                           (credentials (credential-manager-load (provider-credential-manager provider))))
                      (test-assert (typep provider (second spec)) "discovered endpoints select the wire class")
                      (conversation-append-user-message conversation "hello")
                      (multiple-value-bind (request delivery)
                          (provider-request-object
                           provider conversation
                           (json-array
                            (json-object "type" "namespace" "name" "shell" "tools"
                                         (json-array (json-object "name" "run" "description" "Run a command."
                                                                  "parameters" (json-object "type" "object" "properties" (json-object)))))
                            (json-object "type" "namespace" "name" "mcp.test" "tools"
                                         (json-array (json-object "name" "read_file" "description" "Read a file."
                                                                  "parameters" (json-object "type" "object" "properties" (json-object)))))))
                        (declare (ignore delivery))
                        (test-assert (= (length (json-get request "tools")) 2) "each Copilot protocol advertises the tools")
                        (loop for tool across (json-get request "tools")
                              for function = (json-get tool "function")
                              for name = (if (hash-table-p function)
                                             (json-get function "name") (json-get tool "name"))
                              do (test-assert
                                  (and (plusp (length name)) (<= (length name) 64)
                                       (every (lambda (character)
                                                (find character "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")) name))
                                  "every advertised tool obeys the Copilot function-name grammar"))
                        (when (typep provider 'copilot-responses-provider)
                          (loop for tool across (json-get request "tools")
                                for expected in '(("shell" "run") ("mcp.test" "read_file"))
                                for wire-name = (json-get tool "name")
                                for call = (provider-normalize-output-item
                                            provider (json-object "type" "function_call" "call_id" "test-call"
                                                                  "name" wire-name "arguments" "{}"))
                                for replay = (provider-wire-input-item provider call)
                                do (test-assert
                                    (and (equal (json-get call "namespace") (first expected))
                                         (equal (json-get call "name") (second expected))
                                         (equal (json-get replay "name") wire-name)
                                         (equal (json-get replay "call_id") "test-call")
                                         (null (json-get replay "namespace")))
                                    "Responses tool calls decode for local dispatch and re-encode for follow-up requests")))
                        (test-assert (equal (json-get request "model") (first spec)) "local model prefixes never reach the API")
                        (provider-open-response-stream provider request :credentials credentials :conversation conversation))
                      (let ((headers (third (first posts))))
                        (test-assert (equal (first (first posts)) (concatenate 'string "https://api.individual.githubcopilot.com" (third spec)))
                                     "streams use the discovered endpoint")
                        (test-assert (and (equal (rest (assoc "X-Initiator" headers :test #'string-equal)) "user")
                                          (search "Bearer tid=test" (rest (assoc "Authorization" headers :test #'string-equal)))
                                          (null (assoc "x-api-key" headers :test #'string-equal)))
                                     "all protocols use Copilot bearer auth and identify user initiation"))
                      (let ((result (provider-stream-turn provider conversation :tool-namespaces #()
                                                          :event-callback (lambda (event) (declare (ignore event))))))
                        (test-assert (typep result 'provider-result)
                                     "each Copilot wire stream runs through the shared SSE parser"))
                      (conversation-append-provider-item conversation (json-object "type" "function_call" "call_id" "call-1" "name" "shell.run" "arguments" "{}"))
                      (test-assert (equal (rest (assoc "X-Initiator" (copilot--request-headers credentials conversation (json-object)) :test #'string-equal)) "agent")
                                   "tool follow-ups are correctly marked agent-initiated")
                      (let* ((other (configuration-copy selected :model "copilot/gpt-test"))
                             (switched (provider-with-configuration provider other)))
                        (test-assert (and (typep switched 'copilot-responses-provider)
                                          (eq (provider-credential-manager provider) (provider-credential-manager switched)))
                                     "cross-protocol model switches preserve the credential manager")))))))
          (provider--registry-restore snapshot)))))
  nil)

(-> copilot-test--model-policies () null)
(defun copilot-test--model-policies ()
  "Exercise policy enabling, Individual fallback, strict Business filtering, and cache failure."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil))
    (with-test-configuration (configuration)
      (let* ((credentials (copilot-test--credentials configuration))
             (posts 0)
             (state "unconfigured")
             (picker t))
        (test-call-with-function-replacements
         (list
          (list 'dexador:get
                (lambda (&rest ignored)
                  (declare (ignore ignored))
                  (values (json-encode (json-object "data" (json-array
                                    (json-object "id" "claude-test"
                                                 "model_picker_enabled" (if picker t (json-false))
                                                 "policy" (json-object "state" state))))) 200 nil)))
          (list 'dexador:post
                (lambda (url &rest arguments)
                  (test-assert (and (search "/models/claude-test/policy" url)
                                    (equal (getf arguments :content) "{\"state\":\"enabled\"}"))
                               "login enables only the selected policy endpoint")
                  (incf posts)
                  (setf state "enabled")
                  (values "{}" 200 nil))))
         (lambda ()
           (test-assert (null (copilot--fetch-models configuration)) "ordinary discovery does not change account policies")
           (test-assert (= posts 0) "no policy POST occurs without explicit login")
           (test-assert (equal (getf (first (copilot--fetch-models configuration :enable-p t)) :name) "copilot/claude-test")
                        "login includes a successfully enabled picker model")
           (setf picker nil)
           (test-assert (= (length (copilot--fetch-models configuration)) 1)
                        "Individual accounts fall back to explicitly enabled policies")
           (setf (slot-value credentials 'cl-rfc8628::access-token)
                 "tid=test;proxy-ep=proxy.business.githubcopilot.com;")
           (credential-source-save (credential-manager-primary-source (copilot-credential-manager-create configuration)) credentials)
           (test-assert (null (copilot--fetch-models configuration)) "Business accounts never bypass model picker restrictions")
           (setf picker t state "disabled")
           (test-assert (null (copilot--fetch-models configuration :enable-p t)) "disabled policies are never enabled automatically")
           (test-assert (= posts 1) "only the unconfigured model is enabled"))))))
  nil)

(-> copilot-test--login-selects-backend () null)
(defun copilot-test--login-selects-backend ()
  "Login through startup and the application, then send a prompt without ChatGPT credentials."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil) ("AUTOLITH_MODEL" nil))
    (with-test-configuration (configuration)
      (let ((snapshot (provider--registry-snapshot))
            (output (make-string-output-stream))
            (models-p t)
            (deny-p nil)
            (discoveries 0)
            (streams 0))
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'device-authentication-login
                     (lambda (client manager &key stream open-browser-p)
                       (declare (ignore client manager stream open-browser-p))
                       (when deny-p
                         (error 'authentication-error :message "Synthetic login denial."))
                       (copilot-test--credentials configuration)))
               (list 'dexador:get
                     (lambda (url &rest arguments)
                       (declare (ignore arguments))
                      (incf discoveries)
                       (test-assert (search "githubcopilot.com/models" url)
                                    "login discovers the Copilot account catalog")
                       (values (if models-p
                                   "{\"data\":[{\"id\":\"chat-test\",\"model_picker_enabled\":true},{\"id\":\"other-test\",\"model_picker_enabled\":true}]}"
                                   "{\"data\":[]}")
                               200 nil)))
               (list 'provider-post-event-stream
                     (lambda (url body &key headers)
                       (declare (ignore body))
                       (incf streams)
                       (test-assert (and (search "githubcopilot.com/chat/completions" url)
                                         (search "Bearer tid=test" (rest (assoc "Authorization" headers :test #'string-equal))))
                                    "the first prompt uses the saved Copilot credentials and endpoint")
                       (values (copilot-test--stream url) 200 nil))))
              (lambda ()
                (let ((*standard-output* output))
                  (main-authenticate configuration "copilot"))
                (test-assert (= discoveries 1) "login discovers the model catalog exactly once")
                (test-assert (equal (config :model configuration) "copilot/chat-test")
                             "startup login selects the authenticated backend")
                (test-assert (equal (getf (preferences-load-values configuration) :model) "copilot/chat-test")
                             "the selected backend is saved for subsequent launches")
                (test-assert (search "Active provider: copilot; model: copilot/chat-test." (get-output-stream-string output))
                             "startup reports the actual selected provider and model")
                (setf (config :model configuration) "copilot/other-test")
                (let ((*standard-output* output))
                  (main-authenticate configuration "copilot"))
                (test-assert (equal (config :model configuration) "copilot/other-test")
                             "reauthentication preserves a model already served by Copilot")
                (setf (config :model configuration) *default-model*)
                (let* ((conversation (conversation-create configuration))
                       (application
                         (make-instance 'application
                                        :configuration configuration :conversation conversation
                                        :provider (provider-create configuration)
                                        :tool-registry (make-instance 'tool-registry)
                                        :worker ':test-worker
                                        :ui (terminal-ui-create
                                             :terminal (make-instance 'stream-terminal
                                                                      :input-stream (make-string-input-stream "")
                                                                      :output-stream output :input-file-descriptor -1
                                                                      :columns 80)))))
                  (application-authenticate application "copilot")
                  (test-assert (and (typep (application-provider application) 'copilot-chat-provider)
                                    (eq (agent-provider (application-agent application)) (application-provider application))
                                    (equal (conversation-model conversation) "copilot/chat-test"))
                               "in-session login replaces the live provider, agent, and conversation selection")
                  (test-assert (equal (provider-result-assistant-text
                                      (agent-run-user-turn (application-agent application) "hello" :tools-p nil)) "ok")
                               "a prompt after login succeeds without any ChatGPT credentials")
                  (test-assert (= streams 1) "the prompt makes exactly one Copilot request")
                  (setf (config :model configuration) *default-model*
                        deny-p t)
                  (let ((*standard-output* output))
                    (test-assert (handler-case (progn (main-authenticate configuration "copilot") nil)
                                   (authentication-error () t)) "failed login propagates its error"))
                  (test-assert (equal (config :model configuration) *default-model*)
                               "failed login does not change the selected backend")
                  (setf deny-p nil models-p nil)
                  (let ((*standard-output* output))
                    (test-assert
                     (handler-case (progn (main-authenticate configuration "copilot") nil)
                       (configuration-error (condition)
                         (search "provider copilot has no available models" (autolith-error-message condition))))
                     "an empty account catalog reports why no backend can be selected")))))
          (provider--registry-restore snapshot)))))
  nil)

(-> copilot-test--domain-validation () null)
(defun copilot-test--domain-validation ()
  "Accept only GitHub.com and one canonical Enterprise Cloud tenant hostname."
  (test-assert
   (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" "github.com"))
     (string= (copilot--domain) "github.com"))
   "github.com is an accepted Copilot issuer")
  (test-assert
   (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" "Company.ghe.com"))
     (string= (copilot--domain) "company.ghe.com"))
   "Enterprise Cloud issuers are canonicalized to lowercase")
  (dolist (domain '("https://github.com" "ghe.com" "a.b.ghe.com"
                    "foo..ghe.com" "-tenant.ghe.com" "tenant-.ghe.com"
                    "tenant.ghe.com/path"))
    (test-assert
     (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" domain))
       (handler-case (progn (copilot--domain) nil)
         (configuration-error () t)))
     "invalid Copilot issuer is rejected"))
  nil)

(-> copilot-test--route-cache-validation () null)
(defun copilot-test--route-cache-validation ()
  "Reject missing, malformed, stale, and unknown routes at account construction."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil))
    (with-test-configuration (configuration)
      (setf configuration (configuration-copy configuration :model "copilot/chat-test"
                                                              :provider-validation-p nil))
      (let ((path (copilot--catalog-path configuration)))
        (dolist (cache '(missing
                         (:domain "github.com" :models "not-a-model-list")
                         (:domain "company.ghe.com"
                          :models ((:name "copilot/chat-test" :protocol :chat-completions)))
                         (:domain "github.com"
                          :models ((:name "copilot/chat-test" :protocol :unknown)))))
          (when (probe-file path)
            (delete-file path))
          (unless (eq cache 'missing)
            (snapshot-write path cache :mode #o600))
          (test-assert
           (handler-case (progn (copilot-provider-create configuration) nil)
             (configuration-error () t))
           "an invalid route cache fails with configuration-error")))))
  nil)

(-> copilot-test--authentication-repairs-route-cache () null)
(defun copilot-test--authentication-repairs-route-cache ()
  "Repair a selected model's route with and without registered model metadata."
  (with-test-environment (("AUTOLITH_COPILOT_DOMAIN" nil) ("AUTOLITH_MODEL" nil))
    (with-test-configuration (configuration)
      (setf configuration (configuration-copy configuration :model "copilot/chat-test"
                                                              :provider-validation-p nil))
      (let ((snapshot (provider--registry-snapshot))
            (path (copilot--catalog-path configuration))
            (logins 0)
            (discoveries 0)
            (output (make-string-output-stream)))
        (unwind-protect
             (progn
               (test-assert (null (provider-registration-models
                                   (provider-registration-find "copilot")))
                            "the first login bootstraps absent model metadata")
               (test-call-with-function-replacements
                (list
                 (list 'device-authentication-login
                       (lambda (client manager &key stream open-browser-p)
                         (declare (ignore client manager stream open-browser-p))
                         (incf logins)
                         (copilot-test--credentials configuration)))
                 (list 'dexador:get
                       (lambda (url &rest arguments)
                         (declare (ignore arguments))
                         (incf discoveries)
                         (test-assert (search "/models" url)
                                      "authentication refreshes the account catalog")
                         (values "{\"data\":[{\"id\":\"chat-test\",\"model_picker_enabled\":true,\"supported_endpoints\":[\"/chat/completions\"]}]}"
                                 200 nil))))
                (lambda ()
                  (dotimes (attempt 2)
                    (snapshot-write path
                                    '(:domain "github.com"
                                      :models ((:name "copilot/chat-test" :protocol :unknown)))
                                    :mode #o600)
                    (let ((*standard-output* output))
                      (main-authenticate configuration "copilot"))
                    (test-assert (= logins (1+ attempt))
                                 "explicit login proceeds independently of the invalid route")
                    (test-assert (= discoveries logins)
                                 "each login refreshes the catalog once")
                    (test-assert (typep (provider-create configuration) 'copilot-chat-provider)
                                 "the refreshed route constructs the selected account adapter")))))
          (provider--registry-restore snapshot)))))
  nil)
