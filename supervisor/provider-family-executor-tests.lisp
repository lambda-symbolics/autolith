;;;; -- Standalone Parent Provider Family Checks --

(in-package #:autolith-supervisor)

(defparameter *provider-family-test-cases*
  '((:codex "gpt-fixture" :responses)
    (:grok "grok-fixture" :responses)
    (:fireworks "accounts/fireworks/models/fixture" :responses)
    (:openai-api "gpt-fixture" :responses)
    (:anthropic "claude-fixture" :anthropic)
    (:mistral "mistral-fixture" :chat-completions)
    (:openrouter "openrouter/anthropic/fixture" :chat-completions)
    (:opencode "opencode/fixture" :chat-completions)
    (:openai-compatible "custom-fixture" :chat-completions)
    (:nous "anthropic/fixture" :anthropic)
    (:nous "fixture" :chat-completions)
    (:gemini "gemini-fixture" :gemini)
    (:copilot "copilot/fixture" :responses)
    (:copilot "copilot/fixture" :anthropic)
    (:copilot "copilot/fixture" :chat-completions))
  "All built-in account routes, plus the trusted custom Chat Completions route.")

(declaim (ftype (function (keyword) string) provider-family-test--sse))

(defun provider-family-test--sse (protocol)
  "Return one real wire completion for each shared family parser."
  (ecase protocol
    (:responses
     (format nil
             "data: {\"type\":\"response.created\",\"response\":{\"id\":\"fixture-response\"}}~%~%data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"message\",\"id\":\"message-fixture\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"fixture-response\",\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}~%~%"))
    (:chat-completions
     (format nil
             "data: {\"id\":\"fixture-chat\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"ok\"},\"finish_reason\":null}]}~%~%data: {\"id\":\"fixture-chat\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}~%~%data: [DONE]~%~%"))
    (:anthropic
     (format nil
             "data: {\"type\":\"message_start\",\"message\":{\"id\":\"fixture-message\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}~%~%data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}~%~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}~%~%data: {\"type\":\"content_block_stop\",\"index\":0}~%~%data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}~%~%data: {\"type\":\"message_stop\"}~%~%"))
    (:gemini
     (format nil
             "data: {\"response\":{\"responseId\":\"fixture-gemini\",\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":1,\"candidatesTokenCount\":1}},\"traceId\":\"fixture-trace\"}~%~%"))))

(declaim (ftype (function (list &key (:endpoint string) (:summary-p boolean) (:kind keyword)
                           (:options list) (:manager cl-rfc8628:credential-manager))
                         (values model-profile provider-account model-invocation &optional))
                provider-family-test--binding))

(defun provider-family-test--binding (case &key (endpoint
                                                 "http://127.0.0.1:1/fixture") summary-p
                                           (kind
                                            ':generate) options
                                           (manager
                                            (make-instance 'provider-executor-test-manager)))
  "Build an account and the same seven logical inputs used by model-service."
  (destructuring-bind (family model protocol) case
    (let ((configuration (make-provider-account-configuration '(:state-root #P"/tmp/provider-family-fixture/"))))
      (multiple-value-bind (profile account)
          (make-parent-provider-profile :id (provider-account--identifier) :family family :model model
                                        :protocol protocol :endpoint endpoint :configuration configuration
                                        :credential-manager manager
                                        :options            (append (list :fixed-endpoint-p t) options)
                                        :capabilities       (when (eq family ':codex) '(:native-compaction)))
        (values profile account
                (make-instance 'model-invocation
                               :inputs   (list (make-instance 'cl-llm-provider-api:wire-request
                                                              :model model :prefix '("stable instructions")
                                                              :suffix  '("request instructions")
                                                              :options '(:maximum-output-tokens 128 :reasoning-effort "high"))
                                               #() profile kind summary-p nil nil)
                               :wire-id 1 :request-id "fixture-request" :turn-id "fixture-turn"
                               :deadline (+ (session-runtime--clock) 30)))))))

(declaim (ftype (function () (integer 0)) provider-family-test--projections))

(defun provider-family-test--projections ()
  "Check every account projection, public profile mapping, summary, and header policy."
  (let ((checks 0))
    (dolist (case *provider-family-test-cases*)
      (multiple-value-bind (profile account invocation)
          (provider-family-test--binding case)
        (multiple-value-bind (registry accounts)
            (make-parent-provider-profile-registry (list (list profile account)))
          (assert (eq (gethash (model-profile-id profile) registry) profile))
          (assert (eq (gethash profile accounts) account)))
        (let* ((provider    (provider-family--wire-provider account))
               (projection  (first (model-invocation-inputs invocation)))
               (ordinary
                (provider-family--request account :provider provider :projection projection :tools
                 #() :session-id "fixture-session"))
               (summary
                (provider-family--request account :provider provider :projection projection :tools
                 #() :summary-p t :session-id "fixture-session"))
               (credentials (provider-executor-test--credentials))
               (headers
                (provider-family--headers account credentials :session-id "fixture-session"
                 :user-agent "fixture" :request ordinary)))
          (assert (and (hash-table-p ordinary) (hash-table-p summary)))
          (assert
           (or (assoc "Authorization" headers :test #'string-equal)
               (assoc "x-api-key" headers :test #'string-equal)))
          (when (eq (first case) ':grok)
            (dolist (entry '(("x-grok-session-id" . "fixture-session")
                             ("x-grok-conv-id" . "fixture-session")
                             ("x-grok-agent-id" . "autolith")
                             ("x-grok-model-override" . "grok-fixture")))
              (assert (equal (rest (assoc (first entry) headers :test #'string=)) (rest entry))))
            (let ((again (provider-family--headers account credentials :session-id "fixture-session"
                          :user-agent "fixture" :request ordinary)))
              (assert (not (equal (rest (assoc "x-grok-req-id" headers :test #'string=))
                                  (rest (assoc "x-grok-req-id" again :test #'string=)))))))
          (when (member (first case) '(:opencode :openrouter :copilot))
            (assert
             (not
              (uiop/utility:string-prefix-p (format nil "~(~A~)/" (first case))
                                            (gethash "model" ordinary)))))
          (when (eq (third case) ':anthropic)
            (assert (gethash "cache_control" (aref (gethash "system" ordinary) 0)))
            (assert (not (gethash "cache_control" (aref (gethash "system" summary) 0)))))
          (when (member (first case) '(:mistral :openrouter))
            (assert (= (gethash "max_tokens" ordinary) 128)))
          (when (eq (first case) ':openrouter)
            (assert (equal (gethash "effort" (gethash "reasoning" ordinary)) "high")))
          (incf checks 6))))
    checks))

(declaim (ftype (function () (integer 0)) provider-family-test--execution))

(defun provider-family-test--execution ()
  "Consume all family completions and prove strict close before executor return."
  (let ((checks 0))
    (dolist (case *provider-family-test-cases*)
      (dolist (summary-p '(nil t))
        (multiple-value-bind (profile account invocation)
            (provider-family-test--binding case :summary-p summary-p)
          (when (eq (first case) ':gemini)
            (setf (provider-account-state account) '(:setup-complete-p t :project "fixture-project")))
          (let ((accounts (make-hash-table :test #'eq)) (opened nil) (events nil))
            (setf (gethash profile accounts) account)
            (multiple-value-bind (execute abort)
                (make-provider-family-model-executor
                 :accounts accounts :session-id "fixture-session" :maximum-attempts 1
                 :opener (lambda (endpoint request credentials &key &allow-other-keys)
                           (declare (ignore endpoint request credentials))
                           (let ((stream (make-string-input-stream (provider-family-test--sse (third case)))))
                             (push stream opened) (values stream 200 nil))))
              (let ((result (funcall execute invocation (lambda (event) (push event events)))))
                (assert (typep result 'cl-llm-provider-api:provider-result))
                (assert (cl-llm-provider-api:provider-result-output-items result))
                (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
                (assert (some (lambda (event) (typep event 'cl-llm-provider-api:provider-completed-event)) events))
                (funcall abort invocation)
                (incf checks 4)))))))
    checks))

(declaim (ftype (function () (integer 0)) provider-family-test--native))

(defun provider-family-test--native ()
  "Check opaque native checkpoint selection, bounded JSON, unsupported families, and strict close."
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:codex "gpt-fixture" :responses) :kind ':native-compaction)
    (let* ((accounts (make-hash-table :test #'eq))
           (opened   nil)
           (opaque   (make-string 12000 :initial-element #\A))
           (body
            (cl-rfc8628:json-object "output"
                                    (vector
                                     (cl-rfc8628:json-object "type" "compaction"
                                                             "encrypted_content" "old")
                                     (cl-rfc8628:json-object "type" "compaction"
                                                             "encrypted_content" opaque))
                                    "usage" (cl-rfc8628:json-object "input_tokens" 7))))
      (setf (gethash profile accounts) account)
      (multiple-value-bind (execute abort)
          (make-provider-family-model-executor :accounts accounts :session-id "fixture-session"
           :maximum-attempts 1 :opener
           (lambda (endpoint request credentials &key &allow-other-keys)
             (declare (ignore credentials))
             (assert (uiop/utility:string-suffix-p endpoint "/compact"))
             (assert (not (gethash "stream" request)))
             (let ((stream (make-string-input-stream (cl-rfc8628:json-encode body))))
               (push stream opened)
               (values stream 200 nil))))
        (let ((result (funcall execute invocation (constantly nil))))
          (assert
           (equal
            (gethash "encrypted_content"
                     (first (cl-llm-provider-api:provider-result-output-items result)))
            opaque))
          (assert
           (= (gethash "input_tokens" (cl-llm-provider-api:provider-result-usage result)) 7))
          (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
          (funcall abort invocation)))))
  (dolist (case (rest *provider-family-test-cases*))
    (multiple-value-bind (profile account invocation)
        (provider-family-test--binding case :kind ':native-compaction)
      (let ((accounts (make-hash-table :test #'eq)))
        (setf (gethash profile accounts) account)
        (multiple-value-bind (execute abort)
            (make-provider-family-model-executor :accounts accounts :session-id "fixture-session"
             :maximum-attempts 1)
          (assert
           (provider-executor-test--fails-p
            (lambda () (funcall execute invocation (constantly nil)))))
          (funcall abort invocation)))))
  20)

(declaim (ftype (function (list function) null) provider-family-test--local-http))

(defun provider-family-test--local-http (case verify)
  "Run the real HTTP adapter against a loopback peer and prove peer-observed close."
  (let* ((listener
          (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8)))
         (server      nil)
         (failure     nil)
         (peer-closed nil)
         (body        (provider-family-test--sse (third case))))
    (unwind-protect
        (progn
         (setf server
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                     (let ((socket
                            (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                       (unwind-protect
                           (cl-llm-provider-api:provider-call-with-response-deadline 5
                                                                                     (lambda ()
                                                                                       (let* ((raw
                                                                                               (usocket:socket-stream
                                                                                                socket))
                                                                                              (stream
                                                                                               (flexi-streams:make-flexi-stream
                                                                                                raw
                                                                                                :external-format
                                                                                                ':utf-8))
                                                                                              (target
                                                                                               (read-line
                                                                                                stream))
                                                                                              (headers
                                                                                               nil)
                                                                                              (length
                                                                                               0))
                                                                                         (loop for line = (string-right-trim
                                                                                                           '(#\Return)
                                                                                                           (read-line
                                                                                                            stream))
                                                                                               until (zerop
                                                                                                      (length
                                                                                                       line))
                                                                                               do (push
                                                                                                   line
                                                                                                   headers) (when
                                                                                                                (uiop/utility:string-prefix-p
                                                                                                                 "Content-Length:"
                                                                                                                 line)
                                                                                                              (setf length
                                                                                                                      (parse-integer
                                                                                                                       line
                                                                                                                       :start
                                                                                                                       15))))
                                                                                         (let ((request
                                                                                                (make-string
                                                                                                 length)))
                                                                                           (read-sequence
                                                                                            request
                                                                                            stream)
                                                                                           (funcall
                                                                                            verify
                                                                                            target
                                                                                            headers
                                                                                            (cl-rfc8628:json-decode
                                                                                             request)))
                                                                                         (write-sequence
                                                                                          (babel:string-to-octets
                                                                                           (format
                                                                                            nil
                                                                                            "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C~X~C~C~A~C~C0~C~C~C~C"
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            (length
                                                                                             body)
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            body
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline
                                                                                            #\Return
                                                                                            #\Newline)
                                                                                           :encoding
                                                                                           ':utf-8)
                                                                                          raw)
                                                                                         (finish-output
                                                                                          raw)
                                                                                         (setf peer-closed
                                                                                                 (null
                                                                                                  (read-byte
                                                                                                   raw
                                                                                                   nil
                                                                                                   nil))))))
                         (usocket:socket-close socket)))
                     (error (condition) (setf failure condition))))))
         (multiple-value-bind (profile account invocation)
             (provider-family-test--binding case :endpoint
              (format nil "http://127.0.0.1:~D/fixture" (usocket:get-local-port listener)))
           (when (eq (first case) ':gemini)
             (setf (provider-account-state account)
                   '(:setup-complete-p t :project "fixture-project")))
           (let ((accounts (make-hash-table :test #'eq)))
             (setf (gethash profile accounts) account)
             (multiple-value-bind (execute abort)
                 (make-provider-family-model-executor :accounts accounts :session-id
                  "fixture-session" :maximum-attempts 1)
               (assert
                (typep (funcall execute invocation (constantly nil))
                       'cl-llm-provider-api:provider-result))
               (funcall abort invocation))))
         (provider-executor-test--join server)
         (when failure (error failure))
         (assert peer-closed))
      (usocket:socket-close listener)
      (when server (provider-executor-test--join server))))
  nil)(declaim (ftype (function () (integer 0)) run-provider-family-executor-tests))

(defun run-provider-family-executor-tests ()
  "Run standalone family projection and real transport checks without loading the agent."
  (assert (not (find-package "AUTOLITH")))
  (let ((checks
         (+ (provider-family-test--projections) (provider-family-test--execution)
            (provider-family-test--native) (provider-family-test--dialects)
             (provider-family-test--setup-and-auto) (provider-family-test--auth-lifecycle)
             (provider-family-test--native-unavailable))))
    (dolist (case *provider-family-test-cases*)
      (provider-family-test--local-http case
       (lambda (target headers request)
         (assert (uiop/utility:string-prefix-p "POST /fixture" target))
         (assert
          (some
           (lambda (header)
             (or (uiop/utility:string-prefix-p "Authorization: Bearer " header)
                 (search "x-api-key: " header :test #'char-equal)))
           headers))
         (assert (hash-table-p request))))
      (incf checks 4))
    checks))

(declaim (ftype (function nil (integer 0)) provider-family-test--dialects))

(defun provider-family-test--dialects ()
  "Check Gemini alias resolution and bounded Grok loop policy."
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:gemini "flash" :gemini))
    (declare (ignore profile))
    (let* ((projection (first (model-invocation-inputs invocation)))
           (request
            (provider-family--request account :provider (provider-family--wire-provider account)
             :projection projection :tools #() :session-id "fixture")))
      (assert (equal (gethash "model" request) "gemini-3-flash-preview"))))
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:grok "grok-fixture" :responses))
    (declare (ignore profile))
    (let* ((provider                                   (provider-family--wire-provider account))
           (projection                                 (first (model-invocation-inputs invocation)))
           (tools
            (vector
             (cl-rfc8628:json-object "type" "namespace" "name" "web" "tools"
                                     (vector (cl-rfc8628:json-object "name" "run")))))
           (request
            (provider-family--request account :provider provider :projection projection :tools
             tools :session-id "fixture"))
           (*provider-family-grok-resamples-remaining* 2)
           (event
            (cl-rfc8628:json-object "doom_loop_check"
                                    (cl-rfc8628:json-object "triggers"
                                                            #("tail_repetition:32@thinking")))))
      (assert (= (length (gethash "tools" request)) 2))
      (assert (equal (gethash "type" (aref (gethash "tools" request) 0)) "web_search"))
      (dotimes (attempt 2)
        (handler-case
         (progn
          (cl-llm-provider-api:provider-note-doom-loop-event provider event)
          (error 'provider-executor-error :reason ':test-missing-resample))
         (cl-llm-provider-api:provider-resample-requested (condition)
          (assert
           (= (cl-llm-provider-api:provider-resample-requested-attempt condition) (1+ attempt))))))
      (assert (null (cl-llm-provider-api:provider-note-doom-loop-event provider event)))
      (assert (not (provider-family--grok-loop-trigger-p "tail_repetition:100@thinking")))
      (assert (not (provider-family--grok-loop-trigger-p "tail_repetition:32@output")))))
  8)

(declaim (ftype (function nil (integer 0)) provider-family-test--setup-and-auto))

(defun provider-family-test--setup-and-auto ()
  "Check Gemini tier fallback and one Copilot catalog refresh before selection."
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:gemini "gemini-fixture" :gemini))
    (let ((accounts (make-hash-table :test #'eq)) (opened nil))
      (setf (gethash profile accounts) account)
      (multiple-value-bind (execute abort)
          (make-provider-family-model-executor :accounts accounts :session-id "fixture"
           :maximum-attempts 1 :opener
           (lambda (endpoint request credentials &key &allow-other-keys)
             (declare (ignore request credentials))
             (let ((stream
                    (make-string-input-stream
                     (if (search "loadCodeAssist" endpoint)
                         (cl-rfc8628:json-encode
                          (cl-rfc8628:json-object "paidTier" (cl-rfc8628:json-object) "currentTier"
                                                  (cl-rfc8628:json-object "id" "standard-tier")
                                                  "cloudaicompanionProject" "fixture-project"))
                         (provider-family-test--sse ':gemini)))))
               (push stream opened)
               (values stream 200 nil))))
        (assert
         (typep (funcall execute invocation (constantly nil))
                'cl-llm-provider-api:provider-result))
        (assert (equal (getf (provider-account-state account) :tier) "standard-tier"))
        (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
        (funcall abort invocation))))
  (dolist (callback-p '(nil t))
    (let* ((refreshes 0)
           (opened    nil)
           (catalog
            (cl-rfc8628:json-object "data"
                                    (vector
                                     (cl-rfc8628:json-object "id" "responses" "supported_endpoints"
                                                             #("/responses")))))
           (options   (when callback-p (list :catalog-refresh (lambda () (incf refreshes) catalog)))))
      (multiple-value-bind (profile account invocation)
          (provider-family-test--binding '(:copilot "copilot/auto" :auto) :options options)
        (let ((accounts (make-hash-table :test #'eq)))
          (setf (gethash profile accounts) account)
          (multiple-value-bind (execute abort)
              (make-provider-family-model-executor :accounts accounts :session-id "fixture"
               :maximum-attempts 1 :opener
               (lambda (endpoint request credentials &key &allow-other-keys)
                 (declare (ignore request credentials))
                 (let ((stream
                        (make-string-input-stream
                         (cond
                          ((search "/models/session" endpoint)
                           (cl-rfc8628:json-encode
                            (cl-rfc8628:json-object "selected_model" "responses" "session_token"
                                                    "fixture-auto-session")))
                          ((uiop/utility:string-suffix-p endpoint "/models")
                           (incf refreshes)
                           (cl-rfc8628:json-encode catalog))
                          (t
                           (provider-family-test--sse ':responses))))))
                   (push stream opened)
                   (values stream 200 nil))))
            (assert
             (typep (funcall execute invocation (constantly nil))
                    'cl-llm-provider-api:provider-result))
            (assert (= refreshes 1))
            (assert
             (equal (getf (provider-account-state account) :preferred-model) "copilot/responses"))
            (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
            (funcall abort invocation))))))
  11)

(defclass provider-family-test-nous-manager (nous-credential-manager) ()
  (:documentation "Real Nous rotation over a synthetic in-memory source."))

(defmethod cl-rfc8628:credential-manager-call-with-refresh-lock
    ((manager provider-family-test-nous-manager) (function function))
  "Use the manager lock without a filesystem fixture."
  (funcall function))

(defclass provider-family-test-nous-source (provider-executor-test-source) ()
  (:documentation "In-memory rotating source with a stable fixture identity."))

(defmethod cl-rfc8628:credential-source-pathname ((source provider-family-test-nous-source))
  "Give the real refresh engine its persistence identity without disk I/O."
  #P"fixture")

(declaim (ftype (function () cl-rfc8628:credential-manager) provider-family-test--nous-manager))

(defun provider-family-test--nous-manager ()
  "Construct expired, scoped fixture credentials for the real rotating refresh engine."
  (let* ((source      (make-instance 'provider-family-test-nous-source))
         (credentials (make-instance 'cl-rfc8628:oauth-credentials
                        :access-token "e30.eyJzdWIiOiJmaXh0dXJlLWFjY291bnQiLCJzY29wZSI6ImluZmVyZW5jZTppbnZva2UifQ.fixture"
                        :refresh-token "fixture-before-rotation" :account-id "fixture-account"
                        :expires-at 0 :source-path #P"fixture")))
    (setf (provider-executor-test-source-credentials source) credentials)
    (make-instance 'provider-family-test-nous-manager :primary-source source)))

(declaim (ftype (function () (integer 0)) provider-family-test--auth-lifecycle))

(defun provider-family-test--auth-lifecycle ()
  "Test real Nous rotation and Gemini poll cancellation and strict close failures."
  (let ((checks 0))
    (dolist (family '(:nous :gemini))
      (dolist (outcome '(:success :cancel :close-error))
        (let* ((manager (if (eq family ':nous)
                            (provider-family-test--nous-manager)
                            (make-instance 'provider-executor-test-manager)))
               (opened nil) (target nil) (thread nil) (result nil) (failure nil)
               (calls 0) (auth-calls 0)
               (body    (cl-rfc8628:json-encode
                          (cl-rfc8628:json-object
                            "access_token" "e30.eyJzdWIiOiJmaXh0dXJlLWFjY291bnQiLCJzY29wZSI6ImluZmVyZW5jZTppbnZva2UifQ.fixture"
                            "refresh_token" "fixture-after-rotation" "expires_in" 3600))))
          (multiple-value-bind (profile account invocation)
              (provider-family-test--binding
                (if (eq family ':nous) '(:nous "fixture" :chat-completions) '(:gemini "fixture" :gemini))
                :manager manager)
            (let* ((accounts (make-hash-table :test #'eq))
                   (*provider-family-auth-open-function*
                     (lambda (url &key method headers content deadline-seconds)
                       (declare (ignore url method deadline-seconds))
                       (incf auth-calls)
                       (assert (equal (rest (assoc "x-nous-refresh-token" headers :test #'string=))
                                      "fixture-before-rotation"))
                       (assert (not (search "fixture-before-rotation" content)))
                       (setf target (if (eq outcome ':cancel)
                                        (make-instance 'provider-executor-test-stream)
                                        (make-string-input-stream body)))
                       (push target opened)
                       (values target 200 nil))))
              (setf (gethash profile accounts) account)
              (multiple-value-bind (execute abort)
                  (make-provider-family-model-executor
                    :accounts accounts :session-id "fixture-session" :maximum-attempts 1
                    :closer (lambda (stream)
                              (close stream :abort t)
                              (when (and (eq stream target) (eq outcome ':close-error))
                                (error 'provider-executor-error :reason ':test-close-failure)))
                    :opener
                    (lambda (endpoint request credentials &key &allow-other-keys)
                      (declare (ignore request credentials))
                      (incf calls)
                      (let ((stream
                              (cond
                                ((search ":loadCodeAssist" endpoint)
                                 (make-string-input-stream "{\"allowedTiers\":[{\"id\":\"free-tier\",\"isDefault\":true}]}"))
                                ((search ":onboardUser" endpoint)
                                 (make-string-input-stream "{\"name\":\"operations/fixture\"}"))
                                ((search "/operations/fixture" endpoint)
                                 (setf target
                                       (if (eq outcome ':cancel)
                                           (make-instance 'provider-executor-test-stream)
                                           (make-string-input-stream
                                             "{\"done\":true,\"response\":{\"cloudaicompanionProject\":{\"id\":\"fixture-project\"}}}"))))
                                (t
                                 (make-string-input-stream
                                  (provider-family-test--sse (if (eq family ':nous) ':chat-completions ':gemini)))))))
                        (push stream opened)
                        (values stream 200 nil))))
                (unwind-protect
                    (progn
                      (if (eq outcome ':cancel)
                          (let ((auth-open *provider-family-auth-open-function*))
                            (setf thread
                                  (bordeaux-threads:make-thread
                                    (lambda ()
                                      (let ((*provider-family-auth-open-function* auth-open))
                                        (handler-case (setf result (funcall execute invocation (constantly nil)))
                                          (error (condition) (setf failure condition)))))))
                            (loop repeat 500 until target do (sleep 0.02))
                            (assert target)
                            (provider-executor-test--wait (provider-executor-test-stream-read-started target))
                            (funcall abort invocation)
                            (provider-executor-test--join thread))
                          (handler-case (setf result (funcall execute invocation (constantly nil)))
                            (error (condition) (setf failure condition))))
                      (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
                     (when (and (eq outcome ':success) failure) (error failure))
                     (if (eq outcome ':success)
                         (progn (assert (typep result 'cl-llm-provider-api:provider-result))
                                (assert (null failure)))
                         (progn (assert (null result)) (assert failure)))
                     (if (eq family ':nous)
                         (progn
                           (assert (= auth-calls 1))
                           (assert (= calls (if (eq outcome ':success) 1 0)))
                           (assert (equal
                                     (cl-rfc8628:oauth-credentials-refresh-token
                                       (provider-executor-test-source-credentials
                                         (cl-rfc8628:credential-manager-primary-source manager)))
                                     (if (eq outcome ':success) "fixture-after-rotation" "fixture-before-rotation"))))
                         (progn
                           (assert (= calls (if (eq outcome ':success) 4 3)))
                           (assert (eq (not (null (getf (provider-account-state account) :setup-complete-p)))
                                       (eq outcome ':success)))))
                     (incf checks 7))
                 (handler-case (funcall abort invocation)
                   (provider-executor-error (condition)
                     (assert (and (eq outcome ':close-error)
                                  (eq (provider-executor-error-reason condition) ':cleanup-failed)))))
                 (when thread (provider-executor-test--join thread)))))))))
    checks))

(declaim (ftype (function () (integer 0)) provider-family-test--native-unavailable))

(defun provider-family-test--native-unavailable ()
  "Map only explicit unsupported native results to summary fallback after strict close."
  (dolist (case '(:missing :not-found :method-not-allowed :not-implemented :malformed :close-error))
    (multiple-value-bind (profile account invocation)
        (provider-family-test--binding '(:codex "fixture" :responses) :kind ':native-compaction)
      (let ((accounts (make-hash-table :test #'eq)) (opened nil) (failure nil))
        (setf (gethash profile accounts) account)
        (multiple-value-bind (execute abort)
            (make-provider-family-model-executor
              :accounts accounts :session-id "fixture" :maximum-attempts 1
              :closer (lambda (stream)
                        (close stream :abort t)
                        (when (eq case ':close-error)
                          (error 'provider-executor-error :reason ':test-close-failure)))
              :opener (lambda (endpoint request credentials &key &allow-other-keys)
                        (declare (ignore endpoint request credentials))
                        (let ((stream (make-string-input-stream
                                        (if (eq case ':malformed) "{\"output\":false}" "{\"output\":[]}"))))
                          (push stream opened)
                          (values stream (case case (:not-found
                                                     404) (:method-not-allowed
                                                           405)
                                               (:not-implemented
                                                501) (otherwise
                                                      200)) nil))))
          (handler-case (funcall execute invocation (constantly nil))
            (error (condition) (setf failure condition)))
          (assert failure)
          (assert (eq (model-service-executor-failure-code failure invocation)
                      (if (member case '(:missing :not-found :method-not-allowed :not-implemented))
                          ':unsupported ':unavailable)))
          (assert (every (lambda (stream) (not (open-stream-p stream))) opened))
          (handler-case (funcall abort invocation)
            (provider-executor-error (condition)
              (assert (and (eq case ':close-error)
                           (eq (provider-executor-error-reason condition) ':cleanup-failed)))))))))
  18)
