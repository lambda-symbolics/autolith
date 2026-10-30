;;;; -- Trusted Provider Family Execution --

(in-package #:autolith-supervisor)

(defvar *provider-family-account* nil
  "The request-local trusted account used by injected transport functions.")

(defvar *provider-family-session-token* nil
  "Request-local Copilot Auto token. Never store this in account state.")

(defvar *provider-family-http-accept* "text/event-stream"
  "The request-local response media type.")

(defvar *provider-family-http-method* ':post
  "The request-local HTTP method for subscription setup.")

(declaim (ftype (function (provider-account cl-rfc8628:oauth-credentials
                           &key (:session-id string) (:user-agent string) (:request t)) list)
                provider-family--headers))

(defun provider-family--headers (account credentials &key session-id user-agent request)
  "Construct authenticated headers only from parent account and request policy."
  (let* ((family (provider-account-family account))
         (token  (cl-rfc8628:oauth-credentials-access-token credentials))
         (base   (list (cons "Content-Type" "application/json")
                       (cons "Accept" *provider-family-http-accept*)
                       (cons "User-Agent" user-agent))))
    (append
     (case family
       (:codex
         (chatgpt-request-headers credentials :session-id session-id
                                  :user-agent user-agent :accept *provider-family-http-accept*))
       (:anthropic
         (append (list (cons "x-api-key" token) (cons "anthropic-version" "2023-06-01")) base))
       (:grok
         (append (list (cons "Authorization" (format nil "Bearer ~A" token))
                       (cons "X-XAI-Token-Auth" "xai-grok-cli")
                       (cons "x-authenticateresponse" "authenticate-response")
                       (cons "x-grok-client-version" "1.0.13")
                       (cons "x-grok-client-mode" "interactive")
                       (cons "x-grok-client-identifier" "autolith")
                       (cons "x-grok-session-id" session-id)
                       (cons "x-grok-conv-id" session-id)
                       (cons "x-grok-req-id" (provider-account--identifier))
                       (cons "x-grok-agent-id" "autolith")
                       (cons "x-grok-model-override" (gethash "model" request))
                       (cons "x-grok-doom-loop-check" "1024")) base))
       (:copilot
         (if (string= *provider-family-http-accept* "text/event-stream")
             (append
              (cl-llm-provider-api:copilot-stream-headers
               token request :protocol (case (provider-account-protocol account)
                                         (:anthropic
                                          ':messages)
                                         (otherwise
                                          (provider-account-protocol account)))
               :initiator              (getf (provider-account-options account) :initiator "user")
               :user-agent user-agent :anthropic-version "2023-06-01")
              (when *provider-family-session-token*
                (list (cons "Copilot-Session-Token" *provider-family-session-token*))))
             (append (cl-llm-provider-api:copilot-http-headers token :user-agent user-agent)
                     (list (cons "Accept" *provider-family-http-accept*)
                           (cons "Content-Type" "application/json")
                           (cons "X-GitHub-Api-Version" "2026-06-01")
                           (cons "X-Client-Session-Id" session-id)))))
       (otherwise
         (append (list (cons "Authorization" (format nil "Bearer ~A" token))) base)))
     (when (eq family ':opencode) (list (cons "x-opencode-session" session-id)))
     (when (eq family ':openrouter)
       '(("HTTP-Referer" . "https://github.com/lambda-symbolics/autolith")
         ("X-OpenRouter-Title" . "Autolith")))
     (copy-tree (provider-account-headers account)))))

(declaim (ftype (function (string t cl-rfc8628:oauth-credentials
                           &key (:session-id string) (:turn-state t) (:user-agent string)
                           (:deadline-seconds real)) (values t t t &optional))
                provider-family-open-response-stream))

(defun provider-family-open-response-stream (endpoint request credentials
                                             &key session-id turn-state user-agent (deadline-seconds 300))
  "Open an owned, unpooled transport with trusted endpoint, method, and headers."
  (let ((headers (provider-family--headers *provider-family-account* credentials
                                           :session-id session-id :user-agent user-agent :request request)))
    (when (and turn-state (eq (provider-account-family *provider-family-account*) ':codex))
      (setf headers (append headers (list (cons "x-codex-turn-state" turn-state)))))
    (if (eq *provider-family-http-method* ':get)
        (dexador:get endpoint :headers headers :want-stream t :keep-alive nil
                             :use-connection-pool nil :max-redirects 0
                             :connect-timeout (min 30 deadline-seconds) :read-timeout deadline-seconds)
        (cl-llm-provider-api:provider-post-event-stream
         endpoint (cl-llm-provider-api::json-encode request) :headers headers
         :keep-alive-p nil :deadline-seconds deadline-seconds
         :connect-timeout (min 30 deadline-seconds) :read-timeout deadline-seconds))))

(defmethod provider-executor-open-stream
           ((account provider-account) &key executor request credentials invocation turn-state)
  "Open the trusted family route, including renewed Copilot account routing."
  (let* ((*provider-family-account* account)
         (endpoint                  (provider-account-endpoint account))
         (*provider-executor-additional-secrets*
          (when *provider-family-session-token* (list *provider-family-session-token*))))
    (case (provider-account-family account)
      (:gemini
       (setf endpoint
               (concatenate 'string (string-right-trim "/" endpoint)
                            ":streamGenerateContent?alt=sse")))
      (:copilot
       (unless (getf (provider-account-options account) :fixed-endpoint-p)
         (setf endpoint
                 (concatenate 'string (copilot--base-url credentials)
                              (cl-llm-provider-api:copilot-protocol-endpoint
                               (case (provider-account-protocol account)
                                 (:anthropic
                                  ':messages)
                                 (otherwise
                                  (provider-account-protocol account)))))))))
    (funcall (chatgpt-executor-opener executor) endpoint request credentials :session-id
             (chatgpt-executor-session-id executor) :turn-state turn-state :user-agent
             (chatgpt-executor-user-agent executor) :deadline-seconds
             (- (model-invocation-deadline invocation)
                (funcall (chatgpt-executor-clock executor))))))(declaim (ftype (function (t) string) provider-family--response-text))

(defun provider-family--response-text (body)
  "Read a successful JSON body up to 8 MiB without closing its owned transport."
  (let* ((limit (* 8 1024 1024))
         (text
           (cond
             ((stringp body)
              body)
             ((and (streamp body) (subtypep (stream-element-type body) 'character))
              (with-output-to-string (out)
                (loop for character = (read-char body nil nil)
                      for count from 0
                      while character
                      do (when (>= count limit)
                           (error 'provider-executor-error :reason ':response-too-large))
                         (write-char character out))))
             ((streamp body)
              (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
                (loop for byte = (read-byte body nil nil)
                      while byte
                      do (when (>= (length bytes) limit)
                           (error 'provider-executor-error :reason ':response-too-large))
                         (vector-push-extend byte bytes))
                (babel:octets-to-string bytes :encoding ':utf-8)))
             (t
               (error 'provider-executor-error :reason ':invalid-json)))))
    (when (> (length (babel:string-to-octets text :encoding ':utf-8)) limit)
      (error 'provider-executor-error :reason ':response-too-large))
    text))

(declaim (ftype (function (provider-account &key (:executor chatgpt-executor)
                           (:state provider-http-state) (:invocation model-invocation)
                           (:credentials cl-rfc8628:oauth-credentials) (:endpoint string)
                           (:request t) (:method keyword)) hash-table) provider-family--rpc))

(defun provider-family--rpc
       (account &key executor state invocation credentials endpoint request (method ':post))
  "Run one bounded JSON RPC under the same owned transport and attempt deadline."
  (let* ((*provider-family-account*     account)
         (*provider-family-http-accept* "application/json")
         (*provider-family-http-method* method)
         (provider
          (if (eq (provider-account-protocol account) ':auto)
              (make-instance 'family-chat-provider :account account)
              (provider-family--wire-provider account)))
         (closer                        (chatgpt-executor-closer executor))
         (clock                         (chatgpt-executor-clock executor))
         (*provider-executor-additional-secrets*
          (when *provider-family-session-token* (list *provider-family-session-token*))))
    (cl-llm-provider-api:provider--call-with-transport-normalization
     (lambda ()
       (let ((cl-llm-provider-api:*provider-active-credential-values*
              (cl-rfc8628:oauth-credentials-secret-values credentials)))
         (unwind-protect
             (cl-llm-provider-api:provider-call-with-response-deadline
              (- (model-invocation-deadline invocation) (funcall clock))
              (lambda ()
                (multiple-value-bind (stream status headers)
                    (provider-http--open state
                     (lambda ()
                       (provider-http--check-invocation state invocation clock)
                       (provider-http--http-values
                        (lambda ()
                          (funcall (chatgpt-executor-opener executor) endpoint request credentials
                                   :session-id (chatgpt-executor-session-id executor) :turn-state
                                   nil :user-agent (chatgpt-executor-user-agent executor)
                                   :deadline-seconds
                                   (- (model-invocation-deadline invocation) (funcall clock))))))
                     closer)
                  (unless (and (integerp status) (<= 200 status 299))
                    (cl-llm-provider-api:provider--signal-http-status-failure provider status
                                                                              :headers  headers
                                                                              :raw-body stream))
                  (let* ((text (provider-family--response-text stream))
                         (document
                          (handler-case (cl-rfc8628:json-decode text)
                                        (error nil
                                               (error 'provider-executor-error :reason
                                                      ':invalid-json)))))
                    (unless (hash-table-p document)
                      (error 'provider-executor-error :reason ':invalid-json))
                    document))))
           (provider-http--close state closer)))))))

(declaim (ftype (function (provider-account &key (:executor chatgpt-executor)
                           (:state provider-http-state) (:invocation model-invocation)
                           (:credentials cl-rfc8628:oauth-credentials)) null)
                provider-family--gemini-setup))

(defun provider-family--gemini-setup (account &key executor state invocation credentials)
  "Run shared Code Assist setup under the parent transport and account lock."
  (bordeaux-threads:with-lock-held ((provider-account-lock account))
    (unless (getf (provider-account-state account) :setup-complete-p)
      (let ((endpoint (string-right-trim "/" (provider-account-endpoint account)))
            (clock    (chatgpt-executor-clock executor)))
        (multiple-value-bind (project tier)
            (family-domain-gemini-setup
             :project   (getf (provider-account-options account) :project)
             :rpc       (lambda (method stage request)
                          (declare (ignore stage))
                          (provider-family--rpc
                           account :executor executor :state state :invocation invocation
                           :credentials credentials :endpoint (format nil "~A:~A" endpoint method)
                           :request request))
             :operation (lambda (name)
                          (provider-family--rpc
                           account :executor executor :state state :invocation invocation
                           :credentials credentials :endpoint (format nil "~A/~A" endpoint name)
                           :method ':get))
             :wait      (lambda (seconds) (provider-http--sleep state seconds clock))
             :check     (lambda () (provider-http--check-invocation state invocation clock))
             :fail      (lambda (reason &key stage project response)
                          (declare (ignore stage project response))
                          (error 'provider-executor-error :reason reason)))
          (setf (provider-account-state account)
                (list :project project :tier tier :setup-complete-p t))))))
  nil)

(declaim (ftype (function (provider-account cl-llm-provider-api:wire-request
                           &key (:executor chatgpt-executor) (:state provider-http-state)
                           (:invocation model-invocation) (:credentials cl-rfc8628:oauth-credentials))
                         (values provider-account cl-llm-provider-api:wire-request (or null string) &optional))
                provider-family--copilot-auto))

(defun provider-family--copilot-auto
       (account projection &key executor state invocation credentials)
  "Resolve Auto with the shared entitlement selector and keep its token request-local."
  (let* ((base  (if (getf (provider-account-options account) :fixed-endpoint-p)
                    (provider-account-endpoint account)
                    (copilot--base-url credentials)))
         (check (lambda ()
                  (provider-http--check-invocation state invocation
                                                   (chatgpt-executor-clock executor))))
         (fail  (lambda (reason) (error 'provider-executor-error :reason reason)))
         (session
           (family-domain-copilot-session
            :acquire (lambda ()
                       (provider-family--rpc
                        account :executor executor :state state :invocation invocation
                        :credentials credentials :endpoint (concatenate 'string base "/models/session")
                        :request (cl-llm-provider-api:copilot-auto-session-request)))
            :now     (lambda () (- (get-universal-time) 2208988800))
            :check check :fail fail)))
    (multiple-value-bind (spec token)
        (family-domain-copilot-model
         :session         session
         :catalog         (lambda () (getf (provider-account-options account) :catalog))
         :refresh         (lambda ()
                            (let ((refresh (getf (provider-account-options account) :catalog-refresh)))
                              (if refresh
                                  (funcall refresh)
                                  (provider-family--rpc
                                   account :executor executor :state state :invocation invocation
                                   :credentials credentials :endpoint (concatenate 'string base "/models")
                                   :method ':get))))
         :vision-p        (cl-llm-provider-api:copilot-vision-request-p
                           (cl-llm-provider-api:wire-request-items projection))
         :preferred-model (getf (provider-account-state account) :preferred-model)
         :check check :fail fail)
      (let* ((protocol
              (case (getf spec :protocol)
                (:messages
                 ':anthropic)
                (otherwise
                 (getf spec :protocol))))
             (replacement
              (make-provider-account :family ':copilot :protocol protocol :credential-manager
               (provider-account-credential-manager account) :endpoint
               (if (getf (provider-account-options account) :fixed-endpoint-p)
                   (concatenate 'string base
                                (cl-llm-provider-api:copilot-protocol-endpoint
                                 (getf spec :protocol)))
                   base)
               :configuration (provider-account-configuration account) :headers
               (provider-account-headers account) :options (provider-account-options account)))
             (options (copy-tree (cl-llm-provider-api:wire-request-options projection))))
        (remf options :reasoning-effort)
        (remf options :reasoning-summary)
        (setf (provider-account-state account) (list :preferred-model (getf spec :name)))
        (values replacement
                (make-instance 'cl-llm-provider-api:wire-request :model (getf spec :name) :items
                               (cl-llm-provider-api:wire-request-items projection) :prefix
                               (cl-llm-provider-api:wire-request-prefix projection) :suffix
                               (cl-llm-provider-api:wire-request-suffix projection) :options
                               options)
                token)))))

(defmethod model-service-executor-failure-code
    ((condition provider-executor-error) (invocation model-invocation))
  "Permit summary fallback only for an explicit unavailable native operation."
  (if (and (eq (fourth (model-invocation-inputs invocation)) ':native-compaction)
           (eq (provider-executor-error-reason condition) ':native-compaction-unavailable))
      ':unsupported
      (call-next-method)))

(declaim (ftype (function (provider-account &key (:executor chatgpt-executor) (:state provider-http-state)
                           (:invocation model-invocation) (:credentials cl-rfc8628:oauth-credentials)
                           (:projection cl-llm-provider-api:wire-request)) cl-llm-provider-api:provider-result)
                provider-family--native-compact))

(defun provider-family--native-compact
       (account &key executor state invocation credentials projection)
  "Run Codex compact and retain the newest canonical opaque checkpoint."
  (unless (eq (provider-account-family account) ':codex)
    (error 'provider-executor-error :reason ':unsupported))
  (let* ((provider (provider-family--wire-provider account))
         (request
          (provider-family--request account :provider provider :projection projection :tools #()
           :session-id (chatgpt-executor-session-id executor)))
         (endpoint (string-right-trim "/" (provider-account-endpoint account))))
    (dolist
        (key '("stream" "store" "tools" "tool_choice" "reasoning" "include" "max_output_tokens"))
      (remhash key request))
    (let* ((response
            (handler-case
             (provider-family--rpc account :executor executor :state state :invocation invocation
              :credentials credentials :endpoint
              (if (uiop/utility:string-suffix-p endpoint "/responses/compact")
                  endpoint
                  (concatenate 'string endpoint "/compact"))
              :request request)
             (cl-llm-provider-api:provider-error (condition)
              (if (member (cl-llm-provider-api:provider-error-status condition) '(404 405 501))
                  (error 'provider-executor-error :reason ':native-compaction-unavailable)
                  (error condition)))))
           (output (gethash "output" response)))
      (unless (and (vectorp output) (not (stringp output)) (every #'hash-table-p output))
        (error 'provider-executor-error :reason ':invalid-native-compaction))
      (let* ((items
              (loop for item across output
                    for normalized = (clinker-transcript:native-compaction-item-canonicalize
                                      (cl-llm-provider-api:provider-normalize-output-item provider
                                                                                          item))
                    when (clinker-transcript:native-compaction-item-p normalized)
                    collect normalized))
             (latest (first (last items))))
        (unless latest (error 'provider-executor-error :reason ':native-compaction-unavailable))
        (make-instance 'cl-llm-provider-api:provider-result :output-items (list latest) :usage
                       (let ((usage (gethash "usage" response)))
                         (and (hash-table-p usage) usage))
                       :turn-completion ':unspecified)))))

(defmethod provider-executor-attempt
           ((account provider-account) &key executor state invocation credentials callback)
  "Use one family encoder and the shared request engine for an admitted attempt."
  (destructuring-bind
      (projection tools profile kind summary-p continuation turn-state)
      (model-invocation-inputs invocation)
    (unless
        (or (eq (provider-account-protocol account) ':auto)
            (eq (provider-account-protocol account) (model-profile-family profile)))
      (error 'provider-executor-error :reason ':account-profile-mismatch))
    (let ((*provider-account-active-configuration* (provider-account-configuration account))
          (*provider-family-session-token*         nil))
      (when
          (and (eq kind ':native-compaction) (not (eq (provider-account-family account) ':codex)))
        (error 'provider-executor-error :reason ':unsupported))
      (when (eq (provider-account-family account) ':gemini)
        (provider-family--gemini-setup account :executor executor :state state :invocation
         invocation :credentials credentials))
      (when (eq (provider-account-protocol account) ':auto)
        (multiple-value-setq (account projection *provider-family-session-token*)
          (provider-family--copilot-auto account projection :executor executor :state state
           :invocation invocation :credentials credentials)))
      (let* ((*provider-family-account*         account)
             (*provider-executor-wire-provider* (provider-family--wire-provider account))
             (*provider-executor-additional-secrets*
              (when *provider-family-session-token* (list *provider-family-session-token*))))
        (case kind
          (:native-compaction
           (provider-family--native-compact account :executor executor :state state :invocation
            invocation :credentials credentials :projection projection))
          (:generate
           (let ((request
                  (provider-family--request account :provider *provider-executor-wire-provider*
                   :projection projection :tools tools :summary-p summary-p :session-id
                   (chatgpt-executor-session-id executor) :continuation continuation)))
             (let ((result
                    (chatgpt-executor--request :executor executor :state state :invocation
                     invocation :account account :request request :credentials credentials
                     :turn-state turn-state :callback callback)))
               (when
                   (and (eq (provider-account-family account) ':grok) (not summary-p)
                        (null (cl-llm-provider-api:provider-result-tool-calls result))
                        (notany
                         (lambda (item)
                           (and (hash-table-p item) (equal (gethash "type" item) "message")))
                         (cl-llm-provider-api:provider-result-output-items result)))
                 (error 'cl-llm-provider-api:provider-retryable-error :message
                        "Grok completed a response with no message or tool call."))
               result)))
          (otherwise
           (error 'provider-executor-error :reason ':unsupported)))))))(declaim (ftype (function (&key (:accounts hash-table) (:session-id string) (:user-agent string)
                                                                                                  (:maximum-attempts (integer 1)) (:opener function) (:closer function)
                                                                                                  (:clock function) (:delay-function (or null function)))
                                                                                        (values function function &optional)) make-provider-family-model-executor))

(defun make-provider-family-model-executor
       (
        &key accounts session-id (user-agent "autolith") (maximum-attempts 8)
        (opener #'provider-family-open-response-stream) (closer #'provider-http--close-stream)
        (clock #'session-runtime--clock) delay-function)
  "Return the shared executor and strict abort function for trusted family accounts."
  (multiple-value-bind (execute abort)
      (make-chatgpt-model-executor :accounts accounts :session-id session-id :user-agent user-agent
       :maximum-attempts maximum-attempts :opener opener :closer closer :clock clock
       :delay-function delay-function)
    (values
     (lambda (invocation callback)
       (let ((*provider-family-grok-resamples-remaining* 2))
         (funcall execute invocation callback)))
     abort)))
