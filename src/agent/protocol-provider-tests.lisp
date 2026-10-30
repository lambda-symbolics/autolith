;;;; -- Parent Provider Adapter Checks --

(in-package #:autolith)

(-> agent-provider-test--outputs () list)
(defun agent-provider-test--outputs ()
  "Build public text, malformed function arguments, and discovery outputs."
  (list (json-object "type" "message" "role" "assistant" "content"
                     (vector (json-object "type" "output_text" "text" "accepted text")))
        (json-object "type" "function_call" "call_id" "call" "name" "run"
                     "namespace" "shell" "arguments" "{malformed")
        (json-object "type" "tool_search_call" "call_id" "search" "name" "tool_search"
                     "execution" "client" "arguments" "{\"query\":\"shell\"}")))

(-> agent-provider-test--runtime () null)
(defun agent-provider-test--runtime ()
  "Exercise the provider and authoritative replay through real serialized pipes."
  (uiop:with-temporary-file (:pathname session-path :stream session-file)
    (uiop:with-temporary-file (:pathname replay-path :stream replay-file)
      (close session-file)
      (close replay-file)
      (delete-file session-path)
      (delete-file replay-path)
      (unwind-protect
           (agent-protocol-test--peers
            (lambda (parent)
              (let ((welcome (agent-model-test--welcome)))
                (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) (* 1024 1024))
                (autolith-supervisor::parent-connection-handshake parent welcome))
              (let* ((store     (autolith-supervisor::session-store-open session-path "client-session"))
                     (replay    (autolith-supervisor::model-replay-store-open replay-path "client-session"))
                     (profile   (autolith-supervisor::make-model-profile
                                 :id "profile" :model "parent-model" :family ':responses
                                 :capabilities '(:deferred-tools :continuation :turn-state)))
                     (calls     0)
                     (originals (agent-provider-test--outputs))
                     (usage     (json-object "input_tokens" 7 "output_tokens" 3))
                     (metadata  (make-instance 'cl-llm-provider-api:provider-response-metadata :model "observed-model"
                                                :finish-reason "tool_calls" :usage-observed-p t))
                     (service
                       (autolith-supervisor::make-model-service
                        :profiles     (autolith-supervisor::make-model-profile-registry (list profile))
                        :replay-store replay
                        :abort        (lambda (invocation) (declare (ignore invocation)))
                        :executor
                        (lambda (invocation callback)
                          (incf calls)
                          (assert (= 23 (getf (cl-llm-provider-api:wire-request-options
                                               (first (autolith-supervisor::model-invocation-inputs invocation)))
                                              :maximum-output-tokens)))
                          (when (= calls 3)
                            (let* ((request (first (autolith-supervisor::model-invocation-inputs invocation)))
                                   (items   (cl-llm-provider-api:wire-request-items request)))
                              (assert (equalp originals items))))
                          (funcall callback (make-instance 'assistant-delta-event :text "stream text"))
                          (funcall callback (make-instance 'reasoning-delta-event :text "reasoning text"))
                          (dolist (item originals)
                            (funcall callback (make-instance 'provider-item-event :item item)))
                          (when (= calls 1)
                            (error 'provider-error :message "Private parent diagnostic."))
                          (make-instance 'provider-result :output-items originals
                                         :tool-calls                    (list (second originals))
                                         :response-id "response" :usage usage :metadata metadata
                                         :turn-state "private-routing" :turn-completion ':continue))))
                     (runtime   (autolith-supervisor::session-runtime-open
                                 store parent :features '("session.v1" "model.v1" "model-items-v1")
                                 :model-service         service)))
                (unwind-protect
                     (progn
                       (autolith-supervisor::session-runtime-start-turn runtime "turn" "input")
                       (dotimes (index 5)
                         (autolith-supervisor::session-runtime-receive runtime))
                       (autolith-supervisor::session-runtime--operation
                        service runtime (lambda () (assert (= calls 3))))
                       (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 5000)
                       (autolith-supervisor::session-runtime-receive runtime))
                  (autolith-supervisor::session-runtime-disconnect runtime)
                  (autolith-supervisor::session-runtime-await-close runtime :timeout 5))))
            (lambda (client)
              (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
              (agent-protocol-receive client)
              (let* ((*context-contributors*           nil)
                     (*context-request-contributions*
                       (list (make-context-contribution :identifier "once" :instruction "Use once."
                                                        :lifetime ':next-request)))
                     (*context-resolver*               (cl-llm-provider-api:make-context-resolver))
                     (*provider-maximum-output-tokens* 23)
                     (configuration                    (make-configuration))
                     (conversation                     (make-instance 'conversation :identifier "client-session"))
                     (driver                           (agent-protocol-driver-start client))
                     (provider                         (make-agent-protocol-provider configuration driver "profile"
                                                                                    :max-output-tokens 1024))
                     (events                           nil))
                (unwind-protect
                     (progn
                       (assert (handler-case
                                   (progn (provider-stream-turn provider conversation
                                                                :tool-namespaces #()
                                                                :event-callback  (lambda (event) (push event events)))
                                          nil)
                                 (provider-error (condition)
                                   (assert (not (search "Private parent" (princ-to-string condition))))
                                   t)))
                       (assert (notany (lambda (event) (typep event 'provider-item-event)) events))
                       (assert (context-delivery-contributions
                                (agent-context-projection-delivery
                                 (agent-context-project configuration conversation #()
                                                        :profile-id "profile" :turn-id "turn"))))
                       (setf events nil)
                      (let* ((result (provider-stream-turn provider conversation
                                                          :tool-namespaces #()
                                                          :event-callback  (lambda (event) (push event events))))
                             (items  (provider-result-output-items result)))
                        (assert (not (typep provider 'subscription-provider)))
                        (assert (string= "accepted text" (provider-result-assistant-text result)))
                        (assert (= 1 (length (provider-result-tool-calls result))))
                        (assert (= 1 (length (provider-result-tool-search-calls result))))
                        (assert (string= "{malformed" (json-get (second items) "arguments")))
                        (assert (= 7 (json-get (provider-result-usage result) "input_tokens")))
                        (assert (string= "observed-model" (provider-response-metadata-model (provider-result-metadata result))))
                        (assert (eq ':continue (provider-result-turn-completion result)))
                        (assert (non-empty-string-p (provider-result-turn-state result)))
                        (assert (some (lambda (event) (typep event 'assistant-delta-event)) events))
                        (assert (some (lambda (event) (typep event 'reasoning-delta-event)) events))
                        (assert (some (lambda (event) (typep event 'provider-progress-event)) events))
                        (assert (= 3 (count-if (lambda (event) (typep event 'provider-item-event)) events)))
                        ;; The agent repairs malformed arguments for persistence. Replay must
                        ;; retain the original parent item, including its malformed string.
                        (setf (gethash "arguments" (second items)) "{}"
                              (gethash "content" (first items)) #()
                              (conversation-input-items conversation) items)
                        (dolist (item items)
                          (setf (gethash item (conversation-input-item-families conversation)) ':parent))
                        (let ((again (provider-stream-turn provider conversation :tool-namespaces #()
                                                           :event-callback                        (constantly nil))))
                          (assert (string= "accepted text" (provider-result-assistant-text again))))
                        (let ((request (agent-protocol-driver-request driver #'agent-protocol-session-read)))
                          (multiple-value-bind (envelope terminal) (agent-protocol-driver-next request)
                            (assert terminal)
                            (assert (= 3 (length (agent-wire:session-page.records (agent-wire:envelope.session-page envelope)))))))
                        (agent-protocol-finish-turn client ':completed)
                        ;; The driver owns lifecycle reads. Observe state set by that reader.
                        (loop repeat 500 until (agent-protocol-client-stop client) do (sleep 0.01))
                        (assert (agent-protocol-client-stop client))
                        (agent-protocol-ready-to-stop client)))
                  (agent-protocol-driver-close driver)
                  (agent-protocol-driver-join driver :timeout 5)))))
        (dolist (path (list session-path replay-path))
          (when (probe-file path) (delete-file path))
          (let ((lock (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file lock) (delete-file lock)))))))
  nil)

(-> agent-provider-test--callback-abort (&key (:cooperative-p boolean) (:acceptance-p boolean)) null)
(defun agent-provider-test--callback-abort (&key cooperative-p acceptance-p)
  "Close a request on callback exit and join the parent executor before journal cleanup."
  (uiop:with-temporary-file (:pathname session-path :stream session-file)
    (uiop:with-temporary-file (:pathname replay-path :stream replay-file)
      (close session-file)
      (close replay-file)
      (delete-file session-path)
      (delete-file replay-path)
      (unwind-protect
           (agent-protocol-test--peers
            (lambda (parent)
              (let ((welcome (agent-model-test--welcome)))
                (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) (* 1024 1024))
                (autolith-supervisor::parent-connection-handshake parent welcome))
              (let* ((store   (autolith-supervisor::session-store-open session-path "client-session"))
                     (replay  (autolith-supervisor::model-replay-store-open replay-path "client-session"))
                     (gate    (sb-thread:make-semaphore))
                     (aborts  0)
                     (calls   0)
                     (profile (autolith-supervisor::make-model-profile
                               :id "profile" :model "parent-model" :family ':responses))
                     (service
                       (autolith-supervisor::make-model-service
                        :profiles     (autolith-supervisor::make-model-profile-registry (list profile))
                        :replay-store replay
                        :abort        (lambda (invocation)
                                        (declare (ignore invocation))
                                        (incf aborts)
                                        (sb-thread:signal-semaphore gate))
                        :executor     (lambda (invocation callback)
                                        (declare (ignore invocation))
                                        (incf calls)
                                        (funcall callback (make-instance 'assistant-delta-event :text "abort here"))
                                        (unless acceptance-p
                                          (assert (sb-thread:wait-on-semaphore gate :timeout 5)))
                                        (make-instance 'provider-result :output-items (agent-provider-test--outputs)))))
                     (runtime (autolith-supervisor::session-runtime-open
                               store parent :features '("session.v1" "model.v1" "model-items-v1")
                               :model-service         service)))
                (unwind-protect
                     (progn
                       (autolith-supervisor::session-runtime-start-turn runtime "turn" "input")
                       (if cooperative-p
                           (progn
                             (dotimes (index 4)
                               (autolith-supervisor::session-runtime-receive runtime))
                             (autolith-supervisor::session-runtime--operation
                              service runtime
                              (lambda ()
                                (assert (null (autolith-supervisor::session-runtime-turn-id runtime)))))
                             (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 5000)
                             (autolith-supervisor::session-runtime-receive runtime))
                           (handler-case
                               (loop (autolith-supervisor::session-runtime-receive runtime))
                             (autolith-supervisor::parent-protocol-error () nil))))
                  (autolith-supervisor::session-runtime-disconnect runtime)
                  (autolith-supervisor::session-runtime-await-close runtime :timeout 5))
                (assert (= 1 calls))
                (assert (plusp aborts))))
            (lambda (client)
              (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
              (agent-protocol-receive client)
              (let* ((*context-contributors* nil)
                     (*context-resolver*     (cl-llm-provider-api:make-context-resolver))
                     (*context-request-contributions*
                       (list (make-context-contribution :identifier "once" :instruction "Keep receipt."
                                                        :lifetime ':next-request)))
                     (cancelled              nil)
                     (*agent-protocol-provider-cancel-predicate*
                       (when cooperative-p (lambda () cancelled)))
                     (events                 nil)
                     (configuration          (make-configuration))
                     (conversation           (make-instance 'conversation :identifier "client-session"))
                     (driver                 (agent-protocol-driver-start client))
                     (provider               (make-agent-protocol-provider configuration driver "profile")))
                (unwind-protect
                     (progn
                       (assert
                        (eq (if cooperative-p ':cooperatively-cancelled ':callback-aborted)
                            (handler-case
                                (catch 'callback-abort
                                  (provider-stream-turn
                                   provider conversation :tool-namespaces #()
                                   :event-callback
                                   (lambda (event)
                                     (push event events)
                                     (when (typep event 'assistant-delta-event)
                                      (when acceptance-p
                                        ;; Wait for validation before either callback exit.
                                        ;; The sole reader queues completion while we wait.
                                        (loop repeat 500
                                              until (bordeaux-threads:with-lock-held
                                                        ((agent-protocol-client-state-lock client))
                                                      (plusp (hash-table-count (agent-protocol-client-model-terminals client))))
                                              do (sleep 0.01))
                                        (assert (plusp (hash-table-count (agent-protocol-client-model-terminals client)))))
                                      (if cooperative-p
                                          (if acceptance-p
                                              (setf cancelled t)
                                              (error 'application-turn-cancelled))
                                          (throw 'callback-abort ':callback-aborted))))))
                              (application-turn-cancelled () ':cooperatively-cancelled))))
                       (assert (notany (lambda (event) (typep event 'provider-item-event)) events))
                       (assert (notany (lambda (event) (typep event 'provider-completed-event)) events))
                       (assert (eq acceptance-p
                                   (not (null (gethash conversation (agent-protocol-provider-states provider))))))
                       (assert (eq (not acceptance-p)
                                   (not (null (context-delivery-contributions
                                               (agent-context-projection-delivery
                                                (agent-context-project configuration conversation #()
                                                                       :profile-id "profile" :turn-id "turn")))))))
                       (when cooperative-p
                         (assert (null (agent-protocol-driver-failure driver)))
                         (assert (zerop (hash-table-count (agent-protocol-driver-requests driver))))
                         (let ((read (agent-protocol-driver-request driver #'agent-protocol-session-read)))
                           (multiple-value-bind (envelope terminal) (agent-protocol-driver-next read)
                             (assert (agent-wire:envelope.has-session-page envelope))
                             (assert terminal)))
                         (agent-protocol-finish-turn client ':cancelled)
                         (loop repeat 500 until (agent-protocol-client-stop client) do (sleep 0.01))
                         (assert (agent-protocol-client-stop client))
                         (agent-protocol-ready-to-stop client)))
                  (agent-protocol-driver-close driver)
                  (agent-protocol-driver-join driver :timeout 5)))))
        (dolist (path (list session-path replay-path))
          (when (probe-file path) (delete-file path))
          (let ((lock (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file lock) (delete-file lock)))))))
  nil)

(-> agent-provider-test--portable () null)
(defun agent-provider-test--portable ()
  "Preserve optional presence and normalize stored parent item maps."
  (let* ((empty (agent-protocol-provider--metadata (agent-wire:make-response-metadata)))
         (zero  (agent-protocol-provider--metadata (agent-wire:make-response-metadata :cache-write-tokens 0)))
         (usage (agent-protocol-provider--usage
                 (agent-wire:make-usage :input-tokens 0 :cached-input-tokens 0 :cache-write-tokens 3))))
    (assert (null (provider-response-metadata-model empty)))
    (assert (null (provider-response-metadata-finish-reason empty)))
    (assert (null (provider-response-metadata-cache-write-tokens empty)))
    (assert (= 0 (provider-response-metadata-cache-write-tokens zero)))
    (assert (= 0 (gethash "input_tokens" usage)))
    (assert (= 0 (gethash "cached_input_tokens" usage)))
    (assert (= 3 (gethash "cache_write_tokens" usage)))
    (assert (not (nth-value 1 (gethash "output_tokens" usage))))
    (assert (zerop (hash-table-count (agent-protocol-provider--usage (agent-wire:make-usage))))))
  (let* ((wire   (agent-wire:make-output-item
                  :item-index 0 :replay-handle "handle" :family "responses"
                  :tool-call (agent-wire:make-tool-call :call-id "call" :name "run"
                                                        :arguments-json "{malformed")))
         (data   (autolith-supervisor::protocol-message->data wire))
         (item   (agent-protocol-provider-item-from-data data))
         (stored (json-decode (json-encode item))))
    (assert (equalp item (agent-protocol-provider--item wire)))
    (setf (gethash "arguments" stored) "{}")
    (assert (string= "handle" (agent-wire:transcript-item.replay-handle
                               (agent-context-item->transcript stored))))
    (setf (char (gethash "replay_handle" (gethash "view" (gethash "parent_replay" item))) 0) #\X)
    (assert (string= "handle" (agent-wire:transcript-item.replay-handle
                               (agent-context-item->transcript item)))))
  nil)

(-> agent-provider-test--compaction (keyword) null)
(defun agent-provider-test--compaction (outcome)
  "Exercise native acceptance, private replay, durable marker reload, and summary fallback over bytes."
  (let* ((root           (merge-pathnames (format nil "parent-compaction-~A/" (make-identifier)) (uiop:temporary-directory)))
         (session-path   (merge-pathnames "session.sexp" root))
         (replay-path    (merge-pathnames "replay.sexp" root))
         (opaque         (make-string 13000 :initial-element #\Q))
         (profile-id     nil)
         (native-p       (eq outcome ':native))
         (expected-calls (if native-p 4 2)))
    (ensure-directories-exist session-path)
    (unwind-protect
        (agent-protocol-test--peers
         (lambda (parent)
           (let ((welcome (agent-model-test--welcome)))
             (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) (* 1024 1024))
             (autolith-supervisor::parent-connection-handshake parent welcome))
           (multiple-value-bind (profile account ignored)
               (autolith-supervisor::provider-family-test--binding
                  (if (eq outcome ':unsupported)
                    '(:openai-api "fixture" :responses)
                     '(:codex "fixture" :responses))
                  :kind ':native-compaction)
             (declare (ignore ignored))
             (setf profile-id (autolith-supervisor::model-profile-id profile))
             (let ((accounts (make-hash-table :test #'eq)) (calls 0) (opened nil))
               (setf (gethash profile accounts) account)
               (multiple-value-bind (execute abort)
                   (autolith-supervisor::make-provider-family-model-executor
                    :accounts accounts :session-id "compaction-session" :maximum-attempts 1
                    :opener
                    (lambda (endpoint request credentials &key &allow-other-keys)
                      (declare (ignore credentials))
                      (incf calls)
                      (when (and native-p (> calls 1))
                        (assert (find opaque (gethash "input" request)
                                      :key (lambda (item) (gethash "encrypted_content" item)) :test #'equal)))
                      (let ((stream
                              (make-string-input-stream
                               (if (search "/compact" endpoint)
                                   (json-encode
                                    (json-object "output"
                                      (if native-p
                                          (vector (json-object "type" "compaction" "encrypted_content" opaque))
                                          #())))
                                   (autolith-supervisor::provider-family-test--sse ':responses)))))
                        (push stream opened)
                        (values stream 200 nil))))
                 (let* ((store   (autolith-supervisor::session-store-open session-path "compaction-session"))
                        (replay  (autolith-supervisor::model-replay-store-open replay-path "compaction-session"))
                        (service
                          (autolith-supervisor::make-model-service
                           :profiles (autolith-supervisor::make-model-profile-registry (list profile))
                           :replay-store replay :abort abort
                           :executor (if (eq outcome ':nil)
                                         (lambda (invocation callback)
                                           (if (eq (fourth (autolith-supervisor::model-invocation-inputs invocation))
                                                   ':native-compaction)
                                               nil
                                               (funcall execute invocation callback)))
                                         execute)))
                        (runtime (autolith-supervisor::session-runtime-open
                                  store parent :features '("session.v1" "model.v1" "model-items-v1")
                                  :model-service         service)))
                   (unwind-protect
                       (progn
                         (autolith-supervisor::session-runtime-start-turn runtime "turn" "compact")
                         (dotimes (index expected-calls)
                           (autolith-supervisor::session-runtime-receive runtime))
                         (autolith-supervisor::session-runtime-receive runtime)
                         (assert (= calls (if (member outcome '(:nil :unsupported)) 1 expected-calls)))
                         (assert (every (lambda (stream) (not (open-stream-p stream))) opened)))
                     (autolith-supervisor::session-runtime-disconnect runtime)
                     (autolith-supervisor::session-runtime-await-close runtime :timeout 5)))))))
         (lambda (client)
           (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
           (agent-protocol-receive client)
           (let* ((*context-contributors*          nil)
                  (*context-request-contributions* nil)
                  (configuration                   (make-configuration :provider-validation-p nil :state-root root :data-root root :config-root root))
                  (conversation                    (conversation-create configuration :identifier "compaction-session"))
                  (driver                          (agent-protocol-driver-start client))
                  (provider                        (make-agent-protocol-provider configuration driver profile-id)))
             (unwind-protect
                 (progn
                   (conversation-append-user-message conversation "Earlier context.")
                   (let ((item (if (eq outcome ':nil)
                                   (progn
                                     (assert (handler-case
                                                 (progn (provider-native-compact-conversation provider conversation) nil)
                                               (provider-error () t)))
                                     nil)
                                   (provider-native-compact-conversation provider conversation))))
                     (assert (eq (not (null item)) native-p))
                     (when item
                       (assert (conversation-parent-compaction-item-p item))
                       (assert (not (gethash "encrypted_content" item)))
                       (assert (not (search opaque (json-encode item)))))
                     (let* ((view    (if item (conversation-compaction-summary-view conversation (list item) ':parent) conversation))
                            (summary (provider-stream-turn provider view :compaction-p t)))
                       (assert (equal (provider-result-assistant-text summary) "ok"))
                       (if item
                           (conversation-append-native-compaction conversation item :family ':parent :summary "Portable handoff.")
                           (conversation-append-summary conversation "Portable handoff.")))
                     (when item
                       (let* ((reloaded (conversation-load (conversation-pathname conversation)))
                              (items    (conversation-input-items-for-family reloaded ':parent)))
                         (assert (= (length items) 1))
                         (assert (conversation-parent-compaction-item-p (first items)))
                         (assert (= (length (conversation-input-items-for-family reloaded ':codex)) 1))
                         (assert (notany #'conversation-parent-compaction-item-p
                                         (conversation-input-items-for-family reloaded ':codex)))
                         (assert (not (search opaque (uiop:read-file-string (conversation-log-pathname reloaded)))))
                         (assert (equal (provider-result-assistant-text (provider-stream-turn provider reloaded)) "ok"))
                         (assert (conversation-parent-compaction-item-p
                                  (provider-native-compact-conversation provider reloaded))))))
                   (agent-protocol-finish-turn client ':completed))
               (agent-protocol-driver-close driver)
               (agent-protocol-driver-join driver :timeout 5)))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> agent-provider-test--reasoning-request
    (hash-table keyword &key (:index integer) (:supported-p boolean) (:limited-p boolean)) null)
(defun agent-provider-test--reasoning-request (request protocol &key index supported-p limited-p)
  "Check actual encoded generation options after serialized parent validation."
  (let* ((effort    (when supported-p
                     (nth (1- index) (if limited-p '(nil "low" nil nil nil "low" "low")
                                         '("high" "low" "high" nil nil "low" "low")))))
         (summary   (when (and supported-p (eq protocol ':responses))
                      (nth (1- index) (if limited-p '(nil "concise" nil nil nil nil)
                                          '("auto" "concise" "detailed" nil nil nil "none")))))
         (reasoning (gethash "reasoning" request)))
    (case protocol
      (:responses
       (assert (equal effort (and reasoning (gethash "effort" reasoning))))
       (assert (equal summary (and reasoning (gethash "summary" reasoning)))))
      (:chat-completions
       (assert (equal effort (gethash "reasoning_effort" request))))
      (otherwise
       (assert (not (gethash "reasoning_effort" request)))
       (assert (not reasoning)))))
  nil)

(-> agent-provider-test--reasoning (keyword &key (:supported-p boolean) (:limited-p boolean)) null)
(defun agent-provider-test--reasoning (protocol &key supported-p limited-p)
  "Exercise live defaults, overrides and summary policy through bytes and family HTTP encoding."
  (let* ((root         (merge-pathnames (format nil "parent-reasoning-~A/" (make-identifier)) (uiop:temporary-directory)))
         (session-path (merge-pathnames "session.sexp" root))
         (replay-path  (merge-pathnames "replay.sexp" root))
         (family       (ecase protocol
                         (:responses
                          ':codex)
                         (:chat-completions
                          ':mistral)
                         (:anthropic
                          ':anthropic)
                         (:gemini
                          ':gemini))))
    (ensure-directories-exist session-path)
    (unwind-protect
         (multiple-value-bind (unused account invocation)
             (autolith-supervisor::provider-family-test--binding
              (list family "fixture" protocol)
              :options (when (and supported-p (eq protocol ':chat-completions))
                         '(:wire-options (:reasoning-parameter "reasoning_effort"))))
           (declare (ignore unused invocation))
           (when (eq protocol ':gemini)
             (setf (autolith-supervisor::provider-account-state account)
                   '(:setup-complete-p t :project "fixture-project")))
           (let ((profile (autolith-supervisor::make-model-profile
                           :id "reasoning" :model "fixture" :family protocol
                           :options             (when (and supported-p (eq protocol ':chat-completions))
                                                  '(:reasoning-parameter "reasoning_effort"))
                           :reasoning-efforts   (when supported-p (if limited-p '("low") '("low" "high")))
                           :reasoning-summaries (if limited-p '("concise") '("auto" "concise" "detailed" "none")))))
             (agent-protocol-test--peers
              (lambda (parent)
                (let* ((welcome (agent-model-test--welcome))
                       (data    (json-object "profile_id" "reasoning" "model" "fixture"
                                             "workspace" (namestring (uiop:getcwd)) "reasoning_effort" "high"
                                             "settings" (json-object "reasoning-effort" "high" "reasoning-summary" "auto")
                                             "reasoning_capabilities"
                                             (autolith-supervisor::model-profile-reasoning-capabilities profile))))
                  (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) (* 1024 1024))
                  (setf (agent-wire:welcome.configuration welcome) (autolith-supervisor::protocol-data->map data))
                  (autolith-supervisor::parent-connection-handshake parent welcome))
                (let ((accounts (make-hash-table :test #'eq)) (calls 0))
                  (setf (gethash profile accounts) account)
                  (multiple-value-bind (execute abort)
                      (autolith-supervisor::make-provider-family-model-executor
                       :accounts accounts :session-id "reasoning-session" :maximum-attempts 1
                       :opener (lambda (endpoint request credentials &key &allow-other-keys)
                                 (declare (ignore endpoint credentials))
                                 (agent-provider-test--reasoning-request request protocol :index (incf calls)
                                                                        :supported-p supported-p :limited-p limited-p)
                                 (values (make-string-input-stream
                                          (autolith-supervisor::provider-family-test--sse protocol)) 200 nil)))
                    (let* ((store   (autolith-supervisor::session-store-open session-path "reasoning-session"))
                           (service (autolith-supervisor::make-model-service
                                     :profiles     (autolith-supervisor::make-model-profile-registry (list profile))
                                     :replay-store (autolith-supervisor::model-replay-store-open replay-path "reasoning-session")
                                     :executor execute :abort abort))
                           (runtime (autolith-supervisor::session-runtime-open
                                     store parent :features '("session.v1" "model.v1" "model-items-v1")
                                     :model-service         service)))
                      (unwind-protect
                           (progn
                             (autolith-supervisor::session-runtime-start-turn runtime "turn" "reason")
                             (dotimes (index 9) (autolith-supervisor::session-runtime-receive runtime))
                             (assert (= calls (if (or (not supported-p) (and limited-p (eq protocol ':responses))) 6 7))))
                        (autolith-supervisor::session-runtime-disconnect runtime)
                        (autolith-supervisor::session-runtime-await-close runtime :timeout 5))))))
              (lambda (client)
                (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
                (agent-protocol-receive client)
                (multiple-value-bind (configuration profile-id policy capabilities)
                    (agent-protocol-entry--configuration (agent-protocol-client-welcome client))
                  (declare (ignore policy))
                  (let* ((*context-contributors*          nil)
                         (*context-request-contributions* nil)
                         (driver                          (agent-protocol-driver-start client))
                         (conversation                    (make-instance 'conversation :identifier "reasoning-session"))
                         (provider                        (make-agent-protocol-provider configuration driver profile-id
                                                                                        :reasoning-capabilities capabilities)))
                    (unwind-protect
                         (progn
                           (provider-stream-turn provider conversation)
                           (agent-protocol-application-settings-apply
                            configuration (json-object "reasoning-effort" "low" "reasoning-summary" "concise"))
                           (provider-stream-turn provider conversation)
                           (let ((copy (make-configuration :provider-validation-p nil :reasoning-effort "high"
                                                           :reasoning-summary "detailed")))
                             (provider-stream-turn (provider-with-configuration provider copy) conversation))
                           (provider-stream-turn
                            (make-agent-protocol-provider configuration driver profile-id
                                                          :reasoning-capabilities capabilities
                                                          :reasoning-effort nil :reasoning-summary nil) conversation)
                           (provider-stream-turn
                            (make-agent-protocol-provider configuration driver profile-id
                                                          :reasoning-capabilities capabilities
                                                          :reasoning-effort       nil) conversation)
                           (provider-stream-turn provider conversation :compaction-p t)
                           (let ((override (make-agent-protocol-provider configuration driver profile-id
                                                                         :reasoning-capabilities capabilities
                                                                         :reasoning-effort       "low"
                                                                         :reasoning-summary      (when (eq protocol ':responses) "none"))))
                             (if (and supported-p (not (and limited-p (eq protocol ':responses))))
                                 (provider-stream-turn override conversation)
                                 (assert (handler-case (progn (provider-stream-turn override conversation) nil)
                                           (provider-error () t)))))
                           (assert (handler-case
                                       (progn
                                         (provider-stream-turn
                                          (make-agent-protocol-provider configuration driver profile-id
                                                                        :reasoning-effort "unlisted") conversation)
                                         nil)
                                     (provider-error () t)))
                           (agent-protocol-finish-turn client ':completed))
                      (agent-protocol-driver-close driver)
                      (agent-protocol-driver-join driver :timeout 5))))))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> run-agent-protocol-provider-tests () null)
(defun run-agent-protocol-provider-tests ()
  "Check the optional provider boundary and normalized replay contract."
  (agent-provider-test--portable)
  (agent-provider-test--runtime)
  (agent-provider-test--reasoning ':responses :supported-p t)
  (agent-provider-test--reasoning ':responses)
  (agent-provider-test--reasoning ':chat-completions :supported-p t)
  (agent-provider-test--reasoning ':responses :supported-p t :limited-p t)
  (agent-provider-test--reasoning ':chat-completions :supported-p t :limited-p t)
  (agent-provider-test--reasoning ':chat-completions)
  (agent-provider-test--reasoning ':anthropic)
  (agent-provider-test--reasoning ':gemini)
  (agent-provider-test--callback-abort)
  (agent-provider-test--callback-abort :acceptance-p t)
  (agent-provider-test--callback-abort :cooperative-p t)
  (agent-provider-test--callback-abort :cooperative-p t :acceptance-p t)
  (dolist (outcome '(:native :unavailable :unsupported :nil))
    (agent-provider-test--compaction outcome))
  nil)
