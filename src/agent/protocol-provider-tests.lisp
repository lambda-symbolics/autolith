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

(-> run-agent-protocol-provider-tests () null)
(defun run-agent-protocol-provider-tests ()
  "Check the optional provider boundary and normalized replay contract."
  (agent-provider-test--portable)
  (agent-provider-test--runtime)
  (agent-provider-test--callback-abort)
  (agent-provider-test--callback-abort :acceptance-p t)
  (agent-provider-test--callback-abort :cooperative-p t)
  (agent-provider-test--callback-abort :cooperative-p t :acceptance-p t)
  nil)
