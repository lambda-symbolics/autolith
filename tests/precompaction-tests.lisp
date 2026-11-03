(in-package #:autolith)

;;;; -- Mechanical Precompaction Tests --

(-> precompaction-tests--call (string) json-object)
(defun precompaction-tests--call (call-id)
  "Return a completed shell call named CALL-ID."
  (json-object "type" "function_call" "status" "completed" "arguments" "{}"
               "call_id" call-id "name" "run" "namespace" "shell"))

(-> precompaction-tests--exchange (conversation string &key (:success-p boolean)) null)
(defun precompaction-tests--exchange (conversation call-id &key success-p)
  "Append a call named CALL-ID and its result, failed unless SUCCESS-P, to CONVERSATION."
  (conversation-append-provider-item conversation (precompaction-tests--call call-id))
  (conversation-append-tool-result conversation call-id
                                   :tool-name "shell.run"
                                   :output    (if success-p
                                                  "done"
                                                  (format nil "command failed: ~A" call-id))
                                   :success-p success-p)
  nil)

(-> precompaction-tests--call-ids (conversation) list)
(defun precompaction-tests--call-ids (conversation)
  "Return the call identifiers of CONVERSATION's projected calls, in order."
  (loop for item in (coerce (conversation-input-items conversation) 'list)
        when (clinker-transcript:function-call-item-p item)
          collect (json-get item "call_id")))

(-> test-precompaction-retains-failed-exchanges () null)
(defun test-precompaction-retains-failed-exchanges ()
  "Keep earlier failure evidence and its correlated reasoning across resume."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction"))
          (reasoning    (json-object "type" "reasoning" "encrypted_content" "opaque"
                                     "summary" (json-array))))
      (conversation-append-user-message conversation "start")
      (conversation-append-provider-item conversation reasoning)
      (precompaction-tests--exchange conversation "failed-early")
      (precompaction-tests--exchange conversation "succeeded" :success-p t)
      (conversation-append-provider-item conversation (precompaction-tests--call "steered"))
      (conversation-append-user-message conversation "next")
      (conversation-append-tool-result conversation "steered"
                                       :tool-name "shell.run" :output "denied"
                                       :success-p nil)
      (precompaction-tests--exchange conversation "failed-current")
      (let ((records (length (conversation--read-records
                              (conversation-pathname conversation)))))
          (test-assert (zerop (conversation-precompact conversation))
                       "distinct earlier failure evidence is retained")
          (test-assert (equal (precompaction-tests--call-ids conversation)
                              '("failed-early" "succeeded" "steered" "failed-current"))
                       "successful calls, split exchanges and distinct failures stay")
          (test-assert (member reasoning (coerce (conversation-input-items conversation) 'list))
                       "reasoning stays when its failed consequence stays")
          (test-assert (find "failed-early" (coerce (conversation-input-items conversation) 'list)
                             :key (lambda (item) (json-get item "call_id")) :test #'equal)
                       "the retained failure's output stays")
        (test-assert (= records (length (conversation--read-records
                                         (conversation-pathname conversation))))
                     "the durable log keeps every record")
        (let ((loaded (conversation-load-by-id configuration "precompaction")))
          (test-assert (member "failed-early" (precompaction-tests--call-ids loaded)
                               :test #'equal)
                       "loading alone keeps the durable projection")
          (test-assert (zerop (conversation-prepare-provider-request
                               loaded :identity (list ':codex "model-a")))
                       "a resumed request retains distinct failures")
          (test-assert (equal (precompaction-tests--call-ids loaded)
                              '("failed-early" "succeeded" "steered" "failed-current"))
                       "the resumed projection preserves failure evidence")))))
  nil)

(-> test-precompaction-cold-requests () null)
(defun test-precompaction-cold-requests ()
  "Precompact only when the provider or model changes or the cache has idled out."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction-cold"))
          (identity     (list ':codex "model-a")))
      (flet ((prepare (identity now)
               "Prepare a request served by IDENTITY at universal time NOW."
               (conversation-prepare-provider-request conversation
                                                      :identity identity :now now
                                                      :idle-seconds 300))

             (fail-turn (call-id)
               "Append a failed exchange followed by a new user message."
               (precompaction-tests--exchange conversation call-id)
               (conversation-append-user-message conversation "again")))
        (conversation-append-user-message conversation "start")
        (fail-turn "first")
        (test-assert (zerop (prepare identity 1000))
                     "the first request of a new conversation prunes nothing")
        (test-assert (zerop (prepare identity 1010))
                     "a warm request with the same provider prunes nothing")
        (test-assert (zerop (prepare (list ':codex "model-b") 1020))
                     "a model switch precompacts without deleting evidence")
        (fail-turn "second")
        (test-assert (zerop (prepare (list ':codex "model-b") 1030))
                     "a warm follow-up keeps newer failures")
        (test-assert (zerop (prepare (list ':codex "model-b") 1330))
                     "an idled-out cache precompacts without deleting evidence")
        (test-assert (equal '("first" "second")
                            (precompaction-tests--call-ids conversation))
                     "distinct earlier failures remain after cold preparation")
        (fail-turn "third")
        ;; The request at 1330 spent 400 seconds failing and retrying.
        (conversation-note-provider-response conversation :now 1730)
        (test-assert (zerop (prepare (list ':codex "model-b") 1740))
                     "a request completing after long retries leaves its cache warm"))))
  nil)

(-> test-precompaction-survives-resume () null)
(defun test-precompaction-survives-resume ()
  "Replay bounded output and preserve later evidence until a resumed cache is cold."
  (dolist (case '((0 300 nil 0) (400 300 nil 1) (0 nil nil 0) (0 300 t 1)))
    (destructuring-bind (age retention switch-p expected) case
      (with-test-configuration (configuration)
        (let* ((identity     (list ':codex "model-b"))
               (conversation (conversation-create configuration :identifier "precompaction-resume"))
               (large        (make-string 10000 :initial-element #\x)))
          (conversation-append-user-message conversation "start")
          (conversation-append-provider-item conversation (precompaction-tests--call "early"))
          (conversation-append-tool-result conversation "early" :tool-name "shell.run"
                                           :output large :success-p nil)
          (conversation-append-user-message conversation "next")
          (conversation-prepare-provider-request conversation :identity '(:codex "model-a"))
          (test-assert (= 1 (conversation-prepare-provider-request conversation :identity identity))
                       "a model switch records one bounded output")
          (conversation-append-provider-metadata
           conversation (list :request-number 1 :request-identity identity))
          (conversation-append-provider-item conversation (precompaction-tests--call "later"))
          (conversation-append-tool-result conversation "later" :tool-name "shell.run"
                                           :output large :success-p nil)
          (conversation-append-user-message conversation "again")
          (let* ((live   (mapcar #'json-encode (conversation-input-items conversation)))
                 (loaded (conversation-load-by-id configuration "precompaction-resume")))
            (test-assert (equal live (mapcar #'json-encode (conversation-input-items loaded)))
                         "loading replays the exact recorded bounded projection")
            (test-assert
             (= expected (conversation-prepare-provider-request
                          loaded :identity (if switch-p '(:codex "other") identity)
                          :now (+ (get-universal-time) age) :idle-seconds retention))
             "only a cold resumed prefix bounds new old output")
            (test-assert (equal '("early" "later") (precompaction-tests--call-ids loaded))
                         "every failed attempt survives cold preparation")
            (test-assert
             (equal (mapcar #'json-encode (conversation-input-items loaded))
                    (mapcar #'json-encode
                            (conversation-input-items
                             (conversation-load-by-id configuration "precompaction-resume"))))
             "the next resume reproduces the prepared projection"))))))
  nil)

(-> test-precompaction-bounds-repeated-failures () null)
(defun test-precompaction-bounds-repeated-failures ()
  "Bound identical failure evidence while retaining distinct failure effects."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction-repeated")))
      (conversation-append-user-message conversation "start")
      (dolist (call-id '("repeat-1" "repeat-2" "repeat-3"))
        (conversation-append-provider-item conversation (precompaction-tests--call call-id))
        (conversation-append-tool-result conversation call-id
                                         :tool-name "shell.run"
                                         :output    "test failed: same assertion"
                                         :success-p nil))
      (dolist (failure '("denied" "read-only" "unsuperseded-effect"))
        (conversation-append-provider-item conversation (precompaction-tests--call failure))
        (conversation-append-tool-result conversation failure
                                         :tool-name "shell.run"
                                         :output    (format nil "failure: ~A" failure)
                                         :success-p nil))
      (conversation-append-user-message conversation "next")
      (test-assert (zerop (conversation-precompact conversation))
                   "failed calls are never discarded by matching error text")
      (test-assert (equal '(
                            "repeat-1" "repeat-2" "repeat-3"
                            "denied" "read-only" "unsuperseded-effect")
                         (precompaction-tests--call-ids conversation))
                   "every failed attempt and distinct effect remains")
     nil)))

(-> test-precompaction-bounds-oversized-output () null)
(defun test-precompaction-bounds-oversized-output ()
  "Bound old output while retaining an exact durable reference and effects uncertainty."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "precompaction-large"))
          (output       (concatenate 'string "begin-evidence"
                                     (make-string 10000 :initial-element #\x)
                                     "end-evidence")))
      (conversation-append-user-message conversation "start")
      (conversation-append-provider-item conversation (precompaction-tests--call "large"))
      (conversation-append-tool-result conversation "large" :tool-name "shell.run"
                                       :output output :success-p nil)
      (conversation-append-user-message conversation "next")
      (test-assert (= 1 (conversation-precompact conversation))
                   "one oversized old output is bounded")
      (let* ((item    (find-if (lambda (item)
                                 (and (clinker-transcript:function-call-output-item-p item)
                                      (equal "large" (json-get item "call_id"))))
                               (coerce (conversation-input-items conversation) 'list)))
             (bounded (json-get item "output"))
             (records (conversation--read-records (conversation-pathname conversation)))
             (record  (find "large" records
                            :key  (lambda (record) (getf (rest record) :call-id))
                            :test #'equal)))
        (test-assert (<= (length bounded) *conversation-precompaction-output-maximum-characters*)
                     "projected output obeys the character limit")
        (test-assert (and (search "begin-evidence" bounded) (search "end-evidence" bounded))
                     "both ends of the evidence are retained")
        (test-assert (and (search "conversation:id/precompaction-large" bounded)
                          (search "large" bounded))
                     "the excerpt identifies the retrievable conversation and call")
        (test-assert (equal output (getf (rest record) :output))
                     "the durable result contains the full evidence")
        (test-assert (zerop (conversation-precompact conversation))
                     "an already bounded projection is not rewritten"))))
  nil)

(-> test-precompaction-before-compaction () null)
(defun test-precompaction-before-compaction ()
  "Compact a served, warm prefix under threshold pressure with native or portable summaries."
  (dolist (native-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((original (conversation-create configuration :identifier "precompaction-full"))
             (provider (make-instance
                        (if native-p 'native-scripted-provider 'scripted-provider)
                        :configuration configuration
                        :results       (list (agent-test-result "summary" (list (agent-test-compaction-message))
                                                                :turn-completion ':end)
                                            (mission-test--result "Done.")))))
        (when native-p
          (setf (native-scripted-provider-native-items provider)
                (list (json-object "type" "compaction" "encrypted_content" "checkpoint"))))
        (conversation-append-user-message original "start")
        (precompaction-tests--large-exchange original "failed-earlier" nil)
        (precompaction-tests--large-exchange original "succeeded" t)
        (conversation-append-user-message original "continue")
        (precompaction-tests--exchange original "failed-current")
        (conversation-append-provider-metadata
         original (list :request-number   1
                        :request-identity (list (provider-family provider) (config :model configuration))))
        ;; Force threshold pressure independently of token estimation.
        (let* ((conversation (conversation-load-by-id configuration "precompaction-full"))
               (agent        (agent-create :configuration configuration
                                           :provider      provider
                                           :conversation  conversation
                                           :tool-registry (agent-test-registry)
                                           :worker        nil)))
          (test-assert (zerop (agent--prepare-provider-request agent))
                       "a served unknown-lifetime prefix stays unbounded before threshold compaction")
          (test-call-with-function-replacements
           (list (list 'agent-should-compact-p
                       (lambda (agent)
                         (declare (ignore agent))
                         (null (scripted-provider-compaction-flags provider)))))
           (lambda () (agent-run-user-turn agent "next question")))
          (test-assert (equal '(t nil) (reverse (scripted-provider-compaction-flags provider)))
                       "threshold compaction precedes the ordinary request")
          (let* ((items (if native-p
                            (first (native-scripted-provider-native-input-snapshots provider))
                            (append (first (reverse (scripted-provider-input-snapshots provider)))
                                    (conversation-input-items conversation))))
                 (calls (loop for item in items
                              when (clinker-transcript:function-call-item-p item)
                                collect (json-get item "call_id"))))
            (test-assert (equal '("failed-earlier" "succeeded" "failed-current") calls)
                         "compaction receives every failed and successful exchange")
            (test-assert (find-if (lambda (item)
                                    (let ((output (json-get item "output")))
                                      (and (stringp output) (= 10000 (length output)))))
                                  items)
                         "threshold compaction receives the intact served oversized evidence")
            (test-assert (find (if native-p :native-compaction :summary)
                               (conversation--read-records (conversation-pathname conversation))
                               :key #'first)
                         "threshold compaction publishes a durable native or portable checkpoint"))))))
  nil)

(-> test-precompaction-retention-policy () null)
(defun test-precompaction-retention-policy ()
  "Use declared lifetimes for idle pruning; a scheduling estimate alone cannot expire history."
  (dolist (case '((scripted-provider 0 10000 0)
                  (codex-subscription-provider 0 1000 0)
                  (codex-subscription-provider 1800 1000 0)
                  (codex-subscription-provider 1800 2000 1)
                  (scripted-provider 1800 2000 1)))
    (destructuring-bind (provider-class override age expected) case
      (with-test-configuration (configuration)
        (setf (config :prompt-cache-retention-seconds configuration) override)
        (let* ((conversation (conversation-create configuration :identifier "retention"))
               (agent        (agent-create :configuration configuration
                                           :provider      (make-instance provider-class)
                                           :conversation  conversation
                                           :tool-registry (agent-test-registry)
                                           :worker        nil)))
          (conversation-append-user-message conversation "start")
          (agent--prepare-provider-request agent)
          (precompaction-tests--large-exchange conversation "failed" nil)
          (conversation-append-user-message conversation "again")
          (setf (conversation-prompt-processed-at conversation) (- (get-universal-time) age))
          (test-assert (= expected (agent--prepare-provider-request agent))
                       "idle pruning follows the effective retention policy")))))
  nil)

(-> precompaction-tests--large-exchange (conversation string boolean) null)
(defun precompaction-tests--large-exchange (conversation call-id success-p)
  "Append oversized durable evidence named CALL-ID with SUCCESS-P to CONVERSATION."
  (conversation-append-provider-item conversation (precompaction-tests--call call-id))
  (conversation-append-tool-result conversation call-id :tool-name "shell.run"
                                   :output                         (make-string 10000 :initial-element #\x)
                                   :success-p                      success-p)
  nil)

(-> test-precompaction-estimate-alone-keeps-served-output () null)
(defun test-precompaction-estimate-alone-keeps-served-output ()
  "Keep served successful and failed output despite an elapsed retention estimate."
  (dolist (age '(301 7200))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (agent        (agent-create :configuration configuration
                                         :provider      (make-instance 'codex-subscription-provider)
                                         :conversation  conversation
                                         :tool-registry (agent-test-registry) :worker nil)))
        (conversation-append-user-message conversation "start")
        (agent--prepare-provider-request agent)
        (precompaction-tests--large-exchange conversation "success" t)
        (precompaction-tests--large-exchange conversation "failure" nil)
        (conversation-append-user-message conversation "next")
        (agent--prepare-provider-request agent)
        (let ((items   (mapcar #'json-encode (conversation-input-items conversation)))
              (records (conversation--read-records (conversation-pathname conversation))))
          (setf (conversation-prompt-processed-at conversation) (- (get-universal-time) age))
          (test-assert (zerop (agent--prepare-provider-request agent))
                       "an estimate alone cannot bound two served outputs")
          (test-assert (equal items (mapcar #'json-encode (conversation-input-items conversation)))
                       "served successful and failed output stays byte-stable")
          (test-assert (equal records (conversation--read-records (conversation-pathname conversation)))
                       "estimate expiry writes no precompaction record")))))
  nil)

(-> test-precompaction-known-lifetime-expiry () null)
(defun test-precompaction-known-lifetime-expiry ()
  "Expire declared lifetimes while keeping unknown routes stable through real agent preparation."
  (dolist (case '((anthropic-api-key-provider "claude-sonnet-5" "5m" 0 300)
                  (anthropic-api-key-provider "claude-sonnet-5" "1h" 0 3600)
                  (openrouter-chat-completions-provider "openrouter/anthropic/claude-sonnet-4.5" "5m" 0 300)
                  (openrouter-chat-completions-provider "openrouter/anthropic/claude-sonnet-4.5" "1h" 0 3600)
                  (codex-subscription-provider "test" "5m" 1800 1800)
                  (openrouter-chat-completions-provider "openrouter/openai/gpt-test" "5m" 0 nil)
                  (scripted-provider "test" "5m" 0 nil)))
    (destructuring-bind (class model ttl override lifetime) case
      (with-test-configuration (configuration)
        (setf (configuration-validation-deferred-p configuration) t
              (config :model configuration) model
              (config :prompt-cache-ttl configuration) ttl
              (config :prompt-cache-retention-seconds configuration) override)
        (let* ((provider     (case class
                               (anthropic-api-key-provider
                                (anthropic-provider-create configuration))
                               (openrouter-chat-completions-provider
                                (openrouter-provider-create configuration))
                               (otherwise
                                (make-instance class))))
               (conversation (conversation-create configuration))
               (agent        (agent-create :configuration configuration :provider provider
                                           :conversation conversation
                                           :tool-registry (agent-test-registry) :worker nil)))
          (conversation-append-user-message conversation "start")
          (agent--prepare-provider-request agent)
          (precompaction-tests--large-exchange conversation "large" nil)
          (conversation-append-user-message conversation "next")
          (agent--prepare-provider-request agent)
          (let ((records (conversation--read-records (conversation-pathname conversation))))
            (setf (conversation-prompt-processed-at conversation)
                  (- (get-universal-time) (if lifetime (1- lifetime) 10000)))
            (test-assert (zerop (agent--prepare-provider-request agent))
                         "before declared expiry or without a lifetime the prefix is warm")
            (when lifetime
              (setf (conversation-prompt-processed-at conversation)
                    (- (get-universal-time) (1+ lifetime)))
              (test-assert (= 1 (agent--prepare-provider-request agent))
                           "declared lifetime expiry bounds one oversized output")
              (let* ((item   (find "large" (conversation-input-items conversation)
                                   :key  (lambda (item)
                                           (and (clinker-transcript:function-call-output-item-p item)
                                                (json-get item "call_id")))
                                   :test #'equal))
                     (output (json-get item "output")))
                (test-assert (and (< (length output) 10000) (search "large" output))
                             "bounded output includes its retrievable call reference"))
              (test-assert (equal records (butlast (conversation--read-records
                                                    (conversation-pathname conversation))))
                           "expiry appends a projection record without replacing durable evidence")))))))
  nil)

(-> test-precompaction-unknown-lifetime-resume () null)
(defun test-precompaction-unknown-lifetime-resume ()
  "Preserve same-identity served history without a lifetime, retaining cold resume controls."
  (dolist (record-identity-p '(t nil))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (identity     '(:scripted "test")))
        (conversation-append-user-message conversation "start")
        (precompaction-tests--large-exchange conversation "large" nil)
        (conversation-append-user-message conversation "next")
        (when record-identity-p
          (conversation-append-provider-metadata
           conversation (list :request-number 1 :request-identity identity)))
        (let* ((loaded  (conversation-load (conversation-pathname conversation)))
               (items   (mapcar #'json-encode (conversation-input-items loaded)))
               (records (conversation--read-records (conversation-pathname loaded))))
          (test-assert (= (if record-identity-p 0 1)
                          (conversation-prepare-provider-request loaded :identity identity
                                                                 :now             (+ (get-universal-time) 7200)))
                       "only a legacy resume without recorded identity is cold by default")
          (when record-identity-p
            (test-assert (equal items (mapcar #'json-encode (conversation-input-items loaded)))
                         "unknown-lifetime resume preserves served bytes")
            (test-assert (equal records (conversation--read-records (conversation-pathname loaded)))
                         "unknown-lifetime resume writes no projection record")
            (test-assert (= 1 (conversation-prepare-provider-request loaded :identity '(:scripted "other")))
                         "a provider or model change still bounds oversized history"))))))
  nil)
