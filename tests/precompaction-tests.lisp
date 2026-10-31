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
  (dolist (case '((0 300 nil 0) (400 300 nil 1) (0 nil nil 1) (0 300 t 1)))
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
  "Prune resumed history before native or portable threshold compaction captures it."
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
        (precompaction-tests--exchange original "failed-earlier")
        (precompaction-tests--exchange original "succeeded" :success-p t)
        (conversation-append-user-message original "continue")
        (precompaction-tests--exchange original "failed-current")
        ;; Exercise publication independently of the estimator's tests.
        (let* ((conversation (conversation-load-by-id configuration "precompaction-full"))
               (agent        (agent-create :configuration configuration
                                           :provider      provider
                                           :conversation  conversation
                                           :tool-registry (agent-test-registry)
                                           :worker        nil)))
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
                         "compaction receives every failed and successful exchange"))))))
  nil)

(-> test-precompaction-retention-policy () null)
(defun test-precompaction-retention-policy ()
  "Use configured or provider retention for idle pruning; skip unknown retention."
  (dolist (case '((scripted-provider 0 10000 0)
                  (codex-subscription-provider 0 1000 0)
                  (codex-subscription-provider 1800 1000 0)
                  (codex-subscription-provider 1800 2000 0)
                  (scripted-provider 1800 2000 0)))
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
          (precompaction-tests--exchange conversation "failed")
          (conversation-append-user-message conversation "again")
          (setf (conversation-prompt-processed-at conversation) (- (get-universal-time) age))
          (test-assert (= expected (agent--prepare-provider-request agent))
                       "idle pruning follows the effective retention policy")))))
  nil)
