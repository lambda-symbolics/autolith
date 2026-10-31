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
                                   :output    (if success-p "done" "command failed")
                                   :success-p success-p)
  nil)

(-> precompaction-tests--call-ids (conversation) list)
(defun precompaction-tests--call-ids (conversation)
  "Return the call identifiers of CONVERSATION's projected calls, in order."
  (loop for item in (coerce (conversation-input-items conversation) 'list)
        when (clinker-transcript:function-call-item-p item)
          collect (json-get item "call_id")))

(-> test-precompaction-drops-failed-exchanges () null)
(defun test-precompaction-drops-failed-exchanges ()
  "Drop failed exchanges of earlier turns with their reasoning, and keep everything else."
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
        (test-assert (= 1 (conversation-precompact conversation))
                     "one earlier failed exchange is dropped")
        (test-assert (equal (precompaction-tests--call-ids conversation)
                            '("succeeded" "steered" "failed-current"))
                     "successful calls, split exchanges and the current turn's failures stay")
        (test-assert (not (member reasoning (coerce (conversation-input-items conversation) 'list)))
                     "reasoning that only led to a dropped call goes with it")
        (test-assert (not (find "failed-early" (coerce (conversation-input-items conversation) 'list)
                                :key (lambda (item) (json-get item "call_id")) :test #'equal))
                     "the dropped call's output goes with it")
        (test-assert (= records (length (conversation--read-records
                                         (conversation-pathname conversation))))
                     "the durable log keeps every record")
        (let ((loaded (conversation-load-by-id configuration "precompaction")))
          (test-assert (member "failed-early" (precompaction-tests--call-ids loaded)
                               :test #'equal)
                       "loading alone keeps the durable projection")
          (test-assert (= 1 (conversation-prepare-provider-request
                             loaded :identity (list ':codex "model-a")))
                       "a resume without a recorded response precompacts")
          (test-assert (equal (precompaction-tests--call-ids loaded)
                              '("succeeded" "steered" "failed-current"))
                       "the resumed projection matches the live one")))))
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
        (test-assert (= 1 (prepare (list ':codex "model-b") 1020))
                     "a model switch precompacts")
        (fail-turn "second")
        (test-assert (zerop (prepare (list ':codex "model-b") 1030))
                     "a warm follow-up keeps newer failures")
        (test-assert (= 1 (prepare (list ':codex "model-b") 1330))
                     "an idled-out cache precompacts")
        (test-assert (null (precompaction-tests--call-ids conversation))
                     "every earlier failed call is gone")
        (fail-turn "third")
        ;; The request at 1330 spent 400 seconds failing and retrying.
        (conversation-note-provider-response conversation :now 1730)
        (test-assert (zerop (prepare (list ':codex "model-b") 1740))
                     "a request completing after long retries leaves its cache warm"))))
  nil)

(-> precompaction-tests--resume-fixture (configuration list) list)
(defun precompaction-tests--resume-fixture (configuration identity)
  "Create a conversation pruned live and answered by IDENTITY; return its live call ids."
  (let ((conversation (conversation-create configuration :identifier "precompaction-resume"))
        (now          (get-universal-time)))
    (conversation-append-user-message conversation "start")
    (precompaction-tests--exchange conversation "failed-early")
    (precompaction-tests--exchange conversation "succeeded" :success-p t)
    (conversation-append-user-message conversation "next")
    (conversation-prepare-provider-request conversation
                                           :identity (list ':codex "model-a") :now now)
    (test-assert (= 1 (conversation-prepare-provider-request
                       conversation :identity identity :now now))
                 "a model switch prunes the live projection")
    (conversation-append-provider-metadata
     conversation (list :request-number 1 :request-identity identity))
    (precompaction-tests--exchange conversation "failed-later")
    (conversation-append-user-message conversation "again")
    (precompaction-tests--call-ids conversation)))

(-> test-precompaction-survives-resume () null)
(defun test-precompaction-survives-resume ()
  "Replay recorded pruning on load and prune a resumed conversation only when cold."
  (let ((identity (list ':codex "model-b")))
    (dolist (case `((,identity          0   300 ("succeeded" "failed-later")
                     "a prompt resume keeps the warm projection")
                    (,identity          400 300 ("succeeded")
                     "a resume after the retention estimate prunes")
                    (,identity          0   nil ("succeeded")
                     "a resume with unknown retention prunes")
                    ((:codex "model-a") 0   300 ("succeeded")
                     "a resume with another model prunes")))
      (destructuring-bind (request-identity age idle-seconds expected description) case
        (with-test-configuration (configuration)
          (let* ((live   (precompaction-tests--resume-fixture configuration identity))
                 (loaded (conversation-load-by-id configuration "precompaction-resume")))
            (test-assert (equal live '("succeeded" "failed-later"))
                         "later failures wait for the next cold request")
            (test-assert (equal live (precompaction-tests--call-ids loaded))
                         "loading replays recorded pruning")
            (test-assert (equal identity (first (conversation-last-response loaded)))
                         "loading restores the latest response's identity")
            (conversation-prepare-provider-request loaded
                                                   :identity     request-identity
                                                   :now          (+ (get-universal-time) age)
                                                   :idle-seconds idle-seconds)
            (test-assert (equal expected (precompaction-tests--call-ids loaded))
                         description)
            (test-assert (equal expected
                                (precompaction-tests--call-ids
                                 (conversation-load-by-id configuration "precompaction-resume")))
                         "pruning on resume is reproduced by the next load"))))))
  nil)

(-> test-precompaction-before-compaction () null)
(defun test-precompaction-before-compaction ()
  "Prune resumed history before native or portable threshold compaction captures it."
  (dolist (native-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((original (conversation-create configuration :identifier "precompaction-full"))
             (provider (make-instance
                        (if native-p 'native-scripted-provider 'scripted-provider)
                        :results (list (mission-test--result "Earlier work.")
                                       (mission-test--result "Done.")))))
        (when native-p
          (setf (native-scripted-provider-native-items provider)
                (list (json-object "type" "compaction" "encrypted_content" "checkpoint"))))
        (conversation-append-user-message original "start")
        (precompaction-tests--exchange original "failed-earlier")
        (precompaction-tests--exchange original "succeeded" :success-p t)
        (conversation-append-user-message original "continue")
        (precompaction-tests--exchange original "failed-current")
        (conversation-append-provider-metadata
         original (list :request-number 1 :usage '(("total_tokens" 999999))))
        (let* ((conversation (conversation-load-by-id configuration "precompaction-full"))
               (agent        (agent-create :configuration configuration
                                           :provider      provider
                                           :conversation  conversation
                                           :tool-registry (agent-test-registry)
                                           :worker        nil)))
          (agent-run-user-turn agent "next question")
          (test-assert (equal '(t nil) (reverse (scripted-provider-compaction-flags provider)))
                       "threshold compaction precedes the ordinary request")
          (let* ((items (first (if native-p
                                  (native-scripted-provider-native-input-snapshots provider)
                                  (reverse (scripted-provider-input-snapshots provider)))))
                 (calls (loop for item in items
                              when (clinker-transcript:function-call-item-p item)
                                collect (json-get item "call_id"))))
            (test-assert (equal '("succeeded" "failed-current") calls)
                         "compaction receives successful and current exchanges, not earlier failures"))
          (test-assert (equal (list (provider-family provider) (config :model configuration))
                              (first (conversation-last-response conversation)))
                       "completed responses record their request identity")))))
  nil)

(-> test-precompaction-retention-policy () null)
(defun test-precompaction-retention-policy ()
  "Use configured or provider retention for idle pruning; skip unknown retention."
  (dolist (case '((scripted-provider 0 10000 0)
                  (codex-subscription-provider 0 1000 1)
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
          (precompaction-tests--exchange conversation "failed")
          (conversation-append-user-message conversation "again")
          (setf (conversation-prompt-processed-at conversation) (- (get-universal-time) age))
          (test-assert (= expected (agent--prepare-provider-request agent))
                       "idle pruning follows the effective retention policy")))))
  nil)
