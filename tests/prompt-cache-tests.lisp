(in-package #:autolith)

;;;; -- Prompt-Cache Miss Tests --

(-> prompt-cache-tests--usage ((option integer) (option integer)) list)
(defun prompt-cache-tests--usage (input cached)
  "Return portable usage with optional INPUT and CACHED counters."
  (append (list (list "output_tokens" 10))
          (and input (list (list "input_tokens" input)))
          (and cached (list (list "cached_input_tokens" cached)))))

(-> test-prompt-cache-miss-detection () null)
(defun test-prompt-cache-miss-detection ()
  "Test anomalies measured on cached-prefix progression, their causes, and their lost reuse."
  (let ((baseline (make-instance 'prompt-cache-baseline
                                 :prompt-tokens 40000
                                 :cached-tokens 40000
                                 :completed-at  1000
                                 :model         "model-a")))
    (dolist (case '((:model-changed 1010 "model-b" 41000 0 40000 10)
                    (:idle 1400 "model-a" 41000 100 39900 400)
                    (:prompt-shrank 1010 "model-a" 8000 0 40000 10)
                    (:unknown 1010 "model-a" 41000 19999 20001 10)))
      (destructuring-bind (cause started-at model input cached lost idle) case
        (let ((miss (prompt-cache-miss-detect
                     baseline (prompt-cache-tests--usage input cached)
                     :started-at started-at :model model)))
          (test-assert
           (and miss
                 (eq (getf miss :kind) ':shrunk)
                (eq (getf miss :cause) cause)
                (= (getf miss :lost-tokens) lost)
                (= (getf miss :prompt-tokens) input)
                (= (getf miss :cached-tokens) cached)
                (eql (getf miss :idle-seconds) idle))
           (format nil "~A accompanies measured lost cache reads" cause)))))
    (dolist (case '((40000 nil 41000 39000 "mostly cached")
                    (40000 nil 41000 20000 "threshold boundary")
                    (40000 nil 41000 0 "unknown previous cache counter")
                    (40000 nil 41000 nil "missing current cache counter")
                    (40000 30000 nil 0 "missing current input counter")
                    (500 0 600 0 "below the stall minimum")))
      (destructuring-bind (previous previous-cached input cached description) case
        (test-assert
         (null (prompt-cache-miss-detect
                (make-instance 'prompt-cache-baseline
                               :prompt-tokens previous :cached-tokens previous-cached
                               :completed-at 1000 :model "model-a")
                (prompt-cache-tests--usage input cached)
                :started-at 1010 :model "model-a"))
         description)))
    (test-assert
     (null (prompt-cache-miss-detect nil (prompt-cache-tests--usage 41000 0)))
     "no baseline means no comparative report")
    (let ((miss (prompt-cache-miss-detect
                 (make-instance 'prompt-cache-baseline :prompt-tokens 40000 :cached-tokens 40000)
                 (prompt-cache-tests--usage 41000 0)
                 :started-at 1010 :model "model-a")))
      (test-assert
       (and miss (eq (getf miss :cause) ':resumed)
            (null (getf miss :idle-seconds)))
       "a resumed baseline reports session provenance, not cache expiry"))
    ;; Each case: previous cached reads, current input, current cached reads,
    ;; the expected anomaly kind, and the reuse it lost.
    (dolist (case '((63872 73000 63872 nil 0)
                    (63872 73000 63900 nil 0)
                    (63872 73000 64000 nil 0)
                    (63872 73000 70000 nil 0)
                    (63872 73000 60000 :shrunk 3872)
                    (63872 71000 63872 nil 0)
                    (69000 73000 69000 nil 0)
                    (69000 73000 68000 :shrunk 1000)
                    (72000 140000 72000 nil 0)
                    (nil 73000 63872 nil 0)
                    (1024 73000 1024 nil 0)
                    (0 73000 0 nil 0)
                    (1024 73000 20000 nil 0)))
      (destructuring-bind (previous-cached input cached kind lost) case
        (let ((miss (prompt-cache-miss-detect
                     (make-instance 'prompt-cache-baseline
                                    :prompt-tokens 72000 :cached-tokens previous-cached
                                    :completed-at 1000 :model "model-a")
                     (prompt-cache-tests--usage input cached)
                     :started-at 1010 :model "model-a")))
          (test-assert
           (if kind
               (and miss
                    (eq (getf miss :kind) kind)
                    (= (getf miss :lost-tokens) lost)
                    (eql (getf miss :previous-cached-tokens) previous-cached)
                    (eq (getf miss :cause) ':unknown))
               (null miss))
           (format nil "cache reads ~S yield ~S" case kind)))))
    (let ((miss (prompt-cache-miss-detect
                 baseline
                 (append (prompt-cache-tests--usage 41000 0)
                         (list
                          (list "attribution"
                                (list
                                 (list "request_fields"
                                       (list
                                        (list "instructions"
                                              (list (list "cached_tokens" 5627)
                                                    (list "input_tokens" 5738)))
                                        (list "tools"
                                              (list (list "cached_tokens" 0)
                                                    (list "input_tokens" 9221)))))))))
                 :started-at 1010
                 :model      "model-a")))
      (test-assert
       (and miss
             (= (getf miss :instructions-uncached) 111)
             (= (getf miss :tools-uncached) 9221))
       "server attribution reports uncached instructions and tools")))
  nil)

(-> test-prompt-cache-small-losses () null)
(defun test-prompt-cache-small-losses ()
  "Accumulate small cache losses without confusing a partial recovery with stagnation."
  (let ((baseline (prompt-cache-baseline-create (prompt-cache-tests--usage 100000 99000)))
        (reports  nil))
    (loop for request from 1 to 60
          for usage = (prompt-cache-tests--usage (+ 100000 (* 10 request))
                                                 (- 99000 (* 64 request)))
          do (let ((miss (prompt-cache-miss-detect baseline usage)))
               (when miss
                 (push (getf miss :lost-tokens) reports))
               (setf baseline (prompt-cache-baseline-advance
                               baseline usage :reported-p (not (null miss))))))
    (test-assert (= 3840 (reduce #'+ reports))
                 "sixty losses of 64 cached tokens report every lost token")
    (test-assert (every (lambda (lost) (>= lost *prompt-cache-block-tokens*)) reports)
                 "each report covers at least one cache block")
    (let ((grown (prompt-cache-baseline-advance
                  baseline (prompt-cache-tests--usage 101000 100000))))
      (test-assert (= 100000 (prompt-cache-baseline-cached-tokens grown))
                   "growing reads become the new reference")))
  (let ((baseline (prompt-cache-baseline-create (prompt-cache-tests--usage 120000 99000))))
    (loop for (input cached lost) in '((120000 98936 nil)
                                       (121000 98968 nil)
                                       (121000 98904 nil)
                                       (121000 98840 128))
          for usage = (prompt-cache-tests--usage input cached)
          do (let ((miss (prompt-cache-miss-detect baseline usage)))
               (test-assert
                (if lost
                    (and (eq (getf miss :kind) ':shrunk)
                         (= (getf miss :lost-tokens) lost))
                    (null miss))
                "partial recovery resets loss accumulation and is never reported as stalled")
               (setf baseline (prompt-cache-baseline-advance
                               baseline usage :reported-p (not (null miss)))))))
  (dolist (case '((98970 "recovery below the retained peak")
                  (99000 "recovery to the retained peak")))
    (destructuring-bind (recovered description) case
      (let* ((prefix   '(:header "stable" :items (("history" 400000))))
             (grown    '(:header "stable" :items (("history" 400000) ("exchange" 40000))))
             (baseline (prompt-cache-baseline-create
                        (prompt-cache-tests--usage 100000 99000) :prefix prefix))
             (declined (prompt-cache-tests--usage 108000 98936))
             (recovery (prompt-cache-tests--usage 109000 recovered)))
        (test-assert (null (prompt-cache-miss-detect baseline declined :prefix grown))
                     "a small decline stays quiet")
        (setf baseline (prompt-cache-baseline-advance baseline declined :prefix grown))
        (test-assert (eq ':stalled
                         (getf (prompt-cache-miss-detect baseline declined :prefix grown) :kind))
                     "the shared history growth is enough to detect a genuine plateau")
        (test-assert (null (prompt-cache-miss-detect baseline recovery :prefix grown))
                     (format nil "~A after a small decline is not a stall" description))
        (setf baseline (prompt-cache-baseline-advance baseline recovery :prefix grown))
        (let ((miss (prompt-cache-miss-detect
                     baseline (prompt-cache-tests--usage 109000 (- recovered 128))
                     :prefix grown)))
          (test-assert (and (eq (getf miss :kind) ':shrunk)
                            (= (getf miss :lost-tokens) 128))
                       "a later loss is measured from the recovered reads")))))
  nil)

(-> test-prompt-cache-baseline-from-conversation () null)
(defun test-prompt-cache-baseline-from-conversation ()
  "Test the baseline seeded from a conversation's newest persisted usage."
  (with-test-configuration (configuration)
    (let ((conversation
            (conversation-create configuration :identifier "cache-baseline")))
      (test-assert
       (null (prompt-cache-baseline-from-conversation conversation))
       "an empty conversation seeds no baseline")
      (conversation-append-user-message conversation "first")
      (conversation-append-provider-metadata
       conversation
       (list :request-number 1 :usage (prompt-cache-tests--usage 1500 0)))
      (conversation-append-provider-metadata
       conversation
       (list :request-number 2 :usage (prompt-cache-tests--usage 3000 1400)))
      (conversation-append-provider-metadata
       conversation
       (list :request-number 3 :usage nil))
      (let ((baseline (prompt-cache-baseline-from-conversation conversation)))
        (test-assert
         (and baseline
              (= (prompt-cache-baseline-prompt-tokens baseline) 3000)
              (null (prompt-cache-baseline-completed-at baseline))
              (null (prompt-cache-baseline-model baseline)))
         "the newest usage with a prompt size seeds an untimed baseline"))))
  nil)

(-> test-prompt-cache-prefix-progression () null)
(defun test-prompt-cache-prefix-progression ()
  "Exercise shared prefix measurements through request projection and application notices."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "cache-prefix"))
           (provider     (provider-create configuration))
           (terminal     (make-instance 'recording-terminal :columns 100))
           (ui           (terminal-ui-create :terminal terminal))
           (application  (make-instance 'application :configuration configuration
                                                    :conversation conversation :ui ui))
           (observer     (application-agent-observer application))
           (status       (callback-agent-observer-status-callback observer)))
      (unwind-protect
           (labels ((project (tail)
                      "Capture the prefix of the actual Responses request with temporary TAIL."
                      (let* ((prefix nil)
                             (*provider-cache-prefix-function*
                               (lambda (value) (setf prefix value))))
                        (provider-request-object provider conversation #() :goal-context tail)
                        prefix))

                    (exchange (number)
                      "Append an ordinary durable tool exchange."
                      (let ((identifier (format nil "call-~D" number)))
                        (conversation-append-provider-item
                         conversation (json-object "type" "function_call" "call_id" identifier
                                                   "name" "inspect" "arguments" "{}"))
                        (conversation-append-provider-item
                         conversation (json-object "type" "function_call_output"
                                                   "call_id" identifier
                                                   "output" (make-string 11200 :initial-element #\x)))))

                    (complete (input cached prefix)
                      "Complete a request carrying its usage and measured PREFIX."
                      (recording-terminal-reset terminal)
                      (funcall status :provider-request-started nil)
                      (funcall status :provider-request-completed
                               (list :usage        (prompt-cache-tests--usage input cached)
                                     :cache-prefix prefix)))

                    (noticed-p ()
                      "Return whether a cache notice was presented."
                      (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                        (not (null (or (search "cache miss" output)
                                       (search "cache stalled" output)))))))
             (terminal-ui-start ui)
             (setf (application-cache-miss-notices-p application) t)
             (conversation-append-user-message
              conversation (make-string 180000 :initial-element #\h))
             (let ((first (project (make-string 88000 :initial-element #\a))))
               (complete 67000 45000 first)
               (exchange 1)
               (let ((second (project (make-string 88000 :initial-element #\b))))
                 (test-assert
                  (provider-cache-prefix-extends-p first second)
                  "changing temporary context preserves the measured history prefix")
                 (complete 69800 45000 second)
                 (test-assert (not (noticed-p))
                              "45K cached history plus a new 2.8K exchange and 22K tail is healthy")
                 (test-assert
                  (equal second (prompt-cache-baseline-prefix
                                 (application-prompt-cache-baseline application)))
                  "the completion observer carries the projected prefix into the baseline")))
             (exchange 2)
             (complete 72600 45000 (project "temporary context changed again"))
             (test-assert (not (noticed-p)) "small shared growth stays below the stall heuristic")
             (exchange 3)
             (let* ((prefix   (project "short tail"))
                    (baseline (application-prompt-cache-baseline application))
                    (usage    (prompt-cache-tests--usage 75400 45000))
                    (miss     (prompt-cache-miss-detect baseline usage :prefix prefix)))
               (test-assert
                (and (eq (getf miss :kind) ':stalled)
                     (null (getf miss :lost-tokens))
                     (>= (getf miss :history-growth-characters)
                         *prompt-cache-stall-minimum-characters*))
                "a sustained plateau reports measured shared growth, not invented lost tokens")
               (test-assert
                (null (prompt-cache-miss-detect
                       baseline usage :prefix (list :header "changed tools" :items (getf prefix :items))))
                "a changed request header cannot establish reusable prefix growth")
               (test-assert
                (null (prompt-cache-miss-detect baseline (prompt-cache-tests--usage 75400 45028)
                                                :prefix prefix))
                "actual cache-read growth is progress")
               (complete 75400 45000 prefix)
               (test-assert (noticed-p) "sustained shared-history growth reaches the ledger")
               (let ((counts (remove-if-not
                              (lambda (span) (eq ':code (terminal-span-style span)))
                              (application--prompt-cache-miss-notice miss))))
                 (test-assert
                  (equal (mapcar #'terminal-span-text counts)
                         (mapcar #'application--token-count-description '(45000 75400)))
                  "stall notices present only measured cache and input token quantities")))
             (complete 100000 45000 (project (make-string 180000 :initial-element #\z)))
             (test-assert (not (noticed-p))
                          "a larger temporary tail cannot create another shared-growth notice"))
        (ignore-errors (terminal-ui-stop ui)))))
  nil)

(-> test-prompt-cache-request-observation () null)
(defun test-prompt-cache-request-observation ()
  "Carry the projected prefix through a real agent loop without provider transport."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "cache-observation"))
           (provider     (provider-create configuration))
           (agent        (agent-create :configuration configuration :provider provider
                                       :conversation conversation :worker ':unused))
           (completed    nil)
           (observer
             (callback-agent-observer-create
              :status-callback
              (lambda (status details)
                (when (eq status ':provider-request-completed)
                  (setf completed (getf details :cache-prefix)))))))
      (test-call-with-function-replacements
       (list
        (list 'provider-stream-turn
              (lambda (provider conversation &key tool-namespaces event-callback
                                                  goal-context compaction-p)
                (declare (ignore event-callback))
                (provider-request-object provider conversation tool-namespaces
                                         :goal-context goal-context :compaction-p compaction-p)
                (make-instance 'provider-result
                               :response-id     "cache-observation"
                               :output-items nil :tool-calls nil
                               :usage           (prompt-cache-tests--usage 1000 0)
                               :turn-completion ':end))))
       (lambda ()
         (agent-run-user-turn agent "observe this request" :tools-p nil :observer observer)))
      (test-assert
       (and completed (= (length (getf completed :items)) 1)
            (plusp (provider-cache-prefix-characters completed)))
       "agent completion carries the fingerprinted user history, excluding temporary context")
      (test-assert (and (getf (getf completed :wire) :items)
                        (assoc "model" (getf (getf completed :wire) :fields) :test #'string=))
                   "agent completion carries the plan of the final request")))
  nil)

(-> prompt-cache-tests--wire-request (&key (:tools string) (:texts list) (:ttl (option string)))
    json-object)
(defun prompt-cache-tests--wire-request (&key (tools "read") (texts '("a" "b" "c")) ttl)
  "Return a Messages-shaped request with TOOLS, one message per text and a marker on the last."
  (let ((messages (mapcar (lambda (text)
                            (json-object "role" "user"
                                         "content" (json-array
                                                    (json-object "type" "text" "text" text))))
                          texts)))
    (setf (gethash "cache_control"
                   (aref (json-get (first (last messages)) "content") 0))
          (if ttl
              (json-object "type" "ephemeral" "ttl" ttl)
              (json-object "type" "ephemeral")))
    (json-object "model"    "claude-test"
                 "tools"    (json-array (json-object "name" tools))
                 "messages" (coerce messages 'vector))))

(-> test-prompt-cache-wire-plan () null)
(defun test-prompt-cache-wire-plan ()
  "Name the final-request fields, first changed item and markers that differ."
  (let* ((base (provider-cache-wire-plan (prompt-cache-tests--wire-request)))
         (code-assist
           (provider-cache-wire-plan
            (json-object "project" "p" "request" (prompt-cache-tests--wire-request)))))
    (test-assert (and (equal (mapcar #'first (getf base :fields)) '("model" "tools"))
                      (= (length (getf base :items)) 3)
                      (equal (getf base :breakpoints) '((("messages" 2 "content" 0) nil))))
                 "a plan fingerprints fields, items and cache markers")
    (test-assert (equal base code-assist)
                 "a Code Assist envelope is measured through its inner request")
    (dolist (case '((()                              nil         nil nil "repeated requests")
                    ((:texts ("a" "b" "c" "d"))      nil         2   t   "appended items behind a moved marker")
                    ((:texts ("a" "x" "c"))          nil         1   nil "an edited item")
                    ((:texts ("a" "b"))              nil         1   t   "removed items")
                    ((:tools "write")                ("tools")   nil nil "changed tools")
                    ((:ttl "1h")                     nil         2   t   "a new marker lifetime")))
      (destructuring-bind (arguments fields first-changed moved-p description) case
        (let ((change (provider-cache-wire-change
                       base
                       (provider-cache-wire-plan
                        (apply #'prompt-cache-tests--wire-request arguments)))))
          (test-assert (and (equal (getf change :fields) fields)
                            (eql (getf change :first-changed-item) first-changed)
                            (eq (getf change :breakpoints-moved-p) moved-p))
                       (format nil "the change describes ~A" description)))))
    (test-assert (null (provider-cache-wire-change nil base))
                 "a request without a previous plan reports no change"))
  nil)

(-> test-prompt-cache-miss-notices () null)
(defun test-prompt-cache-miss-notices ()
  "Exercise compact colored cache-miss notices through the request lifecycle."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-ui"))
         (terminal      (make-instance 'recording-terminal :columns 40 :styled-p t))
         (ui            (terminal-ui-create :terminal terminal))
         (application   (make-instance 'application
                                       :configuration configuration
                                       :conversation  conversation
                                       :ui            ui)))
    (unwind-protect
         (let* ((observer (application-agent-observer application))
                (send-status
                  (callback-agent-observer-status-callback observer)))
           (labels ((notice-p ()
                     "Return whether the terminal painted a cache-miss notice."
                     (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                       (not (null (or (search "cache miss" output)
                                      (search "cache stalled" output))))))

                    (notice-counts-p (lost total)
                      "Return whether the rendered notice includes the lost and total token counts."
                      (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                        (and (notice-p)
                             (search (application--token-count-description lost) output)
                             (search (application--token-count-description total) output))))

                    (request (usage)
                      "Run one request lifecycle completing with USAGE."
                      (recording-terminal-reset terminal)
                      (funcall send-status :provider-request-started nil)
                      (funcall send-status
                               :provider-request-completed
                               (list :request-number 1 :usage usage))))
             (terminal-ui-start ui)
             (conversation-append-user-message conversation "history")
             (conversation-append-provider-metadata
              conversation
              (list :request-number 1
                    :usage          (prompt-cache-tests--usage 30000 0)))
             (request (prompt-cache-tests--usage 31000 0))
             (test-assert (not (notice-p))
                          "disabled notices are omitted")
             (test-assert
              (= (prompt-cache-baseline-prompt-tokens
                  (application-prompt-cache-baseline application))
                 31000)
              "every completed request advances the baseline")
             (setf (application-cache-miss-notices-p application) t)
             (request (prompt-cache-tests--usage 32000 31500))
             (test-assert (not (notice-p))
                          "cache hits are not reported")
             (setf (application-prompt-cache-baseline application)
                   (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 32000
                                  :cached-tokens 32000
                                  :completed-at  (- (get-universal-time) 900)
                                  :model         (config :model configuration)))
             (request (prompt-cache-tests--usage 33000 0))
             (test-assert (notice-counts-p 32000 33000)
                          "an idle miss reports measured lost cache reads")
             (let* ((spans  (application--prompt-cache-miss-notice
                              '(:kind :shrunk :lost-tokens 32000 :cached-tokens 0
                                :prompt-tokens 33000)))
                    (counts (remove-if-not
                             (lambda (span) (eq ':code (terminal-span-style span))) spans)))
               (test-assert (and (eq ':failure (terminal-span-style (first spans)))
                                 (string= "! " (terminal-span-text (first spans))))
                            "the cache-miss ledger entry begins with a red exclamation mark")
               (test-assert (equal (mapcar #'terminal-span-text counts)
                                   (mapcar #'application--token-count-description '(32000 0 33000)))
                            "the renderer reports lost reuse, cache reads and input"))
             (test-assert
              (equal (terminal-span-text
                      (first (last (application--prompt-cache-miss-notice
                                    '(:kind :shrunk :lost-tokens 32000 :cached-tokens 0
                                      :prompt-tokens 33000
                                      :wire-change (:fields ("tools") :first-changed-item 4
                                                    :items 9 :breakpoints-moved-p t))))))
                     " · request tools changed, item 5 of 9 changed, cache markers moved")
              "the notice names what changed in the request as sent")
             (setf (application-prompt-cache-baseline application)
                   (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 72000
                                  :cached-tokens 63872
                                  :completed-at  (get-universal-time)
                                  :model         (config :model configuration)))
             (request (append
                       (prompt-cache-tests--usage 73000 63872)
                       (list
                        (list "attribution"
                              (list
                               (list "request_fields"
                                     (list
                                      (list "instructions"
                                            (list (list "cached_tokens" 5738)
                                                  (list "input_tokens" 5738)))
                                      (list "tools"
                                            (list (list "cached_tokens" 9221)
                                                  (list "input_tokens" 9221))))))))))
             (test-assert (not (notice-p))
                          "full-input growth alone cannot establish a stalled reusable prefix")
             (funcall send-status :compaction-completed nil)
             (test-assert
              (null (application-prompt-cache-baseline application))
              "compaction drops the baseline its rewrite invalidated")
             (request (prompt-cache-tests--usage 9000 0))
             (test-assert (not (notice-p))
                          "the first request after compaction is expected")
             (let ((resumed
                     (conversation-create configuration
                                          :identifier "cache-miss-resumed")))
               (conversation-append-user-message resumed "older history")
               (conversation-append-provider-metadata
                resumed
                (list :request-number 1
                      :usage          (prompt-cache-tests--usage 20000 19000)))
               (setf (application-conversation application) resumed)
               (request (prompt-cache-tests--usage 21000 20000))
               (test-assert (not (notice-p))
                            "the first request after loading a conversation establishes a fresh baseline")
               (request (prompt-cache-tests--usage 22000 0))
               (test-assert (notice-counts-p 20000 22000)
                            "later misses compare against requests from this session")
               (setf (application-conversation application)
                     (conversation-load (conversation-pathname resumed)))
               (request (prompt-cache-tests--usage 23000 22000))
               (test-assert (not (notice-p))
                            "resuming the same conversation again starts a fresh cache window")
               (request (prompt-cache-tests--usage 24000 0))
               (test-assert (notice-counts-p 22000 24000)
                            "same-conversation resume suppresses only its first request"))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-cache-misses-command () null)
(defun test-cache-misses-command ()
  "Test the /cache-misses command persists and applies its preference."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-command"))
         (terminal      (make-instance 'recording-terminal :columns 100))
         (ui            (terminal-ui-create :terminal terminal))
         (application   (make-instance 'application
                                       :configuration configuration
                                       :conversation  conversation
                                       :ui            ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (application-cache-miss-notices-command application t)
           (test-assert
            (and (application-cache-miss-notices-p application)
                 (config :cache-miss-notices-p configuration))
            "/cache-misses on applies and persists the preference")
           (recording-terminal-reset terminal)
           (application-cache-miss-notices-command application)
           (test-assert
            (search "notices are on" (recording-terminal-output terminal))
            "/cache-misses without an argument reports the current state")
           (application-cache-miss-notices-command application nil)
           (test-assert
            (and (not (application-cache-miss-notices-p application))
                 (not (config :cache-miss-notices-p configuration)))
            "/cache-misses off applies and persists the preference")
           (test-assert
            (handler-case
                (progn
                  (application-cache-miss-notices-command application "loud")
                  nil)
              (configuration-error ()
                t))
            "an unknown /cache-misses argument is rejected")
           (test-assert
            (search "/cache-misses [on|off]" (application-help))
            "help lists the cache-miss notice command"))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
