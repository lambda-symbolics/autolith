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
  "Test measured uncached input, observation limits, and stagnant cache counts."
  (let ((baseline (make-instance 'prompt-cache-baseline
                                 :prompt-tokens 40000
                                 :completed-at 1000
                                 :model "model-a")))
    (dolist (case '((:model-changed 1010 "model-b" 41000 0 41000 10)
                    (:idle 1400 "model-a" 41000 100 40900 400)
                    (:prompt-shrank 1010 "model-a" 8000 0 8000 10)
                    (:unknown 1010 "model-a" 41000 19999 21001 10)))
      (destructuring-bind (cause started-at model input cached uncached idle) case
        (let ((miss (prompt-cache-miss-detect
                     baseline (prompt-cache-tests--usage input cached)
                     :started-at started-at :model model)))
          (test-assert
           (and miss
                (eq (getf miss :cause) cause)
                (= (getf miss :uncached-tokens) uncached)
                (= (getf miss :prompt-tokens) input)
                (= (getf miss :cached-tokens) cached)
                (null (getf miss :stalled-p))
                (eql (getf miss :idle-seconds) idle))
           (format nil "~A reports current usage, not assumed reusable text" cause)))))
    (dolist (case '((40000 nil 41000 39000 "mostly cached")
                    (40000 nil 41000 20000 "threshold boundary")
                    (40000 nil 41000 nil "missing current cache counter")
                    (40000 30000 nil 0 "missing current input counter")
                    (500 0 600 0 "below reporting minimum")))
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
                 (make-instance 'prompt-cache-baseline :prompt-tokens 40000)
                 (prompt-cache-tests--usage 41000 0)
                 :started-at 1010 :model "model-a")))
      (test-assert
       (and miss (eq (getf miss :cause) ':resumed)
            (null (getf miss :idle-seconds)))
       "a resumed baseline reports session provenance, not cache expiry"))
    (dolist (case '((63872 73000 63872 t :unknown)
                    (63872 73000 63900 t :unknown)
                    (63872 73000 70000 nil nil)
                    (63872 73000 60000 t :unknown)
                    (63872 71000 63872 nil nil)
                    (69000 73000 69000 nil nil)
                    (72000 140000 72000 nil nil)
                    (69000 140000 69000 nil nil)
                    (nil 73000 63872 nil nil)
                    (1024 73000 1024 t :unknown)
                    (0 73000 0 t :unknown)
                    (1024 73000 20000 nil :unknown)))
      (destructuring-bind (previous-cached input cached stalled-p cause) case
        (let ((miss (prompt-cache-miss-detect
                     (make-instance 'prompt-cache-baseline
                                    :prompt-tokens 72000 :cached-tokens previous-cached
                                    :completed-at 1000 :model "model-a")
                     (prompt-cache-tests--usage input cached)
                     :started-at 1010 :model "model-a")))
          (test-assert
           (and (eql (not (null (getf miss :stalled-p))) stalled-p)
                (eq (getf miss :cause) cause)
                (or (null miss)
                    (= (getf miss :uncached-tokens) (- input cached))))
           (format nil "cache counts ~S yield stagnation ~S without inferring a cause"
                   case stalled-p)))))
    (let ((miss (prompt-cache-miss-detect
                 (make-instance 'prompt-cache-baseline
                                :prompt-tokens 72000 :cached-tokens 0
                                :completed-at 1000 :model "model-a")
                 (prompt-cache-tests--usage 73000 0)
                 :started-at 1010 :model "model-b")))
      (test-assert
       (and (getf miss :stalled-p) (eq (getf miss :cause) ':model-changed))
       "a severe stall preserves an independently observed model change"))
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
                 :model "model-a")))
      (test-assert
       (and miss
             (= (getf miss :instructions-uncached) 111)
             (= (getf miss :tools-uncached) 9221))
       "server attribution reports uncached instructions and tools")))
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

(-> test-prompt-cache-miss-notices () null)
(defun test-prompt-cache-miss-notices ()
  "Exercise compact colored cache-miss notices through the request lifecycle."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-ui"))
         (terminal (make-instance 'recording-terminal :columns 40 :styled-p t))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui ui)))
    (unwind-protect
         (let* ((observer (application-agent-observer application))
                (send-status
                  (callback-agent-observer-status-callback observer)))
           (flet ((notice-p ()
                    "Return whether the terminal painted a cache-miss notice."
                    (not (null (search "cache miss"
                                       (clinedi:ansi-strip (recording-terminal-output terminal))))))

                  (notice-counts-p (re-read total)
                    "Return whether the rendered notice includes both token counts."
                    (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                      (and (search "cache miss" output)
                           (search (application--token-count-description re-read) output)
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
                    :usage (prompt-cache-tests--usage 30000 0)))
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
                                  :completed-at (- (get-universal-time) 900)
                                  :model (config :model configuration)))
             (request (prompt-cache-tests--usage 33000 0))
             (test-assert (notice-counts-p 33000 33000)
                          "an idle miss reports measured uncached and total input")
             (let* ((spans (application--prompt-cache-miss-notice
                             '(:uncached-tokens 33000 :prompt-tokens 33000)))
                    (counts (remove-if-not
                             (lambda (span) (eq ':code (terminal-span-style span))) spans)))
               (test-assert (and (eq ':failure (terminal-span-style (first spans)))
                                 (string= "! " (terminal-span-text (first spans))))
                            "the cache-miss ledger entry begins with a red exclamation mark")
               (test-assert (equal (mapcar #'terminal-span-text counts)
                                   (mapcar #'application--token-count-description '(33000 33000)))
                             "the renderer reports measured uncached input"))
             (setf (application-prompt-cache-baseline application)
                   (make-instance 'prompt-cache-baseline
                                  :prompt-tokens 72000
                                  :cached-tokens 63872
                                  :completed-at (get-universal-time)
                                  :model (config :model configuration)))
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
             (test-assert (notice-counts-p 9128 73000)
                          "a stalled cache reports measured uncached and total input")
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
                      :usage (prompt-cache-tests--usage 20000 19000)))
               (setf (application-conversation application) resumed)
               (request (prompt-cache-tests--usage 21000 0))
               (test-assert (not (notice-p))
                            "the first request after loading a conversation establishes a fresh baseline")
               (request (prompt-cache-tests--usage 22000 0))
               (test-assert (notice-counts-p 22000 22000)
                            "later misses compare against requests from this session")
               (setf (application-conversation application)
                     (conversation-load (conversation-pathname resumed)))
               (request (prompt-cache-tests--usage 23000 0))
               (test-assert (not (notice-p))
                            "resuming the same conversation again starts a fresh cache window")
               (request (prompt-cache-tests--usage 24000 0))
               (test-assert (notice-counts-p 24000 24000)
                            "same-conversation resume suppresses only its first request"))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-cache-misses-command () null)
(defun test-cache-misses-command ()
  "Test the /cache-misses command persists and applies its preference."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-command"))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui ui)))
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
