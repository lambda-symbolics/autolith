(in-package #:autolith)

;;;; -- Prompt-Cache Miss Tests --

(-> prompt-cache-tests--usage (integer (option integer)) list)
(defun prompt-cache-tests--usage (input cached)
  "Return portable usage with INPUT prompt tokens and optional CACHED reads."
  (append (list (list "input_tokens" input)
                (list "output_tokens" 10))
          (and cached
               (list (list "cached_input_tokens" cached)))))

(-> test-prompt-cache-miss-detection () null)
(defun test-prompt-cache-miss-detection ()
  "Test cache-miss thresholds, re-read accounting, and cause attribution."
  (let ((baseline (make-instance 'prompt-cache-baseline
                                 :prompt-tokens 40000
                                 :completed-at 1000
                                 :model "model-a")))
    (test-assert
     (null (prompt-cache-miss-detect nil (prompt-cache-tests--usage 41000 0)
                                     :started-at 1010
                                     :model "model-a"))
     "the first request of a conversation has no baseline to miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 39000)
                                     :started-at 1010
                                     :model "model-a"))
     "a request served mostly from the cache is not a miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 20000)
                                     :started-at 1010
                                     :model "model-a"))
     "cache reads reaching half of the previous prompt are not a miss")
    (test-assert
     (null (prompt-cache-miss-detect baseline
                                     (prompt-cache-tests--usage 41000 nil)
                                     :started-at 1010
                                     :model "model-a"))
     "providers without cache counters report nothing")
    (test-assert
     (null (prompt-cache-miss-detect
            (make-instance 'prompt-cache-baseline
                           :prompt-tokens 500
                           :completed-at 1000
                           :model "model-a")
            (prompt-cache-tests--usage 600 0)
            :started-at 1010
            :model "model-a"))
     "prompts below the cacheable minimum report nothing")
    (dolist (case '((:model-changed 1010 "model-b" 41000 0 40000 10)
                    (:idle 1400 "model-a" 41000 100 39900 400)
                    (:context-rewritten 1010 "model-a" 8000 0 8000 10)
                    (:prefix-changed 1010 "model-a" 41000 19999 20001 10)))
      (destructuring-bind (cause started-at model input cached re-read idle)
          case
        (let ((miss (prompt-cache-miss-detect
                     baseline
                     (prompt-cache-tests--usage input cached)
                     :started-at started-at
                     :model model)))
          (test-assert
           (and miss
                (eq (getf miss :cause) cause)
                (= (getf miss :re-read-tokens) re-read)
                (= (getf miss :prompt-tokens) input)
                (= (getf miss :cached-tokens) cached)
                (eql (getf miss :idle-seconds) idle))
           (format nil "a ~(~A~) miss reports its re-read tokens and cause"
                   cause)))))
    (let ((miss (prompt-cache-miss-detect
                 (make-instance 'prompt-cache-baseline :prompt-tokens 40000)
                 (prompt-cache-tests--usage 41000 0)
                 :started-at 1010
                 :model "model-a")))
      (test-assert
       (and miss
            (eq (getf miss :cause) ':resumed)
            (null (getf miss :idle-seconds)))
       "a persisted baseline without a time attributes the miss to resuming")))
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
  "Test optional transcript notices for prompt-cache misses across requests."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cache-miss-ui"))
         (terminal (make-instance 'recording-terminal :columns 120))
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
                    (not (null (search "prompt cache miss"
                                       (recording-terminal-output terminal)))))
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
                          "notices stay silent while the preference is off")
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
                                  :model (configuration-model configuration)))
             (request (prompt-cache-tests--usage 33000 0))
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (and (search "prompt cache miss" output)
                     (search "32.0K of 33.0K prompt tokens" output)
                     (search "idle for 15 min" output))
                "an idle miss reports its re-read tokens and cause"))
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
               (test-assert
                (search "resumed conversation"
                        (recording-terminal-output terminal))
                "switching conversations reseeds the baseline from its history"))))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
           (application-cache-miss-notices-command application "on")
           (test-assert
            (and (application-cache-miss-notices-p application)
                 (preferences-cache-miss-notices-p configuration))
            "/cache-misses on applies and persists the preference")
           (recording-terminal-reset terminal)
           (application-cache-miss-notices-command application nil)
           (test-assert
            (search "notices are on" (recording-terminal-output terminal))
            "/cache-misses without an argument reports the current state")
           (application-cache-miss-notices-command application "OFF")
           (test-assert
            (and (not (application-cache-miss-notices-p application))
                 (not (preferences-cache-miss-notices-p configuration)))
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
