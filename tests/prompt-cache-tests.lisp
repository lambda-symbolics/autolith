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
