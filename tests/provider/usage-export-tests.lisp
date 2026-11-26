(in-package #:autolith)

;;;; -- Provider Usage Export --

(defclass usage-export-test-provider (model-provider)
  ((result :initarg :result :reader usage-export-test-provider-result
           :documentation "The successful fixture response.")
   (failure-p :initarg :failure-p :initform nil
              :reader        usage-export-test-provider-failure-p
              :documentation "Whether the fixture signals a provider failure."))
  (:documentation "A deterministic provider for the usage export boundary."))

(defmethod provider-stream-turn
    ((provider usage-export-test-provider) (conversation conversation)
     &key &allow-other-keys)
  "Return the fixture response or signal its configured failure."
  (if (usage-export-test-provider-failure-p provider)
      (error 'provider-error :message "Fixture provider failure.")
      (usage-export-test-provider-result provider)))

(-> usage-export-tests--read (pathname) list)
(defun usage-export-tests--read (path)
  "Decode each physical JSONL record in PATH."
  (with-open-file (stream path :external-format ':utf-8)
    (loop for line = (read-line stream nil) while line collect (json-decode line))))

(-> test-provider-usage-export-opt-in () null)
(defun test-provider-usage-export-opt-in ()
  "Resolve opt-in configuration when a request starts, not when loading a core."
  (with-test-configuration (configuration root)
    (let* ((*llm-usage-events-file* nil)
           (path                    (merge-pathnames "usage.jsonl" root))
           (conversation            (conversation-create configuration))
           (result                  (agent-test-result "usage-result" nil))
           (provider                (make-instance 'usage-export-test-provider
                                                   :result result)))
      (with-test-environment (("ANALYTICS_EVENTS_FILE" nil))
        (test-assert (null (llm-usage--destination)) "export requires a configured destination")
        (test-assert (eq result (provider-stream-turn provider conversation))
                     "disabled export preserves the response"))
      (with-test-environment (("ANALYTICS_EVENTS_FILE" ""))
        (test-assert (null (llm-usage--destination)) "an empty destination disables export"))
      (test-assert (not (probe-file path)) "disabled calls create no export file")
      (with-test-environment (("ANALYTICS_EVENTS_FILE" (namestring path)))
        (test-assert (eq result (provider-stream-turn provider conversation))
                     "runtime opt-in preserves the response")
        (test-assert (= 1 (length (usage-export-tests--read path)))
                     "runtime opt-in exports one completed call"))))
  nil)

(-> test-provider-usage-export-fields () null)
(defun test-provider-usage-export-fields ()
  "Preserve known zero counts, omit unknown counts, and encode JSON strings."
  (with-test-configuration (configuration root)
    (let* ((*llm-usage-events-file* (merge-pathnames "usage.jsonl" root))
           (conversation            (conversation-create configuration))
           (actor                   (format nil "žluťoučký ~S~%actor" "quoted")))
      (with-test-environment (("ANALYTICS_ACTOR" actor))
        (dolist (usage (list (json-object "input_tokens" 0 "output_tokens" 12
                                          "cached_input_tokens" 0)
                            (json-object "input_tokens" 7 "output_tokens" -1
                                         "cached_input_tokens" "unknown")
                            (json-object)))
          (provider-stream-turn
           (make-instance 'usage-export-test-provider
                          :result (agent-test-result "usage-result" nil :usage usage))
           conversation)))
      (let* ((events       (usage-export-tests--read *llm-usage-events-file*))
             (first-event  (first events))
             (second-event (second events)))
        (test-assert (= 3 (length events)) "each completed call has one physical JSONL record")
        (test-assert (string= actor (json-get first-event "actor")) "Unicode and escapes round-trip")
        (test-assert (and (= 1 (json-get first-event "schema_v"))
                          (= 1 (json-get first-event "requests"))
                          (string= "autolith" (json-get first-event "source"))
                          (string= "llm-usage" (json-get first-event "kind"))
                          (string= (conversation-model conversation) (json-get first-event "model"))
                          (typep (json-get first-event "wall_s") '(real 0 *)))
                     "events include schema, identity, model, request count, and elapsed time")
        (test-assert (and (= 0 (json-get first-event "tokens_in"))
                          (= 12 (json-get first-event "tokens_out"))
                          (= 0 (json-get first-event "cached_tokens")))
                     "known counts include explicit zero")
        (test-assert (and (= 7 (json-get second-event "tokens_in"))
                          (not (nth-value 1 (gethash "tokens_out" second-event)))
                          (not (nth-value 1 (gethash "cached_tokens" second-event)))
                          (not (nth-value 1 (gethash "tokens_in" (third events)))))
                     "unavailable and invalid counts are omitted")))
    (test-assert (string= "2000-01-02T03:04:05Z"
                          (llm-usage--iso-utc (encode-universal-time 5 4 3 2 1 2000 0)))
                 "timestamps use UTC independently of the host timezone"))
  nil)

(-> test-provider-usage-export-failures () null)
(defun test-provider-usage-export-failures ()
  "Keep exporter failures separate from provider failures."
  (with-test-configuration (configuration root)
    (let* ((*llm-usage-events-file* (merge-pathnames "usage.jsonl" root))
           (conversation            (conversation-create configuration))
           (result                  (agent-test-result "usage-result" nil))
           (provider                (make-instance 'usage-export-test-provider
                                                   :result result :failure-p t)))
      (test-assert
       (handler-case (progn (provider-stream-turn provider conversation) nil)
         (provider-error () t))
       "the provider failure propagates")
      (test-assert (not (probe-file *llm-usage-events-file*)) "failed calls export no event")
      (let ((*llm-usage-events-file* root))
        (test-assert
         (eq result (provider-stream-turn
                     (make-instance 'usage-export-test-provider
                                    :result result)
                     conversation))
         "an invalid file destination does not change a completed response"))))
  nil)

(-> test-provider-usage-export-concurrent () null)
(defun test-provider-usage-export-concurrent ()
  "Serialize concurrent appenders without partial JSONL records."
  (with-test-configuration (configuration root)
    (let* ((path         (merge-pathnames "usage.jsonl" root))
           (conversation (conversation-create configuration))
           (result       (agent-test-result "usage-result" nil))
           (threads      (loop repeat 4 collect
                               (make-thread
                                (lambda ()
                                  (loop repeat 5
                                        do (llm-usage--export conversation result
                                                              :destination path :wall-seconds 1d0)))))))
      (unwind-protect
           (progn
             (mapc #'join-thread threads)
             (let ((events (usage-export-tests--read path)))
               (test-assert (= 20 (length events)) "all concurrent calls produce a record")
               (test-assert (every (lambda (event) (= 1 (json-get event "requests"))) events)
                            "all concurrent records decode independently")))
        (dolist (thread threads)
          (when (thread-alive-p thread)
            (join-thread thread))))))
  nil)
