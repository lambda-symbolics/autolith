(in-package #:autolith)

;;;; -- Agent and Provider Lifecycles --

(defvar *telemetry-provider-attempt-count* nil
  "The count of per-attempt provider hooks inside the current request.")

(defvar *telemetry-response-model* nil
  "The response model observed in the current provider attempt, never a requested-model fallback.")

(-> telemetry--response-model (model-provider string) (option string))
(defun telemetry--response-model (provider data)
  "Read only the model field of a known protocol envelope, ignoring generated output."
  (handler-case
      (let* ((event (json-decode data))
             (model
               (when (hash-table-p event)
                 (case (provider-wire-protocol provider)
                   (:responses-api
                    (when (member (json-get event "type")
                                  '("response.created" "response.completed") :test #'equal)
                      (json-get (json-get event "response") "model")))
                   (:chat-completions (json-get event "model"))
                   (:anthropic-messages
                    (when (equal (json-get event "type") "message_start")
                      (json-get (json-get event "message") "model")))
                   (:gemini-generate-content
                    (json-get (cl-llm-provider-api:provider-gemini-stream-response provider event)
                              "modelVersion"))))))
        (when (and (stringp model) (<= 1 (length model) 256)) model))
    (error () nil)))

(defmethod provider-consume-stream :around
    ((provider model-provider) stream headers event-callback)
  "Observe bounded SSE model metadata until the provider API exposes it in results.
The provider's original parser remains authoritative. No stream data is retained
after its event boundary, and disabled telemetry never installs an observer."
  (declare (ignore headers event-callback))
  (if (null *telemetry-run*)
      (call-next-method)
      (let ((reader *sse-read-line-function*)
            (lines nil)
            (size 0)
            (discard-p nil))
        (labels ((finish-event ()
                   (unless discard-p
                     (when lines
                       (let ((model (telemetry--response-model
                                     provider (format nil "~{~A~^~%~}" (nreverse lines)))))
                         (when model (setf *telemetry-response-model* model)))))
                   (setf lines nil size 0 discard-p nil)))
          (let ((*sse-read-line-function*
                  (lambda (input)
                    (let ((raw (funcall reader input)))
                      (when (eq input stream)
                        (cond
                          ((eq raw *sse-end-of-stream*) (finish-event))
                          ((stringp raw)
                           (let ((line (string-right-trim '(#\Return) raw)))
                             (cond
                               ((zerop (length line)) (finish-event))
                               ((and (>= (length line) 5) (string= line "data:" :end1 5))
                                (let ((data (subseq line (if (and (> (length line) 5)
                                                                 (char= (char line 5) #\Space)) 6 5))))
                                  (incf size (+ (length data) (if lines 1 0)))
                                  (if (> size *sse-maximum-event-characters*)
                                      (setf discard-p t lines nil)
                                      (unless discard-p (push data lines))))))))))
                      raw))))
            (call-next-method))))))

(defmethod agent-run-user-turn :around
    ((agent agent) (content user-message-input) &rest arguments &key &allow-other-keys)
  "Measure one logical agent turn, including failed and cancelled turns."
  (declare (ignore content arguments))
  (telemetry-call-with-run (agent-configuration agent)
                           (lambda () (call-next-method))))

(-> telemetry--call-with-provider-run (model-provider function) t)
(defun telemetry--call-with-provider-run (provider function)
  "Run standalone requests with telemetry only when a provider has valid configuration."
  (if *telemetry-run*
      (funcall function)
      (let ((configuration
              (telemetry--call-safely (lambda () (provider-configuration provider)))))
        (if (typep configuration 'configuration)
            (telemetry-call-with-run configuration function)
            (funcall function)))))

(-> telemetry--provider-name (model-provider) string)
(defun telemetry--provider-name (provider)
  "Map a local provider family to a fixed public telemetry category."
  (case (provider-family provider)
    ((:codex :openai :openai-chat :openai-responses) "openai")
    (:anthropic "anthropic")
    ((:gemini :gemini-code-assist :google) "google")
    ((:grok :xai) "xai")
    (:ollama "ollama")
    (:deepseek "deepseek")
    (:mistral "mistral_ai")
    (:cohere "cohere")
    (:local "local")
    (otherwise "other")))

(-> telemetry--provider-attempt
    (model-provider function &key (:native-compaction-p boolean)) t)
(defun telemetry--provider-attempt (provider function &key native-compaction-p)
  "Count one real attempt and project only normalized usage from its returned values."
  (when *telemetry-provider-attempt-count*
    (incf *telemetry-provider-attempt-count*))
  (let ((*telemetry-response-model* nil)
        (start (get-internal-real-time))
        (results nil))
    (unwind-protect
         (progn
           (setf results (multiple-value-list (funcall function)))
           (values-list results))
      (telemetry--note-provider-result
       provider start results :native-compaction-p native-compaction-p))))

(defmethod provider-attempt-turn :around
    ((provider model-provider) (conversation conversation)
     &rest arguments &key &allow-other-keys)
  "Count authentication recovery and transport retries as separate model attempts."
  (declare (ignore conversation arguments))
  (telemetry--provider-attempt provider (lambda () (call-next-method))))

(defmethod provider-attempt-native-compaction :around
    ((provider model-provider) (conversation conversation)
     &rest arguments &key &allow-other-keys)
  "Measure native compaction attempts without exporting checkpoint material."
  (declare (ignore conversation arguments))
  (telemetry--provider-attempt provider (lambda () (call-next-method))
                              :native-compaction-p t))

;; Mission admission specializes CONVERSATION on its class. Use T here so these
;; wrappers compose with that accounting method instead of replacing it.
(defmethod provider-stream-turn :around
    ((provider model-provider) (conversation t)
     &rest arguments &key &allow-other-keys)
  "Cover standalone recursive requests and providers without a per-attempt protocol."
  (declare (ignore conversation arguments))
  (telemetry--call-with-provider-run
   provider
   (lambda ()
     (let ((*telemetry-provider-attempt-count* 0)
           (*telemetry-response-model* nil)
           (start (get-internal-real-time))
           (results nil))
       (unwind-protect
            (progn
              (setf results (multiple-value-list (call-next-method)))
              (values-list results))
         (when (zerop *telemetry-provider-attempt-count*)
           (telemetry--note-provider-result provider start results)))))))

(defmethod provider-native-compact-conversation :around
    ((provider model-provider) (conversation t)
     &rest arguments &key &allow-other-keys)
  "Cover native compaction outside agent turns, preserving all returned values."
  (declare (ignore conversation arguments))
  (telemetry--call-with-provider-run
   provider
   (lambda ()
     (let ((*telemetry-provider-attempt-count* 0)
           (*telemetry-response-model* nil)
           (start (get-internal-real-time))
           (results nil))
       (unwind-protect
            (progn
              (setf results (multiple-value-list (call-next-method)))
              (values-list results))
         (when (zerop *telemetry-provider-attempt-count*)
           (telemetry--note-provider-result
            provider start results :native-compaction-p t)))))))

(-> telemetry--note-provider-result
    (model-provider integer list &key (:native-compaction-p boolean)) null)
(defun telemetry--note-provider-result (provider start results &key native-compaction-p)
  "Emit a usage-only event for an attempt, even if it failed before returning."
  (when *telemetry-run*
    (telemetry--call-safely
     (lambda ()
       (let* ((result (first results))
              (usage (telemetry--call-safely
                      (lambda ()
                        (if native-compaction-p
                            (second results)
                            (when (typep result 'provider-result)
                              (provider-result-usage result))))))
              (normalized (when usage
                            (telemetry--call-safely
                             (lambda () (provider-usage-normalize usage))))))
         (telemetry-note-model
          :run *telemetry-run*
          :provider (or (telemetry--call-safely
                         (lambda () (telemetry--provider-name provider)))
                        "other")
          :request-model (telemetry--call-safely
                          (lambda ()
                            (config :model (provider-configuration provider))))
          :response-model *telemetry-response-model*
          :usage normalized
          :duration-ms (telemetry--elapsed-milliseconds start))))))
  nil)

;;;; -- Process Configuration Ownership --

(defmethod (setf application-configuration) :around
    ((configuration configuration) (application application))
  "Retire telemetry consent when the owning application's configuration changes."
  (let* ((previous (when (slot-boundp application 'configuration)
                     (application-configuration application)))
         (replace-owner-p (and *telemetry-controller*
                               (eq previous (telemetry-controller-owner *telemetry-controller*))
                               (not (eq previous configuration)))))
    (multiple-value-prog1 (call-next-method)
      (when replace-owner-p
        (telemetry--call-safely
         (lambda ()
           (telemetry-shutdown)
           (telemetry-attach configuration)))))))

;;;; -- Checkpoint State --

(defmethod checkpoint-detach-state :before ((application application))
  "Remove queued telemetry and foreign/process state from a saver child's heap."
  (declare (ignore application))
  (telemetry-detach))
