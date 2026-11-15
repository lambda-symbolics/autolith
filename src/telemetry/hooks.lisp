(in-package #:autolith)

;;;; -- Agent and Provider Lifecycles --

(defvar *telemetry-provider-attempt-count* nil
  "The count of per-attempt provider hooks inside the current request.")

(defvar *telemetry-response-metadata* nil
  "The latest public metadata snapshot for the current provider attempt.")

(defmethod provider-consume-stream :around
    ((provider model-provider) stream event-callback &key headers)
  "Observe public response metadata without changing parser or callback failures."
  (if (null *telemetry-run*)
      (call-next-method)
      (call-next-method
       provider stream
       (lambda (event)
         (telemetry--call-safely
          (lambda ()
            (when (typep event 'provider-response-metadata-event)
              (setf *telemetry-response-metadata*
                    (provider-response-metadata-event-metadata event)))))
         (when event-callback
           (funcall event-callback event)))
       :headers headers)))

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

(-> telemetry--provider-name (model-provider) keyword)
(defun telemetry--provider-name (provider)
  "Map a local provider family to a fixed public telemetry category."
  (case (provider-family provider)
    ((:codex :openai :openai-chat :openai-responses)
     ':openai)
    (:anthropic
     ':anthropic)
    ((:gemini :gemini-code-assist :google)
     ':google)
    ((:grok :xai)
     ':xai)
    (:ollama
     ':ollama)
    (:deepseek
     ':deepseek)
    (:mistral
     ':mistral-ai)
    (:cohere
     ':cohere)
    (:local
     ':local)
    (otherwise
     ':other)))

(-> telemetry--condition-error-type (condition) keyword)
(defun telemetry--condition-error-type (condition)
  "Classify failures without exporting reports, server bodies, or arbitrary class names."
  (cond ((typep condition 'application-turn-cancelled)
         ':cancelled)
        ((typep condition 'job-aborted)
         (if (eq (job-aborted-reason condition) ':timeout) ':timeout ':cancelled))
        ((typep condition 'authentication-error)
         ':authentication)
        ((typep condition 'autolith-control-condition)
         ':cancelled)
        ((typep condition '(or provider-transport-error stream-error))
         ':transport)
        ((typep condition 'cl-llm-provider-api:provider-error)
         (case (cl-llm-provider-api:provider-error-status condition)
           ((401 403)
            ':authentication)
           (429
            ':rate-limit)
           (otherwise
            ':provider)))
        ((typep condition 'agent-loop-error)
         ':protocol)
        ((or (typep condition 'tool-authority-denied)
             (typep condition 'command-authorization-unavailable)
             (telemetry--permission-error-code-p (tool-failure-code condition)))
         ':permission)
        ((typep condition 'tool-error)
         ':tool)
        (t
         ':internal)))

(-> telemetry--provider-attempt (model-provider function &key (:fallback-p boolean)) t)
(defun telemetry--provider-attempt (provider function &key fallback-p)
  "Record terminal outcomes of real attempts, preserving values and nonlocal exits."
  (when (and (not fallback-p) *telemetry-provider-attempt-count*)
    (incf *telemetry-provider-attempt-count*))
  (let ((*telemetry-response-metadata* nil)
        (start                         (get-internal-real-time))
        (outcome                       ':cancelled)
        (error-type                    nil)
        (results                       nil))
    (unwind-protect
         (handler-bind ((serious-condition
                         (lambda (condition)
                           (when (typep condition 'provider-result-rejected)
                             (setf results (list (provider-result-rejected-result condition))))
                           (setf error-type (telemetry--condition-error-type condition)
                                 outcome (telemetry--outcome :error-type error-type)))))
           (setf results (multiple-value-list (funcall function))
                 outcome ':success error-type nil)
           (values-list results))
      (when (or (not fallback-p) (zerop *telemetry-provider-attempt-count*))
        (telemetry--note-provider-result
         provider start results :outcome outcome :error-type error-type)))))

(defmethod provider-attempt-turn :around
    ((provider model-provider) (conversation conversation)
     &rest arguments &key &allow-other-keys)
  "Count authentication recovery and transport retries as separate model attempts."
  (declare (ignore conversation arguments))
  (telemetry--provider-attempt provider (lambda () (call-next-method))))

;; Mission admission specializes CONVERSATION on its class. Use T here so this
;; wrapper composes with that accounting method instead of replacing it.
(defmethod provider-stream-turn :around
    ((provider model-provider) (conversation t)
     &rest arguments &key &allow-other-keys)
  "Cover standalone recursive requests and providers without a per-attempt protocol."
  (declare (ignore conversation arguments))
  (telemetry--call-with-provider-run
   provider
   (lambda ()
     (let ((*telemetry-provider-attempt-count* 0))
       (telemetry--provider-attempt provider (lambda () (call-next-method)) :fallback-p t)))))

(-> telemetry--note-provider-result
    (model-provider integer list &key (:outcome keyword) (:error-type (option keyword))) null)
(defun telemetry--note-provider-result (provider start results &key outcome error-type)
  "Emit a usage-only event for an attempt, even if it failed before returning."
  (when *telemetry-run*
    (telemetry--call-safely
     (lambda ()
       (let* ((result     (first results))
              (metadata   (or *telemetry-response-metadata*
                              (when (typep result 'provider-result)
                                (provider-result-metadata result))))
              (usage      (when (typep result 'provider-result)
                            (provider-result-usage result)))
              (normalized (when usage
                            (provider-usage-normalize usage)))
              (writes     (if metadata
                              (provider-response-metadata-cache-write-tokens metadata)
                              (provider-cache-write-tokens provider usage))))
         (when writes
           (unless (hash-table-p normalized)
             (setf normalized (make-hash-table :test 'equal))))
         (when (hash-table-p normalized)
           (remhash "cache_creation_input_tokens" normalized)
           (when writes
             (setf (gethash "cache_creation_input_tokens" normalized) writes)))
         (telemetry-note-model
          :run            *telemetry-run*
          :provider       (telemetry--call-safely (lambda () (telemetry--provider-name provider)))
          :request-model  (telemetry--call-safely
                           (lambda ()
                             (config :model (provider-configuration provider))))
          :response-model (when metadata (provider-response-metadata-model metadata))
          :outcome outcome :error-type error-type
          :role           *telemetry-request-role*
          :finish-reason  (or (when metadata (provider-response-metadata-finish-reason metadata))
                              (when (typep result 'provider-result)
                                (if (provider-result-tool-calls result) ':tool-calls
                                    (case (provider-result-turn-completion result)
                                      (:end
                                       ':stop)
                                      (:continue
                                       ':continue)))))
          :usage          normalized
          :duration-ms    (telemetry--elapsed-milliseconds start))))))
  nil)


;;;; -- Process Configuration Ownership --

(defmethod (setf application-configuration) :around
    ((configuration configuration) (application application))
  "Retire telemetry consent when the owning application's configuration changes."
  (let* ((previous        (when (slot-boundp application 'configuration)
                            (application-configuration application)))
         (replace-owner-p (and *telemetry-controller*
                               (eq previous (telemetry-controller-owner *telemetry-controller*))
                               (not (eq previous configuration)))))
    (multiple-value-prog1 (call-next-method)
      (when replace-owner-p
        (telemetry--call-safely
         (lambda ()
           (telemetry-shutdown :drain-p nil)
           (telemetry-attach configuration)))))))


;;;; -- Checkpoint State --

(defmethod checkpoint-detach-state :before ((application application))
  "Remove queued telemetry and foreign/process state from a saver child's heap."
  (declare (ignore application))
  (telemetry-detach))
