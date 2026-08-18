(in-package #:autolith)

;;;; -- Recursive Inference Frames --

(defparameter *rlm-frame-system-prompt*
  "You are one inference frame inside Autolith, a recursive language model runtime.
You receive one task, optional read-only context views, and a required answer shape.
Ground every claim in the supplied views and the task itself; state plainly when they are insufficient.
There is no interlocutor and no tool access: never ask questions, never defer work.
Reply exactly in the requested shape with no preamble and no meta commentary."
  "The compact system prompt replacing the Autolith persona inside frames.")

(define-condition rlm-inference-error
    (error)
  ((task
    :initarg :task
    :initform nil
    :reader rlm-inference-error-task
    :type (option string)
    :documentation "The inference task involved in the failure, when known.")
   (message
    :initarg :message
    :reader rlm-inference-error-message
    :type string
    :documentation "The concise inference failure."))
  (:documentation "An inference frame could not be created or run.")
  (:report
   (lambda (condition stream)
     (format stream "Inference failed~@[ for task ~S~]: ~A"
             (rlm-inference-error-task condition)
             (rlm-inference-error-message condition)))))

(-> rlm--environment () (values model-provider configuration))
(defun rlm--environment ()
  "Return the active application's provider and configuration."
  (let ((application (and (boundp '*active-application*)
                          (symbol-value '*active-application*))))
    (unless application
      (error 'rlm-inference-error
             :message "No active application supplies an inference provider; pass :provider and :configuration."))
    (values (application-provider application)
            (application-configuration application))))

(-> rlm-contract-normalize (t) t)
(defun rlm-contract-normalize (contract)
  "Return ':TEXT or the canonical task output schema CONTRACT denotes."
  (if (or (null contract) (eq contract ':text))
      ':text
      (task-output-schema-normalize contract :source ':programmatic)))

(-> rlm--contract-instructions (t) string)
(defun rlm--contract-instructions (contract)
  "Return the answer-shape instructions for normalized CONTRACT."
  (if (eq contract ':text)
      "Reply with the answer alone."
      (format nil
              "Reply with exactly one JSON object satisfying this JSON Schema, and nothing else:~%~A"
              (json-encode (task-output-schema->json contract)))))

(-> rlm--parse-structured (string) t)
(defun rlm--parse-structured (text)
  "Return the JSON value TEXT carries, tolerating fences, or NIL."
  (flet ((decode (candidate)
           (let ((value (ignore-errors (json-decode candidate))))
             (and (json-object-p value) value))))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (or (decode trimmed)
          (let ((start (position #\{ trimmed))
                (end (position #\} trimmed :from-end t)))
            (when (and start end (< start end))
              (decode (subseq trimmed start (1+ end)))))))))

(-> rlm--contract-value (t (option string)) (values t boolean (option string)))
(defun rlm--contract-value (contract text)
  "Return CONTRACT's value in TEXT, its validity, and any repair reason."
  (let ((trimmed (and text
                      (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (cond
      ((not (non-empty-string-p trimmed))
       (values nil nil "The response contained no answer text."))
      ((eq contract ':text)
       (values trimmed t nil))
      (t
       (let ((value (rlm--parse-structured trimmed)))
         (cond
           ((null value)
            (values nil nil
                    "The response did not contain one parseable JSON object."))
           ((not (task-output-schema-valid-p value contract))
            (values nil nil
                    "The JSON object does not satisfy the required schema."))
           (t
            (values (task-json->sexp value) t nil))))))))

(-> rlm--frame-request (string list string) string)
(defun rlm--frame-request (task views instructions)
  "Compose the single frame user message from TASK, VIEWS, and INSTRUCTIONS."
  (format nil "Task: ~A~@[~%~%Read-only context views:~%~%~A~]~%~A"
          task
          (rlm-views-render views)
          instructions))

(-> rlm--frame-conversation (configuration) conversation)
(defun rlm--frame-conversation (configuration)
  "Create the private trace conversation for one inference frame."
  (let ((root (configuration-inference-root configuration)))
    (ensure-directories-exist root)
    (conversation-create configuration :storage-root root)))

(-> rlm--record-response (conversation provider-result) null)
(defun rlm--record-response (conversation result)
  "Append RESULT's items and usage to the frame trace CONVERSATION."
  (dolist (item (provider-result-output-items result))
    (conversation-append-provider-item conversation item))
  (conversation-append-provider-metadata
   conversation
   (list :usage (agent--portable-value (provider-result-usage result))))
  nil)

(-> infer
    (string &key (:context list)
                 (:contract t)
                 (:budget (option rlm-budget))
                 (:model (option string))
                 (:effort (option string))
                 (:provider (option model-provider))
                 (:configuration (option configuration)))
    (values t string))
(defun infer
    (task &key context (contract ':text) budget model effort provider
               configuration)
  "Run one bounded inference frame over CONTEXT and return TASK's value.

CONTEXT is a list of view designators materialized once for the frame.
CONTRACT is ':TEXT or a task output schema; schema answers return
portable tagged native data. The frame runs on a private conversation
persisted under the inference trace root and never touches the
caller's conversation; the second value is the trace conversation
identifier. Contract violations are repaired by re-asking until BUDGET
signals RLM-BUDGET-EXHAUSTED."
  (unless (non-empty-string-p task)
    (error 'rlm-inference-error
           :message "An inference frame requires a non-empty task."))
  (multiple-value-bind (environment-provider environment-configuration)
      (if (and provider configuration)
          (values provider configuration)
          (rlm--environment))
    (let* ((configuration (or configuration environment-configuration))
           (configuration
             (if model
                 (configuration-with-model configuration model)
                 configuration))
           (configuration
             (if effort
                 (configuration-with-reasoning-effort configuration effort)
                 configuration))
           (provider
             (if (or model effort)
                 (provider-with-configuration
                  (or provider environment-provider) configuration)
                 (or provider environment-provider)))
           (views (rlm-views-materialize context))
           (contract (rlm-contract-normalize contract))
           (budget (or budget (rlm-budget-create)))
           (conversation (rlm--frame-conversation configuration))
           (*system-prompt-override* *rlm-frame-system-prompt*))
      (conversation-append-user-message
       conversation
       (rlm--frame-request task views (rlm--contract-instructions contract)))
      (loop
        (rlm-budget-ensure budget :task task)
        (let ((result
                (provider-stream-turn provider conversation
                                      :tool-namespaces #()
                                      :event-callback
                                      (lambda (event)
                                        (declare (ignore event))
                                        nil))))
          (rlm--record-response conversation result)
          (rlm-budget-charge-call budget)
          (let ((total (conversation--usage-total
                        (provider-result-usage result))))
            (when total
              (rlm-budget-charge-tokens budget total)))
          (multiple-value-bind (value valid-p problem)
              (rlm--contract-value contract
                                   (provider-result-assistant-text result))
            (when valid-p
              (return (values value (conversation-identifier conversation))))
            (conversation-append-user-message
             conversation
             (format nil "~A Reply again in exactly the requested shape."
                     problem))))))))
