(in-package #:autolith)

;;;; -- Next-Request Context Admission --

(defclass context-admission-test-provider (codex-subscription-provider)
  ((network-calls
    :initform      0
    :accessor      context-admission-test-provider-network-calls
    :documentation "Number of admitted requests reaching the fake transport."))
  (:documentation "A real product projection with an inert observable transport."))

(defmethod provider-open-response-stream
    ((provider context-admission-test-provider) (request hash-table)
     &key credentials conversation)
  "Record an admitted request without credentials or network activity."
  (declare (ignore request credentials conversation))
  (incf (context-admission-test-provider-network-calls provider))
  (values nil 200 nil))

(-> test-context-admission-boundary () null)
(defun test-context-admission-boundary ()
  "Reserve explicit output allowances and stop rejected requests before transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                         ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider (make-instance 'context-admission-test-provider
                                     :configuration configuration))
             (request  (json-object "input" (make-string 1000 :initial-element #\x)))
             (window   (config :context-window configuration)))
        (test-assert (= window 5000) "the test controls the shared context window")
        (provider-open-response-stream provider request)
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "an admitted request reaches transport once")
        (setf (gethash "max_output_tokens" request) 4900)
        (test-assert
         (handler-case
             (progn (provider-open-response-stream provider request) nil)
           (provider-context-budget-error (condition)
             (and (= 4900 (provider-context-budget-error-output-reserve condition))
                  (= window (provider-context-budget-error-context-window condition))
                  (plusp (provider-context-budget-error-estimated-input-tokens condition)))))
         "an explicit generation cap takes precedence over minimum headroom")
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "rejection occurs before network I/O")
        (dolist (field '("max_tokens" "max_completion_tokens"))
          (let ((capped (json-object field 6000)))
            (test-assert (= 6000 (provider-request-output-reserve provider capped))
                         "Messages and Chat output caps reserve the requested allowance")))
        (test-assert
         (= 7000 (provider-request-output-reserve
                  provider (json-object "generationConfig"
                                        (json-object "maxOutputTokens" 7000))))
         "GenerateContent output caps participate in admission")
        (test-assert
         (null (provider-check-request-context provider (json-object)))
         "a rejected request does not poison a later small request"))))
  nil)

(-> test-context-admission-fresh-projection () null)
(defun test-context-admission-fresh-projection ()
  "New input and tool evidence trigger compaction independently of stale reported usage."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (provider-create configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (test-call-with-function-replacements
         (list (list 'system-prompt (lambda (configuration)
                                     (declare (ignore configuration)) "small prompt"))
               (list 'request-context-session-state
                     (lambda (configuration &key hurry-up-p)
                       (declare (ignore configuration hurry-up-p)) ""))
               (list 'tool-registry-provider-schemas
                     (lambda (registry &key canonical-names)
                       (declare (ignore registry canonical-names)) #())))
         (lambda ()
           (setf (conversation-last-total-tokens conversation) 100000)
           (test-assert (not (agent-should-compact-p agent))
                        "high stale usage does not force compaction of a small current projection")
           (setf (conversation-last-total-tokens conversation) 1)
           (conversation-append-user-message conversation (make-string 20000 :initial-element #\u))
           (test-assert (agent-should-compact-p agent)
                        "new user input crosses the soft threshold before any usage response")
           (conversation-append-summary conversation "small checkpoint")
           (test-assert (not (agent-should-compact-p agent))
                        "the replacement is measured instead of trusting the reset usage counter")
           (conversation-append-provider-item
            conversation (json-object "type" "function_call" "call_id" "fresh-evidence"
                                      "name" "test" "arguments" "{}"))
           (conversation-append-tool-result
            conversation "fresh-evidence" :tool-name "test"
            :output (make-string 20000 :initial-element #\e) :success-p nil)
           (test-assert (agent-should-compact-p agent)
                        "new tool results contribute before the next provider request"))))))
  nil)

(-> test-context-admission-final-request () null)
(defun test-context-admission-final-request ()
  "Measure newly projected instructions, tools and temporary context without rewriting history."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                         ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'context-admission-test-provider
                                         :configuration configuration))
             (conversation (conversation-create configuration))
             (large        (make-string 22000 :initial-element #\c))
             (baseline     (mapcar #'json-encode (conversation-input-items conversation))))
        (dolist (request
                  (list (json-object "instructions" large)
                        (json-object "input" (vector (json-object "role" "developer" "content" large)))
                        (json-object "tools" (vector (json-object "description" large)))))
          (test-assert
           (handler-case
               (progn (provider-open-response-stream provider request :conversation conversation) nil)
             (provider-context-budget-error () t))
           "every prompt-bearing component in the final request participates in admission"))
        (test-assert (zerop (context-admission-test-provider-network-calls provider))
                     "oversized temporary context is never sent")
        (test-assert (equal baseline (mapcar #'json-encode (conversation-input-items conversation)))
                     "admission rejection preserves the original conversation projection")
        (test-assert (zerop (conversation-last-total-tokens conversation))
                     "local estimates are not recorded as paid usage")
        (conversation-append-summary conversation large)
        (test-assert
         (handler-case
             (progn
               (provider-open-response-stream
                provider (json-object "input" (coerce (conversation-input-items conversation) 'vector)))
               nil)
           (provider-context-budget-error () t))
         "an oversized replacement must pass admission again before continuing"))))
  nil)

(-> test-context-admission-images () null)
(defun test-context-admission-images ()
  "Use the image allowance rather than base64 size or short URL length."
  (with-test-configuration (configuration)
    (let ((provider (make-instance 'context-admission-test-provider :configuration configuration)))
      (multiple-value-bind (short uncertain-p)
          (provider-request-input-token-estimate
           provider (json-object "type" "input_image" "image_url" "https://example.invalid/image"))
        (test-assert (and uncertain-p (= short *provider-image-token-estimate*))
                     "image token accounting is explicitly approximate")
        (multiple-value-bind (encoded encoded-uncertain-p)
            (provider-request-input-token-estimate
             provider (json-object "type" "input_image" "image_url"
                                   (concatenate 'string "data:image/png;base64,"
                                                (make-string 100000 :initial-element #\a))))
          (test-assert (and encoded-uncertain-p (= short encoded))
                       "base64 wire size is not interpreted as text tokens")))))
  nil)
