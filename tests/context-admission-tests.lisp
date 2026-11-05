(in-package #:autolith)

;;;; -- Next-Request Context Admission --

(defclass context-admission-test-provider (codex-subscription-provider)
  ((last-ceiling
    :initform      nil
    :accessor      context-admission-test-provider-last-ceiling
    :documentation "Effective local output ceiling at the transport boundary.")
   (last-request
    :initform      nil
    :accessor      context-admission-test-provider-last-request
    :documentation "The actual final request seen by the inert transport.")
   (network-calls
    :initform      0
    :accessor      context-admission-test-provider-network-calls
    :documentation "Number of admitted requests reaching the fake transport."))
  (:documentation "A real product projection with an inert observable transport."))

(defmethod provider-open-response-stream
    ((provider context-admission-test-provider) (request hash-table)
     &key credentials conversation)
  "Record an admitted request without credentials or network activity."
  (declare (ignore credentials conversation))
  (setf (context-admission-test-provider-last-request provider) request
        (context-admission-test-provider-last-ceiling provider) *provider-maximum-output-tokens*)
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

(-> test-context-admission-checkpoint-transaction () null)
(defun test-context-admission-checkpoint-transaction ()
  "Reject oversized final checkpoints before changing durable or projected history."
  (dolist (native-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance 'context-admission-test-provider
                                            :configuration configuration))
               (window       (list (json-object "type" "compaction"
                                                "encrypted_content" "private-checkpoint"))))
          (conversation-append-user-message conversation "Original exact request.")
          (let ((capture (conversation-compaction-capture conversation)))
            (conversation-append-user-message conversation
                                              (make-string 20000 :initial-element #\a))
            (let ((before    (copy-list (conversation-input-items conversation)))
                  (sequence  (conversation-next-sequence conversation))
                  (checked-p nil))
              (flet ((validate (candidate)
                       "Admit the full candidate, including arrivals, before publication."
                       (setf checked-p t)
                       (test-assert (find (second before)
                                          (conversation-input-items candidate) :test #'eq)
                                    "the validator sees the exact chronological arrival")
                       (provider-check-request-context
                        provider (json-object "input"
                                              (coerce (conversation-input-items candidate) 'vector)))))
                (test-assert
                 (handler-case
                     (progn
                       (if native-p
                           (conversation-append-native-compaction
                            conversation window :family ':responses :summary "Small handoff."
                            :compaction capture :validate-projection #'validate)
                           (conversation-append-summary
                            conversation "Small handoff." :compaction capture
                            :validate-projection                      #'validate))
                       nil)
                   (provider-context-budget-error () t))
                 "oversized candidate admission aborts publication"))
              (test-assert checked-p "publication invoked the admission callback")
              (test-assert (and (= sequence (conversation-next-sequence conversation))
                                (equal before (conversation-input-items conversation)))
                           "rejection preserves the full projection and durable sequence")
              (let ((loaded (conversation-load-by-id
                             configuration (conversation-identifier conversation))))
                (test-assert
                 (equal (mapcar #'json-encode before)
                        (mapcar #'json-encode (conversation-input-items loaded)))
                 "reload sees no rejected checkpoint"))))))))
  nil)

(-> test-context-admission-mission-large-budget () null)
(defun test-context-admission-mission-large-budget ()
  "Bound a real mission allocation at final projection without shrinking its ledger."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "272000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'context-admission-test-provider
                                          :configuration configuration))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application)))
        (application-mission-start
         application (mission-test--specification :turns 5 :tokens most-positive-fixnum))
        (unwind-protect
             (let* ((context              (mission-context-find conversation))
                    (goal                 (mission-context-goal context))
                    (original-reservation (floor most-positive-fixnum 5)))
               (mission--account-inference
                context
                (lambda ()
                  (test-assert (= original-reservation (getf goal :tokens-reserved))
                               "the original aggregate reservation is retained through dispatch")
                  (provider-open-response-stream
                   provider (provider-request-object provider conversation #())
                   :conversation conversation))
                (lambda (results)
                  (declare (ignore results))
                  (json-object "input_tokens" 7 "output_tokens" 3)))
               (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                            "real projection and admission dispatch exactly once")
               (let ((ceiling (context-admission-test-provider-last-ceiling provider))
                     (request (context-admission-test-provider-last-request provider)))
                 (test-assert (and (plusp ceiling) (<= ceiling original-reservation))
                              "only the internal output allowance is reduced")
                 (test-assert (<= (+ (provider-request-input-token-estimate provider request)
                                     (max 256 ceiling))
                                  272000)
                              "the actual final request fits its complete context envelope")
                 (test-assert (null (json-get request "max_output_tokens"))
                              "the unsupported Codex wire ceiling is omitted"))
               (test-assert (= most-positive-fixnum (getf goal :token-limit))
                            "the finite aggregate budget is unchanged")
               (test-assert (and (= 10 (getf goal :tokens-used))
                                 (zerop (getf goal :tokens-reserved))
                                 (zerop (getf goal :requests-outstanding)))
                            "reported usage is charged once and the original reservation released"))
          (mission-detach application)))))
  (context-admission-test--bounded-child)
  nil)


;;;; -- Mission Ceiling Transport Fixtures --

(defclass context-admission-capped-test-provider (context-admission-test-provider)
  ()
  (:documentation "An inert Responses transport that accepts output ceilings."))

(defmethod provider-output-ceiling-p ((provider context-admission-capped-test-provider))
  "Enable the standard Responses wire ceiling for this inert transport."
  (declare (ignore provider))
  t)

(defmethod provider-open-native-compaction
    ((provider context-admission-test-provider) (request hash-table)
     &key credentials conversation)
  "Capture admitted native compaction without opening a transport."
  (declare (ignore credentials conversation))
  (setf (context-admission-test-provider-last-request provider) request
        (context-admission-test-provider-last-ceiling provider) *provider-maximum-output-tokens*)
  (incf (context-admission-test-provider-network-calls provider))
  (values nil 200 nil))

(-> test-context-admission-mission-wire-ceiling () null)
(defun test-context-admission-mission-wire-ceiling ()
  "Measure serialized cap overhead and finalize ordinary and compaction requests."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (native-p '(nil t))
        (let* ((provider                            (make-instance 'context-admission-capped-test-provider
                                                                   :configuration configuration))
               (request                             (json-object "input" (make-string 1000 :initial-element #\x)))
               (original                            (json-encode request))
               (*provider-mission-output-allowance* most-positive-fixnum)
               (observed                            nil)
               (*provider-cache-request-prefix*     '(:fixture t))
               (*provider-cache-prefix-function*
                 (lambda (prefix) (setf observed prefix))))
          (funcall (if native-p #'provider-open-native-compaction #'provider-open-response-stream)
                   provider request)
          (let* ((final-request (context-admission-test-provider-last-request provider))
                 (ceiling       (context-admission-test-provider-last-ceiling provider))
                 (wire          (json-get final-request "max_output_tokens"))
                 (input         (provider-request-input-token-estimate provider final-request)))
            (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                         "exactly one finalized request reaches the inert transport")
            (test-assert (and (plusp ceiling) (= wire ceiling)
                              (<= (+ input (max 256 ceiling)) 5000))
                         "the actual wire ceiling fits including its own serialized overhead")
            (test-assert (> input (provider-request-input-token-estimate provider request))
                         "the cap field is included in the input estimate")
            (test-assert (equal original (json-encode request))
                         "the caller's original request is not changed")
            (unless native-p
              (test-assert (equal (getf observed :wire)
                                  (provider-cache-wire-plan
                                   final-request
                                   :volatile-items (getf observed :volatile-items 0)))
                           "cache observation fingerprints the finalized wire request")))))))
  nil)

(-> test-context-admission-mission-explicit-policy () null)
(defun test-context-admission-mission-explicit-policy ()
  "Never hide oversized caller ceilings or prompt content behind a mission allowance."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                            (make-instance 'context-admission-capped-test-provider
                                                                 :configuration configuration))
             (*provider-mission-output-allowance* 10))
        (dolist (request
                  (list (json-object "max_output_tokens" 6000)
                        (json-object "max_completion_tokens" 6000)
                        (json-object "max_tokens" 6000)
                        (json-object "generationConfig" (json-object "maxOutputTokens" 6000))
                        (json-object "input" (make-string 22000 :initial-element #\x))
                        (json-object "instructions" (make-string 22000 :initial-element #\x))
                        (json-object "tools" (vector (json-object "description"
                                                                   (make-string 22000 :initial-element #\x))))))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider request) nil)
             (provider-context-budget-error () t))
           "original explicit wire policy and all final prompt components retain refusal"))
        (let ((*provider-maximum-output-tokens* 6000))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider (json-object)) nil)
             (provider-context-budget-error () t))
           "an oversized explicit dynamic cap is not silently reduced"))
        (test-assert (zerop (context-admission-test-provider-network-calls provider))
                     "all explicit policy refusals precede transport")
        (let ((*provider-maximum-output-tokens* 8))
          (provider-open-response-stream provider (json-object "max_output_tokens" 6))
          (test-assert (= 6 (context-admission-test-provider-last-ceiling provider))
                       "the effective ceiling is the conjunction of mission, caller and wire limits"))
        (let ((*provider-mission-output-allowance* nil))
          (provider-open-response-stream provider (json-object))
          (test-assert (null (context-admission-test-provider-last-ceiling provider))
                       "ordinary nonmission requests acquire no dynamic output ceiling")
          (test-assert (null (json-get (context-admission-test-provider-last-request provider)
                                     "max_output_tokens"))
                       "ordinary nonmission requests acquire no wire ceiling")))))
  nil)

(-> test-context-admission-mission-copy-protocol () null)
(defun test-context-admission-mission-copy-protocol ()
  "Copy each accepted ceiling field without mutating the original wire policy."
  (with-test-configuration (configuration)
    (dolist (entry
              (list (list (make-instance 'openai-api-provider :configuration configuration)
                          "max_output_tokens")
                    (list (make-instance 'openai-compatible-provider :configuration configuration
                                          :display-name "Inert" :family ':openai-compatible)
                          "max_completion_tokens")
                    (list (make-instance 'anthropic-api-key-provider :configuration configuration)
                          "max_tokens")
                    (list (make-instance 'cl-llm-provider-api:gemini-generate-content-provider)
                          "maxOutputTokens")))
      (destructuring-bind (provider field) entry
        (let* ((nested-p   (string= field "maxOutputTokens"))
               (generation (json-object "temperature" 0.5 "maxOutputTokens" 20))
               (request    (if nested-p
                               (json-object "generationConfig" generation "model" "inert")
                               (json-object field 20 "model" "inert")))
               (before     (json-encode request))
               (copy       (provider-request-with-output-ceiling provider request 10)))
          (test-assert (= 10 (json-get (if nested-p (json-get copy "generationConfig") copy)
                                      field))
                       "the accepted provider field carries the reduced ceiling")
          (test-assert (and (not (eq request copy))
                            (equal before (json-encode request))
                            (equal "inert" (json-get copy "model")))
                       "copying preserves the caller's explicit policy and unrelated fields")
          (when nested-p
            (test-assert (and (not (eq generation (json-get copy "generationConfig")))
                              (= 0.5 (json-get (json-get copy "generationConfig") "temperature")))
                         "nested generation settings are copied and preserved"))))))
  nil)

(-> test-context-admission-mission-reviewer () null)
(defun test-context-admission-mission-reviewer ()
  "Conjoin finite mission, reviewer, caller and wire limits with original settlement."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (caller '(nil 8))
        (let* ((provider                         (make-instance 'context-admission-capped-test-provider
                                                                :configuration configuration))
               (application                      (mission-inference-test--application configuration provider))
               (conversation                     (application-conversation application))
               (*provider-maximum-output-tokens* caller))
          (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
          (unwind-protect
               (let* ((context     (mission-context-find conversation))
                      (goal        (mission-context-goal context))
                      (reviewer    (list :turn-limit 3 :token-limit 30 :turns-used 0 :tokens-used 0))
                      (reservation (or caller 10)))
                 (mission--account-inference
                  context
                  (lambda ()
                    (test-assert (and (= reservation (getf goal :tokens-reserved))
                                      (= reservation (getf reviewer :tokens-reserved)))
                                 "both ledgers retain their original minimum reservation")
                    (provider-open-response-stream
                     provider (if caller (json-object "max_output_tokens" 6) (json-object))))
                  (lambda (results)
                    (declare (ignore results))
                    (json-object "input_tokens" 2 "output_tokens" 3))
                  :allowance reviewer)
                 (test-assert (= (if caller 6 10)
                                 (context-admission-test-provider-last-ceiling provider))
                              "small finite allowances and explicit caller policy are conjoined")
                 (dolist (limit (list goal reviewer))
                   (test-assert (and (= 5 (getf limit :tokens-used))
                                     (= 1 (getf limit :turns-used))
                                     (zerop (getf limit :requests-outstanding))
                                     (zerop (getf limit :tokens-reserved)))
                                "each ledger charges usage once and releases its original reservation")))
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-refusal-settlement () null)
(defun test-context-admission-mission-refusal-settlement ()
  "Settle locally refused stream, compaction and reviewer requests as known zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (native-p '(nil t))
      (dolist (reviewer-p '(nil t))
        (with-test-configuration (configuration)
          (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
                 (application  (mission-inference-test--application configuration provider))
                 (conversation (application-conversation application))
                 (reviewer     (when reviewer-p
                                 (list :turn-limit 4 :token-limit 100 :turns-used 0 :tokens-used 0))))
            (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
            (unwind-protect
                 (let* ((context (mission-context-find conversation))
                        (goal    (mission-context-goal context)))
                   (test-assert
                    (handler-case
                        (mission--account-inference
                         context
                         (lambda ()
                           (funcall (if native-p #'provider-open-native-compaction #'provider-open-response-stream)
                                    provider (json-object "max_output_tokens" 6000)))
                         (lambda (results) (declare (ignore results)) nil)
                         :allowance reviewer)
                      (mission-error () t))
                    "the accounted request is refused by ordinary admission")
                   (test-assert (zerop (context-admission-test-provider-network-calls provider))
                                "local refusal never enters the transport")
                   (dolist (limit (remove nil (list goal reviewer)))
                     (test-assert (zerop (getf limit :unknown-usage 0))
                                  "the local refusal has known zero usage")
                     (test-assert (and (zerop (getf limit :tokens-used))
                                       (= 1 (getf limit :turns-used))
                                       (zerop (getf limit :tokens-reserved))
                                       (zerop (getf limit :requests-outstanding)))
                                  "settlement releases once without refunding the admitted turn"))
                   (test-assert (eq ':active (getf goal :status))
                                "known zero usage does not quarantine the mission"))
              (mission-detach application)))))))
  nil)

(-> test-context-admission-mission-refusal-proof () null)
(defun test-context-admission-mission-refusal-proof ()
  "Require admission origin and no earlier admitted request before certifying zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (native-p '(nil t))
      (dolist (mode '(:synthetic :outside-boundary :after-admission :second-refusal :caught-refusal))
        (with-test-configuration (configuration)
          (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
                 (application  (mission-inference-test--application configuration provider))
                 (conversation (application-conversation application))
                 (open         (if native-p #'provider-open-native-compaction #'provider-open-response-stream))
                 (large        (json-object "max_output_tokens" 6000))
                 (entered-p    (member mode '(:after-admission :second-refusal))))
            (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
            (unwind-protect
                 (let* ((context (mission-context-find conversation))
                        (goal    (mission-context-goal context)))
                   (test-assert
                    (handler-case
                        (mission--account-inference
                         context
                         (lambda ()
                           (when entered-p (funcall open provider (json-object)))
                           (case mode
                             (:outside-boundary
                              (provider-check-request-context provider large))
                             (:second-refusal
                              (funcall open provider large))
                             (:caught-refusal
                              (handler-case (funcall open provider large)
                                (provider-context-budget-error () nil))
                              (error "An unrelated later failure."))
                             (t
                              (error 'provider-context-budget-error
                                     :message "Synthetic admission-like failure."
                                     :estimated-input-tokens 6000 :output-reserve 256
                                     :context-window 5000 :non-text-estimate-p nil))))
                         (lambda (results) (declare (ignore results)) nil))
                      (mission-error () t))
                    "the supervised failure propagates")
                   (test-assert (= (if entered-p 1 0)
                                   (context-admission-test-provider-network-calls provider))
                                "the negative control records its actual transport entries")
                   (test-assert (and (= 1 (getf goal :unknown-usage))
                                     (eq ':blocked (getf goal :status)))
                                "a condition name or later local refusal cannot prove zero usage")
                   (test-assert (and (zerop (getf goal :tokens-used))
                                     (= 1 (getf goal :turns-used))
                                     (zerop (getf goal :tokens-reserved))
                                     (zerop (getf goal :requests-outstanding)))
                                "uncertain settlement releases its reservation exactly once"))
              (mission-detach application)))))))
  nil)

(-> test-context-admission-mission-code-assist-envelope () null)
(defun test-context-admission-mission-code-assist-envelope ()
  "Preserve Code Assist's routing envelope and its original inner output policy."
  (with-test-configuration (configuration)
    (let* ((provider   (make-instance 'gemini-code-assist-provider :configuration configuration))
           (inner      (json-object "generationConfig" (json-object "maxOutputTokens" 20
                                                                       "temperature" 0.5)))
           (request    (json-object "request" inner "project" "inert"))
           (before     (json-encode request))
           (copy       (provider-request-with-output-ceiling provider request 10))
           (generation (json-get (json-get copy "request") "generationConfig")))
      (test-assert (and (= 10 (json-get generation "maxOutputTokens"))
                        (= 0.5 (json-get generation "temperature"))
                        (equal "inert" (json-get copy "project")))
                   "the ceiling is encoded inside the copied GenerateContent request")
      (test-assert (and (equal before (json-encode request))
                        (not (eq copy request))
                        (not (eq inner (json-get copy "request"))))
                   "both the routing envelope and inner generation policy are request-local")
      (test-assert (member 20 (provider-request-output-ceilings request :enclosed-p t))
                   "original inner ceilings participate in ordinary admission and conjunction")))
  nil)

(-> test-context-admission-code-assist-first-setup () null)
(defun test-context-admission-code-assist-first-setup ()
  "Admit and observe the actual serialized Code Assist envelope after initial setup."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (allowance (list most-positive-fixnum nil))
        (let* ((provider                            (make-instance 'gemini-code-assist-provider :configuration configuration))
               (request                             (json-object "model" "inert" "project" nil
                                                                 "request" (json-object "contents" #())))
               (before                              (json-encode request))
               (credentials                         (make-instance 'oauth-credentials :access-token "inert"))
               (*provider-mission-output-allowance* allowance)
               (setup-calls                         0)
               (observations                        nil)
               (sent                                nil))
          (test-call-with-function-replacements
           (list
            (list 'gemini-code-assist-ensure-setup
                  (lambda (provider credentials)
                    (declare (ignore credentials))
                    (incf setup-calls)
                    (setf (gemini-code-assist-provider-project provider)
                          "this-is-a-project-id-discovered-during-first-setup"
                          (gemini-code-assist-provider-setup-complete-p provider) t)
                    provider))
            (list 'provider-cache-prefix-observe
                  (lambda (request)
                    (push (json-encode request) observations)))
            (list 'provider-post-event-stream
                  (lambda (url body &rest options)
                    (declare (ignore url options))
                    (setf sent (json-decode (babel:octets-to-string body :encoding :utf-8)))
                    (values nil 200 nil))))
           (lambda ()
             (provider-open-response-stream provider request :credentials credentials)))
          (test-assert (json-object-p sent) "the actual primary method serialized one request")
          (let* ((input      (provider-request-input-token-estimate provider sent))
                 (generation (json-get (json-get sent "request") "generationConfig"))
                 (cap        (and generation (json-get generation "maxOutputTokens"))))
            (format t "~&Code Assist first setup: input=~D cap=~S total=~D window=5000~%"
                    input cap (+ input (max 256 (or cap 0))))
            (test-assert (<= (+ input (max 256 (or cap 0))) 5000)
                         "the actual serialized input and output headroom fit the window")
            (test-assert (if allowance (and cap (plusp cap)) (null cap))
                         "only a mission allowance adds a wire ceiling"))
          (test-assert (equal (json-get sent "project")
                              "this-is-a-project-id-discovered-during-first-setup")
                       "the initial nil project is finalized before transport")
          (test-assert (and (= 1 setup-calls) (= 1 (length observations)))
                       "setup and cache observation each run exactly once")
          (test-assert (equal (first observations) (json-encode sent))
                       "cache observation sees exactly the serialized final envelope")
          (test-assert (equal before (json-encode request))
                       "neither routing setup nor mission headroom mutates the original request")))))
  nil)

(defclass context-admission-nonmonotonic-test-provider (context-admission-capped-test-provider)
  ()
  (:documentation "An inert transport with deliberately nonmonotonic cap-sensitive estimates."))

(defmethod provider-request-input-token-estimate
    ((provider context-admission-nonmonotonic-test-provider) (request hash-table))
  "Make a smaller serialized ceiling increase estimated input beyond the window."
  (declare (ignore provider))
  (let ((ceiling (json-get request "max_output_tokens")))
    (values (cond ((null ceiling)
                   100)
                  ((> ceiling 1000)
                   4500)
                  (t
                   6000))
            nil)))

(-> test-context-admission-mission-final-recheck () null)
(defun test-context-admission-mission-final-recheck ()
  "Fail closed when a custom estimator invalidates a reduced or provisional candidate."
  (dolist (window '("5000" "1000"))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" window)
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let ((provider                            (make-instance 'context-admission-nonmonotonic-test-provider
                                                                 :configuration configuration))
              (*provider-mission-output-allowance* most-positive-fixnum))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider (json-object)) nil)
             (provider-context-budget-error () t))
           "a nonpositive envelope or an oversized final copy is refused")
          (test-assert (zerop (context-admission-test-provider-network-calls provider))
                       "no candidate bypasses final context admission")))))
  nil)

(defclass context-admission-child-test-provider (context-admission-capped-test-provider)
  ((allocation :initform nil :accessor context-admission-child-test-allocation
               :documentation "Full mission admission before final request projection.")
   (caller-cap :initform nil :accessor context-admission-child-test-caller-cap
               :documentation "Explicit caller policy before transport admission."))
  (:documentation "Fail a real child after observing its final encoded request."))

(defmethod provider-with-configuration
    ((provider context-admission-child-test-provider) (configuration configuration))
  "Retain the inert transport and observations in the native child's provider."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
    ((provider context-admission-child-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Admit the real request into an inert transport, then fail without usage."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (setf (context-admission-child-test-allocation provider) *provider-mission-output-allowance*
        (context-admission-child-test-caller-cap provider) *provider-maximum-output-tokens*)
  (provider-open-response-stream provider (provider-request-object provider conversation #())
                                 :conversation conversation)
  (error "Synthetic failure after final encoded admission without usage."))

(-> context-admission-test--bounded-child () null)
(defun context-admission-test--bounded-child ()
  "Distinguish full child allocation, lowered wire cap and durable measured totals."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "272000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((provider     (make-instance 'context-admission-child-test-provider
                                          :configuration configuration))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application))
             (registry     (task-augment-tool-registry (application-tool-registry application)))
             (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
             (definition   (task-agent-definition-create
                            :name "admission-child" :description "Exercise admission and settlement"
                            :instructions "Attempt one request." :tools ':all :source ':test))
             (entry        (list :definition definition :detached t
                                 :item (list :agent "admission-child" :task "Attempt one request.")))
             (job          nil))
        (setf (agent-provider (application-agent application)) provider)
        (application-mission-start application
                                   (mission-test--specification :tokens most-positive-fixnum))
        (let* ((goal    (application-goal application))
               (context (mission-context-find conversation)))
          (unwind-protect
               (progn
                 (setf job (first (task-orchestrator-start-jobs
                                   orchestrator (application-agent application) (list entry))))
                 (task-job-await job nil)
                 (let* ((allocation (context-admission-child-test-allocation provider))
                        (ceiling    (context-admission-test-provider-last-ceiling provider))
                        (request    (context-admission-test-provider-last-request provider))
                        (result     (getf (task-job-snapshot job) :result))
                        (summary    (list :unknown-usage 1 :reservation-charged-tokens allocation
                                          :reservation-charge-requests 1)))
                   (test-assert (and (null (context-admission-child-test-caller-cap provider))
                                     (typep allocation '(integer 1))
                                     (typep ceiling '(integer 1)) (< ceiling allocation)
                                     (= ceiling (json-get request "max_output_tokens"))
                                     (= 1 (context-admission-test-provider-network-calls provider)))
                                "no caller ceiling: one actual final encoded request has 0<W<R")
                   (test-assert (<= (+ (provider-request-input-token-estimate provider request) ceiling) 272000)
                                "the final encoded request fits the window, independently of full allocation")
                   (test-assert (and (eq ':failed (getf result :status)) (getf result :error)
                                     (null (getf result :usage)) (= 1 (getf result :unknown-usage 0))
                                     (= allocation (getf result :reservation-charged-tokens 0))
                                     (= 1 (getf result :reservation-charge-requests 0)))
                                "the actual child retains failure and UNKNOWN audit with one full-R charge")
                   (test-assert (and (eq goal (application-goal application))
                                     (eq ':active (getf goal :status))
                                     (= allocation (getf goal :tokens-used))
                                     (= allocation (getf goal :reservation-charged-tokens 0))
                                     (zerop (getf goal :unknown-usage))
                                     (zerop (getf goal :requests-outstanding))
                                     (zerop (getf goal :tokens-reserved)))
                                "settlement releases R once without shared quarantine or replacing the goal")
                   (test-assert (equal summary (mission-task-job-accounting-summary job))
                                "the locked child summary separates allocation charge from measured usage")
                   (test-assert (every #'zerop (subseq (conversation-usage conversation) 0 5))
                                "an allocation charge is absent from running measured totals")
                   (mission--account-inference
                    context (lambda () (json-object "input_tokens" 2 "output_tokens" 3)) #'first)
                   (conversation-append-provider-metadata
                    conversation '(:usage (("input_tokens" 2) ("output_tokens" 3) ("total_tokens" 5))))
                   (test-assert (and (eq goal (application-goal application))
                                     (eq ':active (getf goal :status))
                                     (= (+ allocation 5) (getf goal :tokens-used))
                                     (= allocation (getf goal :reservation-charged-tokens))
                                     (equal '(2 3 5 0 0 nil) (conversation-usage conversation)))
                                "subsequent known-usage work is admitted and only measured usage enters totals")
                   (task-job-await job nil)
                   (test-assert (and (= 1 (context-admission-test-provider-network-calls provider))
                                     (= (+ allocation 5) (getf goal :tokens-used))
                                     (equal summary (mission-task-job-accounting-summary job)))
                                "duplicate completion observation makes no request or second charge")
                   (application--record-goal application)
                   (conversation-append-summary conversation "Synthetic compaction after bounded failure.")
                   (let* ((loaded   (conversation-load (conversation-pathname conversation)))
                          (restored (progn (application--load-goal application)
                                           (application-goal application))))
                     (format t "~&MEASURED-COMPAT ~S ~S~%"
                             (conversation-usage conversation) (conversation-usage loaded))
                     (test-assert (equal '(2 3 5 0 0 nil) (conversation-usage loaded))
                                  "compaction and reload preserve measured-only running totals")
                     (test-assert (and (= (+ allocation 5) (getf restored :tokens-used))
                                       (= allocation (getf restored :reservation-charged-tokens))
                                       (= 1 (rest (assoc (job-identifier job)
                                                        (getf restored :unknown-usage-by-child) :test #'equal)))
                                       (equal (getf goal :reservation-charges-by-child)
                                              (getf restored :reservation-charges-by-child))
                                       (zerop (getf restored :unknown-usage))
                                       (eq ':active (getf restored :status)))
                                  "goal append, compaction and reload retain full allocation and child audit"))))
            (when job (task-job-await job nil))
            (task-orchestrator-close orchestrator)
            (mission-detach application))))))
  nil)
