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

(defclass context-admission-transport-fields-test-provider (context-admission-test-provider)
  ()
  (:documentation "A provider that adds transport-only fields after encoding, as routing providers do."))

(defmethod provider-request-object :around
    ((provider context-admission-transport-fields-test-provider)
     (conversation conversation) (tool-namespaces vector)
     &key goal-context compaction-p)
  "Add fields after encoding that the wire encoder does not reproduce."
  (declare (ignore goal-context compaction-p))
  (let ((request (call-next-method)))
    (setf (gethash "model" request) "transport/model"
          (gethash "session_id" request) "transport-session")
    request))

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
        (progn
          (test-assert (null (provider-check-request-context provider (json-object)))
                       "a rejected request does not poison a later small request")
          (let* ((opaque-provider (make-instance 'cl-llm-provider-api:subscription-provider :configuration configuration))
                 (opaque          (json-object "request" (json-object "input" (make-string 24000 :initial-element #\x))
                                               "messages" #("outer"))))
            (test-assert
             (handler-case (progn (provider-check-request-context opaque-provider opaque) nil)
               (provider-context-budget-error (condition)
                 (> (provider-context-budget-error-estimated-input-tokens condition) 5000)))
             "opaque nested request data participates in admission instead of being guessed as an envelope")
            (let ((plan (provider-cache-wire-plan opaque-provider opaque)))
              (test-assert (and (null (getf plan :items))
                                (assoc "request" (getf plan :fields) :test #'equal)
                                (assoc "messages" (getf plan :fields) :test #'equal))
                           "unknown transports retain opaque fields without assumed history")))))))
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
           (conversation-append-summary conversation (make-continuation :intent "small checkpoint"))
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
        (conversation-append-summary
         conversation
         (make-continuation :intent     "Continue."
                            :next       (list :text "Read the record." :refs '("r"))
                            :references (list (list :handle "r" :kind ':message :label large))))
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
        (test-assert (and uncertain-p (= short (provider-image-token-allowance)))
                     "image token accounting is explicitly approximate")
        (multiple-value-bind (encoded encoded-uncertain-p)
            (provider-request-input-token-estimate
             provider (json-object "type" "input_image" "image_url"
                                   (concatenate 'string "data:image/png;base64,"
                                                (make-string 100000 :initial-element #\a))))
          (test-assert (and encoded-uncertain-p (= short encoded))
                       "base64 wire size is not interpreted as text tokens")))))
  nil)

(-> context-admission-test--tool-output (t) hash-table)
(defun context-admission-test--tool-output (output)
  "Return a Codex function_call_output history item carrying OUTPUT."
  (json-object "type" "function_call_output" "call_id" "image-call" "output" output))

(-> context-admission-test--tool-image (string) hash-table)
(defun context-admission-test--tool-image (url)
  "Return an input_image part at URL as Autolith encodes attached tool results."
  (json-object "type" "input_image" "image_url" url "detail" "high"))

(-> test-context-admission-tool-output-images () null)
(defun test-context-admission-tool-output-images ()
  "Charge direct tool-output image parts the image allowance in full and measured estimates."
  (let ((short-url "https://example.invalid/image")
        (long-url  (concatenate 'string "data:image/png;base64,"
                                (make-string 100000 :initial-element #\a))))
    (flet ((direct (url)
             (context-admission-test--tool-output
              (vector (json-object "type" "input_text" "text" "screenshot")
                      (context-admission-test--tool-image url))))
           (nested (url)
             (context-admission-test--tool-output
              (vector (json-object "payload" (context-admission-test--tool-image url))))))
      (with-test-configuration (configuration)
        (let ((provider (make-instance 'context-admission-test-provider :configuration configuration)))
          (flet ((estimate (item)
                   (provider-request-input-token-estimate provider (json-object "input" (vector item)))))
            (multiple-value-bind (short uncertain-p) (estimate (direct short-url))
              (test-assert (and uncertain-p (>= short (provider-image-token-allowance)))
                           "a direct tool-output image receives the image allowance")
              (test-assert (= short (estimate (direct long-url)))
                           "tool-output image URL size does not replace the allowance"))
            (test-assert (< (estimate (nested short-url)) (provider-image-token-allowance))
                         "image-shaped tool data is ordinary JSON")
            (test-assert (>= (estimate (nested long-url)) (floor (length long-url) 4))
                         "image-shaped tool data is counted by its encoded bytes"))))
      (context-admission-test--with-measured-context
       (lambda (agent)
         (let ((conversation (agent-conversation agent))
               (provider     (agent-provider agent)))
           (context-admission-test--measure agent)
           (flet ((estimate (item)
                    (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
                           (request                    (provider-request-object provider conversation #())))
                      (setf (gethash "input" request)
                            (concatenate 'vector (json-get request "input") (vector item)))
                      (multiple-value-list (provider-request-input-token-estimate provider request)))))
             (destructuring-bind (short uncertain-p) (estimate (direct short-url))
               (test-assert (and uncertain-p (> short (provider-image-token-allowance)))
                            "a measured suffix charges a direct tool-output image the allowance")
               (test-assert (= short (first (estimate (direct long-url))))
                            "a measured suffix ignores tool-output image URL size"))
             (test-assert (< (first (estimate (nested short-url))) (provider-image-token-allowance))
                          "a measured suffix counts image-shaped tool data as JSON")
             (test-assert (>= (first (estimate (nested long-url))) (floor (length long-url) 4))
                          "a measured suffix counts image-shaped tool data by its bytes")))))))
  nil)

(-> test-context-admission-tool-output-image-edges () null)
(defun test-context-admission-tool-output-image-edges ()
  "Charge only direct Responses tool-output images, once, on every estimate path."
  (let* ((data-url (concatenate 'string "data:image/png;base64,"
                                (make-string 100000 :initial-element #\a)))
         (image    (context-admission-test--tool-image data-url))
         (text     (json-object "type" "input_text" "text" "screenshot")))
    (with-test-configuration (configuration)
      (let ((provider (make-instance 'context-admission-test-provider :configuration configuration)))
        (labels ((ours (value)
                   (multiple-value-list (provider-context--request-characters provider value)))

                 (library (value)
                   (multiple-value-list
                    (provider-request-characters provider value
                                                 :image-characters (* 4 (provider-image-token-allowance)))))

                 (as-content (parts)
                   ;; The library's declared Responses image location; "content" is one character longer.
                   (vector (json-object "type" "function_call_output" "call_id" "image-call" "content" parts)))

                 (matches-content-p (parts)
                   (destructuring-bind (characters uncertain-p)
                       (ours (vector (context-admission-test--tool-output parts)))
                     (destructuring-bind (expected expected-uncertain-p) (library (as-content parts))
                       (and (= characters (1- expected)) (eq uncertain-p expected-uncertain-p))))))
          (dolist (output (list "plain string output" (vector text)))
            (let ((items (vector (context-admission-test--tool-output output))))
              (test-assert (equal (ours items) (library items))
                           "tool output without images is counted as before")))
          (test-assert (matches-content-p (vector image text image))
                       "several direct images each receive the content-array allowance")
          (test-assert (matches-content-p (vector image (json-object "payload" image)))
                       "a nested copy of a direct image is still ordinary JSON")
          (let ((items (vector (context-admission-test--tool-output
                                (vector (json-object "type" "image_url" "image_url" data-url))))))
            (test-assert (equal (ours items) (library items))
                         "another protocol's image type is ordinary Responses JSON"))
          (let* ((history (vector (json-object "type" "message" "role" "user" "content" "hello")
                                  (context-admission-test--tool-output "text")))
                 (request (json-object "input" history)))
            (test-assert (and (eq history (provider-context--without-tool-output-images provider history))
                              (eq request (provider-context--without-tool-output-images provider request)))
                         "histories without tool-output images are not copied"))
          (let* ((item    (context-admission-test--tool-output (vector text image)))
                 (request (json-object "input" (vector item))))
            (ours request)
            (test-assert (eq image (aref (json-get item "output") 1))
                         "estimating leaves the encoded request unchanged"))
          (let ((plain (make-instance 'model-provider))
                (items (vector (context-admission-test--tool-output (vector text image)))))
            (test-assert (equal (multiple-value-list (provider-context--request-characters plain items))
                                (multiple-value-list
                                 (provider-request-characters plain items
                                                              :image-characters (* 4 (provider-image-token-allowance)))))
                         "a transport that is not Responses keeps the library count")))))
    (context-admission-test--with-measured-context
     (lambda (agent)
       (let ((conversation (agent-conversation agent))
             (provider     (agent-provider agent)))
         (test-call-with-function-replacements
          ;; Admit requests carrying image allowances; this test measures, it does not refuse.
          (list (list 'provider-effective-context-window
                      (lambda (provider) (declare (ignore provider)) 1000000)))
          (lambda ()
            (conversation-append-provider-item
             conversation (json-object "type" "function_call" "call_id" "image-call"
                                       "name" "test" "arguments" "{}"))
            (conversation-append-provider-item
             conversation (context-admission-test--tool-output (vector text image)))
            (context-admission-test--measure agent)
            (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
                   (request                    (provider-request-object provider conversation #())))
              (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                           "a measured tool-output image is not charged again")
              (setf (gethash "input" request)
                    (concatenate 'vector (json-get request "input")
                                 (vector (context-admission-test--tool-output (vector image)))))
              (provider-open-response-stream provider request :conversation conversation)
              (test-assert (= (getf *provider-context-request* :wire-characters)
                              (provider-context--request-characters
                               provider (provider-request-history provider request)))
                           "recorded wire size uses the same image charge as the estimates")
              (test-assert (> (getf *provider-context-request* :wire-characters)
                              (* 8 (provider-image-token-allowance)))
                           "recorded wire size charges both tool-output images"))))))))
  nil)

(-> context-admission-test--image-request ((integer 0)) hash-table)
(defun context-admission-test--image-request (count)
  "Return a request whose single user message carries COUNT image parts."
  (json-object
   "input"
   (vector
    (json-object "type" "message" "role" "user" "content"
                 (coerce (loop repeat count
                               collect (json-object "type" "input_image"
                                                    "image_url" "https://example.invalid/image"
                                                    "detail" "high"))
                         'vector)))))

(-> test-context-admission-prepared-images () null)
(defun test-context-admission-prepared-images ()
  "Charge each image at the local preparation cap rather than a flat larger allowance."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "40000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let ((provider (make-instance 'context-admission-test-provider
                                     :configuration configuration))
            (cap      *image-input-maximum-patches*))
        (labels ((image-tokens (count)
                   (- (provider-request-input-token-estimate
                       provider (context-admission-test--image-request count))
                      (provider-request-input-token-estimate
                       provider (context-admission-test--image-request 0))))

                 (charged-p (count allowance)
                   ;; Allow one token per image for array delimiters and rounding.
                   (<= (* count allowance) (image-tokens count) (* count (1+ allowance)))))
          (test-assert (charged-p 1 cap)
                       "one image is charged at the preparation patch cap")
          (test-assert (charged-p 4 cap)
                       "several images are each charged at the preparation cap")
          (let ((*image-input-maximum-patches* 1200))
            (test-assert (charged-p 1 1200)
                         "the allowance follows a rebound preparation cap"))
          (let ((*provider-image-token-estimate* 9000))
            (test-assert (charged-p 1 9000)
                         "an explicit per-image allowance still takes precedence"))
          (provider-open-response-stream provider (context-admission-test--image-request 4))
          (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                       "four prepared images fit a window the flat allowance would refuse")))))
  nil)

(-> test-context-admission-prepared-image-bounds () null)
(defun test-context-admission-prepared-image-bounds ()
  "Prepared images at, under and over the patch cap never exceed the charged allowance."
  (flet ((prepared-patches (width height)
           (multiple-value-bind (format target-width target-height)
               (apply #'yolokuva:image-preparation :png width height (image-input--limits))
             (declare (ignore format))
             (values (* (ceiling target-width *image-input-patch-size*)
                        (ceiling target-height *image-input-patch-size*))
                     target-width target-height))))
    (multiple-value-bind (patches width height) (prepared-patches 1600 1600)
      (test-assert (and (= patches *image-input-maximum-patches*) (= width 1600) (= height 1600))
                   "an image exactly at the patch cap is kept and charged in full"))
    (multiple-value-bind (patches width) (prepared-patches 1600 1568)
      (test-assert (and (< patches *image-input-maximum-patches*) (= width 1600))
                   "an image just under the patch cap is kept"))
    (dolist (size '((1601 1600) (1632 1600) (2048 2048) (4000 3000) (8000 100) (100 8000)))
      (multiple-value-bind (patches width height) (apply #'prepared-patches size)
        (test-assert (and (<= patches (provider-image-token-allowance))
                          (<= (max width height) *image-input-maximum-dimension*))
                     (format nil "a ~{~D~^x~} image is reduced to the charged cap" size))))
    (test-assert (= 1 (prepared-patches 1 1))
                 "a tiny image still fits the allowance"))
  nil)

(-> test-context-admission-checkpoint-transaction () null)
(defun test-context-admission-checkpoint-transaction ()
  "Reject oversized final checkpoints before changing durable or projected history."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'context-admission-test-provider
                                          :configuration configuration)))
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
                     (conversation-append-summary
                      conversation (make-continuation :intent "Small handoff.") :compaction capture
                      :validate-projection                                                  #'validate)
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
               "reload sees no rejected checkpoint")))))))
  (context-admission-test--with-measured-context
   (lambda (agent)
     (context-admission-test--measure agent :envelope-length 64)
     (let* ((conversation (agent-conversation agent))
            (previous     (copy-tree (conversation-context-usage-sample conversation)))
            (append       #'conversation-append-record))
       (test-call-with-function-replacements
        (list (list 'system-prompt
                    (lambda (configuration) (declare (ignore configuration)) "small policy č")))
        (lambda ()
          (test-call-with-function-replacements
           (list (list 'conversation-append-record
                       (lambda (conversation record)
                         (when (eq (first record) ':context-usage)
                           (error 'file-error :pathname (conversation-pathname conversation)))
                         (funcall append conversation record))))
           (lambda ()
             (test-assert (handler-case
                              (progn (context-admission-test--measure agent :envelope-length 64) nil)
                            (file-error () t))
                          "a failed measured-sample append propagates")))
          (test-assert (equal previous (conversation-context-usage-sample conversation))
                       "failed publication preserves the live measured anchor")
          (test-assert (equal previous (conversation-context-usage-sample
                                        (conversation-load (conversation-pathname conversation))))
                       "failed publication preserves the durable measured anchor")
          (context-admission-test--measure agent :envelope-length 64)
          (let* ((records (conversation--read-records (conversation-pathname conversation)))
                 (sample  (getf (rest (first (last records))) :sample)))
            (test-assert (equal '("small policy č" "[]") (getf sample :header-text))
                         "retry publishes a self-contained changed-header snapshot")))))))
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

(-> test-context-admission-mission-wire-ceiling () null)
(defun test-context-admission-mission-wire-ceiling ()
  "Measure serialized cap overhead and finalize ordinary and compaction requests."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                            (make-instance 'context-admission-capped-test-provider
                                                                 :configuration configuration))
             (request                             (json-object "input" (make-string 1000 :initial-element #\x)))
             (original                            (json-encode request))
             (*provider-mission-output-allowance* most-positive-fixnum)
             (observed                            nil)
             (*provider-cache-request-prefix*     '(:fixture t))
             (*provider-cache-prefix-function*
               (lambda (prefix) (setf observed prefix))))
        (provider-open-response-stream provider request)
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
          (test-assert (equal (getf observed :wire)
                              (provider-cache-wire-plan provider final-request :volatile-items (getf observed :volatile-items 0)))
                       "cache observation fingerprints the finalized wire request")))))
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
  "Settle locally refused stream and reviewer requests as known zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (reviewer-p '(nil t))
      (with-test-configuration (configuration)
        (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
               (application  (mission-inference-test--application configuration provider))
               (conversation (application-conversation application))
               (reviewer     (when reviewer-p
                               (list :turn-limit 4 :token-limit 100 :turns-used 0 :tokens-used 0))))
          (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
          (unwind-protect
               (let* ((context          (mission-context-find conversation))
                      (goal             (mission-context-goal context))
                      (original-refusal nil))
                 (test-assert
                  (handler-case
                      (mission--account-inference
                       context
                       (lambda ()
                         (handler-bind ((provider-context-budget-error
                                          (lambda (condition) (setf original-refusal condition))))
                           (provider-open-response-stream provider (json-object "max_output_tokens" 6000))))
                       (lambda (results) (declare (ignore results)) nil)
                       :allowance reviewer)
                    (error (condition)
                      (and (typep condition 'provider-context-budget-error)
                           (eq condition original-refusal)
                           (= 6000 (provider-context-budget-error-output-reserve condition))
                           (= 5000 (provider-context-budget-error-context-window condition))
                           (plusp (provider-context-budget-error-estimated-input-tokens condition)))))
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
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-refusal-proof () null)
(defun test-context-admission-mission-refusal-proof ()
  "Require admission origin and no earlier admitted request before certifying zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (mode '(:synthetic :outside-boundary :after-admission :second-refusal :caught-refusal))
      (with-test-configuration (configuration)
        (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
               (application  (mission-inference-test--application configuration provider))
               (conversation (application-conversation application))
               (open         #'provider-open-response-stream)
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
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-code-assist-envelope () null)
(defun test-context-admission-mission-code-assist-envelope ()
  "Preserve Code Assist's routing envelope and its original inner output policy."
  (with-test-configuration (configuration)
    (let* ((provider   (make-instance 'gemini-code-assist-provider :configuration configuration))
           (inner      (json-object "generationConfig" (json-object "maxOutputTokens" 20 "temperature" 0.5)
                                    "contents" (vector (json-object "role" "user" "parts" (vector (json-object "text" "hello"))))))
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
      (progn
        (test-assert (member 20 (provider-request-output-ceilings provider request))
                     "original inner ceilings participate in ordinary admission and conjunction")
        (test-assert (equal (provider-cache-wire-plan provider request)
                            (provider-cache-wire-plan (make-instance 'cl-llm-provider-api:gemini-generate-content-provider) inner))
                     "the declared envelope uses the same history and cache analysis as plain GenerateContent")
        (test-assert (equal (provider-context--wire-plan provider request)
                            (provider-context--wire-plan provider copy))
                     "an inner output-only change preserves the measured-input fingerprint")
        (test-assert (= 1 (length (getf (provider-cache-wire-plan provider request) :items)))
                     "Code Assist history is declared through the library protocol"))))
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
                  (lambda (provider request) (declare (ignore provider)) (push (json-encode request) observations)))
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
      (let* ((provider
              (make-instance 'context-admission-child-test-provider :configuration
                             configuration))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application))
             (registry
              (task-augment-tool-registry (application-tool-registry application)))
             (orchestrator
              (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
             (definition   nil)
             (entry
              (list :detached t :item
                    (append (list :task "Attempt one request.")
                            definition)))
             (job          nil))
        (setf (agent-provider (application-agent application)) provider)
        (application-mission-start application
                                   (mission-test--specification :tokens
                                                                most-positive-fixnum))
        (let* ((goal    (application-goal application))
               (context (mission-context-find conversation)))
          (unwind-protect
              (progn
               (setf job
                       (first
                        (task-orchestrator-start-jobs orchestrator
                                                      (application-agent application)
                                                      (list entry))))
               (task-job-await job nil)
               (let* ((allocation (context-admission-child-test-allocation provider))
                      (ceiling    (context-admission-test-provider-last-ceiling provider))
                      (request    (context-admission-test-provider-last-request provider))
                      (result     (getf (task-job-snapshot job) :result))
                      (summary
                       (list :unknown-usage 1 :reservation-charged-tokens allocation
                             :reservation-charge-requests 1)))
                 (test-assert
                  (and (null (context-admission-child-test-caller-cap provider))
                       (typep allocation '(integer 1)) (typep ceiling '(integer 1))
                       (< ceiling allocation)
                       (= ceiling (json-get request "max_output_tokens"))
                       (= 1 (context-admission-test-provider-network-calls provider)))
                  "no caller ceiling: one actual final encoded request has 0<W<R")
                 (test-assert
                  (<=
                   (+ (provider-request-input-token-estimate provider request) ceiling)
                   272000)
                  "the final encoded request fits the window, independently of full allocation")
                 (test-assert
                  (and (eq ':failed (getf result :status)) (getf result :error)
                       (null (getf result :usage)) (= 1 (getf result :unknown-usage 0))
                       (= allocation (getf result :reservation-charged-tokens 0))
                       (= 1 (getf result :reservation-charge-requests 0)))
                  "the actual child retains failure and UNKNOWN audit with one full-R charge")
                 (test-assert
                  (and (eq goal (application-goal application))
                       (eq ':active (getf goal :status))
                       (= allocation (getf goal :tokens-used))
                       (= allocation (getf goal :reservation-charged-tokens 0))
                       (zerop (getf goal :unknown-usage))
                       (zerop (getf goal :requests-outstanding))
                       (zerop (getf goal :tokens-reserved)))
                  "settlement releases R once without shared quarantine or replacing the goal")
                 (test-assert (equal summary (mission-task-job-accounting-summary job))
                              "the locked child summary separates allocation charge from measured usage")
                 (test-assert
                  (every #'zerop (subseq (conversation-usage conversation) 0 5))
                  "an allocation charge is absent from running measured totals")
                 (mission--account-inference context
                                             (lambda ()
                                               (json-object "input_tokens" 2
                                                            "output_tokens" 3))
                                             #'first)
                 (conversation-append-provider-metadata conversation
                                                        '(:usage
                                                          (("input_tokens" 2)
                                                           ("output_tokens" 3)
                                                           ("total_tokens" 5))))
                 (test-assert
                  (and (eq goal (application-goal application))
                       (eq ':active (getf goal :status))
                       (= (+ allocation 5) (getf goal :tokens-used))
                       (= allocation (getf goal :reservation-charged-tokens))
                       (equal '(2 3 5 0 0 nil) (conversation-usage conversation)))
                  "subsequent known-usage work is admitted and only measured usage enters totals")
                 (task-job-await job nil)
                 (test-assert
                  (and (= 1 (context-admission-test-provider-network-calls provider))
                       (= (+ allocation 5) (getf goal :tokens-used))
                       (equal summary (mission-task-job-accounting-summary job)))
                  "duplicate completion observation makes no request or second charge")
                 (application--record-goal application)
                 (conversation-append-summary conversation
                                              (make-continuation :intent
                                                                 "Synthetic compaction after bounded failure."))
                 (let* ((loaded (conversation-load (conversation-pathname conversation)))
                        (restored
                         (progn
                          (application--load-goal application)
                          (application-goal application))))
                   (test-assert (equal '(2 3 5 0 0 nil) (conversation-usage loaded))
                                "compaction and reload preserve measured-only running totals")
                   (test-assert
                    (and (= (+ allocation 5) (getf restored :tokens-used))
                         (= allocation (getf restored :reservation-charged-tokens))
                         (= 1
                            (rest
                             (assoc (job-identifier job)
                                    (getf restored :unknown-usage-by-child) :test
                                    #'equal)))
                         (equal (getf goal :reservation-charges-by-child)
                                (getf restored :reservation-charges-by-child))
                         (zerop (getf restored :unknown-usage))
                         (eq ':active (getf restored :status)))
                    "goal append, compaction and reload retain full allocation and child audit"))))
            (when job (task-job-await job nil))
            (task-orchestrator-close orchestrator)
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-handoff-reasons () null)
(defun test-context-admission-mission-handoff-reasons ()
  "Keep cancellation and deadline supervisor failures despite retained refusal proof."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (reason '(:aborted :exhausted :gate-timeout))
      (with-test-configuration (configuration)
        (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
               (application  (mission-inference-test--application configuration provider))
               (conversation (application-conversation application))
               (failure      (make-condition 'mission-error :reason reason :message "Supervisor stopped.")))
          (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
          (unwind-protect
               (test-call-with-function-replacements
                (list (list 'mission--supervise
                            (lambda (context label function)
                              (declare (ignore context label))
                              (handler-case (funcall function)
                                (provider-context-budget-error () (error failure))))))
                (lambda ()
                  (let ((context (mission-context-find conversation)))
                    (test-assert
                     (handler-case
                         (mission--account-inference
                          context
                          (lambda () (provider-open-response-stream
                                      provider (json-object "max_output_tokens" 6000)))
                          (lambda (results) (declare (ignore results)) nil))
                       (mission-error (condition) (eq condition failure)))
                     "nonfailure supervisor reasons preserve their original condition")
                    (test-assert
                     (and (zerop (context-admission-test-provider-network-calls provider))
                          (zerop (getf (mission-context-goal context) :unknown-usage 0))
                          (zerop (getf (mission-context-goal context) :requests-outstanding)))
                     "the handoff guard does not change d0 settlement"))))
            (mission-detach application))))))
  nil)


;;;; -- Measured Ordinary Context --

(-> context-admission-test--with-measured-context (function &key (:provider-class symbol)) t)
(defun context-admission-test--with-measured-context (function &key (provider-class 'context-admission-test-provider))
  "Run FUNCTION with real request encoding, small policy and an inert transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance provider-class :configuration configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (test-call-with-function-replacements
         (list (list 'system-prompt
                     (lambda (configuration) (declare (ignore configuration)) "small policy"))
               (list 'request-context-session-state
                     (lambda (configuration &key hurry-up-p)
                       (declare (ignore configuration hurry-up-p)) ""))
               (list 'context-resolve-request
                     (lambda (configuration conversation tools &key goal-context compaction-p)
                       (declare (ignore configuration conversation tools goal-context compaction-p)) nil)))
         (lambda () (funcall function agent)))))))

(-> context-admission-test--measure
    (agent &key (:usage t) (:envelope-length (integer 1)) (:tools vector)
           (:during-request (option function)) (:goal-context (option string))) null)
(defun context-admission-test--measure
    (agent &key (usage (json-object "input_tokens" 1000 "output_tokens" 200
                                   "total_tokens" 1200 "cached_input_tokens" 900))
         (envelope-length 30000) (tools #()) during-request goal-context)
  "Admit a real request, persist opaque reasoning and anchor its ordinary usage."
  (let* ((provider                   (agent-provider agent))
         (conversation               (agent-conversation agent))
         (context                    (provider-context-request-create provider conversation tools))
         (*provider-context-request* context)
         (request                    (provider-request-object provider conversation tools
                                                              :goal-context goal-context))
         (result                     (make-instance 'provider-result
                                                    :output-items
                                                    (list (json-object "type" "reasoning" "summary" #()
                                                                       "encrypted_content"
                                                                       (make-string envelope-length :initial-element #\x)))
                                                    :usage usage)))
    (provider-open-response-stream provider request :conversation conversation)
    (when during-request (funcall during-request conversation))
    (agent--persist-provider-result agent result :request-number 1)
    (provider-context-note-response context result))
  nil)

(-> test-context-admission-measured-reasoning () null)
(defun test-context-admission-measured-reasoning ()
  "Use full ordinary usage for retained reasoning, charging output once and new input now."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (conversation-append-user-message conversation "Before the measured request.")
       (context-admission-test--measure agent)
       (test-assert (conversation-context-usage-sample conversation) "ordinary usage has a projection anchor")
       (test-assert (not (agent-should-compact-p agent)) "opaque replay does not inflate the soft estimate")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                      "input includes cached tokens and output is carried exactly once")
         (provider-open-response-stream provider request :conversation conversation)
         (let ((*provider-context-request* nil))
           (test-assert (> (provider-request-input-token-estimate provider request) 5000)
                        "unmeasured opaque history keeps its uncertain fallback"))
         (test-assert
          (handler-case
              (progn
                (setf (gethash "max_output_tokens" request) 3900)
                (provider-open-response-stream provider request :conversation conversation) nil)
            (provider-context-budget-error (condition)
              (= 3900 (provider-context-budget-error-output-reserve condition))))
          "measured input plus an explicit output cap is refused before transport")
         (test-assert (= 2 (context-admission-test-provider-network-calls provider))
                      "only the two admitted requests reached transport"))
       (conversation-append-provider-item
        conversation (json-object "type" "function_call" "name" "test" "call_id" "new-local-evidence"
                                  "arguments" "{}"))
       (conversation-append-tool-result conversation "new-local-evidence" :tool-name "test"
                                        :output (make-string 20000 :initial-element #\e) :success-p t)
       (test-assert (agent-should-compact-p agent) "new tool evidence crosses the soft limit without new usage")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :conversation conversation) nil)
            (provider-context-budget-error () t))
          "new tool evidence also crosses hard admission")))))
  nil)

(-> test-context-admission-measured-transport-fields () null)
(defun test-context-admission-measured-transport-fields ()
  "Anchor usage when the transport adds fields the wire encoder does not reproduce."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (conversation-append-user-message conversation "Before the measured request.")
       (context-admission-test--measure agent)
       (test-assert (conversation-context-usage-sample conversation)
                    "transport-only fields do not prevent the usage anchor")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                      "the next request reuses the measured input"))
       (test-call-with-function-replacements
        (list (list 'system-prompt
                    (lambda (configuration) (declare (ignore configuration)) "changed policy")))
        (lambda ()
          (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
                 (request                    (provider-request-object provider conversation #())))
            (test-assert (nth-value 1 (provider-context--matched-input provider request))
                         "changed policy preserves measured transport fields and overrides")
            (setf (gethash "session_id" request) "tampered transport")
            (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                         "encoder-invisible transport edits reject the anchor"))))))
   :provider-class 'context-admission-transport-fields-test-provider)
  nil)

(-> test-context-admission-measured-request-tails () null)
(defun test-context-admission-measured-request-tails ()
  "Include new temporary context, locally expanded input and image allowances."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (context-admission-test--measure agent)
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #()
                                                                   :goal-context (make-string 20000 :initial-element #\g))))
         (test-assert (> (provider-request-input-token-estimate provider request) 5000)
                      "the current temporary tail is not covered by previous usage")
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :conversation conversation) nil)
            (provider-context-budget-error () t))
          "temporary context can refuse the next request"))
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (setf (gethash "input" request)
               (concatenate 'vector (json-get request "input")
                            (vector (json-object "type" "message" "role" "user" "content"
                                                 (vector (json-object "type" "input_image" "image_url" "https://example.invalid/image"))))))
         (multiple-value-bind (tokens uncertain-p) (provider-request-input-token-estimate provider request)
           (test-assert (and uncertain-p (> tokens (provider-image-token-allowance)))
                        "new media retains its bounded allowance alongside measured input"))))))
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((configuration (agent-configuration agent))
            (conversation  (agent-conversation agent))
            (provider      (agent-provider agent))
            (guidance      (make-string 800 :initial-element #\g)))
       (test-call-with-function-replacements
        (list (list 'request-context-session-state
                    (lambda (configuration &key hurry-up-p)
                      (declare (ignore configuration hurry-up-p))
                      guidance)))
        (lambda ()
          (context-admission-test--measure agent :goal-context guidance)
          (test-assert (= 1200 (context-estimate-known-tokens
                                configuration conversation :provider provider))
                       "the meter counts already measured nonempty guidance once")
          (setf conversation (conversation-load (conversation-pathname conversation)))
          (dolist (entry (list (list guidance #'=)
                              (list (make-string 1200 :initial-element #\g) #'>)
                              (list (make-string 400 :initial-element #\g) #'<)
                              (list nil #'<)))
            (let* ((*provider-context-request*
                     (provider-context-request-create provider conversation #()))
                   (request (provider-request-object provider conversation #()
                                                     :goal-context (first entry))))
              (test-assert (funcall (second entry)
                                    (provider-request-input-token-estimate provider request)
                                    1200)
                           "admission replaces measured guidance after reload"))))))))
  nil)

(-> test-context-admission-measured-invalidation () null)
(defun test-context-admission-measured-invalidation ()
  "Reject malformed samples and invalidate measurements at context ownership boundaries."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (dolist (usage (list nil (json-object "input_tokens" 1000)
                           (json-object "input_tokens" -1 "output_tokens" 20)
                           (json-object "input_tokens" "1000" "output_tokens" 20)
                           (json-object "input_tokens" 1000 "output_tokens" 0)
                           (json-object "input_tokens" 1000 "output_tokens" 200 "total_tokens" 2)
                           (json-object "input_tokens" 1000 "output_tokens" 200 "cached_input_tokens" 2000)))
        (context-admission-test--measure agent :envelope-length 64)
        (let ((sample (copy-tree (conversation-context-usage-sample conversation))))
          (context-admission-test--measure agent :usage usage :envelope-length 64)
          (test-assert (equal sample (conversation-context-usage-sample conversation))
                       "invalid usage cannot replace the previous measurement")
          (test-assert (equal sample (conversation-context-usage-sample
                                      (conversation-load (conversation-pathname conversation))))
                       "replay retains the previous measurement after invalid usage")))
       (context-admission-test--measure agent :envelope-length 64)
       (let* ((unconfigured (make-instance 'model-provider))
              (context      (provider-context-request-create unconfigured conversation #())))
          (test-assert (null (getf context :eligible-p))
                       "providers without a configuration method use unmeasured admission")
          (multiple-value-bind (tokens known-p)
              (context-estimate-known-tokens (agent-configuration agent) conversation
                                             :family   (provider-family provider)
                                             :provider unconfigured)
            (test-assert (and (plusp tokens) (not known-p)
                              (= tokens (context-estimate-known-tokens
                                         (agent-configuration agent) conversation
                                         :family (provider-family provider))))
                         "providers without a configuration method also use unmeasured metering")))
       (dolist (field '("instructions" "model" "tools" "input"))
         (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
                (request                    (provider-request-object provider conversation #())))
           (setf (gethash field request)
                 (if (equal field "input")
                     (vector (json-object "type" "message" "role" "user" "content" "A changed covered prefix."))
                     "A changed request field."))
           (multiple-value-bind (tokens matched-p) (provider-context--matched-input provider request)
             (declare (ignore tokens))
             (test-assert (not matched-p) "changed encoded headers or covered items reject the baseline"))))
       (let* ((other                      (make-instance 'context-admission-test-provider
                                                         :configuration (agent-configuration agent)))
              (*provider-context-request* (provider-context-request-create other conversation #()))
              (request                    (provider-request-object other conversation #())))
         (multiple-value-bind (tokens matched-p) (provider-context--matched-input other request)
           (declare (ignore tokens))
           (test-assert matched-p "an identically configured encoder revalidates portable coverage")))
       (let* ((other                      (conversation-create (agent-configuration agent)))
              (*provider-context-request* (provider-context-request-create provider other #()))
              (request                    (provider-request-object provider other #())))
         (multiple-value-bind (tokens matched-p) (provider-context--matched-input provider request)
           (declare (ignore tokens))
           (test-assert (not matched-p) "conversations do not share usage anchors")))
      (let ((sample (copy-tree (conversation-context-usage-sample conversation))))
        (context-admission-test--measure
         agent :envelope-length 64
         :during-request        (lambda (conversation)
                                  (conversation-append-user-message conversation "Arrived while awaiting a reply.")))
        (test-assert (equal sample (conversation-context-usage-sample conversation))
                     "concurrent input prevents advancing the measurement"))
      (context-admission-test--measure agent :envelope-length 64)
      (conversation-append-provider-metadata conversation '(:usage (("input_tokens" 1200) ("output_tokens" 200))))
      (test-assert (conversation-context-usage-sample conversation)
                   "ordinary provider metadata does not discard the latest measured anchor")
      (let ((sample (copy-tree (conversation-context-usage-sample conversation))))
        (agent--note-compaction-request
         agent (make-instance 'agent-observer) :request-number 2
         :usage                                                (json-object "input_tokens" 1200 "output_tokens" 200))
        (test-assert (equal sample (conversation-context-usage-sample conversation))
                     "compaction request metering preserves the live ordinary anchor")
        (test-assert (equal sample (conversation-context-usage-sample
                                    (conversation-load (conversation-pathname conversation))))
                     "disk replay preserves the ordinary anchor after compaction metering"))
      (conversation-append-summary conversation (make-continuation :intent "A replacement context."))
      (test-assert (null (conversation-context-usage-sample conversation)) "summary publication invalidates the sample")
      (test-assert (null (conversation-context-usage-sample
                          (conversation-load (conversation-pathname conversation))))
                   "disk replay invalidates the sample at summary publication")
      (context-admission-test--measure agent :envelope-length 64)
      (test-assert (conversation-context-usage-sample
                    (conversation-load (conversation-pathname conversation)))
                   "disk resume restores fingerprint evidence for revalidation"))))
  nil)

(-> test-context-admission-measured-high-usage () null)
(defun test-context-admission-measured-high-usage ()
  "A genuinely full measured context crosses both limits despite a small serialization."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (context-admission-test--measure
        agent :envelope-length 64
        :usage                 (json-object "input_tokens" 4800 "output_tokens" 200
                                            "total_tokens" 5000 "cached_input_tokens" 4700))
       (test-assert (agent-should-compact-p agent) "full measured occupancy crosses the soft limit")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :conversation conversation) nil)
            (provider-context-budget-error (condition)
              (= 5000 (provider-context-budget-error-estimated-input-tokens condition))))
          "a small envelope cannot erase authoritative measured occupancy")))))
  nil)

(-> test-context-admission-measured-header-resume () null)
(defun test-context-admission-measured-header-resume ()
  "Keep opaque measured history through disk resume and changed environment policy."
  (context-admission-test--with-measured-context
   (lambda (original)
     (let ((tools (vector (json-object "type" "function" "name" "lookup"
                                       "description" (make-string 2000 :initial-element #\d)
                                       "parameters" (json-object "type" "object")))))
       (context-admission-test--measure original :tools tools)
       (let* ((conversation (agent-conversation original))
              (snapshot     (getf (conversation-context-usage-sample conversation) :header-text)))
         (test-assert (equal snapshot (list "small policy" (json-encode tools)))
                      "the first measured epoch stores raw prompt and canonical tool text")
         (context-admission-test--measure original :tools tools
                                          :usage          (json-object "input_tokens" 1300 "output_tokens" 200
                                                                       "total_tokens" 1500))
         (let ((records (remove-if-not (lambda (record) (eq (first record) ':context-usage))
                                      (conversation--read-records (conversation-pathname conversation)))))
           (test-assert (null (getf (getf (rest (first (last records))) :sample) :header-text))
                        "later samples in the same epoch omit repeated header text"))
         (test-assert (equal snapshot
                             (getf (conversation-context-usage-sample
                                    (conversation-load (conversation-pathname conversation))) :header-text))
                      "replay hydrates omitted text into the latest sample"))
       (let* ((configuration (agent-configuration original))
              (conversation  (conversation-load (conversation-pathname (agent-conversation original))))
              (provider      (make-instance 'context-admission-test-provider :configuration configuration))
              (registry      (make-instance 'tool-registry))
              (agent         (agent-create :configuration configuration :conversation conversation
                                           :provider provider :tool-registry registry))
              (application   (make-instance 'application :configuration configuration
                                            :conversation conversation :provider provider
                                            :agent agent :tool-registry registry))
              (prompt        "small policy č😊"))
         (test-call-with-function-replacements
          (list (list 'system-prompt
                      (lambda (configuration) (declare (ignore configuration)) prompt))
                (list 'tool-registry-provider-schemas
                      (lambda (registry &key canonical-names)
                        (declare (ignore registry canonical-names)) tools)))
          (lambda ()
            (let* ((charge   7)
                   (expected (+ 1500 charge)))
              (multiple-value-bind (tokens exact-p)
                  (context-estimate-known-tokens configuration conversation
                                                 :provider provider :tool-registry registry)
                (test-assert (= tokens expected) "only the changed prompt is charged over measured occupancy")
                (test-assert (not exact-p) "changed policy is an estimated meter"))
              (multiple-value-bind (tokens estimated-p) (application-context-usage application)
                (test-assert (and (= expected tokens) estimated-p)
                             "the resumed UI exposes the conservative estimated occupancy"))
              (let* ((*provider-context-request* (provider-context-request-create provider conversation tools))
                     (request                    (provider-request-object provider conversation tools)))
                (multiple-value-bind (tokens uncertain-p)
                    (provider-request-input-token-estimate provider request)
                  (test-assert (and (= expected tokens) uncertain-p)
                               "wire admission retains opaque history before transport observation"))
                (provider-open-response-stream provider request :conversation conversation)
                (setf (gethash "instructions" request) "Post-encode tamper")
                (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                             "changed soft headers do not authorize post-encode edits")
                (setf (gethash "instructions" request) "small policy")
                (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                             "restoring an old measured header also rejects post-encode tamper")))
            (let ((description (copy-seq (json-get (aref tools 0) "description"))))
              (setf (char description 10) #\č
                    (char description 1900) #\😊
                    (gethash "description" (aref tools 0)) description)
              (let* ((*provider-context-request* (provider-context-request-create provider conversation tools))
                     (request                    (provider-request-object provider conversation tools)))
               (test-assert (= 1521 (provider-request-input-token-estimate provider request))
                            "prompt bytes and canonical tool JSON edits share the measured anchor")))
            (setf tools #())
            (let* ((*provider-context-request* (provider-context-request-create provider conversation tools))
                   (request                    (provider-request-object provider conversation tools)))
              (test-assert (= 1507 (provider-request-input-token-estimate provider request))
                           "tool deletion gives no credit and inserts no schema bytes"))
            (setf prompt "small")
            (multiple-value-bind (tokens exact-p)
                (context-estimate-known-tokens configuration conversation
                                               :provider provider :tool-registry registry)
              (test-assert (and (= 1500 tokens) (not exact-p))
                           "deletion-only header edits retain occupancy but are not measured projections"))
            (let* ((*provider-context-request* (provider-context-request-create provider conversation tools))
                   (request                    (provider-request-object provider conversation tools)))
              (multiple-value-bind (tokens uncertain-p) (provider-request-input-token-estimate provider request)
                (test-assert (and (= 1500 tokens) uncertain-p)
                             "wire admission also marks a zero-surcharge changed header estimated")))
            (setf prompt (make-string 20000 :initial-element #\p))
            (test-assert (agent-should-compact-p agent) "genuine prompt growth crosses soft admission")
            (let* ((*provider-context-request* (provider-context-request-create provider conversation tools))
                   (request                    (provider-request-object provider conversation tools)))
              (test-assert
               (handler-case (progn (provider-open-response-stream provider request :conversation conversation) nil)
                 (provider-context-budget-error () t))
               "genuine prompt growth crosses hard admission"))
            (setf prompt "small policy")
            (conversation-append-summary conversation (make-continuation :intent "A replacement context."))
            (context-admission-test--measure agent :tools tools :envelope-length 64)
            (let* ((records (conversation--read-records (conversation-pathname conversation)))
                   (sample  (getf (rest (first (last records))) :sample)))
              (test-assert (equal '("small policy" "[]") (getf sample :header-text))
                           "a reset starts a self-contained header epoch"))))))))
  nil)

(-> test-context-admission-measured-header-identity () null)
(defun test-context-admission-measured-header-identity ()
  "Reject model changes, modified measured history and malformed format-two persistence."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (context-admission-test--measure agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent))
            (sample       (copy-tree (conversation-context-usage-sample conversation))))
       (dolist (key '(:policy :prompt :tool-header :encoder-fields))
         (let ((malformed (copy-tree sample)))
           (setf (getf malformed key) '("invalid" 1))
           (conversation-append-record conversation (list :context-usage :sample malformed))
           (test-assert (null (conversation-context-usage-sample
                               (conversation-load (conversation-pathname conversation))))
                        "malformed component fingerprints are rejected on disk resume")))
       (dotimes (index 2)
         (let* ((malformed (copy-tree sample))
                (text      (copy-list (getf malformed :header-text))))
           (setf (nth index text) "mismatched text"
                 (getf malformed :header-text) text)
           (conversation-append-record conversation (list :context-usage :sample malformed))
           (test-assert (null (conversation-context-usage-sample
                               (conversation-load (conversation-pathname conversation))))
                        "snapshot text must match both stored component fingerprints")))
       (let ((empty (copy-tree sample)))
         (setf (getf empty :header-text) (list "" (second (getf sample :header-text)))
               (getf empty :prompt) (provider-cache-prefix--entry ""))
         (test-assert (conversation-context-usage-sample-p empty)
                      "empty prompt text is valid present snapshot evidence"))
       (let ((legacy (copy-tree sample)))
         (remf legacy :header-text)
         (remf legacy :wire-history-p)
         (conversation-append-record conversation '(:context-usage :sample nil))
         (conversation-append-record conversation (list :context-usage :sample legacy))
         (let ((loaded (conversation-context-usage-sample
                        (conversation-load (conversation-pathname conversation)))))
           (test-assert (and (= 26 (length loaded)) (null (getf loaded :header-text)))
                        "genuine fingerprint-only format-two records load without invented text")
           (test-call-with-function-replacements
            (list (list 'system-prompt
                        (lambda (configuration) (declare (ignore configuration)) "small policy č")))
            (lambda ()
              (test-assert (= 15 (provider-context--header-charge
                                  loaded (provider-context--header-components provider #())))
                           "old format-two evidence charges current UTF-8 bytes until remeasured")))))
       (setf (conversation-context-usage-sample conversation) sample)
       (let* ((components (provider-context--header-components provider #()))
              (history    (provider-context--history provider conversation)))
         (setf (getf components :policy) (provider-cache-prefix--entry "different model"))
         (test-assert (not (provider-context--sample-matches-p
                            sample (provider-cache-prefix--entry "different header") history :components components))
                      "a different model policy cannot retain measured occupancy"))
       (let ((*provider-context-request* (provider-context-request-create provider conversation #())))
         (let ((request (provider-request-object provider conversation #())))
           (setf (gethash "encrypted_content"
                          (first (conversation-input-items-for-family conversation (provider-family provider))))
                 "Edited measured history")
           (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                        "history is revalidated at admission rather than trusting its captured snapshot")))
       (let ((legacy (copy-tree sample)))
          (dolist (key '(:policy :prompt :tool-header :encoder-fields :header-text :wire-history-p))
            (remf legacy key))
         (setf (getf legacy :format) 1)
         (test-assert (conversation-context-usage-sample-p legacy) "legacy exact anchors are readable")
         (test-assert (not (provider-context--sample-matches-p
                            legacy (provider-cache-prefix--entry "changed legacy header") nil))
                      "legacy samples do not authorize a changed header")))))
  nil)

(-> test-context-admission-measured-resume () null)
(defun test-context-admission-measured-resume ()
  "Revalidate persisted measurements after restart and charge newly retained evidence."
  (context-admission-test--with-measured-context
   (lambda (original)
     (context-admission-test--measure original)
     (let* ((configuration (agent-configuration original))
            (conversation  (conversation-load (conversation-pathname (agent-conversation original))))
            (provider      (make-instance 'context-admission-test-provider :configuration configuration))
            (registry      (make-instance 'tool-registry))
            (agent         (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry registry))
            (application   (make-instance 'application :configuration configuration
                                                      :conversation conversation :provider provider
                                                      :agent agent :tool-registry registry)))
       (test-assert (conversation-context-usage-sample-p (conversation-context-usage-sample conversation))
                    "restart restores portable coverage evidence")
       (test-assert (not (agent-should-compact-p agent))
                    "retained opaque replay does not cause startup compaction")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                      "the final encoded prefix revalidates the historical measurement")
         (setf (gethash "instructions" request) "Different wire policy.")
         (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                      "restart does not bypass final wire validation"))
       (multiple-value-bind (before estimated-p)
           (application-context-usage application)
         (test-assert (not estimated-p)
                      "an exact measured anchor without retained suffix is not estimated")
         (conversation-append-provider-item
          conversation (json-object "type" "function_call" "name" "test" "call_id" "resume-output"
                                    "arguments" "{}"))
         (conversation-append-tool-result conversation "resume-output" :tool-name "test"
                                          :output (make-string 20000 :initial-element #\e) :success-p t)
        (test-assert (nth-value 1 (application-context-usage application))
                     "new retained evidence makes the context meter estimated")
        (test-assert (> (application-context-usage application) before)
                     "the meter includes tool output not covered by the last response")
        (test-assert (= (application-context-usage application)
                        (context-estimate-known-tokens configuration conversation
                                                      :provider provider :tool-registry registry))
                     "the meter uses the same current estimate as soft compaction")
        (test-assert (agent-should-compact-p agent)
                     "new retained evidence can cross the soft limit after restart"))
       (agent--persist-provider-result
        agent (make-instance 'provider-result :output-items nil
                             :usage                         (json-object "input_tokens" 1000 "output_tokens" 200
                                                                         "total_tokens" 1200))
        :request-number 2)
       (test-assert (conversation-context-usage-sample-p
                     (conversation-context-usage-sample
                      (conversation-load (conversation-pathname conversation))))
                    "an interrupted response preserves the older measured anchor")
       (conversation-append-record conversation '(:context-usage :sample (:format 1)))
       (test-assert (null (conversation-context-usage-sample
                           (conversation-load (conversation-pathname conversation))))
                    "malformed coverage is discarded during durable replay"))))
  nil)

(-> test-context-admission-switched-reasoning () null)
(defun test-context-admission-switched-reasoning ()
  "Compare the meter and encoded cold request with and without another model's sealed state."
  (with-test-configuration (configuration)
    (let* ((provider     (provider-create configuration))
           (model        (config :model configuration))
           (conversation (conversation-create configuration))
           (blob         (make-string 100000 :initial-element #\z)))
      (conversation-set-model-selection conversation "gpt-6.2-astra" "high")
      (conversation-append-user-message conversation "portable message")
      (conversation-append-provider-item conversation (json-object "type" "reasoning" "encrypted_content" blob))
      (conversation-append-provider-item conversation (json-object "type" "function_call" "name" "sample"
                                                                  "call_id" "pair" "arguments" "{}"))
      (conversation-append-tool-result conversation "pair" :tool-name "sample" :output "portable result" :success-p t)
      (conversation-set-model-selection conversation model "high")
      (let* ((loaded (conversation-load (conversation-pathname conversation)))
             (clean  (conversation-compaction-summary-view loaded (remove-if #'reasoning-item-p (conversation-input-items loaded))))
             (wire   (json-get (provider-request-object provider loaded #()) "input")))
        (test-assert (and (not (find-if #'reasoning-item-p wire))
                          (test-object-contains-string-p wire "portable message")
                          (test-object-contains-string-p wire "portable result")
                          (find-if #'function-call-item-p wire))
                     "the switched wire omits ciphertext and keeps the message and tool pair")
        (test-assert (= (context-estimate-known-tokens configuration loaded :provider provider)
                        (context-estimate-known-tokens configuration clean :provider provider))
                     "the cold meter ignores exactly the omitted reasoning")
        (test-assert (find-if #'reasoning-item-p (conversation-input-items-for-family loaded (provider-family provider) :model "gpt-6.2-astra"))
                     "the producing model can recover its state after reload"))))
  nil)

(-> test-context-admission-switched-measurement () null)
(defun test-context-admission-switched-measurement ()
  "Keep verified occupancy after a Codex model change and charge new input."
  (with-test-configuration (base)
    (let* ((before       (configuration-copy base :model "gpt-6.2-astra" :model-provider "chatgpt"
                                                  :reasoning-effort "high"))
           (after        (configuration-copy before :model "gpt-6.1-sol"))
           (old-provider (provider-create before))
           (new-provider (provider-create after))
           (conversation (conversation-create before))
           (reasoning    (json-object "type" "reasoning" "summary" #() "encrypted_content" "old state")))
      (test-call-with-function-replacements
       (list (list 'system-prompt (lambda (configuration) (declare (ignore configuration)) "same prompt"))
             (list 'provider-effective-context-window (lambda (provider) (declare (ignore provider)) 32768))
             (list 'context-resolve-request
                   (lambda (configuration conversation tools &key goal-context compaction-p)
                     (declare (ignore configuration conversation tools goal-context compaction-p))
                     nil)))
       (lambda ()
         (conversation-set-model-selection conversation "gpt-6.2-astra" "high" :provider "chatgpt")
         (conversation-append-user-message conversation (make-string 160000 :initial-element #\a))
         (let* ((*provider-context-request* (provider-context-request-create old-provider conversation #()))
                (request                    (provider-request-object old-provider conversation #())))
           (setf (getf *provider-context-request* :wire) (provider-context--wire-plan old-provider request)
                 (getf *provider-context-request* :wire-characters)
                 (provider-context--request-characters old-provider (provider-request-history old-provider request)))
           (conversation-append-provider-item conversation reasoning)
           (provider-context-note-response
            *provider-context-request*
            (make-instance 'provider-result :output-items (list reasoning)
                           :usage                         (json-object "input_tokens" 1000 "output_tokens" 200))))
         (conversation-set-model-selection conversation "gpt-6.1-sol" "high" :provider "chatgpt")
         (let* ((loaded                     (conversation-load (conversation-pathname conversation)))
                (*provider-context-request* (provider-context-request-create new-provider loaded #()))
                (request                    (provider-request-object new-provider loaded #())))
           (test-assert (getf (conversation-context-usage-sample loaded) :wire-history-p)
                        "admission proof survives model change and reload")
           (multiple-value-bind (tokens exact-p)
               (context-estimate-known-tokens after loaded :provider new-provider)
             (test-assert (and (= tokens 1200) (not exact-p))
                          "measured occupancy survives a durable model switch as an estimate"))
           (test-assert (not (find-if #'reasoning-item-p (provider-request-history new-provider request)))
                        "only portable history is encoded for the new model")
           (test-assert (null (provider-check-request-context new-provider request))
                        "unchanged tool and message history is not charged again")
           (dolist (case '(("same prompté" 2) ("same 🙂 prompt" 5) ("same" 0)))
             (test-call-with-function-replacements
              (list (list 'system-prompt
                          (lambda (configuration)
                            (declare (ignore configuration))
                            (first case))))
              (lambda ()
                (let* ((*provider-context-request* (provider-context-request-create new-provider loaded #()))
                       (changed                    (provider-request-object new-provider loaded #())))
                  (multiple-value-bind (tokens uncertain-p)
                      (provider-request-input-token-estimate new-provider changed)
                    (test-assert (and (= tokens (+ 1200 (second case))) uncertain-p)
                                 "changed prompts after a model switch charge inserted UTF-8 bytes without deletion credit"))
                  (test-assert (null (provider-check-request-context new-provider changed))
                               "the final changed-prompt request passes admission after reload")))))
           (let ((*provider-context-request* (provider-context-request-create new-provider loaded #())))
             (let ((large (provider-request-object new-provider loaded #()
                                                   :goal-context (make-string 180000 :initial-element #\b))))
               (test-assert (handler-case (progn (provider-check-request-context new-provider large) nil)
                              (provider-context-budget-error () t))
                            "new encoded context still counts at the request boundary")))
           (setf (gethash "input" request) (vector (json-object "type" "message" "role" "user" "content" "changed")))
           (test-assert (not (nth-value 1 (provider-context--matched-input new-provider request)))
                        "changed encoded history cannot reuse measured occupancy"))))))
  nil)
