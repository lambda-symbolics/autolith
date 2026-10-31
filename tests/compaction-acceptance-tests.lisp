(in-package #:autolith)

;;;; -- Compaction Acceptance and Paid Attempts --

(defclass compaction-admission-test-provider (scripted-provider codex-subscription-provider)
  ((caps :initform nil :accessor compaction-admission-test-caps
         :documentation "Output caps observed by ordinary scripted requests.")
   (encoded-choices :initform nil :accessor compaction-admission-test-encoded-choices
                    :documentation "Tool choices in encoded candidate requests.")
   (ordinary-choices :initform nil :accessor compaction-admission-test-ordinary-choices
                     :documentation "Tool choices supplied to ordinary requests."))
  (:documentation "Combine deterministic responses with the real Codex request encoder."))

(defmethod provider-request-object
    ((provider compaction-admission-test-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Build admission requests with the production Codex encoder."
  (multiple-value-bind (request delivery)
      (provider-request-object
       (make-instance 'codex-subscription-provider :configuration (provider-configuration provider))
       conversation tool-namespaces :goal-context goal-context :compaction-p compaction-p)
    (push (json-get request "tool_choice") (compaction-admission-test-encoded-choices provider))
    (values request delivery)))

(defmethod provider-native-compact-conversation
    ((provider compaction-admission-test-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Use the portable path without credentials or transport."
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values nil nil))

(defmethod provider-stream-turn :before
    ((provider compaction-admission-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record the cap actually supplied to each ordinary request."
  (declare (ignore conversation tool-namespaces event-callback goal-context))
  (unless compaction-p
    (push *provider-maximum-output-tokens* (compaction-admission-test-caps provider))
    (push *provider-tool-choice* (compaction-admission-test-ordinary-choices provider))))

(defclass compaction-admission-test-observer (agent-observer)
  ((cap :initarg :cap :reader compaction-admission-test-cap
        :documentation "Selected ordinary output allowance.")
   (starts :initform nil :accessor compaction-admission-test-starts
           :documentation "Details for each announced ordinary request."))
  (:documentation "Select an observable cap once at each ordinary request boundary."))

(defmethod agent-observer-status
    ((observer compaction-admission-test-observer) status details)
  "Record request starts and return the selected output cap."
  (when (eq status ':provider-request-started)
    (push details (compaction-admission-test-starts observer))
    (compaction-admission-test-cap observer)))

(-> test-compaction-summary-acceptance () null)
(defun test-compaction-summary-acceptance ()
  "Validate paid summaries and publish the last text answer even when misshapen.

Each case lists its output items, turn completion, whether the handoff passes
the strict shape, and whether the only allowed request publishes it."
  (let* ((valid (agent-test-compaction-message))
         (text  (provider-result-assistant-text
                 (agent-test-result "text" (list valid) :turn-completion ':end))))
    (dolist (case (list (list (list valid) ':end t t)
                       (list (list valid) ':continue nil nil)
                       (list (list valid) ':unspecified t t)
                       (list nil ':end nil nil)
                       (list (list (agent-test-message "unstructured text")) ':end nil t)
                       (list (list (agent-test-message
                                    (format nil "## [Progress]~%Work is recorded.~2%**[Decisions]** Continue.~2%## [Context]~%State is durable.~2%## [Constraints]~%Follow the user.~2%## [Next steps]~%Resume.~2%## [References]~%The archive."))) ':end t t)
                       (list (list (agent-test-message
                                    (format nil "Here is the handoff.~%~A" text))) ':end nil t)
                       (list (list (agent-test-message
                                    (concatenate 'string text " [Progress] Duplicate."))) ':end nil t)
                       (list (list (agent-test-message
                                    (concatenate 'string text (make-string 12000 :initial-element #\x)))) ':end nil t)
                       (list (list valid (json-object "type" "refusal" "refusal" "No.")) ':end nil nil)
                       (list (list valid (json-object "type" "message" "role" "assistant"
                                                     "refusal" "No." "content" #())) ':end nil nil)))
      (destructuring-bind (items completion usable-p published-p) case
        (with-test-configuration (configuration)
          (let* ((conversation (conversation-create configuration))
                 (result       (agent-test-result "paid-summary" items :turn-completion completion))
                 (provider     (make-instance 'scripted-provider :configuration configuration :results (list result)))
                 (agent        (agent-create :configuration configuration :conversation conversation
                                             :provider provider :tool-registry (agent-test-registry) :worker nil)))
            (conversation-append-user-message conversation "retain exact source evidence")
            (let ((before                                      (mapcar #'json-encode (conversation-input-items conversation)))
                  (*agent-compaction-maximum-summary-requests* 1))
              (test-assert (eq usable-p (agent--compaction-summary-usable-p result))
                           "only a finished structured refusal-free handoff is usable")
              (let ((published
                      (handler-case
                          (progn (agent-compact-conversation agent (make-instance 'agent-observer)) t)
                        (provider-protocol-error () nil))))
                (test-assert (eq published-p published)
                             "the last request publishes any text answer and nothing else"))
              (test-assert (equal '((1 :summary)) (agent-tests--request-numbers conversation))
                           "every completed summary attempt is charged once")
              (unless published-p
                (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                             "rejection preserves the source projection")
                (test-assert (equal before
                                    (mapcar #'json-encode
                                            (conversation-input-items
                                             (conversation-load (conversation-pathname conversation)))))
                             "rejection preserves the replayed projection"))))))))
  nil)

(-> test-compaction-native-paid-rejection () null)
(defun test-compaction-native-paid-rejection ()
  "Meter paid native completion even with no opaque output or rejected content."
  (dolist (invalid-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'scripted-provider :configuration configuration
                                          :results                          (list (agent-test-result "summary"
                                                                                                    (list (agent-test-compaction-message))
                                                                                                    :turn-completion ':end))))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (agent-test-registry) :worker nil)))
        (conversation-append-user-message conversation "native source")
        (test-call-with-function-replacements
         (list (list 'provider-native-compact-conversation
                     (lambda (provider view &key tool-namespaces event-callback)
                       (declare (ignore provider view tool-namespaces))
                       (funcall event-callback
                                (make-instance 'native-compaction-completed-event
                                               :usage nil :response-id "native-paid"))
                       (when invalid-p
                         (error 'provider-protocol-error :message "Rejected paid native output."
                                                        :status nil :code nil :request-id nil
                                                        :response-id "native-paid" :response nil))
                       (values nil nil))))
         (lambda ()
           (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
             (provider-protocol-error () nil))))
        (test-assert (equal (if invalid-p '((1 :native)) '((1 :native) (2 :summary)))
                            (agent-tests--request-numbers conversation))
                     "paid native completion is recorded once, before interpretation"))))
  nil)

(-> test-compaction-next-request-admission () null)
(defun test-compaction-next-request-admission ()
  "Validate pending caps and MCP contributions before publishing a checkpoint."
  (dolist (large-mcp-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "50000"))
      (with-test-configuration (configuration)
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance 'compaction-admission-test-provider
                                            :configuration configuration
                                            :results       (list (agent-test-result "summary"
                                                                                   (list (agent-test-compaction-message))
                                                                                   :turn-completion ':end))))
               (observer     (make-instance 'compaction-admission-test-observer
                                            :cap (if large-mcp-p 1000 50000)))
               (agent        (agent-create :configuration configuration :conversation conversation
                                           :provider provider :tool-registry (agent-test-registry) :worker nil)))
          (conversation-append-user-message conversation "original source")
          (let ((before (mapcar #'json-encode (conversation-input-items conversation))))
            (test-call-with-function-replacements
             (list (list 'mcp-tool-registry-context-contributions
                         (lambda (registry)
                           (declare (ignore registry))
                           (when large-mcp-p
                            (loop for index below 2
                                  collect (make-context-contribution
                                           :identifier  (format nil "large-mcp-~D" index)
                                           :priority 1000 :class ':mandatory
                                           :instruction (make-string 120000 :initial-element #\x)))))))
             (lambda ()
               (test-assert
                (handler-case
                    (progn (agent-compact-conversation agent observer
                                                      :next-request-status-details t) nil)
                  (provider-context-budget-error () t))
                "candidate admission includes the actual cap and MCP context")))
            (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                         "oversized next-request context cannot replace the source")
            (test-assert (equal '((1 :summary)) (agent-tests--request-numbers conversation))
                         "candidate rejection records the paid summary, not a phantom ordinary request")
            (test-assert (= 1 (length (compaction-admission-test-starts observer)))
                         "the next ordinary cap is selected once"))))))
  nil)

(-> test-compaction-repeated-continuation-accounting () null)
(defun test-compaction-repeated-continuation-accounting ()
  "Continue through repeated checkpoints and reloads with exact request accounting."
  (with-test-configuration (configuration)
    (let* ((conversation                           (conversation-create configuration))
           (provider                               (make-instance 'compaction-admission-test-provider
                                                                  :configuration configuration
                                                                  :results       (loop repeat 3 append
                                                                                   (list (agent-test-result "summary"
                                                                                                            (list (agent-test-compaction-message))
                                                                                                            :turn-completion ':end)
                                                                                         (agent-test-result "answer" (list (agent-test-message "verified"))
                                                                                                            :turn-completion ':end)))))
           (observer                               (make-instance 'compaction-admission-test-observer :cap 1000))
           (pending                                nil)
           (*agent-restricted-maximum-tool-rounds* 0))
      (test-call-with-function-replacements
       (list (list 'agent-should-compact-p
                   (lambda (agent) (declare (ignore agent))
                     (prog1 pending (setf pending nil)))))
       (lambda ()
         (loop for index from 1 to 3
               do (setf pending t)
                  (let ((agent (agent-create :configuration configuration :conversation conversation
                                             :provider provider :tool-registry (agent-test-registry) :worker nil)))
                    (agent-run-user-turn agent (format nil "literal continuation ~D" index)
                                         :observer observer :tool-allowlist '("test.echo")
                                         :tool-restriction-p (oddp index)))
                  (setf conversation (conversation-load (conversation-pathname conversation))))))
      (test-assert (equal '((1 :summary) (2 nil) (1 :summary) (2 nil) (1 :summary) (2 nil))
                          (agent-tests--request-numbers conversation))
                   "every checkpoint and continuation contributes one paid record")
      (test-assert (equal '(1000 1000 1000) (compaction-admission-test-caps provider))
                   "ordinary requests reuse the cap checked before publication")
      (test-assert (= 3 (length (compaction-admission-test-starts observer)))
                   "compaction does not duplicate ordinary request-start notifications")
      (test-assert (equal '("none" "auto" "none")
                          (compaction-admission-test-encoded-choices provider))
                   "candidate encoding includes the resolved restricted-round tool choice")
      (test-assert (equal '(:none :auto :none)
                          (compaction-admission-test-ordinary-choices provider))
                   "ordinary requests use the same tool choice checked before publication")
      (test-assert (test-object-contains-string-p (conversation-input-items conversation)
                                                "literal continuation 3")
                   "the latest exact user continuation survives publication and replay")))
  nil)
