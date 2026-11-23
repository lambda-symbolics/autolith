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

(-> test-compaction-submission-acceptance () null)
(defun test-compaction-submission-acceptance ()
  "Accept exactly one finished compaction.submit call whose arguments are a valid checkpoint.

Each case lists a response's output items, its turn completion, and whether
it submits a checkpoint."
  (flet ((submit (arguments &rest fields)
           "Return a submit call with ARGUMENTS and FIELDS replacing its own."
           (let ((call (agent-test-checkpoint-call arguments)))
             (loop for (name value) on fields by (partial #'nthcdr 2)
                   do (setf (gethash name call) value))
             call))

         (arguments (&key (intent "Go on.") (notes "[]") (next "") (next-refs "[]") (extra ""))
           "Return submit arguments built from their fields; NOTES and NEXT-REFS are JSON texts."
           (format nil "{\"intent\":~S,\"notes\":~A,\"next\":~S,\"next_refs\":~A~A}"
                   intent notes next next-refs extra))

         (note (kind text)
           "Return the JSON text of one note of KIND holding TEXT."
           (format nil "{\"kind\":~S,\"text\":~S,\"refs\":[]}" kind text)))
    (let* ((valid      (agent-test-checkpoint-call))
           (citing     (submit (arguments :next "Read it." :next-refs "[\"known\"]")))
           (references (list (list :handle "known" :kind ':message :label "A record.")
                             (list :handle "other" :kind ':message :label "Another record."))))
      (flet ((submission (items completion)
               "Return the checkpoint a response of ITEMS and COMPLETION submits, or NIL."
               (agent--compaction-submission
                (agent-test-result "submission" items :turn-completion completion)
                references nil)))
        (loop for (items completion accepted-p) in
              (list (list (list valid) ':end t)
                    ;; A tool call leaves some providers' turns unfinished.
                    (list (list valid) ':continue t)
                    (list (list valid) ':unspecified t)
                    (list (list (json-object "type" "reasoning" "summary" #()) valid) ':end t)
                    (list (list citing) ':end t)
                    (list nil ':end nil)
                    (list (list (agent-test-message "A summary in prose.")) ':end nil)
                    (list (list valid (agent-test-message "Submitted.")) ':end nil)
                    (list (list valid valid) ':end nil)
                    (list (list (agent-test-call :call-id "wait" :namespace "job" :name "wait"))
                          ':end nil)
                    (list (list (submit (arguments) "call_id" "")) ':end nil)
                    (list (list (submit (arguments) "status" "incomplete")) ':end nil)
                    (list (list (submit "{\"intent\":")) ':end nil)
                    (list (list (submit (arguments :intent ""))) ':end nil)
                    (list (list (submit (arguments
                                         :intent (make-string
                                                  (1+ *continuation-intent-maximum-characters*)
                                                  :initial-element #\i))))
                          ':end nil)
                    (list (list (submit (arguments :extra ",\"verified\":true"))) ':end nil)
                    (list (list (submit (arguments :extra ",\"intent\":\"Twice.\""))) ':end nil)
                    (list (list (submit (arguments
                                         :notes (format nil "[~A]" (note "done" "Tests pass.")))))
                          ':end nil)
                    (list (list (submit (arguments
                                         :notes (format nil "[~A]" (note "open" "")))))
                          ':end nil)
                    (list (list (submit (arguments
                                         :notes (format nil "[~{~A~^,~}]"
                                                        (loop repeat (1+ *continuation-maximum-notes*)
                                                              collect (note "open" "More."))))))
                          ':end nil)
                    (list (list (submit (arguments :next-refs "[\"unknown\"]"))) ':end nil)
                    ;; The next step is a string, never an object.
                    (list (list (submit "{\"intent\":\"Go on.\",\"notes\":[],\"next\":{\"text\":\"\",\"refs\":[]}}"))
                          ':end nil))
              do (test-assert (eq accepted-p (not (null (submission items completion))))
                              "only one finished, valid compaction.submit call submits a checkpoint"))
        (test-assert (equal (list (first references))
                            (getf (submission (list citing) ':end) :references))
                     "a checkpoint keeps the references it cites and no others"))))
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
                                                                                   (list (agent-test-checkpoint-call))
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
                                                                                                            (list (agent-test-checkpoint-call))
                                                                                                            :turn-completion ':end)
                                                                                         (agent-test-result "answer" (list (agent-test-message "verified"))
                                                                                                            :turn-completion ':end)))))
           (observer                               (make-instance 'compaction-admission-test-observer :cap 1000))
           (pending                                nil)
           (*agent-restricted-maximum-tool-rounds* 0))
      (test-call-with-function-replacements
       (list (list 'agent-should-compact-p
                   (lambda (agent &key &allow-other-keys) (declare (ignore agent))
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

(defvar *compaction-gap-test-contributions* nil
  "Dynamically bound contributor invocation counter for recovery fixtures.")

(-> compaction-gap-test--contribute (request-context) context-contribution)
(defun compaction-gap-test--contribute (context)
  "Record one invocation and deliver a real request-local instruction."
  (declare (ignore context))
  (incf (first *compaction-gap-test-contributions*))
  (make-context-contribution :identifier "recovery" :instruction "Keep the continuation concise."))

(defclass compaction-gap-test-provider
    (compaction-admission-test-provider context-admission-test-provider)
  ((mode :initarg :mode :initform nil :reader compaction-gap-test-mode
         :documentation "Optional failure control at actual request admission.")
   (attempts :initform 0 :accessor compaction-gap-test-attempts
             :documentation "Ordinary attempts, including locally refused ones.")
   (reservations :initform nil :accessor compaction-gap-test-reservations
                 :documentation "Independent mission reservations seen by each attempt."))
  (:documentation "Use real encoded admission before deterministic scripted responses."))

(defmethod provider-stream-turn
    ((provider compaction-gap-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Exercise final admission without credentials or network access."
  (declare (ignore event-callback))
  (unless compaction-p
    (incf (compaction-gap-test-attempts provider)))
  (let ((context (mission-context-find conversation)))
    (when context
      (let ((ledger (mission-context-goal context)))
        (push (list (getf ledger :requests-outstanding) (getf ledger :tokens-used)
                    (getf ledger :tokens-reserved) *provider-mission-output-allowance*)
              (compaction-gap-test-reservations provider)))))
  (when (and (not compaction-p) (eq (compaction-gap-test-mode provider) :generic))
    (error 'provider-protocol-error :message "Scripted transport failure."
           :status nil :code nil :request-id nil :response-id nil :response nil))
  (let ((before (and *compaction-gap-test-contributions* (first *compaction-gap-test-contributions*))))
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation tool-namespaces
                                 :goal-context goal-context :compaction-p compaction-p)
      (declare (ignore delivery))
      (when (and before (not compaction-p))
        (push (- (first *compaction-gap-test-contributions*) before)
              (rest *compaction-gap-test-contributions*)))
      (when (and (not compaction-p)
                 (or (eq (compaction-gap-test-mode provider) :refuse)
                     (and (eq (compaction-gap-test-mode provider) :second-refusal)
                          (> (compaction-gap-test-attempts provider) 1))))
        (setf (gethash "max_output_tokens" request) 6000))
      (provider-open-response-stream provider request :conversation conversation)))
  (call-next-method))

(defclass compaction-gap-test-observer (agent-observer)
  ((compactions :initform 0 :accessor compaction-gap-test-compactions
                :documentation "Automatic compaction announcements."))
  (:documentation "Observe automatic compaction without changing its decision."))

(defmethod agent-observer-status ((observer compaction-gap-test-observer) status details)
  "Count announced automatic compactions."
  (declare (ignore details))
  (when (eq status :compaction-started)
    (incf (compaction-gap-test-compactions observer)))
  nil)

(-> compaction-gap-test--recover
    (boolean &key (:mode (option keyword)) (:proactive-p boolean)
                  (:failure-type symbol)
                  (:compactions (integer 0)) (:network-calls (integer 0)))
    null)
(defun compaction-gap-test--recover
    (mission-p &key mode proactive-p (failure-type 'null)
                   (compactions 1) (network-calls 2))
  "Exercise local refusal and recovery, optionally under mission accounting."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider
               (make-instance 'compaction-gap-test-provider
                              :configuration configuration
                              :mode          mode
                              :results       (list (agent-test-result "summary" (list (agent-test-checkpoint-call)))
                                                   (agent-test-result "answer" (list (agent-test-message "OK"))))))
             (application
               (when mission-p (mission-inference-test--application configuration provider)))
             (conversation
               (if application (application-conversation application) (conversation-create configuration)))
             (observer
               (make-instance 'compaction-gap-test-observer))
             (agent
               (agent-create :configuration configuration :conversation conversation
                             :provider provider :tool-registry (make-instance 'tool-registry)))
             (goal
               (make-string 10000 :initial-element #\g))
             (*conversation-compaction-tail-characters*
               1024)
             (*context-contributors*
               nil)
             (*context-resolver*
               (cl-llm-provider-api:make-context-resolver))
             (*compaction-gap-test-contributions*
               (list 0)))
        (register-context-contributor "recovery" 'compaction-gap-test--contribute)
        (when application
          (application-mission-start application (mission-test--specification :turns 8 :tokens 100000)))
        (unwind-protect
             (progn
               (conversation-append-user-message conversation (make-string 12000 :initial-element #\h))
               (conversation-append-user-message conversation "continue")
               (test-call-with-function-replacements
                (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
                      (list 'request-context-session-state
                            (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) ""))
                      ;; Isolate final admission: the prepared soft check would
                      ;; otherwise compact this oversized request first.
                      (list 'agent-should-compact-p
                            (lambda (candidate &key &allow-other-keys)
                              (declare (ignore candidate))
                              proactive-p)))
                (lambda ()
                  (test-assert (eq (not (null proactive-p)) (agent-should-compact-p agent :goal-context goal))
                               "the controlled soft trigger matches this scenario")
                  (let ((failure nil))
                    (handler-case (agent--run-provider-loop agent observer :goal-context goal :tools-p nil)
                      (error (condition) (setf failure condition)))
                    (format t "~&FINAL-ADMISSION-WITNESS mission=~S compactions=~D transport=~D error=~S~%"
                            mission-p (compaction-gap-test-compactions observer)
                            (context-admission-test-provider-network-calls provider)
                            (and failure (type-of failure)))
                    (test-assert (typep failure failure-type) "the ordinary attempt has the expected outcome")
                    (test-assert (= compactions (compaction-gap-test-compactions observer))
                                 "the step uses at most one compaction opportunity")
                    (test-assert (= network-calls (context-admission-test-provider-network-calls provider))
                                 "only admitted summary and ordinary requests reach inert transport"))))
               (test-assert (every (lambda (count) (= 1 count))
                                   (rest *compaction-gap-test-contributions*))
                            "each ordinary preparation invokes its contributor exactly once")
               (when application
                 (let ((ledger (mission-context-goal (mission-context-find conversation))))
                   (format t "~&MISSION-LEDGER-WITNESS status=~S used=~D turns=~D unknown=~D reserved=~D outstanding=~D attempts=~S~%"
                           (getf ledger :status) (getf ledger :tokens-used) (getf ledger :turns-used)
                           (getf ledger :unknown-usage 0) (getf ledger :tokens-reserved)
                           (getf ledger :requests-outstanding) (compaction-gap-test-reservations provider))
                   (test-assert
                    (every (lambda (snapshot)
                             (destructuring-bind (outstanding used reserved allowance) snapshot
                               (declare (ignore used))
                               (and (= 1 outstanding) (= reserved allowance))))
                           (compaction-gap-test-reservations provider))
                    "each attempt owns a single independent reservation")
                   (when (eq mode :second-refusal)
                     (test-assert (and (eq ':active (getf ledger :status))
                                       (zerop (getf ledger :unknown-usage 0))
                                       (zerop (getf ledger :tokens-reserved))
                                       (zerop (getf ledger :requests-outstanding))
                                       (= (* 2 network-calls) (getf ledger :tokens-used))
                                       (= (+ 2 network-calls) (getf ledger :turns-used)))
                                  "both refused attempts and the paid summary reserve turns"))
                   (unless (eq mode :second-refusal)
                     (test-assert (and (eq ':active (getf ledger :status))
                                       (zerop (getf ledger :unknown-usage 0))
                                       (zerop (getf ledger :tokens-reserved))
                                       (zerop (getf ledger :requests-outstanding))
                                       (= (* 2 network-calls) (getf ledger :tokens-used))
                                       (= (+ 1 network-calls) (getf ledger :turns-used)))
                                  "recovery settles all reservations without unknown usage")))))
          (when application (mission-detach application))))))
  nil)

(-> test-compaction-final-admission-trigger () null)
(defun test-compaction-final-admission-trigger ()
  "Recover a final request larger than the known-history projection."
  (compaction-gap-test--recover nil))

(-> test-compaction-mission-admission-trigger () null)
(defun test-compaction-mission-admission-trigger ()
  "Recover outside mission ownership after a proven zero-usage refusal."
  (compaction-gap-test--recover t))

(-> test-compaction-admission-recovery-bounds () null)
(defun test-compaction-admission-recovery-bounds ()
  "Bound recovery, share the proactive opportunity and preserve unrelated errors."
  (dolist (mission-p '(nil t))
    (compaction-gap-test--recover mission-p :mode ':second-refusal
                                 :failure-type 'provider-context-budget-error :network-calls 1)
    (compaction-gap-test--recover mission-p :mode ':refuse :proactive-p t
                                 :failure-type 'provider-context-budget-error :network-calls 1))
  (compaction-gap-test--recover nil :mode ':generic :failure-type 'provider-protocol-error
                               :compactions 0 :network-calls 0)
  nil)

(-> test-compaction-final-projection-gap () null)
(defun test-compaction-final-projection-gap ()
  "Characterize the difference between known history and final encoded admission."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'context-admission-test-provider :configuration configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (conversation-append-user-message conversation (make-string 12000 :initial-element #\h))
        (test-call-with-function-replacements
         (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
               (list 'request-context-session-state
                     (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) "")))
         (lambda ()
           (test-assert (not (agent-should-compact-p agent))
                        "the fresh known estimate is below the soft threshold")
           (let ((request (provider-request-object provider conversation #()
                                                   :goal-context (make-string 10000 :initial-element #\g))))
             (test-assert (handler-case (progn (provider-check-request-context provider request) nil)
                            (provider-context-budget-error () t))
                          "the final encoded request nevertheless exceeds the hard window")))))))
  nil)

(defclass prepared-soft-test-provider (compaction-gap-test-provider)
  ()
  (:documentation "Answer compaction requests with a checkpoint and ordinary requests from the script."))

(defmethod provider-stream-turn :around
    ((provider prepared-soft-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Supply a valid checkpoint to each compaction request."
  (declare (ignore tool-namespaces event-callback goal-context))
  (when compaction-p
    (push (agent-test-result "summary" (list (agent-test-checkpoint-call)))
          (scripted-provider-results provider)))
  (call-next-method))

(defvar *prepared-soft-test-invocations* nil
  "Ordinary contributor invocations counted by the prepared soft compaction fixture.")

(-> prepared-soft-test--contribute (request-context) (option context-contribution))
(defun prepared-soft-test--contribute (context)
  "Count one ordinary invocation and deliver a marker naming it."
  (unless (request-context-compaction-p context)
    (make-context-contribution
     :identifier  "prepared-soft-marker"
     :instruction (format nil "Context marker ~D." (incf (first *prepared-soft-test-invocations*))))))

(-> prepared-soft-test--run
    (&key (:measured-tool-characters (integer 0)) (:goal-characters (integer 0)) (:tools-p boolean))
    list)
(defun prepared-soft-test--run
    (&key (measured-tool-characters 0) (goal-characters 0) (tools-p t))
  "Run one ordinary step after a measured Codex turn and report the delivered requests."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                                  (make-instance 'prepared-soft-test-provider
                                                                      :configuration configuration
                                                                      :results       (list (agent-test-result "answer" (list (agent-test-message "OK"))))))
             (registry                                  (make-instance 'tool-registry))
             (conversation                              (conversation-create configuration))
             (observer                                  (make-instance 'compaction-gap-test-observer))
             (agent                                     (agent-create :configuration configuration :conversation conversation
                                                                     :provider provider :tool-registry registry))
             (*conversation-compaction-tail-characters* 1024)
             (*context-contributors*                    nil)
             (*context-resolver*                        (cl-llm-provider-api:make-context-resolver))
             (*prepared-soft-test-invocations*          (list 0)))
        (when (plusp measured-tool-characters)
          (tool-registry-register
           registry
           (make-instance 'agent-test-echo-tool
                          :namespace   "test"
                          :name        "echo"
                          :description (make-string measured-tool-characters :initial-element #\t)
                          :parameters  (tool-object-schema
                                        (json-object "value" (tool-string-property "The value to echo."))
                                        '("value")))))
        (test-call-with-function-replacements
         (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small policy"))
               (list 'request-context-session-state
                     (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) "")))
         (lambda ()
           (conversation-append-user-message conversation "Measured request.")
           (context-admission-test--measure
            agent
            :tools           (tool-registry-provider-schemas registry)
            :envelope-length 100
            :usage           (if (plusp measured-tool-characters)
                                 (json-object "input_tokens" 4100 "output_tokens" 100 "total_tokens" 4200)
                                 (json-object "input_tokens" 50 "output_tokens" 10 "total_tokens" 60)))
           (setf (context-admission-test-provider-network-calls provider) 0)
           (conversation-append-user-message conversation "continue")
           (register-context-contributor "prepared-soft-marker" 'prepared-soft-test--contribute)
           (register-context-contributor "task-notes" 'context--task-notes)
           (task-note-post (conversation-identifier conversation) "child" "pending child note")
           (let ((goal    (and (plusp goal-characters) (make-string goal-characters :initial-element #\g)))
                 (failure nil))
             (handler-case (agent--run-provider-loop agent observer :goal-context goal :tools-p tools-p)
               (error (condition) (setf failure condition)))
             (list :requests    (reverse (scripted-provider-compaction-flags provider))
                   :transport   (context-admission-test-provider-network-calls provider)
                   :failure     failure
                   :invocations (first *prepared-soft-test-invocations*)
                   :sent        (json-encode (context-admission-test-provider-last-request provider))))))))))

(-> test-compaction-prepared-soft-threshold () null)
(defun test-compaction-prepared-soft-threshold ()
  "Send only the necessary requests and deliver fresh context and queued child notes."
  (dolist (scenario '((12000 0 nil (nil)) (0 15600 t (t nil))))
    (destructuring-bind (tool-characters goal-characters tools-p requests) scenario
      (let* ((report (prepared-soft-test--run :measured-tool-characters tool-characters
                                              :goal-characters          goal-characters
                                              :tools-p                  tools-p))
             (count  (getf report :invocations))
             (sent   (getf report :sent)))
        (test-assert (null (getf report :failure)) "the step completes")
        (test-assert (equal requests (getf report :requests))
                     "only the oversized prepared request needs a compaction before the ordinary request")
        (test-assert (= (length requests) (getf report :transport))
                     "only the expected requests reach the transport")
        (test-assert (search "pending child note" sent)
                     "queued child output reaches the ordinary request, including after compaction")
        (test-assert (search (format nil "Context marker ~D." count) sent)
                     "the ordinary request contains its latest context")
        (test-assert (if (plusp goal-characters)
                         (and (> count 1) (not (search "Context marker 1." sent)))
                         (= count 1))
                     "context is shared without compaction and rebuilt after compaction"))))
  nil)


;;;; -- Mission Ownership of Detached Compaction Requests --

(defclass compaction-ownership-test-provider (compaction-gap-test-provider)
  ((summary-mode :initarg       :summary-mode
                 :initform      ':known
                 :reader        compaction-ownership-test-summary-mode
                 :documentation "Known, unknown, failed or locally refused summary requests.")
   (views :initform nil :accessor compaction-ownership-test-views
          :documentation "Request views and exact mission identity during dispatch."))
  (:documentation "Observe detached request ownership without credentials or network access."))

(defmethod provider-stream-turn
    ((provider compaction-ownership-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record summary identity and exercise ordinary failure accounting."
  (declare (ignore tool-namespaces event-callback goal-context))
  (when compaction-p
    (push (list conversation (mission-context-find conversation))
          (compaction-ownership-test-views provider))
    (case (compaction-ownership-test-summary-mode provider)
      (:error
       (error 'provider-protocol-error :message "Scripted summary failure."
              :status nil :code nil :request-id nil :response-id nil :response nil))
      (:refuse
       (provider-open-response-stream
        provider (json-object "input" "short" "max_output_tokens" 6000)
        :conversation conversation))))
  (call-next-method))

(-> compaction-ownership-test--run
    (&key (:mission-p boolean) (:mode keyword) (:turns (integer 1)) (:continued-p boolean)) null)
(defun compaction-ownership-test--run (&key (mission-p t) (mode ':known) (turns 8) continued-p)
  "Verify actual usage, refusal, budget and cleanup on a private mission."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((usage        (if (eq mode :unknown) (json-object)
                              (json-object "input_tokens" 1 "output_tokens" 1)))
             (provider     (make-instance
                            'compaction-ownership-test-provider
                            :configuration configuration :summary-mode mode
                            :results (append
                                      (when continued-p
                                        (list (agent-test-result "unfinished" nil :turn-completion ':continue)))
                                      (list (agent-test-result
                                             "summary" (list (agent-test-checkpoint-call))
                                             :usage usage)))))
             (application  (when mission-p (mission-inference-test--application configuration provider)))
             (conversation (if application (application-conversation application)
                               (conversation-create configuration)))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider      provider :tool-registry (make-instance 'tool-registry)))
             (failure      nil)
             (context      nil))
        (when application
          (application-mission-start application (mission-test--specification :turns turns :tokens 100000))
          (setf context (mission-context-find conversation)))
        (unwind-protect
             (progn
               (conversation-append-user-message conversation "Preserve this exact instruction.")
               (when (and application (= turns 1))
                 (mission-inference-test--call
                  (make-instance 'scripted-provider
                                 :configuration configuration
                                 :results       (list (agent-test-result "prior" (list (agent-test-message "prior")))))
                  conversation))
               (test-call-with-function-replacements
                (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
                      (list 'request-context-session-state
                            (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) "")))
                (lambda ()
                  (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
                    (error (condition) (setf failure condition)))))
               (dolist (entry (compaction-ownership-test-views provider))
                 (test-assert (eq context (second entry)) "the request uses its source's exact mission identity")
                 (test-assert (null (mission-context-find (first entry))) "fresh ownership ends on every unwind"))
               (when application
                 (let* ((ledger      (mission-context-goal context))
                        (blocked-p   (member mode '(:unknown :error)))
                        (exhausted-p (= turns 1))
                        (expected-tokens
                          (cond
                            (exhausted-p
                             2)
                            (blocked-p
                             0)
                            ((eq mode :refuse)
                             0)
                            (continued-p
                             4)
                            (t
                             2))))
                   (format t "~&OWNERSHIP-WITNESS mode=~S turns=~D error=~S ledger=~S~%"
                           mode turns (and failure (type-of failure)) ledger)
                   (test-assert (= expected-tokens (getf ledger :tokens-used)) "only actual reported usage is charged")
                   (test-assert (= (if blocked-p 1 0) (getf ledger :unknown-usage 0)) "unknown follows ordinary mission policy")
                   (test-assert (zerop (getf ledger :requests-outstanding)) "all request reservations settle")
                   (test-assert (zerop (getf ledger :tokens-reserved)) "no output reservation leaks")
                   (cond
                     (exhausted-p
                      (test-assert failure "the exhausted mission refuses summary dispatch")
                      (test-assert (null (compaction-ownership-test-views provider)) "no summary enters after turn exhaustion"))
                     (blocked-p
                      (test-assert (eq ':blocked (getf ledger :status)) "unknown summary usage blocks new mission requests"))
                     ((eq mode :refuse)
                      (test-assert (typep failure 'provider-context-budget-error) "a local refusal retains its type")
                      (test-assert (eq ':active (getf ledger :status)) "a local compaction refusal has known zero usage"))
                     (t
                      (test-assert (null failure) "owned compaction succeeds")
                      (test-assert (= (if continued-p 2 1) (getf ledger :turns-used))
                                   "each summary request reserves one turn"))))))
          (when application (mission-detach application))))))
  nil)

(-> test-compaction-summary-mission-ownership () null)
(defun test-compaction-summary-mission-ownership ()
  "Charge known summaries and bound view ownership."
  (compaction-ownership-test--run)
  (compaction-ownership-test--run :continued-p t)
  (compaction-ownership-test--run :mission-p nil))

(-> test-compaction-summary-mission-failures () null)
(defun test-compaction-summary-mission-failures ()
  "Retain ordinary UNKNOWN and budget policies without invented usage."
  (dolist (mode '(:unknown :error :refuse))
    (compaction-ownership-test--run :mode mode))
  (compaction-ownership-test--run :turns 1))

(-> test-compaction-request-view-isolation () null)
(defun test-compaction-request-view-isolation ()
  "Bound exact mission aliases without clobbering nested or unrelated request owners."
  (with-test-configuration (configuration)
    (let* ((first-application  (mission-test--application configuration))
           (second-application (mission-test--application configuration))
           (first-source       (application-conversation first-application))
           (second-source      (application-conversation second-application))
           (view               (conversation-create configuration)))
      (application-mission-start first-application (mission-test--specification))
      (application-mission-start second-application (mission-test--specification))
      (unwind-protect
           (let ((first-context  (mission-context-find first-source))
                 (second-context (mission-context-find second-source)))
             (mission-call-with-request-view
              first-context view
              (lambda ()
                (test-assert (eq first-context (mission-context-find view)) "the outer request owns its fresh view")
                (mission-call-with-request-view
                 second-context view
                 (lambda ()
                   (test-assert (eq first-context (mission-context-find view)) "nested work cannot replace an existing owner")))
                (test-assert (eq first-context (mission-context-find view)) "nested cleanup preserves the outer association")))
             (test-assert (null (mission-context-find view)) "outer cleanup removes only its introduced association")
             (mission-context-bind second-context view)
             (mission-call-with-request-view
              first-context view
              (lambda ()
                (test-assert (eq second-context (mission-context-find view)) "unrelated existing ownership is preserved")))
             (test-assert (eq second-context (mission-context-find view)) "cleanup never removes an unrelated association")
             (with-lock-held (*mission-contexts-lock*)
               (remhash view *mission-contexts*)))
        (mission-detach first-application)
        (mission-detach second-application))))
  nil)
