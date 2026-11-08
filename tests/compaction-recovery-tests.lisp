(in-package #:autolith)

;;;; -- Emergency Compaction Recovery --

(defclass compaction-recovery-test-provider (compaction-gap-test-provider)
  ((window :initarg :window :initform nil :reader compaction-recovery-test-window
           :documentation "Deterministic selected-route metadata.")
   (native-calls :initform 0 :accessor compaction-recovery-test-native-calls
                 :documentation "Admitted inert native transports."))
  (:documentation "Exercise both final admission boundaries without credentials."))

(defmethod provider-compaction-recovery-window ((provider compaction-recovery-test-provider))
  "Inject existing selected-route metadata deterministically."
  (compaction-recovery-test-window provider))

(defmethod provider-open-native-compaction
    ((provider compaction-recovery-test-provider) (request hash-table) &key credentials conversation)
  "Record actual native dispatch without network traffic."
  (declare (ignore request credentials conversation))
  (incf (compaction-recovery-test-native-calls provider))
  (values nil 200 nil))

(defmethod provider-native-compact-conversation
    ((provider compaction-recovery-test-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Admit a real native projection, then model unsupported portable fallback."
  (declare (ignore event-callback))
  (provider-open-native-compaction
   provider (provider-native-compaction-request-object provider conversation tool-namespaces)
   :conversation conversation)
  (values nil nil))

(-> compaction-recovery-test-small-context (function) t)
(defun compaction-recovery-test-small-context (function)
  "Isolate admission fixtures from the host's prompt and request-local contributors."
  (let ((*context-contributors* nil)
        (*context-resolver*     (cl-llm-provider-api:make-context-resolver)))
    (test-call-with-function-replacements
     (list (list 'system-prompt (lambda (configuration) (declare (ignore configuration)) "small"))
           (list 'request-context-session-state
                 (lambda (conversation &key hurry-up-p) (declare (ignore conversation hurry-up-p)) "")))
     function)))

(-> test-compaction-recovery-encrypted-history () null)
(defun test-compaction-recovery-encrypted-history ()
  "Recover a refused native compaction using detached old encrypted reasoning."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation                              (conversation-create configuration))
             (provider                                  (make-instance 'compaction-recovery-test-provider
                                                         :configuration configuration
                                                         :results       (list (agent-test-result
                                                                               "summary" (list (agent-test-compaction-message))))))
             (agent                                     (agent-create :configuration configuration :conversation conversation
                                                         :provider provider :tool-registry (make-instance 'tool-registry)))
             (*conversation-compaction-tail-characters* 256)
             (failure                                   nil))
        (conversation-append-user-message conversation "older exchange")
        (conversation-append-provider-item
         conversation (json-object "type" "reasoning" "id" "old-secret"
                                   "encrypted_content" (make-string 40000 :initial-element #\e)))
        (conversation-append-provider-item conversation (agent-test-message "old answer"))
        (conversation-append-user-message conversation "continue")
        (compaction-recovery-test-small-context
         (lambda ()
           (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
             (provider-context-budget-error (condition) (setf failure condition)))))
        (test-assert (null failure)
                     (format nil "Emergency compaction must recover the refused request: ~A" failure))
        (test-assert (= 1 (compaction-recovery-test-native-calls provider))
                     "Exactly one admitted native attempt reaches transport.")
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "Exactly one admitted portable summary reaches transport."))))
  nil)

(-> test-compaction-recovery-publication-preserves-tail-families () null)
(defun test-compaction-recovery-publication-preserves-tail-families ()
  "Preserve protected private tail visibility through publication and durable replay."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation                              (conversation-create configuration))
             (provider                                  (make-instance 'compaction-recovery-test-provider
                                                         :configuration configuration
                                                         :results       (list (agent-test-result
                                                                               "summary" (list (agent-test-compaction-message))))))
             (agent                                     (agent-create :configuration configuration :conversation conversation
                                                         :provider provider :tool-registry (make-instance 'tool-registry)))
             (*conversation-compaction-tail-characters* 256)
             (family                                    (provider-family provider))
             (old                                       (json-object "type" "reasoning" "id" "old-secret"
                                                                     "encrypted_content" (make-string 40000 :initial-element #\e)))
             (current                                   (json-object "type" "reasoning" "id" "rs-current"
                                                                     "encrypted_content" "current encrypted"))
             (failure                                   nil))
        (conversation-append-user-message conversation "older exchange")
        (conversation-append-provider-item conversation old)
        (conversation-append-provider-item conversation (agent-test-message "old answer"))
        (conversation-append-user-message conversation "continue")
        (conversation-append-provider-item conversation current)
        (conversation-append-provider-item conversation (compaction-tail-tests--call "current-call"))
        (conversation-append-tool-result conversation "current-call"
                                         :tool-name "test.inspect" :output "current output" :success-p t)
        (test-assert (eq family (conversation-input-item-family conversation current))
                     "Fixture: current reasoning has its provider family.")
        (test-assert (member current (conversation-input-items-for-family conversation family :include-ephemeral-p nil))
                     "Fixture: current reasoning is visible before compaction.")
        (let ((tail (mapcar #'json-encode (last (conversation-input-items conversation) 4))))
          (compaction-recovery-test-small-context
           (lambda ()
             (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
               (provider-context-budget-error (condition) (setf failure condition)))))
          (test-assert (null failure) "Path: portable recovery successfully publishes.")
          (test-assert (= 1 (compaction-recovery-test-native-calls provider))
                       "Path: one native attempt exhausts into portable compaction.")
          (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                       "Path: exactly one admitted portable summary reaches transport.")
          (let* ((summary (find :summary (conversation-records-newest conversation 20) :key #'first))
                 (audit   (getf (rest summary) :recovery)))
            (test-assert (find :encrypted (getf audit :stages) :key (lambda (stage) (getf stage :tier)))
                         "Path: durable publication records Tier1 encrypted recovery."))
          (test-assert (not (find 40000 (conversation-input-items conversation)
                                 :key (lambda (item) (length (or (json-get item "encrypted_content") "")))))
                       "Path: eligible oversized old encrypted reasoning is removed.")
          (test-assert (= 40000 (length (json-get old "encrypted_content")))
                       "Path: original old item is not mutated.")
          (test-assert (equal "current encrypted" (json-get current "encrypted_content"))
                       "Path: original current item is not mutated.")
          (test-assert (equal tail (mapcar #'json-encode (last (conversation-input-items conversation) 4)))
                       "Path: protected tail content and order survive publication."))
        (compaction-recovery-test-small-context
         (lambda ()
           (let ((request (provider-request-object provider conversation #())))
             (test-assert (find "current encrypted" (json-get request "input")
                                :key (lambda (item) (json-get item "encrypted_content")) :test #'equal)
                          "Contract: next provider request includes protected current reasoning."))))
        (dolist (item (conversation-input-items conversation))
          (when (equal "reasoning" (json-get item "type"))
            (test-assert (eq family (conversation-input-item-family conversation item))
                         "Contract: published reasoning retains family metadata.")))
        (let* ((loaded     (conversation-load-by-id configuration (conversation-identifier conversation)))
               (projection (conversation-input-items-for-family loaded family :include-ephemeral-p nil)))
          (test-assert (equal (mapcar #'json-encode
                                     (conversation-input-items-for-family conversation family :include-ephemeral-p nil))
                              (mapcar #'json-encode projection))
                       "Replay: provider projection agrees with the published projection.")
          (test-assert (find "current encrypted" projection
                             :key (lambda (item) (json-get item "encrypted_content")) :test #'equal)
                       "Contract: replayed provider projection includes protected current reasoning.")
          (dolist (item (conversation-input-items loaded))
            (when (equal "reasoning" (json-get item "type"))
              (test-assert (eq family (conversation-input-item-family loaded item))
                           "Contract: replayed reasoning retains family metadata.")))))))
  nil)

(-> test-compaction-recovery-metadata-permit () null)
(defun test-compaction-recovery-metadata-permit ()
  "Use strictly larger route metadata once at either actual transport boundary."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (window '(nil 4000 5000 20000))
        (let* ((provider (make-instance 'compaction-recovery-test-provider
                                       :configuration configuration :window window :results nil))
               (state    (make-compaction-recovery)))
          (compaction-recovery-initialize state provider)
          (test-assert (eq (not (null (compaction-recovery-permit state))) (eql window 20000))
                       "Only strictly larger metadata creates an allowance.")))
      (dolist (native-p '(nil t))
        (let* ((provider (make-instance 'compaction-recovery-test-provider
                                       :configuration configuration :window 20000 :results nil))
               (state    (make-compaction-recovery))
               (request  (json-object "input" (make-string 24000 :initial-element #\x)
                                     "max_output_tokens" 700))
               (oversize (json-object "input" (make-string 80000 :initial-element #\x)
                                      "max_output_tokens" 700)))
          (compaction-recovery-initialize state provider)
          (let ((*provider-compaction-recovery-permit* (compaction-recovery-permit state)))
            (provider-check-request-context provider request)
            (test-assert (eq :ready (third *provider-compaction-recovery-permit*))
                         "A pure preflight leaves the permit ready.")
            (test-assert (handler-case (progn (provider-check-request-context provider oversize) nil)
                           (provider-context-budget-error () t))
                         "The actual output reserve and metadata window remain hard bounds.")
            (test-assert (eq :ready (third *provider-compaction-recovery-permit*))
                         "A refused preflight cannot consume a permit.")
            (if native-p
                (provider-open-native-compaction provider request)
                (provider-open-response-stream provider request))
            (test-assert (= 5000 (provider-effective-context-window provider))
                         "Actual dispatch consumes the sole allowance.")
            (test-assert (handler-case (progn (provider-open-response-stream provider request) nil)
                           (provider-context-budget-error () t))
                         "A second dispatch is refused even inside the same dynamic extent."))
          (test-assert (handler-case (progn (provider-check-request-context provider request) nil)
                         (provider-context-budget-error () t))
                       "The next ordinary request sees its configured window.")))))
  nil)

(-> test-compaction-recovery-unrelated-failure () null)
(defun test-compaction-recovery-unrelated-failure ()
  "A larger dispatch has no automatic retry and cannot authorize lossy recovery on failure."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (type '(provider-protocol-error provider-unauthorized))
        (let* ((provider (make-instance 'compaction-recovery-test-provider
                                       :configuration configuration :window 20000 :results nil))
               (view     (conversation-create configuration))
               (state    (make-compaction-recovery))
               (request  (json-object "input" (make-string 24000 :initial-element #\x)))
               (failure  (make-condition type :message "Unrelated failure." :status 500
                                             :code nil :request-id nil :response-id nil :response nil))
               (caught   nil)
               (attempts 0))
          (handler-case
              (compaction-recovery-call
               state provider view
               :preflight (lambda (candidate) (declare (ignore candidate))
                            (provider-check-request-context provider request))
               :dispatch
               (lambda (candidate)
                 (declare (ignore candidate))
                 (provider-check-request-context provider request)
                (provider--call-with-bounded-retries
                  provider
                  (lambda (force-refresh callback)
                    (declare (ignore force-refresh callback))
                    (provider-open-response-stream provider request)
                    (incf attempts)
                    (error failure))
                  (lambda (event) (declare (ignore event))))))
            (error (condition) (setf caught condition)))
          (test-assert (eq failure caught) "The unrelated original condition escapes unchanged.")
          (test-assert (= 1 attempts) "There is only one actual enlarged transport attempt.")
          (test-assert (zerop (compaction-recovery-tier state)) "No lossy tier follows auth or transport failure.")))))
  nil)

(-> test-compaction-recovery-encrypted-eligibility () null)
(defun test-compaction-recovery-encrypted-eligibility ()
  "Keep plaintext reasoning, current-loop reasoning, anchors and correlated replay groups."
  (with-test-configuration (configuration)
    (let* ((state        (make-compaction-recovery))
           (conversation (conversation-create configuration))
           (old          (json-object "type" "reasoning" "encrypted_content" "old encrypted"))
           (plain        (json-object "type" "reasoning" "summary" #("plaintext reasoning")))
           (paired       (json-object "type" "reasoning" "encrypted_content" "paired encrypted"))
           (call         (compaction-tail-tests--call "paired"))
           (output       (compaction-tail-tests--output "paired" "answer"))
           (current      (json-object "type" "reasoning" "encrypted_content" "current encrypted"))
           (view         (conversation-compaction-summary-view
                          conversation (list old plain paired call output
                                             (json-object "type" "message" "role" "user" "content" "latest") current)))
           (detached     (compaction-recovery-view state view))
           (candidate    (compaction-recovery-strip-encrypted state detached)))
      (test-assert (not (test-object-contains-string-p (conversation-input-items candidate) "old encrypted"))
                   "Eligible old encrypted content is removed.")
      (dolist (text '("plaintext reasoning" "paired encrypted" "paired" "current encrypted" "latest"))
        (test-assert (test-object-contains-string-p (conversation-input-items candidate) text)
                     "Protected reasoning and replay pairs survive."))
      (test-assert (equal "old encrypted" (json-get old "encrypted_content"))
                   "Detached removal leaves source objects unchanged.")))
  nil)

(-> test-compaction-recovery-tools-and-transcript () null)
(defun test-compaction-recovery-tools-and-transcript ()
  "Stub newest eligible old outputs first and preserve whole newest exchanges in plain recovery."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (state        (make-compaction-recovery))
           (a            (compaction-tail-tests--output "a" (make-string 12000 :initial-element #\a)))
           (b            (compaction-tail-tests--output "b" (make-string 12000 :initial-element #\b)))
           (items        (list (json-object "type" "message" "role" "user" "content" "old")
                          (compaction-tail-tests--call "a") a
                          (compaction-tail-tests--call "b") b
                          (json-object "type" "message" "role" "user"
                                       "content" (make-string 180000 :initial-element #\p))))
           (view         (compaction-recovery-view state (conversation-compaction-summary-view conversation items)))
           (candidates   (compaction-recovery-stub-candidates state view)))
      (test-assert (equal '("b" "a") (mapcar (lambda (item) (json-get item "call_id")) candidates))
                   "Eligible outputs are newest-first outside the 40K protected tail.")
      (let* ((candidate (compaction-recovery-stub state view (first candidates)))
             (outputs   (remove-if-not (lambda (item) (json-string= (json-get item "type") "function_call_output"))
                                     (conversation-input-items candidate))))
        (test-assert (equal '("a" "b") (mapcar (lambda (item) (json-get item "call_id")) outputs))
                     "IDs and batch order survive stubbing.")
        (test-assert (= 12000 (length (json-get (first outputs) "output")))
                     "The older eligible output is not unnecessarily stubbed.")
        (test-assert (search "12000" (json-get (second outputs) "output"))
                     "The stub records original size.")
        (test-assert (= 12000 (length (json-get b "output"))) "Source tool objects are detached.")
        (let* ((text   (compaction-recovery-text-view state candidate))
               (newest (compaction-recovery-drop-exchange text)))
          (test-assert (= 1 (length (conversation-input-items newest)))
                       "Plain recovery drops the oldest whole exchange.")
          (test-assert (test-object-contains-string-p (conversation-input-items newest)
                                                     (make-string 180000 :initial-element #\p))
                       "The newest protected text is never clipped.")
          (test-assert (null (compaction-recovery-drop-exchange newest))
                       "The newest exchange cannot be evicted.")))))
  nil)

(defclass compaction-recovery-accounting-test-provider
    (compaction-recovery-test-provider compaction-ownership-test-provider) ()
  (:documentation "Combine final native admission with C1 ordinary mission ownership."))

(defmethod provider-native-compact-conversation
    ((provider compaction-recovery-accounting-test-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Exercise the real accounted native attempt boundary."
  (provider-attempt-native-compaction provider conversation :tool-namespaces tool-namespaces
                                     :event-callback event-callback :force-refresh nil))

(defmethod provider-attempt-native-compaction
    ((provider compaction-recovery-accounting-test-provider) (conversation conversation)
     &key tool-namespaces force-refresh event-callback)
  "Admit a native request, then report known or unknown usage through ordinary accounting."
  (declare (ignore force-refresh event-callback))
  (push (list conversation (mission-context-find conversation)) (compaction-ownership-test-views provider))
  (provider-open-native-compaction
   provider (provider-native-compaction-request-object provider conversation tool-namespaces)
   :conversation conversation)
  (values nil (if (eq :unknown (compaction-ownership-test-summary-mode provider))
                  (json-object)
                  (json-object "input_tokens" 2 "output_tokens" 3))))

(-> test-compaction-recovery-mission-accounting () null)
(defun test-compaction-recovery-mission-accounting ()
  "Charge actual recovery usage, block UNKNOWN, and refuse exhausted mission budgets."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (mode '(:known :unknown :budget))
        (let* ((provider                                  (make-instance 'compaction-recovery-accounting-test-provider
                                                           :configuration configuration :summary-mode mode :native-p t
                                                           :results (list (agent-test-result "summary" (list (agent-test-compaction-message))
                                                                                            :usage (json-object "input_tokens" 1 "output_tokens" 1)))))
               (application                               (mission-inference-test--application configuration provider))
               (conversation                              (application-conversation application))
               (agent                                     (agent-create :configuration configuration :conversation conversation
                                                           :provider provider :tool-registry (make-instance 'tool-registry)))
               (*conversation-compaction-tail-characters* 256)
               (failure                                   nil))
          (conversation-append-user-message conversation "old user")
          (conversation-append-provider-item
           conversation (json-object "type" "reasoning" "encrypted_content" (make-string 40000 :initial-element #\e)))
          (conversation-append-provider-item conversation (agent-test-message "old answer"))
          (conversation-append-user-message conversation "continue")
          (application-mission-start application (mission-test--specification :tokens 100000 :turns (if (eq mode :budget) 1 8)))
          (let* ((context (mission-context-find conversation))
                 (before  (mapcar #'json-encode (conversation-input-items conversation))))
            (compaction-recovery-test-small-context
             (lambda ()
               (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
                 (error (condition) (setf failure condition)))))
            (let ((ledger (mission-context-goal context)))
              (test-assert (if (eq mode :known) (null failure) (typep failure 'mission-error))
                           "Only known, budget-admitted recovery succeeds.")
              (test-assert (= (if (eq mode :known) 7 0) (getf ledger :tokens-used 0))
                           "Only actual reported usage is charged.")
              (test-assert (= (if (eq mode :unknown) 1 0) (getf ledger :unknown-usage 0))
                           "Unknown usage follows existing mission policy.")
              (test-assert (zerop (getf ledger :requests-outstanding 0)) "All reservations settle.")
              (unless (eq mode :known)
                (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                             "Failure preserves original history."))
              (dolist (entry (compaction-ownership-test-views provider))
                (test-assert (eq context (second entry)) "Every dispatched copy has the exact captured owner."))
              (when (eq mode :budget)
                (test-assert (zerop (compaction-recovery-test-native-calls provider))
                             "Hard mission budget refusal reaches no transport."))))))))
  nil)

(-> test-compaction-recovery-selected-route () null)
(defun test-compaction-recovery-selected-route ()
  "Read the selected route's existing metadata without provider preparation."
  (with-test-configuration (configuration)
    (let ((provider (make-instance 'context-admission-test-provider :configuration configuration))
          (calls    nil))
      (test-call-with-function-replacements
       (list (list 'provider-model-context-window-for
                   (lambda (model &key provider)
                     (push (list model provider) calls)
                     20000)))
       (lambda ()
         (test-assert (= 20000 (provider-compaction-recovery-window provider))
                      "The existing selected-route metadata is returned.")))
      (test-assert (equal calls (list (list (config :model configuration)
                                          (config :model-provider configuration))))
                   "Lookup receives exactly the selected model and provider.")))
  nil)

(-> test-compaction-recovery-exhaustion () null)
(defun test-compaction-recovery-exhaustion ()
  "Preserve history on tier exhaustion and on ordinary publication refusal."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'compaction-recovery-test-provider
                                          :configuration configuration
                                          :results       (list (agent-test-result
                                                                "summary" (list (agent-test-compaction-message))))))
             (agent        (agent-create :configuration configuration :conversation conversation
                                          :provider provider :tool-registry (make-instance 'tool-registry)))
             (state        (make-compaction-recovery))
             (failure      nil))
        (conversation-append-user-message conversation "old user")
        (conversation-append-provider-item conversation (agent-test-message "old answer"))
        (conversation-append-user-message conversation (make-string 40000 :initial-element #\n))
        (let ((before (copy-list (conversation-input-items conversation)))
              (bytes  (mapcar #'json-encode (conversation-input-items conversation))))
          (compaction-recovery-test-small-context
           (lambda ()
             (flet ((admit (candidate)
                      (provider-check-request-context
                       provider (provider-native-compaction-request-object provider candidate #()))))
               (dotimes (attempt 2)
                 (handler-case
                     (compaction-recovery-call state provider conversation :dispatch #'admit :preflight #'admit)
                   (provider-context-budget-error (condition) (setf failure condition)))))))
          (test-assert (typep failure 'provider-context-budget-error) "All tiers exhaust on the intact newest exchange.")
          (test-assert (= 3 (compaction-recovery-tier state)) "Portable exhaustion reaches Tier3.")
          (dolist (tier '(:encrypted :tool-output-stage :plain-text))
            (test-assert (= 1 (count tier (compaction-recovery-notes state)
                                    :key (lambda (note) (getf note :tier))))
                         "Each lossy stage runs once across repeated calls."))
          (setf failure nil)
          (compaction-recovery-test-small-context
           (lambda ()
             (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
               (provider-context-budget-error (condition) (setf failure condition)))))
          (test-assert (typep failure 'provider-context-budget-error)
                       "The retained oversized user request fails ordinary final publication admission.")
          (test-assert (every #'eq before (conversation-input-items conversation))
                       "Every original input item retains its identity.")
          (test-assert (equal bytes (mapcar #'json-encode (conversation-input-items conversation)))
                       "Exhaustion and publication refusal never rewrite or clip the source.")
          (test-assert (and (zerop (compaction-recovery-test-native-calls provider))
                            (= 1 (context-admission-test-provider-network-calls provider)))
                       "Only the admitted older-history summary reaches transport.")))))
  nil)


;;;; -- Unsupported Native Dispatch Accounting --

(defclass unsupported-native-test-provider (scripted-provider codex-subscription-provider)
  ((status :initarg :status :reader unsupported-native-test-status
           :documentation "Synthetic native HTTP status or transport failure.")
   (window :initarg :window :initform nil :reader unsupported-native-test-window
           :documentation "Existing metadata available to recovery.")
   (source :initform nil :accessor unsupported-native-test-source
           :documentation "Canonical conversation, never used for native dispatch.")
   (context :initform nil :accessor unsupported-native-test-context
            :documentation "Exact source mission owner.")
   (native-calls :initform 0 :accessor unsupported-native-test-native-calls
                 :documentation "Actual admitted native dispatches.")
   (summary-calls :initform 0 :accessor unsupported-native-test-summary-calls
                  :documentation "Admitted portable summary requests.")
   (settlements :initform nil :accessor unsupported-native-test-settlements
                :documentation "Ledger snapshots after native accounting settles."))
  (:documentation "Production native attempts with inert final dispatch and real mission accounting."))

(defmethod provider-compaction-recovery-window ((provider unsupported-native-test-provider))
  "Supply existing metadata without discovery or network access."
  (unsupported-native-test-window provider))

(defmethod provider-request-object
    ((provider unsupported-native-test-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Use the real Codex encoder rather than scripted request encoding."
  (provider-request-object
   (make-instance 'codex-subscription-provider :configuration (provider-configuration provider))
   conversation tool-namespaces :goal-context goal-context :compaction-p compaction-p))

(defmethod provider-open-native-compaction
    ((provider unsupported-native-test-provider) (request hash-table) &key credentials conversation)
  "Refuse only at the final native dispatch boundary, without network traffic."
  (declare (ignore request credentials))
  (incf (unsupported-native-test-native-calls provider))
  (test-assert (not (eq conversation (unsupported-native-test-source provider)))
               "Native requests use a detached view.")
  (test-assert (eq (mission-context-find conversation) (unsupported-native-test-context provider))
               "The detached native dispatch retains its exact mission owner.")
  (test-assert (= 1 (getf (mission-context-goal (mission-context-find conversation)) :requests-outstanding))
               "Dispatch occurs under real mission reservation and supervision.")
  (case (unsupported-native-test-status provider)
    (:transport
     (error 'provider-transport-error :message "Inert native transport failure."
            :status nil :code nil :request-id nil :response-id nil :response nil))
    (200
     (values (json-encode
              (json-object "output" (json-array (json-object "type" "compaction" "encrypted_content" "fixture"))
                           "usage" (json-object "input_tokens" 2 "output_tokens" 3)))
             200 nil))
    (t
     (values "Not found" (unsupported-native-test-status provider) nil))))

(defmethod provider-attempt-native-compaction :around
    ((provider unsupported-native-test-provider) (conversation conversation)
     &key tool-namespaces force-refresh event-callback)
  "Observe settlement outside the production accounting around method."
  (declare (ignore conversation tool-namespaces force-refresh event-callback))
  (unwind-protect (call-next-method)
    (push (copy-tree (mission-context-goal (unsupported-native-test-context provider)))
          (unsupported-native-test-settlements provider))))

(defmethod provider-open-response-stream
    ((provider unsupported-native-test-provider) (request hash-table) &key credentials conversation)
  "Admit an inert stream through the normal context boundary."
  (declare (ignore request credentials conversation))
  (values nil 200 nil))

(defmethod provider-stream-turn
    ((provider unsupported-native-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Apply final admission, then return deterministic positive portable usage."
  (declare (ignore event-callback))
  (provider-open-response-stream
   provider (provider-request-object provider conversation tool-namespaces
                                     :goal-context goal-context :compaction-p compaction-p)
   :conversation conversation)
  (when compaction-p
    (incf (unsupported-native-test-summary-calls provider)))
  (call-next-method))

(-> unsupported-native-test--run (&key (:status (or integer keyword)) (:reactive-p boolean)
                                     (:permit-p boolean)) null)
(defun unsupported-native-test--run (&key status reactive-p permit-p)
  "Exercise actual manual or C1 operations and retain native settlement witnesses."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                                  (make-instance 'unsupported-native-test-provider
                                                                       :configuration configuration :status status
                                                                       :credential-manager (provider-credential-manager (provider-create configuration))
                                                                       :window             (when permit-p 50000)
                                                                       :results            (list (agent-test-result
                                                                                                  "summary" (list (agent-test-compaction-message))
                                                                                                  :usage (json-object "input_tokens" 7 "output_tokens" 3))
                                                                                                 (agent-test-result
                                                                                                  "answer" (list (agent-test-message "OK"))
                                                                                                  :usage (json-object "input_tokens" 2 "output_tokens" 1)))))
             (application                               (mission-inference-test--application configuration provider))
             (conversation                              (application-conversation application))
             (agent                                     (agent-create :configuration configuration :conversation conversation
                                                                      :provider provider :tool-registry (make-instance 'tool-registry)))
             (observer                                  (make-instance 'agent-observer))
             (*conversation-compaction-tail-characters* 256)
             (failure                                   nil)
             (unsupported-p                             (member status '(404 405 501))))
        (setf (application-agent application) agent
              (unsupported-native-test-source provider) conversation)
        (conversation-append-user-message conversation "old exchange")
        (if permit-p
            (conversation-append-provider-item conversation (agent-test-message (make-string 40000 :initial-element #\h)))
            (conversation-append-provider-item
             conversation (json-object "type" "reasoning" "encrypted_content" (make-string 40000 :initial-element #\e))))
        (conversation-append-user-message conversation "continue")
        (application-mission-start application (mission-test--specification :tokens 100000 :turns 16))
        (setf (unsupported-native-test-context provider) (mission-context-find conversation))
        (unwind-protect
             (progn
               (compaction-recovery-test-small-context
                (lambda ()
                  (test-call-with-function-replacements
                   (list (list 'credential-manager-credentials
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (provider-tests--credentials configuration)))
                         (list 'agent-should-compact-p (lambda (agent) (declare (ignore agent)) nil)))
                   (lambda ()
                     (handler-case
                         (if reactive-p
                             (agent--run-provider-loop agent observer :tools-p nil)
                             (application-compact application))
                       (error (condition) (setf failure condition)))))))
               (when failure (format t "~&FIXTURE-FAILURE ~A~%" failure))
               (let* ((ledger   (mission-context-goal (unsupported-native-test-context provider)))
                      (snapshot (first (unsupported-native-test-settlements provider))))
                 (format t "~&UNSUPPORTED-WITNESS status=~S reactive=~S permit=~S failure=~S native=~D summary=~D settlement=~S final=~S~%"
                         status reactive-p permit-p (and failure (type-of failure))
                         (unsupported-native-test-native-calls provider)
                         (unsupported-native-test-summary-calls provider) snapshot ledger)
                 (test-assert (= 1 (unsupported-native-test-native-calls provider)) "One native dispatch reaches transport.")
                 (test-assert (zerop (getf ledger :requests-outstanding 0)) "All final requests settle.")
                 (test-assert (zerop (getf ledger :tokens-reserved 0)) "All final allocations settle.")
                 (when unsupported-p
                   (test-assert (null failure) "Unsupported native dispatch falls back without mission error.")
                   (test-assert (eq :active (getf ledger :status)) "Unsupported native dispatch preserves ACTIVE status.")
                   (test-assert (= 1 (unsupported-native-test-summary-calls provider)) "Portable fallback is admitted exactly once.")
                   (test-assert (= (if reactive-p 13 10) (getf ledger :tokens-used 0)) "Only legitimate positive summary and resumed usage is charged.")
                   (dolist (key '(:tokens-used :unknown-usage :reservation-charged-tokens :tokens-reserved :requests-outstanding))
                     (test-assert (and snapshot (zerop (getf snapshot key 0)))
                                  (format nil "Native settlement is known zero before summary admission: ~S" key))))
                 (unless unsupported-p
                   (test-assert (typep failure 'mission-error) "Unrelated native failures retain mission handling.")
                   (test-assert (= 1 (getf ledger :unknown-usage 0)) "Unrelated native usage stays uncertain.")
                   (test-assert (zerop (unsupported-native-test-summary-calls provider)) "Unrelated failures never take unsupported fallback."))))
          (mission-detach application)))))
  nil)

(-> test-unsupported-native-mission-fallback () null)
(defun test-unsupported-native-mission-fallback ()
  "Cover all unsupported HTTP statuses and unrelated failures in manual and C1 paths."
  (dolist (reactive-p '(nil t))
    (dolist (status '(404 405 501 500 :transport))
      (unsupported-native-test--run :status status :reactive-p reactive-p)))
  nil)

(-> test-unsupported-native-metadata-handoff () null)
(defun test-unsupported-native-metadata-handoff ()
  "A refused native attempt must not spend the portable summary's metadata allowance."
  (dolist (reactive-p '(nil t))
    (dolist (status '(404 405 501))
      (unsupported-native-test--run :status status :reactive-p reactive-p :permit-p t)))
  nil)

(-> test-compaction-recovery-notice-counts () null)
(defun test-compaction-recovery-notice-counts ()
  "Count interleaved recovery tiers in first-seen order without changing audit data."
  (let* ((stages (append (list (list :tier :metadata) (list :tier :tool-output)
                              (list :tier :plain-text))
                         (loop repeat 26 collect (list :tier :tool-output))
                         (list (list :tier :metadata))))
         (audit  (list :outcome :success :stages stages))
         (before (copy-tree audit))
         (notice (compaction-recovery-notice audit)))
    (test-assert (= 1 (cl-ppcre:count-matches "tool-output" notice)) "Repeated tiers appear once.")
    (test-assert (search "tool-output (27)" notice) "Multiplicity reflects every interleaved stage.")
    (test-assert (< (search "metadata" notice) (search "tool-output" notice) (search "plain-text" notice))
                 "Tiers follow first-seen chronological order.")
    (test-assert (equal before audit) "The durable detailed audit is untouched.")
    (test-assert (null (compaction-recovery-notice nil)) "Absent audit has no notice."))
  nil)

(-> test-unsupported-native-permit-controls () null)
(defun test-unsupported-native-permit-controls ()
  "Limit a metadata allowance to one native-to-portable handoff at real dispatch."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (status '(404 405 501 200 500 :transport :local))
      (with-test-configuration (configuration)
        (let* ((provider      (make-instance 'unsupported-native-test-provider
                                             :configuration      configuration
                                             :credential-manager (provider-credential-manager (provider-create configuration))
                                             :status             (if (eq status :local) 404 status)
                                             :results            (list (agent-test-result
                                                                        "summary" (list (agent-test-compaction-message))
                                                                        :usage (json-object "input_tokens" 7 "output_tokens" 3)))))
               (application   (mission-inference-test--application configuration provider))
               (source        (application-conversation application))
               (candidate     (conversation-create configuration))
               (permit        (list provider (if (eq status :local) 5001 50000) :ready))
               (failure       nil)
               (unsupported-p (member status '(404 405 501))))
          (conversation-append-user-message candidate (make-string 40000 :initial-element #\x))
          (application-mission-start application (mission-test--specification :tokens 100000 :turns 16))
          (setf (unsupported-native-test-source provider) source
                (unsupported-native-test-context provider) (mission-context-find source))
          (unwind-protect
               (compaction-recovery-test-small-context
                (lambda ()
                  (test-call-with-function-replacements
                   (list (list 'credential-manager-credentials
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (provider-tests--credentials configuration))))
                   (lambda ()
                     (let ((*provider-compaction-recovery-permit* permit))
                       (flet ((native ()
                                (mission-call-with-native-compaction-view
                                 (unsupported-native-test-context provider) candidate
                                 (lambda ()
                                   (provider-attempt-native-compaction
                                    provider candidate :tool-namespaces #() :force-refresh nil
                                    :event-callback #'identity)))))
                         (handler-case (native)
                           (error (condition) (setf failure condition)))
                         (test-assert (= (if (eq status :local) 0 1) (unsupported-native-test-native-calls provider))
                                      "Only locally admitted attempts reach native dispatch.")
                         (cond
                           (unsupported-p
                            (test-assert (null failure) "Unsupported native dispatch settles normally.")
                            ;; A second native attempt must not use the portable allowance.
                            (setf failure nil)
                            (handler-case (native)
                              (error (condition) (setf failure condition)))
                            (test-assert (typep failure 'provider-context-budget-error)
                                         "The handoff cannot fund a repeated native request.")
                            (test-assert (= 1 (unsupported-native-test-native-calls provider))
                                         "Repeated native refusal reaches no new transport.")
                            (mission-call-with-request-view
                             (unsupported-native-test-context provider) candidate
                             (lambda ()
                               (provider-stream-turn provider candidate :tool-namespaces #()
                                                     :event-callback #'identity :compaction-p t)))
                            (test-assert (= 1 (unsupported-native-test-summary-calls provider))
                                         "Exactly one portable request uses the handoff.")
                            (setf failure nil)
                            (handler-case
                                (mission-call-with-request-view
                                 (unsupported-native-test-context provider) candidate
                                 (lambda ()
                                   (provider-stream-turn provider candidate :tool-namespaces #()
                                                         :event-callback                     #'identity)))
                              (error (condition) (setf failure condition)))
                            (test-assert (typep failure 'provider-context-budget-error)
                                         "Later ordinary requests cannot reuse the spent allowance."))
                           ((eq status :local)
                            (test-assert (typep failure 'provider-context-budget-error)
                                         "Local refusal never consumes an allowance."))
                           ((eql status 200)
                            (test-assert (null failure) "A genuine native result retains positive usage.")
                            (test-assert (= 5000 (provider-effective-context-window provider))
                                         "A successful native request does not renew an allowance."))
                           (t
                            (test-assert (typep failure 'mission-error)
                                         "Other admitted failures retain mission error handling.")
                            (test-assert (= 5000 (provider-effective-context-window provider))
                                         "Unrelated failures do not renew an allowance.")))))))))
            (let ((ledger (mission-context-goal (unsupported-native-test-context provider))))
              (format t "~&PERMIT-CONTROL status=~S permit=~S used=~D unknown=~D~%"
                      status (third permit) (getf ledger :tokens-used) (getf ledger :unknown-usage 0))
              (test-assert (= (cond (unsupported-p
                                     10)
                                    ((eql status 200)
                                     5)
                                    (t
                                     0)) (getf ledger :tokens-used))
                           "Native and portable positive usage retain their exact charges.")
              (test-assert (= (if (member status '(500 :transport)) 1 0) (getf ledger :unknown-usage 0))
                           "Only unrelated admitted failures have unknown usage.")
              (test-assert (zerop (getf ledger :requests-outstanding)) "Every control settles its reservation.")
              (test-assert (zerop (getf ledger :tokens-reserved)) "Every control releases reserved tokens."))
            (mission-detach application))))))
  nil)
