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
  "Keep correlated neighbors and detach arbitrary metadata before removing old reasoning."
  (with-test-configuration (configuration)
    (let* ((state        (make-compaction-recovery))
           (conversation (conversation-create configuration))
           (old          (json-object "type" "reasoning" "encrypted_content" "old encrypted"))
           (plain        (json-object "type" "reasoning" "summary" #("plaintext reasoning")))
           (paired       (json-object "type" "reasoning" "encrypted_content" "paired encrypted"))
           (call         (compaction-tail-tests--call "paired"))
           (output       (compaction-tail-tests--output "paired" "answer"))
           (after-output (json-object "type" "reasoning" "encrypted_content" "after output"))
           (anchor       (json-object "type" "compaction" "encrypted_content" "native anchor"))
           (after-anchor (json-object "type" "reasoning" "encrypted_content" "after anchor"))
           (separator    (json-object "type" "message" "role" "assistant" "content" "separator"))
           (old-second   (json-object "type" "reasoning" "encrypted_content" "second old encrypted"))
           (current      (json-object "type" "reasoning" "encrypted_content" "current encrypted"))
           (metadata     (json-object "nested" (vector (list (copy-seq "source metadata")))))
           (view         (conversation-compaction-summary-view
                          conversation (list old plain paired call output after-output anchor after-anchor
                                             separator old-second
                                             (json-object "type" "message" "role" "user" "content" "latest") current))))
      (setf (gethash plain (clinker-transcript:projection-metadata-table
                            (conversation-projection view) "extra-namespace")) metadata
            (gethash plain (clinker-transcript:projection-metadata-table
                            (conversation-projection view) ':explicit-nil)) nil
            (gethash old (clinker-transcript:projection-metadata-table
                          (conversation-projection view) ':discarded)) t)
      (let* ((detached  (compaction-recovery-view state view))
             (candidate (compaction-recovery-strip-encrypted state detached))
             (copy      (second (conversation-input-items detached)))
             (copied-metadata
               (gethash copy (clinker-transcript:projection-metadata-table
                              (conversation-projection candidate) "extra-namespace"))))
        (dolist (text '("old encrypted" "second old encrypted"))
          (test-assert (not (test-object-contains-string-p (conversation-input-items candidate) text))
                       "Eligible standalone old encrypted content is removed."))
        (dolist (text '("plaintext reasoning" "paired encrypted" "paired" "after output"
                        "native anchor" "after anchor" "current encrypted" "latest"))
          (test-assert (test-object-contains-string-p (conversation-input-items candidate) text)
                       "Original linked neighbors and protected reasoning survive."))
        (test-assert (= 2 (getf (first (compaction-recovery-notes state)) :removed-items))
                     "The chronological pass reports both eligible removals.")
        (test-assert (nth-value 1 (gethash copy (clinker-transcript:projection-metadata-table
                                                 (conversation-projection candidate) ':explicit-nil)))
                     "Present NIL metadata survives detachment and removal.")
        (test-assert (zerop (hash-table-count
                             (clinker-transcript:projection-metadata-table
                              (conversation-projection candidate) ':discarded)))
                     "Removed roots leave no metadata entries.")
        (setf (char (first (aref (json-get copied-metadata "nested") 0)) 0) #\X)
        (test-assert (string= "source metadata" (first (aref (json-get metadata "nested") 0)))
                     "Nested arbitrary metadata is detached from canonical history.")
        (test-assert (equal "old encrypted" (json-get old "encrypted_content"))
                     "Detached removal leaves source objects unchanged."))))
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
      (let ((output (first candidates)))
        (setf (gethash output (clinker-transcript:projection-metadata-table
                               (conversation-projection view) ':explicit-nil)) nil
              (gethash output (clinker-transcript:projection-metadata-table
                               (conversation-projection view) "stub-annotation"))
              (json-object "nested" (vector (copy-seq "candidate metadata")))))
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
        (let* ((stub     (second outputs))
               (source   (gethash (first candidates)
                                 (clinker-transcript:projection-metadata-table
                                  (conversation-projection view) "stub-annotation")))
               (metadata (gethash stub (clinker-transcript:projection-metadata-table
                                        (conversation-projection candidate) "stub-annotation"))))
          (test-assert (and (nth-value 1
                             (gethash stub (clinker-transcript:projection-metadata-table
                                            (conversation-projection candidate) ':explicit-nil)))
                           (null (gethash stub (clinker-transcript:projection-metadata-table
                                                (conversation-projection candidate) ':explicit-nil))))
                       "The exact stub candidate preserves present NIL metadata.")
          (test-assert (and metadata (string= (aref (json-get metadata "nested") 0)
                                              "candidate metadata"))
                       "The exact stub candidate inherits arbitrary output metadata.")
          (setf (char (aref (json-get metadata "nested") 0) 0) #\X)
          (test-assert (string= (aref (json-get source "nested") 0) "candidate metadata")
                       "Stub metadata is detached from the preceding candidate."))
        (let* ((text   (compaction-recovery-text-view state candidate))
               (newest (compaction-recovery-drop-exchange text)))
          (test-assert (= 1 (length (conversation-input-items newest)))
                       "Plain recovery drops the oldest whole exchange.")
          (test-assert (test-object-contains-string-p (conversation-input-items newest)
                                                     (make-string 180000 :initial-element #\p))
                       "The newest protected text is never clipped.")
          (test-assert (null (compaction-recovery-drop-exchange newest))
                       "The newest exchange cannot be evicted."))))
    (let* ((conversation (conversation-create configuration))
           (state        (make-compaction-recovery))
           (items        (list (compaction-tail-tests--call "boundary")
                               (compaction-tail-tests--output "boundary" "old output")
                               (json-object "type" "compaction" "encrypted_content" "native")
                               (json-object "type" "message" "role" "user" "content"
                                            (make-string 180000 :initial-element #\p))))
           (view         (compaction-recovery-view
                          state (conversation-compaction-summary-view conversation items))))
      (test-assert (= 2 (compaction-recovery-protected-start state view))
                   "The native neighbor begins the protected suffix.")
      (test-assert (null (compaction-recovery-stub-candidates state view))
                   "The scan sees a native next item across the protected boundary.")))
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

(-> compaction-image-tests--image (integer) json-object)
(defun compaction-image-tests--image (size)
  "Build synthetic media without opening or decoding an image."
  (json-object "type" "input_image" "detail" "high"
               "image_url" (concatenate 'string "data:image/png;base64,PAYLOAD-MARKER"
                                         (make-string size :initial-element #\a))
               "private_metadata" "IMAGE-SECRET-MARKER"))

(-> compaction-image-tests--items (integer &key (:separate-p boolean)) list)
(defun compaction-image-tests--items (size &key separate-p)
  "Build forty-seven recent fs.view-image-style correlated outputs."
  (let ((items nil))
    (unless separate-p
      (push (json-object "type" "message" "role" "user" "content"
                         (concatenate 'string "LATEST-INSTRUCTION " (make-string 32000 :initial-element #\u)))
            items))
    (dotimes (index 47)
      (when separate-p
        (push (json-object "type" "message" "role" "user" "content"
                           (format nil "Recent instruction ~D ~A" index (make-string 680 :initial-element #\u)))
              items))
      (let ((id (format nil "image-call-~D" index)))
        (push (compaction-tail-tests--call id) items)
        (push (json-object "type" "function_call_output" "call_id" id
                           "output" (vector (json-object "type" "input_text" "text" "Viewed synthetic PNG")
                                            (compaction-image-tests--image size)))
              items)))
    (nreverse items)))

(-> compaction-image-tests--transcript (conversation) list)
(defun compaction-image-tests--transcript (view)
  "Read the structured exchanges from an emergency text projection."
  (loop for item in (conversation-input-items view)
        append (coerce (json-decode (json-get (aref (json-get item "content") 0) "text")) 'list)))

(-> test-compaction-recovery-image-projection () null)
(defun test-compaction-recovery-image-projection ()
  "Bound semantic media nodes without redacting ordinary text or changing source objects."
  (with-test-configuration (configuration)
    (let ((lengths nil))
      (dolist (size '(16 100000 3000000))
        (let* ((conversation (conversation-create configuration))
               (state        (make-compaction-recovery))
               (literal      "literal data:image/png;base64,ORDINARY-TEXT")
               (items        (list (json-object "type" "message" "id" "old-user" "role" "user"
                                                "content" (vector (compaction-image-tests--image size)))
                                   (compaction-tail-tests--call "paired-image")
                                   (json-object "type" "function_call_output" "call_id" "paired-image"
                                                "output" (vector (json-object "type" "input_text" "text" literal)
                                                                 (json-object "type" "image_url" "image_url"
                                                                              (json-object "url" "https://example.invalid/?token=URL-SECRET-MARKER"))))
                                   (json-object "type" "message" "id" "newest-user" "role" "user"
                                                "content" (vector (json-object "type" "input_text" "text" "LATEST-INSTRUCTION")
                                                                  (compaction-image-tests--image size)))))
               (before       (mapcar #'json-encode items))
               (source       (conversation-compaction-summary-view conversation items))
               (view         (compaction-recovery-text-view state (compaction-recovery-view state source)))
               (encoded      (json-encode (coerce (conversation-input-items view) 'vector)))
               (restored     (compaction-image-tests--transcript view)))
          (push (length encoded) lengths)
          (test-assert (< (length encoded) 3000) "Emergency image payload is bounded independently of bytes.")
          (dolist (secret '("PAYLOAD-MARKER" "IMAGE-SECRET-MARKER" "URL-SECRET-MARKER"))
            (test-assert (not (search secret encoded)) "Entire image media nodes are omitted, including metadata and URLs."))
          (test-assert (equal before (mapcar #'json-encode items)) "Canonical media and source containers are unchanged.")
          (test-assert (equal '("old-user" nil nil "newest-user")
                              (mapcar (lambda (item) (json-get item "id")) restored))
                       "Item IDs and exchange order survive the text representation.")
          (test-assert (equal "paired-image" (json-get (third restored) "call_id")) "Output correlation survives.")
          (test-assert (test-object-contains-string-p restored literal) "Literal data URLs in text are not media.")
          (test-assert (test-object-contains-string-p restored "LATEST-INSTRUCTION") "Newest user text is intact.")
          (let ((notice (aref (json-get (first restored) "content") 0)))
            (test-assert (and (json-string= (json-get notice "type") "input_text")
                              (plusp (length (or (json-get notice "text") "")))
                              (search "unavailable" (json-get notice "text")))
                         "Image-only exchanges have an explicit unavailable-visual-content notice."))
          (let ((audit (find :plain-text (compaction-recovery-notes state) :key (lambda (note) (getf note :tier)))))
            (test-assert (= 3 (getf audit :images-omitted 0)) "One bounded aggregate records all omitted images."))
          (test-assert (equal (mapcar #'json-encode (conversation-input-items view))
                              (mapcar #'json-encode
                                      (conversation-input-items
                                       (compaction-recovery-text-view (make-compaction-recovery) source))))
                       "Independent retries produce the same detached transcript.")))
      (test-assert (apply #'= lengths) "Several orders of payload magnitude produce identical transcript size.")))
  nil)

(-> test-compaction-recovery-recent-images () null)
(defun test-compaction-recovery-recent-images ()
  "Admit the real summary projection with forty-seven protected recent images at 600K."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "4096"))
    (with-test-configuration (configuration)
      (dolist (separate-p '(nil t))
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance 'compaction-recovery-test-provider :configuration configuration))
               (state        (make-compaction-recovery))
               (items        (compaction-image-tests--items 100000 :separate-p separate-p))
               (before       (mapcar #'json-encode items))
               (source       (conversation-compaction-summary-view conversation items))
               (view         (compaction-recovery-text-view state (compaction-recovery-view state source))))
          (compaction-recovery-test-small-context
           (lambda ()
             (let* ((raw     (provider-request-object provider source #() :compaction-p t))
                    (request (provider-request-object provider view #() :compaction-p t))
                    (decoded (compaction-image-tests--transcript view))
                    (outputs (remove-if-not (lambda (item) (json-string= (json-get item "type") "function_call_output")) decoded)))
               (test-assert (> (provider-request-input-token-estimate provider raw) 600000)
                            "The unknown-model conservative raw image allowance exceeds this window.")
               (test-assert (= 47 (length outputs)) "All recent correlated outputs are retained.")
               (test-assert (every (lambda (item)
                                    (json-string= (json-get (aref (json-get item "output") 1) "type") "input_text")) outputs)
                            "Every recent image, including protected newest images, is represented by a notice.")
               (test-assert (null (handler-case (provider-check-request-context provider request)
                                    (provider-context-budget-error (condition) condition)))
                            "Actual summary request plus positive output reserve fits 600K after Tier3.")
               (test-assert (>= (provider-request-output-reserve provider request) 4096) "Configured and wire output headroom is charged.")
               (format t "~&RECENT-IMAGES separate=~S raw=~D projected=~D reserve=~D~%"
                       separate-p (provider-request-input-token-estimate provider raw)
                       (provider-request-input-token-estimate provider request)
                       (provider-request-output-reserve provider request)))))
          (test-assert (equal before (mapcar #'json-encode items)) "Source images survive summary preflight unchanged.")))))
  nil)

(-> test-compaction-recovery-image-text-controls () null)
(defun test-compaction-recovery-image-text-controls ()
  "Keep newest image-only input useful and ordinary text fully costed."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (provider     (make-instance 'context-admission-test-provider :configuration configuration))
           (state        (make-compaction-recovery))
           (source       (conversation-compaction-summary-view
                          conversation (list (json-object "type" "message" "role" "user" "content"
                                                           (vector (compaction-image-tests--image 1000000))))))
           (view         (compaction-recovery-text-view state source))
           (notice       (aref (json-get (first (compaction-image-tests--transcript view)) "content") 0)))
      (test-assert (json-string= (json-get notice "type") "input_text") "Newest image-only input becomes useful text.")
      (test-assert (search "unavailable" (or (json-get notice "text") "")) "No invented visual description is supplied.")
      (test-assert (null (compaction-recovery-drop-exchange view)) "Newest exchange is never dropped.")
      (let ((counts nil))
        (dolist (size '(100 100000))
          (let* ((literal (concatenate 'string "data:image/png;base64," (make-string size :initial-element #\t)))
                 (item    (json-object "type" "message" "role" "user" "content"
                                      (vector (json-object "type" "input_text" "text" literal))))
                 (text    (compaction-recovery-text-view
                           (make-compaction-recovery) (conversation-compaction-summary-view conversation (list item)))))
            (test-assert (test-object-contains-string-p (compaction-image-tests--transcript text) literal)
                         "Ordinary image-looking text is preserved verbatim.")
            (push (provider-request-input-token-estimate
                   provider (json-object "input" (coerce (conversation-input-items text) 'vector))) counts)))
        (test-assert (> (first counts) (+ 20000 (second counts))) "Ordinary long text retains its full estimate."))))
  nil)

(-> test-compaction-recovery-image-summary-transport () null)
(defun test-compaction-recovery-image-summary-transport ()
  "Recover a real portable summary at the inert transport with positive reported usage."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "4096"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'compaction-recovery-test-provider
                                          :configuration configuration
                                          :results       (list (agent-test-result
                                                                "summary" (list (agent-test-compaction-message))
                                                                :usage (json-object "input_tokens" 120 "output_tokens" 30)))))
             (items        (compaction-image-tests--items 100000))
             (source       (conversation-compaction-summary-view conversation items))
             (before       (mapcar #'json-encode items))
             (state        (make-compaction-recovery))
             (result       nil)
             (candidate    nil)
             (failure      nil))
        (compaction-recovery-test-small-context
         (lambda ()
           (handler-case
               (multiple-value-setq (result candidate)
                 (compaction-recovery-call
                  state provider source
                  :dispatch  (lambda (view)
                               (provider-stream-turn provider view :tool-namespaces #() :compaction-p t
                                                     :event-callback (lambda (event) (declare (ignore event)))))
                  :preflight (lambda (view)
                               (provider-check-request-context
                                provider (provider-request-object provider view #() :compaction-p t)))))
             (provider-context-budget-error (condition) (setf failure condition)))))
        (test-assert (null failure) "All protected images permit portable recovery through the actual admission boundary.")
        (test-assert (= 1 (context-admission-test-provider-network-calls provider)) "Only the admitted summary reaches transport.")
        (when result
          (test-assert (= 120 (json-get (provider-result-usage result) "input_tokens")) "Positive paid input usage is preserved.")
          (test-assert (= 30 (json-get (provider-result-usage result) "output_tokens")) "Positive paid output usage is preserved."))
        (test-assert (equal before (mapcar #'json-encode items)) "Local refusals and successful recovery do not mutate canonical media.")
        (test-assert (= 1 (count :plain-text (compaction-recovery-notes state)
                               :key          (lambda (note) (getf note :tier)))) "Tier3 runs once across refused candidates.")
        (when candidate
          (let ((notes (copy-tree (compaction-recovery-notes state))))
            (compaction-recovery-test-small-context
             (lambda ()
               (provider-check-request-context
                provider (provider-request-object provider (compaction-recovery-view state candidate) #() :compaction-p t))))
            (test-assert (equal notes (compaction-recovery-notes state)) "Readmission does not duplicate omission audit entries."))))))
  nil)


;;;; -- Production Streaming Native Fallback --

(defclass expanded-native-test-provider (codex-subscription-provider)
  ((results :initarg :results :accessor expanded-native-results
            :documentation "Response data encoded into real completed SSE streams.")
   (input-snapshots :initform nil :accessor expanded-native-input-snapshots
                    :documentation "Input identities seen only at admitted opening I/O.")
   (compaction-flags :initform nil :accessor expanded-native-compaction-flags
                     :documentation "Summary role observed at admitted opening I/O.")
   (native-views :initform nil :accessor expanded-native-test-views
                 :documentation "Actual native opening conversation and mission context.")
   (after-summary :initform nil :accessor expanded-native-test-after-summary
                  :documentation "Canonical arrival delivered when the summary stream closes."))
  (:documentation "Retain production Codex streaming and replace only inert response I/O."))

(defmethod provider-compaction-recovery-window ((provider expanded-native-test-provider))
  "Keep the fixture's metadata ceiling equal to its configured window."
  (config :context-window (provider-configuration provider)))

(defclass expanded-native-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((source :initarg :source :reader expanded-native-input-source
           :documentation "A standard completed synthetic SSE response.")
   (on-close :initarg :on-close :initform nil :accessor expanded-native-input-on-close
             :documentation "Optional synthetic concurrent arrival at transport completion."))
  (:documentation "Character transport with an independently observable completion boundary."))

(defmethod trivial-gray-streams:stream-read-char ((stream expanded-native-input-stream))
  "Read the inert transport without bypassing the production event decoder."
  (read-char (expanded-native-input-source stream) nil :eof))

(defmethod trivial-gray-streams:stream-unread-char ((stream expanded-native-input-stream) character)
  "Return CHARACTER to the inert transport."
  (unread-char character (expanded-native-input-source stream)))

(defmethod close :after ((stream expanded-native-input-stream) &key abort)
  "Deliver one canonical concurrent arrival after a normally consumed summary stream."
  (unless abort
    (let ((callback (shiftf (expanded-native-input-on-close stream) nil)))
      (when callback (funcall callback)))))

(-> expanded-native-test--sse (provider-result) string)
(defun expanded-native-test--sse (result)
  "Encode fixture data as standard created, output-item and completed response events."
  (with-output-to-string (stream)
    (write-string (test-sse-event-string
                   (json-object "type" "response.created"
                                "response" (json-object "id" (provider-result-response-id result))))
                  stream)
    (dolist (item (provider-result-output-items result))
      (write-string (test-sse-event-string (json-object "type" "response.output_item.done" "item" item)) stream))
    (write-string (test-sse-event-string
                   (json-object "type" "response.completed"
                                "response" (json-object "id" (provider-result-response-id result)
                                                        "usage" (provider-result-usage result))))
                  stream)))

(defmethod provider-open-native-compaction :before
    ((provider expanded-native-test-provider) (request hash-table) &key credentials conversation)
  "Observe actual native opening while retaining production POST and final admission."
  (declare (ignore request credentials))
  (push (list conversation (mission-context-find conversation)) (expanded-native-test-views provider)))

(defmethod provider-open-response-stream
    ((provider expanded-native-test-provider) (request hash-table) &key credentials conversation)
  "Return inert SSE I/O after production projection, attempt, encoder and final admission."
  (declare (ignore request credentials))
  (let* ((summary-p (eq *telemetry-request-role* :compaction))
         (result    (pop (expanded-native-results provider)))
         (callback  (and summary-p (shiftf (expanded-native-test-after-summary provider) nil))))
    (assert result)
    (push (conversation-input-items conversation) (expanded-native-input-snapshots provider))
    (push summary-p (expanded-native-compaction-flags provider))
    (format t "~&EXPANDED-STREAM role=~S sequence=~D~%" (if summary-p :summary :answer)
            (length (expanded-native-input-snapshots provider)))
    (values (make-instance 'expanded-native-input-stream
                           :source   (make-string-input-stream (expanded-native-test--sse result))
                           :on-close callback)
            200 nil)))

;; Couple small histories to an earlier soft threshold and a bounded hard window.
;; Keep the actual system instructions and encoded request admission in every cell.
(-> expanded-native-test--soft-pressure (application) null)
(defun expanded-native-test--soft-pressure (application)
  "Configure the smaller fixture's soft limit and check actual input pressure."
  (setf (config :compaction-threshold-percent (application-configuration application)) 35)
  (let* ((agent         (application-agent application))
         (configuration (application-configuration application))
         (input         (context-estimate-known-tokens
                         configuration (application-conversation application)
                         :tool-registry (agent-tool-registry agent)
                         :hurry-up-p    (agent-hurry-up-p agent)
                         :family        (provider-family (agent-provider agent))))
         (threshold     (configuration-compaction-token-limit configuration)))
    (test-assert (>= input threshold) "The reduced history still crosses the real soft compaction threshold.")
    (format t "~&SOFT-PRESSURE input=~D threshold=~D window=~D~%"
            input threshold (config :context-window configuration)))
  nil)

(-> expanded-native-test--refusal-budget (provider-context-budget-error) null)
(defun expanded-native-test--refusal-budget (condition)
  "Require a measured hard overflow from the original final-request guard."
  (let ((input   (provider-context-budget-error-estimated-input-tokens condition))
        (reserve (provider-context-budget-error-output-reserve condition))
        (window  (provider-context-budget-error-context-window condition)))
    (test-assert (> (+ input reserve) window)
                 "The reduced arrival still exceeds actual encoded request admission.")
    (format t "~&FINAL-REFUSAL input=~D reserve=~D window=~D~%" input reserve window))
  nil)

(-> expanded-native-test--fixture (function &key (:owned-p boolean) (:large-p boolean)) null)
(defun expanded-native-test--fixture (function &key owned-p large-p)
  "Call FUNCTION with a real application, optional mission, and inert Codex transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'expanded-native-test-provider
                                         :configuration      configuration
                                         :credential-manager (provider-credential-manager (provider-create configuration))
                                         :results            (list (agent-test-result
                                                                    "summary" (list (agent-test-compaction-message))
                                                                    :usage (json-object "input_tokens" 100 "output_tokens" 50))
                                                                   (agent-test-result
                                                                    "answer" (list (agent-test-message "Resumed answer."))
                                                                    :usage (json-object "input_tokens" 20 "output_tokens" 10)))))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (setf (application-agent application) agent
              (config :session-title-generation-p configuration) nil)
        (conversation-append-user-message conversation "An earlier text-only exchange.")
        (conversation-append-provider-item
         conversation (agent-test-message (if large-p (make-string 190000 :initial-element #\h) "Earlier answer.")))
        (conversation-append-user-message conversation "Continue the actual work.")
        (when large-p (expanded-native-test--soft-pressure application))
        (when owned-p
          (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32)))
        (unwind-protect
             (funcall function application provider)
          (when owned-p (mission-detach application))
          (remhash application *mission-schedule-services*)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> expanded-native-test--transport
    (application &key (:status integer) (:signaled-p boolean) (:function function)
                      (:failure (option condition)) (:expected-posts (integer 0))) null)
(defun expanded-native-test--transport
    (application &key status signaled-p function failure (expected-posts 1))
  "Invoke FUNCTION with a true Dexador condition or its returned-status control at POST."
  (let ((calls         0)
        (configuration (application-configuration application)))
    (test-call-with-function-replacements
     (list (list 'credential-manager-credentials
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (provider-tests--credentials configuration)))
           (list 'dexador:post
                 (lambda (endpoint &rest arguments)
                   (declare (ignore arguments))
                   (incf calls)
                   (test-assert (search "/responses/compact" endpoint) "The actual native POST boundary is reached.")
                   (when failure (error failure))
                   (if signaled-p
                       (error 'http-request-failed :status status :body "Native endpoint unavailable."
                                                  :headers nil :uri endpoint :method ':post)
                       (values "Native endpoint unavailable." status nil)))))
     function)
    (test-assert (= expected-posts calls) "Only admitted native requests reach the actual POST boundary.")
    (format t "~&NATIVE-HTTP status=~D signaled=~S POST=~D~%" status signaled-p calls))
  nil)

(-> test-expanded-native-unavailable-negative-controls () null)
(defun test-expanded-native-unavailable-negative-controls ()
  "Keep authentication, transient HTTP, and uncertain transport failures outside fallback."
  (dolist (status '(401 429 500 503 0))
    (dolist (owned-p '(nil t))
      (expanded-native-test--fixture
       (lambda (application provider)
         (let ((*provider-maximum-transient-retries* 0)
               (failure                              nil))
           (test-call-with-function-replacements
            (list (list 'credential-manager-refreshable-p
                        (lambda (manager) (declare (ignore manager)) nil)))
            (lambda ()
              (expanded-native-test--transport
               application :status status :signaled-p t
               :failure (when (zerop status)
                          (make-condition 'end-of-file :stream (make-string-input-stream "")))
               :function
               (lambda ()
                 (handler-case (application-compact application)
                   (error (condition) (setf failure condition)))))))
           (format t "~&NATIVE-NEGATIVE status=~D owned=~S failure=~S~%" status owned-p (type-of failure))
           (test-assert failure "An unrelated native failure must propagate to its existing owner.")
           (test-assert (not (typep failure 'provider-context-budget-error))
                        "An unrelated native failure is not a local context refusal.")
           (test-assert (null (expanded-native-input-snapshots provider))
                        "No portable summary or ordinary request follows an unrelated failure.")
           (test-assert (not (find :summary
                                  (conversation-records-newest (application-conversation application) 30)
                                  :key #'first))
                        "An unrelated failure cannot publish a portable checkpoint.")
           (when owned-p
             (test-assert (plusp (getf (application-goal application) :unknown-usage 0))
                          "Uncertain request usage is never settled as known zero."))))
       :owned-p owned-p :large-p t)))
  nil)

(-> test-expanded-native-unavailable-representations () null)
(defun test-expanded-native-unavailable-representations ()
  "Classify both wire representations before bound and detached mission supervision."
  (dolist (status '(404 405 501))
    (dolist (signaled-p '(nil t))
      (dolist (ownership '(:unowned :bound :detached))
        (expanded-native-test--fixture
         (lambda (application provider)
           (let* ((source  (application-conversation application))
                  (view    (if (eq ownership ':detached) (conversation-compaction-view (conversation-compaction-capture source)) source))
                  (context (mission-request-view-context source))
                  (failure nil) (results nil))
             (expanded-native-test--transport
              application :status status :signaled-p signaled-p :function
              (lambda ()
                (handler-case
                    (setf results
                          (multiple-value-list
                           (if (eq ownership ':detached)
                               (mission-call-with-native-compaction-view
                                context view
                                (lambda ()
                                  (provider-native-compact-conversation provider view :tool-namespaces #()
                                                                               :event-callback         #'identity)))
                               (provider-native-compact-conversation provider view :tool-namespaces #()
                                                                                  :event-callback   #'identity))))
                  (error (condition) (setf failure condition)))))
             (format t "~&NATIVE-REPRESENTATION owner=~S failure=~S values=~S~%"
                     ownership (and failure (type-of failure)) results)
             (test-assert (null failure) "An unavailable endpoint returns normally before mission supervision.")
             (test-assert (null (first results)) "No opaque native checkpoint is fabricated.")
             (when context
               (let ((ledger (mission-context-goal context)))
                 (test-assert (zerop (getf ledger :requests-outstanding 0)) "Every native request settles.")
                 (test-assert (zerop (getf ledger :tokens-reserved 0)) "Every native reservation settles.")
                 (test-assert (zerop (getf ledger :unknown-usage 0)) "A definitive unavailable endpoint has known usage.")
                 (test-assert (zerop (getf ledger :tokens-used 0)) "A definitive unavailable endpoint charges no inference.")))
             (when (eq ownership ':detached)
               (test-assert (null (mission-context-find view)) "Detached request ownership is released."))))
         :owned-p (not (eq ownership ':unowned))))))
  nil)

(-> test-expanded-native-unavailable-ordinary () null)
(defun test-expanded-native-unavailable-ordinary ()
  "Continue real large text-only proactive turns after every native unavailable status."
  (dolist (status '(404 405 501))
    (dolist (owned-p '(nil t))
      (expanded-native-test--fixture
       (lambda (application provider)
         (let* ((agent        (application-agent application))
                (conversation (application-conversation application))
                (failure nil) (result nil))
           (test-assert (agent-should-compact-p agent) "The real large-text estimate selects proactive compaction.")
           (expanded-native-test--transport
            application :status status :signaled-p t :function
            (lambda ()
              (handler-case
                  (setf result (agent--run-provider-loop agent (make-instance 'agent-observer) :tools-p nil))
                (error (condition) (setf failure condition)))))
           (format t "~&NATIVE-ORDINARY owner=~S status=~D failure=~S flags=~S~%"
                   owned-p status (and failure (type-of failure)) (expanded-native-compaction-flags provider))
           (test-assert (null failure) "Unavailable native compaction must not abort the ordinary turn.")
           (test-assert (and result (equal "Resumed answer." (provider-result-assistant-text result)))
                        "The ordinary answer really continues after the portable summary.")
           (test-assert (equal '(nil t) (expanded-native-compaction-flags provider))
                        "One admitted summary and one admitted ordinary answer complete.")
           (test-assert (find :summary (conversation-records-newest conversation 30) :key #'first)
                        "The portable checkpoint is durably published.")
           (let ((loaded (conversation-load-by-id (application-configuration application)
                                                  (conversation-identifier conversation))))
             (test-assert (equal (mapcar #'json-encode (conversation-input-items conversation))
                                 (mapcar #'json-encode (conversation-input-items loaded)))
                          "Reload preserves the actual published projection."))
           (when owned-p
             (let ((ledger (application-goal application)))
               (test-assert (= 180 (getf ledger :tokens-used 0)) "Positive summary and answer usage settle once.")
               (test-assert (zerop (getf ledger :unknown-usage 0)) "Unavailable native usage is not quarantined.")
               (test-assert (zerop (getf ledger :requests-outstanding 0)) "No request remains outstanding.")
               (test-assert (zerop (getf ledger :tokens-reserved 0)) "No allocation remains reserved.")))))
       :owned-p owned-p :large-p t)))
  nil)


;;;; -- Native HTTP Sibling Lifecycles --

(-> expanded-native-test--settled (application integer) null)
(defun expanded-native-test--settled (application tokens)
  "Check the actual owner ledger after all admitted work has settled."
  (let ((goal (application-goal application)))
    (test-assert (= tokens (getf goal :tokens-used 0)) "Only completed positive paid usage is charged.")
    (test-assert (eq :active (getf goal :status)) "A definitive unavailable endpoint preserves the active mission.")
    (dolist (key '(:requests-outstanding :tokens-reserved :unknown-usage))
      (test-assert (zerop (getf goal key 0)) (format nil "No residual ~A." key))))
  nil)

(-> expanded-native-test--checkpoint (application) null)
(defun expanded-native-test--checkpoint (application)
  "Verify publication from authoritative disk, independently of the active projection."
  (let ((loaded (conversation-load
                 (conversation-pathname (application-conversation application)))))
    (test-assert (find :summary (conversation-records-newest loaded 30) :key #'first)
                 "The portable summary survives canonical reload."))
  nil)

(-> test-expanded-native-unavailable-manual () null)
(defun test-expanded-native-unavailable-manual ()
  "Use manual application compaction with and without a mission owner."
  (dolist (status '(404 405 501))
    (dolist (owned-p '(nil t))
      (expanded-native-test--fixture
       (lambda (application provider)
         (let ((failure nil))
           (expanded-native-test--transport
            application :status status :signaled-p t :function
            (lambda ()
              (handler-case (application-compact application)
                (error (condition) (setf failure condition)))))
           (format t "~&NATIVE-MANUAL status=~D owned=~S failure=~S~%" status owned-p (type-of failure))
           (test-assert (null failure) "Manual compaction returns normally after portable fallback.")
           (test-assert (equal '(t) (expanded-native-compaction-flags provider)) "Manual compaction admits one paid summary.")
           (expanded-native-test--checkpoint application)
           (when owned-p (expanded-native-test--settled application 150))))
       :owned-p owned-p :large-p t)))
  nil)

(defclass expanded-native-output-observer (agent-observer)
  ((output-limit :initform 200000 :accessor expanded-native-output-limit
                 :documentation "One initial ordinary output allowance, consumed at request start."))
  (:documentation "A legitimate initial output allowance that unwinds before reactive compaction."))

(defmethod agent-observer-status
    ((observer expanded-native-output-observer) (status symbol) details)
  "Request a large output allowance once, using the ordinary observer protocol."
  (declare (ignore details))
  (when (eq status :provider-request-started)
    (shiftf (expanded-native-output-limit observer) nil)))

(-> test-expanded-native-unavailable-reactive () null)
(defun test-expanded-native-unavailable-reactive ()
  "Reject the initial wire envelope with a genuine output allowance, then recover."
  (dolist (status '(404 405 501))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((conversation (application-conversation application))
              (agent (application-agent application)) (failure nil)
              (observer     (make-instance 'expanded-native-output-observer)))
         (conversation-append-provider-item conversation (agent-test-message (make-string 400000 :initial-element #\h)))
         (conversation-append-user-message conversation "Continue the real text work.")
         (test-assert (not (agent-should-compact-p agent)) "The text conversation is below the real soft threshold.")
         (expanded-native-test--transport
          application :status status :signaled-p t :function
          (lambda ()
            (handler-case (agent--run-provider-loop agent observer :tools-p nil)
              (error (condition) (setf failure condition)))))
         (format t "~&NATIVE-REACTIVE status=~D failure=~S flags=~S~%" status (type-of failure)
                 (expanded-native-compaction-flags provider))
         (test-assert (null failure) "Final admission recovery reaches portable summary and resumed answer.")
         (test-assert (equal '(nil t) (expanded-native-compaction-flags provider)) "The rejected ordinary wire request is never paid.")
         (expanded-native-test--checkpoint application)
         (expanded-native-test--settled application 180)))
     :owned-p t))
  nil)

(-> test-expanded-native-unavailable-schedule () null)
(defun test-expanded-native-unavailable-schedule ()
  "Consume a genuine claimed occurrence through the default mission runner."
  (dolist (status '(404 405 501))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((now     100)
              (service (mission-schedule-service-create application :clock (lambda () now)
                         :enqueue                                          (lambda (owner command) (declare (ignore owner command)) t))))
         (setf (gethash application *mission-schedule-services*) service)
         (application-mission-schedule-add application :id "native" :content "Inspect scheduled work."
                                          :at 100 :interval 10)
         (let* ((occurrence (first (application-mission-schedules-wake application)))
                (ticket (mission-schedule-tests--ticket occurrence)) (failure nil))
           (test-assert (eq :claimed (getf occurrence :state)) "The real scheduler persists a claimed ticket before execution.")
           (expanded-native-test--transport application :status status :signaled-p t :function
            (lambda ()
              (handler-case (application-operation-call application "mission-wakeup" ticket)
                (error (condition) (setf failure condition)))))
           (format t "~&NATIVE-SCHEDULE status=~D failure=~S~%" status (type-of failure))
           (test-assert (null failure) "The default scheduled runner continues past unavailable native compaction.")
           (let ((wakeups (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups)))
             (test-assert (eq :completed (getf (first wakeups) :state)) "Only the completed answer marks the occurrence completed."))
           (let ((count (length (expanded-native-input-snapshots provider))))
             (handler-case (application-operation-call application "mission-wakeup" ticket)
               (mission-error () nil))
             (test-assert (= count (length (expanded-native-input-snapshots provider))) "Reconsuming the claimed ticket issues no request."))
           (expanded-native-test--checkpoint application)
           (expanded-native-test--settled application 180)
           (setf now 110
                 (expanded-native-results provider)
                 (list (agent-test-result "later" (list (agent-test-message "Later scheduled answer."))
                                          :usage (json-object "input_tokens" 20 "output_tokens" 10))))
           (let ((next (first (application-mission-schedules-wake application))))
             (test-assert next "The next recurring occurrence is independently eligible.")
             (when next
                 (expanded-native-test--transport
                  application :status status :signaled-p t :expected-posts 0
                  :function (lambda ()
                              (application-operation-call application "mission-wakeup" (mission-schedule-tests--ticket next))))
               (test-assert (= 3 (length (expanded-native-input-snapshots provider))) "A later recurrence actually answers."))))))
     :owned-p t :large-p t))
  nil)

(-> test-expanded-native-unavailable-terminal-idle () null)
(defun test-expanded-native-unavailable-terminal-idle ()
  "Run due terminal episodes with and without mission ownership, then accept later input."
  (dolist (status '(404 405 501))
    (dolist (owned-p '(nil t))
      (expanded-native-test--fixture
       (lambda (application provider)
         (let* ((controller                       (make-instance 'application-input-controller :application application :main-thread (current-thread)))
                (*idle-compaction-clock-function* (lambda () 1180)))
           (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
           (setf (application-input-controller application) controller
                 (config :prompt-cache-retention-seconds (application-configuration application)) 300)
           (idle-compaction-tests--prime application)
           (let ((work (application-input-controller--next-work controller)))
             (test-assert (eq :idle-compaction (first work)) "A due real episode selects idle work.")
             (expanded-native-test--transport application :status status :signaled-p t :function
              (lambda () (application-input-controller--run-work controller work)))
             (application-input-controller--finish-work controller)
             (expanded-native-test--checkpoint application)
             (test-assert (null (application-idle-compaction-episode application)) "The idle opportunity is consumed once.")
             (test-assert (not (application-input-controller-active-p controller)) "The terminal controller releases activity.")
             (test-assert (equal '(t) (expanded-native-compaction-flags provider)) "Idle maintenance completes one paid summary.")
             (when owned-p (expanded-native-test--settled application 150))
             (expanded-native-test--transport
              application :status status :signaled-p t :expected-posts 0
              :function (lambda ()
                          (application--run-message-input application "Later actual user input." :tools-p nil :goal-continuations-p nil)))
             (test-assert (equal '(nil t) (expanded-native-compaction-flags provider)) "Later input gets its ordinary answer.")
             (when owned-p (expanded-native-test--settled application 180)))
           (setf (application-input-controller application) nil)))
       :owned-p owned-p :large-p t)))
  nil)

(-> test-expanded-native-unavailable-acp-idle () null)
(defun test-expanded-native-unavailable-acp-idle ()
  "Use inert ACP session ownership, cleanup, and a later real prompt."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000") ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (acp-input-test--session
     (lambda (session client)
       (declare (ignore client))
       (let* ((application  (acp-session-application session))
              (provider     (application-provider application))
              (conversation (application-conversation application))
              (*idle-compaction-clock-function* (lambda () 1180)) (failure nil))
         (change-class provider 'expanded-native-test-provider
                       :credential-manager (credential-manager-create (application-configuration application)))
         (setf (slot-value provider 'cl-llm-provider-api::configuration) (application-configuration application)
               (expanded-native-results provider)
               (list (agent-test-result "summary" (list (agent-test-compaction-message)) :usage (json-object "input_tokens" 100 "output_tokens" 50))
                     (agent-test-result "answer" (list (agent-test-message "Later ACP answer.")) :usage (json-object "input_tokens" 20 "output_tokens" 10)))
               (config :prompt-cache-retention-seconds (application-configuration application)) 300)
         (conversation-append-user-message conversation "Earlier exchange.")
         (conversation-append-provider-item conversation (agent-test-message (make-string 190000 :initial-element #\h)))
         (conversation-append-user-message conversation "Continue.")
         (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32))
         (unwind-protect
              (progn
                (idle-compaction-tests--prime application)
                (expanded-native-test--transport application :status 404 :signaled-p t :function
                 (lambda () (handler-case (acp-completion--cycle session)
                              (error (condition) (setf failure condition)))))
                (format t "~&NATIVE-ACP failure=~S~%" (type-of failure))
                (test-assert (null failure) "ACP idle unavailability does not escape as an exception.")
                (expanded-native-test--checkpoint application)
                (test-assert (null (acp-session-prompt-thread session)) "ACP releases prompt ownership.")
                (test-assert (not (acp-session-prompt-interruptible-p session)) "ACP finalizes interruption state.")
                (test-assert (not (application-input-controller-active-p (acp-input-controller session))) "ACP releases controller activity.")
                (test-assert (null (application-idle-compaction-episode application)) "ACP consumes the opportunity once.")
                (test-assert (null (acp-completion--cycle session)) "Consumed ACP work cannot repeat.")
                (expanded-native-test--settled application 150)
                (expanded-native-test--transport
                 application :status 404 :signaled-p t :expected-posts 0
                 :function (lambda () (acp-session-run-turn session "Later actual ACP prompt.")))
                (test-assert (equal '(nil t) (expanded-native-compaction-flags provider)) "A later ACP prompt really answers.")
                (expanded-native-test--settled application 180))
           (mission-detach application))))))
  nil)


;;;; -- Unpublishable Portable Compaction --

(-> expanded-native-test--no-fit
    (application expanded-native-test-provider keyword
     &key (:entry keyword) (:session t) (:client t)) null)
(defun expanded-native-test--no-fit (application provider phase &key entry session client)
  "Exercise real pre-dispatch or post-paid refusal through an owning product entry."
  (setf (config :compaction-threshold-percent (application-configuration application)) 35)
  (let* ((conversation                     (application-conversation application))
         (controller                       (or (application-input-controller application)
                                               (make-instance 'application-input-controller :application application :main-thread (current-thread))))
         (*idle-compaction-clock-function* (lambda () 1180))
         (note-original                    (symbol-function 'agent--note-compaction-request))
         (arrival                          nil)
         (escaped nil) (result nil) (work nil) (ticket nil) (service nil) (completion-id nil))
    (ecase phase
      (:before
       (conversation-append-user-message conversation (make-string 600000 :initial-element #\a))
       (conversation-append-provider-item conversation (agent-test-message "Old protected user exchange completed."))
       (conversation-append-user-message
        conversation (make-string (if (eq entry :acp-completion) 600000 1000) :initial-element #\b)))
      (:after
       (conversation-append-user-message conversation "Older large response.")
       (conversation-append-provider-item conversation (agent-test-message (make-string 190000 :initial-element #\h)))
       (conversation-append-user-message conversation "Summarize before continuing.")
       (setf arrival
             (lambda ()
               (conversation-append-user-message conversation (make-string 600000 :initial-element #\n))))))
    (when (member entry '(:terminal :acp))
      (setf (application-input-controller application) controller
            (config :prompt-cache-retention-seconds (application-configuration application)) 300)
      (unless session
        (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal))
      (idle-compaction-tests--prime application)
      (unless session
        (setf work (application-input-controller--next-work controller))
        (test-assert (eq :idle-compaction (first work)) "No-fit begins with a genuine due idle episode.")))
    (when (eq entry :wakeup)
      (setf service (mission-schedule-service-create application :clock (lambda () 100)
                     :enqueue                                           (lambda (owner command) (declare (ignore owner command)) t))
            (gethash application *mission-schedule-services*) service)
      (application-mission-schedule-add application :id "no-fit" :content "Perform scheduled work." :at 100 :interval 10)
      (setf ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
    (when (eq entry :acp-queue)
      (acp-input-test--submit session "_autolith/queue" "Actual queued ACP input.")
      (acp-input-test--submit session "_autolith/queue" "Remaining queued ACP input."))
    (when (eq entry :acp-completion)
      (let* ((agent (application-agent application))
             (job   (job-completion-tests--start agent (task-completion--runtime agent))))
        (setf completion-id (session-job-execution-identifier job))
        (test-assert (task-completion-pending agent) "A real completed job awaits automatic delivery.")
        (acp-completion-test--wait
         (lambda () (let ((notices (acp-completion--admitted-notices session)))
                      (and notices (every (lambda (notice) (<= (getf notice :ready-at) (get-universal-time))) notices)))))
        (test-assert (not (task-completion--delivered-p conversation (session-job-execution-identifier job)))
                     "The completion has not been consumed before owner admission.")))
    (test-call-with-function-replacements
     (list (list 'agent--note-compaction-request
                 (lambda (agent observer &rest arguments)
                   (multiple-value-prog1 (apply note-original agent observer arguments)
                     ;; Append once after the original durable paid-summary receipt.
                     (when (and arrival (eq :summary (getf arguments :kind)))
                       (funcall (shiftf arrival nil)))))))
     (lambda ()
      (expanded-native-test--transport
       application :status 404 :signaled-p (eq phase :after) :function
       (lambda ()
         (handler-case
             (setf result
                   (ecase entry
                     (:manual
                      (application-compact application))
                     (:turn
                      (nth-value 1 (application--run-turn application "Perform the requested work." :tools-p nil :continuation-p t)))
                     (:message
                      (application--run-message-input application "Actual mission user input." :tools-p nil))
                     (:continuation
                      (application--run-goal-continuations application))
                     (:wakeup
                      (application-operation-call application "mission-wakeup" ticket))
                     (:terminal
                      (application-input-controller--run-work controller work))
                     (:acp
                      (acp-completion--cycle session))
                     (:acp-prompt
                      (agentcomms:client-prompt client (acp-session-identifier session)
                                                (list (agentcomms:acp-text-content "Actual explicit ACP input."))))
                     ((:acp-queue :acp-completion)
                      (acp-completion--cycle session))))
           (error (condition) (setf escaped condition))))
       :expected-posts (if (eq phase :before) 0 1))))
    (when (eq entry :terminal) (application-input-controller--finish-work controller))
    (let* ((loaded  (conversation-load (conversation-pathname conversation)))
           (records (conversation-records-newest loaded 100))
           (notices (remove-if-not (lambda (record) (eq :side-status (first record))) records)))
      (format t "~&NO-FIT phase=~S entry=~S escaped=~S result=~S summary-requests=~D notices=~D~%"
              phase entry (type-of escaped) (and (keywordp result) result)
              (count t (expanded-native-compaction-flags provider)) (length notices))
      (test-assert (null escaped) "Context admission refusal is a normal explained product outcome.")
      (test-assert (not (find :summary records :key #'first)) "No invalid checkpoint is durably published.")
      (test-assert (not (find :turn-aborted records :key #'first)) "A local no-fit refusal is not recorded as an application crash.")
      (test-assert (find-if (lambda (record)
                             (and (test-object-contains-string-p record "context")
                                  (every (lambda (span) (not (eq :failure (terminal-span-style span))))
                                         (getf (rest record) :spans)))) notices)
                   "A durable informational explanation identifies the context refusal.")
      (test-assert (equal (conversation-input-history loaded) (conversation-input-history conversation))
                   "Canonical reload preserves every user arrival and the original text history.")
      (when completion-id
        (test-assert (task-completion--delivered-p loaded completion-id)
                     "Durable reload retains the claimed completion's delivery exactly once."))
      (test-assert (= (if (eq phase :after) 1 0) (length (expanded-native-input-snapshots provider)))
                   "Only an admitted, completed summary can incur paid usage.")
      (expanded-native-test--settled application (if (eq phase :after) 150 0))
      (when (eq entry :acp-prompt)
        (test-assert (eq :refusal result) "The ACP protocol returns refusal, never a completed answer."))
      (when (eq entry :turn)
        (test-assert (eq :failed result) "A refused turn is never returned as a completed answer."))
      (when service
        (let* ((snapshot   (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)))
               (occurrence (first (getf snapshot :wakeups))))
          (test-assert (eq :failed (getf occurrence :state)) "The refused scheduled occurrence is settled failed, never executed or unknown.")
          (test-assert (zerop (hash-table-count (mission-schedule-executing service)))
                       "A refused wakeup releases its live execution reservation.")
          (let* ((restored (mission-schedule-service-create application :clock (lambda () 100)))
                 (record   (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler restored)) :wakeups))))
            (test-assert (and (eq :failed (getf record :state)) (eq :runner-failed (getf record :result)))
                         "Reopening preserves a truthful failed refusal rather than an executed or replayable claim.")
            (setf (gethash application *mission-schedule-services*) restored)
            (let ((count   (length (expanded-native-input-snapshots provider)))
                  (notices (length (mission-schedule-test--notices application))))
              (handler-case (application-operation-call application "mission-wakeup" ticket)
                (mission-error () nil))
              (mission-schedule--announce restored)
              (test-assert (= count (length (expanded-native-input-snapshots provider))) "A reopened refusal cannot charge the occurrence twice.")
              (test-assert (= notices (length (mission-schedule-test--notices application)))
                           "Reopening and duplicate consumption do not republish a terminal notice."))))))
    (when (eq entry :continuation)
      (test-assert (= 1 (getf (application-goal application) :continuations 0))
                   "A refusal during continuation stops after one attempt, without a busy retry."))
    (when (member entry '(:terminal :acp))
      (test-assert (null (application-idle-compaction-episode application)) "Refusal consumes the idle episode once.")
      (test-assert (not (application-input-controller-active-p controller)) "Refusal releases controller activity.")
      (when session
        (test-assert (null (acp-session-prompt-thread session)) "Refusal releases ACP prompt ownership.")
        (test-assert (not (acp-session-prompt-interruptible-p session)) "Refusal finalizes ACP interruption state.")
        (test-assert (null (acp-completion--cycle session)) "The consumed ACP opportunity cannot repeat.")))
    (unless session (setf (application-input-controller application) nil)))
  nil)

(-> test-expanded-native-no-fit-product-boundaries () null)
(defun test-expanded-native-no-fit-product-boundaries ()
  "Check pre-dispatch exhaustion and true unavailable-HTTP post-paid refusal."
  (dolist (phase '(:before :after))
    (dolist (entry '(:manual :turn :message :continuation :wakeup :terminal))
      (expanded-native-test--fixture
       (lambda (application provider) (expanded-native-test--no-fit application provider phase :entry entry))
       :owned-p t)))
  nil)

(-> test-expanded-native-no-fit-acp () null)
(defun test-expanded-native-no-fit-acp ()
  "Return a normal context refusal while finalizing real inert ACP idle ownership."
  (dolist (phase '(:before :after))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000") ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
      (acp-input-test--session
       (lambda (session client)
         (declare (ignore client))
         (let* ((application (acp-session-application session)) (provider (application-provider application)))
           (change-class provider 'expanded-native-test-provider
                         :credential-manager (credential-manager-create (application-configuration application)))
           (setf (slot-value provider 'cl-llm-provider-api::configuration) (application-configuration application)
                 (expanded-native-results provider)
                 (list (agent-test-result "summary" (list (agent-test-compaction-message))
                                          :usage (json-object "input_tokens" 100 "output_tokens" 50))))
           (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32))
           (unwind-protect
                (expanded-native-test--no-fit application provider phase :entry ':acp :session session)
             (mission-detach application)))))))
  nil)


;;;; -- ACP Prompt and Continuation Refusals --

(-> expanded-native-test--acp-turn-refusal (keyword) null)
(defun expanded-native-test--acp-turn-refusal (entry)
  "Exercise the real ACP owner and dispatcher, including subsequent reuse."
  (dolist (phase '(:before :after))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000")
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
      (acp-input-test--session
       (lambda (session client)
         (let* ((application  (acp-session-application session))
                (conversation (application-conversation application))
                (agent        (application-agent application))
                (provider     (application-provider application))
                (controller   (acp-input-controller session)))
           (change-class provider 'expanded-native-test-provider
                         :credential-manager (credential-manager-create (application-configuration application)))
           (setf (slot-value provider 'cl-llm-provider-api::configuration) (application-configuration application)
                 (expanded-native-results provider)
                 (list (agent-test-result "summary" (list (agent-test-compaction-message))
                                          :usage (json-object "input_tokens" 100 "output_tokens" 50))))
           (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32))
           (mission-context-bind (mission-context-find conversation) agent)
           (unwind-protect
                (progn
                  (expanded-native-test--no-fit application provider phase :entry entry :session session :client client)
                  (acp-completion-test--wait (lambda () (= 2 (length (acp-session-test-turn-events client)))))
                  (acp-session-test-check-turns client
                                               (list (ecase entry
                                                       (:acp-prompt
                                                        "prompt")
                                                       (:acp-queue
                                                        "queue")
                                                       (:acp-completion
                                                        "completion")))
                                               '("refusal"))
                  (test-assert (find-if (lambda (update)
                                         (and (equal "agent_message_chunk" (json-get update "sessionUpdate"))
                                              (test-object-contains-string-p update "Context admission refused")))
                                       (acp-session-test-updates client))
                               "The client receives the plain explanation inside the refused turn.")
                  (test-assert (null (acp-session-completion-report session)) "A local refusal is not a controller crash report.")
                  (test-assert (null (acp-session-prompt-thread session)) "Refusal releases the ACP owner.")
                  (test-assert (not (acp-session-prompt-interruptible-p session)) "Refusal completes interruption cleanup.")
                  (test-assert (not (application-input-controller-active-p controller)) "Refusal releases native controller activity.")
                  (test-assert (application-input-controller-queued-work-paused-p controller)
                               "Refusal pauses automatic queued work without pretending to answer.")
                  (test-assert (not (acp-input--pending-p session)) "The refused current input cannot replay automatically.")
                  (test-assert (null (task-completion-pending agent)) "Delivered completion work is consumed once.")
                  (let ((reopened (make-instance 'application-input-controller
                                                 :application application :main-thread nil)))
                    (application-input-controller--load-pending reopened)
                    (test-assert (equal (if (eq entry :acp-queue) '("Remaining queued ACP input.") nil)
                                        (mapcar (lambda (work) (user-message-input-text (second work)))
                                                (deque->list (application-input-controller-work-items reopened))))
                                 "Durable reload retains only unclaimed queue tail, not the refused current prompt.")
                    (test-assert (deque-empty-p (application-input-controller-initial-work-items reopened))
                                 "No refused input is restored into the initial queue."))
                  (format t "~&ACP-REFUSAL entry=~S phase=~S outcome=~S report=~S goal=~S~%"
                          entry phase (json-get (first (last (acp-session-test-turn-events client))) "outcome")
                          (acp-session-completion-report session) (getf (application-goal application) :status))
                  (let ((count   (length (expanded-native-input-snapshots provider)))
                        (records (conversation-next-sequence conversation)))
                    (test-assert (null (acp-completion--cycle session)) "A refusal does not start a retry cycle.")
                    (test-assert (= count (length (expanded-native-input-snapshots provider))) "No paid work repeats.")
                    (test-assert (= records (conversation-next-sequence conversation)) "No durable delivery repeats."))
                  (expanded-native-test--settled application (if (eq phase :after) 150 0)))
             (mission-detach application)))))))
  nil)

(-> test-expanded-native-no-fit-acp-prompt () null)
(defun test-expanded-native-no-fit-acp-prompt ()
  "Return a plain protocol refusal for real explicit ACP prompts."
  (expanded-native-test--acp-turn-refusal :acp-prompt))

(-> test-expanded-native-no-fit-acp-queue () null)
(defun test-expanded-native-no-fit-acp-queue ()
  "Consume queued ACP input once when portable recovery cannot fit."
  (expanded-native-test--acp-turn-refusal :acp-queue))

(-> test-expanded-native-no-fit-acp-completion () null)
(defun test-expanded-native-no-fit-acp-completion ()
  "Deliver a real job once without crashing its automatic ACP continuation."
  (expanded-native-test--acp-turn-refusal :acp-completion))

(-> test-expanded-native-stream-fixture () null)
(defun test-expanded-native-stream-fixture ()
  "Independently qualify native success and the production stream event decoder."
  (expanded-native-test--fixture
   (lambda (application provider)
     (let ((configuration (application-configuration application))
           (conversation  (application-conversation application))
           (posts         0))
       (test-call-with-function-replacements
        (list (list 'credential-manager-credentials
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (provider-tests--credentials configuration)))
              (list 'dexador:post
                    (lambda (endpoint &rest arguments)
                      (declare (ignore arguments))
                      (incf posts)
                      (test-assert (search "/responses/compact" endpoint) "Native success uses the real native POST.")
                      (values (json-encode
                               (json-object "id" "native-fixture" "output"
                                            (json-array (json-object "type" "compaction" "encrypted_content" "synthetic"))
                                            "usage" (json-object "input_tokens" 1 "output_tokens" 1)))
                              200 nil))))
        (lambda ()
          (let ((window (provider-native-compact-conversation provider conversation
                                                             :tool-namespaces #() :event-callback #'identity)))
            (test-assert window "A genuine native success decodes a complete native window."))
          (let* ((*telemetry-request-role* :compaction)
                 (result                   (provider-stream-turn provider conversation :tool-namespaces #()
                                               :event-callback #'identity :compaction-p t)))
            (test-assert (equal "summary" (provider-result-response-id result)) "Production decoding returns the completed response identity.")
            (test-assert (equal (provider-result-assistant-text result)
                                (provider-result-assistant-text (agent-test-result "expected" (list (agent-test-compaction-message)))))
                         "Production decoding reconstructs the complete structured summary.")
            (test-assert (= 100 (json-get (provider-result-usage result) "input_tokens")) "Production decoding retains metered input usage.")
            (test-assert (= 50 (json-get (provider-result-usage result) "output_tokens")) "Production decoding retains metered output usage."))))
       (test-assert (= 1 posts) "Fixture qualification admits one real native POST.")
       (test-assert (equal '(t) (expanded-native-compaction-flags provider)) "Fixture qualification admits one production summary stream."))))
  nil)


;;;; -- Paid Summary Arrival and Terminal Ownership --

(-> expanded-native-test--owner-snapshot
    (&key (:entry keyword) (:owned-p boolean) (:status integer)
          (:signaled-p boolean) (:arrival-size (option (integer 1))) (:reuse-p boolean) (:controller-p boolean) (:one-shot-p boolean)) list)
(defun expanded-native-test--owner-snapshot
    (&key entry owned-p status signaled-p arrival-size reuse-p controller-p one-shot-p)
  "Observe real owning work and append a canonical arrival after original decoded usage persistence."
  (let ((snapshot nil))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((conversation  (application-conversation application))
              (configuration (application-configuration application))
              (note-original (symbol-function 'agent--note-compaction-request))
              (fit-original  (symbol-function 'agent--validate-compaction-projection))
              (controller    (if controller-p
                              (application-input-controller-create application :load-pending-p nil :start-reader-p nil)
                              (make-instance 'application-input-controller
                                             :application application :main-thread (current-thread))))
              (events nil) (posts 0) (summaries 0) (failure nil) (result nil)
              (ticket nil) (service nil) (reuse-failure nil) (reuse-result nil))
         (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
         (when (or controller-p (eq entry :terminal-message))
           (setf (application-input-controller application) controller))
         (when (eq entry :wakeup)
           (setf service (mission-schedule-service-create
                          application :clock (lambda () 100) :notify (when controller-p (lambda () nil))
                          :enqueue (if controller-p #'mission-schedule--queue
                                       (lambda (owner command) (declare (ignore owner command)) t)))
                 (gethash application *mission-schedule-services*) service)
           (apply #'application-mission-schedule-add application :id "paid-arrival"
                  :content "Perform scheduled work." :at 100 (unless one-shot-p (list :interval 10)))
           (setf ticket (mission-schedule-tests--ticket
                         (first (application-mission-schedules-wake application)))))
         (test-call-with-function-replacements
          (list
           (list 'credential-manager-credentials
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (provider-tests--credentials configuration)))
           (list 'dexador:post
                 (lambda (endpoint &rest arguments)
                   (declare (ignore arguments))
                   (test-assert (search "/responses/compact" endpoint) "The actual native POST is used.")
                   (incf posts)
                   (push (list :native-post status signaled-p) events)
                   (cond
                     (signaled-p
                      (error 'http-request-failed :status status :body "Native endpoint unavailable."
                                                 :headers nil :uri endpoint :method ':post))
                     ((= status 200)
                      (values (json-encode
                               (json-object "id" "native-control" "output"
                                            (json-array (json-object "type" "compaction"
                                                                     "encrypted_content" "synthetic"))
                                            "usage" (json-object "input_tokens" 1 "output_tokens" 1)))
                              200 nil))
                     (t
                      (values "Native endpoint unavailable." status nil)))))
           (list 'agent--note-compaction-request
                 (lambda (agent observer &rest arguments)
                   (multiple-value-prog1 (apply note-original agent observer arguments)
                     (let ((usage (provider-usage-normalize (getf arguments :usage))))
                       (push (list :usage-persisted (getf arguments :kind)
                                   (getf arguments :request-number)
                                   (json-get usage "input_tokens") (json-get usage "output_tokens")) events))
                     (when (eq :summary (getf arguments :kind))
                       (incf summaries)
                       (when (and arrival-size (= summaries 1))
                         (conversation-append-user-message
                          conversation (make-string arrival-size :initial-element #\n))
                         (push (list :canonical-arrival arrival-size) events))))))
           (list 'agent--validate-compaction-projection
                 (lambda (&rest arguments)
                   (push (list :validation-start) events)
                   (handler-bind
                       ((provider-context-budget-error
                          (lambda (condition)
                            (expanded-native-test--refusal-budget condition)
                            (push (list :validation-refusal (type-of condition)) events))))
                     (multiple-value-prog1 (apply fit-original arguments)
                       (push (list :validation-returned) events))))))
          (lambda ()
            (handler-case
                (setf result
                      (ecase entry
                        (:manual
                         (application-compact application))
                        (:terminal-message
                         (multiple-value-bind (accepted delivery)
                             (application-input-controller-submit-primary-prompt
                              controller "Actual terminal user work." :prefer-steering-p nil)
                           (push (list :terminal-submit accepted delivery) events))
                         (when owned-p
                           (application-input-controller-submit-primary-prompt
                            controller "Later queued user work." :prefer-steering-p nil))
                         (let ((work (application-input-controller--next-work controller)))
                           (push (list :terminal-work (first work)) events)
                           (unwind-protect
                                (application-input-controller--run-work controller work)
                             (application-input-controller--finish-work controller))))
                        (:wakeup
                         (if controller-p
                             (let ((work (application-input-controller--next-work controller)))
                               (push (list :terminal-work (first work)) events)
                               (unwind-protect (application-input-controller--run-work controller work)
                                 (application-input-controller--finish-work controller)))
                             (application-operation-call application "mission-wakeup" ticket)))))
              (error (condition)
                (setf failure condition)))
            (let* ((loaded  (conversation-load (conversation-pathname conversation)))
                   (records (conversation-records-newest loaded 100))
                   (goal    (application-goal application))
                   (wake    (when service
                              (first (getf (cl-jobpond:scheduler-snapshot
                                            (mission-schedule-scheduler service)) :wakeups)))))
              (setf snapshot
                    (list :entry               entry
                          :owned-p             owned-p
                          :status              status
                          :signaled-p          signaled-p
                          :arrival-size        arrival-size
                          :posts               posts
                          :summaries           summaries
                          :events              (reverse events)
                          :failure             (and failure (type-of failure))
                          :result              (and (keywordp result) result)
                          :flags               (copy-list (expanded-native-compaction-flags provider))
                          :checkpoint-p        (not (null (find :summary records :key #'first)))
                          :aborted-p           (not (null (find :turn-aborted records :key #'first)))
                          :notice-p
                          (not (null (find-if
                                      (lambda (record)
                                        (and (eq :side-status (first record))
                                             (test-object-contains-string-p record "context"))) records)))
                          :arrival-present-p
                          (or (null arrival-size)
                              (not (null (find (make-string arrival-size :initial-element #\n) (conversation-input-history loaded) :test #'equal))))
                          :arrival-preserved-p
                          (or (null arrival-size)
                              (= arrival-size (length (first (last (conversation-input-history loaded))))))
                          :paid                (getf goal :tokens-used 0)
                          :outstanding         (getf goal :requests-outstanding 0)
                          :reserved            (getf goal :tokens-reserved 0)
                          :unknown             (getf goal :unknown-usage 0)
                          :mission-status      (getf goal :status)
                          :wakeup-state        (getf wake :state)
                          :wakeup-result       (getf wake :result)
                          :executing           (if service (hash-table-count (mission-schedule-executing service)) 0)
                          :controller-active-p (application-input-controller-active-p controller)
                          :prompt-status       (application-input-controller-prompt-marker-status controller)
                          :input-count         (count "Actual terminal user work." (conversation-input-history loaded) :test #'equal))))
            (when reuse-p
              (setf (expanded-native-results provider)
                    (list (agent-test-result "reuse-summary" (list (agent-test-compaction-message))
                                             :usage (json-object "input_tokens" 100 "output_tokens" 50))
                          (agent-test-result "reuse-answer" (list (agent-test-message "Later actual answer."))
                                             :usage (json-object "input_tokens" 20 "output_tokens" 10))))
              (handler-case
                  (setf reuse-result (application--run-message-input
                                      application "Later explicit user work." :tools-p nil :goal-continuations-p nil))
                (error (condition)
                  (setf reuse-failure condition)))
              (setf (getf snapshot :reuse-failure) (and reuse-failure (type-of reuse-failure))
                    (getf snapshot :reuse-result) reuse-result
                    (getf snapshot :reuse-flags) (copy-list (expanded-native-compaction-flags provider))
                    (getf snapshot :reuse-input-preserved-p)
                    (not (null (member "Later explicit user work." (conversation-input-history conversation) :test #'equal)))))))
         (setf (getf snapshot :restored-p)
               (and (eq note-original (symbol-function 'agent--note-compaction-request))
                    (eq fit-original (symbol-function 'agent--validate-compaction-projection))))
         (setf (application-input-controller application) nil)))
     :owned-p owned-p :large-p t)
    snapshot))

(-> expanded-native-test--paid-snapshot-checks (list) null)
(defun expanded-native-test--paid-snapshot-checks (snapshot)
  "Check genuine paid completion, canonical arrival and original final guard in order."
  (let* ((events (getf snapshot :events))
         (kinds  (mapcar #'first events))
         (usage  (find :summary events :key #'second)))
    (test-assert (= 1 (getf snapshot :posts)) "One genuine unavailable native POST precedes portable recovery.")
    (test-assert (= 1 (getf snapshot :summaries)) "The production decoder completes exactly one summary.")
    (test-assert (and usage (= 100 (fourth usage)) (= 50 (fifth usage)))
                 "The original post-decoder event persists metered input100/output50.")
    (test-assert (and (position :canonical-arrival kinds) (position :validation-start kinds)
                      (< (position :canonical-arrival kinds) (position :validation-start kinds)))
                 "The legitimate arrival precedes the original publication fit check.")
    (test-assert (getf snapshot :arrival-preserved-p) "Canonical reload retains the entire new user input.")
    (test-assert (member :validation-refusal kinds) "The actual publication guard rejects the next projection.")
    (test-assert (not (getf snapshot :checkpoint-p)) "An invalid summary is never durably published.")
    (test-assert (getf snapshot :restored-p) "Both original observational bindings are restored.")
    (when (getf snapshot :owned-p)
      (test-assert (= 150 (getf snapshot :paid)) "Completed paid summary usage is retained exactly once.")
      (test-assert (eq :active (getf snapshot :mission-status)) "The definitive fit refusal preserves the mission.")
      (dolist (key '(:outstanding :reserved :unknown))
        (test-assert (zerop (getf snapshot key)) (format nil "The genuine refusal releases ~A." key)))))
  nil)

(-> test-expanded-native-paid-arrival-fixture () null)
(defun test-expanded-native-paid-arrival-fixture ()
  "Qualify original-once usage interleaving with native success and portable arrival controls."
  (dolist (cell '((200 nil) (404 nil) (404 64)))
    (let ((snapshot (expanded-native-test--owner-snapshot
                     :entry ':manual :owned-p t :status (first cell) :arrival-size (second cell))))
      (test-assert (null (getf snapshot :failure)) "A fitting native or portable control returns normally.")
      (test-assert (getf snapshot :checkpoint-p) "Fitting controls publish a real validated checkpoint.")
      (test-assert (= 1 (getf snapshot :summaries)) "The original stream and decoder complete one summary.")
      (test-assert (getf snapshot :arrival-preserved-p) "The small concurrent arrival is canonical.")
      (test-assert (member :validation-returned (mapcar #'first (getf snapshot :events)))
                   "The actual original validator returns normally for fitting controls.")
      (test-assert (getf snapshot :restored-p) "Qualification restores both original functions.")))
  nil)

(-> test-expanded-native-paid-owner-refusals () null)
(defun test-expanded-native-paid-owner-refusals ()
  "Reject actual post-paid unpublishable projections without aborting manual or scheduled owners."
  (dolist (entry '(:manual :terminal-message :wakeup))
    (dolist (status '(404 405 501))
      (let ((snapshot (expanded-native-test--owner-snapshot
                       :entry entry :owned-p t :status status :arrival-size 600000)))
        (expanded-native-test--paid-snapshot-checks snapshot)
        (test-assert (null (getf snapshot :failure)) "The real owner presents an explained refusal normally.")
        (test-assert (not (getf snapshot :aborted-p)) "A definitive fit refusal is not a crashed turn.")
        (test-assert (getf snapshot :notice-p) "The owner durably explains the context refusal.")
        (when (eq entry :wakeup)
          (test-assert (eq :failed (getf snapshot :wakeup-state)) "A refused occurrence is failed, not executed or unknown.")
          (test-assert (eq :runner-failed (getf snapshot :wakeup-result)) "The default runner reports truthful failure.")
          (test-assert (zerop (getf snapshot :executing)) "The real executing reservation is released.")))))
  nil)

(-> test-expanded-native-terminal-message-owners () null)
(defun test-expanded-native-terminal-message-owners ()
  "Reach real submitted terminal message work for returned and typed unavailable statuses."
  (dolist (status '(200 404 405 501))
    (dolist (signaled-p (if (= status 200) '(nil) '(nil t)))
      (dolist (owned-p '(nil t))
        (let ((snapshot (expanded-native-test--owner-snapshot
                         :entry ':terminal-message :owned-p owned-p :status status :signaled-p signaled-p)))
          (test-assert (= 1 (getf snapshot :posts)) "The message owner reaches the actual native POST.")
          (test-assert (null (getf snapshot :failure)) "Unavailable native compaction does not escape the terminal owner.")
          (test-assert (= 1 (getf snapshot :input-count)) "Real submitted user input is persisted exactly once.")
          (test-assert (equal '(nil t) (getf snapshot :flags)) "One real summary and the original ordinary answer complete.")
          (test-assert (getf snapshot :checkpoint-p) "The actual owner publishes the portable or native checkpoint.")
          (test-assert (not (getf snapshot :aborted-p)) "The real message lifecycle never records a native-unavailable abort.")
          (test-assert (not (getf snapshot :controller-active-p)) "The terminal work releases controller activity.")
          (when owned-p
            (test-assert (= (if (= status 200) 182 180) (getf snapshot :paid))
                         "Only actual decoded native, summary and answer usage is charged."))))))
  nil)


;;;; -- Actual ACP and Idle Owner Matrix --

(-> expanded-native-test--matrix-fixture (function &key (:acp-p boolean) (:owned-p boolean)) null)
(defun expanded-native-test--matrix-fixture (function &key acp-p owned-p)
  "Supply an actual ACP session or terminal application with identical production transport."
  (if acp-p
      (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000") ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
        (acp-input-test--session
         (lambda (session client)
           (let* ((application   (acp-session-application session))
                  (configuration (application-configuration application))
                  (provider      (application-provider application))
                  (conversation  (application-conversation application)))
             (change-class provider 'expanded-native-test-provider
                           :credential-manager (credential-manager-create configuration))
             (setf (slot-value provider 'cl-llm-provider-api::configuration) configuration
                   (config :session-title-generation-p configuration) nil)
             (conversation-append-user-message conversation "An earlier text-only exchange.")
             (conversation-append-provider-item conversation (agent-test-message (make-string 190000 :initial-element #\h)))
             (conversation-append-user-message conversation "Continue the actual work.")
             (expanded-native-test--soft-pressure application)
             (when owned-p
               (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32)))
             (unwind-protect (funcall function application provider session client)
               (when owned-p (mission-detach application)))))))
      (expanded-native-test--fixture
       (lambda (application provider) (funcall function application provider nil nil))
       :owned-p owned-p :large-p t))
  nil)

(-> expanded-native-test--matrix-snapshot
    (&key (:entry keyword) (:owned-p boolean) (:status integer) (:signaled-p boolean)
          (:arrival-p boolean) (:image-p boolean) (:reuse-p boolean) (:cancel-before-refusal-p boolean)) list)
(defun expanded-native-test--matrix-snapshot
    (&key entry owned-p status signaled-p arrival-p image-p reuse-p cancel-before-refusal-p)
  "Record each genuine entry separately, retaining owner errors without intercepting product conditions."
  (let ((snapshot nil))
    (expanded-native-test--matrix-fixture
     (lambda (application provider session client)
       (let* ((conversation                     (application-conversation application))
              (configuration                    (application-configuration application))
              (controller                       (if session (acp-input-controller session)
                                                    (make-instance 'application-input-controller :application application :main-thread (current-thread))))
              (note-original                    (symbol-function 'agent--note-compaction-request))
              (fit-original                     (symbol-function 'agent--validate-compaction-projection))
              (turn-original                    (symbol-function 'acp-session-run-turn))
              (guard-original                   (symbol-function 'provider-check-request-context))
              (application-turn-original        (symbol-function 'application--run-turn))
              (later-turn-result                nil)
              (*idle-compaction-clock-function* (lambda () 1180))
              (events nil) (posts 0) (summaries 0) (failure nil) (result nil)
              (completion-id nil) (runtime nil) (work nil) (later-failure nil) (later-result nil) (reuse-started-p nil))
         (unless session
           (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
           (setf (application-input-controller application) controller))
         (setf (expanded-native-results provider)
               (list (agent-test-result "matrix-summary" (list (agent-test-compaction-message))
                                        :usage (json-object "input_tokens" 100 "output_tokens" 50))
                     (agent-test-result "matrix-answer" (list (agent-test-message "Matrix ordinary answer."))
                                        :usage (json-object "input_tokens" 20 "output_tokens" 10))))
         (when image-p
           (conversation-append-provider-item
            conversation (json-object "type" "message" "role" "user" "content"
                                      (json-array (json-object "type" "input_text" "text" "Synthetic image review.")
                                                  (json-object "type" "input_image" "image_url" "https://example.invalid/synthetic.png" "detail" "high")))))
         (when (member entry '(:terminal-idle :acp-idle))
           (setf (config :prompt-cache-retention-seconds configuration) 300)
           (idle-compaction-tests--prime application)
           (unless session
             (setf work (application-input-controller--next-work controller))
             (push (list :selected-work (first work)) events)))
         (when (eq entry :acp-queue)
           (acp-input-test--submit session "_autolith/queue" "Matrix claimed queued input.")
           (acp-input-test--submit session "_autolith/queue" "Matrix unclaimed queued tail."))
         (when (eq entry :acp-completion)
           (setf runtime (task-completion--runtime (application-agent application)))
           (let ((job (job-completion-tests--start (application-agent application) runtime)))
             (setf completion-id (session-job-execution-identifier job))
             (test-assert (not (null (task-completion-pending (application-agent application)))) "The notification comes from an actually completed job.")
             (acp-completion-test--wait
              (lambda ()
                (let ((notices (acp-completion--admitted-notices session)))
                  (and notices (every (lambda (notice) (<= (getf notice :ready-at) (get-universal-time))) notices)))))
             (push (list :completed-job-ready (not (null (acp-completion--admitted-notices session)))) events)))
         (unwind-protect
              (test-call-with-function-replacements
               (list
                 (list 'provider-check-request-context
                       (lambda (&rest arguments)
                         (handler-bind ((provider-context-budget-error
                                          (lambda (condition)
                                            (when reuse-started-p
                                              (push (list :later-context-condition (type-of condition)) events)))))
                           (apply guard-original arguments))))
                (list 'credential-manager-credentials
                      (lambda (&rest arguments) (declare (ignore arguments)) (provider-tests--credentials configuration)))
                (list 'dexador:post
                      (lambda (endpoint &rest arguments)
                        (declare (ignore arguments))
                        (incf posts)
                        (test-assert (search "/responses/compact" endpoint) "A real admitted native POST reaches only inert I/O.")
                        (push (list :native-post status signaled-p) events)
                        (cond
                          (signaled-p
                               (error 'http-request-failed :status status :body "Native endpoint unavailable."
                                                          :headers nil :uri endpoint :method ':post))
                          ((= status 200)
                           (values (json-encode (json-object "id" "native-control" "output"
                                                            (json-array (json-object "type" "compaction" "encrypted_content" "synthetic"))
                                                            "usage" (json-object "input_tokens" 1 "output_tokens" 1))) 200 nil))
                          (t
                           (values "Native endpoint unavailable." status nil)))))
                (list 'application--run-turn
                      (lambda (&rest arguments)
                        (let ((results (multiple-value-list (apply application-turn-original arguments))))
                          (when reuse-started-p (setf later-turn-result (second results)))
                          (values-list results))))
                (list 'agent--note-compaction-request
                      (lambda (agent observer &rest arguments)
                        (multiple-value-prog1 (apply note-original agent observer arguments)
                          (let ((usage (provider-usage-normalize (getf arguments :usage))))
                            (push (list :usage-persisted (getf arguments :kind) (getf arguments :request-number)
                                        (json-get usage "input_tokens") (json-get usage "output_tokens")) events))
                          (when (eq :summary (getf arguments :kind))
                            (incf summaries)
                            (when (and arrival-p (= summaries 1))
                              (conversation-append-user-message conversation (make-string 600000 :initial-element #\n))
                              (push (list :canonical-arrival 600000) events))))))
                (list 'agent--validate-compaction-projection
                      (lambda (&rest arguments)
                        (push (list :validation-start) events)
                        (handler-bind ((provider-context-budget-error
                                         (lambda (condition)
                                             (expanded-native-test--refusal-budget condition)
                                             (push (list :validation-refusal (type-of condition)) events)
                                             (when cancel-before-refusal-p
                                               (acp-session-cancel session)
                                               (push (list :cancel-recorded (acp-session-cancelled-p session)) events)))))
                          (multiple-value-prog1 (apply fit-original arguments)
                            (push (list :validation-returned) events)))))
                (list 'acp-session-run-turn
                      (lambda (&rest arguments)
                        (push (list :shared-turn-entry (getf (rest (rest arguments)) :automatic-p)
                                    (getf (rest (rest arguments)) :queued-p)) events)
                        (handler-bind ((provider-context-budget-error
                                         (lambda (condition)
                                           (when reuse-started-p
                                             (push (list :later-context-condition (type-of condition)) events)))))
                          (multiple-value-prog1 (setf result (apply turn-original arguments))
                            (push (list :shared-turn-return result) events))))))
               (lambda ()
                 (handler-case
                     (handler-bind ((provider-context-budget-error
                                      (lambda (condition) (push (list :owner-context-condition (type-of condition)) events)))
                                    (mission-error
                                      (lambda (condition) (declare (ignore condition)) (push (list :owner-mission-error) events))))
                       (ecase entry
                         (:terminal-idle
                          (unwind-protect (application-input-controller--run-work controller work)
                            (application-input-controller--finish-work controller)))
                         (:acp-prompt
                          (setf result (agentcomms:client-prompt
                                        client (acp-session-identifier session)
                                        (list (agentcomms:acp-text-content "Matrix actual client input.")))))
                         ((:acp-idle :acp-queue :acp-completion)
                          (acp-completion--cycle session))))
                   (application-turn-cancelled (condition) (setf failure condition))
                   (error (condition) (setf failure condition)))
                 (let* ((loaded  (conversation-load (conversation-pathname conversation)))
                        (records (conversation-records-newest loaded 200))
                        (goal    (application-goal application)))
                   (setf snapshot
                         (list :entry entry :owned-p owned-p :status status :signaled-p signaled-p
                               :arrival-size (when arrival-p 600000) :image-p image-p :posts posts :summaries summaries
                               :events (reverse events) :failure (and failure (type-of failure)) :result result
                               :flags                   (copy-list (expanded-native-compaction-flags provider))
                               :checkpoint-p            (not (null (find :summary records :key #'first)))
                               :aborted-p               (not (null (find :turn-aborted records :key #'first)))
                               :notice-p                (not (null (find-if (lambda (record) (and (eq :side-status (first record))
                                                                                                (test-object-contains-string-p record "context"))) records)))
                               :answer-p                (test-object-contains-string-p (conversation-input-items loaded) "Matrix ordinary answer.")
                               :arrival-preserved-p     (or (not arrival-p) (= 600000 (length (first (last (conversation-input-history loaded))))))
                               :paid (getf goal :tokens-used 0) :outstanding (getf goal :requests-outstanding 0)
                               :reserved (getf goal :tokens-reserved 0) :unknown (getf goal :unknown-usage 0)
                               :mission-status          (getf goal :status)
                               :ui-compacting-p         (and (application-ui application) (terminal-ui-compacting-p (application-ui application)))
                               :controller-active-p     (application-input-controller-active-p controller)
                               :paused-p                (application-input-controller-queued-work-paused-p controller)
                               :prompt-thread-p         (and session (not (null (acp-session-prompt-thread session))))
                               :interruptible-p         (and session (acp-session-prompt-interruptible-p session))
                               :report-p                (and session (not (null (acp-session-completion-report session))))
                               :episode-p               (not (null (application-idle-compaction-episode application)))
                               :queued-input-count      (count "Matrix claimed queued input." (conversation-input-history loaded) :test #'equal)
                               :queue-tail-in-history-p (not (null (member "Matrix unclaimed queued tail." (conversation-input-history loaded) :test #'equal)))
                               :queue-tail-pending-p    (and (eq entry :acp-queue) (not (deque-empty-p (application-input-controller-work-items controller))))
                               :queue-reloaded-tail-p   (and (eq entry :acp-queue)
                                                             (multiple-value-bind (form complete-p)
                                                                 (snapshot-read (configuration-pending-inputs-path configuration (conversation-pathname loaded)))
                                                               (let* ((state      (and complete-p (input-state-from-form form (conversation-identifier loaded))))
                                                                      (work-items (getf state :work-items)))
                                                                 (and (= 1 (length work-items))
                                                                      (equal "Matrix unclaimed queued tail."
                                                                              (user-message-input-text (second (first work-items))))))))
                               :completion-delivered-p  (and completion-id (task-completion--delivered-p loaded completion-id)))))
                 (when (or (eq entry :acp-completion) (and (eq entry :acp-queue) arrival-p) (eq entry :acp-idle))
                   (let ((before (length (expanded-native-input-snapshots provider))))
                     (handler-case (acp-completion--cycle session)
                        (error (condition) (setf (getf snapshot :repeat-cycle-failure) (type-of condition))))
                     (setf (getf snapshot :repeat-cycle-opens) (- (length (expanded-native-input-snapshots provider)) before))))
                 (when reuse-p
                   (setf reuse-started-p t)
                   (setf (expanded-native-results provider)
                         (list (agent-test-result "matrix-later-answer" (list (agent-test-message "Matrix later ordinary answer."))
                                                  :usage (json-object "input_tokens" 20 "output_tokens" 10))))
                   (let ((before       (length (expanded-native-input-snapshots provider)))
                         (history      (conversation-input-history conversation))
                         (later-events nil))
                     (handler-case
                         (handler-bind ((provider-context-budget-error
                                          (lambda (condition) (push (type-of condition) later-events))))
                           (setf later-result
                                 (if session
                                     (agentcomms:client-prompt client (acp-session-identifier session)
                                                              (list (agentcomms:acp-text-content "Matrix later explicit input.")))
                                     (application--run-message-input application "Matrix later explicit input."
                                                                     :tools-p nil :goal-continuations-p nil))))
                       (error (condition) (setf later-failure condition)))
                     (setf (getf snapshot :later-failure) (and later-failure (type-of later-failure))
                           (getf snapshot :later-public-result) later-result
                           (getf snapshot :later-result) (if session later-result later-turn-result)
                           (getf snapshot :later-context-conditions) (append (reverse later-events)
                                                                             (loop for event in events
                                                                                   when (eq :later-context-condition (first event))
                                                                                     collect (second event)))
                           (getf snapshot :later-opens) (- (length (expanded-native-input-snapshots provider)) before)
                           (getf snapshot :later-role) (first (expanded-native-compaction-flags provider))
                           (getf snapshot :later-answer-p) (test-object-contains-string-p (conversation-input-items conversation) "Matrix later ordinary answer.")
                           (getf snapshot :prior-history-preserved-p) (equal history (subseq (conversation-input-history conversation) 0 (length history)))
                           (getf snapshot :window) (config :context-window configuration)
                           (getf snapshot :later-owner-clean-p)
                           (and (not (application-input-controller-active-p controller))
                                (or (null session) (null (acp-session-prompt-thread session)))
                                (zerop (getf (application-goal application) :requests-outstanding 0))
                                (zerop (getf (application-goal application) :tokens-reserved 0))))))))
           (when runtime (task-orchestrator-close runtime))
           (unless session (setf (application-input-controller application) nil)))
         (setf (getf snapshot :restored-p)
               (and (eq note-original (symbol-function 'agent--note-compaction-request))
                    (eq application-turn-original (symbol-function 'application--run-turn))
                    (eq fit-original (symbol-function 'agent--validate-compaction-projection))
                    (eq guard-original (symbol-function 'provider-check-request-context))
                    (eq turn-original (symbol-function 'acp-session-run-turn))))))
     :acp-p (not (eq entry :terminal-idle)) :owned-p owned-p)
    snapshot))

(-> expanded-native-test--matrix-checks (list) null)
(defun expanded-native-test--matrix-checks (snapshot)
  "Assert portable success or explained final refusal at the actual owning boundary."
  (test-assert (= 1 (getf snapshot :posts)) "Every cell actually reaches native POST.")
  (test-assert (null (getf snapshot :failure)) "The owner does not propagate a crash or remote error.")
  (test-assert (not (getf snapshot :aborted-p)) "A context refusal is not a crashed turn.")
  (test-assert (not (getf snapshot :report-p)) "An explained refusal is not an ACP completion crash report.")
  (test-assert (getf snapshot :restored-p) "All original observer and owner bindings are restored.")
  (dolist (key '(:controller-active-p :prompt-thread-p :interruptible-p))
    (test-assert (not (getf snapshot key)) (format nil "The owner releases ~A." key)))
  (if (getf snapshot :arrival-size)
      (progn
        (expanded-native-test--paid-snapshot-checks snapshot)
        (test-assert (getf snapshot :notice-p) "A real postpaid refusal has a durable context explanation.")
        (when (member (getf snapshot :entry) '(:acp-prompt :acp-queue :acp-completion))
          (test-assert (eq :refusal (getf snapshot :result)) "The actual shared ACP turn returns refusal.")))
      (progn
        (test-assert (getf snapshot :checkpoint-p) "A fitting portable summary is durably published.")
        (test-assert (= 1 (getf snapshot :summaries)) "One real summary is decoded and persisted.")
        (unless (member (getf snapshot :entry) '(:terminal-idle :acp-idle))
          (test-assert (getf snapshot :answer-p) "The actual owner records a genuine ordinary answer."))))
  (when (getf snapshot :owned-p)
    (dolist (key '(:outstanding :reserved :unknown))
      (test-assert (zerop (getf snapshot key)) (format nil "The settled request releases ~A." key))))
  (when (member (getf snapshot :entry) '(:terminal-idle :acp-idle))
    (test-assert (not (getf snapshot :episode-p)) "The due cache episode is consumed once."))
  (when (eq :acp-queue (getf snapshot :entry))
    (test-assert (= 1 (getf snapshot :queued-input-count)) "The genuinely claimed current input is durable once.")
    (test-assert (not (getf snapshot :queue-tail-in-history-p)) "The unclaimed tail is not silently appended.")
    (test-assert (getf snapshot :queue-tail-pending-p) "The unclaimed tail is retained in pending work.")
    (test-assert (getf snapshot :queue-reloaded-tail-p) "The original pending-state reader reloads only the unclaimed durable tail.")
    (when (getf snapshot :arrival-size)
      (test-assert (getf snapshot :paused-p) "Refusal pauses unclaimed queued work.")))
  (when (eq :acp-completion (getf snapshot :entry))
    (test-assert (getf snapshot :completion-delivered-p) "The real completed job has a durable delivery receipt."))
  (when (getf snapshot :repeat-cycle-opens)
    (test-assert (null (getf snapshot :repeat-cycle-failure)) "The repeated default completion cycle does not fail.")
    (test-assert (zerop (getf snapshot :repeat-cycle-opens)) "A consumed notice or paused episode cannot automatically replay provider work."))
  (when (getf snapshot :window)
    (test-assert (getf snapshot :later-owner-clean-p) "The later explicit input releases its real ownership and reservations.")
    (test-assert (= 144000 (getf snapshot :window)) "Later explicit work uses the same configured window.")
    (test-assert (getf snapshot :prior-history-preserved-p) "Later explicit work retains every prior durable input.")
    (test-assert (null (getf snapshot :later-failure)) "Later explicit admission is not a leaked owner failure.")
    (if (getf snapshot :arrival-size)
        (progn
          (test-assert (eq (if (eq :terminal-idle (getf snapshot :entry)) :failed :refusal) (getf snapshot :later-result))
                       "Protected giant input receives another explained refusal.")
          (test-assert (getf snapshot :later-context-conditions) "The original later admission guard observes legitimately oversized protected history."))
        (progn
          (test-assert (= 1 (getf snapshot :later-opens)) "A fitting later input reaches exactly one ordinary transport.")
          (test-assert (null (getf snapshot :later-role)) "The later transport is an answer, not a summary.")
          (test-assert (getf snapshot :later-answer-p) "The genuine later ordinary answer is durable."))))
  nil)

(-> test-expanded-native-acp-owner-matrix () null)
(defun test-expanded-native-acp-owner-matrix ()
  "Exercise actual client, claimed queue and completed-job owners over all unavailable statuses."
  (dolist (entry '(:acp-prompt :acp-queue :acp-completion))
    (dolist (status '(404 405 501))
      (dolist (owned-p '(nil t))
        (dolist (cell '((nil nil) (nil t) (t nil) (t t)))
          (expanded-native-test--matrix-checks
           (expanded-native-test--matrix-snapshot :entry entry :status status :owned-p owned-p
                                                 :signaled-p (first cell) :arrival-p (second cell)))))))
  nil)

(-> test-expanded-native-idle-owner-matrix () null)
(defun test-expanded-native-idle-owner-matrix ()
  "Run terminal and ACP due episodes through real idle owners on all status and ownership cells."
  (dolist (entry '(:terminal-idle :acp-idle))
    (dolist (status '(404 405 501))
      (dolist (owned-p '(nil t))
        (dolist (cell '((nil nil) (nil t) (t nil) (t t)))
          (expanded-native-test--matrix-checks
           (expanded-native-test--matrix-snapshot :entry entry :status status :owned-p owned-p
                                                 :signaled-p (first cell) :arrival-p (second cell) :reuse-p t))))))
  nil)

(-> test-expanded-native-image-owner-and-reuse () null)
(defun test-expanded-native-image-owner-and-reuse ()
  "Contrast synthetic image and text owners and role-qualify fitting later ordinary answers."
  (dolist (image-p '(nil t))
    (dolist (status '(200 404 405 501))
      (dolist (arrival-p (if (= status 200) '(nil) '(nil t)))
        (expanded-native-test--matrix-checks
         (expanded-native-test--matrix-snapshot :entry ':acp-prompt :owned-p t :status status
                                               :arrival-p arrival-p :image-p image-p :reuse-p t)))))
  nil)

(defclass expanded-native-values-agent (agent)
  ((return-values :initarg :return-values :reader expanded-native-test-return-values
                  :documentation "The complete values returned by a test generic extension."))
  (:documentation "Agent extension exercising the owner's primary-value contract."))

(defmethod agent-run-user-turn ((agent expanded-native-values-agent) content &key &allow-other-keys)
  "Return the configured value count without replacing the generic function."
  (declare (ignore content))
  (values-list (expanded-native-test-return-values agent)))

(-> test-expanded-native-owner-return-values () null)
(defun test-expanded-native-owner-return-values ()
  "Accept generic extensions returning zero, one, or multiple values at the real terminal owner."
  (expanded-native-test--fixture
   (lambda (application provider)
     (declare (ignore provider))
     (let* ((agent          (application-agent application))
            (primary        (agent-test-result "extension" (list (agent-test-message "Extension result."))))
            (note-original  (symbol-function 'application--note-goal-turn))
            (title-original (symbol-function 'application--maybe-refresh-conversation-title)))
       (change-class agent 'expanded-native-values-agent :return-values nil)
       (dolist (returned (list nil (list primary) (list primary :secondary)))
         (setf (slot-value agent 'return-values) returned)
         (let ((noted :unset) (titles 0) (failure nil) (result nil))
           (test-call-with-function-replacements
            (list (list 'application--note-goal-turn
                        (lambda (owner value)
                          (setf noted value)
                          (when value (funcall note-original owner value))))
                  (list 'application--maybe-refresh-conversation-title
                        (lambda (owner) (incf titles) (funcall title-original owner))))
            (lambda ()
              (handler-case (setf result (multiple-value-list (application--run-turn application "Extension input.")))
                (error (condition) (setf failure condition)))))
           (test-assert (null failure) "The actual turn owner accepts the extension's value count.")
           (test-assert (equal (list nil (first returned)) result) "Only the primary extension value is selected.")
           (test-assert (eq noted (first returned)) "Goal accounting receives the primary value, including NIL.")
           (test-assert (= 1 titles) "Successful input reaches title finalization once."))))))
  nil)

(-> test-expanded-native-idle-cancellation-refusal () null)
(defun test-expanded-native-idle-cancellation-refusal ()
  "Record cancellation at the real final validator before the ACP idle refusal catch."
  (dolist (cancel-p '(nil t))
    (let ((snapshot (expanded-native-test--matrix-snapshot
                     :entry :acp-idle :owned-p t :status 404 :signaled-p t
                     :arrival-p t :cancel-before-refusal-p cancel-p)))
      (test-assert (= 1 (getf snapshot :posts)) "Cancellation is recorded after the admitted native attempt.")
      (test-assert (= 1 (getf snapshot :summaries)) "The original paid summary completed before final validation.")
      (test-assert (= 150 (getf snapshot :paid)) "Paid usage settles exactly once.")
      (test-assert (eq :active (getf snapshot :mission-status)) "The mission remains active.")
      (test-assert (getf snapshot :arrival-preserved-p) "Canonical arriving history is retained.")
      (test-assert (not (getf snapshot :checkpoint-p)) "An unfit projection is not published.")
      (test-assert (if cancel-p (eq 'application-turn-cancelled (getf snapshot :failure))
                      (null (getf snapshot :failure)))
                   "Recorded cancellation wins before refusal presentation; the ordinary control refuses normally.")
      (test-assert (eq (not cancel-p) (getf snapshot :notice-p)) "Cancellation produces no spurious context-refusal notice.")
      (test-assert (not (getf snapshot :report-p)) "The idle controller has no completion crash report.")
      (test-assert (not (getf snapshot :controller-active-p)) "External work ownership is finalized.")
      (test-assert (not (getf snapshot :prompt-thread-p)) "The idle prompt lease is released.")
      (test-assert (not (getf snapshot :interruptible-p)) "Finalization disables interruption.")
      (test-assert (not (getf snapshot :episode-p)) "The idle episode is consumed once.")
      (test-assert (not (getf snapshot :aborted-p)) "No failed model turn is fabricated.")
      (test-assert (every #'zerop (mapcar (lambda (key) (getf snapshot key)) '(:outstanding :reserved :unknown)))
                   "Accounting has no outstanding, reserved, or unknown usage.")
      (test-assert (zerop (getf snapshot :repeat-cycle-opens)) "A repeated default cycle starts no paid request.")))
  nil)

(-> test-expanded-native-owner-finalization-errors () null)
(defun test-expanded-native-owner-finalization-errors ()
  "Keep goal, title and terminal presentation failures outside the inference refusal catch."
  (dolist (target '(application--note-goal-turn application--maybe-refresh-conversation-title application-render-records))
    (expanded-native-test--fixture
     (lambda (application provider)
       (declare (ignore provider))
       (let* ((agent     (application-agent application))
              (condition (make-condition 'provider-context-budget-error
                                         :estimated-input-tokens 300000 :output-reserve 8192
                                         :context-window 272000 :non-text-estimate-p nil))
              (caught nil) (presented 0))
         (change-class agent 'expanded-native-values-agent
                       :return-values (list (agent-test-result "extension" (list (agent-test-message "Extension result.")))))
         (test-call-with-function-replacements
          (list (list target (lambda (&rest arguments) (declare (ignore arguments)) (error condition)))
                (list 'application-present-context-refusal
                      (lambda (&rest arguments) (declare (ignore arguments)) (incf presented))))
          (lambda ()
            (handler-case (application--run-turn application "Finalization input.")
              (provider-context-budget-error (failure) (setf caught failure)))))
         (test-assert (eq condition caught) "The original finalization condition escapes to its existing owner.")
         (test-assert (zerop presented) "A finalization failure is not presented as inference context refusal.")))))
  nil)

(-> test-expanded-native-scheduled-controller () null)
(defun test-expanded-native-scheduled-controller ()
  "Consume valid claimed wakeups through the default enqueue, terminal dispatch and default runner."
  (dolist (status '(404 405 501))
    (dolist (signaled-p '(nil t))
      (dolist (mode '(:fitting-recurring :refused-recurring :refused-one-shot))
        (let* ((arrival-p (not (eq mode :fitting-recurring)))
               (snapshot  (expanded-native-test--owner-snapshot
                           :entry :wakeup :owned-p t :status status :signaled-p signaled-p
                           :controller-p t :one-shot-p (eq mode :refused-one-shot)
                           :arrival-size (when arrival-p 600000))))

          (test-assert (null (getf snapshot :failure)) "Actual scheduled controller dispatch returns without an owner exception.")
          (test-assert (member '(:terminal-work :native-lisp) (getf snapshot :events) :test #'equal)
                       "The valid ticket enters the real noninteractive native Lisp work path.")
          (test-assert (= 1 (getf snapshot :posts)) "One original native attempt reaches inert transport.")
          (test-assert (= 1 (getf snapshot :summaries)) "The portable summary is paid once.")
          (test-assert (equal (if arrival-p '(t) '(nil t)) (getf snapshot :flags))
                       "No unrequested paid request follows a final-fit refusal.")
          (test-assert (= (if arrival-p 150 180) (getf snapshot :paid)) "Only the actual summary and fitting answer are charged.")
          (test-assert (eq (if arrival-p :failed :completed) (getf snapshot :wakeup-state))
                       "The default runner durably settles refused or completed work, never UNKNOWN.")
          (test-assert (not (getf snapshot :aborted-p)) "The mission turn is not aborted.")
          (test-assert (eq :active (getf snapshot :mission-status)) "The scheduled mission remains active.")
          (test-assert (getf snapshot :arrival-present-p) "Refusal retains the complete canonical arrival even when operation records follow it.")
          (test-assert (not (getf snapshot :controller-active-p)) "The controller releases active work.")
          (test-assert (zerop (getf snapshot :executing)) "Scheduler execution reservations are released.")
          (test-assert (every #'zerop (mapcar (lambda (key) (getf snapshot key)) '(:outstanding :reserved :unknown)))
                       "No usage remains outstanding, reserved, or unknown.")))))
  nil)
