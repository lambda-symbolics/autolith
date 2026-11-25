(in-package #:autolith)

;;;; -- Emergency Compaction Recovery --

(defclass compaction-recovery-test-provider (compaction-gap-test-provider)
  ((window :initarg :window :initform nil :reader compaction-recovery-test-window
           :documentation "Deterministic selected-route metadata."))
  (:documentation "Exercise final admission without credentials."))

(defmethod provider-compaction-recovery-window ((provider compaction-recovery-test-provider))
  "Inject existing selected-route metadata deterministically."
  (compaction-recovery-test-window provider))

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
  "Recover a refused compaction request by dropping detached old encrypted reasoning."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation                              (conversation-create configuration))
             (provider                                  (make-instance 'compaction-recovery-test-provider
                                                         :configuration configuration
                                                         :results       (list (agent-test-result
                                                                               "summary" (list (agent-test-checkpoint-call))))))
             (agent                                     (agent-create :configuration configuration :conversation conversation
                                                         :provider provider :tool-registry (make-instance 'tool-registry)))
             (*conversation-compaction-tail-characters* 256)
             (failure                                   nil))
        (conversation-append-user-message conversation "older exchange")
        (conversation-append-provider-item
         conversation (json-object "type" "reasoning" "id" "old-secret"
                                   "encrypted_content" (make-string 40000 :initial-element #\e)))
        (conversation-append-provider-item conversation (agent-test-message "old answer"))
        ;; A later exchange keeps the old reasoning out of the retained tail,
        ;; so the checkpoint request is the one that has to read it.
        (conversation-append-user-message conversation "a later question")
        (conversation-append-provider-item
         conversation (agent-test-message (make-string 300 :initial-element #\l)))
        (conversation-append-user-message conversation "continue")
        (compaction-recovery-test-small-context
         (lambda ()
           (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
             (provider-context-budget-error (condition) (setf failure condition)))))
        (test-assert (null failure)
                     (format nil "Emergency compaction must recover the refused request: ~A" failure))
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "Exactly one admitted checkpoint request reaches transport."))))
  nil)

(-> test-compaction-recovery-publication-preserves-tail-families () null)
(defun test-compaction-recovery-publication-preserves-tail-families ()
  "Publish recovery with concurrent arrivals and replay its provider-specific tail."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation                              (conversation-create configuration))
             (provider                                  (make-instance 'compaction-recovery-test-provider
                                                         :configuration configuration
                                                         :results       (list (agent-test-result
                                                                               "summary" (list (agent-test-checkpoint-call))))))
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
        (let ((tail          (mapcar #'json-encode (last (conversation-input-items conversation) 4)))
              (note-original (symbol-function 'agent--note-compaction-request)))
          (compaction-recovery-test-small-context
           (lambda ()
             (test-call-with-function-replacements
              (list (list 'agent--note-compaction-request
                          (lambda (owner observer &rest options)
                            (multiple-value-prog1 (apply note-original owner observer options)
                              (conversation-append-user-message conversation "Concurrent arrival.")
                              (setf tail (append tail (list (json-encode (first (last (conversation-input-items conversation)))))))))))
              (lambda ()
                (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
                  (provider-context-budget-error (condition) (setf failure condition)))))))
          (test-assert (null failure) "Path: portable recovery successfully publishes.")
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
          (test-assert (equal tail (mapcar #'json-encode (last (conversation-input-items conversation) (length tail))))
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
          (test-assert (test-object-contains-string-p projection "Concurrent arrival.")
                       "Concurrent arrival survives recovery publication and reload.")
          (test-assert (find "current-call" projection :key (lambda (item) (json-get item "call_id")) :test #'equal)
                       "Correlated tool work survives recovery replay.")
          (test-assert (not (find "current encrypted" (conversation-input-items-for-family loaded :other-provider :include-ephemeral-p nil)
                                  :key (lambda (item) (json-get item "encrypted_content")) :test #'equal))
                       "Protected reasoning is private to its original provider family.")
          (dolist (item (conversation-input-items loaded))
            (when (equal "reasoning" (json-get item "type"))
              (test-assert (eq family (conversation-input-item-family loaded item))
                           "Contract: replayed reasoning retains family metadata.")))))))
  nil)

(-> test-compaction-recovery-model-window () null)
(defun test-compaction-recovery-model-window ()
  "Check every request of a compaction against a larger model window, and no other request."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let ((request  (json-object "input" (make-string 24000 :initial-element #\x)
                                   "max_output_tokens" 700))
            (oversize (json-object "input" (make-string 80000 :initial-element #\x)
                                   "max_output_tokens" 700)))
        (dolist (window '(nil 4000 5000 20000))
          (let* ((provider (make-instance 'compaction-recovery-test-provider
                                          :configuration configuration :window window :results nil))
                 (view     (conversation-create configuration))
                 (state    (make-compaction-recovery)))
            (conversation-append-user-message view "continue")
            (flet ((refused-p (candidate)
                     "Return true when a compaction that sends CANDIDATE is refused locally."
                     (flet ((check (view)
                              (declare (ignore view))
                              (provider-check-request-context provider candidate))

                            (send (view)
                              (declare (ignore view))
                              (provider-open-response-stream provider candidate)))
                       (handler-case
                           (progn (compaction-recovery-call state provider view
                                                            :dispatch #'send :preflight #'check)
                                  nil)
                         (provider-context-budget-error () t)))))
              (if (eql window 20000)
                  (progn
                    (test-assert (not (or (refused-p request) (refused-p request)))
                                 "Every request of the compaction is admitted, not only the first.")
                    (test-assert (null (compaction-recovery-audit state ':completed))
                                 "A request the model window admits is no recovery.")
                    (test-assert (refused-p oversize)
                                 "The output reserve and the model window remain hard bounds."))
                  (test-assert (refused-p request)
                               "Only a strictly larger model window admits the request.")))
            (test-assert (handler-case (progn (provider-check-request-context provider request) nil)
                           (provider-context-budget-error () t))
                         "An ordinary request sees the configured window."))))))
  nil)

(-> test-compaction-recovery-unrelated-failure () null)
(defun test-compaction-recovery-unrelated-failure ()
  "A failure after admission escapes unchanged and starts no lossy recovery."
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
               (caught   nil))
          (handler-case
              (compaction-recovery-call
               state provider view
               :preflight (lambda (candidate) (declare (ignore candidate))
                            (provider-check-request-context provider request))
               :dispatch
               (lambda (candidate)
                 (declare (ignore candidate))
                 (provider-open-response-stream provider request)
                 (error failure)))
            (error (condition) (setf caught condition)))
          (test-assert (eq failure caught) "The unrelated original condition escapes unchanged.")
          (test-assert (zerop (compaction-recovery-tier state)) "No lossy tier follows auth or transport failure.")))))
  nil)

(-> test-compaction-recovery-encrypted-eligibility () null)
(defun test-compaction-recovery-encrypted-eligibility ()
  "Keep correlated neighbors while removing old reasoning."
  (with-test-configuration (configuration)
    (let* ((state        (make-compaction-recovery))
           (conversation (conversation-create configuration))
           (old          (json-object "type" "reasoning" "encrypted_content" "old encrypted"))
           (plain        (json-object "type" "reasoning" "summary" #("plaintext reasoning")))
           (paired       (json-object "type" "reasoning" "encrypted_content" "paired encrypted"))
           (call         (compaction-tail-tests--call "paired"))
           (output       (function-call-output-item "paired" "answer"))
           (after-output (json-object "type" "reasoning" "encrypted_content" "after output"))
           (anchor       (json-object "type" "compaction" "encrypted_content" "native anchor"))
           (after-anchor (json-object "type" "reasoning" "encrypted_content" "after anchor"))
           (separator    (json-object "type" "message" "role" "assistant" "content" "separator"))
           (old-second   (json-object "type" "reasoning" "encrypted_content" "second old encrypted"))
           (current      (json-object "type" "reasoning" "encrypted_content" "current encrypted"))
           (view         (conversation-compaction-summary-view
                          conversation (list old plain paired call output after-output anchor after-anchor
                                             separator old-second
                                             (json-object "type" "message" "role" "user" "content" "latest") current))))
      (let* ((detached  (compaction-recovery-view state view))
             (candidate (compaction-recovery-strip-encrypted state detached)))
        (dolist (text '("old encrypted" "second old encrypted"))
          (test-assert (not (test-object-contains-string-p (conversation-input-items candidate) text))
                       "Eligible standalone old encrypted content is removed."))
        (dolist (text '("plaintext reasoning" "paired encrypted" "paired" "after output"
                        "native anchor" "after anchor" "current encrypted" "latest"))
          (test-assert (test-object-contains-string-p (conversation-input-items candidate) text)
                       "Original linked neighbors and protected reasoning survive."))
        (test-assert (= 2 (getf (first (compaction-recovery-notes state)) :removed-items))
                     "The chronological pass reports both eligible removals.")
        (test-assert (equal "old encrypted" (json-get old "encrypted_content"))
                     "Detached removal leaves source objects unchanged."))))
  nil)

(-> test-compaction-recovery-tools-and-transcript () null)
(defun test-compaction-recovery-tools-and-transcript ()
  "Stub newest eligible old outputs first and preserve whole newest exchanges in plain recovery."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (state        (make-compaction-recovery))
           (a            (function-call-output-item "a" (make-string 12000 :initial-element #\a)))
           (b            (function-call-output-item "b" (make-string 12000 :initial-element #\b)))
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
                               (function-call-output-item "boundary" "old output")
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
  (:documentation "Combine final admission with ordinary mission ownership."))

(-> test-compaction-recovery-mission-accounting () null)
(defun test-compaction-recovery-mission-accounting ()
  "Charge actual recovery usage and block the mission on unknown usage."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (mode '(:known :unknown))
        (let* ((provider                                  (make-instance 'compaction-recovery-accounting-test-provider
                                                           :configuration configuration :summary-mode mode
                                                           :results (list (agent-test-result "summary" (list (agent-test-checkpoint-call))
                                                                                            :usage (if (eq mode :unknown)
                                                                                                       (json-object)
                                                                                                       (json-object "input_tokens" 1 "output_tokens" 1))))))
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
          (conversation-append-user-message conversation "a later question")
          (conversation-append-provider-item
           conversation (agent-test-message (make-string 300 :initial-element #\l)))
          (conversation-append-user-message conversation "continue")
          (application-mission-start application (mission-test--specification :tokens 100000 :turns 8))
          (let ((context (mission-context-find conversation)))
            (compaction-recovery-test-small-context
             (lambda ()
               (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
                 (error (condition) (setf failure condition)))))
            (let ((ledger (mission-context-goal context)))
              (test-assert (null failure)
                           (format nil "Recovery with ~(~A~) usage ended with ~A." mode failure))
              (test-assert (= (if (eq mode :known) 2 0) (getf ledger :tokens-used 0))
                           "Only actual reported usage is charged.")
              (test-assert (= (if (eq mode :unknown) 1 0) (getf ledger :unknown-usage 0))
                           "Unknown usage follows existing mission policy.")
              (test-assert (eq (if (eq mode :unknown) ':blocked ':active) (getf ledger :status))
                           "Unknown usage blocks new mission requests.")
              (test-assert (zerop (getf ledger :requests-outstanding 0)) "All reservations settle.")
              (dolist (entry (compaction-ownership-test-views provider))
                (test-assert (eq context (second entry)) "Every dispatched copy has the exact captured owner."))))))))
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
                                                                "summary" (list (agent-test-checkpoint-call))))))
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
                       provider (provider-request-object provider candidate #() :compaction-p t))))
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
          (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                       "Only the admitted older-history summary reaches transport.")))))
  nil)


;;;; -- Mission Accounting of Codex Compaction Requests --

(defclass codex-compaction-test-provider (scripted-provider codex-subscription-provider)
  ((window :initarg :window :initform nil :reader codex-compaction-test-window
           :documentation "The window the registry lists for the model.")
   (summary-calls :initform 0 :accessor codex-compaction-test-summary-calls
                  :documentation "Admitted checkpoint requests."))
  (:documentation "The production Codex encoder with inert dispatch and real mission accounting."))

(defmethod provider-compaction-recovery-window ((provider codex-compaction-test-provider))
  "Supply existing metadata without discovery or network access."
  (codex-compaction-test-window provider))

(defmethod provider-request-object
    ((provider codex-compaction-test-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Use the real Codex encoder rather than scripted request encoding."
  (provider-request-object
   (make-instance 'codex-subscription-provider :configuration (provider-configuration provider))
   conversation tool-namespaces :goal-context goal-context :compaction-p compaction-p))

(defmethod provider-open-response-stream
    ((provider codex-compaction-test-provider) (request hash-table) &key credentials conversation)
  "Admit an inert stream through the normal context boundary."
  (declare (ignore request credentials conversation))
  (values nil 200 nil))

(defmethod provider-stream-turn
    ((provider codex-compaction-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Apply final admission, then return deterministic positive usage."
  (declare (ignore event-callback))
  (provider-open-response-stream
   provider (provider-request-object provider conversation tool-namespaces
                                     :goal-context goal-context :compaction-p compaction-p)
   :conversation conversation)
  (when compaction-p
    (incf (codex-compaction-test-summary-calls provider)))
  (call-next-method))

(-> test-compaction-recovery-mission-model-window () null)
(defun test-compaction-recovery-mission-model-window ()
  "Admit a mission's checkpoint request against the model window in manual and reactive compaction."
  (dolist (reactive-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let* ((provider                                  (make-instance 'codex-compaction-test-provider
                                                                         :configuration      configuration
                                                                         :credential-manager (provider-credential-manager (provider-create configuration))
                                                                         :window             50000
                                                                         :results            (list (agent-test-result
                                                                                                    "summary" (list (agent-test-checkpoint-call))
                                                                                                    :usage (json-object "input_tokens" 7 "output_tokens" 3))
                                                                                                   (agent-test-result
                                                                                                    "answer" (list (agent-test-message "OK"))
                                                                                                    :usage (json-object "input_tokens" 2 "output_tokens" 1)))))
               (application                               (mission-inference-test--application configuration provider))
               (conversation                              (application-conversation application))
               (agent                                     (agent-create :configuration configuration :conversation conversation
                                                                        :provider provider :tool-registry (make-instance 'tool-registry)))
               (*conversation-compaction-tail-characters* 256)
               (failure                                   nil))
          (setf (application-agent application) agent)
          (conversation-append-user-message conversation "old exchange")
          (conversation-append-provider-item conversation (agent-test-message (make-string 40000 :initial-element #\h)))
          (conversation-append-user-message conversation "continue")
          (application-mission-start application (mission-test--specification :tokens 100000 :turns 16))
          (unwind-protect
               (let ((context (mission-context-find conversation)))
                 (compaction-recovery-test-small-context
                  (lambda ()
                    (test-call-with-function-replacements
                     (list (list 'credential-manager-credentials
                                 (lambda (&rest arguments)
                                   (declare (ignore arguments))
                                   (provider-tests--credentials configuration)))
                           (list 'agent-should-compact-p
                                 (lambda (agent &key &allow-other-keys) (declare (ignore agent)) nil)))
                     (lambda ()
                       (handler-case
                           (if reactive-p
                               (agent--run-provider-loop agent (make-instance 'agent-observer) :tools-p nil)
                               (application-compact application))
                         (error (condition) (setf failure condition)))))))
                 (let ((ledger (mission-context-goal context)))
                   (test-assert (null failure)
                                (format nil "The checkpoint request is admitted without a mission error: ~A" failure))
                   (test-assert (eq :active (getf ledger :status)) "The mission stays active.")
                   (test-assert (= 1 (codex-compaction-test-summary-calls provider))
                                "The checkpoint request is admitted exactly once.")
                   (test-assert (= (if reactive-p 13 10) (getf ledger :tokens-used 0))
                                "Only the checkpoint and resumed usage are charged.")
                   (test-assert (zerop (getf ledger :requests-outstanding 0)) "All final requests settle.")
                   (test-assert (zerop (getf ledger :tokens-reserved 0)) "All final allocations settle.")))
            (mission-detach application))))))
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
  (with-test-configuration (configuration)
    (dolist (case (list
                   (list (clinker-transcript:input-image-item "data:image/png;base64,CANONICAL") t)
                   (list (json-object "type" "image_url" "image_url"
                                      (json-object "url" "https://example.invalid/legacy-image")) t)
                   (list (json-object "type" "image" "value" "ordinary result") nil)
                   (list (json-object "mimeType" "image/png" "data" "ordinary result") nil)
                   (list (json-object "mime_type" "image/png" "data" "ordinary result") nil)
                   (list (json-object "inlineData"
                                      (json-object "mimeType" "image/png" "data" "ordinary result")) nil)
                   (list (json-object "fileData"
                                      (json-object "mimeType" "image/png" "fileUri" "ordinary result")) nil)))
      (destructuring-bind (part image-p) case
        (let* ((state     (make-compaction-recovery))
               (source    (conversation-compaction-summary-view
                           (conversation-create configuration)
                           (list (json-object "type" "message" "role" "user"
                                              "content" (vector part)))))
               (view      (compaction-recovery-text-view state source))
               (projected (aref (json-get (first (compaction-image-tests--transcript view)) "content") 0))
               (audit     (find :plain-text (compaction-recovery-notes state)
                                :key        (lambda (note) (getf note :tier)))))
          (test-assert (= (if image-p 1 0) (getf audit :images-omitted))
                       "Only canonical and supported legacy media parts count as omitted images.")
          (test-assert (if image-p
                           (json-string= (json-get projected "type") "input_text")
                           (equal (json-encode part) (json-encode projected)))
                       "Canonical media becomes text; ordinary structured result data survives intact.")))))
  nil)

(-> test-compaction-recovery-recent-images () null)
(defun test-compaction-recovery-recent-images ()
  "Admit the real summary projection with forty-seven protected recent images at 600K."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "4096"))
    (with-test-configuration (configuration)
     (let ((*provider-image-token-estimate* 16384))
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
          (test-assert (equal before (mapcar #'json-encode items)) "Source images survive summary preflight unchanged."))))))
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
     (let ((*provider-image-token-estimate* 16384))
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'compaction-recovery-test-provider
                                          :configuration configuration
                                          :results       (list (agent-test-result
                                                                "summary" (list (agent-test-checkpoint-call))
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
            (test-assert (equal notes (compaction-recovery-notes state)) "Readmission does not duplicate omission audit entries.")))))))
  nil)


;;;; -- Compaction Through Production Streaming --

(defclass compaction-owner-test-provider (codex-subscription-provider)
  ((results :initarg :results :accessor compaction-owner-results
            :documentation "Response data encoded into real completed SSE streams.")
   (input-snapshots :initform nil :accessor compaction-owner-input-snapshots
                    :documentation "Input identities seen only at admitted opening I/O.")
   (compaction-flags :initform nil :accessor compaction-owner-compaction-flags
                     :documentation "Summary role observed at admitted opening I/O.")
   (after-summary :initform nil :accessor compaction-owner-test-after-summary
                  :documentation "Canonical arrival delivered when the summary stream closes."))
  (:documentation "Retain production Codex streaming and replace only inert response I/O."))

(defmethod provider-compaction-recovery-window ((provider compaction-owner-test-provider))
  "Keep the fixture's metadata ceiling equal to its configured window."
  (config :context-window (provider-configuration provider)))

(defclass compaction-owner-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((source :initarg :source :reader compaction-owner-input-source
           :documentation "A standard completed synthetic SSE response.")
   (on-close :initarg :on-close :initform nil :accessor compaction-owner-input-on-close
             :documentation "Optional synthetic concurrent arrival at transport completion."))
  (:documentation "Character transport with an independently observable completion boundary."))

(defmethod trivial-gray-streams:stream-read-char ((stream compaction-owner-input-stream))
  "Read the inert transport without bypassing the production event decoder."
  (read-char (compaction-owner-input-source stream) nil :eof))

(defmethod trivial-gray-streams:stream-unread-char ((stream compaction-owner-input-stream) character)
  "Return CHARACTER to the inert transport."
  (unread-char character (compaction-owner-input-source stream)))

(defmethod close :after ((stream compaction-owner-input-stream) &key abort)
  "Deliver one canonical concurrent arrival after a normally consumed summary stream."
  (unless abort
    (let ((callback (shiftf (compaction-owner-input-on-close stream) nil)))
      (when callback (funcall callback)))))

(-> compaction-owner-test--sse (provider-result) string)
(defun compaction-owner-test--sse (result)
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

(defmethod provider-open-response-stream
    ((provider compaction-owner-test-provider) (request hash-table) &key credentials conversation)
  "Return inert SSE I/O after production projection, attempt, encoder and final admission."
  (declare (ignore request credentials))
  (let* ((summary-p (eq *telemetry-request-role* :compaction))
         (result    (pop (compaction-owner-results provider)))
         (callback  (and summary-p (shiftf (compaction-owner-test-after-summary provider) nil))))
    (assert result)
    (push (conversation-input-items conversation) (compaction-owner-input-snapshots provider))
    (push summary-p (compaction-owner-compaction-flags provider))
    (format t "~&EXPANDED-STREAM role=~S sequence=~D~%" (if summary-p :summary :answer)
            (length (compaction-owner-input-snapshots provider)))
    (values (make-instance 'compaction-owner-input-stream
                           :source   (make-string-input-stream (compaction-owner-test--sse result))
                           :on-close callback)
            200 nil)))

;; Couple small histories to an earlier soft threshold and a bounded hard window.
;; Keep the actual system instructions and encoded request admission in every cell.
(-> compaction-owner-test--soft-pressure (application) null)
(defun compaction-owner-test--soft-pressure (application)
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

(-> compaction-owner-test--refusal-budget (provider-context-budget-error) null)
(defun compaction-owner-test--refusal-budget (condition)
  "Require a measured hard overflow from the original final-request guard."
  (let ((input   (provider-context-budget-error-estimated-input-tokens condition))
        (reserve (provider-context-budget-error-output-reserve condition))
        (window  (provider-context-budget-error-context-window condition)))
    (test-assert (> (+ input reserve) window)
                 "The reduced arrival still exceeds actual encoded request admission.")
    (format t "~&FINAL-REFUSAL input=~D reserve=~D window=~D~%" input reserve window))
  nil)

(-> compaction-owner-test--fixture (function &key (:owned-p boolean) (:large-p boolean)) null)
(defun compaction-owner-test--fixture (function &key owned-p large-p)
  "Call FUNCTION with a real application, optional mission, and inert Codex transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'compaction-owner-test-provider
                                         :configuration      configuration
                                         :credential-manager (provider-credential-manager (provider-create configuration))
                                         :results            (list (agent-test-result
                                                                    "summary" (list (agent-test-checkpoint-call))
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
        (when large-p (compaction-owner-test--soft-pressure application))
        (when owned-p
          (application-mission-start application (mission-test--specification :tokens 2000000 :turns 32)))
        (unwind-protect
             (funcall function application provider)
          (when owned-p (mission-detach application))
          (remhash application *mission-schedule-services*)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> compaction-owner-test--call-with-credentials (application function) null)
(defun compaction-owner-test--call-with-credentials (application function)
  "Call FUNCTION with the fixture's Codex credentials."
  (let ((configuration (application-configuration application)))
    (test-call-with-function-replacements
     (list (list 'credential-manager-credentials
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (provider-tests--credentials configuration))))
     function))
  nil)

(-> test-compaction-owner-proactive () null)
(defun test-compaction-owner-proactive ()
  "Compact a real large text-only conversation ahead of a proactive turn, then answer."
  (dolist (owned-p '(nil t))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (let* ((agent        (application-agent application))
              (conversation (application-conversation application))
              (failure nil) (result nil))
         (test-assert (agent-should-compact-p agent) "The real large-text estimate selects proactive compaction.")
         (compaction-owner-test--call-with-credentials
          application
          (lambda ()
            (handler-case
                (setf result (agent--run-provider-loop agent (make-instance 'agent-observer) :tools-p nil))
              (error (condition) (setf failure condition)))))
         (format t "~&OWNER-PROACTIVE owner=~S failure=~S flags=~S~%"
                 owned-p (and failure (type-of failure)) (compaction-owner-compaction-flags provider))
         (test-assert (null failure) "Compaction must not abort the ordinary turn.")
         (test-assert (and result (equal "Resumed answer." (provider-result-assistant-text result)))
                      "The ordinary answer really continues after the checkpoint.")
         (test-assert (equal '(nil t) (compaction-owner-compaction-flags provider))
                      "One admitted summary and one admitted ordinary answer complete.")
         (test-assert (find :summary (conversation-records-newest conversation 30) :key #'first)
                      "The checkpoint is durably published.")
         (let ((loaded (conversation-load-by-id (application-configuration application)
                                                (conversation-identifier conversation))))
           (test-assert (equal (mapcar #'json-encode (conversation-input-items conversation))
                               (mapcar #'json-encode (conversation-input-items loaded)))
                        "Reload preserves the actual published projection."))
         (when owned-p
           (let ((ledger (application-goal application)))
             (test-assert (= 180 (getf ledger :tokens-used 0)) "Positive summary and answer usage settle once.")
             (test-assert (zerop (getf ledger :unknown-usage 0)) "No usage is quarantined.")
             (test-assert (zerop (getf ledger :requests-outstanding 0)) "No request remains outstanding.")
             (test-assert (zerop (getf ledger :tokens-reserved 0)) "No allocation remains reserved.")))))
     :owned-p owned-p :large-p t))
  nil)


;;;; -- Product Owner Lifecycles --

(-> compaction-owner-test--settled (application integer) null)
(defun compaction-owner-test--settled (application tokens)
  "Check the actual owner ledger after all admitted work has settled."
  (let ((goal (application-goal application)))
    (test-assert (= tokens (getf goal :tokens-used 0)) "Only completed positive paid usage is charged.")
    (test-assert (eq :active (getf goal :status)) "The mission stays active.")
    (dolist (key '(:requests-outstanding :tokens-reserved :unknown-usage))
      (test-assert (zerop (getf goal key 0)) (format nil "No residual ~A." key))))
  nil)

(-> compaction-owner-test--checkpoint (application) null)
(defun compaction-owner-test--checkpoint (application)
  "Verify publication from authoritative disk, independently of the active projection."
  (let ((loaded (conversation-load
                 (conversation-pathname (application-conversation application)))))
    (test-assert (find :summary (conversation-records-newest loaded 30) :key #'first)
                 "The checkpoint survives canonical reload."))
  nil)

(-> test-compaction-owner-manual () null)
(defun test-compaction-owner-manual ()
  "Use manual application compaction with and without a mission owner."
  (dolist (owned-p '(nil t))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (let ((failure nil))
         (compaction-owner-test--call-with-credentials
          application
          (lambda ()
            (handler-case (application-compact application)
              (error (condition) (setf failure condition)))))
         (format t "~&OWNER-MANUAL owned=~S failure=~S~%" owned-p (type-of failure))
         (test-assert (null failure) "Manual compaction returns normally.")
         (test-assert (equal '(t) (compaction-owner-compaction-flags provider)) "Manual compaction admits one paid summary.")
         (compaction-owner-test--checkpoint application)
         (when owned-p (compaction-owner-test--settled application 150))))
     :owned-p owned-p :large-p t))
  nil)

(defclass compaction-owner-output-observer (agent-observer)
  ((output-limit :initform 200000 :accessor compaction-owner-output-limit
                 :documentation "One initial ordinary output allowance, consumed at request start."))
  (:documentation "A legitimate initial output allowance that unwinds before reactive compaction."))

(defmethod agent-observer-status
    ((observer compaction-owner-output-observer) (status symbol) details)
  "Request a large output allowance once, using the ordinary observer protocol."
  (declare (ignore details))
  (when (eq status :provider-request-started)
    (shiftf (compaction-owner-output-limit observer) nil)))

(-> test-compaction-owner-reactive () null)
(defun test-compaction-owner-reactive ()
  "Reject the initial wire envelope with a genuine output allowance, then recover."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (let* ((conversation (application-conversation application))
            (agent (application-agent application)) (failure nil)
            (observer     (make-instance 'compaction-owner-output-observer)))
       (conversation-append-provider-item conversation (agent-test-message (make-string 400000 :initial-element #\h)))
       (conversation-append-user-message conversation "Continue the real text work.")
       (test-assert (not (agent-should-compact-p agent)) "The text conversation is below the real soft threshold.")
       (compaction-owner-test--call-with-credentials
        application
        (lambda ()
          (handler-case (agent--run-provider-loop agent observer :tools-p nil)
            (error (condition) (setf failure condition)))))
       (format t "~&OWNER-REACTIVE failure=~S flags=~S~%" (type-of failure)
               (compaction-owner-compaction-flags provider))
       (test-assert (null failure) "Final admission recovery reaches the checkpoint and the resumed answer.")
       (test-assert (equal '(nil t) (compaction-owner-compaction-flags provider)) "The rejected ordinary wire request is never paid.")
       (compaction-owner-test--checkpoint application)
       (compaction-owner-test--settled application 180)))
   :owned-p t)
  nil)

(-> test-compaction-owner-schedule () null)
(defun test-compaction-owner-schedule ()
  "Consume a genuine claimed occurrence through the default mission runner."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (let* ((now     100)
            (service (mission-schedule-service-create application :clock (lambda () now)
                       :enqueue                                          (lambda (owner command) (declare (ignore owner command)) t))))
       (setf (gethash application *mission-schedule-services*) service)
       (application-mission-schedule-add application :id "owner" :content "Inspect scheduled work."
                                        :at 100 :interval 10)
       (let* ((occurrence (first (application-mission-schedules-wake application)))
              (ticket (mission-schedule-tests--ticket occurrence)) (failure nil))
         (test-assert (eq :claimed (getf occurrence :state)) "The real scheduler persists a claimed ticket before execution.")
         (compaction-owner-test--call-with-credentials application
          (lambda ()
            (handler-case (application-operation-call application "mission-wakeup" ticket)
              (error (condition) (setf failure condition)))))
         (format t "~&OWNER-SCHEDULE failure=~S~%" (type-of failure))
         (test-assert (null failure) "The default scheduled runner continues past compaction.")
         (let ((wakeups (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups)))
           (test-assert (eq :completed (getf (first wakeups) :state)) "Only the completed answer marks the occurrence completed."))
         (let ((count (length (compaction-owner-input-snapshots provider))))
           (handler-case (application-operation-call application "mission-wakeup" ticket)
             (mission-error () nil))
           (test-assert (= count (length (compaction-owner-input-snapshots provider))) "Reconsuming the claimed ticket issues no request."))
         (compaction-owner-test--checkpoint application)
         (compaction-owner-test--settled application 180)
         (setf now 110
               (compaction-owner-results provider)
               (list (agent-test-result "later" (list (agent-test-message "Later scheduled answer."))
                                        :usage (json-object "input_tokens" 20 "output_tokens" 10))))
         (let ((next (first (application-mission-schedules-wake application))))
           (test-assert next "The next recurring occurrence is independently eligible.")
           (when next
               (compaction-owner-test--call-with-credentials
                application (lambda ()
                             (application-operation-call application "mission-wakeup" (mission-schedule-tests--ticket next))))
             (test-assert (= 3 (length (compaction-owner-input-snapshots provider))) "A later recurrence actually answers."))))))
   :owned-p t :large-p t)
  nil)

(-> test-compaction-owner-terminal-idle () null)
(defun test-compaction-owner-terminal-idle ()
  "Run due terminal episodes with and without mission ownership, then accept later input."
  (dolist (owned-p '(nil t))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (let* ((controller                       (make-instance 'application-input-controller :application application :main-thread (current-thread)))
              (*idle-compaction-clock-function* (lambda () 1180)))
         (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
         (setf (application-input-controller application) controller
               (config :prompt-cache-retention-seconds (application-configuration application)) 300)
         (idle-compaction-tests--prime application)
         (let ((work (application-input-controller--next-work controller)))
           (test-assert (eq :idle-compaction (first work)) "A due real episode selects idle work.")
           (compaction-owner-test--call-with-credentials application
            (lambda () (application-input-controller--run-work controller work)))
           (application-input-controller--finish-work controller)
           (compaction-owner-test--checkpoint application)
           (test-assert (null (application-idle-compaction-episode application)) "The idle opportunity is consumed once.")
           (test-assert (not (application-input-controller-active-p controller)) "The terminal controller releases activity.")
           (test-assert (equal '(t) (compaction-owner-compaction-flags provider)) "Idle maintenance completes one paid summary.")
           (when owned-p (compaction-owner-test--settled application 150))
           (compaction-owner-test--call-with-credentials
            application (lambda ()
                         (application--run-message-input application "Later actual user input." :tools-p nil :goal-continuations-p nil)))
           (test-assert (equal '(nil t) (compaction-owner-compaction-flags provider)) "Later input gets its ordinary answer.")
           (when owned-p (compaction-owner-test--settled application 180)))
         (setf (application-input-controller application) nil)))
     :owned-p owned-p :large-p t))
  nil)

(-> test-compaction-owner-acp-idle () null)
(defun test-compaction-owner-acp-idle ()
  "Use the shared idle owner and channel-to-ACP outcome adapter, then accept later input."
  (dolist (owned-p '(nil t))
    (compaction-owner-test--matrix-checks
     (compaction-owner-test--matrix-snapshot :entry :acp-idle :owned-p owned-p :reuse-p t)))
  nil)


;;;; -- Unpublishable Compaction --

(-> compaction-owner-test--no-fit
    (application compaction-owner-test-provider keyword
     &key (:entry keyword) (:session t) (:client t)) null)
(defun compaction-owner-test--no-fit (application provider phase &key entry session client)
  "Exercise pre-dispatch and postpaid refusal through shared channel owners."
  (declare (ignore client))
  (setf (config :compaction-threshold-percent (application-configuration application)) 35)
  (let* ((conversation                     (application-conversation application))
         (controller                       (or (application-input-controller application)
                                               (make-instance 'application-input-controller
                                                              :application application :main-thread (current-thread))))
         (*idle-compaction-clock-function* (lambda () 1180))
         (note-original                    (symbol-function 'agent--note-compaction-request))
         (arrival nil) (escaped nil) (result nil) (ticket nil) (service nil) (completion-id nil))
    (ecase phase
      (:before
       (conversation-append-user-message conversation (make-string 600000 :initial-element #\a))
       (conversation-append-provider-item conversation (agent-test-message "Protected exchange completed."))
       (conversation-append-user-message conversation (make-string (if (eq entry :acp-completion) 600000 1000) :initial-element #\b)))
      (:after
       (conversation-append-user-message conversation "Older large response.")
       (conversation-append-provider-item conversation (agent-test-message (make-string 190000 :initial-element #\h)))
       (conversation-append-user-message conversation "Summarize before continuing.")
       (setf arrival (lambda ()
                       (conversation-append-user-message conversation (make-string 600000 :initial-element #\n))))))
    (setf (application-input-controller application) controller)
    (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
    (when (member entry '(:terminal :acp))
      (setf (config :prompt-cache-retention-seconds (application-configuration application)) 300)
      (idle-compaction-tests--prime application))
    (when (eq entry :wakeup)
      (setf service (mission-schedule-service-create
                     application :clock (lambda () 100)
                     :enqueue           (lambda (owner command) (declare (ignore owner command)) t))
            (gethash application *mission-schedule-services*) service)
      (application-mission-schedule-add application :id "no-fit" :content "Perform scheduled work." :at 100 :interval 10)
      (setf ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
    (when (eq entry :acp-queue)
      (channel-subscription-submit session "Actual queued ACP input." :source ':queue)
      (channel-subscription-submit session "Remaining queued ACP input." :source ':queue))
    (when (eq entry :acp-completion)
      (setf completion-id (compaction-owner-test--ready-completion application)))
    (test-call-with-function-replacements
     (list (list 'agent--note-compaction-request
                 (lambda (agent observer &rest arguments)
                   (multiple-value-prog1 (apply note-original agent observer arguments)
                     (when arrival
                       (funcall (shiftf arrival nil)))))))
     (lambda ()
       (compaction-owner-test--call-with-credentials
        application
        (lambda ()
          (handler-case
              (setf result
                    (ecase entry
                      (:manual
                       (application-compact application))
                      (:turn
                       (nth-value 1 (application--run-turn application "Perform requested work." :tools-p nil :continuation-p t)))
                      (:message
                       (application--run-message-input application "Actual mission input." :tools-p nil))
                      (:continuation
                       (application--run-goal-continuations application))
                      (:wakeup
                       (application-operation-call application "mission-wakeup" ticket))
                      ((:terminal :acp :acp-queue :acp-completion)
                       (compaction-owner-test--run-next controller))
                      (:acp-prompt
                       (acp-session-prompt session "Actual explicit ACP input."))))
            (serious-condition (condition) (setf escaped condition)))))))
    (let* ((records (conversation-records-newest conversation 100))
           (loaded  (conversation-load (conversation-pathname conversation))))
      (test-assert (null escaped) "A local context refusal is explained, not a channel or ACP crash.")
      (test-assert (not (find :summary records :key #'first)) "No invalid checkpoint is published.")
      (test-assert (not (find :turn-aborted records :key #'first)) "A local refusal is not a failed model turn.")
      (test-assert (find-if (lambda (record) (and (eq :side-status (first record))
                                                (test-object-contains-string-p record "context admission refused"))) records)
                   "The refusal has a persisted informational explanation.")
      (test-assert (equal (conversation-input-history loaded) (conversation-input-history conversation))
                   "Reload preserves the original history and every concurrent arrival.")
      (when completion-id
        (test-assert (task-completion--delivered-p loaded completion-id) "The claimed job is delivered durably once."))
      (test-assert (= (if (eq phase :after) 1 0) (length (compaction-owner-input-snapshots provider)))
                   "Only an admitted summary incurs usage.")
      (compaction-owner-test--settled application (if (eq phase :after) 150 0))
      (when (eq entry :acp-prompt)
        (test-assert (eq :end-turn result) "A local refusal completes the ACP submission without -32603."))
      (when (eq entry :turn)
        (test-assert (eq :failed result) "The shared inference owner stops successful finalization on refusal."))
      (when service
        (let* ((occurrence (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups)))
               (restored   (mission-schedule-service-create application :clock (lambda () 100))))
          (test-assert (eq :failed (getf occurrence :state)) "The refused wakeup settles failed, not executed or unknown.")
          (test-assert (zerop (hash-table-count (mission-schedule-executing service))) "Wakeup execution ownership is released.")
          (test-assert (eq :failed (getf (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler restored)) :wakeups)) :state))
                       "Reload retains the terminal wakeup refusal.")
          (let ((count (length (compaction-owner-input-snapshots provider))))
            (handler-case (application-operation-call application "mission-wakeup" ticket) (mission-error () nil))
            (test-assert (= count (length (compaction-owner-input-snapshots provider))) "A settled wakeup cannot repeat paid work.")))))
    (when (eq entry :continuation)
      (test-assert (= 1 (getf (application-goal application) :continuations 0)) "Refusal stops after one continuation."))
    (when (member entry '(:terminal :acp))
      (test-assert (null (application-idle-compaction-episode application)) "Idle maintenance consumes its episode once."))
    (test-assert (not (application-input-controller-active-p controller)) "The channel releases work ownership.")
    (when session
      (test-assert (not (channel-subscription-submission-active-p session)) "The ACP adapter releases prompt ownership."))
    (unless session (setf (application-input-controller application) nil)))
  nil)

(-> test-compaction-owner-no-fit-product-boundaries () null)
(defun test-compaction-owner-no-fit-product-boundaries ()
  "Check pre-dispatch exhaustion and post-paid refusal."
  (dolist (phase '(:before :after))
    (dolist (entry '(:manual :turn :message :continuation :wakeup :terminal))
      (compaction-owner-test--fixture
       (lambda (application provider) (compaction-owner-test--no-fit application provider phase :entry entry))
       :owned-p t)))
  nil)

(-> test-compaction-owner-no-fit-acp () null)
(defun test-compaction-owner-no-fit-acp ()
  "Explain both no-fit phases through the shared channel idle owner."
  (dolist (phase '(:before :after))
    (compaction-owner-test--matrix-fixture
     (lambda (application provider session events)
       (declare (ignore events))
       (compaction-owner-test--no-fit application provider phase :entry :acp :session session))
     :acp-p t :owned-p t))
  nil)


;;;; -- ACP Prompt and Continuation Refusals --

(-> compaction-owner-test--acp-turn-refusal (keyword) null)
(defun compaction-owner-test--acp-turn-refusal (entry)
  "Finalize actual channel queue and job owners, and the ACP prompt adapter, once per refusal."
  (dolist (phase '(:before :after))
    (compaction-owner-test--matrix-fixture
     (lambda (application provider session events)
       (let ((agent (application-agent application)))
         (unwind-protect
              (progn
                (compaction-owner-test--no-fit application provider phase :entry entry :session session)
                (test-assert (= 1 (count :work-finished (first events) :key #'first))
                             "Refusal publishes exactly one terminal channel work event.")
                (test-assert (null (acp-session-turn session)) "The ACP turn closes once.")
                (test-assert (eq :end-turn (acp-session-last-outcome session)) "The ACP observer does not classify refusal as -32603.")
                (test-assert (application-input-controller-queued-work-paused-p (application-input-controller application))
                             "Refusal pauses automatic queued work.")
                (test-assert (null (task-completion-pending agent)) "Delivered completion work cannot repeat."))
           (when (eq entry :acp-completion)
             (task-orchestrator-close (task-completion--runtime agent))))))
     :acp-p t :owned-p t))
  nil)

(-> test-compaction-owner-no-fit-acp-prompt () null)
(defun test-compaction-owner-no-fit-acp-prompt ()
  "Return a plain protocol refusal for real explicit ACP prompts."
  (compaction-owner-test--acp-turn-refusal :acp-prompt)
  (compaction-owner-test--wire-refusal))

(-> test-compaction-owner-no-fit-acp-queue () null)
(defun test-compaction-owner-no-fit-acp-queue ()
  "Consume queued ACP input once when recovery cannot fit."
  (compaction-owner-test--acp-turn-refusal :acp-queue))

(-> test-compaction-owner-no-fit-acp-completion () null)
(defun test-compaction-owner-no-fit-acp-completion ()
  "Deliver a real job once without crashing its automatic ACP continuation."
  (compaction-owner-test--acp-turn-refusal :acp-completion))


;;;; -- Paid Summary Arrival and Terminal Ownership --

(-> compaction-owner-test--owner-snapshot
    (&key (:entry keyword) (:owned-p boolean) (:arrival-size (option (integer 1)))
          (:reuse-p boolean) (:controller-p boolean) (:one-shot-p boolean)) list)
(defun compaction-owner-test--owner-snapshot
    (&key entry owned-p arrival-size reuse-p controller-p one-shot-p)
  "Observe real owning work and append a canonical arrival after original decoded usage persistence."
  (let ((snapshot nil))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (let* ((conversation  (application-conversation application))
              (configuration (application-configuration application))
              (note-original (symbol-function 'agent--note-compaction-request))
              (fit-original  (symbol-function 'agent--validate-compaction-projection))
              (controller    (if controller-p
                              (application-input-controller-create application :load-pending-p nil :start-reader-p nil)
                              (make-instance 'application-input-controller
                                             :application application :main-thread (current-thread))))
              (events nil) (summaries 0) (failure nil) (result nil)
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
           (list 'agent--note-compaction-request
                 (lambda (agent observer &rest arguments)
                   (multiple-value-prog1 (apply note-original agent observer arguments)
                     (let ((usage (provider-usage-normalize (getf arguments :usage))))
                       (push (list :usage-persisted :summary
                                   (getf arguments :request-number)
                                   (json-get usage "input_tokens") (json-get usage "output_tokens")) events))
                     (incf summaries)
                     (when (and arrival-size (= summaries 1))
                       (conversation-append-user-message
                        conversation (make-string arrival-size :initial-element #\n))
                       (push (list :canonical-arrival arrival-size) events)))))
           (list 'agent--validate-compaction-projection
                 (lambda (&rest arguments)
                   (push (list :validation-start) events)
                   (handler-bind
                       ((provider-context-budget-error
                          (lambda (condition)
                            (compaction-owner-test--refusal-budget condition)
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
                          :arrival-size        arrival-size
                          :summaries           summaries
                          :events              (reverse events)
                          :failure             (and failure (type-of failure))
                          :result              (and (keywordp result) result)
                          :flags               (copy-list (compaction-owner-compaction-flags provider))
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
              (setf (compaction-owner-results provider)
                    (list (agent-test-result "reuse-summary" (list (agent-test-checkpoint-call))
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
                    (getf snapshot :reuse-flags) (copy-list (compaction-owner-compaction-flags provider))
                    (getf snapshot :reuse-input-preserved-p)
                    (not (null (member "Later explicit user work." (conversation-input-history conversation) :test #'equal)))))))
         (setf (application-input-controller application) nil)))
     :owned-p owned-p :large-p t)
    snapshot))

(-> compaction-owner-test--paid-snapshot-checks (list) null)
(defun compaction-owner-test--paid-snapshot-checks (snapshot)
  "Check genuine paid completion, canonical arrival and original final guard in order."
  (let* ((events (getf snapshot :events))
         (kinds  (mapcar #'first events))
         (usage  (find :summary events :key #'second)))
    (test-assert (= 1 (getf snapshot :summaries)) "The production decoder completes exactly one summary.")
    (test-assert (and usage (= 100 (fourth usage)) (= 50 (fifth usage)))
                 "The original post-decoder event persists metered input100/output50.")
    (test-assert (and (position :canonical-arrival kinds) (position :validation-start kinds)
                      (< (position :canonical-arrival kinds) (position :validation-start kinds)))
                 "The legitimate arrival precedes the original publication fit check.")
    (test-assert (getf snapshot :arrival-preserved-p) "Canonical reload retains the entire new user input.")
    (test-assert (member :validation-refusal kinds) "The actual publication guard rejects the next projection.")
    (test-assert (not (getf snapshot :checkpoint-p)) "An invalid summary is never durably published.")
    (when (getf snapshot :owned-p)
      (test-assert (= 150 (getf snapshot :paid)) "Completed paid summary usage is retained exactly once.")
      (test-assert (eq :active (getf snapshot :mission-status)) "The definitive fit refusal preserves the mission.")
      (dolist (key '(:outstanding :reserved :unknown))
        (test-assert (zerop (getf snapshot key)) (format nil "The genuine refusal releases ~A." key)))))
  nil)

(-> test-compaction-owner-paid-arrival-fixture () null)
(defun test-compaction-owner-paid-arrival-fixture ()
  "Qualify original-once usage interleaving with and without a small arrival."
  (dolist (arrival-size '(nil 64))
    (let ((snapshot (compaction-owner-test--owner-snapshot
                     :entry ':manual :owned-p t :arrival-size arrival-size)))
      (test-assert (null (getf snapshot :failure)) "A fitting control returns normally.")
      (test-assert (getf snapshot :checkpoint-p) "Fitting controls publish a real validated checkpoint.")
      (test-assert (= 1 (getf snapshot :summaries)) "The original stream and decoder complete one summary.")
      (test-assert (getf snapshot :arrival-preserved-p) "The small concurrent arrival is canonical.")
      (test-assert (member :validation-returned (mapcar #'first (getf snapshot :events)))
                   "The actual original validator returns normally for fitting controls.")))
  nil)

(-> test-compaction-owner-paid-refusals () null)
(defun test-compaction-owner-paid-refusals ()
  "Reject actual post-paid unpublishable projections without aborting manual or scheduled owners."
  (dolist (entry '(:manual :terminal-message :wakeup))
    (let ((snapshot (compaction-owner-test--owner-snapshot
                     :entry entry :owned-p t :arrival-size 600000)))
      (compaction-owner-test--paid-snapshot-checks snapshot)
      (test-assert (null (getf snapshot :failure)) "The real owner presents an explained refusal normally.")
      (test-assert (not (getf snapshot :aborted-p)) "A definitive fit refusal is not a crashed turn.")
      (test-assert (getf snapshot :notice-p) "The owner durably explains the context refusal.")
      (when (eq entry :wakeup)
        (test-assert (eq :failed (getf snapshot :wakeup-state)) "A refused occurrence is failed, not executed or unknown.")
        (test-assert (eq :runner-failed (getf snapshot :wakeup-result)) "The default runner reports truthful failure.")
        (test-assert (zerop (getf snapshot :executing)) "The real executing reservation is released."))))
  nil)

(-> test-compaction-owner-terminal-messages () null)
(defun test-compaction-owner-terminal-messages ()
  "Reach real submitted terminal message work with and without a mission owner."
  (dolist (owned-p '(nil t))
    (let ((snapshot (compaction-owner-test--owner-snapshot :entry ':terminal-message :owned-p owned-p)))
      (test-assert (null (getf snapshot :failure)) "Compaction does not escape the terminal owner.")
      (test-assert (= 1 (getf snapshot :input-count)) "Real submitted user input is persisted exactly once.")
      (test-assert (equal '(nil t) (getf snapshot :flags)) "One real summary and the original ordinary answer complete.")
      (test-assert (getf snapshot :checkpoint-p) "The actual owner publishes the checkpoint.")
      (test-assert (not (getf snapshot :aborted-p)) "The real message lifecycle never records an abort.")
      (test-assert (not (getf snapshot :controller-active-p)) "The terminal work releases controller activity.")
      (when owned-p
        (test-assert (= 180 (getf snapshot :paid))
                     "Only actual decoded summary and answer usage is charged."))))
  nil)


;;;; -- Actual ACP and Idle Owner Matrix --

(-> compaction-owner-test--matrix-fixture (function &key (:acp-p boolean) (:owned-p boolean)) null)
(defun compaction-owner-test--matrix-fixture (function &key acp-p owned-p)
  "Run shared channel owners and the ACP outcome adapter with inert channel transport."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (let* ((configuration   (application-configuration application))
            (controller      (make-instance 'application-input-controller
                                            :application application :main-thread (current-thread)))
            (service         (make-instance 'acp-service :configuration configuration))
            (session         (when acp-p
                               (make-instance 'acp-session :service service
                                              :identifier           (conversation-identifier (application-conversation application))
                                              :configuration        configuration)))
            (events          (list nil))
            (queued-boundary nil)
            (publish         (symbol-function 'application-clients-publish)))
       (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
       (setf (application-input-controller application) controller)
       (task-augment-tool-registry (agent-tool-registry (application-agent application)))
       (when session
         (setf (acp-session-update-buffer session)
               (agentcomms:make-agent-update-buffer service (channel-subscription-identifier session))))
       (unwind-protect
            (test-call-with-function-replacements
             (list
              (list 'application-clients-publish
                    (lambda (owner event)
                      (funcall publish owner event)
                      (when (eq owner application)
                        (push event (first events))
                        ;; Admit a real command boundary behind one mission turn so
                        ;; unrelated successful goal expansion is deferred.
                        (when (and (eq :work-started (first event))
                                   (member (getf (rest event) :kind) '(:message :job-completion))
                                   (mission-goal-p (application-goal application))
                                   (deque-empty-p (application-input-controller-work-items controller)))
                          (setf queued-boundary (list :command "/status"))
                          (application-input-controller-enqueue-work controller queued-boundary))
                        (when (and (eq :work-finished (first event)) queued-boundary)
                          (test-assert (eq queued-boundary
                                           (first (deque->list (application-input-controller-work-items controller))))
                                       "Successful mission expansion yields to queued channel work.")
                          (deque-pop-front (application-input-controller-work-items controller))
                          (setf queued-boundary nil
                                (application-deferred-goal-continuation application) nil)
                          (application-input-controller--persist-pending controller))
                        (when (and session (member (first event) '(:work-started :work-finished)))
                          (channel-subscription-event session event)))))
              (list 'acp-extension-notify
                    (lambda (recipient kind &optional data)
                      (declare (ignore recipient))
                      (push (list :extension kind data) (first events))))
              (list 'channel-subscription-call
                    (lambda (recipient command &rest arguments)
                      (test-assert (eq recipient session) "The editor addresses its channel session.")
                      (ecase command
                        (:submit
                         (application-input-controller--queue-input
                          controller
                          (user-message-input-create :text       (getf arguments :text)
                                                     :submission (getf arguments :submission)))
                         (when (eq ':prompt (getf (getf arguments :submission) :source))
                           (compaction-owner-test--run-next controller))
                         (list :busy-p nil))
                        (:cancel
                         (application-input-controller--request-active-turn-cancellation
                          controller :pause-queued-work-p t))))))
             (lambda () (funcall function application provider session events)))
         (setf (application-input-controller application) nil))))
   :owned-p owned-p :large-p t)
  nil)

(-> compaction-owner-test--matrix-snapshot
    (&key (:entry keyword) (:owned-p boolean) (:arrival-p boolean) (:image-p boolean)
          (:reuse-p boolean) (:cancel-before-refusal-p boolean)) list)
(defun compaction-owner-test--matrix-snapshot
    (&key entry owned-p arrival-p image-p reuse-p cancel-before-refusal-p)
  "Observe shared channel work, durable recovery and ACP outcome translation without old ACP owners."
  (let ((snapshot nil))
    (compaction-owner-test--matrix-fixture
     (lambda (application provider session wire)
       (let* ((conversation                     (application-conversation application))
              (configuration                    (application-configuration application))
              (controller                       (application-input-controller application))
              (note-original                    (symbol-function 'agent--note-compaction-request))
              (fit-original                     (symbol-function 'agent--validate-compaction-projection))
              (*idle-compaction-clock-function* (lambda () 1180))
              (summaries 0) (events nil) (failure nil) (result nil) (completion-id nil))
         (setf (compaction-owner-results provider)
               (list (agent-test-result "matrix-summary" (list (agent-test-checkpoint-call))
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
           (idle-compaction-tests--prime application))
         (when (eq entry :acp-queue)
           (channel-subscription-submit session "Matrix claimed queued input." :source ':queue)
           (channel-subscription-submit session "Matrix unclaimed queued tail." :source ':queue))
         (when (eq entry :acp-completion)
           (setf completion-id (compaction-owner-test--ready-completion application)))
         (unwind-protect
              (test-call-with-function-replacements
               (list
                (list 'credential-manager-credentials
                      (lambda (&rest arguments) (declare (ignore arguments)) (provider-tests--credentials configuration)))
                (list 'agent--note-compaction-request
                      (lambda (agent observer &rest arguments)
                        (multiple-value-prog1 (apply note-original agent observer arguments)
                          (let ((usage (provider-usage-normalize (getf arguments :usage))))
                            (push (list :usage-persisted :summary (getf arguments :request-number)
                                        (json-get usage "input_tokens") (json-get usage "output_tokens")) events))
                          (incf summaries)
                          (when (and arrival-p (= summaries 1))
                            (conversation-append-user-message conversation (make-string 600000 :initial-element #\n))
                            (push (list :canonical-arrival 600000) events)))))
                (list 'agent--validate-compaction-projection
                      (lambda (&rest arguments)
                        (push (list :validation-start) events)
                        (handler-bind ((provider-context-budget-error
                                         (lambda (condition)
                                           (compaction-owner-test--refusal-budget condition)
                                           (push (list :validation-refusal (type-of condition)) events)
                                           (when cancel-before-refusal-p
                                             (application-input-controller--request-active-turn-cancellation
                                              controller :pause-queued-work-p t)))))
                          (apply fit-original arguments)))))
               (lambda ()
                 (handler-case
                     (setf result (if (eq entry :acp-prompt)
                                      (acp-session-prompt session "Matrix actual client input.")
                                      (compaction-owner-test--run-next controller)))
                   (serious-condition (condition) (setf failure condition)))
                 (let* ((records (conversation-records-newest conversation 200))
                        (loaded  (conversation-load (conversation-pathname conversation)))
                        (goal    (application-goal application)))
                   (setf snapshot
                         (list :entry entry :owned-p owned-p
                               :arrival-size (when arrival-p 600000) :image-p image-p :summaries summaries
                               :events (reverse events) :failure (and failure (type-of failure)) :result result
                               :flags                   (copy-list (compaction-owner-compaction-flags provider))
                               :checkpoint-p            (not (null (find :summary records :key #'first)))
                               :aborted-p               (not (null (find :turn-aborted records :key #'first)))
                               :notice-p                (not (null (find-if (lambda (record) (and (eq :side-status (first record))
                                                                                                 (test-object-contains-string-p record "context admission refused"))) records)))
                               :answer-p                (test-object-contains-string-p (conversation-input-items loaded) "Matrix ordinary answer.")
                               :arrival-preserved-p     (or (not arrival-p) (member (make-string 600000 :initial-element #\n)
                                                                                   (conversation-input-history loaded) :test #'equal))
                               :paid (getf goal :tokens-used 0) :outstanding (getf goal :requests-outstanding 0)
                               :reserved (getf goal :tokens-reserved 0) :unknown (getf goal :unknown-usage 0)
                               :mission-status          (getf goal :status)
                               :ui-compacting-p         (terminal-ui-compacting-p (application-ui application))
                               :controller-active-p     (application-input-controller-active-p controller)
                               :paused-p                (application-input-controller-queued-work-paused-p controller)
                               :prompt-active-p         (and session (channel-subscription-submission-active-p session))
                               :turn-open-p             (and session (acp-session-turn session))
                               :episode-p               (not (null (application-idle-compaction-episode application)))
                               :finished-count          (count :work-finished (first wire) :key #'first)
                               :protocol-outcome        (and session (acp-session-last-outcome session))
                               :queued-input-count      (count "Matrix claimed queued input." (conversation-input-history loaded) :test #'equal)
                               :queue-tail-in-history-p (member "Matrix unclaimed queued tail." (conversation-input-history loaded) :test #'equal)
                               :queue-tail-pending-p    (and (eq entry :acp-queue) (not (deque-empty-p (application-input-controller-work-items controller))))
                               :queue-reloaded-tail-p
                               (and (eq entry :acp-queue)
                                    (let ((reopened (make-instance 'application-input-controller :application application :main-thread nil)))
                                      (application-input-controller--load-pending reopened)
                                      (equal '("Matrix unclaimed queued tail.")
                                             (mapcar (lambda (work) (user-message-input-text (second work)))
                                                     (deque->list (application-input-controller-work-items reopened))))))
                               :completion-delivered-p  (and completion-id (task-completion--delivered-p loaded completion-id))
                               :automatic-work-p        (or (application-input-controller-pending-work-p controller)
                                                            (application-job-completions--take-work controller)))))
                 (when reuse-p
                   (let ((before  (length (compaction-owner-input-snapshots provider)))
                         (history (conversation-input-history conversation))
                         (later-failure nil) (later-result nil) (conditions nil))
                     (setf (compaction-owner-results provider)
                           (list (agent-test-result "matrix-later-answer" (list (agent-test-message "Matrix later ordinary answer."))
                                                    :usage (json-object "input_tokens" 20 "output_tokens" 10))))
                     ;; Explicit input resumes paused work; automatic dispatch never clears this pause.
                     (setf (application-input-controller-queued-work-paused-p controller) nil)
                     (let ((request-original (symbol-function 'provider-check-request-context)))
                       (test-call-with-function-replacements
                        (list (list 'provider-check-request-context
                                    (lambda (&rest arguments)
                                      (handler-bind ((provider-context-budget-error
                                                       (lambda (condition) (push (type-of condition) conditions))))
                                        (apply request-original arguments)))))
                        (lambda ()
                          (handler-case
                           (setf later-result (if session
                                                  (acp-session-prompt session "Matrix later explicit input.")
                                                  (application--run-message-input application "Matrix later explicit input."
                                                                                  :tools-p nil :goal-continuations-p nil)))
                           (serious-condition (condition) (setf later-failure condition))))))
                     (setf (getf snapshot :later-failure) (and later-failure (type-of later-failure))
                            (getf snapshot :later-failure-message) (and later-failure (princ-to-string later-failure))
                           (getf snapshot :later-result) later-result
                           (getf snapshot :later-context-conditions) conditions
                           (getf snapshot :later-opens) (- (length (compaction-owner-input-snapshots provider)) before)
                           (getf snapshot :later-role) (first (compaction-owner-compaction-flags provider))
                           (getf snapshot :later-answer-p) (test-object-contains-string-p (conversation-input-items conversation) "Matrix later ordinary answer.")
                           (getf snapshot :prior-history-preserved-p) (equal history (subseq (conversation-input-history conversation) 0 (length history)))
                           (getf snapshot :window) (config :context-window configuration)
                           (getf snapshot :later-owner-clean-p)
                           (and (not (application-input-controller-active-p controller))
                                (or (null session) (not (channel-subscription-submission-active-p session)))
                                (zerop (getf (application-goal application) :requests-outstanding 0))
                                (zerop (getf (application-goal application) :tokens-reserved 0))))))))
           (when completion-id (task-orchestrator-close (task-completion--runtime (application-agent application)))))))
     :acp-p (not (eq entry :terminal-idle)) :owned-p owned-p)
    snapshot))

(-> compaction-owner-test--matrix-checks (list) null)
(defun compaction-owner-test--matrix-checks (snapshot)
  "Assert shared owner cleanup, publication or local refusal and protocol outcomes."
  (test-assert (null (getf snapshot :failure)) "A local refusal never escapes as a channel or ACP crash.")
  (test-assert (not (getf snapshot :aborted-p)) "A local refusal is not recorded as a crashed model turn.")
  (test-assert (= 1 (getf snapshot :finished-count)) "Channel work finalizes exactly once.")
  (dolist (key '(:controller-active-p :prompt-active-p :turn-open-p :ui-compacting-p))
    (test-assert (not (getf snapshot key)) (format nil "The owner releases ~A." key)))
  (when (getf snapshot :protocol-outcome)
    (test-assert (eq :end-turn (getf snapshot :protocol-outcome)) "A local refusal is a normal ACP outcome, never -32603."))
  (if (getf snapshot :arrival-size)
      (progn
        (compaction-owner-test--paid-snapshot-checks snapshot)
        (test-assert (getf snapshot :notice-p) "A postpaid refusal has a persisted context explanation.")
        (test-assert (not (getf snapshot :automatic-work-p)) "Refusal cannot automatically retry queued or completion work."))
      (progn
        (test-assert (getf snapshot :checkpoint-p) "Fitting recovery publishes a durable checkpoint.")
        (test-assert (= 1 (getf snapshot :summaries)) "The projection uses one checkpoint request.")
        (unless (member (getf snapshot :entry) '(:terminal-idle :acp-idle))
          (test-assert (getf snapshot :answer-p) "The original user or completion turn receives its ordinary answer."))))
  (when (getf snapshot :owned-p)
    (dolist (key '(:outstanding :reserved :unknown))
      (test-assert (zerop (getf snapshot key)) (format nil "The request releases ~A." key))))
  (when (member (getf snapshot :entry) '(:terminal-idle :acp-idle))
    (test-assert (not (getf snapshot :episode-p)) "Idle maintenance consumes its episode once."))
  (when (eq :acp-queue (getf snapshot :entry))
    (test-assert (= 1 (getf snapshot :queued-input-count)) "The claimed queued prompt is durable once.")
    (test-assert (not (getf snapshot :queue-tail-in-history-p)) "The unclaimed tail is not persisted as executed input.")
    (test-assert (getf snapshot :queue-tail-pending-p) "The queue retains its unclaimed tail.")
    (test-assert (getf snapshot :queue-reloaded-tail-p) "Pending-state reload retains exactly the unclaimed tail."))
  (when (eq :acp-completion (getf snapshot :entry))
    (test-assert (getf snapshot :completion-delivered-p) "The genuine job completion has a durable delivery receipt."))
  (when (getf snapshot :window)
    (test-assert (getf snapshot :later-owner-clean-p) "Later explicit work releases ownership and reservations.")
    (test-assert (getf snapshot :prior-history-preserved-p) "Later input retains the earlier canonical history.")
    (test-assert (= 144000 (getf snapshot :window)) "Explicit retry uses the same configured window.")
    (test-assert (null (getf snapshot :later-failure))
                 (format nil "Later input failed in ~S: ~A (~A)."
                         (loop for key in '(:entry :owned-p :arrival-size)
                               append (list key (getf snapshot key)))
                         (getf snapshot :later-failure)
                         (getf snapshot :later-failure-message)))
    (if (getf snapshot :arrival-size)
        (progn
          (test-assert (getf snapshot :later-context-conditions) "The oversized protected arrival still reaches real admission.")
          (test-assert (= 0 (getf snapshot :later-opens)) "An unfit explicit retry opens no transport."))
        (progn
          (test-assert (= 1 (getf snapshot :later-opens)) "A fitting later input opens one ordinary request.")
          (test-assert (null (getf snapshot :later-role)) "The later request is an answer, not another summary.")
          (test-assert (getf snapshot :later-answer-p) "The later ordinary answer is durable."))))
  nil)

(-> test-compaction-owner-acp-matrix () null)
(defun test-compaction-owner-acp-matrix ()
  "Exercise actual client, claimed queue and completed-job owners."
  (dolist (entry '(:acp-prompt :acp-queue :acp-completion))
    (dolist (owned-p '(nil t))
      (dolist (arrival-p '(nil t))
        (compaction-owner-test--matrix-checks
         (compaction-owner-test--matrix-snapshot :entry entry :owned-p owned-p :arrival-p arrival-p)))))
  nil)

(-> test-compaction-owner-idle-matrix () null)
(defun test-compaction-owner-idle-matrix ()
  "Run terminal and ACP due episodes through real idle owners on all ownership cells."
  (dolist (entry '(:terminal-idle :acp-idle))
    (dolist (owned-p '(nil t))
      (dolist (arrival-p '(nil t))
        (compaction-owner-test--matrix-checks
         (compaction-owner-test--matrix-snapshot :entry entry :owned-p owned-p :arrival-p arrival-p :reuse-p t)))))
  nil)

(-> test-compaction-owner-image-and-reuse () null)
(defun test-compaction-owner-image-and-reuse ()
  "Contrast synthetic image and text owners and role-qualify fitting later ordinary answers."
  (dolist (image-p '(nil t))
    (dolist (arrival-p '(nil t))
      (compaction-owner-test--matrix-checks
       (compaction-owner-test--matrix-snapshot :entry ':acp-prompt :owned-p t
                                             :arrival-p arrival-p :image-p image-p :reuse-p t))))
  nil)

(defclass compaction-owner-values-agent (agent)
  ((return-values :initarg :return-values :reader compaction-owner-test-return-values
                  :documentation "The complete values returned by a test generic extension."))
  (:documentation "Agent extension exercising the owner's primary-value contract."))

(defmethod agent-run-user-turn ((agent compaction-owner-values-agent) content &key &allow-other-keys)
  "Return the configured value count without replacing the generic function."
  (declare (ignore content))
  (values-list (compaction-owner-test-return-values agent)))

(-> test-compaction-owner-return-values () null)
(defun test-compaction-owner-return-values ()
  "Accept generic extensions returning zero, one, or multiple values at the real terminal owner."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (declare (ignore provider))
     (let* ((agent         (application-agent application))
            (primary       (agent-test-result "extension" (list (agent-test-message "Extension result."))))
            (note-original (symbol-function 'application--note-goal-turn)))
       (change-class agent 'compaction-owner-values-agent :return-values nil)
       (dolist (returned (list nil (list primary) (list primary :secondary)))
         (setf (slot-value agent 'return-values) returned)
         (let ((noted :unset) (failure nil) (result nil))
           (test-call-with-function-replacements
            (list (list 'application--note-goal-turn
                        (lambda (owner value)
                          (setf noted value)
                          (when value (funcall note-original owner value)))))
            (lambda ()
              (handler-case (setf result (multiple-value-list (application--run-turn application "Extension input.")))
                (error (condition) (setf failure condition)))))
           (test-assert (null failure) "The actual turn owner accepts the extension's value count.")
           (test-assert (equal (list nil (first returned)) result) "Only the primary extension value is selected.")
           (test-assert (eq noted (first returned)) "Goal accounting receives the primary value, including NIL."))))))
  nil)

(-> test-compaction-owner-idle-cancellation-refusal () null)
(defun test-compaction-owner-idle-cancellation-refusal ()
  "Give recorded channel cancellation precedence over final-fit idle refusal."
  (dolist (cancel-p '(nil t))
    (let ((snapshot (compaction-owner-test--matrix-snapshot
                     :entry :acp-idle :owned-p t :arrival-p t :cancel-before-refusal-p cancel-p)))
      (test-assert (= 1 (getf snapshot :summaries)) "The paid summary completed before final validation.")
      (test-assert (= 150 (getf snapshot :paid)) "Paid usage settles once.")
      (test-assert (eq :active (getf snapshot :mission-status)) "The mission retains its active authority.")
      (test-assert (getf snapshot :arrival-preserved-p) "Canonical arrival history survives cancellation.")
      (test-assert (not (getf snapshot :checkpoint-p)) "The unfit checkpoint is not published.")
      (test-assert (if cancel-p (eq 'application-turn-cancelled (getf snapshot :failure))
                      (null (getf snapshot :failure))) "Recorded cancellation wins over refusal.")
      (test-assert (eq (not cancel-p) (getf snapshot :notice-p)) "Cancellation emits no spurious refusal notice.")
      (test-assert (not (getf snapshot :controller-active-p)) "Cancellation releases channel ownership.")
      (test-assert (not (getf snapshot :prompt-active-p)) "The ACP prompt adapter retains no owner.")
      (test-assert (not (getf snapshot :turn-open-p)) "The ACP observer closes the turn.")
      (test-assert (= 1 (getf snapshot :finished-count)) "Cancellation finalizes channel work once.")
      (test-assert (not (getf snapshot :episode-p)) "The idle episode is consumed once.")
      (test-assert (not (getf snapshot :aborted-p)) "Idle cancellation does not fabricate a failed model turn.")
      (test-assert (every #'zerop (mapcar (lambda (key) (getf snapshot key)) '(:outstanding :reserved :unknown)))
                   "Accounting retains no outstanding reservation or unknown usage.")))
  nil)

(-> test-compaction-owner-finalization-errors () null)
(defun test-compaction-owner-finalization-errors ()
  "Keep goal, title and terminal presentation failures outside the inference refusal catch."
  (dolist (target '(application--note-goal-turn application--maybe-name-session application-render-records))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (declare (ignore provider))
       (let* ((agent     (application-agent application))
              (condition (make-condition 'provider-context-budget-error
                                         :estimated-input-tokens 300000 :output-reserve 8192
                                         :context-window 272000 :non-text-estimate-p nil))
              (caught nil) (presented 0))
         (change-class agent 'compaction-owner-values-agent
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

(-> test-compaction-owner-scheduled-controller () null)
(defun test-compaction-owner-scheduled-controller ()
  "Consume valid claimed wakeups through the default enqueue, terminal dispatch and default runner."
  (dolist (mode '(:fitting-recurring :refused-recurring :refused-one-shot))
    (let* ((arrival-p (not (eq mode :fitting-recurring)))
           (snapshot  (compaction-owner-test--owner-snapshot
                       :entry :wakeup :owned-p t
                       :controller-p t :one-shot-p (eq mode :refused-one-shot)
                       :arrival-size (when arrival-p 600000))))
      (test-assert (null (getf snapshot :failure)) "Actual scheduled controller dispatch returns without an owner exception.")
      (test-assert (member '(:terminal-work :native-lisp) (getf snapshot :events) :test #'equal)
                   "The valid ticket enters the real noninteractive native Lisp work path.")
      (test-assert (= 1 (getf snapshot :summaries)) "The checkpoint request is paid once.")
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
                   "No usage remains outstanding, reserved, or unknown.")))
  nil)

(-> compaction-owner-test--run-next (application-input-controller) null)
(defun compaction-owner-test--run-next (controller)
  "Dispatch and finalize one genuinely selected channel work item."
  (let ((work (application-input-controller--next-work controller)))
    (unwind-protect
         (application-input-controller--run-work controller work)
      (application-input-controller--finish-work controller)))
  nil)

(-> compaction-owner-test--ready-completion (application) non-empty-string)
(defun compaction-owner-test--ready-completion (application)
  "Start a real completed job and wait for its automatic continuation deadline."
  (let* ((agent    (application-agent application))
         (job      (job-completion-tests--start agent (task-completion--runtime agent)))
         (deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop until (application-job-completions--take-work (application-input-controller application))
          do (when (> (get-internal-real-time) deadline)
               (error "The completed job did not become eligible for channel delivery."))
             (sleep 0.01))
    (session-job-execution-identifier job)))

(-> compaction-owner-test--wire-refusal () null)
(defun compaction-owner-test--wire-refusal ()
  "Return local admission refusal over a served ACP connection without an RPC error."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "144000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192")
                          ("AUTOLITH_AUTOCOMPACT_THRESHOLD" "35"))
    (with-test-configuration (configuration)
      (let ((provider (make-instance 'compaction-owner-test-provider
                                     :configuration      configuration
                                     :credential-manager (provider-credential-manager (provider-create configuration))
                                     :results            nil)))
        (acp-session-test--call-with-client
         configuration
         (lambda (service client)
           (let* ((identifier   (agentcomms:client-new-session client (namestring (test-configuration-root configuration))))
                  (session      (acp-service--session service identifier))
                  (application  (acp-session-test--application session))
                  (conversation (application-conversation application)))
             (setf (config :session-title-generation-p configuration) nil)
             (conversation-append-user-message conversation (make-string 600000 :initial-element #\x))
             (conversation-append-provider-item conversation (agent-test-message "Wire earlier answer."))
             (conversation-append-user-message conversation (make-string 1000 :initial-element #\q))
             (test-assert (null (world-channel-find (world-load configuration) identifier))
                          "Fixture history precedes durable channel registration.")
             (test-assert (world-channel-find (application-world application) identifier)
                          "The hosted channel is already visible in the presentation catalogue.")
             (test-assert (application-world-agent-present-p application)
                          "A hosted-only projection does not refuse agent admission.")
             (compaction-owner-test--call-with-credentials
              application
              (lambda ()
                (let ((result (agentcomms:client-prompt
                               client identifier
                               (list (agentcomms:acp-text-content "Wire refused request.")))))
                  (test-assert (eq :end-turn result)
                               "A local refusal returns a normal ACP prompt response."))))
             (test-assert (test-object-contains-string-p (acp-session-test-updates client) "context admission refused")
                          "The ACP client receives the local refusal explanation.")
             (let ((updates nil))
               (acp-replay-map-updates conversation (lambda (update) (push update updates)))
               (test-assert (= 1 (count-if (lambda (update)
                                            (test-object-contains-string-p update "context admission refused"))
                                         updates))
                            "ACP replay includes the persisted refusal exactly once."))
             (test-assert (null (compaction-owner-compaction-flags provider))
                          "Neither summary nor answer crosses transport after preflight refusal.")
             (test-assert (not (find :turn-aborted (conversation-records-newest conversation 100) :key #'first))
                          "Wire refusal does not append a failed model turn.")
             (test-assert (= 2 (length (acp-session-test-turn-events client)))
                          "The served ACP turn starts and finalizes exactly once.")))
         :provider provider))))
  nil)
