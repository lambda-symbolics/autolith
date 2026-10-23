(in-package #:autolith)

;;;; -- Request-Local Context Tests --

(defvar *context-test-next-request-p* t
  "Whether the next-request test contributor is currently active.")

(defvar *context-test-invocation-state* nil
  "A shared mutable counter used by the contributor serialization test.")

(defparameter *context-test-sensitive-instruction*
  "CONTEXT-DELIVERY-INSTRUCTION-MUST-NOT-BE-RETAINED"
  "A request-local payload sentinel that retained diagnostics must discard.")

(defparameter *context-test-sensitive-evidence*
  "CONTEXT-DELIVERY-EVIDENCE-MUST-NOT-BE-RETAINED"
  "An evidence sentinel that retained diagnostics must discard.")

(-> context-tests--next-request (request-context) (option context-contribution))
(defun context-tests--next-request (context)
  "Return one edge-triggered contribution while its fixture is active."
  (declare (ignore context))
  (when *context-test-next-request-p*
    (make-context-contribution
     :identifier "next"
     :instruction "This appears on the next completed request only."
     :lifetime ':next-request)))

(-> context-tests--failure (request-context) null)
(defun context-tests--failure (context)
  "Signal the deterministic contributor failure used by diagnostics tests."
  (declare (ignore context))
  (error "broken contributor"))

(-> context-tests--conversation-advice (request-context) context-contribution)
(defun context-tests--conversation-advice (context)
  "Return advice identifying CONTEXT's conversation for diagnostic selection."
  (let ((identifier
          (conversation-identifier (request-context-conversation context))))
    (make-context-contribution
     :identifier "conversation-advice"
     :instruction (format nil "Advice for conversation ~A." identifier))))

(-> context-tests--serialized (request-context) context-contribution)
(defun context-tests--serialized (context)
  "Record one invocation for the contributor serialization test."
  (declare (ignore context))
  (incf (first *context-test-invocation-state*))
  (make-context-contribution
   :identifier "serialized"
   :instruction "Serialized contributor invocation."))

(-> context-tests--sensitive (request-context) context-contribution)
(defun context-tests--sensitive (context)
  "Return a contribution whose payload must not enter retained diagnostics."
  (declare (ignore context))
  (make-context-contribution
   :identifier "sensitive"
   :instruction *context-test-sensitive-instruction*
   :evidence *context-test-sensitive-evidence*
   :priority 42
   :class ':mandatory))

(-> context-tests--defining-form () null)
(defun context-tests--defining-form ()
  "Test durable contributor definition installation and exact undo."
  (let* ((name 'context-tests--defined)
         (source
           "(define-context-contributor context-tests--defined (context) (declare (ignore context)) (make-context-contribution :identifier \"defined-advice\" :instruction \"Defined advice.\"))")
         (definition (self-read-form source))
         (identifier (context--definition-identifier name)))
    (when (fboundp name)
      (fmakunbound name))
    (unregister-context-contributor identifier)
    (let ((undo (self--definition-undo-action
                 definition nil (find-package '#:autolith))))
      (unwind-protect
           (progn
             (test-assert (definition-form-p definition)
                          "self.redefine accepts context contributor definitions")
             (self--install-definition definition source)
             (test-assert
              (and (fboundp name)
                   (context--registration-find identifier))
              "installing a contributor definition registers its function"))
        (funcall undo)
        (remhash (definition-key definition) *exploratory-definitions*)))
    (test-assert
     (and (not (fboundp name))
          (null (context--registration-find identifier)))
     "discard restores both contributor function and registration state"))
  nil)

(-> context-tests--serialized-invocation
    (configuration conversation)
    null)
(defun context-tests--serialized-invocation (configuration conversation)
  "Test that a concurrent request cannot invoke contributors through the lock."
  (let* ((*context-contributors* nil)
         (*context-resolver* (cl-llm-provider-api:make-context-resolver))
         (*context-last-deliveries* (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil)
         (state (list 0))
         (*context-test-invocation-state* state)
         (ready-lock (make-lock "Autolith context test ready"))
         (ready-condition (make-condition-variable))
         (ready-p nil)
         (thread nil)
         (thread-error nil))
    (register-context-contributor "serialized" 'context-tests--serialized)
    (let ((registrations *context-contributors*)
          (receipts *context-resolver*)
          (deliveries *context-last-deliveries*)
          (delivery-order *context-last-delivery-order*))
      (with-lock-held (*context-contributor-invocation-lock*)
        (setf thread
              (make-thread
               (lambda ()
                 (let ((*context-contributors* registrations)
                       (*context-resolver* receipts)
                       (*context-last-deliveries* deliveries)
                       (*context-last-delivery-order* delivery-order)
                       (*context-test-invocation-state* state))
                   (with-lock-held (ready-lock)
                     (setf ready-p t)
                     (condition-notify ready-condition))
                   (handler-case
                       (context-resolve-request configuration conversation #())
                     (error (condition)
                       (setf thread-error condition)))))
               :name "Autolith context serialization test"))
        (with-lock-held (ready-lock)
          (loop until ready-p
                do (condition-wait ready-condition ready-lock)))
        (test-assert (zerop (first state))
                     "contributor invocation waits for its serialization lock"))
      (join-thread thread)
      (when thread-error
        (error thread-error))
      (test-assert (= (first state) 1)
                   "the waiting request invokes its contributor exactly once")))
  nil)

(-> test-session-state-context-contributor () null)
(defun test-session-state-context-contributor ()
  "Test the protected built-in mutable session-state contribution."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "session-state-context")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect session state")
           (let* ((*request-context-hurry-up-p* t)
                  (delivery
                    (context-resolve-request configuration conversation #()))
                  (contribution
                    (find "session-state"
                          (context-delivery-contributions delivery)
                          :test #'string=
                          :key #'context-contribution-identifier)))
             (test-assert
              (and contribution
                   (search "HURRY-UP MODE IS ACTIVE"
                           (context-contribution-instruction contribution)))
              "the built-in contributor delivers current mutable session state"))
           (let* ((*system-prompt-override* "compact frame prompt")
                  (delivery
                    (context-resolve-request configuration conversation #())))
             (test-assert
              (not (find "session-state"
                         (context-delivery-contributions delivery)
                         :test #'string=
                         :key #'context-contribution-identifier))
              "a compact prompt override suppresses full session-state context"))
           (test-assert
            (handler-case
                (progn
                  (register-context-contributor
                   "session-state" 'context-tests--sensitive
                   :source ':built-in)
                  nil)
              (configuration-error ()
                t))
            "another function cannot replace built-in context")
           (test-assert
            (not (unregister-context-contributor "session-state"))
            "built-in context cannot be unregistered"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-request-local-context () null)
(defun test-request-local-context ()
  "Test contributor registration, product diagnostics, and request projection."
  (context-tests--defining-form)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration
                                            :identifier "context-test"))
         (*context-contributors* nil)
         (*context-resolver* (cl-llm-provider-api:make-context-resolver))
         (*context-last-deliveries* (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil)
         (*context-test-next-request-p* t))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect this request")
           (setf *context-contributors* nil)
           (register-context-contributor "failure" 'context-tests--failure)
           (let ((delivery
                   (context-resolve-request configuration conversation #())))
             (test-assert (string= (first (first
                                           (context-delivery-failures delivery)))
                                   "failure")
                          "contributor failures degrade to diagnostics")
             (test-assert (search "broken contributor" (context-status))
                          "/context diagnostics expose contributor failures"))
           (setf *context-contributors* nil)
           (register-context-contributor
            "sensitive"
            'context-tests--sensitive
            :source ':built-in)
           (let* ((delivery
                    (context-resolve-request configuration conversation #()))
                  (retained
                    (gethash
                     (conversation-identifier conversation)
                     *context-last-deliveries*))
                  (diagnostic
                    (first
                     (context-delivery-diagnostic-contributions retained)))
                  (status (context-status conversation)))
             (test-assert
              (and
               (search *context-test-sensitive-instruction*
                       (context-delivery-rendered delivery))
               (search *context-test-sensitive-evidence*
                       (context-delivery-rendered delivery)))
              "the provider delivery retains complete request-local payloads")
             (test-assert
              (and
               (typep retained 'context-delivery-diagnostic)
               (typep diagnostic 'context-contribution-diagnostic)
               (string=
                (context-contribution-diagnostic-identifier diagnostic)
                "sensitive")
               (= (context-contribution-diagnostic-instruction-character-count
                   diagnostic)
                  (length *context-test-sensitive-instruction*))
               (= (context-contribution-diagnostic-evidence-character-count
                   diagnostic)
                  (length *context-test-sensitive-evidence*)))
              "retained context diagnostics preserve bounded observability metadata")
             (test-assert
              (and
               (not
                (test-object-contains-string-p
                 *context-last-deliveries*
                 *context-test-sensitive-instruction*))
               (not
                (test-object-contains-string-p
                 *context-last-deliveries*
                 *context-test-sensitive-evidence*))
               (not (search *context-test-sensitive-instruction* status))
               (not (search *context-test-sensitive-evidence* status)))
              "retained diagnostics and /context discard request-local payload text")
             (test-assert
              (and (search "sensitive" status)
                   (search "mandatory" status)
                   (search "instruction character" status)
                   (search "evidence character" status))
              "/context renders useful payload-free contribution metadata"))
           (setf *context-contributors* nil)
           (register-context-contributor
            "conversation-advice"
            'context-tests--conversation-advice)
           (let ((other-conversation
                   (conversation-create configuration
                                        :identifier "context-diagnostic-other")))
             (context-resolve-request configuration conversation #())
             (context-resolve-request configuration other-conversation #())
             (let ((current-status (context-status conversation))
                   (other-status (context-status other-conversation))
                   (latest-status (context-status)))
               (test-assert
                (and (search "conversation context-test" current-status)
                     (search "conversation-advice" current-status)
                     (not
                      (search "conversation context-diagnostic-other"
                              current-status)))
                "/context selects diagnostics for the current conversation")
               (test-assert
                (and
                 (search "conversation context-diagnostic-other" other-status)
                 (search "conversation-advice" other-status))
                "diagnostics retain another conversation's newest delivery")
               (test-assert
                (search "conversation context-diagnostic-other" latest-status)
                "context-status without a selection retains newest-first behavior")))
           (clrhash *context-last-deliveries*)
           (setf *context-last-delivery-order* nil)
           (loop for index below (+ *context-delivery-diagnostic-limit* 2)
                 for identifier = (format nil "context-diagnostic-~2,'0D" index)
                 for diagnostic-conversation =
                   (conversation-create configuration :identifier identifier)
                 do (context-resolve-request configuration
                                             diagnostic-conversation
                                             #()))
           (test-assert
            (= (hash-table-count *context-last-deliveries*)
               *context-delivery-diagnostic-limit*)
            "per-conversation context diagnostics stay bounded")
           (test-assert
            (= (length *context-last-delivery-order*)
               *context-delivery-diagnostic-limit*)
            "the context diagnostic recency index stays bounded")
           (test-assert
            (and (null (gethash "context-diagnostic-00"
                                *context-last-deliveries*))
                 (gethash "context-diagnostic-33"
                          *context-last-deliveries*))
            "context diagnostics evict the oldest conversation first")
           (setf *context-contributors* nil)
           (register-context-contributor "next" 'context-tests--next-request)
           (let* ((provider (provider-create configuration))
                  (before (copy-list
                           (conversation-input-items conversation)))
                  (request (provider-request-object provider conversation #()))
                  (input (json-get request "input")))
             (test-assert
              (and (= (length input) (1+ (length before)))
                   (string= (json-get (aref input (1- (length input))) "role")
                            "developer"))
              "request-local context appends one developer input")
             (test-assert
              (search "Temporary context"
                      (context--message-text
                       (aref input (1- (length input)))))
              "request-local context follows durable provider input")
             (test-assert
              (equal before (conversation-input-items conversation))
              "context request assembly never mutates durable conversation input"))
           (let ((delivery (context-resolve-request configuration conversation #())))
             (test-assert (context-delivery-contributions delivery)
                          "request projection alone does not consume delivery state")
             (context-delivery-complete delivery)
             (test-assert
              (null (context-delivery-contributions
                     (context-resolve-request configuration conversation #())))
              "product completion forwards successful delivery to the generic resolver"))
           (let ((request
                   (make-instance 'request-context
                                  :configuration configuration
                                  :conversation conversation
                                  :tool-namespaces #())))
             (test-assert
              (string= (request-context-latest-user-text request)
                       "inspect this request")
              "contributors receive the latest durable user text"))
           (context-tests--serialized-invocation configuration conversation))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-known-context-estimate () null)
(defun test-known-context-estimate ()
  "Account for known prompt, session state, history, and schemas before usage arrives."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration))
          (registry (make-instance 'tool-registry))
          (prompt (make-string 64 :initial-element #\p))
          (session-state "")
          (schemas (vector (json-object "description"
                                        (make-string 800 :initial-element #\s)))))
      (test-call-with-function-replacements
       (list (list 'system-prompt
                   (lambda (configuration)
                     (declare (ignore configuration))
                     prompt))
              (list 'request-context-session-state
                    (lambda (configuration &key hurry-up-p)
                      (declare (ignore configuration hurry-up-p))
                      session-state))
             (list 'tool-registry-provider-schemas
                   (lambda (registry &key canonical-names)
                     (declare (ignore registry canonical-names))
                     schemas)))
       (lambda ()
         (let ((base (context-estimate-known-tokens configuration conversation)))
           (test-assert (plusp base) "a known system prompt seeds a nonzero estimate")
           (setf session-state (make-string 400 :initial-element #\s))
           (let ((with-state (context-estimate-known-tokens configuration conversation)))
             (test-assert (> with-state base) "mutable session state contributes to the estimate")
             (conversation-append-user-message
              conversation (make-string 400 :initial-element #\u))
             (let ((with-history (context-estimate-known-tokens configuration conversation)))
               (test-assert (> with-history with-state)
                            "retained conversation input contributes to the estimate")
               (test-assert (> (context-estimate-known-tokens
                               configuration conversation :tool-registry registry)
                              with-history)
                            "known tool schemas contribute to the estimate")
               (conversation-append-summary conversation "short handoff")
               (test-assert (< (context-estimate-known-tokens configuration conversation)
                               with-history)
                            "compaction estimates the replacement input rather than old history"))))))))
  nil)


(-> test-context-estimate-cache () null)
(defun test-context-estimate-cache ()
  "Cache known-input counts across progress repaints and invalidate at state boundaries."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (registry (make-instance 'tool-registry))
           (ui (terminal-ui-create
                :terminal (make-instance 'recording-terminal :columns 100)))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider nil :tool-registry registry :ui ui))
           (prompt-calls 0)
           (session-calls 0)
           (schema-calls 0)
           (session-hurry-up-p nil))
      (test-call-with-function-replacements
       (list
        (list 'system-prompt
              (lambda (configuration)
                (declare (ignore configuration))
                (incf prompt-calls)
                (make-string 64 :initial-element #\p)))
        (list 'request-context-session-state
              (lambda (configuration &key hurry-up-p)
                (declare (ignore configuration))
                (incf session-calls)
                (setf session-hurry-up-p hurry-up-p)
                (if hurry-up-p (make-string 400 :initial-element #\h) "")))
        (list 'tool-registry-provider-schemas
              (lambda (registry &key canonical-names)
                (declare (ignore registry canonical-names))
                (incf schema-calls)
                #())))
       (lambda ()
         (application-set-activity application "starting")
         (dotimes (index 4)
           (application-set-activity application (format nil "progress ~D" index)))
         (test-assert (equal (list prompt-calls session-calls schema-calls) '(1 1 1))
                      "unchanged progress never rebuilds known request inputs")
         (let ((before (terminal-ui-context-used ui)))
           (setf (config :hurry-up-p configuration) t)
           (application-set-activity application "hurry")
           (test-assert (and session-hurry-up-p
                             (> (terminal-ui-context-used ui) before))
                        "the estimate uses the application's actual hurry-up state"))
         (conversation-append-user-message conversation "new request input")
         (application-set-activity application "new input")
         (test-assert (= prompt-calls 3)
                      "conversation changes invalidate known-input counts")
         (setf (application-tool-registry application) (make-instance 'tool-registry))
         (application-set-activity application "new registry")
         (test-assert (= schema-calls 4) "registry replacement refreshes schema counts")
         (setf (config :relax-p configuration) t)
         (application-set-activity application "new prompt setting")
         (test-assert (= prompt-calls 5) "in-place prompt settings invalidate the estimate")
         (setf (application-context-estimate-cache application) nil)
         (application-set-activity application "request boundary")
         (test-assert (= session-calls 6) "a new request samples mutable session inputs")
         (setf (conversation-last-total-tokens conversation) 1234)
         (application-set-activity application "reported usage")
         (test-assert (and (= (terminal-ui-context-used ui) 1234)
                           (not (terminal-ui-context-estimated-p ui))
                           (= prompt-calls 6))
                      "positive provider totals are authoritative without estimate builders")
         (setf (conversation-last-total-tokens conversation) 0)
         (let ((before (application-context-usage application))
               (sequence (conversation-next-sequence conversation)))
           (conversation--append-ephemeral-input-item
            conversation
            (json-object "role" "user"
                         "content" (json-array
                                    (json-object "type" "input_text"
                                                 "text" (make-string 800 :initial-element #\e)))))
           (application-set-activity application "ephemeral input")
           (test-assert (and (= sequence (conversation-next-sequence conversation))
                             (> (terminal-ui-context-used ui) before)
                             (= prompt-calls 7))
                        "ephemeral input invalidates without requiring a durable append")
           (conversation-clear-ephemeral-input-items conversation)
           (application-set-activity application "ephemeral input removed")
           (test-assert (and (= (terminal-ui-context-used ui) before)
                             (= prompt-calls 8))
                        "clearing ephemeral input refreshes the estimate"))
         (dotimes (index 2)
           (tool-registry-register
            (application-tool-registry application)
            (make-instance 'tool :namespace "test" :name "estimate"
                                 :description "Known estimate tool."
                                 :parameters (json-object "type" "object")))
           (application-set-activity application "tool replacement")
           (test-assert (= schema-calls (+ 9 index))
                        "in-place registration and same-name replacement refresh schema counts"))))))
  nil)

(-> test-context-estimate-provider-family () null)
(defun test-context-estimate-provider-family ()
  "Estimate only the checkpoint or portable handoff usable by the active provider."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider (make-instance 'native-scripted-provider)))
           (checkpoint (json-object "type" "context_compaction"
                                    "encrypted_content"
                                    (make-string 4000 :initial-element #\c))))
      (conversation-append-native-compaction
       conversation checkpoint :family ':codex :summary "Portable handoff.")
      (test-call-with-function-replacements
       (list (list 'system-prompt
                   (lambda (configuration)
                     (declare (ignore configuration))
                     ""))
             (list 'request-context-session-state
                   (lambda (configuration &key hurry-up-p)
                     (declare (ignore configuration hurry-up-p))
                     "")))
       (lambda ()
         (setf (config :model configuration) "grok-4.5")
         (let ((codex (application-context-usage application)))
           (test-assert
            (= codex (ceiling
                      (context--known-value-character-count
                       (conversation-input-items-for-family conversation ':codex))
                      4))
            "the producing family counts its opaque checkpoint without the portable handoff")
           (setf (application-provider application) nil)
           (let ((grok (application-context-usage application)))
             (test-assert
              (= grok (ceiling
                       (context--known-value-character-count
                        (conversation-input-items-for-family conversation ':grok))
                       4))
              "a family switch counts only compatible portable history")
             (test-assert (< grok codex)
                          "foreign encrypted checkpoint contents do not inflate the estimate")))))))
  nil)
