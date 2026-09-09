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
