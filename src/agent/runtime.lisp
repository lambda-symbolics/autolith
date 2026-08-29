(in-package #:autolith)

;;;; -- Agent Events --

(defclass agent-observer ()
  ()
  (:documentation "A presentation sink for incremental agent output and lifecycle status."))

(defclass callback-agent-observer (agent-observer)
  ((text-callback
    :initarg :text-callback
    :initform nil
    :reader callback-agent-observer-text-callback
    :type (option function)
    :documentation "The optional function called with each assistant text delta.")
   (reasoning-callback
    :initarg :reasoning-callback
    :initform nil
    :reader callback-agent-observer-reasoning-callback
    :type (option function)
    :documentation "The optional function called with each visible reasoning delta.")
   (status-callback
    :initarg :status-callback
    :initform nil
    :reader callback-agent-observer-status-callback
    :type (option function)
    :documentation "The optional function called with a status keyword and portable details.")
   (steering-callback
    :initarg :steering-callback
    :initform nil
    :reader callback-agent-observer-steering-callback
    :type (option function)
    :documentation "The optional function that drains user messages waiting for a tool boundary.")
   (steering-persisted-callback
    :initarg :steering-persisted-callback
    :initform nil
    :reader callback-agent-observer-steering-persisted-callback
    :type (option function)
    :documentation
    "The optional function acknowledging one identified durable steering message.")
   (pending-operations-callback
    :initarg :pending-operations-callback
    :initform nil
    :reader callback-agent-observer-pending-operations-callback
    :type (option function)
    :documentation
    "The optional function applying queued user operations at a safe boundary.")
   (command-authorization-callback
    :initarg :command-authorization-callback
    :initform nil
    :reader callback-agent-observer-command-authorization-callback
    :type (option function)
    :documentation "The optional function authorizing one external command.")
   (tool-authorization-callback
    :initarg :tool-authorization-callback
    :initform nil
    :reader callback-agent-observer-tool-authorization-callback
    :type (option function)
    :documentation "The optional function authorizing one external tool call."))
  (:documentation "An agent observer implemented by ordinary terminal-facing callbacks."))

(defclass serialized-agent-observer (agent-observer)
  ((delegate
    :initarg :delegate
    :reader serialized-agent-observer-delegate
    :type agent-observer
    :documentation "The observer receiving callbacks under the serialization lock.")
   (lock
    :initform (make-recursive-lock "Autolith agent observer callbacks")
    :reader serialized-agent-observer-lock
    :documentation "The recursive lock serializing callbacks from tool workers."))
  (:documentation
   "An observer wrapper serializing callbacks made by concurrent tool workers."))

(defclass agent-steering-input ()
  ((identifier
    :initarg :identifier
    :reader agent-steering-input-identifier
    :type non-empty-string
    :documentation "The durable pending-input identifier for this steering message.")
   (content
    :initarg :content
    :reader agent-steering-input-content
    :type (or string user-message-input)
    :documentation "The user message to append at the next tool boundary."))
  (:documentation "One identified steering message awaiting durable conversation append."))

(defclass agent ()
  ((configuration
    :initarg :configuration
    :accessor agent-configuration
    :type configuration
    :documentation "The paths and model choices governing this agent.")
   (provider
    :initarg :provider
    :accessor agent-provider
    :type model-provider
    :documentation "The replaceable streaming model provider.")
   (conversation
    :initarg :conversation
    :reader agent-conversation
    :type conversation
    :documentation "The durable conversation owned by this agent.")
   (tool-registry
    :initarg :tool-registry
    :accessor agent-tool-registry
    :type tool-registry
    :documentation "The namespaced tool schemas and dispatch table.")
   (worker
    :initarg :worker
    :reader agent-worker
    :type t
    :documentation "The disposable Lisp worker supplied to lisp.* calls.")
   (session-id
    :initarg :session-id
    :initform nil
    :accessor agent-session-id
    :type (option string)
    :documentation "The localgroup session identity associated with this agent.")
   (hurry-up-p
    :initarg :hurry-up-p
    :initform nil
    :accessor agent-hurry-up-p
    :type boolean
    :documentation "Whether provider requests use the urgent execution profile.")
   (turn-lock
    :initform (make-lock "Autolith agent turn")
    :reader agent-turn-lock
    :documentation "The lock preventing concurrent mutation of conversation turn state."))
  (:documentation "A model-driven conversation loop with namespaced Common Lisp tools."))

(defparameter *agent-restricted-maximum-tool-rounds* 4
  "Maximum executed tool rounds within one restricted agent turn.")

(defparameter *agent-maximum-provider-requests-per-turn* 512
  "Maximum provider requests issued within one ordinary agent turn.")

(defparameter *agent-maximum-concurrent-tool-workers* 8
  "Maximum worker threads executing independent calls from one provider batch.")


;;;; -- Agent Conditions --

(define-condition agent-loop-error (autolith-error)
  ((conversation-id
    :initarg :conversation-id
    :reader agent-loop-error-conversation-id
    :type (option string)
    :documentation "The conversation whose turn could not continue.")
   (request-number
    :initarg :request-number
    :reader agent-loop-error-request-number
    :type (option integer)
    :documentation "The provider request number within the user turn, if known."))
  (:documentation "A malformed response or invariant violation in the main agent loop."))


;;;; -- Observer Protocol --

(-> agent-observer-text (agent-observer string) null)
(defgeneric agent-observer-text (observer text)
  (:documentation "Present one incremental assistant TEXT fragment through OBSERVER."))

(-> agent-observer-reasoning (agent-observer string) null)
(defgeneric agent-observer-reasoning (observer text)
  (:documentation "Present one visible reasoning TEXT fragment through OBSERVER."))

(-> agent-observer-status (agent-observer keyword list) null)
(defgeneric agent-observer-status (observer status details)
  (:documentation "Present STATUS and portable DETAILS through OBSERVER."))

(-> agent-observer-take-steering (agent-observer) list)
(defgeneric agent-observer-take-steering (observer)
  (:documentation
   "Return and consume user messages waiting at OBSERVER's next tool boundary."))

(-> agent-observer-steering-persisted
    (agent-observer non-empty-string)
    null)
(defgeneric agent-observer-steering-persisted (observer identifier)
  (:documentation
   "Acknowledge that steering input IDENTIFIER is durable through OBSERVER."))

(-> agent-observer-apply-pending-operations (agent-observer t) null)
(defgeneric agent-observer-apply-pending-operations (observer agent)
  (:documentation
   "Apply user operations queued for AGENT's next safe provider boundary."))

(-> agent-observer-authorize-command
    (agent-observer string pathname)
    keyword)
(defgeneric agent-observer-authorize-command (observer command directory)
  (:documentation "Return the execution permission for COMMAND in DIRECTORY."))

(-> agent-observer-authorize-tool
    (agent-observer tool json-object)
    keyword)
(defgeneric agent-observer-authorize-tool (observer tool arguments)
  (:documentation "Return :ALLOW or :DENY for external TOOL and ARGUMENTS."))

(defmethod agent-observer-text ((observer agent-observer) (text string))
  "Ignore assistant TEXT for the default silent OBSERVER."
  (declare (ignore observer text))
  nil)

(defmethod agent-observer-reasoning ((observer agent-observer) (text string))
  "Ignore reasoning TEXT for the default silent OBSERVER."
  (declare (ignore observer text))
  nil)

(defmethod agent-observer-status
    ((observer agent-observer) status details)
  "Ignore STATUS and DETAILS for the default silent OBSERVER."
  (declare (type keyword status)
           (type list details))
  (declare (ignore observer status details))
  nil)

(defmethod agent-observer-take-steering ((observer agent-observer))
  "Return no steering messages for the default silent OBSERVER."
  (declare (ignore observer))
  nil)

(defmethod agent-observer-steering-persisted
    ((observer agent-observer) (identifier string))
  "Ignore durable steering IDENTIFIER for the default silent OBSERVER."
  (declare (ignore observer identifier))
  nil)

(defmethod agent-observer-apply-pending-operations
    ((observer agent-observer) agent)
  "Apply no queued operations for the default silent OBSERVER."
  (declare (ignore observer agent))
  nil)

(defmethod agent-observer-authorize-command
    ((observer agent-observer) (command string) (directory pathname))
  "Deny COMMAND when OBSERVER has no authorization interface."
  (declare (ignore observer command directory))
  ':deny)

(defmethod agent-observer-authorize-tool
    ((observer agent-observer) (tool tool) (arguments hash-table))
  "Deny TOOL when OBSERVER has no authorization interface."
  (declare (ignore observer tool arguments))
  ':deny)

(defmethod agent-observer-text ((observer callback-agent-observer) (text string))
  "Send assistant TEXT to OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-text-callback observer)))
    (when callback
      (funcall callback text)))
  nil)

(defmethod agent-observer-reasoning
    ((observer callback-agent-observer) (text string))
  "Send reasoning TEXT to OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-reasoning-callback observer)))
    (when callback
      (funcall callback text)))
  nil)

(defmethod agent-observer-status
    ((observer callback-agent-observer) status details)
  "Send STATUS and DETAILS to OBSERVER's configured callback."
  (declare (type keyword status)
           (type list details))
  (let ((callback (callback-agent-observer-status-callback observer)))
    (when callback
      (funcall callback status details)))
  nil)

(defmethod agent-observer-take-steering ((observer callback-agent-observer))
  "Drain steering messages through OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-steering-callback observer)))
    (if callback
        (funcall callback)
        nil)))

(defmethod agent-observer-steering-persisted
    ((observer callback-agent-observer) (identifier string))
  "Acknowledge durable steering IDENTIFIER through OBSERVER's callback."
  (let ((callback
          (callback-agent-observer-steering-persisted-callback observer)))
    (when callback
      (funcall callback identifier)))
  nil)

(defmethod agent-observer-apply-pending-operations
    ((observer callback-agent-observer) agent)
  "Apply queued operations through OBSERVER's configured callback."
  (let ((callback
          (callback-agent-observer-pending-operations-callback observer)))
    (when callback
      (funcall callback agent)))
  nil)

(defmethod agent-observer-authorize-command
    ((observer callback-agent-observer) (command string) (directory pathname))
  "Authorize COMMAND through OBSERVER's callback, denying when absent."
  (let ((callback
          (callback-agent-observer-command-authorization-callback observer)))
    (if callback
        (funcall callback command directory)
        ':deny)))

(defmethod agent-observer-authorize-tool
    ((observer callback-agent-observer)
     (tool tool)
     (arguments hash-table))
  "Authorize TOOL through OBSERVER's callback, denying when absent."
  (let ((callback
          (callback-agent-observer-tool-authorization-callback observer)))
    (if callback
        (funcall callback tool arguments)
        ':deny)))

(defmethod agent-observer-text
    ((observer serialized-agent-observer) (text string))
  "Forward assistant TEXT to OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-text (serialized-agent-observer-delegate observer) text)))

(defmethod agent-observer-reasoning
    ((observer serialized-agent-observer) (text string))
  "Forward reasoning TEXT to OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-reasoning
     (serialized-agent-observer-delegate observer) text)))

(defmethod agent-observer-status
    ((observer serialized-agent-observer) status details)
  "Forward STATUS and DETAILS to OBSERVER's delegate under its callback lock."
  (declare (type keyword status)
           (type list details))
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-status
     (serialized-agent-observer-delegate observer) status details)))

(defmethod agent-observer-take-steering
    ((observer serialized-agent-observer))
  "Drain steering from OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-take-steering
     (serialized-agent-observer-delegate observer))))

(defmethod agent-observer-steering-persisted
    ((observer serialized-agent-observer) (identifier string))
  "Forward durable steering IDENTIFIER under OBSERVER's callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-steering-persisted
     (serialized-agent-observer-delegate observer) identifier)))

(defmethod agent-observer-apply-pending-operations
    ((observer serialized-agent-observer) agent)
  "Apply queued operations through OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-apply-pending-operations
     (serialized-agent-observer-delegate observer) agent)))

(defmethod agent-observer-authorize-command
    ((observer serialized-agent-observer)
     (command string)
     (directory pathname))
  "Authorize COMMAND through OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-authorize-command
     (serialized-agent-observer-delegate observer) command directory)))

(defmethod agent-observer-authorize-tool
    ((observer serialized-agent-observer)
     (tool tool)
     (arguments hash-table))
  "Authorize TOOL through OBSERVER's delegate under its callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-authorize-tool
     (serialized-agent-observer-delegate observer) tool arguments)))


;;;; -- Construction and Turn Entry --

(-> agent-steering-input-create
    (&key (:identifier non-empty-string)
          (:content (or string user-message-input)))
    agent-steering-input)
(defun agent-steering-input-create (&key identifier content)
  "Create one identified steering message carrying CONTENT."
  (make-instance 'agent-steering-input
                 :identifier identifier
                 :content content))

(-> callback-agent-observer-create
    (&key
     (:text-callback (option function))
     (:reasoning-callback (option function))
     (:status-callback (option function))
     (:steering-callback (option function))
     (:steering-persisted-callback (option function))
     (:pending-operations-callback (option function))
     (:command-authorization-callback (option function))
     (:tool-authorization-callback (option function)))
    callback-agent-observer)
(defun callback-agent-observer-create
    (&key text-callback reasoning-callback status-callback steering-callback
      steering-persisted-callback pending-operations-callback
      command-authorization-callback tool-authorization-callback)
  "Create an observer backed by optional presentation callbacks."
  (make-instance 'callback-agent-observer
                 :text-callback text-callback
                 :reasoning-callback reasoning-callback
                 :status-callback status-callback
                 :steering-callback steering-callback
                 :steering-persisted-callback steering-persisted-callback
                 :pending-operations-callback pending-operations-callback
                 :command-authorization-callback
                 command-authorization-callback
                 :tool-authorization-callback tool-authorization-callback))

(-> agent-create
    (&key
     (:configuration configuration)
     (:provider (option model-provider))
     (:conversation (option conversation))
     (:tool-registry (option tool-registry))
     (:worker t)
     (:session-id (option string)))
    agent)
(defun agent-create
    (&key
       configuration
       provider
       conversation
       tool-registry
       worker
       session-id)
  "Create an agent, filling unspecified provider, conversation, registry, and worker roles."
  (unless (typep configuration 'configuration)
    (error 'configuration-error
           :message "AGENT-CREATE requires a CONFIGURATION instance."))
  (make-instance 'agent
                 :configuration configuration
                 :provider (or provider (provider-create configuration))
                 :conversation (or conversation
                                   (conversation-create configuration))
                 :tool-registry (or tool-registry
                                     (make-default-tool-registry))
                 :worker (or worker (lisp-worker-pool-create configuration))
                 :session-id session-id))

(-> agent-run-user-turn
    (agent (or string user-message-input)
     &key (:observer agent-observer)
          (:goal-context (option string))
          (:tools-p boolean)
          (:tool-allowlist (option list))
          (:tool-restriction-p boolean)
          (:pending-input-identifier (option non-empty-string))
          (:automatic-p boolean))
    provider-result)
(defgeneric agent-run-user-turn
    (agent content
     &key observer goal-context tools-p tool-allowlist tool-restriction-p
          pending-input-identifier automatic-p)
  (:documentation
   "Persist user CONTENT, run model and optional tool rounds, and return the final provider result."))

(-> agent-turn-complete-p (agent provider-result) boolean)
(defgeneric agent-turn-complete-p (agent result)
  (:documentation
   "Return true when RESULT completes AGENT's active user turn."))

(defmethod agent-turn-complete-p ((agent agent) (result provider-result))
  "Return true when RESULT needs neither tool execution nor a provider follow-up."
  (declare (ignore agent))
  (and (null (provider-result-tool-calls result))
       (not (eq (provider-result-turn-completion result) ':continue))))

(-> agent-turn-completion-details (agent) list)
(defgeneric agent-turn-completion-details (agent)
  (:documentation "Return AGENT-specific portable turn-completion details."))

(defmethod agent-turn-completion-details ((agent agent))
  "Return no extra completion details for an ordinary AGENT."
  (declare (ignore agent))
  nil)


(defmethod agent-run-user-turn
    ((agent agent) (content string)
     &key (observer (make-instance 'agent-observer)) goal-context (tools-p t)
          tool-allowlist (tool-restriction-p nil) pending-input-identifier
          automatic-p)
  "Normalize a textual user turn before running it through AGENT."
  (agent-run-user-turn agent
                       (user-message-input-create :text content)
                       :observer observer
                       :goal-context goal-context
                       :tools-p tools-p
                       :tool-allowlist tool-allowlist
                       :tool-restriction-p tool-restriction-p
                       :pending-input-identifier pending-input-identifier
                       :automatic-p automatic-p))

(defmethod agent-run-user-turn
    ((agent agent) (content user-message-input)
     &key (observer (make-instance 'agent-observer)) goal-context (tools-p t)
          tool-allowlist (tool-restriction-p nil) pending-input-identifier
          automatic-p)
  "Run one serialized user turn through AGENT while presenting events to OBSERVER."
  (unless (or (non-empty-string-p (user-message-input-text content))
              (user-message-input-image-pathnames content))
    (error 'agent-loop-error
           :message "A user turn requires text or an image."
           :conversation-id (conversation-identifier (agent-conversation agent))
           :request-number nil))
  (with-observed-agent-turn
      (agent content :automatic-p automatic-p)
    (call-with-skill-logical-turn
     content
     (lambda ()
       (with-lock-held ((agent-turn-lock agent))
         (let ((conversation (agent-conversation agent)))
           ;; Compact before appending CONTENT so the fresh question survives
           ;; verbatim instead of being folded into the summary.
           (when (agent-should-compact-p agent)
             (agent-compact-conversation
              agent observer
              :tool-allowlist tool-allowlist
              :tool-restriction-p tool-restriction-p))
           (multiple-value-bind (item record)
               (conversation-append-user-message
                conversation
                content
                :pending-input-identifier pending-input-identifier
                :automatic-p automatic-p)
             (declare (ignore item))
             (agent-observer-status
              observer
              :user-message-persisted
              (append
               (list :sequence (getf (rest record) :seq)
                     :time (getf (rest record) :time))
               (when pending-input-identifier
                 (list :pending-input-identifier pending-input-identifier)))))
           (unwind-protect
                (agent--run-provider-loop
                 agent observer
                 :goal-context goal-context
                 :tools-p tools-p
                 :tool-allowlist tool-allowlist
                 :tool-restriction-p tool-restriction-p)
             (conversation-clear-ephemeral-input-items conversation)
             (setf (conversation-turn-state conversation) nil))))))))


;;;; -- Provider and Persistence Flow --

(-> agent--portable-value (t) t)
(defun agent--portable-value (value)
  "Convert provider VALUE into portable readable conversation metadata."
  (cond
    ((hash-table-p value)
     (sort
      (loop for key being the hash-keys of value
              using (hash-value child)
            collect (list key (agent--portable-value child)))
      #'string<
      :key #'first))
    ((vectorp value)
     (loop for child across value
           collect (agent--portable-value child)))
    ((listp value)
     (mapcar #'agent--portable-value value))
    (t
     value)))

(-> agent--provider-event-callback (agent-observer) function)
(defun agent--provider-event-callback (observer)
  "Return a provider callback that forwards streaming presentation events to OBSERVER."
  (lambda (event)
    (typecase event
      (provider-retry-event
       (let ((attempt (provider-retry-event-attempt event))
             (maximum-attempts (provider-retry-event-maximum-attempts event))
             (delay (provider-retry-event-delay event)))
         (agent-observer-status
          observer
          :provider-retrying
          (list :attempt attempt
                :maximum-attempts maximum-attempts
                :delay delay))
         (observability-mark-provider-retry
          attempt maximum-attempts delay)))
      (assistant-delta-event
       (agent-observer-status observer :provider-progress nil)
       (agent-observer-text observer (assistant-delta-event-text event)))
      (reasoning-delta-event
       (agent-observer-status observer :provider-progress nil)
       (agent-observer-reasoning observer (reasoning-delta-event-text event)))
      (provider-event
       (agent-observer-status observer :provider-progress nil))
      (t
       nil))))

(-> agent--persist-provider-result
    (agent provider-result
     &key (:request-number integer)
          (:call-plans list))
    null)
(defun agent--persist-provider-result
    (agent result &key request-number call-plans)
  "Append RESULT items in wire order under their per-tool persistence policies."
  (let ((conversation (agent-conversation agent)))
    (dolist (item (provider-result-output-items result))
      (unless (json-object-p item)
        (error 'agent-loop-error
               :message "The provider returned a completed item that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number request-number))
      (let ((plan
              (and
               (function-call-item-p item)
               (find (json-get item "call_id")
                     call-plans
                     :key (lambda (entry)
                            (json-get (getf entry :call) "call_id"))
                     :test #'equal))))
        (when (and plan (getf plan :argument-error))
          (setf (gethash "arguments" item) "{}"))
        (conversation-append-provider-item
         conversation
         item
         :persistence
         (if plan
             (getf plan :persistence)
             ':durable))))
    (conversation-append-provider-metadata
     conversation
     (list :request-number request-number
           :response-id (provider-result-response-id result)
           :usage (agent--portable-value
                   (provider-usage-normalize
                    (provider-result-usage result))))))
  nil)

(-> agent--note-persisted-assistant-response
    (agent-observer provider-result integer)
    null)
(defun agent--note-persisted-assistant-response (observer result request-number)
  "Report RESULT's durable verbal assistant text through OBSERVER, when present."
  (let ((text (provider-result-assistant-text result)))
    (when (and (non-empty-string-p text)
               (plusp
                (length
                 (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
      (agent-observer-status
       observer
       ':assistant-response-persisted
       (list :request-number request-number
             :response-id (provider-result-response-id result)
             :text text
             :time (get-universal-time)))))
  nil)

(-> agent--sanitize-tool-call-arguments (json-object) (option string))
(defun agent--sanitize-tool-call-arguments (call)
  "Make CALL replayable and return a failure message for malformed arguments."
  (unless (json-object-source-p (json-get call "arguments"))
    (setf (gethash "arguments" call) "{}")
    (format nil
            "Tool ~A was not executed because the provider returned arguments that were not a valid JSON object."
            (function-call-canonical-name call))))

(-> agent--tool-call-plans (agent list) list)
(defun agent--tool-call-plans (agent calls)
  "Return CALLS annotated with tools, persistence, and round-trip barriers."
  (let ((barrier-seen-p nil)
        (plans nil)
        (registry (agent-tool-registry agent)))
    (dolist (call calls)
      (let* ((namespace (json-get call "namespace"))
             (name      (json-get call "name"))
             (argument-error
               (agent--sanitize-tool-call-arguments call))
             (tool
               (and (non-empty-string-p namespace)
                    (non-empty-string-p name)
                    (tool-registry-find registry namespace name)))
             (blocked-p barrier-seen-p)
             (persistence
               (if blocked-p
                   ':next-response
                   (if tool
                       (tool-conversation-persistence tool)
                       ':durable))))
        (push (list :call call
                    :tool tool
                    :persistence persistence
                    :blocked-p blocked-p
                    :argument-error argument-error)
              plans)
        (when (and tool
                   (tool-provider-round-trip-barrier-p tool))
          (setf barrier-seen-p t))))
    (nreverse plans)))

(-> agent--provider-error-metadata (provider-error) list)
(defun agent--provider-error-metadata (condition)
  "Return portable terminal failure metadata for provider CONDITION."
  (list :message (bounded-string (format nil "~A" condition) :limit 2000)
        :status (provider-error-status condition)
        :code (provider-error-code condition)
        :incomplete-reason
        (and (typep condition 'provider-incomplete-response)
             (provider-incomplete-response-reason condition))
        :request-id (provider-error-request-id condition)
        :response-id (provider-error-response-id condition)
        :response (provider-error-response condition)
        :retryable-p (typep condition 'provider-retryable-error)))

(-> agent--validate-tool-call-identifiers
    (agent list
     &key (:seen-call-identifiers hash-table) (:request-number integer))
    null)
(defun agent--validate-tool-call-identifiers
    (agent calls &key seen-call-identifiers request-number)
  "Validate CALLS and reserve their unique call identifiers for this user turn."
  (let ((round-identifiers (make-hash-table :test #'equal))
        (conversation (agent-conversation agent)))
    (dolist (call calls)
      (unless (json-object-p call)
        (error 'agent-loop-error
               :message "The provider returned a tool call that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number request-number))
      (let ((call-id (json-get call "call_id")))
        (unless (non-empty-string-p call-id)
          (error 'agent-loop-error
                 :message "The provider returned a function call without a call_id."
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (when (or (gethash call-id seen-call-identifiers)
                  (gethash call-id round-identifiers))
          (error 'agent-loop-error
                 :message (format nil "The provider repeated function call identifier ~S."
                                  call-id)
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (setf (gethash call-id round-identifiers) t)))
    (maphash (lambda (call-id present-p)
               (declare (ignore present-p))
               (setf (gethash call-id seen-call-identifiers) t))
             round-identifiers))
  nil)

(-> agent--tool-call-allowed-p (json-object list) boolean)
(defun agent--tool-call-allowed-p (call allowlist)
  "Return true when CALL's canonical name appears in ALLOWLIST."
  (not
   (null
    (member (function-call-canonical-name call)
            allowlist
            :test #'string=))))

(-> agent--validate-tool-call-allowlist
    (agent list list &key (:request-number integer))
    null)
(defun agent--validate-tool-call-allowlist
    (agent calls allowlist &key request-number)
  "Reject CALLS outside ALLOWLIST before persistence or execution."
  (dolist (call calls)
    (unless (and (json-object-p call)
                 (agent--tool-call-allowed-p call allowlist))
      (error 'agent-loop-error
             :message
             (format nil
                     "Tool ~A is unavailable during this restricted turn."
                     (if (json-object-p call)
                         (function-call-canonical-name call)
                         "unknown"))
             :conversation-id
             (conversation-identifier (agent-conversation agent))
             :request-number request-number)))
  nil)

(-> agent--reject-tool-call-plans
    (agent list agent-observer
     &key (:tool-round integer) (:message string))
    null)
(defun agent--reject-tool-call-plans
    (agent plans observer &key tool-round message)
  "Append one explicit MESSAGE failure output for every rejected call in PLANS."
  (dolist (plan plans)
    (let* ((call      (getf plan :call))
           (call-id   (json-get call "call_id"))
           (tool-name (function-call-canonical-name call)))
      (conversation-append-tool-result
       (agent-conversation agent)
       call-id
       :tool-name tool-name
       :output message
       :success-p nil
       :persistence (getf plan :persistence))
      (agent-observer-status
       observer
       ':tool-call-completed
       (list :tool-round tool-round
             :call-id call-id
             :tool tool-name
             :success-p nil
             :output message))))
  nil)

(-> agent--execute-tool-plan
    (agent list agent-observer boolean boolean)
    list)
(defun agent--execute-tool-plan
    (agent plan observer tool-restriction-p record-timings-p)
  "Execute one PLAN body and return its result, timings, or fatal condition."
  (let* ((call      (getf plan :call))
         (call-id   (json-get call "call_id"))
         (argument-error (getf plan :argument-error))
         (context
           (make-instance
            'tool-context
            :configuration (agent-configuration agent)
            :worker (agent-worker agent)
            :conversation (agent-conversation agent)
            :registry (agent-tool-registry agent)
            :agent agent
            :observer observer
            :call-id call-id
            :command-authorization-function
            (lambda (command directory)
              (observability-mark
               :tool-authorization-requested
               :kind "command")
              (let ((decision
                      (agent-observer-authorize-command
                       observer command directory)))
                (when (eq decision ':deny)
                  (observability-mark
                   :tool-authorization-denied
                   :kind "command"))
                decision))
            :tool-authorization-function
            (lambda (tool arguments)
              (observability-mark
               :tool-authorization-requested
               :kind "tool"
               :tool tool)
              (let ((decision
                      (agent-observer-authorize-tool observer tool arguments)))
                (when (eq decision ':deny)
                  (observability-mark
                   :tool-authorization-denied
                   :kind "tool"
                   :tool tool))
                decision))))
         (*workspace-tool-readable-roots*
           (and tool-restriction-p
                (list
                 (configuration-working-directory
                  (agent-configuration agent))
                 (configuration-source-root
                  (agent-configuration agent)))))
         (*resource-readable-schemes*
           (and tool-restriction-p '("workspace")))
         (real-start (get-internal-real-time))
         (cpu-start (get-internal-run-time))
         (result nil)
         (condition nil))
    (handler-case
        (setf result
              (with-observed-tool-call (call)
                (if argument-error
                    (tool-failure argument-error :code ':invalid-arguments)
                    (tool-registry-execute-call
                     (agent-tool-registry agent)
                     call
                     context))))
      (serious-condition (failure)
        (setf condition failure)))
    (list
     :plan plan
     :result result
     :condition condition
     :cpu-microseconds
     (and record-timings-p
          (round (* (- (get-internal-run-time) cpu-start) 1000000)
                 internal-time-units-per-second))
     :real-microseconds
     (and record-timings-p
          (round (* (- (get-internal-real-time) real-start) 1000000)
                 internal-time-units-per-second)))))

(-> agent--complete-tool-executions
    (agent list agent-observer integer)
    null)
(defun agent--complete-tool-executions
    (agent executions observer tool-round)
  "Persist EXECUTIONS and report completions in provider wire order."
  (dolist (execution executions)
    (let* ((plan      (getf execution :plan))
           (call      (getf plan :call))
           (call-id   (json-get call "call_id"))
           (tool-name (function-call-canonical-name call))
           (result    (getf execution :result)))
      (when result
        (conversation-append-tool-result
         (agent-conversation agent)
         call-id
         :tool-name tool-name
         :output (tool-result-content result)
         :content-blocks (tool-result-content-blocks result)
         :success-p (tool-result-success-p result)
         :cpu-microseconds (getf execution :cpu-microseconds)
         :real-microseconds (getf execution :real-microseconds)
         :persistence (getf plan :persistence))
        (agent-observer-status
         observer
         :tool-call-completed
         (list :tool-round tool-round
               :call-id call-id
               :tool tool-name
               :success-p (tool-result-success-p result)
               :cpu-microseconds (getf execution :cpu-microseconds)
               :real-microseconds (getf execution :real-microseconds)
               :output (tool-result-content result)
               :details (tool-result-details result))))))
  (let ((failure
          (find-if (lambda (execution)
                     (getf execution :condition))
                   executions)))
    (when failure
      (error (getf failure :condition))))
  nil)

(-> agent--run-tool-wave
    (agent list agent-observer integer boolean)
    null)
(defun agent--run-tool-wave
    (agent plans observer tool-round tool-restriction-p)
  "Execute independent PLANS concurrently and complete them in wire order."
  (dolist (plan plans)
    (let ((call (getf plan :call)))
      (agent-observer-status
       observer
       :tool-call-started
       (list :tool-round tool-round
             :call-id (json-get call "call_id")
             :tool (function-call-canonical-name call)))))
  (let* ((count (length plans))
         (observability-context (capture-observability-context))
         (executions (make-array count))
         (record-timings-p (= count 1)))
    (if (= count 1)
        (setf (aref executions 0)
              (with-observability-context (observability-context)
                (agent--execute-tool-plan
                 agent (first plans) observer tool-restriction-p t)))
        (let ((next-index 0)
              (claim-lock (make-lock "Autolith tool wave claims"))
              (threads nil)
              (thread-creation-condition nil))
          (flet ((work ()
                   (loop
                     (let ((index
                             (with-lock-held (claim-lock)
                               (when (< next-index count)
                                 (prog1 next-index
                                   (incf next-index))))))
                       (unless index
                         (return))
                       (setf (aref executions index)
                             (with-observability-context (observability-context)
                               (agent--execute-tool-plan
                                agent
                                (nth index plans)
                                observer
                                tool-restriction-p
                                record-timings-p)))))))
            (unwind-protect
                 (progn
                   (loop repeat
                           (min count *agent-maximum-concurrent-tool-workers*)
                         while (null thread-creation-condition)
                         do
                           (handler-case
                               (push
                                (make-thread #'work :name "autolith-tool-call")
                                threads)
                             (serious-condition (condition)
                               (setf thread-creation-condition condition))))
                   (when thread-creation-condition
                     (work))
                   (mapc #'join-thread threads))
              (mapc (lambda (thread)
                      (when (thread-alive-p thread)
                        (ignore-errors (join-thread thread))))
                    threads))
            (when thread-creation-condition
              (agent--complete-tool-executions
               agent (coerce executions 'list) observer tool-round)
              (error thread-creation-condition)))))
    (agent--complete-tool-executions
     agent (coerce executions 'list) observer tool-round))
  nil)

(-> agent--execute-tool-calls
    (agent list provider-result
     &key (:observer agent-observer) (:tool-round integer)
          (:tool-allowlist (option list)) (:tool-restriction-p boolean))
    null)
(defun agent--execute-tool-calls
    (agent call-plans provider-result
     &key observer tool-round tool-allowlist (tool-restriction-p nil))
  "Execute planned calls in independent waves while respecting barriers."
  (let ((serialized-observer
          (make-instance 'serialized-agent-observer :delegate observer))
        (wave nil)
        (wave-keys (make-hash-table :test #'eql)))
    (labels ((flush-wave ()
               (when wave
                 (agent--run-tool-wave
                  agent
                  (nreverse wave)
                  serialized-observer
                  tool-round
                  tool-restriction-p)
                 (setf wave nil)
                 (clrhash wave-keys))))
      (loop for remaining on call-plans
            for plan = (first remaining)
            for call = (getf plan :call)
            for tool = (getf plan :tool)
            for barrier-p = (and tool
                                 (tool-provider-round-trip-barrier-p tool))
            for exclusive-p = (or barrier-p
                                  (and tool
                                       (eq (tool-execution-policy tool)
                                           ':exclusive)))
            for key = (and tool (tool-concurrency-key tool))
            do
              (when (getf plan :blocked-p)
                (flush-wave)
                (agent--reject-tool-call-plans
                 agent
                 remaining
                 serialized-observer
                 :tool-round tool-round
                 :message
                 "This call was not executed because a preceding tool requires a provider round trip. Retry it after inspecting that tool's result and the refreshed instructions.")
                (return))
              (when (and tool-restriction-p
                         (not (agent--tool-call-allowed-p call tool-allowlist)))
                (error 'agent-loop-error
                       :message
                       (format nil
                               "Tool ~A is unavailable during this restricted turn."
                               (function-call-canonical-name call))
                       :conversation-id
                       (conversation-identifier (agent-conversation agent))
                       :request-number nil))
              (when (or exclusive-p
                        (and key (gethash key wave-keys)))
                (flush-wave))
              (push plan wave)
              (when key
                (setf (gethash key wave-keys) t))
              (when exclusive-p
                (flush-wave))
              (when (agent-turn-complete-p agent provider-result)
                (flush-wave)
                (when (rest remaining)
                  (agent--reject-tool-call-plans
                   agent
                   (rest remaining)
                   serialized-observer
                   :tool-round tool-round
                   :message
                   "This call was not executed because the agent turn already completed."))
                (return))
            finally (flush-wave))))
  nil)

(-> agent--apply-steering-input (agent agent-observer integer) null)
(defun agent--apply-steering-input (agent observer request-number)
  "Apply queued operations and persist steering at a safe provider boundary.

Queued user operations run first so a replaced provider, configuration, or
tool registry reaches the very next provider request."
  (agent-observer-apply-pending-operations observer agent)
  (let ((messages (agent-observer-take-steering observer))
        (conversation (agent-conversation agent)))
    (unless (listp messages)
      (error 'agent-loop-error
             :message "The agent observer returned malformed steering input."
             :conversation-id (conversation-identifier conversation)
             :request-number request-number))
    (dolist (entry messages)
      (let* ((message
               (etypecase entry
                 (agent-steering-input (agent-steering-input-content entry))
                 ((or string user-message-input) entry)))
             (identifier
               (and (typep entry 'agent-steering-input)
                    (agent-steering-input-identifier entry))))
        (unless (or (and (stringp message) (non-empty-string-p message))
                    (and (typep message 'user-message-input)
                         (or (non-empty-string-p
                              (user-message-input-text message))
                             (user-message-input-image-pathnames message))))
          (error 'agent-loop-error
                 :message "The agent observer returned an empty steering message."
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (conversation-append-user-message
         conversation
         message
         :pending-input-identifier identifier)
        (when identifier
          (agent-observer-steering-persisted observer identifier))
        (skill-record-steering-input message)))
    (when messages
      (agent-observer-status
       observer
       :steering-applied
       (list :message-count (length messages)))))
  nil)

(-> agent-should-compact-p (agent) boolean)
(defgeneric agent-should-compact-p (agent)
  (:documentation
   "Return true when AGENT's conversation should compact before continuing."))

(defmethod agent-should-compact-p ((agent agent))
  "Return true when the newest usage crossed AGENT's compaction limit."
  (>= (conversation-last-total-tokens (agent-conversation agent))
      (configuration-compaction-token-limit (agent-configuration agent))))

(-> agent-compact-conversation
    (agent agent-observer &key (:tool-allowlist (option list))
                               (:tool-restriction-p boolean))
    null)
(defun agent-compact-conversation
    (agent observer &key tool-allowlist (tool-restriction-p nil))
  "Compact AGENT's conversation with native state when the provider supports it.

A supported native checkpoint becomes the input to the portable summarization
side channel, avoiding a second upload of the full pre-compaction history. The
durable summary remains a handoff for another provider family."
  (let ((conversation (agent-conversation agent))
        (*request-context-hurry-up-p* (agent-hurry-up-p agent)))
    (agent-observer-status
     observer
     :compaction-started
     (list :total-tokens (conversation-last-total-tokens conversation)))
    (let* ((*provider-hosted-tools-enabled-p* (not tool-restriction-p))
           (provider (agent-provider agent))
           (native-item
             (provider-native-compact-conversation
              provider
              conversation
              :tool-namespaces
              (if tool-restriction-p
                  (tool-registry-provider-schemas
                   (agent-tool-registry agent)
                   :canonical-names tool-allowlist)
                  (tool-registry-provider-schemas
                   (agent-tool-registry agent)))
              :event-callback
              (lambda (event)
                (declare (ignore event))
                (agent-observer-status observer :provider-progress nil))))
            (summary-conversation
              (if native-item
                  (conversation-native-compaction-summary-view
                   conversation native-item (provider-family provider))
                  conversation))
            (result (provider-stream-turn
                     provider
                     summary-conversation
                     :tool-namespaces #()
                     :event-callback
                     (lambda (event)
                       (declare (ignore event))
                       (agent-observer-status observer :provider-progress nil))
                     :compaction-p t))
           (summary (provider-result-assistant-text result)))
      (unless (non-empty-string-p summary)
        (error 'agent-loop-error
               :message "Compaction produced no summary text."
               :conversation-id (conversation-identifier conversation)
               :request-number nil))
      (if native-item
          (conversation-append-native-compaction
           conversation native-item
           :family (provider-family provider)
           :summary summary)
          (conversation-append-summary conversation summary))
      (agent-observer-status
       observer
       :compaction-completed
       (list :summary-characters (length summary)
             :native-p (not (null native-item))))
      (observability-mark
       :compaction-completed
       :summary-characters (length summary)
       :native-p (not (null native-item))))
  nil))

(-> agent--run-provider-loop
    (agent agent-observer &key (:goal-context (option string))
                          (:tools-p boolean)
                          (:tool-allowlist (option list))
                          (:tool-restriction-p boolean))
    provider-result)
(defun agent--run-provider-loop
    (agent observer
     &key goal-context (tools-p t) tool-allowlist (tool-restriction-p nil))
  "Run provider and optional tool rounds until AGENT's turn completes."
  (let ((seen-call-identifiers (make-hash-table :test #'equal))
        (request-number 0)
        (tool-rounds 0)
        (tool-calls 0))
    (loop
      (when (>= request-number *agent-maximum-provider-requests-per-turn*)
        (error 'agent-loop-error
               :message
               (format nil
                       "The turn reached its ~D provider-request safety limit."
                       *agent-maximum-provider-requests-per-turn*)
               :conversation-id
               (conversation-identifier (agent-conversation agent))
               :request-number request-number))
      (when (agent-should-compact-p agent)
        (agent-compact-conversation
         agent observer
         :tool-allowlist tool-allowlist
         :tool-restriction-p tool-restriction-p))
      (when (and tools-p (not tool-restriction-p))
        (mcp-tool-registry-refresh
         (agent-tool-registry agent)
         :only-dirty-p t))
      (incf request-number)
      (agent-observer-status
       observer
       :provider-request-started
       (list :request-number request-number
             :tool-rounds tool-rounds))
      (let* ((conversation (agent-conversation agent))
             (tool-schemas-p
               (and tools-p
                    (or (not tool-restriction-p)
                        (< tool-rounds
                           *agent-restricted-maximum-tool-rounds*))))
             (result
               (handler-case
                   (let ((*provider-hosted-tools-enabled-p*
                            (not tool-restriction-p))
                          (*request-context-hurry-up-p*
                            (agent-hurry-up-p agent))
                          (*context-request-contributions*
                           (and
                            tools-p
                            (not tool-restriction-p)
                            (mcp-tool-registry-context-contributions
                             (agent-tool-registry agent)))))
                     (provider-stream-turn
                      (agent-provider agent)
                      conversation
                      :tool-namespaces
                      (if tool-schemas-p
                          (if tool-restriction-p
                              (tool-registry-provider-schemas
                               (agent-tool-registry agent)
                               :canonical-names tool-allowlist)
                              (tool-registry-provider-schemas
                               (agent-tool-registry agent)))
                          #())
                      :event-callback (agent--provider-event-callback observer)
                      :goal-context goal-context))
                 (provider-error (condition)
                   (conversation-append-provider-metadata
                    conversation
                    (list :request-number request-number
                          :failure
                          (agent--provider-error-metadata condition)))
                   (error condition))))
             (calls (provider-result-tool-calls result)))
        (when (and calls (not tools-p))
          (error 'agent-loop-error
                 :message "A tool-free model turn returned function calls."
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (when (and calls tool-restriction-p (not tool-schemas-p))
          (error 'agent-loop-error
                 :message
                 (format nil
                         "A restricted turn exceeded its ~D tool-round limit."
                         *agent-restricted-maximum-tool-rounds*)
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (when (and calls tool-restriction-p)
          (agent--validate-tool-call-allowlist
           agent calls tool-allowlist :request-number request-number))
        (conversation-clear-ephemeral-input-items conversation)
        (agent--validate-tool-call-identifiers
         agent
         calls
         :seen-call-identifiers seen-call-identifiers
         :request-number request-number)
        (let ((call-plans (agent--tool-call-plans agent calls)))
          (agent--persist-provider-result
           agent
           result
           :request-number request-number
           :call-plans call-plans)
          (agent--note-persisted-assistant-response
           observer result request-number)
          (setf (conversation-turn-state conversation)
                (provider-result-turn-state result))
          (agent-observer-status
           observer
           :provider-request-completed
           (list :request-number request-number
                 :response-id (provider-result-response-id result)
                 :usage (agent--portable-value
                         (provider-usage-normalize
                          (provider-result-usage result)))
                 :output-item-count
                 (length (provider-result-output-items result))
                 :tool-call-count (length calls)
                 :turn-completion (provider-result-turn-completion result)))
          (when (agent-turn-complete-p agent result)
            (agent-observer-status
             observer
             :turn-completed
             (append
              (list :provider-requests request-number
                    :tool-rounds tool-rounds
                    :tool-calls tool-calls
                    :response-id (provider-result-response-id result))
              (agent-turn-completion-details agent)))
            (return result))
          (cond
            ((null calls)
             (agent-observer-status
              observer
              :provider-follow-up
              (list :request-number request-number))
             (agent--apply-steering-input
              agent observer request-number))
            (t
             (incf tool-rounds)
             (incf tool-calls (length calls))
             (agent--execute-tool-calls agent call-plans result
                                        :observer observer
                                        :tool-round tool-rounds
                                        :tool-allowlist tool-allowlist
                                        :tool-restriction-p tool-restriction-p)
             (if (agent-turn-complete-p agent result)
                 (progn
                   (agent-observer-status
                    observer
                    :turn-completed
                    (append
                     (list :provider-requests request-number
                           :tool-rounds tool-rounds
                           :tool-calls tool-calls
                           :response-id (provider-result-response-id result))
                     (agent-turn-completion-details agent)))
                   (return result))
                 (agent--apply-steering-input
                  agent observer request-number)))))))))
