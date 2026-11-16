(in-package #:autolith)

;;;; -- Agent Events --

(defclass agent-observer ()
  ()
  (:documentation "A presentation sink for incremental agent output and lifecycle status."))

(defclass callback-agent-observer (agent-observer)
  ((text-callback
    :initarg       :text-callback
    :initform      nil
    :reader        callback-agent-observer-text-callback
    :type          (option function)
    :documentation "The optional function called with each assistant text delta.")
   (reasoning-callback
    :initarg       :reasoning-callback
    :initform      nil
    :reader        callback-agent-observer-reasoning-callback
    :type          (option function)
    :documentation "The optional function called with each visible reasoning delta.")
   (status-callback
    :initarg       :status-callback
    :initform      nil
    :reader        callback-agent-observer-status-callback
    :type          (option function)
    :documentation "The optional function called with a status keyword and portable details.")
   (steering-callback
    :initarg       :steering-callback
    :initform      nil
    :reader        callback-agent-observer-steering-callback
    :type          (option function)
    :documentation "The optional function that drains user messages waiting for a tool boundary.")
   (steering-persisted-callback
    :initarg  :steering-persisted-callback
    :initform nil
    :reader   callback-agent-observer-steering-persisted-callback
    :type     (option function)
    :documentation
    "The optional function acknowledging one identified durable steering message.")
   (steering-entry-callback
    :initarg       :steering-entry-callback
    :initform      nil
    :reader        callback-agent-observer-steering-entry-callback
    :type          (option function)
    :documentation "The optional function persisting one command entry in steering order.")
   (pending-operations-callback
    :initarg  :pending-operations-callback
    :initform nil
    :reader   callback-agent-observer-pending-operations-callback
    :type     (option function)
    :documentation
    "The optional function applying queued user operations at a safe boundary.")
   (command-authorization-callback
    :initarg       :command-authorization-callback
    :initform      nil
    :reader        callback-agent-observer-command-authorization-callback
    :type          (option function)
    :documentation "The optional function authorizing one external command.")
   (tool-authorization-callback
    :initarg       :tool-authorization-callback
    :initform      nil
    :reader        callback-agent-observer-tool-authorization-callback
    :type          (option function)
    :documentation "The optional function authorizing one external tool call."))
  (:documentation "An agent observer implemented by ordinary terminal-facing callbacks."))

(defclass serialized-agent-observer (agent-observer)
  ((delegate
    :initarg       :delegate
    :reader        serialized-agent-observer-delegate
    :type          agent-observer
    :documentation "The observer receiving callbacks under the serialization lock.")
   (lock
    :initform      (make-recursive-lock "Autolith agent observer callbacks")
    :reader        serialized-agent-observer-lock
    :documentation "The recursive lock serializing callbacks from tool workers."))
  (:documentation
   "An observer wrapper serializing callbacks made by concurrent tool workers."))

(defclass agent-steering-input ()
  ((identifier
    :initarg       :identifier
    :reader        agent-steering-input-identifier
    :type          non-empty-string
    :documentation "The durable pending-input identifier for this steering message.")
   (content
    :initarg       :content
    :reader        agent-steering-input-content
    :type          (or string user-message-input)
    :documentation "The user message to append at the next tool boundary.")
   (operation-request
    :initarg       :operation-request
    :initform      nil
    :reader        agent-steering-input-operation-request
    :type          (option list)
    :documentation "The operation that generated this message, such as a channel delivery, or NIL for the person's own input."))
  (:documentation "One identified steering message awaiting durable conversation append."))

(defclass participant-state ()
  ((identity :initarg :identity :initform nil :accessor participant-state-identity
             :documentation "The durable participant record, once registered.")
   (conversation :initarg :conversation :initform nil
                 :accessor      participant-state-conversation
                 :type          (option conversation)
                 :documentation "The actual live provider context backing, or NIL while stored.")
   (goal :initarg :goal :initform nil :accessor participant-state-goal :type list
         :documentation "The ordinary goal, independent of execution assignments."))
  (:documentation "The identity, live context and ordinary goal carried by one participant."))

(defclass agent ()
  ((configuration
    :initarg       :configuration
    :accessor      agent-configuration
    :type          configuration
    :documentation "The paths and model choices governing this agent.")
   (provider
    :initarg       :provider
    :accessor      agent-provider
    :type          model-provider
    :documentation "The replaceable streaming model provider.")
   (participant-state
    :initarg       :participant-state
    :initform      (make-instance 'participant-state)
    :accessor      agent-participant-state
    :type          participant-state
    :documentation "The participant this execution runs, shared with its host.")
   (tool-registry
    :initarg       :tool-registry
    :accessor      agent-tool-registry
    :type          tool-registry
    :documentation "The namespaced tool schemas and dispatch table.")
   (worker
    :initarg       :worker
    :reader        agent-worker
    :type          t
    :documentation "The disposable Lisp worker supplied to lisp.* calls.")
   (hurry-up-p
    :initarg       :hurry-up-p
    :initform      nil
    :accessor      agent-hurry-up-p
    :type          boolean
    :documentation "Whether provider requests use the urgent execution profile.")
   (turn-lock
    :initform      (make-lock "Autolith agent turn")
    :reader        agent-turn-lock
    :documentation "The lock preventing concurrent mutation of conversation turn state."))
  (:documentation "A model-driven conversation loop with namespaced Common Lisp tools."))

(defvar *mission-admitted-operation* nil
  "The admitted child task-job continued by this thread, or NIL for new work.
Every thread hop must carry the same identity; its loss refuses quarantined work.")

(defparameter *agent-restricted-maximum-tool-rounds* 4
  "Maximum executed tool rounds within one restricted agent turn.")

(defparameter *agent-maximum-provider-requests-per-turn* 512
  "Maximum provider requests issued within one ordinary agent turn.")

(defparameter *agent-compaction-maximum-generations* 4
  "Maximum model generations spent on one compaction checkpoint.")

(defparameter *agent-compaction-latest-request-characters* 4000
  "Maximum characters of the latest user message a compaction request quotes.")

(defparameter *agent-maximum-concurrent-tool-workers* 8
  "Maximum worker threads executing independent calls from one provider batch.")

(defparameter *agent-tool-storm-identical-call-limit* 16
  "Withhold this many matching non-read-only calls within one user turn.")

(defparameter *agent-tool-storm-signature-window-size* 8
  "Maximum recent non-read-only call signatures retained by the storm guard.")

(defstruct (agent-tool-storm-state
            (:constructor agent-tool-storm-state-create ()))
  "Per-user-turn state for tool storm and retry diagnosis.

Repetition counts cover the whole turn, while the signature window only
needs the few most recent calls to recognize an oscillation."
  (signatures nil :type list)
  (repetitions (make-hash-table :test #'equalp) :type hash-table)
  (consecutive-withholds 0 :type (integer 0))
  (failures (make-hash-table :test #'equalp) :type hash-table))


;;;; -- Agent Conditions --

(define-condition agent-loop-error (autolith-error)
  ((conversation-id
    :initarg       :conversation-id
    :reader        agent-loop-error-conversation-id
    :type          (option string)
    :documentation "The conversation whose turn could not continue.")
   (request-number
    :initarg       :request-number
    :reader        agent-loop-error-request-number
    :type          (option integer)
    :documentation "The provider request number within the user turn, if known."))
  (:documentation "A malformed response or invariant violation in the main agent loop."))


;;;; -- Observer Protocol --

(defgeneric agent-observer-text (observer text)
  (:documentation "Present one incremental assistant TEXT fragment through OBSERVER."))

(defgeneric agent-observer-reasoning (observer text)
  (:documentation "Present one visible reasoning TEXT fragment through OBSERVER."))

(defgeneric agent-observer-status (observer status details)
  (:documentation
   "Present STATUS and portable DETAILS, returning a positive request output ceiling or NIL."))

(defgeneric agent-observer-take-steering (observer)
  (:documentation
   "Return and consume user messages waiting at OBSERVER's next tool boundary."))

(defgeneric agent-observer-steering-persisted (observer identifier)
  (:documentation
   "Acknowledge that steering input IDENTIFIER is durable through OBSERVER."))

(defgeneric agent-observer-apply-steering-entry (observer agent entry)
  (:documentation
   "Persist command ENTRY for AGENT in steering order; return true when handled.

Return only after its model notification is durable and projected. Ordinary user
messages return NIL and use the agent's normal append path."))

(defmethod agent-observer-apply-steering-entry ((observer agent-observer) agent entry)
  "Leave ordinary steering entries to the agent's message append path."
  (declare (ignore observer agent entry))
  nil)

(defmethod agent-observer-apply-steering-entry
    ((observer callback-agent-observer) agent entry)
  "Persist command ENTRY through OBSERVER's optional per-entry callback."
  (let ((callback (callback-agent-observer-steering-entry-callback observer)))
    (and callback (not (null (funcall callback agent entry))))))

(defmethod agent-observer-apply-steering-entry
    ((observer serialized-agent-observer) agent entry)
  "Apply ENTRY through the delegate under OBSERVER's callback lock."
  (with-recursive-lock-held ((serialized-agent-observer-lock observer))
    (agent-observer-apply-steering-entry
     (serialized-agent-observer-delegate observer) agent entry)))

(defgeneric agent-observer-apply-pending-operations (observer agent)
  (:documentation
   "Apply user operations queued for AGENT's next safe provider boundary."))

(defgeneric agent-observer-authorize-command (observer command directory)
  (:documentation "Return the execution permission for COMMAND in DIRECTORY."))

(defgeneric agent-observer-authorize-tool (observer tool arguments)
  (:documentation "Return :ALLOW or :DENY for external TOOL and ARGUMENTS."))

(defgeneric agent-observer-call-with-tool-execution (observer identifier function)
  (:documentation "Call tool FUNCTION within OBSERVER's execution ownership and bindings."))

(defmethod agent-observer-call-with-tool-execution ((observer agent-observer) identifier function)
  "Run a native tool without extra presentation-specific execution state."
  (declare (ignore identifier))
  (funcall function))

(defmethod agent-observer-call-with-tool-execution
    ((observer serialized-agent-observer) identifier function)
  "Forward execution ownership without holding the callback serialization lock."
  (agent-observer-call-with-tool-execution
   (serialized-agent-observer-delegate observer) identifier function))

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
      (funcall callback status details))))

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
          (:content (or string user-message-input))
          (:operation-request (option list)))
    agent-steering-input)
(defun agent-steering-input-create (&key identifier content operation-request)
  "Create one identified steering message carrying CONTENT.

OPERATION-REQUEST names the operation that generated it; the message is then
recorded as automatic input that the transcript shows as that operation."
  (make-instance 'agent-steering-input
                 :identifier        identifier
                 :content           content
                 :operation-request operation-request))

(-> callback-agent-observer-create
    (&key
     (:text-callback (option function))
     (:reasoning-callback (option function))
     (:status-callback (option function))
     (:steering-callback (option function))
     (:steering-persisted-callback (option function))
     (:steering-entry-callback (option function))
     (:pending-operations-callback (option function))
     (:command-authorization-callback (option function))
     (:tool-authorization-callback (option function)))
    callback-agent-observer)
(defun callback-agent-observer-create
    (&key text-callback reasoning-callback status-callback steering-callback
      steering-persisted-callback steering-entry-callback pending-operations-callback
      command-authorization-callback tool-authorization-callback)
  "Create an observer backed by optional presentation callbacks."
  (make-instance 'callback-agent-observer
                 :text-callback                  text-callback
                 :reasoning-callback             reasoning-callback
                 :status-callback                status-callback
                 :steering-callback              steering-callback
                 :steering-persisted-callback    steering-persisted-callback
                 :steering-entry-callback        steering-entry-callback
                 :pending-operations-callback    pending-operations-callback
                 :command-authorization-callback command-authorization-callback
                 :tool-authorization-callback    tool-authorization-callback))

(-> agent-create
    (&key (:configuration configuration) (:provider (option model-provider))
          (:conversation (option conversation)) (:tool-registry (option tool-registry))
          (:hurry-up-p boolean) (:worker t) (:participant-state (option participant-state))
          (:class symbol) (:initargs list))
    agent)
(defun agent-create (&key configuration provider conversation tool-registry
                          (hurry-up-p nil) worker participant-state (class 'agent)
                          initargs)
  "Create an execution of CLASS for PARTICIPANT-STATE, filling missing resources.
INITARGS supplies subclass assignment metadata, never participant state."
  (unless (typep configuration 'configuration)
    (error 'configuration-error :message "AGENT-CREATE requires a CONFIGURATION instance."))
  (apply #'make-instance class
         :configuration     configuration
         :provider          (or provider (provider-create configuration))
         :participant-state (or participant-state
                                (make-instance 'participant-state
                                               :conversation (or conversation
                                                                 (conversation-create configuration))))
         :conversation      conversation
         :tool-registry     (or tool-registry
                                (make-default-tool-registry :configuration configuration))
         :hurry-up-p        hurry-up-p
         :worker            (or worker (lisp-worker-pool-create configuration))
         initargs))

(defgeneric agent-run-user-turn
    (agent content
     &key observer goal-context tools-p tool-allowlist tool-restriction-p
          pending-input-identifier automatic-p operation-request)
  (:documentation
   "Persist user CONTENT, run model and optional tool rounds, and return the final provider result.

OPERATION-REQUEST marks automatic CONTENT generated by a user operation with
the portable metadata its transcript entry shows in place of the text."))

(defgeneric agent-turn-complete-p (agent result)
  (:documentation
   "Return true when RESULT completes AGENT's active user turn."))

(defmethod agent-turn-complete-p ((agent agent) (result provider-result))
  "Return true when RESULT needs neither tool execution nor a provider follow-up.

A client-executed tool search is answered locally and needs the follow-up
request that carries its expansion."
  (declare (ignore agent))
  (and (null (provider-result-tool-calls result))
       (null (provider-result-tool-search-calls result))
       (not (eq (provider-result-turn-completion result) ':continue))))

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
          automatic-p operation-request)
  "Normalize a textual user turn before running it through AGENT."
  (agent-run-user-turn agent
                       (user-message-input-create :text content)
                       :observer                 observer
                       :goal-context             goal-context
                       :tools-p                  tools-p
                       :tool-allowlist           tool-allowlist
                       :tool-restriction-p       tool-restriction-p
                       :pending-input-identifier pending-input-identifier
                       :automatic-p              automatic-p
                       :operation-request        operation-request))

(defmethod agent-run-user-turn
    ((agent agent) (content user-message-input)
     &key (observer (make-instance 'agent-observer)) goal-context (tools-p t)
          tool-allowlist (tool-restriction-p nil) pending-input-identifier
          automatic-p operation-request)
  "Run one serialized user turn through AGENT while presenting events to OBSERVER."
  (unless (or (non-empty-string-p (user-message-input-text content))
              (user-message-input-image-pathnames content))
    (error 'agent-loop-error
           :message         "A user turn requires text or an image."
           :conversation-id (conversation-identifier (agent-conversation agent))
           :request-number  nil))
  (call-with-skill-logical-turn
   content
   (lambda ()
     (with-lock-held ((agent-turn-lock agent))
       (let ((conversation (agent-conversation agent)))
          (context-rule-start-turn agent)
         (conversation-flush-async-lisp-events conversation)
           ;; The first admission check runs after the fresh question is appended.
         (let ((compaction-requests 0))
          (when (conversation-async-lisp--safe-boundary-p conversation)
            (task-completion-deliver agent))
          (multiple-value-bind (item record)
              (conversation-append-user-message
               conversation
               content
               :pending-input-identifier pending-input-identifier
               :automatic-p              automatic-p
               :operation-request        operation-request)
            (declare (ignore item))
            (agent-observer-status
             observer
             :user-message-persisted
             (append
              (list :sequence (getf (rest record) :seq)
                    :time     (getf (rest record) :time))
              (when pending-input-identifier
                (list :pending-input-identifier pending-input-identifier)))))
          (unwind-protect
               (progn
                 (agent-run-fitting-procedure agent observer
                                              (or (user-message-input-text content) "")
                                              :automatic-p        automatic-p
                                              :tools-p            tools-p
                                              :tool-restriction-p tool-restriction-p)
                 (agent--run-provider-loop
                  agent observer
                  :goal-context       goal-context
                  :tools-p            tools-p
                  :tool-allowlist     tool-allowlist
                  :tool-restriction-p tool-restriction-p
                  :request-number     compaction-requests))
            (conversation-clear-ephemeral-input-items conversation))))))))


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
    ((stringp value)
     value)
    ((vectorp value)
     (loop for child across value
           collect (agent--portable-value child)))
    ((listp value)
     (mapcar #'agent--portable-value value))
    (t
     value)))

(-> agent--provider-event-callback
    (agent-observer &key (:conversation (option conversation))
                         (:request-number (option integer)))
    function)
(defun agent--provider-event-callback
    (observer &key conversation request-number)
  "Return a provider callback that forwards streaming presentation events to OBSERVER.

Failed attempts are reported as :PROVIDER-ATTEMPT-FAILED and, when
CONVERSATION is given, persisted as provider metadata under REQUEST-NUMBER so
retries stay auditable next to the request they belong to."
  (lambda (event)
    (typecase event
      (memory-context-event
       (agent-observer-status
        observer :related-memories
        (list :count      (memory-context-event-count event)
              :stale      (memory-context-event-stale-count event)
              :unverified (memory-context-event-unverified-count event)
              :facts      (memory-context-event-fact-count event)
              :ids        (copy-list (memory-context-event-identifiers event)))))
      (knowledge-change-event
       (agent-observer-status
        observer :knowledge-changes
        (list :count (knowledge-change-event-count event))))
      (plan-change-event
       (agent-observer-status
        observer :plan-changes
        (list :count (plan-change-event-count event))))
      (procedure-offer-event
       (agent-observer-status
        observer :procedure-offers
        (list :names     (procedure-offer-event-names event)
              :related-p (procedure-offer-event-related-p event))))
      (provider-retries-exhausted-event
       (agent-observer-status
        observer :provider-retries-exhausted
        (list :attempts (provider-attempt-failed-event-attempt event)
              :code     (provider-error-code
                         (provider-attempt-failed-event-condition event)))))
      (provider-attempt-failed-event
       (let ((details
               (list :request-number request-number
                     :attempt        (provider-attempt-failed-event-attempt event)
                     :elapsed-seconds
                     (provider-attempt-failed-event-elapsed-seconds event)
                     :output-received-p
                     (provider-attempt-failed-event-output-received-p event)
                     :retryable-p
                     (provider-attempt-failed-event-retryable-p event)
                     :failure
                     (agent--provider-error-metadata
                      (provider-attempt-failed-event-condition event)))))
         (when conversation
           (conversation-append-provider-metadata conversation details))
         (agent-observer-status observer :provider-attempt-failed details)))
      (provider-retry-event
       (agent-observer-status
        observer
        :provider-retrying
        (list :attempt (provider-retry-event-attempt event)
              :maximum-attempts
              (provider-retry-event-maximum-attempts event)
              :delay   (provider-retry-event-delay event))))
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
               :message         "The provider returned a completed item that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number  request-number))
      (let ((plan
              (and
               (function-call-item-p item)
               (find (json-get item "call_id")
                     call-plans
                     :key  (lambda (entry)
                             (json-get (getf entry :call) "call_id"))
                     :test #'equal))))
        (when (and plan (getf plan :argument-error))
          (setf (gethash "arguments" item) "{}"))
        (handler-case
            (conversation-append-provider-item
             conversation
             item
             :tool-label (when (getf plan :tool)
                           (tool-display-name (getf plan :tool)))
             :persistence
             (if plan
                 (getf plan :persistence)
                 ':durable))
          (provider-call-conflict (condition)
            (error 'agent-loop-error
                   :message         (princ-to-string condition)
                   :conversation-id (conversation-identifier conversation)
                   :request-number  request-number)))))
    (conversation-append-provider-metadata
     conversation
     (append (list :request-number         request-number
                   :context-usage-pending t
                   :response-id            (provider-result-response-id result)
                   :usage                  (agent--portable-value
                                            (provider-usage-normalize
                                             (provider-result-usage result)))
                   :request-identity       (conversation-request-identity conversation))
             ;; The service's effective retention mode, when it reports one.
             (let ((context (agent--result-reasoning-context result)))
               (and context (list :reasoning-context context))))))
  nil)

(-> agent--answer-tool-searches
    (agent provider-result vector &key (:request-number integer))
    null)
(defun agent--answer-tool-searches (agent result tool-namespaces &key request-number)
  "Answer RESULT's client tool searches from the TOOL-NAMESPACES this request offered.

Each answer is appended as a durable provider item, so the expansion replays in
every later request and the server keeps those tools loaded without another
search. The search covers exactly the namespaces the request advertised, so a
restricted turn cannot discover tools outside its allowlist."
  (let ((conversation (agent-conversation agent))
        (provider     (agent-provider agent)))
    (dolist (call (provider-result-tool-search-calls result))
      (unless (non-empty-string-p (json-get call "call_id"))
        (error 'agent-loop-error
               :message         "The provider returned a tool search without a call_id."
               :conversation-id (conversation-identifier conversation)
               :request-number  request-number))
      (conversation-append-provider-item
       conversation
       (provider-answer-tool-search provider call tool-namespaces))))
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
             :response-id    (provider-result-response-id result)
             :text           text
             :time           (get-universal-time)))))
  nil)

(-> agent--sanitize-tool-call-arguments (json-object) (option string))
(defun agent--sanitize-tool-call-arguments (call)
  "Make CALL replayable and return a mechanics message for malformed arguments."
  (unless (json-object-source-p (json-get call "arguments"))
    (setf (gethash "arguments" call) "{}")
    (format nil
            "Tool ~A was not executed because the provider returned arguments that were not a valid JSON object."
            (function-call-canonical-name call))))

(-> agent--tool-signature-value (t) t)
(defun agent--tool-signature-value (value)
  "Return VALUE in a deterministic tree suitable for EQUALP call comparison."
  (cond
    ((json-object-p value)
     (sort
      (loop for key being the hash-keys of value
            using (hash-value child)
            collect (cons key (agent--tool-signature-value child)))
      #'string<
      :key #'first))
    ((vectorp value)
     (map 'list #'agent--tool-signature-value value))
    (t
     value)))

(-> agent--tool-call-signature (json-object) (option list))
(defun agent--tool-call-signature (call)
  "Return CALL's canonical tool and argument signature, or NIL when malformed."
  (handler-case
      (let ((arguments (json-decode (json-get call "arguments"))))
        (when (json-object-p arguments)
          (list (function-call-canonical-name call)
                (agent--tool-signature-value arguments))))
    (error ()
      nil)))

(-> agent--tool-storm-note-call
    (agent-tool-storm-state list string)
    (option string))
(defun agent--tool-storm-note-call (state signature tool-name)
  "Record SIGNATURE in STATE and return a mechanics message when it storms."
  (let* ((history        (agent-tool-storm-state-signatures state))
         (repetitions    (agent-tool-storm-state-repetitions state))
         (identical-p
           (>= (gethash signature repetitions 0)
               (1- *agent-tool-storm-identical-call-limit*)))
         (history-length (length history))
         (oscillation-p
           (and (>= history-length 3)
                (let ((a (nth (- history-length 3) history))
                      (b (nth (- history-length 2) history))
                      (c (nth (1- history-length) history)))
                  (and (equalp a c)
                       (equalp signature b)
                       (not (equalp a b))))))
         (reason
           (cond
             (identical-p
              (format nil
                      "Autolith withheld repeated call ~A after ~D matching calls this turn. Reassess the approach before choosing another mutating action."
                      tool-name
                      (1- *agent-tool-storm-identical-call-limit*)))
             (oscillation-p
              (format nil
                      "Autolith withheld call ~A because the recent mutating calls formed an A-B-A-B oscillation. Reassess the approach before retrying."
                      tool-name)))))
    (incf (gethash signature repetitions 0))
    (setf history (append history (list signature)))
    (when (> (length history) *agent-tool-storm-signature-window-size*)
      (setf history
            (subseq history
                    (- (length history)
                       *agent-tool-storm-signature-window-size*))))
    (setf (agent-tool-storm-state-signatures state) history
          (agent-tool-storm-state-consecutive-withholds state)
          (if reason
              (1+ (agent-tool-storm-state-consecutive-withholds state))
              0))
    (when (and reason
               (> (agent-tool-storm-state-consecutive-withholds state) 1))
      (setf reason
            (format nil "~A This is the ~:R consecutive withheld call."
                    reason
                    (agent-tool-storm-state-consecutive-withholds state))))
    reason))

(-> agent--tool-call-plans
    (agent list &key (:storm-state (option agent-tool-storm-state)))
    list)
(defun agent--tool-call-plans (agent calls &key storm-state)
  "Return CALLS annotated with tools, persistence, barriers, and storm guards."
  (let ((barrier-seen-p nil)
        (plans          nil)
        (registry       (agent-tool-registry agent)))
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
             (call-signature
               (and storm-state
                    tool
                    (null argument-error)
                    (agent--tool-call-signature call)))
             (storm-signature
               (and call-signature
                    (not blocked-p)
                    (not (tool-storm-guard-exempt-p tool))
                    call-signature))
             (withheld-reason
               (and storm-signature
                    (agent--tool-storm-note-call
                     storm-state
                     storm-signature
                     (function-call-canonical-name call))))
             (persistence
               (if blocked-p
                   ':next-response
                   (if tool
                       (tool-conversation-persistence tool)
                       ':durable))))
         (push (list :call            call
                     :tool            tool
                     :persistence     persistence
                     :blocked-p       blocked-p
                     :argument-error  argument-error
                     :signature       call-signature
                     :storm-state     storm-state
                     :withheld-reason withheld-reason)
              plans)
        (when (and tool
                   (null withheld-reason)
                   (tool-provider-round-trip-barrier-p tool))
          (setf barrier-seen-p t))))
    (nreverse plans)))

(-> agent--provider-error-metadata (provider-error) list)
(defun agent--provider-error-metadata (condition)
  "Return portable terminal failure metadata for provider CONDITION."
  (list :message     (bounded-string (format nil "~A" condition) :limit 2000)
        :status      (provider-error-status condition)
        :code        (provider-error-code condition)
        :incomplete-reason
        (and (typep condition 'provider-incomplete-response)
             (provider-incomplete-response-reason condition))
        :request-id  (provider-error-request-id condition)
        :response-id (provider-error-response-id condition)
        :response    (provider-error-response condition)
        :retryable-p (typep condition 'provider-retryable-error)))

(-> agent--validate-tool-call-identifiers
    (agent list
     &key (:seen-call-identifiers hash-table) (:request-number integer))
    null)
(defun agent--validate-tool-call-identifiers
    (agent calls &key seen-call-identifiers request-number)
  "Validate CALLS and reserve their unique call identifiers for this user turn."
  (let ((round-identifiers (make-hash-table :test #'equal))
        (conversation      (agent-conversation agent)))
    (dolist (call calls)
      (unless (json-object-p call)
        (error 'agent-loop-error
               :message         "The provider returned a tool call that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number  request-number))
      (let ((call-id (json-get call "call_id")))
        (unless (non-empty-string-p call-id)
          (error 'agent-loop-error
                 :message         "The provider returned a function call without a call_id."
                 :conversation-id (conversation-identifier conversation)
                 :request-number  request-number))
        (when (or (gethash call-id seen-call-identifiers)
                  (gethash call-id round-identifiers))
          (error 'agent-loop-error
                 :message         (format nil "The provider repeated function call identifier ~S."
                                          call-id)
                 :conversation-id (conversation-identifier conversation)
                 :request-number  request-number))
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
      (when *telemetry-run*
        (telemetry--call-safely
         (lambda ()
           (telemetry-note-tool :run       *telemetry-run*
                                :tool      tool-name
                                :arguments (json-get call "arguments")
                                :outcome ':error :error-code ':rejected-plan :duration-ms 0))))
      (conversation-append-tool-result
       (agent-conversation agent)
       call-id
       :tool-name   tool-name
       :tool-label  (when (getf plan :tool)
                      (tool-display-name (getf plan :tool)))
       :output      message
       :success-p   nil
       :persistence (getf plan :persistence))
      (agent-observer-status
       observer
       ':tool-call-completed
       (list :tool-round tool-round
             :call-id    call-id
             :tool       tool-name
             :success-p  nil
             :output     message))))
  nil)

(-> agent--execute-tool-plan
    (agent list &key (:observer agent-observer) (:tool-restriction-p boolean)
                     (:record-timings-p boolean))
    list)
(defun agent--execute-tool-plan
    (agent plan &key observer tool-restriction-p record-timings-p)
  "Execute one PLAN body and return its result, timings, or fatal condition."
  (let* ((call                                 (getf plan :call))
         (call-id                              (json-get call "call_id"))
         (argument-error                       (getf plan :argument-error))
         (context
           (make-instance
            'tool-context
            :configuration (agent-configuration agent)
            :worker        (agent-worker agent)
            :conversation  (agent-conversation agent)
            :registry      (agent-tool-registry agent)
            :agent         agent
            :observer      observer
            :call-id       call-id
            :command-authorization-function
            (lambda (command directory)
              (agent-observer-authorize-command observer command directory))
            :tool-authorization-function
            (lambda (tool arguments)
              (agent-observer-authorize-tool observer tool arguments))))
         (*workspace-tool-readable-roots*
           (list
            (config :working-directory
             (agent-configuration agent))
            (config :source-root
             (agent-configuration agent))))
         (*resource-readable-schemes*
           (and tool-restriction-p '("workspace")))
         (*telemetry-tool-corrects-previous-p* nil)
         (*telemetry-tool-arguments*           nil)
         (*telemetry-tool-capture-p*           (not (null *telemetry-run*)))
         (*telemetry-tool-name*                (function-call-canonical-name call))
         (real-start                           (get-internal-real-time))
         (cpu-start                            (get-internal-run-time))
         (result                               nil)
         (condition                            nil))
    (handler-case
        (setf result
              (multiple-value-bind (terminal-p mission-context) (mission-agent-terminal-p agent)
                (cond
                  (terminal-p
                   (mission--stopped-tool-result mission-context))
                  ((getf plan :withheld-reason)
                   (tool-mechanics (getf plan :withheld-reason)
                                   :code ':storm-withheld))
                  (argument-error
                   (tool-mechanics argument-error :code ':invalid-arguments))
                  (t
                   (agent-observer-call-with-tool-execution
                    observer call-id
                    (lambda ()
                      (tool-registry-execute-call
                       (agent-tool-registry agent) call context)))))))
      ((or autolith-control-condition
           serious-condition)
       (failure)
       (setf condition failure)))
    (list
     :plan                          plan
     :result                        result
     :condition                     condition
     :telemetry-corrects-previous-p *telemetry-tool-corrects-previous-p*
     :telemetry-arguments           *telemetry-tool-arguments*
     :telemetry-duration-ms         (telemetry--elapsed-milliseconds real-start)
     :cpu-microseconds
     (and record-timings-p
          (round (* (- (get-internal-run-time) cpu-start) 1000000)
                 internal-time-units-per-second))
     :real-microseconds
     (and record-timings-p
          (round (* (- (get-internal-real-time) real-start) 1000000)
                 internal-time-units-per-second)))))

(-> agent--tool-retry-output (list tool-result) string)
(defun agent--tool-retry-output (plan result)
  "Return RESULT text with a changed prior failure diagnosis for the same PLAN."
  (let* ((state     (getf plan :storm-state))
         (signature (getf plan :signature))
         (content   (tool-result-content result))
         (category  (tool-result-category result)))
    (cond
      ((and state signature (eq category ':failure))
       (multiple-value-bind (previous present-p)
           (gethash signature (agent-tool-storm-state-failures state))
         (setf (gethash signature (agent-tool-storm-state-failures state))
               content)
         (if (and present-p (not (string= previous content)))
             (bounded-string
              (format nil
                      "~A~%Previous failure for this exact call: ~A"
                      content previous))
             content)))
      ((and state signature (eq category ':success))
       (remhash signature (agent-tool-storm-state-failures state))
       content)
      (t
       content))))

(-> agent--complete-tool-executions
    (agent list &key (:observer agent-observer) (:tool-round integer))
    null)
(defun agent--complete-tool-executions
    (agent executions &key observer tool-round)
  "Persist EXECUTIONS and report completions in provider wire order.

Every execution receives an output before its condition is re-signalled. Missing
worker results become explicit unknown outcomes so provider history stays valid."
  (dolist (execution executions)
    (let* ((plan      (getf execution :plan))
           (call      (getf plan :call))
           (call-id   (json-get call "call_id"))
           (tool-name (function-call-canonical-name call))
           (result    (getf execution :result))
           (condition (getf execution :condition)))
      (unless (or result condition)
        (setf condition
              (make-condition
               'agent-loop-error
               :message        (format nil "Tool ~A ended without returning a result."
                                       tool-name)
               :conversation-id
               (conversation-identifier (agent-conversation agent))
               :request-number nil)
              (getf execution :condition) condition))
      (let* ((effective-result
               (or result
                   (tool-failure *conversation-interrupted-tool-output*
                                 :code ':unknown-outcome)))
             (error-type (when condition (telemetry--condition-error-type condition)))
             (output
               (if result
                   (agent--tool-retry-output plan result)
                   *conversation-interrupted-tool-output*)))
        (when *telemetry-run*
          (telemetry--call-safely
           (lambda ()
             (telemetry-note-tool
              :run               *telemetry-run*
              :tool              tool-name
              :arguments         (or (getf execution :telemetry-arguments) (json-get call "arguments"))
              :corrects-previous (getf execution :telemetry-corrects-previous-p)
              :error-code        (tool-result-error-code effective-result)
              :error-type        error-type
              :exit-code         (getf (tool-result-details effective-result) :exit-code)
              :outcome           (telemetry--outcome
                                  :success-p  (tool-result-success-p effective-result)
                                  :error-type error-type
                                  :denied-p   (tool-result-denied-p effective-result)
                                  :error-code (tool-result-error-code effective-result))
              :duration-ms       (getf execution :telemetry-duration-ms)))))
        (conversation-append-tool-result
         (agent-conversation agent)
         call-id
         :tool-name         tool-name
         :tool-label        (when (getf plan :tool)
                              (tool-display-name (getf plan :tool)))
         :output            output
         :details           (tool-result-details effective-result)
         :content-blocks    (tool-result-content-blocks effective-result)
         :success-p         (tool-result-success-p effective-result)
         :category          (tool-result-category effective-result)
         :cpu-microseconds  (getf execution :cpu-microseconds)
         :real-microseconds (getf execution :real-microseconds)
         :persistence       (getf plan :persistence))
        (agent-observer-status
         observer
         ':tool-call-completed
         (list :tool-round        tool-round
               :call-id           call-id
               :tool              tool-name
               :success-p         (tool-result-success-p effective-result)
               :category          (tool-result-category effective-result)
               :cpu-microseconds  (getf execution :cpu-microseconds)
               :real-microseconds (getf execution :real-microseconds)
               :output            output
               :details           (tool-result-details effective-result))))))
  (let ((failure
          (find-if (lambda (execution)
                     (getf execution :condition))
                   executions)))
    (when failure
      (error (getf failure :condition))))
  nil)

(-> agent--tool-thread-function (function) function)
(defun agent--tool-thread-function (function)
  "Return FUNCTION wrapped with the current turn-scoped tool bindings."
  (let ((skill-logical-turn-state   *skill-logical-turn-state*)
        (worker-host-tool-policy    (copy-tree *worker-host-tool-policy*))
        (telemetry-run              *telemetry-run*)
        (shell-log-publications     *shell-log-publications*)
        (mission-admitted-operation *mission-admitted-operation*))
    (lambda ()
      (let ((*skill-logical-turn-state*   skill-logical-turn-state)
            (*worker-host-tool-policy*    worker-host-tool-policy)
            (*telemetry-run*              telemetry-run)
            (*shell-log-publications*     shell-log-publications)
            (*mission-admitted-operation* mission-admitted-operation))
        (funcall function)))))

(defvar *agent-tool-wave-pools* (make-hash-table :test #'eq)
  "Tool wave pools retained until their workers have actually stopped.")

(defvar *agent-tool-wave-pool-lock* (make-lock "Autolith tool wave ownership")
  "Protect the retained wave registry, never held during pool shutdown.")

(-> agent--make-tool-wave-pool (integer) job-pool)
(defun agent--make-tool-wave-pool (count)
  "Create and register a wave before cancellation can unwind its owner."
  (sb-sys:without-interrupts
    (let ((pool (make-job-pool
                 :name                     "Autolith tool wave"
                 :maximum-concurrency      (min count *agent-maximum-concurrent-tool-workers*)
                 :maximum-batch-size count :maximum-live-jobs count
                 :terminal-retention-limit count)))
      (with-lock-held (*agent-tool-wave-pool-lock*)
        (setf (gethash pool *agent-tool-wave-pools*)
              (list :parent cl-jobpond::*current-job* :closing-p nil)))
      pool)))

(-> agent--close-tool-wave-pool (job-pool) null)
(defun agent--close-tool-wave-pool (pool)
  "Bound shutdown while retaining every unfinished wave in the ownership registry."
  (sb-sys:without-interrupts
    (with-lock-held (*agent-tool-wave-pool-lock*)
      (setf (getf (gethash pool *agent-tool-wave-pools*) :closing-p) t)))
  (when (job-pool-close pool)
    (with-lock-held (*agent-tool-wave-pool-lock*)
      (remhash pool *agent-tool-wave-pools*)))
  nil)

(-> agent-tool-wave-cleanup-snapshot (job) list)
(defun agent-tool-wave-cleanup-snapshot (ancestor)
  "Inspect ANCESTOR's wave descendants and retire settled pools without waiting."
  (let ((entries
          (with-lock-held (*agent-tool-wave-pool-lock*)
            (loop for pool being the hash-keys of *agent-tool-wave-pools*
                    using (hash-value record)
                  for parent = (getf record :parent)
                  when (and parent
                            (equal (cl-jobpond:job-root-identifier parent)
                                   (cl-jobpond:job-root-identifier ancestor))
                            (or (eq parent ancestor)
                                (member (job-identifier ancestor)
                                        (job-owner-identifiers parent) :test #'equal)))
                    collect (cons pool (copy-list record))))))
    (loop for (pool . record) in entries
          for closed-p =
          (and (getf record :closing-p)
               (let ((cl-jobpond:*shutdown-timeout-seconds* 0))
                 (job-pool-close pool)))
          if closed-p
          do (with-lock-held (*agent-tool-wave-pool-lock*)
               (remhash pool *agent-tool-wave-pools*))
          else
            collect
            (list :state     (cl-jobpond:job-pool-lifecycle-state pool)
                  :closing-p (getf record :closing-p)
                  :jobs
                  (loop for job in (cl-jobpond:job-pool-list-jobs pool)
                        for snapshot = (job-snapshot job)
                        collect (list :id                  (job-identifier job)
                                      :state               (getf snapshot :state)
                                      :cancellation-reason (getf snapshot :cancellation-reason)
                                      :condition-report    (getf snapshot :condition-report)))))))

(-> agent--run-tool-wave
    (agent list &key (:observer agent-observer) (:tool-round integer)
                     (:tool-restriction-p boolean))
    null)
(defun agent--run-tool-wave
    (agent plans &rest options &key observer tool-round tool-restriction-p)
  "Retain output through durable wave results, forwarding supplied keywords unchanged."
  (declare (ignore observer tool-round tool-restriction-p))
  (shell-log-call-with-publication
   (agent-configuration agent)
   (lambda ()
     (apply #'agent--run-tool-wave-body agent plans options))))

(-> agent--run-tool-wave-body
    (agent list &key (:observer agent-observer) (:tool-round integer)
                     (:tool-restriction-p boolean))
    null)
(defun agent--run-tool-wave-body
    (agent plans &key observer tool-round tool-restriction-p)
  "Execute independent PLANS concurrently and complete them in wire order."
  (dolist (plan plans)
    (let ((call (getf plan :call)))
      (agent-observer-status
       observer
       :tool-call-started
       (list :tool-round tool-round
             :call-id    (json-get call "call_id")
             :tool       (function-call-canonical-name call)
             :arguments  (json-get call "arguments")))))
  (let* ((count            (length plans))
         (executions
           (map 'vector (lambda (plan) (list :plan plan)) plans))
         (record-timings-p (= count 1)))
    (if (= count 1)
        (setf (aref executions 0)
              (agent--execute-tool-plan agent (first plans)
                                        :observer           observer
                                        :tool-restriction-p tool-restriction-p
                                        :record-timings-p   t))
        (let* ((pool        nil)
               (jobs        nil)
               (completed-p nil))
          (unwind-protect
               (progn
                 (sb-sys:without-interrupts
                   (setf pool (agent--make-tool-wave-pool count)))
                 (setf jobs
                       (job-pool-submit-batch
                        pool
                        (loop for plan in plans
                              for index from 0
                              collect
                              (let* ((plan  plan)
                                     (index index)
                                     (function
                                       (agent--tool-thread-function
                                        (lambda ()
                                          (setf (aref executions index)
                                                (agent--execute-tool-plan
                                                 agent plan
                                                 :observer           observer
                                                 :tool-restriction-p tool-restriction-p
                                                 :record-timings-p   record-timings-p))))))
                                (list :parent-job cl-jobpond::*current-job*
                                      :function   (lambda (job)
                                                    (job-check-cancellation job)
                                                    (funcall function)))))))
                 (dolist (job jobs)
                   (job-await job)
                   (unless (eq (job-state job) ':completed)
                     (job-check-cancellation job)))
                 (setf completed-p t))
            (when pool
              (unwind-protect
                   (unless completed-p
                     (dolist (job jobs)
                       (job-cancel job :reason ':wave-cancelled :cascade-p t)))
                (agent--close-tool-wave-pool pool))))))
    (agent--complete-tool-executions agent (coerce executions 'list)
                                     :observer   observer
                                     :tool-round tool-round))
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
        (wave      nil)
        (wave-keys (make-hash-table :test #'eql))
        (*worker-host-tool-policy*
          (list :restricted-p tool-restriction-p :allowlist (copy-list tool-allowlist))))
    (labels ((flush-wave ()
               (when wave
                 (agent--run-tool-wave
                  agent
                  (nreverse wave)
                  :observer           serialized-observer
                  :tool-round         tool-round
                  :tool-restriction-p tool-restriction-p)
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
  (conversation-flush-async-lisp-events (agent-conversation agent))
  (when (conversation-async-lisp--safe-boundary-p (agent-conversation agent))
    (task-completion-deliver agent))
  (agent--persist-steering-input
   agent observer
   :messages       (agent-observer-take-steering observer)
   :request-number request-number))

(-> agent--persist-steering-input
    (agent agent-observer &key (:messages list) (:request-number integer))
    null)
(defun agent--persist-steering-input (agent observer &key messages request-number)
  "Persist MESSAGES before acknowledging each identifier through OBSERVER."
  (let ((conversation (agent-conversation agent)))
    (unless (listp messages)
      (error 'agent-loop-error
             :message         "The agent observer returned malformed steering input."
             :conversation-id (conversation-identifier conversation)
             :request-number  request-number))
    (dolist (entry messages)
      (let* ((message
               (etypecase entry
                 (agent-steering-input
                  (agent-steering-input-content entry))
                 ((or string user-message-input)
                  entry)))
             (identifier
               (and (typep entry 'agent-steering-input)
                    (agent-steering-input-identifier entry)))
             (request
               (and (typep entry 'agent-steering-input)
                    (agent-steering-input-operation-request entry))))
        (unless (or (and (stringp message) (non-empty-string-p message))
                    (and (typep message 'user-message-input)
                         (or (non-empty-string-p
                              (user-message-input-text message))
                             (user-message-input-image-pathnames message))))
          (error 'agent-loop-error
                 :message         "The agent observer returned an empty steering message."
                 :conversation-id (conversation-identifier conversation)
                 :request-number  request-number))
        (unless (agent-observer-apply-steering-entry observer agent entry)
          (conversation-append-user-message
           conversation
           message
           :pending-input-identifier identifier
           :automatic-p              (not (null request))
           :operation-request        request)
          (skill-record-steering-input message))
        (when identifier
          (agent-observer-steering-persisted observer identifier))))
    (when messages
      (agent-observer-status
       observer
       :steering-applied
       (list :message-count (length messages)))))
  nil)

(defgeneric agent-should-compact-p (agent)
  (:documentation
   "Return true when AGENT's conversation should compact before continuing."))

(defgeneric agent-compaction-tool-p (agent)
  (:documentation
   "Return true when AGENT's requests that offer tools also declare compaction.submit."))

(defmethod agent-compaction-tool-p ((agent agent))
  "Declare it, so a compaction request repeats the tools of the requests around it."
  t)

(defmethod agent-should-compact-p ((agent agent))
  "Compare current matched input accounting with the soft compaction threshold."
  (>= (context-estimate-known-tokens
       (agent-configuration agent) (agent-conversation agent)
       :tool-registry (agent-tool-registry agent)
       :hurry-up-p    (agent-hurry-up-p agent)
       :provider      (agent-provider agent)
       :family        (provider-family (agent-provider agent)))
      (configuration-compaction-token-limit (agent-configuration agent))))

(-> agent--note-compaction-request
    (agent agent-observer &key (:request-number (integer 1))
                               (:usage t)
                               (:response-id (option string))
                               (:problem (option string)))
    null)
(defun agent--note-compaction-request
    (agent observer &key request-number usage response-id problem)
  "Persist and report one compaction request's USAGE under REQUEST-NUMBER.

The request re-reads the prompt and generates output under the configured
effort, so it stays visible to usage accounting instead of hiding behind the
compaction record. PROBLEM says why the request gave no usable checkpoint."
  (let ((details
          (append (list :request-number request-number
                        :compaction     ':summary
                        :response-id    response-id
                        :usage          (agent--portable-value
                                         (provider-usage-normalize usage)))
                  (when problem
                    (list :problem problem)))))
    (conversation-append-provider-metadata (agent-conversation agent) details)
    (agent-observer-status observer :compaction-request-completed details))
  nil)

(-> agent--compaction-submission
    (provider-result list list) (values list (option string)))
(defun agent--compaction-submission (result references unsummarized)
  "Return the checkpoint RESULT submits, or NIL and why RESULT submits none.

RESULT must hold exactly one finished compaction.submit call and no visible
text; private reasoning may accompany the call. A tool call can leave the
turn ':CONTINUE, so turn completion says nothing here. REFERENCES and
UNSUMMARIZED pass to CONTINUATION-SUBMISSION."
  (let* ((calls (provider-result-tool-calls result))
         (call  (first calls))
         (text  (provider-result-assistant-text result))
         (problem
           (cond
             ((not (and call
                        (null (rest calls))
                        (equal (list (json-get call "namespace") (json-get call "name"))
                               *continuation-submit-tool*)))
              "the response must hold exactly one compaction.submit call")
             ((not (and (non-empty-string-p (json-get call "call_id"))
                        (member (json-get call "status") '(nil "completed") :test #'equal)
                        (stringp (json-get call "arguments"))))
              "the compaction.submit call is incomplete")
             ((and (stringp text) (some #'alphanumericp text))
              "the response holds text beside the call"))))
    (if problem
        (values nil problem)
        (continuation-submission (json-get call "arguments") references
                                 :unsummarized unsummarized))))

(-> agent--advance-compaction-view (conversation provider-result string) null)
(defun agent--advance-compaction-view (view result problem)
  "Show the model its rejected RESULT in transient VIEW and name the PROBLEM to correct.

Each call gets a correlated output and is never executed. A result that
called nothing, or that cannot be replayed, gets a user message instead."
  (let ((items      (provider-result-output-items result))
        (calls      (provider-result-tool-calls result))
        (correction (format nil "The checkpoint was not accepted: ~A. Call compaction.submit once more with the complete corrected checkpoint."
                            problem)))
    (if (and calls
             (every #'json-object-p items)
             (every (lambda (call) (non-empty-string-p (json-get call "call_id"))) calls))
        (progn
          (dolist (item items)
            (conversation--append-input-item view item))
          (dolist (call calls)
            (conversation--append-input-item
             view (function-call-output-item (json-get call "call_id") correction))))
        (conversation--append-input-item view (user-message-item correction))))
  nil)

(-> agent--compaction-failure-terminal-p (provider-error) boolean)
(defun agent--compaction-failure-terminal-p (condition)
  "Return true when CONDITION ends compaction instead of costing one generation.

A permission failure is one. So is a retryable failure that still reached
this caller: with regeneration turned off, the provider layer spent its
reconnections on it before any generation began. Another generation cannot
repair either, and a checkpoint without notes would only lose history."
  (or (eql (provider-error-status condition) 403)
      (typep condition 'provider-retryable-error)))

(-> agent--request-checkpoint
    (agent agent-observer conversation
     &key (:tool-namespaces vector) (:recovery compaction-recovery) (:request-context t)
          (:latest-request (option string)) (:references list) (:unsummarized list)
          (:request-number (integer 0)))
    (values (option list) (integer 0)))
(defun agent--request-checkpoint
    (agent observer view
     &key tool-namespaces recovery request-context latest-request references unsummarized
          request-number)
  "Ask the model for a checkpoint of VIEW; return it and the number of requests issued.

Each request declares TOOL-NAMESPACES, the tools of the ordinary requests
around it with compaction.submit among them, and forces that call where the
provider supports it. The instructions follow the history.
They quote LATEST-REQUEST, the user message that stays after the checkpoint,
and list REFERENCES. A rejected submission is shown back to the
model with what to correct. The checkpoint is NIL when
*AGENT-COMPACTION-MAXIMUM-GENERATIONS* requests produced none. The provider
layer regenerates nothing by itself here, so every generation counts against
that maximum. A refusal and a terminal failure are signaled."
  (let ((provider                             (agent-provider agent))
        (issued                               0)
        (*compaction-instructions*
          (format nil "~A~2%~@[The latest user message, which stays after the checkpoint:~%~A~2%~]~A"
                  *compaction-instructions*
                  (and latest-request
                       (bounded-string latest-request
                                       :limit *agent-compaction-latest-request-characters*))
                  (continuation-catalog references)))
        (*provider-tool-choice*               *continuation-submit-tool*)
        (*provider-maximum-streaming-retries* 0)
        (*telemetry-request-role*             ':compaction))
    (loop repeat *agent-compaction-maximum-generations*
          do (multiple-value-bind (result candidate)
                 (handler-case
                     (compaction-recovery-call
                      recovery provider view :dispatch
                      (lambda (candidate)
                        (agent--check-request-budget agent (+ request-number issued))
                        (mission-call-with-request-view
                         request-context candidate
                         (lambda ()
                           (provider-stream-turn
                            provider candidate :tool-namespaces tool-namespaces
                            :event-callback
                            (lambda (event)
                              (declare (ignore event))
                              (agent-observer-status observer :provider-progress nil))
                            :compaction-p                       t))))
                      :preflight (lambda (candidate)
                                  (provider-check-request-context
                                   provider
                                   (provider-request-object provider candidate tool-namespaces
                                                            :compaction-p t))))
                   (provider-error (condition)
                     (when (agent--compaction-failure-terminal-p condition)
                       (error condition))
                     condition))
                (when (and *compaction-report-context* (typep result 'provider-result))
                  (compaction-report-capture-result *compaction-report-context* result))
               (incf issued)
               (if (typep result 'provider-result)
                   (multiple-value-bind (continuation problem)
                       (agent--compaction-submission result references unsummarized)
                     ;; Every completed paid request is accounted for before its
                     ;; response decides anything.
                     (agent--note-compaction-request
                      agent observer
                      :request-number (+ request-number issued)
                      :usage          (provider-result-usage result)
                      :response-id    (provider-result-response-id result)
                      :problem        problem)
                     (when (some #'provider--output-refusal-p
                                 (provider-result-output-items result))
                       (error 'provider-protocol-error
                              :message     "The provider refused to write the compaction checkpoint."
                              :status      nil
                              :code        nil
                              :request-id  nil
                              :response-id (provider--sanitize-wire-string
                                            (provider-result-response-id result))
                              :response    nil))
                     (when continuation
                       (return (values continuation issued)))
                     (setf view candidate)
                     (agent--advance-compaction-view view result problem))
                   (agent--note-compaction-request
                    agent observer
                    :request-number (+ request-number issued)
                    :problem        (princ-to-string result))))
          finally (return (values nil issued)))))

(-> agent--compaction-checkpoint
    (agent agent-observer conversation-compaction
     &key (:view conversation) (:tool-namespaces vector) (:recovery compaction-recovery)
          (:request-context t) (:unfinished-work list) (:request-number (integer 0)))
    (values list (integer 0)))
(defun agent--compaction-checkpoint
    (agent observer compaction
     &key view tool-namespaces recovery request-context unfinished-work request-number)
  "Return the checkpoint for COMPACTION and the number of requests it took.

The model writes the checkpoint from VIEW, the history before the retained
tail. It may cite the captured user messages and tool results, plan steps,
mission gates, the jobs of UNFINISHED-WORK and what the previous checkpoint
cited. The catalog lines of the tail's records are all it learns of the tail
beside the latest user message. When it submits none, the checkpoint keeps the
previous one's notes and names the records of VIEW as unsummarized."
  (let ((prior    (conversation-continuation (agent-conversation agent)))
        (evidence (conversation-compaction-evidence compaction)))
    (multiple-value-bind (submitted requests)
        (agent--request-checkpoint
         agent observer view
         :tool-namespaces tool-namespaces
         :recovery        recovery
         :request-context request-context
         :latest-request  (getf (find nil evidence :key (lambda (entry) (getf entry :call-id)))
                                :text)
         :references      (continuation-references agent evidence
                                                   :prior           prior
                                                   :unfinished-work unfinished-work)
         :unsummarized    (getf prior :unsummarized)
         :request-number  request-number)
      (values (or submitted (continuation-without-summary prior evidence))
              requests))))

(-> agent--result-reasoning-context (provider-result) (option string))
(defun agent--result-reasoning-context (result)
  "Return the reasoning retention mode RESULT's response reports, or NIL."
  (let ((metadata (provider-result-metadata result)))
    (and metadata (provider-response-metadata-reasoning-context metadata))))

(-> agent--check-request-budget (agent (integer 0)) null)
(defun agent--check-request-budget (agent request-number)
  "Refuse another request when AGENT's turn has used its request budget."
  (when (>= request-number *agent-maximum-provider-requests-per-turn*)
    (error 'agent-loop-error
           :message         (format nil "The turn reached its ~D provider-request safety limit."
                                    *agent-maximum-provider-requests-per-turn*)
           :conversation-id (conversation-identifier (agent-conversation agent))
           :request-number  request-number))
  nil)

(-> agent--prepare-provider-request (agent) (integer 0))
(defun agent--prepare-provider-request (agent)
  "Bound cold-cache history for AGENT; a retention estimate alone never rewrites it."
  (let ((provider      (agent-provider agent))
        (configuration (agent-configuration agent)))
    (conversation-prepare-provider-request
     (agent-conversation agent)
     :identity     (list (provider-family provider) (config :model configuration))
     :idle-seconds (provider-prompt-cache-lifetime-seconds provider configuration))))

(-> agent--request-tool-choice (boolean boolean (integer 0)) (member :auto :none))
(defun agent--request-tool-choice (tools-p tool-restriction-p tool-rounds)
  "Resolve the next request's tool choice from its restricted round budget."
  (if (or (not tools-p)
          (not tool-restriction-p)
          (< tool-rounds *agent-restricted-maximum-tool-rounds*))
      ':auto
      ':none))

(-> agent--configured-max-output-tokens (agent) (option integer))
(defun agent--configured-max-output-tokens (agent)
  "Return AGENT's configured generation cap, or NIL when none is set."
  (let ((value (config :max-output-tokens (agent-configuration agent))))
    (and (integerp value) (plusp value) value)))

(-> agent--tool-namespaces
    (agent &key (:tools-p boolean) (:tool-restriction-p boolean) (:tool-allowlist (option list))
                (:compaction-p boolean))
    vector)
(defun agent--tool-namespaces
    (agent &key tools-p tool-restriction-p tool-allowlist compaction-p)
  "Return the tool namespaces AGENT's requests declare.

A request that offers tools also declares compaction.submit, unless AGENT
never compacts in the ordinary course. A compaction request then repeats the
declarations of the ordinary requests around it and only forces that call, so
it reads the prompt prefix they cached instead of paying for the whole history
again. A COMPACTION-P request declares compaction.submit in any case."
  (let ((offered (cond
                   ((not tools-p)
                    #())
                   (tool-restriction-p
                    (tool-registry-provider-schemas (agent-tool-registry agent)
                                                    :canonical-names tool-allowlist))
                   (t
                    (tool-registry-provider-schemas (agent-tool-registry agent))))))
    (if (or compaction-p
            (and (plusp (length offered)) (agent-compaction-tool-p agent)))
        (concatenate 'vector offered (continuation-tool-namespaces))
        offered)))

(-> agent--validate-compaction-projection
    (agent conversation &key (:goal-context (option string))
                      (:tool-restriction-p boolean) (:tool-allowlist (option list))
                       (:tools-p boolean) (:tool-rounds (integer 0))
                       (:maximum-output-tokens (option integer)))
    null)
(defun agent--validate-compaction-projection
    (agent candidate &key goal-context tool-restriction-p tool-allowlist tools-p
                         (tool-rounds 0) maximum-output-tokens)
  "Reject a candidate checkpoint whose fresh ordinary request cannot fit."
  (let* ((provider        (agent-provider agent))
         (tool-namespaces (agent--tool-namespaces agent
                                                  :tools-p            tools-p
                                                  :tool-restriction-p tool-restriction-p
                                                  :tool-allowlist     tool-allowlist)))
    (let ((*provider-maximum-output-tokens*
            (or maximum-output-tokens
                (agent--configured-max-output-tokens agent)))
          (*provider-tool-choice*
            (agent--request-tool-choice tools-p tool-restriction-p tool-rounds))
          (*context-request-contributions*
            (and tools-p (not tool-restriction-p)
                 (mcp-tool-registry-context-contributions (agent-tool-registry agent)))))
      (multiple-value-bind (request delivery)
          (provider-request-object provider candidate tool-namespaces
                                   :goal-context goal-context)
        (declare (ignore delivery))
        (provider-check-request-context provider request)))))

(-> agent-compact-conversation
    (agent agent-observer &key (:tool-allowlist (option list))
                               (:tool-restriction-p boolean)
                               (:tools-p boolean)
                               (:goal-context (option string))
                               (:request-number (integer 0))
                               (:next-request-status-details boolean)
                               (:next-request-tool-rounds (integer 0)))
    (values (integer 0) (option list)))
(defun agent-compact-conversation
    (agent observer &key tool-allowlist (tool-restriction-p nil) (tools-p t)
                           goal-context (request-number 0)
                           (next-request-status-details nil) (next-request-tool-rounds 0))
  "Compact AGENT's conversation into a typed checkpoint.

The model submits the checkpoint over the readable captured history, so every
provider family can read it. When no usable submission arrives, the checkpoint
keeps the previous one's notes and names the records nobody summarized.

Each completed provider request is metered before response validation. Capture
the durable cutoff before the requests and retain arrivals at publication.
Return issued request count and optional next-request details."
  (compaction-report-call
   (agent-configuration agent)
   (lambda (report)
     (agent--check-request-budget agent request-number)
     (agent--prepare-provider-request agent)
     (let* ((conversation                 (agent-conversation agent))
            (compaction                   (conversation-compaction-portable-capture
                                           (conversation-compaction-capture conversation)))
            (captured-view                (conversation-compaction-view compaction))
            (request-context              (mission-request-view-context conversation))
            (recovery
              (make-compaction-recovery
               (lambda (details)
                 (conversation-append-provider-metadata
                  conversation (list :compaction-recovery details))
                 (agent-observer-status observer :compaction-recovery details))))
            (unfinished-work              nil)
            (*request-context-hurry-up-p* (agent-hurry-up-p agent)))
       ;; Announce compaction before reading job storage, which can take a while.
       (agent-observer-status
        observer
        :compaction-started
        (list :total-tokens (conversation-last-total-tokens conversation)))
       (setf unfinished-work (task-unfinished-work-snapshot agent))
       (let ((*provider-hosted-tools-enabled-p* (not tool-restriction-p))
             (family                            (provider-family (agent-provider agent))))
         (multiple-value-bind (continuation issued)
             (agent--compaction-checkpoint
              agent observer compaction
              :view            (conversation-compaction-summary-view
                                captured-view
                                (conversation-input-items-for-family
                                 captured-view family :include-ephemeral-p nil)
                                family)
              :tool-namespaces (agent--tool-namespaces agent
                                                       :tools-p            tools-p
                                                       :tool-restriction-p tool-restriction-p
                                                       :tool-allowlist     tool-allowlist
                                                       :compaction-p       t)
              :recovery        recovery
              :request-context request-context
              :unfinished-work unfinished-work
              :request-number  request-number)
           (let* ((next-output-tokens
                    (if next-request-status-details
                        (let ((value
                                (agent-observer-status
                                 observer :provider-request-started
                                 (list :request-number (+ request-number issued 1)
                                       :tool-rounds    next-request-tool-rounds))))
                          (or (and (integerp value) (plusp value) value)
                              (agent--configured-max-output-tokens agent)))
                        (or *provider-maximum-output-tokens*
                            (agent--configured-max-output-tokens agent))))
                  (next-request-details
                    (and next-request-status-details
                         (list :announced-p           t
                               :maximum-output-tokens next-output-tokens)))
                  (publication
                    (list :compaction (compaction-recovery-publication recovery compaction)
                          :recovery   (compaction-recovery-audit recovery :completed)
                          :validate-projection
                          (lambda (candidate)
                            (agent--validate-compaction-projection
                             agent candidate
                             :goal-context          goal-context
                             :tools-p               tools-p
                             :tool-rounds           next-request-tool-rounds
                             :maximum-output-tokens next-output-tokens
                             :tool-restriction-p    tool-restriction-p
                             :tool-allowlist        tool-allowlist)))))
             (with-recursive-lock-held ((conversation-append-lock conversation))
               (setf unfinished-work (task-unfinished-work-snapshot agent))
               (let ((*conversation-compaction-publication-function*
                       (lambda () (setf (compaction-report-published-p report) t))))
                 (apply #'conversation-append-summary
                        conversation continuation
                        :unfinished-work unfinished-work
                        publication)))
             (agent-observer-status
              observer :compaction-completed (list :provider-requests issued))
             (values issued next-request-details))))))
   :notify (lambda (details)
             (agent-observer-status observer :compaction-failed details))))

(-> agent--run-provider-loop
    (agent agent-observer &key (:goal-context (option string))
                          (:tools-p boolean)
                          (:tool-allowlist (option list))
                          (:tool-restriction-p boolean)
                          (:request-number (integer 0)))
    provider-result)
(defun agent--run-provider-loop
    (agent observer
     &key goal-context (tools-p t) tool-allowlist (tool-restriction-p nil)
          (request-number 0))
  "Run provider and optional tool rounds until AGENT's turn completes.

REQUEST-NUMBER counts provider requests the turn already issued before the
loop, so compaction ahead of the user message shares the turn's budget."
    (let ((*task-unfinished-work-agent* agent)
          (seen-call-identifiers        (make-hash-table :test #'equal))
          (storm-state                  (agent-tool-storm-state-create))
          (tool-rounds                  0)
          (tool-calls                   0)
          (maximum-output-tokens        (agent--configured-max-output-tokens agent))
          (next-request-details         nil))
     (declare (special *task-unfinished-work-agent*))
     (loop for recovery-used-p = nil
           do (agent--check-request-budget agent request-number)
       (when (and tools-p (not tool-restriction-p))
         (mcp-tool-registry-refresh
          (agent-tool-registry agent)
          :only-dirty-p t))
       (when (agent-should-compact-p agent)
         (setf recovery-used-p t)
         (multiple-value-bind (compaction-requests details)
             (agent-compact-conversation
              agent observer
              :tool-allowlist              tool-allowlist
              :tool-restriction-p          tool-restriction-p
              :tools-p                     tools-p
              :goal-context                goal-context
              :request-number              request-number
              :next-request-status-details t
              :next-request-tool-rounds    tool-rounds)
           (incf request-number compaction-requests)
           (setf next-request-details details)))
       (let* ((conversation    (agent-conversation agent))
              (tool-calls-p
                (and tools-p
                     (eq (agent--request-tool-choice tools-p tool-restriction-p tool-rounds)
                         ':auto)))
              ;; A restricted turn past its tool rounds keeps its schemas and
              ;; disables calls instead, so its answer reuses the cached prefix.
              (tool-namespaces
                (agent--tool-namespaces agent
                                        :tools-p            tools-p
                                        :tool-restriction-p tool-restriction-p
                                        :tool-allowlist     tool-allowlist))
              (cache-prefix    nil)
              (context-request nil)
              (result
                (loop
                  (agent--check-request-budget agent request-number)
                  (agent--prepare-provider-request agent)
                  (incf request-number)
                  (let ((output-limit
                          (if (and next-request-details (getf next-request-details :announced-p))
                              (getf next-request-details :maximum-output-tokens)
                              (agent-observer-status
                               observer :provider-request-started
                               (list :request-number request-number :tool-rounds tool-rounds)))))
                    (setf next-request-details  nil
                          maximum-output-tokens
                          (or (and (integerp output-limit)
                                   (plusp output-limit) output-limit)
                              (agent--configured-max-output-tokens agent))
                          cache-prefix          nil))
                  (handler-case
                      (return
                        (let* ((context-request
                                 (setf context-request
                                       (provider-context-request-create
                                        (agent-provider agent) conversation
                                        (tool-registry-provider-schemas (agent-tool-registry agent)))))
                               (*provider-context-request*       context-request)
                               (*request-context-participant*
                                 (participant-state-identity (agent-participant-state agent)))
                               (*provider-hosted-tools-enabled-p*
                                 (not tool-restriction-p))
                               (*provider-tool-choice*
                                 (agent--request-tool-choice tools-p tool-restriction-p tool-rounds))
                               (*provider-maximum-output-tokens* maximum-output-tokens)
                               (*provider-cache-request-prefix*  nil)
                               (*provider-cache-prefix-function*
                                 (lambda (prefix) (setf cache-prefix prefix)))
                               (*request-context-hurry-up-p*
                                 (agent-hurry-up-p agent))
                               (*context-request-contributions*
                                 (and tools-p
                                      (not tool-restriction-p)
                                      (mcp-tool-registry-context-contributions
                                       (agent-tool-registry agent)))))
                          (provider-stream-turn
                           (agent-provider agent)
                           conversation
                           :tool-namespaces tool-namespaces
                           :event-callback
                           (agent--provider-event-callback
                            observer
                            :conversation   conversation
                            :request-number request-number)
                           :goal-context    goal-context)))
                    (provider-context-budget-error (condition)
                      ;; Mission supervision and zero-usage settlement have unwound.
                      (when recovery-used-p (error condition))
                      (setf recovery-used-p t)
                      (agent--check-request-budget agent request-number)
                      (multiple-value-bind (compaction-requests details)
                          (agent-compact-conversation
                           agent observer
                           :tool-allowlist              tool-allowlist
                           :tool-restriction-p          tool-restriction-p
                           :tools-p                     tools-p
                           :goal-context                goal-context
                           :request-number              request-number
                           :next-request-status-details t
                           :next-request-tool-rounds    tool-rounds)
                        (incf request-number compaction-requests)
                        (setf next-request-details details)))
                    (provider-error (condition)
                      (conversation-append-provider-metadata
                       conversation
                       (list :request-number request-number
                             :failure
                             (agent--provider-error-metadata condition)))
                      (error condition)))))
              (calls           (provider-result-tool-calls result))
              (completion-details
                (list :request-number    request-number
                      :response-id       (provider-result-response-id result)
                      :usage             (agent--portable-value
                                          (provider-usage-normalize (provider-result-usage result)))
                      :cache-prefix      cache-prefix
                      :output-item-count (length (provider-result-output-items result))
                      :tool-call-count   (length calls)
                      :turn-completion   (provider-result-turn-completion result))))
          (conversation-note-provider-response conversation)
          (let ((call-plans nil))
            (handler-case
                (progn
                  (when (and calls (not tools-p))
                    (error 'agent-loop-error
                           :message         "A tool-free model turn returned function calls."
                           :conversation-id (conversation-identifier conversation)
                           :request-number  request-number))
                  (when (and calls tool-restriction-p (not tool-calls-p))
                    (error 'agent-loop-error
                           :message
                           (format nil "A restricted turn exceeded its ~D tool-round limit."
                                   *agent-restricted-maximum-tool-rounds*)
                           :conversation-id (conversation-identifier conversation)
                           :request-number  request-number))
                  (when (and calls tool-restriction-p)
                    (agent--validate-tool-call-allowlist
                     agent calls tool-allowlist :request-number request-number))
                  (conversation-clear-ephemeral-input-items conversation)
                  (agent--validate-tool-call-identifiers
                   agent calls :seen-call-identifiers seen-call-identifiers
                               :request-number        request-number)
                  (setf call-plans
                        (agent--tool-call-plans agent calls :storm-state storm-state))
                  (agent--persist-provider-result
                   agent
                   result
                   :request-number request-number
                   :call-plans     call-plans))
              (agent-loop-error (condition)
                ;; Rejected calls still carry billable provider usage.
                (agent-observer-status observer :provider-request-completed completion-details)
                (error condition)))
            (provider-context-note-response context-request result)
            (agent--answer-tool-searches
             agent result tool-namespaces :request-number request-number)
            (agent--note-persisted-assistant-response
             observer result request-number)
            (agent-observer-status observer :provider-request-completed completion-details)
            (knowledge-usage-capture conversation
                                     :calls          calls
                                     :request-number request-number
                                     :usage          (provider-result-usage result))
            (when (or (agent-turn-complete-p agent result)
                      (and (null calls) (mission-agent-terminal-p agent)))
              (agent-observer-status
               observer
               :turn-completed
               (append
                (list :provider-requests request-number
                      :tool-rounds       tool-rounds
                      :tool-calls        tool-calls
                      :response-id       (provider-result-response-id result))
                (agent-turn-completion-details agent)))
              (agent-learn-procedures agent observer tool-calls)
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
                                          :observer           observer
                                          :tool-round         tool-rounds
                                          :tool-allowlist     tool-allowlist
                                          :tool-restriction-p tool-restriction-p)
               (if (or (agent-turn-complete-p agent result)
                       (mission-agent-terminal-p agent))
                   (progn
                     (agent-observer-status
                      observer
                      :turn-completed
                      (append
                       (list :provider-requests request-number
                             :tool-rounds       tool-rounds
                             :tool-calls        tool-calls
                             :response-id       (provider-result-response-id result))
                       (agent-turn-completion-details agent)))
                     (agent-learn-procedures agent observer tool-calls)
                     (return result))
                   (agent--apply-steering-input
                    agent observer request-number)))))))))

(defmethod initialize-instance :after ((agent agent) &key conversation)
  "Attach a new execution to its participant's context backing."
  (when (and conversation
             (null (participant-state-conversation (agent-participant-state agent))))
    (setf (participant-state-conversation (agent-participant-state agent)) conversation)))

(defgeneric agent-conversation (agent)
  (:documentation "Return the live context backing of AGENT's participant, or NIL before setup."))

(defmethod agent-conversation ((agent agent))
  "Read AGENT's participant backing without retaining execution-owned context."
  (participant-state-conversation (agent-participant-state agent)))
