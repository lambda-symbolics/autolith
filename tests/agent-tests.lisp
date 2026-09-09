(in-package #:autolith)

;;;; -- Scripted Agent Boundary --

(defclass scripted-provider (model-provider)
  ((configuration
    :initarg :configuration
    :initform nil
    :reader scripted-provider-configuration
    :type (option configuration)
    :documentation "Optional configuration used to inspect request-local Skills.")
   (results
    :initarg :results
    :accessor scripted-provider-results
    :type list
    :documentation "The provider results returned in request order.")
   (input-counts
    :initform nil
    :accessor scripted-provider-input-counts
    :type list
    :documentation "Conversation input lengths observed before each request.")
   (input-snapshots
    :initform nil
    :accessor scripted-provider-input-snapshots
    :type list
    :documentation "Request projections observed before each provider request.")
   (skill-selection-snapshots
    :initform nil
    :accessor scripted-provider-skill-selection-snapshots
    :type list
    :documentation "Logical-turn Skill names observed before each request.")
   (skill-contribution-snapshots
    :initform nil
    :accessor scripted-provider-skill-contribution-snapshots
    :type list
    :documentation "Skill contribution identifiers observed before each request.")
   (turn-states
    :initform nil
    :accessor scripted-provider-turn-states
    :type list
    :documentation "Request-local turn states observed before each request.")
   (tool-schema-counts
    :initform nil
    :accessor scripted-provider-tool-schema-counts
    :type list
    :documentation "The number of tool namespaces advertised on each request.")
   (goal-contexts
    :initform nil
    :accessor scripted-provider-goal-contexts
    :type list
    :documentation "The goal context supplied on each request.")
   (compaction-flags
    :initform nil
    :accessor scripted-provider-compaction-flags
    :type list
    :documentation "The compaction flag supplied on each request."))
  (:documentation "A deterministic provider for exercising repeated agent rounds."))

(defclass native-scripted-provider (scripted-provider)
  ((native-items
    :initarg :native-items
    :accessor native-scripted-provider-native-items
    :type list
    :documentation "Opaque native checkpoints returned in request order.")
   (native-input-snapshots
    :initform nil
    :accessor native-scripted-provider-native-input-snapshots
    :type list
    :documentation "Durable projections observed by native compaction requests."))
  (:documentation "A scripted provider that supports Codex-style native compaction."))

(defmethod provider-family ((provider native-scripted-provider))
  "Treat the native scripted provider as the Codex model family."
  (declare (ignore provider))
  ':codex)

(defmethod provider-native-compact-conversation
    ((provider native-scripted-provider)
     (conversation conversation)
     &key tool-namespaces event-callback)
  "Return the next scripted native checkpoint after recording durable input."
  (declare (ignore tool-namespaces event-callback))
  (push (conversation-input-items-for-request
         conversation :include-ephemeral-p nil)
        (native-scripted-provider-native-input-snapshots provider))
  (pop (native-scripted-provider-native-items provider)))

(defmethod provider-stream-turn
    ((provider scripted-provider)
     (conversation conversation)
     &key
       tool-namespaces
       event-callback
       goal-context
       compaction-p)
  "Return PROVIDER's next scripted result after recording request state."
  (declare (type vector tool-namespaces)
           (type function event-callback))
  (let ((input-items
          (conversation-input-items-for-request
           conversation
           :include-ephemeral-p (not compaction-p))))
    (push (copy-list input-items)
          (scripted-provider-input-snapshots provider))
    (push (length input-items)
        (scripted-provider-input-counts provider))
    (push (and (skill-logical-turn-active-p)
               (skill-logical-turn-selection-names))
          (scripted-provider-skill-selection-snapshots provider))
    (push
     (and (scripted-provider-configuration provider)
          (mapcar
           #'context-contribution-identifier
           (skill-request-contributions
            (scripted-provider-configuration provider)
            conversation)))
     (scripted-provider-skill-contribution-snapshots provider)))
  (push (conversation-turn-state conversation)
        (scripted-provider-turn-states provider))
  (push (length tool-namespaces)
        (scripted-provider-tool-schema-counts provider))
  (push goal-context
        (scripted-provider-goal-contexts provider))
  (push compaction-p
        (scripted-provider-compaction-flags provider))
  (let ((result (pop (scripted-provider-results provider))))
    (unless result
      (error "The scripted provider has no remaining result."))
    (when (typep result 'serious-condition)
      (error result))
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "delta"))
    result))

(defclass agent-test-echo-tool (tool)
  ()
  (:documentation "Return one required string to the scripted agent provider."))

(defmethod tool-execute ((tool agent-test-echo-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the required test value without external effects."
  (declare (ignore tool context))
  (tool-success
   (format nil "echo: ~A"
           (tool-argument arguments "value" :required t))))


(defclass agent-test-read-only-echo-tool (agent-test-echo-tool)
  ()
  (:documentation "A deterministic echo tool exempt from mutating-call storm guards."))

(defmethod tool-storm-guard-exempt-p ((tool agent-test-read-only-echo-tool))
  "Exempt the deterministic read-only echo tool from storm detection."
  t)


(defclass agent-test-changing-failure-tool (tool)
  ((attempt-count
    :initform 0
    :accessor agent-test-changing-failure-tool-attempt-count
    :type (integer 0)
    :documentation "The number of deterministic failures returned so far."))
  (:documentation "Return a different failed result on each execution."))

(defmethod tool-execute ((tool agent-test-changing-failure-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the next deterministic failure text for retry-diagnosis tests."
  (declare (ignore context arguments))
  (tool-failure
   (format nil "changing failure ~D"
           (incf (agent-test-changing-failure-tool-attempt-count tool)))))

(defclass agent-test-concurrency-state ()
  ((lock
    :initform (make-lock "Autolith agent test tool state")
    :reader agent-test-concurrency-state-lock
    :documentation "The lock protecting mutable execution state.")
   (condition-variable
    :initform (make-condition-variable)
    :reader agent-test-concurrency-state-condition-variable
    :documentation "The condition used to align concurrent tool starts.")
   (active-count
    :initform 0
    :accessor agent-test-concurrency-state-active-count
    :type (integer 0)
    :documentation "The number of currently executing test tools.")
   (maximum-active-count
    :initform 0
    :accessor agent-test-concurrency-state-maximum-active-count
    :type (integer 0)
    :documentation "The largest observed concurrent execution count.")
   (overlap-observed-p
    :initform nil
    :accessor agent-test-concurrency-state-overlap-observed-p
    :type boolean
    :documentation "Whether two tool bodies executed at the same time.")
   (logical-turn-states
    :initform nil
    :accessor agent-test-concurrency-state-logical-turn-states
    :type list
    :documentation "The turn-scoped Skill state observed by each tool worker.")
   (events
    :initform nil
    :accessor agent-test-concurrency-state-events
    :type list
    :documentation "Execution start and finish events in reverse time order."))
  (:documentation "Shared state for deterministic concurrent tool tests."))

(defclass agent-test-concurrent-tool (tool)
  ((state
    :initarg :state
    :reader agent-test-concurrent-tool-state
    :type agent-test-concurrency-state
    :documentation "The shared execution state recorded by this tool.")
   (execution-policy
    :initarg :execution-policy
    :initform ':parallel
    :reader agent-test-concurrent-tool-execution-policy
    :type (member :parallel :exclusive)
    :documentation "Whether this test tool requires exclusive execution.")
   (concurrency-key
    :initarg :concurrency-key
    :initform nil
    :reader agent-test-concurrent-tool-concurrency-key
    :type t
    :documentation "The optional runtime key shared with conflicting tools."))
  (:documentation "A deterministic tool that records and aligns execution."))

(defmethod tool-execution-policy ((tool agent-test-concurrent-tool))
  "Return TOOL's configured execution policy."
  (agent-test-concurrent-tool-execution-policy tool))

(defmethod tool-concurrency-key ((tool agent-test-concurrent-tool))
  "Return TOOL's configured shared-runtime key."
  (agent-test-concurrent-tool-concurrency-key tool))

(defmethod tool-execute ((tool agent-test-concurrent-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Record one test execution, optionally aligning it with a sibling call."
  (let* ((state
           (agent-test-concurrent-tool-state tool))
         (label
           (tool-argument arguments "label" :required t))
         (delay
           (or (tool-argument arguments "delay") 0.0d0))
         (await-peer-p
           (eq (tool-argument arguments "await_peer") t))
         (fail-p
           (eq (tool-argument arguments "fail") t))
         (fatal
           (tool-argument arguments "fatal")))
    (with-lock-held ((agent-test-concurrency-state-lock state))
      (push *skill-logical-turn-state*
            (agent-test-concurrency-state-logical-turn-states state))
      (incf (agent-test-concurrency-state-active-count state))
      (setf (agent-test-concurrency-state-maximum-active-count state)
            (max (agent-test-concurrency-state-maximum-active-count state)
                 (agent-test-concurrency-state-active-count state)))
      (push (list ':start label)
            (agent-test-concurrency-state-events state))
      (when (> (agent-test-concurrency-state-active-count state) 1)
        (setf (agent-test-concurrency-state-overlap-observed-p state) t)
        (condition-notify
         (agent-test-concurrency-state-condition-variable state)))
      (when (and await-peer-p
                 (= (agent-test-concurrency-state-active-count state) 1))
        (condition-wait
         (agent-test-concurrency-state-condition-variable state)
         (agent-test-concurrency-state-lock state)
         :timeout 0.5)))
    (unwind-protect
         (progn
           (agent-observer-status
            (tool-context-observer context)
            ':agent-test-tool-callback
            (list :label label))
           (sleep delay)
           (when fail-p
             (error "requested test failure"))
           (cond
             ((equal fatal "rollback")
              (error 'rollback-requested
                     :message "requested rollback test"
                     :generation-id "test-generation"))
             ((equal fatal "corruption")
              (error
               'active-image-corruption
               :message "requested corruption test"
               :original-condition
               (make-condition 'simple-error
                               :format-control "original failure")
               :restoration-condition
               (make-condition 'simple-error
                               :format-control "restoration failure")))
             ((equal fatal "job-aborted")
              (error 'job-aborted
                     :message "requested job abort test"
                     :identifier "test-job"
                     :reason ':test)))
           (tool-success (format nil "completed: ~A" label)))
      (with-lock-held ((agent-test-concurrency-state-lock state))
        (push (list ':finish label)
              (agent-test-concurrency-state-events state))
        (decf (agent-test-concurrency-state-active-count state))))))

(-> agent-test-registry () tool-registry)
(defun agent-test-registry ()
  "Return a registry containing the deterministic echo tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-echo-tool
      :namespace "test"
      :name "echo"
      :description "Echo a test string."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))


(-> agent-test-read-only-registry () tool-registry)
(defun agent-test-read-only-registry ()
  "Return a registry containing one storm-guard-exempt echo tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-read-only-echo-tool
      :namespace "test"
      :name "inspect"
      :description "Echo a read-only test string."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))


(-> agent-test-changing-failure-registry () tool-registry)
(defun agent-test-changing-failure-registry ()
  "Return a registry containing one stateful deterministic failing tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-changing-failure-tool
      :namespace "test"
      :name "changing-failure"
      :description "Return a changing deterministic failure."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The stable retry identity."))
       '("value"))))
    registry))

(-> agent-test-restricted-registry () tool-registry)
(defun agent-test-restricted-registry ()
  "Return read-only and mutation-labelled deterministic test tools."
  (let ((registry (agent-test-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-echo-tool
      :namespace "mutation"
      :name "write"
      :description "Represent a forbidden mutation tool."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))

(-> agent-test-call
    (&key
     (:call-id (option string))
     (:namespace string)
     (:name string)
     (:arguments string))
    json-object)
(defun agent-test-call
    (&key call-id (namespace "test") (name "echo") (arguments "{}"))
  "Return a scripted function call with optional CALL-ID."
  (let ((call (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" arguments)))
    (when call-id
      (setf (gethash "call_id" call) call-id))
    call))

(-> agent-test-message (string) json-object)
(defun agent-test-message (text)
  "Return one scripted assistant message containing TEXT."
  (json-object
   "type" "message"
   "role" "assistant"
   "content" (json-array
              (json-object "type" "output_text" "text" text))))

(-> agent-test-result
    (string list
     &key
     (:turn-state (option string))
     (:turn-completion turn-completion)
     (:usage json-object))
    provider-result)
(defun agent-test-result
    (response-id output-items
     &key turn-state (turn-completion :unspecified)
       (usage (json-object "input_tokens" 1 "output_tokens" 1)))
  "Return a scripted provider result containing OUTPUT-ITEMS."
  (make-instance 'provider-result
                 :response-id response-id
                 :output-items output-items
                 :tool-calls (remove-if-not #'function-call-item-p output-items)
                 :usage usage
                 :turn-state turn-state
                 :turn-completion turn-completion))

(-> agent-test-concurrency-tool
    (agent-test-concurrency-state string
     &key (:execution-policy (member :parallel :exclusive))
          (:concurrency-key t))
    agent-test-concurrent-tool)
(defun agent-test-concurrency-tool
    (state name &key (execution-policy ':parallel) concurrency-key)
  "Create one concurrency test tool named NAME sharing STATE."
  (make-instance
   'agent-test-concurrent-tool
   :namespace "concurrency"
   :name name
   :description "Record deterministic concurrent execution."
   :parameters
   (tool-object-schema
    (json-object
     "label" (tool-string-property "The execution label.")
     "delay" (json-object "type" "number")
     "await_peer" (tool-boolean-property "Wait for one concurrent peer.")
     "fail" (tool-boolean-property "Signal a test failure.")
     "fatal" (tool-string-property
              "The optional fatal condition kind to signal."))
    '("label"))
   :state state
   :execution-policy execution-policy
   :concurrency-key concurrency-key))

(-> agent-test-concurrency-registry (list) tool-registry)
(defun agent-test-concurrency-registry (tools)
  "Return a registry containing concurrency test TOOLS."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool tools)
      (tool-registry-register registry tool))
    registry))

(-> agent-test-tool-outputs (conversation) list)
(defun agent-test-tool-outputs (conversation)
  "Return provider-visible tool outputs from CONVERSATION in durable order."
  (loop for item in (conversation-input-items conversation)
        when (string= (or (json-get item "type") "")
                      "function_call_output")
          collect (json-get item "output")))

(-> test-agent-tool-loop () null)
(defun test-agent-tool-loop ()
  "Test authoritative replay, correlated tool output, callbacks, and turn-state scope."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "agent-loop"))
         (call
           (json-object
            "type" "function_call"
            "call_id" "call-1"
            "namespace" "test"
            "name" "echo"
            "arguments" "{\"value\":\"hello\"}"))
         (blank-message (agent-test-message "   "))
         (message
           (json-object
            "type" "message"
            "role" "assistant"
            "content" (json-array
                       (json-object
                        "type" "output_text"
                        "text" "complete"))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "response-1"
              (list call blank-message)
              :turn-state "turn-state-1")
             (agent-test-result
              "response-2"
              (list message)
              :usage (json-object
                      "input_tokens" 10
                      "output_tokens" 2
                      "input_tokens_details"
                      (json-object "cached_tokens" 8
                                   "cache_write_tokens" 1))))))
         (registry (agent-test-registry))
         (deltas nil)
         (statuses nil)
         (completed-usages nil)
         (persisted-responses nil))
    (unwind-protect
         (progn
           (let* ((agent
                    (agent-create
                     :configuration configuration
                     :provider provider
                     :conversation conversation
                     :tool-registry registry
                     :worker ':unused))
                  (observer
                    (callback-agent-observer-create
                     :text-callback
                     (lambda (text)
                       (push text deltas))
                     :status-callback
                     (lambda (status details)
                       (push status statuses)
                       (when (eq status ':provider-request-completed)
                         (push (copy-tree (getf details :usage))
                               completed-usages))
                       (when (eq status ':assistant-response-persisted)
                         (let ((text (getf details :text)))
                           (push
                            (list
                             :details (copy-tree details)
                             :durable-p
                             (some
                              (lambda (item)
                                (and
                                 (json-object-p item)
                                 (string=
                                  (or (response-item-assistant-text item) "")
                                  text)))
                              (conversation-input-items conversation)))
                            persisted-responses))))))
                  (result
                    (agent-run-user-turn
                     agent "run the echo" :observer observer)))
             (test-assert
              (string= (provider-result-response-id result) "response-2")
              "the agent returns the final tool-free provider result")
             (test-assert
              (equal (nreverse (scripted-provider-input-counts provider))
                     '(1 4))
              "the second request replays the call, blank message, and tool output")
             (test-assert
              (equal (nreverse (scripted-provider-turn-states provider))
                     '(nil "turn-state-1"))
              "provider turn state is replayed only inside the active turn")
             (test-assert
              (null (conversation-turn-state conversation))
              "the agent clears request-local turn state after completion")
             (test-assert
              (= (length (conversation-input-items conversation)) 5)
              "conversation history contains user, call, blank answer, output, and answer")
             (test-assert
              (equal (nreverse deltas) '("delta" "delta"))
              "the observer receives deltas from every provider request")
             (test-assert
              (member :tool-call-completed statuses)
              "the observer receives correlated tool lifecycle status")
             (test-assert
              (member :user-message-persisted statuses)
              "the observer learns when user input becomes durable")
             (let ((usage (first completed-usages)))
               (test-assert
                (and (= (second (assoc "cached_input_tokens" usage
                                       :test #'string=))
                        8)
                     (= (second (assoc "cache_creation_input_tokens" usage
                                       :test #'string=))
                        1))
                "provider completion status carries normalized cache usage"))
             (let* ((responses (nreverse persisted-responses))
                    (response (first responses))
                    (details (getf response :details))
                    (ordered-statuses (reverse statuses))
                    (response-position
                      (position ':assistant-response-persisted ordered-statuses))
                    (completion-position
                      (position ':provider-request-completed ordered-statuses
                                :from-end t)))
               (test-assert
                (and (= (length responses) 1)
                     (= (getf details :request-number) 2)
                     (string= (getf details :response-id) "response-2")
                     (string= (getf details :text) "complete")
                     (typep (getf details :time) 'timestamp)
                     (getf response :durable-p))
                "only durable nonblank verbal provider results emit response status")
               (test-assert
                (and response-position
                     completion-position
                     (= (1+ response-position) completion-position))
                "durable verbal response status precedes request completion"))
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (tool-result
                      (find :tool-result records :key #'first))
                    (provider-record
                      (find-if
                       (lambda (record)
                         (and (eq (first record) :provider)
                              (string= (getf (getf (rest record) :metadata)
                                             :response-id)
                                       "response-2")))
                       records))
                    (persisted-usage
                      (getf (getf (rest provider-record) :metadata) :usage)))
               (test-assert
                (and (typep (getf (rest tool-result) :cpu-microseconds)
                            '(integer 0))
                     (typep (getf (rest tool-result) :real-microseconds)
                            '(integer 0)))
                "executed tool results persist CPU and real timing")
               (test-assert
                (and (= (second (assoc "cached_input_tokens" persisted-usage
                                       :test #'string=))
                        8)
                     (= (second (assoc "cache_creation_input_tokens"
                                       persisted-usage :test #'string=))
                        1))
                "provider metadata persists normalized cache usage"))
             (test-assert
              (= (count :provider-progress statuses) 2)
              "every streamed delta refreshes visible provider progress")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-free-turn () null)
(defun test-agent-tool-free-turn ()
  "Test diagnosis-style turns advertise and execute no tools."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-tool-free"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "tool-free"
              (list (agent-test-message "diagnosis only"))))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (agent-run-user-turn agent "diagnose the crash" :tools-p nil)
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0))
            "a tool-free turn advertises no tool schemas")
           (let* ((call-conversation
                    (conversation-create
                     configuration :identifier "agent-tool-free-call"))
                  (call-provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "forbidden-call"
                       (list
                        (agent-test-call
                         :call-id "forbidden-call"
                         :arguments "{\"value\":\"no\"}"))))))
                  (call-agent
                    (agent-create
                     :configuration configuration
                     :provider call-provider
                     :conversation call-conversation
                     :tool-registry (agent-test-registry)
                     :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn
                     call-agent
                     "do not call tools"
                     :tools-p nil)
                    nil)
                (agent-loop-error ()
                  t))
              "a tool-free turn rejects provider function calls")
             (test-assert
              (= (length (conversation-input-items call-conversation)) 1)
              "a rejected tool-free call is never persisted or executed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-read-only-tool-allowlist () null)
(defun test-agent-read-only-tool-allowlist ()
  "Test restricted turns advertise and execute only explicitly allowed tools."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-read-only"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "read-only-call"
              (list
               (agent-test-call
                :call-id "read-only-call"
                :arguments "{\"value\":\"inspect\"}")))
             (agent-test-result
              "read-only-answer"
              (list (agent-test-message "diagnosed"))
              :turn-completion ':end))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (let ((*agent-restricted-maximum-tool-rounds* 1))
             (agent-run-user-turn
              agent
              "diagnose safely"
              :tool-allowlist '("test.echo")
              :tool-restriction-p t))
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0 1))
            "a restricted turn removes tool schemas after its bounded round")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider)) '(1 3))
            "an allowed read-only call executes and returns to the same turn")
           (let* ((forbidden-conversation
                    (conversation-create
                     configuration :identifier "agent-read-only-forbidden"))
                  (forbidden-provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "forbidden-mutation"
                       (list
                        (agent-test-call
                         :call-id "forbidden-mutation"
                         :namespace "mutation"
                         :name "write"
                         :arguments "{\"value\":\"change\"}"))))))
                  (forbidden-agent
                    (agent-create
                     :configuration configuration
                     :provider forbidden-provider
                     :conversation forbidden-conversation
                     :tool-registry (agent-test-restricted-registry)
                     :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn
                     forbidden-agent
                     "do not mutate"
                     :tool-allowlist '("test.echo")
                     :tool-restriction-p t)
                    nil)
                (agent-loop-error ()
                  t))
              "a restricted turn rejects a non-allowlisted mutation call")
             (test-assert
              (= (length (conversation-input-items forbidden-conversation)) 1)
              "a forbidden mutation call is neither persisted nor executed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-agent-restricted-resource-schemes () null)
(defun test-agent-restricted-resource-schemes ()
  "Test restricted turns confine generic resource reads to workspace URIs."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (workspace          (merge-pathnames "restricted-workspace/" root))
         (configuration
           (configuration--clone base-configuration
                                 :working-directory workspace))
         (conversation
           (conversation-create configuration
                                :identifier "agent-restricted-resources"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "restricted-workspace-read"
              (list
               (agent-test-call
                :call-id "restricted-workspace-read"
                :namespace "resource"
                :name "read"
                :arguments
                (json-encode (json-object "uri" "workspace:allowed.txt")))))
             (agent-test-result
              "restricted-agenda-read"
              (list
               (agent-test-call
                :call-id "restricted-agenda-read"
                :namespace "resource"
                :name "read"
                :arguments
                (json-encode (json-object "uri" "agenda:current")))))
             (agent-test-result
              "restricted-memory-read"
              (list
               (agent-test-call
                :call-id "restricted-memory-read"
                :namespace "resource"
                :name "read"
                :arguments
                (json-encode (json-object "uri" "memory:relevant")))))
             (agent-test-result
              "restricted-resource-answer"
              (list (agent-test-message "diagnosed"))
              :turn-completion ':end))))
         (registry (make-default-tool-registry))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry registry
                         :worker ':unused)))
    (unwind-protect
         (progn
           (ensure-directories-exist workspace)
           (with-open-file (stream (merge-pathnames "allowed.txt" workspace)
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "restricted workspace content" stream))
           (let ((*agent-restricted-maximum-tool-rounds* 3))
             (agent-run-user-turn
              agent
              "diagnose within the workspace"
              :tool-allowlist '("resource.read")
              :tool-restriction-p t))
           (let ((outputs
                   (loop for item in (conversation-input-items conversation)
                         when (string= (or (json-get item "type") "")
                                       "function_call_output")
                           collect (json-get item "output"))))
             (test-assert
              (and (= (length outputs) 3)
                   (search "restricted workspace content" (first outputs)))
              "a restricted turn may read workspace resources")
             (test-assert
              (and (search "agenda:current" (second outputs))
                   (search "unavailable under this authority context"
                           (second outputs)))
              "a restricted turn rejects agenda resources")
             (test-assert
              (and (search "memory:relevant" (third outputs))
                   (search "unavailable under this authority context"
                           (third outputs)))
              "a restricted turn rejects memory resources")))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-restricted-tool-round-limit () null)
(defun test-agent-restricted-tool-round-limit ()
  "Test restricted turns reject calls returned after their bounded tool rounds."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-tool-limit"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "tool-limit-first"
              (list
               (agent-test-call
                :call-id "tool-limit-first"
                :arguments "{\"value\":\"first\"}")))
             (agent-test-result
              "tool-limit-second"
              (list
               (agent-test-call
                :call-id "tool-limit-second"
                :arguments "{\"value\":\"second\"}"))))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker ':unused)))
    (unwind-protect
         (let ((*agent-restricted-maximum-tool-rounds* 1))
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn
                   agent
                   "inspect once"
                   :tool-allowlist '("test.echo")
                   :tool-restriction-p t)
                  nil)
              (agent-loop-error (condition)
                (search "tool-round limit" (format nil "~A" condition))))
            "a restricted turn rejects calls beyond its tool-round limit")
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0 1))
            "the provider sees no tool schemas after the restricted limit")
           (test-assert
            (= (length (conversation-input-items conversation)) 3)
            "the over-limit call is rejected before persistence or execution"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-empty-tool-allowlist () null)
(defun test-agent-empty-tool-allowlist ()
  "Test an explicit empty restriction advertises and executes no tools."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-empty-tools"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "empty-allowlist-call"
              (list
               (agent-test-call
                :call-id "empty-allowlist-call"
                :arguments "{\"value\":\"blocked\"}"))))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn
                   agent
                   "inspect nothing"
                   :tool-allowlist nil
                   :tool-restriction-p t)
                  nil)
              (agent-loop-error ()
                t))
            "an explicit empty restriction rejects every function call")
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0))
            "an explicit empty restriction advertises zero namespaces")
           (test-assert
            (= (length (conversation-input-items conversation)) 1)
            "an empty restriction persists no rejected call or tool result"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-steering () null)
(defun test-agent-steering ()
  "Test pending user input is persisted after a tool round and before its follow-up."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-steering"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "steering-1"
              (list (agent-test-call
                     :call-id "steering-call"
                     :arguments "{\"value\":\"before\"}"))
              :turn-state "transient-turn-state")
             (agent-test-result
              "steering-2"
              (list (agent-test-message "changed course"))
              :turn-completion ':end))))
          (pending-input
            (list
             (agent-steering-input-create
              :identifier "steering-persisted"
              :content "change direction")))
          (persisted-identifiers nil)
          (statuses nil))
    (unwind-protect
         (let* ((agent
                  (agent-create :configuration configuration
                                :provider provider
                                :conversation conversation
                                :tool-registry (agent-test-registry)
                                :worker nil))
                (observer
                  (callback-agent-observer-create
                   :steering-callback
                   (lambda ()
                     (prog1 pending-input
                       (setf pending-input nil)))
                   :steering-persisted-callback
                   (lambda (identifier)
                     (push identifier persisted-identifiers))
                   :status-callback
                   (lambda (status details)
                     (declare (ignore details))
                     (push status statuses)))))
           (agent-run-user-turn agent "start here" :observer observer)
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider))
                   '(1 4))
            "steering follows the function call and correlated tool output")
           (test-assert
            (equal (nreverse (scripted-provider-turn-states provider))
                   '(nil nil))
            "new steering invalidates the request-local provider turn state")
           (let ((user-records
                   (loop for record in
                           (rest (conversation--read-records
                                  (conversation-pathname conversation)))
                         when (and (eq (first record) ':message)
                                   (eq (getf (rest record) :role) ':user))
                           collect record)))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :content))
                             user-records)
                     '("start here" "change direction"))
              "steering is durable ordinary user input")
             (test-assert
              (string= (getf (rest (second user-records))
                             :pending-input-identifier)
                       "steering-persisted")
              "identified steering records its pending provenance"))
           (test-assert
            (equal persisted-identifiers '("steering-persisted"))
            "the observer acknowledges each steering append immediately")
           (test-assert (member :steering-applied statuses)
                        "the observer is notified after steering becomes durable"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-explicit-continuation () null)
(defun test-agent-explicit-continuation ()
  "Test a tool-free explicit continuation receives another bounded request."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "agent-continue"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list (agent-test-result "response-1"
                                     (list (agent-test-message "working"))
                                     :turn-state "continuation-state"
                                     :turn-completion ':continue)
                  (agent-test-result "response-2"
                                     (list (agent-test-message "done"))
                                     :turn-completion ':end)))))
    (unwind-protect
         (let* ((agent (agent-create
                        :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry (agent-test-registry)
                        :worker ':unused))
                (result (agent-run-user-turn agent "continue explicitly")))
           (test-assert (string= (provider-result-response-id result) "response-2")
                        "the agent follows an explicit provider continuation")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider)) '(1 2))
            "the continuation request replays the first completed message")
           (test-assert
            (equal (nreverse (scripted-provider-turn-states provider))
                   '(nil "continuation-state"))
            "the continuation request receives request-local routing state")
           (test-assert (null (conversation-turn-state conversation))
                        "explicit continuation state is cleared after the user turn"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-request-limit () null)
(defun test-agent-provider-request-limit ()
  "Test an ordinary turn stops before an unbounded provider continuation loop."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-request-limit"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (loop for index from 1 to 3
                  collect
                  (agent-test-result
                   (format nil "response-~D" index)
                   (list (agent-test-message "still working"))
                   :turn-completion ':continue)))))
    (unwind-protect
         (let* ((agent
                  (agent-create
                   :configuration configuration
                   :provider provider
                   :conversation conversation
                   :tool-registry (agent-test-registry)
                   :worker ':unused))
                (condition nil)
                (*agent-maximum-provider-requests-per-turn* 2))
           (handler-case
               (agent-run-user-turn agent "keep continuing")
             (agent-loop-error (caught)
               (setf condition caught)))
           (test-assert
            (and condition
                 (= (agent-loop-error-request-number condition) 2))
            "the request safety limit reports the completed request count")
           (test-assert
            (= (length (scripted-provider-input-snapshots provider)) 2)
            "the request safety limit prevents another paid provider call"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-invalid-call-history () null)
(defun test-agent-invalid-call-history ()
  "Test uncorrelatable and duplicate calls cannot poison durable history."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration :identifier "missing-call-id"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "missing-id"
                       (list (agent-test-call :arguments "{\"value\":\"x\"}"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reject missing id")
                    nil)
                (agent-loop-error ()
                  t))
              "a call without correlation identity is rejected")
             (test-assert (= (length (conversation-input-items conversation)) 1)
                          "an uncorrelatable provider item is never persisted"))
           (let* ((conversation
                    (conversation-create configuration :identifier "duplicate-call-id"))
                  (first-call
                    (agent-test-call :call-id "duplicate"
                                     :arguments "{\"value\":\"first\"}"))
                  (duplicate-call
                    (agent-test-call :call-id "duplicate"
                                     :arguments "{\"value\":\"second\"}"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list (agent-test-result "first" (list first-call)
                                              :turn-state "duplicate-state")
                           (agent-test-result "duplicate" (list duplicate-call)))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reject duplicate id")
                    nil)
                (agent-loop-error ()
                  t))
              "a repeated call identity is rejected before persistence")
             (test-assert (= (length (conversation-input-items conversation)) 3)
                          "only the first call and its correlated output remain")
             (test-assert (null (conversation-turn-state conversation))
                          "turn state clears after a duplicate-call invariant failure")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-malformed-tool-arguments () null)
(defun test-agent-malformed-tool-arguments ()
  "Test malformed tool arguments fail safely and legacy poison is repaired on replay."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (malformed     "{\"value\":\"first\"}{\"value\":\"second\"}"))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "malformed-tool-arguments"))
                  (call
                    (agent-test-call
                     :call-id "malformed-call" :arguments malformed))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result "malformed-call" (list call))
                      (agent-test-result
                       "malformed-final"
                       (list (agent-test-message "malformed call handled"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused))
                  (result (agent-run-user-turn agent "handle malformed arguments"))
                  (snapshots
                    (nreverse (scripted-provider-input-snapshots provider)))
                  (replayed-call
                    (find-if #'function-call-item-p (second snapshots)))
                  (outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (string= (provider-result-response-id result) "malformed-final")
              "malformed arguments do not stop the provider tool loop")
             (test-assert
              (string= (json-get replayed-call "arguments") "{}")
              "the next provider request receives replayable call arguments")
             (test-assert
              (and (= (length outputs) 1)
                   (search "not a valid JSON object" (first outputs)))
              "malformed arguments receive one correlated failed tool result")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (provider-record
                      (find ':provider-item records :key #'first))
                    (persisted-call
                      (json-decode (getf (rest provider-record) :wire-json))))
               (test-assert
                (string= (json-get persisted-call "arguments") "{}")
                "new malformed calls are sanitized before durable persistence")))
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "legacy-malformed-tool-arguments"))
                  (call
                    (agent-test-call
                     :call-id "legacy-malformed-call" :arguments malformed)))
             (conversation-append-user-message conversation "legacy history")
             (conversation-append-record
              conversation
              (list :provider-item :wire-json (json-encode call)))
             (conversation-append-tool-result
              conversation
              "legacy-malformed-call"
              :tool-name "test.echo"
              :output "The legacy call was rejected."
              :success-p nil)
             (test-assert
              (handler-case
                  (progn
                    (conversation-append-provider-item conversation call)
                    nil)
                (conversation-invariant-error ()
                  t))
              "the durable provider-item boundary rejects malformed arguments")
             (let* ((pathname (conversation-pathname conversation))
                    (reloaded
                      (conversation-load-by-id
                       configuration "legacy-malformed-tool-arguments"))
                    (replayed-call
                      (find-if #'function-call-item-p
                               (conversation-input-items reloaded)))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (list
                        (agent-test-result
                         "legacy-recovered"
                         (list (agent-test-message "legacy history recovered"))))))
                    (agent
                      (agent-create :configuration configuration
                                    :provider provider
                                    :conversation reloaded
                                    :tool-registry (agent-test-registry)
                                    :worker ':unused)))
               (test-assert
                (string= (json-get replayed-call "arguments") "{}")
                "loading legacy history repairs malformed call arguments in memory")
               (let* ((records (conversation--read-records pathname))
                      (provider-record
                        (find ':provider-item records :key #'first))
                      (persisted-call
                        (json-decode (getf (rest provider-record) :wire-json))))
                 (test-assert
                  (string= (json-get persisted-call "arguments") malformed)
                  "legacy repair leaves the append-only record unchanged"))
               (agent-run-user-turn agent "continue recovered history")
               (let* ((snapshot
                        (first (scripted-provider-input-snapshots provider)))
                      (provider-call (find-if #'function-call-item-p snapshot)))
                 (test-assert
                  (string= (json-get provider-call "arguments") "{}")
                  "recovered legacy history is replayable on the next request")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-agent-tool-storm-guard () null)
(defun test-agent-tool-storm-guard ()
  "Test canonical repetition and oscillation guards while read-only calls remain exempt."
  (let* ((*agent-tool-storm-identical-call-limit* 3)
         (configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((left
                   (agent-test-call
                    :arguments
                    "{\"second\":1.0,\"first\":{\"b\":2,\"a\":1}}"))
                 (right
                   (agent-test-call
                    :arguments
                    "{\"first\":{\"a\":1.0,\"b\":2},\"second\":1}")))
             (test-assert
              (equalp (agent--tool-call-signature left)
                      (agent--tool-call-signature right))
              "storm signatures canonicalize object order and numeric representation"))
           (flet ((tool-result-records (conversation)
                    (remove-if-not
                     (lambda (record)
                       (and (listp record)
                            (eq (first record) ':tool-result)))
                     (conversation--read-records
                      (conversation-pathname conversation)))))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-repetition"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (list
                        (agent-test-result
                         "repeat-1"
                         (list (agent-test-call
                                :call-id "repeat-1"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-2"
                         (list (agent-test-call
                                :call-id "repeat-2"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-3"
                         (list (agent-test-call
                                :call-id "repeat-3"
                                :arguments "{\"value\":\"same\"}")))
                         (agent-test-result
                          "repeat-4"
                          (list (agent-test-call
                                 :call-id "repeat-4"
                                 :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-done"
                         (list (agent-test-message "repetition handled"))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider provider
                       :conversation conversation
                       :tool-registry (agent-test-registry)
                       :worker ':unused)))
               (agent-run-user-turn agent "repeat a mutating call")
               (let ((outputs (agent-test-tool-outputs conversation))
                     (records (tool-result-records conversation)))
                 (test-assert
                  (equal (subseq outputs 0 2)
                         '("echo: same" "echo: same"))
                  "the first two identical mutating calls execute")
                  (test-assert
                   (and (= (length outputs) 4)
                        (search "withheld repeated call" (third outputs)))
                   "the third identical mutating call is withheld")
                  (test-assert
                   (search "second consecutive withheld call" (fourth outputs))
                   "consecutive withholds escalate their model-visible diagnosis")
                  (test-assert
                   (equal (mapcar (lambda (record)
                                    (getf (rest record) :category))
                                  records)
                          '(:success :success :mechanics :mechanics))
                   "withheld calls persist as mechanics rather than failures")))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-oscillation"))
                    (arguments
                      '("{\"value\":\"a\"}"
                        "{\"value\":\"b\"}"
                        "{\"value\":\"a\"}"
                        "{\"value\":\"b\"}"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (append
                        (loop for source in arguments
                              for index from 1
                              collect
                              (agent-test-result
                               (format nil "oscillate-~D" index)
                               (list (agent-test-call
                                      :call-id (format nil "oscillate-~D" index)
                                      :arguments source))))
                        (list
                         (agent-test-result
                          "oscillate-done"
                          (list (agent-test-message "oscillation handled")))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider provider
                       :conversation conversation
                       :tool-registry (agent-test-registry)
                       :worker ':unused)))
               (agent-run-user-turn agent "oscillate between mutating calls")
               (let ((outputs (agent-test-tool-outputs conversation)))
                 (test-assert
                  (and (= (length outputs) 4)
                       (search "A-B-A-B oscillation" (fourth outputs)))
                  "the fourth alternating mutating call is withheld")))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-read-only"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (append
                        (loop for index from 1 to 4
                              collect
                              (agent-test-result
                               (format nil "read-only-~D" index)
                               (list (agent-test-call
                                      :call-id (format nil "read-only-~D" index)
                                      :name "inspect"
                                      :arguments "{\"value\":\"same\"}"))))
                        (list
                         (agent-test-result
                          "read-only-done"
                          (list (agent-test-message "inspection complete")))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider provider
                       :conversation conversation
                       :tool-registry (agent-test-read-only-registry)
                       :worker ':unused)))
               (agent-run-user-turn agent "repeat a read-only call")
               (test-assert
                (equal (agent-test-tool-outputs conversation)
                       '("echo: same" "echo: same" "echo: same" "echo: same"))
                "repeated read-only calls remain executable"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-agent-tool-retry-guidance () null)
(defun test-agent-tool-retry-guidance ()
  "Test missing-argument skeletons and inherited diagnoses for changed failures."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "tool-missing-argument-skeleton"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "missing-argument"
                       (list (agent-test-call
                              :call-id "missing-argument"
                              :arguments "{}")))
                      (agent-test-result
                       "complete-argument"
                       (list (agent-test-call
                              :call-id "complete-argument"
                              :arguments "{\"value\":\"fixed\"}")))
                      (agent-test-result
                       "argument-done"
                       (list (agent-test-message "argument repaired"))))))
                  (agent
                    (agent-create
                     :configuration configuration
                     :provider provider
                     :conversation conversation
                     :tool-registry (agent-test-registry)
                     :worker ':unused)))
             (agent-run-user-turn agent "repair missing arguments")
             (let* ((outputs (agent-test-tool-outputs conversation))
                    (records
                      (remove-if-not
                       (lambda (record)
                         (and (listp record)
                              (eq (first record) ':tool-result)))
                       (conversation--read-records
                        (conversation-pathname conversation)))))
               (test-assert
                (and (= (length outputs) 2)
                     (search "required arguments are missing: value"
                             (first outputs))
                     (search "Call skeleton:" (first outputs))
                     (search "\"value\":\"\"" (first outputs)))
                "a missing required argument returns a concrete retry skeleton")
               (test-assert
                (string= (second outputs) "echo: fixed")
                "the corrected call executes on the following provider round")
               (test-assert
                (eq (getf (rest (first records)) :category) ':mechanics)
                "missing required arguments are mechanics rather than failures")))
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "tool-retry-diagnosis"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "failure-1"
                       (list (agent-test-call
                              :call-id "failure-1"
                              :name "changing-failure"
                              :arguments "{\"value\":\"same\"}")))
                      (agent-test-result
                       "failure-2"
                       (list (agent-test-call
                              :call-id "failure-2"
                              :name "changing-failure"
                              :arguments "{\"value\":\"same\"}")))
                      (agent-test-result
                       "failure-done"
                       (list (agent-test-message "failures diagnosed"))))))
                  (agent
                    (agent-create
                     :configuration configuration
                     :provider provider
                     :conversation conversation
                     :tool-registry (agent-test-changing-failure-registry)
                     :worker ':unused)))
             (agent-run-user-turn agent "retry the same failed call")
             (let ((outputs (agent-test-tool-outputs conversation)))
               (test-assert
                (string= (first outputs) "changing failure 1")
                "the initial failure is returned without inherited diagnosis")
               (test-assert
                (and (search "changing failure 2" (second outputs))
                     (search "Previous failure for this exact call: changing failure 1"
                             (second outputs)))
                "a changed exact-call failure quotes the previous diagnosis"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-failures () null)
(defun test-agent-tool-failures ()
  "Test successful and failed calls retain independent correlation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration :identifier "mixed-tools"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "tool-batch"
                       (list
                        (agent-test-call :call-id "good"
                                         :arguments "{\"value\":\"ok\"}")
                        (agent-test-call :call-id "bad"
                                         :namespace "missing"
                                         :name "tool")))
                      (agent-test-result "tool-final"
                                         (list (agent-test-message "finished"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (agent-run-user-turn agent "run mixed tools")
             (test-assert (= (length (conversation-input-items conversation)) 6)
                          "multiple calls each receive one correlated output")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (tool-results
                      (remove-if-not
                       (lambda (record)
                         (and (listp record) (eq (first record) :tool-result)))
                       records)))
               (test-assert
                (equal (mapcar (lambda (record) (getf (rest record) :status))
                               tool-results)
                       '(:ok :error))
                "successful and failed calls remain explicitly distinguished"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-failure-persistence () null)
(defun test-agent-provider-failure-persistence ()
  "Test a terminal provider failure is durable but absent from model replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-failure"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition
              'provider-error
              :message "The provider rejected the prompt."
              :status 400
              :code "invalid_prompt"
              :request-id "request-invalid"
              :response-id "response-invalid"
              :response nil))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn agent "persist this failure")
                  nil)
              (provider-error ()
                t))
            "terminal provider failures still reach the caller")
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (provider-record
                    (find-if (lambda (record)
                               (eq (first record) ':provider))
                             records))
                  (metadata (getf (rest provider-record) :metadata))
                  (failure (getf metadata :failure)))
             (test-assert (= (getf metadata :request-number) 1)
                          "provider failure metadata retains its request number")
             (test-assert (string= (getf failure :code) "invalid_prompt")
                          "provider failure metadata retains its error code")
             (test-assert (null (getf failure :incomplete-reason))
                          "ordinary provider failures have no incomplete reason")
             (test-assert
              (string= (getf failure :request-id) "request-invalid")
              "provider failure metadata retains its request identifier")
             (test-assert
              (string= (getf failure :response-id) "response-invalid")
              "provider failure metadata retains its response identifier")
             (test-assert (null (getf failure :retryable-p))
                          "terminal failure metadata is not marked retryable"))
           (let ((reloaded
                   (conversation-load-by-id configuration "provider-failure")))
             (test-assert (= (length (conversation-input-items reloaded)) 1)
                          "provider failure metadata stays outside model replay")
             (test-assert
              (string= (json-get (first (conversation-input-items reloaded))
                                 "role")
                       "user")
              "replayed input retains the user message that failed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-incomplete-provider-failure-persistence () null)
(defun test-agent-incomplete-provider-failure-persistence ()
  "Test incomplete provider reasons survive durable failure recording."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration
            :identifier "provider-incomplete-failure"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition
              'provider-incomplete-response
              :message
              "The provider returned an incomplete response (max_output_tokens)."
              :reason "max_output_tokens"
              :status nil
              :code "response_incomplete"
              :request-id "request-incomplete"
              :response-id "response-incomplete"
              :response nil))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn agent "persist this incomplete failure")
                  nil)
              (provider-incomplete-response ()
                t))
            "incomplete provider failures still reach the caller")
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (provider-record
                    (find-if (lambda (record)
                               (eq (first record) ':provider))
                             records))
                  (metadata (getf (rest provider-record) :metadata))
                  (failure (getf metadata :failure)))
             (test-assert
              (string= (getf failure :code) "response_incomplete")
              "incomplete failure metadata retains its stable error code")
             (test-assert
              (string= (getf failure :incomplete-reason) "max_output_tokens")
              "incomplete failure metadata retains its structured reason")
              (test-assert (null (getf failure :retryable-p))
                           "incomplete failure metadata is terminal")
             (test-assert
              (string= (getf failure :request-id) "request-incomplete")
              "incomplete failure metadata retains its request identifier")
             (test-assert
              (string= (getf failure :response-id) "response-incomplete")
              "incomplete failure metadata retains its response identifier")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-credential-failure-containment () null)
(defun test-agent-provider-credential-failure-containment ()
  "Test provider credential echoes cannot reach durable failure metadata."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration
            :identifier "provider-secret-failure"))
         (provider (provider-create configuration))
         (credentials (provider-tests--credentials configuration))
         (secrets (oauth-credentials-secret-values credentials))
         (source
           (test-sse-event-string
            (json-object
             "type" "response.failed"
             "response"
             (json-object
              "id" (oauth-credentials-access-token credentials)
              "error"
              (json-object
               "code" "invalid_prompt"
               "message"
               (format
                nil
                "~A/~A"
                (oauth-credentials-refresh-token credentials)
                (oauth-credentials-id-token credentials))
               "request_id"
               (oauth-credentials-account-id credentials))))))
         (agent
           (agent-create
            :configuration configuration
            :provider provider
            :conversation conversation
            :tool-registry (agent-test-registry)
            :worker ':unused)))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source
             (provider-credential-manager provider))
            credentials)
           (test-assert
            (handler-case
                (test-call-with-function-replacements
                 (list
                  (list
                   'provider-open-response-stream
                   (lambda (active-provider request &rest arguments)
                     (declare
                      (ignore active-provider request arguments))
                     (values
                      (make-instance
                       'test-character-input-stream
                       :source source)
                      200
                      nil))))
                 (lambda ()
                   (agent-run-user-turn
                    agent
                    "persist a provider credential echo")))
              (provider-error ()
                t))
            "a credential-echoing provider failure reaches the caller")
            (let ((records nil))
              (conversation-map-records
               conversation
               (lambda (record)
                 (push record records)))
              (setf records (nreverse records))
              (let* ((text
                       (with-output-to-string (stream)
                         (dolist (segment
                                  (conversation-storage-pathnames
                                   (conversation-pathname conversation)))
                           (write-string (uiop:read-file-string segment) stream))))
                     (provider-record
                       (find-if
                        (lambda (record)
                          (eq (first record) ':provider))
                        records))
                     (failure
                       (getf
                        (getf (rest provider-record) :metadata)
                        :failure)))
                (provider-tests--assert-credential-free
                 (list records text)
                 secrets
                 "durable provider failure state contains no credential")
                (test-assert
                 (and
                  (string= (getf failure :code) "invalid_prompt")
                  (test-object-contains-string-p
                   failure
                   *provider-credential-redaction-marker*)
                  (search *provider-credential-redaction-marker* text))
                 "durable provider failure metadata retains sanitized diagnostics"))))
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-long-tool-turn () null)
(defun test-agent-long-tool-turn ()
  "Test a useful turn may execute more than eight tool batches before completion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "long-turn"))
         (tool-results
           (loop for index from 1 to 12
                 collect (agent-test-result
                          (format nil "tool-~D" index)
                          (list (agent-test-call
                                 :call-id (format nil "call-~D" index)
                                 :arguments (format nil "{\"value\":\"~D\"}" index))))))
         (provider
           (make-instance
            'scripted-provider
            :results (append tool-results
                             (list (agent-test-result
                                    "long-final"
                                    (list (agent-test-message "done")))))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                      (agent-run-user-turn agent "perform a long task"))
                     "long-final")
            "a twelve-batch turn completes without a fixed eight-round cutoff")
           (test-assert
            (= (count-if (lambda (record)
                           (eq (first record) :tool-result))
                         (conversation--read-records
                          (conversation-pathname conversation)))
               12)
            "every long-turn tool call receives a durable correlated output"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-unbounded-tool-calls () null)
(defun test-agent-unbounded-tool-calls ()
  "Test one turn may exceed the former cumulative tool-call ceiling."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "unbounded-tools"))
         (tool-results
           (loop for index from 1 to 257
                 collect
                 (agent-test-result
                  (format nil "tool-~D" index)
                  (list
                   (agent-test-call
                    :call-id (format nil "call-~D" index)
                    :arguments (format nil "{\"value\":\"~D\"}" index))))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (append tool-results
                    (list
                     (agent-test-result
                      "large-tool-final"
                      (list (agent-test-message "done")))))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                     (agent-run-user-turn agent "run every requested tool"))
                     "large-tool-final")
            "a turn exceeding 256 calls reaches its normal completion")
           (test-assert
            (= (count-if (lambda (record)
                           (eq (first record) :tool-result))
                         (conversation--read-records
                          (conversation-pathname conversation)))
               257)
            "every call above the former ceiling receives a durable result"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-default-turn-has-no-step-guillotine () null)
(defun test-agent-default-turn-has-no-step-guillotine ()
  "Test the default turn may keep working beyond the former provider-step limit."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "unbounded-turn"))
         (continuations
           (loop for index from 1 below 64
                 collect
                 (agent-test-result
                  (format nil "continue-~D" index)
                  (list (agent-test-message "Still working."))
                  :turn-completion ':continue)))
         (provider
           (make-instance
            'scripted-provider
            :results
            (append continuations
                    (list
                     (agent-test-result
                      "step-64-tool"
                      (list (agent-test-call :call-id "late-tool"
                                             :arguments "{\"value\":\"late\"}")))
                     (agent-test-result
                      "step-65-final"
                      (list (agent-test-message "Done."))
                      :turn-completion ':end)))))
         (agent
           (agent-create :configuration configuration
                         :provider provider
                         :conversation conversation
                         :tool-registry (agent-test-registry)
                         :worker ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                      (agent-run-user-turn agent "finish a long task"))
                     "step-65-final")
            "the default turn continues past provider step 64")
           (test-assert
            (every #'plusp
                   (scripted-provider-tool-schema-counts provider))
            "tools stay available throughout the default turn")
           (test-assert
            (find "late-tool"
                  (conversation--read-records
                   (conversation-pathname conversation))
                  :key (lambda (record)
                         (and (listp record)
                              (eq (first record) :tool-result)
                              (getf (rest record) :call-id)))
                  :test #'string=)
            "the tool requested on provider step 64 executes normally"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-skill-provider-barrier () null)
(defun test-agent-skill-provider-barrier ()
  "Test skill.load correlation, persistence, and same-result action blocking."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skill-root (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "agent-skill-barrier"))
         (before-call
           (agent-test-call
            :call-id "before-call"
            :arguments "{\"value\":\"before\"}"))
         (skill-call
           (agent-test-call
            :call-id "skill-call"
            :namespace "skill"
            :name "load"
            :arguments "{\"name\":\"alpha\"}"))
         (after-call
           (agent-test-call
            :call-id "after-call"
            :arguments "{\"value\":\"after\"}"))
         (provider
           (make-instance
            'scripted-provider
            :configuration configuration
            :results
            (list
             (agent-test-result
              "skill-barrier-1"
              (list before-call skill-call after-call))
             (agent-test-result
              "skill-barrier-2"
              (list (agent-test-message "Applied selected instructions."))
              :turn-completion ':end))))
         (registry
           (skill-augment-tool-registry (agent-test-registry)))
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :tool-registry registry
                          :ui ui))
         (observer
           (application-agent-observer
            application
            :user-message-input
            (user-message-input-create
             :text "Select the relevant Skill, then continue."))))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (recording-terminal-reset terminal)
           (skill-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "Apply the barrier test instructions."
             "BARRIER-SKILL-INSTRUCTIONS"))
           (agent-run-user-turn
            (agent-create :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry registry
                          :worker nil)
            "Select the relevant Skill, then continue."
            :observer observer)
           (let ((terminal-output (recording-terminal-output terminal)))
             (test-assert
              (and (search "◆ loaded skill: alpha" terminal-output)
                   (null (search "✓ skill.load" terminal-output)))
              "the real Skill result path emits one compact transcript marker"))
           (let* ((snapshots
                    (reverse
                     (scripted-provider-input-snapshots provider)))
                  (second-request (second snapshots))
                  (outputs
                    (loop for item in second-request
                          when (string=
                                (or (json-get item "type") "")
                                "function_call_output")
                            collect item))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record-source
                    (with-output-to-string (stream)
                      (prin1 records stream))))
             (test-assert
              (equal
               (reverse
                (scripted-provider-skill-selection-snapshots provider))
               '(nil ("alpha")))
              "skill.load selection remains active at the required provider boundary")
             (test-assert
              (equal
               (second
                (reverse
                 (scripted-provider-skill-contribution-snapshots provider)))
               '("skill-catalog" "skill-selected-alpha"))
              "the provider retry receives the selected instructions before more actions")
             (test-assert
              (and (= (length second-request) 7)
                   (equal
                    (mapcar
                     (lambda (item)
                       (json-get item "call_id"))
                     (rest second-request))
                    '("before-call"
                      "skill-call"
                      "after-call"
                      "before-call"
                      "skill-call"
                      "after-call")))
              "mixed durable and request-local calls preserve provider wire order")
             (test-assert
              (and (= (length outputs) 3)
                   (search "echo: before"
                           (json-get (first outputs) "output"))
                   (search "Selected skill alpha"
                           (json-get (second outputs) "output"))
                   (search "preceding tool requires a provider round trip"
                           (json-get (third outputs) "output"))
                   (null (search "echo: after"
                                 (json-get (third outputs) "output"))))
              "calls after skill.load are explicitly deferred instead of executed")
             (test-assert
              (and (null (search "skill-call" record-source))
                   (null (search "after-call" record-source))
                   (null (search "BARRIER-SKILL-INSTRUCTIONS" record-source)))
              "Skill selection correlation and instruction text never enter durable history")
             (test-assert
              (and (= (length (conversation-input-items conversation)) 4)
                   (null (conversation-ephemeral-input-entries conversation)))
              "the next successful provider response consumes ephemeral correlation")
             (let* ((reloaded
                      (conversation-load-by-id
                       configuration
                       "agent-skill-barrier"))
                    (replay-terminal
                      (make-instance 'recording-terminal :columns 80))
                    (replay-ui
                      (terminal-ui-create :terminal replay-terminal))
                    (replay-application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation reloaded
                                     :tool-registry registry
                                     :ui replay-ui)))
               (test-assert
                (and (= (length (conversation-input-items reloaded)) 4)
                     (find "before-call"
                           (conversation-input-items reloaded)
                           :key (lambda (item)
                                  (json-get item "call_id"))
                           :test #'string=)
                     (null
                      (find "skill-call"
                            (conversation-input-items reloaded)
                            :key (lambda (item)
                                   (json-get item "call_id"))
                            :test #'string=)))
                "crash replay retains only the complete durable call pair")
               (unwind-protect
                    (progn
                      (terminal-ui-start replay-ui)
                      (recording-terminal-reset replay-terminal)
                      (application-render-records replay-application)
                      (let ((replay-output
                              (recording-terminal-output replay-terminal)))
                        (test-assert
                         (and (null (search "◆ loaded skill" replay-output))
                              (null (search "BARRIER-SKILL-INSTRUCTIONS"
                                            replay-output)))
                         "conversation reload does not replay Skill presentation or instructions")))
                 (ignore-errors (terminal-ui-stop replay-ui))))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-compaction-missing-summary () null)
(defun test-agent-compaction-missing-summary ()
  "Test empty compaction output is a recoverable provider protocol failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "agent-empty-compaction"))
                (provider
                  (make-instance
                   'scripted-provider
                   :results
                   (list (agent-test-result "compact-empty" nil
                                            :turn-completion ':end))))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry (agent-test-registry)
                                     :worker nil))
                (records-before
                  (conversation--read-records
                   (conversation-pathname conversation)))
                (failure
                  (handler-case
                      (progn
                        (agent-compact-conversation
                         agent (make-instance 'agent-observer))
                        nil)
                    (provider-protocol-error (condition)
                      condition))))
           (test-assert
            (and failure
                 (string= (autolith-error-message failure)
                          "Compaction produced no summary text.")
                 (string= (provider-error-response-id failure)
                          "compact-empty")
                 (null (provider-error-status failure))
                 (null (provider-error-code failure))
                 (null (provider-error-request-id failure))
                 (null (provider-error-response failure)))
            "empty compaction output is a structured provider protocol error")
           (test-assert
            (equal records-before
                   (conversation--read-records
                    (conversation-pathname conversation)))
            "failed compaction persists no partial summary state"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-compaction () null)
(defun test-agent-compaction ()
  "Test threshold-triggered compaction through the scripted provider."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier
                                                   "agent-compaction"))
                (summary-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object
                               "type" "output_text"
                               "text" "Summary: earlier work is complete."))))
                (answer-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text"
                                           "text" "Done."))))
                (provider
                  (make-instance
                   'scripted-provider
                   :results (list (agent-test-result "compact-1"
                                                     (list summary-item)
                                                     :turn-completion ':end)
                                  (agent-test-result "turn-1"
                                                     (list answer-item)
                                                     :turn-completion ':end))))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry (agent-test-registry)
                                     :worker nil)))
           (conversation-append-user-message conversation "earlier context")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "seed"
                  :usage '(("total_tokens" 999999))))
           (agent-run-user-turn agent "hello")
           (test-assert (equal (reverse
                                (scripted-provider-compaction-flags provider))
                               '(t nil))
                        "the compaction request precedes the user request")
           (test-assert (equal (reverse
                                (scripted-provider-input-counts provider))
                               '(1 2))
                        "the user question survives compaction verbatim")
           (test-assert (find :summary
                              (rest (conversation--read-records
                                     (conversation-pathname conversation)))
                              :key #'first)
                        "compaction persists one durable summary record")
           (test-assert (search "A previous segment"
                                (json-get
                                 (aref (json-get
                                        (first (conversation-input-items
                                                conversation))
                                        "content")
                                       0)
                                 "text"))
                        "the live projection starts from the summary bridge"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-native-compaction () null)
(defun test-agent-native-compaction ()
  "Test that an opaque native checkpoint supplements the durable handoff."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "agent-native-compact"))
                (summary-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object
                               "type" "output_text"
                               "text" "Portable compaction handoff."))))
                 (earlier-item
                   (json-object
                    "type" "message"
                    "role" "assistant"
                    "content" (json-array
                               (json-object "type" "output_text"
                                            "text" "Earlier response."))))
                (answer-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text"
                                           "text" "Done."))))
                (provider
                  (make-instance
                   'native-scripted-provider
                   :native-items
                   (list (json-object "type" "compaction"
                                      "encrypted_content" "native-checkpoint"))
                   :results (list (agent-test-result "compact-native"
                                                     (list summary-item)
                                                     :turn-completion ':end)
                                  (agent-test-result "turn-native"
                                                     (list answer-item)
                                                     :turn-completion ':end))))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry (agent-test-registry)
                                     :worker nil)))
            (conversation-append-user-message conversation "earlier context")
            (conversation-append-provider-item conversation earlier-item)
            (conversation-append-provider-metadata
             conversation
             (list :request-number 1
                   :response-id "seed-native"
                   :usage '(("total_tokens" 999999))))
            (agent-run-user-turn agent "hello")
            (test-assert
             (= (length
                 (first
                  (native-scripted-provider-native-input-snapshots provider)))
                2)
             "native compaction receives the full durable projection")
            (test-assert
             (= (first (reverse (scripted-provider-input-counts provider))) 1)
             "portable summarization starts from the native checkpoint only")
           (test-assert
            (find :native-compaction
                  (rest (conversation--read-records
                         (conversation-pathname conversation)))
                  :key #'first)
            "native compaction persists its opaque checkpoint")
           (test-assert
            (native-compaction-item-p
             (first (conversation-input-items conversation)))
            "the active provider reuses the opaque checkpoint")
            (test-assert
             (= (length (conversation-input-items-for-family conversation ':codex)) 3)
             "the active provider omits the redundant portable handoff")
           (test-assert
            (= (length (conversation-input-items-for-family conversation ':grok)) 3)
            "another provider receives the portable handoff and new messages"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-tool-wave () null)
(defun test-agent-parallel-tool-wave ()
  "Test independent calls overlap while callbacks and persistence stay ordered."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-parallel-wave"))
         (state (make-instance 'agent-test-concurrency-state))
         (tool (agent-test-concurrency-tool state "run"))
         (registry (agent-test-concurrency-registry (list tool)))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "parallel-calls"
              (list
               (agent-test-call
                :call-id "parallel-a"
                :namespace "concurrency"
                :name "run"
                :arguments
                "{\"label\":\"a\",\"delay\":0.05,\"await_peer\":true}")
               (agent-test-call
                :call-id "parallel-b"
                :namespace "concurrency"
                :name "run"
                :arguments
                "{\"label\":\"b\",\"await_peer\":true}")))
             (agent-test-result
              "parallel-done"
              (list (agent-test-message "done"))))))
         (callback-lock (make-lock "Autolith observer callback test"))
         (callback-active-count 0)
         (callback-maximum-active-count 0)
         (observer
           (callback-agent-observer-create
            :status-callback
            (lambda (status details)
              (declare (ignore details))
              (when (eq status ':agent-test-tool-callback)
                (with-lock-held (callback-lock)
                  (incf callback-active-count)
                  (setf callback-maximum-active-count
                        (max callback-maximum-active-count
                             callback-active-count)))
                (sleep 0.02)
                (with-lock-held (callback-lock)
                  (decf callback-active-count)))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider provider
                  :conversation conversation
                  :tool-registry registry
                  :worker ':unused)))
           (agent-run-user-turn agent "run both" :observer observer)
           (test-assert
            (agent-test-concurrency-state-overlap-observed-p state)
            "independent tool bodies overlap")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 2)
            "one provider batch uses two concurrent tool workers")
            (let ((logical-turn-states
                    (agent-test-concurrency-state-logical-turn-states state)))
              (test-assert
               (and (= (length logical-turn-states) 2)
                    (first logical-turn-states)
                    (every (lambda (turn-state)
                             (eq turn-state (first logical-turn-states)))
                           (rest logical-turn-states)))
               "parallel tool workers share the active logical-turn state"))
           (test-assert
            (= callback-maximum-active-count 1)
            "observer callbacks remain serialized across tool workers")
           (test-assert
            (equal (agent-test-tool-outputs conversation)
                   '("completed: a" "completed: b"))
            "tool outputs persist in provider wire order")
           (let ((events
                   (reverse (agent-test-concurrency-state-events state))))
             (test-assert
              (and (every (lambda (event)
                            (eq (first event) ':start))
                          (subseq events 0 2))
                   (equal (subseq events 2)
                          '((:finish "b") (:finish "a"))))
              "both bodies start before reverse completion finishes")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-concurrency-key () null)
(defun test-agent-tool-concurrency-key ()
  "Test calls sharing one runtime identity execute in separate waves."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-runtime-key"))
         (state (make-instance 'agent-test-concurrency-state))
         (runtime (list ':shared-runtime))
         (tool (agent-test-concurrency-tool
                state "keyed" :concurrency-key runtime))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "keyed-calls"
              (list
               (agent-test-call
                :call-id "keyed-a"
                :namespace "concurrency"
                :name "keyed"
                :arguments "{\"label\":\"a\",\"delay\":0.03}")
               (agent-test-call
                :call-id "keyed-b"
                :namespace "concurrency"
                :name "keyed"
                :arguments "{\"label\":\"b\",\"delay\":0.03}")))
             (agent-test-result
              "keyed-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider provider
                  :conversation conversation
                  :tool-registry
                  (agent-test-concurrency-registry (list tool))
                  :worker ':unused)))
           (agent-run-user-turn agent "run keyed calls")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 1)
            "calls sharing one runtime key do not overlap")
           (test-assert
            (equal (reverse (agent-test-concurrency-state-events state))
                   '((:start "a") (:finish "a")
                     (:start "b") (:finish "b")))
            "shared-runtime calls preserve provider order"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-exclusive-tool-waves () null)
(defun test-agent-exclusive-tool-waves ()
  "Test an exclusive call divides parallel calls into ordered waves."
  (test-assert
   (eq (tool-execution-policy
        (make-instance 'shell-run-tool
                       :namespace "shell"
                       :name "run"
                       :description "Run one command."
                       :parameters (json-object)))
       ':exclusive)
   "shell commands opt into exclusive ordered waves")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-exclusive-wave"))
         (state (make-instance 'agent-test-concurrency-state))
         (before (agent-test-concurrency-tool state "before"))
         (exclusive
           (agent-test-concurrency-tool
            state "exclusive" :execution-policy ':exclusive))
         (after (agent-test-concurrency-tool state "after"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "exclusive-calls"
              (list
               (agent-test-call
                :call-id "before"
                :namespace "concurrency"
                :name "before"
                :arguments "{\"label\":\"before\",\"delay\":0.02}")
               (agent-test-call
                :call-id "exclusive"
                :namespace "concurrency"
                :name "exclusive"
                :arguments "{\"label\":\"exclusive\",\"delay\":0.02}")
               (agent-test-call
                :call-id "after"
                :namespace "concurrency"
                :name "after"
                :arguments "{\"label\":\"after\",\"delay\":0.02}")))
             (agent-test-result
              "exclusive-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider provider
                  :conversation conversation
                  :tool-registry
                  (agent-test-concurrency-registry
                   (list before exclusive after))
                  :worker ':unused)))
           (agent-run-user-turn agent "run exclusive call")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 1)
            "exclusive execution prevents overlap across adjacent waves")
           (test-assert
            (equal (reverse (agent-test-concurrency-state-events state))
                   '((:start "before") (:finish "before")
                     (:start "exclusive") (:finish "exclusive")
                     (:start "after") (:finish "after")))
            "the exclusive call divides calls into ordered waves"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-tool-failure () null)
(defun test-agent-parallel-tool-failure ()
  "Test one failed parallel call does not discard its sibling result."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-parallel-failure"))
         (state (make-instance 'agent-test-concurrency-state))
         (tool (agent-test-concurrency-tool state "fail"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "failure-calls"
              (list
               (agent-test-call
                :call-id "failed"
                :namespace "concurrency"
                :name "fail"
                :arguments
                "{\"label\":\"failed\",\"await_peer\":true,\"fail\":true}")
               (agent-test-call
                :call-id "sibling"
                :namespace "concurrency"
                :name "fail"
                :arguments
                "{\"label\":\"sibling\",\"await_peer\":true}")))
             (agent-test-result
              "failure-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider provider
                  :conversation conversation
                  :tool-registry
                  (agent-test-concurrency-registry (list tool))
                  :worker ':unused)))
           (agent-run-user-turn agent "run failing calls")
           (let ((outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (= (length outputs) 2)
              "both parallel calls persist outputs after one fails")
             (test-assert
              (search "requested test failure" (first outputs))
              "the failed call persists its failure output")
             (test-assert
              (string= (second outputs) "completed: sibling")
              "the successful sibling result remains available")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-fatal-propagation () null)
(defun test-agent-parallel-fatal-propagation ()
  "Test propagated tool conditions persist unknown outcomes before propagation."
  (dolist (case '(("rollback" rollback-requested)
                  ("corruption" active-image-corruption)
                  ("job-aborted" job-aborted)))
    (destructuring-bind (fatal expected-type) case
      (let* ((configuration (test-configuration))
             (root (test-configuration-root configuration))
             (conversation
               (conversation-create
                configuration
                :identifier (format nil "agent-fatal-~A" fatal)))
             (state (make-instance 'agent-test-concurrency-state))
             (tool (agent-test-concurrency-tool state "fatal"))
             (provider
               (make-instance
                'scripted-provider
                :results
                (list
                 (agent-test-result
                  "fatal-calls"
                  (list
                   (agent-test-call
                    :call-id "fatal"
                    :namespace "concurrency"
                    :name "fatal"
                    :arguments
                    (format nil
                            "{\"label\":\"fatal\",\"await_peer\":true,\"fatal\":\"~A\"}"
                            fatal))
                   (agent-test-call
                    :call-id "sibling"
                    :namespace "concurrency"
                    :name "fatal"
                    :arguments
                    "{\"label\":\"sibling\",\"await_peer\":true}")))))))
        (unwind-protect
             (let* ((agent
                      (agent-create
                       :configuration configuration
                       :provider provider
                       :conversation conversation
                       :tool-registry
                       (agent-test-concurrency-registry (list tool))
                       :worker ':unused))
                    (condition
                      (handler-case
                          (progn
                            (agent-run-user-turn agent "run fatal calls")
                            nil)
                        (rollback-requested (failure)
                          failure)
                        (active-image-corruption (failure)
                          failure)
                        (job-aborted (failure)
                          failure))))
               (test-assert
                (typep condition expected-type)
                "the original fatal tool condition reaches the agent caller")
               (test-assert
                (agent-test-concurrency-state-overlap-observed-p state)
                "the fatal call executes concurrently with its sibling")
                (test-assert
                 (equal (agent-test-tool-outputs conversation)
                        (list *conversation-interrupted-tool-output*
                              "completed: sibling"))
                 "unknown and sibling results persist before fatal propagation"))
          (platform-delete-directory-tree
           *platform*
           root :validate t :if-does-not-exist ':ignore)))))
  nil)

(-> test-agent-shell-authorization-unavailable () null)
(defun test-agent-shell-authorization-unavailable ()
  "Test unavailable shell approval returns a failed tool result and continues the turn."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (marker (merge-pathnames "unavailable-shell-ran" root))
         (conversation
           (conversation-create configuration :identifier "shell-authorization"))
         (call
           (json-object
            "type" "function_call"
            "call_id" "shell-authorization-call"
            "namespace" "shell"
            "name" "run"
            "arguments"
            (json-encode
             (json-object
              "command"
              (format nil "printf ran > ~A"
                      (uiop:escape-shell-token (namestring marker)))
              "directory" (namestring root)))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result "shell-authorization-1" (list call))
             (agent-test-result
              "shell-authorization-2"
              (list (agent-test-message "approval failure handled"))))))
         (agent nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (setf agent
                 (agent-create
                  :configuration configuration
                  :provider provider
                  :conversation conversation
                  :tool-registry (make-default-tool-registry)
                  :worker ':unused))
           (let* ((observer
                    (callback-agent-observer-create
                     :command-authorization-callback
                     (lambda (command directory)
                       (error 'command-authorization-unavailable
                              :message
                              "Command approval requires an interactive terminal."
                              :command command
                              :directory directory))))
                  (result
                    (agent-run-user-turn
                     agent "run a command" :observer observer))
                  (outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (string= (provider-result-response-id result)
                       "shell-authorization-2")
              "the agent performs the provider round after unavailable approval")
             (test-assert
              (and (= (length outputs) 1)
                   (search "interactive terminal" (first outputs)))
              "the next provider round receives the failed shell tool result")
             (test-assert (not (probe-file marker))
                          "unavailable approval never starts the subprocess")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
  nil))


(-> test-agent-portable-value () null)
(defun test-agent-portable-value ()
  "Test portable provider metadata preserves strings and projects vectors."
  (test-assert
   (equal (agent--portable-value (vector "label" 7))
          '("label" 7))
   "portable provider metadata preserves strings inside vectors")
  nil)


(-> run-agent-tests () boolean)
(defun run-agent-tests ()
  "Run focused agent-loop tests and return true on success."
  (test-agent-portable-value)
  (test-agent-tool-loop)
  (test-agent-shell-authorization-unavailable)
  (test-agent-tool-free-turn)
  (test-agent-read-only-tool-allowlist)
  (test-agent-restricted-resource-schemes)
  (test-agent-restricted-tool-round-limit)
  (test-agent-empty-tool-allowlist)
  (test-agent-steering)
  (test-agent-explicit-continuation)
  (test-agent-provider-request-limit)
  (test-agent-invalid-call-history)
  (test-agent-malformed-tool-arguments)
  (test-agent-tool-storm-guard)
  (test-agent-tool-retry-guidance)
  (test-agent-tool-failures)
  (test-agent-provider-failure-persistence)
  (test-agent-incomplete-provider-failure-persistence)
  (test-agent-provider-credential-failure-containment)
  (test-agent-long-tool-turn)
  (test-agent-unbounded-tool-calls)
  (test-agent-default-turn-has-no-step-guillotine)
  (test-agent-skill-provider-barrier)
  (test-agent-compaction-missing-summary)
  (test-agent-compaction)
  (test-agent-native-compaction)
  (test-agent-parallel-tool-wave)
  (test-agent-tool-concurrency-key)
  (test-agent-exclusive-tool-waves)
  (test-agent-parallel-tool-failure)
  (test-agent-parallel-fatal-propagation)
  t)