(in-package #:autolith)

;;;; -- Recursive Inference Tests --

(-> test-rlm-frame-budget-activity () null)
(defun test-rlm-frame-budget-activity ()
  "Test framed inference forwards nested budget activity."
  (let ((budget (rlm-budget-create :calls 2 :tokens 100 :depth 1))
        (activities nil))
    (multiple-value-bind (status-callback flush-tranche)
        (rlm--frame-budget-callback
         budget "nested activity"
         :activity-callback
         (lambda (activity)
           (push activity activities)))
      (test-assert
       (= (funcall status-callback ':provider-request-started nil) 25)
       "framed inference returns the reserved output tranche to the agent")
      (funcall status-callback
               ':tool-call-progress
               (list ':activity "rlm.infer · request 1 · 1 call left"))
      (funcall flush-tranche)
      (test-assert
       (equal activities
              '("rlm.infer · request 1 · 1 call left"
                "request 1 · 1 calls left"))
       "framed inference forwards provider and nested RLM progress")))
  nil)

(-> test-rlm-context-designators () null)
(defun test-rlm-context-designators ()
  "Test Autolith normalizes root inference context designators."
  (test-assert (equal (rlm--context-designators "bare slice") '("bare slice"))
               "a bare string context wraps into one designator")
  (test-assert (equal (rlm--context-designators '(:label "solo" :content "x"))
                      '((:label "solo" :content "x")))
               "a bare plist context wraps into one designator")
  (test-assert (equal (rlm--context-designators (list "a" "b")) '("a" "b"))
               "designator lists pass through unchanged")
  (test-assert (null (rlm--context-designators nil))
               "an absent context stays empty")
  nil)

(-> test-rlm-response-usage-normalization () null)
(defun test-rlm-response-usage-normalization ()
  "Test inference traces persist canonical provider cache usage."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "rlm-cache-usage"))
         (result
           (make-instance
            'provider-result
            :response-id "rlm-cache-response"
            :output-items nil
            :tool-calls nil
            :usage (json-object
                    "prompt_tokens" 100
                    "completion_tokens" 25
                    "prompt_tokens_details"
                    (json-object "cached_tokens" 80
                                 "cache_write_tokens" 10))
            :turn-state nil
            :turn-completion ':unspecified)))
    (unwind-protect
         (progn
           (rlm--record-response conversation result)
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record (find :provider records :key #'first))
                  (usage (getf (getf (rest record) :metadata) :usage)))
             (test-assert
              (and (= (second (assoc "input_tokens" usage :test #'string=)) 100)
                   (= (second (assoc "output_tokens" usage :test #'string=)) 25)
                   (= (second (assoc "total_tokens" usage :test #'string=)) 125)
                   (= (second (assoc "cached_input_tokens" usage
                                     :test #'string=))
                      80)
                   (= (second (assoc "cache_creation_input_tokens" usage
                                     :test #'string=))
                      10))
              "inference metadata persists canonical cache usage")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(defclass rlm-inference-test-provider (model-provider)
  ((results
    :initarg :results
    :accessor rlm-inference-test-provider-results
    :type list
    :documentation "The provider results returned in request order.")
   (input-snapshots
    :initform nil
    :accessor rlm-inference-test-provider-input-snapshots
    :type list
    :documentation "The provider request items observed, newest first.")
   (output-limits
    :initform nil
    :accessor rlm-inference-test-provider-output-limits
    :type list
    :documentation "The bound output ceilings observed per request, newest first.")
   (tool-schema-counts
    :initform nil
    :accessor rlm-inference-test-provider-tool-schema-counts
    :type list
    :documentation "The provider tool schema counts observed, newest first."))
  (:documentation "A deterministic provider for exercising inference frames."))

(defmethod provider-with-configuration
    ((provider rlm-inference-test-provider) (configuration configuration))
  "Keep the scripted inference provider across configuration changes."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
    ((provider rlm-inference-test-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return PROVIDER's next scripted inference result."
  (declare (ignore event-callback goal-context compaction-p))
  (push (copy-tree (conversation-input-items-for-request conversation))
        (rlm-inference-test-provider-input-snapshots provider))
  (push (length tool-namespaces)
        (rlm-inference-test-provider-tool-schema-counts provider))
  (push *provider-maximum-output-tokens*
        (rlm-inference-test-provider-output-limits provider))
  (let ((result (pop (rlm-inference-test-provider-results provider))))
    (unless result
      (error "The inference test provider has no remaining result."))
    result))

(-> rlm-inference-test-result
    (string string (integer 0) &key (:cached-tokens (option (integer 0))))
    provider-result)
(defun rlm-inference-test-result (response-id text total-tokens
                                  &key cached-tokens)
  "Return a scripted assistant TEXT result reporting TOTAL-TOKENS usage.

CACHED-TOKENS, when supplied, reports that share as prompt-cache reads."
  (make-instance 'provider-result
                 :response-id response-id
                 :output-items (list (agent-test-message text))
                 :tool-calls nil
                 :usage (if cached-tokens
                            (json-object "total_tokens" total-tokens
                                         "cached_input_tokens" cached-tokens)
                            (json-object "total_tokens" total-tokens))
                 :turn-state nil
                 :turn-completion ':unspecified))

(-> test-rlm-budget-cache-discount () null)
(defun test-rlm-budget-cache-discount ()
  "Test budget settlement discounts prompt-cache reads from reported usage."
  (test-assert (null (rlm--usage-billable-tokens nil))
               "usage-less responses settle as a full refund")
  (test-assert (= (rlm--usage-billable-tokens
                   (json-object "total_tokens" 150
                                "cached_input_tokens" 80))
                  70)
               "cached input tokens do not drain the token pool")
  (test-assert (= (rlm--usage-billable-tokens '(("total_tokens" 150))) 150)
               "usage without cache counters settles at the reported total")
  (test-assert (zerop (rlm--usage-billable-tokens
                       (json-object "total_tokens" 100
                                    "cached_input_tokens" 120)))
               "over-reported cache reads clamp settlement at zero")
  (let ((provider
          (make-instance
           'rlm-inference-test-provider
           :results (list (rlm-inference-test-result "resp-1" "plain" 150
                                                     :cached-tokens 80))))
        (budget (rlm-budget-create :calls 2 :tokens 1000 :depth 1)))
    (test-assert (string= (infer "Say plain."
                                 :budget budget
                                 :provider provider
                                 :configuration (test-configuration))
                          "plain")
                 "an inference with cached usage returns its answer")
    (test-assert (= (rlm-budget-remaining-tokens budget) 930)
                 "settlement drains only the uncached token share"))
  nil)

(-> test-rlm-infer () null)
(defun test-rlm-infer ()
  "Test frames repair contract violations, charge budgets, and leave traces."
  (let ((configuration (test-configuration)))
    (test-assert (string= (let ((*system-prompt-override* "frame prompt"))
                            (system-prompt configuration))
                          "frame prompt")
                 "the system prompt override replaces the persona wholesale")
    (let ((provider
            (make-instance
             'rlm-inference-test-provider
             :results
             (list (rlm-inference-test-result "resp-1" "not json" 100)
                   (rlm-inference-test-result
                    "resp-2" "{\"answer\": \"42\"}" 150))))
          (budget (rlm-budget-create :calls 4 :tokens 1000 :depth 1))
          (activities nil))
      (multiple-value-bind (value trace-identifier)
          (infer "Answer the question."
                 :context (list "the question is six times seven")
                 :contract '(:type :object
                             :properties (("answer" (:type :string)))
                             :required ("answer"))
                 :budget budget
                 :provider provider
                 :configuration configuration
                 :activity-callback
                 (lambda (activity)
                   (push activity activities)))
        (test-assert (equal value '(:object ("answer" "42")))
                     "schema contracts return portable tagged native data")
        (test-assert (= (rlm-budget-remaining-calls budget) 2)
                     "the repair round charges a second call")
        (test-assert (= (rlm-budget-remaining-tokens budget) 750)
                     "reported usage drains the token pool")
        (test-assert
         (equal (reverse activities)
                '("request 1 · 3 calls left"
                  "request 2 · 2 calls left"))
         "direct inference reports each provider request and remaining calls")
        (test-assert (equal (reverse
                             (rlm-inference-test-provider-output-limits
                              provider))
                            '(250 225))
                     "each request carries its reserved tranche as the ceiling")
        (let ((identity
                (merge-pathnames
                 (make-pathname :name trace-identifier :type "sexp")
                 (configuration-inference-root configuration))))
          (test-assert (not (null (conversation-storage-pathnames identity)))
                       "the frame persists its trace conversation"))))
    (let ((provider
            (make-instance
             'rlm-inference-test-provider
             :results (list (rlm-inference-test-result "resp-1" "plain" 10))))
          (budget (rlm-budget-create :calls 2 :tokens 100 :depth 1))
          (callback-count 0))
      (test-assert
       (string=
        (infer "Ignore activity callback failures."
               :budget budget
               :provider provider
               :configuration configuration
               :activity-callback
               (lambda (activity)
                 (declare (ignore activity))
                 (incf callback-count)
                 (error "synthetic activity callback failure")))
        "plain")
       "activity callback failures do not abort direct inference")
      (test-assert
       (and (= callback-count 1)
            (= (rlm-budget-remaining-calls budget) 1)
            (= (rlm-budget-remaining-tokens budget) 90))
       "activity callback failures do not alter inference budget accounting"))
    (let ((provider
            (make-instance
             'rlm-inference-test-provider
             :results (list (rlm-inference-test-result "resp-1" "plain" 10)))))
      (test-assert (string= (infer "Say plain."
                                   :context "one bare slice"
                                   :provider provider
                                   :configuration configuration)
                            "plain")
                   "text contracts accept a bare context designator"))
    (let ((provider
            (make-instance
             'rlm-inference-test-provider
             :results (list (rlm-inference-test-result
                             "resp-1"
                             "The findings: [\"first\", \"second\"]" 10)))))
      (test-assert (equal (infer "List findings."
                                 :contract '(:type :array
                                             :items (:type :string))
                                 :provider provider
                                 :configuration configuration)
                          '(:array "first" "second"))
                   "top-level array contracts parse and validate"))
    (flet ((structured (text contract)
             (infer "Answer structurally."
                    :contract contract
                    :provider (make-instance
                               'rlm-inference-test-provider
                               :results (list (rlm-inference-test-result
                                               "resp-1" text 10)))
                    :configuration configuration)))
      (test-assert (null (structured "false" '(:type :boolean)))
                   "a JSON false answer validates as native nil")
      (test-assert (eq (structured "null" '(:type :null)) ':null)
                   "a JSON null answer validates as :null")
      (test-assert (equal (structured
                           "{\"enabled\": false, \"value\": null}"
                           '(:type :object
                             :properties
                             (("enabled" (:type :boolean))
                              ("value" (:type :null)))
                             :required ("enabled" "value")))
                          '(:object ("enabled" nil) ("value" :null)))
                   "nested false and null fields validate losslessly"))
    (let* ((arguments
             (tool-decode-arguments
              (rlm-infer-tool-create)
              "{\"task\": \"x\", \"contract\": {\"type\": \"object\", \"properties\": {\"on\": {\"type\": \"boolean\", \"enum\": [true, false]}}, \"additionalProperties\": false}}"))
           (contract (rlm--json-schema->contract
                      (gethash "contract" arguments))))
      (test-assert (eq (gethash "additionalProperties"
                                 (gethash "contract" arguments))
                       false)
                   "rlm tool arguments keep JSON false distinct from null")
      (test-assert (null (getf contract ':additional-properties ':missing))
                   "a JSON false additionalProperties converts to native nil")
      (test-assert (equal (getf (second
                                 (assoc "on" (getf contract ':properties)
                                        :test #'string=))
                                ':enum)
                          '(t nil))
                   "enum booleans convert to native true and false"))
    (let ((provider
            (make-instance
             'rlm-inference-test-provider
             :results (list (rlm-inference-test-result "resp-1" "no" 10)
                            (rlm-inference-test-result "resp-2" "no" 10)))))
      (test-assert
       (handler-case
           (progn
             (infer "Structured."
                    :contract '(:type :object
                                :properties (("answer" (:type :string)))
                                :required ("answer"))
                    :budget (rlm-budget-create :calls 1 :tokens 1000 :depth 1)
                    :provider provider
                    :configuration configuration)
             nil)
         (rlm-budget-exhausted (condition)
           (eq (rlm-budget-exhausted-dimension condition) ':calls))
         (error () nil))
       "unrepaired contracts stop at the call budget"))
    (test-assert (handler-case
                     (progn (infer "  ") nil)
                   (rlm-inference-error () t)
                   (error () nil))
                 "a blank task is refused before any provider work"))
  nil)

(defclass rlm-frame-test-search-tool (tool)
  ((executed-p
    :initform nil
    :accessor rlm-frame-test-search-tool-executed-p
    :type boolean
    :documentation "True once the frame executed this fake search tool."))
  (:documentation "A deterministic read-only stand-in for frame registries."))

(defmethod tool-execute
    ((tool rlm-frame-test-search-tool) (context tool-context)
     (arguments hash-table))
  "Record the execution and return fixed evidence."
  (declare (ignore context arguments))
  (setf (rlm-frame-test-search-tool-executed-p tool) t)
  (tool-success "fixed search evidence"))

(-> rlm-frame-test-tool (string string) tool)
(defun rlm-frame-test-tool (namespace name)
  "Return a minimal named tool for frame registry composition tests."
  (make-instance (if (string= namespace "search")
                     'rlm-frame-test-search-tool
                     'tool)
                 :namespace namespace
                 :name name
                 :description "Deterministic test tool."
                 :parameters (tool-object-schema (json-object) '())))

(-> test-rlm-frame-registry () null)
(defun test-rlm-frame-registry ()
  "Test frame registries keep read-only tools and add nested rlm.infer."
  (let ((source (make-instance 'tool-registry))
        (provider (make-instance 'rlm-inference-test-provider :results nil))
        (budget (rlm-budget-create :calls 2 :tokens 100 :depth 1)))
    (dolist (specification '(("resource" "read") ("resource" "edit")
                             ("shell" "run") ("search" "content")))
      (tool-registry-register
       source
       (rlm-frame-test-tool (first specification) (second specification))))
    (let* ((registry (rlm--frame-registry source provider budget))
           (names (sort (mapcar #'tool-canonical-name
                                (tool-registry-tools registry))
                        #'string<)))
      (test-assert (equal names
                          '("resource.read" "rlm.infer" "rlm.map"
                            "search.content"))
                   "frames gain nested rlm calls but never rlm.complete")
      (dolist (name '("infer" "map"))
        (let ((nested (tool-registry-find registry "rlm" name)))
          (test-assert (eq (rlm-frame-tool--budget nested) budget)
                       "the nested rlm tools share the frame budget")))))
  nil)

(-> test-rlm-framed-inference () null)
(defun test-rlm-framed-inference ()
  "Test read-capability frames execute restricted tools and charge budgets."
  (let* ((configuration (test-configuration))
         (source (make-instance 'tool-registry))
         (search-tool (rlm-frame-test-tool "search" "content"))
         (provider
           (make-instance
            'rlm-inference-test-provider
            :results
            (list (agent-test-result
                   "resp-1"
                   (list (agent-test-call :call-id "call-1"
                                          :namespace "search"
                                          :name "content"
                                          :arguments "{}")))
                  (rlm-inference-test-result "resp-2" "frame answer" 100))))
         (budget (rlm-budget-create :calls 5 :tokens 1000 :depth 1)))
    (tool-registry-register source search-tool)
    (multiple-value-bind (value trace-identifier)
        (infer "Find the evidence and answer."
               :capabilities ':read
               :budget budget
               :provider provider
               :configuration configuration
               :source-registry source)
      (test-assert (string= value "frame answer")
                   "read-capability frames return the final answer")
      (test-assert (non-empty-string-p trace-identifier)
                   "read-capability frames leave a trace identifier")
      (test-assert (rlm-frame-test-search-tool-executed-p search-tool)
                   "the frame executed its restricted read-only tool")
      (test-assert (= (rlm-budget-remaining-calls budget) 3)
                   "each provider request in the frame charges one call")
      (test-assert (= (rlm-budget-remaining-tokens budget) 898)
                   "every reported frame usage drains the token pool")))
  (let* ((configuration (test-configuration))
         (source (make-instance 'tool-registry))
         (search-tool (rlm-frame-test-tool "search" "content"))
         (provider
           (make-instance
            'rlm-inference-test-provider
            :results
            (list (agent-test-result
                   "reserved-1"
                   (list (agent-test-call :call-id "reserved-call"
                                          :namespace "search"
                                          :name "content"
                                          :arguments "{}")))
                  (rlm-inference-test-result
                   "reserved-2" "budgeted answer" 10))))
         (budget (rlm-budget-create :calls 2 :tokens 1000 :depth 1)))
    (tool-registry-register source search-tool)
    (test-assert
     (string= (infer "Inspect once, then answer."
                     :capabilities ':read
                     :budget budget
                     :provider provider
                     :configuration configuration
                     :source-registry source)
              "budgeted answer")
     "read-capability frames reserve the final provider call for an answer")
    (test-assert
     (equal (reverse
             (rlm-inference-test-provider-tool-schema-counts provider))
            '(2 0))
     "the final reserved request omits every tool schema")
    (test-assert (zerop (rlm-budget-remaining-calls budget))
                 "the reserved final answer may consume the last call"))
  nil)

(defclass rlm-routing-test-provider (rlm-inference-test-provider)
  ((configurations
    :initform nil
    :type list
    :accessor rlm-routing-test-configurations
    :documentation "Configurations applied before scripted provider requests."))
  (:documentation "Record the configurations selected for scripted requests."))

(defmethod provider-with-configuration :before
    ((provider rlm-routing-test-provider) (configuration configuration))
  "Record each route applied to the scripted provider."
  (push configuration (rlm-routing-test-configurations provider)))

(-> test-rlm-tool-routing () null)
(defun test-rlm-tool-routing ()
  "Test distinct root routes, invalid selections and nested route inheritance."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((model (find-if
                   (lambda (candidate)
                     (and (not (equal candidate (configuration-model configuration)))
                          (provider-model-reasoning-efforts-for candidate)))
                   (provider-model-identifiers)))
           (effort (first (provider-model-reasoning-efforts-for model)))
           (registry (make-instance 'tool-registry))
           (context (make-instance 'tool-context :configuration configuration
                                  :worker nil :registry registry
                                  :conversation (conversation-create configuration)))
           (provider (make-instance 'rlm-routing-test-provider :results nil)))
      (test-assert model "routing tests select a different registered model")
      (labels ((check-route ()
                 (let ((routes (rlm-routing-test-configurations provider)))
                   (test-assert
                    (and routes
                         (every (lambda (route)
                                  (and (equal model (configuration-model route))
                                       (equal effort (configuration-reasoning-effort route))))
                                routes))
                    "the operation uses the requested model and effort"))))
        (dolist (constructor (list #'rlm-infer-tool-create
                                  #'rlm-map-tool-create
                                  #'rlm-complete-tool-create))
          (let ((tool (funcall constructor :provider provider)))
            (multiple-value-bind (selected routed)
                (rlm--tool-routing tool context (json-object "model" model "effort" effort))
              (test-assert (eq selected provider) "the scripted provider handles the selected route")
              (test-assert (and (equal model (configuration-model routed))
                                (equal effort (configuration-reasoning-effort routed)))
                           "explicit routing changes the configuration")
              (let ((nested-context
                      (make-instance 'tool-context :configuration routed
                                     :worker nil :registry registry
                                     :conversation (tool-context-conversation context))))
                (dolist (nested-constructor (list #'rlm-infer-tool-create #'rlm-map-tool-create))
                  (let* ((budget (rlm-budget-create :calls 4 :tokens 10000 :depth 2))
                         (nested (funcall nested-constructor :provider selected :budget budget)))
                    (multiple-value-bind (nested-provider inherited)
                        (rlm--tool-routing nested nested-context (json-object))
                      (test-assert (and (eq nested-provider selected) (eq inherited routed))
                                   "nested calls inherit the explicitly selected root route"))
                    (dolist (override (list (json-object "model" model)
                                            (json-object "effort" effort)))
                      (test-assert
                       (handler-case
                           (progn (rlm--tool-routing nested nested-context override) nil)
                         (rlm-inference-error () t))
                       "nested calls cannot override either routing argument"))))))
            (dolist (arguments (list (json-object "model" 42)
                                     (json-object "model" "")
                                     (json-object "model" "unknown-rlm-test-model")
                                     (json-object "model" model "effort" "unsupported-effort")))
              (setf (rlm-routing-test-configurations provider) nil)
              (test-assert
               (handler-case
                   (progn (rlm--tool-routing tool context arguments) nil)
                 ((or rlm-inference-error configuration-error) () t))
               "invalid routing is rejected")
              (test-assert (null (rlm-routing-test-configurations provider))
                           "invalid routing never reconfigures the provider"))))
        (setf (rlm-routing-test-configurations provider) nil
              (rlm-inference-test-provider-results provider)
              (list (rlm-inference-test-result "route-infer" "routed" 10)))
        (test-assert
         (tool-result-success-p
          (tool-execute (rlm-infer-tool-create :provider provider) context
                        (json-object "task" "Return routed." "model" model "effort" effort)))
         "the inference tool executes with explicit routing")
        (check-route)
        (setf (rlm-routing-test-configurations provider) nil
              (rlm-inference-test-provider-results provider)
              (list (rlm-inference-test-result "route-map-1" "one" 10)
                    (rlm-inference-test-result "route-map-2" "two" 10)))
        (test-assert
         (tool-result-success-p
          (tool-execute (rlm-map-tool-create :provider provider) context
                        (json-object "tasks" (json-array (json-object "task" "one")
                                                         (json-object "task" "two"))
                                     "model" model "effort" effort "concurrency" 1)))
         "the map tool executes its frames with explicit routing")
        (check-route)
        (setf (rlm-routing-test-configurations provider) nil
              (rlm-inference-test-provider-results provider)
              (list (rlm-inference-test-result "route-programmatic-map" "mapped" 10)))
        (test-assert
         (equal "mapped"
                (getf (first (rlm-map '("one") :model model :effort effort
                                     :provider provider :configuration configuration))
                      :value))
         "programmatic maps accept routing arguments")
        (check-route)
        (setf (rlm-routing-test-configurations provider) nil
              (rlm-inference-test-provider-results provider)
              (list (agent-test-result
                     "route-root"
                     (list (agent-test-call
                            :call-id "route-eval" :namespace "env" :name "eval"
                            :arguments (json-encode
                                        (json-object "form"
                                                     "(finish (infer \"Return nested.\" :context \"x\"))")))))
                    (rlm-inference-test-result "route-child" "nested" 10)))
        (let ((result
                (tool-execute (rlm-complete-tool-create :provider provider) context
                              (json-object "task" "Delegate and finish."
                                           "context" (json-object "text" "x")
                                           "model" model "effort" effort
                                           "calls" 6 "tokens" 100000))))
          (test-assert (tool-result-success-p result)
                       "a routed completion can execute a nested inference")
          (test-assert (null (rlm-inference-test-provider-results provider))
                       "the root and child each perform their provider request"))
        (check-route))))
  nil)

(-> test-rlm-infer-tool () null)
(defun test-rlm-infer-tool ()
  "Test rlm.infer runs frames from tool arguments and reports failures."
  (let* ((configuration (test-configuration))
         (source (make-instance 'tool-registry))
         (conversation (conversation-create configuration
                                            :identifier "rlm-tool-test"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry source)))
    (let* ((provider
             (make-instance
              'rlm-inference-test-provider
              :results
              (list (rlm-inference-test-result
                     "resp-1" "{\"answer\": \"4\"}" 50))))
           (tool (rlm-infer-tool-create :provider provider))
           (result
             (tool-execute
              tool
              context
              (json-object
               "task" "Sum the view."
               "views" (json-array (json-object "label" "sum"
                                                "text" "2 + 2"))
               "contract" (json-object
                           "type" "object"
                           "properties" (json-object
                                         "answer" (json-object
                                                   "type" "string"))
                           "required" (json-array "answer"))
               "calls" 3))))
      (test-assert (tool-result-success-p result)
                   "rlm.infer succeeds on a contract-satisfying frame")
      (test-assert (and (search ":VALUE" (tool-result-content result))
                        (search "\"4\"" (tool-result-content result))
                        (search ":TRACE" (tool-result-content result)))
                   "rlm.infer reports the value and the trace identifier"))
    (let* ((provider
             (make-instance
              'rlm-inference-test-provider
              :results (list (rlm-inference-test-result "resp-1" "no" 10)
                             (rlm-inference-test-result "resp-2" "no" 10))))
           (tool (rlm-infer-tool-create :provider provider))
           (result
             (tool-execute
              tool
              context
              (json-object
               "task" "Structured."
               "contract" (json-object
                           "type" "object"
                           "properties" (json-object
                                         "answer" (json-object
                                                   "type" "string"))
                           "required" (json-array "answer"))
               "calls" 1))))
      (test-assert (and (not (tool-result-success-p result))
                        (search "budget" (tool-result-content result)))
                   "rlm.infer reports budget exhaustion as a tool failure"))
    (let* ((provider
             (make-instance 'rlm-inference-test-provider :results nil))
           (tool (rlm-infer-tool-create
                  :provider provider
                  :budget (rlm-budget-create :calls 4 :tokens 100 :depth 0)))
           (result
             (tool-execute tool context (json-object "task" "Nested."))))
      (test-assert (and (not (tool-result-success-p result))
                        (search "depth" (tool-result-content result)))
                   "nested rlm.infer refuses descent past the depth budget")))
  nil)

(defclass rlm-map-test-provider (model-provider)
  ((request-count
    :initform 0
    :accessor rlm-map-test-provider-request-count
    :type (integer 0)
    :documentation "The total provider requests served across all threads.")
   (count-lock
    :initform (make-lock "Autolith map test provider")
    :reader rlm-map-test-provider--count-lock
    :documentation "The lock serializing concurrent request counting."))
  (:documentation "A thread-safe provider answering from the request itself."))

(-> rlm-map-test--last-user-text (conversation) (option string))
(defun rlm-map-test--last-user-text (conversation)
  "Return the newest user message text in CONVERSATION's request items."
  (loop for item in (reverse (conversation-input-items-for-request
                              conversation))
        when (and (json-object-p item)
                  (json-string= (json-get item "role") "user"))
          do (let ((content (json-get item "content")))
               (return
                 (loop for part across content
                       when (and (json-object-p part)
                                 (json-string= (json-get part "type")
                                               "input_text"))
                         do (return (json-get part "text")))))))

(defmethod provider-stream-turn
    ((provider rlm-map-test-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Echo the newest user request text back as the assistant answer."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (with-lock-held ((rlm-map-test-provider--count-lock provider))
    (incf (rlm-map-test-provider-request-count provider)))
  (let ((text (or (rlm-map-test--last-user-text conversation) "")))
    (when (search "explode" text)
      (error 'rlm-inference-error :message "scripted map explosion"))
    (make-instance 'provider-result
                   :response-id "map-response"
                   :output-items (list (agent-test-message
                                        (format nil "echo: ~A" text)))
                   :tool-calls nil
                   :usage (json-object "total_tokens" 10)
                   :turn-state nil
                   :turn-completion ':unspecified)))

(-> test-rlm-map () null)
(defun test-rlm-map ()
  "Test parallel maps keep order, share budgets, and capture failures."
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider))
         (budget (rlm-budget-create :calls 10 :tokens 1000 :depth 1))
         (activity-lock (make-lock "Autolith inference activity test"))
         (activities nil)
         (results
           (rlm-map (list "alpha"
                          (list ':task "beta"
                                ':context (list "beta extra view"))
                          "gamma explode"
                          "delta")
                    :budget budget
                    :provider provider
                    :configuration configuration
                    :concurrency 3
                    :activity-callback
                    (lambda (activity)
                      (with-lock-held (activity-lock)
                        (push activity activities))))))
    (test-assert (equal (mapcar (lambda (result) (getf result ':task))
                                results)
                        '("alpha" "beta" "gamma explode" "delta"))
                 "map results keep the task order")
    (test-assert (loop for result in results
                       for task in '("alpha" "beta" "delta")
                       always (or (getf result ':error)
                                  (search (getf result ':task)
                                          (getf result ':value))))
                 "each completed frame answered its own task")
    (test-assert (search "beta extra view"
                         (getf (second results) ':value))
                 "per-task context views reach the frame")
    (test-assert (search "explosion" (getf (third results) ':error))
                 "a failing frame is captured without discarding the rest")
    (test-assert (non-empty-string-p (getf (first results) ':trace))
                 "completed map frames report their trace identifiers")
    (test-assert (= (rlm-budget-remaining-calls budget) 6)
                 "every attempted request holds its reservation, failures included")
    (test-assert (= (rlm-budget-remaining-tokens budget) 970)
                 "the three completed frames drained the shared token pool")
    (let ((activities
            (with-lock-held (activity-lock)
              (copy-list activities))))
      (test-assert
       (member "starting 4 frames" activities :test #'string=)
       "map activity reports the fan width before workers start")
      (test-assert
       (some (lambda (activity)
               (and (search "frame " activity)
                    (search "request 1" activity)
                    (search "calls left" activity)))
             activities)
       "map activity reports live frame request progress")))
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider))
         (budget (rlm-budget-create :calls 2 :tokens 1000 :depth 1))
         (results (rlm-map (list "one" "two" "three" "four")
                           :budget budget
                           :provider provider
                           :configuration configuration
                           :concurrency 1)))
    (test-assert (equal (mapcar (lambda (result)
                                  (if (getf result ':error) ':error ':value))
                                results)
                        '(:value :value :error :error))
                 "budget exhaustion mid-map keeps finished frames and fails the rest"))
  (test-assert (null (rlm-map nil))
               "an empty task list maps to no results")
  (test-assert (handler-case
                   (progn (rlm-map (list 42)
                                   :provider (make-instance
                                              'rlm-map-test-provider)
                                   :configuration (test-configuration))
                          nil)
                 (rlm-inference-error () t)
                 (error () nil))
               "a malformed map element is refused before any frame runs")
  nil)

(-> test-rlm-map-tool () null)
(defun test-rlm-map-tool ()
  "Test rlm.map fans tool tasks out and bounds the fan width."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "rlm-map-tool-test"))
         (activity-lock (make-lock "Autolith inference tool activity test"))
         (activities nil)
         (observer
           (callback-agent-observer-create
            :status-callback
            (lambda (status details)
              (when (eq status ':tool-call-progress)
                (with-lock-held (activity-lock)
                  (push (getf details :activity) activities))))))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry (make-instance 'tool-registry)
                                 :observer observer)))
    (let* ((provider (make-instance 'rlm-map-test-provider))
           (tool (rlm-map-tool-create :provider provider))
           (result
             (tool-execute
              tool
              context
              (json-object
               "tasks" (json-array
                        (json-object "task" "alpha")
                        (json-object "task" "beta"
                                     "views" (json-array
                                              (json-object
                                               "text" "beta view"))))
               "calls" 6
               "concurrency" 2))))
      (test-assert (tool-result-success-p result)
                   "rlm.map succeeds when its frames complete")
      (let ((content (tool-result-content result)))
        (test-assert (and (search "alpha" content)
                          (search "beta view" content)
                          (search ":RESULTS" content)
                          (search ":TRACE" content))
                     "rlm.map reports ordered values with traces")))
      (let ((activities
              (with-lock-held (activity-lock)
                (copy-list activities))))
        (test-assert
         (some (lambda (activity)
                 (and (stringp activity)
                      (search "rlm.map · " activity)
                      (search "frame" activity)))
               activities)
         "rlm.map publishes compact live activity through its tool observer"))
    (let* ((provider (make-instance 'rlm-map-test-provider))
           (tool (rlm-map-tool-create :provider provider))
           (result
             (tool-execute
              tool
              context
              (json-object
               "tasks" (coerce
                        (loop repeat (1+ *rlm-tool-maximum-map-tasks*)
                              collect (json-object "task" "flood"))
                        'vector)))))
      (test-assert (and (not (tool-result-success-p result))
                        (search "at most" (tool-result-content result)))
                   "rlm.map refuses fans wider than the task cap")))
  nil)

(defmethod rlm-decompose-inference-task
    ((policy (eql ':rlm-policy-test-split)) (task string) (views list)
     (budget rlm-budget))
  "Split any task into two fixed labeled subtasks for policy tests."
  (declare (ignore views budget))
  (list (list ':task (format nil "part one of: ~A" task))
        (list ':task (format nil "part two of: ~A" task)
              ':context (list "part two extra view"))))

(-> test-rlm-policies () null)
(defun test-rlm-policies ()
  "Test policies decompose, fan out, and synthesize under one budget."
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider))
         (budget (rlm-budget-create :calls 10 :tokens 1000 :depth 1)))
    (multiple-value-bind (value trace-identifier)
        (rlm-run "summarize the module"
                 :policy ':rlm-policy-test-split
                 :budget budget
                 :provider provider
                 :configuration configuration)
      (test-assert (and (search "part one of: summarize the module" value)
                        (search "part two of: summarize the module" value)
                        (search "part two extra view" value))
                   "synthesis frames see every subtask result as a view")
      (test-assert (search "subtask 2" value)
                   "synthesis views are labeled per subtask")
      (test-assert (non-empty-string-p trace-identifier)
                   "the synthesis frame reports its trace identifier")
      (test-assert (= (rlm-budget-remaining-calls budget) 7)
                   "two subtasks and one synthesis share the call pool")
      (test-assert (= (rlm-map-test-provider-request-count provider) 3)
                   "the split policy runs exactly three frames")))
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider)))
    (test-assert (search "direct question"
                         (rlm-run "direct question"
                                  :provider provider
                                  :configuration configuration))
                 "the default policy runs the task as one frame")
    (test-assert (= (rlm-map-test-provider-request-count provider) 1)
                 "the default policy never fans out"))
  nil)

(-> test-rlm-trace-resource () null)
(defun test-rlm-trace-resource ()
  "Test persisted frame traces read back through the inference scheme."
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider))
         (conversation (conversation-create configuration
                                            :identifier "rlm-trace-test"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry (make-instance 'tool-registry)))
         (resolver (make-instance 'inference-trace-resolver
                                  :scheme "inference"))
         (read-tool (make-instance 'resource-read-tool
                                   :namespace "resource"
                                   :name "read"
                                   :description "Test resource read."
                                   :parameters (tool-object-schema
                                                (json-object) '())
                                   :resource-registry
                                   (make-resource-registry))))
    (multiple-value-bind (value trace-identifier)
        (infer "trace me please"
               :provider provider
               :configuration configuration)
      (declare (ignore value))
      (let* ((resource (resource-resolver-resolve resolver trace-identifier
                                                  context))
             (result (resource-tool-read resource read-tool context
                                         (json-object))))
        (test-assert (tool-result-success-p result)
                     "persisted traces read back successfully")
        (test-assert (search "trace me please" (tool-result-content result))
                     "the trace carries the frame request verbatim")
        (test-assert (uiop:string-prefix-p "lines 1-"
                                           (tool-result-content result))
                     "trace reads report their window and total lines")
        (let ((window (resource-tool-read resource read-tool context
                                          (json-object "start-line" 2
                                                       "line-count" 1))))
          (test-assert (uiop:string-prefix-p "lines 2-2 of "
                                             (tool-result-content window))
                       "trace reads honor start-line and line-count"))))
    (let ((missing (resource-resolver-resolve resolver "zzzz-none" context)))
      (test-assert (not (tool-result-success-p
                         (resource-tool-read missing read-tool context
                                             (json-object))))
                   "missing traces read as tool failures"))
    (test-assert (handler-case
                     (progn
                       (resource-resolver-resolve resolver "../escape"
                                                  context)
                       nil)
                   (resource-operation-unsupported () t)
                   (error () nil))
                 "unsafe trace identifiers are refused at resolution"))
  nil)

(-> test-rlm-permission-classifier () null)
(defun test-rlm-permission-classifier ()
  "Test automatic permission context, decisions, and fail-closed fallbacks."
  (let ((configuration (test-configuration)))
    (flet ((classify (&rest texts)
             (permissions-model-classify-command
              "cargo build" #p"/root/project/"
              :provider (make-instance
                         'rlm-inference-test-provider
                         :results
                         (mapcar (lambda (text)
                                   (rlm-inference-test-result "response" text 20))
                                 texts))
              :configuration configuration
              :user-instructions "Build the project in this workspace.")))
      (test-assert
       (equal (getf (second
                     (first (getf *permissions-model-decision-contract*
                                  ':properties)))
                    ':enum)
              '("sandboxed" "full" "deny"))
       "the model contract exposes only automatic permission outcomes")
      (test-assert (null (permissions--model-decision-keyword "ask"))
                   "the decision parser rejects manual ask output")
      (test-assert (null (permissions--model-decision-keyword "pick"))
                   "the decision parser rejects pick output")
      (let ((provider
              (make-instance
               'rlm-inference-test-provider
               :results
               (list
                (rlm-inference-test-result
                 "context"
                 "{\"decision\": \"sandboxed\", \"reason\": \"workspace build\"}"
                 20)))))
        (test-assert
         (eq (permissions-model-classify-command
              "cargo build" #p"/root/project/"
              :provider provider
              :configuration configuration
              :user-instructions "Build and test the current checkout.")
             ':sandboxed)
         "a valid sandbox decision is returned")
        (let* ((snapshot
                 (first
                  (rlm-inference-test-provider-input-snapshots provider)))
               (request-text
                 (loop for item in (reverse snapshot)
                       when (and (json-object-p item)
                                 (string= (or (json-get item "role") "") "user"))
                         do (return (context--message-text item)))))
          (test-assert
           (search "current user instructions" request-text)
           "classification includes a labeled user-instruction view")
          (test-assert
           (search "Build and test the current checkout." request-text)
           "classification forwards the current user instructions verbatim")))
      (multiple-value-bind (decision reason)
          (classify
           "{\"decision\": \"sandboxed\", \"reason\": \"workspace build\"}")
        (test-assert (eq decision ':sandboxed)
                     "sandbox decisions map to the sandboxed grant")
        (test-assert (string= reason "workspace build")
                     "the model's reason is passed through"))
      (multiple-value-bind (decision reason)
          (permissions-model-classify-command
           "git status" #p"/root/project/"
           :provider
           (make-instance
            'rlm-inference-test-provider
            :results
            (list
             (rlm-inference-test-result
              "unavailable-sandbox"
              "{\"decision\": \"sandboxed\", \"reason\": \"read-only inspection\"}"
              20)))
           :configuration configuration
           :sandbox-available-p nil
           :user-instructions "Inspect the checkout.")
        (test-assert (eq decision ':deny)
                     "unavailable sandbox decisions are denied")
        (test-assert (string= reason "the workspace sandbox is unavailable")
                     "unavailable sandbox denial explains the missing grant"))
      (test-assert
       (eq (classify "{\"decision\": \"full\", \"reason\": \"network\"}")
           ':full-access)
       "full decisions map to the full-access grant")
      (test-assert
       (eq (classify "{\"decision\": \"deny\", \"reason\": \"wipe\"}")
           ':deny)
       "deny decisions map to the deny grant")
      (test-assert
       (eq (permissions-model-classify-command
            "git restore file" #p"/root/project/"
            :provider
            (make-instance
             'rlm-inference-test-provider
             :results
             (list
              (rlm-inference-test-result
               "pick-1"
               "{\"decision\": \"pick\", \"reason\": \"uncertain\"}"
               20)
              (rlm-inference-test-result
               "pick-2"
               "{\"decision\": \"pick\", \"reason\": \"still uncertain\"}"
               20)))
            :configuration configuration
            :user-instructions "Inspect changes only.")
           ':deny)
       "unknown pick output is denied")
      (multiple-value-bind (decision reason)
          (classify "garbage" "still garbage")
        (test-assert (eq decision ':deny)
                     "unrepaired classifier output is denied")
        (test-assert (non-empty-string-p reason)
                     "malformed-output denial carries an explanation"))
      (test-assert (eq (classify) ':deny)
                   "provider failures are denied")))
  nil)

(-> test-rlm-context-object-adapter () null)
(defun test-rlm-context-object-adapter ()
  "Test Autolith maps configurations to provider API context stores."
  (let* ((configuration (test-configuration))
         (object (rlm-context-intern configuration "shared corpus"
                                     :label "corpus")))
    (test-assert (uiop:subpathp (rlm-context-object-pathname object)
                                (rlm-object-root configuration))
                 "context objects are stored below the configuration data root")
    (multiple-value-bind (found content)
        (rlm-context-object-find configuration
                                 (rlm-context-object-digest object))
      (test-assert (and found
                        (string= content "shared corpus")
                        (string= (rlm-context-object-label object) "corpus"))
                   "the adapter returns provider API objects and verified content"))
    (test-assert (handler-case
                     (progn
                       (rlm-context-designator-object configuration 42)
                       nil)
                   (rlm-view-error () t)
                   (error () nil))
                 "the adapter preserves provider API designator validation"))
  nil)

(-> rlm-endpoint-test-call (rlm-endpoint list) list)
(defun rlm-endpoint-test-call (endpoint request)
  "Send one raw REQUEST packet to ENDPOINT and return its response."
  (multiple-value-bind (socket stream)
      (daemon-connect (rlm-endpoint-port endpoint))
    (declare (ignore socket))
    (unwind-protect
         (progn
           (daemon-write-packet stream request)
           (daemon-read-packet stream))
      (ignore-errors (close stream)))))

(-> test-rlm-endpoint () null)
(defun test-rlm-endpoint ()
  "Test the loopback endpoint proxies inference under the shared budget."
  (let* ((configuration (test-configuration))
         (provider (make-instance 'rlm-map-test-provider))
         (budget (rlm-budget-create :calls 6 :tokens 1000 :depth 1))
         (records nil)
         (endpoint (rlm-endpoint-start :provider provider
                                       :configuration configuration
                                       :budget budget
                                       :ledger (lambda (record)
                                                 (push record records)))))
    (unwind-protect
         (progn
           (let ((response
                   (rlm-endpoint-test-call
                    endpoint
                    (list :rlm-request
                          :token (rlm-endpoint-token endpoint)
                          :operation ':infer
                          :arguments (list :task "proxy this question"
                                           :context (list "proxy view"))))))
             (test-assert (eq (getf (rest response) ':status) ':ok)
                          "proxied inference succeeds through the endpoint")
             (test-assert (search "proxy this question"
                                  (getf (rest response) ':value))
                          "proxied inference returns the frame value")
             (test-assert (non-empty-string-p
                           (getf (rest response) ':trace))
                          "proxied inference reports its trace"))
           (test-assert (= (rlm-budget-remaining-calls budget) 5)
                        "proxied calls drain the shared root budget")
           (let ((response
                   (rlm-endpoint-test-call
                    endpoint
                    (list :rlm-request
                          :token "wrong-token"
                          :operation ':infer
                          :arguments (list :task "steal a call")))))
             (test-assert (eq (getf (rest response) ':status) ':error)
                          "an invalid token is refused"))
           (test-assert (= (rlm-budget-remaining-calls budget) 5)
                        "refused requests spend nothing")
           (let ((response
                   (rlm-endpoint-test-call
                    endpoint
                    (list :rlm-request
                          :token (rlm-endpoint-token endpoint)
                          :operation ':map
                          :arguments
                          (list :tasks (list "first part" "second part")
                                :concurrency 2)))))
             (test-assert (and (eq (getf (rest response) ':status) ':ok)
                               (= (length (getf (rest response) ':value)) 2))
                          "proxied maps fan out and return ordered results"))
           (multiple-value-bind (value final-p) (rlm-endpoint-final endpoint)
             (declare (ignore value))
             (test-assert (not final-p)
                          "no final value exists before finish"))
           (rlm-endpoint-test-call
            endpoint
            (list :rlm-request
                  :token (rlm-endpoint-token endpoint)
                  :operation ':finish
                  :arguments (list :value 42)))
           (multiple-value-bind (value final-p) (rlm-endpoint-final endpoint)
             (test-assert (and final-p (eql value 42))
                          "finish records the environment's final value"))
           (let ((response
                   (rlm-endpoint-test-call
                    endpoint
                    (list :rlm-request
                          :token (rlm-endpoint-token endpoint)
                          :operation ':finish
                          :arguments (list :value 43)))))
             (test-assert (eq (getf (rest response) ':status) ':error)
                          "a second finish is refused"))
           (test-assert (eql (rlm-endpoint-final endpoint) 42)
                        "the first finish wins")
           (let ((response
                   (rlm-endpoint-test-call
                    endpoint
                    (list :rlm-request
                          :token (rlm-endpoint-token endpoint)
                          :operation ':infer
                          :arguments (list :task "after the end")))))
             (test-assert (eq (getf (rest response) ':status) ':error)
                          "finished runs refuse further inference"))
           (let ((operations (mapcar (lambda (record)
                                       (getf record ':operation))
                                     (reverse records))))
             (test-assert (equal operations '(:infer :map :finish))
                          "the ledger records every served operation in order"))
           (let ((infer-record (find ':infer records
                                     :key (lambda (record)
                                            (getf record ':operation)))))
             (test-assert (and (non-empty-string-p
                                (getf infer-record ':child-trace))
                               (integerp
                                (getf infer-record ':calls-remaining)))
                          "ledger records link child traces and budget state")))
      (rlm-endpoint-stop endpoint)))
  (let* ((configuration (test-configuration))
         (activities nil)
         (endpoint
           (rlm-endpoint-start
            :provider (make-instance 'rlm-map-test-provider)
            :configuration configuration
            :budget (rlm-budget-create :calls 1 :tokens 100 :depth 1)
            :activity-callback
            (lambda (activity)
              (push activity activities))))
         (retained-callback
           (rlm-endpoint--operation-activity-callback endpoint ':infer))
         (stopped-p nil))
    (unwind-protect
         (progn
           (rlm-endpoint-stop endpoint)
           (setf stopped-p t)
           (funcall retained-callback "request 2 · 0 calls left")
           (test-assert
            (null activities)
            "a retained handler callback cannot publish after endpoint shutdown"))
      (unless stopped-p
        (rlm-endpoint-stop endpoint))))
  nil)

(defclass rlm-litmus-provider (model-provider)
  ((window-limit
    :initarg :window-limit
    :reader rlm-litmus-provider--window-limit
    :type (integer 1)
    :documentation "The simulated provider context window in request characters.")
   (largest-request
    :initform 0
    :accessor rlm-litmus-provider-largest-request
    :type (integer 0)
    :documentation "The largest request observed across all threads.")
   (request-count
    :initform 0
    :accessor rlm-litmus-provider-request-count
    :type (integer 0)
    :documentation "The total provider requests served.")
   (root-results
    :initarg :root-results
    :accessor rlm-litmus-provider--root-results
    :type list
    :documentation "The scripted root-model results in request order.")
   (lock
    :initform (make-lock "Autolith litmus provider")
    :reader rlm-litmus-provider--lock
    :documentation "The lock guarding counters across handler threads."))
  (:documentation
   "A window-limited provider serving scripted root turns and computed sub-answers."))

(defmethod provider-with-configuration
    ((provider rlm-litmus-provider) (configuration configuration))
  "Keep the litmus provider across configuration changes."
  (declare (ignore configuration))
  provider)

(-> rlm-litmus--count-occurrences (string string) (integer 0))
(defun rlm-litmus--count-occurrences (needle text)
  "Return how many times NEEDLE occurs in TEXT, overlapping included."
  (loop with start = 0
        with count = 0
        for position = (search needle text :start2 start)
        while position
        do (incf count)
           (setf start (1+ position))
        finally (return count)))

(defmethod provider-stream-turn
    ((provider rlm-litmus-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Refuse over-window requests, answer sub-frames, script root turns."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (let ((size (loop for item in (conversation-input-items-for-request
                                 conversation)
                    sum (length (json-encode item)))))
    (with-lock-held ((rlm-litmus-provider--lock provider))
      (incf (rlm-litmus-provider-request-count provider))
      (setf (rlm-litmus-provider-largest-request provider)
            (max (rlm-litmus-provider-largest-request provider) size)))
    (when (> size (rlm-litmus-provider--window-limit provider))
      (error 'rlm-inference-error
             :message
             (format nil "A ~D character request exceeds the ~D window."
                     size (rlm-litmus-provider--window-limit provider))))
    (let* ((text (or (rlm-map-test--last-user-text conversation) ""))
           (view-start (search "<view" text))
           (view-end (search "</view" text :from-end t)))
      (if (and view-start view-end)
          (rlm-inference-test-result
           "litmus-sub"
           (format nil "~D"
                   (rlm-litmus--count-occurrences
                    "XYZZY" (subseq text view-start view-end)))
           40)
          (let ((result
                  (with-lock-held ((rlm-litmus-provider--lock provider))
                    (pop (rlm-litmus-provider--root-results provider)))))
            (unless result
              (error "The litmus provider has no remaining root result."))
            result)))))

(-> rlm-litmus--corpus () string)
(defun rlm-litmus--corpus ()
  "Return a four-block corpus with markers never straddling slice edges."
  (with-output-to-string (stream)
    (dotimes (block-number 4)
      (let ((block-text
              (with-output-to-string (block-stream)
                (dotimes (line 760)
                  (format block-stream
                          "filler-~D-~4,'0D lorem ipsum dolor sit~%"
                          block-number line)
                  (when (zerop (mod line 89))
                    (format block-stream "XYZZY~%"))))))
        (write-string block-text stream)
        (loop repeat (- 40000 (length block-text))
              do (write-char #\. stream))))))

(-> test-rlm-litmus-completion () null)
(defun test-rlm-litmus-completion ()
  "Test the defining RLM property over an input beyond the provider window."
  (let* ((configuration (test-configuration))
         (corpus (rlm-litmus--corpus))
         (expected (rlm-litmus--count-occurrences "XYZZY" corpus))
         (window-limit 60000)
         (code
           "(finish (loop for start from 0 below (context-length) by 40000 sum (parse-integer (infer \"Count the marker occurrences and reply with only the integer.\" :context (list (context-slice start (min (context-length) (+ start 40000))))))))")
         (provider
           (make-instance
            'rlm-litmus-provider
            :window-limit window-limit
            :root-results
            (list (agent-test-result
                   "root-1"
                   (list (agent-test-call
                          :call-id "eval-1"
                          :namespace "env"
                          :name "eval"
                          :arguments (json-encode
                                      (json-object "form" code))))))))
          ;; Exactly one root request plus four sub-inferences: a finish on
          ;; the last remaining call must end the run without another request.
          (budget (rlm-budget-create :calls 5 :tokens 100000 :depth 2))
          (activities nil))
    (test-assert (= (length corpus) 160000)
                 "the corpus is four exact provider-window-sized blocks")
    (test-assert (> (length corpus) window-limit)
                 "the corpus exceeds the provider window")
    (test-assert (plusp expected)
                 "the corpus contains markers to count")
    (multiple-value-bind (value trace-identifier)
        (rlm-complete "Count how many marker lines the corpus contains."
                      :context (list ':label "corpus" ':content corpus)
                      :budget budget
                      :provider provider
                      :configuration configuration
                      :activity-callback
                      (lambda (activity)
                        (push activity activities)))
      (test-assert (eql value expected)
                   "the recursive run returns the exact marker count")
      (test-assert (<= (rlm-litmus-provider-largest-request provider)
                       window-limit)
                   "no provider request exceeded the context window")
      (test-assert (= (rlm-litmus-provider-request-count provider) 5)
                   "the run used four sub-inferences and one root request")
      (test-assert (zerop (rlm-budget-remaining-calls budget))
                   "a finish on the last call ends the run without failing")
      (test-assert
       (some (lambda (activity)
               (search "infer · request" activity))
             activities)
       "root completion reports proxied sub-inference request progress")
      (let ((trace (rlm--trace-content configuration trace-identifier)))
        (test-assert (and trace (not (search "lorem" trace)))
                     "corpus content never entered the root conversation")
        (test-assert (search "context-slice" trace)
                     "the root model authored the decomposition")
        (test-assert (and (search ":RLM-CALL" trace)
                          (search ":CHILD-TRACE" trace))
                     "the root trace carries the run's invocation ledger"))))
  nil)

(-> test-rlm-boundary-litmus () null)
(defun test-rlm-boundary-litmus ()
  "Test overlapping slices count evidence crossing naive slice boundaries."
  (let* ((configuration (test-configuration))
         (corpus (rlm-litmus--corpus))
         (window-limit 60000)
         (code
           "(finish (let* ((size 40000) (overlap 100) (step (- size overlap))) (- (loop for start from 0 below (context-length) by step sum (parse-integer (infer \"Count the marker occurrences and reply with only the integer.\" :context (list (context-slice start (min (context-length) (+ start size))))))) (loop for start from step below (context-length) by step sum (parse-integer (infer \"Count the marker occurrences and reply with only the integer.\" :context (list (context-slice start (min (context-length) (+ start overlap))))))))))")
         (provider
           (make-instance
            'rlm-litmus-provider
            :window-limit window-limit
            :root-results
            (list (agent-test-result
                   "root-1"
                   (list (agent-test-call
                          :call-id "eval-1"
                          :namespace "env"
                          :name "eval"
                          :arguments (json-encode
                                      (json-object "form" code))))))))
         (budget (rlm-budget-create :calls 12 :tokens 100000 :depth 2)))
    ;; Plant markers straddling the naive 40000-character boundaries; the
    ;; base corpus pads block edges with dots, so the splice is exact.
    (replace corpus "XYZZY" :start1 39998)
    (replace corpus "XYZZY" :start1 79998)
    (let ((expected (rlm-litmus--count-occurrences "XYZZY" corpus))
          (naive (loop for start from 0 below (length corpus) by 40000
                       sum (rlm-litmus--count-occurrences
                            "XYZZY"
                            (subseq corpus start
                                    (min (length corpus) (+ start 40000)))))))
      (test-assert (> expected naive)
                   "the planted markers straddle naive slice boundaries")
      (multiple-value-bind (value trace-identifier)
          (rlm-complete "Count how many marker lines the corpus contains."
                        :context (list ':label "corpus" ':content corpus)
                        :budget budget
                        :provider provider
                        :configuration configuration)
        (declare (ignore trace-identifier))
        (test-assert (eql value expected)
                     "overlapping slices recover boundary-straddling evidence")
        (test-assert (<= (rlm-litmus-provider-largest-request provider)
                         window-limit)
                     "boundary-aware slicing still fits the provider window"))))
  nil)

(-> test-rlm-complete-tool () null)
(defun test-rlm-complete-tool ()
  "Test rlm.complete runs a root completion from tool arguments."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "rlm-complete-tool"))
         (activities nil)
         (observer
           (callback-agent-observer-create
            :status-callback
            (lambda (status details)
              (when (eq status ':tool-call-progress)
                (push (getf details :activity) activities)))))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry (make-instance 'tool-registry)
                                 :observer observer))
         (provider
           (make-instance
            'rlm-litmus-provider
            :window-limit 100000
            :root-results
            (list (agent-test-result
                   "root-1"
                   (list (agent-test-call
                          :call-id "eval-1"
                          :namespace "env"
                          :name "eval"
                          :arguments (json-encode
                                      (json-object
                                       "form"
                                       "(finish (context-length))"))))))))
         (tool (rlm-complete-tool-create :provider provider))
         (result
           (tool-execute
            tool
            context
            (json-object
             "task" "Measure the corpus."
             "context" (json-object
                        "label" "corpus"
                        "text" (make-string 200 :initial-element #\x))
             "calls" 6))))
    (test-assert (tool-result-success-p result)
                 "rlm.complete succeeds when the run records a value")
    (test-assert (and (search ":VALUE 200" (tool-result-content result))
                      (search ":TRACE" (tool-result-content result)))
                 "rlm.complete reports the recorded value and root trace")
    (test-assert
     (and (member "rlm.complete · starting environment"
                  activities :test #'string=)
          (member "rlm.complete · request 1 · 5 calls left"
                  activities :test #'string=))
     "rlm.complete publishes environment startup and root request progress")
    (test-assert (not (tool-result-success-p
                       (tool-execute tool context
                                     (json-object "task" "No context."))))
                 "rlm.complete refuses a missing context argument"))
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "rlm-big-value"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry (make-instance 'tool-registry)))
         (provider
           (make-instance
            'rlm-litmus-provider
            :window-limit 100000
            :root-results
            (list (agent-test-result
                   "root-1"
                   (list (agent-test-call
                          :call-id "eval-1"
                          :namespace "env"
                          :name "eval"
                          :arguments
                          (json-encode
                           (json-object
                            "form"
                            "(finish (make-string 20000 :initial-element #\\x))"))))))))
         (result (tool-execute (rlm-complete-tool-create :provider provider)
                               context
                               (json-object
                                "task" "Produce a huge value."
                                "context" (json-object "text" "small")
                                "calls" 4))))
    (test-assert (tool-result-success-p result)
                 "rlm.complete succeeds on an oversized final value")
    (let ((content (tool-result-content result)))
      (test-assert (and (search ":VALUE-CONTEXT" content)
                        (search "context:" content)
                        (search ":VALUE-PREVIEW" content))
                   "oversized final values are externalized with a preview")
      (test-assert (< (length content) 15000)
                   "externalized results stay bounded in the tool result")))
  nil)

(-> test-rlm-designator-confinement () null)
(defun test-rlm-designator-confinement ()
  "Test model-visible designators stay inside the resource protocol."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "rlm-designators"))
         (registry (make-instance 'tool-registry))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry registry)))
    (resource-registry-register
     (tool-registry-resource-registry registry)
     (make-instance 'workspace-file-resolver :scheme "workspace"))
    (resource-registry-register
     (tool-registry-resource-registry registry)
     (make-instance 'context-object-resolver :scheme "context"))
    (flet ((run (view &key (calls 3))
             (tool-execute
              (rlm-infer-tool-create
               :provider (make-instance
                          'rlm-inference-test-provider
                          :results (list (rlm-inference-test-result
                                          "response" "designator answer"
                                          10))))
              context
              (json-object "task" "Describe the view."
                           "views" (json-array view)
                           "calls" calls))))
      (test-assert (not (tool-result-success-p
                         (run (json-object "path" "/etc/passwd"))))
                   "raw filesystem paths are not a model-visible designator")
      (test-assert (not (tool-result-success-p
                         (run (json-object "text" "inline"
                                           "uri" "workspace:README.org"))))
                   "views supplying several source fields are refused")
      (test-assert (tool-result-success-p
                    (run (json-object "uri" "workspace:README.org")))
                   "resource uris materialize through the resource protocol")
      (test-assert (let ((*resource-readable-schemes* (list "scratchpad")))
                     (not (tool-result-success-p
                           (run (json-object "uri" "workspace:README.org")))))
                   "restricted scheme sets confine uri designators")
      (let ((object (rlm-context-intern configuration
                                        "stored designator content")))
        (test-assert (tool-result-success-p
                      (run (json-object
                            "object"
                            (format nil "context:~A"
                                    (rlm-context-object-digest object)))))
                     "stored context objects are a model-visible designator")
        (test-assert (not (tool-result-success-p
                           (run (json-object "object" "context:00ff"))))
                     "unknown context object digests are refused"))))
  nil)
