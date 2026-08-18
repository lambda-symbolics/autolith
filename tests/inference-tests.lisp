(in-package #:autolith)

;;;; -- Recursive Inference Tests --

(-> test-rlm-budget-accounting () null)
(defun test-rlm-budget-accounting ()
  "Test atomic request acquisition and settlement over a shared pool."
  (let ((budget (rlm-budget-create :calls 2 :tokens 100 :depth 1)))
    (test-assert (= (rlm-budget-remaining-calls budget) 2)
                 "a fresh budget reports its full call allowance")
    (let ((tranche (rlm-budget-acquire-request budget)))
      (test-assert (= tranche 25)
                   "a reservation takes its share of the remaining pool")
      (test-assert (= (rlm-budget-remaining-calls budget) 1)
                   "acquiring reserves the call from the shared pool")
      (test-assert (= (rlm-budget-remaining-tokens budget) 75)
                   "the reserved tranche leaves the shared pool")
      (rlm-budget-settle-output budget tranche 40)
      (test-assert (= (rlm-budget-remaining-tokens budget) 60)
                   "settling refunds the tranche and charges actual usage"))
    (let ((tranche (rlm-budget-acquire-request budget)))
      (test-assert (= tranche 16)
                   "small shares are lifted to the provider validation floor")
      (rlm-budget-settle-output budget tranche nil)
      (test-assert (= (rlm-budget-remaining-tokens budget) 60)
                   "settling without usage refunds the whole tranche"))
    (test-assert (handler-case
                     (progn (rlm-budget-acquire-request budget :task "again")
                            nil)
                   (rlm-budget-exhausted (condition)
                     (and (eq (rlm-budget-exhausted-dimension condition)
                              ':calls)
                          (equal (rlm-budget-exhausted-task condition)
                                 "again")))
                   (error () nil))
                 "a drained call pool refuses the next request and names the task")
    (test-assert (= (rlm-budget-remaining-tokens budget) 60)
                 "a refused request spends nothing"))
  (let ((budget (rlm-budget-create :calls 4 :tokens 50 :depth 1)))
    (rlm-budget-settle-output budget (rlm-budget-acquire-request budget) 900)
    (test-assert (= (rlm-budget-remaining-tokens budget) 0)
                 "token overdraft clamps at zero instead of going negative")
    (test-assert (handler-case
                     (progn (rlm-budget-acquire-request budget) nil)
                   (rlm-budget-exhausted (condition)
                     (eq (rlm-budget-exhausted-dimension condition) ':tokens))
                   (error () nil))
                 "a drained token pool refuses the next request")
    (test-assert (= (rlm-budget-remaining-calls budget) 3)
                 "a refused request reserves no call"))
  nil)

(-> test-rlm-budget-contention () null)
(defun test-rlm-budget-contention ()
  "Test concurrent acquisition never oversubscribes a tiny token pool."
  (let* ((budget (rlm-budget-create :calls 16 :tokens 100 :depth 1))
         (results-lock (make-lock "Autolith budget contention test"))
         (tranches nil)
         (refusals 0))
    (mapc #'join-thread
          (loop repeat 8
                collect
                (make-thread
                 (lambda ()
                   (handler-case
                       (let ((tranche (rlm-budget-acquire-request budget)))
                         (with-lock-held (results-lock)
                           (push tranche tranches)))
                     (rlm-budget-exhausted ()
                       (with-lock-held (results-lock)
                         (incf refusals)))))
                 :name "autolith-budget-contention")))
    (test-assert (= (+ (length tranches) refusals) 8)
                 "every competing worker either reserves or is refused")
    (test-assert (<= (reduce #'+ tranches :initial-value 0) 100)
                 "combined reservations never exceed the token pool")
    (test-assert (plusp (length tranches))
                 "at least one competing worker wins the pool"))
  nil)

(-> test-rlm-context-views () null)
(defun test-rlm-context-views ()
  "Test view designators materialize with labels, digests, and rendering."
  (let ((views (rlm-views-materialize
                (list "first literal"
                      (list ':label "notes" ':content "second literal")))))
    (test-assert (equal (mapcar #'rlm-view-label views) '("literal" "notes"))
                 "strings and labeled plists keep their labels")
    (test-assert (string= (rlm-view-digest (first views))
                          (rlm-view--digest "first literal"))
                 "views carry the content digest")
    (let ((rendered (rlm-views-render views)))
      (test-assert (and (search "label=\"notes\"" rendered)
                        (search "second literal" rendered))
                   "rendering includes labels and exact content")))
  (test-assert (equal (mapcar #'rlm-view-label
                              (rlm-views-materialize (list "one" "two")))
                      '("literal#1" "literal#2"))
               "duplicate labels are numbered deterministically")
  (uiop:with-temporary-file (:pathname pathname :stream stream :keep nil
                             :prefix "autolith-rlm-view")
    (write-string "file view content" stream)
    (finish-output stream)
    :close-stream
    (let ((view (rlm-view-materialize pathname)))
      (test-assert (string= (rlm-view-content view) "file view content")
                   "pathname designators read the file at call time")
      (test-assert (string= (rlm-view-origin view) (namestring pathname))
                   "pathname views record their origin")))
  (test-assert (handler-case
                   (progn
                     (rlm-view-materialize #p"/nonexistent/rlm-view-test")
                     nil)
                 (rlm-view-error () t)
                 (error () nil))
               "unreadable files signal a view error")
  (test-assert (handler-case
                   (progn (rlm-view-materialize 42) nil)
                 (rlm-view-error () t)
                 (error () nil))
               "unsupported designators signal a view error")
  nil)

(defclass rlm-inference-test-provider (model-provider)
  ((results
    :initarg :results
    :accessor rlm-inference-test-provider-results
    :type list
    :documentation "The provider results returned in request order.")
   (output-limits
    :initform nil
    :accessor rlm-inference-test-provider-output-limits
    :type list
    :documentation "The bound output ceilings observed per request, newest first."))
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
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (push *provider-maximum-output-tokens*
        (rlm-inference-test-provider-output-limits provider))
  (let ((result (pop (rlm-inference-test-provider-results provider))))
    (unless result
      (error "The inference test provider has no remaining result."))
    result))

(-> rlm-inference-test-result (string string (integer 0)) provider-result)
(defun rlm-inference-test-result (response-id text total-tokens)
  "Return a scripted assistant TEXT result reporting TOTAL-TOKENS usage."
  (make-instance 'provider-result
                 :response-id response-id
                 :output-items (list (agent-test-message text))
                 :tool-calls nil
                 :usage (json-object "total_tokens" total-tokens)
                 :turn-state nil
                 :turn-completion ':unspecified))

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
          (budget (rlm-budget-create :calls 4 :tokens 1000 :depth 1)))
      (multiple-value-bind (value trace-identifier)
          (infer "Answer the question."
                 :context (list "the question is six times seven")
                 :contract '(:type :object
                             :properties (("answer" (:type :string)))
                             :required ("answer"))
                 :budget budget
                 :provider provider
                 :configuration configuration)
        (test-assert (equal value '(:object ("answer" "42")))
                     "schema contracts return portable tagged native data")
        (test-assert (= (rlm-budget-remaining-calls budget) 2)
                     "the repair round charges a second call")
        (test-assert (= (rlm-budget-remaining-tokens budget) 750)
                     "reported usage drains the token pool")
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
             :results (list (rlm-inference-test-result "resp-1" "plain" 10)))))
      (test-assert (string= (infer "Say plain."
                                   :provider provider
                                   :configuration configuration)
                            "plain")
                   "text contracts return the trimmed answer"))
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
      (test-assert (= (rlm-budget-remaining-tokens budget) 900)
                   "reported frame usage drains the token pool")))
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
         (results
           (rlm-map (list "alpha"
                          (list ':task "beta"
                                ':context (list "beta extra view"))
                          "gamma explode"
                          "delta")
                    :budget budget
                    :provider provider
                    :configuration configuration
                    :concurrency 3)))
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
                 "the three completed frames drained the shared token pool"))
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
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry (make-instance 'tool-registry))))
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
                     "the trace carries the frame request verbatim")))
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
  "Test model permission decisions map to keywords and fail toward asking."
  (let ((configuration (test-configuration)))
    (flet ((classify (&rest texts)
             (permissions-model-classify-command
              "cargo build" #p"/root/project/"
              :provider (make-instance
                         'rlm-inference-test-provider
                         :results
                         (mapcar (lambda (text)
                                   (rlm-inference-test-result "response"
                                                              text 20))
                                 texts))
              :configuration configuration)))
      (multiple-value-bind (decision reason)
          (classify
           "{\"decision\": \"sandboxed\", \"reason\": \"workspace build\"}")
        (test-assert (eq decision ':sandboxed)
                     "sandbox decisions map to the sandboxed grant")
        (test-assert (string= reason "workspace build")
                     "the model's reason is passed through"))
      (test-assert (eq (classify
                        "{\"decision\": \"full\", \"reason\": \"network\"}")
                       ':full-access)
                   "full decisions map to the full-access grant")
      (test-assert (eq (classify
                        "{\"decision\": \"deny\", \"reason\": \"wipe\"}")
                       ':deny)
                   "deny decisions map to the deny grant")
      (test-assert (eq (classify
                        "{\"decision\": \"ask\", \"reason\": \"unclear\"}")
                       ':ask)
                   "ask decisions defer to the human")
      (multiple-value-bind (decision reason)
          (classify "garbage" "still garbage")
        (test-assert (eq decision ':ask)
                     "unrepaired classifier output falls back to asking")
        (test-assert (non-empty-string-p reason)
                     "the fallback carries an explanation"))
      (test-assert (eq (classify) ':ask)
                   "provider failures fall back to asking")))
  nil)

(-> test-rlm-context-objects () null)
(defun test-rlm-context-objects ()
  "Test content-addressed context objects intern once and read back."
  (let* ((configuration (test-configuration))
         (first-object (rlm-context-intern configuration "shared corpus"
                                           :label "corpus"))
         (second-object (rlm-context-intern configuration "shared corpus")))
    (test-assert (string= (rlm-context-object-digest first-object)
                          (rlm-context-object-digest second-object))
                 "identical content interns to one digest")
    (test-assert (equal (rlm-context-object-pathname first-object)
                        (rlm-context-object-pathname second-object))
                 "identical content shares one stored file")
    (test-assert (= (length (directory
                             (merge-pathnames
                              (make-pathname :name ':wild :type "txt")
                              (rlm-object-root configuration))))
                    1)
                 "repeated interning never duplicates storage")
    (test-assert (string= (uiop:read-file-string
                           (rlm-context-object-pathname first-object))
                          "shared corpus")
                 "the stored object carries the exact content")
    (test-assert (= (rlm-context-object-characters first-object) 13)
                 "object handles report the exact character count")
    (let ((found (rlm-context-object-find
                  configuration
                  (rlm-context-object-digest first-object))))
      (test-assert (and found
                        (= (rlm-context-object-characters found) 13))
                   "stored objects are findable by digest"))
    (test-assert (null (rlm-context-object-find configuration
                                                (rlm-view--digest "absent")))
                 "unknown digests find nothing")
    (let ((pathname (rlm-context-object-pathname first-object)))
      (test-assert (zerop (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring pathname)))
                                  #o222))
                   "stored objects are read-only on disk")
      (sb-posix:chmod (namestring pathname) #o644)
      (with-open-file (stream pathname :direction ':output
                                       :if-exists ':supersede)
        (write-string "tampered" stream))
      (test-assert (handler-case
                       (progn
                         (rlm-context-object-find
                          configuration
                          (rlm-context-object-digest first-object))
                         nil)
                     (rlm-view-error () t)
                     (error () nil))
                   "a mutated stored object is detected, not trusted")
      (rlm-context-intern configuration "shared corpus")
      (test-assert (string= (uiop:read-file-string pathname) "shared corpus")
                   "re-interning repairs a mutated stored object")
      (test-assert (zerop (logand (sb-posix:stat-mode
                                   (sb-posix:stat (namestring pathname)))
                                  #o222))
                   "a repaired object is read-only again"))
    (test-assert (handler-case
                     (progn
                       (rlm-context-designator-object configuration 42)
                       nil)
                   (rlm-view-error () t)
                   (error () nil))
                 "unsupported root context designators are refused"))
  nil)

(-> rlm-endpoint-test-call (rlm-endpoint list) list)
(defun rlm-endpoint-test-call (endpoint request)
  "Send one raw REQUEST packet to ENDPOINT and return its response."
  (multiple-value-bind (socket stream)
      (localgroup-connect (rlm-endpoint-port endpoint))
    (declare (ignore socket))
    (unwind-protect
         (progn
           (localgroup-write-packet stream request)
           (localgroup-read-packet stream))
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
           (view-end (search "</view>" text :from-end t)))
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
         (budget (rlm-budget-create :calls 5 :tokens 100000 :depth 2)))
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
                      :configuration configuration)
      (test-assert (eql value expected)
                   "the recursive run returns the exact marker count")
      (test-assert (<= (rlm-litmus-provider-largest-request provider)
                       window-limit)
                   "no provider request exceeded the context window")
      (test-assert (= (rlm-litmus-provider-request-count provider) 5)
                   "the run used four sub-inferences and one root request")
      (test-assert (zerop (rlm-budget-remaining-calls budget))
                   "a finish on the last call ends the run without failing")
      (let ((trace (rlm--trace-content configuration trace-identifier)))
        (test-assert (and trace (not (search "lorem" trace)))
                     "corpus content never entered the root conversation")
        (test-assert (search "context-slice" trace)
                     "the root model authored the decomposition")
        (test-assert (and (search ":RLM-CALL" trace)
                          (search ":CHILD-TRACE" trace))
                     "the root trace carries the run's invocation ledger"))))
  nil)

(-> test-rlm-complete-tool () null)
(defun test-rlm-complete-tool ()
  "Test rlm.complete runs a root completion from tool arguments."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "rlm-complete-tool"))
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
    (test-assert (not (tool-result-success-p
                       (tool-execute tool context
                                     (json-object "task" "No context."))))
                 "rlm.complete refuses a missing context argument"))
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

(-> test-rlm-budget-descent () null)
(defun test-rlm-budget-descent ()
  "Test descended budgets share counters and bound recursion depth."
  (let* ((root (rlm-budget-create :calls 4 :tokens 100 :depth 1))
         (child (rlm-budget-descend root)))
    (test-assert (= (rlm-budget-remaining-depth child) 0)
                 "descending decrements the remaining depth")
    (rlm-budget-acquire-request child)
    (test-assert (= (rlm-budget-remaining-calls root) 3)
                 "child charges drain the root's shared pool")
    (test-assert (handler-case
                     (progn (rlm-budget-descend child) nil)
                   (rlm-budget-exhausted (condition)
                     (eq (rlm-budget-exhausted-dimension condition) ':depth))
                   (error () nil))
                 "depth zero refuses further descent"))
  nil)
