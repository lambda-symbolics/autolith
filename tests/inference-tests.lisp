(in-package #:autolith)

;;;; -- Recursive Inference Tests --

(-> test-rlm-budget-accounting () null)
(defun test-rlm-budget-accounting ()
  "Test call and token charges drain a shared budget pool."
  (let ((budget (rlm-budget-create :calls 2 :tokens 100 :depth 1)))
    (test-assert (= (rlm-budget-remaining-calls budget) 2)
                 "a fresh budget reports its full call allowance")
    (rlm-budget-charge-call budget)
    (test-assert (= (rlm-budget-remaining-calls budget) 1)
                 "charging a call decrements the shared pool")
    (rlm-budget-charge-tokens budget 40)
    (test-assert (= (rlm-budget-remaining-tokens budget) 60)
                 "token charges decrement the shared pool")
    (rlm-budget-charge-tokens budget 900)
    (test-assert (= (rlm-budget-remaining-tokens budget) 0)
                 "token overdraft clamps at zero instead of going negative")
    (test-assert (handler-case
                     (progn (rlm-budget-ensure budget) nil)
                   (rlm-budget-exhausted (condition)
                     (eq (rlm-budget-exhausted-dimension condition) ':tokens))
                   (error () nil))
                 "a drained token pool refuses the next call gate"))
  (let ((budget (rlm-budget-create :calls 1 :tokens 100 :depth 1)))
    (rlm-budget-ensure budget)
    (rlm-budget-charge-call budget)
    (rlm-budget-charge-call budget)
    (test-assert (= (rlm-budget-remaining-calls budget) 0)
                 "post-response call charges clamp at zero without signaling")
    (test-assert (handler-case
                     (progn (rlm-budget-ensure budget :task "again") nil)
                   (rlm-budget-exhausted (condition)
                     (and (eq (rlm-budget-exhausted-dimension condition)
                              ':calls)
                          (equal (rlm-budget-exhausted-task condition)
                                 "again")))
                   (error () nil))
                 "a drained call pool refuses the next gate and names the task"))
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
    :documentation "The provider results returned in request order."))
  (:documentation "A deterministic provider for exercising inference frames."))

(defmethod provider-stream-turn
    ((provider rlm-inference-test-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return PROVIDER's next scripted inference result."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
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
                          '("resource.read" "rlm.infer" "search.content"))
                   "frames keep read-only tools and gain nested rlm.infer")
      (let ((nested (tool-registry-find registry "rlm" "infer")))
        (test-assert (eq (rlm-infer-tool--budget nested) budget)
                     "the nested rlm.infer tool shares the frame budget"))))
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
    (test-assert (= (rlm-budget-remaining-calls budget) 7)
                 "the three completed frames drained the shared call pool")
    (test-assert (= (rlm-budget-remaining-tokens budget) 970)
                 "the three completed frames drained the shared token pool"))
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

(-> test-rlm-budget-descent () null)
(defun test-rlm-budget-descent ()
  "Test descended budgets share counters and bound recursion depth."
  (let* ((root (rlm-budget-create :calls 4 :tokens 100 :depth 1))
         (child (rlm-budget-descend root)))
    (test-assert (= (rlm-budget-remaining-depth child) 0)
                 "descending decrements the remaining depth")
    (rlm-budget-charge-call child)
    (test-assert (= (rlm-budget-remaining-calls root) 3)
                 "child charges drain the root's shared pool")
    (test-assert (handler-case
                     (progn (rlm-budget-descend child) nil)
                   (rlm-budget-exhausted (condition)
                     (eq (rlm-budget-exhausted-dimension condition) ':depth))
                   (error () nil))
                 "depth zero refuses further descent"))
  nil)
