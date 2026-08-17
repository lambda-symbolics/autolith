(in-package #:autolith)

;;;; -- Recursive Inference Tests --

(-> test-rlm-budget-accounting () null)
(defun test-rlm-budget-accounting ()
  "Test call and token charges drain a shared budget pool."
  (let ((budget (rlm-budget-create :calls 2 :tokens 100 :depth 1)))
    (test-assert (= (rlm-budget-remaining-calls budget) 2)
                 "a fresh budget reports its full call allowance")
    (rlm-budget-charge-call budget :task "count")
    (test-assert (= (rlm-budget-remaining-calls budget) 1)
                 "charging a call decrements the shared pool")
    (rlm-budget-charge-tokens budget 40)
    (test-assert (= (rlm-budget-remaining-tokens budget) 60)
                 "token charges decrement the shared pool")
    (rlm-budget-charge-tokens budget 900)
    (test-assert (= (rlm-budget-remaining-tokens budget) 0)
                 "token overdraft clamps at zero instead of going negative")
    (test-assert (handler-case
                     (progn (rlm-budget-charge-call budget) nil)
                   (rlm-budget-exhausted (condition)
                     (eq (rlm-budget-exhausted-dimension condition) ':tokens))
                   (error () nil))
                 "a drained token pool refuses the next call reservation"))
  (let ((budget (rlm-budget-create :calls 1 :tokens 100 :depth 1)))
    (rlm-budget-charge-call budget)
    (test-assert (handler-case
                     (progn (rlm-budget-charge-call budget :task "again") nil)
                   (rlm-budget-exhausted (condition)
                     (and (eq (rlm-budget-exhausted-dimension condition)
                              ':calls)
                          (equal (rlm-budget-exhausted-task condition)
                                 "again")))
                   (error () nil))
                 "a drained call pool refuses further calls and names the task"))
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
