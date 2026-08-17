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
