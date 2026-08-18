(in-package #:autolith)

;;;; -- Recursive Inference Budgets --

(defparameter *rlm-default-call-budget* 8
  "The default number of provider calls one inference subtree may spend.")

(defparameter *rlm-default-token-budget* 80000
  "The default combined token allowance for one inference subtree.")

(defparameter *rlm-default-depth-budget* 2
  "The default remaining recursion depth below a root inference frame.")

(defparameter *rlm-output-reserve-tokens* 16000
  "The output token tranche reserved from the pool per provider request.")

(define-condition rlm-budget-exhausted
    (error)
  ((dimension
    :initarg :dimension
    :reader rlm-budget-exhausted-dimension
    :type keyword
    :documentation "The exhausted allowance: :calls, :tokens, or :depth.")
   (task
    :initarg :task
    :initform nil
    :reader rlm-budget-exhausted-task
    :type (option string)
    :documentation "The inference task that requested the exceeded allowance, when known."))
  (:documentation "An inference subtree spent its shared budget.")
  (:report
   (lambda (condition stream)
     (format stream "The inference ~A budget is exhausted~@[ for task ~S~]."
             (string-downcase
              (symbol-name (rlm-budget-exhausted-dimension condition)))
             (rlm-budget-exhausted-task condition)))))

(defclass rlm-budget-pool ()
  ((lock
    :initform (make-lock "Autolith inference budget")
    :reader rlm-budget-pool--lock
    :documentation "The lock serializing charges from concurrent frames.")
   (calls-remaining
    :initarg :calls-remaining
    :accessor rlm-budget-pool--calls-remaining
    :type (integer 0)
    :documentation "The provider calls the subtree may still spend.")
   (tokens-remaining
    :initarg :tokens-remaining
    :accessor rlm-budget-pool--tokens-remaining
    :type (integer 0)
    :documentation "The combined tokens the subtree may still spend."))
  (:documentation
   "The call and token counters shared by every budget in one subtree."))

(defclass rlm-budget ()
  ((pool
    :initarg :pool
    :reader rlm-budget--pool
    :type rlm-budget-pool
    :documentation "The subtree-shared call and token counters.")
   (depth-remaining
    :initarg :depth-remaining
    :reader rlm-budget-remaining-depth
    :type (integer 0)
    :documentation "The recursion levels still allowed below this budget."))
  (:documentation
   "One inference frame's view of a shared subtree budget."))

(-> rlm-budget-create
    (&key (:calls (integer 1)) (:tokens (integer 1)) (:depth (integer 0)))
    rlm-budget)
(defun rlm-budget-create
    (&key (calls *rlm-default-call-budget*)
          (tokens *rlm-default-token-budget*)
          (depth *rlm-default-depth-budget*))
  "Create a root inference budget of CALLS, TOKENS, and recursion DEPTH."
  (make-instance 'rlm-budget
                 :pool (make-instance 'rlm-budget-pool
                                      :calls-remaining calls
                                      :tokens-remaining tokens)
                 :depth-remaining depth))

(-> rlm-budget-remaining-calls (rlm-budget) (integer 0))
(defun rlm-budget-remaining-calls (budget)
  "Return the provider calls BUDGET's subtree may still spend."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (rlm-budget-pool--calls-remaining pool))))

(-> rlm-budget-remaining-tokens (rlm-budget) (integer 0))
(defun rlm-budget-remaining-tokens (budget)
  "Return the combined tokens BUDGET's subtree may still spend."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (rlm-budget-pool--tokens-remaining pool))))

(-> rlm-budget-acquire-call
    (rlm-budget &key (:task (option string)))
    rlm-budget)
(defun rlm-budget-acquire-call (budget &key task)
  "Atomically reserve one provider call from BUDGET before issuing it.

Signal RLM-BUDGET-EXHAUSTED when no calls remain, or when earlier
responses already drained the token allowance. The reservation and the
check happen under one lock, so concurrent frames can never spend more
calls than the subtree allocation."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (when (zerop (rlm-budget-pool--calls-remaining pool))
        (error 'rlm-budget-exhausted :dimension ':calls :task task))
      (when (zerop (rlm-budget-pool--tokens-remaining pool))
        (error 'rlm-budget-exhausted :dimension ':tokens :task task))
      (decf (rlm-budget-pool--calls-remaining pool))))
  budget)

(-> rlm-budget-reserve-output (rlm-budget) (integer 16))
(defun rlm-budget-reserve-output (budget)
  "Atomically reserve one request's output tranche from BUDGET.

The tranche is the request's advertised provider output ceiling.
Concurrent frames each hold their own tranche while their requests
run, so combined output can never dramatically overrun the pool; a
small floor keeps tiny remainders acceptable to provider validation.
Settle the tranche with RLM-BUDGET-SETTLE-OUTPUT once usage is known."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (let ((tranche (max 16 (min *rlm-output-reserve-tokens*
                                  (rlm-budget-pool--tokens-remaining pool)))))
        (setf (rlm-budget-pool--tokens-remaining pool)
              (max 0 (- (rlm-budget-pool--tokens-remaining pool) tranche)))
        tranche))))

(-> rlm-budget-settle-output
    (rlm-budget (integer 0) (option (integer 0)))
    rlm-budget)
(defun rlm-budget-settle-output (budget tranche usage-total)
  "Settle one request's TRANCHE against its reported USAGE-TOTAL.

The tranche refund and the actual charge happen in one atomic step.
A NIL USAGE-TOTAL refunds the whole tranche, matching failed requests
and providers that report no usage; input tokens therefore stay post
hoc, gating the next reservation rather than the current one."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (setf (rlm-budget-pool--tokens-remaining pool)
            (max 0 (+ (rlm-budget-pool--tokens-remaining pool)
                      (- tranche (or usage-total 0)))))))
  budget)

(-> rlm-budget-descend (rlm-budget &key (:task (option string))) rlm-budget)
(defun rlm-budget-descend (budget &key task)
  "Return BUDGET one recursion level down, sharing its call and token pool.

Signal RLM-BUDGET-EXHAUSTED when no recursion depth remains."
  (when (zerop (rlm-budget-remaining-depth budget))
    (error 'rlm-budget-exhausted :dimension ':depth :task task))
  (make-instance 'rlm-budget
                 :pool (rlm-budget--pool budget)
                 :depth-remaining (1- (rlm-budget-remaining-depth budget))))
