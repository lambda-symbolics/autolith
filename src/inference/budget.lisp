(in-package #:autolith)

;;;; -- Recursive Inference Budgets --

(defparameter *rlm-default-call-budget* 8
  "The default number of provider calls one inference subtree may spend.")

(defparameter *rlm-default-token-budget* 80000
  "The default combined token allowance for one inference subtree.")

(defparameter *rlm-default-depth-budget* 2
  "The default remaining recursion depth below a root inference frame.")

(defparameter *rlm-output-reserve-tokens* 16000
  "The largest output token tranche reserved per provider request.")

(defparameter *rlm-output-reserve-share* 4
  "The fraction of the remaining pool one reservation may take.

Reserving only a share leaves headroom for concurrent siblings, so a
small shared pool admits parallel fan-out instead of letting the
first request drain it and starve the rest.")

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
    :type integer
    :documentation "The combined tokens the subtree may still spend.

The internal balance may carry debt below zero while overdrafts and
outstanding reservations settle; readers clamp it to zero."))
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
      (max 0 (rlm-budget-pool--tokens-remaining pool)))))

(-> rlm-budget-acquire-request
    (rlm-budget &key (:task (option string)))
    (integer 1))
(defun rlm-budget-acquire-request (budget &key task)
  "Atomically reserve one provider call and its output tranche from BUDGET.

Return the tranche, which is also the request's advertised provider
output ceiling. The tranche takes at most a configured share of the
remaining pool, leaving headroom for concurrent siblings, and never
more than the pool holds, so combined reservations cannot
oversubscribe the subtree allocation; only a nearly dry pool can
yield a ceiling below common provider minimums, at which point the
budget is ending anyway. The exhaustion checks, the call decrement,
and the tranche reservation happen under one lock. Signal
RLM-BUDGET-EXHAUSTED when no calls remain, or when earlier responses
already drained the token allowance. Settle the tranche with
RLM-BUDGET-SETTLE-OUTPUT once usage is known."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (when (zerop (rlm-budget-pool--calls-remaining pool))
        (error 'rlm-budget-exhausted :dimension ':calls :task task))
      (unless (plusp (rlm-budget-pool--tokens-remaining pool))
        (error 'rlm-budget-exhausted :dimension ':tokens :task task))
      (decf (rlm-budget-pool--calls-remaining pool))
      (let* ((remaining (rlm-budget-pool--tokens-remaining pool))
             (tranche
               (min remaining
                    (max 16 (min *rlm-output-reserve-tokens*
                                 (ceiling remaining
                                          *rlm-output-reserve-share*))))))
        (setf (rlm-budget-pool--tokens-remaining pool)
              (- remaining tranche))
        tranche))))

(-> rlm-budget-settle-output
    (rlm-budget (integer 0) (option (integer 0)))
    rlm-budget)
(defun rlm-budget-settle-output (budget tranche usage-total)
  "Settle one request's TRANCHE against its reported USAGE-TOTAL.

The tranche refund and the actual charge happen in one atomic step.
A NIL USAGE-TOTAL refunds the whole tranche, matching failed requests
and providers that report no usage; input tokens therefore stay post
hoc, gating the next reservation rather than the current one. The
balance carries overdraft debt below zero, so refunds of outstanding
reservations can never resurrect tokens an earlier settlement spent."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (incf (rlm-budget-pool--tokens-remaining pool)
            (- tranche (or usage-total 0)))))
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
