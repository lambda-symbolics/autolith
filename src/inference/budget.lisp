(in-package #:autolith)

;;;; -- Recursive Inference Budgets --

(defparameter *rlm-default-call-budget* 8
  "The default number of provider calls one inference subtree may spend.")

(defparameter *rlm-default-token-budget* 80000
  "The default combined token allowance for one inference subtree.")

(defparameter *rlm-default-depth-budget* 2
  "The default remaining recursion depth below a root inference frame.")

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

(-> rlm-budget-charge-tokens (rlm-budget (integer 0)) rlm-budget)
(defun rlm-budget-charge-tokens (budget tokens)
  "Record TOKENS spent by a completed response against BUDGET.

Token usage is only known after a response, so this never signals; a
drained allowance instead refuses the next RLM-BUDGET-ACQUIRE-CALL."
  (let ((pool (rlm-budget--pool budget)))
    (with-lock-held ((rlm-budget-pool--lock pool))
      (setf (rlm-budget-pool--tokens-remaining pool)
            (max 0 (- (rlm-budget-pool--tokens-remaining pool) tokens)))))
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
