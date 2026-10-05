(in-package #:autolith)

;;;; -- Concurrent Mission Inference --

(-> mission--available-tokens (list) integer)
(defun mission--available-tokens (allowance)
  "Return the unspent token allowance after outstanding output reservations."
  (- (getf allowance :token-limit)
     (getf allowance :tokens-used)
     (getf allowance :tokens-reserved 0)))

(-> mission--output-reservation (list) (integer 1))
(defun mission--output-reservation (allowance)
  "Divide available output tokens among remaining turns, honoring the caller's cap."
  (let ((available (mission--available-tokens allowance)))
    (min available
         (or *provider-maximum-output-tokens*
             (max 1 (floor available
                           (- (getf allowance :turn-limit)
                              (getf allowance :turns-used))))))))

(-> mission--reserve-inference (mission-context &key (:allowance (option list))) (integer 1))
(defun mission--reserve-inference (context &key allowance)
  "Reserve one turn and its output ceiling atomically before provider execution."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (with-recursive-lock-held ((mission-context-lock context))
      (mission--ensure-reservations (mission-context-goal context))
      (when allowance (mission--ensure-reservations allowance))
      (mission--admit context)
      (let* ((goal (mission-context-goal context))
             (limits (remove nil (list goal allowance))))
        (when (and allowance
                   (or (>= (getf allowance :turns-used) (getf allowance :turn-limit))
                       (>= (getf allowance :tokens-used) (getf allowance :token-limit))))
          (mission--reject ':review-budget "Independent reviewer allowance exhausted."))
        (unless (every (lambda (limit) (plusp (mission--available-tokens limit))) limits)
          (mission--reject ':budget-reserved "Outstanding requests hold the remaining output allowance."))
        (let ((reservation (reduce #'min limits :key #'mission--output-reservation)))
          (dolist (limit limits)
            (incf (getf limit :turns-used))
            (incf (getf limit :requests-outstanding 0))
            (incf (getf limit :tokens-reserved 0) reservation))
          (handler-case
              (progn (mission--record context) reservation)
            (error (condition)
              ;; No provider request was dispatched if admission cannot persist.
              (dolist (limit limits)
                (decf (getf limit :turns-used))
                (decf (getf limit :requests-outstanding))
                (decf (getf limit :tokens-reserved) reservation))
              (error condition))))))))

(-> mission--settle-inference
    (mission-context integer (option integer) &key (:allowance (option list)) (:failure t)) null)
(defun mission--settle-inference (context reservation tokens &key allowance failure)
  "Release RESERVATION once and retain reported usage or an uncertain outcome."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (with-recursive-lock-held ((mission-context-lock context))
      (let ((goal (mission-context-goal context)))
        (dolist (limit (remove nil (list goal allowance)))
          (decf (getf limit :requests-outstanding))
          (decf (getf limit :tokens-reserved) reservation)
          (when (integerp tokens)
            (incf (getf limit :tokens-used) tokens)))
        (unless (integerp tokens)
          (incf (getf goal :unknown-usage))
          (when (eq (getf goal :status) ':active)
            (if failure
                (mission--transition context ':failed failure)
                (mission--transition
                 context ':blocked
                 "Provider omitted billable usage; the remaining token budget is unknown."))))
        (when (and (eq (getf goal :status) ':active)
                   (> (getf goal :tokens-used) (getf goal :token-limit)))
          (mission--transition context ':exhausted "Mission token budget exhausted."))
        (mission--record context))))
  nil)

(-> mission--account-inference
    (mission-context function function &key (:allowance (option list))) t)
(defun mission--account-inference (context function usage-function &key allowance)
  "Reserve allowances, execute without mission locks, then settle exactly once.

ALLOWANCE optionally charges the same request to an independent reviewer policy.
Provider-reported billable usage includes input tokens; output reservations are
ceilings, not estimates of that usage. Preserve all provider return values."
  (let ((reservation nil)
        (settled-p nil)
        (failure nil))
    (unwind-protect
         (progn
           (sb-sys:without-interrupts
             (setf reservation (mission--reserve-inference context :allowance allowance)))
           (handler-case
               (let* ((*provider-maximum-output-tokens* reservation)
                      (results (mission--supervise
                                context "Mission inference"
                                (lambda () (multiple-value-list (funcall function)))))
                      (tokens (rlm-usage-billable-tokens
                               (provider-usage-normalize (funcall usage-function results)))))
                 (sb-sys:without-interrupts
                   (setf settled-p t)
                   (mission--settle-inference context reservation tokens :allowance allowance))
                 (when (and allowance
                            (or (null tokens)
                                (> (getf allowance :tokens-used) (getf allowance :token-limit))))
                   (mission--reject ':review-budget "Reviewer token allowance exhausted or usage unknown."))
                 (values-list results))
             (error (condition)
               (setf failure condition)
               (error condition))))
      (when (and reservation (not settled-p))
        (sb-sys:without-interrupts
          (setf settled-p t)
          (mission--settle-inference
           context reservation nil :allowance allowance
           :failure (or failure "Inference interrupted before usage settlement.")))))))
