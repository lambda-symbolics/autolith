(in-package #:autolith)

;;;; -- Periodic Self-Modification Review --

(defparameter *self-review-enabled-p* t
  "Whether the periodic self-modification review reminder is active.")

(defparameter *self-review-minimum-user-turns* 10
  "The user turns required before the first review reminder.")

(defparameter *self-review-period-user-turns* 12
  "The user turns required between review reminders.")

(defparameter *self-review-working-seconds-gate* 600
  "The accumulated working seconds required between review reminders.")

(defparameter *self-review-instruction*
    "Session review checkpoint. Scan the recent conversation for Autolith-side friction: a repeated workaround, a recurring failure, missing observability into your own state, or a stable user preference needing executable behavior. If a small self-modification within your existing authority would materially help, make or propose it, using the least durable mechanism that fits. When recent inference traces show one decomposition pattern succeeding repeatedly, consider rlm.distill to propose a reusable policy. If nothing qualifies, continue silently; this reminder is never a mutation quota."
  "The periodic advice asking the model to consider self-modification.")

(defvar *self-review-receipts* (make-hash-table :test #'equal)
  "Process-lifetime reminder receipts keyed by conversation identifier.

Each receipt records the user-turn count and working seconds at the last
reminder, so restarts merely allow one early reminder.")

(-> self-review--contribution () context-contribution)
(defun self-review--contribution ()
  "Return one freshly built review reminder contribution."
  (make-context-contribution
   :identifier "self-improvement-review"
   :instruction *self-review-instruction*
   :priority 10
   :lifetime ':turn
   :class ':advice
   :deduplication-key "self-improvement-review"))

(-> self-review-context (request-context) (option context-contribution))
(defun self-review-context (request)
  "Remind the model periodically to consider small self-modifications.

The reminder fires on a logical turn only when both the user-turn period
and the accumulated working-time gate have passed since the last
reminder, and it repeats within that trigger turn so the advice survives
the turn's whole request loop."
  (let ((configuration (request-context-configuration request))
        (conversation (request-context-conversation request)))
    (when (and *self-review-enabled-p*
               (not (request-context-compaction-p request))
               (not (configuration-immutable-p configuration)))
      (let* ((identifier (conversation-identifier conversation))
             (turns (conversation-user-turn-count conversation))
             (worked (conversation-working-seconds conversation))
             (receipt (gethash identifier *self-review-receipts*))
             (fired-turn (and receipt (getf receipt :user-turns)))
             (turn-floor
               (if receipt
                   (+ fired-turn *self-review-period-user-turns*)
                   *self-review-minimum-user-turns*))
             (worked-floor
               (+ (if receipt
                      (getf receipt :working-seconds)
                      0)
                  *self-review-working-seconds-gate*)))
        (cond
          ((and fired-turn (= turns fired-turn))
           (self-review--contribution))
          ((and (>= turns turn-floor)
                (>= worked worked-floor))
           (setf (gethash identifier *self-review-receipts*)
                 (list :user-turns turns :working-seconds worked))
           (self-review--contribution)))))))

(register-context-contributor "self-improvement-review"
                              'self-review-context
                              :source ':built-in)
