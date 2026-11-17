(in-package #:autolith)

;;;; -- Deferred Conversation Maintenance --

(defparameter *conversation-maintenance-runtime-milliseconds* 30000
  "The wall-clock allowance for one channel's deferred archive repair.")

(define-condition conversation-maintenance-stop-error (autolith-error)
  ((snapshot
    :initarg       :snapshot
    :reader        conversation-maintenance-stop-error-snapshot
    :documentation "The managed job snapshot when shutdown could not finish."))
  (:documentation "Maintenance still owns conversation access, so its lease cannot be released."))

(-> application-conversation-maintenance-start
    (application &key (:run-function function) (:complete-function function)
     (:resume-p boolean))
    (option job))(defun application-conversation-maintenance-start
                  (application
                   &key
                   (run-function
                    (lambda (conversation &key check-function)
                      (funcall 'conversation-maintenance-run conversation :check-function
                               check-function)))
                   (complete-function
                    (lambda (application conversation result)
                      (funcall 'application-conversation-maintenance-complete application
                               conversation result)))
                   resume-p)
                  "Queue one managed archive-repair job after input readiness and return it.
Repeated calls return the same job. RUN-FUNCTION implements cancellable repair;
starting performs no archive traversal and does not wait for the job."
                  (with-lock-held ((application-conversation-maintenance-lock application))
                    (when
                        (and resume-p
                             (application-conversation-maintenance-stopping-p application)
                             (null (application-conversation-maintenance-pool application))
                             (application-conversation-lease application)
                             (conversation-lease-held-p
                              (application-conversation-lease application)))
                      (setf (application-conversation-maintenance-stopping-p application) nil
                            (application-conversation-maintenance-job application) nil))
                    (unless (application-conversation-maintenance-stopping-p application)
                      (or (application-conversation-maintenance-job application)
                          (let* ((conversation (application-conversation application))
                                 (pool
                                  (or (application-conversation-maintenance-pool application)
                                      (setf (application-conversation-maintenance-pool
                                             application)
                                            (make-job-pool :name
                                                           "Autolith conversation maintenance"
                                                           :maximum-concurrency      1
                                                           :maximum-live-jobs        1
                                                           :maximum-batch-size       1
                                                           :terminal-retention-limit 1
                                                           :maximum-runtime-milliseconds
                                                           *conversation-maintenance-runtime-milliseconds*)))))
                            (setf (application-conversation-maintenance-job application)
                                  (job-pool-submit pool
                                                   (lambda (job)
                                                     (job-check-cancellation job)
                                                     (let ((result
                                                            (funcall run-function
                                                                     conversation
                                                                     :check-function
                                                                     (lambda ()
                                                                       (job-check-cancellation
                                                                        job)))))
                                                       (job-check-cancellation job)
                                                       (when (getf result :input-complete-p)
                                                         (funcall complete-function
                                                                  application conversation
                                                                  result))
                                                       result))
                                                   :name
                                                   "Deferred conversation archive repair")))))))(-> application-conversation-maintenance-status (application) list)
(defun application-conversation-maintenance-status (application)
  "Return the retained managed job snapshot, or an idle maintenance state."
  (with-lock-held ((application-conversation-maintenance-lock application))
    (let ((job (application-conversation-maintenance-job application)))
      (if job
          (job-snapshot job)
          (list :state (if (application-conversation-maintenance-stopping-p application)
                           ':closed ':idle))))))

(-> application-conversation-maintenance-stop (application) null)
(defun application-conversation-maintenance-stop (application)
  "Cancel and join managed maintenance before releasing conversation ownership.
Retain its pool and job after incomplete shutdown so another close can retry."
  (let ((pool
          (with-lock-held ((application-conversation-maintenance-lock application))
            (setf (application-conversation-maintenance-stopping-p application) t)
            (application-conversation-maintenance-pool application))))
    (when pool
      (unless (job-pool-close pool)
        (error 'conversation-maintenance-stop-error
               :message  "Conversation maintenance has not stopped; its lease is retained."
               :snapshot (application-conversation-maintenance-status application)))
      (with-lock-held ((application-conversation-maintenance-lock application))
        (when (eq pool (application-conversation-maintenance-pool application))
          (setf (application-conversation-maintenance-pool application) nil)))))
  nil)
