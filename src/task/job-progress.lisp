(in-package #:autolith)

;;;; -- Headless Job Progress --

(-> run-job-progress-observer (run-job-request) function)
(defun run-job-progress-observer (request)
  "Persist aggregate job progress and enforce its request ceiling before dispatch."
  (let ((requests 0)
        (usage nil)
        (lock (make-lock "Headless job progress")))
    (lambda (job status details)
      (with-lock-held (lock)
        (when (eq status ':provider-request-started)
          (let ((maximum (run-job-request-maximum-requests request)))
            (when (and maximum (>= requests maximum))
              (run-job--error ':request-budget
                              "The job exhausted its ~D provider requests." maximum)))
          (incf requests))
        (when (eq status ':provider-request-completed)
          (setf usage (task-progress--merge-usage usage (getf details :usage))))
        (let ((path (run-job-request-progress-path request)))
          (when path
            (run-job-write-result-atomically
             path
             (list :autolith-job-progress :version 1
                   :id (run-job-request-identifier request)
                   :trace-id (task-job-execution-identifier job)
                   :provider-requests requests
                   :usage usage
                   :status status
                   :updated-at (run-job--timestamp (get-universal-time))))))))))
