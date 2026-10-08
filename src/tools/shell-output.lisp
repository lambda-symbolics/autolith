(in-package #:autolith)

;;;; -- Retained Shell Results --

(defclass shell-command-tool-result (tool-result)
  ((details
    :initarg :details
    :reader tool-result-details
    :type list
    :documentation "Portable execution and retained-log metadata, without log contents."))
  (:documentation "A bounded shell preview with independently readable retained output."))

(-> shell-command-result (string list boolean) shell-command-tool-result)
(defun shell-command-result (content details success-p)
  "Return a bounded shell outcome without spilling command output to context storage."
  (make-instance 'shell-command-tool-result
                 :content (bounded-string content :limit 8000)
                 :details details
                 :success-p success-p))

(-> shell-output--render-capture
    (cl-exec-sandbox:sandbox-capture string string &key (:budget (integer 0))) string)
(defun shell-output--render-capture (capture reference label &key (budget 6000))
  "Render CAPTURE's bounded head and tail, actual byte counts, and stable REFERENCE."
  (handler-case
      (multiple-value-bind (head tail omitted)
          (cl-exec-sandbox:sample-capture
           capture :head-bytes (floor budget 4) :tail-bytes (- budget (floor budget 4)))
        (with-output-to-string (stream)
          (format stream "~A: ~D bytes retained, ~D bytes observed; capture-complete ~A; status ~(~A~)~%Log: ~A~%"
                  label (cl-exec-sandbox:sandbox-capture-byte-count capture)
                  (cl-exec-sandbox:sandbox-capture-observed-byte-count capture)
                  (if (cl-exec-sandbox:sandbox-capture-complete-p capture) "yes" "no")
                  (cl-exec-sandbox:sandbox-capture-status capture) reference)
          (write-string (cl-exec-sandbox:decode-capture-bytes head) stream)
          (when (plusp omitted)
            (format stream "~%[~D retained bytes omitted from preview]~%" omitted))
          (write-string (cl-exec-sandbox:decode-capture-bytes tail) stream)))
    (file-error (condition)
      (format nil "~A preview unavailable: ~A~%Log: ~A~%"
              label (bounded-string (princ-to-string condition) :limit 300) reference))))

(-> shell-output--render-result
    (cl-exec-sandbox:sandbox-result shell-log-artifact (integer 0)
     &key (:diagnostic (option string))) string)
(defun shell-output--render-result (result artifact budget &key diagnostic)
  "Render RESULT with bounded samples, giving separate stderr most of BUDGET."
  (let* ((output (cl-exec-sandbox:sandbox-result-output-capture result))
         (error-output (cl-exec-sandbox:sandbox-result-error-capture result))
         (separate-p (and error-output t))
         (budget (min 6000 budget))
         (output-budget (if separate-p (floor budget 3) budget)))
    (with-output-to-string (stream)
      (if (sandbox-result-exit-code result)
          (format stream "exit ~D~%" (sandbox-result-exit-code result))
          (format stream "exit unavailable~%"))
      (format stream "Execution: ~(~A~); timeout ~A; cancelled ~A~%"
              (cl-exec-sandbox:sandbox-result-status result)
              (if (sandbox-result-timed-out-p result) "yes" "no")
              (if (cl-exec-sandbox:sandbox-result-cancelled-p result) "yes" "no"))
      (if output
          (write-string
           (shell-output--render-capture
            output (shell-log-reference artifact)
             (if separate-p "stdout" "combined output") :budget output-budget)
           stream)
          (format stream "Output capture unavailable. Log: ~A~%"
                  (shell-log-reference artifact)))
      (when error-output
        (terpri stream)
        (write-string
         (shell-output--render-capture
          error-output (shell-log-reference artifact ':error)
          "stderr" :budget (- budget output-budget))
         stream))
      (when diagnostic
        (format stream "~%Log metadata diagnostic: ~A~%" diagnostic)))))

(-> workspace-tool-run-shell-command
    (string pathname t (integer 1) (integer 0)
     &key (:environment list) (:context tool-context) (:merge-output-p boolean))
    tool-result)
(defun workspace-tool-run-shell-command
    (command directory policy timeout output-limit
     &key environment context (merge-output-p t))
  "Run one authorized command after allocating its private execution-owned capture."
  (sb-sys:without-interrupts
    (let* ((job *tool-execution-current-job*)
           (artifact (shell-log-allocate context :job job :merge-output-p merge-output-p)))
      (unwind-protect
           (sb-sys:with-local-interrupts
             (let* ((diagnostic nil)
                    (result
                      (handler-case
                          (destructuring-bind (program &rest arguments)
                              (platform-shell-command-line *platform* command)
                            (run-sandboxed
                             program arguments
                             :policy policy :working-directory directory :environment environment
                             :timeout timeout :merge-output-p merge-output-p
                             :output-limit 0 :error-output-limit 0
                             :capture-directory (shell-log-capture-directory artifact)
                             :retain-output-p t
                             :capture-byte-limit (shell-log-capture-byte-limit artifact)
                             :cancel-function (when job (lambda () (job-cancellation-requested-p job)))
                             :capture-created-function
                             (lambda (capture-result)
                               (shell-log-record-capture artifact capture-result :closed-p nil))
                             :capture-function
                             (lambda (capture-result)
                               (handler-case
                                   (shell-log-record-capture artifact capture-result)
                                 (error (condition)
                                   (setf diagnostic
                                         (bounded-string (princ-to-string condition) :limit 500)))))))
                        (cl-exec-sandbox:sandbox-execution-error (condition)
                          (setf diagnostic (bounded-string (princ-to-string condition) :limit 500))
                          (cl-exec-sandbox:sandbox-execution-error-result condition)))))
               (shell-command-result
                (shell-output--render-result result artifact output-limit :diagnostic diagnostic)
                (list :exit-code (sandbox-result-exit-code result)
                      :execution-status (cl-exec-sandbox:sandbox-result-status result)
                      :shell-logs (list (shell-log--public-metadata
                                         (shell-log-artifact-metadata artifact)))
                      :capture-diagnostic diagnostic)
                (and (null diagnostic)
                     (eq (cl-exec-sandbox:sandbox-result-status result) ':exited)))))
        (ignore-errors (shell-log-release-artifact artifact))))))
