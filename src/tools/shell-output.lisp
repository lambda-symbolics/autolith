(in-package #:autolith)

;;;; -- Retained Shell Results --

(defclass shell-command-tool-result (tool-result)
  ((details
    :initarg        :details
    :reader         tool-result-details
    :type           list
     :documentation "Portable execution metadata and bounded output samples."))
  (:documentation "A bounded shell preview with independently readable retained output."))

(-> shell-command-result
    (string list boolean &key (:code (option keyword)) (:denied-p boolean))
    shell-command-tool-result)
(defun shell-command-result (content details success-p &key code denied-p)
  "Return a bounded shell outcome without spilling command output to context storage."
  (make-instance 'shell-command-tool-result
                 :content    (bounded-string content :limit 8000)
                 :details    details
                 :error-code (or code
                                 (when (and (not success-p)
                                            (integerp (getf details :exit-code))
                                            (not (zerop (getf details :exit-code))))
                                   ':nonzero-exit))
                 :denied-p   denied-p
                 :success-p  success-p))

(-> shell-command-denial-result () tool-result)
(defun shell-command-denial-result ()
  "Retain the exact generic refusal so presentation can distinguish added diagnostics."
  (let ((message "The user denied this command."))
    (tool-failure message :denied-p t :details (list :shell-command-denial message))))

(-> shell-output--render-capture
    (cl-exec-sandbox:sandbox-capture string string &key (:budget (integer 0))) string)
(defun shell-output--render-capture (capture reference label &key (budget 6000))
  "Render CAPTURE's head and tail samples, retained and observed byte counts, and stable REFERENCE."
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
  "Render RESULT with bounded samples, using most of BUDGET for separately captured stderr."
  (let* ((output        (cl-exec-sandbox:sandbox-result-output-capture result))
         (error-output  (cl-exec-sandbox:sandbox-result-error-capture result))
         (separate-p    (and error-output t))
         (budget        (min 6000 budget))
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

(-> shell-output--capture-details
    ((option cl-exec-sandbox:sandbox-capture) keyword (integer 0)) list)
(defun shell-output--capture-details (capture stream budget)
  "Return STREAM's bounded decoded samples and explicit omission state."
  (if capture
      (handler-case
          (multiple-value-bind (head tail omitted)
              (cl-exec-sandbox:sample-capture
               capture :head-bytes (floor budget 4) :tail-bytes (- budget (floor budget 4)))
            (list :stream     stream
                  :head       (cl-exec-sandbox:decode-capture-bytes head)
                  :tail       (cl-exec-sandbox:decode-capture-bytes tail)
                  :omitted-p  (plusp omitted)
                  :complete-p (cl-exec-sandbox:sandbox-capture-complete-p capture)))
        (file-error ()
          (list :stream stream :unavailable-p t)))
      (list :stream stream :unavailable-p t)))

(-> shell-output--stream-details (cl-exec-sandbox:sandbox-result (integer 0)) list)
(defun shell-output--stream-details (result budget)
  "Return RESULT's stream previews using the model preview's byte allocation."
  (let* ((output        (cl-exec-sandbox:sandbox-result-output-capture result))
         (error-output  (cl-exec-sandbox:sandbox-result-error-capture result))
         (budget        (min 6000 budget))
         (stdout-budget (if error-output (floor budget 3) budget)))
    (append
     (list (shell-output--capture-details output (if error-output ':stdout ':combined)
                                         stdout-budget))
     (when error-output
       (list (shell-output--capture-details error-output ':stderr (- budget stdout-budget)))))))

(-> workspace-tool-run-shell-command
    (string pathname
     &key (:policy t) (:output-limit (integer 0))
           (:environment list) (:context tool-context) (:merge-output-p boolean)
           (:cancel-function (option function)) (:result-function (option function)))
    tool-result)
(defun workspace-tool-run-shell-command
    (command directory
     &key policy output-limit environment context (merge-output-p t)
       cancel-function result-function)
  "Run one authorized command after allocating its private execution-owned capture.

Keep publication ownership through preview construction and native cleanup."
  (shell-log-call-with-publication (tool-context-configuration context)
   (lambda ()
    (let ((job      *tool-execution-current-job*)
          (artifact nil))
      (unwind-protect
           (progn
             (shell-log-allocate context :job job :merge-output-p merge-output-p
                                         :receive (lambda (allocated) (setf artifact allocated)))
             (let* ((diagnostic nil)
                    (result
                      (handler-case
                          (destructuring-bind (program &rest arguments)
                              (platform-shell-command-line *platform* command)
                            (run-sandboxed
                             program arguments
                             :policy policy :working-directory directory :environment environment
                             :merge-output-p     merge-output-p
                             :output-limit 0 :error-output-limit 0
                             :capture-directory  (shell-log-capture-directory artifact)
                             :retain-output-p    t
                             :capture-byte-limit (shell-log-capture-byte-limit artifact)
                             :cancel-function
                             (when (or job cancel-function)
                               (lambda ()
                                 (or (and job (job-cancellation-requested-p job))
                                     (and cancel-function (funcall cancel-function)))))
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
                        (cl-exec-sandbox:sandbox-process-cleanup-error (condition)
                          (with-shell-log-lock ((shell-log-artifact-configuration artifact))
                            (setf (shell-log-artifact-native-owners artifact)
                                  (append (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-readers condition)
                                          (when (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-thread condition)
                                            (list (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-thread condition))))))
                          (unless job
                            (error condition))
                          (tool-execution-job-await-native-cleanup job condition)
                          (setf diagnostic (bounded-string (princ-to-string condition) :limit 500))
                          (or (cl-exec-sandbox:sandbox-execution-error-result condition)
                              (return-from workspace-tool-run-shell-command
                                (tool-failure diagnostic))))
                        (cl-exec-sandbox:sandbox-execution-error (condition)
                          (setf diagnostic (bounded-string (princ-to-string condition) :limit 500))
                          (or (cl-exec-sandbox:sandbox-execution-error-result condition)
                              (return-from workspace-tool-run-shell-command
                                (tool-failure diagnostic)))))))
              (when result-function (funcall result-function result))
              (shell-command-result
               (shell-output--render-result result artifact output-limit :diagnostic diagnostic)
               (list :exit-code          (sandbox-result-exit-code result)
                     :execution-status   (cl-exec-sandbox:sandbox-result-status result)
                      :timed-out-p       (sandbox-result-timed-out-p result)
                      :cancelled-p       (cl-exec-sandbox:sandbox-result-cancelled-p result)
                      :output-streams    (shell-output--stream-details result output-limit)
                     :shell-logs         (list (shell-log--public-metadata
                                                (shell-log-artifact-metadata artifact)))
                     :capture-diagnostic diagnostic)
               (and (null diagnostic)
                    (eql (sandbox-result-exit-code result) 0)
                    (eq (cl-exec-sandbox:sandbox-result-status result) ':exited)))))
        (when artifact
          ; mallet:suppress no-ignore-errors -- releasing the log must not mask the command result or failure
          (ignore-errors (shell-log-release-artifact artifact))))))))
