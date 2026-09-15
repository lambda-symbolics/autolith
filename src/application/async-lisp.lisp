(in-package #:autolith)

;;;; -- Asynchronous Active-image Lisp --

(-> application-async-lisp--present
    (application conversation (or string list)) null)
(defun application-async-lisp--present (application conversation entry)
  "Present ENTRY only while APPLICATION is displaying its originating CONVERSATION."
  (with-lock-held ((application-task-presentation-lock application))
    (when (eq conversation (application-conversation application))
      (application-present application entry)))
  nil)

(-> application-async-lisp--evaluate
    (application string application-async-lisp-output-stream) tool-result)
(defun application-async-lisp--evaluate (application source output)
  "Evaluate one SOURCE form with captured streams and noninteractive recovery."
  (let* ((input (make-string-input-stream ""))
         (terminal (make-two-way-stream input output))
         (*standard-input* input)
         (*standard-output* output)
         (*error-output* output)
         (*trace-output* output)
         (*terminal-io* terminal)
         (*query-io* terminal)
         (*debug-io* terminal)
         (*package* (find-package '#:autolith))
         (*application-operation-application* application)
         (*application-local-user-evaluation-p* t)
         (*application-command-interactive-p* nil)
         (*application-user-operation-recording-suppressed-p* t))
    (multiple-value-bind (values status condition restart-names selected-restart)
        (application-lisp-call-with-debugger
         (lambda ()
           (application-operation-install-bindings application)
           ;; Render while the capture bindings still cover user PRINT-OBJECT methods.
           (mapcar (lambda (value)
                     (multiple-value-bind (text truncated-p)
                         (management-repl--print-bounded value 4000)
                       (if truncated-p
                           (concatenate 'string text " [value truncated]")
                           text)))
                   (multiple-value-list (eval (self-read-form source)))))
         :source source :operation-kind ':lisp :retry-p nil)
      (declare (ignore restart-names selected-restart))
      (let* ((evaluation
               (application-lisp-evaluation-create
                :status status :values (first values) :condition condition))
             (text (terminal--spans-text
                    (application-lisp--result-entry evaluation))))
        (if (eq status ':ok)
            (tool-success text)
            (tool-failure text))))))

(-> application-async-lisp--finish
    (&key (:application application) (:conversation conversation)
          (:source string) (:submission-identifier string)
          (:output application-async-lisp-output-stream)
          (:job tool-execution-job) (:state keyword) (:result t)
          (:report (option string)))
    (values list (option string) keyword))
(defun application-async-lisp--finish
    (&key application conversation source submission-identifier output job state result report)
  "Retain JOB's terminal outcome even if presentation fails or its session changed."
  (multiple-value-bind (record final-report final-state)
      (tool-execution-job--terminal-record job state result report)
    (let* ((captured (application-async-lisp-output-text output))
           (outcome (getf record :content))
           (content
             (format nil "Async Lisp ~A (~(~A~))~%~@[output:~%~A~%~]~A"
                     (session-job-identifier job) final-state
                     (and (plusp (length captured)) captured) outcome)))
      (setf (getf record :content) content)
      (handler-case
          (conversation-append-async-lisp-event
           conversation source content :submission-identifier submission-identifier)
        (error (condition)
          (setf final-report
                (format nil "Async Lisp result could not be saved: ~A" condition)
                final-state ':failed
                (getf record :status) ':failed
                (getf record :content) (format nil "~A~%~A" content final-report))))
      (handler-case
          (progn
            (application-async-lisp-output-flush output)
            (application-async-lisp--present
             application conversation
             (list (terminal-span ':notice
                                  (format nil "? ~A~%" (session-job-identifier job)))
                   (terminal-span (if (eq final-state ':completed) ':success ':failure)
                                  (or final-report outcome)))))
        (error (condition)
          (setf final-report
                (format nil "~@[~A; ~]Async Lisp presentation failed: ~A"
                        final-report condition))))
      (values record final-report final-state))))

(-> application-run-async-lisp-input (application string) keyword)
(defun application-run-async-lisp-input (application source)
  "Submit ? SOURCE for nonblocking evaluation in the active image.

Persist the submission before starting it. Capture its conversation and identifier
so completion enters the originating session even after a session switch."
  (let* ((prefix (terminal-ui--async-lisp-prefix-length source))
         (conversation (application-conversation application))
         (orchestrator (application--task-orchestrator application))
         (form-source (and prefix (subseq source prefix))))
    (unless (and prefix orchestrator)
      (error 'configuration-error
             :message "Async Lisp requires a ? whitespace prefix and a session job runtime."))
    (let* ((submission-identifier
             (conversation-append-async-lisp-event conversation source nil))
           (output
             (application-async-lisp-output-stream-create
              :output-function
              (lambda (text)
                (application-async-lisp--present
                 application conversation
                 (list (terminal-span ':notice
                                      (format nil "? output ~A~%" submission-identifier))
                       (terminal-span ':plain text)))))))
      (application-async-lisp--present
       application conversation
       (append (list (terminal-span ':lisp-prompt "? "))
               (or (syntax--highlight-spans
                    form-source :language *application-common-lisp-language*)
                   (list (terminal-span ':code form-source)))))
      (let ((job
              (handler-case
                  (task-orchestrator-start-execution-job
                   orchestrator (application-agent application)
                   :tool-name "async-lisp" :summary source :detached-p t
                   :operation-function
                   (lambda ()
                     (application-async-lisp--evaluate application form-source output))
                   :terminal-result-function
                   (lambda (job state result report)
                     (application-async-lisp--finish
                      :application application :conversation conversation
                      :source source :submission-identifier submission-identifier
                      :output output :job job :state state :result result :report report)))
                (error (condition)
                  (let ((message (format nil "Async Lisp could not start: ~A" condition)))
                    (conversation-append-async-lisp-event
                     conversation source message :submission-identifier submission-identifier)
                    (application-async-lisp--present application conversation message)
                    (return-from application-run-async-lisp-input ':failed))))))
        (application-async-lisp--present
         application conversation
         (format nil "Async Lisp submitted: ~A" (session-job-identifier job))))
      ':continue)))
