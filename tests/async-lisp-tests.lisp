(in-package #:autolith)

;;;; -- Asynchronous Lisp Execution Tests --

(defvar *async-lisp-test-gate* nil
  "A semaphore used to hold an evaluation independently of the terminal reader.")

(defvar *async-lisp-test-started* nil
  "A semaphore signaled when an asynchronous test form starts.")

(defvar *async-lisp-test-value* 42
  "The active-image value queried without a parenthesized form.")

(-> async-lisp-tests--call-with-application (function) null)
(defun async-lisp-tests--call-with-application (function)
  "Call FUNCTION with a temporary application and its initialized job owner."
  (multiple-value-bind (application root) (lisp-machine-tests--application)
    (unwind-protect
         (progn
           (task-augment-tool-registry (application-tool-registry application))
           (setf (application-agent application)
                 (make-instance 'agent
                                :conversation (application-conversation application)
                                :configuration (application-configuration application)
                                :tool-registry (application-tool-registry application)))
           (terminal-ui-start (application-ui application))
           (funcall function application))
      (ignore-errors
        (tool-registry-close-runtime-state (application-tool-registry application)))
      (ignore-errors (terminal-ui-stop (application-ui application)))
      (platform-delete-directory-tree *platform* root
                                      :validate t :if-does-not-exist ':ignore)))
  nil)

(-> async-lisp-tests--last-job (application) tool-execution-job)
(defun async-lisp-tests--last-job (application)
  "Return APPLICATION's most recently admitted execution."
  (first (last (task-orchestrator-list-jobs
                (application--task-orchestrator application)))))

(-> async-lisp-tests--wait (tool-execution-job) list)
(defun async-lisp-tests--wait (job)
  "Wait a bounded interval for JOB and return its retained result."
  (let ((deadline (+ (get-internal-real-time)
                     (* 10 internal-time-units-per-second))))
    (loop until (job-terminal-p job)
          do (when (> (get-internal-real-time) deadline)
               (error "Async Lisp job did not finish."))
             (sleep 0.01)))
  (job-result job))

(-> test-application-async-lisp-evaluation () null)
(defun test-application-async-lisp-evaluation ()
  "Test bare symbols, values, conditions, and every standard output binding."
  (async-lisp-tests--call-with-application
   (lambda (application)
     (dolist (case '(("? *async-lisp-test-value*" "42" :completed)
                     ("? (values 1 2)" "⇒ 2" :completed)
                     ("? (values)" "no values" :completed)
                     ("? (error \"async failure\")" "async failure" :failed)
                     ("? (read-line)" "end of file" :failed)
                     ("? (list 1) (list 2)" "form" :failed)))
       (destructuring-bind (source expected state) case
         (test-assert (eq (application-run-async-lisp-input application source)
                          ':continue)
                      "submission returns without taking terminal ownership")
         (let* ((job (async-lisp-tests--last-job application))
                (record (async-lisp-tests--wait job)))
           (test-assert (and (eq (job-state job) state)
                             (search expected (getf record :content)
                                     :test #'char-equal))
                        (format nil "~A retains its expected outcome: ~S" source record)))))
     (application-run-async-lisp-input
      application
      "? (progn (write-string \"stdout\" *standard-output*) (write-string \"stderr\" *error-output*) (write-string \"trace\" *trace-output*) (write-string \"query\" *query-io*) (write-string \"terminal\" *terminal-io*) (write-string \"debug\" *debug-io*) :done)")
     (let ((record (async-lisp-tests--wait (async-lisp-tests--last-job application))))
       (test-assert
        (every (lambda (text) (search text (getf record :content)))
               '("stdout" "stderr" "trace" "query" "terminal" "debug" ":DONE"))
        "all standard output streams are captured in the completion"))))
  nil)

(-> test-application-async-lisp-concurrency () null)
(defun test-application-async-lisp-concurrency ()
  "Test responsive submission, independent evaluations, cancellation, and ownership."
  (async-lisp-tests--call-with-application
   (lambda (application)
     (setf *async-lisp-test-gate* (sb-thread:make-semaphore)
           *async-lisp-test-started* (sb-thread:make-semaphore))
     (let ((origin (application-conversation application)))
       (unwind-protect
            (progn
              (application-run-async-lisp-input
               application
               "? (progn (write-line \"before blocking\") (sb-thread:signal-semaphore *async-lisp-test-started*) (sb-thread:wait-on-semaphore *async-lisp-test-gate*) :unblocked)")
              (let ((blocked (async-lisp-tests--last-job application)))
                (test-assert
                 (sb-thread:wait-on-semaphore *async-lisp-test-started* :timeout 5)
                 "the submitted form starts in the background")
                (test-assert (not (job-terminal-p blocked))
                             "the caller regained control before evaluation finished")
                (application-run-async-lisp-input application "? *async-lisp-test-value*")
                (test-assert
                 (search "42" (getf (async-lisp-tests--wait
                                     (async-lisp-tests--last-job application)) :content))
                 "a blocked form does not prevent another asynchronous query")
                (setf (application-conversation application)
                      (conversation-create (application-configuration application)
                                           :identifier (make-identifier)))
                (sb-thread:signal-semaphore *async-lisp-test-gate*)
                (test-assert
                 (search ":UNBLOCKED" (getf (async-lisp-tests--wait blocked) :content))
                 "completion retains the result after a session switch")
                (test-assert
                 (zerop (length (conversation-input-items
                                 (application-conversation application))))
                 "completion does not enter the replacement conversation")))
         (sb-thread:signal-semaphore *async-lisp-test-gate*)
         (setf (application-conversation application) origin)))))
  nil)

(-> test-application-async-lisp-cancellation () null)
(defun test-application-async-lisp-cancellation ()
  "Test terminal capture and session delivery after job cancellation."
  (async-lisp-tests--call-with-application
   (lambda (application)
     (setf *async-lisp-test-started* (sb-thread:make-semaphore))
     (application-run-async-lisp-input
      application
      "? (progn (write-line \"cancelled output\") (sb-thread:signal-semaphore *async-lisp-test-started*) (sleep 60))")
     (let ((job (async-lisp-tests--last-job application)))
       (test-assert
        (sb-thread:wait-on-semaphore *async-lisp-test-started* :timeout 5)
        "the cancellable job entered its form")
       (session-job-cancel job ':user)
       (let ((record (async-lisp-tests--wait job)))
         (test-assert (and (eq (job-state job) ':aborted)
                           (search "cancelled output" (getf record :content)))
                      "cancellation preserves captured output and a terminal result")))))
  nil)

(-> test-application-async-lisp-routing () null)
(defun test-application-async-lisp-routing ()
  "Test async classification, multiline input, busy routing, and recalled work consumption."
  (dolist (source '("? *package*" "? (values 1 2)"))
    (test-assert (null (application--message-input source))
                 "async Lisp bypasses ordinary model prompt routing")
    (test-assert
     (equal (application-input-controller--restore-work-item
             (application-input-controller--pending-work-entry-form
              (application-input-controller--input-work source)))
            (list ':lisp source))
     "async input retains its exact prefix through pending-work serialization"))
  (dolist (source '("??" "?text" "?" " ? *package*"))
    (test-assert (equal source (application--message-input source))
                 "only an initial question mark followed by whitespace selects async Lisp"))
  (async-lisp-tests--call-with-application
   (lambda (application)
     (let ((controller (lisp-machine-tests--controller application))
           (submitted nil))
       (test-assert
        (application-input-controller--defer-lisp-submission-p controller "? (list 1")
        "an incomplete async form continues editing")
       (test-assert
        (string= (line-editor-text (terminal-ui-editor (application-ui application)))
                 (format nil "? (list 1~%"))
        "continuation preserves the async prefix")
       (test-assert
        (not (application-input-controller--defer-lisp-submission-p controller "? *package*"))
        "a bare symbol is complete async input")
       (test-call-with-function-replacements
        (list (list 'application-run-async-lisp-input
                    (lambda (observed source)
                      (test-assert (eq observed application) "routing keeps its application")
                      (push source submitted)
                      ':continue)))
        (lambda ()
          (dolist (active-p '(nil t))
            (setf (application-input-controller-active-p controller) active-p)
            (application-input-controller--handle-submission controller "? *package*")
            (application-input-controller--handle-queue-submission controller "? *package*")
            (setf (application-input-controller-follow-up-edit-index controller) 0
                  (application-input-controller-follow-up-edit-work controller)
                  '(:message "recalled draft"))
            (test-assert
             (application-input-controller--handle-recalled-submission
              controller "? *package*")
             "a recalled draft can be changed into an immediate async form")
            (test-assert
             (and (null (application-input-controller-follow-up-edit-index controller))
                  (null (application-input-controller-follow-up-edit-work controller))
                  (deque-empty-p (application-input-controller-work-items controller))
                  (deque-empty-p (application-input-controller-steering-items controller)))
             "the recalled slot is consumed once without queuing or steering the form"))
          (test-assert (= 6 (length submitted)) "every submission starts exactly once"))))))
  nil)
