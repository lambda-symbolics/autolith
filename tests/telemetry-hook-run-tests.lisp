(in-package #:autolith)

;;;; -- Run Lifecycle and Consent Ownership --

(-> test-telemetry-terminal-outcomes () null)
(defun test-telemetry-terminal-outcomes ()
  "Classify every job state and boundary override with one outcome function."
  (dolist (case '((nil :error :internal "unfinished-run")
                  ((:state :running) :error :internal "unfinished-run")
                  ((:state :completed) :success nil nil)
                  ((:state :succeeded) :success nil nil)
                  ((:state :failed :yielded-p t :request-count 1) :error :internal nil)
                  ((:state :failed :request-count 1) :error :provider nil)
                  ((:state :failed :request-count 0) :error :internal nil)
                  ((:state :aborted) :cancelled :cancelled nil)
                  ((:state :cancelled) :cancelled :cancelled nil)
                  ((:state :failed :error-type :cancelled) :cancelled :cancelled nil)
                  ((:state :timed-out) :error :timeout nil)
                  ((:state :aborted :cancellation-reason :timeout) :error :timeout nil)
                  ((:state :failed :category :provider-failure) :error :provider nil)
                  ((:state :failed :category :invalid-output) :error :protocol nil)
                  ((:state :succeeded :published-p nil) :error :internal nil)
                  ((:state :succeeded :category :result-publication-failed) :error :internal nil)
                  ((:state :failed :category :artifact-failure) :error :internal nil)
                  ((:state :failed :error-type :authentication) :error :authentication nil)))
    (test-assert (equal (multiple-value-list (telemetry--terminal-outcome (first case))) (rest case))
                 (format nil "Terminal classification: ~S" (first case))))
  nil)

(-> test-telemetry-run-owner-token () null)
(defun test-telemetry-run-owner-token ()
  "A borrowed run cannot finish; late reports and repeated finishes are harmless."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((diagnostics    (make-string-output-stream))
            (*error-output* diagnostics)
            (bodies
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (multiple-value-bind (run token) (telemetry--begin-run configuration)
                   (telemetry-report-terminal run '(:state :completed))
                   (telemetry-finish-run run (list nil) '(:state :failed))
                   (test-assert (telemetry-run-active run) "A non-owner cannot close a borrowed root.")
                   (telemetry-finish-run run token)
                   (telemetry-report-terminal run '(:state :failed))
                   (telemetry-finish-run run token '(:state :failed))
                   (test-assert (not (telemetry-run-active run)) "Owner cleanup disposes the run."))))))
       (test-assert (= (length bodies) 1) "The owner flushes exactly one root.")
       (test-assert (equal "success" (test-telemetry--attribute
                                       (first (test-telemetry-hooks--kind (first bodies) "run"))
                                       "autolith.run.outcome"))
                    "Late reports do not rewrite the result.")
       (test-assert (zerop (length (get-output-stream-string diagnostics)))
                    "Ignored late reports do not publish unsolicited console output."))))
  nil)

(-> test-telemetry-hooks--contended-report
    (terminal-ui stream function &key (:main-thread-p boolean)) null)
(defun test-telemetry-hooks--contended-report (ui output report &key main-thread-p)
  "Call REPORT while a different thread owns UI; all barriers have deadlines."
  (let ((ready   (bt:make-semaphore))
        (release (bt:make-semaphore))
        (done    (bt:make-semaphore))
        (thread  nil)
        (failure nil))
    (flet ((wait-for (semaphore)
             (unless (bt:wait-on-semaphore semaphore :timeout 4)
               (error "Timed out at telemetry presentation barrier."))))
      (unwind-protect
           (flet ((start-worker ()
                    (setf thread
                          (make-thread
                           (lambda ()
                             (let ((*error-output*    output)
                                   (*standard-output* output)
                                   (*trace-output*    output))
                               (unwind-protect
                                    (handler-case
                                        (if main-thread-p
                                            (with-terminal-ui-locked (ui)
                                              (bt:signal-semaphore ready)
                                              (wait-for release))
                                            (funcall report))
                                      (error (condition) (setf failure condition)))
                                 (bt:signal-semaphore done))))
                           :name "Telemetry presentation peer"))))
             (if main-thread-p
                 (progn
                   (start-worker)
                   (wait-for ready)
                   (let ((*error-output* output)) (funcall report)))
                 (with-terminal-ui-locked (ui)
                   (start-worker)
                   (wait-for done)
                   (bt:signal-semaphore done))))
        (bt:signal-semaphore release)
        (when thread
          (wait-for done)
          (join-thread thread)))
      (when failure (error failure))))
  nil)

(-> test-telemetry-late-report-presentation () null)
(defun test-telemetry-late-report-presentation ()
  "Ignored reports cannot bypass an active prompt, including a contended UI lock."
  (dolist (fullscreen-p '(nil t))
    (dolist (main-thread-p '(nil t))
      (test-telemetry--call
       (lambda (configuration)
         (let* ((output   (make-string-output-stream))
                (terminal (make-instance 'shared-output-terminal :output output :columns 80 :rows 24))
                (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p)))
           (unwind-protect
                (let ((bodies
                        (test-telemetry-hooks--capture
                         configuration
                         (lambda ()
                           (terminal-ui-start ui)
                           (terminal-ui-set-input ui "editable draft")
                           (terminal-ui-open-prompt-block ui)
                           (test-assert (if fullscreen-p
                                            (fullscreen-terminal-ui-active-p ui)
                                            (eq (terminal-ui-prompt-marker-state ui) ':input))
                                        "The actual prompt is active before reporting.")
                           (test-assert (plusp (length (get-output-stream-string output)))
                                        "Actual renderer writes reach the shared sink.")
                           (multiple-value-bind (run token) (telemetry--begin-run configuration)
                             (let ((*error-output* output))
                               (telemetry-report-terminal run '(:state :failed :error-type :provider)))
                             (test-assert (equal '(:error :provider nil) (telemetry-run-borrower-outcome run))
                                          "Live borrower reports update fallback.")
                             (telemetry-finish-run run (list nil) '(:state :failed))
                             (test-assert (telemetry-run-active run) "A borrower cannot finish the owner.")
                             (telemetry-finish-run run token '(:state :completed))
                             (test-telemetry-hooks--contended-report
                              ui output
                              (lambda ()
                                (let ((*telemetry-run* run))
                                  (telemetry-call-with-run
                                   configuration
                                   (lambda ()
                                     (test-assert (eq *telemetry-run* run) "Late work borrows the same run.")
                                     (telemetry-report-terminal *telemetry-run* '(:state :timed-out))))))
                              :main-thread-p main-thread-p)
                             (test-assert (zerop (length (get-output-stream-string output)))
                                          "No direct writes bypass the active presentation owner.")
                             (test-assert (equal '(:error :provider nil) (telemetry-run-borrower-outcome run))
                                          "A stronger late failure is ignored.")
                             (telemetry-finish-run run token '(:state :failed))
                             (multiple-value-bind (action payload) (terminal-ui-process-event ui :submit)
                               (test-assert (and (eq action ':submit) (equal payload "editable draft"))
                                            "The editable draft submits through the real input boundary.")))))))
                  (test-assert (= 1 (length bodies)) "The owner publishes one root despite repeated finish.")
                  (test-assert (equal "success" (test-telemetry--attribute
                                                 (first (test-telemetry-hooks--kind (first bodies) "run"))
                                                 "autolith.run.outcome"))
                               "Ignored late failures cannot rewrite owner success."))
             (terminal-ui-stop ui)))))))
  nil)

(-> test-telemetry-run-unwind () null)
(defun test-telemetry-run-unwind ()
  "Non-local exits and errors close and flush their owner roots before propagating."
  (dolist (exit '(:throw :error))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((bodies
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (catch 'leave
                     (handler-case
                         (telemetry-call-with-run
                          configuration
                          (lambda ()
                            (telemetry-report-terminal *telemetry-run* '(:state :completed))
                            (if (eq exit ':throw) (throw 'leave t) (error "Injected failure."))))
                       (error () nil))))))
              (root (first (test-telemetry-hooks--kind (first bodies) "run"))))
         (test-assert (= (length bodies) 1) "An unwinding owner flushes once.")
         (test-assert (equal "error" (test-telemetry--attribute root "autolith.run.outcome")) "Unwinding is an error.")
         (test-assert (equal "internal" (test-telemetry--attribute root "error.type")) "Unclassified exits are internal.")
         (test-assert (equal (when (eq exit ':throw) "unfinished-run")
                             (test-telemetry--attribute root "autolith.run.error_code"))
                      "An unreported non-local exit has a dedicated code.")
         (test-assert (null (telemetry-controller-runs *telemetry-controller*)) "No root remains open.")))))
  nil)

(-> test-telemetry-borrower-outcomes () null)
(defun test-telemetry-borrower-outcomes ()
  "Borrower ordering is stable and a successful owner can handle child failures."
  (dolist (reports '(((:state :failed :error-type :provider) (:state :cancelled) (:state :completed))
                     ((:state :completed) (:state :cancelled) (:state :failed :error-type :provider))
                     ((:state :failed :error-type :internal) (:state :failed :error-type :provider))
                     ((:state :failed :error-type :provider) (:state :failed :error-type :internal))))
    (dolist (owner-result-p '(nil t))
      (test-telemetry--call
       (lambda (configuration)
         (let* ((bodies
                  (test-telemetry-hooks--capture
                   configuration
                   (lambda ()
                     (flet ((report ()
                              (dolist (terminal reports)
                                (telemetry-report-terminal *telemetry-run* terminal))))
                       (if owner-result-p
                           (telemetry-call-with-run configuration #'report)
                           (multiple-value-bind (*telemetry-run* token) (telemetry--begin-run configuration)
                             (report)
                             (telemetry-finish-run *telemetry-run* token)))))))
                (root (first (test-telemetry-hooks--kind (first bodies) "run"))))
           (test-assert (equal (test-telemetry--attribute root "autolith.run.outcome")
                               (if owner-result-p "success" "error"))
                        "Owner success overrides children; otherwise the worst borrower wins.")
           (test-assert (equal (test-telemetry--attribute root "error.type")
                               (unless owner-result-p "provider"))
                        "Equal outcomes select the same error type in either order."))))))
  nil)

(-> test-telemetry-finish-freezes-counters () null)
(defun test-telemetry-finish-freezes-counters ()
  "Completions during root upload cannot exceed the snapshotted counters."
  (test-telemetry--call
   (lambda (configuration)
     (let ((flush (symbol-function 'telemetry-flush)))
       (test-call-with-function-replacements
        (list (list 'telemetry-flush
                    (lambda (&optional (run *telemetry-run*))
                      (telemetry-note-model :run run :provider "local")
                      (telemetry-note-tool :run run :tool "shell.run" :outcome ':success)
                      (funcall flush run))))
        (lambda ()
          (let* ((bodies (test-telemetry-hooks--capture
                          configuration (lambda () (telemetry-call-with-run configuration (lambda () t)))))
                 (root   (first (test-telemetry-hooks--kind (first bodies) "run"))))
            (test-assert (and (null (test-telemetry-hooks--kind (first bodies) "model"))
                              (null (test-telemetry-hooks--kind (first bodies) "tool"))
                              (equal (test-telemetry--attribute root "autolith.run.model_calls") "0")
                              (equal (test-telemetry--attribute root "autolith.run.tool_calls") "0"))
                         "Finalization freezes child admission and counters together.")))))))
  nil)

(-> test-telemetry-detached-run-lifetime () null)
(defun test-telemetry-detached-run-lifetime ()
  "A real pool child finishes and exports after the parent's run has closed."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((orchestrator  (task-orchestrator-create))
            (parent        (task-tests--primary-agent configuration "telemetry-parent"))
            (definition    (task-agent-definition-create
                            :name "telemetry-detached" :description "Exercise detached telemetry."
                            :instructions "Complete after the parent." :source ':test))
            (started       (bt:make-semaphore))
            (released      (bt:make-semaphore))
            (lock          (bt:make-lock "detached telemetry capture"))
            (bodies        nil)
            (job           nil)
            (parent-run    nil)
            (child-run     nil)
            (worker-thread nil))
       (setf (config :telemetry-enabled-p configuration) t)
       (telemetry-attach configuration)
       (unwind-protect
            (test-call-with-function-replacements
             (list (list 'telemetry--process
                         (lambda (run request)
                           (declare (ignore run))
                           (when (eq (first request) ':upload)
                             (bt:with-lock-held (lock) (push (third request) bodies)))
                           t))
                   (list 'task-run-child
                         (lambda (candidate)
                           (setf child-run (task-job-telemetry-run candidate)
                                 worker-thread (bt:current-thread))
                           (bt:signal-semaphore started)
                           (bt:wait-on-semaphore released)
                           (telemetry-note-model :run child-run :provider "local")
                           (list :status ':success :yielded-p t :request-count 1))))
             (lambda ()
               (telemetry-call-with-run
                configuration
                (lambda ()
                  (setf parent-run *telemetry-run*
                        job (first (task-orchestrator-start-jobs
                                    orchestrator parent
                                    (list (list :definition definition :detached t
                                                :telemetry-run parent-run
                                                :item          (list :name "detached" :task "Wait for release."))))))
                  (test-assert (bt:wait-on-semaphore started :timeout 10) "The child starts on the pool.")))
               (test-assert (not (telemetry-run-active parent-run)) "The parent has finished first.")
               (test-assert (and child-run (not (eq worker-thread (bt:current-thread)))
                                 (telemetry-run-active child-run)
                                 (not (equal (telemetry-run-trace-id parent-run) (telemetry-run-trace-id child-run)))
                                 (= (telemetry-run-attempt child-run) 1))
                            "A detached pool child owns a fresh live trace, ignoring even an explicit borrowed run.")
               (bt:signal-semaphore released)
               (multiple-value-bind (snapshot terminal-p) (task-job-await job 10)
                 (test-assert (and terminal-p (eq (getf snapshot :state) ':completed)) "The child completes."))
               (let* ((spans (loop for body in bodies append (test-telemetry-hooks--kind body "run")))
                      (child (find (telemetry-run-trace-id child-run) spans
                                   :key (lambda (span) (json-get span "traceId")) :test #'equal)))
                 (test-assert (= (length spans) 2) "Both owners export one terminal root.")
                 (test-assert (equal (test-telemetry--attribute child "autolith.run.model_calls") "1")
                              "Post-parent child telemetry is retained in its own root."))))
         (bt:signal-semaphore released)
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-telemetry-scheduler-run-ownership () null)
(defun test-telemetry-scheduler-run-ownership ()
  "Scheduler owners finish through execution and publication unwinds; borrowers report only."
  (dolist (borrow-p '(nil t))
    (dolist (exit '(:normal :throw :error :publication-throw))
      (test-telemetry--call
       (lambda (configuration)
         (let* ((orchestrator (task-orchestrator-create))
                (parent       (agent-create :configuration configuration :provider (make-instance 'model-provider)
                                            :conversation (conversation-create configuration)
                                            :tool-registry (make-instance 'tool-registry) :worker nil))
                (job          (task-tests--make-job orchestrator :identifier "ownership" :parent-agent parent
                                                    :definition (task-agent-definition-create
                                                                 :name "ownership" :description "Exercise ownership."
                                                                 :instructions "Complete." :source ':test)
                                                    :item       (list :task "Exercise ownership.")))
                (bodies       nil))
           (unwind-protect
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (multiple-value-bind (borrowed token)
                       (when borrow-p (telemetry--begin-run configuration))
                     (setf (task-job-telemetry-run job) borrowed)
                     (let ((*telemetry-process-function*
                             (lambda (run request)
                               (declare (ignore run))
                               (push (third request) bodies)
                               t)))
                       (test-call-with-function-replacements
                        (append
                         (list (list 'task-continuity-record-job (lambda (&rest arguments) (declare (ignore arguments)) nil))
                               (list 'task-run-child
                                     (lambda (job)
                                       (telemetry-report-terminal (task-job-telemetry-run job) '(:state :completed))
                                       (case exit
                                         (:throw
                                          (throw 'leave t))
                                         (:error
                                          (error "Injected child failure."))
                                         (otherwise
                                          (list :status ':success :yielded-p t :request-count 0))))))
                         (when (eq exit ':publication-throw)
                           (list (list 'task-job--publish-terminal-record
                                       (lambda (&rest arguments) (declare (ignore arguments)) (throw 'leave t))))))
                        (lambda ()
                          (catch 'leave
                            (handler-case
                                (let ((result (task-job--run job)))
                                  (task-job--terminal-record job :state  ':completed
                                                                 :result result))
                              (error (condition)
                                (task-job--terminal-record job :state  ':failed
                                                               :report (princ-to-string condition)))))))
                       (when borrow-p
                         (test-assert (and (telemetry-run-active borrowed) (null bodies))
                                      "The scheduler never exports a borrowed root on any exit.")
                         (telemetry-finish-run borrowed token))
                       (test-assert (= (length bodies) 1) "Only the creator exports the root.")
                       (test-assert (null (telemetry-controller-runs *telemetry-controller*)) "Every owner scope closes its run.")
                       (when (and (not borrow-p) (member exit '(:throw :publication-throw)))
                         (let ((root (first (test-telemetry-hooks--kind (first bodies) "run"))))
                           (test-assert (equal "unfinished-run" (test-telemetry--attribute root "autolith.run.error_code"))
                                        "Unreported execution and publication exits have a dedicated code.")))))))
             (task-orchestrator-close orchestrator)))))))
  nil)

(-> test-telemetry-headless-run-unwind () null)
(defun test-telemetry-headless-run-unwind ()
  "A non-local headless executor exit still flushes its root before leaving."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((input  (merge-pathnames "unwind-input.sexp" (config :cache-root configuration)))
            (output (merge-pathnames "unwind-output.sexp" (config :cache-root configuration)))
            (bodies nil))
       (task-tests--write-text input (run-job--write-data-sexp (run-job-tests--request-form)))
       (test-telemetry-hooks--capture
        configuration
        (lambda ()
          (let ((*telemetry-process-function*
                  (lambda (run request) (declare (ignore run)) (push (third request) bodies) t)))
            (catch 'leave
              (run-job--run-artifact
               input output ':full-access :configuration configuration
               :executor                                 (lambda (&rest arguments) (declare (ignore arguments)) (throw 'leave t)))))))
       (test-assert (= (length bodies) 1) "Headless cleanup flushes once before propagating the exit.")
       (let ((root (first (test-telemetry-hooks--kind (first bodies) "run"))))
         (test-assert (equal "unfinished-run" (test-telemetry--attribute root "autolith.run.error_code"))
                      "An unreported boundary exit cannot claim success.")))))
  nil)

(-> test-telemetry-recovered-process-trace () null)
(defun test-telemetry-recovered-process-trace ()
  "Two fresh saved-core workers export one launcher trace with distinct attempt roots."
  (test-telemetry--call
   (lambda (configuration)
     (let ((context         "00-11111111111111111111111111111111-2222222222222222-01")
           (roots           nil)
           (ordinary-output (merge-pathnames "ordinary-child.json" (config :cache-root configuration))))
       (with-test-environment (("AUTOLITH_TRACEPARENT" context) ("AUTOLITH_RUN_ATTEMPT" "1"))
         (dotimes (index 2)
           (let* ((output (merge-pathnames (format nil "attempt-~D.json" index) (config :cache-root configuration)))
                  (worker (lisp-worker-create configuration :name (format nil "trace-attempt-~D" index)))
                  (form
                    `(let* ((*model-catalog-fetch-function* (lambda (run) (declare (ignore run)) nil))
                            (configuration
                              (make-configuration
                               ,@(loop for key in '(:source-root :working-directory :config-root :data-root :state-root :cache-root)
                                       append (list key (config key configuration))))))
                       (telemetry-shutdown)
                       (unless (and (null (uiop:getenv "AUTOLITH_TRACEPARENT"))
                                    (null (uiop:getenv "AUTOLITH_RUN_ATTEMPT")))
                         (error "Launcher context leaked into the active environment."))
                       (setf (config :telemetry-enabled-p configuration) t)
                       (telemetry-attach configuration)
                       (let ((*telemetry-process-function*
                               (lambda (run request)
                                 (declare (ignore run))
                                 (ensure-directories-exist ,output)
                                 (with-open-file (stream ,output :direction ':output :if-exists ':supersede)
                                   (write-string (third request) stream))
                                 t)))
                         (let ((input  (merge-pathnames "process-input.sexp" (config :cache-root configuration)))
                               (result (merge-pathnames "process-result.sexp" (config :cache-root configuration))))
                           (ensure-directories-exist input)
                           (with-open-file (stream input :direction ':output :if-exists ':supersede)
                             (write-string (run-job--write-data-sexp ',(run-job-tests--request-form)) stream))
                           (run-job--run-artifact
                            input result ':full-access :configuration configuration
                            :executor                                 (lambda (&rest arguments)
                                                                        (declare (ignore arguments))
                                                                        ,(if (zerop index)
                                                                             '(error "First attempt failed.")
                                                                             '(values ':succeeded (json-object "answer" "done")))))))
                       (telemetry-shutdown)
                       42)))
             (when (zerop index)
               (let ((ordinary-form (subst ordinary-output output form)))
                 (setf form
                       (append
                        (butlast form)
                        (list
                         `(let ((child (lisp-worker-create configuration :name "ordinary-child")))
                            (unwind-protect
                                 (let ((response (lisp-worker-request
                                                  child ':eval
                                                  (list :forms (list ,(let ((*package* (find-package '#:cl-user)))
                                                                       (write-to-string ordinary-form :readably t)))))))
                                   (unless (eq (getf (rest response) :status) ':ok)
                                     (error "Ordinary child failed: ~S" response)))
                              (lisp-worker-stop child)))
                         42)))))
             (unwind-protect
                  (let* ((*package* (find-package '#:cl-user))
                         (response  (lisp-worker-request worker ':eval (list :forms (list (write-to-string form :readably t))))))
                    (test-assert (eq (getf (rest response) :status) ':ok) "A fresh worker exports its process root.")
                    (push (first (test-telemetry-hooks--kind (uiop:read-file-string output) "run")) roots))
               (lisp-worker-stop worker)))
           (when (zerop index)
             (autolith-script-advance-run-attempt)))
         (setf roots (nreverse roots))
         (test-assert (equal (json-get (first roots) "traceId") (json-get (second roots) "traceId"))
                      "Recovery retains the original trace across processes.")
         (test-assert (not (equal (json-get (first roots) "spanId") (json-get (second roots) "spanId")))
                      "Each process creates a fresh attempt span ID.")
         (loop for root in roots for attempt from 1
               do (test-assert (equal (test-telemetry--attribute root "autolith.run.attempt") (write-to-string attempt))
                               "Recovery increments the exported attempt.")
                  (test-assert (eq (json-true-p (test-telemetry--attribute root "autolith.run.recovered"))
                                   (> attempt 1))
                               "Only reruns are marked recovered.")
                  (test-assert (null (json-get root "parentSpanId")) "Attempts remain parentless collector roots."))
         (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome") "error")
                      "The first failed process flushes its root before stopping.")
         (let ((ordinary (first (test-telemetry-hooks--kind (uiop:read-file-string ordinary-output) "run"))))
           (test-assert (not (equal (json-get ordinary "traceId") (json-get (first roots) "traceId")))
                        "An unrelated Autolith child starts its own trace.")
           (test-assert (and (equal (test-telemetry--attribute ordinary "autolith.run.attempt") "1")
                             (not (json-true-p (test-telemetry--attribute ordinary "autolith.run.recovered"))))
                        "An unrelated child is never a recovered attempt."))))))
  nil)

(-> test-telemetry-shell-context-isolation () null)
(defun test-telemetry-shell-context-isolation ()
  "Full-access shell.run inherits no context after startup consumes it."
  (with-test-fixture (':posix-shell "ordinary child trace isolation")
    (test-telemetry--call
     (lambda (configuration)
       (with-test-environment (("AUTOLITH_TRACEPARENT" "00-11111111111111111111111111111111-2222222222222222-01")
                               ("AUTOLITH_RUN_ATTEMPT" "2"))
         (let* ((*telemetry-launch-context*   (telemetry--consume-launch-context))
                (*telemetry-launch-claimed-p* nil)
                (registry                     (make-default-tool-registry))
                (context                      (shell-output-tests--context configuration registry))
                (tool                         (tool-registry-find registry "shell" "run"))
                (result                       (tool-execute
                                               tool context
                                               (json-object "command"
                                                            "test -z \"${AUTOLITH_TRACEPARENT+x}\" && test -z \"${AUTOLITH_RUN_ATTEMPT+x}\""))))
           (unwind-protect
                (progn
                  (test-assert (tool-result-success-p result) "An ordinary full-access shell inherits neither variable.")
                  (platform-setenv "AUTOLITH_TRACEPARENT" "changed-after-startup")
                  (let* ((bodies (test-telemetry-hooks--capture
                                  configuration
                                  (lambda ()
                                    (multiple-value-bind (run token) (telemetry--begin-run configuration :launcher-p t)
                                      (telemetry-finish-run run token '(:state :completed))))))
                         (root   (first (test-telemetry-hooks--kind (first bodies) "run"))))
                    (test-assert (and (equal (json-get root "traceId") "11111111111111111111111111111111")
                                      (equal (test-telemetry--attribute root "autolith.run.attempt") "2"))
                                 "Run creation uses the startup snapshot rather than rereading the environment.")))
             (tool-registry-close-runtime-state registry)))))))
  nil)

(-> test-telemetry-launch-context-validation () null)
(defun test-telemetry-launch-context-validation ()
  "Strictly validate external context and bounded decimal attempts without crashing."
  (let ((valid "00-11111111111111111111111111111111-2222222222222222-01"))
    (flet ((check-context (context)
             "Check one trace context the host environment can represent."
             (test-telemetry--call
              (lambda (configuration)
                (with-test-environment (("AUTOLITH_TRACEPARENT" context))
                  (let* ((diagnostics                  (make-string-output-stream))
                         (*error-output*               diagnostics)
                         (*telemetry-launch-context*   (telemetry--consume-launch-context))
                         (*telemetry-launch-claimed-p* nil)
                         (bodies                       (test-telemetry-hooks--capture
                                                        configuration
                                                        (lambda ()
                                                          (multiple-value-bind (run token) (telemetry--begin-run configuration :launcher-p t)
                                                            (telemetry-finish-run run token '(:state :completed))))))
                         (root                         (first (test-telemetry-hooks--kind (first bodies) "run")))
                         (trace                        (json-get root "traceId")))
                    (test-assert (if (equal context valid) (equal trace (subseq valid 3 35))
                                     (not (equal trace (subseq valid 3 35))))
                                 "Only strict version 00 context is adopted.")
                    (test-assert (eq (not (null (search "WARNING" (get-output-stream-string diagnostics))))
                                     (and context (not (equal context valid)) t))
                                 "Malformed supplied context warns without its contents.")))))))
      (dolist (context (list valid nil "" "00-bad" (concatenate 'string valid "-extra")
                            "01-11111111111111111111111111111111-2222222222222222-01"
                            "00-00000000000000000000000000000000-2222222222222222-01"
                            "00-11111111111111111111111111111111-0000000000000000-01"
                            "00-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA-2222222222222222-01"
                            "00-11111111111111111111111111111111-2222222222222222-0g"))
        (if (equal context "")
            (with-test-fixture (':empty-environment-values "empty supplied trace context")
              (check-context context))
            (check-context context))))
    (dolist (case '((nil 1) ("1" 1) ("2" 2) ("65535" 65535) ("0" 1) ("65536" 1)
                    ("-1" 1) ("+2" 1) ("2x" 1) (" 2" 1) ("" 1) ("999999999999999999999999" 1)))
      (let ((*error-output* (make-string-output-stream)))
        (test-assert (= (telemetry--run-attempt (first case)) (second case)) "Attempts are bounded decimal integers.")
        (when (and (first case) (= (second case) 1) (not (equal (first case) "1")))
          (test-assert (search "WARNING" (get-output-stream-string *error-output*)) "Malformed attempts warn.")))))
  nil)

(-> test-telemetry-launch-context-lifetime () null)
(defun test-telemetry-launch-context-lifetime ()
  "Interactive turns never claim context; headless claims survive owner replacement."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((*telemetry-launch-context*   '("11111111111111111111111111111111" 2))
            (*telemetry-launch-claimed-p* nil)
            (application                  (make-instance 'application :configuration configuration))
            (replacement                  nil)
            (bodies
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (telemetry-call-with-run configuration (lambda () t))
                 (handler-case
                     (telemetry-call-with-run configuration (lambda () (error "Failed later turn.")))
                   (error () nil))
                 (setf replacement (configuration-copy configuration)
                       (application-configuration application) replacement)
                 (telemetry-call-with-run replacement (lambda () t))
                 (test-assert (not *telemetry-launch-claimed-p*) "Interactive turns leave launcher context unclaimed.")
                 (multiple-value-bind (run token) (telemetry--begin-run replacement :launcher-p t)
                   (telemetry-finish-run run token '(:state :completed)))
                 (setf (application-configuration application) configuration)
                 (multiple-value-bind (run token) (telemetry--begin-run configuration :launcher-p t)
                   (telemetry-finish-run run token '(:state :completed))))))
            (roots                        (loop for body in bodies append (test-telemetry-hooks--kind body "run"))))
       (test-assert (= (length roots) 5) "Each independent owner exports one root.")
       (test-assert (= (length (remove-duplicates (mapcar (lambda (root) (json-get root "traceId")) roots)
                                                 :test #'equal)) 5)
                    "Configuration replacement and a failed later turn never reuse a trace.")
       (loop for root in roots for index from 0
             do (test-assert (equal (test-telemetry--attribute root "autolith.run.attempt")
                                    (if (= index 3) "2" "1"))
                             "Only the first headless owner adopts the attempt.")
                (test-assert (eq (equal (json-get root "traceId") (first *telemetry-launch-context*)) (= index 3))
                             "Only the first headless owner adopts the launcher trace.")))))
  nil)

(-> test-telemetry-scheduler-cancellation-publication () null)
(defun test-telemetry-scheduler-cancellation-publication ()
  "An escaping cancellation finishes after the real pool's artifact publication."
  (dolist (case '((:cancelled nil "cancelled" "cancelled")
                  (:cancelled t "error" "internal")
                  (:timeout nil "error" "timeout")
                  (:timeout t "error" "internal")))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((publication-fails-p (second case))
              (orchestrator        (task-orchestrator-create))
              (parent              (task-tests--primary-agent configuration "cancel-publication"))
              (definition          (task-agent-definition-create
                                    :name "cancel-publication" :description "Cancel before publication."
                                    :instructions "Cancel." :source ':test))
              (write-artifact      (symbol-function 'task--write-result-artifact))
              (bodies              nil))
         (unwind-protect
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (test-call-with-function-replacements
                  (list (list 'task-run-child
                              (lambda (job)
                                (error 'job-aborted :reason (first case) :identifier (job-identifier job)
                                                   :message "Injected cancellation.")))
                        (list 'task--write-result-artifact
                              (lambda (job result)
                                (test-assert (telemetry-run-active (task-job-telemetry-run job))
                                             "The root remains live until publication resolves.")
                                (if publication-fails-p
                                    (error "Injected publication failure.")
                                    (funcall write-artifact job result))))
                        (list 'telemetry--process
                              (lambda (run request)
                                (declare (ignore run))
                                (when (eq (first request) ':upload) (push (third request) bodies))
                                t)))
                  (lambda ()
                    (let ((job (first (task-orchestrator-start-jobs
                                       orchestrator parent
                                       (list (list :definition definition :item (list :task "Cancel.")))))))
                      (multiple-value-bind (snapshot terminal-p) (task-job-await job 10)
                        (test-assert (and terminal-p (eq (getf snapshot :state)
                                                        (if publication-fails-p ':failed ':aborted)))
                                     "Publication decides the final pool state.")))))))
           (task-orchestrator-close orchestrator))
         (let ((roots (loop for body in bodies append (test-telemetry-hooks--kind body "run"))))
           (test-assert (= (length roots) 1) "The scheduler owner exports exactly one root.")
           (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome")
                               (third case))
                        "A publication failure overrides the earlier cancellation.")
           (test-assert (equal (test-telemetry--attribute (first roots) "error.type")
                               (fourth case))
                        "Failure metadata includes publication status."))))))
  nil)

(-> test-telemetry-launch-context-atomic-claim () null)
(defun test-telemetry-launch-context-atomic-claim ()
  "Concurrent headless claimants consume the process context exactly once."
  (let ((previous *telemetry-launch-claimed-p*)
        (ready    (bt:make-semaphore))
        (threads  nil))
    (unwind-protect
         (progn
           (setf *telemetry-launch-claimed-p* nil)
           (dotimes (index 8)
             (push (bt:make-thread (lambda ()
                                     (bt:wait-on-semaphore ready)
                                     (telemetry--claim-launch-context))
                                   :name (format nil "launcher claimant ~D" index))
                   threads))
           (dotimes (index 8) (bt:signal-semaphore ready))
           (test-assert (= (count-if #'identity (mapcar #'bt:join-thread threads)) 1)
                        "Exactly one concurrent caller receives the startup context.")
           (test-assert (null (telemetry--claim-launch-context)) "The context stays consumed."))
      (dolist (thread threads)
        (bt:signal-semaphore ready)
        (bt:join-thread thread))
      (setf *telemetry-launch-claimed-p* previous)))
  nil)

(-> test-telemetry-run-job-publication-outcomes () null)
(defun test-telemetry-run-job-publication-outcomes ()
  "Finalize real headless traces only after contract checking and result publication."
  (dolist (case '(:success :missing-data :invalid-data :artifact-failure :result-publication-failed))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((provider
                (make-instance
                 'run-job-test-provider :configuration configuration
                 :results                              (list (agent-test-result
                                                              "publication-outcome"
                                                              (list (agent-test-call
                                                                     :call-id "terminal" :namespace "yield" :name "submit"
                                                                     :arguments "{\"status\":\"success\",\"data\":{\"answer\":\"yes\"}}"))))))
              (assemble (symbol-function 'task--assemble-child-result)))
         (test-call-with-function-replacements
          (case case
            ((:missing-data :invalid-data)
             (list (list 'task--assemble-child-result
                         (lambda (&rest arguments)
                           (let ((result (apply assemble arguments)))
                             (if (eq case ':missing-data)
                                 (setf (getf result :structured-output-present-p) nil
                                       (getf result :structured-output) nil)
                                 (setf (getf result :structured-output) '(:object ("wrong" "value"))))
                             result)))))
            (:artifact-failure
             (list (list 'run-job--read-task-result-artifact
                         (lambda (path)
                           (declare (ignore path))
                           (run-job--error ':artifact-failure "Injected unreadable child artifact.")))))
            (:result-publication-failed
             (list (list 'run-job-write-result-atomically
                         (lambda (path envelope)
                           (declare (ignore path envelope))
                           (error "Injected boundary publication failure."))))))
          (lambda ()
            (multiple-value-bind (spans results)
                (test-telemetry-hooks--run-job configuration provider :publish-p t)
              (let* ((roots    (remove-if-not
                                (lambda (span)
                                  (equal (test-telemetry--attribute span "autolith.event.kind") "run"))
                                spans))
                     (terminal (find ':run-finished (third results)
                                     :key (lambda (record) (getf (rest record) :kind))))
                     (data     (getf (rest terminal) :data))
                     (fields   (rest (second results)))
                     (category (case case
                                 ((:missing-data :invalid-data)
                                  ':invalid-output)
                                 (:artifact-failure
                                  ':artifact-failure)
                                 (:result-publication-failed
                                  ':result-publication-failed))))
                (test-assert (= (length roots) 1)
                             (format nil "~A: Publication exports exactly one root." case))
                (test-assert (eq (getf data :status) (if category ':failed ':succeeded))
                             (format nil "~A: The terminal event agrees with boundary classification." case))
                (test-assert (eql (first results) (if category 1 0))
                             (format nil "~A: The exit code agrees with publication success." case))
                (test-assert (eq (getf data :category) category)
                             (format nil "~A: The terminal failure category is retained." case))
                (if (eq case ':result-publication-failed)
                    (test-assert (and (null (second results)) (not (getf data :published-p)))
                                 (format nil "~A: A publication failure leaves no success artifact." case))
                    (test-assert (and (eq (getf fields :status) (getf data :status))
                                      (eq (getf (getf fields :failure) :category) category))
                                 (format nil "~A: The published envelope agrees with its terminal event." case)))
                (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome")
                                    (if category "error" "success"))
                             (format nil "~A: The root agrees with the published terminal state." case))
                (test-assert (equal (test-telemetry--attribute (first roots) "error.type")
                                    (case case
                                      (:success
                                       nil)
                                      ((:missing-data :invalid-data)
                                       "protocol")
                                      (otherwise
                                       "internal")))
                             (format nil "~A: Invalid output is protocol failure; persistence failures are internal." case))))))))))
  nil)

(-> test-telemetry-run-job-zero-request-failure () null)
(defun test-telemetry-run-job-zero-request-failure ()
  "Respect a published provider failure even when no provider request started."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((input  (merge-pathnames "zero-request.sexp" (config :cache-root configuration)))
            (output (merge-pathnames "zero-request-result.sexp" (config :cache-root configuration))))
       (task-tests--write-text input (run-job--write-data-sexp (run-job-tests--request-form)))
       (dolist (category '(:provider-failure :process-failure))
         (let* ((bodies (test-telemetry-hooks--capture
                         configuration
                         (lambda ()
                           (run-job-run
                            input output ':full-access :configuration configuration
                            :executor                                 (lambda (configuration request permissions)
                                                                        (declare (ignore configuration request permissions))
                                                                        (values ':failed nil nil '(:provider-requests 0) category
                                                                                "Model rejected before a request."))))))
                (root   (first (test-telemetry-hooks--kind (first bodies) "run")))
                (fields (rest (run-job-read-file output))))
           (test-assert (eq (getf (getf fields :failure) :category) category)
                        "The artifact keeps the supplied failure classification.")
           (test-assert (zerop (getf (getf fields :usage) :provider-requests))
                        "No request was counted.")
           (test-assert (equal (test-telemetry--attribute root "autolith.run.outcome") "error")
                        "Zero requests cannot turn a failure into success.")
           (test-assert (equal (test-telemetry--attribute root "error.type")
                               (if (eq category ':provider-failure) "provider" "internal"))
                        "The terminal category, rather than request count, determines the root error."))))))
  nil)

(-> test-telemetry-run-job-provider-failure () null)
(defun test-telemetry-run-job-provider-failure ()
  "Export exactly one failed root for a real unlisted-model headless job."
  (test-telemetry--call
   (lambda (configuration)
     (let ((snapshot (provider--registry-snapshot))
           (model    "gpt-e2e-unlisted-9"))
       (unwind-protect
            (progn
              (register-openai-compatible-provider
               :name "telemetry-e2e" :family ':openai :models (list model)
               :endpoint "http://127.0.0.1:1/v1/chat/completions")
              (setf (config :model configuration) model)
              (multiple-value-bind (spans results)
                  (test-telemetry-hooks--run-job configuration (provider-create configuration))
                (test-assert (eq (first results) ':failed) "The real job fails without credentials.")
                (test-assert (eq (fifth results) ':provider-failure) "The terminal job records PROVIDER-FAILURE.")
                (let ((roots (remove-if-not
                               (lambda (span)
                                 (equal (test-telemetry--attribute span "autolith.event.kind") "run"))
                               spans)))
                  (test-assert (= (length roots) 1) "One failed job exports exactly one run root.")
                  (test-assert (= (length (remove-duplicates (mapcar (lambda (span) (json-get span "traceId")) spans)
                                                             :test #'equal))
                                  1)
                               "The provider failure and terminal job share one trace.")
                  (dolist (root roots)
                    (test-assert (equal (test-telemetry--attribute root "autolith.run.outcome") "error")
                                 "A terminal failed job never exports success.")
                    (test-assert (equal (test-telemetry--attribute root "error.type") "provider")
                                 "The root retains the real terminal failure category.")
                    (test-assert (equal (test-telemetry--attribute root "gen_ai.request.model") "custom")
                                 "The failed job projects its private requested model.")
                    (test-assert (not (json-true-p (test-telemetry--attribute root "autolith.model.known")))
                                 "An unlisted model stays outside the catalog.")))))
         (provider--registry-restore snapshot)))))
  nil)

(-> test-telemetry-run-job-unregistered-model () null)
(defun test-telemetry-run-job-unregistered-model ()
  "Classify real deferred unknown-model rejection honestly before any provider request."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((configuration (configuration-copy configuration :provider-validation-p nil
                                               :model                               "gpt-e2e-unlisted-9"))
            (input         (merge-pathnames "unknown-model.sexp" (config :cache-root configuration)))
            (output        (merge-pathnames "unknown-model-result.sexp" (config :cache-root configuration)))
            (exit-code     nil))
       (task-tests--write-text input (run-job--write-data-sexp (run-job-tests--request-form)))
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (setf exit-code (run-job-run input output ':full-access :configuration configuration)))))
              (roots  (test-telemetry-hooks--kind (first bodies) "run"))
              (fields (rest (run-job-read-file output))))
         (test-assert (= exit-code 1) "Deferred unknown-model rejection fails the real boundary.")
         (test-assert (eq (getf (getf fields :failure) :category) ':process-failure)
                      "An unregistered model fails configuration, before a provider is invoked.")
         (test-assert (zerop (getf (getf fields :usage) :provider-requests)) "No request started.")
         (test-assert (= (length roots) 1) "Configuration rejection exports exactly one root.")
         (test-assert (null (test-telemetry-hooks--kind (first bodies) "model"))
                      "No model span claims a provider attempt.")
         (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome") "error")
                      "Configuration rejection is a failed run.")
         (test-assert (equal (test-telemetry--attribute (first roots) "error.type") "internal")
                      "A local configuration failure is not relabelled as a provider rejection.")))))
  nil)

(-> test-telemetry-run-job-terminal-yields () null)
(defun test-telemetry-run-job-terminal-yields ()
  "Use the published job state even when its agent turn returned normally."
  (dolist (case '(("success" :succeeded "success" nil)
                  ("failed" :failed "error" "internal")
                  ("aborted" :cancelled "cancelled" "cancelled")))
    (test-telemetry--call
     (lambda (configuration)
       (let ((provider
               (make-instance
                'run-job-test-provider :configuration configuration
                :results
                (list (agent-test-result
                       "terminal-yield"
                       (list (agent-test-call
                              :call-id "terminal" :namespace "yield" :name "submit"
                              :arguments
                              (json-encode
                               (if (equal (first case) "success")
                                   (json-object "status" "success" "data" (json-object "answer" "yes"))
                                   (json-object "status" (first case) "error" "The assignment failed."))))))))))
         (multiple-value-bind (spans results)
             (test-telemetry-hooks--run-job configuration provider)
           (let ((roots (remove-if-not
                         (lambda (span)
                           (equal (test-telemetry--attribute span "autolith.event.kind") "run"))
                         spans)))
             (test-assert (eq (first results) (second case)) "The real job publishes the yielded state.")
             (test-assert (= (length roots) 1) "One completed job has one root across provider and agent wrappers.")
             (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome") (third case))
                          "The run outcome follows the terminal job rather than the normal function return.")
             (test-assert (equal (test-telemetry--attribute (first roots) "error.type") (fourth case))
                          "The terminal state determines the root category.")))))))
  nil)

(-> test-telemetry-run-job-terminal-failures () null)
(defun test-telemetry-run-job-terminal-failures ()
  "Export errors for publication failure and deadline cancellation after successful work."
  (dolist (case '((publication-failure nil :failed "internal")
                  (deadline-expired t :timed-out "timeout")))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((timeout-p (second case))
              (provider
               (make-instance
                'run-job-test-provider :configuration configuration :block-on-empty-p timeout-p
                :results
                (list (agent-test-result
                       "before-terminal-failure"
                       (list (agent-test-call
                              :call-id "terminal-failure" :namespace "yield"
                              :name      (if timeout-p "note" "submit")
                              :arguments (if timeout-p
                                             "{\"note\":\"Keep working.\"}"
                                             "{\"status\":\"success\",\"data\":{\"answer\":\"yes\"}}"))))))))
         (test-call-with-function-replacements
          (unless timeout-p
            (list (list 'task--write-result-artifact
                        (lambda (job result)
                          (declare (ignore job result))
                          (error "Injected task result publication failure.")))))
          (lambda ()
            (multiple-value-bind (spans results)
                (test-telemetry-hooks--run-job configuration provider :timeout-seconds 1)
              (let ((roots (remove-if-not
                            (lambda (span)
                              (equal (test-telemetry--attribute span "autolith.event.kind") "run"))
                            spans)))
                (test-assert (eq (first results) (third case))
                             (format nil "~A: The job reaches the expected real terminal failure." (first case)))
                (test-assert (= (length roots) 1)
                             (format nil "~A: Terminal failures export exactly one job root." (first case)))
                (test-assert (equal (test-telemetry--attribute (first roots) "autolith.run.outcome") "error")
                             (format nil "~A: Successful work cannot hide a later terminal failure." (first case)))
                (test-assert (equal (test-telemetry--attribute (first roots) "error.type")
                                    (fourth case))
                             (format nil "~A: The final job error retains its applicable category." (first case)))))))))))
  nil)

(-> test-telemetry-run-protocol-failure () null)
(defun test-telemetry-run-protocol-failure ()
  "Classify a real duplicate-call invariant failure at the root boundary."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((provider (make-instance
                       'scripted-provider :results
                       (loop for index from 1 to 2
                             collect (agent-test-result
                                      (format nil "response-~D" index)
                                      (list (agent-test-call :call-id   "repeated"
                                                             :arguments "{\"value\":\"private-value\"}"))))))
            (agent    (agent-create :configuration configuration :provider provider
                                    :conversation (conversation-create configuration)
                                    :tool-registry (agent-test-registry) :worker ':unused))
            (caught   nil)
            (bodies   (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (handler-case (agent-run-user-turn agent "Exercise call identity.")
                           (agent-loop-error () (setf caught t))))))
            (span     (first (test-telemetry-hooks--kind (first bodies) "run"))))
       (test-assert (and caught
                         (equal (test-telemetry--attribute span "autolith.run.outcome") "error")
                         (equal (test-telemetry--attribute span "error.type") "protocol"))
                    "A failed agent protocol invariant exports its root category.")
       (test-telemetry--collector-fixture "native-run-protocol.json" (json-decode (first bodies))))))
  nil)

(-> test-telemetry-job-cancellation-hooks () null)
(defun test-telemetry-job-cancellation-hooks ()
  "Preserve typed job timeout and cancellation at the root boundary."
  (test-telemetry--call
   (lambda (configuration)
     (dolist (case '((:timeout "timeout" "error") (:cancelled "cancelled" "cancelled")))
       (let* ((caught nil)
              (bodies (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (handler-case
                             (telemetry-call-with-run
                              configuration
                              (lambda ()
                                (telemetry--provider-attempt
                                 (test-codex-provider-create configuration nil)
                                 (lambda ()
                                   (error 'job-aborted :reason    (first case)
                                                      :identifier "PRIVATE-JOB-ID"
                                                      :message    "PRIVATE-CANCELLATION-REPORT")))))
                           (job-aborted () (setf caught t))))))
              (span   (first (test-telemetry-hooks--kind (first bodies) "run")))
              (model  (first (test-telemetry-hooks--kind (first bodies) "model"))))
         (test-assert
          (and caught
               (equal (test-telemetry--attribute span "error.type") (second case))
               (equal (test-telemetry--attribute span "autolith.run.outcome") (third case)))
          "Job cancellation retains its bounded category and terminal outcome.")
         (test-assert
          (and (equal (test-telemetry--attribute model "error.type") (second case))
               (equal (test-telemetry--attribute model "autolith.model.outcome") (third case)))
          "Interrupted model attempts preserve the same cancellation category.")
         (test-assert (not (search "PRIVATE-" (first bodies)))
                      "Cancellation reports and job identifiers stay private.")))))
  nil)

(-> test-telemetry-configuration-owner-hook () null)
(defun test-telemetry-configuration-owner-hook ()
  "Replacing the owning application's configuration invalidates old runs and consent."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((application (make-instance 'application :configuration configuration))
            (replacement (configuration-copy configuration :telemetry-enabled-p nil))
            (stale-copy  nil))
       (test-telemetry-hooks--capture
        configuration
        (lambda ()
          (setf stale-copy (configuration-copy configuration))
          (telemetry-call-with-run
           configuration
           (lambda ()
             (let ((old-run *telemetry-run*))
               (test-assert (eq (setf (application-configuration application) replacement) replacement)
                            "The configuration setter preserves its value.")
               (test-assert (eq (telemetry-controller-owner *telemetry-controller*) replacement)
                            "The replacement is the process consent authority.")
               (test-assert (not (telemetry--live-p old-run)) "Owner replacement invalidates in-flight old runs.")
               (telemetry-note-model :run old-run :provider "local"))))))
       (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "Retired owners cannot queue spans.")
       (telemetry-call-with-run
        stale-copy
        (lambda () (test-assert (null *telemetry-run*) "A stale enabled copy cannot re-enable replacement consent.")))
       (test-assert (not (telemetry-controller-enabled *telemetry-controller*)) "The new default-off owner is authoritative."))))
  nil)

(-> test-telemetry-secondary-root-settings () null)
(defun test-telemetry-secondary-root-settings ()
  "Interactive ACP and application settings use the process consent authority."
  (test-telemetry--call
   (lambda (configuration)
     (telemetry-attach configuration)
     (let* ((secondary   (configuration-copy configuration))
            (application (make-instance 'application :configuration secondary)))
       (telemetry-attach secondary)
       (application-apply-setting application :telemetry-enabled-p t)
       (test-assert (eq (config :telemetry-enabled-p configuration) t)
                    "A deliberate secondary-root consent write changes the owner.")
       (test-assert (null (config :telemetry-enabled-p secondary))
                    "A copied snapshot is not the consent authority.")
       (test-assert (eq (json-get (acp-extension--setting-item
                                   application secondary (find-setting :telemetry-enabled-p))
                                "value") t)
                    "ACP displays process-authoritative consent.")
       (telemetry-call-with-run
        secondary
        (lambda ()
          (test-assert *telemetry-run* "An attached root uses current process consent.")
          (application-apply-setting application :telemetry-enabled-p nil)
          (test-assert (not (telemetry--live-p *telemetry-run*))
                       "Interactive revocation invalidates the in-flight root."))))))
  nil)
