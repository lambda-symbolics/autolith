(in-package #:autolith)

;;;; -- Scheduler Boundary Tests --

(-> task-tests--release-blocking-tool (task-test-blocking-tool) null)
(defun task-tests--release-blocking-tool (tool)
  "Permit TOOL to return normally when a cancellation test is unwinding."
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-released-p tool) t)
    (sb-thread:condition-broadcast
     (task-test-blocking-tool-condition-variable tool)))
  nil)

(-> test-task-durable-job-lookup () null)
(defun test-task-durable-job-lookup ()
  "Test conversation-owned durable result lookup and validation."
  (let* ((configuration        (test-configuration))
         (root                 (test-configuration-root configuration))
         (registry             (task-augment-tool-registry (make-default-tool-registry)))
         (get-tool             (tool-registry-find registry "job" "get"))
         (primary-a
          (task-tests--primary-agent configuration "durable-lookup-primary-a" registry))
         (primary-b
          (task-tests--primary-agent configuration "durable-lookup-primary-b" registry))
         (execution-identifier (make-identifier))
         (artifact-path
          (merge-pathnames (format nil "~A/result.sexp" execution-identifier)
                           (task--artifact-group-root configuration
                                                      (conversation-identifier
                                                       (agent-conversation primary-a)))))
         (durable-result
          (list :id "resumed-child-1" :name "resumed-child" :status
                ':success :output "Recovered durable child output." :error nil
                :duration-ms 17 :model "test-model" :detached t)))
    (labels ((execute-get (viewer identifier)
               (tool-execute get-tool
                             (make-instance 'tool-context :configuration configuration
                                            :worker nil :conversation
                                            (agent-conversation viewer) :registry
                                            registry :agent viewer :call-id
                                            "durable-job-lookup")
                             (json-object "id" identifier))))
      (unwind-protect
          (progn
           (task-continuity--write
            (merge-pathnames "continuity.sexp" (uiop:pathname-directory-pathname artifact-path))
            (list :version 1 :kind ':task :job-id "resumed-child-1"
                  :execution-id                  execution-identifier
                  :root-conversation             (conversation-identifier (agent-conversation primary-a))
                  :completion-owner-conversation (conversation-identifier (agent-conversation primary-a))
                  :owners                        nil))
           (snapshot-write artifact-path durable-result)
           (let* ((result        (execute-get primary-a execution-identifier))
                  (details       (rest (tool-result-details result)))
                  (record        (getf details :job))
                  (result-record (getf record :result))
                  (artifact      (getf result-record :artifact)))
             (test-assert
              (and (tool-result-success-p result) (getf details :durable-p)
                   (string= (getf record :id) "resumed-child-1")
                   (string= (getf record :execution-id) execution-identifier)
                   (eq (getf record :state) :completed)
                   (eq (getf result-record :status) :success)
                   (string= (getf result-record :output)
                            "Recovered durable child output.")
                   (string= (getf artifact :path) (namestring artifact-path))
                   (getf artifact :available-p))
              "job.get reconstructs a bounded terminal record from its artifact"))
           (snapshot-write artifact-path '(:status :success :status :failed))
           (test-assert
            (handler-case (progn (execute-get primary-a execution-identifier) nil)
                          (task-error (condition)
                           (string= (tool-error-tool-name condition) "job.get")))
            "job.get reports malformed durable result records at the task boundary")
           (snapshot-write artifact-path durable-result)
           (let ((*task-result-maximum-octets* 16))
             (test-assert
              (handler-case (progn (execute-get primary-a execution-identifier) nil)
                            (task-error nil t))
              "job.get bounds durable artifacts before reconstructing results"))
           (test-assert
            (handler-case (progn (execute-get primary-b execution-identifier) nil)
              (task-error (condition)
                (search "No visible job" (princ-to-string condition))))
            "durable lookup does not expose another conversation's artifact")
           (task-continuity--write
            (merge-pathnames "continuity.sexp" (uiop:pathname-directory-pathname artifact-path))
            (list :version 1 :kind ':task :job-id "resumed-child-1"
                  :execution-id                  execution-identifier
                  :root-conversation             (conversation-identifier (agent-conversation primary-a))
                  :completion-owner-conversation (conversation-identifier (agent-conversation primary-b))
                  :owners                        nil))
           (test-assert
            (handler-case (progn (execute-get primary-a execution-identifier) nil)
              (task-error (condition)
                (search "No visible job" (princ-to-string condition))))
            "an artifact under the viewer's root requires the exact recorded owner"))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-session-tool-execution-jobs () null)
(defun test-session-tool-execution-jobs ()
  "Test visibility, inspection, waiting, and cancellation of tool jobs."
  (let* ((configuration   (test-configuration))
         (root            (test-configuration-root configuration))
         (registry        (task-augment-tool-registry (make-default-tool-registry)))
         (orchestrator    (tool-registry-runtime-binding registry 'task-orchestrator))
         (primary-a       (task-tests--primary-agent configuration "execution-primary-a" registry))
         (primary-b       (task-tests--primary-agent configuration "execution-primary-b" registry))
         (context         (make-instance 'tool-context :configuration configuration :worker nil
                                         :conversation (agent-conversation primary-a)
                                         :registry registry :agent primary-a
                                         :call-id      "execution-job-test"))
         (barrier         (make-instance 'task-test-blocking-tool :namespace "test" :name
                                         "execution-block" :description "Hold an execution job."
                                         :parameters (tool-object-schema (json-object) nil)))
         (execution-job   nil)
         (operation-count 0))
    (unwind-protect
         (progn
           (setf execution-job
                 (task-orchestrator-start-execution-job
                  orchestrator primary-a :tool-name "lisp.eval"
                  :summary                          "Wait inside an inspectable execution job."
                  :operation-function               (lambda ()
                                                      (incf operation-count)
                                                      (tool-execute barrier context (json-object)))
                  :detached-p t :parent-call-id "execution-job-test"))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held ((task-test-blocking-tool-lock barrier))
                 (task-test-blocking-tool-started-p barrier))) 2)
            "the asynchronous operation starts on the execution pool")
           (let ((identifier (session-job-identifier execution-job)))
             (test-assert
              (and (string= identifier (format nil "exec:~D" (session-job-order execution-job)))
                   (equal (mapcar #'session-job-identifier
                                  (task-orchestrator-list-visible-jobs orchestrator primary-a))
                          (list identifier))
                   (null (task-orchestrator-list-visible-jobs orchestrator primary-b)))
              "the owning conversation sees its execution job")
             (multiple-value-bind (snapshot job)
                 (task-query-job orchestrator primary-a :identifier identifier :tool-name "job.get")
               (test-assert
                (and (eq job execution-job) (eq (getf snapshot :type) ':tool)
                     (eq (getf snapshot :state) ':running)
                     (string= (getf snapshot :tool) "lisp.eval"))
                "job lookup returns a live execution snapshot"))
             (multiple-value-bind (snapshot terminal-p) (session-job-await execution-job 0)
               (test-assert (and (not terminal-p) (eq (getf snapshot :state) ':running))
                            "await returns a nonterminal snapshot on timeout"))
             (session-job-cancel execution-job :test-cancellation)
             (multiple-value-bind (snapshot terminal-p) (session-job-await execution-job 2)
               (test-assert
                (and terminal-p (= operation-count 1)
                     (eq (getf snapshot :state) ':aborted)
                     (eq (getf (getf snapshot :result) :status) ':aborted)
                     (null (tool-execution-job-operation-function execution-job)))
                "the execution runs once, aborts, and releases its closure"))))
      (task-tests--release-blocking-tool barrier)
      (when (and execution-job (not (job-terminal-p execution-job)))
        (session-job-cancel execution-job :test-cleanup)
        (session-job-await execution-job 2))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-execution-admission-presentation () null)
(defun test-execution-admission-presentation ()
  "Present admitted shell work even while every execution worker is occupied."
  (with-test-configuration (configuration)
    (let* ((registry     (task-augment-tool-registry (make-instance 'tool-registry)))
           (orchestrator (tool-registry-runtime-binding registry 'task-orchestrator))
           (parent       (task-tests--primary-agent configuration "queued-shell" registry))
           (terminal     (make-instance 'recording-terminal :columns 100 :rows 24))
           (ui           (terminal-ui-create :terminal terminal))
           (application  (make-instance 'application :configuration configuration
                                        :conversation               (agent-conversation parent)
                                        :tool-registry registry :ui ui))
           (lock         (make-lock "Queued shell presentation"))
           (condition    (make-condition-variable))
           (ready-p      nil)
           (release-p    nil))
      (unwind-protect
           (with-terminal-ui (ui ui)
             (let ((pool (task-orchestrator-execution-pool orchestrator)))
               (job-pool-update-limits
                pool :maximum-concurrency     1
                :maximum-batch-size           (job-pool-maximum-batch-size pool)
                :maximum-live-jobs            (job-pool-maximum-live-jobs pool)
                :maximum-runtime-milliseconds (job-pool-maximum-runtime-milliseconds pool)))
             (application-connect-task-presentation application)
             (task-orchestrator-start-execution-job
              orchestrator parent :tool-name "shell.run" :summary "held command"
              :operation-function
              (lambda ()
                (with-lock-held (lock)
                  (setf ready-p t)
                  (sb-thread:condition-broadcast condition)
                  (loop until release-p do (condition-wait condition lock :timeout 2)))
                (make-instance 'tool-result :content "done")))
             (test-assert (task-tests--wait-until (lambda () ready-p) 5)
                          "the first execution occupies the only worker")
             (let* ((job        (task-orchestrator-start-execution-job
                                 orchestrator parent :tool-name "shell.run" :summary "queued command"
                                 :operation-function (lambda () (make-instance 'tool-result :content "done"))))
                    (identifier (session-job-identifier job))
                    (row        (find identifier (terminal-ui-command-activities ui)
                                      :key (lambda (activity) (getf activity :id)) :test #'equal)))
               (test-assert (and (eq (job-state job) ':queued) row
                                 (eq (getf row :state) ':queued)
                                 (not (getf row :detached)))
                            "synchronous queued work reaches the prompt area before a worker starts it")
               (recording-terminal-reset terminal)
               (terminal-ui-refresh-status ui)
               (test-assert (search identifier (recording-terminal-output terminal))
                            "the reader can paint the queued execution's identity")))
        (with-lock-held (lock)
          (setf release-p t)
          (sb-thread:condition-broadcast condition))
        (application-disconnect-task-presentation application)
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-shell-execution-jobs () null)
(defun test-shell-execution-jobs ()
  "Test authorized shell jobs, automatic handoff, inspection, and cancellation."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (shell-tool          (tool-registry-find registry "shell" "run"))
         (orchestrator        (tool-registry-runtime-binding registry 'task-orchestrator))
         (primary
           (task-tests--primary-agent
            configuration "shell-execution-primary" registry))
         (authorization-count 0)
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              (incf authorization-count)
              ':full-access)))
         (denied-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-denied-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              ':deny)))
         (sandboxed-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-sandboxed-test"
            :command-authorization-function
            (lambda (command directory)
              (declare (ignore command directory))
              ':sandboxed)))
         (unavailable-context
           (make-instance
            'tool-context
            :configuration configuration
            :worker        nil
            :conversation  (agent-conversation primary)
            :registry      registry
            :agent         primary
            :call-id       "shell-execution-unavailable-test"
            :command-authorization-function
            (lambda (command directory)
              (error 'command-authorization-unavailable
                     :message   "Command approval requires an interactive terminal."
                     :command   command
                     :directory directory))))
         (slow-path           (merge-pathnames "shell-execution-once.txt" root))
         (started-path        (merge-pathnames "shell-execution-started.txt" root))
         (finished-path       (merge-pathnames "shell-execution-finished.txt" root)))
    (labels ((run-shell (execution-context &rest arguments)
               "Execute shell.run with decoded test ARGUMENTS."
               (tool-execute
                shell-tool execution-context
                (apply #'json-object arguments)))

             (execution-count ()
               "Return the retained asynchronous execution count."
               (length
                (job-pool-list-jobs
                 (task-orchestrator-execution-pool orchestrator))))

             (handoff-job (result)
               "Return RESULT's visible handed-off execution job."
               (let* ((details    (tool-result-details result))
                      (record     (and (listp details)
                                       (getf (rest details) :job)))
                      (identifier (and record (getf record :id))))
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier :viewer primary :tool-name "shell.run")))))
      (unwind-protect
           (progn
             (let* ((parameters   (tool-parameters shell-tool))
                    (properties   (json-get parameters "properties"))
                    (required     (json-get parameters "required"))
                    (async-schema (and properties
                                       (gethash "async" properties)))
                    (description-schema
                      (and properties
                           (gethash "description" properties))))
               (test-assert
                (and (json-object-p async-schema)
                     (string= (json-get async-schema "type") "boolean")
                     (json-object-p description-schema)
                     (string= (json-get description-schema "type") "string")
                     (not (find "description" required :test #'string=)))
                "shell.run advertises an optional description"))
             (let ((before (execution-count))
                   (result
                     (run-shell denied-context
                                "command"
                                (format nil "printf denied > ~A"
                                        (uiop:escape-shell-token
                                         (namestring slow-path)))
                                "async" t)))
               (test-assert
                (and (not (tool-result-success-p result))
                     (= (execution-count) before)
                     (not (probe-file slow-path)))
                "shell authorization denial occurs before execution-job admission"))
             (let ((before (execution-count))
                   (result
                     (run-shell unavailable-context
                                "command"
                                (format nil "printf unavailable > ~A"
                                        (uiop:escape-shell-token
                                         (namestring slow-path))))))
               (test-assert
                (and (not (tool-result-success-p result))
                     (search "interactive terminal"
                             (tool-result-content result))
                     (= (execution-count) before)
                     (not (probe-file slow-path)))
                "unavailable interactive authorization becomes a failed tool result before subprocess admission"))
             (with-test-fixture (':posix-shell "shell execution below the POSIX root")
               (let ((result
                       (run-shell context
                                  "command" "pwd -P"
                                  "directory" "/")))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search (format nil "~%/~%")
                               (tool-result-content result)))
                  "full-access shell execution accepts a directory outside workspace roots")))
             (test-assert
              (handler-case
                  (progn
                    (run-shell sandboxed-context
                               "command" "pwd -P"
                               "directory" "/")
                    nil)
                (tool-error (condition)
                  (search "outside the allowed workspace roots"
                          (princ-to-string condition))))
              "sandboxed shell execution still rejects a directory outside workspace roots")
             (let* ((*tool-execution-blocking-grace-seconds* 2)
                    (before                                  (execution-count))
                    (authorizations-before                   authorization-count)
                    (result
                      (run-shell context
                                 "command" (test-fixture-shell-command
                                            *platform* "printf fast-shell"
                                            "[Console]::Write('fast-shell')"))))
               (test-assert
                (and (tool-result-success-p result)
                     (eq (tool-result-category result) ':success)
                     (= 0 (getf (tool-result-details result) :exit-code))
                     (search "exit 0" (tool-result-content result))
                     (search "fast-shell" (tool-result-content result))
                     (= (execution-count) (1+ before))
                     (= authorization-count (1+ authorizations-before)))
                "a fast default shell job returns its ordinary result"))
             (let* ((*tool-execution-blocking-grace-seconds* 0.01)
                    (before                                  (execution-count))
                    (result
                      (run-shell
                       context
                       "command"
                       (format nil
                               (test-fixture-shell-command
                                *platform* "sleep 1; printf once >> ~A"
                                "Start-Sleep 1; [IO.File]::AppendAllText(~A,'once')")
                               (test-fixture-shell-quote *platform* (namestring slow-path)))))
                    (details                                 (tool-result-details result))
                    (job                                     (handoff-job result))
                    (identifier                              (and job (session-job-identifier job))))
               (test-assert
                (and (typep result 'task-tool-result)
                     (tool-result-success-p result)
                     (eq (getf (rest details) :handoff-reason)
                         :grace-expired)
                     job
                     (session-job-detached-p job)
                     (= (execution-count) (1+ before)))
                "a slow default shell call hands off its existing execution job")
               (let* ((activity (session-job-live-activity job))
                      (summary  (tool-execution-job-summary job)))
                 (test-assert
                  (and (null (tool-execution-job-description job))
                       (string= (getf activity :description) summary)
                       (<= (length summary) *tool-execution-summary-limit*)
                       (search "sleep 1" (string-downcase summary)))
                  "an omitted description uses the retained bounded job summary"))
              (multiple-value-bind (snapshot terminal-p) (session-job-await job 2)
                (test-assert
                 (and terminal-p
                      (string= (getf snapshot :job-id) identifier)
                      (null (getf snapshot :description))
                      (eq (getf snapshot :state) ':completed)
                      (string= (uiop:read-file-string slow-path) "once")
                      (= (execution-count) (1+ before)))
                 "the handed-off shell execution completes exactly once")))
             (let* ((result
                      (run-shell
                       context
                       "command"
                       (format nil
                               (test-fixture-shell-command
                                *platform* "printf started > ~A; sleep 1; printf finished > ~A"
                                "[IO.File]::WriteAllText(~A,'started'); Start-Sleep 1; [IO.File]::WriteAllText(~A,'finished')")
                               (test-fixture-shell-quote *platform* (namestring started-path))
                               (test-fixture-shell-quote *platform* (namestring finished-path)))
                       "async" t))
                    (details    (tool-result-details result))
                    (job        (handoff-job result))
                    (identifier (and job (session-job-identifier job))))
               (test-assert
                (and (typep result 'task-tool-result)
                     (eq (getf (rest details) :handoff-reason) :requested)
                     job
                     (task-tests--wait-until
                      (lambda () (probe-file started-path)) 2))
                "explicit async returns an inspectable shell job while it runs")
              (let* ((get-result (tool-execute (tool-registry-find registry "job" "get")
                                               context (json-object "id" identifier)))
                     (record     (getf (rest (tool-result-details get-result)) :job)))
                (test-assert
                 (and (tool-result-success-p get-result)
                      (string= (getf record :id) identifier)
                      (eq (getf record :state) ':running))
                 "job.get inspects the running shell execution"))
              (let* ((cancel-result
                       (tool-execute
                        (tool-registry-find registry "job" "cancel")
                        context
                        (json-object "id" identifier)))
                     (cancel-details
                       (rest (tool-result-details cancel-result))))
                (test-assert
                 (and (tool-result-success-p cancel-result)
                      (getf cancel-details :accepted-p))
                 "job.cancel accepts cancellation of a running shell execution"))
              (multiple-value-bind (snapshot terminal-p)
                  (session-job-await job 2)
                (test-assert
                 (and terminal-p
                      (eq (getf snapshot :state) :aborted)
                      (eq (getf (getf snapshot :result) :status) :aborted))
                 "shell cancellation publishes an aborted terminal job"))
              (sleep 1.2)
              (test-assert
               (not (probe-file finished-path))
               "shell cancellation prevents the external command from continuing"))
             (let* ((result
                      (let ((*shell-maximum-output-characters* 5))
                        (run-shell context
                                   "command" (test-fixture-shell-command
                                              *platform* "printf 123456789"
                                              "[Console]::Write('123456789')")
                                   "async" t)))
                    (job (handoff-job result)))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (let* ((record  (getf snapshot :result))
                        (content (getf record :content))
                        (capture (first (getf (first (getf record :shell-logs)) :captures))))
                   (test-assert
                    (and terminal-p
                         (search "6789" content)
                         (not (search "123456789" content))
                         (= 9 (getf capture :byte-count))
                         (getf capture :complete-p))
                    "An asynchronous shell job bounds its preview and retains complete raw output")))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root :validate t
                                             :if-does-not-exist   ':ignore))))
  nil)

(-> test-tool-execution-overflow-binding () null)
(defun test-tool-execution-overflow-binding ()
  "Test lossless result spilling inside execution-pool worker threads."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (orchestrator  (tool-registry-runtime-binding registry 'task-orchestrator))
         (primary
           (task-tests--primary-agent
            configuration "execution-overflow-primary" registry)))
    (unwind-protect
         (let* ((*tool-result-overflow-function*
                  (lambda (text)
                    (declare (ignore text))
                    "context:worker-overflow"))
                (result
                  (tool-execution-invoke
                   orchestrator primary
                   :tool-name "test.overflow"
                   :summary   "Return one oversized worker result."
                   :operation-function
                   (lambda ()
                     (tool-success (make-string 9000 :initial-element #\x)))
                   :async-p   nil)))
           (test-assert
            (and (tool-result-success-p result)
                 (search "context:worker-overflow"
                         (tool-result-content result)))
            "execution workers inherit the complete-result spill function"))
      (ignore-errors (task-orchestrator-close orchestrator))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist   ':ignore)))
  nil)

(-> test-tool-execution-retention () null)
(defun test-tool-execution-retention ()
  "Test bounded execution retention and detachment."
  (let* ((*tool-execution-terminal-retention-limit* 2)
         (configuration                             (test-configuration))
         (root                                      (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (orchestrator                              (tool-registry-runtime-binding registry 'task-orchestrator))
         (primary
           (task-tests--primary-agent
            configuration "execution-retention-primary" registry))
         (identifiers                               nil)
         (closed-p                                  nil))
    (unwind-protect
         (progn
           (dotimes (index 3)
             (let ((job
                     (task-orchestrator-start-execution-job
                      orchestrator primary
                      :tool-name  "lisp.eval"
                      :summary    (format nil "retained execution ~D" index)
                      :operation-function
                      (lambda ()
                        (tool-success (format nil "result ~D" index)))
                      :detached-p t)))
               (push (session-job-identifier job) identifiers)
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 2)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :completed)
                       (null (tool-execution-job-operation-function job)))
                  "each retained execution completes and releases its closure"))))
           (setf identifiers (nreverse identifiers))
           (let ((retained
                   (job-pool-list-jobs
                    (task-orchestrator-execution-pool orchestrator))))
             (test-assert
              (and (= (length retained) 2)
                   (equal (mapcar #'session-job-identifier retained)
                          (rest identifiers))
                   (null
                    (task-orchestrator--find-job
                     orchestrator (first identifiers)))
                   (zerop (task-orchestrator-session-live-count orchestrator)))
              "the execution pool retains exactly its newest terminal records"))
           (setf closed-p (task-orchestrator-close orchestrator))
           (test-assert closed-p "the execution pool closes completely")
           (task-orchestrator-detach orchestrator)
           (test-assert
            (null (job-pool-list-jobs (task-orchestrator-execution-pool orchestrator)))
            "detachment removes closed execution jobs"))
      (unless closed-p
        (ignore-errors (task-orchestrator-close orchestrator)))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t
                                           :if-does-not-exist   ':ignore)))
  nil)
