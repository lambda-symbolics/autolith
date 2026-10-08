(in-package #:autolith)

;;;; -- Shell Log Job Continuity --

(-> shell-log-job-tests--fixture (function) t)
(defun shell-log-job-tests--fixture (function)
  "Call FUNCTION with a private shell context and supervised session runtime."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (task-augment-tool-registry
                      (make-default-tool-registry :configuration configuration)))
           (parent (agent-create :configuration configuration
                                 :provider (make-instance 'model-provider)
                                 :tool-registry registry :worker nil))
           (orchestrator (task-run-tool-orchestrator
                          (tool-registry-find registry "task" "run")))
           (context (make-instance 'tool-context
                                   :configuration configuration
                                   :agent parent :registry registry :worker nil
                                   :conversation (agent-conversation parent)
                                   :call-id "shell-log-job-test"
                                   :command-authorization-function
                                   (lambda (command directory)
                                     (declare (ignore command directory))
                                     ':full-access))))
      (unwind-protect
           (funcall function configuration parent orchestrator context registry)
        (task-orchestrator-close orchestrator)))))

(-> test-shell-log-job-results () null)
(defun test-shell-log-job-results ()
  "Exercise sync and detached shell results through durable job metadata."
  (shell-log-job-tests--fixture
   (lambda (configuration parent orchestrator context registry)
     (let ((tool (tool-registry-find registry "shell" "run")))
       (dolist (async-p '(nil t))
         (let* ((result
                  (tool-execute
                   tool context
                   (json-object
                    "command" (test-fixture-shell-command
                               *platform* "printf shell-log-result"
                               "[Console]::Write('shell-log-result')")
                   "async" (if async-p t (json-false)))))
                (job
                  (if async-p
                      (task-orchestrator-find-visible-job
                       orchestrator
                       (getf (getf (rest (tool-result-details result)) :job) :id)
                       parent "job.get")
                      (first (last (task-orchestrator-list-visible-jobs
                                    orchestrator parent))))))
           (test-assert job "shell invocation has a supervised execution identity")
           (when async-p
             (multiple-value-bind (snapshot terminal-p) (session-job-await job 10)
               (declare (ignore snapshot))
               (test-assert terminal-p "detached shell invocation finishes")))
           (let* ((record (job-result job))
                  (logs (getf record :shell-logs))
                  (rebuilt (tool-execution-job-result->tool-result job))
                  (native (session-job-native-record
                           job (session-job-snapshot job) parent))
                  (directory (merge-pathnames
                              (format nil "~A/" (session-job-execution-identifier job))
                              (task--artifact-group-root
                               configuration (session-job-root-conversation-identifier job))))
                  (persisted (task-continuity--read
                              (merge-pathnames "terminal.sexp" directory))))
             (test-assert (and logs (getf (first logs) :captures))
                          "shell logs survive terminal job publication")
             (test-assert (equal (getf record :details) (tool-result-details rebuilt))
                          "terminal tool reconstruction preserves structured details")
             (test-assert (equal logs (getf native :shell-logs))
                          "job inspection exposes stable capture metadata")
             (test-assert (equal logs (getf (getf persisted :result) :shell-logs))
                          "durable terminal metadata retains shell logs")
            (when async-p
              (let* ((unfinished (task-unfinished-work-snapshot parent))
                     (row (find (session-job-execution-identifier job)
                                (getf unfinished :jobs)
                                :key (lambda (item) (getf item :execution-id))
                                :test #'equal)))
                (test-assert (and row (getf row :artifact-path))
                             "compaction evidence retains the terminal metadata reference")
                (test-assert
                 (equal logs
                        (getf (getf (task-continuity--read (pathname (getf row :artifact-path)))
                                    :result) :shell-logs))
                 "the compaction reference reopens captures without replay")))
             (let ((fresh (task-orchestrator-create)))
               (unwind-protect
                    (let* ((entry (task-continuity--find
                                   parent (session-job-execution-identifier job)))
                           (classified (task-continuity-classify entry parent fresh))
                           (references (getf (getf classified :result)
                                             :shell-log-references)))
                      (test-assert references
                                   "a fresh runtime reopens stable log references")
                      (test-assert
                       (equal references
                              (getf (task--shell-log-reference-summary logs)
                                    :shell-log-references))
                       "reopening preserves the original capture identities"))
                 (task-orchestrator-close fresh)))))))))
  nil)

(-> test-shell-log-job-cancellation () null)
(defun test-shell-log-job-cancellation ()
  "Retain allocated log references when cancellation prevents a normal tool result."
  (shell-log-job-tests--fixture
   (lambda (configuration parent orchestrator context registry)
     (let* ((result
              (tool-execute
               (tool-registry-find registry "shell" "run") context
               (json-object
                "command" (test-fixture-shell-command
                           *platform* "printf started; sleep 30"
                           "[Console]::Write('started'); Start-Sleep -Seconds 30")
                "async" t)))
            (job (task-orchestrator-find-visible-job
                  orchestrator
                  (getf (getf (rest (tool-result-details result)) :job) :id)
                  parent "job.get")))
       (test-assert
        (task-tests--wait-until
         (lambda ()
           (getf (first (getf (shell-log-job-metadata configuration job) :shell-logs))
                 :captures)) 10)
        "capture identity is durable before operation completion")
        (test-assert (not (job-interrupt-on-cancellation-p job))
                     "Retained shell execution uses cooperative cancellation")
       (job-cancel job :reason ':test-cancel)
       (multiple-value-bind (snapshot terminal-p) (session-job-await job 10)
         (test-assert terminal-p "cancelled execution terminates")
         (test-assert (eq (getf snapshot :state) ':aborted)
                      "cancellation retains the aborted job outcome")
         (let* ((record (getf snapshot :result))
                (logs (getf record :shell-logs)))
           (test-assert logs "cancellation retains allocated capture metadata")
            (test-assert
             (every (lambda (capture)
                      (and (eq (getf capture :status) ':cancelled)
                           (not (getf capture :complete-p))))
                    (getf (first logs) :captures))
             "Cancelled captures report an incomplete cancelled stream")
           (test-assert
            (equal logs (getf (tool-result-details
                              (tool-execution-job-result->tool-result job)) :shell-logs))
            "the failed tool result retains references without an operation result")
           (let* ((entry (task-continuity--find parent (session-job-execution-identifier job)))
                  (directory (uiop:pathname-directory-pathname (getf entry :path)))
                  (metadata (task-completion--metadata
                             (getf entry :record) :directory directory :result record
                             :state ':aborted)))
             (test-assert (getf metadata :shell-log-references)
                          "completion evidence retains cancelled capture references")))))))
  nil)

(-> test-shell-log-reference-summary () null)
(defun test-shell-log-reference-summary ()
  "Bound completion reference evidence without including capture bodies."
  (let* ((logs (loop for index below 8
                     collect (list :created-at index :captures
                                   (list (list :reference
                                               (format nil "shell-log:root/job/~D/output" index)
                                               :byte-count 1000000)))))
         (summary (task--shell-log-reference-summary logs)))
    (test-assert (= 4 (length (getf summary :shell-log-references)))
                 "completion evidence retains a bounded latest reference set")
    (test-assert (= 4 (getf summary :shell-log-omitted-count))
                 "completion evidence counts omitted references")
    (test-assert
     (equal (getf summary :shell-log-references)
            (loop for index downfrom 7 to 4
                  collect (format nil "shell-log:root/job/~D/output" index)))
     "the newest captures are inspectable from completion evidence"))
  nil)


(-> test-shell-log-durable-tool-details () null)
(defun test-shell-log-durable-tool-details ()
  "Append and reopen portable shell references through the durable replay boundary."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration))
           (details (list :exit-code 7 :execution-status ':exited
                          :shell-logs
                          (list (list :captures
                                      (list (list :stream ':output
                                                  :reference "shell-log:root/execution/artifact/output"
                                                  :byte-count 1000000
                                                  :complete-p t)))))))
      (conversation-append-tool-result
       conversation "shell-log-call" :tool-name "shell.run" :output "Bounded preview"
       :success-p t :details details)
      (let* ((loaded (conversation-replay-load
                      configuration (conversation-identifier conversation)))
             (session (conversation-replay-create loaded))
             (record (conversation-replay-record-record
                      (aref (conversation-replay-session-records session) 0))))
        (test-assert (equal details (getf (rest record) :details))
                     "durable replay preserves structured shell capture references"))
      (dolist (details (list (make-hash-table)
                            (list :oversized (make-string
                                             (1+ *conversation-tool-details-maximum-octets*)
                                             :initial-element #\x))))
        (conversation-append-tool-result
         conversation "omitted-details-call" :tool-name "test.details" :output "preview"
         :success-p t :details details))
      (let* ((loaded (conversation-replay-load
                      configuration (conversation-identifier conversation)))
             (records (conversation-replay-session-records
                       (conversation-replay-create loaded))))
        (loop for index from 1 below (length records)
              for record = (conversation-replay-record-record (aref records index))
              do (test-assert (getf (getf (rest record) :details) :omitted-p)
                              "unportable or excessive details produce durable diagnostics")))))
  nil)


(-> test-shell-log-publication-failures () null)
(defun test-shell-log-publication-failures ()
  "Keep final evidence after publication failure and roll back failed allocations."
  (shell-log-job-tests--fixture
   (lambda (configuration parent orchestrator context registry)
     (let* ((*shell-log-active* (make-hash-table :test #'equal))
            (writer (symbol-function 'shell-log--write-manifest))
            (tool (tool-registry-find registry "shell" "run"))
            (result nil))
       (unwind-protect
            (progn
              (setf (symbol-function 'shell-log--write-manifest)
                    (lambda (artifact)
                      (if (eq (getf (shell-log-artifact-metadata artifact) :state) ':closed)
                          (shell-log--fail "Injected final manifest publication failure.")
                          (funcall writer artifact))))
              (setf result
                    (tool-execute
                     tool context
                     (json-object "command"
                                  (test-fixture-shell-command
                                   *platform* "printf retained-after-failure"
                                   "[Console]::Write('retained-after-failure')")))))
         (setf (symbol-function 'shell-log--write-manifest) writer))
       (let* ((job (first (last (task-orchestrator-list-visible-jobs orchestrator parent))))
              (details (tool-result-details result))
              (logs (getf details :shell-logs))
              (capture (first (getf (first logs) :captures)))
              (reference (getf capture :reference))
              (resource (resource-resolver-resolve
                         (make-instance 'shell-log-resolver :scheme "shell-log")
                         (subseq reference 10) context))
              (directory (shell-log-resource-directory resource))
              (disk (shell-log--read-manifest configuration directory))
              (terminal (task-continuity--read
                         (merge-pathnames "../terminal.sexp"
                                          (uiop:pathname-parent-directory-pathname directory)))))
         (test-assert (and (not (tool-result-success-p result))
                           (getf details :capture-diagnostic)
                           (eq ':closed (getf (first logs) :state))
                           (getf capture :complete-p))
                      "The result reports metadata failure alongside truthful final capture evidence")
         (test-assert (and (zerop (hash-table-count *shell-log-active*))
                           (eq ':interrupted (getf disk :state))
                           (= 22 (getf (first (getf disk :captures)) :byte-count))
                           (not (getf (first (getf disk :captures)) :observed-byte-count-known-p)))
                      "Restart inspection measures retained bytes without inventing observed totals")
         (test-assert (and (equal logs (getf (getf terminal :result) :shell-logs))
                           (equal logs (getf (session-job-native-record
                                            job (session-job-snapshot job) parent) :shell-logs)))
                      "Terminal storage and inspection prefer the final result over the stale manifest")
         (test-assert (search "retained-after-failure"
                              (tool-result-content
                               (resource-tool-read resource
                                                   (tool-registry-find registry "resource" "read")
                                                   context (json-object))))
                      "A failed metadata publication does not lose readable raw output")
         (conversation-append-tool-result
          (agent-conversation parent) (tool-context-call-id context)
          :tool-name "shell.run" :output (tool-result-content result)
          :success-p nil :details details)
         (test-assert (shell-log--prunable-p configuration directory disk)
                      "Durable final tool evidence acknowledges an interrupted stale manifest"))
       (let ((directory nil))
         (unwind-protect
              (progn
                (setf (symbol-function 'shell-log--write-manifest)
                      (lambda (artifact)
                        (setf directory (shell-log-capture-directory artifact))
                        (funcall writer artifact)
                        (shell-log--fail "Injected failure after initial publication.")))
                (test-assert (handler-case (progn (shell-log-allocate context) nil)
                               (shell-log-error () t))
                             "An initial publication failure refuses launch"))
           (setf (symbol-function 'shell-log--write-manifest) writer))
         (test-assert (and directory (not (probe-file directory))
                           (zerop (hash-table-count *shell-log-active*)))
                      "Failed allocation removes its partial manifest, directory and reservation")))))
  nil)


(-> test-shell-log-command-gates () null)
(defun test-shell-log-command-gates ()
  "Persist command-gate capture metadata and acknowledge successful or failed evidence."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 32)
           (*shell-log-retention-count-limit* 1)
           (configuration (configuration-copy configuration :working-directory root))
           (application (mission-test--application configuration))
           (registry (task-augment-tool-registry (application-tool-registry application)))
           (orchestrator (task-run-tool-orchestrator
                          (tool-registry-find registry "task" "run"))))
      (unwind-protect
           (dolist (exit-code '(0 7))
             (application-mission-start
              application
              (mission-test--specification
               :gates (list (list :id "retained-gate" :kind ':command
                                  :command (test-fixture-shell-command
                                            *platform*
                                            (format nil "printf gate-output; exit ~D" exit-code)
                                            (format nil "[Console]::Write('gate-output'); exit ~D" exit-code))))))
             (let* ((mission (mission-context-find (application-conversation application)))
                    (gate (first (getf (application-goal application) :gates)))
                    (context (mission-test--context application)))
               (test-assert (eq (zerop exit-code) (mission--check-gate mission gate context))
                            "Command-gate acceptance follows its actual exit status")
               (let* ((logs (getf (getf gate :tool-details) :shell-logs))
                      (reference (getf (first (getf (first logs) :captures)) :reference))
                      (resource (resource-resolver-resolve
                                 (make-instance 'shell-log-resolver :scheme "shell-log")
                                 (subseq reference 10) context))
                      (directory (shell-log-resource-directory resource))
                      (metadata (shell-log--read-manifest configuration directory)))
                 (test-assert (search "gate-output"
                                      (tool-result-content
                                       (resource-tool-read resource
                                                           (tool-registry-find registry "resource" "read")
                                                           context (json-object))))
                              "A command mission gate retains authorized raw output")
                 (test-assert (and (getf metadata :job-id)
                                   (null (getf metadata :parent-call-id))
                                   (shell-log--sync-delivered-p configuration metadata))
                              "Durable mission evidence acknowledges a gate outside provider tool calls"))))
        (task-orchestrator-close orchestrator))))
  nil)
