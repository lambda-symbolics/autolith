(in-package #:autolith)

;;;; -- Tool Outcomes and Worker Propagation --

(defclass telemetry-test-correction-tool (agent-test-echo-tool) ()
  (:documentation "Check that correction metadata does not reach the underlying tool."))

(defmethod tool-execute :before ((tool telemetry-test-correction-tool)
                                 (context tool-context) (arguments hash-table))
  "Assert that the registry stripped correction metadata from ARGUMENTS."
  (declare (ignore tool context))
  (test-assert (not (nth-value 1 (gethash "corrects_previous" arguments)))
               "The registry consumes correction metadata before invoking the tool."))

(-> test-telemetry-explicit-correction-hook () null)
(defun test-telemetry-explicit-correction-hook ()
  "An explicit correction reaches telemetry without changing arguments or leaking between calls."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry     (make-instance 'tool-registry))
            (tool         (default-tools--register
                           registry (list 'telemetry-test-correction-tool "test" "echo" "Fixture echo."
                                          (tool-object-schema (json-object "value" (tool-string-property "Value.")) '("value")))))
            (conversation (conversation-create configuration :identifier "telemetry-correction"))
            (provider     (make-instance 'scripted-provider
                                         :results
                                         (list (agent-test-result
                                                "correction-calls"
                                                (list (agent-test-call :call-id "first" :arguments "{\"value\":\"same\"}")
                                                      (agent-test-call :call-id "second" :arguments "{\"value\":\"same\",\"corrects_previous\":true}")
                                                      (agent-test-call :call-id "third" :arguments "{\"value\":\"different\"}")))
                                               (agent-test-result "done" (list (agent-test-message "done"))))))
            (agent        (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry registry :worker :unused))
            (bodies       (test-telemetry-hooks--capture configuration
                                                         (lambda () (agent-run-user-turn agent "Fixture correction."))))
            (spans        (test-telemetry-hooks--kind (first bodies) "tool")))
       (test-assert (equal (json-get (json-get (json-get (tool-parameters tool) "properties") "corrects_previous") "type") "boolean")
                    "Builtin registration advertises the annotation as a boolean.")
       (test-assert (equal (mapcar (lambda (span) (json-true-p (test-telemetry--attribute span "autolith.tool.corrects_previous"))) spans)
                           '(nil t nil)) "The explicit correction is recorded on exactly its own call.")
       (test-assert (json-true-p (test-telemetry--attribute (second spans) "autolith.tool.same_as_previous"))
                    "Control metadata does not change local argument equality.")
       (test-assert (= (length (agent-test-tool-outputs conversation)) 3) "All calls preserve their normal results.")
       (test-assert (null *telemetry-tool-arguments*) "Decoded arguments do not escape their execution binding."))))
  nil)

(-> test-telemetry-shell-exit-hook () null)
(defun test-telemetry-shell-exit-hook ()
  "Export actual shell failures and exit status through the agent completion path."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry     (make-default-tool-registry))
            (conversation (conversation-create configuration))
            (provider     (make-instance 'scripted-provider
                                        :results
                                        (list (agent-test-result
                                               "shell-exit"
                                               (list (agent-test-call
                                                      :call-id "shell" :namespace "shell" :name "run"
                                                      :arguments (json-encode
                                                                  (json-object "command"
                                                                               (test-fixture-shell-command
                                                                                *platform* "exit 7" "exit 7"))))))
                                              (agent-test-result "done" (list (agent-test-message "done"))))))
            (agent        (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry registry :worker :unused))
            (observer     (callback-agent-observer-create
                           :command-authorization-callback
                           (lambda (command directory) (declare (ignore command directory)) ':full-access)))
            (bodies       (test-telemetry-hooks--capture
                           configuration (lambda () (agent-run-user-turn agent "exit" :observer observer))))
            (span         (first (test-telemetry-hooks--kind (first bodies) "tool"))))
       (test-telemetry-hooks--assert-attributes
        span
        '((failed-command "autolith.tool.outcome" "error"
           "Nonzero exit is exported as failure.")
          (exit-status "autolith.tool.exit_code" "7"
           "The exit status reaches export.")
          (structured-code "autolith.tool.error_code" "nonzero-exit"
           "The structured failure code reaches export."))))))
  nil)

(-> test-telemetry-tool-failure-categories () null)
(defun test-telemetry-tool-failure-categories ()
  "Export real worker, missing-resource and yield failures through agent completion."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry                                (task-augment-tool-registry (make-default-tool-registry)))
            (orchestrator                            (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
            (*tool-execution-blocking-grace-seconds* 90)
            (worker                                  (lisp-worker-pool-create configuration))
            (cases                                   '(("lisp" "eval" "{\"forms\":[\"(/ 1 0)\"]}" "division-by-zero")
                                                       ("lisp" "eval" "{\"forms\":[\"(error \\\"boom-custom\\\")\"]}" "simple-error")
                                                       ("lisp" "eval" "{\"forms\":[\"(+ 1 \\\"two\\\")\"]}" "type-error")
                                                       ("resource" "read" "{\"uri\":\"workspace:private-missing-file.txt\"}" "not-found")
                                                       ("yield" "submit" "{\"status\":\"success\"}" "invalid-yield")))
            (provider                                (make-instance
                                                      'scripted-provider :configuration configuration
                                                      :results                          (append
                                                                                         (loop for (namespace name arguments nil) in cases
                                                                                               for index from 1
                                                                                               collect (agent-test-result
                                                                                                        (format nil "failure-~D" index)
                                                                                                        (list (agent-test-call
                                                                                                               :call-id (format nil "call-~D" index)
                                                                                                               :namespace namespace :name name :arguments arguments))))
                                                                                         (list (agent-test-result "done" (list (agent-test-message "done")))))))
            (agent                                   (agent-create :configuration configuration :provider provider
                                                                   :conversation (conversation-create configuration)
                                                                   :tool-registry registry :worker worker)))
       (tool-registry-register
        registry (make-instance 'task-yield-tool :namespace "yield" :name "submit"
                                :description "Test terminal yield."
                                :parameters  (tool-object-schema
                                              (json-object "status" (tool-string-property "Yield status."))
                                              '("status"))))
       (unwind-protect
            (let* ((bodies (test-telemetry-hooks--capture
                            configuration (lambda () (agent-run-user-turn agent "Exercise failures."))))
                   (tools  (test-telemetry-hooks--kind (first bodies) "tool")))
              (test-assert (= (length tools) (length cases)) "Every attempted tool is exported.")
              (loop for span in tools
                    for case in cases
                    do (test-assert
                        (and (equal (test-telemetry--attribute span "autolith.tool.outcome") "error")
                             (equal (test-telemetry--attribute span "autolith.tool.error_code") (fourth case))
                             (equal (test-telemetry--attribute span "error.type") "tool"))
                        "Tool failures export fixed codes and categories.")
                       (test-assert (not (test-telemetry--attribute span "autolith.issue.kind"))
                                    "Ordinary failures do not acquire issue context."))
              (test-assert
               (null (test-telemetry--attribute
                      (first (test-telemetry-hooks--kind (first bodies) "run")) "error.type"))
               "A recovered tool failure does not mark the successful root as failed.")
              (dolist (private '("boom-custom" "two" "private-missing-file"))
                (test-assert (not (search private (first bodies))) "Private tool data stays off the wire."))
              (test-telemetry--collector-fixture "native-tool-failures.json" (json-decode (first bodies))))
         (task-orchestrator-close orchestrator)
         (lisp-worker-pool-stop-all worker)))))
  nil)

(-> test-telemetry-outside-resource-failures () null)
(defun test-telemetry-outside-resource-failures ()
  "Export missing absolute resource failures after real out-of-root authorization."
  (test-telemetry--call
   (lambda (configuration)
     (with-test-fixture (':posix-adapter "absolute POSIX resource failure paths")
       (let* ((configuration (configuration-copy configuration :working-directory #P"/tmp/"))
              (paths         '("/definitely/missing/e2e-file.txt" "/tmp/e2e-no-such-file-123.txt"))
              (registry      (make-default-tool-registry))
              (provider      (make-instance
                              'scripted-provider :configuration configuration
                              :results                          (append
                                                                 (loop for path in paths for index from 1
                                                                       collect (agent-test-result
                                                                                (format nil "missing-~D" index)
                                                                                (list (agent-test-call
                                                                                       :call-id   (format nil "missing-~D" index)
                                                                                       :namespace "resource" :name "read"
                                                                                       :arguments (json-encode (json-object "uri" path))))))
                                                                 (list (agent-test-result "done" (list (agent-test-message "done")))))))
              (agent         (agent-create :configuration configuration :provider provider
                                           :conversation (conversation-create configuration)
                                           :tool-registry registry :worker :unused))
              (authorized    nil)
              (observer      (callback-agent-observer-create
                              :command-authorization-callback
                              (lambda (command directory)
                                (declare (ignore directory))
                                (push command authorized)
                                ':full-access)))
              (bodies        (test-telemetry-hooks--capture
                              configuration (lambda () (agent-run-user-turn agent "Read missing." :observer observer))))
              (tools         (test-telemetry-hooks--kind (first bodies) "tool")))
         (test-assert authorized "The absolute path exercises out-of-root authorization.")
         (test-assert (= (length tools) 2) "Both resource attempts are exported.")
         (dolist (span tools)
           (test-assert (equal (test-telemetry--attribute span "autolith.tool.outcome") "error")
                        "The missing path fails.")
           (test-assert (equal (test-telemetry--attribute span "autolith.tool.error_code") "not-found")
                        "The missing absolute path exports not-found."))))))
  nil)

(defclass telemetry-test-failure-tool (tool)
  ((code :initarg :code :reader telemetry-test-failure-code
         :documentation "The keyword failure code emitted by this fixture."))
  (:documentation "Emit an extension-supplied failure code through real tool dispatch."))

(defmethod tool-execute ((tool telemetry-test-failure-tool) (context tool-context) (arguments hash-table))
  "Return a failed tool result with the fixture's arbitrary keyword code."
  (declare (ignore context arguments))
  (tool-failure "Private extension failure." :code (telemetry-test-failure-code tool)))

(-> test-telemetry-tool-error-code-boundary () null)
(defun test-telemetry-tool-error-code-boundary ()
  "Bound extension keyword codes at the real agent-to-export boundary."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((cases    (append '((:division-by-zero "division-by-zero")
                                (:permission-denied "permission-denied")
                                (:access-denied "access-denied")
                                (:authorization "authorization")
                                (:not-authorized "not-authorized")
                                (:capability-denied "capability-denied")
                                (:authority "authority")
                                (:os-permission-denied "os-permission-denied")
                                (:|custom-code9| "custom-code9")
                                (:|| "tool-failure")
                                (:|private.name| "tool-failure")
                                (:|private/path| "tool-failure")
                                (:|private name| "tool-failure")
                                (:|échec| "tool-failure"))
                             (list (list (intern (make-string 64 :initial-element #\A) :keyword)
                                         (make-string 64 :initial-element #\a))
                                   (list (intern (make-string 65 :initial-element #\A) :keyword) "tool-failure"))))
            (registry (make-default-tool-registry))
            (provider (make-instance
                       'scripted-provider :configuration configuration
                       :results                          (append
                                                          (loop for index from 1 to (length cases)
                                                                collect (agent-test-result
                                                                         (format nil "error-~D" index)
                                                                         (list (agent-test-call :call-id   (format nil "error-~D" index)
                                                                                                :namespace "fixture"
                                                                                                :name      (format nil "failure~D" index)
                                                                                                :arguments "{}"))))
                                                          (list (agent-test-result "done" (list (agent-test-message "done")))))))
            (agent    (agent-create :configuration configuration :provider provider
                                    :conversation (conversation-create configuration)
                                    :tool-registry registry :worker :unused)))
       (loop for (code nil) in cases for index from 1
             do (tool-registry-register
                 registry (make-instance 'telemetry-test-failure-tool :code code
                                         :namespace "fixture" :name (format nil "failure~D" index)
                                         :description                       "Emit a fixture failure."
                                         :parameters                        (tool-object-schema (json-object) nil))))
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration (lambda () (agent-run-user-turn agent "Exercise error codes."))))
              (tools  (test-telemetry-hooks--kind (first bodies) "tool")))
         (test-assert (= (length tools) (length cases)) "Every extension tool is executed and exported.")
         (loop for span in tools for (code expected) in cases
               do (test-assert (equal (test-telemetry--attribute span "autolith.tool.error_code") expected)
                               "Only bounded lowercase ASCII codes reach the wire.")
                  (test-assert
                   (equal (test-telemetry--attribute span "autolith.tool.outcome")
                          (if (member code '(:permission-denied :access-denied :authorization
                                             :not-authorized :capability-denied :authority))
                              "permission_denied" "error"))
                   "Authority codes are denials; OS and ordinary failures remain errors.")
                  (test-assert
                   (equal (test-telemetry--attribute span "error.type")
                          (if (member code '(:permission-denied :access-denied :authorization
                                             :not-authorized :capability-denied :authority :os-permission-denied))
                              "permission" "tool"))
                   "Both permission failure families retain the permission error category."))))))
  nil)

(defclass telemetry-test-concurrent-tool (agent-test-concurrent-tool)
  ((runs
    :initform      nil
    :accessor      telemetry-test-concurrent-runs
    :documentation "Telemetry runs observed by each parallel invocation, newest first."))
  (:documentation "Record ambient telemetry identity inside a real parallel tool worker."))

(defmethod tool-execute :before ((tool telemetry-test-concurrent-tool)
                                 (context tool-context) (arguments hash-table))
  "Record the telemetry run that is ambient inside the parallel tool worker."
  (declare (ignore context arguments))
  (with-lock-held ((agent-test-concurrency-state-lock (agent-test-concurrent-tool-state tool)))
    (push *telemetry-run* (telemetry-test-concurrent-runs tool))))

(-> test-telemetry-parallel-tool-hooks () null)
(defun test-telemetry-parallel-tool-hooks ()
  "Parallel workers share one run and export in wire order, independently of completion order."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-parallel"))
            (state        (make-instance 'agent-test-concurrency-state))
            (tool         (make-instance 'telemetry-test-concurrent-tool
                                         :namespace "concurrency" :name "run" :description "Parallel fixture."
                                         :parameters (tool-parameters (agent-test-concurrency-tool state "run"))
                                         :state state :execution-policy :parallel))
            (registry     (agent-test-concurrency-registry (list tool)))
            (provider     (make-instance 'scripted-provider :results
                                        (list (agent-test-result
                                               "parallel-calls"
                                               (list (agent-test-call
                                                      :call-id "first" :namespace "concurrency" :name "run"
                                                      :arguments "{\"label\":\"first\",\"await_peer\":true,\"delay\":0.05,\"fail\":true}")
                                                     (agent-test-call
                                                      :call-id "second" :namespace "concurrency" :name "run"
                                                      :arguments "{\"label\":\"second\",\"await_peer\":true}")))
                                              (agent-test-result "parallel-done" (list (agent-test-message "done"))))))
            (agent        (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry registry :worker :unused))
            (bodies       (test-telemetry-hooks--capture
                           configuration (lambda () (agent-run-user-turn agent "parallel"))))
            (spans        (test-telemetry-hooks--kind (first bodies) "tool"))
            (runs         (telemetry-test-concurrent-runs tool)))
       (test-assert (agent-test-concurrency-state-overlap-observed-p state) "The fixture executes concurrently.")
       (test-assert (and (= (length runs) 2) (first runs) (eq (first runs) (second runs)))
                    "Both real worker threads inherit the same telemetry run.")
       (test-assert (= (length bodies) 1) "One agent turn produces one root, including its providers.")
       (test-assert (= (length spans) 2) "Both parallel attempts are recorded.")
       (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.tool.sequence")) spans)
                           '("1" "2")) "Tool sequence follows provider order.")
       (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.tool.outcome")) spans)
                           '("error" "success")) "Reverse completion does not reorder outcomes.")
       (test-assert (>= (- (parse-integer (json-get (first spans) "endTimeUnixNano"))
                           (parse-integer (json-get (first spans) "startTimeUnixNano"))) 30000000)
                    "Parallel spans retain measured wall duration even without presentation timings.")
       (test-telemetry-hooks--assert-attempts (first bodies) 2))))
  nil)
