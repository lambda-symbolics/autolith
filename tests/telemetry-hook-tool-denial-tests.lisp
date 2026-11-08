(in-package #:autolith)

;;;; -- Tool Authorization and Policy Denials --

(defclass telemetry-test-authorized-tool (agent-test-echo-tool) ()
  (:documentation "Exercise explicit authorization through the real registry helper."))

(defmethod tool-execute ((tool telemetry-test-authorized-tool)
                         (context tool-context) (arguments hash-table))
  "Deny when the authorization decision is :DENY; otherwise echo as usual."
  (if (eq (tool-context-authorize-tool context tool arguments) :deny)
      (tool-failure "Denied fixture request." :code ':permission-denied)
      (call-next-method)))

(-> test-telemetry-tool-denial-hooks () null)
(defun test-telemetry-tool-denial-hooks ()
  "An explicit authorization decision is distinguishable from an ordinary failure string."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-denial"))
            (tool         (make-instance 'telemetry-test-authorized-tool
                                         :namespace "test" :name "echo" :description "Authorized fixture."
                                         :parameters (tool-object-schema
                                                      (json-object "value" (tool-string-property "Value."))
                                                      '("value"))))
            (registry     (agent-test-concurrency-registry (list tool)))
            (provider     (make-instance 'scripted-provider :results
                                        (list (agent-test-result
                                               "denied-call"
                                               (list (agent-test-call :call-id   "denied"
                                                                      :arguments "{\"value\":\"private-value\"}")))
                                              (agent-test-result "denied-done" (list (agent-test-message "done"))))))
            (agent        (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry registry :worker :unused))
            (observer     (callback-agent-observer-create
                           :tool-authorization-callback
                           (lambda (tool arguments) (declare (ignore tool arguments)) :deny)))
            (bodies       (test-telemetry-hooks--capture
                           configuration (lambda () (agent-run-user-turn agent "deny" :observer observer))))
            (spans        (test-telemetry-hooks--kind (first bodies) "tool")))
       (test-assert (= (length spans) 1) "Denied execution is one completed attempt.")
       (test-assert (equal (test-telemetry--attribute (first spans) "error.type") "permission")
                    "Authorization failures export a permission category.")
       (test-assert (equal (test-telemetry--attribute (first spans) "autolith.tool.error_code")
                           "permission-denied") "Failure codes survive actual agent execution.")
       (test-assert (equal (test-telemetry--attribute (first spans) "autolith.tool.outcome")
                           "permission_denied") "The terminal denial reaches export.")
       (test-assert (= (length (agent-test-tool-outputs conversation)) 1)
                    "Denied execution still records its provider-visible result.")
       (test-assert (not (search "private-value" (first bodies))) "Raw arguments never enter the envelope."))))
  (dolist (decision '(:deny :unavailable))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((observer
                (callback-agent-observer-create
                 :command-authorization-callback
                 (lambda (command directory)
                   (if (eq decision ':unavailable)
                       (error 'command-authorization-unavailable :command command :directory directory
                                                                :message "No interactive approval.")
                       decision))))
              (spans (test-telemetry-hooks--tool-attempts
                      configuration
                      (list (agent-test-call :call-id "shell-denial" :namespace "shell" :name "run"
                                             :arguments "{\"command\":\"printf forbidden\"}"))
                      :observer observer)))
         (test-assert (= 1 (length spans)) "The shell refusal produces one terminal result.")
         (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "tool-failure")))))
  nil)

(defclass telemetry-test-recovering-tool (agent-test-echo-tool) ()
  (:documentation "A tool that probes authority before a terminal fallback outcome."))

(defmethod tool-execute ((tool telemetry-test-recovering-tool)
                         (context tool-context) (arguments hash-table))
  "Probe real outside-path authority, then return or signal the fallback's terminal outcome."
  (test-assert
   (not (workspace-tool-authorize-outside-path
         context (merge-pathnames "outside" (test-configuration-root (tool-context-configuration context)))
         (tool-canonical-name tool)))
   "The real authority probe is refused before the fallback executes.")
  (let ((outcome (tool-argument arguments "outcome")))
    (cond
      ((equal outcome "cancelled")
       (error 'application-turn-cancelled))
      ((equal outcome "os")
       (tool-failure "Fallback failed in the OS." :code ':os-permission-denied))
      ((equal outcome "error")
       (tool-failure "Fallback arguments failed." :code ':invalid-arguments))
      (t
       (tool-success "Recovered with an allowed fallback.")))))

(-> test-telemetry-recovered-denial-hook () null)
(defun test-telemetry-recovered-denial-hook ()
  "Terminal success, ordinary failure, OS failure and cancellation win over a denied probe."
  (test-telemetry--call
   (lambda (configuration)
     (let ((registry (make-instance 'tool-registry)))
       (tool-registry-register
        registry (make-instance 'telemetry-test-recovering-tool
                                :namespace "test" :name "recover" :description "Probe and recover."
                                :parameters (tool-object-schema
                                             (json-object "outcome" (tool-string-property "Fallback outcome.")) nil)))
       (dolist (decision '(:deny :sandboxed :unavailable))
         (dolist (case '(("success" "success" nil nil)
                         ("error" "error" "invalid-arguments" "tool")
                         ("os" "error" "os-permission-denied" "permission")
                         ("cancelled" "cancelled" "unknown-outcome" "cancelled")))
           (let* ((observer
                    (callback-agent-observer-create
                     :command-authorization-callback
                     (lambda (command directory)
                       (if (eq decision ':unavailable)
                           (error 'command-authorization-unavailable :command command :directory directory
                                                                    :message "No interactive approval.")
                           decision))))
                  (conversation (conversation-create configuration))
                  (provider     (make-instance 'scripted-provider
                                              :results (list (agent-test-result
                                                              "recover-call"
                                                              (list (agent-test-call :call-id "recover" :namespace "test" :name "recover"
                                                                                     :arguments (json-encode (json-object "outcome" (first case))))))
                                                             (agent-test-result "done" (list (agent-test-message "done"))))))
                  (agent        (agent-create :configuration configuration :conversation conversation
                                              :provider provider :tool-registry registry :worker ':unused))
                  (cancelled-p  nil)
                  (bodies       (test-telemetry-hooks--capture
                                 configuration
                                 (lambda ()
                                   (handler-case (agent-run-user-turn agent "Probe then fall back." :observer observer)
                                     (application-turn-cancelled () (setf cancelled-p t))))))
                  (spans        (test-telemetry-hooks--kind (first bodies) "tool")))
             (test-assert (= 1 (length spans)) "The recovered attempt is exported.")
             (test-assert (eq cancelled-p (equal (first case) "cancelled")) "Cancellation retains its control condition.")
             (test-assert (equal (second case) (test-telemetry--attribute (first spans) "autolith.tool.outcome"))
                          "The terminal outcome wins over the earlier denial probe.")
             (test-assert (equal (third case) (test-telemetry--attribute (first spans) "autolith.tool.error_code"))
                          "The terminal result retains its own failure code.")
             (test-assert (equal (fourth case) (test-telemetry--attribute (first spans) "error.type"))
                          "Only the terminal failure determines the exported error category.")))))))
  nil)

(-> test-telemetry-mcp-policy-denial-hook () null)
(defun test-telemetry-mcp-policy-denial-hook ()
  "Execute the MCP server-policy refusal through an actual agent turn."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry (make-instance 'tool-registry))
            (manager  (test-mcp--manager configuration))
            (runtime  (first (mcp-manager-runtimes manager))))
       (unwind-protect
            (progn
              (tool-registry-register
               registry (make-instance 'mcp-provider-tool :namespace "mcp" :name "refused"
                                       :manager manager :runtime runtime
                                       :raw-tool    (first (mcp-server-runtime-tools runtime))
                                       :description "Server-denied tool."
                                       :parameters  (tool-object-schema (json-object) nil)
                                       :approval-policy ':deny :trusted-read-only-p nil :child-safe-p nil))
              (multiple-value-bind (spans outputs)
                  (test-telemetry-hooks--tool-attempts
                   configuration (list (agent-test-call :call-id "mcp-deny" :namespace "mcp" :name "refused" :arguments "{}"))
                   :registry registry)
                (test-assert (search "denied by its server policy" (first outputs)) "The real MCP refusal is invoked.")
                (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "permission-denied")))
         (mcp-manager-close manager)))))
  nil)

(-> test-telemetry-emacs-buffer-denial-hook () null)
(defun test-telemetry-emacs-buffer-denial-hook ()
  "Read a non-file buffer with sandbox-only approval through resource.read."
  (test-telemetry--call
   (lambda (configuration)
     (let ((metadata-calls 0))
       (test-call-with-function-replacements
        (list (list 'emacs-resource--call
                    (lambda (context tool-name form &rest arguments)
                      (declare (ignore context tool-name arguments))
                      (test-assert (equal form *emacs-buffer-metadata-elisp*)
                                   "Only buffer metadata is fetched before approval.")
                      (incf metadata-calls)
                      (json-object "status" "ok" "buffer" "*private-notes*"))))
        (lambda ()
          (let* ((observer (callback-agent-observer-create
                            :command-authorization-callback
                            (lambda (command directory) (declare (ignore command directory)) ':sandboxed)))
                 (spans    (test-telemetry-hooks--tool-attempts
                            configuration
                            (list (agent-test-call :call-id "buffer" :namespace "resource" :name "read"
                                                   :arguments (json-encode (json-object "uri" (emacs-buffer-uri "*private-notes*")))))
                            :observer observer)))
            (test-assert (= 1 metadata-calls) "The actual non-file buffer emitter runs.")
            (test-telemetry-hooks--assert-failure (first spans) "permission_denied" "permission-denied")))))))
  nil)

(defclass telemetry-test-managed-denial-tool (agent-test-echo-tool) ()
  (:documentation "Carry a real shell authorization refusal through managed execution."))

(defmethod tool-execute ((tool telemetry-test-managed-denial-tool)
                         (context tool-context) (arguments hash-table))
  "Run the shell refusal emitter inside an execution job with each result rendering policy."
  (declare (ignore tool))
  (let* ((kind    (tool-argument arguments "kind"))
         (shell-p (equal kind "shell"))
         (result
           (tool-execution-invoke
            (tool-context-execution-runtime context) (tool-context-agent context)
            :tool-name (if shell-p "shell.run" "test.managed-denial") :summary "Authorization fixture."
            :operation-function
            (lambda ()
              (let* ((refusal  (tool-execute (tool-registry-find (tool-context-registry context) "shell" "run")
                                             context (json-object "command" "echo private-denial-command")))
                     (content  (tool-result-content refusal))
                     (code     (tool-result-error-code refusal))
                     (denied-p (tool-result-denied-p refusal)))
                (test-assert denied-p "The real shell emitter refuses authorization inside the job.")
                (cond
                  (shell-p
                   (shell-command-result content '(:fixture t) nil :code code :denied-p denied-p))
                  ((equal kind "details")
                   (task-tool-result content '(:fixture t) :success-p nil :code code :denied-p denied-p))
                  (t
                   refusal)))))))
    (test-assert (and (tool-result-denied-p result)
                      (eq (tool-result-error-code result) ':tool-failure))
                 "The managed result retains both the explicit denial and its ordinary code.")
    (when (or shell-p (equal kind "details"))
      (test-assert (getf (tool-result-details result) :fixture) "Specialized result details survive retention."))
    result))

(-> test-telemetry-managed-denial-hook () null)
(defun test-telemetry-managed-denial-hook ()
  "Keep real shell denials through job retention, reconstruction and agent export."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry                                (task-augment-tool-registry (make-default-tool-registry)))
            (orchestrator                            (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
            (*tool-execution-blocking-grace-seconds* 90)
            (observer                                (callback-agent-observer-create
                                                      :command-authorization-callback
                                                      (lambda (command directory) (declare (ignore command directory)) ':deny))))
       (unwind-protect
            (progn
              (tool-registry-register registry
                                      (make-instance 'telemetry-test-managed-denial-tool
                                                     :namespace "test" :name "managed-denial" :description "Retain a denial."
                                                     :parameters (tool-object-schema
                                                                  (json-object "kind" (tool-string-property "Result policy."))
                                                                  '("kind"))))
              (multiple-value-bind (spans outputs body)
                  (test-telemetry-hooks--tool-attempts
                   configuration
                   (loop for kind in '("plain" "details" "shell")
                         collect (agent-test-call :call-id kind :namespace "test" :name "managed-denial"
                                                  :arguments (json-encode (json-object "kind" kind))))
                   :registry registry :observer observer)
                (test-assert (= 3 (length spans)) "Every managed result policy completes one attempt.")
                (dolist (span spans)
                  (test-telemetry-hooks--assert-failure span "permission_denied" "tool-failure"))
                (test-assert (every (lambda (output) (search "user denied" output)) outputs)
                             "Reconstructed denials reach the model.")
                (test-assert (not (search "private-denial-command" body)) "The refused command stays off the wire.")))
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-telemetry-outside-path-denial-hooks () null)
(defun test-telemetry-outside-path-denial-hooks ()
  "Refused, sandbox-only and unavailable approvals deny paths without changing replies."
  (dolist (decision '(:deny :sandboxed :unavailable))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((root      (test-configuration-root configuration))
              (workspace (merge-pathnames "workspace/" root))
              (path      (uiop:native-namestring (merge-pathnames "outside-missing.txt" root)))
              (observer
                (callback-agent-observer-create
                 :command-authorization-callback
                 (lambda (command directory)
                   (if (eq decision ':unavailable)
                       (error 'command-authorization-unavailable
                              :message "Fixture has no interactive approval."
                              :command command :directory directory)
                       decision))))
              (calls
                (list
                 (agent-test-call :call-id "read" :namespace "resource" :name "read"
                                  :arguments (json-encode (json-object "uri" path)))
                 (agent-test-call :call-id "edit" :namespace "resource" :name "edit"
                                  :arguments (json-encode
                                              (json-object "uri" path "base-revision" "R-denied"
                                                           "operations" #())))
                 (agent-test-call :call-id "paren" :namespace "lisp" :name "paren-check"
                                  :arguments (json-encode (json-object "path" path)))
                 (agent-test-call :call-id "emacs" :namespace "emacs" :name "eval"
                                  :arguments "{\"form\":\"(+ 1 1)\"}")
                 (agent-test-call :call-id "allowed" :namespace "resource" :name "read"
                                  :arguments "{\"uri\":\"workspace:allowed.txt\"}"))))
         (ensure-directories-exist workspace)
         (setf (config :working-directory configuration) workspace)
         (workspace-resource-tests--write-text (merge-pathnames "allowed.txt" workspace) "allowed content")
         (multiple-value-bind (spans outputs body)
             (test-telemetry-hooks--tool-attempts configuration calls :observer observer)
           (test-assert (= (length spans) 5) "Every denied call and the following allowed call is exported.")
           (loop for span in (subseq spans 0 4)
                 for code in '("permission-denied" "permission-denied" "invalid-arguments" "invalid-arguments")
                 do (test-assert
                     (and (equal (test-telemetry--attribute span "autolith.tool.outcome") "permission_denied")
                          (equal (test-telemetry--attribute span "autolith.tool.error_code") code)
                          (equal (test-telemetry--attribute span "error.type") "permission"))
                     "All rejected authority decisions retain codes and export permission_denied."))
           (test-assert
            (and (= (length outputs) 5)
                 (every (lambda (output) (search "requires full-access approval" output)) (subseq outputs 0 3))
                 (search "emacs.eval was not authorized" (fourth outputs))
                 (search "allowed content" (fifth outputs)))
            "Denials keep their original model-visible replies and do not perform the operation.")
           (test-assert (equal (test-telemetry--attribute (fifth spans) "autolith.tool.outcome") "success")
                        "A denial does not leak into the next call.")
           (test-assert (not (search "outside-missing" body)) "Denied paths stay out of telemetry."))))))
  nil)

(-> test-telemetry-resource-policy-denial-hooks () null)
(defun test-telemetry-resource-policy-denial-hooks ()
  "Actual scheme and ownership policy failures retain access-denied and count as denials."
  (dolist (case '((restricted-scheme t "memory:relevant")
                  (foreign-owner nil "shell-log:foreign/execution/artifact/output")))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((restricted-p (second case))
              (uri          (third case))
              (calls
                (append
                 (list (agent-test-call :call-id "read" :namespace "resource" :name "read"
                                        :arguments (json-encode (json-object "uri" uri))))
                 (when restricted-p
                   (list (agent-test-call :call-id "edit" :namespace "resource" :name "edit"
                                          :arguments (json-encode
                                                      (json-object "uri" uri "base-revision" "R-denied"
                                                                   "operations" #()))))))))
         (multiple-value-bind (spans outputs)
             (test-telemetry-hooks--tool-attempts configuration calls :restricted-p restricted-p)
           (test-assert (= (length spans) (length calls))
                        (format nil "~A: Every policy failure is exported." (first case)))
           (dolist (span spans)
             (test-assert
              (and (equal (test-telemetry--attribute span "autolith.tool.outcome") "permission_denied")
                   (equal (test-telemetry--attribute span "autolith.tool.error_code") "access-denied")
                   (equal (test-telemetry--attribute span "error.type") "permission"))
              (format nil "~A: Resource authority failures are permission denials with the existing code." (first case))))
           (test-assert (and (= (length outputs) (length calls))
                             (every (lambda (output) (search uri output)) outputs))
                        (format nil "~A: Every refusal still reaches the model with its original resource identity." (first case))))))))
  (test-telemetry--call
   (lambda (configuration)
     (let ((provider
             (make-instance
              'run-job-test-provider :configuration configuration
              :results
              (list
               (agent-test-result
                "child-read"
                (list (agent-test-call :call-id "read" :namespace "resource" :name "read"
                                       :arguments "{\"uri\":\"papercut:current\"}")))
               (agent-test-result
                "child-edit"
                (list (agent-test-call :call-id "edit" :namespace "resource" :name "edit"
                                       :arguments "{\"uri\":\"papercut:current\",\"base-revision\":\"\",\"operations\":[]}")))
               (agent-test-result
                "terminal"
                (list (agent-test-call :call-id "yield" :namespace "yield" :name "submit"
                                       :arguments "{\"status\":\"success\",\"data\":{\"answer\":\"yes\"}}")))))))
       (multiple-value-bind (spans results)
           (test-telemetry-hooks--run-job configuration provider :tools '("resource.read" "resource.edit"))
         (let ((tools (remove-if-not
                       (lambda (span) (equal (test-telemetry--attribute span "autolith.event.kind") "tool"))
                       spans)))
           (test-assert (eq (first results) ':succeeded) "The real child recovers from its resource denials.")
           (test-assert (= (length tools) 3) "Both child-resource denials and terminal yield are exported.")
           (dolist (span (subseq tools 0 2))
             (test-assert
              (and (equal (test-telemetry--attribute span "autolith.tool.outcome") "permission_denied")
                   (equal (test-telemetry--attribute span "autolith.tool.error_code") "access-denied")
                   (equal (test-telemetry--attribute span "error.type") "permission"))
              "The E2E papercut read and edit are child-policy denials even with full command access.")))))))
  nil)
