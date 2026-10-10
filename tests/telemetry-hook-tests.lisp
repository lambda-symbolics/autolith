(in-package #:autolith)

;;;; -- Real Lifecycle Boundaries --

(defclass telemetry-test-stream-provider (responses-api-provider)
  ((data :initarg :data :accessor telemetry-test-stream-data
         :documentation "The synthetic SSE body consumed by this fixture."))
  (:documentation "Read real Responses SSE through the native telemetry boundary."))

(defmethod provider-stream-turn ((provider telemetry-test-stream-provider) conversation
                                &key tool-namespaces event-callback goal-context compaction-p)
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (with-input-from-string (stream (telemetry-test-stream-data provider))
    (provider-consume-stream provider stream nil event-callback)))

(defun test-telemetry-returned-model-hook ()
  "Read multiline wire metadata, map aliases, and never reuse a prior attempt's model."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-model-aliases configuration)
           '(("private-response-model" . "model-b")))
     (let ((provider (make-instance 'telemetry-test-stream-provider
                                    :configuration configuration
                                    :data (format nil "data: {\"type\":\"response.completed\",~%data: \"response\":{\"id\":\"private-response-id\",\"model\":\"private-response-model\",\"usage\":{}}}~%~%"))))
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration
                       (lambda ()
                         (provider-stream-turn provider nil :event-callback #'identity)
                         (setf (telemetry-test-stream-data provider)
                               (format nil "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"second\",\"usage\":{}}}~%~%"))
                         (provider-stream-turn provider nil :event-callback #'identity))))
              (first (first (test-telemetry-hooks--kind (first bodies) "model")))
              (second (first (test-telemetry-hooks--kind (second bodies) "model"))))
         (test-assert (equal (test-telemetry--attribute first "gen_ai.response.model") "model-b")
                      "Returned model metadata passes through the configured public alias map.")
         (test-assert (equal (test-telemetry--attribute second "gen_ai.response.model") "unknown")
                      "An absent model stays unknown instead of inheriting a previous request.")
         (dolist (body bodies)
           (test-assert (not (search "private-response" body))
                        "Neither raw response model nor response identity is exported."))))))
  nil)

(defun test-telemetry-hooks--capture (configuration function)
  "Capture offline uploads while exercising the actual lifecycle wrappers."
  (setf (config :telemetry-enabled-p configuration) t)
  (telemetry-attach configuration)
  (let ((bodies nil))
    (let ((*telemetry-process-function*
            (lambda (run request)
              (declare (ignore run))
              (test-assert (eq (first request) :upload) "Metadata hooks only upload projected JSON.")
              (push (fourth request) bodies)
              t)))
      (let ((results (multiple-value-list (funcall function))))
        (values (nreverse bodies) results)))))

(defun test-telemetry-hooks--kind (body kind)
  "Select spans by their fixed public event kind."
  (remove-if-not
   (lambda (span)
     (equal (test-telemetry--attribute span "autolith.event.kind") kind))
   (coerce (test-telemetry--spans body) 'list)))

(defun test-telemetry-papercut-metadata-hooks ()
  "Classified reports and resource edits retain metadata through durable replay."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry (make-default-tool-registry))
            (context (make-instance 'tool-context :configuration configuration :worker nil
                                    :conversation (conversation-create configuration)))
            (bodies
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (telemetry-call-with-run
                  configuration
                  (lambda ()
                    (test-assert
                     (tool-result-success-p
                      (papercut-resource-tests--call
                       registry context "papercut" "report"
                       "title" "Shell permission denial" "content" "Fixture command authorization fails."
                       "issue-kind" "authorization" "tool" "shell.run"))
                     "The direct report tool accepts explicit metadata.")
                    (let* ((read (papercut-resource-tests--call registry context "resource" "read"
                                                              "uri" "papercut:current"))
                           (revision (papercut-resource-tests--field (tool-result-content read) "Revision: "))
                           (report (papercut-resource-tests--call
                                    registry context "resource" "edit" "uri" "papercut:current"
                                    "base-revision" revision "operations"
                                    (vector (json-object "op" "papercut-report"
                                                         "title" "Image renderer returns blank pixels"
                                                         "content" "Fixture raster decoding loses every pixel."
                                                         "issue-kind" "broken_tool" "tool" "fs.view-image")))))
                      (test-assert (tool-result-success-p report) "Resource reporting accepts metadata.")
                      (let ((papercut (find "fs.view-image" (papercut-list configuration)
                                            :key #'papercut-tool :test #'equal)))
                        (test-assert (equal (papercut-issue-kind papercut) "broken_tool")
                                     "The structured category survives reading the durable store.")
                        (papercut-assess configuration (papercut-identifier papercut)
                                         :verdict ':improved :note "Private fixture assessment.")
                        (papercut-mark-closed configuration (papercut-identifier papercut)
                                              :resolution "Private fixture closure."))))))))
            (reports (test-telemetry-hooks--kind (first bodies) "report"))
            (repairs (test-telemetry-hooks--kind (first bodies) "repair")))
       (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.issue.kind")) reports)
                           '("authorization" "broken_tool")) "Both real entry points emit classified reports.")
       (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "gen_ai.tool.name")) reports)
                           '("shell.run" "fs.view-image")) "Affected tools reach the wire.")
       (test-assert (and (= (length repairs) 2)
                         (every (lambda (span) (equal (test-telemetry--attribute span "autolith.repair.target")
                                                       "fs.view-image")) repairs))
                    "Assessments and closure retain the original affected tool."))))
  nil)

(defun test-telemetry-hooks--assert-attempts (body count)
  "Require one span per actual attempt and an exact root counter."
  (let ((models (test-telemetry-hooks--kind body "model"))
        (root (first (test-telemetry-hooks--kind body "run"))))
    (test-assert (= (length models) count) "Every attempted request has one model span.")
    (test-assert (equal (test-telemetry--attribute root "autolith.run.model_calls")
                        (write-to-string count))
                 "The root counts real attempts, without an outer fallback duplicate.")))

(defun test-telemetry-provider-retry-hooks ()
  "Exercise inherited subscription streaming, authentication refresh and transport retry."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-retry"))
            (result (agent-test-result "retry-result" nil))
            (provider (test-codex-provider-create
                       configuration (list :unauthorized :server-error :unauthorized result)))
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              ;; Only provider-attempt-turn is scripted. The streaming and recovery
              ;; methods are the real inherited subscription-provider protocol.
              (provider-stream-turn provider conversation
                                    :tool-namespaces #() :event-callback #'identity)))
         (test-assert (eq (first results) result) "Recovery returns the original provider result.")
         (test-assert (equal (reverse (test-codex-provider-refresh-flags provider))
                             '(nil t nil t))
                      "Transport recovery starts a fresh bounded authentication cycle.")
         (test-assert (= (length bodies) 1) "Standalone recovery has one logical root.")
         (test-telemetry-hooks--assert-attempts (first bodies) 4)
         (let ((models (test-telemetry-hooks--kind (first bodies) "model")))
           (dolist (span (butlast models))
             (test-assert (null (test-telemetry--attribute span "gen_ai.usage.input_tokens"))
                          "Failed attempts omit unknown usage, rather than recording zero."))
           (test-assert (equal (test-telemetry--attribute (car (last models))
                                                        "gen_ai.usage.input_tokens") "1")
                        "The successful attempt retains normalized usage."))))))
  nil)

(defun test-telemetry-provider-failure-hooks ()
  "Failed refresh and cancellation during backoff preserve their conditions and counts."
  (test-telemetry--call
   (lambda (configuration)
     (let ((conversation (conversation-create configuration :identifier "telemetry-failures")))
       (dolist (cancel-p '(nil t))
         (let* ((provider (test-codex-provider-create
                          configuration
                          (if cancel-p (list :overloaded (agent-test-result "unused" nil))
                              '(:unauthorized :unauthorized))))
                (caught nil)
                (*bounded-retry-sleep-function*
                  (lambda (seconds)
                    (declare (ignore seconds))
                    (when cancel-p (error 'application-turn-cancelled)))))
           (let ((bodies
                   (test-telemetry-hooks--capture
                    configuration
                    (lambda ()
                      (handler-case
                          (provider-stream-turn provider conversation
                                                :tool-namespaces #() :event-callback #'identity)
                        (authentication-error (condition) (setf caught condition))
                        (application-turn-cancelled (condition) (setf caught condition)))))))
             (test-assert (typep caught (if cancel-p 'application-turn-cancelled 'authentication-error))
                          "The original terminal condition survives telemetry cleanup.")
             (test-telemetry-hooks--assert-attempts (first bodies) (if cancel-p 1 2))
             (test-assert (equal (test-telemetry--attribute
                                 (first (test-telemetry-hooks--kind (first bodies) "run"))
                                 "autolith.run.outcome")
                                (if cancel-p "cancelled" "error"))
                          "Standalone terminal roots distinguish cancellation and failure.")))))))
  nil)

(defclass telemetry-test-values-provider (scripted-provider) ()
  (:documentation "Custom provider bypassing the per-attempt protocol."))

(defmethod provider-configuration ((provider telemetry-test-values-provider))
  (scripted-provider-configuration provider))

(defmethod provider-family ((provider telemetry-test-values-provider))
  (declare (ignore provider))
  (error "Injected optional provider metadata failure."))

(defmethod provider-stream-turn
    ((provider telemetry-test-values-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  (declare (ignore conversation tool-namespaces event-callback goal-context compaction-p))
  (values (pop (scripted-provider-results provider)) :secondary 17))

(defclass telemetry-test-mission-provider (telemetry-test-values-provider) ()
  (:documentation "Custom streaming and native-compaction provider for mission accounting."))

(defmethod provider-native-compact-conversation
    ((provider telemetry-test-mission-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values :compacted (json-object "input_tokens" 2 "output_tokens" 3) :secondary))

(defun test-telemetry-hooks--mission-accounting ()
  "Compose enabled telemetry with mission admission and both usage protocols."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((result (mission-test--result "done"))
            (provider (make-instance 'telemetry-test-mission-provider
                                     :configuration configuration :results (list result result)))
            (application (mission-test--application configuration))
            (conversation (application-conversation application)))
       (setf (application-provider application) provider)
       (application-mission-start application (mission-test--specification :turns 2))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (telemetry-call-with-run
               configuration
               (lambda ()
                 (test-assert
                  (equal (multiple-value-list
                          (provider-stream-turn provider conversation
                                                :tool-namespaces #() :event-callback #'identity))
                         (list result :secondary 17))
                  "Mission streaming retains all provider values with telemetry enabled.")
                 (multiple-value-bind (item usage extra)
                     (provider-native-compact-conversation
                      provider conversation :tool-namespaces #() :event-callback #'identity)
                   (test-assert (and (eq item :compacted) (eq extra :secondary)
                                     (= 2 (json-get usage "input_tokens")))
                                "Mission native compaction retains all provider values."))
                 (test-assert
                  (handler-case
                      (progn (provider-stream-turn provider conversation
                                                   :tool-namespaces #() :event-callback #'identity)
                             nil)
                    (mission-error (condition) (eq :exhausted (mission-error-reason condition))))
                  "Mission exhaustion rejects before telemetry or provider invocation.")))))
         (declare (ignore results))
         (test-telemetry-hooks--assert-attempts (first bodies) 2))
       (let ((goal (application-goal application)))
         (test-assert (= 2 (getf goal :turns-used)) "Both protocols charge one mission turn.")
         (test-assert (= 10 (getf goal :tokens-used)) "Both protocols settle exact reported usage.")
         (test-assert (eq :exhausted (getf goal :status)) "Mission exhaustion is durable.")
         (test-assert (= 1 (length (scripted-provider-results provider)))
                      "Rejected inference does not consume a scripted response.")))))
  nil)

(defun test-telemetry-provider-fallback-hooks ()
  "Custom providers get one standalone event despite broken metadata, with all values intact."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-fallback"))
            (result (agent-test-result "custom-result" nil))
            (provider (make-instance 'telemetry-test-values-provider
                                     :configuration configuration :results (list result))))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (provider-stream-turn provider conversation
                                    :tool-namespaces #() :event-callback #'identity)))
         (test-assert (equal results (list result :secondary 17)) "All custom provider values survive.")
         (test-telemetry-hooks--assert-attempts (first bodies) 1)
         (test-assert (equal (test-telemetry--attribute
                             (first (test-telemetry-hooks--kind (first bodies) "model"))
                             "gen_ai.provider.name") "other")
                      "An inaccessible provider family uses the public fallback."))
       ;; Scripted fixtures with no configuration are valid when telemetry is off.
       (let ((bare (make-instance 'telemetry-test-values-provider :results (list result))))
         (test-assert (equal (multiple-value-list
                             (provider-stream-turn bare conversation
                                                   :tool-namespaces #() :event-callback #'identity))
                            (list result :secondary 17))
                      "An unconfigured provider is usable outside an ambient run.")))))
  (test-telemetry-hooks--mission-accounting)
  nil)

(defclass telemetry-test-authorized-tool (agent-test-echo-tool) ()
  (:documentation "Exercise explicit authorization through the real registry helper."))

(defmethod tool-execute ((tool telemetry-test-authorized-tool)
                         (context tool-context) (arguments hash-table))
  (if (eq (tool-context-authorize-tool context tool arguments) :deny)
      (tool-failure "Denied fixture request.")
      (call-next-method)))

(defun test-telemetry-tool-denial-hooks ()
  "An explicit authorization decision is distinguishable from an ordinary failure string."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-denial"))
            (tool (make-instance 'telemetry-test-authorized-tool
                                 :namespace "test" :name "echo" :description "Authorized fixture."
                                 :parameters (tool-object-schema
                                              (json-object "value" (tool-string-property "Value."))
                                              '("value"))))
            (registry (agent-test-concurrency-registry (list tool)))
            (provider (make-instance 'scripted-provider :results
                                    (list (agent-test-result
                                           "denied-call"
                                           (list (agent-test-call :call-id "denied"
                                                                  :arguments "{\"value\":\"private-value\"}")))
                                          (agent-test-result "denied-done" (list (agent-test-message "done"))))))
            (agent (agent-create :configuration configuration :conversation conversation
                                 :provider provider :tool-registry registry :worker :unused))
            (observer (callback-agent-observer-create
                       :tool-authorization-callback
                       (lambda (tool arguments) (declare (ignore tool arguments)) :deny)))
            (bodies (test-telemetry-hooks--capture
                     configuration (lambda () (agent-run-user-turn agent "deny" :observer observer))))
            (spans (test-telemetry-hooks--kind (first bodies) "tool")))
       (test-assert (= (length spans) 1) "Denied execution is one completed attempt.")
       (test-assert (equal (test-telemetry--attribute (first spans) "autolith.tool.outcome")
                           "permission_denied") "The validated denial flag reaches export.")
       (test-assert (= (length (agent-test-tool-outputs conversation)) 1)
                    "Denied execution still records its provider-visible result.")
       (test-assert (not (search "private-value" (first bodies))) "Raw arguments never enter the envelope."))))
  nil)

(defclass telemetry-test-concurrent-tool (agent-test-concurrent-tool)
  ((runs :initform nil :accessor telemetry-test-concurrent-runs))
  (:documentation "Record ambient telemetry identity inside a real parallel tool worker."))

(defmethod tool-execute :before ((tool telemetry-test-concurrent-tool)
                                 (context tool-context) (arguments hash-table))
  (declare (ignore context arguments))
  (with-lock-held ((agent-test-concurrency-state-lock (agent-test-concurrent-tool-state tool)))
    (push *telemetry-run* (telemetry-test-concurrent-runs tool))))

(defun test-telemetry-parallel-tool-hooks ()
  "Parallel workers share one run and export in wire order, independently of completion order."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((conversation (conversation-create configuration :identifier "telemetry-parallel"))
            (state (make-instance 'agent-test-concurrency-state))
            (tool (make-instance 'telemetry-test-concurrent-tool
                                 :namespace "concurrency" :name "run" :description "Parallel fixture."
                                 :parameters (tool-parameters (agent-test-concurrency-tool state "run"))
                                 :state state :execution-policy :parallel))
            (registry (agent-test-concurrency-registry (list tool)))
            (provider (make-instance 'scripted-provider :results
                                    (list (agent-test-result
                                           "parallel-calls"
                                           (list (agent-test-call
                                                  :call-id "first" :namespace "concurrency" :name "run"
                                                  :arguments "{\"label\":\"first\",\"await_peer\":true,\"delay\":0.05,\"fail\":true}")
                                                 (agent-test-call
                                                  :call-id "second" :namespace "concurrency" :name "run"
                                                  :arguments "{\"label\":\"second\",\"await_peer\":true}")))
                                          (agent-test-result "parallel-done" (list (agent-test-message "done"))))))
            (agent (agent-create :configuration configuration :conversation conversation
                                 :provider provider :tool-registry registry :worker :unused))
            (bodies (test-telemetry-hooks--capture
                     configuration (lambda () (agent-run-user-turn agent "parallel"))))
            (spans (test-telemetry-hooks--kind (first bodies) "tool"))
            (runs (telemetry-test-concurrent-runs tool)))
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

(defun test-telemetry-repair-hook-projections ()
  "Journal transitions expose scalar verification; duplicate papercuts do not add reports."
  (test-telemetry--call
   (lambda (configuration)
     (let ((bodies
             (test-telemetry-hooks--capture
              configuration
              (lambda ()
                (telemetry-call-with-run
                 configuration
                 (lambda ()
                   (let ((*telemetry-tool-name* "self.redefine"))
                     (dolist (result '(:pending :installed :passed :failed :discarded :committed))
                       (telemetry-note-mutation-journal
                        (list :mutation :kind :definition :result result
                              :source "private-source-fixture" :output "private-output-fixture"))))
                   (multiple-value-bind (report duplicate-p)
                       (papercut-report configuration :title "Fixture event dispatch fault"
                                       :content "Fixture callback invocation loses its event.")
                     (test-assert (not duplicate-p) "First fixture report is new.")
                     (multiple-value-bind (duplicate duplicate-p)
                         (papercut-report configuration :title "Fixture event dispatch fault"
                                         :content "Fixture callback invocation loses its event.")
                       (test-assert (and duplicate-p
                                         (string= (papercut-identifier report) (papercut-identifier duplicate)))
                                    "Duplicates reuse the active report identifier."))
                     (papercut-mark-closed configuration (papercut-identifier report)
                                           :resolution "Fixture manually closed.")
                     (test-assert (null (papercut-list configuration)) "Closure updates persistent report state."))))))))
       (let* ((body (first bodies))
              (repairs (test-telemetry-hooks--kind body "repair"))
              (reports (test-telemetry-hooks--kind body "report")))
         (test-assert (= (length reports) 1) "Only a new papercut emits a report.")
         (test-assert (= (length repairs) 7) "Six journal transitions and manual closure emit repairs.")
         (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) repairs)
                             '("proposed" "applied" "verified" "failed" "reverted" "committed" "applied"))
                      "Journal outcomes have exact public projections.")
         (test-assert (json-true-p (test-telemetry--attribute (third repairs) "autolith.repair.verified"))
                      "Only passed journal verification is asserted as tested.")
         (test-assert (not (json-true-p (test-telemetry--attribute (car (last repairs)) "autolith.repair.verified")))
                      "Manual closure does not imply successful testing.")
         (dolist (span repairs)
           (test-assert (equal (test-telemetry--attribute span "autolith.report.id")
                               (test-telemetry--attribute (first reports) "autolith.report.id"))
                        "Repairs, including those preceding the report, share run correlation."))
         (dolist (private '("private-source-fixture" "private-output-fixture" "Fixture manually closed."))
           (test-assert (not (search private body)) "Metadata hooks omit private journal and summary text."))))))
  nil)

(defun test-telemetry-configuration-owner-hook ()
  "Replacing the owning application's configuration invalidates old runs and consent."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((application (make-instance 'application :configuration configuration))
            (replacement (configuration-copy configuration :telemetry-enabled-p nil))
            (stale-copy nil))
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

(defun test-telemetry-secondary-root-settings ()
  "Interactive ACP and application settings use the process consent authority."
  (test-telemetry--call
   (lambda (configuration)
     (telemetry-attach configuration)
     (let* ((secondary (configuration-copy configuration))
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
