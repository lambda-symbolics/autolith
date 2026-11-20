(in-package #:autolith)

;;;; -- Papercut Reports and Failure Correlation --

(-> test-telemetry-duplicate-flushed-tool-report-link () null)
(defun test-telemetry-duplicate-flushed-tool-report-link ()
  "Link duplicate incidents once through queued tool rows or exported references."
  (dolist (case '((queued-failure nil) (flushed-failure t)))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((flushed-p (second case))
              (report    nil)
              (bodies
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (telemetry-call-with-run
                    configuration
                    (lambda ()
                      (setf report (papercut-report configuration :title "Repeated resource failure"
                                                   :content              "The same resource read keeps failing."
                                                   :tool "resource.read" :issue-kind ':broken-tool))
                      (telemetry-note-tool :tool "resource.read" :outcome "error" :duration-ms 1)
                      (when flushed-p (telemetry-flush))
                      (loop repeat 2
                            do (multiple-value-bind (duplicate duplicate-p)
                                   (papercut-report configuration :title "Repeated resource failure"
                                                    :content             "The same resource read keeps failing."
                                                    :tool "resource.read" :issue-kind ':broken-tool)
                                 (test-assert (and duplicate-p (equal (papercut-identifier duplicate)
                                                                     (papercut-identifier report)))
                                              (format nil "~A: The repeated report reuses its durable incident." (first case))))))))))
              (tool      (first (test-telemetry-hooks--kind (first bodies) "tool")))
              (reports   (test-telemetry-hooks--kind (first (last bodies)) "report")))
         (test-assert (= (length bodies) (if flushed-p 2 1))
                      (format nil "~A: Only an explicit mid-run flush splits the upload." (first case)))
         (test-assert (= (length reports) 1)
                      (format nil "~A: Repeated duplicates add at most one reference." (first case)))
         (if flushed-p
             (let ((reference (first reports)))
               (test-assert (null (test-telemetry--attribute tool "autolith.report.id"))
                            (format nil "~A: A reference does not rewrite an exported tool row." (first case)))
               (test-assert (equal (test-telemetry--attribute reference "autolith.report.id")
                                   (papercut-telemetry-id report))
                            (format nil "~A: The reference retains the existing incident's export identity." (first case)))
               (test-assert (equal (test-telemetry--attribute reference "autolith.tool.span_id")
                                   (json-get tool "spanId"))
                            (format nil "~A: The duplicate reference identifies the exact flushed attempt." (first case)))
               (test-assert (equal (json-get reference "traceId") (json-get tool "traceId"))
                            (format nil "~A: The reference and failed attempt share a run." (first case))))
             (test-assert (equal (test-telemetry--attribute tool "autolith.report.id")
                                 (papercut-telemetry-id report))
                          (format nil "~A: A queued failed attempt carries the duplicate's link directly." (first case))))))))
  nil)

(-> test-telemetry-duplicate-consent-transition () null)
(defun test-telemetry-duplicate-consent-transition ()
  "Persist a previously private duplicate before linking queued or exported failures."
  (dolist (case '((queued-consent nil) (flushed-consent t)))
    (test-telemetry--call
     (lambda (configuration)
       (let* ((flushed-p  (second case))
              (report     (papercut-report configuration :title "Repeated private resource failure"
                                           :content             "The same resource read keeps failing."
                                           :tool "resource.read" :issue-kind ':broken-tool))
              (identifier (papercut-identifier report))
              (identity   nil))
         (test-assert (null (papercut-telemetry-id report))
                      (format nil "~A: Reporting without consent allocates no export ID." (first case)))
         (let* ((bodies
                  (test-telemetry-hooks--capture
                   configuration
                   (lambda ()
                     (telemetry-call-with-run
                      configuration
                      (lambda ()
                        (telemetry-note-tool :tool "resource.read" :outcome "error" :duration-ms 1)
                        (when flushed-p (telemetry-flush))
                        (multiple-value-bind (duplicate duplicate-p)
                            (papercut-report configuration :title "Repeated private resource failure"
                                             :content             "The same resource read keeps failing."
                                             :tool "resource.read" :issue-kind ':broken-tool)
                          (test-assert (and duplicate-p (equal identifier (papercut-identifier duplicate)))
                                       (format nil "~A: Consent does not create another incident." (first case)))
                          (setf identity (papercut-telemetry-id duplicate))
                          (test-assert (telemetry--report-id-p identity)
                                       (format nil "~A: The duplicate acquires an export ID." (first case)))
                          (test-assert (equal identity (papercut-telemetry-id (papercut-find configuration identifier)))
                                       (format nil "~A: The first link's identity is already durable." (first case)))))))))
                (tool (first (test-telemetry-hooks--kind (first bodies) "tool")))
                (link (if flushed-p
                          (first (test-telemetry-hooks--kind (second bodies) "report"))
                          tool)))
           (test-assert (equal (test-telemetry--attribute link "autolith.incident.id") identity)
                        (format nil "~A: The first exported link uses the persisted incident identity." (first case)))
           (when flushed-p
             (test-assert (equal (test-telemetry--attribute link "autolith.tool.span_id") (json-get tool "spanId"))
                          (format nil "~A: The report reference identifies the exported attempt." (first case)))))
         (telemetry-shutdown)
         (let* ((bodies
                  (test-telemetry-hooks--capture
                   configuration
                   (lambda ()
                     (telemetry-call-with-run
                      configuration
                      (lambda ()
                        (test-assert (equal (getf (papercut-repair-context configuration identifier) :incident-id) identity)
                                     (format nil "~A: Repair context replays the original link's identity." (first case)))
                        (papercut-assess configuration identifier :verdict ':improved :note "Private assessment.")
                        (papercut-mark-closed configuration identifier :resolution "Private closure."))))))
                (body (first bodies)))
           (dolist (kind '("assessment" "closure"))
             (test-assert (equal (test-telemetry--attribute (first (test-telemetry-hooks--kind body kind))
                                                           "autolith.incident.id")
                                 identity)
                          (format nil "~A: Later incident events retain the first link's identity after restart." (first case)))))))))
  nil)

(-> test-telemetry-tool-report-link () null)
(defun test-telemetry-tool-report-link ()
  "Link a report to the latest matching failure without joining unrelated attempts."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry   (make-default-tool-registry))
            (provider   (make-instance
                         'scripted-provider :configuration configuration
                         :results
                         (list (agent-test-result "failure"
                                                  (list (agent-test-call :call-id "older" :namespace "resource" :name "read"
                                                                         :arguments "{\"uri\":\"private-older-missing-file\"}")
                                                        (agent-test-call :call-id "missing" :namespace "resource" :name "read"
                                                                         :arguments "{\"uri\":\"private-missing-file\"}")))
                               (agent-test-result "report"
                                                  (list (agent-test-call :call-id "report" :namespace "papercut" :name "report"
                                                                         :arguments "{\"title\":\"Fixture missing resource\",\"content\":\"Fixture resource read fails.\",\"tool\":\"resource.read\",\"issue-kind\":\"broken_tool\"}")))
                               (agent-test-result "done" (list (agent-test-message "done"))))))
            (agent      (agent-create :configuration configuration :provider provider
                                      :conversation (conversation-create configuration)
                                      :tool-registry registry :worker :unused))
            (body       (first (test-telemetry-hooks--capture configuration (lambda () (agent-run-user-turn agent "Read then report.")))))
            (tools      (test-telemetry-hooks--kind body "tool"))
            (report     (first (test-telemetry-hooks--kind body "report")))
            (identifier (test-telemetry--attribute report "autolith.report.id")))
       (test-assert (telemetry--report-id-p identifier) "A real papercut is filed.")
       (test-assert (equal (test-telemetry--attribute report "autolith.tool.span_id")
                           (json-get (second tools) "spanId"))
                    "The report identifies the failed attempt independently of the tool row.")
       (dolist (key '("autolith.report.id" "autolith.incident.id"))
         (test-assert (null (test-telemetry--attribute (first tools) key))
                      "An older failure does not acquire the latest attempt's report.")
         (test-assert (equal (test-telemetry--attribute (second tools) key) identifier)
                      "The failed attempt joins to its report.")
         (test-assert (null (test-telemetry--attribute (third tools) key))
                      "The reporting tool does not claim to be the failed attempt."))
       (test-assert (not (search "private-missing-file" body)) "The local path stays private.")
       (test-telemetry--collector-fixture "native-tool-report.json" (json-decode body)))))
  nil)

(-> test-telemetry-flushed-tool-report-link () null)
(defun test-telemetry-flushed-tool-report-link ()
  "Join a real papercut to an exported tool failure without rewriting that row."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((bodies
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (telemetry-call-with-run
                  configuration
                  (lambda ()
                    (telemetry-note-tool :tool "resource.read" :outcome "error"
                                         :arguments   (json-object "uri" "private-flushed-missing-file")
                                         :duration-ms 1)
                    (telemetry-flush)
                    (papercut-report configuration :title "Flushed fixture failure"
                                     :content             "A resource read failed before this report."
                                     :tool "resource.read" :issue-kind ':broken-tool)
                    (papercut-report configuration :title "Another fixture issue"
                                     :content             "An independent issue must not claim the same attempt."
                                     :tool "resource.read" :issue-kind ':other))))))
            (tool    (first (test-telemetry-hooks--kind (first bodies) "tool")))
            (reports (test-telemetry-hooks--kind (second bodies) "report"))
            (report  (first reports)))
       (test-assert (= (length bodies) 2) "The tool is uploaded before the reports.")
       (test-assert (equal (test-telemetry--attribute tool "autolith.tool.outcome") "error")
                    "The completed tool failure reaches the first upload.")
       (test-assert (null (test-telemetry--attribute tool "autolith.report.id"))
                    "The already exported tool has no later report identity.")
       (test-assert (equal (test-telemetry--attribute report "autolith.tool.span_id")
                           (json-get tool "spanId"))
                    "The later report retains the exact exported failure identity.")
       (test-assert (equal (json-get report "traceId") (json-get tool "traceId"))
                    "The join uses the same run's trace and tool span.")
       (test-assert (equal (test-telemetry--attribute report "autolith.incident.id")
                           (test-telemetry--attribute report "autolith.report.id"))
                    "The report identity is the incident identity used by repairs.")
       (test-assert (null (test-telemetry--attribute (second reports) "autolith.tool.span_id"))
                    "Another incident cannot replace the first report's link.")
       (let* ((later        (test-telemetry-hooks--capture
                             configuration
                             (lambda ()
                               (telemetry-call-with-run
                                configuration
                                (lambda ()
                                  (papercut-report configuration :title "Later run fixture issue"
                                                   :content             "Do not link a previous run's failure."
                                                   :tool "resource.read" :issue-kind ':other))))))
              (later-report (first (test-telemetry-hooks--kind (first later) "report"))))
         (test-assert (null (test-telemetry--attribute later-report "autolith.tool.span_id"))
                      "Failure identities never cross runs."))
       (test-telemetry--collector-fixture "native-flushed-tool-report.json"
                                          (map 'vector #'json-decode bodies)))))
  nil)

(-> test-telemetry-role-papercut-report () null)
(defun test-telemetry-role-papercut-report ()
  "Grant papercut reporting to a role and export a report from its restricted agent."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry       (make-default-tool-registry))
            (definition     (task-agent-definition-create
                             :name "reporter" :description "Report a fixture problem."
                             :instructions "Report once." :tools '("papercut.report") :source ':test))
            (child-registry (task-child-tool-registry registry definition :depth 1))
            (conversation   (conversation-create configuration))
            (provider       (make-instance
                             'scripted-provider
                             :results (list (agent-test-result
                                             "report"
                                             (list (agent-test-call
                                                    :call-id "report" :namespace "papercut" :name "report"
                                                    :arguments "{\"title\":\"Fixture defect\",\"content\":\"Fixture details\",\"tool\":\"shell.run\",\"issue-kind\":\"broken_tool\"}")))
                                            (agent-test-result "done" (list (agent-test-message "done"))))))
            (agent          (agent-create :configuration configuration :conversation conversation
                                          :provider provider :tool-registry child-registry :worker :unused)))
       (task-agent-definition-validate-tools-available definition registry)
       (let* ((bodies (test-telemetry-hooks--capture
                       configuration (lambda () (agent-run-user-turn agent "Report fixture"))))
              (report (first (test-telemetry-hooks--kind (first bodies) "report"))))
         (test-assert report "A granted role exports the report event.")
         (test-assert (equal (test-telemetry--attribute report "gen_ai.tool.name") "shell.run")
                      "The report preserves its structured affected tool.")))))
  nil)

(-> test-telemetry-papercut-metadata-hooks () null)
(defun test-telemetry-papercut-metadata-hooks ()
  "Classified reports and resource edits retain metadata through durable replay."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((registry   (make-default-tool-registry))
            (context    (make-instance 'tool-context :configuration configuration :worker nil
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
                       registry context
                       :namespace "papercut"
                       :name      "report"
                       :arguments (list "title" "Shell permission denial"
                                        "content" "Fixture command authorization fails."
                                        "issue-kind" "authorization" "tool" "shell.run")))
                     "The direct report tool accepts explicit metadata.")
                    (let ((papercut (papercut-report configuration :title "Image renderer returns blank pixels"
                                                    :content "Fixture raster decoding loses every pixel."
                                                    :issue-kind ':broken-tool :tool "fs.view-image")))
                        (papercut-assess configuration (papercut-identifier papercut)
                                         :verdict ':improved :note "Private fixture assessment.")
                        (papercut-mark-closed configuration (papercut-identifier papercut)
                                              :resolution "Private fixture closure.")))))))
            (reports    (test-telemetry-hooks--kind (first bodies) "report"))
            (assessment (first (test-telemetry-hooks--kind (first bodies) "assessment")))
            (closure    (first (test-telemetry-hooks--kind (first bodies) "closure"))))
        (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.issue.kind")) reports)
                            '("authorization" "broken_tool")) "Reports emit their classified issue kinds.")
        (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "gen_ai.tool.name")) reports)
                            '("shell.run" "fs.view-image")) "Affected tools reach the wire.")
        (test-assert (equal (test-telemetry--attribute assessment "autolith.repair.outcome") "improved")
                     "Assessment retains its effectiveness verdict.")
        (test-assert (equal (test-telemetry--attribute closure "autolith.repair.outcome") "closed")
                     "Closure is a separate event with a closed outcome.")
        (dolist (span (list assessment closure))
          (test-assert (equal (test-telemetry--attribute span "autolith.repair.target") "fs.view-image")
                       "Assessment and closure retain the affected tool.")))))
  nil)

(-> test-telemetry-issue-correlation () null)
(defun test-telemetry-issue-correlation ()
  "Separate issues in one run and preserve their own IDs across runs and controller restarts."
  (test-telemetry--call
   (lambda (configuration)
     (let ((first-id nil) (second-id nil) (first-body nil))
       (setf first-body
             (first
              (test-telemetry-hooks--capture
               configuration
               (lambda ()
                 (telemetry-call-with-run
                  configuration
                  (lambda ()
                    (setf first-id (papercut-identifier
                                    (papercut-report configuration :title "Shell output missing"
                                                     :content             "Fixture command stdout disappears."
                                                     :tool "shell.run" :issue-kind ':broken-tool))
                          second-id (papercut-identifier
                                     (papercut-report configuration :title "Image decoder fails"
                                                      :content             "Fixture picture pixels are corrupted."
                                                      :tool "fs.view-image" :issue-kind ':performance)))))))))
       (telemetry-shutdown)
       (let* ((later-bodies
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (telemetry-call-with-run
                    configuration
                    (lambda () (papercut-assess configuration first-id :verdict ':improved :note "Fixture improved.")))
                   (telemetry-call-with-run
                    configuration
                    (lambda () (papercut-mark-closed configuration first-id :resolution "Fixture resolved."))))))
              (reports   (test-telemetry-hooks--kind first-body "report"))
              (report-id (test-telemetry--attribute (first reports) "autolith.report.id")))
         (test-assert (not (equal report-id (test-telemetry--attribute (second reports) "autolith.report.id")))
                      "Two issues in a single run have independent telemetry identities.")
         (test-assert (not (equal report-id (remove #\- first-id))) "Local papercut IDs are not reused on the wire.")
         (loop for body in later-bodies
               for kind in '("assessment" "closure")
               do (test-assert (equal report-id
                                      (test-telemetry--attribute
                                       (first (test-telemetry-hooks--kind body kind)) "autolith.report.id"))
                               "Later incident events retain identity across fresh runs.")
                  (test-assert (not (search first-id body)) "Local issue identifiers stay private."))
         (test-assert (papercut-find configuration second-id) "The unrelated issue remains open."))
       (setf (config :telemetry-enabled-p configuration) nil)
       (let* ((off (papercut-report configuration :title "Offline fixture issue" :content "Offline fixture details."))
              (id  (papercut-identifier off)))
         (test-assert (null (papercut-telemetry-id off)) "Default-off reporting allocates no export identity.")
         (test-telemetry-hooks--capture
          configuration
          (lambda ()
            (telemetry-call-with-run configuration
                                     (lambda () (papercut-assess configuration id :verdict ':improved :note "Improved.")))))
         (let ((identity (papercut-telemetry-id (papercut-find configuration id))))
           (test-assert (telemetry--report-id-p identity) "First consented assessment persists an independent identity.")
           (test-assert (equal identity (papercut-telemetry-id (papercut-find configuration id)))
                        "The identity annotation replays without regeneration."))))))
  nil)
