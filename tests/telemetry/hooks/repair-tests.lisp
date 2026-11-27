(in-package #:autolith)

;;;; -- Durable Incident Repair Lifecycle --

(defvar *test-telemetry-repair-setting* :baseline
  "Dedicated setting for real repair lifecycle calls.")

(-> test-telemetry-hooks--repair-fixture (configuration function) t)
(defun test-telemetry-hooks--repair-fixture (configuration function)
  "Call FUNCTION with a real registry and an isolated image-state identity."
  (let ((*test-telemetry-repair-setting*   :baseline)
        (*image-state-initialized-p*       nil)
        (*active-image-lineage-identifier* nil)
        (*active-image-commit-identifier*  nil)
        (*active-image-history-commit*     nil)
        (*exploratory-undo-actions*        (make-hash-table :test #'equal))
        (*image-replay-skips*              nil))
    (image-state-load configuration)
    (funcall function (make-default-tool-registry)
             (make-instance 'tool-context :configuration configuration :worker nil
                            :conversation (conversation-create configuration)))))

(-> test-telemetry-hooks--latest-set (configuration) list)
(defun test-telemetry-hooks--latest-set (configuration)
  "Return the last installed setting mutation from the durable journal."
  (find-if (lambda (record)
             (and (eq (getf (rest record) :kind) ':set)
                  (eq (getf (rest record) :result) ':installed)))
           (reverse (mutation-journal-read-records configuration))))

(-> test-telemetry-evaluation-repair-intent () null)
(defun test-telemetry-evaluation-repair-intent ()
  "Export evaluation repairs only for calls linked to an incident."
  (test-telemetry--call
   (lambda (configuration)
     (test-telemetry-hooks--repair-fixture
      configuration
      (lambda (registry context)
        (dolist (case '(("42" nil "applied")
                        ("(error \"private-probe-failure\")" t "failed")
                        ("(" t "failed")))
          (destructuring-bind (source expected-failure outcome) case
            (dolist (linked-p '(nil t))
              (let* ((bodies
                       (test-telemetry-hooks--capture
                        configuration
                        (lambda ()
                          (telemetry-call-with-run
                           configuration
                           (lambda ()
                             (let* ((incident
                                      (when linked-p
                                        (papercut-report configuration
                                                         :title   "Evaluation repair fixture"
                                                         :content "Explicit evaluation repair")))
                                    (arguments (json-object "forms" (vector source))))
                               (when incident
                                 (setf (gethash "incident" arguments) (papercut-identifier incident)))
                               (test-assert
                                (eq expected-failure
                                    (handler-case
                                        (progn
                                          (tool-execute (tool-registry-find registry "self" "eval")
                                                        context arguments)
                                          nil)
                                      (error () t)))
                                "Evaluation results propagate to the caller.")
                               (when incident
                                 (papercut-mark-closed configuration (papercut-identifier incident)
                                                       :resolution "Evaluation fixture complete."))))))))
                     (body    (test-telemetry-hooks--merge-bodies bodies))
                     (repairs (test-telemetry-hooks--kind body "repair")))
                (test-assert
                 (equal (mapcar (lambda (span)
                                  (test-telemetry--attribute span "autolith.repair.outcome"))
                                repairs)
                        (when linked-p (list "proposed" outcome)))
                 "Only incident-linked evaluations emit repair transitions.")
                (when linked-p
                  (let* ((report      (first (test-telemetry-hooks--kind body "report")))
                         (incident-id (test-telemetry--attribute report "autolith.incident.id"))
                         (attempt-id  (test-telemetry--attribute (first repairs)
                                                                 "autolith.repair.attempt.id")))
                    (test-assert (telemetry--report-id-p attempt-id)
                                 "The linked evaluation has a public attempt identity.")
                    (dolist (span repairs)
                      (test-assert
                       (and (equal incident-id (test-telemetry--attribute span "autolith.incident.id"))
                            (equal attempt-id (test-telemetry--attribute span "autolith.repair.attempt.id")))
                       "Proposal and result retain their incident and attempt identities."))))))))
        (test-assert
         (= 12 (count ':eval (mutation-journal-read-records configuration)
                      :key (lambda (record) (getf (rest record) :kind))))
         "All six evaluations retain their proposal and result audit records.")))))
  nil)

(-> test-telemetry-incident-repair-lifecycle () null)
(defun test-telemetry-incident-repair-lifecycle ()
  "Exercise repair attempts, durable linkage, check verdicts and closure."
  (test-telemetry--call
   (lambda (configuration)
     (test-telemetry-hooks--repair-fixture
      configuration
      (lambda (registry context)
        (let ((incident nil) (mutation nil) (bodies nil))
          (labels ((run (function)
                     (setf bodies
                           (append bodies (test-telemetry-hooks--capture
                                           configuration
                                           (lambda () (telemetry-call-with-run configuration function))))))

                   (call (name arguments)
                     (tool-execute (tool-registry-find registry "self" name) context arguments)))
            (run (lambda ()
                   (setf incident (papercut-identifier
                                   (papercut-report configuration :title "Private repair fixture"
                                                    :content             "Private repair content"
                                                    :tool "shell.run" :issue-kind ':broken-tool)))
                   (call "set" (json-object "symbol" "*test-telemetry-repair-setting*"
                                            "value" ":private-repair-value" "incident" incident
                                            "repair-kind" "change_tool_strategy"))
                   (setf mutation (getf (rest (test-telemetry-hooks--latest-set configuration)) :id))
                   (call "exercise" (json-object "form" "(assert (eq *test-telemetry-repair-setting* :private-repair-value))"))
                   (test-assert
                    (handler-case
                        (progn (call "exercise" (json-object "mutation" mutation
                                                             "form" "(error \"private-check-failure\")")) nil)
                      (error () t))
                    "The failing exercise propagates its failure.")))
            (telemetry-shutdown)
            ;; Restart the telemetry controller without changing the live image lineage.
            (setf context (make-instance 'tool-context :configuration configuration :worker nil
                                         :conversation (conversation-create configuration)))
            (run (lambda ()
                   (call "exercise" (json-object "mutation" mutation "verification-scope" "full"
                                                "form" "(assert (eq *test-telemetry-repair-setting* :private-repair-value))"))
                   (let ((*telemetry-tool-name* "self.commit"))
                     (self-repair-check-mutations
                      configuration
                      (make-instance 'callback-mutation-checker
                                     :active-callback (lambda (configuration source)
                                                        (declare (ignore configuration source)) "private-check-output"))
                      "private-check-source" :mutations (list mutation))
                     (test-assert
                      (handler-case
                          (progn
                            (self-repair-check-mutations
                             configuration
                             (make-instance 'callback-mutation-checker
                                            :active-callback (lambda (configuration source)
                                                               (declare (ignore configuration source))
                                                               (error "private-check-failure")))
                             "private-check-source" :mutations (list mutation))
                            nil)
                        (error () t))
                      "The configured checker propagates failure."))
                   (call "set" (json-object "symbol" "*test-telemetry-repair-setting*"
                                            "value" ":second-private-value" "incident" incident))
                   (call "discard" (json-object))
                   (test-assert (eq *test-telemetry-repair-setting* :private-repair-value)
                                "Discard restores the prior setting value.")
                   (papercut-assess configuration incident :verdict ':improved :note "Private assessment note")
                   (papercut-mark-closed configuration incident :resolution "Private closure note")))
            (let* ((first-body    (first bodies))
                   (later-body    (second bodies))
                   (repairs       (test-telemetry-hooks--kind first-body "repair"))
                   (checks        (test-telemetry-hooks--kind first-body "check"))
                   (export-id     (test-telemetry--attribute (first (test-telemetry-hooks--kind first-body "report")) "autolith.incident.id"))
                   (attempt       (test-telemetry--attribute (first repairs) "autolith.repair.attempt.id"))
                   (later-check   (first (test-telemetry-hooks--kind later-body "check")))
                   (later-repair  (first (test-telemetry-hooks--kind later-body "repair")))
                   (checker-spans (rest (test-telemetry-hooks--kind later-body "check"))))
              (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome"))
                                          checker-spans) '("verified" "failed"))
                           "Configured checkers emit one completed check per verdict.")
              (dolist (span checker-spans)
                (test-assert (equal (test-telemetry--attribute span "autolith.verification.scope") "focused")
                             "Callback checkers use focused verification scope.")
                (test-assert (equal (test-telemetry--attribute span "autolith.repair.operation") "self.commit")
                             "Checker context retains its executing operation.")
                (test-assert (equal (test-telemetry--attribute span "autolith.repair.attempt.id") attempt)
                             "Aggregate checker results retain mutation attempt context."))
              (test-assert (eq (mutation-checker-verification-scope (make-instance 'full-suite-mutation-checker)) ':full)
                           "Only the full-suite checker declares full-suite verification.")
              (let ((reverted (find "reverted" (test-telemetry-hooks--kind later-body "repair")
                                    :key  (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome"))
                                    :test #'equal)))
                (test-assert (equal (test-telemetry--attribute reverted "autolith.repair.attempt.id")
                                    (test-telemetry--attribute later-repair "autolith.repair.attempt.id"))
                             "Discard retains the reverted attempt identity."))
              (test-assert (and (telemetry--report-id-p export-id) (telemetry--report-id-p attempt)
                                (not (equal export-id attempt))) "Incident and attempt IDs are distinct random IDs.")
              (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) repairs)
                                  '("proposed" "applied")) "One attempt emits proposal and application.")
              (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) checks)
                                  '("verified" "failed")) "Only completed checks emit check events.")
              (test-assert (eq (test-telemetry--attribute (first checks) "autolith.repair.verified") t)
                           "Only the passing check asserts verification.")
              (test-assert (not (test-telemetry--attribute (second checks) "autolith.repair.verified"))
                           "The failed check does not assert verification.")
              (dolist (span (append repairs checks (list later-check)))
                (test-assert (equal (test-telemetry--attribute span "autolith.report.id") export-id)
                             "Repair report IDs use the incident identity, not the attempt identity.")
                (test-assert (equal (test-telemetry--attribute span "autolith.repair.attempt.id") attempt)
                             "Explicit and default checks inherit durable attempt context.")
                (test-assert (equal (test-telemetry--attribute span "autolith.incident.id") export-id)
                             "Fresh runs retain incident linkage.")
                (test-assert (equal (test-telemetry--attribute span "autolith.repair.kind") "change_tool_strategy")
                             "Checks retain the explicit repair category.")
                (test-assert (equal (test-telemetry--attribute span "gen_ai.tool.name") "shell.run")
                             "The affected tool is distinct from the executing self operation."))
              (test-assert (equal (test-telemetry--attribute later-check "autolith.verification.scope") "full")
                           "The explicit check scope reaches the wire.")
              (test-assert (not (equal (test-telemetry--attribute later-repair "autolith.repair.attempt.id") attempt))
                           "Another mutation creates another attempt.")
              (loop for kind in '("assessment" "closure")
                    for outcome in '("improved" "closed")
                    for span = (first (test-telemetry-hooks--kind later-body kind))
                    do (test-assert (equal (test-telemetry--attribute span "autolith.repair.outcome") outcome)
                                    "Assessment and closure keep distinct outcomes.")
                       (test-assert (equal (test-telemetry--attribute span "autolith.incident.id") export-id)
                                    "Incident phases retain correlation."))
              (dolist (body bodies)
                (dolist (private (list incident mutation "private-repair-value" "private-check-failure"
                                      "*test-telemetry-repair-setting*" "Private assessment" "Private closure"
                                      "Private repair" "private-check-source" "private-check-output" "(assert"))
                  (test-assert (not (search private body)) "Source, values, notes and local IDs stay private."))))))))))
  nil)

(-> test-telemetry-invalid-incident-link () null)
(defun test-telemetry-invalid-incident-link ()
  "Reject unknown and conflicting incidents before mutation; allocate nothing without consent."
  (test-telemetry--call
   (lambda (configuration)
     (test-telemetry-hooks--repair-fixture
      configuration
      (lambda (registry context)
        (labels ((call (name arguments)
                   (tool-execute (tool-registry-find registry "self" name) context arguments)))
          (call "set" (json-object "symbol" "*test-telemetry-repair-setting*" "value" ":offline"))
          (test-assert (every (lambda (record) (null (getf (rest record) :repair-context)))
                              (mutation-journal-read-records configuration))
                       "Default-off mutation allocates no export context.")
          (test-telemetry-hooks--capture
           configuration
           (lambda ()
             (telemetry-call-with-run
              configuration
              (lambda ()
                (let* ((first  (papercut-report configuration :title "First incident" :content "First fixture"))
                       (second (papercut-report configuration :title "Second incident" :content "Second fixture"))
                       (count  (length (mutation-journal-read-records configuration))))
                  (test-assert
                   (handler-case
                       (progn (call "set" (json-object "symbol" "*test-telemetry-repair-setting*" "value" ":invalid"
                                                       "incident" "missing-local-incident")) nil)
                     (papercut-error () t))
                   "Unknown incident IDs raise a typed failure.")
                  (test-assert (and (eq *test-telemetry-repair-setting* :offline)
                                    (= count (length (mutation-journal-read-records configuration))))
                               "Unknown links fail before changing the setting or journal.")
                  (call "set" (json-object "symbol" "*test-telemetry-repair-setting*" "value" ":linked"
                                           "incident" (papercut-identifier first)))
                  (let ((mutation (getf (rest (test-telemetry-hooks--latest-set configuration)) :id))
                        (count    (length (mutation-journal-read-records configuration))))
                    (test-assert
                     (handler-case
                         (progn (call "exercise" (json-object "mutation" mutation "incident" (papercut-identifier second)
                                                             "form" "(setf *test-telemetry-repair-setting* :invalid)")) nil)
                       (source-mutation-error () t))
                     "Conflicting incident links raise a typed failure.")
                    (test-assert (and (eq *test-telemetry-repair-setting* :linked)
                                      (= count (length (mutation-journal-read-records configuration))))
                                 "Conflicting links fail before source evaluation or journal writes."))))))))))))
  nil)

(-> test-telemetry-repair-hook-projections () null)
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
                   (let ((*telemetry-tool-name* "self.define"))
                     (dolist (result '(:pending :installed :passed :failed :discarded :committed))
                       (telemetry-note-mutation-journal
                        (list :mutation :kind (if (member result '(:passed :failed)) ':exercise ':definition)
                              :id "private-mutation-id" :result result
                              :source "private-source-fixture" :output "private-output-fixture"))))
                   (multiple-value-bind (report duplicate-p)
                       (papercut-report configuration :title "Fixture event dispatch fault"
                                       :content              "Fixture callback invocation loses its event.")
                     (test-assert (not duplicate-p) "First fixture report is new.")
                     (multiple-value-bind (duplicate duplicate-p)
                         (papercut-report configuration :title "Fixture event dispatch fault"
                                         :content              "Fixture callback invocation loses its event.")
                       (test-assert (and duplicate-p
                                         (string= (papercut-identifier report) (papercut-identifier duplicate)))
                                    "Duplicates reuse the active report identifier."))
                     (papercut-mark-closed configuration (papercut-identifier report)
                                           :resolution "Fixture manually closed.")
                     (test-assert (null (papercut-list configuration)) "Closure updates persistent report state."))))))))
        (let* ((body    (first bodies))
               (repairs (test-telemetry-hooks--kind body "repair"))
               (checks  (test-telemetry-hooks--kind body "check"))
               (closure (first (test-telemetry-hooks--kind body "closure")))
               (reports (test-telemetry-hooks--kind body "report")))
          (test-assert (= (length reports) 1) "Only a new papercut emits a report.")
          (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) repairs)
                              '("proposed" "applied" "reverted" "committed"))
                       "Repair transitions have exact public outcomes.")
          (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) checks)
                              '("verified" "failed")) "Check outcomes are separate from repair transitions.")
          (test-assert (json-true-p (test-telemetry--attribute (first checks) "autolith.repair.verified"))
                       "Only a passed check asserts verification.")
          (test-assert (not (json-true-p (test-telemetry--attribute (second checks) "autolith.repair.verified")))
                       "A failed check does not assert verification.")
          (test-assert (not (json-true-p (test-telemetry--attribute closure "autolith.repair.verified")))
                       "Manual closure does not imply successful testing.")
          (test-assert (equal (test-telemetry--attribute closure "autolith.report.id")
                              (test-telemetry--attribute (first reports) "autolith.report.id"))
                       "Closure references its own report.")
          (test-assert (equal (test-telemetry--attribute closure "autolith.repair.outcome") "closed")
                       "Closure keeps its explicit closed outcome.")
          (dolist (span (append repairs checks))
            (test-assert (equal (test-telemetry--attribute span "autolith.report.id")
                                (test-telemetry--attribute (first repairs) "autolith.report.id"))
                         "Transitions of the same mutation retain their own correlation.")
            (test-assert (not (equal (test-telemetry--attribute span "autolith.report.id")
                                     (test-telemetry--attribute (first reports) "autolith.report.id")))
                         "An unrelated mutation must not claim to repair a reported issue."))
         (dolist (private '("private-source-fixture" "private-output-fixture" "Fixture manually closed."))
           (test-assert (not (search private body)) "Metadata hooks omit private journal and summary text."))))))
  nil)
