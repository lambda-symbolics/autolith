(in-package #:autolith)

;;;; -- Explicit Incident Explanations --

(-> test-telemetry-incident-diagnostics () null)
(defun test-telemetry-incident-diagnostics ()
  "Export explicit explanations across repair attempts and restart, with consent and real outcomes."
  (dolist (consent '(nil t))
    (test-telemetry--call
     (lambda (configuration)
       (setf (config :telemetry-enabled-p configuration) t
             (config :telemetry-diagnostics-p configuration) consent)
       (telemetry-attach configuration)
       (test-telemetry-hooks--repair-fixture
        configuration
        (lambda (registry context)
          (let* ((explanation (format nil "Observed retries returned success without a result. ~A Contact fixture@example.org token=fixture-token"
                                      (apply #'concatenate 'string (make-list 40 :initial-element "The cause is still a hypothesis. "))))
                 (incident    nil)
                 (mutation    nil)
                 (bodies      nil)
                 (redactions  0)
                 (*telemetry-process-function*
                   (lambda (run request)
                     (declare (ignore run))
                     (ecase (first request)
                       (:redact
                        (incf redactions)
                        (test-assert (not (search "fixture-token" (second request)))
                                     "Pre-redaction masks credentials before native work.")
                        (second request))
                       (:upload
                        (push (third request) bodies) t)))))
            (labels ((call (namespace name arguments)
                       (tool-execute (tool-registry-find registry namespace name) context arguments)))
              (telemetry-call-with-run
               configuration
               (lambda ()
                 (telemetry-note-tool :tool "shell.run" :outcome "error")
                 (call "papercut" "report"
                       (json-object "title" "Diagnostic failure fixture" "content" "PRIVATE_LOCAL_REPORT"
                                    "tool" "shell.run" "issue-kind" "broken_tool" "diagnostic" explanation))
                 (let ((report (first (papercut-list configuration))))
                   (setf incident (papercut-identifier report))
                   (test-assert (equal explanation (papercut-diagnostic report))
                                "The explicit report explanation survives durable replay."))
                 (call "self" "set"
                       (json-object "symbol" "*test-telemetry-repair-setting*" "value" ":changed"
                                    "incident" incident "diagnostic" "Changed the setting to test the retry hypothesis."))
                 (setf mutation (getf (rest (test-telemetry-hooks--latest-set configuration)) :id))))
              (telemetry-shutdown)
              (telemetry-attach configuration)
              (telemetry-call-with-run
               configuration
               (lambda ()
                 (call "self" "exercise"
                       (json-object "mutation" mutation "form" "(assert (eq *test-telemetry-repair-setting* :changed))"
                                    "diagnostic" "Check that the setting took effect; this does not test retry behavior."))
                 (test-assert
                  (handler-case
                      (progn
                        (call "self" "exercise"
                              (json-object "mutation" mutation "form" "(error \"PRIVATE_CHECK_OUTPUT\")"
                                           "diagnostic" "Fixed. This deliberate failing check must remain failed."))
                        nil)
                    (error () t))
                  "Check failures propagate despite the supplied explanation.")
                 (call "self" "discard" (json-object "mutation" mutation
                                                       "diagnostic" "Reverted the setting because the hypothesis remains unverified."))
                 (papercut-assess configuration incident :verdict ':improved :note "PRIVATE_LOCAL_ASSESSMENT"
                                  :diagnostic "The focused setting check passed; the retry cause remains uncertain.")
                 (papercut-mark-closed configuration incident :resolution "PRIVATE_LOCAL_RESOLUTION"
                                      :diagnostic                         "Closed this synthetic incident after rollback; no claim of a full fix.")))
              (let* ((body        (test-telemetry-hooks--merge-bodies (nreverse bodies)))
                     (report      (first (test-telemetry-hooks--kind body "report")))
                     (checks      (test-telemetry-hooks--kind body "check"))
                     (incident-id (test-telemetry--attribute report "autolith.incident.id"))
                     (repairs     (test-telemetry-hooks--kind body "repair"))
                     (attempt-id  (test-telemetry--attribute (first repairs) "autolith.repair.attempt.id")))
                (dolist (span (append repairs checks))
                  (test-assert (and (equal incident-id (test-telemetry--attribute span "autolith.incident.id"))
                                    (equal attempt-id (test-telemetry--attribute span "autolith.repair.attempt.id")))
                               "Restart preserves issue and attempt links for explanations."))
                (test-assert (equal (mapcar (lambda (span) (test-telemetry--attribute span "autolith.repair.outcome")) checks)
                                    '("verified" "failed"))
                             "Only real check outcomes determine verification.")
                (test-assert (not (json-true-p (test-telemetry--attribute (second checks) "autolith.repair.verified")))
                             "Narrative claims cannot verify a failed check.")
                (dolist (private '("PRIVATE_LOCAL_REPORT" "PRIVATE_LOCAL_ASSESSMENT" "PRIVATE_LOCAL_RESOLUTION"
                                   "PRIVATE_CHECK_OUTPUT" "fixture@example.org" "fixture-token"))
                  (test-assert (not (search private body)) "Private local text and credentials stay off the wire."))
                (if consent
                    (progn
                      (test-assert (> (length (test-telemetry--attribute report "autolith.diagnostic.summary")) 1024)
                                   "Detailed explanations survive the former limit.")
                      (dolist (kind '("repair" "check" "assessment" "closure"))
                        (test-assert (every (lambda (span) (test-telemetry--attribute span "autolith.diagnostic.summary"))
                                            (test-telemetry-hooks--kind body kind))
                                     "Each phase carries its own explicit explanation."))
                      (test-telemetry--collector-fixture "native-incident-diagnostics.json" (json-decode body)))
                    (test-assert (and (zerop redactions) (not (search "autolith.diagnostic.summary" body)))
                                 "Disabled diagnostics do not acquire or send narratives."))))))))))
  nil)

(-> test-telemetry-diagnostic-redactor-pieces () null)
(defun test-telemetry-diagnostic-redactor-pieces ()
  "Retain the entire narrative through the bounded native API, or discard it all."
  (let* ((text    (apply #'concatenate 'string (make-list 200 :initial-element "Check the retry result. ")))
         (pieces  nil)
         (fail-at nil))
    (test-call-with-function-replacements
     (list (list 'defingerprinter:redact
                 (lambda (redactor piece &key language)
                   (declare (ignore redactor language))
                   (push piece pieces)
                   (test-assert (<= (length piece) 512) "Each native input stays within its safe output budget.")
                   (if (eql (length pieces) fail-at)
                       (values nil ':inference-error)
                       (values piece ':ok)))))
     (lambda ()
       (test-assert (equal text (telemetry--redact-narrative nil text "en"))
                    "All prose and separators survive native piece assembly.")
       (test-assert (equal text (apply #'concatenate 'string (reverse pieces)))
                    "Every character passes through the redactor exactly once.")
       (setf pieces nil fail-at 2)
       (test-assert (null (telemetry--redact-narrative nil text "en"))
                    "A later native failure discards earlier successful pieces."))))
  nil)

(-> test-telemetry-resource-diagnostics () null)
(defun test-telemetry-resource-diagnostics ()
  "Resource assessments and closure use explicit text and preserve event phases."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-diagnostics-p configuration) t)
     (telemetry-attach configuration)
     (let* ((registry (make-default-tool-registry))
            (context  (make-instance 'tool-context :configuration configuration :worker nil
                                    :conversation (conversation-create configuration)))
            (bodies   nil)
            (*telemetry-process-function*
              (lambda (run request)
                (declare (ignore run))
                (ecase (first request)
                  (:redact
                   (second request))
                  (:upload
                   (push (third request) bodies) t)))))
       (labels ((read-resource (uri)
                  (test-resource-call registry context
                                                 :namespace "resource"
                                                 :name      "read"
                                                 :arguments (list "uri" uri)))

                (edit (uri operation)
                  (let ((revision (test-resource-field
                                   (tool-result-content (read-resource uri)) "Revision: ")))
                    (test-resource-call registry context
                                                  :namespace "resource"
                                                  :name      "edit"
                                                  :arguments (list "uri" uri "base-revision" revision
                                                                   "operations" (vector operation))))))
         (telemetry-call-with-run
          configuration
          (lambda ()
             (let* ((report (papercut-report configuration :title "Resource diagnostic fixture"
                                             :content             "PRIVATE_RESOURCE_CONTENT"))
                    (uri    (resource-item-uri "papercut" (papercut-identifier report))))
              (test-assert
               (not (tool-result-success-p
                     (edit uri (json-object "op" "papercut-assess" "verdict" "improved" "note" "PRIVATE_RESOURCE_NOTE"
                                            "diagnostic" (make-string 8001 :initial-element #\Space)))))
               "Oversized diagnostics fail before a resource edit mutates state.")
              (test-assert (null (papercut-assessment-verdict (papercut-find configuration (papercut-identifier report))))
                           "Invalid explanation leaves the assessment unchanged.")
              (test-assert
               (tool-result-success-p
                (edit uri (json-object "op" "papercut-assess" "verdict" "improved" "note" "PRIVATE_RESOURCE_NOTE"
                                       "diagnostic" "The focused probe improved; full behavior is untested.")))
               "Resource assessment accepts its explicit explanation.")
              (test-assert
               (tool-result-success-p
                (edit uri (json-object "op" "papercut-close" "resolution" "PRIVATE_RESOURCE_RESOLUTION"
                                       "diagnostic" "Closed after the focused probe; full verification remains absent.")))
               "Resource closure accepts its explicit explanation."))))
         (let* ((body       (test-telemetry-hooks--merge-bodies bodies))
                (assessment (first (test-telemetry-hooks--kind body "assessment")))
                (closure    (first (test-telemetry-hooks--kind body "closure"))))
           (test-assert (null (test-telemetry-hooks--kind body "repair")) "Assessments and closure are separate from repairs.")
           (test-assert (and (equal (test-telemetry--attribute assessment "autolith.repair.outcome") "improved")
                             (equal (test-telemetry--attribute closure "autolith.repair.outcome") "closed"))
                        "Resource lifecycle events retain their exact phase and outcome.")
           (test-assert (not (search "PRIVATE_RESOURCE" body)) "Private resource notes stay off the wire."))))))
  nil)

(-> test-telemetry-diagnostic-upload-bounds () null)
(defun test-telemetry-diagnostic-upload-bounds ()
  "Split escaped long narratives by actual UTF-8 request bytes without losing events."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-diagnostics-p configuration) t)
     (telemetry-attach configuration)
     (let ((bodies nil)
           (text   (apply #'concatenate 'string (make-list 2000 :initial-element "é\\ \""))))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (ecase (first request)
                   (:redact
                    (second request))
                   (:upload
                    (push (third request) bodies) t)))))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (loop repeat 100
                  do (telemetry-note-report :summary-function (lambda () text)))))
         (loop while (telemetry-controller-queue *telemetry-controller*)
               do (telemetry--upload-batch (telemetry--upload-context *telemetry-controller*))))
       (test-assert (> (length bodies) 1) "Expanded JSON is split into several requests.")
       (test-assert (= 101 (loop for body in bodies sum (length (test-telemetry--spans body))))
                    "All reports and the root survive byte-based batching.")
       (dolist (body bodies)
         (test-assert (<= (length (babel:string-to-octets body :encoding :utf-8)) 1048576)
                      "Every encoded request fits the collector's byte limit.")))))
  nil)


;;;; -- Native Narrative Boundary --

(-> test-telemetry-worker-narratives () null)
(defun test-telemetry-worker-narratives ()
  "Reject incomplete native narratives and close one context across all chunks."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((library (merge-pathnames "redactor-library" root))
          (context (list ':fixture))
          (text    (apply #'concatenate 'string
                          (make-list 500 :initial-element "Detailed observation. "))))
      (with-open-file (stream library :direction ':output :if-exists ':supersede)
        (write-string "fixture" stream))
      (dolist (case '(:success :failure :saturated :expanded :unbroken :oversized))
        (let ((loads 0) (creates 0) (closes 0) (pieces nil)
              (input (case case
                       (:unbroken
                        (make-string 600 :initial-element #\a))
                       (:oversized
                        (make-string (1+ *telemetry-diagnostic-limit*)
                                     :initial-element #\Space))
                       (otherwise
                        (subseq text 0 (min (length text) *telemetry-diagnostic-limit*))))))
          (test-call-with-function-replacements
           (list (list 'defingerprinter:load-library
                       (lambda (path) (declare (ignore path)) (incf loads)))
                 (list 'defingerprinter:make-redactor
                       (lambda (&key model-directory runtime-library)
                         (declare (ignore model-directory runtime-library))
                         (incf creates)
                         context))
                 (list 'defingerprinter:redact
                       (lambda (redactor piece &key language)
                         (test-assert (and (eq redactor context) (equal language "en")
                                           (<= (length piece) 512))
                                      "All chunks use the same context and fit the native input bound.")
                         (push piece pieces)
                         (cond
                           ((and (eq case ':failure) (= (length pieces) 2))
                            (values "PRIVATE_UNSAFE" ':inference-error))
                           ((and (eq case ':saturated) (= (length pieces) 2))
                            (values (make-string 1024 :initial-element #\Space) ':ok))
                           ((eq case ':expanded)
                            (values (make-string 1023 :initial-element #\Space) ':ok))
                           (t
                            (values piece ':ok)))))
                 (list 'defingerprinter:close-redactor
                       (lambda (redactor)
                         (test-assert (eq redactor context) "Close the narrative's native context.")
                         (incf closes))))
           (lambda ()
             (let ((result (telemetry--worker-redact
                            (list ':redact input "en" nil nil (namestring library)))))
               (if (eq case ':success)
                   (test-assert (and (equal result input) (> (length pieces) 1))
                                "Chunk concatenation preserves the complete narrative in order.")
                   (test-assert (null result)
                                "Any failed, saturated, oversized or unsplittable narrative is omitted.")))))
          (test-assert (= loads creates closes (if (eq case ':oversized) 0 1))
                       "Each admitted narrative owns exactly one native context.")
          (when (member case '(:failure :saturated))
            (test-assert (= (length pieces) 2)
                         "A bad middle chunk stops inference without exporting the first chunk."))))))
  nil)
