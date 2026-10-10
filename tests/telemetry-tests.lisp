(in-package #:autolith)

;;;; -- Consent and Schema Boundaries --

(defun test-telemetry--call (function)
  "Run one telemetry fixture with private configuration roots and complete cleanup."
  (with-test-configuration (configuration)
    (telemetry-shutdown)
    (unwind-protect (funcall function configuration)
      (telemetry-shutdown))))

(defun test-telemetry--spans (body)
  "Decode the projected spans of a captured mock upload."
  (let* ((envelope (json-decode body))
         (resource (aref (json-get envelope "resourceSpans") 0))
         (scope (aref (json-get resource "scopeSpans") 0)))
    (json-get scope "spans")))

(defun test-telemetry--attribute (span name)
  "Return a captured OTLP scalar attribute."
  (let* ((attribute (find name (json-get span "attributes")
                          :key (lambda (entry) (json-get entry "key")) :test #'string=))
         (value (and attribute (json-get attribute "value"))))
    (when value
      (or (json-get value "stringValue") (json-get value "intValue")
          (json-get value "boolValue")))))

(defun test-telemetry-default-off ()
  "Default-off does not allocate IDs, inspect text, invoke native work or upload."
  (test-telemetry--call
   (lambda (configuration)
     (let ((calls 0)
           (*telemetry-process-function* (lambda (&rest arguments)
                                          (declare (ignore arguments))
                                          (error "Unexpected telemetry process."))))
       (telemetry-attach configuration)
       (test-assert (null (config :telemetry-enabled-p configuration)) "Telemetry defaults off.")
       (test-assert (null (config :telemetry-diagnostics-p configuration)) "Diagnostics default off.")
       (test-call-with-function-replacements
        (list (list 'telemetry--id (lambda (bytes) (declare (ignore bytes))
                                    (error "Unexpected identity allocation."))))
        (lambda ()
          (test-assert
           (equal (multiple-value-list
                   (telemetry-call-with-run
                    configuration
                    (lambda ()
                      (telemetry-note-model :provider "local" :usage (json-object "input_tokens" 3))
                      (telemetry-note-tool :tool "shell.run" :arguments (json-object "command" "fixture"))
                      (telemetry-note-report :summary-function (lambda () (incf calls) "fixture"))
                      (values 1 2)))) '(1 2))
           "All values survive the disabled wrapper.")))
       (test-assert (zerop calls) "Disabled summaries are never read.")
       (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "No disabled queue.")
       (dolist (value '(1 :yes "ambiguous"))
         (test-assert (handler-case (progn (setf (config :telemetry-enabled-p configuration) value) nil)
                        (setting-invalid () t)) "Consent accepts exact T or NIL."))
       (dolist (endpoint '("https://example.org/v1/traces" "http://127.0.0.1:4318/v1/traces"
                           "http://localhost:4318/v1/traces" "http://[::1]:4318/v1/traces"))
         (test-assert (telemetry--endpoint-p endpoint) "Accepted safe endpoint."))
       (dolist (endpoint '("http://example.org/v1/traces" "https://u:p@example.org/v1/traces"
                           "https://example.org/v1/traces?token=fixture" "https://example.org/v1/traces#x"
                           "https://example.org/traces" "file:///v1/traces"))
         (test-assert (not (telemetry--endpoint-p endpoint)) "Rejected unsafe endpoint."))))))

(defun test-telemetry-schema-and-runs ()
  "Metadata export has public aliases, exact counters, random roots and no raw payloads."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-model-aliases configuration) '(("private-local-model" . "model-a")))
     (telemetry-attach configuration)
     (let ((bodies nil) (reads 0) (first-run nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (test-assert (eq (first request) ':upload) "Metadata never calls the redactor.")
                 (push (fourth request) bodies) t)))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (setf first-run *telemetry-run*)
            (telemetry-call-with-run configuration
                                     (lambda () (test-assert (eq first-run *telemetry-run*)
                                                            "Nested provider calls reuse the ambient root.")))
            (telemetry-note-model :provider "openai" :request-model "private-local-model"
                                  :response-model "unmapped-private-model" :duration-ms 12
                                  :usage (json-object "input_tokens" 17 "output_tokens" 3
                                                      "cached_input_tokens" 4 "provider_secret" "usage-fixture"))
            (telemetry-note-model :provider "private-provider" :usage '(:output-tokens 7))
            (dotimes (i 2)
              (declare (ignore i))
              (telemetry-note-tool :tool "external.private-tool" :outcome "success" :duration-ms 2
                                   :arguments (json-object "secret" "private-argument")))
            (telemetry-note-report :tool "shell.run" :issue-kind "broken_tool"
                                 :summary-function (lambda () (incf reads) "raw-summary-fixture"))
            (telemetry-note-repair :target "self.redefine" :repair-kind "redefine_function" :outcome "applied")))
         (telemetry-call-with-run configuration
                                  (lambda () (telemetry-note-repair :target "self.set" :outcome "proposed"))))
       (test-assert (zerop reads) "Metadata-only consent never reads a summary.")
       (test-assert (= (length bodies) 2) "Exactly one bounded snapshot per root.")
       (let* ((spans (test-telemetry--spans (second bodies)))
              (independent-repair (aref (test-telemetry--spans (first bodies)) 0))
              (second-root (aref (test-telemetry--spans (first bodies)) 1))
              (model (aref spans 0)) (tool (aref spans 3)) (root (aref spans 6)))
         (test-assert (= (length (test-telemetry--attribute independent-repair "autolith.report.id")) 32)
                      "A repair without a preceding report has a fresh report correlation ID.")
         (test-assert (equal (test-telemetry--attribute (aref spans 4) "autolith.report.id")
                             (test-telemetry--attribute (aref spans 5) "autolith.report.id"))
                      "Reports and repairs share their run's report ID.")
         (test-assert (string= (test-telemetry--attribute model "gen_ai.request.model") "model-a")
                      "Only trusted aliases expose model identifiers.")
         (test-assert (string= (test-telemetry--attribute model "gen_ai.response.model") "custom")
                      "Unmapped models become custom.")
         (test-assert (string= (test-telemetry--attribute (aref spans 1) "gen_ai.provider.name") "unknown")
                      "Unknown providers are not exposed.")
         (test-assert (string= (test-telemetry--attribute tool "gen_ai.tool.name") "unknown")
                      "External tool names become unknown.")
         (test-assert (json-true-p (test-telemetry--attribute tool "autolith.tool.same_as_previous"))
                      "Consecutive local argument equality exports only a boolean.")
         (test-assert (equal (test-telemetry--attribute root "autolith.run.model_calls") "2")
                      "Every actual model attempt counts.")
         (test-assert (equal (test-telemetry--attribute root "autolith.run.tool_calls") "2")
                      "Every tool attempt counts.")
         (test-assert (not (equal (json-get root "traceId") (json-get second-root "traceId")))
                      "Independent runs have independent traces.")
         (loop for span across spans
               do (test-assert (= (length (json-get span "traceId")) 32) "Trace is random fixed-size hex.")
                  (test-assert (= (length (json-get span "spanId")) 16) "Span is random fixed-size hex.")
                  (test-assert (every #'digit-char-p (json-get span "startTimeUnixNano"))
                               "Timestamps use decimal Unix nanoseconds."))
         (dolist (private '("private-local-model" "unmapped-private-model" "private-provider"
                            "external.private-tool" "private-argument" "usage-fixture" "raw-summary-fixture"))
           (test-assert (notany (lambda (body) (search private body)) bodies) "Private metadata never reaches wire.")))
       (test-assert (null (telemetry-run-previous-digest first-run)) "Run-end disposes equality state.")))))

(defun test-telemetry-revocation ()
  "Owner revocation invalidates copied configuration, inflight results and old roots."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-diagnostics-p configuration) t)
     (telemetry-attach configuration)
     (let ((copy (configuration-copy configuration)) (uploads 0) (reads 0) (old-run nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (case (first request)
                   (:redact (setf (config :telemetry-enabled-p configuration) nil) "redacted fixture")
                   (:upload (incf uploads) t)))))
         (telemetry-call-with-run
          copy (lambda ()
                 (setf old-run *telemetry-run*)
                 (telemetry-note-tool :tool "shell.run" :arguments (json-object) :outcome "success")
                 (telemetry-note-report :summary-function (lambda () (incf reads) "fixture")))))
       (test-assert (= reads 1) "A consented summary is read once.")
       (test-assert (zerop uploads) "Revoked inflight results never upload.")
       (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "Revocation purges queued spans.")
       (test-assert (null (telemetry-controller-runs *telemetry-controller*)) "Revocation purges active roots.")
       (telemetry-attach copy)
       (telemetry-call-with-run copy (lambda () (test-assert (null *telemetry-run*) "A stale copy cannot re-enable.")))
       (setf (config :telemetry-enabled-p configuration) t)
       (telemetry-note-report :run old-run :summary-function (lambda () (incf reads) "fixture"))
       (test-assert (= reads 1) "Re-consent cannot resurrect stale run generations.")
       (let ((secondary (configuration-copy configuration :telemetry-enabled-p nil :telemetry-diagnostics-p nil)))
         (telemetry-attach secondary)
         (test-assert (telemetry-controller-enabled *telemetry-controller*)
                      "Attaching a default-off root does not revoke process consent.")
         (test-assert (eq (telemetry-consent-configuration secondary) configuration)
                      "Attached roots use the canonical consent configuration.")
         (setf (config :telemetry-diagnostics-p copy) nil)
         (test-assert (and (telemetry-controller-enabled *telemetry-controller*)
                           (not (telemetry-controller-diagnostics *telemetry-controller*)))
                      "Secondary diagnostic revocation preserves numerical telemetry.")
         (test-assert (null (config :telemetry-diagnostics-p configuration))
                      "Secondary revocation synchronizes the durable owner.")
         (setf (config :telemetry-enabled-p copy) nil)
         (test-assert (not (telemetry-controller-enabled *telemetry-controller*))
                      "Attached root revocation synchronously disables process telemetry.")
         (setf (config :telemetry-enabled-p copy) t)
         (test-assert (not (telemetry-controller-enabled *telemetry-controller*))
                      "An attached non-owner cannot re-enable revoked consent.")
         (setf (config :telemetry-enabled-p configuration) t)
         (setf (config :telemetry-enabled-p (telemetry-consent-configuration secondary)) nil)
         (test-assert (not (telemetry-controller-enabled *telemetry-controller*))
                      "Canonical consent writes revoke even when the secondary local value was already NIL.")
         (setf (config :telemetry-enabled-p configuration) t))
       (telemetry-call-with-quiescence
        (lambda ()
          (test-assert (null (telemetry--begin-run configuration)) "Checkpoint quiescence stops admissions.")))
       (let ((run (telemetry--begin-run configuration)))
         (test-assert run "Parent resumes fresh admissions after checkpoint.")
         (telemetry-quiesce)
         (telemetry-detach)
         (test-assert (null *telemetry-controller*) "Checkpoint child detaches process-owned state."))))))

(defun test-telemetry-redaction-failures ()
  "Native failure drops text, preserves numerical events, and bounds queue overflow."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-diagnostics-p configuration) t
           (config :telemetry-queue-limit configuration) 2)
     (telemetry-attach configuration)
     (let ((body nil) (redactions 0) (reads 0))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (case (first request)
                   (:redact (incf redactions) nil)
                   (:upload (setf body (fourth request)) nil)))))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (dotimes (i 4)
              (declare (ignore i))
              (telemetry-note-tool :tool "shell.run" :arguments (json-object) :outcome "error"))
            (telemetry-note-report :summary-function (lambda () (incf reads) "private summary"))
            (telemetry-note-repair :target "self.redefine" :repair-kind "redefine_function" :outcome "failed"
                                   :summary-function (lambda () (incf reads) "private replacement")))))
       (test-assert (= reads redactions 2) "Each diagnostic goes through native redaction.")
       (test-assert (= (length (test-telemetry--spans body)) 2) "Overflow retains only the bounded tail.")
       (test-assert (not (search "autolith.diagnostic.summary" body)) "Redactor failure never falls back to text.")
       (test-assert (not (search "private" body)) "Failed raw text never enters the queue.")
       (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "Failed upload drops its snapshot."))
     (setf (config :telemetry-queue-limit configuration) 128)
     (let ((body nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (case (first request)
                   (:redact (test-assert (not (search "fixture@example.org" (second request)))
                                          "Deterministic credential defense precedes native inference.")
                            "[PERSON] at fixture@example.org token=fixture-token /private/home/file")
                   (:upload (setf body (fourth request)) t)))))
         (telemetry-call-with-run configuration
                                  (lambda () (telemetry-note-report
                                              :summary-function (lambda () "Person fixture@example.org token=fixture-token")))))
       (test-assert (search "autolith.redaction.engine" body) "Successful native output has its marker.")
       (test-assert (not (search "fixture@example.org" body)) "Post-redaction defense masks email.")
       (test-assert (not (search "fixture-token" body)) "Post-redaction defense masks credentials.")
       (test-assert (not (search "/private/home" body)) "Post-redaction defense masks paths.")))))

;;;; -- Supervision and Authentication --

(defun test-telemetry-subprocess-deadlines ()
  "Deadline and revocation terminate an actual isolated child, without HTTP or native FFI."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-timeout-ms configuration) 100)
     (telemetry-attach configuration)
     (let ((original (symbol-function 'uiop:launch-program)) (process nil))
       (test-call-with-function-replacements
        (list (list 'uiop:launch-program
                    (lambda (command &rest options)
                      (declare (ignore command))
                      (setf process
                            (apply original
                                   (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--non-interactive"
                                         "--eval" "(sleep 30)") options)))))
        (lambda ()
          (let* ((run (telemetry--begin-run configuration)) (start (get-internal-real-time)))
            (test-assert (null (telemetry--process run '(:redact "fixture" "en" nil nil)))
                         "Hung child times out without unsafe native interruption.")
            (test-assert (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 3)
                         "Hard deadline returns within a bounded allowance.")
            (test-assert (not (uiop:process-alive-p process)) "Timeout kills and reaps the child."))
          (setf (config :telemetry-timeout-ms configuration) 5000)
          (let* ((run (telemetry--begin-run configuration))
                 (thread (bt:make-thread (lambda () (telemetry--process run '(:redact "fixture" "en" nil nil)))
                                         :name "test inflight telemetry")))
            (unwind-protect
                 (progn
                   (loop repeat 400
                         when (bt:with-lock-held ((telemetry-controller-lock *telemetry-controller*))
                                (not (null (telemetry-controller-processes *telemetry-controller*))))
                           do (return)
                         do (sleep 0.005))
                   (test-assert (telemetry-controller-processes *telemetry-controller*)
                                "Inflight child is owned by the process controller.")
                   (setf (config :telemetry-enabled-p configuration) nil)
                   (test-assert (not (uiop:process-alive-p process)) "Consent revocation kills child synchronously.")
                   (test-assert (null (telemetry-controller-processes *telemetry-controller*)) "No live children after revocation."))
              (bt:join-thread thread)))))
       ;; Exercise the actual saved-core argv and startup dispatch, without replacements.
       (setf (config :telemetry-enabled-p configuration) t
             (config :telemetry-timeout-ms configuration) 10000)
       (let ((run (telemetry--begin-run configuration)) (*telemetry-process-function* nil))
         (test-assert (eq (telemetry--process run '(:ping)) ':ready)
                      "Real saved-core worker dispatch returns a readiness response."))))))

(defun test-telemetry-token-boundary ()
  "Only the upload child reads a private token file; redirects and verbose diagnostics are disabled."
  (test-telemetry--call
   (lambda (configuration)
     (let ((file (merge-pathnames "fixture-token" (config :data-root configuration))) (headers nil))
       (ensure-directories-exist file)
       (with-open-file (stream file :direction :output :if-exists :supersede)
         (write-string "fixture-authentication-value" stream))
       (platform-make-private *platform* file)
       (test-call-with-function-replacements
        (list (list 'dex:request
                    (lambda (endpoint &rest options)
                      (declare (ignore endpoint))
                      (test-assert (zerop (getf options :max-redirects)) "Redirects are disabled.")
                      (test-assert (not (getf options :insecure)) "HTTPS certificates are verified.")
                      (test-assert (not (getf options :verbose)) "HTTP diagnostics are suppressed.")
                      (test-assert (not (search "fixture-authentication-value" (getf options :content)))
                                   "Token is absent from the OTLP body.")
                      (setf headers (getf options :headers))
                      (values (make-string-input-stream "") 200))))
        (lambda ()
          (telemetry--worker-upload (list :upload "http://127.0.0.1:4318/v1/traces" file "{}"))))
       (test-assert (equal (cdr (assoc "Authorization" headers :test #'string=))
                           "Bearer fixture-authentication-value") "Only authentication headers contain the token.")
       (setf (config :telemetry-enabled-p configuration) t
             (config :telemetry-token-file configuration) file)
       (telemetry-attach configuration)
       (let ((*telemetry-process-function* (lambda (run request) (declare (ignore run request)) t)))
         (test-call-with-function-replacements
          (list (list 'telemetry--read-token (lambda (path) (declare (ignore path))
                                              (error "Parent read token contents."))))
          (lambda () (telemetry-call-with-run configuration (lambda () :done)))))
       (test-assert (not (test-object-contains-string-p (telemetry-controller-owner *telemetry-controller*)
                                                       "fixture-authentication-value"))
                    "Owner configuration does not retain token contents.")))))

(defun test-telemetry-real-redactor ()
  "Explicit opt-in actual-model test; uploads are always mocked."
  (when (equal (uiop:getenv "AUTOLITH_TEST_TELEMETRY_MODEL") "1")
    (test-telemetry--call
     (lambda (configuration)
       (setf (config :telemetry-enabled-p configuration) t
             (config :telemetry-diagnostics-p configuration) t
             (config :telemetry-timeout-ms configuration) 10000)
       (telemetry-attach configuration)
       (let ((body nil) (process-function (symbol-function 'telemetry--process)))
         (let ((*telemetry-process-function*
                 (lambda (run request)
                   (if (eq (first request) ':upload)
                       (progn (setf body (fourth request)) t)
                       (let ((*telemetry-process-function* nil)) (funcall process-function run request))))))
           (telemetry-call-with-run
            configuration (lambda () (telemetry-note-report
                                      :summary-function (lambda () "Alice Smith lives in Amsterdam.")))))
         (test-assert (and body (search "autolith.redaction.engine" body))
                      "Explicit real model integration succeeds.")
         (test-assert (not (search "Alice Smith" body)) "Actual model removes the person's name."))))))
