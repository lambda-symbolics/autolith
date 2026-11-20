(in-package #:autolith)

;;;; -- Consent and Schema Boundaries --

(-> test-telemetry--call
    (function &key (:telemetry-defaults-p boolean) (:background-uploads-p boolean)) t)
(defun test-telemetry--call (function &key telemetry-defaults-p background-uploads-p)
  "Run a private telemetry fixture; projection tests use deterministic upload scheduling.
BACKGROUND-UPLOADS-P selects the real scheduler for upload lifecycle tests."
  (let ((*model-catalog-fetch-function* (lambda (run) (declare (ignore run)) nil)))
    (let* ((configuration (test-configuration :telemetry-defaults-p telemetry-defaults-p))
           (root          (test-configuration-root configuration)))
      (telemetry-shutdown)
      (unwind-protect
           (if background-uploads-p
               (funcall function configuration)
               (test-call-with-function-replacements
                (list (list 'telemetry--schedule-upload (lambda (controller) (declare (ignore controller)) nil))
                      (list 'telemetry-flush
                            (lambda (&optional (run *telemetry-run*))
                              (when run
                                (telemetry--upload-batch run))
                              nil)))
                (lambda () (funcall function configuration))))
        (telemetry-shutdown)
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))))

(-> test-telemetry--spans (string) vector)
(defun test-telemetry--spans (body)
  "Decode the projected spans of a captured mock upload."
  (let* ((envelope (json-decode body))
         (resource (aref (json-get envelope "resourceSpans") 0))
         (scope    (aref (json-get resource "scopeSpans") 0)))
    (json-get scope "spans")))

(-> test-telemetry--attribute (hash-table string) t)
(defun test-telemetry--attribute (span name)
  "Return a captured OTLP scalar attribute."
  (let* ((attribute (find name (json-get span "attributes")
                          :key (lambda (entry) (json-get entry "key")) :test #'string=))
         (value     (and attribute (json-get attribute "value"))))
    (when value
      (or (json-get value "stringValue") (json-get value "intValue")
          (json-get value "boolValue")))))

(-> test-telemetry--collector-fixture (string t) null)
(defun test-telemetry--collector-fixture (name value)
  "Optionally emit actual native OTLP for the collector's cross-repository contract tests."
  (let ((directory (uiop:getenv "AUTOLITH_TELEMETRY_FIXTURE_DIRECTORY")))
    (when directory
      (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
        (ensure-directories-exist path)
        (with-open-file (stream path :direction ':output :if-exists ':supersede :external-format :utf-8)
          (write-string (json-encode value) stream)
          (terpri stream)))))
  nil)

(-> test-telemetry--launch-stalled-child (function list pathname) t)
(defun test-telemetry--launch-stalled-child (launch options ready-path)
  "Launch a stock-core child and wait boundedly for its pre-sleep readiness file.
The caller owns termination. Keep startup stderr beside READY-PATH for failures."
  (when (probe-file ready-path)
    (delete-file ready-path))
  (ensure-directories-exist ready-path)
  (setf (getf options :error-output) (make-pathname :type "stderr" :defaults ready-path)
        (getf options :if-error-output-exists) ':supersede)
  (let* ((core     (merge-pathnames "sbcl.core" (uiop:lisp-implementation-directory)))
         (process
           (apply launch
                  (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                        "--core" (namestring core)
                        "--no-sysinit" "--no-userinit" "--non-interactive"
                        "--eval"
                        (format nil "(progn
  (with-open-file (stream ~S :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create)
    (write-line ~S stream))
  (sleep 30))"
                                (namestring ready-path) "ready"))
                  options))
         (deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
    (loop until (or (probe-file ready-path)
                    (not (uiop:process-alive-p process))
                    (>= (get-internal-real-time) deadline))
          do (sleep 0.005))
    process))

(-> test-telemetry--stalled-child-diagnostic (t pathname) string)
(defun test-telemetry--stalled-child-diagnostic (process ready-path)
  "Describe a stall fixture without waiting for a live child or reading unbounded stderr."
  (let ((alive-p     (and process (uiop:process-alive-p process)))
        (stderr-path (make-pathname :type "stderr" :defaults ready-path)))
    (format nil "Stalled child: ready ~S, alive ~S, exit ~S, stock core ~S, stderr ~S"
            (not (null (probe-file ready-path))) alive-p
            (when (and process (not alive-p)) (uiop:wait-process process))
            (merge-pathnames "sbcl.core" (uiop:lisp-implementation-directory))
            (unless alive-p
              (with-open-file (stream stderr-path :external-format   ':utf-8
                                                  :if-does-not-exist nil)
                (when stream
                  (let ((buffer (make-string 4096)))
                    (subseq buffer 0 (read-sequence buffer stream)))))))))

(-> test-telemetry-span-applicability () null)
(defun test-telemetry-span-applicability ()
  "Keep non-applicable model, tool, role and error metadata absent on the wire."
  (test-telemetry--call
   (lambda (configuration)
     (let ((bodies                       nil)
           (*telemetry-process-function* nil))
       (setf (config :telemetry-enabled-p configuration) t)
       (telemetry-attach configuration)
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (push (third request) bodies)
                 t)))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (telemetry-note-model :provider "openai" :request-model "gpt-e2e-unlisted-9"
                                  :role "agent" :outcome "error" :error-type "authentication")
            (telemetry-note-tool :tool "resource.read" :outcome "success")
            (telemetry-note-tool :tool "resource.read" :outcome "permission_denied"
                                 :error-code ':access-denied))))
       (loop for span across (test-telemetry--spans (first bodies))
             for kind = (test-telemetry--attribute span "autolith.event.kind")
             do (cond
                  ((equal kind "tool")
                   (dolist (key '("autolith.request.role" "gen_ai.request.model" "gen_ai.response.model"))
                     (test-assert (null (test-telemetry--attribute span key))
                                  "Tool spans omit model and request-role attributes."))
                   (test-assert (equal (test-telemetry--attribute span "gen_ai.tool.name") "resource.read")
                                "Applicable tool identity survives.")
                   (test-assert
                    (equal (test-telemetry--attribute span "error.type")
                           (when (equal (test-telemetry--attribute span "autolith.tool.outcome") "permission_denied")
                             "permission"))
                    "Successful tools omit error types; failed tools keep their real category."))
                  ((equal kind "model")
                   (test-assert (null (test-telemetry--attribute span "gen_ai.tool.name"))
                                "Model spans omit the non-applicable tool attribute.")
                   (test-assert (equal (test-telemetry--attribute span "autolith.request.role") "agent")
                                "Model requests keep their applicable role.")
                   (test-assert (equal (test-telemetry--attribute span "gen_ai.request.model") "custom")
                                "Model requests keep their projected model category.")
                   (test-assert (equal (test-telemetry--attribute span "error.type") "authentication")
                                "Failed model requests keep their applicable error type.")))))))
  nil)

(-> test-telemetry-run-finalization-once () null)
(defun test-telemetry-run-finalization-once ()
  "Claim root finalization before upload, including reentrant failure cleanup."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((bodies      nil)
           (reentered-p nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (push (third request) bodies)
                 (unless reentered-p
                   (setf reentered-p t)
                   (telemetry-finish-run run (telemetry-run-owner run) '(:state :completed)))
                 t)))
         (let ((run (telemetry--begin-run configuration)))
           (telemetry-finish-run run (telemetry-run-owner run) '(:state :failed :error-type :provider))
           (telemetry-finish-run run (telemetry-run-owner run) '(:state :completed))))
       (test-assert reentered-p "The upload exercises competing terminal cleanup.")
       (test-assert (= (length bodies) 1) "Finalization exports one root exactly once.")
       (test-assert (equal (test-telemetry--attribute (aref (test-telemetry--spans (first bodies)) 0)
                                                    "autolith.run.outcome")
                           "error")
                    "A later finalizer cannot overwrite the first terminal outcome."))))
  nil)

(-> test-telemetry-category-projection () null)
(defun test-telemetry-category-projection ()
  "Omit unavailable categories and normalize observed reasons at the wire boundary."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((cases  '((nil nil) ("unknown" nil) ("private reason" nil)
                      ("STOP" "stop") ("MAX_TOKENS" "max_tokens")
                      ("MODEL_CONTEXT_WINDOW_EXCEEDED" "model_context_window_exceeded")
                      ("safety" "SAFETY") ("recitation" "RECITATION")
                      ("other" "OTHER") ("malformed_function_call" "MALFORMED_FUNCTION_CALL")))
            (bodies (test-telemetry-hooks--capture
                     configuration
                     (lambda ()
                       (telemetry-call-with-run
                        configuration
                        (lambda ()
                          (dolist (case cases)
                            (telemetry-note-model :finish-reason (first case)))
                          (telemetry-note-model :outcome "private" :error-type "none"
                                                :role "private" :provider "private")
                          (telemetry-note-tool)
                          (telemetry-note-report)
                          (telemetry-note-repair)
                          (telemetry-note-repair :event-kind ':check
                                                 :attempt-id "11111111111111111111111111111111")
                          (telemetry-note-repair :event-kind ':check :verification-scope ':none
                                                 :attempt-id "22222222222222222222222222222222"
                                                 :issue-kind ':other :repair-kind ':other
                                                 :outcome ':verified :verified t))))))
            (body   (first bodies))
            (models (test-telemetry-hooks--kind body "model"))
            (checks (test-telemetry-hooks--kind body "check")))
       (loop for span in models for case in cases
             do (test-assert (equal (test-telemetry--attribute span "gen_ai.response.finish_reason")
                                    (second case))
                             "Finish reasons use normalized catalog spellings."))
       (dolist (span (append models
                            (test-telemetry-hooks--kind body "tool")
                            (test-telemetry-hooks--kind body "report")
                            (test-telemetry-hooks--kind body "repair")
                            (list (first checks))))
         (dolist (key '("autolith.model.outcome" "autolith.tool.outcome" "autolith.request.role"
                        "error.type" "gen_ai.provider.name" "gen_ai.request.model" "gen_ai.response.model"
                        "gen_ai.tool.name" "autolith.issue.kind" "autolith.repair.target"
                        "autolith.repair.operation" "autolith.repair.kind" "autolith.repair.outcome"
                        "autolith.verification.scope"))
           (test-assert (not (find key (json-get span "attributes")
                                   :key (lambda (attribute) (json-get attribute "key")) :test #'equal))
                        "Unavailable attributes are absent, including their OTLP keys.")))
       (loop for (key expected) in '(("autolith.verification.scope" "none")
                                     ("autolith.issue.kind" "other")
                                     ("autolith.repair.kind" "other")
                                     ("autolith.repair.outcome" "verified"))
             do (test-assert (equal (test-telemetry--attribute (second checks) key) expected)
                             "Explicit protocol categories retain their meaning.")))))
  nil)

(-> test-telemetry-verification-scope () null)
(defun test-telemetry-verification-scope ()
  "Assert verification only for passing checks with focused or full scope."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((attempt-id "0123456789abcdef0123456789abcdef")
            (cases      '((:check nil :verified t nil)
                          (:check :none :verified t nil)
                          (:check :focused :verified t t)
                          (:check :full :verified t t)
                          (:check :invalid :verified t nil)
                          (:check :focused :failed t nil)
                          (:check :full :verified nil nil)
                          (:repair :full :verified t nil)))
            (body       (first
                         (test-telemetry-hooks--capture
                          configuration
                          (lambda ()
                            (telemetry-call-with-run
                             configuration
                             (lambda ()
                               (loop for (kind scope outcome verified) in cases
                                     do (telemetry-note-repair :event-kind kind :attempt-id attempt-id
                                                               :verification-scope scope :outcome outcome
                                                               :verified verified))))))))
            (spans      (remove-if-not
                         (lambda (span)
                           (member (test-telemetry--attribute span "autolith.event.kind")
                                   '("check" "repair") :test #'equal))
                         (coerce (test-telemetry--spans body) 'list))))
       (test-assert (= (length spans) (length cases)) "Every verification case exports a span.")
       (loop for span in spans
             for (kind scope nil nil expected) in cases
             do (test-assert (eq (json-true-p (test-telemetry--attribute span "autolith.repair.verified"))
                                expected)
                             "Verification requires a passing check, an explicit verdict and real scope.")
                (test-assert (equal (test-telemetry--attribute span "autolith.verification.scope")
                                    (and (eq kind ':check)
                                         (case scope (:none
                                                      "none")
                                                     (:focused
                                                      "focused")
                                                     (:full
                                                      "full"))))
                             "Missing and invalid scopes are omitted; explicit scopes survive.")
                (test-assert (equal (test-telemetry--attribute span "autolith.repair.attempt.id") attempt-id)
                             "Check spans carry the valid attempt linkage required by the collector."))
       (test-telemetry--collector-fixture "native-verification-scope.json" (json-decode body)))))
  nil)

(-> test-telemetry-catalog-reload () null)
(defun test-telemetry-catalog-reload ()
  "Export the current catalog after replacement without reloading the exporter."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((*telemetry-finish-reasons*
              '((:stop "stop" "STOP") (:catalog-reload "catalog-reload" "RELOADED")))
            (body  (first
                    (test-telemetry-hooks--capture
                     configuration
                     (lambda ()
                       (telemetry-call-with-run
                        configuration
                        (lambda ()
                          (dolist (reason '("stop" "catalog-reload" "completed"))
                            (telemetry-note-model :finish-reason reason))))))))
            (spans (test-telemetry-hooks--kind body "model")))
       (test-assert (= (length spans) 3) "All model requests reach the exporter.")
       (loop for span in spans
             for expected in '("STOP" "RELOADED" nil)
             do (test-assert (equal (test-telemetry--attribute span "gen_ai.response.finish_reason") expected)
                             "Changed, added and removed categories use the current catalog.")))))
  nil)

(-> test-telemetry-model-collector-contract () null)
(defun test-telemetry-model-collector-contract ()
  "Export private model names as categories with independent public-membership flags."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (let ((*model-catalog-fetch-function*
             (lambda (run)
               (declare (ignore run))
               (test-model-catalog--fixture '("gpt-4.1")))))
       (telemetry-attach configuration)
       (test-model-catalog--await *telemetry-controller*))
     (let ((fixtures nil)
           (names    '("jane-doe-private-client" "client_acme/internal"
                       "john@example.com/model" "/Users/fixture/model"
                       "token=fixture-secret" "127.0.0.1:8000/model"
                       "private.internal:8443/model" "John Smith model"
                       "gpt?4.1" "GPT-4.1")))
       (dolist (name names)
         (dolist (request-p '(nil t))
           (let* ((bodies   nil)
                  (*telemetry-process-function*
                    (lambda (run request)
                      (declare (ignore run))
                      (push (third request) bodies)
                      t))
                  (request  (if request-p name "gpt-4.1"))
                  (response (if request-p "gpt-4.1" name)))
             (telemetry-call-with-run
              configuration
              (lambda ()
                (telemetry-note-model :request-model request :response-model response :provider "openai")))
             (test-assert (not (search name (first bodies)))
                          "Private model names never enter an exported envelope.")
             (let ((fixture (json-object "request_model" (if request-p "custom" "gpt-4.1")
                                        "response_model" (if request-p "gpt-4.1" "custom")
                                        "request_known" (json-boolean (not request-p))
                                        "response_known" (json-boolean request-p)
                                        "envelope" (json-decode (first bodies)))))
               (loop for span across (test-telemetry--spans (first bodies))
                     do (test-assert
                         (and (equal (test-telemetry--attribute span "gen_ai.request.model")
                                     (json-get fixture "request_model"))
                              (equal (test-telemetry--attribute span "gen_ai.response.model")
                                     (json-get fixture "response_model"))
                              (eq (json-true-p (test-telemetry--attribute span "autolith.model.known"))
                                  (not request-p))
                              (eq (json-true-p (test-telemetry--attribute span "autolith.model.response_known"))
                                  request-p))
                         "Model and run spans distinguish public IDs from private categories."))
               (push fixture fixtures)))))
       (test-telemetry--collector-fixture "native-models.json" (coerce (nreverse fixtures) 'vector)))))
  nil)

(-> test-telemetry-public-models () null)
(defun test-telemetry-public-models ()
  "Export exact public names, explicit aliases, and categories for unlisted IDs."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-model-aliases configuration)
           '(("gpt-6.1-sol" . "owner-public-alias")
             ("private-local-model" . "local-public-alias")))
     (let ((*model-catalog-fetch-function*
             (lambda (run)
               (declare (ignore run))
               (test-model-catalog--fixture
                '("gpt-6.1-sol" "gpt-5.4" "claude-opus-5"
                  "gemini-3.5-flash" "grok-4.7" "accounts/fireworks/models/kimi-k3")))))
       (telemetry-attach configuration)
       (test-model-catalog--await *telemetry-controller*))
     (let ((bodies nil)
           (cases  '(("gpt-6.1-sol" "owner-public-alias" t)
                     ("gpt-5.4" "gpt-5.4" t)
                     ("claude-opus-5" "claude-opus-5" t)
                     ("gemini-3.5-flash" "gemini-3.5-flash" t)
                     ("grok-4.7" "grok-4.7" t)
                     ("accounts/fireworks/models/kimi-k3" "accounts/fireworks/models/kimi-k3" t)
                     ("private-local-model" "local-public-alias" t)
                     (nil nil nil)
                     ("" nil nil)
                     (42 nil nil)
                     ("private-response-model" "custom" nil)
                     ("gpt-6.1-sol-private" "custom" nil)
                     ("GPT-5.4" "custom" nil)
                     ("tenant/gemini-3.5-flash" "custom" nil)
                     ("claude-opus-5 " "custom" nil))))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (push (third request) bodies)
                 t)))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (dolist (case cases)
              (telemetry-note-model :provider       "openai"
                                    :request-model  (first case)
                                    :response-model (first case))))))
       (let ((models (remove-if-not
                      (lambda (span)
                        (equal (test-telemetry--attribute span "autolith.event.kind") "model"))
                      (coerce (test-telemetry--spans (first bodies)) 'list))))
         (test-assert (= (length models) (length cases)) "Every model attempt is exported.")
         (loop for span in models
               for (name expected known-p) in cases
               do (dolist (attribute '("gen_ai.request.model" "gen_ai.response.model"))
                    (test-assert (equal (test-telemetry--attribute span attribute) expected)
                                 (format nil "Model ~S projects as ~A for ~A." name expected attribute)))
                  (dolist (attribute '("autolith.model.known" "autolith.model.response_known"))
                    (test-assert (eq (json-true-p (test-telemetry--attribute span attribute)) known-p)
                                 "Public membership and explicit aliases determine the known flags.")))))))
  nil)

(-> test-telemetry-run-model-identity () null)
(defun test-telemetry-run-model-identity ()
  "Populate root identities from agent calls while preserving mixed and missing values."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-model-aliases configuration)
           '(("main" . "model-a") ("alternate" . "model-b") ("helper" . "helper")))
     (dolist (case '((() nil nil nil)
                     ((("main" "main" "agent") ("helper" "helper" "permission_classifier"))
                      "openai" "model-a" "model-a")
                     ((("helper" "helper" "title_generation") ("main" nil "agent"))
                      "openai" "model-a" nil)
                     ((("main" "main" "agent") ("alternate" "alternate" "agent"))
                      "openai" "mixed" "mixed")
                     ((("main" "main" nil) ("alternate" "alternate" "agent"))
                      "openai" "mixed" "mixed")
                     ((("helper" "helper" "inference")) "openai" "helper" "helper")))
       (let* ((bodies
                (test-telemetry-hooks--capture
                 configuration
                 (lambda ()
                   (telemetry-call-with-run
                    configuration
                    (lambda ()
                      (dolist (attempt (first case))
                        (telemetry-note-model :provider "openai" :request-model (first attempt)
                                              :response-model (second attempt) :role (third attempt))))))))
              (root (first (test-telemetry-hooks--kind (first bodies) "run"))))
         (loop for key in '("gen_ai.provider.name" "gen_ai.request.model" "gen_ai.response.model")
               for expected in (rest case)
               do (test-assert (equal (test-telemetry--attribute root key) expected)
                               "Root metadata describes primary attempts, including mixed identities."))))))
  nil)

(-> test-telemetry-model-attributes-golden () null)
(defun test-telemetry-model-attributes-golden ()
  "Pin complete typed model and run attributes, including zero and omitted usage."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-model-aliases configuration) '(("main" . "model-a")))
     (let* ((body     (first
                       (test-telemetry-hooks--capture
                        configuration
                        (lambda ()
                          (telemetry-call-with-run
                           configuration
                           (lambda ()
                             (telemetry-note-model
                              :provider "openai" :request-model "main"
                              :response-model "private-response" :role "agent"
                              :outcome "success" :finish-reason "STOP"
                              :usage '(:input-tokens 10 :output-tokens 3
                                       :cached-input-tokens 0 :cache-creation-input-tokens 2))
                             (telemetry-note-model
                              :provider "openai" :request-model "main"
                              :response-model "private-response" :role "agent"
                              :outcome "error" :error-type "provider")))))))
            (spans    (test-telemetry--spans body))
            (root-id  (json-get (aref spans 2) "spanId"))
            (common   (list (list "autolith.run.span_id" "stringValue" root-id)))
            (identity '(("gen_ai.provider.name" "stringValue" "openai")
                        ("gen_ai.request.model" "stringValue" "model-a")
                        ("gen_ai.response.model" "stringValue" "custom")
                        ("autolith.model.known" "boolValue" t)
                        ("autolith.model.response_known" "boolValue" nil)))
            (expected
              (list
               (append '(("autolith.event.kind" "stringValue" "model")) common
                       '(("autolith.usage.cache_write_tokens" "intValue" "2")
                         ("autolith.usage.cache_read_tokens" "intValue" "0")
                         ("gen_ai.usage.output_tokens" "intValue" "3")
                         ("gen_ai.usage.input_tokens" "intValue" "10"))
                       identity
                       '(("autolith.request.role" "stringValue" "agent")
                         ("autolith.model.outcome" "stringValue" "success")
                         ("gen_ai.response.finish_reason" "stringValue" "stop")))
               (append '(("autolith.event.kind" "stringValue" "model")) common identity
                       '(("autolith.request.role" "stringValue" "agent")
                         ("autolith.model.outcome" "stringValue" "error")
                         ("error.type" "stringValue" "provider")))
               (append '(("autolith.event.kind" "stringValue" "run")) common identity
                       '(("autolith.run.outcome" "stringValue" "success")
                         ("autolith.run.attempt" "intValue" "1")
                         ("autolith.run.recovered" "boolValue" nil)
                         ("autolith.run.tool_calls" "intValue" "0")
                         ("autolith.run.model_calls" "intValue" "2"))))))
       (test-assert (= (length spans) (length expected)) "All attempts and the root are exported.")
       (loop for span across spans
             for attributes in expected
             do (test-assert
                 (equal (loop for attribute across (json-get span "attributes")
                              for scalar = (json-get attribute "value")
                              append (loop for type being the hash-keys of scalar
                                           using (hash-value value)
                                           collect (list (json-get attribute "key") type
                                                         (if (equal type "boolValue")
                                                             (json-true-p value) value))))
                        attributes)
                 "Complete ordered attribute names, scalar types and values match the golden export.")))))
  nil)

(-> test-telemetry-default-on () null)
(defun test-telemetry-default-on ()
  "Default telemetry exports metadata; diagnostics and persisted opt-outs stay disabled."
  (test-telemetry--call
   (lambda (configuration)
     (let ((uploads nil) (reads 0))
       (test-assert (eq (config :telemetry-enabled-p configuration) t) "Telemetry defaults on.")
       (test-assert (null (config :telemetry-diagnostics-p configuration)) "Diagnostics default off.")
       (telemetry-attach configuration)
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (test-assert (eq (first request) ':upload) "Default telemetry never invokes redaction.")
                 (push (third request) uploads)
                 t)))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (telemetry-note-report :summary-function (lambda () (incf reads) "fixture")))))
       (test-assert (= (length uploads) 1) "A default-enabled run uploads metadata.")
       (test-assert (zerop reads) "Diagnostics require separate opt-in.")
       (with-test-environment (("AUTOLITH_TELEMETRY" "off"))
         (test-assert (null (config :telemetry-enabled-p (preferences-tests--create configuration)))
                      "The process environment can opt out without a saved setting."))
       (with-test-environment (("AUTOLITH_TELEMETRY" nil))
         (test-assert (eq (config :telemetry-enabled-p (preferences-tests--create configuration)) t)
                      "Process configuration also defaults on without a saved setting.")
         (preferences-store configuration ':telemetry-enabled-p nil)
         (let ((loaded (preferences-tests--create configuration)))
           (test-assert (null (config :telemetry-enabled-p loaded)) "A saved opt-out overrides the default.")
           (telemetry-shutdown)
           (telemetry-attach loaded)
           (test-assert (null (telemetry--begin-run loaded)) "A saved opt-out prevents new runs.")))))
   :telemetry-defaults-p t)
  nil)

(-> test-telemetry-disabled () null)
(defun test-telemetry-disabled ()
  "An opt-out does not allocate IDs, inspect text, invoke native work or upload."
  (test-telemetry--call
   (lambda (configuration)
     (let ((calls                        0)
           (*telemetry-process-function* (lambda (&rest arguments)
                                          (declare (ignore arguments))
                                          (error "Unexpected telemetry process."))))
       (telemetry-attach configuration)
       (test-assert (null (config :telemetry-enabled-p configuration)) "Telemetry is explicitly disabled.")
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
         (test-assert (not (telemetry--endpoint-p endpoint)) "Rejected unsafe endpoint.")))))
  nil)

(-> test-telemetry-schema-and-runs () null)
(defun test-telemetry-schema-and-runs ()
  "Metadata export has public aliases, exact counters, random roots and no raw payloads."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-model-aliases configuration) '(("private-local-model" . "model-a")))
     (telemetry-attach configuration)
     (let ((bodies nil) (reads 0) (first-run nil) (report-id nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (test-assert (eq (first request) ':upload) "Metadata never calls the redactor.")
                 (push (third request) bodies) t)))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (setf first-run *telemetry-run*)
            (setf report-id (telemetry-new-report-id))
            (telemetry-call-with-run configuration
                                     (lambda () (test-assert (eq first-run *telemetry-run*)
                                                            "Nested provider calls reuse the ambient root.")))
            (telemetry-note-model :provider "openai" :request-model "private-local-model"
                                  :response-model "unmapped-private-model" :duration-ms 12
                                  :usage (json-object "input_tokens" 17 "output_tokens" 3
                                                      "cached_input_tokens" 4 "provider_secret" "usage-fixture"))
            (telemetry-note-model :provider "private-provider" :usage '(:output-tokens 7))
            (loop repeat 2
                  do (telemetry-note-tool :tool "external.private-tool" :outcome "success" :duration-ms 2
                                          :arguments (json-object "secret" "private-argument")))
            (telemetry-note-report :report-id report-id :tool "shell.run" :issue-kind ':broken-tool
                                 :summary-function (lambda () (incf reads) "raw-summary-fixture"))
            (telemetry-note-repair :report-id report-id :target "self.define" :repair-kind "redefine_function" :outcome "applied")))
         (telemetry-call-with-run configuration
                                  (lambda () (telemetry-note-repair :target "self.set" :outcome "proposed"))))
       (test-assert (zerop reads) "Metadata-only consent never reads a summary.")
       (test-assert (= (length bodies) 2) "Exactly one bounded snapshot per root.")
       (let* ((spans              (test-telemetry--spans (second bodies)))
              (independent-repair (aref (test-telemetry--spans (first bodies)) 0))
              (second-root        (aref (test-telemetry--spans (first bodies)) 1))
              (model (aref spans 0)) (tool (aref spans 3)) (root (aref spans 6)))
         (loop for span across spans
               when (equal (test-telemetry--attribute span "autolith.event.kind") "tool")
                 do (loop for attribute across (json-get span "attributes")
                          for name = (json-get attribute "key")
                          do (test-assert (not (or (uiop:string-prefix-p "autolith.issue." name)
                                                  (uiop:string-prefix-p "autolith.report." name)))
                                          "Ordinary tool rows export no issue or report attributes.")))
         (test-assert (= (length (test-telemetry--attribute independent-repair "autolith.report.id")) 32)
                      "A repair without a preceding report has a fresh report correlation ID.")
         (test-assert (equal (test-telemetry--attribute (aref spans 4) "autolith.report.id")
                             (test-telemetry--attribute (aref spans 5) "autolith.report.id"))
                      "An explicit report identity links its repair.")
         (test-assert (string= (test-telemetry--attribute model "gen_ai.request.model") "model-a")
                      "Trusted aliases take precedence.")
         (test-assert (string= (test-telemetry--attribute model "gen_ai.response.model") "custom")
                      "Unmapped models export the private-model category.")
         (test-assert (not (json-true-p (test-telemetry--attribute model "autolith.model.response_known")))
                      "An unlisted returned identity is explicitly unknown to the catalog.")
         (test-assert (null (test-telemetry--attribute (aref spans 1) "gen_ai.provider.name"))
                      "Unknown providers are not exposed.")
         (test-assert (null (test-telemetry--attribute tool "gen_ai.tool.name"))
                      "Uncatalogued tool identities are omitted.")
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
         (dolist (private '("private-local-model" "private-provider"
                            "external.private-tool" "private-argument" "usage-fixture" "raw-summary-fixture"))
           (test-assert (notany (lambda (body) (search private body)) bodies) "Private metadata never reaches wire.")))
       (test-assert (null (telemetry-run-previous-digest first-run)) "Run-end disposes equality state."))))
  nil)

(-> test-telemetry-revocation () null)
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
                   (:redact
                    (setf (config :telemetry-enabled-p configuration) nil) "redacted fixture")
                   (:upload
                    (incf uploads) t)))))
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
         (test-assert (null *telemetry-controller*) "Checkpoint child detaches process-owned state.")))))
  nil)

(-> test-telemetry-redaction-failures () null)
(defun test-telemetry-redaction-failures ()
  "Native failure drops diagnostic text without blocking event upload."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t
           (config :telemetry-diagnostics-p configuration) t)
     (telemetry-attach configuration)
     (let ((body nil) (redactions 0) (reads 0))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (case (first request)
                   (:redact
                    (incf redactions) nil)
                   (:upload
                    (setf body (third request)) nil)))))
         (telemetry-call-with-run
          configuration
          (lambda ()
            (loop repeat 4
                  do (telemetry-note-tool :tool "shell.run" :arguments (json-object) :outcome "error"))
            (telemetry-note-report :summary-function (lambda () (incf reads) "private summary"))
            (telemetry-note-repair :target "self.define" :repair-kind "redefine_function" :outcome "failed"
                                   :summary-function (lambda () (incf reads) "private replacement")))))
       (test-assert (= reads redactions 2) "Each diagnostic goes through native redaction.")
       (test-assert (not (search "autolith.diagnostic.summary" body)) "Redactor failure never falls back to text.")
       (test-assert (not (search "private" body)) "Failed raw text never enters the queue.")
       (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "Failed upload drops its snapshot."))
     (let ((body nil))
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (case (first request)
                   (:redact
                    (test-assert (not (search "fixture@example.org" (second request)))
                                 "Deterministic credential defense precedes native inference.")
                    "[PERSON] at fixture@example.org token=fixture-token /private/home/file")
                   (:upload
                    (setf body (third request)) t)))))
         (telemetry-call-with-run configuration
                                  (lambda () (telemetry-note-report
                                              :summary-function (lambda () "Person fixture@example.org token=fixture-token")))))
       (test-assert (search "autolith.redaction.engine" body) "Successful native output has its marker.")
       (test-assert (not (search "fixture@example.org" body)) "Post-redaction defense masks email.")
       (test-assert (not (search "fixture-token" body)) "Post-redaction defense masks credentials.")
       (test-assert (not (search "/private/home" body)) "Post-redaction defense masks paths."))))
  nil)

(-> test-telemetry-worker-redactor-contract () null)
(defun test-telemetry-worker-redactor-contract ()
  "Exercise the native binding boundary, status filtering and context cleanup."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((library  (merge-pathnames "redactor-library" root))
          (context  (list ':fixture))
          (safe     nil)
          (status   ':ok)
          (signal-p nil)
          (loads    0)
          (closes   0))
      (with-open-file (stream library :direction ':output :if-exists ':supersede)
        (write-string "fixture" stream))
      (test-call-with-function-replacements
       (list
        (list 'defingerprinter:load-library
              (lambda (path)
                (test-assert (equal path (platform-truename *platform* library))
                             "Native loading receives the resolved trusted library.")
                (incf loads)))
        (list 'defingerprinter:make-redactor
              (lambda (&key model-directory runtime-library)
                (test-assert (and (null model-directory) (null runtime-library))
                             "Absent explicit artifacts use native defaults.")
                context))
        (list 'defingerprinter:redact
              (lambda (redactor text &key language)
                (test-assert (and (eq redactor context) (string= text "fixture")
                                  (string= language "nl"))
                             "Pass the context, text and language to REDACT.")
                (when signal-p (error 'simple-error))
                (values safe status)))
        (list 'defingerprinter:close-redactor
              (lambda (redactor)
                (test-assert (eq redactor context) "Close the created context.")
                (incf closes))))
       (lambda ()
         (test-assert (null (telemetry--worker-redact
                             '(:redact "fixture" "nl" nil nil "relative-library")))
                      "Reject relative libraries before native setup.")
         (test-assert (zerop loads) "Invalid paths never load native code.")
         (let ((request (list ':redact "fixture" "nl" nil nil (namestring library))))
           (dolist (entry (list (list "[PERSON] fixture@example.org" ':ok "[PERSON] [EMAIL]")
                               (list "unsafe output" ':inference-error nil)
                               (list nil ':ok nil)
                               (list (make-string 1023 :initial-element #\Space) ':ok
                                     (make-string 1023 :initial-element #\Space))
                               (list (make-string 1024 :initial-element #\Space) ':ok nil)
                               (list (make-string 1025 :initial-element #\Space) ':ok nil)))
             (destructuring-bind (output result expected) entry
               (setf safe output status result)
               (test-assert (equal (telemetry--worker-redact request) expected)
                            "Only bounded successful strings pass post-redaction masking.")))
           (setf signal-p t)
           (test-assert (handler-case (progn (telemetry--worker-redact request) nil)
                          (simple-error () t))
                        "Native conditions unwind through context cleanup.")
           (test-assert (= loads closes 7) "Every created context closes on success or failure."))))))
  nil)


;;;; -- Supervision and Authentication --

(-> test-telemetry-subprocess-deadlines () null)
(defun test-telemetry-subprocess-deadlines ()
  "Deadline and revocation terminate an actual isolated child, without HTTP or native FFI."
  (with-test-environment (("AUTOLITH_TEST_SUPERVISED_SECRET" "fixture-secret"))
    (let ((environment (supervised-process-environment))
          (path        (uiop:getenv "PATH")))
      (when path
        (test-assert (member (concatenate 'string "PATH=" path) environment :test #'equal)
                     "Saved-core children inherit the native library search path."))
      (test-assert (not (find "AUTOLITH_TEST_SUPERVISED_SECRET=" environment
                             :test (lambda (prefix entry) (uiop:string-prefix-p prefix entry))))
                   "Unlisted parent environment values are not passed to children.")))
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((original                     (symbol-function 'uiop:launch-program))
           (ready-path                   (merge-pathnames "telemetry-stall.ready" (config :cache-root configuration)))
           (process                      nil)
           (*telemetry-process-function* nil))
       (test-call-with-function-replacements
        (list (list 'uiop:launch-program
                    (lambda (command &rest options)
                      (if (and (listp command) (member "--telemetry-worker" command :test #'equal))
                          (setf process
                                (test-telemetry--launch-stalled-child original options ready-path))
                          (apply original command options)))))
        (lambda ()
          (let* ((run (telemetry--begin-run configuration)) (start (get-internal-real-time)))
            (test-assert (null (telemetry--process run '(:redact "fixture" "en" nil nil)))
                         "Hung child times out without unsafe native interruption.")
            (test-assert (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)
                         "Hard deadline returns within a bounded allowance.")
            (test-assert (probe-file ready-path)
                         (test-telemetry--stalled-child-diagnostic process ready-path))
            (test-assert (not (uiop:process-alive-p process)) "Timeout kills and reaps the child."))
          (let* ((run    (telemetry--begin-run configuration))
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
                   (test-assert (and (probe-file ready-path) (uiop:process-alive-p process))
                                (test-telemetry--stalled-child-diagnostic process ready-path))
                   (setf (config :telemetry-enabled-p configuration) nil)
                   (test-assert (not (uiop:process-alive-p process)) "Consent revocation kills child synchronously.")
                   (test-assert (null (telemetry-controller-processes *telemetry-controller*)) "No live children after revocation."))
              (bt:join-thread thread)))))
       ;; Keep the real saved-core argv and retain controlled startup failures.
       (setf (config :telemetry-enabled-p configuration) t)
       (let ((run                          (telemetry--begin-run configuration))
             (*telemetry-process-function* nil)
             (stderr-path                  (merge-pathnames "worker-ping.stderr" (config :cache-root configuration)))
             (result                       nil))
         (ensure-directories-exist stderr-path)
         (setf process nil)
         (unwind-protect
              (progn
                (test-call-with-function-replacements
                 (list (list 'uiop:launch-program
                             (lambda (command &rest options)
                               (when (and (listp command)
                                          (member "--telemetry-worker" command :test #'equal))
                                 (setf (getf options :error-output) stderr-path
                                       (getf options :if-error-output-exists) ':supersede))
                               (let ((child (apply original command options)))
                                 (when (and (listp command)
                                            (member "--telemetry-worker" command :test #'equal))
                                   (setf process child))
                                 child))))
                 (lambda () (setf result (telemetry--process run '(:ping)))))
                (test-assert
                 (eq result ':ready)
                 (format nil "Real saved-core readiness: result ~S, core ~S (exists ~S), child exit ~S, stderr ~S"
                         result (config :active-image-core configuration)
                         (not (null (probe-file (config :active-image-core configuration))))
                         (when process (uiop:wait-process process))
                         (with-open-file (stream stderr-path :external-format   ':utf-8
                                                             :if-does-not-exist nil)
                           (when stream
                             (let ((buffer (make-string 4096)))
                               (subseq buffer 0 (read-sequence buffer stream))))))))
           (when (probe-file stderr-path) (delete-file stderr-path)))))))
  nil)

(-> test-telemetry-anonymous-upload () null)
(defun test-telemetry-anonymous-upload ()
  "Upload without credentials while preserving transport and consent boundaries."
  (test-telemetry--call
   (lambda (configuration)
     (let ((headers nil) (uploaded nil))
       (test-call-with-function-replacements
        (list (list 'dex:request
                    (lambda (endpoint &rest options)
                      (test-assert (equal endpoint "http://127.0.0.1:4318/v1/traces") "Use the configured endpoint.")
                      (test-assert (eq (getf options :method) :post) "Uploads use POST.")
                      (test-assert (zerop (getf options :max-redirects)) "Redirects are disabled.")
                      (test-assert (not (getf options :insecure)) "HTTPS certificates are verified.")
                      (test-assert (not (getf options :verbose)) "HTTP diagnostics are suppressed.")
                      (setf headers (getf options :headers) uploaded (getf options :content))
                      (values (make-string-input-stream "") 200))))
        (lambda ()
          (test-assert (telemetry--worker-upload
                        (list :upload "http://127.0.0.1:4318/v1/traces" "{}")) "Anonymous upload succeeds.")))
       (test-assert (equal uploaded "{}") "Upload body is unchanged.")
       (test-assert (equal headers '(("Content-Type" . "application/json")))
                    "Anonymous requests contain only the content-type header.")
       (setf uploaded nil (config :telemetry-enabled-p configuration) t)
       (test-assert (not (config :telemetry-diagnostics-p configuration)) "Diagnostics require separate consent.")
       (telemetry-attach configuration)
       (let ((*telemetry-process-function*
               (lambda (run request)
                 (declare (ignore run))
                 (when (eq (first request) :upload)
                   (test-assert (= (length request) 3) "Upload IPC has no credential field.")
                   (setf uploaded (third request))) t)))
         (telemetry-call-with-run configuration (lambda () :done)))
       (test-assert (stringp uploaded) "Consent-admitted runs upload without token configuration."))))
  nil)

(-> test-telemetry-real-redactor () null)
(defun test-telemetry-real-redactor ()
  "Explicit opt-in actual-model test; uploads are always mocked."
  (when (equal (uiop:getenv "AUTOLITH_TEST_TELEMETRY_MODEL") "1")
    (test-telemetry--call
     (lambda (configuration)
       (setf (config :telemetry-enabled-p configuration) t
             (config :telemetry-diagnostics-p configuration) t)
       (telemetry-attach configuration)
       (let ((body nil) (process-function (symbol-function 'telemetry--process)))
         (let ((*telemetry-process-function*
                 (lambda (run request)
                   (if (eq (first request) ':upload)
                       (progn (setf body (third request)) t)
                       (let ((*telemetry-process-function* nil)) (funcall process-function run request))))))
           (telemetry-call-with-run
            configuration (lambda () (telemetry-note-report
                                      :summary-function
                                      (lambda ()
                                        (concatenate 'string "Alice Smith lives in Amsterdam. "
                                                     (apply #'concatenate 'string
                                                            (make-list 170 :initial-element
                                                                       "The tool returned success without a result. "))
                                                     "Verification remains incomplete."))))))
         (test-assert (and body (search "autolith.redaction.engine" body))
                      "Explicit real model integration succeeds.")
         (test-assert (not (search "Alice Smith" body)) "Actual model removes the person's name.")
         (test-assert (search "remains incomplete" body) "Actual native redaction retains the end of a long explanation.")
         (let ((report (find "report" (test-telemetry--spans body) :test #'equal
                             :key                                        (lambda (span) (test-telemetry--attribute span "autolith.event.kind")))))
           (test-assert (> (length (test-telemetry--attribute report "autolith.diagnostic.summary")) 1024)
                        "The actual native pipeline exports a complete long explanation."))))))
  nil)
