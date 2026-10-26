(in-package #:autolith)

;;;; -- Schema 1 OTLP Projection --

(defparameter *telemetry-tool-catalog*
  '("resource.read" "resource.edit" "search.content" "search.files" "search.glob" "shell.run"
    "lisp.eval" "lisp.load-system" "lisp.run-tests" "lisp.source" "lisp.apropos" "lisp.describe"
    "lisp.scratchpad-run" "lisp.repls" "lisp.images" "lisp.reset" "lisp.start" "lisp.stop"
    "lisp.save-image" "lisp.paren-check" "self.eval" "self.redefine" "self.define" "self.set"
    "self.exercise" "self.commit" "self.diff" "self.discard" "self.status" "self.checkpoint"
    "self.generations" "self.persist-definition" "self.experiment-start" "self.experiment-settle"
    "self.refine" "self.rollback" "task.run" "task.agents" "job.get" "job.wait" "job.send"
    "job.cancel" "job.list" "job.continuity" "task.worktree" "skill.invoke" "skill.verify"
    "skill.edit" "agenda.transport" "mcp.status" "mcp.refresh" "mcp.resources" "mcp.read-resource"
    "mcp.prompts" "mcp.get-prompt" "yield.note" "yield.submit" "tool_search.tool_search_tool"
    "multi_tool_use.parallel" "web.run" "web_extra.gist" "rlm.infer" "rlm.map" "rlm.complete"
    "rlm.distill" "papercut.report" "fs.view-image" "skill.load")
  "Exact Seismograph schema-1 named tools, excluding absence and external categories.")

(defparameter *telemetry-field-catalogs*
  '(("autolith.event.kind" . *telemetry-event-kinds*)
    ("autolith.run.outcome" . *telemetry-outcomes*)
    ("autolith.model.outcome" . *telemetry-outcomes*)
    ("autolith.tool.outcome" . *telemetry-tool-outcomes*)
    ("error.type" . *telemetry-error-types*)
    ("gen_ai.provider.name" . *telemetry-providers*)
    ("gen_ai.request.model" . *telemetry-model-categories*)
    ("gen_ai.response.model" . *telemetry-model-categories*)
    ("autolith.request.role" . *telemetry-request-roles*)
    ("gen_ai.response.finish_reason" . *telemetry-finish-reasons*)
    ("autolith.issue.kind" . *telemetry-issue-kinds*)
    ("autolith.repair.kind" . *telemetry-repair-kinds*)
    ("autolith.repair.outcome" . *telemetry-repair-outcomes*)
    ("autolith.verification.scope" . *telemetry-verification-scopes*))
  "Map projected categorical fields to symbols whose current values define wire vocabularies.")

(-> telemetry--tool (t) (option string))
(defun telemetry--tool (name)
  "Return a catalog tool name or NIL when no public identity is available."
  (when (stringp name) (find name *telemetry-tool-catalog* :test #'string=)))

(-> telemetry--wire-value (string t) t)
(defun telemetry--wire-value (name value)
  "Translate categorical keywords only at export, retaining catalog-required casing."
  (let ((catalog-symbol (rest (assoc name *telemetry-field-catalogs* :test #'string=))))
    (if (and catalog-symbol (keywordp value))
        (let ((entry (assoc value (symbol-value catalog-symbol))))
          (or (third entry) (second entry)))
        value)))

(-> telemetry--attribute (cons) hash-table)
(defun telemetry--attribute (pair)
  "Encode one allowlisted scalar OTLP attribute."
  (let ((value (telemetry--wire-value (first pair) (rest pair))))
    (json-object "key" (first pair) "value"
                 (cond ((stringp value)
                        (json-object "stringValue" value))
                       ((integerp value)
                        (json-object "intValue" (write-to-string value :base 10 :radix nil)))
                       (t
                        (json-object "boolValue" value))))))

(-> telemetry--diagnostic-attributes ((option string)) list)
(defun telemetry--diagnostic-attributes (safe)
  "Attach text only after successful native redaction."
  (when safe
    (list (cons "autolith.diagnostic.summary" safe)
          (cons "autolith.redaction.engine" "rampart")
          (cons "autolith.redaction.revision" "b1993e4e68b082835b80ffc65acc03325ea2e501"))))

(-> telemetry--enqueue (telemetry-run keyword &key (:attributes list) (:duration-ms t)) hash-table)
(defun telemetry--enqueue (run kind &key attributes duration-ms)
  "Queue and return a projected span under the consent lock, discarding oldest on overflow."
  (let* ((kind       (telemetry--wire-value "autolith.event.kind"
                                           (telemetry--enum kind *telemetry-event-kinds*)))
         (controller (telemetry-run-controller run))
         (end        (telemetry--now))
         (duration   (if (typep duration-ms '(integer 0 86400000)) duration-ms 0))
         (root-p     (string= kind "run"))
         (span       (json-object
                      "traceId" (telemetry-run-trace-id run)
                      "spanId" (if root-p (telemetry-run-span-id run) (telemetry--id 8))
                      "name" (concatenate 'string "autolith." kind) "kind" 1
                      "startTimeUnixNano" (write-to-string (if root-p (telemetry-run-start run)
                                                              (- end (* duration 1000000))) :base 10 :radix nil)
                      "endTimeUnixNano" (write-to-string (max end (telemetry-run-start run)) :base 10 :radix nil)
                      "attributes"
                      (coerce (mapcar #'telemetry--attribute
                                      (append (list (cons "autolith.event.kind" kind)
                                                    (cons "autolith.run.span_id" (telemetry-run-span-id run)))
                                              (remove-if (lambda (pair) (null (rest pair))) attributes))) 'vector))))
    (unless root-p (setf (gethash "parentSpanId" span) (telemetry-run-span-id run)))
    (unless (telemetry-controller-closing controller)
      (let* ((queue (telemetry-controller-queue controller))
             (limit (config :telemetry-queue-limit (telemetry-controller-owner controller))))
        (when (>= (length queue) limit) (setf queue (rest queue)))
        (setf (telemetry-controller-queue controller) (nconc queue (list span)))
        (telemetry--schedule-upload controller)))
    span))

(-> telemetry--model-attributes ((option telemetry-model)) list)
(defun telemetry--model-attributes (model)
  "Build the shared model identity attributes only at the export boundary."
  (when model
    (list (cons "gen_ai.provider.name" (telemetry-model-provider model))
          (cons "gen_ai.request.model" (telemetry-model-request-model model))
          (cons "gen_ai.response.model" (telemetry-model-response-model model))
          (cons "autolith.model.known" (json-boolean (telemetry-model-request-known-p model)))
          (cons "autolith.model.response_known" (json-boolean (telemetry-model-response-known-p model))))))

(-> telemetry--export-model (telemetry-run telemetry-model t) hash-table)
(defun telemetry--export-model (run model duration-ms)
  "Export one projected attempt, preserving zero counters and omitting unreported ones."
  (telemetry--enqueue
   run ':model
   :attributes  (append (list (cons "autolith.usage.cache_write_tokens"
                                    (telemetry-model-cache-write-tokens model))
                              (cons "autolith.usage.cache_read_tokens"
                                    (telemetry-model-cache-read-tokens model))
                              (cons "gen_ai.usage.output_tokens" (telemetry-model-output-tokens model))
                              (cons "gen_ai.usage.input_tokens" (telemetry-model-input-tokens model)))
                        (telemetry--model-attributes model)
                        (list (cons "autolith.request.role" (telemetry-model-role model))
                              (cons "autolith.model.outcome" (telemetry-model-outcome model))
                              (cons "gen_ai.response.finish_reason" (telemetry-model-finish-reason model))
                              (cons "error.type" (telemetry-model-error-type model))))
   :duration-ms duration-ms))

(-> telemetry--export-run (telemetry-run (option keyword) (option keyword)) hash-table)
(defun telemetry--export-run (run outcome error-type)
  "Export the aggregate identity and terminal run counters."
  (telemetry--enqueue
   run ':run
   :attributes  (append (telemetry--model-attributes (telemetry-run-model-identity run))
                        (list (cons "autolith.run.outcome" outcome)
                              (cons "autolith.run.attempt" (telemetry-run-attempt run))
                              (cons "autolith.run.recovered" (json-boolean (telemetry-run-recovered run)))
                              (cons "autolith.run.error_code" (telemetry-run-error-code run))
                              (cons "error.type" error-type)
                              (cons "autolith.run.tool_calls" (telemetry-run-tools run))
                              (cons "autolith.run.model_calls" (telemetry-run-models run))))
   :duration-ms 0))

(-> telemetry--export-tool
    (telemetry-run &key (:tool (option string)) (:outcome (option keyword))
                   (:error-code (option string)) (:exit-code (option integer))
                   (:error-type (option keyword)) (:same-p boolean)
                   (:corrects-previous-p boolean) (:duration-ms t)) hash-table)
(defun telemetry--export-tool (run &key tool outcome error-code exit-code error-type
                                      same-p corrects-previous-p duration-ms)
  "Export a projected tool completion and return its queued span for issue linkage."
  (telemetry--enqueue
   run ':tool
   :attributes  (list (cons "autolith.tool.error_code" error-code)
                      (cons "autolith.tool.exit_code" exit-code)
                      (cons "error.type" error-type)
                      (cons "gen_ai.tool.name" tool)
                      (cons "autolith.tool.sequence" (telemetry-run-tools run))
                      (cons "autolith.tool.outcome" outcome)
                      (cons "autolith.tool.same_as_previous" (json-boolean same-p))
                      (cons "autolith.tool.corrects_previous" (json-boolean corrects-previous-p)))
   :duration-ms duration-ms))

(-> telemetry--export-report
    (telemetry-run &key (:identifier string) (:tool (option string))
                   (:issue-kind (option keyword)) (:tool-span-id (option string))
                   (:safe (option string))) hash-table)
(defun telemetry--export-report (run &key identifier tool issue-kind tool-span-id safe)
  "Export an admitted incident and its optional failed-tool link and scrubbed summary."
  (telemetry--enqueue
   run ':report
   :attributes  (append (list (cons "autolith.report.id" identifier)
                              (cons "autolith.incident.id" identifier)
                              (cons "gen_ai.tool.name" tool)
                              (cons "autolith.issue.kind" issue-kind)
                              (cons "autolith.tool.span_id" tool-span-id))
                        (telemetry--diagnostic-attributes safe))
   :duration-ms 0))

(-> telemetry--export-report-link (hash-table string) vector)
(defun telemetry--export-report-link (span report-id)
  "Attach the first incident linkage to a queued tool span under the consent lock."
  (setf (gethash "attributes" span)
        (concatenate 'vector (json-get span "attributes")
                     (mapcar #'telemetry--attribute
                             (list (cons "autolith.report.id" report-id)
                                   (cons "autolith.incident.id" report-id))))))

(-> telemetry--export-repair
    (telemetry-run &key (:event-kind keyword) (:report-id string) (:target (option string))
                   (:operation (option string)) (:issue-kind (option keyword))
                   (:repair-kind (option keyword)) (:outcome (option keyword))
                   (:scope (option keyword)) (:verified-p boolean) (:incident-id (option string))
                   (:attempt-id (option string)) (:safe (option string))) hash-table)
(defun telemetry--export-repair (run &key event-kind report-id target operation issue-kind
                                        repair-kind outcome scope verified-p incident-id attempt-id safe)
  "Export a projected repair lifecycle event and its validated issue identities."
  (telemetry--enqueue
   run event-kind
   :attributes  (append (list (cons "autolith.report.id" report-id)
                              (cons "gen_ai.tool.name" target)
                              (cons "autolith.issue.kind" issue-kind)
                              (cons "autolith.repair.target" target)
                              (cons "autolith.repair.operation" operation)
                              (cons "autolith.repair.kind" repair-kind)
                              (cons "autolith.repair.outcome" outcome)
                              (cons "autolith.verification.scope" scope)
                              (cons "autolith.repair.verified" (json-boolean verified-p))
                              (cons "autolith.incident.id" incident-id)
                              (cons "autolith.repair.attempt.id" attempt-id))
                        (telemetry--diagnostic-attributes safe))
   :duration-ms 0))

(-> telemetry--envelope (telemetry-controller list) hash-table)
(defun telemetry--envelope (controller spans)
  "Construct exactly the allowlisted schema-1 resource and scope envelope."
  (json-object
   "resourceSpans"
   (json-array
    (json-object
     "resource"
     (json-object "attributes"
                  (coerce (mapcar #'telemetry--attribute
                                  (list (cons "service.name" "autolith")
                                       (cons "service.version" (asdf:component-version (asdf:find-system :autolith-agent)))
                                       (cons "autolith.consent.telemetry" (json-boolean t))
                                       (cons "autolith.consent.diagnostics"
                                             (json-boolean (telemetry-controller-diagnostics controller)))
                                       (cons "autolith.schema.version" 1)
                                       (cons "autolith.instrumentation.version" 7))) 'vector))
     "scopeSpans"
     (json-array (json-object "scope" (json-object "name" "seismograph.autolith" "version" "0.1.0")
                              "spans" (coerce spans 'vector)))))))


;;;; -- Supervised Isolated Work --

(-> telemetry--process (telemetry-run list) t)
(defun telemetry--process (run request)
  "Perform one child operation with atomic consent admission and synchronous retirement."
  (unless (bt:with-lock-held ((telemetry-controller-lock (telemetry-run-controller run)))
            (telemetry--live-p run))
    (return-from telemetry--process nil))
  (if *telemetry-process-function*
      (funcall *telemetry-process-function* run request)
      (let* ((controller    (telemetry-run-controller run))
             (configuration (telemetry-controller-owner controller))
             (record        (telemetry--make-process)))
        (supervised-process-request
         record request
         :seconds  (if (eq (first request) ':catalog)
                       *model-catalog-request-seconds*
                       (/ (config :telemetry-timeout-ms configuration) 1000))
         :start
         (lambda (install)
           ;; Admission, registration, and I/O thread installation share the
           ;; owner lock, so retirement cannot miss any part of the operation.
           (bt:with-lock-held ((telemetry-controller-lock controller))
             (when (and (telemetry--live-p run)
                        (< (length (telemetry-controller-processes controller)) 8))
               (let ((core (config :active-image-core configuration)))
                 (when (and core (probe-file core))
                   (let ((process
                           (uiop:launch-program
                            (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                                  "--core" (namestring core) "--end-runtime-options"
                                  (namestring (config :source-root configuration)) "--telemetry-worker")
                            :input :stream :output :stream :error-output nil
                            :external-format :utf-8 :environment (supervised-process-environment))))
                     (push record (telemetry-controller-processes controller))
                     (funcall install process)))))))
         :active-p (lambda ()
                     (bt:with-lock-held ((telemetry-controller-lock controller))
                       (telemetry--live-p run)))
         :accept   (lambda (response)
                     (bt:with-lock-held ((telemetry-controller-lock controller))
                       (when (telemetry--live-p run) (funcall response))))
         :stop     #'telemetry--stop-process-once
         :complete (lambda (record) (telemetry--complete-process controller record))))))

(-> telemetry--pre-redact (string) string)
(defun telemetry--pre-redact (text)
  "Mask credentials, paths, URLs, hosts and opaque identifiers around native inference."
  (let ((result text))
    (dolist (entry
             '(("(?is)-----BEGIN [^-\\r\\n]*PRIVATE KEY-----.*?-----END [^-\\r\\n]*PRIVATE KEY-----" . "[SECRET]")
               ("(?i)\\b(?:Bearer\\s+[^\\s,;]+|(?:password|passwd|secret|token|api[_-]?key|authorization)\\s*[:=]\\s*(?:\"[^\"]*\"|'[^']*'|[^\\s,;]+))" . "[SECRET]")
               ("\\b(?:sk-[A-Za-z0-9_-]+|gh[pousr]_[A-Za-z0-9]+|AKIA[A-Z0-9]{16})\\b" . "[SECRET]")
               ("(?i)\\b[a-z][a-z0-9+.-]*://[^\\s<>\"']+" . "[URL]")
               ("(?:[A-Za-z]:\\\\|\\\\\\\\)[^\\s<>\"']+" . "[PATH]")
               ("(?:/~|~/|/)[^\\s<>\"']+" . "[PATH]")
               ("(?i)\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,63}\\b" . "[EMAIL]")
               ("(?i)\\b(?:[a-z0-9-]+\\.)+(?:[a-z]{2,63}|local|internal)\\b" . "[HOST]")
               ("(?i)\\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\b" . "[ID]")
               ("\\b[A-Za-z0-9_+/=-]{32,}\\b" . "[OPAQUE]")))
      (setf result (cl-ppcre:regex-replace-all (first entry) result (rest entry))))
    (map 'string (lambda (char)
                   (let ((code (char-code char)))
                     (if (or (< code 32) (<= 127 code 159)) #\Space char))) result)))

(-> telemetry--redact-narrative (t string string) (option string))
(defun telemetry--redact-narrative (redactor text language)
  "Redact complete sentence or word pieces within the native ABI's output bound.
The native API truncates at 1,024 characters. Reject saturated output or any
failed piece rather than exporting a partial explanation. One session handles
all pieces within the caller's existing deadline."
  (block redact
    (let ((start  0)
          (length (length text)))
      (with-output-to-string (stream)
        (loop while (< start length)
              for ceiling = (min length (+ start 512))
              for end = (if (= ceiling length)
                            ceiling
                            (or (loop for index downfrom (1- ceiling) above (+ start 256)
                                      when (and (char= (char text index) #\Space)
                                                (find (char text (1- index)) ".!?;"))
                                        return (1+ index))
                                (let ((space (position #\Space text :start start :end ceiling :from-end t)))
                                  (when space (1+ space)))
                                (return-from redact nil)))
              do (multiple-value-bind (safe status)
                     (defingerprinter:redact redactor (subseq text start end) :language language)
                   (unless (and (eq status ':ok) (stringp safe) (< (length safe) 1024))
                     (return-from redact nil))
                   (write-string safe stream))
                 (setf start end))))))

(-> telemetry--worker-redact (list) (option string))
(defun telemetry--worker-redact (request)
  "Load a trusted absolute library and a lazy context inside a consent-admitted child."
  (destructuring-bind (operation text language model runtime library) request
    (declare (ignore operation))
    (unless (and (stringp text) (<= (length text) *telemetry-diagnostic-limit*))
      (return-from telemetry--worker-redact nil))
    (unless (and (stringp library) (uiop:absolute-pathname-p library))
      (return-from telemetry--worker-redact nil))
    (let* ((path   (platform-truename *platform* (pathname library)))
           (status (platform-path-status *platform* path)))
      (unless (and status (eq (platform-file-status-kind status) ':file))
        (return-from telemetry--worker-redact nil))
      (defingerprinter:load-library path)
      (let ((redactor (defingerprinter:make-redactor :model-directory model :runtime-library runtime)))
        (unwind-protect
             (let ((safe (telemetry--redact-narrative redactor text language)))
               (when (and (stringp safe)
                          (<= (length safe) *telemetry-diagnostic-limit*))
                 (telemetry--pre-redact safe)))
          (defingerprinter:close-redactor redactor))))))

(-> telemetry--worker-upload (list) boolean)
(defun telemetry--worker-upload (request)
  "Upload anonymously; never follow redirects or retain response text."
  (destructuring-bind (operation endpoint body) request
    (declare (ignore operation))
    (unless (telemetry--endpoint-p endpoint) (return-from telemetry--worker-upload nil))
    (multiple-value-bind (stream status)
        (dex:request endpoint :method ':post :content body
                  :headers '(("Content-Type" . "application/json"))
                  :max-redirects 0 :connect-timeout 3 :read-timeout 3
                  :keep-alive nil :use-connection-pool nil :want-stream t
                  :proxy nil :insecure nil :verbose nil)
      (unwind-protect (and (integerp status) (<= 200 status 299) t)
        (when (streamp stream) (close stream))))))

(-> telemetry--worker-main () nil)
(defun telemetry--worker-main ()
  "One-shot subprocess entry. All failures return NIL without printing conditions or text."
  (let ((input *standard-input*) (output *standard-output*))
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output*    (make-broadcast-stream))
          (*trace-output*    (make-broadcast-stream))
          (*debug-io*        (make-two-way-stream (make-string-input-stream "") (make-broadcast-stream))))
      (let ((result
              (handler-case
                  (let* ((*read-eval* nil) (request (read input nil nil)))
                    (case (first request)
                      (:ping
                       ':ready)
                      (:redact
                       (telemetry--worker-redact request))
                      (:upload
                       (telemetry--worker-upload request))
                      (:catalog
                       (model-catalog--http-fetch))))
                (error () nil))))
        (let ((*print-readably* t) (*print-pretty* nil))
          (write result :stream output) (terpri output) (finish-output output)))))
  (uiop:quit 0))
