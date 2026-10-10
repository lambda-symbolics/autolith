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
  "Exact Seismograph schema-1 tool catalog; external names are unknown.")

(defun telemetry--tool (name)
  "Return a catalog tool name, or unknown without exporting the supplied name."
  (or (and (stringp name) (find name *telemetry-tool-catalog* :test #'string=)) "unknown"))

(defun telemetry--attribute (pair)
  "Encode one allowlisted scalar OTLP attribute."
  (let ((value (cdr pair)))
    (json-object "key" (car pair) "value"
                 (cond ((stringp value) (json-object "stringValue" value))
                       ((integerp value) (json-object "intValue" (write-to-string value :base 10 :radix nil)))
                       (t (json-object "boolValue" value))))))

(defun telemetry--diagnostic-attributes (safe)
  "Attach text only after successful native redaction."
  (when safe
    (list (cons "autolith.diagnostic.summary" safe)
          (cons "autolith.redaction.engine" "rampart")
          (cons "autolith.redaction.revision" "b1993e4e68b082835b80ffc65acc03325ea2e501"))))

(defun telemetry--enqueue (run kind attributes duration-ms)
  "Queue a projected span under the consent lock, discarding oldest spans on overflow."
  (let* ((controller (telemetry-run-controller run))
         (end (telemetry--now))
         (duration (if (typep duration-ms '(integer 0 86400000)) duration-ms 0))
         (root-p (string= kind "run"))
         (span (json-object
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
                                        attributes)) 'vector))))
    (unless root-p (setf (gethash "parentSpanId" span) (telemetry-run-span-id run)))
    (let* ((queue (telemetry-controller-queue controller))
           (limit (config :telemetry-queue-limit (telemetry-controller-owner controller))))
      (when (>= (length queue) limit) (setf queue (rest queue)))
      (setf (telemetry-controller-queue controller) (nconc queue (list span)))))
  nil)

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
                                        (cons "service.version" (asdf:component-version (asdf:find-system :autolith)))
                                        (cons "autolith.consent.telemetry" (json-boolean t))
                                        (cons "autolith.consent.diagnostics"
                                              (json-boolean (telemetry-controller-diagnostics controller)))
                                        (cons "autolith.schema.version" 1)
                                        (cons "autolith.instrumentation.version" 1))) 'vector))
     "scopeSpans"
     (json-array (json-object "scope" (json-object "name" "seismograph.autolith" "version" "0.1.0")
                              "spans" (coerce spans 'vector)))))))

(defun telemetry-flush (&optional (run *telemetry-run*))
  "Upload one bounded queue snapshot; failures discard it and never enter agent output."
  (when run
    (let* ((controller (telemetry-run-controller run))
           (request
             (bt:with-lock-held ((telemetry-controller-lock controller))
               (when (telemetry--live-p run)
                 (let ((spans (telemetry-controller-queue controller))
                       (configuration (telemetry-controller-owner controller)))
                   (setf (telemetry-controller-queue controller) nil)
                   (when spans
                     (list :upload (config :telemetry-endpoint configuration)
                           (config :telemetry-token-file configuration)
                           (json-encode (telemetry--envelope controller spans)))))))))
      (when request (ignore-errors (telemetry--process run request)))))
  nil)

;;;; -- Supervised Isolated Work --

(defun telemetry--worker-environment ()
  "Supply only locale, platform runtime and certificate paths to the one-shot child."
  (cons "LANG=en_US.UTF-8"
        (loop for name in '("SYSTEMROOT" "WINDIR" "TEMP" "TMP" "TMPDIR" "SBCL_HOME"
                            "SSL_CERT_FILE" "SSL_CERT_DIR" "NIX_SSL_CERT_FILE")
              for value = (uiop:getenv name)
              when value collect (concatenate 'string name "=" value))))

(defun telemetry--process (run request)
  "Perform one child operation, enforcing a hard deadline and synchronous revocation.
Neither native model handles nor token contents ever enter the parent heap."
  (unless (bt:with-lock-held ((telemetry-controller-lock (telemetry-run-controller run)))
            (telemetry--live-p run))
    (return-from telemetry--process nil))
  (if *telemetry-process-function*
      (funcall *telemetry-process-function* run request)
      (let* ((controller (telemetry-run-controller run))
             (configuration (telemetry-controller-owner controller))
             (deadline (+ (get-internal-real-time)
                          (ceiling (* (config :telemetry-timeout-ms configuration)
                                      internal-time-units-per-second) 1000)))
             (process nil) (thread nil) (result nil) (done nil)
             (result-lock (bt:make-lock "telemetry worker response")))
        (unwind-protect
             (progn
               (bt:with-lock-held ((telemetry-controller-lock controller))
                 (unless (and (telemetry--live-p run)
                              (< (length (telemetry-controller-processes controller)) 8))
                   (return-from telemetry--process nil))
                 (let ((core (config :active-image-core configuration)))
                   (unless (and core (probe-file core)) (return-from telemetry--process nil))
                   (setf process
                         (uiop:launch-program
                          (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                                "--core" (namestring core) "--end-runtime-options"
                                (namestring (config :source-root configuration)) "--telemetry-worker")
                          :input :stream :output :stream :error-output nil
                          :external-format :utf-8 :environment (telemetry--worker-environment)))
                   (push process (telemetry-controller-processes controller))))
               (setf thread
                     (bt:make-thread
                      (lambda ()
                        (let ((response
                                (handler-case
                                    (progn
                                      (let ((*print-readably* t) (*print-pretty* nil))
                                        (write request :stream (uiop:process-info-input process))
                                        (terpri (uiop:process-info-input process))
                                        (finish-output (uiop:process-info-input process)))
                                      (close (uiop:process-info-input process))
                                      (let ((*read-eval* nil))
                                        (read (uiop:process-info-output process) nil nil)))
                                  (error () nil))))
                          (bt:with-lock-held (result-lock)
                            (setf result response done t))))
                      :name "telemetry subprocess I/O"))
               (loop
                 (when (bt:with-lock-held (result-lock) done) (return))
                 (unless (bt:with-lock-held ((telemetry-controller-lock controller))
                           (telemetry--live-p run)) (return))
                 (when (>= (get-internal-real-time) deadline) (return))
                 (sleep 0.005))
               (bt:with-lock-held ((telemetry-controller-lock controller))
                 (when (and (telemetry--live-p run)
                            (< (get-internal-real-time) deadline))
                   (bt:with-lock-held (result-lock) (when done result)))))
          (when process
            (telemetry--stop-process process)
            (bt:with-lock-held ((telemetry-controller-lock controller))
              (setf (telemetry-controller-processes controller)
                    (remove process (telemetry-controller-processes controller)))))
          ;; Closing the killed child's pipes releases the Lisp-only I/O thread.
          (when thread (ignore-errors (bt:join-thread thread)))))))

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
      (setf result (cl-ppcre:regex-replace-all (car entry) result (cdr entry))))
    (map 'string (lambda (char)
                   (let ((code (char-code char)))
                     (if (or (< code 32) (<= 127 code 159)) #\Space char))) result)))

(defun telemetry--read-token (path)
  "Read a bounded private, owner-controlled regular file only inside the upload child."
  (when path
    (multiple-value-bind (stream status) (platform-open-regular-file *platform* path)
      (unwind-protect
           (when (and (platform-file-status-private-p status)
                      (platform-file-status-owned-p status)
                      (<= 1 (platform-file-status-size status) 4096))
             (let* ((bytes (make-array (platform-file-status-size status)
                                      :element-type '(unsigned-byte 8)))
                    (length (read-sequence bytes stream))
                    (text (and (= length (length bytes))
                               (babel:octets-to-string bytes :encoding :utf-8 :errorp t))))
               (when text
                 (setf text (string-trim '(#\Newline #\Return) text))
                 (when (and (plusp (length text))
                            (every (lambda (char) (<= 33 (char-code char) 126)) text)) text))))
        (close stream)))))

(defun telemetry--worker-redact (request)
  "Load a trusted absolute library and a lazy context inside a consent-admitted child."
  (destructuring-bind (operation text language model runtime library) request
    (declare (ignore operation))
    (unless (and (stringp library) (uiop:absolute-pathname-p library))
      (return-from telemetry--worker-redact nil))
    (let* ((path (platform-truename *platform* (pathname library)))
           (status (platform-path-status *platform* path)))
      (unless (and status (eq (platform-file-status-kind status) ':file))
        (return-from telemetry--worker-redact nil))
      (defingerprinter:load-library path)
      (let ((redactor (defingerprinter:make-redactor :model-directory model :runtime-library runtime)))
        (unwind-protect
             (multiple-value-bind (safe status)
                 (defingerprinter:protect redactor text :language language)
               (when (and (eq status ':ok) (stringp safe) (<= (length safe) 1024))
                 (telemetry--pre-redact safe)))
          (defingerprinter:close-redactor redactor))))))

(defun telemetry--worker-upload (request)
  "Read authentication only at upload time; never follow redirects or retain response text."
  (destructuring-bind (operation endpoint token-file body) request
    (declare (ignore operation))
    (unless (telemetry--endpoint-p endpoint) (return-from telemetry--worker-upload nil))
    (let ((token (telemetry--read-token token-file)))
      (when (and token-file (null token)) (return-from telemetry--worker-upload nil))
      (multiple-value-bind (stream status)
          (dex:request endpoint :method ':post :content body
                    :headers (append '(("Content-Type" . "application/json"))
                                     (when token (list (cons "Authorization" (concatenate 'string "Bearer " token)))))
                    :max-redirects 0 :connect-timeout 3 :read-timeout 3
                    :keep-alive nil :use-connection-pool nil :want-stream t
                    :proxy nil :insecure nil :verbose nil)
        (unwind-protect (and (integerp status) (<= 200 status 299) t)
          (when (streamp stream) (close stream)))))))

(defun telemetry--worker-main ()
  "One-shot subprocess entry. All failures return NIL without printing conditions or text."
  (let ((input *standard-input*) (output *standard-output*))
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream))
          (*trace-output* (make-broadcast-stream))
          (*debug-io* (make-two-way-stream (make-string-input-stream "") (make-broadcast-stream))))
      (let ((result
              (handler-case
                  (let* ((*read-eval* nil) (request (read input nil nil)))
                    (case (first request)
                      (:ping ':ready)
                      (:redact (telemetry--worker-redact request))
                      (:upload (telemetry--worker-upload request))))
                (error () nil))))
        (let ((*print-readably* t) (*print-pretty* nil))
          (write result :stream output) (terpri output) (finish-output output)))))
  (uiop:quit 0))
