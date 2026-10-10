(in-package #:autolith)

;;;; -- Process-Owned Consent --

(defstruct (telemetry-controller (:constructor telemetry--make-controller))
  "Process-local consent, bounded projected spans and supervised subprocesses."
  owner roots library listener (lock (bt:make-lock "telemetry consent"))
  (privacy-lock (bt:make-lock "telemetry checkpoint"))
  enabled diagnostics (epoch 0) queue runs processes quiescent)

(defstruct (telemetry-run (:constructor telemetry--make-run))
  "Fresh run identity and counters; never holds prompts, arguments or native handles."
  controller epoch trace-id span-id report-id start (tools 0) (models 0) previous-digest active)

(defvar *telemetry-controller* nil
  "The current process owner's controller; detached before saving a checkpoint.")

(defvar *telemetry-run* nil
  "Dynamic run context, explicitly passed to parallel tool completions.")

(defvar *telemetry-process-function* nil
  "Test-only scoped replacement of the supervised subprocess transport.")

(defun telemetry--id (bytes)
  "Return a fresh cryptographically random lowercase hexadecimal identifier."
  (string-downcase (format nil "~{~2,'0X~}" (coerce (ironclad:random-data bytes) 'list))))

(defun telemetry--now ()
  "Return decimal Unix nanoseconds with wall-clock microsecond precision."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000000) (* microseconds 1000))))

(defun telemetry--live-p (run)
  "Check RUN's generation while holding its controller lock."
  (and run (telemetry-run-active run)
       (let ((controller (telemetry-run-controller run)))
         (and (eq controller *telemetry-controller*)
              (telemetry-controller-enabled controller)
              (not (telemetry-controller-quiescent controller))
              (= (telemetry-run-epoch run) (telemetry-controller-epoch controller))))))

(defun telemetry--stop-process (process)
  "Kill and reap a supervised child without exposing its diagnostics."
  (ignore-errors (platform-terminate-process-object *platform* process :force t))
  (ignore-errors (uiop:wait-process process))
  (ignore-errors (uiop:close-streams process)))

(defun telemetry--purge (controller)
  "Revoke every run and child synchronously, under the controller lock."
  (incf (telemetry-controller-epoch controller))
  (dolist (run (telemetry-controller-runs controller))
    (setf (telemetry-run-active run) nil
          (telemetry-run-previous-digest run) nil))
  (setf (telemetry-controller-queue controller) nil
        (telemetry-controller-runs controller) nil)
  (dolist (process (telemetry-controller-processes controller))
    (telemetry--stop-process process))
  (setf (telemetry-controller-processes controller) nil)
  nil)

(defun telemetry--settings-changed (configuration setting old new)
  "Adopt owner consent changes; attached secondary roots may revoke, never enable."
  (declare (ignore old))
  (let ((controller *telemetry-controller*) (mirror nil))
    (when controller
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (let* ((name (setting-name setting))
               (owner-p (eq configuration (telemetry-controller-owner controller)))
               (root-p (member configuration (telemetry-controller-roots controller) :test #'eq))
               (consent-p (member name '(:telemetry-enabled-p :telemetry-diagnostics-p))))
          (when (and root-p
                     (or (and owner-p (or (eq name ':active-image-core)
                                         (search "TELEMETRY-" (symbol-name name))))
                         (and consent-p (null new))))
            (telemetry--purge controller)
            (case name
              (:telemetry-enabled-p
               (setf (telemetry-controller-enabled controller) (and owner-p (eq new t))
                     (telemetry-controller-diagnostics controller)
                     (and (telemetry-controller-enabled controller)
                          (eq (config :telemetry-diagnostics-p (telemetry-controller-owner controller)) t))))
              (:telemetry-diagnostics-p
               (setf (telemetry-controller-diagnostics controller)
                     (and owner-p (telemetry-controller-enabled controller) (eq new t)))))
            (when (and (not owner-p) consent-p) (setf mirror name)))))
      ;; Process consent is already revoked before synchronizing the durable owner.
      (when mirror
        (ignore-errors (setf (config mirror (telemetry-controller-owner controller)) nil)))))
  nil)

(-> telemetry-consent-configuration (configuration) configuration)
(defun telemetry-consent-configuration (configuration)
  "Return the canonical configuration for consent UI reads/writes on attached roots.
Unregistered child copies retain their own local settings and cannot acquire ownership."
  (let ((controller *telemetry-controller*))
    (if (and controller (member configuration (telemetry-controller-roots controller) :test #'eq))
        (telemetry-controller-owner controller)
        configuration)))

(defun telemetry-attach (configuration)
  "Attach the owner once, then register secondary roots for revocation only.
Attaching a default-off root does not change existing process consent."
  (unless *telemetry-controller*
    (setf *telemetry-controller*
          (telemetry--make-controller
           :owner configuration :listener #'telemetry--settings-changed
           :library (uiop:getenv "AUTOLITH_DEFINGERPRINTER_LIBRARY")
           :enabled (eq (config :telemetry-enabled-p configuration) t)
           :diagnostics (and (eq (config :telemetry-enabled-p configuration) t)
                             (eq (config :telemetry-diagnostics-p configuration) t)))))
  (let ((controller *telemetry-controller*))
    (bt:with-lock-held ((telemetry-controller-lock controller))
      (pushnew configuration (telemetry-controller-roots controller) :test #'eq))
    (configuration-add-listener configuration (telemetry-controller-listener controller)))
  nil)

(defun telemetry-shutdown ()
  "Synchronously revoke workers, spans and runs, then remove the owner listener."
  (let ((controller *telemetry-controller*))
    (when controller
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (setf (telemetry-controller-enabled controller) nil)
        (telemetry--purge controller))
      (dolist (configuration (telemetry-controller-roots controller))
        (configuration-remove-listener configuration (telemetry-controller-listener controller)))
      (setf *telemetry-controller* nil)))
  nil)

(defun telemetry-quiesce ()
  "Discard pending telemetry and synchronously stop children before a checkpoint."
  (let ((controller *telemetry-controller*))
    (when controller
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (setf (telemetry-controller-quiescent controller) t)
        (telemetry--purge controller))))
  nil)

(defun telemetry-detach ()
  "Remove inherited process state in a checkpoint child, without killing parent children."
  (let ((controller *telemetry-controller*))
    (when controller
      (dolist (configuration (telemetry-controller-roots controller))
        (configuration-remove-listener configuration (telemetry-controller-listener controller)))))
  (setf *telemetry-controller* nil *telemetry-run* nil)
  nil)

(defun telemetry-call-with-quiescence (function)
  "Exclude diagnostic text and worker launch throughout FUNCTION's checkpoint fork.
The caller invokes TELEMETRY-DETACH in the fork child before saving its core.
Pending runs and spans are discarded; the parent accepts fresh runs afterwards."
  (let ((controller *telemetry-controller*))
    (if (null controller)
        (funcall function)
        (bt:with-lock-held ((telemetry-controller-privacy-lock controller))
          (telemetry-quiesce)
          (unwind-protect (funcall function)
            (bt:with-lock-held ((telemetry-controller-lock controller))
              (setf (telemetry-controller-quiescent controller) nil)))))))

(defun telemetry--begin-run (configuration)
  "Create an independent root only when both the owner and caller currently consent."
  (let ((controller *telemetry-controller*))
    (when controller
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (and (telemetry-controller-enabled controller)
                   (not (telemetry-controller-quiescent controller))
                   (or (member configuration (telemetry-controller-roots controller) :test #'eq)
                       (eq (config :telemetry-enabled-p configuration) t))
                   (< (length (telemetry-controller-runs controller)) 128))
          (let ((run (telemetry--make-run :controller controller
                                        :epoch (telemetry-controller-epoch controller)
                                        :trace-id (telemetry--id 16) :span-id (telemetry--id 8)
                                        :report-id (telemetry--id 16)
                                        :start (telemetry--now) :active t)))
            (push run (telemetry-controller-runs controller))
            run))))))

(defun telemetry-call-with-run (configuration function)
  "Call FUNCTION with an ambient root or a fresh independent root, preserving all exits.
Run-end upload is bounded and failure-isolated. Nonlocal exits count as cancelled."
  (if *telemetry-run*
      (funcall function)
      (let ((*telemetry-run* (ignore-errors (telemetry--begin-run configuration)))
            (outcome "cancelled"))
        (unwind-protect
             (handler-bind ((serious-condition
                             (lambda (condition)
                               (let ((cancelled (find-class 'application-turn-cancelled nil))
                                     (control (find-class 'autolith-control-condition nil)))
                                 (setf outcome
                                       (if (or (and cancelled (typep condition cancelled))
                                               (and control (typep condition control)))
                                           "cancelled" "error"))))))
               (multiple-value-prog1 (funcall function) (setf outcome "success")))
          (when *telemetry-run*
            (ignore-errors (telemetry--end-run *telemetry-run* outcome)))))))

;;;; -- Narrow Metadata Projections --

(defun telemetry--enum (value choices fallback)
  "Project only an exact public enumeration, without printing arbitrary objects."
  (let ((text (cond ((stringp value) value)
                    ((keywordp value) (string-downcase (symbol-name value))))))
    (or (and text (find text choices :test #'string=)) fallback)))

(defun telemetry--count (value)
  "Accept bounded nonnegative integer metadata."
  (and (typep value '(integer 0 1000000000)) value))

(defun telemetry--model (controller value)
  "Map a local model through the owner's explicit public aliases."
  (if (null value) "unknown"
      (let ((alias (and (stringp value)
                        (cdr (assoc value (config :telemetry-model-aliases
                                                  (telemetry-controller-owner controller))
                                    :test #'string=)))))
        (if (telemetry--alias-p alias) alias "custom"))))

(defun telemetry--usage-field (usage name)
  "Read a normalized hash table or portable plist counter, never arbitrary usage fields."
  (telemetry--count
   (cond ((hash-table-p usage) (gethash name usage))
         ((listp usage)
          (or (getf usage (intern (string-upcase (substitute #\- #\_ name)) :keyword))
              (getf usage (intern (string-upcase name) :keyword)))))))

(defun telemetry-note-model (&key (run *telemetry-run*) provider request-model response-model usage duration-ms)
  "Record one actual provider attempt, projecting only aliases and token counters."
  (when run
    (let ((controller (telemetry-run-controller run)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (telemetry--live-p run)
          (incf (telemetry-run-models run))
          (let ((attributes
                  (list (cons "gen_ai.provider.name"
                              (telemetry--enum provider
                                              '("openai" "anthropic" "google" "xai" "mistral_ai"
                                                "deepseek" "ollama" "other" "cohere" "meta" "local")
                                              "unknown"))
                        (cons "gen_ai.request.model" (telemetry--model controller request-model))
                        (cons "gen_ai.response.model" (telemetry--model controller response-model)))))
            (loop for (source . target) in
                  '(("input_tokens" . "gen_ai.usage.input_tokens")
                    ("output_tokens" . "gen_ai.usage.output_tokens")
                    ("cached_input_tokens" . "autolith.usage.cache_read_tokens")
                    ("cache_creation_input_tokens" . "autolith.usage.cache_write_tokens"))
                  for value = (telemetry--usage-field usage source)
                  when value do (push (cons target value) attributes))
            (telemetry--enqueue run "model" attributes duration-ms))))))
  nil)

(defun telemetry--argument-digest (arguments tool trace-id)
  "Hash bounded canonical JSON with fresh run salt; retain only the local digest."
  (handler-case
      (let* ((limits (make-json-limits :maximum-characters 65536 :maximum-depth 24
                                     :maximum-nodes 4096 :maximum-string-characters 65536
                                     :maximum-aggregate-string-characters 65536))
             ;; Validate before traversing user arguments, including cycles and large atoms.
             (validated (json-decode (json-encode arguments :limits limits) :limits limits))
             (canonical
               (with-output-to-string (stream)
                 (labels ((emit (value)
                            (cond
                              ((hash-table-p value)
                               (write-char #\{ stream)
                               (loop for key in (sort (loop for key being the hash-keys of value collect key) #'string<)
                                     for first-p = t then nil
                                     do (unless first-p (write-char #\, stream))
                                        (write-string (json-encode key) stream) (write-char #\: stream)
                                        (emit (gethash key value)))
                               (write-char #\} stream))
                              ((and (vectorp value) (not (stringp value)))
                               (write-char #\[ stream)
                               (loop for item across value for first-p = t then nil
                                     do (unless first-p (write-char #\, stream)) (emit item))
                               (write-char #\] stream))
                              (t (write-string (json-encode value) stream)))))
                   (emit validated)))))
        (ironclad:digest-sequence
         :sha256 (babel:string-to-octets
                  (concatenate 'string trace-id (if (stringp tool) tool "unknown") canonical)
                  :encoding :utf-8)))
    (error () nil)))

(defun telemetry-note-tool (&key (run *telemetry-run*) tool arguments outcome duration-ms corrects-previous)
  "Record a completed tool attempt. Equality is local; export only its boolean."
  (when run
    (let ((controller (telemetry-run-controller run)))
      (unless (bt:with-lock-held ((telemetry-controller-lock controller)) (telemetry--live-p run))
        (return-from telemetry-note-tool nil))
      (bt:with-lock-held ((telemetry-controller-privacy-lock controller))
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (when (telemetry--live-p run)
            (let* ((digest (telemetry--argument-digest arguments tool (telemetry-run-trace-id run)))
                   (previous (telemetry-run-previous-digest run))
                   (same (and digest previous (equalp digest previous))))
              (incf (telemetry-run-tools run))
              (setf (telemetry-run-previous-digest run) digest)
              (telemetry--enqueue
               run "tool"
               (list (cons "gen_ai.tool.name" (telemetry--tool tool))
                     (cons "autolith.tool.sequence" (telemetry-run-tools run))
                     (cons "autolith.tool.outcome"
                           (telemetry--enum outcome '("success" "error" "permission_denied" "cancelled")
                                            "unknown"))
                     (cons "autolith.tool.same_as_previous" (json-boolean (not (null same))))
                     (cons "autolith.tool.corrects_previous" (json-boolean (eq corrects-previous t))))
               duration-ms)))))))
  nil)

(defun telemetry--diagnostic (run summary-function language)
  "Gate before reading the summary thunk, redact in a deadline-supervised child, then recheck."
  (let ((controller (telemetry-run-controller run)))
    (unless (and (functionp summary-function)
                 (bt:with-lock-held ((telemetry-controller-lock controller))
                   (and (telemetry--live-p run) (telemetry-controller-diagnostics controller))))
      (return-from telemetry--diagnostic nil))
    (bt:with-lock-held ((telemetry-controller-privacy-lock controller))
      (when (bt:with-lock-held ((telemetry-controller-lock controller))
              (and (telemetry--live-p run) (telemetry-controller-diagnostics controller)
                   (functionp summary-function)
                   (member language '("en" "es" "fr" "de" "it" "pt" "nl") :test #'equal)))
        (handler-case
            (let ((text (funcall summary-function)))
              (when (and (stringp text) (<= (length text) 4096)
                         (every (lambda (char) (<= (char-code char) #x024f)) text))
                (let ((safe (telemetry--process run
                                              (list :redact (telemetry--pre-redact text) language
                                                    (config :telemetry-model-directory
                                                            (telemetry-controller-owner controller))
                                                    (config :telemetry-runtime-library
                                                            (telemetry-controller-owner controller))
                                                    (telemetry-controller-library controller)))))
                  (when (and (stringp safe) (<= (length safe) 1024))
                    (telemetry--pre-redact safe)))))
          (error () nil))))))

(defun telemetry-note-report (&key (run *telemetry-run*) tool issue-kind summary-function (language "en"))
  "Record structured report metadata and, with separate consent, a redacted summary."
  (when run
    (let ((safe (telemetry--diagnostic run summary-function language))
          (controller (telemetry-run-controller run)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (telemetry--live-p run)
          (telemetry--enqueue
           run "report"
           (append (list (cons "autolith.report.id" (telemetry-run-report-id run))
                         (cons "gen_ai.tool.name" (telemetry--tool tool))
                         (cons "autolith.issue.kind"
                               (telemetry--enum issue-kind
                                                '("broken_tool" "misleading_success" "authorization"
                                                  "performance" "other") "other")))
                   (telemetry--diagnostic-attributes safe)) 0)))))
  nil)

(defun telemetry-note-repair (&key (run *telemetry-run*) target repair-kind outcome verified summary-function
                                  (language "en"))
  "Record a repair transition without source, replacement values or raw verification output."
  (when run
    (let ((safe (telemetry--diagnostic run summary-function language))
          (controller (telemetry-run-controller run)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (telemetry--live-p run)
          (telemetry--enqueue
           run "repair"
           (append (list (cons "autolith.report.id" (telemetry-run-report-id run))
                         (cons "autolith.repair.target" (telemetry--tool target))
                         (cons "autolith.repair.kind"
                               (telemetry--enum repair-kind
                                                '("redefine_function" "change_setting" "change_tool_strategy" "other")
                                                "other"))
                         (cons "autolith.repair.outcome"
                               (telemetry--enum outcome
                                                '("proposed" "applied" "verified" "failed" "reverted" "committed")
                                                "proposed"))
                         (cons "autolith.repair.verified" (json-boolean (eq verified t))))
                   (telemetry--diagnostic-attributes safe)) 0)))))
  nil)

(defun telemetry--end-run (run outcome)
  "Queue the root counters, dispose local equality state, and flush once within a hard bound."
  (let ((controller (telemetry-run-controller run)))
    (bt:with-lock-held ((telemetry-controller-lock controller))
      (when (telemetry--live-p run)
        (telemetry--enqueue run "run"
                            (list (cons "autolith.run.outcome" outcome)
                                  (cons "autolith.run.tool_calls" (telemetry-run-tools run))
                                  (cons "autolith.run.model_calls" (telemetry-run-models run))) 0)
        (setf (telemetry-run-previous-digest run) nil)))
    (unwind-protect (telemetry-flush run)
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (setf (telemetry-run-active run) nil
              (telemetry-controller-runs controller)
              (remove run (telemetry-controller-runs controller))))))
  nil)
