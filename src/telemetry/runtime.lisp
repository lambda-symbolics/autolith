(in-package #:autolith)

;;;; -- Process-Owned Consent --

(defparameter *telemetry-diagnostic-limit* 8000
  "Maximum characters in an explicit diagnostic before and after redaction.")

(-> telemetry-diagnostic-text (t) (option string))
(defun telemetry-diagnostic-text (value)
  "Validate optional text deliberately supplied for consent-gated diagnostic export."
  (when value
    (unless (and (non-empty-string-p value)
                 (<= (length value) *telemetry-diagnostic-limit*))
      (error 'tool-error :tool-name "diagnostic"
                         :message   "Diagnostic must contain between 1 and 8,000 characters."))
    value))

(defstruct (telemetry-controller (:include model-catalog)
                               (:constructor telemetry--make-controller))
  "Process-local consent, bounded projected spans and supervised subprocesses."
  owner roots library listener
  (privacy-lock (bt:make-lock "telemetry checkpoint"))
  enabled diagnostics stopped closing (epoch 0) queue runs processes quiescent
  upload-thread upload-now-p retired-upload-threads
  (upload-wakeup (bt:make-condition-variable :name "telemetry uploads")))

(defstruct (telemetry-run (:constructor telemetry--make-run))
  "Fresh run identity and counters; never holds prompts, arguments or native handles."
  controller epoch trace-id span-id start (tools 0) (models 0) previous-digest active upload-p
  model-identity model-role ending-p owner borrower-outcome (attempt 1) recovered error-code
  (tool-failures (make-hash-table :test #'equal))
  (repair-ids (make-hash-table :test #'equal)))

(defstruct (telemetry-process (:include supervised-process)
                              (:constructor telemetry--make-process))
  "One consent-owned child and its completion acknowledgement."
  completed-p)

(defstruct telemetry-model
  "Projected identity and attempt metadata; nullable counters preserve unreported usage."
  provider request-model response-model request-known-p response-known-p
  input-tokens output-tokens cache-read-tokens cache-write-tokens
  role outcome finish-reason error-type)

(defvar *telemetry-controller* nil
  "The current process owner's controller; detached before saving a checkpoint.")

(defvar *telemetry-run* nil
  "Dynamic run context, explicitly passed to parallel tool completions.")

(defvar *telemetry-process-function* nil
  "Test-only scoped replacement of the supervised subprocess transport.")

(-> telemetry--id ((integer 1)) string)
(defun telemetry--id (bytes)
  "Return a fresh cryptographically random lowercase hexadecimal identifier."
  (string-downcase (format nil "~{~2,'0X~}" (coerce (ironclad:random-data bytes) 'list))))

(-> telemetry--now () (integer 0))
(defun telemetry--now ()
  "Return decimal Unix nanoseconds with wall-clock microsecond precision."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000000) (* microseconds 1000))))

(-> telemetry--report-id-p (t) boolean)
(defun telemetry--report-id-p (value)
  "Accept only a nonzero 128-bit hexadecimal correlation identifier."
  (and (stringp value) (= (length value) 32)
       (every (lambda (char) (digit-char-p char 16)) value)
       (not (every (lambda (char) (char= char #\0)) value)) t))

(-> telemetry-new-report-id () (option string))
(defun telemetry-new-report-id ()
  "Allocate an issue identity only inside a live consented run."
  (when *telemetry-run*
    (bt:with-lock-held ((telemetry-controller-lock (telemetry-run-controller *telemetry-run*)))
      (when (telemetry--live-p *telemetry-run*) (telemetry--id 16)))))

(-> telemetry-mutation-report-id (t) (option string))
(defun telemetry-mutation-report-id (identifier)
  "Correlate one mutation's transitions within a run, without exporting its local ID."
  (when (and *telemetry-run* (stringp identifier))
    (bt:with-lock-held ((telemetry-controller-lock (telemetry-run-controller *telemetry-run*)))
      (when (telemetry--live-p *telemetry-run*)
        (or (gethash identifier (telemetry-run-repair-ids *telemetry-run*))
            (setf (gethash identifier (telemetry-run-repair-ids *telemetry-run*)) (telemetry--id 16)))))))

(-> telemetry--live-p ((option telemetry-run)) boolean)
(defun telemetry--live-p (run)
  "Check RUN's generation while holding its controller lock."
  (and run (telemetry-run-active run)
       (let ((controller (telemetry-run-controller run)))
         (and (eq controller *telemetry-controller*)
              (telemetry-controller-enabled controller)
              (not (telemetry-controller-stopped controller))
              (or (not (telemetry-controller-closing controller)) (telemetry-run-upload-p run))
              (not (telemetry-controller-quiescent controller))
              (= (telemetry-run-epoch run) (telemetry-controller-epoch controller))))))

(defmethod model-catalog-context-active-p ((context telemetry-run))
  "Keep model catalog I/O and publication inside this exact telemetry consent epoch."
  (telemetry--live-p context))

(defmethod model-catalog-context-fetch ((context telemetry-run))
  "Fetch the public catalog in telemetry's consent-supervised disposable process."
  (telemetry--process context '(:catalog)))

(-> telemetry--ensure-model-catalog (telemetry-controller) null)
(defun telemetry--ensure-model-catalog (controller)
  "Supply the neutral catalog module with the current telemetry consent epoch."
  (let ((context (bt:with-lock-held ((telemetry-controller-lock controller))
                   (telemetry--make-run :controller controller
                                        :epoch (telemetry-controller-epoch controller) :active t))))
    (model-catalog-ensure controller context)))

(-> telemetry--stop-process (t) null)
(defun telemetry--stop-process (process)
  "Kill and reap a supervised child without exposing its diagnostics."
  (supervised-process-terminate process))

(-> telemetry--stop-process-once (telemetry-process) null)
(defun telemetry--stop-process-once (record)
  "Serialize RECORD's child teardown without holding the controller lock."
  (supervised-process-stop record :terminate #'telemetry--stop-process))

(-> telemetry--complete-process (telemetry-controller telemetry-process) null)
(defun telemetry--complete-process (controller record)
  "Remove RECORD only after its child and I/O thread have both stopped."
  (bt:with-lock-held ((telemetry-controller-lock controller))
    (setf (telemetry-controller-processes controller)
          (remove record (telemetry-controller-processes controller)))
    (bt:with-lock-held ((telemetry-process-lock record))
      (setf (telemetry-process-completed-p record) t)))
  nil)

(-> telemetry--retire-process (telemetry-process) null)
(defun telemetry--retire-process (record)
  "Stop RECORD and wait for its owning operation to finish I/O cleanup."
  (telemetry--stop-process-once record)
  (loop until (bt:with-lock-held ((telemetry-process-lock record))
                (telemetry-process-completed-p record))
        do (sleep 0.005))
  nil)

(-> telemetry--purge (telemetry-controller) list)
(defun telemetry--purge (controller)
  "Revoke runs under the controller lock and return all outstanding operations.
Keep records registered until teardown completes, including across concurrent purges."
  (model-catalog-retire controller)
  (telemetry--retire-uploader controller)
  (incf (telemetry-controller-epoch controller))
  (dolist (run (telemetry-controller-runs controller))
    (clrhash (telemetry-run-tool-failures run))
    (clrhash (telemetry-run-repair-ids run))
    (setf (telemetry-run-active run) nil
          (telemetry-run-previous-digest run) nil))
  (setf (telemetry-controller-queue controller) nil
        (telemetry-controller-runs controller) nil)
  (copy-list (telemetry-controller-processes controller)))

(-> telemetry--settings-changed (configuration setting &key (:new t)) null)
(defun telemetry--settings-changed (configuration setting &key new)
  "Adopt owner consent changes; attached secondary roots may revoke, never enable.

NEW is SETTING's value after the change."
  (let ((controller *telemetry-controller*) (mirror nil) (processes nil))
    (when controller
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (let* ((name      (setting-name setting))
               (owner-p   (eq configuration (telemetry-controller-owner controller)))
               (root-p    (member configuration (telemetry-controller-roots controller) :test #'eq))
               (consent-p (member name '(:telemetry-enabled-p :telemetry-diagnostics-p))))
          (when (and (not (telemetry-controller-stopped controller)) root-p
                     (or (and owner-p (or (eq name ':active-image-core)
                                          (search "TELEMETRY-" (symbol-name name))))
                         (and consent-p (null new))))
            (setf processes (telemetry--purge controller))
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
      ;; Process termination may wait for an I/O thread. Never hold the
      ;; controller lock while performing that blocking teardown.
      (dolist (record processes)
        (telemetry--retire-process record))
      ;; Process consent is already revoked before synchronizing the durable owner.
      (when mirror
        ; mallet:suppress no-ignore-errors -- consent is already revoked in this process; persisting it is best effort
        (ignore-errors (setf (config mirror (telemetry-controller-owner controller)) nil)))
      (model-catalog-join controller)
      (telemetry--join-uploader controller)
      (telemetry--ensure-model-catalog controller)))
  nil)

(-> telemetry-consent-configuration (configuration) configuration)
(defun telemetry-consent-configuration (configuration)
  "Return the canonical configuration for consent UI reads/writes on attached roots.
Unregistered child copies retain their own local settings and cannot acquire ownership."
  (let ((controller *telemetry-controller*))
    (if (and controller (member configuration (telemetry-controller-roots controller) :test #'eq))
        (telemetry-controller-owner controller)
        configuration)))

(-> telemetry-attach (configuration) null)
(defun telemetry-attach (configuration)
  "Attach the owner once, then register secondary roots for revocation only.
Attaching a disabled root does not change existing process consent."
  (unless *telemetry-controller*
    (setf *telemetry-controller*
          (telemetry--make-controller
           :owner       configuration
           :listener    (lambda (root setting old new)
                          (declare (ignore old))
                          (telemetry--settings-changed root setting :new new))
           :cache-root  (config :cache-root configuration)
           :library     (uiop:getenv "AUTOLITH_DEFINGERPRINTER_LIBRARY")
           :enabled     (eq (config :telemetry-enabled-p configuration) t)
           :diagnostics (and (eq (config :telemetry-enabled-p configuration) t)
                             (eq (config :telemetry-diagnostics-p configuration) t)))))
  (let ((controller *telemetry-controller*))
    (bt:with-lock-held ((telemetry-controller-lock controller))
      (pushnew configuration (telemetry-controller-roots controller) :test #'eq))
    (configuration-add-listener configuration (telemetry-controller-listener controller))
    (telemetry--ensure-model-catalog controller))
  nil)

(-> telemetry-shutdown (&key (:drain-p boolean)) null)
(defun telemetry-shutdown (&key (drain-p t))
  "Stop this controller, optionally giving final uploads one deadline to drain.
Revocation and checkpoint quiescence always cancel immediately instead of draining."
  (let ((controller *telemetry-controller*))
    (when controller
      (when drain-p
        ; mallet:suppress no-ignore-errors -- draining final uploads must never block shutdown
        (ignore-errors (telemetry--drain-uploads controller)))
      (let (processes)
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (setf (telemetry-controller-stopped controller) t
                (telemetry-controller-enabled controller) nil
                processes (telemetry--purge controller)))
        (dolist (record processes)
          (telemetry--retire-process record)))
      (model-catalog-join controller)
      (telemetry--join-uploader controller)
      (dolist (configuration (telemetry-controller-roots controller))
        (configuration-remove-listener configuration (telemetry-controller-listener controller)))
      (setf *telemetry-controller* nil)))
  nil)

(-> telemetry-quiesce () null)
(defun telemetry-quiesce ()
  "Discard pending telemetry and synchronously stop children before a checkpoint."
  (let ((controller *telemetry-controller*))
    (when controller
      (let (processes)
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (setf (telemetry-controller-quiescent controller) t
                processes (telemetry--purge controller)))
        (dolist (record processes)
          (telemetry--retire-process record)))
      (model-catalog-join controller)
      (telemetry--join-uploader controller)))
  nil)

(-> telemetry-detach () null)
(defun telemetry-detach ()
  "Remove inherited process state in a checkpoint child, without killing parent children."
  (let ((controller *telemetry-controller*))
    (when controller
      (dolist (configuration (telemetry-controller-roots controller))
        (configuration-remove-listener configuration (telemetry-controller-listener controller)))))
  (setf *telemetry-controller* nil *telemetry-run* nil)
  nil)

(-> telemetry-call-with-quiescence (function) t)
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
            (when (eq controller *telemetry-controller*)
              (bt:with-lock-held ((telemetry-controller-lock controller))
                (setf (telemetry-controller-quiescent controller) nil))
              (telemetry--ensure-model-catalog controller)))))))

(-> telemetry--log (keyword string) null)
(defun telemetry--log (level message)
  "Write a bounded local lifecycle diagnostic without external context values."
  ; mallet:suppress no-ignore-errors -- a local diagnostic must never fail its caller
  (ignore-errors (format *error-output* "[telemetry ~A] ~A~%" level message))
  nil)

(-> telemetry--traceparent-id (t) (option string))
(defun telemetry--traceparent-id (value)
  "Return the trace ID only for a strict, nonzero W3C version 00 context."
  (when (and (stringp value) (= (length value) 55)
             (string= value "00-" :end1 3)
             (char= (char value 35) #\-)
             (char= (char value 52) #\-)
             (every (lambda (part)
                      (every (lambda (character) (find character "0123456789abcdef")) part))
                    (list (subseq value 3 35) (subseq value 36 52) (subseq value 53 55)))
             (not (every (lambda (character) (char= character #\0)) (subseq value 3 35)))
             (not (every (lambda (character) (char= character #\0)) (subseq value 36 52))))
    (subseq value 3 35)))

(-> telemetry--run-attempt (t) (integer 1 65535))
(defun telemetry--run-attempt (value)
  "Accept a decimal UInt16 attempt; invalid external values warn and become one."
  (cond ((null value)
         1)
        ((and (stringp value) (<= 1 (length value) 5)
              (every (lambda (character) (find character "0123456789")) value)
              (<= 1 (parse-integer value) 65535))
         (parse-integer value))
        (t
         (telemetry--log ':warning "Invalid AUTOLITH_RUN_ATTEMPT; using attempt 1.")
         1)))

(-> telemetry--consume-launch-context () list)
(defun telemetry--consume-launch-context ()
  "Consume launcher-only environment before any agent work can spawn children."
  (let ((context (uiop:getenv "AUTOLITH_TRACEPARENT"))
        (attempt (uiop:getenv "AUTOLITH_RUN_ATTEMPT")))
    (platform-unsetenv "AUTOLITH_TRACEPARENT")
    (platform-unsetenv "AUTOLITH_RUN_ATTEMPT")
    (let ((trace-id (telemetry--traceparent-id context)))
      (when (and context (not trace-id))
        (telemetry--log ':warning "Invalid AUTOLITH_TRACEPARENT; starting a fresh trace."))
      (list trace-id (telemetry--run-attempt attempt)))))

(defvar *telemetry-launch-context* (telemetry--consume-launch-context)
  "Launcher context consumed once on source startup, never inherited by agent children.")

(defvar *telemetry-launch-lock* (bt:make-lock "launcher telemetry claim")
  "Serialize launcher claims independently of controller consent and lifetime.")

(defvar *telemetry-launch-claimed-p* nil
  "True after this process's top-level headless run claims its launcher context.")

(-> telemetry--initialize-process-context () null)
(defun telemetry--initialize-process-context ()
  "Replace saved-image context with this process's launcher environment at startup."
  (bt:with-lock-held (*telemetry-launch-lock*)
    (setf *telemetry-launch-context* (telemetry--consume-launch-context)
          *telemetry-launch-claimed-p* nil))
  nil)

(pushnew 'telemetry--initialize-process-context sb-ext:*init-hooks*)

(-> telemetry--claim-launch-context () list)
(defun telemetry--claim-launch-context ()
  "Return the startup context to at most one headless owner in this process."
  (bt:with-lock-held (*telemetry-launch-lock*)
    (unless *telemetry-launch-claimed-p*
      (setf *telemetry-launch-claimed-p* t)
      *telemetry-launch-context*)))

(-> telemetry--begin-run
    ((option configuration) &key (:launcher-p boolean))
    (values (option telemetry-run) list))
(defun telemetry--begin-run (configuration &key launcher-p)
  "Create a consented root and return RUN and its opaque finish token as two values.
Only the first top-level launcher scope can adopt the process trace context."
  (let ((controller *telemetry-controller*))
    (when controller
      (telemetry--ensure-model-catalog controller)
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (and (telemetry-controller-enabled controller)
                   (not (telemetry-controller-quiescent controller))
                   (not (telemetry-controller-closing controller))
                   (or (member configuration (telemetry-controller-roots controller) :test #'eq)
                       (eq (config :telemetry-enabled-p configuration) t))
                   (< (length (telemetry-controller-runs controller)) 128))
          (let* ((context  (and launcher-p (telemetry--claim-launch-context)))
                 (trace-id (first context))
                 (attempt  (or (second context) 1))
                 (token    (list nil))
                 (run      (telemetry--make-run :controller controller :owner token
                                             :epoch (telemetry-controller-epoch controller)
                                             :trace-id (or trace-id (telemetry--id 16)) :span-id (telemetry--id 8)
                                             :attempt attempt :recovered (> attempt 1)
                                             :start (telemetry--now) :active t)))
            (push run (telemetry-controller-runs controller))
            (values run token)))))))

(-> telemetry-call-with-run ((option configuration) function) t)
(defun telemetry-call-with-run (configuration function)
  "Call FUNCTION with an ambient root or a fresh independent root, preserving all exits.
Run-end upload is asynchronous and failure-isolated. Unreported exits are internal errors."
  (if *telemetry-run*
      (funcall function)
      (multiple-value-bind (run token)
          ; mallet:suppress no-ignore-errors -- telemetry failures must never prevent the run itself
          (ignore-errors (telemetry--begin-run configuration))
        (let ((*telemetry-run*  run)
              (failure-terminal nil)
              (returned-p       nil))
          (unwind-protect
               (handler-bind ((serious-condition
                               (lambda (condition)
                                 (setf failure-terminal
                                       (list :state      ':failed
                                             :error-type (telemetry--condition-error-type condition))))))
                 (multiple-value-prog1 (funcall function)
                   (setf returned-p t)))
            ; mallet:suppress no-ignore-errors -- telemetry failures must never replace the run's own exit
            (ignore-errors
              (telemetry-finish-run run token
                                    (if returned-p
                                        (list :state ':completed)
                                        (or failure-terminal (list :state ':unfinished))))))))))


;;;; -- Narrow Metadata Projections --

(defparameter *telemetry-outcomes*
  '((:success "success") (:error "error") (:cancelled "cancelled"))
  "Run and model terminal outcomes; missing outcomes have no category.")

(defparameter *telemetry-tool-outcomes*
  (append *telemetry-outcomes* '((:permission-denied "permission_denied")))
  "Tool outcomes distinguish an authority denial from an execution error.")

(defparameter *telemetry-error-types*
  '((:cancelled "cancelled") (:authentication "authentication")
    (:rate-limit "rate_limit") (:provider "provider") (:transport "transport")
    (:internal "internal") (:tool "tool") (:permission "permission")
    (:timeout "timeout") (:protocol "protocol"))
  "Failure categories. INTERNAL means an unclassified local failure, never absence.")

(defparameter *telemetry-providers*
  '((:openai "openai") (:anthropic "anthropic") (:google "google")
    (:xai "xai") (:mistral-ai "mistral_ai") (:deepseek "deepseek")
    (:ollama "ollama") (:other "other") (:cohere "cohere")
    (:meta "meta") (:local "local"))
  "Provider categories. OTHER denotes an uncatalogued family or multiple families.")

(defparameter *telemetry-model-categories*
  '((:mixed "mixed") (:custom "custom"))
  "MIXED denotes differing run identities; CUSTOM replaces unlisted model IDs.")

(defparameter *telemetry-request-roles*
  '((:agent "agent") (:permission-classifier "permission_classifier")
    (:title-generation "title_generation") (:compaction "compaction")
    (:inference "inference"))
  "Known purposes of actual model requests.")

(defparameter *telemetry-finish-reasons*
  '((:stop "stop") (:completed "completed") (:tool-calls "tool_calls")
    (:function-call "function_call") (:length "length")
    (:max-output-tokens "max_output_tokens") (:max-tokens "max_tokens")
    (:content-filter "content_filter") (:end-turn "end_turn") (:tool-use "tool_use")
    (:stop-sequence "stop_sequence") (:pause-turn "pause_turn") (:refusal "refusal")
    (:continue "continue") (:failed "failed") (:safety "safety" "SAFETY")
    (:recitation "recitation" "RECITATION") (:other "other" "OTHER")
    (:malformed-function-call "malformed_function_call" "MALFORMED_FUNCTION_CALL")
    (:model-context-window-exceeded "model_context_window_exceeded"))
  "Lowercase terminal reasons, with wire overrides for uppercase-only catalog entries.
OTHER is the provider's explicit terminal reason.")

(defparameter *telemetry-issue-kinds*
  '((:broken-tool "broken_tool") (:misleading-success "misleading_success")
    (:authorization "authorization") (:performance "performance") (:other "other"))
  "Issue categories. OTHER is an explicitly classified issue outside the named kinds.")

(defparameter *telemetry-repair-kinds*
  '((:redefine-function "redefine_function") (:change-setting "change_setting")
    (:change-tool-strategy "change_tool_strategy") (:other "other"))
  "Repair categories. OTHER denotes an actual repair outside the named operations.")

(defparameter *telemetry-repair-outcomes*
  '((:proposed "proposed") (:applied "applied") (:verified "verified")
    (:failed "failed") (:reverted "reverted") (:committed "committed")
    (:improved "improved") (:worse "worse") (:unchanged "unchanged")
    (:too-early "too-early") (:closed "closed"))
  "Observed repair and assessment transitions.")

(defparameter *telemetry-verification-scopes*
  '((:none "none") (:focused "focused") (:full "full"))
  "NONE explicitly records no verification; non-check events omit scope.")

(defparameter *telemetry-event-kinds*
  '((:run "run") (:model "model") (:tool "tool") (:report "report")
    (:repair "repair") (:check "check") (:assessment "assessment") (:closure "closure"))
  "Kinds of recorded lifecycle events.")

(-> telemetry--enum (t list) (option keyword))
(defun telemetry--enum (value catalog)
  "Return a catalog keyword or NIL without retaining an arbitrary external value.
Catalog rows contain a keyword, accepted spelling and optional exporter wire spelling."
  (cond ((keywordp value)
         (first (assoc value catalog)))
        ((stringp value)
         (first (find value catalog :key #'second :test #'string=)))))

(-> telemetry--count (t) (option (integer 0 1000000000)))
(defun telemetry--count (value)
  "Accept bounded nonnegative integer metadata."
  (and (typep value '(integer 0 1000000000)) value))

(-> telemetry--model (telemetry-controller t) (values (or null string keyword) boolean))
(defun telemetry--model (controller value)
  "Return an explicit alias, exact public ID, or a fixed category with its known flag."
  (cond ((or (null value) (equal value ""))
         (values nil nil))
        ((stringp value)
         (let ((alias (rest (assoc value
                                  (config :telemetry-model-aliases
                                          (telemetry-controller-owner controller))
                                  :test #'string=))))
           (cond ((telemetry--alias-p alias)
                  (values alias t))
                 ((model-catalog-known-p controller value)
                  (values value t))
                 (t
                  (values (or (telemetry--enum value *telemetry-model-categories*)
                              ':custom) nil)))))
        (t
         (values nil nil))))

(-> telemetry--model-identity
    (telemetry-controller &key (:provider t) (:request-model t) (:response-model t))
    telemetry-model)
(defun telemetry--model-identity (controller &key provider request-model response-model)
  "Project both identities and independent known flags before retaining run metadata."
  (multiple-value-bind (request request-known) (telemetry--model controller request-model)
    (multiple-value-bind (response response-known) (telemetry--model controller response-model)
      (make-telemetry-model :provider (telemetry--enum provider *telemetry-providers*)
                            :request-model request :response-model response
                            :request-known-p request-known :response-known-p response-known))))

(-> telemetry--usage-field (t string) (option (integer 0 1000000000)))
(defun telemetry--usage-field (usage name)
  "Read a normalized hash table or portable plist counter, never arbitrary usage fields."
  (telemetry--count
   (cond ((hash-table-p usage)
          (gethash name usage))
         ((listp usage)
          (or (getf usage (intern (string-upcase (substitute #\- #\_ name)) :keyword))
              (getf usage (intern (string-upcase name) :keyword)))))))

(-> telemetry--run-model-identity (telemetry-run telemetry-model keyword) null)
(defun telemetry--run-model-identity (run identity role)
  "Aggregate primary agent identities, using helper requests only in helper-only runs.
Different primary identities become mixed; missing response names never inherit requests."
  (cond ((or (null (telemetry-run-model-identity run))
             (and (eq role ':agent) (not (eq (telemetry-run-model-role run) ':agent))))
         (setf (telemetry-run-model-identity run) (copy-telemetry-model identity)
               (telemetry-run-model-role run) role))
        ((equal role (telemetry-run-model-role run))
         (let ((aggregate (telemetry-run-model-identity run)))
           (unless (equal (telemetry-model-provider aggregate) (telemetry-model-provider identity))
             (setf (telemetry-model-provider aggregate) ':other))
           (unless (equal (telemetry-model-request-model aggregate) (telemetry-model-request-model identity))
             (setf (telemetry-model-request-model aggregate) ':mixed))
           (unless (equal (telemetry-model-response-model aggregate) (telemetry-model-response-model identity))
             (setf (telemetry-model-response-model aggregate) ':mixed))
           (setf (telemetry-model-request-known-p aggregate)
                 (and (telemetry-model-request-known-p aggregate)
                      (telemetry-model-request-known-p identity)
                      (not (eq (telemetry-model-request-model aggregate) ':mixed)))
                 (telemetry-model-response-known-p aggregate)
                 (and (telemetry-model-response-known-p aggregate)
                      (telemetry-model-response-known-p identity)
                      (not (eq (telemetry-model-response-model aggregate) ':mixed)))))))
  nil)

(-> telemetry-note-model
    (&key (:run (option telemetry-run)) (:provider t) (:request-model t) (:response-model t)
          (:outcome t) (:finish-reason t) (:error-type t) (:role t) (:usage t) (:duration-ms t))
    null)
(defun telemetry-note-model (&key (run *telemetry-run*) provider request-model response-model
                                 outcome finish-reason error-type role usage duration-ms)
  "Record one actual provider attempt, projecting only aliases and token counters."
  (when run
    (let ((controller (telemetry-run-controller run)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (and (telemetry--live-p run) (not (telemetry-run-ending-p run)))
          (incf (telemetry-run-models run))
          (let* ((role     (telemetry--enum role *telemetry-request-roles*))
                 (identity (telemetry--model-identity controller :provider     provider
                                                               :request-model  request-model
                                                               :response-model response-model)))
            (setf (telemetry-model-role identity) role
                  (telemetry-model-outcome identity) (telemetry--enum outcome *telemetry-outcomes*)
                  (telemetry-model-finish-reason identity)
                  (telemetry--enum (if (stringp finish-reason) (string-downcase finish-reason) finish-reason)
                                   *telemetry-finish-reasons*)
                  (telemetry-model-error-type identity) (telemetry--enum error-type *telemetry-error-types*)
                  (telemetry-model-input-tokens identity) (telemetry--usage-field usage "input_tokens")
                  (telemetry-model-output-tokens identity) (telemetry--usage-field usage "output_tokens")
                  (telemetry-model-cache-read-tokens identity) (telemetry--usage-field usage "cached_input_tokens")
                  (telemetry-model-cache-write-tokens identity) (telemetry--usage-field usage "cache_creation_input_tokens"))
            (telemetry--run-model-identity
             run identity
             ;; Unlabelled requests retain their historical primary grouping.
             (or role ':agent))
            (telemetry--export-model run identity duration-ms))))))
  nil)

(-> telemetry--argument-digest (t t string) (option (vector (unsigned-byte 8))))
(defun telemetry--argument-digest (arguments tool trace-id)
  "Hash bounded canonical JSON with fresh run salt; retain only the local digest."
  (handler-case
      (let* ((limits    (make-json-limits :maximum-characters 65536 :maximum-depth 24
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
                              (t
                               (write-string (json-encode value) stream)))))
                   (emit validated)))))
        (ironclad:digest-sequence
         :sha256 (babel:string-to-octets
                  (concatenate 'string trace-id (if (stringp tool) tool "") canonical)
                  :encoding :utf-8)))
    (error () nil)))

(-> telemetry--error-code (t) (option string))
(defun telemetry--error-code (code)
  "Bound a supplied failure CODE to the collector's lowercase ASCII vocabulary."
  (when code
    (let ((name (and (keywordp code) (symbol-name code))))
      (if (and name (<= 1 (length name) 64)
               (every (lambda (character)
                        (or (char<= #\a character #\z)
                            (char<= #\A character #\Z)
                            (char<= #\0 character #\9)
                            (char= character #\-)))
                      name))
          (string-downcase name)
          "tool-failure"))))

(-> telemetry--authority-denial-code-p (t) boolean)
(defun telemetry--authority-denial-code-p (code)
  "Recognize tool failures caused by user or policy authority, excluding OS refusals."
  (not (null (member code '(:access-denied :permission-denied :authorization
                            :not-authorized :capability-denied :authority)))))

(-> telemetry--permission-error-code-p (t) boolean)
(defun telemetry--permission-error-code-p (code)
  "Recognize authority denials and OS permission failures for error classification."
  (or (telemetry--authority-denial-code-p code)
      (eq code ':os-permission-denied)))

(-> telemetry--outcome
    (&key (:outcome (option keyword)) (:success-p boolean) (:error-type (option keyword))
          (:denied-p boolean) (:error-code t)) (option keyword))
(defun telemetry--outcome (&key (outcome ':error) success-p error-type denied-p error-code)
  "Classify completion once, distinguishing authority denial from OS permission errors."
  (cond ((or success-p (eq outcome ':success))
         ':success)
        ((or (eq outcome ':cancelled) (eq error-type ':cancelled))
         ':cancelled)
        ((and (eq outcome ':error)
              (not (eq error-code ':os-permission-denied))
              (or denied-p (telemetry--authority-denial-code-p error-code)))
         ':permission-denied)
        (t
         outcome)))

(-> telemetry--tool-error-type ((option keyword) t t) (option keyword))
(defun telemetry--tool-error-type (outcome error-code error-type)
  "Keep explicit failure metadata or infer a category from an observed tool failure."
  (unless (eq outcome ':success)
    (or (telemetry--enum error-type *telemetry-error-types*)
        (cond ((eq outcome ':cancelled)
               ':cancelled)
              ((or (eq outcome ':permission-denied)
                   (telemetry--permission-error-code-p error-code))
               ':permission)
              ((member error-code '(:timeout :timed-out))
               ':timeout)
              ((eq outcome ':error)
               ':tool)))))

(-> telemetry-note-tool
    (&key (:run (option telemetry-run)) (:tool t) (:arguments t) (:outcome t) (:error-code t)
          (:exit-code t) (:error-type t) (:duration-ms t) (:corrects-previous t))
    null)
(defun telemetry-note-tool (&key (run *telemetry-run*) tool arguments outcome error-code exit-code error-type
                                duration-ms corrects-previous)
  "Record a completed tool attempt. Equality is local; export only its boolean."
  (setf outcome (telemetry--outcome :outcome  (telemetry--enum outcome *telemetry-tool-outcomes*)
                                  :error-code error-code))
  (when run
    (let ((controller (telemetry-run-controller run)))
      (unless (bt:with-lock-held ((telemetry-controller-lock controller)) (telemetry--live-p run))
        (return-from telemetry-note-tool nil))
      (bt:with-lock-held ((telemetry-controller-privacy-lock controller))
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (when (and (telemetry--live-p run) (not (telemetry-run-ending-p run)))
            (let* ((digest   (telemetry--argument-digest arguments tool (telemetry-run-trace-id run)))
                   (previous (telemetry-run-previous-digest run))
                   (same     (and digest previous (equalp digest previous))))
              (incf (telemetry-run-tools run))
              (setf (telemetry-run-previous-digest run) digest)
              (let ((span
                      (telemetry--export-tool
                       run :tool (telemetry--tool tool) :outcome outcome
                       :error-code  (telemetry--error-code error-code)
                       :exit-code   (when (typep exit-code '(signed-byte 32)) exit-code)
                       :error-type  (telemetry--tool-error-type outcome error-code error-type)
                       :same-p (not (null same)) :corrects-previous-p (eq corrects-previous t)
                       :duration-ms duration-ms)))
                (when (and (stringp (telemetry--tool tool))
                           (member outcome '(:error :permission-denied)))
                  (setf (gethash tool (telemetry-run-tool-failures run))
                        (cons (json-get span "spanId") nil))))))))))
  nil)

(-> telemetry--diagnostic (telemetry-run t t) (option string))
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
              (when (and (stringp text) (<= (length text) *telemetry-diagnostic-limit*)
                         (every (lambda (char) (<= (char-code char) #x024f)) text))
                (let ((safe (telemetry--process run
                                              (list :redact (telemetry--pre-redact text) language
                                                    (config :telemetry-model-directory
                                                            (telemetry-controller-owner controller))
                                                    (config :telemetry-runtime-library
                                                            (telemetry-controller-owner controller))
                                                    (telemetry-controller-library controller)))))
                  (when (and (stringp safe) (<= (length safe) *telemetry-diagnostic-limit*))
                    (let ((masked (telemetry--pre-redact safe)))
                      (when (<= (length masked) *telemetry-diagnostic-limit*) masked))))))
          (error () nil))))))

(-> telemetry-note-report
    (&key (:run (option telemetry-run)) (:report-id t) (:tool t) (:issue-kind t) (:duplicate-p t)
          (:summary-function t) (:language t))
    null)
(defun telemetry-note-report (&key (run *telemetry-run*) report-id tool issue-kind duplicate-p
                                  summary-function (language "en"))
  "Record an incident or a duplicate's new link to an already exported tool span."
  (when run
    (let* ((safe       (unless duplicate-p (telemetry--diagnostic run summary-function language)))
           (controller (telemetry-run-controller run))
           (identifier (if (telemetry--report-id-p report-id) report-id (telemetry--id 16))))
      (multiple-value-bind (tool-span-id exported-link-p)
          (telemetry-link-tool-report tool identifier :run run)
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (when (and (telemetry--live-p run) (or (not duplicate-p) exported-link-p))
            (telemetry--export-report
             run :identifier identifier :tool (telemetry--tool tool)
             :issue-kind (telemetry--enum issue-kind *telemetry-issue-kinds*)
             :tool-span-id tool-span-id :safe safe))))))
  nil)

(-> telemetry-link-tool-report
    ((option string) (option string) &key (:run (option telemetry-run)))
    (values (option string) boolean))
(defun telemetry-link-tool-report (tool report-id &key (run *telemetry-run*))
  "Return the latest failed TOOL span ID and link it to REPORT-ID, even after flush.
The second value is true only for a newly linked span no longer in the queue.
Retain one identity per catalog tool until run end. Never replace another report's link."
  (let ((identifier      nil)
        (exported-link-p nil))
    (when (and run (telemetry--report-id-p report-id)
               (stringp (telemetry--tool tool)))
      (let ((controller (telemetry-run-controller run)))
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (when (telemetry--live-p run)
            (let ((failure (gethash tool (telemetry-run-tool-failures run))))
              (when (and failure (or (null (rest failure)) (equal (rest failure) report-id)))
                (setf identifier (first failure))
                (unless (rest failure)
                  (setf (rest failure) report-id)
                  (let ((span (find-if
                               (lambda (span)
                                 (and (equal (json-get span "traceId") (telemetry-run-trace-id run))
                                      (equal (json-get span "spanId") identifier)))
                               (telemetry-controller-queue controller))))
                    (if span
                        (telemetry--export-report-link span report-id)
                        (setf exported-link-p t))))))))))
    (values identifier exported-link-p)))

(-> telemetry-note-repair
    (&key (:run (option telemetry-run)) (:report-id t) (:incident-id t) (:attempt-id t) (:target t)
          (:issue-kind t) (:repair-kind t) (:outcome t) (:verified t) (:operation t) (:event-kind t)
          (:verification-scope t) (:summary-function t) (:language t))
    null)
(defun telemetry-note-repair (&key (run *telemetry-run*) report-id incident-id attempt-id
                                  target issue-kind repair-kind outcome verified operation
                                  (event-kind ':repair) verification-scope
                                  summary-function (language "en"))
  "Record a typed incident event without source, values or raw check output."
  (when run
    (let* ((safe       (telemetry--diagnostic run summary-function language))
           (controller (telemetry-run-controller run))
           (event-kind (let ((kind (telemetry--enum event-kind *telemetry-event-kinds*)))
                         (if (member kind '(:repair :check :assessment :closure)) kind ':repair)))
           (outcome    (telemetry--enum outcome *telemetry-repair-outcomes*))
           (scope      (when (eq event-kind ':check)
                         (telemetry--enum verification-scope *telemetry-verification-scopes*))))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (telemetry--live-p run)
          (telemetry--export-repair
           run :event-kind event-kind
           :report-id      (cond ((telemetry--report-id-p incident-id)
                                  incident-id)
                                 ((telemetry--report-id-p report-id)
                                  report-id)
                                 ((telemetry--report-id-p attempt-id)
                                  attempt-id)
                                 (t
                                  (telemetry--id 16)))
           :target (telemetry--tool target) :operation (telemetry--tool operation)
           :issue-kind     (telemetry--enum issue-kind *telemetry-issue-kinds*)
           :repair-kind    (telemetry--enum repair-kind *telemetry-repair-kinds*)
           :outcome outcome :scope scope
           :verified-p     (and (eq event-kind ':check) (member scope '(:focused :full))
                                (eq outcome ':verified) (eq verified t))
           :incident-id    (when (telemetry--report-id-p incident-id) incident-id)
           :attempt-id (when (telemetry--report-id-p attempt-id) attempt-id) :safe safe)))))
  nil)

(-> telemetry--terminal-outcome (list) (values keyword (option keyword) (option string)))
(defun telemetry--terminal-outcome (terminal)
  "Map recorded state to outcome, error type and dedicated unfinished error code."
  (let ((state    (getf terminal :state))
        (category (getf terminal :category)))
    (cond ((not (member state '(:completed :succeeded :failed :aborted :cancelled :timed-out)))
           (values ':error ':internal "unfinished-run"))
          ((or (eq (getf terminal :published-p t) nil) (eq category ':result-publication-failed))
           (values ':error ':internal nil))
          ((or (eq state ':timed-out) (eq category ':timeout)
               (eq (getf terminal :cancellation-reason) ':timeout))
           (values ':error ':timeout nil))
          ((eq category ':invalid-output)
           (values ':error ':protocol nil))
          ((member state '(:completed :succeeded))
           (values ':success nil nil))
          ((or (member state '(:aborted :cancelled)) (eq (getf terminal :error-type) ':cancelled))
           (values ':cancelled ':cancelled nil))
          (t
           (values ':error
                   (or (getf terminal :error-type)
                       (and (eq category ':provider-failure) ':provider)
                       (and (not (getf terminal :yielded-p))
                            (plusp (or (getf terminal :request-count) 0)) ':provider)
                       ':internal)
                   nil)))))

(-> telemetry-report-terminal ((option telemetry-run) list) null)
(defun telemetry-report-terminal (run terminal)
  "Keep the worst borrower outcome: error > cancelled > success.
Equal outcomes use lexical error type/code order, independent of completion order.
An owner's explicit terminal result always overrides these fallback candidates.
Stale reports are silently ignored."
  (when run
    (bt:with-lock-held ((telemetry-controller-lock (telemetry-run-controller run)))
      (if (or (telemetry-run-ending-p run) (not (telemetry--live-p run)))
          nil
          (let* ((candidate (multiple-value-list (telemetry--terminal-outcome terminal)))
                 (previous  (telemetry-run-borrower-outcome run)))
            (when (or (null previous)
                      (string> (telemetry--outcome-order candidate)
                               (telemetry--outcome-order previous)))
              (setf (telemetry-run-borrower-outcome run) candidate))))))
  nil)

(-> telemetry--outcome-order (list) string)
(defun telemetry--outcome-order (outcome)
  "Return a stable ordering key for bounded borrower outcome metadata."
  (format nil "~D/~A/~A" (ecase (first outcome) (:success
                                                 0)
                                                (:cancelled
                                                 1)
                                                (:error
                                                 2))
          (or (second outcome) "") (or (third outcome) "")))

(-> telemetry-finish-run ((option telemetry-run) t &optional list) null)
(defun telemetry-finish-run (run token &optional terminal)
  "Finish RUN exactly once for its creator's TOKEN; request a background flush."
  (when (and run token (eq token (telemetry-run-owner run)))
    (telemetry--end-run run terminal))
  nil)

(-> telemetry--end-run (telemetry-run list) null)
(defun telemetry--end-run (run terminal)
  "Queue final root counters and wake the uploader without waiting for network work."
  (let ((controller (telemetry-run-controller run))
        (claimed-p  nil))
    (unwind-protect
         (progn
           (bt:with-lock-held ((telemetry-controller-lock controller))
             (unless (and (telemetry--live-p run) (not (telemetry-run-ending-p run)))
               (return-from telemetry--end-run nil))
             (setf (telemetry-run-ending-p run) t claimed-p t)
             (multiple-value-bind (outcome error-type error-code)
                 (if terminal
                     (telemetry--terminal-outcome terminal)
                     (values-list (or (telemetry-run-borrower-outcome run)
                                      (multiple-value-list (telemetry--terminal-outcome nil)))))
               (setf (telemetry-run-error-code run) error-code)
               (telemetry--export-run run outcome error-type)))
           (telemetry-flush run))
      (when claimed-p
        (bt:with-lock-held ((telemetry-controller-lock controller))
          (clrhash (telemetry-run-tool-failures run))
          (clrhash (telemetry-run-repair-ids run))
          (setf (telemetry-run-active run) nil
                (telemetry-run-previous-digest run) nil
                (telemetry-controller-runs controller)
                (remove run (telemetry-controller-runs controller)))))))
  nil)
