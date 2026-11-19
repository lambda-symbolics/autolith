(in-package #:autolith)

;;;; -- Application Tool Outcomes --

(-> application-tool-outcome-tests--application
    (configuration &key (:identifier string) (:compact-view-p boolean)
                   (:fullscreen-p boolean)) application)
(defun application-tool-outcome-tests--application
    (configuration &key (identifier "outcomes") (compact-view-p t) (fullscreen-p t))
  "Construct an application with an isolated conversation and deterministic UI."
  (setf (config :compact-view-p configuration) compact-view-p)
  (make-instance 'application
                 :configuration configuration
                 :conversation  (conversation-create configuration :identifier identifier)
                 :tool-registry (make-default-tool-registry)
                 :ui            (terminal-ui-create
                                 :terminal     (make-instance 'recording-terminal :columns 100 :rows 24)
                                 :fullscreen-p fullscreen-p)))

(-> application-tool-outcome-tests--call (integer string &key (:name string)) list)
(defun application-tool-outcome-tests--call (sequence call-id &key (name "read"))
  "Build a sequenced provider function-call record with a visible argument."
  (list :provider-item :seq sequence :time 0
        :wire-json (json-encode
                    (json-object "type" "function_call" "namespace" "resource"
                                 "name" name "call_id" call-id
                                 "arguments" "{\"uri\":\"workspace:x\"}"))))

(-> application-tool-outcome-tests--result
    (integer string &key (:status keyword) (:output string)) list)
(defun application-tool-outcome-tests--result (sequence call-id &key (status ':ok) (output "done"))
  "Build a sequenced tool-result record associated with CALL-ID."
  (list :tool-result :seq sequence :time 0 :tool "resource.read"
        :call-id call-id :status status :output output))

(-> application-tool-outcome-tests--completion
    (integer string &key (:outcome keyword) (:summary string)) list)
(defun application-tool-outcome-tests--completion
    (sequence job-id &key (outcome ':success) (summary "finished"))
  "Build a sequenced automatic job-completion message."
  (list :message :seq sequence :time 0 :role ':user :automatic-p t
        :job-completion (list :job-id job-id :outcome outcome :summary summary)))

(-> application-tool-outcome-tests--rows (application) list)
(defun application-tool-outcome-tests--rows (application)
  "Return the committed fullscreen transcript as plain rows."
  (let ((viewport (fullscreen-terminal-ui-viewport (application-ui application))))
    (loop for row below (clinedi:transcript-viewport-row-count viewport)
          collect (clinedi:transcript-viewport-row-text viewport row))))

(-> application-tool-outcome-tests--text (application) string)
(defun application-tool-outcome-tests--text (application)
  "Return the committed fullscreen transcript as newline-separated text."
  (format nil "~{~A~^~%~}" (application-tool-outcome-tests--rows application)))

(-> application-tool-outcome-tests--present (application &rest list) (integer 0))
(defun application-tool-outcome-tests--present (application &rest records)
  "Present RECORDS through the application batch presentation path."
  (application--present-conversation-records application records))

(-> application-tool-outcome-tests--schemas (integer list) list)
(defun application-tool-outcome-tests--schemas (sequence names)
  "Build a provider schema lookup with namespace NAMES in discovery order."
  (list :provider-item :seq sequence :time 0
        :wire-json (json-encode
                    (json-object
                     "type" "tool_search_output"
                     "tools" (coerce (loop for name in names
                                           collect (json-object "type" "namespace" "name" name))
                                     'vector)))))

(-> test-transcript-schema-coalescing () null)
(defun test-transcript-schema-coalescing ()
  "Coalesce adjacent resident schema lookups across batches, styles and relayout."
  (dolist (style '(:labels :repl))
    (dolist (batch-p '(nil t))
      (with-test-configuration (configuration)
        (setf (config :transcript-style configuration) style)
        (let* ((application (application-tool-outcome-tests--application configuration))
               (ui          (application-ui application))
               (viewport    (fullscreen-terminal-ui-viewport ui))
               (records     (list (application-tool-outcome-tests--schemas 1 '("web" "search"))
                                  (application-tool-outcome-tests--schemas 2 nil)
                                  (application-tool-outcome-tests--schemas
                                   3 '("skill" "search" "shodan"))
                                  (application-tool-outcome-tests--schemas 4 '("web")))))
          (if batch-p
              (application--present-conversation-records application records)
              (dolist (record records)
                (application--present-conversation-record application record)))
          (test-assert (= 1 (clinedi:transcript-viewport-chunk-count viewport))
                       "consecutive visible lookups share one resident chunk")
          (test-assert (search "web, search, skill, shodan"
                               (application-tool-outcome-tests--text application))
                       "merged namespaces retain first discovery order without repeats")
          (application--present-conversation-records application records)
          (test-assert (= 1 (clinedi:transcript-viewport-chunk-count viewport))
                       "replayed merged identifiers do not produce duplicate chunks")
          (clinedi:transcript-viewport-resize viewport 28)
          (test-assert (every (lambda (row) (<= (text-cell-width row) 28))
                              (application-tool-outcome-tests--rows application))
                       "merged schema rows respect the resized viewport")
          (clinedi:transcript-viewport-resize viewport 100)
          (test-assert (search "web, search, skill, shodan"
                               (application-tool-outcome-tests--text application))
                       "widening recovers all merged names from semantic sources")))))
  (with-test-configuration (configuration)
    (let* ((application  (application-tool-outcome-tests--application configuration))
           (presentation (application-terminal-ui-create
                          :client-pane   '(:rows 24 :columns 100)
                          :configuration configuration))
           (terminal     (terminal-ui-terminal presentation)))
      (setf (application-ui application) presentation)
      (with-terminal-ui (presentation presentation)
        (test-assert (and (not (terminal-interactive-p terminal))
                          (not (terminal-plain-output-p terminal)))
                     "a started presentation without clients has no append-only sink")
        (application-tool-outcome-tests--present
         application (application-tool-outcome-tests--schemas 1 '("web" "search")))
        (application-tool-outcome-tests--present
         application (application-tool-outcome-tests--schemas 2 '("skill" "shodan")))
        (with-terminal-ui (client (terminal-ui-create
                                   :terminal     (make-instance 'recording-terminal :columns 100 :rows 24)
                                   :fullscreen-p t))
          (dolist (event (mapcar #'client-portable (channel-presentation-replay presentation)))
            (apply #'presentation-apply client (getf (rest event) :operation) (rest event)))
          (let ((viewport (fullscreen-terminal-ui-viewport client)))
            (test-assert (and (= 1 (clinedi:transcript-viewport-chunk-count viewport))
                              (search "web, search, skill, shodan"
                                      (format nil "~{~A~^~%~}"
                                              (loop for row below (clinedi:transcript-viewport-row-count viewport)
                                                    collect (clinedi:transcript-viewport-row-text viewport row)))))
                         "a client that attaches later coalesces the schema rows the channel presented before it"))))))
  (with-test-configuration (configuration)
    (let* ((application (application-tool-outcome-tests--application configuration))
           (ui          (application-ui application))
           (terminal    (terminal-ui-terminal ui)))
      (with-terminal-ui (ui ui)
        (setf (terminal-interactive-p terminal) nil)
        (application-tool-outcome-tests--present
         application (application-tool-outcome-tests--schemas 1 '("web" "search"))
                     (application-tool-outcome-tests--schemas 2 '("skill" "shodan")))
        (test-assert (and (search "web, search" (recording-terminal-output terminal))
                          (search "skill, shodan" (recording-terminal-output terminal)))
                     "append-only terminal output includes every lookup's namespaces"))))
  (let ((*terminal-ui-finalized-identifier-limit* 64))
    (with-test-configuration (configuration)
      (let* ((application (application-tool-outcome-tests--application configuration))
             (ui          (application-ui application)))
        (loop for sequence from 1 to 200
              do (application-tool-outcome-tests--present
                  application (application-tool-outcome-tests--schemas sequence '("web"))))
        (test-assert (and (<= (hash-table-count (terminal-ui-finalized-identifiers ui)) 64)
                          (loop for record being the hash-values of (fullscreen-terminal-ui-presentation-records ui)
                                always (<= (length (getf record :identifiers)) 64)))
                     "duplicate namespace lookups cannot retain unlimited merged identities"))))
  nil)

(-> test-transcript-schema-interruptions () null)
(defun test-transcript-schema-interruptions ()
  "Keep schema groups separate when any visible entry interrupts discovery."
  (dolist (style '(:labels :repl))
    (dolist (interruption '(:side-status :message :call :stream :tail :cleared-tail))
      (with-test-configuration (configuration)
        (setf (config :transcript-style configuration) style)
        (let* ((application (application-tool-outcome-tests--application configuration))
               (ui          (application-ui application))
               (viewport    (fullscreen-terminal-ui-viewport ui)))
          (setf (terminal-interactive-p (terminal-ui-terminal ui)) t)
          (with-terminal-ui (ui ui)
            (application-tool-outcome-tests--present
             application (application-tool-outcome-tests--schemas 1 '("web" "search")))
            (case interruption
              (:side-status
               (application-tool-outcome-tests--present
                application (list :side-status :seq 2 :spans (list (terminal-span ':notice "child note")))))
              (:message
               (application-tool-outcome-tests--present
                application (list :message :seq 2 :role ':user :content "interruption" :time 0)))
              (:call
               (application-tool-outcome-tests--present
                application (application-tool-outcome-tests--call 2 "ordinary-call")))
              (:stream
               (terminal-ui-stream-update
                ui :rows (list (list (terminal-span ':plain "streamed text")))))
              ((:tail :cleared-tail)
               (terminal-ui-stream-update ui :tail "unfinished streamed text")
               (when (eq interruption ':cleared-tail)
                 (terminal-ui-stream-update ui :tail nil))))
            (let ((before (clinedi:transcript-viewport-chunk-count viewport)))
              (application-tool-outcome-tests--present
               application (application-tool-outcome-tests--schemas 3 '("skill" "shodan")))
              (application-tool-outcome-tests--present
               application (application-tool-outcome-tests--schemas 4 '("skill")))
              (test-assert (= (+ before (if (eq interruption ':tail) 2 1))
                              (clinedi:transcript-viewport-chunk-count viewport))
                           "only uninterrupted visible lookups share the next schema chunk")
              (test-assert (not (search "search, skill"
                                       (application-tool-outcome-tests--text application)))
                           "an interruption prevents merging back into an earlier schema row")))))))
  nil)

(-> test-application-resource-edit-failure-placement () null)
(defun test-application-resource-edit-failure-placement ()
  "Present a short failed operation before its proposed diff, including after resize."
  (with-test-configuration (configuration)
    (let* ((application (application-tool-outcome-tests--application configuration))
           (revision    "RE081CDEE7B09FDB2")
           (call        (list :provider-item :seq 1 :time 0
                              :wire-json
                              (json-encode
                               (json-object
                                "type" "function_call" "namespace" "resource" "name" "edit"
                                "call_id" "edit"
                                "arguments"
                                (json-encode
                                 (json-object "uri" "workspace:example.lisp" "base-revision" revision
                                              "operations" (json-array
                                                            (json-object "op" "insert-after" "line" 197
                                                                         "content" "requested-change"))))))))
           (output      (format nil "resource.edit failed: Resource revision ~A is stale, expired, or was not observed in this conversation. Reread workspace:example.lisp with resource.read and retry against the returned revision." revision))
           (result      (list :tool-result :seq 2 :time 0 :tool "resource.edit" :call-id "edit"
                              :status ':error :output output)))
      (application-tool-outcome-tests--present application call result)
      (dolist (columns '(100 40 100))
        (terminal-ui-resize (application-ui application) columns :rows 24)
        (let* ((text    (application-tool-outcome-tests--text application))
               (heading (search "operation 1" text))
               (error   (search "resource revision" text))
               (change  (search "requested-change" text)))
          (test-assert (and heading error change (< heading error change))
                       "the failure appears between its operation heading and proposed change")
          (test-assert (and (null (search "Reread" text))
                            (search "Reread" (getf (rest result) :output)))
                       "the transcript omits retry advice retained in the agent's result")))
      (let* ((spans   (application--resource-failure-spans
                       (format nil "operation 1 · insert after line 197 · resource revision ~A is stale" revision)))
             (neutral (remove-if-not (lambda (span) (eq (terminal-span-style span) ':plain)) spans)))
        (test-assert (equal (mapcar #'terminal-span-text neutral) (list "1" "197" revision))
                     "only operation numbers, line numbers and revision identifiers are neutral")
        (test-assert (every (lambda (span)
                              (member (terminal-span-style span) '(:plain :failure))) spans)
                     "the rest of the failed operation and summary use the failure color"))))
  nil)

(-> test-application-tool-outcomes-interleaved () null)
(defun test-application-tool-outcomes-interleaved ()
  "Associate same-name calls by ID, preserving arguments and railed failure output."
  (with-test-configuration (configuration)
    (let* ((application (application-tool-outcome-tests--application configuration))
           (viewport    (fullscreen-terminal-ui-viewport (application-ui application))))
      (application-tool-outcome-tests--present
       application
       (application-tool-outcome-tests--call 1 "a")
       (application-tool-outcome-tests--call 2 "b")
       (application-tool-outcome-tests--result 3 "b" :status ':error :output "second failed")
       (application-tool-outcome-tests--result 4 "a" :output "first succeeded"))
      (let ((first  (clinedi:transcript-viewport-chunk-text viewport 0))
            (second (clinedi:transcript-viewport-chunk-text viewport 1)))
        (test-assert (and (= 2 (clinedi:transcript-viewport-chunk-count viewport))
                          (search "✓ (resource.read :uri" first)
                          (search "! (resource.read :uri" second)
                          (search "workspace:x" first) (search "workspace:x" second)
                          (search "second failed" second)
                          (not (search "first succeeded" first)))
                     "each original invocation receives only its own outcome")
        (test-assert
         (some (lambda (row) (and (search "│" row) (search "second failed" row)))
               (application-tool-outcome-tests--rows application))
         "failure output has a rail beneath the invocation"))))
  nil)

(-> test-application-tool-outcomes-scoped-identities () null)
(defun test-application-tool-outcomes-scoped-identities ()
  "Scope equal provider IDs when applications share a fullscreen transcript."
  (with-test-configuration (configuration)
    (let* ((first  (application-tool-outcome-tests--application configuration :identifier "first"))
           (second (application-tool-outcome-tests--application configuration :identifier "second")))
      (setf (application-ui second) (application-ui first))
      (application-tool-outcome-tests--present first (application-tool-outcome-tests--call 1 "same"))
      (application-tool-outcome-tests--present second (application-tool-outcome-tests--call 1 "same"))
      (application-tool-outcome-tests--present first (application-tool-outcome-tests--result 2 "same"))
      (let ((text (application-tool-outcome-tests--text first)))
        (test-assert (and (search "✓ (resource.read :uri" text)
                          (search "▸ (resource.read :uri" text))
                     "an outcome does not complete another conversation's equal call ID"))))
  nil)

(-> test-application-tool-outcomes-compact-and-prominent () null)
(defun test-application-tool-outcomes-compact-and-prominent ()
  "Update hidden successes and retain expanded output and prominent result notices."
  (dolist (compact-p '(t nil))
    (with-test-configuration (configuration)
      (let ((application (application-tool-outcome-tests--application
                          configuration :compact-view-p compact-p)))
        (application-tool-outcome-tests--present
         application (application-tool-outcome-tests--call 1 "success")
         (application-tool-outcome-tests--result 2 "success" :output "ordinary output"))
        (let ((text (application-tool-outcome-tests--text application)))
          (test-assert (and (search "✓ (resource.read :uri" text)
                            (eql (not compact-p) (not (null (search "ordinary output" text)))))
                       "only expanded presentation retains ordinary successful output"))
        (let ((call   (application-tool-outcome-tests--call 3 "notice"))
              (result (application-tool-outcome-tests--result 4 "notice" :output "report details")))
          (setf (getf (rest call) :wire-json)
                (json-encode (json-object "type" "function_call" "namespace" "papercut"
                                          "name" "report" "call_id" "notice" "arguments" "{}"))
                (getf (rest result) :tool) "papercut.report")
          (application-tool-outcome-tests--present application call result)
          (let ((text (application-tool-outcome-tests--text application)))
            (test-assert (and (search "✓ (papercut.report)" text)
                              (search "PAPERCUT RECORDED" text) (search "report details" text))
                         "successful prominent notices retain their heading and body"))))))
  nil)

(-> test-application-tool-outcomes-background-lifecycle () null)
(defun test-application-tool-outcomes-background-lifecycle ()
  "Keep handoffs pending, then complete their own call from job identity."
  (dolist (compact-p '(t nil))
    (dolist (outcome '(:success :failure :cancelled :aborted))
      (with-test-configuration (configuration)
        (let* ((application (application-tool-outcome-tests--application
                             configuration :compact-view-p compact-p))
               (handoff     (application-tool-outcome-tests--result 2 "handoff" :output "background")))
          (setf (getf (rest handoff) :details)
                (list :tool-execution :handed-off-p t :job (list :id "job-1")))
          (application-tool-outcome-tests--present
           application (application-tool-outcome-tests--call 1 "handoff") handoff)
          (test-assert (search "▸ (resource.read :uri" (application-tool-outcome-tests--text application))
                       "handed-off invocations are pending before terminal completion")
          (application-tool-outcome-tests--present
           application (application-tool-outcome-tests--completion
                        3 "job-1" :outcome outcome :summary "terminal summary"))
          (let* ((success-p (eq outcome ':success))
                 (retain-p  (or (not success-p) (not compact-p)))
                 (text      (application-tool-outcome-tests--text application)))
            (test-assert (and (search (if success-p "✓ (resource.read :uri" "! (resource.read :uri") text)
                              (eql retain-p (not (null (search "terminal summary" text)))))
                         "job outcomes retain failures and expanded successful summaries"))))))
  nil)

(-> test-application-tool-outcomes-inline-compatibility () null)
(defun test-application-tool-outcomes-inline-compatibility ()
  "Present inline calls and results without replacing original invocation output."
  (with-test-configuration (configuration)
    (let* ((application (application-tool-outcome-tests--application configuration :fullscreen-p nil))
           (ui          (application-ui application))
           (call        (application-tool-outcome-tests--call 1 "inline"))
           (result      (application-tool-outcome-tests--result 2 "inline" :status ':error
                                                               :output             "inline failure")))
      (application-tool-outcome-tests--present application call result)
      (let ((text (clinedi:ansi-strip (recording-terminal-output (terminal-ui-terminal ui)))))
        (test-assert (and (search "▸ (resource.read :uri" text) (search "inline failure" text))
                     "inline output includes its original invocation and separate error"))))
  nil)

(-> test-application-tool-outcomes-transient () null)
(defun test-application-tool-outcomes-transient ()
  "Reconcile non-durable results by call ID without duplicating invocation output."
  (dolist (success-p '(t nil))
    (with-test-configuration (configuration)
      (let* ((application (application-tool-outcome-tests--application configuration))
             (ui          (application-ui application))
             (details     (list :tool "resource.read" :call-id "transient"
                                :success-p success-p :output "transient details")))
        (application-tool-outcome-tests--present
         application (application-tool-outcome-tests--call 1 "transient"))
        (application--present-transient-tool-result application details)
        (application--present-transient-tool-result application details)
        (let ((text (application-tool-outcome-tests--text application)))
          (test-assert
           (and (= 1 (clinedi:transcript-viewport-chunk-count
                      (fullscreen-terminal-ui-viewport ui)))
                (search (if success-p "✓ (resource.read :uri" "! (resource.read :uri") text)
                (search "workspace:x" text)
                (eql (not success-p) (not (null (search "transient details" text)))))
           "transient outcomes update their invocation once with the correct visible body")))))
  (dolist (success-p '(t nil))
    (with-test-configuration (configuration)
      (let* ((application (application-tool-outcome-tests--application
                           configuration :fullscreen-p nil))
             (details     (list :tool "resource.read" :call-id "inline-transient"
                                :success-p success-p :output (if success-p "" "transient details"))))
        (setf (terminal-interactive-p (terminal-ui-terminal (application-ui application))) nil)
        (application--present-transient-tool-result application details)
        (application--present-transient-tool-result application details)
        (let* ((ui   (application-ui application))
               (text (clinedi:ansi-strip
                      (recording-terminal-output (terminal-ui-terminal ui)))))
          (test-assert (eql (not success-p)
                            (not (null (search (format nil "~%~%") text))))
                       "single-row inline transient successes compact; failures use blocks")))))
  nil)
