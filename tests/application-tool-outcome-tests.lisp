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
  nil)
