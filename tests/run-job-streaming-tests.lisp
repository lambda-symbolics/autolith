(in-package #:autolith)

;;;; -- Headless Streaming Boundary Tests --

(defclass run-job-streaming-publication-stream (run-job-event-test-stream)
  ((path :initarg :path :reader run-job-streaming-publication-stream-path
         :documentation "The authoritative output whose publication is observed.")
   (observations :initform nil :accessor run-job-streaming-publication-stream-observations
                 :documentation "Terminal wire status and the status read at its write boundary."))
  (:documentation "Observe persisted outcomes before terminal event delivery."))

(defmethod trivial-gray-streams:stream-write-string :before
    ((stream run-job-streaming-publication-stream) text &optional (start 0) end)
  "Record the artifact visible when a complete terminal event is written."
  (let* ((record (run-job-event-read-string (subseq text start end)))
         (fields (rest record))
         (data (getf fields :data)))
    (when (member (getf fields :kind) '(:job-finished :run-finished))
      (push (list (getf data :status)
                  (getf data :published-p)
                  (and (probe-file (run-job-streaming-publication-stream-path stream))
                       (getf (rest (run-job-read-file
                                    (run-job-streaming-publication-stream-path stream))) :status)))
            (run-job-streaming-publication-stream-observations stream)))))

(-> run-job-streaming-tests--input (pathname) pathname)
(defun run-job-streaming-tests--input (path)
  "Write a valid request containing synthetic private caller data."
  (let ((form (run-job-tests--request-form :prompt "PRIVATE-TEST-PROMPT"
                                          :input '(:value "PRIVATE-TEST-INPUT"))))
    (setf (getf (rest form) :id) "PRIVATE-TEST-CALLER-ID")
    (task-tests--write-text path (run-job--write-data-sexp form :pretty-p t))))

(-> run-job-streaming-tests--kinds (list) list)
(defun run-job-streaming-tests--kinds (records)
  "Return the kinds in the emitted sequence."
  (mapcar (lambda (record) (getf (rest record) :kind)) records))

(-> test-run-job-streaming-terminal-publication () null)
(defun test-run-job-streaming-terminal-publication ()
  "Match streamed terminal outcomes to the atomically published artifact."
  (with-test-configuration (configuration root)
    (let ((input (merge-pathnames "job.sexp" root)))
      (run-job-streaming-tests--input input)
      (dolist (case '((:succeeded nil 0) (:failed :child-failure 1)
                      (:timed-out :timeout 1) (:cancelled :cancelled 1)
                      (:invalid-output :invalid-output 1)))
        (destructuring-bind (expected category exit-code) case
          (let* ((output (merge-pathnames (format nil "~(~A~).sexp" expected) root))
                 (stream (make-instance 'run-job-streaming-publication-stream :path output))
                 (diagnostics (make-string-output-stream))
                 (*error-output* diagnostics)
                 (*standard-input* (make-string-input-stream "interactive input"))
                 (status
                   (run-job-run
                    input output ':full-access :configuration configuration
                    :events ':sexp :event-output stream
                    :executor
                    (lambda (active-configuration request permissions)
                      (declare (ignore active-configuration request))
                      (assert (eq permissions ':full-access))
                      (labels ((diagnose (name)
                                 (dolist (io (list *standard-input* *query-io* *terminal-io* *debug-io*))
                                   (assert (null (read-char io nil nil))))
                                 (write-line (format nil "~A diagnostic" name))
                                 (write-line (format nil "~A query diagnostic" name) *query-io*)
                                 (write-line (format nil "~A terminal diagnostic" name) *terminal-io*)))
                        (diagnose "main-thread")
                        (join-thread
                         (make-thread
                          (lambda ()
                            (diagnose "worker-thread")
                            (join-thread (make-thread (lambda () (diagnose "nested-worker"))))))))
                      (values (if (eq expected ':invalid-output) ':succeeded expected)
                              (if (eq expected ':invalid-output)
                                  '(:object ("wrong" "value"))
                                  '(:object ("answer" "PRIVATE-TEST-RESULT")))
                              "test-trace" '(:input-tokens 2 :output-tokens 3 :provider-requests 1)
                              category "PRIVATE-TEST-FAILURE")))))
            (test-assert (= status exit-code) "streaming preserves the outcome's exit status")
            (let* ((text (get-output-stream-string (run-job-event-test-stream-output stream)))
                   (records (run-job-event-tests--records text))
                   (terminal (getf (rest (first (last records))) :data))
                   (actual-status (if (eq expected ':invalid-output) ':failed expected)))
              (test-assert (and (eq (first (run-job-streaming-tests--kinds records)) ':run-started)
                                (eq (first (last (run-job-streaming-tests--kinds records))) ':run-finished)
                                (eq (getf terminal :status) actual-status)
                                (getf terminal :published-p))
                           "the final event reports the committed result")
              (test-assert
               (every (lambda (observation)
                        (and (eq (first observation) actual-status)
                             (second observation)
                             (eq (third observation) actual-status)))
                      (run-job-streaming-publication-stream-observations stream))
               "terminal events follow artifact publication")
              (test-assert (run-job-streaming-publication-stream-observations stream)
                           "the writer observes terminal publication")
              (test-assert (not (search "PRIVATE-TEST-" text))
                           "caller data, results and failure messages are excluded from events")
              (test-assert (string= (getf (rest (run-job-read-file output)) :id)
                                   "PRIVATE-TEST-CALLER-ID")
                           "the authoritative artifact preserves the caller identity")
              (let ((job-terminal (find ':job-finished records
                                        :key (lambda (record) (getf (rest record) :kind)))))
                (test-assert
                 (and (equal (getf (getf (rest job-terminal) :data) :job-id) "test-trace")
                      (equal (getf (getf (rest job-terminal) :data) :execution-id) "test-trace")
                      (getf (getf (rest job-terminal) :data) :result-uri))
                 "root terminal identity and artifact reference match its executor execution"))
              (let ((output (get-output-stream-string diagnostics)))
                (test-assert (every (lambda (name)
                                      (and (search (format nil "~A diagnostic" name) output)
                                           (search (format nil "~A query diagnostic" name) output)
                                           (search (format nil "~A terminal diagnostic" name) output)))
                                    '("main-thread" "worker-thread" "nested-worker"))
                             "producer diagnostics, including new threads, are outside the event stream"))))))))
  nil)

(-> test-run-job-streaming-input-and-publication-failures () null)
(defun test-run-job-streaming-input-and-publication-failures ()
  "Report invalid input and failed publication without announcing success."
  (with-test-configuration (configuration root)
    (let* ((input (merge-pathnames "invalid.sexp" root))
           (output (merge-pathnames "invalid-result.sexp" root))
           (stream (make-string-output-stream))
           (executed-p nil))
      (task-tests--write-text input "#.(error \"PRIVATE-TEST-READER\")")
      (test-assert
       (= 64 (run-job-run input output ':auto :configuration configuration
                          :events ':sexp :event-output stream
                          :executor (lambda (&rest arguments)
                                      (declare (ignore arguments)) (setf executed-p t))))
       "invalid input retains its data-error exit code")
      (let* ((records (run-job-event-tests--records (get-output-stream-string stream)))
             (terminal (getf (rest (first (last records))) :data)))
        (test-assert (and (not executed-p) (eq (getf terminal :status) ':failed)
                          (eq (getf terminal :category) ':invalid-input)
                          (getf terminal :published-p))
                     "invalid input publishes a failure before the final event")))
    (let* ((input (merge-pathnames "valid.sexp" root))
           (output (merge-pathnames "output-directory/" root))
           (stream (make-string-output-stream)))
      (run-job-streaming-tests--input input)
      (ensure-directories-exist output)
      (test-assert
       (not (zerop (run-job-run
                    input output ':auto :configuration configuration
                    :events ':sexp :event-output stream
                    :executor (lambda (&rest arguments)
                                (declare (ignore arguments))
                                (values ':succeeded '(:object ("answer" "yes")) nil nil nil nil)))))
       "result publication failure exits nonzero")
      (let* ((records (run-job-event-tests--records (get-output-stream-string stream)))
             (terminal (getf (rest (first (last records))) :data)))
        (test-assert (and (eq (getf terminal :status) ':failed)
                          (not (getf terminal :published-p))
                          (eq (getf terminal :category) ':result-publication-failed))
                     "publication failure cannot announce an uncommitted success"))))
  nil)

(-> test-run-job-streaming-broken-consumer () null)
(defun test-run-job-streaming-broken-consumer ()
  "Continue result execution when event output fails."
  (with-test-configuration (configuration root)
    (let ((input (merge-pathnames "job.sexp" root))
          (output (merge-pathnames "result.sexp" root))
          (stream (make-instance 'run-job-event-test-stream :mode ':broken))
          (executed-p nil)
          (*error-output* (make-string-output-stream)))
      (run-job-streaming-tests--input input)
      (test-assert
       (zerop (run-job-run
               input output ':sandboxed :configuration configuration
               :events ':sexp :event-output stream
               :executor (lambda (active-configuration request permissions)
                           (declare (ignore active-configuration request))
                           (assert (eq permissions ':sandboxed))
                           (setf executed-p t)
                           (values ':succeeded '(:object ("answer" "yes")) nil nil nil nil))))
       "broken streaming does not fail a successful job")
      (test-assert (and executed-p
                        (eq (getf (rest (run-job-read-file output)) :status) ':succeeded))
                   "the successful result is persisted independently of the event pipe")))
  nil)

(-> test-run-job-streaming-owned-observation () null)
(defun test-run-job-streaming-owned-observation ()
  "Project runtime callbacks only for the admitted root and durable owned descendants."
  (with-test-configuration (configuration directory)
    (declare (ignore directory))
    (let* ((registry (make-instance 'tool-registry))
           (parent (task-tests--primary-agent configuration "streaming-scope" registry))
           (orchestrator (task-orchestrator-create))
           (definition (task-agent-definition-create
                        :name "streaming-role" :description "Observer fixture."
                        :instructions "Return a result." :source ':test))
           (stream (make-string-output-stream))
           (emitter (run-job-event-emitter-create "run-scoped" stream))
           (listener (run-job-event-observer-create
                      orchestrator emitter :configuration configuration :registry registry
                      :root-conversation "streaming-scope" :root-name "PRIVATE-ROOT-ALIAS")))
      (tool-registry-register
       registry (make-instance 'task-test-effect-tool :namespace "test" :name "effect"
                               :description "Record a test effect."
                               :parameters (tool-object-schema (json-object) nil)))
      (task-orchestrator-add-listener orchestrator listener)
      (unwind-protect
           (let* ((root (task-tests--register-job orchestrator parent definition
                                                :name "PRIVATE-ROOT-ALIAS"))
                  (child (task-tests--register-job
                          orchestrator parent definition :name "PRIVATE-CHILD-ALIAS"
                          :owner-identifiers (list (job-identifier root))))
                  (grandchild (task-tests--register-job
                               orchestrator parent definition :name "PRIVATE-GRANDCHILD-ALIAS"
                               :owner-identifiers (list (job-identifier root) (job-identifier child))))
                  (foreign (task-tests--register-job orchestrator parent definition
                                                   :name "PRIVATE-FOREIGN-ROOT"))
                  (other-session (task-tests--register-job
                                  orchestrator parent definition :name "PRIVATE-OTHER-SESSION"
                                  :owner-identifiers (list (job-identifier root))
                                  :root-conversation-identifier "other-conversation")))
             (run-job-event-emit emitter ':run-started nil)
             ;; The transition is not observable until continuity is durable.
             (task-orchestrator-emit orchestrator ':task-subagent-lifecycle
                                     (list :id (job-identifier root) :status ':started))
             (dolist (job (list root child grandchild foreign other-session))
               (task-continuity-record-job job parent)
               (task-orchestrator-emit orchestrator ':task-subagent-lifecycle
                                       (list :id (job-identifier job) :status ':started))
               (task-progress-note-status job ':provider-request-started '(:request-number 2))
               (task-progress-note-status
                job ':provider-request-completed
                '(:usage (("input_tokens" 5) ("output_tokens" 7) ("total_tokens" 12))
                  :message "PRIVATE-PROVIDER-BODY")))
             (task-progress-note-status root ':tool-call-started
                                        '(:tool "test.effect" :call-id "PRIVATE-CALL-ID"
                                          :arguments "PRIVATE-ARGUMENTS"))
             (task-progress-note-status root ':tool-call-completed
                                        '(:tool "test.effect" :call-id "PRIVATE-CALL-ID"
                                          :success-p t :result "PRIVATE-RESULT-BODY"))
             (task-progress-note-status child ':tool-call-started
                                        '(:tool "PRIVATE-UNKNOWN-TOOL" :call-id "PRIVATE-OTHER-CALL-ID"))
             (task-tests--publish-terminal
              child ':completed (task-tests--terminal-result child :output "PRIVATE-CHILD-RESULT"))
             (task-tests--publish-terminal
              root ':completed (task-tests--terminal-result root :output "PRIVATE-ROOT-RESULT"))
             (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                          "connected owned observation completes")
             (let* ((text (get-output-stream-string stream))
                    (records (run-job-event-tests--records text))
                    (starts (remove-if-not
                             (lambda (record) (eq (getf (rest record) :kind) ':job-started)) records))
                    (finishes (remove-if-not
                               (lambda (record) (eq (getf (rest record) :kind) ':job-finished)) records))
                    (tools (remove-if-not
                            (lambda (record) (member (getf (rest record) :kind)
                                                     '(:tool-started :tool-finished))) records))
                    (usage (find ':usage records :key (lambda (record) (getf (rest record) :kind)))))
               (test-assert (= (length starts) 3)
                            "one durable start per owned job excludes premature and foreign transitions")
               (test-assert
                (every (lambda (record)
                         (let ((id (getf (getf (rest record) :data) :job-id)))
                           (or (null id)
                               (member id (mapcar #'session-job-execution-identifier
                                                   (list root child grandchild)) :test #'equal))))
                       records)
                "wire identifiers are execution UUIDs scoped to this root and conversation")
               (test-assert
                (and (= (length finishes) 1)
                     (equal (getf (getf (rest (first finishes)) :data) :job-id)
                            (session-job-execution-identifier child))
                     (getf (getf (rest (first finishes)) :data) :published-p))
                "only the durably published descendant terminal is observed before outer publication")
               (test-assert (= (length tools) 3) "provider callbacks supply tool boundaries")
               (test-assert
                (and (equal (getf (getf (rest (first tools)) :data) :tool-name) "test.effect")
                     (equal (getf (getf (rest (third tools)) :data) :tool-name) "unknown")
                     (equal (getf (getf (rest (first tools)) :data) :call-id)
                            (getf (getf (rest (second tools)) :data) :call-id)))
                "known tool names and stable pseudonymized calls are projected")
               (test-assert (and (= (getf (getf (rest usage) :data) :input-tokens) 5)
                                 (= (getf (getf (rest usage) :data) :output-tokens) 7)
                                 (= (getf (getf (rest usage) :data) :total-tokens) 12)
                                 (= (getf (getf (rest usage) :data) :provider-requests) 2))
                            "normalized provider usage counters are emitted")
               (test-assert (not (search "PRIVATE-" text))
                            "caller aliases, provider text, arguments and result bodies are absent")))
        (task-orchestrator-remove-listener orchestrator listener)
        (task-orchestrator-close orchestrator)
        (run-job-event-emitter-finish emitter (run-job-event-tests--terminal)))))
  nil)

(-> test-run-job-streaming-cli () null)
(defun test-run-job-streaming-cli ()
  "Read actual launcher stdout as forms, keeping disabled and invalid modes distinct."
  (with-test-fixture (':posix-shell "the run-job launcher stream")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((launcher (merge-pathnames "bin/autolith" (asdf:system-source-directory :autolith)))
             (input (merge-pathnames "unsafe.sexp" root))
             (environment (append
                           (list "env"
                                 (format nil "XDG_CONFIG_HOME=~A" (merge-pathnames "config/" root))
                                 (format nil "XDG_STATE_HOME=~A" (merge-pathnames "state/" root))
                                 (format nil "XDG_CACHE_HOME=~A" (merge-pathnames "cache/" root)))
                           (test-active-core-environment))))
        (task-tests--write-text input "#.(error \"PRIVATE-READER-EVALUATION\")")
        (dolist (mode '(nil "sexp" "json"))
          (let ((output (merge-pathnames (format nil "~A-result.sexp" (or mode "legacy")) root)))
            (multiple-value-bind (stdout stderr exit-code)
                (uiop:run-program
                 (append environment
                         (list (namestring launcher) "run-job"
                               "--input" (namestring input) "--output" (namestring output))
                         (when mode (list "--events" mode)))
                 :output ':string :error-output ':string :ignore-error-status t)
              (test-assert (not (zerop exit-code)) "invalid input and unsupported event modes exit nonzero")
              (cond
                ((equal mode "json")
                 (test-assert (and (zerop (length stdout)) (plusp (length stderr))
                                   (not (probe-file output)))
                              "unsupported serialization is rejected on stderr before execution"))
                (t
                 (test-assert (= exit-code 64) "the launcher preserves data-error exit status")
                 (test-assert (eq (getf (rest (run-job-read-file output)) :status) ':failed)
                              "invalid input publishes the authoritative failure artifact")
                 (if mode
                     (let* ((records (run-job-event-tests--records stdout))
                            (terminal (getf (rest (first (last records))) :data)))
                       (test-assert (and (equal (run-job-streaming-tests--kinds records)
                                                '(:run-started :run-finished))
                                         (eq (getf terminal :status) ':failed)
                                         (getf terminal :published-p))
                                    "actual CLI stdout consists solely of committed event forms")
                       (test-assert (not (search "PRIVATE-" stdout)) "unsafe input is absent from the stream"))
                     (test-assert (zerop (length stdout)) "legacy headless execution has no event stdout"))))))))))
  nil)

(-> test-run-job-streaming-cli-blocked-exit () null)
(defun test-run-job-streaming-cli-blocked-exit ()
  "A one-shot CLI must exit with its result status even while native stdout is blocked."
  (with-test-configuration (configuration root)
    (let* ((input (merge-pathnames "job.sexp" root))
           (output (merge-pathnames "result.sexp" root))
           (error-path (merge-pathnames "child-stderr.log" root))
           (code
             `(let ((*standard-output* (sb-ext:symbol-global-value '*standard-output*)))
                (setf (symbol-function 'run-job-execute-with-application)
                      (lambda (configuration request permission-mode)
                        (declare (ignore configuration request permission-mode))
                        (dotimes (count 200)
                          (run-job-event-emit
                           *run-job-event-emitter* ':tool-finished
                           (list :resource-uris (list (make-string 4000 :initial-element #\a)))))
                        (values ':succeeded '(:object ("answer" "yes"))
                                (make-identifier) nil nil nil)))
                (main (list "run-job" "--input" ,(namestring input)
                            "--output" ,(namestring output) "--events" "sexp"))
                (uiop:quit 0)))
           (process nil))
      (run-job-streaming-tests--input input)
      (unwind-protect
           (progn
             ;; Boot the checked core through its normal worker entry, then invoke the
             ;; actual CLI handler with a deterministic executor. Keep stdout unread.
             (setf process
                   (uiop:launch-program
                    (active-image-process-command configuration '("--worker"))
                    :input ':stream :output ':stream :error-output error-path))
             (let ((*package* (find-package :autolith))
                   (*print-readably* nil) (*print-escape* t)
                   (*print-pretty* nil) (*print-circle* nil))
               (write (list :request :id 1 :operation ':eval
                            :arguments (list :forms (list (write-to-string code))))
                      :stream (uiop:process-info-input process))
               (terpri (uiop:process-info-input process))
               (finish-output (uiop:process-info-input process)))
             (test-assert (task-tests--wait-until (lambda () (probe-file output)) 5)
                          "native backpressure does not prevent artifact publication")
             (test-assert (task-tests--wait-until
                           (lambda () (not (uiop:process-alive-p process))) 3)
                          "CLI exits without waiting for its native blocked writer")
             (unless (uiop:process-alive-p process)
               (test-assert (zerop (uiop:wait-process process))
                            "stdout failure cannot change the successful result's exit status"))
             (when (probe-file output)
               (test-assert (eq (getf (rest (run-job-read-file output)) :status) ':succeeded)
                            "the authoritative result survives one-shot process shutdown")))
        (when process
          (when (uiop:process-alive-p process)
            (uiop:terminate-process process :urgent t))
          (when (task-tests--wait-until (lambda () (not (uiop:process-alive-p process))) 3)
            (uiop:wait-process process)
            (unless (probe-file output)
              (format *error-output* "~&Blocked-exit fixture stdout: ~S~%"
                      (coerce (loop repeat 2000
                                    for character = (read-char (uiop:process-info-output process) nil nil)
                                    while character collect character)
                              'string))
              (when (probe-file error-path)
                (with-open-file (errors error-path :external-format ':utf-8)
                  (format *error-output* "Blocked-exit fixture stderr: ~S~%"
                          (coerce (loop repeat 2000 for character = (read-char errors nil nil)
                                        while character collect character)
                                  'string)))))
            (ignore-errors (close (uiop:process-info-input process)))
            (ignore-errors (close (uiop:process-info-output process))))))))
  nil)
