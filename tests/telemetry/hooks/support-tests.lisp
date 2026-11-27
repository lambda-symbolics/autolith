(in-package #:autolith)

;;;; -- Shared Offline Hook Fixtures --

(-> test-telemetry-hooks--assert-attributes (hash-table list) null)
(defun test-telemetry-hooks--assert-attributes (span cases)
  "Check each named (NAME ATTRIBUTE EXPECTED MESSAGE) row against SPAN."
  (dolist (case cases)
    (destructuring-bind (name attribute expected message) case
      (test-assert (equal (test-telemetry--attribute span attribute) expected)
                   (format nil "~A: ~A" name message))))
  nil)

(-> test-telemetry-hooks--capture (configuration function) (values list list))
(defun test-telemetry-hooks--capture (configuration function)
  "Capture offline uploads while exercising the actual lifecycle wrappers."
  (setf (config :telemetry-enabled-p configuration) t)
  (telemetry-attach configuration)
  (let ((bodies nil))
    (let ((*telemetry-process-function*
            (lambda (run request)
              (declare (ignore run))
              (test-assert (eq (first request) :upload) "Metadata hooks only upload projected JSON.")
              (push (third request) bodies)
              t)))
      (let ((results (multiple-value-list (funcall function))))
        (let* ((controller *telemetry-controller*)
               (thread     (when controller
                             (bt:with-lock-held ((telemetry-controller-lock controller))
                               (telemetry-controller-upload-thread controller)))))
          (when thread (bt:join-thread thread)))
        (values (nreverse bodies) results)))))

(-> test-telemetry-hooks--kind (string string) list)
(defun test-telemetry-hooks--kind (body kind)
  "Select spans by their fixed public event kind."
  (remove-if-not
   (lambda (span)
     (equal (test-telemetry--attribute span "autolith.event.kind") kind))
   (coerce (test-telemetry--spans body) 'list)))

(-> test-telemetry-hooks--merge-bodies (list) (option string))
(defun test-telemetry-hooks--merge-bodies (bodies)
  "Combine batches from one captured turn for projection assertions and collector fixtures."
  (when bodies
    (let* ((envelope (json-decode (first bodies)))
           (resource (aref (json-get envelope "resourceSpans") 0))
           (scope    (aref (json-get resource "scopeSpans") 0)))
      (setf (gethash "spans" scope)
            (coerce (loop for body in bodies append (coerce (test-telemetry--spans body) 'list)) 'vector))
      (json-encode envelope))))

(-> test-telemetry-hooks--run-job
    (configuration model-provider
      &key (:publish-p boolean))
    (values list list))
(defun test-telemetry-hooks--run-job (configuration provider &key publish-p)
  "Run a real headless child with offline uploads and deterministic application wiring."
  (let* ((application (mission-test--application configuration))
         (request     (run-job-validate-envelope (run-job-tests--request-form)))
         (bodies      nil)
         (results     nil)
         (lock        (make-lock "captured job telemetry")))
    (setf (config :telemetry-enabled-p configuration) t)
    (telemetry-attach configuration)
    (setf (application-provider application) provider
          (agent-provider (application-agent application)) provider)
    (task-augment-tool-registry (application-tool-registry application))
    (test-call-with-function-replacements
     (list
      (list 'application-create
            (lambda (&rest arguments) (declare (ignore arguments)) application))
      ;; Keep headless fixtures independent of installed user skills.
      (list 'skill-roots
            (lambda (configuration)
              (list (merge-pathnames "skills/" (config :config-root configuration)))))
      (list 'telemetry--process
            (lambda (run request)
              (declare (ignore run))
              (when (eq (first request) ':upload)
                (with-lock-held (lock)
                  (push (third request) bodies)))
              t)))
     (lambda ()
       (setf results
               (multiple-value-list
                (if publish-p
                    (let* ((input
                            (merge-pathnames "job-input.sexp"
                                             (config :cache-root configuration)))
                           (output
                             (merge-pathnames "job-output.sexp"
                                              (config :cache-root configuration))))
                      (test-write-text input
                                       (run-job--write-data-sexp
                                        (run-job-tests--request-form)))
                      (values
                        (run-job-run input output ':full-access :configuration configuration)
                        (when (probe-file output) (run-job-read-file output))))
                    (run-job-execute-with-application configuration request
                                                      ':full-access))))))
    (values
     (loop for body in (nreverse bodies)
           append (coerce (test-telemetry--spans body) 'list))
     results)))

(-> test-telemetry-hooks--assert-attempts (string (integer 0)) null)
(defun test-telemetry-hooks--assert-attempts (body count)
  "Require one span per actual attempt and an exact root counter."
  (let ((models (test-telemetry-hooks--kind body "model"))
        (root   (first (test-telemetry-hooks--kind body "run"))))
    (test-assert (= (length models) count) "Every attempted request has one model span.")
    (test-assert (equal (test-telemetry--attribute root "autolith.run.model_calls")
                        (write-to-string count))
                 "The root counts real attempts, without an outer fallback duplicate."))
  nil)

(-> test-telemetry-hooks--tool-attempts
    (configuration list
     &key
     (:observer agent-observer)
     (:restricted-p boolean)
     (:conversation conversation)
     (:registry tool-registry)
     (:agent (option agent)))
    (values list list (option string)))
(defun test-telemetry-hooks--tool-attempts
    (configuration calls &key (observer (make-instance 'agent-observer)) restricted-p
                              (conversation (conversation-create configuration))
                              (registry (emacs-register-tools (make-default-tool-registry))) agent)
  "Return exported tools, model-visible outputs and upload for sequential real CALLS."
  (let* ((provider
           (make-instance
            'scripted-provider :configuration configuration
            :results
            (append
             (loop for call in calls for index from 1
                   collect (agent-test-result (format nil "attempt-~D" index) (list call)))
             (list (agent-test-result "done" (list (agent-test-message "done")))))))
         (agent (or agent (agent-create :configuration configuration :provider provider
                                       :conversation conversation
                                       :tool-registry registry :worker ':unused)))
         (bodies
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (setf (agent-provider agent) provider)
              (agent-run-user-turn
               agent "Exercise tool permissions." :observer observer
               :tool-restriction-p                          restricted-p
               :tool-allowlist                              (when restricted-p '("resource.read" "resource.edit")))))))
    (let ((body (test-telemetry-hooks--merge-bodies bodies)))
      (values (test-telemetry-hooks--kind body "tool")
              (agent-test-tool-outputs conversation)
              body))))

(-> test-telemetry-hooks--assert-failure (hash-table string string) null)
(defun test-telemetry-hooks--assert-failure (span outcome code)
  "Assert the exported failure OUTCOME and CODE for one real emitter's SPAN."
  (test-assert (and (equal outcome (test-telemetry--attribute span "autolith.tool.outcome"))
                    (equal code (test-telemetry--attribute span "autolith.tool.error_code"))
                    (equal (if (equal outcome "permission_denied") "permission" "tool")
                           (test-telemetry--attribute span "error.type")))
               (format nil "Expected ~A with ~A from the real refusal." outcome code))
  nil)
