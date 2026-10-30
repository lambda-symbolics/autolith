(in-package #:autolith)

;;;; -- Standalone Supervisor Boundary --

(-> test-supervisor-subprocess-boundary () null)
(defun test-supervisor-subprocess-boundary ()
  "Exercise the standalone supervisor without loading the agent into its process."
  (let* ((root    (asdf:system-source-directory :autolith-agent))
         (command (list (active-image--sbcl-command)
                        "--noinform" "--end-runtime-options" "--no-sysinit" "--no-userinit" "--script"
                        (namestring (merge-pathnames "supervisor/tests.lisp" root)))))
    (multiple-value-bind (output errors status)
        (uiop:run-program command :directory          root
                                 :output ':string :error-output ':string
                                 :ignore-error-status t)
      (test-assert (zerop status)
                   (format nil "standalone supervisor checks exit ~D: ~A~A"
                           status output errors))))
  nil)

(-> test-supervisor-protocol-boundary () null)
(defun test-supervisor-protocol-boundary ()
  "Exercise the generated wire protocol in a fresh parent process without agent code."
  (let* ((root    (asdf:system-source-directory :autolith-agent))
         (command (list (active-image--sbcl-command)
                        "--noinform" "--end-runtime-options" "--no-sysinit" "--no-userinit" "--script"
                        (namestring (merge-pathnames "supervisor/protocol-test-entry.lisp" root)))))
    (multiple-value-bind (output errors status)
        (uiop:run-program command :directory          root
                                 :output ':string :error-output ':string
                                 :ignore-error-status t)
      (test-assert (zerop status)
                   (format nil "parent protocol checks exit ~D: ~A~A" status output errors))))
  nil)

(-> test-agent-protocol-client () null)
(defun test-agent-protocol-client ()
  "Exercise the optional client against serialized parent envelopes in this isolated worker."
  (let ((root (asdf:system-source-directory :autolith-agent)))
    (dolist (definition '("autolith-protocol.asd" "autolith-supervisor.asd"
                          "autolith-protocol-client.asd"))
      (asdf:load-asd (merge-pathnames definition root)))
    (test-assert (asdf:test-system '#:autolith-protocol-client :verbose nil)
                 "optional agent protocol client checks pass")
    (test-assert (asdf:test-system '#:autolith-protocol-client/records :verbose nil)
                 "optional record client checks pass")
    (test-assert (asdf:test-system '#:autolith-protocol-client/records/resources :verbose nil)
                 "parent record resource checks pass")
    (test-assert (asdf:test-system '#:autolith-protocol-client/knowledge :verbose nil)
                 "parent knowledge and plan client checks pass")
    (test-assert (asdf:test-system '#:autolith-protocol-client/access :verbose nil)
                 "optional access client checks pass"))
  nil)

(-> test-supervisor--native-entry (string integer) null)
(defun test-supervisor--native-entry (mode expected-checks)
  "Run MODE in a fresh trusted parent with this test run's source-matched core."
  (let* ((root (asdf:system-source-directory :autolith-agent))
         (core (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
    (assert (and (non-empty-string-p core) (probe-file core)) ()
            "Native parent checks require AUTOLITH_TEST_ACTIVE_CORE from script/check.")
    (test-call-with-temporary-root
     (lambda (temporary)
       (let* ((result-path (merge-pathnames "result.sexp" temporary))
              (command     (list (active-image--sbcl-command)
                                 "--noinform" "--end-runtime-options" "--no-sysinit" "--no-userinit" "--script"
                                 (namestring (merge-pathnames "supervisor/parent-native-test-entry.lisp" root))
                                 mode (active-image--sbcl-command) core
                                 (namestring temporary) (namestring result-path))))
         (multiple-value-bind (output errors status)
             (uiop:run-program command :directory root :output ':string :error-output ':string
                                      :ignore-error-status t)
           (test-assert (zerop status)
                        (format nil "parent ~A checks exit ~D: ~A~A" mode status output errors))
           (when (zerop status)
             (with-open-file (stream result-path)
               (let* ((*read-eval* nil)
                      (result      (read stream)))
                 (case (getf result :status)
                   (:passed
                    (test-assert (>= (getf result :checks 0) expected-checks)
                                 (format nil "parent ~A checks exercised native enforcement" mode)))
                   (:skipped
                    (test-withheld ':native-access
                                   (format nil "parent ~A checks: ~A" mode (getf result :reason))))
                   (otherwise
                    (test-assert nil (format nil "invalid parent native result: ~S" result))))))))))))
  nil)

(-> test-supervisor-native-host-access () null)
(defun test-supervisor-native-host-access ()
  "Run the real parent session, startup replay, image replacement and nested worker checks."
  (test-supervisor--native-entry "session" 18))

(-> test-supervisor-production-launch () null)
(defun test-supervisor-production-launch ()
  "Run production HTTP, resource, Lisp and shell tools with access enabled and disabled."
  (test-supervisor--native-entry "production" 2))

(-> test-supervisor-native-host-policy () null)
(defun test-supervisor-native-host-policy ()
  "Retain the distinct C helper checks for filesystem, network and process policy."
  (test-supervisor--native-entry "policy" 1))

(-> test-supervisor-network-policy () null)
(defun test-supervisor-network-policy ()
  "Check network denial, explicit parent approval, process replacement and revocation."
  (test-supervisor--native-entry "network" 15))
