(in-package #:autolith)

;;;; -- ACP Launcher --

(-> acp-launcher-tests--run-parser (&rest string) (values string string integer))
(defun acp-launcher-tests--run-parser (&rest arguments)
  "Run the shared shell launcher parser and return its exported state."
  (let* ((script  (asdf:system-relative-pathname :autolith
                                                "script/launcher-cli.sh"))
         (quoted  (mapcar (lambda (argument)
                            (format nil "'~A'" argument))
                          arguments))
         (command (format nil
                          "source ~A; autolith_launcher_parse source ~{~A ~}; printf '%s\\n' \"$from_source_requested\" \"$recovery_requested\" \"$update_requested\" \"$uninstall_requested\" \"$data_requested\" \"$acp_requested\" \"${remaining_arguments[*]}\""
                          (namestring script) quoted)))
    (uiop:run-program (list "bash" "-c" command)
                      :output       :string
                      :error-output :string)))

(-> test-acp-launcher-forwards-pristine-and-permissions () null)
(defun test-acp-launcher-forwards-pristine-and-permissions ()
  "Exercise forwarding of ACP pristine selection and permission arguments."
  (with-test-fixture (':posix-shell "shared shell launcher parser")
    (let ((lines (uiop:split-string
                  (acp-launcher-tests--run-parser "--pristine" "acp" "--permissions" "auto")
                  :separator '(#\Newline))))
      (test-assert (equal (subseq lines 0 5) '("true" "false" "false" "false" "false"))
                   "pristine selects source mode")
      (test-assert (equal (seventh lines) "--pristine acp --permissions auto")
                   "the launcher forwards protocol arguments verbatim")))
  nil)

(-> test-acp-launcher-terminator-preserves-acp-arguments () null)
(defun test-acp-launcher-terminator-preserves-acp-arguments ()
  "Exercise the application argument terminator through the shared launcher parser."
  (with-test-fixture (':posix-shell "shared shell launcher parser")
    (let ((lines (uiop:split-string
                  (acp-launcher-tests--run-parser "acp" "--" "--pristine" "--permissions" "full")
                  :separator '(#\Newline))))
      (test-assert (equal (subseq lines 0 5) '("false" "false" "false" "false" "false"))
                   "options after the terminator belong to the application")
      (test-assert (equal (seventh lines) "acp -- --pristine --permissions full")
                   "the application receives the terminated argument sequence")))
  nil)

(-> test-acp-launcher-detects-command-not-option-values () null)
(defun test-acp-launcher-detects-command-not-option-values ()
  "Select protocol diagnostic routing only for the ACP application command."
  (with-test-fixture (':posix-shell "shared shell launcher parser")
    (dolist (case '((("acp") t)
                    (("--pristine" "acp") t)
                    (("--permissions" "auto" "acp") t)
                    (("acp" "--" "--pristine") t)
                    (("--image" "acp") nil)
                    (("--permissions" "acp") nil)
                    (("replay" "acp") nil)))
      (destructuring-bind (arguments expected) case
        (let ((lines (uiop:split-string
                      (apply #'acp-launcher-tests--run-parser arguments)
                      :separator '(#\Newline))))
          (test-assert (eql (not (null (string= "true" (sixth lines)))) expected)
                       "ACP routing follows the command, not an operand")))))
  nil)

(-> acp-launcher-tests--call-with-roots (pathname function) t)
(defun acp-launcher-tests--call-with-roots (root function)
  "Run FUNCTION with isolated process configuration and the managed SBCL."
  (test-call-with-environment
   (list (list "XDG_CONFIG_HOME" (namestring (merge-pathnames "config/" root)))
         (list "XDG_DATA_HOME" (namestring (merge-pathnames "data/" root)))
         (list "XDG_STATE_HOME" (namestring (merge-pathnames "state/" root)))
         (list "XDG_CACHE_HOME" (namestring (merge-pathnames "cache/" root)))
         (list "AUTOLITH_SITE_CONFIG_ROOT" nil)
         (list "AUTOLITH_RECOVERED" nil)
         (list "AUTOLITH_AGENT_LAUNCHER" nil)
         (list "AUTOLITH_ACTIVE_CORE" (or (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")
                                          (uiop:getenv "AUTOLITH_ACTIVE_CORE")))
         (list "AUTOLITH_SBCL" (lisp-worker-sbcl-command)))
   function))

(define-condition acp-launcher-test-failure (error)
  ((cause :initarg :cause :reader acp-launcher-test-failure-cause
          :documentation "The original initialization or cleanup condition.")
   (exit-code :initarg :exit-code :reader acp-launcher-test-failure-exit-code
              :documentation "The child's reaped exit status, if available.")
   (stderr :initarg :stderr :reader acp-launcher-test-failure-stderr
           :documentation "A bounded tail of retained child diagnostics."))
  (:documentation "A source-launcher failure with its subprocess diagnostics.")
  (:report (lambda (condition stream)
             (format stream "ACP launcher initialization failed: ~A~%Child exit: ~S~%Stderr:~%~A"
                     (acp-launcher-test-failure-cause condition)
                     (acp-launcher-test-failure-exit-code condition)
                     (acp-launcher-test-failure-stderr condition)))))

(-> acp-launcher-tests--initialize
    (string &key (:arguments list) (:directory pathname) (:timeout real)
                 (:session-directory (option string)))
    (values hash-table (option string) &optional))
(defun acp-launcher-tests--initialize (program &key arguments
                                                  (directory *default-pathname-defaults*)
                                                  (timeout 180) session-directory)
  "Initialize PROGRAM over standard I/O and optionally create a native session.
Return the initialization response and the new session identity, if requested."
  (let ((channel    (agentcomms:acp-launch-agent program
                                                :arguments arguments
                                                :directory directory))
        (client     (make-instance 'agentcomms:acp-client))
        (result     nil)
        (identifier nil)
        (failure    nil))
    (unwind-protect
         (handler-case
             (progn
               (agentcomms:acp-client-connect client channel)
              (setf result (agentcomms:client-initialize client :timeout timeout))
              (when session-directory
                (setf identifier (agentcomms:client-new-session client session-directory))))
           (serious-condition (condition)
             (setf failure condition)))
      (handler-case
          (unwind-protect
               (when (agentcomms:acp-client-connection client)
                 (agentcomms:connection-close (agentcomms:acp-client-connection client)))
            (agentcomms:channel-close channel))
        (serious-condition (condition)
          (unless failure (setf failure condition)))))
    (when failure
      (let ((diagnostics (agentcomms:acp-process-channel-stderr-text channel)))
        (error 'acp-launcher-test-failure
               :cause failure :exit-code (agentcomms:acp-process-channel-exit-code channel)
               :stderr (subseq diagnostics (max 0 (- (length diagnostics) 8192))))))
    (values result identifier)))

(-> test-acp-launcher-source-stdio-roundtrip () null)
(defun test-acp-launcher-source-stdio-roundtrip ()
  "Initialize and create a native session through the public source launcher."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (acp-launcher-tests--call-with-roots
     root
     (lambda ()
       (let ((workspace (merge-pathnames "workspace/" root)))
         (ensure-directories-exist (merge-pathnames "probe" workspace))
        (multiple-value-bind (result identifier)
            (acp-launcher-tests--initialize
             (lisp-worker-sbcl-command)
             :arguments (list "--noinform" "--no-sysinit" "--no-userinit" "--script"
                              (namestring (asdf:system-relative-pathname
                                           :autolith "script/launcher.lisp"))
                              "--from-source" "--pristine" "acp")
             :directory workspace :session-directory (namestring workspace))
          (test-assert
           (equal "autolith" (agentcomms:json-get
                              (agentcomms:json-get result "agentInfo") "name"))
           "the source launcher emits JSON-RPC without preceding diagnostics")
          (test-assert
           (agentcomms:acp-capability-enabled-p
            (agentcomms:json-get result "agentCapabilities") "loadSession")
           "the subprocess advertises durable session loading")
          (test-assert (and (stringp identifier) (plusp (length identifier)))
                       "the parent creates a native-isolated ACP session"))))))
  nil)

(-> test-acp-launcher-failure-keeps-stdout-clean () null)
(defun test-acp-launcher-failure-keeps-stdout-clean ()
  "A failed ACP invocation exits with stderr diagnostics rather than recovery UI."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (acp-launcher-tests--call-with-roots
     root
     (lambda ()
       (multiple-value-bind (output diagnostics status)
           (uiop:run-program
            (list (lisp-worker-sbcl-command) "--noinform" "--no-sysinit" "--no-userinit"
                  "--script" (namestring (asdf:system-relative-pathname
                                          :autolith "script/launcher.lisp"))
                  "--from-source" "--pristine" "acp" "--permissions" "invalid")
            :directory root :input nil :output ':string :error-output ':string
            :ignore-error-status t)
         (test-assert (not (zerop status)) "invalid ACP options produce a failing process status")
         (test-assert (zerop (length output)) "startup failures emit no non-protocol stdout")
         (test-assert (plusp (length diagnostics)) "startup diagnostics are on stderr")))))
  nil)

(-> test-acp-launcher-failure-diagnostics () null)
(defun test-acp-launcher-failure-diagnostics ()
  "Retain the original transport failure, child status, and drained stderr."
  (handler-case
      (progn
        (acp-launcher-tests--initialize
         (lisp-worker-sbcl-command)
         :arguments '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"
                      "--eval" "(progn (write-line \"ACP startup diagnostic\" *error-output*) (finish-output *error-output*) (sb-ext:exit :code 23))"))
        (test-assert nil "the failed child cannot initialize"))
    (acp-launcher-test-failure (condition)
      (test-assert (typep (acp-launcher-test-failure-cause condition)
                          'agentcomms:acp-connection-closed)
                   "the diagnostic condition retains the transport cause")
      (test-assert (eql 23 (acp-launcher-test-failure-exit-code condition))
                   "the child is reaped before diagnostics are reported")
      (test-assert (search "ACP startup diagnostic" (acp-launcher-test-failure-stderr condition))
                   "stderr is drained before diagnostics are reported")))
  nil)

(-> test-launcher-agent-supervisor-routing () null)
(defun test-launcher-agent-supervisor-routing ()
  "Route public CLI commands to the parent and fixed workers through the agent wrapper."
  (with-test-fixture (':posix-shell "agent launcher executable wrapper")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((runtime (merge-pathnames "fake-runtime" root))
             (wrapper (merge-pathnames "wrapper with spaces" root))
             (log     (merge-pathnames "launch.log" root))
             (core    (merge-pathnames "active/agent.core" root))
             (sbcl    (lisp-worker-sbcl-command))
             (payload "spaces ; $(touch unexpected-shell-file) *"))
        (recovery-tests--write-executable
         runtime
         (format nil "#!/bin/sh~%case \"$*\" in~%  *parent.lisp*) printf '%s\\n' \"$@\" >> \"$AUTOLITH_TEST_PARENT_LOG\"; printf 'parent-output\\n'; exit 64 ;;~%  *autolith-supervisor*) exec \"$AUTOLITH_TEST_REAL_SBCL\" \"$@\" ;;~%  *--autolith-internal-active-image-probe*) exit 0 ;;~%esac~%printf 'agent-output\\n'~%exit 64~%"))
        (recovery-tests--write-executable
         wrapper
         (format nil "#!/bin/sh~%printf 'WRAPPER\\n' >> \"$AUTOLITH_TEST_LAUNCH_LOG\"~%printf '%s\\n' \"$@\" >> \"$AUTOLITH_TEST_LAUNCH_LOG\"~%exec \"$@\"~%"))
        (ensure-directories-exist core)
        (with-open-file (stream core :direction ':output :if-exists ':supersede)
          (write-line "fake core" stream))
        (with-open-file (stream (merge-pathnames "manifest.sexp" core)
                                :direction ':output :if-exists ':supersede)
          (write-line "(:SBCL-GENERATIONS-IMAGE-MANIFEST :VERSION 1 )" stream))
        (acp-launcher-tests--call-with-roots
         root
         (lambda ()
           (with-test-environment
               (("AUTOLITH_SBCL" (namestring runtime))
                ("AUTOLITH_TEST_REAL_SBCL" sbcl)
                ("AUTOLITH_AGENT_LAUNCHER" (namestring wrapper))
                ("AUTOLITH_TEST_LAUNCH_LOG" (namestring log))
                ("AUTOLITH_TEST_PARENT_LOG" (namestring (merge-pathnames "parent.log" root)))
                ("AUTOLITH_ACTIVE_CORE" (namestring core)))
             (dolist (shell-p '(t nil))
               (dolist (source-p '(t nil))
                 (when (probe-file log)
                   (delete-file log))
                 (let ((command
                         (append
                          (if shell-p
                              (list "bash" (namestring (asdf:system-relative-pathname
                                                        :autolith "bin/autolith")))
                              (list sbcl "--noinform"
                                    "--no-sysinit" "--no-userinit" "--script"
                                    (namestring (asdf:system-relative-pathname
                                                 :autolith "script/launcher.lisp"))))
                          (when source-p (list "--from-source"))
                          (list "--worker" payload))))
                   (multiple-value-bind (output diagnostics status)
                       (uiop:run-program command :directory root :input nil
                                                :output ':string :error-output ':string
                                                :ignore-error-status t)
                    (declare (ignore diagnostics))
                    (test-assert (= status 64) "the supervisor returns the agent exit status")
                    (test-assert (string= output (format nil "agent-output~%"))
                                 "the supervisor preserves the agent standard output"))
                   (let ((lines (uiop:read-file-lines log)))
                     (test-assert (= (count "WRAPPER" lines :test #'string=)
                                     (if source-p 1 2))
                                 (format nil "wrapper routing shell=~S source=~S: ~S"
                                         shell-p source-p lines))
                     (test-assert (member payload lines :test #'string=)
                                  "the wrapper receives each argument without shell parsing")
                     (test-assert (if source-p
                                      (find-if (lambda (line)
                                                 (uiop:string-suffix-p line "bin/autolith-agent"))
                                               lines)
                                      (member (namestring core) lines :test #'string=))
                                 (format nil "selected entrypoint shell=~S source=~S: ~S"
                                         shell-p source-p lines)))
                   (test-assert (not (probe-file (merge-pathnames "unexpected-shell-file" root)))
                                "shell syntax in an argument is inert"))))
             (dolist (shell-p '(t nil))
               (dolist (source-p '(t nil))
                 (dolist (arguments (list nil (list "acp" payload)
                                         (list "--help") (list "--image" "named")))
                   (when (probe-file log)
                     (delete-file log))
                   (let* ((parent-log (merge-pathnames "parent.log" root))
                          (command
                            (append
                             (if shell-p
                                 (list "bash" (namestring (asdf:system-relative-pathname
                                                           :autolith "bin/autolith")))
                                 (list sbcl "--noinform" "--no-sysinit" "--no-userinit"
                                       "--script" (namestring (asdf:system-relative-pathname
                                                               :autolith "script/launcher.lisp"))))
                             (when source-p (list "--from-source"))
                             arguments)))
                     (when (probe-file parent-log)
                       (delete-file parent-log))
                     (multiple-value-bind (output diagnostics status)
                         (uiop:run-program command :directory root :input nil
                                                  :output ':string :error-output ':string
                                                  :ignore-error-status t)
                       (declare (ignore diagnostics))
                       (test-assert (and (= status 64)
                                         (string= output (format nil "parent-output~%")))
                                    "public commands run in the selected host runtime"))
                     (let* ((lines    (uiop:read-file-lines parent-log))
                            (expected (append (list "--cli")
                                              (when source-p (list "--from-source"))
                                              arguments)))
                       (test-assert (equal (subseq lines (- (length lines) (length expected)))
                                           expected)
                                    "the parent receives CLI arguments without shell parsing")
                      (test-assert (not (probe-file log))
                                   "the launcher does not apply the agent wrapper to the parent")
                      (test-assert (not (member (namestring core) lines :test #'equal))
                                   "public startup does not select a legacy agent core"))))))))))))
  nil)
