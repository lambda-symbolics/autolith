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
                    (("--model" "custom/model" "acp") t)
                    (("--provider" "custom" "--model" "shared" "acp") t)
                    (("--model" "acp") nil)
                    (("--provider" "acp") nil)
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
    (string &key (:arguments list) (:directory pathname) (:timeout real)) hash-table)
(defun acp-launcher-tests--initialize (program &key arguments
                                                  (directory *default-pathname-defaults*)
                                                  (timeout 180))
  "Initialize PROGRAM over its actual standard I/O and return the protocol response."
  (let ((channel (agentcomms:acp-launch-agent program
                                             :arguments arguments
                                             :directory directory))
        (client  (make-instance 'agentcomms:acp-client))
        (result  nil)
        (failure nil))
    (unwind-protect
         (handler-case
             (progn
               (agentcomms:acp-client-connect client channel)
               (setf result (agentcomms:client-initialize client :timeout timeout)))
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
    result))

(-> acp-launcher-tests--initialize-with-caller-environment (string) hash-table)
(defun acp-launcher-tests--initialize-with-caller-environment (program)
  "Initialize PROGRAM and observe restored ASDF values from executable user init."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (acp-launcher-tests--call-with-roots
     root
     (lambda ()
       (let* ((configuration (configuration-create :defer-provider-validation-p t))
              (observation   (merge-pathnames "caller-environment.sexp" root))
              (registry      (namestring (merge-pathnames "caller//:" root))))
         (ensure-directories-exist (merge-pathnames "caller/" root))
         (with-open-file (stream (ensure-directories-exist
                                  (configuration-user-init-path configuration))
                                 :direction :output :if-exists :supersede)
           (write `(with-open-file (output ,(namestring observation)
                                          :direction :output :if-exists :supersede)
                     (write (list (uiop:getenv "CL_SOURCE_REGISTRY")
                                  (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")
                                  (uiop:getenv "AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE")
                                  (uiop:getenv "AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE"))
                            :stream output))
                  :stream stream))
         (with-test-environment (("CL_SOURCE_REGISTRY" registry)
                                 ("ASDF_OUTPUT_TRANSLATIONS" "")
                                 ("AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE" nil)
                                 ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE" nil)
                                 ("AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
           (let ((result (acp-launcher-tests--initialize program :arguments '("acp")
                                                       :directory           root)))
             (with-open-file (stream observation)
               (let ((*read-eval* nil))
                 (assert (equal (list registry "" nil nil) (read stream)))))
             result)))))))

(-> test-acp-launcher-caller-environment () null)
(defun test-acp-launcher-caller-environment ()
  "Observe caller values after dependency loading through a real ACP launch."
  (with-test-fixture (':posix-shell "ACP caller environment")
    (let ((result (acp-launcher-tests--initialize-with-caller-environment
                   (namestring (asdf:system-relative-pathname :autolith "bin/autolith")))))
      (test-assert (equal "autolith" (agentcomms:json-get
                                      (agentcomms:json-get result "agentInfo") "name"))
                   "ACP initializes after caller environment restoration")))
  nil)

(-> test-acp-launcher-source-stdio-roundtrip () null)
(defun test-acp-launcher-source-stdio-roundtrip ()
  "Initialize through the real source launcher and require a clean protocol response."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (acp-launcher-tests--call-with-roots
     root
     (lambda ()
       (let ((result
               (acp-launcher-tests--initialize
                (lisp-worker-sbcl-command)
                :arguments (list "--noinform" "--no-sysinit" "--no-userinit" "--script"
                                 (namestring (asdf:system-relative-pathname
                                              :autolith "script/launcher.lisp"))
                                 "--from-source" "--pristine" "acp")
                :directory root)))
         (test-assert
          (equal "autolith" (agentcomms:json-get
                             (agentcomms:json-get result "agentInfo") "name"))
          "the source launcher emits JSON-RPC without preceding diagnostics")
         (test-assert
          (agentcomms:acp-capability-enabled-p
           (agentcomms:json-get result "agentCapabilities") "loadSession")
          "the subprocess advertises durable session loading")))))
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

(-> test-acp-launcher-failure-skips-recovery () null)
(defun test-acp-launcher-failure-skips-recovery ()
  "The stable shell launcher keeps a failed ACP status and starts no recovery."
  (with-test-fixture (':posix-shell "the stable launcher's ACP status")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let ((fake-sbcl (merge-pathnames "bin/fake-sbcl" root))
            (log       (merge-pathnames "calls.log" root)))
        (recovery-tests--write-executable
         fake-sbcl
         "#!/bin/sh
printf 'call\\n' >> \"$AUTOLITH_TEST_LOG\"
exit 1
")
        (multiple-value-bind (output diagnostics status)
            (uiop:run-program
             (list "env"
                   (format nil "AUTOLITH_SBCL=~A" (namestring fake-sbcl))
                   (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))
                   (format nil "XDG_DATA_HOME=~A" (namestring (merge-pathnames "data/" root)))
                   (format nil "XDG_STATE_HOME=~A" (namestring (merge-pathnames "state/" root)))
                   (namestring (asdf:system-relative-pathname :autolith "bin/autolith"))
                   "--from-source" "acp")
             :directory           root
             :input               nil
             :output              ':string
             :error-output        ':string
             :ignore-error-status t)
          (test-assert (= status 1) "the launcher keeps the ACP status")
          (test-assert (zerop (length output)) "the launcher writes nothing to ACP's stdout")
          (test-assert (not (search "RECOVERY" diagnostics)) "no recovery banner follows"))
        (test-assert (equal (uiop:read-file-lines log) '("call"))
                     "the launcher runs SBCL once, for ACP only"))))
  nil)

(-> test-acp-launcher-interrupt-exits-quietly () null)
(defun test-acp-launcher-interrupt-exits-quietly ()
  "Ctrl-C ends a serving ACP launcher with status 130 and no failure report."
  (with-test-fixture (':posix-shell "process-group interrupts")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (acp-launcher-tests--call-with-roots
       root
       (lambda ()
         (with-test-environment (("AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
           (let* ((channel (agentcomms:acp-launch-agent
                            (namestring (asdf:system-relative-pathname :autolith "bin/autolith"))
                            :arguments '("acp")
                            :directory root))
                  (process (agentcomms:acp-process-channel-process channel))
                  (client  (make-instance 'agentcomms:acp-client)))
             (unwind-protect
                  (progn
                    (agentcomms:acp-client-connect client channel)
                    (agentcomms:client-initialize client :timeout 180)
                    ;; The launcher leads its own process group, as a shell job does.
                    (uiop:run-program (list "kill" "-INT" "--"
                                            (format nil "-~D" (uiop:process-info-pid process))))
                    ;; Channel cleanup owns reaping, including on reader EOF.
                    ;; A second blocking wait races SBCL's process event pipe.
                    (test-assert
                     (task-tests--wait-until (lambda () (not (uiop:process-alive-p process))) 10)
                     "the interrupt ends the launcher before channel cleanup"))
               (agentcomms:channel-close channel))
             (let ((diagnostics (agentcomms:acp-process-channel-stderr-text channel)))
               (test-assert (eql 130 (agentcomms:acp-process-channel-exit-code channel))
                            "the interrupted launcher exits with status 130")
               (test-assert (not (search "failed" diagnostics))
                            "the interrupt reports no failure")
               (test-assert (not (search "RECOVERY" diagnostics))
                            "the interrupt starts no recovery"))))))))
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


;;;; -- Caller ASDF Environment --

(-> caller-environment-tests--observation-form () string)
(defun caller-environment-tests--observation-form ()
  "Return an evaluation that observes only caller settings and actual child consumers."
  (prin1-to-string
   '(list
     (uiop:getenv "CL_SOURCE_REGISTRY")
     (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")
     (uiop:run-program
      '("bash" "-c"
        "printf '%s|%s|%s|%s' \"${CL_SOURCE_REGISTRY+x}\" \"${CL_SOURCE_REGISTRY-}\" \"${ASDF_OUTPUT_TRANSLATIONS+x}\" \"${ASDF_OUTPUT_TRANSLATIONS-}\"")
      :output ':string)
     (uiop:getenv "AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE")
     (uiop:getenv "AUTOLITH_CALLER_CL_SOURCE_REGISTRY")
     (uiop:getenv "AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE")
     (uiop:getenv "AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS")
     (not (null (asdf:find-system :autolith nil)))
     (when (uiop:getenv "AUTOLITH_TEST_CALLER_SYSTEM")
       (read-from-string
        (uiop:run-program
         (list (namestring sb-ext:*runtime-pathname*)
               "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"
               "--eval" "(require :asdf)"
               "--eval"
               "(let ((*standard-output* *error-output*) (*compile-verbose* nil)) (asdf:load-system :caller-environment-fixture))"
               "--eval"
               "(prin1 (list (namestring (asdf:system-source-directory :caller-environment-fixture)) (cl-user::caller-environment-probe)))")
         :output ':string :error-output *error-output*))))))

(-> caller-environment-tests--probe (pathname &key (:source-p boolean)) list)
(defun caller-environment-tests--probe (root &key source-p)
  "Observe the actual launcher's worker and its shell child after private startup."
  (let* ((wrapper  (merge-pathnames "runtime" root))
         (launcher (asdf:system-relative-pathname :autolith "bin/autolith"))
         (form     (caller-environment-tests--observation-form)))
    (recovery-tests--write-executable
     wrapper
     (format nil "#!/usr/bin/env bash~%export CL_SOURCE_REGISTRY=\"$AUTOLITH_TEST_PRIVATE_REGISTRY\"~%export ASDF_OUTPUT_TRANSLATIONS=\"$AUTOLITH_TEST_PRIVATE_TRANSLATIONS\"~%if [[ \"${AUTOLITH_TEST_REENTER:-}\" = yes && \"$*\" == *--worker* ]]; then~%  export AUTOLITH_TEST_REENTER=no~%  exec bash ~S --worker~%fi~%exec ~S \"$@\"~%"
             (namestring launcher) (lisp-worker-sbcl-command)))
    (with-input-from-string
        (input (format nil "(:request :id 1 :operation :eval :arguments (:forms (~S)))~%" form))
      (multiple-value-bind (output diagnostics status)
          (uiop:run-program
           (append (list "env" (format nil "AUTOLITH_SBCL=~A" wrapper))
                   (test-active-core-environment)
                   (list "bash" (namestring launcher))
                   (when source-p '("--from-source"))
                   '("--worker"))
           :directory root :input input :output ':string :error-output ':string
           :ignore-error-status t)
        (test-assert (zerop status) (format nil "worker launch succeeded: ~A" diagnostics))
        (let ((*read-eval* nil))
          (with-input-from-string (stream output)
            (test-assert (eq (first (read stream)) :autolith-worker)
                         "the real launcher reaches the worker protocol")
            (let ((response (read stream nil nil)))
              (test-assert (eq (getf (rest response) :status) :ok)
                           (format nil "the worker evaluated the environment observation: ~A ~A" output diagnostics))
              (when response (read-from-string (first (getf (rest response) :values)))))))))))

(-> test-caller-asdf-environment () null)
(defun test-caller-asdf-environment ()
  "Keep absent, empty and literal caller settings across runtime injection and reentry."
  (with-test-fixture (':posix-shell "the caller's launcher environment")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (acp-launcher-tests--call-with-roots
       root
       (lambda ()
         (let ((registry             (format nil "~Acaller//:" root))
               (translations         (format nil "~Acaller//:~Acompiled//" root root))
               (private-registry     (uiop:getenv "CL_SOURCE_REGISTRY"))
               (private-translations (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
           (test--write-lisp-worker-text
            (merge-pathnames "caller/caller-environment-fixture.asd" root)
            "(asdf:defsystem :caller-environment-fixture :components ((:file \"probe\")))")
           (test--write-lisp-worker-text
            (merge-pathnames "caller/probe.lisp" root)
            "(defun cl-user::caller-environment-probe () :caller)")
           (dolist (case (list (list nil nil nil nil)
                              (list "" "" nil nil)
                              (list registry translations nil nil)
                              (list nil translations nil nil)
                              (list registry "" nil nil)
                              (list "" nil nil nil)
                              (list registry translations t nil)
                              (list registry translations nil t)))
             (destructuring-bind (source output reenter source-p) case
               (with-test-environment
                   (("AUTOLITH_TEST_CALLER_SYSTEM" (when (equal source registry) "yes"))
                    ("CL_SOURCE_REGISTRY" source)
                    ("ASDF_OUTPUT_TRANSLATIONS" output)
                    ("AUTOLITH_TEST_PRIVATE_REGISTRY" private-registry)
                    ("AUTOLITH_TEST_PRIVATE_TRANSLATIONS" private-translations)
                    ("AUTOLITH_TEST_REENTER" (when reenter "yes"))
                    ("AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE" nil)
                    ("AUTOLITH_CALLER_CL_SOURCE_REGISTRY" nil)
                    ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE" nil)
                    ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS" nil))
                 (let ((observed (caller-environment-tests--probe root :source-p source-p)))
                   (test-assert (equal observed
                                       (list source output
                                             (format nil "~A|~A|~A|~A"
                                                     (if source "x" "") (or source "")
                                                     (if output "x" "") (or output ""))
                                             nil nil nil nil t
                                             (when (equal source registry)
                                               (list (namestring (merge-pathnames "caller/" root))
                                                     :caller))))
                                "the worker and its shell receive exact caller values without markers"))))))))))
  nil)

(-> test-caller-asdf-environment-legacy () null)
(defun test-caller-asdf-environment-legacy ()
  "Leave legacy startup untouched and make restoration one-shot."
  (with-test-environment (("CL_SOURCE_REGISTRY" "literal::source//")
                          ("ASDF_OUTPUT_TRANSLATIONS" "literal::output//")
                          ("AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE" nil)
                          ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE" nil))
    (test-assert (fboundp 'main--restore-caller-asdf-environment)
                 "startup has a caller restoration boundary")
    (when (fboundp 'main--restore-caller-asdf-environment)
      (funcall 'main--restore-caller-asdf-environment)
      (test-assert (equal (uiop:getenv "CL_SOURCE_REGISTRY") "literal::source//")
                   "no capture is different from captured absence")
      (with-test-environment (("AUTOLITH_CALLER_CL_SOURCE_REGISTRY_STATE" "absent")
                              ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS_STATE" "set")
                              ("AUTOLITH_CALLER_ASDF_OUTPUT_TRANSLATIONS" ""))
        (funcall 'main--restore-caller-asdf-environment)
        (test-assert (null (uiop:getenv "CL_SOURCE_REGISTRY")) "captured absence unsets")
        (test-assert (equal (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS") "") "captured empty is set")
        (platform-setenv "CL_SOURCE_REGISTRY" "later//")
        (funcall 'main--restore-caller-asdf-environment)
        (test-assert (equal (uiop:getenv "CL_SOURCE_REGISTRY") "later//")
                     "a repeated restore does not overwrite later consumers"))))
  nil)
