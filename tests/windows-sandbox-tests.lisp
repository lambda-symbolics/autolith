(in-package #:autolith)

;;;; -- Windows Shell Sandbox --

(-> windows-sandbox-tests--enabled-p () boolean)
(defun windows-sandbox-tests--enabled-p ()
  "Require native enforcement before running Windows integration cases."
  (test-assert (application--command-sandbox-available-p)
               "native Windows network-isolated sandbox is available")
  t)

(-> windows-sandbox-tests--command
    (pathname string &key (:timeout (option integer)) (:authorization keyword)) tool-result)
(defun windows-sandbox-tests--command (root command &key timeout (authorization ':sandboxed))
  "Run sandboxed COMMAND in ROOT through the public shell.run tool."
  (let* ((configuration (configuration-with-working-directory
                         (test-configuration) root))
         (registry (make-default-tool-registry))
         (conversation (conversation-create configuration
                                             :identifier "windows-sandbox"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry registry
                                 :command-authorization-function
                                 (lambda (ignored-command ignored-directory)
                                   (declare (ignore ignored-command ignored-directory))
                                  authorization)))
         (tool (tool-registry-find registry "shell" "run"))
         (arguments (json-object "command" command
                                 "directory" ".")))
    (when timeout
      (setf (gethash "timeout-seconds" arguments) timeout))
    (unwind-protect (tool-execute tool context arguments)
      (tool-registry-close-runtime-state registry))))

(-> windows-sandbox-tests--successful-exit-p (tool-result) boolean)
(defun windows-sandbox-tests--successful-exit-p (result)
  "Return whether RESULT reports a successful shell exit."
  (and (tool-result-success-p result)
       (search "exit 0" (tool-result-content result))
       t))

(defun test-windows-shell-sandbox-available ()
  "Require the native Windows sandbox helper when running on Windows."
  (test-assert (application--command-sandbox-available-p)
               "the Windows sandbox helper is available"))

(defun test-windows-shell-sandbox-integration ()
  "Exercise native Windows shell containment through shell.run."
  (when (windows-sandbox-tests--enabled-p)
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((workspace (merge-pathnames "workspace/" root))
             (outside (merge-pathnames "outside/" root))
             (protected (merge-pathnames ".git/" workspace))
             (inside-file (merge-pathnames "inside.txt" workspace))
             (outside-file (merge-pathnames "outside.txt" outside))
             (protected-file (merge-pathnames "config" protected)))
        (uiop:ensure-all-directories-exist
         (list workspace outside protected))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "printf inside > '~A'"
                         (uiop:native-namestring inside-file)))))
          (test-assert (windows-sandbox-tests--successful-exit-p result)
                       "Windows shell.run writes inside the workspace")
          (test-assert (uiop:file-exists-p inside-file)
                       "the workspace write is observable"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "printf outside > '~A'"
                         (uiop:native-namestring outside-file)))))
          (test-assert (not (windows-sandbox-tests--successful-exit-p result))
                       "Windows shell.run denies writes outside the workspace")
          (test-assert (not (uiop:file-exists-p outside-file))
                       "the denied outside write leaves no file"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "printf protected > '~A'"
                         (uiop:native-namestring protected-file)))))
          (test-assert (not (windows-sandbox-tests--successful-exit-p result))
                       "Windows shell.run denies protected metadata mutation")
          (test-assert (not (uiop:file-exists-p protected-file))
                       "the protected metadata write leaves no file"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 "printf '%s' \"$TEMP\" > temp-path.txt; printf temporary > \"$TEMP/autolith-sandbox-temp.txt\"")))
          (test-assert (windows-sandbox-tests--successful-exit-p result)
                       "Windows shell.run permits its private temporary directory")
          (test-assert
           (not (uiop:directory-exists-p
                 (uiop:parse-native-namestring
                  (uiop:read-file-string (merge-pathnames "temp-path.txt" workspace)))))
           "private temporary directory is removed after execution"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace "(sleep 3; printf leaked > late.txt) & wait" :timeout 1)))
          (test-assert (not (tool-result-success-p result))
                       "Windows shell.run reports command timeout")
          (sleep 4)
          (test-assert (not (probe-file (merge-pathnames "late.txt" workspace)))
                       "timeout terminates descendants before scope cleanup"))))))

(defun test-windows-shell-sandbox-async ()
  "Exercise the asynchronous shell.run sandbox lifecycle on Windows."
  (when (windows-sandbox-tests--enabled-p)
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (registry (task-augment-tool-registry (make-default-tool-registry)))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (primary (task-tests--primary-agent configuration
                                               "windows-sandbox-primary"
                                               registry))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation (agent-conversation primary)
                            :registry registry
                            :agent primary
                            :command-authorization-function
                            (lambda (ignored-command ignored-directory)
                              (declare (ignore ignored-command ignored-directory))
                              ':sandboxed)))
           (tool (tool-registry-find registry "shell" "run"))
           (workspace (merge-pathnames "workspace/" root)))
      (unwind-protect
           (progn
             (uiop:ensure-all-directories-exist (list workspace))
             (let* ((result
                      (tool-execute
                       tool context
                       (json-object "command" "printf async > async.txt"
                                     "directory" (uiop:native-namestring workspace)
                                    "async" t)))
                    (record (rest (tool-result-details result)))
                   (job-id (getf (getf record :job) :id))
                    (job (and job-id
                              (task-orchestrator-find-visible-job
                               orchestrator job-id primary "shell.run"))))
               (test-assert (and job-id job)
                            "asynchronous shell.run returns an inspectable job")
               (when job
                 (multiple-value-bind (snapshot terminal-p)
                     (session-job-await job 30)
                   (declare (ignore snapshot))
                   (test-assert terminal-p
                                "asynchronous sandboxed shell.run terminates")
                   (test-assert
                    (uiop:file-exists-p (merge-pathnames "async.txt" workspace))
                    "asynchronous sandboxed shell.run writes in the workspace")))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root
                                        :validate t
                                        :if-does-not-exist ':ignore)))))

(defun test-windows-shell-sandbox-missing-helper-fails-closed ()
  "Verify missing Windows helper does not silently fall back."
  (when (typep *platform* 'win32-platform)
    (let ((saved (uiop:getenv "CL_EXEC_SANDBOX_WINDOWS_HELPER")))
      (unwind-protect
           (progn
             (platform-setenv "CL_EXEC_SANDBOX_WINDOWS_HELPER"
                              (namestring
                               (merge-pathnames "missing-sandbox-helper.exe"
                                                (uiop:temporary-directory))))
             (with-test-configuration (configuration root)
               (declare (ignore configuration))
          (handler-case
              (test-assert
               (not (windows-sandbox-tests--successful-exit-p
                     (windows-sandbox-tests--command
                      root "printf should-not-run > missing-helper.txt")))
               "missing Windows sandbox helper fails closed")
            (platform-capability-unavailable ()
              (test-assert t "missing helper signals unavailable")))
          (test-assert (not (probe-file (merge-pathnames "missing-helper.txt" root)))
                       "missing helper command never executes")))
        (if saved
            (platform-setenv "CL_EXEC_SANDBOX_WINDOWS_HELPER" saved)
            (platform-unsetenv "CL_EXEC_SANDBOX_WINDOWS_HELPER"))))))
