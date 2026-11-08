(in-package #:autolith)

;;;; -- Windows Shell Sandbox --

(-> windows-sandbox-tests--enabled-p () boolean)
(defun windows-sandbox-tests--enabled-p ()
  "Require native enforcement before running Windows integration cases."
  (test-assert (application--command-sandbox-available-p)
               "native Windows network-isolated sandbox is available")
  t)

(-> windows-sandbox-tests--profile-helper-script (pathname) string)
(defun windows-sandbox-tests--profile-helper-script (source-path)
  "Return a PowerShell AST fixture for Remove-SandboxUserProfile."
  (format nil
          (concatenate 'string
           "$ErrorActionPreference = 'Stop'~%~%"
           "$source = Get-Content -LiteralPath '~A' -Raw~%"
           "$tokens = $null; $errors = $null~%"
           "$ast = [System.Management.Automation.Language.Parser]::ParseInput($source, [ref]$tokens, [ref]$errors)~%"
           "if ($errors.Count) { throw ($errors | Out-String) }~%"
           "$function = $ast.Find({ param($node) $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq 'Remove-SandboxUserProfile' }, $true)~%"
           "if (-not $function) { throw 'Remove-SandboxUserProfile was not found in the script AST.' }~%"
           ". ([scriptblock]::Create($function.Extent.Text))~%~%"
           "function Assert([bool]$condition, [string]$message) { if (-not $condition) { throw $message } }~%"
           "function Start-Sleep { param([int]$Milliseconds) }~%~%"
           "$sid = 'S-1-5-21-100-200-300-400'~%"
           "$otherSid = 'S-1-5-21-100-200-300-401'~%"
           "$script:processes = @()~%"
           "$script:profiles = @()~%"
           "$script:stopped = @()~%"
           "$script:removeCalls = 0~%"
           "$script:removeFailures = 0~%~%"
           "function Get-CimInstance {~%"
           "  param([string]$ClassName, [string]$Filter)~%"
           "  if ($ClassName -eq 'Win32_Process') { return $script:processes }~%"
           "  if ($ClassName -eq 'Win32_UserProfile') { return $script:profiles }~%"
           "  throw \"Unexpected CIM class: $ClassName\"~%"
           "}~%"
           "function Invoke-CimMethod {~%"
           "  param($InputObject, [string]$MethodName)~%"
           "  [pscustomobject]@{ ReturnValue = $InputObject.ReturnValue; Sid = $InputObject.Sid }~%"
           "}~%"
           "function Stop-Process { param([int]$Id, [switch]$Force) $script:stopped += $Id }~%"
           "function Remove-CimInstance {~%"
           "  param([Parameter(ValueFromPipeline = $true)]$InputObject)~%"
           "  process { $script:removeCalls++; if ($script:removeFailures -gt 0) { $script:removeFailures--; throw 'profile is busy' } }~%"
           "}~%~%"
           "$script:processes = @([pscustomobject]@{ ProcessId = 11; ReturnValue = 0; Sid = $sid }, [pscustomobject]@{ ProcessId = 12; ReturnValue = 0; Sid = $otherSid }, [pscustomobject]@{ ProcessId = 13; ReturnValue = 5; Sid = $sid })~%"
           "$script:profiles = @()~%"
           "Remove-SandboxUserProfile -Sid $sid -TimeoutSeconds 1~%"
           "Assert ($script:stopped.Count -eq 1 -and $script:stopped[0] -eq 11) 'only a process with the exact owner SID is stopped'~%~%"
           "$script:stopped = @(); $script:processes = @(); $script:profiles = @()~%"
           "Remove-SandboxUserProfile -Sid $sid -TimeoutSeconds 1~%"
           "Assert ($script:stopped.Count -eq 0 -and $script:removeCalls -eq 0) 'disappeared process and profile are harmless'~%~%"
           "$script:removeCalls = 0; $script:removeFailures = 1; $script:profiles = @([pscustomobject]@{ SID = $sid })~%"
           "Remove-SandboxUserProfile -Sid $sid -TimeoutSeconds 60~%"
           "Assert ($script:removeCalls -eq 2) 'transient profile removal failure is retried'~%~%"
           "$script:removeCalls = 0; $script:removeFailures = 100; $failure = $null~%"
           "try { Remove-SandboxUserProfile -Sid $sid -TimeoutSeconds 0 } catch { $failure = $_.Exception.Message }~%"
           "Assert ($failure -match 'profile is busy') 'permanent profile removal failure is bounded and reported'~%"
           "'PASS'~%"
           "")
          (with-output-to-string (stream)
            (loop for character across (namestring source-path)
                  do (write-char character stream)
                  when (char= character #\')
                    do (write-char character stream)))))

(-> windows-sandbox-tests--command
    (pathname string &key (:timeout (option integer)) (:authorization keyword)) tool-result)
(defun windows-sandbox-tests--command (root command &key timeout (authorization ':sandboxed))
  "Run sandboxed COMMAND in ROOT through the public shell.run tool."
  (let* ((configuration (configuration-copy
                         (test-configuration) :working-directory root))
         (registry      (make-default-tool-registry))
         (conversation  (conversation-create configuration
                                              :identifier "windows-sandbox"))
         (context       (make-instance 'tool-context
                                       :configuration configuration
                                       :worker        nil
                                       :conversation  conversation
                                       :registry      registry
                                       :command-authorization-function
                                       (lambda (ignored-command ignored-directory)
                                         (declare (ignore ignored-command ignored-directory))
                                        authorization)))
         (tool          (tool-registry-find registry "shell" "run"))
         (arguments     (json-object "command" command
                                     "directory" ".")))
    (when timeout
      (setf (gethash "timeout-seconds" arguments) timeout))
    (unwind-protect
         (let ((result (tool-execute tool context arguments)))
           (format t "~&Windows shell command: ~A~%~A~%" command (tool-result-content result))
           result)
      (tool-registry-close-runtime-state registry))))

(-> windows-sandbox-tests--successful-exit-p (tool-result) boolean)
(defun windows-sandbox-tests--successful-exit-p (result)
  "Return whether RESULT reports a successful shell exit."
  (and (tool-result-success-p result)
       (search "exit 0" (tool-result-content result))
       t))

(-> test-windows-sandbox-bundle-installation () null)
(defun test-windows-sandbox-bundle-installation ()
  "Exercise complete bundle selection and private native generation publication."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((script (merge-pathnames "script/check-windows-sandbox-bundle.ps1"
                                    (asdf:system-source-directory :autolith))))
      (multiple-value-bind (output diagnostics status)
          (uiop:run-program
           (list (or (uiop:getenv "POWERSHELL") "powershell.exe")
                 "-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass"
                 "-File" (uiop:native-namestring script)
                 "-Root" (uiop:native-namestring root))
           :output ':string :error-output ':string :ignore-error-status t)
        (test-assert (eql status 0)
                     (format nil "bundle fixture failed: ~A~%~A" output diagnostics)))))
  nil)

(-> test-windows-sandbox-profile-cleanup-helper () null)
(defun test-windows-sandbox-profile-cleanup-helper ()
  "Exercise the packaged sandbox profile cleanup helper with mocked CIM calls."
  (when (typep *platform* 'win32-platform)
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((source     (merge-pathnames "script/check-windows-sandbox.ps1"
                                          (asdf:system-source-directory :autolith)))
             (fixture    (merge-pathnames "profile-helper-tests.ps1" root))
             (powershell (or (uiop:getenv "AUTOLITH_PWSH")
                             (uiop:getenv "POWERSHELL")
                             "powershell.exe")))
        (with-open-file (stream fixture :direction ':output :if-exists ':supersede)
          (write-string (windows-sandbox-tests--profile-helper-script source) stream))
        (multiple-value-bind (output diagnostics status)
            (uiop:run-program
             (list powershell "-NoProfile" "-NonInteractive" "-File"
                   (namestring fixture))
             :output              ':string
             :error-output        ':string
             :ignore-error-status t)
          (test-assert (and (eql status 0) (search "PASS" output))
                       (format nil "profile cleanup fixture failed: ~A~%~A"
                               output diagnostics))))))
  nil)

(-> test-windows-shell-sandbox-git-null () null)
(defun test-windows-shell-sandbox-git-null ()
  "Exercise unmodified Git and its /dev/null handling through shell.run."
  (when (windows-sandbox-tests--enabled-p)
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((workspace (merge-pathnames "workspace/" root))
             (git       (or (uiop:getenv "AUTOLITH_TEST_GIT")
                            (uiop:native-namestring
                             (merge-pathnames
                              "Git/cmd/git.exe"
                              (uiop:ensure-directory-pathname
                               (or (uiop:getenv "ProgramFiles") "C:/Program Files/")))))))
        (ensure-directories-exist workspace)
        (multiple-value-bind (output diagnostics status)
            (uiop:run-program (list git "init" "--quiet")
                              :directory           workspace
                              :output ':string :error-output ':string
                              :ignore-error-status t)
          (test-assert (eql status 0)
                       (format nil "Git initialization failed: ~A~%~A"
                               output diagnostics)))
        (with-open-file (stream (merge-pathnames "probe.txt" workspace)
                                :direction ':output)
          (write-line "autolith-git-null-probe" stream))
        (dolist (case (list (list "status --porcelain" 0 "?? probe.txt")
                           (list "rev-parse --show-toplevel" 0
                                 (string-right-trim
                                  "/" (substitute #\/ #\\
                                                  (uiop:native-namestring workspace))))
                           (list "diff --no-index -- /dev/null probe.txt" 1
                                 "+autolith-git-null-probe")))
          (destructuring-bind (arguments expected-status expected-output) case
            (let* ((result
                     (windows-sandbox-tests--command
                      workspace
                      (format nil "& ~A ~A; exit $LASTEXITCODE"
                              (test-fixture-shell-quote *platform* git) arguments)))
                   (content (tool-result-content result)))
              (test-assert
               (and (search (format nil "exit ~D" expected-status) content)
                    (search expected-output content))
               (format nil "Sandboxed Git ~A failed: ~A" arguments content))))))))
  nil)

(-> test-windows-shell-sandbox-integration () null)
(defun test-windows-shell-sandbox-integration ()
  "Exercise native Windows shell containment through shell.run."
  (when (windows-sandbox-tests--enabled-p)
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((workspace      (merge-pathnames "workspace/" root))
             (outside        (merge-pathnames "outside/" root))
             (protected      (merge-pathnames ".git/" workspace))
             (inside-file    (merge-pathnames "inside.txt" workspace))
             (outside-file   (merge-pathnames "outside.txt" outside))
             (protected-file (merge-pathnames "config" protected)))
        (uiop:ensure-all-directories-exist
         (list workspace outside protected))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "[IO.File]::WriteAllText(~A,'inside')"
                         (test-fixture-shell-quote *platform* (namestring inside-file))))))
          (test-assert (windows-sandbox-tests--successful-exit-p result)
                       "Windows shell.run writes inside the workspace")
          (test-assert (uiop:file-exists-p inside-file)
                       "the workspace write is observable"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "[IO.File]::WriteAllText(~A,'outside')"
                         (test-fixture-shell-quote *platform* (namestring outside-file))))))
          (test-assert (not (windows-sandbox-tests--successful-exit-p result))
                       "Windows shell.run denies writes outside the workspace")
          (test-assert (not (uiop:file-exists-p outside-file))
                       "the denied outside write leaves no file"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 (format nil "[IO.File]::WriteAllText(~A,'protected')"
                         (test-fixture-shell-quote *platform* (namestring protected-file))))))
          (test-assert (not (windows-sandbox-tests--successful-exit-p result))
                       "Windows shell.run denies protected metadata mutation")
          (test-assert (not (uiop:file-exists-p protected-file))
                       "the protected metadata write leaves no file"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace
                 "[IO.File]::WriteAllText('temp-path.txt',$env:TEMP); [IO.File]::WriteAllText((Join-Path $env:TEMP 'private.txt'),'temporary')")))
          (test-assert (windows-sandbox-tests--successful-exit-p result)
                       "Windows shell.run permits its private temporary directory")
          (test-assert
           (not (uiop:directory-exists-p
                 (uiop:parse-native-namestring
                  (uiop:read-file-string (merge-pathnames "temp-path.txt" workspace)))))
           "private temporary directory is removed after execution"))
        (let ((result
                (windows-sandbox-tests--command
                 workspace "Start-Sleep 5" :timeout 1)))
          (test-assert (not (tool-result-success-p result))
                       "Windows shell.run reports command timeout")))))
  nil)

(-> test-windows-shell-sandbox-async () null)
(defun test-windows-shell-sandbox-async ()
  "Exercise the asynchronous shell.run sandbox lifecycle on Windows."
  (when (windows-sandbox-tests--enabled-p)
    (let* ((base          (test-configuration))
           (root          (test-configuration-root base))
           (workspace     (let ((path (merge-pathnames "workspace/" root)))
                            (ensure-directories-exist path)
                            path))
           (configuration (configuration-copy base :working-directory workspace))
           (registry      (task-augment-tool-registry (make-default-tool-registry)))
           (run-tool      (tool-registry-find registry "child" "spawn"))
           (orchestrator  (task-run-tool-orchestrator run-tool))
           (primary       (task-tests--primary-agent configuration
                                                     "windows-sandbox-primary"
                                                     registry))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker        nil
                            :conversation  (agent-conversation primary)
                            :registry      registry
                            :agent         primary
                            :command-authorization-function
                            (lambda (ignored-command ignored-directory)
                              (declare (ignore ignored-command ignored-directory))
                              ':sandboxed)))
           (tool          (tool-registry-find registry "shell" "run")))
      (unwind-protect
           (progn
             (uiop:ensure-all-directories-exist (list workspace))
             (let* ((result
                      (tool-execute
                       tool context
                       (json-object "command" "[IO.File]::WriteAllText('async.txt','async')"
                                     "directory" (uiop:native-namestring workspace)
                                    "async" t)))
                    (record (rest (tool-result-details result)))
                    (job-id (getf (getf record :job) :id))
                    (job    (and job-id
                                 (task-orchestrator-find-visible-job
                                  orchestrator job-id :viewer primary :tool-name "shell.run"))))
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
                    "asynchronous sandboxed shell.run writes in the workspace"))))
             (let* ((started    (merge-pathnames "cancel-started.txt" workspace))
                    (result
                      (tool-execute tool context
                                    (json-object "command" "[IO.File]::WriteAllText('cancel-temp.txt',$env:TEMP); [IO.File]::WriteAllText('cancel-started.txt','started'); Start-Sleep 10; [IO.File]::WriteAllText('cancel-late.txt','leaked')"
                                                 "async" t)))
                    (record     (getf (rest (tool-result-details result)) :job))
                    (identifier (getf record :id))
                    (job        (task-orchestrator-find-visible-job orchestrator identifier
                                                                    :viewer    primary
                                                                    :tool-name "shell.run")))
               (test-assert (task-tests--wait-until (lambda () (probe-file started)) 10)
                            "sandboxed command starts before cancellation")
               (tool-execute (tool-registry-find registry "job" "cancel") context
                             (json-object "id" identifier))
               (multiple-value-bind (snapshot terminal-p) (session-job-await job 30)
                 (test-assert (and terminal-p (eq (getf snapshot :state) ':aborted))
                              "sandboxed command cancellation terminates the job"))
               (test-assert (not (probe-file (merge-pathnames "cancel-late.txt" workspace)))
                            "cancelled command cannot finish its delayed write")
               (test-assert
                (not (uiop:directory-exists-p
                      (uiop:parse-native-namestring
                       (uiop:read-file-string (merge-pathnames "cancel-temp.txt" workspace)))))
                "cancellation removes the private temporary scope")))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (platform-delete-directory-tree *platform* root
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-windows-shell-sandbox-missing-helper-fails-closed () null)
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
                          root "[IO.File]::WriteAllText('missing-helper.txt','should-not-run')")))
                   "missing Windows sandbox helper fails closed")
                (platform-capability-unavailable ()
                  (test-assert t "missing helper signals unavailable")))
              (test-assert (not (probe-file (merge-pathnames "missing-helper.txt" root)))
                           "missing helper command never executes")))
        (if saved
            (platform-setenv "CL_EXEC_SANDBOX_WINDOWS_HELPER" saved)
            (platform-unsetenv "CL_EXEC_SANDBOX_WINDOWS_HELPER")))))
  nil)

(-> test-windows-shell-sandbox-network () null)
(defun test-windows-shell-sandbox-network ()
  "Verify shell networking against a live loopback listener and full-access control."
  (windows-sandbox-tests--enabled-p)
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                   :type ':stream :protocol ':tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
             (sb-bsd-sockets:socket-listen listener 4)
             (let* ((port     (nth-value 1 (sb-bsd-sockets:socket-name listener)))
                    (command
                     (format nil "try{$c=[Net.Sockets.TcpClient]::new(); $a=$c.BeginConnect('127.0.0.1',~D,$null,$null); if(-not $a.AsyncWaitHandle.WaitOne(2000)){throw 'timeout'}; $c.EndConnect($a); Write-Output CONNECTED; exit 0}catch{Write-Output BLOCKED; exit 7}" port))
                    (control  (windows-sandbox-tests--command root command
                                                               :authorization ':full-access))
                    (isolated (windows-sandbox-tests--command root command)))
               (test-assert (windows-sandbox-tests--successful-exit-p control)
                            "explicit full access connects to the live listener")
               (test-assert (and (not (windows-sandbox-tests--successful-exit-p isolated))
                                 (search "BLOCKED" (tool-result-content isolated)))
                            "AppContainer blocks the same shell network request")))
        (sb-bsd-sockets:socket-close listener))))
  nil)
