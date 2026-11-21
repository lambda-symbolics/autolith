(require :asdf)

;;; Build the platform helper. Windows also installs a private copy outside
;;; the source workspace, since the helper cannot be exposed as writable.
#+(or linux darwin freebsd netbsd openbsd)
(let ((variables '("CL_EXEC_SANDBOX_PROCESS_GROUP_HELPER"
                   #+linux "CL_EXEC_SANDBOX_HELPER")))
  (unless (every (lambda (variable)
                   (let ((path (uiop:getenv variable)))
                     (and path (plusp (length path)) (probe-file path))))
                 variables)
    (let* ((system-root (asdf:system-source-directory :cl-exec-sandbox))
           (builder     (merge-pathnames "scripts/build-helper" system-root)))
      (unless (probe-file builder)
        (error "cl-exec-sandbox helper builder is missing at ~A." builder))
      (uiop:run-program (list "/usr/bin/env" "bash" (namestring builder))
                        :output       ':interactive
                        :error-output ':interactive))))

#+win32
(progn
  (load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))
  (let* ((system-root     (asdf:system-source-directory :cl-exec-sandbox))
         (builder         (merge-pathnames "scripts/build-windows-helper.ps1" system-root))
         (bundle-script   (merge-pathnames "scripts/windows-bundle.ps1" system-root))
         (script-root     (uiop:pathname-directory-pathname *load-truename*))
         (launcher-bundle (merge-pathnames "../bin/cl-exec-sandbox-bundle.ps1" script-root))
         (installer       (merge-pathnames "windows-sandbox.ps1" script-root))
         (installed       (merge-pathnames "native/sandbox/"
                                          (autolith-application-root ':data)))
         (explicit        (uiop:getenv "CL_EXEC_SANDBOX_WINDOWS_HELPER"))
         (powershell      (or (uiop:getenv "POWERSHELL") "powershell.exe")))
    (unless (probe-file bundle-script)
      (error "cl-exec-sandbox standalone bundle helper is missing at ~A." bundle-script))
    (uiop:copy-file bundle-script launcher-bundle)
    (uiop:with-current-directory (system-root)
      (if (and explicit (plusp (length explicit)))
          (uiop:run-program
           (list powershell "-NoProfile" "-ExecutionPolicy" "Bypass" "-File"
                 (uiop:native-namestring installer) "-Action" "Validate"
                 "-Source" explicit "-Root" (uiop:native-namestring installed))
           :output ':interactive :error-output ':interactive)
          (progn
            (unless (probe-file builder)
              (error "cl-exec-sandbox Windows helper builder is missing at ~A." builder))
            (uiop:run-program
             (list powershell "-NoProfile" "-ExecutionPolicy" "Bypass" "-File"
                   (uiop:native-namestring installer) "-Action" "BuildInstall"
                   "-Builder" (uiop:native-namestring builder)
                   "-Root" (uiop:native-namestring installed)
                   "-Workspace" (uiop:native-namestring
                                 (uiop:pathname-parent-directory-pathname script-root)))
             :output ':interactive :error-output ':interactive)
            ;; Export the installed selection to this process and its test workers.
            (autolith-script-setenv
             "CL_EXEC_SANDBOX_WINDOWS_HELPER"
             (string-trim '(#\Return #\Newline)
                          (uiop:run-program
                           (list powershell "-NoProfile" "-ExecutionPolicy" "Bypass" "-File"
                                 (uiop:native-namestring installer) "-Action" "Resolve"
                                 "-Root" (uiop:native-namestring installed))
                           :output ':string :error-output ':interactive
                           :external-format ':utf-8))))))))

#-(or linux darwin freebsd netbsd openbsd win32)
(format t "~&Skipping the cl-exec-sandbox native helper on this platform.~%")
