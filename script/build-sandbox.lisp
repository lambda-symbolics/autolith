(require :asdf)

;;; Build the platform helper. Windows also installs a private copy outside
;;; the source workspace, since the helper cannot be exposed as writable.
#+linux
(let* ((system-root (asdf:system-source-directory :cl-exec-sandbox))
       (environment (uiop:getenv "CL_EXEC_SANDBOX_HELPER")))
  (unless (and environment (probe-file environment))
    (let ((builder (merge-pathnames "scripts/build-helper" system-root)))
      (unless (probe-file builder)
        (error "cl-exec-sandbox helper builder is missing at ~A." builder))
      (uiop:run-program (list "/usr/bin/env" "bash" (namestring builder))
                        :output ':interactive
                        :error-output ':interactive))))

#+win32
(progn
  (load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))
  (let* ((system-root (asdf:system-source-directory :cl-exec-sandbox))
         (builder (merge-pathnames "scripts/build-windows-helper.ps1" system-root))
         (installed (merge-pathnames "native/sandbox/cl-exec-sandbox-windows.exe"
                                     (autolith-application-root ':data)))
         (staged (make-pathname :type "new.exe" :defaults installed)))
    (unless (probe-file builder)
      (error "cl-exec-sandbox Windows helper builder is missing at ~A." builder))
    (uiop:with-current-directory (system-root)
      (uiop:run-program
       (list (or (uiop:getenv "POWERSHELL") "powershell.exe")
             "-NoProfile" "-ExecutionPolicy" "Bypass" "-File"
             (uiop:native-namestring builder))
       :output ':interactive :error-output ':interactive))
    (ensure-directories-exist installed)
    (unwind-protect
         (progn
           (uiop:copy-file (merge-pathnames "build/cl-exec-sandbox-windows.exe" system-root)
                           staged)
           (autolith-script-replace-file staged installed))
      (when (probe-file staged) (delete-file staged)))))

#-(or linux win32)
(format t "~&Skipping the cl-exec-sandbox native helper on this platform.~%")
