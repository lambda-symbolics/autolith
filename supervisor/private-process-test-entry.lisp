;;;; -- Standalone Private Process Tests --

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname *load-truename*)))
  (asdf:load-asd (merge-pathnames "autolith-supervisor.asd" root))
  (handler-bind ((style-warning #'muffle-warning))
    (dolist (file '("supervisor/package.lisp" "supervisor/framing.lisp"
                    #+win32 "supervisor/platform-windows.lisp"
                    #-win32 "supervisor/platform-posix.lisp"
                    "supervisor/process.lisp" "supervisor/private-process.lisp"
                    #+win32 "supervisor/private-process-windows.lisp"
                    #-win32 "supervisor/private-process-posix.lisp"
                    "supervisor/private-process-tests.lisp"))
      (load (merge-pathnames file root)))))

(format t "~D private subprocess checks passed.~%"
        (autolith-supervisor::run-private-process-tests))
