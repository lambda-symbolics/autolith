;;;; -- Standalone Supervisor Boot --

(require :asdf)

;; Load only this module. Do not read agent startup files or ASDF registries.
(let ((root (uiop:pathname-parent-directory-pathname *load-truename*)))
  (load (merge-pathnames "supervisor/package.lisp" root))
  (load (merge-pathnames "supervisor/process.lisp" root))
  (load (merge-pathnames #+win32 "supervisor/platform-windows.lisp"
                        #-win32 "supervisor/platform-posix.lisp"
                        root)))

(uiop:quit (uiop:symbol-call '#:autolith-supervisor '#:main))
