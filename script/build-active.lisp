(require :asdf)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (home (user-homedir-pathname))
       (data-home
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "XDG_DATA_HOME")
              (merge-pathnames ".local/share/" home))))
       (default-core
         (merge-pathnames "autolith/active/autolith-active.core" data-home))
       (arguments (uiop:command-line-arguments))
       (core-pathname
         (pathname
          (or (first arguments)
              (uiop:getenv "AUTOLITH_ACTIVE_CORE")
              default-core))))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file project-setup)
    (error "Active-image builds need locked dependencies; run ./script/bootstrap."))
  (format t "~&Loading Autolith for its preloaded active image.~%")
  (finish-output)
  (load project-setup)
  (load (merge-pathnames "script/build-sandbox.lisp" source-root))
  (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
  (let ((profile-library-directory
          (merge-pathnames ".guix-profile/lib/" home))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
  (asdf:load-system :autolith)
  (format t "~&Saving and validating the preloaded active image.~%")
  (finish-output)
  (uiop:symbol-call '#:autolith '#:active-image-install
                    source-root
                    core-pathname)
  (format t "~&Installed preloaded active image at ~A.~%" core-pathname))
