(require :asdf)

;;; SBCL on NetBSD still stubs CONTEXT-FLOAT-REGISTER. Compiling
;;; ieee-floats through Opticl signals that warning, and ASDF treats it
;;; as a compile-file failure.
#+netbsd
(setf asdf:*compile-file-failure-behaviour* :warn)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defun build-active--environment-directory (variable fallback)
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let* ((value (uiop:getenv variable))
         (pathname (and value
                        (plusp (length value))
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (default-core
         (merge-pathnames "active/autolith-active.core"
                          (autolith-application-root :data)))
       (arguments (uiop:command-line-arguments))
       ;; ACTIVE-IMAGE-INSTALL starts this script again with --child on hosts
       ;; without fork; that process loads the system and saves itself.
       (child-core
         (and (string= (or (first arguments) "") "--child")
              (second arguments)))
       (core-pathname
         (pathname
          (or (and (not child-core) (first arguments))
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
          (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
  (asdf:load-system :autolith)
  (cond
    (child-core
     (format t "~&Saving the preloaded active image.~%")
     (finish-output)
     (uiop:symbol-call '#:autolith '#:active-image-save
                       source-root
                       (uiop:parse-native-namestring child-core)))
    (t
     (format t "~&Saving and validating the preloaded active image.~%")
     (finish-output)
     (uiop:symbol-call '#:autolith '#:active-image-install
                       source-root
                       core-pathname)
     (format t "~&Installed preloaded active image at ~A.~%" core-pathname))))
