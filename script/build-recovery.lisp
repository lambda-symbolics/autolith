(require :asdf)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)
(require :sb-posix)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (arguments (uiop:command-line-arguments))
       (child-p (and arguments (string= (first arguments) "--child")))
       (home (user-homedir-pathname))
       (data-home
         (uiop:ensure-directory-pathname
          (or (uiop:getenv "XDG_DATA_HOME")
              (merge-pathnames ".local/share/" home))))
       (default-core
         (merge-pathnames "autolith/recovery/autolith-recovery.core" data-home))
       (core-pathname
         (pathname
          (or (if child-p (second arguments) (first arguments))
              default-core)))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup (merge-pathnames "quicklisp/setup.lisp" home))
       (quicklisp-setup (if (probe-file project-setup)
                           project-setup
                           user-setup)))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (labels ((load-recovery-source ()
             "Load only the packages needed to compile the recovery runtime."
             (unless (probe-file quicklisp-setup)
               (error "Recovery build needs Quicklisp at ~A." quicklisp-setup))
             (load quicklisp-setup)
             (uiop:symbol-call '#:ql '#:quickload :serapeum :silent t)
             (let ((package (or (find-package "AUTOLITH")
                                (make-package "AUTOLITH" :use '("CL")))))
               (export (mapcar (lambda (name) (intern name package))
                               '("RECOVERY-MAIN" "RECOVERY-IMAGE-SAVE"))
                       package))
             (load (merge-pathnames "recovery/runtime.lisp" source-root)))

           (git-output (arguments)
             "Return trimmed output from one source-root Git command."
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              (uiop:run-program
               (append (list "git" "-C" (namestring source-root)) arguments)
               :output :string
               :error-output :output)))

           (source-blob (relative-pathname)
             "Return the Git object identity of one current source file."
             (git-output (list "hash-object" relative-pathname)))

           (source-identity ()
             "Return exact provenance for inputs controlling the recovery image."
             (let* ((paths '("script/build-recovery"
                             "script/build-recovery.lisp"
                             "recovery/runtime.lisp"
                             "recovery/launcher.lisp"
                             "bin/autolith"
                             "bin/autolith-active"
                             "bin/autolith-runtime"
                             "script/check"
                             "script/check.lisp"
                             "qlfile.lock"
                             "sbcl.version"))
                    (status (git-output
                             (append '("status" "--porcelain" "--")
                                     paths))))
               (list :source-commit (git-output '("rev-parse" "HEAD"))
                     :source-clean-p (zerop (length status))
                     :runtime-blob (source-blob "recovery/runtime.lisp")
                     :builder-blob (source-blob "script/build-recovery")
                     :builder-source-blob
                     (source-blob "script/build-recovery.lisp")
                     :source-launcher-blob
                     (source-blob "recovery/launcher.lisp")
                     :stable-launcher-blob (source-blob "bin/autolith")
                     :active-source-launcher-blob
                     (source-blob "bin/autolith-active")
                     :runtime-adapter-blob
                     (source-blob "bin/autolith-runtime")
                     :check-blob (source-blob "script/check")
                     :check-source-blob (source-blob "script/check.lisp")
                     :dependency-lock-blob (source-blob "qlfile.lock")
                     :sbcl-version-blob (source-blob "sbcl.version"))))

           (probe-core (pathname sbcl-command)
             "Require PATHNAME to boot and report the current recovery protocol."
             (let* ((output
                      (uiop:run-program
                       (list sbcl-command
                             "--noinform"
                             "--core" (namestring pathname)
                             "--end-runtime-options"
                             (namestring source-root)
                             "--probe")
                       :output :string
                       :error-output :output))
                    (*read-eval* nil)
                    (stream (make-string-input-stream output))
                    (end-marker (gensym "RECOVERY-PROBE-END-"))
                    (form (read stream nil end-marker))
                    (trailing (read stream nil end-marker)))
               (unless (and (listp form)
                            (eq (first form) :recovery-probe)
                            (= (or (getf (rest form) :version) 0) 2)
                            (string= (or (getf (rest form) :sbcl-version) "")
                                     (lisp-implementation-version))
                            (string= (or (getf (rest form) :operating-system) "")
                                     (software-type))
                            (string= (or (getf (rest form)
                                              :operating-system-version)
                                         "")
                                     (software-version))
                            (string= (or (getf (rest form) :architecture) "")
                                     (machine-type))
                            (eq trailing end-marker))
                 (error "The recovery child produced an invalid probe: ~S"
                        output))))

           (write-manifest (pathname identity)
             "Write the installed recovery core identity beside PATHNAME."
             (let* ((directory (uiop:pathname-directory-pathname pathname))
                    (manifest (merge-pathnames "manifest.sexp" directory))
                    (temporary
                      (merge-pathnames
                       (format nil ".manifest.~D.tmp" (sb-posix:getpid))
                       directory)))
               (with-open-file (stream temporary
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (let ((*print-circle* t)
                       (*print-pretty* nil)
                       (*print-readably* t))
                   (prin1 (append (list :recovery-image
                                        :version 2
                                        :core (namestring pathname)
                                        :sbcl-version
                                        (lisp-implementation-version)
                                        :operating-system (software-type)
                                        :operating-system-version
                                        (software-version)
                                        :architecture (machine-type)
                                        :built-at (get-universal-time))
                                  identity)
                          stream)
                   (terpri stream)
                   (finish-output stream)))
               (sb-posix:chmod (namestring temporary) #o444)
               (uiop:rename-file-overwriting-target temporary manifest))))
    (if child-p
        (progn
          (load-recovery-source)
          (uiop:symbol-call '#:autolith '#:recovery-image-save core-pathname))
        (let* ((directory (uiop:pathname-directory-pathname core-pathname))
               (temporary
                 (merge-pathnames
                  (format nil ".autolith-recovery.~D.core" (sb-posix:getpid))
                  directory))
               (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
               (identity-before (source-identity)))
          (ensure-directories-exist core-pathname)
          (when (probe-file temporary)
            (delete-file temporary))
          (uiop:run-program
           (list sbcl-command
                 "--noinform"
                 "--script"
                 (namestring script-path)
                 "--child"
                 (namestring temporary))
           :input :interactive
           :output :interactive
           :error-output :interactive)
          (unless (probe-file temporary)
            (error "The recovery child did not publish its temporary core."))
          (probe-core temporary sbcl-command)
          (let ((identity-after (source-identity)))
            (unless (equal identity-before identity-after)
              (error "Recovery image inputs changed while the core was built.")))
          (when (probe-file core-pathname)
            (sb-posix:chmod (namestring core-pathname) #o600))
          (uiop:rename-file-overwriting-target temporary core-pathname)
          (sb-posix:chmod (namestring core-pathname) #o444)
          (write-manifest core-pathname identity-before)
          (format t "~&Installed pristine recovery image at ~A.~%" core-pathname)))))
