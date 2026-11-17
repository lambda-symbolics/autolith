(require :asdf)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)
(require :sb-posix)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defun build-recovery--environment-directory (variable fallback)
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let* ((value    (uiop:getenv variable))
         (pathname (and value
                        (plusp (length value))
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

(let* ((script-path      (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root      (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl-minimum.version" source-root))
       (arguments        (uiop:command-line-arguments))
       (child-p          (and arguments (string= (first arguments) "--child")))
       (home             (user-homedir-pathname))
       (default-core
         (merge-pathnames "recovery/autolith-recovery.core"
                          (autolith-application-root :data)))
       (core-pathname
         (pathname
          (or (if child-p (second arguments) (first arguments))
              default-core)))
       (project-setup    (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup       (merge-pathnames "quicklisp/setup.lisp" home))
       (quicklisp-setup  (if (probe-file project-setup)
                            project-setup
                            user-setup)))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file quicklisp-setup)
    (error "Recovery provenance needs Quicklisp at ~A." quicklisp-setup))
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload :sbcl-generations :silent t)
  (labels ((load-recovery-source ()
             "Load only the packages needed to compile the recovery runtime."
             (uiop:symbol-call '#:ql '#:quickload :serapeum :silent t)
             (let ((package (or (find-package "AUTOLITH")
                                (make-package "AUTOLITH" :use '("CL")))))
               (export (mapcar (lambda (name) (intern name package))
                               '("RECOVERY-MAIN" "RECOVERY-IMAGE-SAVE"))
                       package))
             (load (merge-pathnames "recovery/runtime.lisp" source-root)))

           (source-identity ()
             "Return exact provenance for inputs controlling the recovery image."
             (let* ((paths      '("script/build-recovery"
                                  "script/build-recovery.lisp"
                                  "script/runtime-requirement.lisp"
                                  "script/windows-stat.lisp"
                                  "recovery/runtime.lisp"
                                  "script/trace-context.lisp"
                                  "recovery/launcher.lisp"
                                  "bin/autolith"
                                  "bin/autolith-active"
                                  "bin/autolith-runtime"
                                  "script/check"
                                  "script/check.lisp"
                                  "qlfile.lock"
                                  "sbcl-minimum.version"
                                  "sbcl.version"))
                    (source     (uiop:symbol-call '#:sbcl-generations
                                                  '#:make-source-provenance
                                                  source-root))
                    (identities (uiop:symbol-call '#:sbcl-generations
                                                  '#:source-file-identities
                                                  source paths)))
               (flet ((file-identity (path)
                        (second (assoc path identities :test #'string=))))
                 (list :source-commit
                       (uiop:symbol-call '#:sbcl-generations '#:source-revision source)
                       :source-clean-p
                       (uiop:symbol-call '#:sbcl-generations '#:source-clean-p
                                         source :paths paths)
                       :runtime-blob                (file-identity "recovery/runtime.lisp")
                       :trace-context-source-blob
                       (file-identity "script/trace-context.lisp")
                       :builder-blob                (file-identity "script/build-recovery")
                       :builder-source-blob         (file-identity "script/build-recovery.lisp")
                       :runtime-requirement-blob    (file-identity "script/runtime-requirement.lisp")
                       :windows-stat-blob           (file-identity "script/windows-stat.lisp")
                       :source-launcher-blob        (file-identity "recovery/launcher.lisp")
                       :stable-launcher-blob        (file-identity "bin/autolith")
                       :active-source-launcher-blob (file-identity "bin/autolith-active")
                       :runtime-adapter-blob        (file-identity "bin/autolith-runtime")
                       :check-blob                  (file-identity "script/check")
                       :check-source-blob           (file-identity "script/check.lisp")
                       :dependency-lock-blob        (file-identity "qlfile.lock")
                       :sbcl-minimum-version-blob   (file-identity "sbcl-minimum.version")
                       :sbcl-version-blob           (file-identity "sbcl.version")))))

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
                       :output       ':string
                       :error-output ':output))
                    (*read-eval* nil)
                    (stream      (make-string-input-stream output))
                    (end-marker  (gensym "RECOVERY-PROBE-END-"))
                    (form        (read stream nil end-marker))
                    (trailing    (read stream nil end-marker)))
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
                    (manifest  (merge-pathnames "manifest.sexp" directory))
                    (temporary
                      (merge-pathnames
                       (format nil ".manifest.~D.tmp" (sb-posix:getpid))
                       directory)))
               (with-open-file (stream temporary
                                       :direction         ':output
                                       :if-exists         ':supersede
                                       :if-does-not-exist ':create
                                       :external-format   ':utf-8)
                 (let ((*print-circle*   t)
                       (*print-pretty*   nil)
                       (*print-readably* t))
                   (prin1 (append (list :recovery-image
                                        :version 2
                                        :core             (namestring pathname)
                                        :sbcl-version
                                        (lisp-implementation-version)
                                        :operating-system (software-type)
                                        :operating-system-version
                                        (software-version)
                                        :architecture     (machine-type)
                                        :built-at         (get-universal-time))
                                  identity)
                          stream)
                   (terpri stream)
                   (finish-output stream)))
               (autolith-script-set-file-mode temporary #o444)
               (autolith-script-replace-file temporary manifest))))
    (if child-p
        (progn
          (load-recovery-source)
          (uiop:symbol-call '#:autolith '#:recovery-image-save core-pathname))
        (let* ((directory       (uiop:pathname-directory-pathname core-pathname))
               (temporary
                 (merge-pathnames
                  (format nil ".autolith-recovery.~D.core" (sb-posix:getpid))
                  directory))
               (sbcl-command    (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
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
           :input        ':interactive
           :output       ':interactive
           :error-output ':interactive)
          (unless (probe-file temporary)
            (error "The recovery child did not publish its temporary core."))
          (probe-core temporary sbcl-command)
          (let ((identity-after (source-identity)))
            (unless (equal identity-before identity-after)
              (error "Recovery image inputs changed while the core was built.")))
          (when (probe-file core-pathname)
            (autolith-script-set-file-mode core-pathname #o600))
          (autolith-script-replace-file temporary core-pathname)
          (autolith-script-set-file-mode core-pathname #o444)
          (write-manifest core-pathname identity-before)
          (format t "~&Installed pristine recovery image at ~A.~%" core-pathname)))))
