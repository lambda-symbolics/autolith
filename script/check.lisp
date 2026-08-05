(require :asdf)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup (merge-pathnames "quicklisp/setup.lisp"
                                    (user-homedir-pathname)))
       (quicklisp-setup (if (probe-file project-setup)
                           project-setup
                           user-setup)))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file quicklisp-setup)
    (error "Autolith needs Quicklisp at ~A" quicklisp-setup))
  (load quicklisp-setup)
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
  (asdf:test-system :autolith)
  (let* ((home (user-homedir-pathname))
         (data-home
           (uiop:ensure-directory-pathname
            (or (uiop:getenv "XDG_DATA_HOME")
                (merge-pathnames ".local/share/" home))))
         (recovery-core
           (merge-pathnames "autolith/recovery/autolith-recovery.core" data-home))
         (recovery-manifest
           (merge-pathnames "autolith/recovery/manifest.sexp" data-home))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (unless (probe-file recovery-core)
      (error "Autolith's pristine recovery image is missing; run ./script/bootstrap."))
    (unless (probe-file recovery-manifest)
      (error "Autolith's pristine recovery manifest is missing; run ./script/bootstrap."))
    (let ((*read-eval* nil))
      (with-open-file (stream recovery-manifest
                              :direction :input
                              :external-format :utf-8)
        (let ((manifest (read stream t nil))
              (end-marker (gensym "RECOVERY-MANIFEST-END-")))
          (unless (and (listp manifest)
                       (eq (first manifest) :recovery-image)
                       (= (or (getf (rest manifest) :version) 0) 2)
                       (equal (truename (getf (rest manifest) :core))
                              (truename recovery-core))
                       (eq (read stream nil end-marker) end-marker))
            (error "Autolith's pristine recovery manifest is invalid.")))))
    (let* ((probe-output
             (uiop:run-program
              (list sbcl-command
                    "--noinform"
                    "--core" (namestring recovery-core)
                    "--end-runtime-options"
                    (namestring source-root)
                    "--probe")
              :output :string
              :error-output :output))
           (*read-eval* nil)
           (stream (make-string-input-stream probe-output))
           (end-marker (gensym "RECOVERY-PROBE-END-"))
           (probe (read stream nil end-marker)))
      (unless (and (listp probe)
                   (eq (first probe) :recovery-probe)
                   (= (or (getf (rest probe) :version) 0) 2)
                   (eq (read stream nil end-marker) end-marker))
        (error "Autolith's pristine recovery probe is invalid: ~S" probe-output)))
    (uiop:run-program
     (list sbcl-command
           "--noinform"
           "--core" (namestring recovery-core)
           "--end-runtime-options"
           (namestring source-root)
           "--list")
     :output :string
     :error-output :output)
    (let* ((temporary-root
             (uiop:ensure-directory-pathname
              (merge-pathnames
               (format nil "autolith-recovery-check-~D-~D/"
                       (get-universal-time)
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
           (temporary-data (merge-pathnames "data/" temporary-root))
           (temporary-state (merge-pathnames "state/" temporary-root))
           (temporary-cache (merge-pathnames "cache/" temporary-root))
           (temporary-home (merge-pathnames "home/" temporary-root))
           (expected-version
             (format nil
                     "autolith ~A"
                     (symbol-value
                      (find-symbol "*AUTOLITH-VERSION*" "AUTOLITH")))))
      (unwind-protect
           (progn
             (ensure-directories-exist temporary-home)
             (let ((output
                     (uiop:run-program
                      (list "env"
                            (format nil "HOME=~A" temporary-home)
                            (format nil "XDG_DATA_HOME=~A" temporary-data)
                            (format nil "XDG_STATE_HOME=~A" temporary-state)
                            (format nil "XDG_CACHE_HOME=~A" temporary-cache)
                            (format nil "AUTOLITH_PROJECT_SETUP=~A"
                                    quicklisp-setup)
                            sbcl-command
                            "--noinform"
                            "--core" (namestring recovery-core)
                            "--end-runtime-options"
                            (namestring source-root)
                            "--"
                            "--version")
                      :output :string
                      :error-output :output)))
               (unless (and (search "No compatible retained generation is available."
                                    output)
                            (search expected-version output))
                 (error "Recovery without a retained generation failed: ~A"
                        output))))
        (uiop:delete-directory-tree temporary-root
                                    :validate t
                                    :if-does-not-exist :ignore)))))
