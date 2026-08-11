(require :asdf)

(defun check--read-single-form (pathname)
  "Read and return PATHNAME's only form with reader evaluation disabled."
  (let ((*read-eval* nil))
    (with-open-file (stream pathname
                            :direction :input
                            :external-format :utf-8)
      (let ((form (read stream t nil))
            (end-marker (gensym "CHECK-END-")))
        (unless (eq (read stream nil end-marker) end-marker)
          (error "Check data at ~A contains trailing forms." pathname))
        form))))

(defun check--wait-for-process (label process output-pathname)
  "Wait for PROCESS and return its captured output, signaling on failure."
  (let ((status (uiop:wait-process process))
        (output (uiop:read-file-string output-pathname)))
    (unless (zerop status)
      (error "~A failed with status ~D:~%~A" label status output))
    output))

(defun check--stop-processes (processes)
  "Stop and reap any still-running PROCESSES."
  (dolist (process processes)
    (when (uiop:process-alive-p process)
      (uiop:terminate-process process :urgent t))
    (ignore-errors (uiop:wait-process process)))
  nil)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup (merge-pathnames "quicklisp/setup.lisp"
                                    (user-homedir-pathname)))
       (nix-development-p
         (string= (or (uiop:getenv "AUTOLITH_NIX_DEVELOPMENT") "") "1"))
       (nix-setup
         (merge-pathnames "script/nix-project-setup.lisp" source-root))
       (dependency-setup
         (if nix-development-p
             nix-setup
             (if (probe-file project-setup) project-setup user-setup))))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file dependency-setup)
    (error "Autolith needs its dependency setup at ~A" dependency-setup))
  (load dependency-setup)
  (load (merge-pathnames "script/build-sandbox.lisp" source-root))
  (if nix-development-p
      (asdf:load-system :cffi)
      (uiop:symbol-call '#:ql '#:quickload :cffi :silent t))
  (let ((profile-library-directory
          (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (asdf:load-asd (merge-pathnames "autolith.asd" source-root))
  (let* ((home (user-homedir-pathname))
         (data-home
           (uiop:ensure-directory-pathname
            (or (uiop:getenv "XDG_DATA_HOME")
                (merge-pathnames ".local/share/" home))))
         (recovery-core
           (merge-pathnames "autolith/recovery/autolith-recovery.core" data-home))
         (recovery-manifest
           (merge-pathnames "autolith/recovery/manifest.sexp" data-home))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
         (temporary-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-check-~D-~D/"
                     (get-universal-time)
                     (random most-positive-fixnum))
             (uiop:temporary-directory))))
         (temporary-data (merge-pathnames "data/" temporary-root))
         (temporary-state (merge-pathnames "state/" temporary-root))
         (temporary-cache (merge-pathnames "cache/" temporary-root))
         (temporary-home (merge-pathnames "home/" temporary-root))
         (probe-output-pathname (merge-pathnames "recovery-probe.txt" temporary-root))
         (list-output-pathname (merge-pathnames "recovery-list.txt" temporary-root))
         (fallback-output-pathname
           (merge-pathnames "recovery-fallback.txt" temporary-root))
         (processes nil))
    (unwind-protect
         (progn
           (unless (probe-file recovery-core)
             (error "Autolith's pristine recovery image is missing; run ./script/bootstrap."))
           (unless (probe-file recovery-manifest)
             (error "Autolith's pristine recovery manifest is missing; run ./script/bootstrap."))
           (let ((manifest (check--read-single-form recovery-manifest)))
             (unless (and (listp manifest)
                          (eq (first manifest) :recovery-image)
                          (= (or (getf (rest manifest) :version) 0) 2)
                          (equal (truename (getf (rest manifest) :core))
                                 (truename recovery-core)))
               (error "Autolith's pristine recovery manifest is invalid.")))
           (ensure-directories-exist temporary-home)
           ;; Recovery checks are independent of source tests. Starting them first
           ;; overlaps three core boots with the test suite instead of serializing
           ;; all four expensive phases.
           (setf processes
                 (list
                  (uiop:launch-program
                   (list sbcl-command
                         "--noinform"
                         "--core" (namestring recovery-core)
                         "--end-runtime-options"
                         (namestring source-root)
                         "--probe")
                   :output probe-output-pathname
                   :error-output :output)
                  (uiop:launch-program
                   (list sbcl-command
                         "--noinform"
                         "--core" (namestring recovery-core)
                         "--end-runtime-options"
                         (namestring source-root)
                         "--list")
                   :output list-output-pathname
                   :error-output :output)
                  (uiop:launch-program
                   (list "env"
                         (format nil "HOME=~A" temporary-home)
                         (format nil "XDG_DATA_HOME=~A" temporary-data)
                         (format nil "XDG_STATE_HOME=~A" temporary-state)
                         (format nil "XDG_CACHE_HOME=~A" temporary-cache)
                         (format nil "AUTOLITH_PROJECT_SETUP=~A"
                                 dependency-setup)
                         sbcl-command
                         "--noinform"
                         "--core" (namestring recovery-core)
                         "--end-runtime-options"
                         (namestring source-root)
                         "--"
                         "--version")
                   :output fallback-output-pathname
                   :error-output :output)))
           (asdf:test-system :autolith)
           (let* ((probe-output
                    (check--wait-for-process
                     "Recovery probe" (first processes) probe-output-pathname))
                  (probe-stream (make-string-input-stream probe-output))
                  (*read-eval* nil)
                  (end-marker (gensym "RECOVERY-PROBE-END-"))
                  (probe (read probe-stream nil end-marker)))
             (unless (and (listp probe)
                          (eq (first probe) :recovery-probe)
                          (= (or (getf (rest probe) :version) 0) 2)
                          (eq (read probe-stream nil end-marker) end-marker))
               (error "Autolith's pristine recovery probe is invalid: ~S"
                      probe-output)))
           (check--wait-for-process
            "Recovery generation listing" (second processes) list-output-pathname)
           (let* ((fallback-output
                    (check--wait-for-process
                     "Recovery fallback" (third processes) fallback-output-pathname))
                  (expected-version
                    (format nil "autolith ~A"
                            (asdf:component-version
                             (asdf:find-system :autolith)))))
             (unless (and (search "No compatible retained generation is available."
                                  fallback-output)
                          (search expected-version fallback-output))
               (error "Recovery without a retained generation failed: ~A"
                      fallback-output)))
           (setf processes nil))
      (check--stop-processes processes)
      (uiop:delete-directory-tree temporary-root
                                  :validate t
                                  :if-does-not-exist :ignore))))
