(require :asdf)

(load (merge-pathnames "runtime-requirement.lisp"
                       (uiop:pathname-directory-pathname *load-truename*)))

(let* ((arguments (uiop:command-line-arguments))
       (source-root
         (and (first arguments)
              (uiop:ensure-directory-pathname (first arguments))))
       (installation
         (and (second arguments)
              (uiop:ensure-directory-pathname (second arguments))))
       (temporary-root
         (and (third arguments)
              (uiop:ensure-directory-pathname (third arguments))))
       (bootstrap-installation
         (and (fourth arguments)
              (uiop:ensure-directory-pathname (fourth arguments)))))
  (labels ((fail (control &rest values)
             "Signal a release runtime build failure using CONTROL and VALUES."
             (error "Autolith release runtime build failed: ~?" control values))

           (trimmed-file (pathname)
             "Read PATHNAME and remove surrounding ASCII whitespace."
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (uiop:read-file-string pathname)))

           (sha256-p (value)
             "Return true when VALUE is a lowercase SHA-256 identity."
             (and (= (length value) 64)
                  (every (lambda (character)
                           (or (digit-char-p character)
                               (find character "abcdef")))
                         value)
                  t))

           (run (command &key directory (input ':interactive)
                                  (output ':interactive)
                                  (error-output ':interactive))
             "Run one runtime build COMMAND with visible diagnostics."
             (uiop:run-program command
                               :directory directory
                               :input input
                               :output output
                               :error-output error-output))

           (command-available-p (name)
             "Return true when executable NAME is available in PATH."
             (loop for directory-name
                   in (uiop:split-string (or (uiop:getenv "PATH") "")
                                         :separator '(#\:))
                   thereis (and (plusp (length directory-name))
                                (probe-file (merge-pathnames
                                             name
                                             (uiop:ensure-directory-pathname
                                              directory-name))))))

           (check-archive (archive expected-sha256)
             "Require ARCHIVE to match EXPECTED-SHA256."
             (let ((actual
                     (string-trim
                      '(#\Space #\Tab #\Newline #\Return)
                      (cond
                        ((command-available-p "sha256sum")
                         (first
                          (uiop:split-string
                           (run (list "sha256sum" (namestring archive))
                                :output ':string
                                :error-output ':output
                                :directory temporary-root)
                           :separator '(#\Space #\Tab))))
                        ((command-available-p "shasum")
                         (first
                          (uiop:split-string
                           (run (list "shasum" "-a" "256"
                                      (namestring archive))
                                :output ':string
                                :error-output ':output
                                :directory temporary-root)
                           :separator '(#\Space #\Tab))))
                        ((command-available-p "sha256")
                         (run (list "sha256" "-q" (file-namestring archive))
                              :output ':string
                              :error-output ':output
                              :directory temporary-root))
                        (t
                         (fail "sha256sum, shasum, or sha256 is required."))))))
               (unless (string-equal actual expected-sha256)
                 (fail "~A does not match the expected SHA-256."
                       (file-namestring archive)))))

             (runtime-version (command)
               "Return the implementation version reported by SBCL COMMAND."
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (run (list "env" "-u" "SBCL_HOME" (namestring command)
                           "--noinform" "--no-userinit" "--no-sysinit"
                           "--non-interactive" "--eval"
                           "(write-string (lisp-implementation-version))")
                     :output ':string
                     :error-output ':output)))

             (runtime-thread-support-p (command)
               "Return true when SBCL COMMAND was built with :SB-THREAD."
               (string-equal
                (string-trim
                 '(#\Space #\Tab #\Newline #\Return)
                 (run (list "env" "-u" "SBCL_HOME" (namestring command)
                            "--noinform" "--no-userinit" "--no-sysinit"
                            "--non-interactive" "--eval"
                            "(write (and (find :sb-thread *features*) t))")
                      :output ':string
                      :error-output ':output))
                "T")))
    (handler-case
        (progn
          (unless (and (= (length arguments) 4)
                       source-root installation temporary-root
                       bootstrap-installation)
            (fail "usage: build-release-runtime.lisp SOURCE INSTALLATION TEMP BOOTSTRAP"))
          (unless (or (and (string-equal (software-type) "Linux")
                           (member (string-downcase (machine-type))
                                   '("x86-64" "x86_64" "amd64"
                                     "aarch64" "arm64")
                                   :test #'string=))
                      (and (string-equal (software-type) "Darwin")
                             (member (string-downcase (machine-type))
                                     '("x86-64" "x86_64" "amd64"
                                       "arm64" "aarch64")
                                     :test #'string=))
                      (and (member (software-type)
                                   '("FreeBSD" "NetBSD" "OpenBSD")
                                   :test #'string-equal)
                           (member (string-downcase (machine-type))
                                   '("x86-64" "x86_64" "amd64")
                                   :test #'string=)))
             (fail "release runtimes currently support Linux x86-64, Linux aarch64, macOS x86-64, macOS arm64, FreeBSD x86-64, NetBSD x86-64, and OpenBSD x86-64 only."))
          (let* ((runtime-version
                   (trimmed-file (merge-pathnames "sbcl.version" source-root)))
                 (runtime-sha256
                   (trimmed-file
                    (merge-pathnames "sbcl-source.sha256" source-root)))
                 (bootstrap-command
                   (merge-pathnames "bin/sbcl" bootstrap-installation))
                 (runtime-archive
                   (merge-pathnames
                    (format nil "sbcl-~A-source.tar.bz2" runtime-version)
                    temporary-root))
                 (runtime-source
                   (merge-pathnames (format nil "sbcl-~A/" runtime-version)
                                    temporary-root)))
            (unless (autolith-version-components runtime-version)
              (fail "sbcl.version is malformed."))
            (unless (sha256-p runtime-sha256)
              (fail "sbcl-source.sha256 is malformed."))
            (let* ((bootstrap-version (runtime-version bootstrap-command))
                   (host-bootstrap-p
                     (equal (uiop:getenv "AUTOLITH_HOST_BOOTSTRAP") "1"))
                   (host-minimum
                     (or (uiop:getenv "AUTOLITH_HOST_BOOTSTRAP_MINIMUM")
                         "2.0.0")))
              (if host-bootstrap-p
                  (unless (autolith-version-at-least-p bootstrap-version host-minimum)
                    (fail "the host bootstrap compiler ~A does not satisfy SBCL ~A or newer."
                          bootstrap-version host-minimum))
                  (unless (string= bootstrap-version "2.4.0")
                    (fail "the bootstrap compiler does not report version 2.4.0."))))
            (format t "~&Building the pinned SBCL ~A release runtime.~%"
                    runtime-version)
            (finish-output)
            (run
             (list "curl" "--fail" "--location" "--show-error" "--retry" "3"
                   "--proto" "=https" "--tlsv1.2"
                   "--output" (namestring runtime-archive)
                   (format nil
                           "https://downloads.sourceforge.net/project/sbcl/sbcl/~A/sbcl-~A-source.tar.bz2"
                           runtime-version runtime-version)))
            (check-archive runtime-archive runtime-sha256)
            (run (list "tar" "-xjf" (namestring runtime-archive)
                       "-C" (namestring temporary-root)))
            (when (string-equal (software-type) "OpenBSD")
              ;; Match lang/sbcl's USE_NOBTCFI: indirect branches into generated
              ;; Lisp code otherwise raise SIGILL (ILL_BTCFI) on CET-capable CPUs.
              ;; Building also requires a filesystem mounted with wxallowed.
              (with-open-file
                  (stream (merge-pathnames "src/runtime/Config.generic-openbsd"
                                           runtime-source)
                          :direction ':output
                          :if-exists ':append
                          :if-does-not-exist ':error)
                (format stream "~%LINKFLAGS += -Wl,-z,nobtcfi~%")))
            (run
             (list "sh" "make.sh"
                   "--with-sb-thread"
                   (format nil "--prefix=~A" (namestring installation))
                   (format nil "--xc-host=~A --no-userinit --no-sysinit"
                           (namestring bootstrap-command)))
             :directory runtime-source)
            (run
             (list "env" "-u" "SBCL_HOME" "sh" "install.sh"
                   (format nil "--prefix=~A" (namestring installation)))
             :directory runtime-source)
            (when (equal (uiop:getenv "AUTOLITH_STATIC_MUSL") "1")
              (run (list "make" "-C" "src/runtime" "libsbcl.a")
                   :directory runtime-source)
              (let* ((support-root
                       (merge-pathnames
                        "static-build/"
                        (uiop:pathname-parent-directory-pathname installation)))
                     (runtime-archive
                       (merge-pathnames "src/runtime/libsbcl.a"
                                        runtime-source))
                     (installed-archive
                       (merge-pathnames "libsbcl.a" support-root))
                     (builder-installation
                       (merge-pathnames "installation/" support-root))
                     (builder-runtime
                       (merge-pathnames "bin/sbcl" builder-installation)))
                (unless (probe-file runtime-archive)
                  (fail "the static runtime archive was not built."))
                (uiop:ensure-all-directories-exist
                 (list support-root builder-installation))
                (uiop:copy-file runtime-archive installed-archive)
                (run (list "cp" "-RPp"
                           (format nil "~A." (namestring installation))
                           (namestring builder-installation)))
                (unless (probe-file builder-runtime)
                  (fail "the dynamic builder runtime was not preserved."))))
            (let* ((installed (merge-pathnames "bin/sbcl" installation))
                   (actual (runtime-version installed)))
              (unless (string= actual runtime-version)
                (fail "the release runtime reports ~A." actual))
              (unless (runtime-thread-support-p installed)
                (fail "the release runtime was built without threads.")))))
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)
        (uiop:quit 1)))))
