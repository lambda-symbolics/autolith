(require :asdf)

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

           (semantic-version-p (value)
             "Return true when VALUE has three numeric components."
             (let ((components
                     (uiop:split-string value :separator '(#\.))))
               (and (= (length components) 3)
                    (every (lambda (component)
                             (and (plusp (length component))
                                  (every #'digit-char-p component)))
                           components)
                    t)))

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
             (if (command-available-p "sha256sum")
                 (run (list "sha256sum" "--check" "--status" "-")
                      :output nil
                      :error-output ':output
                      :directory temporary-root
                      :input
                      (make-string-input-stream
                       (format nil "~A  ~A~%"
                               expected-sha256
                               (file-namestring archive))))
                 (run (list "shasum" "-a" "256" "--check" "--status" "-")
                      :output nil
                      :error-output ':output
                      :directory temporary-root
                      :input
                      (make-string-input-stream
                       (format nil "~A  ~A~%"
                               expected-sha256
                               (file-namestring archive))))))

           (runtime-version (command)
             "Return the implementation version reported by SBCL COMMAND."
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              (run (list "env" "-u" "SBCL_HOME" (namestring command)
                         "--noinform" "--no-userinit" "--no-sysinit"
                         "--non-interactive" "--eval"
                         "(write-string (lisp-implementation-version))")
                   :output ':string
                   :error-output ':output))))
    (handler-case
        (progn
          (unless (and (= (length arguments) 4)
                       source-root installation temporary-root
                       bootstrap-installation)
            (fail "usage: build-release-runtime.lisp SOURCE INSTALLATION TEMP BOOTSTRAP"))
          (unless (or (and (string-equal (software-type) "Linux")
                           (member (string-downcase (machine-type))
                                   '("x86-64" "x86_64" "amd64")
                                   :test #'string=))
                      (and (string-equal (software-type) "Darwin")
                           (member (string-downcase (machine-type))
                                   '("arm64" "aarch64")
                                   :test #'string=)))
            (fail "release runtimes currently support Linux x86-64 and macOS arm64 only."))
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
            (unless (semantic-version-p runtime-version)
              (fail "sbcl.version is malformed."))
            (unless (sha256-p runtime-sha256)
              (fail "sbcl-source.sha256 is malformed."))
            (unless (string= (runtime-version bootstrap-command) "2.4.0")
              (fail "the bootstrap compiler does not report version 2.4.0."))
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
            (run
             (list "sh" "make.sh"
                   (format nil "--prefix=~A" (namestring installation))
                   (format nil "--xc-host=~A --no-userinit --no-sysinit"
                           (namestring bootstrap-command)))
             :directory runtime-source)
            (run
             (list "env" "-u" "SBCL_HOME" "sh" "install.sh"
                   (format nil "--prefix=~A" (namestring installation)))
             :directory runtime-source)
            (let ((actual
                    (runtime-version
                     (merge-pathnames "bin/sbcl" installation))))
              (unless (string= actual runtime-version)
                (fail "the release runtime reports ~A." actual)))))
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)
        (uiop:quit 1)))))
