(in-package #:autolith)

;;;; -- Release Archive --

(define-condition release-archive-error (error)
  ((stage
    :initarg :stage
    :reader release-archive-error-stage
    :type keyword
    :documentation "The archive construction stage that failed.")
   (cause
    :initarg :cause
    :reader release-archive-error-cause
    :type t
    :documentation "The underlying failure or diagnostic value."))
  (:report
   (lambda (condition stream)
     (format stream "Release archive failed during ~(~A~): ~A"
             (release-archive-error-stage condition)
             (release-archive-error-cause condition))))
  (:documentation "A structured portable release archive failure."))

(-> release-archive--trimmed-file (pathname) string)
(defun release-archive--trimmed-file (pathname)
  "Read PATHNAME and remove surrounding ASCII whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (uiop:read-file-string pathname)))

(-> release-archive--environment-pathname (string pathname) pathname)
(defun release-archive--environment-pathname (name fallback)
  "Return environment pathname NAME or FALLBACK when NAME is absent."
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        (pathname value)
        fallback)))

(-> release-archive--run
    (list &key (:directory (option pathname)) (:output t) (:error-output t))
    t)
(defun release-archive--run
    (command &key directory (output ':interactive) (error-output ':interactive))
  "Run COMMAND for archive construction, preserving diagnostics by default."
  (uiop:run-program command
                    :directory directory
                    :output output
                    :error-output error-output))

(-> release-archive--git-output (pathname list) string)
(defun release-archive--git-output (source-root arguments)
  "Return trimmed output from a Git command below SOURCE-ROOT."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (release-archive--run
    (append (list "git" "-C" (namestring source-root)) arguments)
    :output ':string
    :error-output ':output)))

(-> release-archive--command-pathname (string) (option pathname))
(defun release-archive--command-pathname (name)
  "Return the executable pathname for NAME, or NIL when it is unavailable."
  (loop for directory-name
        in (uiop:split-string (or (uiop:getenv "PATH") "")
                              :separator '(#\:))
        when (plusp (length directory-name))
          do (let* ((directory
                      (uiop:ensure-directory-pathname directory-name))
                    (candidate (merge-pathnames name directory)))
               (when (probe-file candidate)
                 (return candidate)))))

(-> release-archive--require-commands (list) null)
(defun release-archive--require-commands (commands)
  "Require every executable named by COMMANDS."
  (dolist (command commands)
    (unless (release-archive--command-pathname command)
      (error 'release-archive-error
             :stage ':prerequisites
             :cause (format nil "~A is required." command))))
  nil)

(-> release-archive--semantic-version-p (string) boolean)
(defun release-archive--semantic-version-p (value)
  "Return true when VALUE is a three-component numeric version."
  (release-tag-valid-p (format nil "v~A" value)))

(-> release-archive--commit-p (string) boolean)
(defun release-archive--commit-p (value)
  "Return true when VALUE is a lowercase forty-character Git identity."
  (and (= (length value) 40)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find character "abcdef")))
              value)
       t))

(-> release-archive--sandbox-helper (pathname) (option pathname))
(defun release-archive--sandbox-helper (source-root)
  "Locate the private sandbox helper built below SOURCE-ROOT."
  (let ((configured (uiop:getenv "AUTOLITH_RELEASE_SANDBOX_HELPER")))
    (or (and configured
             (plusp (length configured))
             (probe-file configured))
        (let* ((software-root
                 (merge-pathnames
                  ".qlot/dists/cl-exec-sandbox/software/"
                  source-root))
               (output
                 (when (uiop:directory-exists-p software-root)
                   (release-archive--run
                    (list "find" "-L" (namestring software-root)
                          "-type" "f" "-name" "cl-exec-sandbox-helper"
                          "-print" "-quit")
                    :output ':string
                    :error-output ':output)))
               (pathname
                 (and output
                      (plusp (length (string-trim '(#\Newline #\Return) output)))
                      (pathname (string-trim '(#\Newline #\Return) output)))))
          (and pathname (probe-file pathname))))))


(-> release-archive--colorlisp-library () pathname)
(defun release-archive--colorlisp-library ()
  "Return the configured or materialized ColorLisp native library."
  (let ((configured (uiop:getenv "AUTOLITH_RELEASE_COLORLISP_LIBRARY")))
    (pathname
     (if (and configured (plusp (length configured)))
         configured
         (native-library-path)))))

(-> release-archive--copy (pathname pathname) null)
(defun release-archive--copy (source target)
  "Copy SOURCE recursively and without dereferencing it to TARGET."
  (release-archive--run
   (list "cp" "-a" "--" (namestring source) (namestring target)))
  nil)

(-> release-archive--dependency-links (pathname) list)
(defun release-archive--dependency-links (dependency-root)
  "Return every symbolic link below DEPENDENCY-ROOT."
  (remove-if
   (lambda (value) (zerop (length value)))
   (uiop:split-string
    (release-archive--run
     (list "find" (namestring dependency-root) "-type" "l" "-print0")
     :output ':string
     :error-output ':output)
    :separator (list (code-char 0)))))

(-> release-archive--link-target (pathname) (option pathname))
(defun release-archive--link-target (link)
  "Return LINK's canonical existing target, or NIL for a broken link."
  (let ((target (ignore-errors (truename link))))
    (and target (probe-file target) target)))

(-> release-archive--materialize-dependency-links (pathname) null)
(defun release-archive--materialize-dependency-links (dependency-root)
  "Replace links below DEPENDENCY-ROOT with private copies of their targets."
  (dolist (link-name (release-archive--dependency-links dependency-root))
    (let* ((link (pathname link-name))
           (target (release-archive--link-target link)))
      (delete-file link)
      (when target
        (release-archive--copy target link))))
  nil)

(-> release-archive--write-record
    (pathname &key (:version string) (:tag string) (:commit string))
    null)
(defun release-archive--write-record (pathname &key version tag commit)
  "Write the strict VERSION, TAG, and COMMIT release record to PATHNAME."
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (format stream "version=~A~%tag=~A~%commit=~A~%" version tag commit))
  nil)

(-> release-archive--make-temporary-root (pathname) pathname)
(defun release-archive--make-temporary-root (output-directory)
  "Create and return a private temporary directory below OUTPUT-DIRECTORY."
  (let ((pathname
          (merge-pathnames
           (format nil ".autolith-release.~A/" (make-identifier))
           output-directory)))
    (ensure-directories-exist (merge-pathnames ".keep" pathname))
    pathname))

(-> release-archive--cleanup (pathname) null)
(defun release-archive--cleanup (temporary-root)
  "Remove TEMPORARY-ROOT even when archive inputs were made read-only."
  (when (uiop:directory-exists-p temporary-root)
    (ignore-errors
      (release-archive--run
       (list "chmod" "-R" "u+w" (namestring temporary-root))
       :output nil
       :error-output nil))
    (uiop:delete-directory-tree temporary-root
                                :validate t
                                :if-does-not-exist ':ignore))
  nil)

(-> release-archive--identity-git-command (pathname list) list)
(defun release-archive--identity-git-command (source-root arguments)
  "Return an isolated Git command for the packaged SOURCE-ROOT."
  (append
   (list "env"
         "GIT_CONFIG_NOSYSTEM=1"
         "GIT_CONFIG_GLOBAL=/dev/null"
         "git" "-C" (namestring source-root))
   arguments))

(-> release-archive--create-source-identity (pathname string string) null)
(defun release-archive--create-source-identity
    (source-root tag commit-time)
  "Create a minimal deterministic Git identity for packaged SOURCE-ROOT."
  (release-archive--run
   (release-archive--identity-git-command
    source-root
    '("init" "--quiet" "--initial-branch=master" "--template=")))
  (dolist (setting
           '(("user.name" "Autolith release build")
             ("user.email" "release-build@localhost")
             ("gc.auto" "0")
             ("maintenance.auto" "false")
             ("core.logAllRefUpdates" "false")
             ("core.autocrlf" "false")
             ("core.fileMode" "true")))
    (release-archive--run
     (release-archive--identity-git-command
      source-root
      (list "config" (first setting) (second setting)))))
  (release-archive--run
   (release-archive--identity-git-command
    source-root '("add" "--force" "--all")))
  (let* ((tree
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (release-archive--run
             (release-archive--identity-git-command source-root '("write-tree"))
             :output ':string
             :error-output ':output)))
         (commit
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (release-archive--run
             (append
              (list "env"
                    "GIT_CONFIG_NOSYSTEM=1"
                    "GIT_CONFIG_GLOBAL=/dev/null"
                    "LC_ALL=C"
                    "TZ=UTC"
                    (format nil "GIT_AUTHOR_DATE=@~A +0000" commit-time)
                    (format nil "GIT_COMMITTER_DATE=@~A +0000" commit-time)
                    "git" "-C" (namestring source-root)
                    "commit-tree" tree
                    "-m" (format nil "Autolith ~A source" tag)))
             :output ':string
             :error-output ':output))))
    (unless (release-archive--commit-p commit)
      (error 'release-archive-error
             :stage ':source-identity
             :cause "Git did not create a valid packaged source commit."))
    (release-archive--run
     (release-archive--identity-git-command
      source-root
      (list "update-ref" "refs/heads/master" commit))))
  (delete-file (merge-pathnames ".git/index" source-root))
  (release-archive--run
   (release-archive--identity-git-command source-root '("read-tree" "HEAD")))
  nil)

(-> release-archive--platform () string)
(defun release-archive--platform ()
  "Return the canonical release platform identifier."
  (let ((os (software-type))
        (arch (string-downcase (machine-type))))
    (cond
      ((and (string-equal os "Linux")
            (member arch '("x86-64" "x86_64" "amd64") :test #'string=))
       "x86_64-linux")
      ((and (string-equal os "Darwin")
            (member arch '("arm64" "aarch64") :test #'string=))
       "arm64-darwin")
      (t
       (error 'release-archive-error
              :stage ':prerequisites
              :cause "Binary releases currently support Linux x86-64 and macOS arm64 only.")))))

(-> release-archive--validate-platform () null)
(defun release-archive--validate-platform ()
  "Require a supported release target platform."
  (release-archive--platform)
  nil)

(-> release-archive--shared-library-extension () string)
(defun release-archive--shared-library-extension ()
  "Return the dynamic library extension for the current platform."
  (if (string-equal (software-type) "Darwin")
      "dylib"
      "so"))

(-> release-archive--checksum-file (pathname pathname) pathname)
(defun release-archive--checksum-file (file output)
  "Write the SHA-256 checksum of FILE to OUTPUT."
  (if (release-archive--command-pathname "sha256sum")
      (release-archive--run
       (list "sha256sum" (file-namestring file))
       :directory (uiop:pathname-directory-pathname file)
       :output output
       :error-output ':output)
      (let* ((output-string
               (release-archive--run
                (list "shasum" "-a" "256" (file-namestring file))
                :directory (uiop:pathname-directory-pathname file)
                :output ':string
                :error-output ':output)))
        (with-open-file (stream output
                                :direction ':output
                                :if-exists ':supersede
                                :if-does-not-exist ':create
                                :external-format ':utf-8)
          (write-string output-string stream))))
  output)

(-> release-archive--tar-command (pathname pathname string string) list)
(defun release-archive--tar-command (tar-file working-dir release-name commit-time)
  "Return a tar creation command for TAR-FILE containing RELEASE-NAME below WORKING-DIR."
  (if (release-archive--command-pathname "gtar")
      (list "gtar" "--sort=name"
            (format nil "--mtime=@~A" commit-time)
            "--owner=0" "--group=0" "--numeric-owner"
            "-cf" (namestring tar-file)
            "-C" (namestring working-dir) release-name)
      (if (string-equal (software-type) "Linux")
          (list "tar" "--sort=name"
                (format nil "--mtime=@~A" commit-time)
                "--owner=0" "--group=0" "--numeric-owner"
                "-cf" (namestring tar-file)
                "-C" (namestring working-dir) release-name)
          (list "tar" "-cf" (namestring tar-file)
                "-C" (namestring working-dir) release-name))))

(-> release-archive-build
    (&key (:source-root pathname) (:output-directory pathname))
    (values pathname pathname))
(defun release-archive-build (&key source-root output-directory)
  "Build a deterministic release archive and checksum from SOURCE-ROOT.

Return the published archive and checksum pathnames. Environment overrides name
the managed runtime, matching SBCL source, native libraries, and sandbox helper."
  (handler-case
      (let* ((source-root
               (uiop:ensure-directory-pathname
                (or source-root (asdf:system-source-directory :autolith))))
             (output-directory
               (uiop:ensure-directory-pathname
                (or output-directory (merge-pathnames "dist/" source-root))))
             (platform (release-archive--platform))
             (lib-extension (release-archive--shared-library-extension))
             (fff-library-name (format nil "libfff_c.~A" lib-extension))
             (colorlisp-library-name
               (format nil "libcolorlisp-tree-sitter.~A" lib-extension))
             (runtime-version
               (release-archive--trimmed-file
                (merge-pathnames "sbcl.version" source-root)))
             (home (user-homedir-pathname))
             (data-home
               (uiop:ensure-directory-pathname
                (or (uiop:getenv "XDG_DATA_HOME")
                    (merge-pathnames ".local/share/" home))))
             (runtime-root
               (merge-pathnames
                (format nil "autolith/runtimes/~A/" runtime-version)
                data-home))
             (runtime-installation
               (release-archive--environment-pathname
                "AUTOLITH_RELEASE_RUNTIME"
                (merge-pathnames "installation/" runtime-root)))
             (runtime-source
               (release-archive--environment-pathname
                "AUTOLITH_RELEASE_SBCL_SOURCE"
                (merge-pathnames "source/" runtime-root)))
             (fff-library
               (release-archive--environment-pathname
                "AUTOLITH_RELEASE_FFF_LIBRARY"
                (merge-pathnames (format nil "autolith/native/fff/~A" fff-library-name)
                                 data-home)))
             (colorlisp-library (release-archive--colorlisp-library))
             (sandbox-helper (release-archive--sandbox-helper source-root))
             (version (release-builder--source-version source-root))
             (tag (format nil "v~A" version))
             (commit (release-archive--git-output source-root '("rev-parse" "HEAD")))
             (commit-time
               (release-archive--git-output
                source-root '("show" "-s" "--format=%ct" "HEAD"))))
        (release-archive--require-commands
         '("chmod" "cp" "find" "git" "gzip" "readlink" "tar"))
        (unless (or (release-archive--command-pathname "sha256sum")
                    (release-archive--command-pathname "shasum"))
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "sha256sum or shasum is required."))
        (release-archive--validate-platform)
        (unless (release-archive--semantic-version-p runtime-version)
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "sbcl.version is malformed."))
        (unless (probe-file (merge-pathnames ".qlot/setup.lisp" source-root))
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "Locked dependencies are absent; run ./script/bootstrap."))
        (unless (uiop:file-exists-p
                 (merge-pathnames "bin/sbcl" runtime-installation))
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "The managed SBCL runtime is absent; run ./script/bootstrap."))
        (unless (probe-file (merge-pathnames "version.lisp-expr" runtime-source))
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "The managed SBCL source is absent; run ./script/bootstrap."))
        (unless (probe-file fff-library)
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "The private fff library is absent; run ./script/bootstrap."))
        (unless (probe-file colorlisp-library)
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "The private ColorLisp library is absent; run ./script/bootstrap."))
        (when (string-equal (software-type) "Linux")
          (unless sandbox-helper
            (error 'release-archive-error
                   :stage ':prerequisites
                   :cause "The private sandbox helper is absent; run ./script/check.")))
        (unless (release-archive--semantic-version-p version)
          (error 'release-archive-error
                 :stage ':source-validation
                 :cause "autolith.asd does not declare one semantic version."))
        (unless (release-archive--commit-p commit)
          (error 'release-archive-error
                 :stage ':source-validation
                 :cause "The source commit is malformed."))
        (unless (and (plusp (length commit-time))
                     (every #'digit-char-p commit-time))
          (error 'release-archive-error
                 :stage ':source-validation
                 :cause "The source commit time is malformed."))
        (uiop:ensure-all-directories-exist (list output-directory))
        (let* ((output-directory
                 (uiop:ensure-directory-pathname (truename output-directory)))
               (temporary-root
                 (release-archive--make-temporary-root output-directory)))
          (unwind-protect
               (let* ((release-name
                        (format nil "autolith-~A-~A" tag platform))
                      (release-root
                        (merge-pathnames (format nil "~A/" release-name)
                                         temporary-root))
                      (packaged-source
                        (merge-pathnames "libexec/autolith/" release-root))
                      (archive
                        (merge-pathnames (format nil "~A.tar.gz" release-name)
                                         output-directory))
                      (checksum
                        (merge-pathnames
                         (format nil "~A.tar.gz.sha256" release-name)
                         output-directory))
                      (temporary-tar
                        (merge-pathnames (format nil "~A.tar" release-name)
                                         temporary-root))
                      (temporary-archive
                        (merge-pathnames (format nil "~A.tar.gz" release-name)
                                         temporary-root))
                      (temporary-checksum
                        (merge-pathnames
                         (format nil "~A.tar.gz.sha256" release-name)
                         temporary-root))
                      (tracked-source
                        (merge-pathnames "tracked-source.tar" temporary-root)))
                 (uiop:ensure-all-directories-exist
                  (list packaged-source
                        (merge-pathnames "bin/" release-root)
                        (merge-pathnames "lib/" release-root)))
                 (format t "~&Collecting tracked source at ~A.~%" commit)
                 (finish-output)
                 (release-archive--run
                  (list "git" "-C" (namestring source-root)
                        "archive" "--format=tar"
                        "--output" (namestring tracked-source) "HEAD"))
                 (release-archive--run
                  (list "tar" "-xf" (namestring tracked-source)
                        "-C" (namestring packaged-source)))
                 (release-archive--copy
                  (merge-pathnames ".qlot" source-root)
                  (merge-pathnames ".qlot" packaged-source))
                 (release-archive--materialize-dependency-links
                  (merge-pathnames ".qlot/" packaged-source))
                 (format t "~&Collecting the pinned SBCL runtime and source.~%")
                 (finish-output)
                 (release-archive--copy
                  runtime-installation (merge-pathnames "runtime" release-root))
                 (release-archive--copy
                  runtime-source
                  (merge-pathnames "libexec/sbcl-source" release-root))
                 (release-archive--copy
                  fff-library
                  (merge-pathnames (format nil "lib/~A" fff-library-name) release-root))
                 (release-archive--copy
                  colorlisp-library
                  (merge-pathnames (format nil "lib/~A" colorlisp-library-name)
                                   release-root))
                 (when sandbox-helper
                   (release-archive--copy
                    sandbox-helper
                    (merge-pathnames "libexec/cl-exec-sandbox-helper" release-root))
                   (release-archive--run
                    (list "chmod" "755"
                          (namestring
                           (merge-pathnames "libexec/cl-exec-sandbox-helper"
                                            release-root)))))
                 (release-archive--copy
                  (merge-pathnames "bin/autolith-release" source-root)
                  (merge-pathnames "bin/autolith" release-root))
                 (release-archive--run
                  (list "chmod" "755"
                        (namestring (merge-pathnames "bin/autolith" release-root))))
                 (release-archive--run
                  (list "chmod" "644"
                        (namestring
                         (merge-pathnames (format nil "lib/~A" fff-library-name)
                                          release-root))
                        (namestring
                         (merge-pathnames (format nil "lib/~A" colorlisp-library-name)
                                          release-root))))
                 (release-archive--write-record
                  (merge-pathnames "RELEASE" release-root)
                  :version version :tag tag :commit commit)
                 (format t "~&Creating the internal source identity.~%")
                 (finish-output)
                 (release-archive--create-source-identity
                  packaged-source tag commit-time)
                 (let ((actual-runtime-version
                         (string-trim
                          '(#\Space #\Tab #\Newline #\Return)
                          (release-archive--run
                           (list "env" "-u" "SBCL_HOME"
                                 (namestring
                                  (merge-pathnames "runtime/bin/sbcl"
                                                   release-root))
                                 "--noinform" "--no-userinit" "--no-sysinit"
                                 "--non-interactive" "--eval"
                                 "(write-string (lisp-implementation-version))")
                           :output ':string
                           :error-output ':output))))
                   (unless (string= actual-runtime-version runtime-version)
                     (error 'release-archive-error
                            :stage ':runtime-validation
                            :cause
                            (format nil "The copied SBCL runtime reports ~A."
                                    actual-runtime-version))))
                 (release-archive--run
                  (list "find" (namestring release-root)
                        "-type" "d" "-exec" "chmod" "a-w" "{}" "+"))
                 (release-archive--run
                  (list "find" (namestring release-root)
                        "-type" "f" "-exec" "chmod" "a-w" "{}" "+"))
                 (format t "~&Writing ~A.~%" archive)
                 (finish-output)
                 (release-archive--run
                  (release-archive--tar-command
                   temporary-tar temporary-root release-name commit-time))
                 (release-archive--run
                  (list "gzip" "-9n" (namestring temporary-tar)))
                 (release-archive--checksum-file temporary-archive temporary-checksum)
                 (uiop:rename-file-overwriting-target temporary-archive archive)
                 (uiop:rename-file-overwriting-target temporary-checksum checksum)
                 (let ((checksum-value
                         (first
                          (uiop:split-string
                           (release-archive--trimmed-file checksum)
                           :separator '(#\Space #\Tab)))))
                   (format t "~&Built ~A~%SHA-256: ~A~%" archive checksum-value))
                 (finish-output)
                 (values archive checksum))
            (release-archive--cleanup temporary-root))))
    (release-archive-error (condition)
      (error condition))
    (error (cause)
      (error 'release-archive-error :stage ':construction :cause cause))))
