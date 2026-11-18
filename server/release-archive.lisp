(in-package #:autolith)

;;;; -- Release Archive --

(define-condition release-archive-error (error)
  ((stage
    :initarg       :stage
    :reader        release-archive-error-stage
    :type          keyword
    :documentation "The archive construction stage that failed.")
   (cause
    :initarg       :cause
    :reader        release-archive-error-cause
    :type          t
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
                    :directory    directory
                    :output       output
                    :error-output error-output))

(-> release-archive--git-output (pathname list) string)
(defun release-archive--git-output (source-root arguments)
  "Return trimmed output from a Git command below SOURCE-ROOT."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
    (release-archive--run
     (append (list "git"
                   "-c" "safe.directory=*"
                   "-C" (namestring source-root))
             arguments)
     :output       ':string
     :error-output ':output)))

(defparameter *release-archive-extra-command-directories*
  '("/usr/local/bin" "/usr/pkg/bin" "/opt/local/bin" "/bin" "/usr/bin")
  "Additional directories searched for GNU tar when PATH is incomplete.")

(-> release-archive--path-directories () list)
(defun release-archive--path-directories ()
  "Return non-empty PATH directories."
  (remove-if (lambda (name) (zerop (length name)))
             (uiop:split-string (or (uiop:getenv "PATH") "")
                                :separator '(#\:))))

(-> release-archive--command-pathname
    (string &key (:extra-directories list))
    (option pathname))
(defun release-archive--command-pathname (name &key extra-directories)
  "Return the executable pathname for NAME, or NIL when it is unavailable."
  (loop for directory-name
        in (append (release-archive--path-directories) extra-directories)
        for directory = (uiop:ensure-directory-pathname directory-name)
        for candidate = (merge-pathnames name directory)
        when (probe-file candidate)
          return candidate))

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

(-> release-archive--locked-sandbox-binary (pathname string string)
    (option pathname))
(defun release-archive--locked-sandbox-binary (source-root variable name)
  "Locate the built cl-exec-sandbox binary NAME for SOURCE-ROOT's locked dependency.

Environment VARIABLE overrides the lookup when it names an existing file."
  (let ((configured (uiop:getenv variable)))
    (or (and configured
             (plusp (length configured))
             (probe-file configured))
        (handler-case
            (when (equal (truename source-root)
                         (truename (asdf:system-source-directory :autolith)))
              (let ((candidate
                      (merge-pathnames
                       (format nil "build/~A" name)
                       (asdf:system-source-directory :cl-exec-sandbox))))
                (and (probe-file candidate) candidate)))
          (error () nil)))))

(-> release-archive--sandbox-helper (pathname) (option pathname))
(defun release-archive--sandbox-helper (source-root)
  "Locate the private Linux sandbox helper for SOURCE-ROOT's locked dependency."
  (release-archive--locked-sandbox-binary
   source-root "AUTOLITH_RELEASE_SANDBOX_HELPER" "cl-exec-sandbox-helper"))

(-> release-archive--process-group-helper (pathname) (option pathname))
(defun release-archive--process-group-helper (source-root)
  "Locate the private process-group helper for SOURCE-ROOT's locked dependency.

Every POSIX release ships it so the packaged launcher never compiles into its
read-only source tree."
  (release-archive--locked-sandbox-binary
   source-root "AUTOLITH_RELEASE_PROCESS_GROUP_HELPER"
   "cl-exec-sandbox-process-group"))

(-> release-archive--stage-executable (pathname pathname string) null)
(defun release-archive--stage-executable (source release-root relative)
  "Copy SOURCE to RELATIVE below RELEASE-ROOT and mark it executable."
  (let ((target (merge-pathnames relative release-root)))
    (release-archive--copy source target)
    (release-archive--run (list "chmod" "755" (namestring target))))
  nil)

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
   (list "cp" "-RPp" (namestring source) (namestring target)))
  nil)

(-> release-archive--validate-static-elf (pathname) null)
(defun release-archive--validate-static-elf (pathname)
  "Require PATHNAME to be an ELF executable without dynamic linkage."
  (let ((description
          (release-archive--run
           (list "file" "--brief" (namestring pathname))
           :output ':string :error-output ':output))
        (program-headers
          (release-archive--run
           (list "readelf" "-l" (namestring pathname))
           :output ':string :error-output ':output))
        (dynamic-section
          (release-archive--run
           (list "readelf" "-d" (namestring pathname))
           :output ':string :error-output ':output)))
    (unless (and (search "ELF" description)
                 (search "statically linked" description)
                 (not (search "INTERP" program-headers))
                 (not (search "(NEEDED)" dynamic-section)))
      (error 'release-archive-error
             :stage ':runtime-validation
             :cause (format nil "~A is not a static ELF executable."
                            pathname))))
  nil)

(-> release-archive--dependency-links (pathname) list)
(defun release-archive--dependency-links (dependency-root)
  "Return every symbolic link below DEPENDENCY-ROOT."
  (if (uiop:directory-exists-p dependency-root)
      (remove-if
       (lambda (value) (zerop (length value)))
       (uiop:split-string
        (release-archive--run
         (list "find" (namestring dependency-root) "-type" "l")
         :output       ':string
         :error-output ':output)
        :separator '(#\Newline #\Return)))
      nil))

(-> release-archive--link-target (pathname) (option pathname))
(defun release-archive--link-target (link)
  "Return LINK's canonical existing target, or NIL for a broken link.

TRUENAME of a dangling symbolic link returns the link's own canonical path
rather than failing, so existence needs the following stat first."
  (and (handler-case (sb-posix:stat (namestring link))
         (sb-posix:syscall-error ()
           nil))
       (handler-case (truename link)
         (file-error ()
           nil))))

(-> release-archive--materialize-dependency-links (pathname) null)
(defun release-archive--materialize-dependency-links (dependency-root)
  "Replace links below DEPENDENCY-ROOT with private copies of their targets."
  (dolist (link-name (release-archive--dependency-links dependency-root))
    (let* ((link   (pathname link-name))
           (target (release-archive--link-target link)))
      (delete-file link)
      (when target
        (release-archive--copy target link))))
  nil)

(-> release-archive--write-record
    (pathname &key (:version string) (:tag string) (:commit string)
                   (:platform string))
    null)
(defun release-archive--write-record
    (pathname &key version tag commit platform)
  "Write the strict VERSION, TAG, COMMIT, and PLATFORM record to PATHNAME."
  (with-open-file (stream pathname
                          :direction         ':output
                          :if-exists         ':supersede
                          :if-does-not-exist ':create
                          :external-format   ':utf-8)
    (format stream "version=~A~%tag=~A~%commit=~A~%platform=~A~%"
            version tag commit platform))
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
    ;; mallet:suppress no-ignore-errors -- best effort; the deletion below reports real failures
    (ignore-errors
      (release-archive--run
       (list "chmod" "-R" "u+w" (namestring temporary-root))
       :output       nil
       :error-output nil))
    (uiop:delete-directory-tree temporary-root
                                :validate          t
                                :if-does-not-exist ':ignore))
  nil)

(-> release-archive--identity-git-command (pathname list) list)
(defun release-archive--identity-git-command (source-root arguments)
  "Return an isolated Git command for the packaged SOURCE-ROOT."
  (append
   (list "env"
         "GIT_CONFIG_NOSYSTEM=1"
         "GIT_CONFIG_GLOBAL=/dev/null"
         "git"
         "-c" "safe.directory=*"
         "-C" (string-right-trim "/" (namestring source-root)))
   arguments))

(-> release-archive--create-source-identity (pathname string string) null)
(defun release-archive--create-source-identity
    (source-root tag commit-time)
  "Create a minimal deterministic Git identity for packaged SOURCE-ROOT."
  (release-archive--run
   (release-archive--identity-git-command
    source-root
    '("init" "--quiet" "--template=")))
  (release-archive--run
   (release-archive--identity-git-command
    source-root
    '("symbolic-ref" "HEAD" "refs/heads/master")))
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
             :output       ':string
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
                    "git"
                    "-c" "safe.directory=*"
                    "-C" (string-right-trim "/" (namestring source-root))
                    "commit-tree" tree
                    "-m" (format nil "Autolith ~A source" tag)))
             :output       ':string
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

(-> release-archive--x86-64-architecture-p (string) boolean)
(defun release-archive--x86-64-architecture-p (architecture)
  "Return true when ARCHITECTURE names an x86-64 machine."
  (and (find (string-downcase architecture)
             '("x86-64" "x86_64" "amd64")
             :test #'string=)
       t))

(-> release-archive--linux-libc-output->identity (string) (member :musl :glibc))
(defun release-archive--linux-libc-output->identity (output)
  "Return the Linux C library identity established by ldd OUTPUT."
  (cond
    ((search "musl" output :test #'char-equal)
     ':musl)
    ((or (search "libc.so.6" output :test #'char-equal)
         (search "ld-linux" output :test #'char-equal))
     ':glibc)
    (t
     (error 'release-archive-error
            :stage ':prerequisites
            :cause (format nil "Could not identify the Linux C library from: ~A"
                           output)))))

(-> release-archive--linux-libc () (member :musl :glibc))
(defun release-archive--linux-libc ()
  "Return the detected native Linux C library identity."
  (unless (release-archive--command-pathname "ldd")
    (error 'release-archive-error
           :stage ':prerequisites
           :cause "ldd is required to identify the Linux C library."))
  (let* ((output
           (release-archive--run
            (list "sh" "-c" "ldd /bin/sh 2>&1 || true")
            :output       ':string
            :error-output ':output))
         (detected   (release-archive--linux-libc-output->identity output))
         (configured (uiop:getenv "AUTOLITH_LIBC")))
    (when (and configured
               (plusp (length configured))
               (not (string-equal configured (symbol-name detected))))
      (error 'release-archive-error
             :stage ':prerequisites
             :cause (format nil "AUTOLITH_LIBC names ~A, but this host uses ~(~A~)."
                            configured detected)))
    detected))

(-> release-archive--platform-id
    (string string &optional (option (member :musl :glibc)))
    string)
(defun release-archive--platform-id (os architecture &optional libc)
  "Return the canonical release identifier for OS, ARCHITECTURE, and LIBC."
  (let ((architecture (string-downcase architecture))
        (musl-p       (eq libc ':musl)))
    (cond
      ((and (string-equal os "Linux")
            (release-archive--x86-64-architecture-p architecture))
       (if musl-p "x86_64-linux-musl" "x86_64-linux"))
      ((and (string-equal os "Linux")
            (member architecture '("arm64" "aarch64") :test #'string=))
       (if musl-p "aarch64-linux-musl" "aarch64-linux"))
      ((and (string-equal os "Darwin")
            (release-archive--x86-64-architecture-p architecture))
       "x86_64-darwin")
      ((and (string-equal os "Darwin")
            (member architecture '("arm64" "aarch64") :test #'string=))
       "arm64-darwin")
      ((and (string-equal os "FreeBSD")
            (release-archive--x86-64-architecture-p architecture))
       "x86_64-freebsd")
      ((and (string-equal os "NetBSD")
            (release-archive--x86-64-architecture-p architecture))
       "x86_64-netbsd")
      ((and (string-equal os "OpenBSD")
            (release-archive--x86-64-architecture-p architecture))
       "x86_64-openbsd")
      (t
       (error 'release-archive-error
              :stage  ':prerequisites
               :cause "Binary releases currently support Linux x86-64, Linux aarch64, macOS x86-64, macOS arm64, FreeBSD x86-64, NetBSD x86-64, and OpenBSD x86-64 only.")))))

(-> release-archive--platform () string)
(defun release-archive--platform ()
  "Return the validated native release platform identifier."
  (let* ((os         (software-type))
         (libc       (and (string-equal os "Linux")
                          (release-archive--linux-libc)))
         (detected   (release-archive--platform-id os (machine-type) libc))
         (configured (uiop:getenv "AUTOLITH_RELEASE_PLATFORM")))
    (when (and configured
               (plusp (length configured))
               (not (string= configured detected)))
      (error 'release-archive-error
             :stage ':prerequisites
             :cause (format nil "AUTOLITH_RELEASE_PLATFORM names ~A, but this host is ~A."
                            configured detected)))
    detected))

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

(-> release-archive--sha256-command () string)
(defun release-archive--sha256-command ()
  "Return the available SHA-256 command name."
  (cond
    ((release-archive--command-pathname "sha256sum")
     "sha256sum")
    ((release-archive--command-pathname "shasum")
     "shasum")
    ((release-archive--command-pathname "sha256")
     "sha256")
    (t
     (error 'release-archive-error
            :stage ':prerequisites
            :cause "sha256sum, shasum, or sha256 is required."))))

(-> release-archive--gnu-tar-required-p (string) boolean)
(defun release-archive--gnu-tar-required-p (os)
  "Return true when OS needs GNU tar for reproducible archives."
  (not (string-equal os "Linux")))

(-> release-archive--gnu-tar-command () (option pathname))
(defun release-archive--gnu-tar-command ()
  "Return the GNU tar executable, or NIL when it is unavailable."
  (or (release-archive--command-pathname
       "gtar"
       :extra-directories *release-archive-extra-command-directories*)
      (release-archive--command-pathname
       "gnutar"
       :extra-directories *release-archive-extra-command-directories*)))

(-> release-archive--require-gnu-tar () null)
(defun release-archive--require-gnu-tar ()
  "Require GNU tar on platforms whose system tar is not GNU."
  (when (and (release-archive--gnu-tar-required-p (software-type))
             (not (release-archive--gnu-tar-command)))
    (error 'release-archive-error
           :stage ':prerequisites
           :cause "GNU tar (gtar) is required for reproducible release archives."))
  nil)

(-> release-archive--sha256-digest (pathname) string)
(defun release-archive--sha256-digest (file)
  "Return the lowercase SHA-256 hex digest of FILE."
  (let* ((directory (uiop:pathname-directory-pathname file))
         (name      (file-namestring file))
         (command   (release-archive--sha256-command))
         (output
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            (cond
              ((string= command "sha256sum")
               (release-archive--run
                (list "sha256sum" name)
                :directory    directory
                :output       ':string
                :error-output ':output))
              ((string= command "shasum")
               (release-archive--run
                (list "shasum" "-a" "256" name)
                :directory    directory
                :output       ':string
                :error-output ':output))
              (t
               (release-archive--run
                (list "sha256" "-q" name)
                :directory    directory
                :output       ':string
                :error-output ':output)))))
         (digest    (string-downcase
                     (first (uiop:split-string output
                                               :separator '(#\Space #\Tab))))))
    (unless (and (= (length digest) 64)
                 (every (lambda (character)
                          (or (digit-char-p character)
                              (find character "abcdef")))
                        digest))
      (error 'release-archive-error
             :stage ':prerequisites
             :cause "Could not compute a SHA-256 digest."))
    digest))

(-> release-archive--checksum-file (pathname pathname) pathname)
(defun release-archive--checksum-file (file output)
  "Write the SHA-256 checksum of FILE to OUTPUT in GNU coreutils format."
  (with-open-file (stream output
                          :direction         ':output
                          :if-exists         ':supersede
                          :if-does-not-exist ':create
                          :external-format   ':utf-8)
    (format stream "~A  ~A~%"
            (release-archive--sha256-digest file)
            (file-namestring file)))
  output)

(-> release-archive--tar-command
    (pathname &key (:working-dir pathname) (:release-name string) (:commit-time string))
    list)
(defun release-archive--tar-command (tar-file &key working-dir release-name commit-time)
  "Return a deterministic GNU tar command for RELEASE-NAME below WORKING-DIR."
  (list (namestring
         (if (release-archive--gnu-tar-required-p (software-type))
             (or (release-archive--gnu-tar-command)
                 (error 'release-archive-error
                        :stage ':prerequisites
                        :cause "GNU tar (gtar) is required for reproducible release archives."))
             (or (release-archive--command-pathname "tar")
                 #p"tar")))
        "--sort=name"
        (format nil "--mtime=@~A" commit-time)
        "--owner=0" "--group=0" "--numeric-owner"
        "-cf" (namestring tar-file)
        "-C" (namestring working-dir) release-name))

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
             (platform         (release-archive--platform))
             (static-musl-p
               (not (null (search "-linux-musl" platform :from-end t))))
             (lib-extension    (release-archive--shared-library-extension))
             (fff-library-name (format nil "libfff_c.~A" lib-extension))
             (colorlisp-library-name
               (format nil "libcolorlisp-tree-sitter.~A" lib-extension))
             (runtime-version
               (release-archive--trimmed-file
                (merge-pathnames "sbcl.version" source-root)))
             (home             (user-homedir-pathname))
             (data-home
               (environment-directory
                "XDG_DATA_HOME"
                (merge-pathnames ".local/share/" home)))
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
             (static-runtime-marker
               (merge-pathnames "lib/autolith-static-musl"
                                runtime-installation))
             (fff-library
               (unless static-musl-p
                 (release-archive--environment-pathname
                  "AUTOLITH_RELEASE_FFF_LIBRARY"
                  (merge-pathnames
                   (format nil "autolith/native/fff/~A" fff-library-name)
                   data-home))))
             (colorlisp-library
               (unless static-musl-p
                 (release-archive--colorlisp-library)))
             (sandbox-helper   (release-archive--sandbox-helper source-root))
             (process-group-helper
               (release-archive--process-group-helper source-root))
             (version          (release-builder--source-version source-root))
             (tag              (format nil "v~A" version))
             (commit           (release-archive--git-output source-root '("rev-parse" "HEAD")))
             (commit-time
               (release-archive--git-output
                source-root '("show" "-s" "--format=%ct" "HEAD"))))
        (release-archive--require-commands
         '("chmod" "cp" "find" "git" "gzip" "tar"))
        (when static-musl-p
          (release-archive--require-commands '("file" "readelf")))
        (release-archive--sha256-command)
        (release-archive--require-gnu-tar)
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
        (if static-musl-p
            (unless (probe-file static-runtime-marker)
              (error 'release-archive-error
                     :stage ':prerequisites
                     :cause "The musl SBCL runtime was not statically linked."))
            (progn
              (unless (probe-file fff-library)
                (error 'release-archive-error
                       :stage ':prerequisites
                       :cause "The private fff library is absent; run ./script/bootstrap."))
              (unless (probe-file colorlisp-library)
                (error 'release-archive-error
                       :stage ':prerequisites
                       :cause "The private ColorLisp library is absent; run ./script/bootstrap."))))
        (when (string-equal (software-type) "Linux")
          (unless sandbox-helper
            (error 'release-archive-error
                   :stage ':prerequisites
                   :cause "The private sandbox helper is absent; run ./script/check.")))
        (unless process-group-helper
          (error 'release-archive-error
                 :stage ':prerequisites
                 :cause "The private process-group helper is absent; run ./script/bootstrap."))
        (when static-musl-p
          (release-archive--validate-static-elf
           (merge-pathnames "bin/sbcl" runtime-installation))
          (release-archive--validate-static-elf sandbox-helper)
          (release-archive--validate-static-elf process-group-helper))
        (unless (release-archive--semantic-version-p version)
          (error 'release-archive-error
                 :stage ':source-validation
                 :cause "ASDF does not report one semantic release version."))
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
                  (list "git" "-c" "safe.directory=*"
                        "-C" (namestring source-root)
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
                 (unless static-musl-p
                   (release-archive--copy
                    fff-library
                    (merge-pathnames (format nil "lib/~A" fff-library-name)
                                     release-root))
                   (release-archive--copy
                    colorlisp-library
                    (merge-pathnames (format nil "lib/~A" colorlisp-library-name)
                                     release-root)))
                 (when sandbox-helper
                   (release-archive--stage-executable
                    sandbox-helper release-root "libexec/cl-exec-sandbox-helper"))
                 (release-archive--stage-executable
                  process-group-helper release-root
                  "libexec/cl-exec-sandbox-process-group")
                 (release-archive--stage-executable
                  (merge-pathnames "bin/autolith-release" source-root)
                  release-root "bin/autolith")
                 (unless static-musl-p
                   (release-archive--run
                    (list "chmod" "644"
                          (namestring
                           (merge-pathnames (format nil "lib/~A" fff-library-name)
                                            release-root))
                          (namestring
                           (merge-pathnames
                            (format nil "lib/~A" colorlisp-library-name)
                            release-root)))))
                (release-archive--write-record
                 (merge-pathnames "RELEASE" release-root)
                 :version version :tag tag :commit commit :platform platform)
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
                          :output       ':string
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
                 (release-archive--tar-command temporary-tar
                                               :working-dir  temporary-root
                                               :release-name release-name
                                               :commit-time  commit-time))
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
