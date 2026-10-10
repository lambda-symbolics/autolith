(in-package #:autolith)

;;;; -- Release Script Tests --

(defparameter *release-script-tests-version* "0.11.0"
  "The semantic fixture version used by release boundary tests.")

(defparameter *release-script-tests-commit*
  "0123456789abcdef0123456789abcdef01234567"
  "The valid Git identity used by release boundary tests.")

(-> release-script-tests--write-file (pathname string) pathname)
(defun release-script-tests--write-file (pathname content)
  "Write CONTENT to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string content stream))
  pathname)

(-> test-build-sandbox-packaged-helpers () null)
(defun test-build-sandbox-packaged-helpers ()
  "Build POSIX helpers only when a platform-required packaged helper is missing."
  (let* ((script (asdf:system-relative-pathname :autolith "script/build-sandbox.lisp"))
         (existing (namestring script))
         (missing (namestring (merge-pathnames "missing-sandbox-helper" script))))
    (dolist (platform '(:linux :darwin :freebsd :netbsd :openbsd))
      (dolist (case '((:existing :existing nil)
                      (:existing nil t)
                      (nil :existing t)
                      (:missing :existing t)
                      (:empty :existing t)))
        (destructuring-bind (process-group helper linux-build-p) case
          (flet ((path (value)
                   (case value
                     (:existing
                      existing)
                     (:missing
                      missing)
                     (:empty
                      ""))))
            (let ((*features* (cons platform
                                   (set-difference *features*
                                                   '(:linux :darwin :freebsd
                                                     :netbsd :openbsd :win32))))
                  (built-p nil))
              (with-test-environment
                  (("CL_EXEC_SANDBOX_PROCESS_GROUP_HELPER" (path process-group))
                   ("CL_EXEC_SANDBOX_HELPER" (path helper)))
                (test-call-with-function-replacements
                 (list (list 'uiop:run-program
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (setf built-p t))))
                 (lambda () (load script))))
              (test-assert
               (eq built-p (if (eq platform ':linux)
                               linux-build-p
                               (not (eq process-group ':existing))))
               "Packaged helper detection respects platform requirements"))))))))

(-> test-release-runtime-openbsd-linker () null)
(defun test-release-runtime-openbsd-linker ()
  "Evaluate the runtime linker flags at the OpenBSD release build boundary."
  (with-test-configuration (configuration)
    (let* ((root (test-configuration-root configuration))
           (source (merge-pathnames "source/" root))
           (temporary (merge-pathnames "temporary/" root))
           (installation (merge-pathnames "installation/" root))
           (bootstrap (merge-pathnames "bootstrap/" root))
           (runtime-source (merge-pathnames "sbcl-2.6.6/" temporary))
           (runtime-config
             (merge-pathnames "src/runtime/Config.generic-openbsd" runtime-source))
           (script (asdf:system-relative-pathname
                    :autolith "script/build-release-runtime.lisp"))
           (digest (make-string 64 :initial-element #\0))
           (run-program (symbol-function 'uiop:run-program))
           (make (or (release-archive--command-pathname "gmake")
                     (release-archive--command-pathname "make"))))
      (release-script-tests--write-file
       (merge-pathnames "sbcl.version" source) "2.6.6")
      (release-script-tests--write-file
       (merge-pathnames "sbcl-source.sha256" source) digest)
      (dolist (platform '("OpenBSD" "FreeBSD"))
        (release-script-tests--write-file
         runtime-config (format nil "LINKFLAGS = -Wl,-z,wxneeded~%"))
        (let ((build-flags nil)
              (installed-p nil))
          (sb-ext:without-package-locks
            (test-call-with-function-replacements
             (list
              (list 'software-type (lambda () platform))
              (list 'machine-type (lambda () "x86-64"))
              (list 'uiop:command-line-arguments
                    (lambda ()
                      (mapcar #'namestring
                              (list source installation temporary bootstrap))))
              (list 'uiop:run-program
                    (lambda (command &rest options &key &allow-other-keys)
                      (declare (ignore options))
                      (cond
                        ((member (first command) '("sha256sum" "shasum" "sha256")
                                 :test #'string=)
                         digest)
                        ((equal (subseq command 0 (min 2 (length command)))
                                '("sh" "make.sh"))
                         (setf build-flags
                               (uiop:split-string
                                (funcall run-program
                                         (list (namestring make) "-s"
                                               "-f" (namestring runtime-config) "-f" "-")
                                         :input (make-string-input-stream
                                                 (format nil "flags:~%~C@printf '%s\\n' '$(LINKFLAGS)'~%"
                                                         #\Tab))
                                         :output ':string)
                                :separator '(#\Space #\Newline))))
                        ((member "install.sh" command :test #'string=)
                         (setf installed-p t))
                        ((member "(write-string (lisp-implementation-version))"
                                 command :test #'string=)
                         (if installed-p "2.6.6" "2.4.0"))
                        ((member "(write (and (find :sb-thread *features*) t))"
                                 command :test #'string=)
                         "T"))))
              (list 'uiop:quit
                    (lambda (status &rest options)
                      (declare (ignore options))
                      (error "Runtime builder exited with status ~D." status))))
             (lambda ()
               (let ((*package* (find-package '#:cl-user)))
                 (with-test-environment (("AUTOLITH_HOST_BOOTSTRAP" nil)
                                         ("AUTOLITH_STATIC_MUSL" nil))
                   (load script))))))
          (test-assert installed-p "The release runtime reaches installation")
          (test-assert (member "-Wl,-z,wxneeded" build-flags :test #'string=)
                       "Runtime configuration retains the W^X linker setting")
          (test-assert
           (eq (not (null (member "-Wl,-z,nobtcfi" build-flags :test #'string=)))
               (string= platform "OpenBSD"))
           "Only OpenBSD release builds opt generated Lisp code out of IBT")))))
  nil)

(-> release-script-tests--run
    (list &key (:directory (option pathname)) (:environment list)
               (:ignore-error-status boolean) (:output t))
    t)
(defun release-script-tests--run
    (command &key directory environment ignore-error-status (output ':string))
  "Run COMMAND with optional ENVIRONMENT assignments and return its output.

Expected failures capture their diagnostics separately, so a tolerant caller
can assert on the exact failure message through the second return value."
  (uiop:run-program
   (if environment
       (append (list "env") environment command)
       command)
   :directory directory
   :ignore-error-status ignore-error-status
   :output output
   :error-output (if ignore-error-status ':string ':output)))

(-> release-script-tests--pty-command (string string) list)
(defun release-script-tests--pty-command (command answer)
  "Return a platform PTY command running shell COMMAND and submitting ANSWER."
  (if (member :darwin *features*)
      (list "env"
            (format nil "AUTOLITH_TEST_PTY_COMMAND=~A" command)
            (format nil "AUTOLITH_TEST_PTY_ANSWER=~A" answer)
            "/usr/bin/expect"
            "-c"
            "set timeout 30; spawn -noecho /bin/sh -c \"$env(AUTOLITH_TEST_PTY_COMMAND)\"; expect -exact {[Y/n]}; send -- \"$env(AUTOLITH_TEST_PTY_ANSWER)\\r\"; expect eof; catch wait result; exit [lindex $result 3]")
      (list "env" "SHELL=/bin/sh" "script" "-q" "-e" "-c" command "/dev/null")))

(-> release-script-tests--chmod (string pathname) null)
(defun release-script-tests--chmod (mode pathname)
  "Apply numeric or symbolic MODE to PATHNAME."
  (release-script-tests--run
   (list "chmod" mode (namestring pathname))
   :output nil)
  nil)

(-> release-script-tests--record
    (pathname string &key (:platform (option string)))
    pathname)
(defun release-script-tests--record (pathname tag &key platform)
  "Write a fixture release record with TAG and optional PLATFORM to PATHNAME."
  (release-script-tests--write-file
   pathname
   (format nil "version=~A~%tag=~A~%commit=~A~@[~%platform=~A~]~%"
           *release-script-tests-version*
           tag
           *release-script-tests-commit*
           platform)))

(-> release-script-tests--fixture-curl () string)
(defun release-script-tests--fixture-curl ()
  "Return a curl substitute serving files from the test fixture root."
  (format nil
          "#!/bin/sh~%set -eu~%output=~%write_out=~%url=~%while [ \"$#\" -gt 0 ]; do~%  case $1 in~%    --output) output=$2; shift 2 ;;~%    --write-out) write_out=$2; shift 2 ;;~%    --retry|--proto|--max-time) shift 2 ;;~%    --*) shift ;;~%    *) url=$1; shift ;;~%  esac~%done~%case $url in~%  */latest)~%    [ -n \"$write_out\" ]~%    printf \"%s\" \"https://example.invalid/releases/${AUTOLITH_TEST_LATEST_TAG:-v0.11.0}\"~%    exit 0~%    ;;~%esac~%cp \"$AUTOLITH_TEST_RELEASE_FIXTURE/${url##*/}\" \"$output\"~%"))

(-> release-script-tests--install-linux-host-tools
    (pathname &key (:architecture string) (:libc string))
    pathname)
(defun release-script-tests--install-linux-host-tools
    (directory &key (architecture "x86_64") (libc "glibc"))
  "Install fixture commands reporting and satisfying one Linux release target."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (uname (merge-pathnames "uname" directory))
         (ldd (merge-pathnames "ldd" directory))
         (bwrap (merge-pathnames "bwrap" directory))
         (chmod (merge-pathnames "chmod" directory))
         (move (merge-pathnames "mv" directory)))
    (release-script-tests--write-file
     uname
     (format nil "#!/bin/sh
case ${1:-} in
  -s) printf 'Linux\\n' ;;
  -m) printf '~A\\n' ;;
  *) exit 64 ;;
esac
"
             architecture))
    (release-script-tests--write-file
     ldd
     (format nil "#!/bin/sh
printf '~A\\n'
"
             (ecase (intern (string-upcase libc) '#:keyword)
               (:glibc "libc.so.6 => /lib/libc.so.6 /lib64/ld-linux-x86-64.so.2")
               (:musl "musl libc")
               (:unknown "unrecognized libc"))))
    (release-script-tests--write-file bwrap "#!/bin/sh
exit 0
")
    (release-script-tests--write-file chmod "#!/bin/sh
recursive=
if [ \"${1:-}\" = -R ]; then
  recursive=-R
  shift
fi
mode=$1
shift
if [ \"${1:-}\" = -- ]; then
  shift
fi
if [ -n \"$recursive\" ]; then
  exec /bin/chmod \"$recursive\" \"$mode\" \"$@\"
else
  exec /bin/chmod \"$mode\" \"$@\"
fi
")
    (release-script-tests--write-file move "#!/bin/sh
force=
no_target=false
case ${1:-} in
  -Tf|-fT) force=-f; no_target=true; shift ;;
  -f) force=-f; shift ;;
  -T) no_target=true; shift ;;
esac
if [ \"${1:-}\" = -- ]; then
  shift
fi
if [ \"$no_target\" = true ]; then
  /bin/rm -f \"$2\"
fi
if [ -n \"$force\" ]; then
  exec /bin/mv -f \"$@\"
else
  exec /bin/mv \"$@\"
fi
")
    (dolist (pathname (list uname ldd bwrap chmod move))
      (release-script-tests--chmod "755" pathname))
    directory))

(-> release-script-tests--readlink (pathname) string)
(defun release-script-tests--readlink (pathname)
  "Return the literal target stored in symbolic link PATHNAME."
  (string-trim
   '(#\Newline #\Return)
   (release-script-tests--run
    (list "readlink" (namestring pathname)))))

(-> release-script-tests--cleanup (pathname) null)
(defun release-script-tests--cleanup (root)
  "Remove test ROOT after restoring write permissions."
  (when (uiop:directory-exists-p root)
    (release-script-tests--run
     (list "chmod" "-R" "u+w" (namestring root))
     :ignore-error-status t
     :output nil)
    (platform-delete-directory-tree *platform* root
                                    :validate t
                                    :if-does-not-exist ':ignore))
  nil)

(-> release-script-tests--make-release
    (pathname pathname &key (:library-extension string)
                            (:platform (option string)))
    pathname)
(defun release-script-tests--make-release
    (source-root release-root &key (library-extension "so") platform)
  "Create a minimal packaged release fixture below RELEASE-ROOT."
  (dolist (relative
           (list "libexec/autolith/.qlot/setup.lisp"
                 "libexec/autolith/autolith.asd"
                 "libexec/autolith/script/install"
                 "libexec/sbcl-source/version.lisp-expr"
                 (format nil "lib/libfff_c.~A" library-extension)
                 (format nil "lib/libcolorlisp-tree-sitter.~A"
                         library-extension)
                 "runtime/bin/sbcl"
                 "libexec/cl-exec-sandbox-helper"
                 "libexec/cl-exec-sandbox-process-group"))
    (release-script-tests--write-file
     (merge-pathnames relative release-root)
     ""))
  (let ((launcher (merge-pathnames "bin/autolith" release-root)))
    (ensure-directories-exist launcher)
    (uiop:copy-file (merge-pathnames "bin/autolith-release" source-root)
                    launcher)
    (release-script-tests--chmod "755" launcher))
  (uiop:copy-file
   (merge-pathnames "script/install" source-root)
   (merge-pathnames "libexec/autolith/script/install" release-root))
  (uiop:copy-file
   (merge-pathnames "script/launcher-cli.sh" source-root)
   (merge-pathnames "libexec/autolith/script/launcher-cli.sh" release-root))
  (release-script-tests--chmod
   "755" (merge-pathnames "libexec/autolith/script/install" release-root))
  (release-script-tests--chmod
   "755" (merge-pathnames "runtime/bin/sbcl" release-root))
  (release-script-tests--chmod
   "755" (merge-pathnames "libexec/cl-exec-sandbox-helper" release-root))
  (release-script-tests--chmod
   "755" (merge-pathnames "libexec/cl-exec-sandbox-process-group" release-root))
  (release-script-tests--record
   (merge-pathnames "RELEASE" release-root)
   (format nil "v~A" *release-script-tests-version*)
   :platform platform)
  release-root)

(-> release-script-tests--syntax (pathname) null)
(defun release-script-tests--syntax (source-root)
  "Check the remaining bootstrap shell boundaries and tracked Lisp programs."
  (dolist (relative '("bin/autolith"
                      "bin/autolith-release"
                      "bin/autolith-runtime"
                      "script/bootstrap"
                      "script/build-active"
                      "script/build-fff"
                      "script/build-recovery"
                      "script/check"
                      "script/launcher-cli.sh"
                      "script/build-release"
                      "script/build-release-runtime"
                      "script/build-static-release-runtime"
                      "script/ci-package-release"
                      "script/validate-linux-release-artifact"
                      "server/build-in-container"))
    (release-script-tests--run
     (list "bash" "-n" (namestring (merge-pathnames relative source-root)))
     :output nil))
  (release-script-tests--run
   (list "sh" "-n"
         (namestring (merge-pathnames "script/install" source-root)))
   :output nil)
  (dolist (relative '("script/build-release-runtime.lisp"
                      "script/build-static-release-runtime.lisp"
                      "script/runtime-requirement.lisp"
                      "script/validate-static-release.lisp"
                      "server/build-in-container.lisp"))
    (with-open-file (stream (merge-pathnames relative source-root)
                            :direction ':input
                            :external-format ':utf-8)
      (let ((*read-eval* nil))
        (loop while (read stream nil nil))))
    (test-assert (probe-file (merge-pathnames relative source-root))
                 (format nil "release program ~A is readable Lisp" relative)))
  (multiple-value-bind (output error-output status)
      (release-script-tests--run
       (list (or (uiop:getenv "AUTOLITH_SBCL")
                 (namestring (truename (uiop:argv0)))
                 "sbcl")
             "--noinform" "--no-userinit" "--no-sysinit"
             "--script"
             (namestring
              (merge-pathnames "script/validate-static-release.lisp"
                               source-root)))
       :ignore-error-status t)
    (declare (ignore output error-output))
    (test-assert (not (zerop status))
                 "the static release smoke program rejects missing arguments"))
  (let ((minimum-source-sha256
          (string-trim
           '(#\Space #\Tab #\Newline #\Return)
           (uiop:read-file-string
            (merge-pathnames "sbcl-source.sha256" source-root))))
        (release-source-identities
          (uiop:read-file-string
           (merge-pathnames "sbcl-source-releases.sha256" source-root))))
    (test-assert
     (search (format nil "~A  sbcl-2.6.6-source.tar.bz2"
                     minimum-source-sha256)
             release-source-identities)
     "the source release catalog retains the minimum runtime source identity"))
  nil)

(-> release-script-tests--darwin-fff-library-path (pathname pathname) null)
(defun release-script-tests--darwin-fff-library-path (source-root root)
  "Exercise Darwin SDK library discovery before the fff Cargo build."
  (let* ((fixture (merge-pathnames "darwin-fff-library-path/" root))
         (script-directory (merge-pathnames "script/" fixture))
         (script (merge-pathnames "build-fff.lisp" script-directory))
         (native-directory (merge-pathnames "native/fff/" fixture))
         (commit "0123456789abcdef0123456789abcdef01234567")
         (home (merge-pathnames "home/" fixture))
         (cache-home (merge-pathnames "cache/" fixture))
         (data-home (merge-pathnames "data/" fixture))
         (checkout
           (merge-pathnames (format nil "autolith/build/fff/~A/" commit)
                            cache-home))
         (fixture-bin (merge-pathnames "bin/" fixture))
         (xcrun (merge-pathnames "xcrun" fixture-bin))
         (git (merge-pathnames "git" fixture-bin))
         (cargo (merge-pathnames "cargo" fixture-bin))
         (sdk (merge-pathnames "MacOSX.sdk/" fixture))
         (sdk-library (merge-pathnames "usr/lib/" sdk))
         (event-log (merge-pathnames "library-path.log" fixture)))
    (ensure-directories-exist (merge-pathnames "placeholder" sdk-library))
    (ensure-directories-exist (merge-pathnames ".git/placeholder" checkout))
    (ensure-directories-exist script)
    (uiop:copy-file (merge-pathnames "script/build-fff.lisp" source-root)
                    script)
    (uiop:copy-file (merge-pathnames "script/roots.lisp" source-root)
                    (merge-pathnames "roots.lisp" script-directory))
    (release-script-tests--write-file
     (merge-pathnames "commit" native-directory)
     (format nil "~A~%" commit))
    (release-script-tests--write-file
     xcrun
     "#!/bin/sh
set -eu
[ \"$1\" = --show-sdk-path ]
printf '%s\\n' \"${AUTOLITH_TEST_SDK%/}\"
")
    (release-script-tests--write-file
     git
     "#!/bin/sh
set -eu
case \"$*\" in
  *\"rev-parse HEAD\") printf '%s\\n' \"${AUTOLITH_TEST_FFF_COMMIT:?}\" ;;
esac
")
    (release-script-tests--write-file
     cargo
     "#!/bin/sh
set -eu
mkdir -p target/release
printf '%s\\n' \"${LIBRARY_PATH:-}\" > \"${AUTOLITH_TEST_EVENT_LOG:?}\"
: > target/release/libfff_c.dylib
")
    (dolist (pathname (list xcrun git cargo))
      (release-script-tests--chmod "755" pathname))
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (lisp-worker-sbcl-command)
               "--noinform"
               "--no-sysinit"
               "--no-userinit"
               "--disable-debugger"
               "--eval" "(pushnew :darwin *features*)"
               "--script" (namestring script))
         :environment
         (list (format nil "HOME=~A" (namestring home))
               (format nil "XDG_CACHE_HOME=~A" (namestring cache-home))
               (format nil "XDG_DATA_HOME=~A" (namestring data-home))
               (format nil "PATH=~A:~A"
                       (string-right-trim "/" (namestring fixture-bin))
                       (or (uiop:getenv "PATH") ""))
               (format nil "AUTOLITH_TEST_SDK=~A" (namestring sdk))
               (format nil "AUTOLITH_TEST_FFF_COMMIT=~A" commit)
               (format nil "AUTOLITH_TEST_EVENT_LOG=~A"
                       (namestring event-log))
               "LIBRARY_PATH=/existing/lib")
         :ignore-error-status t)
      (test-assert
       (zerop status)
       (format nil "the Darwin fff build fixture succeeds:~%~A~%~A"
               output error-output)))
    (let ((library-path
            (string-trim '(#\Newline #\Return)
                         (uiop:read-file-string event-log))))
      (test-assert
       (string= library-path
                (format nil "/existing/lib:~A" (namestring sdk-library)))
       (format nil "the Darwin fff build appends the SDK library directory: ~A"
               library-path)))
    (test-assert
     (probe-file
      (merge-pathnames "autolith/native/fff/libfff_c.dylib" data-home))
     "the Darwin fff build publishes its private library")
    nil))

(-> release-script-tests--bootstrap-dependency-order (pathname pathname) null)
(defun release-script-tests--bootstrap-dependency-order (source-root root)
  "Exercise project dependency setup before bootstrap CFFI loading."
  (let* ((fixture (merge-pathnames "bootstrap-dependency-order/" root))
         (script-directory (merge-pathnames "script/" fixture))
         (home (merge-pathnames "home/" fixture))
         (quicklisp-directory (merge-pathnames "quicklisp/" home))
         (qlot-directory (merge-pathnames ".qlot/" fixture))
         (cache-home (merge-pathnames "cache/" fixture))
         (data-home (merge-pathnames "data/" fixture))
         (state-home (merge-pathnames "state/" fixture))
         (event-log (merge-pathnames "events.log" fixture))
         (bootstrap (merge-pathnames "bootstrap.lisp" script-directory))
         (fake-sbcl (merge-pathnames "fake-sbcl" fixture)))
    (ensure-directories-exist (merge-pathnames "placeholder" quicklisp-directory))
    (ensure-directories-exist (merge-pathnames "placeholder" qlot-directory))
    (ensure-directories-exist bootstrap)
    (uiop:copy-file (merge-pathnames "script/bootstrap.lisp" source-root)
                    bootstrap)
    (release-script-tests--write-file
     (merge-pathnames "runtime-requirement.lisp" script-directory)
     "(defun autolith-require-minimum-runtime (pathname)
  (declare (ignore pathname))
  nil)
")
    (dolist (relative '("build-sandbox.lisp" "build-fff.lisp"))
      (release-script-tests--write-file
       (merge-pathnames relative script-directory)
       "nil
"))
    (release-script-tests--write-file
     (merge-pathnames "setup.lisp" quicklisp-directory)
     "(defpackage #:ql
  (:use #:cl)
  (:export #:quickload))
(in-package #:ql)

(defun record-event (event)
  (with-open-file (stream (uiop:getenv \"AUTOLITH_TEST_EVENT_LOG\")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream \"~A~%\" event)))

(defun quickload (system &key silent)
  (declare (ignore silent))
  (when (eql system :cffi)
    (record-event \"global-cffi\")
    (unless (find-package \"CFFI\")
      (make-package \"CFFI\" :use '(\"CL\")))
    (setf (symbol-value
           (intern \"*FOREIGN-LIBRARY-DIRECTORIES*\" \"CFFI\"))
          nil))
  system)
")
    (release-script-tests--write-file
     (merge-pathnames "setup.lisp" qlot-directory)
     "(defpackage #:ql
  (:use #:cl)
  (:export #:quickload))
(in-package #:ql)

(defun record-event (event)
  (with-open-file (stream (uiop:getenv \"AUTOLITH_TEST_EVENT_LOG\")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream \"~A~%\" event)))

(record-event \"locked-setup\")

(let* ((setup (truename *load-truename*))
       (directory (uiop:pathname-directory-pathname setup))
       (root (uiop:pathname-parent-directory-pathname directory)))
  (pushnew root asdf:*central-registry* :test #'equal))

(defun quickload (system &key silent)
  (declare (ignore silent))
  (when (eql system :cffi)
    (record-event \"locked-cffi\")
    (unless (find-package \"CFFI\")
      (make-package \"CFFI\" :use '(\"CL\")))
    (setf (symbol-value
           (intern \"*FOREIGN-LIBRARY-DIRECTORIES*\" \"CFFI\"))
          nil))
  system)
")
    (release-script-tests--write-file
     (merge-pathnames "colorlisp.asd" fixture)
     "(asdf:defsystem #:colorlisp
  :components ((:file \"colorlisp\")))
")
    (release-script-tests--write-file
     (merge-pathnames "colorlisp.lisp" fixture)
     "(defpackage #:colorlisp
  (:use #:cl)
  (:export #:native-library-path))
(in-package #:colorlisp)

(defun native-library-path ()
  nil)
")
    (release-script-tests--write-file
     fake-sbcl
     "#!/bin/sh
set -eu
printf 'subprocess %s\\n' \"$*\" >> \"${AUTOLITH_TEST_EVENT_LOG:?}\"
")
    (release-script-tests--chmod "755" fake-sbcl)
    (release-script-tests--write-file
     (merge-pathnames "sbcl.version" fixture)
     (format nil "~A~%" (lisp-implementation-version)))
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (lisp-worker-sbcl-command)
               "--noinform"
               "--no-sysinit"
               "--no-userinit"
               "--disable-debugger"
               "--script"
               (namestring bootstrap))
         :environment
         (list (format nil "HOME=~A" (namestring home))
               (format nil "XDG_CACHE_HOME=~A" (namestring cache-home))
               (format nil "XDG_DATA_HOME=~A" (namestring data-home))
               (format nil "XDG_STATE_HOME=~A" (namestring state-home))
               (format nil "AUTOLITH_SBCL=~A" (namestring fake-sbcl))
               (format nil "AUTOLITH_TEST_EVENT_LOG=~A"
                       (namestring event-log))
               "NO_COLOR=1")
         :ignore-error-status t)
      (test-assert
       (zerop status)
       (format nil "the bootstrap dependency-order fixture succeeds:~%~A~%~A"
               output error-output)))
    (let* ((events (uiop:read-file-string event-log))
           (qlot-install (search "script/qlot-install.lisp" events))
           (locked-setup (search "locked-setup" events))
           (locked-cffi (search "locked-cffi" events)))
      (test-assert
       (and qlot-install
            locked-setup
            locked-cffi
            (< qlot-install locked-setup locked-cffi)
            (not (search "global-cffi" events)))
       (format nil
               "bootstrap materializes and activates locked dependencies before loading CFFI:~%~A"
               events))))
  nil)

(-> release-script-tests--source-launcher (pathname pathname) null)
(defun release-script-tests--source-launcher (source-root root)
  "Exercise source-image selection, bootstrap prompting, and forced source use."
  (let* ((fixture-root (merge-pathnames "source-launcher/" root))
         (bin-directory (merge-pathnames "bin/" fixture-root))
         (script-directory (merge-pathnames "script/" fixture-root))
         (recovery-directory (merge-pathnames "recovery/" fixture-root))
         (data-home (merge-pathnames "data/" fixture-root))
         (state-home (merge-pathnames "state/" fixture-root))
         (home (merge-pathnames "home/" fixture-root))
         (active-directory (merge-pathnames "autolith/active/" data-home))
         (active-core (merge-pathnames "autolith-active.core" active-directory))
         (active-manifest (merge-pathnames "manifest.sexp" active-directory))
         (launcher (merge-pathnames "autolith" bin-directory))
         (active-source (merge-pathnames "autolith-active" bin-directory))
         (bootstrap (merge-pathnames "bootstrap" script-directory))
         (recovery-source (merge-pathnames "launcher.lisp" recovery-directory))
         (fake-sbcl (merge-pathnames "fake-sbcl" fixture-root))
         (log (merge-pathnames "launcher.log" fixture-root)))
    (uiop:ensure-all-directories-exist
     (list bin-directory script-directory recovery-directory
           data-home state-home home))
    (uiop:copy-file (merge-pathnames "bin/autolith" source-root) launcher)
    (uiop:copy-file (merge-pathnames "script/launcher-cli.sh" source-root)
                    (merge-pathnames "launcher-cli.sh" script-directory))
    (release-script-tests--write-file active-source "")
    (release-script-tests--write-file recovery-source "")
    (release-script-tests--write-file
     (merge-pathnames "sbcl.version" fixture-root)
     "2.6.6\n")
    (release-script-tests--write-file
     (merge-pathnames "sbcl-source-releases.sha256" fixture-root)
     (format nil "~A  sbcl-2.6.6-source.tar.bz2~%"
             (make-string 64 :initial-element #\0)))
    (release-script-tests--write-file
     fake-sbcl
     "#!/bin/sh
set -eu
printf 'SBCL %s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
mode=UNKNOWN
probe=false
for argument in \"$@\"; do
  case $argument in
    --autolith-internal-active-image-probe) probe=true ;;
    */bin/autolith-active) mode=SOURCE ;;
    */autolith-active.core) mode=ACTIVE ;;
  esac
done
if [ \"$probe\" = true ]; then
  exit 0
fi
printf '%s %s\\n' \"$mode\" \"$*\"
case \" $* \" in
  *\" fixture-update \"*) exit 76 ;;
  *\" fixture-data-failure \"*) exit 1 ;;
esac
")
    (release-script-tests--write-file
     bootstrap
     "#!/bin/sh
set -eu
printf 'BOOTSTRAP\\n' >> \"$AUTOLITH_TEST_LOG\"
active=$XDG_DATA_HOME/autolith/active
mkdir -p \"$active\"
: > \"$active/autolith-active.core\"
printf '(:SBCL-GENERATIONS-IMAGE-MANIFEST :VERSION 1\\n)\\n' > \"$active/manifest.sexp\"
")
    (dolist (pathname (list launcher fake-sbcl bootstrap))
      (release-script-tests--chmod "755" pathname))
    (let* ((environment
             (list (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                   (format nil "XDG_STATE_HOME=~A" (namestring state-home))
                   (format nil "AUTOLITH_SBCL=~A" (namestring fake-sbcl))
                   "AUTOLITH_ACTIVE_CORE="
                   "AUTOLITH_RECOVERY_CORE="
                   (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))))
           (source-output
             (release-script-tests--run
              (list (namestring launcher) "--from-source" "fixture-argument")
              :environment environment)))
      (test-assert
       (and (search "SOURCE" source-output)
            (not (search "fast startup image" source-output))
            (not (search "--from-source" (uiop:read-file-string log))))
       "--from-source quietly bypasses images and is not forwarded")
      (let* ((fallback-environment
               (list (format nil "HOME=~A" (namestring home))
                     "XDG_DATA_HOME=relative/data"
                     "XDG_STATE_HOME=relative/state"
                     (format nil "AUTOLITH_SBCL=~A" (namestring fake-sbcl))
                     (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))))
             (output
               (release-script-tests--run
                (list (namestring launcher) "--from-source")
                :environment fallback-environment)))
        (test-assert
         (and (search "SOURCE" output)
              (uiop:directory-exists-p
               (merge-pathnames
                ".local/state/autolith/crash-pointers/" home))
              (not
               (uiop:directory-exists-p
                (merge-pathnames "relative/state/autolith/" fixture-root))))
         "the source launcher falls back from relative XDG base directories"))
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring launcher) "--from-source" "fixture-update")
           :environment environment
           :ignore-error-status t
           :output nil)
        (declare (ignore output error-output))
        (test-assert (= status 76)
                     "update handoff bypasses crash recovery unchanged"))
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring launcher) "--from-source" "data" "import" "fixture-data-failure")
           :environment environment :ignore-error-status t)
        (declare (ignore error-output))
        (test-assert (and (= status 1) (not (search "/recovery/launcher.lisp" output)))
                     "a noninteractive data failure returns without starting recovery"))
      (let* ((cache-home (merge-pathnames "cache/" fixture-root))
             (config-home (merge-pathnames "config/" fixture-root))
             (artifacts
               (list (merge-pathnames "autolith/generations/g1/autolith.core" data-home)
                     (merge-pathnames "autolith/runtimes/2.6.6/installation/bin/sbcl" data-home)
                     (merge-pathnames "autolith/lisp-images/worker.core" data-home)
                     (merge-pathnames "autolith/release-images" data-home)
                     (merge-pathnames "autolith/crashes/capsule.sexp" state-home)
                     (merge-pathnames "autolith/current-generation.sexp" state-home)
                     (merge-pathnames "autolith/provider-models.sexp" state-home)
                     (merge-pathnames "autolith/fff/index" cache-home)))
             (kept
               (list (merge-pathnames "autolith/conversations/c1/00000000000000000001.sexp"
                                      data-home)
                     (merge-pathnames "autolith/memories.sexp" data-home)
                     (merge-pathnames "autolith/image-commits/i1/reconstruct.lisp" data-home)
                     (merge-pathnames "autolith/auth.sexp" state-home)
                     (merge-pathnames "autolith/mutation-history/HEAD" state-home)
                     (merge-pathnames "autolith/mutations.sexp" state-home)
                     (merge-pathnames "autolith/preferences.sexp" state-home)
                     (merge-pathnames "autolith/init.lisp" config-home)))
             (uninstall-environment
               (append environment
                       (list (format nil "XDG_CACHE_HOME=~A" (namestring cache-home))
                             (format nil "XDG_CONFIG_HOME=~A" (namestring config-home))))))
        (flet ((populate ()
                 (dolist (pathname (append artifacts kept))
                   (release-script-tests--write-file pathname "x"))
                 (release-script-tests--write-file log "")))
          (populate)
          (multiple-value-bind (output error-output status)
              (release-script-tests--run
               (list (namestring launcher) "uninstall")
               :environment uninstall-environment :ignore-error-status t)
            (declare (ignore output))
            (test-assert
             (and (= status 64)
                  (search "--yes" error-output)
                  (every #'probe-file artifacts)
                  (zerop (length (uiop:read-file-string log))))
             "an unconfirmed uninstall without a terminal removes nothing"))
          (multiple-value-bind (output error-output status)
              (release-script-tests--run
               (list (namestring launcher) "uninstall" "--yes")
               :environment uninstall-environment :ignore-error-status t)
            (declare (ignore output))
            (test-assert
             (and (zerop status)
                  (notany #'probe-file artifacts)
                  (not (uiop:directory-exists-p (merge-pathnames "autolith/" cache-home)))
                  (every #'probe-file kept)
                  (search "source checkout" error-output)
                  (search (string-right-trim "/" (namestring (truename fixture-root)))
                          error-output)
                  (zerop (length (uiop:read-file-string log))))
             (format nil "a confirmed source uninstall removes artifacts and keeps user data: ~A"
                     error-output)))
          (multiple-value-bind (output error-output status)
              (release-script-tests--run
               (list (namestring launcher) "uninstall" "--yes")
               :environment uninstall-environment :ignore-error-status t)
            (declare (ignore output))
            (test-assert
             (and (zerop status) (search "Nothing to remove" error-output))
             "a second uninstall reports nothing to remove"))
          (populate)
          (let* ((command
                   (format nil "env ~{~A~^ ~} ~A uninstall"
                           (mapcar #'uiop:escape-shell-token uninstall-environment)
                           (uiop:escape-shell-token (namestring launcher))))
                 (output
                   (with-input-from-string (input (format nil "n~%"))
                     (uiop:run-program
                      (release-script-tests--pty-command command "n")
                      :input input
                      :output ':string
                      :error-output ':output
                      :ignore-error-status t))))
            (test-assert
             (and (search "Continue?" output)
                  (search "not removed" output)
                  (every #'probe-file artifacts))
             (format nil "declining the uninstall prompt removes nothing: ~A" output)))
          (dolist (pathname artifacts)
            (when (probe-file pathname)
              (delete-file pathname)))))
      (dolist (arguments '(("update") ("--update") ("update" "extra")))
        (release-script-tests--write-file log "")
        (multiple-value-bind (output error-output status)
            (release-script-tests--run
             (cons (namestring launcher) arguments)
             :environment (append environment
                                  '("AUTOLITH_INSTALLATION_KIND=release"))
             :ignore-error-status t)
          (declare (ignore output))
          (test-assert
           (and (= status 64)
                (plusp (length error-output))
                (zerop (length (uiop:read-file-string log))))
           "source update rejects before invoking Lisp despite a release environment marker")))
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring launcher) "update" "--help")
           :environment environment :ignore-error-status t)
        (declare (ignore error-output))
        (test-assert
         (and (zerop status) (plusp (length output))
              (zerop (length (uiop:read-file-string log))))
         "update help exits without starting the source application"))
      (release-script-tests--write-file log "")
      (let* ((command
               (format nil
                       "env ~{~A~^ ~} ~A fixture-argument"
                       (mapcar #'uiop:escape-shell-token environment)
                       (uiop:escape-shell-token (namestring launcher))))
             (output
               (with-input-from-string (input (format nil "y~%"))
                 (uiop:run-program
                  (release-script-tests--pty-command command "y")
                  :input input
                  :output ':string
                  :error-output ':output
                  :ignore-error-status t)))
             (events (uiop:read-file-string log)))
        (test-assert
         (and (search "fast startup image is missing or stale" output)
              (search "BOOTSTRAP" events)
              (search "ACTIVE" output)
              (not (search "SOURCE" output)))
         (format nil
                 "accepting the interactive prompt bootstraps and starts the new image:~%terminal: ~A~%events: ~A"
                 output events)))
      (let ((output
              (release-script-tests--run
               (list (namestring launcher) "resume" "--pristine")
               :environment environment)))
        (test-assert
         (and (search "SOURCE" output)
              (search "--pristine" output)
              (not (search "ACTIVE" output)))
         "pristine startup bypasses an available active core and forwards its flag"))
      (dolist (arguments '(("--" "--recovery" "--from-source")
                           ("--image" "--from-source")
                           ("--image" "--recovery")))
        (let ((output (release-script-tests--run
                       (cons (namestring launcher) arguments)
                       :environment environment)))
          (test-assert
           (and (search "ACTIVE" output)
                (search (format nil "~{~A~^ ~}" arguments) output))
           "launcher-looking operands are forwarded to the active image")))
      (dolist (pathname (list active-core active-manifest))
        (when (probe-file pathname)
          (delete-file pathname)))
      (release-script-tests--write-file log "")
      (let* ((command
               (format nil
                       "env ~{~A~^ ~} ~A fixture-argument"
                       (mapcar #'uiop:escape-shell-token environment)
                       (uiop:escape-shell-token (namestring launcher))))
             (output
               (with-input-from-string (input (format nil "n~%"))
                 (uiop:run-program
                  (release-script-tests--pty-command command "n")
                  :input input
                  :output ':string
                  :error-output ':output
                  :ignore-error-status t)))
             (events (uiop:read-file-string log)))
        (test-assert
         (and (search "fast startup image is missing or stale" output)
              (search "SOURCE" output)
              (not (search "BOOTSTRAP" events)))
         (format nil
                 "declining the interactive prompt loads source without bootstrapping: ~A"
                 output))))
  nil))

(-> release-script-tests--launcher (pathname pathname) null)
(defun release-script-tests--launcher (source-root root)
  "Exercise packaged launcher validation and its machine-readable probe."
  (let* ((release-root
           (merge-pathnames
            (format nil "autolith-v~A/" *release-script-tests-version*)
            root))
         (launcher
           (merge-pathnames "bin/autolith" release-root))
         (host-bin (merge-pathnames "linux-host-bin/" root))
         (path (format nil "~A:~A"
                       (string-right-trim "/" (namestring host-bin))
                       (or (uiop:getenv "PATH") "")))
         (environment
           (list "AUTOLITH_NO_UPDATE_CHECK=1"
                 (format nil "PATH=~A" path))))
    (release-script-tests--install-linux-host-tools host-bin)
    (release-script-tests--make-release source-root release-root
                                        :platform "x86_64-linux")
    (let ((output
            (release-script-tests--run
             (list (namestring launcher) "--autolith-release-probe")
             :environment environment)))
      (dolist (line
               (list
                (format nil "version=~A" *release-script-tests-version*)
                (format nil "tag=v~A" *release-script-tests-version*)
                (format nil "commit=~A" *release-script-tests-commit*)
                 "platform=x86_64-linux"
                (format nil "source=~A"
                        (string-right-trim
                         "/"
                         (namestring
                          (truename
                           (merge-pathnames "libexec/autolith/" release-root)))))
                (format nil "runtime=~A"
                        (namestring
                         (truename
                          (merge-pathnames "runtime/bin/sbcl" release-root))))))
        (test-assert (find line
                           (uiop:split-string output
                                              :separator '(#\Newline #\Return))
                           :test #'string=)
                     (format nil "release probe reports ~A" line))))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" *release-script-tests-version*)
     :platform "aarch64-linux")
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (not (eql status 0))
            (search "RELEASE platform aarch64-linux does not match host x86_64-linux."
                    error-output))
       "the release launcher rejects a mismatched platform record"))
    (release-script-tests--install-linux-host-tools host-bin :libc "musl")
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" *release-script-tests-version*))
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (not (eql status 0))
            (search "RELEASE lacks platform identity for this release variant."
                    error-output))
       "a musl launcher rejects legacy metadata without platform identity"))
    (release-script-tests--install-linux-host-tools host-bin)
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" *release-script-tests-version*)
     :platform "x86_64-linux")
    (let ((library
            (merge-pathnames "lib/libcolorlisp-tree-sitter.so" release-root)))
      (delete-file library)
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring launcher) "--autolith-release-probe")
           :environment environment
           :ignore-error-status t
           :output nil)
        (declare (ignore output error-output))
        (test-assert (not (eql status 0))
                     "the release launcher requires its private syntax library"))
      (release-script-tests--write-file library ""))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     "v0.12.0")
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (not (eql status 0))
            (search "RELEASE has an inconsistent tag." error-output))
       "the release launcher rejects mismatched RELEASE metadata"))
    (delete-file (merge-pathnames "RELEASE" release-root)))
  nil)

(-> release-script-tests--launcher-darwin (pathname pathname) null)
(defun release-script-tests--launcher-darwin (source-root root)
  "Exercise Darwin packaged launcher validation and its machine-readable probe."
  (let* ((release-root
           (merge-pathnames
            (format nil "autolith-darwin-launcher-v~A/"
                    *release-script-tests-version*)
            root))
         (launcher
           (merge-pathnames "bin/autolith" release-root))
         (host-bin (merge-pathnames "darwin-host-bin/" root))
         (path (format nil "~A:~A"
                       (string-right-trim "/" (namestring host-bin))
                       (or (uiop:getenv "PATH") "")))
         (environment
           (list "AUTOLITH_NO_UPDATE_CHECK=1"
                 (format nil "PATH=~A" path))))
    (release-script-tests--install-darwin-host-tools host-bin)
    (release-script-tests--make-release source-root release-root
                                        :library-extension "dylib"
                                        :platform "arm64-darwin")
    (let ((output
            (release-script-tests--run
             (list (namestring launcher) "--autolith-release-probe")
             :environment environment)))
      (dolist (line
               (list
                (format nil "version=~A" *release-script-tests-version*)
                (format nil "tag=v~A" *release-script-tests-version*)
                (format nil "commit=~A" *release-script-tests-commit*)
                 "platform=arm64-darwin"
                (format nil "source=~A"
                        (string-right-trim
                         "/"
                         (namestring
                          (truename
                           (merge-pathnames "libexec/autolith/" release-root)))))
                (format nil "runtime=~A"
                        (namestring
                         (truename
                          (merge-pathnames "runtime/bin/sbcl" release-root))))))
        (test-assert (find line
                           (uiop:split-string output
                                              :separator '(#\Newline #\Return))
                           :test #'string=)
                     (format nil "Darwin release probe reports ~A" line))))
    (let ((library
            (merge-pathnames "lib/libcolorlisp-tree-sitter.dylib"
                             release-root)))
      (delete-file library)
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring launcher) "--autolith-release-probe")
           :environment environment
           :ignore-error-status t
           :output nil)
        (declare (ignore output error-output))
        (test-assert (not (eql status 0))
                     "the Darwin release launcher requires its private syntax library"))
      (release-script-tests--write-file library ""))
    (release-script-tests--install-darwin-host-tools host-bin "x86_64")
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" *release-script-tests-version*)
     :platform "x86_64-darwin")
    (let ((output
            (release-script-tests--run
             (list (namestring launcher) "--autolith-release-probe")
             :environment environment)))
      (test-assert
       (find "platform=x86_64-darwin"
             (uiop:split-string output :separator '(#\Newline #\Return))
             :test #'string=)
       "the Darwin x86-64 release launcher reports its platform"))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" *release-script-tests-version*))
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (not (eql status 0))
            (search "RELEASE lacks platform identity for this release variant."
                    error-output))
       "the new Darwin x86-64 variant requires platform metadata"))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     "v0.12.0")
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (not (eql status 0))
            (search "RELEASE has an inconsistent tag." error-output))
       "the Darwin release launcher rejects mismatched RELEASE metadata"))
    (delete-file (merge-pathnames "RELEASE" release-root)))
  nil)

(-> release-script-tests--installer (pathname pathname) null)
(defun release-script-tests--installer (source-root root)
  "Exercise binary installer download, verification, link updates, and unsupported platform rejection."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (next-tag "v0.12.0")
         (release-name
           (format nil "autolith-~A-x86_64-linux" tag))
         (release-root
           (merge-pathnames
            (format nil "autolith-v~A/" *release-script-tests-version*)
            root))
         (fixture-root (merge-pathnames "fixture/" root))
         (fixture-source (merge-pathnames "fixture-source/" root))
         (fixture-bin (merge-pathnames "fixture-bin/" root))
         (fixture-release
           (merge-pathnames (format nil "~A/" release-name) fixture-source))
         (archive
           (merge-pathnames (format nil "~A.tar.gz" release-name) fixture-root))
         (checksum
           (merge-pathnames (format nil "~A.tar.gz.sha256" release-name)
                            fixture-root))
         (install-root (merge-pathnames "installation/" root))
         (bin-directory (merge-pathnames "bin/" root))
         (curl (merge-pathnames "curl" fixture-bin))
         (installer (merge-pathnames "script/install" source-root)))
    (release-script-tests--make-release source-root release-root)
    (uiop:ensure-all-directories-exist
     (list fixture-root fixture-source fixture-bin fixture-release))
    (let* ((uname (merge-pathnames "uname" fixture-bin))
           (path
             (format nil "~A:~A"
                     (string-right-trim "/" (namestring fixture-bin))
                     (or (uiop:getenv "PATH") ""))))
        (release-script-tests--write-file
         uname
         "#!/bin/sh
case ${1:-} in
  -s) printf 'SunOS\\n' ;;
  -m) printf 'amd64\\n' ;;
  *) exit 64 ;;
esac
")
        (release-script-tests--chmod "755" uname)
        (multiple-value-bind (output error-output status)
            (release-script-tests--run
             (list "/bin/sh" "-c"
                   (format nil "~A --version ~A 2>&1"
                           (namestring installer)
                           tag))
             :environment
             (list (format nil "HOME=~A" (namestring root))
                   (format nil "PATH=~A" path))
             :ignore-error-status t)
          (declare (ignore error-output))
          (test-assert
           (and (not (eql status 0))
                (search
                 "binary releases currently support Linux x86-64, Linux aarch64, macOS x86-64, macOS arm64, FreeBSD x86-64, NetBSD x86-64, and OpenBSD x86-64 only."
                 output))
           "the binary installer rejects unsupported platforms")))
    (release-script-tests--install-linux-host-tools fixture-bin)
    (release-script-tests--run
     (list "cp" "-a" (format nil "~A." (namestring release-root))
           (namestring fixture-release))
     :output nil)
    (release-script-tests--chmod "a-w" fixture-release)
    (release-script-tests--run
     (list "tar" "-czf" (namestring archive)
           "-C" (namestring fixture-source) release-name)
     :output nil)
    (if (release-archive--command-pathname "sha256sum")
        (release-script-tests--run
         (list "sha256sum" (file-namestring archive))
         :directory fixture-root
         :output checksum)
        (let ((output
                (release-script-tests--run
                 (list "shasum" "-a" "256" (file-namestring archive))
                 :directory fixture-root
                 :output ':string)))
          (release-script-tests--write-file checksum output)))
    (release-script-tests--write-file
     curl (release-script-tests--fixture-curl))
    (release-script-tests--chmod "755" curl)
    (let* ((path (format nil "~A:~A"
                         (string-right-trim "/" (namestring fixture-bin))
                         (or (uiop:getenv "PATH") "")))
           (base-environment
             (list
              (format nil "PATH=~A" path)
              (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A"
                      (namestring fixture-root))
              "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
              (format nil "AUTOLITH_INSTALL_ROOT=~A"
                      (string-right-trim "/" (namestring install-root)))
              (format nil "AUTOLITH_BIN_DIR=~A"
                      (string-right-trim "/" (namestring bin-directory))))))
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (test-assert
       (probe-file
         (merge-pathnames
          (format nil "releases/~A-x86_64-linux/bin/autolith" tag)
          install-root))
       "the installer publishes the requested release")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "current" install-root))
                (format nil "releases/~A-x86_64-linux" tag))
       "the installer selects the requested version atomically")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "autolith" bin-directory))
                (namestring (merge-pathnames "current/bin/autolith"
                                             install-root)))
       "the installer publishes the user command link")
      (let* ((fallback-home (merge-pathnames "installer-home/" root))
             (fallback-root
               (merge-pathnames
                ".local/share/autolith/installation/" fallback-home))
             (fallback-environment
               (append
                (remove-if
                 (lambda (entry)
                   (or (uiop:string-prefix-p "AUTOLITH_INSTALL_ROOT=" entry)
                       (uiop:string-prefix-p "AUTOLITH_BIN_DIR=" entry)))
                 base-environment)
                (list (format nil "HOME=~A" (namestring fallback-home))
                      "XDG_DATA_HOME=relative/data"
                      (format nil "AUTOLITH_BIN_DIR=~A"
                              (namestring bin-directory))))))
        (uiop:ensure-all-directories-exist (list fallback-home))
        (release-script-tests--run
         (list (namestring installer) "--version" tag)
         :environment fallback-environment
         :output nil)
        (test-assert
         (probe-file
          (merge-pathnames
           (format nil "releases/~A-x86_64-linux/bin/autolith" tag)
           fallback-root))
         "the installer falls back from a relative XDG data home"))
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (let ((next-target (merge-pathnames (format nil "releases/~A/" next-tag)
                                           install-root)))
        (release-script-tests--run
         (list "cp" "-a"
                (namestring
                 (merge-pathnames
                  (format nil "releases/~A-x86_64-linux/" tag)
                  install-root))
               (namestring next-target))
         :output nil)
        (release-script-tests--run
         (list "chmod" "-R" "u+w" (namestring next-target))
         :output nil)
        (release-script-tests--write-file
         (merge-pathnames "RELEASE" next-target)
         (format nil "version=~A~%tag=~A~%commit=~A~%"
                 (subseq next-tag 1)
                 next-tag
                 *release-script-tests-commit*))
        (release-script-tests--run
         (list (namestring installer)
               "--without-command-link" "--version" next-tag)
         :environment base-environment
         :output nil))
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "current" install-root))
                (format nil "releases/~A" next-tag))
       "the installer replaces an existing selected release link")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "autolith" bin-directory))
                (namestring (merge-pathnames "current/bin/autolith"
                                             install-root)))
       "no-link publication preserves the existing command prefix")
      (release-script-tests--run
       (list (namestring installer))
       :environment
       (append
        (remove "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
                base-environment :test #'string=)
        '("AUTOLITH_RELEASE_BASE_URL=https://example.invalid/releases"
          "AUTOLITH_RELEASE_LATEST_URL=https://example.invalid/releases/latest"))
       :output nil)))
  nil)

(-> release-script-tests--install-darwin-host-tools
    (pathname &optional string)
    pathname)
(defun release-script-tests--install-darwin-host-tools
    (directory &optional (architecture "arm64"))
  "Install fixture commands reporting and satisfying a Darwin release target."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (uname (merge-pathnames "uname" directory))
         (chmod (merge-pathnames "chmod" directory))
         (move (merge-pathnames "mv" directory)))
    (release-script-tests--write-file
     uname
     (format nil "#!/bin/sh~%case ${1:-} in~%  -s) printf 'Darwin\\n' ;;~%  -m) printf '~A\\n' ;;~%  *) exit 64 ;;~%esac~%"
             architecture))
    (release-script-tests--write-file chmod "#!/bin/sh
recursive=
if [ \"${1:-}\" = -R ]; then
  recursive=-R
  shift
fi
mode=$1
shift
if [ \"${1:-}\" = -- ]; then
  shift
fi
if [ -n \"$recursive\" ]; then
  exec /bin/chmod \"$recursive\" \"$mode\" \"$@\"
else
  exec /bin/chmod \"$mode\" \"$@\"
fi
")
    (release-script-tests--write-file move "#!/bin/sh
force=
if [ \"${1:-}\" = -f ]; then
  force=-f
  shift
fi
if [ \"${1:-}\" = -- ]; then
  shift
fi
if [ -n \"$force\" ]; then
  exec /bin/mv -f \"$@\"
else
  exec /bin/mv \"$@\"
fi
")
    (dolist (command (list uname chmod move))
      (release-script-tests--chmod "755" command))
    directory))

(-> release-script-tests--installer-darwin (pathname pathname) null)
(defun release-script-tests--installer-darwin (source-root root)
  "Exercise Darwin binary installer download, verification, and link updates."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (release-name
           (format nil "autolith-~A-arm64-darwin" tag))
         (x86-release-name
           (format nil "autolith-~A-x86_64-darwin" tag))
         (release-root
           (merge-pathnames
            (format nil "autolith-darwin-v~A/" *release-script-tests-version*)
            root))
         (fixture-root (merge-pathnames "fixture-darwin/" root))
         (fixture-source (merge-pathnames "fixture-darwin-source/" root))
         (fixture-bin (merge-pathnames "fixture-darwin-bin/" root))
         (fixture-release
           (merge-pathnames (format nil "~A/" release-name) fixture-source))
         (x86-fixture-release
           (merge-pathnames (format nil "~A/" x86-release-name)
                            fixture-source))
         (archive
           (merge-pathnames (format nil "~A.tar.gz" release-name) fixture-root))
         (x86-archive
           (merge-pathnames (format nil "~A.tar.gz" x86-release-name)
                            fixture-root))
         (checksum
           (merge-pathnames (format nil "~A.tar.gz.sha256" release-name)
                            fixture-root))
         (x86-checksum
           (merge-pathnames (format nil "~A.tar.gz.sha256" x86-release-name)
                            fixture-root))
         (install-root (merge-pathnames "darwin-installation/" root))
         (bin-directory (merge-pathnames "darwin-bin/" root))
         (curl (merge-pathnames "curl" fixture-bin))
         (installer (merge-pathnames "script/install" source-root)))
    (release-script-tests--make-release source-root release-root
                                        :library-extension "dylib"
                                        :platform "arm64-darwin")
    (uiop:ensure-all-directories-exist
     (list fixture-root fixture-source fixture-bin fixture-release
           x86-fixture-release))
    (release-script-tests--install-darwin-host-tools fixture-bin)
    (release-script-tests--run
     (list "cp" "-a" (format nil "~A." (namestring release-root))
           (namestring fixture-release))
     :output nil)
    (release-script-tests--run
     (list "cp" "-a" (format nil "~A." (namestring release-root))
           (namestring x86-fixture-release))
     :output nil)
    (release-script-tests--record
     (merge-pathnames "RELEASE" x86-fixture-release)
     tag
     :platform "x86_64-darwin")
    (release-script-tests--chmod "a-w" fixture-release)
    (release-script-tests--chmod "a-w" x86-fixture-release)
    (release-script-tests--run
     (list "tar" "-czf" (namestring archive)
           "-C" (namestring fixture-source) release-name)
     :output nil)
    (release-script-tests--run
     (list "tar" "-czf" (namestring x86-archive)
           "-C" (namestring fixture-source) x86-release-name)
     :output nil)
    (if (release-archive--command-pathname "sha256sum")
        (release-script-tests--run
         (list "sha256sum" (file-namestring archive))
         :directory fixture-root
         :output checksum)
        (let ((output
                (release-script-tests--run
                 (list "shasum" "-a" "256" (file-namestring archive))
                 :directory fixture-root
                 :output ':string)))
          (release-script-tests--write-file checksum output)))
    (if (release-archive--command-pathname "sha256sum")
        (release-script-tests--run
         (list "sha256sum" (file-namestring x86-archive))
         :directory fixture-root
         :output x86-checksum)
        (let ((output
                (release-script-tests--run
                 (list "shasum" "-a" "256" (file-namestring x86-archive))
                 :directory fixture-root
                 :output ':string)))
          (release-script-tests--write-file x86-checksum output)))
    (release-script-tests--write-file
     curl (release-script-tests--fixture-curl))
    (release-script-tests--chmod "755" curl)
    (let* ((path (format nil "~A:~A"
                         (string-right-trim "/" (namestring fixture-bin))
                         (or (uiop:getenv "PATH") "")))
           (base-environment
             (list
              (format nil "PATH=~A" path)
              (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A"
                      (namestring fixture-root))
              "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
              (format nil "AUTOLITH_INSTALL_ROOT=~A"
                      (string-right-trim "/" (namestring install-root)))
              (format nil "AUTOLITH_BIN_DIR=~A"
                      (string-right-trim "/" (namestring bin-directory))))))
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring installer) "--musl" "--version" tag)
           :environment base-environment
           :ignore-error-status t
           :output nil)
        (declare (ignore output))
        (test-assert
         (and (not (eql status 0))
              (search "libc selection is supported only on Linux."
                      error-output))
         "the Darwin installer rejects Linux libc selection"))
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (test-assert
       (probe-file
         (merge-pathnames
          (format nil "releases/~A-arm64-darwin/bin/autolith" tag)
          install-root))
       "the Darwin installer publishes the requested release")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "current" install-root))
                (format nil "releases/~A-arm64-darwin" tag))
       "the Darwin installer selects the requested version")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "autolith" bin-directory))
                (namestring (merge-pathnames "current/bin/autolith"
                                             install-root)))
       "the Darwin installer publishes the user command link")
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (release-script-tests--install-darwin-host-tools fixture-bin "x86_64")
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (test-assert
       (probe-file
        (merge-pathnames
         (format nil "releases/~A-x86_64-darwin/bin/autolith" tag)
         install-root))
       "the Darwin x86-64 installer publishes the requested release")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "current" install-root))
                (format nil "releases/~A-x86_64-darwin" tag))
       "the Darwin x86-64 installer selects the requested version")
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)))
  nil)

(-> release-script-tests--update-handoff (pathname pathname) null)
(defun release-script-tests--update-handoff (source-root root)
  "Exercise clean update handoff and preservation of a custom installation prefix."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (next-tag "v0.12.0")
         (fixture-root (merge-pathnames "update-handoff/" root))
         (install-root (merge-pathnames "custom-installation/" fixture-root))
         (release-root (merge-pathnames (format nil "releases/~A/" tag)
                                        install-root))
         (packaged-source (merge-pathnames "libexec/autolith/" release-root))
         (inner-launcher (merge-pathnames "bin/autolith" packaged-source))
         (bundled-installer (merge-pathnames "script/install" packaged-source))
         (launcher (merge-pathnames "bin/autolith" release-root))
         (bin-directory (merge-pathnames "custom-bin/" fixture-root))
         (command-link (merge-pathnames "autolith" bin-directory))
         (data-home (merge-pathnames "data/" fixture-root))
         (state-home (merge-pathnames "state/" fixture-root))
         (fixture-bin (merge-pathnames "fixture-bin/" fixture-root))
         (curl (merge-pathnames "curl" fixture-bin))
         (updated-launcher (merge-pathnames "updated-autolith" fixture-root))
         (log (merge-pathnames "handoff.log" fixture-root)))
    (release-script-tests--make-release source-root release-root)
    (uiop:ensure-all-directories-exist
     (list bin-directory data-home state-home fixture-bin))
    (release-script-tests--install-linux-host-tools fixture-bin)
    (uiop:run-program
     (list "ln" "-s" (format nil "releases/~A" tag)
           (namestring (merge-pathnames "current" install-root))))
    (uiop:run-program
     (list "ln" "-s" (namestring (merge-pathnames "current/bin/autolith"
                                                   install-root))
           (namestring command-link)))
    (release-script-tests--write-file
     inner-launcher
     "#!/bin/sh
set -eu
printf 'INNER_KIND=%s\\n' \"${AUTOLITH_INSTALLATION_KIND:-}\" >> \"$AUTOLITH_TEST_LOG\"
printf 'INNER_ROOT=%s\\n' \"${AUTOLITH_RELEASE_ROOT:-}\" >> \"$AUTOLITH_TEST_LOG\"
printf 'INNER_ARGS=%s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
printf 'INNER_ARG=<%s>\\n' \"$@\" >> \"$AUTOLITH_TEST_LOG\"
printf 'INNER_ARGC=%s\\n' \"$#\" >> \"$AUTOLITH_TEST_LOG\"
[ \"${AUTOLITH_SUPPRESS_UPDATE_OFFER:-}\" != 1 ] || exit 0
exit 76
")
    (release-script-tests--write-file
     updated-launcher
     "#!/bin/sh
set -eu
printf 'UPDATED_ARGS=%s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
printf 'UPDATED_ARG=<%s>\\n' \"$@\" >> \"$AUTOLITH_TEST_LOG\"
printf 'UPDATED_ARGC=%s\\n' \"$#\" >> \"$AUTOLITH_TEST_LOG\"
")
    (release-script-tests--write-file
     bundled-installer
     "#!/bin/sh
set -eu
without=false
requested=
while [ \"$#\" -gt 0 ]; do
  case $1 in
    --without-command-link) without=true; shift ;;
    --version) requested=$2; shift 2 ;;
    *) exit 91 ;;
  esac
done
[ \"$without\" = true ]
[ \"$requested\" = \"$AUTOLITH_TEST_LATEST_TAG\" ]
printf 'INSTALL_ROOT=%s\\n' \"$AUTOLITH_INSTALL_ROOT\" >> \"$AUTOLITH_TEST_LOG\"
printf 'INSTALL_ARGS=without-command-link,%s\\n' \"$requested\" >> \"$AUTOLITH_TEST_LOG\"
[ \"${AUTOLITH_TEST_INSTALL_STATUS:-0}\" = 0 ] || exit \"$AUTOLITH_TEST_INSTALL_STATUS\"
target=$AUTOLITH_INSTALL_ROOT/releases/$requested
mkdir -p \"$target/bin\"
cp \"$AUTOLITH_TEST_UPDATED_LAUNCHER\" \"$target/bin/autolith\"
chmod 755 \"$target/bin/autolith\"
temporary=$AUTOLITH_INSTALL_ROOT/.current.$$
ln -s \"releases/$requested\" \"$temporary\"
mv -Tf \"$temporary\" \"$AUTOLITH_INSTALL_ROOT/current\"
")
    (release-script-tests--write-file
     curl
     "#!/bin/sh
set -eu
printf 'DISCOVERY\\n' >> \"$AUTOLITH_TEST_LOG\"
[ \"${AUTOLITH_TEST_DISCOVERY_STATUS:-0}\" = 0 ] || exit \"$AUTOLITH_TEST_DISCOVERY_STATUS\"
printf 'https://example.invalid/releases/%s' \"$AUTOLITH_TEST_LATEST_TAG\"
")
    (release-script-tests--write-file
     (merge-pathnames "runtime/bin/sbcl" release-root)
     "#!/bin/sh
printf 'RUNTIME=%s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
")
    (dolist (pathname (list inner-launcher bundled-installer updated-launcher
                            curl))
      (release-script-tests--chmod "755" pathname))
    (let* ((active-root (merge-pathnames "autolith/active/" data-home))
           (recovery-root (merge-pathnames "autolith/recovery/" data-home)))
      (dolist (pathname (list (merge-pathnames "autolith-active.core" active-root)
                              (merge-pathnames "autolith-recovery.core"
                                               recovery-root)))
        (release-script-tests--write-file pathname "core"))
      (release-script-tests--write-file
       (merge-pathnames "manifest.sexp" active-root)
       "(:SBCL-GENERATIONS-IMAGE-MANIFEST :VERSION 1)\n")
      (release-script-tests--write-file
       (merge-pathnames "manifest.sexp" recovery-root)
       "(:RECOVERY-IMAGE :VERSION 2)\n")
      (release-script-tests--write-file
       (merge-pathnames "autolith/release-images" data-home)
       (format nil "~A:x86_64-linux~%" tag)))
    (let* ((environment
             (list (format nil "PATH=~A:~A"
                           (string-right-trim "/" (namestring fixture-bin))
                           (or (uiop:getenv "PATH") ""))
                   (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                   (format nil "XDG_STATE_HOME=~A" (namestring state-home))
                   (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))
                   (format nil "AUTOLITH_TEST_UPDATED_LAUNCHER=~A"
                           (namestring updated-launcher))
                   "AUTOLITH_SUPPRESS_UPDATE_OFFER="
                   "AUTOLITH_RELEASE_LATEST_URL=https://example.invalid/releases/latest"))
           (current (merge-pathnames "current" install-root))
           (image-marker (merge-pathnames "autolith/release-images" data-home)))
      (labels ((select-original ()
                 (sb-posix:unlink (namestring current))
                 (sb-posix:symlink (format nil "releases/~A" tag)
                                   (namestring current))
                 (release-script-tests--write-file log ""))

                (run-command (arguments &key (latest-tag next-tag)
                                          (discovery-status 0) (install-status 0)
                                          (expected-status 0) installed-p
                                          (discovered-p t) restart-p)
                 (select-original)
                 (multiple-value-bind (output error-output status)
                     (release-script-tests--run
                      (cons (namestring command-link) arguments)
                      :environment
                      (append environment
                              (list (format nil "AUTOLITH_TEST_LATEST_TAG=~A"
                                            latest-tag)
                                    (format nil "AUTOLITH_TEST_DISCOVERY_STATUS=~D"
                                            discovery-status)
                                    (format nil "AUTOLITH_TEST_INSTALL_STATUS=~D"
                                            install-status)))
                      :ignore-error-status t)
                   (test-assert
                     (= expected-status status)
                    (format nil "~S exits as expected: ~A ~A"
                            arguments output error-output)))
                 (let ((events (uiop:read-file-string log)))
                   (test-assert
                    (eq discovered-p (not (null (search "DISCOVERY" events))))
                    "update validates arguments before release discovery")
                   (test-assert
                    (eq restart-p
                        (not (null (or (search "INNER_ARGS=" events)
                                       (search "UPDATED_ARGS=" events)))))
                    "only interactive updates start a session")
                   (unless restart-p
                     (test-assert (not (search "RUNTIME=" events))
                                  "update-only never probes or rebuilds images")))
                 (test-assert
                  (string= (release-script-tests--readlink current)
                           (format nil "releases/~A" (if installed-p next-tag tag)))
                  "successful installation selects the update; other outcomes preserve selection")))
        (delete-file image-marker)
        (dolist (alias '("update" "--update"))
          (run-command (list alias) :installed-p t)
          (run-command (list alias "--") :installed-p t)
          (run-command (list alias) :latest-tag tag)
          (run-command (list alias) :latest-tag "v0.10.0")
          (run-command (list alias) :discovery-status 22 :expected-status 1)
          (run-command (list alias) :latest-tag "invalid-tag" :expected-status 1)
          (run-command (list alias) :install-status 7 :expected-status 1)
          (dolist (tail '(("--unknown") ("extra") ("--help" "extra")
                          ("--" "--help") ("--recovery")
                          ("resume" "fixture-conversation")))
            (run-command (cons alias tail) :expected-status 64 :discovered-p nil))
          (dolist (tail '(("--help") ("-h") ("--help" "--")))
            (run-command (cons alias tail) :discovered-p nil)))
        (run-command '("--immutable" "update")
                     :expected-status 64 :discovered-p nil)
        (run-command '("--from-source" "--update")
                     :expected-status 64 :discovered-p nil)
        (release-script-tests--write-file
         image-marker (format nil "~A:x86_64-linux~%" tag))
        (dolist (arguments '(nil ("resume" "fixture-conversation")
                             ("resume" "--" "--recovery" "--from-source")
                             ("--image" "--recovery" "resume" "two words" "")))
          (run-command arguments :installed-p t :restart-p t)
          (let ((events (uiop:read-file-string log)))
            (test-assert
             (and (search (format nil "INNER_ARGC=~D~%" (length arguments)) events)
                  (search (format nil "UPDATED_ARGC=~D~%" (length arguments)) events)
                  (search (format nil "~{INNER_ARG=<~A>~%~}" arguments) events)
                  (search (format nil "~{UPDATED_ARG=<~A>~%~}" arguments) events))
             "interactive update handoff preserves every original argument")))
        (run-command '("resume") :latest-tag tag :restart-p t)
        (run-command '("resume") :discovery-status 22 :restart-p t)
        (run-command '("resume") :install-status 7 :restart-p t)
        (select-original)
        (release-script-tests--run
         (list (namestring launcher) "resume" "fixture-conversation")
         :environment (append environment
                              (list (format nil "AUTOLITH_TEST_LATEST_TAG=~A"
                                            next-tag)))
         :output nil)))
    (let ((events (uiop:read-file-string log)))
      (test-assert
       (and (search "INNER_KIND=release" events)
            (search (format nil "INNER_ROOT=~A"
                            (string-right-trim
                             "/"
                             (namestring (truename release-root))))
                    events))
       "only the selected packaged topology receives release provenance")
      (test-assert
       (and (search (format nil "INSTALL_ROOT=~A"
                            (string-right-trim
                             "/"
                             (namestring (truename install-root))))
                    events)
            (search (format nil "INSTALL_ARGS=without-command-link,~A" next-tag)
                    events))
       "the bundled installer receives the derived custom root and no-link mode")
      (test-assert
       (and (search "INNER_ARGS=resume fixture-conversation" events)
            (search "UPDATED_ARGS=resume fixture-conversation" events))
       "the restarted release receives the original command arguments"))
    (test-assert
     (string= (release-script-tests--readlink command-link)
              (namestring (merge-pathnames "current/bin/autolith" install-root)))
     "a custom command prefix remains untouched across an update")
    (test-assert
     (string= (release-script-tests--readlink
               (merge-pathnames "current" install-root))
              (format nil "releases/~A" next-tag))
     "the verified updater atomically selects the new release"))
  nil)

(-> release-script-tests--uninstall (pathname pathname) null)
(defun release-script-tests--uninstall (source-root root)
  "Exercise packaged-release removal through the command link with user data kept."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (fixture-root (merge-pathnames "uninstall/" root))
         (install-root (merge-pathnames "installation/" fixture-root))
         (release-root (merge-pathnames (format nil "releases/~A/" tag) install-root))
         (bin-directory (merge-pathnames "bin/" fixture-root))
         (command-link (merge-pathnames "autolith" bin-directory))
         (data-home (merge-pathnames "data/" fixture-root))
         (state-home (merge-pathnames "state/" fixture-root))
         (cache-home (merge-pathnames "cache/" fixture-root))
         (config-home (merge-pathnames "config/" fixture-root))
         (fixture-bin (merge-pathnames "fixture-bin/" fixture-root))
         (log (merge-pathnames "uninstall.log" fixture-root))
         (artifacts
           (list (merge-pathnames "autolith/active/autolith-active.core" data-home)
                 (merge-pathnames "autolith/recovery/autolith-recovery.core" data-home)
                 (merge-pathnames "autolith/release-images" data-home)
                 (merge-pathnames "autolith/crash-pointers/launcher-1.path" state-home)
                 (merge-pathnames "autolith/fff/index" cache-home)))
         (kept
           (list (merge-pathnames "autolith/conversations/c1/00000000000000000001.sexp"
                                  data-home)
                 (merge-pathnames "autolith/agendas.sexp" data-home)
                 (merge-pathnames "autolith/api-keys.sexp" state-home)
                 (merge-pathnames "autolith/permissions.sexp" state-home)
                 (merge-pathnames "autolith/init.lisp" config-home)))
         (environment
           (list (format nil "PATH=~A:~A"
                         (string-right-trim "/" (namestring fixture-bin))
                         (or (uiop:getenv "PATH") ""))
                 (format nil "HOME=~A" (namestring fixture-root))
                 (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                 (format nil "XDG_STATE_HOME=~A" (namestring state-home))
                 (format nil "XDG_CACHE_HOME=~A" (namestring cache-home))
                 (format nil "XDG_CONFIG_HOME=~A" (namestring config-home))
                 (format nil "AUTOLITH_BIN_DIR=~A" (string-right-trim "/" (namestring bin-directory)))
                 (format nil "AUTOLITH_TEST_LOG=~A" (namestring log)))))
    (release-script-tests--make-release source-root release-root)
    (uiop:ensure-all-directories-exist (list bin-directory fixture-bin))
    (release-script-tests--install-linux-host-tools fixture-bin)
    (uiop:run-program
     (list "ln" "-s" (format nil "releases/~A" tag)
           (namestring (merge-pathnames "current" install-root))))
    (uiop:run-program
     (list "ln" "-s" (namestring (merge-pathnames "current/bin/autolith" install-root))
           (namestring command-link)))
    (release-script-tests--write-file
     (merge-pathnames "libexec/autolith/bin/autolith" release-root)
     "#!/bin/sh
printf 'INNER\\n' >> \"$AUTOLITH_TEST_LOG\"
")
    (release-script-tests--chmod
     "755" (merge-pathnames "libexec/autolith/bin/autolith" release-root))
    (dolist (pathname (append artifacts kept))
      (release-script-tests--write-file pathname "x"))
    (release-script-tests--write-file log "")
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring command-link) "uninstall")
         :environment environment :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (= status 64)
            (search "--yes" error-output)
            (probe-file command-link)
            (every #'probe-file artifacts))
       "an unconfirmed release uninstall without a terminal removes nothing"))
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring command-link) "uninstall" "--yes")
         :environment environment :ignore-error-status t)
      (declare (ignore output))
      (test-assert
       (and (zerop status)
            (not (uiop:directory-exists-p install-root))
            (not (probe-file command-link))
            (notany #'probe-file artifacts)
            (not (uiop:directory-exists-p (merge-pathnames "autolith/" cache-home)))
            (every #'probe-file kept)
            (search (string-right-trim "/" (namestring (truename fixture-root)))
                    error-output)
            (zerop (length (uiop:read-file-string log))))
       (format nil "a confirmed release uninstall removes the installation and keeps user data: ~A"
               error-output))))
  nil)

(-> release-script-tests--musl-update-handoff (pathname pathname) null)
(defun release-script-tests--musl-update-handoff (source-root root)
  "Exercise musl-preserving update from a platform-qualified installation."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (next-tag "v0.12.0")
         (platform "x86_64-linux-musl")
         (fixture-root (merge-pathnames "musl-update-handoff/" root))
         (install-root (merge-pathnames "installation/" fixture-root))
         (release-root
           (merge-pathnames (format nil "releases/~A-~A/" tag platform)
                            install-root))
         (packaged-source (merge-pathnames "libexec/autolith/" release-root))
         (inner-launcher (merge-pathnames "bin/autolith" packaged-source))
         (bundled-installer (merge-pathnames "script/install" packaged-source))
         (launcher (merge-pathnames "bin/autolith" release-root))
         (data-home (merge-pathnames "data/" fixture-root))
         (fixture-bin (merge-pathnames "fixture-bin/" fixture-root))
         (curl (merge-pathnames "curl" fixture-bin))
         (updated-launcher (merge-pathnames "updated-autolith" fixture-root))
         (log (merge-pathnames "handoff.log" fixture-root)))
    (release-script-tests--make-release
     source-root release-root :platform platform)
    (uiop:ensure-all-directories-exist (list data-home fixture-bin))
    (release-script-tests--install-linux-host-tools fixture-bin :libc "musl")
    (release-script-tests--write-file
     curl (release-script-tests--fixture-curl))
    (release-script-tests--chmod "755" curl)
    (sb-posix:symlink
     (format nil "releases/~A-~A" tag platform)
     (namestring (merge-pathnames "current" install-root)))
    (release-script-tests--write-file
     inner-launcher
     "#!/bin/sh
exit 76
")
    (release-script-tests--write-file
     updated-launcher
     "#!/bin/sh
printf 'UPDATED_ARGS=%s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
")
    (release-script-tests--write-file
     bundled-installer
     "#!/bin/sh
set -eu
printf 'INSTALL_ARGS=%s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
[ \"$1\" = --musl ]
[ \"$2\" = --without-command-link ]
[ \"$3\" = --version ]
requested=$4
[ \"$requested\" = \"$AUTOLITH_TEST_LATEST_TAG\" ]
target=$AUTOLITH_INSTALL_ROOT/releases/${requested}-x86_64-linux-musl
mkdir -p \"$target/bin\"
cp \"$AUTOLITH_TEST_UPDATED_LAUNCHER\" \"$target/bin/autolith\"
chmod 755 \"$target/bin/autolith\"
temporary=$AUTOLITH_INSTALL_ROOT/.current.$$
ln -s \"releases/${requested}-x86_64-linux-musl\" \"$temporary\"
mv -Tf \"$temporary\" \"$AUTOLITH_INSTALL_ROOT/current\"
")
    (dolist (pathname (list inner-launcher bundled-installer updated-launcher))
      (release-script-tests--chmod "755" pathname))
    (let* ((active-root (merge-pathnames "autolith/active/" data-home))
           (recovery-root (merge-pathnames "autolith/recovery/" data-home)))
      (dolist (pathname (list (merge-pathnames "autolith-active.core" active-root)
                              (merge-pathnames "autolith-recovery.core"
                                               recovery-root)))
        (release-script-tests--write-file pathname "core"))
      (release-script-tests--write-file
       (merge-pathnames "manifest.sexp" active-root)
       "(:SBCL-GENERATIONS-IMAGE-MANIFEST :VERSION 1)\n")
      (release-script-tests--write-file
       (merge-pathnames "manifest.sexp" recovery-root)
       "(:RECOVERY-IMAGE :VERSION 2)\n")
      (release-script-tests--write-file
       (merge-pathnames "autolith/release-images" data-home)
       (format nil "~A:~A~%" tag platform)))
    (release-script-tests--run
     (list (namestring launcher) "resume" "fixture-conversation")
     :environment
     (list
      (format nil "PATH=~A:~A"
              (string-right-trim "/" (namestring fixture-bin))
              (or (uiop:getenv "PATH") ""))
      (format nil "XDG_DATA_HOME=~A" (namestring data-home))
      (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))
      (format nil "AUTOLITH_TEST_UPDATED_LAUNCHER=~A"
              (namestring updated-launcher))
      (format nil "AUTOLITH_TEST_LATEST_TAG=~A" next-tag)
      "AUTOLITH_RELEASE_LATEST_URL=https://example.invalid/releases/latest")
     :output nil)
    (let ((events (uiop:read-file-string log)))
      (test-assert
       (search (format nil
                       "INSTALL_ARGS=--musl --without-command-link --version ~A"
                       next-tag)
               events)
       "a musl release preserves its variant during self-update")
      (test-assert
       (search "UPDATED_ARGS=resume fixture-conversation" events)
       "the updated musl release receives the original arguments"))
    (test-assert
     (string=
      (release-script-tests--readlink
       (merge-pathnames "current" install-root))
      (format nil "releases/~A-~A" next-tag platform))
     "the musl updater selects the platform-qualified target"))
  nil)

(-> release-script-tests--image-marker-platform (pathname pathname) null)
(defun release-script-tests--image-marker-platform (source-root root)
  "Exercise saved-image rebuilds when one tag switches Linux libc variants."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (fixture-root (merge-pathnames "image-marker-platform/" root))
         (release-root (merge-pathnames "release/" fixture-root))
         (launcher (merge-pathnames "bin/autolith" release-root))
         (inner-launcher
           (merge-pathnames "libexec/autolith/bin/autolith" release-root))
         (runtime (merge-pathnames "runtime/bin/sbcl" release-root))
         (fixture-bin (merge-pathnames "fixture-bin/" fixture-root))
         (data-home (merge-pathnames "data/" fixture-root))
         (log (merge-pathnames "sbcl.log" fixture-root))
         (marker (merge-pathnames "autolith/release-images" data-home)))
    (release-script-tests--make-release
     source-root release-root :platform "x86_64-linux")
    (uiop:ensure-all-directories-exist (list fixture-bin data-home))
    (release-script-tests--install-linux-host-tools fixture-bin)
    (release-script-tests--write-file
     inner-launcher
     "#!/bin/sh
exit 0
")
    (release-script-tests--write-file
     runtime
     "#!/bin/sh
set -eu
printf '%s\\n' \"$*\" >> \"$AUTOLITH_TEST_SBCL_LOG\"
target=
for argument in \"$@\"; do target=$argument; done
case \" $* \" in
  *'build-recovery.lisp'*)
    mkdir -p \"$(dirname \"$target\")\"
    : > \"$target\"
    printf '(:RECOVERY-IMAGE :VERSION 2)\\n' > \"$(dirname \"$target\")/manifest.sexp\"
    ;;
  *'build-active.lisp'*)
    mkdir -p \"$(dirname \"$target\")\"
    : > \"$target\"
    printf '(:SBCL-GENERATIONS-IMAGE-MANIFEST :VERSION 1)\\n' > \"$(dirname \"$target\")/manifest.sexp\"
    ;;
esac
")
    (dolist (pathname (list inner-launcher runtime))
      (release-script-tests--chmod "755" pathname))
    (labels ((environment ()
               (list
                "AUTOLITH_NO_UPDATE_CHECK=1"
                (format nil "PATH=~A:~A"
                        (string-right-trim "/" (namestring fixture-bin))
                        (or (uiop:getenv "PATH") ""))
                (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                (format nil "AUTOLITH_TEST_SBCL_LOG=~A" (namestring log))))

             (build-count ()
               (let ((content (uiop:read-file-string log)))
                 (loop with start = 0
                       for position = (search "--script" content :start2 start)
                       while position
                       count t
                       do (setf start (+ position 8))))))
      (release-script-tests--run
       (list (namestring launcher))
       :environment (environment)
       :output nil)
      (test-assert
       (and (string= (string-trim '(#\Newline #\Return)
                                  (uiop:read-file-string marker))
                     (format nil "~A:x86_64-linux" tag))
            (= (build-count) 2))
       "the first glibc launch builds and marks both saved images")
      (release-script-tests--record
       (merge-pathnames "RELEASE" release-root)
       tag
       :platform "x86_64-linux-musl")
      (release-script-tests--install-linux-host-tools fixture-bin :libc "musl")
      (release-script-tests--run
       (list (namestring launcher))
       :environment (environment)
       :output nil)
      (test-assert
       (and (string= (string-trim '(#\Newline #\Return)
                                  (uiop:read-file-string marker))
                     (format nil "~A:x86_64-linux-musl" tag))
            (= (build-count) 4))
       "switching one tag from glibc to musl rebuilds both saved images")))
  nil)

(-> release-script-tests--runtime-adapter (pathname pathname) null)
(defun release-script-tests--runtime-adapter (source-root root)
  "Exercise minimum-version runtime selection in the pinned-runtime adapter."
  (let* ((fixture-root (merge-pathnames "runtime-adapter/" root))
         (bin-directory (merge-pathnames "bin/" fixture-root))
         (tools-directory (merge-pathnames "tools/" fixture-root))
         (data-home (merge-pathnames "data/" fixture-root))
         (adapter-target
           (merge-pathnames "autolith-runtime-target" bin-directory))
         (adapter (merge-pathnames "autolith-runtime" bin-directory))
         (fake-curl (merge-pathnames "curl" tools-directory))
         (fake-readlink (merge-pathnames "readlink" tools-directory))
         (fake-sha256sum (merge-pathnames "sha256sum" tools-directory))
         (fake-tar (merge-pathnames "tar" tools-directory))
         (script (merge-pathnames "script.lisp" fixture-root)))
    (uiop:ensure-all-directories-exist
     (list bin-directory tools-directory data-home))
    (uiop:copy-file (merge-pathnames "bin/autolith-runtime" source-root)
                    adapter-target)
    (dolist (name '("runtime-probe.lisp" "runtime-requirement.lisp"))
      (let ((target (merge-pathnames (format nil "script/~A" name) fixture-root)))
        (ensure-directories-exist target)
        (uiop:copy-file (merge-pathnames (format nil "script/~A" name) source-root)
                        target)))
    (release-script-tests--chmod "755" adapter-target)
    (sb-posix:symlink (namestring adapter-target) (namestring adapter))
    (release-script-tests--write-file
     fake-readlink
     "#!/bin/sh
set -eu
case ${1-} in
  -f|--) exit 64 ;;
esac
exec /usr/bin/readlink \"$@\"
")
    (release-script-tests--chmod "755" fake-readlink)
    (release-script-tests--write-file
     fake-curl
     "#!/bin/sh
set -eu
output=
while [ \"$#\" -gt 0 ]; do
  case $1 in
    --output) output=$2; shift 2 ;;
    *) shift ;;
  esac
done
[ -n \"$output\" ]
printf 'fixture archive\n' > \"$output\"
")
    (release-script-tests--write-file
     fake-sha256sum
     "#!/bin/sh
set -eu
printf '%s  %s\n' \"${AUTOLITH_TEST_SHA256:?}\" \"$1\"
")
    (release-script-tests--write-file
     fake-tar
     "#!/bin/sh
set -eu
archive=
destination=
while [ \"$#\" -gt 0 ]; do
  case $1 in
    -C) destination=$2; shift 2 ;;
    *.tar.bz2) archive=$1; shift ;;
    *) shift ;;
  esac
done
[ -n \"$archive\" ]
[ -n \"$destination\" ]
name=${archive##*/}
version=${name#sbcl-}
version=${version%-source.tar.bz2}
mkdir -p \"$destination/sbcl-$version/src/code\"
printf '\"%s\"\n' \"$version\" > \"$destination/sbcl-$version/version.lisp-expr\"
: > \"$destination/sbcl-$version/src/code/list.lisp\"
")
    (dolist (pathname (list fake-curl fake-sha256sum fake-tar))
      (release-script-tests--chmod "755" pathname))
    (release-script-tests--write-file
     (merge-pathnames "sbcl.version" fixture-root)
     (format nil "2.6.6~%"))
    (release-script-tests--write-file
     (merge-pathnames "sbcl-source-releases.sha256" fixture-root)
     (format nil "~A  sbcl-2.6.6-source.tar.bz2~%~A  sbcl-2.6.7-source.tar.bz2~%"
             (make-string 64 :initial-element #\0)
             (make-string 64 :initial-element #\1)))
    (release-script-tests--write-file script (format nil "(quit)~%"))
    (flet ((fake-runtime (version)
             (let ((pathname
                     (merge-pathnames (format nil "sbcl-~A" version)
                                      fixture-root)))
               (release-script-tests--write-file
                pathname
                (format nil
                        "#!/bin/sh
set -eu
case \" $* \" in
  *runtime-probe.lisp*) shift 3; exec ~A --noinform --no-userinit --no-sysinit --eval ~A \"$@\" ;;
  *' --script '*) printf 'ADAPTER-SCRIPT version=~A source=%s %s\\n' \"${AUTOLITH_SBCL_SOURCE_ROOT-}\" \"$*\" ;;
esac
"
                        (test-fixture-shell-quote *platform* (namestring sb-ext:*runtime-pathname*))
                        (test-fixture-shell-quote
                         *platform*
                         (format nil "(progn (require :asdf) (sb-ext:without-package-locks (setf (symbol-function 'lisp-implementation-version) (lambda () ~S))))"
                                 version))
                        version))
               (release-script-tests--chmod "755" pathname)
               pathname))
           (run-adapter (runtime &key install-p
                                    inherited-source
                                    (sha256
                                      (make-string 64 :initial-element #\1)))
             (multiple-value-bind (output error-output status)
                 (release-script-tests--run
                  (list "/bin/sh" "-c"
                        (format nil "~A~:[~; --install~] --script ~A 2>&1"
                                (namestring adapter)
                                install-p
                                (namestring script)))
                  :directory fixture-root
                  :environment
                  (list (format nil "PATH=~A:~A"
                                (namestring tools-directory)
                                (or (uiop:getenv "PATH") ""))
                        (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                        (format nil "AUTOLITH_SBCL=~A" (namestring runtime))
                        (format nil "AUTOLITH_SBCL_SOURCE_ROOT=~A"
                                (or inherited-source ""))
                        (format nil "AUTOLITH_TEST_SHA256=~A" sha256))
                  :ignore-error-status t)
               (declare (ignore error-output))
               (values (or output "") status))))
      (multiple-value-bind (output status)
          (run-adapter (fake-runtime "2.6.7"))
        (test-assert (zerop status)
                     "the adapter accepts a newer runtime than the minimum")
        (test-assert (search "ADAPTER-SCRIPT" output)
                     "the adapter runs the script on a newer runtime")
        (test-assert
         (probe-file
          (merge-pathnames "autolith/runtimes/command" data-home))
         "the adapter records the accepted runtime command"))
      (multiple-value-bind (output status)
          (run-adapter (fake-runtime "2.6.7") :install-p t)
        (test-assert (and (zerop status) (search "ADAPTER-SCRIPT" output))
                     "the adapter installs tracked source for a newer runtime")
        (test-assert
         (string=
          (uiop:read-file-string
           (merge-pathnames "autolith/runtimes/2.6.7/source.identity"
                            data-home))
          (format nil "2.6.7 ~A~%"
                  (make-string 64 :initial-element #\1)))
         "the adapter records the independently tracked source identity")
        (test-assert
         (probe-file
          (merge-pathnames
           "autolith/runtimes/2.6.7/source/src/code/list.lisp"
           data-home))
         "the adapter publishes the matching read-only source tree"))
      (multiple-value-bind (output status)
          (run-adapter (fake-runtime "2.10.0"))
        (test-assert (and (zerop status) (search "ADAPTER-SCRIPT" output))
                     "the adapter compares version fields numerically"))
      (multiple-value-bind (output status)
          (run-adapter (fake-runtime "2.10.0") :install-p t)
        (test-assert (and (zerop status) (search "ADAPTER-SCRIPT" output))
                     "an untracked newer runtime can bootstrap without managed source"))
      (let* ((version "2.10.0-local")
             (runtime (fake-runtime version))
             (matching (merge-pathnames "inherited-source/" fixture-root))
             (mismatched (merge-pathnames "mismatched-source/" fixture-root)))
        (release-script-tests--write-file
         (merge-pathnames "version.lisp-expr" matching)
         (format nil "~S~%" version))
        (release-script-tests--write-file
         (merge-pathnames "version.lisp-expr" mismatched)
         (format nil "~S~%" "2.6.6"))
        (dolist (entry (list (list (namestring matching) t)
                            (list (namestring mismatched) nil)
                            (list "inherited-source/" nil)
                            (list (namestring (merge-pathnames "missing-source/"
                                                             fixture-root))
                                  nil)))
          (destructuring-bind (inherited accepted-p) entry
            (multiple-value-bind (output status)
                (run-adapter runtime :inherited-source inherited)
              (test-assert
               (and (zerop status)
                    (search (format nil "version=~A source=~A " version
                                    (if accepted-p inherited ""))
                            output))
               (format nil "inherited implementation source ~S is ~:[rejected~;preserved~]"
                       inherited accepted-p))))))
      (let ((identity
              (merge-pathnames "autolith/runtimes/2.6.7/source.identity"
                               data-home)))
        (release-script-tests--chmod "600" identity)
        (release-script-tests--write-file
         identity
         (format nil "2.6.7 ~A~%" (make-string 64 :initial-element #\0)))
        (release-script-tests--chmod "444" identity)
        (multiple-value-bind (output status)
            (run-adapter (fake-runtime "2.6.7")
                         :install-p t
                         :sha256 (make-string 64 :initial-element #\0))
          (declare (ignore output))
          (test-assert (not (zerop status))
                       "source installation rejects a mismatched archive digest")))
      (dolist (version '("2.6.8.termux" "2.6.6-local" "2.7.0.123-gabc" "2.6.6+build_1"))
        (multiple-value-bind (output status)
            (run-adapter (fake-runtime version) :install-p t)
          (test-assert (and (zerop status)
                            (search (format nil "version=~A source= " version) output))
                       (format nil "custom runtime ~A keeps its identity without mismatched source" version))))
      (dolist (version '("2.6.5.termux" "2.6" "2..8" "2.6.8." "2.6.8garbage"))
        (multiple-value-bind (output status)
            (run-adapter (fake-runtime version))
          (declare (ignore output))
          (test-assert (not (zerop status))
                       (format nil "the adapter rejects invalid or old runtime ~S" version))))
      (multiple-value-bind (output status)
          (run-adapter (fake-runtime "2.6.3"))
        (declare (ignore output))
        (test-assert (not (zerop status))
                     "the adapter rejects a runtime older than the minimum")))
    (let ((*package* (find-package "CL-USER")))
      (load (merge-pathnames "script/runtime-requirement.lisp" source-root)))
    (let ((at-least-p
            (fdefinition
             (find-symbol "AUTOLITH-VERSION-AT-LEAST-P" "CL-USER")))
          (require-runtime
            (fdefinition
             (find-symbol "AUTOLITH-REQUIRE-MINIMUM-RUNTIME" "CL-USER"))))
      (test-assert (funcall at-least-p "2.6.6" "2.6.6")
                   "the version requirement accepts the minimum itself")
      (test-assert (funcall at-least-p "2.10.0" "2.6.6")
                   "the version requirement compares fields numerically")
      (dolist (version '("2.6.8.termux" "2.7.0.123-gabc" "2.6.6-local" "2.6.6+build_1"))
        (test-assert (funcall at-least-p version "2.6.6")
                     (format nil "the shared requirement accepts custom build ~S" version)))
      (dolist (version '("" "2" "2.6" "2..6" "2.6.6." "2.6.6-" "2.6.6+"
                          "2.6.6/termux" "2.6.6. ../bad" "2.6.6termux" "2.6.5.termux"))
        (test-assert (not (funcall at-least-p version "2.6.6"))
                     (format nil "the shared requirement rejects invalid or old build ~S" version)))
      (test-assert (not (funcall at-least-p "2.6.3" "2.6.6"))
                   "the version requirement rejects older versions")
      (test-assert (not (funcall at-least-p "1.9.9" "2.6.6"))
                   "the version requirement rejects older major series")
      (dolist (minimum '("garbage" "2..4" "2.6" "2.6.6.1"))
        (let ((pathname (merge-pathnames "malformed-sbcl.version" fixture-root)))
          (release-script-tests--write-file pathname minimum)
          (test-assert
           (handler-case
               (progn
                 (funcall require-runtime pathname)
                 nil)
             (error ()
               t))
           (format nil "the runtime requirement rejects malformed minimum ~S"
                   minimum)))))
    nil))

(-> release-script-tests--write-uname (pathname string string) pathname)
(defun release-script-tests--write-uname (directory os architecture)
  "Install a uname fixture reporting OS and ARCHITECTURE."
  (let ((uname (merge-pathnames "uname"
                                (uiop:ensure-directory-pathname directory))))
    (release-script-tests--write-file
     uname
     (format nil
             "#!/bin/sh
case ${1:-} in
  -s) printf '~A\\n' ;;
  -m) printf '~A\\n' ;;
  *) exit 64 ;;
esac
"
             os architecture))
    (release-script-tests--chmod "755" uname)
    uname))

(-> release-script-tests--write-checksum (pathname pathname) pathname)
(defun release-script-tests--write-checksum (archive checksum)
  "Write the SHA-256 checksum of ARCHIVE to CHECKSUM."
  (if (release-archive--command-pathname "sha256sum")
      (release-script-tests--run
       (list "sha256sum" (file-namestring archive))
       :directory (uiop:pathname-directory-pathname archive)
       :output checksum)
      (let ((output
              (release-script-tests--run
               (list "shasum" "-a" "256" (file-namestring archive))
               :directory (uiop:pathname-directory-pathname archive)
               :output ':string)))
        (release-script-tests--write-file checksum output)))
  checksum)

(-> release-script-tests--write-release-archive
    (pathname pathname &key (:tag string) (:platform string)
                            (:record-platform (option string)))
    (values pathname pathname))
(defun release-script-tests--write-release-archive
    (release-root fixture-root &key tag platform record-platform)
  "Package RELEASE-ROOT as TAG's PLATFORM archive below FIXTURE-ROOT."
  (let* ((release-name (format nil "autolith-~A-~A" tag platform))
         (fixture-source (merge-pathnames "source/" fixture-root))
         (fixture-release
           (merge-pathnames (format nil "~A/" release-name) fixture-source))
         (archive
           (merge-pathnames (format nil "~A.tar.gz" release-name) fixture-root))
         (checksum
           (merge-pathnames (format nil "~A.tar.gz.sha256" release-name)
                            fixture-root)))
    (release-script-tests--cleanup fixture-release)
    (dolist (pathname (list archive checksum))
      (when (probe-file pathname)
        (delete-file pathname)))
    (uiop:ensure-all-directories-exist (list fixture-root fixture-source))
    (release-script-tests--run
     (list "cp" "-a" (format nil "~A." (namestring release-root))
           (namestring fixture-release))
     :output nil)
    (release-script-tests--run
     (list "chmod" "-R" "u+w" (namestring fixture-release))
     :output nil)
    (release-script-tests--record
     (merge-pathnames "RELEASE" fixture-release)
     tag
     :platform record-platform)
    (release-script-tests--chmod "a-w" fixture-release)
    (release-script-tests--run
     (list "tar" "-czf" (namestring archive)
           "-C" (namestring fixture-source) release-name)
     :output nil)
    (release-script-tests--write-checksum archive checksum)
    (values archive checksum)))

(-> release-script-tests--installer-platform-identity (pathname pathname) null)
(defun release-script-tests--installer-platform-identity (source-root root)
  "Exercise installer platform validation and same-tag Linux variant isolation."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (release-root (merge-pathnames "identity-release/" root))
         (fixture-root (merge-pathnames "identity-fixture/" root))
         (fixture-bin (merge-pathnames "identity-bin/" root))
         (install-root (merge-pathnames "identity-installation/" root))
         (bin-directory (merge-pathnames "identity-command-bin/" root))
         (curl (merge-pathnames "curl" fixture-bin))
         (installer (merge-pathnames "script/install" source-root)))
    (release-script-tests--make-release source-root release-root)
    (uiop:ensure-all-directories-exist
     (list fixture-root fixture-bin install-root bin-directory))
    (release-script-tests--write-file
     curl (release-script-tests--fixture-curl))
    (release-script-tests--chmod "755" curl)
    (labels ((environment ()
               (list
                (format nil "PATH=~A:~A"
                        (string-right-trim "/" (namestring fixture-bin))
                        (or (uiop:getenv "PATH") ""))
                (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A"
                        (namestring fixture-root))
                "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
                (format nil "AUTOLITH_INSTALL_ROOT=~A"
                        (string-right-trim "/" (namestring install-root)))
                (format nil "AUTOLITH_BIN_DIR=~A"
                        (string-right-trim "/" (namestring bin-directory)))))

             (install-fails (arguments diagnostic description)
               (multiple-value-bind (output error-output status)
                   (release-script-tests--run
                    (append (list (namestring installer)) arguments)
                    :environment (environment)
                    :ignore-error-status t
                    :output nil)
                 (declare (ignore output))
                 (test-assert
                  (and (not (eql status 0))
                       (search diagnostic error-output))
                  description))))
      (release-script-tests--write-release-archive
       release-root fixture-root
       :tag tag
       :platform "aarch64-linux")
      (release-script-tests--install-linux-host-tools
       fixture-bin :architecture "aarch64")
      (install-fails
       (list "--version" tag)
       "unexpected platform identity or layout"
       "Linux aarch64 rejects a legacy archive without platform identity")
      (release-script-tests--write-release-archive
       release-root fixture-root
       :tag tag
       :platform "x86_64-linux"
       :record-platform "aarch64-linux")
      (release-script-tests--install-linux-host-tools fixture-bin)
      (install-fails
       (list "--version" tag)
       "unexpected platform identity or layout"
       "the installer rejects extracted platform metadata that mismatches the archive")
      (release-script-tests--write-release-archive
       release-root fixture-root
       :tag tag
       :platform "x86_64-linux"
       :record-platform "x86_64-linux")
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment (environment)
       :output nil)
      (release-script-tests--write-release-archive
       release-root fixture-root
       :tag tag
       :platform "x86_64-linux-musl")
      (release-script-tests--install-linux-host-tools fixture-bin :libc "musl")
      (install-fails
       (list "--musl" "--version" tag)
       "unexpected platform identity or layout"
       "musl rejects a legacy archive without platform identity")
      (release-script-tests--write-release-archive
       release-root fixture-root
       :tag tag
       :platform "x86_64-linux-musl"
       :record-platform "x86_64-linux-musl")
      (release-script-tests--install-linux-host-tools fixture-bin)
      (install-fails
       (list "--musl" "--version" tag)
       "requested libc musl does not match detected libc glibc"
       "the installer rejects musl selection on a glibc host")
      (release-script-tests--install-linux-host-tools fixture-bin :libc "unknown")
      (install-fails
       (list "--version" tag)
       "Linux libc could not be identified as glibc or musl"
       "the installer fails closed on unknown Linux libc output")
      (release-script-tests--install-linux-host-tools fixture-bin :libc "musl")
      (release-script-tests--run
       (list (namestring installer) "--musl" "--version" tag)
       :environment (environment)
       :output nil)
      (test-assert
       (and
        (probe-file
         (merge-pathnames
          (format nil "releases/~A-x86_64-linux/bin/autolith" tag)
          install-root))
        (probe-file
         (merge-pathnames
          (format nil "releases/~A-x86_64-linux-musl/bin/autolith" tag)
          install-root))
        (string=
         (release-script-tests--readlink
          (merge-pathnames "current" install-root))
         (format nil "releases/~A-x86_64-linux-musl" tag)))
       "glibc and musl installations of one tag remain distinct")
      (release-script-tests--install-linux-host-tools fixture-bin)
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment (environment)
       :output nil)
      (test-assert
       (string=
        (release-script-tests--readlink
         (merge-pathnames "current" install-root))
        (format nil "releases/~A-x86_64-linux" tag))
       "the installer can reselect the glibc variant without collision")))
  nil)

(-> release-script-tests--platform-ids () null)
(defun release-script-tests--platform-ids ()
  "Exercise canonical release platform identifiers."
  (dolist (case '(("Linux" "x86_64" "x86_64-linux")
                  ("Linux" "aarch64" "aarch64-linux")
                  ("Darwin" "x86_64" "x86_64-darwin")
                  ("Darwin" "arm64" "arm64-darwin")
                  ("FreeBSD" "x86_64" "x86_64-freebsd")
                  ("NetBSD" "x86_64" "x86_64-netbsd")
                  ("OpenBSD" "x86_64" "x86_64-openbsd")))
    (destructuring-bind (os architecture expected) case
      (test-assert
       (string= (release-archive--platform-id os architecture) expected)
       (format nil "~A/~A maps to ~A" os architecture expected))))
  (test-assert
   (string= (release-archive--platform-id "Linux" "x86_64" "musl")
            "x86_64-linux-musl")
   "Linux/x86_64 with musl maps to x86_64-linux-musl")
  (test-assert
   (string= (release-archive--platform-id "Linux" "aarch64" "musl")
            "aarch64-linux-musl")
   "Linux/aarch64 with musl maps to aarch64-linux-musl")
  (test-assert
   (string= (release-archive--linux-libc-output->identity
             "/lib/ld-musl-x86_64.so.1")
            "musl")
   "musl ldd output establishes musl identity")
  (test-assert
   (string= (release-archive--linux-libc-output->identity
             "libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6")
            "glibc")
   "glibc ldd output establishes glibc identity")
  (test-assert
   (handler-case
       (progn
         (release-archive--linux-libc-output->identity "unknown")
         nil)
     (release-archive-error (condition)
       (search "Could not identify the Linux C library"
               (release-archive-error-cause condition))))
   "unrecognized ldd output is rejected")
  (dolist (case '(("SunOS" "amd64")
                  ("FreeBSD" "aarch64")))
    (destructuring-bind (os architecture) case
      (test-assert
       (handler-case
           (progn
             (release-archive--platform-id os architecture)
             nil)
         (release-archive-error (condition)
           (and (eq (release-archive-error-stage condition) ':prerequisites)
                (search "Binary releases currently support"
                        (release-archive-error-cause condition)))))
       (format nil "~A/~A is not a release target" os architecture))))
  (let ((old-platform (uiop:getenv "AUTOLITH_RELEASE_PLATFORM")))
    (unwind-protect
         (progn
           (platform-setenv "AUTOLITH_RELEASE_PLATFORM" "sparc-sunos")
           (test-assert
            (handler-case
                (progn
                  (release-archive--platform)
                  nil)
              (release-archive-error (condition)
                (search "AUTOLITH_RELEASE_PLATFORM names"
                        (release-archive-error-cause condition))))
            "release platform overrides must match the native host"))
      (if old-platform
          (platform-setenv "AUTOLITH_RELEASE_PLATFORM" old-platform)
          (platform-unsetenv "AUTOLITH_RELEASE_PLATFORM"))))
  (when (string-equal (software-type) "Linux")
    (let* ((old-libc (uiop:getenv "AUTOLITH_LIBC"))
           (detected (release-archive--linux-libc))
           (mismatch (if (string= detected "musl") "glibc" "musl")))
      (unwind-protect
           (progn
             (platform-setenv "AUTOLITH_LIBC" mismatch)
             (test-assert
              (handler-case
                  (progn
                    (release-archive--linux-libc)
                    nil)
                (release-archive-error (condition)
                  (search "but this host uses"
                          (release-archive-error-cause condition))))
              "release libc overrides must match the native host"))
        (if old-libc
            (platform-setenv "AUTOLITH_LIBC" old-libc)
            (platform-unsetenv "AUTOLITH_LIBC")))))
  nil)

(-> release-script-tests--launcher-bsd (pathname pathname) null)
(defun release-script-tests--launcher-bsd (source-root root)
  "Exercise BSD packaged launcher validation without Bubblewrap."
  (dolist (spec '(("FreeBSD" "x86_64-freebsd")
                  ("NetBSD" "x86_64-netbsd")
                  ("OpenBSD" "x86_64-openbsd")))
    (destructuring-bind (os platform) spec
      (let* ((release-root
               (merge-pathnames (format nil "bsd-launcher-~A/" os) root))
             (launcher (merge-pathnames "bin/autolith" release-root))
             (host-bin (merge-pathnames (format nil "bsd-host-~A/" os) root))
             (path (format nil "~A:~A"
                           (string-right-trim "/" (namestring host-bin))
                           (or (uiop:getenv "PATH") "")))
             (environment
               (list "AUTOLITH_NO_UPDATE_CHECK=1"
                     (format nil "PATH=~A" path))))
        (release-script-tests--write-uname host-bin os "amd64")
        (release-script-tests--make-release
         source-root release-root :platform platform)
        (let ((output
                (release-script-tests--run
                 (list (namestring launcher) "--autolith-release-probe")
                 :environment environment)))
          (test-assert
           (and (search (format nil "version=~A" *release-script-tests-version*)
                        output)
                (search (format nil "platform=~A" platform) output))
           (format nil "the ~A release launcher reports its platform" os)))
        (let ((library
                (merge-pathnames "lib/libcolorlisp-tree-sitter.so" release-root)))
          (delete-file library)
          (multiple-value-bind (output error-output status)
              (release-script-tests--run
               (list (namestring launcher) "--autolith-release-probe")
               :environment environment
               :ignore-error-status t
               :output nil)
            (declare (ignore output error-output))
            (test-assert (not (eql status 0))
                         (format nil
                                 "the ~A release launcher requires its private syntax library"
                                 os)))
          (release-script-tests--write-file library ""))
        (let ((helper
                (merge-pathnames "libexec/cl-exec-sandbox-process-group"
                                 release-root)))
          (delete-file helper)
          (multiple-value-bind (output error-output status)
              (release-script-tests--run
               (list (namestring launcher) "--autolith-release-probe")
               :environment environment
               :ignore-error-status t)
            (declare (ignore output))
            (test-assert
             (and (not (eql status 0))
                  (search "the private process-group helper is missing."
                          error-output))
             (format nil
                     "the ~A release launcher requires its packaged process-group helper"
                     os)))
          (release-script-tests--write-file helper "")
          (release-script-tests--chmod "755" helper)))))
  nil)

(-> release-script-tests--installer-bsd (pathname pathname) null)
(defun release-script-tests--installer-bsd (source-root root)
  "Exercise FreeBSD, NetBSD, and OpenBSD amd64 installer publication."
  (let* ((tag (format nil "v~A" *release-script-tests-version*))
         (release-root (merge-pathnames "bsd-release/" root))
         (installer (merge-pathnames "script/install" source-root)))
    (release-script-tests--make-release source-root release-root)
    (dolist (spec '(("FreeBSD" "amd64" "x86_64-freebsd")
                    ("NetBSD" "amd64" "x86_64-netbsd")
                    ("OpenBSD" "amd64" "x86_64-openbsd")))
      (destructuring-bind (os architecture platform) spec
        (let* ((release-name (format nil "autolith-~A-~A" tag platform))
               (fixture-root
                 (merge-pathnames (format nil "fixture-~A/" platform) root))
               (fixture-source
                 (merge-pathnames (format nil "fixture-~A-source/" platform)
                                  root))
               (fixture-bin
                 (merge-pathnames (format nil "fixture-~A-bin/" platform) root))
               (fixture-release
                 (merge-pathnames (format nil "~A/" release-name)
                                  fixture-source))
               (archive
                 (merge-pathnames (format nil "~A.tar.gz" release-name)
                                  fixture-root))
               (checksum
                 (merge-pathnames (format nil "~A.tar.gz.sha256" release-name)
                                  fixture-root))
               (install-root
                 (merge-pathnames (format nil "~A-installation/" platform)
                                  root))
               (bin-directory
                 (merge-pathnames (format nil "~A-bin/" platform) root))
               (curl (merge-pathnames "curl" fixture-bin)))
          (uiop:ensure-all-directories-exist
           (list fixture-root fixture-source fixture-bin fixture-release))
          (release-script-tests--write-uname fixture-bin os architecture)
          (release-script-tests--run
           (list "cp" "-a" (format nil "~A." (namestring release-root))
                 (namestring fixture-release))
           :output nil)
          (release-script-tests--chmod "a-w" fixture-release)
          (release-script-tests--run
           (list "tar" "-czf" (namestring archive)
                 "-C" (namestring fixture-source) release-name)
           :output nil)
          (release-script-tests--write-checksum archive checksum)
          (release-script-tests--write-file
           curl (release-script-tests--fixture-curl))
          (release-script-tests--chmod "755" curl)
          (release-script-tests--run
           (list (namestring installer) "--version" tag)
           :environment
           (list
            (format nil "PATH=~A:~A"
                    (string-right-trim "/" (namestring fixture-bin))
                    (or (uiop:getenv "PATH") ""))
            (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A"
                    (namestring fixture-root))
            "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
            (format nil "AUTOLITH_INSTALL_ROOT=~A"
                    (string-right-trim "/" (namestring install-root)))
            (format nil "AUTOLITH_BIN_DIR=~A"
                    (string-right-trim "/" (namestring bin-directory))))
           :output nil)
          (test-assert
           (probe-file
             (merge-pathnames
              (format nil "releases/~A-~A/bin/autolith" tag platform)
              install-root))
           (format nil "the ~A installer publishes the requested release" os))
          (test-assert
           (string= (release-script-tests--readlink
                     (merge-pathnames "current" install-root))
                    (format nil "releases/~A-~A" tag platform))
           (format nil "the ~A installer selects the requested version" os))))))
  nil)

(-> release-script-tests--portable-copy (pathname) null)
(defun release-script-tests--portable-copy (root)
  "Exercise recursive copy that preserves symbolic links."
  (let* ((source (merge-pathnames "copy-source" root))
         (target (merge-pathnames "copy-target" root))
         (source-directory (uiop:ensure-directory-pathname source))
         (target-directory (uiop:ensure-directory-pathname target))
         (file (merge-pathnames "file.txt" source-directory))
         (link (merge-pathnames "link.txt" source-directory)))
    (release-script-tests--write-file file "payload")
    (sb-posix:symlink "file.txt" (namestring link))
    (release-archive--copy source target)
    (test-assert
     (probe-file (merge-pathnames "file.txt" target-directory))
     "portable copy preserves regular files")
    (test-assert
     (string= (sb-posix:readlink
               (namestring (merge-pathnames "link.txt" target-directory)))
              "file.txt")
     "portable copy preserves symbolic links without dereferencing them"))
  nil)

(-> release-script-tests--checksum-format (pathname) null)
(defun release-script-tests--checksum-format (root)
  "Exercise GNU-format SHA-256 checksum publication."
  (let* ((file (merge-pathnames "payload.bin" root))
         (checksum (merge-pathnames "payload.bin.sha256" root)))
    (release-script-tests--write-file file "autolith")
    (release-archive--checksum-file file checksum)
    (let* ((line (string-trim '(#\Newline #\Return)
                              (uiop:read-file-string checksum)))
           (digest (first (uiop:split-string line :separator '(#\Space))))
           (name (first (last (uiop:split-string line :separator '(#\Space))))))
      (test-assert (= (length digest) 64)
                   "checksum digest is SHA-256")
      (test-assert (string= name "payload.bin")
                   "checksum names the archived file")
      (test-assert (search "  " line)
                   "checksum uses GNU two-space format")
      (test-assert (string= digest (release-archive--sha256-digest file))
                   "checksum matches the computed digest")))
  nil)

(-> release-script-tests--runtime-bootstrap (pathname pathname) null)
(defun release-script-tests--runtime-bootstrap (source-root root)
  "Exercise pinned and host runtime bootstrap selection and validation."
  (let* ((fixture (merge-pathnames "runtime-bootstrap/" root))
         (bin (merge-pathnames "bin/" fixture))
         (installation (merge-pathnames "installation/" fixture))
         (log (merge-pathnames "bootstrap.log" fixture))
         (curl-log (merge-pathnames "curl.log" fixture))
         (sbcl (merge-pathnames "sbcl" bin))
         (curl (merge-pathnames "curl" bin))
         (ldd (merge-pathnames "ldd" bin))
         (bash (release-archive--command-pathname "bash"))
         (script (merge-pathnames "script/build-release-runtime" source-root))
         (path (format nil "~A:/bin:/usr/bin"
                       (string-right-trim "/" (namestring bin)))))
    (uiop:ensure-all-directories-exist (list bin installation))
    (release-script-tests--write-uname bin "FreeBSD" "amd64")
    (release-script-tests--write-file
     sbcl
     (format nil "#!/bin/sh~%case \" $* \" in~%  *lisp-implementation-version*) printf '%s' \"${AUTOLITH_TEST_SBCL_VERSION:-2.6.6}\"; exit 0 ;;~%esac~%printf '%s\\n' \"$*\" > \"${AUTOLITH_TEST_BOOTSTRAP_LOG:?}\"~%exit 0~%"))
    (release-script-tests--write-file
     curl
     (format nil "#!/bin/sh~%printf 'curl invoked\\n' > \"${AUTOLITH_TEST_CURL_LOG:?}\"~%exit 1~%"))
    (release-script-tests--write-file
     ldd
     "#!/bin/sh
case ${AUTOLITH_TEST_LIBC:-glibc} in
  musl) printf 'musl libc\\n' ;;
  glibc) printf 'libc.so.6\\n' ;;
  *) printf 'unknown libc\\n'; exit 1 ;;
esac
")
    (dolist (pathname (list sbcl curl ldd))
      (release-script-tests--chmod "755" pathname))
    (labels ((environment (&rest extra)
               (append
                (list (format nil "PATH=~A" path)
                      (format nil "AUTOLITH_SBCL=~A" (namestring sbcl))
                      (format nil "AUTOLITH_TEST_BOOTSTRAP_LOG=~A"
                              (namestring log))
                      (format nil "AUTOLITH_TEST_CURL_LOG=~A"
                              (namestring curl-log)))
                extra))

             (run-bootstrap (&rest extra)
               (release-script-tests--run
                (list (namestring bash) (namestring script)
                      (namestring installation))
                :environment (apply #'environment extra)
                :ignore-error-status t)))
      (multiple-value-bind (output error-output status)
          (run-bootstrap)
        (declare (ignore error-output))
        (test-assert (zerop status)
                     "BSD runtime bootstrap uses the host SBCL")
        (test-assert
         (search "Using host SBCL 2.6.6 as the FreeBSD bootstrap compiler."
                 output)
         "BSD runtime bootstrap reports the validated host compiler")
        (test-assert (not (probe-file curl-log))
                     "BSD runtime bootstrap does not download an official binary")
        (test-assert
         (and (probe-file log)
              (search "build-release-runtime.lisp" (uiop:read-file-string log)))
         "BSD runtime bootstrap invokes the runtime builder"))
      (when (probe-file log)
        (delete-file log))
      (multiple-value-bind (output error-output status)
          (run-bootstrap "AUTOLITH_TEST_SBCL_VERSION=2.6.5-85913ede1")
        (declare (ignore error-output))
        (test-assert
         (and (zerop status)
              (search "Using host SBCL 2.6.5-85913ede1 as the FreeBSD bootstrap compiler."
                      output)
              (not (probe-file curl-log))
              (probe-file log))
         "host runtime bootstrap accepts a release with a vendor suffix"))
      (when (probe-file log)
        (delete-file log))
      (multiple-value-bind (output error-output status)
          (run-bootstrap "AUTOLITH_TEST_SBCL_VERSION=1.9.9-foo")
        (let ((diagnostic (concatenate 'string (or output "")
                                       (or error-output ""))))
          (test-assert
           (and (not (zerop status))
                (search "needs host SBCL 2.0.0 or newer" diagnostic)
                (not (probe-file log)))
           "host runtime bootstrap rejects an unsupported compiler")))
      (release-script-tests--write-uname bin "Linux" "x86_64")
      (multiple-value-bind (output error-output status)
          (run-bootstrap)
        (declare (ignore error-output))
        (test-assert
         (and (zerop status)
              (search "Using host SBCL 2.6.6 as the Linux bootstrap compiler."
                      output)
              (not (probe-file curl-log))
              (probe-file log))
         "Linux x86-64 runtime bootstrap uses a validated host compiler"))
      (multiple-value-bind (output error-output status)
          (run-bootstrap "AUTOLITH_LIBC=musl")
        (let ((diagnostic (concatenate 'string (or output "")
                                       (or error-output ""))))
          (test-assert
           (and (not (zerop status))
                (search "expected musl but detected glibc" diagnostic))
           "runtime bootstrap rejects a mismatched libc override")))
      (multiple-value-bind (output error-output status)
          (run-bootstrap "AUTOLITH_TEST_LIBC=unknown")
        (let ((diagnostic (concatenate 'string (or output "")
                                       (or error-output ""))))
          (test-assert
           (and (not (zerop status))
                (search "could not identify the Linux C library" diagnostic))
           "runtime bootstrap rejects unrecognized libc output")))
      (when (probe-file curl-log)
        (delete-file curl-log))
      (multiple-value-bind (output error-output status)
          (run-bootstrap "AUTOLITH_LIBC=musl" "AUTOLITH_TEST_LIBC=musl")
        (declare (ignore error-output))
        (test-assert
         (and (zerop status)
              (search "Using host SBCL 2.6.6 as the Linux bootstrap compiler."
                      output)
              (not (probe-file curl-log)))
         "Linux musl runtime bootstrap uses a validated host compiler"))
      (release-script-tests--write-uname bin "Darwin" "x86_64")
      (multiple-value-bind (output error-output status)
          (run-bootstrap)
        (declare (ignore error-output))
        (test-assert
         (and (zerop status)
              (search "Using host SBCL 2.6.6 as the Darwin bootstrap compiler."
                      output)
              (not (probe-file curl-log))
              (probe-file log))
         "Darwin x86-64 runtime bootstrap uses a validated host compiler"))
      (release-script-tests--write-uname bin "SunOS" "amd64")
      (multiple-value-bind (output error-output status)
          (run-bootstrap)
        (let ((diagnostic (concatenate 'string (or output "")
                                       (or error-output ""))))
          (test-assert
           (and (not (zerop status))
                (search "currently supports Linux x86-64, Linux aarch64, macOS x86-64, macOS arm64, FreeBSD x86-64, NetBSD x86-64, and OpenBSD x86-64"
                        diagnostic))
           "runtime bootstrap rejects unsupported platforms")))
      (release-script-tests--write-uname bin "OpenBSD" "amd64")
      (multiple-value-bind (output error-output status)
          (release-script-tests--run
           (list (namestring bash) (namestring script)
                 (namestring installation))
           :environment
           (list (format nil "PATH=~A" path)
                 "AUTOLITH_SBCL=/no/such/sbcl")
           :ignore-error-status t)
        (let ((diagnostic (concatenate 'string (or output "")
                                       (or error-output ""))))
          (test-assert
           (and (not (zerop status))
                (search "needs a host SBCL on OpenBSD" diagnostic))
           "BSD runtime bootstrap requires a host SBCL"))))
    nil))

(-> release-script-tests--archive-helpers (pathname) null)
(defun release-script-tests--archive-helpers (root)
  "Exercise SHA-256 and GNU tar release helpers."
  (test-assert (not (release-archive--gnu-tar-required-p "Linux"))
               "Linux may use system tar for reproducible archives")
  (dolist (os '("Darwin" "FreeBSD" "NetBSD" "OpenBSD"))
    (test-assert (release-archive--gnu-tar-required-p os)
                 (format nil "~A requires GNU tar for reproducible archives" os)))
  (let ((record (merge-pathnames "release-record" root)))
    (release-archive--write-record
     record
     :version "0.11.0"
     :tag "v0.11.0"
     :commit *release-script-tests-commit*
     :platform "aarch64-linux-musl")
    (test-assert
     (search "platform=aarch64-linux-musl"
             (uiop:read-file-string record))
     "release records preserve the exact archive platform"))
    (let ((missing (merge-pathnames "no-sandbox/" root)))
      (test-assert (null (release-archive--sandbox-helper missing))
                   "sandbox helper lookup is silent for an unrelated source root")
      (let* ((source-directory (symbol-function 'asdf:system-source-directory))
             (source-root (asdf:system-source-directory :autolith))
             (library-root (merge-pathnames "sandbox-library/" root))
             (helper (merge-pathnames "build/cl-exec-sandbox-helper" library-root))
             (process-group (merge-pathnames "build/cl-exec-sandbox-process-group" library-root)))
        (release-script-tests--write-file helper "fixture helper")
        (release-script-tests--write-file process-group "fixture process-group helper")
        (test-call-with-function-replacements
         (list
          (list 'asdf:system-source-directory
                (lambda (system)
                  (if (eq system ':cl-exec-sandbox)
                      library-root
                      (funcall source-directory system)))))
         (lambda ()
           (test-assert
            (equal
             (platform-truename *platform* (release-archive--sandbox-helper source-root))
             (platform-truename *platform* helper))
            "sandbox helper lookup uses the locked ASDF dependency")
           (test-assert
            (equal
             (platform-truename *platform* (release-archive--process-group-helper source-root))
             (platform-truename *platform* process-group))
            "process-group helper lookup uses the locked ASDF dependency"))))
      (test-assert (null (release-archive--process-group-helper missing))
                   "process-group helper lookup is silent for an unrelated source root")
      (with-test-environment
          (("AUTOLITH_RELEASE_PROCESS_GROUP_HELPER"
            (namestring (merge-pathnames "release-record" root))))
        (test-assert
         (equal (platform-truename *platform* (release-archive--process-group-helper missing))
                (platform-truename *platform* (merge-pathnames "release-record" root)))
         "process-group helper lookup honours its environment override")))
  (let* ((bin (merge-pathnames "sha256-only/" root))
         (empty (merge-pathnames "no-digest/" root))
         (sha256 (merge-pathnames "sha256" bin))
         (saved (or (uiop:getenv "PATH") "")))
    (uiop:ensure-all-directories-exist (list bin empty))
    (release-script-tests--write-file sha256 "#!/bin/sh\nexit 0\n")
    (release-script-tests--chmod "755" sha256)
    (unwind-protect
         (progn
           (platform-setenv "PATH" (string-right-trim "/" (namestring bin)))
           (test-assert (string= (release-archive--sha256-command) "sha256")
                        "sha256 helper accepts sha256")
           (platform-setenv "PATH" (string-right-trim "/" (namestring empty)))
           (test-assert
            (handler-case
                (progn
                  (release-archive--sha256-command)
                  nil)
              (release-archive-error (condition)
                (and (eq (release-archive-error-stage condition) ':prerequisites)
                     (search "sha256sum, shasum, or sha256 is required."
                             (release-archive-error-cause condition)))))
            "sha256 helper names every accepted digest command"))
        (platform-setenv "PATH" saved)))
    (test-assert
     (equal *release-archive-extra-command-directories*
            '("/usr/local/bin" "/usr/pkg/bin" "/opt/local/bin" "/bin" "/usr/bin"))
     "gnu tar extra directories include BSD prefixes")
    (let* ((bin (merge-pathnames "gnu-tar/" root))
           (extra (merge-pathnames "gnu-tar-extra/" root))
           (empty (merge-pathnames "gnu-tar-empty/" root))
           (gtar (merge-pathnames "gtar" bin))
           (gnutar (merge-pathnames "gnutar" extra))
           (saved (or (uiop:getenv "PATH") "")))
      (uiop:ensure-all-directories-exist (list bin extra empty))
      (release-script-tests--write-file gtar "#!/bin/sh\nexit 0\n")
      (release-script-tests--write-file gnutar "#!/bin/sh\nexit 0\n")
      (release-script-tests--chmod "755" gtar)
      (release-script-tests--chmod "755" gnutar)
      (unwind-protect
           (progn
             (platform-setenv "PATH" (string-right-trim "/" (namestring bin)))
             (test-assert
              (equal (namestring (truename (release-archive--gnu-tar-command)))
                     (namestring (truename gtar)))
              "gnu tar lookup finds gtar on PATH")
             (platform-setenv "PATH" (string-right-trim "/" (namestring empty)))
             (let ((*release-archive-extra-command-directories*
                     (list (string-right-trim "/" (namestring extra)))))
               (test-assert
                (equal (namestring (truename (release-archive--gnu-tar-command)))
                       (namestring (truename gnutar)))
                "gnu tar lookup finds gnutar outside PATH"))
             (let ((*release-archive-extra-command-directories* '()))
               (test-assert (null (release-archive--gnu-tar-command))
                            "gnu tar lookup is silent when gtar is absent"))
             (platform-setenv "PATH" (string-right-trim "/" (namestring bin)))
             (let ((command
                     (release-archive--tar-command
                      (merge-pathnames "release.tar" root)
                      root
                      "autolith-v0.0.0-x86_64-openbsd"
                      "0")))
               (test-assert (and (stringp (first command))
                                 (plusp (length (first command))))
                            "tar command starts with a command pathname")
               (when (release-archive--gnu-tar-required-p (software-type))
                 (test-assert
                   (equal (truename (first command)) (truename gtar))
                  "tar command uses the discovered GNU tar pathname"))))
          (platform-setenv "PATH" saved)))
    nil)

(-> release-script-tests--linux-release-validator (pathname pathname) null)
(defun release-script-tests--linux-release-validator (source-root root)
  "Exercise fail-closed Linux release artifact validation boundaries."
  (let* ((validator
           (merge-pathnames "script/validate-linux-release-artifact" source-root))
         (fixture-root
           (uiop:ensure-directory-pathname
            (merge-pathnames "linux-release-validator/" root)))
         (release-name "autolith-v0.11.0-x86_64-linux")
         (release-root
           (merge-pathnames (format nil "~A/" release-name) fixture-root))
         (release-record (merge-pathnames "RELEASE" release-root))
         (runtime (merge-pathnames "runtime/bin/sbcl" release-root))
         (fixture-bin (merge-pathnames "fixture-bin/" fixture-root))
         (file-command (merge-pathnames "file" fixture-bin))
         (readelf-command (merge-pathnames "readelf" fixture-bin))
         (environment
           (list
            (format nil "PATH=~A:~A"
                    (string-right-trim "/" (namestring fixture-bin))
                    (or (uiop:getenv "PATH") "")))))
    (release-script-tests--write-file
     runtime
     "#!/bin/sh
set -eu
static_smoke=false
for argument do
  [ \"$argument\" = --script ] && static_smoke=true
done
if [ \"$static_smoke\" = true ]; then
  [ \"$AUTOLITH_FFF_LIBRARY\" = \"$0\" ]
  [ \"$COLORLISP_NATIVE_LIBRARY\" = \"$0\" ]
  printf 'Static native smoke test passed.\\n'
else
  printf '%s' \"${AUTOLITH_TEST_RUNTIME_VERSION:-2.6.6}\"
fi
")
    (release-script-tests--write-file
     file-command
     "#!/bin/sh
printf '%s\\n' \"${AUTOLITH_TEST_FILE_DESCRIPTION:-ELF 64-bit LSB pie executable, x86-64}\"
")
    (release-script-tests--write-file
     readelf-command
     "#!/bin/sh
case $1 in
  -h) printf '  Machine: %s\\n' \"${AUTOLITH_TEST_MACHINE:-Advanced Micro Devices X86-64}\" ;;
  -l) [ \"${AUTOLITH_TEST_STATIC_HEADERS:-0}\" = 1 ] || printf '      [Requesting program interpreter: %s]\\n' \"${AUTOLITH_TEST_INTERPRETER:-/lib64/ld-linux-x86-64.so.2}\" ;;
  -d) [ -z \"${AUTOLITH_TEST_NEEDED:-}\" ] || printf ' 0x0000000000000001 (NEEDED) Shared library: [%s]\\n' \"$AUTOLITH_TEST_NEEDED\" ;;
esac
")
    (dolist (pathname (list runtime file-command readelf-command))
      (release-script-tests--chmod "755" pathname))
    (labels ((archive-path (case-name &optional (name release-name))
               (merge-pathnames
                (format nil "~A/~A.tar.gz" case-name name)
                fixture-root))

             (make-archive (case-name &key extra-top-level
                                           (name release-name))
               (let ((archive (archive-path case-name name)))
                 (ensure-directories-exist archive)
                 (release-script-tests--run
                  (append (list "tar" "-czf" (namestring archive)
                                "-C" (namestring fixture-root) name)
                          (when extra-top-level
                            (list extra-top-level)))
                  :output nil)
                 archive))

             (validate (archive &key extra-environment)
               (release-script-tests--run
                (list (namestring validator) (namestring archive)
                      "x86_64-linux" "x86_64"
                      "/lib64/ld-linux-x86-64.so.2")
                :environment (append extra-environment environment)
                :ignore-error-status t))

              (validate-static (archive &key extra-environment
                                             (platform "x86_64-linux"))
                (release-script-tests--run
                 (list (namestring validator) (namestring archive)
                       platform "x86_64" "static")
                 :environment (append extra-environment environment)
                 :ignore-error-status t))

             (assert-failure (archive description &key extra-environment)
               (multiple-value-bind (output error-output status)
                   (validate archive :extra-environment extra-environment)
                 (declare (ignore output error-output))
                 (test-assert (not (zerop status)) description))))
      (let ((missing (archive-path "missing"))
            (unsupported
              (release-script-tests--write-file
               (archive-path "unsupported") "")))
        (assert-failure
         missing "Linux artifact validation rejects a missing archive")
        (multiple-value-bind (output error-output status)
            (release-script-tests--run
             (list (namestring validator) (namestring unsupported)
                   "sparc64-linux" "sparc64" "/lib/ld-linux.so.2")
             :ignore-error-status t)
          (declare (ignore output error-output))
          (test-assert (not (zerop status))
                       "Linux artifact validation rejects an unsupported architecture")))
      (release-script-tests--record
       release-record "v0.11.0" :platform "x86_64-linux")
      (let ((valid (make-archive "valid")))
        (multiple-value-bind (output error-output status)
            (validate valid)
          (declare (ignore output error-output))
          (test-assert (zerop status)
                       "Linux artifact validation accepts a matching archive"))
        (assert-failure
         valid "Linux artifact validation rejects a mismatched ELF machine"
         :extra-environment '("AUTOLITH_TEST_MACHINE=SPARC V9"))
        (assert-failure
         valid "Linux artifact validation rejects a mismatched interpreter"
         :extra-environment '("AUTOLITH_TEST_INTERPRETER=/wrong/loader"))
        (assert-failure
         valid "Linux artifact validation rejects a malformed runtime version"
         :extra-environment '("AUTOLITH_TEST_RUNTIME_VERSION=2.6")))
      (release-script-tests--write-file
       (merge-pathnames "rogue" fixture-root) "outside release root")
      (assert-failure
       (make-archive "extra-top-level" :extra-top-level "rogue")
       "Linux artifact validation rejects extra top-level members")
      (release-script-tests--record
       release-record "v0.11.0" :platform "x86_64-linux")
      (with-open-file (stream release-record
                              :direction ':output
                              :if-exists ':append)
        (write-line "platform=aarch64-linux" stream))
      (assert-failure
       (make-archive "duplicate-platform")
       "Linux artifact validation rejects duplicate platform fields")
      (release-script-tests--record
       release-record "v0.11.0" :platform "aarch64-linux")
      (assert-failure
       (make-archive "mismatched-platform")
       "Linux artifact validation rejects mismatched platform metadata")
      (release-script-tests--record
       release-record "v0.11.0" :platform "x86_64-linux-musl")
      (multiple-value-bind (output error-output status)
          (validate-static
           (make-archive "mismatched-static-root")
           :platform "x86_64-linux-musl"
           :extra-environment
           '("AUTOLITH_TEST_FILE_DESCRIPTION=ELF 64-bit LSB executable, x86-64, statically linked"
             "AUTOLITH_TEST_STATIC_HEADERS=1"))
        (declare (ignore output error-output))
        (test-assert (not (zerop status))
                     "static validation rejects a mismatched archive root"))
      (release-script-tests--record
       release-record "v0.11.0" :platform "x86_64-linux")
      (let* ((musl-name "autolith-v0.11.0-x86_64-linux-musl")
             (musl-root
               (merge-pathnames (format nil "~A/" musl-name) fixture-root))
             (musl-record (merge-pathnames "RELEASE" musl-root)))
        (release-script-tests--run
         (list "cp" "-RPp" (namestring release-root) (namestring musl-root))
         :output nil)
        (release-script-tests--record
         musl-record "v0.11.0" :platform "x86_64-linux-musl")
        (let ((static (make-archive "static" :name musl-name)))
          (multiple-value-bind (output error-output status)
              (validate-static
               static
               :platform "x86_64-linux-musl"
               :extra-environment
               '("AUTOLITH_TEST_FILE_DESCRIPTION=ELF 64-bit LSB executable, x86-64, statically linked"
                 "AUTOLITH_TEST_STATIC_HEADERS=1"))
            (declare (ignore error-output))
            (test-assert (zerop status)
                         "Linux artifact validation accepts a static musl archive")
            (test-assert (search "Static native smoke test passed." output)
                         "static Linux validation exercises the native smoke test"))
          (multiple-value-bind (output error-output status)
              (validate-static
               static
               :platform "x86_64-linux-musl"
               :extra-environment
               '("AUTOLITH_TEST_FILE_DESCRIPTION=ELF 64-bit LSB executable, x86-64, statically linked"
                 "AUTOLITH_TEST_STATIC_HEADERS=1"
                 "AUTOLITH_TEST_NEEDED=libc.so"))
            (declare (ignore output error-output))
            (test-assert (not (zerop status))
                         "static Linux validation rejects dynamic dependencies")))
        (release-script-tests--run
         (list "ln" "-s" "/bin/sh"
               (namestring (merge-pathnames "escaped-runtime" musl-root)))
         :output nil)
        (multiple-value-bind (output error-output status)
            (validate-static
             (make-archive "static-escaping-link" :name musl-name)
             :platform "x86_64-linux-musl"
             :extra-environment
             '("AUTOLITH_TEST_FILE_DESCRIPTION=ELF 64-bit LSB executable, x86-64, statically linked"
               "AUTOLITH_TEST_STATIC_HEADERS=1"))
          (declare (ignore output error-output))
          (test-assert (not (zerop status))
                       "static validation rejects escaping symbolic links")))
    nil)))


(-> release-script-tests--launcher-preflight (pathname) null)
(defun release-script-tests--launcher-preflight (source-root)
  "Check shared launcher argument handling against the live Clingon catalog."
  (let ((helper (namestring (merge-pathnames "script/launcher-cli.sh" source-root))))
    (labels ((run-preflight (arguments &optional (kind "source"))
               (release-script-tests--run
                (append
                 (list "bash" "-c"
                       "set -eu; source \"$1\"; shift; autolith_launcher_parse \"$@\"; printf '%s\\n' \"$recovery_requested\" \"$from_source_requested\" \"$update_requested\"; if [ \"${#remaining_arguments[@]}\" -gt 0 ]; then printf '%s\\n' \"${remaining_arguments[@]}\"; fi"
                       "launcher-test" helper kind)
                 arguments)
                :ignore-error-status t))

             (check-forwarding (arguments &key recovery-p from-source-p forwarded)
               (multiple-value-bind (output error-output status)
                   (run-preflight arguments)
                 (test-assert
                  (and (zerop status)
                       (string= output
                                (format nil "~A~%~A~%false~%~{~A~%~}"
                                        (if recovery-p "true" "false")
                                        (if from-source-p "true" "false")
                                        forwarded)))
                  (format nil "launcher forwards ~S without consuming values: ~A ~A"
                          arguments output error-output))))

             (check-command (command)
               (dolist (option (clingon:command-options command))
                 (when (clingon:option-parameter option)
                   (dolist (name (remove nil
                                        (list
                                         (when (clingon:option-long-name option)
                                           (format nil "--~A"
                                                   (clingon:option-long-name option)))
                                         (when (clingon:option-short-name option)
                                           (format nil "-~A"
                                                   (clingon:option-short-name option))))))
                     (dolist (value '("--recovery" "--from-source" "--update"
                                      "update" "uninstall" "--"))
                       (let ((arguments (list name value)))
                         (check-forwarding arguments :forwarded arguments))))))
               (dolist (child (clingon:command-sub-commands command))
                 (check-command child))))
      (check-command (main--top-level-command))
      (check-forwarding nil)
      (dolist (arguments '(("--" "--recovery" "--from-source" "--update")
                           ("resume" "update")
                           ("resume" "uninstall")
                           ("--" "uninstall")
                           ("--image=--recovery" "resume" "two words")
                           ("-i--from-source" "resume")
                           ("--" "update")))
        (check-forwarding arguments :forwarded arguments))
      (check-forwarding '("--immutable" "--from-source" "resume" "saved")
                        :from-source-p t :forwarded '("--immutable" "resume" "saved"))
      (check-forwarding '("--pristine" "resume" "saved")
                        :from-source-p t
                        :forwarded '("--pristine" "resume" "saved"))
      (check-forwarding '("resume" "saved" "--from-source")
                        :from-source-p t :forwarded '("resume" "saved"))
      (check-forwarding '("--recovery" "--original-argument" "--from-source"
                          "--" "--recovery")
                        :recovery-p t
                        :forwarded '("--original-argument" "--from-source"
                                     "--" "--recovery"))
      (dolist (kind '("source" "nix"))
        (dolist (alias '("update" "--update"))
          (dolist (tail '(nil ("--") ("extra") ("--help" "extra")
                          ("--" "--help") ("--recovery") ("--from-source")))
            (multiple-value-bind (output error-output status)
                (run-preflight (cons alias tail) kind)
              (test-assert
               (and (= status 64) (zerop (length output))
                    (plusp (length error-output)))
               "unmanaged updates and invalid arguments exit before startup"))))
        (dolist (tail '(("--help") ("-h") ("--help" "--")))
          (let ((results
                  (loop for alias in '("update" "--update")
                        collect (multiple-value-list
                                 (run-preflight (cons alias tail) kind)))))
            (test-assert
             (and (equal (first results) (second results))
                  (zerop (third (first results)))
                  (plusp (length (first (first results)))))
             "update aliases provide the same successful help without startup")))
        (dolist (tail '(("extra") ("--yes" "extra") ("--" "--yes") ("--recovery")
                        ("--from-source") ("resume")))
          (multiple-value-bind (output error-output status)
              (run-preflight (cons "uninstall" tail) kind)
            (test-assert
             (and (= status 64) (zerop (length output))
                  (plusp (length error-output)))
             "malformed uninstall arguments exit before startup")))
        (dolist (tail '(("--help") ("-h") ("--yes" "--help") ("--help" "--")))
          (multiple-value-bind (output error-output status)
              (run-preflight (cons "uninstall" tail) kind)
            (declare (ignore error-output))
            (test-assert
             (and (zerop status) (search "Usage: autolith uninstall" output))
             "uninstall help exits successfully without startup")))
        (dolist (tail '(nil ("--yes") ("--yes" "--")))
          (multiple-value-bind (output error-output status)
              (run-preflight (cons "uninstall" tail) kind)
            (declare (ignore error-output))
            (test-assert
             (and (zerop status)
                  (string= output (format nil "false~%false~%false~%uninstall~%~{~A~%~}"
                                          tail)))
             "well-formed uninstall requests parse without acting"))))))
  nil)

(-> release-script-tests--update-dispatch () null)
(defun release-script-tests--update-dispatch ()
  "Verify update catalog discovery and safe dispatch when a launcher is bypassed."
  (let ((started-p nil))
    (test-assert
     (find "update" (clingon:command-sub-commands (main--top-level-command))
           :test #'string= :key #'command-name)
     "update is a discoverable Clingon command")
    (test-call-with-function-replacements
     (list
      (list 'uiop:quit (lambda (status &key urgent)
                         (declare (ignore urgent))
                         (throw 'update-dispatch-exit status)))
      (list 'worker-main (lambda () (setf started-p t)))
      (list 'main--start-session (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (setf started-p t))))
     (lambda ()
       (dolist (alias '("update" "--update"))
         (dolist (tail '(nil ("--") ("extra") ("--unknown")
                         ("--help" "extra") ("--" "--help")
                         ("resume" "saved") ("--recovery") ("--worker")))
           (let ((*standard-output* (make-string-output-stream))
                 (*error-output* (make-string-output-stream)))
             (test-assert
              (= 64 (catch 'update-dispatch-exit
                      (main (cons alias tail))))
              "bypassing a launcher cannot update or start a session")))
         (dolist (tail '(("--help") ("-h") ("--help" "--")))
           (let ((*standard-output* (make-string-output-stream))
                 (*error-output* (make-string-output-stream)))
             (test-assert
              (zerop (catch 'update-dispatch-exit
                       (main-dispatch (cons alias tail))))
              "direct dispatch supports help for both update spellings"))))))
    (test-assert
     (find "uninstall" (clingon:command-sub-commands (main--top-level-command))
           :test #'string= :key #'command-name)
     "uninstall is a discoverable Clingon command")
    (test-call-with-function-replacements
     (list
      (list 'uiop:quit (lambda (status &key urgent)
                         (declare (ignore urgent))
                         (throw 'update-dispatch-exit status)))
      (list 'worker-main (lambda () (setf started-p t)))
      (list 'main--start-session (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (setf started-p t))))
     (lambda ()
       (dolist (tail '(nil ("--yes") ("extra") ("--unknown")))
         (let ((*standard-output* (make-string-output-stream))
               (*error-output* (make-string-output-stream)))
           (test-assert
            (= 64 (catch 'update-dispatch-exit
                    (main (cons "uninstall" tail))))
            "bypassing a launcher cannot uninstall or start a session")))
       (let ((*standard-output* (make-string-output-stream))
             (*error-output* (make-string-output-stream)))
         (test-assert
          (zerop (catch 'update-dispatch-exit
                   (main-dispatch '("uninstall" "--help"))))
          "direct dispatch supports uninstall help"))))
    (test-assert (not started-p) "update and uninstall never fall through to a session"))
  nil)

(-> release-script-tests--models-command () null)
(defun release-script-tests--models-command ()
    "Verify the non-interactive model catalog command."
  (let* ((output (with-output-to-string (*standard-output*)
                    (main-dispatch '("models"))))
         (models (remove-if (lambda (line)
                              (zerop (length line)))
                              (uiop:split-string output :separator #(#\Newline))))
         (sorted (sort (copy-list models) #'string<)))
    (test-assert
     (equal models sorted)
     "models output is sorted")
    (test-assert
     (equal (length models)
            (length (remove-duplicates models :test #'string=)))
     "models output is deduplicated")
    (test-assert
     (member "gpt-5.6-luna" models :test #'string=)
     "models includes catalog entries without authentication")
    (test-assert
     (every (lambda (model)
              (and (plusp (length model))
                   (not (find #\Return model))))
            models)
     "models emits one plain identifier per line"))
  nil)

(-> test-image-manifest-relocation () null)
(defun test-image-manifest-relocation ()
  "Verify published core paths, preserved metadata, and prepublication validation."
  (with-test-configuration (configuration)
    (let* ((root (test-configuration-root configuration))
           (stage (merge-pathnames "stage/" root))
           (destination (merge-pathnames "published/" root))
           (active (merge-pathnames "active/manifest.sexp" stage))
           (recovery (merge-pathnames "recovery/manifest.sexp" stage))
           (active-record
             (list ':sbcl-generations-image-manifest :version 1
                   :core (namestring (merge-pathnames "active/autolith-active.core" stage))
                   :source '(:commit "fixture" :files ("a.lisp" "b.lisp"))))
           (recovery-record
             (list ':recovery-image :version 2
                   :core (namestring (merge-pathnames "recovery/autolith-recovery.core" stage))
                   :implementation "SBCL" :source '(:commit "fixture")))
           (command
             (list (lisp-worker-sbcl-command) "--noinform" "--no-sysinit" "--no-userinit"
                   "--disable-debugger" "--script"
                   (namestring (asdf:system-relative-pathname
                                :autolith "script/relocate-image-manifests.lisp"))
                   (namestring stage) (namestring destination))))
      (labels ((write-record (pathname record)
                 "Replace PATHNAME with a readable fixture RECORD."
                 (when (probe-file pathname)
                   (delete-file pathname))
                 (release-script-tests--write-file
                  pathname (with-standard-io-syntax (prin1-to-string record))))

               (run ()
                 "Run the publication script and return its status."
                 (nth-value 2 (uiop:run-program command :output ':string
                                                       :error-output ':string
                                                       :ignore-error-status t))))
        (dolist (invalid (list (cons ':wrong-tag (rest recovery-record))
                              (list ':recovery-image :version 99 :core "wrong.core")
                              (list ':recovery-image :version 2 :core "wrong.core")))
          (write-record active active-record)
          (write-record recovery invalid)
          (test-assert (not (zerop (run))) "invalid staged manifests fail publication")
          (test-assert
           (equal active-record (read-one-form (uiop:read-file-string active) :read-eval nil))
           "validate both manifests before changing either"))
        (dolist (invalid (list "(:recovery-image :version 2) (:extra)"
                              "#.(error \"Reader evaluation is forbidden\")"))
          (write-record active active-record)
          (release-script-tests--write-file recovery invalid)
          (test-assert (not (zerop (run))) "reject multiple forms and reader evaluation")
          (test-assert
           (equal active-record (read-one-form (uiop:read-file-string active) :read-eval nil))
           "invalid manifest syntax fails before changing paths"))
        (write-record active active-record)
        (write-record recovery recovery-record)
        (dolist (pathname (list active recovery))
          (release-script-tests--chmod "444" pathname))
        (test-assert (zerop (run)) "replace read-only manifests before directory publication")
        (loop for pathname in (list active recovery)
              for original in (list active-record recovery-record)
              for relative in '("active/autolith-active.core" "recovery/autolith-recovery.core")
              for expected = (copy-tree original)
              do (setf (getf (rest expected) :core)
                       (namestring (merge-pathnames relative destination)))
                 (test-assert
                  (equal expected (read-one-form (uiop:read-file-string pathname) :read-eval nil))
                  "publish the final core path and preserve all other manifest fields")))))
  nil)

(-> test-release-scripts () null)
(defun test-release-scripts ()
  "Test shell bootstrap boundaries through Common Lisp fixtures."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-script-tests-~A/" (make-identifier))
             (uiop:temporary-directory)))))
    (unwind-protect
         (progn
              (release-script-tests--syntax source-root)
              (release-script-tests--launcher-preflight source-root)
              (release-script-tests--update-dispatch)
              (release-script-tests--models-command)
              (release-script-tests--bootstrap-dependency-order source-root root)
              (release-script-tests--darwin-fff-library-path
               source-root root)
              (release-script-tests--linux-release-validator source-root root)
              (release-script-tests--runtime-adapter source-root root)
              (release-script-tests--runtime-bootstrap source-root root)
              (release-script-tests--source-launcher source-root root)
              (release-script-tests--platform-ids)
              (release-script-tests--archive-helpers root)
              (release-script-tests--portable-copy root)
              (release-script-tests--checksum-format root)
              (release-script-tests--launcher source-root root)
              (release-script-tests--launcher-darwin source-root root)
              (release-script-tests--launcher-bsd source-root root)
              (release-script-tests--update-handoff source-root root)
              (release-script-tests--musl-update-handoff source-root root)
              (release-script-tests--uninstall source-root root)
              (release-script-tests--image-marker-platform source-root root)
              (release-script-tests--installer source-root root)
              (release-script-tests--installer-platform-identity source-root root)
              (release-script-tests--installer-darwin source-root root)
              (release-script-tests--installer-bsd source-root root))
      (release-script-tests--cleanup root)))
  nil)
