(require :asdf)
(require :sb-posix)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defun fff--environment-directory (variable fallback)
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let* ((value    (uiop:getenv variable))
         (pathname (and value
                        (plusp (length value))
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

#+darwin
(defun fff--prepare-darwin-build-environment ()
  "Ensure Darwin SDK library paths are available in LIBRARY_PATH."
  (let ((sdk (handler-case
                 (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:run-program '("xcrun" "--show-sdk-path")
                                                :output              ':string
                                                :ignore-error-status t))
               (error ()
                 nil))))
    (when (and sdk (plusp (length sdk)) (probe-file sdk))
      (let* ((sdk-directory
               (uiop:ensure-directory-pathname (pathname sdk)))
             (sdk-lib
               (namestring
                (uiop:merge-pathnames* "usr/lib/" sdk-directory)))
             (previous-library-path (or (uiop:getenv "LIBRARY_PATH") ""))
             (entries
               (uiop:split-string previous-library-path :separator ":")))
        (unless (member sdk-lib entries :test #'string=)
          (sb-posix:setenv "LIBRARY_PATH"
                           (if (plusp (length previous-library-path))
                               (format nil "~A:~A"
                                       previous-library-path sdk-lib)
                               sdk-lib)
                           1))))))

(defun fff--prepare-build-environment ()
  "Prepare the build environment for host-specific C dependency quirks.

  LMDB uses SysV semaphores on BSD and only defines union semun when
  _SEM_SEMUN_UNDEFINED is set. glibc defines that macro. NetBSD does
  not define it and also omits the union, so the flag is required there.
  OpenBSD 7.9 already provides union semun, so the flag would redefine it."
  #+netbsd
  (let ((previous (or (uiop:getenv "CFLAGS") "")))
    (unless (search "-D_SEM_SEMUN_UNDEFINED" previous)
      (sb-posix:setenv "CFLAGS"
                       (string-trim
                        '(#\Space)
                        (format nil "~A -D_SEM_SEMUN_UNDEFINED" previous))
                       1)))
  #+darwin
  (fff--prepare-darwin-build-environment)
  nil)

(destructuring-bind (&optional option artifact &rest ignored) (uiop:command-line-arguments)
  (declare (ignore ignored))
  (load (if (equal option "--artifact")
            (uiop:parse-native-namestring artifact)
            (let ((root (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname *load-truename*))))
              (load (merge-pathnames ".qlot/setup.lisp" root))
              (merge-pathnames "source/artifact.lisp"
                               (asdf:system-source-directory "clifff"))))))

(let* ((script-path         (truename *load-truename*))
       (script-directory    (uiop:pathname-directory-pathname script-path))
       (source-root         (uiop:pathname-parent-directory-pathname script-directory))
       (commit-pathname     (merge-pathnames "native/fff/commit" source-root))
       (commit              (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (uiop:read-file-string commit-pathname)))
       (checkout
         (merge-pathnames (format nil "build/fff/~A/" commit)
                          (autolith-application-root :cache)))
       (install-root        (merge-pathnames "native/fff/"
                                             (autolith-application-root :data)))
       (library-name        (uiop:symbol-call :clifff :fff-library-file-name))
       (library             (merge-pathnames library-name install-root))
       (static-build-p
         (equal (uiop:getenv "AUTOLITH_BUILD_STATIC_NATIVE") "1"))
       (static-library-name "libfff_c.a")
       (static-library      (merge-pathnames static-library-name install-root)))
  (labels ((run (command &key directory (output ':interactive))
             "Run one build COMMAND, keeping diagnostics and the native exit status."
             (handler-case
                 (uiop:run-program command
                                   :directory    directory
                                   :input        ':interactive
                                   :output       output
                                   :error-output ':interactive)
               (uiop:subprocess-error (condition)
                 (format *error-output* "~&~A~%" condition)
                 (finish-output *error-output*)
                 (uiop:quit (uiop:subprocess-error-code condition))))))
      (if (uiop:symbol-call :clifff              :fff-library-current-p library commit
                             :required-artifacts (when static-build-p (list static-library)))
        (format t "~&The pinned private fff library is already installed at ~A.~%"
                library)
        (progn
          (unless (probe-file (merge-pathnames ".git/" checkout))
            (ensure-directories-exist checkout)
            (run (list "git" "init" (namestring checkout)))
            (run (list "git"
                       "-C" (namestring checkout)
                       "remote" "add" "origin"
                       "https://github.com/dmtrKovalenko/fff.git")))
          (format t "~&Fetching fff at ~A.~%" commit)
          (finish-output)
          (run (list "git"
                     "-C" (namestring checkout)
                     "fetch" "--depth" "1" "origin" commit))
          (run (list "git"
                     "-C" (namestring checkout)
                     "checkout" "--detach" "--force" "FETCH_HEAD"))
          (let ((actual
                  (string-trim
                   '(#\Space #\Tab #\Newline #\Return)
                   (run (list "git" "-C" (namestring checkout) "rev-parse" "HEAD")
                        :output ':string))))
            (unless (string= actual commit)
              (error "Fetched fff commit ~A instead of ~A." actual commit)))
          (format t "~&Building fff's C library. The first build can take several minutes.~%")
          (finish-output)
          (fff--prepare-build-environment)
          (run (list "cargo" "build" "--locked" "--release" "-p" "fff-c")
               :directory checkout)
          (when static-build-p
            (run (list "cargo" "rustc" "--locked" "--release"
                       "-p" "fff-c" "--crate-type" "staticlib")
                 :directory checkout))
          (let ((built (merge-pathnames
                        (format nil "target/release/~A" library-name)
                        checkout)))
            (unless (probe-file built)
              (error "fff did not produce ~A." built))
            (ensure-directories-exist library)
            (uiop:symbol-call :clifff :fff-library-publish built library)
            (when static-build-p
              (let ((built-static
                      (merge-pathnames
                       (format nil "target/release/~A" static-library-name)
                       checkout)))
                (unless (probe-file built-static)
                  (error "fff did not produce ~A." built-static))
                (uiop:symbol-call :clifff :fff-library-publish built-static static-library)))
            (uiop:symbol-call :clifff :fff-library-write-manifest library commit)
            (format t "~&Installed the private fff library at ~A.~%" library))))))
