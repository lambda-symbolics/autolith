;;;; Runtime provisioning shared by the PowerShell launchers.

;;; bin/autolith-runtime.ps1 selects an SBCL that satisfies sbcl.version and
;;; runs this script in it. The script mirrors the rest of bin/autolith-runtime:
;;; it records the runtime command for later launches, installs the matching
;;; SBCL source tree when asked, exports AUTOLITH_SBCL and
;;; AUTOLITH_SBCL_SOURCE_ROOT, and then loads the requested Lisp entry point
;;; with the remaining arguments as its command line.
;;;
;;; Usage: sbcl --script script/runtime.lisp [--install] --script PATH [ARGUMENT...]

(require :asdf)
(require :sb-posix)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))
(load (merge-pathnames "runtime-requirement.lisp"
                       (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *runtime-source-archive-base-url*
  "https://downloads.sourceforge.net/project/sbcl/sbcl"
  "The directory holding the official SBCL source archives.")

(defun runtime-fail (control &rest arguments)
  "Report a runtime setup failure and exit with status 1."
  (format *error-output* "~&Autolith runtime setup failed: ~?~%" control arguments)
  (finish-output *error-output*)
  (uiop:quit 1))

(defun runtime-trimmed (string)
  "Return STRING without surrounding whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun runtime-first-line (pathname)
  "Return the first line of PATHNAME without its line ending, or NIL."
  (with-open-file (stream pathname :direction :input :if-does-not-exist nil
                                   :external-format :utf-8)
    (when stream
      (let ((line (read-line stream nil nil)))
        (and line (runtime-trimmed line))))))

(defun runtime-hex-digest-p (string)
  "Return true when STRING is a lowercase or uppercase SHA-256 hex digest."
  (and (= (length string) 64)
       (every (lambda (character) (digit-char-p character 16)) string)))

(defun runtime-source-checksum (checksums-pathname version)
  "Return the tracked SHA-256 of SBCL VERSION's source archive, or NIL."
  (let ((archive-name (format nil "sbcl-~A-source.tar.bz2" version)))
    (with-open-file (stream checksums-pathname :direction :input :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((fields (remove "" (uiop:split-string (runtime-trimmed line)
                                                           :separator " ")
                                     :test #'string=)))
                 (when (and (= (length fields) 2)
                            (string= (second fields) archive-name)
                            (runtime-hex-digest-p (first fields)))
                   (return (string-downcase (first fields)))))))))

(defun runtime-program-available-p (program)
  "Return true when PROGRAM can be found on the search path."
  (and (uiop:run-program (list (if (uiop:os-windows-p) "where" "command")
                               (if (uiop:os-windows-p) program "-v")
                               (if (uiop:os-windows-p) "" program))
                         :output nil :error-output nil :ignore-error-status t
                         :force-shell (not (uiop:os-windows-p)))
       t))

(defun runtime-command-available-p (program)
  "Return true when PROGRAM runs from the search path."
  (handler-case
      (zerop (uiop:wait-process
              (uiop:launch-program (list program "--version")
                                   :output nil :error-output nil :input nil)))
    (error ()
      nil)))

(defun runtime-file-sha256 (pathname)
  "Return PATHNAME's SHA-256 through the host's checksum tool, or NIL."
  (let ((native (uiop:native-namestring pathname)))
    (dolist (command (if (uiop:os-windows-p)
                         (list (list "certutil" "-hashfile" native "SHA256"))
                         (list (list "sha256sum" "--" native)
                               (list "shasum" "-a" "256" "--" native))))
      (let ((output (ignore-errors
                     (uiop:run-program command :output :string :error-output nil
                                               :ignore-error-status t))))
        (when output
          (dolist (line (uiop:split-string output :separator '(#\Newline #\Return)))
            (let ((token (remove #\Space (runtime-trimmed line))))
              (when (runtime-hex-digest-p token)
                (return-from runtime-file-sha256 (string-downcase token)))
              (let ((first-field (first (uiop:split-string (runtime-trimmed line)
                                                           :separator " "))))
                (when (and first-field (runtime-hex-digest-p first-field))
                  (return-from runtime-file-sha256 (string-downcase first-field)))))))))
    nil))

(defun runtime-version-file-matches-p (pathname version)
  "Return true when PATHNAME holds the line \"VERSION\" with its quotes."
  (with-open-file (stream pathname :direction :input :if-does-not-exist nil
                                   :external-format :utf-8)
    (and stream
         (loop for line = (read-line stream nil nil)
               while line
               thereis (string= (runtime-trimmed line)
                                (format nil "\"~A\"" version))))))

(defun runtime-source-available-p (managed-source identity-pathname version expected-sha256)
  "Return true when MANAGED-SOURCE holds the verified SBCL VERSION source tree."
  (and expected-sha256
       (uiop:directory-exists-p managed-source)
       (probe-file (merge-pathnames "version.lisp-expr" managed-source))
       (let ((identity (runtime-first-line identity-pathname)))
         (and identity
              (let ((fields (uiop:split-string identity :separator " ")))
                (and (= (length fields) 2)
                     (string= (first fields) version)
                     (string-equal (second fields) expected-sha256)))))
       (runtime-version-file-matches-p (merge-pathnames "version.lisp-expr" managed-source)
                                       version)))

(defun runtime-set-tree-writable (directory writable-p)
  "Make every file below DIRECTORY writable, or read-only when WRITABLE-P is false."
  (uiop:run-program
   (if (uiop:os-windows-p)
       (list "attrib" (if writable-p "-R" "+R")
             (format nil "~A*" (uiop:native-namestring directory)) "/S" "/D")
       (list "chmod" "-R" (if writable-p "u+w" "a-w") (uiop:native-namestring directory)))
   :output nil :error-output nil :ignore-error-status t))

(defun runtime-publish-private-file (pathname content)
  "Write CONTENT to PATHNAME through a temporary file, replacing any earlier file."
  (let ((temporary (make-pathname :name (format nil ".~A.~D" (pathname-name pathname)
                                                (sb-posix:getpid))
                                  :type (pathname-type pathname)
                                  :defaults pathname)))
    (with-open-file (stream temporary :direction :output :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
      (write-line content stream))
    (autolith-script-replace-file temporary pathname)))

(defun runtime-install-source (runtime-root managed-source identity-pathname
                               version expected-sha256)
  "Download, verify, and publish the SBCL VERSION source tree below RUNTIME-ROOT."
  (unless (autolith-version-components version)
    (runtime-fail "SBCL ~A is not an official release with a published source archive; set AUTOLITH_SBCL to a release build."
                  version))
  (unless expected-sha256
    (runtime-fail "SBCL ~A has no tracked source archive identity." version))
  (dolist (program '("curl" "tar"))
    (unless (runtime-command-available-p program)
      (runtime-fail "automatic source installation needs ~A." program)))
  (ensure-directories-exist runtime-root)
  (let* ((temporary (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil ".source.~D/" (sb-posix:getpid))
                                      runtime-root)))
         (archive-name (format nil "sbcl-~A-source.tar.bz2" version))
         (archive (merge-pathnames archive-name temporary))
         (distribution (merge-pathnames (format nil "sbcl-~A/" version) temporary)))
    (unwind-protect
         (progn
           (uiop:delete-directory-tree temporary :validate t :if-does-not-exist :ignore)
           (ensure-directories-exist archive)
           (format *error-output* "~&Installing matching SBCL ~A source for Autolith.~%"
                   version)
           (finish-output *error-output*)
           (uiop:run-program (list "curl" "--fail" "--location" "--show-error"
                                   "--retry" "3" "--progress-bar"
                                   "--proto" "=https" "--tlsv1.2"
                                   "--output" (uiop:native-namestring archive)
                                   (format nil "~A/~A/~A" *runtime-source-archive-base-url*
                                           version archive-name))
                             :output :interactive :error-output :interactive)
           (let ((actual (runtime-file-sha256 archive)))
             (unless actual
               (runtime-fail "automatic source installation needs a SHA-256 tool."))
             (unless (string-equal actual expected-sha256)
               (runtime-fail "the downloaded SBCL source archive has the wrong SHA-256 identity.")))
           (uiop:run-program (list "tar" "-xf" (uiop:native-namestring archive)
                                   "-C" (uiop:native-namestring temporary))
                             :output :interactive :error-output :interactive)
           (unless (and (probe-file (merge-pathnames "version.lisp-expr" distribution))
                        (probe-file (merge-pathnames "src/code/list.lisp" distribution)))
             (runtime-fail "the SBCL source archive has an unexpected layout."))
           (delete-file archive)
           (let ((stale (merge-pathnames (format nil "source.stale.~D/" (sb-posix:getpid))
                                         runtime-root)))
             (when (uiop:directory-exists-p managed-source)
               (rename-file managed-source stale))
             (handler-case
                 (rename-file distribution managed-source)
               (error ()
                 (when (uiop:directory-exists-p stale)
                   (rename-file stale managed-source))
                 (runtime-fail "the matching SBCL source could not be published.")))
             (runtime-set-tree-writable managed-source nil)
             (when (uiop:directory-exists-p stale)
               (runtime-set-tree-writable stale t)
               (uiop:delete-directory-tree stale :validate t :if-does-not-exist :ignore)))
           (runtime-publish-private-file identity-pathname
                                         (format nil "~A ~A" version expected-sha256)))
      (when (uiop:directory-exists-p temporary)
        (runtime-set-tree-writable temporary t)
        (uiop:delete-directory-tree temporary :validate t :if-does-not-exist :ignore)))))

(let* ((arguments (uiop:command-line-arguments))
       (install-p nil)
       (script nil)
       (script-arguments nil))
  (loop while arguments
        do (let ((argument (pop arguments)))
             (cond
               ((string= argument "--install")
                (setf install-p t))
               ((string= argument "--script")
                (unless arguments
                  (runtime-fail "--script needs a pathname."))
                (setf script (pop arguments)
                      script-arguments arguments
                      arguments nil))
               (t
                (runtime-fail "unknown argument ~A." argument)))))
  (unless script
    (runtime-fail "no Lisp script was provided."))
  (unless (probe-file script)
    (runtime-fail "Lisp script ~A does not exist." script))
  (let* ((source-root (uiop:pathname-parent-directory-pathname
                       (uiop:pathname-directory-pathname (truename *load-truename*))))
         (version-pathname (merge-pathnames "sbcl.version" source-root))
         (checksums-pathname (merge-pathnames "sbcl-source-releases.sha256" source-root))
         (version (lisp-implementation-version))
         (runtimes-root (merge-pathnames "runtimes/" (autolith-application-root :data)))
         (runtime-command (uiop:native-namestring sb-ext:*runtime-pathname*)))
    (handler-case
        (autolith-require-minimum-runtime version-pathname)
      (error (condition)
        (runtime-fail "~A" condition)))
    (unless (probe-file checksums-pathname)
      (runtime-fail "sbcl-source-releases.sha256 is unavailable."))
    (ensure-directories-exist runtimes-root)
    (runtime-publish-private-file (merge-pathnames "command" runtimes-root) runtime-command)
    (let* ((runtime-root (merge-pathnames (format nil "~A/" version) runtimes-root))
           (managed-source (merge-pathnames "source/" runtime-root))
           (identity-pathname (merge-pathnames "source.identity" runtime-root))
           (expected-sha256 (and (autolith-version-components version)
                                 (runtime-source-checksum checksums-pathname version))))
      (when (and install-p
                 (not (runtime-source-available-p managed-source identity-pathname
                                                  version expected-sha256)))
        ;; Only the pinned release has a verified source archive. A newer
        ;; SBCL still satisfies the minimum, so it runs Autolith; workers
        ;; then report that matching implementation source is unavailable.
        (if expected-sha256
            (runtime-install-source runtime-root managed-source identity-pathname
                                    version expected-sha256)
            (format *error-output*
                    "~&SBCL ~A has no tracked source archive; implementation source stays unavailable to Lisp workers until the release pinned in sbcl.version is used.~%"
                    version)))
      (autolith-script-setenv "AUTOLITH_SBCL" runtime-command)
      (if (runtime-source-available-p managed-source identity-pathname version expected-sha256)
          (autolith-script-setenv "AUTOLITH_SBCL_SOURCE_ROOT"
                                  (uiop:native-namestring managed-source))
          (autolith-script-unsetenv "AUTOLITH_SBCL_SOURCE_ROOT")))
    ;; The entry point sees only its own arguments, as it would under --script.
    (setf sb-ext:*posix-argv* (cons (first sb-ext:*posix-argv*) script-arguments))
    (load (truename script))))
