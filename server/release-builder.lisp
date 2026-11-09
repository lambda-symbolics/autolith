(in-package #:autolith)

;;;; -- Release Builder Configuration --

(defparameter *release-builder-default-poll-seconds* 300
  "The default delay between remote release-tag checks.")

(defparameter *release-builder-default-repository*
  "https://github.com/lambda-symbolics/autolith.git"
  "The public source repository inspected for release tags.")

(defparameter *release-builder-container-image*
  "autolith-release-builder:ubuntu-22.04"
  "The local container image used for portable release builds.")

(defparameter *release-builder-default-container-timeout-seconds* 1800
  "The maximum runtime of one isolated release build and check.")

(defvar *release-builder-command-function* #'uiop:run-program
  "The command runner used by the release builder and replaced by tests.")

(defclass release-builder-configuration ()
  ((source-root
    :initarg       :source-root
    :reader        release-builder-configuration-source-root
    :type          pathname
    :documentation "The deployed checkout containing the builder container files.")
   (state-root
    :initarg       :state-root
    :reader        release-builder-configuration-state-root
    :type          pathname
    :documentation "The private root for checkouts, caches, and staging files.")
   (public-root
    :initarg       :public-root
    :reader        release-builder-configuration-public-root
    :type          pathname
    :documentation "The root receiving complete atomic release publications.")
   (repository
    :initarg       :repository
    :reader        release-builder-configuration-repository
    :type          string
    :documentation "The Git repository queried for release tags.")
   (poll-seconds
    :initarg       :poll-seconds
    :reader        release-builder-configuration-poll-seconds
    :type          (integer 1)
    :documentation "The delay after each remote-tag scan.")
   (container-command
    :initarg       :container-command
    :reader        release-builder-configuration-container-command
    :type          string
    :documentation "The container engine executable used for isolated builds.")
   (container-timeout-seconds
    :initarg       :container-timeout-seconds
    :reader        release-builder-configuration-container-timeout-seconds
    :type          (integer 1)
    :documentation "The maximum seconds allowed for one container build and check."))
  (:documentation "Private settings for the automatic release builder."))

(defclass release-source-tag ()
  ((name
    :initarg       :name
    :reader        release-source-tag-name
    :type          string
    :documentation "The semantic Git tag name.")
   (commit
    :initarg       :commit
    :reader        release-source-tag-commit
    :type          string
    :documentation "The exact forty-character Git commit identity."))
  (:documentation "One immutable remote source tag selected for building."))

(define-condition release-builder-error (error)
  ((stage
    :initarg       :stage
    :reader        release-builder-error-stage
    :type          keyword
    :documentation "The builder stage that failed.")
   (tag
    :initarg       :tag
    :initform      nil
    :reader        release-builder-error-tag
    :type          (option string)
    :documentation "The release tag being built, when one was selected.")
   (cause
    :initarg       :cause
    :reader        release-builder-error-cause
    :type          t
    :documentation "The underlying failure condition or diagnostic value."))
  (:report
   (lambda (condition stream)
     (format stream "Release builder failed during ~(~A~)~@[ for ~A~]: ~A"
             (release-builder-error-stage condition)
             (release-builder-error-tag condition)
             (release-builder-error-cause condition))))
  (:documentation "A structured automatic release build failure."))

(-> release-builder--positive-integer
    ((option string) integer string)
    (integer 1))
(defun release-builder--positive-integer (value fallback name)
  "Parse positive integer VALUE, using FALLBACK when absent and naming NAME."
  (if (null value)
      fallback
      (let ((parsed (handler-case (parse-integer value)
                      (parse-error ()
                        nil))))
        (if (and parsed (plusp parsed))
            parsed
            (error 'configuration-error
                   :message (format nil "Invalid ~A ~S." name value))))))

(-> release-host--call-with-lock (pathname function) t)
(defun release-host--call-with-lock (state-root function)
  "Call FUNCTION while holding the release host's process-shared operation lock."
  (let* ((pathname
           (merge-pathnames "host-operation.lock"
                            (uiop:ensure-directory-pathname state-root)))
         (descriptor nil))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (setf descriptor
                 (sb-posix:open
                  (namestring pathname)
                  (logior sb-posix:o-creat sb-posix:o-rdwr)
                  #o600))
           (sb-posix:lockf descriptor sb-posix:f-lock 0)
           (funcall function))
      (when descriptor
        (handler-case (sb-posix:lockf descriptor sb-posix:f-ulock 0)
          (sb-posix:syscall-error ()
            nil))
        (handler-case (sb-posix:close descriptor)
          (sb-posix:syscall-error ()
            nil))))))

(-> release-builder-configuration-create
    (&key (:source-root (option pathname))
          (:state-root (option pathname))
          (:public-root (option pathname))
          (:repository (option string))
          (:poll-seconds (option integer))
          (:container-command (option string))
          (:container-timeout-seconds (option integer)))
    release-builder-configuration)
(defun release-builder-configuration-create
    (&key source-root state-root public-root repository poll-seconds
          container-command container-timeout-seconds)
  "Create automatic release builder settings from arguments and environment."
  (make-instance
   'release-builder-configuration
   :source-root
   (uiop:ensure-directory-pathname
    (or source-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_SOURCE_ROOT")))
          (and configured (pathname configured)))
        (asdf:system-source-directory :autolith)))
   :state-root
   (uiop:ensure-directory-pathname
    (or state-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_STATE_ROOT")))
          (and configured (pathname configured)))
        #p"/var/lib/autolith-release-server/"))
   :public-root
   (uiop:ensure-directory-pathname
    (or public-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_PUBLIC_ROOT")))
          (and configured (pathname configured)))
        #p"/srv/autolith-release-server/"))
   :repository        (or repository
                          (uiop:getenv "AUTOLITH_RELEASE_REPOSITORY")
                          *release-builder-default-repository*)
   :poll-seconds
   (or poll-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_POLL_SECONDS")
        *release-builder-default-poll-seconds*
        "release poll interval"))
   :container-command (or container-command
                          (uiop:getenv "AUTOLITH_RELEASE_CONTAINER_COMMAND")
                          "docker")
   :container-timeout-seconds
   (or container-timeout-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_CONTAINER_TIMEOUT_SECONDS")
        *release-builder-default-container-timeout-seconds*
        "release container timeout"))))


;;;; -- Remote Tags --

(-> release-builder--commit-valid-p (string) boolean)
(defun release-builder--commit-valid-p (commit)
  "Return true when COMMIT is a lowercase forty-character Git identity."
  (and (= (length commit) 40)
       (not
        (null
         (every (lambda (character)
                  (or (digit-char-p character)
                      (find character "abcdef")))
                commit)))))

(-> release-builder--parse-remote-tags (string) list)
(defun release-builder--parse-remote-tags (output)
  "Parse Git ls-remote OUTPUT into semantic release source tags.

Annotated tags publish both the tag object and a peeled commit line ending in
^{}. Prefer the peeled commit so checkout validation matches HEAD after clone.
Lightweight tags only publish one line and keep that commit identity."
  (let ((commits (make-hash-table :test #'equal))
        (peeled  (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string output :separator '(#\Newline #\Return)))
      (unless (zerop (length line))
        (let* ((separator (position #\Tab line))
               (commit    (and separator (subseq line 0 separator)))
               (reference (and separator (subseq line (1+ separator))))
               (prefix    "refs/tags/")
               (suffix    "^{}")
               (tag       nil)
               (peeled-p  nil))
          (when (and reference (uiop:string-prefix-p prefix reference))
            (let ((name (subseq reference (length prefix))))
              (when (and (>= (length name) (length suffix))
                         (string= name suffix
                                  :start1 (- (length name) (length suffix))))
                (setf peeled-p t
                      name (subseq name 0 (- (length name) (length suffix)))))
              (setf tag name)))
          (unless (and commit
                       tag
                       (release-builder--commit-valid-p commit)
                       (release-tag-valid-p tag))
            (error 'release-builder-error
                   :stage ':tag-discovery
                   :cause (format nil "Malformed remote tag line ~S." line)))
          (if peeled-p
              (setf (gethash tag commits) commit
                    (gethash tag peeled) t)
              (unless (gethash tag peeled)
                (setf (gethash tag commits) commit))))))
    (sort
     (loop for tag being the hash-keys of commits using (hash-value commit)
           collect (make-instance 'release-source-tag
                                  :name   tag
                                  :commit commit))
     #'release-tag<
     :key #'release-source-tag-name)))

(-> release-builder-remote-tags (release-builder-configuration) list)
(defun release-builder-remote-tags (configuration)
  "Return semantic release tags currently published by the source remote."
  (handler-case
      (release-builder--parse-remote-tags
       (uiop:run-program
        ;; Omit --refs so annotated tags also emit peeled ^{} commit lines.
        (list "git" "ls-remote" "--tags"
              (release-builder-configuration-repository configuration)
              "v*.*.*")
        :output       ':string
        :error-output ':output))
    (release-builder-error (condition)
      (error condition))
    (error (cause)
      (error 'release-builder-error
             :stage ':tag-discovery
             :cause cause))))

(-> release-builder-pending-tags
    (release-builder-configuration list)
    list)
(defun release-builder-pending-tags (configuration remote-tags)
  "Return REMOTE-TAGS newer than publication state and needing a build.

An empty publication root starts at the newest remote tag instead of rebuilding
historical releases. Once one release exists, every newer tag is built in
semantic order so a temporary builder outage cannot skip a version."
  (let* ((server-configuration
           (release-server-configuration-create
            :source-root (release-builder-configuration-source-root configuration)
            :public-root (release-builder-configuration-public-root configuration)))
         (published (release-server-published-tags server-configuration))
         (latest    (first (last published))))
    (if latest
        (remove-if-not
         (lambda (source-tag)
           (and (release-tag< latest (release-source-tag-name source-tag))
                (not
                 (release-server--release-complete-p
                  server-configuration
                  (release-source-tag-name source-tag)))))
         remote-tags)
        (last remote-tags))))


;;;; -- Source Checkout --

(-> release-builder--checkout-path
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--checkout-path (configuration source-tag)
  "Return the private reusable checkout path for SOURCE-TAG."
  (merge-pathnames
   (format nil "checkouts/~A/" (release-source-tag-name source-tag))
   (release-builder-configuration-state-root configuration)))

(-> release-builder--git-output (pathname list) string)
(defun release-builder--git-output (directory arguments)
  "Return trimmed output from one Git command in DIRECTORY."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:run-program (append (list "git" "-C" (namestring directory)) arguments)
                     :output       ':string
                     :error-output ':output)))

(-> release-builder--checkout-valid-p
    (pathname release-source-tag)
    boolean)
(defun release-builder--checkout-valid-p (checkout source-tag)
  "Return true when CHECKOUT is clean and exactly matches SOURCE-TAG."
  (handler-case
      (and (uiop:directory-exists-p checkout)
           (string= (release-builder--git-output checkout '("rev-parse" "HEAD"))
                    (release-source-tag-commit source-tag))
           (zerop
            (length
             (release-builder--git-output
              checkout '("status" "--porcelain" "--untracked-files=no")))))
    (error ()
      nil)))

(-> release-builder--source-version (pathname) string)
(defun release-builder--source-version (checkout)
  "Read CHECKOUT's version through ASDF in an isolated Lisp process.

The checkout may use ASDF's file-backed version or an older literal. Loading
its definition outside this image preserves the running server's system registry."
  (handler-case
      (let ((version
              (string-trim
               '(#\Newline #\Return)
               (uiop:run-program
                (list (active-image--sbcl-command)
                      "--noinform" "--no-sysinit" "--no-userinit"
                      "--non-interactive"
                      "--eval" "(require :asdf)"
                      "--eval"
                      "(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))"
                      "--eval"
                      (format nil
                              "(progn (asdf:load-asd ~S) (write-line (asdf:component-version (asdf:find-system ~S))))"
                              (namestring (merge-pathnames "autolith.asd" checkout))
                              "autolith"))
                :output       ':string
                :error-output ':string))))
        (unless (release-tag-valid-p (format nil "v~A" version))
          (error 'release-builder-error
                 :stage ':source-validation
                 :cause "ASDF did not report one semantic release version."))
        version)
    (error (condition)
      (error 'release-builder-error
             :stage ':source-validation
             :cause condition))))


;;;; -- Container Build --

(-> release-builder--staging-path
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--staging-path (configuration source-tag)
  "Return SOURCE-TAG's private release output staging directory."
  (merge-pathnames
   (format nil "staging/~A/" (release-source-tag-name source-tag))
   (release-builder-configuration-state-root configuration)))

(-> release-builder--container-volume (pathname string) string)
(defun release-builder--container-volume (pathname target)
  "Return one Docker bind-mount argument from PATHNAME to TARGET."
  (format nil "~A:~A" (namestring (truename pathname)) target))

(-> release-builder--clear-shared-source-fasl-cache (pathname) null)
(defun release-builder--clear-shared-source-fasl-cache (state-root)
  "Delete cached ASDF FASLs shared by release builds at /source.
Every checkout uses the same container path and cache, so stale FASLs could
otherwise be reused for a different release tag."
  (let ((cache-root (merge-pathnames "cache/common-lisp/" state-root)))
    (when (uiop:directory-exists-p cache-root)
      (dolist (implementation (uiop:subdirectories cache-root))
        (let ((source-cache (merge-pathnames "source/" implementation)))
          (when (uiop:directory-exists-p source-cache)
            (uiop:delete-directory-tree source-cache
                                        :validate          t
                                        :if-does-not-exist ':ignore)
            (format t "~&Cleared shared source fasl cache at ~A.~%"
                    source-cache)
            (finish-output))))))
  nil)

(-> release-builder--container-name (release-source-tag) string)
(defun release-builder--container-name (source-tag)
  "Return a unique managed container name for SOURCE-TAG."
  (format nil "autolith-release-builder-~A-~D"
          (release-source-tag-name source-tag)
          (sb-posix:getpid)))

(-> release-builder--container-diagnostics
    (release-builder-configuration string)
    null)
(defun release-builder--container-diagnostics (configuration container-name)
  "Print bounded process and log diagnostics for managed CONTAINER-NAME."
  (dolist (arguments
           (list (list "top" container-name
                       "-eo" "pid,ppid,pgid,sid,user,stat,wchan:32,etime,cmd")
                 (list "logs" "--tail" "200" container-name)))
    ;; mallet:suppress no-ignore-errors -- diagnostics must never mask the build failure
    (ignore-errors
      (funcall *release-builder-command-function*
               (cons
                (release-builder-configuration-container-command configuration)
                arguments)
               :output       ':interactive
               :error-output ':interactive)))
  nil)

(-> release-builder--remove-container
    (release-builder-configuration string)
    null)
(defun release-builder--remove-container (configuration container-name)
  "Force-remove managed CONTAINER-NAME after a failed or timed-out build."
  ;; mallet:suppress no-ignore-errors -- best-effort cleanup after a build that already failed
  (ignore-errors
    (funcall *release-builder-command-function*
             (list (release-builder-configuration-container-command configuration)
                   "rm" "--force" container-name)
             :output       ':interactive
             :error-output ':interactive))
  nil)

(-> release-builder--run-container
    (release-builder-configuration release-source-tag pathname)
    pathname)
(defun release-builder--run-container (configuration source-tag checkout)
  "Build and check SOURCE-TAG in the pinned container, returning its output."
  (let* ((state          (merge-pathnames "container/"
                                          (release-builder-configuration-state-root
                                           configuration)))
         (staging        (release-builder--staging-path configuration source-tag))
         (container-name (release-builder--container-name source-tag))
         (container-command
           (release-builder-configuration-container-command configuration)))
    (when (uiop:directory-exists-p staging)
      (uiop:delete-directory-tree staging
                                  :validate          t
                                  :if-does-not-exist ':ignore))
    (uiop:ensure-all-directories-exist (list state staging))
    (release-builder--clear-shared-source-fasl-cache state)
    (handler-case
        (funcall
         *release-builder-command-function*
         (list "timeout"
               "--signal=TERM"
               "--kill-after=30"
               (princ-to-string
                (release-builder-configuration-container-timeout-seconds
                 configuration))
               container-command
               "run" "--name" container-name "--rm" "--privileged"
               "--volume" (release-builder--container-volume checkout "/source")
               "--volume" (release-builder--container-volume state "/state")
               "--volume" (release-builder--container-volume staging "/output")
               *release-builder-container-image*)
         :output       ':interactive
         :error-output ':interactive)
      (error (cause)
        (release-builder--container-diagnostics configuration container-name)
        (release-builder--remove-container configuration container-name)
        (error 'release-builder-error
               :stage ':container-build
               :tag   (release-source-tag-name source-tag)
               :cause cause)))
    staging))


;;;; -- GitHub Release Mirror --

(-> release-builder--string-prefix-p (string string) boolean)
(defun release-builder--string-prefix-p (prefix value)
  "Return true when VALUE begins with PREFIX."
  (and (>= (length value) (length prefix))
       (string= prefix value :end2 (length prefix))
       t))

(-> release-builder--string-suffix-p (string string) boolean)
(defun release-builder--string-suffix-p (suffix value)
  "Return true when VALUE ends with SUFFIX."
  (and (>= (length value) (length suffix))
       (string= suffix value :start2 (- (length value) (length suffix)))
       t))

(-> release-builder--strip-git-suffix (string) string)
(defun release-builder--strip-git-suffix (value)
  "Remove a trailing .git suffix from VALUE."
  (if (release-builder--string-suffix-p ".git" value)
      (subseq value 0 (- (length value) 4))
      value))

(-> release-builder--split-owner-repo (string) (values string string))
(defun release-builder--split-owner-repo (path)
  "Split PATH into GitHub owner and repository names."
  (let ((parts (remove "" (uiop:split-string path :separator '(#\/))
                       :test #'string=)))
    (unless (= (length parts) 2)
      (error 'release-builder-error
             :stage ':tag-discovery
             :cause (format nil "Repository path ~S is not owner/repo." path)))
    (values (first parts) (second parts))))

(-> release-builder--github-repository (string) (values string string))
(defun release-builder--github-repository (repository)
  "Return OWNER and REPO parsed from a GitHub REPOSITORY URL."
  (let ((value
          (release-builder--strip-git-suffix
           (string-right-trim '(#\/) repository))))
    (cond
      ((release-builder--string-prefix-p "https://github.com/" value)
       (release-builder--split-owner-repo
        (subseq value (length "https://github.com/"))))
      ((release-builder--string-prefix-p "http://github.com/" value)
       (release-builder--split-owner-repo
        (subseq value (length "http://github.com/"))))
      ((release-builder--string-prefix-p "git@github.com:" value)
       (release-builder--split-owner-repo
        (subseq value (length "git@github.com:"))))
      ((release-builder--string-prefix-p "ssh://git@github.com/" value)
       (release-builder--split-owner-repo
        (subseq value (length "ssh://git@github.com/"))))
      (t
       (error 'release-builder-error
              :stage ':tag-discovery
              :cause (format nil "Repository ~S is not a GitHub URL."
                             repository))))))

(-> release-builder--github-asset-url (string string &key (:tag string) (:name string)) string)
(defun release-builder--github-asset-url (owner repo &key tag name)
  "Return the public GitHub download URL for one release asset."
  (format nil "https://github.com/~A/~A/releases/download/~A/~A"
          owner repo tag name))

(-> release-builder--try-download (string pathname) boolean)
(defun release-builder--try-download (url destination)
  "Download URL to DESTINATION and return true when a non-empty file arrives."
  (handler-case
      (progn
        (funcall *release-builder-command-function*
                   (list "curl" "--fail" "--silent" "--location" "--show-error" "--retry" "3"
                         "--proto" "=https" "--tlsv1.2"
                       "--output" (namestring destination)
                       url)
                 :output       ':interactive
                 :error-output ':interactive)
        (and (uiop:file-exists-p destination)
             (plusp
              (with-open-file (stream destination
                                      :direction    ':input
                                      :element-type '(unsigned-byte 8))
                (file-length stream)))
             t))
    (error ()
      (when (uiop:file-exists-p destination)
        (delete-file destination))
      nil)))

(-> release-builder--linux-artifacts-present-p (pathname string) boolean)
(defun release-builder--linux-artifacts-present-p (directory tag)
  "Return true when DIRECTORY contains TAG's Linux archive and checksum."
  (multiple-value-bind (archive checksum)
      (release-builder--artifact-pathnames directory tag)
    (and (uiop:file-exists-p archive)
         (uiop:file-exists-p checksum)
         t)))

(-> release-builder--published-p
    (release-builder-configuration string)
    boolean)
(defun release-builder--published-p (configuration tag)
  "Return true when TAG is already Linux-complete in the public root."
  (release-server--release-complete-p
   (release-server-configuration-create
    :source-root (release-builder-configuration-source-root configuration)
    :public-root (release-builder-configuration-public-root configuration))
   tag))

(-> release-builder--published-source-tags
    (release-builder-configuration list)
    list)
(defun release-builder--published-source-tags (configuration remote-tags)
  "Return REMOTE-TAGS that already have a complete Linux publication."
  (remove-if-not
   (lambda (source-tag)
     (release-builder--published-p configuration
                                   (release-source-tag-name source-tag)))
   remote-tags))

(-> release-builder--fetch-github-assets
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--fetch-github-assets (configuration source-tag)
  "Download TAG's available GitHub archives and checksums into a staging directory."
  (let* ((tag     (release-source-tag-name source-tag))
         (staging (release-builder--staging-path configuration source-tag))
         (target
           (release-server--release-directory
            (release-server-configuration-create
             :source-root (release-builder-configuration-source-root configuration)
             :public-root (release-builder-configuration-public-root configuration))
            tag)))
    (multiple-value-bind (owner repo)
        (release-builder--github-repository
         (release-builder-configuration-repository configuration))
      (when (uiop:directory-exists-p staging)
        (uiop:delete-directory-tree staging
                                    :validate          t
                                    :if-does-not-exist ':ignore))
      (uiop:ensure-all-directories-exist (list staging))
      (dolist (platform *release-server-platform-ids*)
        (let* ((archive-name  (release-server--archive-name tag platform))
               (checksum-name (format nil "~A.sha256" archive-name))
               (archive       (merge-pathnames archive-name staging))
               (checksum      (merge-pathnames checksum-name staging)))
          (when (or (not (uiop:file-exists-p (merge-pathnames archive-name target)))
                    (not (uiop:file-exists-p (merge-pathnames checksum-name target))))
            (when (and (release-builder--try-download
                        (release-builder--github-asset-url owner repo :tag  tag
                                                                      :name archive-name)
                        archive)
                       (release-builder--try-download
                        (release-builder--github-asset-url owner repo :tag  tag
                                                                      :name checksum-name)
                        checksum))
              (format t "~&Fetched ~A.~%" archive-name)
              (finish-output)))))
      staging)))

(-> release-builder--files-identical-p (pathname pathname) boolean)
(defun release-builder--files-identical-p (left right)
  "Return true when LEFT and RIGHT contain the same bytes."
  (with-open-file (left-stream left
                               :direction    ':input
                               :element-type '(unsigned-byte 8))
    (with-open-file (right-stream right
                                  :direction    ':input
                                  :element-type '(unsigned-byte 8))
      (and (= (file-length left-stream) (file-length right-stream))
           (loop for left-byte = (read-byte left-stream nil nil)
                 for right-byte = (read-byte right-stream nil nil)
                 always (eql left-byte right-byte)
                 while left-byte)
           t))))


;;;; -- Atomic Publication --

(-> release-builder--artifact-pathnames
    (pathname string)
    (values pathname pathname))
(defun release-builder--artifact-pathnames (directory tag)
  "Return TAG's archive and checksum pathnames below DIRECTORY."
  (let ((archive (merge-pathnames (release-server--archive-name tag) directory)))
    (values archive
            (merge-pathnames
             (format nil "~A.sha256" (file-namestring archive))
             directory))))

(-> release-builder--staging-files (pathname string) list)
(defun release-builder--staging-files (directory tag)
  "Return archive/checksum pathnames present in DIRECTORY for TAG."
  (loop for platform in *release-server-platform-ids*
        for archive-name = (release-server--archive-name tag platform)
        for checksum-name = (format nil "~A.sha256" archive-name)
        for archive = (merge-pathnames archive-name directory)
        for checksum = (merge-pathnames checksum-name directory)
        when (or (uiop:file-exists-p archive)
                 (uiop:file-exists-p checksum))
          collect (list archive checksum archive-name)))

(-> release-builder--checksum-matches-p (pathname pathname) boolean)
(defun release-builder--checksum-matches-p (archive checksum)
  "Verify CHECKSUM's single GNU record names ARCHIVE and matches its SHA-256."
  (let* ((name          (file-namestring archive))
         (record-length (+ 66 (length name)))
         (record
           (with-open-file (stream checksum :external-format ':utf-8)
             (when (<= record-length (file-length stream) (+ record-length 2))
               (let ((line (read-line stream nil nil)))
                 (when (and line (null (read-line stream nil nil)))
                   (string-right-trim '(#\Return) line)))))))
    (and record
         (= (length record) record-length)
         (string= record name :start1 66)
         (member (subseq record 64 66) '("  " " *") :test #'string=)
         (string-equal record (release-archive--sha256-digest archive)
                       :end1 64)
         t)))

(-> release-builder--validate-artifacts (pathname string) null)
(defun release-builder--validate-artifacts (directory tag)
  "Require DIRECTORY's fetched archives to match their checksums."
  (let ((files (release-builder--staging-files directory tag)))
    (unless files
      (error 'release-builder-error
             :stage ':artifact-validation
             :tag   tag
             :cause "No GitHub release archives were fetched."))
    (dolist (entry files)
      (destructuring-bind (archive checksum archive-name) entry
        (unless (and (uiop:file-exists-p archive)
                     (uiop:file-exists-p checksum)
                     (plusp
                      (with-open-file (stream archive
                                              :direction    ':input
                                              :element-type '(unsigned-byte 8))
                        (file-length stream))))
          (error 'release-builder-error
                 :stage ':artifact-validation
                 :tag   tag
                 :cause (format nil "~A is missing its archive or checksum."
                                archive-name)))
        (unless (handler-case
                    (release-builder--checksum-matches-p archive checksum)
                  (error (cause)
                    (error 'release-builder-error
                           :stage ':artifact-validation
                           :tag   tag
                           :cause cause)))
          (error 'release-builder-error
                 :stage ':artifact-validation
                 :tag   tag
                 :cause (format nil "~A has an invalid checksum." archive-name))))))
  nil)

(-> release-builder--install-artifact (pathname pathname) pathname)
(defun release-builder--install-artifact (source destination)
  "Copy SOURCE to DESTINATION and freeze it as a published artifact."
  (uiop:copy-file source destination)
  (sb-posix:chmod (namestring destination) #o444)
  destination)

(-> release-builder--merge-artifacts (pathname pathname string) pathname)
(defun release-builder--merge-artifacts (staging target tag)
  "Add new STAGING artifacts into TARGET without changing existing files."
    (sb-posix:chmod (namestring target) #o755)
    (unwind-protect
         (dolist (entry (release-builder--staging-files staging tag))
           (destructuring-bind (archive checksum archive-name) entry
             (let ((published-archive (merge-pathnames archive-name target))
                   (published-checksum
                     (merge-pathnames (format nil "~A.sha256" archive-name) target)))
               (cond
                 ((and (uiop:file-exists-p published-archive)
                       (uiop:file-exists-p published-checksum))
                  (unless (and (release-builder--files-identical-p
                                archive published-archive)
                               (release-builder--files-identical-p
                                checksum published-checksum))
                    (error 'release-builder-error
                           :stage ':publication
                           :tag   tag
                           :cause (format nil "~A already exists with different contents."
                                          archive-name))))
                 ((or (uiop:file-exists-p published-archive)
                      (uiop:file-exists-p published-checksum))
                  (error 'release-builder-error
                         :stage ':publication
                         :tag   tag
                         :cause (format nil "~A is only partially published."
                                        archive-name)))
                 (t
                  (release-builder--install-artifact archive published-archive)
                  (release-builder--install-artifact checksum published-checksum)
                  (format t "~&Added ~A to Autolith ~A.~%" archive-name tag)
                  (finish-output))))))
      (sb-posix:chmod (namestring target) #o555))
    target)

(-> release-builder-publish
    (release-builder-configuration pathname string)
    pathname)
(defun release-builder-publish (configuration staging tag)
  "Validate STAGING and publish or merge TAG, returning its directory."
  (release-builder--validate-artifacts staging tag)
  (let* ((releases-root
           (merge-pathnames "releases/"
                            (release-builder-configuration-public-root
                             configuration)))
         (target (merge-pathnames (format nil "~A/" tag) releases-root))
         (temporary
           (merge-pathnames
            (format nil ".~A.~D/" tag (sb-posix:getpid))
            releases-root)))
    (when (uiop:directory-exists-p target)
      (return-from release-builder-publish
        (release-builder--merge-artifacts staging target tag)))
    (unless (release-builder--linux-artifacts-present-p staging tag)
      (error 'release-builder-error
             :stage ':publication
             :tag   tag
             :cause "The Linux archive is required for first publication."))
    (when (uiop:directory-exists-p temporary)
      (uiop:delete-directory-tree temporary
                                  :validate          t
                                  :if-does-not-exist ':ignore))
    (uiop:ensure-all-directories-exist (list temporary))
    (dolist (entry (release-builder--staging-files staging tag))
      (destructuring-bind (archive checksum archive-name) entry
        (release-builder--install-artifact
         archive (merge-pathnames archive-name temporary))
        (release-builder--install-artifact
         checksum (merge-pathnames (format nil "~A.sha256" archive-name)
                                   temporary))))
    (sb-posix:chmod (namestring temporary) #o555)
    (rename-file temporary target)
    (format t "~&Published Autolith ~A at ~A.~%" tag target)
    (finish-output)
    target))


;;;; -- Builder Loop --

(-> release-builder-build
    (release-builder-configuration release-source-tag)
    (option pathname))
(defun release-builder-build (configuration source-tag)
  "Fetch GitHub assets for SOURCE-TAG and publish or merge them."
  (let* ((tag         (release-source-tag-name source-tag))
         (staging     (release-builder--fetch-github-assets configuration source-tag))
         (published-p (release-builder--published-p configuration tag)))
    (cond
      ((release-builder--linux-artifacts-present-p staging tag)
       (release-builder-publish configuration staging tag))
      (published-p
       (if (release-builder--staging-files staging tag)
           (release-builder-publish configuration staging tag)
           (release-server--release-directory
            (release-server-configuration-create
             :source-root (release-builder-configuration-source-root configuration)
             :public-root (release-builder-configuration-public-root configuration))
            tag)))
      (t
       (format t "~&Waiting for the GitHub Linux archive of Autolith ~A.~%" tag)
       (finish-output)
       nil))))

(-> release-builder-build-pending (release-builder-configuration) list)
(defun release-builder-build-pending (configuration)
  "Mirror newly tagged and already published GitHub releases."
  (release-host--call-with-lock
   (release-builder-configuration-state-root configuration)
   (lambda ()
     (let ((remote-tags (release-builder-remote-tags configuration)))
       (loop for source-tag in
             (append (release-builder-pending-tags configuration remote-tags)
                     (release-builder--published-source-tags
                      configuration remote-tags))
             for published = (release-builder-build configuration source-tag)
             when published
               collect published)))))

(-> release-builder-run (release-builder-configuration) null)
(defun release-builder-run (configuration)
  "Poll forever, building and publishing newly tagged releases."
  (format t "~&Autolith release builder polling ~A every ~D seconds.~%"
          (release-builder-configuration-repository configuration)
          (release-builder-configuration-poll-seconds configuration))
  (finish-output)
  (loop
    (handler-case
        (release-builder-build-pending configuration)
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)))
    (sleep (release-builder-configuration-poll-seconds configuration))))
