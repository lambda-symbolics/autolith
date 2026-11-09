(in-package #:autolith)

;;;; -- POSIX Platform Adapter --

(defclass posix-platform (platform)
  ()
  (:documentation "The adapter for Linux, macOS, and BSD hosts built on SB-POSIX."))

(defmethod platform-file-uri ((platform posix-platform) pathname)
  "Encode POSIX path bytes, preserving literal backslashes as escaped filename characters."
  (concatenate 'string "file://" (platform--encode-uri-path (uiop:native-namestring pathname))))


;;;; -- Process Observations --

(-> posix--process-query (list) (values (option string) boolean))
(defun posix--process-query (arguments)
  "Run a ps query, returning its output and whether it completed successfully."
  (handler-case
      (multiple-value-bind (output errors status)
          (uiop:run-program (cons "ps" arguments)
                            :output ':string :error-output ':string
                            :ignore-error-status t)
        (declare (ignore errors))
        (values output (eql status 0)))
    (error ()
      (values nil nil))))

(-> posix--unsigned-field (string) (option (integer 0)))
(defun posix--unsigned-field (text)
  "Parse one unsigned decimal field, allowing only surrounding whitespace."
  (let ((field (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (when (and (plusp (length field))
               (every (lambda (character) (digit-char-p character 10)) field))
      (parse-integer field))))

(defmethod platform-process-resident-bytes ((platform posix-platform) process-id)
  "Observe ps RSS in KiB and return octets; unavailable observations return NIL."
  (when (typep process-id '(integer 1))
    (multiple-value-bind (output available-p)
        (posix--process-query (list "-o" "rss=" "-p" (write-to-string process-id)))
      (when available-p
        (let ((kibibytes (posix--unsigned-field output)))
          (when kibibytes
            (* kibibytes 1024)))))))

(-> posix--parse-parent-snapshot (string) (values list boolean))
(defun posix--parse-parent-snapshot (output)
  "Parse complete ps PID/PPID rows, rejecting malformed or duplicate rows."
  (let ((parents nil)
        (seen    (make-hash-table)))
    (dolist (line (uiop:split-string output :separator '(#\Newline)))
      (let ((fields (remove "" (uiop:split-string
                                line :separator '(#\Space #\Tab #\Return))
                            :test #'string=)))
        (when fields
          (unless (= (length fields) 2)
            (return-from posix--parse-parent-snapshot (values nil nil)))
          (let ((pid    (posix--unsigned-field (first fields)))
                (parent (posix--unsigned-field (second fields))))
            (unless (and pid parent (not (gethash pid seen)))
              (return-from posix--parse-parent-snapshot (values nil nil)))
            (setf (gethash pid seen) t)
            (push (cons pid parent) parents)))))
    (values (nreverse parents) t)))

(defmethod platform-process-parent-snapshot ((platform posix-platform))
  "Observe one host-wide ps PID/PPID snapshot, distinguishing failure from empty."
  (multiple-value-bind (output available-p)
      (posix--process-query '("-ax" "-o" "pid=" "-o" "ppid="))
    (if available-p
        (posix--parse-parent-snapshot output)
        (values nil nil))))


;;;; -- Failure Translation --

(defmethod platform-permission-error-code-p ((platform posix-platform) (code integer))
  "Recognize POSIX permission failures without exposing host errno values."
  (not (null (member code (list sb-posix:eacces sb-posix:eperm)))))

(-> posix--failure-reason (integer) platform-failure-reason)
(defun posix--failure-reason (errno)
  "Return the portable failure reason for ERRNO."
  (cond
    ((= errno sb-posix:enoent)
     ':missing)
    ((= errno sb-posix:eexist)
     ':exists)
    ((= errno sb-posix:econnrefused)
     ':refused)
    ((= errno sb-posix:enotdir)
     ':not-directory)
    ((= errno sb-posix:eloop)
     ':symbolic-link)
    (t
     ':failed)))

(-> posix--signal (keyword (option pathname) sb-posix:syscall-error) nil)
(defun posix--signal (operation pathname condition)
  "Signal PLATFORM-ERROR for CONDITION raised by OPERATION on PATHNAME."
  (let ((errno (sb-posix:syscall-errno condition)))
    (error 'platform-error
           :message   (princ-to-string condition)
           :operation operation
           :pathname  pathname
           :reason    (posix--failure-reason errno)
           :code      errno)))

(-> posix--call (keyword (option pathname) function) t)
(defun posix--call (operation pathname function)
  "Call FUNCTION, translating an SB-POSIX failure of OPERATION on PATHNAME."
  (handler-case
      (funcall function)
    (sb-posix:syscall-error (condition)
      (posix--signal operation pathname condition))))

(-> posix--namestring (pathname) string)
(defun posix--namestring (pathname)
  "Return PATHNAME as the native namestring SB-POSIX expects."
  (uiop:native-namestring pathname))

(defmethod platform-ensure-directories-exist ((platform posix-platform) pathname)
  "Create absent parent directories through POSIX so failures retain their errno."
  (platform--ensure-directories-exist
   platform pathname
   (lambda (directory)
     (posix--call ':create directory
                  (lambda () (sb-posix:mkdir (posix--namestring directory) #o777))))))

(defmethod platform-open-new-file ((platform posix-platform) pathname)
  "Create a staging file through POSIX so creation errors retain their errno."
  (posix--call
   ':open pathname
   (lambda ()
     (let ((descriptor (sb-posix:open (posix--namestring pathname)
                                     (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl)
                                     #o666)))
       (unwind-protect
            (prog1 (sb-sys:make-fd-stream descriptor :output t
                                        :element-type        '(unsigned-byte 8)
                                        :file                (posix--namestring pathname)
                                        :pathname pathname :auto-close t)
              (setf descriptor nil))
         (when descriptor
           (sb-posix:close descriptor)))))))


;;;; -- Capabilities and Processes --

(defmethod platform-supports-p ((platform posix-platform) capability)
  "Report POSIX facilities and the hosts supported by the image saver."
  (case capability
    (:forked-image-saver
     (and (member :sbcl *features*)
          (or (member :linux *features*)
              (member :darwin *features*))
          t))
    (otherwise
     (and (member capability '(:local-sockets :process-groups :detached-sessions))
          t))))

(defmethod platform-process-alive-p ((platform posix-platform) process-id)
  "Probe PROCESS-ID with signal zero through ls-compat."
  (process-alive-p process-id))

(defmethod platform-process-group-alive-p ((platform posix-platform)
                                           process-group-id)
  "Probe process group PROCESS-GROUP-ID with signal zero through ls-compat."
  (process-group-alive-p process-group-id))

(defmethod platform-terminate-process ((platform posix-platform) process-id
                                       &key force)
  "Signal PROCESS-ID with SIGTERM, or SIGKILL when FORCE."
  (posix--call ':terminate nil
               (lambda ()
                 (signal-process process-id (if force ':kill ':terminate))))
  nil)

(defmethod platform-terminate-process-group ((platform posix-platform)
                                             process-group-id &key force)
  "Signal process group PROCESS-GROUP-ID with SIGTERM, or SIGKILL when FORCE."
  (posix--call ':terminate nil
               (lambda ()
                 (signal-process-group process-group-id (if force ':kill ':terminate))))
  nil)

(defmethod platform-detach-session ((platform posix-platform))
  "Start a new session with SETSID."
  (posix--call ':detach nil (lambda () (sb-posix:setsid)))
  nil)


;;;; -- Environment --

(defmethod platform-set-environment-variable ((platform posix-platform)
                                              name value)
  "Set or remove NAME with setenv and unsetenv, which C code and children share."
  (declare (ignore platform))
  (if value
      (sb-posix:setenv name value 1)
      (sb-posix:unsetenv name))
  nil)


;;;; -- Files --

(-> posix--environment-directory (string pathname) pathname)
(defun posix--environment-directory (variable fallback)
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let ((value (uiop:getenv variable)))
    (if (and (non-empty-string-p value) (char= (char value 0) #\/))
        (uiop:parse-native-namestring
         (if (char= (char value (1- (length value))) #\/)
             value
             (concatenate 'string value "/")))
        (uiop:ensure-directory-pathname fallback))))
;;; Remove this legacy XDG bridge before 0.68.0.
(define-condition posix-legacy-xdg-root-conflict (error)
  ((variable :initarg :variable :reader posix-legacy-xdg-root-variable
             :documentation "The XDG variable selecting both roots.")
   (native :initarg :native :reader posix-legacy-xdg-root-native
           :documentation "The correctly parsed native application root.")
   (legacy :initarg :legacy :reader posix-legacy-xdg-root-legacy
           :documentation "The application root selected by the old parser."))
  (:documentation "Both native and legacy XDG roots contain installation content.")
  (:report (lambda (condition stream)
             (format stream "Both XDG roots contain content for ~A: ~S and ~S. Choose a root through a restart or resolve the directories while Autolith is stopped. No files were moved."
                     (posix-legacy-xdg-root-variable condition)
                     (uiop:native-namestring (posix-legacy-xdg-root-native condition))
                     (uiop:native-namestring (posix-legacy-xdg-root-legacy condition))))))

(define-condition posix-legacy-xdg-root-relative-link (file-error)
  ()
  (:documentation "A relative root symlink cannot be moved without changing its referent.")
  (:report (lambda (condition stream)
             (format stream "The legacy root ~S is a relative symlink; relocating it would change its target. Set an explicit root or resolve the link while Autolith is stopped."
                     (uiop:native-namestring (file-error-pathname condition))))))

(-> posix--xdg-root-content-p (string) boolean)
(defun posix--xdg-root-content-p (path)
  "Inspect entries without opening files or following links; directory-only trees are empty."
  (handler-case
      (let ((stat (sb-posix:lstat (string-right-trim "/" path))))
        (cond
          ((not (sb-posix:s-isdir (sb-posix:stat-mode stat)))
           t)
          (t
           (let ((directory (sb-posix:opendir path)))
             (unwind-protect
                  (loop for entry = (sb-posix:readdir directory)
                        until (sb-alien:null-alien entry)
                        for name = (sb-posix:dirent-name entry)
                        thereis (and (not (member name '("." "..") :test #'string=))
                                     (posix--xdg-root-content-p
                                      (concatenate 'string path "/" name))))
               (sb-posix:closedir directory))))))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (error condition)))))

(-> posix--xdg-root-stat (pathname) (option sb-posix:stat))
(defun posix--xdg-root-stat (path)
  "Return the root entry's identity without following links, or NIL when absent."
  (handler-case (sb-posix:lstat (string-right-trim "/" (uiop:native-namestring path)))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (error condition)))))

(-> posix--call-with-xdg-migration-lock (pathname function) t)
(defun posix--call-with-xdg-migration-lock (legacy function)
  "Serialize runtime and standalone migration with a persistent source-sibling flock."
  (let* ((parent (uiop:pathname-parent-directory-pathname legacy))
         (lock   (merge-pathnames ".autolith-xdg-migration.lock" parent)))
    (ensure-directories-exist parent)
    (let ((descriptor (sb-posix:open (uiop:native-namestring lock)
                                     (logior sb-posix:o-creat sb-posix:o-rdwr
                                             sb-posix:o-nofollow)
                                     #o600)))
      (unwind-protect
           (progn
             ;; FD_CLOEXEC is 1 on the supported POSIX hosts.
             (sb-posix:fcntl descriptor sb-posix:f-setfd 1)
             (loop for result = (sb-alien:alien-funcall
                                 (sb-alien:extern-alien "flock"
                                   (function sb-alien:int sb-alien:int sb-alien:int))
                                 descriptor 2)
                   until (zerop result)
                   do (unless (= (sb-alien:get-errno) sb-posix:eintr)
                        (error 'sb-posix:syscall-error :name "flock"
                               :errno                        (sb-alien:get-errno))))
             (funcall function))
        (sb-posix:close descriptor)))))

(-> posix--move-xdg-root (pathname pathname) pathname)
(defun posix--move-xdg-root (legacy native)
  "Atomically rename LEGACY to NATIVE, retaining any directory-only scaffold.
The caller holds the sibling migration lock and has checked both roots. Failed
publication leaves LEGACY intact; a displaced scaffold is restored when possible."
  (let ((source      (string-right-trim "/" (uiop:native-namestring legacy)))
        (target      (string-right-trim "/" (uiop:native-namestring native)))
        (backup      nil)
        (container   nil)
        (displaced-p nil)
        (published-p nil))
    (let ((stat (posix--xdg-root-stat legacy)))
      (when (and stat (sb-posix:s-islnk (sb-posix:stat-mode stat))
                 (not (char= (char (sb-posix:readlink source) 0) #\/)))
        (error 'posix-legacy-xdg-root-relative-link :pathname legacy)))
    (ensure-directories-exist (uiop:pathname-parent-directory-pathname native))
    (unwind-protect
         (progn
           (when (posix--xdg-root-content-p target)
             (error 'sb-posix:syscall-error :name "rename" :errno sb-posix:enotempty))
           (when (posix--xdg-root-stat native)
             ;; Keep empty directories and their metadata rather than deleting them.
             (setf container
                   (sb-posix:mkdtemp
                    (concatenate 'string
                                 (uiop:native-namestring
                                  (uiop:pathname-parent-directory-pathname native))
                                 ".autolith-xdg-precreated-XXXXXX"))
                   backup (concatenate 'string container "/autolith"))
             (sb-posix:rename target backup)
             (setf displaced-p t)
             (when (posix--xdg-root-content-p backup)
               (error 'sb-posix:syscall-error :name "rename" :errno sb-posix:enotempty)))
           (sb-posix:rename source target)
           (setf published-p t)
           native)
      (unless published-p
        (when (and displaced-p (not (posix--xdg-root-stat native)))
          (sb-posix:rename backup target))
        (when container
          (sb-posix:rmdir container))))))

(-> posix--legacy-xdg-root (string pathname) pathname)
(defun posix--legacy-xdg-root (variable native)
  "Move a backslash-misparsed installation once, before 0.68.0.
Conflicts require an explicit choice. Rename failures warn and retry next lookup."
  (let* ((value  (uiop:getenv variable))
         (legacy (and value (find #\\ value) (plusp (length value))
                      (char= (char value 0) #\/)
                      (handler-case
                          (let ((base (uiop:ensure-directory-pathname (pathname value))))
                            (when (and (uiop:absolute-pathname-p base)
                                       (not (wild-pathname-p base)))
                              (merge-pathnames "autolith/" base)))
                        (error () nil)))))
    (if (or (null legacy) (equal native legacy)
            (not (posix--xdg-root-content-p (uiop:native-namestring legacy))))
        native
        (handler-case
            (posix--call-with-xdg-migration-lock
             legacy
             (lambda ()
               ;; Another process may have completed the move while we waited.
               (let ((native-stat (posix--xdg-root-stat native))
                     (legacy-stat (posix--xdg-root-stat legacy)))
                 (cond
                   ((and native-stat legacy-stat
                         (= (sb-posix:stat-dev native-stat) (sb-posix:stat-dev legacy-stat))
                         (= (sb-posix:stat-ino native-stat) (sb-posix:stat-ino legacy-stat)))
                    native)
                   ((not (posix--xdg-root-content-p (uiop:native-namestring legacy)))
                    native)
                   ((posix--xdg-root-content-p (uiop:native-namestring native))
                    (setf *legacy-xdg-root-choice-p* t)
                    (restart-case
                        (error 'posix-legacy-xdg-root-conflict
                               :variable variable :native native :legacy legacy)
                      (use-native-root ()
                        :report "Use the literal native XDG root for this lookup."
                        native)
                      (use-legacy-root ()
                        :report "Use the historical XDG root for this lookup."
                        legacy)))
                   (t
                    (posix--move-xdg-root legacy native))))))
          ((or sb-posix:syscall-error file-error) (condition)
            (setf *legacy-xdg-root-choice-p* t)
            (warn "Could not move legacy XDG root ~S to ~S: ~A. Using the legacy root; migration will retry."
                  (uiop:native-namestring legacy) (uiop:native-namestring native) condition)
            legacy)))))

(defmethod platform-application-root ((platform posix-platform) kind)
  "Place each root under its XDG base directory or the XDG default below home."
  (let* ((home     (user-homedir-pathname))
         (variable (ecase kind
                     (:config
                      "XDG_CONFIG_HOME")
                     (:data
                      "XDG_DATA_HOME")
                     (:state
                      "XDG_STATE_HOME")
                     (:cache
                      "XDG_CACHE_HOME")))
         (fallback (merge-pathnames (ecase kind
                                     (:config
                                      ".config/")
                                     (:data
                                      ".local/share/")
                                     (:state
                                      ".local/state/")
                                     (:cache
                                      ".cache/"))
                                   home)))
    (let ((native (merge-pathnames
                   "autolith/" (posix--environment-directory variable fallback))))
      (if (member variable *legacy-xdg-root-bypass* :test #'string=)
          native
          (posix--legacy-xdg-root variable native)))))

(defmethod platform-parse-namestring ((platform posix-platform) string)
  "Read STRING as a Unix namestring, as UIOP does for pathname designators."
  (declare (ignore platform))
  (uiop:parse-unix-namestring string))

(-> posix--mode (pathname) integer)
(defun posix--mode (pathname)
  "Return PATHNAME's current mode bits through ls-compat, following links."
  (posix--call ':protect pathname
               (lambda ()
                 (ls-compat.posix:file-mode pathname))))

(-> posix--change-mode (pathname integer) null)
(defun posix--change-mode (pathname mode)
  "Set PATHNAME's access and special permission bits with CHMOD."
  (posix--call ':protect pathname
               (lambda ()
                 (sb-posix:chmod (posix--namestring pathname) (logand mode #o7777))))
  nil)

(defmethod platform-make-private ((platform posix-platform) pathname
                                  &key read-only-p)
  "Set mode 0700 on a directory, 0400 on a read-only file, and 0600 otherwise."
  (posix--change-mode pathname
                      (cond
                        ((sb-posix:s-isdir (posix--mode pathname))
                         #o700)
                        (read-only-p
                         #o400)
                        (t
                         #o600))))

(defmethod platform-make-read-only ((platform posix-platform) pathname)
  "Set mode 0444 on PATHNAME."
  (posix--change-mode pathname #o444))

(defmethod platform-file-uri-pathname ((platform posix-platform) decoded-path)
  "Parse the decoded absolute POSIX path of a local file URI."
  (uiop:parse-native-namestring decoded-path))

(defmethod platform-file-permissions ((platform posix-platform) pathname)
  "Capture PATHNAME's permission bits."
  (logand #o7777 (posix--mode pathname)))

(defmethod (setf platform-file-permissions) (permissions (platform posix-platform) pathname)
  "Restore captured permission bits with CHMOD."
  (posix--change-mode pathname permissions)
  permissions)

(defmethod platform-delete-file ((platform posix-platform) pathname)
  "Remove one exact native pathname with typed host failure reporting."
  (posix--call ':delete pathname
               (lambda () (sb-posix:unlink (posix--namestring pathname))))
  nil)

(defmethod platform-copy-file-permissions ((platform posix-platform)
                                           source target)
  "Copy SOURCE's permission bits onto TARGET."
  (posix--change-mode target (logand #o7777 (posix--mode source))))

(defmethod platform-set-file-times ((platform posix-platform) pathname
                                    universal-time)
  "Set PATHNAME's access and modification times with UTIME."
  (let ((unix-time (max 0 (universal-time->unix-time universal-time))))
    (posix--call ':times pathname
                 (lambda ()
                   (sb-posix:utime (posix--namestring pathname)
                                   unix-time unix-time))))
  nil)

(defmethod platform-replace-file ((platform posix-platform) source target)
  "Rename SOURCE over TARGET with RENAME."
  (posix--call ':replace target
               (lambda ()
                 (sb-posix:rename (posix--namestring source)
                                  (posix--namestring target))))
  nil)

(defmethod platform-executable-file-p ((platform posix-platform) pathname)
  "Probe PATHNAME with ACCESS for execute permission."
  (handler-case
      (zerop (sb-posix:access (posix--namestring pathname) sb-posix:x-ok))
    (sb-posix:syscall-error ()
      nil)))

(defmethod platform-make-temporary-directory ((platform posix-platform)
                                              parent prefix)
  "Create the directory with MKDTEMP."
  (uiop:ensure-directory-pathname
   (posix--call ':create parent
                (lambda ()
                  (sb-posix:mkdtemp
                   (posix--namestring
                    (merge-pathnames (concatenate 'string prefix "XXXXXX")
                                     parent)))))))

(defmethod platform-delete-directory-tree ((platform posix-platform) pathname
                                           &rest arguments
                                           &key validate if-does-not-exist)
  "Delete PATHNAME's tree with UIOP; deletion here consults only the directory."
  (declare (ignore platform validate if-does-not-exist))
  (apply #'uiop:delete-directory-tree pathname arguments))


;;;; -- Terminal and Shell --

(defmethod platform-watch-terminal-resize ((platform posix-platform) function)
  "Install FUNCTION as the SIGWINCH handler."
  (sb-sys:enable-interrupt sb-unix:sigwinch
                           (lambda (signal code context)
                             (declare (ignore signal code context))
                             (funcall function)))
  ':sigwinch)

(defmethod platform-unwatch-terminal-resize ((platform posix-platform) token)
  "Restore the default SIGWINCH disposition."
  (ecase token
    (:sigwinch
     (sb-sys:enable-interrupt sb-unix:sigwinch :default)))
  nil)

(defmethod platform-shell-command-line ((platform posix-platform) command)
  "Run COMMAND through the POSIX shell."
  (list "/bin/sh" "-c" command))

(defmethod platform-open-url ((platform posix-platform) url)
  "Hand URL to the desktop launcher: open on macOS, xdg-open elsewhere."
  (declare (ignore platform))
  (platform--launch-quietly (list #+darwin "open" #-darwin "xdg-open" url)))


;;;; -- Local Sockets --

(defmethod platform-local-listener ((platform posix-platform) pathname
                                    &key (backlog 8))
  "Bind, privatize, and listen on a Unix stream socket at PATHNAME."
  (let ((listener (make-instance 'sb-bsd-sockets:local-socket :type ':stream)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-bind listener (posix--namestring pathname))
          (platform-make-private platform pathname)
          (sb-bsd-sockets:socket-listen listener backlog)
          listener)
      (error (condition)
        ; mallet:suppress no-ignore-errors -- closing the failed socket must not mask the original failure
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (error condition)))))

(defmethod platform-connect-local ((platform posix-platform) pathname)
  "Connect a Unix stream socket to PATHNAME."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type ':stream)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect socket (posix--namestring pathname))
          socket)
      (sb-bsd-sockets:connection-refused-error (condition)
        ; mallet:suppress no-ignore-errors -- closing the failed socket must not mask the original failure
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error 'platform-error
               :message   (princ-to-string condition)
               :operation ':connect
               :pathname  pathname
               :reason    ':refused))
      (sb-bsd-sockets:socket-error (condition)
        ; mallet:suppress no-ignore-errors -- closing the failed socket must not mask the original failure
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error 'platform-error
               :message   (princ-to-string condition)
               :operation ':connect
               :pathname  pathname
               :reason    ':failed)))))

(defmethod platform-source-check-command ((platform posix-platform) source-root)
  "Run the executable repository check script on POSIX."
  (declare (ignore platform))
  (list (namestring (merge-pathnames "script/check" source-root))))

(defmethod platform-session-launch-command ((platform posix-platform) source-root)
  "Return the executable stable POSIX launcher."
  (let ((pathname (merge-pathnames "bin/autolith" source-root)))
    (unless (platform-executable-file-p platform pathname)
      (error 'platform-error :operation ':launch :pathname pathname
             :message "The stable Autolith launcher is unavailable."))
    (list (namestring pathname))))

(defmethod platform-launch-detached-process
    ((platform posix-platform) arguments
     &key directory output ticket)
  "Launch ARGUMENTS behind image-daemon's POSIX process-group supervisor."
  (declare (ignore platform))
  (image-daemon:handoff-launch-supervised arguments ticket
                                          :directory directory
                                          :output    output))


;;;; -- Lock Owner Snapshot --

(defmethod platform-file-lock-owner ((platform posix-platform) pathname)
  "Query the native F_GETLK layout when SB-POSIX supports it, otherwise return unknown."
  (handler-case
      (let* ((names   '("FLOCK" "FCNTL" "FLOCK-TYPE" "FLOCK-PID"
                        "F-GETLK" "F-WRLCK" "F-RDLCK" "F-UNLCK" "SEEK-SET"))
             (symbols (mapcar (lambda (name) (find-symbol name '#:sb-posix)) names)))
        (unless (and (every #'identity symbols)
                     (find-class (first symbols) nil)
                     (every #'fboundp (subseq symbols 1 4))
                     (every #'boundp (subseq symbols 4)))
          (return-from platform-file-lock-owner (values ':unknown nil)))
        (destructuring-bind (class fcntl type-reader pid-reader get-lock write-lock
                             read-lock unlocked seek-set)
            symbols
          (let ((lock (make-instance class :type (symbol-value write-lock)
                                    :whence (symbol-value seek-set) :start 0 :len 0)))
            (with-open-stream (stream (platform-open-regular-file platform pathname))
              (funcall fcntl (sb-sys:fd-stream-fd stream) (symbol-value get-lock) lock)
              (let ((type (funcall type-reader lock))
                    (pid  (funcall pid-reader lock)))
                (cond
                  ((eql type (symbol-value unlocked))
                   (values ':free nil))
                  ((and (member type (list (symbol-value write-lock)
                                          (symbol-value read-lock)))
                        (typep pid '(integer 1)))
                   (values ':held pid))
                  (t
                   (values ':unknown nil))))))))
    ((or sb-sys:deadline-timeout error) ()
      (values ':unknown nil))))


;;;; -- Installation --

(setf *platform* (make-instance 'posix-platform))
