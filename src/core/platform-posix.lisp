(in-package #:autolith)

;;;; -- POSIX Platform Adapter --

;;; This file implements the platform protocol for Linux, macOS, and BSD
;;; hosts on top of SB-POSIX. Besides the Windows adapter, it is the only
;;; Autolith source naming SB-POSIX process, permission, link, terminal,
;;; and signal operations, and the only one with host reader conditionals.

(defclass posix-platform (platform)
  ()
  (:documentation "The adapter for Linux, macOS, and BSD hosts built on SB-POSIX."))


;;;; -- Failure Translation --

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
           :message (princ-to-string condition)
           :operation operation
           :pathname pathname
           :reason (posix--failure-reason errno)
           :code errno)))

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


;;;; -- Capabilities and Processes --

(defmethod platform-supports-p ((platform posix-platform) capability)
  "Report the capabilities every supported POSIX host provides."
  (and (member capability '(:local-sockets :process-groups :detached-sessions
                            :forked-image-saver))
       t))

(-> posix--target-alive-p (integer) boolean)
(defun posix--target-alive-p (target)
  "Return true when signal zero reaches kill TARGET or is merely refused."
  (handler-case
      (progn
        (sb-posix:kill target 0)
        t)
    (sb-posix:syscall-error (condition)
      (= (sb-posix:syscall-errno condition) sb-posix:eperm))))

(defmethod platform-process-alive-p ((platform posix-platform) process-id)
  "Probe PROCESS-ID with signal zero."
  (posix--target-alive-p process-id))

(defmethod platform-process-group-alive-p ((platform posix-platform)
                                           process-group-id)
  "Probe process group PROCESS-GROUP-ID with signal zero."
  (posix--target-alive-p (- process-group-id)))

(-> posix--terminate (integer boolean) null)
(defun posix--terminate (target force-p)
  "Send SIGTERM, or SIGKILL when FORCE-P, to kill TARGET."
  (posix--call ':terminate nil
               (lambda ()
                 (sb-posix:kill target
                                (if force-p sb-posix:sigkill sb-posix:sigterm))))
  nil)

(defmethod platform-terminate-process ((platform posix-platform) process-id
                                       &key force)
  "Signal PROCESS-ID with SIGTERM, or SIGKILL when FORCE."
  (posix--terminate process-id (and force t)))

(defmethod platform-terminate-process-group ((platform posix-platform)
                                             process-group-id &key force)
  "Signal process group PROCESS-GROUP-ID with SIGTERM, or SIGKILL when FORCE."
  (posix--terminate (- process-group-id) (and force t)))

(defmethod platform-detach-session ((platform posix-platform))
  "Start a new session with SETSID."
  (posix--call ':detach nil (lambda () (sb-posix:setsid)))
  nil)

(defmethod platform-run-image-saver ((platform posix-platform) child-function)
  "Fork the saver, run CHILD-FUNCTION in the child, and reap it in the parent."
  (let ((child-pid (posix--call ':fork nil (lambda () (sb-posix:fork)))))
    (if (zerop child-pid)
        (progn
          (funcall child-function)
          (sb-ext:exit :code 1 :abort t))
        (multiple-value-bind (waited-pid status)
            (posix--call ':wait nil (lambda () (sb-posix:waitpid child-pid 0)))
          (and (= waited-pid child-pid)
               (sb-posix:wifexited status)
               (zerop (sb-posix:wexitstatus status))
               t)))))

(defmethod platform-unique-identifier ((platform posix-platform))
  "Return a kernel-generated UUID string."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   #+linux
   (with-open-file (stream #P"/proc/sys/kernel/random/uuid"
                           :direction ':input
                           :external-format ':utf-8)
     (read-line stream))
   #+(and (not linux) (or darwin macos macosx bsd))
   (uiop:run-program '("/usr/bin/uuidgen") :output :string)
   #-(or linux darwin macos macosx bsd)
   (error 'platform-capability-unavailable
          :message "This host provides no kernel identifier generator."
          :capability ':unique-identifiers)))


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
  (let* ((value (uiop:getenv variable))
         (pathname (and (non-empty-string-p value)
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

(defmethod platform-application-root ((platform posix-platform) kind)
  "Place each root under its XDG base directory or the XDG default below home."
  (let ((home (user-homedir-pathname)))
    (merge-pathnames
     "autolith/"
     (ecase kind
       (:config
        (posix--environment-directory "XDG_CONFIG_HOME"
                                      (merge-pathnames ".config/" home)))
       (:data
        (posix--environment-directory "XDG_DATA_HOME"
                                      (merge-pathnames ".local/share/" home)))
       (:state
        (posix--environment-directory "XDG_STATE_HOME"
                                      (merge-pathnames ".local/state/" home)))
       (:cache
        (posix--environment-directory "XDG_CACHE_HOME"
                                      (merge-pathnames ".cache/" home)))))))

(-> posix--status (t) platform-file-status)
(defun posix--status (stat)
  "Return the platform file status described by SB-POSIX STAT."
  (let* ((mode (sb-posix:stat-mode stat))
         (owned-p (= (sb-posix:stat-uid stat) (sb-posix:getuid))))
    (make-instance 'platform-file-status
                   :kind (cond
                           ((sb-posix:s-isreg mode)
                            ':file)
                           ((sb-posix:s-isdir mode)
                            ':directory)
                           ((sb-posix:s-islnk mode)
                            ':symbolic-link)
                           ((sb-posix:s-issock mode)
                            ':socket)
                           (t
                            ':other))
                   :identity (cons (sb-posix:stat-dev stat)
                                   (sb-posix:stat-ino stat))
                   :size (sb-posix:stat-size stat)
                   :modification-time (sb-posix:stat-mtime stat)
                   :change-time (sb-posix:stat-ctime stat)
                   :owned-p owned-p
                   :private-p (and owned-p (zerop (logand mode #o077)))
                   :read-only-p (zerop (logand mode #o200)))))

(defmethod platform-parse-namestring ((platform posix-platform) string)
  "Read STRING as a Unix namestring, as UIOP does for pathname designators."
  (declare (ignore platform))
  (uiop:parse-unix-namestring string))

(defmethod platform-truename ((platform posix-platform) pathname)
  "Resolve PATHNAME with TRUENAME, which follows every link here."
  (declare (ignore platform))
  (handler-case (truename pathname)
    (file-error (condition)
      (error 'platform-error
             :message (princ-to-string condition)
             :operation ':resolve
             :pathname (pathname pathname)
             :reason (if (ignore-errors (probe-file pathname))
                         ':failed
                         ':missing)))))

(defmethod platform-path-status ((platform posix-platform) pathname
                                 &key follow-links-p)
  "Observe PATHNAME with STAT, or LSTAT unless FOLLOW-LINKS-P."
  (let ((native (posix--namestring pathname)))
    (handler-case
        (posix--status (if follow-links-p
                           (sb-posix:stat native)
                           (sb-posix:lstat native)))
      (sb-posix:syscall-error (condition)
        (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
            nil
            (posix--signal ':status pathname condition))))))

(defmethod platform-stream-status ((platform posix-platform) stream)
  "Observe STREAM's descriptor with FSTAT."
  (posix--call ':status nil
               (lambda ()
                 (posix--status (sb-posix:fstat (sb-sys:fd-stream-fd stream))))))

(defmethod platform-open-regular-file ((platform posix-platform) pathname
                                       &key follow-links-p)
  "Open PATHNAME read-only without blocking, refusing links unless FOLLOW-LINKS-P."
  (let* ((native (posix--namestring pathname))
         (descriptor
           (posix--call ':open pathname
                        (lambda ()
                          (sb-posix:open native
                                         (logior sb-posix:o-rdonly
                                                 sb-posix:o-nonblock
                                                 (if follow-links-p
                                                     0
                                                     sb-posix:o-nofollow)))))))
    (unwind-protect
         (let ((status
                 (posix--call ':open pathname
                              (lambda ()
                                (posix--status (sb-posix:fstat descriptor))))))
           (unless (eq (platform-file-status-kind status) ':file)
             (error 'platform-error
                    :message (format nil "~A is not a regular file." native)
                    :operation ':open
                    :pathname pathname
                    :reason ':not-regular))
           (let ((stream (sb-sys:make-fd-stream descriptor
                                                :input t
                                                :element-type '(unsigned-byte 8)
                                                :auto-close t)))
             (setf descriptor nil)
             (values stream status)))
      (when descriptor
        (ignore-errors (sb-posix:close descriptor))))))

(defmethod platform-list-directory ((platform posix-platform) pathname
                                    &key (limit most-positive-fixnum))
  "Enumerate PATHNAME with OPENDIR and READDIR."
  (let ((handle nil)
        (names nil)
        (count 0)
        (more-p nil))
    (posix--call ':list pathname
                 (lambda ()
                   (unwind-protect
                        (progn
                          (setf handle (sb-posix:opendir (posix--namestring pathname)))
                          (loop for entry = (sb-posix:readdir handle)
                                until (sb-alien:null-alien entry)
                                for name = (sb-posix:dirent-name entry)
                                unless (member name '("." "..") :test #'string=)
                                  do (if (< count limit)
                                         (progn
                                           (push name names)
                                           (incf count))
                                         (progn
                                           (setf more-p t)
                                           (return)))))
                     (when handle
                       (sb-posix:closedir handle)))))
    (values (nreverse names) more-p)))

(defmethod platform-create-private-file ((platform posix-platform) pathname)
  "Create PATHNAME with O_EXCL and mode 0600."
  (let ((descriptor
          (posix--call ':create pathname
                       (lambda ()
                         (sb-posix:open (posix--namestring pathname)
                                        (logior sb-posix:o-wronly
                                                sb-posix:o-creat
                                                sb-posix:o-excl)
                                        #o600)))))
    (sb-sys:make-fd-stream descriptor
                           :output t
                           :element-type '(unsigned-byte 8)
                           :auto-close t)))

(-> posix--mode (pathname) integer)
(defun posix--mode (pathname)
  "Return PATHNAME's current mode bits, following links."
  (posix--call ':protect pathname
               (lambda ()
                 (sb-posix:stat-mode (sb-posix:stat (posix--namestring pathname))))))

(-> posix--change-mode (pathname integer) null)
(defun posix--change-mode (pathname mode)
  "Set PATHNAME's mode bits to MODE."
  (posix--call ':protect pathname
               (lambda ()
                 (sb-posix:chmod (posix--namestring pathname) mode)))
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

(defmethod platform-publish-new-file ((platform posix-platform) source target)
  "Hard-link SOURCE to TARGET, which fails atomically when TARGET exists."
  (posix--call ':publish target
               (lambda ()
                 (sb-posix:link (posix--namestring source)
                                (posix--namestring target))))
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

(defmethod platform-shared-library-file-name ((platform posix-platform)
                                              base-name)
  "Prefix BASE-NAME with lib and add the host shared-library extension."
  (format nil #+darwin "lib~A.dylib" #-darwin "lib~A.so" base-name))

(defmethod platform-delete-directory-tree ((platform posix-platform) pathname
                                           &rest arguments
                                           &key validate if-does-not-exist)
  "Delete PATHNAME's tree with UIOP; deletion here consults only the directory."
  (declare (ignore platform validate if-does-not-exist))
  (apply #'uiop:delete-directory-tree pathname arguments))


;;;; -- Terminal and Shell --

(defmethod platform-interactive-descriptor-p ((platform posix-platform)
                                              descriptor)
  "Probe DESCRIPTOR with ISATTY."
  (and (not (minusp descriptor))
       (let ((result (sb-unix:unix-isatty descriptor)))
         (and result (plusp result) t))))

(defmethod platform-disable-input-echo ((platform posix-platform) descriptor)
  "Clear the ECHO flag in DESCRIPTOR's terminal attributes."
  (posix--call ':terminal nil
               (lambda ()
                 (let ((saved (sb-posix:tcgetattr descriptor))
                       (hidden (sb-posix:tcgetattr descriptor)))
                   (setf (sb-posix:termios-lflag hidden)
                         (logandc2 (sb-posix:termios-lflag hidden) sb-posix:echo))
                   (sb-posix:tcsetattr descriptor sb-posix:tcsanow hidden)
                   saved))))

(defmethod platform-restore-input-echo ((platform posix-platform)
                                        descriptor state)
  "Reinstall the terminal attributes STATE saved from DESCRIPTOR."
  (posix--call ':terminal nil
               (lambda ()
                 (sb-posix:tcsetattr descriptor sb-posix:tcsanow state)))
  nil)

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
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error 'platform-error
               :message (princ-to-string condition)
               :operation ':connect
               :pathname pathname
               :reason ':refused))
      (sb-bsd-sockets:socket-error (condition)
        (ignore-errors (sb-bsd-sockets:socket-close socket))
        (error 'platform-error
               :message (princ-to-string condition)
               :operation ':connect
               :pathname pathname
               :reason ':failed)))))


;;;; -- Installation --

(setf *platform* (make-instance 'posix-platform))
