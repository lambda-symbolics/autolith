(in-package #:autolith)

;;;; -- POSIX Platform Adapter --

;;; This file implements the platform protocol for Linux, macOS, and BSD
;;; hosts on top of SB-POSIX. Besides the Windows adapter, it is the only
;;; Autolith source naming SB-POSIX process, permission, link, terminal,
;;; and signal operations, and the only one with host reader conditionals.

(defclass posix-platform (platform)
  ()
  (:documentation "The adapter for Linux, macOS, and BSD hosts built on SB-POSIX."))

(defmethod platform-file-uri ((platform posix-platform) pathname)
  "Encode POSIX path bytes, preserving literal backslashes as escaped filename characters."
  (concatenate 'string "file://" (platform--encode-uri-path (uiop:native-namestring pathname))))


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
                                          :output output))


;;;; -- Installation --

(setf *platform* (make-instance 'posix-platform))
