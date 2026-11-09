;;;; -- Directory Roots --

;;; Per-user directory roots shared by the standalone build scripts.

(require :sb-posix)

(defun autolith-script-setenv (name value)
  "Set environment variable NAME to VALUE for this process and its children.

Windows keeps two environments: sb-posix:setenv updates the C runtime's copy,
while children and uiop:getenv see the process block, which the runtime only
updates for variables this process did not inherit. The block is set through
SetEnvironmentVariableW, found at run time so POSIX hosts never reference it."
  (sb-posix:setenv name value 1)
  (when (uiop:os-windows-p)
    (let ((address (sb-sys:find-foreign-symbol-address "SetEnvironmentVariableW")))
      (when address
        (sb-alien:alien-funcall
         (sb-alien:sap-alien (sb-sys:int-sap address)
                             (function sb-alien:int
                                       (sb-alien:c-string :external-format :ucs-2le)
                                       (sb-alien:c-string :external-format :ucs-2le)))
         name value))))
  nil)

(defun autolith-script-unsetenv (name)
  "Remove environment variable NAME for this process and its children."
  (sb-posix:unsetenv name)
  (when (uiop:os-windows-p)
    (let ((address (sb-sys:find-foreign-symbol-address "SetEnvironmentVariableW")))
      (when address
        (sb-alien:alien-funcall
         (sb-alien:sap-alien (sb-sys:int-sap address)
                             (function sb-alien:int
                                       (sb-alien:c-string :external-format :ucs-2le)
                                       (sb-alien:c-string :external-format :ucs-2le)))
         name nil))))
  nil)

(defun autolith-script-clear-read-only (pathname)
  "Clear the Windows read-only attribute of PATHNAME when it carries one.

A file an older build protected with chmod carries the attribute, and Windows
then refuses to rename over or delete it whatever its directory allows. POSIX
hosts have no such attribute and return at once."
  (when (and (uiop:os-windows-p) (probe-file pathname))
    (let ((get-address (sb-sys:find-foreign-symbol-address "GetFileAttributesW"))
          (set-address (sb-sys:find-foreign-symbol-address "SetFileAttributesW"))
          (native      (uiop:native-namestring pathname)))
      (when (and get-address set-address)
        (let ((attributes
                (sb-alien:alien-funcall
                 (sb-alien:sap-alien (sb-sys:int-sap get-address)
                                     (function (sb-alien:unsigned 32)
                                               (sb-alien:c-string :external-format :ucs-2le)))
                 native)))
          (when (and (/= attributes #xFFFFFFFF) (logtest attributes 1))
            (sb-alien:alien-funcall
             (sb-alien:sap-alien (sb-sys:int-sap set-address)
                                 (function sb-alien:int
                                           (sb-alien:c-string :external-format :ucs-2le)
                                           (sb-alien:unsigned 32)))
             native (logandc2 attributes 1)))))))
  nil)

(defun autolith-script-replace-file (source target)
  "Rename SOURCE over TARGET, first clearing a Windows read-only attribute on TARGET."
  (autolith-script-clear-read-only target)
  (uiop:rename-file-overwriting-target source target))

(defun autolith-script-set-file-mode (pathname mode)
  "Give PATHNAME POSIX MODE where modes exist.

Windows keeps the access list the private data root gives its files: chmod
there only sets the read-only attribute, which would block the replacement the
next build performs."
  (unless (uiop:os-windows-p)
    (sb-posix:chmod (uiop:native-namestring pathname) mode))
  nil)

(defun autolith-script-environment-directory (variable)
  "Return the absolute directory VARIABLE names, or NIL when unset or relative."
  (let ((value (uiop:getenv variable)))
    (when (and value (plusp (length value)))
      (let ((pathname
              (if (uiop:os-windows-p)
                  (uiop:ensure-directory-pathname
                   (uiop:parse-native-namestring value))
                  (uiop:parse-native-namestring
                   (if (char= (char value (1- (length value))) #\/)
                       value
                       (concatenate 'string value "/"))))))
        (when (uiop:absolute-pathname-p pathname)
          pathname)))))

(defun autolith-script-known-folder (variable)
  "Return the Windows shell folder that environment VARIABLE names."
  (or (autolith-script-environment-directory variable)
      (error "The ~A environment variable does not name an absolute directory."
             variable)))

;;; Remove this legacy XDG bridge before 0.68.0.
(define-condition autolith-script-legacy-xdg-root-conflict
    (error)
    ((variable :initarg :variable :reader autolith-script-legacy-xdg-root-variable
      :documentation "The XDG variable selecting both roots.")
     (native :initarg :native :reader autolith-script-legacy-xdg-root-native :documentation
      "The correctly parsed native application root.")
     (legacy :initarg :legacy :reader autolith-script-legacy-xdg-root-legacy :documentation
      "The application root selected by the old parser."))
  (:documentation "Both native and legacy XDG roots contain installation content.")
  (:report
   (lambda (condition stream)
     (format stream
             "Both XDG roots contain content for ~A: ~S and ~S. Choose a root through a restart or resolve the directories while Autolith is stopped. No files were moved."
             (autolith-script-legacy-xdg-root-variable condition)
             (uiop/filesystem:native-namestring
              (autolith-script-legacy-xdg-root-native condition))
             (uiop/filesystem:native-namestring
              (autolith-script-legacy-xdg-root-legacy condition))))))

(define-condition autolith-script-legacy-xdg-root-relative-link (file-error)
  ()
  (:documentation "A relative root symlink cannot be moved without changing its referent.")
  (:report (lambda (condition stream)
             (format stream "The legacy root ~S is a relative symlink; relocating it would change its target. Set an explicit root or resolve the link while Autolith is stopped."
                     (uiop:native-namestring (file-error-pathname condition))))))

#-win32
(defun autolith-script-xdg-root-content-p (path)
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
                                     (autolith-script-xdg-root-content-p
                                      (concatenate 'string path "/" name))))
               (sb-posix:closedir directory))))))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (error condition)))))

#-win32
(defun autolith-script-xdg-root-stat (path)
  "Return the root entry's identity without following links, or NIL when absent."
  (handler-case (sb-posix:lstat (string-right-trim "/" (uiop:native-namestring path)))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          nil
          (error condition)))))

#-win32
(defun autolith-script-call-with-xdg-migration-lock (legacy function)
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

#-win32
(defun autolith-script-move-xdg-root (legacy native)
  "Atomically rename LEGACY to NATIVE, retaining any directory-only scaffold.
The caller holds the sibling migration lock and has checked both roots. Failed
publication leaves LEGACY intact; a displaced scaffold is restored when possible."
  (let ((source      (string-right-trim "/" (uiop:native-namestring legacy)))
        (target      (string-right-trim "/" (uiop:native-namestring native)))
        (backup      nil)
        (container   nil)
        (displaced-p nil)
        (published-p nil))
    (let ((stat (autolith-script-xdg-root-stat legacy)))
      (when (and stat (sb-posix:s-islnk (sb-posix:stat-mode stat))
                 (not (char= (char (sb-posix:readlink source) 0) #\/)))
        (error 'autolith-script-legacy-xdg-root-relative-link :pathname legacy)))
    (ensure-directories-exist (uiop:pathname-parent-directory-pathname native))
    (unwind-protect
         (progn
           (when (autolith-script-xdg-root-content-p target)
             (error 'sb-posix:syscall-error :name "rename" :errno sb-posix:enotempty))
           (when (autolith-script-xdg-root-stat native)
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
             (when (autolith-script-xdg-root-content-p backup)
               (error 'sb-posix:syscall-error :name "rename" :errno sb-posix:enotempty)))
           (sb-posix:rename source target)
           (setf published-p t)
           native)
      (unless published-p
        (when (and displaced-p (not (autolith-script-xdg-root-stat native)))
          (sb-posix:rename backup target))
        (when container
          (sb-posix:rmdir container))))))

#-win32
(defun autolith-script-legacy-xdg-root (variable native)
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
            (not (autolith-script-xdg-root-content-p (uiop:native-namestring legacy))))
        native
        (handler-case
            (autolith-script-call-with-xdg-migration-lock
             legacy
             (lambda ()
               ;; Another process may have completed the move while we waited.
               (let ((native-stat (autolith-script-xdg-root-stat native))
                     (legacy-stat (autolith-script-xdg-root-stat legacy)))
                 (cond
                   ((and native-stat legacy-stat
                         (= (sb-posix:stat-dev native-stat) (sb-posix:stat-dev legacy-stat))
                         (= (sb-posix:stat-ino native-stat) (sb-posix:stat-ino legacy-stat)))
                    native)
                   ((not (autolith-script-xdg-root-content-p (uiop:native-namestring legacy)))
                    native)
                   ((autolith-script-xdg-root-content-p (uiop:native-namestring native))

                    (restart-case
                        (error 'autolith-script-legacy-xdg-root-conflict
                               :variable variable :native native :legacy legacy)
                      (use-native-root ()
                        :report "Use the literal native XDG root for this lookup."
                        native)
                      (use-legacy-root ()
                        :report "Use the historical XDG root for this lookup."
                        legacy)))
                   (t
                    (autolith-script-move-xdg-root legacy native))))))
          ((or sb-posix:syscall-error file-error) (condition)

            (warn "Could not move legacy XDG root ~S to ~S: ~A. Using the legacy root; migration will retry."
                  (uiop:native-namestring legacy) (uiop:native-namestring native) condition)
            legacy)))))

(defun autolith-application-root (kind)
  "Return Autolith's per-user directory of KIND, one of :config, :data, :state, or :cache."
  (let* ((variable (ecase kind
                     (:config
                      "XDG_CONFIG_HOME")
                     (:data
                      "XDG_DATA_HOME")
                     (:state
                      "XDG_STATE_HOME")
                     (:cache
                      "XDG_CACHE_HOME")))
         (xdg      (autolith-script-environment-directory variable)))
    (cond
      (xdg
       (let ((native (merge-pathnames "autolith/" xdg)))
         #+win32 native
         #-win32 (autolith-script-legacy-xdg-root variable native)))
      ((and (uiop:os-windows-p) (eq kind :config))
       (merge-pathnames "autolith/" (autolith-script-known-folder "APPDATA")))
      ((uiop:os-windows-p)
       (merge-pathnames (ecase kind
                          (:data
                           "autolith/data/")
                          (:state
                           "autolith/state/")
                          (:cache
                           "autolith/cache/"))
                        (autolith-script-known-folder "LOCALAPPDATA")))
      (t
       (merge-pathnames "autolith/"
                        (merge-pathnames (ecase kind
                                           (:config
                                            ".config/")
                                           (:data
                                            ".local/share/")
                                           (:state
                                            ".local/state/")
                                           (:cache
                                            ".cache/"))
                                         (user-homedir-pathname)))))))
