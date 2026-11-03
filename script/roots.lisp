;;;; -- Directory Roots --

;;; Per-user directory roots shared by the standalone build scripts.

(require :sb-posix)

;;; The scripts run before the Autolith system is loaded, so they cannot ask
;;; the platform adapter. This mirrors PLATFORM-APPLICATION-ROOT from
;;; src/core/platform-posix.lisp and src/core/platform-win32.lisp: an absolute
;;; XDG variable wins on every host, otherwise POSIX uses the XDG defaults
;;; below the home directory and Windows uses the application data folders.

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

(defun autolith-script-xdg-root-content-p (path)
  "Inspect entries without opening files or following links; directory-only trees are empty."
  (let ((stat
         (handler-case (sb-posix:lstat (string-right-trim "/" path))
                       (sb-posix:syscall-error (condition)
                                               (if (= (sb-posix:syscall-errno condition)
                                                      sb-posix:enoent)
                                                   nil
                                                   (error condition))))))
    (cond ((null stat)
           nil)
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
               (sb-posix:closedir directory)))))))

(defun autolith-script-xdg-root-stat (path)
  "Return the root's identity, or NIL when it does not exist."
  (handler-case (sb-posix:stat (uiop/filesystem:native-namestring path))
                (sb-posix:syscall-error (condition)
                                        (if (= (sb-posix:syscall-errno condition)
                                               sb-posix:enoent)
                                            nil
                                            (error condition)))))

(defun autolith-script-legacy-xdg-root (variable native)
  "Keep an existing backslash-misparsed installation accessible until 0.68.0.
Never move or merge files. Conflicting installations require an explicit choice."
  (let* ((value (uiop/os:getenv variable))
         (legacy
          (and value (find #\\ value) (plusp (length value)) (char= (char value 0) #\/)
               (handler-case
                (let ((base (uiop/pathname:ensure-directory-pathname (pathname value))))
                  (when
                      (and (uiop/pathname:absolute-pathname-p base)
                           (not (wild-pathname-p base)))
                    (merge-pathnames "autolith/" base)))
                (error nil nil)))))
    (if (or (null legacy) (equal native legacy))
        native
        (let ((native-stat (autolith-script-xdg-root-stat native))
              (legacy-stat (autolith-script-xdg-root-stat legacy)))
          (cond
           ((and native-stat legacy-stat
                 (= (sb-posix:stat-dev native-stat) (sb-posix:stat-dev legacy-stat))
                 (= (sb-posix:stat-ino native-stat) (sb-posix:stat-ino legacy-stat)))
            native)
           ((not
             (autolith-script-xdg-root-content-p (uiop/filesystem:native-namestring legacy)))
            native)
           ((autolith-script-xdg-root-content-p (uiop/filesystem:native-namestring native))
            (restart-case (error 'autolith-script-legacy-xdg-root-conflict :variable variable
                                 :native native :legacy legacy)
              (use-native-root nil :report
                "Use the literal native XDG root for this lookup." native)
              (use-legacy-root nil :report "Use the historical XDG root for this lookup."
               legacy)))
           (t
            (warn
             "Using legacy XDG installation at ~S instead of ~S. Before 0.68.0, set ~A to ~S to keep using it, or relocate it while Autolith is stopped. No files were moved."
             (uiop/filesystem:native-namestring legacy)
             (uiop/filesystem:native-namestring native) variable
             (uiop/filesystem:native-namestring
              (uiop/pathname:pathname-parent-directory-pathname legacy)))
            legacy))))))

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
         (if (uiop:os-windows-p)
             native
             (autolith-script-legacy-xdg-root variable native))))
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
