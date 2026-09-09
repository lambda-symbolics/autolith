;;;; Per-user directory roots shared by the standalone build scripts.

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
          (native (uiop:native-namestring pathname)))
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
      (let ((pathname (if (uiop:os-windows-p)
                          (uiop:parse-native-namestring value)
                          (pathname value))))
        (when (uiop:absolute-pathname-p pathname)
          (uiop:ensure-directory-pathname pathname))))))

(defun autolith-script-known-folder (variable)
  "Return the Windows shell folder that environment VARIABLE names."
  (or (autolith-script-environment-directory variable)
      (error "The ~A environment variable does not name an absolute directory."
             variable)))

(defun autolith-application-root (kind)
  "Return Autolith's per-user directory of KIND, one of :config, :data, :state, or :cache."
  (let ((xdg (autolith-script-environment-directory
              (ecase kind
                (:config "XDG_CONFIG_HOME")
                (:data "XDG_DATA_HOME")
                (:state "XDG_STATE_HOME")
                (:cache "XDG_CACHE_HOME")))))
    (cond
      (xdg
       (merge-pathnames "autolith/" xdg))
      ((and (uiop:os-windows-p) (eq kind :config))
       (merge-pathnames "autolith/" (autolith-script-known-folder "APPDATA")))
      ((uiop:os-windows-p)
       (merge-pathnames (ecase kind
                          (:data "autolith/data/")
                          (:state "autolith/state/")
                          (:cache "autolith/cache/"))
                        (autolith-script-known-folder "LOCALAPPDATA")))
      (t
       (merge-pathnames "autolith/"
                        (merge-pathnames (ecase kind
                                           (:config ".config/")
                                           (:data ".local/share/")
                                           (:state ".local/state/")
                                           (:cache ".cache/"))
                                         (user-homedir-pathname)))))))
