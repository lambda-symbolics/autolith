;;;; -- Darwin Whole-Agent Seatbelt Adapter --

(in-package #:autolith-supervisor)

(defclass darwin-access-backend (native-access-backend) ()
  (:documentation "Deny-default Seatbelt with an inescapable inherited process group.
Workers may fork and exec. They cannot use posix_spawn, setsid or setpgid.
Explicit session-wide outbound networking is supported. Mach service grants are
not supported. This adapter does not defend against a privileged host."))

(defmethod access-native-check-network ((backend darwin-access-backend) enabled)
  "Support an explicit whole-session outbound network policy."
  (declare (ignore enabled))
  t)

(sb-alien:define-alien-routine ("realpath" access-native--darwin-realpath) sb-alien:c-string
  (path sb-alien:c-string) (buffer (* sb-alien:char)))

(defmethod access-native--host-class ((host (eql ':darwin)))
  "Refuse missing Seatbelt or syscall-filter support before an approval prompt."
  (unless (probe-file #P"/usr/bin/sandbox-exec")
    (access-policy--reject ':unsupported-native-host))
  (multiple-value-bind (output errors status)
      (uiop:run-program
       '("/usr/bin/sandbox-exec" "-p"
         "(version 1)(allow default)(deny syscall-unix (syscall-number 82) (syscall-number 147) (syscall-number 244))"
         "/usr/bin/true")
       :output nil :error-output nil :ignore-error-status t)
    (declare (ignore output errors))
    (unless (eql status 0) (access-policy--reject ':unsupported-native-host)))
  'darwin-access-backend)

(defmethod access-native--canonical-path ((backend darwin-access-backend) target)
  "Resolve existing Darwin names without retaining a symlink alias in a grant."
  (sb-alien:with-alien ((buffer (sb-alien:array sb-alien:char 4096)))
    (let ((resolved (access-native--darwin-realpath
                     target (sb-alien:cast buffer (* sb-alien:char)))))
      (unless resolved (access-policy--reject ':missing-filesystem-target))
      (if (uiop:directory-exists-p resolved)
          (uiop:native-namestring (uiop:ensure-directory-pathname resolved))
          resolved))))

(defmethod access-native--cleanup-proof ((backend darwin-access-backend) evidence)
  "Combine an empty group with inherited syscall restrictions to prove cleanup."
  (unless (and (getf evidence :native-scope-clean-p)
               (eq (getf (getf evidence :native-scope) :kind) ':posix-process-group))
    (access-policy--reject ':cleanup-unproved))
  ':seatbelt-inherited-group)

(declaim (ftype (function (string) string) access-native--darwin-quote))

(defun access-native--darwin-quote (text)
  "Quote a validated literal path for the Seatbelt profile language."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across text
          do (when (find character '(#\\ #\")) (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(declaim (ftype (function (string) string) access-native--darwin-filter))

(defun access-native--darwin-filter (target)
  "Select the exact bound file name or canonical directory subtree."
  (format nil "(~A ~A)"
          (if (uiop:directory-pathname-p target) "subpath" "literal")
          (access-native--darwin-quote (if (string= target "/") target
                                         (string-right-trim "/" target)))))

(declaim (ftype (function (stream string list) null) access-native--darwin-allow))

(defun access-native--darwin-allow (stream target operations)
  "Emit only the requested filesystem operations for this exact target."
  (let ((filter (access-native--darwin-filter target)))
    (dolist (operation operations)
      (cond ((string= operation "read")
             (format stream "(allow file-read* ~A)~%" filter))
            ((string= operation "write")
             (format stream "(allow file-write* ~A)~%" filter))
            ((string= operation "execute")
             (format stream "(allow process-exec file-map-executable ~A)~%" filter))
            (t
             (access-policy--reject ':unsupported-native-operation)))))
  nil)

(declaim (ftype (function (stream string) null) access-native--darwin-traverse))

(defun access-native--darwin-traverse (stream target)
  "Permit metadata checks on ancestor names without granting their contents."
  (loop for path = (uiop:pathname-directory-pathname (pathname target))
          then (uiop:pathname-parent-directory-pathname path)
        while path
        do (let ((name (uiop:native-namestring path)))
             (format stream "(allow file-read-metadata (literal ~A))~%"
                     (access-native--darwin-quote
                      (if (string= name "/") name (string-right-trim "/" name)))))
        until (equal (pathname-directory path) '(:absolute)))
  nil)

(declaim (ftype (function (darwin-access-backend list function &key (:follow-links boolean)) null)
                access-native--darwin-walk))

(defun access-native--darwin-walk (backend roots function &key follow-links)
  "Inspect file identities without reading contents. Refuse incomplete inspections.
Resolve private links; skip admitted links because Seatbelt checks their targets.
The caller must stop the old guest first or keep its link syscalls denied."
  (let ((seen (make-hash-table :test 'equal)) (count 0))
    (labels ((visit (path)
               (when (> (incf count) 1000000)
                 (access-policy--reject ':native-inode-scan-limit))
               (let ((actual (if follow-links (access-native--canonical-path backend path) path)))
                 (multiple-value-bind (success device inode mode links)
                     (sb-unix:unix-lstat actual)
                   (unless success (access-policy--reject ':native-inode-inspection-failed))
                   (unless (= (logand mode #o170000) #o120000)
                     (funcall function actual :device device :inode inode :mode mode :links links)
                     (when (and (= (logand mode #o170000) #o040000)
                                (not (gethash (list device inode) seen)))
                       (setf (gethash (list device inode) seen) t)
                       (handler-case
                           (dolist (child (directory
                                           (make-pathname :name :wild :type :wild
                                                          :defaults (uiop:ensure-directory-pathname actual))
                                           :resolve-symlinks nil))
                             (visit (uiop:native-namestring child)))
                         (file-error () (access-policy--reject ':native-inode-inspection-failed)))))))))
      (dolist (root roots) (visit root))))
  nil)

(declaim (ftype (function (darwin-access-backend access-context list) null)
                access-native--darwin-check-aliases))

(defun access-native--darwin-check-aliases (backend context scopes)
  "Reject pre-existing private inode aliases before approval and again at launch.
Installed syscall filters prohibit guest link creation after this inspection.
A trusted host that later gives the guest an alias or secret is outside this
boundary. Private storage writers must not publish aliases into admitted scopes."
  (let ((identities (make-hash-table :test 'equal)))
    (access-native--darwin-walk
     backend (access-context-private-roots context)
     (lambda (path &key device inode mode links)
       (when (some (lambda (scope) (access-policy--overlap-p path scope)) scopes)
         (access-policy--reject ':private-inode-alias))
       (unless (= (logand mode #o170000) #o040000)
         (when (> links 1) (setf (gethash (list device inode) identities) t))))
     :follow-links t)
    (when (plusp (hash-table-count identities))
      (access-native--darwin-walk
       backend scopes
       (lambda (path &key device inode mode links)
         (declare (ignore path mode links))
         (when (gethash (list device inode) identities)
           (access-policy--reject ':private-inode-alias))))))
  nil)

(defmethod access-backend-bind :around ((backend darwin-access-backend) context grant)
  "Check the requested scope and baseline scopes for private inode aliases."
  (let ((bound (call-next-method)))
    (access-native--darwin-check-aliases
     backend context
     (append (mapcar (lambda (path) (access-native--bound-path backend context path))
                     (list (access-context-workspace context)
                           (access-native--temporary-root backend)))
             (list (getf bound :target))
             (mapcar (lambda (path) (access-native--bound-path backend context path))
                     (access-native--runtime-roots backend))))
    bound))

(defmethod access-native--profile ((backend darwin-access-backend) context grants)
  "Compile immutable canonical scopes, then deny private roots independently.
The kernel resolves symlinks on each operation. Replacing an allowed name with
a link cannot extend its authority to a different canonical subtree. Reject
pre-existing private inode aliases and prohibit guest hard links and kernel
file-copy shortcuts. A trusted host can still supply new data or aliases."
  (let ((workspace (access-native--bound-path backend context (access-context-workspace context)))
        (temporary (access-native--bound-path backend context (access-native--temporary-root backend)))
        (runtime   (mapcar (lambda (root) (access-native--bound-path backend context root))
                           (access-native--runtime-roots backend))))
    (unless (and (uiop:directory-pathname-p workspace) (uiop:directory-pathname-p temporary)
                 (every #'uiop:directory-pathname-p runtime))
      (access-policy--reject ':invalid-native-directory))
    (access-native--darwin-check-aliases
     backend context (append (list workspace temporary)
                             (mapcar (lambda (grant) (getf grant :target)) grants) runtime))
    (with-output-to-string (stream)
      (format stream "(version 1)~%(deny default)~%")
      (format stream "(allow process-fork)~%(allow sysctl-read)~%(allow signal (target same-sandbox))~%")
      (when (access-native-network-p backend)
        (format stream "(allow network-outbound)~%")
        (format stream "(allow mach-lookup (global-name \"com.apple.system.opendirectoryd.libinfo\"))~%")
        (format stream "(allow file-read* (literal \"/private/etc/resolv.conf\") (literal \"/private/etc/hosts\") (literal \"/private/etc/services\"))~%"))
      ;; Darwin syscall numbers are shared by arm64 and x86-64. Blocking spawn
      ;; prevents POSIX_SPAWN_SETPGROUP from bypassing setpgid's syscall filter.
      ;; Fork/exec and vfork/exec inherit the group and the installed profile.
      (format stream "(deny syscall-unix (syscall-number 82) (syscall-number 147) (syscall-number 244)~%")
      (format stream " (syscall-number 14) (syscall-number 554) (syscall-number 61)~%")
      (format stream " (syscall-number 159) (syscall-number 164) (syscall-number 167)~%")
      (format stream " (syscall-number 424) (syscall-number 526)~%")
      ;; No guest may introduce an inode alias after admission, including from
      ;; a source directory that another granted scope can modify.
      (format stream " (syscall-number 9) (syscall-number 471) (syscall-number 227)~%")
      (format stream " (syscall-number 462) (syscall-number 517))~%")
      ;; The Darwin runtime reads the root directory itself during startup.
      ;; This literal rule does not expose file contents below that directory.
      (format stream "(allow file-read-data (literal \"/\"))~%")
      (format stream "(allow file-read* file-write* (literal \"/dev/null\"))~%")
      (format stream "(allow file-read* (literal \"/dev/urandom\") (literal \"/dev/random\"))~%")
      (dolist (target (append (list workspace temporary (access-context-workspace context)
                                   (access-native--temporary-root backend)
                                   (first (access-native--command backend)))
                             runtime (access-native--runtime-roots backend)
                             (mapcar (lambda (grant) (getf grant :target)) grants)))
        (access-native--darwin-traverse stream target))
      (access-native--darwin-allow stream workspace '("read" "write" "execute"))
      (access-native--darwin-allow stream temporary '("read" "write" "execute"))
      (dolist (root runtime) (access-native--darwin-allow stream root '("read" "execute")))
      (dolist (grant grants)
        (unless (equal grant (access-backend-bind backend context grant))
          (access-policy--reject ':stale-binding))
        (access-native--darwin-allow stream (getf grant :target) (getf grant :operations)))
      (dolist (root (access-context-private-roots context))
        (dolist (target (remove-duplicates
                         (list root (access-native--canonical-path backend root)) :test #'string=))
          (format stream "(deny file-read* file-write* process-exec file-map-executable ~A)~%"
                  (access-native--darwin-filter
                   (uiop:native-namestring (uiop:ensure-directory-pathname target)))))))))

(defmethod access-native--launch-command ((backend darwin-access-backend) profile)
  "Apply Seatbelt before the first agent instruction, without a writable policy file."
  (append (list "/usr/bin/sandbox-exec" "-p" profile) (copy-tree (access-native--command backend))))
