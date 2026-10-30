;;;; -- Linux Whole-Agent Namespace and Cgroup Adapter --

(in-package #:autolith-supervisor)

(defclass linux-access-backend (native-access-backend)
  ((helper :initarg :helper :reader access-native--linux-helper
           :documentation "Trusted lifecycle and syscall-filter executable.")
   (cgroup-root :initarg :cgroup-root :reader access-native--linux-cgroup-root
                :documentation "Parent-only delegated cgroup v2 directory."))
  (:documentation "Minimal-root Bubblewrap, inherited seccomp, and delegated cgroup v2 cleanup.
Every child inherits the cgroup. The guest cannot see the cgroup filesystem.
The adapter refuses operation unless cgroup.kill and the native helpers exist."))

(defmethod access-native-check-network ((backend linux-access-backend) enabled)
  "Support explicit session-wide network access without weakening mount or cgroup policy."
  (declare (ignore enabled))
  t)

(defstruct (linux-access-profile (:constructor access-native--linux-make-profile))
  "Parent-owned immutable launch manifest for one process instance."
  command roots cgroup-token (prepared-p nil) (cleaned-p nil))

#+linux
(sb-alien:define-alien-routine ("realpath" access-native--linux-realpath) sb-alien:c-string
  (path sb-alien:c-string) (buffer (* sb-alien:char)))

(declaim (ftype (function (list) boolean) access-native--linux-mask-p))

(defun access-native--linux-mask-p (operations)
  "Accept only exact RX or RWX authority, independent of operation order."
  (and (member (length operations) '(2 3))
       (every (lambda (operation) (member operation '("read" "write" "execute") :test #'string=))
              operations)
       (= (length operations) (length (remove-duplicates operations :test #'string=)))
       (not (null (member "read" operations :test #'string=)))
       (not (null (member "execute" operations :test #'string=)))))

(declaim (ftype (function (list) string) access-native--linux-control))

(defun access-native--linux-control (command)
  "Run one bounded trusted lifecycle operation and require its success."
  (multiple-value-bind (output errors status)
      (uiop:run-program command :output ':string :error-output nil :ignore-error-status t)
    (declare (ignore errors))
    (unless (eql status 0) (access-policy--reject ':native-control-failed))
    (string-trim '(#\Space #\Newline #\Return) output)))

(defmethod access-native--host-class ((host (eql ':linux)))
  "Require Linux x86-64, native helpers and a writable delegated cgroup before approval."
  #-linux (access-policy--reject ':unsupported-native-host)
  #+linux
  (let ((helper (uiop:getenv "AUTOLITH_NATIVE_LINUX_HELPER"))
        (root (uiop:getenv "AUTOLITH_NATIVE_CGROUP_ROOT")))
    (unless (and (member :x86-64 *features*)
                 helper (access-policy--absolute-path-p helper) (probe-file helper)
                 root (access-policy--absolute-path-p root) (uiop:directory-exists-p root)
                 (cl-exec-sandbox::linux--find-bwrap)
                 (cl-exec-sandbox::linux--find-helper)
                 (string= (access-native--linux-control (list helper "--version"))
                          "autolith-access-linux-v1"))
      (access-policy--reject ':unsupported-native-host))
    (let ((token (format nil "al-~x-~x" (get-universal-time) (random most-positive-fixnum))))
      (access-native--linux-control (list helper "--prepare" root token))
      (unless (string= (access-native--linux-control (list helper "--cleanup" root token "5000"))
                       "autolith-cgroup-empty-v1")
        (access-policy--reject ':cleanup-unproved)))
    'linux-access-backend))

(defmethod initialize-instance :after ((backend linux-access-backend) &key)
  "Capture parent configuration once; child environment cannot change the backend."
  (unless (slot-boundp backend 'helper)
    (setf (slot-value backend 'helper) (copy-seq (uiop:getenv "AUTOLITH_NATIVE_LINUX_HELPER"))))
  (unless (slot-boundp backend 'cgroup-root)
    (setf (slot-value backend 'cgroup-root) (copy-seq (uiop:getenv "AUTOLITH_NATIVE_CGROUP_ROOT")))))

(defmethod access-native--canonical-path ((backend linux-access-backend) target)
  "Resolve literal existing Linux names, including private symlink targets."
  #-linux (access-policy--reject ':unsupported-native-host)
  #+linux
  (sb-alien:with-alien ((buffer (sb-alien:array sb-alien:char 4096)))
    (let ((resolved (access-native--linux-realpath target (sb-alien:cast buffer (* sb-alien:char)))))
      (unless resolved (access-policy--reject ':missing-filesystem-target))
      (if (uiop:directory-exists-p resolved)
          (uiop:native-namestring (uiop:ensure-directory-pathname resolved))
          resolved))))

(defgeneric access-native--linux-walk (backend roots function)
  (:documentation "Follow links and inspect all identities. Refuse incomplete or excessive scans."))

(defmethod access-native--linux-walk ((backend linux-access-backend) roots function)
  "Inspect native device/inode identities without reading file contents."
  (let ((seen (make-hash-table :test 'equal)) (count 0))
    (labels ((visit (path)
               (when (> (incf count) 1000000) (access-policy--reject ':native-inode-scan-limit))
               (let ((actual (access-native--canonical-path backend path)))
                 (multiple-value-bind (success device inode mode links) (sb-unix:unix-lstat actual)
                   (declare (ignore links))
                   (unless success (access-policy--reject ':native-inode-inspection-failed))
                   (funcall function actual device inode mode)
                   (when (and (= (logand mode #o170000) #o040000)
                              (not (gethash (list device inode) seen)))
                     (setf (gethash (list device inode) seen) t)
                     (handler-case
                         (dolist (child (directory (make-pathname :name :wild :type :wild
                                                                  :defaults (uiop:ensure-directory-pathname actual))
                                                  :resolve-symlinks nil))
                           (visit (uiop:native-namestring child)))
                       (file-error () (access-policy--reject ':native-inode-inspection-failed))))))))
      (dolist (root roots) (visit root))))
  nil)

(declaim (ftype (function (linux-access-backend access-context list) null) access-native--linux-check-aliases))

(defun access-native--linux-check-aliases (backend context roots)
  "Reject pre-existing private inode aliases, including same-volume hard links."
  (let ((private (make-hash-table :test 'equal)))
    (access-native--linux-walk
     backend (access-context-private-roots context)
     (lambda (path device inode mode)
       (declare (ignore path mode))
       (setf (gethash (list device inode) private) t)))
    (access-native--linux-walk
     backend roots
     (lambda (path device inode mode)
       (declare (ignore path mode))
       (when (gethash (list device inode) private)
         (access-policy--reject ':private-inode-alias)))))
  nil)

(defgeneric access-native--linux-tools (backend)
  (:documentation "Return the trusted Bubblewrap and isolated-network helper names."))

(defmethod access-native--linux-tools ((backend linux-access-backend))
  "Reuse the pinned host adapter's trusted executable discovery."
  (let ((bwrap  (cl-exec-sandbox::linux--find-bwrap))
        (helper (cl-exec-sandbox::linux--find-helper)))
    (unless (and bwrap helper) (access-policy--reject ':unsupported-native-host))
    (values (uiop:native-namestring bwrap) (uiop:native-namestring helper))))

(defgeneric access-native--linux-network-files (backend context)
  (:documentation "Bind public resolver files needed for approved session networking."))

(defmethod access-native--linux-network-files ((backend linux-access-backend) context)
  "Resolve fixed resolver files and reject private aliases before mounting them."
  (when (access-native-network-p backend)
    (let ((files (loop for path in '("/etc/resolv.conf" "/etc/hosts" "/etc/nsswitch.conf" "/etc/services")
                       when (probe-file path)
                         collect (list (access-native--bound-path backend context path) path))))
      (access-native--linux-check-aliases backend context (mapcar #'first files))
      files)))

(declaim (ftype (function (linux-access-backend access-context list) list) access-native--linux-scopes))

(defun access-native--linux-scopes (backend context grants)
  "Bind explicit public scopes, excluding private names and file identities."
  (let* ((workspace      (access-native--bound-path backend context (access-context-workspace context)))
         (temporary      (access-native--bound-path backend context (access-native--temporary-root backend)))
         (runtime        (mapcar (lambda (root) (access-native--bound-path backend context root))
                                 (access-native--runtime-roots backend)))
         (helper         (access-native--bound-path backend context (access-native--linux-helper backend)))
         (network-helper (access-native--bound-path backend context
                                                    (nth-value 1 (access-native--linux-tools backend))))
         (scopes         (append (list (list workspace t) (list temporary t) (list helper nil)
                                       (list network-helper nil))
                                 (mapcar (lambda (root) (list root nil)) runtime)
                                 (mapcar (lambda (grant) (list (getf grant :target)
                                                              (not (null (member "write" (getf grant :operations)
                                                                                  :test #'string=)))))
                                         grants))))
    (unless (every #'uiop:directory-pathname-p (append (list workspace temporary) runtime))
      (access-policy--reject ':invalid-native-directory))
    ;; A writable ancestor could replace a runtime helper or its dynamic loader.
    (dolist (scope scopes)
      (when (second scope)
        (dolist (immutable (append (list helper network-helper) runtime))
          (when (access-policy--overlap-p (first scope) immutable)
            (access-policy--reject ':writable-native-runtime)))))
    (dolist (scope scopes)
      (when (or (string= (first scope) "/")
                (access-policy--overlap-p (first scope) (access-native--linux-cgroup-root backend)))
        (access-policy--reject ':native-control-authority)))
    (access-native--linux-check-aliases backend context (mapcar #'first scopes))
    (let ((immutable (make-hash-table :test 'equal)))
      (access-native--linux-walk
       backend (append (list helper network-helper) runtime)
       (lambda (path device inode mode)
         (declare (ignore path mode))
         (setf (gethash (list device inode) immutable) t)))
      (access-native--linux-walk
       backend (loop for (path writable-p) in scopes when writable-p collect path)
       (lambda (path device inode mode)
         (declare (ignore path mode))
         (when (gethash (list device inode) immutable)
           (access-policy--reject ':writable-native-runtime-alias)))))
    (access-native--normalize-scopes scopes)))

(defmethod access-backend-bind :around ((backend linux-access-backend) context grant)
  "Refuse narrow masks before approval; mount rights include read and execute."
  (unless (and (access-policy--grant-p grant) (eq (getf grant :resource) ':filesystem)
               (access-native--linux-mask-p (getf grant :operations)))
    (access-policy--reject ':unsupported-native-authority))
  (let ((bound (call-next-method)))
    (access-native--linux-scopes backend context (list bound))
    bound))

(defmethod access-native--profile ((backend linux-access-backend) context grants)
  "Build a minimal mount root, with no host-root or implicit home-directory bind."
  (let* ((scopes         (access-native--linux-scopes backend context grants))
         (helper         (access-native--canonical-path backend (access-native--linux-helper backend)))
         (network-helper (nth-value 1 (access-native--linux-tools backend)))
         (bwrap          (access-native--linux-tools backend))
         (arguments      (append '("--die-with-parent" "--new-session" "--unshare-user" "--unshare-pid"
                                   "--unshare-ipc" "--unshare-uts")
                                 (unless (access-native-network-p backend) '("--unshare-net"))
                                 '("--tmpfs" "/" "--dev" "/dev" "--proc" "/proc"))))
    ;; PRIVATE-PROCESS-OPEN replaces the environment and adds the instance ID.
    ;; Preserve that explicit environment through Bubblewrap and both helpers.
    (dolist (scope scopes)
      (setf arguments (cl-exec-sandbox::linux--append-target-parent-arguments arguments (first scope))
            arguments (append arguments (list (if (second scope) "--bind" "--ro-bind")
                                              (first scope) (first scope)))))
    (dolist (file (access-native--linux-network-files backend context))
      (setf arguments (cl-exec-sandbox::linux--append-target-parent-arguments arguments (second file))
            arguments (append arguments (list "--ro-bind" (first file) (second file)))))
    (setf arguments (append arguments
                            (list "--chdir" (access-native--canonical-path backend
                                                                          (access-context-workspace context))
                                  "--" helper "--inner")
                            (unless (access-native-network-p backend)
                              (list network-helper "inner" "isolated" "--"))
                            (cons (access-native--bound-path backend context
                                                             (first (access-native--command backend)))
                                  (mapcar #'copy-seq (rest (access-native--command backend))))))
    (access-native--linux-make-profile
     :command (cons bwrap arguments) :roots scopes
     :cgroup-token (format nil "al-~x-~x" (get-universal-time) (random most-positive-fixnum)))))

(defmethod access-native--launch-command ((backend linux-access-backend) profile)
  "Join the dedicated cgroup before Bubblewrap executes any guest instruction."
  (append (list (access-native--linux-helper backend) "--run"
                (access-native--linux-cgroup-root backend) (linux-access-profile-cgroup-token profile))
          (copy-tree (linux-access-profile-command profile))))

(defmethod access-native--open ((backend linux-access-backend) application)
  "Prepare the delegated scope and launch. Remove the scope on startup failure."
  (let ((profile (access-native--application-profile application)))
    (access-native--linux-control
     (list (access-native--linux-helper backend) "--prepare" (access-native--linux-cgroup-root backend)
           (linux-access-profile-cgroup-token profile)))
    (setf (linux-access-profile-prepared-p profile) t)
    (unwind-protect
         (call-next-method)
      (unless (native-access-application-process application)
        (access-native--linux-cleanup backend profile)))))

(declaim (ftype (function (linux-access-backend linux-access-profile) null) access-native--linux-cleanup))

(defun access-native--linux-cleanup (backend profile)
  "Kill and remove the exact cgroup, requiring the kernel's empty-scope result."
  (when (and (linux-access-profile-prepared-p profile) (not (linux-access-profile-cleaned-p profile)))
    (unless (string= (access-native--linux-control
                      (list (access-native--linux-helper backend) "--cleanup"
                            (access-native--linux-cgroup-root backend)
                            (linux-access-profile-cgroup-token profile)
                            (format nil "~d" (ceiling (* 1000 (access-native--cleanup-timeout backend))))))
                     "autolith-cgroup-empty-v1")
      (access-policy--reject ':cleanup-unproved))
    (setf (linux-access-profile-cleaned-p profile) t))
  nil)

(defmethod access-native--close ((backend linux-access-backend) application)
  "Remove descendants in their inherited cgroup before closing protocol ownership."
  (let ((profile (access-native--application-profile application)))
    (access-native--linux-cleanup backend profile)
    (let ((evidence (call-next-method)))
      (append (list :cgroup-clean-p (linux-access-profile-cleaned-p profile)
                    :cgroup-token   (copy-seq (linux-access-profile-cgroup-token profile))) evidence))))

(defmethod access-native--cleanup-proof ((backend linux-access-backend) evidence)
  "Require delegated-cgroup removal; a POSIX process group alone is insufficient."
  (unless (and (getf evidence :cgroup-clean-p) (getf evidence :native-scope-clean-p))
    (access-policy--reject ':cleanup-unproved))
  ':bubblewrap-seccomp-cgroup)
