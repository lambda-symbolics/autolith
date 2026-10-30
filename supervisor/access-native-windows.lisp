;;;; -- Windows Whole-Agent AppContainer Adapter --

(in-package #:autolith-supervisor)

(defclass windows-access-backend (native-access-backend)
  ((helper :initarg :helper :reader access-native--windows-helper
           :documentation "Trusted pinned AppContainer helper executable."))
  (:documentation "Unique AppContainer identity, restricted token, path pins and nested job containment.
The pinned helper sets ACLs before its suspended child runs. Every descendant
inherits containment. Explicit cleanup removes its job, ACLs and profile."))

(defstruct (windows-access-profile (:constructor access-native--windows-make-profile))
  "Parent-only invocation identity and exact native ACL manifest."
  identity rules workspace (cleaned-p nil))

(defgeneric access-native--windows-inspect (backend target &key follow-links)
  (:documentation "Return canonical path, volume, file ID, directory flag, link count and reparse flag.
Open a noninheritable host handle. FOLLOW-LINKS is true for private inventory.
Refuse missing names, unknown metadata and unsupported local path forms."))

#+win32
(progn
  (sb-alien:define-alien-type access-native-wide-string
      (sb-alien:c-string :external-format :ucs-2le))
  (sb-alien:define-alien-routine ("GetFinalPathNameByHandleW" access-native--windows-final-path)
      (sb-alien:unsigned 32)
    (handle (sb-alien:signed 64)) (buffer (* sb-alien:unsigned-short))
    (count (sb-alien:unsigned 32)) (flags (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("GetFileInformationByHandle" access-native--windows-file-info)
      sb-alien:int
    (handle (sb-alien:signed 64)) (information (* t))))

(declaim (ftype (function (list) boolean) access-native--windows-mask-p))

(defun access-native--windows-mask-p (operations)
  "Match the helper's exact RX and RWX masks. Reject all narrower requests."
  (and (member (length operations) '(2 3))
       (every (lambda (operation) (member operation '("read" "write" "execute") :test #'string=))
              operations)
       (= (length operations) (length (remove-duplicates operations :test #'string=)))
       (not (null (member "read" operations :test #'string=)))
       (not (null (member "execute" operations :test #'string=)))))

(defmethod access-native--host-class ((host (eql ':windows)))
  "Require the native x86-64 helper and both architecture-specific compatibility libraries."
  #-win32 (access-policy--reject ':unsupported-native-host)
  #+win32
  (let ((helper (cl-exec-sandbox::windows--find-helper)))
    (unless (and (member :x86-64 *features*) helper
                 (every #'uiop:file-exists-p (cl-exec-sandbox::windows--compatibility-libraries helper)))
      (access-policy--reject ':unsupported-native-host))
    'windows-access-backend))

(defmethod initialize-instance :after ((backend windows-access-backend) &key)
  "Capture the trusted helper path, separate from the explicit child environment."
  (unless (slot-boundp backend 'helper)
    (let ((helper (cl-exec-sandbox::windows--find-helper)))
      (unless helper (access-policy--reject ':unsupported-native-host))
      (setf (slot-value backend 'helper) (uiop:native-namestring helper)))))

(defmethod access-native--windows-inspect ((backend windows-access-backend) target &key follow-links)
  "Use file handles, not lexical paths, to bind authority and inspect identities."
  (declare (ignorable target follow-links))
  #-win32 (access-policy--reject ':unsupported-native-host)
  #+win32
  (progn
    (cl-exec-sandbox::windows--validate-path (uiop:parse-native-namestring target))
    (let ((handle (supervisor--open-pipe target #x80 7 nil 3
                                        (logior #x02000000 (if follow-links 0 #x00200000)) 0)))
      (when (= handle -1) (access-policy--reject ':native-inode-inspection-failed))
      (unwind-protect
           (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 52))
                                 (buffer (sb-alien:array sb-alien:unsigned-short 32768)))
             (unless (plusp (access-native--windows-file-info handle (sb-alien:alien-sap information)))
               (access-policy--reject ':native-inode-inspection-failed))
             (let* ((size (access-native--windows-final-path handle
                                                           (sb-alien:cast buffer (* sb-alien:unsigned-short))
                                                           32768 0))
                    (sap  (sb-alien:alien-sap information)))
               (unless (< 0 size 32768) (access-policy--reject ':native-inode-inspection-failed))
               (let* ((raw         (sb-alien:cast buffer access-native-wide-string))
                      (path        (if (uiop:string-prefix-p "\\\\?\\" raw) (subseq raw 4) raw))
                      (directory-p (not (zerop (logand #x10 (sb-sys:sap-ref-32 sap 0))))))
                 (cl-exec-sandbox::windows--validate-path (uiop:parse-native-namestring path))
                 (values (if directory-p
                             (uiop:native-namestring (uiop:ensure-directory-pathname path)) path)
                         (sb-sys:sap-ref-32 sap 28)
                         (logior (ash (sb-sys:sap-ref-32 sap 44) 32) (sb-sys:sap-ref-32 sap 48))
                         directory-p (sb-sys:sap-ref-32 sap 40)
                         (not (zerop (logand #x400 (sb-sys:sap-ref-32 sap 0))))))))
        (supervisor--close-handle handle)))))

(defmethod access-native--canonical-path ((backend windows-access-backend) target)
  "Bind the resolved, drive-qualified local path."
  (access-native--windows-inspect backend target :follow-links t))

(declaim (ftype (function (windows-access-backend list function &key (:private-p boolean)) null)
                access-native--windows-walk))

(defun access-native--windows-walk (backend roots function &key private-p)
  "Inventory private targets through links; reject reparse and hard-link admitted objects.
Refuse incomplete scans. Bound the walk by unique directory IDs and one million entries."
  (let ((seen (make-hash-table :test 'equal)) (count 0))
    (labels ((visit (path)
               (when (> (incf count) 1000000) (access-policy--reject ':native-inode-scan-limit))
               (multiple-value-bind (actual volume identity directory-p links reparse-p)
                   (access-native--windows-inspect backend path :follow-links private-p)
                 (when (and (not private-p) (or reparse-p (and (not directory-p) (/= links 1))))
                   (access-policy--reject ':unsupported-native-alias))
                 (funcall function actual volume identity directory-p)
                 (when (and directory-p (not (gethash (list volume identity) seen)))
                   (setf (gethash (list volume identity) seen) t)
                   (handler-case
                       (dolist (child (directory (make-pathname :name :wild :type :wild
                                                                :defaults (uiop:ensure-directory-pathname actual))
                                                :resolve-symlinks nil))
                         (visit (uiop:native-namestring child)))
                     (file-error () (access-policy--reject ':native-inode-inspection-failed)))))))
      (dolist (root roots) (visit root))))
  nil)

(declaim (ftype (function (windows-access-backend access-context list) list)
                access-native--windows-private-inventory))

(defun access-native--windows-private-inventory (backend context admitted)
  "Exclude every private identity and retain minimal resolved targets for native denies."
  (let ((private (make-hash-table :test 'equal)) (paths nil))
    (labels ((covered-p (path root)
               (or (string-equal path root)
                   (and (find (char root (1- (length root))) '(#\\ #\/))
                        (<= (length root) (length path))
                        (string-equal root path :end2 (length root))))))
      (access-native--windows-walk
       backend (access-context-private-roots context)
       (lambda (path volume identity directory-p)
         (declare (ignore directory-p))
         (setf (gethash (list volume identity) private) t)
         (unless (some (lambda (root) (covered-p path root)) paths)
           (setf paths (cons path (remove-if (lambda (root) (covered-p root path)) paths)))))
       :private-p t)
      (access-native--windows-walk
       backend admitted
       (lambda (path volume identity directory-p)
         (declare (ignore path directory-p))
         (when (gethash (list volume identity) private)
           (access-policy--reject ':private-inode-alias)))))
    paths))

(declaim (ftype (function (windows-access-backend access-context list) list)
                access-native--windows-scopes))

(defun access-native--windows-scopes (backend context grants)
  "Collect explicit work, temporary and runtime scopes without parent private state."
  (let* ((workspace (access-native--bound-path backend context (access-context-workspace context)))
         (temporary (access-native--bound-path backend context (access-native--temporary-root backend)))
         (runtime   (mapcar (lambda (path) (access-native--bound-path backend context path))
                            (append (access-native--runtime-roots backend)
                                    (mapcar #'uiop:native-namestring
                                            (cl-exec-sandbox::windows--compatibility-libraries
                                             (uiop:parse-native-namestring
                                              (access-native--windows-helper backend)))))))
         (scopes    (append (list (list "write" workspace) (list "write" temporary))
                            (mapcar (lambda (path) (list "read" path)) runtime)
                            (mapcar (lambda (grant)
                                      (list (if (member "write" (getf grant :operations) :test #'string=)
                                                "write" "read") (getf grant :target))) grants))))
    (unless (and (uiop:directory-pathname-p workspace) (uiop:directory-pathname-p temporary))
      (access-policy--reject ':invalid-native-directory))
    (dolist (scope scopes)
      (when (string= (first scope) "write")
        (dolist (immutable (cons (access-native--windows-helper backend) runtime))
          (when (access-policy--overlap-p (second scope) immutable)
            (access-policy--reject ':writable-native-runtime)))))
    (access-native--windows-private-inventory backend context (mapcar #'second scopes))
    (mapcar (lambda (scope) (list (if (second scope) "write" "read") (first scope)))
            (access-native--normalize-scopes
             (mapcar (lambda (scope) (list (second scope) (string= (first scope) "write"))) scopes)
             :case-insensitive-p t))))

(defmethod access-backend-bind :around ((backend windows-access-backend) context grant)
  "Refuse requests that the helper would silently widen to RX or RWX."
  (unless (and (access-policy--grant-p grant) (eq (getf grant :resource) ':filesystem)
               (access-native--windows-mask-p (getf grant :operations)))
    (access-policy--reject ':unsupported-native-authority))
  (let ((bound (call-next-method)))
    (access-native--windows-scopes backend context (list bound))
    bound))

(defmethod access-native--profile ((backend windows-access-backend) context grants)
  "Allocate a unique profile name and explicit ACL rules for this process instance."
  (let* ((scopes  (access-native--windows-scopes backend context grants))
         (private (access-native--windows-private-inventory backend context (mapcar #'second scopes))))
    (access-native--windows-make-profile
     :identity  (cl-exec-sandbox::windows--new-profile
                 (uiop:parse-native-namestring (access-native--windows-helper backend)))
     :workspace (access-native--canonical-path backend (access-context-workspace context))
     :rules     (append (apply #'append scopes)
                        (loop for path in private append (list "deny" path))))))

(defmethod access-native--launch-command ((backend windows-access-backend) profile)
  "Use the pinned suspended-launch, restricted-token and explicit-handle helper."
  (append (list (access-native--windows-helper backend) "--run"
                (windows-access-profile-identity profile) "isolated" (windows-access-profile-workspace profile))
          (copy-list (windows-access-profile-rules profile)) (list "--")
          (copy-tree (access-native--command backend))))

(declaim (ftype (function (windows-access-backend windows-access-profile) null)
                access-native--windows-cleanup))

(defun access-native--windows-cleanup (backend profile)
  "Require native job, ACL and profile removal before the bounded deadline."
  (unless (windows-access-profile-cleaned-p profile)
    (let* ((process  (uiop:launch-program
                      (append (list (access-native--windows-helper backend) "--cleanup"
                                    (windows-access-profile-identity profile))
                              (copy-list (windows-access-profile-rules profile)))
                      :input nil :output nil :error-output nil))
           (deadline (+ (private-process--now) (access-native--cleanup-timeout backend))))
      (loop while (uiop:process-alive-p process)
            do (when (>= (private-process--now) deadline)
                 (uiop:terminate-process process :urgent t)
                 (access-policy--reject ':native-cleanup-timeout))
               (sleep 0.01))
      (unless (eql (uiop:wait-process process) 0)
        (access-policy--reject ':cleanup-unproved)))
    (setf (windows-access-profile-cleaned-p profile) t))
  nil)

(defmethod access-native--open ((backend windows-access-backend) application)
  "Remove partial native ACL installation even if startup never obtains a connection."
  (unwind-protect
       (call-next-method)
    (unless (native-access-application-process application)
      (access-native--windows-cleanup backend (access-native--application-profile application)))))

(defmethod access-native--close ((backend windows-access-backend) application)
  "Prove the outer job empty, then remove the inner job, ACL entries and profile."
  (let* ((evidence (call-next-method))
         (profile  (access-native--application-profile application)))
    (access-native--windows-cleanup backend profile)
    (append (list :appcontainer-clean-p (windows-access-profile-cleaned-p profile)
                  :profile              (copy-seq (windows-access-profile-identity profile))) evidence)))

(defmethod access-native--cleanup-proof ((backend windows-access-backend) evidence)
  "Require both empty job evidence and native ACL/profile cleanup."
  (unless (and (getf evidence :appcontainer-clean-p) (getf evidence :native-scope-clean-p)
               (eq (getf (getf evidence :native-scope) :kind) ':windows-job))
    (access-policy--reject ':cleanup-unproved))
  ':appcontainer-restricted-token-job)
