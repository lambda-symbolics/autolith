;;;; -- Linux and Windows Whole-Agent Access Checks --

(in-package #:autolith-supervisor)

(defclass native-host-test-linux (linux-access-backend)
  ((tree :initarg :tree :reader access-native--test-linux-tree
         :documentation "Explicit file-identity observations for portable policy checks."))
  (:documentation "Read-only test observations; never a production enforcement adapter."))

(defclass native-host-test-windows (windows-access-backend)
  ((entries :initarg :entries :reader access-native--test-windows-entries
            :documentation "Explicit handle observations for portable policy checks."))
  (:documentation "Read-only test observations; never a production enforcement adapter."))

(defmethod access-native--canonical-path ((backend native-host-test-linux) target)
  "Use the literal name in a portable policy fixture."
  target)

(defmethod access-native--linux-tools ((backend native-host-test-linux))
  "Return distinct immutable fixture helper names."
  (values "/fixture/tools/bwrap" "/fixture/tools/network"))

(defmethod access-native--linux-network-files ((backend native-host-test-linux) context)
  "Keep portable argument checks independent of the host resolver files."
  (declare (ignore context))
  nil)

(defmethod access-native--linux-walk ((backend native-host-test-linux) roots function)
  "Replay complete identity observations or refuse an unknown root."
  (dolist (root roots)
    (let ((entry (assoc root (access-native--test-linux-tree backend) :test #'string=)))
      (unless entry (access-policy--reject ':native-inode-inspection-failed))
      (dolist (object (rest entry)) (apply function object))))
  nil)

(defmethod access-native--windows-inspect ((backend native-host-test-windows) target &key follow-links)
  "Replay handle observations, following private links only when requested."
  (let ((entry (rest (assoc target (access-native--test-windows-entries backend) :test #'string=))))
    (unless entry (access-policy--reject ':native-inode-inspection-failed))
    (if (and follow-links (getf entry :target))
        (access-native--windows-inspect backend (getf entry :target) :follow-links t)
        (values target (getf entry :volume) (getf entry :identity)
                (getf entry :directory-p) (getf entry :links 1) (getf entry :reparse-p)))))

(declaim (ftype (function (function keyword) boolean) access-native--host-test-rejected-p))

(defun access-native--host-test-rejected-p (function code)
  "Require a typed refusal, not an unrelated failure."
  (handler-case (progn (funcall function) nil)
    (access-policy-error (condition) (eq code (access-policy-error-code condition)))))

(declaim (ftype (function () integer) run-native-host-access-tests))

(defun run-native-host-access-tests ()
  "Check exact authority masks, identity exclusion, launch arguments and cleanup proofs.
These portable checks do not claim native enforcement on a foreign host."
  (let ((checks 0))
    (flet ((check (value) (assert value) (incf checks)))
      (check (access-native--host-test-rejected-p
              (lambda () (access-native-check-network
                          (make-instance 'native-host-test-windows :helper "/fixture/tools/helper") t))
              ':unsupported-native-network))
      (dolist (row '((nil nil) (("read") nil) (("write") nil) (("execute") nil)
                     (("read" "write") nil) (("write" "execute") nil)
                     (("read" "execute") t) (("execute" "read") t)
                     (("read" "write" "execute") t) (("write" "execute" "read") t)
                     (("read" "read") nil) (("read" "execute" "other") nil)))
        (check (eq (not (null (access-native--linux-mask-p (first row)))) (second row)))
        (check (eq (not (null (access-native--windows-mask-p (first row)))) (second row))))
      (dolist (row '(((("/work/" nil) ("/work/" t)) (("/work/" t)))
                     ((("/work/" t) ("/work/file" nil)) (("/work/" t)))
                     ((("/work/file" t) ("/work/" nil)) (("/work/" nil) ("/work/file" t)))
                     ((("/a/file" nil) ("/a/files" t)) (("/a/file" nil) ("/a/files" t)))))
        (check (equal (access-native--normalize-scopes (first row)) (second row))))
      (check (equal (access-native--normalize-scopes '(("C:/WORK/" t) ("c:/work/file" nil))
                                                     :case-insensitive-p t)
                    '(("C:/WORK/" t))))
      (let* ((tree    (copy-tree '(("/fixture/private/" ("/fixture/private/secret" 1 99 33152))
                                   ("/fixture/workspace/" ("/fixture/workspace/file" 1 10 33152))
                                   ("/fixture/temporary/") ("/fixture/runtime/" ("/fixture/runtime/lib" 1 20 33152))
                                   ("/fixture/tools/helper" ("/fixture/tools/helper" 1 30 33152))
                                   ("/fixture/tools/network" ("/fixture/tools/network" 1 31 33152))
                                   ("/fixture/extra/rx" ("/fixture/extra/rx" 1 40 33152))
                                   ("/fixture/extra/rwx" ("/fixture/extra/rwx" 1 41 33152)))))
             (backend (make-instance 'native-host-test-linux :helper "/fixture/tools/helper"
                                     :cgroup-root "/control/cgroup/" :tree tree
                                     :command                        '("/fixture/runtime/agent" "--core" "/fixture/runtime/agent.core")
                                     :environment                    '("PATH=/fixture/runtime")
                                     :runtime-roots                  '("/fixture/runtime/")
                                     :temporary-root "/fixture/temporary/" :startup #'identity
                                     :startup-timeout 1 :cleanup-timeout 1))
             (context (make-instance 'access-context :workspace "/fixture/workspace/"
                                     :private-roots             '("/fixture/private/")))
             (grants  '((:resource :filesystem :target "/fixture/extra/rx"
                         :operations ("read" "execute") :duration :session)
                        (:resource :filesystem :target "/fixture/extra/rwx"
                         :operations ("read" "write" "execute") :duration :session)))
             (profile (access-native--profile backend context grants))
             (command (linux-access-profile-command profile)))
        (dolist (argument '("--unshare-user" "--unshare-pid" "--unshare-ipc" "--unshare-uts"
                            "--unshare-net" "--die-with-parent" "--new-session" "--tmpfs"))
          (check (member argument command :test #'string=)))
        (check (not (member "--clearenv" command :test #'string=)))
        (loop for tail on command
              when (member (first tail) '("--bind" "--ro-bind") :test #'string=)
                do (check (not (string= (second tail) "/"))))
        (check (equal (subseq (access-native--launch-command backend profile) 0 4)
                      (list "/fixture/tools/helper" "--run" "/control/cgroup/"
                            (linux-access-profile-cgroup-token profile))))
        (let* ((network-backend (make-instance 'native-host-test-linux
                                              :network-p t :helper "/fixture/tools/helper"
                                              :cgroup-root "/control/cgroup/" :tree tree
                                              :command        '("/fixture/runtime/agent")
                                              :runtime-roots  '("/fixture/runtime/")
                                              :temporary-root "/fixture/temporary/"))
               (network-command (linux-access-profile-command
                                 (access-native--profile network-backend context grants))))
          (check (not (member "--unshare-net" network-command :test #'string=)))
          (check (not (member "isolated" network-command :test #'string=)))
          (check (member "--inner" network-command :test #'string=)))
        (check (access-native--host-test-rejected-p
                (lambda () (access-backend-bind backend context
                                               '(:resource :filesystem :target "/fixture/extra/rx"
                                                 :operations ("read") :duration :session)))
                ':unsupported-native-authority))
        (check (access-native--host-test-rejected-p
                (lambda () (access-backend-bind backend context '(:resource :network)))
                ':unsupported-native-authority))
        (check (access-native--host-test-rejected-p
                (lambda () (access-native--cleanup-proof backend '(:native-scope-clean-p t
                                                                   :native-scope (:kind :posix-process-group))))
                ':cleanup-unproved))
        (check (eq (access-native--cleanup-proof backend '(:native-scope-clean-p t :cgroup-clean-p t))
                   ':bubblewrap-seccomp-cgroup))
        (let ((entry (assoc "/fixture/workspace/" tree :test #'string=)))
          (unwind-protect
               (progn
                 (setf (rest entry) '(("/fixture/workspace/private-alias" 1 99 33152)))
                 (check (access-native--host-test-rejected-p
                         (lambda () (access-native--profile backend context nil)) ':private-inode-alias))
                 (setf (rest entry) '(("/fixture/workspace/helper-alias" 1 30 33152)))
                 (check (access-native--host-test-rejected-p
                         (lambda () (access-native--profile backend context nil)) ':writable-native-runtime-alias))
                 (setf (rest entry) '(("/fixture/workspace/same-number-other-device" 2 99 33152)))
                 (check (typep (access-native--profile backend context nil) 'linux-access-profile)))
            (setf (rest entry) '(("/fixture/workspace/file" 1 10 33152))))))
      (let* ((entries '(("private-link" :target "private-target" :reparse-p t :volume 1 :identity 1)
                        ("private-target" :volume 1 :identity 99)
                        ("public" :volume 2 :identity 99)
                        ("alias" :volume 1 :identity 99)
                        ("hardlink" :volume 1 :identity 10 :links 2)
                        ("reparse" :volume 1 :identity 11 :reparse-p t)))
             (backend (make-instance 'native-host-test-windows :helper "helper.exe" :entries entries
                                     :command '("agent.exe" "--core" "agent.core")))
             (context (make-instance 'access-context :private-roots '("private-link")))
             (profile (access-native--windows-make-profile
                       :identity "cl-exec-sandbox.11111111-1111-1111-1111-111111111111"
                       :workspace "work" :rules '("write" "work" "read" "runtime" "deny" "private"))))
        (check (equal (access-native--windows-private-inventory backend context '("public"))
                      '("private-target")))
        (check (access-native--host-test-rejected-p
                (lambda () (access-native--windows-private-inventory backend context '("alias")))
                ':private-inode-alias))
        (dolist (path '("hardlink" "reparse"))
          (check (access-native--host-test-rejected-p
                  (lambda () (access-native--windows-private-inventory backend context (list path)))
                  ':unsupported-native-alias)))
        (check (access-native--host-test-rejected-p
                (lambda () (access-native--windows-private-inventory backend context '("unknown")))
                ':native-inode-inspection-failed))
        (check (access-native--host-test-rejected-p
                (lambda () (access-backend-bind backend context
                                               '(:resource :filesystem :target "public"
                                                 :operations ("write") :duration :session)))
                ':unsupported-native-authority))
        (check (equal (access-native--launch-command backend profile)
                      (append (list "helper.exe" "--run" (windows-access-profile-identity profile) "isolated" "work")
                              '("write" "work" "read" "runtime" "deny" "private" "--"
                                "agent.exe" "--core" "agent.core"))))
        (dolist (evidence '((:native-scope-clean-p t :native-scope (:kind :windows-job))
                            (:appcontainer-clean-p t :native-scope-clean-p t
                             :native-scope (:kind :posix-process-group))))
          (check (access-native--host-test-rejected-p
                  (lambda () (access-native--cleanup-proof backend evidence)) ':cleanup-unproved)))
        (check (eq (access-native--cleanup-proof backend
                                                 '(:appcontainer-clean-p t :native-scope-clean-p t
                                                   :native-scope (:kind :windows-job)))
                   ':appcontainer-restricted-token-job))))
    checks))

(declaim (ftype (function (stream) list) access-native--host-test-report))

(defun access-native--host-test-report (stream)
  "Read a bounded ASCII report without using the Lisp reader."
  (let* ((line      (with-output-to-string (output)
                      (loop repeat 128 for byte = (read-byte stream)
                            when (= byte 10) do (return)
                            do (assert (<= 32 byte 126)) (write-char (code-char byte) output)
                            finally (error "Native fixture report is too long."))))
         (separator (position #\Space line)))
    (assert separator)
    (let ((kind (subseq line 0 separator))
          (mask (parse-integer line :start (1+ separator) :junk-allowed nil)))
      (assert (member kind '("root" "leaf") :test #'string=))
      (list kind mask))))

(declaim (ftype (function (&key (:runtime-roots list) (:compiler string)) list)
                run-native-host-live-access-tests))

(defun run-native-host-live-access-tests (&key runtime-roots (compiler "cc"))
  "Run actual Linux/Windows enforcement checks with a native C fixture.
Return (:STATUS :PASSED :CHECKS N :EVIDENCE proofs), or an explicit :SKIPPED
result with a :REASON. Foreign hosts need no compiler, helper or runtime roots.
RUNTIME-ROOTS must explicitly include the public libraries needed by all three
executables. COMPILER must accept GCC/Clang arguments and, on Windows, -municode.
Missing native prerequisites are a skip, not an enforcement success."
  (unless (or (member :linux *features*) (uiop:os-windows-p))
    (return-from run-native-host-live-access-tests (list :status ':skipped :reason ':foreign-host)))
  (unless runtime-roots
    (return-from run-native-host-live-access-tests (list :status ':skipped :reason ':missing-runtime-roots)))
  (handler-case (access-native--host-class (if (uiop:os-windows-p) ':windows ':linux))
    (access-policy-error (condition)
      (return-from run-native-host-live-access-tests
        (list :status ':skipped :reason ':native-prerequisite :code (access-policy-error-code condition)))))
  (require :sb-bsd-sockets)
  (let* ((root       (merge-pathnames (format nil "autolith-native-host-~x-~x/"
                                             (get-universal-time) (random most-positive-fixnum))
                                      (uiop:temporary-directory)))
         (workspace  (merge-pathnames "workspace/" root))
         (temporary  (merge-pathnames "temporary/" root))
         (private    (merge-pathnames "private/" root))
         (outside    (merge-pathnames "outside/" root))
         (runtime    (merge-pathnames "runtime/" root))
         (executable (merge-pathnames (if (uiop:os-windows-p) "fixture.exe" "fixture") runtime))
         (allowed    (merge-pathnames "allowed" workspace))
         (secret     (merge-pathnames "secret" private))
         (blocked    (merge-pathnames "blocked" outside))
         (rx         (merge-pathnames "rx" outside))
         (rwx        (merge-pathnames "rwx" outside))
         (alias      (merge-pathnames "alias" workspace))
         (leaf-alias (merge-pathnames "leaf-alias" workspace))
         (source     (asdf:system-relative-pathname '#:autolith-supervisor "supervisor/access-native-host-test-child.c"))
         (listener nil) (backend nil) (context nil) (application nil)
         (checks 0) (proofs nil))
    (unwind-protect
         (progn
           (dolist (directory (list workspace temporary private outside runtime))
             (ensure-directories-exist directory))
           (dolist (file (list allowed secret blocked rx rwx))
             (with-open-file (output file :direction ':output :if-exists ':error)
               (write-string "native fixture sentinel" output)))
           (handler-case
               (multiple-value-bind (output errors status)
                   (uiop:run-program
                    (append (list compiler "-O2" "-Wall" "-Wextra" "-Werror")
                            (when (uiop:os-windows-p) '("-municode"))
                            (list "-o" (uiop:native-namestring executable) (uiop:native-namestring source))
                            (when (uiop:os-windows-p) '("-lws2_32")))
                    :output nil :error-output nil :ignore-error-status t)
                 (declare (ignore output errors))
                 (unless (eql status 0)
                   (return-from run-native-host-live-access-tests
                     (list :status ':skipped :reason ':compiler-unavailable :exit-code status))))
             (error ()
               (return-from run-native-host-live-access-tests
                 (list :status ':skipped :reason ':compiler-unavailable))))
           (setf listener (make-instance (find-symbol "INET-SOCKET" "SB-BSD-SOCKETS")
                                        :type ':stream :protocol ':tcp))
           (uiop:symbol-call '#:sb-bsd-sockets '#:socket-bind listener #(127 0 0 1) 0)
           (uiop:symbol-call '#:sb-bsd-sockets '#:socket-listen listener 8)
           (let* ((port    (nth-value 1 (uiop:symbol-call '#:sb-bsd-sockets '#:socket-name listener)))
                  (command (append (list (uiop:native-namestring executable) "root")
                                   (mapcar #'uiop:native-namestring (list allowed secret blocked rx rwx alias leaf-alias))
                                   (list "*" "unused" (princ-to-string port)))))
             (setf backend (make-native-access-backend
                            :command        command
                            :environment    (when (uiop:os-windows-p)
                                              (list (concatenate 'string "SystemRoot=" (uiop:getenv "SystemRoot"))))
                            :runtime-roots  (cons (uiop:native-namestring runtime) runtime-roots)
                            :temporary-root (uiop:native-namestring temporary)
                            :startup-timeout 15 :cleanup-timeout 10
                            :startup        (lambda (value)
                                              (let* ((stream   (private-process-connection
                                                                (native-access-application-process value)))
                                                     (reports  (list (access-native--host-test-report stream)
                                                                     (access-native--host-test-report stream)))
                                                     (expected (if (zerop (access-application-revision value)) 903 1007)))
                                                (assert (equal (sort (mapcar #'first reports) #'string<) '("leaf" "root")))
                                                (dolist (report reports) (assert (= (second report) expected)) (incf checks 10))))))
             (setf context (make-instance 'access-context :backend backend :workspace (uiop:native-namestring workspace)
                                         :private-roots (list (uiop:native-namestring private))))
             ;; A same-volume private hard link must be refused before any launch.
             (multiple-value-bind (output errors status)
                 (uiop:run-program (list (uiop:native-namestring executable) "host-link"
                                         (uiop:native-namestring secret) (uiop:native-namestring alias))
                                   :output nil :error-output nil :ignore-error-status t)
               (declare (ignore output errors))
               (unless (eql status 0)
                 (return-from run-native-host-live-access-tests
                   (list :status ':skipped :reason ':hardlink-fixture-unavailable))))
             (assert (access-native--host-test-rejected-p
                      (lambda () (access-backend-start backend context "alias-refusal" :revision 0 :grants nil))
                      (if (uiop:os-windows-p) ':unsupported-native-alias ':private-inode-alias)))
             (incf checks)
             (delete-file alias)
             (dotimes (revision 2)
               (let* ((id     (format nil "native-host-~d" revision))
                      (grants (when (= revision 1)
                                (mapcar (lambda (entry)
                                          (access-backend-bind
                                           backend context (list :resource ':filesystem
                                                                 :target   (uiop:native-namestring (first entry))
                                                                 :operations (second entry) :duration ':session)))
                                        (list (list rx '("read" "execute"))
                                              (list rwx '("read" "write" "execute")))))))
                 (setf application (access-backend-start backend context id :revision revision :grants grants))
                 (assert (access-backend-stop backend context id))
                 (push (copy-tree (native-access-application-evidence application)) proofs)
                 (assert (eq (access-native--application-state application) ':stopped))
                 (incf checks 2)
                 (setf application nil))))
           (list :status ':passed :checks checks :evidence (nreverse proofs)))
      (when application
        (access-backend-stop backend context (access-application-instance-id application)))
      (when listener (uiop:symbol-call '#:sb-bsd-sockets '#:socket-close listener))
      ;; The fixture creates only these files and directories. Avoid following any link during cleanup.
      (dolist (file (list alias leaf-alias allowed secret blocked rx rwx executable))
        (when (probe-file file) (delete-file file)))
      (dolist (directory (list workspace temporary private outside runtime root))
        (when (uiop:directory-exists-p directory) (uiop:delete-empty-directory directory))))))
