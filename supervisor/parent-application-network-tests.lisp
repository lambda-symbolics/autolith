;;;; -- Trusted Session Network Policy Checks --

(in-package #:autolith-supervisor)

(defclass parent-network-test-backend (native-access-backend) ()
  (:documentation "Portable approval fixture without process launch authority."))

(defmethod access-native-check-network ((backend parent-network-test-backend) enabled)
  "Permit testing broad approval independently of the current host."
  (declare (ignore enabled))
  t)

(serapeum/types:-> run-parent-application-network-tests () integer)
(defun run-parent-application-network-tests ()
  "Check parent consent, unavailable policy, deferred replacement and scope lifetime."
  (let ((checks 0))
    (flet ((check (value) (assert value) (incf checks)))
      (dolist (row '(("YES" :line t) ("NO" :line nil) ("yes" :line nil)
                     (nil :eof nil) (nil :cancelled nil)))
        (let* ((configuration (make-instance 'parent-configuration :values '(:network-p nil)))
               (backend       (make-instance 'parent-network-test-backend))
               (application   (make-instance 'parent-application :configuration configuration))
               (shown         nil)
               (terminal      (make-parent-terminal
                               :input-source (lambda (timeout)
                                               (declare (ignore timeout))
                                               (values (first row) (second row)))
                               :renderer     (lambda (region kind payload)
                                               (push (list region kind payload) shown)))))
          (setf (parent-application-backend application) backend)
          (check (eq (parent-application-set-network application t :terminal terminal)
                     (if (third row) ':restarting ':denied)))
          (check (not (parent-application-network-p application)))
          (check (eq backend (parent-application-backend application)))
          (check (equal (subseq (first shown) 0 2) '(:trusted :network-approval)))
          (check (eq (not (null (parent-application-after-stop application))) (third row)))
          (when (third row)
            (check (parent-application--restart-p application))
            ;; The production controller invokes this callback only after native cleanup.
            (funcall (parent-application-after-stop application))
            (check (parent-application-network-p application))
            (check (null (parent-application-backend application)))
            (check (null (parent-application-policy application)))
            (setf (parent-application-after-stop application) nil
                  (parent-application-restart application) nil
                  (parent-application-backend application) backend)
            (check (eq (parent-application-set-network application nil) ':restarting))
            (check (parent-application-network-p application))
            (funcall (parent-application-after-stop application))
            (check (not (parent-application-network-p application))))))
      (let* ((configuration (make-instance 'parent-configuration :values '(:network-p nil)))
             (application   (make-instance 'parent-application :configuration configuration))
             (prompts       0)
             (terminal      (make-parent-terminal
                             :input-source (lambda (timeout) (declare (ignore timeout)) (values "YES" ':line))
                             :renderer     (lambda (&rest values) (declare (ignore values)) (incf prompts)))))
        (setf (parent-application-backend application) (make-instance 'native-access-backend))
        (check (handler-case
                   (progn (parent-application-set-network application t :terminal terminal) nil)
                 (access-policy-error (condition)
                   (eq (access-policy-error-code condition) ':unsupported-native-network))))
        (check (zerop prompts))
        (check (null (parent-application-after-stop application))))
      (check (parent-application-test--rejects
              (lambda () (make-parent-configuration
                          (append (parent-application-test--data #P"/fixture/") '(:network-p "yes"))))))
      (let* ((configuration (make-instance 'parent-configuration
                                           :values '(:network-p t :private-root "/fixture/private/")))
             (application   (make-instance 'parent-application :configuration configuration :session-id "old")))
        (parent-application-switch-session application "new")
        (check (parent-application-network-p application))
        (funcall (parent-application-after-stop application))
        (check (not (parent-application-network-p application))))
      checks)))

(serapeum/types:-> parent-network-test--report (stream) list)
(defun parent-network-test--report (stream)
  "Read one bounded native probe report without reader evaluation."
  (let ((text (make-string-output-stream)))
    (loop repeat 64
          for byte = (read-byte stream nil nil)
          do (assert byte)
             (when (= byte 10) (return))
             (assert (< 31 byte 127))
             (write-char (code-char byte) text)
          finally (error "Native network report exceeds its limit."))
    (let ((*read-eval* nil))
      (let ((report (read-from-string (format nil "(~A)" (get-output-stream-string text)))))
        (assert (and (= (length report) 2) (every #'integerp report)))
        report))))

(serapeum/types:-> run-parent-network-native-test
    (&key (:runtime-roots list) (:compiler string)) list)
(defun run-parent-network-native-test (&key runtime-roots (compiler "cc"))
  "Prove TCP policy across denied, approved and revoked native process instances.
The parent approves whole-session network access and replaces the process only
after native cleanup. Return an explicit skip when host policy is unavailable."
  (when (uiop:os-windows-p)
    (return-from run-parent-network-native-test
      (list :status ':skipped :reason ':unsupported-native-network)))
  (unless runtime-roots
    (return-from run-parent-network-native-test
      (list :status ':skipped :reason ':missing-runtime-roots)))
  (unless (handler-case
              (eql (nth-value 2 (uiop:run-program (list compiler "--version")
                                                 :output nil :error-output nil :ignore-error-status t)) 0)
            (error () nil))
    (return-from run-parent-network-native-test
      (list :status ':skipped :reason ':compiler-unavailable)))
  (require :sb-bsd-sockets)
  (let* ((root          (merge-pathnames (format nil "parent-network-~A/" (parent-application--identifier))
                                         (uiop:temporary-directory)))
         (workspace     (merge-pathnames "workspace/" root))
         (temporary     (merge-pathnames "temporary/" root))
         (runtime       (merge-pathnames "runtime/" root))
         (private       (merge-pathnames "private/" root))
         (executable    (merge-pathnames "probe" runtime))
         (configuration (make-instance 'parent-configuration :values '(:network-p nil)))
         (owner         (make-instance 'parent-application :configuration configuration))
         (listener      nil)
         (backend       nil)
         (context       nil)
         (instance      nil)
         (pids          nil)
         (checks        0))
    (unwind-protect
         (progn
           (dolist (directory (list workspace temporary runtime private))
             (ensure-directories-exist directory))
           (multiple-value-bind (output errors status)
               (uiop:run-program
                (list compiler "-O2" "-Wall" "-Wextra" "-Werror" "-o" (namestring executable)
                      (namestring (asdf:system-relative-pathname '#:autolith-supervisor
                                                                 "supervisor/access-network-test-child.c")))
                :output nil :error-output nil :ignore-error-status t)
             (declare (ignore output errors))
             (assert (eql status 0) () "Native network fixture compilation failed with exit code ~D." status))
           (setf listener (make-instance (find-symbol "INET-SOCKET" "SB-BSD-SOCKETS")
                                         :type ':stream :protocol ':tcp))
           (uiop:symbol-call '#:sb-bsd-sockets '#:socket-bind listener #(127 0 0 1) 0)
           (uiop:symbol-call '#:sb-bsd-sockets '#:socket-listen listener 8)
           (let ((port (nth-value 1 (uiop:symbol-call '#:sb-bsd-sockets '#:socket-name listener))))
             (dotimes (revision 3)
               (when (plusp revision)
                 (setf (parent-application-after-stop owner) nil
                       (parent-application-restart owner) nil)
                 (assert (eq (parent-application-set-network
                              owner (= revision 1)
                              :terminal (make-parent-terminal
                                         :input-source (lambda (timeout)
                                                         (declare (ignore timeout)) (values "YES" ':line))
                                         :renderer     (lambda (&rest arguments) (declare (ignore arguments)))))
                             ':restarting))
                 (incf checks)
                 (assert (eql (parent-application-network-p owner) (= revision 2)))
                 (incf checks)
                 ;; The old process still has its immutable policy until cleanup.
                 (access-backend-stop backend context (access-application-instance-id instance))
                 (incf checks)
                 (funcall (parent-application-after-stop owner))
                 (assert (eql (parent-application-network-p owner) (= revision 1)))
                 (incf checks))
               (setf backend
                     (make-native-access-backend
                      :command        (list (namestring executable) (princ-to-string port))
                      :environment nil :runtime-roots (cons (namestring runtime) runtime-roots)
                      :temporary-root (namestring temporary)
                      :network-p      (parent-application-network-p owner)
                      :startup-timeout 15 :cleanup-timeout 10
                      :startup
                      (lambda (application)
                        (let ((report (parent-network-test--report
                                       (private-process-connection (native-access-application-process application)))))
                          (assert (= (first report) (if (= revision 1) 1 0)))
                          (assert (not (member (second report) pids)))
                          (push (second report) pids)
                          (incf checks 2)))))
               (setf (parent-application-backend owner) backend
                     context (make-instance 'access-context :backend backend :workspace (namestring workspace)
                                             :private-roots (list (namestring private)))
                     instance (access-backend-start backend context (format nil "network-~D" revision)
                                                    :revision revision :grants nil))))
           (access-backend-stop backend context (access-application-instance-id instance))
           (setf instance nil)
           (incf checks)
           (list :status ':passed :checks checks))
      (when instance (access-backend-stop backend context (access-application-instance-id instance)))
      (when listener (uiop:symbol-call '#:sb-bsd-sockets '#:socket-close listener))
      (parent-application-test--cleanup root))))
