;;;; -- Standalone Access Policy Checks --

(in-package #:autolith-supervisor)

(defclass access-test-backend (access-backend)
  ((mode :initform nil :accessor access-test-backend-mode
         :documentation "Controlled adapter fault.")
   (starts :initform nil :accessor access-test-backend-starts
           :documentation "Recorded fixture launches, not OS enforcement.")
   (stops :initform nil :accessor access-test-backend-stops
          :documentation "Recorded fixture stops."))
  (:documentation "A deterministic policy fixture. This does not restrict any process."))

(declaim (ftype (function (string) string) access-test--path))

(defun access-test--path (path)
  "Map fixture authority names into this host's absolute temporary root."
  (if (and (>= (length path) 8) (string= path "/fixture" :end1 8))
      (concatenate 'string (uiop:native-namestring (uiop:temporary-directory)) (subseq path 1))
      path))

(defmethod access-backend-bind ((backend access-test-backend) context grant)
  "Bind a fixture alias and reject an unsupported network authority."
  (declare (ignore context))
  (when (equal (getf grant :target) "unsupported:443")
    (access-policy--reject ':unsupported-backend))
  (when (equal (getf grant :target) (access-test--path "/fixture/alias"))
    (setf (getf grant :target) (access-test--path "/fixture/private/secret")))
  (when (eq (access-test-backend-mode backend) ':stale)
    (setf (getf grant :target) (access-test--path "/fixture/changed")))
  grant)

(defmethod access-backend-start ((backend access-test-backend) context instance-id &key revision grants)
  "Return typed fixture evidence, or exercise a failed application."
  (declare (ignore context))
  (push instance-id (access-test-backend-starts backend))
  (when (eq (access-test-backend-mode backend) ':fail-start)
    (access-policy--reject ':fixture-start))
  (when (eq (access-test-backend-mode backend) ':throw-start)
    (throw 'access-test-abort ':aborted))
  (when (eq (access-test-backend-mode backend) ':exit-start)
    (sb-ext:exit :code 24 :abort t))
  (make-instance 'access-application :backend backend :instance-id instance-id
                 :revision (if (eq (access-test-backend-mode backend) ':wrong-revision)
                               (1+ revision) revision)
                 :grants   grants))

(defmethod access-backend-stop ((backend access-test-backend) context instance-id)
  "Confirm fixture cleanup unless this check injects a cleanup failure."
  (declare (ignore context))
  (push instance-id (access-test-backend-stops backend))
  (when (eq (access-test-backend-mode backend) ':fail-stop)
    (access-policy--reject ':fixture-stop))
  t)

(declaim (ftype (function (pathname keyword) nil) access-test--crash-entry))

(defun access-test--crash-entry (path mode)
  "Exit this disposable parent at a real durable transition boundary."
  (let* ((backend (make-instance 'access-test-backend))
         (terminal
           (make-parent-terminal
            :input-source (lambda (seconds) (declare (ignore seconds)) (values "YES" ':line))
            :renderer     (lambda (region kind payload)
                            (declare (ignore region payload))
                            (when (and (eq mode ':prompt) (eq kind ':approval))
                              (sb-ext:exit :code 23 :abort t)))))
         (context (make-access-context
                   :pathname path :session-id "session-test"
                   :workspace     (access-test--path "/fixture/workspace/")
                   :private-roots (list (uiop:native-namestring path) (access-test--path "/fixture/private/"))
                   :backend backend :terminal terminal))
         (service (make-access-service context))
         (request (cl-protobufs.autolith.parent.v1:make-access-request
                   :operation-id     "child-operation"
                   :requested-grants (list (cl-protobufs.autolith.parent.v1:make-grant
                                            :resource ':filesystem :target (access-test--path "/fixture/file")
                                            :operations '("read") :duration ':session)))))
    (access-service-handle service request)
    (setf (access-test-backend-mode backend) ':exit-start)
    (access-service-start service "crashed-launch")
    (error "Crash fixture reached its end.")))

(declaim (ftype (function (pathname keyword) integer) access-test--crash-process))

(defun access-test--crash-process (path mode)
  "Run a fresh parent-only SBCL and stop it at a durable policy boundary."
  (let* ((root      (asdf:system-source-directory "autolith-supervisor/terminal"))
         (forms     (append
                     (list '(require :asdf)
                           `(load ,(merge-pathnames ".qlot/setup.lisp" root))
                           `(push ,root asdf:*central-registry*)
                           '(asdf:load-system "autolith-supervisor/terminal")
                           '(asdf:load-system "sexp-store") '(asdf:load-system "ls-flock"))
                     (loop for file in '("access-policy.lisp" "access-service.lisp" "access-tests.lisp")
                           collect `(load ,(merge-pathnames file (merge-pathnames "supervisor/" root))))
                     (list '(assert (not (find-package :autolith)))
                           `(access-test--crash-entry ,path ',mode))))
         (arguments (append (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                                  "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
                            (loop for form in forms append
                                  (list "--eval" (let ((*package* (find-package :cl-user)))
                                                   (write-to-string form :readably t)))))))
    (multiple-value-bind (output error-output code)
        (uiop:run-program arguments :output ':string :error-output ':output :ignore-error-status t)
      (declare (ignore error-output))
      (unless (= code (if (eq mode ':prompt) 23 24))
        (error "Parent access crash fixture exited ~D: ~A" code output))
      code)))

(declaim (ftype (function () integer) run-parent-access-tests))

(defun run-parent-access-tests ()
  "Check validation, exact consent, durable retries, expiry and restart transitions.
Use a named test backend only. These checks do not establish OS confinement."
  (let ((checks 0)
        (paths  nil))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Parent access check ~D failed." checks)))

             (rejects (function code)
               (check (handler-case (progn (funcall function) nil)
                        (access-policy-error (condition)
                          (eq code (access-policy-error-code condition))))))

             (grant (target &key (resource ':filesystem) (duration ':session) (operations '("read")))
               (cl-protobufs.autolith.parent.v1:make-grant
                :resource resource :target (if (eq resource ':filesystem) (access-test--path target) target)
                :operations operations :duration duration))

             (request (id grants &optional (reason "agent reason"))
               (cl-protobufs.autolith.parent.v1:make-access-request
                :operation-id id :requested-grants grants :reason reason))

             (state (decision)
               (cl-protobufs.autolith.parent.v1:access-decision.state decision))

             (effective (decision)
               (cl-protobufs.autolith.parent.v1:access-decision.effective-grants decision))

             (fixture (&key (answer "YES") backend source renderer)
               (let* ((path    (merge-pathnames (format nil "autolith-access-~A.sexp" (gensym))
                                                (uiop:temporary-directory)))
                      (adapter (or backend (make-instance 'access-test-backend)))
                      (prompts (list 0))
                      (terminal
                        (make-parent-terminal
                         :input-source (or source (lambda (seconds) (declare (ignore seconds))
                                                   (values answer ':line)))
                         :renderer     (or renderer
                                           (lambda (region kind payload)
                                             (declare (ignore payload))
                                             (when (and (eq region ':trusted) (eq kind ':approval))
                                              (incf (first prompts))))))))
                 (push path paths)
                 (let ((context
                         (make-access-context :pathname path :session-id "session-test"
                                              :workspace           (access-test--path "/fixture/workspace/")
                                              :private-roots
                                              (list (uiop:native-namestring path) (access-test--path "/fixture/private/"))
                                              :private-authorities '("parent-control:1234")
                                              :services            '("approved-service")
                                              :backend adapter :terminal terminal)))
                   (values (make-access-service context) adapter prompts context)))))

      (unwind-protect
           (progn
             (multiple-value-bind (service backend prompts context) (fixture)
               (let* ((item     (request "approve" (list (grant "/fixture/workspace/file"))))
                      (decision (access-service-handle service item)))
                 (check (eq (state decision) ':approved-pending-restart))
                 (check (null (effective decision)))
                 (check (null (access-test-backend-starts backend)))
                 (check (= (first prompts) 1))
                 (check (eq (state (access-service-handle service item)) ':approved-pending-restart))
                 (check (= (first prompts) 1))
                 (rejects (lambda () (access-service-handle
                                      service (request "approve" (list (grant "/fixture/other")))))
                          ':operation-conflict)
                 (let ((started (access-service-start service "instance-1")))
                   (check (eq (state started) ':active))
                   (check (= (length (effective started)) 1)))
                 (check (eq (state (access-service-handle service item)) ':active))
                 (rejects (lambda () (access-service-start service "instance-2")) ':stop-required)
                 (let ((reopened (make-access-service context)))
                   (check (eq (state (access-service-handle reopened item)) ':approved-pending-restart))
                   (check (null (effective (access-service-handle reopened item)))))
                 (access-service-stop service)
                 (rejects (lambda () (access-service-start service "instance-1")) ':instance-reuse)
                 (check (eq (state (access-service-start service "instance-2")) ':active))
                 (check (= (first prompts) 1))
                 (access-service-revoke service "approve")
                 (check (eq (state (access-service-handle service item)) ':expired))
                 (check (null (effective (access-service-handle service item))))
                 ;; The old OS policy persists until its process has stopped.
                 (check (= (length (access-test-backend-stops backend)) 1))
                 (access-service-stop service)
                 (check (null (effective (access-service-start service "instance-3"))))
                 (check (some (lambda (note) (eq (getf note :kind) ':expired))
                              (access-service-notes service)))
                 (access-service-close service)
                (check (eq (state (access-service-handle service item)) ':expired))
                (rejects (lambda () (access-service-handle service
                                                         (request "closed-new" (list (grant "/fixture/new")))))
                         ':session-closed)))
             (multiple-value-bind (service backend prompts context) (fixture :answer "NO")
               (declare (ignore backend))
               (let ((item (request "deny" (list (grant "/fixture/workspace/file")))))
                 (check (eq (state (access-service-handle service item)) ':denied))
                 (check (eq (state (access-service-handle (make-access-service context) item)) ':denied))
                 (check (= (first prompts) 1))
                 (check (zerop (cl-protobufs.autolith.parent.v1:access-decision.policy-revision
                                (access-service-handle service item))))))
             (multiple-value-bind (service backend prompts) (fixture)
               (declare (ignore backend))
              (let ((control (make-access-request-control)))
                (check (eq (access-request-control-cancel control) ':cancelled))
                (rejects (lambda () (access-service-handle
                                     service (request "cancel" (list (grant "/fixture/file")))
                                     :control control)) ':cancelled))
              (check (zerop (first prompts))))
             (multiple-value-bind (service backend prompts) (fixture)
               (declare (ignore backend))
               (dolist (item (list (request "empty" nil)
                                  (request "" (list (grant "/fixture/file")))
                                  (request "relative" (list (grant "relative")))
                                  (request "parent" (list (grant "/fixture/../private")))
                                  (request "wild" (list (grant "/fixture/*")))
                                  (request "bad-operation" (list (grant "/fixture/file" :operations '("connect"))))
                                  (request "bad-duration" (list (grant "/fixture/file" :duration ':duration-unspecified)))))
                 (rejects (lambda () (access-service-handle service item))
                          (if (member (cl-protobufs.autolith.parent.v1:access-request.operation-id item)
                                      '("empty" "") :test #'string=) ':invalid-request ':invalid-grant)))
               (rejects (lambda () (access-service-handle service "not a request")) ':invalid-request)
               (dolist (target '("/fixture/private/secret" "/fixture/private" "/fixture/" "/"))
                 (rejects (lambda () (access-service-handle service (request target (list (grant target)))))
                          ':private-authority))
               (rejects (lambda () (access-service-handle
                                    service (request "alias" (list (grant "/fixture/alias")))))
                        ':private-authority)
               (rejects (lambda () (access-service-handle
                                    service (request "control" (list (grant "parent-control:1234"
                                                                            :resource   ':network
                                                                            :operations '("connect"))))))
                        ':private-authority)
               (rejects (lambda () (access-service-handle
                                    service (request "service" (list (grant "unknown-service" :resource ':service
                                                                            :operations                 '("connect"))))))
                        ':unknown-service)
               (rejects (lambda () (access-service-handle
                                    service (request "atomic" (list (grant "/fixture/file")
                                                                   (grant "unsupported:443" :resource  ':network
                                                                                           :operations '("connect"))))))
                        ':unsupported-backend)

               (check (zerop (first prompts))))
             (multiple-value-bind (service backend prompts) (fixture :backend (make-instance 'access-backend))
               (declare (ignore backend))
               (rejects (lambda () (access-service-handle service
                                                         (request "unsupported" (list (grant "/fixture/file")))))
                        ':unsupported-backend)
               (check (zerop (first prompts))))
             (multiple-value-bind (service backend prompts) (fixture)
               (declare (ignore prompts))
               (let ((item (request "mixed" (list (grant "/fixture/instance" :duration ':instance)
                                                  (grant "/fixture/session")))))
                 (access-service-handle service item)
                 (check (= 2 (length (effective (access-service-start service "mixed-1")))))
                 (access-service-stop service)
                 (check (eq (state (access-service-handle service item)) ':expired))
                 (check (null (effective (access-service-handle service item))))
                 (let ((decision (access-service-start service "mixed-2")))
                   (check (= 1 (length (effective decision))))
                  (check (string= (access-test--path "/fixture/session")
                                  (cl-protobufs.autolith.parent.v1:grant.target (first (effective decision))))))
                 (access-service-revoke service "mixed")
                 (access-service-stop service)
                 (check (null (effective (access-service-start service "mixed-3"))))
                 (check (= 3 (length (access-test-backend-starts backend))))))
             (multiple-value-bind (service backend prompts context) (fixture)
               (declare (ignore prompts))
               (let ((item (request "failure" (list (grant "/fixture/file")))))
                 (access-service-handle service item)
                 (setf (access-test-backend-mode backend) ':wrong-revision)
                 (rejects (lambda () (access-service-start service "failed-1")) ':application-unproved)
                 (check (eq (state (access-service-handle service item)) ':approved-pending-restart))
                 (check (equal (access-test-backend-stops backend) '("failed-1")))
                 (setf (access-test-backend-mode backend) ':throw-start)
                 (check (eq (catch 'access-test-abort (access-service-start service "failed-2")) ':aborted))
                 (check (= 2 (length (access-test-backend-stops backend))))
                 (setf (access-test-backend-mode backend) nil)
                 (check (eq (state (access-service-start service "failure-ok")) ':active))
                 (setf (access-test-backend-mode backend) ':fail-stop)
                 (rejects (lambda () (access-service-stop service)) ':fixture-stop)
                 (check (eq (state (access-service-handle service item)) ':active))
                 (rejects (lambda () (access-service-start (make-access-service context) "unsafe")) ':stop-required)
                 (setf (access-test-backend-mode backend) nil)
                 (access-service-stop service)
                 (setf (access-test-backend-mode backend) ':stale)
                 (rejects (lambda () (access-service-start service "stale")) ':stale-binding)))
             (multiple-value-bind (service backend prompts context) (fixture)
               (declare (ignore backend))
               (let* ((item (request "crashed-prompt" (list (grant "/fixture/file"))))
                      (data (access-policy--request-data context item)))
                 ;; Simulate the durable boundary immediately before a prompt.
                 (access-service--transaction
                  service
                  (lambda (snapshot)
                    (push (list :request data :bound (getf data :grants) :state ':pending
                                :revision 0 :instance nil) (getf snapshot :operations))
                    (access-service--commit service snapshot ':pending :operation-id "crashed-prompt")))
                 (with-open-file (stream (access-context-pathname context) :direction ':output
                                          :if-exists                                  ':append)
                   (write-string "(:incomplete" stream))
                 (let ((reopened (make-access-service context)))
                   (check (eq (state (access-service-handle reopened item)) ':failed))
                   (check (zerop (first prompts))))))
             (multiple-value-bind (service backend prompts context) (fixture)
               (declare (ignore backend context))
               (let* ((item    (request "parallel" (list (grant "/fixture/file"))))
                      (results (make-array 8 :initial-element nil))
                      (threads (loop for index below 8 collect
                                     (let ((slot index))
                                       (bordeaux-threads:make-thread
                                        (lambda () (setf (aref results slot)
                                                         (state (access-service-handle service item)))))))))
                 (mapc #'bordeaux-threads:join-thread threads)
                 (check (every (lambda (result) (eq result ':approved-pending-restart)) results))
                 (check (= (first prompts) 1))))
             (dolist (mode '(:prompt :launch))
               (multiple-value-bind (service backend prompts context) (fixture)
                 (declare (ignore service backend))
                 (check (= (access-test--crash-process (access-context-pathname context) mode)
                           (if (eq mode ':prompt) 23 24)))
                 (let* ((reopened (make-access-service context))
                        (item     (request "child-operation" (list (grant "/fixture/file")) "")))
                   (check (eq (state (access-service-handle reopened item))
                              (if (eq mode ':prompt) ':failed ':approved-pending-restart)))
                   (check (zerop (first prompts)))
                   (when (eq mode ':launch)
                     (rejects (lambda () (access-service-start reopened "after-crash")) ':stop-required)
                     (access-service-stop reopened)
                     (check (eq (state (access-service-start reopened "after-crash")) ':active))))))
             (let ((answer "YES"))
               (multiple-value-bind (service backend prompts) (fixture :source (lambda (seconds)
                                                                                (declare (ignore seconds))
                                                                                (values answer ':line)))
                 (declare (ignore backend))
                 (let ((old (request "old-active" (list (grant "/fixture/old"))))
                       (new (request "new-pending" (list (grant "/fixture/new")))))
                   (access-service-handle service old)
                   (access-service-start service "old-instance")
                   (setf answer "NO")
                   (check (eq (state (access-service-handle service
                                                         (request "denied-new" (list (grant "/fixture/deny")))))
                              ':denied))
                   (check (eq (state (access-service-handle service old)) ':active))
                   (setf answer "YES")
                   (check (eq (state (access-service-handle service new)) ':approved-pending-restart))
                   (check (eq (state (access-service-handle service old)) ':active))
                   (check (= 1 (length (effective (access-service-handle service old)))))
                   (check (null (effective (access-service-handle service new))))
                   (check (= (first prompts) 3)))))
             checks)
        (dolist (path paths)
          (when (probe-file path) (delete-file path))
          (let ((lock (make-pathname :name (concatenate 'string (file-namestring path) ".lock")
                                     :type nil :defaults path)))
            (when (probe-file lock) (delete-file lock))))))))
