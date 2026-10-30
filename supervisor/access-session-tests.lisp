;;;; -- Parent Access Runtime Checks --

(in-package #:autolith-supervisor)

(defclass access-session-test-owner ()
  ((lock :initform (bordeaux-threads:make-lock "access test owner") :reader access-session-test-lock
         :documentation "Serialize fixture owner calls.")
   (sent :initform nil :accessor access-session-test-sent :documentation "Reserved fixture replies."))
  (:documentation "A deterministic owner/writer fixture, with no transport or OS authority."))

(defmethod session-runtime--operation ((owner access-session-test-owner) runtime function)
  "Run a fixture owner operation serially."
  (declare (ignore runtime))
  (bordeaux-threads:with-lock-held ((access-session-test-lock owner))
    (handler-bind ((error (lambda (condition) (format *error-output* "Fixture owner error: ~A~%" condition))))
      (funcall function))))

(defmethod session-runtime--fence ((owner access-session-test-owner) runtime reason &key turn-id)
  "Fence the fixture access adapter."
  (declare (ignore owner))
  (session-access-fence (session-runtime-access-service runtime) runtime reason :turn-id turn-id))

(defmethod session-runtime--send ((owner access-session-test-owner) runtime envelope)
  "Reserve an envelope without blocking the fixture owner."
  (declare (ignore runtime))
  (push envelope (access-session-test-sent owner))
  (length (access-session-test-sent owner)))

(declaim (ftype (function () integer) run-parent-access-session-tests))

(defun run-parent-access-session-tests ()
  "Check async approval, cancellation, durable parent notes, receipts and restart fencing."
  (let ((checks 0) (paths nil) (adapters nil))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Access session check ~D failed." checks)))

             (wait-for (function)
               (let ((deadline (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
                 (loop until (funcall function)
                       do (when (> (get-internal-real-time) deadline) (error "Access session check timed out."))
                          (sleep 0.005)))
               (check t))

             (rejects (function code)
               (check (handler-case (progn (funcall function) nil)
                        (access-policy-error (condition) (eq code (access-policy-error-code condition))))))

             (grant (target &optional (duration ':session))
               (parent-proto:make-grant :resource ':filesystem :target (access-test--path target)
                                       :operations '("read") :duration duration))

             (request (id target &optional (duration ':session))
               (parent-proto:make-access-request :operation-id id :requested-grants (list (grant target duration))))

             (records (store)
               (parent-proto:session-page.records
                (session-store-read store (parent-proto:make-session-read :limit 256))))

             (fixture (&optional (answer "YES"))
               (let* ((path       (merge-pathnames (format nil "access-session-~A.sexp" (gensym)) (uiop:temporary-directory)))
                      (store-path (make-pathname :name     (concatenate 'string (pathname-name path) "-session")
                                                 :defaults path))
                      (input (list answer)) (prompts (list 0)) (events (list nil))
                      (terminal   (make-parent-terminal
                                   :input-source (lambda (seconds)
                                                   (declare (ignore seconds))
                                                   (if (eq (first input) ':wait) (values nil ':timeout)
                                                       (values (first input) ':line)))
                                   :renderer     (lambda (region kind payload)
                                                   (declare (ignore region payload))
                                                   (when (eq kind ':approval) (incf (first prompts))))))
                      (policy     (make-access-service
                                   (make-access-context :pathname path :session-id "access-test"
                                                        :workspace     (access-test--path "/fixture/workspace/")
                                                        :private-roots (list (uiop:native-namestring path)
                                                                             (access-test--path "/fixture/private/"))
                                                        :backend (make-instance 'access-test-backend) :terminal terminal)))
                      (store      (session-store-open store-path "access-test" :recover-interrupted nil))
                      (owner      (make-instance 'access-session-test-owner))
                      (adapter    (make-access-session-service policy))
                      (runtime    (make-instance 'session-runtime :service owner :store store :connection nil
                                                 :access-service    adapter
                                                :features '("session.v1" "access.v1") :initial-sequence 0
                                                :output-callback nil :clock #'session-runtime--clock
                                                :lifecycle-callback (lambda (kind turn outcome)
                                                                      (push (list kind turn outcome) (first events))))))
                 (push adapter adapters)
                 (setf paths (append (list path store-path
                                          (session-store-lock-pathname store)
                                          (make-pathname :name (concatenate 'string (file-namestring path) ".lock")
                                                         :type nil :defaults path)) paths))
                 (session-access-attach adapter runtime)
                 (values adapter policy runtime owner input prompts store events)))

             (dispatch (adapter runtime id request)
               (with-session-runtime-owner (runtime)
                 (session-access-dispatch adapter runtime
                                          (parent-proto:make-envelope :message-id id :access-request request))))

             (cancel (adapter runtime id target)
               (with-session-runtime-owner (runtime)
                 (session-access-cancel adapter runtime
                  (parent-proto:make-envelope :message-id id :cancel-request
                                              (parent-proto:make-cancel-request :request-id target)))))

             (replies (runtime owner id)
               (with-session-runtime-owner (runtime)
                 (remove-if-not (lambda (reply) (= id (parent-proto:envelope.reply-to reply)))
                                (reverse (access-session-test-sent owner)))))

             (finished-p (adapter runtime id)
               (with-session-runtime-owner (runtime) (gethash id (access-session-terminal adapter)))))
      (unwind-protect
           (progn
             (multiple-value-bind (adapter policy runtime owner input prompts store events) (fixture ':wait)
               (check (dispatch adapter runtime 1 (request "approve" "/fixture/file")))
               (wait-for (lambda () (plusp (first prompts))))
               (check (eq (parent-proto:access-decision.state
                           (parent-proto:envelope.access-decision (first (replies runtime owner 1)))) ':pending-approval))
               (check (dispatch adapter runtime 2 (request "busy" "/fixture/second")))
               (check (eq (parent-proto:failure.code (parent-proto:envelope.failure (first (replies runtime owner 2))))
                          ':resource-limit))
               (setf (first input) "YES")
               (wait-for (lambda () (finished-p adapter runtime 1)))
               (let ((decision (parent-proto:envelope.access-decision (first (last (replies runtime owner 1))))))
                 (check (eq (parent-proto:access-decision.state decision) ':approved-pending-restart))
                 (check (null (parent-proto:access-decision.effective-grants decision))))
               (check (= (first prompts) 1))
               (check (= (access-session-restart-revision adapter) 1))
               (check (assoc ':access-restart-required (first events)))
               (check (= (length (records store)) 2))
               (check (every (lambda (record) (eq (parent-proto:session-record.origin record) ':parent)) (records store)))
               (check (= (access-service-project-notes policy store) 2))
               (check (= (length (records store)) 2))
               ;; Global parent notes do not open a turn. Notes during a turn do
               ;; not finish it, and replay does not create another turn.
               (session-store-start-turn store "turn-1" "input")
               (access-service-prepare-stop policy)
               (access-service-project-notes policy store)
               (session-store-finish-turn store "turn-1" ':completed)
               (let ((reopened (session-store-open (session-store-pathname store) "access-test" :recover-interrupted nil)))
                 (check (= (length (records store)) (length (records reopened))))
                 (session-store-start-turn reopened "turn-2" "next")
                 (session-store-finish-turn reopened "turn-2" ':completed))
               (session-access-quiesce adapter runtime))
             (multiple-value-bind (adapter policy runtime owner input prompts store events) (fixture ':wait)
               (declare (ignore policy input store events))
               (dispatch adapter runtime 10 (request "cancel" "/fixture/file"))
               (wait-for (lambda () (plusp (first prompts))))
               (check (cancel adapter runtime 11 10))
               (wait-for (lambda () (and (finished-p adapter runtime 10) (replies runtime owner 11))))
               (check (eq (parent-proto:failure.code
                           (parent-proto:envelope.failure (first (last (replies runtime owner 10))))) ':cancelled))
               (check (cancel adapter runtime 12 10))
               (check (parent-proto:cancel-ack.already-terminal
                       (parent-proto:envelope.cancel-ack (first (replies runtime owner 12)))))
               (check (not (cancel adapter runtime 13 999)))
               (session-access-quiesce adapter runtime))
             (multiple-value-bind (adapter policy runtime owner input prompts store events) (fixture "NO")
               (declare (ignore policy input prompts store events))
               (dispatch adapter runtime 20 (request "deny" "/fixture/file"))
               (wait-for (lambda () (finished-p adapter runtime 20)))
               (check (eq (parent-proto:access-decision.state
                           (parent-proto:envelope.access-decision (first (last (replies runtime owner 20))))) ':denied))
               (session-access-quiesce adapter runtime))
             (multiple-value-bind (adapter policy runtime owner input prompts store events) (fixture)
               (declare (ignore owner input store events))
               (let* ((original (request "source" "/fixture/directory/"))
                      (receipt  (request "receipt" "/fixture/directory/file" ':instance)))
                 (access-service-handle policy original)
                 (access-service-start policy "instance-A")
                 (let ((decision (access-service-handle policy receipt)))
                   (check (eq (parent-proto:access-decision.state decision) ':active))
                   (check (= (parent-proto:access-decision.policy-revision decision) 1))
                   (check (= (length (parent-proto:access-decision.effective-grants decision)) 1)))
                 (check (= (first prompts) 1))
                 (access-service-revoke policy "source")
                 (check (eq (parent-proto:access-decision.state (access-service-handle policy receipt)) ':expired))
                 (let* ((prepare  (access-service-prepare-stop policy))
                        (revision (parent-proto:prepare-stop.next-policy-revision prepare)))
                   (rejects (lambda () (access-service-handle policy (request "fenced" "/fixture/new"))) ':restart-in-progress)
                   (access-service-stop policy)
                   (rejects (lambda () (access-service-start policy "wrong" :expected-revision (1+ revision))) ':revision-conflict)
                   (check (null (parent-proto:access-decision.effective-grants
                                 (access-service-start policy "instance-B" :expected-revision revision)))))
                 (check (= (first prompts) 1)))
               (session-access-quiesce adapter runtime))
             checks)
        (dolist (adapter adapters)
          (ignore-errors (session-access-quiesce adapter (access-session-runtime adapter))))
        (dolist (path paths) (when (probe-file path) (delete-file path)))))))
