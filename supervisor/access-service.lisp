;;;; -- Durable Access Decisions and Restart Transitions --

(in-package #:autolith-supervisor)

(declaim (ftype (function (access-context) list) access-service--initial))

(defun access-service--initial (context)
  "Build the first private policy snapshot."
  (list :version 1 :session (access-context-session-id context) :revision 0
        :active-revision 0 :active nil :instance nil :phase ':stopped :closed nil
        :operations nil :notes nil :instances nil))

(declaim (ftype (function (t access-context) boolean) access-service--state-p))

(defun access-service--state-p (state context)
  "Validate all persisted policy data before using it."
  (and (access-policy--shape-p state '(:version :session :revision :active-revision :active
                                       :instance :phase :closed :operations :notes :instances))
       (eql (getf state :version) 1)
       (equal (getf state :session) (access-context-session-id context))
       (typep (getf state :revision) '(unsigned-byte 64))
       (typep (getf state :active-revision) '(unsigned-byte 64))
       (<= (getf state :active-revision) (getf state :revision))
       (member (getf state :phase) '(:stopped :active :restart :preparing :replacing :failed))
       (member (getf state :closed) '(t nil))
       (or (null (getf state :instance)) (access-policy--text-p (getf state :instance) 256))
       (listp (getf state :instances)) (ignore-errors (list-length (getf state :instances)))
       (every (lambda (id) (access-policy--text-p id 256)) (getf state :instances))
       (listp (getf state :active)) (ignore-errors (list-length (getf state :active)))
       (every #'access-policy--grant-p (getf state :active))
       (listp (getf state :operations)) (ignore-errors (list-length (getf state :operations)))
       (every
        (lambda (entry)
          (and (access-policy--shape-p entry '(:request :bound :state :revision :instance))
               (member (getf entry :state) '(:pending :denied :cancelled :approved :receipt :receipt-expired :failed :expired))
               (typep (getf entry :revision) '(unsigned-byte 64))
               (<= (getf entry :revision) (getf state :revision))
               (or (null (getf entry :instance))
                   (access-policy--text-p (getf entry :instance) 256))
               (access-policy--shape-p (getf entry :request) '(:id :reason :grants))
               (access-policy--text-p (getf (getf entry :request) :id) 256)
               (stringp (getf (getf entry :request) :reason))
               (let ((grants (getf (getf entry :request) :grants))
                     (bound  (getf entry :bound)))
                 (and (listp grants) (ignore-errors (<= 1 (list-length grants) 64))
                      (every #'access-policy--grant-p grants)
                      (listp bound) (ignore-errors (list-length bound))
                      (every #'access-policy--grant-p bound)))))
        (getf state :operations))
       (= (length (getf state :operations))
          (length (remove-duplicates (getf state :operations) :test #'equal
                                     :key                           (lambda (entry) (getf (getf entry :request) :id)))))
       (listp (getf state :notes)) (ignore-errors (list-length (getf state :notes)))
       (every (lambda (note)
                (and (access-policy--shape-p note '(:sequence :kind :operation-id :revision))
                     (typep (getf note :sequence) '(integer 1))
                     (member (getf note :kind) '(:pending :denied :cancelled :approved :failed :expired
                                                 :preparing :stopped :active :closed))
                     (stringp (getf note :operation-id))
                     (typep (getf note :revision) '(unsigned-byte 64))))
              (getf state :notes)) t))

(declaim (ftype (function (access-service function) t) access-service--transaction))

(defun access-service--transaction (service function)
  "Replay a validated private snapshot under thread and process locks."
  (let* ((context (access-service-context service))
         (path    (access-context-pathname context))
         (lock    (make-pathname :name (concatenate 'string (file-namestring path) ".lock")
                                 :type nil :defaults path)))
    (bordeaux-threads:with-lock-held ((access-service-lock service))
      (ls-flock:with-file-lock (lock)
        (let ((state nil))
          (when (probe-file path)
            (multiple-value-bind (position incomplete count)
                (sexp-store:log-map
                 (lambda (form)
                   (unless (access-service--state-p form context)
                     (access-policy--reject ':corrupt-store))
                   (setf state form)) path)
              (declare (ignore position count))
              (when incomplete (sexp-store:log-repair-tail path))))
          (funcall function (or state (access-service--initial context))))))))

(declaim (ftype (function (access-service list keyword &key (:operation-id string)) null)
                access-service--commit))

(defun access-service--commit (service state kind &key (operation-id ""))
  "Atomically append policy, operation identities, results and parent note data."
  (let ((notes (getf state :notes)))
    (setf (getf state :notes)
          (append notes (list (list :sequence (1+ (length notes)) :kind kind
                                   :operation-id operation-id :revision (getf state :revision))))))
  (unless (access-service--state-p state (access-service-context service))
    (access-policy--reject ':invalid-transition))
  (sexp-store:log-append (access-context-pathname (access-service-context service)) state
                         :repair-tail-p nil)
  nil)

(declaim (ftype (function (access-context) access-service) make-access-service))

(defun make-access-service (context)
  "Open a parent policy service. Do not infer running enforcement from disk data.
A reopened service must stop the recorded instance before starting another one.
Finish an interrupted prompt as FAILED, without another prompt or new approval."
  (let ((service (make-instance 'access-service :context context)))
    (ensure-directories-exist (access-context-pathname context))
    (access-service--transaction
     service
     (lambda (state)
       (dolist (entry (getf state :operations))
         (when (eq (getf entry :state) ':pending)
           (setf (getf entry :state) ':failed)
           (access-service--commit service state ':failed
                                   :operation-id (getf (getf entry :request) :id))))))
    service))

(declaim (ftype (function (list string) list) access-service--find))

(defun access-service--find (state id)
  "Look up a session-scoped operation identity."
  (find id (getf state :operations) :test #'equal
        :key                              (lambda (entry) (getf (getf entry :request) :id))))

(declaim (ftype (function (list) list) access-service--approved))

(defun access-service--approved (state)
  "Return the complete approved grant set, including pending replacement grants."
  (remove-duplicates
   (loop for entry in (getf state :operations)
         when (member (getf entry :state) '(:approved :expired))
           append (copy-tree (getf entry :bound)))
   :test #'equal))

(declaim (ftype (function (list list &key (:active boolean)) boolean) access-service--covered-p))

(defun access-service--covered-p (state grants &key active)
  "Check policy coverage without converting receipts into independent grants."
  (let ((approved (access-service--approved state)))
    (every (lambda (grant)
             (and (some (lambda (scope) (access-policy-grant-covers-p scope grant)) approved)
                  (or (not active)
                      (some (lambda (scope) (access-policy-grant-covers-p scope grant))
                            (getf state :active))))) grants)))

(declaim (ftype (function (access-service list list) cl-protobufs.autolith.parent.v1:access-decision)
                access-service--decision))

(defun access-service--decision (service state entry)
  "Report current disposition, never historical active grants."
  (let* ((status   (getf entry :state))
         (active   (and (member (getf state :phase) '(:active :restart :preparing))
                        (equal (access-service-owner service) (getf state :instance))
                        (getf state :instance)
                        (<= (getf entry :revision) (getf state :active-revision))
                        (access-service--covered-p state (getf entry :bound) :active t)))
         (decision (case status
                     (:pending
                      ':pending-approval)
                     (:denied
                      ':denied)
                     (:failed
                      ':failed)
                    ((:expired :receipt-expired)
                     ':expired)
                    ((:approved :receipt)
                     (if active ':active ':approved-pending-restart)))))
    (when (eq status ':cancelled) (access-policy--reject ':cancelled))
    (cl-protobufs.autolith.parent.v1:make-access-decision
     :state decision :policy-revision (getf state (if (eq decision ':active) :active-revision :revision))
     :effective-grants (if (eq decision ':active)
                           (map 'list #'access-policy--wire-grant (getf state :active)) nil)
     :explanation      (case decision
                         (:approved-pending-restart
                          "Approved policy requires a new restricted instance.")
                         (:active
                          "External enforcement is active for this instance.")
                         (:failed
                          "Approval or policy application did not complete.")
                         (:expired
                          "Grant lifetime ended or was revoked.")
                         (:denied
                          "The trusted user did not approve these grants.")
                         (otherwise
                          "Approval is pending.")))))

(declaim (ftype (function (access-service t &key (:control access-request-control))
                         cl-protobufs.autolith.parent.v1:access-decision) access-service-handle))

(defun access-service-handle (service request &key (control (make-access-request-control)))
  "Obtain exact trusted consent and commit one atomic grant batch.
CONTROL serializes cancellation against policy publication. Do not run the
normal terminal input reader during this call. Identical operation retries use
the durable outcome, without a new prompt. Conflicting identity reuse fails."
  (let* ((context (access-service-context service))
         (data    (access-policy--request-data context request))
         (id      (getf data :id)))
    (access-service--transaction
     service
     (lambda (state)
       (let ((entry (access-service--find state id)))
         (when entry
           (unless (equal data (getf entry :request))
             (access-policy--reject ':operation-conflict))
           (bordeaux-threads:with-lock-held ((access-request-control-lock control))
             (setf (access-request-control-disposition control)
                   (if (eq (getf entry :state) ':approved) ':committed ':terminal)))
           (return-from access-service-handle (access-service--decision service state entry))))
       (when (getf state :closed) (access-policy--reject ':session-closed))
       (when (member (getf state :phase) '(:preparing :replacing))
         (access-policy--reject ':restart-in-progress))
       (let* ((backend   (access-context-backend context))
              (bound
                (mapcar
                 (lambda (grant)
                   (let ((resolved (access-policy--check-grant
                                    context (access-backend-bind backend context (copy-tree grant)))))
                     (unless (and (eq (getf grant :resource) (getf resolved :resource))
                                  (eq (getf grant :duration) (getf resolved :duration))
                                  (equal (getf grant :operations) (getf resolved :operations))
                                  (or (eq (getf grant :resource) ':filesystem)
                                      (equal (getf grant :target) (getf resolved :target))))
                       (access-policy--reject ':backend-grant-mismatch))
                     resolved)) (getf data :grants)))
              (entry     (list :request data :bound bound :state ':pending
                               :revision (getf state :revision) :instance nil))
              (completed nil))
         (push entry (getf state :operations))
         (when (and (getf state :instance)
                    (equal (access-service-owner service) (getf state :instance))
                    (access-service--covered-p state bound :active t))
           (bordeaux-threads:with-lock-held ((access-request-control-lock control))
             (setf (getf entry :state) (if (access-request-control-cancelled control) ':cancelled ':receipt)
                   (getf entry :revision) (getf state :active-revision)
                   (getf entry :instance) (getf state :instance))
             (access-service--commit service state
                                     (if (eq (getf entry :state) ':receipt) ':active ':cancelled)
                                     :operation-id id)
             (setf (access-request-control-disposition control) ':terminal))
           (return-from access-service-handle (access-service--decision service state entry)))
         (access-service--commit service state ':pending :operation-id id)
         (unwind-protect
              (let ((approved
                      (and (not (access-request-control-cancelled-p control))
                           (handler-case
                               (parent-terminal-approve
                                (access-context-terminal context)
                                (map 'vector #'access-policy--wire-grant bound)
                                :operation-id id :reason (getf data :reason)
                                :cancelled (lambda () (access-request-control-cancelled-p control)))
                             (parent-terminal-error () nil)))))
                (bordeaux-threads:with-lock-held ((access-request-control-lock control))
                  (cond
                    ((access-request-control-cancelled control)
                     (setf (getf entry :state) ':cancelled))
                    (approved
                     (when (= (getf state :revision) (1- (expt 2 64)))
                       (access-policy--reject ':revision-exhausted))
                     (incf (getf state :revision))
                     (setf (getf entry :state) ':approved
                           (getf entry :revision) (getf state :revision)
                           (getf state :phase) ':restart))
                    (t
                     (setf (getf entry :state) ':denied)))
                  (access-service--commit service state (getf entry :state) :operation-id id)
                  (setf (access-request-control-disposition control)
                        (if (eq (getf entry :state) ':approved) ':committed ':terminal)
                        completed t))
                (access-service--decision service state entry))
           (unless completed
             (setf (getf entry :state) ':failed)
             (access-service--commit service state ':failed :operation-id id))))))))

(declaim (ftype (function (access-service &key (:reason string) (:grace-milliseconds (unsigned-byte 32)))
                         cl-protobufs.autolith.parent.v1:prepare-stop) access-service-prepare-stop))

(defun access-service-prepare-stop (service &key (reason "Access policy changed.") (grace-milliseconds 5000))
  "Pin the announced replacement revision and return the parent's PREPARESTOP.
Reject new approvals until replacement start. Existing operation retries report
current disposition. The dispatcher must fence input, cancel pending prompts,
wait for ReadyToStop or the grace deadline, then call ACCESS-SERVICE-STOP."
  (unless (and (stringp reason) (<= (length reason) 4096)
               (typep grace-milliseconds '(unsigned-byte 32)))
    (access-policy--reject ':invalid-stop))
  (access-service--transaction
   service
   (lambda (state)
     (when (getf state :closed) (access-policy--reject ':session-closed))
     (let ((next (+ (getf state :revision) (if (getf state :instance) 1 0))))
       (unless (typep next '(unsigned-byte 64)) (access-policy--reject ':revision-exhausted))
       (setf (getf state :phase) (if (getf state :instance) ':preparing ':replacing))
       (access-service--commit service state ':preparing)
       (cl-protobufs.autolith.parent.v1:make-prepare-stop
        :reason reason :grace-milliseconds grace-milliseconds :next-policy-revision next)))))

(declaim (ftype (function (access-service) null) access-service-stop))

(defun access-service-stop (service)
  "Stop the recorded instance through the backend before expiring its grants.
Keep active policy unchanged if cleanup fails. Expire instance grants only after
confirmed cleanup. Pending, unbound instance grants belong to the replacement."
  (access-service--transaction
   service
   (lambda (state)
     (let* ((context  (access-service-context service))
            (instance (getf state :instance))
            (prepared (member (getf state :phase) '(:preparing :replacing))))
       (when instance
         (unless (eq t (access-backend-stop (access-context-backend context) context instance))
           (access-policy--reject ':cleanup-unproved))
         (dolist (entry (getf state :operations))
           (when (and (member (getf entry :state) '(:approved :receipt))
                      (equal (getf entry :instance) instance))
             (let ((survivors (remove ':instance (getf entry :bound)
                                      :key (lambda (grant) (getf grant :duration)))))
               (unless (= (length survivors) (length (getf entry :bound)))
                (setf (getf entry :state) (if (eq (getf entry :state) ':receipt) ':receipt-expired ':expired))
                ;; Retain session survivors while the original batch reports EXPIRED.
                (setf (getf entry :bound) (copy-tree survivors))))))
         (incf (getf state :revision)))
       (setf (getf state :instance) nil (getf state :active) nil
             (getf state :phase) (if prepared ':replacing ':stopped)
             (access-service-owner service) nil)
       (access-service--commit service state ':stopped))))
  nil)

(declaim (ftype (function (access-service string &key (:expected-revision (or null (unsigned-byte 64))))
                         cl-protobufs.autolith.parent.v1:access-decision) access-service-start))

(defun access-service-start (service instance-id &key expected-revision)
  "Apply approved policy only to a fresh externally restricted replacement.
Stop the old instance first. A failed launch never activates an approved revision.
Return the complete active set for WELCOME only after exact backend evidence."
  (unless (access-policy--text-p instance-id 256) (access-policy--reject ':invalid-instance))
  (access-service--transaction
   service
   (lambda (state)
     (when (or (getf state :closed) (getf state :instance))
       (access-policy--reject ':stop-required))
     (when (and expected-revision (/= expected-revision (getf state :revision)))
       (access-policy--reject ':revision-conflict))
     (when (member instance-id (getf state :instances) :test #'equal)
       (access-policy--reject ':instance-reuse))
     (let* ((context   (access-service-context service))
            (backend   (access-context-backend context))
            (revision  (getf state :revision))
            (grants    (access-service--approved state))
            (completed nil))
       (dolist (grant grants)
         (access-policy--check-grant context grant)
         (unless (equal grant (access-backend-bind backend context (copy-tree grant)))
           (access-policy--reject ':stale-binding)))
       ;; Persist the recovery identity before starting a process. A parent crash
       ;; must not permit a second launch beside an unrecorded first instance.
       (push (copy-seq instance-id) (getf state :instances))
       (setf (getf state :instance) (copy-seq instance-id) (getf state :phase) ':failed)
       (access-service--commit service state ':pending)
       (unwind-protect
            (let ((application (access-backend-start backend context instance-id :revision revision
                                                     :grants                               (copy-tree grants))))
              (unless (and (typep application 'access-application)
                           (eq backend (access-application-backend application))
                           (equal instance-id (access-application-instance-id application))
                           (eql revision (access-application-revision application))
                           (equal grants (access-application-grants application)))
                (access-policy--reject ':application-unproved))
              (dolist (entry (getf state :operations))
                (when (and (eq (getf entry :state) ':approved) (null (getf entry :instance)))
                  (setf (getf entry :instance) (copy-seq instance-id))))
              (setf (getf state :active) grants (getf state :active-revision) revision
                    (getf state :phase) ':active)
              (access-service--commit service state ':active)
              (setf (access-service-owner service) (copy-seq instance-id) completed t)
              (cl-protobufs.autolith.parent.v1:make-access-decision
               :state ':active :policy-revision revision
               :effective-grants (map 'list #'access-policy--wire-grant grants)))
         (unless completed
           (setf (getf state :phase) ':failed (getf state :active) nil)
           (when (eq t (access-backend-stop backend context instance-id))
             (setf (getf state :instance) nil))
           (access-service--commit service state ':failed)))))))

(declaim (ftype (function (access-service string) null) access-service-revoke))

(defun access-service-revoke (service operation-id)
  "Revoke a trusted user's batch through the same mandatory restart path.
Do not call from agent dispatch. Do not mutate the running sandbox in place."
  (access-service--transaction
   service
   (lambda (state)
     (let ((entry (access-service--find state operation-id)))
       (unless entry (access-policy--reject ':unknown-operation))
       (when (member (getf state :phase) '(:preparing :replacing))
         (access-policy--reject ':restart-in-progress))
       (when (member (getf entry :state) '(:receipt :receipt-expired))
         (access-policy--reject ':receipt-not-grant))
       (unless (and (eq (getf entry :state) ':expired) (null (getf entry :bound)))
         (setf (getf entry :state) ':expired (getf entry :bound) nil
               (getf state :phase) ':restart)
         (incf (getf state :revision))
         (dolist (receipt (getf state :operations))
           (when (and (eq (getf receipt :state) ':receipt)
                      (not (access-service--covered-p state (getf receipt :bound))))
             (setf (getf receipt :state) ':receipt-expired (getf receipt :bound) nil)))
         (access-service--commit service state ':expired :operation-id operation-id)))))
  nil)

(declaim (ftype (function (access-service) list) access-service-notes))

(defun access-service-notes (service)
  "Return durable parent-authored outcomes for session-note projection.
Deduplicate projection by session identity and note sequence. No agent supplies
these notes. The session dispatcher must append them before replacement input."
  (access-service--transaction service (lambda (state) (copy-tree (getf state :notes)))))

(declaim (ftype (function (access-service) null) access-service-close))

(defun access-service-close (service)
  "Stop all contained processes, then expire all grants and close the session."
  (access-service-stop service)
  (access-service--transaction
   service
   (lambda (state)
     (dolist (entry (getf state :operations))
       (when (member (getf entry :state) '(:approved :expired :receipt :receipt-expired))
         (setf (getf entry :state) (if (member (getf entry :state) '(:receipt :receipt-expired))
                                     ':receipt-expired ':expired)
               (getf entry :bound) nil)))
     (setf (getf state :closed) t)
     (access-service--commit service state ':closed)))
  nil)
