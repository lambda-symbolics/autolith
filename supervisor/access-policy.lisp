;;;; -- Parent Access Policy --

(in-package #:autolith-supervisor)

(define-condition access-policy-error (error)
  ((code :initarg :code :reader access-policy-error-code
         :documentation "A fixed code without private path data."))
  (:documentation "A rejected access request or policy transition.")
  (:report (lambda (condition stream)
             (format stream "Access policy failed: ~A" (access-policy-error-code condition)))))

(defclass access-backend () ()
  (:documentation "Trusted whole-agent enforcement adapter.
An adapter must bind paths against link and replacement attacks, exclude parent
private roots, contain descendants, and constrain DNS and redirects for network
rules. A command sandbox or process group alone does not satisfy this contract.
The base backend refuses every grant and launch. No agent supplies an adapter."))

(defgeneric access-backend-bind (backend context grant)
  (:documentation "Return an exact bound grant as a portable plist.
Signal ACCESS-POLICY-ERROR before approval if the host cannot enforce this grant.
Keep the resource, operations and duration unchanged. Resolve filesystem targets
under the host's link and replacement rules. Check private-root exclusion even
for directory ancestors. Service IDs must come from the context allowlist."))

(defgeneric access-backend-start (backend context instance-id &key revision grants)
  (:documentation "Launch a new restricted agent and return ACCESS-APPLICATION.
Return only after external enforcement is installed for this exact revision and
all descendants. On failure, remove the failed instance before signalling. Never
load agent Lisp in the parent. GRANTS is the entire approved grant set."))

(defgeneric access-backend-stop (backend context instance-id)
  (:documentation "Stop INSTANCE-ID and all contained descendants; return exactly T.
Signal on failed cleanup. The service will not permit a replacement on failure."))

(defclass access-application ()
  ((backend :initarg :backend :reader access-application-backend
            :documentation "The trusted adapter that installed enforcement.")
   (instance-id :initarg :instance-id :reader access-application-instance-id
                :documentation "The replacement instance bound to enforcement.")
   (revision :initarg :revision :reader access-application-revision
             :documentation "The exact externally installed approved revision.")
   (grants :initarg :grants :reader access-application-grants
           :documentation "The complete installed portable grant snapshot."))
  (:documentation "Parent-only evidence returned by a trusted backend.
This is a typed adapter contract, not evidence supplied by the agent or wire."))

(defclass access-context ()
  ((pathname :initarg :pathname :reader access-context-pathname
             :documentation "Private policy log, outside every agent grant.")
   (session-id :initarg :session-id :reader access-context-session-id
               :documentation "Trusted durable session identity.")
   (workspace :initarg :workspace :reader access-context-workspace
              :documentation "Absolute trusted workspace root.")
   (private-roots :initarg :private-roots :reader access-context-private-roots
                  :documentation "Canonical forbidden directory roots, including control state.")
   (private-authorities :initarg :private-authorities :reader access-context-private-authorities
                        :documentation "Forbidden network authorities, including the control endpoint.")
   (services :initarg :services :reader access-context-services
             :documentation "Exact parent-configured service IDs.")
   (backend :initarg :backend :reader access-context-backend
            :documentation "Trusted external enforcement adapter.")
   (terminal :initarg :terminal :reader access-context-terminal
             :documentation "Parent-owned terminal for exact-grant approval."))
  (:documentation "Explicit trusted dependencies. No agent configuration is evaluated."))

(defclass access-request-control ()
  ((lock :initform (bordeaux-threads:make-lock "access commit") :reader access-request-control-lock
         :documentation "Serialize cancellation against the durable policy commit.")
   (cancelled :initform nil :accessor access-request-control-cancelled
              :documentation "Cancellation accepted before policy commit.")
   (disposition :initform ':pending :accessor access-request-control-disposition
                :documentation "PENDING, COMMITTED policy, or TERMINAL rejection."))
  (:documentation "Parent-owned cancellation state. No wire data contains Lisp callbacks."))

(declaim (ftype (function () (values access-request-control &optional)) make-access-request-control))

(defun make-access-request-control ()
  "Make a fresh cancellation/commit fence for one access request."
  (make-instance 'access-request-control))

(declaim (ftype (function (access-request-control) keyword) access-request-control-cancel))

(defun access-request-control-cancel (control)
  "Cancel before commit, or report :COMMITTED or :TERMINAL without revoking policy."
  (bordeaux-threads:with-lock-held ((access-request-control-lock control))
    (case (access-request-control-disposition control)
      (:committed
       ':committed)
      (:terminal
       ':terminal)
      (otherwise
       (setf (access-request-control-cancelled control) t)
       ':cancelled))))

(declaim (ftype (function (access-request-control) boolean) access-request-control-cancelled-p))

(defun access-request-control-cancelled-p (control)
  "Read the trusted cancellation flag under the same short lock as commit."
  (bordeaux-threads:with-lock-held ((access-request-control-lock control))
    (access-request-control-cancelled control)))

(defclass access-service ()
  ((context :initarg :context :reader access-service-context
            :documentation "Trusted dependencies.")
   (lock :initform (bordeaux-threads:make-lock "parent-access") :reader access-service-lock
         :documentation "Serialize local transactions and prompts.")
   (owner :initform nil :accessor access-service-owner
          :documentation "Non-durable current instance owned by this service."))
  (:documentation "Durable approved policy, separate from externally active policy.
Every transaction replays the private log under a process-shared file lock.
Persist pending approval before prompting. A crash during approval fails closed
and never prompts again for that operation. Acknowledgement has sexp-store's
publication guarantees, not stronger power-loss guarantees."))

(declaim (ftype (function (keyword) nil) access-policy--reject))

(defun access-policy--reject (code)
  "Signal a fixed access failure."
  (error 'access-policy-error :code code))

(defmethod access-backend-bind ((backend access-backend) context grant)
  "Refuse unsupported whole-agent enforcement before prompting."
  (declare (ignore context grant))
  (access-policy--reject ':unsupported-backend))

(defmethod access-backend-start ((backend access-backend) context instance-id &key revision grants)
  "Refuse a launch without external enforcement."
  (declare (ignore context instance-id revision grants))
  (access-policy--reject ':unsupported-backend))

(defmethod access-backend-stop ((backend access-backend) context instance-id)
  "Refuse unproved descendant cleanup."
  (declare (ignore context instance-id))
  (access-policy--reject ':unsupported-backend))

(declaim (ftype (function (t integer) boolean) access-policy--text-p))

(defun access-policy--text-p (value limit)
  "Recognize bounded nonempty text without control characters."
  (and (stringp value) (<= 1 (length value) limit)
       (every (lambda (character) (let ((code (char-code character)))
                                   (and (>= code 32) (/= code 127)))) value)))

(declaim (ftype (function (t) boolean) access-policy--absolute-path-p))

(defun access-policy--absolute-path-p (target)
  "Recognize literal absolute host paths without wildcard or parent components."
  (and (access-policy--text-p target 4096)
       (not (find-if (lambda (character) (find character "*?[]")) target))
       (handler-case
           (let ((path (uiop:parse-native-namestring target)))
             (and (uiop:absolute-pathname-p path)
                  (not (intersection (pathname-directory path) '(:up :back))) t))
         (error () nil))))

(declaim (ftype (function (string string) boolean) access-policy--overlap-p))

(defun access-policy--overlap-p (target root)
  "Return whether a literal target overlaps a forbidden directory in either direction."
  (let ((path (uiop:parse-native-namestring target))
        (base (uiop:ensure-directory-pathname (uiop:parse-native-namestring root))))
    (or (equal (uiop:native-namestring path) (uiop:native-namestring base))
        (and (uiop:subpathp path base) t)
        (and (uiop:subpathp base (uiop:ensure-directory-pathname path)) t))))

(declaim (ftype (function (t list) boolean) access-policy--shape-p))

(defun access-policy--shape-p (value keys)
  "Recognize a finite plist with exactly one of each key."
  (and (listp value)
       (let ((size (ignore-errors (list-length value))))
         (and size (= size (* 2 (length keys)))
              (every (lambda (tail) (keywordp (first tail)))
                     (loop for tail on value by (lambda (tail) (rest (rest tail))) collect tail))
              (equal (sort (loop for tail on value by (lambda (tail) (rest (rest tail))) collect (first tail))
                           #'string< :key #'symbol-name)
                     (sort (copy-list keys) #'string< :key #'symbol-name))))))

(declaim (ftype (function (t) boolean) access-policy--grant-p))

(defun access-policy--grant-p (grant)
  "Validate the portable exact grant shape and supported operation vocabulary."
  (and (access-policy--shape-p grant '(:resource :target :operations :duration))
       (member (getf grant :duration) '(:instance :session))
       (access-policy--text-p (getf grant :target) 4096)
       (let* ((resource   (getf grant :resource))
              (operations (getf grant :operations))
              (allowed    (case resource
                            (:filesystem
                             '("read" "write" "execute"))
                            ((:network :service)
                             '("connect")))))
         (and allowed (listp operations)
              (ignore-errors (<= 1 (list-length operations) 3))
              (every (lambda (item) (member item allowed :test #'equal)) operations)
              (= (length operations) (length (remove-duplicates operations :test #'equal)))
              (or (not (eq resource ':filesystem))
                  (access-policy--absolute-path-p (getf grant :target))) t))))

(declaim (ftype (function (list list) boolean) access-policy-grant-covers-p))

(defun access-policy-grant-covers-p (active requested)
  "Check exact authority, operations and lifetime without adding new permission.
Only a backend-bound directory target covers descendants. Session authority can
cover an instance receipt; instance authority cannot cover a session receipt."
  (and (eq (getf active :resource) (getf requested :resource))
       (or (eq (getf active :duration) ':session)
           (eq (getf requested :duration) ':instance))
       (subsetp (getf requested :operations) (getf active :operations) :test #'equal)
       (let ((target (getf active :target)) (needed (getf requested :target)))
         (or (string= target needed)
             (and (eq (getf active :resource) ':filesystem)
                  (uiop:directory-pathname-p (uiop:parse-native-namestring target))
                  (uiop:subpathp (uiop:parse-native-namestring needed)
                                 (uiop:parse-native-namestring target)) t)))))

(declaim (ftype (function (access-context list) list) access-policy--check-grant))

(defun access-policy--check-grant (context grant)
  "Validate a requested or backend-bound grant against trusted forbidden authority."
  (unless (access-policy--grant-p grant) (access-policy--reject ':invalid-grant))
  (let ((target (getf grant :target)))
    (case (getf grant :resource)
      (:filesystem
       (when (some (lambda (root) (access-policy--overlap-p target root))
                   (access-context-private-roots context))
         (access-policy--reject ':private-authority)))
      (:service
       (unless (member target (access-context-services context) :test #'string=)
         (access-policy--reject ':unknown-service)))
      (:network
       (when (member target (access-context-private-authorities context) :test #'string-equal)
         (access-policy--reject ':private-authority))
       (let* ((colon (position #\: target :from-end t))
              (port  (and colon (ignore-errors (parse-integer target :start (1+ colon))))))
         (unless (and colon (plusp colon) port (<= 1 port 65535)
                      (not (find-if (lambda (character) (find character "/\\@?#* ")) target)))
           (access-policy--reject ':invalid-authority)))))
    (copy-tree grant)))

(declaim (ftype (function (list) cl-protobufs.autolith.parent.v1:grant) access-policy--wire-grant))

(defun access-policy--wire-grant (grant)
  "Build a detached wire grant from trusted portable policy data."
  (cl-protobufs.autolith.parent.v1:make-grant
   :resource (getf grant :resource) :target (copy-seq (getf grant :target))
   :operations (map 'list #'copy-seq (getf grant :operations)) :duration (getf grant :duration)))

(declaim (ftype (function (access-context t) list) access-policy--request-data))

(defun access-policy--request-data (context request)
  "Validate and copy a typed request before any callback or persistence."
  (unless (typep request 'cl-protobufs.autolith.parent.v1:access-request)
    (access-policy--reject ':invalid-request))
  (let ((id     (cl-protobufs.autolith.parent.v1:access-request.operation-id request))
        (reason (cl-protobufs.autolith.parent.v1:access-request.reason request))
        (grants (cl-protobufs.autolith.parent.v1:access-request.requested-grants request)))
    (unless (and (access-policy--text-p id 256) (stringp reason) (<= (length reason) 32768)
                 (<= 1 (length grants) 64))
      (access-policy--reject ':invalid-request))
    (list :id (copy-seq id) :reason (copy-seq reason)
          :grants
          (map 'list
               (lambda (grant)
                 (unless (typep grant 'cl-protobufs.autolith.parent.v1:grant)
                   (access-policy--reject ':invalid-grant))
                 (access-policy--check-grant
                  context
                  (list :resource   (cl-protobufs.autolith.parent.v1:grant.resource grant)
                        :target     (copy-seq (cl-protobufs.autolith.parent.v1:grant.target grant))
                        :operations (sort (map 'list #'copy-seq
                                              (cl-protobufs.autolith.parent.v1:grant.operations grant)) #'string<)
                        :duration   (cl-protobufs.autolith.parent.v1:grant.duration grant)))) grants))))

(declaim (ftype (function (&key (:pathname pathname) (:session-id string) (:workspace string)
                              (:private-roots list) (:private-authorities list) (:services list)
                              (:backend access-backend) (:terminal parent-terminal))
                         (values access-context &optional)) make-access-context))

(defun make-access-context (&key pathname session-id workspace private-roots private-authorities
                                 services backend terminal)
  "Build trusted dependencies. Include all private roots and control authorities.
The backend must also exclude private network addresses after name resolution."
  (unless (and pathname (uiop:absolute-pathname-p pathname)
               (access-policy--text-p session-id 256)
               (access-policy--absolute-path-p workspace)
               private-roots (every #'access-policy--absolute-path-p private-roots)
               (some (lambda (root) (access-policy--overlap-p (uiop:native-namestring pathname) root))
                     private-roots)
               (every (lambda (id) (access-policy--text-p id 4096)) private-authorities)
               (every (lambda (id) (access-policy--text-p id 256)) services)
               (typep backend 'access-backend) (typep terminal 'parent-terminal))
    (access-policy--reject ':invalid-context))
  (values (make-instance 'access-context :pathname pathname :session-id (copy-seq session-id)
                         :workspace (copy-seq workspace) :private-roots (mapcar #'copy-seq private-roots)
                         :private-authorities (mapcar #'copy-seq private-authorities)
                         :services (mapcar #'copy-seq services) :backend backend :terminal terminal)))
