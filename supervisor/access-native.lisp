;;;; -- Native Whole-Agent Access Enforcement --

(in-package #:autolith-supervisor)

(defclass native-access-backend (access-backend)
  ((network-p :initarg :network-p :initform nil :reader access-native-network-p
              :documentation "Explicit parent-owned session-wide network authority.")
   (command :initarg :command :reader access-native--command
            :documentation "Trusted child argv, copied at construction.")
   (environment :initarg :environment :reader access-native--environment
                :documentation "Explicit non-secret child environment.")
   (runtime-roots :initarg :runtime-roots :reader access-native--runtime-roots
                  :documentation "Trusted runtime directories with read and execute access.")
   (temporary-root :initarg :temporary-root :reader access-native--temporary-root
                   :documentation "Dedicated child temporary directory.")
   (startup :initarg :startup :reader access-native--startup
            :documentation "Trusted callback accepting the application and completing Ready.")
   (startup-timeout :initarg :startup-timeout :reader access-native--startup-timeout
                    :documentation "Maximum seconds to finish protocol startup.")
   (cleanup-timeout :initarg :cleanup-timeout :reader access-native--cleanup-timeout
                    :documentation "Maximum seconds to remove the contained process group.")
   (instances :initform (make-hash-table :test 'equal) :reader access-native--instances
              :documentation "Parent-only process ownership, including stopped identities.")
   (lock :initform (bordeaux-threads:make-lock "native access") :reader access-native--lock
         :documentation "Serialize launch and cleanup ownership."))
  (:documentation "Immutable launch configuration and parent-only native process ownership.
The base class refuses all host operations. A host adapter must install external
restrictions before agent code starts. Policy changes require a new process.
Runtime roots must contain only public executable code, not parent state."))

(defclass native-access-application (access-application)
  ((process :initform nil :accessor native-access-application-process
            :documentation "Owned private process, installed before the startup callback.")
   (context :initarg :context :reader access-native--application-context
            :documentation "Exact trusted context that owns this instance.")
   (profile :initarg :profile :reader access-native--application-profile
            :documentation "Immutable external policy text installed at launch.")
   (state :initform ':starting :accessor access-native--application-state
          :documentation "Starting, running, stopped, or uncertain cleanup state.")
   (evidence :initform nil :accessor native-access-application-evidence
             :documentation "Cleanup evidence bound to this instance and policy revision."))
  (:documentation "External enforcement and process ownership for one replacement instance."))

(defgeneric access-native--host-class (host)
  (:documentation "Return an implemented native adapter class or refuse the host."))

(defgeneric access-native--canonical-path (backend target)
  (:documentation "Resolve an existing literal path through the native host adapter."))

(defgeneric access-native--profile (backend context grants)
  (:documentation "Return an immutable OS profile after validating every authority."))

(defgeneric access-native--launch-command (backend profile)
  (:documentation "Prepend the trusted host policy installer to the child argv."))

(defgeneric access-native--cleanup-proof (backend evidence)
  (:documentation "Validate the native scope and return the installed external policy kind."))

(defgeneric access-native--open (backend application)
  (:documentation "Launch the externally restricted application and finish protocol startup."))

(defgeneric access-native--close (backend application)
  (:documentation "Remove the application scope and return host-verified cleanup evidence."))

(defmethod access-native--open ((backend native-access-backend) application)
  "Open the explicit private connection and install ownership before startup."
  (private-process-open
   (access-native--launch-command backend (access-native--application-profile application))
   :instance-id     (access-application-instance-id application)
   :environment     (copy-tree (access-native--environment backend))
   :directory       (access-context-workspace (access-native--application-context application))
   :startup-timeout (access-native--startup-timeout backend)
   :cleanup-timeout (access-native--cleanup-timeout backend)
   :startup         (lambda (process)
                      (setf (native-access-application-process application) process)
                      (funcall (access-native--startup backend) application))))

(defmethod access-native--close ((backend native-access-backend) application)
  "Close the private process and retain its native scope proof."
  (private-process-close (native-access-application-process application)))

(defmethod access-native--host-class (host)
  "Refuse hosts without a concrete enforcement adapter."
  (declare (ignore host))
  (access-policy--reject ':unsupported-native-host))

(defmethod access-native--canonical-path ((backend native-access-backend) target)
  "Refuse path binding without native resolution."
  (declare (ignore target))
  (access-policy--reject ':unsupported-native-host))

(defmethod access-native--profile ((backend native-access-backend) context grants)
  "Refuse compilation without an external policy implementation."
  (declare (ignore context grants))
  (access-policy--reject ':unsupported-native-host))

(defmethod access-native--launch-command ((backend native-access-backend) profile)
  "Refuse launch without an external policy installer."
  (declare (ignore profile))
  (access-policy--reject ':unsupported-native-host))

(defmethod access-native--cleanup-proof ((backend native-access-backend) evidence)
  "Refuse cleanup evidence without a native scope contract."
  (declare (ignore evidence))
  (access-policy--reject ':cleanup-unproved))

(defgeneric access-native-check-network (backend enabled)
  (:documentation "Validate explicit session-wide network policy before consent or launch."))

(defmethod access-native-check-network ((backend native-access-backend) enabled)
  "Refuse broad network authority without a host implementation."
  (when enabled (access-policy--reject ':unsupported-native-network))
  t)

(defmethod initialize-instance :after ((backend native-access-backend) &key)
  "Check immutable network authority before any guest can start."
  (access-native-check-network backend (access-native-network-p backend)))

(declaim (ftype (function (&key (:command list) (:environment list) (:runtime-roots list)
                              (:temporary-root string) (:startup function)
                              (:network-p boolean) (:startup-timeout real) (:cleanup-timeout real))
                         native-access-backend) make-native-access-backend))

(defun make-native-access-backend (&key command (environment nil environment-p) runtime-roots
                                    temporary-root startup (network-p nil)
                                    (startup-timeout 10) (cleanup-timeout 5))
  "Build a trusted host backend without launching an agent.
STARTUP accepts NATIVE-ACCESS-APPLICATION and must finish Hello/Welcome/Ready.
Use its inherited connection, exact revision, grants and instance identity.
The callback executes in the parent; it must never load or evaluate agent code.
Supply only public runtime directories and an explicit non-secret environment.
Unsupported hosts fail here, before approval. Darwin denies posix_spawn, setsid
and setpgid to keep fork/exec workers inside the owned cleanup group."
  (supervisor--validate-argv command 'command)
  (unless (and environment-p (listp environment)
               (ignore-errors (list-length environment)) (every #'stringp environment)
               (listp runtime-roots) (ignore-errors (list-length runtime-roots))
               runtime-roots (every #'access-policy--absolute-path-p runtime-roots)
               (access-policy--absolute-path-p temporary-root)
               (member network-p '(nil t))
               (functionp startup) (plusp startup-timeout) (plusp cleanup-timeout))
    (access-policy--reject ':invalid-native-configuration))
  (handler-case (private-process--environment environment "native-validation")
    (private-process-error () (access-policy--reject ':invalid-native-environment)))
  (let ((host (cond ((uiop:os-macosx-p)
                     ':darwin)
                    ((member :linux *features*)
                     ':linux)
                    ((uiop:os-windows-p)
                     ':windows)
                    (t
                     ':unsupported))))
    (make-instance (access-native--host-class host)
                   :command (mapcar #'copy-seq command) :environment (mapcar #'copy-seq environment)
                   :network-p network-p :runtime-roots (mapcar #'copy-seq runtime-roots)
                   :temporary-root (copy-seq temporary-root) :startup startup
                   :startup-timeout startup-timeout :cleanup-timeout cleanup-timeout)))

(declaim (ftype (function (native-access-backend access-context string) string)
                access-native--bound-path))

(defun access-native--bound-path (backend context target)
  "Bind an existing path by canonical name and exclude both forms of private roots.
A directory grants its canonical subtree. A file grants only its canonical name.
The OS must check resolved paths on each access, including after replacement."
  (unless (access-policy--absolute-path-p target)
    (access-policy--reject ':invalid-filesystem-target))
  (let ((canonical (access-native--canonical-path backend target)))
    (dolist (root (access-context-private-roots context))
      (let ((resolved (access-native--canonical-path backend root)))
        (when (or (access-policy--overlap-p canonical resolved)
                  (access-policy--overlap-p canonical root))
          (access-policy--reject ':private-authority))))
    canonical))

(declaim (ftype (function (list &key (:case-insensitive-p boolean)) list)
                access-native--normalize-scopes))

(defun access-native--normalize-scopes (scopes &key case-insensitive-p)
  "Return the RX/RWX union as detached parent-first scopes.
Each input is (canonical-path writable-p). Keep writable descendants of read-only
parents. Remove read-only descendants of writable parents and duplicate scopes."
  (let ((same (if case-insensitive-p #'string-equal #'string=)) (unique nil))
    (labels ((covers-p (root path)
               (or (funcall same root path)
                   (and (find (char root (1- (length root))) '(#\/ #\\))
                        (<= (length root) (length path))
                        (funcall same root path :end2 (length root))))))
      (dolist (scope scopes)
        (let ((prior (assoc (first scope) unique :test same)))
          (if prior
              (setf (second prior) (or (second prior) (not (null (second scope)))))
              (push (list (copy-seq (first scope)) (not (null (second scope)))) unique))))
      (sort (remove-if (lambda (scope)
                         (some (lambda (other)
                                 (and (not (eq scope other))
                                      (or (second other) (not (second scope)))
                                      (covers-p (first other) (first scope)))) unique)) unique)
            #'< :key (lambda (scope) (length (first scope)))))))

(declaim (ftype (function (list) list) access-native--copy-grants))

(defun access-native--copy-grants (grants)
  "Detach a validated policy snapshot, including every mutable string."
  (mapcar (lambda (grant)
            (list :resource (getf grant :resource) :target (copy-seq (getf grant :target))
                  :operations (mapcar #'copy-seq (getf grant :operations))
                  :duration   (getf grant :duration))) grants))

(defmethod access-backend-bind ((backend native-access-backend) context grant)
  "Bind exact filesystem authority; refuse network and service grants before approval."
  (access-policy--check-grant context grant)
  (unless (eq (getf grant :resource) ':filesystem)
    (access-policy--reject ':unsupported-native-authority))
  (let ((bound (first (access-native--copy-grants (list grant)))))
    (setf (getf bound :target) (access-native--bound-path backend context (getf grant :target)))
    bound))

(declaim (ftype (function (native-access-backend access-context string) native-access-application)
                access-native-application))

(defun access-native-application (backend context instance-id)
  "Return parent-owned process evidence for this exact context and instance."
  (let ((application (gethash instance-id (access-native--instances backend))))
    (unless (and application (eq context (access-native--application-context application)))
      (access-policy--reject ':unknown-native-instance))
    application))

(declaim (ftype (function (native-access-application) boolean) access-native--stop-application))

(defun access-native--stop-application (application)
  "Remove the owned scope; never infer cleanup from a lost process handle."
  (unless (eq (access-native--application-state application) ':stopped)
    (let ((process (native-access-application-process application)))
      (unless process (access-policy--reject ':cleanup-unproved))
      (let* ((evidence (access-native--close (access-application-backend application) application))
             (kind     (access-native--cleanup-proof (access-application-backend application) evidence)))
       (setf (native-access-application-evidence application)
             (list :instance-id (copy-seq (access-application-instance-id application))
                   :revision    (access-application-revision application)
                   :external-policy kind :process-scope (copy-tree evidence))
             (access-native--application-state application) ':stopped))))
  t)

(defmethod access-backend-start ((backend native-access-backend) context instance-id
                                 &key revision grants)
  "Install immutable external policy and complete startup before returning evidence."
  (unless (and (access-policy--text-p instance-id 256)
               (typep revision '(unsigned-byte 64))
               (listp grants) (ignore-errors (<= (list-length grants) 64))
               (every #'access-policy--grant-p grants))
    (access-policy--reject ':invalid-native-instance))
  (bordeaux-threads:with-lock-held ((access-native--lock backend))
    (when (gethash instance-id (access-native--instances backend))
      (access-policy--reject ':instance-reuse))
    (maphash (lambda (id application)
               (declare (ignore id))
               (unless (eq (access-native--application-state application) ':stopped)
                 (access-policy--reject ':stop-required)))
             (access-native--instances backend))
    (dolist (grant grants)
      (unless (equal grant (access-backend-bind backend context grant))
        (access-policy--reject ':stale-binding)))
    (let* ((profile     (access-native--profile backend context grants))
           (application (make-instance 'native-access-application :backend backend
                                       :context context :instance-id (copy-seq instance-id)
                                       :revision revision :grants (access-native--copy-grants grants)
                                       :profile                            profile))
           (completed   nil))
      (setf (gethash instance-id (access-native--instances backend)) application)
      (unwind-protect
           (progn
             (access-native--open backend application)
             (setf (access-native--application-state application) ':running completed t)
             application)
        (unless completed
          (setf (access-native--application-state application) ':uncertain)
          (when (native-access-application-process application)
            (access-native--stop-application application)))))))

(defmethod access-backend-stop ((backend native-access-backend) context instance-id)
  "Prove cleanup for this context and instance before permitting a replacement."
  (bordeaux-threads:with-lock-held ((access-native--lock backend))
    (access-native--stop-application (access-native-application backend context instance-id))))
