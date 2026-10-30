;;;; -- Explicit Trusted Parent Application --

(in-package #:autolith-supervisor)

(defclass parent-application ()
  ((configuration :initarg :configuration :reader parent-application-configuration
                  :documentation "Trusted owner configuration.")
   (session-id :initarg :session-id :reader parent-application-session-id
               :documentation "Durable session identity selected by the owner.")
   (directory :initarg :directory :reader parent-application-directory
              :documentation "Private session directory.")
   (profiles :initarg :profiles :reader parent-application-profiles
             :documentation "Public model profile registry, retained in the parent.")
   (accounts :initarg :accounts :reader parent-application-accounts
             :documentation "Private account bindings, never sent to the child.")
   (provider-registry :initarg :provider-registry :reader parent-application-provider-registry
                      :documentation "Executable provider registrations retained only in the parent.")
   (provider-paths :initarg :provider-paths :reader parent-application-provider-paths
                   :documentation "Resolved extension files excluded from every child scope.")
   (input-source :initarg :input-source :reader parent-application-input-source
                 :documentation "Trusted polling terminal source.")
   (renderer :initarg :renderer :reader parent-application-renderer
             :documentation "Trusted output and approval renderer.")
   (input-runner :initform nil :accessor parent-application-input-runner
                 :documentation "Optional current-interface controller called with this application and launch.")
   (exclusive-input :initform #'funcall :accessor parent-application-exclusive-input
                    :documentation "Trusted complete-modal input ownership wrapper.")
   (modal-reader :initform nil :accessor parent-application-modal-reader
                 :documentation "Complete parent editor approval reader, never an agent callback.")
   (recovery-handler :initform nil :accessor parent-application-recovery-handler
                     :documentation "Trusted post-cleanup failure UI; true requests a fresh isolated child.")
   (after-stop :initform nil :accessor parent-application-after-stop
               :documentation "Trusted deferred image/configuration change after native cleanup.")
   (operation-handlers :initarg :operation-handlers :initform nil :accessor parent-application-operation-handlers
                       :documentation "Parent-only fixed operation handlers for the current session.")
   (event-callback :initarg :event-callback :initform nil :reader parent-application-event-callback
                   :documentation "Trusted typed presentation event receiver.")
   (notification-callback :initarg :notification-callback :initform nil
                          :reader        parent-application-notification-callback
                          :documentation "Trusted typed completion-ready notification receiver.")
   (knowledge-approval :initarg :knowledge-approval :reader parent-application-knowledge-approval
                       :documentation "Trusted owner policy for exact rule and contract admission.")
   (url-evidence :initarg :url-evidence :reader parent-application-url-evidence
                 :documentation "Trusted retrieval receipt lookup for knowledge evidence.")
   (policy :initform nil :accessor parent-application-policy
           :documentation "Persistent external base and optional grant policy.")
   (backend :initform nil :accessor parent-application-backend
            :documentation "Mandatory host enforcement backend.")
   (lock :initform (bordeaux-threads:make-lock "parent application")
         :reader parent-application-lock :documentation "Protect trusted restart requests.")
   (restart :initform nil :accessor parent-application-restart
            :documentation "A trusted owner requested policy replacement.")
   (native-startup :initform nil :accessor parent-application-native-startup
                   :documentation "Current trusted private-process startup callback.")
   (native-application :initform nil :accessor parent-application-native-application
                       :documentation "Exact backend application during startup.")
   (launch :initform nil :accessor parent-application-launch
           :documentation "Current child launch, or NIL before startup.")
   (access-session :initform nil :accessor parent-application-access-session
                   :documentation "Fresh connection-specific access adapter."))
  (:documentation "Trusted production composition, independent of the agent system."))

(serapeum/types:-> parent-application-revoke (parent-application string) null)
(defun parent-application-revoke (application operation-id)
  "Revoke a trusted owner's grant and request replacement of the running child.
This parent-only operation is not available through agent messages."
  (unless (parent-application-policy application)
    (parent-configuration--reject ':application-not-running))
  (access-service-revoke (parent-application-policy application) operation-id)
  (bordeaux-threads:with-lock-held ((parent-application-lock application))
    (setf (parent-application-restart application) t))
  nil)

(serapeum/types:-> parent-application-defer (parent-application function) null)
(defun parent-application-defer (application function)
  "Apply one trusted configuration change after the child has fully stopped."
  (bordeaux-threads:with-lock-held ((parent-application-lock application))
    (when (parent-application-after-stop application)
      (error 'parent-application-operation-error :code ':replacement-pending))
    (setf (parent-application-after-stop application) function
          (parent-application-restart application) t))
  nil)

(serapeum/types:-> parent-application-switch-session
    (parent-application string &key (:configuration (or null parent-configuration))) null)
(defun parent-application-switch-session (application identifier &key configuration)
  "Select a prepared durable session after native cleanup without nesting controllers."
  (unless (parent-application--session-id-p identifier)
    (parent-configuration--reject ':invalid-session-id))
  (when configuration
    (parent-application--prepare-session-code (parent-application-configuration application) configuration))
  (parent-application-defer
   application
   (lambda ()
     (when configuration (setf (slot-value application 'configuration) configuration))
     (unless (equal identifier (parent-application-session-id application))
       (let* ((selected (parent-application-configuration application))
              (values   (copy-tree (parent-configuration-values selected))))
         (setf (getf values :network-p) nil
               (slot-value selected 'values) values)))
     (setf (slot-value application 'session-id) identifier
           (slot-value application 'directory)
           (uiop:pathname-directory-pathname
            (parent-session-path (parent-application-configuration application) identifier "session.sexp"))
           (parent-application-policy application) nil
           (parent-application-backend application) nil
           (parent-application-access-session application) nil)))
  nil)

(serapeum/types:-> parent-application-set-permission-mode (parent-application (or string keyword)) null)
(defun parent-application-set-permission-mode (application mode)
  "Replace optional grant policy after cleanup. Native isolation is always required."
  (let ((normalized (rest (assoc mode '((:full . "full") (:full-access . "full")
                                        (:workspace . "sandbox") (:sandbox . "sandbox")
                                        (:ask . "ask") (:auto . "auto"))))))
    (when (stringp mode) (setf normalized mode))
    (unless (member normalized '("full" "sandbox" "ask" "auto") :test #'equal)
      (parent-configuration--reject ':invalid-permission-mode))
    (parent-application-defer
     application
     (lambda ()
       (let* ((configuration (parent-application-configuration application))
              (values        (slot-value configuration 'values)))
         (setf (getf values :access-p) (equal normalized "full")
               (getf (getf values :settings) :permission-mode) normalized
               (slot-value configuration 'values) values
               (parent-application-policy application) nil
               (parent-application-backend application) nil
               (parent-application-access-session application) nil)))))
  nil)

(serapeum/types:-> parent-application--restart-p (parent-application) boolean)
(defun parent-application--restart-p (application)
  "Read pending trusted revocation or committed access approval."
  (or (bordeaux-threads:with-lock-held ((parent-application-lock application))
        (not (null (parent-application-restart application))))
      (not (null (and (parent-application-access-session application)
                      (access-session-restart-revision (parent-application-access-session application)))))))

(serapeum/types:-> parent-application--identifier () string)
(defun parent-application--identifier ()
  "Return a fresh opaque parent-owned identity."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(serapeum/types:-> parent-application--session-id-p (t) boolean)
(defun parent-application--session-id-p (value)
  "Accept a safe owner-selected directory component."
  (and (stringp value) (<= 1 (length value) 128)
       (every (lambda (character)
                (find character "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"))
              value) t))

(serapeum/types:-> parent-application--path (parent-application string) pathname)
(defun parent-application--path (application name)
  "Return a fixed private store pathname under the resolved session directory."
  (merge-pathnames name (ls-compat.posix:resolve-pathname
                         (parent-application-directory application))))

(serapeum/types:-> parent-application-command (parent-configuration) list)
(defun parent-application-command (configuration)
  "Enter the child-only protocol bootstrap in a source-matched standard active core."
  (list (parent-configuration-get configuration ':runtime)
        "--core" (parent-configuration-get configuration ':agent-core)
        "--noinform" "--end-runtime-options"
        (parent-configuration-get configuration ':source-root) "--parent-agent"))

(serapeum/types:-> parent-application-input-history (parent-application) list)
(defun parent-application-input-history (application)
  "Return only durable parent-origin user input, including earlier process lifetimes."
  (let ((store (session-store-open (parent-application--path application "session.sexp")
                                   (parent-application-session-id application)))
        (after 0) (high nil) (inputs nil))
    (loop
      (let ((page (session-store-read store (parent-proto:make-session-read
                                             :after-sequence after :limit 256)
                                      :high-water-sequence high)))
        (setf high (parent-proto:session-page.high-water-sequence page))
        (dolist (record (parent-proto:session-page.records page))
          (setf after (parent-proto:session-record.sequence record))
          (when (and (eq (parent-proto:session-record.origin record) ':parent)
                     (eq (parent-proto:session-record.kind record) ':user-input))
            (let ((text (gethash "text" (protocol-value->data
                                         (parent-proto:session-record.payload record)))))
              (when (stringp text) (push text inputs)))))
        (unless (parent-proto:session-page.has-more page)
          (return (nreverse inputs)))))))

(serapeum/types:-> parent-application-memory-context (parent-application) memory-store-context)
(defun parent-application-memory-context (application)
  "Bind the shared record and knowledge stores to trusted workspace paths."
  (let* ((configuration (parent-application-configuration application))
         (workspace     (pathname (parent-configuration-get configuration ':workspace)))
         (root          (merge-pathnames "records/" (uiop:ensure-directory-pathname
                                                     (parent-configuration-get configuration ':private-root)))))
    (ensure-directories-exist (merge-pathnames "memory.sexp" root))
    (make-memory-store-context :memory-path    (merge-pathnames "memory.sexp" root)
                               :knowledge-path (merge-pathnames "knowledge.sexp" root)
                               :workspace workspace :source-roots (list workspace))))

(serapeum/types:-> parent-application-knowledge
    (parent-application &key (:terminal (or null parent-terminal))) knowledge-service)
(defun parent-application-knowledge (application &key terminal)
  "Compose knowledge with parent-only scope, evidence and admission policy."
  (make-knowledge-service
   :context        (parent-application-memory-context application)
   :session-id     (parent-application-session-id application)
   :allow-global-p (parent-configuration-get (parent-application-configuration application) ':allow-global-p)
   :input-history  (lambda () (parent-application-input-history application))
   :url-evidence   (parent-application-url-evidence application)
   :approve
   (or (parent-application-knowledge-approval application)
       (and terminal
            (lambda (kind scope identifier version proposal)
              (handler-case
                  (parent-terminal-approve-knowledge terminal :kind kind :scope scope
                                                    :identifier identifier :version version
                                                    :proposal proposal)
                (parent-terminal-error () nil))))
       (constantly nil))))

(serapeum/types:-> parent-application-records (parent-application) record-dispatcher)
(defun parent-application-records (application)
  "Share durable record domains across sessions; keep operation claims session-bound."
  (let* ((configuration (parent-application-configuration application))
         (session-id    (parent-application-session-id application))
         (workspace     (parent-configuration-get configuration ':workspace))
         (root          (merge-pathnames "records/" (uiop:ensure-directory-pathname
                                                     (parent-configuration-get configuration ':private-root))))
         (memory        (parent-application-memory-context application)))
    (ensure-directories-exist (merge-pathnames "memory.sexp" root))
    (make-record-dispatcher
     :pathname (parent-application--path application "record-claims.sexp") :session-id session-id
     :agenda-service
     (make-agenda-service
      :context    (make-instance 'agenda-context :pathname (merge-pathnames "agenda.sexp" root)
                                 :directory                workspace
                                 :memory-validator
                                 (lambda (identifier)
                                   (let ((item (memory-find memory identifier)))
                                     (and item (eq (memory-scope item) ':workspace)
                                          (equal (memory-workspace item) workspace) t))))
      :session-id session-id)
     :memory-service (make-memory-service :context memory :session-id session-id
                                          :allow-global-p (parent-configuration-get configuration ':allow-global-p)
                                          :input-history
                                          (lambda () (parent-application-input-history application)))
     :papercut-service
     (make-papercut-service
      :context    (make-instance 'papercut-context :pathname (merge-pathnames "papercuts.sexp" root)
                                 :workspace                  workspace)
      :session-id session-id))))

(serapeum/types:-> make-parent-application
                   (parent-configuration &key (:session-id (or null string))
                    (:input-source function) (:renderer function)
                    (:knowledge-approval (or null function)) (:url-evidence function)
                    (:settings list) (:operation-handlers list)
                    (:event-callback (or null function)) (:notification-callback (or null function))) parent-application)
(defun make-parent-application (configuration &key session-id input-source renderer
                                                 knowledge-approval operation-handlers
                                                 event-callback notification-callback
                                                 (url-evidence (constantly nil))
                                                 (settings (parent-configuration-get configuration ':settings)))
  "Select a private session and load only explicit trusted parent extensions."
  (let ((identity (or session-id (parent-application--identifier))))
    (unless (and (parent-application--session-id-p identity)
                 (functionp input-source) (functionp renderer)
                 (or (null knowledge-approval) (functionp knowledge-approval)) (functionp url-evidence))
      (parent-configuration--reject ':invalid-application))
    (let* ((root      (uiop:ensure-directory-pathname
                       (parent-configuration-get configuration ':private-root)))
           (directory (merge-pathnames (format nil "sessions/~A/" identity) root))
           (accounts  (make-provider-account-configuration
                       (parent-configuration-get configuration ':account-configuration))))
      (ensure-directories-exist (merge-pathnames "session.sexp" directory))
      (ensure-directories-exist
       (merge-pathnames "probe" (uiop:ensure-directory-pathname
                                 (parent-configuration-get configuration ':temporary-root))))
      (ensure-directories-exist
       (merge-pathnames "probe" (uiop:ensure-directory-pathname
                                 (provider-account-config ':state-root accounts))))
      (multiple-value-bind (profiles bindings registry protected)
          (parent-provider-build-bindings
           configuration settings :registry (parent-configuration-provider-registry configuration))
        (make-instance 'parent-application :configuration configuration :session-id identity
                       :directory directory :profiles profiles :accounts bindings
                       :provider-registry registry :provider-paths protected
                       :input-source input-source :renderer renderer
                       :operation-handlers operation-handlers
                       :event-callback event-callback :notification-callback notification-callback
                       :knowledge-approval knowledge-approval :url-evidence url-evidence)))))

(serapeum/types:-> parent-application--terminal (parent-application) parent-terminal)
(defun parent-application--terminal (application)
  "Make a fresh terminal cancellation latch for each connection."
  (make-parent-terminal :input-source    (parent-application-input-source application)
                        :renderer        (parent-application-renderer application)
                        :exclusive-input (parent-application-exclusive-input application)
                        :read-function   (parent-application-modal-reader application)))

(serapeum/types:-> parent-application-private-roots (parent-configuration) list)
(defun parent-application-private-roots (configuration)
  "Resolve all parent storage, including credential files and their future-write directories.
Reject any overlap with an admitted base scope before creating the native backend."
  (let* ((accounts (make-provider-account-configuration
                    (parent-configuration-get configuration ':account-configuration)))
         (roots    (list (pathname (parent-configuration-get configuration ':private-root))
                         (uiop:ensure-directory-pathname (provider-account-config ':state-root accounts))))
         (source   (parent-configuration-source-path configuration))
         (codex    (merge-pathnames ".codex/" (user-homedir-pathname))))
    (dolist (root roots)
      (ensure-directories-exist
       (merge-pathnames "probe" (uiop:ensure-directory-pathname root))))
    (when source (push source roots))
    (dolist (specification (parent-configuration-get configuration ':provider-extensions))
      (push (parent-provider-validate-extension-path
             (getf specification :path)
             (remove nil (list (parent-configuration-get configuration ':workspace)
                               (parent-configuration-get configuration ':temporary-root)
                               (getf (parent-configuration-get configuration ':settings) :site-config-root))))
            roots))
    (dolist (key '(:api-keys-path :grok-bootstrap-auth-path))
      (let ((path (provider-account-config key accounts)))
        (when path
          (let ((file (pathname path)))
            (ensure-directories-exist file)
            (push (uiop:pathname-directory-pathname file) roots)
            (when (probe-file file) (push file roots))))))
    (when (and (find ':codex (parent-configuration-get configuration ':profiles)
                     :key (lambda (profile) (getf profile :family)))
               (uiop:directory-exists-p codex))
      (push codex roots))
    (let ((resolved
            (remove-duplicates
             (mapcar (lambda (path) (namestring (ls-compat.posix:resolve-pathname path))) roots)
             :test #'equal))
          (scopes (mapcar (lambda (path) (namestring (ls-compat.posix:resolve-pathname path)))
                          (append (list (parent-configuration-get configuration ':workspace)
                                        (parent-configuration-get configuration ':temporary-root))
                                  (parent-configuration-get configuration ':runtime-roots)))))
      (when (some (lambda (root)
                    (some (lambda (scope) (access-policy--overlap-p root scope)) scopes)) resolved)
        (parent-configuration--reject ':private-root-overlaps-agent-scope))
      resolved)))

(serapeum/types:-> parent-application--ensure-policy (parent-application parent-terminal) null)
(defun parent-application--ensure-policy (application terminal)
  "Build mandatory native enforcement. Unsupported hosts refuse before approval."
  (unless (parent-application-policy application)
    (let* ((configuration (parent-application-configuration application))
           (private-roots (parent-application-private-roots configuration))
           (backend
             (make-native-access-backend
              :command        (parent-application-command configuration)
              :environment    (parent-configuration-child-environment configuration)
              :runtime-roots  (parent-configuration-get configuration ':runtime-roots)
              :temporary-root (parent-configuration-get configuration ':temporary-root)
              :network-p      (parent-configuration-get configuration ':network-p)
              :startup
              (lambda (native)
                (setf (parent-application-native-application application) native)
                (funcall (parent-application-native-startup application)
                         (native-access-application-process native)))))
           (context
             (make-access-context
              :pathname      (parent-application--path application "policy.sexp")
              :session-id    (parent-application-session-id application)
              :workspace     (parent-configuration-get configuration ':workspace)
              :private-roots private-roots
              :backend backend :terminal terminal)))
      (setf (parent-application-backend application) backend
            (parent-application-policy application) (make-access-service context))))
  nil)

(serapeum/types:-> parent-application-open (parent-application) parent-launch)
(defun parent-application-open (application)
  "Apply mandatory external base isolation, then open one fresh child.
ACCESS-P selects access requests, not whether private roots are excluded."
  (let* ((configuration (parent-application-configuration application))
         (terminal      (parent-application--terminal application))
         (instance-id   (parent-application--identifier))
         (access-p      (parent-configuration-get configuration ':access-p))
         (extra         nil))
    (parent-application--ensure-policy application terminal)
    (let* ((policy  (parent-application-policy application))
           (context (access-service-context policy)))
      (setf (slot-value context 'terminal) terminal)
      (access-service-project-notes
       policy (session-store-open (parent-application--path application "session.sexp")
                                  (parent-application-session-id application)))
      (setf (parent-application-access-session application)
            (when access-p (make-access-session-service policy))
            extra
            (list
             :access-service (parent-application-access-session application)
             :application-service
             (make-parent-application-operation-service
              (parent-application-operation-handlers application))
             :process-opener
             (lambda (command &key environment directory instance-id startup startup-timeout)
               (declare (ignore command environment directory startup-timeout))
               (setf (parent-application-native-startup application) startup)
               (access-service-start policy instance-id)
               (native-access-application-process
                (parent-application-native-application application)))
             :process-closer
             (lambda (process)
               (access-service-stop policy)
               (private-process-close process))
             :startup-policy
             (lambda (process)
               (declare (ignore process))
               (let ((native (parent-application-native-application application)))
                 (values (access-application-revision native)
                         (mapcar #'access-policy--wire-grant (access-application-grants native))))))))
    (setf (parent-application-launch application)
          (apply #'parent-launch-open
                 :command                           (parent-application-command configuration)
                 :environment                       (parent-configuration-child-environment configuration)
                 :directory                         (parent-configuration-get configuration ':workspace)
                 :session-id (parent-application-session-id application) :instance-id instance-id
                 :session-path                      (parent-application--path application "session.sexp")
                 :replay-path                       (parent-application--path application "replay.sexp")
                 :configuration                     (parent-configuration-agent-map
                                                     configuration :command-policy ':inherited-scope
                                                     :profile                      (model-profile-lookup
                                                                                    (parent-application-profiles application)
                                                                                    (parent-configuration-get configuration ':profile-id)))
                 :profiles                          (parent-application-profiles application)
                 :accounts (parent-application-accounts application) :terminal terminal
                 :executor-factory                  #'parent-provider-model-executor
                 :application-event-callback        (parent-application-event-callback application)
                 :application-notification-callback (parent-application-notification-callback application)
                  :knowledge-service                (parent-application-knowledge application :terminal terminal)
                 :record-service                    (parent-application-records application) extra))))

(serapeum/types:-> parent-application-run (parent-application) keyword)
(defun parent-application-run (application)
  "Recover only through a trusted owner callback after complete native cleanup."
  (loop
    (handler-case
        (return (parent-application--run application))
      (error (condition)
        (let ((handler (parent-application-recovery-handler application))
              (launch  (parent-application-launch application)))
          (unless handler (error condition))
          (when (and launch (null (parent-launch-evidence launch)))
            (error condition))
          (unless (funcall handler application condition)
            (return ':failed)))))))

(serapeum/types:-> parent-application--run (parent-application) keyword)
(defun parent-application--run (application)
  "Run trusted input and replace policy only after complete native cleanup.
  A custom input runner may poll during work; complete modal prompts own input."
  (loop
    (let* ((launch  (parent-application-open application))
           (done nil) (result nil) (failure nil)
           (lock    (bordeaux-threads:make-lock "parent input result"))
           (thread
             (bordeaux-threads:make-thread
              (lambda ()
                (unwind-protect
                     (handler-case
                        (setf result
                              (if (parent-application-input-runner application)
                                  (funcall (parent-application-input-runner application) application launch)
                                  (parent-launch-run-input launch #'parent-application--identifier)))
                       (error (condition) (setf failure condition)))
                  (bordeaux-threads:with-lock-held (lock) (setf done t))))
              :name "trusted parent input"))
           (restart nil))
      (unwind-protect
           (progn
             (loop
               (when (parent-application--restart-p application)
                 (setf restart t)
                 (parent-launch-close launch)
                 (return))
               (when (bordeaux-threads:with-lock-held (lock) done) (return))
               (sleep 0.01))
             (sb-thread:join-thread thread)
             (when restart
               (bordeaux-threads:with-lock-held ((parent-application-lock application))
                (setf (parent-application-restart application) nil))
              (let ((function (parent-application-after-stop application)))
                (when function
                  (setf (parent-application-after-stop application) nil)
                  (funcall function))))
             (unless restart
               (when failure (error failure))
               (return result)))
        (parent-launch-close launch)))))

(serapeum/types:-> parent-application-main (pathname &key (:session-id (or null string))
                                                   (:input stream) (:output stream)) keyword)
(defun parent-application-main (configuration-path &key session-id
                                                       (input *standard-input*) (output *standard-output*))
  "Run the explicit parent entry from one trusted data configuration file."
  (let ((application
          (make-parent-application
           (read-parent-configuration configuration-path) :session-id session-id
           :input-source                                              (parent-terminal-stream-input-source input)
           :renderer                                                  (parent-terminal-stream-renderer output))))
    (format output "PARENT | Session ~A~%" (parent-application-session-id application))
    (finish-output output)
    (parent-application-run application)))
