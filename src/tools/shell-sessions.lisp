(in-package #:autolith)

;;;; -- Reusable Authorized Shells --

(defclass shell-session-pool ()
  ()
  (:documentation "Registry-owned identity for reusable shell shutdown and retention."))

(defclass shell-session ()
  ((name :initarg :name :reader shell-session-name
         :documentation "Owner-local shell name.")
   (directory :initarg :directory :accessor shell-session-directory
              :documentation "Last observed physical shell working directory.")
   (scope :initarg :scope :reader shell-session-scope
          :documentation "Creation permission mode, authorization and filesystem scopes.")
   (lock :initform (make-recursive-lock "shell session command") :reader shell-session-lock
         :documentation "Serialize commands and working-directory transitions.")
   (temporary :initform nil :accessor shell-session-temporary
              :documentation "Private control and bounded-output pipe directory.")
   (staging :initform nil :accessor shell-session-staging
            :documentation "Trusted publication directory excluded from shell write scopes.")
   (programs :initform nil :accessor shell-session-programs
             :documentation "Resolved executable paths captured at session creation.")
   (cleanup-condition :initform nil :accessor shell-session-cleanup-condition
                      :documentation "Native cleanup ownership retained until reaping and reader completion.")
   (thread :initform nil :accessor shell-session-thread
           :documentation "Lifetime supervisor holding sandbox scope ownership.")
   (stop-p :initform nil :accessor shell-session-stop-p
           :documentation "Request complete supervised process-tree termination.")
   (dead-p :initform nil :accessor shell-session-dead-p
           :documentation "A terminated shell must be explicitly reset.")
   (diagnostic :initform nil :accessor shell-session-diagnostic
               :documentation "Bounded lifetime supervisor failure explanation."))
  (:documentation "One supervised shell process and its explicit lifetime state."))

(defclass shell-session-tool (workspace-tool)
  ((operation :initarg :operation :reader shell-session-tool-operation
              :documentation "Session management operation: sessions, reset or close.")
   (session-pool :initarg :session-pool :reader shell-session-tool-pool
                 :documentation "Registry-owned reusable shell pool."))
  (:documentation "Inspect or explicitly replace owner-local reusable shells."))

(defmethod tool-child-safe-p ((tool shell-session-tool))
  "Keep reusable shell management available to isolated child agents."
  t)

(defvar *shell-session-lock* (make-lock "shell sessions")
  "Protect the owner-local session registry.")

(defvar *shell-sessions* (make-hash-table :test #'equal)
  "Live process state indexed by conversation, agent identity and local name.")

(defmethod tool-runtime-identity ((tool shell-run-tool))
  "Retain the same shell pool when replacing its owning registry."
  (shell-run-session-pool tool))

(defmethod tool-runtime-identity ((tool shell-session-tool))
  "Share one shutdown owner across shell lifecycle operations."
  (shell-session-tool-pool tool))

(-> shell-session--close-pool (shell-session-pool) null)
(defun shell-session--close-pool (pool)
  "Remove this pool's sessions and stop every supervised process tree."
  (let ((sessions nil))
    (with-lock-held (*shell-session-lock*)
      (maphash (lambda (key session)
                 (when (eq (first key) pool)
                   (push session sessions)))
               *shell-sessions*)
      (dolist (session sessions)
        (maphash (lambda (key value)
                   (when (eq value session) (remhash key *shell-sessions*)))
                 *shell-sessions*)))
    (dolist (session sessions) (shell-session--stop session)))
  nil)

(defmethod tool-runtime-close ((tool shell-run-tool))
  "Terminate persistent shell state when its owning registry closes."
  (when (shell-run-session-pool tool)
    (shell-session--close-pool (shell-run-session-pool tool))))

(defmethod tool-runtime-close ((tool shell-session-tool))
  "Terminate persistent shell state once for this shared runtime."
  (shell-session--close-pool (shell-session-tool-pool tool)))

(-> shell-session--key (tool-context string) list)
(defun shell-session--key (context name)
  "Return a key that never shares shells between agents or conversations."
  (list (tool-registry-runtime-binding (tool-context-registry context) 'shell-session-pool)
        (conversation-identifier (tool-context-conversation context))
        (tool-context-agent context) name))

(-> shell-session--scope (tool-context keyword) list)
(defun shell-session--scope (context authorization)
  "Snapshot every permission and filesystem scope governing a shell lifetime."
  (let ((configuration (tool-context-configuration context)))
    (list authorization (config :permission-mode configuration)
          (config :working-directory configuration)
          (copy-list (task-worktree-command-writable-roots context)))))

(-> shell-session--find (tool-context string) (option shell-session))
(defun shell-session--find (context name)
  "Find an owner-local session without allocating a process."
  (with-lock-held (*shell-session-lock*)
    (gethash (shell-session--key context name) *shell-sessions*)))

(-> shell-session--fail (string) nil)
(defun shell-session--fail (message)
  "Signal a typed reusable shell lifecycle or scope failure."
  (error 'tool-error :tool-name "shell.run" :message message))

(-> shell-session--require-capability () null)
(defun shell-session--require-capability ()
  "Reject unavailable transports even for manually constructed internal calls."
  (unless (platform-shell-sessions-p *platform*)
    (platform-shell-session-start-command *platform* (uiop:temporary-directory)))
  nil)

(-> shell-session--path (shell-session string) pathname)
(defun shell-session--path (session name)
  "Return a private protocol path belonging to SESSION."
  (merge-pathnames name (shell-session-temporary session)))

(-> shell-session--write (shell-session pathname string) null)
(defun shell-session--write (session path text)
  "Publish atomically from trusted staging, never opening or chmodding shell-planted links."
  (let* ((directory (platform-make-temporary-directory
                     *platform* (shell-session-staging session) "publish-"))
         (temporary (merge-pathnames "value" directory)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction ':output :if-exists ':error
                                             :if-does-not-exist ':create :external-format ':utf-8)
             (write-string text stream))
           (platform-make-private *platform* temporary)
           (uiop:rename-file-overwriting-target temporary path))
      (platform-shell-session-delete-directory *platform* directory)))
  nil)

(-> shell-session--await-native-cleanup
    (shell-session cl-exec-sandbox:sandbox-process-cleanup-error) null)
(defun shell-session--await-native-cleanup (session condition)
  "Retain native cleanup ownership inside the sandbox scope until reaping is proved."
  (setf (shell-session-cleanup-condition session) condition)
  (let ((owner    (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-thread condition))
        (retry    (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-retry condition))
        (readers  (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-readers condition))
        (reaped-p nil))
    (loop
      (unless (or owner reaped-p)
        (when retry
          (handler-case (setf owner (funcall retry))
            (error () nil))))
      (when (and owner (not (thread-alive-p owner)))
        (setf reaped-p (handler-case (not (null (join-thread owner)))
                         (error () nil))
              owner nil))
      (when (and reaped-p (notany #'thread-alive-p readers))
        (return))
      (sleep 0.01)))
  (setf (shell-session-cleanup-condition session) nil)
  nil)

(-> shell-session--stop (shell-session) null)
(defun shell-session--stop (session)
  "Request cancellation, then join proven cleanup before marking dead or deleting paths."
  (setf (shell-session-stop-p session) t)
  (with-recursive-lock-held ((shell-session-lock session))
    (let ((thread (shell-session-thread session)))
      (when (and thread (not (eq thread sb-thread:*current-thread*)))
        (sb-thread:join-thread thread)))
    (setf (shell-session-dead-p session) t)
    (dolist (directory (list (shell-session-temporary session) (shell-session-staging session)))
      (when directory
        (platform-shell-session-delete-directory *platform* directory))))
  nil)

(-> shell-session--start (shell-session tool-context keyword) null)
(defun shell-session--start (session context authorization)
  "Start one supervised shell and retain sandbox scope until its process tree dies."
  (let* ((configuration (tool-context-configuration context))
         (workspace     (config :working-directory configuration))
         (programs      (platform-shell-session-programs *platform*))
         (temporary     (platform-make-temporary-directory
                         *platform* (uiop:temporary-directory) "autolith-shell-"))
         (staging       (platform-make-temporary-directory
                         *platform* (uiop:temporary-directory) "autolith-shell-publish-"))
         (roots         (or (task-worktree-command-writable-roots context) (list workspace)))
         (exit-code     nil))
    (setf (shell-session-temporary session) temporary
          (shell-session-staging session) staging
          (shell-session-programs session) programs)
    (when (and (eq authorization ':sandboxed)
               (some (lambda (root)
                       (or (pathname-within-p staging root)
                           (pathname-within-p (uiop:pathname-parent-directory-pathname temporary) root)))
                     roots))
      (shell-session--stop session)
      (shell-session--fail "Named-shell private directories overlap a writable root; use a narrower workspace or a temporary directory outside its write scopes."))
    (setf (shell-session-thread session)
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (handler-case
                      (flet ((run (policy environment)
                               (destructuring-bind (program &rest arguments)
                                  (platform-shell-session-command-line *platform* temporary :programs programs)
                                (let ((result
                                        (handler-case
                                            (run-sandboxed
                                             program arguments :policy policy :environment environment
                                             :working-directory (shell-session-directory session)
                                             :output-limit 1000 :error-output-limit 1000
                                             :cancel-function   (lambda () (shell-session-stop-p session)))
                                          (cl-exec-sandbox:sandbox-process-cleanup-error (condition)
                                            (shell-session--await-native-cleanup session condition)
                                            (or (cl-exec-sandbox:sandbox-execution-error-result condition)
                                                (error condition))))))
                                  (setf exit-code (sandbox-result-exit-code result)
                                        (shell-session-diagnostic session)
                                        (bounded-string
                                         (format nil "Shell exited (~A): ~A"
                                                 (sandbox-result-exit-code result)
                                                 (cl-exec-sandbox:sandbox-result-error-output result)) :limit 1000))))))
                        (ecase authorization
                          (:sandboxed
                           (platform-call-with-command-sandbox
                            *platform* workspace #'run :writable-roots (append roots (list temporary))
                            :read-roots                                (cons staging
                                                                             (mapcar (lambda (entry)
                                                                                       (uiop:pathname-directory-pathname (pathname (second entry))))
                                                                                     programs))))
                          (:full-access
                           (run (external-sandbox-policy) nil))))
                    (error (condition)
                      (setf (shell-session-diagnostic session)
                            (bounded-string (princ-to-string condition) :limit 1000))))
              (unless (probe-file (shell-session--path session "status"))
                (shell-session--write session (shell-session--path session "status")
                                      (princ-to-string (or exit-code 1))))
              (setf (shell-session-dead-p session) t)))
           :name (format nil "shell session ~A" (shell-session-name session))))
    (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
          until (probe-file (shell-session--path session "ready"))
          do (when (or (shell-session-dead-p session)
                       (> (get-internal-real-time) deadline))
               (shell-session--stop session)
               (shell-session--fail (or (shell-session-diagnostic session)
                                       "Reusable shell startup timed out.")))
             (sleep 0.02)))
  nil)

(-> shell-session--execute
    (shell-session tool-context string
     &key (:directory pathname) (:authorization keyword)
          (:timeout (integer 1)) (:merge-output-p boolean) (:output-limit (integer 0))
          (:change-directory-p boolean)) tool-result)
(defun shell-session--execute
    (session context command
     &key directory authorization timeout merge-output-p output-limit change-directory-p)
  "Run one serialized command without replay, using normal retained shell logs."
  (with-recursive-lock-held ((shell-session-lock session))
    (unless (equal (shell-session-scope session) (shell-session--scope context authorization))
      (shell-session--fail "Shell permissions or workspace scopes changed; use shell.reset."))
    (when (or (shell-session-dead-p session) (shell-session-stop-p session))
      (shell-session--fail "Shell is dead; use shell.reset before running another command."))
    (unless (or change-directory-p (equal directory (shell-session-directory session)))
      (shell-session--fail "Shell directory changed while this command was queued; retry for authorization."))
    (unless (shell-session-thread session)
      (shell-session--start session context authorization))
    (let ((complete-p nil)
          (status     nil))
      (unwind-protect
           (progn
             (dolist (name '("status" "cwd"))
               (let ((path (shell-session--path session name)))
                 (when (probe-file path) (delete-file path))))
            (shell-session--write session (shell-session--path session "command") command)
            (shell-session--write
             session (shell-session--path session "frame")
             (platform-shell-session-frame *platform* (shell-session-temporary session)
                                           (and change-directory-p directory)
                                           :programs       (shell-session-programs session)
                                           :merge-output-p merge-output-p))
            (shell-session--write session (shell-session--path session "go") "")
            (let ((result
                    (workspace-tool-run-shell-command
                     (platform-shell-session-reader-command
                      *platform* (shell-session-temporary session) :merge-output-p merge-output-p
                      :programs                                                    (shell-session-programs session))
                     (shell-session-temporary session)
                     :policy (external-sandbox-policy) :timeout timeout :output-limit output-limit
                     :context context :merge-output-p merge-output-p
                     :cancel-function (lambda ()
                                        (and (shell-session-dead-p session)
                                             (not (probe-file (shell-session--path session "status")))))
                     :result-function (lambda (result)
                                        (setf status (cl-exec-sandbox:sandbox-result-status result))))))
              (when (and (not (shell-session-dead-p session))
                         (probe-file (shell-session--path session "status")))
               (let ((cwd (platform-shell-session-read-record *platform* (shell-session--path session "cwd") 16384)))
                  (setf (shell-session-directory session)
                        (uiop:ensure-directory-pathname
                         (subseq cwd 0 (max 0 (1- (length cwd))))))))
             (setf complete-p (and (eq status ':exited)
                                   (not (shell-session-dead-p session))
                                   (probe-file (shell-session--path session "status"))))
             result))
        (unless complete-p (shell-session--stop session))))))

(-> shell-session-run (tool-context hash-table) tool-result)
(defun shell-session-run (context arguments)
  "Authorize the current cwd, pin lifetime scopes, and invoke one named command."
  (shell-session--require-capability)
  (let* ((name               (tool-argument arguments "session" :required t))
         (command            (tool-argument arguments "command" :required t))
         (session            (shell-session--find context name))
         (explicit-directory (tool-argument arguments "directory"))
         (directory          (if (and session (null explicit-directory))
                                 (shell-session-directory session)
                                 (workspace-tool-resolve-path context explicit-directory)))
         (authorization      (tool-context-authorize-command context command directory)))
    (unless (and (non-empty-string-p name) (non-empty-string-p command))
      (shell-session--fail "Named shells require a nonempty session name and command."))
    (when (eq authorization ':deny)
      (return-from shell-session-run (tool-failure "The user denied this command." :denied-p t)))
    (when (eq authorization ':sandboxed)
      (workspace-tool-confined-path context (uiop:native-namestring directory) :tool-name "shell.run"))
    (with-lock-held (*shell-session-lock*)
      (setf session
            (or (gethash (shell-session--key context name) *shell-sessions*)
                (setf (gethash (shell-session--key context name) *shell-sessions*)
                      (make-instance 'shell-session :name name :directory directory
                                     :scope (shell-session--scope context authorization))))))
    (tool-execution-invoke
     (tool-context-execution-runtime context) (tool-context-agent context)
     :tool-name "shell.run" :description (tool-argument arguments "description")
     :summary           (format nil "~A in shell ~A" command name)
     :operation-function
     (lambda ()
      (shell-session--execute session context command :directory directory :authorization authorization
                               :timeout            (workspace-tool-shell-timeout arguments)
                               :merge-output-p     (not (tool-boolean-argument arguments "separate-output" :tool-name "shell.run"))
                               :output-limit       *shell-maximum-output-characters*
                               :change-directory-p (not (null explicit-directory))))
     :async-p           (tool-boolean-argument arguments "async" :tool-name "shell.run")
     :completion-policy (tool-completion-policy-argument arguments :tool-name "shell.run"))))

(defmethod tool-execute ((tool shell-session-tool) (context tool-context) (arguments hash-table))
  "Inspect shells or explicitly close/reset one owned shell."
  (shell-session--require-capability)
  (let ((name (tool-argument arguments "session")))
    (case (shell-session-tool-operation tool)
      (:sessions
       (tool-success
        (with-output-to-string (stream)
          (with-lock-held (*shell-session-lock*)
            (maphash (lambda (key session)
                       (when (equal (subseq key 0 3) (subseq (shell-session--key context "") 0 3))
                         (format stream "~A ~A ~A~%" (shell-session-name session)
                                 (if (shell-session-dead-p session) "dead" "live")
                                 (shell-session-directory session))))
                     *shell-sessions*)))))
      ((:close :reset)
       (unless (non-empty-string-p name)
         (shell-session--fail "A session name is required."))
       (let ((session (shell-session--find context name)))
         (when session
           (shell-session--stop session)
           (with-lock-held (*shell-session-lock*)
             (remhash (shell-session--key context name) *shell-sessions*))))
       (if (eq (shell-session-tool-operation tool) ':reset)
           (shell-session-run context (json-object "session" name "command" ":"
                                                    "directory" (tool-argument arguments "directory")))
           (tool-success (format nil "Closed shell ~A." name)))))))
