(in-package #:autolith)

;;;; -- Isolated Task Workspaces --

(defvar *task-worktree-lock* (make-lock "Autolith task Git operations")
  "Serialize owned Git lifecycle and integration operations within this image.")

(defparameter *task-worktree-command-timeout* 120
  "Maximum seconds for one authorized Git command.")

(defparameter *task-worktree-artifact-limit* (* 16 1024 1024)
  "Maximum captured patch characters and serialized engineering artifact octets.")

(define-condition task-worktree-error (task-error) ()
  (:documentation "An isolated task could not satisfy its authority or lifecycle contract."))

(defclass task-worktree-tool (task-orchestrator-tool)
  ()
  (:documentation "Explicit parent inspection, integration and owned cleanup."))

(-> task-worktree--error (string &key (:code (option keyword))) nil)
(defun task-worktree--error (message &key code)
  "Signal a product-specific worktree failure with tool attribution."
  (error 'task-worktree-error :message message :code code :tool-name "task.worktree"))

(-> task-worktree-normalize-options (t) list)
(defun task-worktree-normalize-options (object)
  "Validate optional isolation OBJECT into a portable spawn policy."
  (unless object (return-from task-worktree-normalize-options nil))
  (unless (json-object-p object)
    (task-worktree--error "isolation must be an object."))
  (task--validate-json-fields object '("baseline" "dirtyPolicy" "artifactKind")
                              "task isolation")
  (let ((baseline (or (json-get object "baseline") "HEAD"))
        (dirty    (or (json-get object "dirtyPolicy") "reject"))
        (kind     (or (json-get object "artifactKind") "patch")))
    (unless (non-empty-string-p baseline)
      (task-worktree--error "isolation.baseline must be a non-empty revision."))
    (unless (member dirty '("reject" "ignore") :test #'equal)
      (task-worktree--error "isolation.dirtyPolicy must be reject or ignore."))
    (unless (member kind '("patch" "commit-range") :test #'equal)
      (task-worktree--error "isolation.artifactKind must be patch or commit-range."))
    (list :baseline baseline :dirty-policy (if (string= dirty "reject") ':reject ':ignore)
          :artifact-kind kind)))

(-> task-worktree-options-schema () hash-table)
(defun task-worktree-options-schema ()
  "Describe the opt-in task isolation object."
  (tool-object-schema
   (json-object
    "baseline" (tool-string-property "Committed Git baseline; defaults to HEAD.")
    "dirtyPolicy" (json-object "type" "string" "enum" (json-array "reject" "ignore")
                                "description" "Reject dirty sources by default; ignore explicitly excludes all uncommitted source edits.")
    "artifactKind" (json-object "type" "string" "enum" (json-array "patch" "commit-range")
                                 "description" "Terminal engineering artifact; defaults to patch.")) nil))

(-> task-worktree--environment (&key (:overrides list)) list)
(defun task-worktree--environment (&key overrides)
  "Combine sandbox OVERRIDES with an allowlist excluding Git repository selectors.
HOME and absolute XDG_CONFIG_HOME supply identity, ignore and attribute configuration;
cl-worktree disables execution callbacks. Sandbox temporary paths take precedence
without changing the host environment."
  (append overrides
          (loop for name in '("PATH" "HOME" "USERPROFILE" "SYSTEMROOT" "WINDIR"
                              "TMPDIR" "TMP" "TEMP" "LANG" "LC_ALL" "XDG_CONFIG_HOME")
                for value = (uiop:getenv name)
                when (and value
                          (or (not (string= name "XDG_CONFIG_HOME"))
                              (and (plusp (length value))
                                   (uiop:absolute-pathname-p
                                    (uiop:parse-native-namestring value))))
                          (not (some (lambda (binding)
                                       (uiop:string-prefix-p (format nil "~A=" name) binding))
                                     overrides)))
                  collect (format nil "~A=~A" name value))))

(-> task-worktree--git-read-roots () list)
(defun task-worktree--git-read-roots ()
  "Expose only Git's existing subtree of the forwarded XDG configuration root."
  (loop for binding in (task-worktree--environment)
        when (uiop:string-prefix-p "XDG_CONFIG_HOME=" binding)
          append (let ((directory (merge-pathnames
                                   "git/" (uiop:ensure-directory-pathname
                                           (uiop:parse-native-namestring
                                            (subseq binding (length "XDG_CONFIG_HOME=")))))))
                   (when (uiop:directory-exists-p directory)
                     (list directory)))))

(-> task-worktree--runner
    (configuration function &key (:writable-roots list) (:cancel-function (option function))) function)
(defun task-worktree--runner (configuration authorize &key writable-roots cancel-function)
  "Return a cancellation-aware argv runner using the ordinary command policy."
  (lambda (argv &key directory input)
    (task-worktree--require-settled (config :working-directory configuration))
    (task-worktree--require-settled directory)
    (when (and cancel-function (funcall cancel-function))
      (task-worktree--error "Worktree operation cancelled." :code ':cancelled))
    (let* ((directory (uiop:ensure-directory-pathname directory))
           (command   (format nil "task.worktree Git argv ~S" argv))
           (decision  (funcall authorize command directory)))
      (unless (member decision '(:sandboxed :full-access))
        (task-worktree--error "Worktree Git command was denied." :code ':authority))
      (labels ((run (policy environment)
                 (handler-bind
                     ((cl-exec-sandbox:sandbox-process-cleanup-error
                        (lambda (condition)
                          (task-worktree--retain-cleanup
                           condition (config :working-directory configuration)))))
                  (let ((result
                          (run-sandboxed
                           (first argv) (rest argv) :policy policy
                           :working-directory directory :input (and input (make-string-input-stream input))
                           :environment                     (task-worktree--environment :overrides environment)
                           :clear-environment-p t :timeout *task-worktree-command-timeout*
                           :cancel-function                 cancel-function
                           :output-limit                    (if (member "diff" argv :test #'equal)
                                                                *task-worktree-artifact-limit* 65536)
                           :error-output-limit              65536)))
                    (when (eq (cl-exec-sandbox:sandbox-result-status result) ':cancelled)
                      (task-worktree--error "Worktree Git command cancelled." :code ':cancelled))
                    (when (or (sandbox-result-timed-out-p result)
                              (sandbox-result-output-truncated-p result)
                              (cl-exec-sandbox:sandbox-result-error-output-truncated-p result))
                      (task-worktree--error "Git command timed out or exceeded the artifact capture limit."))
                    (values (sandbox-result-output result)
                            (cl-exec-sandbox:sandbox-result-error-output result)
                           (sandbox-result-exit-code result))))))
        (ecase decision
          (:full-access
           (run (external-sandbox-policy) nil))
          (:sandboxed
           (platform-call-with-command-sandbox
            *platform* (config :working-directory configuration) #'run
             :read-roots (task-worktree--git-read-roots)
            :writable-roots
            (remove-duplicates
             (append writable-roots
                     (task-worktree--git-admin-roots
                      (config :working-directory configuration))
                     (task-worktree--git-admin-roots directory)
                     (when (uiop:subpathp
                            directory
                            (merge-pathnames "task-worktrees/checkouts/"
                                             (config :data-root configuration)))
                       (list directory)))
             :test #'equal))))))))

(-> task-worktree--manager
    (configuration function &key (:writable-roots list) (:cancel-function (option function))) cl-worktree:manager)
(defun task-worktree--manager (configuration authorize &key writable-roots cancel-function)
  "Make a manager over the private durable ownership registry."
  (cl-worktree:make-manager
   :registry (merge-pathnames "task-worktrees/registry/" (config :data-root configuration))
   :runner   (task-worktree--runner configuration authorize :writable-roots  writable-roots
                                                            :cancel-function cancel-function)))

(-> task-worktree--git-admin-roots (pathname) list)
(defun task-worktree--git-admin-roots (source)
  "Resolve SOURCE's private and shared Git administrative directories."
  (let* ((source  (uiop:ensure-directory-pathname source))
         (dot-git (merge-pathnames ".git" source))
         (status  (platform-path-status *platform* dot-git :follow-links-p t))
         (git-dir
           (when status
             (case (platform-file-status-kind status)
               (:directory
                (uiop:ensure-directory-pathname dot-git))
               (:file
                (let* ((text (uiop:read-file-string dot-git))
                       (line (first (uiop:split-string text
                                                      :separator '(#\Newline #\Return)))))
                  (unless (and line (uiop:string-prefix-p "gitdir: " line))
                    (task-worktree--error "Malformed Git administrative pointer."))
                  (merge-pathnames (uiop:parse-native-namestring (subseq line 8)) source)))
               (otherwise
                (task-worktree--error "Git administrative path is not a file or directory."))))))
    (when git-dir
      (let* ((git-dir   (uiop:ensure-directory-pathname
                         (platform-truename *platform* git-dir)))
             (commondir (merge-pathnames "commondir" git-dir))
             (common-dir
               (if (uiop:file-exists-p commondir)
                   (merge-pathnames
                    (uiop:ensure-directory-pathname
                     (uiop:parse-native-namestring
                      (string-right-trim '(#\Newline #\Return)
                                         (uiop:read-file-string commondir))))
                    git-dir)
                   git-dir)))
        (remove-duplicates
         (list git-dir (uiop:ensure-directory-pathname
                        (platform-truename *platform* common-dir)))
         :test #'equal)))))

(-> task-worktree-command-writable-roots (tool-context) list)
(defun task-worktree-command-writable-roots (context)
  "Return shell write scopes for the exact isolated checkout owned by CONTEXT."
  (let* ((agent     (tool-context-agent context))
         (job       (and (typep agent 'task-child-agent) (task-child-agent-job agent)))
         (metadata  (and job (getf (task-job-item job) :worktree)))
         (workspace (config :working-directory (tool-context-configuration context))))
    (when metadata
      (unless (and (equal (getf metadata :owner) (task-worktree--owner job))
                   (equal (platform-truename *platform* workspace)
                          (platform-truename *platform* (getf metadata :path))))
        (task-worktree--error "Shell workspace does not match the child's owned checkout."))
      (cons workspace (task-worktree--git-admin-roots (pathname (getf metadata :source)))))))

(-> task-worktree--owner (task-job) string)
(defun task-worktree--owner (job)
  "Return a durable conversation-qualified child execution owner."
  (format nil "~A/~A" (task-job-root-conversation-identifier job)
          (task-job-execution-identifier job)))

(-> task-worktree--metadata (cl-worktree:worktree string string) list)
(defun task-worktree--metadata (handle kind metadata-path)
  "Return the portable task identity needed for extraction and recovery."
  (list :version 1 :id (cl-worktree:worktree-id handle)
        :owner       (cl-worktree:worktree-owner handle)
        :source      (namestring (cl-worktree:worktree-source handle))
        :path        (namestring (cl-worktree:worktree-path handle))
        :baseline    (cl-worktree:worktree-baseline handle)
        :artifact-kind kind :metadata-path metadata-path
        :setup-state (cl-worktree:worktree-state handle)))

(defvar *task-worktree-cleanup-job* nil
  "Task owning the Git command currently being executed.")

(defvar *task-worktree-cleanups* nil
  "Native Git cleanup ownership retained beyond task terminal publication.")

(defvar *task-worktree-cleanup-lock* (make-lock "Autolith Git cleanup ownership")
  "Protect retained Git cleanup records independently of the lifecycle lock.")

(-> task-worktree--retain-cleanup
    (cl-exec-sandbox:sandbox-process-cleanup-error pathname) null)
(defun task-worktree--retain-cleanup (condition source)
  "Accept native ownership before the Git caller unwinds."
  (sb-sys:without-interrupts
    (with-lock-held (*task-worktree-cleanup-lock*)
      (push (list :job       *task-worktree-cleanup-job*
                  :source    (pathname
                              (or (and *task-worktree-cleanup-job*
                                       (getf (getf (task-job-item *task-worktree-cleanup-job*) :worktree)
                                             :source))
                                  source))
                  :condition condition
                  :thread    (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-thread condition)
                  :reaped-p
                  (and (null (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-thread condition))
                       (not (cl-exec-sandbox:sandbox-process-cleanup-error-alive-p condition))))
            *task-worktree-cleanups*)))
  nil)

(-> task-worktree--cleanup-pending-p (list) boolean)
(defun task-worktree--cleanup-pending-p (record)
  "Retry missing native ownership and retire only proven reaping and stopped readers."
  (let* ((condition (getf record :condition))
         (retry     (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-retry condition))
         (owner     (getf record :thread)))
    (unless (or owner (getf record :reaped-p))
      (when retry
        (handler-case (setf owner (funcall retry) (getf record :thread) owner)
          (error () nil))))
    (when (and owner (not (thread-alive-p owner)))
      (setf (getf record :reaped-p)
            (handler-case (not (null (join-thread owner))) (error () nil))
            (getf record :thread) nil))
    (not (and (getf record :reaped-p)
              (notany #'thread-alive-p
                      (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-readers condition))))))

(-> task-worktree-cleanup-snapshot (&key (:job (option task-job)) (:source t)) list)
(defun task-worktree-cleanup-snapshot (&key job source)
  "Return portable native cleanup ownership without waiting for any live thread."
  (with-lock-held (*task-worktree-cleanup-lock*)
    (setf *task-worktree-cleanups*
          (delete-if-not #'task-worktree--cleanup-pending-p *task-worktree-cleanups*))
    (loop for record in *task-worktree-cleanups*
          for condition = (getf record :condition)
          when (and (or (null job) (eq job (getf record :job)))
                    (or (null source)
                        (equal (handler-case
                                   (uiop:ensure-directory-pathname (platform-truename *platform* source))
                                 (error () (uiop:ensure-directory-pathname source)))
                               (handler-case
                                   (uiop:ensure-directory-pathname
                                    (platform-truename *platform* (getf record :source)))
                                 (error () (uiop:ensure-directory-pathname (getf record :source)))))))
            collect
            (list :source    (namestring (getf record :source))
                  :condition (princ-to-string condition)
                  :cleanup-thread
                  (when (getf record :thread)
                    (bordeaux-threads:thread-name (getf record :thread)))
                  :cleanup-thread-alive-p
                  (not (null (and (getf record :thread) (thread-alive-p (getf record :thread)))))
                  :cleanup-retry-p
                  (not (null (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-retry condition)))
                  :cleanup-readers-alive
                  (count-if #'thread-alive-p
                            (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-readers condition))
                  :cleanup-readers
                  (loop for reader in
                        (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-readers condition)
                        collect (list :name    (bordeaux-threads:thread-name reader)
                                      :alive-p (not (null (thread-alive-p reader)))))
                  :cleanup-errors
                  (mapcar #'princ-to-string
                          (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-errors condition))))))

(-> task-worktree--require-settled (t) null)
(defun task-worktree--require-settled (source)
  "Refuse Git recovery or deletion while an earlier native owner can still write."
  (when (task-worktree-cleanup-snapshot :source source)
    (task-worktree--error "Git cleanup is pending; inspect the task before retrying."
                          :code ':cleanup-pending))
  nil)

(-> task-worktree-configuration (task-job configuration) configuration)
(defun task-worktree-configuration (job configuration)
  "Create JOB's requested detached workspace before opening its child session.
Registry records survive cancellation between Git creation and metadata publication.
The source is never reset or stashed, and child cleanup is always explicit."
  (let ((policy (getf (task-job-item job) :isolation)))
    (let ((*task-worktree-cleanup-job* job))
     (unless policy (return-from task-worktree-configuration configuration))
     (with-lock-held (*task-worktree-lock*)
       (let* ((source        (config :working-directory configuration))
              (owner         (task-worktree--owner job))
              (path          (merge-pathnames
                              (format nil "task-worktrees/checkouts/~A/workspace/"
                                      (task-job-execution-identifier job))
                              (config :data-root configuration)))
              (container     (uiop:pathname-parent-directory-pathname path))
              (metadata-path (merge-pathnames "worktree.sexp" (task--artifact-root configuration job)))
              (manager       (task-worktree--manager
                              configuration (task-job-command-authorization-function job)
                              :cancel-function (lambda () (job-cancellation-requested-p job))
                              :writable-roots  (cons container (task-worktree--git-admin-roots source)))))
         (ensure-directories-exist (merge-pathnames "probe" container))
         (labels ((publish (handle)
                    ;; Only transfer durable ownership with interrupts suppressed.
                    ;; All Git commands and checkout work run with interrupts enabled.
                    (sb-sys:without-interrupts
                      (let ((metadata (task-worktree--metadata handle (getf policy :artifact-kind)
                                                              (namestring metadata-path))))
                        (setf (getf (task-job-item job) :worktree) metadata)
                        (snapshot-write metadata-path metadata)))))
           (let ((handle (cl-worktree:create-worktree
                          manager :source source :path path :owner owner
                          :baseline (getf policy :baseline) :dirty-policy (getf policy :dirty-policy)
                          :reserved-function #'publish)))
             (publish handle)
             (configuration-copy configuration :working-directory (cl-worktree:worktree-path handle)))))))))

(-> task-worktree--find-handle (cl-worktree:manager list) cl-worktree:worktree)
(defun task-worktree--find-handle (manager metadata)
  "Recover a matching owned handle and verify all retained identity fields."
  (task-worktree--require-settled (getf metadata :source))
  (let ((handles (multiple-value-list
                  (cl-worktree:discover-worktrees manager :owner (getf metadata :owner)
                                                 :source         (getf metadata :source)))))
    (cl-worktree:recover-worktree
     manager
     (or (find-if
          (lambda (handle)
            (and (equal (cl-worktree:worktree-id handle) (getf metadata :id))
                 (equal (cl-worktree:worktree-baseline handle) (getf metadata :baseline))
                 (equal (namestring (cl-worktree:worktree-source handle)) (getf metadata :source))
                 (equal (namestring (cl-worktree:worktree-path handle)) (getf metadata :path))))
          (append (first handles) (second handles)))
         (task-worktree--error "The owned worktree identity is unavailable or changed."))
     :owner (getf metadata :owner))))

(-> task-worktree--extract (cl-worktree:manager list) cl-worktree:artifact)
(defun task-worktree--extract (manager metadata)
  "Extract the selected complete artifact, including an interrupted child's edits."
  (let ((handle (task-worktree--find-handle manager metadata)))
    (if (equal (getf metadata :artifact-kind) "commit-range")
        (cl-worktree:extract-commit-range manager handle :owner (getf metadata :owner))
        (cl-worktree:extract-patch manager handle :owner (getf metadata :owner)))))

(-> task-worktree--write-artifact
    (pathname cl-worktree:artifact &key (:require-absent boolean)) pathname)
(defun task-worktree--write-artifact (path artifact &key require-absent)
  "Publish a complete portable artifact only when its serialized UTF-8 data fits.
Apply the same bound during publication and reading so terminal success always
identifies a readable secured artifact."
  (let ((text (task--write-readable-sexp (cl-worktree:artifact->plist artifact)
                                        :pretty-p t)))
    (when (> (task--utf8-length text) *task-worktree-artifact-limit*)
      (task-worktree--error "The complete engineering artifact exceeds its storage limit."))
    (snapshot-write-text path text :require-absent require-absent)))

(-> task-worktree-finalize-result (task-job list) list)
(defun task-worktree-finalize-result (job result)
  "Attach ownership and publish artifacts for uncancelled terminal results.
Cancelled tasks retain an explicit pending artifact for parent extraction without
waiting for Git or the lifecycle lock during terminal publication."
  (let ((metadata (getf (task-job-item job) :worktree)))
    (unless metadata (return-from task-worktree-finalize-result result))
    (setf (getf result :worktree) metadata
          (getf result :worktree-artifact-state) ':pending)
    (when (or (member (getf result :status) '(:cancelled :aborted :timeout))
              (job-cancellation-requested-p job))
      (setf (getf result :worktree-artifact-state) ':pending)
      (return-from task-worktree-finalize-result result))
    (handler-case
        (with-lock-held (*task-worktree-lock*)
          (let* ((configuration (agent-configuration (task-job-parent-agent job)))
                 (manager       (task-worktree--manager
                                 configuration (task-job-command-authorization-function job)
                                 :cancel-function (lambda () (job-cancellation-requested-p job))
                                 :writable-roots
                                 (cons (pathname (getf metadata :path))
                                       (task-worktree--git-admin-roots
                                        (pathname (getf metadata :source))))))
                 (artifact      (task-worktree--extract manager metadata))
                 (path          (merge-pathnames "worktree-artifact.sexp" (task--artifact-root configuration job))))
            (task-worktree--write-artifact path artifact :require-absent t)
            (setf (getf result :worktree-artifact-path) (namestring path)
                  (getf result :worktree-artifact-state) ':ready)))
      (error (condition)
        (setf (getf result :worktree-error) (princ-to-string condition))
        (setf (getf result :worktree-artifact-state) ':pending)
        (when (eq (getf result :status) ':success)
          (setf (getf result :status) ':failed
                (getf result :error) (format nil "Worktree artifact extraction failed: ~A" condition)))))
    result))

(-> task-worktree--read-artifact (list) cl-worktree:artifact)
(defun task-worktree--read-artifact (metadata)
  "Read and validate the durable artifact at the fixed metadata sibling path."
  (let* ((path     (merge-pathnames "worktree-artifact.sexp" (pathname (getf metadata :metadata-path))))
         (artifact (cl-worktree:plist->artifact
                    (snapshot-read path :grammar        (task--result-grammar)
                                        :maximum-octets *task-worktree-artifact-limit*))))
    (unless (and (equal (cl-worktree:artifact-worktree-id artifact) (getf metadata :id))
                 (equal (cl-worktree:artifact-owner artifact) (getf metadata :owner))
                 (equal (cl-worktree:artifact-source artifact) (getf metadata :source))
                 (equal (cl-worktree:artifact-isolated-path artifact) (getf metadata :path))
                 (equal (cl-worktree:artifact-baseline artifact) (getf metadata :baseline)))
      (task-worktree--error "Engineering artifact provenance does not match the task."))
    artifact))

(-> task-worktree--visible-metadata (task-worktree-tool tool-context string) list)
(defun task-worktree--visible-metadata (tool context identifier)
  "Resolve terminal metadata through existing job visibility or durable task lookup."
  (let* ((viewer (tool-context-agent context))
         (job    (task-orchestrator-visible-job-named
                  (task-orchestrator-tool-orchestrator tool) viewer identifier))
         (result (if job (getf (session-job-snapshot job) :result)
                     (task--durable-job-result viewer identifier "task.worktree"))))
    (cond
      ((and job (null result))
       (task-worktree--error
        (format nil "Task ~A is still running; inspect its worktree once job.wait reports it finished."
                identifier)))
      ((null result)
       (task-worktree--error "No visible terminal isolated task has that identifier; use the job ID or execution ID task.run reported."))
      ((null (getf result :worktree))
       (task-worktree--error
        (format nil "Task ~A did not run in an isolated worktree, so it has none to inspect." identifier))))
    (getf result :worktree)))

(-> task-worktree--owned-discovery (cl-worktree:manager tool-context pathname) list)
(defun task-worktree--owned-discovery (manager context source)
  "Return current primary conversation handles, including absent owned orphans."
  (let ((prefix (format nil "~A/" (conversation-identifier (tool-context-conversation context)))))
    (multiple-value-bind (active absent)
        (cl-worktree:discover-worktrees manager :source source)
      (remove-if-not
       (lambda (handle) (uiop:string-prefix-p prefix (cl-worktree:worktree-owner handle)))
       (append active absent)))))

(defmethod tool-execute ((tool task-worktree-tool) (context tool-context) arguments)
  "Inspect or deliberately integrate an isolated child's artifact, or clean its owned checkout."
  (when (typep (tool-context-agent context) 'task-child-agent)
    (task-worktree--error "Worktree integration and cleanup belong to the primary parent." :code ':authority))
  (task--validate-tool-arguments arguments '("action" "id" "source" "target" "discardDirty")
                                 "task.worktree")
  (let* ((action        (tool-argument arguments "action" :required t))
         (configuration (tool-context-configuration context))
         (authorize     (lambda (command directory)
                          (tool-context-authorize-command context command directory)))
         (writable-roots
           (when (member action '("apply" "commits" "abort") :test #'equal)
             (list (workspace-tool-path context (tool-argument arguments "target" :required t)
                                        :tool-name "task.worktree"))))
         (manager       (task-worktree--manager configuration authorize :writable-roots writable-roots)))
    (with-lock-held (*task-worktree-lock*)
      (let ((answer
              (cond
                ((equal action "discover")
                 (let* ((source (workspace-tool-path context (tool-argument arguments "source")
                                                     :tool-name "task.worktree"))
                        (prefix (format nil "~A/" (conversation-identifier (tool-context-conversation context)))))
                   (multiple-value-bind (active absent)
                       (cl-worktree:discover-worktrees manager :source source)
                     (flet ((owned (handles)
                              (loop for handle in handles
                                    when (uiop:string-prefix-p prefix (cl-worktree:worktree-owner handle))
                                      collect (task-worktree--metadata handle "patch" ""))))
                       (list :active (owned active) :absent (owned absent))))))
                ((equal action "cleanup-orphan")
                 (let* ((source (workspace-tool-path context (tool-argument arguments "source" :required t)
                                                     :tool-name "task.worktree"))
                        (id     (tool-argument arguments "id" :required t))
                        (handle (find id (task-worktree--owned-discovery manager context source)
                                      :key #'cl-worktree:worktree-id :test #'equal)))
                   (unless handle
                     (task-worktree--error "No owned orphan has that worktree ID."))
                   (when (some (lambda (job)
                                 (and (typep job 'task-job)
                                      (not (job-terminal-p job))
                                      (equal (task-worktree--owner job)
                                             (cl-worktree:worktree-owner handle))))
                               (task-orchestrator-list-visible-jobs
                                (task-orchestrator-tool-orchestrator tool)
                                (tool-context-agent context)))
                     (task-worktree--error "The checkout belongs to a live task; cancel and join it before cleanup."))
                   (let ((cleanup-manager
                           (task-worktree--manager
                            configuration authorize
                            :writable-roots
                            (cons (uiop:pathname-parent-directory-pathname
                                   (cl-worktree:worktree-path handle))
                                  (task-worktree--git-admin-roots source)))))
                     (list :removed (cl-worktree:cleanup-worktree
                                     cleanup-manager handle :owner (cl-worktree:worktree-owner handle)
                                     :discard-dirty                (tool-boolean-argument arguments "discardDirty"
                                                                                         :tool-name "task.worktree"))))))
                ((member action '("conflicts" "abort") :test #'equal)
                 (let ((target (workspace-tool-path context (tool-argument arguments "target" :required t)
                                                    :tool-name "task.worktree")))
                   (if (equal action "abort")
                       (list :aborted (cl-worktree:abort-integration manager target))
                       (list :conflicts (cl-worktree:integration-conflicts manager target)))))
                (t
                 (let* ((identifier (tool-argument arguments "id" :required t))
                        (metadata   (task-worktree--visible-metadata tool context identifier))
                        (manager
                          (task-worktree--manager
                           configuration authorize
                           :writable-roots
                           (append writable-roots
                                   (list (uiop:pathname-parent-directory-pathname
                                          (pathname (getf metadata :path))))
                                   (task-worktree--git-admin-roots (pathname (getf metadata :source)))))))
                   (cond
                     ((and (equal action "inspect")
                           (task-worktree-cleanup-snapshot :source (getf metadata :source)))
                      (list :identity metadata :artifact-state ':pending
                            :cleanup (task-worktree-cleanup-snapshot :source (getf metadata :source))))
                     ((equal action "cleanup")
                      (list :removed
                            (cl-worktree:cleanup-worktree
                             manager (task-worktree--find-handle manager metadata)
                             :owner         (getf metadata :owner)
                             :discard-dirty (tool-boolean-argument arguments "discardDirty"
                                                                  :tool-name "task.worktree"))))
                     ((equal action "extract")
                      (let* ((artifact (task-worktree--extract manager metadata))
                             (path     (merge-pathnames "worktree-artifact.sexp"
                                                        (pathname (getf metadata :metadata-path)))))
                        (task-worktree--write-artifact path artifact)
                        (list :artifact-path (namestring path) :baseline (cl-worktree:artifact-baseline artifact)
                              :paths (cl-worktree:artifact-paths artifact))))
                     ((member action '("inspect" "check" "apply" "commits") :test #'equal)
                      (let ((artifact (task-worktree--read-artifact metadata)))
                        (if (equal action "inspect")
                            (list :identity metadata :kind (cl-worktree:artifact-kind artifact)
                                  :tip (cl-worktree:artifact-tip artifact) :paths (cl-worktree:artifact-paths artifact)
                                  :artifact-path (namestring (merge-pathnames "worktree-artifact.sexp"
                                                                           (pathname (getf metadata :metadata-path)))))
                            (let ((target (workspace-tool-path context (tool-argument arguments "target" :required t)
                                                               :tool-name "task.worktree")))
                              (list :integrated
                                    (cond
                                      ((equal action "check")
                                       (cl-worktree:check-patch manager target (cl-worktree:artifact-patch artifact)))
                                      ((equal action "apply")
                                       (cl-worktree:apply-patch manager target (cl-worktree:artifact-patch artifact)))
                                      (t
                                       (cl-worktree:apply-commit-range manager target artifact))))))))
                     (t
                      (task-worktree--error "Unknown task.worktree action."))))))))
        (task-tool-result (bounded-string (task--write-readable-sexp answer :pretty-p t)
                                          :limit *task-tool-content-limit*) answer)))))

(-> task-worktree--cleanup-root (configuration pathname) pathname)
(defun task-worktree--cleanup-root (configuration checkout)
  "Select only one owned checkout or its dedicated workspace container.
Legacy checkouts directly beneath checkouts/ never authorize deleting their parent."
  (let* ((root             (uiop:ensure-directory-pathname
                            (merge-pathnames "task-worktrees/checkouts/" (config :data-root configuration))))
         (checkout         (uiop:ensure-directory-pathname checkout))
         (parent           (uiop:pathname-parent-directory-pathname checkout))
         (container-parent (uiop:pathname-parent-directory-pathname parent)))
    (cond
      ((and (equal parent root)
            (stringp (first (last (pathname-directory checkout)))))
       checkout)
      ((and (equal container-parent root)
            (stringp (first (last (pathname-directory parent))))
            (equal (first (last (pathname-directory checkout))) "workspace"))
       parent)
      (t
       (task-worktree--error "Checkout is outside its dedicated task container." :code ':ownership)))))

(-> task-worktree-discard (configuration string &key (:pathname (option pathname))) null)
(defun task-worktree-discard (configuration channel &key pathname)
  "Discard a removed child's verified checkout without widening legacy cleanup."
  (let* ((pathname (or pathname
                       (find channel (conversation-task-pathnames (config :data-root configuration))
                             :key #'pathname-name :test #'equal)))
         (record   (and pathname
                        (merge-pathnames "worktree.sexp" (uiop:pathname-directory-pathname pathname))))
         (metadata (and record (probe-file record) (snapshot-read record))))
    (when metadata
      (with-lock-held (*task-worktree-lock*)
        (let* ((checkout     (pathname (getf metadata :path)))
               (cleanup-root (task-worktree--cleanup-root configuration checkout))
               (manager      (task-worktree--manager
                              configuration (constantly ':full-access)
                              :writable-roots (cons cleanup-root
                                                    (task-worktree--git-admin-roots
                                                     (pathname (getf metadata :source)))))))
          (cl-worktree:cleanup-worktree manager (task-worktree--find-handle manager metadata)
                                        :owner (getf metadata :owner) :discard-dirty t)
          (platform-delete-directory-tree *platform* cleanup-root
                                          :validate t :if-does-not-exist ':ignore)))))
  nil)

(-> task-worktree-register-tool (tool-registry task-orchestrator) tool-registry)
(defun task-worktree-register-tool (registry orchestrator)
  "Register the primary parent's explicit engineering artifact lifecycle operation."
  (tool-registry-register
   registry
   (make-instance
    'task-worktree-tool :orchestrator orchestrator :namespace "task" :name "worktree"
    :description "Inspect/extract isolated task artifacts, explicitly check/apply patches or commits to an authorized target, inspect/abort conflicts, discover owned orphans, or explicitly clean owned checkouts. No automatic parent integration or dirty cleanup."
    :parameters
    (tool-object-schema
     (json-object
      "action" (json-object "type" "string" "enum"
                            (json-array "inspect" "extract" "check" "apply" "commits" "conflicts" "abort" "discover" "cleanup" "cleanup-orphan"))
      "id" (tool-string-property "Visible terminal job ID, durable execution ID, or discovery worktree ID for cleanup-orphan.")
      "source" (tool-string-property "Repository to scan for owned orphan recovery; defaults to workspace.")
      "target" (tool-string-property "Explicit repository integration target.")
      "discardDirty" (tool-boolean-property "Explicitly discard this owned checkout's edits during cleanup."))
     '("action"))))
  registry)
