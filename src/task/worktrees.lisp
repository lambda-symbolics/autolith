(in-package #:autolith)

;;;; -- Isolated Task Workspaces --

(defvar *task-worktree-lock* (make-lock "Autolith task Git operations")
  "Serialize owned Git lifecycle and integration operations within this image.")

(defparameter *task-worktree-artifact-limit* (* 16 1024 1024)
  "Maximum captured patch characters and serialized engineering artifact octets.")

(define-condition task-worktree-error (task-error) ()
  (:documentation "An isolated task could not satisfy its authority or lifecycle contract."))

(define-condition task-worktree-authority-error (task-worktree-error) ()
  (:documentation "Git execution was denied by the caller's command policy."))

(defvar *task-worktree-authority-denial* nil
  "Dynamically bound cell retaining a denial until library unwinding completes.")

(defclass task-worktree-tool (tool)
  ()
  (:documentation "Explicit parent inspection, integration and owned cleanup."))

(-> task-worktree--error (string &key (:code (option keyword))) nil)
(defun task-worktree--error (message &key code)
  "Signal a product-specific worktree failure with tool attribution."
  (error 'task-worktree-error :message message :code code :tool-name "child.worktree"))

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
    (list :baseline      baseline
          :dirty-policy  (if (string= dirty "reject") ':reject ':ignore)
          :artifact-kind (if (string= kind "patch") ':patch ':commit-range))))

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
  "Forward Git configuration and let sandbox temporary bindings take precedence."
  (git-process-environment :overrides overrides :absolute-xdg-p t))

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
           (command   (format nil "child.worktree Git argv ~S" argv))
           (decision  (funcall authorize command directory)))
        (unless (member decision '(:sandboxed :full-access))
          (let ((condition (make-condition 'task-worktree-authority-error
                                           :message "Worktree Git command was denied."
                                           :code ':authority :tool-name "child.worktree")))
            (when *task-worktree-authority-denial*
              (setf (first *task-worktree-authority-denial*) condition))
            (error condition)))
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
                           :clear-environment-p             t
                           :cancel-function                 cancel-function
                           :output-limit                    (if (member "diff" argv :test #'equal)
                                                                *task-worktree-artifact-limit* 65536)
                           :error-output-limit              65536)))
                    (when (eq (cl-exec-sandbox:sandbox-result-status result) ':cancelled)
                      (task-worktree--error "Worktree Git command cancelled." :code ':cancelled))
                    (when (or (sandbox-result-output-truncated-p result)
                              (cl-exec-sandbox:sandbox-result-error-output-truncated-p result))
                      (task-worktree--error "Git command exceeded the artifact capture limit."))
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

(-> task-worktree--admin-path-kind (pathname) (option keyword))
(defun task-worktree--admin-path-kind (path)
  "Distinguish absent Git metadata from unsupported or unresolvable entries."
  (when-let ((status (or (platform-path-status *platform* path :follow-links-p t)
                         (platform-path-status *platform* path :follow-links-p nil))))
    (platform-file-status-kind status)))

(-> task-worktree--git-admin-roots (pathname) list)
(defun task-worktree--git-admin-roots (source)
  "Use the platform filesystem adapter to resolve SOURCE's exact Git layout."
  (handler-case
      (cl-worktree:checkout-admin-roots source
                                       :path-kind    #'task-worktree--admin-path-kind
                                       :canonicalize (lambda (path) (platform-truename *platform* path)))
    (cl-worktree:ownership-error (condition)
      (task-worktree--error (cl-worktree:failure-detail condition)))))

(-> task-worktree--owner (string string) string)
(defun task-worktree--owner (parent-identifier child-identifier)
  "Return the durable owner of a child checkout."
  (format nil "~A/~A" (conversation-identifier-validate-path-component parent-identifier)
          (conversation-identifier-normalize child-identifier)))

(-> task-worktree--metadata-path (configuration string string) pathname)
(defun task-worktree--metadata-path (configuration parent-identifier child-identifier)
  "Return the private sidecar path for a child conversation."
  (declare (ignore parent-identifier))
  (merge-pathnames
   (format nil "task-worktrees/children/~A/worktree.sexp"
           (conversation-identifier-normalize child-identifier))
   (config :data-root configuration)))

(-> task-worktree--read-metadata (configuration string string) (option list))
(defun task-worktree--read-metadata (configuration parent-identifier child-identifier)
  "Read and verify the child's durable checkout sidecar, when present."
  (let ((path (task-worktree--metadata-path configuration parent-identifier child-identifier)))
    (when (probe-file path)
      (let ((metadata (snapshot-read path :grammar       (task--result-grammar)
                                         :maximum-octets *task-worktree-artifact-limit*)))
        (unless (and (equal (getf metadata :owner)
                            (task-worktree--owner parent-identifier child-identifier))
                     (equal (getf metadata :metadata-path) (namestring path)))
          (task-worktree--error "Worktree metadata owner or path does not match the child."))
        metadata))))

(-> task-worktree--removed-metadata (configuration string) (option list))
(defun task-worktree--removed-metadata (configuration child-identifier)
  "Resolve a deleted child's sidecar without depending on its removed world node."
  (let ((path (task-worktree--metadata-path configuration "" child-identifier)))
    (when (probe-file path)
      (let* ((metadata  (snapshot-read path :grammar        (task--result-grammar)
                                            :maximum-octets *task-worktree-artifact-limit*))
             (owner     (getf metadata :owner))
             (separator (and (stringp owner) (position #\/ owner))))
        (unless separator
          (task-worktree--error "Worktree metadata has an invalid owner."))
        (task-worktree--read-metadata configuration (subseq owner 0 separator) child-identifier)))))

(-> task-worktree-command-writable-roots (tool-context) list)
(defun task-worktree-command-writable-roots (context)
  "Return write scopes only for the checkout owned by this conversation."
  (let* ((conversation  (tool-context-conversation context))
         (parent        (conversation-parent-identifier conversation))
         (configuration (tool-context-configuration context))
         (metadata      (and parent (conversation-identifier-path-fragment
                                     (conversation-identifier conversation))
                             (task-worktree--read-metadata
                              configuration parent (conversation-identifier conversation))))
         (workspace     (config :working-directory configuration)))
    (when metadata
      (unless (equal (platform-truename *platform* workspace)
                     (platform-truename *platform* (getf metadata :path)))
        (task-worktree--error "Shell workspace does not match the child's owned checkout."))
      (cons workspace (task-worktree--git-admin-roots (pathname (getf metadata :source)))))))

(-> task-worktree--metadata (cl-worktree:worktree (member :patch :commit-range) string) list)
(defun task-worktree--metadata (handle kind metadata-path)
  "Return the portable task identity needed for extraction and recovery."
  (list :version 1 :id (cl-worktree:worktree-id handle)
        :owner       (cl-worktree:worktree-owner handle)
        :source      (namestring (cl-worktree:worktree-source handle))
        :path        (namestring (cl-worktree:worktree-path handle))
        :baseline    (cl-worktree:worktree-baseline handle)
        :artifact-kind kind :metadata-path metadata-path
        :setup-state (cl-worktree:worktree-state handle)))

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
      (push (list :source    (pathname source)
                  :condition condition
                  :owner     (cl-exec-sandbox:sandbox-process-cleanup-error-owner condition))
            *task-worktree-cleanups*)))
  nil)

(-> task-worktree--cleanup-pending-p (list) boolean)
(defun task-worktree--cleanup-pending-p (record)
  "Advance the library's cleanup owner once without waiting for native work."
  (eq ':pending (cl-exec-sandbox:sandbox-cleanup-poll (getf record :owner))))

(-> task-worktree-cleanup-snapshot (&key (:source t)) list)
(defun task-worktree-cleanup-snapshot (&key source)
  "Return portable native cleanup ownership without waiting for any live thread."
  (with-lock-held (*task-worktree-cleanup-lock*)
    (setf *task-worktree-cleanups*
          (delete-if-not #'task-worktree--cleanup-pending-p *task-worktree-cleanups*))
    (loop for record in *task-worktree-cleanups*
          for condition = (getf record :condition)
          when (or (null source)
                   (equal (handler-case
                              (uiop:ensure-directory-pathname (platform-truename *platform* source))
                            (error () (uiop:ensure-directory-pathname source)))
                          (handler-case
                              (uiop:ensure-directory-pathname
                               (platform-truename *platform* (getf record :source)))
                           (error () (uiop:ensure-directory-pathname (getf record :source))))))
            collect
            (append (list :source    (namestring (getf record :source))
                          :condition (princ-to-string condition))
                    (cl-exec-sandbox:sandbox-cleanup-status (getf record :owner))
                    (list :cleanup-errors
                          (mapcar #'princ-to-string
                                  (cl-exec-sandbox:sandbox-process-cleanup-error-cleanup-errors condition)))))))

(-> task-worktree--require-settled (t) null)
(defun task-worktree--require-settled (source)
  "Refuse Git recovery or deletion while an earlier native owner can still write."
  (when (task-worktree-cleanup-snapshot :source source)
    (task-worktree--error "Git cleanup is pending; inspect the task before retrying."
                          :code ':cleanup-pending))
  nil)

(-> task-worktree-configuration
    (string string configuration &key (:isolation list) (:authorize (option function))
            (:cancel-function (option function)))
    (values configuration (option list)))
(defun task-worktree-configuration (parent-identifier child-identifier configuration
                                    &key isolation authorize cancel-function)
  "Create the child's detached checkout and publish its durable sidecar.
Return the child configuration and the sidecar metadata as two values."
  (unless isolation
    (return-from task-worktree-configuration (values configuration nil)))
  (unless authorize
    (task-worktree--error "Worktree creation requires command authorization." :code ':authority))
  (with-lock-held (*task-worktree-lock*)
    (let* ((source        (config :working-directory configuration))
           (owner         (task-worktree--owner parent-identifier child-identifier))
           (path          (merge-pathnames
                           (format nil "task-worktrees/checkouts/~A/workspace/"
                                   (conversation-identifier-normalize child-identifier))
                           (config :data-root configuration)))
           (container     (uiop:pathname-parent-directory-pathname path))
           (metadata-path (task-worktree--metadata-path configuration parent-identifier child-identifier))
           (manager       (task-worktree--manager
                           configuration authorize :cancel-function cancel-function
                           :writable-roots                          (cons container (task-worktree--git-admin-roots source)))))
      (ensure-directories-exist (merge-pathnames "probe" container))
      (labels ((publish (handle)
                 (sb-sys:without-interrupts
                   (snapshot-write metadata-path
                                   (task-worktree--metadata handle (getf isolation :artifact-kind)
                                                            (namestring metadata-path))))))
        (let* ((handle   (cl-worktree:create-worktree
                          manager :source source :path path :owner owner
                          :baseline (getf isolation :baseline) :dirty-policy (getf isolation :dirty-policy)
                          :reserved-function #'publish))
               (metadata (task-worktree--metadata handle (getf isolation :artifact-kind)
                                                   (namestring metadata-path))))
          (publish handle)
          (values (configuration-copy configuration :working-directory (cl-worktree:worktree-path handle))
                  metadata))))))

(-> task-worktree--find-handle (cl-worktree:manager list) cl-worktree:worktree)
(defun task-worktree--find-handle (manager metadata)
  "Recover a matching owned handle and verify all retained identity fields."
  (task-worktree--require-settled (getf metadata :source))
  (handler-case
      (cl-worktree:recover-worktree-by-identity manager
                                               :owner    (getf metadata :owner)
                                               :source   (getf metadata :source)
                                               :id       (getf metadata :id)
                                               :baseline (getf metadata :baseline)
                                               :path     (getf metadata :path))
    (cl-worktree:worktree-identity-mismatch (condition)
      (task-worktree--error (cl-worktree:failure-detail condition)))))

(-> task-worktree--extract (cl-worktree:manager list) cl-worktree:artifact)
(defun task-worktree--extract (manager metadata)
  "Extract the selected complete artifact, including an interrupted child's edits."
  (let ((handle (task-worktree--find-handle manager metadata)))
    ;; Metadata written before the kind was a keyword holds its name as a string.
    (if (member (getf metadata :artifact-kind) '(:commit-range "commit-range") :test #'equal)
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
    (when (> (utf8-string-length text) *task-worktree-artifact-limit*)
      (task-worktree--error "The complete engineering artifact exceeds its storage limit."))
    (snapshot-write-text path text :require-absent require-absent)))

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

(-> task-worktree--visible-metadata (tool-context string) list)
(defun task-worktree--visible-metadata (context identifier)
  "Resolve the durable sidecar of a verified child channel."
  (or (task-worktree--read-metadata
       (tool-context-configuration context)
       (conversation-identifier (tool-context-conversation context)) identifier)
      (task-worktree--error "The child has no isolated worktree metadata.")))

(-> task-worktree--owned-discovery (cl-worktree:manager tool-context pathname) (values list list))
(defun task-worktree--owned-discovery (manager context source)
  "Return present conversation-owned handles and absent orphans as two values."
  (let ((prefix (format nil "~A/" (conversation-identifier (tool-context-conversation context)))))
    (flet ((foreign-p (handle)
             (not (uiop:string-prefix-p prefix (cl-worktree:worktree-owner handle)))))
      (multiple-value-bind (owned absent)
          (cl-worktree:discover-worktrees manager :source source)
        (values (remove-if #'foreign-p (set-difference owned absent :test #'eq))
                (remove-if #'foreign-p absent))))))

(defmethod tool-execute :around ((tool task-worktree-tool) (context tool-context) arguments)
  "Preserve the typed authorization denial after library unwinding and cleanup."
  (declare (ignore tool context arguments))
  (let ((*task-worktree-authority-denial* (list nil)))
    (handler-case (call-next-method)
      (cl-worktree:worktree-error (condition)
        (error (or (first *task-worktree-authority-denial*) condition))))))

(defmethod tool-execute ((tool task-worktree-tool) (context tool-context) arguments)
  "Inspect or deliberately integrate an isolated child's artifact, or clean its owned checkout."
  (declare (ignore tool))
  (task--validate-tool-arguments arguments '("action" "id" "source" "target" "discardDirty")
                                 "child.worktree")
  (let* ((action        (tool-argument arguments "action" :required t))
         (configuration (tool-context-configuration context))
         (authorize     (lambda (command directory)
                          (tool-context-authorize-command context command directory)))
         (writable-roots
           (when (member action '("apply" "commits" "abort") :test #'equal)
             (list (workspace-tool-path context (tool-argument arguments "target" :required t)
                                        :tool-name "child.worktree"))))
         (manager       (task-worktree--manager configuration authorize :writable-roots writable-roots)))
    (with-lock-held (*task-worktree-lock*)
      (let ((answer
              (cond
                ((equal action "discover")
                 (let ((source (workspace-tool-path context (tool-argument arguments "source")
                                                    :tool-name "child.worktree")))
                   (multiple-value-bind (active absent)
                       (task-worktree--owned-discovery manager context source)
                     (flet ((metadata (handles)
                              (loop for handle in handles collect (task-worktree--metadata handle ':patch ""))))
                       (list :active (metadata active) :absent (metadata absent))))))
                ((equal action "cleanup-orphan")
                 (let* ((source (workspace-tool-path context (tool-argument arguments "source" :required t)
                                                     :tool-name "child.worktree"))
                        (id     (tool-argument arguments "id" :required t))
                        (handle (multiple-value-bind (active absent)
                                    (task-worktree--owned-discovery manager context source)
                                  (or (find id active :key #'cl-worktree:worktree-id :test #'equal)
                                      (find id absent :key #'cl-worktree:worktree-id :test #'equal)))))
                   (unless handle
                     (task-worktree--error "No owned orphan has that worktree ID."))
                   (let* ((owner     (cl-worktree:worktree-owner handle))
                          (separator (position #\/ owner :from-end t))
                          (child     (and separator (subseq owner (1+ separator))))
                          (lease     (and child (conversation-identifier-path-fragment child)
                                          (handler-case (conversation-lease-acquire configuration child)
                                            (conversation-in-use ()
                                              (task-worktree--error "The checkout owner is busy."))))))
                     (unless lease
                       (task-worktree--error "Cannot safely lease this orphan's owner."))
                     (unwind-protect
                          (progn
                            (when (find child
                                        (world-channel-children
                                         (world-load configuration)
                                         (conversation-identifier (tool-context-conversation context)))
                                        :key #'channel-identifier :test #'equal)
                              (task-worktree--error "The checkout belongs to an existing child; use cleanup by child ID."))
                            (let ((cleanup-manager
                                    (task-worktree--manager
                                     configuration authorize
                                     :writable-roots
                                     (cons (uiop:pathname-parent-directory-pathname
                                            (cl-worktree:worktree-path handle))
                                           (task-worktree--git-admin-roots source)))))
                              (list :removed (cl-worktree:cleanup-worktree
                                              cleanup-manager handle :owner owner
                                              :discard-dirty                (tool-boolean-argument
                                                                             arguments "discardDirty"
                                                                             :tool-name "child.worktree")))))
                       (conversation-lease-release lease)))))
                ((member action '("conflicts" "abort") :test #'equal)
                 (let ((target (workspace-tool-path context (tool-argument arguments "target" :required t)
                                                    :tool-name "child.worktree")))
                   (if (equal action "abort")
                       (list :aborted (cl-worktree:abort-integration manager target))
                       (list :conflicts (cl-worktree:integration-conflicts manager target)))))
                (t
                 (let* ((identifier (task-child--find context (tool-argument arguments "id" :required t)
                                                      "child.worktree"))
                        (answer     nil))
                   (task-child--call-with-lease
                    context identifier
                    (lambda ()
                      (setf answer
                            (let* ((metadata (task-worktree--visible-metadata context identifier))
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
                                (let ((handle        (task-worktree--find-handle manager metadata))
                                      (discard-dirty (tool-boolean-argument arguments "discardDirty"
                                                                            :tool-name "child.worktree")))
                                  (setf (getf metadata :discarding) t)
                                  (snapshot-write (pathname (getf metadata :metadata-path)) metadata)
                                  (list :removed
                                        (cl-worktree:cleanup-worktree manager handle
                                                                      :owner         (getf metadata :owner)
                                                                      :discard-dirty discard-dirty))))
                               ((member action '("extract" "inspect") :test #'equal)
                                (let* ((artifact (task-worktree--extract manager metadata))
                                       (path     (merge-pathnames "worktree-artifact.sexp"
                                                                  (pathname (getf metadata :metadata-path)))))
                                  (task-worktree--write-artifact path artifact)
                                  (list :identity metadata :kind (cl-worktree:artifact-kind artifact)
                                        :tip (cl-worktree:artifact-tip artifact) :paths (cl-worktree:artifact-paths artifact)
                                        :artifact-path (namestring path))))
                               ((member action '("check" "apply" "commits") :test #'equal)
                                (let ((artifact (task-worktree--read-artifact metadata)))
                                     (let ((target (workspace-tool-path context (tool-argument arguments "target" :required t)
                                                                        :tool-name "child.worktree")))
                                       (list :integrated
                                             (cond
                                               ((equal action "check")
                                                (cl-worktree:check-patch manager target (cl-worktree:artifact-patch artifact)))
                                               ((equal action "apply")
                                                (cl-worktree:apply-patch manager target (cl-worktree:artifact-patch artifact)))
                                               (t
                                                (cl-worktree:apply-commit-range manager target artifact)))))))
                               (t
                                (task-worktree--error "Unknown child.worktree action."))))))
                    :tool-name "child.worktree")
                   answer)))))
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
            (stringp (lastcar (pathname-directory checkout))))
       checkout)
      ((and (equal container-parent root)
            (stringp (lastcar (pathname-directory parent)))
            (equal (lastcar (pathname-directory checkout)) "workspace"))
       parent)
      (t
       (task-worktree--error "Checkout is outside its dedicated task container." :code ':ownership)))))

(-> task-worktree--discard-completed-p
    (configuration cl-worktree:manager list &key (:authorize function)) boolean)
(defun task-worktree--discard-completed-p (configuration manager metadata &key authorize)
  "Verify a journaled discard has no remaining checkout, registration or owner record."
  (let* ((source (pathname (getf metadata :source)))
         (path   (getf metadata :path))
         (trees  (cl-worktree:discover-worktrees manager :source source)))
    (when (or (probe-file path)
              (find (getf metadata :id) trees :key #'cl-worktree:worktree-id :test #'equal)
              (find path trees :key #'cl-worktree:worktree-path :test #'equal))
      (return-from task-worktree--discard-completed-p nil))
    (multiple-value-bind (output error-output status)
        (funcall (task-worktree--runner configuration authorize)
                 '("git" "worktree" "list" "--porcelain" "-z") :directory source)
      (unless (zerop status)
        (task-worktree--error (format nil "Cannot verify worktree removal: ~A" error-output)))
      (let ((entry (concatenate 'string "worktree " (string-right-trim '(#\/ #\\) path))))
        (not (member entry (uiop:split-string output :separator (list #\Null)) :test #'equal))))))

(-> task-worktree-discard (configuration string &key (:pathname (option pathname))
                                         (:parent-identifier (option string))
                                         (:authorize (option function))) null)
(defun task-worktree-discard (configuration channel
                              &key pathname parent-identifier
                                (authorize (constantly ':full-access)))
  "Discard a removed child's verified checkout without widening legacy cleanup.
PARENT-IDENTIFIER identifies a child whose spawn failed before world publication.
AUTHORIZE governs Git cleanup, defaulting to trusted channel-removal authority."
  (unless authorize
    (task-worktree--error "Worktree cleanup requires command authorization." :code ':authority))
  (let* ((pathname (or pathname
                       (conversation-task-pathname-for-id configuration channel)))
         (record   (and pathname
                        (merge-pathnames "worktree.sexp" (uiop:pathname-directory-pathname pathname))))
         (node     (world-channel-find (world-load configuration) channel))
         (parent   (or parent-identifier (and node (channel-parent node))))
         (metadata (or (and (conversation-identifier-path-fragment channel)
                            (if parent
                                (task-worktree--read-metadata configuration parent channel)
                                (task-worktree--removed-metadata configuration channel)))
                       (and record (probe-file record) (snapshot-read record)))))
    (when metadata
      (with-lock-held (*task-worktree-lock*)
        (let* ((checkout     (pathname (getf metadata :path)))
               (cleanup-root (task-worktree--cleanup-root configuration checkout))
               (sidecar      (and (conversation-identifier-path-fragment channel)
                                  (task-worktree--metadata-path configuration "" channel)))
               (modern-p     (and sidecar (equal (getf metadata :metadata-path) (namestring sidecar))))
               (manager      (task-worktree--manager
                              configuration authorize
                              :writable-roots (cons cleanup-root
                                                    (task-worktree--git-admin-roots
                                                     (pathname (getf metadata :source))))))
               (handle       (handler-case (task-worktree--find-handle manager metadata)
                               (task-worktree-error (condition)
                                 (unless (and modern-p (getf metadata :discarding)
                                               (task-worktree--discard-completed-p
                                                configuration manager metadata :authorize authorize))
                                   (error condition))
                                 nil))))
          (when handle
            (when modern-p
              (setf (getf metadata :discarding) t)
              (snapshot-write sidecar metadata))
            (unless (cl-worktree:cleanup-worktree manager handle
                                                  :owner (getf metadata :owner) :discard-dirty t)
              (task-worktree--error "Worktree cleanup was retained." :code ':cleanup-pending)))
          (platform-delete-directory-tree *platform* cleanup-root
                                          :validate t :if-does-not-exist ':ignore)
          (when (and modern-p (probe-file sidecar))
            (delete-file sidecar))))))
  nil)

(-> task-worktree-register-tool (tool-registry) tool-registry)
(defun task-worktree-register-tool (registry)
  "Register the primary parent's explicit engineering artifact lifecycle operation."
  (tool-registry-register
   registry
   (make-instance
    'task-worktree-tool :namespace "child" :name "worktree"
    :description "Inspect/extract isolated task artifacts, explicitly check/apply patches or commits to an authorized target, inspect/abort conflicts, discover owned orphans, or explicitly clean owned checkouts. No automatic parent integration or dirty cleanup."
    :parameters
    (tool-object-schema
     (json-object
      "action" (json-object "type" "string" "enum"
                            (json-array "inspect" "extract" "check" "apply" "commits" "conflicts" "abort" "discover" "cleanup" "cleanup-orphan"))
      "id" (tool-string-property "Child name or channel ID, or discovery worktree ID for cleanup-orphan.")
      "source" (tool-string-property "Repository to scan for owned orphan recovery; defaults to workspace.")
      "target" (tool-string-property "Explicit repository integration target.")
      "discardDirty" (tool-boolean-property "Explicitly discard this owned checkout's edits during cleanup."))
     '("action"))))
  registry)
