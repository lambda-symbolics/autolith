(in-package #:autolith)

;;;; -- Isolated Task Worktree Tests --

(-> task-worktree-tests--write (pathname string) pathname)
(defun task-worktree-tests--write (path text)
  "Write one ordinary fixture file without involving resource revisions."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction ':output :if-exists ':supersede
                              :if-does-not-exist ':create :external-format ':utf-8)
    (write-string text stream))
  path)

(-> task-worktree-tests--authorize (string pathname) keyword)
(defun task-worktree-tests--authorize (command directory)
  "Grant direct execution only to this disposable fixture's injected runner."
  (declare (ignore command directory))
  ':full-access)

(-> task-worktree-tests--sandboxed (string pathname) keyword)
(defun task-worktree-tests--sandboxed (command directory)
  "Authorize only the normal isolated-command sandbox for this fixture."
  (declare (ignore command directory))
  ':sandboxed)

(-> task-worktree-tests--git
    (configuration pathname list &key (:authorize function) (:writable-roots list)) string)
(defun task-worktree-tests--git (configuration directory arguments
                                 &key (authorize #'task-worktree-tests--authorize)
                                   writable-roots)
  "Run real Git through the product runner and require success."
  (multiple-value-bind (output error-output status)
      (funcall (task-worktree--runner configuration authorize
                                       :writable-roots writable-roots)
               (cons "git" arguments) :directory directory)
    (test-assert (zerop status) (format nil "Git ~S: ~A" arguments error-output))
    output))

(-> task-worktree-tests--fixture (function &optional t) t)
(defun task-worktree-tests--fixture (function &optional ignored)
  "Call FUNCTION with a disposable real repository, parent, job and operation tool."
  (declare (ignore ignored))
  (with-test-configuration (initial root)
    (let* ((source (merge-pathnames "source/" root))
           (configuration (progn (ensure-directories-exist (merge-pathnames "probe" source))
                                 (configuration-copy initial :working-directory source)))
           (orchestrator (task-orchestrator-create))
           (parent (task-tests--primary-agent configuration "worktree-fixture"))
           (definition (task-agent-definition-create :name "isolated" :description "Isolated edits."
                                                     :instructions "Edit only the child workspace."
                                                     :source ':test))
           (job (task-tests--make-job orchestrator :identifier "isolated-task"
                                     :parent-agent parent :definition definition
                                     :item (list :task "Modify one tracked file."
                                                 :isolation (task-worktree-normalize-options (json-object)))))
           (registry (make-instance 'tool-registry))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                   :agent parent :conversation (agent-conversation parent)
                                   :command-authorization-function #'task-worktree-tests--authorize)))
      (unwind-protect
           (progn
             (setf (task-job-command-authorization-function job) #'task-worktree-tests--authorize)
             (task-worktree-register-tool registry orchestrator)
             (task-worktree-tests--git configuration source '("init"))
             (task-worktree-tests--git configuration source '("config" "user.name" "Fixture"))
             (task-worktree-tests--git configuration source '("config" "user.email" "fixture@example.invalid"))
             (task-worktree-tests--write (merge-pathnames "file" source) "baseline")
             (task-worktree-tests--git configuration source '("add" "file"))
             (task-worktree-tests--git configuration source '("commit" "-m" "baseline"))
             (funcall function configuration source parent job
                      (tool-registry-find registry "task" "worktree") context))
        (task-orchestrator-close orchestrator)))))

(-> task-worktree-tests--publish (task-job keyword) list)
(defun task-worktree-tests--publish (job status)
  "Finalize a terminal child and publish its real durable task result."
  (let ((result (task-worktree-finalize-result
                 job (list :status status :id (job-identifier job) :yielded-p (eq status ':success)))))
    (task--write-result-artifact job result)
    result))

(-> task-worktree-tests--call (task-worktree-tool tool-context hash-table) list)
(defun task-worktree-tests--call (tool context arguments)
  "Invoke the actual parent operation and return its structured result."
  (task-tool-result-details (tool-execute tool context arguments)))

(-> test-task-worktree-options () null)
(defun test-task-worktree-options ()
  "Test opt-in isolation and strict explicit source/artifact policies."
  (test-assert (null (task-worktree-normalize-options nil)) "Shared workspace is the default.")
  (test-assert (equal '(:baseline "HEAD" :dirty-policy :reject :artifact-kind "patch")
                      (task-worktree-normalize-options (json-object))) "Default policy rejects dirty source.")
  (test-assert (equal ':ignore (getf (task-worktree-normalize-options
                                     (json-object "dirtyPolicy" "ignore")) :dirty-policy))
               "Only explicit ignore selects the committed baseline.")
  (dolist (object (list (json-object "dirtyPolicy" "stash")
                       (json-object "artifactKind" "snapshot")
                       (json-object "baseline" "")
                       (json-object "path" "/unowned")
                       "true"))
    (test-assert (handler-case (progn (task-worktree-normalize-options object) nil)
                   (task-error () t)) "Malformed or unsupported isolation is rejected."))
  nil)

(-> test-task-worktree-isolation () null)
(defun test-task-worktree-isolation ()
  "Test real isolated checkout identity, dirty policy and owned recovery."
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore parent))
     (task-worktree-tests--write (merge-pathnames "file" source) "parent dirty")
     (test-assert (handler-case (progn (task-worktree-configuration job configuration) nil)
                    (cl-worktree:dirty-workspace () t)) "Dirty source is rejected without mutation.")
     (test-assert (equal "parent dirty" (uiop:read-file-string (merge-pathnames "file" source)))
                  "Parent edits survive rejected admission.")
     (setf (getf (task-job-item job) :isolation)
           (task-worktree-normalize-options (json-object "dirtyPolicy" "ignore")))
     (let* ((child (task-worktree-configuration job configuration))
            (path (config :working-directory child))
            (metadata (getf (task-job-item job) :worktree)))
       (test-assert (not (equal path source)) "Child has a distinct workspace.")
       (test-assert (equal "baseline" (uiop:read-file-string (merge-pathnames "file" path)))
                    "Ignore excludes uncommitted source content.")
       (test-assert (equal metadata (snapshot-read (getf metadata :metadata-path)))
                    "Identity is durable before child execution.")
       (test-assert (= 40 (length (getf metadata :baseline))) "Recorded baseline is a complete commit identity.")
       (let ((discovery (task-worktree-tests--call tool context
                                                 (json-object "action" "discover" "source" (namestring source)))))
         (test-assert (= 1 (length (getf discovery :active))) "Discovery returns the owned checkout."))
       (task-worktree-tests--call tool context
                                 (json-object "action" "cleanup-orphan" "source" (namestring source)
                                              "id" (getf metadata :id)))
       (test-assert (not (probe-file path)) "Orphan cleanup removes only the recorded checkout.")
       (test-assert (equal "parent dirty" (uiop:read-file-string (merge-pathnames "file" source)))
                    "Parent dirty file survives owned cleanup."))))
  nil)

(-> test-task-worktree-artifacts () null)
(defun test-task-worktree-artifacts ()
  "Test terminal patch persistence, explicit application and cleanup authority."
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore parent))
     (let* ((child (task-worktree-configuration job configuration))
            (path (config :working-directory child)))
       (task-worktree-tests--write (merge-pathnames "file" path) "child edits")
       (let* ((result (task-worktree-tests--publish job ':success))
              (id (task-job-execution-identifier job))
              (artifact (task-worktree--read-artifact (getf result :worktree))))
         (test-assert (eq ':success (getf result :status)) "A terminal artifact was extracted.")
         (test-assert (equal '("file") (cl-worktree:artifact-paths artifact)) "Artifact records touched paths.")
         (let ((*task-worktree-artifact-limit* 32)
               (too-large-path (merge-pathnames "too-large.sexp" (task--artifact-root configuration job))))
           (test-assert (handler-case
                           (progn (task-worktree--write-artifact too-large-path artifact) nil)
                         (task-worktree-error () t)) "Incomplete size-limited artifacts are refused.")
           (test-assert (not (probe-file too-large-path)) "Refused artifacts are never published."))
         (test-assert (equal "baseline" (uiop:read-file-string (merge-pathnames "file" source)))
                      "Extraction never integrates into the parent.")
         (test-assert (getf (task-worktree-tests--call tool context (json-object "action" "inspect" "id" id))
                           :artifact-path) "The durable execution ID supports inspection.")
         (test-assert (handler-case
                         (progn (task-worktree-tests--call tool context
                                                          (json-object "action" "cleanup" "id" id)) nil)
                       (cl-worktree:dirty-workspace () t)) "Cleanup refuses uncommitted child edits.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "check" "id" id "target" (namestring source)))
         (test-assert (equal "baseline" (uiop:read-file-string (merge-pathnames "file" source)))
                      "Applicability checks do not modify the parent.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "apply" "id" id "target" (namestring source)))
         (test-assert (equal "child edits" (uiop:read-file-string (merge-pathnames "file" source)))
                      "Explicit integration applies the durable patch.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "cleanup" "id" id "discardDirty" t))
         (test-assert (not (probe-file path)) "Explicit cleanup releases the owned worktree.")
         (test-assert (equal '("file") (cl-worktree:artifact-paths
                                        (task-worktree--read-artifact (getf result :worktree))))
                      "The secured patch is usable after checkout removal.")))))
  nil)

(-> test-task-worktree-authority () null)
(defun test-task-worktree-authority ()
  "Test denied creation, sanitized Git selectors and conversation ownership."
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore parent))
     (setf (task-job-command-authorization-function job)
           (lambda (command directory) (declare (ignore command directory)) ':deny))
     (test-assert (handler-case (progn (task-worktree-configuration job configuration) nil)
                    (cl-worktree:worktree-error () t)
                    (task-worktree-error () t)) "Creation needs command authorization.")
     (setf (task-job-command-authorization-function job) #'task-worktree-tests--authorize)
     (with-test-environment (("GIT_DIR" "/nonexistent-selector")
                            ("GIT_WORK_TREE" "/nonexistent-workspace")
                            ("GIT_INDEX_FILE" "/nonexistent-index")
                            ("GIT_COMMON_DIR" "/nonexistent-common")
                            ("GIT_OBJECT_DIRECTORY" "/nonexistent-objects")
                            ("GIT_ALTERNATE_OBJECT_DIRECTORIES" "/nonexistent-alternates")
                            ("GIT_NAMESPACE" "wrong-namespace")
                            ("GIT_CONFIG_PARAMETERS" "malformed")
                            ("GIT_CONFIG_COUNT" "1")
                            ("GIT_CONFIG_KEY_0" "core.bare")
                            ("GIT_CONFIG_VALUE_0" "true"))
       (let* ((child (task-worktree-configuration job configuration))
              (path (config :working-directory child))
              (metadata (getf (task-job-item job) :worktree))
              (other (cl-worktree:create-worktree
                      (task-worktree--manager configuration #'task-worktree-tests--authorize)
                      :source source :path (merge-pathnames "unrelated/" (test-configuration-root configuration))
                      :baseline "HEAD" :owner "other-conversation/task")))
         (test-assert (probe-file (merge-pathnames "file" path)) "Inherited Git selectors cannot redirect checkout.")
         (test-assert (equal "/nonexistent-selector" (uiop:getenv "GIT_DIR"))
                      "Runner sanitation does not alter process-global environment.")
         (test-assert (handler-case
                         (progn (task-worktree-tests--call
                                 tool context (json-object "action" "cleanup-orphan" "source" (namestring source)
                                                           "id" (cl-worktree:worktree-id other))) nil)
                       (task-worktree-error () t)) "A different conversation's checkout cannot be cleaned.")
         (test-assert (probe-file (cl-worktree:worktree-path other)) "Unrelated worktree is preserved.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "cleanup-orphan" "source" (namestring source)
                                                "id" (getf metadata :id)))
         (cl-worktree:cleanup-worktree
          (task-worktree--manager configuration #'task-worktree-tests--authorize)
          other :owner "other-conversation/task")))))
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (let ((live (task-tests--register-job (task-orchestrator-tool-orchestrator tool)
                                          parent (task-job-definition job) :name "live-isolation")))
       (setf (getf (task-job-item live) :isolation)
             (task-worktree-normalize-options (json-object))
             (task-job-command-authorization-function live) #'task-worktree-tests--authorize)
       (let* ((child (task-worktree-configuration live configuration))
              (metadata (getf (task-job-item live) :worktree)))
         (test-assert (handler-case
                         (progn (task-worktree-tests--call
                                 tool context (json-object "action" "cleanup-orphan" "source" (namestring source)
                                                           "id" (getf metadata :id) "discardDirty" t)) nil)
                       (task-worktree-error () t)) "Owned orphan cleanup refuses a live task's checkout.")
         (test-assert (probe-file (config :working-directory child)) "The live child's workspace is preserved.")
         (task-tests--publish-terminal live ':aborted (list :status ':aborted))
         (task-worktree-tests--call tool context
                                   (json-object "action" "cleanup" "id" (job-identifier live)))))))
  nil)

(-> test-task-worktree-interruption () null)
(defun test-task-worktree-interruption ()
  "Test interrupted terminal states and extraction failure retention."
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore source parent))
     (let* ((child (task-worktree-configuration job configuration))
            (path (config :working-directory child)))
       (task-worktree-tests--write (merge-pathnames "file" path) "interrupted edits")
       (let ((result (task-worktree-tests--publish job ':aborted)))
         (test-assert (eq ':aborted (getf result :status)) "Cancellation is not reclassified as success.")
         (test-assert (getf result :worktree-artifact-path) "Interrupted tracked edits have a durable patch.")
         (test-assert (probe-file path) "Cancellation never cleans the checkout automatically.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "cleanup" "id" (task-job-execution-identifier job)
                                                "discardDirty" t))))))
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore source parent))
     (let* ((child (task-worktree-configuration job configuration))
            (path (config :working-directory child)))
       (task-worktree-tests--write (merge-pathnames "untracked" path) "new file")
       (let ((result (task-worktree-tests--publish job ':success)))
         (test-assert (eq ':failed (getf result :status)) "Missing complete artifact downgrades child success.")
         (test-assert (getf result :worktree-error) "Extraction error is inspectable.")
         (test-assert (probe-file path) "Failed extraction preserves child work.")
         (task-worktree-tests--git configuration path '("add" "untracked"))
         (task-worktree-tests--call tool context
                                   (json-object "action" "extract" "id" (task-job-execution-identifier job)))
         (test-assert (equal '("untracked") (cl-worktree:artifact-paths
                                             (task-worktree--read-artifact (getf result :worktree))))
                      "Parent can explicitly recover the artifact after repair.")
         (task-worktree-tests--call tool context
                                   (json-object "action" "cleanup" "id" (task-job-execution-identifier job)
                                                "discardDirty" t))))))
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore source parent))
     (let ((entered (sb-thread:make-semaphore))
           (release (sb-thread:make-semaphore))
           (paused-p nil)
           (outcome nil)
           (thread nil))
       (setf (task-job-command-authorization-function job)
             (lambda (command directory)
               (declare (ignore directory))
               (when (and (not paused-p) (search "\"worktree\" \"add\"" command))
                 (setf paused-p t)
                 (sb-thread:signal-semaphore entered)
                 (sb-thread:wait-on-semaphore release))
               ':full-access))
       (unwind-protect
            (progn
              (setf thread
                    (sb-thread:make-thread
                     (lambda ()
                       (setf outcome
                             (handler-case
                                 (progn (task-worktree-configuration job configuration) ':completed)
                               (job-aborted () ':aborted)
                               (error (condition) (princ-to-string condition)))))
                     :name "isolated creation cancellation fixture"))
              (test-assert (sb-thread:wait-on-semaphore entered :timeout 30)
                           "Creation reached the ownership-sensitive Git operation.")
              (sb-thread:interrupt-thread
               thread (lambda ()
                        (error 'job-aborted :identifier (job-identifier job)
                                           :reason ':test-cancel :message "Interrupted creation.")))
              (sb-thread:signal-semaphore release)
              (sb-thread:join-thread thread :timeout 30 :default nil)
              (test-assert (eq ':aborted outcome) "Asynchronous cancellation unwinds the child.")
              (let ((metadata (getf (task-job-item job) :worktree)))
                (test-assert metadata "Cancellation cannot interrupt the ownership publication window.")
                (when metadata
                  (test-assert (probe-file (getf metadata :metadata-path))
                               "Interrupted creation has durable task provenance.")))
              (setf (task-job-command-authorization-function job) #'task-worktree-tests--authorize)
              (let ((result (task-worktree-tests--publish job ':aborted)))
                (test-assert (getf result :worktree-artifact-path)
                             "Interrupted creation remains extractable by terminal publication.")
                (task-worktree-tests--call tool context
                                          (json-object "action" "cleanup" "id" (task-job-execution-identifier job)))))
         (sb-thread:signal-semaphore release)
         (when (and thread (sb-thread:thread-alive-p thread))
           (sb-thread:terminate-thread thread)
           (sb-thread:join-thread thread :timeout 30 :default nil))))))
  nil)

(-> test-task-worktree-conflicts () null)
(defun test-task-worktree-conflicts ()
  "Test durable commit-range integration, conflict inspection and explicit abort."
  (task-worktree-tests--fixture
   (lambda (configuration source parent job tool context)
     (declare (ignore parent))
     (setf (getf (task-job-item job) :isolation)
           (task-worktree-normalize-options (json-object "artifactKind" "commit-range")))
     (let* ((child (task-worktree-configuration job configuration))
            (path (config :working-directory child)))
       (task-worktree-tests--write (merge-pathnames "file" path) "child commit")
       (task-worktree-tests--git configuration path '("add" "file"))
       (task-worktree-tests--git configuration path '("commit" "-m" "child"))
       (let ((result (task-worktree-tests--publish job ':success))
             (id (task-job-execution-identifier job)))
         (test-assert (eq ':commit-range (cl-worktree:artifact-kind
                                             (task-worktree--read-artifact (getf result :worktree))))
                      "Selected artifact retains committed engineering changes.")
         (task-worktree-tests--write (merge-pathnames "file" source) "parent commit")
         (task-worktree-tests--git configuration source '("add" "file"))
         (task-worktree-tests--git configuration source '("commit" "-m" "parent"))
         (let ((tip (task-worktree-tests--git configuration source '("rev-parse" "HEAD"))))
           (test-assert (handler-case
                           (progn (task-worktree-tests--call
                                   tool context (json-object "action" "commits" "id" id "target" (namestring source))) nil)
                         (cl-worktree:integration-conflict () t)) "Conflicting commit range is reported.")
           (test-assert (equal '("file") (getf (task-worktree-tests--call
                                               tool context (json-object "action" "conflicts" "target" (namestring source)))
                                             :conflicts)) "Conflict paths are available for inspection.")
           (task-worktree-tests--call tool context (json-object "action" "abort" "target" (namestring source)))
           (test-assert (equal tip (task-worktree-tests--git configuration source '("rev-parse" "HEAD")))
                        "Explicit abort restores pre-integration parent tip."))
         (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id))
         (test-assert (equal "parent commit" (uiop:read-file-string (merge-pathnames "file" source)))
                      "Owned cleanup preserves the parent conflict resolution baseline.")))))
  nil)


(-> test-task-worktree-sandboxed-authorization () null)
(defun test-task-worktree-sandboxed-authorization ()
  "Run the complete authorized lifecycle from ordinary and linked repositories."
  (dolist (linked-p '(nil t))
    (task-worktree-tests--fixture
     (lambda (configuration source parent job tool context)
       (when linked-p
         (let ((linked (merge-pathnames "../linked/" source)))
           (task-worktree-tests--git configuration source
                                    (list "worktree" "add" "--detach" (namestring linked)))
           (setf source (platform-truename *platform* linked)
                 configuration (configuration-copy configuration :working-directory source))))
       (setf (task-job-command-authorization-function job) #'task-worktree-tests--sandboxed
             (getf (task-job-item job) :isolation)
             (task-worktree-normalize-options (json-object "artifactKind" "commit-range")))
       (let* ((child (task-worktree-configuration job configuration))
              (path (config :working-directory child))
              (conversation (conversation-create child))
              (agent (make-instance 'task-child-agent
                                    :configuration child :conversation conversation
                                    :provider (agent-provider parent)
                                    :tool-registry (agent-tool-registry parent)
                                    :worker nil :definition (task-job-definition job)
                                    :identity (task-job-identity job) :depth 1
                                    :completion (make-instance 'task-completion)
                                    :orchestrator (task-job-orchestrator job) :job job))
              (child-context (make-instance 'tool-context :configuration child :worker nil
                                           :agent agent :conversation conversation
                                           :command-authorization-function #'task-worktree-tests--sandboxed))
              (parent-context (make-instance 'tool-context :configuration configuration :worker nil
                                            :agent parent :conversation (agent-conversation parent)
                                            :command-authorization-function #'task-worktree-tests--sandboxed))
              (shell (make-instance 'shell-run-tool :namespace "shell" :name "run"
                                   :description "Run the sandbox fixture."))
              (unrelated (merge-pathnames "unrelated-config" source)))
         (declare (ignore context))
         (test-assert (probe-file (merge-pathnames "file" path)) "The owned checkout is created in the sandbox.")
         (task-worktree-tests--write (merge-pathnames "file" path) "isolated commit")
         (dolist (command '("git add file" "git -c user.name=Fixture -c user.email=fixture@example.invalid commit -m isolated"))
           (let ((result (tool-execute shell child-context (json-object "command" command))))
             (test-assert (and (tool-result-success-p result)
                               (uiop:string-prefix-p (format nil "exit 0~%") (tool-result-content result)))
                          "Ordinary sandboxed child commands can stage and commit.")))
         (let ((result (tool-execute
                        shell child-context
                        (json-object "command" (format nil "git config --file ~S fixture.unrelated denied"
                                                       (namestring unrelated))))))
           (test-assert (not (uiop:string-prefix-p (format nil "exit 0~%") (tool-result-content result)))
                        "The child's sandbox denies unrelated source writes.")
           (test-assert (not (probe-file unrelated)) "Denied commands leave the unrelated path absent."))
         (test-assert
          (handler-case
              (progn
                (funcall (task-worktree--runner child (lambda (command directory)
                                                       (declare (ignore command directory)) ':deny))
                         '("git" "status") :directory path)
                nil)
            (task-worktree-error () t))
          "Denied authorization prevents the Git operation.")
         (let* ((result (task-worktree-tests--publish job ':success))
                (identifier (task-job-execution-identifier job)))
           (test-assert (getf result :worktree-artifact-path) "Sandboxed extraction retains the commit artifact.")
           (task-worktree-tests--call tool parent-context
                                     (json-object "action" "commits" "id" identifier
                                                  "target" (namestring source)))
           (test-assert (equal "isolated commit" (uiop:read-file-string (merge-pathnames "file" source)))
                        "Sandboxed integration applies the isolated commit to the authorized target.")
           (task-worktree-tests--call tool parent-context
                                     (json-object "action" "cleanup" "id" identifier))
           (test-assert (not (probe-file path)) "Sandboxed cleanup removes only the owned checkout."))))))
  nil)
