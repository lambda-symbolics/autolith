(in-package #:autolith)

;;;; -- Isolated Task Worktree Tests --

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

(-> task-worktree-tests--fixture (function) t)
(defun task-worktree-tests--fixture (function)
  "Call FUNCTION with a real repository and an exact parent channel."
  (with-test-configuration (initial root)
    (let* ((source        (merge-pathnames "source/" root))
           (configuration (configuration-copy initial :working-directory source))
           (registry      (task-worktree-register-tool (make-instance 'tool-registry)))
           (parent        (task-tests--primary-agent configuration "Par2134" registry))
           (context       (make-instance 'tool-context :configuration configuration :worker nil
                                                      :agent parent :conversation (agent-conversation parent)
                                                      :command-authorization-function #'task-worktree-tests--authorize)))
      (ensure-directories-exist source)
      (conversation-append-user-message (agent-conversation parent) "Prepare the isolated worktree.")
      (task-worktree-tests--git configuration source '("init"))
      (task-worktree-tests--git configuration source '("config" "user.name" "Fixture"))
      (task-worktree-tests--git configuration source '("config" "user.email" "fixture@example.invalid"))
      (test-write-text (merge-pathnames "file" source) "baseline")
      (task-worktree-tests--git configuration source '("add" "file"))
      (task-worktree-tests--git configuration source '("commit" "-m" "baseline"))
      (funcall function configuration source parent "Chi2134"
               (tool-registry-find registry "child" "worktree") context))))

(-> task-worktree-tests--prepare
    (configuration agent string &key (:options hash-table) (:authorize function)
                                (:register-p boolean))
    (values configuration list))
(defun task-worktree-tests--prepare (configuration parent identifier
                                     &key (options (json-object))
                                      (authorize #'task-worktree-tests--authorize) (register-p t))
  "Prepare a checkout and optionally register its idle child without sending work."
  (multiple-value-bind (child metadata)
      (task-worktree-configuration (conversation-identifier (agent-conversation parent))
                                   identifier configuration
                                   :isolation (task-worktree-normalize-options options)
                                   :authorize authorize)
    (when register-p
      (world-child-create parent :identifier identifier :configuration child))
    (values child metadata)))

(-> task-worktree-tests--call (task-worktree-tool tool-context hash-table) list)
(defun task-worktree-tests--call (tool context arguments)
  "Invoke the actual parent operation and return its structured result."
  (task-tool-result-details (tool-execute tool context arguments)))

(-> test-task-worktree-xdg-git-configuration () null)
(defun test-task-worktree-xdg-git-configuration ()
  "Read real global Git configuration with narrow, read-only sandbox scopes."
  (with-test-configuration (initial root)
    (let* ((source        (merge-pathnames "repository/" root))
           (home          (merge-pathnames "empty-home/" root))
           (xdg           (merge-pathnames "xdg config/" root))
           (git-root      (merge-pathnames "git/" xdg))
           (configuration (configuration-copy initial :working-directory source))
           (sandbox       (fdefinition 'platform-call-with-command-sandbox)))
      (ensure-directories-exist source)
      (ensure-directories-exist home)
      (test-write-text
       (merge-pathnames "git/config" xdg)
       (format nil "[user]~%name = XDG Identity~%email = xdg@example.invalid~%"))
      (test-write-text (merge-pathnames "git/ignore" xdg)
                       (format nil "ignored.tmp~%"))
      (test-write-text (merge-pathnames "git/attributes" xdg)
                       (format nil "*.fixture fixture-attribute=xdg~%"))
      (test-write-text
       (merge-pathnames ".config/git/config" home)
       (format nil "[user]~%name = Default Identity~%"))
      (test-call-with-function-replacements
       (list (list 'platform-call-with-command-sandbox
                   (lambda (platform workspace function &key writable-roots read-roots)
                     (test-assert (equal (list git-root) read-roots)
                                  "Git receives only its XDG subtree as an extra read scope")
                     (funcall sandbox platform workspace function
                              :writable-roots writable-roots :read-roots read-roots))))
       (lambda ()
         (with-test-environment (("HOME" (uiop:native-namestring home))
                                 ("XDG_CONFIG_HOME" (uiop:native-namestring xdg)))
           (task-worktree-tests--git configuration source '("init"))
           (let ((authorizers (list #'task-worktree-tests--authorize)))
             (if (application--command-sandbox-available-p)
                 (push #'task-worktree-tests--sandboxed authorizers)
                 (test-withheld ':command-sandbox "sandboxed XDG Git configuration"))
             (dolist (authorize authorizers)
               (flet ((git (&rest arguments)
                        "Read Git output through one authorization policy."
                        (string-trim '(#\Return #\Newline)
                                     (task-worktree-tests--git
                                      configuration source arguments :authorize authorize))))
                 (test-assert (string= "XDG Identity" (git "config" "--global" "--get" "user.name"))
                              "Git reads global identity from the custom XDG config root")
                 (test-assert (search "XDG Identity <xdg@example.invalid>" (git "var" "GIT_AUTHOR_IDENT"))
                              "Git uses the XDG identity for authorship")
                 (test-assert (string= "ignored.tmp" (git "check-ignore" "--no-index" "--" "ignored.tmp"))
                              "Git reads the default global XDG ignore file")
                 (test-assert (string= "sample.fixture: fixture-attribute: xdg"
                                      (git "check-attr" "fixture-attribute" "--" "sample.fixture"))
                              "Git reads the default global XDG attributes file")))
             (when (member #'task-worktree-tests--sandboxed authorizers)
               (multiple-value-bind (output error-output status)
                   (funcall (task-worktree--runner configuration #'task-worktree-tests--sandboxed)
                            (list "git" "config" "--file"
                                  (uiop:native-namestring (merge-pathnames "config" git-root))
                                  "user.name" "Forbidden")
                            :directory source)
                 (declare (ignore output error-output))
                 (test-assert (not (zerop status))
                              "The XDG Git read scope does not permit configuration writes"))
               (test-assert
                (string= "XDG Identity"
                         (string-trim '(#\Return #\Newline)
                                      (task-worktree-tests--git
                                       configuration source '("config" "--global" "--get" "user.name"))))
                "The sandbox cannot change the host Git identity")))
           (dolist (value '(nil "" "relative" "relative["))
             (with-test-environment (("XDG_CONFIG_HOME" value))
               (test-assert (null (task-worktree--git-read-roots))
                            "Invalid XDG config roots grant no read scopes")
               (test-assert
                (string= "Default Identity"
                         (string-trim '(#\Return #\Newline)
                                      (task-worktree-tests--git
                                       configuration source '("config" "--global" "--get" "user.name"))))
                "Invalid XDG config roots are omitted so Git uses the home default"))))))))
  nil)

(-> test-task-worktree-options () null)
(defun test-task-worktree-options ()
  "Test opt-in isolation and strict explicit source/artifact policies."
  (test-assert (null (task-worktree-normalize-options nil)) "Shared workspace is the default.")
  (test-assert (equal '(:baseline "HEAD" :dirty-policy :reject :artifact-kind :patch)
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
  "Check dirty policy, durable channel ownership and unpublished-child cleanup."
  (task-worktree-tests--fixture
   (lambda (configuration source parent id tool context)
     (test-write-text (merge-pathnames "file" source) "parent dirty")
     (test-assert (handler-case
                      (progn (task-worktree-tests--prepare configuration parent id :register-p nil) nil)
                    (cl-worktree:dirty-workspace () t))
                  "Default dirty policy refuses checkout without mutation.")
     (multiple-value-bind (child metadata)
         (task-worktree-tests--prepare configuration parent id :register-p nil
                                             :options                      (json-object "dirtyPolicy" "ignore"))
       (let ((parent-id (conversation-identifier (agent-conversation parent)))
             (path      (config :working-directory child)))
         (test-assert (not (equal path source)) "Child has a distinct workspace.")
         (test-assert (equal metadata (task-worktree--read-metadata configuration parent-id id))
                      "The exact parent and child identify the durable sidecar.")
         (test-assert (handler-case
                          (null (task-worktree--read-metadata configuration "Par2135" id))
                        (task-worktree-error () t))
                      "A different parent cannot read the sidecar.")
         (test-assert (= 1 (length (getf (task-worktree-tests--call
                                          tool context (json-object "action" "discover" "source" (namestring source)))
                                         :active)))
                      "Discovery returns the caller's checkout.")
         (test-assert (handler-case
                          (progn (task-worktree-discard configuration id :parent-identifier parent-id
                                                                         :authorize         (constantly ':deny)) nil)
                        (task-worktree-error () t)
                        (cl-worktree:worktree-error () t))
                      "An unauthorized rollback cannot remove the checkout.")
         (test-assert (probe-file path) "Denied rollback preserves the checkout.")
         (let ((cleanup (fdefinition 'cl-worktree:cleanup-worktree)))
           (test-call-with-function-replacements
            (list (list 'cl-worktree:cleanup-worktree
                        (lambda (&rest arguments)
                          (when (apply cleanup arguments)
                            (error 'job-aborted :message "Interrupted after Git removal.")))))
            (lambda ()
              (test-assert (handler-case
                               (progn (task-worktree-discard configuration id :parent-identifier parent-id) nil)
                             (job-aborted () t))
                           "The simulated interruption follows successful Git removal."))))
         (test-assert (not (probe-file path)) "Git removed the checkout before interruption.")
         (test-assert (probe-file (getf metadata :metadata-path)) "The interrupted discard retains its receipt.")
         (task-worktree-discard configuration id :parent-identifier parent-id)
         (test-assert (not (probe-file (getf metadata :metadata-path))) "Verified retry retires the receipt.")
         (test-assert (equal "parent dirty" (uiop:read-file-string (merge-pathnames "file" source)))
                      "Discard preserves the source changes.")))))
  nil)

(-> test-task-worktree-artifacts () null)
(defun test-task-worktree-artifacts ()
  "Extract, check, apply and retain an explicitly secured patch."
  (task-worktree-tests--fixture
   (lambda (configuration source parent id tool context)
     (multiple-value-bind (child metadata) (task-worktree-tests--prepare configuration parent id)
       (let ((path (config :working-directory child)))
         (test-write-text (merge-pathnames "file" path) "child edits")
         (let* ((result         (task-worktree-tests--call tool context (json-object "action" "extract" "id" id)))
                (artifact       (task-worktree--read-artifact metadata))
                (too-large-path (merge-pathnames "too-large.sexp" (pathname (getf result :artifact-path)))))
           (let ((*task-worktree-artifact-limit* 32))
             (test-assert (handler-case
                              (progn (task-worktree--write-artifact too-large-path artifact) nil)
                            (task-worktree-error () t))
                          "Incomplete size-limited artifacts are refused.")
             (test-assert (not (probe-file too-large-path)) "Refused artifacts are never published."))
           (test-assert (handler-case
                            (progn (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id)) nil)
                          (cl-worktree:dirty-workspace () t))
                        "Cleanup refuses uncommitted child edits.")
           (task-worktree-tests--call tool context (json-object "action" "check" "id" id "target" (namestring source)))
           (test-assert (equal "baseline" (uiop:read-file-string (merge-pathnames "file" source)))
                        "Extraction and applicability checks do not modify the parent.")
           (task-worktree-tests--call tool context (json-object "action" "apply" "id" id "target" (namestring source)))
           (test-assert (equal "child edits" (uiop:read-file-string (merge-pathnames "file" source)))
                        "Explicit integration applies the durable patch.")
           (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id "discardDirty" t))
           (test-assert (not (probe-file path)) "Cleanup releases the owned checkout.")
           (test-assert (equal '("file") (cl-worktree:artifact-paths (task-worktree--read-artifact metadata)))
                        "The secured patch is usable after checkout removal.")
           (test-assert (= 1 (channel-remove configuration (list id))) "Channel removal finishes after explicit cleanup.")
           (test-assert (not (probe-file (getf metadata :metadata-path))) "Channel removal retires its worktree sidecar."))))))
  nil)

(-> test-task-worktree-authority () null)
(defun test-task-worktree-authority ()
  "Exercise denied Git creation, selector sanitation and exact child ownership."
  (task-worktree-tests--fixture
   (lambda (configuration source parent id tool context)
     (test-assert (handler-case
                      (progn (task-worktree-tests--prepare configuration parent id
                                                          :authorize (constantly ':deny)) nil)
                    (cl-worktree:worktree-error () t)
                    (task-worktree-error () t))
                  "Checkout creation requires command authorization.")
     (with-test-environment (("GIT_DIR" "/nonexistent-selector")
                             ("GIT_WORK_TREE" "/nonexistent-workspace")
                             ("GIT_INDEX_FILE" "/nonexistent-index")
                             ("GIT_COMMON_DIR" "/nonexistent-common")
                             ("GIT_OBJECT_DIRECTORY" "/nonexistent-objects")
                             ("GIT_ALTERNATE_OBJECT_DIRECTORIES" "/nonexistent-alternates")
                             ("GIT_NAMESPACE" "wrong-namespace")
                             ("GIT_CONFIG_PARAMETERS" "malformed")
                             ("GIT_CONFIG_COUNT" "1") ("GIT_CONFIG_KEY_0" "core.bare")
                             ("GIT_CONFIG_VALUE_0" "true"))
       (multiple-value-bind (child metadata) (task-worktree-tests--prepare configuration parent id)
         (let* ((path    (config :working-directory child))
                (manager (task-worktree--manager configuration #'task-worktree-tests--authorize))
                (other   (cl-worktree:create-worktree
                          manager :source source :baseline "HEAD" :owner "other-conversation/task"
                                  :path (merge-pathnames "unrelated/" (test-configuration-root configuration)))))
           (test-assert (probe-file (merge-pathnames "file" path)) "Inherited Git selectors cannot redirect checkout.")
           (test-assert (equal "/nonexistent-selector" (uiop/os:getenv "GIT_DIR"))
                        "Runner sanitation does not alter the process environment.")
           (dolist (worktree-id (list (getf metadata :id) (cl-worktree:worktree-id other)))
             (test-assert (handler-case
                              (progn (task-worktree-tests--call
                                      tool context (json-object "action" "cleanup-orphan" "source" (namestring source)
                                                                 "id" worktree-id "discardDirty" t)) nil)
                            (task-worktree-error () t))
                          "Orphan cleanup refuses existing children and foreign checkouts."))
           (let ((lease (conversation-lease-acquire configuration id)))
             (unwind-protect
                  (test-assert (handler-case
                                   (progn (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id)) nil)
                                 (task-error () t))
                               "Cleanup cannot race the child's active lease.")
               (conversation-lease-release lease)))
           (test-assert (and (probe-file path) (probe-file (cl-worktree:worktree-path other)))
                        "Refused cleanup preserves both checkouts.")
           (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id))
           (cl-worktree:cleanup-worktree manager other :owner "other-conversation/task"))))))
  nil)

(-> test-task-worktree-interruption () null)
(defun test-task-worktree-interruption ()
  "Preserve edits after failed extraction and scope legacy checkout deletion."
  (task-worktree-tests--fixture
   (lambda (configuration source parent id tool context)
     (multiple-value-bind (child metadata) (task-worktree-tests--prepare configuration parent id)
       (let* ((path             (config :working-directory child))
              (legacy-path      (merge-pathnames "task-worktrees/checkouts/legacy-task/"
                                                 (config :data-root configuration)))
              (manager          (task-worktree--manager configuration #'task-worktree-tests--authorize))
              (legacy           (cl-worktree:create-worktree manager :source source :path legacy-path
                                                                    :owner "legacy-owner" :baseline "HEAD"))
              (legacy-directory (merge-pathnames "legacy-result/" (config :data-root configuration)))
              (record           (merge-pathnames "worktree.sexp" legacy-directory)))
         (ensure-directories-exist record)
         (snapshot-write record (task-worktree--metadata legacy ':patch (namestring record)))
         (task-worktree-discard configuration "legacy-task"
                                :pathname (merge-pathnames "conversation.sexp" legacy-directory))
         (test-assert (not (probe-file legacy-path)) "Discard removes the verified legacy checkout.")
         (test-assert (probe-file (merge-pathnames "file" path)) "Legacy discard preserves the sibling checkout.")
         (test-assert (handler-case
                          (progn (task-worktree--cleanup-root configuration
                                  (merge-pathnames "task-worktrees/checkouts/" (config :data-root configuration))) nil)
                        (task-worktree-error () t))
                      "The shared checkouts root cannot become a deletion scope.")
         (test-write-text (merge-pathnames "untracked" path) "new file")
         (test-assert (handler-case
                          (progn (task-worktree-tests--call tool context (json-object "action" "extract" "id" id)) nil)
                        (cl-worktree:worktree-error () t))
                      "Extraction refuses an incomplete patch.")
         (test-assert (probe-file (merge-pathnames "untracked" path)) "Failed extraction preserves child work.")
         (task-worktree-tests--git configuration path '("add" "untracked"))
         (task-worktree-tests--call tool context (json-object "action" "extract" "id" id))
         (test-assert (equal '("untracked") (cl-worktree:artifact-paths (task-worktree--read-artifact metadata)))
                      "The parent can extract again after staging the change.")
         (task-worktree-tests--call tool context (json-object "action" "cleanup" "id" id "discardDirty" t))))))
  (with-test-fixture (':posix-shell "Cancel a running Git subprocess")
    (with-test-configuration (configuration root)
      (dolist (authorize (list #'task-worktree-tests--authorize #'task-worktree-tests--sandboxed))
        (let* ((start  (get-internal-real-time))
               (runner (task-worktree--runner
                        configuration authorize
                        :cancel-function (lambda ()
                                           (> (/ (- (get-internal-real-time) start)
                                                 internal-time-units-per-second)
                                              0.2)))))
          (test-assert
           (handler-case
               (progn (funcall runner '("git" "-c" "alias.wait=!sleep 30" "wait")
                               :directory root) nil)
             (task-worktree-error () t))
           "The Git runner observes cancellation while the subprocess is running.")
          (test-assert (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 3)
                       "Cancellation does not wait for the command deadline.")))))
  nil)

(-> test-task-worktree-conflicts () null)
(defun test-task-worktree-conflicts ()
  "Test durable commit-range integration, conflict inspection and explicit abort."
  (task-worktree-tests--fixture
   (lambda (configuration source parent id tool context)
     (multiple-value-bind (child metadata)
         (task-worktree-tests--prepare configuration parent id :options (json-object "artifactKind" "commit-range"))
       (let ((path (config :working-directory child)))
         (test-write-text (merge-pathnames "file" path) "child commit")
         (task-worktree-tests--git configuration path '("add" "file"))
         (task-worktree-tests--git configuration path '("commit" "-m" "child"))
         (let ((result (task-worktree-tests--call tool context (json-object "action" "extract" "id" id))))
           (test-assert (getf result :artifact-path) "Commit-range extraction succeeds."))
         (test-assert (eq ':commit-range (cl-worktree:artifact-kind (task-worktree--read-artifact metadata)))
                      "Selected artifact retains committed engineering changes.")
         (test-write-text (merge-pathnames "file" source) "parent commit")
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
  (unless (application--command-sandbox-available-p)
    (test-withheld ':command-sandbox "sandboxed isolated worktree authorization")
    (return-from test-task-worktree-sandboxed-authorization nil))
  (dolist (linked-p '(nil t))
    (task-worktree-tests--fixture
     (lambda (configuration source parent id tool context)
       (declare (ignore context))
       (when linked-p
         (let ((linked (merge-pathnames "../linked/" source)))
           (task-worktree-tests--git configuration source
                                     (list "worktree" "add" "--detach"
                                           (namestring linked)))
           (setf source (platform-truename *platform* linked)
                 configuration
                   (configuration-copy configuration :working-directory source)
                 (agent-configuration parent) configuration)))
       (let* ((child        (task-worktree-tests--prepare configuration parent id
                                                         :options   (json-object "artifactKind" "commit-range")
                                                         :authorize #'task-worktree-tests--sandboxed))
              (path         (config :working-directory child))
              (conversation (conversation-load-by-id child id))
              (agent        (agent-create :configuration child :conversation conversation
                                          :provider (agent-provider parent)
                                          :tool-registry (agent-tool-registry parent) :worker nil))
              (child-context
               (make-instance 'tool-context :configuration child :worker nil :agent agent
                              :conversation conversation :command-authorization-function
                              #'task-worktree-tests--sandboxed))
              (parent-context
               (make-instance 'tool-context :configuration configuration :worker nil
                              :agent parent :conversation (agent-conversation parent)
                              :command-authorization-function
                              #'task-worktree-tests--sandboxed))
              (shell
               (make-instance 'shell-run-tool :namespace "shell" :name "run" :description
                              "Run the sandbox fixture."))
              (unrelated    (merge-pathnames "unrelated-config" source)))
         (test-assert (probe-file (merge-pathnames "file" path))
                      "The owned checkout is created in the sandbox.")
         (test-write-text (merge-pathnames "file" path) "isolated commit")
         (dolist
             (command
              '("git add file"
                "git -c user.name=Fixture -c user.email=fixture@example.invalid commit -m isolated"))
           (let ((result
                  (tool-execute shell child-context (json-object "command" command))))
             (test-assert
              (and (tool-result-success-p result)
                   (uiop/utility:string-prefix-p (format nil "exit 0~%")
                                                 (tool-result-content result)))
              "Ordinary sandboxed child commands can stage and commit.")))
         (let ((result
                (tool-execute shell child-context
                              (json-object "command"
                                           (format nil
                                                   "git config --file ~S fixture.unrelated denied"
                                                   (namestring unrelated))))))
           (test-assert
            (not
             (uiop/utility:string-prefix-p (format nil "exit 0~%")
                                           (tool-result-content result)))
            "The child's sandbox denies unrelated source writes.")
           (test-assert (not (probe-file unrelated))
                        "Denied commands leave the unrelated path absent."))
         (test-assert
          (handler-case
           (progn
            (funcall
             (task-worktree--runner child
                                    (lambda (command directory)
                                      (declare (ignore command directory))
                                      ':deny))
             '("git" "status") :directory path)
            nil)
           (task-worktree-error nil t))
          "Denied authorization prevents the Git operation.")
         (let ((result (task-worktree-tests--call tool parent-context (json-object "action" "extract" "id" id))))
           (test-assert (getf result :artifact-path) "Sandboxed extraction retains the commit artifact.")
           (task-worktree-tests--call tool parent-context
                                      (json-object "action" "commits" "id" id
                                                   "target" (namestring source)))
           (test-assert
            (equal "isolated commit"
                   (uiop/stream:read-file-string (merge-pathnames "file" source)))
            "Sandboxed integration applies the isolated commit to the authorized target.")
           (task-worktree-tests--call tool parent-context
                                      (json-object "action" "cleanup" "id" id))
           (test-assert (not (probe-file path))
                        "Sandboxed cleanup removes only the owned checkout."))))))
  nil)

(-> test-task-worktree-native-cleanup-ownership () null)
(defun test-task-worktree-native-cleanup-ownership ()
  "Refuse worktree recovery while native or capture cleanup still owns Git."
  (dolist (mode '(:native :retry :readers))
    (task-worktree-tests--fixture
     (lambda (configuration source parent id tool context)
       (declare (ignore tool context))
       (let* ((release   (sb-thread:make-semaphore))
              (owner     (make-thread (lambda () (sb-thread:wait-on-semaphore release) t)
                                      :name "held Git cleanup owner"))
              (condition (make-condition
                          'cl-exec-sandbox:sandbox-process-cleanup-error
                          :message "Held native cleanup" :command '("git" "worktree" "add")
                          :process nil :alive-p (not (eq mode ':readers)) :termination-timeout 0.01
                          :cleanup-thread  (when (eq mode ':native) owner)
                          :cleanup-retry   (lambda () (when (eq mode ':retry) owner))
                          :cleanup-readers (when (eq mode ':readers) (list owner))))
              (parent-id (conversation-identifier (agent-conversation parent)))
              (runner    (fdefinition 'run-sandboxed)))
         (unwind-protect
              (progn
                (test-call-with-function-replacements
                 (list (list 'run-sandboxed
                             (lambda (program arguments &rest options)
                               (multiple-value-prog1 (apply runner program arguments options)
                                 (when (and (member "worktree" arguments :test #'equal)
                                            (member "add" arguments :test #'equal))
                                   (error condition))))))
                 (lambda ()
                   (test-assert (handler-case
                                    (progn (task-worktree-tests--prepare configuration parent id :register-p nil) nil)
                                  (cl-worktree:worktree-error () t))
                                "Git reports incomplete cleanup after checkout creation.")))
                (let* ((metadata (task-worktree--read-metadata configuration parent-id id))
                       (path     (getf metadata :path)))
                  (test-assert (task-worktree-cleanup-snapshot :source source)
                               "Cleanup retains the outstanding Git owner.")
                  (test-assert (handler-case
                                   (progn (task-worktree-discard configuration id :parent-identifier parent-id) nil)
                                 (task-worktree-error () t)
                                 (cl-worktree:worktree-error () t))
                               "Discard cannot race an outstanding Git owner.")
                  (test-assert (probe-file path) "Refusal preserves the checkout.")
                  (sb-thread:signal-semaphore release)
                  (join-thread owner)
                  (test-assert (not (task-worktree-cleanup-snapshot :source source))
                               "Inspection retires settled cleanup.")
                  (task-worktree-discard configuration id :parent-identifier parent-id)
                  (test-assert (not (probe-file path)) "Discard succeeds after cleanup settles.")
                  (test-assert (probe-file (merge-pathnames "file" source)) "Discard preserves the repository.")))
           (sb-thread:signal-semaphore release)
           (when (thread-alive-p owner) (join-thread owner))
           (task-worktree-cleanup-snapshot))))))
  nil)
