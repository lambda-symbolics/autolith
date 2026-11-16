(in-package #:autolith)

;;;; -- Local Repository Group Tests --

(-> world-repository-tests--git (pathname list) string)
(defun world-repository-tests--git (directory arguments)
  "Run a bounded Git fixture command in the disposable test root."
  (let ((result
          (run-sandboxed
           "git" (append '("--no-pager" "--no-optional-locks"
                           "-c" "core.hooksPath=" "-c" "core.fsmonitor=false"
                           "-c" "commit.gpgsign=false" "-c" "user.name=Fixture"
                           "-c" "user.email=fixture@example.invalid")
                         arguments)
           :policy (external-sandbox-policy) :working-directory directory
           :environment (world-repository--environment) :clear-environment-p t
           :timeout 10 :output-limit 65536 :error-output-limit 4096)))
    (test-assert (eql 0 (sandbox-result-exit-code result))
                 (format nil "Git fixture ~S: ~A" arguments (cl-exec-sandbox:sandbox-result-error-output result)))
    (sandbox-result-output result)))

(-> world-repository-tests--channel (string pathname &key (:parent (option string))) channel)
(defun world-repository-tests--channel (identifier directory &key parent)
  "Return a channel executing in DIRECTORY, optionally under PARENT."
  (make-instance 'channel :identifier identifier :owner "user"
                         :workspace (world-workspace-name directory)
                         :parent parent :created-at 1 :updated-at 1))

(-> world-repository-tests--initialize (pathname) pathname)
(defun world-repository-tests--initialize (directory)
  "Create one committed repository owned exclusively by the test fixture."
  (ensure-directories-exist directory)
  (world-repository-tests--git directory '("init" "-q"))
  (world-repository-tests--git directory '("commit" "--allow-empty" "-qm" "fixture"))
  directory)

(-> test-world-repository-groups () null)
(defun test-world-repository-groups ()
  "Exercise real local Git identities, origins, clone boundaries and gather batching."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((main            (world-repository-tests--initialize (merge-pathnames "main/" root)))
           (linked          (merge-pathnames "linked/" root))
           (nested          (merge-pathnames "src/deep/" main))
           (clone-a         (merge-pathnames "clone-a/" root))
           (clone-b         (merge-pathnames "clone-b/" root))
           (separate        (merge-pathnames "separate/" root))
           (admin           (merge-pathnames "arbitrary-admin/" root))
           (separate-linked (merge-pathnames "separate-linked/" root))
           (bare            (merge-pathnames "bare-origin/" root))
           (bare-linked     (merge-pathnames "bare-linked/" root)))
      (ensure-directories-exist nested)
      (world-repository-tests--git main (list "worktree" "add" "-qb" "linked" (namestring linked)))
      (dolist (clone (list clone-a clone-b))
        (world-repository-tests--git root (list "clone" "-q" (namestring main) (namestring clone))))
      (let* ((channels (list (world-repository-tests--channel "child" linked :parent "main")
                             (world-repository-tests--channel "main" main)
                             (world-repository-tests--channel "nested" nested)
                             (world-repository-tests--channel "clone-a" clone-a)
                             (world-repository-tests--channel "clone-b" clone-b)))
             (groups   (world-repository-groups channels "child")))
        (test-assert (= 3 (length groups)) "main, linked and nested folders form one local repository group")
        (test-assert (equal '("child" "main" "nested")
                            (mapcar #'channel-identifier (world-repository-group-channels (first groups))))
                     "grouping preserves supplied member order")
        (test-assert (string= (world-workspace-name main)
                              (world-repository-group-directory (first groups)))
                     "the originating main folder supplies the heading")
        (test-assert (not (string= (world-repository-group-key (second groups))
                                   (world-repository-group-key (third groups))))
                     "independent clones retain distinct local identities")
        (test-assert (string= (world-repository-tests--git clone-a '("remote" "get-url" "origin"))
                              (world-repository-tests--git clone-b '("remote" "get-url" "origin")))
                     "clone separation holds even with identical origin URLs"))
      (let ((describe-function   (symbol-function 'world-repository--describe))
            (main-entry-function (symbol-function 'world-repository--main-entry))
            (descriptions        (make-hash-table :test #'equal))
            (main-entries        0))
        (test-call-with-function-replacements
         (list (list 'world-repository--describe
                     (lambda (directory)
                       (incf (gethash directory descriptions 0))
                       (funcall describe-function directory)))
               (list 'world-repository--main-entry
                     (lambda (directory)
                       (incf main-entries)
                       (funcall main-entry-function directory))))
         (lambda ()
           (let ((groups (world-repository-groups
                          (loop for id in '("one" "two" "three")
                                collect (world-repository-tests--channel id linked)) "one")))
             (test-assert (= 1 (length groups)) "linked-only channels recover the main folder from Git")
             (test-assert (= 1 main-entries) "one common identity requires one worktree-list query")
             (test-assert (loop for count being the hash-values of descriptions always (= 1 count))
                          "each workspace, including the discovered origin, is resolved once per gather")))))
      (ensure-directories-exist admin)
      (world-repository-tests--git root (list "init" "-q" "--separate-git-dir"
                                              (namestring admin) (namestring separate)))
      (world-repository-tests--git separate '("commit" "--allow-empty" "-qm" "fixture"))
      (world-repository-tests--git separate (list "worktree" "add" "-qb" "separate-linked"
                                                  (namestring separate-linked)))
      (let* ((child      (world-repository-tests--channel "separate-child" separate-linked))
             (unresolved (first (world-repository-groups (list child) "separate-child")))
             (observed   (world-repository-groups
                          (list child (world-repository-tests--channel "separate-main" separate))
                          "separate-child")))
        (test-assert (string= (channel-workspace child) (world-repository-group-key unresolved))
                     "an admin-only main record is not mistaken for the originating checkout")
        (test-assert (and (= 1 (length observed))
                          (string= (world-workspace-name separate)
                                   (world-repository-group-directory (first observed))))
                     "an observed authoritative main checkout resolves separate-dir siblings"))
      (world-repository-tests--git root (list "clone" "-q" "--bare" (namestring main) (namestring bare)))
      (world-repository-tests--git bare (list "worktree" "add" "-qb" "bare-linked" (namestring bare-linked)))
      (let ((groups (world-repository-groups
                     (list (world-repository-tests--channel "bare-child" bare-linked)
                           (world-repository-tests--channel "bare" bare)) "bare-child")))
        (test-assert (and (= 1 (length groups))
                          (string= (world-workspace-name bare)
                                   (world-repository-group-directory (first groups))))
                     "a bare repository is its linked worktrees' originating local folder"))))
  nil)

(-> test-world-repository-group-refresh () null)
(defun test-world-repository-group-refresh ()
  "Recheck missing, invalid and changed Git metadata without sticky discovery state."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((main    (merge-pathnames "main/" root))
           (nested  (merge-pathnames "nested/" main))
           (missing (merge-pathnames "missing/" root)))
      (ensure-directories-exist nested)
      (let ((channels (list (world-repository-tests--channel "main" main)
                            (world-repository-tests--channel "nested" nested))))
        (test-assert (= 2 (length (world-repository-groups channels "main")))
                     "non-repositories retain exact workspace groups")
        (world-repository-tests--initialize main)
        (test-assert (= 1 (length (world-repository-groups channels "main")))
                     "the next gather sees a repository created after a negative lookup")
        (with-test-environment (("GIT_DIR" (namestring missing))
                                ("GIT_WORK_TREE" (namestring missing)))
          (test-assert (= 1 (length (world-repository-groups channels "main")))
                       "process-local metadata queries ignore inherited Git selectors")
          (test-assert (string= (uiop:getenv "GIT_DIR") (namestring missing))
                       "discovery never changes the host process environment"))
        (platform-delete-directory-tree *platform* (merge-pathnames ".git/" main) :validate t)
        (test-assert (= 2 (length (world-repository-groups channels "main")))
                     "positive repository identity is rechecked after metadata disappears")
        (with-open-file (stream (merge-pathnames ".git" main) :direction ':output
                                :if-does-not-exist                       ':create)
          (write-line "gitdir: no-such-administration" stream))
        (test-assert (= 2 (length (world-repository-groups channels "main")))
                     "invalid pointers fall back independently for each exact workspace")
        (test-assert (equal (world-workspace-name missing)
                            (world-repository-group-key
                             (first (world-repository-groups
                                     (list (world-repository-tests--channel "missing" missing)) "missing"))))
                     "a channel whose directory disappeared retains its workspace group")
        (test-call-with-function-replacements
         (list (list 'world-repository--git-output
                     (lambda (directory arguments)
                       (declare (ignore directory arguments))
                       nil)))
         (lambda ()
           (test-assert (= 2 (length (world-repository-groups channels "main")))
                        "unavailable metadata does not prevent other groups from rendering"))))))
  nil)
