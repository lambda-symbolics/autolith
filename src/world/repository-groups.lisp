(in-package #:autolith)

;;;; -- Repository Presentation Groups --

(defclass world-repository-group ()
  ((key
    :initarg :key :reader world-repository-group-key
    :documentation "Canonical local Git identity, or the exact non-repository workspace.")
   (directory
    :initarg :directory :reader world-repository-group-directory
    :documentation "Validated originating folder displayed above this group's channels.")
   (channels
    :initarg :channels :accessor world-repository-group-channels
    :documentation "Channels in their supplied presentation order."))
  (:documentation "An ephemeral presentation group, independent of execution directories."))

(-> world-repository--environment () list)
(defun world-repository--environment ()
  "Return a process-local environment without inherited Git repository selectors."
  (loop for name in '("PATH" "HOME" "USERPROFILE" "SYSTEMROOT" "WINDIR"
                      "TMPDIR" "TMP" "TEMP" "LANG" "LC_ALL" "XDG_CONFIG_HOME")
        for value = (uiop:getenv name)
        when value collect (format nil "~A=~A" name value)))

(-> world-repository--git-output (string list) (option string))
(defun world-repository--git-output (directory arguments)
  "Read bounded Git metadata in DIRECTORY, returning NIL on unavailable metadata.

Only fixed metadata queries call this helper. No shell, permission picker, or
process-global environment change participates in presentation discovery."
  (handler-case
      (let ((result
              (run-sandboxed
               "git" (append '("--no-pager" "--no-optional-locks"
                               "-c" "core.fsmonitor=false" "-c" "core.hooksPath="
                               "-c" "gc.auto=0" "-c" "maintenance.auto=false")
                             arguments)
               :policy            (external-sandbox-policy)
               :working-directory (platform-pathname directory)
               :environment       (world-repository--environment)
               :clear-environment-p t :timeout 1
               :output-limit 65536 :error-output-limit 4096)))
        (when (and (eql 0 (sandbox-result-exit-code result))
                   (not (sandbox-result-timed-out-p result))
                   (not (sandbox-result-output-truncated-p result))
                   (not (cl-exec-sandbox:sandbox-result-error-output-truncated-p result)))
          (sandbox-result-output result)))
    (error ()
      nil)))

(-> world-repository--path-output (string list) (option string))
(defun world-repository--path-output (directory arguments)
  "Canonicalize one existing directory returned by a Git path query."
  (let ((output (world-repository--git-output directory arguments)))
    (when (and output (plusp (length output))
               (char= (char output (1- (length output))) #\Newline))
      (handler-case
          (let* ((end  (1- (length output)))
                 (path (platform-pathname (subseq output 0 end))))
            (when (and (uiop:absolute-pathname-p path) (uiop:directory-exists-p path))
              (workspace-directory-name path)))
        (error ()
          nil)))))

(-> world-repository--describe (string) list)
(defun world-repository--describe (directory)
  "Return available local administrative identity and worktree root for DIRECTORY."
  (when (uiop:directory-exists-p (platform-pathname directory))
    (let ((common (world-repository--path-output
                   directory '("rev-parse" "--path-format=absolute" "--git-common-dir"))))
      (when common
        (list :common  common
              :private (world-repository--path-output
                        directory '("rev-parse" "--absolute-git-dir"))
              :root    (world-repository--path-output
                        directory '("rev-parse" "--show-toplevel")))))))

(-> world-repository--main-entry (string) list)
(defun world-repository--main-entry (directory)
  "Read the first NUL-delimited worktree record without unquoting path text."
  (let* ((output (world-repository--git-output directory '("worktree" "list" "--porcelain" "-z")))
         (fields (and output (uiop:split-string output :separator '(#\Null))))
         (end    (position "" fields :test #'string=))
         (first  (first fields)))
    (when (and end first (uiop:string-prefix-p "worktree " first))
      (list :directory (subseq first 9)
            :bare-p    (not (null (member "bare" (subseq fields 0 end) :test #'string=)))))))

(-> world-repository-current-groups (list (option string)) list)
(defun world-repository-current-groups (groups current)
  "Put CURRENT's group first without performing discovery or changing GROUPS."
  (let ((home (find-if (lambda (group)
                         (find current (world-repository-group-channels group)
                               :key #'channel-identifier :test #'equal))
                       groups)))
    (if home
        (cons home (remove home groups))
        groups)))

(-> world-repository-workspace-groups (list (option string)) list)
(defun world-repository-workspace-groups (channels current)
  "Build exact-workspace presentation groups without filesystem discovery."
  (loop for (directory . members) in (world-channel-groups channels current)
        collect (make-instance 'world-repository-group :key       directory
                                                       :directory directory
                                                       :channels  members)))

(-> world-repository-groups (list (option string)) list)
(defun world-repository-groups (channels current)
  "Gather local repository presentation groups without changing channel workspaces.

Resolve each workspace and common identity once in this call. Main worktree
folders must be supported by Git metadata; unresolved origins keep their exact
workspace group. Results are ephemeral and may be reused by pure UI rebuilds.
Every new gather rechecks both successful and unsuccessful discovery."
  (let ((descriptions (make-hash-table :test #'equal))
        (origins      (make-hash-table :test #'equal))
        (groups       nil))
    (labels ((description (directory)
               "Memoize DIRECTORY's metadata only within this gather."
               (multiple-value-bind (value present-p) (gethash directory descriptions)
                 (if present-p
                     value
                     (setf (gethash directory descriptions)
                           (handler-case (world-repository--describe directory)
                             (error () nil))))))

             (origin (directory common)
               "Validate the main worktree record, including bare origins."
               (or (gethash common origins)
                   (multiple-value-bind (value present-p) (gethash common origins)
                     (declare (ignore value))
                     (unless present-p
                       (setf (gethash common origins)
                             (handler-case
                                 (let* ((entry (world-repository--main-entry directory))
                                        (path  (getf entry :directory))
                                        (path  (and path (world-workspace-name path))))
                                   (when path
                                     (if (getf entry :bare-p)
                                         (and (string= path common) path)
                                        (let ((metadata (description path)))
                                           (and (equal common (getf metadata :common))
                                                (equal common (getf metadata :private))
                                                (equal path (getf metadata :root))
                                                path)))))
                               (error () nil))))))))
      ;; An observed main checkout is authoritative even when --separate-git-dir
      ;; makes worktree-list name the administrative directory instead.
      (dolist (channel channels)
        (let* ((metadata (description (channel-workspace channel)))
               (common   (getf metadata :common))
               (root     (getf metadata :root)))
          (when (and common root (equal common (getf metadata :private)))
            (setf (gethash common origins) root))))
      (dolist (channel channels)
        (let* ((directory (channel-workspace channel))
               (common    (getf (description directory) :common))
               (origin    (and common (origin directory common)))
               (key       (if origin (concatenate 'string "git:" common) directory))
               (group     (find key groups :key #'world-repository-group-key :test #'string=)))
          (if group
              (setf (world-repository-group-channels group)
                    (append (world-repository-group-channels group) (list channel)))
              (setf groups
                    (append groups
                            (list (make-instance 'world-repository-group
                                                 :key key :directory (or origin directory)
                                                 :channels (list channel))))))))
      (world-repository-current-groups groups current))))
