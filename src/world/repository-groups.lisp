(in-package #:autolith)

;;;; -- Repository Presentation Groups --

(defclass world-repository-group ()
  ((key
    :initarg :key :reader world-repository-group-key
    :documentation "Canonical local Git identity, or the exact non-repository workspace.")
   (directory
    :initarg :directory :reader world-repository-group-directory
    :documentation "Validated originating folder displayed above this group's channels.")
   (origin-observed-p
    :initarg :origin-observed-p :initform nil :reader world-repository-group-origin-observed-p
    :type          boolean
    :documentation "Whether fresh verified Git metadata established this group's originating folder.")
   (channels
    :initarg :channels :accessor world-repository-group-channels
    :documentation "Channels in their supplied presentation order."))
  (:documentation "An ephemeral presentation group, independent of execution directories."))

(-> world-repository--git-output (string list) (option string))
(defun world-repository--git-output (directory arguments)
  "Read bounded Git metadata in DIRECTORY, returning NIL on unavailable metadata.

Only fixed metadata queries call this helper. No shell, permission picker, or
process-global environment change participates in presentation discovery."
  (handler-case
      (destructuring-bind (program &rest options) (cl-worktree:git-read-argv arguments)
        (let ((result
                (run-sandboxed
                 program options
                 :policy            (external-sandbox-policy)
                 :working-directory (platform-pathname directory)
                 :environment       (git-process-environment)
                 :clear-environment-p t :timeout 1
                 :output-limit 65536 :error-output-limit 4096)))
          (when (and (eql 0 (sandbox-result-exit-code result))
                     (not (sandbox-result-timed-out-p result))
                     (not (sandbox-result-output-truncated-p result))
                     (not (cl-exec-sandbox:sandbox-result-error-output-truncated-p result)))
            (sandbox-result-output result))))
    (error ()
      nil)))

(-> world-repository--canonical-directory (string) (option string))
(defun world-repository--canonical-directory (directory)
  "Return the platform's canonical name for an existing absolute directory."
  (handler-case
      (let ((path (platform-pathname directory)))
        (when (and (uiop:absolute-pathname-p path) (uiop:directory-exists-p path))
          (workspace-directory-name path)))
    (error () nil)))

(-> world-repository--describe (string) list)
(defun world-repository--describe (directory)
  "Describe DIRECTORY using the bounded runner and platform path adapter."
  (cl-worktree:describe-repository directory #'world-repository--git-output
                                   :canonicalize #'world-repository--canonical-directory))

(-> world-repository--origin (string list &key (:describe function)) (option string))
(defun world-repository--origin (directory metadata &key (describe #'world-repository--describe))
  "Resolve a verified origin, allowing the gather to reuse cached descriptions."
  (cl-worktree:repository-origin directory metadata #'world-repository--git-output
                                 :canonicalize #'world-repository--canonical-directory
                                 :describe     describe))

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

(-> world-repository-make-memo () list)
(defun world-repository-make-memo ()
  "Return an empty memo of Git metadata by workspace and of origins by repository."
  (list (make-hash-table :test #'equal) (make-hash-table :test #'equal)))

(-> world-repository-groups (list (option string) &key (:memo list)) list)
(defun world-repository-groups (channels current &key (memo (world-repository-make-memo)))
  "Gather local repository presentation groups without changing channel workspaces.

Resolve each workspace and common identity once per MEMO, which a caller that
gathers again and again keeps for a while instead of asking Git each time.
Prefer fresh verified Git metadata, then the channel's durable originating
repository. Unattributed channels keep their exact workspace group. This
function never writes the world store or changes the supplied channel objects."
  (let ((descriptions (first memo))
        (origins      (second memo))
        (groups       nil))
    (labels ((description (directory)
               "Memoize DIRECTORY's metadata for as long as MEMO lives."
               (multiple-value-bind (value present-p) (gethash directory descriptions)
                 (if present-p
                     value
                     (setf (gethash directory descriptions)
                           (handler-case (world-repository--describe directory)
                             (error () nil))))))

             (origin (directory metadata)
               "Memoize origin discovery, including failed observations."
               (let ((common (getf metadata :common)))
                 (multiple-value-bind (value present-p) (gethash common origins)
                   (if present-p
                       value
                       (setf (gethash common origins)
                             (handler-case
                                 (world-repository--origin directory metadata :describe #'description)
                               (error () nil))))))))
      ;; An observed main checkout is authoritative even when --separate-git-dir
      ;; makes worktree-list name the administrative directory instead.
      (dolist (channel channels)
        (let* ((metadata (description (channel-workspace channel)))
               (root     (cl-worktree:repository-primary-checkout metadata)))
          (when root
            (setf (gethash (getf metadata :common) origins) root))))
      ;; Complete the verified origin memo before saved members choose headings.
      (dolist (channel channels)
        (let* ((directory (channel-workspace channel))
               (metadata  (description directory)))
          (when metadata (origin directory metadata))))
      (dolist (channel channels)
        (let* ((directory       (channel-workspace channel))
               (common          (getf (description directory) :common))
               (key             (cond
                                  ((and common (gethash common origins))
                                   (concatenate 'string "git:" common))
                                  ((channel-repository-key channel)
                                   (channel-repository-key channel))
                                  (t
                                   directory)))
               (observed-origin (and (uiop:string-prefix-p "git:" key)
                                     (gethash (subseq key 4) origins)))
               (origin          (or observed-origin (channel-repository-directory channel) directory))
               (group           (find key groups :key #'world-repository-group-key :test #'string=)))
          (if group
              (setf (world-repository-group-channels group)
                    (append (world-repository-group-channels group) (list channel)))
              (setf groups
                    (append groups
                            (list (make-instance 'world-repository-group
                                                 :key key :directory origin
                                                 :origin-observed-p (not (null observed-origin))
                                                 :channels          (list channel))))))))
      (world-repository-current-groups groups current))))


;;;; -- Durable Repository Observations --

(-> world-repository-resolve-directory (string) list)
(defun world-repository-resolve-directory (directory)
  "Resolve DIRECTORY to a verified Git key and originating folder, or NIL."
  (handler-case
      (let* ((directory (world-workspace-name directory))
             (metadata  (world-repository--describe directory))
             (origin    (world-repository--origin directory metadata)))
        (when origin
          (list :key (concatenate 'string "git:" (getf metadata :common)) :directory origin)))
    (error () nil)))

(defparameter *world-repository-artifact-limit* 4096
  "Maximum task metadata files considered in one legacy attribution pass.")

(defparameter *world-repository-artifact-characters* 65536
  "Maximum characters accepted from one task worktree metadata file.")

(defparameter *world-repository-recovery-depth* 16
  "Maximum exact historical source links followed for a removed checkout.")

(-> world-repository--read-metadata (pathname) list)
(defun world-repository--read-metadata (pathname)
  "Read one bounded portable metadata form, rejecting trailing forms and reader evaluation."
  (handler-case
      (with-open-file (stream pathname :external-format ':utf-8)
        (let* ((buffer (make-string (1+ *world-repository-artifact-characters*)))
               (count  (read-sequence buffer stream)))
          (when (<= count *world-repository-artifact-characters*)
            (let* ((grammar (sexp-config:make-source-grammar
                             :label                        "repository metadata"
                             :line-comments-permitted-p    nil
                             ;; Readable snapshots may print base strings in array notation.
                             :readable-strings-permitted-p t
                             :whitespace-characters        '(#\Space #\Tab #\Newline #\Return)))
                   (form    (sexp-config:read-source (subseq buffer 0 count) grammar)))
              (when (sexp-store:plist-schema-p form
                                              :required-keys  '(:version :owner :path :source)
                                              :keyword-keys-p t)
                form)))))
    (error () nil)))

(-> world-repository--task-sources (configuration) hash-table)
(defun world-repository--task-sources (configuration)
  "Index exact persisted task checkout/source pairs, without consulting task residency.

Conflicting source records make a checkout ambiguous. A saturated scan supplies
no evidence rather than assuming uniqueness from a truncated inventory."
  (let ((sources (make-hash-table :test #'equal)))
    (handler-case
        (let* ((root    (merge-pathnames "tasks/" (config :data-root configuration)))
               (pattern (merge-pathnames
                         (make-pathname :directory '(:relative :wild :wild)
                                        :name "worktree" :type "sexp") root))
               (files   (directory pattern)))
          (when (<= (length files) *world-repository-artifact-limit*)
            (dolist (file files)
              (let* ((metadata (world-repository--read-metadata file))
                     (path     (getf metadata :path))
                     (source   (getf metadata :source)))
                (when (and (eql (getf metadata :version) 1)
                           (non-empty-string-p (getf metadata :owner))
                           (non-empty-string-p path) (non-empty-string-p source)
                           (uiop:absolute-pathname-p (platform-pathname path))
                           (uiop:absolute-pathname-p (platform-pathname source)))
                  (let* ((path     (world-workspace-name path))
                         (source   (world-workspace-name source))
                         (previous (gethash path sources)))
                    (setf (gethash path sources)
                          (if (or (eq previous ':ambiguous)
                                  (and previous (not (equal previous source))))
                              ':ambiguous source))))))))
      (error () (clrhash sources)))
    sources))

(-> world-repository--recover (string hash-table list) list)
(defun world-repository--recover (directory sources channels)
  "Follow exact task source evidence for DIRECTORY, refusing cycles or conflicting origins."
  (let ((seen (make-hash-table :test #'equal)))
    (loop for source = (gethash directory sources)
          repeat *world-repository-recovery-depth*
          do (when (or (null source) (eq source ':ambiguous) (gethash directory seen))
               (return nil))
             (setf (gethash directory seen) t)
             (let ((resolved (world-repository-resolve-directory source)))
               (when resolved (return resolved)))
             (let ((identities
                     (remove-duplicates
                      (loop for channel in channels
                            when (and (equal source (channel-workspace channel))
                                      (channel-repository-key channel))
                              collect (list :key       (channel-repository-key channel)
                                            :directory (channel-repository-directory channel)))
                      :test #'equal)))
               (when (rest identities) (return nil))
               (when identities (return (first identities))))
             (setf directory source))))

(-> world-repository-remember (configuration list list) list)
(defun world-repository-remember (configuration channels groups)
  "Persist positive repository observations with workspace guards and return channel replacements.

Discover Git and bounded legacy metadata outside the world transaction. Never
resurrect a deleted channel or mutate objects shared by a read-only catalogue."
  (let ((observations nil)
        (sources      nil))
    (dolist (group groups)
      (dolist (channel (world-repository-group-channels group))
        (let* ((key (world-repository-group-key group))
               (identity
                  (cond
                    ((and (uiop:string-prefix-p "git:" key)
                          (world-repository-group-origin-observed-p group))
                     (list :key key :directory (world-repository-group-directory group)))
                    ((and (null (channel-repository-key channel))
                          (not (uiop:directory-exists-p (platform-pathname (channel-workspace channel)))))
                     (unless sources (setf sources (world-repository--task-sources configuration)))
                     (world-repository--recover (channel-workspace channel) sources channels)))))
          (when (and identity
                     (not (and (equal (getf identity :key) (channel-repository-key channel))
                               (equal (getf identity :directory) (channel-repository-directory channel)))))
            (push (list channel identity) observations)))))
    (if (null observations)
        channels
        (world--transact
         configuration
         (lambda (world)
           (let ((records      nil)
                 (replacements (make-hash-table :test #'equal)))
             (dolist (observation observations)
               (destructuring-bind (observed identity) observation
                 (let ((current (world-channel-find world (channel-identifier observed))))
                   (when (and current
                              (equal (channel-workspace current) (channel-workspace observed))
                              (equal (channel-repository-key current) (channel-repository-key observed))
                              (equal (channel-repository-directory current) (channel-repository-directory observed)))
                     (let ((updated (world--copy-channel
                                     current :repository-key (getf identity :key)
                                     :repository-directory   (getf identity :directory))))
                       (push (world--channel-record updated) records)
                       (setf (gethash (channel-identifier current) replacements) updated))))))
             (values (nreverse records)
                     (mapcar (lambda (channel)
                               (or (gethash (channel-identifier channel) replacements) channel)) channels))))))))
