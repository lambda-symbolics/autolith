(in-package #:autolith)

;;;; -- Release Host Updater Configuration --

(defparameter *release-updater-default-poll-seconds* 60
  "The default delay between release-host source update checks.")

(defparameter *release-updater-default-activation-timeout-seconds* 30
  "The default time allowed for the restarted release server to become healthy.")

(defparameter *release-updater-service-command-function* #'uiop:run-program
  "The effect used for s6 service control and status commands.")

(defparameter *release-updater-host-command-function* #'uiop:run-program
  "The effect used for attended host setup commands.")

(defclass release-updater-configuration ()
  ((selection-path
    :initarg       :selection-path
    :reader        release-updater-configuration-selection-path
    :type          pathname
    :documentation "The symlink atomically selecting the active host deployment.")
   (deployments-root
    :initarg       :deployments-root
    :reader        release-updater-configuration-deployments-root
    :type          pathname
    :documentation "The root containing immutable versioned host deployments.")
   (state-root
    :initarg       :state-root
    :reader        release-updater-configuration-state-root
    :type          pathname
    :documentation "The private root containing updater state and its process lock.")
   (host-lock-root
    :initarg       :host-lock-root
    :reader        release-updater-configuration-host-lock-root
    :type          pathname
    :documentation "The state root containing the shared builder/updater lock.")
   (repository
    :initarg       :repository
    :reader        release-updater-configuration-repository
    :type          string
    :documentation "The public Git repository queried for immutable release tags.")
   (poll-seconds
    :initarg       :poll-seconds
    :reader        release-updater-configuration-poll-seconds
    :type          (integer 1)
    :documentation "The delay after a remote release-tag scan.")
   (activation-timeout-seconds
    :initarg       :activation-timeout-seconds
    :reader        release-updater-configuration-activation-timeout-seconds
    :type          (integer 1)
    :documentation "The maximum time allowed for host service activation.")
   (server-service
    :initarg       :server-service
    :reader        release-updater-configuration-server-service
    :type          pathname
    :documentation "The supervised release-server service directory.")
   (builder-service
    :initarg       :builder-service
    :reader        release-updater-configuration-builder-service
    :type          pathname
    :documentation "The supervised release-builder service directory.")
   (health-url
    :initarg       :health-url
    :reader        release-updater-configuration-health-url
    :type          string
    :documentation "The loopback release-server health endpoint.")
   (service-account
    :initarg       :service-account
    :reader        release-updater-configuration-service-account
    :type          string
    :documentation "The unprivileged account used to prepare runtime files.")
   (service-home
    :initarg       :service-home
    :reader        release-updater-configuration-service-home
    :type          pathname
    :documentation "The home used by unprivileged bootstrap and check processes."))
  (:documentation "Settings for transactional release-host source updates."))

(define-condition release-updater-error (error)
  ((stage
    :initarg       :stage
    :reader        release-updater-error-stage
    :type          keyword
    :documentation "The updater stage that failed.")
   (tag
    :initarg       :tag
    :initform      nil
    :reader        release-updater-error-tag
    :type          (option string)
    :documentation "The release tag involved in the failure, when known.")
   (cause
    :initarg       :cause
    :reader        release-updater-error-cause
    :type          t
    :documentation "The underlying condition or bounded diagnostic value."))
  (:report
   (lambda (condition stream)
     (format stream "Release host update failed during ~(~A~)~@[ for ~A~]: ~A"
             (release-updater-error-stage condition)
             (release-updater-error-tag condition)
             (release-updater-error-cause condition))))
  (:documentation "A structured transactional release-host update failure."))

(-> release-updater-configuration-create
    (&key (:selection-path (option pathname))
          (:deployments-root (option pathname))
          (:state-root (option pathname))
          (:host-lock-root (option pathname))
          (:repository (option string))
          (:poll-seconds (option integer))
          (:activation-timeout-seconds (option integer))
          (:server-service (option pathname))
          (:builder-service (option pathname))
          (:health-url (option string))
          (:service-account (option string))
          (:service-home (option pathname)))
    release-updater-configuration)
(defun release-updater-configuration-create
    (&key selection-path deployments-root state-root host-lock-root repository
          poll-seconds activation-timeout-seconds server-service builder-service
          health-url service-account service-home)
  "Create release-host updater settings from arguments and the environment."
  (make-instance
   'release-updater-configuration
   :selection-path
   (or selection-path
       (uiop:getenv-pathname "AUTOLITH_RELEASE_SELECTION_PATH")
       #p"/srv/autolith-release-server/current")
   :deployments-root
   (uiop:ensure-directory-pathname
    (or deployments-root
        (uiop:getenv-pathname "AUTOLITH_RELEASE_DEPLOYMENTS_ROOT")
        #p"/srv/autolith-release-server/deployments/"))
   :state-root
   (uiop:ensure-directory-pathname
    (or state-root
        (uiop:getenv-pathname "AUTOLITH_RELEASE_UPDATER_STATE_ROOT")
        #p"/var/lib/autolith-release-server/updater/"))
   :host-lock-root
   (uiop:ensure-directory-pathname
    (or host-lock-root
        (uiop:getenv-pathname "AUTOLITH_RELEASE_STATE_ROOT")
        #p"/var/lib/autolith-release-server/builder/"))
   :repository
   (or repository
       (uiop:getenv "AUTOLITH_RELEASE_REPOSITORY")
       *release-builder-default-repository*)
   :poll-seconds
   (or poll-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_UPDATE_POLL_SECONDS")
        *release-updater-default-poll-seconds*
        "release host update poll interval"))
   :activation-timeout-seconds
   (or activation-timeout-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_ACTIVATION_TIMEOUT_SECONDS")
        *release-updater-default-activation-timeout-seconds*
        "release host activation timeout"))
   :server-service
   (or server-service
       (uiop:getenv-pathname "AUTOLITH_RELEASE_SERVER_SERVICE")
       #p"/run/service/autolith-release-server")
   :builder-service
   (or builder-service
       (uiop:getenv-pathname "AUTOLITH_RELEASE_BUILDER_SERVICE")
       #p"/run/service/autolith-release-builder")
   :health-url
   (or health-url
       (uiop:getenv "AUTOLITH_RELEASE_HEALTH_URL")
       "http://127.0.0.1:8098/health")
   :service-account
   (or service-account
       (uiop:getenv "AUTOLITH_RELEASE_SERVICE_ACCOUNT")
       "autolith-release")
   :service-home
   (uiop:ensure-directory-pathname
    (or service-home
        (uiop:getenv-pathname "AUTOLITH_RELEASE_SERVICE_HOME")
        #p"/var/lib/autolith-release-server/home/"))))


;;;; -- Durable Update State --

(-> release-updater--state-pathname (release-updater-configuration) pathname)
(defun release-updater--state-pathname (configuration)
  "Return CONFIGURATION's durable deployment transaction pathname."
  (merge-pathnames "deployment.sexp"
                   (release-updater-configuration-state-root configuration)))

(-> release-updater--state
    (keyword release-source-tag
     &key (:previous (option release-source-tag))
          (:failed (option release-source-tag)))
    list)
(defun release-updater--state (phase source-tag &key previous failed)
  "Return one validated native deployment-state form."
  (list ':autolith-release-deployment
        ':version 1
        ':phase phase
        ':tag (release-source-tag-name source-tag)
        ':commit (release-source-tag-commit source-tag)
        ':previous-tag (and previous (release-source-tag-name previous))
        ':previous-commit (and previous (release-source-tag-commit previous))
        ':failed-tag (and failed (release-source-tag-name failed))
        ':failed-commit (and failed (release-source-tag-commit failed))))

(-> release-updater--state-valid-p (t) boolean)
(defun release-updater--state-valid-p (state)
  "Return true when STATE is one complete supported deployment-state form."
  (and (listp state)
       (= (length state) 17)
       (eq (first state) ':autolith-release-deployment)
       (eq (second state) ':version)
       (eql (third state) 1)
       (eq (fourth state) ':phase)
       (member (fifth state) '(:prepared :selected :active) :test #'eq)
       (eq (sixth state) ':tag)
       (stringp (seventh state))
       (release-tag-valid-p (seventh state))
       (eq (eighth state) ':commit)
       (stringp (ninth state))
       (release-commit-valid-p (ninth state))
       (eq (tenth state) ':previous-tag)
       (or (null (nth 10 state))
           (and (stringp (nth 10 state))
                (release-tag-valid-p (nth 10 state))))
       (eq (nth 11 state) ':previous-commit)
       (or (null (nth 12 state))
           (and (stringp (nth 12 state))
                (release-commit-valid-p (nth 12 state))))
       (eq (nth 13 state) ':failed-tag)
       (or (null (nth 14 state))
           (and (stringp (nth 14 state))
                (release-tag-valid-p (nth 14 state))))
       (eq (nth 15 state) ':failed-commit)
       (or (null (nth 16 state))
           (and (stringp (nth 16 state))
                (release-commit-valid-p (nth 16 state))))
       (eq (null (nth 10 state)) (null (nth 12 state)))
       (eq (null (nth 14 state)) (null (nth 16 state)))))

(-> release-updater--read-state
    (release-updater-configuration)
    (option list))
(defun release-updater--read-state (configuration)
  "Read and validate CONFIGURATION's single durable deployment-state form."
  (let ((pathname (release-updater--state-pathname configuration)))
    (unless (probe-file pathname)
      (return-from release-updater--read-state nil))
    (handler-case
        (with-open-file (stream pathname
                                :direction       ':input
                                :external-format ':utf-8)
          (let ((*read-eval* nil)
                (eof         (list)))
            (let ((state    (read stream nil eof))
                  (trailing (read stream nil eof)))
              (unless (and (not (eq state eof))
                           (eq trailing eof)
                           (release-updater--state-valid-p state))
                (error 'release-updater-error
                       :stage ':state
                       :cause "The deployment-state file is malformed or unsupported."))
              state)))
      (release-updater-error (condition)
        (error condition))
      (error (cause)
        (error 'release-updater-error
               :stage ':state
               :cause cause)))))

(-> release-updater--write-state
    (release-updater-configuration list)
    pathname)
(defun release-updater--write-state (configuration state)
  "Atomically persist validated deployment STATE and return its pathname."
  (unless (release-updater--state-valid-p state)
    (error 'release-updater-error
           :stage ':state
           :cause "Refusing to persist an invalid deployment state."))
  (let* ((pathname (release-updater--state-pathname configuration))
         (temporary
           (make-pathname
            :name     (format nil ".deployment.~D" (sb-posix:getpid))
            :type     "tmp"
            :defaults pathname)))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (let ((*print-pretty*   t)
                   (*print-readably* t))
               (write state :stream stream)
               (terpri stream))
             (finish-output stream)
             (sb-posix:fsync (sb-sys:fd-stream-fd stream)))
           (sb-posix:chmod (namestring temporary) #o600)
           (sb-posix:rename (namestring temporary) (namestring pathname))
           pathname)
      (when (probe-file temporary)
        (delete-file temporary)))))

(-> release-updater--state-source-tag (list keyword keyword) release-source-tag)
(defun release-updater--state-source-tag (state tag-key commit-key)
  "Return the source tag stored under TAG-KEY and COMMIT-KEY in STATE."
  (make-instance 'release-source-tag
                 :name   (getf (rest state) tag-key)
                 :commit (getf (rest state) commit-key)))

(-> release-updater--state-optional-source-tag
    (list keyword keyword)
    (option release-source-tag))
(defun release-updater--state-optional-source-tag (state tag-key commit-key)
  "Return STATE's optional source tag under TAG-KEY and COMMIT-KEY."
  (let ((tag    (getf (rest state) tag-key))
        (commit (getf (rest state) commit-key)))
    (and tag
         (make-instance 'release-source-tag :name tag :commit commit))))


;;;; -- Immutable Deployments --

(-> release-updater--deployment-path
    (release-updater-configuration release-source-tag)
    pathname)
(defun release-updater--deployment-path (configuration source-tag)
  "Return SOURCE-TAG's final immutable deployment path."
  (merge-pathnames
   (format nil "~A/" (release-source-tag-name source-tag))
   (release-updater-configuration-deployments-root configuration)))

(-> release-updater--checkout-valid-p
    (pathname release-source-tag)
    boolean)
(defun release-updater--checkout-valid-p (checkout source-tag)
  "Return true when CHECKOUT is clean and exactly represents SOURCE-TAG.

The tag must be lightweight. Annotated tags and a tag moved away from the
checked-out commit are rejected."
  (handler-case
      (and (uiop:directory-exists-p checkout)
           (string=
            (release-server--git-output
             checkout
             (list "cat-file" "-t"
                   (format nil "refs/tags/~A"
                           (release-source-tag-name source-tag))))
            "commit")
           (string=
            (release-server--git-output
             checkout
             (list "rev-parse"
                   (format nil "refs/tags/~A"
                           (release-source-tag-name source-tag))))
            (release-source-tag-commit source-tag))
           (string=
            (release-server--git-output checkout '("rev-parse" "HEAD"))
            (release-source-tag-commit source-tag))
           (zerop
            (length
             (release-server--git-output
              checkout '("status" "--porcelain" "--untracked-files=no"))))
           (string=
            (release-builder--source-version checkout)
            (subseq (release-source-tag-name source-tag) 1)))
    (error ()
      nil)))

(-> release-updater--source-tag-at (pathname) release-source-tag)
(defun release-updater--source-tag-at (checkout)
  "Return the exact lightweight semantic tag represented by CHECKOUT."
  (let* ((version (release-builder--source-version checkout))
         (tag     (format nil "v~A" version))
         (commit
           (release-server--git-output checkout '("rev-parse" "HEAD")))
         (source-tag
           (make-instance 'release-source-tag :name tag :commit commit)))
    (unless (release-updater--checkout-valid-p checkout source-tag)
      (error 'release-updater-error
             :stage ':source-validation
             :tag   tag
             :cause
             "The selected deployment is not its exact clean lightweight tag."))
    source-tag))

(-> release-updater--selected-deployment
    (release-updater-configuration)
    pathname)
(defun release-updater--selected-deployment (configuration)
  "Return the canonical deployment selected by CONFIGURATION's symlink."
  (let* ((selection
           (release-updater-configuration-selection-path configuration))
         (deployments-root
           (truename
            (release-updater-configuration-deployments-root configuration)))
         (target
           (handler-case
               (truename selection)
             (error (cause)
               (error 'release-updater-error
                      :stage ':selection
                      :cause cause))))
         (root-name   (namestring deployments-root))
         (target-name (namestring (uiop:ensure-directory-pathname target))))
    (unless (and (uiop:directory-exists-p target)
                 (uiop:string-prefix-p root-name target-name)
                 (> (length target-name) (length root-name)))
      (error 'release-updater-error
             :stage ':selection
             :cause
             "The current selection does not resolve inside the deployments root."))
    (uiop:ensure-directory-pathname target)))

(-> release-updater--selected-source-tag
    (release-updater-configuration)
    release-source-tag)
(defun release-updater--selected-source-tag (configuration)
  "Return the exact source tag selected for the running release host."
  (release-updater--source-tag-at
   (release-updater--selected-deployment configuration)))

(-> release-updater--replace-symlink (pathname pathname) pathname)
(defun release-updater--replace-symlink (selection target)
  "Atomically replace symlink SELECTION with an absolute link to TARGET."
  (let ((temporary
          (make-pathname
           :name     (format nil ".~A.~D"
                             (or (pathname-name selection) "selection")
                             (sb-posix:getpid))
           :type     "tmp"
           :defaults selection)))
    (ensure-directories-exist selection)
    (handler-case (delete-file temporary)
      (file-error ()
        nil))
    (unwind-protect
         (progn
           (sb-posix:symlink
            (namestring (truename (uiop:ensure-directory-pathname target)))
            (namestring temporary))
           (sb-posix:rename (namestring temporary) (namestring selection))
           selection)
      (handler-case (delete-file temporary)
        (file-error ()
          nil)))))

(-> release-updater--clone-tag
    (release-updater-configuration release-source-tag pathname)
    pathname)
(defun release-updater--clone-tag (configuration source-tag destination)
  "Fetch SOURCE-TAG into new exact detached checkout DESTINATION."
  (let ((tag (release-source-tag-name source-tag)))
    (ensure-directories-exist destination)
    (uiop:delete-empty-directory destination)
    (handler-case
        (progn
          (uiop:run-program
           (list "git" "init" "--quiet" (namestring destination))
           :output       ':interactive
           :error-output ':interactive)
          (release-server--git-run
           destination
           (list "remote" "add" "origin"
                 (release-updater-configuration-repository configuration))
           :output ':interactive)
          (release-server--git-run
           destination
           (list "-c" "gc.auto=0"
                 "-c" "maintenance.auto=false"
                 "fetch" "--quiet" "--depth" "1" "--no-tags" "origin"
                 (format nil "refs/tags/~A:refs/tags/~A" tag tag))
           :output ':interactive)
          (release-server--git-run
           destination
           (list "checkout" "--quiet" "--detach"
                 (release-source-tag-commit source-tag))
           :output ':interactive))
      (error (cause)
        (error 'release-updater-error
               :stage ':checkout
               :tag   tag
               :cause cause))))
  destination)

(-> release-updater--run-host-command
    (list &key (:directory (option pathname)))
    null)
(defun release-updater--run-host-command (command &key directory)
  "Run one attended host setup COMMAND, preserving output and failure status."
  (funcall *release-updater-host-command-function*
           command
           :directory    directory
           :input        ':interactive
           :output       ':interactive
           :error-output ':interactive)
  nil)

(-> release-updater--run-as-service
    (release-updater-configuration list
     &key (:stage keyword) (:tag string) (:directory (option pathname)))
    null)
(defun release-updater--run-as-service
    (configuration command &key stage tag directory)
  "Run COMMAND as the configured service account, reporting STAGE for TAG."
  (handler-case
      (let ((home
              (release-updater-configuration-service-home configuration)))
        (release-updater--run-host-command
         (append
          (list "s6-setuidgid"
                (release-updater-configuration-service-account configuration)
                "env"
                "-u" "AUTOLITH_SBCL"
                "-u" "AUTOLITH_SBCL_SOURCE_ROOT"
                (format nil "HOME=~A" (namestring home))
                (format nil "XDG_DATA_HOME=~A"
                        (namestring (merge-pathnames ".local/share/" home)))
                (format nil "XDG_STATE_HOME=~A"
                        (namestring (merge-pathnames ".local/state/" home)))
                (format nil "XDG_CACHE_HOME=~A"
                        (namestring (merge-pathnames ".cache/" home))))
          command)
         :directory directory)
        nil)
    (error (cause)
      (error 'release-updater-error
             :stage stage
             :tag   tag
             :cause cause))))

(-> release-updater--validate-remote-identity
    (release-updater-configuration release-source-tag)
    null)
(defun release-updater--validate-remote-identity (configuration source-tag)
  "Require the remote lightweight tag to retain SOURCE-TAG's exact identity."
  (let ((remote
          (find (release-source-tag-name source-tag)
                (release-builder-remote-tags
                 (release-builder-configuration-create
                  :source-root
                  (release-updater-configuration-selection-path configuration)
                  :state-root
                  (release-updater-configuration-state-root configuration)
                  :public-root #p"/tmp/"
                  :repository
                  (release-updater-configuration-repository configuration)))
                :key  #'release-source-tag-name
                :test #'string=)))
    (unless (and remote
                 (string= (release-source-tag-commit remote)
                          (release-source-tag-commit source-tag)))
      (error 'release-updater-error
             :stage ':tag-identity
             :tag   (release-source-tag-name source-tag)
             :cause "The remote release tag disappeared or changed identity.")))
  nil)

(-> release-updater--candidate-probe
    (release-updater-configuration pathname release-source-tag)
    null)
(defun release-updater--candidate-probe (configuration deployment source-tag)
  "Load and validate DEPLOYMENT's release-host entry points in a fresh process."
  (release-updater--run-as-service
   configuration
   (list "env"
         (format nil "AUTOLITH_RELEASE_SOURCE_ROOT=~A"
                 (namestring deployment))
         (namestring (merge-pathnames "server/run" deployment))
         "host-probe"
         (release-source-tag-name source-tag)
         (release-source-tag-commit source-tag))
   :stage ':candidate-probe
   :tag   (release-source-tag-name source-tag))
  (unless (release-updater--checkout-valid-p deployment source-tag)
    (error 'release-updater-error
           :stage ':candidate-probe
           :tag   (release-source-tag-name source-tag)
           :cause "Candidate validation changed tracked source state."))
  nil)

(-> release-updater--remove-managed-deployment (pathname) null)
(defun release-updater--remove-managed-deployment (deployment)
  "Remove one unselected updater-owned DEPLOYMENT after making it writable."
  (when (uiop:directory-exists-p deployment)
    ;; mallet:suppress no-ignore-errors -- best effort; the deletion below reports real failures
    (ignore-errors
      (uiop:run-program
       (list "chmod" "-R" "u+w" (namestring deployment))
       :output       nil
       :error-output nil))
    (uiop:delete-directory-tree deployment
                                :validate          t
                                :if-does-not-exist ':ignore))
  nil)

(-> release-updater--check-deployment
    (release-updater-configuration pathname release-source-tag)
    null)
(defun release-updater--check-deployment
    (configuration deployment source-tag)
  "Run DEPLOYMENT's repository check, retrying one fresh process after failure."
  (let ((command
          (list (namestring (merge-pathnames "script/check" deployment))))
        (tag (release-source-tag-name source-tag)))
    (handler-case
        (release-updater--run-as-service configuration command
                                         :stage     ':check
                                         :tag       tag
                                         :directory deployment)
      (release-updater-error (first-failure)
        (format *error-output*
                "~&Release host candidate check failed once; retrying: ~A~%"
                first-failure)
        (finish-output *error-output*)
        (release-updater--run-as-service configuration command
                                         :stage     ':check
                                         :tag       tag
                                         :directory deployment))))
  nil)

(-> release-updater--grant-service-write-access
    (release-updater-configuration pathname)
    null)
(defun release-updater--grant-service-write-access
    (configuration deployment)
  "Give the service account ownership of DEPLOYMENT and its private home."
  (let* ((account
           (release-updater-configuration-service-account configuration))
         (ownership (format nil "~A:~A" account account)))
    (release-updater--run-host-command
     (list "chown" "-R" ownership
           (namestring
            (release-updater-configuration-service-home configuration))))
    (release-updater--run-host-command
     (list "chown" "-R" ownership (namestring deployment))))
  nil)

(-> release-updater--setup-deployment
    (release-updater-configuration pathname release-source-tag)
    null)
(defun release-updater--setup-deployment
    (configuration deployment source-tag)
  "Bootstrap, check, probe, and lock down DEPLOYMENT at its final pathname."
  (let ((tag (release-source-tag-name source-tag)))
    (release-updater--grant-service-write-access configuration deployment)
    (release-updater--run-as-service
     configuration
     (list (namestring (merge-pathnames "script/bootstrap" deployment)))
     :stage     ':bootstrap
     :tag       tag
     :directory deployment)
    (release-updater--check-deployment configuration deployment source-tag)
    (release-updater--run-host-command
     (list "chown" "-R" "root:root" (namestring deployment)))
    (release-updater--run-host-command
     (list "chmod" "-R" "go-w" (namestring deployment)))
    (release-updater--candidate-probe configuration deployment source-tag)
    (release-updater--validate-remote-identity configuration source-tag))
  nil)

(-> release-updater-prepare-deployment
    (release-updater-configuration release-source-tag
     &key (:setup-function function))
    pathname)
(defun release-updater-prepare-deployment
    (configuration source-tag
     &key (setup-function #'release-updater--setup-deployment))
  "Prepare and fully validate immutable SOURCE-TAG before making it selectable."
  (let* ((tag       (release-source-tag-name source-tag))
         (deployments-root
           (release-updater-configuration-deployments-root configuration))
         (deployment
           (release-updater--deployment-path configuration source-tag))
         (incoming
           (merge-pathnames
            (format nil ".~A.incoming.~D/" tag (sb-posix:getpid))
            deployments-root))
         (managed-p nil))
    (when (uiop:directory-exists-p deployment)
      (unless (release-updater--checkout-valid-p deployment source-tag)
        (error 'release-updater-error
               :stage ':candidate-existing
               :tag   tag
               :cause
               "An existing deployment has the wrong identity or tracked changes."))
      (setf managed-p t))
    (when (uiop:directory-exists-p incoming)
      (error 'release-updater-error
             :stage ':candidate-existing
             :tag   tag
             :cause "The updater's incoming deployment path already exists."))
    (uiop:ensure-all-directories-exist (list deployments-root))
    (handler-case
        (progn
          (unless managed-p
            (release-updater--clone-tag configuration source-tag incoming)
            (unless (release-updater--checkout-valid-p incoming source-tag)
              (error 'release-updater-error
                     :stage ':source-validation
                     :tag   tag
                     :cause
                     "The fetched source does not match its immutable tag."))
            (rename-file incoming deployment)
            (setf managed-p t))
          (funcall setup-function configuration deployment source-tag)
          deployment)
      (release-updater-error (condition)
        (when managed-p
          (release-updater--remove-managed-deployment deployment))
        (release-updater--remove-managed-deployment incoming)
        (error condition))
      (error (cause)
        (when managed-p
          (release-updater--remove-managed-deployment deployment))
        (release-updater--remove-managed-deployment incoming)
        (error 'release-updater-error
               :stage ':preparation
               :tag   tag
               :cause cause)))))


;;;; -- Selection, Activation, and Rollback --

(-> release-updater--service-cycle
    (release-updater-configuration pathname)
    null)
(defun release-updater--service-cycle (configuration service)
  "Synchronously stop and start supervised SERVICE."
  (let ((timeout
          (write-to-string
           (* 1000
              (release-updater-configuration-activation-timeout-seconds
               configuration)))))
    (funcall
     *release-updater-service-command-function*
     (list "s6-svc" "-wd" "-T" timeout "-d" (namestring service))
     :output       ':interactive
     :error-output ':interactive)
    (funcall
     *release-updater-service-command-function*
     (list "s6-svc" "-wu" "-T" timeout "-u" (namestring service))
     :output       ':interactive
     :error-output ':interactive))
  nil)

(-> release-updater--server-healthy-p
    (release-updater-configuration (option release-source-tag))
    boolean)
(defun release-updater--server-healthy-p (configuration source-tag)
  "Return true when the server reports SOURCE-TAG, or generic legacy health."
  (handler-case
      (string=
       (string-trim
        *line-whitespace*
        (dexador:get
         (if source-tag
             (format nil "~A/~A/~A"
                     (string-right-trim
                      "/"
                      (release-updater-configuration-health-url configuration))
                     (release-source-tag-name source-tag)
                     (release-source-tag-commit source-tag))
             (release-updater-configuration-health-url configuration))
         :connect-timeout 2
         :read-timeout    2
         :force-string    t))
       "ok")
    (error ()
      nil)))

(-> release-updater--wait-for-server
    (release-updater-configuration (option release-source-tag))
    null)
(defun release-updater--wait-for-server (configuration source-tag)
  "Wait until the server reports SOURCE-TAG or its activation deadline expires."
  (let ((deadline
          (+ (get-internal-real-time)
             (* (release-updater-configuration-activation-timeout-seconds
                 configuration)
                internal-time-units-per-second))))
    (loop
      (when (release-updater--server-healthy-p configuration source-tag)
        (return nil))
      (when (>= (get-internal-real-time) deadline)
        (error 'release-updater-error
               :stage ':activation
               :cause "The restarted release server did not become healthy."))
      (sleep 1))))

(-> release-updater--service-up-p (pathname) boolean)
(defun release-updater--service-up-p (service)
  "Return true when s6 reports SERVICE up with a positive process identifier."
  (handler-case
      (let ((fields
              (uiop:split-string
               (string-trim
                *line-whitespace*
                (funcall
                 *release-updater-service-command-function*
                 (list "s6-svstat" "-o" "up,pid" (namestring service))
                 :output       ':string
                 :error-output ':output))
               :separator '(#\Space #\Tab))))
        (and (= (length fields) 2)
             (string= (first fields) "true")
             (plusp (parse-integer (second fields) :junk-allowed nil))))
    (error ()
      nil)))

(-> release-updater--restart-host-services
    (release-updater-configuration)
    null)
(defun release-updater--restart-host-services (configuration)
  "Cycle and verify the selected server and private release builder."
  (let* ((deployment
           (release-updater--selected-deployment configuration))
         (selected
           (and
            (uiop:file-exists-p
             (merge-pathnames "server/release-updater.lisp" deployment))
            (release-updater--source-tag-at deployment))))
    (release-updater--service-cycle
     configuration
     (release-updater-configuration-server-service configuration))
    (release-updater--wait-for-server configuration selected))
  (release-updater--service-cycle
   configuration
   (release-updater-configuration-builder-service configuration))
  (unless (release-updater--service-up-p
           (release-updater-configuration-builder-service configuration))
    (error 'release-updater-error
           :stage ':activation
           :cause "The restarted release builder is not running."))
  nil)

(-> release-updater--prune-deployments
    (release-updater-configuration list)
    null)
(defun release-updater--prune-deployments (configuration state)
  "Remove clean deployments other than STATE's current, previous, and failed."
  (let ((retained
          (remove
           nil
           (list (getf (rest state) ':tag)
                 (getf (rest state) ':previous-tag)
                 (getf (rest state) ':failed-tag)))))
    (dolist
        (deployment
         (uiop:subdirectories
          (release-updater-configuration-deployments-root configuration)))
      (let* ((components (pathname-directory deployment))
             (tag        (and components (lastcar components))))
        (when (and (stringp tag)
                   (release-tag-valid-p tag)
                   (not (member tag retained :test #'string=)))
          (handler-case
              (let ((source-tag
                      (release-updater--source-tag-at deployment)))
                (unless (string= tag (release-source-tag-name source-tag))
                  (error 'release-updater-error
                         :stage ':pruning
                         :tag   tag
                         :cause "Deployment directory and source tag differ."))
                (release-updater--remove-managed-deployment deployment)
                (format t "~&Pruned superseded host deployment ~A.~%" tag)
                (finish-output))
            (error (cause)
              (format *error-output*
                      "~&Retaining unverified host deployment ~A: ~A~%"
                      deployment cause)
              (finish-output *error-output*)))))))
  nil)

(-> release-updater--activate-selected
    (release-updater-configuration list function)
    release-source-tag)
(defun release-updater--activate-selected
    (configuration state restart-function)
  "Activate selected deployment STATE or roll back to its previous deployment."
  (let* ((selected
           (release-updater--state-source-tag state ':tag ':commit))
         (previous
           (release-updater--state-optional-source-tag
            state ':previous-tag ':previous-commit))
         (previous-path
           (and previous
                (release-updater--deployment-path configuration previous))))
    (unless previous
      (error 'release-updater-error
             :stage ':activation
             :tag   (release-source-tag-name selected)
             :cause "A selected deployment has no rollback identity."))
    (handler-case
        (progn
          (funcall restart-function configuration)
          (let ((active-state
                  (release-updater--state
                   ':active selected :previous previous)))
            (release-updater--write-state configuration active-state)
            (handler-case
                (release-updater--prune-deployments
                 configuration active-state)
              (error (cause)
                (format *error-output*
                        "~&Could not prune old host deployments: ~A~%"
                        cause)
                (finish-output *error-output*))))
          selected)
      (error (activation-cause)
        (handler-case
            (progn
              (release-updater--replace-symlink
               (release-updater-configuration-selection-path configuration)
               previous-path)
              (funcall restart-function configuration)
              (release-updater--write-state
               configuration
               (release-updater--state
                ':active previous :previous selected :failed selected))
              (error 'release-updater-error
                     :stage ':activation
                     :tag   (release-source-tag-name selected)
                     :cause activation-cause))
          (release-updater-error (condition)
            (error condition))
          (error (rollback-cause)
            (error 'release-updater-error
                   :stage ':rollback
                   :tag   (release-source-tag-name selected)
                   :cause
                   (format nil
                           "Activation failed (~A), and rollback failed (~A)."
                           activation-cause rollback-cause))))))))

(-> release-updater-promote
    (release-updater-configuration pathname release-source-tag
     &key (:restart-function function))
    release-source-tag)
(defun release-updater-promote
    (configuration deployment source-tag
     &key (restart-function #'release-updater--restart-host-services))
  "Atomically select prepared DEPLOYMENT and activate or roll back its services."
  (unless (and (equal (truename deployment)
                      (truename
                       (release-updater--deployment-path configuration source-tag)))
               (release-updater--checkout-valid-p deployment source-tag))
    (error 'release-updater-error
           :stage ':promotion
           :tag   (release-source-tag-name source-tag)
           :cause "The prepared deployment is not its exact final checkout."))
  (let* ((previous (release-updater--selected-source-tag configuration))
         (previous-path
           (release-updater--selected-deployment configuration))
         (state
           (release-updater--state
            ':prepared source-tag :previous previous)))
    (release-updater--write-state configuration state)
    (release-updater--replace-symlink
     (merge-pathnames "previous"
                      (uiop:pathname-directory-pathname
                       (release-updater-configuration-selection-path
                        configuration)))
     previous-path)
    (release-updater--replace-symlink
     (release-updater-configuration-selection-path configuration)
     deployment)
    (setf state
          (release-updater--state
           ':selected source-tag :previous previous))
    (release-updater--write-state configuration state)
    (release-updater--activate-selected
     configuration state restart-function)))

(-> release-updater--reconcile
    (release-updater-configuration function)
    release-source-tag)
(defun release-updater--reconcile (configuration restart-function)
  "Finish any interrupted deployment transaction and return the active tag."
  (let* ((current (release-updater--selected-source-tag configuration))
         (state   (release-updater--read-state configuration)))
    (unless state
      (release-updater--write-state
       configuration
       (release-updater--state ':active current))
      (return-from release-updater--reconcile current))
    (let* ((phase (getf (rest state) ':phase))
           (selected
             (release-updater--state-source-tag state ':tag ':commit))
           (previous
             (release-updater--state-optional-source-tag
              state ':previous-tag ':previous-commit))
           (current-selected-p
             (and (string=
                   (release-source-tag-name current)
                   (release-source-tag-name selected))
                  (string=
                   (release-source-tag-commit current)
                   (release-source-tag-commit selected))))
           (current-previous-p
             (and previous
                  (string=
                   (release-source-tag-name current)
                   (release-source-tag-name previous))
                  (string=
                   (release-source-tag-commit current)
                   (release-source-tag-commit previous)))))
      (cond
        ((eq phase ':active)
         (unless current-selected-p
           (error 'release-updater-error
                  :stage ':reconciliation
                  :cause "The active state does not match the current symlink."))
         current)
        ((and (eq phase ':prepared) current-previous-p)
         (let ((deployment
                 (release-updater--deployment-path configuration selected)))
           (unless (release-updater--checkout-valid-p deployment selected)
             (error 'release-updater-error
                    :stage ':reconciliation
                    :tag   (release-source-tag-name selected)
                    :cause "The prepared deployment is missing or invalid."))
           (release-updater--replace-symlink
            (merge-pathnames
             "previous"
             (uiop:pathname-directory-pathname
              (release-updater-configuration-selection-path configuration)))
            (release-updater--deployment-path configuration previous))
           (release-updater--replace-symlink
            (release-updater-configuration-selection-path configuration)
            deployment)
           (setf state
                 (release-updater--state
                  ':selected selected :previous previous))
           (release-updater--write-state configuration state)
           (release-updater--activate-selected
            configuration state restart-function)))
        ((and (member phase '(:prepared :selected) :test #'eq)
              current-selected-p)
         (unless (eq phase ':selected)
           (setf state
                 (release-updater--state
                  ':selected selected :previous previous))
           (release-updater--write-state configuration state))
         (release-updater--activate-selected
          configuration state restart-function))
        ((and (eq phase ':selected) current-previous-p)
         (funcall restart-function configuration)
         (release-updater--write-state
          configuration
          (release-updater--state
           ':active previous :previous selected :failed selected))
         previous)
        (t
         (error 'release-updater-error
                :stage ':reconciliation
                :cause
                "The durable transaction and current symlink cannot be reconciled."))))))


;;;; -- Polling --

(-> release-updater--pending-tag
    (release-source-tag list (option list))
    (option release-source-tag))
(defun release-updater--pending-tag (current remote-tags state)
  "Return the newest unfailed remote tag newer than CURRENT."
  (let* ((failed
           (and state
                (release-updater--state-optional-source-tag
                 state ':failed-tag ':failed-commit)))
         (eligible
           (remove-if-not
            (lambda (source-tag)
              (and
               (release-tag<
                (release-source-tag-name current)
                (release-source-tag-name source-tag))
               (not
                (and failed
                     (string=
                      (release-source-tag-name failed)
                      (release-source-tag-name source-tag))
                     (string=
                      (release-source-tag-commit failed)
                      (release-source-tag-commit source-tag))))))
            remote-tags)))
    (lastcar eligible)))

(-> release-updater--deterministic-preparation-failure-p
    (release-updater-error)
    boolean)
(defun release-updater--deterministic-preparation-failure-p (condition)
  "Return true when CONDITION cannot improve while its immutable tag is fixed."
  (not
   (null
    (member (release-updater-error-stage condition)
            '(:candidate-existing
              :source-validation
              :check
              :candidate-probe
              :tag-identity)
            :test #'eq))))

(-> release-updater--record-failed-tag
    (release-updater-configuration release-source-tag release-source-tag)
    pathname)
(defun release-updater--record-failed-tag (configuration current failed)
  "Record immutable FAILED as skipped while preserving CURRENT as active."
  (let* ((state (release-updater--read-state configuration))
         (previous
           (and state
                (release-updater--state-optional-source-tag
                 state ':previous-tag ':previous-commit))))
    (release-updater--write-state
     configuration
     (release-updater--state
      ':active current :previous previous :failed failed))))

(-> release-updater--call-with-lock
    (release-updater-configuration function)
    t)
(defun release-updater--call-with-lock (configuration function)
  "Call FUNCTION while holding CONFIGURATION's process-shared updater lock."
  (let ((pathname (merge-pathnames
                   "update.lock"
                   (release-updater-configuration-state-root configuration))))
    (ensure-directories-exist pathname)
    (call-with-file-lock pathname function)))

(-> release-updater-update-once
    (release-updater-configuration
     &key (:restart-function function))
    boolean)
(defun release-updater-update-once
    (configuration
     &key (restart-function #'release-updater--restart-host-services))
  "Reconcile and install at most one newest release-host update."
  (release-updater--call-with-lock
   configuration
   (lambda ()
     (release-host--call-with-lock
      (release-updater-configuration-host-lock-root configuration)
      (lambda ()
        (let* ((current
                 (release-updater--reconcile
                  configuration restart-function))
               (remote
                 (release-builder-remote-tags
                  (release-builder-configuration-create
                   :source-root
                   (release-updater-configuration-selection-path configuration)
                   :state-root
                   (release-updater-configuration-state-root configuration)
                   :public-root #p"/tmp/"
                   :repository
                   (release-updater-configuration-repository configuration))))
               (pending
                 (release-updater--pending-tag
                  current remote (release-updater--read-state configuration))))
          (unless pending
            (return-from release-updater-update-once nil))
          (handler-case
              (let ((deployment
                      (release-updater-prepare-deployment
                       configuration pending)))
                (release-updater--validate-remote-identity
                 configuration pending)
                (release-updater-promote
                 configuration deployment pending
                 :restart-function restart-function)
                t)
            (release-updater-error (condition)
              (when (release-updater--deterministic-preparation-failure-p
                     condition)
                (release-updater--record-failed-tag
                 configuration current pending))
              (error condition)))))))))

(-> release-updater-run (release-updater-configuration) null)
(defun release-updater-run (configuration)
  "Poll until one host deployment is promoted, then exit for an s6 reload."
  (format t "~&Autolith release host updater polling ~A every ~D seconds.~%"
          (release-updater-configuration-repository configuration)
          (release-updater-configuration-poll-seconds configuration))
  (finish-output)
  (loop
    (handler-case
        (when (release-updater-update-once configuration)
          (format t "~&Release host update activated; reloading updater.~%")
          (finish-output)
          (return nil))
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)))
    (sleep (release-updater-configuration-poll-seconds configuration))))


;;;; -- Candidate Probe --

(-> release-updater-host-probe (string string) null)
(defun release-updater-host-probe (tag commit)
  "Validate the loaded source as exact host candidate TAG at COMMIT."
  (unless (and (release-tag-valid-p tag)
               (release-commit-valid-p commit))
    (error 'release-updater-error
           :stage ':candidate-probe
           :tag   tag
           :cause "The candidate probe identity is malformed."))
  (let* ((source-root (asdf:system-source-directory :autolith))
         (source-tag
           (make-instance 'release-source-tag :name tag :commit commit)))
    (unless (release-updater--checkout-valid-p source-root source-tag)
      (error 'release-updater-error
             :stage ':candidate-probe
             :tag   tag
             :cause "Loaded source does not match the requested candidate."))
    (dolist (pathname
             (list (merge-pathnames "server/run" source-root)
                   (merge-pathnames "server/Containerfile" source-root)
                   (merge-pathnames "script/install" source-root)
                   (merge-pathnames ".qlot/setup.lisp" source-root)))
      (unless (uiop:file-exists-p pathname)
        (error 'release-updater-error
               :stage ':candidate-probe
               :tag   tag
               :cause (format nil "Required host file ~A is unavailable."
                              pathname))))
    (format t "~&Autolith release host candidate ~A at ~A is ready.~%"
            tag commit)
    (finish-output))
  nil)
