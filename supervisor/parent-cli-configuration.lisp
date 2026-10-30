;;;; -- Parent Settings Bridge --

(in-package #:autolith-supervisor)

(defparameter *parent-cli-setting-defaults*
  '(:model-provider "" :model "gpt-6.1-sol" :reasoning-effort "high" :reasoning-summary "auto"
    :openrouter-provider-routing "" :codex-fast-mode-p nil :prefer-codex-context-window-p t
    :web-search-mode "cached" :compaction-threshold-percent 80
    :reasoning-traces-p nil :compact-view-p t :turn-timestamps-p nil :cache-miss-notices-p nil
    :knowledge-mode "active" :diagnosis-p nil :idle-compaction-p t
    :prompt-cache-retention-seconds 0 :simple-technical-english-p nil :session-title-generation-p t
    :hurry-up-p nil :relax-p nil :fullscreen-p nil :onboarding-p t :terminal-theme :autolith
    :boot-screen-p t :boot-screen-seconds 3.5 :boot-screen-linger-p t :boot-screen-tip-seconds 10
    :permission-mode :ask :immutable-p nil :pristine-p nil)
  "Ordinary public setting defaults, independent of agent initialization.")

(defparameter *parent-cli-setting-environments*
  '((:model-provider . "AUTOLITH_MODEL_PROVIDER") (:model . "AUTOLITH_MODEL")
    (:reasoning-effort . "AUTOLITH_REASONING_EFFORT")
    (:knowledge-mode . "AUTOLITH_KNOWLEDGE")
    (:site-config-root . "AUTOLITH_SITE_CONFIG_ROOT")
    (:web-search-mode . "AUTOLITH_WEB_SEARCH")
    (:openrouter-provider-routing . "AUTOLITH_OPENROUTER_PROVIDER_ROUTING")
    (:codex-fast-mode-p . "AUTOLITH_CODEX_FAST_MODE")
    (:prefer-codex-context-window-p . "AUTOLITH_PREFER_CODEX_CONTEXT_WINDOW")
    (:compaction-threshold-percent . "AUTOLITH_COMPACTION_THRESHOLD")
    (:onboarding-p . "AUTOLITH_ONBOARDING") (:diagnosis-p . "AUTOLITH_DIAGNOSIS")
    (:boot-screen-p . "AUTOLITH_BOOT_SCREEN") (:boot-screen-seconds . "AUTOLITH_BOOT_DURATION")
    (:boot-screen-linger-p . "AUTOLITH_BOOT_LINGER")
    (:boot-screen-tip-seconds . "AUTOLITH_BOOT_TIP_SECONDS"))
  "Existing public environment bindings read by the trusted settings bridge.")

(serapeum/types:-> parent-cli-root (keyword) pathname)
(defun parent-cli-root (kind)
  "Resolve a per-user root using the ordinary XDG and Windows precedence."
  (let* ((variable (ecase kind (:config
                                "XDG_CONFIG_HOME") (:state
                                                    "XDG_STATE_HOME")
                              (:data
                               "XDG_DATA_HOME") (:cache
                                                 "XDG_CACHE_HOME")))
         (value    (uiop:getenv variable)))
    (cond
      ((and value (uiop:absolute-pathname-p value))
       (merge-pathnames "autolith/" (uiop:ensure-directory-pathname value)))
      ((uiop:os-windows-p)
       (let ((base (uiop:getenv (if (eq kind ':config) "APPDATA" "LOCALAPPDATA"))))
         (unless (and base (uiop:absolute-pathname-p base))
           (parent-configuration--reject ':missing-application-root))
         (merge-pathnames (ecase kind (:config
                                       "autolith/") (:state
                                                     "autolith/state/")
                                     (:data
                                      "autolith/data/") (:cache
                                                         "autolith/cache/"))
                          (uiop:ensure-directory-pathname base))))
      (t
       (merge-pathnames (ecase kind (:config
                                     ".config/autolith/") (:state
                                                           ".local/state/autolith/")
                                   (:data
                                    ".local/share/autolith/") (:cache
                                                               ".cache/autolith/"))
                        (user-homedir-pathname))))))

(serapeum/types:-> parent-cli--reader-dispatch (stream character t) nil)
(defun parent-cli--reader-dispatch (stream character argument)
  "Reject allocating or executable dispatch syntax in owner settings."
  (declare (ignore stream character argument))
  (parent-configuration--reject ':unsupported-reader-syntax))

(serapeum/types:-> parent-cli-read-preferences ((or pathname string)) list)
(defun parent-cli-read-preferences (pathname)
  "Read one bounded version 8 record. Preserve first-key precedence and unknown data.
Reader evaluation, shared objects, arrays, structures and reader conditionals are disabled."
  (unless (probe-file pathname)
    (return-from parent-cli-read-preferences nil))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (when (> (file-length stream) 1048576)
      (parent-configuration--reject ':preferences-too-large)))
  (handler-case
      (with-open-file (stream pathname :external-format ':utf-8)
        (let ((*read-eval* nil)
              (*package*   (find-package '#:cl-user))
              (*readtable* (copy-readtable nil))
              (end         (list nil)))
          (dolist (character '(#\= #\# #\( #\A #\a #\S #\s #\. #\+ #\- #\C #\c))
            (set-dispatch-macro-character #\# character #'parent-cli--reader-dispatch))
          (let* ((form   (read stream nil end))
                 (values (and (consp form) (rest form))))
            (unless (and (parent-configuration--data-p form)
                         (consp form) (eq (first form) ':preferences)
                         (listp values) (list-length values) (evenp (length values))
                         (loop for (key) on values by (lambda (tail) (rest (rest tail)))
                               always (keywordp key))
                         (eql (getf values :version) 8)
                         (eq (read stream nil end) end))
              (parent-configuration--reject ':invalid-preferences))
            (loop with seen = nil
                  for (key value) on values by (lambda (tail) (rest (rest tail)))
                  unless (member key seen)
                    do (push key seen)
                    and append (unless (eq key ':version) (list key value))))))
    (parent-configuration-error (condition)
      (error condition))
    (error ()
      (parent-configuration--reject ':invalid-preferences))))

(serapeum/types:-> parent-cli--environment-value (keyword string) t)
(defun parent-cli--environment-value (key value)
  "Decode existing typed environment values without using the Lisp reader."
  (cond
    ((member key '(:codex-fast-mode-p :prefer-codex-context-window-p :onboarding-p
                   :diagnosis-p :boot-screen-p :boot-screen-linger-p))
     (cond
       ((member (string-downcase value) '("1" "true" "yes" "on") :test #'string=)
        t)
       ((member (string-downcase value) '("0" "false" "no" "off") :test #'string=)
        nil)
       (t
        (parent-configuration--reject ':invalid-boolean-environment))))
    ((member key '(:compaction-threshold-percent :boot-screen-tip-seconds))
     (handler-case
         (parse-integer value :junk-allowed nil)
       (parse-error ()
         (parent-configuration--reject ':invalid-numeric-environment))))
    ((eq key ':boot-screen-seconds)
     (let* ((parts    (uiop:split-string value :separator "."))
            (whole    (first parts))
            (fraction (second parts)))
       (unless (and (<= 1 (length parts) 2) (plusp (length whole))
                    (every #'digit-char-p whole)
                    (or (null fraction)
                        (and (plusp (length fraction)) (every #'digit-char-p fraction))))
         (parent-configuration--reject ':invalid-duration-environment))
       (let ((number (+ (parse-integer whole)
                        (if fraction (/ (parse-integer fraction) (expt 10 (length fraction))) 0))))
         (unless (<= 0 number 3600)
           (parent-configuration--reject ':invalid-duration-environment))
         number)))
    (t
     value)))

(serapeum/types:-> parent-cli-settings
    (parent-cli-request &key (:preferences list) (:overrides list)) list)
(defun parent-cli-settings (request &key preferences overrides)
  "Resolve explicit values, environment, durable preferences, then defaults.
Keep this complete owner settings plist in the parent. Project approved keys separately."
  (let ((settings (copy-tree *parent-cli-setting-defaults*)))
    (loop for (key value) on preferences by (lambda (tail) (rest (rest tail)))
          do (setf (getf settings key) value))
    (dolist (entry *parent-cli-setting-environments*)
      (let ((value (uiop:getenv (rest entry))))
        (when value
          (setf (getf settings (first entry))
                (parent-cli--environment-value (first entry) value)))))
    (loop for (key value) on overrides by (lambda (tail) (rest (rest tail)))
          do (setf (getf settings key) value))
    (loop for (option setting) in '((:fullscreen :fullscreen-p) (:immutable :immutable-p)
                                    (:pristine :pristine-p) (:site-config-root :site-config-root)
                                    (:permissions :permission-mode))
          when (member option (parent-cli-request-options request))
            do (setf (getf settings setting) (parent-cli-option request option)))
    (when (parent-cli-option request ':almighty)
      (setf (getf settings :fullscreen-p) t
            (getf settings :terminal-theme) ':almighty))
    (when (getf settings :pristine-p)
      (remf settings :provider-extensions))
    settings))

(serapeum/types:-> parent-cli--absolute (t &optional pathname) string)
(defun parent-cli--absolute (value &optional (base (uiop:getcwd)))
  "Resolve a user path against BASE without interpreting wildcard syntax."
  (unless (and (typep value '(or pathname string)) (not (wild-pathname-p value)))
    (parent-configuration--reject ':invalid-path))
  (namestring (merge-pathnames value base)))

(serapeum/types:-> parent-cli-runtime-roots
    (t t &key (:agent-core t) (:worker-core t)) list)
(defun parent-cli-runtime-roots (source runtime &key agent-core worker-core)
  "Return public source, image, shell and foreign-library directories for native workers.
Shared libraries reopen after an image starts. Admit their library directories,
not their installation prefix or the user's home directory. A Nix runtime also
needs its public store for executable link dependencies outside SBCL's registry.
Native policy rejects any overlap with parent-private storage."
  (remove-duplicates
   (append (list (parent-cli--absolute source)
                 (namestring (uiop:pathname-directory-pathname
                              (pathname (parent-cli--absolute runtime)))))
           (supervisor--shell-runtime-roots)
           (when (uiop:subpathp (pathname (parent-cli--absolute runtime)) #P"/nix/store/")
             (list "/nix/store/"))
           (loop for core in (list agent-core worker-core)
                 when core collect (namestring (uiop:pathname-directory-pathname
                                                (pathname (parent-cli--absolute core)))))
           (loop for object in (copy-list sb-sys:*shared-objects*)
                 collect (namestring
                          (uiop:pathname-directory-pathname
                           (pathname (parent-cli--absolute
                                      (sb-alien::shared-object-pathname object)))))))
   :test #'equal))

(serapeum/types:-> parent-cli-build-source-core (t t pathname) pathname)
(defun parent-cli-build-source-core (source runtime temporary)
  "Build a fresh child image in a separate process without loading agent code here.
The standard image builder loads tracked source, not ordinary init or private replay."
  (let ((core (merge-pathnames "source/autolith-active.core" temporary))
        (log  (merge-pathnames "source/build.log" temporary)))
    (ensure-directories-exist core)
    (with-open-file (stream log :direction ':output :if-exists ':supersede)
      (multiple-value-bind (output errors status)
          (uiop:run-program
           (list (parent-cli--absolute runtime) "--noinform" "--no-sysinit" "--no-userinit"
                 "--script" (parent-cli--absolute "script/build-active.lisp"
                                                    (uiop:ensure-directory-pathname source))
                 (namestring core))
           :directory source :output stream :error-output stream :ignore-error-status t)
        (declare (ignore output errors))
        (unless (and (eql status 0) (probe-file core))
          (parent-configuration--reject ':source-image-build-failed))))
    core))

(serapeum/types:-> parent-cli-configuration
    (parent-cli-request &key (:source-root (or null pathname string))
                            (:runtime (or null pathname string))
                            (:agent-core (or null pathname string))
                            (:worker-core (or null pathname string))
                            (:profiles list) (:profile-id (or null string))
                            (:overrides list)) parent-configuration)
(defun parent-cli-configuration (request &key source-root runtime agent-core worker-core
                                             profiles profile-id overrides)
  "Build owner-only launch settings without loading AUTOLITH or ordinary init.
Select registered provider profiles unless the caller supplies an explicit profile set."
  (let* ((data        (parent-cli-root ':data))
         (state       (parent-cli-root ':state))
         (preferences (merge-pathnames "preferences.sexp" state))
         (settings    (parent-cli-settings request :preferences (parent-cli-read-preferences preferences)
                                                  :overrides    overrides))
         (source      (or source-root (uiop:getenv "AUTOLITH_SOURCE_ROOT")
                          (asdf:system-source-directory "autolith-supervisor")))
         (runtime     (or runtime (uiop:getenv "AUTOLITH_SBCL")
                          (first (uiop:raw-command-line-arguments))))
         (core        (or agent-core (uiop:getenv "AUTOLITH_ACTIVE_CORE")
                          (merge-pathnames "active/autolith-active.core" data)))
         (worker      (or worker-core (uiop:getenv "AUTOLITH_WORKER_CORE") core))
         (workspace   (uiop:ensure-directory-pathname (uiop:getcwd)))
         (private     (merge-pathnames "parent/" data))
         (temporary   (merge-pathnames (format nil "autolith-parent-~36R/~36R/"
                                              (get-universal-time) (random (expt 36 12)))
                                       (uiop:temporary-directory)))
         (source-core (when (parent-cli-option request ':from-source)
                        (parent-cli-build-source-core source runtime temporary)))
         (core        (or source-core core))
         (worker      (if source-core source-core worker))
         (selection
           (unless profiles
             (ensure-directories-exist (merge-pathnames "probe" temporary))
             (ensure-directories-exist (merge-pathnames "probe" private))
             (multiple-value-list
              (parent-provider-configuration-profiles
               settings :extensions (unless (getf settings :pristine-p)
                                      (getf settings :provider-extensions))
               :writable-roots      (append (list workspace temporary private
                                                  (pathname (parent-cli--absolute source))
                                                  (uiop:pathname-directory-pathname (pathname (parent-cli--absolute runtime))))
                                            (getf settings :child-writable-roots)
                                            (when (getf settings :site-config-root)
                                              (list (uiop:ensure-directory-pathname
                                                     (parent-cli--absolute (getf settings :site-config-root))))))))))
         (configuration
           (make-parent-configuration
            (list :version 1 :runtime (parent-cli--absolute runtime)
                  :agent-core (parent-cli--absolute core) :worker-core (parent-cli--absolute worker)
                  :source-root (parent-cli--absolute source) :workspace (namestring workspace)
                  :private-root (namestring private) :temporary-root (namestring temporary)
                  :runtime-roots             (parent-cli-runtime-roots
                                              source runtime :agent-core core :worker-core worker)
                  :profiles                  (or profiles (first selection))
                  :profile-id                (or profile-id (second selection))
                  :account-configuration     (list :state-root    (namestring state)
                                                   :api-keys-path (namestring (merge-pathnames "api-keys.sexp" state)))
                  :reasoning-effort          (getf settings :reasoning-effort)
                  :knowledge-mode            (getf settings :knowledge-mode "active")
                  :allow-global-p            t
                  :access-p                  (not (null (member (getf settings :permission-mode) '("full" :full-access)
                                                               :test #'equal)))
                  :settings settings :preferences-path (namestring preferences)
                  :provider-extensions       (getf settings :provider-extensions)
                  :legacy-conversations-root (namestring (merge-pathnames "conversations/" data))
                  :data-root (namestring data) :state-root (namestring state)
                  :config-root               (namestring (parent-cli-root ':config))))))
    (setf (parent-configuration-provider-registry configuration) (third selection)
          (parent-configuration-provider-paths configuration)
          (loop for specification in (unless (getf settings :pristine-p)
                                       (getf settings :provider-extensions))
                collect (ls-compat.posix:resolve-pathname (getf specification :path))))
    configuration))
