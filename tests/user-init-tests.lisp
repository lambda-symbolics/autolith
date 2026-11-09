(in-package #:autolith)

;;;; -- User Initialization Tests --

(defvar *user-init-test-value* nil
  "The value installed by the isolated user initialization fixture.")

(defvar *directory-user-init-test-log* nil
  "The load evidence installed by trusted directory initialization fixtures.")

(-> test-site-configuration-root () null)
(defun test-site-configuration-root ()
  "Test canonical site-root configuration, preservation, and rejection."
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-site-root-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (site-root             (merge-pathnames "site/" root))
         (environment-site-root (merge-pathnames "environment-site/" root))
         (missing-root          (merge-pathnames "missing/" root))
         (source-root           (asdf:system-source-directory :autolith))
         (previous-site-root    (uiop:getenv "AUTOLITH_SITE_CONFIG_ROOT")))
    (ensure-directories-exist site-root)
    (ensure-directories-exist environment-site-root)
    (unwind-protect
         (progn
           (platform-setenv "AUTOLITH_SITE_CONFIG_ROOT"
                            (namestring environment-site-root))
           (let ((environment-configuration
                   (configuration-create
                    :source-root                 source-root
                    :working-directory           source-root
                    :defer-provider-validation-p t)))
             (test-assert
              (equal
                (config :site-config-root environment-configuration)
               (uiop:ensure-directory-pathname
                  (platform-truename *platform* environment-site-root)))
              "the site root defaults from AUTOLITH_SITE_CONFIG_ROOT"))
           (let ((configuration
                   (configuration-create
                    :source-root                 source-root
                    :working-directory           source-root
                    :site-config-root            site-root
                    :defer-provider-validation-p t)))
             (test-assert
               (equal (config :site-config-root configuration)
                     (uiop:ensure-directory-pathname
                       (platform-truename *platform* site-root)))
              "an explicit site root overrides the environment default")
             (test-assert
              (equal (configuration-site-init-path configuration)
                     (merge-pathnames "init.lisp"
                                       (config :site-config-root configuration)))
              "the site init path is derived without creating the file")
             (test-assert
               (equal (config :site-config-root
                              (configuration-copy configuration :immutable-p t))
                      (config :site-config-root configuration))
              "configuration copies preserve their site root")
             (let ((command
                     (parse-command-line
                      (main--top-level-command)
                      (list "--site-config-root" (namestring site-root)))))
               (test-assert
                (string= (getopt* command ':site-config-root)
                         (namestring site-root))
                "the global command line accepts an explicit site root"))
             (test-assert
              (handler-case
                  (progn
                    (configuration-create
                     :source-root                 source-root
                     :working-directory           source-root
                     :site-config-root            #P"relative-site/"
                     :defer-provider-validation-p t)
                    nil)
                (configuration-error ()
                  t))
              "a relative explicit site root is rejected")
             (test-assert
              (handler-case
                  (progn
                    (configuration-create
                     :source-root                 source-root
                     :working-directory           source-root
                     :site-config-root            missing-root
                     :defer-provider-validation-p t)
                    nil)
                (configuration-error ()
                  t))
              "a missing explicit site root is rejected")
             (platform-unsetenv "AUTOLITH_SITE_CONFIG_ROOT")
             (let ((without-site
                     (configuration-create
                      :source-root                 source-root
                      :working-directory           source-root
                      :defer-provider-validation-p t)))
               (test-assert
                 (and (null (config :site-config-root without-site))
                     (null (configuration-site-init-path without-site)))
                "an unset site root preserves ordinary user configuration"))))
      (if previous-site-root
          (platform-setenv "AUTOLITH_SITE_CONFIG_ROOT" previous-site-root)
          (platform-unsetenv "AUTOLITH_SITE_CONFIG_ROOT"))
      (platform-delete-directory-tree *platform* root
                                     :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-site-configuration-noninteractive-commands () null)
(defun test-site-configuration-noninteractive-commands ()
  "Test site-root selection through both noninteractive command handlers."
  (with-test-configuration (fixture root)
    (let ((site-root        (merge-pathnames "site/" root))
          (environment-root (merge-pathnames "environment-site/" root)))
      (ensure-directories-exist site-root)
      (ensure-directories-exist environment-root)
      (with-test-environment
          (("XDG_CONFIG_HOME" (namestring (config :config-root fixture)))
           ("XDG_DATA_HOME" (namestring (config :data-root fixture)))
           ("XDG_STATE_HOME" (namestring (config :state-root fixture)))
           ("XDG_CACHE_HOME" (namestring (config :cache-root fixture))))
        (dolist (arguments '(("models")
                             ("run-job" "--input" "input.sexp"
                                        "--output" "output.sexp")))
          (dolist (selection '(:explicit :environment :override :absent))
            (let ((observed   nil)
                  (explicit-p (member selection '(:explicit :override))))
              (with-test-environment
                  (("AUTOLITH_SITE_CONFIG_ROOT"
                    (when (member selection '(:environment :override))
                      (namestring environment-root))))
                (test-call-with-function-replacements
                 (list
                  (list 'user-init-load
                        (lambda (configuration)
                          (setf observed configuration)))
                  (list 'provider-bootstrap-configuration
                        (lambda (configuration)
                          (declare (ignore configuration))))
                  (list 'run-job-run
                        (lambda (input output permission-mode &key configuration)
                          (declare (ignore input output permission-mode))
                          (setf observed configuration)
                          0)))
                 (lambda ()
                   (let ((*standard-output* (make-string-output-stream)))
                     (main-dispatch
                      (append (when explicit-p
                                (list "--site-config-root" (namestring site-root)))
                              arguments)))))
                (test-assert
                 (and observed
                      (equal
                        (config :site-config-root observed)
                       (let ((expected (cond
                                         (explicit-p
                                          site-root)
                                         ((eq selection :environment)
                                          environment-root))))
                         (when expected
                           (uiop:ensure-directory-pathname
                            (platform-truename *platform* expected))))))
                 (format nil "~A honors ~A site-root selection"
                         (first arguments) selection)))))))))
  nil)

(-> test-user-init-registry-lifecycle () null)
(defun test-user-init-registry-lifecycle ()
  "Test participant-wide clearing and rollback after loading or clearing fails."
  (let* ((registry  (first *extension-registry-lifecycles*))
         (names     '(context--registry-snapshot context--registry-restore
                      context--remove-registration-source))
         (originals (mapcar #'symbol-function names))
         (events    nil))
    (unwind-protect
         (progn
           (mapc (lambda (name callback) (setf (symbol-function name) callback))
                 names
                 (list (lambda () (push ':snapshot events) ':current)
                       (lambda (snapshot) (push (list ':restore snapshot) events))
                       (lambda (source) (push (list ':remove source) events))))
           (test-assert (eq (registry-snapshot registry) ':current)
                        "initialized participants use the current snapshot definition")
           (registry-remove-source registry ':user)
           (registry-restore registry ':current)
           (test-assert
            (equal (reverse events)
                   '(:snapshot (:remove :user) (:restore :current)))
            "initialized participants use current removal and restoration definitions"))
      (mapc (lambda (name original) (setf (symbol-function name) original))
            names originals)))
  (with-test-configuration (configuration)
    (let* ((states  (make-hash-table))
           (events  nil)
           (failure nil)
           (*extension-registry-lifecycles*
             (mapcar
              (lambda (name)
                (make-registry-lifecycle
                 :snapshot-function
                 (lambda ()
                   (push (list ':snapshot name) events)
                   (copy-list (gethash name states)))
                 :restore-function
                 (lambda (snapshot)
                   (push (list ':restore name) events)
                   (setf (gethash name states) (copy-list snapshot)))
                 :remove-source-function
                 (lambda (source)
                   (push (list ':remove name source) events)
                   (setf (gethash name states)
                         (remove source (gethash name states)))
                   (when (and (eq failure ':remove)
                              (eq name ':second)
                              (eq source ':user))
                     (error "broken registry clearing")))))
              '(:first :second))))
      (dolist (name '(:first :second))
        (setf (gethash name states) (list ':runtime ':site ':user)))
      (test-assert (null (user-init-load configuration))
                   "empty initialization clears every participant's old layers")
      (test-assert
       (every (lambda (name) (equal (gethash name states) '(:runtime)))
              '(:first :second))
       "clearing preserves unrelated registration sources")
      (test-mcp-configuration--write
       (configuration-user-init-path configuration)
       "(error \"broken registry initialization\")")
      (dolist (mode '(:load :remove))
        (setf failure mode events nil)
        (dolist (name '(:first :second))
          (setf (gethash name states) (list ':runtime ':site ':user)))
        (test-assert
         (handler-case
             (progn (user-init-load configuration) nil)
           (user-init-error (condition)
             (and (eq (user-init-error-layer condition) ':user)
                  (equal (user-init-error-pathname condition)
                         (configuration-user-init-path configuration))
                  (typep (user-init-error-cause condition) 'simple-error))))
         "participant and init failures retain the startup error boundary")
        (test-assert
         (every (lambda (name)
                  (equal (gethash name states) '(:runtime :site :user)))
                '(:first :second))
         "every participant restores its snapshot after either failure")
        (test-assert
         (equal (reverse events)
                '((:snapshot :first) (:snapshot :second)
                  (:remove :first :site) (:remove :second :site)
                  (:remove :first :user) (:remove :second :user)
                  (:restore :first) (:restore :second)))
         "snapshot, source clearing, and restoration preserve participant order"))))
  nil)

(-> test-user-init () null)
(defun test-user-init ()
  "Test user initialization discovery, package binding, and typed failure."
  (let* ((configuration         (test-configuration))
         (root                  (test-configuration-root configuration))
         (pathname              (configuration-user-init-path configuration))
         (context-registrations (context--registry-snapshot))
         (command-registrations (application-command--registry-snapshot))
         (mcp-registrations     (mcp--registry-snapshot)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-assert (null (user-init-load configuration))
                        "a missing user init is an ordinary empty configuration")
           (with-open-file (stream pathname
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (write-string
              "(progn
                 (setf *user-init-test-value*
                       (list *user-init-loading-p* *package*))
                 (register-context-contributor
                  \"user-init-test\"
                  'context-tests--next-request)
                 (define-context-contributor user-init-tests--contributor
                     (request)
                   (declare (ignore request))
                   (make-context-contribution
                    :identifier \"user-init-transaction\"
                    :instruction \"old contributor\"))
                 (define-application-command user-init-tests--command
                     (:name \"/user-init-test\"
                      :argument nil
                      :description \"exercise user command registration\"
                      :tip \"exists only in the user-init test.\"
                      :busy-behavior :inspect
                      :terminal-behavior :shared)
                     (application invocation)
                   (declare (ignore application invocation))
                   :continue))"
              stream))
           (setf *user-init-test-value* nil)
           (test-assert (equal (user-init-load configuration) pathname)
                        "the configured user init pathname is loaded")
           (test-assert
            (and (first *user-init-test-value*)
                 (eq (second *user-init-test-value*)
                     (find-package '#:autolith)))
            "user init executes in the Autolith package and marked dynamic extent")
           (test-assert
            (eq (getf (find "user-init-test"
                            (context-contributor-registrations)
                            :test #'string=
                            :key  (lambda (registration)
                                    (getf registration :identifier)))
                      :source)
                ':user)
            "contributors registered by user init retain their source")
           (let ((command (application-command-find "/user-init-test")))
             (test-assert
              (and command
                   (eq
                    (getf
                     (find
                      'user-init-tests--command
                      (application-command--registrations)
                      :key (lambda (registration)
                             (getf registration :definition-name)))
                     :source)
                    ':user))
              "commands registered by user init retain their source"))
           (with-open-file (stream pathname
                                   :direction       ':output
                                   :if-exists       ':supersede
                                   :external-format ':utf-8)
             (write-string
              "(progn
                 (define-context-contributor user-init-tests--contributor
                     (request)
                   (declare (ignore request))
                   (make-context-contribution
                    :identifier \"user-init-transaction\"
                    :instruction \"new contributor\"))
                 (error \"broken user init\"))"
              stream))
           (test-assert
            (handler-case
                (progn
                  (user-init-load configuration)
                  nil)
              (user-init-error (condition)
                (and (equal (user-init-error-pathname condition) pathname)
                     (eq (user-init-error-layer condition) ':user)
                     (typep (user-init-error-cause condition)
                            'serious-condition))))
            "a broken user init signals a structured startup condition")
           (test-assert
            (find "user-init-test" (context-contributor-registrations)
                  :test #'string=
                  :key  (lambda (registration)
                          (getf registration :identifier)))
            "a failed reload restores the previous contributor registry")
           (let* ((registration
                    (find
                     "user-init-tests--contributor"
                     (context-contributor-registrations)
                     :test #'string=
                     :key  (lambda (candidate)
                             (getf candidate :identifier))))
                  (contribution
                    (and registration
                         (funcall (getf registration :function) nil))))
             (test-assert
              (and contribution
                   (string=
                    (context-contribution-instruction contribution)
                    "old contributor"))
              "a failed reload restores the exact previous contributor function"))
           (test-assert
            (application-command-find "/user-init-test")
            "a failed reload restores the previous command registry")
           (delete-file pathname)
           (user-init-load configuration)
           (test-assert
            (null (find ':user (context-contributor-registrations)
                        :key (lambda (registration)
                               (getf registration :source))))
            "removing init.lisp removes its stale contributor registrations")
           (test-assert
            (null
             (find ':user
                   (application-command--registrations)
                   :key (lambda (registration)
                          (getf registration :source))))
            "removing init.lisp removes its stale command registrations"))
      (setf *user-init-test-value* nil)
      (context--registry-restore context-registrations)
      (application-command--registry-restore command-registrations)
      (mcp--registry-restore mcp-registrations)
      (when (fboundp 'user-init-tests--command)
        (fmakunbound 'user-init-tests--command))
      (when (fboundp 'user-init-tests--contributor)
        (fmakunbound 'user-init-tests--contributor))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-local-source-tree-registration () null)
(defun test-local-source-tree-registration ()
  "Test user tree lookups resolve new systems without shadowing loaded ones."
  (let* ((root     (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (format nil "autolith-source-tree-~A/" (make-identifier))
                     (uiop:temporary-directory))))
         (checkout (merge-pathnames "fresh-system/" root))
         (buried   (merge-pathnames "fresh-system/_build/decoy/" root)))
    (unwind-protect
         (flet ((write-definition (pathname content)
                  (with-open-file (stream pathname
                                          :direction         ':output
                                          :if-exists         ':supersede
                                          :if-does-not-exist ':create)
                    (write-string content stream))))
           (ensure-directories-exist checkout)
           (ensure-directories-exist buried)
           (write-definition (merge-pathnames "fresh-system.asd" checkout)
                             "(asdf:defsystem #:fresh-system)")
           (write-definition (merge-pathnames "skipped-system.asd" buried)
                             "(asdf:defsystem #:skipped-system)")
           (write-definition (merge-pathnames "autolith.asd" checkout)
                             "(asdf:defsystem #:autolith :version \"0.0.0\")")
           (let* ((version-before  (asdf:component-version
                                    (asdf:find-system "autolith")))
                  (located         (main--locate-user-tree-system
                                    "fresh-system" (list root)))
                  (expected        (merge-pathnames "fresh-system.asd" checkout))
                  (located-status  (and located
                                        (platform-path-status *platform* located)))
                  (expected-status (platform-path-status *platform* expected)))
             (test-assert (and located-status
                               expected-status
                               (platform-file-status-same-object-p
                                located-status expected-status))
                          "user tree lookups find unregistered systems")
            (test-assert (null (main--locate-user-tree-system
                                "skipped-system" (list root)))
                         "user tree lookups skip build directories")
            (test-assert (null (main--locate-user-tree-system
                                "autolith" (list root)))
                         "user tree lookups never shadow registered systems")
            (main--register-local-source-trees)
            (test-assert (null (main--register-local-source-trees))
                         "source tree registration is idempotent and quiet")
            (test-assert (eq (first
                              (last asdf:*system-definition-search-functions*))
                             'main--locate-user-tree-system)
                         "user tree lookups run after every other ASDF search")
            (test-assert (string= (asdf:component-version
                                   (asdf:find-system "autolith"))
                                  version-before)
                         "registration keeps the loaded system authoritative")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-directory-user-init () null)
(defun test-directory-user-init ()
  "Test site, trusted directory, and user executable configuration layering."
  (let* ((site-container
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-site-init-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (site-root              (merge-pathnames "site/" site-container))
         (base-configuration
           (progn
             (ensure-directories-exist site-root)
             (test-configuration
              :site-config-root
              (uiop:ensure-directory-pathname
               (platform-truename *platform* site-root)))))
         (root                   (test-configuration-root base-configuration))
         (anchor                 (merge-pathnames "trusted/" root))
         (workspace              (merge-pathnames "project/" anchor))
         (configuration          nil)
         (site-init              (configuration-site-init-path base-configuration))
         (directory-init         (configuration-directory-init-path anchor))
         (global-init            (configuration-user-init-path base-configuration))
         (context-registrations  (context--registry-snapshot))
         (command-registrations  (application-command--registry-snapshot))
         (mcp-registrations      (mcp--registry-snapshot))
         (provider-registrations (provider--registry-snapshot)))
    (ensure-directories-exist workspace)
    (setf configuration
          (configuration-copy base-configuration :working-directory workspace))
    (test-directory-configuration--write-manifest
     configuration
     (list (namestring anchor)))
    (unwind-protect
         (progn
           (test-mcp-configuration--write
            site-init
            "(progn
               (push (list :site *user-init-layer*
                           *extension-registration-source*
                           *user-init-pathname*)
                     *directory-user-init-test-log*)
               (register-context-contributor
                \"site-user-init-test\"
                'context-tests--next-request)
               (register-context-contributor
                \"layered-user-init-test\"
                'context-tests--next-request))")
           (test-mcp-configuration--write
            directory-init
            "(progn
               (push (list :directory *user-init-layer*
                           *extension-registration-source*
                           *user-init-pathname*
                            (config :working-directory
                                    *user-init-configuration*))
                     *directory-user-init-test-log*)
               (defun directory-user-init-tests--definition () :loaded)
               (register-context-contributor
                \"directory-user-init-test\"
                'context-tests--next-request))")
           (test-mcp-configuration--write
            global-init
            "(progn
               (push (list :global *user-init-layer*
                           *extension-registration-source*
                           *user-init-pathname*)
                     *directory-user-init-test-log*)
               (register-context-contributor
                \"layered-user-init-test\"
                'context-tests--next-request))")
           (setf *directory-user-init-test-log* nil)
           (test-assert
            (equal (user-init-load configuration) global-init)
            "global init remains the final executable configuration layer")
           (test-assert
            (and (equal (mapcar #'first *directory-user-init-test-log*)
                        '(:global :directory :site))
                 (equal (mapcar #'second *directory-user-init-test-log*)
                        '(:user :directory :site))
                 (equal (mapcar #'third *directory-user-init-test-log*)
                        '(:user :user :site))
                 (equal (fourth (first *directory-user-init-test-log*))
                        global-init)
                 (equal (fourth (second *directory-user-init-test-log*))
                        directory-init)
                 (equal (fifth (second *directory-user-init-test-log*))
                        (platform-truename *platform* workspace)))
            "site, directory, and user init receive exact layer and source bindings")
           (test-assert
            (eq (getf
                 (find "site-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key  (lambda (registration)
                               (getf registration :identifier)))
                 :source)
                ':site)
            "site init registrations retain their lower-precedence source")
           (test-assert
            (eq (getf
                 (find "layered-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key  (lambda (registration)
                               (getf registration :identifier)))
                 :source)
                ':user)
            "the final user init overrides a colliding site registration")
           (test-assert
            (eq (directory-user-init-tests--definition) :loaded)
            "trusted directory init may redefine the live Lisp image")
           (test-mcp-configuration--write
            site-init
            "(progn
               (register-context-contributor
                \"site-user-init-test\"
                'context-tests--next-request)
               (error \"broken site init\"))")
           (test-assert
            (handler-case
                (progn
                  (user-init-load configuration)
                  nil)
              (user-init-error (condition)
                (and (eq (user-init-error-layer condition) ':site)
                     (equal (user-init-error-pathname condition) site-init))))
            "a broken site init identifies its exact configuration layer")
           (test-assert
            (eq (getf
                 (find "layered-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key  (lambda (registration)
                               (getf registration :identifier)))
                 :source)
                ':user)
            "a failed site reload restores the prior layered registry")
           (test-mcp-configuration--write
            site-init
            "(progn
               (register-context-contributor
                \"site-user-init-test\"
                'context-tests--next-request)
               (register-context-contributor
                \"layered-user-init-test\"
                'context-tests--next-request))")
           (delete-file global-init)
           (user-init-load base-configuration)
           (test-assert
            (null
             (find "directory-user-init-test"
                   (context-contributor-registrations)
                   :test #'string=
                   :key  (lambda (registration)
                           (getf registration :identifier))))
            "leaving a trusted scope removes its extension registrations")
           (test-assert
            (eq (getf
                 (find "layered-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key  (lambda (registration)
                               (getf registration :identifier)))
                 :source)
                ':site)
            "site configuration becomes effective when the user layer is absent")
           (delete-file site-init)
           (user-init-load base-configuration)
           (test-assert
            (null
             (find ':site
                   (context-contributor-registrations)
                   :key (lambda (registration)
                          (getf registration :source))))
            "removing site init removes its stale registrations")
           (test-assert
            (eq (directory-user-init-tests--definition) :loaded)
            "arbitrary directory init mutations remain sticky until reversed"))
      (setf *directory-user-init-test-log* nil)
      (context--registry-restore context-registrations)
      (application-command--registry-restore command-registrations)
      (mcp--registry-restore mcp-registrations)
      (provider--registry-restore provider-registrations)
      (when (fboundp 'directory-user-init-tests--definition)
        (fmakunbound 'directory-user-init-tests--definition))
      (platform-delete-directory-tree
       *platform* root :validate t :if-does-not-exist ':ignore)
      (platform-delete-directory-tree
       *platform* site-container :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-user-init-settings () null)
(defun test-user-init-settings ()
  "Test init files and the prompt read and change settings without naming a configuration."
  (with-test-configuration (configuration)
    (let ((pathname (configuration-user-init-path configuration)))
      (configuration-ensure-directories configuration)
      (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                       :external-format ':utf-8)
        (write-string "(setf (config :turn-timestamps-p) t
      (config :compaction-threshold-percent) 70)" stream))
      (user-init-load configuration)
      (test-assert (and (config :turn-timestamps-p configuration)
                        (= (config :compaction-threshold-percent configuration) 70))
                   "init.lisp changes the configuration it initializes")
      (let ((*active-application* (make-instance 'application :configuration configuration)))
        (test-assert (eql (config :compaction-threshold-percent) 70)
                     "config without an instance reads the running session")
        (setf (config :cache-miss-notices-p) t)
        (test-assert (config :cache-miss-notices-p configuration)
                     "setf config without an instance changes the running session"))
      (let ((*active-application* nil))
        (test-assert (handler-case (progn (config :model) nil)
                       (configuration-missing () t))
                     "config without any session signals a missing configuration"))))
  nil)


;;;; -- Private Startup Replay --

(-> user-init-tests--startup-definitions () list)
(defun user-init-tests--startup-definitions ()
  "Return private definitions for fresh-process startup and authentication checks."
  '((defvar *user-init-tests--replay-count* 0
      "Number of private replays in this fresh process.")
    (defun openai-compatible--authenticate (provider &key stream open-browser-p)
      "Intercept authentication before browser access or credential publication."
      (declare (ignore stream open-browser-p))
      (assert (string= (provider-account-label provider) "startup-private"))
      (assert (= *user-init-tests--replay-count* 1))
      "PRIVATE-STARTUP-AUTHENTICATED")
    (defparameter *user-init-tests--replay-lineage*
      (progn
        (incf *user-init-tests--replay-count*)
        (register-openai-compatible-provider
         :name     "startup-private"
         :endpoint "https://startup.invalid/v1/chat/completions"
         :models   '("startup-private/model"))
        *active-image-lineage-identifier*)
      "Lineage selected before private initializers ran.")
    (defun user-init-tests--register-provider ()
      "Register a provider through executable init after private replay."
      (register-openai-compatible-provider
       :name     "startup-init"
       :endpoint "https://startup.invalid/v1/chat/completions"
       :models   '("startup-init/model")))
    (defun world-client-run-script (configuration &rest options)
      "Check authentication's continuation into the world server without starting one."
      (declare (ignore configuration options))
      (assert (= *user-init-tests--replay-count* 1))
      (assert (equal *user-init-tests--replay-lineage*
                     *active-image-lineage-identifier*))
      (format t "PRIVATE-STARTUP-SESSION~%")
      nil)))

(-> user-init-tests--run-startup (configuration list)
    (values integer string))
(defun user-init-tests--run-startup (configuration arguments)
  "Run a real CLI with isolated user roots and return its status and output."
  (let ((root (test-configuration-root configuration)))
    (test-run-program-with-environment
     (active-image-process-command configuration arguments)
     (append
      (loop for (name key) in '(("XDG_CONFIG_HOME" :config-root)
                                ("XDG_DATA_HOME" :data-root)
                                ("XDG_STATE_HOME" :state-root)
                                ("XDG_CACHE_HOME" :cache-root))
            collect (format nil "~A=~A" name
                            (namestring (uiop:pathname-parent-directory-pathname
                                         (config key configuration)))))
      (list (format nil "HOME=~A" (namestring root))
            (format nil "CODEX_HOME=~A" (namestring root))
            "AUTOLITH_SITE_CONFIG_ROOT="
            (format nil "AUTOLITH_MODEL=~A" (config :model configuration))
            (format nil "AUTOLITH_REASONING_EFFORT=~A"
                    (config :reasoning-effort configuration))
            "AUTOLITH_TELEMETRY_ENABLED=false")))))

(-> test-private-startup-replay () null)
(defun test-private-startup-replay ()
  "Test selected private definitions before standalone models, init, and auth."
  (with-test-configuration (configuration)
    (let ((configuration
            (apply #'configuration-copy configuration
                   (loop for key in '(:config-root :data-root :state-root :cache-root)
                         append (list key (merge-pathnames "autolith/"
                                                           (config key configuration))))))
          (*active-image-commit-identifier*  nil)
          (*active-image-history-commit*     nil)
          (*active-image-lineage-identifier* nil)
          (*image-state-initialized-p*       nil)
          (*image-replay-skips*              nil))
      (configuration-ensure-directories configuration)
      (image-commit-publish
       configuration :title "Exercise private CLI startup"
       :additional-entries
       (loop for form in (user-init-tests--startup-definitions)
             collect (list :kind ':definition :id (make-identifier)
                           :target (definition-key form) :package "AUTOLITH"
                           :source (with-standard-io-syntax
                                     (let ((*package* (find-package :autolith)))
                                       (prin1-to-string form))))))
      (with-open-file (stream (configuration-user-init-path configuration)
                              :direction ':output :if-exists ':supersede)
        (write-line "(write-line \"PRIVATE-STARTUP-INIT\")" stream)
        (write-line "(user-init-tests--register-provider)" stream))
      (flet ((run (arguments &key pristine-p failure-p)
               "Check CLI output from one fresh process."
               (multiple-value-bind (status output)
                   (user-init-tests--run-startup configuration arguments)
                 (test-assert
                  (= status (if (and pristine-p (member "auth" arguments
                                                        :test #'string=))
                                64 0))
                  (format nil "~S exits with expected status: ~A" arguments output))
                 (cond
                   (pristine-p
                    (test-assert (not (search "PRIVATE-STARTUP-INIT" output))
                                 "pristine commands bypass executable init")
                    (when (member "models" arguments :test #'string=)
                      (test-assert (not (search "startup-private/model" output))
                                   "pristine models bypass private initializers")))
                   ((member "models" arguments :test #'string=)
                    (test-assert
                     (and (search "startup-private/model" output)
                          (search "startup-init/model" output))
                     "models includes private and init-registered providers"))
                   (t
                    (test-assert
                     (and (search "PRIVATE-STARTUP-AUTHENTICATED" output)
                          (search "PRIVATE-STARTUP-SESSION" output))
                     "auth uses private helpers and continues with one replay and lineage")))
                 (when (and failure-p (member "models" arguments :test #'string=))
                   (test-assert (search "startup-replay-sentinel" output)
                                "standalone models reports private replay failures")))))
        (run '("models"))
        (run '("--pristine" "models") :pristine-p t)
        (run '("auth" "startup-private"))
        (run '("--pristine" "auth" "startup-private") :pristine-p t)
        ;; Deliberately publish a broken replay after the working definitions.
        (let ((*image-commit-replay-probe-function*
                (lambda (configuration script identifier)
                  (declare (ignore configuration script identifier))
                  nil)))
          (image-commit-publish
           configuration :title "Exercise startup replay diagnostics"
           :additional-entries
           (list (list :kind ':legacy :id (make-identifier)
                       :target "startup-failure"
                       :source "(error \"startup-replay-sentinel\")"))))
        (run '("models") :failure-p t)
        (run '("auth" "startup-private") :failure-p t))))
  nil)


;;;; -- Command-line Model Overrides --

(-> test-cli-model-overrides () null)
(defun test-cli-model-overrides ()
  "Exercise launch overrides through interactive and headless command handlers."
  (with-test-configuration (fixture root)
    (let ((fixture              (apply #'configuration-copy fixture
                                       (loop for key in '(:config-root :data-root :state-root :cache-root)
                                             append (list key (merge-pathnames "autolith/"
                                                                               (config key fixture))))))
          (snapshot             (provider--registry-snapshot))
          (*active-application* nil)
          (observed             nil))
      (unwind-protect
           (progn
             (configuration-ensure-directories fixture)
             (preferences-store fixture :model *default-model*)
             (preferences-store fixture :model-provider "chatgpt")
             (register-openai-compatible-provider
              :name            "cli-custom"
              :endpoint        "http://localhost:9999/v1/chat/completions"
              :models-endpoint "http://localhost:9999/v1/models")
             (openai-compatible-provider-tests--save-key fixture "cli-custom" "synthetic-cli-key")
             (test-call-with-function-replacements
              (list (list 'dexador:get
                          (lambda (url &rest arguments)
                            (declare (ignore url arguments))
                            (values "{\"data\":[{\"id\":\"hf:custom/model\"}]}"
                                    200 nil nil))))
              (lambda ()
                (test-assert (null (provider-refresh-models fixture :provider-name "cli-custom"))
                             "the custom catalog is cached before CLI startup")))
             (unregister-provider "cli-custom" :source ':runtime)
             (with-open-file (stream (configuration-user-init-path fixture)
                                     :direction ':output :if-exists ':supersede)
               (write-line
                "(register-openai-compatible-provider :name \"cli-custom\" :endpoint \"http://localhost:9999/v1/chat/completions\" :models-endpoint \"http://localhost:9999/v1/models\")"
                stream))
             (let ((saved (uiop:read-file-string (configuration-preferences-path fixture))))
               (with-test-environment
                   (("XDG_CONFIG_HOME" (namestring (uiop:pathname-parent-directory-pathname
                                                    (config :config-root fixture))))
                    ("XDG_DATA_HOME" (namestring (uiop:pathname-parent-directory-pathname
                                                  (config :data-root fixture))))
                    ("XDG_STATE_HOME" (namestring (uiop:pathname-parent-directory-pathname
                                                   (config :state-root fixture))))
                    ("XDG_CACHE_HOME" (namestring (uiop:pathname-parent-directory-pathname
                                                   (config :cache-root fixture))))
                    ("AUTOLITH_SITE_CONFIG_ROOT" nil)
                    ("AUTOLITH_MODEL" *default-model*)
                    ("AUTOLITH_MODEL_PROVIDER" "chatgpt"))
                 (flet ((prepare (configuration)
                          (test-assert (configuration-validation-deferred-p configuration)
                                       "CLI model validation waits for executable init")
                          (user-init-load configuration)
                          (setf observed (provider-bootstrap-configuration configuration))
                          (make-instance 'application :configuration observed)))
                   (test-call-with-function-replacements
                    (list
                     (list 'dexador:get
                           (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error "CLI startup must use the cached provider catalog.")))
                     (list 'main--client-session-p (constantly nil))
                     (list 'main--connect-application
                           (lambda (&key configuration &allow-other-keys)
                             (prepare configuration)))
                     (list 'application-run
                           (lambda (application &rest arguments)
                             (declare (ignore application arguments))))
                     (list 'run-job-run
                           (lambda (input output mode &key configuration)
                             (declare (ignore input output mode))
                             (prepare configuration)
                             0)))
                    (lambda ()
                      (dolist (arguments
                               '(("--model" "cli-custom/hf:custom/model")
                                 ("--model" "hf:custom/model" "--provider" "cli-custom")
                                 ("run-job" "--model" "cli-custom/hf:custom/model"
                                  "--input" "job.sexp" "--output" "result.sexp")
                                 ("--provider" "cli-custom" "--model" "hf:custom/model"
                                  "run-job" "--input" "job.sexp" "--output" "result.sexp")))
                        (setf observed nil)
                        (main-dispatch arguments)
                        (test-assert
                         (and observed
                              (equal (config :model observed) "hf:custom/model")
                              (equal (config :model-provider observed) "cli-custom"))
                         (format nil "~S selects the custom provider after init" arguments))
                        (let* ((conversation (conversation-create observed))
                               (copied       (configuration-copy observed)))
                          (setf (conversation-model conversation) *default-model*
                                (conversation-model-provider conversation) "chatgpt")
                          (test-assert
                           (eq observed (application--configuration-for-conversation
                                         observed conversation))
                           "the CLI selection beats the saved conversation model")
                          (test-assert (config :model-selection-override-p copied)
                                       "configuration copies retain the launch override"))
                        (let* ((handoff   (merge-pathnames "handoff.sexp" root))
                               (arguments (localgroup-handoff--arguments observed handoff))
                               (parsed    (parse-command-line (main--top-level-command)
                                                              (member "--permissions" arguments :test #'equal))))
                          (test-assert
                           (equal (getopt* parsed ':model) "hf:custom/model")
                           "detached session arguments carry the launch model")
                          (test-assert (equal (getopt* parsed ':provider) "cli-custom")
                                       "detached session arguments carry provider identity")))))))
               (test-assert
                (equal saved (uiop:read-file-string (configuration-preferences-path fixture)))
                "launch overrides do not rewrite saved preferences")))
        (provider--registry-restore snapshot))))
  nil)

(-> test-cli-model-override-errors () null)
(defun test-cli-model-override-errors ()
  "Reject missing, empty, unknown, and incompatible model selections."
  (dolist (arguments '(("--model") ("--provider")
                       ("--model" "") ("--model" "   ")
                       ("--provider" "custom")
                       ("--model" "example" "--provider" "")
                       ("run-job" "--model")
                       ("--model" "--provider" "custom")
                       ("--modle" "example")))
    (test-assert
     (handler-case
         (progn
           (main--model-arguments (parse-command-line (main--top-level-command) arguments))
           nil)
       (error () t))
     (format nil "invalid CLI selection ~S is rejected" arguments)))
  (provider-selection-tests--with-providers
   (lambda ()
     (with-test-configuration (fixture)
       (dolist (pair '(("missing" "selection-a") ("shared" "unknown")
                       ("selection-a/shared" "selection-b")))
         (test-assert
          (handler-case
              (progn
                (provider-bootstrap-configuration
                 (configuration-copy fixture :model                 (first pair)
                                             :model-provider        (second pair)
                                             :provider-validation-p nil))
                nil)
            (error () t))
          (format nil "unavailable model/provider ~S is rejected after bootstrap" pair))))))
  nil)
