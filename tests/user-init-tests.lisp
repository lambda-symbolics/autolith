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
         (site-root (merge-pathnames "site/" root))
         (environment-site-root (merge-pathnames "environment-site/" root))
         (missing-root (merge-pathnames "missing/" root))
         (source-root (asdf:system-source-directory :autolith))
         (previous-site-root (uiop:getenv "AUTOLITH_SITE_CONFIG_ROOT")))
    (ensure-directories-exist site-root)
    (ensure-directories-exist environment-site-root)
    (unwind-protect
         (progn
           (platform-setenv "AUTOLITH_SITE_CONFIG_ROOT"
                            (namestring environment-site-root))
           (let ((environment-configuration
                   (configuration-create
                    :source-root source-root
                    :working-directory source-root
                    :defer-provider-validation-p t)))
             (test-assert
              (equal
               (configuration-site-config-root environment-configuration)
               (uiop:ensure-directory-pathname
                  (platform-truename *platform* environment-site-root)))
              "the site root defaults from AUTOLITH_SITE_CONFIG_ROOT"))
           (let ((configuration
                   (configuration-create
                    :source-root source-root
                    :working-directory source-root
                    :site-config-root site-root
                    :defer-provider-validation-p t)))
             (test-assert
              (equal (configuration-site-config-root configuration)
                     (uiop:ensure-directory-pathname
                       (platform-truename *platform* site-root)))
              "an explicit site root overrides the environment default")
             (test-assert
              (equal (configuration-site-init-path configuration)
                     (merge-pathnames "init.lisp"
                                      (configuration-site-config-root
                                       configuration)))
              "the site init path is derived without creating the file")
             (test-assert
              (equal (configuration-site-config-root
                      (configuration--clone configuration :immutable-p t))
                     (configuration-site-config-root configuration))
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
                     :source-root source-root
                     :working-directory source-root
                     :site-config-root #P"relative-site/"
                     :defer-provider-validation-p t)
                    nil)
                (configuration-error ()
                  t))
              "a relative explicit site root is rejected")
             (test-assert
              (handler-case
                  (progn
                    (configuration-create
                     :source-root source-root
                     :working-directory source-root
                     :site-config-root missing-root
                     :defer-provider-validation-p t)
                    nil)
                (configuration-error ()
                  t))
              "a missing explicit site root is rejected")
             (platform-unsetenv "AUTOLITH_SITE_CONFIG_ROOT")
             (let ((without-site
                     (configuration-create
                      :source-root source-root
                      :working-directory source-root
                      :defer-provider-validation-p t)))
               (test-assert
                (and (null (configuration-site-config-root without-site))
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
    (let ((site-root (merge-pathnames "site/" root))
          (environment-root (merge-pathnames "environment-site/" root)))
      (ensure-directories-exist site-root)
      (ensure-directories-exist environment-root)
      (with-test-environment
          (("XDG_CONFIG_HOME" (namestring (configuration-config-root fixture)))
           ("XDG_DATA_HOME" (namestring (configuration-data-root fixture)))
           ("XDG_STATE_HOME" (namestring (configuration-state-root fixture)))
           ("XDG_CACHE_HOME" (namestring (configuration-cache-root fixture))))
        (dolist (arguments '(("models")
                             ("run-job" "--input" "input.sexp"
                                        "--output" "output.sexp")))
          (dolist (selection '(:explicit :environment :override :absent))
            (let ((observed nil)
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
                       (configuration-site-config-root observed)
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

(-> test-user-init () null)
(defun test-user-init ()
  "Test user initialization discovery, package binding, and typed failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-user-init-path configuration))
         (context-registrations (context--registry-snapshot))
         (command-registrations (application-command--registry-snapshot))
         (mcp-registrations (mcp--registry-snapshot)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-assert (null (user-init-load configuration))
                        "a missing user init is an ordinary empty configuration")
           (with-open-file (stream pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
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
                            :key (lambda (registration)
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
                                   :direction ':output
                                   :if-exists ':supersede
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
                  :key (lambda (registration)
                         (getf registration :identifier)))
            "a failed reload restores the previous contributor registry")
           (let* ((registration
                    (find
                     "user-init-tests--contributor"
                     (context-contributor-registrations)
                     :test #'string=
                     :key (lambda (candidate)
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
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "autolith-source-tree-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (checkout (merge-pathnames "fresh-system/" root))
         (buried (merge-pathnames "fresh-system/_build/decoy/" root)))
    (unwind-protect
         (flet ((write-definition (pathname content)
                  (with-open-file (stream pathname
                                          :direction ':output
                                          :if-exists ':supersede
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
            (let* ((version-before (asdf:component-version
                                    (asdf:find-system "autolith")))
                   (located (main--locate-user-tree-system
                             "fresh-system" (list root)))
                   (expected (merge-pathnames "fresh-system.asd" checkout))
                   (located-status (and located
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
         (site-root (merge-pathnames "site/" site-container))
         (base-configuration
           (progn
             (ensure-directories-exist site-root)
             (test-configuration
              :site-config-root
              (uiop:ensure-directory-pathname
               (platform-truename *platform* site-root)))))
         (root (test-configuration-root base-configuration))
         (anchor (merge-pathnames "trusted/" root))
         (workspace (merge-pathnames "project/" anchor))
         (configuration nil)
         (site-init (configuration-site-init-path base-configuration))
         (directory-init (configuration-directory-init-path anchor))
         (global-init (configuration-user-init-path base-configuration))
         (context-registrations (context--registry-snapshot))
         (command-registrations (application-command--registry-snapshot))
         (mcp-registrations (mcp--registry-snapshot))
         (provider-registrations (provider--registry-snapshot)))
    (ensure-directories-exist workspace)
    (setf configuration
          (configuration-with-working-directory base-configuration workspace))
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
                           (configuration-working-directory
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
                       :key (lambda (registration)
                              (getf registration :identifier)))
                 :source)
                ':site)
            "site init registrations retain their lower-precedence source")
           (test-assert
            (eq (getf
                 (find "layered-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key (lambda (registration)
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
                       :key (lambda (registration)
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
                   :key (lambda (registration)
                          (getf registration :identifier))))
            "leaving a trusted scope removes its extension registrations")
           (test-assert
            (eq (getf
                 (find "layered-user-init-test"
                       (context-contributor-registrations)
                       :test #'string=
                       :key (lambda (registration)
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
