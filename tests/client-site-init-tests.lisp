(in-package #:autolith)

;;;; -- Terminal Client Site Initialization Tests --

(defvar *client-site-init-tests-log* nil
  "The site initialization evidence recorded by the client fixture.")

(defvar *client-site-init-tests-user-loaded-p* nil
  "Whether the personal initialization file of the client fixture ran.")

(-> client-site-init-tests--write (pathname string) null)
(defun client-site-init-tests--write (pathname content)
  "Write CONTENT to PATHNAME as UTF-8 text."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction         ':output
                          :if-exists         ':supersede
                          :if-does-not-exist ':create
                          :external-format   ':utf-8)
    (write-string content stream))
  nil)

(defparameter *client-site-init-tests--site-source*
  "(setf *client-site-init-tests-log*
      (list *user-init-layer*
            *user-init-pathname*
            *user-init-configuration*
            (eq *configuration* *user-init-configuration*)))
(define-context-contributor client-site-init-tests--contributor
    (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier \"client-site-init-tests-site\"
   :instruction \"site contributor\"))
(let ((theme (make-terminal-theme
              :name        :client-site-init-tests
              :style-table (terminal-style-table-autolith))))
  (setf *terminal-themes*
        (cons theme
              (remove :client-site-init-tests *terminal-themes*
                      :key #'terminal-theme-name)))
  (terminal-theme-install :client-site-init-tests))
"
  "The site initialization file the client fixtures load.")

(defparameter *client-site-init-tests--failing-source*
  "(define-context-contributor client-site-init-tests--failure
    (request)
  (declare (ignore request))
  (make-context-contribution
   :identifier \"client-site-init-tests-failure\"
   :instruction \"failing contributor\"))
(error \"broken site initialization\")
"
  "The site initialization file that must degrade the client.")

(-> client-site-init-tests--site-configuration (configuration pathname) configuration)
(defun client-site-init-tests--site-configuration (fixture site-root)
  "Return a copy of FIXTURE using the existing SITE-ROOT."
  (ensure-directories-exist site-root)
  (configuration-copy fixture
                      :site-config-root
                      (uiop:ensure-directory-pathname
                       (platform-truename *platform* site-root))
                      :provider-validation-p nil))

(-> client-site-init-tests--registration (string) (option list))
(defun client-site-init-tests--registration (identifier)
  "Return the contributor registration named IDENTIFIER, when present."
  (find identifier (context-contributor-registrations)
        :test #'string=
        :key  (lambda (registration)
                (getf registration :identifier))))

(-> client-site-init-tests--call-with-preserved-state (function) t)
(defun client-site-init-tests--call-with-preserved-state (function)
  "Call FUNCTION with the theme and extension registries restored afterward."
  (let ((context-snapshot  (context--registry-snapshot))
        (command-snapshot  (application-command--registry-snapshot))
        (mcp-snapshot      (mcp--registry-snapshot))
        (provider-snapshot (provider--registry-snapshot))
        (themes            *terminal-themes*)
        (theme             *terminal-theme*)
        (style-table       *terminal-style-table*))
    (unwind-protect
         (funcall function)
      (context--registry-restore context-snapshot)
      (application-command--registry-restore command-snapshot)
      (mcp--registry-restore mcp-snapshot)
      (provider--registry-restore provider-snapshot)
      (setf *terminal-themes*      themes
            *terminal-theme*       theme
            *terminal-style-table* style-table))))

(-> test-client-site-init-load () null)
(defun test-client-site-init-load ()
  "Test the terminal client loads only the site layer and replaces it on reload."
  (with-test-configuration (fixture root)
    (client-site-init-tests--call-with-preserved-state
     (lambda ()
       (setf *client-site-init-tests-log*        nil
             *client-site-init-tests-user-loaded-p* nil)
       (let* ((site-root     (merge-pathnames "site/" root))
              (site-init     (merge-pathnames "init.lisp" site-root))
              (configuration (client-site-init-tests--site-configuration
                              fixture site-root))
              (user-function nil))
         (client-site-init-tests--write site-init
                                        *client-site-init-tests--site-source*)
         (client-site-init-tests--write
          (configuration-user-init-path configuration)
          "(setf *client-site-init-tests-user-loaded-p* t)")
         (terminal-theme-install :autolith)
         (test-assert (equal (user-init-load-site configuration)
                             (configuration-site-init-path configuration))
                      "the client site load returns the site init pathname")
         (test-assert (eq (first *client-site-init-tests-log*) ':site)
                      "the site file runs as the site layer")
         (test-assert (equal (second *client-site-init-tests-log*)
                             (configuration-site-init-path configuration))
                      "the site file sees its own pathname as the current initialization")
         (test-assert (eq (third *client-site-init-tests-log*) configuration)
                      "the site file sees the client configuration")
         (test-assert (fourth *client-site-init-tests-log*)
                      "the site file sees that configuration as *configuration*")
         (test-assert (eq (terminal-theme-name *terminal-theme*)
                          :client-site-init-tests)
                      "a site theme replaces the configured stock theme")
         (test-assert (null *client-site-init-tests-user-loaded-p*)
                      "the client does not load personal initialization")
         (let ((*extension-registration-source* :user))
           (define-context-contributor client-site-init-tests--user-contributor
               (request)
             (declare (ignore request))
             (make-context-contribution
              :identifier  "client-site-init-tests-user"
              :instruction "user contributor")))
         (setf user-function
               (getf (client-site-init-tests--registration
                      "client-site-init-tests--user-contributor")
                     :function))
         (test-assert user-function
                      "the user registration exists before the site reload")
         (test-assert (user-init-load-site configuration)
                      "the client site layer reloads")
         (test-assert (= 1 (count :client-site-init-tests *terminal-themes*
                                  :key #'terminal-theme-name))
                      "a site reload replaces its theme instead of stacking it")
         (test-assert (= 1 (count "client-site-init-tests--contributor"
                                  (context-contributor-registrations)
                                  :test #'string=
                                  :key  (lambda (registration)
                                          (getf registration :identifier))))
                      "a site reload replaces its registrations")
         (test-assert (eq (getf (client-site-init-tests--registration
                                 "client-site-init-tests--user-contributor")
                                :function)
                          user-function)
                      "a site-only load keeps personal registrations")
         (test-assert (null *client-site-init-tests-user-loaded-p*)
                      "a site reload still does not load personal initialization")))))
  nil)

(-> test-client-site-init-failure () null)
(defun test-client-site-init-failure ()
  "Test a failing site file reports once and leaves the stock presentation."
  (with-test-configuration (fixture root)
    (client-site-init-tests--call-with-preserved-state
     (lambda ()
       (setf *client-site-init-tests-log*        nil
             *client-site-init-tests-user-loaded-p* nil)
       (let* ((site-root     (merge-pathnames "site/" root))
              (site-init     (merge-pathnames "init.lisp" site-root))
              (output        (make-string-output-stream))
              (configuration (client-site-init-tests--site-configuration
                              fixture site-root)))
         (client-site-init-tests--write site-init
                                        *client-site-init-tests--failing-source*)
         (terminal-theme-install :autolith)
         (test-assert (null (user-init-load-site configuration :stream output))
                      "a failing site layer does not abort the client")
         (let ((report (get-output-stream-string output)))
           (test-assert (search "site initialization failed" report)
                        "a failing site layer is reported")
           (test-assert (search (namestring
                                 (configuration-site-init-path configuration))
                                report)
                        "the report names the failing site file"))
         (test-assert (eq (terminal-theme-name *terminal-theme*) :autolith)
                      "a failing site layer keeps the configured theme")
         (test-assert (null (client-site-init-tests--registration
                             "client-site-init-tests-failure"))
                      "a failing site layer rolls its registrations back")
         (test-assert (null *client-site-init-tests-log*)
                      "a failing site layer records no successful evidence")))))
  nil)

(-> test-client-site-init-policy () null)
(defun test-client-site-init-policy ()
  "Test which processes load the site layer themselves."
  (with-test-configuration (fixture root)
    (let* ((site-root     (merge-pathnames "site/" root))
           (configuration (client-site-init-tests--site-configuration
                           fixture site-root)))
      (test-assert (main--client-site-init-p configuration)
                   "an ordinary terminal client with a site root loads the site layer")
      (test-assert (not (main--client-site-init-p fixture))
                   "a client without a site root loads no site layer")
      (test-assert (not (main--client-site-init-p configuration :pristine-p t))
                   "a pristine recovery skips the site layer")
      (test-assert (not (main--client-site-init-p configuration :server-p t))
                   "a detached world server skips the client site layer")))
  nil)

(-> test-client-site-init-server-arguments () null)
(defun test-client-site-init-server-arguments ()
  "Test AUTOLITH_SITE_CONFIG_ROOT reaches the detached world server arguments."
  (with-test-configuration (fixture root)
    (let ((site-root (merge-pathnames "environment-site/" root))
          (handoff   (merge-pathnames "handoff.sexp" root)))
      (ensure-directories-exist site-root)
      (with-test-environment
          (("AUTOLITH_SITE_CONFIG_ROOT" (namestring site-root))
           ("XDG_CONFIG_HOME" (namestring (config :config-root fixture)))
           ("XDG_DATA_HOME"   (namestring (config :data-root fixture)))
           ("XDG_STATE_HOME"  (namestring (config :state-root fixture)))
           ("XDG_CACHE_HOME"  (namestring (config :cache-root fixture))))
        (let* ((configuration
                 (configuration-create
                  :source-root                 (config :source-root fixture)
                  :working-directory           (config :working-directory fixture)
                  :defer-provider-validation-p t))
               (expected  (uiop:ensure-directory-pathname
                           (platform-truename *platform* site-root)))
               (arguments (world-startup--arguments
                           configuration handoff :permission-mode ':auto)))
          (test-assert (equal (config :site-config-root configuration) expected)
                       "the environment supplies the client site root")
          (test-assert (equal
                        (second (member "--site-config-root" arguments
                                        :test #'string=))
                        (namestring expected))
                       "the detached world server arguments carry the site root")))))
  nil)

(-> test-client-site-init-start-session () null)
(defun test-client-site-init-start-session ()
  "Test client startup loads the site layer before the UI and skips it when not a client."
  (with-test-configuration (fixture root)
    (client-site-init-tests--call-with-preserved-state
     (lambda ()
       (setf *client-site-init-tests-log*        nil
             *client-site-init-tests-user-loaded-p* nil)
       (let* ((site-root     (merge-pathnames "site/" root))
              (site-init     (merge-pathnames "init.lisp" site-root))
              (configuration (client-site-init-tests--site-configuration
                              fixture site-root))
              (handoff       (merge-pathnames "handoff.sexp" root))
              (observed      nil)
              (server-observed nil)
              (handoff-p     nil))
         (client-site-init-tests--write site-init
                                        *client-site-init-tests--site-source*)
         (flet ((run (arguments)
                  (setf observed nil
                        server-observed nil)
                  (test-call-with-function-replacements
                   (list
                    (list 'main--register-local-source-trees (lambda () nil))
                    (list 'configuration-create
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            configuration))
                    (list 'application-recovery-state
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (values nil nil)))
                    (list 'application-recovery-diagnosis-prompt
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            nil))
                    (list 'application--command-sandbox-available-p (lambda () t))
                    (list 'world-startup-selection
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (when handoff-p '(:world-startup))))
                    (list 'world-startup-begin-startup
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            nil))
                    (list 'world-server-run
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (setf server-observed *client-site-init-tests-log*)
                            nil))
                    (list 'main--interactive-input-p (lambda () t))
                    (list 'main--run-world-client
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (setf observed
                                  (list *client-site-init-tests-log*
                                        (terminal-theme-name *terminal-theme*)))
                            nil)))
                   (lambda ()
                     (main--start-session
                      (parse-command-line (main--top-level-command) arguments))))))
           (run nil)
           (test-assert (eq (first (first observed)) ':site)
                        "bare startup loads the site layer before the UI")
           (test-assert (eq (second observed) :client-site-init-tests)
                        "bare startup installs the site theme before the UI")
           (setf *client-site-init-tests-log* nil)
           (terminal-theme-install :autolith)
           (run '("--pristine"))
           (test-assert (null (first observed))
                        "pristine startup skips the site layer")
           (setf handoff-p t)
           (run (list "--world-startup" (namestring handoff)))
           (test-assert (null server-observed)
                        "the detached server startup skips the client site layer"))))))
  nil)
