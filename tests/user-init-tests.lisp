(in-package #:autolith)

;;;; -- User Initialization Tests --

(defvar *user-init-test-value* nil
  "The value installed by the isolated user initialization fixture.")

(defvar *directory-user-init-test-log* nil
  "The load evidence installed by trusted directory initialization fixtures.")

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
           (let ((version-before (asdf:component-version
                                  (asdf:find-system "autolith"))))
             (test-assert (equal (main--locate-user-tree-system
                                  "fresh-system" (list root))
                                 (merge-pathnames "fresh-system.asd" checkout))
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
  "Test trusted executable inheritance and sticky full-power mutations."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (anchor (merge-pathnames "trusted/" root))
         (workspace (merge-pathnames "project/" anchor))
         (configuration nil)
         (directory-init (configuration-directory-init-path anchor))
         (global-init (configuration-user-init-path base-configuration))
         (context-registrations (context--registry-snapshot))
         (command-registrations (application-command--registry-snapshot))
         (mcp-registrations (mcp--registry-snapshot)))
    (ensure-directories-exist workspace)
    (setf configuration
          (configuration-with-working-directory base-configuration workspace))
    (test-directory-configuration--write-manifest
     configuration
     (list (namestring anchor)))
    (unwind-protect
         (progn
           (test-mcp-configuration--write
            directory-init
            "(progn
               (push (list :directory *user-init-pathname*
                           (configuration-working-directory
                            *user-init-configuration*))
                     *directory-user-init-test-log*)
               (defun directory-user-init-tests--definition () :loaded)
               (register-context-contributor
                \"directory-user-init-test\"
                'context-tests--next-request))")
           (test-mcp-configuration--write
            global-init
            "(push (list :global *user-init-pathname*)
                   *directory-user-init-test-log*)")
           (setf *directory-user-init-test-log* nil)
           (test-assert
            (equal (user-init-load configuration) global-init)
            "global init remains the final executable configuration layer")
           (test-assert
            (and (eq (first (first *directory-user-init-test-log*)) :global)
                 (eq (first (second *directory-user-init-test-log*)) :directory)
                 (equal (second (second *directory-user-init-test-log*))
                        directory-init)
                 (equal (third (second *directory-user-init-test-log*))
                        (truename workspace)))
            "trusted directory init receives its path and target configuration")
           (test-assert
            (eq (directory-user-init-tests--definition) :loaded)
            "trusted directory init may redefine the live Lisp image")
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
            (eq (directory-user-init-tests--definition) :loaded)
            "arbitrary directory init mutations remain sticky until reversed"))
      (setf *directory-user-init-test-log* nil)
      (context--registry-restore context-registrations)
      (application-command--registry-restore command-registrations)
      (mcp--registry-restore mcp-registrations)
      (when (fboundp 'directory-user-init-tests--definition)
        (fmakunbound 'directory-user-init-tests--definition))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
