(in-package #:autolith)

;;;; -- Directory-Scoped Configuration Tests --

(-> test-directory-configuration--write-manifest
    (configuration list)
    pathname)
(defun test-directory-configuration--write-manifest (configuration directories)
  "Write DIRECTORY trust anchors for CONFIGURATION and return the manifest."
  (test-mcp-configuration--write
   (configuration-directory-scopes-path configuration)
   (with-output-to-string (stream)
     (let ((*print-readably* nil))
       (write (list :version 1 :directories directories) :stream stream)))))

(-> test-directory-configuration--write-mcp
    (pathname list)
    pathname)
(defun test-directory-configuration--write-mcp (directory servers)
  "Write SERVERS beneath trusted DIRECTORY and return the native MCP file."
  (test-mcp-configuration--write
   (configuration-directory-mcp-path directory)
   (with-output-to-string (stream)
     (let ((*print-readably* nil))
       (write (list :version 1 :servers servers) :stream stream)))))

(-> test-directory-configuration--server-form
    (string keyword)
    list)
(defun test-directory-configuration--server-form (name approval)
  "Return one independently printable native MCP server form."
  (test-mcp-configuration--server-form
   :name name
   :transport (list :type :stdio :command (copy-seq "/bin/true"))
   :approval approval))

(-> test-directory-configuration--registration (string) mcp-server-registration)
(defun test-directory-configuration--registration (name)
  "Return the effective MCP registration named NAME."
  (or
   (find name
         (mcp-server-registrations)
         :test #'string=
         :key
         (lambda (registration)
           (mcp-server-configuration-name
            (mcp-server-registration-configuration registration))))
   (error "Missing effective MCP registration ~S." name)))

(-> test-directory-configuration () null)
(defun test-directory-configuration ()
  "Test explicit trust, ancestor inheritance, precedence, and rollback."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (anchor (merge-pathnames "braiins/" root))
         (project (merge-pathnames "pool-tools/project/" anchor))
         (nested-anchor (merge-pathnames "pool-tools/" anchor))
         (outside (merge-pathnames "outside/" root))
         (registry-snapshot (mcp--registry-snapshot)))
    (ensure-directories-exist project)
    (ensure-directories-exist outside)
    (unwind-protect
         (let* ((configuration
                  (configuration-with-working-directory
                   base-configuration project))
                (outside-configuration
                  (configuration-with-working-directory
                   base-configuration outside))
                (global-server
                  (test-directory-configuration--server-form
                   "global-only" :prompt))
                (global-shared
                  (test-directory-configuration--server-form
                   "shared" :prompt))
                (directory-server
                  (test-directory-configuration--server-form
                   "directory-only" :allow))
                (directory-shared
                  (test-directory-configuration--server-form
                   "shared" :deny))
                (untrusted-server
                  (test-directory-configuration--server-form
                   "untrusted" :allow)))
           (test-mcp-configuration--write
            (configuration-mcp-path configuration)
            (with-output-to-string (stream)
              (let ((*print-readably* nil))
                (write (list :version 1
                             :servers (list global-server global-shared))
                       :stream stream))))
           (test-directory-configuration--write-manifest
            configuration
            (list (namestring anchor)))
           (test-directory-configuration--write-mcp
            anchor
            (list directory-server directory-shared))
           (test-directory-configuration--write-mcp
            project
            (list untrusted-server))
           (test-assert
            (equal (directory-configuration-active-directories configuration)
                   (list (truename anchor)))
            "a trusted parent directory applies to every descendant workspace")
           (test-assert
            (equal (directory-configuration-active-mcp-paths configuration)
                   (list (configuration-directory-mcp-path (truename anchor))))
            "only explicitly trusted anchors contribute inherited MCP files")
           (mcp-configuration-load configuration)
           (let ((global
                   (test-directory-configuration--registration "global-only"))
                 (directory
                   (test-directory-configuration--registration "directory-only"))
                 (shared
                   (test-directory-configuration--registration "shared")))
             (test-assert
              (eq (mcp-server-registration-source global) :config)
              "global MCP definitions retain the config source")
             (test-assert
              (eq (mcp-server-registration-source directory) :directory)
              "inherited MCP definitions expose their directory source")
             (test-assert
              (and (eq (mcp-server-registration-source shared) :directory)
                   (eq (test-mcp-configuration--effective-approval "shared")
                       :deny))
              "directory MCP definitions override same-name global definitions"))
           (test-assert
            (not
             (find "untrusted"
                   (mcp-server-registrations)
                   :test #'string=
                   :key
                   (lambda (registration)
                     (mcp-server-configuration-name
                      (mcp-server-registration-configuration registration)))))
            "an untrusted descendant cannot activate its own MCP file")
           (test-directory-configuration--write-manifest
            configuration
            (list (namestring anchor) (namestring nested-anchor)))
           (test-directory-configuration--write-mcp
            nested-anchor
            (list
             (test-directory-configuration--server-form
              "shared" :read-only)))
           (test-assert
            (equal
             (directory-configuration-active-directories configuration)
             (list (truename anchor) (truename nested-anchor)))
            "multiple trusted ancestors apply from outermost to nearest")
           (mcp-configuration-load configuration)
           (test-assert
            (eq (test-mcp-configuration--effective-approval "shared")
                :read-only)
            "the nearest trusted ancestor wins within the directory layer")
           (mcp-configuration-load outside-configuration)
           (test-assert
            (not
             (find :directory
                   (mcp-server-registrations)
                   :key #'mcp-server-registration-source))
            "leaving trusted ancestors removes inherited registrations")
           (mcp-configuration-load configuration)
           (let ((before (mcp--registry-snapshot)))
             (test-mcp-configuration--write
              (configuration-directory-mcp-path nested-anchor)
              "(:version 99 :servers ())")
             (test-assert
              (handler-case
                  (progn
                    (mcp-configuration-load configuration)
                    nil)
                (mcp-configuration-error (condition)
                  (equal
                   (mcp-configuration-error-pathname condition)
                   (configuration-directory-mcp-path nested-anchor))))
              "a malformed inherited file identifies its exact pathname")
             (test-assert
              (equal (mcp--registry-snapshot) before)
              "a malformed inherited file leaves registrations unchanged")))
      (mcp--registry-restore registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
