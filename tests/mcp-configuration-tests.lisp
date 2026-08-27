(in-package #:autolith)

;;;; -- Native Configuration Tests --

(-> test-mcp-configuration--write (pathname string) pathname)
(defun test-mcp-configuration--write (pathname contents)
  "Write native MCP test CONTENTS to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create)
    (write-string contents stream)
    (finish-output stream))
  pathname)

(-> test-mcp-configuration--signals-p
    (configuration string)
    boolean)
(defun test-mcp-configuration--signals-p (configuration contents)
  "Return true when native MCP CONTENTS fail closed for CONFIGURATION."
  (test-mcp-configuration--write
   (configuration-mcp-path configuration)
   contents)
  (handler-case
      (progn
        (mcp-configuration-read configuration)
        nil)
    (mcp-configuration-error ()
      t)))

(-> test-mcp-configuration--signals-form-p (configuration t) boolean)
(defun test-mcp-configuration--signals-form-p (configuration form)
  "Return true when native MCP FORM fails closed for CONFIGURATION."
  (test-mcp-configuration--signals-p
   configuration
   (with-output-to-string (stream)
     (let ((*print-readably* t))
       (write form :stream stream)))))

(-> test-mcp-configuration--server-form
    (&key (:name string)
          (:transport list)
          (:approval keyword)
          (:trusted-read-only-tools list)
          (:child-tools list))
    list)
(defun test-mcp-configuration--server-form
    (&key
       (name "test")
       (transport '(:type :stdio :command "/bin/true"))
       (approval :prompt)
       (trusted-read-only-tools nil)
       (child-tools nil))
  "Return one native MCP server form for focused validation tests."
  (list :name name
        :transport transport
        :approval approval
        :trusted-read-only-tools trusted-read-only-tools
        :child-tools child-tools))

(-> test-mcp-configuration--http-server-form
    (string &key (:headers list))
    list)
(defun test-mcp-configuration--http-server-form (url &key headers)
  "Return one native Streamable HTTP test server for URL and HEADERS."
  (test-mcp-configuration--server-form
   :transport
   (list :type :http
         :url url
         :headers headers)))

(-> test-mcp-configuration--server-signals-p (list) boolean)
(defun test-mcp-configuration--server-signals-p (form)
  "Return true when native MCP server FORM fails validation."
  (handler-case
      (progn
        (mcp-configuration--server form)
        nil)
    (mcp-configuration-error ()
      t)))

(-> test-mcp-configuration--registration
    (string keyword keyword)
    mcp-server-registration)
(defun test-mcp-configuration--registration (name source approval)
  "Return one validated test registration for NAME, SOURCE, and APPROVAL."
  (make-instance
   'mcp-server-registration
   :configuration
   (mcp-server-configuration-create
    :name name
    :transport '(:type :stdio :command "/bin/true")
    :approval approval)
   :source source))

(-> test-mcp-configuration--effective-approval (string) keyword)
(defun test-mcp-configuration--effective-approval (name)
  "Return the effective approval policy for registered server NAME."
  (let ((registration
          (find
           name
           (mcp-server-registrations)
           :test #'string=
           :key
           (lambda (candidate)
             (mcp-server-configuration-name
              (mcp-server-registration-configuration candidate))))))
    (mcp-server-configuration-approval-policy
     (mcp-server-registration-configuration registration))))

(-> test-mcp-configuration () null)
(defun test-mcp-configuration ()
  "Test strict native MCP parsing and layered registration transactions."
  (let* ((site-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-site-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration
           (progn
             (ensure-directories-exist site-root)
             (setf site-root
                   (uiop:ensure-directory-pathname (truename site-root)))
             (test-configuration :site-config-root site-root)))
         (root (test-configuration-root configuration))
         (pathname (configuration-mcp-path configuration))
         (site-pathname (configuration-site-mcp-path configuration))
         (registry-snapshot (mcp--registry-snapshot)))
    (unwind-protect
         (progn
           (test-assert
            (string= (file-namestring pathname) "mcp.sexp")
            "native MCP configuration has one XDG mcp.sexp entry point")
            (test-mcp-configuration--write
             pathname
             "(:version 1 :servers ((:name\"adjacent\" :transport (:type :stdio :command\"/bin/true\") :approval :deny :trusted-read-only-tools () :child-tools ())))")
            (let ((definitions (mcp-configuration-read configuration)))
              (test-assert
               (and (= (length definitions) 1)
                    (string=
                     (mcp-server-configuration-name (first definitions))
                     "adjacent"))
               "native MCP parsing accepts a string adjacent to its keyword"))
            (delete-file pathname)
           (labels
               ((read-nonregular-source (source-pathname)
                  "Read SOURCE-PATHNAME in a bounded thread and return its condition."
                  (let* ((caught-condition nil)
                        (thread
                          (make-thread
                           (lambda ()
                             (setf caught-condition
                                   (handler-case
                                       (progn
                                         (mcp-configuration-read configuration)
                                         nil)
                                     (serious-condition (cause)
                                       cause))))
                           :name "Autolith MCP nonregular configuration test")))
                    (loop repeat 100
                          while (thread-alive-p thread)
                          do (sleep 0.01))
                    (when (thread-alive-p thread)
                      (bordeaux-threads:destroy-thread thread)
                      (error
                       "MCP configuration read blocked on nonregular source ~A."
                       source-pathname))
                    (join-thread thread)
                    caught-condition)))
             (ensure-directories-exist pathname)
             (sb-posix:mkfifo (namestring pathname) #o600)
             (test-assert
              (typep (read-nonregular-source pathname)
                     'mcp-configuration-error)
              "a FIFO mcp.sexp is rejected without blocking")
             (sb-posix:unlink (namestring pathname))
             (test-assert
              (handler-case
                  (progn
                    (mcp-configuration--source-present-p #P"/dev/null")
                    nil)
                (mcp-configuration-error ()
                  t))
             "a device node is rejected before the configuration reader opens it")
             (let ((fifo (merge-pathnames "mcp-source.fifo" root)))
               (sb-posix:mkfifo (namestring fifo) #o600)
               (unwind-protect
                    (progn
                      (sb-posix:symlink
                       (namestring fifo)
                       (namestring pathname))
                      (test-assert
                       (typep (read-nonregular-source pathname)
                              'mcp-configuration-error)
                       "a symlinked mcp.sexp is rejected without following a FIFO"))
                 (ignore-errors
                   (sb-posix:unlink (namestring pathname)))
                 (ignore-errors
                   (sb-posix:unlink (namestring fifo))))))
           (let ((target (merge-pathnames "managed-mcp.sexp" root)))
             (unwind-protect
                  (progn
                    (test-mcp-configuration--write
                     target
                     "(:version 1 :servers ())")
                    (sb-posix:symlink
                     (namestring target)
                     (namestring pathname))
                    (test-assert
                     (null (mcp-configuration-read configuration))
                     "a managed symlink to a regular MCP configuration is accepted"))
               (ignore-errors
                 (sb-posix:unlink (namestring pathname)))
               (ignore-errors
                 (delete-file target))))
           (test-mcp-configuration--write
            pathname
            "(:version 1
               :servers
               ((:name \"local\"
                 :transport
                 (:type :stdio
                  :command \"/usr/bin/example-mcp\"
                  :arguments (\"--quiet\")
                  :directory :workspace
                  :environment
                  ((\"MCP_TOKEN\" :environment \"AUTOLITH_TEST_TOKEN\")))
                 :required-p t
                 :startup-timeout-seconds 3
                 :tool-timeout-seconds 9
                 :approval :prompt
                 :child-tools (\"lookup\"))
                (:name \"remote\"
                 :transport
                 (:type :http
                  :url \"https://example.test/mcp\"
                  :headers
                  ((\"Authorization\" :environment
                    \"AUTOLITH_TEST_AUTHORIZATION\"))
                  :connect-timeout-seconds 4)
                 :approval :read-only
                 :trusted-read-only-tools (\"lookup\"))))")
           (let* ((servers (mcp-configuration-read configuration))
                  (local (first servers))
                  (remote (second servers))
                  (stdio
                    (mcp-server-configuration-transport local))
                  (http
                    (mcp-server-configuration-transport remote)))
             (test-assert (= (length servers) 2)
                          "native MCP configuration reads every server")
             (test-assert
              (and
               (typep stdio 'mcp-stdio-transport-configuration)
               (string=
                (mcp-stdio-configuration-command stdio)
                "/usr/bin/example-mcp")
               (equal
                (mcp-stdio-configuration-arguments stdio)
                '("--quiet"))
               (eq (mcp-stdio-configuration-directory stdio)
                   :workspace))
              "stdio transport fields remain native typed values")
             (test-assert
              (and
               (mcp-server-configuration-required-p local)
               (= (mcp-server-configuration-startup-timeout-seconds local)
                  3)
               (= (mcp-server-configuration-tool-timeout-seconds local)
                  9)
               (eq (mcp-server-configuration-approval-policy local)
                   :prompt)
               (equal
                (mcp-server-configuration-child-tools local)
                '("lookup")))
              "server policy is validated without JSON compatibility state")
             (test-assert
              (and
               (typep http 'mcp-http-transport-configuration)
               (string=
                (mcp-http-configuration-url http)
                "https://example.test/mcp")
               (= (mcp-http-configuration-connect-timeout-seconds http)
                  4)
               (string=
                (mcp-environment-binding-source
                 (first
                  (mcp-http-configuration-header-bindings http)))
                "AUTOLITH_TEST_AUTHORIZATION"))
              "HTTP credentials are retained only as environment names")
           (test-assert
              (and
               (eq (mcp-server-configuration-approval-policy remote)
                   :read-only)
               (equal
                (mcp-server-configuration-trusted-read-only-tools remote)
                '("lookup")))
              "read-only approval trusts annotations only for exact raw names"))
           (test-assert
            (eq
             (mcp-server-configuration-approval-policy
              (mcp-server-configuration-create
               :name "programmatic-default"
               :transport '(:type :stdio :command "/bin/true")))
             :prompt)
            "programmatic MCP registration defaults to prompt")
           (test-mcp-configuration--write
            pathname
            "(:version 1
               :servers
               ((:name \"reader-state\"
                 :transport (:type :stdio :command \"/bin/true\")
                 :startup-timeout-seconds 15)))")
           (let ((*read-base* 16)
                 (*read-suppress* t))
             (let ((server
                     (first (mcp-configuration-read configuration))))
               (test-assert
                (= (mcp-server-configuration-startup-timeout-seconds server)
                   15)
                "native MCP reading ignores mutable ambient reader state")))
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 0 :servers ())")
            "unsupported native MCP versions fail closed")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1 :servers ()
               :legacy-json-compatibility t)")
            "unknown compatibility keys are rejected")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1 :version 1 :servers ())")
            "duplicate native MCP keys are rejected")
           (let ((name
                   (format nil
                           "AUTOLITH-MCP-UNKNOWN-~A"
                           (string-upcase (make-identifier)))))
             (test-assert
              (null (find-symbol name "KEYWORD"))
              "the novel MCP keyword starts absent")
             (test-assert
              (test-mcp-configuration--signals-p
               configuration
               (format nil "(:version 1 :servers () :~A t)" name))
              "unknown native MCP keyword tokens fail closed")
             (test-assert
              (null (find-symbol name "KEYWORD"))
              "rejected MCP keyword tokens do not pollute KEYWORD"))
           (let ((name
                   (format nil
                           "AUTOLITH-MCP-QUALIFIED-~A"
                           (string-upcase (make-identifier)))))
             (test-assert
              (null (find-symbol name "AUTOLITH"))
              "the novel qualified MCP symbol starts absent")
             (test-assert
              (test-mcp-configuration--signals-p
               configuration
               (format nil
                       "(:version 1 :servers () AUTOLITH::~A t)"
                       name))
              "package-qualified native MCP symbols fail closed")
             (test-assert
              (null (find-symbol name "AUTOLITH"))
              "rejected MCP forms do not pollute existing packages"))
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "{\"mcpServers\":{\"example\":{\"command\":\"server\"}}}")
            "JSON-shaped MCP compatibility configuration is rejected")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1
               :servers
               ((:name \"bad\"
                 :transport
                 (:type :http
                  :url \"https://example.test/mcp\"
                  :headers ((\"Authorization\" \"literal-secret\"))))))")
            "literal HTTP credentials are not accepted")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1
               :servers
               ((:name \"bad\"
                 :transport
                 (:type :stdio
                  :command \"server\"
                  :environment ((\"TOKEN\" :literal \"secret\"))))))")
            "literal standard-input credentials are not accepted")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1
               :servers
               ((:name \"bad\"
                 :transport
                 (:type :stdio :command \"server\")
                 :tool-timeout-seconds nil)))")
            "an explicit invalid timeout is not mistaken for a default")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (append
              (test-mcp-configuration--server-form)
              (list
               :tool-timeout-seconds
               (1+ *mcp-maximum-timeout-seconds*))))
            "MCP operation deadlines have a finite upper bound")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "(:version 1 :servers ()) (:extra t)")
            "native MCP configuration rejects additional top-level forms")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             (make-string
              (1+ *mcp-configuration-maximum-bytes*)
              :initial-element #\Space))
            "native MCP configuration rejects oversized input while reading")
           (test-assert
            (test-mcp-configuration--signals-p
             configuration
             "#.(error \"reader evaluation ran\")")
            "native MCP reading disables reader evaluation")
           (let ((shared (list :shared)))
             (test-assert
              (handler-case
                  (progn
                    (mcp-configuration--validate-readable-tree
                     (list shared shared))
                    nil)
                (mcp-configuration-error ()
                  t))
              "native MCP validation rejects shared reader structure"))
           (let ((circular (list :circular)))
             (setf (rest circular) circular)
             (test-assert
              (handler-case
                  (progn
                    (mcp-configuration--validate-readable-tree circular)
                    nil)
                (mcp-configuration-error ()
                  t))
              "native MCP validation rejects circular reader structure"))
           (let ((nested nil))
             (loop repeat (+ 2 *mcp-configuration-maximum-depth*)
                   do (setf nested (list nested)))
             (test-assert
              (handler-case
                  (progn
                    (mcp-configuration--validate-readable-tree nested)
                    nil)
                (mcp-configuration-error ()
                  t))
              "native MCP validation bounds nested structure"))
           (test-assert
            (handler-case
                (progn
                  (mcp-configuration--validate-readable-tree
                   (make-list
                    *mcp-configuration-maximum-nodes*
                    :initial-element nil))
                  nil)
              (mcp-configuration-error ()
                t))
            "native MCP validation bounds total structure")
           (test-assert
            (test-mcp-configuration--signals-form-p
             configuration
             (list
              :version 1
              :servers
              (loop for index
                      below (1+ *mcp-configuration-maximum-servers*)
                    collect
                    (test-mcp-configuration--server-form
                     :name (format nil "server-~D" index)))))
            "native MCP configuration bounds its server count")
           (test-assert
            (and
             (not
              (test-mcp-configuration--server-signals-p
               (test-mcp-configuration--server-form
                :name
                (make-string
                 *mcp-server-name-maximum-characters*
                 :initial-element #\s))))
             (test-mcp-configuration--server-signals-p
              (test-mcp-configuration--server-form
               :name
               (make-string
                (1+ *mcp-server-name-maximum-characters*)
                :initial-element #\s))))
            "MCP server names have an inclusive character bound")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command
               (make-string
                (1+ *mcp-stdio-command-maximum-characters*)
                :initial-element #\c))))
            "MCP standard-input commands are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command "/bin/true"
               :arguments
               (make-list
                (1+ *mcp-stdio-maximum-arguments*)
                :initial-element "argument"))))
            "MCP standard-input argument counts are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command "/bin/true"
               :arguments
               (list
                (make-string
                (1+ *mcp-stdio-argument-maximum-characters*)
                 :initial-element #\a)))))
            "individual MCP standard-input arguments are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command "/bin/true"
               :directory
               (make-string
                (1+ *mcp-stdio-directory-maximum-characters*)
                :initial-element #\d))))
            "MCP standard-input directories are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command "/bin/true"
               :environment
               (loop for index
                       below
                       (1+ *mcp-stdio-maximum-environment-bindings*)
                     collect
                     (list
                      (format nil "MCP_TEST_~D" index)
                      :environment
                      (format nil "AUTOLITH_TEST_~D" index))))))
            "MCP standard-input environment binding counts are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :transport
              (list
               :type :stdio
               :command "/bin/true"
               :environment
               (list
                (list
                 (make-string
                  (1+ *mcp-environment-name-maximum-characters*)
                  :initial-element #\E)
                 :environment
                 "AUTOLITH_TEST")))))
            "MCP environment variable names are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--http-server-form
              "https://example.test/mcp"
              :headers
              (loop for index
                      below (1+ *mcp-http-maximum-header-bindings*)
                    collect
                    (list
                     (format nil "X-Autolith-Test-~D" index)
                     :environment
                     (format nil "AUTOLITH_TEST_~D" index)))))
            "MCP Streamable HTTP header binding counts are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--http-server-form
              "https://example.test/mcp"
              :headers
              (list
               (list
                (make-string
                 (1+ *mcp-http-header-name-maximum-characters*)
                 :initial-element #\X)
                :environment
                "AUTOLITH_TEST"))))
            "MCP Streamable HTTP header names are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--http-server-form
              "https://example.test/mcp"
              :headers
              '(("lAsT-EvEnT-Id" :environment "AUTOLITH_TEST"))))
            "MCP transport-owned HTTP headers are reserved case-insensitively")
           (test-assert
            (not
             (test-mcp-configuration--server-signals-p
              (test-mcp-configuration--http-server-form
               "HTTPS://example.test/mcp")))
            "MCP URL schemes are accepted case-insensitively")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--http-server-form
              (concatenate
               'string
               "https://example.test/"
               (make-string
                *mcp-http-url-maximum-characters*
                :initial-element #\u))))
            "MCP Streamable HTTP URLs are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :approval ':read-only
              :trusted-read-only-tools
              (loop for index
                      below (1+ *mcp-maximum-trusted-read-only-tools*)
                    collect (format nil "trusted-tool-~D" index))))
            "trusted read-only MCP tool counts are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :approval ':read-only
              :trusted-read-only-tools '("duplicate" "duplicate")))
            "trusted read-only MCP tool names must be unique")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :approval ':prompt
              :trusted-read-only-tools '("unused")))
            "trusted read-only MCP tools require the read-only policy")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :child-tools
              (loop for index below (1+ *mcp-maximum-child-tools*)
                    collect (format nil "tool-~D" index))))
            "MCP child tool grants are bounded")
           (test-assert
            (test-mcp-configuration--server-signals-p
             (test-mcp-configuration--server-form
              :child-tools
              (list
               (make-string
                (1+ *mcp-child-tool-name-maximum-characters*)
                :initial-element #\t))))
            "individual MCP child tool names are bounded")
            (dolist
                (url
                  '("http://localhost:3000/mcp"
                    "http://localhost.:3000/mcp"
                    "http://127.0.0.2:3000/mcp"
                    "http://[::1]:3000/mcp"
                    "http://[0:0:0:0:0:0:0:1]:3000/mcp"
                    "https://example.test/mcp"
                    "https://[2001:db8::1]:65535/mcp"
                    "https://user@example.test/mcp"
                    "https://example.test/mcp?token=secret"
                    "https://example.test/mcp?"
                    "https://example.test/mcp#fragment"
                    "https://example.test/mcp#"))
              (test-assert
               (not
                (test-mcp-configuration--server-signals-p
                 (test-mcp-configuration--http-server-form url)))
               (format nil "secure or loopback MCP URL ~S is accepted" url)))
            (dolist
                (url
                  '("http://example.test/mcp"
                    "http://localhost.example/mcp"
                    "http://127.0.0.1.example/mcp"
                    "https://-example.test/mcp"
                    "https://example..test/mcp"
                    "https://exa_mple.test/mcp"
                    "https://127.999.1.1/mcp"
                    "https://[:::]/mcp"
                    "https://[192.0.2.1::]/mcp"
                    "https://example.test:/mcp"
                    "https://example.test:+1/mcp"
                    "https://example.test:0/mcp"
                    "https://example.test:65536/mcp"))
             (test-assert
              (test-mcp-configuration--server-signals-p
               (test-mcp-configuration--http-server-form url))
              (format nil "unsafe or malformed MCP URL ~S is rejected" url)))
           (let* ((sentinel "AUTOLITH-URL-CREDENTIAL-SENTINEL")
                  (condition
                    (handler-case
                        (progn
                          (mcp-configuration--server
                           (test-mcp-configuration--http-server-form
                            (format nil
                                    "https://user:~A@example.test:%/mcp"
                                    sentinel)))
                          nil)
                      (mcp-configuration-error (cause)
                        cause))))
             (test-assert
              (and condition
                   (not (search sentinel (princ-to-string condition)))
                   (null (mcp-configuration-error-cause condition)))
              "malformed MCP URL failures retain and print no credential text"))
           (mcp--registry-restore nil)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :allow)
            :source ':runtime)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :deny)
            :source ':tracked)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :read-only)
            :source ':user)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :prompt)
            :source ':config)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :deny)
            :source ':site)
           (register-mcp-server
            '(:name "precedence"
              :transport (:type :stdio :command "/bin/true")
              :approval :allow)
            :source ':directory)
           (test-assert
            (and
             (eq
              (mcp-server-registration-source
               (first (mcp-server-registrations)))
              ':runtime)
             (eq (test-mcp-configuration--effective-approval "precedence")
                 ':allow))
            "runtime MCP registration wins regardless of registration order")
           (mcp--remove-registration-source :runtime)
           (test-assert
            (eq (test-mcp-configuration--effective-approval "precedence")
                ':read-only)
            "user MCP registration shadows config and tracked registrations")
           (mcp--remove-registration-source :user)
           (test-assert
            (eq
             (mcp-server-registration-source
              (first (mcp-server-registrations)))
             ':directory)
            "directory MCP registration shadows config and site registrations")
           (mcp--remove-registration-source ':directory)
           (test-assert
            (eq
             (mcp-server-registration-source
              (first (mcp-server-registrations)))
             ':config)
            "config MCP registration shadows a site registration")
           (mcp--remove-registration-source :config)
           (test-assert
            (eq
             (mcp-server-registration-source
              (first (mcp-server-registrations)))
             ':site)
            "site MCP registration shadows a tracked registration")
           (mcp--remove-registration-source ':site)
           (test-assert
            (eq (test-mcp-configuration--effective-approval "precedence")
                ':deny)
            "tracked MCP registration remains the lowest precedence")
           (test-assert
            (handler-case
                (progn
                  (register-mcp-server
                   '(:name "unsupported-source"
                     :transport (:type :stdio :command "/bin/true"))
                   :source ':compatibility)
                  nil)
              (mcp-configuration-error ()
                t))
            "unknown MCP registration sources fail closed")
           (let ((before (mcp--registry-snapshot)))
             (test-assert
              (handler-case
                  (progn
                    (mcp--registry-restore
                     (loop for index
                             below
                             (1+ *mcp-configuration-maximum-servers*)
                           collect
                           (test-mcp-configuration--registration
                            (format nil "bounded-~D" index)
                            :runtime
                            :prompt)))
                    nil)
                (mcp-configuration-error ()
                  t))
              "the live MCP registry bounds its effective server count")
             (test-assert
              (equal before (mcp--registry-snapshot))
              "a rejected oversized MCP registry leaves live state intact"))
           (test-mcp-configuration--write
            site-pathname
            "(:version 1
               :servers
               ((:name \"layered\"
                 :transport
                 (:type :stdio :command \"/bin/true\")
                 :approval :deny)
                (:name \"site-only\"
                 :transport
                 (:type :stdio :command \"/bin/true\")
                 :approval :allow)))")
           (test-mcp-configuration--write
            pathname
            "(:version 1
               :servers
               ((:name \"layered\"
                 :transport
                 (:type :stdio :command \"/bin/true\")
                 :approval :read-only)))")
           (mcp--registry-restore nil)
           (register-mcp-server
            '(:name "layered"
              :transport (:type :stdio :command "/bin/false")
              :approval :deny)
            :source ':tracked)
           (mcp-configuration-load configuration)
           (user-init-load configuration)
           (test-assert
            (eq
             (mcp-server-configuration-approval-policy
              (mcp-server-registration-configuration
               (first (mcp-server-registrations))))
             :read-only)
            "the native config layer shadows site and tracked servers")
           (test-assert
            (eq
             (mcp-server-registration-source
              (find
               "site-only"
               (mcp-server-registrations)
               :test #'string=
               :key
               (lambda (registration)
                 (mcp-server-configuration-name
                  (mcp-server-registration-configuration registration)))))
             ':site-config)
            "site mcp.sexp survives executable-init loading below user config")
           (let ((*user-init-loading-p* t)
                 (*extension-registration-source* ':user))
             (register-mcp-server
              '(:name "layered"
                :transport (:type :stdio :command "/bin/true")
                :approval :allow)))
           (test-assert
            (and
             (eq
              (mcp-server-registration-source
               (first (mcp-server-registrations)))
              :user)
             (eq
              (mcp-server-configuration-approval-policy
               (mcp-server-registration-configuration
                (first (mcp-server-registrations))))
              :allow))
            "user initialization becomes the final MCP registration layer")
           (let ((snapshot (mcp--registry-snapshot)))
             (mcp--remove-registration-source :user)
             (test-assert
              (eq
               (mcp-server-registration-source
                (first (mcp-server-registrations)))
               :config)
              "removing user MCP layers reveals native configuration")
             (mcp--registry-restore snapshot)
             (test-assert
              (eq
               (mcp-server-registration-source
                (first (mcp-server-registrations)))
               :user)
              "MCP registry snapshots restore exact layered state"))
           (let ((before (mcp--registry-snapshot)))
             (test-mcp-configuration--write
              pathname
              "(:version 99 :servers ())")
             (handler-case
                 (mcp-configuration-load configuration)
               (mcp-configuration-error ()
                 nil))
             (test-assert
              (equal before (mcp--registry-snapshot))
              "a malformed reload leaves every prior MCP layer intact")))
      (mcp--registry-restore registry-snapshot)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist ':ignore)
      (uiop:delete-directory-tree
       site-root :validate t :if-does-not-exist ':ignore)))
  nil)
