(in-package #:autolith)

;;;; -- Registration Source Tests --

(-> registration-source-tests--tool
    (tool-registry string string registration-source-designator)
    tool)
(defun registration-source-tests--tool (registry namespace name source)
  "Register and return one stateless tool in REGISTRY attributed to SOURCE."
  (let ((tool
          (make-instance 'tool
                         :namespace namespace
                         :name name
                         :description "Registered by one test registration source."
                         :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry tool :source source)
    tool))

(-> registration-source-tests--command (symbol string) application-command)
(defun registration-source-tests--command (definition-name name)
  "Return one complete command fixture named NAME."
  (application-command-create
   :definition-name definition-name
   :name name
   :argument nil
   :description "registration source test command"
   :tip "exists for registration source tests."
   :busy-behavior ':inspect
   :terminal-behavior ':shared
   :handler (lambda (application invocation)
              (declare (ignore application invocation))
              ':continue)))

(-> registration-source-tests--command-present-p (string) boolean)
(defun registration-source-tests--command-present-p (name)
  "Return true when an effective command layer carries NAME."
  (let ((registration
          (find name (application-command--registry-snapshot)
                :test #'string=
                :key (lambda (candidate)
                       (application-command-name
                        (application-command-registration-command candidate))))))
    (and registration t)))

(-> registration-source-tests--mcp-registration
    (string)
    (option mcp-server-registration))
(defun registration-source-tests--mcp-registration (name)
  "Return the MCP registration layer whose server is NAME, when present."
  (find name (mcp-server-registrations)
        :test #'string=
        :key (lambda (registration)
               (mcp-server-configuration-name
                (mcp-server-registration-configuration registration)))))

(-> test-registration-source-identity () null)
(defun test-registration-source-identity ()
  "Test that plugin registration sources intern per plugin name."
  (let ((first-source (plugin-registration-source "example-plugin"))
        (repeat-source (plugin-registration-source "example-plugin"))
        (other-source (plugin-registration-source "other-plugin")))
    (test-assert
     (and (eq first-source repeat-source)
          (not (eq first-source other-source))
          (string= (registration-source-plugin first-source) "example-plugin")
          (string= (registration-source-plugin other-source) "other-plugin"))
     "one interned registration source exists per plugin name")
    (test-assert
     (and (eq (registration-source-layer first-source) ':plugin)
          (eq (registration-source-layer ':builtin) ':builtin)
          (registration-source-plugin-p first-source)
          (not (registration-source-plugin-p ':builtin)))
     "a plugin source reports the plugin layer and carries plugin identity")
    (test-assert
     (and (registration-source-designator-p first-source)
          (registration-source-designator-p ':user)
          (not (registration-source-designator-p ':plugin))
          (not (registration-source-designator-p "example-plugin")))
     "a registration source designator is a layer keyword or a plugin source")
    (test-assert
     (and (eq (registration-source-coerce first-source) first-source)
          (eq (registration-source-coerce ':site) ':site))
     "coercion returns a valid designator unchanged")
    (dolist (invalid (list ':plugin "example-plugin" 42 nil))
      (test-assert
       (handler-case
           (progn
             (registration-source-coerce invalid)
             nil)
         (configuration-error () t))
       (format nil "~S is rejected as a registration source" invalid)))
    (test-assert
     (eq (current-registration-source) ':runtime)
     "the default load context registers contributions as runtime layers")
    (let ((*extension-registration-source* first-source))
      (test-assert
       (eq (current-registration-source) first-source)
       "a bound plugin source is the current registration source"))
    (test-assert
     (handler-case
         (let ((*extension-registration-source* ':plugin))
           (current-registration-source)
           nil)
       (configuration-error () t))
     "the bare plugin layer is rejected as a registration source"))
  nil)

(-> test-registration-source-tool-registry () null)
(defun test-registration-source-tool-registry ()
  "Test tool registry source attribution and per-source removal."
  (let* ((registry (make-instance 'tool-registry))
         (plugin-source (plugin-registration-source "deploy-plugin"))
         (other-source (plugin-registration-source "other-plugin"))
         (builtin-tool
           (registration-source-tests--tool registry "base" "lookup" ':builtin))
         (plugin-tool
           (registration-source-tests--tool registry "deploy" "start" plugin-source))
         (other-tool
           (registration-source-tests--tool registry "deploy" "stop" other-source)))
    (test-assert
     (and (eq (tool-registry-source registry builtin-tool) ':builtin)
          (eq (tool-registry-source registry plugin-tool) plugin-source)
          (eq (tool-registry-source registry other-tool) other-source))
     "the tool registry records the source of every registration")
    (let ((replacement
            (make-instance 'tool
                           :namespace "base"
                           :name "lookup"
                           :description "Replacement tool from another source."
                           :parameters (tool-object-schema (json-object) nil))))
      (tool-registry-register registry replacement :source other-source)
      (test-assert
       (and (eq (tool-registry-find registry "base" "lookup") replacement)
            (eq (tool-registry-source registry replacement) other-source)
            (= (length (tool-registry-tools registry)) 3))
       "replacement re-attributes the source without adding a tool"))
    (test-assert
     (and (equal (mapcar (lambda (tool)
                           (tool-canonical-name tool))
                         (tool-registry-remove-source registry plugin-source))
                 '("deploy.start"))
          (null (tool-registry-find registry "deploy" "start"))
          (eq (tool-registry-source registry other-tool) other-source)
          (= (length (tool-registry-tools registry)) 2))
     "per-source removal removes exactly that source's tools")
    (test-assert
     (null (tool-registry-remove-source registry plugin-source))
     "a repeated removal of the same source removes nothing")
    (test-assert
     (and (equal (mapcar (lambda (tool)
                           (tool-canonical-name tool))
                         (tool-registry-remove-source registry other-source))
                 '("base.lookup" "deploy.stop"))
          (null (tool-registry-tools registry)))
     "removing the last plugin source empties the registry")
    (let ((temporary
            (registration-source-tests--tool registry "temp" "one" plugin-source)))
      (tool-registry-delete-if registry
                               (lambda (tool)
                                 (eq tool temporary)))
      (test-assert
       (and (null (tool-registry-find registry "temp" "one"))
            (null (tool-registry-source registry temporary)))
       "deleting a tool drops its recorded registration source")))
  nil)

(-> test-registration-source-scoped-removal () null)
(defun test-registration-source-scoped-removal ()
  "Test that the command, context, provider, and MCP registries remove by source."
  (let ((provider-snapshot (provider--registry-snapshot))
        (command-snapshot (application-command--registry-snapshot))
        (context-snapshot (context--registry-snapshot))
        (mcp-snapshot (mcp--registry-snapshot))
        (first-source (plugin-registration-source "first-plugin"))
        (second-source (plugin-registration-source "second-plugin")))
    (unwind-protect
         (progn
           (flet ((provider-factory (configuration &key reasoning-summaries-p)
                    "Reject an accidental provider construction in this test."
                    (declare (ignore configuration reasoning-summaries-p))
                    (error "A provider factory in this test is never called."))
                  (register-server (name source)
                    "Register one MCP server layer named NAME from SOURCE."
                    (register-mcp-server
                     (list :name name
                           :transport (list :type ':stdio :command "/bin/true")
                           :approval ':deny)
                     :source source)))
             (register-provider "first-plugin-provider"
                                :description "Provided by the first test plugin."
                                :models '("first-plugin/model")
                                :factory #'provider-factory
                                :source first-source)
             (register-provider "second-plugin-provider"
                                :models '("second-plugin/model")
                                :description "Provided by the second test plugin."
                                :factory #'provider-factory
                                :source second-source)
             (test-assert
              (and (provider-registration-find "first-plugin-provider")
                   (provider-registration-find "second-plugin-provider"))
              "plugin sources register providers in the provider registry")
             (provider--remove-registration-source second-source)
             (test-assert
              (and (provider-registration-find "first-plugin-provider")
                   (null (provider-registration-find "second-plugin-provider")))
              "provider removal by one plugin source keeps another plugin's provider")
             (provider--remove-registration-source first-source)
             (test-assert
              (null (provider-registration-find "first-plugin-provider"))
              "provider removal by its own plugin source removes its provider")
             (register-application-command
              (registration-source-tests--command
               'registration-source-tests--first-command
               "/registration-source-first")
              :source first-source)
             (register-application-command
              (registration-source-tests--command
               'registration-source-tests--second-command
               "/registration-source-second")
              :source second-source)
             (application-command--remove-registration-source second-source)
             (test-assert
              (and (registration-source-tests--command-present-p
                    "/registration-source-first")
                   (not (registration-source-tests--command-present-p
                         "/registration-source-second")))
              "command removal by one plugin source keeps another plugin's command")
             (application-command--remove-registration-source first-source)
             (test-assert
              (not (registration-source-tests--command-present-p
                    "/registration-source-first"))
              "command removal by its own plugin source removes its command")
             (register-context-contributor
              "first-plugin/example"
              (lambda (request)
                (declare (ignore request))
                nil)
              :source first-source)
             (register-context-contributor
              "second-plugin/example"
              (lambda (request)
                (declare (ignore request))
                nil)
              :source second-source)
             (test-assert
              (and (eq (getf (context--registration-find "first-plugin/example")
                             :source)
                       first-source)
                   (eq (getf (context--registration-find "second-plugin/example")
                             :source)
                       second-source))
              "plugin sources register context contributors with their identity")
             (context--remove-registration-source second-source)
             (test-assert
              (and (context--registration-find "first-plugin/example")
                   (null (context--registration-find "second-plugin/example")))
              "context removal by one plugin source keeps another plugin's contributor")
             (context--remove-registration-source first-source)
             (test-assert
              (null (context--registration-find "first-plugin/example"))
              "context removal by its own plugin source removes its contributor")
             (register-server "first-plugin-server" first-source)
             (register-server "second-plugin-server" second-source)
             (test-assert
              (and (registration-source-tests--mcp-registration "first-plugin-server")
                   (registration-source-tests--mcp-registration "second-plugin-server"))
              "plugin sources register MCP servers above the bundled layers")
             (mcp--remove-registration-source second-source)
             (test-assert
              (and (registration-source-tests--mcp-registration "first-plugin-server")
                   (null (registration-source-tests--mcp-registration
                          "second-plugin-server")))
              "MCP removal by one plugin source keeps another plugin's server")
             (mcp--remove-registration-source first-source)
             (test-assert
              (null (registration-source-tests--mcp-registration "first-plugin-server"))
              "MCP removal by its own plugin source removes its server")))
      (mcp--registry-restore mcp-snapshot)
      (context--registry-restore context-snapshot)
      (application-command--registry-restore command-snapshot)
      (provider--registry-restore provider-snapshot)))
  nil)
