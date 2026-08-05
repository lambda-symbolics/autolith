(in-package #:autolith)

(define-condition test-mcp-fatal-condition (serious-condition)
  ()
  (:documentation "A non-error serious condition used to test fatal propagation."))

;;;; -- Scripted MCP Transport --

(defclass test-mcp-transport (mcp-transport)
  ((handler
    :initarg :handler
    :reader test-mcp-transport-handler
    :type function
    :documentation "The deterministic JSON-RPC request handler.")
   (requests
    :initform nil
    :accessor test-mcp-transport-requests
    :type list
    :documentation "Requests observed in chronological order.")
   (notifications
    :initform nil
    :accessor test-mcp-transport-notifications
    :type list
    :documentation "Notifications observed in chronological order.")
   (open-p
    :initform nil
    :accessor test-mcp-transport-open-p
    :type boolean
    :documentation "Whether the scripted transport is logically open.")
   (close-count
    :initform 0
    :accessor test-mcp-transport-close-count
    :type (integer 0)
    :documentation "The number of explicit close operations.")
   (close-secret-use-p
    :initform nil
    :accessor test-mcp-transport-close-secret-use-p
    :type boolean
    :documentation "Whether close ran inside the transient secret-use guard.")
   (detach-count
    :initform 0
    :accessor test-mcp-transport-detach-count
    :type (integer 0)
    :documentation "The number of inherited-resource detach operations."))
  (:documentation "A deterministic in-memory MCP transport for Autolith tests."))

(defmethod mcp-transport-open ((transport test-mcp-transport))
  "Open scripted TRANSPORT."
  (setf (test-mcp-transport-open-p transport) t)
  transport)

(defmethod mcp-transport-open-p ((transport test-mcp-transport))
  "Return scripted TRANSPORT's logical open state."
  (test-mcp-transport-open-p transport))

(defmethod mcp-transport-request
    ((transport test-mcp-transport) request timeout)
  "Record REQUEST and dispatch it through TRANSPORT's deterministic handler."
  (declare (ignore timeout))
  (setf (test-mcp-transport-requests transport)
        (nconc (test-mcp-transport-requests transport)
               (list request)))
  (funcall (test-mcp-transport-handler transport) transport request))

(defmethod mcp-transport-notify
    ((transport test-mcp-transport) notification timeout)
  "Record one JSON-RPC NOTIFICATION."
  (declare (ignore timeout))
  (setf (test-mcp-transport-notifications transport)
        (nconc (test-mcp-transport-notifications transport)
               (list notification)))
  nil)

(defmethod mcp-transport-set-protocol-version
    ((transport test-mcp-transport) version)
  "Accept the negotiated VERSION for scripted TRANSPORT."
  (declare (ignore version))
  transport)

(defmethod mcp-transport-close ((transport test-mcp-transport))
  "Close scripted TRANSPORT and record the operation."
  (incf (test-mcp-transport-close-count transport))
  (setf (test-mcp-transport-open-p transport) nil
        (test-mcp-transport-close-secret-use-p transport)
        (secret-use-active-p)
        (test-mcp-transport-requests transport) nil
        (test-mcp-transport-notifications transport) nil)
  nil)

(defmethod mcp-transport-detach ((transport test-mcp-transport))
  "Detach scripted TRANSPORT and record the operation."
  (incf (test-mcp-transport-detach-count transport))
  (setf (test-mcp-transport-open-p transport) nil
        (test-mcp-transport-requests transport) nil
        (test-mcp-transport-notifications transport) nil)
  nil)

(-> test-mcp--rpc-result (hash-table t) hash-table)
(defun test-mcp--rpc-result (request result)
  "Return one successful JSON-RPC response to REQUEST carrying RESULT."
  (json-object
   "jsonrpc" "2.0"
   "id" (json-get request "id")
   "result" result))

(-> test-mcp--tool-definition
    (string &key (:description string)
                 (:read-only-p boolean)
                 (:destructive-p boolean)
                 (:task-required-p boolean))
    hash-table)
(defun test-mcp--tool-definition
    (name &key
          (description "")
          read-only-p
          (destructive-p t)
          task-required-p)
  "Return one scripted MCP tool named NAME."
  (let ((definition
          (json-object
           "name" name
           "description" description
           "inputSchema"
           (json-object
            "type" "object"
            "properties"
            (json-object
             "value" (json-object "type" "string"))
            "additionalProperties" false)
           "annotations"
           (json-object
            "readOnlyHint" (if read-only-p yason:true false)
            "destructiveHint" (if destructive-p yason:true false)))))
    (when task-required-p
      (setf
       (gethash "execution" definition)
       (json-object "taskSupport" "required")))
    definition))

(-> test-mcp--handler (test-mcp-transport hash-table) hash-table)
(defun test-mcp--handler (transport request)
  "Return deterministic initialization, tool, and resource responses."
  (declare (ignore transport))
  (let ((method (json-get request "method")))
    (cond
      ((string= method "initialize")
       (test-mcp--rpc-result
        request
        (json-object
         "protocolVersion" "2025-11-25"
         "capabilities"
         (json-object
          "tools" (json-object "listChanged" false)
          "resources" (json-object "subscribe" false)
          "prompts" (json-object "listChanged" false))
         "serverInfo"
         (json-object "name" "autolith-test" "version" "1")
         "instructions" "Use only the deterministic test fixture.")))
      ((string= method "tools/list")
       (test-mcp--rpc-result
        request
        (json-object
         "tools"
         (vector
          (test-mcp--tool-definition
           "read file"
           :description "Read a deterministic value."
           :read-only-p t
           :destructive-p nil)
          (test-mcp--tool-definition
           "read/file"
           :description "Exercise a provider-name collision."
           :read-only-p t
           :destructive-p nil)
          (test-mcp--tool-definition
           "mutate"
           :description "Exercise approval metadata.")
          (test-mcp--tool-definition
           "error"
           :description "Return one MCP isError result.")
          (test-mcp--tool-definition
           "image"
           :description "Return one MCP image result.")
          (test-mcp--tool-definition
           "background-only"
           :description "Require unsupported MCP task execution."
           :task-required-p t)))))
      ((string= method "tools/call")
       (let ((name (json-get (json-get request "params") "name")))
         (cond
           ((string= name "read file")
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object "type" "text" "text" "first")
               (json-object
                "type" "resource"
                "resource"
                (json-object
                 "uri" "test://embedded"
                 "mimeType" "text/plain"
                 "text" "second")))
              "structuredContent"
              (json-object "answer" 42)
              "isError" false)))
           ((string= name "error")
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object
                "type" "text"
                "text" "server-declared failure"))
              "isError" yason:true)))
           ((string= name "image")
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object
                "type" "text"
                "text" "before image")
               (json-object
                "type" "image"
                "mimeType" "image/png"
                "data" *test-conversation-tiny-png*)
               (json-object
                "type" "text"
                "text" "after image"))
              "isError" false)))
           (t
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object "type" "text" "text" "mutated"))
              "isError" false))))))
      ((string= method "resources/list")
       (test-mcp--rpc-result
        request
        (json-object
         "resources"
         (vector
          (json-object
           "uri" "test://resource"
           "name" "test resource"
           "mimeType" "text/plain")))))
      ((string= method "resources/templates/list")
       (test-mcp--rpc-result
        request
        (json-object
         "resourceTemplates"
         (vector
          (json-object
           "uriTemplate" "test://item/{identifier}"
           "name" "test template")))))
      ((string= method "resources/read")
       (test-mcp--rpc-result
        request
        (json-object
         "contents"
         (vector
          (json-object
           "uri" "test://resource"
           "mimeType" "text/plain"
           "text" "resource body")))))
      ((string= method "prompts/list")
       (test-mcp--rpc-result
        request
        (json-object
         "prompts"
         (vector
          (json-object
           "name" "summarize"
           "description" "Summarize one input.")))))
      ((string= method "prompts/get")
       (test-mcp--rpc-result
        request
        (json-object
         "description" "Resolved deterministic prompt."
         "messages"
         (vector
          (json-object
           "role" "user"
           "content"
           (json-object "type" "text" "text" "Summarize this."))
          (json-object
           "role" "assistant"
           "content"
           (json-object
            "type" "image"
            "mimeType" "image/png"
            "data" *test-conversation-tiny-png*))))))
      (t
       (error "Unexpected scripted MCP method ~S." method)))))

(-> test-mcp--manager
    (configuration)
    (values mcp-manager test-mcp-transport))
(defun test-mcp--manager (configuration)
  "Return a ready scripted MCP manager and its transport."
  (let* ((server-configuration
           (mcp-server-configuration-create
            :name "Test Server"
            :transport
            '(:type :stdio :command "/bin/true")
            :approval :read-only
            :trusted-read-only-tools '("read file")
            :child-tools '("read file")))
         (transport
           (make-instance
            'test-mcp-transport
            :handler #'test-mcp--handler))
         (client
           (make-mcp-client
            transport
            :name "autolith-test"
            :version "1"))
         (runtime
           (make-instance
            'mcp-server-runtime
            :configuration server-configuration
            :registration-source :runtime
            :provider-namespace "mcp__test_server"
            :client client))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list runtime))))
    (mcp-server-runtime-connect runtime)
    (values manager transport)))

(-> test-mcp--tool-with-raw-name
    (tool-registry string)
    (option mcp-provider-tool))
(defun test-mcp--tool-with-raw-name (registry raw-name)
  "Return REGISTRY's MCP tool with exact RAW-NAME."
  (find raw-name
        (tool-registry-tools registry)
        :test #'string=
        :key
        (lambda (tool)
          (and (typep tool 'mcp-provider-tool)
               (mcp-tool-name (mcp-provider-tool-raw-tool tool))))))

(-> test-mcp--context
    (configuration conversation tool-registry)
    tool-context)
(defun test-mcp--context (configuration conversation registry)
  "Return one MCP test tool context."
  (make-instance
   'tool-context
   :configuration configuration
   :worker nil
   :conversation conversation
   :registry registry))

(-> test-mcp--generation-rediscovery () null)
(defun test-mcp--generation-rediscovery ()
  "Test stable rediscovery after a transparent MCP session replacement."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (session 0)
         (resources-expired-p nil)
         (tool-list-expired-p nil)
         (tool-list-request-count 0)
         (server
           (mcp-server-configuration-create
            :name "generation-test"
            :transport '(:type :stdio :command "/bin/true")))
         (transport
           (make-instance
            'test-mcp-transport
            :handler
            (lambda (transport request)
              (let ((method (json-get request "method")))
                (cond
                  ((string= method "initialize")
                   (incf session)
                   (test-mcp--rpc-result
                    request
                    (json-object
                     "protocolVersion" "2025-11-25"
                     "capabilities"
                     (json-object
                      "tools" (json-object)
                      "resources" (json-object))
                     "serverInfo"
                     (json-object
                      "name" "generation-test"
                      "version" "1")
                     "instructions"
                     (format nil "Instructions from session ~D." session))))
                  ((string= method "resources/list")
                   (if (and (= session 1)
                            (not resources-expired-p))
                       (progn
                         (setf resources-expired-p t)
                         (error
                          'mcp-session-expired
                          :message "Expire the first scripted session."
                          :transport transport
                          :cause nil))
                       (test-mcp--rpc-result
                        request
                        (json-object "resources" (vector)))))
                  ((string= method "tools/list")
                   (incf tool-list-request-count)
                   (if (and (= session 2)
                            (not tool-list-expired-p))
                       (progn
                         (setf tool-list-expired-p t)
                         (error
                          'mcp-session-expired
                          :message
                          "Expire the first rediscovery session."
                          :transport transport
                          :cause nil))
                       (test-mcp--rpc-result
                        request
                        (json-object
                         "tools"
                         (vector
                          (test-mcp--tool-definition
                           (if (= session 1)
                               "before-reconnect"
                               "after-reconnect")
                           :read-only-p t
                           :destructive-p nil))))))
                  (t
                   (error
                    "Generation fixture received unexpected method ~S."
                    method)))))))
         (client
           (make-mcp-client transport :name "autolith-test"))
         (runtime
           (make-instance
            'mcp-server-runtime
            :configuration server
            :registration-source :runtime
            :provider-namespace "mcp__generation_test"
            :client client))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list runtime)))
         (first-registry (make-instance 'tool-registry))
         (second-registry (make-instance 'tool-registry)))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect runtime)
           (mcp-tool-registry-register-manager first-registry manager)
           (mcp-tool-registry-register-manager second-registry manager)
           (test-assert
            (and
             (test-mcp--tool-with-raw-name
              first-registry "before-reconnect")
             (test-mcp--tool-with-raw-name
              second-registry "before-reconnect"))
            "both registries begin with the first MCP session's tools")
           (mcp-client-list-resources client)
           (test-assert
            (/=
             (mcp-server-runtime-observed-connection-generation runtime)
             (mcp-client-connection-generation client))
            "transparent MCP reconnection changes the client generation")
           (mcp-tool-registry-refresh first-registry :only-dirty-p t)
           (test-assert
            (and
             tool-list-expired-p
             (null
              (test-mcp--tool-with-raw-name
               first-registry "before-reconnect"))
             (test-mcp--tool-with-raw-name
              first-registry "after-reconnect"))
            "generation mismatch restarts discovery and reconciles one registry")
           (mcp-tool-registry-refresh second-registry :only-dirty-p t)
           (test-assert
            (and
             (null
              (test-mcp--tool-with-raw-name
               second-registry "before-reconnect"))
             (test-mcp--tool-with-raw-name
              second-registry "after-reconnect"))
            "every registry reconciles the rediscovered runtime revision")
           (let ((contributions
                   (mcp-tool-registry-context-contributions
                    first-registry)))
             (test-assert
              (and
               (= session 3)
               (= (length contributions) 1)
               (search
                "session 3"
                (context-contribution-evidence
                 (first contributions))))
              "rediscovery publishes instructions from the stable session"))
           (test-assert
            (= 5 tool-list-request-count)
            "pagination and tool discovery both restart after generation churn"))
      (ignore-errors (mcp-server-runtime-close runtime))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp--generation-rediscovery-bound () null)
(defun test-mcp--generation-rediscovery-bound ()
  "Test bounded failure when every MCP tool discovery changes sessions."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (session 0)
         (churn-request-count 0)
         (server
           (mcp-server-configuration-create
            :name "unstable-generation-test"
            :transport '(:type :stdio :command "/bin/true")))
         (transport
           (make-instance
            'test-mcp-transport
            :handler
            (lambda (transport request)
              (declare (ignore transport))
              (let ((method (json-get request "method")))
                (cond
                  ((string= method "initialize")
                   (incf session)
                   (test-mcp--rpc-result
                    request
                    (json-object
                     "protocolVersion" "2025-11-25"
                     "capabilities" (json-object "tools" (json-object))
                     "serverInfo"
                     (json-object
                      "name" "unstable-generation-test"
                      "version" "1"))))
                  ((string= method "tools/list")
                   (test-mcp--rpc-result
                    request
                    (json-object
                     "tools"
                     (vector
                      (test-mcp--tool-definition
                       (format nil "session-~D-tool" session))))))
                  (t
                   (error
                    "Unstable generation fixture received unexpected method ~S."
                    method)))))))
         (client
           (make-mcp-client transport :name "autolith-test"))
         (list-tools-function
           (symbol-function 'mcp-client-list-tools))
         (runtime
           (make-instance
            'mcp-server-runtime
            :configuration server
            :registration-source :runtime
            :provider-namespace "mcp__unstable_generation_test"
            :client client)))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect runtime)
           (mcp-server-runtime-request-tool-refresh runtime)
           (let* ((*mcp-tool-discovery-restart-limit* 2)
                  (failure
                    (test-call-with-function-replacements
                     (list
                      (list
                       'mcp-client-list-tools
                       (lambda (target)
                         (let ((tools
                                 (funcall list-tools-function target)))
                           (incf churn-request-count)
                           (mcp-client-close target)
                           (mcp-client-connect target)
                           tools))))
                     (lambda ()
                       (handler-case
                           (progn
                             (mcp-server-runtime-connect runtime)
                             nil)
                         (mcp-server-startup-error (condition)
                           condition))))))
             (test-assert
              (and
               failure
               (= session 3)
               (= churn-request-count 2)
               (eq (mcp-server-runtime-state runtime) :failed)
               (null (mcp-server-runtime-tools runtime))
               (search "2 consecutive tool discovery attempts"
                       (format nil "~A" failure)))
              "continual generation churn stops at the discovery restart bound")))
      (ignore-errors (mcp-server-runtime-close runtime))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp--empty-context (request-context) null)
(defun test-mcp--empty-context (request)
  "Return no request-local context for reload transaction tests."
  (declare (ignore request))
  nil)

(-> test-mcp--reload-command (symbol string) application-command)
(defun test-mcp--reload-command (definition-name name)
  "Return one inert application command for reload transaction tests."
  (application-command-create
   :definition-name definition-name
   :name name
   :aliases nil
   :argument nil
   :description "Exercise MCP reload registry rollback."
   :tip "exists only during the MCP reload transaction test."
   :busy-behavior ':inspect
   :terminal-behavior ':shared
   :handler
   (lambda (application invocation)
     (declare (ignore application invocation))
     ':continue)))


;;;; -- Reload Transaction Tests --

(-> test-mcp-reload-registry-rollback () null)
(defun test-mcp-reload-registry-rollback ()
  "Test failed MCP reload restores MCP, context, and command registrations."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (mcp-pathname (configuration-mcp-path configuration))
         (init-pathname (configuration-user-init-path configuration))
         (original-mcp-registrations (mcp--registry-snapshot))
         (original-context-registrations (context--registry-snapshot))
         (original-command-registrations
           (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (mcp--registry-restore nil)
           (context--registry-restore nil)
           (application-command--registry-restore nil)
           (register-mcp-server
            '(:name "mcp-reload-old"
              :transport (:type :stdio :command "/bin/true"))
            :source ':user)
           (register-context-contributor
            "mcp-reload-old-context"
            'test-mcp--empty-context
            :source ':user)
           (register-application-command
            (test-mcp--reload-command
             'test-mcp--reload-old-command
             "/mcp-reload-old")
            :source ':user)
           (let* ((mcp-registrations (mcp--registry-snapshot))
                  (context-registrations (context--registry-snapshot))
                  (command-registrations
                    (application-command--registry-snapshot))
                  (old-tool-registry (make-instance 'tool-registry))
                  (application
                    (make-instance
                     'application
                     :configuration configuration
                     :tool-registry old-tool-registry)))
             (with-open-file (stream mcp-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (write-string
                "(:version 1
                   :servers
                   ((:name \"mcp-reload-required-missing\"
                     :transport
                     (:type :stdio
                      :command \"/autolith-tests/no-such-mcp-server\")
                     :required-p t
                     :startup-timeout-seconds 1)))"
                stream))
             (with-open-file (stream init-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (write-string
                "(progn
                   (register-context-contributor
                    \"mcp-reload-new-context\"
                    'test-mcp--empty-context)
                   (register-application-command
                    (test-mcp--reload-command
                     'test-mcp--reload-new-command
                     \"/mcp-reload-new\")))"
                stream))
             (test-assert
              (handler-case
                  (progn
                    (application-reload-mcp application)
                    nil)
                (mcp-server-startup-error (condition)
                  (and
                   (mcp-server-startup-error-required-p condition)
                   (string=
                    (mcp-server-startup-error-server-name condition)
                    "mcp-reload-required-missing"))))
              "a required MCP startup failure aborts the complete reload")
             (test-assert
              (equal mcp-registrations (mcp--registry-snapshot))
              "failed MCP reload restores the exact MCP registration layers")
             (test-assert
              (equal context-registrations (context--registry-snapshot))
              "failed MCP reload restores the exact context registration layers")
             (test-assert
              (equal command-registrations
                     (application-command--registry-snapshot))
              "failed MCP reload restores the exact command registration layers")
             (test-assert
              (and
               (find "mcp-reload-old-context"
                     (context-contributor-registrations)
                     :test #'string=
                     :key (lambda (registration)
                            (getf registration :identifier)))
               (null
                (find "mcp-reload-new-context"
                      (context-contributor-registrations)
                      :test #'string=
                      :key (lambda (registration)
                             (getf registration :identifier))))
               (application-command-find "/mcp-reload-old")
               (null (application-command-find "/mcp-reload-new")))
              "failed MCP reload keeps old user policy and rejects new policy")
             (let ((server-names
                     (mapcar
                      (lambda (registration)
                        (mcp-server-configuration-name
                         (mcp-server-registration-configuration registration)))
                      (mcp-server-registrations))))
               (test-assert
                (and
                 (member "mcp-reload-old" server-names :test #'string=)
                 (not
                  (member "mcp-reload-required-missing"
                          server-names
                          :test #'string=)))
                "failed MCP reload keeps old servers and rejects new servers"))
             (test-assert
              (eq (application-tool-registry application) old-tool-registry)
              "failed MCP reload retains the live tool registry")))
      (mcp--registry-restore original-mcp-registrations)
      (context--registry-restore original-context-registrations)
      (application-command--registry-restore original-command-registrations)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp-reload-registry-isolation () null)
(defun test-mcp-reload-registry-isolation ()
  "Test reload publishes one registry generation and preserves later writers."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (mcp-pathname (configuration-mcp-path configuration))
         (init-pathname (configuration-user-init-path configuration))
         (original-mcp-registrations (mcp--registry-snapshot))
         (original-context-registrations (context--registry-snapshot))
         (original-command-registrations
           (application-command--registry-snapshot))
         (barrier-lock (make-lock "Autolith MCP reload isolation barrier"))
         (barrier (make-condition-variable))
         (reload-paused-p nil)
         (release-reload-p nil)
         (state-lock (make-lock "Autolith MCP reload isolation state"))
         (reload-failure nil)
         (reload-finished-p nil)
         (reader-result nil)
         (reader-finished-p nil)
         (writer-finished-p nil)
         (reload-thread nil)
         (reader-thread nil)
         (writer-thread nil))
    (labels
        ((mcp-names (registrations)
           "Return raw server names from MCP REGISTRATIONS."
           (mapcar
            (lambda (registration)
              (mcp-server-configuration-name
               (mcp-server-registration-configuration registration)))
            registrations))

         (context-identifiers (registrations)
           "Return contributor identifiers from REGISTRATIONS."
           (mapcar
            (lambda (registration)
              (getf registration :identifier))
            registrations))

         (command-names (registrations)
           "Return canonical command names from REGISTRATIONS."
           (mapcar
            (lambda (registration)
              (application-command-name
               (application-command-registration-command registration)))
            registrations))

         (release-reload ()
           "Release the deterministic reload construction barrier."
           (with-lock-held (barrier-lock)
             (setf release-reload-p t)
             (condition-notify barrier))))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (mcp--registry-restore nil)
             (context--registry-restore nil)
             (application-command--registry-restore nil)
             (register-mcp-server
              '(:name "isolation-old"
                :transport (:type :stdio :command "/bin/true"))
              :source ':user)
             (register-context-contributor
              "isolation-old-context"
              'test-mcp--empty-context
              :source ':user)
             (register-application-command
              (test-mcp--reload-command
               'test-mcp--reload-isolation-old-command
               "/mcp-isolation-old")
              :source ':user)
             (with-open-file (stream mcp-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (write-string
                "(:version 1
                   :servers
                   ((:name \"isolation-new\"
                     :transport
                     (:type :stdio :command \"/bin/true\"))))"
                stream))
             (with-open-file (stream init-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (write-string
                "(progn
                   (register-context-contributor
                    \"isolation-new-context\"
                    'test-mcp--empty-context)
                   (register-application-command
                    (test-mcp--reload-command
                     'test-mcp--reload-isolation-new-command
                     \"/mcp-isolation-new\")))"
                stream))
             (let* ((old-tool-registry (make-instance 'tool-registry))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :tool-registry old-tool-registry)))
               (test-call-with-function-replacements
                (list
                 (list
                  'application--create-tool-registry
                  (lambda (new-configuration)
                    (declare (ignore new-configuration))
                    (with-lock-held (barrier-lock)
                      (setf reload-paused-p t)
                      (condition-notify barrier)
                      (loop until release-reload-p
                            do (condition-wait barrier barrier-lock)))
                    (error "Injected isolated reload failure."))))
                (lambda ()
                  (setf reload-thread
                        (make-thread
                         (lambda ()
                           (setf reload-failure
                                 (handler-case
                                     (progn
                                       (application-reload-mcp application)
                                       nil)
                                   (simple-error (cause)
                                     cause)))
                           (with-lock-held (state-lock)
                             (setf reload-finished-p t)))
                         :name "Autolith isolated MCP reload"))
                  (with-lock-held (barrier-lock)
                    (loop until reload-paused-p
                          unless
                            (condition-wait
                             barrier barrier-lock :timeout 2)
                            do (error
                                "Timed out waiting for isolated MCP reload.")))
                  (setf reader-thread
                        (make-thread
                         (lambda ()
                           (let ((snapshot
                                   (list
                                    (mcp--registry-snapshot)
                                    (context--registry-snapshot)
                                    (application-command--registry-snapshot))))
                             (with-lock-held (state-lock)
                               (setf reader-result snapshot
                                     reader-finished-p t))))
                         :name "Autolith MCP registry generation reader")
                        writer-thread
                        (make-thread
                         (lambda ()
                           (with-extension-registry-transaction
                             (register-mcp-server
                              '(:name "isolation-concurrent"
                                :transport
                                (:type :stdio :command "/bin/true"))
                              :source ':runtime)
                             (register-context-contributor
                              "isolation-concurrent-context"
                              'test-mcp--empty-context
                              :source ':runtime)
                             (register-application-command
                              (test-mcp--reload-command
                               'test-mcp--reload-isolation-concurrent-command
                               "/mcp-isolation-concurrent")
                              :source ':runtime))
                           (with-lock-held (state-lock)
                             (setf writer-finished-p t)))
                         :name "Autolith MCP concurrent registry writer"))
                  (sleep 0.05)
                  (test-assert
                   (with-lock-held (state-lock)
                     (and (not reload-finished-p)
                          (not reader-finished-p)
                          (not writer-finished-p)))
                   "reload isolation blocks readers and later writers at its publication boundary")
                  (release-reload)
                  (join-thread reload-thread)
                  (join-thread reader-thread)
                  (join-thread writer-thread))))
             (test-assert
              (and reload-failure reload-finished-p
                   reader-finished-p writer-finished-p)
              "the isolated reload fails before every waiting registry operation completes")
             (destructuring-bind
                 (reader-mcp reader-context reader-commands)
                 reader-result
               (let* ((reader-mcp-names (mcp-names reader-mcp))
                      (reader-context-identifiers
                        (context-identifiers reader-context))
                      (reader-command-names (command-names reader-commands))
                      (concurrent-count
                        (count
                         t
                         (list
                          (not
                           (null
                            (member "isolation-concurrent"
                                    reader-mcp-names :test #'string=)))
                          (not
                           (null
                            (member "isolation-concurrent-context"
                                    reader-context-identifiers
                                    :test #'string=)))
                          (not
                           (null
                            (member "/mcp-isolation-concurrent"
                                    reader-command-names
                                    :test #'string=)))))))
                 (test-assert
                  (and
                   (member "isolation-old" reader-mcp-names :test #'string=)
                   (member "isolation-old-context"
                           reader-context-identifiers :test #'string=)
                   (member "/mcp-isolation-old"
                           reader-command-names :test #'string=)
                   (not
                    (member "isolation-new"
                            reader-mcp-names :test #'string=))
                   (not
                    (member "isolation-new-context"
                            reader-context-identifiers :test #'string=))
                   (not
                    (member "/mcp-isolation-new"
                            reader-command-names :test #'string=))
                   (member concurrent-count '(0 3)))
                  "a concurrent reader sees one complete restored registry generation")))
             (test-assert
              (and
               (member "isolation-concurrent"
                       (mcp-names (mcp--registry-snapshot))
                       :test #'string=)
               (member "isolation-concurrent-context"
                       (context-identifiers (context--registry-snapshot))
                       :test #'string=)
               (member "/mcp-isolation-concurrent"
                       (command-names
                        (application-command--registry-snapshot))
                       :test #'string=))
              "a complete caller transaction after failed reload is not erased"))
        (release-reload)
        (dolist (thread (list reload-thread reader-thread writer-thread))
          (when (and thread (thread-alive-p thread))
            (join-thread thread)))
        (mcp--registry-restore original-mcp-registrations)
        (context--registry-restore original-context-registrations)
        (application-command--registry-restore
         original-command-registrations)
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist :ignore))))
  nil)

(-> test-mcp-reload-transaction-boundary () null)
(defun test-mcp-reload-transaction-boundary ()
  "Test pre-commit rollback and post-commit cleanup failure isolation."
  (labels ((run-case (stage)
             "Exercise one late failure STAGE in a complete reload."
             (let* ((configuration (test-configuration))
                    (root (test-configuration-root configuration))
                    (mcp-pathname (configuration-mcp-path configuration))
                    (init-pathname
                      (configuration-user-init-path configuration))
                    (original-mcp-registrations (mcp--registry-snapshot))
                    (original-context-registrations
                      (context--registry-snapshot))
                    (original-command-registrations
                      (application-command--registry-snapshot))
                    (registry-create
                      (symbol-function
                       'application--create-tool-registry))
                    (presentation-connect
                      (symbol-function
                       'application-connect-task-presentation))
                    (agent-create-function
                      (symbol-function 'agent-create))
                    (registry-close
                      (symbol-function
                       'tool-registry-close-runtime-state))
                    (context-reset
                      (symbol-function 'context-runtime-reset))
                    (old-registry nil)
                    (new-registry nil)
                    (new-registry-closed-p nil)
                    (context-reset-p nil)
                    (worker nil)
                    (application nil))
               (unwind-protect
                    (progn
                      (configuration-ensure-directories configuration)
                      (mcp--registry-restore nil)
                      (context--registry-restore nil)
                      (application-command--registry-restore nil)
                      (register-context-contributor
                       "mcp-reload-late-old-context"
                       'test-mcp--empty-context
                       :source ':user)
                      (register-application-command
                       (test-mcp--reload-command
                        'test-mcp--reload-late-old-command
                        "/mcp-reload-late-old")
                       :source ':user)
                      (with-open-file (stream mcp-pathname
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create
                                              :external-format :utf-8)
                        (write-string
                         "(:version 1 :servers ())"
                         stream))
                      (with-open-file (stream init-pathname
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create
                                              :external-format :utf-8)
                        (write-string
                         "(progn
                            (register-context-contributor
                             \"mcp-reload-late-new-context\"
                             'test-mcp--empty-context)
                            (register-application-command
                             (test-mcp--reload-command
                              'test-mcp--reload-late-new-command
                              \"/mcp-reload-late-new\")))"
                         stream))
                      (let* ((conversation
                               (conversation-create
                                configuration
                                :identifier
                                (format nil "mcp-reload-late-~(~A~)"
                                        stage)))
                             (provider (provider-create configuration)))
                        (setf old-registry
                              (funcall registry-create configuration)
                              worker
                              (lisp-worker-pool-create configuration))
                        (let ((agent
                                (agent-create
                                 :configuration configuration
                                 :provider provider
                                 :conversation conversation
                                 :tool-registry old-registry
                                 :worker worker)))
                          (setf application
                                (make-instance
                                 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :provider provider
                                 :tool-registry old-registry
                                 :worker worker
                                 :agent agent
                                 :ui
                                 (terminal-ui-create
                                  :terminal
                                  (make-instance
                                   'recording-terminal
                                   :columns 72))))))
                      (application-connect-task-presentation application)
                      (let ((mcp-registrations
                              (mcp--registry-snapshot))
                            (context-registrations
                              (context--registry-snapshot))
                            (command-registrations
                              (application-command--registry-snapshot))
                            (old-agent (application-agent application))
                            (old-orchestrator
                              (application--task-orchestrator application))
                            (failure-p nil))
                        (test-call-with-function-replacements
                         (list
                          (list
                           'context-runtime-reset
                           (lambda ()
                             (setf context-reset-p t)
                             (funcall context-reset)))
                          (list
                           'application--create-tool-registry
                           (lambda (new-configuration)
                             (setf new-registry
                                   (funcall
                                    registry-create new-configuration))))
                          (list
                           'agent-create
                           (lambda (&rest arguments)
                             (when (eq stage ':agent-creation)
                               (error
                                "Injected reload agent creation failure."))
                             (apply agent-create-function arguments)))
                          (list
                           'application-connect-task-presentation
                           (lambda (target)
                             (prog1
                                 (funcall presentation-connect target)
                               (when
                                   (and
                                    (eq stage ':presentation)
                                    (eq target application)
                                    (eq
                                     (application-tool-registry target)
                                     new-registry))
                                 (error
                                  "Injected reload presentation failure.")))))
                          (list
                           'tool-registry-close-runtime-state
                           (lambda (registry)
                             (cond
                               ((eq registry old-registry)
                                (funcall registry-close registry)
                                (when (eq stage ':old-runtime-close)
                                  (error
                                   "Injected reload old-runtime close failure.")))
                               (t
                                (when (eq registry new-registry)
                                  (setf new-registry-closed-p t))
                                (funcall registry-close registry))))))
                         (lambda ()
                           (setf failure-p
                                 (handler-case
                                     (progn
                                       (application-reload-mcp application)
                                       nil)
                                   (error (condition)
                                     (case stage
                                       (:agent-creation
                                        (and
                                         (typep condition 'simple-error)
                                         (search
                                          "Injected reload agent creation"
                                          (format nil "~A" condition))
                                         condition))
                                       (:presentation
                                        (and
                                         (typep
                                          condition
                                          'application-runtime-replacement-error)
                                         (eq
                                          (application-runtime-replacement-error-operation
                                           condition)
                                          ':mcp-reload)
                                         (eq
                                          (application-runtime-replacement-error-stage
                                           condition)
                                          ':install)
                                         (search
                                          "Injected reload presentation"
                                          (format
                                           nil
                                           "~A"
                                           (application-runtime-replacement-error-cause
                                            condition)))
                                         condition))
                                       (t
                                        condition)))))))
                        (ecase stage
                          ((:agent-creation :presentation)
                           (test-assert
                            failure-p
                            "reload propagates a pre-commit replacement failure")
                           (when (eq stage ':presentation)
                             (test-assert
                              (and
                               (typep
                                failure-p
                                'application-runtime-replacement-error)
                               (null
                                (application-runtime-replacement-error-rollback-causes
                                 failure-p)))
                              "presentation failure reports structured installation rollback"))
                           (test-assert
                            (and
                             (eq (application-tool-registry application)
                                 old-registry)
                             (eq (application-agent application) old-agent)
                             (eq
                              (agent-tool-registry
                               (application-agent application))
                              old-registry)
                             (eq
                              (task-orchestrator-lifecycle-state
                               old-orchestrator)
                              ':open)
                             (application-task-presentation-listener
                              application)
                             (not context-reset-p))
                            "pre-commit failure restores the live old application")
                           (test-assert
                            (and
                             new-registry
                             new-registry-closed-p
                             (eq
                              (task-orchestrator-lifecycle-state
                               (task-run-tool-orchestrator
                                (tool-registry-find
                                 new-registry "task" "run")))
                              ':closed))
                            "pre-commit failure closes the replacement runtime")
                           (test-assert
                            (and
                             (equal mcp-registrations
                                    (mcp--registry-snapshot))
                             (equal context-registrations
                                    (context--registry-snapshot))
                             (equal
                              command-registrations
                              (application-command--registry-snapshot))
                             (application-command-find
                              "/mcp-reload-late-old")
                             (null
                              (application-command-find
                               "/mcp-reload-late-new")))
                            "pre-commit failure restores every registration layer"))
                          (:old-runtime-close
                           (let ((new-orchestrator
                                   (application--task-orchestrator
                                    application)))
                             (test-assert
                              (not failure-p)
                              "old-runtime cleanup failure does not fail reload")
                             (test-assert
                              (and
                               (eq
                                (application-tool-registry application)
                                new-registry)
                               (not
                                (eq (application-agent application)
                                    old-agent))
                               (eq
                                (agent-tool-registry
                                 (application-agent application))
                                new-registry)
                               (eq
                                (application-task-presentation-orchestrator
                                 application)
                                new-orchestrator))
                              "cleanup failure leaves the replacement application installed")
                             (test-assert
                             (and
                               context-reset-p
                               (not new-registry-closed-p)
                               (eq
                                (task-orchestrator-lifecycle-state
                                 new-orchestrator)
                                ':open)
                               (application-task-presentation-listener
                                application))
                              "cleanup failure leaves the replacement runtime live")
                             (test-assert
                              (and
                               (find
                                "mcp-reload-late-new-context"
                                (context-contributor-registrations)
                                :test #'string=
                                :key
                                (lambda (registration)
                                  (getf registration :identifier)))
                               (null
                                (find
                                 "mcp-reload-late-old-context"
                                 (context-contributor-registrations)
                                 :test #'string=
                                 :key
                                 (lambda (registration)
                                   (getf registration :identifier))))
                               (application-command-find
                                "/mcp-reload-late-new")
                               (null
                                (application-command-find
                                 "/mcp-reload-late-old")))
                              "cleanup failure retains newly committed registrations"))))))
                 (when application
                   (ignore-errors
                     (application-disconnect-task-presentation
                      application)))
                 (when old-registry
                   (ignore-errors
                     (funcall registry-close old-registry)))
                 (when (and application
                            (slot-boundp application 'tool-registry)
                            (not
                             (eq (application-tool-registry application)
                                 old-registry)))
                   (ignore-errors
                     (funcall
                      registry-close
                      (application-tool-registry application))))
                 (when worker
                   (ignore-errors
                     (lisp-worker-manager-stop worker)))
                 (mcp--registry-restore original-mcp-registrations)
                 (context--registry-restore
                  original-context-registrations)
                 (application-command--registry-restore
                  original-command-registrations)
                 (uiop:delete-directory-tree
                  root :validate t :if-does-not-exist :ignore)))))
    (run-case ':agent-creation)
    (run-case ':presentation)
    (run-case ':old-runtime-close))
  nil)


;;;; -- Credential Echo Containment --

(-> test-mcp--credential-echo-handler
    (string test-mcp-transport hash-table)
    hash-table)
(defun test-mcp--credential-echo-handler
    (environment-name transport request)
  "Echo the current credential through every server-controlled result surface."
  (declare (ignore transport))
  (let ((credential (uiop:getenv environment-name))
        (method (json-get request "method")))
    (cond
      ((string= method "initialize")
       (test-mcp--rpc-result
        request
        (json-object
         "protocolVersion" "2025-11-25"
         "capabilities"
         (json-object
          "tools" (json-object "credential" credential)
          "resources" (json-object "credential" credential)
          "prompts" (json-object "credential" credential))
         "serverInfo"
         (json-object
          "name" "credential-echo"
          "version" credential)
         "instructions"
         (format nil "Server instructions echoed ~A." credential))))
      ((string= method "tools/list")
       (test-mcp--rpc-result
        request
        (json-object
         "tools"
         (vector
          (json-object
           "name" (format nil "echo-~A" credential)
           "title" (format nil "Title ~A" credential)
           "description" (format nil "Description ~A" credential)
           "inputSchema"
           (json-object
            "type" "object"
            "properties"
            (json-object
             "mode"
             (json-object
              "type" "string"
              "description" credential))
            "additionalProperties" false)
           "outputSchema"
           (json-object
            "type" "object"
            "description" credential)
           "annotations"
           (json-object
            "title" credential
            "readOnlyHint" yason:true
            "destructiveHint" false)
           "_meta" (json-object "credential" credential))))))
      ((string= method "tools/call")
       (let* ((params (json-get request "params"))
              (arguments (json-get params "arguments"))
              (mode (json-get arguments "mode")))
         (cond
           ((and (stringp mode) (string= mode "exception"))
            (error "MCP exception echoed ~A." credential))
           ((and (stringp mode) (string= mode "failure"))
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object
                "type" "text"
                "text" (format nil "Failure echoed ~A." credential)))
              "isError" yason:true)))
           (t
            (test-mcp--rpc-result
             request
             (json-object
              "content"
              (vector
               (json-object
                "type" "text"
                "text" (format nil "Success echoed ~A." credential)))
              "structuredContent"
              (json-object "credential" credential)
              "isError" false))))))
      ((string= method "resources/list")
       (test-mcp--rpc-result
        request
        (json-object
         "resources"
         (vector
          (json-object
           "uri" "test://credential"
           "name" (format nil "Resource ~A" credential)
           "description" credential)))))
      ((string= method "resources/templates/list")
       (test-mcp--rpc-result
        request
        (json-object
         "resourceTemplates"
         (vector
          (json-object
           "uriTemplate" "test://credential/{value}"
           "name" (format nil "Template ~A" credential)
           "description" credential)))))
      ((string= method "resources/read")
       (test-mcp--rpc-result
        request
        (json-object
         "contents"
         (vector
          (json-object
           "uri" "test://credential"
           "mimeType" "text/plain"
           "text" (format nil "Resource body ~A." credential))))))
      ((string= method "prompts/list")
       (test-mcp--rpc-result
        request
        (json-object
         "prompts"
         (vector
          (json-object
           "name" "credential"
           "description" (format nil "Prompt ~A" credential))))))
      ((string= method "prompts/get")
       (test-mcp--rpc-result
        request
        (json-object
         "description" (format nil "Resolved prompt ~A." credential)
         "messages"
         (vector
          (json-object
           "role" "user"
           "content"
           (json-object
            "type" "text"
            "text" (format nil "Prompt body ~A." credential)))))))
      (t
       (error "Unexpected credential-echo method ~S." method)))))

(-> test-mcp--object-contains-string-p (t string) boolean)
(defun test-mcp--object-contains-string-p (root needle)
  "Return true when reachable ordinary object ROOT contains string NEEDLE."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (value)
               "Search VALUE without invoking application accessors."
               (cond
                 ((stringp value)
                  (not (null (search needle value))))
                 ((or (null value)
                      (numberp value)
                      (characterp value)
                      (symbolp value)
                      (pathnamep value)
                      (functionp value))
                  nil)
                 ((gethash value seen)
                  nil)
                 ((consp value)
                  (setf (gethash value seen) t)
                  (or (visit (first value))
                      (visit (rest value))))
                 ((hash-table-p value)
                  (setf (gethash value seen) t)
                  (loop for key being the hash-keys of value
                          using (hash-value child)
                        thereis
                        (or (visit key) (visit child))))
                 ((vectorp value)
                  (setf (gethash value seen) t)
                  (loop for child across value
                        thereis (visit child)))
                 ((typep value 'standard-object)
                  (setf (gethash value seen) t)
                  (handler-case
                      (loop for slot in (class-slots (class-of value))
                            for name = (slot-definition-name slot)
                            thereis
                            (and
                             (slot-boundp value name)
                             (visit (slot-value value name))))
                    (error ()
                      nil)))
                 (t
                  nil))))
      (and (visit root) t))))

(-> test-mcp-retained-tool-metadata-boundaries () null)
(defun test-mcp-retained-tool-metadata-boundaries ()
  "Test retained MCP tools discard unused metadata and bound input schemas."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (environment-name
           (format nil
                   "AUTOLITH_MCP_METADATA_~A"
                   (remove #\- (string-upcase (make-identifier)))))
         (credential
           (format nil "mcp-unused-secret-~A" (make-identifier)))
         (oversized-output
           (concatenate
            'string
            credential
            (make-string
             (1+ *mcp-maximum-tool-schema-bytes*)
             :initial-element #\X)))
         (server
           (mcp-server-configuration-create
            :name "metadata-boundary"
            :transport
            `(:type :stdio
              :command "/bin/true"
              :environment
              (("SERVICE_TOKEN" :environment ,environment-name)))
            :approval ':read-only
            :trusted-read-only-tools '("bounded")))
         (transport nil)
         (runtime nil)
         (manager nil)
         (registry nil)
         (conversation nil)
         (poisoned nil)
         (input-runtime nil))
    (unwind-protect
         (progn
           (sb-posix:setenv environment-name credential 1)
           (setf
            transport
            (make-instance
             'test-mcp-transport
             :handler
             (lambda (transport request)
               (declare (ignore transport))
               (let ((method (json-get request "method")))
                 (cond
                   ((string= method "initialize")
                    (test-mcp--rpc-result
                     request
                     (json-object
                      "protocolVersion" "2025-11-25"
                      "capabilities" (json-object "tools" (json-object))
                      "serverInfo"
                      (json-object
                       "name" "metadata-boundary"
                       "version" "1"))))
                   ((string= method "tools/list")
                    (test-mcp--rpc-result
                     request
                     (json-object
                      "tools"
                      (vector
                       (json-object
                        "name" "bounded"
                        "title" "Bounded tool"
                        "description" "Use one bounded input."
                        "inputSchema"
                        (json-object
                         "type" "object"
                         "properties"
                         (json-object
                          "value" (json-object "type" "string")))
                        "outputSchema"
                        (json-object
                         "type" "object"
                         "description" oversized-output)
                        "annotations"
                        (json-object
                         "title" credential
                         "readOnlyHint" yason:true
                         "destructiveHint" false
                         "idempotentHint" yason:true
                         "openWorldHint" false
                         "unused" credential)
                        "execution"
                        (json-object
                         "taskSupport" "optional"
                         "unused" credential)
                        "_meta"
                        (json-object
                         "unused" credential
                         "payload" oversized-output))))))
                   (t
                    (error
                     "Metadata fixture received unexpected method ~S."
                     method))))))
            runtime
            (make-instance
             'mcp-server-runtime
             :configuration server
             :registration-source ':runtime
             :provider-namespace "mcp__metadata_boundary"
             :client
             (make-mcp-client
              transport
              :name "autolith-test"
              :version "1"))
            manager
            (make-instance
             'mcp-manager
             :configuration configuration
             :runtimes (list runtime))
            registry (make-instance 'tool-registry)
            conversation
            (conversation-create
             configuration
             :identifier "mcp-metadata-boundary"))
           (mcp-server-runtime-connect runtime)
           (setf
            poisoned
            (let ((*mcp-active-credential-values* (list credential)))
              (mcp-tools--sanitize-tool
               (make-instance
                'mcp-tool
                :name "poisoned-annotations"
                :description "Reject non-boolean policy annotations."
                :input-schema
                (json-object
                 "type" "object"
                 "properties" (json-object))
                :annotations
                (json-object
                 "readOnlyHint" credential
                 "destructiveHint"
                 (json-object "secret" credential))))))
           (with-lock-held ((mcp-server-runtime-lock runtime))
             (setf (mcp-server-runtime-tools runtime)
                   (append
                    (mcp-server-runtime-tools runtime)
                    (list poisoned)))
             (incf
              (mcp-server-runtime-tool-schema-bytes runtime)
              (length
               (sb-ext:string-to-octets
                (json-encode (mcp-tool-input-schema poisoned))
                :external-format :utf-8)))
             (incf (mcp-server-runtime-tools-revision runtime)))
           (mcp-tool-registry-register-manager registry manager)
           (let* ((retained
                    (first (mcp-server-runtime-tools runtime)))
                  (provider-tool
                    (test-mcp--tool-with-raw-name registry "bounded"))
                  (poisoned-provider-tool
                    (test-mcp--tool-with-raw-name
                     registry
                     "poisoned-annotations"))
                  (annotations (mcp-tool-annotations retained))
                  (raw-unbound-p
                    (handler-case
                        (progn
                          (mcp-tool-raw retained)
                          nil)
                      (unbound-slot ()
                        t))))
             (test-assert
              (and
               (string= (mcp-tool-title retained) "Bounded tool")
               (string=
                (mcp-tool-description retained)
                "Use one bounded input.")
               (string= (mcp-tool-task-support retained) "optional")
               (null (mcp-tool-output-schema retained))
               (null (mcp-tool-execution retained))
               raw-unbound-p
               (hash-table-p annotations)
               (= (hash-table-count annotations) 2)
               (mcp-tool-read-only-p retained)
               (not (mcp-tool-destructive-p retained)))
              "retained MCP tools contain only provider and policy metadata")
             (test-assert
              (and
               poisoned
               poisoned-provider-tool
               (null (mcp-tool-annotations poisoned))
               (not (mcp-tool-read-only-p poisoned))
               (mcp-tool-destructive-p poisoned))
              "non-boolean policy annotations are discarded as untrusted metadata")
             (test-assert
              (and
               provider-tool
               (not
                (test-mcp--object-contains-string-p
                 runtime credential))
               (not
                (test-mcp--object-contains-string-p
                 runtime *mcp-credential-redaction-marker*))
               (not
                (test-mcp--object-contains-string-p
                 provider-tool credential))
               (not
                (test-mcp--object-contains-string-p
                 provider-tool *mcp-credential-redaction-marker*))
               (not
                (test-mcp--object-contains-string-p
                 poisoned-provider-tool credential))
               (not
                (test-mcp--object-contains-string-p
                 poisoned-provider-tool
                 *mcp-credential-redaction-marker*)))
              "unused MCP metadata cannot survive in runtime or provider tools")
             (let ((application
                     (make-instance
                      'application
                      :configuration configuration
                      :conversation conversation
                      :provider nil
                      :tool-registry registry
                      :worker nil
                      :agent nil
                      :ui nil)))
               (tool-registry-close-runtime-state registry)
               (checkpoint-detach-state application)
               (test-assert
                (and
                 (not
                  (test-mcp--object-contains-string-p
                   application credential))
                 (not
                  (test-mcp--object-contains-string-p
                   application *mcp-credential-redaction-marker*)))
                "unused MCP metadata cannot survive in a checkpoint graph")))
           (let* ((input-server
                    (mcp-server-configuration-create
                     :name "input-boundary"
                     :transport
                     '(:type :stdio :command "/bin/true")))
                  (input-transport
                    (make-instance
                     'test-mcp-transport
                     :handler
                     (lambda (transport request)
                       (declare (ignore transport))
                       (let ((method (json-get request "method")))
                         (cond
                           ((string= method "initialize")
                            (test-mcp--rpc-result
                             request
                             (json-object
                              "protocolVersion" "2025-11-25"
                              "capabilities"
                              (json-object "tools" (json-object))
                              "serverInfo"
                              (json-object
                               "name" "input-boundary"
                               "version" "1"))))
                           ((string= method "tools/list")
                            (test-mcp--rpc-result
                             request
                             (json-object
                              "tools"
                              (vector
                               (json-object
                                "name" "oversized-input"
                                "inputSchema"
                                (json-object
                                 "type" "object"
                                 "description"
                                 (make-string
                                  (1+
                                   *mcp-maximum-tool-schema-string-characters*)
                                  :initial-element #\X)))))))
                           (t
                            (error
                             "Input fixture received unexpected method ~S."
                             method))))))))
             (setf
              input-runtime
              (make-instance
               'mcp-server-runtime
               :configuration input-server
               :registration-source ':runtime
               :provider-namespace "mcp__input_boundary"
               :client
               (make-mcp-client
                input-transport
                :name "autolith-test"
                :version "1")))
             (let ((failure
                     (handler-case
                         (progn
                           (mcp-server-runtime-connect input-runtime)
                           nil)
                       (mcp-server-startup-error (condition)
                         condition))))
               (test-assert
                (and
                 failure
                 (search
                  "oversized schema string"
                  (autolith-error-message failure))
                 (eq (mcp-server-runtime-state input-runtime) :failed)
                 (null (mcp-server-runtime-tools input-runtime)))
                "MCP input schemas remain subject to structural bounds"))))
      (when input-runtime
        (ignore-errors
          (mcp-server-runtime-close input-runtime)))
      (when registry
        (ignore-errors
          (tool-registry-close-runtime-state registry)))
      (when manager
        (ignore-errors
          (mcp-manager-close manager)))
      (sb-posix:unsetenv environment-name)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp--credential-redacted-p (string string) boolean)
(defun test-mcp--credential-redacted-p (rendered credential)
  "Return true when RENDERED contains the marker and omits CREDENTIAL."
  (and (search *mcp-credential-redaction-marker* rendered)
       (not (search credential rendered))
       t))

(-> test-mcp-http-credential-exchange-scope () null)
(defun test-mcp-http-credential-exchange-scope ()
  "Test background HTTP exchanges sanitize transient credential echoes."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registrations (mcp--registry-snapshot))
         (environment-name
           (format nil
                   "AUTOLITH_MCP_HTTP_ECHO_~A"
                   (remove #\- (string-upcase (make-identifier)))))
         (credential
           (format nil "mcp-http-secret-~A" (make-identifier)))
         (manager nil)
         (runtime nil)
         (transport nil)
         (scope-active-p nil)
         (thread-failure nil))
    (unwind-protect
         (progn
           (sb-posix:setenv environment-name credential 1)
           (mcp--registry-restore nil)
           (register-mcp-server
            `(:name "credential-http"
              :transport
              (:type :http
               :url "https://example.test/mcp"
               :headers
               (("Authorization" :environment ,environment-name)))
              :approval :allow)
            :source ':runtime)
           (test-call-with-function-replacements
            (list
             (list
              'mcp-manager--connect-runtimes
              (lambda (candidate &rest arguments)
                (declare (ignore candidate arguments))
                nil)))
            (lambda ()
              (setf manager (mcp-manager-create configuration))))
           (setf runtime (first (mcp-manager-runtimes manager))
                 transport
                 (mcp-client-transport
                  (mcp-server-runtime-client runtime)))
           (let ((thread
                   (make-thread
                    (lambda ()
                      (handler-case
                          (funcall
                           (mcp-http-transport-exchange-scope-function
                            transport)
                           (lambda ()
                             (setf
                              scope-active-p
                              (and
                               (secret-use-active-p)
                               (not
                                (null
                                 (member
                                  credential
                                  *mcp-active-credential-values*
                                  :test #'string=))))
                              (mcp-http-transport-session-identifier
                               transport)
                              (format nil "session echoed ~A" credential)
                              (mcp-http-transport-pending-session-identifier
                               transport)
                              (format nil "pending echoed ~A" credential)
                              (mcp-http-transport-listener-failure transport)
                              (make-condition
                               'simple-error
                               :format-control
                               "listener echoed ~A"
                               :format-arguments (list credential)))))
                        (error (condition)
                          (setf thread-failure condition))))
                    :name "Autolith MCP HTTP credential echo")))
             (join-thread thread))
           (let ((retained
                   (format
                    nil "~A~%~A~%~A"
                    (mcp-http-transport-session-identifier transport)
                    (mcp-http-transport-pending-session-identifier transport)
                    (mcp-http-transport-listener-failure transport))))
             (test-assert
              (and
               (typep transport 'mcp-streamable-http-transport)
               scope-active-p
               (null thread-failure)
               (not (secret-use-active-p))
               (test-mcp--credential-redacted-p retained credential)
               (not
                (test-mcp--object-contains-string-p runtime credential)))
              "HTTP listener exchanges redact credentials before leaving their secret scope")))
      (when manager
        (ignore-errors
          (mcp-manager-close manager)))
      (mcp--registry-restore registrations)
      (sb-posix:unsetenv environment-name)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp-credential-echo-containment () null)
(defun test-mcp-credential-echo-containment ()
  "Test configured credentials cannot survive any MCP result boundary."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (environment-name
           (format nil
                   "AUTOLITH_MCP_ECHO_~A"
                   (remove #\- (string-upcase (make-identifier)))))
         (credential
           (format nil "mcp-secret-~A" (make-identifier)))
         (server
           (mcp-server-configuration-create
            :name "credential-echo"
            :transport
            `(:type :stdio
              :command "/bin/true"
              :environment
              (("SERVICE_TOKEN" :environment ,environment-name)))
            :approval :allow))
         (transport nil)
         (runtime nil)
         (manager nil)
         (registry nil)
         (conversation nil))
    (unwind-protect
         (progn
           (sb-posix:setenv environment-name credential 1)
           (setf
            transport
            (make-instance
             'test-mcp-transport
             :handler
             (lambda (transport request)
               (test-mcp--credential-echo-handler
                environment-name transport request)))
            runtime
            (make-instance
             'mcp-server-runtime
             :configuration server
             :registration-source :runtime
             :provider-namespace "mcp__credential_echo"
             :client
             (make-mcp-client
              transport
              :name "autolith-test"
              :version "1"))
            manager
            (make-instance
             'mcp-manager
             :configuration configuration
             :runtimes (list runtime))
            registry (make-instance 'tool-registry)
            conversation
            (conversation-create
             configuration
             :identifier "mcp-credential-echo"))
           (mcp-server-runtime-connect runtime)
           (mcp-tool-registry-register-manager registry manager)
           (let* ((context
                    (test-mcp--context
                     configuration conversation registry))
                  (provider-tool
                    (find-if
                     (lambda (tool)
                       (typep tool 'mcp-provider-tool))
                     (tool-registry-tools registry)))
                  (resources
                    (tool-registry-find registry "mcp" "resources"))
                  (templates
                    (tool-registry-find
                     registry "mcp" "resource-templates"))
                  (read-resource
                    (tool-registry-find
                     registry "mcp" "read-resource"))
                  (prompts
                    (tool-registry-find registry "mcp" "prompts"))
                  (get-prompt
                    (tool-registry-find registry "mcp" "get-prompt"))
                  (schemas
                    (json-encode
                     (tool-registry-provider-schemas registry)))
                  (identity
                    (format
                     nil "~S~%~A"
                     (tool-authorization-identity-fields provider-tool)
                     (application--tool-authorization-title provider-tool)))
                  (client-state
                    (format
                     nil "~A~%~A~%~A"
                     (mcp-client-instructions
                      (mcp-server-runtime-client runtime))
                     (json-encode
                      (mcp-client-server-capabilities
                       (mcp-server-runtime-client runtime)))
                     (json-encode
                      (mcp-client-server-info
                       (mcp-server-runtime-client runtime)))))
                  (contributions
                    (mcp-tool-registry-context-contributions registry))
                  (context-text
                    (format
                     nil "~{~A~^~%~}"
                     (mapcar
                      #'context-contribution-evidence
                      contributions)))
                  (results
                    (list
                     (tool-execute
                      provider-tool context (json-object))
                     (tool-execute
                      provider-tool
                      context
                      (json-object "mode" "failure"))
                     (tool-execute
                      provider-tool
                      context
                      (json-object "mode" "exception"))
                     (tool-execute
                      resources
                      context
                      (json-object "server" "credential-echo"))
                     (tool-execute
                      templates
                      context
                      (json-object "server" "credential-echo"))
                     (tool-execute
                      read-resource
                      context
                      (json-object
                       "server" "credential-echo"
                       "uri" "test://credential"))
                     (tool-execute
                      prompts
                      context
                      (json-object "server" "credential-echo"))
                     (tool-execute
                      get-prompt
                      context
                      (json-object
                       "server" "credential-echo"
                       "name" "credential")))))
             (test-assert
              (and
               provider-tool
               (=
                (mcp-server-runtime-tool-schema-bytes runtime)
                (loop for tool in (mcp-server-runtime-tools runtime)
                      sum
                      (length
                       (sb-ext:string-to-octets
                        (json-encode (mcp-tool-input-schema tool))
                        :external-format :utf-8))))
               (test-mcp--credential-redacted-p schemas credential)
               (test-mcp--credential-redacted-p identity credential)
               (test-mcp--credential-redacted-p client-state credential)
               (test-mcp--credential-redacted-p context-text credential))
              "credential echoes are redacted and exact retained schemas are counted")
             (test-assert
              (every
               (lambda (result)
                 (test-mcp--credential-redacted-p
                  (tool-result-content result)
                  credential))
               results)
              "credential echoes are redacted from every MCP result surface")
             (loop for result in results
                   for index from 0
                   do
                      (conversation-append-tool-result
                       conversation
                       (format nil "credential-echo-~D" index)
                       :tool-name "mcp"
                       :output (tool-result-content result)
                       :success-p (tool-result-success-p result)))
             (test-assert
              (test-mcp--credential-redacted-p
               (uiop:read-file-string
                (conversation-pathname conversation))
               credential)
              "credential echoes never enter durable conversation records")
             (let* ((stdio-client
                      (mcp-tools--client server configuration))
                    (stdio-runtime
                      (make-instance
                       'mcp-server-runtime
                       :configuration server
                       :registration-source :runtime
                       :provider-namespace "mcp__credential_stderr"
                       :client stdio-client))
                    (stdio-manager
                      (make-instance
                       'mcp-manager
                       :configuration configuration
                       :runtimes (list stdio-runtime)))
                    (stdio-transport
                      (mcp-client-transport stdio-client))
                    (condition-text nil))
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list
                       'mcp-client-connect
                       (lambda (client)
                         (setf
                          (mcp-stdio-transport-stderr-text
                           (mcp-client-transport client))
                          (format nil "stderr echoed ~A." credential))
                         (error "Connection echoed ~A." credential))))
                     (lambda ()
                       (handler-case
                           (mcp-server-runtime-connect stdio-runtime)
                         (mcp-server-startup-error (condition)
                           (setf condition-text
                                 (format
                                  nil "~A~%~A"
                                  (autolith-error-message condition)
                                  (mcp-server-startup-error-cause
                                   condition)))))))
                 (let ((status
                         (mcp-manager-render-status stdio-manager))
                       (stderr
                         (mcp-stdio-transport-stderr-text
                          stdio-transport)))
                   (test-assert
                    (and
                     (test-mcp--credential-redacted-p
                      condition-text credential)
                     (test-mcp--credential-redacted-p
                      status credential)
                     (not (search credential stderr)))
                    "credential echoes are redacted from failures, status, and stderr"))
                 (ignore-errors
                   (mcp-server-runtime-close stdio-runtime))))
             (let* ((provider (provider-create configuration))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation conversation
                       :provider provider
                       :tool-registry registry
                       :worker nil
                       :agent
                       (agent-create
                        :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry registry
                        :worker nil)
                       :ui nil)))
               (tool-registry-close-runtime-state registry)
               (checkpoint-detach-state application)
               (test-assert
                (not
                 (test-mcp--object-contains-string-p
                  application credential))
               "a detached checkpoint graph contains no MCP credential")
               (test-assert
                (notany
                 (lambda (tool)
                   (typep tool 'mcp-provider-tool))
                 (tool-registry-tools registry))
                "checkpoint detachment removes dynamic MCP provider tools")
               (test-assert
                (not
                 (test-mcp--object-contains-string-p
                  runtime *mcp-credential-redaction-marker*))
                "checkpoint detachment removes retained MCP runtime markers"))))
      (when registry
        (ignore-errors
          (tool-registry-close-runtime-state registry)))
      (when manager
        (ignore-errors
          (mcp-manager-close manager)))
      (sb-posix:unsetenv environment-name)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Credential Standard-Input Retention --

(-> test-mcp-credential-stdio-ingress-projection () null)
(defun test-mcp-credential-stdio-ingress-projection ()
  "Test value-independent ingress projection for credential-bearing stdio."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (credential-server
           (mcp-server-configuration-create
            :name "credential-ingress"
            :transport
            '(:type :stdio
              :command "/bin/true"
              :environment
              (("SERVICE_TOKEN"
                :environment
                "AUTOLITH_MCP_INGRESS_TOKEN")))))
         (plain-server
           (mcp-server-configuration-create
            :name "plain-ingress"
            :transport '(:type :stdio :command "/bin/true")))
         (credential-transport
           (mcp-tools--transport credential-server configuration))
         (plain-transport
           (mcp-tools--transport plain-server configuration))
         (credential-projector
           (mcp-stdio-transport-ingress-projector
            credential-transport))
         (plain-projector
           (mcp-stdio-transport-ingress-projector plain-transport)))
    (unwind-protect
         (progn
           (let ((response
                   (json-object
                    "jsonrpc" "2.0"
                    "id" 1
                    "result" (json-object "value" "server result"))))
             (test-assert
              (eq response (funcall credential-projector ':response response))
              "credential stdio preserves guarded response delivery"))
           (test-assert
            (null
             (funcall
              credential-projector ':stderr "raw server diagnostic"))
            "credential stdio drops asynchronous stderr before retention")
           (test-assert
            (string=
             "The MCP stdio reader stopped."
             (funcall
              credential-projector
              ':reader-failure
              (make-condition
               'simple-error
               :format-control "raw server failure"
               :format-arguments nil)))
            "credential stdio replaces reader failures before retention")
           (let* ((params (json-object "secret" "raw notification params"))
                  (notification
                    (json-object
                     "jsonrpc" "2.0"
                     "method" "notifications/tools/list_changed"
                     "params" params))
                  (projected
                    (funcall
                     credential-projector ':notification notification)))
             (test-assert
              (and
               (hash-table-p projected)
               (not (eq notification projected))
               (string=
                (json-get projected "method")
                "notifications/tools/list_changed")
               (eq (json-get projected "params" :absent) :absent))
              "credential stdio retains only a detached tool-change signal"))
           (test-assert
            (null
             (funcall
              credential-projector
              ':notification
              (json-object
               "jsonrpc" "2.0"
               "method" "notifications/progress"
               "params" (json-object "secret" "raw progress"))))
            "credential stdio drops unrelated notifications")
           (let* ((request
                    (json-object
                     "jsonrpc" "2.0"
                     "id" 7
                     "method" "ping"
                     "params" (json-object "secret" "raw request")))
                  (projected
                    (funcall credential-projector ':request request)))
             (test-assert
              (and
               (hash-table-p projected)
               (not (eq request projected))
               (= (json-get projected "id") 7)
               (string= (json-get projected "method") "ping")
               (eq (json-get projected "params" :absent) :absent))
              "credential stdio retains only a detached integer ping request"))
           (test-assert
            (null
             (funcall
              credential-projector
              ':request
              (json-object
               "jsonrpc" "2.0"
               "id" "raw request identifier"
               "method" "sampling/createMessage")))
            "credential stdio rejects server requests that could retain data")
           (let ((value (json-object "value" "ordinary server data")))
             (test-assert
              (eq value (funcall plain-projector ':notification value))
              "stdio without mapped credentials preserves normal diagnostics")))
      (mcp-transport-close credential-transport)
      (mcp-transport-close plain-transport)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp-environment-fingerprint-lifecycle () null)
(defun test-mcp-environment-fingerprint-lifecycle ()
  "Test keyed canonical launch digests and explicit key erasure."
  (let* ((server
           (mcp-server-configuration-create
            :name "fingerprint-lifecycle"
            :transport
            '(:type :stdio
              :command "/bin/true"
              :environment
              (("FIRST_TOKEN" :environment "FIRST_SOURCE")
               ("SECOND_TOKEN" :environment "SECOND_SOURCE")))))
         (bindings
           (mcp-stdio-configuration-environment-bindings
            (mcp-server-configuration-transport server)))
         (first-binding (first bindings))
         (second-binding (second bindings))
         (snapshot
           (list
            (cons first-binding "first value")
            (cons second-binding "second value")))
         (different-value
           (list
            (cons first-binding "first value")
            (cons second-binding "changed value")))
         (boundary-a
           (list
            (cons first-binding "AB")
            (cons second-binding "C")))
         (boundary-b
           (list
            (cons first-binding "A")
            (cons second-binding "BC"))))
    (unwind-protect
         (progn
           (mcp-tools--clear-environment-fingerprint-key)
           (let* ((first
                    (mcp-tools--environment-snapshot-fingerprint snapshot))
                  (repeated
                    (mcp-tools--environment-snapshot-fingerprint snapshot))
                  (changed
                    (mcp-tools--environment-snapshot-fingerprint
                     different-value))
                  (first-boundary
                    (mcp-tools--environment-snapshot-fingerprint boundary-a))
                  (second-boundary
                    (mcp-tools--environment-snapshot-fingerprint boundary-b))
                  (retained-key *mcp-environment-fingerprint-key*))
             (test-assert
              (and
               (= (length first) 32)
               (string= first repeated)
               (not (string= first changed))
               (not (string= first-boundary second-boundary)))
              "one key yields stable collision-resistant canonical digests")
             (mcp-tools--clear-environment-fingerprint-key)
             (test-assert
              (and
               (null *mcp-environment-fingerprint-key*)
               (every #'zerop retained-key))
              "forgetting the fingerprint key erases its original octets")
             (let ((rekeyed
                     (mcp-tools--environment-snapshot-fingerprint snapshot)))
               (test-assert
                (not (string= first rekeyed))
                "a fresh process key changes the retained launch digest"))))
      (mcp-tools--clear-environment-fingerprint-key)))
  nil)


;;;; -- Cleanup Without Credential Availability --

(-> test-mcp--cleanup-stdio-server-form () string)
(defun test-mcp--cleanup-stdio-server-form ()
  "Return one tiny standard-input MCP server form that remains alive for close."
  (let ((*package* (find-package '#:autolith)))
    (write-to-string
     `(progn
        (read-line)
        (write-line
         ,(json-encode
           (json-object
            "jsonrpc" "2.0"
            "id" 1
            "result"
            (json-object
             "protocolVersion" "2025-11-25"
             "capabilities" (json-object "tools" (json-object))
             "serverInfo"
             (json-object "name" "cleanup-fixture" "version" "1")))))
        (finish-output)
        (read-line)
        (read-line)
        (write-line
         ,(json-encode
           (json-object
            "jsonrpc" "2.0"
            "id" 2
            "result" (json-object "tools" #()))))
        (finish-output)
        (loop (sleep 1)))
     :pretty nil)))

(-> test-mcp--rotating-stdio-server-form () string)
(defun test-mcp--rotating-stdio-server-form ()
  "Return a standard-input MCP server that echoes its launch credential."
  (let ((*package* (find-package '#:autolith)))
    (write-to-string
     `(let ((token (sb-ext:posix-getenv "SERVICE_TOKEN")))
        (labels ((request-identifier (line)
                   "Return LINE's numeric JSON-RPC identifier."
                   (let ((marker (search "\"id\":" line)))
                     (and
                      marker
                      (parse-integer
                       line :start (+ marker 5) :junk-allowed t))))

                 (reply (identifier result)
                   "Write one JSON-RPC RESULT for IDENTIFIER."
                   (format
                    t
                    "{\"jsonrpc\":\"2.0\",\"id\":~D,\"result\":~A}~%"
                    identifier
                    result)
                   (finish-output)))
          (loop for line = (read-line nil nil)
                while line
                do
                   (cond
                     ((search "\"initialize\"" line)
                      (reply
                       (request-identifier line)
                       (format
                        nil
                        "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{\"tools\":{}},\"serverInfo\":{\"name\":\"rotation-fixture\",\"version\":\"~A\"},\"instructions\":\"~A\"}"
                        token
                        token)))
                     ((search "\"tools/list\"" line)
                      (reply
                       (request-identifier line)
                       (format
                        nil
                        "{\"tools\":[{\"name\":\"echo\",\"description\":\"~A\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}},\"annotations\":{\"readOnlyHint\":true,\"destructiveHint\":false}}]}"
                        token)))
                     ((search "\"tools/call\"" line)
                      (reply
                       (request-identifier line)
                       (format
                        nil
                        "{\"content\":[{\"type\":\"text\",\"text\":\"~A\"}],\"isError\":false}"
                        token)))))))
     :pretty nil)))

(-> test-mcp--process-alive-p (t) boolean)
(defun test-mcp--process-alive-p (process)
  "Return true when PROCESS is a live UIOP process handle."
  (and
   process
   (handler-case
       (and (uiop:process-alive-p process) t)
     (serious-condition ()
       nil))))

(-> test-mcp-cleanup-without-credential-availability () null)
(defun test-mcp-cleanup-without-credential-availability ()
  "Test registry cleanup cannot be blocked by unavailable MCP credentials."
  (labels ((deny-secret-use (&rest arguments)
             "Reject any attempt to begin a new transient secret scope."
             (declare (ignore arguments))
             (error "A cleanup path attempted to begin transient secret use.")))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (environment-name
             (format nil
                     "AUTOLITH_MCP_CLEANUP_STDIO_~A"
                     (remove #\- (string-upcase (make-identifier)))))
           (server
             (mcp-server-configuration-create
              :name "cleanup-stdio"
              :transport
              `(:type :stdio
                :command ,(lisp-worker-sbcl-command)
                :arguments
                ("--noinform"
                 "--no-sysinit"
                 "--no-userinit"
                 "--disable-debugger"
                 "--non-interactive"
                 "--eval"
                 ,(test-mcp--cleanup-stdio-server-form))
                :environment
                (("SERVICE_TOKEN" :environment ,environment-name)))
              :required-p t))
           (client (mcp-tools--client server configuration))
           (runtime
             (make-instance
              'mcp-server-runtime
              :configuration server
              :registration-source ':runtime
              :provider-namespace "mcp__cleanup_stdio"
              :client client))
           (manager
             (make-instance
              'mcp-manager
              :configuration configuration
              :runtimes (list runtime)))
           (registry (make-instance 'tool-registry))
           (transport (mcp-client-transport client))
           (process-slot (find-symbol "PROCESS" "MCPAREN"))
           (process nil))
      (unwind-protect
           (progn
             (sb-posix:setenv environment-name "cleanup-secret" 1)
             (mcp-server-runtime-connect runtime)
             (mcp-tool-registry-register-manager registry manager)
             (setf process (slot-value transport process-slot))
             (test-assert
              (and process (uiop:process-alive-p process))
              "the cleanup regression starts a real standard-input MCP process")
             (sb-posix:unsetenv environment-name)
             (test-call-with-function-replacements
              (list (list 'call-with-secret-use #'deny-secret-use))
              (lambda ()
                (tool-registry-close-runtime-state registry)))
             (test-assert
              (and
               (eq (mcp-server-runtime-state runtime) :disconnected)
               (not (mcp-transport-open-p transport))
               (null (slot-value transport process-slot))
               (not
                (handler-case
                    (uiop:process-alive-p process)
                  (error ()
                    nil))))
              "missing close-time credentials cannot orphan an MCP process"))
        (sb-posix:unsetenv environment-name)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (when
            (and process
                 (handler-case
                     (uiop:process-alive-p process)
                   (error ()
                     nil)))
          (ignore-errors (uiop:terminate-process process :urgent t))
          (ignore-errors (uiop:wait-process process)))
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist :ignore)))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (environment-name
             (format nil
                     "AUTOLITH_MCP_CLEANUP_HTTP_~A"
                     (remove #\- (string-upcase (make-identifier)))))
           (server
             (mcp-server-configuration-create
              :name "cleanup-http"
              :transport
              `(:type :http
                :url "http://127.0.0.1:9/mcp"
                :headers
                (("Authorization" :environment ,environment-name)))
              :required-p t))
           (client
             (mcp-tools--client
              server
              configuration
              :exchange-scope-function
              (lambda (function)
                (call-with-secret-use function))))
           (runtime
             (make-instance
              'mcp-server-runtime
              :configuration server
              :registration-source ':runtime
              :provider-namespace "mcp__cleanup_http"
              :client client))
           (manager
             (make-instance
              'mcp-manager
              :configuration configuration
              :runtimes (list runtime)))
           (registry (make-instance 'tool-registry))
           (transport (mcp-client-transport client))
           (listener-slot (find-symbol "LISTENER-THREAD" "MCPAREN"))
           (listener-stopping-slot
             (find-symbol "LISTENER-STOPPING-P" "MCPAREN"))
           (listener nil))
      (unwind-protect
           (progn
             (sb-posix:setenv environment-name "cleanup-secret" 1)
             (mcp-tool-registry-register-manager registry manager)
             (mcp-transport-open transport)
             (setf (mcp-http-transport-session-identifier transport)
                   "cleanup-session"
                   listener
                   (make-thread
                    (lambda ()
                      (loop
                        until
                        (slot-value transport listener-stopping-slot)
                        do (sleep 0.01)))
                    :name "Autolith MCP cleanup listener")
                   (slot-value transport listener-slot)
                   listener)
             (sb-posix:unsetenv environment-name)
             (test-call-with-function-replacements
              (list (list 'call-with-secret-use #'deny-secret-use))
              (lambda ()
                (tool-registry-close-runtime-state registry)))
             (test-assert
              (and
               (not (mcp-transport-open-p transport))
               (null (slot-value transport listener-slot))
               (not (thread-alive-p listener)))
              "missing reload-time credentials cannot orphan an HTTP listener"))
        (sb-posix:unsetenv environment-name)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (when (and listener (thread-alive-p listener))
          (ignore-errors (bordeaux-threads:destroy-thread listener))
          (ignore-errors (join-thread listener)))
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist :ignore))))
  nil)


;;;; -- Credential Snapshot Rotation --

(-> test-mcp-short-credential-redaction-markers () null)
(defun test-mcp-short-credential-redaction-markers ()
  "Test scope-local markers cannot collide with short credential values."
  (dolist (credential '("MCP" "A"))
    (let* ((environment-name
             (format nil
                     "AUTOLITH_MCP_SHORT_~A"
                     (remove #\- (string-upcase (make-identifier)))))
           (server
             (mcp-server-configuration-create
              :name "short-credential"
              :transport
              `(:type :http
                :url "https://example.test/mcp"
                :headers
                (("Authorization" :environment ,environment-name)))))
           (marker nil)
           (rendered nil))
      (unwind-protect
           (progn
             (sb-posix:setenv environment-name credential 1)
             (mcp-tools--call-with-server-secret-use
              server
              (lambda ()
                (setf marker *mcp-active-credential-redaction-marker*
                      rendered
                      (mcp-tools--sanitize-string
                       (format nil "before ~A after" credential)))))
             (test-assert
              (and
               (non-empty-string-p marker)
               (not (search credential marker))
               (search marker rendered)
               (not (search credential rendered)))
              "short MCP credentials receive collision-free redaction markers"))
        (sb-posix:unsetenv environment-name))))
  nil)

(-> test-mcp-stdio-credential-snapshot-rotation () null)
(defun test-mcp-stdio-credential-snapshot-rotation ()
  "Test persistent stdio credentials rotate or disappear without residue."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (environment-name
           (format nil
                   "AUTOLITH_MCP_ROTATION_~A"
                   (remove #\- (string-upcase (make-identifier)))))
         (credential-a
           (format nil "rotation-a-~A" (make-identifier)))
         (credential-b
           (format nil "rotation-b-~A" (make-identifier)))
         (credential-c
           (format nil "rotation-c-~A" (make-identifier)))
         (server
           (mcp-server-configuration-create
            :name "credential-rotation"
            :transport
            `(:type :stdio
              :command ,(lisp-worker-sbcl-command)
              :arguments
              ("--noinform"
               "--no-sysinit"
               "--no-userinit"
               "--disable-debugger"
               "--non-interactive"
               "--eval"
               ,(test-mcp--rotating-stdio-server-form))
              :environment
              (("SERVICE_TOKEN" :environment ,environment-name)))
            :approval :allow
            :required-p t))
         (client (mcp-tools--client server configuration))
         (runtime
           (make-instance
            'mcp-server-runtime
            :configuration server
            :registration-source ':runtime
            :provider-namespace "mcp__credential_rotation"
            :client client))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list runtime)))
         (registry (make-instance 'tool-registry))
         (conversation
           (conversation-create
            configuration
            :identifier "mcp-credential-rotation"))
         (transport (mcp-client-transport client))
         (process-slot (find-symbol "PROCESS" "MCPAREN"))
         (process-a nil)
         (process-b nil))
    (unwind-protect
         (progn
           (sb-posix:setenv environment-name credential-a 1)
           (mcp-server-runtime-connect runtime)
           (mcp-tool-registry-register-manager registry manager)
           (setf process-a (slot-value transport process-slot))
           (test-assert
            (and
             (test-mcp--process-alive-p process-a)
             (non-empty-string-p
              (mcp-server-runtime-launch-environment-fingerprint runtime)))
            "the first credential snapshot launches one tracked MCP process")
           (let* ((provider-tool
                    (find-if
                     (lambda (tool)
                       (typep tool 'mcp-provider-tool))
                     (tool-registry-tools registry)))
                  (context
                    (test-mcp--context
                     configuration conversation registry))
                  (snapshot-function
                    (symbol-function
                     'mcp-tools--resolve-environment-snapshot))
                  (result nil))
             (sb-posix:setenv environment-name credential-b 1)
             (test-call-with-function-replacements
              (list
               (list
                'mcp-tools--resolve-environment-snapshot
                (lambda (server-configuration)
                  (multiple-value-prog1
                      (funcall snapshot-function server-configuration)
                    (sb-posix:setenv
                     environment-name credential-c 1)))))
              (lambda ()
                (setf
                 result
                 (tool-execute
                  provider-tool context (json-object)))))
             (setf process-b (slot-value transport process-slot))
             (conversation-append-tool-result
              conversation
              "credential-rotation"
              :tool-name "mcp"
              :output (tool-result-content result)
              :success-p (tool-result-success-p result))
             (let ((status (mcp-manager-render-status manager))
                   (conversation-text
                     (uiop:read-file-string
                      (conversation-pathname conversation))))
               (test-assert
                (and
                 (tool-result-success-p result)
                 (search
                  *mcp-credential-redaction-marker*
                  (tool-result-content result))
                 (not (search credential-a (tool-result-content result)))
                 (not (search credential-b (tool-result-content result)))
                 (not (search credential-c (tool-result-content result)))
                 (not (eq process-a process-b))
                 (not (test-mcp--process-alive-p process-a))
                 (test-mcp--process-alive-p process-b)
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-a))
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-b))
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-c))
                 (not (search credential-a status))
                 (not (search credential-b status))
                 (not (search credential-c status))
                 (not (search credential-a conversation-text))
                 (not (search credential-b conversation-text))
                 (not (search credential-c conversation-text)))
                "one exact snapshot drives rotation, launch, redaction, and persistence"))
             (sb-posix:unsetenv environment-name)
             (let ((result
                     (tool-execute
                      provider-tool context (json-object))))
               (test-assert
                (and
                 (not (tool-result-success-p result))
                 (eq (mcp-server-runtime-state runtime) :failed)
                 (not (mcp-transport-open-p transport))
                 (null (slot-value transport process-slot))
                 (not (test-mcp--process-alive-p process-b))
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-a))
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-b))
                 (not
                  (test-mcp--object-contains-string-p
                   runtime credential-c)))
                "an unset launch credential closes the old process before failure")))
           (let* ((provider (provider-create configuration))
                  (application
                    (make-instance
                     'application
                     :configuration configuration
                     :conversation conversation
                     :provider provider
                     :tool-registry registry
                     :worker nil
                     :agent
                     (agent-create
                      :configuration configuration
                      :provider provider
                      :conversation conversation
                      :tool-registry registry
                      :worker nil)
                     :ui nil)))
             (tool-registry-close-runtime-state registry)
             (checkpoint-detach-state application)
             (dolist
                 (entry
                   (list
                    (cons "the original MCP credential" credential-a)
                    (cons "the rotated MCP credential" credential-b)
                    (cons "the post-snapshot environment value" credential-c)))
               (test-assert
                (not
                 (test-mcp--object-contains-string-p
                  application (rest entry)))
                (format nil
                        "checkpoint detachment removes ~A"
                        (first entry))))
             (test-assert
              (and
               (notany
                (lambda (tool)
                  (typep tool 'mcp-provider-tool))
               (tool-registry-tools registry))
               (not
                (test-mcp--object-contains-string-p
                 runtime *mcp-credential-redaction-marker*))
               (null
                (mcp-server-runtime-launch-environment-fingerprint runtime))
               (null *mcp-environment-fingerprint-key*))
              "checkpoint detachment removes MCP tools, digests, and keys")))
      (sb-posix:unsetenv environment-name)
      (ignore-errors (tool-registry-close-runtime-state registry))
      (when (test-mcp--process-alive-p process-a)
        (ignore-errors (uiop:terminate-process process-a :urgent t))
        (ignore-errors (uiop:wait-process process-a)))
      (when (test-mcp--process-alive-p process-b)
        (ignore-errors (uiop:terminate-process process-b :urgent t))
        (ignore-errors (uiop:wait-process process-b)))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Aggregate Discovery Bounds --

(-> test-mcp--bounded-runtime
    (configuration
     &key (:name string)
          (:required-p boolean)
          (:tools-function function)
          (:initialization-function t))
    mcp-server-runtime)
(defun test-mcp--bounded-runtime
    (configuration
     &key name required-p tools-function initialization-function)
  "Return one scripted runtime for manager-wide discovery-bound tests."
  (declare (ignore configuration))
  (let* ((server
           (mcp-server-configuration-create
            :name name
            :transport '(:type :stdio :command "/bin/true")
            :required-p required-p))
         (transport
           (make-instance
            'test-mcp-transport
            :handler
            (lambda (transport request)
              (declare (ignore transport))
              (let ((method (json-get request "method")))
                (cond
                  ((string= method "initialize")
                   (test-mcp--rpc-result
                    request
                    (if initialization-function
                        (funcall initialization-function)
                        (json-object
                         "protocolVersion" "2025-11-25"
                         "capabilities"
                         (json-object "tools" (json-object))
                         "serverInfo"
                         (json-object "name" name "version" "1")))))
                  ((string= method "tools/list")
                   (test-mcp--rpc-result
                    request
                    (json-object
                     "tools"
                     (coerce (funcall tools-function) 'vector))))
                  (t
                   (error
                    "Bounded MCP fixture ~A received unexpected method ~S."
                    name
                    method))))))))
    (make-instance
     'mcp-server-runtime
     :configuration server
     :registration-source ':runtime
     :provider-namespace
     (mcp-tools--identifier-base name :prefix "mcp__")
     :client
     (make-mcp-client
      transport
      :name "autolith-test"
      :version "1"))))

(-> test-mcp-aggregate-discovery-bounds () null)
(defun test-mcp-aggregate-discovery-bounds ()
  "Test required-first aggregate allocation during startup and refresh."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (required-definition
           (test-mcp--tool-definition "required-schema"))
         (optional-definition
           (test-mcp--tool-definition "optional-schema"))
         (optional-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "optional-schema"
            :required-p nil
            :tools-function
            (lambda () (list optional-definition))))
         (required-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "required-schema"
            :required-p t
            :tools-function
            (lambda () (list required-definition))))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list optional-runtime required-runtime))))
    (unwind-protect
         (multiple-value-bind (schema schema-bytes)
             (mcp-tools--provider-schema
              required-runtime
              (json-get required-definition "inputSchema"))
           (declare (ignore schema))
           (let ((*mcp-maximum-retained-tools* 8)
                 (*mcp-maximum-retained-input-schema-bytes*
                   schema-bytes))
             (with-lock-held ((mcp-manager-lock manager))
               (mcp-manager--connect-runtimes manager)))
           (test-assert
            (and
             (eq (first (mcp-manager-runtimes manager)) optional-runtime)
             (eq (second (mcp-manager-runtimes manager)) required-runtime)
             (eq (mcp-server-runtime-state required-runtime) :ready)
             (= (length (mcp-server-runtime-tools required-runtime)) 1)
             (eq (mcp-server-runtime-state optional-runtime) :failed)
             (null (mcp-server-runtime-tools optional-runtime))
             (search
              "encoded input schema bytes"
              (mcp-server-runtime-failure optional-runtime)))
            "required servers receive aggregate schema budget before optional servers"))
      (ignore-errors (mcp-manager-close manager))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-definition
           (test-mcp--tool-definition "first-required"))
         (second-definition
           (test-mcp--tool-definition "second-required"))
         (first-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "first-required"
            :required-p t
            :tools-function
            (lambda () (list first-definition))))
         (second-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "second-required"
            :required-p t
            :tools-function
            (lambda () (list second-definition))))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list first-runtime second-runtime))))
    (unwind-protect
         (multiple-value-bind (schema schema-bytes)
             (mcp-tools--provider-schema
              first-runtime
              (json-get first-definition "inputSchema"))
           (declare (ignore schema))
           (let* ((*mcp-maximum-retained-tools* 8)
                  (*mcp-maximum-retained-input-schema-bytes*
                    schema-bytes)
                  (failure
                    (handler-case
                        (with-lock-held ((mcp-manager-lock manager))
                          (mcp-manager--connect-runtimes manager)
                          nil)
                      (mcp-aggregate-budget-exceeded (condition)
                        condition))))
             (test-assert
              (and
               failure
               (mcp-server-startup-error-required-p failure)
               (eq
                (mcp-aggregate-budget-exceeded-resource failure)
                :input-schema-bytes)
               (= (mcp-aggregate-budget-exceeded-allocated failure)
                  schema-bytes)
               (= (mcp-aggregate-budget-exceeded-requested failure)
                  schema-bytes)
               (= (mcp-aggregate-budget-exceeded-limit failure)
                  schema-bytes)
               (eq (mcp-server-runtime-state second-runtime) :failed)
               (null (mcp-server-runtime-tools second-runtime)))
              "a required aggregate overflow aborts with structured budget data")))
      (ignore-errors (mcp-manager-close manager))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (optional-definitions
           (list (test-mcp--tool-definition "optional-existing")))
         (required-definitions
           (list (test-mcp--tool-definition "required-before")))
         (optional-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "optional-refresh"
            :required-p nil
            :tools-function (lambda () optional-definitions)))
         (required-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "required-refresh"
            :required-p t
            :tools-function (lambda () required-definitions)))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list optional-runtime required-runtime))))
    (unwind-protect
         (let ((*mcp-maximum-retained-tools* 2)
               (*mcp-maximum-retained-input-schema-bytes*
                 (* 8 1024 1024)))
           (with-lock-held ((mcp-manager-lock manager))
             (mcp-manager--connect-runtimes manager))
           (setf
            required-definitions
            (list
             (test-mcp--tool-definition "required-after")
             (test-mcp--tool-definition
              "required-task"
              :task-required-p t)))
           (mcp-server-runtime-request-tool-refresh required-runtime)
           (with-lock-held ((mcp-manager-lock manager))
             (mcp-manager--connect-runtimes manager))
           (let ((required-record
                   (find
                    "required-refresh"
                    (mcp-manager-status-records manager)
                    :test #'string=
                    :key (lambda (record) (getf record :name)))))
             (test-assert
              (and
               (eq (mcp-server-runtime-state required-runtime) :ready)
               (= (length (mcp-server-runtime-tools required-runtime)) 2)
               (= (getf required-record :tool-count) 1)
               (= (getf required-record :task-required-tool-count) 1)
               (eq (mcp-server-runtime-state optional-runtime) :failed)
               (null (mcp-server-runtime-tools optional-runtime))
               (search
                "retained tool count"
                (mcp-server-runtime-failure optional-runtime)))
              "a required list change evicts optional tools without exposing task-required tools")))
      (ignore-errors (mcp-manager-close manager))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (fail-refresh-p nil)
         (discovery-count 0)
         (definition (test-mcp--tool-definition "required-retry"))
         (runtime
           (test-mcp--bounded-runtime
            configuration
            :name "required-retry"
            :required-p t
            :tools-function
            (lambda ()
              (incf discovery-count)
              (when fail-refresh-p
                (error "Injected required tool refresh failure."))
              (list definition))))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list runtime)))
         (registry (make-instance 'tool-registry)))
    (unwind-protect
         (progn
           (with-lock-held ((mcp-manager-lock manager))
             (mcp-manager--connect-runtimes manager))
           (mcp-tool-registry-register-manager registry manager)
           (setf fail-refresh-p t)
           (mcp-server-runtime-request-tool-refresh runtime)
           (labels ((refresh-failure ()
                      "Return one required failure from a provider-boundary refresh."
                      (handler-case
                          (progn
                            (mcp-tool-registry-refresh
                             registry :only-dirty-p t)
                            nil)
                        (mcp-server-startup-error (condition)
                          condition))))
             (let ((first-failure (refresh-failure))
                   (second-failure (refresh-failure)))
               (test-assert
                (and
                 first-failure
                 second-failure
                 (mcp-server-startup-error-required-p first-failure)
                 (mcp-server-startup-error-required-p second-failure)
                 (= discovery-count 3)
                 (eq (mcp-server-runtime-state runtime) :failed)
                 (=
                  (mcp-server-runtime-tools-discovered-version runtime)
                  (mcp-server-runtime-tools-change-version runtime)))
                "a failed required refresh retries and remains a provider-boundary barrier")))
           (setf fail-refresh-p nil)
           (mcp-tool-registry-refresh registry :only-dirty-p t)
           (test-assert
            (and
             (= discovery-count 4)
             (eq (mcp-server-runtime-state runtime) :ready)
             (test-mcp--tool-with-raw-name registry "required-retry"))
            "a required server can recover after a failed refresh")
           (tool-registry-close-runtime-state registry)
           (setf fail-refresh-p t)
           (let ((resume-failure
                   (handler-case
                       (progn
                         (tool-registry-resume-runtime-state registry)
                         nil)
                     (mcp-server-startup-error (condition)
                       condition))))
             (test-assert
              (and
               resume-failure
               (mcp-server-startup-error-required-p resume-failure)
               (eq (mcp-server-runtime-state runtime) :failed))
              "checkpoint resume cannot bypass a failed required MCP server")))
      (ignore-errors
        (tool-registry-close-runtime-state registry))
      (ignore-errors (mcp-manager-close manager))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-mcp-initialization-metadata-bounds () null)
(defun test-mcp-initialization-metadata-bounds ()
  "Test multi-server initialization metadata is projected before retention."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (marker "MCP-INITIALIZATION-METADATA-SHOULD-DISAPPEAR")
         (payload
           (concatenate
            'string
            marker
            (make-string 10000 :initial-element #\X)))
         (instructions
           (concatenate
            'string
            (make-string 3000 :initial-element #\I)
            payload))
         (initialization-function
           (lambda ()
             (json-object
              "protocolVersion" "2025-11-25"
              "capabilities"
              (json-object
               "tools" (json-object "payload" payload)
               "resources" (json-object "payload" payload)
               "prompts" (json-object "payload" payload)
               "unused" (json-object "payload" payload))
              "serverInfo"
              (json-object
               "name" "oversized-initialization"
               "version" "1"
               "payload" payload)
              "instructions" instructions)))
         (first-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "initialization-one"
            :required-p t
            :tools-function (lambda () nil)
            :initialization-function initialization-function))
         (second-runtime
           (test-mcp--bounded-runtime
            configuration
            :name "initialization-two"
            :required-p t
            :tools-function (lambda () nil)
            :initialization-function initialization-function))
         (manager
           (make-instance
            'mcp-manager
            :configuration configuration
            :runtimes (list first-runtime second-runtime)))
         (registry (make-instance 'tool-registry))
         (conversation
           (conversation-create
            configuration
            :identifier "mcp-initialization-bounds")))
    (unwind-protect
         (progn
           (with-lock-held ((mcp-manager-lock manager))
             (mcp-manager--connect-runtimes manager))
           (dolist (runtime (mcp-manager-runtimes manager))
             (let* ((client (mcp-server-runtime-client runtime))
                    (capabilities
                      (mcp-client-server-capabilities client))
                    (retained-instructions
                      (mcp-client-instructions client)))
               (test-assert
                (and
                 (hash-table-p capabilities)
                 (= (hash-table-count capabilities) 3)
                 (every
                  (lambda (name)
                    (let ((capability (json-get capabilities name)))
                      (and
                       (hash-table-p capability)
                       (zerop (hash-table-count capability)))))
                  '("tools" "resources" "prompts"))
                 (null (mcp-client-server-info client))
                 (stringp retained-instructions)
                 (=
                  (length retained-instructions)
                  *mcp-maximum-server-instruction-characters*)
                 (not
                  (test-mcp--object-contains-string-p runtime marker)))
                "one MCP runtime retains only bounded initialization projections")))
           (mcp-tool-registry-register-manager registry manager)
           (let ((contributions
                   (mcp-tool-registry-context-contributions registry)))
             (test-assert
              (and
               (= (length contributions) 2)
               (every
                (lambda (contribution)
                  (=
                   (length (context-contribution-evidence contribution))
                   *mcp-maximum-server-instruction-characters*))
                contributions))
              "multi-server instructions share the exact model-visible bound"))
           (let ((application
                   (make-instance
                    'application
                    :configuration configuration
                    :conversation conversation
                    :provider nil
                    :tool-registry registry
                    :worker nil
                    :agent nil
                    :ui nil)))
             (tool-registry-close-runtime-state registry)
             (checkpoint-detach-state application)
             (test-assert
              (and
               (not
                (test-mcp--object-contains-string-p application marker))
               (not
                (test-mcp--object-contains-string-p application payload)))
              "detached checkpoint graphs retain no initialization payload")))
      (ignore-errors
        (tool-registry-close-runtime-state registry))
      (ignore-errors (mcp-manager-close manager))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Provider Compatibility --

(-> test-mcp-provider-schema-json-fidelity () null)
(defun test-mcp-provider-schema-json-fidelity ()
  "Test MCP schemas retain distinct JSON false and null values."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (multiple-value-bind (manager transport)
             (test-mcp--manager configuration)
           (declare (ignore transport))
           (unwind-protect
                (let* ((runtime
                         (first (mcp-manager-runtimes manager)))
                       (property-key (copy-seq "namespace"))
                       (string-type (copy-seq "string"))
                       (namespace-schema
                         (json-object
                          "anyOf"
                          (vector
                           (json-object "type" string-type)
                           (json-object "type" "null"))
                          "default"
                          (mcparen:json-null-value)))
                       (source-properties (json-object))
                       (source
                         (progn
                           (setf (gethash property-key source-properties)
                                 namespace-schema)
                           (json-object
                            "type" "object"
                            "additionalProperties" yason:false
                            "properties" source-properties))))
                  (multiple-value-bind (projected bytes)
                      (mcp-tools--provider-schema runtime source)
                    (let* ((properties
                             (json-get projected "properties"))
                           (namespace
                             (json-get properties "namespace"))
                           (any-of
                             (json-get namespace "anyOf"))
                           (encoded
                             (json-encode projected)))
                      (test-assert
                       (and (> bytes 0)
                            (not (eq projected source))
                            (not (eq namespace namespace-schema)))
                       "provider projection returns a detached schema")
                      (test-assert
                       (eq
                        (json-get projected "additionalProperties")
                        yason:false)
                       "provider projection preserves JSON false")
                      (test-assert
                       (and
                        (vectorp any-of)
                        (= (length any-of) 2)
                        (eq
                         (json-get namespace "default")
                         (mcparen:json-null-value))
                        (string= (json-get (aref any-of 1) "type") "null"))
                       "provider projection preserves nullable schema values")
                      (test-assert
                       (and (search "\"additionalProperties\":false" encoded)
                            (search "\"default\":null" encoded))
                       "provider JSON distinguishes false from null")
                      (setf (char property-key 0) #\N
                            (char string-type 0) #\S)
                      (let ((detached-namespace
                              (json-get properties "namespace")))
                        (test-assert
                         (and (hash-table-p detached-namespace)
                              (string=
                               (json-get
                                (aref
                                 (json-get detached-namespace "anyOf")
                                 0)
                                "type")
                               "string"))
                         "provider projection detaches mutable JSON strings")))))
             (mcp-manager-close manager)))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Integration Tests --

(-> test-mcp-tools () null)
(defun test-mcp-tools ()
  "Test MCP tool projection, authorization, resources, and lifecycle behavior."
  (test-mcp-provider-schema-json-fidelity)
  (test-mcp-credential-stdio-ingress-projection)
  (test-mcp-environment-fingerprint-lifecycle)
  (test-mcp-cleanup-without-credential-availability)
  (test-mcp-short-credential-redaction-markers)
  (test-mcp-stdio-credential-snapshot-rotation)
  (test-mcp-http-credential-exchange-scope)
  (test-mcp-retained-tool-metadata-boundaries)
  (test-mcp-credential-echo-containment)
  (test-mcp-aggregate-discovery-bounds)
  (test-mcp-initialization-metadata-bounds)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (multiple-value-bind (manager transport)
             (test-mcp--manager configuration)
           (let ((forward
                   (mcp-tools--identifier-map
                    '("Alpha Server" "Alpha/Server")
                    :prefix "mcp__"))
                 (reverse
                   (mcp-tools--identifier-map
                    '("Alpha/Server" "Alpha Server")
                    :prefix "mcp__")))
             (test-assert
              (and
               (not
                (string=
                 (gethash "Alpha Server" forward)
                 (gethash "Alpha/Server" forward)))
               (string=
                (gethash "Alpha Server" forward)
                (gethash "Alpha Server" reverse))
               (string=
                (gethash "Alpha/Server" forward)
                (gethash "Alpha/Server" reverse)))
              "colliding MCP server namespaces are stable and order-independent"))
           (let ((before
                   (mcp-tools--identifier-map '("read/file")))
                 (after
                   (mcp-tools--identifier-map
                    '("read/file" "read file"))))
             (test-assert
              (string=
               (gethash "read/file" before)
               (gethash "read/file" after))
              "adding a colliding MCP name never renames an existing tool"))
           (let* ((registry (make-instance 'tool-registry))
                  (conversation
                    (conversation-create
                     configuration
                     :identifier "mcp-tools"))
                  (context
                    (test-mcp--context
                     configuration conversation registry)))
             (mcp-tool-registry-register-manager registry manager)
             (let* ((environment-name "HOME")
                    (server
                      (mcp-server-configuration-create
                       :name "late-environment"
                       :transport
                       `(:type :http
                         :url "https://example.test/mcp"
                         :headers
                         (("Authorization" :environment
                           ,environment-name)))))
                    (transport-configuration
                      (mcp-server-configuration-transport server))
                    (headers
                      (mcp-tools--call-with-server-secret-use
                       server
                       (lambda ()
                         (funcall
                          (mcp-tools--http-headers-function
                           server transport-configuration))))))
               (test-assert
                (string=
                 (rest (first headers))
                 (uiop:getenv environment-name))
                "HTTP header values are resolved from the environment on demand"))
             (let* ((missing-name
                      (loop for index from 0
                            for name =
                              (format nil
                                      "AUTOLITH_MCP_TEST_MISSING_~D"
                                      index)
                            unless (uiop:getenv name)
                              return name))
                    (server
                      (mcp-server-configuration-create
                       :name "missing-environment"
                       :transport
                       `(:type :http
                         :url "https://example.test/mcp"
                         :headers
                         (("Authorization" :environment
                           ,missing-name)))))
                    (binding
                      (first
                       (mcp-http-configuration-header-bindings
                        (mcp-server-configuration-transport server)))))
               (test-assert
                (handler-case
                    (progn
                      (mcp-tools--call-with-server-secret-use
                       server
                       (lambda ()
                         (mcp-tools--environment-value server binding))
                       :allow-incomplete-p t)
                      nil)
                  (mcp-environment-unavailable (condition)
                    (string=
                     (mcp-environment-unavailable-variable condition)
                     missing-name)))
                "an unavailable environment-backed secret fails explicitly"))
             (let* ((sentinel-name
                      (format nil
                              "AUTOLITH_MCP_PARENT_SECRET_~A"
                              (remove
                               #\-
                               (string-upcase (make-identifier)))))
                    (sentinel-entry
                      (format nil "~A=must-not-leak" sentinel-name))
                    (server
                      (mcp-server-configuration-create
                       :name "environment-allowlist"
                       :transport
                       '(:type :stdio :command "/bin/true")))
                    (transport
                      (mcp-server-configuration-transport server)))
               (unwind-protect
                    (progn
                      (sb-posix:setenv sentinel-name "must-not-leak" 1)
                      (let ((environment
                              (mcp-tools--call-with-server-secret-use
                               server
                               (lambda ()
                                 (funcall
                                  (mcp-tools--stdio-environment-function
                                   server transport))))))
                        (test-assert
                         (and
                          (member "AUTOLITH_MCP=1"
                                  environment
                                  :test #'string=)
                          (not
                           (member sentinel-entry
                                   environment
                                   :test #'string=)))
                         "stdio MCP servers inherit only the safe environment allowlist")))
                      (let* ((mapped-server
                               (mcp-server-configuration-create
                                :name "explicit-environment"
                                :transport
                                `(:type :stdio
                                  :command "/bin/true"
                                  :environment
                                  (("SERVICE_TOKEN"
                                    :environment
                                    ,sentinel-name)))))
                             (mapped-transport
                               (mcp-server-configuration-transport
                                mapped-server))
                             (environment
                               (mcp-tools--call-with-server-secret-use
                                mapped-server
                                (lambda ()
                                  (funcall
                                   (mcp-tools--stdio-environment-function
                                    mapped-server mapped-transport))))))
                      (test-assert
                       (and
                        (member "SERVICE_TOKEN=must-not-leak"
                                environment
                                  :test #'string=)
                        (not
                         (member sentinel-entry
                                 environment
                                 :test #'string=)))
                       "stdio MCP servers receive only explicitly mapped secret values"))
                    (let* ((snapshot-server
                             (mcp-server-configuration-create
                              :name "shared-environment-source"
                              :transport
                              `(:type :stdio
                                :command "/bin/true"
                                :environment
                                (("FIRST_TOKEN"
                                  :environment
                                  ,sentinel-name)
                                 ("SECOND_TOKEN"
                                  :environment
                                  ,sentinel-name)))))
                           (getenv-function (symbol-function 'uiop:getenv))
                           (source-read-count 0)
                           (snapshot nil))
                      (test-call-with-function-replacements
                       (list
                        (list
                         'uiop:getenv
                         (lambda (name)
                           (if (string= name sentinel-name)
                               (progn
                                 (incf source-read-count)
                                 (if (= source-read-count 1)
                                     "snapshot-one"
                                     "snapshot-two"))
                               (funcall getenv-function name)))))
                       (lambda ()
                         (setf
                          snapshot
                          (mcp-tools--resolve-environment-snapshot
                           snapshot-server))))
                      (test-assert
                       (and
                        (= source-read-count 1)
                        (= (length snapshot) 2)
                        (every
                         (lambda (entry)
                           (string= (rest entry) "snapshot-one"))
                         snapshot))
                       "one environment source is read once per exact snapshot"))
                 (sb-posix:unsetenv sentinel-name)))
             (let* ((read-tool
                      (test-mcp--tool-with-raw-name
                       registry "read file"))
                    (collision-tool
                      (test-mcp--tool-with-raw-name
                       registry "read/file"))
                    (mutating-tool
                      (test-mcp--tool-with-raw-name
                       registry "mutate"))
                    (task-required-tool
                      (test-mcp--tool-with-raw-name
                       registry "background-only")))
               (test-assert
                (and
                 read-tool
                 collision-tool
                 (null task-required-tool)
                 (not
                  (string=
                   (tool-name read-tool)
                   (tool-name collision-tool))))
                "provider tools use stable names and omit task-required tools")
               (let ((name-map
                       (mcp-tools--identifier-map
                        '("read/file" "read file"))))
                 (test-assert
                  (and
                   (string=
                    (gethash "read file" name-map)
                    (tool-name read-tool))
                   (string=
                    (gethash "read/file" name-map)
                    (tool-name collision-tool)))
                  "provider-name collision resolution is order-independent"))
               (let* ((fields
                        (tool-authorization-identity-fields collision-tool))
                      (title
                        (application--tool-authorization-title collision-tool)))
                 (test-assert
                  (and
                   (equal fields
                          '(("MCP server" "Test Server")
                            ("MCP tool" "read/file")))
                   (search "\"Test Server\"" title)
                   (search "\"read/file\"" title)
                   (not
                    (search
                     (tool-canonical-name collision-tool)
                     title)))
                  "MCP approval identifies the server and unmangled raw tool"))
               (let* ((tail "CONSEQUENTIAL-ARGUMENT-TAIL")
                      (arguments
                        (json-object
                         "change"
                         (concatenate
                          'string
                          (make-string 512 :initial-element #\x)
                          tail)))
                      (encoded (json-encode arguments))
                      (entry
                        (application--tool-authorization-request-entry
                         mutating-tool arguments)))
                 (test-assert
                  (and
                   (search encoded entry)
                   (search tail entry)
                   (> (length entry) 512))
                  "MCP approval displays consequential arguments without truncation"))
               (test-assert
                (and
                 (tool-child-safe-p read-tool)
                 (not (tool-child-safe-p collision-tool))
                 (not (tool-child-safe-p mutating-tool)))
                "only exact raw child grants cross the child boundary")
               (test-assert
                (and
                 (not (tool-compact-result-visible-p read-tool))
                 (tool-compact-result-visible-p collision-tool)
                 (tool-compact-result-visible-p mutating-tool)
                 (= (tool-runtime-close-priority read-tool) 50))
                "only user-trusted read-only annotations affect compact visibility")
               (let ((decoded
                       (tool-decode-arguments
                        read-tool
                        "{\"flag\":false,\"empty\":null,\"items\":[]}")))
                 (test-assert
                  (and
                   (eq (json-get decoded "flag") yason:false)
                   (eq (json-get decoded "empty") :null)
                   (vectorp (json-get decoded "items"))
                   (zerop (length (json-get decoded "items"))))
                  "MCP argument decoding preserves exact JSON wire values"))
               (test-assert
                (handler-case
                    (progn
                      (tool-decode-arguments
                       read-tool
                       "{\"value\":1} trailing")
                      nil)
                  (tool-error ()
                    t))
                "MCP argument decoding rejects trailing documents")
               (let* ((result
                        (tool-execute
                         read-tool context (json-object)))
                      (text (tool-result-content result)))
                 (test-assert
                  (and
                   (tool-result-success-p result)
                   (< (search "first" text)
                      (search "Resource test://embedded" text))
                   (< (search "second" text)
                      (search "Structured content" text))
                   (search "\"answer\":42" text))
                  "ordered text, embedded resources, and structured output survive"))
               (test-assert
                (not
                 (tool-result-success-p
                  (tool-execute
                   mutating-tool context (json-object))))
                "unannotated MCP calls fail closed without approval")
               (test-assert
                (not
                 (tool-result-success-p
                  (tool-execute
                   collision-tool context (json-object))))
                "untrusted read-only annotations still require approval")
               (let ((context
                       (make-instance
                        'tool-context
                        :configuration configuration
                        :worker nil
                        :conversation conversation
                        :registry registry
                        :tool-authorization-function
                        (lambda (tool arguments)
                          (declare (ignore tool arguments))
                          :allow))))
                 (test-assert
                  (tool-result-success-p
                   (tool-execute
                    mutating-tool context (json-object)))
                  "the explicit MCP authorization callback can approve a call")
                 (let ((error-result
                         (tool-execute
                          (test-mcp--tool-with-raw-name registry "error")
                          context
                          (json-object))))
                   (test-assert
                    (and
                     (not (tool-result-success-p error-result))
                     (search
                      "server-declared failure"
                      (tool-result-content error-result)))
                    "MCP isError becomes an Autolith tool failure"))
                 (let* ((image-result
                          (tool-execute
                           (test-mcp--tool-with-raw-name registry "image")
                           context
                           (json-object)))
                        (attachments
                          (tool-result-image-attachments image-result))
                        (blocks
                          (tool-result-content-blocks image-result)))
                   (test-assert
                    (and
                     (tool-result-success-p image-result)
                     (= (length attachments) 1)
                     (search
                      "before image"
                      (tool-result-content image-result))
                     (search
                      "after image"
                      (tool-result-content image-result))
                     (probe-file
                      (image-attachment-pathname
                       (first attachments)))
                     (= (length blocks) 3)
                     (string= (first blocks) "before image")
                     (typep (second blocks) 'image-attachment)
                     (string= (third blocks) "after image"))
                    "MCP image blocks retain exact provider-visible ordering"))))
             (let ((resources
                     (tool-registry-find registry "mcp" "resources"))
                   (templates
                     (tool-registry-find
                      registry "mcp" "resource-templates"))
                   (read-resource
                     (tool-registry-find
                      registry "mcp" "read-resource"))
                   (prompts (tool-registry-find registry "mcp" "prompts"))
                   (get-prompt
                     (tool-registry-find registry "mcp" "get-prompt")))
               (test-assert
                (search
                 "test://resource"
                 (tool-result-content
                  (tool-execute
                   resources context
                   (json-object "server" "Test Server"))))
                "the MCP resource helper lists complete resource metadata")
               (test-assert
                (search
                 "test://item/{identifier}"
                 (tool-result-content
                  (tool-execute
                   templates context
                   (json-object "server" "Test Server"))))
                "the MCP template helper lists complete template metadata")
               (test-assert
                (search
                 "resource body"
                 (tool-result-content
                  (tool-execute
                   read-resource context
                   (json-object
                    "server" "Test Server"
                    "uri" "test://resource"))))
                "the MCP resource reader preserves textual resource content")
               (test-assert
                (search
                 "summarize"
                 (tool-result-content
                  (tool-execute
                   prompts context
                   (json-object "server" "Test Server"))))
                "the MCP prompt helper lists complete prompt metadata")
               (let* ((result
                        (tool-execute
                         get-prompt context
                         (json-object
                          "server" "Test Server"
                          "name" "summarize")))
                      (blocks (tool-result-content-blocks result)))
                 (test-assert
                  (and
                   (search "Summarize this."
                           (tool-result-content result))
                   (= (length
                       (tool-result-image-attachments result))
                      1)
                   (some
                    (lambda (block)
                      (typep block 'image-attachment))
                    blocks))
                  "the MCP prompt resolver preserves text and image content")))
             (let ((contributions
                     (mcp-tool-registry-context-contributions registry)))
               (test-assert
                (and (= (length contributions) 1)
                     (search
                      "deterministic test fixture"
                      (context-contribution-evidence
                       (first contributions))))
                "server instructions remain bounded untrusted request context"))
             (let* ((record (first (mcp-manager-status-records manager)))
                    (rendered (mcp-manager-render-status manager)))
               (test-assert
                (and
                 (search "Test Server" rendered)
                 (search "RUNTIME" rendered)
                 (search "READY" rendered)
                 (search "1 task-required tool unavailable" rendered)
                 (eq (getf record :source) :runtime)
                 (= (getf record :tool-count) 5)
                 (= (getf record :task-required-tool-count) 1)
                 (search
                  "task execution"
                  (getf record :task-required-tool-reason))
                 (eq (mcp-tool-registry-manager registry) manager))
                "MCP status reports task-required tools without exposing them"))
             (let* ((runtime (first (mcp-manager-runtimes manager)))
                    (all-tools-registry (make-instance 'tool-registry))
                    (child-safe-registry (make-instance 'tool-registry)))
               (mcp-tool-registry-bind-manager
                all-tools-registry manager (constantly t))
               (mcp-tool-registry-bind-manager
                child-safe-registry manager #'tool-child-safe-p)
               (mcp-tool-registry-refresh
                all-tools-registry :only-dirty-p t)
               (mcp-tool-registry-refresh
                child-safe-registry :only-dirty-p t)
               (test-assert
                (and
                 (test-mcp--tool-with-raw-name
                  all-tools-registry "read file")
                 (test-mcp--tool-with-raw-name
                  child-safe-registry "read file")
                 (null
                  (test-mcp--tool-with-raw-name
                   all-tools-registry "background-only"))
                 (null
                  (test-mcp--tool-with-raw-name
                   child-safe-registry "background-only"))
                 (null
                  (test-mcp--tool-with-raw-name
                   child-safe-registry "mutate")))
                "registry predicates cannot expose task-required MCP tools")
               (with-lock-held ((mcp-server-runtime-lock runtime))
                 (setf
                  (mcp-server-runtime-tools runtime)
                  (remove
                   "read file"
                   (mcp-server-runtime-tools runtime)
                   :test #'string=
                   :key #'mcp-tool-name))
                 (incf (mcp-server-runtime-tools-revision runtime)))
               (mcp-tool-registry-refresh registry :only-dirty-p t)
               (mcp-tool-registry-refresh
                all-tools-registry :only-dirty-p t)
               (mcp-tool-registry-refresh
                child-safe-registry :only-dirty-p t)
               (test-assert
                (and
                 (null
                  (test-mcp--tool-with-raw-name registry "read file"))
                 (null
                  (test-mcp--tool-with-raw-name
                   all-tools-registry "read file"))
                 (null
                  (test-mcp--tool-with-raw-name
                   child-safe-registry "read file")))
                "every registry reconciles one shared runtime revision")
               (mcp-server-runtime-request-tool-refresh runtime)
               (mcp-tool-registry-refresh
                all-tools-registry :only-dirty-p t)
               (mcp-tool-registry-refresh registry :only-dirty-p t)
               (mcp-tool-registry-refresh
                child-safe-registry :only-dirty-p t)
               (test-assert
                (and
                 (test-mcp--tool-with-raw-name registry "read file")
                 (test-mcp--tool-with-raw-name
                  all-tools-registry "read file")
                 (test-mcp--tool-with-raw-name
                  child-safe-registry "read file"))
                "one registry discovers a change and every registry reconciles it"))
             (tool-registry-close-runtime-state registry)
             (test-assert
              (and
               (= (test-mcp-transport-close-count transport) 1)
               (test-mcp-transport-close-secret-use-p transport)
               (not (secret-use-active-p)))
              "a shared MCP manager closes once inside transient secret use")
             (mcp-server-runtime-connect
              (first (mcp-manager-runtimes manager)))
             (test-assert
              (and
               (eq
                (mcp-server-runtime-state
                 (first (mcp-manager-runtimes manager)))
                :ready)
               (mcp-client-connected-p
                (mcp-server-runtime-client
                 (first (mcp-manager-runtimes manager)))))
              "a closed MCP runtime reconnects and rediscovers its tools")
             (tool-registry-detach-runtime-state registry)
             (test-assert
              (= (test-mcp-transport-detach-count transport) 1)
              "a shared MCP manager detaches exactly once through its registry")))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  (test-mcp--generation-rediscovery)
  (test-mcp--generation-rediscovery-bound)
  (let* ((configuration (test-configuration))
         (server
           (mcp-server-configuration-create
            :name "resources-only"
            :transport '(:type :stdio :command "/bin/true")))
         (transport
           (make-instance
            'test-mcp-transport
            :handler
            (lambda (transport request)
              (declare (ignore transport))
              (if (string= (json-get request "method") "initialize")
                  (test-mcp--rpc-result
                   request
                   (json-object
                    "protocolVersion" "2025-11-25"
                    "capabilities"
                    (json-object "resources" (json-object))
                    "serverInfo"
                    (json-object
                     "name" "resources-only"
                     "version" "1")))
                  (error "Resources-only fixture received unexpected method ~S."
                         (json-get request "method"))))))
         (runtime
           (make-instance
            'mcp-server-runtime
            :configuration server
            :registration-source :runtime
            :provider-namespace "mcp__resources_only"
            :client
            (make-mcp-client transport :name "autolith-test"))))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect runtime)
           (test-assert
            (and (eq (mcp-server-runtime-state runtime) :ready)
                 (null (mcp-server-runtime-tools runtime))
                 (not
                  (find "tools/list"
                        (test-mcp-transport-requests transport)
                        :test #'string=
                        :key
                        (lambda (request)
                          (json-get request "method")))))
            "a resources-only MCP server connects without tools/list"))
      (ignore-errors (mcp-server-runtime-close runtime))
      (uiop:delete-directory-tree
       (test-configuration-root configuration)
       :validate t
       :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (missing-directory
           (namestring
            (merge-pathnames "missing-mcp-directory/" root)))
         (registry-snapshot (mcp--registry-snapshot)))
    (unwind-protect
         (progn
           (mcp--registry-restore nil)
           (register-mcp-server
            `(:name "optional-missing"
              :transport
              (:type :stdio
               :command "/bin/true"
               :directory ,missing-directory)
              :required-p nil)
            :source :runtime)
           (let ((manager (mcp-manager-create configuration)))
             (unwind-protect
                  (let ((record
                          (first
                           (mcp-manager-status-records manager)))
                        (rendered
                          (string-downcase
                           (mcp-manager-render-status manager))))
                    (test-assert
                     (and
                      (eq (getf record :state) :failed)
                      (non-empty-string-p (getf record :failure))
                      (search "optional-missing" rendered)
                      (search "failed" rendered))
                     "an optional missing MCP directory remains observably failed"))
               (mcp-manager-close manager)))
           (mcp--registry-restore nil)
           (register-mcp-server
            `(:name "required-missing"
              :transport
              (:type :stdio
               :command "/bin/true"
               :directory ,missing-directory)
              :required-p t)
            :source :runtime)
           (test-assert
            (handler-case
                (progn
                  (mcp-manager-create configuration)
                  nil)
              (mcp-server-startup-error (condition)
                (and
                 (mcp-server-startup-error-required-p condition)
                 (string=
                  (mcp-server-startup-error-server-name condition)
                  "required-missing"))))
            "a required missing MCP directory prevents startup"))
      (mcp--registry-restore registry-snapshot)
      (uiop:delete-directory-tree
       root
       :validate t
       :if-does-not-exist :ignore)))
  nil)
