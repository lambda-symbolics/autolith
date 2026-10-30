;;;; -- Private ACP MCP Transports --

(in-package #:autolith-supervisor)

(defclass parent-acp-mcp-server ()
  ((identifier :initarg :identifier :reader parent-acp-mcp-identifier
               :documentation "An opaque parent-assigned handle, not a URL or credential.")
   (name :initarg :name :reader parent-acp-mcp-name
         :documentation "The editor-selected public server name.")
   (client :initarg :client :reader parent-acp-mcp-client
           :documentation "The private MCP client and transport.")
   (tools :initform (make-hash-table :test #'equal) :reader parent-acp-mcp-tools
          :documentation "The discovered tool metadata that authorizes call lookup.")
   (secrets :initarg :secrets :accessor parent-acp-mcp-secrets
            :documentation "Literal transport credentials removed from all returned data.")
   (forget-bindings :initarg :forget-bindings :initform (constantly nil)
                    :reader        parent-acp-mcp-forget-bindings
                    :documentation "Release the bindings captured by the private transport.")
   (lock :initform (bordeaux-threads:make-lock "parent ACP MCP server")
         :reader parent-acp-mcp-lock :documentation "Serializes discovery, execution, and cleanup."))
  (:documentation "An ACP client's MCP declaration retained entirely in the trusted parent."))

(serapeum/types:-> parent-acp-mcp--replace (string string string) string)
(defun parent-acp-mcp--replace (text secret replacement)
  "Replace every literal SECRET without treating it as a pattern."
  (if (zerop (length secret))
      text
      (with-output-to-string (stream)
        (loop with start = 0
              for position = (search secret text :start2 start)
              do (if position
                     (progn
                       (write-string text stream :start start :end position)
                       (write-string replacement stream)
                       (setf start (+ position (length secret))))
                     (progn (write-string text stream :start start) (return)))))))

(serapeum/types:-> parent-acp-mcp--public (parent-acp-mcp-server t) t)
(defun parent-acp-mcp--public (server value)
  "Copy bounded JSON and remove literal credentials from server-controlled metadata and results."
  (let ((nodes 0))
    (labels ((copy-value (item depth)
               (when (or (> (incf nodes) 4096) (> depth 16))
                 (error 'parent-application-operation-error :code ':resource-limit))
               (typecase item
                 (string
                  (when (> (length item) 262144)
                    (error 'parent-application-operation-error :code ':resource-limit))
                  (reduce (lambda (text secret) (parent-acp-mcp--replace text secret "[credential]"))
                          (parent-acp-mcp-secrets server) :initial-value (copy-seq item)))
                 (hash-table
                  (let ((copy (make-hash-table :test #'equal)))
                    (maphash (lambda (key value)
                               (unless (stringp key)
                                 (error 'parent-application-operation-error :code ':invalid-request))
                               (setf (gethash (copy-value key (1+ depth)) copy)
                                     (copy-value value (1+ depth)))) item)
                    copy))
                 ((or vector cons)
                  (map 'vector (lambda (value) (copy-value value (1+ depth))) item))
                 ((or null integer float)
                  item)
                 (symbol
                  (if (or (eq item t) (member item '(:null :false)))
                      item
                      (error 'parent-application-operation-error :code ':invalid-request)))
                 (t
                  (error 'parent-application-operation-error :code ':invalid-request)))))
      (copy-value value 0))))

(serapeum/types:-> parent-acp-mcp--pairs (t) list)
(defun parent-acp-mcp--pairs (values)
  "Validate literal editor header or environment values without retaining them in public data."
  (let ((entries (agentcomms:json-sequence->list values)))
    (when (> (length entries) 128)
      (agentcomms:acp-invalid-params "Too many MCP credential bindings."))
    (loop for entry in entries
          for name = (agentcomms:acp-field entry "name" :type ':string :required-p t)
          for value = (agentcomms:acp-field entry "value" :type ':string :required-p t)
          do (unless (and (<= 1 (length name) 256) (<= (length value) 8192)
                          (not (find #\Return name)) (not (find #\Newline name))
                          (not (find #\Return value)) (not (find #\Newline value)))
               (agentcomms:acp-invalid-params "Invalid MCP credential binding."))
          collect (cons (copy-seq name) (copy-seq value)))))

(serapeum/types:-> parent-acp-mcp--server (parent-acp-session hash-table) parent-acp-mcp-server)
(defun parent-acp-mcp--server (session declaration)
  "Create a private HTTP or stdio transport from one validated editor declaration."
  (agentcomms:acp-validate-mcp-servers (list declaration))
  (let* ((name    (agentcomms:acp-field declaration "name" :type ':string :required-p t))
         (type    (agentcomms:json-get declaration "type" "stdio"))
         (pairs   (parent-acp-mcp--pairs
                   (agentcomms:json-get declaration (if (equal type "http") "headers" "env"))))
         (transport
           (cond
             ((equal type "http")
              (mcparen:make-mcp-streamable-http-transport
               (agentcomms:acp-field declaration "url" :type ':string :required-p t)
               :headers-function (lambda () (copy-tree pairs))))
             ((equal type "stdio")
              (mcparen:make-mcp-stdio-transport
               (agentcomms:acp-field declaration "command" :type ':string :required-p t)
               :arguments            (agentcomms:json-sequence->list (gethash "args" declaration))
                :directory           (parent-configuration-get
                                      (parent-application-configuration (parent-acp-session-application session)) ':workspace)
               :environment-function (lambda ()
                                       (mapcar (lambda (entry) (format nil "~A=~A" (first entry) (rest entry))) pairs))))
             (t
              (agentcomms:acp-invalid-params "Unsupported MCP transport."))))
         (secrets (remove-duplicates
                   (loop for pair in pairs
                         for value = (rest pair)
                         append (cons value
                                      (when (and (> (length value) 7)
                                                 (string-equal "Bearer " value :end2 7))
                                        (list (subseq value 7)))))
                   :test #'equal)))
    (make-instance 'parent-acp-mcp-server :identifier (parent-application--identifier)
                   :name name :secrets secrets
                   :forget-bindings                   (lambda () (setf pairs nil))
                   :client                            (mcparen:make-mcp-client transport :name "autolith-parent" :version "1"))))

(serapeum/types:-> parent-acp-mcp--discover (parent-acp-mcp-server) list)
(defun parent-acp-mcp--discover (server)
  "Refresh public tool metadata while retaining callable tool objects only in the parent."
  (let ((tools (mcparen:mcp-client-list-tools (parent-acp-mcp-client server))))
    (when (> (length tools) 256)
      (error 'parent-application-operation-error :code ':resource-limit))
    (clrhash (parent-acp-mcp-tools server))
    (loop for tool in tools
          do (setf (gethash (mcparen:mcp-tool-name tool) (parent-acp-mcp-tools server)) tool)
          collect (parent-acp-mcp--public
                   server
                   (agentcomms:json-object
                    "serverId" (parent-acp-mcp-identifier server) "serverName" (parent-acp-mcp-name server)
                    "name" (mcparen:mcp-tool-name tool) "description" (mcparen:mcp-tool-description tool)
                    "inputSchema" (mcparen:mcp-tool-input-schema tool)
                    "annotations" (mcparen:mcp-tool-annotations tool))))))

(serapeum/types:-> parent-acp-mcp-open (parent-acp-session list) vector)
(defun parent-acp-mcp-open (session declarations)
  "Retain client declarations only in parent memory and return sanitized tool descriptors."
  (when (> (length declarations) 32)
    (agentcomms:acp-invalid-params "Too many MCP servers."))
  (let ((names (make-hash-table :test #'equal)) (descriptors nil))
    (dolist (declaration declarations)
      (let ((server (parent-acp-mcp--server session declaration)))
        (when (gethash (parent-acp-mcp-name server) names)
          (mcparen:mcp-client-close (parent-acp-mcp-client server))
          (agentcomms:acp-invalid-params "Duplicate MCP server name."))
        (setf (gethash (parent-acp-mcp-name server) names) t
              (gethash (parent-acp-mcp-identifier server) (parent-acp-session-mcp session)) server)
        (setf descriptors (nconc descriptors (parent-acp-mcp--discover server)))))
    (coerce descriptors 'vector)))

(serapeum/types:-> parent-acp-mcp-close (parent-acp-session) null)
(defun parent-acp-mcp-close (session)
  "Close all transports before forgetting literal credentials. Preserve failed cleanup for retry."
  (let ((closed nil))
    (maphash (lambda (identifier server)
               (bordeaux-threads:with-lock-held ((parent-acp-mcp-lock server))
                 (mcparen:mcp-client-close (parent-acp-mcp-client server))
                 (funcall (parent-acp-mcp-forget-bindings server))
                 (setf (parent-acp-mcp-secrets server) nil)
                 (push identifier closed)))
             (parent-acp-session-mcp session))
    (dolist (identifier closed) (remhash identifier (parent-acp-session-mcp session))))
  nil)

(serapeum/types:-> parent-acp-mcp--authorize
                  (parent-acp-session &key (:server parent-acp-mcp-server) (:tool string)
                   (:arguments hash-table) (:cancelled function)) boolean)
(defun parent-acp-mcp--authorize (session &key server tool arguments cancelled)
  "Ask the editor for the exact private MCP call, unless the owner selected full access."
  (when (funcall cancelled)
    (error 'parent-application-operation-error :code ':cancelled))
  (or (equal (parent-acp-session-mode session) "full-access")
      (and (equal (parent-acp-session-mode session) "auto")
           (parent-application-authorize-tool
            (parent-acp-session-application session)
            (format nil "~A:~A" (parent-acp-mcp-name server) tool)
            arguments :cancelled cancelled))
      (progn
        (parent-acp--render session :region ':trusted :kind ':approval
                            :payload (list :operation-id (parent-application--identifier)
                                           :server (parent-acp-mcp-name server) :tool tool :arguments arguments))
        (multiple-value-bind (answer status) (parent-acp--permission-input session 60)
          (and (eq status ':line) (equal answer "YES") (not (funcall cancelled)) t)))))

(serapeum/types:-> parent-acp-mcp--operation (parent-acp-session hash-table function) hash-table)
(defun parent-acp-mcp--operation (session arguments cancelled)
  "Execute only list/call against parent-registered handles, never a child-supplied URL."
  (let* ((identifier (gethash "server_id" arguments))
         (operation  (gethash "operation" arguments))
         (server     (and (stringp identifier) (gethash identifier (parent-acp-session-mcp session)))))
    (unless (and server (member operation '("list" "call") :test #'equal))
      (error 'parent-application-operation-error :code ':invalid-request))
    (bordeaux-threads:with-lock-held ((parent-acp-mcp-lock server))
      (when (funcall cancelled)
        (error 'parent-application-operation-error :code ':cancelled))
      (handler-case
          (if (string= operation "list")
              (agentcomms:json-object "tools" (coerce (parent-acp-mcp--discover server) 'vector))
              (let* ((name       (gethash "tool_name" arguments))
                     (parameters (gethash "arguments" arguments))
                     (tool       (and (stringp name) (gethash name (parent-acp-mcp-tools server)))))
                (unless (and tool (hash-table-p parameters))
                  (error 'parent-application-operation-error :code ':invalid-request))
                (unless (parent-acp-mcp--authorize session :server server :tool name
                                                    :arguments parameters :cancelled cancelled)
                  (error 'parent-application-operation-error :code ':permission-denied))
                (let ((done    nil)
                      (lock    (bordeaux-threads:make-lock "parent MCP cancellation"))
                      (watcher nil))
                  (unwind-protect
                       (progn
                         (setf watcher
                               (bordeaux-threads:make-thread
                                (lambda ()
                                  (loop
                                    (when (bordeaux-threads:with-lock-held (lock) done) (return))
                                    (when (funcall cancelled)
                                      (mcparen:mcp-client-close (parent-acp-mcp-client server))
                                      (return))
                                    (sleep 0.02)))
                                :name "parent ACP MCP cancellation"))
                         (let ((result (mcparen:mcp-client-call-tool
                                        (parent-acp-mcp-client server) tool parameters :timeout 120)))
                           (when (funcall cancelled)
                             (error 'parent-application-operation-error :code ':cancelled))
                           (agentcomms:json-object "result" (parent-acp-mcp--public server (mcparen:mcp-call-result-raw result)))))
                    (bordeaux-threads:with-lock-held (lock) (setf done t))
                    (when watcher
                      (sb-thread:join-thread watcher :timeout 5 :default nil))))))
        (mcparen:mcp-error ()
          (error 'parent-application-operation-error
                 :code (if (funcall cancelled) ':cancelled ':unavailable)))))))

(serapeum/types:-> parent-acp-operation-handlers (parent-acp-session) list)
(defun parent-acp-operation-handlers (session)
  "Return fixed private operation callbacks for this ACP session."
  (list (list ':mcp (lambda (arguments cancelled)
                     (parent-acp-mcp--operation session arguments cancelled)))))
