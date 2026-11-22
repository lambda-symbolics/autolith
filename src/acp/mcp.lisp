(in-package #:autolith)

;;;; -- ACP MCP Overlay --

(defclass acp-mcp-stdio-transport-configuration
    (mcp-stdio-transport-configuration)
  ((environment-values
    :initarg       :environment-values
    :accessor      acp-mcp-environment-values
    :documentation "Client-supplied literal (NAME . VALUE) environment pairs."))
  (:documentation
   "A stdio MCP transport carrying literal ACP client environment credentials."))

(defclass acp-mcp-http-transport-configuration
    (mcp-http-transport-configuration)
  ((header-values
    :initarg       :header-values
    :accessor      acp-mcp-header-values
    :documentation "Client-supplied literal (NAME . VALUE) HTTP header pairs."))
  (:documentation
   "An HTTP MCP transport carrying literal ACP client header credentials."))

(-> acp-mcp--credential-scope (list function) t)
(defun acp-mcp--credential-scope (values function)
  "Call FUNCTION with literal ACP credentials dynamically scoped and redacted."
  (call-with-secret-use
   (lambda ()
     (let ((credentials (remove-duplicates (mapcar #'rest values) :test #'string=)))
       (let ((*mcp-active-credential-values* credentials)
             (*mcp-active-credential-redaction-marker*
              (safe-redaction-marker *mcp-credential-redaction-marker*
                                     credentials)))
         (funcall function))))))

(-> acp-mcp--project-ingress (t t) t)
(defun acp-mcp--project-ingress (kind value)
  "Redact server-controlled VALUE while an ACP credential scope is active."
  (declare (ignore kind))
  (mcp-tools--sanitize-value value))

(defmethod mcp-transport-configuration--materialize
    ((transport acp-mcp-stdio-transport-configuration)
     (server-configuration mcp-server-configuration)
     (configuration configuration)
     &key notification-handler exchange-scope-function)
  "Materialize a stdio transport whose environment carries ACP literal credentials."
  (declare (ignore exchange-scope-function))
  (make-mcp-stdio-transport
   (mcp-stdio-configuration-command transport)
   :arguments            (mcp-stdio-configuration-arguments transport)
   :directory            (lambda ()
                           (mcp-tools--stdio-directory
                            server-configuration configuration transport))
   :environment-function
   (lambda ()
     (acp-mcp--credential-scope
      (acp-mcp-environment-values transport)
      (lambda ()
        (cons "AUTOLITH_MCP=1"
              (mapcar (lambda (entry)
                        (format nil "~A=~A" (first entry) (rest entry)))
                      (acp-mcp-environment-values transport))))))
   :notification-handler notification-handler
   :ingress-projector
   (lambda (kind value)
     (acp-mcp--credential-scope
      (acp-mcp-environment-values transport)
      (lambda () (acp-mcp--project-ingress kind value))))))

(defmethod mcp-transport-configuration--materialize
    ((transport acp-mcp-http-transport-configuration)
     (server-configuration mcp-server-configuration)
     (configuration configuration)
     &key notification-handler exchange-scope-function)
  "Materialize an HTTP transport whose headers and exchanges carry ACP literal credentials."
  (make-mcp-streamable-http-transport
   (mcp-http-configuration-url transport)
   :headers-function
   (lambda ()
     (acp-mcp--credential-scope
      (acp-mcp-header-values transport)
      (lambda () (copy-tree (acp-mcp-header-values transport)))))
   :exchange-scope-function
   (lambda (function)
     (funcall (or exchange-scope-function #'funcall)
              (lambda ()
                (acp-mcp--credential-scope
                 (acp-mcp-header-values transport) function))))
   :notification-handler notification-handler
   :connect-timeout
   (mcp-http-configuration-connect-timeout-seconds transport)))

(-> acp-mcp--pair-field (hash-table string) t)
(defun acp-mcp--pair-field (object name)
  "Require a literal pair member, leaving present values to the pair check."
  (let ((value (json-get object name :absent)))
    (when (eq value :absent)
      (agentcomms:acp-invalid-params "MCP server field ~A is required." name))
    value))

(-> acp-mcp--pairs (t string) list)
(defun acp-mcp--pairs (entries label)
  "Validate literal ACP name/value entries and copy their strings."
  (unless (or (null entries) (vectorp entries) (listp entries))
    (agentcomms:acp-invalid-params "MCP ~A must be an array." label))
  (loop for entry in (if (vectorp entries) (coerce entries 'list) entries)
        collect
        (progn
          (unless (json-object-p entry)
            (agentcomms:acp-invalid-params
             "Each MCP ~A entry must be an object." label))
          (let ((name  (acp-mcp--pair-field entry "name"))
                (value (acp-mcp--pair-field entry "value")))
            (unless (and (non-empty-string-p name) (stringp value))
              (agentcomms:acp-invalid-params
               "MCP ~A entries must contain string name and value." label))
            (cons (copy-seq name) (copy-seq value))))))

(-> acp-mcp--server (hash-table) mcp-server-configuration)
(defun acp-mcp--server (server)
  "Attach ephemeral literal bindings to native policy for a validated declaration."
  (let* ((name (agentcomms:acp-field server "name"))
         (kind (agentcomms:acp-mcp-server-transport server))
         (policy
          (case kind
            (:stdio
             (list :type ':stdio :command (agentcomms:acp-field server "command")
                   :arguments (coerce (agentcomms:acp-field server "args") 'list)
                   :directory ':workspace :environment nil))
            (:http
             (list :type ':http :url (agentcomms:acp-field server "url") :headers nil))
            (otherwise
             (agentcomms:acp-invalid-params
              "Unsupported MCP transport ~A." (json-get server "type"))))))
    (handler-case
        (let* ((configuration (mcp-server-configuration-create
                               :name name :transport policy :approval ':prompt :required-p t))
               (transport     (mcp-server-configuration-transport configuration)))
          (if (eq kind ':stdio)
              (change-class transport 'acp-mcp-stdio-transport-configuration
                            :environment-values (acp-mcp--pairs (json-get server "env") "environment"))
              (change-class transport 'acp-mcp-http-transport-configuration
                            :header-values (acp-mcp--pairs (json-get server "headers") "headers")))
          configuration)
      (mcp-configuration-error ()
        (agentcomms:acp-invalid-params "Invalid MCP configuration for ~A." name)))))

(defgeneric acp-mcp--transport-values (transport)
  (:documentation "Return ephemeral literal credential bindings, or NIL for native policy."))

(defmethod acp-mcp--transport-values ((transport mcp-transport-configuration))
  "Native transports resolve their credentials through the native secret boundary."
  nil)

(defmethod acp-mcp--transport-values ((transport acp-mcp-stdio-transport-configuration))
  "Return the client-supplied environment values."
  (acp-mcp-environment-values transport))

(defmethod acp-mcp--transport-values ((transport acp-mcp-http-transport-configuration))
  "Return the client-supplied header values."
  (acp-mcp-header-values transport))

(defmethod mcparen:mcp-managed-call-with-scope :around ((runtime mcp-server-runtime) function)
  "Install literal redaction inside the native exchange scope."
  (let ((values (acp-mcp--transport-values
                 (mcp-server-configuration-transport (mcp-server-runtime-configuration runtime)))))
    (if values
        (call-next-method runtime (lambda () (acp-mcp--credential-scope values function)))
        (call-next-method))))

(defmethod mcparen:mcp-managed-call-with-cleanup :around ((runtime mcp-server-runtime) function)
  "Apply literal redaction during cleanup before dropping the client declarations."
  (let ((values (acp-mcp--transport-values
                 (mcp-server-configuration-transport (mcp-server-runtime-configuration runtime)))))
    (if values
        (call-next-method runtime (lambda () (acp-mcp--credential-scope values function)))
        (call-next-method))))

(-> acp-mcp-create-tool-registry (configuration list) tool-registry)
(defun acp-mcp-create-tool-registry (configuration declarations)
  "Build the native registry with session-local client servers and no global mutation.

ACP validates its wire requests separately. World :OPEN arguments may come
from local calls or IPC, so check the whole list here before applying native
transport policy or decoding literal credentials."
  (let* ((servers                    (mapcar #'acp-mcp--server
                                             (agentcomms:acp-validate-mcp-servers declarations)))
         (*mcp-server-registrations* (mcp-server-registrations))
         (names                      (make-hash-table :test #'equal)))
    (dolist (registration *mcp-server-registrations*)
      (setf (gethash (mcp-server-configuration-name
                      (mcp-server-registration-configuration registration)) names) t))
    (dolist (server servers)
      (let ((name (mcp-server-configuration-name server)))
        (when (gethash name names)
          (agentcomms:acp-invalid-params "MCP server name ~A is already registered." name))
        (setf (gethash name names) t)
        (register-mcp-server server :source ':runtime)))
    (application--create-tool-registry configuration)))

(-> acp-mcp-release-credentials ((option mcp-manager)) null)
(defun acp-mcp-release-credentials (manager)
  "Forget literal client values after MANAGER's resources have closed successfully."
  (when manager
    (dolist (runtime (mcp-manager-runtimes manager))
      (let ((transport
             (mcp-server-configuration-transport
              (mcp-server-runtime-configuration runtime))))
        (when (typep transport 'acp-mcp-stdio-transport-configuration)
          (setf (acp-mcp-environment-values transport) nil))
        (when (typep transport 'acp-mcp-http-transport-configuration)
          (setf (acp-mcp-header-values transport) nil)))))
  nil)
