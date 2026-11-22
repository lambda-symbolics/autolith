(in-package #:autolith)

;;;; -- ACP MCP Overlay Tests --

(defclass acp-mcp-test-transport (test-mcp-transport)
  ((projector :initarg :projector :initform nil :reader acp-mcp-test-projector
              :documentation "The production stdio ingress projection callback.")
   (scope :initarg :scope :initform #'funcall :reader acp-mcp-test-scope
          :documentation "The production HTTP exchange callback.")
   (bindings :initarg :bindings :initform nil :reader acp-mcp-test-bindings
             :documentation "The production environment or header callback.")
   (observed-bindings :initform nil :accessor acp-mcp-test-observed-bindings
                      :documentation "Values observed at transport open."))
  (:documentation "A scripted wire endpoint exercising the ACP transport callbacks."))

(defmethod mcp-transport-open :before ((transport acp-mcp-test-transport))
  "Materialize client bindings at the actual transport-open boundary."
  (when (acp-mcp-test-bindings transport)
    (setf (acp-mcp-test-observed-bindings transport)
          (funcall (acp-mcp-test-bindings transport)))))

(defmethod mcp-transport-request :around
    ((transport acp-mcp-test-transport) request timeout)
  "Apply production exchange and reader scopes to scripted wire responses."
  (funcall (acp-mcp-test-scope transport)
           (lambda ()
             (let ((response (call-next-method)))
               (if (acp-mcp-test-projector transport)
                   (funcall (acp-mcp-test-projector transport) ':response response)
                   response)))))

(-> acp-mcp-test--handler (string) function)
(defun acp-mcp-test--handler (echo)
  "Return a modern endpoint echoing ECHO in discovery metadata and tool results."
  (lambda (transport request)
    (let ((method (json-get request "method")))
      (cond
        ((equal method "server/discover")
         (test-mcp--rpc-result
          request (json-object "resultType" "complete"
                               "supportedVersions" (vector "2026-07-28")
                               "capabilities" (json-object "tools" (json-object)))))
        ((equal method "tools/list")
         (test-mcp--rpc-result
          request (json-object "resultType" "complete"
                               "tools" (vector (test-mcp--tool-definition
                                                "echo" :description echo :read-only-p t
                                                :destructive-p nil)))))
        ((equal method "tools/call")
         (test-mcp--rpc-result
          request (json-object "resultType" "complete"
                               "content" (vector (json-object "type" "text" "text" echo)))))
        (t
         (test-mcp--handler transport request))))))

(-> test-acp-mcp-overlay-configuration () null)
(defun test-acp-mcp-overlay-configuration ()
  "Decode validated declarations and retain owned literal bindings without starting transports."
  (let* ((env          (agentcomms:acp-env-variable "TOKEN" "fixture-value"))
         (header       (agentcomms:acp-http-header "Authorization" "fixture-value"))
         (stdio        (agentcomms:acp-mcp-server-stdio
                        "stdio" "echo" :arguments '("hello") :environment (list env)))
         (http         (agentcomms:acp-mcp-server-http
                        "http" "https://example.invalid/mcp" :headers (list header)))
         (stdio-config (acp-mcp--server stdio))
         (http-config  (acp-mcp--server http))
         (stdio-policy (mcp-server-configuration-transport stdio-config))
         (http-policy  (mcp-server-configuration-transport http-config)))
    (test-assert (typep stdio-policy 'acp-mcp-stdio-transport-configuration)
                 "stdio declarations retain native validated policy")
    (test-assert (typep http-policy 'acp-mcp-http-transport-configuration)
                 "HTTP declarations retain native validated policy")
    (loop for policy in (list stdio-policy http-policy)
          for entry  in (list env header)
          for pair   = (first (acp-mcp--transport-values policy))
          for name   = (json-get entry "name")
          for value  = (json-get entry "value")
          do (test-assert (and (equal pair (cons name value))
                               (not (eq (first pair) name))
                               (not (eq (rest pair) value)))
                          "literal binding strings do not alias the declaration"))
    (test-assert (handler-case
                     (progn (acp-mcp--server
                              (agentcomms:acp-mcp-server-sse "sse" "https://example.invalid"))
                            nil)
                   (agentcomms:acp-method-error () t))
                 "unsupported SSE declarations fail before construction"))
  nil)

(-> test-acp-mcp-overlay-discovery-call-and-cleanup () null)
(defun test-acp-mcp-overlay-discovery-call-and-cleanup ()
  "Merge native/client servers and redact literal credentials at real managed exchanges."
  (with-test-configuration (configuration)
    (let* ((*mcp-server-registrations* nil)
           (secret                     "acp-literal-fixture-value")
           (transports                 nil)
           (registry                   nil)
           (declarations
            (list (agentcomms:acp-mcp-server-stdio
                   "client-stdio" "stdio-fixture"
                   :environment (list (agentcomms:acp-env-variable "TOKEN" secret)))
                  (agentcomms:acp-mcp-server-http
                   "client-http" "https://example.invalid/mcp"
                   :headers (list (agentcomms:acp-http-header "Authorization" secret))))))
      (register-mcp-server '(:name "native" :transport (:type :stdio :command "native-fixture")
                             :approval :allow) :source ':runtime)
      (test-call-with-function-replacements
       (list
        (list 'make-mcp-stdio-transport
              (lambda (command &rest options)
                (let ((transport
                       (make-instance 'acp-mcp-test-transport
                                      :handler   (acp-mcp-test--handler
                                                  (if (equal command "native-fixture") "native-result" secret))
                                      :projector (getf options :ingress-projector)
                                      :bindings  (getf options :environment-function))))
                  (push transport transports)
                  transport)))
        (list 'make-mcp-streamable-http-transport
              (lambda (url &rest options)
                (declare (ignore url))
                (let ((transport
                       (make-instance 'acp-mcp-test-transport
                                      :handler  (acp-mcp-test--handler secret)
                                      :scope    (getf options :exchange-scope-function)
                                      :bindings (getf options :headers-function))))
                  (push transport transports)
                  transport))))
       (lambda ()
         (unwind-protect
              (progn
                (setf registry (acp-mcp-create-tool-registry configuration declarations))
                (let* ((manager      (mcp-tool-registry-manager registry))
                       (runtimes     (mcp-manager-runtimes manager))
                       (conversation (conversation-create configuration))
                       (context      (test-mcp--context configuration conversation registry)))
                  (test-assert (= 3 (length runtimes)) "native and client servers share one manager")
                  (test-assert (= 1 (length (mcp-server-registrations)))
                               "client registrations have no global lifetime")
                  (dolist (runtime runtimes)
                    (let* ((tool (find runtime (mcp-tool-registry-discovered-tools registry)
                                       :test #'eq
                                       :key  (lambda (candidate)
                                               (mcp-provider-tool-runtime candidate))))
                           (activated
                             (mcp-tool-registry-activate-tools registry (list tool)))
                           (result
                            (mcparen:mcp-server-runtime-call
                             runtime
                             (lambda (client)
                               (mcp-tools--call-result
                                tool context
                                (mcp-client-call-tool client (mcp-provider-tool-raw-tool tool)
                                                      (json-object)))))))
                      (test-assert tool "managed discovery publishes every server's tool")
                      (test-assert (member (tool-canonical-name tool) activated
                                          :test #'string=)
                                   "discovery tools activate on demand")
                      (test-assert (not (search secret (tool-description tool)))
                                   "discovery metadata is redacted")
                      (test-assert (not (search secret (tool-result-content result)))
                                   "tool results are redacted before leaving the exchange scope")))
                  (test-assert
                   (some (lambda (transport)
                           (member (concatenate 'string "TOKEN=" secret)
                                   (acp-mcp-test-observed-bindings transport) :test #'equal))
                         transports)
                   "stdio receives the literal client environment")
                  (test-assert
                   (some (lambda (transport)
                           (equal (assoc "Authorization" (remove-if-not #'consp
                                                                        (acp-mcp-test-observed-bindings transport))
                                         :test #'equal)
                                  (cons "Authorization" secret)))
                         transports)
                   "HTTP receives the literal client header")
                  (tool-registry-close-runtime-state registry)
                  (acp-mcp-release-credentials manager)
                  (test-assert (every (lambda (runtime)
                                        (null (acp-mcp--transport-values
                                               (mcp-server-configuration-transport
                                                (mcp-server-runtime-configuration runtime)))))
                                      runtimes)
                               "successful close drops retained client credential references")))
           (when registry (tool-registry-close-runtime-state registry)))))
      (test-assert (every (lambda (transport)
                            (not (test-mcp-transport-open-p transport))) transports)
                   "all merged transports close")))
  nil)

(-> test-acp-mcp-overlay-duplicate-validation () null)
(defun test-acp-mcp-overlay-duplicate-validation ()
  "Reject colliding server identities before constructing a transport."
  (with-test-configuration (configuration)
    (let ((*mcp-server-registrations* nil)
          (server                     (agentcomms:acp-mcp-server-stdio "duplicate" "fixture")))
      (dolist (native-p '(nil t))
        (when native-p
          (register-mcp-server '(:name "duplicate" :transport (:type :stdio :command "fixture"))
                               :source ':runtime))
        (test-assert
         (handler-case
             (progn (acp-mcp-create-tool-registry configuration
                                                  (if native-p (list server) (list server server)))
                    nil)
           (agentcomms:acp-method-error () t))
         "duplicate client/client and client/native names fail before connection"))))
  nil)

(-> test-acp-mcp-overlay-list-validation-order () null)
(defun test-acp-mcp-overlay-list-validation-order ()
  "Reject a malformed declaration before applying native policy to any declaration."
  (with-test-configuration (configuration)
    (flet ((rejection (declarations)
             (handler-case
                 (progn (acp-mcp-create-tool-registry configuration declarations) nil)
               (agentcomms:acp-method-error (condition)
                 (agentcomms:acp-error-message condition)))))
      (let* ((sse       (agentcomms:acp-mcp-server-sse "sse" "https://example.invalid"))
             (malformed (list (json-object "name" "no-command")))
             (expected  (rejection malformed)))
        (test-assert (and expected (equal expected (rejection (cons sse malformed))))
                     "malformed declarations take precedence over native SSE policy"))))
  nil)
