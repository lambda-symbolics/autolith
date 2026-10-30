;;;; -- Standalone Supervisor System --

;; Class types change from forward references to installed classes during load.
;; Muffle only SBCL's resulting proclamation mismatch, as for agent source files.
(defclass autolith-supervisor-source-file (asdf:cl-source-file)
  ()
  (:documentation "Supervisor source with class declaration reload reports suppressed."))

(defmethod asdf:perform :around
    ((operation asdf:load-op) (component autolith-supervisor-source-file))
  "Load COMPONENT without SBCL's class forward-reference redeclaration report."
  (handler-bind ((sb-kernel::ftype-proclamation-mismatch-warning #'muffle-warning))
    (call-next-method)))

(asdf:defsystem #:autolith-supervisor
  :description "A small process supervisor with inherited terminal descriptors."
  :license     "ISC"
  :version     (:read-file-line "VERSION")
  :depends-on  (#:uiop)
  :in-order-to ((asdf:test-op (asdf:test-op #:autolith-supervisor/tests)))
  :serial      t
  :components  ((:file "supervisor/package")
                (:file "supervisor/types")
                (:file "supervisor/permission-guidance")
                (:file "supervisor/framing")
                (:file "supervisor/process")
                (:file "supervisor/platform-posix" :if-feature (:not :win32))
                (:file "supervisor/platform-windows" :if-feature :win32)))

(asdf:defsystem #:autolith-supervisor/tests
  :description "Standalone parent-agent framing checks."
  :depends-on  (#:autolith-supervisor)
  :components  ((:file "supervisor/framing-tests"))
  :perform     (asdf:test-op (operation component)
                 (uiop:symbol-call '#:autolith-supervisor '#:run-framing-tests)))

(asdf:defsystem #:autolith-supervisor/private-process
  :description             "Private inherited byte transport and native subprocess cleanup."
  :depends-on              (#:autolith-supervisor)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/private-process-posix" :if-feature (:not :win32))
                            (:file "supervisor/private-process-windows" :if-feature :win32)
                            (:file "supervisor/private-process"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/private-process/tests))))

(asdf:defsystem #:autolith-supervisor/private-process/tests
  :description             "Real subprocess transport and cleanup checks."
  :depends-on              (#:autolith-supervisor/private-process)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/private-process-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-private-process-tests)))

(asdf:defsystem #:autolith-supervisor/terminal
  :description             "Parent-owned terminal input, output, and approval presentation."
  :depends-on              (#:autolith-supervisor/protocol #:bordeaux-threads)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/terminal"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/terminal/tests))))

(asdf:defsystem #:autolith-supervisor/terminal/tests
  :description             "Standalone terminal isolation, control escaping, and bounded I/O checks."
  :depends-on              (#:autolith-supervisor/terminal)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/terminal-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-parent-terminal-tests)))

(asdf:defsystem #:autolith-supervisor/accounts
  :description             "Shared private OAuth storage and ChatGPT account refresh without agent code."
  :depends-on              (#:autolith-supervisor #:cl-rfc8628 #:cl-llm-provider-api/dexador
                            #:sexp-store #:ls-compat/files #:bordeaux-threads)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/accounts"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/accounts/tests))))

(asdf:defsystem #:autolith-supervisor/accounts/tests
  :description             "Standalone shared OAuth account storage, refresh, and secret-scope checks."
  :depends-on              (#:autolith-supervisor/accounts #:ls-compat/posix)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/accounts-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-accounts-tests)))

(asdf:defsystem #:autolith-supervisor/provider-http
  :description             "Shared authenticated ChatGPT HTTP transport."
  :depends-on              (#:autolith-supervisor/accounts #:cl-llm-provider-api/dexador)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/provider-http")))

(asdf:defsystem #:autolith-supervisor/provider-executor
  :description             "Trusted parent ChatGPT execution and transport cleanup."
  :depends-on              (#:autolith-supervisor/model #:autolith-supervisor/provider-http)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/provider-executor"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/provider-executor/tests))))

(asdf:defsystem #:autolith-supervisor/provider-executor/tests
  :description             "Parent HTTP execution, retry, and cancellation checks."
  :depends-on              (#:autolith-supervisor/provider-executor #:trivial-gray-streams
                            #:autolith-supervisor/model/tests)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/provider-executor-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-provider-executor-tests)))

(asdf:defsystem #:autolith-supervisor/protocol
  :description             "Bounded Protobuf decoding and parent startup negotiation."
  :depends-on              (#:autolith-supervisor #:autolith-protocol #:bordeaux-threads
                            #:serapeum #:ironclad #:babel)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/protobuf-validation")
                            (:file "supervisor/values")
                            (:file "supervisor/protocol")
                            (:file "supervisor/protocol-application"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/protocol/tests))))

(asdf:defsystem #:autolith-supervisor/protocol/tests
  :description             "Malformed input and startup checks without the agent system."
  :depends-on              (#:autolith-supervisor/protocol #:autolith-supervisor/tests
                            #:trivial-gray-streams)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/protobuf-validation-tests")
                            (:file "supervisor/values-tests")
                            (:file "supervisor/protocol-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-protobuf-validation-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-protocol-value-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-protocol-handshake-tests)))

(asdf:defsystem #:autolith-supervisor/session
  :description             "Parent-owned durable sessions and session.v1 wire service."
  :depends-on              (#:autolith-supervisor/protocol #:sexp-store #:ls-flock #:ls-compat/posix)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/session-store")
                            (:file "supervisor/session-dispatch")
                            (:file "supervisor/session-application")
                            (:file "supervisor/parent-application-operations"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/session/tests))))

(asdf:defsystem #:autolith-supervisor/session/tests
  :description             "Durable session and wire lifecycle checks without agent code."
  :depends-on              (#:autolith-supervisor/session #:autolith-supervisor/protocol/tests)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/session-store-tests")
                            (:file "supervisor/session-dispatch-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-session-store-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-session-dispatch-tests)))

(asdf:defsystem #:autolith-supervisor/model
  :description             "Parent model projection, replay storage, and provider event collection."
  :depends-on              (#:autolith-supervisor/session #:cl-llm-provider-api/wire #:cl-base64
                            #:sexp-store #:ls-flock #:ls-compat/posix #:ironclad)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/model-profile")
                            (:file "supervisor/model-projection")
                            (:file "supervisor/model-replay-store")
                            (:file "supervisor/model-reconciliation")
                            (:file "supervisor/model-events")
                            (:file "supervisor/model-replay-adapter")
                            (:file "supervisor/model-runtime")
                            (:file "supervisor/model-service"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/model/tests))))

(asdf:defsystem #:autolith-supervisor/model/tests
  :description             "Model authority and family projection checks without agent code."
  :depends-on              (#:autolith-supervisor/model #:autolith-supervisor/session/tests)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/model-profile-tests")
                            (:file "supervisor/model-projection-tests")
                            (:file "supervisor/model-replay-store-tests")
                            (:file "supervisor/model-reconciliation-tests")
                            (:file "supervisor/model-events-tests")
                            (:file "supervisor/model-replay-adapter-tests")
                            (:file "supervisor/model-service-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-profile-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-projection-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-replay-store-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-reconciliation-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-event-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-replay-adapter-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-service-tests)))

(asdf:defsystem #:autolith-supervisor/agendas
  :description             "Shared agenda domain and atomic store."
  :depends-on              (#:autolith-supervisor/accounts #:sexp-store #:serapeum
                            #:bordeaux-threads #:ironclad)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/agendas")))

(asdf:defsystem #:autolith-supervisor/agenda-service
  :description             "Parent-owned agenda record service."
  :depends-on              (#:autolith-supervisor/agendas #:autolith-supervisor/protocol #:babel)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/agenda-service"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/agenda-service/tests))))

(asdf:defsystem #:autolith-supervisor/agenda-service/tests
  :description             "Agenda persistence and record service checks."
  :depends-on              (#:autolith-supervisor/agenda-service)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/agenda-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-agenda-store-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-agenda-service-tests)))

(asdf:defsystem #:autolith-supervisor/memory
  :description             "Shared memory, knowledge, evidence, and atomic stores."
  :depends-on              (#:autolith-supervisor/accounts #:serapeum #:bordeaux-threads
                            #:sexp-store #:ls-flock #:ls-compat/files #:ls-compat/posix
                            #:ironclad #:argo)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/memory-types")
                            (:file "supervisor/memory-context")
                            (:file "supervisor/memory-support")
                            (:file "supervisor/memory-sources")
                            (:file "supervisor/memory-knowledge-terms")
                            (:file "supervisor/memory-knowledge-schemas")
                            (:file "supervisor/memory-record-types")
                            (:file "supervisor/memory-store-state")
                            (:file "supervisor/memory-domain")
                            (:file "supervisor/memory-knowledge-store")
                            (:file "supervisor/memory-knowledge-rules")
                            (:file "supervisor/memory-knowledge-applicability")
                            (:file "supervisor/memory-knowledge-evaluation")
                            (:file "supervisor/memory-knowledge-text")
                            (:file "supervisor/memory-knowledge-methods")
                            (:file "supervisor/knowledge-plan-domain")
                            (:file "supervisor/memory-knowledge-contracts")
                            (:file "supervisor/memory-knowledge-explanation")
                            (:file "supervisor/memory-knowledge-procedure-views")
                            (:file "supervisor/knowledge-service-views")
                            (:file "supervisor/memory-records")
                            (:file "supervisor/memory-status")))

(asdf:defsystem #:autolith-supervisor/knowledge-service
  :description             "Parent-owned structured knowledge and contract requests."
  :depends-on              (#:autolith-supervisor/memory-service #:autolith-supervisor/session)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/knowledge-service")
                            (:file "supervisor/knowledge-service-operations")
                            (:file "supervisor/knowledge-service-context")
                            (:file "supervisor/knowledge-service-plan")
                            (:file "supervisor/knowledge-service-requests"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/knowledge-service/tests))))

(asdf:defsystem #:autolith-supervisor/knowledge-service/tests
  :description             "Knowledge service authority, guards and durable retries."
  :depends-on              (#:autolith-supervisor/knowledge-service)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/knowledge-service-tests")
                            (:file "supervisor/knowledge-service-context-tests")
                            (:file "supervisor/knowledge-service-plan-tests"))
  :perform                 (asdf:test-op (operation component)
                            (declare (ignore operation component))
                            (uiop:symbol-call '#:autolith-supervisor '#:run-knowledge-service-tests)
                            (uiop:symbol-call '#:autolith-supervisor '#:run-knowledge-context-tests)
                            (uiop:symbol-call '#:autolith-supervisor '#:run-knowledge-plan-tests)))

(asdf:defsystem #:autolith-supervisor/memory-service
  :description             "Parent-owned memory record service."
  :depends-on              (#:autolith-supervisor/memory #:autolith-supervisor/protocol)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/memory-service")
                            (:file "supervisor/memory-record-metadata"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/memory-service/tests))))

(asdf:defsystem #:autolith-supervisor/memory-service/tests
  :description             "Memory persistence, evidence, and record service checks."
  :depends-on              (#:autolith-supervisor/memory-service)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/memory-tests")
                            (:file "supervisor/memory-record-metadata-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-memory-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-memory-record-metadata-tests)))

(asdf:defsystem #:autolith-supervisor/papercuts
  :description             "Shared papercut lifecycle store."
  :depends-on              (#:autolith-supervisor/accounts #:sexp-store #:serapeum
                            #:bordeaux-threads #:ironclad #:babel)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/papercuts")))

(asdf:defsystem #:autolith-supervisor/papercut-service
  :description             "Parent-owned papercut record service."
  :depends-on              (#:autolith-supervisor/papercuts #:autolith-supervisor/protocol)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/papercut-service"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/papercut-service/tests))))

(asdf:defsystem #:autolith-supervisor/papercut-service/tests
  :description             "Papercut lifecycle and durable receipt checks."
  :depends-on              (#:autolith-supervisor/papercut-service)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/papercut-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-papercut-store-tests)))

(asdf:defsystem #:autolith-supervisor/parent-launch
  :description             "Parent-owned agent process and session composition."
  :depends-on              (#:autolith-supervisor/provider-executor
                            #:autolith-supervisor/private-process
                            #:autolith-supervisor/terminal)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/parent-launch"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/parent-launch/tests))))

(asdf:defsystem #:autolith-supervisor/parent-launch/tests
  :description             "Parent launch unit checks and fresh-process fixtures."
  :depends-on              (#:autolith-supervisor/parent-launch
                            #:autolith-supervisor/provider-executor/tests
                            #:autolith-supervisor/access-native
                            #:autolith-supervisor/access/session
                            #:autolith-supervisor/records)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/parent-launch-tests")
                            (:file "supervisor/parent-launch-e2e-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-parent-launch-unit-tests)))

(asdf:defsystem #:autolith-supervisor/parent-launch/production-tests
  :description             "Standard main-core production E2E fixtures."
  :depends-on              (#:autolith-supervisor/parent-application
                            #:autolith-supervisor/parent-launch/tests #:argo)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/parent-launch-production-tests")))

(asdf:defsystem #:autolith-supervisor/records
  :description             "Session-wide record dispatch and durable operation claims."
  :depends-on              (#:autolith-supervisor/session
                            #:autolith-supervisor/agenda-service
                            #:autolith-supervisor/memory-service
                            #:autolith-supervisor/papercut-service #:sexp-store #:ironclad)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/record-dispatch"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/records/tests))))

(asdf:defsystem #:autolith-supervisor/records/tests
  :description             "Framed record dispatch and operation claim checks."
  :depends-on              (#:autolith-supervisor/records #:autolith-supervisor/model)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/record-dispatch-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-record-dispatch-tests)))

(asdf:defsystem #:autolith-supervisor/access
  :description             "Parent-owned approved and active access revisions."
  :depends-on              (#:autolith-supervisor/terminal #:sexp-store #:ls-flock)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/access-policy")
                            (:file "supervisor/access-service"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/access/tests))))

(asdf:defsystem #:autolith-supervisor/access/tests
  :description             "Access approval, persistence, and replacement checks."
  :depends-on              (#:autolith-supervisor/access)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/access-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-parent-access-tests)))

(asdf:defsystem #:autolith-supervisor/access-native
  :description             "Native whole-agent access enforcement."
  :depends-on              (#:autolith-supervisor/access #:autolith-supervisor/private-process
                            #:bordeaux-threads #:cl-exec-sandbox #:uiop)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/access-native")
                            (:file "supervisor/access-native-darwin" :if-feature :darwin)
                            (:file "supervisor/access-native-linux" :if-feature :linux)
                            (:file "supervisor/access-native-windows" :if-feature :win32))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/access-native/tests))))

(asdf:defsystem #:autolith-supervisor/access-native/tests
  :description             "Native policy and descendant cleanup checks."
  :depends-on              (#:autolith-supervisor/access-native)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/access-native-linux" :if-feature (:not :linux))
                            (:file "supervisor/access-native-windows" :if-feature (:not :win32))
                            (:file "supervisor/access-native-tests")
                            (:file "supervisor/access-native-host-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-native-access-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-native-host-access-tests)))

(asdf:defsystem #:autolith-supervisor/access/session
  :description             "Asynchronous access requests on the parent session runtime."
  :depends-on              (#:autolith-supervisor/access #:autolith-supervisor/session)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/access-session"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/access/session/tests))))

(asdf:defsystem #:autolith-supervisor/access/session/tests
  :description             "Asynchronous approval, cancellation, and restart checks."
  :depends-on              (#:autolith-supervisor/access/session #:autolith-supervisor/access/tests)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/access-session-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-parent-access-session-tests)))

(asdf:defsystem #:autolith-supervisor/family-accounts
  :description             "Shared credential sources and refresh for provider families."
  :depends-on              (#:autolith-supervisor/accounts #:sexp-store #:ls-compat/posix
                            #:dexador #:cl-llm-provider-api #:cl-llm-provider-api/dexador
                            #:local-time)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/family-account-settings")
                            (:file "supervisor/provider-accounts-authentication"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/family-accounts/tests))))

(asdf:defsystem #:autolith-supervisor/family-accounts/tests
  :description             "Shared provider account settings and authentication checks."
  :depends-on              (#:autolith-supervisor/family-accounts)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/family-account-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-family-account-tests)))

(asdf:defsystem #:autolith-supervisor/provider-family-domain
  :description             "Shared Gemini setup and Copilot Auto policy."
  :depends-on              (#:autolith-supervisor/family-accounts #:serapeum)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/provider-family-domain"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/provider-family-domain/tests))))

(asdf:defsystem #:autolith-supervisor/provider-family-domain/tests
  :description             "Shared provider domain callback fixtures."
  :depends-on              (#:autolith-supervisor/provider-family-domain)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/provider-family-domain-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-family-domain-tests)))

(asdf:defsystem #:autolith-supervisor/provider-families
  :description             "Parent accounts and execution for supported provider families."
  :depends-on              (#:autolith-supervisor/provider-executor
                            #:autolith-supervisor/family-accounts
                            #:autolith-supervisor/provider-family-domain #:local-time)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/provider-accounts")
                            (:file "supervisor/provider-accounts-factory")
                            (:file "supervisor/provider-family-executor-policy")
                            (:file "supervisor/provider-family-executor")
                            (:file "supervisor/provider-family-executor-authentication"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/provider-families/tests))))

(asdf:defsystem #:autolith-supervisor/provider-families/tests
  :description             "Provider-family projections and local transport checks."
  :depends-on              (#:autolith-supervisor/provider-families
                            #:autolith-supervisor/provider-executor/tests
                            #:autolith-supervisor/provider-family-domain/tests)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/provider-family-executor-tests")
                            (:file "supervisor/provider-family-domain-adapter-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-provider-family-executor-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-parent-family-domain-adapter-tests)))

(asdf:defsystem #:autolith-supervisor/terminal-ui
  :description             "Shared terminal editor and trusted parent frontend."
  :depends-on              (#:autolith-supervisor/terminal #:clinedi #:cl-colorist
                            (:feature (:not :win32) #:clinedi/posix)
                            (:feature :win32 #:clinedi/win32)
                            (:feature :win32 #:cffi)
                            #:colordiff #:colorlisp #:cl-termdown #:serapeum
                            #:bordeaux-threads #:ls-compat #:yolokuva #:sophisticated-clipboard
                            #:image-daemon/runtime)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/terminal-ui-support")
                            (:file "supervisor/terminal-ui-host-posix" :if-feature (:not :win32))
                            (:file "supervisor/terminal-ui-host-windows" :if-feature :win32)
                            (:file "supervisor/terminal-ui-input-value")
                            (:file "supervisor/terminal-ui-protocol")
                            (:file "supervisor/terminal-ui-relay")
                            (:file "supervisor/terminal-ui-style")
                            (:file "supervisor/terminal-ui-layout")
                            (:file "supervisor/terminal-ui-input")
                            (:file "supervisor/terminal-ui-stream")
                            (:file "supervisor/terminal-ui-path-completion")
                            (:file "supervisor/terminal-ui-syntax-highlighting")
                            (:file "supervisor/terminal-ui")
                            (:file "supervisor/terminal-ui-fullscreen")
                            (:file "supervisor/terminal-ui-boot-screen")
                            (:file "supervisor/terminal-ui-clipboard")
                            (:file "supervisor/terminal-ui-events")
                            (:file "supervisor/terminal-ui-frontend")))

(asdf:defsystem #:autolith-supervisor/parent-provider-extensions
  :description             "Explicit trusted parent provider callbacks and public metadata."
  :depends-on              (#:autolith-supervisor/provider-families
                            #:cl-llm-provider-api/registry)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/parent-provider-extensions")))

(asdf:defsystem #:autolith-supervisor/provider-login
  :description             "Shared trusted provider login adapters."
  :depends-on              (#:autolith-supervisor/provider-families #:cl-rfc8252)
  :default-component-class autolith-supervisor-source-file
  :components              ((:file "supervisor/parent-provider-login")))

(asdf:defsystem #:autolith-supervisor/parent-application
  :description             "Trusted parent configuration, services, and replacement control."
  :depends-on              (#:autolith-supervisor/parent-launch
                            #:autolith-supervisor/provider-families
                            #:autolith-supervisor/parent-provider-extensions
                            #:autolith-supervisor/provider-login
                            #:autolith-supervisor/terminal-ui
                            #:autolith-supervisor/records
                            #:autolith-supervisor/knowledge-service
                            #:autolith-supervisor/access/session
                            #:agentcomms #:image-daemon/runtime #:mcparen #:local-time #:sbcl-generations
                            #:dexador #:babel #:quri #:fetch-gist #:yason
                            #:autolith-supervisor/access-native)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/parent-configuration")
                            (:file "supervisor/parent-provider-bindings")
                            (:file "supervisor/parent-cli")
                            (:file "supervisor/parent-cli-configuration")
                            (:file "supervisor/parent-application")
                            (:file "supervisor/parent-web")
                            (:file "supervisor/parent-application-network")
                            (:file "supervisor/parent-cli-settings-operations")
                            (:file "supervisor/parent-session-compatibility")
                            (:file "supervisor/parent-session-artifacts")
                            (:file "supervisor/parent-detached")
                            (:file "supervisor/parent-acp")
                            (:file "supervisor/parent-acp-events")
                            (:file "supervisor/parent-acp-mcp")
                            (:file "supervisor/parent-application-interface")
                            (:file "supervisor/parent-session-cli")
                            (:file "supervisor/parent-standard-operations")
                            (:file "supervisor/parent-cli-dispatch"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/parent-application/tests))))

(asdf:defsystem #:autolith-supervisor/parent-application/tests
  :description             "Production parent configuration and session control checks."
  :depends-on              (#:autolith-supervisor/parent-application
                            #:autolith-supervisor/provider-executor/tests)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/parent-application-tests")
                            (:file "supervisor/terminal-ui-tests")
                            (:file "supervisor/parent-cli-tests")
                            (:file "supervisor/parent-provider-extensions-tests")
                            (:file "supervisor/parent-provider-bindings-tests")
                            (:file "supervisor/parent-provider-login-tests")
                            (:file "supervisor/parent-standard-operations-tests")
                            (:file "supervisor/parent-web-tests")
                            (:file "supervisor/parent-application-network-tests")
                            (:file "supervisor/parent-provider-extensions-e2e-tests")
                            (:file "supervisor/parent-session-compatibility-tests")
                            (:file "supervisor/parent-application-interface-tests")
                            (:file "supervisor/parent-interface-adapters-tests"))
  :perform                 (asdf:test-op (operation component)
                             (declare (ignore operation component))
                             (dolist (name '(run-parent-application-tests
                                             run-parent-terminal-ui-tests
                                             run-parent-cli-tests
                                             run-parent-provider-login-tests
                                             run-parent-web-tests
                                             run-parent-application-network-tests
                                             run-parent-cli-settings-tests
                                             run-parent-cli-launcher-tests
                                             run-parent-session-compatibility-reader-tests
                                             run-parent-session-compatibility-tests
                                             run-parent-session-artifact-tests
                                             run-parent-session-cli-tests
                                             run-parent-application-interface-tests
                                             run-parent-interface-adapters-tests))
                               (uiop:symbol-call '#:autolith-supervisor name))
                             (let ((temporary (merge-pathnames
                                               (format nil "autolith-parent-extensions-~A/" (gensym))
                                               (uiop:temporary-directory))))
                               (unwind-protect
                                   (progn
                                     (uiop:symbol-call '#:autolith-supervisor
                                                       '#:run-parent-provider-extension-tests temporary)
                                     (uiop:symbol-call '#:autolith-supervisor
                                                       '#:run-parent-provider-binding-tests temporary)
                                     (uiop:symbol-call '#:autolith-supervisor
                                                       '#:run-parent-standard-operations-tests temporary))
                                 (uiop:symbol-call '#:autolith-supervisor
                                                  '#:parent-application-test--cleanup temporary)))))
