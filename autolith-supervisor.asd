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
  :depends-on              (#:autolith-supervisor #:autolith-protocol #:bordeaux-threads)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/protobuf-validation")
                            (:file "supervisor/values")
                            (:file "supervisor/protocol"))
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
                            (:file "supervisor/session-dispatch"))
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
