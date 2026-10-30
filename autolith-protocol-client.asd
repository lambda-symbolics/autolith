;;;; -- Optional Agent Protocol Client --

(let ((definition (merge-pathnames "autolith-agent.asd" *load-truename*)))
  (unless (find-class 'autolith-source-file nil)
    (asdf:load-asd definition)))

(asdf:defsystem #:autolith-protocol-client
  :description             "Explicit-stream agent client for parent session and model protocols."
  :version                 (:read-file-line "VERSION")
  :license                 "ISC"
  :depends-on              (#:autolith-agent #:autolith-supervisor/protocol)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-client")
                            (:file "src/agent/protocol-model-client")
                            (:file "src/agent/protocol-context")
                            (:file "src/agent/protocol-driver")
                            (:file "src/agent/protocol-provider")
                            (:file "src/agent/protocol-session-codec")
                            (:file "src/agent/protocol-session-conversation")
                            (:file "src/agent/protocol-session")
                            (:file "src/agent/protocol-terminal-ui")
                            (:file "src/agent/protocol-application")
                            (:file "src/agent/protocol-application-input")
                            (:file "src/agent/protocol-application-job")
                            (:file "src/agent/protocol-application-acp")
                            (:file "src/agent/protocol-web")
                            (:file "src/agent/protocol-observer"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/tests))))

(asdf:defsystem #:autolith-protocol-client/tests
  :description             "Serialized parent and agent protocol boundary checks."
  :depends-on              (#:autolith-protocol-client #:autolith-protocol-client/entry
                            #:autolith-supervisor/parent-application/tests
                            #:autolith-supervisor/protocol/tests
                            #:autolith-supervisor/model #:autolith-supervisor/provider-families/tests)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-client-tests")
                            (:file "src/agent/protocol-model-client-tests")
                            (:file "src/agent/protocol-model-runtime-tests")
                            (:file "src/agent/protocol-context-tests")
                            (:file "src/agent/protocol-driver-tests")
                            (:file "src/agent/protocol-provider-tests")
                            (:file "src/agent/protocol-session-tests")
                            (:file "src/agent/protocol-application-tests")
                            (:file "src/agent/protocol-web-tests")
                            (:file "src/agent/protocol-observer-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-model-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-model-runtime-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-context-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-driver-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-provider-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-session-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-application-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-web-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-observer-tests)))

(asdf:defsystem #:autolith-protocol-client/records
  :description             "Typed parent-owned record client and agenda adapter."
  :depends-on              (#:autolith-protocol-client)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-record-client")
                            (:file "src/agent/protocol-agenda")
                            (:file "src/agent/protocol-papercut")
                            (:file "src/agent/protocol-memory"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/records/tests))))

(asdf:defsystem #:autolith-protocol-client/records/tests
  :description             "Serialized record client boundary checks."
  :depends-on              (#:autolith-protocol-client/records
                            #:autolith-protocol-client/tests
                            #:autolith-supervisor/records
                            #:autolith-supervisor/agenda-service
                            #:autolith-supervisor/papercut-service
                            #:autolith-supervisor/memory-service)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-record-client-tests")
                            (:file "src/agent/protocol-agenda-tests")
                            (:file "src/agent/protocol-papercut-tests")
                            (:file "src/agent/protocol-memory-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-record-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-agenda-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-papercut-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-memory-tests)))

(asdf:defsystem #:autolith-protocol-client/entry
  :description             "Fresh parent-backed agent process entry."
  :depends-on              (#:autolith-protocol-client/knowledge/resources
                            #:autolith-protocol-client/access
                            #:autolith-supervisor/private-process)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-entry")))

(asdf:defsystem #:autolith-protocol-client/entry/tests
  :description             "Fresh agent process fixture entry."
  :depends-on              (#:autolith-protocol-client/entry)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-entry-tests")))

(asdf:defsystem #:autolith-protocol-client/access
  :description             "Typed parent access decisions and path authorization."
  :depends-on              (#:autolith-protocol-client)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-access"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/access/tests))))

(asdf:defsystem #:autolith-protocol-client/access/tests
  :description             "Framed access decisions, cancellation, and path authorization checks."
  :depends-on              (#:autolith-protocol-client/access #:autolith-protocol-client/tests)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-access-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-access-tests)))

(asdf:defsystem #:autolith-protocol-client/records/resources
  :description             "Parent record services through the resource protocol."
  :depends-on              (#:autolith-protocol-client/records #:autolith-protocol-client/knowledge)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-record-resources"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/records/resources/tests))))

(asdf:defsystem #:autolith-protocol-client/records/resources/tests
  :description             "Record resource tools on serialized parent connections."
  :depends-on              (#:autolith-protocol-client/records/resources
                            #:autolith-protocol-client/records/tests)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-record-resources-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-record-resource-tests)))

(asdf:defsystem #:autolith-protocol-client/knowledge
  :description             "Typed parent-owned knowledge client."
  :depends-on              (#:autolith-protocol-client)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-knowledge"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/knowledge/tests))))

(asdf:defsystem #:autolith-protocol-client/knowledge/resources
  :description             "Parent knowledge resources, contracts and turn context."
  :depends-on              (#:autolith-protocol-client/records/resources)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-knowledge-resources")
                            (:file "src/agent/protocol-knowledge-contracts")
                            (:file "src/agent/protocol-knowledge-context")
                            (:file "src/agent/protocol-knowledge-plan")))

(asdf:defsystem #:autolith-protocol-client/knowledge/tests
  :description             "Knowledge client and normal resource tool boundary checks."
  :depends-on              (#:autolith-protocol-client/knowledge/resources
                            #:autolith-protocol-client/records/resources/tests
                            #:autolith-supervisor/knowledge-service)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-knowledge-tests")
                            (:file "src/agent/protocol-knowledge-plan-tests"))
  :perform                 (asdf:test-op (operation component)
                            (declare (ignore operation component))
                            (uiop:symbol-call '#:autolith '#:run-agent-protocol-knowledge-tests)
                            (uiop:symbol-call '#:autolith '#:run-agent-protocol-plan-tests)))
