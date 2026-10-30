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
                            (:file "src/agent/protocol-observer"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/tests))))

(asdf:defsystem #:autolith-protocol-client/tests
  :description             "Serialized parent and agent protocol boundary checks."
  :depends-on              (#:autolith-protocol-client #:autolith-supervisor/protocol/tests
                            #:autolith-supervisor/model)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-client-tests")
                            (:file "src/agent/protocol-model-client-tests")
                            (:file "src/agent/protocol-model-runtime-tests")
                            (:file "src/agent/protocol-context-tests")
                            (:file "src/agent/protocol-driver-tests")
                            (:file "src/agent/protocol-provider-tests")
                            (:file "src/agent/protocol-session-tests")
                            (:file "src/agent/protocol-observer-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-model-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-model-runtime-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-context-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-driver-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-provider-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-session-tests)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-observer-tests)))

(asdf:defsystem #:autolith-protocol-client/records
  :description             "Typed parent-owned record client and agenda adapter."
  :depends-on              (#:autolith-protocol-client)
  :default-component-class autolith-source-file
  :serial                  t
  :components              ((:file "src/agent/protocol-record-client")
                            (:file "src/agent/protocol-agenda"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/records/tests))))

(asdf:defsystem #:autolith-protocol-client/records/tests
  :description             "Serialized record client boundary checks."
  :depends-on              (#:autolith-protocol-client/records
                            #:autolith-protocol-client/tests)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-record-client-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-record-tests)))
