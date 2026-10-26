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
  :description             "Parent-owned model profiles and semantic request projection."
  :depends-on              (#:autolith-supervisor/protocol #:cl-llm-provider-api/wire #:cl-base64)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/model-profile")
                            (:file "supervisor/model-projection"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-supervisor/model/tests))))

(asdf:defsystem #:autolith-supervisor/model/tests
  :description             "Model authority and family projection checks without agent code."
  :depends-on              (#:autolith-supervisor/model)
  :default-component-class autolith-supervisor-source-file
  :serial                  t
  :components              ((:file "supervisor/model-profile-tests")
                            (:file "supervisor/model-projection-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-profile-tests)
                             (uiop:symbol-call '#:autolith-supervisor '#:run-model-projection-tests)))
