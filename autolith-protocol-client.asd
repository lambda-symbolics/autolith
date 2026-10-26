;;;; -- Optional Agent Protocol Client --

(let ((definition (merge-pathnames "autolith-agent.asd" *load-truename*)))
  (unless (find-class 'autolith-source-file nil)
    (asdf:load-asd definition)))

(asdf:defsystem #:autolith-protocol-client
  :description             "Explicit-stream agent client for the parent session protocol."
  :version                 (:read-file-line "VERSION")
  :license                 "ISC"
  :depends-on              (#:autolith-agent #:autolith-supervisor/protocol)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-client"))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith-protocol-client/tests))))

(asdf:defsystem #:autolith-protocol-client/tests
  :description             "Serialized parent and agent protocol boundary checks."
  :depends-on              (#:autolith-protocol-client #:autolith-supervisor/protocol/tests
                            #:autolith-supervisor/session)
  :default-component-class autolith-source-file
  :components              ((:file "src/agent/protocol-client-tests"))
  :perform                 (asdf:test-op (operation component)
                             (uiop:symbol-call '#:autolith '#:run-agent-protocol-tests)))
