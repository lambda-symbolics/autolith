;;;; -- Standalone Supervisor System --

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
