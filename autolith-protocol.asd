(asdf:defsystem #:autolith-protocol
  :description "Generated private parent-agent protocol bindings."
  :license     "MIT"
  :depends-on  (#:cl-protobufs)
  :serial      t
  :components  ((:file "proto/generated/value")
                (:file "proto/generated/protocol")))
