(asdf:defsystem #:autolith
  :description "A live, self-modifying Common Lisp agent."
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.21.4"
  :serial t
  :depends-on (#:cl-base64
               #:cl+ssl
               #:cl-colorist
               #:cl-exec-sandbox
               #:clifff
               #:clinedi
               #:colorlisp
               #:closer-mop
               #:dexador
               #:flexi-streams
               #:ironclad/mac/siphash
               #:bordeaux-threads
               #:mcparen
               #:opticl
               #:quri
               #:serapeum
               #:sb-posix
               #:sbcl-workers
               #:sexp-store
               #:usocket
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "core/package")
                             (:file "core/types")
                             (:file "core/conditions")
                             (:file "core/json")
                             (:file "core/time")
                             (:file "core/source-files")
                             (:file "core/streams")
                             (:file "configuration/settings")
                             (:file "configuration/workspace")
                             (:file "conversation/image-input")
                             (:file "state/records")
                             (:file "state/updates")
                             (:file "configuration/preferences")
                             (:file "configuration/permissions")
                             (:file "state/later")
                             (:file "provider/authentication")
                             (:file "provider/grok/authentication")
                             (:file "workers/images")
                             (:file "provider/device-authentication")
                             (:file "provider/grok/device-authentication")
                             (:file "conversation/identifiers")
                             (:file "conversation/identifier-migration")
                             (:file "conversation/store")
                             (:file "state/memories")
                             (:file "state/agendas")
                             (:file "state/plan")
                             (:file "agent/prompt")
                             (:file "agent/context")
                             (:file "mcp/configuration")
                             (:file "configuration/directory")
                             (:file "skills/runtime")
                             (:file "application/command")
                             (:file "configuration/project-adaptations")
                             (:file "agent/memory-context")
                             (:file "agent/interpreter-discipline")
                             (:file "self/review")
                             (:file "provider/client")
                             (:file "provider/grok/client")
                             (:file "tools/registry")
                             (:file "skills/tools")
                             (:file "mcp/tools")
                             (:file "tools/memory")
                             (:file "tools/agenda")
                             (:file "tools/plan")
                             (:file "tools/seatbelt")
                             (:file "tools/workspace-edit")
                             (:file "tools/workspace")
                             (:file "tools/search")
                             (:file "workers/search")
                             (:file "workers/lisp")
                             (:file "workers/scratchpad")
                             (:file "self/tools")
                             (:file "state/overlays")
                             (:file "state/durable-mutations")
                             (:file "state/image-commits")
                             (:file "startup/user-init")
                             (:file "state/generations")
                             (:file "self/status")
                             (:file "self/discard")
                             (:file "self/exercise")
                             (:file "tools/defaults")
                             (:file "agent/runtime")
                             (:file "task/contracts")
                             (:file "task/agents")
                             (:file "task/state")
                             (:file "task/runtime")
                             (:file "task/child")
                             (:file "task/scheduler")
                             (:file "task/tools")
                             (:file "terminal/protocol")
                             (:file "terminal/input")
                             (:file "terminal/style")
                             (:file "terminal/syntax-highlighting")
                             (:file "terminal/layout")
                             (:file "terminal/markdown")
                             (:file "terminal/stream")
                             (:file "terminal/ui")
                             (:file "application/runtime")
                             (:file "application/tool-presentation")
                             (:file "application/recovery")
                             (:file "application/commands")
                             (:file "terminal/responsive-input")
                             (:file "startup/main")
                             (:file "startup/active-image"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:autolith/tests))))

(asdf:defsystem #:autolith/release-server
  :description "The Autolith installer and binary release service."
  :depends-on (#:autolith
               #:sb-bsd-sockets)
  :serial t
  :components ((:module "server"
                :serial t
                :components ((:file "release-server")
                             (:file "release-builder")
                             (:file "release-updater")
                             (:file "release-archive")
                             (:file "release-main")))))

(asdf:defsystem #:autolith/tests
  :description "Tests for Autolith."
  :depends-on (#:autolith
               #:autolith/release-server)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "memory-tests")
                             (:file "update-tests")
                             (:file "agenda-tests")
                             (:file "preferences-tests")
                             (:file "permissions-tests")
                             (:file "later-tests")
                             (:file "context-tests")
                             (:file "interpreter-discipline-tests")
                             (:file "self-review-tests")
                             (:file "skill-tests")
                             (:file "skill-tool-tests")
                             (:file "mcp-configuration-tests")
                              (:file "directory-configuration-tests")
                             (:file "mcp-tool-tests")
                             (:file "application-command-tests")
                             (:file "project-adaptation-tests")
                             (:file "conversation-identifier-tests")
                             (:file "conversation-tests")
                             (:file "plan-tests")
                             (:file "authentication-tests")
                             (:file "grok-authentication-tests")
                             (:file "provider-tests")
                             (:file "grok-provider-tests")
                             (:file "tool-tests")
                             (:file "search-tool-tests")
                             (:file "generation-tests")
                             (:file "active-image-tests")
                             (:file "recovery-tests")
                             (:file "lisp-worker-tests")
                             (:file "self-tool-tests")
                             (:file "device-authentication-tests")
                             (:file "grok-device-authentication-tests")
                             (:file "agent-tests")
                             (:file "task-test-support")
                             (:file "task-agent-tests")
                             (:file "task-execution-tests")
                             (:file "task-scheduler-tests")
                             (:file "terminal-tests")
                             (:file "layout-tests")
                             (:file "markdown-tests")
                             (:file "release-script-tests")
                             (:file "release-server-tests")
                             (:file "application-tests")
                             (:file "user-init-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:autolith '#:run-tests)))
