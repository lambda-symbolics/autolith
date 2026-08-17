(asdf:defsystem #:autolith
  :description "A live, self-modifying Common Lisp agent."
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.33.8"
  :serial t
  :depends-on (#:cl-base64
               #:cl+ssl
               #:cl-colorist
               #:cl-exec-sandbox
               #:cl-jobpond
               #:cl-termdown
               #:clifff
               #:clinedi
               #:clingon
               #:colorlisp
               #:colordiff
               #:closer-mop
               #:dexador
               #:flexi-streams
               #:idsmall
               #:ironclad/digest/sha256
               #:ironclad/mac/siphash
               #:bordeaux-threads
               #:mcparen
               #:opticl
               #:org-templater
               #:parenchek
               #:quri
               #:serapeum
               #:sb-posix
               #:sb-bsd-sockets
               #:sbcl-generations
               #:sbcl-workers
               #:sexp-config
               #:sexp-store
               #:structlisp
               #:usocket
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "core/package")
                             (:file "core/types")
                             (:file "core/conditions")
                             (:file "localgroup/protocol")
                             (:file "core/json")
                             (:file "core/time")
                             (:file "core/source-files")
                             (:file "core/streams")
                             (:file "configuration/settings")
                             (:file "provider/registry")
                             (:file "configuration/workspace")
                             (:file "conversation/image-input")
                             (:file "state/records")
                             (:file "state/updates")
                             (:file "configuration/preferences")
                             (:file "configuration/permissions")
                             (:file "state/later")
                             (:file "provider/authentication")
                             (:file "provider/grok/authentication")
                             (:file "provider/api-key")
                             (:file "provider/fireworks/authentication")
                             (:file "workers/images")
                             (:file "provider/device-authentication")
                             (:file "provider/grok/device-authentication")
                             (:file "conversation/identifiers")
                             (:file "conversation/identifier-migration")
                             (:file "conversation/store")
                             (:file "state/memories")
                             (:file "state/papercuts")
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
                             (:file "provider/wire-protocol")
                             (:file "provider/grok/client")
                             (:file "provider/openai-compatible")
                             (:file "provider/fireworks/client")
                             (:file "provider/anthropic/authentication")
                             (:file "provider/anthropic/client")
                             (:file "provider/opencode/authentication")
                             (:file "provider/opencode/client")
                             (:file "provider/builtins")
                             (:file "resource/protocol")
                             (:file "tools/registry")
                             (:file "skills/tools")
                             (:file "mcp/tools")
                             (:file "tools/papercut")
                             (:file "tools/agenda")
                             (:file "tools/plan")
                             (:file "tools/workspace")
                             (:file "tools/lisp-paren-check")
                             (:file "resource/workspace-file")
                             (:file "resource/agenda")
                             (:file "resource/memory")
                             (:file "tools/search")
                             (:file "workers/search")
                             (:file "workers/lisp")
                             (:file "workers/scratchpad")
                             (:file "self/tools")
                             (:file "state/durable-mutations")
                             (:file "state/image-commits")
                             (:file "startup/user-init")
                             (:file "state/generations")
                             (:file "self/status")
                             (:file "self/discard")
                             (:file "self/exercise")
                             (:file "tools/web")
                             (:file "tools/defaults")
                             (:file "agent/runtime")
                             (:file "task/contracts")
                             (:file "inference/budget")
                             (:file "inference/view")
                             (:file "inference/frame")
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
                             (:file "terminal/stream")
                             (:file "localgroup/terminal")
                             (:file "terminal/ui")
                             (:file "application/runtime")
                             (:file "application/change-viewer")
                             (:file "application/tool-presentation")
                             (:file "application/change-presentation")
                             (:file "application/recovery")
                             (:file "application/commands")
                             (:file "application/operation")
                             (:file "application/user-operation-context")
                             (:file "application/lisp-machine")
                             (:file "terminal/responsive-input")
                             (:file "application/recovery-input-vault")
                             (:file "localgroup/runtime")
                             (:file "localgroup/handoff")
                             (:file "localgroup/checkpoint")
                             (:file "localgroup/client")
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
                             (:file "stream-tests")
                             (:file "memory-tests")
                             (:file "papercut-tests")
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
                             (:file "openai-compatible-provider-tests")
                             (:file "anthropic-provider-tests")
                             (:file "fireworks-provider-tests")
                             (:file "opencode-provider-tests")
                             (:file "resource-tests")
                             (:file "workspace-resource-tests")
                             (:file "agenda-resource-tests")
                             (:file "memory-resource-tests")
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
                             (:file "inference-tests")
                             (:file "task-test-support")
                             (:file "task-agent-tests")
                             (:file "task-execution-tests")
                             (:file "task-scheduler-tests")
                             (:file "terminal-tests")
                             (:file "localgroup-tests")
                             (:file "localgroup-handoff-tests")
                             (:file "localgroup-handoff-boundary-tests")
                             (:file "layout-tests")
                             (:file "release-script-tests")
                             (:file "release-server-tests")
                             (:file "application-tests")
                             (:file "lisp-machine-tests")
                             (:file "user-operation-context-tests")
                             (:file "application-operation-tests")
                             (:file "recovery-input-vault-tests")
                             (:file "user-init-tests")
                             (:file "prompt-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:autolith '#:run-tests)))
