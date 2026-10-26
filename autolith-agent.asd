;;;; -- Autolith Systems --

;;; Compiling a file proclaims its -> declarations while its classes are still
;;; placeholders, so loading the file into the same image proclaims them again
;;; and SBCL reports that each declaration over a class from that file differs
;;; from itself. Loading Autolith's own files muffles that report. A genuine
;;; disagreement needs two declarations of one function, which the source
;;; checks refuse, and still warns when it is compiled.

(defclass autolith-source-file (asdf:cl-source-file)
  ()
  (:documentation "An Autolith source file, loaded without SBCL's self-redeclaration reports."))

(defmethod asdf:perform :around ((operation asdf:load-op) (component autolith-source-file))
  "Load COMPONENT, muffling the report that its -> declarations differ from themselves."
  (handler-bind (#+sbcl
                 (sb-kernel::ftype-proclamation-mismatch-warning #'muffle-warning))
    (call-next-method)))

(asdf:defsystem #:autolith-agent
  :description             "A live, self-modifying Common Lisp agent."
  :author                  "Lukáš Hozda"
  :license                 "ISC"
  :version                 (:read-file-line "VERSION")
  :default-component-class autolith-source-file
  :serial                  t
  :depends-on              (#:agentcomms
                            #:argo
                            #:cl-base64
                            #:cl+ssl
                            #:cl-colorist
                            #:cl-exec-sandbox
                            #:cl-hashline
                            #:cl-jobpond
                            #:cl-jobpond/schedules
                            #:cl-jobpond/mailboxes
                            #:cl-jobpond/completions
                            #:cl-lsp
                            #:cl-resources
                            #:cl-rfc8252
                            #:cl-rfc8628
                            #:cl-worktree
                            #:clinker-transcript
                            #:cl-llm-provider-api
                            #:cl-llm-provider-api/dexador
                            #:cl-llm-provider-api/context
                            #:cl-llm-provider-api/registry
                            #:cl-llm-provider-api/contracts
                            #:cl-skills
                            #:cl-skills/executable
                            #:cl-termdown
                            #:clifff
                            #:clinedi
                            (:feature (:not :win32) #:clinedi/posix)
                            (:feature :win32 #:clinedi/win32)
                            #:ls-compat/files
                            #:ls-compat/posix
                            #:clingon
                            #:colorlisp
                            #:colordiff
                            #:closer-mop
                            #:dexador
                            #:defingerprinter
                            #:fetch-gist
                            #:flexi-streams
                            #:idsmall
                            #:image-daemon/runtime
                            #:image-daemon/eval
                            #:image-daemon/messages
                            #:ironclad/mac/siphash
                            #:lambda-debugger
                            #:ls-flock
                            #:bordeaux-threads
                            #:mcparen/managed
                            #:org-templater
                            #:parenchek
                            #:quri
                            #:serapeum
                            #:setinka
                            #:sb-posix
                            #:sb-bsd-sockets
                            #:sbcl-generations
                            #:sbcl-workers
                            #:sbcl-workers/host-callbacks
                            #:surgeon
                            #:yolokuva
                            #:yolokuva/opticl
                            #:sexp-config
                            #:sexp-store
                            #:sophisticated-clipboard
                            #:structlisp
                            #:trivial-gray-streams
                            #:usocket)
  :components              ((:static-file "VERSION")
                            (:static-file "docs/onboarding-sources.org")
                            (:module     "src"
                             :serial     t
                             :components ((:file "core/package")
                                          (:file "core/types")
                                          (:file "core/conditions")
                                          (:file "core/lists")
                                          (:file "core/layered-registry")
                                          (:file "core/version")
                                          (:file "localgroup/protocol")
                                          (:file "core/bounded-text")
                                          (:file "core/time")
                                          (:file "core/platform")
                                          (:file "core/platform-posix" :if-feature (:not :win32))
                                          (:file "core/platform-win32" :if-feature :win32)
                                          (:file "core/supervised-process")
                                          (:file "core/source-files")
                                          (:file "core/streams")
                                          (:file "core/text-buffer")
                                          (:file "configuration/settings")
                                          (:file "configuration/configuration")
                                          (:file "telemetry/configuration")
                                          (:file "provider/registry")
                                          (:file "configuration/workspace")
                                          (:file "conversation/image-input")
                                          (:file "state/records")
                                          (:file "state/updates")
                                          (:file "configuration/preferences")
                                          (:file "configuration/permissions")
                                          (:file "provider/authentication")
                                          (:file "provider/chatgpt/authentication")
                                          (:file "provider/gemini/authentication")
                                          (:file "provider/grok/authentication")
                                          (:file "provider/api-key")
                                          (:file "provider/api-key-providers")
                                          (:file "provider/static-api-key-providers")
                                          (:file "workers/images")
                                          (:file "provider/device-authentication")
                                          (:file "provider/grok/device-authentication")
                                          (:file "provider/nous/authentication")
                                          (:file "provider/nous/device-authentication")
                                          (:file "provider/copilot/authentication")
                                          (:file "models/catalog")
                                          (:file "models/provider-catalog")
                                          (:file "telemetry/runtime")
                                          (:file "telemetry/exporter")
                                          (:file "telemetry/uploads")
                                          (:file "telemetry/hook-protocol")
                                          (:file "conversation/identifiers")
                                          (:file "conversation/identifier-migration")
                                          (:file "conversation/store")
                                          (:file "conversation/input-history")
                                          (:file "conversation/compaction")
                                          (:file "conversation/async-lisp")
                                          (:file "state/memory-sources")
                                          (:file "state/memories")
                                          (:file "knowledge/terms")
                                          (:file "knowledge/schemas")
                                          (:file "knowledge/store")
                                          (:file "knowledge/applicability")
                                          (:file "knowledge/rules")
                                          (:file "knowledge/evaluation")
                                          (:file "knowledge/explanation")
                                          (:file "knowledge/viewers")
                                          (:file "knowledge/methods")
                                          (:file "knowledge/contracts")
                                          (:file "knowledge/diagnosis")
                                          (:file "state/memory-status")
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
                                          (:file "agent/knowledge-usage")
                                          (:file "agent/plan-context")
                                          (:file "learning/traces")
                                          (:file "learning/procedures")
                                          (:file "learning/mining")
                                          (:file "learning/preflight")
                                          (:file "learning/running")
                                          (:file "agent/procedure-context")
                                          (:file "agent/interpreter-discipline")
                                          (:file "agent/resume-context")
                                          (:file "self/review")
                                          (:file "provider/client")
                                          (:file "provider/usage-export")
                                          (:file "provider/wire-protocol")
                                          (:file "provider/grok/client")
                                          (:file "provider/openai-compatible")
                                          (:file "provider/openai-api")
                                          (:file "provider/gemini-code-assist")
                                          (:file "provider/fireworks/client")
                                          (:file "provider/anthropic/client")
                                          (:file "provider/nous/client")
                                          (:file "provider/opencode/client")
                                          (:file "provider/openrouter/client")
                                          (:file "provider/mistral/client")
                                          (:file "provider/copilot/client")
                                          (:file "provider/copilot/auto")
                                          (:file "provider/builtins")
                                          (:file "resource/protocol")
                                          (:file "resource/item-identity")
                                          (:file "inference/object")
                                          (:file "tools/registry")
                                          (:file "tools/action-contracts")
                                          (:file "skills/tools")
                                          (:file "mcp/tools")
                                          (:file "tools/papercut")
                                          (:file "tools/agenda")
                                          (:file "tools/plan")
                                          (:file "tools/procedure")
                                          (:file "tools/diagnosis")
                                          (:file "tools/workspace")
                                          (:file "tools/lisp-paren-check")
                                          (:file "resource/workspace-file")
                                          (:file "resource/workspace-changes")
                                          (:file "lsp/configuration")
                                          (:file "lsp/tools")
                                          (:file "lsp/semantic-proposals")
                                          (:file "lsp/semantic-tools")
                                          (:file "emacs/server")
                                          (:file "emacs/resource")
                                          (:file "emacs/tools")
                                          (:file "resource/agenda")
                                          (:file "resource/memory")
                                          (:file "resource/memory-knowledge")
                                          (:file "resource/papercut")
                                          (:file "tools/search")
                                          (:file "workers/search")
                                          (:file "workers/lisp")
                                          (:file "workers/host-calls")
                                          (:file "workers/scratchpad")
                                          (:file "skills/executable")
                                          (:file "self/tools")
                                          (:file "self/apropos")
                                          (:file "state/durable-mutations")
                                          (:file "state/image-commits")
                                          (:file "self/tuning-experiments")
                                          (:file "startup/user-init")
                                          (:file "state/generations")
                                          (:file "self/status")
                                          (:file "self/discard")
                                          (:file "self/exercise")
                                          (:file "tools/web")
                                          (:file "tools/web-gist")
                                          (:file "tools/defaults")
                                          (:file "agent/runtime")
                                          (:file "task/contracts")
                                          (:file "task/engineering-artifacts")
                                          (:file "inference/tools")
                                          (:file "inference/frame")
                                          (:file "inference/map")
                                          (:file "inference/policy")
                                          (:file "inference/resource")
                                          (:file "inference/permission")
                                          (:file "inference/endpoint")
                                          (:file "inference/environment")
                                          (:file "inference/distill")
                                          (:file "task/agents")
                                          (:file "task/state")
                                          (:file "task/runtime")
                                          (:file "task/child")
                                          (:file "task/notes")
                                          (:file "task/scheduler")
                                          (:file "task/tools")
                                          (:file "task/continuity")
                                          (:file "task/shell-logs")
                                          (:file "tools/shell-output")
                                          (:file "resource/shell-log")
                                          (:file "task/completions")
                                          (:file "task/unfinished-work")
                                          (:file "task/continuity-tools")
                                          (:file "task/worktrees")
                                          (:file "agent/context-rules")
                                          (:file "agent/saved-context-rules")
                                          (:file "terminal/protocol")
                                          (:file "terminal/input")
                                          (:file "terminal/style")
                                          (:file "terminal/syntax-highlighting")
                                          (:file "terminal/layout")
                                          (:file "terminal/stream")
                                          (:file "localgroup/terminal")
                                          (:file "terminal/path-completion")
                                          (:file "terminal/ui")
                                          (:file "terminal/clipboard")
                                          (:file "terminal/fullscreen")
                                          (:file "terminal/boot-screen")
                                          (:file "state/onboarding")
                                          (:file "application/onboarding-source")
                                          (:file "terminal/onboarding")
                                          (:file "application/prompt-cache")
                                          (:file "application/runtime")
                                          (:file "application/mission")
                                          (:file "application/mission-inference")
                                          (:file "application/mission-gates")
                                          (:file "application/mission-knowledge")
                                          (:file "application/mission-tools")
                                          (:file "application/mission-review")
                                          (:file "application/mission-review-tools")
                                          (:file "application/clipboard")
                                          (:file "conversation/replay")
                                          (:file "conversation/search")
                                          (:file "resource/conversation")
                                          (:file "management/repl")
                                          (:file "slynk/endpoint")
                                          (:file "application/change-viewer")
                                          (:file "application/tool-presentation")
                                          (:file "application/change-presentation")
                                          (:file "application/recovery")
                                          (:file "application/task-inspector")
                                          (:file "application/commands")
                                          (:file "application/memory-commands")
                                          (:file "application/knowledge-commands")
                                          (:file "application/memory-manager-model")
                                          (:file "application/memory-manager")
                                          (:file "application/procedure-commands")
                                          (:file "application/diagnosis-commands")
                                          (:file "application/relax")
                                          (:file "application/complaints")
                                          (:file "application/operation")
                                          (:file "application/mission-schedules")
                                          (:file "application/mission-schedule-tools")
                                          (:file "task/peer-messages")
                                          (:file "task/peer-message-tools")
                                          (:file "self/refinement")
                                          (:file "self/refinement-tools")
                                          (:file "application/help")
                                          (:file "application/user-operation-context")
                                          (:file "application/debugger")
                                          (:file "application/lisp-values")
                                          (:file "application/lisp-machine")
                                          (:file "application/async-lisp-output")
                                          (:file "application/async-lisp")
                                          (:file "terminal/responsive-input")
                                          (:file "application/idle-compaction")
                                          (:file "application/job-completions")
                                          (:file "task/job-events")
                                          (:file "task/job-event-observer")
                                          (:file "task/job-boundary")
                                          (:file "application/recovery-input-vault")
                                          (:file "application/vault-edit")
                                          (:file "localgroup/runtime")
                                          (:file "localgroup/handoff")
                                          (:file "localgroup/checkpoint")
                                          (:file "localgroup/client")
                                          (:file "application/onboarding")
                                          (:file "state/data-transfer")
                                          (:file "state/data-transfer-collect")
                                          (:file "state/data-transfer-validate")
                                          (:file "state/data-transfer-import")
                                          (:file "state/data-transfer-knowledge")
                                          (:file "application/data-transfer")
                                          (:file "startup/data-transfer")
                                          (:file "acp/session")
                                          (:file "acp/observer")
                                          (:file "acp/mcp")
                                          (:file "acp/runtime")
                                          (:file "acp/extensions")
                                          (:file "acp/input")
                                          (:file "acp/extension-settings")
                                          (:file "acp/extension-catalogs")
                                          (:file "acp/extension-conversations")
                                          (:file "acp/extension-jobs")
                                          (:file "acp/extension-resources")
                                          (:file "acp/completions")
                                          (:file "telemetry/hooks")
                                          (:file "startup/main")
                                          (:file "startup/active-image"))))
  :in-order-to             ((asdf:test-op (asdf:test-op #:autolith/tests))))
