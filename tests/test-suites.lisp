(in-package #:autolith)

;;;; -- Test Catalog --

(setf *test-suites* nil)

(define-test-suite core
  test-core-defaults
  test-xdg-directory-selection
  test-context-window-environment
  test-model-environment-validation
  test-text-line-splitting
  test-configuration-source-platform-reading)

(define-test-suite stream
  test-bounded-character-reads)

(define-test-suite memory
  test-memory-persistence)

(define-test-suite papercut
  test-papercuts)

(define-test-suite update
  test-update-state-and-installation-provenance)

(define-test-suite user-init
  test-user-init
  test-local-source-tree-registration
  test-directory-user-init)

(define-test-suite agenda
  test-agenda-persistence-and-transport
  test-agenda-unbounded-item-count
  test-agenda-command
  test-agenda-version-one-migration
  test-agenda-malformed-state
  test-agenda-tools)

(define-test-suite preferences
  test-preferences)

(define-test-suite prompt-cache
  test-prompt-cache-miss-detection
  test-prompt-cache-baseline-from-conversation
  test-prompt-cache-miss-notices
  test-cache-misses-command)

(define-test-suite permissions
  test-command-permission-persistence
  test-command-permission-corruption)

(define-test-suite context
  test-session-state-context-contributor
  test-request-local-context)

(define-test-suite interpreter-discipline
  test-interpreter-discipline)

(define-test-suite self-review
  test-self-review-reminder)

(define-test-suite skill
  skill-tests--roots-and-rendering
  skill-tests--ephemeral-selection
  skill-tests--concurrent-parent-child-selection
  skill-tests--selection-failures-and-limits)

(define-test-suite skill-tool
  test-skill-load-tool
  test-skill-load-presentation)

(define-test-suite mcp-configuration
  test-mcp-configuration)

(define-test-suite directory-configuration
  test-directory-configuration)

(define-test-suite mcp-tool
  test-mcp-tools
  test-mcp-reload-registry-rollback
  test-mcp-reload-registry-isolation
  test-mcp-reload-transaction-boundary)

(define-test-suite application-command
  test-application-command-defining-form
  test-application-command-semantic-calls
  test-application-command-registry
  test-application-command-policies
  test-built-in-application-command-policies
  test-built-in-application-command-calls
  test-application-codex-fast-mode-command
  test-application-mcp-reload-capability-change
  test-terminal-authentication-streams
  test-application-authentication-command)

(define-test-suite project-adaptation
  test-project-adaptations)

(define-test-suite conversation-identifier
  test-conversation-identifier-format
  test-conversation-identifier-allocation
  test-conversation-identifier-migration
  test-conversation-identifier-migration-validation
  test-conversation-identifier-migration-resumption)

(define-test-suite conversation
  test-conversation-image-input
  test-conversation-inherited-reference
  test-conversation-ephemeral-tool-projection
  test-conversation-ephemeral-append-interruption
  test-conversation-malformed-tool-projections
  test-conversation-concurrent-appends
  test-conversation-child-project-setup
  test-conversation-process-lease
  test-conversation-interrupted-tool-call
  test-conversation-late-duplicate-tool-output
  test-conversation-persistence
  test-conversation-private-storage
  test-conversation-origin-directory
  test-conversation-model-selection
  test-conversation-titles
  test-conversation-cross-family-reasoning
  test-conversation-compaction
  test-conversation-native-compaction
  test-conversation-chunk-storage
  test-conversation-segment-validation
  test-conversation-legacy-storage
  test-conversation-working-seconds
  test-conversation-picker-metadata-stability
  test-conversation-picker-rebuild-exclusion
  test-conversation-picker-search
  test-conversation-turn-aborted-boundary
  test-conversation-tail-repair-interruptibility
  test-conversation-initial-publication-serialization
  test-conversation-deletion)

(define-test-suite conversation-replay
  test-conversation-replay-navigation
  test-conversation-replay-projection
  test-conversation-replay-storage
  test-conversation-replay-command-line
  test-conversation-fork-storage
  test-conversation-fork-rejections
  test-conversation-fork-command-line)

(define-test-suite plan
  test-workspace-plan)

(define-test-suite authentication
  test-authentication-store
  test-authentication-bootstrap-and-refresh)

(define-test-suite chatgpt-authentication
  run-chatgpt-authentication-tests)

(define-test-suite gemini-authentication
  run-gemini-authentication-tests)

(define-test-suite grok-authentication
  test-grok-authentication)

(define-test-suite nous-authentication
  nous-authentication-test--manager-loading
  nous-authentication-test--refresh-validation
  nous-authentication-test--serialized-refresh
  nous-authentication-test--refresh-redaction)


(define-test-suite provider
  test-provider-deferred-tool-loading
  test-provider-request
  test-provider-request-tool-filtering
  test-provider-native-compaction
  test-provider-rate-limits
  test-provider-transport-boundary
  test-provider-credential-echo-containment
  test-provider-authentication-retries
  test-provider-persistent-transient-retries
  test-provider-stream-inactivity-deadline
  test-provider-response-deadlines
  test-provider-stream-retries)

(define-test-suite grok-provider
  grok-provider-test--selection
  grok-provider-test--item-normalization
  grok-provider-test--doom-loop-recovery
  grok-provider-test--request-shape
  grok-provider-test--transport-headers)

(define-test-suite openai-compatible-provider
  test-openai-compatible-provider-bootstrap
  test-openai-compatible-provider-deferred-main-validation
  test-openai-compatible-provider-bare-auth-selection
  test-openai-compatible-tool-name-recovery
  test-openai-compatible-provider-discovery-is-on-demand
  test-openai-compatible-provider-model-cache-boundary
  test-openai-compatible-provider-authentication-bootstrap
  test-openai-compatible-provider-discovery
  test-openai-compatible-provider-registration-identity
  test-provider-sse-bounds
  test-openai-compatible-provider)

(define-test-suite gemini-code-assist-provider
  gemini-code-assist-test--builtin-registration
  gemini-code-assist-test--model-catalog
  gemini-code-assist-test--request-conversion
  gemini-code-assist-test--stream-fixture
  gemini-code-assist-test--terminal-semantics
  gemini-code-assist-test--credential-redaction
  gemini-code-assist-test--setup-and-retry)

(define-test-suite anthropic-provider
  anthropic-provider-test--selection
  anthropic-provider-test--credential-source
  anthropic-provider-test--ephemeral-cache-boundary
  anthropic-provider-test--inherited-reference-order
  anthropic-provider-test--compaction-request
  anthropic-provider-test--transport)

(define-test-suite nous-provider
  nous-provider-test--registration-and-discovery
  nous-provider-test--transport)

(define-test-suite fireworks-provider
  fireworks-provider-test--selection
  fireworks-provider-test--credential-source
  fireworks-provider-test--request-shape
  fireworks-provider-test--reasoning-omission
  fireworks-provider-test--inherited-terminal-routing)

(define-test-suite opencode-provider
  opencode-provider-test--selection
  opencode-provider-test--credential-source
  opencode-provider-test--login
  opencode-provider-test--authentication-bootstrap
  opencode-provider-test--request-model
  opencode-provider-test--session-header
  opencode-provider-test--discovery
  opencode-provider-test--legacy-registry-snapshot
  opencode-provider-test--builtin-registration)

(define-test-suite openrouter-provider
  test-openrouter-provider)

(define-test-suite mistral-provider
  test-mistral-provider)

(define-test-suite resource
  test-resource-protocol
  test-resource-edit-operation-schema)

(define-test-suite workspace-resource
  test-workspace-file-resources)

(define-test-suite agenda-resource
  test-agenda-resources)

(define-test-suite memory-resource
  test-memory-resources
  test-memory-resource-mutations)

(define-test-suite papercut-resource
  test-papercut-resources)

(define-test-suite tool
  test-tool-registry
  test-tool-result-overflow
  test-workspace-tools)

(define-test-suite search-tool
  test-search-tools)

(define-test-suite web-tool
  test-web-gist-tool
  test-web-gist-retrieval)

(define-test-suite lisp-worker
  test-lisp-image-manifests
  test-lisp-worker-protocol
  test-lisp-execution-jobs
  test-lisp-busy-worker-operations
  test-lisp-scratchpad-tools
  test-lisp-worker-image-snapshot)

(define-test-suite prompt
  test-system-prompt
  test-request-context-agenda-selection)

(define-test-suite self-tool
  test-self-definition-reader-boundary
  test-self-replay-foreign-home
  test-self-foreign-definition-lifecycle
  test-mutation-journal-tail-repair
  test-self-tools
  test-self-definition-installation-rollback
  test-self-application-command-definitions
  test-self-restart-selection
  test-self-discard
  test-self-tuning-experiments
  test-standard-mutation-checker
  test-durable-self-mutation
  test-durable-definition-publication-boundary)

(define-test-suite generation
  test-generation-manifest
  test-crash-capsule-correlation)

(define-test-suite management-repl
  test-management-repl-configuration
  test-management-repl-protocol
  test-management-repl-debugger-hook
  test-management-repl-adversarial-protocol
  test-management-repl-client-bounds
  test-management-repl-unix-lifecycle
  test-management-repl-start-failure-atomic
  test-management-repl-tcp-lifecycle)

(define-test-suite active-image
  test-active-image-build-record
  test-image-commit-surface-battery
  test-image-commit-replay-probe)

(define-test-suite recovery
  test-recovery-xdg-directories
  test-recovery-conversation-identifiers
  test-recovery-session-handoff
  test-recovery-status-boundary
  test-recovery-source-failure-reporting
  test-recovery-generation-revision-boundary)

(define-test-suite device-authentication
  device-authentication-test--complete-flow
  device-authentication-test--injected-poll
  device-authentication-test--timeout
  device-authentication-test--error-echo-containment
  device-authentication-test--declined
  device-authentication-test--missing-account)

(define-test-suite nous-device-authentication
  nous-device-test--complete-flow
  nous-device-test--rejections
  nous-device-test--secret-redaction
  nous-device-test--request-code-validation)

(define-test-suite agent
  test-agent-portable-value
  test-agent-tool-loop
  test-agent-shell-authorization-unavailable
  test-agent-tool-free-turn
  test-agent-read-only-tool-allowlist
  test-agent-restricted-resource-schemes
  test-agent-restricted-tool-round-limit
  test-agent-empty-tool-allowlist
  test-agent-steering
  test-agent-explicit-continuation
  test-agent-provider-request-limit
  test-agent-invalid-call-history
  test-agent-malformed-tool-arguments
  test-agent-tool-storm-guard
  test-agent-tool-retry-guidance
  test-agent-tool-failures
  test-agent-provider-failure-persistence
  test-agent-incomplete-provider-failure-persistence
  test-agent-provider-credential-failure-containment
  test-agent-long-tool-turn
  test-agent-unbounded-tool-calls
  test-agent-default-turn-has-no-step-guillotine
  test-agent-skill-provider-barrier
  test-agent-compaction-missing-summary
  test-agent-compaction
  test-agent-native-compaction
  test-agent-parallel-tool-wave
  test-agent-tool-concurrency-key
  test-agent-exclusive-tool-waves
  test-agent-parallel-tool-failure
  test-agent-parallel-fatal-propagation)

(define-test-suite task-agent
  test-task-agent-native-reader
  test-task-agent-discovery-precedence
  test-task-agents-tool
  test-task-tool-default-argument-types
  test-task-native-output-contracts
  test-task-yield-contract
  test-task-child-steering-mailbox
  test-task-child-messaging)

(define-test-suite task-execution
  test-task-abort-control-condition
  test-task-orchestration
  test-task-child-cancels-lisp-executions
  test-task-child-shared-agent-loop)

(define-test-suite inference
  test-rlm-frame-budget-activity
  test-rlm-context-designators
  test-rlm-value-preview
  test-rlm-budget-cache-discount
  test-rlm-response-usage-normalization
  test-rlm-context-object-adapter
  test-rlm-infer
  test-rlm-frame-registry
  test-rlm-framed-inference
  test-rlm-infer-tool
  test-rlm-tool-routing
  test-rlm-map
  test-rlm-map-tool
  test-rlm-policies
  test-rlm-distill-validation
  test-rlm-distill
  test-rlm-distill-tool
  test-rlm-trace-resource
  test-rlm-endpoint
  test-rlm-environment-reuse
  test-rlm-litmus-completion
  test-rlm-boundary-litmus
  test-rlm-complete-tool
  test-rlm-designator-confinement
  test-rlm-permission-classifier)

(define-test-suite run-job
  run-run-job-tests)

(define-test-suite task-scheduler
  test-task-default-detachment
  test-task-running-cancellation
  test-task-runtime-deadline
  test-task-artifact-retention
  test-task-nested-parent-cancellation
  test-task-admission-cancellation-barrier
  test-task-hurry-up-admission-races
  test-task-publication-coherence
  test-task-terminal-wakeup-ordering
  test-task-job-visibility
  test-task-durable-job-lookup
  test-session-tool-execution-jobs
  test-shell-execution-jobs
  test-tool-execution-overflow-binding
  test-tool-execution-retention
  test-task-job-list-pagination
  test-task-refresh-after-delayed-close
  test-task-terminal-cancellation-and-publication
  test-task-retention-and-admission
  test-task-evicted-identity-retention
  test-task-live-activity-snapshots
  test-task-cumulative-child-usage
  test-task-run-native-manifest
  test-task-closed-runtime-refresh
  test-task-scheduler)

(define-test-suite terminal
  test-terminal-primary-screen-controls
  test-terminal-nonblocking-lock-interrupt
  test-terminal-prompt-markers
  test-terminal-finalized-batch
  test-terminal-untrusted-text
  test-terminal-finalized-scrollback
  test-terminal-resize-frame
  test-terminal-relayed-resize
  test-terminal-line-editor
  test-terminal-history-replacement
  test-terminal-image-attachments
  test-terminal-input-decoding
  test-terminal-status-worked-time
  test-terminal-context-meter
  test-terminal-bounded-editor-repaint
  test-terminal-transient-notice
  test-terminal-notice-lock-contention
  test-terminal-timed-status
  test-terminal-compaction-indicator
  test-terminal-agent-activities
  test-terminal-command-activities
  test-terminal-stream-update
  test-terminal-command-completion
  test-terminal-lisp-operation-completion
  test-terminal-modal-selection
  test-terminal-application-read-resize
  test-terminal-non-tty-fallback)

(define-test-suite localgroup
  test-localgroup-conversation-identity
  test-localgroup-terminal-restart
  test-localgroup-picker-waits-for-relayed-input
  test-localgroup-remote-detach-never-pauses-reader
  test-localgroup-detached-terminal-lifecycle
  test-localgroup-protocol
  test-localgroup-orphan-reconciliation
  test-localgroup-attachments)

(define-test-suite localgroup-handoff
  test-localgroup-handoff-records
  test-localgroup-handoff-scheduling
  test-localgroup-detach-preempts-active-work
  test-localgroup-client-first-resume
  test-localgroup-abandoned-session-exit
  test-localgroup-fresh-session-spawn
  test-localgroup-process-handoff)

(define-test-suite localgroup-handoff-boundary
  test-localgroup-handoff-cancellation
  test-localgroup-fresh-startup-selection)

;;; The release scripts and the release server that drives them deploy to
;;; POSIX hosts, so their suites exist only where the POSIX shell does.
(when (test-fixture-available-p *platform* ':posix-shell)
  (define-test-suite release-script
    test-installer-checksum-verification
    test-release-scripts))

(define-test-suite data-transfer
  test-data-transfer-commands
  test-data-transfer-roundtrip
  test-data-transfer-workspace
  test-data-transfer-rejection
  test-data-transfer-rollback)

(when (test-fixture-available-p *platform* ':posix-shell)
  (define-test-suite release-server
    test-release-server))

(define-test-suite application
  test-application-command-tips
  test-application-banner-policy
  test-application-git-branch
  test-startup-update-choice
  test-explicit-update-operation
  test-session-titles-command
  test-hurry-up-mode
  test-command-permission-modes
  test-interrupt-resume-instruction
  test-repeated-interrupt-forces-exit
  test-forced-exit-without-durable-conversation
  test-graceful-shutdown-retains-interrupt-escape
  test-active-turn-interrupt-events
  test-active-turn-cancellation-side-effects
  test-idle-interrupt-exits-without-force-hint
  test-recalled-follow-up-interrupt
  test-active-turn-stop-keys
  test-active-command-stop-key
  test-active-tool-stop-repairs-unknown-outcome
  test-interrupt-force-window
  test-visible-interrupt-hint-does-not-extend-window
  test-dropped-interrupt-hint-reappears
  test-active-cancellation-interrupt-window-expiry
  test-cancellation-completion-clears-interrupt-state
  test-failed-turn-publishes-durable-wreckage
  test-transcript-entries
  test-recovery-cursor-normalization
  test-recovery-diagnosis-prompt
  test-recovery-application-construction
  test-bounded-transcript-replay
  test-chunked-transcript-replay
  test-hidden-reasoning-does-not-crowd-replay
  test-paged-transcript-history
  test-provider-protocol-failure-is-recoverable
  test-compaction-presentation-lifecycle
  test-streaming-presentation
  test-provider-retry-presentation
  test-turn-cursor-visibility
  test-responsive-model-input
  test-responsive-goal-inspection
  test-responsive-command-scheduling
  test-recovery-diagnosis-tool-surface
  test-input-reader-quiescence
  test-primary-prompt-admission
  test-command-turn-steering
  test-late-steering-promotion
  test-application-conversation-title-refresh
  test-conversation-picker
  test-working-directory-switch
  test-application-busy-conversation-resume
  test-application-fresh-conversation-lease-collision
  test-application-tool-runtime-lifecycle
  test-application-conversation-input-history
  test-application-runtime-replacement-transactions
  test-application-runtime-retirement-failures
  test-application-create-unwind-safety
  test-application-reconnect-unwind-safety
  test-application-task-presentation
  test-working-directory-command
  test-effort-switch
  test-session-goal
  test-pending-publication-lock-boundaries
  test-pending-input-persistence)

(define-test-suite lisp-machine
  test-application-lisp-evaluation
  test-application-restart-debugger
  test-application-debugger-recoveries
  test-application-debugger-modal-recoveries
  test-application-lisp-activity
  test-application-lisp-input-routing
  test-application-prompt-marker-reader-order
  test-application-prompt-marker-lifecycle)

(define-test-suite user-operation-context
  test-user-operation-persistence-and-context
  test-user-operation-bounds-and-validation
  test-user-operation-capture
  test-user-operation-command-outcomes
  test-user-operation-conversation-switch
  test-user-operation-retention-failure)

(define-test-suite application-operation
  run-application-operation-tests
  test-compact-operation
  test-compact-operation-empty
  test-compact-operation-failures)

(define-test-suite recovery-input-vault
  test-recovery-input-vault-import
  test-recovery-input-vault-legacy-isolation
  test-recovery-input-vault-corruption
  test-recovery-input-vault-unattributed-legacy
  test-recovery-input-vault-restore
  test-recovery-input-vault-active-restore-crash
  test-recovery-input-vault-restore-rollback
  test-recovery-input-vault-post-delete-rollback
  test-recovery-input-vault-discard
  test-recovery-input-vault-disabled-ingress
  test-recovery-input-vault-disabled-recalled-ingress
  test-recovery-input-vault-recovery-startup
  test-recovery-input-vault-corrupt-startup
  test-recovery-input-vault-ordinary-startup
  test-recovery-input-vault-live-primary-submit
  test-recovery-input-vault-capture-message
  test-recovery-input-vault-capture-during-restore)

(define-test-suite fixtures
  test-run-temporary-root-cleanup
  test-configuration-fixture-cleanup
  test-configuration-fixture-isolation
  test-environment-fixture-restoration
  test-environment-fixture-evaluation
  test-function-replacement-fixture-restoration)

(define-test-suite test-runner
  test-runner-selection
  test-runner-failure-reporting
  test-runner-temporary-cleanup
  test-check-worker-temporary-cleanup
  test-runner-catalog
  test-check-command-selection
  test-check-result-validation
  test-check-process-lifecycle)
