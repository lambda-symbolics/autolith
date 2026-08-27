(in-package #:autolith)

;;;; -- Base Conditions --

(define-condition autolith-error (error)
  ((message
    :initarg :message
    :reader autolith-error-message
    :type string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected Autolith failures.")
  (:report (lambda (condition stream)
             (write-string (autolith-error-message condition) stream))))

(define-condition configuration-error (autolith-error)
  ()
  (:documentation "A failure caused by invalid or unavailable configuration."))

(define-condition user-init-error (configuration-error)
  ((pathname
    :initarg :pathname
    :reader user-init-error-pathname
    :type pathname
    :documentation "The executable initialization file that could not be loaded.")
   (layer
    :initarg :layer
    :reader user-init-error-layer
    :type (member :site :directory :user)
    :documentation "The configuration layer whose initialization failed.")
   (cause
    :initarg :cause
    :reader user-init-error-cause
    :type serious-condition
    :documentation "The underlying condition signaled while loading the file."))
  (:documentation "Loading one executable Autolith configuration layer failed."))

(define-condition working-directory-error (configuration-error)
  ((requested-path
    :initarg :requested-path
    :reader working-directory-error-requested-path
    :type t
    :documentation "The user-supplied location that could not become the workspace.")
   (previous-directory
    :initarg :previous-directory
    :reader working-directory-error-previous-directory
    :type pathname
    :documentation "The workspace that was active before the failed change.")
   (stage
    :initarg :stage
    :reader working-directory-error-stage
    :type keyword
    :documentation "The validation, worker, process, tools, or application stage that failed.")
   (cause
    :initarg :cause
    :reader working-directory-error-cause
    :type t
    :documentation "The underlying failure that prevented the directory change.")
   (rollback-cause
    :initarg :rollback-cause
    :initform nil
    :reader working-directory-error-rollback-cause
    :type t
    :documentation "A secondary failure while restoring the previous workspace."))
  (:documentation "Changing the active process and tool workspace failed."))

(define-condition application-runtime-replacement-error (autolith-error)
  ((operation
    :initarg :operation
    :reader application-runtime-replacement-error-operation
    :type keyword
    :documentation "The application operation that attempted to replace the runtime.")
   (stage
    :initarg :stage
    :reader application-runtime-replacement-error-stage
    :type keyword
    :documentation "The retirement or installation stage that failed.")
   (cause
    :initarg :cause
    :reader application-runtime-replacement-error-cause
    :type serious-condition
    :documentation "The failure that prevented the runtime replacement.")
   (rollback-causes
    :initarg :rollback-causes
    :initform nil
    :reader application-runtime-replacement-error-rollback-causes
    :type list
    :documentation "Secondary failures while restoring the previous runtime."))
  (:documentation
   "A live application runtime replacement failed during retirement or installation."))

(define-condition rollback-requested (autolith-error)
  ((generation-id
    :initarg :generation-id
    :reader rollback-requested-generation-id
    :type non-empty-string
    :documentation "The retained generation selected for the next process."))
  (:documentation "A control condition requesting rollback to a retained generation."))

(define-condition update-requested (autolith-error)
  ((tag
    :initarg :tag
    :reader update-requested-tag
    :type non-empty-string
    :documentation "The newer release tag explicitly selected by the user."))
  (:documentation
   "A control condition requesting an update after the terminal has been restored."))

(define-condition application-turn-cancelled (serious-condition)
  ()
  (:documentation
   "An internal control condition unwinding the current application turn.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The active turn was cancelled."
                           stream))))

(define-condition application-input-failed (serious-condition)
  ((original-condition
    :initarg :original-condition
    :reader application-input-failed-original-condition
    :type serious-condition
    :documentation "The terminal reader failure that ended responsive input.")
   (backtrace
    :initarg :backtrace
    :reader application-input-failed-backtrace
    :type (option string)
    :documentation "The backtrace captured on the terminal reader thread."))
  (:documentation
   "An internal control condition transferring reader failure to the main thread.")
  (:report (lambda (condition stream)
             (format stream "Terminal input failed: ~A"
                     (application-input-failed-original-condition condition)))))


;;;; -- Authentication and Provider Conditions --

(define-condition authentication-error (autolith-error)
  ()
  (:documentation "The base condition for authentication failures."))

(define-condition credentials-unavailable (authentication-error)
  ((searched-paths
    :initarg :searched-paths
    :reader credentials-unavailable-searched-paths
    :type list
    :documentation "Credential pathnames inspected before the failure."))
  (:documentation "No usable model-provider credentials were found."))

(define-condition token-refresh-failed (authentication-error)
  ((status
    :initarg :status
    :reader token-refresh-failed-status
    :type (option integer)
    :documentation "The HTTP status returned by the OAuth server, if known.")
   (response
    :initarg :response
    :reader token-refresh-failed-response
    :type (option string)
    :documentation "A bounded, non-secret OAuth error response."))
  (:documentation "Refreshing a ChatGPT OAuth access token failed."))

(define-condition provider-model-discovery-error (configuration-error)
  ((provider-name
    :initarg :provider-name
    :reader provider-model-discovery-error-provider-name
    :type non-empty-string
    :documentation "The provider whose model list could not be discovered.")
   (cause
    :initarg :cause
    :reader provider-model-discovery-error-cause
    :type serious-condition
    :documentation "The underlying request or response failure."))
  (:documentation "A provider's dynamic model list could not be refreshed."))

(define-condition provider-error (autolith-error)
  ((status
    :initarg :status
    :reader provider-error-status
    :type (option integer)
    :documentation "The provider HTTP status, if a response was received.")
   (code
    :initarg :code
    :initform nil
    :reader provider-error-code
    :type (option string)
    :documentation "The provider's structured error code, if supplied.")
   (request-id
    :initarg :request-id
    :reader provider-error-request-id
    :type (option string)
    :documentation "The provider request identifier, if supplied.")
   (response-id
    :initarg :response-id
    :initform nil
    :reader provider-error-response-id
    :type (option string)
    :documentation "The failed response identifier, if supplied.")
   (response
    :initarg :response
    :reader provider-error-response
    :type (option string)
    :documentation "A bounded provider response safe for display."))
  (:documentation "A model-provider request failed."))

(define-condition provider-retryable-error
    (llm-provider-api:provider-retryable-error provider-error)
  ()
  (:documentation "A transient provider failure eligible for bounded retry.
Inherits the library condition so the shared retry ladder recognizes it."))

(define-condition provider-incomplete-response (provider-retryable-error)
  ((reason
    :initarg :reason
    :reader provider-incomplete-response-reason
    :type non-empty-string
    :documentation "The provider's incomplete response reason, or unknown."))
  (:documentation "A retryable provider response that ended incomplete."))

(define-condition provider-resample-requested
    (llm-provider-api:provider-resample-requested provider-retryable-error)
  ()
  (:documentation
   "A provider reported a degenerate generation loop worth resampling.
The trigger, attempt, and budget slots come from the library condition."))

(define-condition provider-transport-error (provider-retryable-error)
  ()
  (:documentation
   "A transient provider connection failure eligible for reconnection."))

(define-condition response-stream-error (provider-transport-error)
  ()
  (:documentation "A provider stream ended without a valid terminal event."))

(define-condition response-stream-limit-error
    (llm-provider-api:provider-stream-limit-error response-stream-error)
  ()
  (:default-initargs :status nil :request-id nil :response nil)
  (:documentation
   "The shared SSE decoder rejected an oversized provider stream."))

(define-condition provider-unauthorized (provider-error)
  ()
  (:documentation "A bounded provider attempt was rejected as unauthorized."))


;;;; -- Persistence and Tool Conditions --

(define-condition search-error (autolith-error)
  ((operation
    :initarg :operation
    :reader search-error-operation
    :type keyword
    :documentation "The native search operation that failed.")
   (pathname
    :initarg :pathname
    :initform nil
    :reader search-error-pathname
    :type (option pathname)
    :documentation "The native library or workspace path involved, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader search-error-cause
    :type t
    :documentation "The underlying native or Lisp failure, when available."))
  (:documentation "The private fff search library could not complete an operation."))

(define-condition image-input-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader image-input-error-pathname
    :type pathname
    :documentation "The local image pathname involved in the failure.")
   (stage
    :initarg :stage
    :reader image-input-error-stage
    :type keyword
    :documentation "The recognition, decoding, resizing, persistence, or loading stage.")
   (cause
    :initarg :cause
    :initform nil
    :reader image-input-error-cause
    :type t
    :documentation "The underlying image or filesystem failure, when available."))
  (:documentation "A local image could not become provider-visible user input."))

(define-condition preferences-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader preferences-error-pathname
    :type pathname
    :documentation "The global preferences file being read or written.")
   (operation
    :initarg :operation
    :reader preferences-error-operation
    :type keyword
    :documentation "The preference operation that failed.")
   (cause
    :initarg :cause
    :reader preferences-error-cause
    :type t
    :documentation "The underlying failure, when available."))
  (:documentation "Global preferences could not be read or persisted."))

(define-condition preferences-load-warning (warning)
  ((pathname
    :initarg :pathname
    :reader preferences-load-warning-pathname
    :type pathname
    :documentation "The invalid preferences file that was ignored.")
   (cause
    :initarg :cause
    :reader preferences-load-warning-cause
    :type preferences-error
    :documentation "The structured preference read failure."))
  (:documentation "A malformed preference file was ignored during startup.")
  (:report (lambda (condition stream)
             (format stream "Ignoring preferences at ~A: ~A"
                     (preferences-load-warning-pathname condition)
                     (preferences-load-warning-cause condition)))))

(define-condition project-adaptation-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader project-adaptation-error-pathname
    :type pathname
    :documentation "The project-adaptation file or state file involved.")
   (operation
    :initarg :operation
    :reader project-adaptation-error-operation
    :type keyword
    :documentation "The project-adaptation operation that failed.")
   (cause
    :initarg :cause
    :initform nil
    :reader project-adaptation-error-cause
    :type t
    :documentation "The underlying failure, when available."))
  (:documentation "Project adaptation notes or offer state could not be handled."))

(define-condition permissions-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader permissions-error-pathname
    :type pathname
    :documentation "The command permission file being read or written.")
   (operation
    :initarg :operation
    :reader permissions-error-operation
    :type keyword
    :documentation "The command permission operation that failed.")
   (cause
    :initarg :cause
    :reader permissions-error-cause
    :type t
    :documentation "The underlying persistence failure, when available."))
  (:documentation "Persistent command permissions could not be read or written."))

(define-condition command-authorization-unavailable (autolith-error)
  ((command
    :initarg :command
    :reader command-authorization-unavailable-command
    :type string
    :documentation "The shell command that could not be presented for approval.")
   (directory
    :initarg :directory
    :reader command-authorization-unavailable-directory
    :type pathname
    :documentation "The command working directory."))
  (:documentation
   "A command requiring user approval had no interactive terminal owner."))

(define-condition permissions-load-warning (warning)
  ((pathname
    :initarg :pathname
    :reader permissions-load-warning-pathname
    :type pathname
    :documentation "The malformed command permission pathname.")
   (cause
    :initarg :cause
    :reader permissions-load-warning-cause
    :type permissions-error
    :documentation "The structured permission read failure."))
  (:report (lambda (condition stream)
             (format stream "Ignoring command permissions at ~A: ~A"
                     (permissions-load-warning-pathname condition)
                     (permissions-load-warning-cause condition))))
  (:documentation "Malformed command permissions were ignored to fail closed."))

(define-condition agenda-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader agenda-error-pathname
    :type pathname
    :documentation "The workspace-agenda file being read or written.")
   (operation
    :initarg :operation
    :reader agenda-error-operation
    :type keyword
    :documentation "The agenda operation that failed.")
   (cause
    :initarg :cause
    :reader agenda-error-cause
    :type t
    :documentation "The underlying agenda failure, when available."))
  (:documentation "Workspace agendas could not be validated, read, or persisted."))

(define-condition agenda-load-warning (warning)
  ((pathname
    :initarg :pathname
    :reader agenda-load-warning-pathname
    :type pathname
    :documentation "The malformed workspace-agenda pathname.")
   (cause
    :initarg :cause
    :reader agenda-load-warning-cause
    :type agenda-error
    :documentation "The structured workspace-agenda read failure."))
  (:report (lambda (condition stream)
             (format stream "Ignoring workspace agendas at ~A: ~A"
                     (agenda-load-warning-pathname condition)
                     (agenda-load-warning-cause condition))))
  (:documentation "Malformed workspace-agenda state was ignored during startup."))

(define-condition conversation-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader conversation-error-pathname
    :type pathname
    :documentation "The conversation file being processed.")
   (sequence
    :initarg :sequence
    :reader conversation-error-sequence
    :type (option integer)
    :documentation "The nearest record sequence number, if known."))
  (:documentation "A conversation file is corrupt or cannot be persisted."))

(define-condition conversation-invariant-error (conversation-error)
  ()
  (:documentation "Conversation persistence or replay violated a critical invariant."))

(define-condition conversation-in-use (conversation-error)
  ((identifier
    :initarg :identifier
    :reader conversation-in-use-identifier
    :type non-empty-string
    :documentation "The normalized identifier owned by another live process.")
   (lease-pathname
    :initarg :lease-pathname
    :reader conversation-in-use-lease-pathname
    :type pathname
    :documentation "The process-shared lease file whose lock is held."))
  (:documentation
   "A live Autolith process already owns the requested conversation."))

(define-condition conversation-identifier-error (autolith-error)
  ((value
    :initarg :value
    :reader conversation-identifier-error-value
    :type t
    :documentation "The value that could not be parsed as a conversation identifier."))
  (:documentation "A displayed or stored conversation identifier is invalid."))

(define-condition conversation-identifier-space-exhausted (conversation-error)
  ((timestamp
    :initarg :timestamp
    :reader conversation-identifier-space-exhausted-timestamp
    :type timestamp
    :documentation "The Universal Time for which every seed was occupied."))
  (:documentation
   "All 58 conversation identifier seeds were occupied for one second."))

(define-condition conversation-identifier-migration-error (conversation-error)
  ((stage
    :initarg :stage
    :reader conversation-identifier-migration-error-stage
    :type keyword
    :documentation "The planning, conversation, reference, artifact, or cleanup stage.")
   (cause
    :initarg :cause
    :initform nil
    :reader conversation-identifier-migration-error-cause
    :type t
    :documentation "The underlying migration failure, when available."))
  (:documentation "Legacy conversation identifiers could not be migrated safely."))

(define-condition memory-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader memory-error-pathname
    :type pathname
    :documentation "The persistent memory file being processed.")
   (identifier
    :initarg :identifier
    :initform nil
    :reader memory-error-identifier
    :type (option string)
    :documentation "The memory identifier involved in the failure, when known."))
  (:documentation "Persistent memory data is invalid or cannot be updated."))

(define-condition papercut-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader papercut-error-pathname
    :type pathname
    :documentation "The persistent papercut file being processed.")
   (identifier
    :initarg :identifier
    :initform nil
    :reader papercut-error-identifier
    :type (option string)
    :documentation "The papercut identifier involved in the failure, when known."))
  (:documentation "Persistent papercut data is invalid or cannot be updated."))

(define-condition tool-error (autolith-error)
  ((tool-name
    :initarg :tool-name
    :reader tool-error-tool-name
    :type string
    :documentation "The canonical dotted tool name.")
   (code
    :initarg :code
    :initform nil
    :reader tool-error-code
    :type (option keyword)
    :documentation "The machine-readable failure discriminator, when one exists."))
  (:documentation "A tool call could not be validated or executed."))

(defgeneric tool-failure-code (condition)
  (:documentation
   "Return CONDITION's machine-readable tool failure code, or NIL.
Codes let tests and callers discriminate failures without pinning prose.")
  (:method ((condition condition))
    nil)
  (:method ((condition tool-error))
    (tool-error-code condition)))

(define-condition worker-error (tool-error)
  ()
  (:documentation "The disposable Lisp worker failed or violated its protocol."))

(define-condition lisp-image-error (worker-error)
  ((pathname
    :initarg :pathname
    :reader lisp-image-error-pathname
    :type (option pathname)
    :documentation "The saved worker-image artifact involved in the failure.")
   (stage
    :initarg :stage
    :reader lisp-image-error-stage
    :type keyword
    :documentation "The worker-image operation stage that failed."))
  (:documentation "A saved Lisp worker image is invalid or could not be published."))

(define-condition source-mutation-error (tool-error)
  ((pathname
    :initarg :pathname
    :reader source-mutation-error-pathname
    :type (option pathname)
    :documentation "The source file involved in the failed mutation."))
  (:documentation "An active-image or durable source mutation failed."))

(define-condition image-commit-error (source-mutation-error)
  ((stage
    :initarg :stage
    :reader image-commit-error-stage
    :type keyword
    :documentation "The private image-commit stage that failed."))
  (:documentation
   "A private live-image mutation commit could not be validated or published."))

(define-condition self-correctable-error (autolith-error)
  ((restart-names
    :initarg :restart-names
    :reader self-correctable-error-restart-names
    :type list
    :documentation "The invokable restart names offered by the failed operation."))
  (:documentation
   "An active-image operation failed while offering selectable restarts."))

(define-condition active-image-corruption (autolith-error)
  ((original-condition
    :initarg :original-condition
    :reader active-image-corruption-original-condition
    :type serious-condition
    :documentation "The mutation failure that initiated restoration.")
   (restoration-condition
    :initarg :restoration-condition
    :reader active-image-corruption-restoration-condition
    :type serious-condition
    :documentation "The second failure that prevented image restoration."))
  (:documentation "A failed mutation could not restore the preceding active definition."))

(define-condition active-image-build-error (autolith-error)
  ((stage
    :initarg :stage
    :reader active-image-build-error-stage
    :type keyword
    :documentation "The active-image build stage that failed.")
   (pathname
    :initarg :pathname
    :reader active-image-build-error-pathname
    :type (option pathname)
    :documentation "The source or image artifact involved in the failure."))
  (:documentation
   "A preloaded active image could not be validated, saved, or published."))

(define-condition checkpoint-error (autolith-error)
  ((stage
    :initarg :stage
    :reader checkpoint-error-stage
    :type keyword
    :documentation "The checkpoint stage that failed.")
   (pathname
    :initarg :pathname
    :reader checkpoint-error-pathname
    :type (option pathname)
    :documentation "The checkpoint artifact involved in the failure, if any.")
   (cause
    :initarg :cause
    :initform nil
    :reader checkpoint-error-cause
    :type t
    :documentation "The underlying checkpoint preparation failure, if available."))
  (:documentation "A generation could not be validated, saved, or published."))

(define-condition checkpoint-runtime-resume-warning (warning)
  ((generation-id
    :initarg :generation-id
    :reader checkpoint-runtime-resume-warning-generation-id
    :type non-empty-string
    :documentation "The generation that continues publishing despite the warning.")
   (cause
    :initarg :cause
    :reader checkpoint-runtime-resume-warning-cause
    :type serious-condition
    :documentation "The failure while resuming a parent-side tool runtime."))
  (:documentation
   "A checkpoint was created but a parent runtime could not resume.")
  (:report
   (lambda (condition stream)
     (format
     stream
      "Checkpoint ~A is publishing, but a tool runtime could not resume."
      (checkpoint-runtime-resume-warning-generation-id condition)))))
