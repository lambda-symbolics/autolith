(in-package #:autolith)

;;;; -- Terminal Defaults --
(defparameter *terminal-history-limit* most-positive-fixnum
  "The practical maximum retained by the line editor, effectively unlimited.")

(defparameter *terminal-ui-visible-completions* 6
  "The maximum number of candidate rows painted at once.")


;;;; -- Terminal Objects --

(defvar *terminal-relayed-resize* nil
  "Pending (rows . columns) relayed by a controlling client, or NIL.

A relayed session's process has no controlling terminal, so its own
kernel size reports only ever see fallback defaults. Controlling
clients therefore relay exact dimensions, and the interactive reader
applies them through TERMINAL-UI-RESIZE. All terminal dimension
mutation passes through TERMINAL-SET-DIMENSIONS.")

(defvar *terminal-relayed-resize-lock*
  (make-lock "Autolith relayed terminal resize")
  "The lock making relayed resize publication and consumption atomic.")

(-> terminal-relayed-resize-publish (integer integer) null)
(defun terminal-relayed-resize-publish (rows columns)
  "Publish positive ROWS and COLUMNS for one relayed terminal resize."
  (when (and (plusp rows) (plusp columns))
    (with-lock-held (*terminal-relayed-resize-lock*)
      (setf *terminal-relayed-resize* (cons rows columns))))
  nil)

(-> terminal-relayed-resize-consume
    ()
    (option (cons (integer 1) (integer 1))))
(defun terminal-relayed-resize-consume ()
  "Atomically consume and return the newest relayed terminal size."
  (with-lock-held (*terminal-relayed-resize-lock*)
    (prog1 *terminal-relayed-resize*
      (setf *terminal-relayed-resize* nil))))

(defclass terminal (clinedi:terminal) ()
  (:documentation "Autolith's terminal transport extension point."))

(defclass stream-terminal (terminal clinedi:host-terminal) ()
  (:default-initargs :event-decoder #'terminal--decode-editing-event
                    :event-prefix-p-function
                    (lambda (character) (find character (list #\Escape (code-char 22))))
                    :styling-p-function #'terminal-environment-styling-p)
  (:documentation "Native stream transport with Autolith's input and styling policy."))

(defclass terminal-ui ()
  ((lock
    :initform (make-recursive-lock "Autolith terminal UI")
    :reader terminal-ui-lock
    :type t
    :documentation "The recursive lock serializing editor state and terminal writes.")
   (terminal
    :initarg :terminal
    :reader terminal-ui-terminal
    :type terminal
    :documentation "The primary-screen terminal transport.")
   (editor
    :initarg :editor
    :reader terminal-ui-editor
    :type line-editor
    :documentation "The Unicode-aware multiline user input editor.")
   (live-region
    :initarg :live-region
    :reader terminal-ui-live-region
    :type live-region
    :documentation "Clinedi region anchoring unfinished content below scrollback.")
   (prompt
    :initarg :prompt
    :reader terminal-ui-prompt
    :type string
    :documentation "The untrusted-text-safe prompt prefix.")
   (message-header-prefixes
    :initarg :message-header-prefixes
    :initform nil
    :reader terminal-ui-message-header-prefixes
    :type list
    :documentation
    "Plain-text prefixes of the transcript lines that open user and assistant messages.")
   (prompt-marker-state
    :initform ':closed
    :accessor terminal-ui-prompt-marker-state
    :type (member :closed :prompt :input :executing)
    :documentation "The current semantic OSC 133 prompt-block boundary.")
    (lisp-input-p
     :initform nil
     :accessor terminal-ui-lisp-input-p
     :type boolean
     :documentation "Whether the editor is explicitly reading Common Lisp input.")
   (live-output-suspended-p
    :initform nil
    :accessor terminal-ui-live-output-suspended-p
    :type boolean
    :documentation "Whether transient live-region repaint is suspended for direct I/O.")
   (boot-waiting-p
    :initform nil
    :accessor terminal-ui-boot-waiting-p
    :type boolean
    :documentation "Whether the boot screen owns keystrokes until the operator starts the session.")
   (boot-start-requested-p
    :initform nil
    :accessor terminal-ui-boot-start-requested-p
    :type boolean
    :documentation "Whether a start key reached the waiting boot screen through the reader.")
   (deferred-live-appended-text
    :initform ""
    :accessor terminal-ui-deferred-live-appended-text
    :type string
    :documentation "Plain scrollback deferred while direct terminal I/O owns the display.")
   (deferred-live-appended-display
    :initform ""
    :accessor terminal-ui-deferred-live-appended-display
    :type string
    :documentation "Styled scrollback deferred while direct terminal I/O owns the display.")
   (deferred-live-appended-regions
    :initform nil
    :accessor terminal-ui-deferred-live-appended-regions
    :type list
    :documentation "Click regions of the deferred scrollback, offset within its plain text.")
   (action-function
    :initform nil
    :accessor terminal-ui-action-function
    :type (option function)
    :documentation
    "The function receiving a clicked transcript action such as (:copy TEXT), or NIL.")
   (pending-action
    :initform nil
    :accessor terminal-ui-pending-action
    :type list
    :documentation
    "A clicked transcript action awaiting delivery once the presentation lock is released.")
   (prompt-render-cache
    :initform nil
    :accessor terminal-ui-prompt-render-cache
    :type (option list)
    :documentation
    "The memoized prompt row, cursor offset, and their exact render inputs.")
   (placeholder
    :initarg :placeholder
    :initform ""
    :reader terminal-ui-placeholder
    :type string
    :documentation "The dim hint shown on the prompt row while input is empty.")
   (completions
    :initarg :completions
    :initform nil
    :reader terminal-ui-completions
    :type list
    :documentation "Completion entries offered while typing an interactive command.")
   (completion-function
    :initarg :completion-function
    :initform nil
    :accessor terminal-ui-completion-function
    :type (option function)
    :documentation
    "Optional function returning the current completion entries on demand.")
   (completion-root
    :initarg :completion-root
    :initform nil
    :accessor terminal-ui-completion-root
    :type (option pathname)
    :documentation
    "Workspace directory used for path completion. NIL uses the process working directory.")
   (path-search-function
    :initarg :path-search-function
    :initform nil
    :accessor terminal-ui-path-search-function
    :type (option function)
    :documentation
    "Optional function of QUERY returning ranked workspace-relative paths.")
   (completion-selector
    :initarg :completion-selector
    :reader terminal-ui-completion-selector
    :type selector
    :documentation "Clinedi navigation state for matching command completions.")
   (completion-active-p
    :initform nil
    :accessor terminal-ui-completion-active-p
    :type boolean
    :documentation "Whether arrows and Tab are choosing among completion candidates.")
   (completion-prefix
    :initform nil
    :accessor terminal-ui-completion-prefix
    :type (option string)
    :documentation "Input restored when active completion selection is cancelled.")
   (completion-history-state
    :initform nil
    :accessor terminal-ui-completion-history-state
    :type (option clinedi:line-editor-state)
    :documentation
    "Clinedi history traversal state restored when completion is cancelled.")
   (completion-dismissed-p
    :initform nil
    :accessor terminal-ui-completion-dismissed-p
    :type boolean
    :documentation "Whether Escape has hidden passive completion suggestions.")
   (selector
    :initform nil
    :accessor terminal-ui-selector
    :type (option selector)
    :documentation "Clinedi navigation state for the active modal picker.")
   (selector-title
    :initform nil
    :accessor terminal-ui-selector-title
    :type (option string)
    :documentation "The application-owned title for the active modal picker.")
   (selector-hint
    :initform nil
    :accessor terminal-ui-selector-hint
    :type (option string)
    :documentation "Optional modal picker hint text after the title.")
   (idle-status-details
    :initform nil
    :accessor terminal-ui-idle-status-details
    :type list
    :documentation "Static session, model, effort, and repository details shown while idle.")
   (status
    :initform nil
    :accessor terminal-ui-status
    :type (option string)
    :documentation "The optional unfinished activity shown above the prompt.")
   (context-used
    :initform nil
    :accessor terminal-ui-context-used
    :type (option (integer 0))
    :documentation "The newest provider-reported context usage in tokens.")
   (context-window
    :initform nil
    :accessor terminal-ui-context-window
    :type (option (integer 1))
    :documentation "The active model's context window in tokens.")
   (context-compaction-limit
    :initform nil
    :accessor terminal-ui-context-compaction-limit
    :type (option (integer 1))
    :documentation "The context usage at which automatic compaction begins.")
   (compacting-p
    :initform nil
    :accessor terminal-ui-compacting-p
    :type boolean
    :documentation "Whether an indeterminate conversation compaction is active.")
   (compaction-started-at
    :initform nil
    :accessor terminal-ui-compaction-started-at
    :type (option real)
    :documentation "The monotonic time at which active compaction began.")
   (notice
    :initform nil
    :accessor terminal-ui-notice
    :type (option string)
    :documentation "The optional transient notice shown above the prompt.")
   (notice-deadline
    :initform nil
    :accessor terminal-ui-notice-deadline
    :type (option real)
    :documentation "The monotonic time at which the transient notice expires.")
   (status-details
    :initform nil
    :accessor terminal-ui-status-details
    :type list
    :documentation "Styled model, effort, and repository details beside the activity.")
   (status-started-at
    :initform nil
    :accessor terminal-ui-status-started-at
    :type (option real)
    :documentation "The monotonic time at which the current activity phase began.")
   (status-progress-at
    :initform nil
    :accessor terminal-ui-status-progress-at
    :type (option real)
    :documentation "The monotonic time of the newest progress within the activity phase.")
   (status-worked-seconds
    :initform nil
    :accessor terminal-ui-status-worked-seconds
    :type (option (integer 0))
    :documentation
    "The conversation's accumulated working seconds when the activity began.")
    (local-activity
     :initform nil
     :accessor terminal-ui-local-activity
     :type (option string)
     :documentation "Explicit local Lisp work shown below the animated status row.")
    (local-activity-started-at
     :initform nil
     :accessor terminal-ui-local-activity-started-at
     :type (option real)
     :documentation "The monotonic time at which explicit local Lisp work began.")
   (agent-activities
    :initform nil
    :accessor terminal-ui-agent-activities
    :type list
    :documentation
    "Sanitized queued and running child-agent summaries in stable display order.")
    (command-activities
     :initform nil
     :accessor terminal-ui-command-activities
     :type list
     :documentation
     "Sanitized queued and running primary command summaries in stable order.")
    (command-unpainted-identifiers
     :initform nil
     :accessor terminal-ui-command-unpainted-identifiers
     :type list
     :documentation
     "Current command identifiers that have not reached a reader-owned paint.")
    (command-pending-completions
     :initform nil
     :accessor terminal-ui-command-pending-completions
     :type list
     :documentation
     "Bounded completed command snapshots awaiting their first visible paint.")
   (status-rendered-signature
    :initform nil
    :accessor terminal-ui-status-rendered-signature
    :type list
    :documentation
    "The command and child-agent values used by the newest live paint.")
   (clock-function
    :initarg :clock-function
    :initform (lambda ()
                (/ (get-internal-real-time)
                   (coerce internal-time-units-per-second 'double-float)))
    :reader terminal-ui-clock-function
    :type function
    :documentation "The injected monotonic clock function returning seconds.")
   (exclusive-input-function
    :initarg :exclusive-input-function
    :initform #'funcall
    :accessor terminal-ui-exclusive-input-function
    :type function
    :documentation
    "The function calling a modal-input thunk with sole ownership of terminal input.")
   (preview-rows
    :initform nil
    :accessor terminal-ui-preview-rows
    :type list
    :documentation "Transient styled rows shown in the live region, never scrollback.")
   (queued-input-previews
    :initform nil
    :accessor terminal-ui-queued-input-previews
    :type list
    :documentation "Sanitized queued follow-up text shown in the live region.")
   (steering-input-previews
    :initform nil
    :accessor terminal-ui-steering-input-previews
    :type list
    :documentation "Sanitized steering text shown in the live region.")
   (image-attachments
    :initform nil
    :accessor terminal-ui-image-attachments
    :type list
    :documentation "Local image pathnames and labels attached to the current draft.")
   (image-history
    :initform nil
    :accessor terminal-ui-image-history
    :type list
    :documentation "Recent editor history text paired with its image attachments.")
   (stream-tail
    :initform nil
    :accessor terminal-ui-stream-tail
    :type (or null string list)
    :documentation "Unfinished streamed text, styled spans, or styled rows continuing the transcript block above.")
   (finalized-identifiers
    :initform (make-hash-table :test #'equal)
    :reader terminal-ui-finalized-identifiers
    :type hash-table
    :documentation "Identifiers whose finalized transcript text was already emitted.")
   (started-p
    :initform nil
    :accessor terminal-ui-started-p
    :type boolean
    :documentation "Whether the UI lifecycle has started."))
  (:documentation
   "A scrollback-preserving UI with immutable transcript output and a bounded live region."))



;;;; -- Terminal Conditions --

(define-condition terminal-error (autolith-error)
  ((operation
    :initarg :operation
    :reader terminal-error-operation
    :type keyword
    :documentation "The terminal operation that could not complete.")
   (cause
    :initarg :cause
    :reader terminal-error-cause
    :type (option condition)
    :documentation "The underlying implementation condition, when available."))
  (:documentation "A terminal mode, input, or output operation failed."))
















(defgeneric terminal--write (terminal text)
  (:documentation "Write trusted renderer TEXT through the terminal transport."))





(-> terminal--prompt-marker-sequence (keyword integer) string)
(defun terminal--prompt-marker-sequence (marker status)
  "Return the OSC 133 sequence for MARKER and integer STATUS.
Autolith repaints its own prompt, so prompt starts disable terminal redraw."
  (semantic-prompt-marker-sequence marker
                                   :status   status
                                   :redraw-p (not (eq marker ':prompt-start))))

(-> terminal-write-prompt-marker
    (terminal keyword &optional (integer 0))
    boolean)
(defun terminal-write-prompt-marker (terminal marker &optional (status 0))
  "Write and flush one OSC 133 MARKER for interactive TERMINAL, if applicable."
  (when (terminal-interactive-p terminal)
    (terminal--write terminal
                     (terminal--prompt-marker-sequence marker status))
    (terminal-flush terminal)
    t))


(-> terminal-write-window-title (terminal string) boolean)
(defun terminal-write-window-title (terminal title)
  "Write and flush one OSC 0 TITLE for interactive TERMINAL, if applicable."
  (let ((safe (sanitize-text title :single-line-p t :replacement-character nil)))
    (when (and (terminal-interactive-p terminal)
               (non-empty-string-p safe))
      (terminal--write terminal (window-title-sequence safe))
      (terminal-flush terminal)
      t)))
;;;; -- Presentation Modes --

(defgeneric terminal-ui-fullscreen-p (ui)
  (:documentation "Return true when UI owns an application-scrolled fullscreen layout."))

(defmethod terminal-ui-fullscreen-p ((ui terminal-ui))
  "Use native scrollback for the ordinary terminal UI."
  nil)

(defgeneric terminal-ui--present-live
    (ui &key status-now appended-text appended-display appended-regions)
  (:documentation
   "Present live content and any newly committed transcript text in UI's mode.

APPENDED-REGIONS are the (START END ACTION) click regions of APPENDED-TEXT."))

(defgeneric terminal-ui-set-epilogue (ui entry)
  (:documentation
   "Present ENTRY so it remains readable after UI stops.

Native scrollback keeps appended output, so the inline UI appends ENTRY at
once; the fullscreen UI writes it to the normal screen when it leaves the
alternate buffer, since anything shown inside that buffer vanishes with it."))

(defgeneric terminal-ui--append-output (ui text display &key regions)
  (:documentation
   "Append plain TEXT and trusted styled DISPLAY to UI's transcript.

REGIONS are the (START END ACTION) click regions indexing TEXT."))
