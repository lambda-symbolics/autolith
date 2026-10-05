(in-package #:autolith)

;;;; -- Optional Debug Session Ownership --

(defparameter *debug-adapter-program* nil
  "Explicit adapter executable path, or NIL to disable debug launch and attach.")

(defparameter *debug-adapter-arguments* nil
  "Literal adapter arguments; configured by the user, never through model input.")

(defparameter *debug-session-limit* 16
  "Maximum adapter sessions, including pending admission, in one registry.")

(defparameter *debug-conversation-session-limit* 4
  "Maximum retained sessions per conversation; terminate to release admission.")

(defparameter *debug-result-byte-limit* 65536
  "Maximum UTF-8 request, response and event frame bytes.")

(define-condition debug-session-error (tool-error)
  ((code :initarg :code :reader debug-session-error-code
         :documentation "Machine-readable product failure category."))
  (:documentation "A debug operation failed at the optional product boundary."))

(defclass debug-session-entry ()
  ((identifier :initform (daemon-random-token) :reader debug-session-entry-identifier
               :documentation "Opaque identifier scoped to the owning conversation.")
   (owner :initarg :owner :reader debug-session-entry-owner
          :documentation "Exact owning conversation object.")
   (session :initform nil :accessor debug-session-entry-session
            :documentation "Daphne session, or NIL during reserved admission.")
   (transport :initform nil :accessor debug-session-entry-transport
              :documentation "Owned adapter transport, retained for lifecycle inspection.")
   (events :initform nil :accessor debug-session-entry-events
           :documentation "At most one bounded 128-event batch awaiting paged presentation.")
   (busy-p :initform t :accessor debug-session-entry-busy-p
           :documentation "One semantic operation holds this entry.")
   (cancelled-p :initform nil :accessor debug-session-entry-cancelled-p
                :documentation "Cancellation requested by a concurrent debug.cancel call."))
  (:documentation "One bounded conversation-owned adapter connection."))

(defclass debug-session-manager ()
  ((entries :initform (make-hash-table :test #'equal) :reader debug-session-manager-entries
            :documentation "Strong entries until explicit termination or registry close.")
   (lock :initform (make-lock "debug sessions") :reader debug-session-manager-lock
         :documentation "Serializes admission, lookup and cancellation.")
   (closed-p :initform nil :accessor debug-session-manager-closed-p
             :documentation "Rejects admission during registry shutdown."))
  (:documentation "Shared ephemeral registry runtime with bounded process ownership."))

(-> debug--fail (keyword string) null)
(defun debug--fail (code message)
  "Signal a structured product error without adapter stderr or secrets."
  (error 'debug-session-error :tool-name "debug" :code code :message message))

(-> debug--bounded-json (t) string)
(defun debug--bounded-json (value)
  "Encode VALUE with an explicit UTF-8 byte limit."
  (let ((text (json-encode value)))
    (when (> (length (babel:string-to-octets text :encoding ':utf-8)) *debug-result-byte-limit*)
      (debug--fail ':result-limit "Debug JSON exceeds the configured byte limit; request a smaller page."))
    text))

(-> debug--authorize (tool-context string t) null)
(defun debug--authorize (context operation value)
  "Obtain execution authority for the exact adapter operation and complete arguments.

Daphne owns a persistent duplex subprocess. The current command sandbox does not
provide a duplex transport, so require full-access approval rather than launching
outside the sandbox under sandbox-only authority. Attach and evaluation use this
same boundary, independently of adapter startup approval."
  (unless (eq (tool-context-authorize-command
               context (format nil "debug.~A ~A" operation (debug--bounded-json value))
               (config :working-directory (tool-context-configuration context))) ':full-access)
    (debug--fail ':authorization "Debug execution requires full-access approval for this exact operation."))
  nil)

(-> debug--configuration (tool-context string) json-object)
(defun debug--configuration (context text)
  "Decode bounded adapter configuration and resolve its explicit program/cwd paths."
  (unless (and (stringp text) (<= (length text) *debug-result-byte-limit*))
    (debug--fail ':invalid-arguments "Configuration must be a bounded JSON object string."))
  (let ((value (handler-case (json-decode text)
                 (error () (debug--fail ':invalid-arguments "Configuration is not valid JSON.")))))
    (unless (hash-table-p value)
      (debug--fail ':invalid-arguments "Configuration must decode to a JSON object."))
    (dolist (key '("program" "cwd"))
      (let ((path (json-get value key)))
        (when path
          (unless (non-empty-string-p path)
            (debug--fail ':invalid-arguments "Configuration program and cwd must be path strings."))
          (setf (gethash key value)
                (uiop:native-namestring (workspace-tool-path context path :tool-name "debug"))))))
    (debug--bounded-json value)
    value))

(-> debug--reserve (debug-session-manager tool-context) debug-session-entry)
(defun debug--reserve (manager context)
  "Reserve process capacity before executing an adapter; never evict active sessions."
  (with-lock-held ((debug-session-manager-lock manager))
    (when (debug-session-manager-closed-p manager)
      (debug--fail ':closed "Debug runtime is closed."))
    (let* ((entries (debug-session-manager-entries manager))
           (owner (tool-context-conversation context))
           (owned (loop for entry being the hash-values of entries
                        count (eq owner (debug-session-entry-owner entry)))))
      (unless owner
        (debug--fail ':scope "Debug sessions require an owning conversation."))
      (when (or (>= (hash-table-count entries) *debug-session-limit*)
                (>= owned *debug-conversation-session-limit*))
        (debug--fail ':session-limit "Debug session limit reached; terminate an owned session first."))
      (let ((entry (make-instance 'debug-session-entry :owner owner)))
        (setf (gethash (debug-session-entry-identifier entry) entries) entry)
        entry))))

(-> debug--find (debug-session-manager tool-context t &key (:reserve-p boolean)) debug-session-entry)
(defun debug--find (manager context identifier &key reserve-p)
  "Resolve only a session owned by CONTEXT; optionally reserve exclusive operation access."
  (unless (non-empty-string-p identifier)
    (debug--fail ':invalid-arguments "Session must be a non-empty opaque identifier string."))
  (with-lock-held ((debug-session-manager-lock manager))
    (let ((entry (gethash identifier (debug-session-manager-entries manager))))
      (unless (and entry (eq (tool-context-conversation context) (debug-session-entry-owner entry)))
        (debug--fail ':unknown-session "Unknown session in this conversation."))
      (when reserve-p
        (when (debug-session-entry-busy-p entry)
          (debug--fail ':session-busy "Another debug operation is in progress; cancel may interrupt it."))
        (setf (debug-session-entry-busy-p entry) t))
      entry)))

(-> debug--remove (debug-session-manager debug-session-entry) null)
(defun debug--remove (manager entry)
  "Close and reap ENTRY before releasing capacity."
  (let ((session (debug-session-entry-session entry)))
    (when session (daphne:session-close session)))
  (with-lock-held ((debug-session-manager-lock manager))
    (remhash (debug-session-entry-identifier entry) (debug-session-manager-entries manager)))
  nil)

(-> debug--cancel-p (debug-session-manager debug-session-entry (option function)) boolean)
(defun debug--cancel-p (manager entry predicate)
  "Combine concurrent cancellation, runtime shutdown and optional supervisor cancellation."
  (or (with-lock-held ((debug-session-manager-lock manager))
        (or (debug-session-manager-closed-p manager) (debug-session-entry-cancelled-p entry)))
      (and predicate (not (null (funcall predicate))))))

(-> debug-session-manager-close (debug-session-manager) null)
(defun debug-session-manager-close (manager)
  "Cancel, close and reap every owned adapter, including pending startup admissions."
  (let ((entries (with-lock-held ((debug-session-manager-lock manager))
                   (setf (debug-session-manager-closed-p manager) t)
                   (loop for entry being the hash-values of (debug-session-manager-entries manager)
                         do (setf (debug-session-entry-cancelled-p entry) t)
                         collect entry))))
    (dolist (entry entries) (debug--remove manager entry)))
  nil)

(-> debug--translate (daphne:dap-error) null)
(defun debug--translate (condition)
  "Translate typed library failures to stable product codes."
  (debug--fail (typecase condition
                 (daphne:dap-cancelled ':cancelled)
                 (daphne:dap-timeout ':timeout)
                 (daphne:dap-transport-error ':adapter-died)
                 (daphne:dap-protocol-error ':protocol)
                 (daphne:dap-state-error ':session-state)
                 (daphne:dap-request-error ':adapter-rejected)
                 (daphne:dap-limit-error ':limit)
                 (t ':adapter-error))
               (let ((message (daphne:dap-error-message condition)))
                 (subseq message 0 (min (length message) 2048)))))
