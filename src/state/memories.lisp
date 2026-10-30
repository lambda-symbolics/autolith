(in-package #:autolith)

;;;; -- Legacy Memory Context --

(defmethod autolith-supervisor::memory-context-make-identifier ((configuration configuration))
  "Use the legacy identity source for memories created through configuration."
  (declare (ignore configuration))
  (make-identifier))

(defmethod autolith-supervisor::memory-context-memory-path ((configuration configuration))
  "Return the legacy configuration's private memory path."
  (configuration-memory-path configuration))

(defmethod autolith-supervisor::memory-context-knowledge-path ((configuration configuration))
  "Return the legacy configuration's private knowledge path."
  (configuration-knowledge-path configuration))

(defmethod autolith-supervisor::memory-context-workspace ((configuration configuration))
  "Return the legacy configuration's approved workspace directory."
  (config :working-directory configuration))

(defmethod autolith-supervisor::memory-context-mode ((configuration configuration))
  "Return the legacy configuration's knowledge mode."
  (config :knowledge-mode configuration))

(defmethod autolith-supervisor::memory-context-read-file ((configuration configuration) pathname)
  "Read an authorized legacy source file with the shared size bound."
  (declare (ignore configuration))
  (read-file-text pathname :maximum-octets *memory-source-file-byte-limit* :follow-links-p t))

(defmethod autolith-supervisor::memory-context-file-status ((configuration configuration) pathname)
  "Return the native status used to check a legacy evidence source."
  (declare (ignore configuration))
  (ls-compat.posix:file-information pathname :follow-links-p t))
