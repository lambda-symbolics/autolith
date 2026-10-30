(in-package #:autolith)

;;;; -- Agent Knowledge Viewer Configuration --

(defmethod configuration-knowledge-viewers-path ((configuration configuration))
  "Return the agent's trusted viewer snapshot pathname."
  (merge-pathnames "knowledge-viewers.sexp" (config :state-root configuration)))
