(in-package #:autolith)

;;;; -- Task Agent Adapter --

(defmethod observability-agent-child-p ((agent task-child-agent))
  "Return true for a nested task agent."
  (declare (ignore agent))
  t)

(defmethod observability-agent-name ((agent task-child-agent))
  "Return the configured observability name for a nested task agent."
  (task-agent-definition-name (task-child-agent-definition agent)))

(defmethod observability-agent-parent-name ((agent task-child-agent))
  "Return the observability name of a nested task agent's parent."
  (let ((parent (task-job-parent-agent (task-child-agent-job agent))))
    (and parent (observability-agent-name parent))))
