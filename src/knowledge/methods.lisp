(in-package #:autolith)

(defgeneric knowledge-expand-tool-remote (context arguments)
  (:documentation "Return a parent expansion result and an installed flag, or NIL values without a parent."))

(defmethod knowledge-expand-tool-remote (context arguments)
  "Select the legacy expansion unless a parent knowledge adapter is installed."
  (declare (ignore context arguments))
  (values nil nil))

(-> knowledge-expand-task
    (knowledge-view list &key (:depth-limit integer) (:node-limit integer))
    (values list keyword list))
(defun knowledge-expand-task
    (view task &key (depth-limit *knowledge-method-depth-limit*)
                    (node-limit *knowledge-method-node-limit*))
  "Return portable plan steps, outcome, and details from the shared method engine."
  (autolith-supervisor::knowledge-expand-task-data
   view task :depth-limit depth-limit :node-limit node-limit))

(defgeneric knowledge-plan-tool-remote (context arguments action)
  (:documentation "Return a parent plan result and an installed flag."))

(defmethod knowledge-plan-tool-remote (context arguments action)
  "Use local plan behavior without an installed parent."
  (declare (ignore context arguments action))
  (values nil nil))

(defgeneric knowledge-plan-context-remote (request)
  (:documentation "Return parent plan advice and an installed flag."))

(defmethod knowledge-plan-context-remote (request)
  "Use local plan advice without an installed parent."
  (declare (ignore request))
  (values nil nil))

(defgeneric knowledge-plan-delivered-remote (configuration conversation plan)
  (:documentation "Record parent plan delivery and return an installed flag."))

(defmethod knowledge-plan-delivered-remote (configuration conversation plan)
  "Use local delivery tracking without an installed parent."
  (declare (ignore configuration conversation plan))
  nil)
